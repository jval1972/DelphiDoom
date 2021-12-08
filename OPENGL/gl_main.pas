//------------------------------------------------------------------------------
//
//  DelphiDoom: A modified and improved DOOM engine for Windows
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 1993-1996 by id Software, Inc.
//  Copyright (C) 2004-2021 by Jim Valavanis
//
//  This program is free software; you can redistribute it and/or
//  modify it under the terms of the GNU General Public License
//  as published by the Free Software Foundation; either version 2
//  of the License, or (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
//  02111-1307, USA.
//
// DESCRIPTION:
//  System specific interface stuff.
//  OpenGL DOOM graphics
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit gl_main;

interface

uses
  SysUtils,
  Windows,
  d_delphi;

// Called by D_DoomMain,
// determines the hardware configuration
// and sets up the video mode
procedure GL_InitGraphics;

procedure GL_ChangeFullScreen(const full: boolean);

function GL_SetDisplayMode(const newwidth, newheight: integer): boolean;

procedure I_ShutDownGraphics;

// Takes full 8 bit values.
procedure I_SetPalette(const palette: PByteArray);

procedure I_FinishUpdate;

procedure I_StartUpdate;

procedure I_ReadScreen32(dest: pointer);

procedure I_ReverseScreen(const p: PLongWordArray);

procedure I_RestoreWindowPos;

var
  fixstallhack: boolean = true;

var
  h_DC: HDC;    // Global device context
  h_RC: HGLRC;  // OpenGL rendering context

var
  gl_initialized: boolean = false;

procedure DoomMain;

var
  NATIVEWIDTH: integer;
  NATIVEHEIGHT: integer;

implementation

uses
  Messages,
  am_map,
  c_con,
  doomdef,
  d_main,
  d_net,
  g_game,
  hu_stuff,
  {$IFDEF DOOM}
  r_draw,
  st_stuff,
  {$ENDIF}
  {$IFNDEF HEXEN}
  e_endoom,
  {$ENDIF}
  {$IFDEF STRIFE}
  f_fade,
  {$ELSE}
  f_wipe,
  {$ENDIF}
  r_main,
  mt_utils,
  i_displaymodes,
  i_mainwindow,
  i_input,
  i_system,
  m_argv,
  m_menu,
  psi_overlay,
  dglOpenGL,
  gl_render, // JVAL OPENGL
  gl_tex,
  gl_defs,
  v_data,
  v_video;

var
  s_alttab_disabled: boolean = false;

var
  screen: PLongWordArray;
  oscreen: pointer;

procedure I_RestoreWindowPos;
begin
  SetWindowPos(hMainWnd, HWND_TOP, 0, 0, SCREENWIDTH, SCREENHEIGHT, SWP_SHOWWINDOW);
end;

procedure I_DisableAltTab;
var
  old: Boolean;
begin
  if s_alttab_disabled then
    Exit;

  if Win32Platform = VER_PLATFORM_WIN32_NT then
  begin
    if isLibrary then
      RegisterHotKey(0, $C000, MOD_ALT, VK_TAB)
    else
      RegisterHotKey(0, 0, MOD_ALT, VK_TAB)
  end
  else
    SystemParametersInfo(SPI_SCREENSAVERRUNNING, 1, @old, 0);

  s_alttab_disabled := true;
end;

procedure I_EnableAltTab;
var
  old: Boolean;
begin
  if s_alttab_disabled then
  begin
    if Win32Platform = VER_PLATFORM_WIN32_NT then
    begin
      if isLibrary then
        UnregisterHotKey(0, $C000)
      else
        UnregisterHotKey(0, 0)
    end
    else
      SystemParametersInfo(SPI_SCREENSAVERRUNNING, 0, @old, 0);

    s_alttab_disabled := false;
  end;
end;

var
  allocscreensize: integer;

{---------------------------------------------------------------------}
{  Properly destroys the window created at startup (no memory leaks)  }
{---------------------------------------------------------------------}
procedure glKillWnd;
begin
  if fullscreen then             // Change back to non fullscreen
    ChangeDisplaySettings(devmode(nil^), 0);

  if gl_initialized then
  begin
    // Makes current rendering context not current, and releases the device
    // context that is used by the rendering context.
    if (not wglMakeCurrent(h_DC, 0)) then
      I_Warning('glKillWnd(): Release of DC and RC failed!'#13#10);

    // Attempts to delete the rendering context
    if (not wglDeleteContext(h_RC)) then
    begin
      I_Warning('glKillWnd(): Release of rendering context failed!'#13#10);
      h_RC := 0;
    end;
  end;

  // Attemps to release the device context
  if ((h_DC > 0) and (ReleaseDC(hMainWnd, h_DC) = 0)) then
  begin
    I_Warning('glKillWnd(): Release of device context failed!'#13#10);
    h_DC := 0;
  end;

end;

var
  overlay_tex: glUint = 0;
  {$IFNDEF HEXEN}
  endoom_tex: glUint = 0;
  {$ENDIF}
  wipe_tex: glUint = 0;

procedure I_ShutDownGraphics;
begin
  gld_ShutDownTextures;
  if overlay_tex <> 0 then
  begin
    glDeleteTextures(1, @overlay_tex);
    overlay_tex := 0;
  end;
  {$IFNDEF HEXEN}
  if endoom_tex <> 0 then
  begin
    glDeleteTextures(1, @endoom_tex);
    endoom_tex := 0;
  end;
  {$ENDIF}
  if wipe_tex <> 0 then
  begin
    glDeleteTextures(1, @wipe_tex);
    wipe_tex := 0;
  end;
  glKillWnd;
  I_EnableAltTab;
  I_ClearDisplayModes;
  memfree(oscreen, allocscreensize);
end;

procedure glEnable2D;
var
  vPort: array[0..3] of GLInt;
begin
  glGetIntegerv(GL_VIEWPORT, @vPort);

  glMatrixMode(GL_PROJECTION);
  glPushMatrix();
  glLoadIdentity();

  glOrtho(0, vPort[2], 0, vPort[3], -1, 1);
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix();
  glLoadIdentity();
end;

procedure glDisable2D;
begin
  glMatrixMode(GL_PROJECTION);
  glPopMatrix();
  glMatrixMode(GL_MODELVIEW);
  glPopMatrix();
end;

var
  set_hud: boolean;

{$IFDEF OPENGLSAFETRANSLATE}
var
  translation_array: array[0..GLDRAWTEXHEIGHT - 1, 0..GLDRAWTEXWIDTH - 1] of LongWord;

procedure GL_SafeTranslate;
var
  i, j, k: integer;
begin
  k := 0;
  for i := 0 to GLDRAWHEIGHT - 1 do
    for j := 0 to GLDRAWWIDTH - 1 do
    begin
      translation_array[i, j] := screen32[k];
      inc(k);
    end;
end;
{$ENDIF}

{$IFDEF DOOM}
var
  overlay_created: boolean = false;
  sub_y2: integer;
  did_fullhutics: integer = 10;
  overlayupdatetic: integer = 30;
  last_y1: integer = MAXINT;
{$ENDIF}

procedure I_FinishUpdateWIPE;
begin
  glPushAttrib(GL_ALL_ATTRIB_BITS);
  gld_Enable2D;

  if wipe_tex = 0 then
    glGenTextures(1, @wipe_tex);

  glBindTexture(GL_TEXTURE_2D, wipe_tex);

  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);

  glTexImage2D(GL_TEXTURE_2D, 0, 4, WIPESCREENWIDTH, WIPESCREENHEIGHT, 0, GL_BGRA, GL_UNSIGNED_BYTE, w_screen32);

  glColor4f(1.0, 1.0, 1.0, 1.0);
  glEnable(GL_ALPHA_TEST);
  glAlphaFunc(GL_NOTEQUAL, 0);

  glBegin(GL_QUADS);
    glTexCoord2f(0.0, SCREENHEIGHT / WIPESCREENHEIGHT);
    glVertex2f(0, 0);
    glTexCoord2f(SCREENWIDTH / WIPESCREENWIDTH, SCREENHEIGHT / WIPESCREENHEIGHT);
    glVertex2f(SCREENWIDTH, 0);
    glTexCoord2f(SCREENWIDTH / WIPESCREENWIDTH, 0.0);
    glVertex2f(SCREENWIDTH, SCREENHEIGHT);
    glTexCoord2f(0.0, 0.0);
    glVertex2f(0, SCREENHEIGHT);
  glEnd;


// Blend overlay (Menu & Console) over the wipe
  glBindTexture(GL_TEXTURE_2D, overlay_tex);
  if set_hud <> gl_linear_hud then
  begin
    if gl_linear_hud then
    begin
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    end
    else
    begin
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    end;
    set_hud := gl_linear_hud;
  end;

  glTexImage2D(GL_TEXTURE_2D, 0, 4, GLDRAWTEXWIDTH, GLDRAWTEXHEIGHT, 0, GL_BGRA, GL_UNSIGNED_BYTE, screen32);

  glColor4f(1.0, 1.0, 1.0, 1.0);
  glEnable(GL_ALPHA_TEST);
  glAlphaFunc(GL_NOTEQUAL, 0);

  glBegin(GL_QUADS);
    glTexCoord2f(0.0, GLDRAWHEIGHT / GLDRAWTEXHEIGHT);
    glVertex2f(0, 0);
    glTexCoord2f(GLDRAWWIDTH / GLDRAWTEXWIDTH, GLDRAWHEIGHT / GLDRAWTEXHEIGHT);
    glVertex2f(SCREENWIDTH, 0);
    glTexCoord2f(GLDRAWWIDTH / GLDRAWTEXWIDTH, 0.0);
    glVertex2f(SCREENWIDTH, SCREENHEIGHT);
    glTexCoord2f(0.0, 0.0);
    glVertex2f(0, SCREENHEIGHT);
  glEnd;

  glDisable(GL_BLEND);

  gld_Disable2D;
  glPopAttrib;

  gld_Finish;
end;

{$IFNDEF HEXEN}
var
  enterendoom: boolean = false;

procedure I_FinishUpdateENDOOM;
begin
  glPushAttrib(GL_ALL_ATTRIB_BITS);
  gld_Enable2D;

  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT or GL_STENCIL_BUFFER_BIT);

  if endoom_tex = 0 then
    glGenTextures(1, @endoom_tex);

  glBindTexture(GL_TEXTURE_2D, endoom_tex);

  if gl_linear_hud then
  begin
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  end
  else
  begin
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  end;

  glTexImage2D(GL_TEXTURE_2D, 0, 4, ESCREENWIDTH, ESCREENHEIGHT, 0, GL_BGRA, GL_UNSIGNED_BYTE, e_screen32);

  glColor4f(1.0, 1.0, 1.0, 1.0);
  glEnable(GL_ALPHA_TEST);
  glAlphaFunc(GL_NOTEQUAL, 0);

  glBegin(GL_QUADS);
    glTexCoord2f(0.0, 200 / ESCREENHEIGHT);
    glVertex2f(0, 0);
    glTexCoord2f(640 / ESCREENWIDTH, 200 / ESCREENHEIGHT);
    glVertex2f(SCREENWIDTH, 0);
    glTexCoord2f(640 / ESCREENWIDTH, 0.0);
    glVertex2f(SCREENWIDTH, SCREENHEIGHT);
    glTexCoord2f(0.0, 0.0);
    glVertex2f(0, SCREENHEIGHT);
  glEnd;

  glDisable(GL_BLEND);

  gld_Disable2D;
  glPopAttrib;

  gld_Finish;
end;
{$ENDIF}

procedure I_FinishUpdateOverlay(const doflush: boolean);
{$IFDEF DOOM}
var
  sub_y1, y1, y2, yovr: integer;
{$ENDIF}
begin
  glPushAttrib(GL_ALL_ATTRIB_BITS);
  gld_Enable2D;

  glBindTexture(GL_TEXTURE_2D, overlay_tex);
  if set_hud <> gl_linear_hud then
  begin
    if gl_linear_hud then
    begin
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    end
    else
    begin
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
    end;
    set_hud := gl_linear_hud;
  end;

  if firstinterpolation then
  begin
{$IFDEF OPENGLSAFETRANSLATE}
    GL_SafeTranslate;
    glTexImage2D(GL_TEXTURE_2D, 0, 4, GLDRAWTEXWIDTH, GLDRAWTEXHEIGHT, 0, GL_BGRA, GL_UNSIGNED_BYTE, @translation_array);
{$ELSE}
{$IFDEF DOOM}
    dec(overlayupdatetic);
    if overlay_created then
    begin
      if (gamestate = GS_LEVEL) and (amstate = am_inactive) and not menuactive then
      begin
        if did_fullhutics > 0 then
        begin
          Dec(did_fullhutics);
          glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, GLDRAWWIDTH, GLDRAWHEIGHT, GL_BGRA, GL_UNSIGNED_BYTE, screen32);
        end
        else
        begin
          y1 := C_ConsolePos;
          yovr := OVR_OverlayHeight;
          if yovr > y1 then
            y1 := yovr;
          sub_y1 := V_PreserveY(HU_Height);
          if sub_y1 > y1 then
            y1 := sub_y1;
          sub_y1 := V_PreserveY(diskbusy_height);
          if sub_y1 > y1 then
            y1 := sub_y1;
          if last_y1 = MAXINT then
            last_y1 := GLDRAWTEXHEIGHT;
          if last_y1 > y1 then
            glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, GLDRAWWIDTH, last_y1, GL_BGRA, GL_UNSIGNED_BYTE, screen32)
          else
            glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, GLDRAWWIDTH, y1, GL_BGRA, GL_UNSIGNED_BYTE, screen32);
          last_y1 := y1;
          if (screenblocks < 12) or (overlayupdatetic = 0) then
            y2 := sub_y2
          else
            y2 := GLDRAWHEIGHT;
          if demoplayback then
            y2 := y2 - V_PreserveY(10);
          glTexSubImage2D(GL_TEXTURE_2D, 0, 0, y2, GLDRAWWIDTH, GLDRAWHEIGHT - y2, GL_BGRA, GL_UNSIGNED_BYTE, @screen32[y2 * GLDRAWWIDTH]);
        end;
      end
      else
      begin
        did_fullhutics := TICRATE;
        glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, GLDRAWWIDTH, GLDRAWHEIGHT, GL_BGRA, GL_UNSIGNED_BYTE, screen32);
      end;
      if overlayupdatetic = 0 then
        overlayupdatetic := 30;
    end
    else
    begin
      overlay_created := true;
      sub_y2 := V_PreserveY(ST_Y);
      glTexImage2D(GL_TEXTURE_2D, 0, 4, GLDRAWTEXWIDTH, GLDRAWTEXHEIGHT, 0, GL_BGRA, GL_UNSIGNED_BYTE, screen32);
    end;
{$ELSE}
    glTexImage2D(GL_TEXTURE_2D, 0, 4, GLDRAWTEXWIDTH, GLDRAWTEXHEIGHT, 0, GL_BGRA, GL_UNSIGNED_BYTE, screen32);
{$ENDIF}
{$ENDIF}
  end;

  glColor4f(1.0, 1.0, 1.0, 1.0);
  glEnable(GL_ALPHA_TEST);
  glAlphaFunc(GL_NOTEQUAL, 0);

  glBegin(GL_QUADS);
    glTexCoord2f(0.0, GLDRAWHEIGHT / GLDRAWTEXHEIGHT);
    glVertex2f(0, 0);
    glTexCoord2f(GLDRAWWIDTH / GLDRAWTEXWIDTH, GLDRAWHEIGHT / GLDRAWTEXHEIGHT);
    glVertex2f(SCREENWIDTH, 0);
    glTexCoord2f(GLDRAWWIDTH / GLDRAWTEXWIDTH, 0.0);
    glVertex2f(SCREENWIDTH, SCREENHEIGHT);
    glTexCoord2f(0.0, 0.0);
    glVertex2f(0, SCREENHEIGHT);
  glEnd;

  glDisable(GL_BLEND);

  gld_Disable2D;
  glPopAttrib;

  if doflush then
    gld_Finish;
end;

var
  wipe: boolean = false;

procedure I_StartUpdate;
begin
  if not wipe then
  begin
    if Ord(gamestate) <> wipegamestate then
      {$IFNDEF HEXEN}
      if gamestate <> GS_ENDOOM then
      {$ENDIF}
      begin
        wipe := true;
        {$IFDEF STRIFE}fade_StartScreen{$ELSE}wipe_StartScreen{$ENDIF};
        wipegamestate := Ord(gamestate);
      end;
  end;
//  else
//    printf('I_StartUpdate(): fuck!');
end;

procedure I_FinishUpdate;
var
  wipestart: integer;
  nowtime: integer;
  tics: integer;
  done: boolean;
begin
  if (hMainWnd = 0) or (screens[SCN_FG] = nil) or (screen32 = nil) then
    exit;

  {$IFNDEF HEXEN}
  if (gamestate = GS_ENDOOM) or enterendoom then
  begin
    enterendoom := true;
    I_FinishUpdateENDOOM;
    exit;
  end;
  {$ENDIF}

  I_FinishUpdateOverlay(not wipe);  // Will also flush

  if not wipe then
    exit;

  // wipe update
  {$IFDEF STRIFE}
  fade_EndScreen;
  {$ELSE}
  wipe_EndScreen;
  {$ENDIF}

  wipedisplay := true;
  wipestart := I_GetTime - 1;

  firstinterpolation := true;

  repeat
    repeat
      nowtime := I_GetTime;
      tics := nowtime - wipestart;
      I_WaitVBL(1);
    until tics <> 0;
    wipestart := nowtime;
    done := {$IFDEF STRIFE}fade_Ticker{$ELSE}wipe_Ticker{$ENDIF}(tics);
    MT_ZeroMemory(screen32, V_GetScreenWidth(SCN_FG) * V_GetScreenHeight(SCN_FG) * 4);
    M_Drawer;         // Menu is drawn even on top of wipes
    C_Drawer;         // Console draw on top of wipes and menus
    I_FinishUpdateWIPE;
    HU_DoFPSStuff;
  until done;
  {$IFDEF STRIFE}
  fade_ClearMemory;
  {$ELSE}
  wipe_ClearMemory;
  {$ENDIF}
  wipedisplay := false;
  wipe := false;
end;

//
// Palette stuff.
//

//
// I_SetPalette
//
procedure I_SetPalette(const palette: PByteArray);
var
  dest: PLongWord;
  src: PByteArray;
  limit: integer;
  curgamma: PByteArray;
begin
  dest := @curpal[0];
  src := palette;
  curgamma := @gammatable[usegamma];
  limit := integer(@palette[256 * 3]);
  while integer(src) < limit do
  begin
    dest^ := (LongWord(curgamma[src[0]]) shl 16) or
             (LongWord(curgamma[src[1]]) shl 8) or
             (LongWord(curgamma[src[2]]));
    inc(dest);
    src := PByteArray(integer(src) + 3);
  end;
end;

function I_AdjustWindowMode: boolean;
begin
  result := false;
  if SCREENWIDTH > GetSystemMetrics(SM_CXSCREEN) then
  begin
    SCREENWIDTH := GetSystemMetrics(SM_CXSCREEN);
    result := true;
  end;
  if SCREENHEIGHT > GetSystemMetrics(SM_CYSCREEN) then
  begin
    SCREENHEIGHT := GetSystemMetrics(SM_CYSCREEN);
    result := true;
  end;
end;

const
  ERROR_OFFSET = 20;

{------------------------------------------------------------------}
{  Handle window resize                                            }
{------------------------------------------------------------------}
procedure glResizeWnd;
begin
  glViewport(0, 0, SCREENWIDTH, SCREENHEIGHT);    // Set the viewport for the OpenGL window
  glMatrixMode(GL_PROJECTION);        // Change Matrix Mode to Projection
  glLoadIdentity;                     // Reset View
  gluPerspective(45.0, SCREENWIDTH / SCREENHEIGHT, 1.0, 100.0);  // Do the perspective calculations. Last value = max clipping depth

  glMatrixMode(GL_MODELVIEW);         // Return to the modelview matrix
  glLoadIdentity;                     // Reset View
end;

{------------------------------------------------------------------}
{  Initialise OpenGL                                               }
{------------------------------------------------------------------}
procedure glInit;
begin
  glClearColor(0.0, 0.0, 0.0, 0.0);        // Black Background
  glShadeModel(GL_SMOOTH);                 // Enables Smooth Color Shading
  glClearDepth(1.0);                       // Depth Buffer Setup
  glEnable(GL_DEPTH_TEST);                 // Enable Depth Buffer
  glDepthFunc(GL_LESS);                    // The Type Of Depth Test To Do
  glHint(GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);   //Realy Nice perspective calculations
  glEnable(GL_TEXTURE_2D);                     // Enable Texture Mapping
end;

function WindowProc(hWnd: HWND; Msg: UINT; wParam: WPARAM;
  lParam: LPARAM): LRESULT; stdcall; export;
begin
  if not I_GameFinished then
  begin
    case Msg of
      WM_SETCURSOR:
        begin
          SetCursor(0);
        end;
      WM_SYSCOMMAND:
        begin
          if (wParam = SC_SCREENSAVE) or (wParam = SC_MINIMIZE) then
          begin
            result := 0;
            exit;
          end;
          if fullscreen and (wParam = SC_TASKLIST) then
          begin
            result := 0;
            exit;
          end;
        end;
      WM_SIZE:
        begin
//          glResizeWnd;
          result := 0;
          exit;
        end;
      WM_ACTIVATE:
        begin
          InBackground := (LOWORD(wparam) = WA_INACTIVE) or (HIWORD(wparam) <> 0);
          I_SynchronizeInput(not InBackground);
        end;
      WM_CLOSE:
        begin
          result := 0; // Preserve closing window by pressing Alt + F4
          exit;
        end;
      WM_DESTROY:
        begin
          result := 0;
          ShowWindow(hWnd, SW_HIDE);
          I_Destroy(0);
          exit;
        end;
    end;
  end;

  result := DefWindowProc(hWnd, Msg, WParam, LParam);
end;

// Called by D_DoomMain,
// determines the hardware configuration
// and sets up the video mode
procedure GL_InitGraphics;
var
  WindowClass: TWndClass;
  dwStyle: DWORD;              // Window styles
  dwExStyle: DWORD;            // Extended window styles
  dmScreenSettings: DEVMODE;   // Screen settings (fullscreen, etc...)
  PixelFormat: GLuint;         // Settings for the OpenGL rendering
  h_Instance: HINST;           // Current instance
  pfd: TPIXELFORMATDESCRIPTOR;  // Settings for the OpenGL window
begin
  printf('GL_InitGraphics: Initializing OpenGL.'#13#10);

  I_EnumDisplayModes;

  InitOpenGL;                               // New call to initialize and bind the OpenGL dll
  gl_initialized := true;

  h_Instance := GetModuleHandle(nil);       //Grab An Instance For Our Window
  ZeroMemory(@WindowClass, SizeOf(TWndClass));  // Clear the window class structure

  with WindowClass do                    // Set up the window class
  begin
    style         := CS_HREDRAW or    // Redraws entire window if length changes
                     CS_VREDRAW or    // Redraws entire window if height changes
                     CS_OWNDC;        // Unique device context for the window
    lpfnWndProc   := @WindowProc;        // Set the window procedure to our func WndProc
    hInstance     := h_Instance;
    hCursor       := LoadCursor(0, IDC_ARROW);
    lpszClassName := 'Doom32';
  end;

  if RegisterClass(WindowClass) = 0 then  // Attemp to register the window class
  begin
    I_Error('GL_InitGraphics(): Failed to register the window class!');
    exit;
  end;

  // Change to fullscreen if so desired
  if fullscreen then
  begin
    ZeroMemory(@dmScreenSettings, SizeOf(dmScreenSettings));
    with dmScreenSettings do        // Set parameters for the screen setting
    begin
      dmSize       := SizeOf(dmScreenSettings);
      dmPelsWidth  := SCREENWIDTH;  // Window width
      dmPelsHeight := SCREENHEIGHT; // Window height
      dmBitsPerPel := 32;           // Window color depth
      dmFields     := DM_PELSWIDTH or DM_PELSHEIGHT or DM_BITSPERPEL;
    end;

    // Try to change screen mode to fullscreen
    if ChangeDisplaySettings(dmScreenSettings, CDS_FULLSCREEN) = DISP_CHANGE_FAILED then
    begin
      I_Error('GL_InitGraphics(): Unable to switch to fullscreen!');
      fullscreen := False;
    end;
  end;

  dwStyle := WS_POPUP or        // Creates a popup window
             WS_CLIPCHILDREN or // Doesn't draw within child windows
             WS_CLIPSIBLINGS;   // Doesn't draw within sibling windows
  dwExStyle := WS_EX_APPWINDOW;         // Top level window

  ShowCursor(False);                    // Turn of the cursor (gets in the way)

  // Attempt to create the actual window
  hMainWnd := CreateWindowEx(
                          dwExStyle,      // Extended window styles
                          WindowClass.lpszClassName,
                          AppTitle,
                          dwStyle,        // Window styles
                          0, 0,           // Window position
                          SCREENWIDTH,
                          SCREENHEIGHT,
                          0,              // No parent window
                          0,              // No menu
                          h_Instance,     // Instance
                          nil);           // Pass nothing to WM_CREATE

  if hMainWnd = 0 then
  begin
    glKillWnd;                // Undo all the settings we've changed
    I_Error('GL_InitGraphics(): Unable to create window!');
    Exit;
  end;

  // Try to get a device context
  h_DC := GetDC(hMainWnd);
  if (h_DC = 0) then
  begin
    glKillWnd;
    I_Error('GL_InitGraphics(): Unable to get a device context!');
    exit;
  end;

  // Settings for the OpenGL window
  with pfd do
  begin
    nSize           := SizeOf(TPIXELFORMATDESCRIPTOR); // Size Of This Pixel Format Descriptor
    nVersion        := 1;                    // The version of this data structure
    dwFlags         := PFD_DRAW_TO_WINDOW    // Buffer supports drawing to window
                       or PFD_SUPPORT_OPENGL // Buffer supports OpenGL drawing
                       or PFD_DOUBLEBUFFER;  // Supports double buffering
    iPixelType      := PFD_TYPE_RGBA;        // RGBA color format
    cColorBits      := 32;                   // OpenGL color depth
    cRedBits        := 0;                    // Number of red bitplanes
    cRedShift       := 0;                    // Shift count for red bitplanes
    cGreenBits      := 0;                    // Number of green bitplanes
    cGreenShift     := 0;                    // Shift count for green bitplanes
    cBlueBits       := 0;                    // Number of blue bitplanes
    cBlueShift      := 0;                    // Shift count for blue bitplanes
    cAlphaBits      := 0;                    // Not supported
    cAlphaShift     := 0;                    // Not supported
    cAccumBits      := 0;                    // No accumulation buffer
    cAccumRedBits   := 0;                    // Number of red bits in a-buffer
    cAccumGreenBits := 0;                    // Number of green bits in a-buffer
    cAccumBlueBits  := 0;                    // Number of blue bits in a-buffer
    cAccumAlphaBits := 0;                    // Number of alpha bits in a-buffer
    cDepthBits      := 16;                   // Specifies the depth of the depth buffer
    cStencilBits    := 16;                   // Turn off stencil buffer
    cAuxBuffers     := 0;                    // Not supported
    iLayerType      := PFD_MAIN_PLANE;       // Ignored
    bReserved       := 0;                    // Number of overlay and underlay planes
    dwLayerMask     := 0;                    // Ignored
    dwVisibleMask   := 0;                    // Transparent color of underlay plane
    dwDamageMask    := 0;                    // Ignored
  end;

  // Attempts to find the pixel format supported by a device context that is the best match to a given pixel format specification.
  PixelFormat := ChoosePixelFormat(h_DC, @pfd);
  if PixelFormat = 0 then
  begin
    glKillWnd;
    I_Error('GL_InitGraphics(): Unable to find a suitable pixel format');
    exit;
  end;

  // Sets the specified device context's pixel format to the format specified by the PixelFormat.
  if not SetPixelFormat(h_DC, PixelFormat, @pfd) then
  begin
    glKillWnd;
    I_Error('GL_InitGraphics(): Unable to set the pixel format');
    exit;
  end;

  // Create a OpenGL rendering context
  h_RC := wglCreateContext(h_DC);
  if h_RC = 0 then
  begin
    glKillWnd;
    I_Error('GL_InitGraphics(): Unable to create an OpenGL rendering context');
    exit;
  end;

  // Makes the specified OpenGL rendering context the calling thread's current rendering context
  if not wglMakeCurrent(h_DC, h_RC) then
  begin
    glKillWnd;
    I_Error('GL_InitGraphics(): Unable to activate OpenGL rendering context');
    exit;
  end;
  // Read And Assign Extentions
  ReadExtensions;
  ReadImplementationProperties;

  // Settings to ensure that the window is the topmost window
  ShowWindow(hMainWnd, SW_SHOW);
  SetForegroundWindow(hMainWnd);
  SetFocus(hMainWnd);

  // Ensure the OpenGL window is resized properly
  glResizeWnd;
  glInit;

  gld_Init(SCREENWIDTH, SCREENHEIGHT);

  allocscreensize := 2048 * (1536 + ERROR_OFFSET) * SizeOf(LongWord) and not (4095);
  screen := mallocA(allocscreensize, $10000, oscreen); // JVAL: Memory padding may increase performance until 4%
  screen32 := screen;

  glGenTextures(1, @overlay_tex);
  glBindTexture(GL_TEXTURE_2D, overlay_tex);
  glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);  {Texture blends with object background}
  if gl_linear_hud then
  begin
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  end
  else
  begin
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  end;
  set_hud := gl_linear_hud;

  glTexImage2D(GL_TEXTURE_2D, 0, 4, GLDRAWTEXWIDTH, GLDRAWTEXHEIGHT, 0, GL_BGRA, GL_UNSIGNED_BYTE, screen32);
end;

procedure GL_ChangeFullScreen(const full: boolean);
var
  dmScreenSettings: DEVMODE;   // Screen settings (fullscreen, etc...)
begin
  I_RestoreWindowPos;
  fullscreen := full;
  // Change to fullscreen if so desired
  if fullscreen then
  begin
    ZeroMemory(@dmScreenSettings, SizeOf(dmScreenSettings));
    with dmScreenSettings do
    begin              // Set parameters for the screen setting
      dmSize       := SizeOf(dmScreenSettings);
      dmPelsWidth  := SCREENWIDTH;                    // Window width
      dmPelsHeight := SCREENHEIGHT;                   // Window height
      dmBitsPerPel := 32;                             // Window color depth
      dmFields     := DM_PELSWIDTH or DM_PELSHEIGHT or DM_BITSPERPEL;
    end;

    // Try to change screen mode to fullscreen
    if ChangeDisplaySettings(dmScreenSettings, CDS_FULLSCREEN) = DISP_CHANGE_FAILED then
      I_Warning('GL_ChangeFullScreen(): Can not change to fullscreen mode'#13#10);
  end
  else
    ChangeDisplaySettings(devmode(nil^), 0);
  I_RestoreWindowPos;
end;


function GL_SetDisplayMode(const newwidth, newheight: integer): boolean;
var
  nwidth, nheight: integer;
begin
  result := false;

  if (SCREENWIDTH = newwidth) and (SCREENHEIGHT = newheight) then
    exit;

  nwidth := newwidth and not 1;
  if nwidth > MAXWIDTH then
    nwidth := MAXWIDTH
  else if nwidth < MINWIDTH then
    nwidth := MINWIDTH;

  nheight := newheight and not 1;
  if nheight > MAXHEIGHT then
    nwidth := MAXHEIGHT
  else if nheight < MINHEIGHT then
    nheight := MINHEIGHT;

  if nheight > nwidth then
    nheight := nwidth;

  if (SCREENWIDTH <> nwidth) or (SCREENHEIGHT <> nheight) then
  begin
    MT_WaitTasks;           // Wait for running tasks to stop
    AM_Stop;                // Stop the aytomap

    SCREENWIDTH := nwidth;
    SCREENHEIGHT := nheight;

    GL_ChangeFullScreen(fullscreen);

    glResizeWnd;
    glInit;
    glViewport(0, 0, SCREENWIDTH, SCREENHEIGHT);

    AM_Start;               // Start the aytomap
    C_AdjustScreenSize;
    R_ExecuteSetViewSize;   // Set-up new SCREENWIDTH & SCREENHEIGHT
    overlay.ReCalcOverlayLookUp;
    result := true;
  end;
end;

procedure I_ReadScreen32(dest: pointer);
begin
  glReadPixels(0, 0, SCREENWIDTH, SCREENHEIGHT, GL_BGRA, GL_UNSIGNED_BYTE, dest);
end;

procedure I_ReverseScreen(const p: PLongWordArray);
var
  src, dest: PLongWordArray;
  buf: LongWord;
  i, j: integer;
begin
  for i := 0 to SCREENHEIGHT div 2 do
  begin
    src := @p[i * SCREENWIDTH];
    dest := @p[(SCREENHEIGHT - i - 1) * SCREENWIDTH];
    for j := 0 to SCREENWIDTH - 1 do
    begin
      buf := dest[j];
      dest[j] := src[j];
      src[j] := buf;
    end;
  end;
end;

procedure DoomMain;
begin
  I_SetDPIAwareness;

  NATIVEWIDTH := I_ScreenWidth;
  NATIVEHEIGHT := I_ScreenHeight;

  D_DoomMain;
end;

end.
