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
//  Foundation, inc., 59 Temple Place - Suite 330, Boston, MA
//  02111-1307, USA.
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit i_input;

interface

procedure I_InitInput;

procedure I_ProcessInput;

procedure I_ShutDownInput;

procedure I_SynchronizeInput(active: boolean);

procedure I_SetMouseClicks(val: integer);

var
  usedirectinput: boolean = false;

implementation

uses
  Windows,
  DirectX,
  MMSystem, // For joystick support
{$IFDEF FPC}
  d_fpc,
{$ENDIF}
  d_delphi,
  doomdef,
  d_event,
  d_main,
  i_mainwindow,
  i_system;

function TranslateKey(keycode: integer): integer;
begin
  case keycode of
    VK_LEFT, VK_NUMPAD4: result := KEY_LEFTARROW;
    VK_RIGHT, VK_NUMPAD6: result := KEY_RIGHTARROW;
    VK_DOWN, VK_NUMPAD2: result := KEY_DOWNARROW;
    VK_UP, VK_NUMPAD8: result := KEY_UPARROW;
    VK_ESCAPE: result := KEY_ESCAPE;
    VK_RETURN: result := KEY_ENTER;
    VK_TAB: result := KEY_TAB;
    VK_SNAPSHOT: result := KEY_PRNT;
    VK_F1: result := KEY_F1;
    VK_F2: result := KEY_F2;
    VK_F3: result := KEY_F3;
    VK_F4: result := KEY_F4;
    VK_F5: result := KEY_F5;
    VK_F6: result := KEY_F6;
    VK_F7: result := KEY_F7;
    VK_F8: result := KEY_F8;
    VK_F9: result := KEY_F9;
    VK_F10: result := KEY_F10;
    VK_F11: result := KEY_F11;
    VK_F12: result := KEY_F12;
    187: result := KEY_EQUALS;
    188: result := Ord(',');
    189: result := KEY_MINUS;
    VK_BACK: result := KEY_BACKSPACE;
    192: result := KEY_CON;
    220: result := Ord('\');
{$IFNDEF DOOM}
    219: result := Ord('[');
    221: result := Ord(']');
{$ENDIF}
    VK_ADD: result := Ord('+');
    VK_SUBTRACT: result := Ord('-');
    VK_MULTIPLY: result := Ord('*');
    VK_DIVIDE, 191: result := Ord('/');
    VK_DECIMAL, 190: result := Ord('.');
    VK_PAUSE: result := KEY_PAUSE;
    VK_NUMPAD3, VK_NEXT: result := KEY_PAGEDOWN;
    VK_NUMPAD9, VK_PRIOR: result := KEY_PAGEUP;
    VK_NUMPAD0, VK_INSERT: result := KEY_INS;
    VK_NUMPAD7, VK_HOME: result := KEY_HOME;
    VK_NUMPAD1, VK_END: result := KEY_END;
    VK_DELETE: result := KEY_DELETE;
  else
    if (keycode >= Ord('A')) and (keycode <= Ord('Z')) then
      result := Ord(tolower(Chr(keycode)))
    else if keycode < 128 then
      result := keycode
    else
      result := 0;
  end;
end;

function TranslateSysKey(keycode: integer): integer;
begin
  case keycode of
    VK_SHIFT: result := KEY_RSHIFT;
    VK_CONTROL: result := KEY_RCTRL;
    VK_MENU: result := KEY_RALT;
  else
    result := 0;
  end;
end;

const
  I_IGNORETICKS = 15; // ~ half second

var
  ignoretics: integer;
  g_pDI: IDirectInputA = nil;
  g_pdidKeyboard: IDirectInputDevice = nil;
  dikb: TDIKeyboardState; // DirectInput keyboard state buffer
  curkeys: PKeyboardState;
  oldkeys: PKeyboardState;
// Mouse support
  mlastx,
  mlasty: integer;
  mflags: byte;
// Joystick support
  jInfo: TJoyInfoEx;
  jPresent: boolean;
  jwXpos: UINT;
  jwYpos: UINT;

type
  setcursorposfunc_t = function(x, y:Integer): BOOL; stdcall;
  getcursorposfunc_t = function(var lpPoint: TPoint): BOOL; stdcall;

var
  getcursorposfunc: getcursorposfunc_t;
  setcursorposfunc: setcursorposfunc_t;
  user32inst: THandle;

procedure I_InitMouse;
begin
  user32inst := LoadLibrary(user32);
  getcursorposfunc := GetProcAddress(user32inst, 'GetPhysicalCursorPos');
  if not assigned(getcursorposfunc) then
    getcursorposfunc := GetProcAddress(user32inst, 'GetCursorPos');
  setcursorposfunc := GetProcAddress(user32inst, 'SetPhysicalCursorPos');
  if not assigned(setcursorposfunc) then
    setcursorposfunc := GetProcAddress(user32inst, 'SetCursorPos');
end;

procedure I_ShutDownMouse;
begin
  FreeLibrary(user32inst);
end;

procedure I_ResetMouse;
begin
  mlastx := SCREENWIDTH div 2;
  mlasty := SCREENHEIGHT div 2;
  setcursorposfunc(mlastx, mlasty);
  mflags := 0;
end;

//-----------------------------------------------------------------------------
// Name: CreateDInput()
// Desc: Initialize the DirectInput variables using:
//           DirectInputCreate
//           IDirectInput::CreateDevice
//           IDirectInputDevice::SetDataFormat
//           IDirectInputDevice::SetCooperativeLevel
//-----------------------------------------------------------------------------
procedure I_InitInput;
var
  hres: HRESULT;
  dipdw: TDIPropDWORD;

  procedure I_ErrorInitInput(const msg: string);
  begin
    I_Error('I_InitInput(): %s failed, result = %d', [msg, hres]);
  end;

begin
  ignoretics := 0;

  I_InitMouse;

  if usedirectinput then
  begin
    // Register with the DirectInput subsystem and get a pointer
    // to a IDirectInput interface we can use
    hres := DirectInputCreate(hInstance, DIRECTINPUT_VERSION, g_pDI, nil);
    if hres <> DD_OK then
      I_ErrorInitInput('DirectInputCreate');

    // Obtain an interface to the keybord device
    hres := g_pDI.CreateDevice(GUID_SysKeyboard, g_pdidKeyboard, nil);
    if hres <> DD_OK then
      I_ErrorInitInput('CreateDevice');

    // Set the data format to "keyboard format". A data format specifies which
    // controls on a device we are interested in, and how they should be
    // reported. This tells DirectInput that we will be passing an array of 256
    // bytes to IDirectInputDevice::GetDeviceState().
    hres := g_pdidKeyboard.SetDataFormat(c_dfDIKeyboard);
    if hres <> DD_OK then
      I_ErrorInitInput('SetDataFormat');

    // Set the cooperative level to let DirectInput know how this device
    // should interact with the system and with other DirectInput applications.
    // Use DISCL_NONEXCLUSIVE to retrieve device data when acquired, not
    // interfering with any other applications which are reading mouse data.
    // Use DISCL_FOREGROUND so that if the user switches away from our app,
    // automatically release the device back to the system.
    hres := g_pdidKeyboard.SetCooperativeLevel(hMainWnd, DISCL_NONEXCLUSIVE or DISCL_FOREGROUND);
    if hres <> DD_OK then
      I_ErrorInitInput('SetCooperativeLevel');

    ZeroMemory(@dipdw, SizeOf(dipdw));
    dipdw.diph.dwSize := SizeOf(dipdw);
    dipdw.diph.dwHeaderSize := SizeOf(dipdw.diph);
    dipdw.diph.dwObj := 0;
    dipdw.diph.dwHow := DIPH_DEVICE;
    dipdw.dwData := SizeOf(TKeyboardState);
    hres := g_pdidKeyboard.SetProperty(DIPROP_BUFFERSIZE, dipdw.diph);
    if hres <> DD_OK then
      I_ErrorInitInput('SetCooperativeLevel');

    printf(' Using DirectInput'#13#10);
  end;

  curkeys := mallocz(SizeOf(TKeyboardState));
  oldkeys := mallocz(SizeOf(TKeyboardState));
  printf(' Keyboard initialized'#13#10);

  I_ResetMouse;
  printf(' Mouse initialized'#13#10);

  jPresent := joyGetNumDevs > 0;
  if jPresent then
    jPresent := joySetCapture(hMainWnd, JOYSTICKID1, 0, false) = JOYERR_NOERROR;

  // Get initial joystic position
  if jPresent then
  begin
    ZeroMemory(@jInfo, SizeOf(TJoyInfoEx));
    jInfo.dwSize := SizeOf(TJoyInfoEx);
    jInfo.dwFlags := JOY_RETURNALL;
    if joyGetPosEx(JOYSTICKID1, @jInfo) = JOYERR_NOERROR then
    begin
      jwXpos := jInfo.wXpos;
      jwYpos := jInfo.wYpos;
    end;
    printf(' Joystick initialized'#13#10);
  end
  else
    printf(' Joystick not found'#13#10);

end;

//-----------------------------------------------------------------------------
// Name: I_ShutDownInput
// Desc: Terminate our usage of DirectInput
//-----------------------------------------------------------------------------
procedure I_ShutDownInput;
begin
  if usedirectinput then
  begin
    if g_pDI <> nil then
    begin
      // Destroy the keyboard object
      if g_pdidKeyboard <> nil then
      begin
        // Unacquire the device (just in case) before exitting.
        g_pdidKeyboard.Unacquire;
        g_pdidKeyboard._Release;
      end;

      // Destroy the DInput object
      g_pDI._Release;
    end;
  end;

  memfree(pointer(curkeys), SizeOf(TKeyboardState));
  memfree(pointer(oldkeys), SizeOf(TKeyboardState));

  joyReleaseCapture(JOYSTICKID1);

  I_ShutDownMouse;
end;

//-----------------------------------------------------------------------------
// Name: I_ProcessInput;
// Desc: The game plays here. Read keyboard data and displaying it.
//-----------------------------------------------------------------------------
procedure I_ProcessInput;

  function DIKEYtoVK(Key: Byte): Integer;
  begin
    result := 0;
    case Key of
      DIK_ESCAPE       : result := VK_ESCAPE;
      DIK_1            : result := Ord('1');
      DIK_2            : result := Ord('2');
      DIK_3            : result := Ord('3');
      DIK_4            : result := Ord('4');
      DIK_5            : result := Ord('5');
      DIK_6            : result := Ord('6');
      DIK_7            : result := Ord('7');
      DIK_8            : result := Ord('8');
      DIK_9            : result := Ord('9');
      DIK_0            : result := Ord('0');
      DIK_EQUALS       : result := Ord('=');
      DIK_BACK         : result := VK_BACK;
      DIK_TAB          : result := VK_TAB;
      DIK_Q            : result := Ord('Q');
      DIK_W            : result := Ord('W');
      DIK_E            : result := Ord('E');
      DIK_R            : result := Ord('R');
      DIK_T            : result := Ord('T');
      DIK_Y            : result := Ord('Y');
      DIK_U            : result := Ord('U');
      DIK_I            : result := Ord('I');
      DIK_O            : result := Ord('O');
      DIK_P            : result := Ord('P');
      DIK_LBRACKET     : result := Ord('[');
      DIK_RBRACKET     : result := Ord(']');
      DIK_RETURN       : result := VK_RETURN;
      DIK_LCONTROL     : result := VK_CONTROL;
      DIK_A            : result := Ord('A');
      DIK_S            : result := Ord('S');
      DIK_D            : result := Ord('D');
      DIK_F            : result := Ord('F');
      DIK_G            : result := Ord('G');
      DIK_H            : result := Ord('H');
      DIK_J            : result := Ord('J');
      DIK_K            : result := Ord('K');
      DIK_L            : result := Ord('L');
      DIK_SEMICOLON    : result := Ord(';');
      DIK_APOSTROPHE   : result := Ord('''');
      DIK_LSHIFT       : result := VK_SHIFT;
      DIK_BACKSLASH    : result := Ord('\');
      DIK_Z            : result := Ord('Z');
      DIK_X            : result := Ord('X');
      DIK_C            : result := Ord('C');
      DIK_V            : result := Ord('V');
      DIK_B            : result := Ord('B');
      DIK_N            : result := Ord('N');
      DIK_M            : result := Ord('M');
      DIK_COMMA        : result := Ord(',');
      DIK_PERIOD       : result := Ord('.');
      DIK_SLASH        : result := Ord('/');
      DIK_RSHIFT       : result := VK_SHIFT;
      DIK_MULTIPLY     : result := Ord('*');
      DIK_LMENU        : result := VK_MENU;
      DIK_SPACE        : result := VK_SPACE;
      DIK_CAPITAL      : result := VK_CAPITAL;
      DIK_F1           : result := VK_F1;
      DIK_F2           : result := VK_F2;
      DIK_F3           : result := VK_F3;
      DIK_F4           : result := VK_F4;
      DIK_F5           : result := VK_F5;
      DIK_F6           : result := VK_F6;
      DIK_F7           : result := VK_F7;
      DIK_F8           : result := VK_F8;
      DIK_F9           : result := VK_F9;
      DIK_F10          : result := VK_F10;
      DIK_NUMLOCK      : result := VK_NUMLOCK;
      DIK_SCROLL       : result := VK_SCROLL;
      DIK_NUMPAD7      : result := VK_NUMPAD7;
      DIK_NUMPAD8      : result := VK_NUMPAD8;
      DIK_NUMPAD9      : result := VK_NUMPAD9;
      DIK_SUBTRACT     : result := VK_SUBTRACT;
      DIK_NUMPAD4      : result := VK_NUMPAD4;
      DIK_NUMPAD5      : result := VK_NUMPAD5;
      DIK_NUMPAD6      : result := VK_NUMPAD6;
      DIK_ADD          : result := VK_ADD;
      DIK_NUMPAD1      : result := VK_NUMPAD1;
      DIK_NUMPAD2      : result := VK_NUMPAD2;
      DIK_NUMPAD3      : result := VK_NUMPAD3;
      DIK_NUMPAD0      : result := VK_NUMPAD0;
      DIK_DECIMAL      : result := VK_DECIMAL;
      DIK_F11          : result := VK_F11;
      DIK_F12          : result := VK_F12;
      DIK_NUMPADENTER  : result := VK_RETURN;
      DIK_RCONTROL     : result := VK_CONTROL;
      DIK_DIVIDE       : result := VK_DIVIDE;
      DIK_RMENU        : result := VK_MENU;
      DIK_HOME         : result := VK_HOME;
      DIK_UP           : result := VK_UP;
      DIK_PRIOR        : result := VK_PRIOR;
      DIK_LEFT         : result := VK_LEFT;
      DIK_RIGHT        : result := VK_RIGHT;
      DIK_END          : result := VK_END;
      DIK_DOWN         : result := VK_DOWN;
      DIK_NEXT         : result := VK_NEXT;
      DIK_INSERT       : result := VK_INSERT;
      DIK_DELETE       : result := VK_DELETE;
      DIK_LWIN         : result := VK_LWIN;
      DIK_RWIN         : result := VK_RWIN;
      DIK_APPS         : result := VK_APPS;
    end;
  end;

var
  hres: HRESULT;
  i: integer;
  ev: event_t;
  key: integer;
  p: PKeyboardState;
  pt: TPoint;
begin
  if ignoretics > 0 then
  begin
    dec(ignoretics);
    exit;
  end;

// Keyboard
  if I_GameFinished or InBackground or
     IsIconic(hMainWnd) or (GetForegroundWindow <> hMainWnd) then
    exit;

  // JVAL -> DirectInput does not work
  if usedirectinput and (g_pdidKeyboard <> nil) then
  begin
    hres := g_pdidKeyboard.GetDeviceState(SizeOf(dikb), dikb);
    if hres = DIERR_INPUTLOST then
    begin
      // DirectInput is telling us that the input stream has been
      // interrupted. Re-acquire and try again.
      hres := g_pdidKeyboard.Acquire;
      if hres = DD_OK then
        I_ProcessInput;
      exit;
    end;

//  The DirectInput key code is converted into the Windows virtual key code.
    for i := Low(dikb) to High(dikb) do
      if dikb[i] and $80 <> 0 then
        curkeys[Byte(DIKEYtoVK(i))] := $80;
  end
  else
    GetKeyboardState(curkeys^);

  ZeroMemory(@ev, SizeOf(ev));

  for i := 0 to SizeOf(curkeys^) - 1 do
  begin

    if (oldkeys[i] and $80) <> (curkeys[i] and $80) then
    begin
      key := TranslateKey(i);
      if key <> 0 then
      begin
        if curkeys[i] and $80 <> 0 then
          ev._type := ev_keydown
        else
          ev._type := ev_keyup;
        ev.data1 := key;
        D_PostEvent(@ev);
      end;

      key := TranslateSysKey(i);
      if key <> 0 then
      begin
        if curkeys[i] and $80 <> 0 then
          ev._type := ev_keydown
        else
          ev._type := ev_keyup;
        ev.data1 := key;
        D_PostEvent(@ev);
      end;
    end;

  end;

  p := oldkeys;
  oldkeys := curkeys;
  curkeys := p;

// Mouse
  if GetKeyState(VK_LBUTTON) < 0 then
    mflags := mflags or 1;
  if GetKeyState(VK_RBUTTON) < 0 then
    mflags := mflags or 2;
  if GetKeyState(VK_MBUTTON) < 0 then
    mflags := mflags or 4;

  getcursorposfunc(pt);

  ev._type := ev_mouse;
  ev.data1 := mflags;
  ev.data2 := mlastx - pt.x;
  ev.data3 := mlasty - pt.y;
  D_PostEvent(@ev);

  I_ResetMouse;

// Joystick
  if jPresent then
  begin
    ZeroMemory(@jInfo, SizeOf(TJoyInfoEx));
    jInfo.dwSize := SizeOf(TJoyInfoEx);
    jInfo.dwFlags := JOY_RETURNALL;
    if joyGetPosEx(JOYSTICKID1, @jInfo) = JOYERR_NOERROR then
    begin
      ev._type := ev_joystick;
      if jInfo.dwButtonNumber > 0 then
        ev.data1 := jInfo.wButtons and ((1 shl NUMJOYBUTTONS) - 1) // Only first NUMJOYBUTTONS buttons of joystic in use
      else
        ev.data1 := 0;
      ev.data2 := jInfo.wXpos - jwXpos;
      ev.data3 := jInfo.wYpos - jwYpos;
      D_PostEvent(@ev);
    end;
  end;

end;

procedure I_SetMouseClicks(val: integer);
begin
  if val > 0 then
    mflags := mflags or val
  else
  begin
    val := -val;
    mflags := mflags and not val;
  end;
end;

procedure I_SynchronizeInput(active: boolean);
begin
  if active then
    ignoretics := I_IGNORETICKS; // Wait ~ half second when get the focus again
  if usedirectinput and (g_pdidKeyboard <> nil) then
  begin
    if active then
      g_pdidKeyboard.Acquire
    else
      g_pdidKeyboard.Unacquire;
  end;
end;

end.

