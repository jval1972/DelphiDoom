//------------------------------------------------------------------------------
//
//  DelphiDoom is a source port of the game Doom and it is
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 1993-1996 by id Software, Inc.
//  Copyright (C) 2004-2022 by Jim Valavanis
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
//
// DESCRIPTION:
//   Main program, simply calls D_DoomMain high level loop.
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

{$WARN SYMBOL_DEPRECATED OFF}

unit i_main;

interface

uses
  Windows;

const
  WINDOW_STYLE = (WS_OVERLAPPED);

//==============================================================================
//
// DoomMain
//
//==============================================================================
procedure DoomMain;

var
  NATIVEWIDTH: integer;
  NATIVEHEIGHT: integer;

implementation

uses
  d_delphi,
  Messages,
  doomdef,
  g_game,
  i_input,
  i_system,
  i_displaymodes,
  i_mainwindow,
  m_argv,
  m_base,
  d_main;

//==============================================================================
//
// WindowProc
//
//==============================================================================
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
          if (fullscreen = FULLSCREEN_EXCLUSIVE) and (wParam = SC_TASKLIST) then
          begin
            result := 0;
            exit;
          end;
        end;
      WM_SIZE:
        begin
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

//==============================================================================
//
// DoomMain
//
//==============================================================================
procedure DoomMain;
var
  WindowClass: TWndClass;
  exStyle: integer;
begin
  I_SetDPIAwareness;

  NATIVEWIDTH := I_ScreenWidth;
  NATIVEHEIGHT := I_ScreenHeight;

  ZeroMemory(@WindowClass, SizeOf(WindowClass));
  WindowClass.lpfnWndProc := @WindowProc;
  WindowClass.hbrBackground := GetStockObject(DKGRAY_BRUSH);
  WindowClass.lpszClassName := WINCLASSNAME;
  if HPrevInst = 0 then
  begin
    WindowClass.hInstance := HInstance;
    WindowClass.hIcon := LoadIcon(HInstance, 'MAINICON');
    WindowClass.hCursor := 0;
    if RegisterClass(WindowClass) = 0 then
      halt(1);
  end;

  if fullscreen = FULLSCREEN_EXCLUSIVE then
    exStyle := WS_EX_TOPMOST
  else
    exStyle := 0;

  hMainWnd := CreateWindowEx(
    exStyle,
    WindowClass.lpszClassName,
    AppTitle,
    WINDOW_STYLE,
    0, 0, 0, 0,
    0,
    0,
    HInstance,
    nil);

  SetWindowLong(hMainWnd, GWL_STYLE, 0);

  SetCursor(0);
  D_DoomMain;
end;

end.

