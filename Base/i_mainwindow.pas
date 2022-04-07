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
// DESCRIPTION:
//  Main Window declaration
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit i_mainwindow;

interface

uses
  Windows;

var
  hMainWnd: HWND = 0;
  windowxpos: integer = 0;
  windowypos: integer = 0;

const
  WINDOW_STYLE_FS = (WS_OVERLAPPED);
  WINDOW_STYLE_W = (WS_POPUPWINDOW or WS_TABSTOP or WS_VISIBLE or WS_SYSMENU or WS_CAPTION);

//==============================================================================
//
// I_GetWindowClientOffset
//
//==============================================================================
procedure I_GetWindowClientOffset(var dw, dh: integer);

//==============================================================================
//
// I_GetWindowOffset
//
//==============================================================================
procedure I_GetWindowOffset(var dw, dh: integer);

//==============================================================================
//
// I_GetWindowPosition
//
//==============================================================================
procedure I_GetWindowPosition(var dw, dh: integer);

implementation

//==============================================================================
//
// I_GetWindowClientOffset
//
//==============================================================================
procedure I_GetWindowClientOffset(var dw, dh: integer);
var
  rw, rc: TRect;
begin
  GetClientRect(hMainWnd, rc);
  GetWindowRect(hMainWnd, rw);
  dw := (rw.Right - rw.Left) - (rc.Right - rc.Left);
  dh := (rw.Bottom - rw.Top) - (rc.Bottom - rc.Top);
end;

//==============================================================================
//
// I_GetWindowOffset
//
//==============================================================================
procedure I_GetWindowOffset(var dw, dh: integer);
var
  rw, rc: TRect;
  border: integer;
begin
  GetClientRect(hMainWnd, rc);
  GetWindowRect(hMainWnd, rw);
  border := ((rw.Right - rw.Left) - (rc.Right - rc.Left)) div 2;
  dw := rw.Right - rc.Right - border;
  dh := rw.Bottom - rc.Bottom - border;
end;

//==============================================================================
//
// I_GetWindowPosition
//
//==============================================================================
procedure I_GetWindowPosition(var dw, dh: integer);
var
  rw: TRect;
begin
  GetWindowRect(hMainWnd, rw);
  dw := rw.Left;
  dh := rw.Top;
end;

end.

