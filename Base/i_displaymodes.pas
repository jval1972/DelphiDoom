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
//  Enum display modes
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit i_displaymodes;

interface

type
  displaymode_t = record
    width, height: integer;
    bpp: integer;
  end;
  displaymode_tArray = array[0..$FF] of displaymode_t;
  Pdisplaymode_tArray = ^displaymode_tArray;

var
  displaymodes: Pdisplaymode_tArray = nil;
  numdisplaymodes: integer = 0;

//==============================================================================
//
// I_DisplayModeIndex
//
//==============================================================================
function I_DisplayModeIndex(const w, h: integer): integer;

//==============================================================================
//
// I_NearestDisplayModeIndex
//
//==============================================================================
function I_NearestDisplayModeIndex(const w, h: integer): integer;

{$IFNDEF OPENGL}

//==============================================================================
//
// I_FindWindowSize
//
//==============================================================================
procedure I_FindWindowSize(const mode: integer);
{$ENDIF}

//==============================================================================
//
// I_EnumDisplayModes
//
//==============================================================================
procedure I_EnumDisplayModes;

//==============================================================================
//
// I_ClearDisplayModes
//
//==============================================================================
procedure I_ClearDisplayModes;

// JVAL: Not the right place to put fullscreen modes
const
  FULLSCREEN_SHARED = 0;
  FULLSCREEN_EXCLUSIVE = 1;
  FULLSCREEN_OFF = 2;
  NUMFULLSCREEN_MODES = 3;

implementation

uses
  Windows,
  d_delphi,
  doomdef,
  r_hires,
  {$IFDEF OPENGL}
  gl_main,
  {$ELSE}
  i_main,
  {$ENDIF}
  i_system;

//==============================================================================
//
// SortDisplayModes
//
//==============================================================================
procedure SortDisplayModes;

  function sortvalue(const idx: integer): double;
  begin
    result := displaymodes[idx].width + displaymodes[idx].height / 1000000
  end;

  procedure qsort(l, r: Integer);
  var
    i, j: Integer;
    tmp: displaymode_t;
    rover: double;
  begin
    repeat
      i := l;
      j := r;
      rover := sortvalue((l + r) shr 1);
      repeat
        while sortvalue(i) < rover do
          inc(i);
        while sortvalue(j) > rover do
          dec(j);
        if i <= j then
        begin
          tmp := displaymodes[i];
          displaymodes[i] := displaymodes[j];
          displaymodes[j] := tmp;
          inc(i);
          dec(j);
        end;
      until i > j;
      if l < j then
        qsort(l, j);
      l := i;
    until i >= r;
  end;

begin
  if numdisplaymodes > 0 then
    qsort(0, numdisplaymodes - 1);
end;

//==============================================================================
//
// I_DisplayModeIndex
//
//==============================================================================
function I_DisplayModeIndex(const w, h: integer): integer;
var
  i: integer;
begin
  result := -1;

  if displaymodes = nil then
    exit;

  for i := 0 to numdisplaymodes - 1 do
    if (displaymodes[i].width = w) and (displaymodes[i].height = h) then
    begin
      result := i;
      exit;
    end;
end;

//==============================================================================
//
// I_NearestDisplayModeIndex
//
//==============================================================================
function I_NearestDisplayModeIndex(const w, h: integer): integer;
var
  i: integer;
  dist: double;
  mindist: double;
begin
  result := I_DisplayModeIndex(w, h);
  if result >= 0 then
    exit;

  mindist := 1000000000000.0;
  for i := 0 to numdisplaymodes - 1 do
  begin
    dist := sqrt(sqr(displaymodes[i].width - SCREENWIDTH) + sqr(displaymodes[i].height - SCREENHEIGHT));
    if SCREENWIDTH < displaymodes[i].width then
      dist := dist + 50.0;
    if SCREENHEIGHT < displaymodes[i].height then
      dist := dist + 50.0;
    if dist < mindist then
    begin
      mindist := dist;
      result := i;
    end;
  end;
end;

//==============================================================================
//
// IsAvailableScreenResolution
//
//==============================================================================
function IsAvailableScreenResolution(const w, h: integer): boolean;
begin
  result := I_DisplayModeIndex(w, h) >= 0;
end;

//==============================================================================
//
// I_EnumDisplayModes
//
//==============================================================================
procedure I_EnumDisplayModes;
var
  dm: TDevMode;
  i: integer;
begin
  if displaymodes <> nil then
    memfree(pointer(displaymodes), numdisplaymodes * SizeOf(displaymode_t));

  numdisplaymodes := 0;
  i := 0;
  while EnumDisplaySettings(nil, i, dm) do
  begin
    if (dm.dmPelsWidth >= 320) and (dm.dmPelsHeight >= 200) and (dm.dmBitsPerPel = 32) and not IsAvailableScreenResolution(dm.dmPelsWidth, dm.dmPelsHeight) then
      if (dm.dmPelsWidth <= MAXWIDTH) and (dm.dmPelsHeight <= MAXHEIGHT) then
      begin
        realloc(pointer(displaymodes), numdisplaymodes * SizeOf(displaymode_t), (numdisplaymodes + 1) * SizeOf(displaymode_t));
        displaymodes[numdisplaymodes].width := dm.dmPelsWidth;
        displaymodes[numdisplaymodes].height := dm.dmPelsHeight;
        displaymodes[numdisplaymodes].bpp := dm.dmBitsPerPel;
        inc(numdisplaymodes);
      end;
    Inc(i);
  end;
  if numdisplaymodes = 0 then
  begin
    while EnumDisplaySettings(nil, i, dm) do
    begin
      if (dm.dmPelsWidth >= 640) and (dm.dmPelsHeight >= 400) and (dm.dmBitsPerPel >= 16) and not IsAvailableScreenResolution(dm.dmPelsWidth, dm.dmPelsHeight) then
        if (dm.dmPelsWidth <= MAXWIDTH) and (dm.dmPelsHeight <= MAXHEIGHT) then
        begin
          realloc(pointer(displaymodes), numdisplaymodes * SizeOf(displaymode_t), (numdisplaymodes + 1) * SizeOf(displaymode_t));
          displaymodes[numdisplaymodes].width := dm.dmPelsWidth;
          displaymodes[numdisplaymodes].height := dm.dmPelsHeight;
          displaymodes[numdisplaymodes].bpp := dm.dmBitsPerPel;
          inc(numdisplaymodes);
        end;
      Inc(i);
    end;
  end;
  if numdisplaymodes = 0 then
  begin
    displaymodes := malloc(SizeOf(displaymode_t));
    displaymodes[0].width := 320;
    displaymodes[0].height := 200;
    displaymodes[0].bpp := 32;
    displaymodes[1].width := 640;
    displaymodes[1].height := 400;
    displaymodes[1].bpp := 32;
    numdisplaymodes := 2;
  end;

  SortDisplayModes;
end;

{$IFNDEF OPENGL}

//==============================================================================
//
// I_FindWindowSize
//
//==============================================================================
procedure I_FindWindowSize(const mode: integer);
var
  i: integer;
  dist: double;
  mindist: double;
  idx: integer;
begin
  if mode = FULLSCREEN_SHARED then
  begin
    WINDOWWIDTH := NATIVEWIDTH;
    WINDOWHEIGHT := NATIVEHEIGHT;
    exit;
  end;

  for i := 0 to numdisplaymodes - 1 do
    if displaymodes[i].width = SCREENWIDTH then
      if displaymodes[i].height = SCREENHEIGHT then
      begin
        WINDOWWIDTH := SCREENWIDTH;
        WINDOWHEIGHT := SCREENHEIGHT;
        exit;
      end;

  mindist := 1000000000000.0;
  idx := -1;
  for i := 0 to numdisplaymodes - 1 do
  begin
    dist := sqrt(sqr(displaymodes[i].width - SCREENWIDTH) + sqr(displaymodes[i].height - SCREENHEIGHT));
    if SCREENWIDTH < displaymodes[i].width then
      dist := dist + 50.0;
    if SCREENHEIGHT < displaymodes[i].height then
      dist := dist + 50.0;
    if dist < mindist then
    begin
      mindist := dist;
      idx := i;
    end;
  end;

  if idx >= 0 then
  begin
    WINDOWWIDTH := displaymodes[idx].width;
    WINDOWHEIGHT := displaymodes[idx].height;
    exit;
  end;

  WINDOWWIDTH := GetSystemMetrics(SM_CXSCREEN);
  WINDOWHEIGHT := GetSystemMetrics(SM_CYSCREEN);
end;
{$ENDIF}

//==============================================================================
//
// I_ClearDisplayModes
//
//==============================================================================
procedure I_ClearDisplayModes;
begin
  realloc(pointer(displaymodes), numdisplaymodes * SizeOf(displaymode_t), 0);
  numdisplaymodes := 0;
end;

end.
