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
//  Mission start screen wipe/melt, special effects.
//  Mission begin melt/wipe screen special effect.
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit f_wipe;

interface

uses
  d_delphi;

//==============================================================================
//
// wipe_StartScreen
//
//==============================================================================
procedure wipe_StartScreen;

//==============================================================================
//
// wipe_EndScreen
//
//==============================================================================
procedure wipe_EndScreen;

//==============================================================================
//
// wipe_Ticker
//
//==============================================================================
function wipe_Ticker(ticks: integer): boolean;

type
  wipe_t = (
    // simple gradual pixel change for 8-bit only
    wipe_ColorXForm,
    // weird screen melt
    wipe_Melt,
    wipe_NUMWIPES
  );

{$IFDEF OPENGL}
var
  WIPESCREENWIDTH: integer;
  WIPESCREENHEIGHT: integer;

var
  w_screen32: PLongWordArray = nil;

//==============================================================================
//
// wipe_ClearMemory
//
//==============================================================================
procedure wipe_ClearMemory;
{$ENDIF}

implementation

uses
  doomdef,
  m_rnd,
  m_fixed,
  mt_utils,
{$IFDEF OPENGL}
  gl_main,
{$ELSE}
  i_video,
{$ENDIF}
  v_data,
  v_video,
  z_zone;

//
//                       SCREEN WIPE PACKAGE
//

var
  wipe_scr_start: PLongWordArray;
  wipe_scr_end: PLongWordArray;

var
  yy: Pfixed_tArray;
  vy: fixed_t;

{$IFDEF OPENGL}

//==============================================================================
//
// wipe_glsize
//
//==============================================================================
function wipe_glsize(const value: integer): integer;
begin
  result := 1;
  while result < value do
    result := result * 2;
end;
{$ENDIF}

//==============================================================================
//
// wipe_initMelt
//
//==============================================================================
procedure wipe_initMelt;
var
  i, r: integer;
  py, py1: Pfixed_t;
  SHEIGHTS: array[0..MAXWIDTH - 1] of integer;
  RANDOMS: array[0..319] of byte;
begin
{$IFDEF OPENGL}
  WIPESCREENWIDTH := wipe_glsize(SCREENWIDTH);
  WIPESCREENHEIGHT := wipe_glsize(SCREENHEIGHT);
  if w_screen32 = nil then
    w_screen32 := malloc(WIPESCREENWIDTH * WIPESCREENHEIGHT * SizeOf(LongWord));
{$ENDIF}

  for i := 0 to SCREENWIDTH - 1 do
    SHEIGHTS[i] := trunc(i * 320 / SCREENWIDTH);
  for i := 0 to 319 do
    RANDOMS[i] := I_Random;

  {$IFNDEF OPENGL}
  // copy start screen to main screen
  MT_memcpy(screen32, wipe_scr_start, SCREENWIDTH * SCREENHEIGHT * SizeOf(LongWord));
  {$ELSE}
  for i := 0 to SCREENWIDTH - 1 do
    for r := 0 to SCREENHEIGHT - 1 do
      w_screen32[r * WIPESCREENWIDTH + i] := wipe_scr_start[r * SCREENWIDTH + i];
  {$ENDIF}
  // setup initial column positions
  // (y<0 => not ready to scroll yet)
  yy := Z_Malloc(SCREENWIDTH * SizeOf(fixed_t), PU_STATIC, nil);
  py := @yy[0];
  py1 := py;

  py^ := -(RANDOMS[0] mod 16);
  for i := 1 to SCREENWIDTH - 1 do
  begin
    inc(py);
    r := (RANDOMS[SHEIGHTS[i]] mod 3) - 1;
    py^ := py1^ + r;
    if py^ > 0 then
      py^ := 0
    else if py^ = -16 then
      py^ := -15;
    inc(py1);
  end;

// JVAL change wipe timing
  vy := FRACUNIT * SCREENHEIGHT div 200;
  py := @yy[0];
  for i := 0 to SCREENWIDTH - 1 do
  begin
    py^ := py^ * vy;
    inc(py);
  end;

  for i := 1 to SCREENWIDTH - 1 do
    if SHEIGHTS[i - 1] = SHEIGHTS[i] then
      yy[i] := yy[i - 1];
end;

//==============================================================================
//
// wipe_doMelt
//
//==============================================================================
function wipe_doMelt(ticks: integer): integer;
var
  i: integer;
  j: integer;
  dy: fixed_t;
  sidx: PLongWord;
  didx: PLongWord;
  py: Pfixed_t;
  pos: integer;
begin
  result := 1;

  while ticks > 0 do
  begin
    py := @yy[0];
    for i := 0 to SCREENWIDTH - 1 do
    begin
      if py^ < 0 then
      begin
        py^ := py^ + vy;
        result := 0;
      end
      else if py^ < SCREENHEIGHT * FRACUNIT then
      begin
        if py^ <= 15 * vy then
          dy := py^ + vy
        else
          dy := 8 * vy;
        if (py^ + dy) >= SCREENHEIGHT * FRACUNIT then
          dy := SCREENHEIGHT * FRACUNIT - py^;
        if ticks = 1 then
        begin
          pos := py^ div FRACUNIT;
          sidx := @wipe_scr_end[i];
          {$IFDEF OPENGL}
          didx := @w_screen32[i];
          {$ELSE}
          didx := @screen32[i];
          {$ENDIF}
          for j := 0 to pos - 1 do
          begin
            didx^ := sidx^{$IFDEF OPENGL} or $FF000000{$ENDIF};
            {$IFDEF OPENGL}
            inc(didx, WIPESCREENWIDTH);
            {$ELSE}
            inc(didx, SCREENWIDTH);
            {$ENDIF}
            inc(sidx, SCREENWIDTH);
          end;
          sidx := @wipe_scr_start[i];
          for j := pos to SCREENHEIGHT - 1 do
          begin
            didx^ := sidx^{$IFDEF OPENGL} or $FF000000{$ENDIF};
            {$IFDEF OPENGL}
            inc(didx, WIPESCREENWIDTH);
            {$ELSE}
            inc(didx, SCREENWIDTH);
            {$ENDIF}
            inc(sidx, SCREENWIDTH);
          end;
        end;

        py^ := py^ + dy;

        result := 0;
      end;
      inc(py);
    end;
    dec(ticks);
  end;
end;

//==============================================================================
//
// wipe_exitMelt
//
//==============================================================================
procedure wipe_exitMelt;
begin
  Z_Free(yy);
  memfree(pointer(wipe_scr_start), SCREENWIDTH * SCREENHEIGHT * SizeOf(LongWord));
  memfree(pointer(wipe_scr_end), SCREENWIDTH * SCREENHEIGHT * SizeOf(LongWord));
end;

{$IFDEF OPENGL}

//==============================================================================
//
// wipe_ClearMemory
//
//==============================================================================
procedure wipe_ClearMemory;
begin
  if w_screen32 <> nil then
    memfree(pointer(w_screen32), WIPESCREENWIDTH * WIPESCREENHEIGHT * SizeOf(LongWord));
end;
{$ENDIF}

//==============================================================================
//
// wipe_StartScreen
//
//==============================================================================
procedure wipe_StartScreen;
begin
  wipe_scr_start := malloc(SCREENWIDTH * SCREENHEIGHT * SizeOf(LongWord));
  I_ReadScreen32(wipe_scr_start);
{$IFDEF OPENGL}
  I_ReverseScreen(wipe_scr_start);
{$ENDIF}
end;

//==============================================================================
//
// wipe_EndScreen
//
//==============================================================================
procedure wipe_EndScreen;
begin
  wipe_scr_end := malloc(SCREENWIDTH * SCREENHEIGHT * SizeOf(LongWord));
  I_ReadScreen32(wipe_scr_end);
{$IFDEF OPENGL}
  I_ReverseScreen(wipe_scr_end);
{$ENDIF}
end;

var
  wiping: boolean = false;

//==============================================================================
// wipe_Ticker
//
// when zero, stop the wipe
//
//==============================================================================
function wipe_Ticker(ticks: integer): boolean;
begin
  // initial stuff
  if not wiping then
  begin
    wiping := true;
    wipe_initMelt;
  end;

  // do a piece of wipe-in
  if wipe_doMelt(ticks) <> 0 then
  begin
    // final stuff
    wiping := false;
    wipe_exitMelt;
  end;

  result := not wiping;
end;

end.
