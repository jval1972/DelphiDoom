//------------------------------------------------------------------------------
//
//  DelphiStrife is a source port of the game Strife.
//
//  Based on:
//    - Linux Doom by "id Software"
//    - Chocolate Strife by "Simon Howard"
//    - DelphiDoom by "Jim Valavanis"
//
//  Copyright (C) 1993-1996 by id Software, Inc.
//  Copyright (C) 2005 Simon Howard
//  Copyright (C) 2010 James Haley, Samuel Villarreal
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
//  Fade screen special effect.
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit f_fade;

interface

uses
  d_delphi;

//==============================================================================
//
// fade_StartScreen
//
//==============================================================================
procedure fade_StartScreen;

//==============================================================================
//
// fade_EndScreen
//
//==============================================================================
procedure fade_EndScreen;

//==============================================================================
//
// fade_Ticker
//
//==============================================================================
function fade_Ticker(ticks: integer): boolean;

{$IFDEF OPENGL}
var
  WIPESCREENWIDTH: integer;
  WIPESCREENHEIGHT: integer;

var
  w_screen32: PLongWordArray = nil;

//==============================================================================
//
// fade_ClearMemory
//
//==============================================================================
procedure fade_ClearMemory;
{$ENDIF}

implementation

uses
  doomdef,
  r_hires,
  m_fixed,
{$IFNDEF OPENGL}
  mt_utils,
{$ENDIF}
  i_system,
{$IFDEF OPENGL}
  gl_main,
{$ELSE}
  i_video,
{$ENDIF}
  v_data;

var
  fade_scr_start: PLongWordArray;
  fade_scr_end: PLongWordArray;

var
  fade: fixed_t = 0;

{$IFDEF OPENGL}

//==============================================================================
//
// fade_glsize
//
//==============================================================================
function fade_glsize(const value: integer): integer;
begin
  result := 1;
  while result < value do
    result := result * 2;
end;
{$ENDIF}

//==============================================================================
//
// fade_initFade
//
//==============================================================================
procedure fade_initFade;
{$IFDEF OPENGL}
var
  i, r: integer;
{$ENDIF}
begin
{$IFDEF OPENGL}
  WIPESCREENWIDTH := fade_glsize(SCREENWIDTH);
  WIPESCREENHEIGHT := fade_glsize(SCREENHEIGHT);
  if w_screen32 = nil then
    w_screen32 := malloc(WIPESCREENWIDTH * WIPESCREENHEIGHT * SizeOf(LongWord));
  // copy start screen to main screen
  for i := 0 to SCREENWIDTH - 1 do
    for r := 0 to SCREENHEIGHT - 1 do
      w_screen32[r * WIPESCREENWIDTH + i] := fade_scr_start[r * SCREENWIDTH + i];
{$ELSE}
  MT_memcpy(screen32, fade_scr_start, SCREENWIDTH * SCREENHEIGHT * SizeOf(LongWord));
{$ENDIF}
  fade := FRACUNIT;
end;

{$IFNDEF OPENGL}

//==============================================================================
//
// fade_doFade_thr
//
//==============================================================================
function fade_doFade_thr(p: mt_range_p): integer; stdcall;
var
  i: integer;
begin
  for i := p.start to p.finish do
    screen32[i] := R_ColorAverage(fade_scr_end[i], fade_scr_start[i], fade);
  result := 0;
end;
{$ENDIF}

//==============================================================================
//
// fade_doFade
//
//==============================================================================
function fade_doFade(ticks: integer): integer;
var
  i: integer;
{$IFDEF OPENGL}
  j: integer;
{$ELSE}
  r1, r2, r3, r4: mt_range_t;
{$ENDIF}
begin
  result := 1;

  if fade = 0 then
    exit;

  fade := fade - ticks * 4096;
  if fade < 0 then
    fade := 0;

  {$IFDEF OPENGL}
  for i := 0 to SCREENWIDTH - 1 do
    for j := 0 to SCREENHEIGHT - 1 do
      w_screen32[j * WIPESCREENWIDTH  + i] := R_ColorAverage(fade_scr_end[j * SCREENWIDTH + i], fade_scr_start[j * SCREENWIDTH + i], fade) or $FF000000;
  {$ELSE}
  if usemultithread then
  begin
    r1.start := 0;
    r1.finish := SCREENWIDTH * SCREENHEIGHT div 4 - 1;
    r2.start := r1.finish + 1;
    r2.finish := SCREENWIDTH * SCREENHEIGHT div 2 - 1;
    r3.start := r2.finish + 1;
    r3.finish := 3 * (SCREENWIDTH * SCREENHEIGHT div 4) - 1;
    r4.start := r3.finish + 1;
    r4.finish := SCREENWIDTH * SCREENHEIGHT - 1;
    MT_Execute(
      @fade_doFade_thr, @r1,
      @fade_doFade_thr, @r2,
      @fade_doFade_thr, @r3,
      @fade_doFade_thr, @r4
    );
  end
  else
    for i := 0 to SCREENWIDTH * SCREENHEIGHT - 1 do
      screen32[i] := R_ColorAverage(fade_scr_end[i], fade_scr_start[i], fade);
  {$ENDIF}
  if fade > 0 then
    result := 0;
end;

//==============================================================================
//
// fade_exitFade
//
//==============================================================================
procedure fade_exitFade;
begin
  memfree(pointer(fade_scr_start), SCREENWIDTH * SCREENHEIGHT * SizeOf(LongWord));
  memfree(pointer(fade_scr_end), SCREENWIDTH * SCREENHEIGHT * SizeOf(LongWord));
end;

{$IFDEF OPENGL}

//==============================================================================
//
// fade_ClearMemory
//
//==============================================================================
procedure fade_ClearMemory;
begin
  if w_screen32 <> nil then
    memfree(pointer(w_screen32), WIPESCREENWIDTH * WIPESCREENHEIGHT * SizeOf(LongWord));
end;
{$ENDIF}

//==============================================================================
//
// fade_StartScreen
//
//==============================================================================
procedure fade_StartScreen;
begin
  fade_scr_start := malloc(SCREENWIDTH * SCREENHEIGHT * SizeOf(LongWord));
  I_ReadScreen32(fade_scr_start);
{$IFDEF OPENGL}
  I_ReverseScreen(fade_scr_start);
{$ENDIF}
end;

//==============================================================================
//
// fade_EndScreen
//
//==============================================================================
procedure fade_EndScreen;
begin
  fade_scr_end := malloc(SCREENWIDTH * SCREENHEIGHT * SizeOf(LongWord));
  I_ReadScreen32(fade_scr_end);
{$IFDEF OPENGL}
  I_ReverseScreen(fade_scr_end);
{$ENDIF}
end;

// when zero, stop the fade
var
  fading: boolean = false;

//==============================================================================
//
// fade_Ticker
//
//==============================================================================
function fade_Ticker(ticks: integer): boolean;
begin
  // initial stuff
  if not fading then
  begin
    fading := true;
    fade_initFade;
  end;

  // do a piece of fade
  if fade_doFade(ticks) <> 0 then
  begin
    // final stuff
    fading := false;
    fade_exitFade;
  end;

  result := not fading;
end;

end.
