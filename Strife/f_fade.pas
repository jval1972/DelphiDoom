//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//
//  DelphiStrife: A modified and improved Strife source port for Windows.
//
//  Based on:
//    - Linux Doom by "id Software"
//    - Chocolate Strife by "Simon Howard"
//    - DelphiDoom by "Jim Valavanis"
//
//  Copyright (C) 2004-2016 by Jim Valavanis
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
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit f_fade;

interface

procedure fade_StartScreen;

procedure fade_EndScreen;

function fade_Ticker(ticks: integer): boolean;

implementation

uses
  d_delphi,
  doomdef,
  r_hires,
  m_fixed,
  i_video,
  v_data,
  v_video;

var
  fade_scr_start: PLongWordArray;
  fade_scr_end: PLongWordArray;

var
  fade: fixed_t = 0;

procedure fade_initFade;
begin
  // copy start screen to main screen
  memcpy(screen32, fade_scr_start, SCREENWIDTH * SCREENHEIGHT * SizeOf(LongWord));
  fade := FRACUNIT;
end;

function fade_doFade(ticks: integer): integer;
var
  i: integer;
begin
  result := 1;

  if fade = 0 then
    exit;

  fade := fade - ticks * 4096;
  if fade < 0 then
    fade := 0;

  for i := 0 to SCREENWIDTH * SCREENHEIGHT - 1 do
    screen32[i] := R_ColorAverage(fade_scr_end[i], fade_scr_start[i], fade);

  if fade > 0 then
    result := 0;
end;

procedure fade_exitFade;
begin
  memfree(pointer(fade_scr_start), SCREENWIDTH * SCREENHEIGHT * SizeOf(LongWord));
  memfree(pointer(fade_scr_end), SCREENWIDTH * SCREENHEIGHT * SizeOf(LongWord));
end;

procedure fade_StartScreen;
begin
  fade_scr_start := malloc(SCREENWIDTH * SCREENHEIGHT * SizeOf(LongWord));
  I_ReadScreen32(fade_scr_start);
end;

procedure fade_EndScreen;
begin
  fade_scr_end := malloc(SCREENWIDTH * SCREENHEIGHT * SizeOf(LongWord));
  I_ReadScreen32(fade_scr_end);
end;

// when zero, stop the fade
var
  fading: boolean = false;

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
