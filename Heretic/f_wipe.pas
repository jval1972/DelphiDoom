//------------------------------------------------------------------------------
//
//  DelphiHeretic: A modified and improved Heretic port for Windows
//  based on original Linux Doom as published by "id Software", on
//  Heretic source as published by "Raven" software and DelphiDoom
//  as published by Jim Valavanis.
//  Copyright (C) 2004-2007 by Jim Valavanis
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
//  Mission start screen wipe/melt, special effects.
//  Mission begin melt/wipe screen special effect.
//
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : http://delphidoom.sitesled.com/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit f_wipe;

interface

procedure wipe_StartScreen;

procedure wipe_EndScreen;

function wipe_Ticker(ticks: integer): boolean;

type
  wipe_t = (
    wipe_ColorXForm,  // simple gradual pixel change for 8-bit only
    wipe_Melt,        // weird screen melt
    wipe_NUMWIPES
  );

implementation

uses
  d_delphi,
  doomdef,
  m_rnd, m_fixed,
  i_video,
  v_data, v_video,
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

procedure wipe_initMelt;
var
  i, r: integer;
  py, py1: Pfixed_t;
begin
  // copy start screen to main screen
  memcpy(screen32, wipe_scr_start, SCREENWIDTH * SCREENHEIGHT * SizeOf(LongWord));

  // setup initial column positions
  // (y<0 => not ready to scroll yet)
  yy := Pfixed_tArray(Z_Malloc(SCREENWIDTH * SizeOf(fixed_t), PU_STATIC, nil));
  py := @yy[0];
  py1 := py;
  py^ := -(M_Random mod 16);
  for i := 1 to SCREENWIDTH - 1 do
  begin
    inc(py);
    r := (M_Random mod 3) - 1;
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

end;

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
          didx := @screen32[i];
          for j := 0 to pos - 1 do
          begin
            didx^ := sidx^;
            inc(didx, SCREENWIDTH);
            inc(sidx, SCREENWIDTH);
          end;
          sidx := @wipe_scr_start[i];
          for j := pos to SCREENHEIGHT - 1 do
          begin
            didx^ := sidx^;
            inc(didx, SCREENWIDTH);
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

procedure wipe_exitMelt;
begin
  Z_Free(yy);
  memfree(pointer(wipe_scr_start), SCREENWIDTH * SCREENHEIGHT * SizeOf(LongWord));
  memfree(pointer(wipe_scr_end), SCREENWIDTH * SCREENHEIGHT * SizeOf(LongWord));
end;

procedure wipe_StartScreen;
begin
  wipe_scr_start := malloc(SCREENWIDTH * SCREENHEIGHT * SizeOf(LongWord));
  I_ReadScreen32(wipe_scr_start);
end;

procedure wipe_EndScreen;
begin
  wipe_scr_end := malloc(SCREENWIDTH * SCREENHEIGHT * SizeOf(LongWord));
  I_ReadScreen32(wipe_scr_end);
end;

// when zero, stop the wipe
var
  wiping: boolean = false;

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

