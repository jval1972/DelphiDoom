//------------------------------------------------------------------------------
//
//  DelphiHexen: A modified and improved Hexen port for Windows
//  based on original Linux Doom as published by "id Software", on
//  Hexen source as published by "Raven" software and DelphiDoom
//  as published by Jim Valavanis.
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
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit r_col_sk;

interface

// Sky column drawing functions
// Sky column drawers
procedure R_DrawSkyColumnLow;
procedure R_DrawSkyColumn;
procedure R_DrawSkyColumnHi;
procedure R_DrawSkyColumnUltra;

implementation

uses
  d_delphi,
  doomdef,
  m_fixed,
  r_draw,
  r_main,
  r_column,
  r_hires,
  v_video;

//
// Sky Column
//
procedure R_DrawSkyColumnLow;
var
  count: integer;
  i: integer;
  bdest: byte;
  dest: PByte;
  frac: fixed_t;
  fracstep: fixed_t;
  spot: integer;
  swidth: integer;
begin
  count := (dc_yh - dc_yl) div 3;

  if count < 0 then
    exit;

  dest := @((ylookup[dc_yl]^)[columnofs[dc_x]]);

  fracstep := 3 * dc_iscale;
  frac := dc_yl * dc_iscale + skytopy * FRACUNIT;
  swidth := SCREENWIDTH;

  for i := 0 to count - 1 do
  begin
  // Invert Sky Texture if below horizont level
    spot := LongWord(frac) shr FRACBITS;
    if spot > 199 then
      spot := 399 - spot;

    bdest := dc_source[spot];
    dest^ := bdest;
    inc(dest, swidth);

    dest^ := bdest;
    inc(dest, swidth);

    dest^ := bdest;
    inc(dest, swidth);

    inc(frac, fracstep);
  end;

  count := (dc_yh - dc_yl) mod 3;
  fracstep := dc_iscale;
  for i := 0 to count do
  begin
  // Invert Sky Texture if below horizont level
    spot := LongWord(frac) shr FRACBITS;
    if spot > 199 then
      spot := 399 - spot;

    dest^ := dc_source[spot];
    inc(dest, swidth);
    inc(frac, fracstep);
  end;

end;

procedure R_DrawSkyColumn;
var
  count: integer;
  i: integer;
  dest: PByte;
  frac: fixed_t;
  fracstep: fixed_t;
  spot: integer;
  swidth: integer;
begin
  count := dc_yh - dc_yl;

  if count < 0 then
    exit;

  dest := @((ylookup[dc_yl]^)[columnofs[dc_x]]);

  fracstep := dc_iscale;
  frac := dc_yl * fracstep + skytopy * FRACUNIT;
  swidth := SCREENWIDTH;

  {$I R_DrawSkyColumnMedium.inc}
end;

procedure R_DrawSkyColumnHi;
var
  count: integer;
  destl: PLongWord;
  frac: fixed_t;
  fracstep: fixed_t;
  fraclimit: fixed_t;
  spot: integer;
  swidth: integer;
  sky_max: integer;
  sky_sub: integer;
  shift: integer;
begin
  count := dc_yh - dc_yl;

  if count < 0 then
    exit;

  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);

  shift := (1 shl dc_texturefactorbits);
  fracstep := dc_iscale * shift;
  frac := dc_yl * fracstep + skytopy * (FRACUNIT * 61 div 50) * shift;
  sky_max := 256 * shift - 1;
  sky_sub := 512 * shift - 1;

  swidth := SCREENWIDTH32PITCH;

  {$I R_DrawSkyColumnHi.inc}
end;

procedure R_DrawSkyColumnUltra;
var
  count: integer;
  destl: PLongWord;
  frac: fixed_t;
  cfrac2: fixed_t;
  fracstep: fixed_t;
  fraclimit: fixed_t;
  spot: integer;
  swidth: integer;

// For inline color averaging
  r1, g1, b1: byte;
  r2, g2, b2: byte;
  c1, c2, r, g, b: LongWord;
  factor1: fixed_t;

  sky_max: integer;
  sky_sub: integer;
  shift: integer;
begin
  count := dc_yh - dc_yl;

  if count < 0 then
    exit;

  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);

  shift := (1 shl dc_texturefactorbits);
  fracstep := dc_iscale * shift;
  frac := dc_yl * fracstep + skytopy * (FRACUNIT * 61 div 50) * shift;
  sky_max := 256 * shift - 1;
  sky_sub := 512 * shift - 1;

  swidth := SCREENWIDTH32PITCH;

  {$I R_DrawSkyColumnUltra.inc}
end;

end.
