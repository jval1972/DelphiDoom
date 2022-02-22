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
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit r_col_sk;

interface

//==============================================================================
// R_DrawSkyColumnLow
//
// Sky column drawing functions
// Sky column drawers
//
//==============================================================================
procedure R_DrawSkyColumnLow;

//==============================================================================
//
// R_DrawSkyColumn
//
//==============================================================================
procedure R_DrawSkyColumn;

//==============================================================================
//
// R_DrawSkyColumnHi
//
//==============================================================================
procedure R_DrawSkyColumnHi;

//==============================================================================
//
// R_DrawSkyColumnUltra
//
//==============================================================================
procedure R_DrawSkyColumnUltra;

implementation

uses
  d_delphi,
  doomdef,
  m_fixed,
  r_draw,
  r_main,
  r_column;

//==============================================================================
// R_DrawSkyColumnLow
//
// Sky Column
//
//==============================================================================
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
  frac := dc_texturemid + (dc_yl - centery) * dc_iscale;
  swidth := SCREENWIDTH;

  for i := 0 to count - 1 do
  begin
  // Invert Sky Texture if below horizont level
    spot := (LongWord(frac) shr FRACBITS) and 127;

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
    spot := (LongWord(frac) shr FRACBITS) and 127;
    dest^ := dc_source[spot];
    inc(dest, swidth);
    inc(frac, fracstep);
  end;

end;

//==============================================================================
//
// R_DrawSkyColumn
//
//==============================================================================
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
  frac := dc_texturemid + (dc_yl - centery) * fracstep;
  swidth := SCREENWIDTH;

  {$I R_DrawSkyColumnMedium.inc}
end;

//==============================================================================
//
// R_DrawSkyColumnHi
//
//==============================================================================
procedure R_DrawSkyColumnHi;
var
  count: integer;
  destl: PLongWord;
  frac: fixed_t;
  fracstep: fixed_t;
  fraclimit: fixed_t;
  spot: integer;
  swidth: integer;
  and_mask: integer;
begin
  count := dc_yh - dc_yl;

  if count < 0 then
    exit;

  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);

  fracstep := dc_iscale;
  frac := dc_texturemid + (dc_yl - centery) * fracstep;

  fracstep := fracstep * (1 shl dc_texturefactorbits);
  frac := frac * (1 shl dc_texturefactorbits);
  and_mask := 128 * (1 shl dc_texturefactorbits) - 1;

  swidth := SCREENWIDTH32PITCH;
  {$I R_DrawSkyColumnHi.inc}
end;

//==============================================================================
//
// R_DrawSkyColumnUltra
//
//==============================================================================
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
  and_mask: integer;

// For inline color averaging
  r1, g1, b1: byte;
  r2, g2, b2: byte;
  c1, c2, r, g, b: LongWord;
  factor1: fixed_t;

begin
  count := dc_yh - dc_yl;

  if count < 0 then
    exit;

  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);

  fracstep := dc_iscale;
  frac := dc_texturemid + (dc_yl - centery) * fracstep - FRACUNIT div 2;

  fracstep := fracstep * (1 shl dc_texturefactorbits);
  frac := frac * (1 shl dc_texturefactorbits);
  and_mask := 128 * (1 shl dc_texturefactorbits) - 1;

  swidth := SCREENWIDTH32PITCH;

  {$I R_DrawSkyColumnUltra.inc}
end;

end.
