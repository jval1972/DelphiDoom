//------------------------------------------------------------------------------
//
//  DelphiHexen is a source port of the game Hexen and it is
//  based on original Linux Doom as published by "id Software", on
//  Hexen source as published by "Raven" software and DelphiDoom
//  as published by Jim Valavanis.
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
//  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
//  02111-1307, USA.
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit r_col_fog;

interface

//==============================================================================
//
// R_DrawColumnHi_Fog
//
//==============================================================================
procedure R_DrawColumnHi_Fog;

//==============================================================================
//
// R_DrawColumnUltra_Fog
//
//==============================================================================
procedure R_DrawColumnUltra_Fog;

implementation

uses
  d_delphi,
  m_fixed,
  doomdef,
  doomtype,
  r_draw,
  r_main,
  r_column,
  r_precalc;

//==============================================================================
//
// R_DrawColumnHi_Fog
//
//==============================================================================
procedure R_DrawColumnHi_Fog;
var
  count: integer;
  destl: PLongWord;
  frac: fixed_t;
  fracstep: fixed_t;
  fraclimit: fixed_t;
  spot: integer;
  swidth: integer;

  r1, g1, b1: byte;
  c, c1, r, g, b: LongWord;
  lfactor: integer;
  lspot: integer;
  ldest: LongWord;
  and_mask: integer;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
begin
  count := dc_yh - dc_yl;

  if count < 0 then
    exit;

  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);

  fracstep := dc_iscale;
  frac := dc_texturemid + (dc_yl - centery) * fracstep;

  if dc_texturefactorbits > 0 then
  begin
    fracstep := fracstep * (1 shl dc_texturefactorbits);
    frac := frac * (1 shl dc_texturefactorbits);
    and_mask := 128 * (1 shl dc_texturefactorbits) - 1;
  end
  else
    and_mask := 127;

  swidth := SCREENWIDTH32PITCH;
  lfactor := dc_lightlevel;
  if fracstep > 2 * FRACUNIT div 5 then
  begin
    if lfactor >= 0 then
    begin
      R_GetFogPrecalc32Tables(lfactor, bf_r, bf_g, bf_b);
      {$UNDEF INVERSECOLORMAPS}
      {$UNDEF MASKEDCOLUMN}
      {$DEFINE FOG}
      {$UNDEF SMALLSTEPOPTIMIZER}
      {$I R_DrawColumnHi.inc}
    end
    else
    begin
      {$DEFINE INVERSECOLORMAPS}
      {$UNDEF MASKEDCOLUMN}
      {$DEFINE FOG}
      {$UNDEF SMALLSTEPOPTIMIZER}
      {$I R_DrawColumnHi.inc}
    end;
  end
  else
  begin
    lspot := MININT;
    ldest := 0;
    if lfactor >= 0 then
    begin
      R_GetFogPrecalc32Tables(lfactor, bf_r, bf_g, bf_b);
      {$UNDEF INVERSECOLORMAPS}
      {$UNDEF MASKEDCOLUMN}
      {$DEFINE FOG}
      {$DEFINE SMALLSTEPOPTIMIZER}
      {$I R_DrawColumnHi.inc}
    end
    else
    begin
      {$DEFINE INVERSECOLORMAPS}
      {$UNDEF MASKEDCOLUMN}
      {$DEFINE FOG}
      {$DEFINE SMALLSTEPOPTIMIZER}
      {$I R_DrawColumnHi.inc}
    end;
  end;
end;

//==============================================================================
//
// R_DrawColumnUltra_Fog
//
//==============================================================================
procedure R_DrawColumnUltra_Fog;
var
  count: integer;
  destl: PLongWord;
  frac: fixed_t;
  fracstep: fixed_t;
  fraclimit: fixed_t;
  cfrac2: fixed_t;
  spot: integer;
  swidth: integer;

// For inline color averaging
  r1, g1, b1: byte;
  r2, g2, b2: byte;
  c, c1, c2, r, g, b: LongWord;
  factor1: fixed_t;
  factor2: fixed_t;
  lfactor: integer;
  and_mask: integer;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
begin

  count := dc_yh - dc_yl;

  if count < 0 then
    exit;

  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);

  fracstep := dc_iscale;
  frac := dc_texturemid + (dc_yl - centery) * fracstep;

  if dc_texturefactorbits > 0 then
  begin
    fracstep := fracstep * (1 shl dc_texturefactorbits);
    frac := frac * (1 shl dc_texturefactorbits);
    and_mask := 128 * (1 shl dc_texturefactorbits) - 1;
  end
  else
    and_mask := 127;

  swidth := SCREENWIDTH32PITCH;
  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetFogPrecalc32Tables(lfactor, bf_r, bf_g, bf_b);
    {$UNDEF INVERSECOLORMAPS}
    {$UNDEF MASKEDCOLUMN}
    {$DEFINE FOG}
    {$I R_DrawColumnUltra.inc}
  end
  else
  begin
    {$DEFINE INVERSECOLORMAPS}
    {$UNDEF MASKEDCOLUMN}
    {$DEFINE FOG}
    {$I R_DrawColumnUltra.inc}
  end;
end;

end.

