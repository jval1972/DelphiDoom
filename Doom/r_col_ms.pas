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
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit r_col_ms;

interface

//==============================================================================
// R_DrawMaskedColumnNormal
//
// Masked column drawing functions
//
//==============================================================================
procedure R_DrawMaskedColumnNormal;

//==============================================================================
//
// R_DrawMaskedColumnHi
//
//==============================================================================
procedure R_DrawMaskedColumnHi;

//==============================================================================
//
// R_DrawMaskedColumnHi32
//
//==============================================================================
procedure R_DrawMaskedColumnHi32;

//==============================================================================
//
// R_DrawMaskedColumnUltra32
//
//==============================================================================
procedure R_DrawMaskedColumnUltra32;

implementation

uses
  d_delphi,
  doomdef,
  m_fixed,
  r_precalc,
  r_draw,
  r_main,
  r_column,
  r_colormaps,
  r_hires,
  v_video;

//==============================================================================
//
// R_DrawMaskedColumnNormal
//
//==============================================================================
procedure R_DrawMaskedColumnNormal;
var
  count: integer;
  destl: PLongWord;
  spot: integer;
  frac: fixed_t;
  fracstep: fixed_t;
  fraclimit: fixed_t;
  swidth: integer;
  r1, g1, b1: byte;
  c: LongWord;
  lfactor: integer;
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

  swidth := SCREENWIDTH32PITCH;
  lfactor := dc_lightlevel;
  if customcolormap = nil then
  begin
    if lfactor >= 0 then
    begin
      R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
      {$UNDEF INVERSECOLORMAPS}
      {$UNDEF CUSTOMCOLORMAP}
      {$I R_DrawMaskedColumnNormal.inc}
    end
    else
    begin
      {$DEFINE INVERSECOLORMAPS}
      {$UNDEF CUSTOMCOLORMAP}
      {$I R_DrawMaskedColumnNormal.inc}
    end;
  end
  else
  begin
    if lfactor >= 0 then
    begin
      R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
      {$UNDEF INVERSECOLORMAPS}
      {$DEFINE CUSTOMCOLORMAP}
      {$I R_DrawMaskedColumnNormal.inc}
    end
    else
    begin
      {$DEFINE INVERSECOLORMAPS}
      {$DEFINE CUSTOMCOLORMAP}
      {$I R_DrawMaskedColumnNormal.inc}
    end;
  end;
end;

//==============================================================================
//
// R_DrawMaskedColumnHi
//
//==============================================================================
procedure R_DrawMaskedColumnHi;
var
  count: integer;
  destl: PLongWord;
  spot: integer;
  spot2: integer;
  fmod: fixed_t;
  frac: fixed_t;
  fracstep: fixed_t;
  fraclimit: fixed_t;
  swidth: integer;
  r1, g1, b1: byte;
  c: LongWord;
  lfactor: integer;
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

  swidth := SCREENWIDTH32PITCH;
  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
    {$UNDEF INVERSECOLORMAPS}
    {$I R_DrawMaskedColumnHi.inc}
  end
  else
  begin
    {$DEFINE INVERSECOLORMAPS}
    {$I R_DrawMaskedColumnHi.inc}
  end;
end;

//==============================================================================
//
// R_DrawMaskedColumnHi32
//
//==============================================================================
procedure R_DrawMaskedColumnHi32;
var
  count: integer;
  destl: PLongWord;
  frac: fixed_t;
  fracstep: fixed_t;
  fraclimit: fixed_t;
  spot: integer;
  swidth: integer;
  r1, g1, b1: byte;
  c: LongWord;
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

  fracstep := fracstep * (1 shl dc_texturefactorbits);
  frac := frac * (1 shl dc_texturefactorbits);
  and_mask := 128 * (1 shl dc_texturefactorbits) - 1;

  swidth := SCREENWIDTH32PITCH;
  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
    {$UNDEF INVERSECOLORMAPS}
    {$DEFINE MASKEDCOLUMN}
    {$UNDEF SMALLSTEPOPTIMIZER}
    {$I R_DrawColumnHi.inc}
  end
  else
  begin
    {$DEFINE INVERSECOLORMAPS}
    {$DEFINE MASKEDCOLUMN}
    {$UNDEF SMALLSTEPOPTIMIZER}
    {$I R_DrawColumnHi.inc}
  end;
end;

//==============================================================================
//
// R_DrawMaskedColumnUltra32
//
//==============================================================================
procedure R_DrawMaskedColumnUltra32;
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
  frac := dc_texturemid + (dc_yl - centery) * fracstep - FRACUNIT div 2;

  fracstep := fracstep * (1 shl dc_texturefactorbits);
  frac := frac * (1 shl dc_texturefactorbits);
  and_mask := 128 * (1 shl dc_texturefactorbits) - 1;

  swidth := SCREENWIDTH32PITCH;
  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
    {$UNDEF INVERSECOLORMAPS}
    {$DEFINE MASKEDCOLUMN}
    {$I R_DrawColumnUltra.inc}
  end
  else
  begin
    {$DEFINE INVERSECOLORMAPS}
    {$DEFINE MASKEDCOLUMN}
    {$I R_DrawColumnUltra.inc}
  end;
end;

end.

