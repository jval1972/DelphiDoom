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

unit r_col_ms_fog;

interface

//==============================================================================
// R_DrawMaskedColumnNormal_Fog
//
// Masked column drawing functions
//
//==============================================================================
procedure R_DrawMaskedColumnNormal_Fog;

//==============================================================================
//
// R_DrawMaskedColumnHi_Fog
//
//==============================================================================
procedure R_DrawMaskedColumnHi_Fog;

//==============================================================================
//
// R_DrawMaskedColumnHi32_Fog
//
//==============================================================================
procedure R_DrawMaskedColumnHi32_Fog;

//==============================================================================
//
// R_DrawMaskedColumnUltra32_Fog
//
//==============================================================================
procedure R_DrawMaskedColumnUltra32_Fog;

implementation
{$DEFINE FOG}

uses
  d_delphi,
  doomdef,
  m_fixed,
  r_precalc,
  r_draw,
  r_main,
  r_column,
  r_hires,
  v_video;

//==============================================================================
//
// R_DrawMaskedColumnNormal_Fog
//
//==============================================================================
procedure R_DrawMaskedColumnNormal_Fog;
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
  if lfactor >= 0 then
  begin
    R_GetFogPrecalc32Tables(lfactor, bf_r, bf_g, bf_b);
    {$UNDEF INVERSECOLORMAPS}
    {$I R_DrawMaskedColumnNormal.inc}
  end
  else
  begin
    {$DEFINE INVERSECOLORMAPS}
    {$I R_DrawMaskedColumnNormal.inc}
  end;
end;

//==============================================================================
//
// R_DrawMaskedColumnHi_Fog
//
//==============================================================================
procedure R_DrawMaskedColumnHi_Fog;
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
  c, r, g, b: LongWord;
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
    R_GetFogPrecalc32Tables(lfactor, bf_r, bf_g, bf_b);
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
// R_DrawMaskedColumnHi32_Fog
//
//==============================================================================
procedure R_DrawMaskedColumnHi32_Fog;
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
    R_GetFogPrecalc32Tables(lfactor, bf_r, bf_g, bf_b);
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
// R_DrawMaskedColumnUltra32_Fog
//
//==============================================================================
procedure R_DrawMaskedColumnUltra32_Fog;
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
    R_GetFogPrecalc32Tables(lfactor, bf_r, bf_g, bf_b);
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

