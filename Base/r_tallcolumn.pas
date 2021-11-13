//------------------------------------------------------------------------------
//
//  DelphiDoom: A modified and improved DOOM engine for Windows
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 1993-1996 by id Software, Inc.
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
//  Foundation, inc., 59 Temple Place - Suite 330, Boston, MA
//  02111-1307, USA.
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit r_tallcolumn;

interface

uses
  d_delphi,
  m_fixed,
  r_main;

// Column drawers
procedure R_DrawTallColumnLowest;
procedure R_DrawTallColumnLow;
procedure R_DrawTallColumnMedium;
procedure R_DrawTallColumnHi;
procedure R_DrawTallColumnUltra;
{$IFDEF HEXEN}
procedure R_DrawTallColumnHi_Fog;
procedure R_DrawTallColumnUltra_Fog;
{$ENDIF}
procedure R_DrawTallColumnBase32;

var
//
// R_DrawTallColumn
// Source is the top of the column to scale.
//
  dc_height: LongWord;

implementation

uses
  doomdef,
  doomtype,
  r_precalc,
  r_column,
  r_data,
  r_draw,
  r_hires,
  v_video;

procedure R_DrawTallColumnLowest;
var
  count: integer;
  i: integer;
  dest: PByte;
  frac: fixed_t;
  fracstep: fixed_t;
  swidth: integer;
  buf: twobytes_t;
begin
  if odd(dc_x) then
    exit;

  count := (dc_yh - dc_yl) div 3;

  if count < 0 then
    exit;

  dest := @((ylookup[dc_yl]^)[columnofs[dc_x]]);

  frac := dc_texturemid + (dc_yl - centery) * dc_iscale;
  fracstep := 3 * dc_iscale;
  swidth := SCREENWIDTH;

  for i := 0 to count - 1 do
  begin
    buf.byte1 := dc_colormap[dc_source[(LongWord(frac) shr FRACBITS) mod dc_height]];
    buf.byte2 := buf.byte1;

    PWord(dest)^ := Word(buf);
    inc(dest, swidth);

    PWord(dest)^ := Word(buf);
    inc(dest, swidth);

    PWord(dest)^ := Word(buf);
    inc(dest, swidth);

    inc(frac, fracstep);
  end;

  count := (dc_yh - dc_yl) mod 3;
  for i := 0 to count do
  begin
    buf.byte1 := dc_colormap[dc_source[(LongWord(frac) shr FRACBITS) mod dc_height]];
    buf.byte2 := buf.byte1;
    PWord(dest)^ := Word(buf);
    inc(dest, swidth);

    inc(frac, dc_iscale);
  end;
end;

procedure R_DrawTallColumnLow;
var
  count: integer;
  i: integer;
  dest: PByte;
  bdest: byte;
  frac: fixed_t;
  fracstep: fixed_t;
  swidth: integer;
begin
  count := (dc_yh - dc_yl) div 3;

  if count < 0 then
    exit;

  dest := @((ylookup[dc_yl]^)[columnofs[dc_x]]);

  frac := dc_texturemid + (dc_yl - centery) * dc_iscale;
  fracstep := 3 * dc_iscale;
  swidth := SCREENWIDTH;

  for i := 0 to count - 1 do
  begin
    bdest := dc_colormap[dc_source[(LongWord(frac) shr FRACBITS) mod dc_height]];

    dest^ := bdest;
    inc(dest, swidth);

    dest^ := bdest;
    inc(dest, swidth);

    dest^ := bdest;
    inc(dest, swidth);

    inc(frac, fracstep);
  end;

  count := (dc_yh - dc_yl) mod 3;
  for i := 0 to count do
  begin
    dest^ := dc_colormap[dc_source[(LongWord(frac) shr FRACBITS) mod dc_height]];
    inc(dest, swidth);

    inc(frac, dc_iscale);
  end;
end;

procedure R_DrawTallColumnMedium;
var
  count: integer;
  dest: PByte;
  frac: fixed_t;
  fracstep: fixed_t;
  fraclimit: fixed_t;
  fraclimit2: fixed_t;
  swidth: integer;
  dc_local: PByteArray;
begin
  count := dc_yh - dc_yl;

  // Zero length, column does not exceed a pixel.
  if count < 0 then
    exit;

  // Framebuffer destination address.
  // Use ylookup LUT to avoid multiply with ScreenWidth.
  // Use columnofs LUT for subwindows?
  dest := @((ylookup[dc_yl]^)[columnofs[dc_x]]);

  // Determine scaling,
  //  which is the only mapping to be done.
  fracstep := dc_iscale;
  frac := dc_texturemid + (dc_yl - centery) * fracstep;
  fraclimit := frac + count * fracstep;
  fraclimit2 := frac + (count - 16) * fracstep;
  swidth := SCREENWIDTH;
  dc_local := dc_source;

  // Inner loop that does the actual texture mapping,
  //  e.g. a DDA-lile scaling.
  // This is as fast as it gets.
  while frac <= fraclimit2 do
  begin
  // Re-map color indices from wall texture column
  //  using a lighting/special effects LUT.
    dest^ := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) mod dc_height]];
    inc(dest, swidth);
    inc(frac, fracstep);

    dest^ := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) mod dc_height]];
    inc(dest, swidth);
    inc(frac, fracstep);

    dest^ := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) mod dc_height]];
    inc(dest, swidth);
    inc(frac, fracstep);

    dest^ := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) mod dc_height]];
    inc(dest, swidth);
    inc(frac, fracstep);

    dest^ := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) mod dc_height]];
    inc(dest, swidth);
    inc(frac, fracstep);

    dest^ := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) mod dc_height]];
    inc(dest, swidth);
    inc(frac, fracstep);

    dest^ := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) mod dc_height]];
    inc(dest, swidth);
    inc(frac, fracstep);

    dest^ := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) mod dc_height]];
    inc(dest, swidth);
    inc(frac, fracstep);

    dest^ := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) mod dc_height]];
    inc(dest, swidth);
    inc(frac, fracstep);

    dest^ := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) mod dc_height]];
    inc(dest, swidth);
    inc(frac, fracstep);

    dest^ := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) mod dc_height]];
    inc(dest, swidth);
    inc(frac, fracstep);

    dest^ := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) mod dc_height]];
    inc(dest, swidth);
    inc(frac, fracstep);

    dest^ := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) mod dc_height]];
    inc(dest, swidth);
    inc(frac, fracstep);

    dest^ := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) mod dc_height]];
    inc(dest, swidth);
    inc(frac, fracstep);

    dest^ := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) mod dc_height]];
    inc(dest, swidth);
    inc(frac, fracstep);

    dest^ := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) mod dc_height]];
    inc(dest, swidth);
    inc(frac, fracstep);
  end;

  while frac <= fraclimit do
  begin
  // Re-map color indices from wall texture column
  //  using a lighting/special effects LUT.
    dest^ := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) mod dc_height]];

    inc(dest, swidth);
    inc(frac, fracstep);
  end;
end;

procedure R_DrawTallColumnBase32;
var
  count: integer;
  i: integer;
  destl: PLongWord;
  frac: fixed_t;
  fracstep: fixed_t;
  spot: integer;
begin
  count := dc_yh - dc_yl;

  if count < 0 then
    exit;

  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);

  fracstep := dc_iscale;
  frac := dc_texturemid + (dc_yl - centery) * fracstep;
  for i := 0 to count do
  begin
    spot := (LongWord(frac) shr FRACBITS) mod dc_height;
    destl^ := dc_colormap32[dc_source[spot]];

    inc(destl, SCREENWIDTH);
    inc(frac, fracstep);
  end;
end;

procedure R_DrawTallColumnHi;
var
  count: integer;
  destl: PLongWord;
  frac: fixed_t;
  fracstep: fixed_t;
  fraclimit: fixed_t;
  fraclimit2: fixed_t;
  spot: integer;
  swidth: integer;

  r1, g1, b1: byte;
  c: LongWord;
  lfactor: integer;
  lspot: integer;
  ldest: LongWord;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  mod_height: LongWord;
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
    mod_height := dc_height * (1 shl dc_texturefactorbits);
  end
  else
    mod_height := dc_height;

  swidth := SCREENWIDTH32PITCH;
  lfactor := dc_lightlevel;
  if fracstep > 2 * FRACUNIT div 5 then
  begin
    if lfactor >= 0 then
    begin
      R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);
      {$UNDEF INVERSECOLORMAPS}
      {$UNDEF FOG}
      {$UNDEF MASKEDCOLUMN}
      {$UNDEF SMALLSTEPOPTIMIZER}
      {$I R_DrawTallColumnHi.inc}
    end
    else
    begin
      {$DEFINE INVERSECOLORMAPS}
      {$UNDEF FOG}
      {$UNDEF MASKEDCOLUMN}
      {$UNDEF SMALLSTEPOPTIMIZER}
      {$I R_DrawTallColumnHi.inc}
    end;
  end
  else
  begin
    lspot := MININT;
    ldest := 0;
    if lfactor >= 0 then
    begin
      R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);
      {$UNDEF INVERSECOLORMAPS}
      {$UNDEF FOG}
      {$UNDEF MASKEDCOLUMN}
      {$DEFINE SMALLSTEPOPTIMIZER}
      {$I R_DrawTallColumnHi.inc}
    end
    else
    begin
      {$DEFINE INVERSECOLORMAPS}
      {$UNDEF FOG}
      {$UNDEF MASKEDCOLUMN}
      {$DEFINE SMALLSTEPOPTIMIZER}
      {$I R_DrawTallColumnHi.inc}
    end;
  end;
end;

procedure R_DrawTallColumnUltra;
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
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  mod_height: LongWord;
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
    mod_height := dc_height * (1 shl dc_texturefactorbits);
  end
  else
    mod_height := dc_height;

  swidth := SCREENWIDTH32PITCH;
  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);
    {$UNDEF INVERSECOLORMAPS}
    {$UNDEF FOG}
    {$UNDEF MASKEDCOLUMN}
    {$I R_DrawTallColumnUltra.inc}
  end
  else
  begin
    {$DEFINE INVERSECOLORMAPS}
    {$UNDEF FOG}
    {$UNDEF MASKEDCOLUMN}
    {$I R_DrawTallColumnUltra.inc}
  end;
end;

{$IFDEF HEXEN}
procedure R_DrawTallColumnHi_Fog;
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
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  mod_height: LongWord;
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
    mod_height := dc_height * (1 shl dc_texturefactorbits);
  end
  else
    mod_height := dc_height;

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
      {$I R_DrawTallColumnHi.inc}
    end
    else
    begin
      {$DEFINE INVERSECOLORMAPS}
      {$UNDEF MASKEDCOLUMN}
      {$DEFINE FOG}
      {$UNDEF SMALLSTEPOPTIMIZER}
      {$I R_DrawTallColumnHi.inc}
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
      {$I R_DrawTallColumnHi.inc}
    end
    else
    begin
      {$DEFINE INVERSECOLORMAPS}
      {$UNDEF MASKEDCOLUMN}
      {$DEFINE FOG}
      {$DEFINE SMALLSTEPOPTIMIZER}
      {$I R_DrawTallColumnHi.inc}
    end;
  end;
end;

procedure R_DrawTallColumnUltra_Fog;
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
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  mod_height: LongWord;
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
    mod_height := dc_height * (1 shl dc_texturefactorbits);
  end
  else
    mod_height := dc_height;

  swidth := SCREENWIDTH32PITCH;
  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetFogPrecalc32Tables(lfactor, bf_r, bf_g, bf_b);
    {$UNDEF INVERSECOLORMAPS}
    {$UNDEF MASKEDCOLUMN}
    {$DEFINE FOG}
    {$I R_DrawTallColumnUltra.inc}
  end
  else
  begin
    {$DEFINE INVERSECOLORMAPS}
    {$UNDEF MASKEDCOLUMN}
    {$DEFINE FOG}
    {$I R_DrawTallColumnUltra.inc}
  end;
end;
{$ENDIF}

end.

