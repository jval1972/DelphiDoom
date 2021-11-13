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
//  Foundation, inc., 59 Temple Place - Suite 330, Boston, MA
//  02111-1307, USA.
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit r_column;

interface

uses
  d_delphi,
  m_fixed,
  r_main;

// Column drawers
procedure R_DrawColumnLowest;
procedure R_DrawColumnLow;
procedure R_DrawColumnMedium;
procedure R_DrawColumnHi;
procedure R_DrawColumnUltra;

procedure R_DrawColumnBase32;

var
//
// R_DrawColumn
// Source is the top of the column to scale.
//
  dc_colormap: PByteArray;
  dc_colormap32: PLongWordArray;
  dc_lightlevel: fixed_t;
  dc_llindex: integer;
  dc_fog: boolean;  // JVAL: Mars fog sectors
  dc_fog2: boolean;  // JVAL: Mars fog sectors

  dc_iscale: fixed_t;
  dc_texturemid: fixed_t;
  dc_x: integer;
  dc_yl: integer;
  dc_yh: integer;
  dc_mod: integer; // JVAL for hi resolution
  dc_texturemod: integer; // JVAL for external textures
  dc_texturefactorbits: integer; // JVAL for hi resolution
  dc_palcolor: LongWord;
  dc_alpha: fixed_t;

const
  MAXTEXTUREFACTORBITS = 3; // JVAL: Allow hi resolution textures x 8

var
// first pixel in a column (possibly virtual)
  dc_source: PByteArray;
// JVAL for hi resolution
  dc_source32: PLongWordArray;

implementation

uses
  doomdef,
  doomtype,
  r_precalc,
  r_data,
  r_draw,
  r_hires,
  v_video;

//
// A column is a vertical slice/span from a wall texture that,
//  given the DOOM style restrictions on the view orientation,
//  will always have constant z depth.
// Thus a special case loop for very fast rendering can
//  be used. It has also been used with Wolfenstein 3D.
//
procedure R_DrawColumnLowest;
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
    buf.byte1 := dc_colormap[dc_source[(LongWord(frac) shr FRACBITS) and 127]];
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
    buf.byte1 := dc_colormap[dc_source[(LongWord(frac) shr FRACBITS) and 127]];
    buf.byte2 := buf.byte1;
    PWord(dest)^ := Word(buf);
    inc(dest, swidth);

    inc(frac, dc_iscale);
  end;
end;

procedure R_DrawColumnLow;
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
    bdest := dc_colormap[dc_source[(LongWord(frac) shr FRACBITS) and 127]];

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
    dest^ := dc_colormap[dc_source[(LongWord(frac) shr FRACBITS) and 127]];
    inc(dest, swidth);

    inc(frac, dc_iscale);
  end;
end;

procedure R_DrawColumnMedium;
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
    dest^ := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    inc(dest, swidth);
    inc(frac, fracstep);

    dest^ := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    inc(dest, swidth);
    inc(frac, fracstep);

    dest^ := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    inc(dest, swidth);
    inc(frac, fracstep);

    dest^ := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    inc(dest, swidth);
    inc(frac, fracstep);

    dest^ := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    inc(dest, swidth);
    inc(frac, fracstep);

    dest^ := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    inc(dest, swidth);
    inc(frac, fracstep);

    dest^ := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    inc(dest, swidth);
    inc(frac, fracstep);

    dest^ := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    inc(dest, swidth);
    inc(frac, fracstep);

    dest^ := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    inc(dest, swidth);
    inc(frac, fracstep);

    dest^ := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    inc(dest, swidth);
    inc(frac, fracstep);

    dest^ := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    inc(dest, swidth);
    inc(frac, fracstep);

    dest^ := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    inc(dest, swidth);
    inc(frac, fracstep);

    dest^ := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    inc(dest, swidth);
    inc(frac, fracstep);

    dest^ := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    inc(dest, swidth);
    inc(frac, fracstep);

    dest^ := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    inc(dest, swidth);
    inc(frac, fracstep);

    dest^ := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    inc(dest, swidth);
    inc(frac, fracstep);
  end;

  while frac <= fraclimit do
  begin
  // Re-map color indices from wall texture column
  //  using a lighting/special effects LUT.
    dest^ := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];

    inc(dest, swidth);
    inc(frac, fracstep);
  end;
end;

procedure R_DrawColumnBase32;
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
    spot := (LongWord(frac) shr FRACBITS) and 127;
    destl^ := dc_colormap32[dc_source[spot]];

    inc(destl, SCREENWIDTH);
    inc(frac, fracstep);
  end;
end;

procedure R_DrawColumnHi;
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
      R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);
      {$UNDEF INVERSECOLORMAPS}
      {$UNDEF MASKEDCOLUMN}
      {$UNDEF FOG}
      {$UNDEF SMALLSTEPOPTIMIZER}
      {$I R_DrawColumnHi.inc}
    end
    else
    begin
      {$DEFINE INVERSECOLORMAPS}
      {$UNDEF MASKEDCOLUMN}
      {$UNDEF FOG}
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
      R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);
      {$UNDEF INVERSECOLORMAPS}
      {$UNDEF MASKEDCOLUMN}
      {$UNDEF FOG}
      {$DEFINE SMALLSTEPOPTIMIZER}
      {$I R_DrawColumnHi.inc}
    end
    else
    begin
      {$DEFINE INVERSECOLORMAPS}
      {$UNDEF MASKEDCOLUMN}
      {$UNDEF FOG}
      {$DEFINE SMALLSTEPOPTIMIZER}
      {$I R_DrawColumnHi.inc}
    end;
  end;
end;

procedure R_DrawColumnUltra;
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
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);
    {$UNDEF INVERSECOLORMAPS}
    {$UNDEF MASKEDCOLUMN}
    {$UNDEF FOG}
    {$UNDEF SMALLSTEPOPTIMIZER}
    {$I R_DrawColumnUltra.inc}
  end
  else
  begin
    {$DEFINE INVERSECOLORMAPS}
    {$UNDEF MASKEDCOLUMN}
    {$UNDEF FOG}
    {$UNDEF SMALLSTEPOPTIMIZER}
    {$I R_DrawColumnUltra.inc}
  end;
end;

end.

