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
// DESCRIPTION:
//  Multithreading sprite rendering
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit r_sprite;

interface

uses
  d_delphi,
  r_trans8;

type
  Pspriterenderinfo_t = ^spriterenderinfo_t;

  spritefunc_t = procedure(parms: Pspriterenderinfo_t);

  spriterenderinfo_t = record
    dc_x: integer;
    dc_yh, dc_yl: integer;
    dc_iscale: integer;
    dc_texturemid: integer;
    dc_source: PByteArray;
    dc_alpha: integer;
    dc_fog: boolean;  // JVAL: Mars fog sectors
    num_batch_columns: integer;
    dc_colormap: PByteArray;
    dc_colormap32: PLongWordArray;
    proc: spritefunc_t;
  end;
  spriterenderinfo_tArray = array[0..$FFF] of spriterenderinfo_t;
  Pspriterenderinfo_tArray = ^spriterenderinfo_tArray;

//==============================================================================
//
// R_SpriteAddMTInfo
//
//==============================================================================
function R_SpriteAddMTInfo: Pspriterenderinfo_t;

//==============================================================================
//
// R_SpriteRenderMT
//
//==============================================================================
procedure R_SpriteRenderMT;

//==============================================================================
//
// R_DrawColumnLowestMT
//
//==============================================================================
procedure R_DrawColumnLowestMT(p: Pspriterenderinfo_t);

//==============================================================================
//
// R_DrawColumnLowMT
//
//==============================================================================
procedure R_DrawColumnLowMT(p: Pspriterenderinfo_t);

//==============================================================================
//
// R_DrawColumnMediumMT
//
//==============================================================================
procedure R_DrawColumnMediumMT(p: Pspriterenderinfo_t);

//==============================================================================
//
// R_DrawColumnAddLowestMT
//
//==============================================================================
procedure R_DrawColumnAddLowestMT(p: Pspriterenderinfo_t);

//==============================================================================
//
// R_DrawColumnAddLowMT
//
//==============================================================================
procedure R_DrawColumnAddLowMT(p: Pspriterenderinfo_t);

//==============================================================================
//
// R_DrawColumnAddMediumMT
//
//==============================================================================
procedure R_DrawColumnAddMediumMT(p: Pspriterenderinfo_t);

//==============================================================================
//
// R_DrawColumnAddHiMT
//
//==============================================================================
procedure R_DrawColumnAddHiMT(p: Pspriterenderinfo_t);

//==============================================================================
//
// R_DrawColumnAddHi_SmallStepMT
//
//==============================================================================
procedure R_DrawColumnAddHi_SmallStepMT(p: Pspriterenderinfo_t);

//==============================================================================
//
// R_DrawColumnSubtractLowestMT
//
//==============================================================================
procedure R_DrawColumnSubtractLowestMT(p: Pspriterenderinfo_t);

//==============================================================================
//
// R_DrawColumnSubtractLowMT
//
//==============================================================================
procedure R_DrawColumnSubtractLowMT(p: Pspriterenderinfo_t);

//==============================================================================
//
// R_DrawColumnSubtractMediumMT
//
//==============================================================================
procedure R_DrawColumnSubtractMediumMT(p: Pspriterenderinfo_t);

//==============================================================================
//
// R_DrawColumnSubtractHiMT
//
//==============================================================================
procedure R_DrawColumnSubtractHiMT(p: Pspriterenderinfo_t);

//==============================================================================
//
// R_DrawColumnAverageLowestMT
//
//==============================================================================
procedure R_DrawColumnAverageLowestMT(p: Pspriterenderinfo_t);

//==============================================================================
//
// R_DrawColumnAverageLowMT
//
//==============================================================================
procedure R_DrawColumnAverageLowMT(p: Pspriterenderinfo_t);

//==============================================================================
//
// R_DrawColumnAverageMediumMT
//
//==============================================================================
procedure R_DrawColumnAverageMediumMT(p: Pspriterenderinfo_t);

//==============================================================================
//
// R_DrawColumnAverageHiMT
//
//==============================================================================
procedure R_DrawColumnAverageHiMT(p: Pspriterenderinfo_t);

//
// Normal functions
//

//==============================================================================
//
// R_DrawMaskedColumnNormalMT
//
//==============================================================================
procedure R_DrawMaskedColumnNormalMT(p: Pspriterenderinfo_t);

//==============================================================================
// R_DrawColumnLow_BatchMT
//
////////////////////////////////////////////////////////////////////////////////
// Batch functions
// Low res
//
//==============================================================================
procedure R_DrawColumnLow_BatchMT(p: Pspriterenderinfo_t);

//==============================================================================
// R_DrawColumnMedium_BatchMT
//
// Medium res
//
//==============================================================================
procedure R_DrawColumnMedium_BatchMT(p: Pspriterenderinfo_t);

//==============================================================================
//
// R_DrawColumnAlphaMedium_BatchMT
//
//==============================================================================
procedure R_DrawColumnAlphaMedium_BatchMT(p: Pspriterenderinfo_t);

//==============================================================================
//
// R_DrawColumnAddMedium_BatchMT
//
//==============================================================================
procedure R_DrawColumnAddMedium_BatchMT(p: Pspriterenderinfo_t);

//==============================================================================
//
// R_DrawColumnSubtractMedium_BatchMT
//
//==============================================================================
procedure R_DrawColumnSubtractMedium_BatchMT(p: Pspriterenderinfo_t);

//==============================================================================
// R_DrawColumnHi_BatchMT
//
// Hi res
//
//==============================================================================
procedure R_DrawColumnHi_BatchMT(p: Pspriterenderinfo_t);

//==============================================================================
//
// R_DrawColumnAverageMedium_BatchMT
//
//==============================================================================
procedure R_DrawColumnAverageMedium_BatchMT(p: Pspriterenderinfo_t);

//==============================================================================
//
// R_DrawColumnAverageHi_BatchMT
//
//==============================================================================
procedure R_DrawColumnAverageHi_BatchMT(p: Pspriterenderinfo_t);

//==============================================================================
//
// R_DrawColumnAlphaHi_BatchMT
//
//==============================================================================
procedure R_DrawColumnAlphaHi_BatchMT(p: Pspriterenderinfo_t);

//==============================================================================
//
// R_DrawColumnAddHi_BatchMT
//
//==============================================================================
procedure R_DrawColumnAddHi_BatchMT(p: Pspriterenderinfo_t);

//==============================================================================
//
// R_DrawColumnSubtractHi_BatchMT
//
//==============================================================================
procedure R_DrawColumnSubtractHi_BatchMT(p: Pspriterenderinfo_t);

implementation

uses
  doomdef,
  i_system,
  mt_utils,
  m_fixed,
  r_draw,
  r_main,
  r_precalc,
  r_column,
  {$IFDEF DOOM_OR_STRIFE}
  r_colormaps,
  {$ENDIF}
  v_video,
  z_zone;

//==============================================================================
//
// R_DrawColumnLowestMT
//
//==============================================================================
procedure R_DrawColumnLowestMT(p: Pspriterenderinfo_t);
var
  count: integer;
  i: integer;
  dest: PByte;
  frac: fixed_t;
  fracstep: fixed_t;
  swidth: integer;
  buf: twobytes_t;
begin
  if odd(p.dc_x) then
    exit;

  count := (p.dc_yh - p.dc_yl) div 3;

  if count < 0 then
    exit;

  dest := @((ylookup[p.dc_yl]^)[columnofs[p.dc_x]]);

  frac := p.dc_texturemid + (p.dc_yl - centery) * p.dc_iscale;
  fracstep := 3 * p.dc_iscale;
  swidth := SCREENWIDTH;

  for i := 0 to count - 1 do
  begin
    buf.byte1 := p.dc_colormap[p.dc_source[(LongWord(frac) shr FRACBITS) and 127]];
    buf.byte2 := buf.byte1;

    PWord(dest)^ := Word(buf);
    inc(dest, swidth);

    PWord(dest)^ := Word(buf);
    inc(dest, swidth);

    PWord(dest)^ := Word(buf);
    inc(dest, swidth);

    inc(frac, fracstep);
  end;

  count := (p.dc_yh - p.dc_yl) mod 3;
  for i := 0 to count do
  begin
    buf.byte1 := p.dc_colormap[p.dc_source[(LongWord(frac) shr FRACBITS) and 127]];
    buf.byte2 := buf.byte1;
    PWord(dest)^ := Word(buf);
    inc(dest, swidth);

    inc(frac, p.dc_iscale);
  end;
end;

//==============================================================================
//
// R_DrawColumnLowMT
//
//==============================================================================
procedure R_DrawColumnLowMT(p: Pspriterenderinfo_t);
var
  count: integer;
  i: integer;
  dest: PByte;
  bdest: byte;
  frac: fixed_t;
  fracstep: fixed_t;
  swidth: integer;
begin
  count := (p.dc_yh - p.dc_yl) div 3;

  if count < 0 then
    exit;

  dest := @((ylookup[p.dc_yl]^)[columnofs[p.dc_x]]);

  frac := p.dc_texturemid + (p.dc_yl - centery) * p.dc_iscale;
  fracstep := 3 * p.dc_iscale;
  swidth := SCREENWIDTH;

  for i := 0 to count - 1 do
  begin
    bdest := p.dc_colormap[p.dc_source[(LongWord(frac) shr FRACBITS) and 127]];

    dest^ := bdest;
    inc(dest, swidth);

    dest^ := bdest;
    inc(dest, swidth);

    dest^ := bdest;
    inc(dest, swidth);

    inc(frac, fracstep);
  end;

  count := (p.dc_yh - p.dc_yl) mod 3;
  for i := 0 to count do
  begin
    dest^ := p.dc_colormap[p.dc_source[(LongWord(frac) shr FRACBITS) and 127]];
    inc(dest, swidth);

    inc(frac, p.dc_iscale);
  end;
end;

//==============================================================================
//
// R_DrawColumnMediumMT
//
//==============================================================================
procedure R_DrawColumnMediumMT(p: Pspriterenderinfo_t);
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
  count := p.dc_yh - p.dc_yl;

  // Zero length, column does not exceed a pixel.
  if count < 0 then
    exit;

  // Framebuffer destination address.
  // Use ylookup LUT to avoid multiply with ScreenWidth.
  // Use columnofs LUT for subwindows?
  dest := @((ylookup[p.dc_yl]^)[columnofs[p.dc_x]]);

  // Determine scaling,
  //  which is the only mapping to be done.
  fracstep := p.dc_iscale;
  frac := p.dc_texturemid + (p.dc_yl - centery) * fracstep;
  fraclimit := frac + count * fracstep;
  fraclimit2 := frac + (count - 16) * fracstep;
  swidth := SCREENWIDTH;
  dc_local := p.dc_source;

  // Inner loop that does the actual texture mapping,
  //  e.g. a DDA-lile scaling.
  // This is as fast as it gets.
  while frac <= fraclimit2 do
  begin
  // Re-map color indices from wall texture column
  //  using a lighting/special effects LUT.
    dest^ := p.dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    inc(dest, swidth);
    inc(frac, fracstep);

    dest^ := p.dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    inc(dest, swidth);
    inc(frac, fracstep);

    dest^ := p.dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    inc(dest, swidth);
    inc(frac, fracstep);

    dest^ := p.dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    inc(dest, swidth);
    inc(frac, fracstep);

    dest^ := p.dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    inc(dest, swidth);
    inc(frac, fracstep);

    dest^ := p.dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    inc(dest, swidth);
    inc(frac, fracstep);

    dest^ := p.dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    inc(dest, swidth);
    inc(frac, fracstep);

    dest^ := p.dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    inc(dest, swidth);
    inc(frac, fracstep);

    dest^ := p.dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    inc(dest, swidth);
    inc(frac, fracstep);

    dest^ := p.dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    inc(dest, swidth);
    inc(frac, fracstep);

    dest^ := p.dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    inc(dest, swidth);
    inc(frac, fracstep);

    dest^ := p.dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    inc(dest, swidth);
    inc(frac, fracstep);

    dest^ := p.dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    inc(dest, swidth);
    inc(frac, fracstep);

    dest^ := p.dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    inc(dest, swidth);
    inc(frac, fracstep);

    dest^ := p.dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    inc(dest, swidth);
    inc(frac, fracstep);

    dest^ := p.dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    inc(dest, swidth);
    inc(frac, fracstep);
  end;

  while frac <= fraclimit do
  begin
  // Re-map color indices from wall texture column
  //  using a lighting/special effects LUT.
    dest^ := p.dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];

    inc(dest, swidth);
    inc(frac, fracstep);
  end;
end;

//
// RenderStyle Add
//

//==============================================================================
//
// R_DrawColumnAddLowestMT
//
//==============================================================================
procedure R_DrawColumnAddLowestMT(p: Pspriterenderinfo_t);
var
  count: integer;
  i: integer;
  dest: PByte;
  frac: fixed_t;
  fracstep: fixed_t;
  swidth: integer;
  buf: twobytes_t;
begin
  if odd(p.dc_x) then
    exit;

  count := (p.dc_yh - p.dc_yl) div 3;

  if count < 0 then
    exit;

  dest := @((ylookup[p.dc_yl]^)[columnofs[p.dc_x]]);

  frac := dc_texturemid + (p.dc_yl - centery) * p.dc_iscale;
  fracstep := 3 * p.dc_iscale;
  swidth := SCREENWIDTH;

  for i := 0 to count - 1 do
  begin
    buf.byte1 := curadd8table[dest^ + (p.dc_colormap[p.dc_source[(LongWord(frac) shr FRACBITS) and 127]] shl 8)];
    buf.byte2 := buf.byte1;

    PWord(dest)^ := Word(buf);
    inc(dest, swidth);

    PWord(dest)^ := Word(buf);
    inc(dest, swidth);

    PWord(dest)^ := Word(buf);
    inc(dest, swidth);

    inc(frac, fracstep);
  end;

  count := (p.dc_yh - p.dc_yl) mod 3;
  for i := 0 to count do
  begin
    buf.byte1 := curadd8table[dest^ + (p.dc_colormap[p.dc_source[(LongWord(frac) shr FRACBITS) and 127]] shl 8)];
    buf.byte2 := buf.byte1;
    PWord(dest)^ := Word(buf);
    inc(dest, swidth);

    inc(frac, p.dc_iscale);
  end;
end;

//==============================================================================
//
// R_DrawColumnAddLowMT
//
//==============================================================================
procedure R_DrawColumnAddLowMT(p: Pspriterenderinfo_t);
var
  count: integer;
  i: integer;
  dest: PByte;
  bdest: byte;
  frac: fixed_t;
  fracstep: fixed_t;
  swidth: integer;
begin
  count := (p.dc_yh - p.dc_yl) div 3;

  if count < 0 then
    exit;

  dest := @((ylookup[p.dc_yl]^)[columnofs[p.dc_x]]);

  frac := dc_texturemid + (p.dc_yl - centery) * p.dc_iscale;
  fracstep := 3 * p.dc_iscale;
  swidth := SCREENWIDTH;

  for i := 0 to count - 1 do
  begin
    bdest := curadd8table[dest^ + (p.dc_colormap[p.dc_source[(LongWord(frac) shr FRACBITS) and 127]] shl 8)];

    dest^ := bdest;
    inc(dest, swidth);

    dest^ := bdest;
    inc(dest, swidth);

    dest^ := bdest;
    inc(dest, swidth);

    inc(frac, fracstep);
  end;

  count := (p.dc_yh - p.dc_yl) mod 3;
  for i := 0 to count do
  begin
    dest^ := curadd8table[dest^ + (p.dc_colormap[p.dc_source[(LongWord(frac) shr FRACBITS) and 127]] shl 8)];
    inc(dest, swidth);

    inc(frac, p.dc_iscale);
  end;
end;

//==============================================================================
//
// R_DrawColumnAddMediumMT
//
//==============================================================================
procedure R_DrawColumnAddMediumMT(p: Pspriterenderinfo_t);
var
  count: integer;
  dest: PByte;
  frac: fixed_t;
  fracstep: fixed_t;
  fraclimit: fixed_t;
  swidth: integer;
begin
  count := p.dc_yh - p.dc_yl;

  // Zero length, column does not exceed a pixel.
  if count < 0 then
    exit;

  // Framebuffer destination address.
  // Use ylookup LUT to avoid multiply with ScreenWidth.
  // Use columnofs LUT for subwindows?
  dest := @((ylookup[p.dc_yl]^)[columnofs[p.dc_x]]);

  // Determine scaling,
  //  which is the only mapping to be done.
  fracstep := p.dc_iscale;
  frac := dc_texturemid + (p.dc_yl - centery) * fracstep;
  fraclimit := frac + count * fracstep;
  swidth := SCREENWIDTH;

  // Inner loop that does the actual texture mapping,
  //  e.g. a DDA-lile scaling.
  // This is as fast as it gets.
  while frac < fraclimit do
  begin
  // Re-map color indices from wall texture column
  //  using a lighting/special effects LUT.
    dest^ := curadd8table[dest^ + (p.dc_colormap[p.dc_source[(LongWord(frac) shr FRACBITS) and 127]] shl 8)];
    inc(dest, swidth);
    inc(frac, fracstep);

    dest^ := curadd8table[dest^ + (p.dc_colormap[p.dc_source[(LongWord(frac) shr FRACBITS) and 127]] shl 8)];
    inc(dest, swidth);
    inc(frac, fracstep);
  end;
  while frac <= fraclimit do
  begin
  // Re-map color indices from wall texture column
  //  using a lighting/special effects LUT.
    dest^ := curadd8table[dest^ + (p.dc_colormap[p.dc_source[(LongWord(frac) shr FRACBITS) and 127]] shl 8)];
    inc(dest, swidth);
    inc(frac, fracstep);
  end;
end;

//==============================================================================
//
// R_DrawColumnAddHiMT
//
//==============================================================================
procedure R_DrawColumnAddHiMT(p: Pspriterenderinfo_t);
var
  count: integer;
  destl: PLongWord;
  frac: fixed_t;
  fracstep: fixed_t;
  fraclimit: fixed_t;
  swidth: integer;
  addfactor: fixed_t;

// For inline color averaging
  r1, g1, b1: byte;
  r2, g2, b2: byte;
  c1, c2, r, g, b: LongWord;
begin
  count := p.dc_yh - p.dc_yl;

  if count < 0 then
    exit;

  destl := @((ylookupl[p.dc_yl]^)[columnofs[p.dc_x]]);

  fracstep := p.dc_iscale;
  frac := dc_texturemid + (p.dc_yl - centery) * fracstep;

  // Inner loop that does the actual texture mapping,
  //  e.g. a DDA-lile scaling.
  // This is as fast as it gets.
  swidth := SCREENWIDTH32PITCH;

  addfactor := p.dc_alpha;

  fraclimit := frac + fracstep * count;

  while frac < fraclimit do
  begin
    c1 := destl^;
    c2 := p.dc_colormap32[p.dc_source[(LongWord(frac) shr FRACBITS) and 127]];

    // Color averaging
    r1 := c1;
    g1 := c1 shr 8;
    b1 := c1 shr 16;
    r2 := c2;
    g2 := c2 shr 8;
    b2 := c2 shr 16;
    if addfactor < FRACUNIT then
    begin
      r2 := (r2 * addfactor) shr FRACBITS;
      g2 := (g2 * addfactor) shr FRACBITS;
      b2 := (b2 * addfactor) shr FRACBITS;
    end;

    r := r2 + r1;
    if r > 255 then
      r := 255;
    g := g2 + g1;
    if g > 255 then
      g := 255;
    b := b2 + b1;
    if b > 255 then
      b := 255;

    destl^ := r + g shl 8 + b shl 16;

    destl := PLongWord(integer(destl) + swidth);
    inc(frac, fracstep);
  end;
end;

//==============================================================================
//
// R_DrawColumnAddHi_SmallStepMT
//
//==============================================================================
procedure R_DrawColumnAddHi_SmallStepMT(p: Pspriterenderinfo_t);
var
  count: integer;
  destl: PLongWord;
  frac: fixed_t;
  fracstep: fixed_t;
  fraclimit: fixed_t;
  swidth: integer;
  addfactor: fixed_t;
// For inline color averaging
  r1, g1, b1: byte;
  r2, g2, b2: byte;
  c1, c2, r, g, b: LongWord;
  pb: PByte;
  pl, pl2: PLongWord;
  addbuf32: array[0..255] of LongWord;
begin
  count := p.dc_yh - p.dc_yl;

  if count < 0 then
    exit;

  destl := @((ylookupl[p.dc_yl]^)[columnofs[p.dc_x]]);

  fracstep := p.dc_iscale;
  frac := dc_texturemid + (p.dc_yl - centery) * fracstep;

  // Inner loop that does the actual texture mapping,
  //  e.g. a DDA-lile scaling.
  // This is as fast as it gets.
  swidth := SCREENWIDTH32PITCH;

  addfactor := p.dc_alpha;

  fraclimit := frac + fracstep * count;

  if (frac > 0) and (fraclimit < 256 * FRACUNIT) then
  begin
    pl := @addbuf32[LongWord(frac) shr FRACBITS];
    pl2 := @addbuf32[LongWord(fraclimit) shr FRACBITS + 1];
    pb := @p.dc_source[LongWord(frac) shr FRACBITS];
    while pl <> pl2 do
    begin
      pl^ := p.dc_colormap32[pb^];
      if addfactor < FRACUNIT then
      begin
        PByteArray(pl)[0] := (PByteArray(pl)[0] * addfactor) shr FRACBITS;
        PByteArray(pl)[1] := (PByteArray(pl)[1] * addfactor) shr FRACBITS;
        PByteArray(pl)[2] := (PByteArray(pl)[2] * addfactor) shr FRACBITS;
      end;
      Inc(pl);
      Inc(pb);
    end;
    while frac < fraclimit do
    begin
      pl := @addbuf32[LongWord(frac) shr FRACBITS];
      PByteArray(destl)[0] := add32_c[PByteArray(pl)[0] + PByteArray(destl)[0]];
      PByteArray(destl)[1] := add32_c[PByteArray(pl)[1] + PByteArray(destl)[1]];
      PByteArray(destl)[2] := add32_c[PByteArray(pl)[2] + PByteArray(destl)[2]];

      destl := PLongWord(integer(destl) + swidth);
      inc(frac, fracstep);
    end;
  end
  else while frac < fraclimit do
  begin
    c1 := destl^;
    c2 := p.dc_colormap32[p.dc_source[(LongWord(frac) shr FRACBITS) and 127]];

    // Color averaging
    r1 := c1;
    g1 := c1 shr 8;
    b1 := c1 shr 16;
    r2 := c2;
    g2 := c2 shr 8;
    b2 := c2 shr 16;
    if addfactor < FRACUNIT then
    begin
      r2 := (r2 * addfactor) shr FRACBITS;
      g2 := (g2 * addfactor) shr FRACBITS;
      b2 := (b2 * addfactor) shr FRACBITS;
    end;

    r := r2 + r1;
    if r > 255 then
      r := 255;
    g := g2 + g1;
    if g > 255 then
      g := 255;
    b := b2 + b1;
    if b > 255 then
      b := 255;

    destl^ := r + g shl 8 + b shl 16;

    destl := PLongWord(integer(destl) + swidth);
    inc(frac, fracstep);
  end;
end;

//
// Renderstyle subtract
//

//==============================================================================
//
// R_DrawColumnSubtractLowestMT
//
//==============================================================================
procedure R_DrawColumnSubtractLowestMT(p: Pspriterenderinfo_t);
var
  count: integer;
  i: integer;
  dest: PByte;
  frac: fixed_t;
  fracstep: fixed_t;
  swidth: integer;
  buf: twobytes_t;
begin
  if odd(p.dc_x) then
    exit;

  count := (p.dc_yh - p.dc_yl) div 3;

  if count < 0 then
    exit;

  dest := @((ylookup[p.dc_yl]^)[columnofs[p.dc_x]]);

  frac := p.dc_texturemid + (p.dc_yl - centery) * p.dc_iscale;
  fracstep := 3 * p.dc_iscale;
  swidth := SCREENWIDTH;

  for i := 0 to count - 1 do
  begin
    buf.byte1 := cursubtract8table[dest^ + (p.dc_colormap[p.dc_source[(LongWord(frac) shr FRACBITS) and 127]] shl 8)];
    buf.byte2 := buf.byte1;

    PWord(dest)^ := Word(buf);
    inc(dest, swidth);

    PWord(dest)^ := Word(buf);
    inc(dest, swidth);

    PWord(dest)^ := Word(buf);
    inc(dest, swidth);

    inc(frac, fracstep);
  end;

  count := (p.dc_yh - p.dc_yl) mod 3;
  for i := 0 to count do
  begin
    buf.byte1 := cursubtract8table[dest^ + (p.dc_colormap[p.dc_source[(LongWord(frac) shr FRACBITS) and 127]] shl 8)];
    buf.byte2 := buf.byte1;
    PWord(dest)^ := Word(buf);
    inc(dest, swidth);

    inc(frac, p.dc_iscale);
  end;
end;

//==============================================================================
//
// R_DrawColumnSubtractLowMT
//
//==============================================================================
procedure R_DrawColumnSubtractLowMT(p: Pspriterenderinfo_t);
var
  count: integer;
  i: integer;
  dest: PByte;
  bdest: byte;
  frac: fixed_t;
  fracstep: fixed_t;
  swidth: integer;
begin
  count := (p.dc_yh - p.dc_yl) div 3;

  if count < 0 then
    exit;

  dest := @((ylookup[p.dc_yl]^)[columnofs[p.dc_x]]);

  frac := p.dc_texturemid + (p.dc_yl - centery) * p.dc_iscale;
  fracstep := 3 * p.dc_iscale;
  swidth := SCREENWIDTH;

  for i := 0 to count - 1 do
  begin
    bdest := cursubtract8table[dest^ + (p.dc_colormap[p.dc_source[(LongWord(frac) shr FRACBITS) and 127]] shl 8)];

    dest^ := bdest;
    inc(dest, swidth);

    dest^ := bdest;
    inc(dest, swidth);

    dest^ := bdest;
    inc(dest, swidth);

    inc(frac, fracstep);
  end;

  count := (p.dc_yh - p.dc_yl) mod 3;
  for i := 0 to count do
  begin
    dest^ := cursubtract8table[dest^ + (p.dc_colormap[p.dc_source[(LongWord(frac) shr FRACBITS) and 127]] shl 8)];
    inc(dest, swidth);

    inc(frac, p.dc_iscale);
  end;
end;

//==============================================================================
//
// R_DrawColumnSubtractMediumMT
//
//==============================================================================
procedure R_DrawColumnSubtractMediumMT(p: Pspriterenderinfo_t);
var
  count: integer;
  dest: PByte;
  frac: fixed_t;
  fracstep: fixed_t;
  fraclimit: fixed_t;
  swidth: integer;
begin
  count := p.dc_yh - p.dc_yl;

  // Zero length, column does not exceed a pixel.
  if count < 0 then
    exit;

  // Framebuffer destination address.
  // Use ylookup LUT to avoid multiply with ScreenWidth.
  // Use columnofs LUT for subwindows?
  dest := @((ylookup[p.dc_yl]^)[columnofs[p.dc_x]]);

  // Determine scaling,
  //  which is the only mapping to be done.
  fracstep := p.dc_iscale;
  frac := p.dc_texturemid + (p.dc_yl - centery) * fracstep;
  fraclimit := frac + count * fracstep;
  swidth := SCREENWIDTH;

  // Inner loop that does the actual texture mapping,
  //  e.g. a DDA-lile scaling.
  // This is as fast as it gets.
  while frac < fraclimit do
  begin
  // Re-map color indices from wall texture column
  //  using a lighting/special effects LUT.
    dest^ := cursubtract8table[dest^ + (p.dc_colormap[p.dc_source[(LongWord(frac) shr FRACBITS) and 127]] shl 8)];
    inc(dest, swidth);
    inc(frac, fracstep);

    dest^ := cursubtract8table[dest^ + (p.dc_colormap[p.dc_source[(LongWord(frac) shr FRACBITS) and 127]] shl 8)];
    inc(dest, swidth);
    inc(frac, fracstep);
  end;
  while frac <= fraclimit do
  begin
  // Re-map color indices from wall texture column
  //  using a lighting/special effects LUT.
    dest^ := cursubtract8table[dest^ + (p.dc_colormap[p.dc_source[(LongWord(frac) shr FRACBITS) and 127]] shl 8)];
    inc(dest, swidth);
    inc(frac, fracstep);
  end;
end;

//==============================================================================
//
// R_DrawColumnSubtractHiMT
//
//==============================================================================
procedure R_DrawColumnSubtractHiMT(p: Pspriterenderinfo_t);
var
  count: integer;
  destl: PLongWord;
  frac: fixed_t;
  fracstep: fixed_t;
  fraclimit: fixed_t;
  swidth: integer;
  subfactor: fixed_t;

// For inline color averaging
  r1, g1, b1: byte;
  r2, g2, b2: byte;
  c1, c2, r, g, b: LongWord;
begin
  count := p.dc_yh - p.dc_yl;

  if count < 0 then
    exit;

  destl := @((ylookupl[p.dc_yl]^)[columnofs[p.dc_x]]);

  fracstep := p.dc_iscale;
  frac := p.dc_texturemid + (p.dc_yl - centery) * fracstep;

  // Inner loop that does the actual texture mapping,
  //  e.g. a DDA-lile scaling.
  // This is as fast as it gets.
  swidth := SCREENWIDTH32PITCH;

  subfactor := p.dc_alpha;

  fraclimit := frac + fracstep * count;
  while frac < fraclimit do
  begin
    c1 := destl^;
    c2 := p.dc_colormap32[p.dc_source[(LongWord(frac) shr FRACBITS) and 127]];

    // Color averaging
    r1 := c1;
    g1 := c1 shr 8;
    b1 := c1 shr 16;
    r2 := c2;
    g2 := c2 shr 8;
    b2 := c2 shr 16;
    if subfactor < FRACUNIT then
    begin
      r2 := (r2 * subfactor) shr FRACBITS;
      g2 := (g2 * subfactor) shr FRACBITS;
      b2 := (b2 * subfactor) shr FRACBITS;
    end;

    if r2 > r1 then
      r := 0
    else
      r := r1 - r2;
    if g2 > g1 then
      g := 0
    else
      g := g1 - g2;
    if b2 > b1 then
      b := 0
    else
      b := b1 - b2;

    destl^ := r + g shl 8 + b shl 16;

    destl := PLongWord(integer(destl) + swidth);
    inc(frac, fracstep);
  end;
end;

//
// Average
//

//==============================================================================
//
// R_DrawColumnAverageLowestMT
//
//==============================================================================
procedure R_DrawColumnAverageLowestMT(p: Pspriterenderinfo_t);
var
  count: integer;
  i: integer;
  dest: PByte;
  frac: fixed_t;
  fracstep: fixed_t;
  swidth: integer;
  buf: twobytes_t;
begin
  if odd(p.dc_x) then
    exit;

  count := (p.dc_yh - p.dc_yl) div 3;

  if count < 0 then
    exit;

  dest := @((ylookup[p.dc_yl]^)[columnofs[p.dc_x]]);

  frac := p.dc_texturemid + (p.dc_yl - centery) * p.dc_iscale;
  fracstep := 3 * p.dc_iscale;
  swidth := SCREENWIDTH;

  for i := 0 to count - 1 do
  begin
    buf.byte1 := averagetrans8table[(dest^ shl 8) + p.dc_colormap[p.dc_source[(LongWord(frac) shr FRACBITS) and 127]]];
    buf.byte2 := buf.byte1;

    PWord(dest)^ := Word(buf);
    inc(dest, swidth);

    PWord(dest)^ := Word(buf);
    inc(dest, swidth);

    PWord(dest)^ := Word(buf);
    inc(dest, swidth);

    inc(frac, fracstep);
  end;

  count := (p.dc_yh - p.dc_yl) mod 3;
  for i := 0 to count do
  begin
    buf.byte1 := averagetrans8table[(dest^ shl 8) + p.dc_colormap[p.dc_source[(LongWord(frac) shr FRACBITS) and 127]]];
    buf.byte2 := buf.byte1;
    PWord(dest)^ := Word(buf);
    inc(dest, swidth);

    inc(frac, p.dc_iscale);
  end;
end;

//==============================================================================
//
// R_DrawColumnAverageLowMT
//
//==============================================================================
procedure R_DrawColumnAverageLowMT(p: Pspriterenderinfo_t);
var
  count: integer;
  i: integer;
  dest: PByte;
  bdest: byte;
  frac: fixed_t;
  fracstep: fixed_t;
  swidth: integer;
begin
  count := (p.dc_yh - p.dc_yl) div 3;

  if count < 0 then
    exit;

  dest := @((ylookup[p.dc_yl]^)[columnofs[p.dc_x]]);

  frac := p.dc_texturemid + (p.dc_yl - centery) * p.dc_iscale;
  fracstep := 3 * p.dc_iscale;
  swidth := SCREENWIDTH;

  for i := 0 to count - 1 do
  begin
    bdest := averagetrans8table[(dest^ shl 8) + p.dc_colormap[p.dc_source[(LongWord(frac) shr FRACBITS) and 127]]];

    dest^ := bdest;
    inc(dest, swidth);

    dest^ := bdest;
    inc(dest, swidth);

    dest^ := bdest;
    inc(dest, swidth);

    inc(frac, fracstep);
  end;

  count := (p.dc_yh - p.dc_yl) mod 3;
  for i := 0 to count do
  begin
    dest^ := averagetrans8table[(dest^ shl 8) + p.dc_colormap[p.dc_source[(LongWord(frac) shr FRACBITS) and 127]]];
    inc(dest, swidth);

    inc(frac, p.dc_iscale);
  end;
end;

//==============================================================================
//
// R_DrawColumnAverageMediumMT
//
//==============================================================================
procedure R_DrawColumnAverageMediumMT(p: Pspriterenderinfo_t);
var
  count: integer;
  dest: PByte;
  frac: fixed_t;
  fracstep: fixed_t;
  fraclimit: fixed_t;
  swidth: integer;
begin
  count := p.dc_yh - p.dc_yl;

  // Zero length, column does not exceed a pixel.
  if count < 0 then
    exit;

  // Framebuffer destination address.
  // Use ylookup LUT to avoid multiply with ScreenWidth.
  // Use columnofs LUT for subwindows?
  dest := @((ylookup[p.dc_yl]^)[columnofs[p.dc_x]]);

  // Determine scaling,
  //  which is the only mapping to be done.
  fracstep := p.dc_iscale;
  frac := p.dc_texturemid + (p.dc_yl - centery) * fracstep;
  fraclimit := frac + count * fracstep;
  swidth := SCREENWIDTH;

  // Inner loop that does the actual texture mapping,
  //  e.g. a DDA-lile scaling.
  // This is as fast as it gets.
  while frac <= fraclimit do
  begin
  // Re-map color indices from wall texture column
  //  using a lighting/special effects LUT.
    dest^ := averagetrans8table[(dest^ shl 8) + p.dc_colormap[p.dc_source[(LongWord(frac) shr FRACBITS) and 127]]];

    inc(dest, swidth);
    inc(frac, fracstep);
  end;
end;

//==============================================================================
//
// R_DrawColumnAverageHiMT
//
//==============================================================================
procedure R_DrawColumnAverageHiMT(p: Pspriterenderinfo_t);
var
  count: integer;
  destb: PByte;
  frac: fixed_t;
  fracstep: fixed_t;
  swidth: integer;
  src: PByteArray;
begin
  count := p.dc_yh - p.dc_yl;

  if count < 0 then
    exit;

  destb := @((ylookupl[p.dc_yl]^)[columnofs[p.dc_x]]);

  fracstep := p.dc_iscale;
  frac := p.dc_texturemid + (p.dc_yl - centery) * fracstep;

  // Inner loop that does the actual texture mapping,
  //  e.g. a DDA-lile scaling.
  // This is as fast as it gets.
  swidth := SCREENWIDTH32PITCH;

  while count >= 0 do
  begin
    src := @p.dc_colormap32[p.dc_source[(LongWord(frac) shr FRACBITS) and 127]];

    PByteArray(destb)[0] := average_byte[src[0] + PByteArray(destb)[0]];
    PByteArray(destb)[1] := average_byte[src[1] + PByteArray(destb)[1]];
    PByteArray(destb)[2] := average_byte[src[2] + PByteArray(destb)[2]];

    destb := PByte(integer(destb) + swidth);
    inc(frac, fracstep);
    dec(count);
  end;
end;

//
// Normal functions
//

//==============================================================================
//
// R_DrawMaskedColumnNormalMT
//
//==============================================================================
procedure R_DrawMaskedColumnNormalMT(p: Pspriterenderinfo_t);
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
  dc_local: PByteArray;
begin
  count := p.dc_yh - p.dc_yl;

  if count < 0 then
    exit;

  destl := @((ylookupl[p.dc_yl]^)[columnofs[p.dc_x]]);

  fracstep := p.dc_iscale;
  frac := p.dc_texturemid + (p.dc_yl - centery) * fracstep;

  swidth := SCREENWIDTH32PITCH;
  lfactor := dc_lightlevel;
  dc_local := p.dc_source;
  {$IFDEF DOOM_OR_STRIFE}
  if customcolormap = nil then
  begin
  {$ENDIF}
    if lfactor >= 0 then
    begin
      R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, p.dc_fog);
      {$UNDEF INVERSECOLORMAPS}
      {$UNDEF CUSTOMCOLORMAP}
      {$I R_DrawMaskedColumnNormalMT.inc}
    end
    else
    begin
      {$DEFINE INVERSECOLORMAPS}
      {$UNDEF CUSTOMCOLORMAP}
      {$I R_DrawMaskedColumnNormalMT.inc}
    end;
  {$IFDEF DOOM_OR_STRIFE}
  end
  else
  begin
    if lfactor >= 0 then
    begin
      R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, p.dc_fog);  // JVAL: Mars fog sectors
      {$UNDEF INVERSECOLORMAPS}
      {$DEFINE CUSTOMCOLORMAP}
      {$I R_DrawMaskedColumnNormalMT.inc}
    end
    else
    begin
      {$DEFINE INVERSECOLORMAPS}
      {$DEFINE CUSTOMCOLORMAP}
      {$I R_DrawMaskedColumnNormalMT.inc}
    end;
  end;
  {$ENDIF}
end;

//==============================================================================
// R_DrawColumnLow_BatchMT
//
////////////////////////////////////////////////////////////////////////////////
// Batch functions
//
//==============================================================================
procedure R_DrawColumnLow_BatchMT(p: Pspriterenderinfo_t);
var
  count: integer;
  i: integer;
  dest: PByte;
  bdest: byte;
  frac: fixed_t;
  fracstep: fixed_t;
  swidth: integer;
  cnt: integer;
begin
  count := (p.dc_yh - p.dc_yl) div 3;

  if count < 0 then
    exit;

  dest := @((ylookup[p.dc_yl]^)[columnofs[p.dc_x]]);

  frac := p.dc_texturemid + (p.dc_yl - centery) * p.dc_iscale;
  fracstep := 3 * p.dc_iscale;
  swidth := SCREENWIDTH - p.num_batch_columns;

  for i := 0 to count - 1 do
  begin
    bdest := dc_colormap[p.dc_source[(LongWord(frac) shr FRACBITS) and 127]];

    cnt := p.num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      inc(dest);
      dec(cnt);
    end;
    inc(dest, swidth);

    cnt := p.num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      inc(dest);
      dec(cnt);
    end;
    inc(dest, swidth);

    cnt := p.num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      inc(dest);
      dec(cnt);
    end;
    inc(dest, swidth);

    inc(frac, fracstep);
  end;

  count := (p.dc_yh - p.dc_yl) mod 3;
  for i := 0 to count do
  begin
    bdest := dc_colormap[p.dc_source[(LongWord(frac) shr FRACBITS) and 127]];

    cnt := p.num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      inc(dest);
      dec(cnt);
    end;
    inc(dest, swidth);

    inc(frac, p.dc_iscale);
  end;
end;

//==============================================================================
//
// R_DrawColumnMedium_BatchMT
//
//==============================================================================
procedure R_DrawColumnMedium_BatchMT(p: Pspriterenderinfo_t);
var
  count: integer;
  dest: PByte;
  frac: fixed_t;
  fracstep: fixed_t;
  fraclimit: fixed_t;
{$IFNDEF NO_INLINE_LOOPS}
  fraclimit2: fixed_t;
{$ENDIF}
  swidth: integer;
  dc_local: PByteArray;
  bdest: byte;
  ldest: LongWord;
  rest_batch_columns: integer;
  num_iters: integer;
  cnt: integer;
begin
  count := p.dc_yh - p.dc_yl;

  // Zero length, column does not exceed a pixel.
  if count < 0 then
    exit;

  // Framebuffer destination address.
  // Use ylookup LUT to avoid multiply with ScreenWidth.
  // Use columnofs LUT for subwindows?
  dest := @((ylookup[p.dc_yl]^)[columnofs[p.dc_x]]);

  // Determine scaling,
  //  which is the only mapping to be done.
  fracstep := p.dc_iscale;
  frac := p.dc_texturemid + (p.dc_yl - centery) * fracstep;
  fraclimit := frac + count * fracstep;
{$IFNDEF NO_INLINE_LOOPS}
  fraclimit2 := frac + (count - 16) * fracstep;
{$ENDIF}  
  swidth := SCREENWIDTH - p.num_batch_columns;
  dc_local := p.dc_source;

  if p.num_batch_columns >= 4 then
  begin
    rest_batch_columns := p.num_batch_columns mod 4;
    num_iters := p.num_batch_columns div 4;

    if rest_batch_columns = 0 then
    begin
{$IFNDEF NO_INLINE_LOOPS}
      while frac <= fraclimit2 do
      begin
      // Re-map color indices from wall texture column
      //  using a lighting/special effects LUT.
        bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
        ldest := precal8_tolong[bdest];
        cnt := num_iters;
        while cnt > 0 do
        begin
          PLongWord(dest)^ := ldest;
          inc(dest, 4);
          dec(cnt);
        end;
        inc(dest, swidth);
        inc(frac, fracstep);

        bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
        ldest := precal8_tolong[bdest];
        cnt := num_iters;
        while cnt > 0 do
        begin
          PLongWord(dest)^ := ldest;
          inc(dest, 4);
          dec(cnt);
        end;
        inc(dest, swidth);
        inc(frac, fracstep);

        bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
        ldest := precal8_tolong[bdest];
        cnt := num_iters;
        while cnt > 0 do
        begin
          PLongWord(dest)^ := ldest;
          inc(dest, 4);
          dec(cnt);
        end;
        inc(dest, swidth);
        inc(frac, fracstep);

        bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
        ldest := precal8_tolong[bdest];
        cnt := num_iters;
        while cnt > 0 do
        begin
          PLongWord(dest)^ := ldest;
          inc(dest, 4);
          dec(cnt);
        end;
        inc(dest, swidth);
        inc(frac, fracstep);

        bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
        ldest := precal8_tolong[bdest];
        cnt := num_iters;
        while cnt > 0 do
        begin
          PLongWord(dest)^ := ldest;
          inc(dest, 4);
          dec(cnt);
        end;
        inc(dest, swidth);
        inc(frac, fracstep);

        bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
        ldest := precal8_tolong[bdest];
        cnt := num_iters;
        while cnt > 0 do
        begin
          PLongWord(dest)^ := ldest;
          inc(dest, 4);
          dec(cnt);
        end;
        inc(dest, swidth);
        inc(frac, fracstep);

        bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
        ldest := precal8_tolong[bdest];
        cnt := num_iters;
        while cnt > 0 do
        begin
          PLongWord(dest)^ := ldest;
          inc(dest, 4);
          dec(cnt);
        end;
        inc(dest, swidth);
        inc(frac, fracstep);

        bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
        ldest := precal8_tolong[bdest];
        cnt := num_iters;
        while cnt > 0 do
        begin
          PLongWord(dest)^ := ldest;
          inc(dest, 4);
          dec(cnt);
        end;
        inc(dest, swidth);
        inc(frac, fracstep);

        bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
        ldest := precal8_tolong[bdest];
        cnt := num_iters;
        while cnt > 0 do
        begin
          PLongWord(dest)^ := ldest;
          inc(dest, 4);
          dec(cnt);
        end;
        inc(dest, swidth);
        inc(frac, fracstep);

        bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
        ldest := precal8_tolong[bdest];
        cnt := num_iters;
        while cnt > 0 do
        begin
          PLongWord(dest)^ := ldest;
          inc(dest, 4);
          dec(cnt);
        end;
        inc(dest, swidth);
        inc(frac, fracstep);

        bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
        ldest := precal8_tolong[bdest];
        cnt := num_iters;
        while cnt > 0 do
        begin
          PLongWord(dest)^ := ldest;
          inc(dest, 4);
          dec(cnt);
        end;
        inc(dest, swidth);
        inc(frac, fracstep);

        bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
        ldest := precal8_tolong[bdest];
        cnt := num_iters;
        while cnt > 0 do
        begin
          PLongWord(dest)^ := ldest;
          inc(dest, 4);
          dec(cnt);
        end;
        inc(dest, swidth);
        inc(frac, fracstep);

        bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
        ldest := precal8_tolong[bdest];
        cnt := num_iters;
        while cnt > 0 do
        begin
          PLongWord(dest)^ := ldest;
          inc(dest, 4);
          dec(cnt);
        end;
        inc(dest, swidth);
        inc(frac, fracstep);

        bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
        ldest := precal8_tolong[bdest];
        cnt := num_iters;
        while cnt > 0 do
        begin
          PLongWord(dest)^ := ldest;
          inc(dest, 4);
          dec(cnt);
        end;
        inc(dest, swidth);
        inc(frac, fracstep);

        bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
        ldest := precal8_tolong[bdest];
        cnt := num_iters;
        while cnt > 0 do
        begin
          PLongWord(dest)^ := ldest;
          inc(dest, 4);
          dec(cnt);
        end;
        inc(dest, swidth);
        inc(frac, fracstep);

        bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
        ldest := precal8_tolong[bdest];
        cnt := num_iters;
        while cnt > 0 do
        begin
          PLongWord(dest)^ := ldest;
          inc(dest, 4);
          dec(cnt);
        end;
        inc(dest, swidth);
        inc(frac, fracstep);
      end;
{$ENDIF}
      while frac <= fraclimit do
      begin
      // Re-map color indices from wall texture column
      //  using a lighting/special effects LUT.
        bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
        ldest := precal8_tolong[bdest];
        cnt := num_iters;
        while cnt > 0 do
        begin
          PLongWord(dest)^ := ldest;
          inc(dest, 4);
          dec(cnt);
        end;
        inc(dest, swidth);
        inc(frac, fracstep);
      end;
      exit;
    end;

{$IFNDEF NO_INLINE_LOOPS}
    while frac <= fraclimit2 do
    begin
    // Re-map color indices from wall texture column
    //  using a lighting/special effects LUT.
      bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
      ldest := precal8_tolong[bdest];
      cnt := num_iters;
      while cnt > 0 do
      begin
        PLongWord(dest)^ := ldest;
        inc(dest, 4);
        dec(cnt);
      end;
      cnt := rest_batch_columns;
      while cnt > 0 do
      begin
        dest^ := bdest;
        inc(dest);
        dec(cnt);
      end;
      inc(dest, swidth);
      inc(frac, fracstep);

      bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
      ldest := precal8_tolong[bdest];
      cnt := num_iters;
      while cnt > 0 do
      begin
        PLongWord(dest)^ := ldest;
        inc(dest, 4);
        dec(cnt);
      end;
      cnt := rest_batch_columns;
      while cnt > 0 do
      begin
        dest^ := bdest;
        inc(dest);
        dec(cnt);
      end;
      inc(dest, swidth);
      inc(frac, fracstep);

      bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
      ldest := precal8_tolong[bdest];
      cnt := num_iters;
      while cnt > 0 do
      begin
        PLongWord(dest)^ := ldest;
        inc(dest, 4);
        dec(cnt);
      end;
      cnt := rest_batch_columns;
      while cnt > 0 do
      begin
        dest^ := bdest;
        inc(dest);
        dec(cnt);
      end;
      inc(dest, swidth);
      inc(frac, fracstep);

      bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
      ldest := precal8_tolong[bdest];
      cnt := num_iters;
      while cnt > 0 do
      begin
        PLongWord(dest)^ := ldest;
        inc(dest, 4);
        dec(cnt);
      end;
      cnt := rest_batch_columns;
      while cnt > 0 do
      begin
        dest^ := bdest;
        inc(dest);
        dec(cnt);
      end;
      inc(dest, swidth);
      inc(frac, fracstep);

      bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
      ldest := precal8_tolong[bdest];
      cnt := num_iters;
      while cnt > 0 do
      begin
        PLongWord(dest)^ := ldest;
        inc(dest, 4);
        dec(cnt);
      end;
      cnt := rest_batch_columns;
      while cnt > 0 do
      begin
        dest^ := bdest;
        inc(dest);
        dec(cnt);
      end;
      inc(dest, swidth);
      inc(frac, fracstep);

      bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
      ldest := precal8_tolong[bdest];
      cnt := num_iters;
      while cnt > 0 do
      begin
        PLongWord(dest)^ := ldest;
        inc(dest, 4);
        dec(cnt);
      end;
      cnt := rest_batch_columns;
      while cnt > 0 do
      begin
        dest^ := bdest;
        inc(dest);
        dec(cnt);
      end;
      inc(dest, swidth);
      inc(frac, fracstep);

      bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
      ldest := precal8_tolong[bdest];
      cnt := num_iters;
      while cnt > 0 do
      begin
        PLongWord(dest)^ := ldest;
        inc(dest, 4);
        dec(cnt);
      end;
      cnt := rest_batch_columns;
      while cnt > 0 do
      begin
        dest^ := bdest;
        inc(dest);
        dec(cnt);
      end;
      inc(dest, swidth);
      inc(frac, fracstep);

      bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
      ldest := precal8_tolong[bdest];
      cnt := num_iters;
      while cnt > 0 do
      begin
        PLongWord(dest)^ := ldest;
        inc(dest, 4);
        dec(cnt);
      end;
      cnt := rest_batch_columns;
      while cnt > 0 do
      begin
        dest^ := bdest;
        inc(dest);
        dec(cnt);
      end;
      inc(dest, swidth);
      inc(frac, fracstep);

      bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
      ldest := precal8_tolong[bdest];
      cnt := num_iters;
      while cnt > 0 do
      begin
        PLongWord(dest)^ := ldest;
        inc(dest, 4);
        dec(cnt);
      end;
      cnt := rest_batch_columns;
      while cnt > 0 do
      begin
        dest^ := bdest;
        inc(dest);
        dec(cnt);
      end;
      inc(dest, swidth);
      inc(frac, fracstep);

      bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
      ldest := precal8_tolong[bdest];
      cnt := num_iters;
      while cnt > 0 do
      begin
        PLongWord(dest)^ := ldest;
        inc(dest, 4);
        dec(cnt);
      end;
      cnt := rest_batch_columns;
      while cnt > 0 do
      begin
        dest^ := bdest;
        inc(dest);
        dec(cnt);
      end;
      inc(dest, swidth);
      inc(frac, fracstep);

      bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
      ldest := precal8_tolong[bdest];
      cnt := num_iters;
      while cnt > 0 do
      begin
        PLongWord(dest)^ := ldest;
        inc(dest, 4);
        dec(cnt);
      end;
      cnt := rest_batch_columns;
      while cnt > 0 do
      begin
        dest^ := bdest;
        inc(dest);
        dec(cnt);
      end;
      inc(dest, swidth);
      inc(frac, fracstep);

      bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
      ldest := precal8_tolong[bdest];
      cnt := num_iters;
      while cnt > 0 do
      begin
        PLongWord(dest)^ := ldest;
        inc(dest, 4);
        dec(cnt);
      end;
      cnt := rest_batch_columns;
      while cnt > 0 do
      begin
        dest^ := bdest;
        inc(dest);
        dec(cnt);
      end;
      inc(dest, swidth);
      inc(frac, fracstep);

      bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
      ldest := precal8_tolong[bdest];
      cnt := num_iters;
      while cnt > 0 do
      begin
        PLongWord(dest)^ := ldest;
        inc(dest, 4);
        dec(cnt);
      end;
      cnt := rest_batch_columns;
      while cnt > 0 do
      begin
        dest^ := bdest;
        inc(dest);
        dec(cnt);
      end;
      inc(dest, swidth);
      inc(frac, fracstep);

      bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
      ldest := precal8_tolong[bdest];
      cnt := num_iters;
      while cnt > 0 do
      begin
        PLongWord(dest)^ := ldest;
        inc(dest, 4);
        dec(cnt);
      end;
      cnt := rest_batch_columns;
      while cnt > 0 do
      begin
        dest^ := bdest;
        inc(dest);
        dec(cnt);
      end;
      inc(dest, swidth);
      inc(frac, fracstep);

      bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
      ldest := precal8_tolong[bdest];
      cnt := num_iters;
      while cnt > 0 do
      begin
        PLongWord(dest)^ := ldest;
        inc(dest, 4);
        dec(cnt);
      end;
      cnt := rest_batch_columns;
      while cnt > 0 do
      begin
        dest^ := bdest;
        inc(dest);
        dec(cnt);
      end;
      inc(dest, swidth);
      inc(frac, fracstep);

      bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
      ldest := precal8_tolong[bdest];
      cnt := num_iters;
      while cnt > 0 do
      begin
        PLongWord(dest)^ := ldest;
        inc(dest, 4);
        dec(cnt);
      end;
      cnt := rest_batch_columns;
      while cnt > 0 do
      begin
        dest^ := bdest;
        inc(dest);
        dec(cnt);
      end;
      inc(dest, swidth);
      inc(frac, fracstep);
    end;
{$ENDIF}
    while frac <= fraclimit do
    begin
    // Re-map color indices from wall texture column
    //  using a lighting/special effects LUT.
      bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
      ldest := precal8_tolong[bdest];
      cnt := num_iters;
      while cnt > 0 do
      begin
        PLongWord(dest)^ := ldest;
        inc(dest, 4);
        dec(cnt);
      end;
      cnt := rest_batch_columns;
      while cnt > 0 do
      begin
        dest^ := bdest;
        inc(dest);
        dec(cnt);
      end;
      inc(dest, swidth);
      inc(frac, fracstep);
    end;
    exit;
  end;

  // Inner loop that does the actual texture mapping,
  //  e.g. a DDA-lile scaling.
  // This is as fast as it gets.
{$IFNDEF NO_INLINE_LOOPS}
  while frac <= fraclimit2 do
  begin
  // Re-map color indices from wall texture column
  //  using a lighting/special effects LUT.
    bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    cnt := p.num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      inc(dest);
      dec(cnt);
    end;
    inc(dest, swidth);
    inc(frac, fracstep);

    bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    cnt := p.num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      inc(dest);
      dec(cnt);
    end;
    inc(dest, swidth);
    inc(frac, fracstep);

    bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    cnt := p.num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      inc(dest);
      dec(cnt);
    end;
    inc(dest, swidth);
    inc(frac, fracstep);

    bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    cnt := p.num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      inc(dest);
      dec(cnt);
    end;
    inc(dest, swidth);
    inc(frac, fracstep);

    bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    cnt := p.num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      inc(dest);
      dec(cnt);
    end;
    inc(dest, swidth);
    inc(frac, fracstep);

    bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    cnt := p.num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      inc(dest);
      dec(cnt);
    end;
    inc(dest, swidth);
    inc(frac, fracstep);

    bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    cnt := p.num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      inc(dest);
      dec(cnt);
    end;
    inc(dest, swidth);
    inc(frac, fracstep);

    bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    cnt := p.num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      inc(dest);
      dec(cnt);
    end;
    inc(dest, swidth);
    inc(frac, fracstep);

    bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    cnt := p.num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      inc(dest);
      dec(cnt);
    end;
    inc(dest, swidth);
    inc(frac, fracstep);

    bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    cnt := p.num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      inc(dest);
      dec(cnt);
    end;
    inc(dest, swidth);
    inc(frac, fracstep);

    bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    cnt := p.num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      inc(dest);
      dec(cnt);
    end;
    inc(dest, swidth);
    inc(frac, fracstep);

    bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    cnt := p.num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      inc(dest);
      dec(cnt);
    end;
    inc(dest, swidth);
    inc(frac, fracstep);

    bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    cnt := p.num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      inc(dest);
      dec(cnt);
    end;
    inc(dest, swidth);
    inc(frac, fracstep);

    bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    cnt := p.num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      inc(dest);
      dec(cnt);
    end;
    inc(dest, swidth);
    inc(frac, fracstep);

    bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    cnt := p.num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      inc(dest);
      dec(cnt);
    end;
    inc(dest, swidth);
    inc(frac, fracstep);

    bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    cnt := p.num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      inc(dest);
      dec(cnt);
    end;
    inc(dest, swidth);
    inc(frac, fracstep);
  end;
{$ENDIF}
  while frac <= fraclimit do
  begin
  // Re-map color indices from wall texture column
  //  using a lighting/special effects LUT.
    bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    cnt := p.num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      inc(dest);
      dec(cnt);
    end;
    inc(dest, swidth);
    inc(frac, fracstep);
  end;
end;

//==============================================================================
//
// R_DrawColumnAlphaMedium_BatchMT
//
//==============================================================================
procedure R_DrawColumnAlphaMedium_BatchMT(p: Pspriterenderinfo_t);
var
  count: integer;
  dest: PByte;
  b: byte;
  u: integer;
  frac: fixed_t;
  fracstep: fixed_t;
  fraclimit: fixed_t;
  swidth: integer;
  cnt: integer;
begin
  count := p.dc_yh - p.dc_yl;

  // Zero length, column does not exceed a pixel.
  if count < 0 then
    exit;

  // Framebuffer destination address.
  // Use ylookup LUT to avoid multiply with ScreenWidth.
  // Use columnofs LUT for subwindows?
  dest := @((ylookup[p.dc_yl]^)[columnofs[p.dc_x]]);

  // Determine scaling,
  //  which is the only mapping to be done.
  fracstep := p.dc_iscale;
  frac := p.dc_texturemid + (p.dc_yl - centery) * fracstep;
  fraclimit := frac + count * fracstep;
  swidth := SCREENWIDTH - p.num_batch_columns;

  // Inner loop that does the actual texture mapping,
  //  e.g. a DDA-lile scaling.
  // This is as fast as it gets.
  while frac <= fraclimit do
  begin
  // Re-map color indices from wall texture column
  //  using a lighting/special effects LUT.

    b := dc_colormap[p.dc_source[(LongWord(frac) shr FRACBITS) and 127]];
    u := b shl 8;
    cnt := p.num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := curtrans8table[dest^ + u];
      inc(dest);
      dec(cnt);
    end;

    inc(dest, swidth);
    inc(frac, fracstep);
  end;
end;

//==============================================================================
//
// R_DrawColumnAddMedium_BatchMT
//
//==============================================================================
procedure R_DrawColumnAddMedium_BatchMT(p: Pspriterenderinfo_t);
var
  count: integer;
  dest: PByte;
  b: byte;
  u: integer;
  frac: fixed_t;
  fracstep: fixed_t;
  fraclimit: fixed_t;
  swidth: integer;
  cnt: integer;
begin
  count := p.dc_yh - p.dc_yl;

  // Zero length, column does not exceed a pixel.
  if count < 0 then
    exit;

  // Framebuffer destination address.
  // Use ylookup LUT to avoid multiply with ScreenWidth.
  // Use columnofs LUT for subwindows?
  dest := @((ylookup[p.dc_yl]^)[columnofs[p.dc_x]]);

  // Determine scaling,
  //  which is the only mapping to be done.
  fracstep := p.dc_iscale;
  frac := p.dc_texturemid + (p.dc_yl - centery) * fracstep;
  fraclimit := frac + count * fracstep;
  swidth := SCREENWIDTH - p.num_batch_columns;

  // Inner loop that does the actual texture mapping,
  //  e.g. a DDA-lile scaling.
  // This is as fast as it gets.
  while frac <= fraclimit do
  begin
  // Re-map color indices from wall texture column
  //  using a lighting/special effects LUT.

    b := dc_colormap[p.dc_source[(LongWord(frac) shr FRACBITS) and 127]];
    u := b shl 8;
    cnt := p.num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := curadd8table[dest^ + u];
      inc(dest);
      dec(cnt);
    end;

    inc(dest, swidth);
    inc(frac, fracstep);
  end;
end;

//==============================================================================
//
// R_DrawColumnSubtractMedium_BatchMT
//
//==============================================================================
procedure R_DrawColumnSubtractMedium_BatchMT(p: Pspriterenderinfo_t);
var
  count: integer;
  dest: PByte;
  b: byte;
  u: integer;
  frac: fixed_t;
  fracstep: fixed_t;
  fraclimit: fixed_t;
  swidth: integer;
  cnt: integer;
begin
  count := p.dc_yh - p.dc_yl;

  // Zero length, column does not exceed a pixel.
  if count < 0 then
    exit;

  // Framebuffer destination address.
  // Use ylookup LUT to avoid multiply with ScreenWidth.
  // Use columnofs LUT for subwindows?
  dest := @((ylookup[p.dc_yl]^)[columnofs[p.dc_x]]);

  // Determine scaling,
  //  which is the only mapping to be done.
  fracstep := p.dc_iscale;
  frac := p.dc_texturemid + (p.dc_yl - centery) * fracstep;
  fraclimit := frac + count * fracstep;
  swidth := SCREENWIDTH - p.num_batch_columns;

  // Inner loop that does the actual texture mapping,
  //  e.g. a DDA-lile scaling.
  // This is as fast as it gets.
  while frac <= fraclimit do
  begin
  // Re-map color indices from wall texture column
  //  using a lighting/special effects LUT.

    b := dc_colormap[p.dc_source[(LongWord(frac) shr FRACBITS) and 127]];
    u := b shl 8;
    cnt := p.num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := cursubtract8table[dest^ + u];
      inc(dest);
      dec(cnt);
    end;

    inc(dest, swidth);
    inc(frac, fracstep);
  end;
end;

//==============================================================================
//
// R_DrawColumnHi_BatchMT
//
//==============================================================================
procedure R_DrawColumnHi_BatchMT(p: Pspriterenderinfo_t);
var
  count: integer;
  destl: PLongWord;
  deststop: PLongWord;
  deststopX4: PLongWord;
  ldest: LongWord;
  c: LongWord;
  frac: fixed_t;
  fracstep: fixed_t;
  spot: integer;
  lastspot: integer;
  swidth: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
  pitch: integer;
  buf: twolongwords_t;
begin
  count := p.dc_yh - p.dc_yl;

  if count < 0 then
    exit;

  {$IFDEF DOOM_OR_STRIFE}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;

  destl := @((ylookupl[p.dc_yl]^)[columnofs[p.dc_x]]);
  pitch := p.num_batch_columns * SizeOf(LongWord);
  deststop := PLongWord(integer(destl) + pitch);
  swidth := SCREENWIDTH32PITCH - pitch;
  fracstep := p.dc_iscale;
  frac := p.dc_texturemid + (p.dc_yl - centery) * fracstep;
  lfactor := dc_lightlevel;

  if p.num_batch_columns > 4 then
  begin
    lastspot := 128;
    ldest := 0; // JVAL: avoid compiler warning
    deststopX4 := PLongWord(integer(deststop) - 4 * SizeOf(pointer));
    if lfactor >= 0 then
    begin
      R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, p.dc_fog);  // JVAL: Mars fog sectors
      while count >= 0 do
      begin
        spot := (LongWord(frac) shr FRACBITS) and 127;
        if lastspot <> spot then
        begin
          c := pal[p.dc_source[spot]];
          ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
          buf.longword1 := ldest;
          buf.longword2 := ldest;
          lastspot := spot;
        end;

        while integer(destl) <= integer(deststopX4) do
        begin
          PInt64(destl)^ := PInt64(@buf)^;
          inc(destl, 2);
          PInt64(destl)^ := PInt64(@buf)^;
          inc(destl, 2);
        end;

        while integer(destl) < integer(deststop) do
        begin
          destl^ := ldest;
          inc(destl);
        end;

        destl := PLongWord(integer(destl) + swidth);
        deststop := PLongWord(integer(destl) + pitch);
        deststopX4 := PLongWord(integer(deststop) - 4 * SizeOf(pointer));
        inc(frac, fracstep);
        dec(count);
      end;
    end
    else
    begin
      while count >= 0 do
      begin
        spot := (LongWord(frac) shr FRACBITS) and 127;
        if lastspot <> spot then
        begin
          c := pal[p.dc_source[spot]];
          r1 := c;
          g1 := c shr 8;
          b1 := c shr 16;
          ldest := precal32_ic[r1 + g1 + b1];
          buf.longword1 := ldest;
          buf.longword2 := ldest;
          lastspot := spot;
        end;

        while integer(destl) < integer(deststopX4) do
        begin
          PInt64(destl)^ := PInt64(@buf)^;
          inc(destl, 2);
          PInt64(destl)^ := PInt64(@buf)^;
          inc(destl, 2);
        end;

        while integer(destl) < integer(deststop) do
        begin
          destl^ := ldest;
          inc(destl);
        end;

        destl := PLongWord(integer(destl) + swidth);
        deststop := PLongWord(integer(destl) + pitch);
        deststopX4 := PLongWord(integer(deststop) - 4 * SizeOf(pointer));
        inc(frac, fracstep);
        dec(count);
      end;
    end;
  end
  else
  begin
    if lfactor >= 0 then
    begin
      R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, p.dc_fog);  // JVAL: Mars fog sectors
      while count >= 0 do
      begin
        spot := (LongWord(frac) shr FRACBITS) and 127;
        c := pal[p.dc_source[spot]];
        ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];

        while integer(destl) < integer(deststop) do
        begin
          destl^ := ldest;
          inc(destl);
        end;

        destl := PLongWord(integer(destl) + swidth);
        deststop := PLongWord(integer(destl) + pitch);
        inc(frac, fracstep);
        dec(count);
      end;
    end
    else
    begin
      while count >= 0 do
      begin
        spot := (LongWord(frac) shr FRACBITS) and 127;
        c := pal[p.dc_source[spot]];
        r1 := c;
        g1 := c shr 8;
        b1 := c shr 16;
        ldest := precal32_ic[r1 + g1 + b1];

        while integer(destl) < integer(deststop) do
        begin
          destl^ := ldest;
          inc(destl);
        end;

        destl := PLongWord(integer(destl) + swidth);
        deststop := PLongWord(integer(destl) + pitch);
        inc(frac, fracstep);
        dec(count);
      end;
    end;
  end;
end;

//==============================================================================
//
// R_DrawColumnAverageMedium_BatchMT
//
//==============================================================================
procedure R_DrawColumnAverageMedium_BatchMT(p: Pspriterenderinfo_t);
var
  count: integer;
  dest: PByte;
  b: byte;
  frac: fixed_t;
  fracstep: fixed_t;
  fraclimit: fixed_t;
  swidth: integer;
  cnt: integer;
  tbl: PByteArray;
begin
  count := p.dc_yh - p.dc_yl;

  // Zero length, column does not exceed a pixel.
  if count < 0 then
    exit;

  // Framebuffer destination address.
  // Use ylookup LUT to avoid multiply with ScreenWidth.
  // Use columnofs LUT for subwindows?
  dest := @((ylookup[p.dc_yl]^)[columnofs[p.dc_x]]);

  // Determine scaling,
  //  which is the only mapping to be done.
  fracstep := p.dc_iscale;
  frac := p.dc_texturemid + (p.dc_yl - centery) * fracstep;
  fraclimit := frac + count * fracstep;
  swidth := SCREENWIDTH - p.num_batch_columns;

  // Inner loop that does the actual texture mapping,
  //  e.g. a DDA-lile scaling.
  // This is as fast as it gets.
  while frac <= fraclimit do
  begin
  // Re-map color indices from wall texture column
  //  using a lighting/special effects LUT.

    b := p.dc_colormap[p.dc_source[(LongWord(frac) shr FRACBITS) and 127]];
    tbl := @averagetrans8table[b shl 8];
    cnt := p.num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := tbl[dest^];
      inc(dest);
      dec(cnt);
    end;

    inc(dest, swidth);
    inc(frac, fracstep);
  end;
end;

//==============================================================================
//
// R_DrawColumnAverageHi_BatchMT
//
//==============================================================================
procedure R_DrawColumnAverageHi_BatchMT(p: Pspriterenderinfo_t);
var
  count: integer;
  destb: PByte;
  frac: fixed_t;
  fracstep: fixed_t;
  swidth: integer;
  deststop: PByte;

// For inline color averaging
  r1, g1, b1: byte;
  c3: LongWord;
  rr, gg, bb: PByteArray;

begin
  count := p.dc_yh - p.dc_yl;

  if count < 0 then
    exit;

  destb := @((ylookupl[p.dc_yl]^)[columnofs[p.dc_x]]);

  fracstep := p.dc_iscale;
  frac := p.dc_texturemid + (p.dc_yl - centery) * fracstep;

  // Inner loop that does the actual texture mapping,
  //  e.g. a DDA-lile scaling.
  // This is as fast as it gets.
  swidth := SCREENWIDTH32PITCH - p.num_batch_columns * SizeOf(LongWord);

  while count >= 0 do
  begin
    c3 := p.dc_colormap32[p.dc_source[(LongWord(frac) shr FRACBITS) and 127]];
    r1 := c3;
    g1 := c3 shr 8;
    b1 := c3 shr 16;
    rr := @average_byte[r1];
    gg := @average_byte[g1];
    bb := @average_byte[b1];

    deststop := destb;
    inc(deststop, 4 * p.num_batch_columns);
    while destb <> deststop do
    begin
      PByteArray(destb)[0] := rr[PByteArray(destb)[0]];
      PByteArray(destb)[1] := gg[PByteArray(destb)[1]];
      PByteArray(destb)[2] := bb[PByteArray(destb)[2]];
      Inc(destb, 4);
    end;

    destb := PByte(integer(destb) + swidth);
    inc(frac, fracstep);
    dec(count);
  end;
end;

//==============================================================================
//
// R_DrawColumnAlphaHi_BatchMT
//
//==============================================================================
procedure R_DrawColumnAlphaHi_BatchMT(p: Pspriterenderinfo_t);
var
  count: integer;
  destl: PLongWord;
  frac: fixed_t;
  fracstep: fixed_t;
  fraclimit: fixed_t;
  swidth: integer;
  cfrac2: fixed_t;
  factor1: fixed_t;

// For inline color averaging
  r1, g1, b1: byte;
  r2, g2, b2: byte;
  fr2, fg2, fb2: integer;
  c1, c2, r, g, b: LongWord;

  cnt: integer;
begin
  count := p.dc_yh - p.dc_yl;

  if count < 0 then
    exit;

  destl := @((ylookupl[p.dc_yl]^)[columnofs[p.dc_x]]);

  fracstep := p.dc_iscale;
  frac := p.dc_texturemid + (p.dc_yl - centery) * fracstep;

  // Inner loop that does the actual texture mapping,
  //  e.g. a DDA-lile scaling.
  // This is as fast as it gets.
  swidth := SCREENWIDTH32PITCH - p.num_batch_columns * SizeOf(LongWord);
  cfrac2 := p.dc_alpha;
  factor1 := FRACUNIT - 1 - cfrac2;

  fraclimit := frac + fracstep * count;
  while frac <= fraclimit do
  begin
    c2 := p.dc_colormap32[p.dc_source[(LongWord(frac) shr FRACBITS) and 127]];
    r2 := c2;
    g2 := c2 shr 8;
    b2 := c2 shr 16;
    fr2 := cfrac2 * r2;
    fg2 := cfrac2 * g2;
    fb2 := cfrac2 * b2;

    cnt := p.num_batch_columns;
    while cnt > 0 do
    begin
    // Color averaging
      c1 := destl^;
      r1 := c1;
      g1 := c1 shr 8;
      b1 := c1 shr 16;

      r := ((fr2) + (r1 * factor1)) shr FRACBITS;
      g := ((fg2) + (g1 * factor1)) shr FRACBITS;
      b := ((fb2) + (b1 * factor1)) and $FF0000;

      destl^ := r + g shl 8 + b;
      inc(destl);
      dec(cnt);
    end;

    destl := PLongWord(integer(destl) + swidth);
    inc(frac, fracstep);
  end;
end;

//==============================================================================
//
// R_DrawColumnAddHi_BatchMT
//
//==============================================================================
procedure R_DrawColumnAddHi_BatchMT(p: Pspriterenderinfo_t);
var
  count: integer;
  destl: PLongWord;
  frac: fixed_t;
  fracstep: fixed_t;
  fraclimit: fixed_t;
  swidth: integer;

  addfactor: integer;

// For inline color averaging
  r1, g1, b1: byte;
  r2, g2, b2: byte;
  c1, c2, r, g, b: LongWord;

  cnt: integer;
begin
  count := p.dc_yh - p.dc_yl;

  if count < 0 then
    exit;

  destl := @((ylookupl[p.dc_yl]^)[columnofs[p.dc_x]]);

  fracstep := p.dc_iscale;
  frac := p.dc_texturemid + (p.dc_yl - centery) * fracstep;

  // Inner loop that does the actual texture mapping,
  //  e.g. a DDA-lile scaling.
  // This is as fast as it gets.
  swidth := SCREENWIDTH32PITCH - p.num_batch_columns * SizeOf(LongWord);

  addfactor := p.dc_alpha;

  fraclimit := frac + fracstep * count;
  while frac <= fraclimit do
  begin
    c2 := p.dc_colormap32[p.dc_source[(LongWord(frac) shr FRACBITS) and 127]];
    r2 := c2;
    g2 := c2 shr 8;
    b2 := c2 shr 16;
    if addfactor < FRACUNIT then
    begin
      r2 := (r2 * addfactor) shr FRACBITS;
      g2 := (g2 * addfactor) shr FRACBITS;
      b2 := (b2 * addfactor) shr FRACBITS;
    end;

    cnt := p.num_batch_columns;
    while cnt > 0 do
    begin
    // Color averaging
      c1 := destl^;
      r1 := c1;
      g1 := c1 shr 8;
      b1 := c1 shr 16;

      r := r2 + r1;
      if r > 255 then
        r := 255;
      g := g2 + g1;
      if g > 255 then
        g := 255;
      b := b2 + b1;
      if b > 255 then
        b := 255;

      destl^ := r + g shl 8 + b shl 16;
      inc(destl);
      dec(cnt);
    end;

    destl := PLongWord(integer(destl) + swidth);
    inc(frac, fracstep);
  end;
end;

//==============================================================================
//
// R_DrawColumnSubtractHi_BatchMT
//
//==============================================================================
procedure R_DrawColumnSubtractHi_BatchMT(p: Pspriterenderinfo_t);
var
  count: integer;
  destl: PLongWord;
  frac: fixed_t;
  fracstep: fixed_t;
  fraclimit: fixed_t;
  swidth: integer;

  subfactor: integer;

// For inline color averaging
  r1, g1, b1: byte;
  r2, g2, b2: byte;
  c1, c2, r, g, b: LongWord;

  cnt: integer;
begin
  count := p.dc_yh - p.dc_yl;

  if count < 0 then
    exit;

  destl := @((ylookupl[p.dc_yl]^)[columnofs[p.dc_x]]);

  fracstep := p.dc_iscale;
  frac := p.dc_texturemid + (p.dc_yl - centery) * fracstep;

  // Inner loop that does the actual texture mapping,
  //  e.g. a DDA-lile scaling.
  // This is as fast as it gets.
  swidth := SCREENWIDTH32PITCH - p.num_batch_columns * SizeOf(LongWord);

  subfactor := p.dc_alpha;

  fraclimit := frac + fracstep * count;
  while frac <= fraclimit do
  begin
    c2 := p.dc_colormap32[p.dc_source[(LongWord(frac) shr FRACBITS) and 127]];
    r2 := c2;
    g2 := c2 shr 8;
    b2 := c2 shr 16;
    if subfactor < FRACUNIT then
    begin
      r2 := (r2 * subfactor) shr FRACBITS;
      g2 := (g2 * subfactor) shr FRACBITS;
      b2 := (b2 * subfactor) shr FRACBITS;
    end;

    cnt := p.num_batch_columns;
    while cnt > 0 do
    begin
    // Color averaging
      c1 := destl^;
      r1 := c1;
      g1 := c1 shr 8;
      b1 := c1 shr 16;

      if r2 > r1 then
        r := 0
      else
        r := r1 - r2;
      if g2 > g1 then
        g := 0
      else
        g := g1 - g2;
      if b2 > b1 then
        b := 0
      else
        b := b1 - b2;

      destl^ := r + g shl 8 + b shl 16;
      inc(destl);
      dec(cnt);
    end;

    destl := PLongWord(integer(destl) + swidth);
    inc(frac, fracstep);
  end;
end;

var
  maxspritejobs: integer = 0;
  numspritejobs: integer = 0;
  spritejobs: Pspriterenderinfo_tArray = nil;

//==============================================================================
//
// R_SpriteAddMTInfo
//
//==============================================================================
function R_SpriteAddMTInfo: Pspriterenderinfo_t;
begin
  inc(numspritejobs);
  if numspritejobs >= maxspritejobs then
  begin
    spritejobs := Z_Realloc(spritejobs, numspritejobs * SizeOf(spriterenderinfo_t), PU_STATIC, nil);
    maxspritejobs := numspritejobs;
  end;
  result := @spritejobs[numspritejobs - 1];
end;

const
  MAXSPRITETHREADS = 16;

var
  SPRIDS: array[0..MAXSPRITETHREADS - 1] of integer = (
    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15
  );

var
  numspritethreads: integer;

//==============================================================================
//
// _sprite_render_thr
//
//==============================================================================
function _sprite_render_thr(p: pointer): integer; stdcall;
var
  i: integer;
begin
  i := PInteger(p)^;
  while i < numspritejobs do
  begin
    spritejobs[i].proc(@spritejobs[i]);
    Inc(i, numspritethreads);
  end;
  result := 0;
end;

//==============================================================================
//
// R_SpriteRenderMT
//
//==============================================================================
procedure R_SpriteRenderMT;
var
  ncpus: integer;
begin
  if numspritejobs = 0 then
    exit;

  numspritethreads := numspritejobs;
  ncpus := I_GetNumCPUs;
  if ncpus > MAXSPRITETHREADS then
    ncpus := MAXSPRITETHREADS;
  if numspritethreads > ncpus then
    numspritethreads := ncpus;

  case numspritethreads of
    1: _sprite_render_thr(@SPRIDS[0]);
    2: MT_Execute(
        @_sprite_render_thr, @SPRIDS[0],
        @_sprite_render_thr, @SPRIDS[1]
        );
    3: MT_Execute(
        @_sprite_render_thr, @SPRIDS[0],
        @_sprite_render_thr, @SPRIDS[1],
        @_sprite_render_thr, @SPRIDS[2]
        );
    4: MT_Execute4(
        @_sprite_render_thr, @SPRIDS[0],
        @_sprite_render_thr, @SPRIDS[1],
        @_sprite_render_thr, @SPRIDS[2],
        @_sprite_render_thr, @SPRIDS[3]
        );
    5: MT_Execute(
        @_sprite_render_thr, @SPRIDS[0],
        @_sprite_render_thr, @SPRIDS[1],
        @_sprite_render_thr, @SPRIDS[2],
        @_sprite_render_thr, @SPRIDS[3],
        @_sprite_render_thr, @SPRIDS[4]
        );
    6: MT_Execute6(
        @_sprite_render_thr, @SPRIDS[0],
        @_sprite_render_thr, @SPRIDS[1],
        @_sprite_render_thr, @SPRIDS[2],
        @_sprite_render_thr, @SPRIDS[3],
        @_sprite_render_thr, @SPRIDS[4],
        @_sprite_render_thr, @SPRIDS[5]
        );
    7: MT_Execute(
        @_sprite_render_thr, @SPRIDS[0],
        @_sprite_render_thr, @SPRIDS[1],
        @_sprite_render_thr, @SPRIDS[2],
        @_sprite_render_thr, @SPRIDS[3],
        @_sprite_render_thr, @SPRIDS[4],
        @_sprite_render_thr, @SPRIDS[5],
        @_sprite_render_thr, @SPRIDS[6]
        );
    8: MT_Execute8(
        @_sprite_render_thr, @SPRIDS[0],
        @_sprite_render_thr, @SPRIDS[1],
        @_sprite_render_thr, @SPRIDS[2],
        @_sprite_render_thr, @SPRIDS[3],
        @_sprite_render_thr, @SPRIDS[4],
        @_sprite_render_thr, @SPRIDS[5],
        @_sprite_render_thr, @SPRIDS[6],
        @_sprite_render_thr, @SPRIDS[7]
        );
    9: MT_Execute(
        @_sprite_render_thr, @SPRIDS[0],
        @_sprite_render_thr, @SPRIDS[1],
        @_sprite_render_thr, @SPRIDS[2],
        @_sprite_render_thr, @SPRIDS[3],
        @_sprite_render_thr, @SPRIDS[4],
        @_sprite_render_thr, @SPRIDS[5],
        @_sprite_render_thr, @SPRIDS[6],
        @_sprite_render_thr, @SPRIDS[7],
        @_sprite_render_thr, @SPRIDS[8]
        );
   10: MT_Execute(
        @_sprite_render_thr, @SPRIDS[0],
        @_sprite_render_thr, @SPRIDS[1],
        @_sprite_render_thr, @SPRIDS[2],
        @_sprite_render_thr, @SPRIDS[3],
        @_sprite_render_thr, @SPRIDS[4],
        @_sprite_render_thr, @SPRIDS[5],
        @_sprite_render_thr, @SPRIDS[6],
        @_sprite_render_thr, @SPRIDS[7],
        @_sprite_render_thr, @SPRIDS[8],
        @_sprite_render_thr, @SPRIDS[9]
        );
   11: MT_Execute(
        @_sprite_render_thr, @SPRIDS[0],
        @_sprite_render_thr, @SPRIDS[1],
        @_sprite_render_thr, @SPRIDS[2],
        @_sprite_render_thr, @SPRIDS[3],
        @_sprite_render_thr, @SPRIDS[4],
        @_sprite_render_thr, @SPRIDS[5],
        @_sprite_render_thr, @SPRIDS[6],
        @_sprite_render_thr, @SPRIDS[7],
        @_sprite_render_thr, @SPRIDS[8],
        @_sprite_render_thr, @SPRIDS[9],
        @_sprite_render_thr, @SPRIDS[10]
        );
   12: MT_Execute12(
        @_sprite_render_thr, @SPRIDS[0],
        @_sprite_render_thr, @SPRIDS[1],
        @_sprite_render_thr, @SPRIDS[2],
        @_sprite_render_thr, @SPRIDS[3],
        @_sprite_render_thr, @SPRIDS[4],
        @_sprite_render_thr, @SPRIDS[5],
        @_sprite_render_thr, @SPRIDS[6],
        @_sprite_render_thr, @SPRIDS[7],
        @_sprite_render_thr, @SPRIDS[8],
        @_sprite_render_thr, @SPRIDS[9],
        @_sprite_render_thr, @SPRIDS[10],
        @_sprite_render_thr, @SPRIDS[11]
        );
   13: MT_Execute(
        @_sprite_render_thr, @SPRIDS[0],
        @_sprite_render_thr, @SPRIDS[1],
        @_sprite_render_thr, @SPRIDS[2],
        @_sprite_render_thr, @SPRIDS[3],
        @_sprite_render_thr, @SPRIDS[4],
        @_sprite_render_thr, @SPRIDS[5],
        @_sprite_render_thr, @SPRIDS[6],
        @_sprite_render_thr, @SPRIDS[7],
        @_sprite_render_thr, @SPRIDS[8],
        @_sprite_render_thr, @SPRIDS[9],
        @_sprite_render_thr, @SPRIDS[10],
        @_sprite_render_thr, @SPRIDS[11],
        @_sprite_render_thr, @SPRIDS[12]
        );
   14: MT_Execute(
        @_sprite_render_thr, @SPRIDS[0],
        @_sprite_render_thr, @SPRIDS[1],
        @_sprite_render_thr, @SPRIDS[2],
        @_sprite_render_thr, @SPRIDS[3],
        @_sprite_render_thr, @SPRIDS[4],
        @_sprite_render_thr, @SPRIDS[5],
        @_sprite_render_thr, @SPRIDS[6],
        @_sprite_render_thr, @SPRIDS[7],
        @_sprite_render_thr, @SPRIDS[8],
        @_sprite_render_thr, @SPRIDS[9],
        @_sprite_render_thr, @SPRIDS[10],
        @_sprite_render_thr, @SPRIDS[11],
        @_sprite_render_thr, @SPRIDS[12],
        @_sprite_render_thr, @SPRIDS[13]
        );
   15: MT_Execute(
        @_sprite_render_thr, @SPRIDS[0],
        @_sprite_render_thr, @SPRIDS[1],
        @_sprite_render_thr, @SPRIDS[2],
        @_sprite_render_thr, @SPRIDS[3],
        @_sprite_render_thr, @SPRIDS[4],
        @_sprite_render_thr, @SPRIDS[5],
        @_sprite_render_thr, @SPRIDS[6],
        @_sprite_render_thr, @SPRIDS[7],
        @_sprite_render_thr, @SPRIDS[8],
        @_sprite_render_thr, @SPRIDS[9],
        @_sprite_render_thr, @SPRIDS[10],
        @_sprite_render_thr, @SPRIDS[11],
        @_sprite_render_thr, @SPRIDS[12],
        @_sprite_render_thr, @SPRIDS[13],
        @_sprite_render_thr, @SPRIDS[14]
        );
  else MT_Execute16(
        @_sprite_render_thr, @SPRIDS[0],
        @_sprite_render_thr, @SPRIDS[1],
        @_sprite_render_thr, @SPRIDS[2],
        @_sprite_render_thr, @SPRIDS[3],
        @_sprite_render_thr, @SPRIDS[4],
        @_sprite_render_thr, @SPRIDS[5],
        @_sprite_render_thr, @SPRIDS[6],
        @_sprite_render_thr, @SPRIDS[7],
        @_sprite_render_thr, @SPRIDS[8],
        @_sprite_render_thr, @SPRIDS[9],
        @_sprite_render_thr, @SPRIDS[10],
        @_sprite_render_thr, @SPRIDS[11],
        @_sprite_render_thr, @SPRIDS[12],
        @_sprite_render_thr, @SPRIDS[13],
        @_sprite_render_thr, @SPRIDS[14],
        @_sprite_render_thr, @SPRIDS[15]
        );
  end;

  numspritejobs := 0;
end;

end.
