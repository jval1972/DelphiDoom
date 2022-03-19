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

unit r_draw_subtractive;

interface

//==============================================================================
// R_DrawColumnSubtractLowest
//
// Alpha column drawers (transparency effects)
//
//==============================================================================
procedure R_DrawColumnSubtractLowest;

//==============================================================================
//
// R_DrawColumnSubtractLow
//
//==============================================================================
procedure R_DrawColumnSubtractLow;

//==============================================================================
//
// R_DrawColumnSubtractMedium
//
//==============================================================================
procedure R_DrawColumnSubtractMedium;

//==============================================================================
//
// R_DrawColumnSubtractHi
//
//==============================================================================
procedure R_DrawColumnSubtractHi;

implementation

uses
  d_delphi,
  doomdef,
  m_fixed,
  r_column,
  r_draw,
  r_trans8,
  r_main;

//==============================================================================
//
// R_DrawColumnSubtractLowest
//
//==============================================================================
procedure R_DrawColumnSubtractLowest;
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
    buf.byte1 := cursubtract8table[dest^ + (dc_colormap[dc_source[(LongWord(frac) shr FRACBITS) and 127]] shl 8)];
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
    buf.byte1 := cursubtract8table[dest^ + (dc_colormap[dc_source[(LongWord(frac) shr FRACBITS) and 127]] shl 8)];
    buf.byte2 := buf.byte1;
    PWord(dest)^ := Word(buf);
    inc(dest, swidth);

    inc(frac, dc_iscale);
  end;
end;

//==============================================================================
//
// R_DrawColumnSubtractLow
//
//==============================================================================
procedure R_DrawColumnSubtractLow;
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
    bdest := cursubtract8table[dest^ + (dc_colormap[dc_source[(LongWord(frac) shr FRACBITS) and 127]] shl 8)];

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
    dest^ := cursubtract8table[dest^ + (dc_colormap[dc_source[(LongWord(frac) shr FRACBITS) and 127]] shl 8)];
    inc(dest, swidth);

    inc(frac, dc_iscale);
  end;
end;

//==============================================================================
//
// R_DrawColumnSubtractMedium
//
//==============================================================================
procedure R_DrawColumnSubtractMedium;
var
  count: integer;
  dest: PByte;
  frac: fixed_t;
  fracstep: fixed_t;
  fraclimit: fixed_t;
  swidth: integer;
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
  swidth := SCREENWIDTH;

  // Inner loop that does the actual texture mapping,
  //  e.g. a DDA-lile scaling.
  // This is as fast as it gets.
  while frac < fraclimit do
  begin
  // Re-map color indices from wall texture column
  //  using a lighting/special effects LUT.
    dest^ := cursubtract8table[dest^ + (dc_colormap[dc_source[(LongWord(frac) shr FRACBITS) and 127]] shl 8)];
    inc(dest, swidth);
    inc(frac, fracstep);

    dest^ := cursubtract8table[dest^ + (dc_colormap[dc_source[(LongWord(frac) shr FRACBITS) and 127]] shl 8)];
    inc(dest, swidth);
    inc(frac, fracstep);
  end;
  while frac <= fraclimit do
  begin
  // Re-map color indices from wall texture column
  //  using a lighting/special effects LUT.
    dest^ := cursubtract8table[dest^ + (dc_colormap[dc_source[(LongWord(frac) shr FRACBITS) and 127]] shl 8)];
    inc(dest, swidth);
    inc(frac, fracstep);
  end;
end;

//==============================================================================
//
// R_DrawColumnSubtractHi
//
//==============================================================================
procedure R_DrawColumnSubtractHi;
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
  count := dc_yh - dc_yl;

  if count < 0 then
    exit;

  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);

  fracstep := dc_iscale;
  frac := dc_texturemid + (dc_yl - centery) * fracstep;

  // Inner loop that does the actual texture mapping,
  //  e.g. a DDA-lile scaling.
  // This is as fast as it gets.
  swidth := SCREENWIDTH32PITCH;

  subfactor := dc_alpha;

  fraclimit := frac + fracstep * count;
  while frac < fraclimit do
  begin
    c1 := destl^;
    c2 := dc_colormap32[dc_source[(LongWord(frac) shr FRACBITS) and 127]];

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

end.

