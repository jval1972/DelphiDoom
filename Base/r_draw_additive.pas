//------------------------------------------------------------------------------
//
//  DelphiDoom: A modified and improved DOOM engine for Windows
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 1993-1996 by id Software, Inc.
//  Copyright (C) 2004-2020 by Jim Valavanis
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

unit r_draw_additive;

interface

// Alpha column drawers (transparency effects)
procedure R_DrawColumnAddLowest;
procedure R_DrawColumnAddLow;
procedure R_DrawColumnAddMedium;
procedure R_DrawColumnAddHi;

implementation

uses
  d_delphi,
  doomdef,
  m_fixed,
  r_column,
  r_draw,
  r_trans8,
  r_main;

procedure R_DrawColumnAddLowest;
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
    buf.byte1 := curadd8table[dest^ + (dc_colormap[dc_source[(LongWord(frac) shr FRACBITS) and 127]] shl 8)];
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
    buf.byte1 := curadd8table[dest^ + (dc_colormap[dc_source[(LongWord(frac) shr FRACBITS) and 127]] shl 8)];
    buf.byte2 := buf.byte1;
    PWord(dest)^ := Word(buf);
    inc(dest, swidth);

    inc(frac, dc_iscale);
  end;
end;

procedure R_DrawColumnAddLow;
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
    bdest := curadd8table[dest^ + (dc_colormap[dc_source[(LongWord(frac) shr FRACBITS) and 127]] shl 8)];

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
    dest^ := curadd8table[dest^ + (dc_colormap[dc_source[(LongWord(frac) shr FRACBITS) and 127]] shl 8)];
    inc(dest, swidth);

    inc(frac, dc_iscale);
  end;
end;

procedure R_DrawColumnAddMedium;
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
  while frac <= fraclimit do
  begin
  // Re-map color indices from wall texture column
  //  using a lighting/special effects LUT.
    dest^ := curadd8table[dest^ + (dc_colormap[dc_source[(LongWord(frac) shr FRACBITS) and 127]] shl 8)];

    inc(dest, swidth);
    inc(frac, fracstep);
  end;
end;

procedure R_DrawColumnAddHi;
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

  addfactor := dc_alpha;

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

end.

