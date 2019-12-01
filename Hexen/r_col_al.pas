//------------------------------------------------------------------------------
//
//  DelphiHexen: A modified and improved Hexen port for Windows
//  based on original Linux Doom as published by "id Software", on
//  Hexen source as published by "Raven" software and DelphiDoom
//  as published by Jim Valavanis.
//  Copyright (C) 2004-2008 by Jim Valavanis
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
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : http://delphidoom.sitesled.com/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit r_col_al;

interface

// Alpha column drawers (transparency effects)
procedure R_DrawColumnAlphaMedium;
procedure R_DrawColumnAlphaHi;

implementation

uses
  d_delphi,
  doomdef,
  m_fixed,
  r_draw, r_main, r_column, r_hires,
  v_video;

// In 8 bit mode we diher, regardless of dc_alpha value
procedure R_DrawColumnAlphaMedium;
var
  count: integer;
  dest: PByte;
  frac: fixed_t;
  fracstep: fixed_t;
  fraclimit: fixed_t;
  swidth: integer;
begin
  if Odd(dc_yh) then
    inc(dc_yh);
  if Odd(dc_yl) then
    dec(dc_yl);

  count := dc_yh - dc_yl;

  // Zero length, column does not exceed a pixel.
  if count <= 0 then
    exit;

  // Framebuffer destination address.
  // Use ylookup LUT to avoid multiply with ScreenWidth.
  // Use columnofs LUT for subwindows?
  dest := @((ylookup[dc_yl]^)[columnofs[dc_x]]);
  if Odd(dc_yl + dc_x) then
    inc(dest, SCREENWIDTH);

  // Determine scaling,
  //  which is the only mapping to be done.
  frac := dc_texturemid + (dc_yl - centery) * dc_iscale;
  fraclimit := frac + count * dc_iscale;
  fracstep := 2 * dc_iscale;
  swidth := 2 * SCREENWIDTH;

  // Inner loop that does the actual texture mapping,
  //  e.g. a DDA-lile scaling.
  // This is as fast as it gets.
  while frac < fraclimit do
  begin
  // Re-map color indices from wall texture column
  //  using a lighting/special effects LUT.
    dest^ := dc_colormap[dc_source[(LongWord(frac) shr FRACBITS) and 127]];

    inc(dest, swidth);
    inc(frac, fracstep);
  end;
end;

procedure R_DrawColumnAlphaHi;
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
  cfrac2 := dc_alpha;


  fraclimit := frac + fracstep * count;
  while frac < fraclimit do
  begin
    c1 := destl^;
    c2 := dc_colormap32[dc_source[(LongWord(frac) shr FRACBITS) and 127]];
    {$UNDEF FOG}
    {$I R_ColorAverage.inc}
    destl^ := r + g shl 8 + b;

    destl := PLongWord(integer(destl) + swidth);
    inc(frac, fracstep);
  end;
end;

end.

