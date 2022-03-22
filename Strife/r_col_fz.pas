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

unit r_col_fz;

interface

//==============================================================================
//
// R_DrawFuzzColumn1
//
//==============================================================================
procedure R_DrawFuzzColumn1;

//==============================================================================
//
// R_DrawFuzzColumn2
//
//==============================================================================
procedure R_DrawFuzzColumn2;

//==============================================================================
//
// R_DrawFuzzColumnTL
//
//==============================================================================
procedure R_DrawFuzzColumnTL;

//==============================================================================
//
// R_DrawFuzzColumn1Hi
//
//==============================================================================
procedure R_DrawFuzzColumn1Hi;

//==============================================================================
//
// R_DrawFuzzColumn2Hi
//
//==============================================================================
procedure R_DrawFuzzColumn2Hi;

//==============================================================================
//
// R_DrawFuzzColumnHiTL
//
//==============================================================================
procedure R_DrawFuzzColumnHiTL;

//==============================================================================
//
// R_DrawFuzzColumn1Hi32
//
//==============================================================================
procedure R_DrawFuzzColumn1Hi32;

//==============================================================================
//
// R_DrawFuzzColumn2Hi32
//
//==============================================================================
procedure R_DrawFuzzColumn2Hi32;

implementation

uses
  d_delphi,
  doomdef,
  m_fixed,
  r_draw,
  r_main,
  r_column,
  r_precalc;

//==============================================================================
// R_DrawFuzzColumn1
//
// Framebuffer postprocessing.
// Creates a fuzzy image by copying pixels
//  from adjacent ones to left and right.
// Used with an all black colormap, this
//  could create the SHADOW effect,
//  i.e. spectres and invisible players.
//
//==============================================================================
procedure R_DrawFuzzColumn1; // DrawTLColumn
var
  count: integer;
  i: integer;
  dest: PByteArray;
  frac, fracstep: fixed_t;
begin
  count := dc_yh - dc_yl;

  // Zero length.
  if count < 0 then
    exit;

  // Does not work with blocky mode.
  dest := @((ylookup[dc_yl]^)[columnofs[dc_x]]);
  fracstep := dc_iscale;
  frac := dc_texturemid + (dc_yl - centery) * fracstep;

  for i := 0 to count do
  begin
    dest[0] := xlatab[(dest[0] shl 8) +
      dc_colormap[dc_source[(frac shr FRACBITS) and 127]]];

    dest := @dest[SCREENWIDTH];
    inc(frac, fracstep);
  end;
end;

//==============================================================================
//
// R_DrawFuzzColumn2
//
//==============================================================================
procedure R_DrawFuzzColumn2; // DrawMVisTLColumn
var
  count: integer;
  i: integer;
  dest: PByteArray;
  frac, fracstep: fixed_t;
begin
  count := dc_yh - dc_yl;

  // Zero length.
  if count < 0 then
    exit;

  // Does not work with blocky mode.
  dest := @((ylookup[dc_yl]^)[columnofs[dc_x]]);
  fracstep := dc_iscale;
  frac := dc_texturemid + (dc_yl - centery) * fracstep;

  for i := 0 to count do
  begin
    dest[0] := xlatab[dest[0] +
      (dc_colormap[dc_source[(frac shr FRACBITS) and 127]] shl 8)];

    dest := @dest[SCREENWIDTH];
    inc(frac, fracstep);
  end;
end;

//==============================================================================
//
// R_DrawFuzzColumnTL
//
//==============================================================================
procedure R_DrawFuzzColumnTL;
var
  count: integer;
  i: integer;
  dest: PByteArray;
  frac, fracstep: fixed_t;
begin
  count := dc_yh - dc_yl;

  // Zero length.
  if count < 0 then
    exit;

  // Does not work with blocky mode.
  dest := @((ylookup[dc_yl]^)[columnofs[dc_x]]);
  fracstep := dc_iscale;
  frac := dc_texturemid + (dc_yl - centery) * fracstep;

  for i := 0 to count do
  begin
    dest[0] := xlatab[(dest[0] shl 8) +
      dc_colormap[dc_translation[dc_source[(frac shr FRACBITS) and 127]]]];

    dest := @dest[SCREENWIDTH];
    inc(frac, fracstep);
  end;
end;

//==============================================================================
//
// R_DrawFuzzColumn1Hi
//
//==============================================================================
procedure R_DrawFuzzColumn1Hi;
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
  cfrac2 := FRACUNIT div 4;
  factor1 := FRACUNIT - 1 - cfrac2;

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

    r := ((r2 * cfrac2) + (r1 * factor1)) shr FRACBITS;
    g := ((g2 * cfrac2) + (g1 * factor1)) shr FRACBITS;
    b := ((b2 * cfrac2) + (b1 * factor1)) and $FF0000;

    destl^ := r + g shl 8 + b;

    destl := PLongWord(integer(destl) + swidth);
    inc(frac, fracstep);
  end;
end;

//==============================================================================
//
// R_DrawFuzzColumn2Hi
//
//==============================================================================
procedure R_DrawFuzzColumn2Hi;
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
  cfrac2 := 3 * (FRACUNIT div 4);
  factor1 := FRACUNIT - 1 - cfrac2;

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

    r := ((r2 * cfrac2) + (r1 * factor1)) shr FRACBITS;
    g := ((g2 * cfrac2) + (g1 * factor1)) shr FRACBITS;
    b := ((b2 * cfrac2) + (b1 * factor1)) and $FF0000;

    destl^ := r + g shl 8 + b;

    destl := PLongWord(integer(destl) + swidth);
    inc(frac, fracstep);
  end;
end;

//==============================================================================
//
// R_DrawFuzzColumnHiTL
//
//==============================================================================
procedure R_DrawFuzzColumnHiTL;
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
  cfrac2 := 3 * (FRACUNIT div 4);
  factor1 := FRACUNIT - 1 - cfrac2;

  fraclimit := frac + fracstep * count;
  while frac < fraclimit do
  begin
    c1 := destl^;
    c2 := dc_colormap32[dc_translation[dc_source[(LongWord(frac) shr FRACBITS) and 127]]];

    // Color averaging
    r1 := c1;
    g1 := c1 shr 8;
    b1 := c1 shr 16;
    r2 := c2;
    g2 := c2 shr 8;
    b2 := c2 shr 16;

    r := ((r2 * cfrac2) + (r1 * factor1)) shr FRACBITS;
    g := ((g2 * cfrac2) + (g1 * factor1)) shr FRACBITS;
    b := ((b2 * cfrac2) + (b1 * factor1)) and $FF0000;

    destl^ := r + g shl 8 + b;

    destl := PLongWord(integer(destl) + swidth);
    inc(frac, fracstep);
  end;
end;

//==============================================================================
//
// R_DrawFuzzColumn1Hi32
//
//==============================================================================
procedure R_DrawFuzzColumn1Hi32;
{$DEFINE ONE}
{$I R_DrawFuzzColumnHi32.inc}

//==============================================================================
//
// R_DrawFuzzColumn2Hi32
//
//==============================================================================
procedure R_DrawFuzzColumn2Hi32;
{$UNDEF ONE}
{$I R_DrawFuzzColumnHi32.inc}

end.
