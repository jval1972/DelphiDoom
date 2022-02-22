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

unit r_col_fz;

interface

//==============================================================================
//
// R_InitFuzzTable
//
//==============================================================================
procedure R_InitFuzzTable;

//==============================================================================
// R_DrawFuzzColumn
//
// The Spectre/Invisibility effect.
//
//==============================================================================
procedure R_DrawFuzzColumn;

//==============================================================================
//
// R_DrawFuzzColumn32
//
//==============================================================================
procedure R_DrawFuzzColumn32;

//==============================================================================
//
// R_DrawFuzzColumnHi
//
//==============================================================================
procedure R_DrawFuzzColumnHi;

//
// Spectre/Invisibility.
//
const
  FUZZTABLE = 50;
  FUZZOFF = 1;

  fuzzoffset: array[0..FUZZTABLE - 1] of integer = (
    FUZZOFF,-FUZZOFF, FUZZOFF,-FUZZOFF, FUZZOFF, FUZZOFF,-FUZZOFF,
    FUZZOFF, FUZZOFF,-FUZZOFF, FUZZOFF, FUZZOFF, FUZZOFF,-FUZZOFF,
    FUZZOFF, FUZZOFF, FUZZOFF,-FUZZOFF,-FUZZOFF,-FUZZOFF,-FUZZOFF,
    FUZZOFF,-FUZZOFF,-FUZZOFF, FUZZOFF, FUZZOFF, FUZZOFF, FUZZOFF,-FUZZOFF,
    FUZZOFF,-FUZZOFF, FUZZOFF, FUZZOFF,-FUZZOFF,-FUZZOFF, FUZZOFF,
    FUZZOFF,-FUZZOFF,-FUZZOFF,-FUZZOFF,-FUZZOFF, FUZZOFF, FUZZOFF,
    FUZZOFF, FUZZOFF,-FUZZOFF, FUZZOFF, FUZZOFF,-FUZZOFF, FUZZOFF
  );

var
  sfuzzoffset: array[0..FUZZTABLE - 1] of integer;
  fuzzpos: integer = 0;

implementation

uses
  d_delphi,
  doomdef,
  r_data,
  r_draw,
  r_column,
  r_hires;

//==============================================================================
// R_DrawFuzzColumn
//
// Framebuffer postprocessing.
// Creates a fuzzy image by copying pixels
//  from adjacent ones to left and right.
// Used with an all black colormap, this
//  could create the SHADOW effect,
//  i.e. spectres and invisible players.
//
//==============================================================================
procedure R_DrawFuzzColumn;
var
  count: integer;
  i: integer;
  dest: PByteArray;
begin
  // Adjust borders. Low...
  if dc_yl = 0 then
    dc_yl := 1;

  // .. and high.
  if dc_yh = viewheight - 1 then
    dc_yh := viewheight - 2;

  count := dc_yh - dc_yl;

  // Zero length.
  if count < 0 then
    exit;

  // Does not work with blocky mode.
  dest := @((ylookup[dc_yl]^)[columnofs[dc_x]]);

  // Looks like an attempt at dithering,
  //  using the colormap #6 (of 0-31, a bit
  //  brighter than average).
  for i := 0 to count do
  begin
    // Lookup framebuffer, and retrieve
    //  a pixel that is either one column
    //  left or right of the current one.
    // Add index from colormap to index.
    dest[0] := colormaps[6 * 256 + dest[sfuzzoffset[fuzzpos]]];

    // Clamp table lookup index.
    inc(fuzzpos);
    if fuzzpos = FUZZTABLE then
      fuzzpos := 0;

    dest := @dest[SCREENWIDTH];

  end;
end;

//==============================================================================
//
// R_DrawFuzzColumn32
//
//==============================================================================
procedure R_DrawFuzzColumn32;
var
  count: integer;
  i: integer;
  destl: PLongWordArray;
begin
  // Adjust borders. Low...
  if dc_yl = 0 then
    dc_yl := 1;

  // .. and high.
  if dc_yh = viewheight - 1 then
    dc_yh := viewheight - 2;

  count := dc_yh - dc_yl;

  // Zero length.
  if count < 0 then
    exit;

  // Does not work with blocky mode.
  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);

  // Looks like an attempt at dithering,
  //  using the colormap #6 (of 0-31, a bit
  //  brighter than average).
  for i := 0 to count do
  begin
    // Lookup framebuffer, and retrieve
    //  a pixel that is either one column
    //  left or right of the current one.
    // Add index from colormap to index.
    destl[0] := R_ColorLight(destl[sfuzzoffset[fuzzpos]], $C000);

    // Clamp table lookup index.
    inc(fuzzpos);
    if fuzzpos = FUZZTABLE then
      fuzzpos := 0;

    destl := @destl[SCREENWIDTH];

  end;
end;

//==============================================================================
//
// R_DrawFuzzColumnHi
//
//==============================================================================
procedure R_DrawFuzzColumnHi;
var
  count: integer;
  i: integer;
  destl: PLongWord;
  swidth: integer;

  r1, g1, b1: byte;
  c, r, g, b: LongWord;
begin
  // Adjust borders. Low...
  if dc_yl = 0 then
    dc_yl := 1;

  // .. and high.
  if dc_yh = viewheight - 1 then
    dc_yh := viewheight - 2;

  count := dc_yh - dc_yl;

  // Zero length.
  if count < 0 then
    exit;

  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);

  swidth := SCREENWIDTH32PITCH;
  for i := 0 to count do
  begin
    c := destl^;
    {$I R_FuzzLight.inc}

    destl := PLongWord(integer(destl) + swidth);

  end;
end;

//==============================================================================
//
// R_InitFuzzTable
//
//==============================================================================
procedure R_InitFuzzTable;
var
  i: integer;
begin
  for i := 0 to FUZZTABLE - 1 do
    sfuzzoffset[i] := fuzzoffset[i] * SCREENWIDTH;
end;

end.
