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
//  Palette generator
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit v_palettes;

interface

uses
  d_delphi,
  v_video;

//==============================================================================
//
// V_CreateDoomPalette
//
//==============================================================================
procedure V_CreateDoomPalette(const inppal: PByteArray; const outpal: PByteArray;
  const colormap: PByteArray; const fogmap: PByteArray);

//==============================================================================
//
// V_CreateTranslation
//
// From palette frompal to palette to pal create translation table
// All arrays must be allocated in memory before calling it
//
//==============================================================================
procedure V_CreateTranslation(const frompal, topal: PByteArray; const trans: PByteArray);

//==============================================================================
//
// V_FixBufferPalette
//
//==============================================================================
procedure V_FixBufferPalette(const buf: PByteArray; const x1, x2: integer);

//==============================================================================
//
// V_AutoGeneratePalettes
//
//==============================================================================
procedure V_AutoGeneratePalettes(const base: string);

implementation

uses
  i_tmp,
  w_wad,
  w_wadwriter,
  z_zone;

//==============================================================================
//
// V_ColorShiftPalette
//
//==============================================================================
procedure V_ColorShiftPalette(const inpal: PByteArray; const outpal: PByteArray;
  const r, g, b: integer; const shift: integer; const steps: integer);
var
  i: integer;
  dr, dg, db: integer;
  in_p, out_p: PByteArray;
begin
  in_p := inpal;
  out_p := outpal;

  for i := 0 to 255 do
  begin
    dr := r - in_p[0];
    dg := g - in_p[1];
    db := b - in_p[2];

    out_p[0] := in_p[0] + (dr * shift) div steps;
    out_p[1] := in_p[1] + (dg * shift) div steps;
    out_p[2] := in_p[2] + (db * shift) div steps;

    in_p := @in_p[3];
    out_p := @out_p[3];
  end;
end;

//==============================================================================
//
// V_CopyPalette
//
//==============================================================================
procedure V_CopyPalette(const inppal, outpal: PByteArray);
var
  i: integer;
begin
  for i := 0 to 767 do
    outpal[i] := inppal[i];
end;

//==============================================================================
//
// V_BestColor
//
//==============================================================================
function V_BestColor(const r, g, b: byte; const palette: PByteArray; const rangel, rangeh: integer): byte;
var
  i: integer;
  dr, dg, db: integer;
  bestdistortion, distortion: integer;
  bestcolor: integer;
  pal: PByteArray;
begin
//
// let any color go to 0 as a last resort
//
  bestdistortion := (r * r + g * g + b * b ) * 2;
  bestcolor := 0;

  pal := @palette[rangel * 3];
  for i := rangel to rangeh do
  begin
    dr := r - pal[0];
    dg := g - pal[1];
    db := b - pal[2];
    pal := @pal[3];
    distortion := dr * dr + dg * dg + db * db;
    if distortion < bestdistortion then
    begin
      if distortion = 0 then
      begin
        result := i;  // perfect match
        exit;
      end;

      bestdistortion := distortion;
      bestcolor := i;
    end;
  end;

  result := bestcolor;
end;

//==============================================================================
//
// V_CreateDoomPalette
//
//==============================================================================
procedure V_CreateDoomPalette(const inppal: PByteArray; const outpal: PByteArray;
  const colormap: PByteArray; const fogmap: PByteArray);
const
  NUMLIGHTS = 32;
var
  lightpalette: packed array[0..NUMLIGHTS + 1, 0..255] of byte;
  i, l, c: integer;
  red, green, blue: integer;
  palsrc: PByte;
  gray: double;
  mx: integer;
begin
  mx := inppal[0];
  for i := 1 to 767 do
    if inppal[i] > mx then
      mx := inppal[i];

  if mx < 64 then
    for i := 0 to 767 do
      inppal[i] := 4 * inppal[i];

  V_CopyPalette(inppal, outpal);

  for i := 1 to 8 do
    V_ColorShiftPalette(inppal, @outpal[768 * i], 255, 0, 0, i, 9);

  for i := 1 to 4 do
    V_ColorShiftPalette(inppal, @outpal[768 * (i + 8)], 215, 186, 69, i, 8);

  {$IFNDEF HEXEN}
  V_ColorShiftPalette(inppal, @outpal[768 * 13], 0, 256, 0, 1, 8);
  {$ELSE}
  for i := 1 to 8 do
    V_ColorShiftPalette(inppal, @outpal[768 * (12 + i)], 44, 92, 36, i, 10);

  V_ColorShiftPalette(inppal, @outpal[768 * 21], 0, 0, 224, 1, 2);

  V_ColorShiftPalette(inppal, @outpal[768 * 22], 130, 130, 130, 1, 2);

  V_ColorShiftPalette(inppal, @outpal[768 * 23], 100, 100, 100, 1, 2);

  V_ColorShiftPalette(inppal, @outpal[768 * 24], 70, 70, 70, 1, 2);

  V_ColorShiftPalette(inppal, @outpal[768 * 25], 150, 110, 0, 1, 2);

  V_ColorShiftPalette(inppal, @outpal[768 * 26], 125, 92, 0, 1, 2);

  V_ColorShiftPalette(inppal, @outpal[768 * 27], 100, 73, 0, 1, 2);
  {$ENDIF}

  if colormap <> nil then
  begin
    for l := 0 to NUMLIGHTS - 1 do
    begin
      palsrc := @inppal[0];
      for c := 0 to 255 do
      begin
        {$IFDEF HEXEN}
        // Hexen palette index 255 - See https://zdoom.org/wiki/Palette#Hexen
        if c = 255 then
        begin
          red := inppal[0];
          green := inppal[1];
          blue := inppal[2];
        end
        else
        {$ENDIF}
        begin
          red := palsrc^; inc(palsrc);
          green := palsrc^; inc(palsrc);
          blue := palsrc^; inc(palsrc);
        end;

        red := (red * (NUMLIGHTS - l) + NUMLIGHTS div 2) div NUMLIGHTS;
        green := (green * (NUMLIGHTS - l) + NUMLIGHTS div 2) div NUMLIGHTS;
        blue := (blue * (NUMLIGHTS - l) + NUMLIGHTS div 2) div NUMLIGHTS;

        lightpalette[l][c] := V_BestColor(red, green, blue, inppal, 0, 255);
      end;
    end;

    palsrc := @inppal[0];
    for c := 0 to 255 do
    begin
      red := palsrc^; inc(palsrc);
      green := palsrc^; inc(palsrc);
      blue := palsrc^; inc(palsrc);

      // https://doomwiki.org/wiki/Carmack%27s_typo
      // Correct Carmack's typo
      gray := red * 0.299 / 256 + green * 0.587 / 265 + blue * 0.114 / 256;
      {$IFNDEF HEXEN}
      gray := 1.0 - gray;
      {$ENDIF}
      lightpalette[NUMLIGHTS][c] := V_BestColor(trunc(gray * 255), trunc(gray * 255), trunc(gray * 255), inppal, 0, 255);
    end;

    for c := 0 to 255 do
      lightpalette[NUMLIGHTS + 1][c] := 0;

    for i := 0 to NUMLIGHTS + 1 do
      for c := 0 to 255 do
        colormap[i * 256 + c] := lightpalette[i][c];
  end;

  if fogmap <> nil then
  begin
    for l := 0 to NUMLIGHTS - 1 do
    begin
      palsrc := @inppal[0];
      for c := 0 to 255 do
      begin
        red := palsrc^; inc(palsrc);
        green := palsrc^; inc(palsrc);
        blue := palsrc^; inc(palsrc);

        red := 255 - red;
        green := 255 - green;
        blue := 255 - blue;
        red := (red * (NUMLIGHTS - l) + NUMLIGHTS div 2) div NUMLIGHTS;
        green := (green * (NUMLIGHTS - l) + NUMLIGHTS div 2) div NUMLIGHTS;
        blue := (blue * (NUMLIGHTS - l) + NUMLIGHTS div 2) div NUMLIGHTS;
        red := 255 - red;
        green := 255 - green;
        blue := 255 - blue;

        lightpalette[l][c] := V_BestColor(red, green, blue, inppal, 0, 255);
      end;
    end;

    palsrc := @inppal[0];
    for c := 0 to 255 do
    begin
      red := palsrc^; inc(palsrc);
      green := palsrc^; inc(palsrc);
      blue := palsrc^; inc(palsrc);

      // https://doomwiki.org/wiki/Carmack%27s_typo
      // Correct Carmack's typo
      gray := red * 0.299 / 256 + green * 0.587 / 265 + blue * 0.114 / 256;
      gray := gray;
      lightpalette[NUMLIGHTS][c] := V_BestColor(trunc(gray * 255), trunc(gray * 255), trunc(gray * 255), inppal, 0, 255);
    end;

    for c := 0 to 255 do
      lightpalette[NUMLIGHTS + 1][c] := 0;

    for i := 0 to NUMLIGHTS + 1 do
      for c := 0 to 255 do
        fogmap[i * 256 + c] := lightpalette[i][c];
  end;
end;

//==============================================================================
// V_CreateTranslation
//
// From palette frompal to palette topal create translation table
// All arrays must be allocated in memory before calling it
//
//==============================================================================
procedure V_CreateTranslation(const frompal, topal: PByteArray; const trans: PByteArray);
var
  i: integer;
  r, g, b: byte;
begin
  for i := 0 to 255 do
  begin
    r := topal[i * 3];
    g := topal[i * 3 + 1];
    b := topal[i * 3 + 2];
    trans[i] := V_BestColor(r, g, b, frompal, 0, 255);
  end;
end;

//==============================================================================
//
// V_FixBufferPalette
//
//==============================================================================
procedure V_FixBufferPalette(const buf: PByteArray; const x1, x2: integer);
var
  i: integer;
  x0: integer;
begin
  x0 := x1;
  if x0 = 0 then
    x0 := 1;
  for i := x0 to x2 do
    if (buf[i] < 16) or (buf[i] > 239) then
      buf[i] := buf[i - 1];
end;

var
  palettesgenerated: boolean = false;

//==============================================================================
//
// V_AutoGeneratePalettes
//
//==============================================================================
procedure V_AutoGeneratePalettes(const base: string);
const
  {$IFDEF HEXEN}
  NUMPALETTES = 28;
  {$ELSE}
  NUMPALETTES = 14;
  {$ENDIF}
var
  wad: TWadWriter;
  fn: string;
  pal: PByteArray;
  playpal: packed array[0..768 * NUMPALETTES - 1] of byte;
  colormap: packed array[0..34 * 256 - 1] of byte;
  {$IFNDEF HEXEN}
  fogmap: packed array[0..34 * 256 - 1] of byte;
  {$ENDIF}
begin
  if palettesgenerated then
    exit;

  if base = '' then
    pal := W_CacheLumpName('PLAYPAL', PU_STATIC)
  else
    pal := W_CacheLumpName(base, PU_STATIC);
  ZeroMemory(@playpal, SizeOf(playpal));
  ZeroMemory(@colormap, SizeOf(colormap));
  {$IFDEF HEXEN}
  V_CreateDoomPalette(pal, @playpal, @colormap, nil);
  {$ELSE}
  ZeroMemory(@fogmap, SizeOf(fogmap));
  V_CreateDoomPalette(pal, @playpal, @colormap, @fogmap);
  {$ENDIF}
  Z_Free(pal);

  fn := I_NewTempFile('auto_palette.wad');

  wad := TWadWriter.Create;
  wad.AddData('PLAYPAL', @playpal, SizeOf(playpal));
  wad.AddData('COLORMAP', @colormap, SizeOf(colormap));
  {$IFNDEF HEXEN}
  wad.AddData('FOGMAP', @fogmap, SizeOf(fogmap));
  {$ENDIF}
  wad.SaveToFile(fn);
  wad.Free;

  W_RuntimeLoad(fn, F_ORIGIN_WAD);
end;

end.

