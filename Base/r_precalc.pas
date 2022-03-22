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

unit r_precalc;

//
// JVAL
//  Precalculate as much as possible :)
//

interface

uses
  d_delphi,
  m_fixed;

//==============================================================================
//
// R_InitPrecalc
//
//==============================================================================
procedure R_InitPrecalc;

//==============================================================================
//
// R_ShutDownPrecalc
//
//==============================================================================
procedure R_ShutDownPrecalc;

//==============================================================================
//
// R_GetPrecalc32Tables
//
//==============================================================================
procedure R_GetPrecalc32Tables(const f: fixed_t; var tr, tg, tb: PIntegerArray; const fog: boolean);  // JVAL: Mars fog sectors

{$IFDEF HEXEN}

//==============================================================================
//
// R_GetFogPrecalc32Tables
//
//==============================================================================
procedure R_GetFogPrecalc32Tables(const f: fixed_t; var tr, tg, tb: PIntegerArray);
{$ENDIF}

var
  // Invert colormap precalc
  precal32_ic: array[0..767] of LongWord;
  precal8_tolong: array[0..255] of LongWord;
  precal8_toword: array[0..255] of LongWord;
  precal_light: array[0..255] of byte;
  precalc_fog_r: array[0..255] of PIntegerArray;
  precalc_fog_g: array[0..255] of PIntegerArray;
  precalc_fog_b: array[0..255] of PIntegerArray;
  average_byte: array[0..511] of Byte;
  average_r: array[0..511] of LongWord;
  average_g: array[0..511] of LongWord;
  average_b: array[0..511] of LongWord;
  add32_c: array[0..511] of Byte;

implementation

var
  precalc32_r: array[0..255] of PIntegerArray;
  precalc32_g: array[0..255] of PIntegerArray;
  precalc32_b: array[0..255] of PIntegerArray;

//==============================================================================
//
// R_InitPrecalc
//
//==============================================================================
procedure R_InitPrecalc;
var
  i, j: integer;
  p: PIntegerArray;
  l: LongWord;
  buf2: twobytes_t;
  buf4: fourbytes_t;
  f1, f2: integer;
  p2: PIntegerArray;
begin
  for i := 0 to 255 do
  begin
    p := malloc(256 * SizeOf(Integer));
    precalc32_r[i] := p;
    for j := 0 to 255 do
      p[j] := (i * j * 256) shr FRACBITS;
    p := malloc(256 * SizeOf(Integer));
    precalc32_g[i] := p;
    for j := 0 to 255 do
      p[j] := (((i * j * 256) shr FRACBITS) shl 8) and $FF00;
    p := malloc(256 * SizeOf(Integer));
    precalc32_b[i] := p;
    for j := 0 to 255 do
      p[j] := (i * j * 256) and $FF0000;

    buf4.byte1 := i;
    buf4.byte2 := i;
    buf4.byte3 := i;
    buf4.byte4 := i;
    precal8_tolong[i] := PLongWord(@buf4)^;

    buf2.byte1 := i;
    buf2.byte2 := i;
    precal8_toword[i] := PWord(@buf2)^;

    f1 := i;
    f2 := 256 - f1;

    p := malloc(256 * SizeOf(Integer));
    precalc_fog_r[i] := p;
    for j := 0 to 255 do
      p[j] := 255 - (i * (255 - j)) shr 8;

    p2 := precalc32_r[i];
    for j := 0 to 255 do
      p[j] := (p[j] * f2 + p2[j] * f1) shr 8;

    p := malloc(256 * SizeOf(Integer));
    precalc_fog_g[i] := p;
    for j := 0 to 255 do
      p[j] := precalc_fog_r[i][j] shl 8;
    p := malloc(256 * SizeOf(Integer));
    precalc_fog_b[i] := p;
    for j := 0 to 255 do
      p[j] := precalc_fog_r[i][j] shl 16;
  end;

  for i := 0 to 767 do
  begin
    l := 255 - i div 6;
    precal32_ic[i] := l + l shl 8 + l shl 16;
  end;

  for i := 0 to 511 do
  begin
    average_byte[i] := i div 2;
    average_r[i] := i div 2;
    average_g[i] := (i div 2) shl 8;
    average_b[i] := (i div 2) shl 16;
    if i > 255 then
      add32_c[i] := 255
    else
      add32_c[i] := i;
  end;

end;

//==============================================================================
//
// R_ShutDownPrecalc
//
//==============================================================================
procedure R_ShutDownPrecalc;
var
  i: integer;
begin
  for i := 0 to 255 do
  begin
    memfree(pointer(precalc32_r[i]), 256 * SizeOf(Integer));
    memfree(pointer(precalc32_g[i]), 256 * SizeOf(Integer));
    memfree(pointer(precalc32_b[i]), 256 * SizeOf(Integer));
    memfree(pointer(precalc_fog_r[i]), 256 * SizeOf(Integer));
    memfree(pointer(precalc_fog_g[i]), 256 * SizeOf(Integer));
    memfree(pointer(precalc_fog_b[i]), 256 * SizeOf(Integer));
  end;
end;

//==============================================================================
//
// R_GetPrecalc32Tables
//
//==============================================================================
procedure R_GetPrecalc32Tables(const f: fixed_t; var tr, tg, tb: PIntegerArray; const fog: boolean);  // JVAL: Mars fog sectors
var
  lf: Integer;
begin
  lf := f shr 8;
  if lf > 255 then
    lf := 255;
  if fog then
  begin
    tr := precalc_fog_r[lf];
    tg := precalc_fog_g[lf];
    tb := precalc_fog_b[lf];
  end
  else
  begin
    tr := precalc32_r[lf];
    tg := precalc32_g[lf];
    tb := precalc32_b[lf];
  end;
end;

{$IFDEF HEXEN}

//==============================================================================
//
// R_GetFogPrecalc32Tables
//
//==============================================================================
procedure R_GetFogPrecalc32Tables(const f: fixed_t; var tr, tg, tb: PIntegerArray);
var
  lf: Integer;
begin
  lf := f shr 8;
  if lf > 255 then
    lf := 255;
  tr := precalc_fog_r[lf];
  tg := precalc_fog_g[lf];
  tb := precalc_fog_b[lf];
end;
{$ENDIF}

end.
