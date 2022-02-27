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
//  DESCRIPTION:
//   JVAL: Emulates different colormaps in 32 bit color textures.
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit r_diher;

interface

uses
  d_delphi;

const
  DIHERBITS = 5;
  DIHERRANGE = 1 shl DIHERBITS;
  DIHERSHIFT = 8 - DIHERBITS;

type
  dihertable_t = array[0..DIHERRANGE - 1, 0..DIHERRANGE - 1, 0..DIHERRANGE - 1] of LongWord; // diheritem_t;
  Pdihertable_t = ^dihertable_t;

//==============================================================================
//
// R_CreateDiherTable
//
//==============================================================================
procedure R_CreateDiherTable(const dihertable: Pdihertable_t; const pal: PLongWordArray);

{$IFDEF OPENGL}

//==============================================================================
//
// R_GetColormapFogColors
//
//==============================================================================
procedure R_GetColormapFogColors(var r, g, b, dens: float; const pal: PLongWordArray);
{$ENDIF}

//==============================================================================
//
// R_InitNonUniformDiherFactor
//
//==============================================================================
procedure R_InitNonUniformDiherFactor;

var
  diher_precalc: array[0..255] of array[0..255] of byte;

implementation

uses
  v_video;

var
  non_uniform_table: array[0..255] of Double;

//==============================================================================
// R_InitNonUniformDiherFactor
//
// JVAL
// Maps double from [0.0 to 256.0] to 8 bits (1 byte) with non uniform way
//
//==============================================================================
procedure R_InitNonUniformDiherFactor;
const
  BYTEBITS = 6;
var
  d: double;
  x, y: integer;
  b: byte;
  cc: integer;
begin
  non_uniform_table[0] := 0.0;

  b := 1;
  while b < 255 do
  begin
    x := b shr BYTEBITS;
    y := b and (1 shl BYTEBITS - 1);
    d := x + y / (1 shl BYTEBITS);
    if d > 2.0 then
      d := d * d / 2.0;
    if d > 4.0 then
      d := d * d / 4.0;
    if d > 6.0 then
      d := d * d / 6;
    if d > 12.544908 then
      d := d * d / 12.544908;
    non_uniform_table[b] := d;
    inc(b);
  end;
  non_uniform_table[255] := 256.0;

  for x := 0 to 255 do
    for y := 0 to 255 do
    begin
      d := non_uniform_table[x];
      cc := Round(y * d);
      if cc <= 0 then
        b := 0
      else if cc >= 255 then
        b := 255
      else
        b := cc;
      diher_precalc[x, y] := b;
    end;
end;

//==============================================================================
//
// R_DiherFactorToDouble
//
//==============================================================================
function R_DiherFactorToDouble(const f: Byte): Double;
begin
  result := non_uniform_table[f];
end;

//==============================================================================
//
// R_DiherFactorToByte
//
//==============================================================================
function R_DiherFactorToByte(const f: Double): Byte;
var
  l, h, i: integer;
  diff1, diff2: Double;
begin
  if f <= non_uniform_table[0] then
  begin
    result := 0;
    exit;
  end;
  if f >= non_uniform_table[255] then
  begin
    result := 255;
    exit;
  end;

  l := 0;
  h := 255;
  while l <= h do
  begin
    i := (l + h) shr 1;
    if non_uniform_table[i] < f then
    begin
      l := i + 1;
      if non_uniform_table[l] >= f then
      begin
        diff1 := f - non_uniform_table[i];
        diff2 := non_uniform_table[l] - f;
        if diff1 < diff2 then
          result := i
        else
          result := l;
        exit;
      end;
    end
    else
    begin
      if non_uniform_table[i] = f then
      begin
        result := i;
        exit;
      end;
      h := i - 1;
    end;
  end;

  // JVAL: Avoid compiler warning
  result := 0;
end;

//==============================================================================
//
// R_GetDiherFactor
//
//==============================================================================
function R_GetDiherFactor(const before, after: Byte): byte;
var
  b, a: integer;
begin
  if after = 0 then
  begin
    result := 0;
    exit;
  end;

  if before = 0 then
    b := 1
  else
    b := before;
  a := after;

  result := R_DiherFactorToByte(a / b);
end;

//==============================================================================
//
// R_GetDiherFactorDouble
//
//==============================================================================
function R_GetDiherFactorDouble(const before, after: Byte): double;
var
  b, a: integer;
begin
  if after = 0 then
  begin
    result := 0.0;
    exit;
  end;

  if before = 0 then
    b := 1
  else
    b := before;
  a := after;

  result := (a / b);
end;

//==============================================================================
//
// R_CreateDiherTable
//
//==============================================================================
procedure R_CreateDiherTable(const dihertable: Pdihertable_t; const pal: PLongWordArray);
var
  i, j, k: integer;
  ic, jc, kc: LongWord;
  palc: LongWord;
  ib, jb, kb: Byte; // Actual diher table colors
  ip, jp, kp: Byte; // Palette aproximates for diher table colors
begin
  for i := 0 to DIHERRANGE - 1 do // Blue color
  begin
    ic := (i shl DIHERSHIFT) shl 16;
    ib := i shl DIHERSHIFT;
    for j := 0 to DIHERRANGE - 1 do // Green color
    begin
      jc := (j shl DIHERSHIFT) shl 8;
      jb := j shl DIHERSHIFT;
      for k := 0 to DIHERRANGE - 1 do // Red color
      begin
        kc := (k shl DIHERSHIFT);
        kb := k shl DIHERSHIFT;
        palc := pal[V_FindAproxColorIndex(pal, ic + jc + kc)];
        ip := palc shr 16;
        jp := (palc shr 8) and $FF;
        kp := palc and $FF; // JVAL: Just "kp := palc;" should work OK
        dihertable[i, j, k] :=
          diher_precalc[R_GetDiherFactor(kb, kp)][kb] +
          (diher_precalc[R_GetDiherFactor(jb, jp)][jb] shl 8) +
          (diher_precalc[R_GetDiherFactor(ib, ip)][ib] shl 16);
      end;
    end;
  end;
end;

{$IFDEF OPENGL}

//==============================================================================
//
// R_GetColormapFogColors
//
//==============================================================================
procedure R_GetColormapFogColors(var r, g, b, dens: float; const pal: PLongWordArray);
var
  i, j, k: integer;
  ic, jc, kc: LongWord;
  palc: LongWord;
  ib, jb, kb: Byte; // Actual diher table colors
  ip, jp, kp: Byte; // Palette aproximates for diher table colors
  r_factor, g_factor, b_factor: Double;
  fmax: Double;
begin
  r_factor := 0.0;
  g_factor := 0.0;
  b_factor := 0.0;
  for i := 0 to DIHERRANGE - 1 do
  begin
    ic := (i shl DIHERSHIFT) shl 16;
    ib := i shl DIHERSHIFT;
    for j := 0 to DIHERRANGE - 1 do
    begin
      jc := (j shl DIHERSHIFT) shl 8;
      jb := j shl DIHERSHIFT;
      for k := 0 to DIHERRANGE - 1 do
      begin
        kc := (k shl DIHERSHIFT);
        kb := k shl DIHERSHIFT;
        palc := pal[V_FindAproxColorIndex(pal, ic + jc + kc)];
        ip := palc shr 16;
        jp := (palc shr 8) and $FF;
        kp := palc and $FF; // JVAL: Just "kp := palc;" should work OK
        b_factor := b_factor + R_GetDiherFactorDouble(kb, kp);
        g_factor := g_factor + R_GetDiherFactorDouble(jb, jp);
        r_factor := r_factor + R_GetDiherFactorDouble(ib, ip);
      end;
    end;
  end;

  r_factor := (r_factor / (DIHERRANGE * DIHERRANGE * DIHERRANGE));
  g_factor := (g_factor / (DIHERRANGE * DIHERRANGE * DIHERRANGE));
  b_factor := (b_factor / (DIHERRANGE * DIHERRANGE * DIHERRANGE));

  fmax := r_factor + g_factor + b_factor;

  if fmax < 0.0001 then
  begin
    r := 0.0;
    g := 0.0;
    b := 0.0;
    dens := 6.0;
    exit;
  end;

  r_factor := r_factor / fmax;
  g_factor := g_factor / fmax;
  b_factor := b_factor / fmax;

  dens := 5.0;
  if r_factor < 1.0 then
    dens := dens + 2 * (1 - r_factor)
  else
    r_factor := 1.0;
  r := r_factor;

  if g_factor < 1.0 then
    dens := dens + 2 * (1 - g_factor)
  else
    g_factor := 1.0;
  g := g_factor;

  if b_factor < 1.0 then
    dens := dens + 2 * (1 - b_factor)
  else
    b_factor := 1.0;
  b := b_factor;

end;
{$ENDIF}

end.
