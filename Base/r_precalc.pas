//------------------------------------------------------------------------------
//
//  DelphiDoom: A modified and improved DOOM engine for Windows
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 2004-2012 by Jim Valavanis
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

unit r_precalc;

//
// JVAL
//  Precalculate as much as possible :)
//

interface

uses
  d_delphi,
  m_fixed;

procedure R_InitPrecalc32;

procedure R_ShutDownPrecalc32;

procedure R_GetPrecalc32Tables(const f: fixed_t; var tr, tg, tb: PIntegerArray);

var
  // Invert colormap precalc
  precal32_ic: array[0..767] of longword;

implementation

var
  precal32_r: array[0..255] of PIntegerArray;
  precal32_g: array[0..255] of PIntegerArray;
  precal32_b: array[0..255] of PIntegerArray;

procedure R_InitPrecalc32;
var
  i, j: integer;
  p: PIntegerArray;
  l: LongWord;
begin
  for i := 0 to 255 do
  begin
    p := malloc(256 * SizeOf(Integer));
    precal32_r[i] := p;
    for j := 0 to 255 do
      p[j] := (i * j * 256) shr FRACBITS;
    p := malloc(256 * SizeOf(Integer));
    precal32_g[i] := p;
    for j := 0 to 255 do
      p[j] := (((i * j * 256) shr FRACBITS) shl 8) and $FF00;
    p := malloc(256 * SizeOf(Integer));
    precal32_b[i] := p;
    for j := 0 to 255 do
      p[j] := (i * j * 256) and $FF0000;
  end;

  for i := 0 to 767 do
  begin
    l := 255 - i div 3;
    precal32_ic[i] := l + l shl 8 + l shl 16;
  end;
end;

procedure R_ShutDownPrecalc32;
var
  i: integer;
begin
  for i := 0 to 255 do
  begin
    memfree(pointer(precal32_r[i]), 256 * SizeOf(Integer));
    memfree(pointer(precal32_g[i]), 256 * SizeOf(Integer));
    memfree(pointer(precal32_b[i]), 256 * SizeOf(Integer));
  end;
end;

procedure R_GetPrecalc32Tables(const f: fixed_t; var tr, tg, tb: PIntegerArray);
var
  lf: Integer;
begin
  lf := f shr 8;
  if lf > 255 then
    lf := 255;
  tr := precal32_r[lf];
  tg := precal32_g[lf];
  tb := precal32_b[lf];
end;

end.
