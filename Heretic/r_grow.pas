//------------------------------------------------------------------------------
//
//  DelphiHeretic: A modified and improved Heretic port for Windows
//  based on original Linux Doom as published by "id Software", on
//  Heretic source as published by "Raven" software and DelphiDoom
//  as published by Jim Valavanis.
//  Copyright (C) 2004-2007 by Jim Valavanis
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

unit r_grow;

interface

uses
  r_ccache, r_scache, r_hires;

procedure R_GrowSpan64to128(const p: Pds32cacheinfo_t);

procedure R_GrowSpan64to256(const p: Pds32cacheinfo_t);

procedure R_GrowSpan64to512(const p: Pds32cacheinfo_t);

var
  spanpixels_left: array[0..4095] of integer;
  spanpixels_down: array[0..4095] of integer;
  spanpixels_leftdown: array[0..4095] of integer;

procedure R_InitSpanTables;

implementation

uses
  d_delphi,
  m_fixed,
  r_span;

procedure R_GrowSpan64to128(const p: Pds32cacheinfo_t);
var
  i: integer;
  dest: PLongWord;
  cA, cB, cC, cD: LongWord;
  p1, p2: Pds32_t;
begin
  if p.scale <> ds64x64 then
    exit;
  p1 := R_Get_ds32(p);
  p.scale := ds128x128;
  p2 := R_Get_ds32(p);
  dest := @p2[0];
  for i := 0 to 4095 do
  begin
    cA := p1[i];
    cB := p1[spanpixels_left[i]];
    cC := p1[spanpixels_down[i]];
    cD := p1[spanpixels_leftdown[i]];
    dest^ := cA;
    inc(dest);
    dest^ := R_ColorMidAverage(cA, cB);
    inc(dest, 127);
    dest^ := R_ColorMidAverage(cA, cC);
    cA := dest^;
    inc(dest);
    dest^ := R_ColorMidAverage(cA, R_ColorMidAverage(cB, cD));
    if i and 63 = 63 then
      inc(dest)
    else
      dec(dest, 127);
  end;
end;

procedure R_GrowSpan64to256(const p: Pds32cacheinfo_t);
var
  i: integer;
  dest: PLongWord;
  cA, cB, cC, cD: LongWord;
  cx, cy: LongWord;
  p1, p2: Pds32_t;
  xmod, ymod: integer;
  spot, lspot: integer;
  xfrac, yfrac: integer;
begin
  if p.scale <> ds64x64 then
    exit;
  p1 := R_Get_ds32(p);
  p.scale := ds256x256;
  p2 := R_Get_ds32(p);
  dest := @p2[0];
  lspot := 0;
  cA := p1[0];
  cB := p1[spanpixels_left[0]];
  cC := p1[spanpixels_down[0]];
  cD := p1[spanpixels_leftdown[0]];
  for i := 0 to $FFFF do
  begin
    xfrac := i and 255;
    yfrac := i shr 8;
    spot := xfrac shr 2 + (yfrac shr 2) * 64;
    if spot <> lspot then
    begin
      cA := p1[spot];
      cB := p1[spanpixels_left[spot]];
      cC := p1[spanpixels_down[spot]];
      cD := p1[spanpixels_leftdown[spot]];
      lspot := spot;
    end;
    xmod := (i and 3) * $4000;
    ymod := (yfrac) and 3 * $4000;

    cx := R_ColorAverage(cA, cB, xmod);
    cy := R_ColorAverage(cC, cD, xmod);

    dest^ := R_ColorAverage(cx, cy, ymod);
    inc(dest);
  end;
end;

//
// JVAL
//
// For extreme flat filtering, slow
//
procedure R_GrowSpan64to512(const p: Pds32cacheinfo_t);
var
  i: integer;
  dest: PLongWord;
  cA, cB, cC, cD: LongWord;
  cx, cy: LongWord;
  p1, p2: Pds32_t;
  xmod, ymod: integer;
  spot, lspot: integer;
  xfrac, yfrac: integer;
begin
  if p.scale <> ds64x64 then
    exit;
  p1 := R_Get_ds32(p);
  p.scale := ds512x512;
  p2 := R_Get_ds32(p);
  dest := @p2[0];
  lspot := 0;
  cA := p1[0];
  cB := p1[spanpixels_left[0]];
  cC := p1[spanpixels_down[0]];
  cD := p1[spanpixels_leftdown[0]];
  for i := 0 to $3FFFF do
  begin
    xfrac := i and 511;
    yfrac := i shr 9;
    spot := xfrac shr 3 + (yfrac shr 3) * 64;
    if spot <> lspot then
    begin
      cA := p1[spot];
      cB := p1[spanpixels_left[spot]];
      cC := p1[spanpixels_down[spot]];
      cD := p1[spanpixels_leftdown[spot]];
      lspot := spot;
    end;
    xmod := (i and 7) * $2000;
    ymod := (yfrac) and 7 * $2000;

    cx := R_ColorAverage(cA, cB, xmod);
    cy := R_ColorAverage(cC, cD, xmod);

    dest^ := R_ColorAverage(cx, cy, ymod);
    inc(dest);
  end;
end;

//
// R_ExpandColumn
// Asuming factor < DC_HIRESFACTOR
procedure R_ExpandColumn(const dc32: Pdc32_t; const factor: integer);
var
  i, j: integer;
begin
  for i := 128 downto 0 do
  begin
    for j := 0 to factor - 1 do
    begin
      dc32[i * factor + j] := R_ColorAverage(dc32[i and 127], dc32[(i + 1) and 127], j * FRACUNIT div factor);
    end;
  end;
end;

//
// R_InitSpanTables
//
procedure R_InitSpanTables;
var
  i: integer;
begin
  for i := 0 to 4095 do
    if (i + 1) mod 64 = 0 then
      spanpixels_left[i] := (i - 63) and 4095
    else
      spanpixels_left[i] := (i + 1) and 4095;

  for i := 0 to 4095 do
    spanpixels_down[i] := (i + 64) and 4095;

  for i := 0 to 4095 do
    if (i + 65) mod 64 = 0 then
      spanpixels_leftdown[i] := (i + 1) and 4095
    else
      spanpixels_leftdown[i] := (i + 65) and 4095;
end;

end.
