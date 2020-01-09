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

unit r_trans8;

// Description
// Transparency for 8 bit color modes

interface

uses
  d_delphi,
  m_fixed;

procedure R_InitTransparency8Tables;

procedure R_FreeTransparency8Tables;

type
  trans8table_t = packed array[0..$FFFF] of byte;
  Ptrans8table_t = ^trans8table_t;

const
  NUMTRANS8TABLES = 8; // Actual tables are NUMTRANS8TABLES + 1

var
  trans8tablescalced: boolean = false;
  trans8tables: array[0..NUMTRANS8TABLES] of Ptrans8table_t;
  additive8tables: array[0..NUMTRANS8TABLES] of Ptrans8table_t;
  subtractive8tables: array[0..NUMTRANS8TABLES] of Ptrans8table_t;
  averagetrans8table: Ptrans8table_t = nil;
  curtrans8table: Ptrans8table_t = nil;
  curadd8table: Ptrans8table_t = nil;
  cursubtract8table: Ptrans8table_t = nil;
  colorlighttrans8table: Ptrans8table_t = nil;
  colorlighttrans8table256: Ptrans8table_t = nil;

function R_GetTransparency8table(const factor: fixed_t = FRACUNIT div 2): Ptrans8table_t;

function R_GetAdditive8table(const factor: fixed_t = FRACUNIT div 2): Ptrans8table_t;

function R_GetSubtractive8table(const factor: fixed_t = FRACUNIT div 2): Ptrans8table_t;

function R_FastApproxColorIndex(const c: LongWord): byte; overload;

function R_FastApproxColorIndex(const r, g, b: byte): byte; overload;

const
  FASTTABLESHIFT = 4;
  FASTTABLEBIT = 1 shl FASTTABLESHIFT;
  FASTTABLECHANNEL = 256 div FASTTABLEBIT;
  FASTTABLESIZE = FASTTABLECHANNEL * FASTTABLECHANNEL * FASTTABLECHANNEL;

var
  approxcolorindexarray: array[0..FASTTABLESIZE - 1] of byte;

implementation

uses
  r_hires,
  v_data,
  v_video,
  z_zone;

procedure R_InitTransparency8Tables;
var
  dest: PLongWord;
  src: PByteArray;
  pal: PByteArray; // Palette lump data
  palL: array[0..255] of LongWord; // Longword palette indexes
  i, j, k: integer;
  factor: fixed_t;
  c: LongWord;
  c1: LongWord;
  ptrans8: PByte;
  r, g, b: LongWord;
begin
  if trans8tablescalced then
    exit;

// Expand WAD palette to longword values
  dest := @palL[0];
  pal := V_ReadPalette(PU_STATIC);
  src := pal;
  while integer(src) < integer(@pal[256 * 3]) do
  begin
    dest^ := (LongWord(src[0]) shl 16) or
             (LongWord(src[1]) shl 8) or
             (LongWord(src[2]));
    inc(dest);
    src := PByteArray(integer(src) + 3);
  end;
  Z_ChangeTag(pal, PU_CACHE);

  for i := 0 to NUMTRANS8TABLES do
  begin
    trans8tables[i] := malloc(SizeOf(trans8table_t));
    ptrans8 := PByte(trans8tables[i]);
    factor := i * (FRACUNIT div NUMTRANS8TABLES);
    for j := 0 to 255 do
    begin
      c1 := palL[j];
      for k := 0 to 255 do
      begin
        c := R_ColorAverage(palL[k], c1, factor);
        ptrans8^ := V_FindAproxColorIndex(@palL, c) and $FF;
        inc(ptrans8);
      end;
    end;
  end;

  averagetrans8table := trans8tables[NUMTRANS8TABLES div 2];

  ptrans8 := @approxcolorindexarray[0];
  for r := 0 to FASTTABLECHANNEL - 1 do
    for g := 0 to FASTTABLECHANNEL - 1 do
      for b := 0 to FASTTABLECHANNEL - 1 do
      begin
        ptrans8^ := V_FindAproxColorIndex(@palL,
                          r shl (16 + FASTTABLESHIFT) + g shl (8 + FASTTABLESHIFT) + b shl FASTTABLESHIFT {+
                          ((1 shl FASTTABLESHIFT) shr 1) shl 16 +
                          ((1 shl FASTTABLESHIFT) shr 1) shl 8 +
                          ((1 shl FASTTABLESHIFT) shr 1)}
                    ) and $FF;
        inc(ptrans8);
      end;

  for i := 0 to NUMTRANS8TABLES do
  begin
    additive8tables[i] := malloc(SizeOf(trans8table_t));
    ptrans8 := PByte(additive8tables[i]);
    for j := 0 to 255 do
    begin
      c1 := palL[j];
      r := ((c1 and $FF) * i * (FRACUNIT div NUMTRANS8TABLES)) div FRACUNIT;
      if r > 255 then
        r := 255;
      g := (((c1 shr 8) and $FF) * i * (FRACUNIT div NUMTRANS8TABLES)) div FRACUNIT;
      if g > 255 then
        g := 255;
      b := (((c1 shr 16) and $FF) * i * (FRACUNIT div NUMTRANS8TABLES)) div FRACUNIT;
      if b > 255 then
        b := 255;
      c1 := r + g shl 8 + b shl 16;
      for k := 0 to 255 do
      begin
        c := R_ColorAdd(palL[k], c1);
        if (i = 0) or (i = 8) then
          ptrans8^ := V_FindAproxColorIndex(@palL, c) and $FF
        else
          ptrans8^ := V_FindAproxColorIndexExcluding(@palL, c, 0, 255, k) and $FF;
        inc(ptrans8);
      end;
    end;
  end;

  for i := 0 to NUMTRANS8TABLES do
  begin
    subtractive8tables[i] := malloc(SizeOf(trans8table_t));
    ptrans8 := PByte(subtractive8tables[i]);
    for j := 0 to 255 do
    begin
      c1 := palL[j];
      r := ((c1 and $FF) * i * (FRACUNIT div NUMTRANS8TABLES)) div FRACUNIT;
      if r > 255 then
        r := 255;
      g := (((c1 shr 8) and $FF) * i * (FRACUNIT div NUMTRANS8TABLES)) div FRACUNIT;
      if g > 255 then
        g := 255;
      b := (((c1 shr 16) and $FF) * i * (FRACUNIT div NUMTRANS8TABLES)) div FRACUNIT;
      if b > 255 then
        b := 255;
      c1 := r + g shl 8 + b shl 16;
      for k := 0 to 255 do
      begin
        c := R_ColorSubtract(palL[k], c1);
        if (i = 0) or (i = 8) then
          ptrans8^ := V_FindAproxColorIndex(@palL, c) and $FF
        else
          ptrans8^ := V_FindAproxColorIndexExcluding(@palL, c, 0, 255, k) and $FF;
        inc(ptrans8);
      end;
    end;
  end;

  colorlighttrans8table := malloc(SizeOf(trans8table_t));
  ptrans8 := @colorlighttrans8table[0];
  for j := 0 to 255 do
  begin
    c1 := palL[j];
    r := (c1 shr 16) and $ff;
    g := (c1 shr 8) and $ff;
    b := c1 and $ff;
    for k := 0 to 255 do
    begin
      c := R_ColorLightAdd(palL[k], r, g, b);
      ptrans8^ := V_FindAproxColorIndex(@palL, c) and $FF;
      inc(ptrans8);
    end;
  end;

  colorlighttrans8table256 := malloc(SizeOf(trans8table_t));
  ptrans8 := @colorlighttrans8table256[0];
  for j := 0 to 255 do
  begin
    for k := 0 to 255 do
    begin
      c1 := palL[k];
      r := (c1 shr 16) and $ff;
      g := (c1 shr 8) and $ff;
      b := c1 and $ff;
      c := R_ColorLightAdd(palL[j], r, g, b);
      ptrans8^ := V_FindAproxColorIndex(@palL, c) and $FF;
      inc(ptrans8);
    end;
  end;

  trans8tablescalced := true;
end;

procedure R_FreeTransparency8Tables;
var
  i: integer;
begin
  for i := 0 to NUMTRANS8TABLES do
  begin
    memfree(pointer(trans8tables[i]), SizeOf(trans8table_t));
    memfree(pointer(additive8tables[i]), SizeOf(trans8table_t));
    memfree(pointer(subtractive8tables[i]), SizeOf(trans8table_t));
  end;

  memfree(pointer(colorlighttrans8table), SizeOf(trans8table_t));
  memfree(pointer(colorlighttrans8table256), SizeOf(trans8table_t));

  averagetrans8table := nil;

  trans8tablescalced := false;

end;

function R_GetTransparency8table(const factor: fixed_t = FRACUNIT div 2): Ptrans8table_t;
var
  idx: integer;
begin
  idx := (factor * NUMTRANS8TABLES) div FRACUNIT;
  if idx < 0 then
    result := trans8tables[0]
  else if idx > NUMTRANS8TABLES then
    result := trans8tables[NUMTRANS8TABLES]
  else
    result := trans8tables[idx];
end;

function R_GetAdditive8table(const factor: fixed_t = FRACUNIT div 2): Ptrans8table_t;
var
  idx: integer;
begin
  idx := (factor * NUMTRANS8TABLES) div FRACUNIT;
  if idx < 0 then
    result := additive8tables[0]
  else if idx > NUMTRANS8TABLES then
    result := additive8tables[NUMTRANS8TABLES]
  else
    result := additive8tables[idx];
end;

function R_GetSubtractive8table(const factor: fixed_t = FRACUNIT div 2): Ptrans8table_t;
var
  idx: integer;
begin
  idx := (factor * NUMTRANS8TABLES) div FRACUNIT;
  if idx < 0 then
    result := subtractive8tables[0]
  else if idx > NUMTRANS8TABLES then
    result := subtractive8tables[NUMTRANS8TABLES]
  else
    result := subtractive8tables[idx];
end;

function R_FastApproxColorIndex(const c: LongWord): byte; overload;
var
  r, g, b: LongWord;
begin
  b := (c shr 4) and $F;
  g := (c shr 12) and $F;
  r := (c shr 20) and $F;
  result := approxcolorindexarray[r shl 8 + g shl 4 + b];
end;

function R_FastApproxColorIndex(const r, g, b: byte): byte; overload;
var
  r1, g1, b1: LongWord;
begin
  b1 := b shr 4;
  g1 := g shr 4;
  r1 := r shr 4;
  result := approxcolorindexarray[r1 shl 8 + g1 shl 4 + b1];
end;

end.
