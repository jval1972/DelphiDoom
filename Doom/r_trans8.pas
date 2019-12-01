//------------------------------------------------------------------------------
//
//  DelphiDoom: A modified and improved DOOM engine for Windows
//  based on original Linux Doom as published by "id Software"
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
  trans8tables: array[0..NUMTRANS8TABLES] of Ptrans8table_t;
  trans8tablescalced: boolean = false;
  averagetrans8table: Ptrans8table_t = nil;
  curtrans8table: Ptrans8table_t = nil;

function R_GetTransparency8table(const factor: fixed_t = FRACUNIT div 2): Ptrans8table_t;

implementation

uses
  r_hires,
  v_data, v_video,
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

  trans8tablescalced := true;
end;

procedure R_FreeTransparency8Tables;
var
  i: integer;
begin
  for i := 0 to NUMTRANS8TABLES do
    memfree(pointer(trans8tables[i]), SizeOf(trans8table_t));

  averagetrans8table := nil;

  trans8tablescalced := false;
end;

function R_GetTransparency8table(const factor: fixed_t = FRACUNIT div 2): Ptrans8table_t;
var
  idx: integer;
begin
  idx := (factor + (FRACUNIT div (NUMTRANS8TABLES * 2)) * NUMTRANS8TABLES) div FRACUNIT;
  if idx < 0 then
    idx := 0
  else if idx > NUMTRANS8TABLES then
    idx := NUMTRANS8TABLES;
  result := trans8tables[idx];
end;

end.
