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
//  Fix tutti frutti bug
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit r_patch;

interface

uses
  d_delphi;

//==============================================================================
//
// R_InitFixedColumn
//
//==============================================================================
procedure R_InitFixedColumn;

//==============================================================================
//
// R_GetFixedColumn
//
//==============================================================================
function R_GetFixedColumn(const src: PByteArray; const tex, col: integer; const multipatch: boolean): PByteArray;

//==============================================================================
//
// R_EnableFixedColumn
//
//==============================================================================
procedure R_EnableFixedColumn;

//==============================================================================
//
// R_DisableFixedColumn
//
//==============================================================================
procedure R_DisableFixedColumn;

implementation

uses
  r_defs,
  r_data,
  z_zone;

const
  FIXEDCOLUMNSIZE = $10000;
  FIXEDCOLUMNMASK = FIXEDCOLUMNSIZE - 1;

type
  Pfixedcolumnitem_t = ^fixedcolumnitem_t;
  fixedcolumnitem_t = record
    tex, column: integer;
    size: integer;
    data: PByteArray;
    next: Pfixedcolumnitem_t;
  end;

var
  fixedcolumns: array[0..FIXEDCOLUMNSIZE - 1] of Pfixedcolumnitem_t;
  fix_col_enabled: boolean = true;

//==============================================================================
//
// R_InitFixedColumn
//
//==============================================================================
procedure R_InitFixedColumn;
begin
  ZeroMemory(@fixedcolumns, SizeOf(fixedcolumns));
end;

//==============================================================================
// R_GetFixedColumnHash
//
// JVAL: Cantor pairing function (https://en.wikipedia.org/wiki/Pairing_function#Cantor_pairing_function)
//
//==============================================================================
function R_GetFixedColumnHash(const tex, col: integer): LongWord;
var
  l: LongWord;
begin
  l := LongWord(tex + 1) + LongWord(col);
  result := ((l * (l + 1)) div 2) and FIXEDCOLUMNMASK;
end;

//==============================================================================
//
// R_GetFixedColumn
//
//==============================================================================
function R_GetFixedColumn(const src: PByteArray; const tex, col: integer; const multipatch: boolean): PByteArray;
var
  hash: LongWord;
  h1, h2: integer;
  item: Pfixedcolumnitem_t;

  procedure _generatedata128;
  var
    i: integer;
  begin
    item.data := Z_Malloc(h2, PU_LEVEL, nil);
    memcpy(item.data, src, h1);
    for i := h1 to h2 - 1 do
      item.data[i] := item.data[i - h1];
  end;

  procedure _generatedataTL;
  var
    i: integer;
    col: Pcolumn_t;
    delta, prevdelta: integer;
    tallpatch: boolean;
    p: integer;
    srcTL: PByteArray;
  begin
    item.data := Z_Malloc(h2, PU_LEVEL, nil);
    col := Pcolumn_t(integer(src) - 3);
    delta := 0;
    tallpatch := false;
    while col.topdelta <> $FF do
    begin
      delta := delta + col.topdelta;
      srcTL := PByteArray(integer(col) + 3);
      for i := 0 to col.length - 1 do
      begin
        p := delta + i;
        if p >= 0 then
          if p < h2 then
            item.data[p] := srcTL[i];
      end;

      if not tallpatch then
      begin
        prevdelta := col.topdelta;
        col := Pcolumn_t(integer(col) + col.length + 4);
        if col.topdelta > prevdelta then
          delta := 0
        else
          tallpatch := true;
      end
      else
        col := Pcolumn_t(integer(col) + col.length + 4);
    end;
  end;

begin
  if not fix_col_enabled then
  begin
    result := src;
    exit;
  end;

  h1 := textures[tex].height;
  if h1 = 128 then
  begin
    result := src;
    exit;
  end;

  if h1 in [2, 4, 8, 16, 32, 64] then
  begin
    h2 := 128;
    hash := R_GetFixedColumnHash(tex, col);
    item := fixedcolumns[hash];
    while item <> nil do
    begin
      if (item.tex = tex) and (item.column = col) and (item.size = h2) then
      begin
        if item.data = nil then
          _generatedata128;
        result := item.data;
        exit;
      end;
      item := item.next;
    end;

    item := Z_Malloc(SizeOf(fixedcolumnitem_t), PU_LEVEL, nil);
    item.tex := tex;
    item.column := col;
    item.size := h2;

    _generatedata128;

    item.next := fixedcolumns[hash];
    fixedcolumns[hash] := item;

    result := item.data;
  end
  else if not multipatch then
  begin
    h2 := h1;
    hash := R_GetFixedColumnHash(tex, col);
    item := fixedcolumns[hash];
    while item <> nil do
    begin
      if (item.tex = tex) and (item.column = col) and (item.size = h2) then
      begin
        if item.data = nil then
          _generatedataTL;
        result := item.data;
        exit;
      end;
      item := item.next;
    end;

    item := Z_Malloc(SizeOf(fixedcolumnitem_t), PU_LEVEL, nil);
    item.tex := tex;
    item.column := col;
    item.size := h2;

    _generatedataTL;

    item.next := fixedcolumns[hash];
    fixedcolumns[hash] := item;

    result := item.data;
  end
  else
    result := src;
end;

//==============================================================================
//
// R_EnableFixedColumn
//
//==============================================================================
procedure R_EnableFixedColumn;
begin
  fix_col_enabled := true;
end;

//==============================================================================
//
// R_DisableFixedColumn
//
//==============================================================================
procedure R_DisableFixedColumn;
begin
  fix_col_enabled := false;
end;

end.
