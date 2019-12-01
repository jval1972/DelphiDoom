//------------------------------------------------------------------------------
//
//  DelphiDoom: A modified and improved DOOM engine for Windows
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 2004-2018 by Jim Valavanis
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
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit r_patch;

interface

uses
  d_delphi;

procedure R_InitFixedColumn;

function R_GetFixedColumn(const src: PByteArray; const tex, col: integer): PByteArray;

procedure R_EnableFixedColumn;

procedure R_DisableFixedColumn;

implementation

uses
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

procedure R_InitFixedColumn;
begin
  ZeroMemory(@fixedcolumns, SizeOf(fixedcolumns));
end;

// JVAL: Cantor pairing function (https://en.wikipedia.org/wiki/Pairing_function#Cantor_pairing_function)
function R_GetFixedColumnHash(const tex, col: integer): LongWord;
var
  l: LongWord;
begin
  result := tex;
  exit;
  l := LongWord(tex + 1) + LongWord(col);
  result := ((l * (l + 1)) div 2) and FIXEDCOLUMNMASK;
end;

function R_GetFixedColumn(const src: PByteArray; const tex, col: integer): PByteArray;
var
  hash: LongWord;
  h1, h2: integer;
  item: Pfixedcolumnitem_t;

  procedure _generatedata;
  var
    i: integer;
  begin
    item.data := Z_Malloc(h2, PU_LEVEL, @item.data);
    memcpy(item.data, src, h1);
    for i := h1 to h2 - 1 do
      item.data[i] := item.data[i - h1];
  end;

begin
  if not fix_col_enabled then
  begin
    result := src;
    exit;
  end;

  h1 := textures[tex].height;
  if h1 mod 128 = 0 then
  begin
    result := src;
    exit;
  end;

  h2 := h1 or 127 + 1;
  hash := R_GetFixedColumnHash(tex, col);
  item := fixedcolumns[hash];
  while item <> nil do
  begin
    if (item.tex = tex) and (item.column = col) and (item.size = h2) then
    begin
      if item.data = nil then
        _generatedata;
      result := item.data;
      exit;
    end;
    item := item.next;
  end;

  item := Z_Malloc(SizeOf(fixedcolumnitem_t), PU_LEVEL, nil);
  item.tex := tex;
  item.column := col;
  item.size := h2;

  _generatedata;

  item.next := fixedcolumns[hash];
  fixedcolumns[hash] := item;

  result := item.data;
end;

procedure R_EnableFixedColumn;
begin
  fix_col_enabled := true;
end;

procedure R_DisableFixedColumn;
begin
  fix_col_enabled := false;
end;

end.
