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
//  FLATINFO lump
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit r_flatinfo;

interface

//==============================================================================
//
// R_ParseFlatInfoLumps
//
//==============================================================================
procedure R_ParseFlatInfoLumps;

//==============================================================================
//
// R_FlatSizeFromSize
//
//==============================================================================
function R_FlatSizeFromSize(const size: integer): integer;

type
  dsscale_t = (ds64x64, ds128x128, ds256x256, ds512x512, NUMDSSCALES);

var
  ds_scale: dsscale_t;

const
  FS64x64 = 0;
  FS128x128 = 1;
  FS256x256 = 2;
  FS512x512 = 3;
  FS1024x1024 = 4;
  FS2048x2048 = 5;
  FS4096x4096 = 6;

type
  dsscalesizeitem_t = record
    memsize: integer;
    flatsize: integer;
  end;

const
  dsscalesize: array[FS64x64..FS4096x4096] of dsscalesizeitem_t = (
    (memsize:   64 *   64; flatsize:   64),
    (memsize:  128 *  128; flatsize:  128),
    (memsize:  256 *  256; flatsize:  256),
    (memsize:  512 *  512; flatsize:  512),
    (memsize: 1024 * 1024; flatsize: 1024),
    (memsize: 2048 * 2048; flatsize: 2048),
    (memsize: 4096 * 4096; flatsize: 4096)
  );

type
  flat64x64_t = packed array[0..64 - 1, 0..64 - 1] of byte;
  Pflat64x64_t = ^flat64x64_t;
  flat64x128_t = packed array[0..64 - 1, 0..128 - 1] of byte;
  Pflat64x128_t = ^flat64x128_t;
  flat128x128_t = packed array[0..128 - 1, 0..128 - 1] of byte;
  Pflat128x128_t = ^flat128x128_t;
  flat256x256_t = packed array[0..256 - 1, 0..256 - 1] of byte;
  Pflat256x256_t = ^flat256x256_t;
  flat512x512_t = packed array[0..512 - 1, 0..512 - 1] of byte;
  Pflat512x512_t = ^flat512x512_t;
  flat1024x1024_t = packed array[0..1024 - 1, 0..1024 - 1] of byte;
  Pflat1024x1024_t = ^flat1024x1024_t;
  flat2048x2048_t = packed array[0..2048 - 1, 0..2048 - 1] of byte;
  Pflat2048x2048_t = ^flat2048x2048_t;
  flat4096x4096_t = packed array[0..4096 - 1, 0..4096 - 1] of byte;
  Pflat4096x4096_t = ^flat4096x4096_t;

//==============================================================================
//
// R_ShrinkFlatTo64x64
//
//==============================================================================
function R_ShrinkFlatTo64x64(const fin: Pointer; const sz: Integer; const fout: Pointer): Boolean;

implementation

uses
  d_delphi,
  i_system,
  r_defs,
  r_data,
  sc_engine,
  w_pak,
  w_wad;

//==============================================================================
//
// R_ParseFlatInfoText
//
//==============================================================================
procedure R_ParseFlatInfoText(const in_text: string);
var
  sc: TScriptEngine;
  token: string;
  flat: Pflat_t;
  i: integer;
begin
  sc := TScriptEngine.Create(in_text);
  try
    while sc.GetString do
    begin
      token := strupper(sc._String);

      if token = 'FLAT' then
      begin
        sc.MustGetString;
        token := strupper(sc._String);
        flat := nil;
        for i := numflats - 1 downto 0 do
        begin
          if token = char8tostring(flats[i].name) then
          begin
            flat := flats[i];
            break;
          end;
        end;

        if flat = nil then
        begin
          I_Warning('R_ParseFlatInfo(): Unknown flat "%s"'#13#10, [token]);
          Continue;
        end;

        sc.MustGetString;
        token := strupper(sc._String);

        if token = 'SIZE' then
        begin
          sc.MustGetInteger;
          flat.size := R_FlatSizeFromSize(sc._Integer);
        end
        else
        begin
          sc.UnGet;
          Continue;
        end;

      end
      else
        I_Warning('R_ParseFlatInfo(): Unknown token "%s"'#13#10, [token]);
    end;
  finally
    sc.Free;
  end;
end;

const
  FLATINFOLUMPNAME = 'FLATINFO';

var
  fl_text: string;

//==============================================================================
//
// R_RetrieveFlatInfo
//
//==============================================================================
procedure R_RetrieveFlatInfo(const in_text: string);
begin
  if fl_text = '' then
    fl_text := in_text
  else
    fl_text := fl_text + #13#10 + in_text;
end;

//==============================================================================
//
// R_ParseFlatInfoLumps
//
//==============================================================================
procedure R_ParseFlatInfoLumps;
var
  i: integer;
begin
  fl_text := '';
  for i := 0 to W_NumLumps - 1 do
    if char8tostring(W_GetNameForNum(i)) = FLATINFOLUMPNAME then
      R_RetrieveFlatInfo(W_TextLumpNum(i));

  PAK_StringIterator(FLATINFOLUMPNAME, R_RetrieveFlatInfo);
  PAK_StringIterator(FLATINFOLUMPNAME + '.txt', R_RetrieveFlatInfo);

  R_ParseFlatInfoText(fl_text);
  fl_text := '';
end;

//==============================================================================
//
// R_FlatSizeFromSize
//
//==============================================================================
function R_FlatSizeFromSize(const size: integer): integer;
begin
  if size = 128 then
    Result := FS128x128
  else if size = 256 then
    Result := FS256x256
  else if size = 512 then
    Result := FS512x512
  else if size = 1024 then
    Result := FS1024x1024
  else if size = 2048 then
    Result := FS2048x2048
  else if size = 4096 then
    Result := FS4096x4096
  else
    Result := FS64x64;
end;

//==============================================================================
//
// R_ShrinkFlatTo64x64
//
//==============================================================================
function R_ShrinkFlatTo64x64(const fin: Pointer; const sz: Integer; const fout: Pointer): Boolean;
var
  f64x64: Pflat128x128_t;
  f128x128: Pflat128x128_t;
  f256x256: Pflat256x256_t;
  f512x512: Pflat512x512_t;
  f1024x1024: Pflat1024x1024_t;
  f2048x2048: Pflat2048x2048_t;
  f4096x4096: Pflat4096x4096_t;
  i, j: integer;
begin
  result := false;

  if (sz = 64 * 64) or (sz = 64 * 128) then
  begin
    memcpy(fout, fin, 64 * 64);
    result := true;
    exit;
  end;

  if sz = 128 * 128 then
  begin
    f128x128 := fin;
    f64x64 := fout;
    for i := 0 to 63 do
      for j := 0 to 63 do
        f64x64[i, j] := f128x128[2 * i, 2 * j];
    result := true;
    exit;
  end;

  if sz = 256 * 256 then
  begin
    f256x256 := fin;
    f64x64 := fout;
    for i := 0 to 63 do
      for j := 0 to 63 do
        f64x64[i, j] := f256x256[4 * i, 4 * j];
    result := true;
    exit;
  end;

  if sz = 512 * 512 then
  begin
    f512x512 := fin;
    f64x64 := fout;
    for i := 0 to 63 do
      for j := 0 to 63 do
        f64x64[i, j] := f512x512[8 * i, 8 * j];
    result := true;
    exit;
  end;

  if sz = 1024 * 1024 then
  begin
    f1024x1024 := fin;
    f64x64 := fout;
    for i := 0 to 63 do
      for j := 0 to 63 do
        f64x64[i, j] := f1024x1024[16 * i, 16 * j];
    result := true;
    exit;
  end;

  if sz = 2048 * 2048 then
  begin
    f2048x2048 := fin;
    f64x64 := fout;
    for i := 0 to 63 do
      for j := 0 to 63 do
        f64x64[i, j] := f2048x2048[32 * i, 32 * j];
    result := true;
    exit;
  end;

  if sz = 4096 * 4096 then
  begin
    f4096x4096 := fin;
    f64x64 := fout;
    for i := 0 to 63 do
      for j := 0 to 63 do
        f64x64[i, j] := f4096x4096[64 * i, 64 * j];
    result := true;
    exit;
  end;
end;

end.

