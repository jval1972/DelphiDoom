//------------------------------------------------------------------------------
//
//  DelphiDoom: A modified and improved DOOM engine for Windows
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

procedure R_ParseFlatInfoLumps;

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

implementation

uses
  d_delphi,
  i_system,
  r_defs,
  r_data,
  sc_engine,
  w_pak,
  w_wad;

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

procedure R_RetrieveFlatInfo(const in_text: string);
begin
  if fl_text = '' then
    fl_text := in_text
  else
    fl_text := fl_text + #13#10 + in_text;
end;

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

end.

