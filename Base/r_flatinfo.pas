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
// DESCRIPTION:
//  FLATINFO lump
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit r_flatinfo;

interface

procedure R_ParseFlatInfoLumps;

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
        for i := 0 to numflats - 1 do
        begin
          if token = char8tostring(flats[i].name) then
          begin
            flat := @flats[i];
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
          flat.size := sc._Integer;
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

end.

