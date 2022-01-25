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
//
// DESCRIPTION:
//  Parsing utilities
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit sc_utils;

interface

function SC_Preprocess(const inp_text: string; const addcomment: boolean): string;

implementation

uses
  d_delphi,
  i_system,
  {$IFDEF DOOM}
  g_game,
  {$ENDIF}
  sc_defines,
  sc_engine,
  w_pak,
  w_wad;

function SC_Preprocess(const inp_text: string; const addcomment: boolean): string;
const
  MAXINCLUDEDEPTH = 32;
var
  includes: TDStringList;
  depth: integer;
  i: integer;
  decoded_text: string;
  ppc: TDefinesPreprocessor;

  function SC_DoPreprocess(const in_text: string): string;
  var
    lst_in: TDStringList;
    i, j, p, idx: integer;
    str, str1: string;
    s1, s2: string;
    str_incl: string;
    do_all: boolean;
    do_one: boolean;
    do_wad: boolean;
    lump: integer;
  begin
    inc(depth);
    if depth >= MAXINCLUDEDEPTH then
    begin
      I_Warning('SC_Preprocess(): Include file maximum depth(%d) exceeded'#13#10, [depth]);
      result := in_text;
      exit;
    end;

    lst_in := TDStringList.Create;
    lst_in.Text := in_text;

    result := '';
    for i := 0 to lst_in.Count - 1 do
    begin
      str1 := lst_in.Strings[i];
      p := Pos('//', str1);
      if p > 0 then
        str :=  Copy(str1, 1, p - 1)
      else
        str := str1;
      do_one := (Pos('#INCLUDE ', strupper(strtrim(str))) = 1) or (Pos('{$INCLUDE ', strupper(strtrim(str))) = 1) or (Pos('{$INCLUDE} ', strupper(strtrim(str))) = 1);
      do_all := (Pos('#INCLUDE_ALL ', strupper(strtrim(str))) = 1) or (Pos('{$INCLUDE_ALL ', strupper(strtrim(str))) = 1) or (Pos('{$INCLUDE_ALL} ', strupper(strtrim(str))) = 1);
      do_wad := (Pos('#INCLUDE_WAD ', strupper(strtrim(str))) = 1) or (Pos('{$INCLUDE_WAD ', strupper(strtrim(str))) = 1) or (Pos('{$INCLUDE_WAD} ', strupper(strtrim(str))) = 1);
      if do_one or do_all or do_wad then
      begin
        splitstring(strtrim(str), s1, s2, ' ');
        s2 := strtrim(s2);
        if s2 <> '' then
        begin
          for j := 1 to Length(s2) do
            if s2[j] in ['"', '''', '<', '>', '}', '{'] then
              s2[j] := ' ';
          s2 := strtrim(s2);
          if s2 <> '' then
          begin
            if do_all then
              str_incl := PAK_ReadAllFilesAsString(s2)
            else if do_wad then
            begin
              lump := W_CheckNumForName(s2);
              if lump >= 0 then
                str_incl := W_TextLumpNum(lump)
              else
                str_incl := '';
            end
            else
              str_incl := PAK_ReadFileAsString(s2);

            if str_incl <> '' then
            begin
              s2 := strupper(s2);
              if includes.IndexOf(s2) < 0 then
              begin
                includes.Add(s2);
                result := result +
                          decide(addcomment, '//---->' + str + #13#10, '') +
                          SC_DoPreprocess(str_incl) + #13#10 +
                          decide(addcomment, '//---->' + str + ' + <--- end of include'#13#10, '');
                idx := includes.IndexOf(s2);
                if idx >= 0 then
                  includes.Delete(idx);
              end
              else
                I_Warning('SC_Preprocess(): Invalid recoursive call of include file %s'#13#10, [s2]);
            end
            else
              I_Warning('SC_Preprocess(): Include file %s does not exist or is empty'#13#10, [s2]);
          end;
        end;
      end
      {$IFDEF DOOM}
      else if (strupper(strtrim(str)) = 'VANILLA_DEMO_OFF') or (strupper(strtrim(str)) = '#VANILLA_DEMO_OFF') or (strupper(strtrim(str)) = '{$VANILLA_DEMO_OFF}') then
        vanilla_demo_off := true
      {$ENDIF}
      else
        result := result + str1 + #13#10;
    end;
    lst_in.Free;
    dec(depth);
  end;

begin
  includes := TDStringList.Create;
  depth := 0;
  decoded_text := '';
  for i := 1 to length(inp_text) do
    if Ord(inp_text[i]) < 128 then
      decoded_text := decoded_text + inp_text[i];
  result := SC_DoPreprocess(decoded_text);
  includes.Free;

  ppc := TDefinesPreprocessor.Create;
  for i := 0 to gamedefines.Count - 1 do
    ppc.AddDefine(gamedefines.Strings[i]);
  result := ppc.Preprocess(result);
  ppc.Free;
end;

end.
