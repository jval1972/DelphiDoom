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
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit sc_tokens;

interface

uses
  d_delphi;

type
  TTokenList = class(TDStringList)
  public
    function IndexOfToken(const S: string): Integer; virtual;
    function AllTokens: TDStringList;
  end;

//==============================================================================
//
// SC_EvalueateIntToken
//
//==============================================================================
function SC_EvalueateIntToken(const token: string; const A: array of string): integer;

implementation

//==============================================================================
//
// TTokenList.IndexOfToken
//
//==============================================================================
function TTokenList.IndexOfToken(const S: string): Integer;
var
  i, j: integer;
  list: TDStringList;
  stmp, stmp2: string;
begin
  result := IndexOf(S);
  if result > -1 then
    exit;

  list := TDStringList.Create;

  for i := 0 to Count - 1 do
  begin
    stmp := Strings[i];
    stmp2 := '';
    for j := 1 to Length(stmp) do
      if stmp[j] = ',' then
        stmp2 := stmp2 + #13#10
      else
        stmp2 := stmp2 + stmp[j];
    stmp := strtrim(stmp2);
    if stmp <> '' then
    begin
      list.Text := stmp;
      for j := 0 to list.Count - 1 do
        list.Strings[j] := strtrim(list.Strings[j]);
      if list.IndexOf(S) > -1 then
      begin
        list.Free;
        result := i;
        exit;
      end;
    end;
  end;
  list.Free;
end;

//==============================================================================
//
// TTokenList.AllTokens
//
//==============================================================================
function TTokenList.AllTokens: TDStringList;
var
  i, j: integer;
  list: TDStringList;
  stmp, stmp2: string;
begin
  Result := TDStringList.Create;

  list := TDStringList.Create;
  for i := 0 to Count - 1 do
  begin
    stmp := Strings[i];
    stmp2 := '';
    for j := 1 to Length(stmp) do
      if stmp[j] = ',' then
        stmp2 := stmp2 + #13#10
      else
        stmp2 := stmp2 + stmp[j];
    stmp := strtrim(stmp2);
    if stmp <> '' then
    begin
      list.Text := stmp;
      for j := 0 to list.Count - 1 do
        Result.Add(strtrim(list.Strings[j]));
    end;
  end;
  list.Free;
end;

//==============================================================================
//
// SC_EvalueateIntToken
//
//==============================================================================
function SC_EvalueateIntToken(const token: string; const A: array of string): integer;
var
  i: integer;
  tt: TTokenList;
begin
  i := atoi(token);
  if itoa(i) = token then
  begin
    result := i;
    exit;
  end;

  tt := TTokenList.Create;
  for i := Low(A) to High(A) do
    tt.Add(A[i]);
  result := tt.IndexOfToken(token);
  tt.Free;
end;

end.
