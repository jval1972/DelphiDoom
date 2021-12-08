//------------------------------------------------------------------------------
//
//  DelphiDoom: A modified and improved DOOM engine for Windows
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 1993-1996 by id Software, Inc.
//  Copyright (C) 2004-2021 by Jim Valavanis
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

unit sc_engine;

interface

uses
  d_delphi;

//
// JVAL
//
// Script Engine
//

// TScriptEngine
const
  MAX_STRING_SIZE = 1024;

type
  TScriptEngine = class
  private
    sc_String: PChar;
    sc_Integer: integer;
    sc_Float: float;
    sc_Line: integer;
    sc_End: boolean;
    fNewLine: boolean;
    fBracketLevel: integer;
    fParenthesisLevel: integer;
    ScriptBuffer: PChar;
    ScriptPtr: PChar;
    ScriptEndPtr: PChar;
    StringBuffer: array [0..MAX_STRING_SIZE] of char;
    ScriptSize: integer;
    AlreadyGot: boolean;
    ignonelist: TDStringList;
    faliases: TDStringList;
  protected
    function fToken: string;
  public
    constructor Create(const tx: string); virtual;
    destructor Destroy; override;
    procedure AddIgnoreToken(const s: string);
    procedure Clear;
    procedure SetText(const tx: string); virtual;
    procedure ScriptError(const err: string); overload;
    procedure ScriptError(const Fmt: string; const Args: array of const); overload;
    procedure ScriptPrintLineError(const lnno, offs: integer);
    function GetString: boolean;
    function GetStringEOL: string;
    function GetStringEOLWithQuotes: string;
    function GetTokensEOL: TDStringList;
    function GetStringEOLUnChanged: string;
    procedure MustGetString;
    procedure MustGetStringName(const name: string);
    function GetInteger: boolean;
    procedure MustGetInteger;
    function GetFloat: boolean;
    procedure MustGetFloat;
    procedure UnGet;
    function MatchString(const strs: TDStringList): integer; overload;
    function MatchString(const str: string): boolean; overload;
    function MatchPosString(const str: string): boolean;
    function MustMatchString(strs: TDStringList): integer;
    function Compare(const txt: string): boolean;
    procedure AddAlias(const src, dest: string);
    procedure ClearAliases;
    property _Integer: integer read sc_Integer;
    property _Float: float read sc_Float;
    property _String: string read fToken;
    property _Finished: boolean read sc_End;
    property _Line: integer read sc_Line;
    property NewLine: boolean read fNewLine;
    property BracketLevel: integer read fBracketLevel;
    property ParenthesisLevel: integer read fParenthesisLevel;
  end;

function SC_RemoveLineQuotes(const sctext: string): string;

function SC_RemoveLineComments(const inp: string): string;

function SC_FixParenthesisLevel(const inp: string): string;

function SC_ParamsToList(inp: string): TDStringList;

function SC_IsStringInQuotes(const s: string): boolean;

function SC_TokensToList(const inp: string): TDStringList;

implementation

uses
  i_system;

const
  ASCII_QUOTE = '"';
  ASCII_COMMENT1 = '/';
  ASCII_COMMENT = $2F2F; // = '//'


// TScriptEngine
constructor TScriptEngine.Create(const tx: string);
begin
  Inherited Create;
  ignonelist := TDStringList.Create;
  faliases := TDStringList.Create;
  fBracketLevel := 0;
  fParenthesisLevel := 0;
  fNewLine := false;
  Clear;
  SetText(tx);
end;

destructor TScriptEngine.Destroy;
begin
  Clear;
  ignonelist.Free;
  faliases.Free;
  Inherited;
end;

procedure TScriptEngine.AddIgnoreToken(const s: string);
begin
  ignonelist.Add(strupper(s));
end;

function TScriptEngine.fToken: string;
var
  idx: integer;
begin
  result := StringVal(sc_String);
  idx := faliases.IndexOfName(strupper(result));
  if idx >= 0 then
    result := faliases.ValuesIdx[idx];
end;

procedure TScriptEngine.Clear;
begin
  if ScriptSize > 0 then
    memfree(pointer(ScriptBuffer), ScriptSize);

  ScriptBuffer := nil;
  ScriptSize := 0;
  ScriptPtr := ScriptBuffer;
  ScriptEndPtr := ScriptPtr + ScriptSize;
  sc_Line := 1;
  sc_End := false;
  sc_String := @StringBuffer[0];
  AlreadyGot := false;
end;

procedure TScriptEngine.SetText(const tx: string);
var
  p: Pointer;
  size: integer;
begin
  size := Length(tx);
  p := malloc(size);
  Move(tx[1], p^, size);

  Clear;
  ScriptBuffer := p;
  ScriptSize := size;
  ScriptPtr := ScriptBuffer;
  ScriptEndPtr := ScriptPtr + ScriptSize;
  sc_Line := 1;
  sc_End := false;
  sc_String := @StringBuffer[0];
  AlreadyGot := false;
end;

procedure TScriptEngine.ScriptError(const err: string);
begin
  I_Warning('%s'#13#10, [err]);
end;

procedure TScriptEngine.ScriptError(const fmt: string; const Args: array of const);
var
  s: string;
begin
  sprintf(s, fmt, Args);
  ScriptError(s);
end;

procedure TScriptEngine.ScriptPrintLineError(const lnno, offs: integer);
var
  lst: TDStringList;
  i: integer;
  x1, x2: integer;
  stmp: string;
begin
  lst := TDStringList.Create;
  lst.Text := StringVal(ScriptBuffer);
  x1 := lnno - offs - 1;
  x2 := lnno + offs - 1;
  if x1 < 0 then
    x1 := 0;
  if x2 >= lst.Count - 1 then
    x2 := lst.Count;
  I_Warning('=========================================================================='#13#10);
  for i := x1 to x2 do
  begin
    stmp := IntToStrZfill(5, i);
    if i = lnno - 1 then
      stmp := stmp + ' *** '
    else
      stmp := stmp + '     ';
    I_Warning(stmp + lst.Strings[i] + #13#10);
  end;
  I_Warning('=========================================================================='#13#10);
  I_Warning(#13#10);
  lst.Free;
end;

procedure TScriptEngine.MustGetString;
begin
  if not GetString then
  begin
    ScriptError('TScriptEngine.MustGetString(): Missing string at Line %d', [sc_Line]);
    ScriptPrintLineError(sc_Line, 2);
  end;
end;

procedure TScriptEngine.MustGetStringName(const name: string);
begin
  MustGetString;
  if not Compare(name) then
  begin
    ScriptError('TScriptEngine.MustGetStringName(): "%s" expected at Line %d', [name, sc_Line]);
    ScriptPrintLineError(sc_Line, 2);
  end;
end;

function TScriptEngine.GetInteger: boolean;
var
  code: integer;
begin
  if GetString then
  begin
    val(StringVal(sc_String), sc_Integer, code);
    if code <> 0 then
    begin
      ScriptError(
          'TScriptEngine.GetInteger(): Bad numeric constant "%s" at Line %d',
          [sc_String, sc_Line]);
      ScriptPrintLineError(sc_Line, 2);
      result := false;
      exit;
    end;
    result := true;
  end
  else
    result := true;
end;

procedure TScriptEngine.MustGetInteger;
begin
  if not GetInteger then
  begin
    ScriptError('TScriptEngine.MustGetInteger(): Missing integer at Line %d', [sc_Line]);
    ScriptPrintLineError(sc_Line, 2);
  end;
end;

function TScriptEngine.GetFloat: boolean;
var
  code: integer;
  i: integer;
  str: string;
begin
  if GetString then
  begin
    str := StringVal(sc_String);
    val(str, sc_Float, code);
    if code <> 0 then
    begin
      for i := 1 to Length(str) do
        if str[i] in ['.', ','] then
          str[i] := '.';
      val(str, sc_Float, code);
      if code <> 0 then
      begin
        for i := 1 to Length(str) do
          if str[i] = '.' then
            str[i] := ',';
        val(str, sc_Float, code);
        if code <> 0 then
        begin
          ScriptError(
              'TScriptEngine.GetFloat(): Bad numeric constant "%s" at Line %d',
              [sc_String, sc_Line]);
          ScriptPrintLineError(sc_Line, 2);
          result := false;
          exit;
        end;
      end;
    end;
    result := true;
  end
  else
    result := true;
end;

procedure TScriptEngine.MustGetFloat;
begin
  if not GetFloat then
  begin
    ScriptError('TScriptEngine.MustGetFloat(): Missing float at Line %d', [sc_Line]);
    ScriptPrintLineError(sc_Line, 2);
  end;
end;

procedure TScriptEngine.UnGet;
// Assumes there is a valid string in sc_String.
begin
  AlreadyGot := true;
end;

function TScriptEngine.MatchString(const str: string): boolean;
begin
  result := Compare(str);
end;

function TScriptEngine.MatchString(const strs: TDStringList): integer;
// Returns the index of the first match to sc_String from the passed
// array of strings, or -1 if not found.
var
  i: integer;
begin
  for i := 0 to strs.Count - 1 do
  begin
    if Compare(strs.Strings[i]) then
    begin
      result := i;
      exit;
    end;
  end;
  result := -1;
end;

function TScriptEngine.MatchPosString(const str: string): boolean;
begin
  result := Pos(strupper(str), strupper(StringVal(sc_String))) > 0;
end;

function TScriptEngine.MustMatchString(strs: TDStringList): integer;
var
  i: integer;
begin
  i := MatchString(strs);
  if i = -1 then
  begin
    ScriptError('TScriptEngine.MustMatchString(): List'#13#10'%s'#13#10'expected at Line %d',  [strs.Text, sc_Line]);
    ScriptPrintLineError(sc_Line, 2);
  end;
  result := i;
end;

function TScriptEngine.Compare(const txt: string): boolean;
begin
  result := strupper(txt) = strupper(fToken);
end;

procedure TScriptEngine.AddAlias(const src, dest: string);
begin
  faliases.Add(strupper(src + '=' + dest));
end;

procedure TScriptEngine.ClearAliases;
begin
  faliases.Clear;
end;

function TScriptEngine.GetString: boolean;
var
  txt: PChar;
  foundToken: boolean;
begin
  if AlreadyGot then
  begin
    AlreadyGot := false;
    result := true;
    exit;
  end;

  fNewLine := false;
  foundToken := false;
  if ScriptPtr >= ScriptEndPtr then
  begin
    sc_End := true;
    result := false;
    exit;
  end;

  while not foundToken do
  begin
    while (ScriptPtr^ <= Chr(32)) or (ScriptPtr^ in ['{', '}', '(', ')', ',']) do
    begin
      if ScriptPtr >= ScriptEndPtr then
      begin
        sc_End := true;
        result := false;
        exit;
      end;
      if ScriptPtr^ = '{' then
        inc(fBracketLevel)
      else if ScriptPtr^ = '}' then
      begin
        dec(fBracketLevel);
        if fBracketLevel < 0 then
        begin
          ScriptError('TScriptEngine.GetString(): Closing bracket "}" found at line %d without opening bracket "{"', [sc_Line]);
          ScriptPrintLineError(sc_Line, 2);
          fBracketLevel := 0;
        end;
      end
      else if ScriptPtr^ = '(' then
        inc(fParenthesisLevel)
      else if ScriptPtr^ = ')' then
      begin
        dec(fParenthesisLevel);
        if fParenthesisLevel < 0 then
        begin
          ScriptError('TScriptEngine.GetString(): Closing parenthesis ")" found at line %d without opening parenthesis "("', [sc_Line]);
          ScriptPrintLineError(sc_Line, 2);
          fParenthesisLevel := 0;
        end;
      end
      else if ScriptPtr^ = Chr(10) then
      begin
        inc(sc_Line);
        fNewLine := true;
      end;
      inc(ScriptPtr);
    end;

    if ScriptPtr >= ScriptEndPtr then
    begin
      sc_End := true;
      result := false;
      exit;
    end;

    if ScriptPtr^ <> ASCII_COMMENT1 then
    begin // Found a token
      foundToken := true;
    end
    else
    begin // Skip comment

      if ScriptPtr >= ScriptEndPtr then
      begin
        sc_End := true;
        result := false;
        exit;
      end;
      inc(ScriptPtr);
      if ScriptPtr^ = ASCII_COMMENT1 then
      begin
        while (ScriptPtr^ <> Chr(13)) and (ScriptPtr^ <> Chr(10)) do
        begin
          if ScriptPtr >= ScriptEndPtr then
          begin
            sc_End := true;
            result := false;
            exit;
          end;
          inc(ScriptPtr);
        end;
        inc(sc_Line);
        fNewLine := true;
      end;
    end;
  end;

  txt := sc_String;
  if ScriptPtr^ = ASCII_QUOTE then
  begin // Quoted string
    inc(ScriptPtr);
    while ScriptPtr^ <> ASCII_QUOTE do
    begin
      txt^ := ScriptPtr^;
      inc(txt);
      inc(ScriptPtr);
      if (ScriptPtr = ScriptEndPtr) or
         (txt = @sc_String[MAX_STRING_SIZE - 1]) then
        break;
    end;
    inc(ScriptPtr);
  end
  else
  begin // Normal string
    while (ScriptPtr^ > Chr(32)) and (ScriptPtr < ScriptEndPtr) and
          (PWord(ScriptPtr)^ <> ASCII_COMMENT) and (not (ScriptPtr^ in ['{', '}', '(', ')', ','])) do
    begin
      txt^ := ScriptPtr^;
      inc(txt);
      inc(ScriptPtr);
      if (ScriptPtr = ScriptEndPtr) or
         (txt = @sc_String[MAX_STRING_SIZE - 1]) then
        break;

    end;
  end;
  txt^ := Chr(0);
  if ignonelist.IndexOf(strupper(StringVal(sc_String))) < 0 then
    result := true
  else
    Result := GetString;
end;

function TScriptEngine.GetStringEOL: string;
begin
  result := '';
  if not GetString then
    exit;

  if fNewLine then
  begin
    AlreadyGot := true;
    exit;
  end;
  result := StringVal(sc_string);
  while not sc_End and not fNewLine do
  begin
    GetString;
    if fNewLine then
    begin
      AlreadyGot := true;
      exit;
    end;
    result := result + ' ' + StringVal(sc_string);
  end;
end;

function TScriptEngine.GetStringEOLWithQuotes: string;
begin
  result := '';
  if not GetString then
    exit;

  if fNewLine then
  begin
    AlreadyGot := true;
    exit;
  end;
  result := '"' + StringVal(sc_string) + '"';
  while not sc_End and not fNewLine do
  begin
    GetString;
    if fNewLine then
    begin
      AlreadyGot := true;
      exit;
    end;
    result := result + ' ' + '"' + StringVal(sc_string) + '"';
  end;
end;

function TScriptEngine.GetTokensEOL: TDStringList;
begin
  result := TDStringList.Create;
  if not GetString then
    exit;

  if fNewLine then
  begin
    AlreadyGot := true;
    exit;
  end;
  result.Add(sc_string);
  while not sc_End and not fNewLine do
  begin
    GetString;
    if fNewLine then
    begin
      AlreadyGot := true;
      exit;
    end;
    result.Add(sc_string);
  end;
end;

function TScriptEngine.GetStringEOLUnChanged: string;
begin
  result := '';
  if ScriptPtr < ScriptEndPtr then
    if ScriptPtr^ = #10 then
    begin
      Inc(ScriptPtr);
      if ScriptPtr < ScriptEndPtr then
      begin
        result := ScriptPtr^;
      end;
    end;
  while ScriptPtr^ <> #10 do
  begin
    Inc(ScriptPtr);
    if ScriptPtr >= ScriptEndPtr then
    begin
      sc_End := true;
      exit;
    end;
    if not (ScriptPtr^ in [#10, #13]) then
      result := result + ScriptPtr^;
  end;
end;

function SC_RemoveLineQuotes(const sctext: string): string;
var
  stmp: string;
  s: TDStringList;
  i, p: integer;
begin
  result := '';
  s := TDStringList.Create;
  try
    s.Text := sctext;
    for i := 0 to s.Count - 1 do
    begin
      stmp := strtrim(s[i]);
      p := Pos('//', stmp);
      if p > 0 then
        stmp := Copy(stmp, 1, p - 1);
      p := Pos(';', stmp);
      if p > 0 then
        stmp := Copy(stmp, 1, p - 1);
      if stmp <> '' then
        result := result + stmp + #13#10;
    end;
  finally
    s.Free;
  end;
end;

function SC_RemoveLineComments(const inp: string): string;
var
  i: integer;
  inquotes: boolean;
begin
  result := '';
  inquotes := false;
  for i := 1 to Length(inp) do
  begin
    if inp[i] = '"' then
      inquotes := not inquotes;
    if not inquotes then
      if i < length(inp) then
        if (inp[i] = '/') and (inp[i + 1] = '/') then
          break;
    result := result + inp[i];
  end;
end;

function SC_FixParenthesisLevel(const inp: string): string;
var
  i: integer;
  pleft, pright: integer;
begin
  result := inp;
  pleft := 0;
  pright := 0;
  for i := 1 to length(result) do
    if result[i] = '(' then
      inc(pleft)
    else if result[i] = ')' then
      inc(pright);
  while pright < pleft do
  begin
    result := result + ')';
    inc(pright);
  end;
  while pright > pleft do
  begin
    result := '(' + result;
    inc(pleft);
  end;
end;

function SC_ParamsToList(inp: string): TDStringList;
var
  stmp: string;
  i: integer;
  parenthesislevel: integer;
  inquotes: boolean;
  allinparenthesis: boolean;
begin
  result := TDStringList.Create;
  inp := strtrim(inp);
  if inp = '' then
    exit;

  if (inp[1] = '(') and (inp[length(inp)] = ')') then
  begin
    allinparenthesis := true;
    parenthesislevel := 0;
    for i := 2 to length(inp) - 1 do
    begin
      if inp[i] = '(' then
        inc(parenthesislevel)
      else if inp[i] = ')' then
        dec(parenthesislevel);
      if parenthesislevel < 0 then
      begin
        allinparenthesis := false;
        break;
      end;
    end;
    if allinparenthesis then
    begin
      inp[1] := ' ';
      inp[length(inp)] := ' ';
      inp := strtrim(inp);
      if inp = '' then
        exit;
    end;
  end;

  parenthesislevel := 0;
  inquotes := false;
  stmp := '';

  for i := 1 to Length(inp) do
  begin
    if inp[i] = ',' then
      if not inquotes and (parenthesislevel = 0) then
      begin
        stmp := strtrim(stmp);
        if stmp <> '' then
        begin
          result.Add(stmp);
          stmp := '';
        end;
        continue;
      end;
    stmp := stmp + inp[i];
    if inp[i] = '(' then
      inc(parenthesislevel)
    else if inp[i] = ')' then
    begin
      dec(parenthesislevel);
      if parenthesislevel < 0 then
      begin
        //I_Warning ....
        parenthesislevel := 0;
      end;
    end else if inp[i] = '"' then
      inquotes := not inquotes;
  end;
  stmp := strtrim(stmp);
  if stmp <> '' then
    result.Add(stmp);
end;

function SC_IsStringInQuotes(const s: string): boolean;
var
  i: integer;
begin
  if Length(s) < 2 then
  begin
    result := false;
    exit;
  end;
  if not (s[1] in ['''', '"']) then
  begin
    result := false;
    exit;
  end;
  if s[1] <> s[length(s)] then
  begin
    result := false;
    exit;
  end;
  for i := 2 to Length(s) - 1 do
    if s[i] = s[1] then
    begin
      result := false;
      exit;
    end;
  result := true;
end;

function SC_TokensToList(const inp: string): TDStringList;
var
  sc: TScriptEngine;
begin
  result := TDStringList.Create;
  sc := TScriptEngine.Create(inp);
  while sc.GetString do
    result.Add(sc._string);
  sc.Free;
end;

end.
