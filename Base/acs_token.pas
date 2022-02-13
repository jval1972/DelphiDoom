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
//  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
//  02111-1307, USA.
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit acs_token;

interface

uses
  d_delphi;

const
  NON_HEX_DIGIT = 255;
  MAX_NESTED_SOURCES = 16;

const
  TK_NONE = 0;
  TK_EOF = 1;
  TK_IDENTIFIER = 2;  // VALUE: (char *) tk_Text
  TK_STRING = 3;  // VALUE: (char *) tk_Text
  TK_NUMBER = 4;  // VALUE: (int) tk_Num
  TK_LINESPECIAL = 5;  // VALUE: (int) tk_LineSpecial
  TK_PLUS = 6;  // '+'
  TK_MINUS = 7;  // '-'
  TK_ASTERISK = 8;  // '*'
  TK_SLASH = 9;  // '/'
  TK_PERCENT = 10;  // '%'
  TK_ASSIGN = 11;  // '='
  TK_ADDASSIGN = 12;  // '+='
  TK_SUBASSIGN = 13;  // '-='
  TK_MULASSIGN = 14;  // '*='
  TK_DIVASSIGN = 15;  // '/='
  TK_MODASSIGN = 16;  // '%='
  TK_INC = 17;  // '++'
  TK_DEC = 18;  // '--'
  TK_EQ = 19;  // '=='
  TK_NE = 20;  // '!='
  TK_LT = 21;  // '<'
  TK_GT = 22;  // '>'
  TK_LE = 23;  // '<='
  TK_GE = 24;  // '>='
  TK_LSHIFT = 25;  // '<<'
  TK_RSHIFT = 26;  // '>>'
  TK_ANDLOGICAL = 27;  // '&&'
  TK_ORLOGICAL = 28;  // '||'
  TK_ANDBITWISE = 29;  // '&'
  TK_ORBITWISE = 30;  // '|'
  TK_EORBITWISE = 31;  // '^'
  TK_TILDE = 32;  // '~'
  TK_LPAREN = 33;  // '('
  TK_RPAREN = 34;  // ')'
  TK_LBRACE = 35;  // '{'
  TK_RBRACE = 36;  // '}'
  TK_LBRACKET = 37;  // '['
  TK_RBRACKET = 38;  // ']'
  TK_COLON = 39;  // ':'
  TK_SEMICOLON = 40;  // ';'
  TK_COMMA = 41;  // ''
  TK_PERIOD = 42;  // '.'
  TK_NOT = 43;  // '!'
  TK_NUMBERSIGN = 44;  // '#'
  TK_CPPCOMMENT = 45;  // '//'
  TK_STARTCOMMENT = 46;  // '/*'
  TK_ENDCOMMENT = 47;  // '*/'
  TK_BREAK = 48;  // 'break'
  TK_CASE = 49;  // 'case'
  TK_CONST = 50;  // 'const'
  TK_CONTINUE = 51;  // 'continue'
  TK_DEFAULT = 52;  // 'default'
  TK_DEFINE = 53;  // 'define'
  TK_DO = 54;  // 'do'
  TK_ELSE = 55;  // 'else'
  TK_FOR = 56;  // 'for'
  TK_GOTO = 57;  // 'goto'
  TK_IF = 58;  // 'if'
  TK_INCLUDE = 59;  // 'include'
  TK_INT = 60;  // 'int'
  TK_OPEN = 61;  // 'open'
  TK_PRINT = 62;  // 'print'
  TK_PRINTBOLD = 63;  // 'printbold'
  TK_RESTART = 64;  // 'restart'
  TK_SCRIPT = 65;  // 'script'
  TK_SPECIAL = 66;  // 'special'
  TK_STR = 67;  // 'str'
  TK_SUSPEND = 68;  // 'suspend'
  TK_SWITCH = 69;  // 'switch'
  TK_TERMINATE = 70;  // 'terminate'
  TK_UNTIL = 71;  // 'until'
  TK_VOID = 72;  // 'void'
  TK_WHILE = 73;  // 'while'
  TK_WORLD = 74;  // 'world'

const
  CHR_EOF = 0;
  CHR_LETTER = 1;
  CHR_NUMBER = 2;
  CHR_QUOTE = 3;
  CHR_SPECIAL = 4;

type
  nestInfo_t = record
    name: string;
    start: PChar;
    finish: PChar;
    position: PChar;
    size: integer;
    line: integer;
    incLineNumber: boolean;
    lastChar: char;
  end;
  PnestInfo_t = ^nestInfo_t;

//==============================================================================
//
// TK_Init
//
//==============================================================================
procedure TK_Init;

//==============================================================================
//
// TK_OpenSource
//
//==============================================================================
procedure TK_OpenSource(const fileName: string);

//==============================================================================
//
// TK_OpenLump
//
//==============================================================================
procedure TK_OpenLump(const slump: integer);

//==============================================================================
//
// TK_CloseSource
//
//==============================================================================
procedure TK_CloseSource;

//==============================================================================
//
// TK_Undo
//
//==============================================================================
procedure TK_Undo;

//==============================================================================
//
// TK_DoInclude
//
//==============================================================================
procedure TK_DoInclude(const fileName: string);

//==============================================================================
//
// TK_NextToken
//
//==============================================================================
function TK_NextToken: integer;

//==============================================================================
//
// TK_NextTokenMustBe
//
//==============================================================================
procedure TK_NextTokenMustBe(const token: integer; const error: integer);

//==============================================================================
//
// TK_TokenMustBe
//
//==============================================================================
procedure TK_TokenMustBe(const token: integer; const error: integer);

//==============================================================================
//
// TK_NextCharacter
//
//==============================================================================
function TK_NextCharacter: integer;

//==============================================================================
//
// TK_Member
//
//==============================================================================
function TK_Member(const list: PIntegerArray): boolean;

var
  tk_Token: integer;
  tk_Line: integer;
  tk_Num: integer;
  tk_Text: string;
  tk_SpecialValue: integer;
  tk_SpecialArgCount: integer;
  tk_SourceName: string;
  tk_IncludedLines: integer;

//==============================================================================
//
// MakeIncludePath
//
//==============================================================================
procedure MakeIncludePath(const sourceName: string);

implementation

uses
  acs_common,
  acs_error,
  acs_misc,
  acs_symbol;

var
  Ch: Char;
  FileStart: PChar;
  FilePtr: PChar;
  FileEnd: PChar;
  SourceOpen: boolean;
  ASCIIToChrCode: array[0..255] of byte;
  ASCIIToHexDigit: array[0..255] of byte;
  OpenFiles: array[0..MAX_NESTED_SOURCES - 1] of nestInfo_t;
  AlreadyGot: boolean;
  NestDepth: integer;
  IncLineNumber: boolean;
  IncludePath: string;

type
  keyword_t = record
    name: string[10];
    token: integer;
  end;

const
  NUM_KEYWORDS = 28;

var
  keywords: array[0..NUM_KEYWORDS - 1] of keyword_t = (
    (name: 'break';     token: TK_BREAK),
    (name: 'case';      token: TK_CASE),
    (name: 'const';     token: TK_CONST),
    (name: 'continue';  token: TK_CONTINUE),
    (name: 'default';   token: TK_DEFAULT),
    (name: 'define';    token: TK_DEFINE),
    (name: 'do';        token: TK_DO),
    (name: 'else';      token: TK_ELSE),
    (name: 'for';       token: TK_FOR),
    (name: 'goto';      token: TK_GOTO),
    (name: 'if';        token: TK_IF),
    (name: 'include';   token: TK_INCLUDE),
    (name: 'int';       token: TK_INT),
    (name: 'open';      token: TK_OPEN),
    (name: 'print';     token: TK_PRINT),
    (name: 'printbold'; token: TK_PRINTBOLD),
    (name: 'restart';   token: TK_RESTART),
    (name: 'script';    token: TK_SCRIPT),
    (name: 'special';   token: TK_SPECIAL),
    (name: 'str';       token: TK_STR),
    (name: 'suspend';   token: TK_SUSPEND),
    (name: 'switch';    token: TK_SWITCH),
    (name: 'terminate'; token: TK_TERMINATE),
    (name: 'until';     token: TK_UNTIL),
    (name: 'void';      token: TK_VOID),
    (name: 'while';     token: TK_WHILE),
    (name: 'world';     token: TK_WORLD),
    (name: '';          token: -1)
  );

//==============================================================================
//
// NextChr
//
//==============================================================================
procedure NextChr;
begin
  if FilePtr >= FileEnd then
  begin
    Ch := Chr(EOF_CHARACTER);
    exit;
  end;

  if IncLineNumber then
  begin
    inc(tk_Line);
    IncLineNumber := false;
  end;

  Ch := FilePtr^;
  inc(FilePtr);
  if Ch < Chr(ASCII_SPACE) then
  begin
    if Ch = #10 then
      IncLineNumber := True;
    Ch := Chr(ASCII_SPACE);
  end;
end;

//==============================================================================
//
// TK_Init
//
//==============================================================================
procedure TK_Init;
var
  i: integer;
begin
  for i := 0 to 255 do
  begin
    ASCIIToChrCode[i] := CHR_SPECIAL;
    ASCIIToHexDigit[i] := NON_HEX_DIGIT;
  end;

  for i := Ord('0') to Ord('9') do
  begin
    ASCIIToChrCode[i] := CHR_NUMBER;
    ASCIIToHexDigit[i] :=  i - Ord('0');
  end;

  for i := Ord('A') to Ord('F') do
    ASCIIToHexDigit[i] := 10 + (i - Ord('A'));

  for i := Ord('a') to Ord('f') do
    ASCIIToHexDigit[i] := 10 + (i - Ord('a'));

  for i := Ord('A') to Ord('Z') do
    ASCIIToChrCode[i] := CHR_LETTER;

  for i := Ord('a') to Ord('z') do
    ASCIIToChrCode[i] := CHR_LETTER;

  ASCIIToChrCode[ASCII_QUOTE] := CHR_QUOTE;
  ASCIIToChrCode[ASCII_UNDERSCORE] := CHR_LETTER;
  ASCIIToChrCode[EOF_CHARACTER] := CHR_EOF;
  tk_Text := '';
  IncLineNumber := false;
  tk_IncludedLines := 0;
  SourceOpen := false;
end;

//==============================================================================
//
// TK_OpenSource
//
//==============================================================================
procedure TK_OpenSource(const fileName: string);
var
  size: integer;
  p: pointer;
begin
  TK_CloseSource;
  size := ACS_LoadFile(fileName, p);
  FileStart := p;
  tk_SourceName := fileName;
  MakeIncludePath(fileName);
  SourceOpen := true;
  FileEnd := FileStart;
  Inc(FileEnd, size);
  FilePtr := FileStart;
  tk_Line := 1;
  tk_Token := TK_NONE;
  AlreadyGot := false;
  NestDepth := 0;
  NextChr;
end;

//==============================================================================
//
// TK_OpenLump
//
//==============================================================================
procedure TK_OpenLump(const slump: integer);
var
  size: integer;
  p: pointer;
begin
  TK_CloseSource;
  size := ACS_LoadLump(slump, p);
  FileStart := p;
  tk_SourceName := itoa(slump);
  IncludePath := '';
  SourceOpen := true;
  FileEnd := FileStart;
  Inc(FileEnd, size);
  FilePtr := FileStart;
  tk_Line := 1;
  tk_Token := TK_NONE;
  AlreadyGot := false;
  NestDepth := 0;
  NextChr;
end;

//==============================================================================
//
// MakeIncludePath
//
//==============================================================================
procedure MakeIncludePath(const sourceName: string);
begin
  IncludePath := fpath(sourceName);
end;

//==============================================================================
//
// TK_DoInclude
//
//==============================================================================
procedure TK_DoInclude(const fileName: string);
var
  size: integer;
  info: PnestInfo_t;
  p: pointer;
begin
  if NestDepth = MAX_NESTED_SOURCES then
    ERR_Exit(ERR_INCL_NESTING_TOO_DEEP, true,
      'Unable to include file ''%s''.', [fileName]);

  info := @OpenFiles[NestDepth];
  Inc(NestDepth);
  info.name := tk_SourceName;
  info.start := FileStart;
  info.finish := FileEnd;
  info.size := Integer(FileEnd) - Integer(FileStart);
  info.position := FilePtr;
  info.line := tk_Line;
  info.incLineNumber := IncLineNumber;
  info.lastChar := Ch;
  tk_SourceName := IncludePath;
  tk_SourceName := tk_SourceName + fileName;
  size := ACS_LoadFile(tk_SourceName, p);
  FileStart := p;
  FileEnd := FileStart;
  Inc(FileEnd, size);
  FilePtr := FileStart;
  tk_Line := 1;
  IncLineNumber := false;
  tk_Token := TK_NONE;
  AlreadyGot := false;
  NextChr;
end;

//==============================================================================
//
// PopNestedSource
//
//==============================================================================
procedure PopNestedSource;
var
  info: PnestInfo_t;
begin
  ACS_Free(Pointer(FileStart));
  tk_IncludedLines := tk_IncludedLines + tk_Line;
  Dec(NestDepth);
  info := @OpenFiles[NestDepth];
  tk_SourceName := info.name;
  FileStart := info.start;
  FileEnd := info.finish;
  FilePtr := info.position;
  tk_Line := info.line;
  IncLineNumber := info.incLineNumber;
  Ch := info.lastChar;
  tk_Token := TK_NONE;
  AlreadyGot := false;
end;

//==============================================================================
//
// TK_CloseSource
//
//==============================================================================
procedure TK_CloseSource;
var
  i: integer;
begin
  if SourceOpen then
  begin
    ACS_Free(Pointer(FileStart));
    for i := 0 to NestDepth - 1 do
      ACS_Free(Pointer(OpenFiles[i].start));

    SourceOpen := false;
  end;
end;

//==============================================================================
//
// TK_NextCharacter
//
//==============================================================================
function TK_NextCharacter: integer;
begin
  while Ch = Chr(ASCII_SPACE) do
    NextChr;

  result := Ord(Ch);
  if result = EOF_CHARACTER then
    result :=  -1;

  NextChr;
end;

//==============================================================================
//
// TK_Member
//
//==============================================================================
function TK_Member(const list: PIntegerArray): boolean;
var
  i: integer;
begin
  i := 0;
  while list[i] <> TK_NONE do
  begin
    if tk_Token = list[i] then
    begin
      result := true;
      exit;
    end;
    inc(i);
  end;
  result := false;
end;

//==============================================================================
//
// TK_Undo
//
//==============================================================================
procedure TK_Undo;
begin
  if tk_Token <> TK_NONE then
    AlreadyGot := true;
end;

//==============================================================================
//
// CheckForKeyword
//
//==============================================================================
function CheckForKeyword: boolean;
var
  i: integer;
begin
  i := 0;
  while Keywords[i].name <> '' do
  begin
    if tk_Text = Keywords[i].name then
    begin
      tk_Token := Keywords[i].token;
      result := true;
      exit;
    end;
    Inc(i);
  end;
  result := false;
end;

//==============================================================================
//
// CheckForLineSpecial
//
//==============================================================================
function CheckForLineSpecial: boolean;
var
  sym: PsymbolNode_t;
begin
  sym := SY_FindGlobal(tk_Text);

  if sym = nil then
  begin
    result := false;
    exit;
  end;

  if sym.typ <> SY_SPECIAL then
  begin
    result := false;
    exit;
  end;

  tk_Token := TK_LINESPECIAL;
  tk_SpecialValue := sym.info.special.value;
  tk_SpecialArgCount := sym.info.special.argCount;
  result := true;
end;

//==============================================================================
//
// CheckForConstant
//
//==============================================================================
function CheckForConstant: boolean;
var
  sym: PsymbolNode_t;
begin
  sym := SY_FindGlobal(tk_Text);

  if sym = nil then
  begin
    result := false;
    exit;
  end;

  if sym.typ <> SY_CONSTANT then
  begin
    result := false;
    exit;
  end;

  tk_Token := TK_NUMBER;
  tk_Num := sym.info.constant.value;
  result := true;
end;

//==============================================================================
//
// ProcessLetterToken
//
//==============================================================================
procedure ProcessLetterToken;
var
  stmp: string[MAX_IDENTIFIER_LENGTH];
begin
  stmp := '';
  while ASCIIToChrCode[Ord(Ch)] in [CHR_LETTER, CHR_NUMBER] do
  begin
    if Length(stmp) = MAX_IDENTIFIER_LENGTH then
      ERR_Exit(ERR_IDENTIFIER_TOO_LONG, True, '', []);
    stmp := stmp + Ch;
    NextChr;
  end;
  tk_Text := strlower(stmp);
  if not CheckForKeyword and not CheckForLineSpecial and not CheckForConstant then
    tk_Token := TK_IDENTIFIER;
end;

//==============================================================================
//
// EvalFixedConstant
//
//==============================================================================
procedure EvalFixedConstant(const whole: integer);
var
  frac: integer;
  divisor: integer;
begin
  frac :=  0;
  divisor :=  1;
  while ASCIIToChrCode[Ord(Ch)] = CHR_NUMBER do
  begin
    frac := 10 * frac + (Ord(Ch) - Ord('0'));
    divisor := divisor * 10;
    NextChr;
  end;
  tk_Num := (whole shl 16) + ((frac shl 16) div divisor);
  tk_Token := TK_NUMBER;
end;

//==============================================================================
//
// EvalHexConstant
//
//==============================================================================
procedure EvalHexConstant;
begin
  tk_Num := 0;
  while ASCIIToHexDigit[Ord(Ch)] <> NON_HEX_DIGIT do
  begin
    tk_Num := (tk_Num shl 4) + ASCIIToHexDigit[Ord(Ch)];
    NextChr;
  end;
  tk_Token := TK_NUMBER;
end;

//==============================================================================
//
// DigitValue
//
// Returns -1 if the digit is not allowed in the specified radix.
//
//==============================================================================
function DigitValue(digit: char; const radix: integer): integer;
begin
  digit := toupper(digit);
  if (digit < '0') or ((digit > '9') and (digit < 'A')) or (digit > 'Z') then
  begin
    result := -1;
    exit;
  end;
  if digit > '9' then
    digit := Chr(10 + Ord(digit) - Ord('A'))
  else
    digit := Chr(Ord(digit) - Ord('0'));
  if Ord(digit) >= radix then
    result := -1
  else
    result := Ord(digit);
end;

//==============================================================================
//
// EvalRadixConstant
//
//==============================================================================
procedure EvalRadixConstant;
var
  radix: integer;
  digitVal: integer;
begin
  radix := tk_Num;
  if (radix < 2) or (radix > 36) then
    ERR_Exit(ERR_BAD_RADIX_CONSTANT, True, '', []);

  tk_Num := 0;
  while true do
  begin
    digitVal := DigitValue(Ch, radix);
    if digitVal = -1 then
      break;
    tk_Num := radix * tk_Num + digitVal;
    NextChr;
  end;
  tk_Token := TK_NUMBER;
end;

//==============================================================================
//
// ProcessNumberToken
//
//==============================================================================
procedure ProcessNumberToken;
var
  c: char;
begin
  c := Ch;
  NextChr;

  if (c = '0') and (Ch in ['x', 'X']) then
  begin  // Hexadecimal constant
    NextChr;
    EvalHexConstant;
    exit;
  end;

  tk_Num := Ord(c) - Ord('0');
  while ASCIIToChrCode[Ord(Ch)] = CHR_NUMBER do
  begin
    tk_Num := 10 * tk_Num + (Ord(Ch) - Ord('0'));
    NextChr;
  end;

  if Ch = '.' then
  begin  // Fixed point
    NextChr; // Skip period
    EvalFixedConstant(tk_Num);
    exit;
  end;

  if Ch = Chr(ASCII_UNDERSCORE) then
  begin
    NextChr; // Skip underscore
    EvalRadixConstant;
    exit;
  end;

  tk_Token := TK_NUMBER;
end;

//==============================================================================
//
// ProcessQuoteToken
//
//==============================================================================
procedure ProcessQuoteToken;
begin
  tk_Text := '';
  NextChr;
  while Ch <> Chr(EOF_CHARACTER) do
  begin
    if Ch = Chr(ASCII_QUOTE) then
      break;
    if Length(tk_Text) = MAX_QUOTED_LENGTH then
      ERR_Exit(ERR_STRING_TOO_LONG, True, '', []);

    tk_Text := tk_Text + Ch;
    NextChr;
  end;

  if Ch = Chr(ASCII_QUOTE) then
    NextChr;

  tk_Token := TK_STRING;
end;

//==============================================================================
//
// ProcessSpecialToken
//
//==============================================================================
procedure ProcessSpecialToken;
var
  c: char;
begin
  c :=  Ch;
  NextChr;
  case c of
    '+':
      case Ch of
        '=':
          begin
            tk_Token := TK_ADDASSIGN;
            NextChr;
          end;
        '+':
          begin
            tk_Token := TK_INC;
            NextChr;
          end;
      else
        tk_Token := TK_PLUS;
      end;
    '-':
      case Ch of
        '=':
          begin
            tk_Token := TK_SUBASSIGN;
            NextChr;
          end;
        '-':
          begin
            tk_Token := TK_DEC;
            NextChr;
          end;
      else
        tk_Token := TK_MINUS;
      end;
    '*':
      case Ch of
        '=':
          begin
            tk_Token :=  TK_MULASSIGN;
            NextChr;
          end;
        '/':
          begin
            tk_Token := TK_ENDCOMMENT;
            NextChr;
          end;
      else
        tk_Token := TK_ASTERISK;
      end;
    '/':
      case Ch of
        '=':
          begin
            tk_Token := TK_DIVASSIGN;
            NextChr;
          end;
        '/':
          tk_Token := TK_CPPCOMMENT;
        '*':
          begin
            tk_Token := TK_STARTCOMMENT;
            NextChr;
          end;
      else
        tk_Token := TK_SLASH;
      end;
    '%':
      if Ch = '=' then
      begin
        tk_Token := TK_MODASSIGN;
        NextChr;
      end
      else
        tk_Token := TK_PERCENT;
    '=':
      if Ch = '=' then
      begin
        tk_Token := TK_EQ;
        NextChr;
      end
      else
        tk_Token := TK_ASSIGN;
    '<':
      if Ch = '=' then
      begin
        tk_Token := TK_LE;
        NextChr;
      end
      else if Ch = '<' then
      begin
        tk_Token := TK_LSHIFT;
        NextChr;
      end
      else
        tk_Token := TK_LT;
    '>':
      if Ch = '=' then
      begin
        tk_Token := TK_GE;
        NextChr;
      end
      else if Ch = '>' then
      begin
        tk_Token := TK_RSHIFT;
        NextChr;
      end
      else
        tk_Token := TK_GT;
    '!':
      if Ch = '=' then
      begin
        tk_Token := TK_NE;
        NextChr;
      end
      else
        tk_Token := TK_NOT;
    '&':
      if Ch = '&' then
      begin
        tk_Token := TK_ANDLOGICAL;
        NextChr;
      end
      else
        tk_Token := TK_ANDBITWISE;
    '|':
      if Ch = '|' then
      begin
        tk_Token := TK_ORLOGICAL;
        NextChr;
      end
      else
        tk_Token := TK_ORBITWISE;
    '(':
      tk_Token := TK_LPAREN;
    ')':
      tk_Token := TK_RPAREN;
    '{':
      tk_Token := TK_LBRACE;
    '}':
      tk_Token := TK_RBRACE;
    '[':
      tk_Token := TK_LBRACKET;
    ']':
      tk_Token := TK_RBRACKET;
    ':':
      tk_Token := TK_COLON;
    ';':
      tk_Token := TK_SEMICOLON;
    ',':
      tk_Token := TK_COMMA;
    '.':
      tk_Token := TK_PERIOD;
    '#':
      tk_Token := TK_NUMBERSIGN;
    '^':
      tk_Token := TK_EORBITWISE;
    '~':
      tk_Token := TK_TILDE;
  else
    ERR_Exit(ERR_BAD_CHARACTER, True, '', []);
  end;
end;

//==============================================================================
//
// SkipComment
//
//==============================================================================
procedure SkipComment;
var
  first: boolean;
begin
  first := false;
  while Ch <> Chr(EOF_CHARACTER) do
  begin
    if first and (Ch = '/') then
      break;
    first := (Ch = '*');
    NextChr;
  end;
  NextChr;
end;

//==============================================================================
//
// SkipCPPComment
//
//==============================================================================
procedure SkipCPPComment;
var
  c: char;
begin
  while FilePtr < FileEnd do
  begin
    c := FilePtr^;
    inc(FilePtr);
    if c = #10 then
    begin
      inc(tk_Line);
      break;
    end;
  end;
  NextChr;
end;

//==============================================================================
//
// TK_NextToken
//
//==============================================================================
function TK_NextToken: integer;
var
  validToken: boolean;
begin
  if AlreadyGot then
  begin
    AlreadyGot := false;
    result := tk_Token;
    Exit;
  end;

  validToken := false;
  repeat
    while Ch = Chr(ASCII_SPACE) do
      NextChr;

    case ASCIIToChrCode[Ord(Ch)] of
      CHR_EOF:
        tk_Token := TK_EOF;
      CHR_LETTER:
        ProcessLetterToken;
      CHR_NUMBER:
        ProcessNumberToken;
      CHR_QUOTE:
        ProcessQuoteToken;
    else
      ProcessSpecialToken;
    end;

    if tk_Token = TK_STARTCOMMENT then
      SkipComment
    else if tk_Token = TK_CPPCOMMENT then
      SkipCPPComment
    else if (tk_Token = TK_EOF) and (NestDepth > 0) then
      PopNestedSource
    else
      validToken := true;
  until validToken;

  result := tk_Token;
end;

//==============================================================================
//
// TK_NextTokenMustBe
//
//==============================================================================
procedure TK_NextTokenMustBe(const token: integer; const error: integer);
begin
  if TK_NextToken <> token then
    ERR_Exit(error, True, '', []);
end;

//==============================================================================
//
// TK_TokenMustBe
//
//==============================================================================
procedure TK_TokenMustBe(const token: integer; const error: integer);
begin
  if tk_Token <> token then
    ERR_Exit(error, True, '', []);
end;

end.
