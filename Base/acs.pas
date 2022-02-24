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

unit acs;

interface

uses
  d_delphi;

//==============================================================================
//
// acc_isscriptlump
//
//==============================================================================
function acc_isscriptlump(const lump: integer): boolean;

//==============================================================================
//
// acc_isbehaviorlump
//
//==============================================================================
function acc_isbehaviorlump(const lump: integer): boolean;

//==============================================================================
//
// acc_main
//
//==============================================================================
function acc_main(const argc: integer; const argv: TDStringList): Pointer;

var
  acs_BigEndianHost: boolean;
  acs_VerboseMode: boolean;
  acs_DebugMode: boolean;
  acs_SourceFileName: string;
  ObjectFileName: string;
  ArgCount: integer;
  ArgVector: TDStringList;

implementation

uses
  acs_common,
  acs_error,
  acs_misc,
  acs_parse,
  acs_pcode,
  acs_strlist,
  acs_symbol,
  acs_token,
  w_wad;

const
  ACC_VERSION_TEXT = '1.10';
  ACC_COPYRIGHT_YEARS_TEXT = '1995';

//==============================================================================
//
// one_or_many
//
//==============================================================================
function one_or_many(const x: integer; const ifone, ifmany: string): string;
begin
  if x = 1 then
    result := ifone
  else
    result := ifmany;
end;

//==============================================================================
//
// acc_isscriptlump
//
//==============================================================================
function acc_isscriptlump(const lump: integer): boolean;
begin
  if IsIntegerInRange(lump, 0, W_NumLumps - 1) then
    if (W_GetNameForNum(lump) = 'SCRIPT') or (W_GetNameForNum(lump) = 'SCRIPTS') then
    begin
      Result := W_LumpLength(lump) > 8;
      Exit;
    end;
  Result := False;
end;

//==============================================================================
//
// acc_isbehaviorlump
//
//==============================================================================
function acc_isbehaviorlump(const lump: integer): boolean;
begin
  if IsIntegerInRange(lump, 0, W_NumLumps - 1) then
    if W_GetNameForNum(lump) = 'BEHAVIOR' then
    begin
      Result := True;
      Exit;
    end;
  Result := False;
end;

//==============================================================================
//
// DisplayBanner
//
//==============================================================================
procedure DisplayBanner;
begin
  printf(#13#10'ACC Version ' + ACC_VERSION_TEXT + ' by Ben Gokey'#13#10);
  printf('Copyright (c) ' + ACC_COPYRIGHT_YEARS_TEXT + ' Raven Software, Corp.'#13#10);
end;

//==============================================================================
//
// DisplayUsage
//
//==============================================================================
procedure DisplayUsage;
begin
  printf('Usage: ACCP [options] source[.acs] [object[.o]]'#13#10);
  printf('-b       Set host native byte order to big endian'#13#10);
  printf('-l       Set host native byte order to little endian'#13#10);
  printf('-d[file] Output debugging information');
  Halt(1);
end;

//==============================================================================
//
// ProcessArgs
//
//==============================================================================
procedure ProcessArgs;
var
  i: integer;
  count: integer;
  txt: string;
  option: char;
begin
  count := 0;
  for i := 0 to ArgCount - 1 do
  begin
    txt := ArgVector[i];
    if Pos('-', txt) = 1 then
    begin
      Delete(txt, 1, 1);
      if txt = '' then
      begin
        DisplayUsage;
      end;
      option := toupper(txt[1]);
      case option of
        'B':
          begin
            if Length(txt) <> 1 then
              DisplayUsage;
            acs_BigEndianHost := True;
          end;
        'L':
          begin
            if Length(txt) <> 1 then
              DisplayUsage;
            acs_BigEndianHost := False;
          end;
        'D':
          begin
            acs_DebugMode := True;
            acs_VerboseMode := True;
          end;
      else
        DisplayUsage;
      end;
      continue;
    end;
    Inc(count);
    case count of
      1:
        begin
          acs_SourceFileName := txt;
          ACS_SuggestFileExt(acs_SourceFileName, '.acs');
        end;
      2:
        begin
          ObjectFileName := txt;
          ACS_SuggestFileExt(ObjectFileName, '.o');
        end;
    else
      DisplayUsage;
    end;
  end;

  if count = 0 then
    DisplayUsage;

  if count = 1 then
  begin
    ObjectFileName := acs_SourceFileName;
    ACS_StripFileExt(ObjectFileName);
    ACS_SuggestFileExt(ObjectFileName, '.o');
  end;
end;

//==============================================================================
//
// Init
//
//==============================================================================
procedure Init;
begin
  acs_BigEndianHost := false;
  acs_VerboseMode := true;
  acs_DebugMode := false;
  TK_Init;
  SY_Init;
  STR_Init;
  ProcessArgs;
  ACS_Message(MSG_NORMAL, 'Host byte order: %s endian'#13#10,
    [decide(acs_BigEndianHost, 'BIG', 'LITTLE')]);
end;

//==============================================================================
//
// acc_main
//
//==============================================================================
function acc_main(const argc: integer; const argv: TDStringList): Pointer;
var
  slump, sext: string;
  lump: integer;
  wadmode: boolean;
begin
  ArgCount := argc;
  ArgVector := argv;

  DisplayBanner;
  Init;

  wadmode := false;
  splitstring_ch(acs_SourceFileName, slump, sext, '.');
  if StrIsLongWord(slump) then
  begin
    lump := atoi(slump);
    if acc_isscriptlump(lump) then
      wadmode := true;
  end;
  if wadmode then
    TK_OpenLump(atoi(slump))
  else
    TK_OpenSource(acs_SourceFileName);

  PC_OpenObject(ObjectFileName, DEFAULT_OBJECT_SIZE, 0, wadmode);
  PA_Parse;
  if wadmode then
    Result := PC_GetObject
  else
  begin
    PC_CloseObject(wadmode);
    Result := nil;
  end;

  TK_CloseSource;

  ACS_Message(MSG_NORMAL, #13#10'''%s'':'#13#10'  %d %s (%d included),',
    [acs_SourceFileName, tk_Line, one_or_many(tk_Line, 'line', 'lines'),
     tk_IncludedLines]);

  ACS_Message(MSG_NORMAL, ' %d %s (%d open), %d functions'#13#10,
    [pa_ScriptCount, one_or_many(pa_ScriptCount, 'script', 'scripts'),
     pa_OpenScriptCount, 0]);

  ACS_Message(MSG_NORMAL, '  %d world %s, %d map %s'#13#10,
    [pa_WorldVarCount, one_or_many(pa_WorldVarCount, 'variable', 'variables'),
     pa_MapVarCount, one_or_many(pa_MapVarCount, 'variable', 'variables')]);

  ACS_Message(MSG_NORMAL, '  object ''%s'': %d bytes'#13#10,
    [ObjectFileName, pc_Address]);
  ERR_RemoveErrorFile;
end;

end.
