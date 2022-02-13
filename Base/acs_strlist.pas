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

unit acs_strlist;

interface

type
  stringInfo_t = record
    name: string;
    address: integer;
  end;

//==============================================================================
//
// STR_Init
//
//==============================================================================
procedure STR_Init;

//==============================================================================
//
// STR_WriteStrings
//
//==============================================================================
procedure STR_WriteStrings;

//==============================================================================
//
// STR_WriteList
//
//==============================================================================
procedure STR_WriteList;

//==============================================================================
//
// STR_Find
//
//==============================================================================
function STR_Find(const name: string): Integer;

implementation

uses
  acs_common,
  acs_error,
  acs_misc,
  acs_pcode;

var
  str_StringCount: integer;
  StringInfo: array[0..MAX_STRINGS - 1] of stringInfo_t;

//==============================================================================
//
// STR_Init
//
//==============================================================================
procedure STR_Init;
begin
  str_StringCount := 0;
end;

//==============================================================================
//
// STR_Find
//
//==============================================================================
function STR_Find(const name: string): Integer;
var
  i: integer;
begin
  for i := 0 to str_StringCount - 1 do
  begin
    if StringInfo[i].name = name then
    begin
      result := i;
      exit;
    end;
  end;
  // Add to list
  if str_StringCount = MAX_STRINGS then
    ERR_Exit(ERR_TOO_MANY_STRINGS, true, 'Current maximum: %d',
      [MAX_STRINGS]);

  ACS_Message(MSG_DEBUG, 'Adding string %d:'#13#10'  ''%s'''#13#10,
    [str_StringCount, name]);
  StringInfo[str_StringCount].name := name;
  result := str_StringCount;
  Inc(str_StringCount);
end;

//==============================================================================
//
// STR_WriteStrings
//
// Writes all the strings to the p-code buffer.
//
//==============================================================================
procedure STR_WriteStrings;
var
  i: integer;
  pad: U_LONG;
begin
  ACS_Message(MSG_DEBUG, '---- STR_WriteStrings ----'#13#10, []);
  for i := 0 to str_StringCount - 1 do
  begin
    StringInfo[i].address := pc_Address;
    PC_AppendString(StringInfo[i].name);
  end;
  if pc_Address mod 4 <> 0 then
  begin  // Need to align
    pad := 0;
    PC_Append(@pad, 4 - (pc_Address mod 4));
  end;
end;

//==============================================================================
//
// STR_WriteList
//
//==============================================================================
procedure STR_WriteList;
var
  i: integer;
begin
  ACS_Message(MSG_DEBUG, '---- STR_WriteList ----'#13#10, []);
  PC_AppendLong(str_StringCount);
  for i := 0 to str_StringCount - 1 do
    PC_AppendLong(StringInfo[i].address);
end;

end.

