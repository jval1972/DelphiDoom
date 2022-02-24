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

unit acs_pcode;

interface

uses
  d_delphi,
  acs_common;

const
  OPEN_SCRIPTS_BASE = 1000;

const
  PCD_NOP = 0;
  PCD_TERMINATE = 1;
  PCD_SUSPEND = 2;
  PCD_PUSHNUMBER = 3;
  PCD_LSPEC1 = 4;
  PCD_LSPEC2 = 5;
  PCD_LSPEC3 = 6;
  PCD_LSPEC4 = 7;
  PCD_LSPEC5 = 8;
  PCD_LSPEC1DIRECT = 9;
  PCD_LSPEC2DIRECT = 10;
  PCD_LSPEC3DIRECT = 11;
  PCD_LSPEC4DIRECT = 12;
  PCD_LSPEC5DIRECT = 13;
  PCD_ADD = 14;
  PCD_SUBTRACT = 15;
  PCD_MULTIPLY = 16;
  PCD_DIVIDE = 17;
  PCD_MODULUS = 18;
  PCD_EQ = 19;
  PCD_NE = 20;
  PCD_LT = 21;
  PCD_GT = 22;
  PCD_LE = 23;
  PCD_GE = 24;
  PCD_ASSIGNSCRIPTVAR = 25;
  PCD_ASSIGNMAPVAR = 26;
  PCD_ASSIGNWORLDVAR = 27;
  PCD_PUSHSCRIPTVAR = 28;
  PCD_PUSHMAPVAR = 29;
  PCD_PUSHWORLDVAR = 30;
  PCD_ADDSCRIPTVAR = 31;
  PCD_ADDMAPVAR = 32;
  PCD_ADDWORLDVAR = 33;
  PCD_SUBSCRIPTVAR = 34;
  PCD_SUBMAPVAR = 35;
  PCD_SUBWORLDVAR = 36;
  PCD_MULSCRIPTVAR = 37;
  PCD_MULMAPVAR = 38;
  PCD_MULWORLDVAR = 39;
  PCD_DIVSCRIPTVAR = 40;
  PCD_DIVMAPVAR = 41;
  PCD_DIVWORLDVAR = 42;
  PCD_MODSCRIPTVAR = 43;
  PCD_MODMAPVAR = 44;
  PCD_MODWORLDVAR = 45;
  PCD_INCSCRIPTVAR = 46;
  PCD_INCMAPVAR = 47;
  PCD_INCWORLDVAR = 48;
  PCD_DECSCRIPTVAR = 49;
  PCD_DECMAPVAR = 50;
  PCD_DECWORLDVAR = 51;
  PCD_GOTO = 52;
  PCD_IFGOTO = 53;
  PCD_DROP = 54;
  PCD_DELAY = 55;
  PCD_DELAYDIRECT = 56;
  PCD_RANDOM = 57;
  PCD_RANDOMDIRECT = 58;
  PCD_THINGCOUNT = 59;
  PCD_THINGCOUNTDIRECT = 60;
  PCD_TAGWAIT = 61;
  PCD_TAGWAITDIRECT = 62;
  PCD_POLYWAIT = 63;
  PCD_POLYWAITDIRECT = 64;
  PCD_CHANGEFLOOR = 65;
  PCD_CHANGEFLOORDIRECT = 66;
  PCD_CHANGECEILING = 67;
  PCD_CHANGECEILINGDIRECT = 68;
  PCD_RESTART = 69;
  PCD_ANDLOGICAL = 70;
  PCD_ORLOGICAL = 71;
  PCD_ANDBITWISE = 72;
  PCD_ORBITWISE = 73;
  PCD_EORBITWISE = 74;
  PCD_NEGATELOGICAL = 75;
  PCD_LSHIFT = 76;
  PCD_RSHIFT = 77;
  PCD_UNARYMINUS = 78;
  PCD_IFNOTGOTO = 79;
  PCD_LINESIDE = 80;
  PCD_SCRIPTWAIT = 81;
  PCD_SCRIPTWAITDIRECT = 82;
  PCD_CLEARLINESPECIAL = 83;
  PCD_CASEGOTO = 84;
  PCD_BEGINPRINT = 85;
  PCD_ENDPRINT = 86;
  PCD_PRINTSTRING = 87;
  PCD_PRINTNUMBER = 88;
  PCD_PRINTCHARACTER = 89;
  PCD_PLAYERCOUNT = 90;
  PCD_GAMETYPE = 91;
  PCD_GAMESKILL = 92;
  PCD_TIMER = 93;
  PCD_SECTORSOUND = 94;
  PCD_AMBIENTSOUND = 95;
  PCD_SOUNDSEQUENCE = 96;
  PCD_SETLINETEXTURE = 97;
  PCD_SETLINEBLOCKING = 98;
  PCD_SETLINESPECIAL = 99;
  PCD_THINGSOUND = 100;
  PCD_ENDPRINTBOLD = 101;
  PCODE_COMMAND_COUNT = 102;

type
  scriptInfo_t = record
    number: integer;
    address: integer;
    argCount: integer;
  end;
  PscriptInfo_t = ^scriptInfo_t;

var
  pc_Address: integer;
  pc_Buffer: PByteArray;
  pc_BufferPtr: PByteArray;
  pc_ScriptCount: integer;

//==============================================================================
//
// PC_OpenObject
//
//==============================================================================
procedure PC_OpenObject(const name: string; const size: Integer; const flags: integer;
  const wadmode: boolean);

//==============================================================================
//
// PC_CloseObject
//
//==============================================================================
procedure PC_CloseObject(const wadmode: boolean);

//==============================================================================
//
// PC_GetObject
//
//==============================================================================
function PC_GetObject: Pointer;

//==============================================================================
//
// PC_AddScript
//
//==============================================================================
procedure PC_AddScript(const number: integer; const argCount: integer);

//==============================================================================
//
// PC_AppendCmd
//
//==============================================================================
procedure PC_AppendCmd(const command: integer);

//==============================================================================
//
// PC_Append
//
//==============================================================================
procedure PC_Append(const buffer: Pointer; const size: integer);

//==============================================================================
//
// PC_AppendString
//
//==============================================================================
procedure PC_AppendString(const str: string);

//==============================================================================
//
// PC_AppendLong
//
//==============================================================================
procedure PC_AppendLong(const v: U_LONG);

//==============================================================================
//
// PC_WriteLong
//
//==============================================================================
procedure PC_WriteLong(const v: U_LONG; const address: integer);

//==============================================================================
//
// PC_SkipLong
//
//==============================================================================
procedure PC_SkipLong;

implementation

uses
  acs_error,
  acs_misc,
  acs_strlist;

var
  BufferSize: integer;
  ObjectOpened: boolean = false;
  ScriptInfo: array[0..MAX_SCRIPT_COUNT - 1] of scriptInfo_t;
  ObjectName: string;
  ObjectFlags: integer;

const
  PCDNames: array[0..PCODE_COMMAND_COUNT - 1] of string[24] = (
    'PCD_NOP',
    'PCD_TERMINATE',
    'PCD_SUSPEND',
    'PCD_PUSHNUMBER',
    'PCD_LSPEC1',
    'PCD_LSPEC2',
    'PCD_LSPEC3',
    'PCD_LSPEC4',
    'PCD_LSPEC5',
    'PCD_LSPEC1DIRECT',
    'PCD_LSPEC2DIRECT',
    'PCD_LSPEC3DIRECT',
    'PCD_LSPEC4DIRECT',
    'PCD_LSPEC5DIRECT',
    'PCD_ADD',
    'PCD_SUBTRACT',
    'PCD_MULTIPLY',
    'PCD_DIVIDE',
    'PCD_MODULUS',
    'PCD_EQ',
    'PCD_NE',
    'PCD_LT',
    'PCD_GT',
    'PCD_LE',
    'PCD_GE',
    'PCD_ASSIGNSCRIPTVAR',
    'PCD_ASSIGNMAPVAR',
    'PCD_ASSIGNWORLDVAR',
    'PCD_PUSHSCRIPTVAR',
    'PCD_PUSHMAPVAR',
    'PCD_PUSHWORLDVAR',
    'PCD_ADDSCRIPTVAR',
    'PCD_ADDMAPVAR',
    'PCD_ADDWORLDVAR',
    'PCD_SUBSCRIPTVAR',
    'PCD_SUBMAPVAR',
    'PCD_SUBWORLDVAR',
    'PCD_MULSCRIPTVAR',
    'PCD_MULMAPVAR',
    'PCD_MULWORLDVAR',
    'PCD_DIVSCRIPTVAR',
    'PCD_DIVMAPVAR',
    'PCD_DIVWORLDVAR',
    'PCD_MODSCRIPTVAR',
    'PCD_MODMAPVAR',
    'PCD_MODWORLDVAR',
    'PCD_INCSCRIPTVAR',
    'PCD_INCMAPVAR',
    'PCD_INCWORLDVAR',
    'PCD_DECSCRIPTVAR',
    'PCD_DECMAPVAR',
    'PCD_DECWORLDVAR',
    'PCD_GOTO',
    'PCD_IFGOTO',
    'PCD_DROP',
    'PCD_DELAY',
    'PCD_DELAYDIRECT',
    'PCD_RANDOM',
    'PCD_RANDOMDIRECT',
    'PCD_THINGCOUNT',
    'PCD_THINGCOUNTDIRECT',
    'PCD_TAGWAIT',
    'PCD_TAGWAITDIRECT',
    'PCD_POLYWAIT',
    'PCD_POLYWAITDIRECT',
    'PCD_CHANGEFLOOR',
    'PCD_CHANGEFLOORDIRECT',
    'PCD_CHANGECEILING',
    'PCD_CHANGECEILINGDIRECT',
    'PCD_RESTART',
    'PCD_ANDLOGICAL',
    'PCD_ORLOGICAL',
    'PCD_ANDBITWISE',
    'PCD_ORBITWISE',
    'PCD_EORBITWISE',
    'PCD_NEGATELOGICAL',
    'PCD_LSHIFT',
    'PCD_RSHIFT',
    'PCD_UNARYMINUS',
    'PCD_IFNOTGOTO',
    'PCD_LINESIDE',
    'PCD_SCRIPTWAIT',
    'PCD_SCRIPTWAITDIRECT',
    'PCD_CLEARLINESPECIAL',
    'PCD_CASEGOTO',
    'PCD_BEGINPRINT',
    'PCD_ENDPRINT',
    'PCD_PRINTSTRING',
    'PCD_PRINTNUMBER',
    'PCD_PRINTCHARACTER',
    'PCD_PLAYERCOUNT',
    'PCD_GAMETYPE',
    'PCD_GAMESKILL',
    'PCD_TIMER',
    'PCD_SECTORSOUND',
    'PCD_AMBIENTSOUND',
    'PCD_SOUNDSEQUENCE',
    'PCD_SETLINETEXTURE',
    'PCD_SETLINEBLOCKING',
    'PCD_SETLINESPECIAL',
    'PCD_THINGSOUND',
    'PCD_ENDPRINTBOLD'
  );

//==============================================================================
//
// PC_OpenObject
//
//==============================================================================
procedure PC_OpenObject(const name: string; const size: Integer; const flags: integer;
  const wadmode: boolean);
begin
  if ObjectOpened then
    PC_CloseObject(wadmode);

  ObjectName := name;
  pc_Buffer := ACS_Alloc(size, ERR_ALLOC_PCODE_BUFFER);
  pc_BufferPtr := pc_Buffer;
  pc_Address := 0;
  ObjectFlags := flags;
  BufferSize := size;
  pc_ScriptCount := 0;
  ObjectOpened := True;
  PC_AppendString('ACS');
  PC_SkipLong; // Script table offset
end;

//==============================================================================
//
// PC_CloseObject
//
//==============================================================================
procedure PC_CloseObject(const wadmode: boolean);
var
  i: integer;
  info: PscriptInfo_t;
begin
  ACS_Message(MSG_DEBUG, '---- PC_CloseObject ----'#13#10, []);
  STR_WriteStrings;
  PC_WriteLong(U_LONG(pc_Address), 4);
  PC_AppendLong(U_LONG(pc_ScriptCount));
  for i := 0 to pc_ScriptCount - 1 do
  begin
    info := @ScriptInfo[i];
    ACS_Message(MSG_DEBUG, 'Script %d, address = %d, arg count = %d'#13#10,
      [info.number, info.address, info.argCount]);
    PC_AppendLong(U_LONG(info.number));
    PC_AppendLong(U_LONG(info.address));
    PC_AppendLong(U_LONG(info.argCount));
   end;
  STR_WriteList;
  if not wadmode then
    if not ACS_SaveFile(ObjectName, pc_Buffer, pc_Address) then
      ERR_Exit(ERR_SAVE_OBJECT_FAILED, False, '', []);
  ACS_Free(pointer(pc_Buffer));
end;

//==============================================================================
//
// PC_GetObject
//
//==============================================================================
function PC_GetObject: Pointer;
var
  i: integer;
  info: PscriptInfo_t;
begin
  ACS_Message(MSG_DEBUG, '---- PC_GetObject ----'#13#10, []);
  STR_WriteStrings;
  PC_WriteLong(U_LONG(pc_Address), 4);
  PC_AppendLong(U_LONG(pc_ScriptCount));
  for i := 0 to pc_ScriptCount - 1 do
  begin
    info := @ScriptInfo[i];
    ACS_Message(MSG_DEBUG, 'Script %d, address = %d, arg count = %d'#13#10,
      [info.number, info.address, info.argCount]);
    PC_AppendLong(U_LONG(info.number));
    PC_AppendLong(U_LONG(info.address));
    PC_AppendLong(U_LONG(info.argCount));
   end;
  STR_WriteList;
  Result := pc_Buffer;
end;

//==============================================================================
//
// PC_Append functions
//
//==============================================================================
procedure Append(const buffer: Pointer; const size: integer);
begin
  if pc_Address + size > BufferSize then
    ERR_Exit(ERR_PCODE_BUFFER_OVERFLOW, False, '', []);

  memcpy(pc_BufferPtr, buffer, size);
  pc_BufferPtr := @pc_BufferPtr[size];
  pc_Address := pc_Address + size;
end;

//==============================================================================
//
// PC_Append
//
//==============================================================================
procedure PC_Append(const buffer: Pointer; const size: integer);
begin
  ACS_Message(MSG_DEBUG, 'AD> %06d = (%d bytes)'#13#10, [pc_Address, size]);
  Append(buffer, size);
end;

//==============================================================================
//
// PC_AppendLong
//
//==============================================================================
procedure PC_AppendLong(const v: U_LONG);
var
  vl: U_LONG;
begin
  ACS_Message(MSG_DEBUG, 'AL> %06d = %d'#13#10, [pc_Address, v]);
  vl := ACS_LittleULONG(v);
  Append(@vl, SizeOf(U_LONG));
end;

//==============================================================================
//
// PC_AppendString
//
//==============================================================================
procedure PC_AppendString(const str: string);
var
  len, i: integer;
  pb: PByteArray;
begin
  len := Length(str) + 1;
  pb := malloc(len);
  for i := 1 to len - 1 do
   pb[i - 1] := Ord(str[i]);
  pb[len - 1] := 0;
  ACS_Message(MSG_DEBUG, 'AS> %06d = ''%s'' (%d bytes)'#13#10, [pc_Address, str, len]);
  Append(pb, len);
  memfree(pointer(pb), len);
end;

//==============================================================================
//
// PC_AppendCmd
//
//==============================================================================
procedure PC_AppendCmd(const command: integer);
var
  cl: integer;
begin
  ACS_Message(MSG_DEBUG, 'AC> %06d = #%d:%s'#13#10, [pc_Address, command, PCDNames[command]]);
  cl := ACS_LittleULONG(command);
  Append(@cl, SizeOf(U_LONG));
end;

//==============================================================================
// DoWrite
//
// PC_Write functions
//
//==============================================================================
procedure DoWrite(const buffer: Pointer; const size, address: integer);
begin
  if address + size > BufferSize then
    ERR_Exit(ERR_PCODE_BUFFER_OVERFLOW, false, '', []);

  memcpy(@pc_Buffer[address], buffer, size);
end;

//==============================================================================
//
// PC_Write
//
//==============================================================================
procedure PC_Write(const buffer: Pointer; const size, address: integer);
begin
  ACS_Message(MSG_DEBUG, 'WD> %06d = (%d bytes)'#13#10, [address, size]);
  DoWrite(buffer, size, address);
end;

//==============================================================================
//
// PC_WriteLong
//
//==============================================================================
procedure PC_WriteLong(const v: U_LONG; const address: integer);
var
  vl: U_LONG;
begin
  ACS_Message(MSG_DEBUG, 'WL> %06d = %d'#13#10, [address, v]);
  vl := ACS_LittleULONG(v);
  DoWrite(@vl, SizeOf(U_LONG), address);
end;

//==============================================================================
//
// PC_WriteString
//
//==============================================================================
procedure PC_WriteString(const str: string; const address: integer);
var
  len, i: integer;
  pb: PByteArray;
begin
  len := Length(str) + 1;
  pb := malloc(len);
  for i := 1 to len - 1 do
   pb[i - 1] := Ord(str[i]);
  pb[len - 1] := 0;
  ACS_Message(MSG_DEBUG, 'WS> %06d = ''%s'' (%d bytes)'#13#10, [address, str, len]);
  DoWrite(pb, len, address);
  memfree(Pointer(pb), len);
end;

//==============================================================================
//
// PC_WriteCmd
//
//==============================================================================
procedure PC_WriteCmd(const command: integer; const address: integer);
var
  cl: integer;
begin
  ACS_Message(MSG_DEBUG, 'WC> %06d = #%d:%s'#13#10, [address, command, PCDNames[command]]);
  cl := ACS_LittleULONG(command);
  DoWrite(@cl, SizeOf(U_LONG), address);
end;

//==============================================================================
//
// PC_Skip functions
//
//==============================================================================
procedure Skip(const size: integer);
begin
  if pc_Address + size > BufferSize then
    ERR_Exit(ERR_PCODE_BUFFER_OVERFLOW, False, '', []);

  pc_BufferPtr := @pc_BufferPtr[size];
  pc_Address := pc_Address + size;
end;

//==============================================================================
//
// PC_Skip
//
//==============================================================================
procedure PC_Skip(const size: integer);
begin
  ACS_Message(MSG_DEBUG, 'SD> %06d (skip %d bytes)'#13#10, [pc_Address, size]);
  Skip(size);
end;

//==============================================================================
//
// PC_SkipLong
//
//==============================================================================
procedure PC_SkipLong;
begin
  ACS_Message(MSG_DEBUG, 'SL> %06d (skip long)'#13#10, [pc_Address]);
  Skip(SizeOf(U_LONG));
end;

//==============================================================================
//
// PC_AddScript
//
//==============================================================================
procedure PC_AddScript(const number: integer; const argCount: integer);
var
  script: PscriptInfo_t;
begin
  if pc_ScriptCount = MAX_SCRIPT_COUNT then
    ERR_Exit(ERR_TOO_MANY_SCRIPTS, true, '', []);

  script := @ScriptInfo[pc_ScriptCount];
  script.number := number;
  script.address := pc_Address;
  script.argCount := argCount;
  inc(pc_ScriptCount);
end;

end.
