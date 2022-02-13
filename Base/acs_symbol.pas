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

unit acs_symbol;

interface

type
  symbolType_t = (
    SY_LABEL,
    SY_SCRIPTVAR,
    SY_MAPVAR,
    SY_WORLDVAR,
    SY_SPECIAL,
    SY_CONSTANT,
    SY_INTERNFUNC
  );

type
  symVar_t = record
    index: integer;
  end;

  symLabel_t = record
    address: integer;
  end;

  symSpecial_t = record
    value: integer;
    argCount: integer;
  end;

  symConstant_t = record
    value: integer;
  end;

  symInternFunc_t = record
    directCommand: integer;
    stackCommand: integer;
    argCount: integer;
    hasReturnValue: boolean;
  end;

  symbolInfo_t = record
  case integer of
    0: (svar: symVar_t);
    1: (slabel: symLabel_t);
    2: (special: symSpecial_t);
    3: (constant: symConstant_t);
    4: (internFunc: symInternFunc_t);
  end;

  PPsymbolNode_t = ^PsymbolNode_t;
  PsymbolNode_t = ^symbolNode_t;
  symbolNode_t = record
    left, right: PsymbolNode_t;
    name: string;
    typ: symbolType_t;
    info: symbolInfo_t;
  end;

  PinternFuncDef_t = ^internFuncDef_t;
  internFuncDef_t = record
    name: string;
    directCommand: integer;
    stackCommand: integer;
    argCount: integer;
    hasReturnValue: boolean;
  end;

//==============================================================================
//
// SY_Init
//
//==============================================================================
procedure SY_Init;

//==============================================================================
//
// SY_FreeLocals
//
//==============================================================================
procedure SY_FreeLocals;

//==============================================================================
//
// SY_FreeGlobals
//
//==============================================================================
procedure SY_FreeGlobals;

//==============================================================================
//
// SY_InsertGlobal
//
//==============================================================================
function SY_InsertGlobal(const name: string; const typ: symbolType_t): PsymbolNode_t;

//==============================================================================
//
// SY_InsertLocal
//
//==============================================================================
function SY_InsertLocal(const name: string; const typ: symbolType_t): PsymbolNode_t;

//==============================================================================
//
// SY_Find
//
//==============================================================================
function SY_Find(const name: string): PsymbolNode_t;

//==============================================================================
//
// SY_FindGlobal
//
//==============================================================================
function SY_FindGlobal(const name: string): PsymbolNode_t;

//==============================================================================
//
// SY_FindLocal
//
//==============================================================================
function SY_FindLocal(const name: string): PsymbolNode_t;

implementation

uses
  d_delphi,
  acs_error,
  acs_misc,
  acs_pcode;

var
  LocalRoot: PsymbolNode_t;
  GlobalRoot: PsymbolNode_t;

const
  NUMINTERLANFUNCTIONS = 22;

const
  InternalFunctions: array[0..NUMINTERLANFUNCTIONS - 1] of internFuncDef_t = (
    (name: 'tagwait';           directCommand: PCD_TAGWAITDIRECT;       stackCommand: PCD_TAGWAIT;          argCount: 1; hasReturnValue: false),
    (name: 'polywait';          directCommand: PCD_POLYWAITDIRECT;      stackCommand: PCD_POLYWAIT;         argCount: 1; hasReturnValue: false),
    (name: 'scriptwait';        directCommand: PCD_SCRIPTWAITDIRECT;    stackCommand: PCD_SCRIPTWAIT;       argCount: 1; hasReturnValue: false),
    (name: 'delay';             directCommand: PCD_DELAYDIRECT;         stackCommand: PCD_DELAY;            argCount: 1; hasReturnValue: false),
    (name: 'random';            directCommand: PCD_RANDOMDIRECT;        stackCommand: PCD_RANDOM;           argCount: 2; hasReturnValue: true),
    (name: 'thingcount';        directCommand: PCD_THINGCOUNTDIRECT;    stackCommand: PCD_THINGCOUNT;       argCount: 2; hasReturnValue: true),
    (name: 'changefloor';       directCommand: PCD_CHANGEFLOORDIRECT;   stackCommand: PCD_CHANGEFLOOR;      argCount: 2; hasReturnValue: false),
    (name: 'changeceiling';     directCommand: PCD_CHANGECEILINGDIRECT; stackCommand: PCD_CHANGECEILING;    argCount: 2; hasReturnValue: false),
    (name: 'lineside';          directCommand: PCD_NOP;                 stackCommand: PCD_LINESIDE;         argCount: 0; hasReturnValue: true),
    (name: 'clearlinespecial';  directCommand: PCD_NOP;                 stackCommand: PCD_CLEARLINESPECIAL; argCount: 0; hasReturnValue: false),
    (name: 'playercount';       directCommand: PCD_NOP;                 stackCommand: PCD_PLAYERCOUNT;      argCount: 0; hasReturnValue: true),
    (name: 'gametype';          directCommand: PCD_NOP;                 stackCommand: PCD_GAMETYPE;         argCount: 0; hasReturnValue: true),
    (name: 'gameskill';         directCommand: PCD_NOP;                 stackCommand: PCD_GAMESKILL;        argCount: 0; hasReturnValue: true),
    (name: 'timer';             directCommand: PCD_NOP;                 stackCommand: PCD_TIMER;            argCount: 0; hasReturnValue: true),
    (name: 'sectorsound';       directCommand: PCD_NOP;                 stackCommand: PCD_SECTORSOUND;      argCount: 2; hasReturnValue: false),
    (name: 'ambientsound';      directCommand: PCD_NOP;                 stackCommand: PCD_AMBIENTSOUND;     argCount: 2; hasReturnValue: false),
    (name: 'soundsequence';     directCommand: PCD_NOP;                 stackCommand: PCD_SOUNDSEQUENCE;    argCount: 1; hasReturnValue: false),
    (name: 'setlinetexture';    directCommand: PCD_NOP;                 stackCommand: PCD_SETLINETEXTURE;   argCount: 4; hasReturnValue: false),
    (name: 'setlineblocking';   directCommand: PCD_NOP;                 stackCommand: PCD_SETLINEBLOCKING;  argCount: 2; hasReturnValue: false),
    (name: 'setlinespecial';    directCommand: PCD_NOP;                 stackCommand: PCD_SETLINESPECIAL;   argCount: 7; hasReturnValue: false),
    (name: 'thingsound';        directCommand: PCD_NOP;                 stackCommand: PCD_THINGSOUND;       argCount: 3; hasReturnValue: false),
    (name: '';                  directCommand: PCD_NOP;                 stackCommand: PCD_NOP;              argCount: 0; hasReturnValue: false)
  );

const
  SymbolTypeNames: array[symbolType_t] of string[14] = (
    'SY_LABEL',
    'SY_SCRIPTVAR',
    'SY_MAPVAR',
    'SY_WORLDVAR',
    'SY_SPECIAL',
    'SY_CONSTANT',
    'SY_INTERNFUNC'
  );

//==============================================================================
//
// SY_Init
//
//==============================================================================
procedure SY_Init;
var
  sym: PsymbolNode_t;
  def: PinternFuncDef_t;
begin
  LocalRoot := nil;
  GlobalRoot := nil;

  def := @InternalFunctions[0];
  while def.name <> '' do
  begin
    sym := SY_InsertGlobal(def.name, SY_INTERNFUNC);
    sym.info.internFunc.directCommand := def.directCommand;
    sym.info.internFunc.stackCommand := def.stackCommand;
    sym.info.internFunc.argCount := def.argCount;
    sym.info.internFunc.hasReturnValue := def.hasReturnValue;
    inc(def);
  end;
end;

//==============================================================================
//
// SY_Find
//
//==============================================================================
function SY_Find(const name: string): PsymbolNode_t;
var
  node: PsymbolNode_t;
begin
  node := SY_FindGlobal(name);
  if node = nil then
  begin
    result := SY_FindLocal(name);
    exit;
  end;
  result := node;
end;

//==============================================================================
//
// DoFind
//
// Find
//
//==============================================================================
function DoFind(const name: string; const root: PsymbolNode_t): PsymbolNode_t;
var
  compare: integer;
  node: PsymbolNode_t;
begin
  node := root;
  while node <> nil do
  begin
    compare := strcmp(name, node.name);
    if compare = 0 then
    begin
      result := node;
      exit;
    end;
    if compare < 0 then
      node := node.left
    else
      node :=  node.right;
  end;
  result := nil;
end;

//==============================================================================
//
// SY_FindGlobal
//
//==============================================================================
function SY_FindGlobal(const name: string): PsymbolNode_t;
begin
  result := DoFind(name, GlobalRoot);
end;

//==============================================================================
//
// SY_Findlocal
//
//==============================================================================
function SY_FindLocal(const name: string): PsymbolNode_t;
begin
  result := DoFind(name, LocalRoot);
end;

//==============================================================================
//
// DoInsert
//
// Insert
//
//==============================================================================
function DoInsert(const name: string; const typ: symbolType_t; root: PPsymbolNode_t): PsymbolNode_t;
var
  compare: integer;
  newNode: PsymbolNode_t;
  node: PsymbolNode_t;
begin
  newNode := ACS_Alloc(SizeOf(symbolNode_t), ERR_NO_SYMBOL_MEM);
  newNode.name := name;
  newNode.left := nil;
  newNode.right := nil;
  newNode.typ := typ;
  node := root^;
  while node <> nil do
  begin
    compare := strcmp(name, node.name);
    if compare < 0 then
      root := @node.left
    else
      root := @node.right;
    node := root^;
  end;
  root^ := newNode;
  result := newNode;
end;

//==============================================================================
//
// SY_InsertLocal
//
//==============================================================================
function SY_InsertLocal(const name: string; const typ: symbolType_t): PsymbolNode_t;
begin
  ACS_Message(MSG_DEBUG, 'Inserting local identifier: %s (%s)'#13#10,
    [name, SymbolTypeNames[typ]]);
  result := DoInsert(name, typ, @LocalRoot);
end;

//==============================================================================
//
// SY_InsertGlobal
//
//==============================================================================
function SY_InsertGlobal(const name: string; const typ: symbolType_t): PsymbolNode_t;
begin
  ACS_Message(MSG_DEBUG, 'Inserting global identifier: %s (%s)'#13#10,
    [name, SymbolTypeNames[typ]]);
  result := DoInsert(name, typ, @GlobalRoot);
end;

//==============================================================================
//
// FreeNodes
//
//==============================================================================
procedure FreeNodes(var root: PsymbolNode_t);
begin
  if root = nil then
    exit;

  FreeNodes(root.left);
  FreeNodes(root.right);
  ACS_Free(Pointer(root));
end;

//==============================================================================
//
// SY_FreeLocals
//
//==============================================================================
procedure SY_FreeLocals;
begin
  ACS_Message(MSG_DEBUG, 'Freeing local identifiers'#13#10, []);
  FreeNodes(LocalRoot);
  LocalRoot := nil;
end;

//==============================================================================
//
// SY_FreeGlobals
//
//==============================================================================
procedure SY_FreeGlobals;
begin
  ACS_Message(MSG_DEBUG, 'Freeing global identifiers'#13#10, []);
  FreeNodes(GlobalRoot);
  GlobalRoot := nil;
end;

end.
