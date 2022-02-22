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

unit acs_parse;

interface

//==============================================================================
//
// PA_Parse
//
//==============================================================================
procedure PA_Parse;

var
  pa_ScriptCount: integer;
  pa_OpenScriptCount: integer;
  pa_MapVarCount: integer;
  pa_WorldVarCount: integer;

implementation

uses
  d_delphi,
  acs_common,
  acs_error,
  acs_misc,
  acs_pcode,
  acs_strlist,
  acs_symbol,
  acs_token;

const
  MAX_STATEMENT_DEPTH = 128;
  MAX_BREAK = 128;
  MAX_CONTINUE = 128;
  MAX_CASE = 128;
  EXPR_STACK_DEPTH = 64;

type
  statement_t = (
    STMT_SCRIPT,
    STMT_IF,
    STMT_ELSE,
    STMT_DO,
    STMT_WHILEUNTIL,
    STMT_SWITCH,
    STMT_FOR
  );

type
  breakInfo_t = record
    level: integer;
    addressPtr: integer;
  end;

type
  continueInfo_t = record
    level: integer;
    addressPtr: integer;
  end;

type
  caseInfo_t = record
    level: integer;
    value: integer;
    isDefault: boolean;
    address: integer;
  end;
  PcaseInfo_t = ^caseInfo_t;

var
  ScriptVarCount: integer;
  StatementHistory: array[0..MAX_STATEMENT_DEPTH - 1] of statement_t;
  StatementIndex: integer;
  BreakInfo: array[0..MAX_BREAK - 1] of breakInfo_t;
  BreakIndex: integer;
  ContinueInfo: array[0..MAX_CONTINUE - 1] of continueInfo_t;
  ContinueIndex: integer;
  CaseInfo: array[0..MAX_CASE - 1] of caseInfo_t;
  CaseIndex: integer;
  StatementLevel: integer;
  ExprStack: array[0..EXPR_STACK_DEPTH - 1] of integer;
  ExprStackIndex: integer;
  ConstantExpression: boolean;

const
  AdjustStmtLevel: array[statement_t] of integer = (
    0,  // STMT_SCRIPT
    0,  // STMT_IF
    0,  // STMT_ELSE
    1,  // STMT_DO
    1,  // STMT_WHILEUNTIL
    1,  // STMT_SWITCH
    1   // STMT_FOR
  );

  IsBreakRoot: array[statement_t] of boolean = (
    false,  // STMT_SCRIPT
    false,  // STMT_IF
    false,  // STMT_ELSE
    true,   // STMT_DO
    true,   // STMT_WHILEUNTIL
    true,   // STMT_SWITCH
    true    // STMT_FOR
  );

  IsContinueRoot: array[statement_t] of boolean = (
    false,  // STMT_SCRIPT
    false,  // STMT_IF
    false,  // STMT_ELSE
    true,   // STMT_DO
    true,   // STMT_WHILEUNTIL
    false,  // STMT_SWITCH
    true    // STMT_FOR
  );

  LevFOps: array[0..2] of integer = (
    TK_EQ,
    TK_NE,
    TK_NONE
  );

  LevGOps: array[0..4] of integer = (
    TK_LT,
    TK_LE,
    TK_GT,
    TK_GE,
    TK_NONE
  );

  LevHOps: array[0..2] of integer = (
    TK_LSHIFT,
    TK_RSHIFT,
    TK_NONE
  );

  LevIOps: array[0..2] of integer = (
    TK_PLUS,
    TK_MINUS,
    TK_NONE
  );

  LevJOps: array[0..3] of integer = (
    TK_ASTERISK,
    TK_SLASH,
    TK_PERCENT,
    TK_NONE
  );

  AssignOps: array[0..6] of integer = (
    TK_ASSIGN,
    TK_ADDASSIGN,
    TK_SUBASSIGN,
    TK_MULASSIGN,
    TK_DIVASSIGN,
    TK_MODASSIGN,
    TK_NONE
  );

//==============================================================================
//
// OuterScript
//
//==============================================================================
procedure OuterScript; forward;

//==============================================================================
//
// OuterMapVar
//
//==============================================================================
procedure OuterMapVar; forward;

//==============================================================================
//
// OuterWorldVar
//
//==============================================================================
procedure OuterWorldVar; forward;

//==============================================================================
//
// OuterSpecialDef
//
//==============================================================================
procedure OuterSpecialDef; forward;

//==============================================================================
//
// OuterDefine
//
//==============================================================================
procedure OuterDefine; forward;

//==============================================================================
//
// OuterInclude
//
//==============================================================================
procedure OuterInclude; forward;

//==============================================================================
//
// ProcessStatement
//
//==============================================================================
function ProcessStatement(const owner: statement_t): boolean; forward;

//==============================================================================
//
// LeadingCompoundStatement
//
//==============================================================================
procedure LeadingCompoundStatement(const owner: statement_t); forward;

//==============================================================================
//
// LeadingVarDeclare
//
//==============================================================================
procedure LeadingVarDeclare; forward;

//==============================================================================
//
// LeadingLineSpecial
//
//==============================================================================
procedure LeadingLineSpecial; forward;

//==============================================================================
//
// LeadingIdentifier
//
//==============================================================================
procedure LeadingIdentifier; forward;

//==============================================================================
//
// LeadingInternFunc
//
//==============================================================================
procedure LeadingInternFunc(const sym: PsymbolNode_t); forward;

//==============================================================================
//
// ProcessInternFunc
//
//==============================================================================
procedure ProcessInternFunc(const sym: PsymbolNode_t); forward;

//==============================================================================
//
// LeadingPrint
//
//==============================================================================
procedure LeadingPrint; forward;

//==============================================================================
//
// LeadingIf
//
//==============================================================================
procedure LeadingIf; forward;

//==============================================================================
//
// LeadingFor
//
//==============================================================================
procedure LeadingFor; forward;

//==============================================================================
//
// LeadingWhileUntil
//
//==============================================================================
procedure LeadingWhileUntil; forward;

//==============================================================================
//
// LeadingDo
//
//==============================================================================
procedure LeadingDo; forward;

//==============================================================================
//
// LeadingSwitch
//
//==============================================================================
procedure LeadingSwitch; forward;

//==============================================================================
//
// LeadingCase
//
//==============================================================================
procedure LeadingCase; forward;

//==============================================================================
//
// LeadingDefault
//
//==============================================================================
procedure LeadingDefault; forward;

//==============================================================================
//
// PushCase
//
//==============================================================================
procedure PushCase(const value: integer; const isDefault: boolean); forward;

//==============================================================================
//
// GetCaseInfo
//
//==============================================================================
function GetCaseInfo: PcaseInfo_t; forward;

//==============================================================================
//
// DefaultInCurrent
//
//==============================================================================
function DefaultInCurrent: boolean; forward;

//==============================================================================
//
// LeadingBreak
//
//==============================================================================
procedure LeadingBreak; forward;

//==============================================================================
//
// PushBreak
//
//==============================================================================
procedure PushBreak; forward;

//==============================================================================
//
// WriteBreaks
//
//==============================================================================
procedure WriteBreaks; forward;

//==============================================================================
//
// BreakAncestor
//
//==============================================================================
function BreakAncestor: boolean; forward;

//==============================================================================
//
// LeadingContinue
//
//==============================================================================
procedure LeadingContinue; forward;

//==============================================================================
//
// PushContinue
//
//==============================================================================
procedure PushContinue; forward;

//==============================================================================
//
// WriteContinues
//
//==============================================================================
procedure WriteContinues(const address: integer); forward;

//==============================================================================
//
// ContinueAncestor
//
//==============================================================================
function ContinueAncestor: boolean; forward;

//==============================================================================
//
// LeadingVarAssign
//
//==============================================================================
procedure LeadingVarAssign(sym: PsymbolNode_t); forward;

//==============================================================================
//
// GetAssignPCD
//
//==============================================================================
function GetAssignPCD(token: integer; symbol: symbolType_t): Integer; forward;

//==============================================================================
//
// LeadingSuspend
//
//==============================================================================
procedure LeadingSuspend; forward;

//==============================================================================
//
// LeadingTerminate
//
//==============================================================================
procedure LeadingTerminate; forward;

//==============================================================================
//
// EvalConstExpression
//
//==============================================================================
function EvalConstExpression: Integer; forward;

//==============================================================================
//
// LeadingRestart
//
//==============================================================================
procedure LeadingRestart; forward;

//==============================================================================
//
// EvalExpression
//
//==============================================================================
procedure EvalExpression; forward;

//==============================================================================
//
// ExprLevA
//
//==============================================================================
procedure ExprLevA; forward;

//==============================================================================
//
// ExprLevB
//
//==============================================================================
procedure ExprLevB; forward;

//==============================================================================
//
// ExprLevC
//
//==============================================================================
procedure ExprLevC; forward;

//==============================================================================
//
// ExprLevD
//
//==============================================================================
procedure ExprLevD; forward;

//==============================================================================
//
// ExprLevE
//
//==============================================================================
procedure ExprLevE; forward;

//==============================================================================
//
// ExprLevF
//
//==============================================================================
procedure ExprLevF; forward;

//==============================================================================
//
// ExprLevG
//
//==============================================================================
procedure ExprLevG; forward;

//==============================================================================
//
// ExprLevH
//
//==============================================================================
procedure ExprLevH; forward;

//==============================================================================
//
// ExprLevI
//
//==============================================================================
procedure ExprLevI; forward;

//==============================================================================
//
// ExprLevJ
//
//==============================================================================
procedure ExprLevJ; forward;

//==============================================================================
//
// ExprFactor
//
//==============================================================================
procedure ExprFactor; forward;

//==============================================================================
//
// ConstExprFactor
//
//==============================================================================
procedure ConstExprFactor; forward;

//==============================================================================
//
// SendExprCommand
//
//==============================================================================
procedure SendExprCommand(const pcd: integer); forward;

//==============================================================================
//
// PushExStk
//
//==============================================================================
procedure PushExStk(const value: integer); forward;

//==============================================================================
//
// PopExStk
//
//==============================================================================
function PopExStk: integer; forward;

//==============================================================================
//
// TokenToPCD
//
//==============================================================================
function TokenToPCD(token: integer): integer; forward;

//==============================================================================
//
// GetPushVarPCD
//
//==============================================================================
function GetPushVarPCD(const symType: symbolType_t): Integer; forward;

//==============================================================================
//
// GetIncDecPCD
//
//==============================================================================
function GetIncDecPCD(token: integer; symbol: symbolType_t): Integer; forward;

//==============================================================================
//
// DemandSymbol
//
//==============================================================================
function DemandSymbol(const name: string): PsymbolNode_t; forward;

//==============================================================================
//
// OuterScript
//
//==============================================================================
procedure OuterScript;
var
  scriptNumber: integer;
  sym: PsymbolNode_t;
begin
  ACS_Message(MSG_DEBUG, '---- OuterScript ----'#13#10, []);
  BreakIndex := 0;
  CaseIndex := 0;
  StatementLevel := 0;
  ScriptVarCount := 0;
  SY_FreeLocals;
  TK_NextToken;
  scriptNumber := EvalConstExpression;
  ACS_Message(MSG_DEBUG, 'Script number: %d'#13#10, [scriptNumber]);
  if tk_Token = TK_LPAREN then
  begin
    if TK_NextToken = TK_VOID then
      TK_NextTokenMustBe(TK_RPAREN, ERR_MISSING_RPAREN)
    else
    begin
      TK_Undo;
      repeat
        TK_NextTokenMustBe(TK_INT, ERR_BAD_VAR_TYPE);
        TK_NextTokenMustBe(TK_IDENTIFIER, ERR_INVALID_IDENTIFIER);
        if SY_FindLocal(tk_Text) <> nil then
        begin  // Redefined
          ERR_Exit(ERR_REDEFINED_IDENTIFIER, true,
            'Identifier: %s', [tk_Text]);
        end;
        sym := SY_InsertLocal(tk_Text, SY_SCRIPTVAR);
        sym.info.svar.index := ScriptVarCount;
        Inc(ScriptVarCount);
        TK_NextToken;
      until tk_Token <> TK_COMMA;
      TK_TokenMustBe(TK_RPAREN, ERR_MISSING_RPAREN);
      if ScriptVarCount > 3 then
        ERR_Exit(ERR_TOO_MANY_SCRIPT_ARGS, True, '', []);
    end;
    ACS_Message(MSG_DEBUG, 'Script type: CLOSED (%d %s)'#13#10,
      [ScriptVarCount, decide(ScriptVarCount = 1, 'arg', 'args')]);
  end
  else if tk_Token = TK_OPEN then
  begin
    ACS_Message(MSG_DEBUG, 'Script type: OPEN'#13#10, []);
    scriptNumber := scriptNumber + OPEN_SCRIPTS_BASE;
    Inc(pa_OpenScriptCount);
  end
  else
    ERR_Exit(ERR_BAD_SCRIPT_DECL, true, '', []);
  PC_AddScript(scriptNumber, ScriptVarCount);
  TK_NextToken;
  if not ProcessStatement(STMT_SCRIPT) then
    ERR_Exit(ERR_INVALID_STATEMENT, True, '', []);
  PC_AppendCmd(PCD_TERMINATE);
  inc(pa_ScriptCount);
end;

//==============================================================================
//
// OuterMapVar
//
//==============================================================================
procedure OuterMapVar;
var
  sym: PsymbolNode_t;
begin
  ACS_Message(MSG_DEBUG, '---- OuterMapVar ----'#13#10, []);
  repeat
    if pa_MapVarCount = MAX_MAP_VARIABLES then
      ERR_Exit(ERR_TOO_MANY_MAP_VARS, True, '', []);
    TK_NextTokenMustBe(TK_IDENTIFIER, ERR_INVALID_IDENTIFIER);
    if SY_FindGlobal(tk_Text) <> nil then
    begin  // Redefined
      ERR_Exit(ERR_REDEFINED_IDENTIFIER, true,
        'Identifier: %s', [tk_Text]);
    end;
    sym := SY_InsertGlobal(tk_Text, SY_MAPVAR);
    sym.info.svar.index := pa_MapVarCount;
    Inc(pa_MapVarCount);
    TK_NextToken;
  until tk_Token <> TK_COMMA;
  TK_TokenMustBe(TK_SEMICOLON, ERR_MISSING_SEMICOLON);
  TK_NextToken;
end;

//==============================================================================
//
// OuterWorldVar
//
//==============================================================================
procedure OuterWorldVar;
var
  index: integer;
  sym: PsymbolNode_t;
begin
  ACS_Message(MSG_DEBUG, '---- OuterWorldVar ----'#13#10, []);
  if TK_NextToken <> TK_INT then
    TK_TokenMustBe(TK_STR, ERR_BAD_VAR_TYPE);
  repeat
    TK_NextTokenMustBe(TK_NUMBER, ERR_MISSING_WVAR_INDEX);
    if tk_Num >= MAX_WORLD_VARIABLES then
    begin
      ERR_Exit(ERR_BAD_WVAR_INDEX, True, '', []);
    end;
    index := tk_Num;
    TK_NextTokenMustBe(TK_COLON, ERR_MISSING_WVAR_COLON);
    TK_NextTokenMustBe(TK_IDENTIFIER, ERR_INVALID_IDENTIFIER);
    if SY_FindGlobal(tk_Text) <> nil then
    begin  // Redefined
      ERR_Exit(ERR_REDEFINED_IDENTIFIER, true,
        'Identifier: %s', [tk_Text]);
    end;
    sym := SY_InsertGlobal(tk_Text, SY_WORLDVAR);
    sym.info.svar.index := index;
    TK_NextToken;
    inc(pa_WorldVarCount);
  until tk_Token <> TK_COMMA;
  TK_TokenMustBe(TK_SEMICOLON, ERR_MISSING_SEMICOLON);
  TK_NextToken;
end;

//==============================================================================
//
// OuterSpecialDef
//
//==============================================================================
procedure OuterSpecialDef;
var
  special: integer;
  sym: PsymbolNode_t;
begin
  ACS_Message(MSG_DEBUG, '---- OuterSpecialDef ----'#13#10, []);
  repeat
    TK_NextTokenMustBe(TK_NUMBER, ERR_MISSING_SPEC_VAL);
    special := tk_Num;
    TK_NextTokenMustBe(TK_COLON, ERR_MISSING_SPEC_COLON);
    TK_NextTokenMustBe(TK_IDENTIFIER, ERR_INVALID_IDENTIFIER);
    if SY_FindGlobal(tk_Text) <> nil then
    begin  // Redefined
      ERR_Exit(ERR_REDEFINED_IDENTIFIER, true,
        'Identifier: %s', [tk_Text]);
    end;
    sym := SY_InsertGlobal(tk_Text, SY_SPECIAL);
    TK_NextTokenMustBe(TK_LPAREN, ERR_MISSING_LPAREN);
    TK_NextTokenMustBe(TK_NUMBER, ERR_MISSING_SPEC_ARGC);
    sym.info.special.value := special;
    sym.info.special.argCount := tk_Num;
    TK_NextTokenMustBe(TK_RPAREN, ERR_MISSING_RPAREN);
    TK_NextToken;
  until tk_Token <> TK_COMMA;
  TK_TokenMustBe(TK_SEMICOLON, ERR_MISSING_SEMICOLON);
  TK_NextToken;
end;

//==============================================================================
//
// OuterDefine
//
//==============================================================================
procedure OuterDefine;
var
  value: integer;
  sym: PsymbolNode_t;
begin
  ACS_Message(MSG_DEBUG, '---- OuterDefine ----'#13#10, []);
  TK_NextTokenMustBe(TK_IDENTIFIER, ERR_INVALID_IDENTIFIER);
  if SY_FindGlobal(tk_Text) <> nil then
  begin  // Redefined
    ERR_Exit(ERR_REDEFINED_IDENTIFIER, true,
      'Identifier: %s', [tk_Text]);
  end;
  sym := SY_InsertGlobal(tk_Text, SY_CONSTANT);
  TK_NextToken;
  value := EvalConstExpression;
  ACS_Message(MSG_DEBUG, 'Constant value: %d'#13#10, [value]);
  sym.info.constant.value := value;
end;

//==============================================================================
//
// OuterInclude
//
//==============================================================================
procedure OuterInclude;
begin
  ACS_Message(MSG_DEBUG, '---- OuterInclude ----'#13#10, []);
  TK_NextTokenMustBe(TK_STRING, ERR_STRING_LIT_NOT_FOUND);
  TK_DoInclude(tk_Text);
  TK_NextToken;
end;

//==============================================================================
//
// ProcessStatement
//
//==============================================================================
function ProcessStatement(const owner: statement_t): boolean;
begin
  if StatementIndex = MAX_STATEMENT_DEPTH then
    ERR_Exit(ERR_STATEMENT_OVERFLOW, True, '', []);

  StatementHistory[StatementIndex] := owner;
  Inc(StatementIndex);
  case tk_Token of
    TK_INT,
    TK_STR:
      LeadingVarDeclare;
    TK_LINESPECIAL:
      LeadingLineSpecial;
    TK_RESTART:
      LeadingRestart;
    TK_SUSPEND:
      LeadingSuspend;
    TK_TERMINATE:
      LeadingTerminate;
    TK_IDENTIFIER:
      LeadingIdentifier;
    TK_PRINT,
    TK_PRINTBOLD:
      LeadingPrint;
    TK_IF:
      LeadingIf;
    TK_FOR:
      LeadingFor;
    TK_WHILE,
    TK_UNTIL:
      LeadingWhileUntil;
    TK_DO:
      LeadingDo;
    TK_SWITCH:
      LeadingSwitch;
    TK_CASE:
      begin
        if owner <> STMT_SWITCH then
          ERR_Exit(ERR_CASE_NOT_IN_SWITCH, True, '', []);
        LeadingCase;
      end;
    TK_DEFAULT:
      begin
        if owner <> STMT_SWITCH then
          ERR_Exit(ERR_DEFAULT_NOT_IN_SWITCH, True, '', []);
        if DefaultInCurrent then
          ERR_Exit(ERR_MULTIPLE_DEFAULT, True, '', []);
        LeadingDefault;
      end;
    TK_BREAK:
      begin
        if not BreakAncestor then
          ERR_Exit(ERR_MISPLACED_BREAK, True, '', []);
        LeadingBreak;
      end;
    TK_CONTINUE:
      begin
        if not ContinueAncestor then
          ERR_Exit(ERR_MISPLACED_CONTINUE, True, '', []);
        LeadingContinue;
      end;
    TK_LBRACE:
      LeadingCompoundStatement(owner);
    TK_SEMICOLON:
      TK_NextToken;
  else
    dec(StatementIndex);
    result := False;
    exit;
  end;
  dec(StatementIndex);
  result := True;
end;

//==============================================================================
//
// LeadingCompoundStatement
//
//==============================================================================
procedure LeadingCompoundStatement(const owner: statement_t);
begin
  StatementLevel := StatementLevel + AdjustStmtLevel[owner];
  TK_NextToken; // Eat the TK_LBRACE
  repeat until not ProcessStatement(owner);
  TK_TokenMustBe(TK_RBRACE, ERR_INVALID_STATEMENT);
  TK_NextToken;
  StatementLevel := StatementLevel - AdjustStmtLevel[owner];
end;

//==============================================================================
//
// LeadingVarDeclare
//
//==============================================================================
procedure LeadingVarDeclare;
var
  sym: PsymbolNode_t;
begin
  ACS_Message(MSG_DEBUG, '---- LeadingVarDeclare ----'#13#10, []);
  repeat
    if ScriptVarCount = MAX_SCRIPT_VARIABLES then
      ERR_Exit(ERR_TOO_MANY_SCRIPT_VARS, True, '', []);
    TK_NextTokenMustBe(TK_IDENTIFIER, ERR_INVALID_IDENTIFIER);
    if SY_FindLocal(tk_Text) <> nil then
    begin  // Redefined
      ERR_Exit(ERR_REDEFINED_IDENTIFIER, True,
        'Identifier: %s', [tk_Text]);
    end;
    sym := SY_InsertLocal(tk_Text, SY_SCRIPTVAR);
    sym.info.svar.index := ScriptVarCount;
    Inc(ScriptVarCount);
    TK_NextToken;
  until tk_Token <> TK_COMMA;
  TK_TokenMustBe(TK_SEMICOLON, ERR_MISSING_SEMICOLON);
  TK_NextToken;
end;

//==============================================================================
//
// LeadingLineSpecial
//
//==============================================================================
procedure LeadingLineSpecial;
var
  i: integer;
  argCount: integer;
  specialValue: integer;
  direct: boolean;
begin
  ACS_Message(MSG_DEBUG, '---- LeadingLineSpecial ----'#13#10, []);
  argCount := tk_SpecialArgCount;
  specialValue := tk_SpecialValue;
  TK_NextTokenMustBe(TK_LPAREN, ERR_MISSING_LPAREN);
  if TK_NextToken = TK_CONST then
  begin
    TK_NextTokenMustBe(TK_COLON, ERR_MISSING_COLON);
    PC_AppendCmd(PCD_LSPEC1DIRECT + (argCount - 1));
    PC_AppendLong(specialValue);
    direct := True;
  end
  else
  begin
    TK_Undo;
    direct := False;
  end;
  i := 0;
  repeat
    if i = argCount then
      ERR_Exit(ERR_BAD_LSPEC_ARG_COUNT, True, '', []);
    TK_NextToken;
    if direct then
      PC_AppendLong(EvalConstExpression)
    else
      EvalExpression;
    inc(i);
  until tk_Token <> TK_COMMA;
  if i <> argCount then
    ERR_Exit(ERR_BAD_LSPEC_ARG_COUNT, True, '', []);
  TK_TokenMustBe(TK_RPAREN, ERR_MISSING_RPAREN);
  TK_NextTokenMustBe(TK_SEMICOLON, ERR_MISSING_SEMICOLON);
  if not direct then
  begin
    PC_AppendCmd(PCD_LSPEC1 + (argCount - 1));
    PC_AppendLong(specialValue);
  end;
  TK_NextToken;
end;

//==============================================================================
//
// LeadingIdentifier
//
//==============================================================================
procedure LeadingIdentifier;
var
  sym: PsymbolNode_t;
begin
  sym := DemandSymbol(tk_Text);
  case sym.typ of
    SY_SCRIPTVAR,
    SY_MAPVAR,
    SY_WORLDVAR:
      LeadingVarAssign(sym);
    SY_INTERNFUNC:
      LeadingInternFunc(sym);
  end;
end;

//==============================================================================
//
// LeadingInternFunc
//
//==============================================================================
procedure LeadingInternFunc(const sym: PsymbolNode_t);
begin
  ProcessInternFunc(sym);
  if sym.info.internFunc.hasReturnValue then
    PC_AppendCmd(PCD_DROP);
  TK_TokenMustBe(TK_SEMICOLON, ERR_MISSING_SEMICOLON);
  TK_NextToken;
end;

//==============================================================================
//
// ProcessInternFunc
//
//==============================================================================
procedure ProcessInternFunc(const sym: PsymbolNode_t);
var
  i: integer;
  argCount: integer;
  direct: boolean;
begin
  ACS_Message(MSG_DEBUG, '---- ProcessInternFunc ----'#13#10, []);
  argCount := sym.info.internFunc.argCount;
  TK_NextTokenMustBe(TK_LPAREN, ERR_MISSING_LPAREN);
  if TK_NextToken = TK_CONST then
  begin
    TK_NextTokenMustBe(TK_COLON, ERR_MISSING_COLON);
    if sym.info.internFunc.directCommand = PCD_NOP then
      ERR_Exit(ERR_NO_DIRECT_VER, True, '', []);
    PC_AppendCmd(sym.info.internFunc.directCommand);
    direct := True;
    TK_NextToken;
  end
  else
    direct := False;
  i :=  0;
  if argCount > 0 then
  begin
    TK_Undo; // Adjust for first expression
    repeat
      if i = argCount then
        ERR_Exit(ERR_BAD_ARG_COUNT, True, '', []);
      TK_NextToken;
      if direct then
        PC_AppendLong(EvalConstExpression)
      else
        EvalExpression;
      Inc(i);
    until tk_Token <> TK_COMMA;
  end;
  if i <> argCount then
    ERR_Exit(ERR_BAD_ARG_COUNT, True, '', []);
  TK_TokenMustBe(TK_RPAREN, ERR_MISSING_RPAREN);
  if not direct then
    PC_AppendCmd(sym.info.internFunc.stackCommand);
  TK_NextToken;
end;

//==============================================================================
//
// LeadingPrint
//
//==============================================================================
procedure LeadingPrint;
var
  printCmd: Integer;
  stmtToken: integer;
begin
  ACS_Message(MSG_DEBUG, '---- LeadingPrint ----'#13#10, []);
  stmtToken := tk_Token; // Will be TK_PRINT or TK_PRINTBOLD
  PC_AppendCmd(PCD_BEGINPRINT);
  TK_NextTokenMustBe(TK_LPAREN, ERR_MISSING_LPAREN);
  repeat
    printCmd := PCD_NOP; // Shut up compiler warning
    case TK_NextCharacter of
      Ord('s'): // string
        printCmd := PCD_PRINTSTRING;
      Ord('i'), // integer
      Ord('d'): // decimal
        printCmd := PCD_PRINTNUMBER;
      Ord('c'): // character
        printCmd := PCD_PRINTCHARACTER;
    else
      ERR_Exit(ERR_UNKNOWN_PRTYPE, True, '', []);
    end;
    TK_NextTokenMustBe(TK_COLON, ERR_MISSING_COLON);
    TK_NextToken;
    EvalExpression;
    PC_AppendCmd(printCmd);
  until tk_Token <> TK_COMMA;
  TK_TokenMustBe(TK_RPAREN, ERR_MISSING_RPAREN);
  if stmtToken = TK_PRINT then
    PC_AppendCmd(PCD_ENDPRINT)
  else
    PC_AppendCmd(PCD_ENDPRINTBOLD);
  TK_NextToken;
end;

//==============================================================================
//
// LeadingIf
//
//==============================================================================
procedure LeadingIf;
var
  jumpAddrPtr1: integer;
  jumpAddrPtr2: integer;
begin
  ACS_Message(MSG_DEBUG, '---- LeadingIf ----'#13#10, []);
  TK_NextTokenMustBe(TK_LPAREN, ERR_MISSING_LPAREN);
  TK_NextToken;
  EvalExpression;
  TK_TokenMustBe(TK_RPAREN, ERR_MISSING_RPAREN);
  PC_AppendCmd(PCD_IFNOTGOTO);
  jumpAddrPtr1 := pc_Address;
  PC_SkipLong;
  TK_NextToken;
  if not ProcessStatement(STMT_IF) then
    ERR_Exit(ERR_INVALID_STATEMENT, True, '', []);
  if tk_Token = TK_ELSE then
  begin
    PC_AppendCmd(PCD_GOTO);
    jumpAddrPtr2 := pc_Address;
    PC_SkipLong;
    PC_WriteLong(pc_Address, jumpAddrPtr1);
    TK_NextToken;
    if not ProcessStatement(STMT_ELSE) then
      ERR_Exit(ERR_INVALID_STATEMENT, True, '', []);
    PC_WriteLong(pc_Address, jumpAddrPtr2);
  end
  else
    PC_WriteLong(pc_Address, jumpAddrPtr1);
end;

//==============================================================================
//
// LeadingFor
//
//==============================================================================
procedure LeadingFor;
begin
end;

//==============================================================================
//
// LeadingWhileUntil
//
//==============================================================================
procedure LeadingWhileUntil;
var
  stmtToken: integer;
  topAddr: integer;
  outAddrPtr: integer;
begin
  ACS_Message(MSG_DEBUG, '---- LeadingWhileUntil ----'#13#10, []);
  stmtToken := tk_Token;
  topAddr := pc_Address;
  TK_NextTokenMustBe(TK_LPAREN, ERR_MISSING_LPAREN);
  TK_NextToken;
  EvalExpression;
  TK_TokenMustBe(TK_RPAREN, ERR_MISSING_RPAREN);
  if stmtToken = TK_WHILE then
    PC_AppendCmd(PCD_IFNOTGOTO)
  else
    PC_AppendCmd(PCD_IFGOTO);
  outAddrPtr := pc_Address;
  PC_SkipLong;
  TK_NextToken;
  if not ProcessStatement(STMT_WHILEUNTIL) then
    ERR_Exit(ERR_INVALID_STATEMENT, True, '', []);
  PC_AppendCmd(PCD_GOTO);
  PC_AppendLong(topAddr);

  PC_WriteLong(pc_Address, outAddrPtr);

  WriteContinues(topAddr);
  WriteBreaks;
end;

//==============================================================================
//
// LeadingDo
//
//==============================================================================
procedure LeadingDo;
var
  topAddr: integer;
  exprAddr: integer;
  stmtToken: integer;
begin
  ACS_Message(MSG_DEBUG, '---- LeadingDo ----'#13#10, []);
  topAddr := pc_Address;
  TK_NextToken;
  if not ProcessStatement(STMT_DO) then
    ERR_Exit(ERR_INVALID_STATEMENT, True, '', []);
  if (tk_Token <> TK_WHILE) and (tk_Token <> TK_UNTIL) then
    ERR_Exit(ERR_BAD_DO_STATEMENT, True, '', []);
  stmtToken := tk_Token;
  TK_NextTokenMustBe(TK_LPAREN, ERR_MISSING_LPAREN);
  exprAddr := pc_Address;
  TK_NextToken;
  EvalExpression;
  TK_TokenMustBe(TK_RPAREN, ERR_MISSING_RPAREN);
  TK_NextTokenMustBe(TK_SEMICOLON, ERR_MISSING_SEMICOLON);
  if stmtToken = TK_WHILE then
    PC_AppendCmd(PCD_IFGOTO)
  else
    PC_AppendCmd(PCD_IFNOTGOTO);
  PC_AppendLong(topAddr);
  WriteContinues(exprAddr);
  WriteBreaks;
  TK_NextToken;
end;

//==============================================================================
//
// LeadingSwitch
//
//==============================================================================
procedure LeadingSwitch;
var
  switcherAddrPtr: integer;
  outAddrPtr: integer;
  cInfo: PcaseInfo_t;
  defaultAddress: integer;
begin
  ACS_Message(MSG_DEBUG, '---- LeadingSwitch ----'#13#10, []);

  TK_NextTokenMustBe(TK_LPAREN, ERR_MISSING_LPAREN);
  TK_NextToken;
  EvalExpression;
  TK_TokenMustBe(TK_RPAREN, ERR_MISSING_RPAREN);

  PC_AppendCmd(PCD_GOTO);
  switcherAddrPtr := pc_Address;
  PC_SkipLong;

  TK_NextToken;
  if not ProcessStatement(STMT_SWITCH) then
    ERR_Exit(ERR_INVALID_STATEMENT, True, '', []);

  PC_AppendCmd(PCD_GOTO);
  outAddrPtr := pc_Address;
  PC_SkipLong;

  PC_WriteLong(pc_Address, switcherAddrPtr);
  defaultAddress := 0;
  while True do
  begin
    cInfo := GetCaseInfo;
    if cInfo = nil then
      Break;
    if cInfo.isDefault then
    begin
      defaultAddress := cInfo.address;
      continue;
    end;
    PC_AppendCmd(PCD_CASEGOTO);
    PC_AppendLong(cInfo.value);
    PC_AppendLong(cInfo.address);
  end;
  PC_AppendCmd(PCD_DROP);

  if defaultAddress <> 0 then
  begin
    PC_AppendCmd(PCD_GOTO);
    PC_AppendLong(defaultAddress);
  end;

  PC_WriteLong(pc_Address, outAddrPtr);

  WriteBreaks;
end;

//==============================================================================
//
// LeadingCase
//
//==============================================================================
procedure LeadingCase;
begin
  ACS_Message(MSG_DEBUG, '---- LeadingCase ----'#13#10, []);
  TK_NextToken;
  PushCase(EvalConstExpression, False);
  TK_TokenMustBe(TK_COLON, ERR_MISSING_COLON);
  TK_NextToken;
end;

//==============================================================================
//
// LeadingDefault
//
//==============================================================================
procedure LeadingDefault;
begin
  ACS_Message(MSG_DEBUG, '---- LeadingDefault ----'#13#10, []);
  TK_NextTokenMustBe(TK_COLON, ERR_MISSING_COLON);
  PushCase(0, True);
  TK_NextToken;
end;

//==============================================================================
//
// PushCase
//
//==============================================================================
procedure PushCase(const value: integer; const isDefault: boolean);
begin
  if CaseIndex = MAX_CASE then
    ERR_Exit(ERR_CASE_OVERFLOW, True, '', []);
  CaseInfo[CaseIndex].level := StatementLevel;
  CaseInfo[CaseIndex].value := value;
  CaseInfo[CaseIndex].isDefault := isDefault;
  CaseInfo[CaseIndex].address := pc_Address;
  Inc(CaseIndex);
end;

//==============================================================================
//
// GetCaseInfo
//
//==============================================================================
function GetCaseInfo: PcaseInfo_t;
begin
  if CaseIndex = 0 then
  begin
    result := nil;
    exit;
  end;
  if CaseInfo[CaseIndex - 1].level > StatementLevel then
  begin
    Dec(CaseIndex);
    result := @CaseInfo[CaseIndex];
    exit;
  end;
  result := nil;
end;

//==============================================================================
//
// DefaultInCurrent
//
//==============================================================================
function DefaultInCurrent: boolean;
var
  i: integer;
begin
  for i := 0 to CaseIndex - 1 do
  begin
    if CaseInfo[i].isDefault and (CaseInfo[i].level = StatementLevel) then
    begin
      result := true;
      exit;
    end;
  end;
  result := false;
end;

//==============================================================================
//
// LeadingBreak
//
//==============================================================================
procedure LeadingBreak;
begin
  ACS_Message(MSG_DEBUG, '---- LeadingBreak ----'#13#10, []);
  TK_NextTokenMustBe(TK_SEMICOLON, ERR_MISSING_SEMICOLON);
  PC_AppendCmd(PCD_GOTO);
  PushBreak;
  PC_SkipLong;
  TK_NextToken;
end;

//==============================================================================
//
// PushBreak
//
//==============================================================================
procedure PushBreak;
begin
  if BreakIndex = MAX_CASE then
    ERR_Exit(ERR_BREAK_OVERFLOW, True, '', []);
  BreakInfo[BreakIndex].level := StatementLevel;
  BreakInfo[BreakIndex].addressPtr := pc_Address;
  Inc(BreakIndex);
end;

//==============================================================================
//
// WriteBreaks
//
//==============================================================================
procedure WriteBreaks;
begin
  if BreakIndex = 0 then
    exit;
  while BreakInfo[BreakIndex - 1].level > StatementLevel do
  begin
    dec(BreakIndex);
    PC_WriteLong(pc_Address, BreakInfo[BreakIndex].addressPtr);
  end;
end;

//==============================================================================
//
// BreakAncestor
//
// Returns YES if the current statement history contains a break root
// statement.
//
//==============================================================================
function BreakAncestor: boolean;
var
  i: integer;
begin
  for i := 0 to StatementIndex - 1 do
  begin
    if IsBreakRoot[StatementHistory[i]] then
    begin
      result := True;
      exit;
    end;
  end;
  result := false;
end;

//==============================================================================
//
// LeadingContinue
//
//==============================================================================
procedure LeadingContinue;
begin
  ACS_Message(MSG_DEBUG, '---- LeadingContinue ----'#13#10, []);
  TK_NextTokenMustBe(TK_SEMICOLON, ERR_MISSING_SEMICOLON);
  PC_AppendCmd(PCD_GOTO);
  PushContinue;
  PC_SkipLong;
  TK_NextToken;
end;

//==============================================================================
//
// PushContinue
//
//==============================================================================
procedure PushContinue;
begin
  if ContinueIndex = MAX_CONTINUE then
    ERR_Exit(ERR_CONTINUE_OVERFLOW, True, '', []);
  ContinueInfo[ContinueIndex].level := StatementLevel;
  ContinueInfo[ContinueIndex].addressPtr := pc_Address;
  Inc(ContinueIndex);
end;

//==============================================================================
//
// WriteContinues
//
//==============================================================================
procedure WriteContinues(const address: integer);
begin
  if ContinueIndex = 0 then
    exit;
  while ContinueInfo[ContinueIndex - 1].level > StatementLevel do
  begin
    Dec(ContinueIndex);
    PC_WriteLong(address, ContinueInfo[ContinueIndex].addressPtr);
  end;
end;

//==============================================================================
//
// ContinueAncestor
//
//==============================================================================
function ContinueAncestor: boolean;
var
  i: integer;
begin
  for i := 0 to StatementIndex - 1 do
  begin
    if IsContinueRoot[StatementHistory[i]] then
    begin
      result := True;
      Exit;
    end;
  end;
  result := False
end;

//==============================================================================
//
// LeadingVarAssign
//
//==============================================================================
procedure LeadingVarAssign(sym: PsymbolNode_t);
var
  done: boolean;
  assignToken: integer;
begin
  ACS_Message(MSG_DEBUG, '---- LeadingVarAssign ----'#13#10, []);
  done := false;
  repeat
    TK_NextToken; // Fetch assignment operator
    if (tk_Token = TK_INC) or (tk_Token = TK_DEC) then
    begin  // Postfix increment or decrement
      PC_AppendCmd(GetIncDecPCD(tk_Token, sym.typ));
      PC_AppendLong(sym.info.svar.index);
      TK_NextToken;
    end
    else
    begin  // Normal operator
      if not TK_Member(@AssignOps) then
        ERR_Exit(ERR_MISSING_ASSIGN_OP, True, '', []);
      assignToken := tk_Token;
      TK_NextToken;
      EvalExpression;
      PC_AppendCmd(GetAssignPCD(assignToken, sym.typ));
      PC_AppendLong(sym.info.svar.index);
    end;
    if tk_Token = TK_COMMA then
    begin
      TK_NextTokenMustBe(TK_IDENTIFIER, ERR_BAD_ASSIGNMENT);
      sym := DemandSymbol(tk_Text);
      if (sym.typ <> SY_SCRIPTVAR) and (sym.typ <> SY_MAPVAR) and (sym.typ <> SY_WORLDVAR) then
        ERR_Exit(ERR_BAD_ASSIGNMENT, True, '', []);
    end
    else
    begin
      TK_TokenMustBe(TK_SEMICOLON, ERR_MISSING_SEMICOLON);
      TK_NextToken;
      done := True;
    end;
  until done;
end;

type
  Lookup_t = record
    token: integer;
    symbol: symbolType_t;
    pcd: integer;
  end;

const
  ASSIGNMENT_LOOKUP_SIZE = 19;

  assignmentLookup: array[0..ASSIGNMENT_LOOKUP_SIZE - 1] of Lookup_t = (
    (token: TK_ASSIGN;    symbol: SY_SCRIPTVAR; pcd: PCD_ASSIGNSCRIPTVAR),
    (token: TK_ASSIGN;    symbol: SY_MAPVAR;    pcd: PCD_ASSIGNMAPVAR),
    (token: TK_ASSIGN;    symbol: SY_WORLDVAR;  pcd: PCD_ASSIGNWORLDVAR),
    (token: TK_ADDASSIGN; symbol: SY_SCRIPTVAR; pcd: PCD_ADDSCRIPTVAR),
    (token: TK_ADDASSIGN; symbol: SY_MAPVAR;    pcd: PCD_ADDMAPVAR),
    (token: TK_ADDASSIGN; symbol: SY_WORLDVAR;  pcd: PCD_ADDWORLDVAR),
    (token: TK_SUBASSIGN; symbol: SY_SCRIPTVAR; pcd: PCD_SUBSCRIPTVAR),
    (token: TK_SUBASSIGN; symbol: SY_MAPVAR;    pcd: PCD_SUBMAPVAR),
    (token: TK_SUBASSIGN; symbol: SY_WORLDVAR;  pcd: PCD_SUBWORLDVAR),
    (token: TK_MULASSIGN; symbol: SY_SCRIPTVAR; pcd: PCD_MULSCRIPTVAR),
    (token: TK_MULASSIGN; symbol: SY_MAPVAR;    pcd: PCD_MULMAPVAR),
    (token: TK_MULASSIGN; symbol: SY_WORLDVAR;  pcd: PCD_MULWORLDVAR),
    (token: TK_DIVASSIGN; symbol: SY_SCRIPTVAR; pcd: PCD_DIVSCRIPTVAR),
    (token: TK_DIVASSIGN; symbol: SY_MAPVAR;    pcd: PCD_DIVMAPVAR),
    (token: TK_DIVASSIGN; symbol: SY_WORLDVAR;  pcd: PCD_DIVWORLDVAR),
    (token: TK_MODASSIGN; symbol: SY_SCRIPTVAR; pcd: PCD_MODSCRIPTVAR),
    (token: TK_MODASSIGN; symbol: SY_MAPVAR;    pcd: PCD_MODMAPVAR),
    (token: TK_MODASSIGN; symbol: SY_WORLDVAR;  pcd: PCD_MODWORLDVAR),
    (token: TK_NONE)
  );

//==============================================================================
//
// GetAssignPCD
//
//==============================================================================
function GetAssignPCD(token: integer; symbol: symbolType_t): Integer;
var
  i: integer;
begin
  i := 0;
  while assignmentLookup[i].token <> TK_NONE do
  begin
    if (assignmentLookup[i].token = token) and (assignmentLookup[i].symbol = symbol) then
    begin
      result := assignmentLookup[i].pcd;
      exit;
    end;
    inc(i);
  end;
  result := PCD_NOP;
end;

//==============================================================================
//
// LeadingSuspend
//
//==============================================================================
procedure LeadingSuspend;
begin
  ACS_Message(MSG_DEBUG, '---- LeadingSuspend ----'#13#10, []);
  TK_NextTokenMustBe(TK_SEMICOLON, ERR_MISSING_SEMICOLON);
  PC_AppendCmd(PCD_SUSPEND);
  TK_NextToken;
end;

//==============================================================================
//
// LeadingTerminate
//
//==============================================================================
procedure LeadingTerminate;
begin
  ACS_Message(MSG_DEBUG, '---- LeadingTerminate ----'#13#10, []);
  TK_NextTokenMustBe(TK_SEMICOLON, ERR_MISSING_SEMICOLON);
  PC_AppendCmd(PCD_TERMINATE);
  TK_NextToken;
end;

//==============================================================================
//
// LeadingRestart
//
//==============================================================================
procedure LeadingRestart;
begin
  ACS_Message(MSG_DEBUG, '---- LeadingRestart ----'#13#10, []);
  TK_NextTokenMustBe(TK_SEMICOLON, ERR_MISSING_SEMICOLON);
  PC_AppendCmd(PCD_RESTART);
  TK_NextToken;
end;

//==============================================================================
//
// EvalConstExpression
//
//==============================================================================
function EvalConstExpression: Integer;
begin
  ExprStackIndex := 0;
  ConstantExpression := True;
  ExprLevA;
  if ExprStackIndex <> 1 then
    ERR_Exit(ERR_BAD_CONST_EXPR, True, '', []);
  result := PopExStk;
end;

//==============================================================================
//
// EvalExpression
//
//==============================================================================
procedure EvalExpression;
begin
  ConstantExpression := False;
  ExprLevA;
end;

//==============================================================================
// ExprLevA
//
// Operator: ||
//
//==============================================================================
procedure ExprLevA;
begin
  ExprLevB;
  while tk_Token = TK_ORLOGICAL do
  begin
    TK_NextToken;
    ExprLevB;
    SendExprCommand(PCD_ORLOGICAL);
  end;
end;

//==============================================================================
// ExprLevB
//
// Operator: &&
//
//==============================================================================
procedure ExprLevB;
begin
  ExprLevC;
  while tk_Token = TK_ANDLOGICAL do
  begin
    TK_NextToken;
    ExprLevC;
    SendExprCommand(PCD_ANDLOGICAL);
  end;
end;

//==============================================================================
// ExprLevC
//
// Operator: |
//
//==============================================================================
procedure ExprLevC;
begin
  ExprLevD;
  while tk_Token = TK_ORBITWISE do
  begin
    TK_NextToken;
    ExprLevD;
    SendExprCommand(PCD_ORBITWISE);
  end;
end;

//==============================================================================
// ExprLevD
//
// Operator: ^
//
//==============================================================================
procedure ExprLevD;
begin
  ExprLevE;
  while tk_Token = TK_EORBITWISE do
  begin
    TK_NextToken;
    ExprLevE;
    SendExprCommand(PCD_EORBITWISE);
  end;
end;

//==============================================================================
// ExprLevE
//
// Operator: &
//
//==============================================================================
procedure ExprLevE;
begin
  ExprLevF;
  while tk_Token = TK_ANDBITWISE do
  begin
    TK_NextToken;
    ExprLevF;
    SendExprCommand(PCD_ANDBITWISE);
  end;
end;

//==============================================================================
// ExprLevF
//
// Operators: = <>
//
//==============================================================================
procedure ExprLevF;
var
  token: integer;
begin
  ExprLevG;
  while TK_Member(@LevFOps) do
  begin
    token := tk_Token;
    TK_NextToken;
    ExprLevG;
    SendExprCommand(TokenToPCD(token));
  end;
end;

//==============================================================================
// ExprLevG
//
// Operators: < <= > >=
//
//==============================================================================
procedure ExprLevG;
var
  token: integer;
begin
  ExprLevH;
  while TK_Member(@LevGOps) do
  begin
    token := tk_Token;
    TK_NextToken;
    ExprLevH;
    SendExprCommand(TokenToPCD(token));
  end;
end;

//==============================================================================
// ExprLevH
//
// Operators:  shl   shr
//
//==============================================================================
procedure ExprLevH;
var
  token: integer;
begin
  ExprLevI;
  while TK_Member(@LevHOps) do
  begin
    token := tk_Token;
    TK_NextToken;
    ExprLevI;
    SendExprCommand(TokenToPCD(token));
  end;
end;

//==============================================================================
// ExprLevI
//
// Operators: + -
//
//==============================================================================
procedure ExprLevI;
var
  token: integer;
begin
  ExprLevJ;
  while TK_Member(@LevIOps) do
  begin
    token := tk_Token;
    TK_NextToken;
    ExprLevJ;
    SendExprCommand(TokenToPCD(token));
  end;
end;

//==============================================================================
// ExprLevJ
//
// Operators: * /  mod
//
//==============================================================================
procedure ExprLevJ;
var
  token: integer;
  unaryMinus: boolean;
begin
  unaryMinus := False;
  if tk_Token = TK_MINUS then
  begin
    unaryMinus := True;
    TK_NextToken;
  end;
  if ConstantExpression then
    ConstExprFactor
  else
    ExprFactor;
  if unaryMinus then
    SendExprCommand(PCD_UNARYMINUS);
  while TK_Member(@LevJOps) do
  begin
    token := tk_Token;
    TK_NextToken;
    if ConstantExpression then
      ConstExprFactor
    else
      ExprFactor;
    SendExprCommand(TokenToPCD(token));
  end;
end;

//==============================================================================
//
// ExprFactor
//
//==============================================================================
procedure ExprFactor;
var
  sym: PsymbolNode_t;
  opToken: integer;
begin
  case tk_Token of
    TK_STRING:
      begin
        PC_AppendCmd(PCD_PUSHNUMBER);
        PC_AppendLong(STR_Find(tk_Text));
        TK_NextToken;
      end;
    TK_NUMBER:
      begin
        PC_AppendCmd(PCD_PUSHNUMBER);
        PC_AppendLong(tk_Num);
        TK_NextToken;
      end;
    TK_LPAREN:
      begin
        TK_NextToken;
        ExprLevA;
        if tk_Token <> TK_RPAREN then
          ERR_Exit(ERR_BAD_EXPR, True, '', []);
        TK_NextToken;
      end;
    TK_NOT:
      begin
        TK_NextToken;
        ExprFactor;
        PC_AppendCmd(PCD_NEGATELOGICAL);
      end;
    TK_INC,
    TK_DEC:
      begin
        opToken := tk_Token;
        TK_NextTokenMustBe(TK_IDENTIFIER, ERR_INCDEC_OP_ON_NON_VAR);
        sym :=  DemandSymbol(tk_Text);
        if (sym.typ <> SY_SCRIPTVAR) and (sym.typ <> SY_MAPVAR) and (sym.typ <> SY_WORLDVAR) then
          ERR_Exit(ERR_INCDEC_OP_ON_NON_VAR, True, '', []);
        PC_AppendCmd(GetIncDecPCD(opToken, sym.typ));
        PC_AppendLong(sym.info.svar.index);
        PC_AppendCmd(GetPushVarPCD(sym.typ));
        PC_AppendLong(sym.info.svar.index);
        TK_NextToken;
      end;
    TK_IDENTIFIER:
      begin
        sym := DemandSymbol(tk_Text);
        case sym.typ of
          SY_SCRIPTVAR,
          SY_MAPVAR,
          SY_WORLDVAR:
            begin
              PC_AppendCmd(GetPushVarPCD(sym.typ));
              PC_AppendLong(sym.info.svar.index);
              TK_NextToken;
              if (tk_Token = TK_INC) or (tk_Token = TK_DEC) then
              begin
                PC_AppendCmd(GetIncDecPCD(tk_Token, sym.typ));
                PC_AppendLong(sym.info.svar.index);
                TK_NextToken;
              end;
            end;
          SY_INTERNFUNC:
            begin
              if not sym.info.internFunc.hasReturnValue then
                ERR_Exit(ERR_EXPR_FUNC_NO_RET_VAL, True, '', []);
              ProcessInternFunc(sym);
            end;
        else
          ERR_Exit(ERR_ILLEGAL_EXPR_IDENT, True, 'Identifier: %s', [tk_Text]);
        end;
      end;
  else
    ERR_Exit(ERR_BAD_EXPR, True, '', []);
  end;
end;

//==============================================================================
//
// ConstExprFactor
//
//==============================================================================
procedure ConstExprFactor;
begin
  case tk_Token of
    TK_STRING:
      begin
        PushExStk(STR_Find(tk_Text));
        TK_NextToken;
      end;
    TK_NUMBER:
      begin
        PushExStk(tk_Num);
        TK_NextToken;
      end;
    TK_LPAREN:
      begin
        TK_NextToken;
        ExprLevA;
        if tk_Token <> TK_RPAREN then
          ERR_Exit(ERR_BAD_CONST_EXPR, True, '', []);
        TK_NextToken;
      end;
    TK_NOT:
      begin
        TK_NextToken;
        ConstExprFactor;
        SendExprCommand(PCD_NEGATELOGICAL);
      end;
  else
    ERR_Exit(ERR_BAD_CONST_EXPR, True, '', []);
  end;
end;

//==============================================================================
//
// SendExprCommand
//
//==============================================================================
procedure SendExprCommand(const pcd: integer);
var
  operand2: integer;
begin
  if not ConstantExpression then
  begin
    PC_AppendCmd(pcd);
    exit;
  end;

  case pcd of
    PCD_ADD:
      PushExStk(PopExStk + PopExStk);
    PCD_SUBTRACT:
      begin
        operand2 := PopExStk;
        PushExStk(PopExStk - operand2);
      end;
    PCD_MULTIPLY:
      PushExStk(PopExStk * PopExStk);
    PCD_DIVIDE:
      begin
        operand2 := PopExStk;
        PushExStk(PopExStk div operand2);
      end;
    PCD_MODULUS:
      begin
        operand2 := PopExStk;
        PushExStk(PopExStk mod operand2);
      end;
    PCD_EQ:
      PushExStk(intval(PopExStk = PopExStk));
    PCD_NE:
      PushExStk(intval(PopExStk <> PopExStk));
    PCD_LT:
      PushExStk(intval(PopExStk >= PopExStk));
    PCD_GT:
      PushExStk(intval(PopExStk <= PopExStk));
    PCD_LE:
      PushExStk(intval(PopExStk > PopExStk));
    PCD_GE:
      PushExStk(intval(PopExStk < PopExStk));
    PCD_ANDLOGICAL:
      PushExStk(PopExStk and PopExStk);
    PCD_ORLOGICAL:
      PushExStk(PopExStk or PopExStk);
    PCD_ANDBITWISE:
      PushExStk(PopExStk and PopExStk);
    PCD_ORBITWISE:
      PushExStk(PopExStk or PopExStk);
    PCD_EORBITWISE:
      PushExStk(PopExStk xor PopExStk);
    PCD_NEGATELOGICAL:
      PushExStk(not PopExStk);
    PCD_LSHIFT:
      begin
        operand2 := PopExStk;
        PushExStk(PopExStk shr operand2);
      end;
    PCD_RSHIFT:
      begin
        operand2 := PopExStk;
        PushExStk(PopExStk shl operand2);
      end;
    PCD_UNARYMINUS:
      PushExStk(-PopExStk);
  else
    ERR_Exit(ERR_UNKNOWN_CONST_EXPR_PCD, True, '', []);
  end;
end;

//==============================================================================
//
// PushExStk
//
//==============================================================================
procedure PushExStk(const value: integer);
begin
  if ExprStackIndex = EXPR_STACK_DEPTH then
    ERR_Exit(ERR_EXPR_STACK_OVERFLOW, True, '', []);
  ExprStack[ExprStackIndex] := value;
  Inc(ExprStackIndex);
end;

//==============================================================================
//
// PopExStk
//
//==============================================================================
function PopExStk: integer;
begin
  if ExprStackIndex < 1 then
    ERR_Exit(ERR_EXPR_STACK_EMPTY, True, '', []);
  Dec(ExprStackIndex);
  result := ExprStack[ExprStackIndex];
end;

type
  operatorLookup_t = record
    token: integer;
    pcd: integer;
  end;

const
  NUM_OPERATOR_LOOKUP = 14;

  operatorLookup: array[0..NUM_OPERATOR_LOOKUP - 1] of operatorLookup_t = (
    (token: TK_EQ;        pcd: PCD_EQ),
    (token: TK_NE;        pcd: PCD_NE),
    (token: TK_LT;        pcd: PCD_LT),
    (token: TK_LE;        pcd: PCD_LE),
    (token: TK_GT;        pcd: PCD_GT),
    (token: TK_GE;        pcd: PCD_GE),
    (token: TK_LSHIFT;    pcd: PCD_LSHIFT),
    (token: TK_RSHIFT;    pcd: PCD_RSHIFT),
    (token: TK_PLUS;      pcd: PCD_ADD),
    (token: TK_MINUS;     pcd: PCD_SUBTRACT),
    (token: TK_ASTERISK;  pcd: PCD_MULTIPLY),
    (token: TK_SLASH;     pcd: PCD_DIVIDE),
    (token: TK_PERCENT;   pcd: PCD_MODULUS),
    (token: TK_NONE;      pcd: PCD_NOP)
  );

//==============================================================================
//
// TokenToPCD
//
//==============================================================================
function TokenToPCD(token: integer): integer;
var
  i: integer;
begin
  i := 0;
  while operatorLookup[i].token <> TK_NONE do
  begin
    if operatorLookup[i].token = token then
    begin
      result := operatorLookup[i].pcd;
      exit;
    end;
    inc(i);
  end;
  result := PCD_NOP;
end;

//==============================================================================
//
// GetPushVarPCD
//
//==============================================================================
function GetPushVarPCD(const symType: symbolType_t): Integer;
begin
  case symType of
    SY_SCRIPTVAR:
      result := PCD_PUSHSCRIPTVAR;
    SY_MAPVAR:
      result := PCD_PUSHMAPVAR;
    SY_WORLDVAR:
      result := PCD_PUSHWORLDVAR;
  else
    result := PCD_NOP;
  end;
end;

const
  NUM_INCDEC_LOOKUP = 7;
  incDecLookup: array[0..NUM_INCDEC_LOOKUP - 1] of Lookup_t = (
    (token: TK_INC;   symbol: SY_SCRIPTVAR; pcd: PCD_INCSCRIPTVAR),
    (token: TK_INC;   symbol: SY_MAPVAR;    pcd: PCD_INCMAPVAR),
    (token: TK_INC;   symbol: SY_WORLDVAR;  pcd: PCD_INCWORLDVAR),
    (token: TK_DEC;   symbol: SY_SCRIPTVAR; pcd: PCD_DECSCRIPTVAR),
    (token: TK_DEC;   symbol: SY_MAPVAR;    pcd: PCD_DECMAPVAR),
    (token: TK_DEC;   symbol: SY_WORLDVAR;  pcd: PCD_DECWORLDVAR),
    (token: TK_NONE)
  );

//==============================================================================
//
// GetIncDecPCD
//
//==============================================================================
function GetIncDecPCD(token: integer; symbol: symbolType_t): Integer;var
  i: integer;
begin
  i := 0;
  while incDecLookup[i].token <> TK_NONE do
  begin
    if (incDecLookup[i].token = token) and (incDecLookup[i].symbol = symbol) then
    begin
      result := incDecLookup[i].pcd;
      exit;
    end;
    inc(i);
  end;
  result := PCD_NOP;
end;

//==============================================================================
//
// DemandSymbol
//
//==============================================================================
function DemandSymbol(const name: string): PsymbolNode_t;
begin
  result := SY_Find(name);
  if result = nil then
    ERR_Exit(ERR_UNKNOWN_IDENTIFIER, True, 'Identifier: %s', [name]);
end;

//==============================================================================
//
// Outside
//
//==============================================================================
procedure Outside;
var
  done: boolean;
begin
  done := false;
  while not done do
  begin
    case tk_Token of
      TK_EOF:
        done := true;
      TK_SCRIPT:
        OuterScript;
      TK_INT,
      TK_STR:
        OuterMapVar;
      TK_WORLD:
        OuterWorldVar;
      TK_SPECIAL:
        OuterSpecialDef;
      TK_NUMBERSIGN:
        begin
          TK_NextToken;
          case tk_Token of
            TK_DEFINE:
              OuterDefine;
            TK_INCLUDE:
              OuterInclude;
          else
            ERR_Exit(ERR_INVALID_DIRECTIVE, True, '', []);
          end;
        end;
    else
      ERR_Exit(ERR_INVALID_DECLARATOR, True, '', []);
    end;
  end;
end;

//==============================================================================
//
// PA_Parse
//
//==============================================================================
procedure PA_Parse;
begin
  pa_ScriptCount := 0;
  pa_OpenScriptCount := 0;
  pa_MapVarCount := 0;
  pa_WorldVarCount := 0;
  TK_NextToken;
  Outside;
  SY_FreeGlobals;
end;

end.

