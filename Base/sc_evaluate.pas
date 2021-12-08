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

unit sc_evaluate;

interface

uses
  d_delphi;

type
  TFindVar = function(v: string): boolean of object;
  TVarValue = function(v: string): string of object;
  TObjFunc = function(p: TDStrings): string of object;
  TExtFunc = function(p: TDStrings): string;

type
  TEvalFunction = class(TObject)
  private
    FName: string;
    FNumParams: integer;
    FEvalObjFunc: TObjFunc;
    FEvalFunc: TExtFunc;
    procedure SetName(v: string);
  public
    property Name: string read FName write SetName;
    constructor Create(aname: string; afunc: TObjFunc; anum: integer); overload;
    constructor Create(aname: string; afunc: TExtFunc; anum: integer); overload;
    function Value(p: TDStrings): string; overload;
    function Value(v: string): string; overload;
  end;

type
  TEvalList = class(TObject)
  private
    fList: PObjectArray;
    fNumItems: integer;
    fRealNumItems: integer;
  protected
    procedure Grow; virtual;
    function Get(Index: Integer): TObject; virtual;
    procedure Put(Index: Integer; const value: TObject); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Add(const value: TObject): integer; virtual;
    procedure Insert(Index: Integer; Item: TObject);
    function Delete(const Index: integer): boolean;
    function IndexOf(const value: TObject): integer; virtual;
    procedure Clear;
    property Count: integer read fNumItems;
    property Objects[Index: Integer]: TObject read Get write Put;
    property List: PObjectArray read fList;
  end;

  TEvalNode = class(TEvalList)
  private
    FOperator: char;
    FExpr: string;
    FFunction: TEvalFunction;
    function GetNode(index: integer): TEvalNode;
  public
    property Node[index: integer]: TEvalNode read GetNode; default;
    constructor Create; override;
    function AddNode: TEvalNode;
  end;

  TEvaluator = class(TEvalList)
  private
    FRoot: TEvalNode;
    FExpr: string;
    FFindVar: TFindVar;
    FVarValue: TVarValue;
    procedure ClearTree;
    procedure SetExpr(v: string);
    procedure Rebuild;
    function EvalTree(p: TEvalNode): string;
    function ValidateExpression(v: string): string;
    procedure ProcessExpression(v: string; p: TEvalNode);
    function FindFunc(AName: string): TEvalFunction;
    function FindPos(v: string): integer;
    procedure _err_incompatible_types(const callfunc: string; const s1, s2: string);
    // operators
    function OP_Add(v1, v2: string): string;
    function OP_Subtract(v1, v2: string): string;
    function OP_Multiply(v1, v2: string): string;
    function OP_Divide(v1, v2: string): string;
    function OP_And(v1, v2: string): string;
    function OP_Or(v1, v2: string): string;
    function OP_XOR(v1, v2: string): string;
    function OP_Equal(v1, v2: string): string;
    function OP_Less(v1, v2: string): string;
    function OP_Greater(v1, v2: string): string;
    function OP_Not(v1, v2: string): string;
    // built-in functions
    function PF_str_cat(p: TDStrings): string;
    function PF_str_len(p: TDStrings): string;
    function PF_str_insert(p: TDStrings): string;
    function PF_str_delete(p: TDStrings): string;
    function PF_str_pos(p: TDStrings): string;
    function PF_if(p: TDStrings): string;
    // Math functions
    function PF_val(p: TDStrings): string;
    function PF_abs(p: TDStrings): string;
    function PF_min(p: TDStrings): string;
    function PF_max(p: TDStrings): string;
    function PF_exp(p: TDStrings): string;
    function PF_log(p: TDStrings): string;
    function PF_log10(p: TDStrings): string;
    function PF_log2(p: TDStrings): string;
    function PF_ceil(p: TDStrings): string;
    function PF_floor(p: TDStrings): string;
    function PF_round(p: TDStrings): string;
    function PF_trunc(p: TDStrings): string;
    function PF_sqr(p: TDStrings): string;
    function PF_sqrt(p: TDStrings): string;
    function PF_frac(p: TDStrings): string;
    function PF_power(p: TDStrings): string;
    // Triginometry functions
    function PF_sin(p: TDStrings): string;
    function PF_cos(p: TDStrings): string;
    function PF_tan(p: TDStrings): string;
    function PF_asin(p: TDStrings): string;
    function PF_acos(p: TDStrings): string;
    function PF_atan(p: TDStrings): string;
    function PF_sinh(p: TDStrings): string;
    function PF_cosh(p: TDStrings): string;
    function PF_tanh(p: TDStrings): string;
    function PF_atan2(p: TDStrings): string;
    function PF_VectorAngle(p: TDStrings): string;
  public
    property Expr: string read FExpr write SetExpr;
    property FindVar: TFindVar read FFindVar write FFindVar;
    property VarValue: TVarValue read FVarValue write FVarValue;
    constructor Create; override;
    destructor Destroy; override;
    function Value: string;
    function EvaluateExpression(const aexpr: string): string;
    procedure AddFunc(aname: string; afunc: TObjFunc; anump: integer); overload; virtual;
    procedure AddFunc(aname: string; afunc: TExtFunc; anump: integer); overload; virtual;
  end;

const
  AllowedFirstChar: charset_t = ['a'..'z', 'A'..'Z', '_'];
  AllowedChar: charset_t = ['a'..'z', 'A'..'Z', '_', '0'..'9', '(', ')', ',',
  '"', ' '];
  HighMarks: charset_t = ['/', '*'];
  LowMarks: charset_t = ['+', '-'];
  BoolMarks: charset_t = ['&', '|', '^', '=', '<', '>', '!']; // AND, OR, XOR
  Marks: charset_t = ['/', '*', '+', '-', '&', '|', '^', '=', '<', '>', '!'];
  InputMarks: charset_t = ['/', '*', '+', '-', '=', '<', '>', '&', '|', '^', '!'];

function StrIsBool(const s: string): boolean;
function BoolToStr(v: boolean): string;
function StrToBool(v: string): boolean;
function StripStr(v: string): string;

implementation

uses
  Math,
  i_system;

////////////////////////////////////////////////////////////////////////////////
// helper functions

// removes leading and trailing quote marks
function StripStr(v: string): string;
begin
  Delete(v, 1, 1);
  Delete(v, Length(v), 1);
  Result := v;
end;

// splits a string at the first position of a semicolon, cares about
// quoted parts
procedure Split(var s, r: string);
var
  i, p, l, bracket: integer;
  quote: boolean;
begin
  p := 0;
  i := 1;
  l := System.Length(r);
  quote := false;
  bracket := 0;
  while (p = 0) and (i < l) do
  begin
    if r[i] = '"' then
      quote := not quote;
    if r[i] = '(' then
      Inc(bracket);
    if r[i] = ')' then
      Dec(bracket);
    if (not quote) and (bracket = 0) and (r[i] = ',') then
      p := i;
    Inc(i);
  end;
  if p = 0 then
  begin
    s := r;
    r := '';
  end
  else
  begin
    s := System.Copy(r, 1, p - 1);
    System.Delete(r, 1, p);
  end;
end;

// splits a string at the position of semicolons into a list
procedure SplitToList(s: string; l: TDStrings);
var
  h: string;
begin
  while s <> '' do
  begin
    Split(h, s);
    if h <> '' then
      l.Add(h);
  end;
end;

// substitutes symbols, except when quoted
function SubstituteMarks(s, sout, sin: string): string;
var
  quote: boolean;
  i: integer;
  a, b, hs: string;
begin
  quote := false;
  i := 1;
  while i < System.Length(s) do
  begin
    if s[i] = '"' then
      quote := not quote;
    if not quote then
    begin
      hs := System.Copy(s, i, System.Length(sout));
      if hs = sout then
      begin
        a := System.Copy(s, 1, i - 1);
        b := System.Copy(s, i + System.Length(sout), System.Length(s));
        s := a + sin + b;
      end;
    end;
    Inc(i);
  end;
  Result := s;
end;

// substitutes words, these must be separated by brackets or spaces
function SubstituteWords(s, sout, sin: string): string;
var
  p: integer;
  b: boolean;
  v: string;
begin
  // simple case
  if s = sout then
  begin
    Result := sin;
    exit;
  end;

  v := s;
  Result := '';
  repeat
    p := System.Pos(sout, v);
    b := false;
    if p > 0 then
    begin
      // divided with spaces at beginning and end
      b := (p = 1) and (v[p + System.Length(sout)] = ' ');
      b := b or ((p = System.Length(v) - System.Length(sout) + 1) and
        (v[p - 1] = ' '));
      // divided with spaces or brackets, that must be an eben number
      if not b then
      begin
        b := (v[p - 1] = '(') and (v[p + System.Length(sout)] = ')');
        b := b or (v[p - 1] = ')') and (v[p + System.Length(sout)] = '(');
        b := b or (v[p - 1] = ' ') and (v[p + System.Length(sout)] = ' ');
      end;
    end;
    if b then
    begin
      System.Delete(v, p, System.Length(sout));
      System.Insert(sin, v, p);
      Result := Result + System.Copy(v, 1, p + System.Length(sin));
      System.Delete(v, 1, p + System.Length(sin));
    end;
  until not b;
  Result := Result + v;
end;

function StrIsBool(const s: string): boolean;
var
  v: string;
begin
  v := strupper(s);
  result := (v = 'FALSE') or (v = 'TRUE');
end;

function BoolToStr(v: boolean): string;
begin
  if v then
    result := 'TRUE'
  else
    result := 'FALSE';
end;

function StrToBool(v: string): boolean;
begin
  result := strupper(v) = 'TRUE';
end;

////////////////////////////////////////////////////////////////////////////////
// TEvalFunction
constructor TEvalFunction.Create(aname: string; afunc: TObjFunc; anum: integer);
begin
  Inherited Create;
  Name := aname;
  FEvalObjFunc := afunc;
  FEvalFunc := nil;
  FNumParams := anum;
end;

constructor TEvalFunction.Create(aname: string; afunc: TExtFunc; anum: integer);
begin
  Inherited Create;
  Name := aname;
  FEvalObjFunc := nil;
  FEvalFunc := afunc;
  FNumParams := anum;
end;

function TEvalFunction.Value(p: TDStrings): string;
begin
  if (not Assigned(FEvalObjFunc)) and (not Assigned(FEvalFunc)) then
    I_Error('TEvalFunction.Value(): No function assigned for %s()', [FName]);
  if (p.Count <> FNumParams) and (FNumParams > -1) then
  begin
    I_Warning('TEvalFunction.Value(): Wrong number of parameters in function %s'#13#10, [FName]);
    result := '';
    exit;
  end;
  if Assigned(FEvalObjFunc) then
    result := FEvalObjFunc(p)
  else if Assigned(FEvalFunc) then
    result := FEvalFunc(p);
end;

function TEvalFunction.Value(v: string): string;
var
  h: TDStringList;
begin
  h := TDStringList.Create;
  SplitToList(v, h);
  Result := Value(h);
  h.Free;
end;

procedure TEvalFunction.SetName(v: string);
begin
  FName := strupper(v);
end;

////////////////////////////////////////////////////////////////////////////////
// TEvalList
constructor TEvalList.Create;
begin
  Inherited;
  fList := nil;
  fNumItems := 0;
  fRealNumItems := 0;
end;

destructor TEvalList.Destroy;
begin
  Clear;
  Inherited;
end;

function TEvalList.Get(Index: Integer): TObject;
begin
  if (Index < 0) or (Index >= fNumItems) then
    result := nil
  else
    result := fList[Index];
end;

procedure TEvalList.Put(Index: Integer; const value: TObject);
begin
  fList[Index] := value;
end;

procedure TEvalList.Grow;
var
  newrealitems: integer;
begin
  if fNumItems >= fRealNumItems then
  begin
    if fRealNumItems < 8 then
      newrealitems := 8
    else if fRealNumItems < 32 then
      newrealitems := 32
    else if fRealNumItems < 128 then
      newrealitems := fRealNumItems + 32
    else
      newrealitems := fRealNumItems + 64;
    realloc(pointer(fList), fRealNumItems * SizeOf(TObject), newrealitems * SizeOf(TObject));
    fRealNumItems := newrealitems;
  end;
end;

function TEvalList.Add(const value: TObject): integer;
begin
  Grow;
  Put(fNumItems, value);
  result := fNumItems;
  inc(fNumItems);
end;

procedure TEvalList.Insert(Index: Integer; Item: TObject);
begin
  if (Index < 0) or (Index > fNumItems) then
  begin
    I_Error('TEvalList.Insert(): Index (%d) out of bounds', [index]);
    exit;
  end;
  Grow;
  if Index < fNumItems then
    Move(FList^[Index], FList^[Index + 1], (fNumItems - Index) * SizeOf(Pointer));
  fList^[Index] := Item;
  Inc(fNumItems);
end;

function TEvalList.Delete(const Index: integer): boolean;
var
  i: integer;
begin
  if (Index < 0) or (Index >= fNumItems) then
  begin
    result := false;
    exit;
  end;

  fList[index].Free;
  for i := Index + 1 to fNumItems - 1 do
    fList[i - 1] := fList[i];

  dec(fNumItems);

  result := true;
end;

function TEvalList.IndexOf(const value: TObject): integer;
var
  i: integer;
begin
  for i := 0 to fNumItems - 1 do
    if fList[i] = value then
    begin
      result := i;
      exit;
    end;
  result := -1;
end;

procedure TEvalList.Clear;
var
  i: integer;
begin
  for i := 0 to fNumItems - 1 do
    fList[i].Free;
  realloc(pointer(fList), fRealNumItems * SizeOf(TObject), 0);
  fList := nil;
  fNumItems := 0;
  fRealNumItems := 0;
end;

////////////////////////////////////////////////////////////////////////////////
// TEvaluator
constructor TEvaluator.Create;
begin
  inherited Create;
  FRoot := nil;
  FExpr := '';
  FFindVar := nil;
  FVarValue := nil;
  // built-in functions
  AddFunc('STRCAT', PF_str_cat, 2);
  AddFunc('STRLEN', PF_str_len, 1);
  AddFunc('STRINS', PF_str_insert, 3);
  AddFunc('STRDEF', PF_str_delete, 3);
  AddFunc('STRPOS', PF_str_pos, 2);
  AddFunc('IF', PF_if, 3);
  AddFunc('IFF', PF_if, 3);
  // Math Functions
  AddFunc('VAL', PF_val, 1);
  AddFunc('ABS', PF_abs, 1);
  AddFunc('MIN', PF_min, -1);
  AddFunc('MAX', PF_max, -1);
  AddFunc('EXP', PF_exp, 1);
  AddFunc('LOG', PF_log, 1);
  AddFunc('LOG10', PF_log10, 1);
  AddFunc('LOG2', PF_log2, 1);
  AddFunc('CEIL', PF_ceil, 1);
  AddFunc('FLOOR', PF_floor, 1);
  AddFunc('ROUND', PF_round, 1);
  AddFunc('TRUNC', PF_trunc, 1);
  AddFunc('INT', PF_trunc, 1);
  AddFunc('SQR', PF_sqr, 1);
  AddFunc('SQRT', PF_sqrt, 1);
  AddFunc('FRAC', PF_frac, 1);
  AddFunc('POWER', PF_power, 1);
  // Trigonometry Functions
  AddFunc('SIN', PF_sin, 1);
  AddFunc('COS', PF_cos, 1);
  AddFunc('TAN', PF_tan, 1);
  AddFunc('ASIN', PF_asin, 1);
  AddFunc('ACOS', PF_acos, 1);
  AddFunc('ATAN', PF_atan, 1);
  AddFunc('SINH', PF_sinh, 1);
  AddFunc('COSH', PF_cosh, 1);
  AddFunc('TANH', PF_tanh, 1);
  AddFunc('ATAN2', PF_atan2, 2);
  AddFunc('VECTORANGLE', PF_VectorAngle, 2);
end;

destructor TEvaluator.Destroy;
begin
  ClearTree;
  inherited Destroy;
end;

procedure TEvaluator.ClearTree;

  procedure DoClear(v: TEvalNode);
  var
    i: integer;
  begin
    if not Assigned(v) then
      exit;
    for i := 0 to v.Count - 1 do
      DoClear(v[i]);
    while v.Count > 0 do
      v.Delete(0);
  end;

begin
  DoClear(FRoot);
  FreeAndNil(FRoot);
end;

function TEvaluator.Value: string;
begin
  Result := EvalTree(FRoot);
end;

function TEvaluator.EvaluateExpression(const aexpr: string): string;
begin
  SetExpr(aexpr);
  Result := EvalTree(FRoot);
end;


// List of functions is sorted to speed up search. No duplicate entries allowed

function ValidIdent(const Ident: string): Boolean;
const
  Alpha = ['A'..'Z', 'a'..'z', '_'];
  AlphaNumeric = Alpha + ['0'..'9'];
var
  I: Integer;
begin
  Result := False;
  if (Length(Ident) = 0) or not (Ident[1] in Alpha) then Exit;
  for I := 2 to Length(Ident) do if not (Ident[I] in AlphaNumeric) then Exit;
  Result := True;
end;

procedure TEvaluator.AddFunc(aname: string; afunc: TObjFunc; anump: integer);
begin
  aname := strupper(aname);
  if not ValidIdent(aname) then
    I_Error('TEvaluator.AddFunc(): Invalid function name "%s"', [aname]);
  if Assigned(FindFunc(aname)) then
    I_Error('TEvaluator.AddFunc(): Function name "%s" already used', [aname]);
  Insert(FindPos(aname), TEvalFunction.Create(aname, afunc, anump));
end;

procedure TEvaluator.AddFunc(aname: string; afunc: TExtFunc; anump: integer);
begin
  aname := strupper(aname);
  if not ValidIdent(aname) then
    I_Error('TEvaluator.AddFunc(): Invalid function name "%s"', [aname]);
  if Assigned(FindFunc(aname)) then
    I_Error('TEvaluator.AddFunc(): Function name "%s" already used', [aname]);
  Insert(FindPos(aname), TEvalFunction.Create(aname, afunc, anump));
end;

// private methods
procedure TEvaluator.SetExpr(v: string);
begin
  FExpr := ValidateExpression(v);
  Rebuild;
end;

function TEvaluator.EvalTree(p: TEvalNode): string;
var
  s: string;
  i: integer;
begin
  Result := p.FExpr;
  if p.FOperator <> '' then
  begin
    case p.FOperator of
      '/': Result := OP_Divide(EvalTree(p.Node[0]), EvalTree(p.Node[1]));
      '*': Result := OP_Multiply(EvalTree(p.Node[0]), EvalTree(p.Node[1]));
      '+': Result := OP_Add(EvalTree(p.Node[0]), EvalTree(p.Node[1]));
      '-': Result := OP_Subtract(EvalTree(p.Node[0]), EvalTree(p.Node[1]));
      '&': Result := OP_AND(EvalTree(p.Node[0]), EvalTree(p.Node[1]));
      '|': Result := OP_OR(EvalTree(p.Node[0]), EvalTree(p.Node[1]));
      '^': Result := OP_XOR(EvalTree(p.Node[0]), EvalTree(p.Node[1]));
      '=': Result := OP_Equal(EvalTree(p.Node[0]), EvalTree(p.Node[1]));
      '<': Result := OP_Less(EvalTree(p.Node[0]), EvalTree(p.Node[1]));
      '>': Result := OP_Greater(EvalTree(p.Node[0]), EvalTree(p.Node[1]));
      '!': Result := OP_Not(EvalTree(p.Node[0]), EvalTree(p.Node[1]));
    end;
  end;
  if Assigned(p.FFunction) then
  begin
    // build argument list
    s := '';
    for i := 0 to p.Count - 1 do
      s := s + ',' + EvalTree(p.Node[i]);
    System.Delete(s, 1, 1);
    // evaluate function
    Result := p.FFunction.Value(s);
  end;
end;

procedure TEvaluator.Rebuild;
begin
  ClearTree;
  FRoot := TEvalNode.Create;
  ProcessExpression(FExpr, FRoot);
end;

procedure TEvaluator.ProcessExpression(v: string; p: TEvalNode);

  function MarkPos(s: string; c: charset_t): integer;
  var
    i, bracket: integer;
    quote: boolean;
  begin
    Result := -1;
    i := Length(s);
    bracket := 0;
    quote := false;
    while (Result < 0) and (i > 0) do
    begin
      if s[i] = '"' then
        quote := not quote;
      if (not quote) and (s[i] = '(') then
        inc(bracket);
      if (not quote) and (s[i] = ')') then
        dec(bracket);
      if (not quote) and (bracket = 0) and (s[i] in c) then
        Result := i;
      dec(i);
    end;
  end;

  function StrIsFunc(s: string): boolean;
  begin
    Result := v[1] in AllowedFirstChar;
  end;

  function StrIsVar(s: string): boolean;
  begin
    Result := false;
    if Assigned(FFindVar) then
      Result := FFindVar(s);
  end;

var
  h: integer;
  fn, fp, s: string;
begin
  if v = '' then
    exit;
  // remove enclosing brackets
  if MarkPos(v, Marks) < 0 then
  begin
    repeat
      if (v[1] = '(') and (v[Length(v)] = ')') then
        v := System.Copy(v, 2, Length(v) - 2);
    until (v[1] <> '(') or (v[Length(v)] <> ')');
  end;
  h := MarkPos(v, LowMarks);
  if h = 1 then
  begin
    v := '0' + v;
    h := 2;
  end;
  if h < 0 then
    h := MarkPos(v, BoolMarks);
  if h < 0 then
    h := MarkPos(v, HighMarks);
  if h = System.Length(v) then
    I_Warning('TEvaluator.ProcessExpression(): Invalid expression "%s"'#13#10, [v]);
  if h > -1 then
  begin
    p.FOperator := v[h];
    ProcessExpression(System.Copy(v, 1, h - 1), p.AddNode);
    ProcessExpression(System.Copy(v, h + 1, System.Length(v)), p.AddNode);
  end
  else
  begin
    if StrIsVar(v) then
      v := fVarValue(v);
    if StrIsFunc(v) then
    begin
      // divide in name and parameters
      fn := v;
      fp := '';
      h := System.Pos('(', fn);
      if h > 0 then
      begin
        fn := System.Copy(v, 1, h - 1);
        fp := System.Copy(v, h + 1, System.Length(v));
        System.Delete(fp, System.Length(fp), 1);
      end;
      if (fp = '') and ((fn = 'TRUE') or (fn = 'FALSE') or (fn = 'INF')) then
        p.FExpr := fn
      else
      begin
        p.FFunction := FindFunc(fn);
        if not Assigned(p.FFunction) then
          I_Warning('TEvaluator.ProcessExpression(): Unknown function "%s"'#13#10, [fn]);
        // one node for each argument
        while fp <> '' do
        begin
          Split(s, fp);
          ProcessExpression(s, p.AddNode);
        end;
      end;
    end
    else
      p.FExpr := v
  end;
end;

function TEvaluator.ValidateExpression(v: string): string;
var
  braclevel, i: Integer;
  quote: boolean;
begin
  Result := '0';
  quote := false;
  braclevel := 0;
  for i := 1 to Length(v) do
  begin
    if v[i] = '"' then
      quote := not quote;
    if (not quote) and (v[i] = '(') then
      inc(braclevel);
    if (not quote) and (v[i] = ')') then
      dec(braclevel);
    if not quote then
    begin
      if not ((v[i] in AllowedChar) or (v[i] in InputMarks) or (v[i] = '.')) then
      begin
        I_Warning('TEvaluator.ValidateExpression(): Illegal character ' + v[i] + ' while validating "%s"'#13#10, [v]);
        v[i] := '_';
      end
      else
        v[i] := toupper(v[i]);
    end;
  end;
  if braclevel <> 0 then
    I_Warning('TEvaluator.ValidateExpression(): Wrong number of brackets "%s"'#13#10, [v]);

  // substitute boolean and multi-character operators
  v := SubstituteWords(v, 'AND', '&');
  v := SubstituteWords(v, 'XOR', '^');
  v := SubstituteWords(v, 'OR', '|');
  v := SubstituteWords(v, 'NOT', 'TRUE^');
  v := SubstituteMarks(v, '<>', '!');
  v := SubstituteMarks(v, '!=', '!');
  v := SubstituteMarks(v, '==', '=');

  // remove spaces
  i := 0;
  quote := false;
  while i < Length(v) do
  begin
    inc(i);
    if v[i] = '"' then
      quote := not quote;
    if (not quote) and (v[i] = ' ') then
    begin
      System.Delete(v, i, 1);
      dec(i);
    end;
  end;
  Result := v;
end;

function TEvaluator.FindFunc(AName: string): TEvalFunction;
var
  a, b, m: integer;
begin
  Result := nil;
  if Count = 0 then
    exit;
  a := 0;
  b := Count - 1;
  while (b - a > 1) do
  begin
    m := (a + b) div 2;
    if AName > TEvalFunction(Objects[m]).Name then
      a := m
    else
      b := m;
  end;

  if TEvalFunction(Objects[a]).Name = AName then
    Result := TEvalFunction(Objects[a]);
  if TEvalFunction(Objects[b]).Name = AName then
    Result := TEvalFunction(Objects[b]);
end;

// looks for insert position

function TEvaluator.FindPos(v: string): integer;
var
  a, b, m: integer;
begin
  Result := 0;
  if Count = 0 then
    exit;
  if v < TEvalFunction(Objects[0]).Name then
    exit;
  if v > TEvalFunction(Objects[Count - 1]).Name then
  begin
    Result := Count;
    exit;
  end;
  a := 0;
  b := Count - 1;
  while b - a > 1 do
  begin
    m := (a + b) div 2;
    if v < TEvalFunction(Objects[m]).Name then
      b := m
    else
      a := m;
  end;
  Result := b;
end;

////////////////////////////////////////////////////////////////////////////////
// arithmetic operators
////////////////////////////////////////////////////////////////////////////////
procedure TEvaluator._err_incompatible_types(const callfunc: string; const s1, s2: string);
begin
  I_Warning('TEvaluator.' + callfunc + '(): Incompatible types ("%s", "%s)'#13#10, [s1, s2]);
end;

function TEvaluator.OP_Add(v1, v2: string): string;
begin
  if StrIsFloat(v1) and StrIsFloat(v2) then
    Result := ftoa(atof(v1) + atof(v2))
  else if (v1 = 'INF') or (v2 = 'INF') then
    Result := 'INF'
  else
      _err_incompatible_types('OP_Add', v1, v2);
end;

function TEvaluator.OP_Subtract(v1, v2: string): string;
begin
  if StrIsFloat(v1) and StrIsFloat(v2) then
    Result := ftoa(atof(v1) - atof(v2))
  else if v1 = 'INF' then
  begin
    if v2 <> 'INF' then
      Result := 'INF'
    else
      Result := '0';
  end
  else
    _err_incompatible_types('OP_Subtract', v1, v2);
end;

function TEvaluator.OP_Multiply(v1, v2: string): string;
begin
  if StrIsFloat(v1) and StrIsFloat(v2) then
    Result := ftoa(atof(v1) * atof(v2))
  else if (v1 = 'INF') or (v2 = 'INF') then
    Result := 'INF'
  else
    _err_incompatible_types('OP_Multiply', v1, v2);
end;

function TEvaluator.OP_Divide(v1, v2: string): string;
begin
  if StrIsFloat(v1) and StrIsFloat(v2) then
    Result := ftoa(atof(v1) / atof(v2))
  else if (v1 = 'INF') and (v2 = 'INF') then
    Result := '1'
  else if v1 = 'INF' then
    Result := 'INF'
  else if v2 = 'INF' then
    Result := '0'
  else
    _err_incompatible_types('OP_Divide', v1, v2);
end;

function TEvaluator.OP_And(v1, v2: string): string;
begin
  if StrIsBool(v1) and StrIsBool(v2) then
    Result := BoolToStr(StrToBool(v1) and StrToBool(v2))
  else if StrIsFloat(v1) and StrIsFloat(v2) then
    Result := itoa(round(atof(v1)) and round(atof(v2)))
  else
    _err_incompatible_types('OP_And', v1, v2);
end;

function TEvaluator.OP_Or(v1, v2: string): string;
begin
  if StrIsBool(v1) and StrIsBool(v2) then
    Result := BoolToStr(StrToBool(v1) or StrToBool(v2))
  else if StrIsFloat(v1) and StrIsFloat(v2) then
    Result := itoa(round(atof(v1)) or round(atof(v2)))
  else
    _err_incompatible_types('OP_OR', v1, v2);
end;

function TEvaluator.OP_XOR(v1, v2: string): string;
begin
  if StrIsBool(v1) and StrIsBool(v2) then
    Result := BoolToStr(StrToBool(v1) xor StrToBool(v2))
  else if StrIsFloat(v1) and StrIsFloat(v2) then
    Result := itoa(round(atof(v1)) xor round(atof(v2)))
  else if StrIsBool(v1) and StrIsFloat(v2) then
  begin
    if v1 = 'TRUE' then
      Result := itoa($FFFFFFFF xor round(atof(v2)))
    else
      Result := itoa(round(atof(v2)))
  end
  else if StrIsFloat(v1) and StrIsBool(v2) then
  begin
    if v2 = 'TRUE' then
      Result := itoa(round(atof(v1)) xor $FFFFFFFF)
    else
      Result := itoa(round(atof(v1)))
  end
  else
    _err_incompatible_types('OP_XOR', v1, v2);
end;

function TEvaluator.OP_Equal(v1, v2: string): string;
begin
  Result := BoolToStr(v1 = v2);
end;

function TEvaluator.OP_Less(v1, v2: string): string;
begin
  if StrIsFloat(v1) and StrIsFloat(v2) then
    Result := BoolToStr(atof(v1) < atof(v2))
  else
    Result := BoolToStr(v1 < v2);
end;

function TEvaluator.OP_Greater(v1, v2: string): string;
begin
  if StrIsFloat(v1) and StrIsFloat(v2) then
    Result := BoolToStr(atof(v1) > atof(v2))
  else
    Result := BoolToStr(v1 > v2);
end;

function TEvaluator.OP_Not(v1, v2: string): string;
begin
  Result := BoolToStr(v1 <> v2);
end;

////////////////////////////////////////////////////////////////////////////////
// TMParser built-in functions
////////////////////////////////////////////////////////////////////////////////

function TEvaluator.PF_str_cat(p: TDStrings): string;
begin
  Result := p[0] + p[1];
  System.Delete(Result, System.Length(p[0]), 2);
end;

function TEvaluator.PF_str_len(p: TDStrings): string;
begin
  Result := itoa(System.Length(p[0]) - 2);
end;

// merges p[0] into p[1] at position p[2]
function TEvaluator.PF_str_insert(p: TDStrings): string;
begin
  Result := StripStr(p[1]);
  System.Insert(StripStr(p[0]), Result, atoi(p[2]));
end;

function TEvaluator.PF_str_delete(p: TDStrings): string;
begin
  Result := StripStr(p[0]);
  System.Delete(Result, atoi(p[1]), atoi(p[2]));
end;

function TEvaluator.PF_str_pos(p: TDStrings): string;
begin
  Result := itoa(System.Pos(StripStr(p[0]), StripStr(p[1])));
end;

function TEvaluator.PF_if(p: TDStrings): string;
begin
  if StrIsBool(p[0]) then
  begin
    if StrToBool(p[0]) then
      result := p[1]
    else
      result := p[2];
  end
  else if atof(p[0]) <> 0 then
    result := p[1]
  else
    result := p[2];
end;

function TEvaluator.PF_val(p: TDStrings): string;
begin
  Result := p[0];
end;

function TEvaluator.PF_abs(p: TDStrings): string;
begin
  Result := p[0];
  if p[0][1] = '-' then
    System.Delete(Result, 1, 1);
end;

function TEvaluator.PF_min(p: TDStrings): string;
var
  fmin, f1: float;
  i: integer;
begin
  if p.Count < 2 then
    I_Warning('TEvaluator.PF_min(): At least 2 parameters expected'#13#10);
  fmin := 3E38;
  for i := 0 to p.Count - 1 do
  begin
    f1 := atof(p[i]);
    if f1 < fmin then
      fmin := f1;
  end;
  result := ftoa(fmin);
end;

function TEvaluator.PF_max(p: TDStrings): string;
var
  fmax, f1: float;
  i: integer;
begin
  if p.Count < 2 then
    I_Warning('TEvaluator.PF_max(): At least 2 parameters expected'#13#10);
  fmax := -3E38;
  for i := 0 to p.Count - 1 do
  begin
    f1 := atof(p[i]);
    if f1 > fmax then
      fmax := f1;
  end;
  result := ftoa(fmax);
end;

function TEvaluator.PF_exp(p: TDStrings): string;
begin
  result := ftoa(exp(atof(p[0])));
end;

function TEvaluator.PF_log(p: TDStrings): string;
begin
  result := ftoa(ln(atof(p[0])));
end;

function TEvaluator.PF_log10(p: TDStrings): string;
begin
  result := ftoa(ln(atof(p[0]) * 0.4342944819032518));
end;

function TEvaluator.PF_log2(p: TDStrings): string;
begin
  result := ftoa(ln(atof(p[0]) * 1.4426950408889634));
end;

function TEvaluator.PF_ceil(p: TDStrings): string;
begin
  result := ftoa(ceil(atof(p[0])));
end;

function TEvaluator.PF_floor(p: TDStrings): string;
begin
  result := ftoa(floor(atof(p[0])));
end;

function TEvaluator.PF_round(p: TDStrings): string;
begin
  result := ftoa(round(atof(p[0])));
end;

function TEvaluator.PF_trunc(p: TDStrings): string;
begin
  result := ftoa(trunc(atof(p[0])));
end;

function TEvaluator.PF_sqr(p: TDStrings): string;
begin
  result := ftoa(sqr(atof(p[0])));
end;

function TEvaluator.PF_sqrt(p: TDStrings): string;
begin
  result := ftoa(sqrt(atof(p[0])));
end;

function TEvaluator.PF_frac(p: TDStrings): string;
begin
  result := ftoa(frac(atof(p[0])));
end;

function TEvaluator.PF_power(p: TDStrings): string;
begin
  if (p[0] = 'INF') or (p[1] = 'INF') then
    Result := 'INF'
  else
    Result := ftoa(Exp(atof(p[1]) * ln(atof(p[0]))));
end;

function TEvaluator.PF_sin(p: TDStrings): string;
begin
  result := ftoa(sin(atof(p[0]) / 360 * 2 * pi));
end;

function TEvaluator.PF_cos(p: TDStrings): string;
begin
  result := ftoa(cos(atof(p[0]) / 360 * 2 * pi));
end;

function TEvaluator.PF_tan(p: TDStrings): string;
begin
  result := ftoa(tan(atof(p[0]) / 360 * 2 * pi));
end;

function TEvaluator.PF_asin(p: TDStrings): string;
begin
  result := ftoa(arcsin(atof(p[0]) / 360 * 2 * pi));
end;

function TEvaluator.PF_acos(p: TDStrings): string;
begin
  result := ftoa(arccos(atof(p[0]) / 360 * 2 * pi));
end;

function TEvaluator.PF_atan(p: TDStrings): string;
begin
  result := ftoa(arctan(atof(p[0]) / 360 * 2 * pi));
end;

function TEvaluator.PF_sinh(p: TDStrings): string;
begin
  result := ftoa(sinh(atof(p[0]) / 360 * 2 * pi));
end;

function TEvaluator.PF_cosh(p: TDStrings): string;
begin
  result := ftoa(cosh(atof(p[0]) / 360 * 2 * pi));
end;

function TEvaluator.PF_tanh(p: TDStrings): string;
begin
  result := ftoa(tanh(atof(p[0]) / 360 * 2 * pi));
end;

function TEvaluator.PF_atan2(p: TDStrings): string;
begin
  result := ftoa(arctan2(atof(p[0]) / 360 * 2 * pi, atof(p[1]) / 360 * 2 * pi));
end;

function TEvaluator.PF_VectorAngle(p: TDStrings): string;
begin
  result := ftoa(arctan2(atof(p[1]) / 360 * 2 * pi, atof(p[0]) / 360 * 2 * pi));
end;

////////////////////////////////////////////////////////////////////////////////
// TEvalNode
constructor TEvalNode.Create;
begin
  FOperator := chr(0);
  FExpr := '';
  FFunction := nil;
  Inherited;
end;

function TEvalNode.AddNode: TEvalNode;
begin
  Result := Node[Add(TEvalNode.Create)];
end;

function TEvalNode.GetNode(index: integer): TEvalNode;
begin
  Result := Objects[index] as TEvalNode;
end;

end.

