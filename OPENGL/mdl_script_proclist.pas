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
//  DESCRIPTION:
//  DDMODEL Script functions import
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit mdl_script_proclist;

interface

uses
  d_delphi,
  ps_proclist,
  ps_compiler,
  ps_runtime;

type
  TMDLProcedureList = class(TObject)
  private
    fList: Pprocitem_tArray;
    fNumItems: integer;
    fRealSize: integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Add(const decl: string; const proc: pointer); virtual;
    procedure RegisterProcsComp(Sender: TPSPascalCompiler); virtual;
    procedure RegisterProcsExec(Sender: TPSExec); virtual;
    procedure Reset;
    function GetDeclarations: string;
    function GetFunctionNames: string;
    property Count: integer read fNumItems;
  end;

//==============================================================================
//
// MDL_InitProcList
//
//==============================================================================
procedure MDL_InitProcList;

//==============================================================================
//
// MDL_ShutDownProcList
//
//==============================================================================
procedure MDL_ShutDownProcList;

//==============================================================================
//
// MDL_RegisterProcsCompiler
//
//==============================================================================
procedure MDL_RegisterProcsCompiler(const C: TPSPascalCompiler);

//==============================================================================
//
// MDL_RegisterProcsExec
//
//==============================================================================
procedure MDL_RegisterProcsExec(const E: TPSExec);

//==============================================================================
//
// MDL_Procs
//
//==============================================================================
function MDL_Procs: string;

implementation

uses
  SysUtils,
  mdl_script_model,
  mdl_script_functions;

//==============================================================================
//
// TMDLProcedureList.Create
//
//==============================================================================
constructor TMDLProcedureList.Create;
begin
  fList := nil;
  fNumItems := 0;
  fRealSize := 0;
  inherited Create;
end;

//==============================================================================
//
// TMDLProcedureList.Destroy
//
//==============================================================================
destructor TMDLProcedureList.Destroy;
var
  i: integer;
begin
  if fRealSize > 0 then
  begin
    for i := 0 to fRealSize - 1 do
    begin
      fList[i].decl.Free;
      fList[i].name.Free;
      fList[i].exportdecl.Free;
    end;
    ReallocMem(fList, 0);
  end;

  inherited;
end;

//==============================================================================
//
// TMDLProcedureList.Add
//
//==============================================================================
procedure TMDLProcedureList.Add(const decl: string; const proc: pointer);
const
  REALLOCSTEP = 16;
var
  i: integer;
  decl1: string;
begin
  decl1 := Trim(decl);
  if decl1 = '' then
    Exit;

  if decl1[Length(decl1)] <> ';' then
    decl1 := decl1 + ';';
  if fNumItems >= fRealSize then
  begin
    ReallocMem(fList, (fRealSize + REALLOCSTEP) * SizeOf(procitem_t));
    for i := fRealSize to fRealSize + REALLOCSTEP - 1 do
    begin
      fList[i].decl := TString.Create('');
      fList[i].name := TString.Create('');
      fList[i].exportdecl := TString.Create('');
    end;
    fRealSize := fRealSize + REALLOCSTEP;
  end;
  fList[fNumItems].proc := proc;
  fList[fNumItems].decl.str := decl1;
  fList[fNumItems].exportdecl.str := decl1;
  fList[fNumItems].iscomputed := true;
  inc(fNumItems);
end;

//==============================================================================
//
// TMDLProcedureList.RegisterProcsComp
//
//==============================================================================
procedure TMDLProcedureList.RegisterProcsComp(Sender: TPSPascalCompiler);
var
  i: integer;
  reg: TPSRegProc;
begin
  for i := 0 to fNumItems - 1 do
  begin
    reg := Sender.AddDelphiFunction(fList[i].decl.str);
    if reg <> nil then
    begin
      fList[i].name.str := reg.Name;
      fList[i].iscomputed := true;
    end;
  end;
end;

//==============================================================================
//
// TMDLProcedureList.RegisterProcsExec
//
//==============================================================================
procedure TMDLProcedureList.RegisterProcsExec(Sender: TPSExec);
var
  i: integer;
begin
  for i := 0 to fNumItems - 1 do
    if fList[i].iscomputed then
      Sender.RegisterDelphiFunction(fList[i].proc, fList[i].name.str, cdRegister);
end;

//==============================================================================
//
// TMDLProcedureList.Reset
//
//==============================================================================
procedure TMDLProcedureList.Reset;
var
  i: integer;
begin
  for i := 0 to fNumItems - 1 do
    fList[i].iscomputed := false;
end;

//==============================================================================
//
// TMDLProcedureList.GetDeclarations
//
//==============================================================================
function TMDLProcedureList.GetDeclarations: string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to fNumItems - 1 do
    Result := Result + flist[i].exportdecl.str + #13#10;
end;

//==============================================================================
//
// TMDLProcedureList.GetFunctionNames
//
//==============================================================================
function TMDLProcedureList.GetFunctionNames: string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to fNumItems - 1 do
    Result := Result + flist[i].name.str + #13#10;
end;

var
  proclist: TMDLProcedureList;

//==============================================================================
//
// MDL_InitProcList
//
//==============================================================================
procedure MDL_InitProcList;
begin
  proclist := TMDLProcedureList.Create;

  //--- base functions
  proclist.Add('procedure Write(const parm: string);', @MDLS_Write);
  proclist.Add('procedure WriteFmt(const Fmt: string; const args: array of const);', @MDLS_WriteFmt);
  proclist.Add('procedure Writeln(const parm: string);', @MDLS_Writeln);
  proclist.Add('procedure WritelnFmt(const Fmt: string; const args: array of const);', @MDLS_WritelnFmt);
  proclist.Add('procedure BreakPoint(const msg: string);', @MDLS_BreakPoint);
  proclist.Add('function Tan(const parm: Extended): Extended;', @MDLS_Tan);
  proclist.Add('function Sin360(const parm: Extended): Extended;', @MDLS_Sin360);
  proclist.Add('function Cos360(const parm: Extended): Extended;', @MDLS_Cos360);
  proclist.Add('function Tan360(const parm: Extended): Extended;', @MDLS_Tan360);
  proclist.Add('function Format(const Fmt: string; const args: array of const): string;', @MDLS_Format);
  proclist.Add('function IFI(const condition: boolean; const iftrue, iffalse: Int64): Int64;', @MDLS_IFI);
  proclist.Add('function IFF(const condition: boolean; const iftrue, iffalse: Extended): Extended;', @MDLS_IFF);
  proclist.Add('function IFS(const condition: boolean; const iftrue, iffalse: string): string;', @MDLS_IFS);
  proclist.Add('function Odd(const x: integer): boolean;', @MDLS_Odd);
  proclist.Add('function Even(const x: integer): boolean;', @MDLS_Even);
  proclist.Add('function MergeIntegerArrays(const A1, A2: TIntegerArray): TIntegerArray;', @MDLS_MergeIntegerArrays);
  proclist.Add('function MergeInt64Arrays(const A1, A2: TInt64Array): TInt64Array;', @MDLS_MergeInt64Arrays);
  proclist.Add('function MergeLongWordArrays(const A1, A2: TLongWordArray): TLongWordArray;', @MDLS_MergeLongWordArrays);
  proclist.Add('function MergeSingleArrays(const A1, A2: TSingleArray): TSingleArray;', @MDLS_MergeSingleArrays);
  proclist.Add('function MergeFloatArrays(const A1, A2: TFloatArray): TFloatArray;', @MDLS_MergeSingleArrays);
  proclist.Add('function MergeDoubleArrays(const A1, A2: TDoubleArray): TDoubleArray;', @MDLS_MergeDoubleArrays);
  proclist.Add('function MergeExtendedArrays(const A1, A2: TExtendedArray): TExtendedArray;', @MDLS_MergeExtendedArrays);
  proclist.Add('function IsPrime(const N: Int64): Boolean;', @MDLS_IsPrime);
  proclist.Add('function IsIntegerInRange(const test, f1, f2: integer): boolean;', @MDLS_IsIntegerInRange);
  proclist.Add('function IsLongWordInRange(const test, f1, f2: LongWord): boolean;', @MDLS_IsLongWordInRange);
  proclist.Add('function IsFloatInRange(const test, f1, f2: single): boolean;', @MDLS_IsFloatInRange);
  proclist.Add('function IsSingleInRange(const test, f1, f2: single): boolean;', @MDLS_IsFloatInRange);
  proclist.Add('function IsDoubleInRange(const test, f1, f2: double): boolean;', @MDLS_IsDoubleInRange);
  proclist.Add('function IsExtendedInRange(const test, f1, f2: Extended): boolean;', @MDLS_IsExtendedInRange);
  proclist.Add('function Sqr(const x: Extended): Extended;', @MDLS_Sqr);
  proclist.Add('function Sqrt(const x: Extended): Extended;', @MDLS_Sqrt);
  proclist.Add('function Cube(const x: Extended): Extended;', @MDLS_Cube);
  proclist.Add('function ArcCos(const X : Extended) : Extended;', @MDLS_ArcCos);
  proclist.Add('function ArcSin(const X : Extended) : Extended;', @MDLS_ArcSin);
  proclist.Add('function ArcTan2(const Y, X: Extended): Extended;', @MDLS_ArcTan2);
  proclist.Add('procedure SinCosE(const Theta: Extended; var S, C: Extended);', @MDLS_SinCosE);
  proclist.Add('procedure SinCosD(const Theta: Extended; var S, C: Double);', @MDLS_SinCosD);
  proclist.Add('procedure SinCosF(const Theta: Extended; var S, C: Single);', @MDLS_SinCosF);
  proclist.Add('function Cosh(const X: Extended): Extended;', @MDLS_Cosh);
  proclist.Add('function Sinh(const X: Extended): Extended;', @MDLS_Sinh);
  proclist.Add('function Tanh(const X: Extended): Extended;', @MDLS_Tanh);
  proclist.Add('function ArcCosh(const X: Extended): Extended;', @MDLS_ArcCosh);
  proclist.Add('function ArcSinh(const X: Extended): Extended;', @MDLS_ArcSinh);
  proclist.Add('function ArcTanh(const X: Extended): Extended;', @MDLS_ArcTanh);
  proclist.Add('function Log10(const X: Extended): Extended;', @MDLS_Log10);
  proclist.Add('function Log2(const X: Extended): Extended;', @MDLS_Log2);
  proclist.Add('function Ln(const X: Extended): Extended;', @MDLS_Ln);
  proclist.Add('function LogN(const Base, X: Extended): Extended;', @MDLS_LogN);
  proclist.Add('function IntPower(const Base: Extended; const Exponent: Integer): Extended;', @MDLS_IntPower);
  proclist.Add('function Power(const Base, Exponent: Extended): Extended;', @MDLS_Power);
  proclist.Add('function Ceil(const X: Extended): Integer;', @MDLS_Ceil);
  proclist.Add('function Floor(const X: Extended): Integer;', @MDLS_Floor);
  //--- model drawing functions (OpenGL wrapper)
  proclist.Add('procedure glBegin(const mode: GLenum);', @MDLS_glBegin);
  proclist.Add('procedure glEnd;', @MDLS_glEnd);
  proclist.Add('procedure glTexCoord2f(const s, t: GLfloat);', @MDLS_glTexCoord2f);
  proclist.Add('procedure glVertex3f(const x, y, z: GLfloat);', @MDLS_glVertex3f);
  proclist.Add('procedure glColor3f(const r, g, b: GLfloat);', @MDLS_glColor3f);
  proclist.Add('procedure glColor4f(const r, g, b, a: GLfloat);', @MDLS_glColor4f);
  proclist.Add('procedure glNormal3f(const nx, ny, nz: GLfloat);', @MDLS_glNormal3f);
  proclist.Add('procedure glMatrixMode(const mode: LongWord);', @MDLS_glMatrixMode);
  proclist.Add('procedure glPushMatrix;', @MDLS_glPushMatrix);
  proclist.Add('procedure glPopMatrix;', @MDLS_glPopMatrix);
  proclist.Add('procedure glTranslatef(const x, y, z: GLfloat);', @MDLS_glTranslatef);
  proclist.Add('procedure glRotatef(const a, x, y, z: GLfloat);', @MDLS_glRotatef);
  proclist.Add('procedure glLoadIdentity;', @MDLS_glLoadIdentity);
  proclist.Add('procedure glScalef(const x, y, z: GLfloat);', @MDLS_glScalef);
  proclist.Add('procedure SetFrame(const frm: integer);', @MDLS_SetFrame);
  proclist.Add('procedure CallFrame(const frm: integer);', @MDLS_CallFrame);
end;

//==============================================================================
//
// MDL_ShutDownProcList
//
//==============================================================================
procedure MDL_ShutDownProcList;
begin
  proclist.Free;
end;

//==============================================================================
//
// MDL_RegisterProcsCompiler
//
//==============================================================================
procedure MDL_RegisterProcsCompiler(const C: TPSPascalCompiler);
begin
  proclist.RegisterProcsComp(C);
end;

//==============================================================================
//
// MDL_RegisterProcsExec
//
//==============================================================================
procedure MDL_RegisterProcsExec(const E: TPSExec);
begin
  proclist.RegisterProcsExec(E);
end;

//==============================================================================
//
// MDL_Procs
//
//==============================================================================
function MDL_Procs: string;
begin
  MDL_InitProcList;
  result := proclist.GetDeclarations;
  MDL_ShutDownProcList;
end;

end.
