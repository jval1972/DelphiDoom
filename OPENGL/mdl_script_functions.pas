//------------------------------------------------------------------------------
//
//  DelphiDoom: A modified and improved DOOM engine for Windows
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 1993-1996 by id Software, Inc.
//  Copyright (C) 2004-2020 by Jim Valavanis
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
//  Script functions library for DD_MODELs
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit mdl_script_functions;

interface

// --------------------------- general -----------------------------------------

type
  TDynamicIntegerArray = array of Integer;
  TDynamicInt64Array = array of Int64;
  TDynamicLongWordArray = array of LongWord;
  TDynamicSingleArray = array of single;
  TDynamicDoubleArray = array of double;
  TDynamicExtendedArray = array of extended;

procedure MDLS_Write(const parm: string);

procedure MDLS_WriteFmt(const Fmt: string; const args: array of const);

procedure MDLS_Writeln(const parm: string);

procedure MDLS_WritelnFmt(const Fmt: string; const args: array of const);

procedure MDLS_BreakPoint(const msg: string);

function MDLS_Tan(const parm: Extended): Extended;

function MDLS_Sin360(const parm: Extended): Extended;

function MDLS_Cos360(const parm: Extended): Extended;

function MDLS_Tan360(const parm: Extended): Extended;

function MDLS_Format(const Fmt: string; const args: array of const): string;

function MDLS_IFI(const condition: boolean; const iftrue, iffalse: Int64): Int64;

function MDLS_IFF(const condition: boolean; const iftrue, iffalse: Extended): Extended;

function MDLS_IFS(const condition: boolean; const iftrue, iffalse: string): string;

function MDLS_Odd(const x: integer): boolean;

function MDLS_Even(const x: integer): boolean;

function MDLS_MergeIntegerArrays(const A1, A2: TDynamicIntegerArray): TDynamicIntegerArray;

function MDLS_MergeInt64Arrays(const A1, A2: TDynamicInt64Array): TDynamicInt64Array;

function MDLS_MergeLongWordArrays(const A1, A2: TDynamicLongWordArray): TDynamicLongWordArray;

function MDLS_MergeSingleArrays(const A1, A2: TDynamicSingleArray): TDynamicSingleArray;

function MDLS_MergeDoubleArrays(const A1, A2: TDynamicDoubleArray): TDynamicDoubleArray;

function MDLS_MergeExtendedArrays(const A1, A2: TDynamicExtendedArray): TDynamicExtendedArray;

function MDLS_IsPrime(const N: Int64): Boolean;

function MDLS_IsIntegerInRange(const test, f1, f2: integer): boolean;

function MDLS_IsLongWordInRange(const test, f1, f2: LongWord): boolean;

function MDLS_IsFloatInRange(const test, f1, f2: single): boolean;

function MDLS_IsDoubleInRange(const test, f1, f2: double): boolean;

function MDLS_IsExtendedInRange(const test, f1, f2: Extended): boolean;

function MDLS_Sqr(const x: Extended): Extended;

function MDLS_Sqrt(const x: Extended): Extended;

function MDLS_Cube(const x: Extended): Extended;

function MDLS_ArcCos(const X: Extended): Extended;

function MDLS_ArcSin(const X: Extended): Extended;

function MDLS_ArcTan2(const Y, X: Extended): Extended;

procedure MDLS_SinCosE(const Theta: Extended; var S, C: Extended);

procedure MDLS_SinCosD(const Theta: Extended; var S, C: Double);

procedure MDLS_SinCosF(const Theta: Extended; var S, C: Single);

function MDLS_Cosh(const X: Extended): Extended;

function MDLS_Sinh(const X: Extended): Extended;

function MDLS_Tanh(const X: Extended): Extended;

function MDLS_ArcCosh(const X: Extended): Extended;

function MDLS_ArcSinh(const X: Extended): Extended;

function MDLS_ArcTanh(const X: Extended): Extended;

function MDLS_Log10(const X: Extended): Extended;

function MDLS_Log2(const X: Extended): Extended;

function MDLS_Ln(const X: Extended): Extended;

function MDLS_LogN(const Base, X: Extended): Extended;

function MDLS_IntPower(const Base: Extended; const Exponent: Integer): Extended;

function MDLS_Power(const Base, Exponent: Extended): Extended;

function MDLS_Ceil(const X: Extended):Integer;

function MDLS_Floor(const X: Extended): Integer;

implementation

uses
  d_delphi,
  SysUtils,
  Math;

procedure MDLS_Write(const parm: string);
begin
  printf(parm);
end;

procedure MDLS_WriteFmt(const Fmt: string; const args: array of const);
begin
  MDLS_Write(MDLS_Format(Fmt, args));
end;

procedure MDLS_Writeln(const parm: string);
begin
  printf(parm + #13#10);
end;

procedure MDLS_WritelnFmt(const Fmt: string; const args: array of const);
begin
  MDLS_Writeln(MDLS_Format(Fmt, args));
end;

var
  bpmsg: string = '';

// Actually for debuging the engine, not script
procedure MDLS_BreakPoint(const msg: string);
begin
  bpmsg := msg;
end;

function MDLS_Tan(const parm: Extended): Extended;
begin
  Result := tan(parm);
end;

function MDLS_Sin360(const parm: Extended): Extended;
begin
  Result := sin(parm / 360 * 2 * pi);
end;

function MDLS_Cos360(const parm: Extended): Extended;
begin
  Result := cos(parm / 360 * 2 * pi);
end;

function MDLS_Tan360(const parm: Extended): Extended;
begin
  Result := tan(parm / 360 * 2 * pi);
end;

function MDLS_Format(const Fmt: string; const args: array of const): string;
begin
  try
    Result := Format(Fmt, Args);
  except
    Result := Fmt;
  end;
end;

function MDLS_IFI(const condition: boolean; const iftrue, iffalse: Int64): Int64;
begin
  if condition then
    Result := iftrue
  else
    Result := iffalse;
end;

function MDLS_IFF(const condition: boolean; const iftrue, iffalse: Extended): Extended;
begin
  if condition then
    Result := iftrue
  else
    Result := iffalse;
end;

function MDLS_IFS(const condition: boolean; const iftrue, iffalse: string): string;
begin
  if condition then
    Result := iftrue
  else
    Result := iffalse;
end;

function MDLS_Odd(const x: integer): boolean;
begin
  Result := Odd(x);
end;

function MDLS_Even(const x: integer): boolean;
begin
  Result := not Odd(x);
end;

function MDLS_MergeIntegerArrays(const A1, A2: TDynamicIntegerArray): TDynamicIntegerArray;
var
  l1, l2: integer;
  i: integer;
begin
  l1 := Length(A1);
  l2 := Length(A2);
  SetLength(Result, l1 + l2);
  for i := 0 to l1 - 1 do
    Result[i] := A1[i];
  for i := 0 to l2 - 1 do
    Result[l1 + i] := A2[i];
end;

function MDLS_MergeInt64Arrays(const A1, A2: TDynamicInt64Array): TDynamicInt64Array;
var
  l1, l2: integer;
  i: integer;
begin
  l1 := Length(A1);
  l2 := Length(A2);
  SetLength(Result, l1 + l2);
  for i := 0 to l1 - 1 do
    Result[i] := A1[i];
  for i := 0 to l2 - 1 do
    Result[l1 + i] := A2[i];
end;

function MDLS_MergeLongWordArrays(const A1, A2: TDynamicLongWordArray): TDynamicLongWordArray;
var
  l1, l2: integer;
  i: integer;
begin
  l1 := Length(A1);
  l2 := Length(A2);
  SetLength(Result, l1 + l2);
  for i := 0 to l1 - 1 do
    Result[i] := A1[i];
  for i := 0 to l2 - 1 do
    Result[l1 + i] := A2[i];
end;

function MDLS_MergeSingleArrays(const A1, A2: TDynamicSingleArray): TDynamicSingleArray;
var
  l1, l2: integer;
  i: integer;
begin
  l1 := Length(A1);
  l2 := Length(A2);
  SetLength(Result, l1 + l2);
  for i := 0 to l1 - 1 do
    Result[i] := A1[i];
  for i := 0 to l2 - 1 do
    Result[l1 + i] := A2[i];
end;

function MDLS_MergeDoubleArrays(const A1, A2: TDynamicDoubleArray): TDynamicDoubleArray;
var
  l1, l2: integer;
  i: integer;
begin
  l1 := Length(A1);
  l2 := Length(A2);
  SetLength(Result, l1 + l2);
  for i := 0 to l1 - 1 do
    Result[i] := A1[i];
  for i := 0 to l2 - 1 do
    Result[l1 + i] := A2[i];
end;

function MDLS_MergeExtendedArrays(const A1, A2: TDynamicExtendedArray): TDynamicExtendedArray;
var
  l1, l2: integer;
  i: integer;
begin
  l1 := Length(A1);
  l2 := Length(A2);
  SetLength(Result, l1 + l2);
  for i := 0 to l1 - 1 do
    Result[i] := A1[i];
  for i := 0 to l2 - 1 do
    Result[l1 + i] := A2[i];
end;

function MDLS_IsPrime(const N: Int64): Boolean;
var
  Test, k: Int64;
  ee: Extended;
begin
  if N <= 3 then
    Result := N > 1
  else if ((N mod 2) = 0) or ((N mod 3) = 0) then
    Result := False
  else
  begin
    Result := True;
    ee := N;
    k := Trunc(Sqrt(ee));
    Test := 5;
    while Test <= k do
    begin
      if ((N mod Test) = 0) or ((N mod (Test + 2)) = 0) then
      begin
        Result := False;
        break; // jump out of the for loop
      end;
      Test := Test + 6;
    end;
  end;
end;

function MDLS_IsIntegerInRange(const test, f1, f2: integer): boolean;
begin
  if f1 < f2 then
    result := (test >= f1) and (test <= f2)
  else
    result := (test >= f2) and (test <= f1)
end;

function MDLS_IsLongWordInRange(const test, f1, f2: LongWord): boolean;
begin
  if f1 < f2 then
    result := (test >= f1) and (test <= f2)
  else
    result := (test >= f2) and (test <= f1)
end;

function MDLS_IsFloatInRange(const test, f1, f2: single): boolean;
begin
  if f1 < f2 then
    result := (test >= f1) and (test <= f2)
  else
    result := (test >= f2) and (test <= f1)
end;

function MDLS_IsDoubleInRange(const test, f1, f2: double): boolean;
begin
  if f1 < f2 then
    result := (test >= f1) and (test <= f2)
  else
    result := (test >= f2) and (test <= f1)
end;

function MDLS_IsExtendedInRange(const test, f1, f2: Extended): boolean;
begin
  if f1 < f2 then
    result := (test >= f1) and (test <= f2)
  else
    result := (test >= f2) and (test <= f1)
end;

function MDLS_Sqr(const x: Extended): Extended;
begin
  Result := x * x;
end;

function MDLS_Sqrt(const x: Extended): Extended;
begin
  Result := Sqrt(x);
end;

function MDLS_Cube(const x: Extended): Extended;
begin
  Result := x * x * x;
end;

function MDLS_ArcCos(const X: Extended): Extended;
begin
  Result := ArcCos(X);
end;

function MDLS_ArcSin(const X: Extended): Extended;
begin
  Result := ArcSin(X);
end;

function MDLS_ArcTan2(const Y, X: Extended): Extended;
begin
  Result := ArcTan2(Y, X);
end;

procedure MDLS_SinCosE(const Theta: Extended; var S, C: Extended);
begin
  SinCos(Theta, S, C);
end;

procedure MDLS_SinCosD(const Theta: Extended; var S, C: Double);
var
  S1, C1: Extended;
begin
  SinCos(Theta, S1, C1);
  S := S1;
  C := C1;
end;

procedure MDLS_SinCosF(const Theta: Extended; var S, C: Single);
var
  S1, C1: Extended;
begin
  SinCos(Theta, S1, C1);
  S := S1;
  C := C1;
end;

function MDLS_Cosh(const X: Extended): Extended;
begin
  Result := Cosh(X);
end;

function MDLS_Sinh(const X: Extended): Extended;
begin
  Result := Sinh(X);
end;

function MDLS_Tanh(const X: Extended): Extended;
begin
  Result := Tanh(X);
end;

function MDLS_ArcCosh(const X: Extended): Extended;
begin
  Result := ArcCosh(X);
end;

function MDLS_ArcSinh(const X: Extended): Extended;
begin
  Result := ArcSinh(X);
end;

function MDLS_ArcTanh(const X: Extended): Extended;
begin
  Result := ArcTanh(X);
end;

function MDLS_Log10(const X: Extended): Extended;
begin
  Result := Log10(X);
end;

function MDLS_Log2(const X: Extended): Extended;
begin
  Result := Log2(X);
end;

function MDLS_Ln(const X: Extended): Extended;
begin
  Result := Ln(X);
end;

function MDLS_LogN(const Base, X: Extended): Extended;
begin
  Result := LogN(Base, X);
end;

function MDLS_IntPower(const Base: Extended; const Exponent: Integer): Extended;
begin
  Result := IntPower(Base, Exponent);
end;

function MDLS_Power(const Base, Exponent: Extended): Extended;
begin
  Result := Power(Base, Exponent);
end;

function MDLS_Ceil(const X: Extended): Integer;
begin
  Result := Ceil(X);
end;

function MDLS_Floor(const X: Extended): Integer;
begin
  Result := Floor(X);
end;

end.

