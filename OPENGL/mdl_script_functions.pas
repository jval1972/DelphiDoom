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
//  Script functions library for DD_MODELs
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
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

//==============================================================================
//
// MDLS_Write
//
//==============================================================================
procedure MDLS_Write(const parm: string);

//==============================================================================
//
// MDLS_WriteFmt
//
//==============================================================================
procedure MDLS_WriteFmt(const Fmt: string; const args: array of const);

//==============================================================================
//
// MDLS_Writeln
//
//==============================================================================
procedure MDLS_Writeln(const parm: string);

//==============================================================================
//
// MDLS_WritelnFmt
//
//==============================================================================
procedure MDLS_WritelnFmt(const Fmt: string; const args: array of const);

//==============================================================================
//
// MDLS_BreakPoint
//
//==============================================================================
procedure MDLS_BreakPoint(const msg: string);

//==============================================================================
//
// MDLS_Tan
//
//==============================================================================
function MDLS_Tan(const parm: Extended): Extended;

//==============================================================================
//
// MDLS_Sin360
//
//==============================================================================
function MDLS_Sin360(const parm: Extended): Extended;

//==============================================================================
//
// MDLS_Cos360
//
//==============================================================================
function MDLS_Cos360(const parm: Extended): Extended;

//==============================================================================
//
// MDLS_Tan360
//
//==============================================================================
function MDLS_Tan360(const parm: Extended): Extended;

//==============================================================================
//
// MDLS_Format
//
//==============================================================================
function MDLS_Format(const Fmt: string; const args: array of const): string;

//==============================================================================
//
// MDLS_IFI
//
//==============================================================================
function MDLS_IFI(const condition: boolean; const iftrue, iffalse: Int64): Int64;

//==============================================================================
//
// MDLS_IFF
//
//==============================================================================
function MDLS_IFF(const condition: boolean; const iftrue, iffalse: Extended): Extended;

//==============================================================================
//
// MDLS_IFS
//
//==============================================================================
function MDLS_IFS(const condition: boolean; const iftrue, iffalse: string): string;

//==============================================================================
//
// MDLS_Odd
//
//==============================================================================
function MDLS_Odd(const x: integer): boolean;

//==============================================================================
//
// MDLS_Even
//
//==============================================================================
function MDLS_Even(const x: integer): boolean;

//==============================================================================
//
// MDLS_MergeIntegerArrays
//
//==============================================================================
function MDLS_MergeIntegerArrays(const A1, A2: TDynamicIntegerArray): TDynamicIntegerArray;

//==============================================================================
//
// MDLS_MergeInt64Arrays
//
//==============================================================================
function MDLS_MergeInt64Arrays(const A1, A2: TDynamicInt64Array): TDynamicInt64Array;

//==============================================================================
//
// MDLS_MergeLongWordArrays
//
//==============================================================================
function MDLS_MergeLongWordArrays(const A1, A2: TDynamicLongWordArray): TDynamicLongWordArray;

//==============================================================================
//
// MDLS_MergeSingleArrays
//
//==============================================================================
function MDLS_MergeSingleArrays(const A1, A2: TDynamicSingleArray): TDynamicSingleArray;

//==============================================================================
//
// MDLS_MergeDoubleArrays
//
//==============================================================================
function MDLS_MergeDoubleArrays(const A1, A2: TDynamicDoubleArray): TDynamicDoubleArray;

//==============================================================================
//
// MDLS_MergeExtendedArrays
//
//==============================================================================
function MDLS_MergeExtendedArrays(const A1, A2: TDynamicExtendedArray): TDynamicExtendedArray;

//==============================================================================
//
// MDLS_IsPrime
//
//==============================================================================
function MDLS_IsPrime(const N: Int64): Boolean;

//==============================================================================
//
// MDLS_IsIntegerInRange
//
//==============================================================================
function MDLS_IsIntegerInRange(const test, f1, f2: integer): boolean;

//==============================================================================
//
// MDLS_IsLongWordInRange
//
//==============================================================================
function MDLS_IsLongWordInRange(const test, f1, f2: LongWord): boolean;

//==============================================================================
//
// MDLS_IsFloatInRange
//
//==============================================================================
function MDLS_IsFloatInRange(const test, f1, f2: single): boolean;

//==============================================================================
//
// MDLS_IsDoubleInRange
//
//==============================================================================
function MDLS_IsDoubleInRange(const test, f1, f2: double): boolean;

//==============================================================================
//
// MDLS_IsExtendedInRange
//
//==============================================================================
function MDLS_IsExtendedInRange(const test, f1, f2: Extended): boolean;

//==============================================================================
//
// MDLS_Sqr
//
//==============================================================================
function MDLS_Sqr(const x: Extended): Extended;

//==============================================================================
//
// MDLS_Sqrt
//
//==============================================================================
function MDLS_Sqrt(const x: Extended): Extended;

//==============================================================================
//
// MDLS_Cube
//
//==============================================================================
function MDLS_Cube(const x: Extended): Extended;

//==============================================================================
//
// MDLS_ArcCos
//
//==============================================================================
function MDLS_ArcCos(const X: Extended): Extended;

//==============================================================================
//
// MDLS_ArcSin
//
//==============================================================================
function MDLS_ArcSin(const X: Extended): Extended;

//==============================================================================
//
// MDLS_ArcTan2
//
//==============================================================================
function MDLS_ArcTan2(const Y, X: Extended): Extended;

//==============================================================================
//
// MDLS_SinCosE
//
//==============================================================================
procedure MDLS_SinCosE(const Theta: Extended; var S, C: Extended);

//==============================================================================
//
// MDLS_SinCosD
//
//==============================================================================
procedure MDLS_SinCosD(const Theta: Extended; var S, C: Double);

//==============================================================================
//
// MDLS_SinCosF
//
//==============================================================================
procedure MDLS_SinCosF(const Theta: Extended; var S, C: Single);

//==============================================================================
//
// MDLS_Cosh
//
//==============================================================================
function MDLS_Cosh(const X: Extended): Extended;

//==============================================================================
//
// MDLS_Sinh
//
//==============================================================================
function MDLS_Sinh(const X: Extended): Extended;

//==============================================================================
//
// MDLS_Tanh
//
//==============================================================================
function MDLS_Tanh(const X: Extended): Extended;

//==============================================================================
//
// MDLS_ArcCosh
//
//==============================================================================
function MDLS_ArcCosh(const X: Extended): Extended;

//==============================================================================
//
// MDLS_ArcSinh
//
//==============================================================================
function MDLS_ArcSinh(const X: Extended): Extended;

//==============================================================================
//
// MDLS_ArcTanh
//
//==============================================================================
function MDLS_ArcTanh(const X: Extended): Extended;

//==============================================================================
//
// MDLS_Log10
//
//==============================================================================
function MDLS_Log10(const X: Extended): Extended;

//==============================================================================
//
// MDLS_Log2
//
//==============================================================================
function MDLS_Log2(const X: Extended): Extended;

//==============================================================================
//
// MDLS_Ln
//
//==============================================================================
function MDLS_Ln(const X: Extended): Extended;

//==============================================================================
//
// MDLS_LogN
//
//==============================================================================
function MDLS_LogN(const Base, X: Extended): Extended;

//==============================================================================
//
// MDLS_IntPower
//
//==============================================================================
function MDLS_IntPower(const Base: Extended; const Exponent: Integer): Extended;

//==============================================================================
//
// MDLS_Power
//
//==============================================================================
function MDLS_Power(const Base, Exponent: Extended): Extended;

//==============================================================================
//
// MDLS_Ceil
//
//==============================================================================
function MDLS_Ceil(const X: Extended): Integer;

//==============================================================================
//
// MDLS_Floor
//
//==============================================================================
function MDLS_Floor(const X: Extended): Integer;

implementation

uses
  d_delphi,
  SysUtils,
  Math;

//==============================================================================
//
// MDLS_Write
//
//==============================================================================
procedure MDLS_Write(const parm: string);
begin
  printf(parm);
end;

//==============================================================================
//
// MDLS_WriteFmt
//
//==============================================================================
procedure MDLS_WriteFmt(const Fmt: string; const args: array of const);
begin
  MDLS_Write(MDLS_Format(Fmt, args));
end;

//==============================================================================
//
// MDLS_Writeln
//
//==============================================================================
procedure MDLS_Writeln(const parm: string);
begin
  printf(parm + #13#10);
end;

//==============================================================================
//
// MDLS_WritelnFmt
//
//==============================================================================
procedure MDLS_WritelnFmt(const Fmt: string; const args: array of const);
begin
  MDLS_Writeln(MDLS_Format(Fmt, args));
end;

var
  bpmsg: string = '';

//==============================================================================
// MDLS_BreakPoint
//
// Actually for debuging the engine, not script
//
//==============================================================================
procedure MDLS_BreakPoint(const msg: string);
begin
  bpmsg := msg;
end;

//==============================================================================
//
// MDLS_Tan
//
//==============================================================================
function MDLS_Tan(const parm: Extended): Extended;
begin
  Result := tan(parm);
end;

//==============================================================================
//
// MDLS_Sin360
//
//==============================================================================
function MDLS_Sin360(const parm: Extended): Extended;
begin
  Result := sin(parm / 360 * 2 * pi);
end;

//==============================================================================
//
// MDLS_Cos360
//
//==============================================================================
function MDLS_Cos360(const parm: Extended): Extended;
begin
  Result := cos(parm / 360 * 2 * pi);
end;

//==============================================================================
//
// MDLS_Tan360
//
//==============================================================================
function MDLS_Tan360(const parm: Extended): Extended;
begin
  Result := tan(parm / 360 * 2 * pi);
end;

//==============================================================================
//
// MDLS_Format
//
//==============================================================================
function MDLS_Format(const Fmt: string; const args: array of const): string;
begin
  try
    Result := Format(Fmt, Args);
  except
    Result := Fmt;
  end;
end;

//==============================================================================
//
// MDLS_IFI
//
//==============================================================================
function MDLS_IFI(const condition: boolean; const iftrue, iffalse: Int64): Int64;
begin
  if condition then
    Result := iftrue
  else
    Result := iffalse;
end;

//==============================================================================
//
// MDLS_IFF
//
//==============================================================================
function MDLS_IFF(const condition: boolean; const iftrue, iffalse: Extended): Extended;
begin
  if condition then
    Result := iftrue
  else
    Result := iffalse;
end;

//==============================================================================
//
// MDLS_IFS
//
//==============================================================================
function MDLS_IFS(const condition: boolean; const iftrue, iffalse: string): string;
begin
  if condition then
    Result := iftrue
  else
    Result := iffalse;
end;

//==============================================================================
//
// MDLS_Odd
//
//==============================================================================
function MDLS_Odd(const x: integer): boolean;
begin
  Result := Odd(x);
end;

//==============================================================================
//
// MDLS_Even
//
//==============================================================================
function MDLS_Even(const x: integer): boolean;
begin
  Result := not Odd(x);
end;

//==============================================================================
//
// MDLS_MergeIntegerArrays
//
//==============================================================================
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

//==============================================================================
//
// MDLS_MergeInt64Arrays
//
//==============================================================================
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

//==============================================================================
//
// MDLS_MergeLongWordArrays
//
//==============================================================================
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

//==============================================================================
//
// MDLS_MergeSingleArrays
//
//==============================================================================
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

//==============================================================================
//
// MDLS_MergeDoubleArrays
//
//==============================================================================
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

//==============================================================================
//
// MDLS_MergeExtendedArrays
//
//==============================================================================
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

//==============================================================================
//
// MDLS_IsPrime
//
//==============================================================================
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

//==============================================================================
//
// MDLS_IsIntegerInRange
//
//==============================================================================
function MDLS_IsIntegerInRange(const test, f1, f2: integer): boolean;
begin
  if f1 < f2 then
    result := (test >= f1) and (test <= f2)
  else
    result := (test >= f2) and (test <= f1)
end;

//==============================================================================
//
// MDLS_IsLongWordInRange
//
//==============================================================================
function MDLS_IsLongWordInRange(const test, f1, f2: LongWord): boolean;
begin
  if f1 < f2 then
    result := (test >= f1) and (test <= f2)
  else
    result := (test >= f2) and (test <= f1)
end;

//==============================================================================
//
// MDLS_IsFloatInRange
//
//==============================================================================
function MDLS_IsFloatInRange(const test, f1, f2: single): boolean;
begin
  if f1 < f2 then
    result := (test >= f1) and (test <= f2)
  else
    result := (test >= f2) and (test <= f1)
end;

//==============================================================================
//
// MDLS_IsDoubleInRange
//
//==============================================================================
function MDLS_IsDoubleInRange(const test, f1, f2: double): boolean;
begin
  if f1 < f2 then
    result := (test >= f1) and (test <= f2)
  else
    result := (test >= f2) and (test <= f1)
end;

//==============================================================================
//
// MDLS_IsExtendedInRange
//
//==============================================================================
function MDLS_IsExtendedInRange(const test, f1, f2: Extended): boolean;
begin
  if f1 < f2 then
    result := (test >= f1) and (test <= f2)
  else
    result := (test >= f2) and (test <= f1)
end;

//==============================================================================
//
// MDLS_Sqr
//
//==============================================================================
function MDLS_Sqr(const x: Extended): Extended;
begin
  Result := x * x;
end;

//==============================================================================
//
// MDLS_Sqrt
//
//==============================================================================
function MDLS_Sqrt(const x: Extended): Extended;
begin
  Result := Sqrt(x);
end;

//==============================================================================
//
// MDLS_Cube
//
//==============================================================================
function MDLS_Cube(const x: Extended): Extended;
begin
  Result := x * x * x;
end;

//==============================================================================
//
// MDLS_ArcCos
//
//==============================================================================
function MDLS_ArcCos(const X: Extended): Extended;
begin
  Result := ArcCos(X);
end;

//==============================================================================
//
// MDLS_ArcSin
//
//==============================================================================
function MDLS_ArcSin(const X: Extended): Extended;
begin
  Result := ArcSin(X);
end;

//==============================================================================
//
// MDLS_ArcTan2
//
//==============================================================================
function MDLS_ArcTan2(const Y, X: Extended): Extended;
begin
  Result := ArcTan2(Y, X);
end;

//==============================================================================
//
// MDLS_SinCosE
//
//==============================================================================
procedure MDLS_SinCosE(const Theta: Extended; var S, C: Extended);
begin
  SinCos(Theta, S, C);
end;

//==============================================================================
//
// MDLS_SinCosD
//
//==============================================================================
procedure MDLS_SinCosD(const Theta: Extended; var S, C: Double);
var
  S1, C1: Extended;
begin
  SinCos(Theta, S1, C1);
  S := S1;
  C := C1;
end;

//==============================================================================
//
// MDLS_SinCosF
//
//==============================================================================
procedure MDLS_SinCosF(const Theta: Extended; var S, C: Single);
var
  S1, C1: Extended;
begin
  SinCos(Theta, S1, C1);
  S := S1;
  C := C1;
end;

//==============================================================================
//
// MDLS_Cosh
//
//==============================================================================
function MDLS_Cosh(const X: Extended): Extended;
begin
  Result := Cosh(X);
end;

//==============================================================================
//
// MDLS_Sinh
//
//==============================================================================
function MDLS_Sinh(const X: Extended): Extended;
begin
  Result := Sinh(X);
end;

//==============================================================================
//
// MDLS_Tanh
//
//==============================================================================
function MDLS_Tanh(const X: Extended): Extended;
begin
  Result := Tanh(X);
end;

//==============================================================================
//
// MDLS_ArcCosh
//
//==============================================================================
function MDLS_ArcCosh(const X: Extended): Extended;
begin
  Result := ArcCosh(X);
end;

//==============================================================================
//
// MDLS_ArcSinh
//
//==============================================================================
function MDLS_ArcSinh(const X: Extended): Extended;
begin
  Result := ArcSinh(X);
end;

//==============================================================================
//
// MDLS_ArcTanh
//
//==============================================================================
function MDLS_ArcTanh(const X: Extended): Extended;
begin
  Result := ArcTanh(X);
end;

//==============================================================================
//
// MDLS_Log10
//
//==============================================================================
function MDLS_Log10(const X: Extended): Extended;
begin
  Result := Log10(X);
end;

//==============================================================================
//
// MDLS_Log2
//
//==============================================================================
function MDLS_Log2(const X: Extended): Extended;
begin
  Result := Log2(X);
end;

//==============================================================================
//
// MDLS_Ln
//
//==============================================================================
function MDLS_Ln(const X: Extended): Extended;
begin
  Result := Ln(X);
end;

//==============================================================================
//
// MDLS_LogN
//
//==============================================================================
function MDLS_LogN(const Base, X: Extended): Extended;
begin
  Result := LogN(Base, X);
end;

//==============================================================================
//
// MDLS_IntPower
//
//==============================================================================
function MDLS_IntPower(const Base: Extended; const Exponent: Integer): Extended;
begin
  Result := IntPower(Base, Exponent);
end;

//==============================================================================
//
// MDLS_Power
//
//==============================================================================
function MDLS_Power(const Base, Exponent: Extended): Extended;
begin
  Result := Power(Base, Exponent);
end;

//==============================================================================
//
// MDLS_Ceil
//
//==============================================================================
function MDLS_Ceil(const X: Extended): Integer;
begin
  Result := Ceil(X);
end;

//==============================================================================
//
// MDLS_Floor
//
//==============================================================================
function MDLS_Floor(const X: Extended): Integer;
begin
  Result := Floor(X);
end;

end.

