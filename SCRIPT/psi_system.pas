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
//  Foundation, inc., 59 Temple Place - Suite 330, Boston, MA
//  02111-1307, USA.
//
// DESCRIPTION:
//  Pascal Script RTL - system import.
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit psi_system;

interface

type
  TDynamicIntegerArray = array of Integer;
  TDynamicInt64Array = array of Int64;
  TDynamicLongWordArray = array of LongWord;
  TDynamicSingleArray = array of single;
  TDynamicDoubleArray = array of double;
  TDynamicExtendedArray = array of extended;

function PS_LevelTime: integer;

procedure PS_Write(const parm: string);

procedure PS_WriteFmt(const Fmt: string; const args: array of const);

procedure PS_Writeln(const parm: string);

procedure PS_WritelnFmt(const Fmt: string; const args: array of const);

procedure PS_OutputDebugString(const parm: string);

procedure PS_OutputDebugStringFmt(const Fmt: string; const args: array of const);

procedure PS_BreakPoint(const msg: string);

function PS_FineSine(const parm: LongWord): Integer;

function PS_FineCosine(const parm: LongWord): Integer;

function PS_FineTangent(const parm: LongWord): Integer;

function PS_Tan(const parm: Extended): Extended;

function PS_Sin360(const parm: Extended): Extended;

function PS_Cos360(const parm: Extended): Extended;

function PS_Tan360(const parm: Extended): Extended;

function PS_Format(const Fmt: string; const args: array of const): string;

function PS_IFI(const condition: boolean; const iftrue, iffalse: Int64): Int64;

function PS_IFF(const condition: boolean; const iftrue, iffalse: Extended): Extended;

function PS_IFS(const condition: boolean; const iftrue, iffalse: string): string;

function PS_Odd(const x: integer): boolean;

function PS_Even(const x: integer): boolean;

function PS_MergeIntegerArrays(const A1, A2: TDynamicIntegerArray): TDynamicIntegerArray;

function PS_MergeInt64Arrays(const A1, A2: TDynamicInt64Array): TDynamicInt64Array;

function PS_MergeLongWordArrays(const A1, A2: TDynamicLongWordArray): TDynamicLongWordArray;

function PS_MergeSingleArrays(const A1, A2: TDynamicSingleArray): TDynamicSingleArray;

function PS_MergeDoubleArrays(const A1, A2: TDynamicDoubleArray): TDynamicDoubleArray;

function PS_MergeExtendedArrays(const A1, A2: TDynamicExtendedArray): TDynamicExtendedArray;

function PS_IsPrime(const N: Int64): Boolean;

function PS_RandomRange(const x1, x2: integer): integer;

// --------------------------- CONSOLE -----------------------------------------

procedure PS_ConsoleCommand(const parm: string);

function PS_GetConsoleStr(const cvar: string): string;

function PS_SetConsoleStr(const cvar: string; const value: string): boolean;

function PS_GetConsoleInt(const cvar: string): integer;

function PS_SetConsoleInt(const cvar: string; const value: integer): boolean;

function PS_GetConsoleBool(const cvar: string): boolean;

function PS_SetConsoleBool(const cvar: string; const value: boolean): boolean;

implementation

uses
  d_delphi,
  Math,
  c_con,
  i_io,
  i_system,
  m_defs,
  m_misc,
  m_rnd,
  p_tick,
  tables;

function PS_LevelTime: integer;
begin
  Result := leveltime;
end;

procedure PS_Write(const parm: string);
begin
  printf(parm);
end;

procedure PS_WriteFmt(const Fmt: string; const args: array of const);
begin
  PS_Write(PS_Format(Fmt, args));
end;

procedure PS_Writeln(const parm: string);
begin
  printf(parm + #13#10);
end;

procedure PS_WritelnFmt(const Fmt: string; const args: array of const);
begin
  PS_Writeln(PS_Format(Fmt, args));
end;

procedure PS_OutputDebugString(const parm: string);
begin
  if debugfile <> nil then
    fprintf(debugfile, parm);
end;

procedure PS_OutputDebugStringFmt(const Fmt: string; const args: array of const);
begin
  PS_OutputDebugString(PS_Format(Fmt, args));
end;

var
  bpmsg: string = '';

// Actually for debuging the engine, not script
procedure PS_BreakPoint(const msg: string);
begin
  bpmsg := msg;
end;

function PS_FineSine(const parm: LongWord): Integer;
begin
  Result := finesine[parm and FINEMASK];
end;

function PS_FineCosine(const parm: LongWord): Integer;
begin
  Result := finecosine[parm and FINEMASK];
end;

function PS_FineTangent(const parm: LongWord): Integer;
begin
  Result := finetangent[parm and FINEMASK];
end;

function PS_Tan(const parm: Extended): Extended;
begin
  Result := tan(parm);
end;

function PS_Sin360(const parm: Extended): Extended;
begin
  Result := sin(parm / 360 * 2 * pi);
end;

function PS_Cos360(const parm: Extended): Extended;
begin
  Result := cos(parm / 360 * 2 * pi);
end;

function PS_Tan360(const parm: Extended): Extended;
begin
  Result := tan(parm / 360 * 2 * pi);
end;

function PS_Format(const Fmt: string; const args: array of const): string;
begin
  try
    sprintf(Result, Fmt, Args);
  except
    I_Warning('Script Runtime Error: Invalid Format Parameters (%s)'#13#10, [Fmt]);
    Result := Fmt;
  end;
end;

function PS_IFI(const condition: boolean; const iftrue, iffalse: Int64): Int64;
begin
  if condition then
    Result := iftrue
  else
    Result := iffalse;
end;

function PS_IFF(const condition: boolean; const iftrue, iffalse: Extended): Extended;
begin
  if condition then
    Result := iftrue
  else
    Result := iffalse;
end;

function PS_IFS(const condition: boolean; const iftrue, iffalse: string): string;
begin
  if condition then
    Result := iftrue
  else
    Result := iffalse;
end;

function PS_Odd(const x: integer): boolean;
begin
  Result := Odd(x);
end;

function PS_Even(const x: integer): boolean;
begin
  Result := not Odd(x);
end;

function PS_MergeIntegerArrays(const A1, A2: TDynamicIntegerArray): TDynamicIntegerArray;
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

function PS_MergeInt64Arrays(const A1, A2: TDynamicInt64Array): TDynamicInt64Array;
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

function PS_MergeLongWordArrays(const A1, A2: TDynamicLongWordArray): TDynamicLongWordArray;
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

function PS_MergeSingleArrays(const A1, A2: TDynamicSingleArray): TDynamicSingleArray;
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

function PS_MergeDoubleArrays(const A1, A2: TDynamicDoubleArray): TDynamicDoubleArray;
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

function PS_MergeExtendedArrays(const A1, A2: TDynamicExtendedArray): TDynamicExtendedArray;
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

function PS_IsPrime(const N: Int64): Boolean;
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

function PS_RandomRange(const x1, x2: integer): integer;
var
  r1, r2: float;
  rnd: integer;
begin
  rnd := P_Random;
  r1 := (rnd * x1) / 255;
  r2 := ((255 - rnd) * x2) / 255;
  result := round(r1 + r2);
end;

procedure PS_ConsoleCommand(const parm: string);
begin
  C_AddCommand(parm);
end;

function PS_GetConsoleStr(const cvar: string): string;
var
  i: integer;
  pd: Pdefault_t;
  cname: string;
  location: Pointer;
begin
  cname := strlower(cvar);

  pd := @defaults[0];
  for i := 0 to NUMDEFAULTS - 1 do
  begin
    if pd.name = cname then
    begin
      if pd.oldlocation <> nil then
        location := pd.oldlocation
      else
        location := pd.location;
      case pd._type of
        tString:
          begin
            Result := PString(location)^;
            Exit;
          end;
        tInteger:
          begin
            Result := itoa(PInteger(location)^);
            Exit;
          end;
        tBoolean:
          begin
            if PBoolean(location)^ then
              Result := 'true'
            else
              Result := 'false';
            Exit;
          end;
      end;
    end;
    inc(pd);
  end;
  Result := '';
end;

function PS_SetConsoleStr(const cvar: string; const value: string): boolean;
begin
  result := M_ForceDefaultString(cvar, value);
end;

function PS_GetConsoleInt(const cvar: string): integer;
var
  i: integer;
  pd: Pdefault_t;
  cname: string;
  location: Pointer;
begin
  cname := strlower(cvar);

  pd := @defaults[0];
  for i := 0 to NUMDEFAULTS - 1 do
  begin
    if pd.name = cname then
    begin
      if pd.oldlocation <> nil then
        location := pd.oldlocation
      else
        location := pd.location;
      case pd._type of
        tString:
          begin
            Result := atoi(PString(location)^, 0);
            Exit;
          end;
        tInteger:
          begin
            Result := PInteger(location)^;
            Exit;
          end;
        tBoolean:
          begin
            if PBoolean(location)^ then
              Result := 1
            else
              Result := 0;
            Exit;
          end;
      end;
    end;
    inc(pd);
  end;
  Result := 0;
end;

function PS_SetConsoleInt(const cvar: string; const value: integer): boolean;
begin
  result := M_ForceDefaultInteger(cvar, value);
end;

function PS_GetConsoleBool(const cvar: string): boolean;
var
  i: integer;
  pd: Pdefault_t;
  cname: string;
  location: Pointer;
begin
  cname := strlower(cvar);

  pd := @defaults[0];
  for i := 0 to NUMDEFAULTS - 1 do
  begin
    if pd.name = cname then
    begin
      if pd.oldlocation <> nil then
        location := pd.oldlocation
      else
        location := pd.location;
      case pd._type of
        tString:
          begin
            Result := PString(location)^ <> '';
            Exit;
          end;
        tInteger:
          begin
            Result := PInteger(location)^ <> 0;
            Exit;
          end;
        tBoolean:
          begin
            Result := PBoolean(location)^;
            Exit;
          end;
      end;
    end;
    inc(pd);
  end;
  Result := False;
end;

function PS_SetConsoleBool(const cvar: string; const value: boolean): boolean;
begin
  result := M_ForceDefaultBoolean(cvar, value);
end;

end.

