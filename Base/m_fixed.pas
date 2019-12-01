//------------------------------------------------------------------------------
//
//  DelphiDoom: A modified and improved DOOM engine for Windows
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 1993-1996 by id Software, Inc.
//  Copyright (C) 2004-2019 by Jim Valavanis
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
//  Fixed point arithemtics, implementation.
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit m_fixed;

interface

uses
  d_delphi;

//
// Fixed point, 32bit as 16.16.
//

const
  FRACBITS = 16;
  FRACUNIT = 1 shl FRACBITS;

type
  fixed_t = integer;
  Pfixed_t = ^fixed_t;
  fixed_tArray = packed array[0..$FFFF] of fixed_t;
  Pfixed_tArray = ^fixed_tArray;
  fixed64_t = int64;
  fixedfloat_t = double;

function FixedMul(const a, b: fixed_t): fixed_t;

function FixedMulEx(const a, b: fixed_t): fixed_t;

function FixedMul88(const a, b: fixed_t): fixed_t;

function FixedMul8(const a, b: fixed_t): fixed_t;

function FixedIntMul(const a, b: fixed_t): fixed_t;

function IntFixedMul(const a, b: fixed_t): fixed_t;

function FixedDiv(const a, b: fixed_t): fixed_t;

function FixedDiv_Positive(const a, b: fixed_t): fixed_t;

function FixedDiv_fast(const a, b: fixed_t): fixed_t;

function FixedDivEx(const a, b: fixed_t): fixed_t;

function FixedDiv2(const a, b: fixed_t): fixed_t;

function FixedInt(const x: integer): integer;

function FloatToFixed(const f: float): fixed_t;

function DoubleToFixed(const f: double): fixed_t;

function ExtendedToFixed(const f: extended): fixed_t;

function FixedToFloat(const x: fixed_t): float;

function FixedToDouble(const x: fixed_t): double;

function FixedToExtended(const x: fixed_t): extended;

function FixedInt_FixedMul(const a, b: fixed_t): fixed_t;

function FixedSqrt(const a: fixed_t): fixed_t;

function FixedInt64(const x: fixed64_t): integer;

function FixedMul64(const a, b: fixed64_t): fixed64_t;

function FloatDiv(const a, b: fixedfloat_t): fixedfloat_t;

function FloatMul(const a, b: fixedfloat_t): fixedfloat_t;

implementation

uses
  doomtype;

function FixedMul(const a, b: fixed_t): fixed_t; assembler;
asm
  imul b
  shrd eax, edx, 16
end;

function FixedMulEx(const a, b: fixed_t): fixed_t;
begin
  result := Round(a / FRACUNIT * b);
end;


function FixedMul88(const a, b: fixed_t): fixed_t; assembler;
asm
  sar a, 8
  sar b, 8
  imul b
  shrd eax, edx, 16
end;

function FixedMul8(const a, b: fixed_t): fixed_t; assembler;
asm
  sar a, 8
  imul b
  shrd eax, edx, 16
end;

function FixedIntMul(const a, b: fixed_t): fixed_t; assembler;
asm
  sar b, FRACBITS
  imul b
  shrd eax, edx, 16
end;

function IntFixedMul(const a, b: fixed_t): fixed_t; assembler;
asm
  sar eax, FRACBITS
  imul b
  shrd eax, edx, 16
end;

function FixedDiv(const a, b: fixed_t): fixed_t;
begin
  if _SHR14(abs(a)) >= abs(b) then
  begin
    if a xor b < 0 then
      result := MININT
    else
      result := MAXINT;
  end
  else
    result := FixedDiv2(a, b);
end;

function FixedDiv_Positive(const a, b: fixed_t): fixed_t;
begin
  if (a shr 14) >= b then
  begin
    if a xor b < 0 then
      result := MININT
    else
      result := MAXINT;
  end
  else
    result := FixedDiv2(a, b);
end;

function FixedDiv_fast(const a, b: fixed_t): fixed_t; assembler;
asm
  mov ebx, b
  cmp ebx, 0
  jne @@loop1
  mov eax, MININT
  jmp @@exit
@@loop1:
  mov ebx, b
  mov eax, a
  mov edx, eax
  sal eax, 16
  sar edx, 16
  idiv ebx
@@exit:
end;

function FixedDivEx(const a, b: fixed_t): fixed_t;
var
  ret: Double;
  ad: Double;
  bd: Double;
begin
  if b = 0 then
  begin
    if a < 0 then
      result := MININT
    else
      result := MAXINT;
  end
  else
  begin
    ad := a / FRACUNIT;
    bd := b / FRACUNIT;
    ret := (ad / bd) * FRACUNIT;
    ret := round(ret);
    if ret < MININT then
      result := MININT
    else if ret > MAXINT then
      result := MAXINT
    else
      result := Round(ret);
  end;
end;

function FixedDiv2(const a, b: fixed_t): fixed_t; assembler;
asm
  mov ebx, b
  mov edx, eax
  sal eax, 16
  sar edx, 16
  idiv ebx
end;

function FixedInt(const x: integer): integer; assembler;
asm
  sar eax, FRACBITS
end;


function FloatToFixed(const f: float): fixed_t;
begin
  result := Round(f * FRACUNIT);
end;

function DoubleToFixed(const f: double): fixed_t;
begin
  result := Round(f * FRACUNIT);
end;

function ExtendedToFixed(const f: extended): fixed_t;
begin
  result := Round(f * FRACUNIT);
end;

function FixedToFloat(const x: fixed_t): float;
begin
  result := x / FRACUNIT;
end;

function FixedToDouble(const x: fixed_t): double;
begin
  result := x / FRACUNIT;
end;

function FixedToExtended(const x: fixed_t): extended;
begin
  result := x / FRACUNIT;
end;

function FixedInt_FixedMul(const a, b: fixed_t): fixed_t; assembler;
asm
  imul b
  shrd eax, edx, 16
  sar eax, FRACBITS
end;

function FixedSqrt(const a: fixed_t): fixed_t;
begin
  Result := Round(Sqrt(a / FRACUNIT) * FRACUNIT);
end;

//
// FixedInt64
// JVAL: This is 30 times faster than using  result := x div FRACUNIT;
//
function FixedInt64(const x: fixed64_t): integer;
var
  x2: fixed64_t;
begin
  if x < 0 then
  begin
    x2 := -x;
    result := -PInteger(integer(@x2) + 2)^;
  end
  else
    result := PInteger(integer(@x) + 2)^;
end;

function FixedMul64(const a, b: fixed64_t): fixed64_t;
begin
  result := Round(a / FRACUNIT * b);
end;

function FloatDiv(const a, b: fixedfloat_t): fixedfloat_t;
var
  ad: fixedfloat_t;
  bd: fixedfloat_t;
begin
  if b = 0 then
  begin
    if a < 0 then
      result := MININT
    else
      result := MAXINT;
  end
  else
  begin
    ad := a / FRACUNIT;
    bd := b / FRACUNIT;
    result := (ad / bd) * FRACUNIT;
  end;
end;

function FloatMul(const a, b: fixedfloat_t): fixedfloat_t;
begin
  result := a / FRACUNIT * b;
end;

end.

