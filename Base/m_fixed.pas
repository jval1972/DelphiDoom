//------------------------------------------------------------------------------
//
//  DelphiDoom: A modified and improved DOOM engine for Windows
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 2004-2013 by Jim Valavanis
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
// DESCRIPTION:
//  Fixed point arithemtics, implementation.
//
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit m_fixed;

interface

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

function FixedMul(const a, b: fixed_t): fixed_t;

function FixedMulEx(const a, b: fixed_t): fixed_t;

function FixedMul88(const a, b: fixed_t): fixed_t;

function FixedMul8(const a, b: fixed_t): fixed_t;

function FixedIntMul(const a, b: fixed_t): fixed_t;

function IntFixedMul(const a, b: fixed_t): fixed_t;

function FixedDiv(const a, b: fixed_t): fixed_t;

function FixedDivEx(const a, b: fixed_t): fixed_t;

function FixedDiv2(const a, b: fixed_t): fixed_t;

function FixedInt(const x: integer): integer;

implementation

uses
  d_delphi,
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
      Result := MAXINT
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

end.

