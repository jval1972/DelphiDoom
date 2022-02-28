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
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit r_mmx;

interface

uses
  d_delphi;

//==============================================================================
//
// R_BatchColorAdd32_MMX
//
//==============================================================================
function R_BatchColorAdd32_MMX(const dest0: PLongWord; const color: LongWord; const numpixels: integer): boolean;

//==============================================================================
//
// R_BatchColorShade_AMD
//
//==============================================================================
function R_BatchColorShade_AMD(const dest0: PByte; const numbytes: integer): boolean;

implementation

type
  rec_2lw = record
    dwords: array[0..1] of LongWord;
  end;

//==============================================================================
//
// R_BatchColorAdd32_MMX
//
//==============================================================================
function R_BatchColorAdd32_MMX(const dest0: PLongWord; const color: LongWord; const numpixels: integer): boolean;
var
  data: rec_2lw;
  pdat: pointer;
  dest: PByte;
  count: integer;
begin
  if mmxMachine = 0 then
  begin
    result := false;
    exit;
  end;

  if color = 0 then
  begin
    result := true;
    exit;
  end;

  dest := PByte(dest0);

  count := numpixels * 4;
  if count and 7 <> 0 then
  begin
    result := false;
    exit;
  end;

  result := true;

  if count = 0 then
    exit;

  data.dwords[0] := color;
  data.dwords[1] := color;
  pdat := @data;

  if count >= 64 then
  begin
    asm
      push esi

      mov eax, dest
      mov esi, pdat

      mov ecx, count
      // 64 bytes per iteration
      shr ecx, 6
@@loop1:
      // Read in source data
      movq mm1, [esi]
      movq mm2, mm1
      movq mm3, mm1
      movq mm4, mm1
      movq mm5, mm1
      movq mm6, mm1
      movq mm7, mm1
      movq mm0, mm1

      paddusb mm1, [eax]
      paddusb mm2, [eax + 8]
      paddusb mm3, [eax + 16]
      paddusb mm4, [eax + 24]
      paddusb mm5, [eax + 32]
      paddusb mm6, [eax + 40]
      paddusb mm7, [eax + 48]
      paddusb mm0, [eax + 56]

      movntq [eax], mm1
      movntq [eax + 8], mm2
      movntq [eax + 16], mm3
      movntq [eax + 24], mm4
      movntq [eax + 32], mm5
      movntq [eax + 40], mm6
      movntq [eax + 48], mm7
      movntq [eax + 56], mm0

      add eax, 64
      dec ecx
      jnz @@loop1

      pop esi
    end;

    inc(dest, count and not 63);
    count := count and 63;
  end;

  if count >= 8 then
  begin
    asm
      push esi

      mov eax, dest
      mov esi, pdat

      mov ecx, count
      // 8 bytes per iteration
      shr ecx, 3
      // Read in source data
      movq mm1, [esi]
@@loop2:
      movq mm2, mm1
      paddusb mm2, [eax]
      movntq [eax], mm2

      add eax, 8
      dec ecx
      jnz @@loop2

      pop esi
    end;
  end;

  asm
    emms
  end;

end;

//==============================================================================
//
// R_BatchColorShade_AMD
//
//==============================================================================
function R_BatchColorShade_AMD(const dest0: PByte; const numbytes: integer): boolean;
var
  data: rec_2lw;
  pdat: pointer;
  dest: PByte;
  count: integer;
begin
  if AMD3DNowMachine = 0 then
  begin
    result := false;
    exit;
  end;

  dest := dest0;

  count := numbytes;

  result := true;

  if count = 0 then
    exit;

  data.dwords[0] := 0;
  data.dwords[1] := 0;
  pdat := @data;

  if count >= 64 then
  begin
    asm
      push esi

      mov eax, dest
      mov esi, pdat

      mov ecx, count
      // 64 bytes per iteration
      shr ecx, 6
@@loop1:
      // Read in source data
      movq mm1, [esi]
      movq mm2, mm1
      movq mm3, mm1
      movq mm4, mm1
      movq mm5, mm1
      movq mm6, mm1
      movq mm7, mm1
      movq mm0, mm1

      pavgusb mm1, [eax]
      pavgusb mm2, [eax + 8]
      pavgusb mm3, [eax + 16]
      pavgusb mm4, [eax + 24]
      pavgusb mm5, [eax + 32]
      pavgusb mm6, [eax + 40]
      pavgusb mm7, [eax + 48]
      pavgusb mm0, [eax + 56]

      movntq [eax], mm1
      movntq [eax + 8], mm2
      movntq [eax + 16], mm3
      movntq [eax + 24], mm4
      movntq [eax + 32], mm5
      movntq [eax + 40], mm6
      movntq [eax + 48], mm7
      movntq [eax + 56], mm0

      add eax, 64
      dec ecx
      jnz @@loop1

      pop esi
    end;

    inc(dest, count and not 63);
    count := count and 63;
  end;

  if count >= 8 then
  begin
    asm
      push esi

      mov eax, dest
      mov esi, pdat

      mov ecx, count
      // 8 bytes per iteration
      shr ecx, 3
      // Read in source data
      movq mm1, [esi]
@@loop2:
      movq mm2, mm1
      pavgusb mm2, [eax]
      movntq [eax], mm2

      add eax, 8
      dec ecx
      jnz @@loop2

      pop esi
    end;
    inc(dest, count and not 7);
    count := count and 7;
  end;

  asm
    emms
  end;

  while count > 0 do
  begin
    dest^ := dest^ shr 1;
    inc(dest);
    dec(count);
  end;

end;

end.
