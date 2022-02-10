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

{
   *********************************************************************
   Version: 1998.06.09
   Copyright (C) 1997, 1998 Gertjan Schouten

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, inc., 675 Mass Ave, Cambridge, MA 02139, USA.
   *********************************************************************
}
unit t_colors;

interface

uses
  d_delphi;

const
  Red     = $FF0000;
  Green   = $00FF00;
  Blue    = $0000FF;
  Cyan    = Green xor Blue;
  Yellow  = Red xor Green;
  Magenta = Red xor Blue;
  Black   = Cyan xor Yellow xor Magenta;
  White   = Red xor Green xor Blue;
  Gray    = $7F7F7F;
  DefaultPalette: array[0..255] of integer = (
    $000000, $000070, $007000, $007070, $700000, $700070, $704800, $C4C4C4,
    $343434, $0000FC, $24FC24, $00FCFC, $FC1414, $B000FC, $FCFC24, $FCFCFC,
    $000000, $000000, $000000, $000000, $000000, $000000, $000000, $000000,
    $000000, $000000, $000000, $000000, $000000, $000000, $000000, $000000,
    $000000, $000000, $000000, $000000, $000000, $000000, $000000, $000000,
    $000000, $330000, $660000, $990000, $CC0000, $FF0000, $003300, $333300,
    $663300, $993300, $CC3300, $FF3300, $006600, $336600, $666600, $996600,
    $CC6600, $FF6600, $009900, $339900, $669900, $999900, $CC9900, $FF9900,
    $00CC00, $33CC00, $66CC00, $99CC00, $CCCC00, $FFCC00, $00FF00, $33FF00,
    $66FF00, $99FF00, $CCFF00, $FFFF00, $000033, $330033, $660033, $990033,
    $CC0033, $FF0033, $003333, $333333, $663333, $993333, $CC3333, $FF3333,
    $006633, $336633, $666633, $996633, $CC6633, $FF6633, $009933, $339933,
    $669933, $999933, $CC9933, $FF9933, $00CC33, $33CC33, $66CC33, $99CC33,
    $CCCC33, $FFCC33, $00FF33, $33FF33, $66FF33, $99FF33, $CCFF33, $FFFF33,
    $000066, $330066, $660066, $990066, $CC0066, $FF0066, $003366, $333366,
    $663366, $993366, $CC3366, $FF3366, $006666, $336666, $666666, $996666,
    $CC6666, $FF6666, $009966, $339966, $669966, $999966, $CC9966, $FF9966,
    $00CC66, $33CC66, $66CC66, $99CC66, $CCCC66, $FFCC66, $00FF66, $33FF66,
    $66FF66, $99FF66, $CCFF66, $FFFF66, $000099, $330099, $660099, $990099,
    $CC0099, $FF0099, $003399, $333399, $663399, $993399, $CC3399, $FF3399,
    $006699, $336699, $666699, $996699, $CC6699, $FF6699, $009999, $339999,
    $669999, $999999, $CC9999, $FF9999, $00CC99, $33CC99, $66CC99, $99CC99,
    $CCCC99, $FFCC99, $00FF99, $33FF99, $66FF99, $99FF99, $CCFF99, $FFFF99,
    $0000CC, $3300CC, $6600CC, $9900CC, $CC00CC, $FF00CC, $0033CC, $3333CC,
    $6633CC, $9933CC, $CC33CC, $FF33CC, $0066CC, $3366CC, $6666CC, $9966CC,
    $CC66CC, $FF66CC, $0099CC, $3399CC, $6699CC, $9999CC, $CC99CC, $FF99CC,
    $00CCCC, $33CCCC, $66CCCC, $99CCCC, $CCCCCC, $FFCCCC, $00FFCC, $33FFCC,
    $66FFCC, $99FFCC, $CCFFCC, $FFFFCC, $0000FF, $3300FF, $6600FF, $9900FF,
    $CC00FF, $FF00FF, $0033FF, $3333FF, $6633FF, $9933FF, $CC33FF, $FF33FF,
    $0066FF, $3366FF, $6666FF, $9966FF, $CC66FF, $FF66FF, $0099FF, $3399FF,
    $6699FF, $9999FF, $CC99FF, $FF99FF, $00CCFF, $33CCFF, $66CCFF, $99CCFF,
    $CCCCFF, $FFCCFF, $00FFFF, $33FFFF, $66FFFF, $99FFFF, $CCFFFF, $FFFFFF
  );

var
  Palette: array[0..255] of integer;

//==============================================================================
//
// RGB
//
//==============================================================================
function RGB(RedValue, GreenValue, BlueValue: word): integer;

//==============================================================================
//
// EncodeColor8
//
//==============================================================================
function EncodeColor8(rgb: LongWord): LongWord;

//==============================================================================
//
// EncodeColor15
//
//==============================================================================
function EncodeColor15(rgb: LongWord): LongWord;

//==============================================================================
//
// EncodeColor24
//
//==============================================================================
function EncodeColor24(rgb: LongWord): LongWord;

//==============================================================================
//
// DecodeColor8
//
//==============================================================================
function DecodeColor8(color: integer): integer;

//==============================================================================
//
// BrightenColor8
//
//==============================================================================
function BrightenColor8(color: integer; light: integer): integer;

//==============================================================================
//
// BrightenColor24
//
//==============================================================================
function BrightenColor24(color: integer; light: integer): integer;

//==============================================================================
//
// setDefaultPalette
//
//==============================================================================
procedure setDefaultPalette(ofs: integer); overload;

//==============================================================================
//
// SetRGBPalette
//
//==============================================================================
procedure SetRGBPalette(index: word; color: integer); overload;

//==============================================================================
//
// SetRGBPalette
//
//==============================================================================
procedure SetRGBPalette(index, RedValue, GreenValue, BlueValue: word); overload;

//==============================================================================
//
// SetRGBPalette
//
//==============================================================================
procedure SetRGBPalette(value: pointer); overload;

implementation

var
  VID_Palette: pointer; // pointer to array of 256 integers

  VID_Color12to8: PByteArray;  // indexcolor := VID_Color12to8[r shl 8 + g shl 4 + b];
  VID_ITable8: PByteArray;
  VID_ITable24: PByteArray;

//==============================================================================
//
// EncodeColor24
//
//==============================================================================
function EncodeColor24(rgb: LongWord): LongWord; assembler;
asm
  and eax, $FFFFFF
end;

//==============================================================================
//
// EncodeColor15
//
//==============================================================================
function EncodeColor15(rgb: LongWord): LongWord; assembler;
asm
  shr  ah, $3
  shr  eax, $3
  shl  ax, $6
  shr  eax, $6
end;

//==============================================================================
//
// EncodeColor8
//
//==============================================================================
function EncodeColor8(rgb: LongWord): LongWord; assembler;
asm
  mov  edx, rgb            // 00000000 RRRRrrrr GGGGgggg BBBBbbbb
  shl  edx, $4             // 0000RRRR rrrrGGGG ggggBBBB bbbb0000
  shl  dx, $4              // 0000RRRR rrrrGGGG BBBBbbbb 00000000
  shr  edx, $12            // 00000000 00000000 RRRRrrrr GGGGBBBB
  shr  dh, $4              // 00000000 00000000 0000RRRR GGGGBBBB
  and  edx, $4095          //
  add  edx, VID_COLOR12TO8
  mov  al, [edx]
end;

//==============================================================================
//
// RGB
//
//==============================================================================
function RGB(RedValue, GreenValue, BlueValue: word): integer; assembler;
asm
  mov  ax, RedValue    // ------------------------RRRRRRRR
  shl  eax, $16        // --------RRRRRRRR----------------
  mov  ax, GreenValue  // --------RRRRRRRR--------RRRRRRRR
  shl  ax, $8          // --------RRRRRRRRGGGGGGGG--------
  mov  dx, BlueValue
  mov  al, dl          // --------RRRRRRRRGGGGGGGGBBBBBBBB
end;

//==============================================================================
//
// InitTables
//
//==============================================================================
procedure InitTables(ofs: integer);
var
  x, y: integer;
  c: integer;
  r, g, b: integer;
  a: byte;
begin
  for r := 0 to 15 do
    for g := 0 to 15 do
      for b := 0 to 15 do
      begin
        vid_color12to8[(r shl 8) or (g shl 4) or b] :=
            ofs + (36 * trunc(b / 2.66666666)) + // Blue Lookup
            (6 * trunc(g / 2.66666666)) + // Green Lookup
            (trunc(r / 2.66666666)); // Red Lookup
      end;
  for y := 0 to 255 do
  begin
    c := Palette[y];
    for x := 0 to 255 do
    begin
      a := EncodeColor8(BrightenColor24(c, Palette[x]));
      VID_ITable8[x + y * 256] := a;
    end;
   end;
end;

//==============================================================================
//
// SetDefaultPalette
//
//==============================================================================
procedure SetDefaultPalette(ofs: integer);
var
  r, b, g, i: integer;
begin
  if ofs > 40 then
    ofs := 40;
  for b := 0 to 5 do
    for g := 0 to 5 do
      for r := 0 to 5 do
      begin
        i := ofs + (36 * b) + (6 * g) + r;
        SetRGBPalette(i, rgb((r * 51), (g * 51), (b * 51)));
      end;
  InitTables(ofs);
end;

//==============================================================================
//
// SetRGBPalette
//
//==============================================================================
procedure SetRGBPalette(index: word; color: integer);assembler;
asm
  push ebx
  xor  eax, eax
  mov  index, ax
  mov  dx, $3c8
  out  dx, al
  mov  ebx, VID_PALETTE
  shl  ax, $2
  add  ebx, eax
  mov  eax, color
  mov  [ebx], eax          //  00000000 RRRRRRRR GGGGGGGG BBBBBBBB
  and  eax, $FCFCFC        //  00000000 RRRRRR00 GGGGGG00 BBBBBB00
  shr  eax, $2             //  00000000 00RRRRRR 00GGGGGG 00BBBBBB
  mov  dx, $3c9
  ror  eax, $16            //  00GGGGGG 00BBBBBB 00000000 00RRRRRR
  out  dx, al
  rol  eax, $8             //  00GGGGGG 00RRRRRR 00000000 00BBBBBB
  out  dx, al
  rol  eax, $8             //  00000000 00RRRRRR 00GGGGGG 00BBBBBB
  out  dx, al
  pop  ebx
end;

//==============================================================================
//
// SetRGBPalette
//
//==============================================================================
procedure SetRGBPalette(index, RedValue, GreenValue, BlueValue: word); assembler;
asm
  push ecx // JVAL unneeded?
  push ebx
  xor  eax, eax
  mov  ax, index
  mov  dx, $3c8
  out  dx, al
  mov  ebx, VID_PALETTE
  shl  ax, $2
  add  ebx, eax
  mov  dx, $3c9
  mov  cx, RedValue
  mov  al, cl
  out  dx, al
  shl  al, $2
  mov  [ebx + 2], al
  mov  cx, GreenValue
  mov  al, cl
  mov  ah, al
  out  dx, al
  mov  cx, BlueValue
  mov  al, cl
  out  dx, al
  and  ax, $CFCF
  shl  ax, $2
  mov  [ebx], ax
  pop  ebx
  pop  ecx // JVAL unneeded?
end;

//==============================================================================
//
// SetRGBPalette
//
//==============================================================================
procedure SetRGBPalette(value: pointer);
var
  i: integer;
begin
  for i := 0 to 255 do
   SetRGBPalette(i, PIntegerArray(value)[i]);
end;

//==============================================================================
//
// DecodeColor8
//
//==============================================================================
function DecodeColor8(color: integer): integer; assembler;
asm
  mov  edx, color
  shl  edx, $2
  and  edx, $1023
  add  edx, VID_PALETTE
  mov  eax, [edx]
end;

//==============================================================================
//
// BrightenColor8
//
//==============================================================================
function BrightenColor8(color: integer; light: integer): integer; assembler;
asm
  push light
  call ENCODECOLOR8
  mov  edx, VID_ITABLE8
  mov  dl, al
  mov  ecx, color
  mov  dh, cl
  mov  al, [edx]
end;

//==============================================================================
//
// BrightenColor24
//
//==============================================================================
function BrightenColor24(color: integer; light: integer): integer; assembler;
asm
  push ebx
  mov  edx, VID_ITABLE24
  mov  eax, color  // BBBBBBBB GGGGGGGG RRRRRRRR ________
  mov  ebx, light  // bbbbbbbb gggggggg rrrrrrrr ________
  mov  dh, al
  mov  dl, bl
  mov  al, [edx]   // bbbbbbbb GGGGGGGG RRRRRRRR ________
  ror  eax, $8     // GGGGGGGG RRRRRRRR ________ bbbbbbbb
  shr  ebx, $8     // gggggggg rrrrrrrr ________ ________
  mov  dh, al
  mov  dl, bl
  mov  al, [edx]   // gggggggg RRRRRRRR ________ bbbbbbbb
  mov  dh, ah
  mov  dl, bh
  mov  ah, [edx]   // gggggggg rrrrrrrr ________ bbbbbbbb
  rol  eax, $8     // bbbbbbbb gggggggg rrrrrrrr ________
  pop  ebx
end;

var
  i, x, y: integer;
  size24: integer;
  size8: integer;

  oVID_ITable24: pointer;
  oVID_ITable8: pointer;

initialization

  VID_Color12to8 := malloc(4096);
  size24 := 65536;
  VID_ITable24 := mallocA(size24, 65536, oVID_ITable24);
  for y := 0 to 255 do
    for x := 0 to 255 do
    begin
      i := (y * x) div 256;
      if (i > 255) then
        i := 255
      else if (i < 0) then
        i := 0;
      VID_ITable24[y * 256 + x] := byte(i);
    end;
  size8 := 65536;
  VID_ITable8 := mallocA(size8, 65536, oVID_ITable8);
  VID_Palette := @Palette;
  Move(DefaultPalette, Palette, 256 * SizeOf(integer));
  InitTables(40);

finalization
  memfree(oVID_ITable24, size24);
  memfree(oVID_ITable8, size8);
  memfree(pointer(VID_Color12to8), 4096);

end.
