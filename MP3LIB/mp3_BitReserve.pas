//------------------------------------------------------------------------------
//
//  DelphiDoom is a source port of the game Doom and it is
//  based on original Linux Doom as published by "id Software"
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
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

(*
 *  File:     $RCSfile: BitReserve.pas,v $
 *  Revision: $Revision: 1.1.1.1 $
 *  Version : $Id: BitReserve.pas,v 1.1.1.1 2002/04/21 12:57:16 fobmagog Exp $
 *  Author:   $Author: fobmagog $
 *  Homepage: http://delphimpeg.sourceforge.net/
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *)
unit mp3_BitReserve;

interface

const
  BUFSIZE = 4096;

type
  PCardinalArray = ^TCardinalArray;
  TCardinalArray = array[0..1024*1024*32] of Cardinal;

  TBitReserve = class
  private
    FOffset, FTotbit, FBufByteIdx: Cardinal;
    FBuf: PCardinalArray;
    FBufBitIdx: Cardinal;
    FPutMask: PCardinalArray;

  public
    property hsstell: Cardinal read FTotBit;

    constructor Create;
    destructor Destroy; override;

    function hgetbits(n: Cardinal): Cardinal;
    function hget1bit: Cardinal;
    procedure hputbuf(val: Cardinal);

    procedure rewindNbits(n: Cardinal);
    procedure rewindNbytes(n: Cardinal);
  end;

implementation

{ TBitReserve }

//==============================================================================
//
// TBitReserve.Create
//
//==============================================================================
constructor TBitReserve.Create;
var ShiftedOne, i: Cardinal;
begin
  inherited Create;

  ShiftedOne := 1;
  FOffset := 0;
  FTotbit := 0;
  FBufByteIdx := 0;
  GetMem(FBuf, BUFSIZE * SizeOf(Cardinal));
  FBufBitIdx := 8;
  GetMem(FPutMask, 32 * SizeOf(Cardinal));

  FPutMask[0] := 0;
  for i := 1 to 31 do
  begin
    FPutMask[i] := FPutMask[i-1] + ShiftedOne;
    ShiftedOne := ShiftedOne shl 1;
  end;
end;

//==============================================================================
//
// TBitReserve.Destroy
//
//==============================================================================
destructor TBitReserve.Destroy;
begin
  FreeMem(FPutMask);
  FreeMem(FBuf);

  inherited Destroy;
end;

//==============================================================================
// TBitReserve.hget1bit
//
// read 1 bit from the bit stream
//
//==============================================================================
function TBitReserve.hget1bit: Cardinal;
var val: Cardinal;
begin
  inc(FTotbit);

  if FBufBitIdx = 0 then
  begin
    FBufBitIdx := 8;
    inc(FBufByteIdx);
  end;

  // BUFSIZE = 4096 = 2^12, so
  // buf_byte_idx%BUFSIZE == buf_byte_idx & 0xfff
  val := FBuf[FBufByteIdx and $fff] and FPutMask[FBufBitIdx];
  dec(FBufBitIdx);
  result := val shr FBufBitIdx;
end;

//==============================================================================
// TBitReserve.hgetbits
//
// read N bits from the bit stream
//
//==============================================================================
function TBitReserve.hgetbits(n: Cardinal): Cardinal;
var val: Cardinal;
    j, k, tmp: Cardinal;
begin
  inc(FTotbit, n);

  val := 0;
  j := N;

  while j > 0 do
  begin
    if FBufBitIdx = 0 then
    begin
      FBufBitIdx := 8;
      inc(FBufByteIdx);
    end;

    if (j < FBufBitIdx) then
      k := j
    else
      k := FBufBitIdx;

    // BUFSIZE = 4096 = 2^12, so
    // buf_byte_idx%BUFSIZE == buf_byte_idx & 0xfff
    tmp := FBuf[FBufByteIdx and $fff] and FPutMask[FBufBitIdx];
    dec(FBufBitIdx, k);
    tmp := tmp shr FBufBitIdx;
    dec(j, k);
    val := val or (tmp shl j);
  end;

  result := val;
end;

//==============================================================================
// TBitReserve.hputbuf
//
// write 8 bits into the bit stream
//
//==============================================================================
procedure TBitReserve.hputbuf(val: Cardinal);
begin
  FBuf[FOffset] := val;
  FOffset := (FOffset + 1) and $fff;
end;

//==============================================================================
//
// TBitReserve.rewindNbits
//
//==============================================================================
procedure TBitReserve.rewindNbits(n: Cardinal);
begin
  dec(FTotBit, n);
  inc(FBufBitIdx, n);

  while FBufBitIdx >= 8 do
  begin
    dec(FBufBitIdx, 8);
    dec(FBufByteIdx);
  end;
end;

//==============================================================================
//
// TBitReserve.rewindNbytes
//
//==============================================================================
procedure TBitReserve.rewindNbytes(n: Cardinal);
begin
  dec(FTotBit, (N shl 3));
  dec(FBufByteIdx, N);
end;

end.
