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
//  SHA1
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit m_sha1;

interface

//==============================================================================
//
// SHA1_CalcSHA1String
//
//==============================================================================
function SHA1_CalcSHA1String(const Buf: AnsiString): AnsiString;

//==============================================================================
//
// SHA1_CalcSHA1Buf
//
//==============================================================================
function SHA1_CalcSHA1Buf(const Buf; const BufSize: Integer): AnsiString;

implementation

type
  T160BitDigest = record
    case integer of
      0: (Longs: array[0..4] of LongWord);
      1: (Words: array[0..9] of Word);
      2: (Bytes: array[0..19] of Byte);
    end;
  P160BitDigest = ^T160BitDigest;

//==============================================================================
//
// SHA1InitDigest
//
//==============================================================================
procedure SHA1InitDigest(var Digest: T160BitDigest);
begin
  Digest.Longs[0] := $67452301;
  Digest.Longs[1] := $EFCDAB89;
  Digest.Longs[2] := $98BADCFE;
  Digest.Longs[3] := $10325476;
  Digest.Longs[4] := $C3D2E1F0;
end;

//==============================================================================
//
// SwapEndian
//
//==============================================================================
function SwapEndian(const Value: LongWord): LongWord; register; assembler;
asm
  XCHG    AH, AL
  ROL     EAX, 16
  XCHG    AH, AL
end;

//==============================================================================
//
// RotateLeftBits
//
//==============================================================================
function RotateLeftBits(const Value: LongWord; const Bits: Byte): LongWord;
asm
  MOV     CL, DL
  ROL     EAX, CL
end;

{ Calculates a SHA Digest (20 bytes) given a Buffer (64 bytes)                 }
{$IFOPT Q+}{$DEFINE QOn}{$Q-}{$ELSE}{$UNDEF QOn}{$ENDIF}

//==============================================================================
//
// TransformSHABuffer
//
//==============================================================================
procedure TransformSHABuffer(var Digest: T160BitDigest; const Buffer; const SHA1: Boolean);
var
  A, B, C, D, E: LongWord;
  W: array[0..79] of LongWord;
  P, Q: PLongWord;
  I: Integer;
  J: LongWord;
begin
  P := @Buffer;
  Q := @W;
  for I := 0 to 15 do
  begin
    Q^ := SwapEndian(P^);
    Inc(P);
    Inc(Q);
  end;
  for I := 0 to 63 do
  begin
    P := Q;
    Dec(P, 16);
    J := P^;
    Inc(P, 2);
    J := J xor P^;
    Inc(P, 6);
    J := J xor P^;
    Inc(P, 5);
    J := J xor P^;
    if SHA1 then
      J := RotateLeftBits(J, 1);
    Q^ := J;
    Inc(Q);
  end;

  A := Digest.Longs[0];
  B := Digest.Longs[1];
  C := Digest.Longs[2];
  D := Digest.Longs[3];
  E := Digest.Longs[4];

  P := @W;
  for I := 0 to 3 do
  begin
    Inc(E, (A shl 5 or A shr 27) + (D xor (B and (C xor D))) + P^ + $5A827999); B := B shr 2 or B shl 30; Inc(P);
    Inc(D, (E shl 5 or E shr 27) + (C xor (A and (B xor C))) + P^ + $5A827999); A := A shr 2 or A shl 30; Inc(P);
    Inc(C, (D shl 5 or D shr 27) + (B xor (E and (A xor B))) + P^ + $5A827999); E := E shr 2 or E shl 30; Inc(P);
    Inc(B, (C shl 5 or C shr 27) + (A xor (D and (E xor A))) + P^ + $5A827999); D := D shr 2 or D shl 30; Inc(P);
    Inc(A, (B shl 5 or B shr 27) + (E xor (C and (D xor E))) + P^ + $5A827999); C := C shr 2 or C shl 30; Inc(P);
  end;

  for I := 0 to 3 do
  begin
    Inc(E, (A shl 5 or A shr 27) + (D xor B xor C) + P^ + $6ED9EBA1); B := B shr 2 or B shl 30; Inc(P);
    Inc(D, (E shl 5 or E shr 27) + (C xor A xor B) + P^ + $6ED9EBA1); A := A shr 2 or A shl 30; Inc(P);
    Inc(C, (D shl 5 or D shr 27) + (B xor E xor A) + P^ + $6ED9EBA1); E := E shr 2 or E shl 30; Inc(P);
    Inc(B, (C shl 5 or C shr 27) + (A xor D xor E) + P^ + $6ED9EBA1); D := D shr 2 or D shl 30; Inc(P);
    Inc(A, (B shl 5 or B shr 27) + (E xor C xor D) + P^ + $6ED9EBA1); C := C shr 2 or C shl 30; Inc(P);
  end;

  for I := 0 to 3 do
  begin
    Inc(E, (A shl 5 or A shr 27) + ((B and C) or (D and (B or C))) + P^ + $8F1BBCDC); B := B shr 2 or B shl 30; Inc(P);
    Inc(D, (E shl 5 or E shr 27) + ((A and B) or (C and (A or B))) + P^ + $8F1BBCDC); A := A shr 2 or A shl 30; Inc(P);
    Inc(C, (D shl 5 or D shr 27) + ((E and A) or (B and (E or A))) + P^ + $8F1BBCDC); E := E shr 2 or E shl 30; Inc(P);
    Inc(B, (C shl 5 or C shr 27) + ((D and E) or (A and (D or E))) + P^ + $8F1BBCDC); D := D shr 2 or D shl 30; Inc(P);
    Inc(A, (B shl 5 or B shr 27) + ((C and D) or (E and (C or D))) + P^ + $8F1BBCDC); C := C shr 2 or C shl 30; Inc(P);
  end;

  for I := 0 to 3 do
  begin
    Inc(E, (A shl 5 or A shr 27) + (D xor B xor C) + P^ + $CA62C1D6); B := B shr 2 or B shl 30; Inc(P);
    Inc(D, (E shl 5 or E shr 27) + (C xor A xor B) + P^ + $CA62C1D6); A := A shr 2 or A shl 30; Inc(P);
    Inc(C, (D shl 5 or D shr 27) + (B xor E xor A) + P^ + $CA62C1D6); E := E shr 2 or E shl 30; Inc(P);
    Inc(B, (C shl 5 or C shr 27) + (A xor D xor E) + P^ + $CA62C1D6); D := D shr 2 or D shl 30; Inc(P);
    Inc(A, (B shl 5 or B shr 27) + (E xor C xor D) + P^ + $CA62C1D6); C := C shr 2 or C shl 30; Inc(P);
  end;

  Inc(Digest.Longs[0], A);
  Inc(Digest.Longs[1], B);
  Inc(Digest.Longs[2], C);
  Inc(Digest.Longs[3], D);
  Inc(Digest.Longs[4], E);
end;
{$IFDEF QOn}{$Q+}{$ENDIF}

//==============================================================================
//
// SHA1Buf
//
//==============================================================================
procedure SHA1Buf(var Digest: T160BitDigest; const Buf; const BufSize: Integer);
var
  P: PByte;
  I, J: Integer;
begin
  I := BufSize;
  if I <= 0 then
    Exit;
  Assert(I mod 64 = 0, 'BufSize must be multiple of 64 bytes');
  P := @Buf;
  for J := 0 to I div 64 - 1 do
  begin
    TransformSHABuffer(Digest, P^, True);
    Inc(P, 64);
  end;
end;

type
  T512BitBuf  = array[0..63] of Byte;

//==============================================================================
//
// ReverseMem
//
//==============================================================================
procedure ReverseMem(var Buf; const BufSize: Integer);
var
  I: Integer;
  P: PByte;
  Q: PByte;
  T: Byte;
begin
  P := @Buf;
  Q := P;
  Inc(Q, BufSize - 1);
  for I := 1 to BufSize div 2 do
  begin
    T := P^;
    P^ := Q^;
    Q^ := T;
    Inc(P);
    Dec(Q);
  end;
end;

{                                                                              }
{ StdFinalBuf                                                                  }
{ Utility function to prepare final buffer(s).                                 }
{ Fills Buf1 and potentially Buf2 from Buf (FinalBufCount = 1 or 2).           }
{ Used by MD5, SHA1, SHA256, SHA512.                                           }
{                                                                              }
procedure StdFinalBuf512(
          const Buf; const BufSize: Integer; const TotalSize: Int64;
          var Buf1, Buf2: T512BitBuf;
          var FinalBufs: Integer;
          const SwapEndian: Boolean);
var
  P, Q: PByte;
  I: Integer;
  L: Int64;
begin
  Assert(BufSize < 64, 'Final BufSize must be less than 64 bytes');
  Assert(TotalSize >= BufSize, 'TotalSize >= BufSize');

  P := @Buf;
  Q := @Buf1[0];
  if BufSize > 0 then
  begin
    Move(P^, Q^, BufSize);
    Inc(Q, BufSize);
  end;
  Q^ := $80;
  Inc(Q);

  L := Int64(TotalSize * 8);
  if SwapEndian then
    ReverseMem(L, 8);
  if BufSize + 1 > 64 - Sizeof(Int64) then
  begin
    FillChar(Q^, 64 - BufSize - 1, #0);
    Q := @Buf2[0];
    FillChar(Q^, 64 - Sizeof(Int64), #0);
    Inc(Q, 64 - Sizeof(Int64));
    PInt64(Q)^ := L;
    FinalBufs := 2;
  end
  else
  begin
    I := 64 - Sizeof(Int64) - BufSize - 1;
    FillChar(Q^, I, #0);
    Inc(Q, I);
    PInt64(Q)^ := L;
    FinalBufs := 1;
  end;
end;

//==============================================================================
//
// SwapEndianBuf
//
//==============================================================================
procedure SwapEndianBuf(var Buf; const Count: Integer);
var
  P: PLongWord;
  I: Integer;
begin
  P := @Buf;
  for I := 1 to Count do
  begin
    P^ := SwapEndian(P^);
    Inc(P);
  end;
end;

//==============================================================================
//
// SecureClear
//
//==============================================================================
procedure SecureClear(var Buf; const BufSize: Integer);
begin
  if BufSize <= 0 then
    Exit;
  FillChar(Buf, BufSize, #$00);
end;

//==============================================================================
//
// SecureClear512
//
//==============================================================================
procedure SecureClear512(var Buf: T512BitBuf);
begin
  SecureClear(Buf, SizeOf(Buf));
end;

//==============================================================================
//
// SHA1FinalBuf
//
//==============================================================================
procedure SHA1FinalBuf(var Digest: T160BitDigest; const Buf; const BufSize: Integer; const TotalSize: Int64);
var
  B1, B2: T512BitBuf;
  C: Integer;
begin
  StdFinalBuf512(Buf, BufSize, TotalSize, B1, B2, C, True);
  TransformSHABuffer(Digest, B1, True);
  if C > 1 then
    TransformSHABuffer(Digest, B2, True);
  SwapEndianBuf(Digest, Sizeof(Digest) div Sizeof(LongWord));
  SecureClear512(B1);
  if C > 1 then
    SecureClear512(B2);
end;

//==============================================================================
//
// CalcSHA1Buf
//
//==============================================================================
function CalcSHA1Buf(const Buf; const BufSize: Integer): T160BitDigest;
var
  I, J: Integer;
  P: PByte;
begin
  SHA1InitDigest(Result);
  P := @Buf;
  if BufSize <= 0 then
    I := 0
  else
    I := BufSize;
  J := (I div 64) * 64;
  if J > 0 then
  begin
    SHA1Buf(Result, P^, J);
    Inc(P, J);
    Dec(I, J);
  end;
  SHA1FinalBuf(Result, P^, I, BufSize);
end;

//==============================================================================
//
// CalcSHA1String
//
//==============================================================================
function CalcSHA1String(const Buf: AnsiString): T160BitDigest;
begin
  Result := CalcSHA1Buf(Pointer(Buf)^, Length(Buf));
end;

//==============================================================================
//
// SHA1DigestAsString
//
//==============================================================================
function SHA1DigestAsString(const Digest: T160BitDigest): AnsiString;
begin
  SetLength(Result, Sizeof(Digest));
  Move(Digest, Pointer(Result)^, Sizeof(Digest));
end;

//==============================================================================
//
// SHA1_CalcSHA1String
//
//==============================================================================
function SHA1_CalcSHA1String(const Buf: AnsiString): AnsiString;
begin
  result := SHA1DigestAsString(CalcSHA1String(Buf));
end;

//==============================================================================
//
// SHA1_CalcSHA1Buf
//
//==============================================================================
function SHA1_CalcSHA1Buf(const Buf; const BufSize: Integer): AnsiString;
begin
  result := SHA1DigestAsString(CalcSHA1Buf(Buf, BufSize));
end;

end.

