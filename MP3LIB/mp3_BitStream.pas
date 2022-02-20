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
 *  File:     $RCSfile: BitStream.pas,v $
 *  Revision: $Revision: 1.1.1.1 $
 *  Version : $Id: BitStream.pas,v 1.1.1.1 2002/04/21 12:57:16 fobmagog Exp $
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
{$DEFINE SEEK_STOP}
{$DEFINE DAMN_INTEL_BYTE_ORDER}
unit mp3_BitStream;

interface

uses
  d_delphi,
  mp3_BitReserve;

type
  TSyncMode = (INITIAL_SYNC, STRICT_SYNC);

const
  BUFFERINTSIZE = 433;
  // max. 1730 bytes per frame: 144 * 384kbit/s / 32000 Hz + 2 Bytes CRC

type
  // Class to extract bitstrings from files:
  TBitStream = class
  private
    FStream: TDStream;

    FBuffer: array[0..BUFFERINTSIZE - 1] of Cardinal;
    FFrameSize: Cardinal;     // number of valid bytes in buffer
    FWordPointer: PCardinal;  // position of next unsigned int for get_bits()
    FBitIndex: Cardinal;      // number (0-31, from MSB to LSB) of next bit for get_bits()
    FSyncWord: Cardinal;
    FSingleChMode: Boolean;
    FCurrentFrameNumber: Integer;
    FLastFrameNumber: Integer;

  public
    NonSeekable: Boolean;
    StreamHandle: Pointer;
    PlayerObj: Pointer;

    property CurrentFrame: Integer read FCurrentFrameNumber;
    property LastFrame: Integer read FLastFrameNumber;

    constructor Create(AStream: TDStream);
    destructor Destroy; override;

    function Restart: Boolean;
    function GetHeader(HeaderString: PCardinal; SyncMode: TSyncMode): Boolean;
    // get next 32 bits from bitstream in an unsigned int,
    // returned value False => end of stream
    function ReadFrame(ByteSize: Cardinal): Boolean;
    // fill buffer with data from bitstream, returned value False => end of stream
    function GetBits(NumberOfBits: Cardinal): Cardinal;
    // read bits (1 <= number_of_bits <= 16) from buffer into the lower bits
    // of an unsigned int. The LSB contains the latest read bit of the stream.
    function GetBitsFloat(NumberOfBits: Cardinal): Single;
    // read bits (1 <= number_of_bits <= 16) from buffer into the lower bits
    // of a floating point. The LSB contains the latest read bit of the stream.
    procedure SetSyncWord(SyncWord: Cardinal);
    // Set the word we want to sync the header to, in
    // Big-Endian byte order
    function FileSize: Cardinal;
    // Returns the size, in bytes, of the input file.

    // Stream searching routines (Jeff Tsay)
    function Seek(Frame: Integer; FrameSize: Integer): Boolean;
    // Seeks to frames
    function SeekPad(Frame: Integer; FrameSize: Integer; var Header: TObject{THeader}; Offset: PCardinalArray): Boolean;
    // Seeks frames for 44.1 or 22.05 kHz (padded) files
  end;

implementation

uses
  mp3_CRC, mp3_Header;

//==============================================================================
//
// SwapInt32
//
//==============================================================================
function SwapInt32(Value: Cardinal): Cardinal;
begin
  result := (Value shl 24) or ((Value shl 8) and $00ff0000) or
            ((Value shr 8) and $0000ff00) or (Value shr 24);
end;

{ TBitStream }

//==============================================================================
//
// TBitStream.Create
//
//==============================================================================
constructor TBitStream.Create(AStream: TDStream);
begin
  FStream := AStream;

  Restart;
  NonSeekable := false;
end;

//==============================================================================
//
// TBitStream.Destroy
//
//==============================================================================
destructor TBitStream.Destroy;
begin
//  FStream.Free;

  inherited Destroy;
end;

//==============================================================================
//
// TBitStream.FileSize
//
//==============================================================================
function TBitStream.FileSize: Cardinal;
begin
  result := FStream.Size;
end;

const
  BitMask: array[0..17] of Cardinal = (
    0,  // dummy
    $00000001, $00000003, $00000007, $0000000F,
    $0000001F, $0000003F, $0000007F, $000000FF,
    $000001FF, $000003FF, $000007FF, $00000FFF,
    $00001FFF, $00003FFF, $00007FFF, $0000FFFF,
    $0001FFFF);

//==============================================================================
//
// TBitStream.GetBits
//
//==============================================================================
function TBitStream.GetBits(NumberOfBits: Cardinal): Cardinal;
var
  ReturnValue: Cardinal;
  Sum: Cardinal;
begin
  Sum := FBitIndex + NumberOfBits;

  if sum <= 32 then
  begin
    // all bits contained in *wordpointer
    result := (FWordPointer^ shr (32 - sum)) and BitMask[NumberOfBits];
    inc(FBitIndex, NumberOfBits);
    if FBitIndex = 32 then
    begin
      FBitIndex := 0;
      inc(FWordPointer);
    end;

    exit;
  end;

{$IFDEF DAMN_INTEL_BYTE_ORDER}
  PWord(@PByteArray(@ReturnValue)[2])^ := PWord(FWordPointer)^;
  inc(FWordPointer);
  PWord(@ReturnValue)^ := PWord(@PByteArray(FWordPointer)[2])^;
{$ELSE}
  PWord(@ReturnValue)^ := PWord(@PByteArray(FWordPointer)[2])^;
  inc(FWordPointer);
  PWord(@PByteArray(@ReturnValue)[2])^ := PWord(FWordPointer)^;
{$ENDIF}

  ReturnValue := ReturnValue shr (48 - Sum);  // returnvalue >>= 16 - (number_of_bits - (32 - bitindex))
  result := ReturnValue and BitMask[NumberOfBits];
  FBitIndex := Sum - 32;
end;

//==============================================================================
//
// TBitStream.GetBitsFloat
//
//==============================================================================
function TBitStream.GetBitsFloat(NumberOfBits: Cardinal): Single;
begin
  PCardinal(@result)^ := GetBits(NumberOfBits);
end;

//==============================================================================
//
// TBitStream.GetHeader
//
//==============================================================================
function TBitStream.GetHeader(HeaderString: PCardinal;
  SyncMode: TSyncMode): Boolean;
var
  Sync: Boolean;
  NumRead: Integer;
begin
  repeat
    // Read 4 bytes from the file, placing the number of bytes actually
    // read in numread
    NumRead := FStream.Read(HeaderString^, 4);
    result := NumRead = 4;
    if not result then
      exit;

    if SyncMode = INITIAL_SYNC then
      Sync := ((HeaderString^ and $0000F0FF) = $0000F0FF)
    else
      Sync := ((HeaderString^ and $000CF8FF) = FSyncWord) and
              (((HeaderString^ and $C0000000) = $C0000000) = FSingleChMode);

    if not Sync then
      // rewind 3 bytes in the file so we can try to sync again, if
      // successful set result to TRUE
      FStream.Seek(-3, sFromCurrent);
  until Sync or (not result);

  if not result then
    exit;

{$IFDEF DAMN_INTEL_BYTE_ORDER}
  HeaderString^ := SwapInt32(HeaderString^);
{$ENDIF}

  inc(FCurrentFrameNumber);

{$IFDEF SEEK_STOP}
  if FLastFrameNumber < FCurrentFrameNumber then
    FLastFrameNumber := FCurrentFrameNumber;
{$ENDIF}

  result := true;
end;

//==============================================================================
//
// TBitStream.ReadFrame
//
//==============================================================================
function TBitStream.ReadFrame(ByteSize: Cardinal): Boolean;
var NumRead: Integer;
{$IFDEF DAMN_INTEL_BYTE_ORDER}
    WordP: PCardinal;
{$ENDIF}
begin
  // read bytesize bytes from the file, placing the number of bytes
  // actually read in numread and setting result to TRUE if
  // successful
  NumRead := FStream.Read(FBuffer, ByteSize);

  FWordPointer := @FBuffer;
  FBitIndex := 0;
  FFrameSize := ByteSize;

{$IFDEF DAMN_INTEL_BYTE_ORDER}
  WordP := @FBuffer[(ByteSize - 1) shr 2];
  while Cardinal(WordP) >= Cardinal(@FBuffer) do
  begin
    WordP^ := SwapInt32(WordP^);
    dec(WordP);
  end;
{$ENDIF}

  result := Cardinal(NumRead) = FFrameSize;
end;

//==============================================================================
//
// TBitStream.Restart
//
//==============================================================================
function TBitStream.Restart: Boolean;
begin
  FStream.Seek(0, sFromBeginning);
  FWordPointer := @FBuffer;
  FBitIndex := 0;

  // Seeking variables
  FCurrentFrameNumber := -1;
  FLastFrameNumber := -1;
  result := true;
end;

//==============================================================================
//
// TBitStream.Seek
//
//==============================================================================
function TBitStream.Seek(Frame, FrameSize: Integer): Boolean;
begin
  FCurrentFrameNumber := Frame - 1;
  if NonSeekable then
  begin
    result := false;
    exit;
  end;

  FStream.Seek(Frame * (FrameSize + 4), sFromBeginning);
  result := true;
end;

//==============================================================================
//
// TBitStream.SeekPad
//
//==============================================================================
function TBitStream.SeekPad(Frame, FrameSize: Integer;
  var Header: TObject; Offset: PCardinalArray): Boolean;
var
  CRC: TCRC16;
  TotalFrameSize: Integer;
  Diff: Integer;
begin
  // base_frame_size is the frame size _without_ padding.
  if NonSeekable then
  begin
    result := false;
    exit;
  end;

  CRC := nil;

  TotalFrameSize := FrameSize + 4;

  if FLastFrameNumber < Frame then
  begin
    if FLastFrameNumber >= 0 then
      Diff := Offset[FLastFrameNumber]
    else
      Diff := 0;

    // set the file pointer to ((last_frame_number+1) * total_frame_size)
    // bytes after the beginning of the file
    FStream.Seek((FLastFrameNumber + 1) * TotalFrameSize + Diff, sFromBeginning);
    FCurrentFrameNumber := FLastFrameNumber;

    repeat
      if not THeader(Header).ReadHeader(Self, CRC) then
      begin
        result := false;
        exit;
      end;
    until (FLastFrameNumber >= Frame);

    result := true;
  end
  else
  begin
    if (Frame > 0) then
      Diff := Offset[Frame - 1]
    else
      Diff := 0;

    // set the file pointer to (frame * total_frame_size  + diff) bytes
    // after the beginning of the file
    FStream.Seek(Frame * TotalFrameSize + Diff, sFrombeginning);
    FCurrentFrameNumber := Frame - 1;
    result := THeader(Header).ReadHeader(Self, CRC);
  end;

  if CRC <> nil then
    FreeAndNil(CRC);
end;

//==============================================================================
//
// TBitStream.SetSyncWord
//
//==============================================================================
procedure TBitStream.SetSyncWord(SyncWord: Cardinal);
begin
{$IFDEF DAMN_INTEL_BYTE_ORDER}
  FSyncWord := SwapInt32(Syncword and $FFFFFF3F);
{$ELSE}
  FSyncWord := SyncWord and $FFFFFF3F;
{$ENDIF}

  FSingleChMode := ((SyncWord and $000000C0) = $000000C0);
end;

end.
