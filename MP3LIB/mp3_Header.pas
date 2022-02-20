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
 *  File:     $RCSfile: Header.pas,v $
 *  Revision: $Revision: 1.1.1.1 $
 *  Version : $Id: Header.pas,v 1.1.1.1 2002/04/21 12:57:16 fobmagog Exp $
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

unit mp3_Header;

interface
uses
  Windows, mp3_CRC, mp3_BitStream, mp3_BitReserve;

type
  TVersion         = (MPEG2_LSF, MPEG1);
  TMode            = (Stereo, JointStereo, DualChannel, SingleChannel);
  TSampleFrequency = (FourtyFourPointOne, FourtyEight, ThirtyTwo, Unknown);

const
  FREQUENCIES: array[TVersion, TSampleFrequency] of Cardinal = (
    (22050, 24000, 16000, 1),
    (44100, 48000, 32000, 1));

type
  // Class for extraction information from a frame header:
  THeader = class
  private
    FLayer: Cardinal;
    FProtectionBit: Cardinal;
    FBitrateIndex: Cardinal;
    FPaddingBit: Cardinal;
    FModeExtension: Cardinal;
    FVersion: TVersion;
    FMode: TMode;
    FSampleFrequency: TSampleFrequency;
    FNumberOfSubbands: Cardinal;
    FIntensityStereoBound: Cardinal;
    FCopyright: Boolean;
    FOriginal: Boolean;
    FInitialSync: Boolean;
    FCRC: TCRC16;
    FOffset: PCardinalArray;
    FChecksum: Cardinal;
    FFrameSize: Cardinal;
    FNumSlots: Cardinal;

    function GetFrequency: Cardinal;
    function GetChecksums: Boolean;
    function GetChecksumOK: Boolean;
    function GetPadding: Boolean;

  public
    property Version: TVersion read FVersion;
    property Layer: Cardinal read FLayer;
    property BitrateIndex: Cardinal read FBitrateIndex;
    property SampleFrequency: TSampleFrequency read FSampleFrequency;
    property Frequency: Cardinal read GetFrequency;
    property Mode: TMode read FMode;
    property Checksums: Boolean read GetChecksums;
    property Copyright: Boolean read FCopyright;
    property Original: Boolean read FOriginal;
    property ChecksumOK: Boolean read GetChecksumOK;
    // compares computed checksum with stream checksum
    property Padding: Boolean read GetPadding;
    property Slots: Cardinal read FNumSlots;
    property ModeExtension: Cardinal read FModeExtension;
    property NumberOfSubbands: Cardinal read FNumberOfSubbands;
    // returns the number of subbands in the current frame
    property IntensityStereoBound: Cardinal read FIntensityStereoBound;
    // (Layer II joint stereo only)
    // returns the number of subbands which are in stereo mode,
    // subbands above that limit are in intensity stereo mode

    constructor Create;
    destructor Destroy; override;

    function ReadHeader(Stream: TBitStream; var CRC: TCRC16): Boolean;
    // read a 32-bit header from the bitstream

    function Bitrate: Cardinal;

    function CalculateFrameSize: Cardinal;

    // Scrolling stuff
    function StreamSeek(Stream: TBitStream; SeekPos: Cardinal): Boolean;
    function MaxNumberOfFrames(Stream: TBitStream): Integer;
    function MinNumberOfFrames(Stream: TBitStream): Integer;

    function MSPerFrame: Single;  // milliseconds per frame, for time display
    function TotalMS(Stream: TBitStream): Single;
  end;

implementation

{ THeader }

const
  BITRATES: array[TVersion, 0..2, 0..15] of Cardinal = (
    ((0 {free format}, 32000, 48000, 56000, 64000, 80000, 96000, 112000, 128000, 144000, 160000, 176000, 192000 ,224000, 256000, 0),
     (0 {free format}, 8000, 16000, 24000, 32000, 40000, 48000, 56000, 64000, 80000, 96000, 112000, 128000, 144000, 160000, 0),
     (0 {free format}, 8000, 16000, 24000, 32000, 40000, 48000, 56000, 64000, 80000, 96000, 112000, 128000, 144000, 160000, 0)),
    ((0 {free format}, 32000, 64000, 96000, 128000, 160000, 192000, 224000, 256000, 288000, 320000, 352000, 384000, 416000, 448000, 0),
     (0 {free format}, 32000, 48000, 56000, 64000, 80000, 96000, 112000, 128000, 160000, 192000, 224000, 256000, 320000, 384000, 0),
     (0 {free format}, 32000, 40000, 48000, 56000, 64000, 80000, 96000, 112000, 128000, 160000, 192000, 224000, 256000, 320000, 0))
    );

//==============================================================================
//
// THeader.Bitrate
//
//==============================================================================
function THeader.Bitrate: Cardinal;
begin
  result := BITRATES[FVersion, FLayer - 1, FBitrateIndex];
end;

//==============================================================================
// THeader.CalculateFrameSize
//
// calculates framesize in bytes excluding header size
//
//==============================================================================
function THeader.CalculateFrameSize: Cardinal;
var
  Val1, Val2: Cardinal;
begin
  if (FLayer = 1) then
  begin
    FFramesize := (12 * BITRATES[FVersion, 0, FBitrateIndex]) div FREQUENCIES[FVersion, FSampleFrequency];

    if (FPaddingBit <> 0) then
      inc(FFrameSize);

    FFrameSize := FFrameSize shl 2;  // one slot is 4 bytes long

    FNumSlots := 0;
  end
  else
  begin
    FFrameSize := (144 * BITRATES[FVersion, FLayer - 1, FBitrateIndex]) div FREQUENCIES[FVersion, FSampleFrequency];

    if (FVersion = MPEG2_LSF) then
      FFrameSize := FFrameSize shr 1;

    if (FPaddingBit <> 0) then
      inc(FFrameSize);

    // Layer III slots
    if FLayer = 3 then
    begin
      if FVersion = MPEG1 then
      begin
        if FMode = SingleChannel then
          Val1 := 17
        else
          Val1 := 32;

        if FProtectionBit <> 0 then
          Val2 := 0
        else
          Val2 := 2;

        FNumSlots := FFramesize - Val1 - Val2 - 4;  // header size
      end
      else
      begin  // MPEG-2 LSF
        if FMode = SingleChannel then
          Val1 := 9
        else
          Val1 := 17;

        if FProtectionBit <> 0 then
          Val2 := 0
        else
          Val2 := 2;

        FNumSlots := FFramesize - Val1 - Val2 - 4;  // header size
      end;
    end
    else
      FNumSlots := 0;
  end;

  dec(FFrameSize, 4);  // subtract header size

  result := FFrameSize;
end;

//==============================================================================
//
// THeader.Create
//
//==============================================================================
constructor THeader.Create;
begin
  FFrameSize := 0;
  FNumSlots := 0;
  FCRC := nil;
  FOffset := nil;
  FInitialSync := false;
end;

//==============================================================================
//
// THeader.Destroy
//
//==============================================================================
destructor THeader.Destroy;
begin
  if FOffset <> nil then
    FreeMem(FOffset);

  inherited;
end;

//==============================================================================
//
// THeader.GetChecksumOK
//
//==============================================================================
function THeader.GetChecksumOK: Boolean;
begin
  result := (FChecksum = FCRC.Checksum);
end;

//==============================================================================
//
// THeader.GetChecksums
//
//==============================================================================
function THeader.GetChecksums: Boolean;
begin
  result := (FProtectionBit = 0);
end;

//==============================================================================
//
// THeader.GetFrequency
//
//==============================================================================
function THeader.GetFrequency: Cardinal;
begin
  result := FREQUENCIES[FVersion, FSampleFrequency];
end;

//==============================================================================
//
// THeader.GetPadding
//
//==============================================================================
function THeader.GetPadding: Boolean;
begin
  result := (FPaddingBit <> 0);
end;

//==============================================================================
// THeader.MaxNumberOfFrames
//
// Returns the maximum number of frames in the stream
//
//==============================================================================
function THeader.MaxNumberOfFrames(Stream: TBitStream): Integer;
begin
  result := Stream.FileSize div (FFrameSize + 4 - FPaddingBit);
end;

//==============================================================================
// THeader.MinNumberOfFrames
//
// Returns the minimum number of frames in the stream
//
//==============================================================================
function THeader.MinNumberOfFrames(Stream: TBitStream): Integer;
begin
  result := Stream.FileSize div (FFrameSize + 5 - FPaddingBit);
end;

const
  MSPerFrameArray: array[0..2, TSampleFrequency] of Single = (
    (8.707483,  8.0, 12.0, 0),
    (26.12245, 24.0, 36.0, 0),
    (26.12245, 24.0, 36.0, 0));

//==============================================================================
//
// THeader.MSPerFrame
//
//==============================================================================
function THeader.MSPerFrame: Single;
begin
  result := MSperFrameArray[FLayer-1, FSampleFrequency];
end;

//==============================================================================
//
// THeader.ReadHeader
//
//==============================================================================
function THeader.ReadHeader(Stream: TBitStream; var CRC: TCRC16): Boolean;
var
  HeaderString, ChannelBitrate: Cardinal;
  max, cf, lf, i: Integer;
begin
  result := false;
  if not FInitialSync then
  begin
    if not Stream.GetHeader(@HeaderString, INITIAL_SYNC) then
      exit;

    FVersion := TVersion((HeaderString shr 19) and 1);
    FSampleFrequency := TSampleFrequency((HeaderString shr 10) and 3);
    if (FSampleFrequency = Unknown) then
    begin
      // report error - not supported header
      exit;
    end;

    Stream.SetSyncWord(HeaderString and $FFF80CC0);

    FInitialSync := true;
  end
  else
  begin
    if not Stream.GetHeader(@HeaderString, STRICT_SYNC) then
    begin
      exit;
    end;
  end;

  FLayer := 4 - (HeaderString shr 17) and 3;
  FProtectionBit := (HeaderString shr 16) and 1;
  FBitrateIndex := (HeaderString shr 12) and $F;
  FPaddingBit := (HeaderString shr 9) and 1;
  FMode := TMode((HeaderString shr 6) and 3);
  FModeExtension := (HeaderString shr 4) and 3;

  if FMode = JointStereo then
    FIntensityStereoBound := (FModeExtension shl 2) + 4
  else
    FIntensityStereoBound := 0;  // should never be used

  FCopyright := ((HeaderString shr 3) and 1 <> 0);
  FOriginal := ((HeaderString shr 2) and 1 <> 0);

  // calculate number of subbands:
  if FLayer = 1 then
    FNumberOfSubbands := 32
  else
  begin
    ChannelBitrate := FBitrateIndex;

    // calculate bitrate per channel:
    if FMode <> SingleChannel then
      if ChannelBitrate = 4 then
        ChannelBitrate := 1
      else
        dec(ChannelBitrate, 4);

    if (ChannelBitrate = 1) or (ChannelBitrate = 2) then
    begin
      if FSampleFrequency = ThirtyTwo then
        FNumberOfSubbands := 12
      else
        FNumberOfSubbands := 8;
    end
    else
    begin
      if (FSampleFrequency = FourtyEight) or ((ChannelBitrate >= 3) and (ChannelBitrate <= 5)) then
        FNumberOfSubbands := 27
      else
        FNumberOfSubbands := 30;
    end;
  end;

  if (FIntensityStereoBound > FNumberOfSubbands) then
    FIntensityStereoBound := FNumberOfSubbands;

  // calculate framesize and nSlots
  CalculateFrameSize;

  // read framedata:
  if not Stream.ReadFrame(FFrameSize) then
  begin
    exit;
  end;

  if FProtectionBit = 0 then
  begin
    // frame contains a crc checksum
    FChecksum := Stream.GetBits(16);
    if FCRC = nil then
      FCRC := TCRC16.Create;

    FCRC.AddBits(HeaderString, 16);
    CRC := FCRC;
  end
  else
    CRC := nil;

{$IFDEF SEEK_STOP}
  if FSampleFrequency = FourtyFourPointOne then
  begin
    if FOffset = nil then
    begin
      max := MaxNumberOfFrames(Stream);
      GetMem(FOffset, Max * sizeof(Cardinal));

      for i := 0 to max - 1 do
        FOffset[i] := 0;
    end;

    cf := Stream.CurrentFrame;
    lf := Stream.LastFrame;
    if (cf > 0) and (cf = lf) then
      FOffset[cf] := FOffset[cf - 1] + FPaddingBit
    else
      FOffset[0] := FPaddingBit;
  end;
{$ENDIF}

  result := true;
end;

//==============================================================================
// THeader.StreamSeek
//
// Stream searching routines
//
//==============================================================================
function THeader.StreamSeek(Stream: TBitStream;
  SeekPos: Cardinal): Boolean;
begin
  if FSampleFrequency = FourtyFourPointOne then
    result := Stream.SeekPad(SeekPos, FFrameSize - FPaddingBit, TObject(Self), FOffset)
  else
    result := Stream.Seek(SeekPos, FFrameSize);
end;

//==============================================================================
//
// THeader.TotalMS
//
//==============================================================================
function THeader.TotalMS(Stream: TBitStream): Single;
begin
  result := MaxNumberOfFrames(Stream) * MSPerFrame;
end;

end.
