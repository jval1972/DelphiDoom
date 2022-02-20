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
 *  File:     $RCSfile: SubBand1.pas,v $
 *  Revision: $Revision: 1.1.1.1 $
 *  Version : $Id: SubBand1.pas,v 1.1.1.1 2002/04/21 12:57:23 fobmagog Exp $
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
unit mp3_SubBand1;

interface
uses
  Windows, mp3_Shared, mp3_BitStream, mp3_Header, mp3_SynthFilter, mp3_SubBand, mp3_CRC;

type
  // class for layer I subbands in single channel mode:
  TSubBandLayer1 = class(TSubBand)
  protected
    FSubBandNumber: Cardinal;
    FSampleNumber: Cardinal;
    FAllocation: Cardinal;
    FScaleFactor: Single;
    FSampleLength: Cardinal;
    FSample: Single;
    FFactor, FOffset: Single;

  public
    constructor Create(SubBandNumber: Cardinal); virtual;

    procedure ReadAllocation(Stream: TBitStream; Header: THeader; CRC: TCRC16); override;
    procedure ReadScaleFactor(Stream: TBitStream; Header: THeader); override;
    function ReadSampleData(Stream: TBitStream): Boolean; override;
    function PutNextSample(Channels: TChannels; Filter1, Filter2: TSynthesisFilter): Boolean; override;
  end;

  // class for layer I subbands in joint stereo mode:
  TSubBandLayer1IntensityStereo = class(TSubBandLayer1)
  protected
    FChannel2ScaleFactor: Single;

  public
    procedure ReadScaleFactor(Stream: TBitStream; Header: THeader); override;
    function PutNextSample(Channels: TChannels; Filter1, Filter2: TSynthesisFilter): Boolean; override;
  end;

  // class for layer I subbands in stereo mode:
  TSubBandLayer1Stereo = class(TSubBandLayer1)
  protected
    FChannel2Allocation: Cardinal;
    FChannel2ScaleFactor: Single;
    FChannel2SampleLength: Cardinal;
    FChannel2Sample: Single;
    FChannel2Factor: Single;
    FChannel2Offset: Single;

  public
    procedure ReadAllocation(Stream: TBitStream; Header: THeader; CRC: TCRC16); override;
    procedure ReadScaleFactor(Stream: TBitStream; Header: THeader); override;
    function ReadSampleData(Stream: TBitStream): Boolean; override;
    function PutNextSample(Channels: TChannels; Filter1, Filter2: TSynthesisFilter): Boolean; override;
  end;

implementation
uses
  mp3_ScaleFac;

const
  // factors and offsets for sample requantization:
  TableFactor: array[0..14] of Single = (
    0.0, (1.0 / 2.0) * (4.0 / 3.0), (1.0 / 4.0) * (8.0 / 7.0), (1.0 / 8.0) * (16.0 / 15.0),
    (1.0 / 16.0) * (32.0 / 31.0), (1.0 / 32.0) * (64.0/63.0), (1.0/64.0) * (128.0/127.0),
    (1.0 / 128.0) * (256.0 / 255.0), (1.0 / 256.0) * (512.0 / 511.0),
    (1.0 / 512.0) * (1024.0 / 1023.0), (1.0 / 1024.0) * (2048.0 / 2047.0),
    (1.0 / 2048.0) * (4096.0 / 4095.0), (1.0 / 4096.0) * (8192.0 / 8191.0),
    (1.0 / 8192.0) * (16384.0 / 16383.0), (1.0 / 16384.0) * (32768.0 / 32767.0));

  TableOffset: array[0..14] of Single = (
    0.0, ((1.0 / 2.0) - 1.0) * (4.0 / 3.0), ((1.0 / 4.0) - 1.0) * (8.0 / 7.0), ((1.0 / 8.0) - 1.0) * (16.0 / 15.0),
    ((1.0 / 16.0) - 1.0) * (32.0 / 31.0), ((1.0 / 32.0) - 1.0) * (64.0 / 63.0), ((1.0 / 64.0) - 1.0) * (128.0 / 127.0),
    ((1.0 / 128.0) - 1.0) * (256.0 / 255.0), ((1.0 / 256.0) - 1.0) * (512.0 / 511.0),
    ((1.0 / 512.0) - 1.0) * (1024.0 / 1023.0), ((1.0 / 1024.0) - 1.0) * (2048.0 / 2047.0),
    ((1.0 / 2048.0) - 1.0) * (4096.0 / 4095.0), ((1.0 / 4096.0) - 1.0) * (8192.0 / 8191.0),
    ((1.0 / 8192.0) - 1.0) * (16384.0 / 16383.0), ((1.0 / 16384.0) - 1.0) * (32768.0 / 32767.0));

{ TSubBandLayer1 }

//==============================================================================
//
// TSubBandLayer1.Create
//
//==============================================================================
constructor TSubBandLayer1.Create(SubBandNumber: Cardinal);
begin
  FSubBandNumber := SubBandNumber;
  FSampleNumber := 0;
end;

//==============================================================================
//
// TSubBandLayer1.PutNextSample
//
//==============================================================================
function TSubBandLayer1.PutNextSample(Channels: TChannels; Filter1,
  Filter2: TSynthesisFilter): Boolean;
var ScaledSample: Single;
begin
  if (FAllocation <> 0) and (Channels <> Right) then
  begin
    ScaledSample := (FSample * FFactor + FOffset) * FScalefactor;
    Filter1.InputSample(ScaledSample, FSubBandNumber);
  end;

  result := true;
end;

//==============================================================================
//
// TSubBandLayer1.ReadAllocation
//
//==============================================================================
procedure TSubBandLayer1.ReadAllocation(Stream: TBitStream;
  Header: THeader; CRC: TCRC16);
begin
  FAllocation := Stream.GetBits(4);
//  if FAllocation = 15 then ;
//  cerr << "WARNING: stream contains an illegal allocation!\n";  // MPEG-stream is corrupted!

  if CRC <> nil then
    CRC.AddBits(FAllocation, 4);

  if (FAllocation <> 0) then
  begin
    FSampleLength := FAllocation + 1;
    FFactor := TableFactor[FAllocation];
    FOffset := TableOffset[FAllocation];
  end;
end;

//==============================================================================
//
// TSubBandLayer1.ReadSampleData
//
//==============================================================================
function TSubBandLayer1.ReadSampleData(Stream: TBitStream): Boolean;
begin
  if FAllocation <> 0 then
    FSample := Stream.GetBitsFloat(FSampleLength);

  inc(FSampleNumber);
  if FSampleNumber = 12 then
  begin
    FSampleNumber := 0;
    result := true;
  end
  else
    result := false;
end;

//==============================================================================
//
// TSubBandLayer1.ReadScaleFactor
//
//==============================================================================
procedure TSubBandLayer1.ReadScaleFactor(Stream: TBitStream;
  Header: THeader);
begin
  if (FAllocation <> 0) then
    FScalefactor := ScaleFactors[Stream.GetBits(6)];
end;

{ TSubBandLayer1IntensityStereo }

//==============================================================================
//
// TSubBandLayer1IntensityStereo.PutNextSample
//
//==============================================================================
function TSubBandLayer1IntensityStereo.PutNextSample(Channels: TChannels;
  Filter1, Filter2: TSynthesisFilter): Boolean;
var Sample1, Sample2: Single;
begin
  if (FAllocation <> 0) then
  begin
    FSample := FSample * FFactor + FOffset;  // requantization
    if Channels = Both then
    begin
      Sample1 := FSample * FScalefactor;
      Sample2 := FSample * FChannel2ScaleFactor;
      Filter1.InputSample(Sample1, FSubBandNumber);
      Filter2.InputSample(Sample2, FSubBandNumber);
    end
    else if Channels = Left then
    begin
      Sample1 := FSample * FScaleFactor;
      Filter1.InputSample(Sample1, FSubBandNumber);
    end
    else
    begin
      Sample2 := FSample * FChannel2ScaleFactor;
      Filter2.InputSample(Sample2, FSubBandNumber);
    end;
  end;

  result := true;
end;

//==============================================================================
//
// TSubBandLayer1IntensityStereo.ReadScaleFactor
//
//==============================================================================
procedure TSubBandLayer1IntensityStereo.ReadScaleFactor(Stream: TBitStream;
  Header: THeader);
begin
  if FAllocation <> 0 then
  begin
    FScaleFactor := ScaleFactors[Stream.GetBits(6)];
    FChannel2ScaleFactor := ScaleFactors[Stream.GetBits(6)];
  end;
end;

{ TSubBandLayer1Stereo }

//==============================================================================
//
// TSubBandLayer1Stereo.PutNextSample
//
//==============================================================================
function TSubBandLayer1Stereo.PutNextSample(Channels: TChannels; Filter1,
  Filter2: TSynthesisFilter): Boolean;
var
  Sample2: Single;
begin
  inherited PutNextSample(Channels, Filter1, Filter2);
  if (FChannel2Allocation <> 0) and (Channels <> Left) then
  begin
    Sample2 := (FChannel2Sample * FChannel2Factor + FChannel2Offset) * FChannel2ScaleFactor;
    if Channels = Both then
      Filter2.InputSample(Sample2, FSubBandNumber)
    else
      Filter1.InputSample(Sample2, FSubBandNumber);
  end;

  result := true;
end;

//==============================================================================
//
// TSubBandLayer1Stereo.ReadAllocation
//
//==============================================================================
procedure TSubBandLayer1Stereo.ReadAllocation(Stream: TBitStream;
  Header: THeader; CRC: TCRC16);
begin
  FAllocation := Stream.GetBits(4);
  FChannel2Allocation := Stream.GetBits(4);
  if CRC <> nil then
  begin
    CRC.AddBits(FAllocation, 4);
    CRC.AddBits(FChannel2Allocation, 4);
  end;

  if FAllocation <> 0 then
  begin
    FSamplelength := FAllocation + 1;
    FFactor := TableFactor[FAllocation];
    FOffset := TableOffset[FAllocation];
  end;

  if FChannel2Allocation <> 0 then
  begin
    FChannel2SampleLength := FChannel2Allocation + 1;
    FChannel2Factor := TableFactor[FChannel2Allocation];
    FChannel2Offset := TableOffset[FChannel2Allocation];
  end;
end;

//==============================================================================
//
// TSubBandLayer1Stereo.ReadSampleData
//
//==============================================================================
function TSubBandLayer1Stereo.ReadSampleData(Stream: TBitStream): Boolean;
begin
  result := inherited ReadSampleData(Stream);

  if FChannel2Allocation <> 0 then
    FChannel2Sample := Stream.GetBitsFloat(FChannel2SampleLength);
end;

//==============================================================================
//
// TSubBandLayer1Stereo.ReadScaleFactor
//
//==============================================================================
procedure TSubBandLayer1Stereo.ReadScaleFactor(Stream: TBitStream;
  Header: THeader);
begin
  if FAllocation <> 0 then
    FScaleFactor := ScaleFactors[Stream.GetBits(6)];

  if FChannel2Allocation <> 0 then
    FChannel2ScaleFactor := ScaleFactors[Stream.GetBits(6)];
end;

end.
