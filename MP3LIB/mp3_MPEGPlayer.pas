//------------------------------------------------------------------------------
//
//  DelphiDoom is a source port of the game Doom and it is
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 2004-2018 by Jim Valavanis
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

{$I Doom32.inc}

(*
 *  File:     $RCSfile: MPEGPlayer.pas,v $
 *  Revision: $Revision: 1.1.1.1 $
 *  Version : $Id: MPEGPlayer.pas,v 1.1.1.1 2002/04/21 12:57:22 fobmagog Exp $
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
unit mp3_MPEGPlayer;

interface

uses
  d_delphi,
  mp3_Player,
  mp3_OBuffer,
  mp3_Args,
  mp3_SynthFilter,
  mp3_CRC,
  mp3_Layer3,
  mp3_Shared,
  mp3_Header;

type
  TMPEGPlayer = class(TPlayer)
  private
    FArgs: TMPEGArgs;
    FFilter1: TSynthesisFilter;
    FFilter2: TSynthesisFilter;
    FCRC: TCRC16;
    FOutput: TOBuffer;
    FLayer: Cardinal;
    FLayer3: TLayerIII_Decoder;
    FDoRepeat: Boolean;
    FDoTryHardRepeat: Boolean; // JVAL
    FIsPlaying: Boolean;
    FThreadID: Cardinal;
    FThreadHandle: Cardinal;
    FStartTime: Cardinal;
    FPaused: Boolean;

    function ThreadProc: Cardinal;
    procedure DoDecode;

  protected
    function GetPosition: integer; override;
    function GetLength: Integer; override;
    function GetMode: TMode; override;
    function GetChannels: TChannels; override;
    function GetVersion: TVersion; override;
    function GetLayer: Integer; override;
    function GetFrequency: Cardinal; override;
    function GetBitrate: Integer; override;
    function GetIsPlaying: Boolean; override;
    function GetDoRepeat: Boolean; override;
    procedure SetDoRepeat(Value: Boolean); override;

  public
    property Position;
    property Length;
    property Mode;
    property Channels;
    property Version;
    property Layer;
    property Frequency;
    property Bitrate;
    property IsPlaying;
    property DoRepeat;
    property DoTryHardRepeat: boolean read FDoTryHardRepeat write FDoTryHardRepeat;

    constructor Create;
    destructor Destroy; override;

    procedure LoadStream(AStream: TDStream); override;
    procedure SetOutput(Output: TOBuffer); override;
    procedure Play; override;
    procedure Stop; override;
    procedure Pause; override;
    procedure Resume; override;
  end;

implementation

uses
  Windows,
  i_system,
  mp3_BitStream,
  mp3_SubBand,
  mp3_SubBand1,
  mp3_SubBand2;

//==============================================================================
//
// _ThreadProc
//
//==============================================================================
function _ThreadProc(Self: TMPEGPlayer): Cardinal; cdecl;
begin
  result := Self.ThreadProc;
end;

{ TMPEGPlayer }

//==============================================================================
//
// TMPEGPlayer.Create
//
//==============================================================================
constructor TMPEGPlayer.Create;
begin
  FArgs := TMPEGArgs.Create;
  FArgs.MPEGHeader := THeader.Create;
  FFilter1 := TSynthesisFilter.Create(0);
  FFilter2 := TSynthesisFilter.Create(1);
  FCRC := nil;
  FOutput := nil;
  FIsPlaying := false;
  FThreadID := 0;
  FThreadHandle := 0;
  FDoRepeat := false;
  FDoTryHardRepeat := false;
  FPaused := False;
end;

//==============================================================================
//
// TMPEGPlayer.Destroy
//
//==============================================================================
destructor TMPEGPlayer.Destroy;
begin
  Stop;

  if FCRC <> nil then
    FreeAndNil(FCRC);

  if FArgs.Stream <> nil then
    FreeAndNil(FArgs.Stream);

  if FOutput <> nil then
    FreeAndNil(FOutput);

  FreeAndNil(FLayer3);

  FreeAndNil(FFilter1);
  FreeAndNil(FFilter2);
  FreeAndNil(FArgs.MPEGHeader);
  FreeAndNil(FArgs);
end;

//==============================================================================
//
// TMPEGPlayer.DoDecode
//
//==============================================================================
procedure TMPEGPlayer.DoDecode;
var
  Mode: TMode;
  NumSubBands, i: Cardinal;
  SubBands: array[0..31] of TSubBand;
  ReadReady, WriteReady: Boolean;
begin
  // is there a change in important parameters?
  // (bitrate switching is allowed)
  if FArgs.MPEGHeader.Layer <> FLayer then
  begin
    // layer switching is allowed
    if FArgs.MPEGHeader.Layer = 3 then
    begin
      FreeAndNil(FLayer3);
      FLayer3 := TLayerIII_Decoder.Create(FArgs.Stream, FArgs.MPEGHeader, FFilter1, FFilter2, FOutput, FArgs.WhichC)
    end
    else if FLayer = 3 then
      FreeAndNil(FLayer3);

    FLayer := FArgs.MPEGHeader.Layer;
  end;

  if FLayer <> 3 then
  begin
    NumSubBands := FArgs.MPEGHeader.NumberOfSubbands;
    Mode := FArgs.MPEGHeader.Mode;

    // create subband objects:
    if FLayer = 1 then
    begin
      if Mode = SingleChannel then
        for i := 0 to NumSubBands - 1 do
          SubBands[i] := TSubbandLayer1.Create(i)
      else if Mode = JointStereo then
      begin
        for i := 0 to FArgs.MPEGHeader.IntensityStereoBound - 1 do
          SubBands[i] := TSubbandLayer1Stereo.Create(i);

        i := FArgs.MPEGHeader.IntensityStereoBound;
        while (Cardinal(i) < NumSubBands) do
        begin
          SubBands[i] := TSubbandLayer1IntensityStereo.Create(i);
          inc(i);
        end;
      end
      else
      begin
        for i := 0 to NumSubBands - 1 do
          SubBands[i] := TSubbandLayer1Stereo.Create(i);
      end;
    end
    else
    begin  // Layer II
      if Mode = SingleChannel then
        for i := 0 to NumSubBands - 1 do
          SubBands[i] := TSubbandLayer2.Create(i)
      else if Mode = JointStereo then
      begin
        for i := 0 to FArgs.MPEGHeader.IntensityStereoBound - 1 do
          SubBands[i] := TSubbandLayer2Stereo.Create(i);

        i := FArgs.MPEGHeader.IntensityStereoBound;
        while Cardinal(i) < NumSubBands do
        begin
          SubBands[i] := TSubbandLayer2IntensityStereo.Create(i);
          inc(i);
        end;
      end
      else
      begin
        for i := 0 to NumSubBands - 1 do
          SubBands[i] := TSubbandLayer2Stereo.Create(i);
      end;
    end;

    // start to read audio data:
    for i := 0 to NumSubBands - 1 do
      SubBands[i].ReadAllocation(FArgs.Stream, FArgs.MPEGHeader, FCRC);

    if FLayer = 2 then
      for i := 0 to NumSubBands - 1 do
        TSubBandLayer2(SubBands[i]).ReadScaleFactorSelection(FArgs.Stream, FCRC);

    if (FCRC = nil) or (FArgs.MPEGHeader.ChecksumOK) then
    begin
      // no checksums or checksum ok, continue reading from stream:
      for i := 0 to NumSubBands - 1 do
        SubBands[i].ReadScaleFactor(FArgs.Stream, FArgs.MPEGHeader);

      repeat
        ReadReady := true;
        for i := 0 to NumSubBands - 1 do
          ReadReady := SubBands[i].ReadSampleData(FArgs.Stream);

        repeat
          WriteReady := true;
          for i := 0 to NumSubBands - 1 do
            WriteReady := SubBands[i].PutNextSample(FArgs.WhichC, FFilter1, FFilter2);

          FFilter1.CalculatePCMSamples(FOutput);
          if (FArgs.WhichC = Both) and (Mode <> SingleChannel) then
            FFilter2.CalculatePCMSamples(FOutput);
        until WriteReady;
      until ReadReady;

      FOutput.WriteBuffer;
    end;

    for i := 0 to NumSubBands - 1 do
      FreeAndNil(SubBands[i]);
  end
  else  // Layer III
    FLayer3.Decode;
end;

//==============================================================================
//
// TMPEGPlayer.GetBitrate
//
//==============================================================================
function TMPEGPlayer.GetBitrate: Integer;
begin
  result := FArgs.MPEGHeader.Bitrate;
end;

//==============================================================================
//
// TMPEGPlayer.GetChannels
//
//==============================================================================
function TMPEGPlayer.GetChannels: TChannels;
begin
  result := FArgs.WhichC;
end;

//==============================================================================
//
// TMPEGPlayer.GetDoRepeat
//
//==============================================================================
function TMPEGPlayer.GetDoRepeat: Boolean;
begin
  result := FDoRepeat;
end;

//==============================================================================
//
// TMPEGPlayer.GetFrequency
//
//==============================================================================
function TMPEGPlayer.GetFrequency: Cardinal;
begin
  result := FArgs.MPEGHeader.Frequency;
end;

//==============================================================================
//
// TMPEGPlayer.GetIsPlaying
//
//==============================================================================
function TMPEGPlayer.GetIsPlaying: Boolean;
begin
  result := FIsPlaying;
end;

//==============================================================================
//
// TMPEGPlayer.GetLayer
//
//==============================================================================
function TMPEGPlayer.GetLayer: Integer;
begin
  result := FArgs.MPEGHeader.Layer;
end;

//==============================================================================
//
// TMPEGPlayer.GetLength
//
//==============================================================================
function TMPEGPlayer.GetLength: Integer;
begin
  result := Round(FArgs.MPEGHeader.TotalMS(FArgs.Stream) / 1000);
end;

//==============================================================================
//
// TMPEGPlayer.GetMode
//
//==============================================================================
function TMPEGPlayer.GetMode: TMode;
begin
  result := FArgs.MPEGHeader.Mode;
end;

//==============================================================================
//
// TMPEGPlayer.GetPosition
//
//==============================================================================
function TMPEGPlayer.GetPosition: Integer;
begin
  if FThreadHandle = 0 then
    result := 0
  else
    result := GetTickCount - FStartTime;
end;

//==============================================================================
//
// TMPEGPlayer.GetVersion
//
//==============================================================================
function TMPEGPlayer.GetVersion: TVersion;
begin
  result := FArgs.MPEGHeader.Version;
end;

//==============================================================================
//
// TMPEGPlayer.LoadStream
//
//==============================================================================
procedure TMPEGPlayer.LoadStream(AStream: TDStream);
begin
  if FCRC <> nil then
    FreeAndNil(FCRC);

  if FArgs.Stream <> nil then
    FreeAndNil(FArgs.Stream);

  FArgs.Stream := TBitStream.Create(AStream);
  FArgs.WhichC := Both;
  FArgs.MPEGHeader.ReadHeader(FArgs.Stream, FCRC);
end;

//==============================================================================
//
// TMPEGPlayer.Play
//
//==============================================================================
procedure TMPEGPlayer.Play;
begin
  // Start the thread.
  FIsPlaying := true;
  FThreadHandle := CreateThread(nil, 0, @_ThreadProc, Self, 0, FThreadID);
  if FThreadHandle = 0 then
    I_Error('TMPEGPlayer.Play(): Thread creation failed.');
  FStartTime := GetTickCount;
end;

//==============================================================================
//
// TMPEGPlayer.SetDoRepeat
//
//==============================================================================
procedure TMPEGPlayer.SetDoRepeat(Value: Boolean);
begin
  FDoRepeat := Value;
end;

//==============================================================================
//
// TMPEGPlayer.SetOutput
//
//==============================================================================
procedure TMPEGPlayer.SetOutput(Output: TOBuffer);
begin
  FOutput := Output;
end;

//==============================================================================
//
// TMPEGPlayer.Stop
//
//==============================================================================
procedure TMPEGPlayer.Stop;
begin
  if FThreadHandle <> 0 then
  begin
    FIsPlaying := false;
    WaitForSingleObject(FThreadHandle, INFINITE);
    CloseHandle(FThreadHandle);
    FThreadHandle := 0;
  end;
end;

//==============================================================================
//
// TMPEGPlayer.ThreadProc
//
//==============================================================================
function TMPEGPlayer.ThreadProc: Cardinal;
var
  FrameRead: Boolean;
  Curr, Total: Cardinal;
begin
  FrameRead := true;
  while FrameRead and FIsPlaying do
  begin
    DoDecode;
    FrameRead := FArgs.MPEGHeader.ReadHeader(FArgs.Stream, FCRC);
    Sleep(0);

    Curr := FArgs.Stream.CurrentFrame;
    Total := FArgs.MPEGHeader.MaxNumberOfFrames(FArgs.Stream);

    if FDoRepeat then
    begin
      if ((not FrameRead) and (Curr + 20 >= Total)) or (Curr >= Total) then
      begin
        FArgs.Stream.Restart;
        if Assigned(FCRC) then
          FreeAndNil(FCRC);

        FrameRead := FArgs.MPEGHeader.ReadHeader(FArgs.Stream, FCRC);
      end;

      if not FrameRead then
        if FDoTryHardRepeat then
        begin
          FArgs.Stream.Restart;
          if (Assigned(FCRC)) then
            FreeAndNil(FCRC);
          FrameRead := FArgs.MPEGHeader.ReadHeader(FArgs.Stream, FCRC);
        end;

      if GetTickCount >= FStartTime + Round(FArgs.MPEGHeader.TotalMS(FArgs.Stream)) then
        FStartTime := GetTickCount;
    end;

    while FPaused do
    begin
      if not FIsPlaying then
        Break;
      Sleep(1);
    end;
  end;
  FIsPlaying := false;
  result := 0;
end;

//==============================================================================
//
// TMPEGPlayer.Pause
//
//==============================================================================
procedure TMPEGPlayer.Pause;
begin
  FPaused := True;
end;

//==============================================================================
//
// TMPEGPlayer.Resume
//
//==============================================================================
procedure TMPEGPlayer.Resume;
begin
  FPaused := False;
end;

end.

