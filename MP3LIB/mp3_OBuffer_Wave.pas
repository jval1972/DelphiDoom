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
 *  File:     $RCSfile: OBuffer_Wave.pas,v $
 *  Revision: $Revision: 1.1.1.1 $
 *  Version : $Id: OBuffer_Wave.pas,v 1.1.1.1 2002/04/21 12:57:22 fobmagog Exp $
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
unit mp3_OBuffer_Wave;

interface
uses
  d_delphi,
  MMSystem, mp3_Shared, mp3_OBuffer, mp3_Player;

type
  TOBuffer_Wave = class(TOBuffer)
  private
    FBufferP: array[0..MAX_CHANNELS-1] of Cardinal;
    FChannels: Cardinal;
    FDataSize: Cardinal;

    FTemp: PByteArray;

    hmmioOut: HMMIO;
    mmioinfoOut: MMIOINFO;
    ckOutRIFF: MMCKINFO;
    ckOut: MMCKINFO;

  public
    constructor Create(NumberOfChannels: Cardinal; Player: TPlayer; Filename: String);
    destructor Destroy; override;

    procedure Append(Channel: Cardinal; Value: SmallInt); override;
    procedure WriteBuffer; override;

{$IFDEF SEEK_STOP}
    procedure ClearBuffer; override;
    procedure SetStopFlag; override;
{$ENDIF}
  end;

//==============================================================================
//
// CreateWaveFileOBffer
//
//==============================================================================
function CreateWaveFileOBffer(Player: TPlayer; Filename: String): TOBuffer;

implementation

uses
  i_system,
  Math, mp3_Header;

//==============================================================================
//
// CreateWaveFileOBffer
//
//==============================================================================
function CreateWaveFileOBffer(Player: TPlayer; Filename: String): TOBuffer;
var Mode: TMode;
    WhichChannels: TChannels;
begin
  Mode := Player.Mode;
  WhichChannels := Player.Channels;
  try
    if (Mode = SingleChannel) or (WhichChannels <> both) then
      result := TOBuffer_Wave.Create(1, Player, Filename)   // mono
    else
      result := TOBuffer_Wave.Create(2, Player, Filename);  // stereo
  except
    I_Warning('CreateWaveFileOBffer() failed'#13#10);
    result := nil;
  end;
end;

{ TOBuffer_Wave }

//==============================================================================
// TOBuffer_Wave.Append
//
// Need to break up the 32-bit integer into 2 8-bit bytes.
// (ignore the first two bytes - either 0x0000 or 0xffff)
// Note that Intel byte order is backwards!!!
//
//==============================================================================
procedure TOBuffer_Wave.Append(Channel: Cardinal; Value: SmallInt);
begin
  FTemp[FBufferP[Channel]]   := (Value and $ff);
  FTemp[FBufferP[Channel]+1] := (Value shr 8);

  inc(FBufferP[Channel], FChannels shl 1);
end;

//==============================================================================
//
// TOBuffer_Wave.ClearBuffer
//
//==============================================================================
procedure TOBuffer_Wave.ClearBuffer;
begin
  // Since we write each frame, and seeks and stops occur between
  // frames, nothing is needed here.
end;

//==============================================================================
//
// TOBuffer_Wave.Create
//
//==============================================================================
constructor TOBuffer_Wave.Create(NumberOfChannels: Cardinal; Player: TPlayer; Filename: String);
var pwf: TWAVEFORMATEX;
    i: Cardinal;

begin
  FChannels := NumberOfChannels;
  FDataSize := FChannels * OBUFFERSIZE;

  if (Player.Version = MPEG2_LSF) then
    FDataSize := FDataSize shr 1;

  if (Player.Layer = 1) then
    FDataSize := FDataSize div 3;

  GetMem(FTemp, FDataSize);

  hmmioOut := mmioOpen(PChar(FileName), nil, MMIO_ALLOCBUF or MMIO_WRITE or MMIO_CREATE);
  if (hmmioOut = 0) then
    TOBuffer_Wave_Failure;

  // Create the output file RIFF chunk of form type WAVE.
  ckOutRIFF.fccType := Ord('W') or (Ord('A') shl 8) or (Ord('V') shl 16) or (Ord('E') shl 24);
  ckOutRIFF.cksize := 0;
  if (mmioCreateChunk(hmmioOut, @ckOutRIFF, MMIO_CREATERIFF) <> MMSYSERR_NOERROR) then
    TOBuffer_Wave_Failure;

  // Initialize the WAVEFORMATEX structure

  pwf.wBitsPerSample  := 16;  // No 8-bit support yet
  pwf.wFormatTag      := WAVE_FORMAT_PCM;
  pwf.nChannels       := FChannels;
  pwf.nSamplesPerSec  := Player.Frequency;
  pwf.nAvgBytesPerSec := (FChannels * Player.Frequency shl 1);
  pwf.nBlockAlign     := (FChannels shl 1);
  pwf.cbSize          := 0;

  // Create the fmt chunk
  ckOut.ckid := Ord('f') or (Ord('m') shl 8) or (Ord('t') shl 16) or (Ord(' ') shl 24);
  ckOut.cksize := sizeof(pwf);

  if (mmioCreateChunk(hmmioOut, @ckOut, 0) <> MMSYSERR_NOERROR) then
    TOBuffer_Wave_Failure;

  // Write the WAVEFORMATEX structure to the fmt chunk.

  if (mmioWrite(hmmioOut, @pwf, sizeof(pwf)) <> sizeof(pwf)) then
    TOBuffer_Wave_Failure;

  // Ascend out of the fmt chunk, back into the RIFF chunk.
  if (mmioAscend(hmmioOut, @ckOut, 0) <> MMSYSERR_NOERROR) then
    TOBuffer_Wave_Failure;

  // Create the data chunk that holds the waveform samples.
  ckOut.ckid   := Ord('d') or (Ord('a') shl 8) or (Ord('t') shl 16) or (Ord('a') shl 24);
  ckOut.cksize := 0;
  if (mmioCreateChunk(hmmioOut, @ckOut, 0) <> MMSYSERR_NOERROR) then
    TOBuffer_Wave_Failure;

  mmioGetInfo(hmmioOut, @mmioinfoOut, 0);

  for i := 0 to FChannels - 1 do
    FBufferP[i] := i * FChannels;
end;

//==============================================================================
//
// TOBuffer_Wave.Destroy
//
//==============================================================================
destructor TOBuffer_Wave.Destroy;
begin
  // Mark the current chunk as dirty and flush it
  mmioinfoOut.dwFlags := mmioinfoOut.dwFlags or MMIO_DIRTY;
  if (mmioSetInfo(hmmioOut, @mmioinfoOut, 0) <> MMSYSERR_NOERROR) then
    TOBuffer_Wave_Failure;

  // Ascend out of data chunk
  if (mmioAscend(hmmioOut, @ckOut, 0) <> MMSYSERR_NOERROR) then
    TOBuffer_Wave_Failure;

  // Ascend out of RIFF chunk
  if (mmioAscend(hmmioOut, @ckOutRIFF, 0) <> MMSYSERR_NOERROR) then
    TOBuffer_Wave_Failure;

  // Close the file
  if (mmioClose(hmmioOut, 0) <> MMSYSERR_NOERROR) then
    TOBuffer_Wave_Failure;

  // Free the buffer memory
  FreeMem(FTemp);
end;

//==============================================================================
//
// TOBuffer_Wave.SetStopFlag
//
//==============================================================================
procedure TOBuffer_Wave.SetStopFlag;
begin
end;

//==============================================================================
//
// TOBuffer_Wave.WriteBuffer
//
//==============================================================================
procedure TOBuffer_Wave.WriteBuffer;
var Write, i: Cardinal;
begin
  Write := Min(FDataSize, Cardinal(mmioinfoOut.pchEndWrite) - Cardinal(mmioinfoOut.pchNext));

  Move(FTemp^, mmioinfoOut.pchNext^, Write);
  inc(Cardinal(mmioinfoOut.pchNext), Write);

  if Write < FDataSize then
  begin
    mmioinfoOut.dwFlags := mmioinfoOut.dwFlags or MMIO_DIRTY;

    if (mmioAdvance(hmmioOut, @mmioinfoOut, MMIO_WRITE) <> MMSYSERR_NOERROR) then
      TOBuffer_Wave_Failure;
  end;

  Move(FTemp[Write], mmioinfoOut.pchNext^, FDataSize - Write);
  inc(Cardinal(mmioinfoOut.pchNext), FDataSize - Write);

  // Reset buffer pointers
  for i := 0 to FChannels - 1 do
    FBufferP[i] := i * FChannels;
end;

end.
