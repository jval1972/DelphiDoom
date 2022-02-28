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
 *  File:     $RCSfile: OBuffer_MCI.pas,v $
 *  Revision: $Revision: 1.1.1.1 $
 *  Version : $Id: OBuffer_MCI.pas,v 1.1.1.1 2002/04/21 12:57:22 fobmagog Exp $
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
unit mp3_OBuffer_MCI;

interface

uses
  MMSystem, mp3_Shared, mp3_OBuffer, mp3_Player;

const
  TWO_TIMES  = 5;
  BUFFERSIZE = OBUFFERSIZE shl TWO_TIMES;
  BIT_SELECT = $1f;
  SLEEPTIME  = 20;

type
  TOBuffer_MCI = class(TOBuffer)
  private
    FBuffer: array[0..MAX_CHANNELS-1] of Cardinal;
    FChannels: Cardinal;
    FWF: PWaveFormatEx;
    FWaveHdrArr: PWaveHdr;
    FHWO: HWAVEOUT;
    FBufferCount: Cardinal;
    FHdrSize: Cardinal;
    FFillup: Cardinal;
    FDataSize: Cardinal;
    FUserStop: Cardinal;

    procedure WaveSwap;

  public
    constructor Create(NumberOfChannels: Cardinal; Player: TPlayer);
    destructor Destroy; override;

    procedure Append(Channel: Cardinal; Value: SmallInt); override;
    procedure WriteBuffer; override;
    procedure ClearBuffer; override;
    procedure SetStopFlag; override;
  end;

//==============================================================================
//
// CreateMCIOBffer
//
//==============================================================================
function CreateMCIOBffer(Player: TPlayer): TOBuffer;

implementation

uses
  i_system,
  mp3_Header;

//==============================================================================
//
// CreateMCIOBffer
//
//==============================================================================
function CreateMCIOBffer(Player: TPlayer): TOBuffer;
var Mode: TMode;
    WhichChannels: TChannels;
begin
  Mode := Player.Mode;
  WhichChannels := Player.Channels;
  try
    if (Mode = SingleChannel) or (WhichChannels <> Both) then
      result := TOBuffer_MCI.Create(1, Player)   // mono
    else
      result := TOBuffer_MCI.Create(2, Player);  // stereo
  except
    result := nil;
  end;
end;

{ TOBuffer_MCI }

//==============================================================================
// TOBuffer_MCI.Append
//
// Need to break up the 32-bit integer into 2 8-bit bytes.
// (ignore the first two bytes - either 0x0000 or 0xffff)
// Note that Intel byte order is backwards!!!
//
//==============================================================================
procedure TOBuffer_MCI.Append(Channel: Cardinal; Value: SmallInt);
var Temp: PChar;
begin
  temp := PWAVEHDR(PPointerArray(FWaveHdrArr)[2]).lpData;
  temp[FBuffer[channel]]   := chr(Value and $ff);
  temp[FBuffer[channel]+1] := chr(Value shr 8);

  FBuffer[channel] := FBuffer[channel] + (FChannels shl 1);
end;

//==============================================================================
// TOBuffer_MCI.ClearBuffer
//
// Clear all the data in the buffers
//
//==============================================================================
procedure TOBuffer_MCI.ClearBuffer;
var i, j: Cardinal;
    temp: PWaveHdr;
begin
  waveOutReset(FHWO);

  for i := 0 to 2 do
  begin
    temp := PPointerArray(FWaveHdrArr)[i];

    if (temp.dwUser <> 0) then
      waveOutUnprepareHeader(FHWO, temp, FHdrSize);

    temp.dwUser := 0;

    for j := 0 to FDataSize - 1 do
      temp.lpData[j] := #0;
  end;

  // Reset buffer pointers
  for i := 0 to FChannels - 1 do
    FBuffer[i] := i * FChannels;

  // Force the buffers to fillup before playing.
  FFillup := 0;
  FBufferCount := 0;
end;

//==============================================================================
//
// TOBuffer_MCI.Create
//
//==============================================================================
constructor TOBuffer_MCI.Create(NumberOfChannels: Cardinal;
  Player: TPlayer);
var i: Cardinal;
    temp: PWaveHdr;
begin
  FChannels := NumberOfChannels;
  FDataSize := FChannels * BUFFERSIZE;

  if (Player.Version = MPEG2_LSF) then
    FDataSize := FDataSize shr 1;

  if (Player.Layer = 1) then
    FDataSize := FDataSize div 3;

  FHdrSize := sizeof(TWAVEHDR);
  FFillup := 0;

  GetMem(FWF, Sizeof(TWAVEFORMATEX));

  FWF.wBitsPerSample  := 16;  // No 8-bit support yet
  FWF.wFormatTag      := WAVE_FORMAT_PCM;
  FWF.nChannels       := FChannels;
  FWF.nSamplesPerSec  := Player.Frequency;
  FWF.nAvgBytesPerSec := FChannels * Player.Frequency shl 1;
  FWF.nBlockAlign     := (FChannels shl 1);
  FWF.cbSize          := 0;

  if (waveOutOpen(@FHWO, WAVE_MAPPER, FWF, 0, 0, WAVE_ALLOWSYNC) <> MMSYSERR_NOERROR) then
  begin
    FreeMem(FWF);
    TOBuffer_Wave_Failure;
  end;

  FBufferCount := 0;

  GetMem(FWaveHdrArr, 3 * sizeof(PWAVEHDR));
  for i := 0 to 2 do
  begin
    GetMem(PPointerArray(FWaveHdrArr)[i], Sizeof(WAVEHDR));
    temp := PPointerArray(FWaveHdrArr)[i];

    if (temp = nil) then
      exit;

    GetMem(temp.lpData, FDataSize);

    if (temp.lpData = nil) then
      exit;

    temp.dwBufferLength  := FDataSize;
    temp.dwBytesRecorded := 0;
    temp.dwUser          := 0;  // If played, dwUser = 1
    temp.dwLoops         := 0;
    temp.dwFlags         := 0;
  end;

  for i := 0 to FChannels - 1 do
    FBuffer[i] := i * FChannels;

  FUserStop := 0;
end;

//==============================================================================
//
// TOBuffer_MCI.Destroy
//
//==============================================================================
destructor TOBuffer_MCI.Destroy;
var
  i, j: integer;
begin
   if (FUserStop <> 0) then
     waveOutReset(FHWO)
   else
   begin
     if (FWaveHdrArr <> nil) then
     begin
       if (FFillup = 1) then
       begin
         // Write the last header calculated (at the top of the array).
         waveOutPrepareHeader(FHWO, PPointerArray(FWaveHdrArr)[0], FHdrSize);
         waveOutWrite(FHWO, PPointerArray(FWaveHdrArr)[0], FHdrSize);

         // Header has been written.
         PWaveHdr(PPointerArray(FWaveHdrArr)[0]).dwUser := 1;
       end;

       if (FBufferCount <> 0) then
       begin
         // Write the last wave header (probably not be written due to buffer
         // size increase.)

         for i := FBuffer[FChannels - 1] to FDataSize - 1 do
           PWaveHdr(PPointerArray(FWaveHdrArr)[2]).lpData[i] := #0;

         waveOutPrepareHeader(FHWO, PPointerArray(FWaveHdrArr)[2], FHdrSize);
         waveOutWrite(FHWO, PPointerArray(FWaveHdrArr)[2], FHdrSize);

         // Header has been written.
         PWaveHdr(PPointerArray(FWaveHdrArr)[2]).dwUser := 1;
         WaveSwap;
       end;
     end;
   end;

   // Unprepare and free the header memory.
   if (FWaveHdrArr <> nil) then
   begin
     for j := 2 downto 0 do
     begin
       if (PWaveHdr(PPointerArray(FWaveHdrArr)[j]).dwUser <> 0) and (FUserStop = 0) then
         while (waveOutUnprepareHeader(FHWO, PPointerArray(FWaveHdrArr)[j], FHdrSize) = WAVERR_STILLPLAYING) do
           I_Sleep(SLEEPTIME);

       FreeMem(PWaveHdr(PPointerArray(FWaveHdrArr)[j]).lpData);
       FreeMem(PPointerArray(FWaveHdrArr)[j]);
     end;
     FreeMem(FWaveHdrArr);
   end;

   if (FWF <> nil) then
     FreeMem(FWF);

   while (waveOutClose(FHWO) = WAVERR_STILLPLAYING) do;
   I_Sleep(SLEEPTIME);
end;

//==============================================================================
// TOBuffer_MCI.SetStopFlag
//
// Set the flag to avoid unpreparing non-existent headers
//
//==============================================================================
procedure TOBuffer_MCI.SetStopFlag;
begin
  FUserStop := 1;
end;

//==============================================================================
//
// TOBuffer_MCI.WaveSwap
//
//==============================================================================
procedure TOBuffer_MCI.WaveSwap;
var temp: Pointer;
begin
  temp := PPointerArray(FWaveHdrArr)[2];
  PPointerArray(FWaveHdrArr)[2] := PPointerArray(FWaveHdrArr)[1];
  PPointerArray(FWaveHdrArr)[1] := PPointerArray(FWaveHdrArr)[0];
  PPointerArray(FWaveHdrArr)[0] := temp;
end;

//==============================================================================
// TOBuffer_MCI.WriteBuffer
//
// Actually write only when buffer is actually full.
//
//==============================================================================
procedure TOBuffer_MCI.WriteBuffer;
var
  i: Cardinal;
begin
  inc(FBufferCount);
  if FBufferCount and BIT_SELECT = 0 then
  begin
    FBufferCount := 0;

    // Wait for 2 completed headers
    if FFillup > 1 then
    begin
      // Prepare & write newest header
      waveOutPrepareHeader(FHWO, PPointerArray(FWaveHdrArr)[2], FHdrSize);
      waveOutWrite(FHWO, PPointerArray(FWaveHdrArr)[2], FHdrSize);

      // Header has now been sent
      PWaveHdr(PPointerArray(FWaveHdrArr)[2]).dwUser := 1;

      WaveSwap;

      // Unprepare oldest header
      if (PWaveHdr(PPointerArray(FWaveHdrArr)[2]).dwUser <> 0) then
      begin
        while (waveOutUnprepareHeader(FHWO, PPointerArray(FWaveHdrArr)[2], FHdrSize) = WAVERR_STILLPLAYING) do
          I_Sleep(SLEEPTIME);
      end;
    end
    else
    begin
      inc(FFillup);
      if (FFillup = 2) then
      begin
        // Write the previously calculated 2 headers
        waveOutPrepareHeader(FHWO, PPointerArray(FWaveHdrArr)[0], FHdrSize);
        waveOutWrite(FHWO, PPointerArray(FWaveHdrArr)[0], FHdrSize);

        // Header has now been sent
        PWaveHdr(PPointerArray(FWaveHdrArr)[0]).dwUser := 1;

        WaveSwap;

        waveOutPrepareHeader(FHWO, PPointerArray(FWaveHdrArr)[0], FHdrSize);
        waveOutWrite(FHWO, PPointerArray(FWaveHdrArr)[0], FHdrSize);

        // Header has now been sent
        PWaveHdr(PPointerArray(FWaveHdrArr)[0]).dwUser := 1;
      end
      else
        WaveSwap;
    end;

    for i := 0 to FChannels - 1 do
      FBuffer[i] := i * FChannels;
  end;
end;

end.
