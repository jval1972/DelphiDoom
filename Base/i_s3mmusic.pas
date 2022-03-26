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
//  S3M music file playback
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit i_s3mmusic;

interface

//==============================================================================
//
// I_PlayS3M
//
//==============================================================================
procedure I_PlayS3M(const data: pointer; const size: integer);

//==============================================================================
//
// I_PauseS3M
//
//==============================================================================
procedure I_PauseS3M;

//==============================================================================
//
// I_ResumeS3M
//
//==============================================================================
procedure I_ResumeS3M;

//==============================================================================
//
// I_StopS3M
//
//==============================================================================
procedure I_StopS3M;

//==============================================================================
//
// I_InitS3M
//
//==============================================================================
procedure I_InitS3M;

//==============================================================================
//
// I_ShutDownS3M
//
//==============================================================================
procedure I_ShutDownS3M;

//==============================================================================
//
// I_SetMusicVolumeS3M
//
//==============================================================================
procedure I_SetMusicVolumeS3M(volume: integer);

//==============================================================================
//
// I_ProcessS3M
//
//==============================================================================
procedure I_ProcessS3M;

//==============================================================================
//
// IsS3MMusicFile
//
//==============================================================================
function IsS3MMusicFile(const buf: pointer; const size: integer): boolean;

implementation

uses
  MMSystem,
  Windows,
  libs3m,
  d_delphi,
  i_system,
  i_threads;

const
  SAMPLING_FREQ = 48000; { 48khz. }
  NUM_CHANNELS = 2;     { Stereo. }
  BUFFER_SAMPLES = 16384; { 64k per buffer. }
  NUM_BUFFERS: LongInt = 8;     { 8 buffers. }
  EXIT_FAILURE: integer = 1;

//==============================================================================
//
// CheckMMError
//
//==============================================================================
procedure CheckMMError(ReturnCode: MMRESULT);
var
  ErrorText: array[0..63] of char;
begin
  if ReturnCode <> MMSYSERR_NOERROR then
  begin
    WaveOutGetErrorText(ReturnCode, @ErrorText, Length(ErrorText));
    I_Error('CheckMMError(): ' + string(ErrorText));
  end;
end;

const
  S3M_MSG_NONE = 0;
  S3M_MSG_PLAY = 1;
  S3M_MSG_PAUSE = 2;
  S3M_MSG_RESUME = 3;
  S3M_MSG_STOP = 4;
  S3M_MSG_DESTROY = 5;

  MOD_STATUS_IDLE = 0;
  MOD_STATUS_PLAYING = 1;

var
  s3m_msg: integer = 0;
  s3m_datapointer: pointer;
  s3m_datasize: integer;
  s3m_thread: TDThread;
  s3m_status: integer = 0;

var
  Semaphore: THandle;

//==============================================================================
//
// WaveOutProc
//
//==============================================================================
procedure WaveOutProc(hWaveOut: HWAVEOUT; uMsg: UINT;
  dwInstance, dwParam1, dwParam2: DWORD); stdcall;
begin
  if uMsg = WOM_DONE then
    ReleaseSemaphore(Semaphore, 1, nil);
end;

var
  s3module: s3m_t;

//==============================================================================
//
// LoadS3M
//
//==============================================================================
procedure LoadS3M;
var
  ret: integer;
begin
  ismultithread := true;

  while s3m_datapointer = nil do
    I_Sleep(10);

  if s3m_datasize < 1084 then
    I_Error('LoadS3M(): Unable to read module header');

  _s3m_initialize(@s3module, SAMPLING_FREQ);

  ret := _s3m_from_ram(@s3module, s3m_datapointer, s3m_datasize);
  if ret <> 0 then
    I_Error('LoadS3M(): Unable to load S3M data, error code =%d', [ret]);

  ismultithread := false;
end;

var
  s3mvolume: integer = $FFFF;

var
  s3mbuffer: packed array[0..BUFFER_SAMPLES * NUM_CHANNELS - 1] of smallint;

//==============================================================================
//
// PlayS3M
//
//==============================================================================
procedure PlayS3M;
var
  WaveFormat: TWaveFormatEx;
  WaveOutHandle: HWaveOut;
  WaveHeaders: array of TWaveHdr;
  WaveBuffers: array of array of smallint;
  PWaveHeader: PWaveHdr;
  Idx, Err, CurrentBuffer: longint;
  i: integer;
begin
  ismultithread := true;
  { Initialise Wave Format Structure. }
  WaveFormat.wFormatTag := WAVE_FORMAT_PCM;
  WaveFormat.nChannels := NUM_CHANNELS;
  WaveFormat.nSamplesPerSec := SAMPLING_FREQ;
  WaveFormat.nAvgBytesPerSec := SAMPLING_FREQ * NUM_CHANNELS * 2;
  WaveFormat.nBlockAlign := NUM_CHANNELS * 2;
  WaveFormat.wBitsPerSample := 16;
  _s3m_play(@s3module);

  { Initialise Waveform Buffers. }
  SetLength(WaveBuffers, NUM_BUFFERS, BUFFER_SAMPLES * NUM_CHANNELS);
  SetLength(WaveHeaders, NUM_BUFFERS);
  for Idx := 0 to NUM_BUFFERS - 1 do
  begin
    FillChar(WaveHeaders[Idx], SizeOf(TWaveHdr), 0);
    WaveHeaders[Idx].lpData := @WaveBuffers[Idx][0];
    WaveHeaders[Idx].dwBufferLength := BUFFER_SAMPLES * NUM_CHANNELS * 2;
  end;

  { Initialise Semaphore. }
  Semaphore := CreateSemaphore(nil, NUM_BUFFERS, NUM_BUFFERS, '');

  { Open Audio Device. }
  Err := WaveOutOpen(@WaveOutHandle, WAVE_MAPPER, @WaveFormat, DWORD(
    @WaveOutProc), 0, CALLBACK_FUNCTION);
  CheckMMError(Err);

  ismultithread := false;
  { Play Through Once. }
  CurrentBuffer := 0;
  while true do
  begin
    { Wait for a buffer to become available. }
    WaitForSingleObject(Semaphore, INFINITE);

    while s3m_msg = S3M_MSG_PAUSE do
      I_Sleep(10);

    if s3m_msg = S3M_MSG_STOP then
      break;

    if s3module.rt.playing = 0 then
      _s3m_play(@s3module);

    _s3m_sound_callback(nil, @s3mbuffer, 2 * BUFFER_SAMPLES * NUM_CHANNELS);
    for i := 0 to BUFFER_SAMPLES * NUM_CHANNELS - 1 do
      WaveBuffers[CurrentBuffer][i] := Trunc(s3mvolume / 65535 * s3mbuffer[i]);

    { Submit buffer to audio system. }
    PWaveHeader := @WaveHeaders[CurrentBuffer];
    CheckMMError(WaveOutUnprepareHeader(WaveOutHandle, PWaveHeader,
      SizeOf(TWaveHdr)));
    CheckMMError(WaveOutPrepareHeader(WaveOutHandle, PWaveHeader, SizeOf(TWaveHdr)));
    CheckMMError(WaveOutWrite(WaveOutHandle, PWaveHeader, SizeOf(TWaveHdr)));

    { Next buffer. }
    CurrentBuffer := CurrentBuffer + 1;
    if CurrentBuffer >= NUM_BUFFERS then
      CurrentBuffer := 0;
  end;

  { Close audio device when finished. }
  while WaveOutClose(WaveOutHandle) = WAVERR_STILLPLAYING do
    I_Sleep(100);

  _s3m_stop(@s3module);
  s3m_msg := S3M_MSG_NONE;
end;

//==============================================================================
//
// I_PlayS3M
//
//==============================================================================
procedure I_PlayS3M(const data: pointer; const size: integer);
begin
  if s3m_status = MOD_STATUS_PLAYING then
  begin
    s3m_msg := S3M_MSG_STOP;
    while s3m_msg <> S3M_MSG_NONE do
      I_Sleep(10);
  end;

  s3m_thread.Wait;

  s3m_datasize := size;
  s3m_datapointer := data;

  s3m_msg := S3M_MSG_PLAY;
  s3m_thread.Activate(nil);
end;

//==============================================================================
//
// I_PauseS3M
//
//==============================================================================
procedure I_PauseS3M;
begin
  if s3m_status = MOD_STATUS_PLAYING then
    s3m_msg := S3M_MSG_PAUSE;
end;

//==============================================================================
//
// I_ResumeS3M
//
//==============================================================================
procedure I_ResumeS3M;
begin
  if s3m_status = MOD_STATUS_PLAYING then
    if s3m_msg = S3M_MSG_PAUSE then
      s3m_msg := S3M_MSG_RESUME;
end;

//==============================================================================
//
// I_StopS3M
//
//==============================================================================
procedure I_StopS3M;
begin
  s3m_msg := S3M_MSG_STOP;
end;

//==============================================================================
//
// threadproc
//
//==============================================================================
function threadproc(p: pointer): integer; stdcall;
begin
  s3m_status := MOD_STATUS_PLAYING;
  LoadS3M;
  PlayS3M;
  result := 0;
  s3m_status := MOD_STATUS_IDLE;
end;

//==============================================================================
//
// I_InitS3M
//
//==============================================================================
procedure I_InitS3M;
begin
  s3m_msg := S3M_MSG_NONE;
  s3m_status := MOD_STATUS_IDLE;
  s3m_thread := TDThread.Create(@threadproc);
end;

//==============================================================================
//
// I_ShutDownS3M
//
//==============================================================================
procedure I_ShutDownS3M;
begin
  if s3m_status = MOD_STATUS_PLAYING then
  begin
    s3m_msg := S3M_MSG_STOP;
    while s3m_msg <> S3M_MSG_NONE do
      I_Sleep(10);
  end;
  s3m_thread.Wait;
  s3m_thread.Free;
end;

const
  S3M_VOLUME_CONTROL: array[0..15] of word = (
        0,     4369,     8738,    13107,
    17476,    21845,    26214,    30583,
    34952,    39321,    43690,    48059,
    52428,    56797,    61166,    65535
  );

//==============================================================================
//
// I_SetMusicVolumeS3M
//
//==============================================================================
procedure I_SetMusicVolumeS3M(volume: integer);
var
  vol: integer;
begin
  vol := GetIntegerInRange(volume, 0, 15);
  s3mvolume := S3M_VOLUME_CONTROL[vol];
end;

//==============================================================================
//
// I_ProcessS3M
//
//==============================================================================
procedure I_ProcessS3M;
begin
end;

//==============================================================================
//
// IsS3MMusicFile
//
//==============================================================================
function IsS3MMusicFile(const buf: pointer; const size: integer): boolean;
var
  h: Ps3m_header_t;
begin
  if size < SizeOf(s3m_header_t) then
  begin
    result := false;
    exit;
  end;

  h := buf;
  Result := (h.id[0] = 'S') and (h.id[1] = 'C') and (h.id[2] = 'R') and (h.id[3] = 'M')
end;

end.
