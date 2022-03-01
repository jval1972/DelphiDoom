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
//  Midi file playback
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit i_midi;

interface

const
  MThd = $6468544D; // Start of file
  MTrk = $6B72544D; // Start of track

const
  midivolumecontrol: array[0..15] of integer = (
    0, 7, 13, 20, 27, 33, 40, 47, 53, 60, 67, 73, 80, 87, 93, 100
  );

const
  MIDI_CTRLCHANGE: Byte = $B0; // + ctrlr + value
  MIDICTRL_VOLUME: Byte = $07;

//==============================================================================
//
// I_PlayMidi
//
//==============================================================================
procedure I_PlayMidi(const aMidiFile: string);

//==============================================================================
//
// I_StopMidi
//
//==============================================================================
procedure I_StopMidi;

//==============================================================================
//
// I_IsMidiPlaying
//
//==============================================================================
function I_IsMidiPlaying: boolean;

//==============================================================================
//
// I_ResumeMidi
//
//==============================================================================
procedure I_ResumeMidi;

//==============================================================================
//
// I_PauseMidi
//
//==============================================================================
procedure I_PauseMidi;

//==============================================================================
//
// I_InitMidi
//
//==============================================================================
procedure I_InitMidi;

//==============================================================================
//
// I_ShutDownMidi
//
//==============================================================================
procedure I_ShutDownMidi;

//==============================================================================
//
// I_SetMusicVolumeMidi
//
//==============================================================================
procedure I_SetMusicVolumeMidi(volume: integer);

//==============================================================================
//
// I_ProcessMidi
//
//==============================================================================
procedure I_ProcessMidi;

//==============================================================================
//
// _mciGetErrorString
//
//==============================================================================
function _mciGetErrorString(const code: LongWord): string;

var
  snd_uselegacymidiplayer: integer = 2;

implementation

uses
  Windows,
  Messages,
  MMSystem,
  d_delphi,
  m_argv,
  i_midi_new,
  i_midi_legacy;

var
  uselegacymidiplayer: boolean = false;

//==============================================================================
//
// I_PlayMidi
//
//==============================================================================
procedure I_PlayMidi(const aMidiFile: string);
begin
  if uselegacymidiplayer then
  begin
    ClearMidiFilePlayList_Legacy;
    AddMidiFileToPlayList_Legacy(aMidiFile);
    I_PlayMidi_Legacy(0);
  end
  else
    I_PlayMidi_New(aMidiFile);
end;

//==============================================================================
//
// I_StopMidi
//
//==============================================================================
procedure I_StopMidi;
begin
  if uselegacymidiplayer then
    I_StopMidi_Legacy
  else
    I_StopMidi_New;
end;

//==============================================================================
//
// I_IsMidiPlaying
//
//==============================================================================
function I_IsMidiPlaying: boolean;
begin
  if uselegacymidiplayer then
    result := I_IsMidiPlaying_Legacy
  else
    result := I_IsMidiPlaying_New;
end;

//==============================================================================
//
// I_ResumeMidi
//
//==============================================================================
procedure I_ResumeMidi;
begin
  if uselegacymidiplayer then
    I_ResumeMidi_Legacy
  else
    I_ResumeMidi_New;
end;

//==============================================================================
//
// I_PauseMidi
//
//==============================================================================
procedure I_PauseMidi;
begin
  if uselegacymidiplayer then
    I_PauseMidi_Legacy
  else
    I_PauseMidi_New;
end;

//==============================================================================
//
// I_InitMidi
//
//==============================================================================
procedure I_InitMidi;
begin
  if M_CheckParm('-uselegacymidiplayer') > 0 then
    snd_uselegacymidiplayer := 1;
  if M_CheckParm('-nouselegacymidiplayer') > 0 then
    snd_uselegacymidiplayer := 0;
  if M_CheckParm('-internalmidiplayer') > 0 then
    snd_uselegacymidiplayer := 2;
  uselegacymidiplayer := snd_uselegacymidiplayer = 1;
  if uselegacymidiplayer then
  begin
    printf('Using legacy midi playback'#13#10);
    I_InitMidi_Legacy;
  end
  else
    I_InitMidi_New
end;

//==============================================================================
//
// I_ShutDownMidi
//
//==============================================================================
procedure I_ShutDownMidi;
begin
  if uselegacymidiplayer then
    I_ShutDownMidi_Legacy
  else
    I_ShutDownMidi_New
end;

//==============================================================================
//
// I_SetMusicVolumeMidi
//
//==============================================================================
procedure I_SetMusicVolumeMidi(volume: integer);
begin
  if uselegacymidiplayer then
    I_SetMusicVolumeMidi_Legacy(volume)
  else
    I_SetMusicVolumeMidi_New(volume);
end;

//==============================================================================
//
// I_ProcessMidi
//
//==============================================================================
procedure I_ProcessMidi;
begin
  if uselegacymidiplayer then
    I_ProcessMidi_Legacy
  else
    I_ProcessMidi_New;
end;

//==============================================================================
//
// _mciGetErrorString
//
//==============================================================================
function _mciGetErrorString(const code: LongWord): string;
var
  buf: array[0..127] of char;
  i:  integer;
begin
  result := '';
  ZeroMemory(@buf, SizeOf(buf));
  if mciGetErrorString(code, buf, SizeOf(buf)) then
    for i := 0 to SizeOf(buf) - 1 do
    begin
      if buf[i] = #0 then
        break;
      result := result + buf[i];
    end;
end;

end.
