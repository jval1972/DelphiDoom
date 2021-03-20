//------------------------------------------------------------------------------
//
//  DelphiDoom: A modified and improved DOOM engine for Windows
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 1993-1996 by id Software, Inc.
//  Copyright (C) 2004-2021 by Jim Valavanis
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
//  S3M & MOD music file playback using mikwin.dll
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit i_mikplay;

interface

uses
  i_music;

procedure I_PlayMik(const data: pointer; const size: integer; const typ: music_t);

procedure I_PauseMik(const typ: music_t);

procedure I_ResumeMik(const typ: music_t);

procedure I_StopMik(const typ: music_t);

procedure I_InitMik;

procedure I_ShutDownMik;

procedure I_SetMusicVolumeMik(volume: integer; const typ: music_t);

procedure I_ProcessMik(const typ: music_t);

implementation

uses
  d_delphi,
  m_argv,
  m_sha1,
  m_misc,
  mikmod,
  i_modmusic,
  i_s3mmusic,
  i_system,
  i_tmp,
  i_mainwindow;

var
  mik_init: boolean = false;

const
  MIK_FREQ = 44100;

procedure I_InitMik;
var
  errstr: string;
begin
  mik_init := MikWin_LoadLibrary;
  if mik_init then
    mik_init := MikWin_Init(MIK_FREQ, True, True, True, hMainWnd, 0);
  if not mik_init then
  begin
    errstr := MikWin_GetErrorText;
    I_Warning('I_InitMik(): %s'#13#10, [errstr]);
  end;
end;

procedure I_ShutDownMik;
begin
  MikWin_Free;
  MikWin_FreeLibrary;
end;

procedure I_PlayMik(const data: pointer; const size: integer; const typ: music_t);
var
  mikfilename: string;
begin
  if not mik_init then
  begin
    if typ = m_s3m then
      I_PlayS3M(data, size)
    else if typ = m_mod then
      I_PlayMod(data, size);
    Exit;
  end;

  mikfilename := M_SaveFileName('DATA\');
  MkDir(mikfilename);
  mikfilename := mikfilename + 'TMP\';
  MkDir(mikfilename);
  mikfilename := mikfilename + 'mik_' + readablestring(SHA1_CalcSHA1Buf(data^, size)) + '.s3m';
  if not fexists(mikfilename) or (fsize(mikfilename) <> size) then
    M_WriteFile(mikfilename, data, size);
  I_DeclareTempFile(mikfilename);

  if not MikWin_Load(PChar(mikfilename)) then
  begin
    if typ = m_s3m then
      I_PlayS3M(data, size)
    else if typ = m_mod then
      I_PlayMOD(data, size);
    mik_init := False;
    Exit;
  end;

  MikWin_Play(True);
end;

procedure I_PauseMik(const typ: music_t);
begin
  if not mik_init then
  begin
    if typ = m_s3m then
      I_PauseS3M
    else if typ = m_mod then
      I_PauseMod;
    Exit;
  end;

  if not MikWin_Paused then
    MikWin_Pause;
end;

procedure I_ResumeMik(const typ: music_t);
begin
  if not mik_init then
  begin
    if typ = m_s3m then
      I_ResumeS3M
    else if typ = m_mod then
      I_ResumeMod;
    Exit;
  end;

  if MikWin_Paused then
    MikWin_Pause;
end;

procedure I_StopMik(const typ: music_t);
begin
  if not mik_init then
  begin
    if typ = m_s3m then
      I_StopS3M
    else if typ = m_mod then
      I_StopMod;
    Exit;
  end;

  MikWin_Stop;
end;

const
  MIK_VOLUME_CONTROL: array[0..15] of word = (
      0,     9,     17,    26,
     34,    43,     51,    60,
     68,    77,     85,    94,
    102,   111,    119,   127
  );

procedure I_SetMusicVolumeMik(volume: integer; const typ: music_t);
begin
  if not mik_init then
  begin
    if typ = m_s3m then
      I_SetMusicVolumeS3M(volume)
    else if typ = m_mod then
      I_SetMusicVolumeMod(volume);
    Exit;
  end;

  Player_SetVolume(MIK_VOLUME_CONTROL[ibetween(volume, 1, 15)]);
end;

procedure I_ProcessMik(const typ: music_t);
begin
  if not mik_init then
  begin
    if typ = m_s3m then
      I_ProcessS3M
    else if typ = m_mod then
      I_ProcessMod;
    Exit;
  end;

  MikWin_Update;
end;

end.
