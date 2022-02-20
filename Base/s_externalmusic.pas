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
//  DESCRIPTION:
//   External track music
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit s_externalmusic;

interface

uses
  d_delphi,
  sounddata;

type
  TExternalMusicInfo = class
    data: pointer;
    size: integer;
    constructor Create(const strm: TDStream); virtual;
  end;

//==============================================================================
//
// S_ExternalMusicInit
//
//==============================================================================
procedure S_ExternalMusicInit;

//==============================================================================
//
// S_ShutDownExternalMusic
//
//==============================================================================
procedure S_ShutDownExternalMusic;

//==============================================================================
//
// S_TryLoadExternalMusic
//
//==============================================================================
function S_TryLoadExternalMusic(music: Pmusicinfo_t): boolean;

implementation

uses
  doomdef,
  i_music,
  z_zone,
  w_pak,
  w_wad;

//==============================================================================
//
// TExternalMusicInfo.Create
//
//==============================================================================
constructor TExternalMusicInfo.Create(const strm: TDStream);
begin
  size := strm.Size;
  strm.Seek(0, sFromBeginning);
  data := Z_Malloc(size, PU_MUSIC, @data);
  strm.Read(data^, size);
end;

var
  externalmusic: TDStringList;

//==============================================================================
//
// S_ExternalMusicInit
//
//==============================================================================
procedure S_ExternalMusicInit;
begin
  externalmusic := TDStringList.Create;
end;

//==============================================================================
//
// S_ShutDownExternalMusic
//
//==============================================================================
procedure S_ShutDownExternalMusic;
var
  i: integer;
begin
  if externalmusic <> nil then
  begin
    for i := 0 to externalmusic.Count - 1 do
      externalmusic.Objects[i].Free;

    externalmusic.Free;
  end;
end;

const
  NUM_MUSIC_EXTENSIONS = 4;
  MUSIC_EXTENSIONS: array[0..NUM_MUSIC_EXTENSIONS - 1] of string[4] = (
    '.MOD', '.S3M', '.IT', '.XM'
  );

//==============================================================================
//
// S_TryLoadExternalMusic
//
//==============================================================================
function S_TryLoadExternalMusic(music: Pmusicinfo_t): boolean;
var
  i: integer;
  mname: string;
  idx: integer;
  minfo: TExternalMusicInfo;
  strm: TPakStream;
begin
  mname := char8tostring(lumpinfo[music.lumpnum].name);
  idx := externalmusic.IndexOf(mname);
  if idx >= 0 then
  begin
    minfo := externalmusic.Objects[idx] as TExternalMusicInfo;
    if minfo.data <> nil then
    begin
      music.data := minfo.data;
      music.handle := I_RegisterSong(music.data, minfo.size);
      Result := True;
      Exit;
    end;
    minfo.Free;
    externalmusic.Delete(idx);
  end;

  for i := 0 to NUM_MUSIC_EXTENSIONS - 1 do
  begin
    strm := TPakStream.Create(mname + MUSIC_EXTENSIONS[i], pm_prefered, gamedirectories);
    if strm.IOResult = 0 then
    begin
      minfo := TExternalMusicInfo.Create(strm);
      externalmusic.AddObject(mname, minfo);
      music.data := minfo.data;
      music.handle := I_RegisterSong(music.data, minfo.size);
      Result := True;
      strm.Free;
      Exit;
    end;
    strm.Free;
  end;
  Result := False;
end;

end.
