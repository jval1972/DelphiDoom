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
//  Support MUSINFO lump (dynamic music changing)
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit p_levelinfo;

interface

uses
  w_wad;

type
  Plevelinfo_t = ^levelinfo_t;
  levelinfo_t = record
    levelname: char8_t;
    musname: char8_t;
    skyflat: char8_t;
    next: Plevelinfo_t;
  end;

//==============================================================================
//
// P_GetLevelInfo
//
//==============================================================================
function P_GetLevelInfo(const levelname: string): Plevelinfo_t;

//==============================================================================
//
// P_LevelInfoChangeMusic
//
//==============================================================================
procedure P_LevelInfoChangeMusic;

implementation

uses
  d_delphi,
  g_game,
  p_setup,
  sounddata,
  s_sound,
  z_zone;

var
  levelinfo: Plevelinfo_t;

//==============================================================================
//
// P_GetLevelInfo
//
//==============================================================================
function P_GetLevelInfo(const levelname: string): Plevelinfo_t;
var
  check: string;
begin
  check := strupper(levelname);
  result := levelinfo;
  while true do
  begin
    if result = nil then
    begin
      result := Z_Malloc(SizeOf(levelinfo_t), PU_STATIC, nil);
      ZeroMemory(result, SizeOf(levelinfo_t));
      result.levelname := stringtochar8(check);
      result.next := levelinfo;
      levelinfo := result;
    end;
    if char8tostring(result.levelname) = check then
      break;
    result := result.next;
  end;
end;

//==============================================================================
//
// P_LevelInfoChangeMusic
//
//==============================================================================
procedure P_LevelInfoChangeMusic;
var
  linfo: Plevelinfo_t;
  levelname: string;
  musname: string;
  i: integer;
begin
  levelname := P_GetMapName({$IFDEF DOOM_OR_HERETIC}gameepisode, {$ENDIF}gamemap);
  linfo := P_GetLevelInfo(levelname);
  musname := strtrim(strupper(char8tostring(linfo.musname)));
  if musname = '' then
    exit;
  for i := 0 to nummusic - 1 do
    if musname = strupper(S_music[i].name) then
    begin
      S_ChangeMusic(i, true);
      exit;
    end;
end;

end.
