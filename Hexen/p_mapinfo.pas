//------------------------------------------------------------------------------
//
//  DelphiHexen is a source port of the game Hexen and it is
//  based on original Linux Doom as published by "id Software", on
//  Hexen source as published by "Raven" software and DelphiDoom
//  as published by Jim Valavanis.
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
//   MAPINFO
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit p_mapinfo;

interface

uses
  m_fixed;

//==============================================================================
//
// P_InitMapInfo
//
//==============================================================================
procedure P_InitMapInfo;

//==============================================================================
//
// P_GetMapSky1Texture
//
//==============================================================================
function P_GetMapSky1Texture(map: integer): integer;

//==============================================================================
//
// P_GetMapSky2Texture
//
//==============================================================================
function P_GetMapSky2Texture(map: integer): integer;

//==============================================================================
//
// P_GetMapSky1ScrollDelta
//
//==============================================================================
function P_GetMapSky1ScrollDelta(map: integer): fixed_t;

//==============================================================================
//
// P_GetMapSky2ScrollDelta
//
//==============================================================================
function P_GetMapSky2ScrollDelta(map: integer): fixed_t;

//==============================================================================
//
// P_GetMapDoubleSky
//
//==============================================================================
function P_GetMapDoubleSky(map: integer): boolean;

//==============================================================================
//
// P_GetMapLightning
//
//==============================================================================
function P_GetMapLightning(map: integer): boolean;

//==============================================================================
//
// P_GetMapNoJump
//
//==============================================================================
function P_GetMapNoJump(map: integer): boolean;

//==============================================================================
//
// P_GetMapNoCrouch
//
//==============================================================================
function P_GetMapNoCrouch(map: integer): boolean;

//==============================================================================
//
// P_TranslateMap
//
//==============================================================================
function P_TranslateMap(map: integer): integer;

//==============================================================================
//
// P_GetMapNextMap
//
//==============================================================================
function P_GetMapNextMap(map: integer): integer;

//==============================================================================
//
// P_GetMapDescName
//
//==============================================================================
function P_GetMapDescName(const map: integer): string;

//==============================================================================
//
// P_GetMapSongLump
//
//==============================================================================
function P_GetMapSongLump(map: integer): string;

//==============================================================================
//
// P_GetMapCluster
//
//==============================================================================
function P_GetMapCluster(map: integer): integer;

//==============================================================================
//
// P_GetMapFadeTable
//
//==============================================================================
function P_GetMapFadeTable(map: integer): integer;

implementation

uses
  d_delphi,
  r_data,
  sc_engine,
  w_wad;

const
  MAPINFO_SCRIPT_NAME = 'MAPINFO';
  MCMD_NOMATCH = -1;
  MCMD_SKY1 = 1;
  MCMD_SKY2 = 2;
  MCMD_LIGHTNING = 3;
  MCMD_FADETABLE = 4;
  MCMD_DOUBLESKY = 5;
  MCMD_CLUSTER = 6;
  MCMD_WARPTRANS = 7;
  MCMD_NEXT = 8;
  MCMD_CDTRACK = 9;
  MCMD_CD_STARTTRACK = 10;
  MCMD_CD_END1TRACK = 11;
  MCMD_CD_END2TRACK = 12;
  MCMD_CD_END3TRACK = 13;
  MCMD_CD_INTERTRACK = 14;
  MCMD_CD_TITLETRACK = 15;
  MCMD_NOJUMP = 16;
  MCMD_NOCROUCH = 17;

const
  MapCmdIDs: array[-1..16] of integer = (
    MCMD_NOMATCH,
    MCMD_SKY1,
    MCMD_SKY2,
    MCMD_DOUBLESKY,
    MCMD_LIGHTNING,
    MCMD_FADETABLE,
    MCMD_CLUSTER,
    MCMD_WARPTRANS,
    MCMD_NEXT,
    MCMD_CDTRACK,
    MCMD_CD_STARTTRACK,
    MCMD_CD_END1TRACK,
    MCMD_CD_END2TRACK,
    MCMD_CD_END3TRACK,
    MCMD_CD_INTERTRACK,
    MCMD_CD_TITLETRACK,
    MCMD_NOJUMP,
    MCMD_NOCROUCH
  );

var
  cd_NonLevelTracks: array[0..5] of integer;

const
  UNKNOWN_MAP_NAME = 'DEVELOPMENT MAP';
  DEFAULT_SKY_NAME = 'SKY1';
  DEFAULT_SONG_LUMP = 'DEFSONG';
  DEFAULT_FADE_TABLE = 'COLORMAP';

type
  mapinfo_t = record
    cluster: smallint;
    warpTrans: smallint;
    nextMap: smallint;
    cdTrack: smallint;
    name: string[32];
    sky1Texture: smallint;
    sky2Texture: smallint;
    sky1ScrollDelta: fixed_t;
    sky2ScrollDelta: fixed_t;
    doubleSky: boolean;
    lightning: boolean;
    nojump: boolean;
    nocrouch: boolean;
    fadetable: integer;
    songLump: string[10];
  end;
  Pmapinfo_t = ^mapinfo_t;

var
  mapcount: integer;

var
  MapInfo: array[0..99] of mapinfo_t;

//==============================================================================
//
// P_GetMapDescName
//
//==============================================================================
function P_GetMapDescName(const map: integer): string;
begin
  result := MapInfo[map].name;
end;

//==============================================================================
//
// P_InitMapInfo
//
//==============================================================================
procedure P_InitMapInfo;
var
  map: integer;
  mapMax: integer;
  mcmdValue: integer;
  info: Pmapinfo_t;
  sc: TScriptEngine;
  MapCmdNames: TDStringList;
begin
  MapCmdNames := TDStringList.Create;
  MapCmdNames.Add('SKY1');
  MapCmdNames.Add('SKY2');
  MapCmdNames.Add('DOUBLESKY');
  MapCmdNames.Add('LIGHTNING');
  MapCmdNames.Add('FADETABLE');
  MapCmdNames.Add('CLUSTER');
  MapCmdNames.Add('WARPTRANS');
  MapCmdNames.Add('NEXT');
  MapCmdNames.Add('CDTRACK');
  MapCmdNames.Add('CD_START_TRACK');
  MapCmdNames.Add('CD_END1_TRACK');
  MapCmdNames.Add('CD_END2_TRACK');
  MapCmdNames.Add('CD_END3_TRACK');
  MapCmdNames.Add('CD_INTERMISSION_TRACK');
  MapCmdNames.Add('CD_TITLE_TRACK');
  MapCmdNames.Add('NOJUMP');
  MapCmdNames.Add('NOCROUCH');

  mapMax := 1;

  // Put defaults into MapInfo[0]
  info := @MapInfo[0];
  info.cluster := 0;
  info.warpTrans := 0;
  info.nextMap := 1; // Always go to map 1 if not specified
  info.cdTrack := 1;
  info.sky1Texture := R_CheckTextureNumForName(DEFAULT_SKY_NAME); // JVAL: Originally was R_TextureNumForName
  info.sky2Texture := info.sky1Texture;
  info.sky1ScrollDelta := 0;
  info.sky2ScrollDelta := 0;
  info.doubleSky := false;
  info.lightning := false;
  info.nojump := false;
  info.fadetable := W_GetNumForName(DEFAULT_FADE_TABLE);
  info.name := UNKNOWN_MAP_NAME;

  sc := TScriptEngine.Create(SC_RemoveLineQuotes(W_TextLumpName(MAPINFO_SCRIPT_NAME)));
  while sc.GetString do
  begin
    if not sc.Compare('MAP') then
    begin
      sc.ScriptError('"MAP" expected'#13#10);
    end;
    sc.MustGetInteger;
    if (sc._Integer < 1) or (sc._Integer > 99) then
    begin //
      sc.ScriptError('Map number (%d) must be in [1..99] range'#13#10, [sc._Integer]);
    end;
    map := sc._Integer;

    info := @MapInfo[map];

    // Copy defaults to current map definition
    memcpy(info, @MapInfo[0], SizeOf(mapinfo_t));

    // The warp translation defaults to the map number
    info.warpTrans := map;

    // Map name must follow the number
    sc.MustGetString;
    info.name := sc._String;

    // Process optional tokens
    while sc.GetString do
    begin
      if sc.Compare('MAP') then
      begin // Start next map definition
        sc.UnGet;
        break;
      end;
      mcmdValue := MapCmdIDs[sc.MustMatchString(MapCmdNames)];
      case mcmdValue of
        MCMD_CLUSTER:
          begin
            sc.MustGetInteger;
            info.cluster := sc._Integer;
          end;
        MCMD_WARPTRANS:
          begin
            sc.MustGetInteger;
            info.warpTrans := sc._Integer;
          end;
        MCMD_NEXT:
          begin
            sc.MustGetInteger;
            info.nextMap := sc._Integer;
          end;
        MCMD_CDTRACK:
          begin
            sc.MustGetInteger;
            info.cdTrack := sc._Integer;
          end;
        MCMD_SKY1:
          begin
            sc.MustGetString;
            info.sky1Texture := R_TextureNumForName(sc._String);
            sc.MustGetInteger;
            info.sky1ScrollDelta := sc._Integer * 256;
          end;
        MCMD_SKY2:
          begin
            sc.MustGetString;
            info.sky2Texture := R_TextureNumForName(sc._String);
            sc.MustGetInteger;
            info.sky2ScrollDelta := sc._Integer * 256;
          end;
        MCMD_DOUBLESKY:
          begin
            info.doubleSky := true;
          end;
        MCMD_LIGHTNING:
          begin
            info.lightning := true;
          end;
        MCMD_NOJUMP:
          begin
            info.nojump := true;
          end;
        MCMD_NOCROUCH:
          begin
            info.nocrouch := true;
          end;
        MCMD_FADETABLE:
          begin
            sc.MustGetString;
            info.fadetable := W_GetNumForName(sc._String);
          end;
        MCMD_CD_STARTTRACK,
        MCMD_CD_END1TRACK,
        MCMD_CD_END2TRACK,
        MCMD_CD_END3TRACK,
        MCMD_CD_INTERTRACK,
        MCMD_CD_TITLETRACK:
          begin
            sc.MustGetInteger;
            cd_NonLevelTracks[mcmdValue - MCMD_CD_STARTTRACK] := sc._Integer;
          end;
      end;
    end;
    if map > mapMax then
      mapMax := map;
  end;
  sc.Free;
  MapCmdNames.Free;
  MapCount := mapMax;
end;

//==============================================================================
//
// P_QualifyMap
//
//==============================================================================
function P_QualifyMap(map: integer): integer;
begin
  if (map < 1) or (map > MapCount) then
    result := 0
  else
    result := map;
end;

//==============================================================================
//
// P_GetMapCluster
//
//==============================================================================
function P_GetMapCluster(map: integer): integer;
begin
  result := MapInfo[P_QualifyMap(map)].cluster;
end;

//==============================================================================
//
// P_GetMapCDTrack
//
//==============================================================================
function P_GetMapCDTrack(map: integer): integer;
begin
  result := MapInfo[P_QualifyMap(map)].cdTrack;
end;

//==============================================================================
//
// P_GetMapWarpTrans
//
//==============================================================================
function P_GetMapWarpTrans(map: integer): integer;
begin
  result := MapInfo[P_QualifyMap(map)].warpTrans;
end;

//==============================================================================
//
// P_GetMapNextMap
//
//==============================================================================
function P_GetMapNextMap(map: integer): integer;
begin
  result := MapInfo[P_QualifyMap(map)].nextMap;
end;

//==============================================================================
//
// P_TranslateMap
//
// Returns the actual map number given a warp map number.
//
//==============================================================================
function P_TranslateMap(map: integer): integer;
var
  i: integer;
begin
  for i := 1 to 98 do // Make this a macro
  begin
    if MapInfo[i].warpTrans = map then
    begin
      result := i;
      exit;
    end;
  end;
  // Not found
  result := -1;
end;

//==============================================================================
//
// P_GetMapSky1Texture
//
//==============================================================================
function P_GetMapSky1Texture(map: integer): integer;
begin
  result := MapInfo[P_QualifyMap(map)].sky1Texture;
end;

//==============================================================================
//
// P_GetMapSky2Texture
//
//==============================================================================
function P_GetMapSky2Texture(map: integer): integer;
begin
  result := MapInfo[P_QualifyMap(map)].sky2Texture;
end;

//==============================================================================
//
// P_GetMapSky1ScrollDelta
//
//==============================================================================
function P_GetMapSky1ScrollDelta(map: integer): fixed_t;
begin
  result := MapInfo[P_QualifyMap(map)].sky1ScrollDelta;
end;

//==============================================================================
//
// P_GetMapSky2ScrollDelta
//
//==============================================================================
function P_GetMapSky2ScrollDelta(map: integer): fixed_t;
begin
  result := MapInfo[P_QualifyMap(map)].sky2ScrollDelta;
end;

//==============================================================================
//
// P_GetMapDoubleSky
//
//==============================================================================
function P_GetMapDoubleSky(map: integer): boolean;
begin
  result := MapInfo[P_QualifyMap(map)].doubleSky;
end;

//==============================================================================
//
// P_GetMapLightning
//
//==============================================================================
function P_GetMapLightning(map: integer): boolean;
begin
  result := MapInfo[P_QualifyMap(map)].lightning;
end;

//==============================================================================
//
// P_GetMapNoJump
//
//==============================================================================
function P_GetMapNoJump(map: integer): boolean;
begin
  result := MapInfo[P_QualifyMap(map)].nojump;
end;

//==============================================================================
//
// P_GetMapNoCrouch
//
//==============================================================================
function P_GetMapNoCrouch(map: integer): boolean;
begin
  result := MapInfo[P_QualifyMap(map)].nocrouch;
end;

//==============================================================================
//
// P_GetMapFadeTable
//
//==============================================================================
function P_GetMapFadeTable(map: integer): integer;
begin
  result := MapInfo[P_QualifyMap(map)].fadetable;
end;

//==============================================================================
//
// P_GetMapSongLump
//
//==============================================================================
function P_GetMapSongLump(map: integer): string;
begin
  if strupper(MapInfo[P_QualifyMap(map)].songLump) = strupper(DEFAULT_SONG_LUMP) then
    result := ''
  else
    result := MapInfo[P_QualifyMap(map)].songLump;
end;

//==============================================================================
//
// P_PutMapSongLump
//
//==============================================================================
procedure P_PutMapSongLump(map: integer; const lumpName: string);
begin
  if (map < 1) or (map > MapCount) then
    exit;

  MapInfo[map].songLump := lumpName;
end;

//==============================================================================
//
// P_GetCDStartTrack
//
//==============================================================================
function P_GetCDStartTrack: integer;
begin
  result := cd_NonLevelTracks[MCMD_CD_STARTTRACK - MCMD_CD_STARTTRACK];
end;

//==============================================================================
//
// P_GetCDEnd1Track
//
//==============================================================================
function P_GetCDEnd1Track: integer;
begin
  result := cd_NonLevelTracks[MCMD_CD_END1TRACK - MCMD_CD_STARTTRACK];
end;

//==============================================================================
//
// P_GetCDEnd2Track
//
//==============================================================================
function P_GetCDEnd2Track: integer;
begin
  result := cd_NonLevelTracks[MCMD_CD_END2TRACK - MCMD_CD_STARTTRACK];
end;

//==============================================================================
//
// P_GetCDEnd3Track
//
//==============================================================================
function P_GetCDEnd3Track: integer;
begin
  result := cd_NonLevelTracks[MCMD_CD_END3TRACK - MCMD_CD_STARTTRACK];
end;

//==============================================================================
//
// P_GetCDIntermissionTrack
//
//==============================================================================
function P_GetCDIntermissionTrack: integer;
begin
  result := cd_NonLevelTracks[MCMD_CD_INTERTRACK - MCMD_CD_STARTTRACK];
end;

//==============================================================================
//
// P_GetCDTitleTrack
//
//==============================================================================
function P_GetCDTitleTrack: integer;
begin
  result := cd_NonLevelTracks[MCMD_CD_TITLETRACK - MCMD_CD_STARTTRACK];
end;

end.
 
