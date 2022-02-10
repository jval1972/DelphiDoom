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

unit p_musinfo;

interface

uses
  p_mobj_h;

//==============================================================================
//
// A_MusicChanger
//
//==============================================================================
procedure A_MusicChanger(actor: Pmobj_t);

const
  S_MUSINFO_PARAM = 'MUSINFOPARAM';
  S_MUSINFO_LUMP = 'MUSINFO';

const
  MUSICCHANGER_LO = 14100;
  MUSICCHANGER_HI = 14164;
  MUSICCHANGER = 14165;

//==============================================================================
//
// P_InitMusInfo
//
//==============================================================================
procedure P_InitMusInfo;

//==============================================================================
//
// P_FindMusinfoMusic
//
//==============================================================================
function P_FindMusinfoMusic(const id: integer; const mapname: string): integer;

implementation

uses
  d_delphi,
  i_system,
  d_player,
  g_game,
  r_defs,
  p_params,
  p_setup,
  p_levelinfo,
  sounddata,
  sounds,
  s_sound,
  sc_engine,
  w_wad;

const
  S_MC_COUNTDOWN = 'COUNTDOWN';
  S_MC_PLAYERONSECTOR = 'PLAYERONSECTOR';

const
  MUSINFOTICS = 30;

//==============================================================================
//
// A_MusicChanger
//
//==============================================================================
procedure A_MusicChanger(actor: Pmobj_t);
var
  countdown: integer;
  oldplayeronsector: integer;
  playeronsector: integer;
  sec: Psector_t;
  mo: Pmobj_t;
  musid, musnumber: integer;
  levelname: string;
  levelinfo: Plevelinfo_t;
begin
  countdown := P_GetMobjCustomParamValue(actor, S_MC_COUNTDOWN, -1);

  oldplayeronsector := P_GetMobjCustomParamValue(actor, S_MC_PLAYERONSECTOR, 0);
  playeronsector := 0;

  sec := Psubsector_t(actor.subsector).sector;
  mo := sec.thinglist;
  while mo <> nil do
  begin
    if mo.player <> nil then
      if mo.player = @players[consoleplayer] then
      begin
        playeronsector := 1;
        break;
      end;
    mo := mo.snext;
  end;

  P_SetMobjCustomParam(actor, S_MC_PLAYERONSECTOR, playeronsector);

  if playeronsector = 0 then
  begin
    P_SetMobjCustomParam(actor, S_MC_COUNTDOWN, -1);
    exit;
  end;

  if playeronsector = 1 then
    if oldplayeronsector = 0 then
      P_SetMobjCustomParam(actor, S_MC_COUNTDOWN, MUSINFOTICS);

  if countdown < 0 then
    exit;

  if countdown > 0 then
  begin
    dec(countdown);
    P_SetMobjCustomParam(actor, S_MC_COUNTDOWN, countdown);
    exit;
  end;

  if countdown = 0 then
  begin
    P_SetMobjCustomParam(actor, S_MC_COUNTDOWN, -1);
    musid := P_GetMobjCustomParamValue(actor, S_MUSINFO_PARAM, -1);
    if musid >= 0 then
    begin
      levelname := P_GetMapName({$IFDEF DOOM_OR_HERETIC}gameepisode, {$ENDIF}gamemap);
      if musid = 0 then
        musnumber := S_DefaultMusicForMap({$IFDEF DOOM_OR_HERETIC}gameepisode, {$ENDIF}gamemap)
      else
        musnumber := P_FindMusinfoMusic(musid, levelname);
      if musnumber > 0 then
      begin
        S_ChangeMusic(musnumber, true);
        levelinfo := P_GetLevelInfo(levelname);
        levelinfo.musname := stringtochar8(strupper(S_music[musnumber].name));
      end;
    end;
  end;
end;

const
  MAXMUSINFO = 1024;

type
  musinfo_t = record
    mapname: char8_t;
    musid: integer;
    musnumber: integer;
    musname: char8_t;
  end;
  Pmusinfo_t = ^musinfo_t;
  musinfo_tArray = array[0..MAXMUSINFO - 1] of musinfo_t;

var
  nummusinfo: integer = 0;
  musinfo: musinfo_tArray;

//==============================================================================
//
// P_AddMusInfo
//
//==============================================================================
procedure P_AddMusInfo(const mapname: string; const musid: integer; const musname: string);
var
  i, idx: integer;
  mapname8: char8_t;
  pm: Pmusinfo_t;
begin
  mapname8 := stringtochar8(strupper(mapname));

  idx := -1;
  for i := 0 to nummusinfo - 1 do
    if mapname8 = musinfo[i].mapname then
      if musid = musinfo[i].musid then
      begin
        idx := i;
        break;
      end;

  if idx = -1 then
  begin
    if nummusinfo < MAXMUSINFO then
    begin
      idx := nummusinfo;
      inc(nummusinfo);
    end
    else
    begin
      I_Warning('P_AddMusInfo(): More than %d musinfo entries found, ignoring mus "%s" of map "%d"'#13#10, [nummusinfo + 1, musname, mapname]);
      exit;
    end;
  end;

  pm := @musinfo[idx];
  pm.mapname := mapname8;
  pm.musid := musid;
  pm.musnumber := S_GetMusicNumForName(musname);
  pm.musname := stringtochar8(strupper(musname));
end;

//==============================================================================
//
// P_InitMusInfo
//
//==============================================================================
procedure P_InitMusInfo;
var
  sc: TScriptEngine;
  lump: integer;
  token, curmapname: string;
  ismap: boolean;
  musid: integer;
  smusid: string;
  musname: string;
begin
  lump := W_CheckNumForName(S_MUSINFO_LUMP);
  if lump < 0 then
    exit;

  curmapname := '';
  sc := TScriptEngine.Create(W_TextLumpNum(lump));
  while sc.GetString do
  begin
    token := strupper(sc._String);
    if token = '' then
      break;
    ismap := false;
    if length(token) = 4 then
      if (token[1] = 'E') and (token[3] = 'M') then
        if isdigit(token[2]) and isdigit(token[4]) then
          ismap := true;
    if length(token) = 5 then
      if (token[1] = 'M') and (token[2] = 'A') and (token[3] = 'P') then
        if isdigit(token[4]) and isdigit(token[5]) then
          ismap := true;
    if ismap then
      curmapname := token
    else
    begin
      if curmapname = '' then
      begin
        I_Warning('P_InitMusInfo(): Map name expected but token "%s" found'#13#10, [token]);
        break;
      end;
      smusid := sc._string;
      musid := atoi(smusid, -1);
      sc.MustGetString;
      musname := sc._String;
      if (musid >= 0) and (musid <= MUSICCHANGER_HI - MUSICCHANGER_LO) then
      begin
        if musname <> '' then
          P_AddMusInfo(curmapname, musid, musname)
        else
          break;
      end
      else
        I_Warning('P_InitMusInfo(): Invalid music id "%s" found, expected number in range [%d, %d]'#13#10,[smusid, 0, MUSICCHANGER_HI - MUSICCHANGER_LO]);
    end;
  end;
  sc.Free;
end;

//==============================================================================
//
// P_FindMusinfoMusic
//
//==============================================================================
function P_FindMusinfoMusic(const id: integer; const mapname: string): integer;
var
  check: char8_t;
  i: integer;
begin
  check := stringtochar8(strupper(mapname));
  for i := 0 to nummusinfo - 1 do
    if id = musinfo[i].musid then
      if check = musinfo[i].mapname then
      begin
        result := musinfo[i].musnumber;
        exit;
      end;
  result := -1;
end;

end.
