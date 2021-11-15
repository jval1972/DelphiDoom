//------------------------------------------------------------------------------
//
//  DelphiHexen: A modified and improved Hexen port for Windows
//  based on original Linux Doom as published by "id Software", on
//  Hexen source as published by "Raven" software and DelphiDoom
//  as published by Jim Valavanis.
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
//  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
//  02111-1307, USA.
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit sv_save;

interface

procedure SV_Init;

procedure SV_InitBaseSlot;

procedure SV_ClearRebornSlot;

function SV_RebornSlotAvailable: boolean;

procedure SV_MapTeleport(map: integer; position: integer);

procedure SV_LoadGame(slot: integer);

function SV_GetRebornSlot: integer;


procedure P_ArchiveScreenShot;

procedure P_UnArchiveScreenShot;
var
  SAVEGAMENAME: string = '%s\hex%d00.hxs';
  SAVEGLOBALSNAME: string = '%s\HEX%d00.HXW';
  SAVEGAMEMAP: string = '%s\hex%d%s.hxs';
  SAVEGAMENAMEDD: string = '%s\hexdd%d00.hxs';
  SAVEGAMEMAPDD: string = '%s\hexdd%d%s.hxs';
  SAVEGLOBALSNAMEDD: string = '%s\HEXDD%d00.HXW';
  SAVEPATH: string = 'HEXNDATA';

function SV_GetSaveGameName(const slot: integer): string;

function SV_GetSaveGameDescription(const slot: integer): string;

procedure SV_UpdateRebornSlot;

procedure SV_SaveGame(slot: integer; description: string);

implementation

uses
  d_delphi,
  d_ticcmd,
  d_think,
  d_player,
  d_main,
  g_game,
  m_fixed,
  mn_screenshot,
  info_h,
  info,
  i_system,
  i_tmp,
  tables,
  m_misc,
  m_argv,
  a_action,
  p_3dfloors,
  p_local,
  p_mobj_h,
  p_floor,
  p_setup,
  p_acs,
  p_maputl,
  p_spec,
  p_plats,
  p_ceilng,
  p_lights,
  p_doors,
  p_pspr_h,
  p_inter,
  p_tick,
  p_mobj,
  p_mobjlist,
  p_enemy,
  p_map,
  p_params,
  p_levelinfo,
  po_man,
  ps_main,
  psi_globals,
  psi_overlay,
  r_defs,
  r_main,
  s_sndseq,
  sb_bar,
  doomdef,
  w_wad,
  r_data,
  z_zone;

var
  saveptr: pointer;
  LOADVERSION: integer;

function _SAVEGAMENAME: string;
begin
  if hexdd_pack then
    Result := SAVEGAMENAMEDD
  else
    Result := SAVEGAMENAME
end;

function _SAVEGAMEMAP: string;
begin
  if hexdd_pack then
    Result := SAVEGAMEMAPDD
  else
    Result := SAVEGAMEMAP
end;

function _SAVEGLOBALSNAME: string;
begin
  if hexdd_pack then
    Result := SAVEGLOBALSNAMEDD
  else
    Result := SAVEGLOBALSNAME
end;

function GET_BYTE: byte; overload;
begin
  result := PByte(saveptr)^;
  saveptr := pointer(integer(saveptr) + SizeOf(byte));
end;

function GET_BYTE(b: PByte): byte; overload;
begin
  result := PByte(saveptr)^;
  b^ := result;
  saveptr := pointer(integer(saveptr) + SizeOf(byte));
end;

function GET_BOOLEAN: boolean;
begin
  result := PBoolean(saveptr)^;
  saveptr := pointer(integer(saveptr) + SizeOf(boolean));
end;

function GET_WORD: word;
begin
  result := PWord(saveptr)^;
  saveptr := pointer(integer(saveptr) + SizeOf(word));
end;

function GET_LONG: integer;
begin
  result := PInteger(saveptr)^;
  saveptr := pointer(integer(saveptr) + SizeOf(integer));
end;

function GET_LONGWORD: LongWord;
begin
  result := PLongWord(saveptr)^;
  saveptr := pointer(integer(saveptr) + SizeOf(LongWord));
end;

function GET_STRING: string;
var
  b: byte;
begin
  result := '';
  b := GET_BYTE;
  while b <> 0 do
  begin
    result := result + Chr(b);
    b := GET_BYTE;
  end;
end;

function GET_CHAR8T: char8_t;
var
  i: integer;
begin
  for i := 0 to 7 do
    result[i] := Chr(GET_BYTE);
end;

function GET_FLOAT: float;
begin
  result := Pfloat(saveptr)^;
  saveptr := pointer(integer(saveptr) + SizeOf(float));
end;

//==========================================================================
//
// Saving to stream helpers
//
//==========================================================================

var
  SavingFP: TFile;

//==========================================================================
//
// StreamOutBuffer
//
//==========================================================================

procedure StreamOutBuffer(buffer: pointer; size: integer);
begin
  SavingFP.Write(buffer^, size);
end;

//==========================================================================
//
// StreamOutByte
//
//==========================================================================

procedure StreamOutByte(val: byte);
begin
  SavingFP.Write(val, SizeOf(byte));
end;

//==========================================================================
//
// StreamOutByte
//
//==========================================================================

procedure StreamOutBoolean(val: boolean);
begin
  SavingFP.Write(val, SizeOf(boolean));
end;

//==========================================================================
//
// StreamOutWord
//
//==========================================================================

procedure StreamOutWord(val: word);
begin
  SavingFP.Write(val, SizeOf(word));
end;

//==========================================================================
//
// StreamOutLong
//
//==========================================================================

procedure StreamOutLong(val: integer);
begin
  SavingFP.Write(val, SizeOf(integer));
end;

//==========================================================================
//
// StreamOutLongWord
//
//==========================================================================

procedure StreamOutLongWord(val: LongWord);
begin
  SavingFP.Write(val, SizeOf(LongWord));
end;

//==========================================================================
//
// StreamOutString
//
//==========================================================================

procedure StreamOutString(const s: string);
var
  b: byte;
  i: integer;
begin
  for i := 1 to length(s) do
  begin
    b := Ord(s[i]);
    SavingFP.Write(b, SizeOf(byte));
  end;
  b := 0;
  SavingFP.Write(b, SizeOf(byte));
end;

procedure StreamOutChar8t(const s: char8_t);
var
  b: byte;
  i: integer;
begin
  for i := 0 to 7 do
  begin
    b := Ord(s[i]);
    SavingFP.Write(b, SizeOf(byte));
  end;
end;


//==========================================================================
//
// StreamOutFloat
//
//==========================================================================

procedure StreamOutFloat(val: float);
begin
  SavingFP.Write(val, SizeOf(float));
end;

const
  MAX_TARGET_PLAYERS = 512;
  MOBJ_NULL = -1;
  MOBJ_XX_PLAYER = -2;
  MAX_MAPS = 99;
  BASE_SLOT = 8;
  REBORN_SLOT = 9;
  REBORN_DESCRIPTION = 'TEMP GAME';
  MAX_THINKER_SIZE = 256;

const
  ASEG_GAME_HEADER = 101;
  ASEG_MAP_HEADER = 102;
  ASEG_WORLD = 103;
  ASEG_POLYOBJS = 104;
  ASEG_MOBJS = 105;
  ASEG_THINKERS = 106;
  ASEG_SCRIPTS = 107;
  ASEG_PLAYERS = 108;
  ASEG_SOUNDS = 109;
  ASEG_MISC = 110;
  ASEG_END = 111;

type
  thinkClass_t = (
    TC_NULL,
    TC_MOVE_CEILING,
    TC_VERTICAL_DOOR,
    TC_MOVE_FLOOR,
    TC_PLAT_RAISE,
    TC_INTERPRET_ACS,
    TC_FLOOR_WAGGLE,
    TC_LIGHT,
    TC_PHASE,
    TC_BUILD_PILLAR,
    TC_ROTATE_POLY,
    TC_MOVE_POLY,
    TC_POLY_DOOR
  );

type
  PArchieveFunc = procedure(p: pointer);

type
  thinkInfo_t = record
    tClass: thinkClass_t;
    thinkerFunc: think_t;
    mangleFunc: PArchieveFunc;
    restoreFunc: PArchieveFunc;
    size: integer;
  end;
  PthinkInfo_t = ^thinkInfo_t;

  ssthinker_t = record
    thinker: thinker_t;
    sector: Psector_t;
  end;
  Pssthinker_t = ^ssthinker_t;


type
  TargetPlayerAddrs_t = array[0..$FFFF] of PInteger;
  PTargetPlayerAddrs_t = ^TargetPlayerAddrs_t;

var
  MapMobjCount: integer;
  MapMobjList: Pmobj_tPArray;
  TargetPlayerAddrs: PTargetPlayerAddrs_t;
  TargetPlayerCount: integer;
  SaveBuffer: PByteArray;
  SavingPlayers: boolean;

//==========================================================================
//
// GetMobjNum
//
//==========================================================================

function GetMobjNum(mobj: Pmobj_t): integer;
begin
  if mobj = nil then
  begin
    result := MOBJ_NULL;
    exit;
  end;

  if (mobj.player <> nil) and not SavingPlayers then
  begin
    result := MOBJ_XX_PLAYER;
    exit;
  end;

  result := mobj.archiveNum;
end;

//==========================================================================
//
// SetMobjPtr
//
//==========================================================================

procedure SetMobjPtr(archiveNum: PInteger);
begin
  if archiveNum^ = MOBJ_NULL then
  begin
    archiveNum^ := 0;
    exit;
  end;

  if archiveNum^ = MOBJ_XX_PLAYER then
  begin
    if TargetPlayerCount = MAX_TARGET_PLAYERS then
      I_Error('RestoreMobj(): exceeded MAX_TARGET_PLAYERS');

    TargetPlayerAddrs[TargetPlayerCount] := archiveNum;
    inc(TargetPlayerCount);
    archiveNum^ := 0;
    exit;
  end;
  if archiveNum^ < MapMobjCount then
    archiveNum^ := integer(MapMobjList[archiveNum^])
  else
    archiveNum^ := 0;
end;


//==========================================================================
//
// RestoreMobj
//
//==========================================================================

procedure RestoreMobj(mobj: Pmobj_t);
begin
  mobj.state := @states[integer(mobj.state)];
  mobj.validcount := 0;
  mobj.lightvalidcount := 0;
  mobj.rendervalidcount := 0;
  if mobj.player <> nil then
  begin
    mobj.player := @players[integer(mobj.player) - 1];
    Pplayer_t(mobj.player).mo := mobj;
  end;
  P_SetThingPosition(mobj);
  mobj.info := @mobjinfo[mobj._type];
  mobj.floorz := P_3dFloorHeight(mobj);
  mobj.ceilingz := P_3dCeilingHeight(mobj);
  SetMobjPtr(PInteger(@mobj.target));
  SetMobjPtr(PInteger(@mobj.tracer));
  SetMobjPtr(PInteger(@mobj.master));
  case mobj._type of
    Ord(MT_KORAX_SPIRIT1),
    Ord(MT_KORAX_SPIRIT2),
    Ord(MT_KORAX_SPIRIT3),
    Ord(MT_KORAX_SPIRIT4),
    Ord(MT_KORAX_SPIRIT5),
    Ord(MT_KORAX_SPIRIT6),
    // Just special1
    Ord(MT_BISH_FX),
    Ord(MT_HOLY_FX),
    Ord(MT_DRAGON),
    Ord(MT_THRUSTFLOOR_UP),
    Ord(MT_THRUSTFLOOR_DOWN),
    Ord(MT_MINOTAUR),
    Ord(MT_SORCFX1):
      begin
        SetMobjPtr(@mobj.special1);
      end;

    // Just special2
    Ord(MT_LIGHTNING_FLOOR),
    Ord(MT_LIGHTNING_ZAP):
      begin
        SetMobjPtr(@mobj.special2);
      end;

    // Both special1 and special2
    Ord(MT_HOLY_TAIL),
    Ord(MT_LIGHTNING_CEILING):
      begin
        SetMobjPtr(@mobj.special1);
        SetMobjPtr(@mobj.special2);
      end;
  end;
end;

//==========================================================================
//
// MangleSSThinker
//
//==========================================================================

procedure MangleSSThinker(sst: Pssthinker_t);
begin
  sst.sector := Psector_t((longword(sst.sector) - longword(@sectors[0])) div SizeOf(sector_t));
end;

//==========================================================================
//
// RestoreSSThinker
//
//==========================================================================

procedure RestoreSSThinker(sst: Pssthinker_t);
begin
  sst.sector := @sectors[integer(sst.sector)];
  sst.sector.specialdata := @sst.thinker._function.acp1;
end;

//==========================================================================
//
// RestoreSSThinkerNoSD
//
//==========================================================================

procedure RestoreSSThinkerNoSD(sst: Pssthinker_t);
begin
  sst.sector := @sectors[integer(sst.sector)];
end;

//==========================================================================
//
// MangleScript
//
//==========================================================================

procedure MangleScript(script: Pacs_t);
begin
  script.ip := PInteger(longword(script.ip) - longword(ActionCodeBase));
  if script.line <> nil then
    script.line :=  Pline_t((longword(script.line) - longword(@lines[0])) div SizeOf(line_t))
  else
    script.line := Pline_t(-1);
  script.activator := Pmobj_t(GetMobjNum(script.activator));
end;

//==========================================================================
//
// RestoreScript
//
//==========================================================================

procedure RestoreScript(script: Pacs_t);
begin
  script.ip := @ActionCodeBase[integer(script.ip)];
  if integer(script.line) = -1 then
    script.line := nil
  else
    script.line := @lines[integer(script.line)];
  SetMobjPtr(PInteger(@script.activator));
end;

//==========================================================================
//
// RestorePlatRaise
//
//==========================================================================

procedure RestorePlatRaise(plat: Pplat_t);
begin
  plat.sector := @sectors[integer(plat.sector)];
  plat.sector.specialdata := @T_PlatRaise;
  P_AddActivePlat(plat);
end;

//==========================================================================
//
// RestoreMoveCeiling
//
//==========================================================================

procedure RestoreMoveCeiling(ceiling: Pceiling_t);
begin
  ceiling.sector := @sectors[integer(ceiling.sector)];
  ceiling.sector.specialdata := @T_MoveCeiling;
  P_AddActiveCeiling(ceiling);
end;


const
  SVNUMTHINKINFO = 13;

var
  ThinkerInfo: array[0..SVNUMTHINKINFO - 1] of thinkInfo_t;

procedure SV_Init;
var
  pti: PthinkInfo_t;
begin
// This list has been prioritized using frequency estimates
  pti := @ThinkerInfo[0];
  pti.tClass := TC_MOVE_FLOOR;
  pti.thinkerFunc.acp1 := @T_MoveFloor;
  pti.mangleFunc := @MangleSSThinker;
  pti.restoreFunc := @RestoreSSThinker;
  pti.size := SizeOf(floormove_t);

  inc(pti);
  pti.tClass := TC_PLAT_RAISE;
  pti.thinkerFunc.acp1 := @T_PlatRaise;
  pti.mangleFunc := @MangleSSThinker;
  pti.restoreFunc := @RestorePlatRaise;
  pti.size := SizeOf(plat_t);

  inc(pti);
  pti.tClass := TC_MOVE_CEILING;
  pti.thinkerFunc.acp1 :=  @T_MoveCeiling;
  pti.mangleFunc := @MangleSSThinker;
  pti.restoreFunc := @RestoreMoveCeiling;
  pti.size := SizeOf(ceiling_t);

  inc(pti);
  pti.tClass := TC_LIGHT;
  pti.thinkerFunc.acp1 := @T_Light;
  pti.mangleFunc := @MangleSSThinker;
  pti.restoreFunc := @RestoreSSThinkerNoSD;
  pti.size := SizeOf(light_t);

  inc(pti);
  pti.tClass := TC_VERTICAL_DOOR;
  pti.thinkerFunc.acp1 :=  @T_VerticalDoor;
  pti.mangleFunc := @MangleSSThinker;
  pti.restoreFunc := @RestoreSSThinker;
  pti.size := SizeOf(vldoor_t);

  inc(pti);
  pti.tClass := TC_PHASE;
  pti.thinkerFunc.acp1 := @T_Phase;
  pti.mangleFunc := @MangleSSThinker;
  pti.restoreFunc := @RestoreSSThinkerNoSD;
  pti.size := SizeOf(phase_t);

  inc(pti);
  pti.tClass := TC_INTERPRET_ACS;
  pti.thinkerFunc.acp1 :=  @T_InterpretACS;
  pti.mangleFunc := @MangleScript;
  pti.restoreFunc := @RestoreScript;
  pti.size := SizeOf(acs_t);

  inc(pti);
  pti.tClass := TC_ROTATE_POLY;
  pti.thinkerFunc.acp1 :=  @T_RotatePoly;
  pti.mangleFunc := nil;
  pti.restoreFunc := nil;
  pti.size := SizeOf(polyevent_t);

  inc(pti);
  pti.tClass := TC_BUILD_PILLAR;
  pti.thinkerFunc.acp1 :=  @T_BuildPillar;
  pti.mangleFunc := @MangleSSThinker;
  pti.restoreFunc := @RestoreSSThinker;
  pti.size := SizeOf(pillar_t);

  inc(pti);
  pti.tClass := TC_MOVE_POLY;
  pti.thinkerFunc.acp1 :=  @T_MovePoly;
  pti.mangleFunc := nil;
  pti.restoreFunc := nil;
  pti.size := SizeOf(polyevent_t);

  inc(pti);
  pti.tClass := TC_POLY_DOOR;
  pti.thinkerFunc.acp1 :=  @T_PolyDoor;
  pti.mangleFunc := nil;
  pti.restoreFunc := nil;
  pti.size := SizeOf(polydoor_t);

  inc(pti);
  pti.tClass := TC_FLOOR_WAGGLE;
  pti.thinkerFunc.acp1 :=  @T_FloorWaggle;
  pti.mangleFunc := @MangleSSThinker;
  pti.restoreFunc := @RestoreSSThinker;
  pti.size := SizeOf(floorWaggle_t);

  inc(pti);
  pti.tClass := TC_NULL;
  pti.thinkerFunc.acp1 :=  nil;
  pti.mangleFunc := nil;
  pti.restoreFunc := nil;
  pti.size := 0;

end;

//==========================================================================
//
// OpenStreamOut
//
//==========================================================================

procedure OpenStreamOut(const fileName: string);
begin
  SavingFP := TFile.Create(fileName, fCreate);
end;

//==========================================================================
//
// CloseStreamOut
//
//==========================================================================

procedure CloseStreamOut;
begin
  if SavingFP <> nil then
  begin
    SavingFP.Free;
    SavingFP := nil;
  end;
end;


//==========================================================================
//
// ArchivePlayers
//
//==========================================================================

procedure ArchivePlayers;
var
  i: integer;
  j: integer;
  tempPlayer: player_t;
begin
  StreamOutLong(ASEG_PLAYERS);
  for i := 0 to MAXPLAYERS - 1 do
    StreamOutBoolean(playeringame[i]);
  for i := 0 to MAXPLAYERS - 1 do
  begin
    if not playeringame[i] then
      continue;
    StreamOutByte(Ord(PlayerClass[i]));
    tempPlayer := players[i];
    for j := 0 to Ord(NUMPSPRITES) - 1 do
    begin
      if tempPlayer.psprites[j].state <> nil then
      begin
        tempPlayer.psprites[j].state := Pstate_t((longword(tempPlayer.psprites[j].state) - longword(@states[0])) div SizeOf(state_t));
      end;
    end;
    StreamOutBuffer(@tempPlayer, SizeOf(player_t));
  end;
end;

//==========================================================================
//
// AssertSegment
//
//==========================================================================

procedure AssertSegment(segType: integer);
begin
  if GET_LONG <> segType then
    I_Error('AssertSegment(): Corrupted save game: Segment [%d] failed alignment check', [segType]);
end;


//==========================================================================
//
// UnarchivePlayers
//
//==========================================================================

procedure UnarchivePlayers;
var
  i, j: integer;
begin
  AssertSegment(ASEG_PLAYERS);
  for i := 0 to MAXPLAYERS - 1 do
    playeringame[i] := GET_BOOLEAN;

  for i := 0 to MAXPLAYERS - 1 do
  begin
    if not playeringame[i] then
    begin
      continue;
    end;
    PlayerClass[i] := pclass_t(GET_BYTE);
    if LOADVERSION <= VERSION141 then
    begin
      ZeroMemory(@players[i], SizeOf(player_t));
      memcpy(@players[i], saveptr, SizeOf(player_t141));
      players[i].laddertics := 0;
      players[i].viewbob := players[i].bob;
      players[i].slopetics := 0; // JVAL: Slopes
      players[i].oldviewz := players[i].viewz;
      players[i].teleporttics := 0;
      players[i].quaketics := 0;
      players[i].quakeintensity := 0;
      players[i].lookdir16 := players[i].lookdir * 16; // JVAL Smooth Look Up/Down
      Pticcmd_t202(@players[i].cmd)^ := players[i].cmd202;
      players[i].cmd.lookupdown16 := (players[i].cmd.lookfly and 15) * 256;
      incp(saveptr, SizeOf(player_t141));
    end
    else if LOADVERSION <= VERSION142 then
    begin
      ZeroMemory(@players[i], SizeOf(player_t));
      memcpy(@players[i], saveptr, SizeOf(player_t142));
      if players[i].quaketics > 0 then
        players[i].quakeintensity := FRACUNIT
      else
        players[i].quakeintensity := 0;
      players[i].lookdir16 := players[i].lookdir * 16; // JVAL Smooth Look Up/Down
      Pticcmd_t202(@players[i].cmd)^ := players[i].cmd202;
      players[i].cmd.lookupdown16 := (players[i].cmd.lookfly and 15) * 256;
      incp(saveptr, SizeOf(player_t142));
    end
    else if LOADVERSION <= VERSION205 then
    begin
      ZeroMemory(@players[i], SizeOf(player_t));
      memcpy(@players[i], saveptr, SizeOf(player_t205));
      if players[i].quaketics > 0 then
        players[i].quakeintensity := FRACUNIT
      else
        players[i].quakeintensity := 0;
      incp(saveptr, SizeOf(player_t205));
    end
    else if LOADVERSION <= VERSION206 then
    begin
      ZeroMemory(@players[i], SizeOf(player_t));
      memcpy(@players[i], saveptr, SizeOf(player_t206));
      incp(saveptr, SizeOf(player_t206));
    end
    else
    begin
      memcpy(@players[i], saveptr, SizeOf(player_t));
      incp(saveptr, SizeOf(player_t));
    end;
    players[i].mo := nil; // Will be set when unarc thinker
    P_ClearMessage(@players[i]);
    players[i].attacker := nil;
    players[i].poisoner := nil;
    for j := 0 to Ord(NUMPSPRITES) - 1 do
    begin
      if players[i].psprites[j].state <> nil then
      begin
        players[i].psprites[j].state := @states[integer(players[i].psprites[j].state)];
      end;
    end;
  end;
end;

//==========================================================================
//
// ArchiveWorld
//
//==========================================================================

procedure ArchiveWorld;
var
  i: integer;
  j: integer;
  sec: Psector_t;
  li: Pline_t;
  si: Pside_t;
  levelinf: Plevelinfo_t;
begin
  StreamOutLong(ASEG_WORLD);

  levelinf := P_GetLevelInfo(P_GetMapName(gamemap));
  StreamOutChar8t(levelinf.musname);
  StreamOutChar8t(levelinf.skyflat);

  sec := @sectors[0];
  for i := 0 to numsectors - 1 do
  begin
    StreamOutLong(sec.floorheight);
    StreamOutLong(sec.ceilingheight);
    StreamOutChar8t(flats[sec.floorpic].name);
    StreamOutChar8t(flats[sec.ceilingpic].name);
    StreamOutWord(sec.lightlevel);
    StreamOutWord(sec.special);
    StreamOutWord(sec.tag);
    StreamOutByte(Ord(sec.seqType));
    StreamOutLongWord(sec.renderflags);
    StreamOutLongWord(sec.flags);
    StreamOutLong(sec.midsec);
    StreamOutLong(sec.midline);
    StreamOutLong(sec.gravity);

    // JVAL: 20200221 - Texture angle
    StreamOutLongWord(sec.floorangle);
    StreamOutLong(sec.flooranglex);
    StreamOutLong(sec.floorangley);
    StreamOutLongWord(sec.ceilingangle);
    StreamOutLong(sec.ceilinganglex);
    StreamOutLong(sec.ceilingangley);

    // JVAL: 20200522 - Slope values
    StreamOutFloat(sec.fa);
    StreamOutFloat(sec.fb);
    StreamOutFloat(sec.fd);
    StreamOutFloat(sec.fic);
    StreamOutFloat(sec.ca);
    StreamOutFloat(sec.cb);
    StreamOutFloat(sec.cd);
    StreamOutFloat(sec.cic);

    StreamOutLong(sec.num_saffectees);
    for j := 0 to sec.num_saffectees - 1 do
      StreamOutLong(sec.saffectees[j]);
    inc(sec);
  end;
  li := @lines[0];
  for i := 0 to numlines -  1 do
  begin
    StreamOutWord(li.flags);
    StreamOutByte(li.special);
    StreamOutByte(li.arg1);
    StreamOutByte(li.arg2);
    StreamOutByte(li.arg3);
    StreamOutByte(li.arg4);
    StreamOutByte(li.arg5);
    for j := 0 to 1 do
    begin
      if li.sidenum[j] = -1 then
        continue;

      si := @sides[li.sidenum[j]];
      StreamOutLong(si.textureoffset);
      StreamOutLong(si.rowoffset);
      StreamOutChar8t(R_NameForSideTexture(si.toptexture));
      StreamOutChar8t(R_NameForSideTexture(si.bottomtexture));
      StreamOutChar8t(R_NameForSideTexture(si.midtexture));
    end;
    inc(li);
  end;
end;

//==========================================================================
//
// UnarchiveWorld
//
//==========================================================================

procedure UnarchiveWorld;
var
  i: integer;
  j: integer;
  sec: Psector_t;
  li: Pline_t;
  si: Pside_t;
  levelinf: Plevelinfo_t;
begin
  AssertSegment(ASEG_WORLD);

  if LOADVERSION >= VERSION205 then
  begin
    levelinf := P_GetLevelInfo(P_GetMapName(gamemap));
    levelinf.musname := GET_CHAR8T;
    levelinf.skyflat := GET_CHAR8T;
  end;

  sec := @sectors[0];
  for i := 0 to numsectors - 1 do
  begin
    sec.floorheight := GET_LONG;
    sec.ceilingheight := GET_LONG;
    if LOADVERSION <= VERSION141 then
    begin
      sec.floorpic := GET_WORD;
      sec.ceilingpic := GET_WORD;
    end
    else
    begin
      sec.floorpic := R_FlatNumForName(GET_CHAR8T);
      sec.ceilingpic := R_FlatNumForName(GET_CHAR8T);
    end;
    sec.lightlevel := GET_WORD;
    sec.special := GET_WORD;
    sec.tag := GET_WORD;
    sec.seqType := seqtype_t(GET_BYTE);
    if LOADVERSION >= VERSION142 then
    begin
      sec.renderflags := GET_LONGWORD;
      sec.flags := GET_LONGWORD;
      sec.midsec := GET_LONGWORD;
      sec.midline := GET_LONGWORD;
    end
    else
    begin
      sec.midsec := -1;
      sec.midline := -1;
    end;
    if LOADVERSION >= VERSION204 then
      sec.gravity := GET_LONGWORD
    else
      sec.gravity := GRAVITY;
    if LOADVERSION >= VERSION206 then
    begin
      sec.floorangle := GET_LONGWORD;
      sec.flooranglex := GET_LONG;
      sec.floorangley := GET_LONG;
      sec.ceilingangle := GET_LONGWORD;
      sec.ceilinganglex := GET_LONG;
      sec.ceilingangley := GET_LONG;

      // JVAL: 20200522 - Slope values
      sec.fa := GET_FLOAT;
      sec.fb := GET_FLOAT;
      sec.fd := GET_FLOAT;
      sec.fic := GET_FLOAT;
      sec.ca := GET_FLOAT;
      sec.cb := GET_FLOAT;
      sec.cd := GET_FLOAT;
      sec.cic := GET_FLOAT;
    end
    else
    begin
      sec.floorangle := 0;
      sec.flooranglex := 0;
      sec.floorangley := 0;
      sec.ceilingangle := 0;
      sec.ceilinganglex := 0;
      sec.ceilingangley := 0;
    end;

    if LOADVERSION >= VERSION142 then
    begin
      sec.num_saffectees := GET_LONG;
      sec.saffectees := Z_Realloc(sec.saffectees, sec.num_saffectees * SizeOf(integer), PU_LEVEL, nil);
      for j := 0 to sec.num_saffectees - 1 do
        sec.saffectees[j] := GET_LONG;
    end;
    sec.specialdata := nil;
    sec.soundtarget := nil;
    sec.iSectorID := i;
    inc(sec);
  end;
  li := @lines[0];
  for i := 0 to numlines - 1 do
  begin
    li.flags := GET_WORD;
    li.special := GET_BYTE;
    li.arg1 := GET_BYTE;
    li.arg2 := GET_BYTE;
    li.arg3 := GET_BYTE;
    li.arg4 := GET_BYTE;
    li.arg5 := GET_BYTE;
    for j := 0 to 1 do
    begin
      if li.sidenum[j] = -1 then
      begin
        continue;
      end;
      si := @sides[li.sidenum[j]];
      si.textureoffset := GET_LONG;
      si.rowoffset := GET_LONG;
      if LOADVERSION <= VERSION141 then
      begin
        si.toptexture := GET_WORD;
        si.bottomtexture := GET_WORD;
        si.midtexture := GET_WORD;
      end
      else
      begin
        si.toptexture := R_SafeTextureNumForName(GET_CHAR8T);
        si.bottomtexture := R_SafeTextureNumForName(GET_CHAR8T);
        si.midtexture := R_SafeTextureNumForName(GET_CHAR8T);
      end;
    end;
    inc(li);
  end;
end;

//==========================================================================
//
// SetMobjArchiveNums
//
// Sets the archive numbers in all mobj structs.  Also sets the MobjCount
// global.  Ignores player mobjs if SavingPlayers is false.
//
//==========================================================================

procedure SetMobjArchiveNums;
var
  mobj: Pmobj_t;
  thinker: Pthinker_t;
begin
  MapMobjCount := 0;
  thinker := thinkercap.next;
  while thinker <> @thinkercap do
  begin
    if @thinker._function.acp1 = @P_MobjThinker then
    begin
      mobj := Pmobj_t(thinker);
      if (mobj.player <> nil) and not SavingPlayers then
      begin // Skipping player mobjs
        thinker := thinker.next;
        continue;
      end;
      mobj.archiveNum := MapMobjCount;
      inc(MapMobjCount);
    end;
    thinker := thinker.next;
  end;
end;

//==========================================================================
//
// MangleMobj
//
//==========================================================================

procedure MangleMobj(mobj: Pmobj_t);
var
  corpse: boolean;
begin
  corpse := mobj.flags and MF_CORPSE <> 0;
  mobj.state := Pstate_t((longword(mobj.state) - longword(@states[0])) div SizeOf(state_t));
  if mobj.player <> nil then
  begin
    mobj.player := pointer((longword(mobj.player) - longword(@players[0])) div SizeOf(player_t) + 1);
  end;
  if corpse then
  begin
    mobj.target := Pmobj_t(MOBJ_NULL);
    mobj.tracer := Pmobj_t(MOBJ_NULL);
    mobj.master := Pmobj_t(MOBJ_NULL);
  end
  else
  begin
    mobj.target := Pmobj_t(GetMobjNum(mobj.target));
    mobj.tracer := Pmobj_t(GetMobjNum(mobj.tracer));
    mobj.master := Pmobj_t(GetMobjNum(mobj.master));
  end;
  case mobj._type of
    Ord(MT_KORAX_SPIRIT1),
    Ord(MT_KORAX_SPIRIT2),
    Ord(MT_KORAX_SPIRIT3),
    Ord(MT_KORAX_SPIRIT4),
    Ord(MT_KORAX_SPIRIT5),
    Ord(MT_KORAX_SPIRIT6),
    // Just special1
    Ord(MT_BISH_FX),
    Ord(MT_HOLY_FX),
    Ord(MT_DRAGON),
    Ord(MT_THRUSTFLOOR_UP),
    Ord(MT_THRUSTFLOOR_DOWN),
    Ord(MT_MINOTAUR),
    Ord(MT_SORCFX1),
    Ord(MT_MSTAFF_FX2):
      begin
        if corpse then
        begin
          mobj.special1 := MOBJ_NULL;
        end
        else
        begin
          mobj.special1 := GetMobjNum(Pmobj_t(mobj.special1));
        end;
      end;

    // Just special2
    Ord(MT_LIGHTNING_FLOOR),
    Ord(MT_LIGHTNING_ZAP):
      begin
        if corpse then
        begin
          mobj.special2 := MOBJ_NULL;
        end
        else
        begin
          mobj.special2 := GetMobjNum(Pmobj_t(mobj.special2));
        end;
      end;

    // Both special1 and special2
    Ord(MT_HOLY_TAIL),
    Ord(MT_LIGHTNING_CEILING):
      begin
        if corpse then
        begin
          mobj.special1 := MOBJ_NULL;
          mobj.special2 := MOBJ_NULL;
        end
        else
        begin
          mobj.special1 := GetMobjNum(Pmobj_t(mobj.special1));
          mobj.special2 := GetMobjNum(Pmobj_t(mobj.special2));
        end;
      end;

    // Miscellaneous
    Ord(MT_KORAX):
      begin
        mobj.special1 := 0; // Searching index
      end;
  end;
end;

//==========================================================================
//
// ArchiveMobjs
//
//==========================================================================

procedure ArchiveMobjs;
var
  count: integer;
  thinker: Pthinker_t;
  tempMobj: mobj_t;
  parm: Pmobjcustomparam_t;
begin
  StreamOutLong(ASEG_MOBJS);
  StreamOutLong(MapMobjCount);
  count := 0;
  thinker := thinkercap.next;
  while thinker <> @thinkercap do
  begin
    if @thinker._function.acp1 <> @P_MobjThinker then
    begin // Not a mobj thinker
      thinker := thinker.next;
      continue;
    end;
    if (Pmobj_t(thinker).player <> nil) and not SavingPlayers then
    begin // Skipping player mobjs
      thinker := thinker.next;
      continue;
    end;
    inc(count);
    memcpy(@tempMobj, thinker, SizeOf(mobj_t));
    MangleMobj(@tempMobj);
    StreamOutBuffer(@tempMobj, SizeOf(mobj_t));

    parm := tempMobj.customparams;
    while parm <> nil do
    begin
      StreamOutBuffer(parm, SizeOf(mobjcustomparam_t));
      parm := parm.next;
    end;

    thinker := thinker.next;
  end;
  if count <> MapMobjCount then
  begin
    I_Error('ArchiveMobjs(): bad mobj count');
  end;
end;

//==========================================================================
//
// UnarchiveMobjs
//
//==========================================================================

procedure UnarchiveMobjs;
var
  i: integer;
  mobj: Pmobj_t;
  parm: mobjcustomparam_t;
begin
  AssertSegment(ASEG_MOBJS);
  TargetPlayerAddrs := Z_Malloc(MAX_TARGET_PLAYERS * SizeOf(PInteger), PU_STATIC, nil);
  TargetPlayerCount := 0;
  MapMobjCount := GET_LONG;
  MapMobjList := Z_Malloc(MapMobjCount * SizeOf(Pmobj_t), PU_STATIC, nil);
  for i := 0 to MapMobjCount - 1 do
  begin
    MapMobjList[i] := Z_Malloc(SizeOf(mobj_t), PU_LEVEL, nil);
  end;
  for i := 0 to MapMobjCount - 1 do
  begin
    mobj := MapMobjList[i];
    if LOADVERSION = VERSION140 then
    begin
      memcpy(mobj, saveptr, SizeOf(mobj_t140));
      incp(saveptr, SizeOf(mobj_t140));
      mobj.prevx := mobj.x;
      mobj.prevy := mobj.y;
      mobj.prevz := mobj.z;
      mobj.nextx := mobj.x;
      mobj.nexty := mobj.y;
      mobj.nextz := mobj.z;
      mobj.prevangle := mobj.angle;
      mobj.nextangle := mobj.angle;
      mobj.intrplcnt := 0;
      mobj.key := 0;
      mobj.customparams := nil;

      mobj.dropitem := 0;

      mobj.lightvalidcount := 0;
      mobj.scale := FRACUNIT;
      mobj.pushfactor := FRACUNIT div 4;
      mobj.gravity := FRACUNIT;
      mobj.flags3_ex := 0;
      mobj.flags4_ex := 0;
      mobj.rendervalidcount := 0;

      mobj.mass := mobjinfo[Ord(mobj._type)].mass;
      mobj.WeaveIndexXY := 0;
      mobj.WeaveIndexZ := 0;
      mobj.master := nil;

      mobj.friction := ORIG_FRICTION;

      // version 207
      mobj.painchance := mobjinfo[Ord(mobj._type)].painchance;
      mobj.spriteDX := mobjinfo[Ord(mobj._type)].spriteDX;
      mobj.spriteDY := mobjinfo[Ord(mobj._type)].spriteDY;
    end
    else if LOADVERSION = VERSION141 then
    begin
      memcpy(mobj, saveptr, SizeOf(mobj_t141));
      incp(saveptr, SizeOf(mobj_t141));

      mobj.dropitem := 0;

      mobj.lightvalidcount := 0;
      mobj.scale := FRACUNIT;
      mobj.pushfactor := FRACUNIT div 4;
      mobj.gravity := FRACUNIT;
      mobj.flags3_ex := 0;
      mobj.flags4_ex := 0;
      mobj.rendervalidcount := 0;

      mobj.mass := mobjinfo[Ord(mobj._type)].mass;
      mobj.WeaveIndexXY := 0;
      mobj.WeaveIndexZ := 0;
      mobj.master := nil;

      mobj.friction := ORIG_FRICTION;

      // version 207
      mobj.painchance := mobjinfo[Ord(mobj._type)].painchance;
      mobj.spriteDX := mobjinfo[Ord(mobj._type)].spriteDX;
      mobj.spriteDY := mobjinfo[Ord(mobj._type)].spriteDY;
    end
    else if LOADVERSION <= VERSION204 then
    begin
      memcpy(mobj, saveptr, SizeOf(mobj_t204));
      incp(saveptr, SizeOf(mobj_t204));

      mobj.lightvalidcount := 0;
      mobj.scale := FRACUNIT;
      mobj.pushfactor := FRACUNIT div 4;
      mobj.gravity := FRACUNIT;
      mobj.flags3_ex := 0;
      mobj.flags4_ex := 0;
      mobj.rendervalidcount := 0;

      mobj.mass := mobjinfo[Ord(mobj._type)].mass;
      mobj.WeaveIndexXY := 0;
      mobj.WeaveIndexZ := 0;
      mobj.master := nil;

      mobj.friction := ORIG_FRICTION;

      // version 207
      mobj.painchance := mobjinfo[Ord(mobj._type)].painchance;
      mobj.spriteDX := mobjinfo[Ord(mobj._type)].spriteDX;
      mobj.spriteDY := mobjinfo[Ord(mobj._type)].spriteDY;
    end
    else if LOADVERSION <= VERSION205 then
    begin
      memcpy(mobj, saveptr, SizeOf(mobj_t205));
      incp(saveptr, SizeOf(mobj_t205));

      mobj.mass := mobjinfo[Ord(mobj._type)].mass;
      mobj.master := nil;

      mobj.friction := ORIG_FRICTION;

      // version 207
      mobj.painchance := mobjinfo[Ord(mobj._type)].painchance;
      mobj.spriteDX := mobjinfo[Ord(mobj._type)].spriteDX;
      mobj.spriteDY := mobjinfo[Ord(mobj._type)].spriteDY;
    end
    else if LOADVERSION <= VERSION206 then
    begin
      memcpy(mobj, saveptr, SizeOf(mobj_t206));
      incp(saveptr, SizeOf(mobj_t206));

      // version 207
      mobj.painchance := mobjinfo[Ord(mobj._type)].painchance;
      mobj.spriteDX := mobjinfo[Ord(mobj._type)].spriteDX;
      mobj.spriteDY := mobjinfo[Ord(mobj._type)].spriteDY;
    end
    else
    begin
      memcpy(mobj, saveptr, SizeOf(mobj_t));
      incp(saveptr, SizeOf(mobj_t));
    end;
    mobj.thinker._function.acp1 := @P_MobjThinker;

    if mobj.key < 1 then
      mobj.key := P_GenGlobalMobjKey;
    P_NotifyMobjKey(mobj);

    if mobj.customparams <> nil then
    begin
      mobj.customparams := nil;
      repeat
        memcpy(@parm, saveptr, SizeOf(mobjcustomparam_t));
        incp(saveptr, SizeOf(mobjcustomparam_t));
        P_SetMobjCustomParam(mobj, parm.name, parm.value);
      until parm.next = nil;
    end;

    RestoreMobj(mobj);
    P_AddThinker(@mobj.thinker);
  end;
  P_CreateTIDList;
  P_InitCreatureCorpseQueue(true); // true := scan for corpses
end;

//==========================================================================
//
// ArchiveThinkers
//
//==========================================================================

procedure ArchiveThinkers;
var
  thinker: Pthinker_t;
  info: PthinkInfo_t;
  buffer: array[0..MAX_THINKER_SIZE - 1] of byte;
begin
  StreamOutLong(ASEG_THINKERS);
  thinker := thinkercap.next;
  while thinker <> @thinkercap do
  begin
    if @thinker._function.acp1 <> @P_MobjThinker then // JVAL Speed up, leave out P_MobjThinker
    begin
      info := @ThinkerInfo[0];
      while info.tClass <> TC_NULL do
      begin
        if @thinker._function.acp1 = @info.thinkerFunc.acp1 then
        begin
          StreamOutByte(Ord(info.tClass));
          memcpy(@buffer, thinker, info.size);
          if Assigned(info.mangleFunc) then
          begin
            info.mangleFunc(@buffer);
          end;
          StreamOutBuffer(@buffer, info.size);
          break;
        end;
        inc(info);
      end;
    end;
    thinker := thinker.next;
  end;
  // Add a termination marker
  StreamOutByte(Ord(TC_NULL));
end;

//==========================================================================
//
// UnarchiveThinkers
//
//==========================================================================

procedure UnarchiveThinkers;
var
  tClass: byte;
  thinker: Pthinker_t;
  info: PthinkInfo_t;
begin
  AssertSegment(ASEG_THINKERS);
  while GET_BYTE(@tClass) <> byte(TC_NULL) do
  begin
    info := @ThinkerInfo[0];
    while info.tClass <> TC_NULL do
    begin
      if thinkClass_t(tClass) = info.tClass then
      begin
        thinker := Z_Malloc(info.size, PU_LEVEL, nil);
        memcpy(thinker, saveptr, info.size);
        incp(saveptr, info.size);
        thinker._function.acp1 := @info.thinkerFunc.acp1;
        if Assigned(info.restoreFunc) then
          info.restoreFunc(thinker);
        P_AddThinker(thinker);
        break;
      end;
      inc(info);
    end;
    if info.tClass = TC_NULL then
      I_Error('UnarchiveThinkers: Unknown tClass %d in savegame', [tClass]);
  end;
end;

procedure SV_ArchiveGlobalVariables(const vars: TGlobalVariablesList);
var
  len: integer;
  pp: pointer;
  ppt: pointer;
begin
  len := vars.StructureSize;
  pp := malloc(len + SizeOf(integer));
  PInteger(pp)^ := len;
  ppt := pp;
  incp(pointer(ppt), SizeOf(integer));
  vars.SaveToBuffer(ppt);
  StreamOutBuffer(pp, len + SizeOf(integer));
  memfree(pp, len + SizeOf(integer));
end;

procedure SV_UnArchiveGlobalVariables(const vars: TGlobalVariablesList);
var
  len: integer;
begin
  if LOADVERSION < VERSION142 then
    Exit;

  len := PInteger(saveptr)^;
  incp(saveptr, SizeOf(integer));
  vars.LoadFromBuffer(saveptr);
  incp(saveptr, len);
end;

procedure SV_ArchivePSMapScript;
var
  fname: string;
  sz: Integer;
  pp: pointer;
begin
  fname := I_NewTempFile('mapscript' + itoa(Random(1000)));

  PS_MapScriptSaveToFile(fname);
  sz := fsize(fname);
  StreamOutLong(sz);
  pp := malloc(sz);
  with TFile.Create(fname, fOpenReadOnly) do
  try
    Read(pp^, sz);
  finally
    Free;
  end;
  fdelete(fname);
  StreamOutBuffer(pp, sz);
  memfree(pp, sz);
end;

procedure SV_UnArchivePSMapScript;
var
  fname: string;
  sz: Integer;
begin
  if LOADVERSION < VERSION142 then
    Exit;

  sz := PInteger(saveptr)^;
  incp(Pointer(saveptr), SizeOf(Integer));

  fname := I_NewTempFile('mapscript' + itoa(Random(1000)));
  with TFile.Create(fname, fCreate) do
  try
    Write(saveptr^, sz);
  finally
    Free;
  end;
  PS_MapScriptLoadFromFile(fname);
  fdelete(fname);
  incp(Pointer(saveptr), sz);
end;

procedure SV_ArchiveOverlay;
var
  buf, buf1: pointer;
  sz: integer;
begin
  sz := overlay.SaveSize;
  buf := malloc(sz);
  buf1 := buf;
  overlay.SaveToBuffer(Pointer(buf));
  StreamOutBuffer(buf1, sz);
  memfree(buf1, sz);
end;

procedure SV_UnArchiveOverlay;
begin
  if LOADVERSION < VERSION142 then
    Exit;

  overlay.LoadFromBuffer(Pointer(saveptr));
end;

//==========================================================================
//
// ArchiveScripts
//
//==========================================================================

procedure ArchiveScripts;
var
  i: integer;
begin
  StreamOutLong(ASEG_SCRIPTS);
  for i := 0 to ACScriptCount - 1 do
  begin
    StreamOutByte(Ord(ACSInfo[i].state));
    StreamOutWord(ACSInfo[i].waitValue);
  end;
  StreamOutBuffer(@ACSMapVars, SizeOf(ACSMapVars));
  SV_ArchiveGlobalVariables(mapvars);
  SV_ArchivePSMapScript;
  SV_ArchiveOverlay;
end;

//==========================================================================
//
// UnarchiveScripts
//
//==========================================================================

procedure UnarchiveScripts;
var
  i: integer;
begin
  AssertSegment(ASEG_SCRIPTS);
  for i := 0 to ACScriptCount - 1 do
  begin
    ACSInfo[i].state := aste_t(GET_BYTE);
    ACSInfo[i].waitValue := GET_WORD;
  end;
  memcpy(@ACSMapVars, saveptr, SizeOf(ACSMapVars));
  incp(saveptr, SizeOf(ACSMapVars));
  SV_UnArchiveGlobalVariables(mapvars);
  SV_UnArchivePSMapScript;
  SV_UnArchiveOverlay;
end;

//==========================================================================
//
// ArchiveMisc
//
//==========================================================================

procedure ArchiveMisc;
var
  ix: integer;
begin
  StreamOutLong(ASEG_MISC);
  for ix := 0 to MAXPLAYERS - 1 do
    StreamOutLong(localQuakeHappening[ix]);
end;

//==========================================================================
//
// UnarchiveMisc
//
//==========================================================================

procedure UnarchiveMisc;
var
  ix: integer;
begin
  AssertSegment(ASEG_MISC);
  for ix := 0 to MAXPLAYERS - 1 do
    localQuakeHappening[ix] := GET_LONG;
end;

//==========================================================================
//
// RemoveAllThinkers
//
//==========================================================================

procedure RemoveAllThinkers;
var
  thinker: Pthinker_t;
  nextThinker: Pthinker_t;
begin
  thinker := thinkercap.next;
  while thinker <> @thinkercap do
  begin
    nextThinker := thinker.next;
    if @thinker._function.acp1 = @P_MobjThinker then
    begin
      P_RemoveMobj(Pmobj_t(thinker));
    end
    else
    begin
      Z_Free(thinker);
    end;
    thinker := nextThinker;
  end;
  P_InitThinkers;
end;

//==========================================================================
//
// ArchiveSounds
//
//==========================================================================

procedure ArchiveSounds;
var
  node: Pseqnode_t;
  sec: Psector_t;
  difference: integer;
  i: integer;
begin
  StreamOutLong(ASEG_SOUNDS);

  // Save the sound sequences
  StreamOutLong(ActiveSequences);
  node := SequenceListHead;
  while node <> nil do
  begin
    StreamOutLong(node.sequence);
    StreamOutLong(node.delayTics);
    StreamOutLong(node.volume);
    StreamOutLong(S_GetSequenceOffset(node.sequence, node.sequencePtr));
    StreamOutLong(node.currentSoundID);
    for i := 0 to po_NumPolyobjs - 1 do
    begin
      if node.mobj = Pmobj_t(@polyobjs[i].startSpot) then
      begin
        break;
      end;
    end;
    if i = po_NumPolyobjs then
    begin // Sound is attached to a sector, not a polyobj
      sec := R_PointInSubsector(node.mobj.x, node.mobj.y).sector;
      difference := (longword(sec) - longword(@sectors[0])) div SizeOf(sector_t);
      StreamOutLong(0); // 0 -- sector sound origin
    end
    else
    begin
      StreamOutLong(1); // 1 -- polyobj sound origin
      difference := i;
    end;
    StreamOutLong(difference);
    node := node.next;
  end;
end;

//==========================================================================
//
// UnarchiveSounds
//
//==========================================================================

procedure UnarchiveSounds;
var
  i: integer;
  numSequences: integer;
  sequence: integer;
  delayTics: integer;
  volume: integer;
  seqOffset: integer;
  soundID: integer;
  polySnd: integer;
  secNum: integer;
  sndMobj: Pmobj_t;
begin
  AssertSegment(ASEG_SOUNDS);

  // Reload and restart all sound sequences
  numSequences := GET_LONG;
  i := 0;
  while i < numSequences do
  begin
    sequence := GET_LONG;
    delayTics := GET_LONG;
    volume := GET_LONG;
    seqOffset := GET_LONG;

    soundID := GET_LONG;
    polySnd := GET_LONG;
    secNum := GET_LONG;
    if polySnd = 0 then
    begin
      sndMobj := Pmobj_t(@sectors[secNum].soundorg);
    end
    else
    begin
      sndMobj := Pmobj_t(@polyobjs[secNum].startSpot);
    end;
    S_StartSequence(sndMobj, sequence);
    S_ChangeNodeData(i, seqOffset, delayTics, volume, soundID);
    inc(i);
  end;
end;

//==========================================================================
//
// ArchivePolyobjs
//
//==========================================================================

procedure ArchivePolyobjs;
var
  i: integer;
begin
  StreamOutLong(ASEG_POLYOBJS);
  StreamOutLong(po_NumPolyobjs);
  for i := 0 to po_NumPolyobjs - 1 do
  begin
    StreamOutLong(polyobjs[i].tag);
    StreamOutLong(polyobjs[i].angle);
    StreamOutLong(polyobjs[i].startSpot.x);
    StreamOutLong(polyobjs[i].startSpot.y);
  end;
end;

//==========================================================================
//
// UnarchivePolyobjs
//
//==========================================================================

procedure UnarchivePolyobjs;
var
  i: integer;
  deltaX: fixed_t;
  deltaY: fixed_t;
begin
  AssertSegment(ASEG_POLYOBJS);
  if GET_LONG <> po_NumPolyobjs then
  begin
    I_Error('UnarchivePolyobjs(): Bad polyobj count');
  end;
  for i := 0 to po_NumPolyobjs - 1 do
  begin
    if GET_LONG <> polyobjs[i].tag then
    begin
      I_Error('UnarchivePolyobjs(): Invalid polyobj tag');
    end;
    PO_RotatePolyobj(polyobjs[i].tag, angle_t(GET_LONG));
    deltaX := GET_LONG - polyobjs[i].startSpot.x;
    deltaY := GET_LONG - polyobjs[i].startSpot.y;
    PO_MovePolyobj(polyobjs[i].tag, deltaX, deltaY);
  end;
end;

//==========================================================================
//
// ClearSaveSlot
//
// Deletes all save game files associated with a slot number.
//
//==========================================================================

procedure ClearSaveSlot(slot: integer);
var
  i: integer;
  fileName: string;
begin
  for i := 1 to MAX_MAPS do
  begin
    sprintf(fileName, _SAVEGAMEMAP, [SAVEPATH, slot, IntToStrZFill(2, i)]);
    filename := M_SaveFileName(filename);
    fdelete(fileName);
  end;
  sprintf(fileName, _SAVEGAMENAME, [SAVEPATH, slot]);
  filename := M_SaveFileName(filename);
  fdelete(fileName);
  sprintf(fileName, _SAVEGLOBALSNAME, [SAVEPATH, slot]);
  filename := M_SaveFileName(filename);
  fdelete(fileName);
end;

//==========================================================================
//
// CopySaveSlot
//
// Copies all the save game files from one slot to another.
//
//==========================================================================

procedure CopySaveSlot(sourceSlot: integer; destSlot: integer);
var
  i: integer;
  sourceName: string;
  destName: string;
begin
  for i := 1 to MAX_MAPS do
  begin
    sprintf(sourceName, _SAVEGAMEMAP, [SAVEPATH, sourceSlot, IntToStrZFill(2, i)]);
    sourceName := M_SaveFileName(sourceName);
    if fexists(sourceName) then
    begin
      sprintf(destName, _SAVEGAMEMAP, [SAVEPATH, destSlot, IntToStrZFill(2, i)]);
      destName := M_SaveFileName(destName);
      CopyFile(sourceName, destName);
    end;
  end;
  sprintf(sourceName, _SAVEGAMENAME, [SAVEPATH, sourceSlot]);
  sourceName := M_SaveFileName(sourceName);
  if fexists(sourceName) then
  begin
    sprintf(destName, _SAVEGAMENAME, [SAVEPATH, destSlot]);
    destName := M_SaveFileName(destName);
    CopyFile(sourceName, destName);
  end;
  sprintf(sourceName, _SAVEGLOBALSNAME, [SAVEPATH, sourceSlot]);
  sourceName := M_SaveFileName(sourceName);
  if fexists(sourceName) then
  begin
    sprintf(destName, _SAVEGLOBALSNAME, [SAVEPATH, destSlot]);
    destName := M_SaveFileName(destName);
    CopyFile(sourceName, destName);
  end;
end;

//==========================================================================
//
// SV_SaveMap
//
//==========================================================================

procedure SV_SaveMap(savePlayers: boolean);
var
  fileName: string;
begin
  SavingPlayers := savePlayers;

  // Open the output file
  sprintf(fileName, _SAVEGAMEMAP, [SAVEPATH, BASE_SLOT, IntToStrZFill(2, gamemap)]);
  fileName := M_SaveFileName(fileName);
  OpenStreamOut(fileName);

  // Place a header marker
  StreamOutLong(VERSION);

  // Write the level timer
  StreamOutLong(leveltime);

  // Set the mobj archive numbers
  SetMobjArchiveNums;

  ArchiveWorld;
  ArchivePolyobjs;
  ArchiveMobjs;
  ArchiveThinkers;
  ArchiveScripts;
  ArchiveSounds;
  ArchiveMisc;

  // Place a termination marker
  StreamOutLong(ASEG_END);

  // Close the output file
  CloseStreamOut;
end;


//==========================================================================
//
// SV_SaveGame
//
//==========================================================================

procedure SV_SaveGame(slot: integer; description: string);
var
  fileName: string;
  versionText: string;
begin
  // Open the output file
  sprintf(fileName, _SAVEGAMENAME, [SAVEPATH, BASE_SLOT]);
  fileName := M_SaveFileName(fileName);
  OpenStreamOut(fileName);

  // Write game save description
  StreamOutString(description);

  // Write version info
  versionText := HXS_VERSION_TEXT;
  StreamOutString(versionText);

  P_ArchiveScreenShot;

  // Place a header marker
  StreamOutLong(ASEG_GAME_HEADER);

  // Write current map and difficulty
  StreamOutByte(gamemap);
  StreamOutByte(Ord(gameskill));

  // Write global script info
  StreamOutBuffer(@ACSWorldVars, SizeOf(ACSWorldVars));
  StreamOutBuffer(@ACSStore, SizeOf(ACSStore));

  ArchivePlayers;

  // Place a termination marker
  StreamOutLong(ASEG_END);

  // Close the output file
  CloseStreamOut;

  // Save out the current map
  SV_SaveMap(true); // true := save player info

  sprintf(fileName, _SAVEGLOBALSNAME, [SAVEPATH, BASE_SLOT]);
  fileName := M_SaveFileName(fileName);
  worldvars.SaveToFile(filename);

  // Clear all save files at destination slot
  ClearSaveSlot(slot);

  // Copy base slot to destination slot
  CopySaveSlot(BASE_SLOT, slot);
end;

//==========================================================================
//
// SV_LoadMap
//
//==========================================================================

procedure SV_LoadMap;
var
  fileName: string;
  ver: integer;
begin
  // Load a base level
  G_InitNew(gameskill, gameepisode, gamemap);

  sprintf(fileName, _SAVEGLOBALSNAME, [SAVEPATH, BASE_SLOT]);
  fileName := M_SaveFileName(fileName);
  if fexists(filename) then
    worldvars.LoadFromFile(filename);

  // Remove all thinkers
  RemoveAllThinkers;

  // Create the name
  sprintf(fileName, _SAVEGAMEMAP, [SAVEPATH, BASE_SLOT, IntToStrZFill(2, gamemap)]);
  fileName := M_SaveFileName(fileName);

  // Load the file
  M_ReadFile(fileName, pointer(SaveBuffer));
  saveptr := SaveBuffer;

  ver := GET_LONG;
  if ver <> ASEG_MAP_HEADER then
    LOADVERSION := ver
  else if LOADVERSION >= VERSION142 then
    LOADVERSION := VERSION141;

  // Read the level timer
  leveltime := GET_LONG;

  UnarchiveWorld;
  UnarchivePolyobjs;
  UnarchiveMobjs;
  UnarchiveThinkers;
  UnarchiveScripts;
  UnarchiveSounds;
  UnarchiveMisc;

  AssertSegment(ASEG_END);

  // Free mobj list and save buffer
  Z_Free(MapMobjList);
  Z_Free(SaveBuffer);
end;


//==========================================================================
//
// SV_LoadGame
//
//==========================================================================

procedure SV_LoadGame(slot: integer);
var
  i: integer;
  fileName: string;
  playerBackup: array[0..MAXPLAYERS - 1] of player_t;
  mobj: Pmobj_t;
  vstring: string;
begin
  // Copy all needed save files to the base slot
  if slot <> BASE_SLOT then
  begin
    ClearSaveSlot(BASE_SLOT);
    CopySaveSlot(slot, BASE_SLOT);
  end;

  // Create the name
  sprintf(fileName, _SAVEGAMENAME, [SAVEPATH, BASE_SLOT]);
  fileName := M_SaveFileName(fileName);

  // Load the file
  M_ReadFile(fileName, pointer(SaveBuffer));

  saveptr := SaveBuffer;
  // Set the save pointer and skip the description field
  GET_STRING; // Description

  // Check the version text
  vstring := GET_STRING;
  if vstring = HXS_VERSION_TEXT_140 then
    LOADVERSION := VERSION140
  else if vstring = HXS_VERSION_TEXT_141 then
    LOADVERSION := VERSION141
  else if vstring = HXS_VERSION_TEXT_142 then
    LOADVERSION := VERSION142
  else if vstring = HXS_VERSION_TEXT_203 then
    LOADVERSION := VERSION203
  else if vstring = HXS_VERSION_TEXT_204 then
    LOADVERSION := VERSION204
  else if vstring = HXS_VERSION_TEXT_205 then
    LOADVERSION := VERSION205
  else if vstring = HXS_VERSION_TEXT_206 then
    LOADVERSION := VERSION206
  else if vstring = HXS_VERSION_TEXT_207 then
    LOADVERSION := VERSION207
  else
  begin // Bad version
    I_Warning('SV_LoadGame(): Game is from unsupported version'#13#10);
    exit;
  end;

  P_UnArchiveScreenShot;

  AssertSegment(ASEG_GAME_HEADER);

  gameepisode := 1;
  gamemap := GET_BYTE;
  gameskill := skill_t(GET_BYTE);

  // Read global script info
  memcpy(@ACSWorldVars, saveptr, SizeOf(ACSWorldVars));
  incp(saveptr, SizeOf(ACSWorldVars));
  memcpy(@ACSStore, saveptr, SizeOf(ACSStore));
  incp(saveptr, SizeOf(ACSStore));

  // Read the player structures
  UnarchivePlayers;

  AssertSegment(ASEG_END);

  Z_Free(SaveBuffer);

  // Save player structs
  for i := 0 to MAXPLAYERS - 1 do
    playerBackup[i] := players[i];

  // Load the current map
  SV_LoadMap;

  // Don't need the player mobj relocation info for load game
  Z_Free(TargetPlayerAddrs);

  // Restore player structs
  inv_ptr := 0;
  curpos := 0;
  for i := 0 to MAXPLAYERS - 1 do
  begin
    mobj := players[i].mo;
    players[i] := playerBackup[i];
    players[i].mo := mobj;
    if i = consoleplayer then
      players[i].readyArtifact := artitype_t(players[i].inventory[inv_ptr]._type);
  end;
end;

//==========================================================================
//
// SV_UpdateRebornSlot
//
// Copies the base slot to the reborn slot.
//
//==========================================================================

procedure SV_UpdateRebornSlot;
begin
  ClearSaveSlot(REBORN_SLOT);
  CopySaveSlot(BASE_SLOT, REBORN_SLOT);
end;

//==========================================================================
//
// SV_ClearRebornSlot
//
//==========================================================================

procedure SV_ClearRebornSlot;
begin
  ClearSaveSlot(REBORN_SLOT);
end;

//==========================================================================
//
// SV_MapTeleport
//
//==========================================================================

procedure SV_MapTeleport(map: integer; position: integer);
var
  i: integer;
  j: integer;
  fileName: string;
  playerBackup: array[0..MAXPLAYERS - 1] of player_t;
  targetPlayerMobj: Pmobj_t;
  mobj: Pmobj_t;
  inventoryPtr: integer;
  currentInvPos: integer;
  rClass: boolean;
  playerWasReborn: boolean;
  oldWeaponowned: array[0..Ord(NUMWEAPONS) - 1] of boolean;
  oldKeys: integer;
  oldPieces: integer;
  bestWeapon: integer;
begin
  oldKeys := 0;
  oldPieces := 0;

  if deathmatch = 0 then
  begin
    if P_GetMapCluster(gamemap) = P_GetMapCluster(map) then
    begin // Same cluster - save map without saving player mobjs
      SV_SaveMap(false);
    end
    else
    begin // Entering new cluster - clear base slot
      ClearSaveSlot(BASE_SLOT);
    end;
  end;

  // Store player structs for later
  rClass := randomclass;
  randomclass := false;
  for i := 0 to MAXPLAYERS - 1 do
    playerBackup[i] := players[i];

  // Save some globals that get trashed during the load
  inventoryPtr := inv_ptr;
  currentInvPos := curpos;

  // Only SV_LoadMap uses TargetPlayerAddrs, so it's niled here
  // for the following check (player mobj redirection)
  TargetPlayerAddrs := nil;

  gamemap := map;
  sprintf(fileName, _SAVEGAMEMAP, [SAVEPATH, BASE_SLOT, IntToStrZFill(2, gamemap)]);
  fileName := M_SaveFileName(fileName);
  if (deathmatch = 0) and fexists(fileName) then
  begin // Unarchive map
    SV_LoadMap;
  end
  else
  begin // New map
    G_InitNew(gameskill, gameepisode, gamemap);

    sprintf(fileName, _SAVEGLOBALSNAME, [SAVEPATH, BASE_SLOT]);
    fileName := M_SaveFileName(fileName);
    if fexists(filename) then
      worldvars.LoadFromFile(filename);

    // Destroy all freshly spawned players
    for i := 0 to MAXPLAYERS - 1 do
    begin
      if playeringame[i] then
        P_RemoveMobj(players[i].mo);
    end;
  end;

  // Restore player structs
  targetPlayerMobj := nil;
  for i := 0 to MAXPLAYERS - 1 do
  begin
    if not playeringame[i] then
      continue;
    players[i] := playerBackup[i];
    P_ClearMessage(@players[i]);
    players[i].attacker := nil;
    players[i].poisoner := nil;

    if netgame then
    begin
      if players[i].playerstate = PST_DEAD then
      begin // In a network game, force all players to be alive
        players[i].playerstate := PST_REBORN;
      end;
      if deathmatch = 0 then
      begin // Cooperative net-play, retain keys and weapons
        oldKeys := players[i].keys;
        oldPieces := players[i].pieces;
        for j := 0 to Ord(NUMWEAPONS) - 1 do
          oldWeaponowned[j] := players[i].weaponowned[j];
      end;
    end;
    playerWasReborn := (players[i].playerstate = PST_REBORN);
    if deathmatch <> 0 then
    begin
      memset(@players[i].frags, 0, SizeOf(players[i].frags));
      mobj := P_SpawnMobj(playerstarts[0, i].x * FRACUNIT,
                          playerstarts[0][i].y * FRACUNIT,
                          0,
                          Ord(MT_PLAYER_FIGHTER));
      players[i].mo := mobj;
      G_DeathMatchSpawnPlayer(i);
      P_RemoveMobj(mobj);
    end
    else
    begin
      P_SpawnPlayer(@playerstarts[position, i]);
    end;

    if playerWasReborn and netgame and (deathmatch = 0) then
    begin // Restore keys and weapons when reborn in co-op
      players[i].keys := oldKeys;
      players[i].pieces := oldPieces;
      bestWeapon := 0;
      for j := 0 to Ord(NUMWEAPONS) - 1 do
      begin
        if oldWeaponowned[j] then
        begin
          bestWeapon := j;
          players[i].weaponowned[j] := true;
        end;
      end;
      players[i].mana[Ord(MANA_1)] := 25;
      players[i].mana[Ord(MANA_2)] := 25;
      if bestWeapon <> 0 then
      begin // Bring up the best weapon
        players[i].pendingweapon := weapontype_t(bestWeapon);
      end;
    end;

    if targetPlayerMobj = nil then
    begin // The poor sap
      targetPlayerMobj := players[i].mo;
    end;
  end;
  randomclass := rClass;

  // Redirect anything targeting a player mobj
  if TargetPlayerAddrs <> nil then
  begin
    for i := 0 to TargetPlayerCount - 1 do
      TargetPlayerAddrs[i]^ := integer(targetPlayerMobj);
    Z_Free(TargetPlayerAddrs);
  end;

  // Destroy all things touching players
  for i := 0 to MAXPLAYERS - 1 do
  begin
    if playeringame[i] then
    begin
      P_TeleportMove(players[i].mo, players[i].mo.x, players[i].mo.y);
    end;
  end;

  // Restore trashed globals
  inv_ptr := inventoryPtr;
  curpos := currentInvPos;

  // Launch waiting scripts
  if deathmatch = 0 then
  begin
    P_CheckACSStore;
  end;

  // For single play, save immediately into the reborn slot
  if not netgame then
    SV_SaveGame(REBORN_SLOT, REBORN_DESCRIPTION);
end;

//==========================================================================
//
// SV_GetRebornSlot
//
//==========================================================================

function SV_GetRebornSlot: integer;
begin
  result := REBORN_SLOT;
end;

//==========================================================================
//
// SV_RebornSlotAvailable
//
// Returns true if the reborn slot is available.
//
//==========================================================================

function SV_RebornSlotAvailable: boolean;
var
  fileName: string;
begin
  sprintf(fileName, _SAVEGAMENAME, [SAVEPATH, REBORN_SLOT]);
  fileName := M_SaveFileName(fileName);
  result := fexists(fileName);
end;

//==========================================================================
//
// SV_InitBaseSlot
//
//==========================================================================

procedure SV_InitBaseSlot;
begin
  ClearSaveSlot(BASE_SLOT);
end;

function SV_GetSaveGameName(const slot: integer): string;
begin
  sprintf(result, _SAVEGAMENAME, [SAVEPATH, slot]);
  result := M_SaveFileName(result);
end;

function SV_GetSaveGameDescription(const slot: integer): string;
var
  filename: string;
  f: file;
  b: byte;
begin
  filename := SV_GetSaveGameName(slot);
  result := '';
  if fexists(filename) then
  begin
    AssignFile(f, filename);
    reset(f, 1);
    b := 255;
    while b <> 0 do
    begin
      BlockRead(f, b, SizeOf(byte));
      if b <> 0 then
        result := result + Chr(b);
    end;
  end;
end;

procedure P_ArchiveScreenShot;
var
  i: integer;
begin
  for i := 0 to MNSCREENSHOT_MAGIC_SIZE - 1 do
    StreamOutByte(mn_screenshotbuffer.header[i]);
  for i := 0 to MN_SCREENSHOTSIZE - 1 do
    StreamOutByte(mn_screenshotbuffer.data[i]);
end;

procedure P_UnArchiveScreenShot;
begin
  // Nothing to do, just inc the buffer
  if LOADVERSION >= VERSION206 then
    saveptr := pointer(integer(saveptr) + SizeOf(menuscreenbuffer_t));
end;

end.
