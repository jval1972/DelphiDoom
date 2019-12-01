//------------------------------------------------------------------------------
//
//  DelphiHexen: A modified and improved Hexen port for Windows
//  based on original Linux Doom as published by "id Software", on
//  Hexen source as published by "Raven" software and DelphiDoom
//  as published by Jim Valavanis.
//  Copyright (C) 2004-2012 by Jim Valavanis
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
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : http://delphidoom.sitesled.com/
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

var
  SAVEGAMENAME: string = '%s\hex%d00.hxs';
  SAVEGAMEMAP: string = '%s\hex%d%s.hxs';
  SAVEGAMENAMEDD: string = '%s\hexdd%d00.hxs';
  SAVEGAMEMAPDD: string = '%s\hexdd%d%s.hxs';
  SAVEPATH: string = 'HEXNDATA';

function SV_GetSaveGameName(const slot: integer): string;

function SV_GetSaveGameDescription(const slot: integer): string;

procedure SV_UpdateRebornSlot;

procedure SV_SaveGame(slot: integer; description: string);

implementation

uses
  d_delphi,
  d_think,
  d_player,
  d_main,
  i_system,
  info_h,
  info,
  m_fixed,
  tables,
  m_misc,
  m_argv,
  g_game,
  a_action,
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
  p_enemy,
  p_map,
  po_man,
  r_defs,
  r_main,
  s_sndseq,
  sb_bar,
  doomdef,
  z_zone;

var
  saveptr: pointer;

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
  MobjCount: integer;
  MobjList: Pmobj_tPArray;
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

  if (mobj.player <> nil) and (not SavingPlayers) then
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
  if archiveNum^ < MobjCount then
    archiveNum^ := integer(MobjList[archiveNum^])
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
  if mobj.player <> nil then
  begin
    mobj.player := @players[integer(mobj.player) - 1];
    Pplayer_t(mobj.player).mo := mobj;
  end;
  P_SetThingPosition(mobj);
  mobj.info := @mobjinfo[mobj._type];
  mobj.floorz := Psubsector_t(mobj.subsector).sector.floorheight;
  mobj.ceilingz := Psubsector_t(mobj.subsector).sector.ceilingheight;
  SetMobjPtr(PInteger(@mobj.target));
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
    I_Error('AssertSegment() Corrupted save game: Segment [%d] failed alignment check', [segType]);
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
    memcpy(@players[i], saveptr, SizeOf(player_t));
    incp(saveptr, SizeOf(player_t));
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
begin
  StreamOutLong(ASEG_WORLD);
  sec := @sectors[0];
  for i := 0 to numsectors - 1 do
  begin
    StreamOutLong(sec.floorheight);
    StreamOutLong(sec.ceilingheight);
    StreamOutWord(sec.floorpic);
    StreamOutWord(sec.ceilingpic);
    StreamOutWord(sec.lightlevel);
    StreamOutWord(sec.special);
    StreamOutWord(sec.tag);
    StreamOutByte(Ord(sec.seqType));
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
      begin
        continue;
      end;
      si := @sides[li.sidenum[j]];
      StreamOutLong(si.textureoffset);
      StreamOutLong(si.rowoffset);
      StreamOutWord(si.toptexture);
      StreamOutWord(si.bottomtexture);
      StreamOutWord(si.midtexture);
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
begin
  AssertSegment(ASEG_WORLD);
  sec := @sectors[0];
  for i := 0 to numsectors - 1 do
  begin
    sec.floorheight := GET_LONG;
    sec.ceilingheight := GET_LONG;
    sec.floorpic := GET_WORD;
    sec.ceilingpic := GET_WORD;
    sec.lightlevel := GET_WORD;
    sec.special := GET_WORD;
    sec.tag := GET_WORD;
    sec.seqType := seqtype_t(GET_BYTE);
    sec.specialdata := nil;
    sec.soundtarget := nil;
    {$IFDEF OPENGL}
    sec.iSectorID := i;  
    {$ENDIF}
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
      si.toptexture := GET_WORD;
      si.bottomtexture := GET_WORD;
      si.midtexture := GET_WORD;
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
  MobjCount := 0;
  thinker := thinkercap.next;
  while thinker <> @thinkercap do
  begin
    if @thinker._function.acp1 = @P_MobjThinker then
    begin
      mobj := Pmobj_t(thinker);
      if (mobj.player <> nil) and (not SavingPlayers) then
      begin // Skipping player mobjs
        thinker := thinker.next;
        continue;
      end;
      mobj.archiveNum := MobjCount;
      inc(MobjCount);
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
  end
  else
  begin
    mobj.target := Pmobj_t(GetMobjNum(mobj.target));
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
begin
  StreamOutLong(ASEG_MOBJS);
  StreamOutLong(MobjCount);
  count := 0;
  thinker := thinkercap.next;
  while thinker <> @thinkercap do
  begin
    if @thinker._function.acp1 <> @P_MobjThinker then
    begin // Not a mobj thinker
      thinker := thinker.next;
      continue;
    end;
    if (Pmobj_t(thinker).player <> nil) and (not SavingPlayers) then
    begin // Skipping player mobjs
      thinker := thinker.next;
      continue;
    end;
    inc(count);
    memcpy(@tempMobj, thinker, SizeOf(mobj_t));
    MangleMobj(@tempMobj);
    StreamOutBuffer(@tempMobj, SizeOf(mobj_t));
    thinker := thinker.next;
  end;
  if count <> MobjCount then
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
begin
  AssertSegment(ASEG_MOBJS);
  TargetPlayerAddrs := Z_Malloc(MAX_TARGET_PLAYERS * SizeOf(PInteger), PU_STATIC, nil);
  TargetPlayerCount := 0;
  MobjCount := GET_LONG;
  MobjList := Z_Malloc(MobjCount * SizeOf(Pmobj_t), PU_STATIC, nil);
  for i := 0 to MobjCount - 1 do
  begin
    MobjList[i] := Z_Malloc(SizeOf(mobj_t), PU_LEVEL, nil);
  end;
  for i := 0 to MobjCount - 1 do
  begin
    mobj := MobjList[i];
    memcpy(mobj, saveptr, SizeOf(mobj_t));
    incp(saveptr, SizeOf(mobj_t));
    mobj.thinker._function.acp1 := @P_MobjThinker;
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
  StreamOutBuffer(@MapVars, SizeOf(MapVars));
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
  memcpy(@MapVars, saveptr, SizeOf(MapVars));
  incp(saveptr, SizeOf(MapVars));
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
  StreamOutLong(ASEG_MAP_HEADER);

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

  // Place a header marker
  StreamOutLong(ASEG_GAME_HEADER);

  // Write current map and difficulty
  StreamOutByte(gamemap);
  StreamOutByte(Ord(gameskill));

  // Write global script info
  StreamOutBuffer(@WorldVars, SizeOf(WorldVars));
  StreamOutBuffer(@ACSStore, SizeOf(ACSStore));

  ArchivePlayers;

  // Place a termination marker
  StreamOutLong(ASEG_END);

  // Close the output file
  CloseStreamOut;

  // Save out the current map
  SV_SaveMap(true); // true := save player info

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
begin
  // Load a base level
  G_InitNew(gameskill, gameepisode, gamemap);

  // Remove all thinkers
  RemoveAllThinkers;

  // Create the name
  sprintf(fileName, _SAVEGAMEMAP, [SAVEPATH, BASE_SLOT, IntToStrZFill(2, gamemap)]);
  fileName := M_SaveFileName(fileName);

  // Load the file
  M_ReadFile(fileName, pointer(SaveBuffer));
  saveptr := SaveBuffer;

  AssertSegment(ASEG_MAP_HEADER);

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
  Z_Free(MobjList);
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
  if GET_STRING <> HXS_VERSION_TEXT then
  begin // Bad version
    exit;
  end;

  AssertSegment(ASEG_GAME_HEADER);

  gameepisode := 1;
  gamemap := GET_BYTE;
  gameskill := skill_t(GET_BYTE);

  // Read global script info
  memcpy(@WorldVars, saveptr, SizeOf(WorldVars));
  incp(saveptr, SizeOf(WorldVars));
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


end.
