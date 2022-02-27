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
//  UDMF Special effects
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit udmf_spec;

interface

uses
  d_delphi,
  p_mobj_h,
  r_defs,
  udmf_doors;

//==============================================================================
//
// P_FindSectorFromTag
//
//==============================================================================
function P_FindSectorFromTag(tag: integer; var start: integer): integer;

//==============================================================================
//
// P_FindSectorFromTag2
//
//==============================================================================
function P_FindSectorFromTag2(tag: integer; var start: integer): integer;

//==============================================================================
//
// P_FindLine
//
//==============================================================================
function P_FindLine(lineTag: integer; searchPosition: PInteger): Pline_t;

//==============================================================================
//
// P_ExecuteLineSpecial
//
//==============================================================================
function P_ExecuteLineSpecial(special: integer; args: PByteArray; line: Pline_t;
  side: integer; mo: Pmobj_t): boolean;

//==============================================================================
//
// P_ActivateLine
//
//==============================================================================
function P_ActivateLine(line: Pline_t; mo: Pmobj_t; side: integer; activationType: integer): boolean;

//==============================================================================
//
// P_LineToArgs
//
//==============================================================================
procedure P_LineToArgs(line: Pline_t; const args: PByteArray);

//==============================================================================
//
// P_ArgsToLine
//
//==============================================================================
procedure P_ArgsToLine(line: Pline_t; const args: PByteArray);

//==============================================================================
//
// P_ExecuteActorSpecial
//
//==============================================================================
function P_ExecuteActorSpecial(special: integer; args: PIntegerArray; mo: Pmobj_t): boolean;

const
  {$IFDEF DOOM_OR_STRIFE}
  LIGHTNING_SPECIAL = 20;
  LIGHTNING_SPECIAL2 = 19;
  LIGHTNING_SPECIAL_MASK = 31;
  {$ENDIF}
  {$IFDEF HERETIC}
  LIGHTNING_SPECIAL = 56;
  LIGHTNING_SPECIAL2 = 55;
  LIGHTNING_SPECIAL_MASK = 255;
  {$ENDIF}

var
  LevelHasLightning: boolean;
  NextLightningFlash: integer;
  LightningFlash: integer;

//==============================================================================
//
// P_ForceLightning
//
//==============================================================================
procedure P_ForceLightning(const tics: Integer = 0);

//==============================================================================
//
// P_InitLightning
//
//==============================================================================
procedure P_InitLightning;

//==============================================================================
//
// P_LightningFlash
//
//==============================================================================
procedure P_LightningFlash;
  
implementation

uses
  doomdef,
  doomdata,
  d_think,
  d_player,
  g_game,
  m_fixed,
  m_rnd,
  tables,
  p_acs,
  p_common,
  p_inter,
  p_maputl,
  p_mobj,
  p_setup,
  p_sounds,
  p_spec,
  p_switch,
  p_tick,
  p_udmf,
  po_man,
  r_sky,
  sounddata,
  s_sound,
  udmf_ceilng,
  udmf_floor,
  udmf_lights,
  udmf_mobj,
  udmf_plats,
  udmf_telept,
  udmf_things;

//==============================================================================
//
// P_FindSectorFromTag
//
//==============================================================================
function P_FindSectorFromTag(tag: integer; var start: integer): integer;
begin
  result := P_FindSectorFromTag2(tag, start);
end;

//==============================================================================
//
// P_FindSectorFromTag2
//
//==============================================================================
function P_FindSectorFromTag2(tag: integer; var start: integer): integer;
var
  i: integer;
  b: Byte;
begin
  if IsIntegerInRange(tag, 0, 255) then
  begin
    b := tag;
    for i := start + 1 to numsectors - 1 do
    begin
      if b in sectors[i].moreids then
      begin
        result := i;
        start := result;
        exit;
      end;
      if sectors[i].tag = tag then
      begin
        result := i;
        start := result;
        exit;
      end;
    end;
  end
  else
  begin
    for i := start + 1 to numsectors - 1 do
      if sectors[i].tag = tag then
      begin
        result := i;
        start := result;
        exit;
      end;
  end;

  result := -1;
  start := -1;
end;

//==============================================================================
//
// P_FindLine
//
//==============================================================================
function P_FindLine(lineTag: integer; searchPosition: PInteger): Pline_t;
var
  i: integer;
begin
  for i := searchPosition^ + 1 to numlines - 1 do
    if lines[i].tag = lineTag then
    begin
      result := @lines[i];
      searchPosition^ := i;
      exit;
    end;
  searchPosition^ := -1;
  result := nil;
end;

//==============================================================================
//
// P_LightningFlash
//
//==============================================================================
procedure P_LightningFlash;
const
  SND_THUNDR = 'THUNDR';
var
  i: integer;
  tempSec: Psector_t;
  foundSec: boolean;
  flashLight: integer;
begin
  if LightningFlash <> 0 then
  begin
    dec(LightningFlash);
    if LightningFlash <> 0 then
    begin
      tempSec := @sectors[0];
      for i := 0 to numsectors - 1 do
      begin
        if (tempSec.ceilingpic = skyflatnum) or
           (tempSec.special and LIGHTNING_SPECIAL_MASK = LIGHTNING_SPECIAL) or
           (tempSec.special and LIGHTNING_SPECIAL_MASK = LIGHTNING_SPECIAL2) then
        begin
          if tempSec.lightninglightlevel < tempSec.lightlevel - 4 then
            tempSec.lightlevel := tempSec.lightlevel - 4;
        end;
        inc(tempSec);
      end;
    end
    else
    begin // remove the alternate lightning flash special
      tempSec := @sectors[0];
      for i := 0 to numsectors - 1 do
      begin
        if (tempSec.ceilingpic = skyflatnum) or
           (tempSec.special and LIGHTNING_SPECIAL_MASK = LIGHTNING_SPECIAL) or
           (tempSec.special and LIGHTNING_SPECIAL_MASK = LIGHTNING_SPECIAL2) then
        begin
          tempSec.lightlevel := tempSec.lightninglightlevel;
        end;
        inc(tempSec);
      end;
      skytexture := skytexture1;
    end;
    exit;
  end;
  LightningFlash := (P_Random and 7) + 8;
  flashLight := 200 + (P_Random and 31);
  tempSec := @sectors[0];
  foundSec := false;
  for i := 0 to numsectors - 1 do
  begin
    if (tempSec.ceilingpic = skyflatnum) or
       (tempSec.special and LIGHTNING_SPECIAL_MASK = LIGHTNING_SPECIAL) or
       (tempSec.special and LIGHTNING_SPECIAL_MASK = LIGHTNING_SPECIAL2) then
    begin
      tempSec.lightninglightlevel := tempSec.lightlevel;
      if tempSec.special and LIGHTNING_SPECIAL_MASK = LIGHTNING_SPECIAL then
      begin
        tempSec.lightlevel := tempSec.lightlevel + 64;
        if tempSec.lightlevel > flashLight then
          tempSec.lightlevel := flashLight;
      end
      else if tempSec.special and LIGHTNING_SPECIAL_MASK = LIGHTNING_SPECIAL2 then
      begin
        tempSec.lightlevel := tempSec.lightlevel + 32;
        if tempSec.lightlevel > flashLight then
          tempSec.lightlevel := flashLight;
      end
      else
        tempSec.lightlevel := flashLight;
      if tempSec.lightlevel < tempSec.lightninglightlevel then
      begin
        tempSec.lightlevel := tempSec.lightninglightlevel;
      end;
      foundSec := true;
    end;
    inc(tempSec);
  end;
  if foundSec then
  begin
    skytexture := skytexture2;
    S_StartSound(nil, SND_THUNDR);
  end;
  // Calculate the next lighting flash
  if NextLightningFlash = 0 then
  begin
    if P_Random < 50 then
    begin // Immediate Quick flash
      NextLightningFlash := (P_Random and 15) + 16;
    end
    else
    begin
      if (P_Random < 128) and (leveltime and 32 = 0) then
        NextLightningFlash := ((P_Random and 7) + 2) * TICRATE
      else
        NextLightningFlash := ((P_Random and 15) + 5) * TICRATE;
    end;
  end;
end;

//==============================================================================
//
// P_ForceLightning
//
//==============================================================================
procedure P_ForceLightning(const tics: Integer = 0);
begin
  NextLightningFlash := tics;
end;

//==============================================================================
//
// P_InitLightning
//
//==============================================================================
procedure P_InitLightning;
var
  i: integer;
  secCount: integer;
begin
  if (gamemapinfo <> nil) and not gamemapinfo.lightning then
  begin
    LevelHasLightning := false;
    LightningFlash := 0;
    exit;
  end
  else if gamemapinfo = nil then
  begin
    LevelHasLightning := false;
    LightningFlash := 0;
    exit;
  end;

  LightningFlash := 0;
  secCount := 0;
  for i := 0 to numsectors - 1 do
    if (sectors[i].ceilingpic = skyflatnum) or
       (sectors[i].special and LIGHTNING_SPECIAL_MASK = LIGHTNING_SPECIAL) or
       (sectors[i].special and LIGHTNING_SPECIAL_MASK = LIGHTNING_SPECIAL2) then
      inc(secCount);

  if secCount > 0 then
    LevelHasLightning := true
  else
  begin
    LevelHasLightning := false;
    exit;
  end;

  NextLightningFlash := ((P_Random and 15) + 5) * TICRATE; // don't flash at level start
end;

//==============================================================================
//
// EVH_SectorSoundChange
//
//==============================================================================
function EVH_SectorSoundChange(args: PByteArray): boolean;
var
  secNum: integer;
begin

  if args[0] = 0 then
  begin
    result := false;
    exit;
  end;

  result := false;
  secNum := -1;
  while P_FindSectorFromTag2(args[0], secNum) >= 0 do
  begin
    sectors[secNum].seqType := args[1];
    result := true;
  end;
end;

//==============================================================================
//
// CheckedLockedDoor
//
//==============================================================================
function CheckedLockedDoor(mo: Pmobj_t; lock: byte): boolean;
var
  LockedBuffer: string;
begin
  if mo.player = nil then
  begin
    result := false;
    exit;
  end;

  if lock <> 0 then
  begin
    {$IFDEF DOOM_OR_STRIFE}
    if not IsIntegerInRange(lock, 1, Ord(NUMCARDS)) or not Pplayer_t(mo.player).cards[lock - 1] then
    {$ENDIF}
    {$IFDEF HERETIC}
    if not IsIntegerInRange(lock, 1, Ord(NUMKEYCARDS)) or not Pplayer_t(mo.player).keys[lock - 1] then
    {$ENDIF}
    begin
      {$IFDEF DOOM_OR_STRIFE}
      if not IsIntegerInRange(lock, 1, Ord(NUMCARDS)) then
        LockedBuffer := 'YOU NEED A KEY'
      else
      {$ENDIF}
      {$IFDEF HERETIC}
      if not IsIntegerInRange(lock, 1, Ord(NUMKEYCARDS)) then
        LockedBuffer := 'YOU NEED A KEY'
      else
      {$ENDIF}
      sprintf(LockedBuffer, 'YOU NEED THE %s', [TextKeyMessages[lock - 1]]);
      Pplayer_t(mo.player)._message := LockedBuffer;
      {$IFDEF DOOM_OR_STRIFE}
      S_StartSound(mo, Ord(sfx_oof));
      {$ENDIF}
      {$IFDEF HERETIC}
      S_StartSound(mo, Ord(sfx_plroof));
      {$ENDIF}
      result := false;
      exit;
    end;
  end;
  result := true;
end;

//==============================================================================
//
// P_LocalEarthQuakeEx
//
//==============================================================================
procedure P_LocalEarthQuakeEx(const actor: Pmobj_t; const tics: integer; const intensity: fixed_t; const maxtdist, maxddist: fixed_t);
const
  SND_QUAKE = 'QUAKE2';
var
  i: integer;
  dist: fixed_t;
  frac: fixed_t;
  testintensity: fixed_t;
  pmo: Pmobj_t;
  rnd: integer;
  an: angle_t;
begin
  // JVAL: Actor's active sound is the quake sound
  if actor.info.activesound <> 0 then
    A_ActiveSound(actor, actor)
  else
    S_StartSound(actor, SND_QUAKE);

  for i := 0 to MAXPLAYERS - 1 do
    if playeringame[i] then
    begin
      pmo := players[i].mo;
      dist := P_AproxDistance(actor.x - pmo.x, actor.y - pmo.y);
      dist := P_AproxDistance(actor.z - pmo.z, dist); // 3d distance
      if dist <= maxtdist then
      begin
        if players[i].quaketics < tics then
          players[i].quaketics := tics;
        frac := FixedDiv(dist, maxtdist) * (FINEANGLES div 4);
        testintensity := FixedMul(finecosine[frac shr ANGLETOFINESHIFT], intensity); // JVAL: 20200508 - Curved
        if players[i].quakeintensity < testintensity then
          players[i].quakeintensity := testintensity;
      end;
      if (dist < maxddist) and (pmo.z <= pmo.floorz) then
      begin
        if P_Random < 50 then
          P_DamageMobj(players[i].mo, nil, nil, (1 + (P_Random and 7)));
        // Thrust player around
        rnd := P_Random;
        an := pmo.angle + ANG1 * rnd;
        P_ThrustMobj(pmo, an, intensity div 2);
      end;
    end;
end;

//==============================================================================
//
// U_LocalQuake
//
// Quake variables
//
//    args[0]    Intensity on richter scale (2..9)
//    args[1]    Duration in tics
//    args[2]    Radius for damage
//    args[3]    Radius for tremor
//    args[4]    TID of map thing for focus of quake
//
//==============================================================================
function U_LocalQuake(args: PByteArray; actor: Pmobj_t): boolean;
const
  QUAKEINTENSITIES: array[2..9] of fixed_t = (
    FRACUNIT div 4,
    FRACUNIT div 2,
    (FRACUNIT * 3) div 4,
    FRACUNIT,
    FRACUNIT + FRACUNIT div 4,
    FRACUNIT + FRACUNIT div 2,
    FRACUNIT + (FRACUNIT * 3) div 4,
    2 * FRACUNIT
  );
var
  focus: Pmobj_t;
  lastfound: Pthinker_t;
begin
  result := false;
  lastfound := @thinkercap;
  repeat
    focus := P_FindMobjFromTID(args[4], Pointer(lastfound));
    if focus <> nil then
      P_LocalEarthQuakeEx(
        focus,
        args[1] * FRACUNIT,
        QUAKEINTENSITIES[GetIntegerInRange(args[0], 2, 9)],
        args[3] * FRACUNIT * 64,
        args[2] * FRACUNIT * 64
      );
  until focus = nil;
end;

//==============================================================================
//
// P_ExecuteLineSpecial
//
//==============================================================================
function P_ExecuteLineSpecial(special: integer; args: PByteArray; line: Pline_t;
  side: integer; mo: Pmobj_t): boolean;
begin
  result := false;

  case special of

    1: // Poly Start Line
      begin
      end;

    2: // Poly Rotate Left
      begin
        result := EVH_RotatePoly(line, args, 1, false);
      end;

    3: // Poly Rotate Right
      begin
        result := EVH_RotatePoly(line, args, -1, false);
      end;

    4: // Poly Move
      begin
        result := EVH_MovePoly(line, args, false, false);
      end;

    5: // Poly Explicit Line:  Only used in initialization
      begin
      end;

    6: // Poly Move Times 8
      begin
        result := EVH_MovePoly(line, args, true, false);
      end;

    7: // Poly Door Swing
      begin
        result := EVH_OpenPolyDoor(line, args, PODOOR_SWING);
      end;

    8: // Poly Door Slide
      begin
        result := EVH_OpenPolyDoor(line, args, PODOOR_SLIDE);
      end;

    10: // Door Close
      begin
        result := EVH_DoDoor(line, args, DREV_CLOSE);
      end;

    11: // Door Open
      begin
        if args[0] = 0 then
          result := EVH_VerticalDoor(line, mo)
        else
          result := EVH_DoDoor(line, args, DREV_OPEN);
      end;

    12: // Door Raise
      begin
        if args[0] = 0 then
          result := EVH_VerticalDoor(line, mo)
        else
          result := EVH_DoDoor(line, args, DREV_NORMAL);
      end;

    13: // Door Locked_Raise
      begin
        if CheckedLockedDoor(mo, args[3]) then
        begin
          if args[0] = 0 then
            result := EVH_VerticalDoor(line, mo)
          else
            result := EVH_DoDoor(line, args, DREV_NORMAL);
        end;
      end;

    20: // Floor Lower by Value
      begin
        result := EVH_DoFloor(line, args, FLEV_LOWERFLOORBYVALUE);
      end;

    21: // Floor Lower to Lowest
      begin
        result := EVH_DoFloor(line, args, FLEV_LOWERFLOORTOLOWEST);
      end;

    22: // Floor Lower to Nearest
      begin
        result := EVH_DoFloor(line, args, FLEV_LOWERFLOOR);
      end;

    23: // Floor Raise by Value
      begin
        result := EVH_DoFloor(line, args, FLEV_RAISEFLOORBYVALUE);
      end;

    24: // Floor Raise to Highest
      begin
        result := EVH_DoFloor(line, args, FLEV_RAISEFLOOR);
      end;

    25: // Floor Raise to Nearest
      begin
        result := EVH_DoFloor(line, args, FLEV_RAISEFLOORTONEAREST);
      end;

    26: // Stairs Build Down Normal
      begin
        result := EVH_BuildStairs(line, args, -1, STAIRS_NORMAL);
      end;

    27: // Build Stairs Up Normal
      begin
        result := EVH_BuildStairs(line, args, 1, STAIRS_NORMAL);
      end;

    28: // Floor Raise and Crush
      begin
        result := EVH_DoFloor(line, args, FLEV_RAISEFLOORCRUSH);
      end;

    29: // Build Pillar (no crushing)
      begin
        result := EVH_BuildPillar(line, args, false);
      end;

    30: // Open Pillar
      begin
        result := EVH_OpenPillar(line, args);
      end;

    31: // Stairs Build Down Sync
      begin
        result := EVH_BuildStairs(line, args, -1, STAIRS_SYNC);
      end;

    32: // Build Stairs Up Sync
      begin
        result := EVH_BuildStairs(line, args, 1, STAIRS_SYNC);
      end;

    35: // Raise Floor by Value Times 8
      begin
        result := EVH_DoFloor(line, args, FLEV_RAISEBYVALUETIMES8);
      end;

    36: // Lower Floor by Value Times 8
      begin
        result := EVH_DoFloor(line, args, FLEV_LOWERBYVALUETIMES8);
      end;

    40: // Ceiling Lower by Value
      begin
        result := EVH_DoCeiling(line, args, CLEV_LOWERBYVALUE);
      end;

    41: // Ceiling Raise by Value
      begin
        result := EVH_DoCeiling(line, args, CLEV_RAISEBYVALUE);
      end;

    42: // Ceiling Crush and Raise
      begin
        result := EVH_DoCeiling(line, args, CLEV_CRUSHANDRAISE);
      end;

    43: // Ceiling Lower and Crush
      begin
        result := EVH_DoCeiling(line, args, CLEV_LOWERANDCRUSH);
      end;

    44: // Ceiling Crush Stop
      begin
        result := EVH_CeilingCrushStop(line, args);
      end;

    45: // Ceiling Crush Raise and Stay
      begin
        result := EVH_DoCeiling(line, args, CLEV_CRUSHRAISEANDSTAY);
      end;

    46: // Floor Crush Stop
      begin
        result := EVH_FloorCrushStop(line, args);
      end;

    60: // Plat Perpetual Raise
      begin
        result := EVH_DoPlat(line, args, PLAT_PERPETUALRAISE, 0);
      end;

    61: // Plat Stop
      begin
        EVH_StopPlat(line, args);
      end;

    62: // Plat Down-Wait-Up-Stay
      begin
        result := EVH_DoPlat(line, args, PLAT_DOWNWAITUPSTAY, 0);
      end;

    63: // Plat Down-by-Value*8-Wait-Up-Stay
      begin
        result := EVH_DoPlat(line, args, PLAT_DOWNBYVALUEWAITUPSTAY, 0);
      end;

    64: // Plat Up-Wait-Down-Stay
      begin
        result := EVH_DoPlat(line, args, PLAT_UPWAITDOWNSTAY, 0);
      end;

    65: // Plat Up-by-Value*8-Wait-Down-Stay
      begin
        result := EVH_DoPlat(line, args, PLAT_UPBYVALUEWAITDOWNSTAY, 0);
      end;

    66: // Floor Lower Instant * 8
      begin
        result := EVH_DoFloor(line, args, FLEV_LOWERTIMES8INSTANT);
      end;

    67: // Floor Raise Instant * 8
      begin
        result := EVH_DoFloor(line, args, FLEV_RAISETIMES8INSTANT);
      end;

    68: // Floor Move to Value * 8
      begin
        result := EVH_DoFloor(line, args, FLEV_MOVETOVALUETIMES8);
      end;

    69: // Ceiling Move to Value * 8
      begin
        result := EVH_DoCeiling(line, args, CLEV_MOVETOVALUETIMES8);
      end;

    70: // Teleport
      begin
        if side = 0 then
        begin // Only teleport when crossing the front side of a line
          result := EVH_Teleport(args[0], mo, true);
        end;
      end;

    71: // Teleport, no fog
      begin
        if side = 0 then
        begin // Only teleport when crossing the front side of a line
          result := EVH_Teleport(args[0], mo, false);
        end;
      end;

    72: // Thrust Mobj
      begin
        if side = 0 then // Only thrust on side 0
        begin
          P_ThrustMobj(mo, args[0] * (ANG90 div 64), args[1] * FRACUNIT);
          result := true;
        end;
      end;

    73: // Damage Mobj
      begin
        if args[0] <> 0 then
          P_DamageMobj(mo, nil, nil, args[0])
        else  // If arg1 is zero, then guarantee a kill
          P_DamageMobj(mo, nil, nil, 10000);
        result := true;
      end;

    {$IFDEF STRIFE}
    74: // Teleport_NewMap
      begin
        if side = 0 then
        begin // Only teleport when crossing the front side of a line
          if not ((mo <> nil) and (mo.player <> nil) and (Pplayer_t(mo.player).playerstate = PST_DEAD)) then // Players must be alive to teleport
          begin
            result := true;
            G_RiftExitLevel(args[0], args[1], args[2] * ANGLEMAX_DIV_256);
          end;
        end;
      end;
    {$ENDIF}

    75: // Teleport_EndGame
      begin
        if side = 0 then
        begin // Only teleport when crossing the front side of a line
          if not ((mo <> nil) and (mo.player <> nil) and (Pplayer_t(mo.player).playerstate = PST_DEAD)) then // Players must be alive to teleport
          begin
            result := true;
            G_ExitLevel{$IFDEF STRIFE}(args[0]){$ENDIF};
          end;
        end;
      end;

    80: // ACS_Execute
      begin
        result := P_StartACS(args[0], args[1], @args[2], mo, line, side);
      end;

    81: // ACS_Suspend
      begin
        result := P_SuspendACS(args[0], args[1]);
      end;

    82: // ACS_Terminate
      begin
        result := P_TerminateACS(args[0], args[1]);
      end;

    83: // ACS_LockedExecute
      begin
        result := P_StartLockedACS(line, args, mo, side);
      end;

    90: // Poly Rotate Left Override
      begin
        result := EVH_RotatePoly(line, args, 1, true);
      end;

    91: // Poly Rotate Right Override
      begin
        result := EVH_RotatePoly(line, args, -1, true);
      end;

    92: // Poly Move Override
      begin
        result := EVH_MovePoly(line, args, false, true);
      end;

    93: // Poly Move Times 8 Override
      begin
        result := EVH_MovePoly(line, args, true, true);
      end;

    94: // Build Pillar Crush
      begin
        result := EVH_BuildPillar(line, args, true);
      end;

    95: // Lower Floor and Ceiling
      begin
        result := EVH_DoFloorAndCeiling(line, args, false);
      end;

    96: // Raise Floor and Ceiling
      begin
        result := EVH_DoFloorAndCeiling(line, args, true);
      end;

    109: // Force Lightning
      begin
        result := true;
        P_ForceLightning(args[0]);
      end;

    110: // Light Raise by Value
      begin
        result := EVH_SpawnLight(line, args, LITE_RAISEBYVALUE);
      end;

    111: // Light Lower by Value
      begin
        result := EVH_SpawnLight(line, args, LITE_LOWERBYVALUE);
      end;

    112: // Light Change to Value
      begin
        result := EVH_SpawnLight(line, args, LITE_CHANGETOVALUE);
      end;

    113: // Light Fade
      begin
        result := EVH_SpawnLight(line, args, LITE_FADE);
      end;

    114: // Light Glow
      begin
        result := EVH_SpawnLight(line, args, LITE_GLOW);
      end;

    115: // Light Flicker
      begin
        result := EVH_SpawnLight(line, args, LITE_FLICKER);
      end;

    116: // Light Strobe
      begin
        result := EVH_SpawnLight(line, args, LITE_STROBE);
      end;

    120: // Quake Tremor
      begin
        result := U_LocalQuake(args, mo);
      end;

{    129: // UsePuzzleItem
      begin
        result := EVH_LineSearchForPuzzleItem(line, args, mo);
      end;}

    130: // Thing_Activate
      begin
        result := EVH_ThingActivate(args[0]);
      end;

    131: // Thing_Deactivate
      begin
        result := EVH_ThingDeactivate(args[0]);
      end;

    132: // Thing_Remove
      begin
        result := EVH_ThingRemove(args[0]);
      end;

    133: // Thing_Destroy
      begin
        result := EVH_ThingDestroy(args[0]);
      end;

    134: // Thing_Projectile
      begin
        result := EVH_ThingProjectile(args, false);
      end;

    135: // Thing_Spawn
      begin
        result := EVH_ThingSpawn(args, true);
      end;

    136: // Thing_ProjectileGravity
      begin
        result := EVH_ThingProjectile(args, true);
      end;

    137: // Thing_SpawnNoFog
      begin
        result := EVH_ThingSpawn(args, false);
      end;

    138: // Floor_Waggle
      begin
        result := EVH_StartFloorWaggle(args[0], args[1], args[2], args[3], args[4]);
      end;

    140: // Sector_SoundChange
      begin
        result := EVH_SectorSoundChange(args);
      end;

    // Line specials only processed during level initialization
    // 100: Scroll_Texture_Left
    // 101: Scroll_Texture_Right
    // 102: Scroll_Texture_Up
    // 103: Scroll_Texture_Down
    // 121: Line_SetIdentification

    // Inert Line specials
  end;
end;

//==============================================================================
//
// P_ActivateLine
//
//==============================================================================
function P_ActivateLine(line: Pline_t; mo: Pmobj_t; side: integer; activationType: integer): boolean;
var
  dorepeat: boolean;
  buttonSuccess: boolean;
  args: array[0..4] of Byte;
begin
  if not IsIntegerInRange(line.special and 1023, UDMF_SPECIAL_START, UDMF_SPECIAL_FINISH) then
  begin
    result := false;
    exit;
  end;

  if line.activators and activationType = 0 then
  begin
    result := false;
    exit;
  end;

  if (mo.player = nil) and (mo.flags and MF_MISSILE = 0) then
  begin

    if line.activators and (ULAC_MCROSS or ULAC_MPUSH) = 0 then
    begin // currently, monsters can only activate the MCROSS & MPUSH activation types
       result := false;
       exit;
    end;

    if line.flags and ML_SECRET <> 0 then
    begin
      result := false;  // never open secret doors
      exit;
    end;

  end;

  dorepeat := line.activators and ULAC_REPEAT <> 0;
  P_LineToArgs(line, @args);
  buttonSuccess := P_ExecuteLineSpecial(line.special, @args, line, side, mo);
  P_ArgsToLine(line, @args);

  if not dorepeat and buttonSuccess then
  begin // clear the special on non-retriggerable lines - keep generalized specials
    line.special := line.special and not 1023;
  end;

  if ((line.activators and ULAC_USE <> 0) or (line.activators and ULAC_IMPACT <> 0)) and buttonSuccess then
    P_ChangeSwitchTexture(line, dorepeat);

  result := true;
end;

//==============================================================================
//
// P_LineToArgs
//
//==============================================================================
procedure P_LineToArgs(line: Pline_t; const args: PByteArray);
begin
  args[0] := line.arg1;
  args[1] := line.arg2;
  args[2] := line.arg3;
  args[3] := line.arg4;
  args[4] := line.arg5;
  line.special := line.special - UDMF_SPECIAL_START;
end;

//==============================================================================
//
// P_ArgsToLine
//
//==============================================================================
procedure P_ArgsToLine(line: Pline_t; const args: PByteArray);
begin
  line.arg1 := args[0];
  line.arg2 := args[1];
  line.arg3 := args[2];
  line.arg4 := args[3];
  line.arg5 := args[4];
  line.special := line.special + UDMF_SPECIAL_START;
end;

//==============================================================================
//
// P_ExecuteActorSpecial
// Note: args are Integers
//
//==============================================================================
function P_ExecuteActorSpecial(special: integer; args: PIntegerArray; mo: Pmobj_t): boolean;
var
  bargs: packed array[0..4] of Byte;
begin
  bargs[0] := args[0];
  bargs[1] := args[1];
  bargs[2] := args[2];
  bargs[3] := args[3];
  bargs[4] := args[4];
  result := P_ExecuteLineSpecial(special - UDMF_SPECIAL_START, @bargs, nil, 0, mo);
  args[0] := bargs[0];
  args[1] := bargs[1];
  args[2] := bargs[2];
  args[3] := bargs[3];
  args[4] := bargs[4];
end;

end.
