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
//  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
//  02111-1307, USA.
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit p_things;

interface

uses
  d_delphi,
  info_h;

const
  TranslateThingType: array[0..108] of mobjtype_t = (
    MT_MAPSPOT,           // T_NONE
    MT_CENTAUR,           // T_CENTAUR
    MT_CENTAURLEADER,     // T_CENTAURLEADER
    MT_DEMON,             // T_DEMON
    MT_ETTIN,             // T_ETTIN
    MT_FIREDEMON,         // T_FIREGARGOYLE
    MT_SERPENT,           // T_WATERLURKER
    MT_SERPENTLEADER,     // T_WATERLURKERLEADER
    MT_WRAITH,            // T_WRAITH
    MT_WRAITHB,           // T_WRAITHBURIED
    MT_FIREBALL1,         // T_FIREBALL1
    MT_MANA1,             // T_MANA1
    MT_MANA2,             // T_MANA2
    MT_SPEEDBOOTS,        // T_ITEMBOOTS
    MT_ARTIEGG,           // T_ITEMEGG
    MT_ARTIFLY,           // T_ITEMFLIGHT
    MT_SUMMONMAULATOR,    // T_ITEMSUMMON
    MT_TELEPORTOTHER,     // T_ITEMTPORTOTHER
    MT_ARTITELEPORT,      // T_ITEMTELEPORT
    MT_BISHOP,            // T_BISHOP
    MT_ICEGUY,            // T_ICEGOLEM
    MT_BRIDGE,            // T_BRIDGE
    MT_BOOSTARMOR,        // T_DRAGONSKINBRACERS
    MT_HEALINGBOTTLE,     // T_ITEMHEALTHPOTION
    MT_HEALTHFLASK,       // T_ITEMHEALTHFLASK
    MT_ARTISUPERHEAL,     // T_ITEMHEALTHFULL
    MT_BOOSTMANA,         // T_ITEMBOOSTMANA
    MT_FW_AXE,            // T_FIGHTERAXE
    MT_FW_HAMMER,         // T_FIGHTERHAMMER
    MT_FW_SWORD1,         // T_FIGHTERSWORD1
    MT_FW_SWORD2,         // T_FIGHTERSWORD2
    MT_FW_SWORD3,         // T_FIGHTERSWORD3
    MT_CW_SERPSTAFF,      // T_CLERICSTAFF
    MT_CW_HOLY1,          // T_CLERICHOLY1
    MT_CW_HOLY2,          // T_CLERICHOLY2
    MT_CW_HOLY3,          // T_CLERICHOLY3
    MT_MW_CONE,           // T_MAGESHARDS
    MT_MW_STAFF1,         // T_MAGESTAFF1
    MT_MW_STAFF2,         // T_MAGESTAFF2
    MT_MW_STAFF3,         // T_MAGESTAFF3
    MT_EGGFX,             // T_MORPHBLAST
    MT_ROCK1,             // T_ROCK1
    MT_ROCK2,             // T_ROCK2
    MT_ROCK3,             // T_ROCK3
    MT_DIRT1,             // T_DIRT1
    MT_DIRT2,             // T_DIRT2
    MT_DIRT3,             // T_DIRT3
    MT_DIRT4,             // T_DIRT4
    MT_DIRT5,             // T_DIRT5
    MT_DIRT6,             // T_DIRT6
    MT_ARROW,             // T_ARROW
    MT_DART,              // T_DART
    MT_POISONDART,        // T_POISONDART
    MT_RIPPERBALL,        // T_RIPPERBALL
    MT_SGSHARD1,          // T_STAINEDGLASS1
    MT_SGSHARD2,          // T_STAINEDGLASS2
    MT_SGSHARD3,          // T_STAINEDGLASS3
    MT_SGSHARD4,          // T_STAINEDGLASS4
    MT_SGSHARD5,          // T_STAINEDGLASS5
    MT_SGSHARD6,          // T_STAINEDGLASS6
    MT_SGSHARD7,          // T_STAINEDGLASS7
    MT_SGSHARD8,          // T_STAINEDGLASS8
    MT_SGSHARD9,          // T_STAINEDGLASS9
    MT_SGSHARD0,          // T_STAINEDGLASS0
    MT_PROJECTILE_BLADE,  // T_BLADE
    MT_ICESHARD,          // T_ICESHARD
    MT_FLAME_SMALL,       // T_FLAME_SMALL
    MT_FLAME_LARGE,       // T_FLAME_LARGE
    MT_ARMOR_1,           // T_MESHARMOR
    MT_ARMOR_2,           // T_FALCONSHIELD
    MT_ARMOR_3,           // T_PLATINUMHELM
    MT_ARMOR_4,           // T_AMULETOFWARDING
    MT_ARTIPOISONBAG,     // T_ITEMFLECHETTE
    MT_ARTITORCH,         // T_ITEMTORCH
    MT_BLASTRADIUS,       // T_ITEMREPULSION
    MT_MANA3,             // T_MANA3
    MT_ARTIPUZZSKULL,     // T_PUZZSKULL
    MT_ARTIPUZZGEMBIG,    // T_PUZZGEMBIG
    MT_ARTIPUZZGEMRED,    // T_PUZZGEMRED
    MT_ARTIPUZZGEMGREEN1, // T_PUZZGEMGREEN1
    MT_ARTIPUZZGEMGREEN2, // T_PUZZGEMGREEN2
    MT_ARTIPUZZGEMBLUE1,  // T_PUZZGEMBLUE1
    MT_ARTIPUZZGEMBLUE2,  // T_PUZZGEMBLUE2
    MT_ARTIPUZZBOOK1,     // T_PUZZBOOK1
    MT_ARTIPUZZBOOK2,     // T_PUZZBOOK2
    MT_KEY1,              // T_METALKEY
    MT_KEY2,              // T_SMALLMETALKEY
    MT_KEY3,              // T_AXEKEY
    MT_KEY4,              // T_FIREKEY
    MT_KEY5,              // T_GREENKEY
    MT_KEY6,              // T_MACEKEY
    MT_KEY7,              // T_SILVERKEY
    MT_KEY8,              // T_RUSTYKEY
    MT_KEY9,              // T_HORNKEY
    MT_KEYA,              // T_SERPENTKEY
    MT_WATER_DRIP,        // T_WATERDRIP
    MT_FLAME_SMALL_TEMP,  // T_TEMPSMALLFLAME
    MT_FLAME_SMALL,       // T_PERMSMALLFLAME
    MT_FLAME_LARGE_TEMP,  // T_TEMPLARGEFLAME
    MT_FLAME_LARGE,       // T_PERMLARGEFLAME
    MT_DEMON_MASH,        // T_DEMON_MASH
    MT_DEMON2_MASH,       // T_DEMON2_MASH
    MT_ETTIN_MASH,        // T_ETTIN_MASH
    MT_CENTAUR_MASH,      // T_CENTAUR_MASH
    MT_THRUSTFLOOR_UP,    // T_THRUSTSPIKEUP
    MT_THRUSTFLOOR_DOWN,  // T_THRUSTSPIKEDOWN
    MT_WRAITHFX4,         // T_FLESH_DRIP1
    MT_WRAITHFX5,         // T_FLESH_DRIP2
    MT_WRAITHFX2          // T_SPARK_DRIP
  );

//==============================================================================
//
// EVH_ThingActivate
//
//==============================================================================
function EVH_ThingActivate(tid: integer): boolean;

//==============================================================================
//
// EVH_ThingDeactivate
//
//==============================================================================
function EVH_ThingDeactivate(tid: integer): boolean;

//==============================================================================
//
// EVH_ThingRemove
//
//==============================================================================
function EVH_ThingRemove(tid: integer): boolean;

//==============================================================================
//
// EVH_ThingDestroy
//
//==============================================================================
function EVH_ThingDestroy(tid: integer): boolean;

//==============================================================================
//
// EVH_ThingProjectile
//
//==============================================================================
function EVH_ThingProjectile(args: PByteArray; gravity: boolean): boolean;

//==============================================================================
//
// EVH_ThingSpawn
//
//==============================================================================
function EVH_ThingSpawn(args: PByteArray; fog: boolean): boolean;

implementation

uses
  d_main,
  a_action,
  info,
  m_fixed,
  p_mobj,
  p_mobj_h,
  p_sounds,
  p_map,
  p_inter,
  tables,
  sounddata,
  udmf_telept,
  s_sound;

//==============================================================================
//
// EVH_ThingProjectile
//
//==============================================================================
function EVH_ThingProjectile(args: PByteArray; gravity: boolean): boolean;
var
  tid: integer;
  angle: angle_t;
  fineAngle: integer;
  speed: fixed_t;
  vspeed: fixed_t;
  moType: mobjtype_t;
  mobj: Pmobj_t;
  newMobj: Pmobj_t;
  searcher: integer;
begin
  result := false;
  searcher := -1;
  tid := args[0];
  moType := TranslateThingType[args[1]];
  if nomonsters and (mobjinfo[Ord(moType)].flags and MF_COUNTKILL <> 0) then
  begin // Don't spawn monsters if -nomonsters
    exit;
  end;

  angle := args[2] * $1000000;
  fineAngle := angle shr ANGLETOFINESHIFT;
  speed := args[3] * 8192;
  vspeed := args[4] * 8192;
  while P_FindMobjFromTID(tid, @searcher, mobj) <> nil do
  begin
    newMobj := P_SpawnMobj(mobj.x, mobj.y, mobj.z, Ord(moType));
    A_SeeSound(newMobj, newMobj);
    newMobj.target := mobj; // Originator
    newMobj.angle := angle;
    newMobj.momx := FixedMul(speed, finecosine[fineAngle]);
    newMobj.momy := FixedMul(speed, finesine[fineAngle]);
    newMobj.momz := vspeed;
    newMobj.flags2 := newMobj.flags2 or MF2_DROPPED; // Don't respawn
    if gravity then
    begin
      newMobj.flags := newMobj.flags and not MF_NOGRAVITY;
      newMobj.flags2 := newMobj.flags2 or MF2_LOGRAV;
      newMobj.flags_ex := newMobj.flags_ex or MF_EX_LOWGRAVITY;
    end;
    if P_CheckMissileSpawn(newMobj) then
      result := true;
  end;
end;

//==============================================================================
//
// EVH_ThingSpawn
//
//==============================================================================
function EVH_ThingSpawn(args: PByteArray; fog: boolean): boolean;
var
  tid: integer;
  angle: angle_t;
  mobj: Pmobj_t;
  newMobj: Pmobj_t;
  fogMobj: Pmobj_t;
  moType: mobjtype_t;
  searcher: integer;
  z: fixed_t;
begin
  result := false;
  searcher := -1;
  tid := args[0];
  moType := TranslateThingType[args[1]];
  if nomonsters and (mobjinfo[Ord(moType)].flags and MF_COUNTKILL <> 0) then
  begin // Don't spawn monsters if -nomonsters
    exit;
  end;

  angle := args[2] * $1000000;
  while P_FindMobjFromTID(tid, @searcher, mobj) <> nil do
  begin
    if (mobjinfo[Ord(moType)].flags2 and MF2_FLOATBOB <> 0) or (mobjinfo[Ord(moType)].flags_ex and MF_EX_FLOATBOB <> 0) then
      z := mobj.z - mobj.floorz
    else
      z := mobj.z;
    newMobj := P_SpawnMobj(mobj.x, mobj.y, z, Ord(moType));
    if not P_TestMobjLocation(newMobj) then
    begin // Didn't fit
      P_RemoveMobj(newMobj);
    end
    else
    begin
      newMobj.angle := angle;
      if fog then
      begin
        fogMobj := P_SpawnMobj(mobj.x, mobj.y, mobj.z + TELEFOGHEIGHT, Ord(MT_TFOG));
        S_StartSound(fogMobj, Ord(SFX_TELEPORT));
      end;
      newMobj.flags2 := newMobj.flags2 or MF2_DROPPED; // Don't respawn
      if (newMobj.flags2 and MF2_FLOATBOB <> 0) or (newMobj.flags_ex and MF_EX_FLOATBOB <> 0) then
        newMobj.special1 := newMobj.z - newMobj.floorz;
      result := true;
    end;
  end;
end;

//==============================================================================
//
// P_ActivateThing
//
//==============================================================================
function P_ActivateThing(mobj: Pmobj_t): boolean;
begin
  if mobj.flags and MF_COUNTKILL <> 0 then
  begin // Monster
    if mobj.flags2 and MF2_DORMANT <> 0 then
    begin
      mobj.flags2 := mobj.flags2 and not MF2_DORMANT;
      mobj.tics := 1;
      result := true;
    end
    else
      result := false;
    exit;
  end;

  case mobj._type of

    Ord(MT_ZTWINEDTORCH),
    Ord(MT_ZTWINEDTORCH_UNLIT):
      begin
        P_SetMobjState(mobj, S_ZTWINEDTORCH_1);
        S_StartSound(mobj, Ord(SFX_IGNITE));
      end;

    Ord(MT_ZWALLTORCH),
    Ord(MT_ZWALLTORCH_UNLIT):
      begin
        P_SetMobjState(mobj, S_ZWALLTORCH1);
        S_StartSound(mobj, Ord(SFX_IGNITE));
      end;

    Ord(MT_ZGEMPEDESTAL):
      begin
        P_SetMobjState(mobj, S_ZGEMPEDESTAL2);
      end;

    Ord(MT_ZWINGEDSTATUENOSKULL):
      begin
        P_SetMobjState(mobj, S_ZWINGEDSTATUENOSKULL2);
      end;

    Ord(MT_THRUSTFLOOR_UP),
    Ord(MT_THRUSTFLOOR_DOWN):
      begin
        if mobj.args[0] = 0 then
        begin
          S_StartSound(mobj, Ord(SFX_THRUSTSPIKE_LOWER));
          mobj.flags2 := mobj.flags2 and not MF2_DONTDRAW;
          if mobj.args[1] <> 0 then
            P_SetMobjState(mobj, S_BTHRUSTRAISE1)
          else
            P_SetMobjState(mobj, S_THRUSTRAISE1);
        end;
      end;

    Ord(MT_ZFIREBULL),
    Ord(MT_ZFIREBULL_UNLIT):
      begin
        P_SetMobjState(mobj, S_ZFIREBULL_BIRTH);
        S_StartSound(mobj, Ord(SFX_IGNITE));
      end;

    Ord(MT_ZBELL):
      begin
        if mobj.health > 0 then
        begin
          P_DamageMobj(mobj, nil, nil, 10); // 'ring' the bell
        end;
      end;

    Ord(MT_ZCAULDRON),
    Ord(MT_ZCAULDRON_UNLIT):
      begin
        P_SetMobjState(mobj, S_ZCAULDRON1);
        S_StartSound(mobj, Ord(SFX_IGNITE));
      end;

    Ord(MT_FLAME_SMALL):
      begin
        S_StartSound(mobj, Ord(SFX_IGNITE));
        P_SetMobjState(mobj, S_FLAME_SMALL1);
      end;

    Ord(MT_FLAME_LARGE):
      begin
        S_StartSound(mobj, Ord(SFX_IGNITE));
        P_SetMobjState(mobj, S_FLAME_LARGE1);
      end;

    Ord(MT_BAT_SPAWNER):
      begin
        P_SetMobjState(mobj, S_SPAWNBATS1);
      end;

  else
    begin
      result := false;
      exit;
    end;

  end;
  result := true;
end;

//==============================================================================
//
// EVH_ThingActivate
//
//==============================================================================
function EVH_ThingActivate(tid: integer): boolean;
var
  mobj: Pmobj_t;
  searcher: integer;
begin
  result := false;
  searcher := -1;
  while P_FindMobjFromTID(tid, @searcher, mobj) <> nil do
    if P_ActivateThing(mobj) then
      result := true;
end;

//==============================================================================
//
// P_DeactivateThing
//
//==============================================================================
function P_DeactivateThing(mobj: Pmobj_t): boolean;
begin
  if mobj.flags and MF_COUNTKILL <> 0 then
  begin // Monster
    if mobj.flags2 and MF2_DORMANT = 0 then
    begin
      mobj.flags2 := mobj.flags2 or MF2_DORMANT;
      mobj.tics := -1;
      result := true;
    end
    else
      result := false;
    exit;
  end;

  case mobj._type of

    Ord(MT_ZTWINEDTORCH),
    Ord(MT_ZTWINEDTORCH_UNLIT):
      begin
        P_SetMobjState(mobj, S_ZTWINEDTORCH_UNLIT);
      end;

    Ord(MT_ZWALLTORCH),
    Ord(MT_ZWALLTORCH_UNLIT):
      begin
        P_SetMobjState(mobj, S_ZWALLTORCH_U);
      end;

    Ord(MT_THRUSTFLOOR_UP),
    Ord(MT_THRUSTFLOOR_DOWN):
      begin
        if mobj.args[0] = 1 then
        begin
          S_StartSound(mobj, Ord(SFX_THRUSTSPIKE_RAISE));
          if mobj.args[1] <> 0 then
            P_SetMobjState(mobj, S_BTHRUSTLOWER)
          else
            P_SetMobjState(mobj, S_THRUSTLOWER);
        end;
      end;

    Ord(MT_ZFIREBULL),
    Ord(MT_ZFIREBULL_UNLIT):
      begin
        P_SetMobjState(mobj, S_ZFIREBULL_DEATH);
      end;

    Ord(MT_ZCAULDRON),
    Ord(MT_ZCAULDRON_UNLIT):
      begin
        P_SetMobjState(mobj, S_ZCAULDRON_U);
      end;

    Ord(MT_FLAME_SMALL):
      begin
        P_SetMobjState(mobj, S_FLAME_SDORM1);
      end;

    Ord(MT_FLAME_LARGE):
      begin
        P_SetMobjState(mobj, S_FLAME_LDORM1);
      end;

    Ord(MT_BAT_SPAWNER):
      begin
        P_SetMobjState(mobj, S_SPAWNBATS_OFF);
      end;

  else
    begin
      result := false;
      exit;
    end;

  end;

  result := true;
end;

//==============================================================================
//
// EVH_ThingDeactivate
//
//==============================================================================
function EVH_ThingDeactivate(tid: integer): boolean;
var
  mobj: Pmobj_t;
  searcher: integer;
begin
  result := false;
  searcher := -1;
  while P_FindMobjFromTID(tid, @searcher, mobj) <> nil do
    if P_DeactivateThing(mobj) then
      result := true;
end;

//==============================================================================
//
// EVH_ThingRemove
//
//==============================================================================
function EVH_ThingRemove(tid: integer): boolean;
var
  mobj: Pmobj_t;
  searcher: integer;
begin
  result := false;
  searcher := -1;
  while P_FindMobjFromTID(tid, @searcher, mobj) <> nil do
  begin
    result := true;
    if mobj._type = Ord(MT_BRIDGE) then
    begin
      A_BridgeRemove(mobj);
      exit;
    end;
    P_RemoveMobj(mobj);
  end;
end;

//==============================================================================
//
// EVH_ThingDestroy
//
//==============================================================================
function EVH_ThingDestroy(tid: integer): boolean;
var
  mobj: Pmobj_t;
  searcher: integer;
begin
  result := false;
  searcher := -1;
  while P_FindMobjFromTID(tid, @searcher, mobj) <> nil do
  begin
    if mobj.flags and MF_SHOOTABLE <> 0 then
    begin
      P_DamageMobj(mobj, nil, nil, 10000);
      result := true;
    end;
  end;
end;

end.
