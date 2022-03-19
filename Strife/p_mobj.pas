//------------------------------------------------------------------------------
//
//  DelphiStrife is a source port of the game Strife.
//
//  Based on:
//    - Linux Doom by "id Software"
//    - Chocolate Strife by "Simon Howard"
//    - DelphiDoom by "Jim Valavanis"
//
//  Copyright (C) 1993-1996 by id Software, Inc.
//  Copyright (C) 2005 Simon Howard
//  Copyright (C) 2010 James Haley, Samuel Villarreal
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
//  Map Objects, MObj, definition and handling.
//  Moving object handling. Spawn functions.
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit p_mobj;

interface

uses
  p_mobj_h,
  tables,
// We need the WAD data structure for Map things,
// from the THINGS lump.
  doomdata,
// States are tied to finite states are
//  tied to animation frames.
// Needs precompiled tables/data structures.
  info_h,
  p_udmf,
  m_fixed;

//==============================================================================
//
// P_SetMobjState
//
//==============================================================================
function P_SetMobjState(mobj: Pmobj_t; state: statenum_t): boolean;

//==============================================================================
//
// P_ExplodeMissile
//
//==============================================================================
procedure P_ExplodeMissile(mo: Pmobj_t);

//==============================================================================
//
// P_MobjThinker
//
//==============================================================================
procedure P_MobjThinker(mobj: Pmobj_t);

//==============================================================================
//
// P_SpawnMobj
//
//==============================================================================
function P_SpawnMobj(x, y, z: fixed_t; _type: integer; const mthing: Pmapthing_t = nil): Pmobj_t;

//==============================================================================
//
// P_RemoveMobj
//
//==============================================================================
procedure P_RemoveMobj(mobj: Pmobj_t);

//==============================================================================
//
// P_SpawnPlayer
//
//==============================================================================
function P_SpawnPlayer(mthing: Pmapthing_t; uthing: Pextrathing_t): Pmobj_t;

//==============================================================================
//
// P_SpawnMapThing
//
//==============================================================================
function P_SpawnMapThing(mthing: Pmapthing_t; uthing: Pextrathing_t): Pmobj_t;

//==============================================================================
//
// P_SpawnPuff
//
//==============================================================================
procedure P_SpawnPuff(x, y, z: fixed_t);

//==============================================================================
//
// P_SpawnMissile
//
//==============================================================================
function P_SpawnMissile(source: Pmobj_t; dest: Pmobj_t; _type: integer): Pmobj_t;

//==============================================================================
//
// P_SpawnMissileXYZ
//
//==============================================================================
function P_SpawnMissileXYZ(x, y, z: fixed_t; source: Pmobj_t; dest: Pmobj_t; _type: integer): Pmobj_t;

//==============================================================================
//
// P_SpawnMissileAngleZ
//
//==============================================================================
function P_SpawnMissileAngleZ(source: Pmobj_t; z: fixed_t; _type: integer; angle: angle_t;
  momz: fixed_t; speed: fixed_t): Pmobj_t;

//==============================================================================
//
// P_SpawnMissileAngleZSpeed
//
//==============================================================================
function P_SpawnMissileAngleZSpeed(source: Pmobj_t; z: fixed_t; _type: integer; angle: angle_t;
  momz: fixed_t; speed: fixed_t; owner: Pmobj_t): Pmobj_t;

//==============================================================================
//
// P_SpawnPlayerMissile
//
//==============================================================================
function P_SpawnPlayerMissile(source: Pmobj_t; _type: integer): Pmobj_t;

//==============================================================================
//
// P_RespawnSpecials
//
//==============================================================================
procedure P_RespawnSpecials;

//==============================================================================
//
// P_SpawnBlood
//
//==============================================================================
procedure P_SpawnBlood(x, y, z: fixed_t; damage: integer; const originator: Pmobj_t);

//==============================================================================
//
// P_SpawnGreenBlood
//
//==============================================================================
procedure P_SpawnGreenBlood(x, y, z: fixed_t; damage: integer);

//==============================================================================
//
// P_SpawnBlueBlood
//
//==============================================================================
procedure P_SpawnBlueBlood(x, y, z: fixed_t; damage: integer);

//==============================================================================
//
// P_SeekerMissile
//
//==============================================================================
function P_SeekerMissile(actor: Pmobj_t; thresh, turnMax: angle_t): boolean;

//==============================================================================
//
// P_HitFloor
//
//==============================================================================
function P_HitFloor(thing: Pmobj_t): integer;

//==============================================================================
//
// P_GetThingFloorType
//
//==============================================================================
function P_GetThingFloorType(thing: Pmobj_t): integer;

//==============================================================================
//
// MObj_Init
//
//==============================================================================
procedure MObj_Init;

//==============================================================================
//
// MObj_ShutDown
//
//==============================================================================
procedure MObj_ShutDown;

var
  iquehead: integer; // Initialized at p_setup
  iquetail: integer; // Initialized at p_setup

//==============================================================================
//
// P_FindMobjFromKey
//
//==============================================================================
function P_FindMobjFromKey(const key: LongWord): Pmobj_t;

//==============================================================================
//
// P_SpawnMortar
//
//==============================================================================
function P_SpawnMortar(source: Pmobj_t; _type: integer): Pmobj_t;

//==============================================================================
//
// P_SpawnFacingMissile
//
//==============================================================================
function P_SpawnFacingMissile(source: Pmobj_t; target: Pmobj_t; _type: integer): Pmobj_t;

//==============================================================================
//
// P_CheckMissileSpawn
//
//==============================================================================
function P_CheckMissileSpawn(th: Pmobj_t): boolean;

//==============================================================================
//
// P_SpawnSparkPuff
//
//==============================================================================
function P_SpawnSparkPuff(x, y, z: fixed_t): Pmobj_t;

implementation

uses
  d_delphi,
  c_cmds,
  sc_engine,
  doomstat,
  d_player,
  d_think,
  d_main,
  m_vectors,
  g_game,
  i_system,
  z_zone,
  m_rnd,
  doomdef,
  p_playertrace,
  p_gravity,
  p_local,
  p_map,
  p_maputl,
  p_mobjlist,
  p_tick,
  p_pspr,
  p_setup,
  p_common,
  p_terrain,
  p_sounds,
  p_3dfloors, // JVAL: 3d floors
  p_slopes, // JVAL: Slopes
  po_man,
  p_spec,
  p_inter,
  p_params,
  p_ladder,
  p_musinfo,
  p_bouncing,
  ps_main,
  r_defs,
  r_sky,
  r_main,
  r_data,
  r_translations,
  st_stuff,
  hu_stuff,
  s_sound,
  sounddata,
  info,
  info_rnd,
  info_common;

// From Chocolate-Doom
// Use a heuristic approach to detect infinite state cycles: Count the number
// of times the loop in P_SetMobjState() executes and exit with an error once
// an arbitrary very large limit is reached.
const
  MOBJ_CYCLE_LIMIT = 1000000;

//==============================================================================
//
// P_SetMobjState
// Returns true if the mobj is still present.
//
//==============================================================================
function P_SetMobjState(mobj: Pmobj_t; state: statenum_t): boolean;
var
  st: Pstate_t;
  cycle_counter: integer;
begin
  cycle_counter := 0;
  repeat
    if state = S_NULL then
    begin
      if mobj.flags_ex and MF_EX_DONOTREMOVE = 0 then // JVAL Do not remove missile
      begin
        mobj.state := @states[Ord(S_NULL)];
        P_RemoveMobj(mobj);
      end;
      result := false;
      exit;
    end;

    if mobj.validcount <> validcount then
    begin
      mobj.validcount := validcount;
      mobj.prevstate := mobj.state;
    end;

    st := @states[Ord(state)];

    mobj.state := st;
    mobj.tics := P_TicsFromState(st);
    mobj.sprite := st.sprite;
    mobj.frame := st.frame;

    // Modified handling.
    // Call action functions when the state is set
    if Assigned(st.action.acp1) then
    begin
      if st.params <> nil then
        st.params.Actor := mobj;
      st.action.acp2(mobj, nil);
    end;

    state := st.nextstate;

    inc(cycle_counter);
    if cycle_counter > MOBJ_CYCLE_LIMIT then
      I_Error('P_SetMobjState(): Infinite state cycle detected in object "%s"!', [Info_GetMobjName(mobj.info)]);
  until mobj.tics <> 0;

  result := true;
end;

//==============================================================================
//
// P_ExplodeMissile
//
// [STRIFE] Removed randomization of deathstate tics
//
//==============================================================================
procedure P_ExplodeMissile(mo: Pmobj_t);
begin
  mo.momx := 0;
  mo.momy := 0;
  mo.momz := 0;

  P_SetMobjState(mo, statenum_t(mobjinfo[Ord(mo._type)].deathstate));

  if mo.tics < 1 then
    mo.tics := 1;

  mo.flags := mo.flags and not MF_MISSILE;

  A_DeathSound1(mo);
end;

//
// P_XYMovement
//
// [STRIFE] Modifications for:
// * No SKULLFLY logic (replaced by BOUNCE flag)
// * Missiles can activate G1/GR line types
// * Player walking logic
// * Air friction for players
//
const
  STOPSPEED = $1000;
  FRICTION = $e800;
  AIRFRICTION = $fff0; // [STRIFE]

//==============================================================================
//
// P_XYMovement
//
//==============================================================================
procedure P_XYMovement(mo: Pmobj_t);
var
  ptryx: fixed_t;
  ptryy: fixed_t;
  player: Pplayer_t;
  xmove: fixed_t;
  ymove: fixed_t;
  wasonfloorz: boolean;
  wasonslope: boolean;
  oldsector: Psector_t;
begin
  wasonfloorz := mo.z <= mo.floorz;
  oldsector := Psubsector_t(mo.subsector).sector;

  // JVAL: 20220222 - Wind thrust
  if mo.flags4_ex and MF4_EX_WINDTHRUST <> 0 then
    if oldsector.windthrust <> 0 then
      P_ThrustMobj(mo, oldsector.windangle, oldsector.windthrust);

  wasonslope := oldsector.renderflags and SRF_SLOPED <> 0;

  player := mo.player;

  if mo.flags3_ex and MF3_EX_NOMAXMOVE = 0 then
  begin
    if mo.momx > MAXMOVE then
      mo.momx := MAXMOVE
    else if mo.momx < -MAXMOVE then
      mo.momx := -MAXMOVE;

    if mo.momy > MAXMOVE then
      mo.momy := MAXMOVE
    else if mo.momy < -MAXMOVE then
      mo.momy := -MAXMOVE;
  end;

  xmove := mo.momx;
  ymove := mo.momy;

  repeat
    if (xmove > MAXMOVE div 2) or (ymove > MAXMOVE div 2) then
    begin
      ptryx := mo.x + xmove div 2;
      ptryy := mo.y + ymove div 2;
      xmove := _SHR1(xmove);
      ymove := _SHR1(ymove);
    end
    else
    begin
      ptryx := mo.x + xmove;
      ptryy := mo.y + ymove;
      xmove := 0;
      ymove := 0;
    end;

    // JVAL: Slopes
    // JVAL 20191209 - Fix 3d floor problem
    tmfloorz := P_3dFloorHeight(ptryx, ptryy, mo.z);
    tmceilingz := P_3dCeilingHeight(ptryx, ptryy, mo.z);

    if not P_TryMove(mo, ptryx, ptryy) then
    begin
      // blocked move
      if mo.player <> nil then
      begin
        if not P_LadderMove(mo) then
          P_SlideMove(mo); // try to slide along it
      end
      // JVAL: 20210209 - Sliding monsters
      else if (mo.flags3_ex and (MF3_EX_SLIDING or MF3_EX_SLIDE or MF3_EX_SLIDEONWALLS) <> 0) and (mo.flags and MF_MISSILE = 0) then
      begin
        P_SlideMove(mo); // try to slide along it
      end
      // JVAL: 20211121 - New bounch on walls mechanics
      else if (G_PlayingEngineVersion >= VERSION207) and (mo.flags3_ex and MF3_EX_WALLBOUNCE <> 0) and (tmbounceline <> nil) and
        (mo.flags and MF_BOUNCE = 0) then
      begin
        P_WallBounceMobj(mo, tmbounceline);
        xmove := 0;
        ymove := 0;
      end
      // villsa [STRIFE] check for bouncy missiles
      else if (mo.flags and MF_BOUNCE <> 0) or (mo.flags3_ex and MF3_EX_WALLBOUNCE <> 0) then
      begin
        mo.momx := mo.momx div 8;
        mo.momy := mo.momy div 8;

        if P_TryMove(mo, mo.x - xmove, ymove + mo.y) then
          mo.momy := -mo.momy
        else
          mo.momx := -mo.momx;

        xmove := 0;
        ymove := 0;
      end
      else if mo.flags and MF_MISSILE <> 0 then
      begin
        // haley 20110203: [STRIFE]
        // This modification allows missiles to activate shoot specials.
        // *** BUG: In vanilla Strife the second condition is simply
        // if(numspechit). However, numspechit can be negative, and
        // when it is, this accesses spechit[-2]. This always causes the
        // DOS exe to read from NULL, and the 'special' value there (in
        // DOS 6.22 at least) is 0x70, which does nothing.
        if blockingline <> nil then
        begin
          if blockingline.flags and ML_TRIGGERSCRIPTS <> 0 then
            if mo.flags2_ex and MF2_EX_DONTRUNSCRIPTS = 0 then
              PS_EventShootLine(mo, pDiff(blockingline, lines, SizeOf(line_t)), P_PointOnLineSide(mo.x, mo.y, blockingline));

          if blockingline.special <> 0 then
            P_ShootSpecialLine(mo, blockingline);
        end;

        if numspechit > 0 then
        begin
          if spechit[numspechit - 1].flags and ML_TRIGGERSCRIPTS <> 0 then
            if mo.flags2_ex and MF2_EX_DONTRUNSCRIPTS = 0 then
              PS_EventShootLine(mo, pDiff(spechit[numspechit - 1], lines, SizeOf(line_t)), P_PointOnLineSide(mo.x, mo.y, spechit[numspechit - 1]));

          P_ShootSpecialLine(mo, spechit[numspechit - 1]);
        end;

        // explode a missile
        if (ceilingline <> nil) and
           (ceilingline.backsector <> nil) and
           (ceilingline.backsector.ceilingpic = skyflatnum) then
        begin
          // Hack to prevent missiles exploding
          // against the sky.
          // Does not handle sky floors.
          P_RemoveMobj(mo);
          exit;
        end;
        P_ExplodeMissile(mo);
      end
      else
      begin
        mo.momx := 0;
        mo.momy := 0;
      end;
    end;
  until not ((xmove <> 0) or (ymove <> 0));

  // slow down
  if (player <> nil) and  (player.cheats and CF_NOMOMENTUM <> 0) then
  begin
    // debug option for no sliding at all
    mo.momx := 0;
    mo.momy := 0;
    exit;
  end;

  // villsa [STRIFE] replace skullfly flag with MF_BOUNCE
  if mo.flags and (MF_MISSILE or MF_BOUNCE) <> 0 then
    exit; // no friction for missiles ever

  if mo.flags3_ex and MF3_EX_BOUNCE <> 0 then
    if G_PlayingEngineVersion <= VERSION206 then
      exit; // no friction for bouncing objects

  // haleyjd 20110224: [STRIFE] players experience friction even in the air,
  // although less than when on the ground. With this fix, the 1.2-and-up
  // IWAD demo is now in sync!
  if G_PlayingEngineVersion > VERSION203 then
    if wasonfloorz and wasonslope and (oldsector = Psubsector_t(mo.subsector).sector) then
      if oldsector.flags and SF_SLIPSLOPEDESCENT = 0 then
        mo.z := mo.floorz;

  if mo.z > mo.floorz then
  begin
    if player <> nil then
    begin
      mo.momx := FixedMul(mo.momx, AIRFRICTION);
      mo.momy := FixedMul(mo.momy, AIRFRICTION);
    end;
    exit; // no friction when airborne
  end;

  if mo.flags and MF_CORPSE <> 0 then
  begin
    // do not stop sliding
    //  if halfway off a step with some momentum
    if (mo.momx > FRACUNIT div 4) or
       (mo.momx < -FRACUNIT div 4) or
       (mo.momy > FRACUNIT div 4) or
       (mo.momy < -FRACUNIT div 4) then
    begin
      if mo.floorz <> P_3dFloorHeight(mo) then // JVAL: 3d floors
        exit;
    end;
  end;

  if (mo.momx > -STOPSPEED) and
     (mo.momx < STOPSPEED) and
     (mo.momy > -STOPSPEED) and
     (mo.momy < STOPSPEED) and
     ((player = nil) or
      ((player.cmd.forwardmove = 0) and
       (player.cmd.sidemove = 0))) then
  begin
    // if in a walking frame, stop moving
    // villsa [STRIFE]: different player state (haleyjd - verified 20110202)
    if (player <> nil) and
       (LongWord((pDiff(player.mo.state, @states[0], SizeOf(states[0]))) - Ord(S_PLAY_01)) < 4) then
      P_SetMobjState(player.mo, S_PLAY_00);

    mo.momx := 0;
    mo.momy := 0;
  end
  else
  begin
    mo.momx := FixedMul(mo.momx, FRICTION);
    mo.momy := FixedMul(mo.momy, FRICTION);
  end;
end;

//==============================================================================
//
// P_ZMovement
//
// [STRIFE] Modifications for:
// * 3D Object Clipping
// * Different momz handling
// * No SKULLFLY logic (replaced with BOUNCE)
// * Missiles don't hit sky flats
//
//==============================================================================
procedure P_ZMovement(mo: Pmobj_t);
var
  dist: fixed_t;
  delta: fixed_t;
  ceilz: fixed_t;
  grav: integer;
  momomz: fixed_t;
  laddertics: integer;
  player: Pplayer_t;
begin
  laddertics := 0;
  player := Pplayer_t(mo.player);
  if player <> nil then
    if player.laddertics > 0 then
    begin
      Dec(player.laddertics);
      laddertics := player.laddertics;
      if laddertics = 0 then
        mo.momz := 0;
    end;

  // check for smooth step up
  if (player <> nil) and (mo.z < mo.floorz) and (laddertics = 0) then
  begin
    player.viewheight := player.viewheight - (mo.floorz - mo.z);
    player.deltaviewheight :=
      _SHR3(PVIEWHEIGHT - player.crouchheight - player.viewheight);
  end;

  // adjust height
  if laddertics > 0 then
  begin
    mo.z := mo.z + mo.momz;
    mo.momz := (mo.momz * 7) div 8;
    if mo.momz < FRACUNIT div 8 then
      mo.momz := 0;
  end
  else
    mo.z := mo.z + mo.momz;

  // adjust height
  // villsa [STRIFE] check for things standing on top of other things
  if not P_CheckPositionZ(mo, mo.z + mo.momz) then
  begin
    if mo.momz >= 0 then
      mo.ceilingz := mo.height + mo.z
    else
      mo.floorz := mo.z;
  end;

  if (mo.flags and MF_FLOAT <> 0) and (mo.target <> nil) then
  begin
    // float down towards target if too close
    if mo.flags and MF_INFLOAT = 0 then
    begin
      dist := P_AproxDistance(mo.x - mo.target.x, mo.y - mo.target.y);

      delta := mo.target.z + (mo.height div 2) - mo.z;

      if (delta < 0) and (dist < -(delta * 3)) then
        mo.z := mo.z - P_FloatSpeed(mo)
      else if (delta > 0) and (dist < (delta * 3)) then
        mo.z := mo.z + P_FloatSpeed(mo);
    end;
  end;

  // clip movement
  if mo.z <= mo.floorz then
  begin
    // hit the floor

    if (mo.flags and MF_BOUNCE <> 0) and (mo.flags3_ex and MF3_EX_FLOORBOUNCE <> 0) then
    begin
      // villsa [STRIFE] affect reactiontime
      // momz is also shifted by 1
      mo.momz := -mo.momz div 2;
      mo.reactiontime := mo.reactiontime div 2;

      if G_PlayingEngineVersion >= VERSION207 then
        if mo.momz + mo.z <= mo.floorz then
        begin
          mo.momz := 0;
          mo.z := mo.floorz;
        end;

      // villsa [STRIFE] get terrain type
      if P_GetTerrainType(mo) <> FLOOR_SOLID then
        mo.flags := mo.flags and not MF_BOUNCE;
    end;

    momomz := mo.momz;
    if mo.momz < 0 then
    begin
      if (player <> nil) and (mo.momz < -P_GetMobjGravity(mo) * 8) then
      begin
        // Squat down.
        // Decrease viewheight for a moment
        // after hitting the ground (hard),
        // and utter appropriate sound.
        player.deltaviewheight := _SHR3(mo.momz);
        // JVAL: 20211101 - Crouch
        if G_PlayingEngineVersion >= VERSION207 then
          player.deltaviewheight := FixedMul(player.deltaviewheight, FixedDiv(mo.height, mo.info.height));

        // villsa [STRIFE] fall damage
        // haleyjd 09/18/10: Repaired calculation
        if mo.momz < -20 * FRACUNIT then
          P_DamageMobj(mo, nil, mo, -(mo.momz div 25000));

        // haleyjd 20110224: *Any* fall centers your view, not just
        // damaging falls (moved outside the above if).
        Pplayer_t(mo.player).centerview := true;

        if leveltime > player.nextoof then
        begin
          S_StartSound(mo, Ord(sfx_oof));
          player.nextoof := leveltime + 4 * TICRATE;
        end;
      end;
      mo.momz := 0;
    end;

    if mo.z - momomz > mo.floorz then
    begin // Spawn splashes, etc.
      P_HitFloor(mo);
    end;

    mo.z := mo.floorz;

    if (mo.flags and MF_MISSILE <> 0) and (mo.flags and (MF_NOCLIP or MF_BOUNCE) = 0) then
    begin
      P_ExplodeMissile(mo);
      exit;
    end;

    if (mo.info.crashstate > 0) and
       (mo.flags and MF_CORPSE <> 0) then
    begin
      P_SetMobjState(mo, statenum_t(mo.info.crashstate));
      exit;
    end;

  end
  else if mo.flags and MF_NOGRAVITY = 0 then
  begin
    grav := P_GetMobjGravity(mo);
    // JVAL
    // Low gravity cheat
    if player <> nil then
      if player.cheats and CF_LOWGRAVITY <> 0 then
        grav := grav div 2;

    if mo.momz = 0 then
      mo.momz := -grav * 2
    else
      mo.momz := mo.momz - grav;

    // JVAL
    // Low gravity flag
    if mo.flags_ex and MF_EX_LOWGRAVITY <> 0 then
      mo.momz := mo.momz div 2
    else if mo.flags2_ex and MF2_EX_MEDIUMGRAVITY <> 0 then
      mo.momz := mo.momz * 3 div 4;

  end;

  ceilz := mo.ceilingz + P_SectorJumpOverhead(Psubsector_t(mo.subsector).sector);

  if mo.z + mo.height > ceilz then
  begin
    // villsa [STRIFE] replace skullfly flag with MF_BOUNCE
    if mo.flags and MF_BOUNCE <> 0 then
    begin
      // villsa [STRIFE] affect reactiontime
      // momz is also shifted by 1
      mo.momz := -mo.momz div 2;
      mo.reactiontime := mo.reactiontime div 2;
    end;

    // hit the ceiling
    if mo.momz > 0 then
    if mo.momz > 0 then
    begin
      if mo.flags3_ex and MF3_EX_CEILINGBOUNCE <> 0 then
        mo.momz := -mo.momz div 2
      else
        mo.momz := 0;
    end;
    mo.z := ceilz - mo.height;

    if (mo.flags and MF_MISSILE <> 0) and (mo.flags and (MF_NOCLIP or MF_BOUNCE) = 0) then
    begin
      P_ExplodeMissile(mo);
      exit;
    end;
  end;
end;

//==============================================================================
//
// P_NightmareRespawn
//
// [STRIFE] Modifications for:
// * Destination fog z coordinate
// * Restoration of all Strife mapthing flags
//
//==============================================================================
procedure P_NightmareRespawn(mobj: Pmobj_t);
var
  x: fixed_t;
  y: fixed_t;
  z: fixed_t;
  ss: Psubsector_t;
  mo: Pmobj_t;
  mthing: Pmapthing_t;
  h: fixed_t; // JVAL: 3d floors
begin
  x := mobj.spawnpoint.x * FRACUNIT;
  y := mobj.spawnpoint.y * FRACUNIT;

  // somthing is occupying it's position?
  if not P_CheckPosition(mobj, x, y) then
    exit; // no respwan

  // spawn a teleport fog at old spot
  // because of removal of the body?
  mo := P_SpawnMobj(mobj.x, mobj.y, mobj.z, Ord(MT_TFOG));
  // initiate teleport sound
  S_StartSound(mo, Ord(sfx_telept));

  // spawn a teleport fog at the new spot
  // haleyjd [STRIFE]: Uses ONFLOORZ instead of ss->sector->floorheight
  ss := R_PointInSubsector(x, y);

// JVAL: 3d floors
  h := ONFLOORZ;
  if ss.sector.midsec >= 0 then
    if mobj.spawnpoint.options and MTF_ONMIDSECTOR <> 0 then
      h := sectors[ss.sector.midsec].ceilingheight;

  mo := P_SpawnMobj(x, y, h, Ord(MT_TFOG));

  S_StartSound(mo, Ord(sfx_telept));

  // spawn the new monster
  mthing := @(mobj.spawnpoint);

  // spawn it
  if mobj.flags and MF_SPAWNCEILING <> 0 then
    z := ONCEILINGZ
  else if mobj.flags_ex and MF_EX_SPAWNFLOAT <> 0 then
    z := ONFLOATZ
  else
    z := ONFLOORZ;

  // inherit attributes from deceased one
  mo := P_SpawnMobj(x, y, z, Ord(mobj._type), mthing);
  mo.spawnpoint := mobj.spawnpoint;
  if mo.flags2_ex and MF2_EX_PRECISESPAWNANGLE <> 0 then
    mo.angle := ANG1 * mthing.angle
  else
    mo.angle := ANG45 * (mthing.angle div 45);

  if mthing.options and MTF_DONOTTRIGGERSCRIPTS <> 0 then
    mobj.flags2_ex := mobj.flags2_ex or MF2_EX_DONTRUNSCRIPTS;

  if mthing.options and MTF_AMBUSH <> 0 then
    mo.flags := mo.flags or MF_AMBUSH;
  if mthing.options and MTF_STAND <> 0 then       // [STRIFE] Standing mode, for NPCs
    mo.flags := mo.flags or MF_STAND;
  if mthing.options and MTF_ALLY <> 0 then      // [STRIFE] Allies
    mo.flags := mo.flags or MF_ALLY;
  if mthing.options and MTF_FRIEND <> 0 then
    mo.flags2_ex := mo.flags2_ex or MF2_EX_FRIEND;
  if mthing.options and MTF_TRANSLUCENT <> 0 then // [STRIFE] Translucent object
    mo.flags := mo.flags or MF_SHADOW;
  if mthing.options and MTF_MVIS <> 0 then        // [STRIFE] Alt. Translucency
    mo.flags := mo.flags or MF_MVIS;

  mo.reactiontime := 18;

  // remove the old monster,
  P_RemoveMobj(mobj);
end;

//==============================================================================
//
// P_MobjThinker
//
// [STRIFE] Modified for:
// * Terrain effects
// * Stonecold cheat
// * Altered skill 5 respawn behavior
//
//==============================================================================
procedure P_MobjThinker(mobj: Pmobj_t);
begin
  // JVAL: Clear just spawned flag
  mobj.flags2_ex := mobj.flags2_ex and not MF2_EX_JUSTAPPEARED;

  // momentum movement
  if (mobj.momx <> 0) or
     (mobj.momy <> 0) then
  begin
    P_XYMovement(mobj);

    if P_ThinkerIsRemoved(@mobj.thinker) then
      exit; // mobj was removed

    // villsa [STRIFE] terrain clipping
    if P_GetTerrainType(mobj) = FLOOR_SOLID then
    begin
      mobj.flags := mobj.flags and not MF_FEETCLIPPED;
      mobj.floorclip := 0;
    end
    else
    begin
      mobj.flags := mobj.flags or MF_FEETCLIPPED;
      mobj.floorclip := FOOTCLIPSIZE;
    end;

  end;

  if mobj.flags_ex and MF_EX_FLOATBOB <> 0 then
  begin
    mobj.z := mobj.floorz + FloatBobOffsets[mobj.bob];
    mobj.bob := (mobj.bob + 1) and FLOATBOBMASK;
  end
  else if ((mobj.z <> mobj.floorz) and (mobj.flags and MF_NOGRAVITY = 0)) or (mobj.momz <> 0) then
  begin
    P_ZMovement(mobj);

    if P_ThinkerIsRemoved(@mobj.thinker) then
      exit; // mobj was removed

    // villsa [STRIFE] terrain clipping and sounds
    if P_GetTerrainType(mobj) = FLOOR_SOLID then
    begin
      mobj.flags := mobj.flags and not MF_FEETCLIPPED;
      mobj.floorclip := 0;
    end
    else
    begin
      S_StartSound(mobj, Ord(sfx_wsplsh));
//      if mobj.flags2_ex and MF2_EX_ // JVAL SOS
      mobj.flags := mobj.flags or MF_FEETCLIPPED;
      mobj.floorclip := FOOTCLIPSIZE;
    end;

  end;

  // cycle through states,
  // calling action functions at transitions
  if mobj.tics <> -1 then
  begin
    dec(mobj.tics);

    // villsa [STRIFE] stonecold cheat
    if stonecold then
      if mobj.flags and MF_COUNTKILL <> 0 then
        P_DamageMobj(mobj, mobj, mobj, 10);

    // you can cycle through multiple states in a tic
    if mobj.tics = 0 then
      if not P_SetMobjState(mobj, mobj.state.nextstate) then
        exit; // freed itself
  end
  else
  begin
    // check for nightmare respawn
    if mobj.flags and MF_COUNTKILL = 0 then
      exit;

    if not respawnmonsters then
      exit;

    mobj.movecount := mobj.movecount + 1;

    // haleyjd [STRIFE]: respawn time increased from 12 to 16
    if mobj.movecount < 16 * TICRATE then
      exit;

    if leveltime and 31 <> 0 then
      exit;

    if P_Random > 4 then
      exit;

    // haleyjd [STRIFE]: NOTDMATCH things don't respawn
    if mobj.flags and MF_NOTDMATCH <> 0 then
      exit;

    P_NightmareRespawn(mobj);
  end;
end;

//==============================================================================
//
// P_SpawnMobj
//
// [STRIFE] Modifications to reactiontime and for terrain types.
//
//==============================================================================
function P_SpawnMobj(x, y, z: fixed_t; _type: integer; const mthing: Pmapthing_t = nil): Pmobj_t;
var
  mobj: Pmobj_t;
  st: Pstate_t;
  info: Pmobjinfo_t;
  space: fixed_t;
  sec: Psector_t;
  msec: Psector_t;  // JVAL: 3d floors
  lowfloorheight, hifloorheight: fixed_t; // JVAL: 3d floors
  onmidfloor: Boolean;
  spawnfloorheight, spawnceilingheight: fixed_t;  // JVAL: Slopes
begin
  mobj := Z_Malloc(SizeOf(mobj_t), PU_LEVEL, nil);

  ZeroMemory(mobj, SizeOf(mobj_t));

  mobj.key := P_GenGlobalMobjKey;

  info := @mobjinfo[_type];
  mobj._type := _type;
  mobj.info := info;
  mobj.x := x;
  mobj.y := y;
  mobj.radius := info.radius;
  mobj.height := info.height;
  mobj.flags := info.flags;
  mobj.flags_ex := info.flags_ex;
// JVAL: Set MF2_EX_JUSTAPPEARED flag
  mobj.flags2_ex := info.flags2_ex or MF2_EX_JUSTAPPEARED;
  mobj.flags3_ex := info.flags3_ex;
  mobj.flags4_ex := info.flags4_ex;
  mobj.flags5_ex := info.flags5_ex;
  mobj.flags6_ex := info.flags6_ex;
  mobj.scale := info.scale;
  mobj.gravity := info.gravity;
  mobj.pushfactor := info.pushfactor;
  mobj.friction := info.friction;
  mobj.renderstyle := info.renderstyle;
  mobj.alpha := info.alpha;
  if mobj.flags_ex and MF_EX_FLOATBOB <> 0 then
    mobj.bob := N_Random and FLOATBOBMASK;
  mobj.health := info.spawnhealth;
  mobj.mass := info.mass;
  mobj.WeaveIndexXY := info.WeaveIndexXY;
  mobj.WeaveIndexZ := info.WeaveIndexZ;
  mobj.painchance := info.painchance;
  mobj.spriteDX := info.spriteDX;
  mobj.spriteDY := info.spriteDY;
  // mbf21+
  mobj.infighting_group := info.infighting_group;
  mobj.projectile_group := info.projectile_group;
  mobj.splash_group := info.splash_group;
  mobj.bloodcolor := info.bloodcolor;
  mobj.translationname := info.translationname;
  R_InitMobjTranslation(mobj);

  mobj.reactiontime := info.reactiontime;

  mobj.lastlook := P_Random mod MAXPLAYERS;
  // do not set the state with P_SetMobjState,
  // because action routines can not be called yet

  // Set the state, but do not use P_SetMobjState, because action
  // routines can't be called yet.  If the spawnstate has an action
  // routine, it will not be called.
  st := @states[info.spawnstate];

  mobj.state := st;
  mobj.prevstate := st;
  mobj.validcount := validcount;
  mobj.tics := P_TicsFromState(st);
  mobj.sprite := st.sprite;
  mobj.frame := st.frame;
  mobj.touching_sectorlist := nil; // NULL head of sector list // phares 3/13/98

  // set subsector and/or block links
  P_SetThingPosition(mobj);

  sec := Psubsector_t(mobj.subsector).sector;

  // JVAL: Slopes
  spawnfloorheight := P_FloorHeight(sec, x, y);
  spawnceilingheight := P_CeilingHeight(sec, x, y);
  mobj.floorz := spawnfloorheight;  // JVAL: Slopes
  mobj.ceilingz := spawnceilingheight;  // JVAL: Slopes
  onmidfloor := false;

// JVAL: 3d floors
  if sec.midsec >= 0 then
  begin
    msec := @sectors[sec.midsec];
    if mthing <> nil then
      if mthing.options and MTF_ONMIDSECTOR <> 0 then
        onmidfloor := true;
    if onmidfloor then
      mobj.floorz := msec.ceilingheight
    else if not onmidfloor and (z = ONCEILINGZ) then
      mobj.ceilingz := msec.floorheight
    else if z = ONFLOATZ then
    begin
      lowfloorheight := msec.floorheight - spawnfloorheight;  // JVAL: Slopes
      hifloorheight := spawnceilingheight - msec.ceilingheight; // JVAL: Slopes
      if lowfloorheight < mobj.info.height then
        mobj.floorz := msec.ceilingheight
      else if hifloorheight < mobj.info.height then
        mobj.ceilingz := msec.floorheight
      else if mthing = nil then
      begin
        if N_Random < Round(lowfloorheight / (lowfloorheight + hifloorheight) * 255) then
          mobj.ceilingz := msec.floorheight
        else
          mobj.floorz := msec.ceilingheight;
      end;
    end
    else
    begin
      if z > msec.floorheight then
        mobj.floorz := msec.ceilingheight
      else
        mobj.ceilingz := msec.floorheight;
    end;
  end
  else
    msec := nil;

  if z = ONFLOORZ then
  begin
    mobj.z := mobj.floorz;
    // villsa [STRIFE]
    if P_GetTerrainType(mobj) <> FLOOR_SOLID then
    begin
      mobj.flags := mobj.flags or MF_FEETCLIPPED;
      mobj.floorclip := FOOTCLIPSIZE;
    end
    else
      mobj.floorclip := 0;
  end
  else if z = ONCEILINGZ then
    mobj.z := mobj.ceilingz - mobj.info.height
  else if z = ONFLOATZ then
  begin
    space := mobj.ceilingz - mobj.info.height - mobj.floorz;
    if space > 48 * FRACUNIT then
    begin
      space := space - 40 * FRACUNIT;
      mobj.z := FixedMul(space, N_Random * 256) + mobj.floorz + 40 * FRACUNIT
    end
    else
      mobj.z := mobj.floorz
  end
  else
    mobj.z := z;

  if (msec <> nil) or (sec.renderflags and SRF_SLOPED <> 0) then  // JVAL: Slopes
  begin
    if mobj.z > mobj.ceilingz - mobj.info.height then
      mobj.z := mobj.ceilingz - mobj.info.height;
    if mobj.z < mobj.floorz then
      mobj.z := mobj.floorz;

    if (mobj.flags and MF_FEETCLIPPED <> 0) and
       (P_GetThingFloorType(mobj) > FLOOR_SOLID) and
       (mobj.z = mobj.floorz) then
      mobj.floorclip := FOOTCLIPSIZE
    else
      mobj.floorclip := 0;
  end
  else
  begin
    if (mobj.flags and MF_FEETCLIPPED <> 0) and
       (P_GetThingFloorType(mobj) > FLOOR_SOLID) and
       (mobj.z = sec.floorheight) then
      mobj.floorclip := FOOTCLIPSIZE
    else
      mobj.floorclip := 0;
  end;

  mobj.momz := mobj.info.vspeed;

  mobj.thinker._function.acp1 := @P_MobjThinker;

  P_AddThinker(@mobj.thinker);

  mobj.prevx := mobj.x;
  mobj.prevy := mobj.y;
  mobj.prevz := mobj.z;
  mobj.nextx := mobj.x;
  mobj.nexty := mobj.y;
  mobj.nextz := mobj.z;
  mobj.prevangle := mobj.angle;
  mobj.nextangle := mobj.angle;
  mobj.intrplcnt := 0;

  result := mobj;
end;

//
// P_RemoveMobj
//
// [STRIFE] Modifications for item respawn timing
//
const
// Time interval for item respawning.
  ITEMQUESIZE = 128;

var
  itemrespawnque: array[0..ITEMQUESIZE - 1] of mapthing_t;
  itemrespawntime: array[0..ITEMQUESIZE - 1] of integer;

//==============================================================================
//
// P_RemoveMobj
//
//==============================================================================
procedure P_RemoveMobj(mobj: Pmobj_t);
var
  _type: word;
  options: smallint;
begin
  if ((mobj.flags and MF_SPECIAL) <> 0) and
     ((mobj.flags and MF_DROPPED) = 0) then
  begin
    itemrespawnque[iquehead] := mobj.spawnpoint;
    itemrespawntime[iquehead] := leveltime + 30 * TICRATE;  // [STRIFE]

    // [STRIFE] haleyjd 20130915
    // -random parameter affects the behavior of respawning items here.
    if randomparm and (iquehead <> iquetail) then
    begin
      _type := itemrespawnque[iquehead]._type;
      options := itemrespawnque[iquehead].options;

      // swap the type and options of iquehead and iquetail
      itemrespawnque[iquehead]._type := itemrespawnque[iquetail]._type;
      itemrespawnque[iquehead].options := itemrespawnque[iquetail].options;
      itemrespawnque[iquetail]._type := _type;
      itemrespawnque[iquetail].options := options;
    end;

    iquehead := (iquehead + 1) and (ITEMQUESIZE - 1);

    // lose one off the end?
    if iquehead = iquetail then
      iquetail := (iquetail + 1) and (ITEMQUESIZE - 1);
  end;

  // unlink from sector and block lists
  P_UnsetThingPosition(mobj);

  // Delete all nodes on the current sector_list               phares 3/16/98

  if sector_list <> nil then
    while sector_list <> nil do
      sector_list := P_DelSecnode(sector_list);

  if mobj.flags4_ex and MF4_EX_ALWAYSFINISHSOUND <> 0 then
    S_UnlinkSound(mobj)
  else if mobj.flags4_ex and MF4_EX_NEVERFINISHSOUND <> 0 then
    S_StopSound(mobj)
  // From Woof: [FG] removed map objects may finish their sounds
  else if full_sounds then
    S_UnlinkSound(mobj)
  else
    // stop any playing sound
    S_StopSound(mobj);

  P_RemoveMobjCustomParams(mobj.customparams);

  // free block
  P_RemoveThinker(Pthinker_t(mobj));
end;

//==============================================================================
//
// P_RespawnSpecials
//
// [STRIFE] modification to item respawn time handling
//
//==============================================================================
procedure P_RespawnSpecials;
var
  x: fixed_t;
  y: fixed_t;
  z: fixed_t;
  ss: Psubsector_t;
  mo: Pmobj_t;
  mthing: Pmapthing_t;
  i: integer;
begin
  // only respawn items in deathmatch
  if deathmatch <> 2 then
    exit; //

  // nothing left to respawn?
  if iquehead = iquetail then
    exit;

  // haleyjd [STRIFE]: 30 second wait is not accounted for here, see above.
  if leveltime < itemrespawntime[iquetail] then
    exit;

  mthing := @itemrespawnque[iquetail];

  x := mthing.x * FRACUNIT;
  y := mthing.y * FRACUNIT;

  // spawn a teleport fog at the new spot
  ss := R_PointInSubsector(x, y);
  mo := P_SpawnMobj(x, y, ss.sector.floorheight, Ord(MT_IFOG), mthing);
  S_StartSound(mo, Ord(sfx_itmbk));

  // find which type to spawn
  i := 0;
  while i < nummobjtypes do
  begin
    if mthing._type = mobjinfo[i].doomednum then
      break;
    inc(i);
  end;

  // spawn it
  if mobjinfo[i].flags and MF_SPAWNCEILING <> 0 then
    z := ONCEILINGZ
  else if mobjinfo[i].flags_ex and MF_EX_SPAWNFLOAT <> 0 then
    z := ONFLOATZ
  else
    z := ONFLOORZ;

  mo := P_SpawnMobj(x, y, z, i, mthing);
  mo.spawnpoint := mthing^;
  if mo.flags2_ex and MF2_EX_PRECISESPAWNANGLE <> 0 then
    mo.angle := ANG1 * mthing.angle
  else
    mo.angle := ANG45 * (mthing.angle div 45);

  // pull it from the que
  iquetail := (iquetail + 1) and (ITEMQUESIZE - 1);
end;

//==============================================================================
//
// P_SpawnPlayer
// Called when a player is spawned on the level.
// Most of the player structure stays unchanged
//  between levels.
//
// [STRIFE] Modifications for:
// * stonecold cheat, -workparm
// * default inventory/questflags
//
//==============================================================================
function P_SpawnPlayer(mthing: Pmapthing_t; uthing: Pextrathing_t): Pmobj_t;
var
  p: Pplayer_t;
  x: fixed_t;
  y: fixed_t;
  z: fixed_t;
  i: integer;
  plnum: integer;
  ss: Psubsector_t;
begin
  // not playing?
  if not playeringame[mthing._type - 1] then
  begin
    result := nil;
    exit;
  end;

  plnum := mthing._type - 1;
  p := @players[plnum];

  if p.playerstate = PST_REBORN then
    G_PlayerReborn(plnum);

  if uthing <> nil then
  begin
    x := uthing.x;
    y := uthing.y;
    if uthing.extraflags and UDMF_TF_HASZ <> 0 then
      z := uthing.z
    else
      z := ONFLOORZ;
  end
  else
  begin
    x := mthing.x * FRACUNIT;
    y := mthing.y * FRACUNIT;
    z := ONFLOORZ;
  end;

  // JVAL: 20191209 - 3d floors - Fixed Player spawned in 3d floor
  ss := R_PointInSubsector(x, y);
  if ss.sector.midsec >= 0 then
    if mthing.options and MTF_ONMIDSECTOR <> 0 then
      z := sectors[ss.sector.midsec].ceilingheight;

  result := P_SpawnMobj(x, y, z, Ord(MT_PLAYER), @mthing);

  // set color translations for player sprites
  if mthing._type > 1 then
    result.flags := result.flags or _SHLW(plnum, MF_TRANSSHIFT);

  if result.flags2_ex and MF2_EX_PRECISESPAWNANGLE <> 0 then
    result.angle := ANG1 * mthing.angle
  else
    result.angle := ANG45 * (mthing.angle div 45);
  result.player := p;
  result.health := p.health;
  result.customparams := nil; // JVAL: Should keep the old params!

  p.mo := result;
  p.playerstate := PST_LIVE;
  p.refire := 0;
  p._message := '';
  p.damagecount := 0;
  p.bonuscount := 0;
  p.extralight := 0;
  p.fixedcolormap := 0;
  p.viewheight := PVIEWHEIGHT;
  // JVAL: 20211117 - Reset extra player fields when spawning player
  p.lastbreath := 0;
  p.hardbreathtics := 0;
  p.angletargetticks := 0;
  p.laddertics := 0;
  p.slopetics := 0;
  p.teleporttics := 0;
  p.nextoof := 0;
  p.quakeintensity := 0;
  p.quaketics := 0;
  p.oldcrouch := 0;
  p.lastongroundtime := 0;
  p.lastautocrouchtime := 0;
  p.crouchheight := 0;
  p.plinetarget := nil;
  p.pcrosstic := leveltime;
  // JVAL: 20211224 - Clear player history
  P_ClearPlayerHistory(p);
  p.nextfire := 0;

  // JVAL: 20220123 - Gravity & Health from UDMF
  if uthing <> nil then
  begin
    if uthing.extraflags and UDMF_TF_HASGRAVITY <> 0 then
    begin
      if uthing.gravity < 0.0 then
        p.mo.gravity := -Round(uthing.gravity * FRACUNIT)
      else
        p.mo.gravity := Round(p.mo.gravity * uthing.gravity);
    end;
    if uthing.extraflags and UDMF_TF_HASHEALTH <> 0 then
    begin
      if uthing.health < 0.0 then
      begin
        p.health := -Round(uthing.health);
        p.mo.health := p.health;
      end
      else
      begin
        p.health := Round(p.health * uthing.health);
        p.mo.health := p.health;
      end
    end;
    p.mo.tid := uthing.id;
  end;

  // setup gun psprite
  P_SetupPsprites(p);

  // villsa [STRIFE]
  stonecold := false;

  // villsa [STRIFE] what a nasty hack...
  if gamemap = 10 then
    p.weaponowned[Ord(wp_sigil)] := true;

  // give all cards in death match mode
  // villsa [STRIFE] instead of just giving cards in deathmatch mode, also
  // set accuracy to 50 and give all quest flags
  if deathmatch <> 0 then
  begin
    p.accuracy := 50;
    p.questflags := QF_ALLQUESTS; // 0x7fffffff
    for i := 0 to Ord(NUMCARDS) - 1 do
      p.cards[i] := true;
  end;

  // villsa [STRIFE] set godmode?
  if workparm then
    p.cheats := p.cheats or CF_GODMODE;

  if plnum = consoleplayer then
  begin
    // wake up the status bar
    ST_Start;
    // wake up the heads up text
    HU_Start;

    p_justspawned := true;
  end;
end;

//==============================================================================
//
// P_SpawnMapThing
// The fields of the mapthing should
// already be in host byte order.
//
// [STRIFE] Modifications for:
// * No Lost Souls, item count
// * New mapthing_t flag bits
// * 8-player support
//
//==============================================================================
function P_SpawnMapThing(mthing: Pmapthing_t; uthing: Pextrathing_t): Pmobj_t;
var
  i: integer;
  bit: integer;
  x: fixed_t;
  y: fixed_t;
  z: fixed_t;
  ss: Psubsector_t; // JVAL: 3d floors
  msec: Psector_t;  // JVAL: 3d floors
  musinfoparam: integer;
begin
  // Count deathmatch start positions
  if mthing._type = 11 then
  begin
    if deathmatch_p < MAX_DEATHMATCH_STARTS then
    begin
      memcpy(@deathmatchstarts[deathmatch_p], mthing, SizeOf(mthing^));
      if uthing <> nil then
        memcpy(@udeathmatchstarts[deathmatch_p], uthing, SizeOf(uthing^))
      else
      begin
        ZeroMemory(@udeathmatchstarts[deathmatch_p], SizeOf(uthing^));
        if mthing.options and 1 <> 0 then
          udeathmatchstarts[deathmatch_p].extraflags :=
            udeathmatchstarts[deathmatch_p].extraflags or UDMF_TF_SKILL1 or UDMF_TF_SKILL2;
        if mthing.options and 2 <> 0 then
          udeathmatchstarts[deathmatch_p].extraflags :=
            udeathmatchstarts[deathmatch_p].extraflags or UDMF_TF_SKILL3;
        if mthing.options and 4 <> 0 then
          udeathmatchstarts[deathmatch_p].extraflags :=
            udeathmatchstarts[deathmatch_p].extraflags or UDMF_TF_SKILL4 or UDMF_TF_SKILL5;
        udeathmatchstarts[deathmatch_p].x := mthing.x * FRACUNIT;
        udeathmatchstarts[deathmatch_p].y := mthing.y * FRACUNIT;
      end;
      inc(deathmatch_p);
    end;
    result := nil;
    exit;
  end;

  if mthing._type <= 0 then
  begin
    // Thing type 0 is actually "player -1 start".
    // For some reason, Vanilla Doom accepts/ignores this.
    result := nil;
    exit;
  end;

  if mthing._type = PO_ANCHOR_TYPE then
  begin // Polyobj Anchor Pt.
    result := nil;
    exit;
  end
  else if (mthing._type = PO_SPAWN_TYPE) or
          (mthing._type = PO_SPAWNCRUSH_TYPE) then
  begin // Polyobj Anchor Pt.
    inc(po_NumPolyobjs);
    result := nil;
    exit;
  end;

  // check for players specially
  // haleyjd 20120209: [STRIFE] 8 player starts
  if mthing._type <= 8 then
  begin
    // save spots for respawning in network games
    playerstarts[mthing._type - 1] := mthing^;
    if uthing <> nil then
      uplayerstarts[mthing._type - 1] := uthing^
    else
    begin
      ZeroMemory(@uplayerstarts[mthing._type - 1], SizeOf(uplayerstarts[mthing._type - 1]));
      uplayerstarts[mthing._type - 1].x := mthing.x * FRACUNIT;
      uplayerstarts[mthing._type - 1].y := mthing.y * FRACUNIT;
    end;
    if deathmatch = 0 then
      result := P_SpawnPlayer(mthing, uthing)
    else
      result := nil;
    exit;
  end;

  if uthing <> nil then
  begin
    if not netgame then
      if uthing.extraflags and UDMF_TF_SINGLE = 0 then
      begin
        result := nil;
        exit;
      end;

    if netgame and (deathmatch <> 0) then
      if uthing.extraflags and UDMF_TF_DM = 0 then
      begin
        result := nil;
        exit;
      end;

    if netgame and (deathmatch = 0) then
      if uthing.extraflags and UDMF_TF_COOP = 0 then
      begin
        result := nil;
        exit;
      end;
  end
  else
  begin
    // check for apropriate skill level
    if not netgame and (mthing.options and 16 <> 0) then
    begin
      result := nil;
      exit;
    end;
  end;

  if uthing <> nil then
  begin
    bit := _SHL(1, Ord(gameskill));
    if bit and uthing.extraflags = 0 then
    begin
      result := nil;
      exit;
    end;
  end
  else
  begin
    if gameskill = sk_baby then
      bit := 1
    else if gameskill = sk_nightmare then
      bit := 4
    else
      bit := _SHL(1, Ord(gameskill) - 1);

    if mthing.options and bit = 0 then
    begin
      result := nil;
      exit;
    end;
  end;

  musinfoparam := -1;
  if (mthing._type >= MUSICCHANGER_LO) and (mthing._type <= MUSICCHANGER_HI) then
  begin
    musinfoparam := mthing._type - MUSICCHANGER_LO;
    mthing._type := MUSICCHANGER;
  end;

  // find which type to spawn
  i := Info_GetMobjNumForDoomNum(mthing._type);
  if i < 0 then
  begin
    I_Warning('P_SpawnMapThing(): Unknown type %d at (%d, %d)'#13#10,
      [mthing._type, mthing.x, mthing.y]);
    i := Info_GetMobjNumForName('UNKNOWN');
    if i < 0 then
    begin
      result := nil;
      exit;
    end;
  end;

  // don't spawn keycards and players in deathmatch
  if (deathmatch <> 0) and (mobjinfo[i].flags and MF_NOTDMATCH <> 0) then
  begin
    result := nil;
    exit;
  end;

  // don't spawn any monsters if -nomonsters
  if nomonsters and (mobjinfo[i].flags and MF_COUNTKILL <> 0) then
  begin
    result := nil;
    exit;
  end;

  // spawn it
  if uthing <> nil then
  begin
    x := uthing.x;
    y := uthing.y;
  end
  else
  begin
    x := mthing.x * FRACUNIT;
    y := mthing.y * FRACUNIT;
  end;

  // JVAL
  // Random map enemies
  if spawnrandommonsters and Info_IsMonster(i) then
    i := Info_SelectRandomMonster(i);

  if mobjinfo[i].flags and MF_SPAWNCEILING <> 0 then
    z := ONCEILINGZ
  else if mobjinfo[i].flags_ex and MF_EX_SPAWNFLOAT <> 0 then
    z := ONFLOATZ
  else
    z := ONFLOORZ;

  if uthing <> nil then
    if uthing.extraflags and UDMF_TF_HASZ <> 0 then
      z := uthing.z;

// JVAL: 3d floors
  ss := R_PointInSubsector(x, y);
  if ss.sector.midsec >= 0 then
  begin
    msec := @sectors[ss.sector.midsec];
    if mthing.options and MTF_ONMIDSECTOR <> 0 then
    begin
      if z = ONFLOATZ then
        z := (msec.ceilingheight + P_CeilingHeight(ss.sector, x, y)) div 2
      else if z = ONFLOORZ then
        z := msec.ceilingheight;
    end
    else
    begin
      if z = ONFLOATZ then
        z := (P_FloorHeight(ss.sector, x, y) + msec.floorheight) div 2
      else if z = ONCEILINGZ then
        z := msec.floorheight;
    end;
  end;

  result := P_SpawnMobj(x, y, z, i, mthing);
  result.spawnpoint := mthing^;

  if musinfoparam >= 0 then
    P_SetMobjCustomParam(result, S_MUSINFO_PARAM, musinfoparam);

  if result.tics > 0 then
    result.tics := 1 + (P_Random mod result.tics);
  if result.flags and MF_COUNTKILL <> 0 then
    inc(totalkills);

  if result.flags2_ex and MF2_EX_PRECISESPAWNANGLE <> 0 then
    result.angle := ANG1 * mthing.angle
  else
    result.angle := ANG45 * (mthing.angle div 45);

  if mthing.options and MTF_DONOTTRIGGERSCRIPTS <> 0 then
    result.flags2_ex := result.flags2_ex or MF2_EX_DONTRUNSCRIPTS;

  if mthing.options and MTF_AMBUSH <> 0 then
    result.flags := result.flags or MF_AMBUSH;
  if mthing.options and MTF_STAND <> 0 then       // [STRIFE] Standing mode, for NPCs
    result.flags := result.flags or MF_STAND;
  if mthing.options and MTF_ALLY <> 0 then        // [STRIFE] Allies
    result.flags := result.flags or MF_ALLY;
  if mthing.options and MTF_FRIEND <> 0 then
    result.flags2_ex := result.flags2_ex or MF2_EX_FRIEND;
  if mthing.options and MTF_TRANSLUCENT <> 0 then // [STRIFE] Translucent object
    result.flags := result.flags or MF_SHADOW;
  if mthing.options and MTF_MVIS <> 0 then        // [STRIFE] Alt. Translucency
    result.flags := result.flags or MF_MVIS;

  // JVAL: 20220123 - Gravity & Health from UDMF
  if uthing <> nil then
  begin
    if uthing.extraflags and UDMF_TF_HASGRAVITY <> 0 then
    begin
      if uthing.gravity < 0.0 then
        result.gravity := -Round(uthing.gravity * FRACUNIT)
      else
        result.gravity := Round(result.gravity * uthing.gravity);
    end;
    if uthing.extraflags and UDMF_TF_HASHEALTH <> 0 then
    begin
      if uthing.health < 0.0 then
        result.health := -Round(uthing.health)
      else
        result.health := Round(result.health * uthing.health);
    end;
    result.special := uthing.special;
    result.args[0] := uthing.arg1;
    result.args[1] := uthing.arg2;
    result.args[2] := uthing.arg3;
    result.args[3] := uthing.arg4;
    result.args[4] := uthing.arg5;
    result.tid := uthing.id;
  end;
end;

//==============================================================================
//
// GAME SPAWN FUNCTIONS
//
// P_SpawnPuff
//
// [STRIFE] Modifications for:
// * No spawn tics randomization
// * Player melee behavior
//
//==============================================================================
procedure P_SpawnPuff(x, y, z: fixed_t);
var
  th: Pmobj_t;
begin
  z := z + _SHL(P_Random - P_Random, 10);

  th := P_SpawnMobj(x, y, z, Ord(MT_STRIFEPUFF)); // [STRIFE]: new type

  // don't make punches spark on the wall
  // [STRIFE] Use a separate melee attack range for the player
  if attackrange = PLAYERMELEERANGE then
    P_SetMobjState(th, S_POW2_00);
end;

//==============================================================================
//
// P_SpawnSparkPuff
//
// villsa [STRIFE] new function
//
//==============================================================================
function P_SpawnSparkPuff(x, y, z: fixed_t): Pmobj_t;
begin
  result := P_SpawnMobj(x, y, z + _SHL(P_Random - P_Random, 10), Ord(MT_SPARKPUFF));
end;

//==============================================================================
//
// P_SpawnBlood
//
// [STRIFE] Modifications for:
// * No spawn tics randomization
// * Different damage ranges for state setting
//
//==============================================================================
procedure P_SpawnBlood(x, y, z: fixed_t; damage: integer; const originator: Pmobj_t);
var
  th: Pmobj_t;
begin
  z := z + _SHL(P_Random - P_Random, 10);
  th := P_SpawnMobj(x, y, z, Ord(MT_BLOOD_DEATH));

  if originator.bloodcolor <> 0 then
    R_SetMobjBloodTranslation(th, originator.bloodcolor);

  th.momz := FRACUNIT * 2;

  if th.flags3_ex and MF3_EX_BLOODIGNOREDAMAGE = 0 then
  begin
    // villsa [STRIFE] different checks for damage range
    if (damage >= 10) and (damage <= 13) then
      P_SetMobjState(th, S_BLOD_00)
    else if (damage >= 7) and (damage < 10) then
      P_SetMobjState(th, S_BLOD_01)
    else if damage < 7 then
      P_SetMobjState(th, S_BLOD_02);
  end;
end;

//==============================================================================
//
// P_SpawnGreenBlood
//
//==============================================================================
procedure P_SpawnGreenBlood(x, y, z: fixed_t; damage: integer);
var
  th: Pmobj_t;
begin
  z := z + _SHL(N_Random - N_Random, 10);

  th := P_SpawnMobj(x, y, z, Ord(MT_GREENBLOOD));
  th.momz := FRACUNIT * 2;
  th.tics := th.tics - (N_Random and 3);

  if th.tics < 1 then
    th.tics := 1;

  if th.flags3_ex and MF3_EX_BLOODIGNOREDAMAGE = 0 then
  begin
    if (damage <= 12) and (damage >= 9) then
      P_SetMobjState(th, S_GREENBLOOD2)
    else if damage < 9 then
      P_SetMobjState(th, S_GREENBLOOD3);
  end;
end;

//==============================================================================
//
// P_SpawnBlueBlood
//
//==============================================================================
procedure P_SpawnBlueBlood(x, y, z: fixed_t; damage: integer);
var
  th: Pmobj_t;
begin
  z := z + _SHL(N_Random - N_Random, 10);

  th := P_SpawnMobj(x, y, z, Ord(MT_BLUEBLOOD));
  th.momz := FRACUNIT * 2;
  th.tics := th.tics - (N_Random and 3);

  if th.tics < 1 then
    th.tics := 1;

  if th.flags3_ex and MF3_EX_BLOODIGNOREDAMAGE = 0 then
  begin
    if (damage <= 12) and (damage >= 9) then
      P_SetMobjState(th, S_BLUEBLOOD2)
    else if damage < 9 then
      P_SetMobjState(th, S_BLUEBLOOD3);
  end;
end;

//==============================================================================
//
// P_CheckMissileSpawn
// Moves the missile forward a bit
//  and possibly explodes it right there.
//
// [STRIFE] Modifications for:
// * No spawn tics randomization
//
//==============================================================================
function P_CheckMissileSpawn(th: Pmobj_t): boolean;
begin
  // move a little forward so an angle can
  // be computed if it immediately explodes
  th.x := th.x + th.momx div 2;
  th.y := th.y + th.momy div 2;
  th.z := th.z + th.momz div 2;

  if not P_TryMove(th, th.x, th.y) then
  begin
    P_ExplodeMissile(th);
    result := false;
  end
  else
    result := true;
end;

//==============================================================================
//
// P_SpawnMissile
//
// [STRIFE] Added MVIS inaccuracy
//
//==============================================================================
function P_SpawnMissile(source: Pmobj_t; dest: Pmobj_t; _type: integer): Pmobj_t;
var
  th: Pmobj_t;
  an: angle_t;
  dist: integer;
begin
  // JVAL: Prevent savegame bug
  if dest = nil then
  begin
    result := nil;
    exit;
  end;

  if source.info.missileheight = 0 then
    th := P_SpawnMobj(source.x, source.y, source.z + 4 * 8 * FRACUNIT, _type)
  else if source.info.missileheight < FRACUNIT div 2 then
    th := P_SpawnMobj(source.x, source.y, source.z + source.info.missileheight * FRACUNIT, _type)
  else
    th := P_SpawnMobj(source.x, source.y, source.z + source.info.missileheight, _type);

  A_SeeSound1(th);

  th.target := source;  // where it came from
  an := R_PointToAngle2(source.x, source.y, dest.x, dest.y);

  // fuzzy player
  if source.flags2_ex and MF2_EX_SEEINVISIBLE = 0 then
  begin
    if dest.flags and MF_SHADOW <> 0 then
      an := an + _SHLW(P_Random - P_Random, 21);
    if dest.flags and MF_MVIS <> 0 then
      an := an + _SHLW(P_Random - P_Random, 22);
  end;

  th.angle := an;
  {$IFDEF FPC}
  an := _SHRW(an, ANGLETOFINESHIFT);
  {$ELSE}
  an := an shr ANGLETOFINESHIFT;
  {$ENDIF}
  th.momx := FixedMul(th.info.speed, finecosine[an]);
  th.momy := FixedMul(th.info.speed, finesine[an]);

  dist := P_AproxDistance(dest.x - source.x, dest.y - source.y);
  // JVAL: If forgot to set custom missile speed we use default (12 * FRACUNIT)
  if th.info.speed = 0 then
    dist := dist div (12 * FRACUNIT)
  else
    dist := dist div th.info.speed;

  if dist < 1 then
    dist := 1;

  th.momz := (dest.z - source.z) div dist;
  P_CheckMissileSpawn(th);

  result := th;
end;

//==============================================================================
//
// P_SpawnMissileXYZ
//
//==============================================================================
function P_SpawnMissileXYZ(x, y, z: fixed_t; source: Pmobj_t; dest: Pmobj_t; _type: integer): Pmobj_t;
var
  flags_ex: integer;
  th: Pmobj_t;
  velocity: vec3_t;
  speed: float;
  an: angle_t;
begin
  // JVAL: Prevent savegame bug
  if dest = nil then
  begin
    result := nil;
    exit;
  end;

  P_SaveRandom;

  flags_ex := mobjinfo[Ord(_type)].flags_ex;

  if flags_ex and MF_EX_FLOORHUGGER <> 0 then
  begin
    if G_PlayingEngineVersion >= VERSION122 then
      z := source.floorz
    else
      z := ONFLOORZ;
  end
  else if flags_ex and MF_EX_CEILINGHUGGER <> 0 then
  begin
    if G_PlayingEngineVersion >= VERSION122 then
      z := source.ceilingz
    else
      z := ONCEILINGZ;
  end
  else if z <> ONFLOORZ then
    z := z - source.floorz;

  th := P_SpawnMobj(x, y, z, _type);

  A_SeeSound1(th);

  th.target := source;  // record missile's originator

  speed := th.info.speed;

  velocity[0] := dest.x - source.x;
  velocity[1] := dest.y - source.y;
  // Floor and ceiling huggers should never have a vertical component to their velocity
  if flags_ex and (MF_EX_FLOORHUGGER or MF_EX_CEILINGHUGGER) <> 0 then
    velocity[2] := 0.0
  else
  begin
    velocity[2] := dest.z - source.z;
    if dest.height <= z - source.z then
      velocity[2] := velocity[2] + dest.height - z + source.z;
  end;

  VectorNormalize(@velocity);
  th.momx := round(velocity[0] * speed);
  th.momy := round(velocity[1] * speed);
  th.momz := round(velocity[2] * speed);

  // fuzzy player
  if dest.flags and MF_SHADOW <> 0 then
    if source.flags2_ex and MF2_EX_SEEINVISIBLE = 0 then
    begin
      an := _SHLW(P_Random - P_Random, 20);
      an := an shr ANGLETOFINESHIFT;
      th.momx := th.momx + FixedMul(th.info.speed, finecosine[an]);
      th.momy := th.momy + FixedMul(th.info.speed, finesine[an]);
    end;

  th.angle := R_PointToAngle2(0, 0, th.momx, th.momy);

  if P_CheckMissileSpawn(th) then
    result := th
  else
    result := nil;

  P_RestoreRandom;
end;

//==============================================================================
//
// P_SpawnMissileAngleZSpeed
//
//==============================================================================
function P_SpawnMissileAngleZSpeed(source: Pmobj_t; z: fixed_t; _type: integer; angle: angle_t;
  momz: fixed_t; speed: fixed_t; owner: Pmobj_t): Pmobj_t;
var
  mo: Pmobj_t;
  flags_ex: integer;
begin
  P_SaveRandom;

  flags_ex := mobjinfo[Ord(_type)].flags_ex;

  if flags_ex and MF_EX_FLOORHUGGER <> 0 then
  begin
    if G_PlayingEngineVersion >= VERSION122 then
      z := source.floorz
    else
      z := ONFLOORZ;
  end
  else if flags_ex and MF_EX_CEILINGHUGGER <> 0 then
  begin
    if G_PlayingEngineVersion >= VERSION122 then
      z := source.ceilingz
    else
      z := ONCEILINGZ;
  end
  else if z <> ONFLOORZ then
    z := z - source.floorz;

  mo := P_SpawnMobj(source.x, source.y, z, _type);

  A_SeeSound1(mo);

  if owner <> nil then
    mo.target := owner
  else
    mo.target := source;  // Originator
  mo.angle := angle;
  angle := angle shr ANGLETOFINESHIFT;
  mo.momx := FixedMul(speed, finecosine[angle]);
  mo.momy := FixedMul(speed, finesine[angle]);
  mo.momz := momz;
  if P_CheckMissileSpawn(mo) then
    result := mo
  else
    result := nil;

  P_RestoreRandom;
end;

//==============================================================================
//
// P_SpawnMissileAngleZ
//
//==============================================================================
function P_SpawnMissileAngleZ(source: Pmobj_t; z: fixed_t; _type: integer; angle: angle_t;
  momz: fixed_t; speed: fixed_t): Pmobj_t;
begin
  result := P_SpawnMissileAngleZSpeed(source, z, _type, angle, momz, mobjinfo[Ord(_type)].speed, nil);
end;

//==============================================================================
//
// P_SpawnFacingMissile
//
// villsa [STRIFE] new function
// Spawn a missile based on source's angle
//
//==============================================================================
function P_SpawnFacingMissile(source: Pmobj_t; target: Pmobj_t; _type: integer): Pmobj_t;
var
  an: angle_t;
  dist: fixed_t;
begin
  result := P_SpawnMobj(source.x, source.y, source.z + (32 * FRACUNIT), _type);

  A_SeeSound1(result);

  result.target := source;    // where it came from
  result.angle := source.angle; // haleyjd 09/06/10: fix0red
  an := result.angle;

  // fuzzy player
  if source.flags2_ex and MF2_EX_SEEINVISIBLE = 0 then
  begin
    if target.flags and MF_SHADOW <> 0 then
    begin
      an := an + _SHLW(P_Random - P_Random, 21);
    end
    // villsa [STRIFE] check for heavily transparent things
    else if target.flags and MF_MVIS <> 0 then
    begin
      an := an + _SHLW(P_Random - P_Random, 22);
    end;
  end;

  an := an shr ANGLETOFINESHIFT;

  result.momx := FixedMul(result.info.speed, finecosine[an]);
  result.momy := FixedMul(result.info.speed, finesine[an]);

  dist := P_AproxDistance(target.x - source.x, target.y - source.y);
  if result.info.speed = 0 then
    dist := 1
  else
  begin
    dist := dist div result.info.speed;
    if dist < 1 then
      dist := 1;
  end;

  result.momz := (target.z - source.z) div dist;
  P_CheckMissileSpawn(result);
end;

//==============================================================================
//
// P_SpawnPlayerMissile
// Tries to aim at a nearby monster
//
// Tries to aim at a nearby monster
// villsa [STRIFE] now returns a mobj
// * Also modified to allow up/down look, and to account for foot-clipping
//   by liquid terrain.
//
//==============================================================================
function P_SpawnPlayerMissile(source: Pmobj_t; _type: integer): Pmobj_t;
var
  th: Pmobj_t;
  an: angle_t;
  x: fixed_t;
  y: fixed_t;
  z: fixed_t;
  slope: fixed_t;
begin
  // see which target is to be aimed at
  an := source.angle;
  slope := P_AimLineAttack(source, an, 16 * 64 * FRACUNIT);

  if linetarget = nil then
  begin
    an := an + $4000000;
    slope := P_AimLineAttack(source, an, 16 * 64 * FRACUNIT);

    if linetarget = nil then
    begin
      an := an - $8000000;
      slope := P_AimLineAttack(source, an, 16 * 64 * FRACUNIT);

      if zaxisshift and (linetarget = nil) then
      begin
        an := source.angle;
        slope := (Pplayer_t(source.player).lookdir * FRACUNIT) div 173;
      end;

    end;
  end;

  // villsa [STRIFE]
  if linetarget <> nil then
    source.target := linetarget;

  x := source.x;
  y := source.y;
  // Also z axis shift calculation
  if zaxisshift then
    z := source.z + 4 * 8 * FRACUNIT + (Pplayer_t(source.player).lookdir * FRACUNIT) div 173
  else
    z := source.z + 4 * 8 * FRACUNIT;

  if source.flags and MF_FEETCLIPPED <> 0 then
    z := z - source.floorclip;

  th := P_SpawnMobj(x, y, z, _type);

  A_SeeSound1(th);

  th.target := source;
  th.angle := an;
  th.momx := FixedMul(th.info.speed, finecosine[{$IFDEF FPC}_SHRW(an, ANGLETOFINESHIFT){$ELSE}an shr ANGLETOFINESHIFT{$ENDIF}]);
  th.momy := FixedMul(th.info.speed, finesine[{$IFDEF FPC}_SHRW(an, ANGLETOFINESHIFT){$ELSE}an shr ANGLETOFINESHIFT{$ENDIF}]);
  th.momz := FixedMul(th.info.speed, slope);

  if P_CheckMissileSpawn(th) then
    result := th
  else
    result := nil;
end;

//==============================================================================
//
// P_SpawnMortar
//
// villsa [STRIFE] new function
// Spawn a high-arcing ballistic projectile
//
//==============================================================================
function P_SpawnMortar(source: Pmobj_t; _type: integer): Pmobj_t;
var
  an: angle_t;
  slope: fixed_t;
begin
  an := source.angle;

  result := P_SpawnMobj(source.x, source.y, source.z, _type);
  result.target := source;
  result.angle := an;
  an := an div ANGLETOFINEUNIT;

  // haleyjd 20110203: corrected order of function calls
  result.momx := FixedMul(result.info.speed, finecosine[an]);
  result.momy := FixedMul(result.info.speed, finesine[an]);

  P_CheckMissileSpawn(result);

  slope := P_AimLineAttack(source, source.angle, 1024 * FRACUNIT);
  result.momz := FixedMul(result.info.speed, slope);
end;

//==============================================================================
//
// FUNC P_SeekerMissile
//
// The missile's tracer field must be the target.  Returns true if
// target was tracked, false if not.
//
//==============================================================================
function P_SeekerMissile(actor: Pmobj_t; thresh, turnMax: angle_t): boolean;
var
  dir: integer;
  dist: integer;
  delta: angle_t;
  angle: angle_t;
  target: Pmobj_t;
  speed: fixed_t;
begin
  target := actor.tracer;
  speed := P_AproxDistance(actor.momx, actor.momy);
  if (target = nil) or (speed = 0) then
  begin
    result := false;
    exit;
  end;

  if target.flags and MF_SHOOTABLE = 0 then
  begin // Target died
    actor.tracer := nil;
    result := false;
    exit;
  end;

  dir := P_FaceMobj(actor, target, delta);
  if delta > thresh then
  begin
    delta := delta shr 1;
    if delta > turnMax then
      delta := turnMax;
  end;

  if dir <> 0 then
  begin // Turn clockwise
    actor.angle := actor.angle + delta;
  end
  else
  begin // Turn counter clockwise
    actor.angle := actor.angle - delta;
  end;

  angle := actor.angle shr ANGLETOFINESHIFT;
  actor.momx := FixedMul(speed, finecosine[angle]);
  actor.momy := FixedMul(speed, finesine[angle]);
  if (actor.z + actor.height < target.z) or
     (target.z + target.height < actor.z) then
  begin // Need to seek vertically
    dist := P_AproxDistance(target.x - actor.x, target.y - actor.y);
    dist := dist div speed;
    if dist < 1 then
      dist := 1;
    actor.momz := ((target.z + target.height div 2) - (actor.z + actor.height div 2)) div dist;
  end;
  result := true;
end;

//==============================================================================
//
// FUNC P_GetThingFloorType
//
// JVAL: 9 December 2007, Added terrain types
//
//==============================================================================
function P_GetThingFloorType(thing: Pmobj_t): integer;
begin
  result := flats[Psubsector_t(thing.subsector).sector.floorpic].terraintype;
end;

//==============================================================================
//
// FUNC P_HitFloor
//
//==============================================================================
function P_HitFloor(thing: Pmobj_t): integer;
var
  mo: Pmobj_t;
  sec: Psector_t;
  z: fixed_t; // JVAL: 3d Floors
  ss: Psubsector_t;
begin
  result := FLOOR_SOLID;

  // don't splash if has MF2_EX_NOHITFLOOR flag
  if thing.flags2_ex and MF2_EX_NOHITFLOOR <> 0 then
    exit;

  // Exit if playing old engine demo or in compatibility mode
  if G_NeedsCompatibilityMode then
    exit;

  ss := thing.subsector;

  if ss.flags and SSF_BRIDGE <> 0 then
    if G_PlayingEngineVersion >= VERSION204 then
      exit;

  sec := ss.sector;
  // don't splash if landing on the edge above water/lava/etc....
  if thing.floorz <> sec.floorheight then
    exit;

  if sec.heightsec <> -1 then
    exit;

  // JVAL: 3d Floors
  if G_PlayingEngineVersion >= VERSION122 then
    z := thing.floorz
  else
    z := ONFLOORZ;

  case P_GetThingFloorType(thing) of
    FLOOR_WATER:
      begin
      // JVAL
      // If we record a demo or playing demo (version > 114)
      // then we force splashes
        if allowterrainsplashes or demorecording or demoplayback then
        begin
          P_SpawnMobj(thing.x, thing.y, z, Ord(MT_SPLASHBASE));
          mo := P_SpawnMobj(thing.x, thing.y, z, Ord(MT_SPLASH));
          mo.target := thing;
          mo.momx := (P_Random - P_Random) * 256;
          mo.momy := (P_Random - P_Random) * 256;
          mo.momz := 2 * FRACUNIT + (P_Random * 256);
          S_StartSound(mo, Ord(sfx_gloop));
        end;
        result := FLOOR_WATER;
        exit;
      end;
    FLOOR_LAVA:
      begin
        if allowterrainsplashes or demorecording or demoplayback then
        begin
          P_SpawnMobj(thing.x, thing.y, z, Ord(MT_LAVASPLASH));
          mo := P_SpawnMobj(thing.x, thing.y, z, Ord(MT_LAVASMOKE));
          mo.momz := FRACUNIT + (P_Random * 128);
          S_StartSound(mo, Ord(sfx_burn));
        end;
        result := FLOOR_LAVA;
        exit;
      end;
    FLOOR_SLUDGE:
      begin
        if allowterrainsplashes or demorecording or demoplayback then
        begin
          P_SpawnMobj(thing.x, thing.y, z, Ord(MT_SLUDGESPLASH));
          mo := P_SpawnMobj(thing.x, thing.y, z, Ord(MT_SLUDGECHUNK));
          mo.target := thing;
          mo.momx := (P_Random - P_Random) * 256;
          mo.momy := (P_Random - P_Random) * 256;
          mo.momz := FRACUNIT + (P_Random * 256);
          S_StartSound(mo, Ord(sfx_sgloop));
        end;
        result := FLOOR_SLUDGE;
        exit;
      end;
    FLOOR_NUKAGE:
      begin
        if allowterrainsplashes or demorecording or demoplayback then
        begin
          P_SpawnMobj(thing.x, thing.y, ONFLOORZ, Ord(MT_NUKAGESPLASH));
          mo := P_SpawnMobj(thing.x, thing.y, ONFLOORZ, Ord(MT_NUKAGECHUNK));
          mo.target := thing;
          mo.momx := (P_Random - P_Random) * 256;
          mo.momy := (P_Random - P_Random) * 256;
          mo.momz := FRACUNIT + (P_Random * 256);
          S_StartSound(mo, Ord(sfx_sgloop));
        end;
        result := FLOOR_NUKAGE;
        exit;
      end;
  end;
end;

//==============================================================================
//
// CmdSpawnMobj
//
//==============================================================================
procedure CmdSpawnMobj(const parm1, parm2: string);
var
  sc: TScriptEngine;
  x, y, z: fixed_t;
  mobjno, dn: integer;
  angle: angle_t;
  parm, tmp: string;
  mo: Pmobj_t;
begin
  if (gamestate <> GS_LEVEL) or demoplayback or demorecording or netgame then
  begin
    printf('You can''t specify the command at this time.'#13#10);
    exit;
  end;

  parm := strtrim(parm1 + ' ' + parm2);
  if parm = '' then
  begin
    printf('Usage:'#13#10' spawnmobj [x y z angle doomednum/doomname]'#13#10);
    exit;
  end;

  sc := TScriptEngine.Create(parm);
  sc.MustGetInteger;
  x := sc._Integer * FRACUNIT;
  sc.MustGetInteger;
  y := sc._Integer * FRACUNIT;
  sc.MustGetString;
  tmp := strupper(sc._String);
  if (tmp = 'ONFLOORZ') or (tmp = 'FLOORZ') then
    z := ONFLOORZ
  else if (tmp = 'ONFLOATZ') or (tmp = 'FLOATZ') then
    z := ONFLOATZ
  else if (tmp = 'ONCEILINGZ') or (tmp = 'CEILINGZ') then
    z := ONCEILINGZ
  else
    z := atoi(tmp, ONFLOORZ) * FRACUNIT;

  sc.MustGetInteger;
  angle := sc._Integer * ANG1;

  tmp := '';
  while sc.GetString do
    tmp := tmp + sc._String + ' ';
  trimprocU(tmp);

  dn := atoi(tmp, 0);
  if dn >= 1 then
    mobjno := Info_GetMobjNumForDoomNum(dn)
  else
    mobjno := Info_GetMobjNumForName(tmp);
  if (mobjno > 0) and (mobjno < nummobjtypes) then
  begin
    mo := P_SpawnMobj(x, y, z, mobjno);
    if mo <> nil then
    begin
      mo.angle := angle;
      printf('spawnmobj: mobj %s spawned, key=%d'#13#10, [tmp, mo.key]);
    end
    else
      printf('spawnmobj: mobj %s can not be spawned'#13#10, [tmp]);
  end
  else
    printf('Unknown mobj %s'#13#10, [tmp]);
  sc.Free;
end;

//==============================================================================
//
// CmdSummon
//
//==============================================================================
procedure CmdSummon(const parm1, parm2: string);
var
  x, y, z: fixed_t;
  mobjno, dn: integer;
  an, angle: angle_t;
  parm: string;
  mo: Pmobj_t;
  dist: fixed_t;
begin
  if (gamestate <> GS_LEVEL) or demoplayback or demorecording or netgame then
  begin
    printf('You can''t specify the command at this time.'#13#10);
    exit;
  end;

  parm := strtrim(parm1);
  if parm = '' then
  begin
    printf('Usage:'#13#10' summon doomednum/doomname'#13#10);
    exit;
  end;

  dn := atoi(parm, 0);
  if dn >= 1 then
    mobjno := Info_GetMobjNumForDoomNum(dn)
  else
    mobjno := Info_GetMobjNumForName(parm);

  if (mobjno <= 0) or (mobjno >= nummobjtypes) then
  begin
    printf('Unknown mobj %s'#13#10, [parm]);
    exit;
  end;

  if players[consoleplayer].mo = nil then
    exit;

  angle := players[consoleplayer].mo.angle;
  an := angle shr ANGLETOFINESHIFT;
  dist := mobjinfo[Ord(MT_PLAYER)].radius + mobjinfo[mobjno].radius + 32 * FRACUNIT;
  x := players[consoleplayer].mo.x + FixedMul(dist, finecosine[an]);
  y := players[consoleplayer].mo.y + FixedMul(dist, finesine[an]);
  if mobjinfo[mobjno].flags and MF_SPAWNCEILING <> 0 then
    z := ONCEILINGZ
  else if mobjinfo[mobjno].flags_ex and MF_EX_SPAWNFLOAT <> 0 then
    z := ONFLOATZ
  else
    z := ONFLOORZ;

  mo := P_SpawnMobj(x, y, z, mobjno);
  if mo <> nil then
  begin
    mo.angle := angle;
    printf('summon: mobj %s spawned, key=%d'#13#10, [parm, mo.key]);
  end
  else
    printf('summon: mobj %s can not be spawned'#13#10, [parm]);
end;

//==============================================================================
//
// MObj_Init
//
//==============================================================================
procedure MObj_Init;
begin
  mobjlist := TMobjList.Create;
  C_AddCmd('spawnmobj, p_spawnmobj', @CmdSpawnMobj);
  C_AddCmd('summon', @CmdSummon);
end;

//==============================================================================
//
// MObj_ShutDown
//
//==============================================================================
procedure MObj_ShutDown;
begin
  mobjlist.Free;
end;

//==============================================================================
//
// FUNC P_FindMobjFromKey
//
//==============================================================================
function P_FindMobjFromKey(const key: LongWord): Pmobj_t;
var
  currentthinker: Pthinker_t;
begin
  currentthinker := thinkercap.next;
  while Pointer(currentthinker) <> Pointer(@thinkercap) do
  begin
    if (@currentthinker._function.acp1 = @P_MobjThinker) and
       (Pmobj_t(currentthinker).key = key) then
    begin
      result := Pmobj_t(currentthinker);
      exit;
    end;
    currentthinker := currentthinker.next;
  end;

  result := nil;
end;

end.

