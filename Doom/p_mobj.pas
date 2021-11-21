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
//  Map Objects, MObj, definition and handling.
//  Moving object handling. Spawn functions.
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
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
  m_fixed;

function P_SetMobjState(mobj: Pmobj_t; state: statenum_t): boolean;

procedure P_ExplodeMissile(mo: Pmobj_t);

procedure P_MobjThinker(mobj: Pmobj_t);

function P_SpawnMobj(x, y, z: fixed_t; _type: integer; const mthing: Pmapthing_t = nil): Pmobj_t;

procedure P_RemoveMobj(mobj: Pmobj_t);

function P_SpawnPlayer(mthing: Pmapthing_t): Pmobj_t;

function P_SpawnMapThing(mthing: Pmapthing_t): Pmobj_t;

procedure P_SpawnPuff(x, y, z: fixed_t);

function P_SpawnMissile(source: Pmobj_t; dest: Pmobj_t; _type: integer): Pmobj_t;

function P_SpawnMissileXYZ(x, y, z: fixed_t; source: Pmobj_t; dest: Pmobj_t; _type: integer): Pmobj_t;

function P_SpawnMissileAngleZ(source: Pmobj_t; z: fixed_t; _type: integer; angle: angle_t;
  momz: fixed_t; speed: fixed_t): Pmobj_t;

function P_SpawnMissileAngleZSpeed(source: Pmobj_t; z: fixed_t; _type: integer; angle: angle_t;
  momz: fixed_t; speed: fixed_t; owner: Pmobj_t): Pmobj_t;

procedure P_SpawnPlayerMissile(source: Pmobj_t; _type: integer);

procedure P_RespawnSpecials;

procedure P_SpawnBlood(x, y, z: fixed_t; damage: integer);

procedure P_SpawnGreenBlood(x, y, z: fixed_t; damage: integer);

procedure P_SpawnBlueBlood(x, y, z: fixed_t; damage: integer);

function P_SeekerMissile(actor: Pmobj_t; thresh, turnMax: angle_t): boolean;

function P_HitFloor(thing: Pmobj_t): integer;

function P_GetThingFloorType(thing: Pmobj_t): integer;

procedure MObj_Init;

procedure MObj_ShutDown;

var
  iquehead: integer; // Initialized at p_setup
  iquetail: integer; // Initialized at p_setup

function P_FindMobjFromKey(const key: LongWord): Pmobj_t;

var
  spawnmask: LongWord = $FFFFFFFF;

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
  p_gravity,
  p_local,
  p_map,
  p_maputl,
  p_mobjlist,
  p_tick,
  p_pspr,
  p_setup,
  p_spec,
  p_common,
  p_terrain,
  p_sounds,
  p_3dfloors, // JVAL: 3d floors
  p_slopes, // JVAL: Slopes
  p_params,
  p_ladder,
  p_musinfo,
  p_bouncing,
  r_defs,
  r_sky,
  r_main,
  r_data,
  st_stuff,
  hu_stuff,
  s_sound,
  sounds,
  info,
  info_rnd,
  info_common;

// From Chocolate-Doom
// Use a heuristic approach to detect infinite state cycles: Count the number
// of times the loop in P_SetMobjState() executes and exit with an error once
// an arbitrary very large limit is reached.
const
  MOBJ_CYCLE_LIMIT = 1000000;

//
// P_SetMobjState
// Returns true if the mobj is still present.
//
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
      I_Error('P_SetMobjState(): Infinite state cycle detected!');
  until mobj.tics <> 0;

  result := true;
end;

//
// P_ExplodeMissile
//
procedure P_ExplodeMissile(mo: Pmobj_t);
begin
  mo.momx := 0;
  mo.momy := 0;
  mo.momz := 0;

  P_SetMobjState(mo, statenum_t(mobjinfo[Ord(mo._type)].deathstate));

  mo.tics := mo.tics - (P_Random and 3);

  if mo.tics < 1 then
    mo.tics := 1;

  mo.flags := mo.flags and not MF_MISSILE;

  A_DeathSound(mo, mo);
end;

//
// P_XYMovement
//
const
  STOPSPEED = $1000;
  FRICTION = $e800;

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
  if (mo.momx = 0) and (mo.momy = 0) then
  begin
    if mo.flags and MF_SKULLFLY <> 0 then
    begin
      // the skull slammed into something
      mo.flags := mo.flags and not MF_SKULLFLY;
      mo.momx := 0;
      mo.momy := 0;
      mo.momz := 0;

      P_SetMobjState(mo, statenum_t(mo.info.spawnstate));
    end;
    exit;
  end;

  wasonfloorz := mo.z <= mo.floorz;
  oldsector := Psubsector_t(mo.subsector).sector;
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
    // JVAL 20180107
    // Please do not change, the div and the _SHR1, problem with demo compatibility..
      ptryx := mo.x + xmove div 2;
      ptryy := mo.y + ymove div 2;
      xmove := _SHR1(xmove);
      ymove := _SHR1(ymove);
    end
    else if (G_PlayingEngineVersion >= VERSION205) and ((xmove < -MAXMOVE div 2) or (ymove < -MAXMOVE div 2)) then
    begin
    // JVAL 20180107
    // Please do not change, the div and the _SHR1, problem with demo compatibility..
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
      else if (mo.flags3_ex and (MF3_EX_SLIDING or MF3_EX_SLIDEONWALLS) <> 0) or (mo.flags and MF_SLIDE <> 0) and (mo.flags and MF_MISSILE = 0) then
      begin
        P_SlideMove(mo); // try to slide along it
      end
      // JVAL: 20211121 - New bounch on walls mechanics
      else if (G_PlayingEngineVersion >= VERSION207) and (mo.flags3_ex and MF3_EX_WALLBOUNCE <> 0) and (tmbounceline <> nil) then
      begin
        P_WallBounceMobj(mo, tmbounceline);
        xmove := 0;
        ymove := 0;
      end
      // JVAL: 20200308 - Bounce on walls
      else if mo.flags3_ex and MF3_EX_WALLBOUNCE <> 0 then
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
  if (player <> nil) and (player.cheats and CF_NOMOMENTUM <> 0) then
  begin
    // debug option for no sliding at all
    mo.momx := 0;
    mo.momy := 0;
    exit;
  end;

  if mo.flags and (MF_MISSILE or MF_SKULLFLY) <> 0 then
    exit; // no friction for missiles ever

  if mo.flags3_ex and MF3_EX_BOUNCE <> 0 then
    exit; // no friction for bouncing objects

  if (player <> nil) and (player.laddertics > 0) then
  else
    if mo.z > mo.floorz then
    begin
      if G_PlayingEngineVersion <= VERSION203 then
        exit; // no friction when airborne
      if wasonfloorz and wasonslope and (oldsector = Psubsector_t(mo.subsector).sector) then
      begin
        if oldsector.flags and SF_SLIPSLOPEDESCENT <> 0 then
          exit; // Slip sector while descenting slope
        mo.z := mo.floorz;
      end
      else
        exit;
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
    if (player <> nil) and
       (LongWord((pDiff(player.mo.state, @states[0], SizeOf(states[0]))) - Ord(S_PLAY_RUN1)) < 4) then
      P_SetMobjState(player.mo, S_PLAY);

    mo.momx := 0;
    mo.momy := 0;
  end
  else
  begin
    mo.momx := FixedMul(mo.momx, FRICTION);
    mo.momy := FixedMul(mo.momy, FRICTION);
  end;
end;

//
// P_ZMovement
//
procedure P_ZMovement(mo: Pmobj_t);
var
  dist: fixed_t;
  delta: fixed_t;
  ceilz: fixed_t;
  grav: integer;
  momomz: fixed_t;
  correct_lost_soul_bounce: Boolean; // JVAL: From Chocolate DOOM
  ladderticks: integer;
  player: Pplayer_t;
begin
  ladderticks := 0;
  player := Pplayer_t(mo.player);
  if player <> nil then
  begin
    if player.laddertics > 0 then
    begin
      Dec(player.laddertics);
      ladderticks := player.laddertics;
    end;
    if player.slopetics > 0 then
      Dec(player.slopetics);

    // check for smooth step up
    if (mo.z < mo.floorz) and (ladderticks = 0) then
    begin
      player.viewheight := player.viewheight - (mo.floorz - mo.z);
      player.deltaviewheight :=
        _SHR((PVIEWHEIGHT - player.crouchheight - player.viewheight), 3); // JVAL: 20211101 - Crouch
    end;
  end;

  // adjust height
  if ladderticks > 0 then
  begin
    mo.z := mo.z + mo.momz;
    mo.momz := (mo.momz * 7) div 8;
    if mo.momz < FRACUNIT div 16 then
      mo.momz := 0;
  end
  else
    mo.z := mo.z + mo.momz;

  if (mo.flags and MF_FLOAT <> 0) and (mo.target <> nil) then
  begin
    // float down towards target if too close
    if (mo.flags and MF_SKULLFLY = 0) and
       (mo.flags and MF_INFLOAT = 0) then
    begin
      dist := P_AproxDistance(mo.x - mo.target.x, mo.y - mo.target.y);

      delta := (mo.target.z + _SHR1(mo.height)) - mo.z; // JVAL is it right ???

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

    if mo.flags3_ex and MF3_EX_FLOORBOUNCE <> 0 then
    begin
      // villsa [STRIFE] affect reactiontime
      // momz is also shifted by 1
      mo.momz := -mo.momz div 2;
      mo.reactiontime := mo.reactiontime div 2;

      // villsa [STRIFE] get terrain type
      if P_GetThingFloorType(mo) <> FLOOR_SOLID then
        mo.flags3_ex := mo.flags3_ex and not MF3_EX_FLOORBOUNCE;
    end;

    // Note (id):
    //  somebody left this after the setting momz to 0,
    //  kinda useless there.

    //
    // cph - This was the a bug in the linuxdoom-1.10 source which
    //  caused it not to sync Doom 2 v1.9 demos. Someone
    //  added the above comment and moved up the following code. So
    //  demos would desync in close lost soul fights.
    // Note that this only applies to original Doom 1 or Doom2 demos - not
    //  Final Doom and Ultimate Doom.  So we test demo_compatibility *and*
    //  gamemission. (Note we assume that Doom1 is always Ult Doom, which
    //  seems to hold for most published demos.)

    //
    //  fraggle - cph got the logic here slightly wrong.  There are three
    //  versions of Doom 1.9:
    //
    //  * The version used in registered doom 1.9 + doom2 - no bounce
    //  * The version used in ultimate doom - has bounce
    //  * The version used in final doom - has bounce
    //
    // So we need to check that this is either retail or commercial
    // (but not doom2)
    correct_lost_soul_bounce := gameversion >= exe_ultimate;

    if correct_lost_soul_bounce or (G_PlayingEngineVersion in [VERSION111..VERSION118]) then
      if mo.flags and MF_SKULLFLY <> 0 then
      begin
        // the skull slammed into something
        mo.momz := -mo.momz;
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
        player.deltaviewheight := _SHR(mo.momz, 3);

        // JVAL: 20211101 - Crouch
        if G_PlayingEngineVersion >= VERSION207 then
          player.deltaviewheight := FixedMul(player.deltaviewheight, FixedDiv(mo.height, mo.info.height));

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

    if not (G_PlayingEngineVersion in [VERSION111..VERSION118]) then
      if not correct_lost_soul_bounce and (mo.flags and MF_SKULLFLY <> 0) then
        mo.momz := -mo.momz;

    if (mo.flags and MF_MISSILE <> 0) and (mo.flags and MF_NOCLIP = 0) then
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
      mo.momz := - grav * 2
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
    // hit the ceiling
    if mo.momz > 0 then
    begin
      if mo.flags3_ex and MF3_EX_CEILINGBOUNCE <> 0 then
        mo.momz := -mo.momz div 2
      else
        mo.momz := 0;
    end;

    mo.z := ceilz - mo.height;

    if mo.flags and MF_SKULLFLY <> 0 then // JVAL: 20200308 - SOS - This is wrong, the momz has been already set to zero!!
      mo.momz := -mo.momz; // the skull slammed into something

    if (mo.flags and MF_MISSILE <> 0) and (mo.flags and MF_NOCLIP = 0) then
    begin
      P_ExplodeMissile(mo);
      exit;
    end;
  end;
end;

//
// P_NightmareRespawn
//
procedure P_NightmareRespawn(mobj: Pmobj_t);
var
  x: fixed_t;
  y: fixed_t;
  z: fixed_t;
  ss: Psector_t;
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
  if G_PlayingEngineVersion >= VERSION122 then
    mo := P_SpawnMobj(mobj.x, mobj.y, mobj.z, Ord(MT_TFOG))  // JVAL: 3d floors
  else
    mo := P_SpawnMobj(mobj.x, mobj.y, Psubsector_t(mobj.subsector).sector.floorheight, Ord(MT_TFOG));

  // initiate teleport sound
  S_StartSound(mo, Ord(sfx_telept));

  // spawn a teleport fog at the new spot
  ss := P_PointInSector(x, y);

// JVAL: 3d floors
  h := P_FloorHeight(ss, mo.x, mo.y);
  if ss.midsec >= 0 then
    if mobj.spawnpoint.options and MTF_ONMIDSECTOR <> 0 then
      h := sectors[ss.midsec].ceilingheight;

  mo := P_SpawnMobj(x, y, h, Ord(MT_TFOG));

  S_StartSound(mo, Ord(sfx_telept));

  // spawn the new monster
  mthing := @(mobj.spawnpoint);

  // spawn it
  if mobj.info.flags and MF_SPAWNCEILING <> 0 then
    z := ONCEILINGZ
  else if mobj.info.flags_ex and MF_EX_SPAWNFLOAT <> 0 then
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

  // killough 11/98: transfer friendliness from deceased
  if mobj.flags2_ex and MF2_EX_FRIEND = 0 then
    mo.flags2_ex := mo.flags2_ex and not MF2_EX_FRIEND
  else
    mo.flags2_ex := mo.flags2_ex or MF2_EX_FRIEND;

  mo.reactiontime := 18;

  // remove the old monster,
  P_RemoveMobj(mobj);
end;

//
// P_MobjThinker
//
procedure P_MobjThinker(mobj: Pmobj_t);
var
  onmo: Pmobj_t;
begin
  // JVAL: Clear just spawned flag
  mobj.flags := mobj.flags and not MF_JUSTAPPEARED;

  // momentum movement
  if (mobj.momx <> 0) or
     (mobj.momy <> 0) or
     (mobj.flags and MF_SKULLFLY <> 0) then
  begin
    P_XYMovement(mobj);

    if P_ThinkerIsRemoved(@mobj.thinker) then
      exit; // mobj was removed
  end;

  if mobj.flags_ex and MF_EX_FLOATBOB <> 0 then
  begin
    mobj.z := mobj.floorz + FloatBobOffsets[mobj.bob];
    mobj.bob := (mobj.bob + 1) and FLOATBOBMASK;
  end
  else if (mobj.z <> mobj.floorz) or (mobj.momz <> 0) then
  begin
    if mobj.flags2_ex and MF2_EX_PASSMOBJ <> 0 then
    begin
      onmo := P_CheckOnmobj(mobj);
      if onmo = nil then
        P_ZMovement(mobj)
      else
      begin
        if (mobj.player <> nil) and (mobj.momz < 0) then
        begin
          mobj.flags2_ex := mobj.flags2_ex or MF2_EX_ONMOBJ;
          mobj.momz := 0;
        end;
        if (mobj.player <> nil) and (onmo.player <> nil) then
        begin
          mobj.momx := onmo.momx;
          mobj.momy := onmo.momy;
          if onmo.z < onmo.floorz then
          begin
            mobj.z := mobj.z + onmo.floorz - onmo.z;
            if onmo.player <> nil then
            begin
              Pplayer_t(onmo.player).viewheight := Pplayer_t(onmo.player).viewheight - (onmo.floorz - onmo.z);
              Pplayer_t(onmo.player).deltaviewheight := (PVIEWHEIGHT - Pplayer_t(onmo.player).viewheight) div 8;
            end;
            onmo.z := onmo.floorz;
          end;
        end;
      end;
    end
    else
      P_ZMovement(mobj);

    if P_ThinkerIsRemoved(@mobj.thinker) then
      exit; // mobj was removed
  end;

  // cycle through states,
  // calling action functions at transitions
  if mobj.tics <> -1 then
  begin
    dec(mobj.tics);

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

    if mobj.movecount < 12 * TICRATE then
    begin
      exit;
    end;

    if leveltime and 31 <> 0 then
      exit;

    if P_Random > 4 then
      exit;

    P_NightmareRespawn(mobj);
  end;
end;

//
// P_SpawnMobj
//
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
// JVAL: Set MF_JUSTAPPEARED flag
  mobj.flags := info.flags or MF_JUSTAPPEARED;
  mobj.flags_ex := info.flags_ex;
  mobj.flags2_ex := info.flags2_ex;
  mobj.flags3_ex := info.flags3_ex;
  mobj.flags4_ex := info.flags4_ex;
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

  if gameskill <> sk_nightmare then
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
    mobj.z := mobj.floorz
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

    if (mobj.flags2_ex and MF2_EX_FLOORCLIP <> 0) and
       (P_GetThingFloorType(mobj) > FLOOR_SOLID) and
       (mobj.z = mobj.floorz) then
      mobj.floorclip := FOOTCLIPSIZE
    else
      mobj.floorclip := 0;
  end
  else
  begin
    if (mobj.flags2_ex and MF2_EX_FLOORCLIP <> 0) and
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
const
// Time interval for item respawning.
  ITEMQUESIZE = 128;

var
  itemrespawnque: array[0..ITEMQUESIZE - 1] of mapthing_t;
  itemrespawntime: array[0..ITEMQUESIZE - 1] of integer;

procedure P_RemoveMobj(mobj: Pmobj_t);
begin
  if ((mobj.flags and MF_SPECIAL) <> 0) and
     ((mobj.flags and MF_DROPPED) = 0) and
     (mobj._type <> Ord(MT_INV)) and
     (mobj._type <> Ord(MT_INS)) then
  begin
    itemrespawnque[iquehead] := mobj.spawnpoint;
    itemrespawntime[iquehead] := leveltime;
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

  // stop any playing sound
  S_StopSound(mobj);

  P_RemoveMobjCustomParams(mobj.customparams);

  // free block
  P_RemoveThinker(Pthinker_t(mobj));
end;

//
// P_RespawnSpecials
//
procedure P_RespawnSpecials;
var
  x: fixed_t;
  y: fixed_t;
  z: fixed_t;
  ss: Psector_t;
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

  // wait at least 30 seconds
  if leveltime - itemrespawntime[iquetail] < 30 * TICRATE then
    exit;

  mthing := @itemrespawnque[iquetail];

  x := mthing.x * FRACUNIT;
  y := mthing.y * FRACUNIT;

  // spawn a teleport fog at the new spot
  ss := P_PointInSector(x, y);
  mo := P_SpawnMobj(x, y, ss.floorheight, Ord(MT_IFOG), mthing);  // JVAL: Slopes
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

//
// P_SpawnPlayer
// Called when a player is spawned on the level.
// Most of the player structure stays unchanged
//  between levels.
//
function P_SpawnPlayer(mthing: Pmapthing_t): Pmobj_t;
var
  p: Pplayer_t;
  x: fixed_t;
  y: fixed_t;
  z: fixed_t;
  i: integer;
  plnum: integer;
  ss: Psector_t;
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

  x := mthing.x * FRACUNIT;
  y := mthing.y * FRACUNIT;
  z := ONFLOORZ;

  // JVAL: 20191209 - 3d floors - Fixed Player spawned in 3d floor
  ss := P_PointInSector(x, y);
  if ss.midsec >= 0 then
    if mthing.options and MTF_ONMIDSECTOR <> 0 then
      z := sectors[ss.midsec].ceilingheight;

  result := P_SpawnMobj(x, y, z, Ord(MT_PLAYER), @mthing);

  // set color translations for player sprites
  if mthing._type > 1 then
    result.flags := result.flags or _SHL(plnum, MF_TRANSSHIFT);

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

  // setup gun psprite
  P_SetupPsprites(p);

  // give all cards in death match mode
  if deathmatch <> 0 then
    for i := 0 to Ord(NUMCARDS) - 1 do
      p.cards[i] := true;

  if plnum = consoleplayer then
  begin
    // wake up the status bar
    ST_Start;
    // wake up the heads up text
    HU_Start;

    p_justspawned := true;
  end;
end;

//
// P_SpawnMapThing
// The fields of the mapthing should
// already be in host byte order.
//
function P_SpawnMapThing(mthing: Pmapthing_t): Pmobj_t;
var
  i: integer;
  bit: integer;
  x: fixed_t;
  y: fixed_t;
  z: fixed_t;
  ss: Psector_t; // JVAL: 3d floors
  msec: Psector_t;  // JVAL: 3d floors
  musinfoparam: integer;
begin
  mthing.options := mthing.options and spawnmask;

  // Count deathmatch start positions
  if mthing._type = 11 then
  begin
    if deathmatch_p < MAX_DEATHMATCH_STARTS then
    begin
      memcpy(@deathmatchstarts[deathmatch_p], mthing, SizeOf(mthing^));
      inc(deathmatch_p);
    end;
    result := nil;
    exit;
  end;

  // phares 5/14/98: Ignore Player 5-8 starts (for now)
  if (mthing._type = DEN_PLAYER5) or
     (mthing._type = DEN_PLAYER6) or
     (mthing._type = DEN_PLAYER7) or
     (mthing._type = DEN_PLAYER8) then
  begin
    result := nil;
    exit;
  end;

  // check for players specially
  if mthing._type <= 4 then
  begin
    // save spots for respawning in network games
    if not netgame and (mthing._type > 1) and (mthing._type <= dogs + 1) then
    begin
      // use secretcount to avoid multiple dogs in case of multiple starts
      players[mthing._type - 1].secretcount := 1;
      mthing._type := mobjinfo[Ord(MT_DOGS)].doomednum;
      if mthing._type <= 0 then
      begin
        result := nil;
        exit;
      end;
      mthing.options := mthing.options or MTF_FRIEND;
    end
    else
    begin
      playerstarts[mthing._type - 1] := mthing^;
      if deathmatch = 0 then
        result := P_SpawnPlayer(mthing)
      else
        result := nil;
      exit;
    end;
  end;

  // check for apropriate skill level
  if not netgame and (mthing.options and 16 <> 0) then
  begin
    result := nil;
    exit;
  end;

  //jff 3/30/98 implement "not deathmatch" thing flag
  if netgame and (deathmatch <> 0) and (mthing.options and 32 <> 0) then
  begin
    result := nil;
    exit;
  end;

  //jff 3/30/98 implement "not cooperative" thing flag
  if netgame and (deathmatch = 0) and (mthing.options and 64 <> 0) then
  begin
    result := nil;
    exit;
  end;

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
  if nomonsters and
    ((i = Ord(MT_SKULL)) or (mobjinfo[i].flags and MF_COUNTKILL <> 0)) then
  begin
    result := nil;
    exit;
  end;

  // spawn it
  x := mthing.x * FRACUNIT;
  y := mthing.y * FRACUNIT;

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

// JVAL: 3d floors
  ss := P_PointInSector(x, y);
  if ss.midsec >= 0 then
  begin
    msec := @sectors[ss.midsec];
    if mthing.options and MTF_ONMIDSECTOR <> 0 then
    begin
      if z = ONFLOATZ then
        z := (msec.ceilingheight + P_CeilingHeight(ss, x, y)) div 2
      else if z = ONFLOORZ then
        z := msec.ceilingheight;
    end
    else
    begin
      if z = ONFLOATZ then
        z := (P_FloorHeight(ss, x, y) + msec.floorheight) div 2
      else if z = ONCEILINGZ then
        z := msec.floorheight;
    end;
  end;

  result := P_SpawnMobj(x, y, z, i, mthing);
  result.spawnpoint := mthing^;

  if mthing.options and MTF_FRIEND <> 0 then
    result.flags2_ex := result.flags2_ex or MF2_EX_FRIEND;

  if musinfoparam >= 0 then
    P_SetMobjCustomParam(result, S_MUSINFO_PARAM, musinfoparam);

  if result.tics > 0 then
    result.tics := 1 + (P_Random mod result.tics);
  if result.flags and MF_COUNTKILL <> 0 then
    inc(totalkills);
  if result.flags and MF_COUNTITEM <> 0 then
    inc(totalitems);

  if result.flags2_ex and MF2_EX_PRECISESPAWNANGLE <> 0 then
    result.angle := ANG1 * mthing.angle
  else
    result.angle := ANG45 * (mthing.angle div 45);
  if mthing.options and MTF_AMBUSH <> 0 then
    result.flags := result.flags or MF_AMBUSH;
end;

//
// GAME SPAWN FUNCTIONS
//


//
// P_SpawnPuff
//
procedure P_SpawnPuff(x, y, z: fixed_t);
var
  th: Pmobj_t;
begin
  z := z + _SHL(P_Random - P_Random, 10);

  th := P_SpawnMobj(x, y, z, Ord(MT_PUFF));
  th.momz := FRACUNIT;
  th.tics := th.tics - (P_Random and 3);

  if th.tics < 1 then
    th.tics := 1;

  // don't make punches spark on the wall
  if attackrange = MELEERANGE then
    P_SetMobjState(th, S_PUFF3);
end;

//
// P_SpawnBlood
//
procedure P_SpawnBlood(x, y, z: fixed_t; damage: integer);
var
  th: Pmobj_t;
begin
  z := z + _SHL(P_Random - P_Random, 10);
  th := P_SpawnMobj(x, y, z, Ord(MT_BLOOD));
  th.momz := FRACUNIT * 2;
  th.tics := th.tics - (P_Random and 3);

  if th.tics < 1 then
    th.tics := 1;

  if th.flags3_ex and MF3_EX_BLOODIGNOREDAMAGE = 0 then
  begin
    if (damage <= 12) and (damage >= 9) then
      P_SetMobjState(th, S_BLOOD2)
    else if damage < 9 then
      P_SetMobjState(th, S_BLOOD3);
  end;
end;

procedure P_SpawnGreenBlood(x, y, z: fixed_t; damage: integer);
var
  th: Pmobj_t;
begin
  z := z + _SHL(N_Random - N_Random, 10);

  th := P_SpawnMobj(x, y, z, MT_GREENBLOOD);
  th.momz := FRACUNIT * 2;
  th.tics := th.tics - (N_Random and 3);

  if th.tics < 1 then
    th.tics := 1;

  if th.flags3_ex and MF3_EX_BLOODIGNOREDAMAGE = 0 then
  begin
    if (damage <= 12) and (damage >= 9) then
      P_SetMobjRelativeState(th, 1)
    else if damage < 9 then
      P_SetMobjRelativeState(th, 2);
  end;
end;

procedure P_SpawnBlueBlood(x, y, z: fixed_t; damage: integer);
var
  th: Pmobj_t;
begin
  z := z + _SHL(N_Random - N_Random, 10);

  th := P_SpawnMobj(x, y, z, MT_BLUEBLOOD);
  th.momz := FRACUNIT * 2;
  th.tics := th.tics - (N_Random and 3);

  if th.tics < 1 then
    th.tics := 1;

  if th.flags3_ex and MF3_EX_BLOODIGNOREDAMAGE = 0 then
  begin
    if (damage <= 12) and (damage >= 9) then
      P_SetMobjRelativeState(th, 1)
    else if damage < 9 then
      P_SetMobjRelativeState(th, 2);
  end;
end;


//
// P_CheckMissileSpawn
// Moves the missile forward a bit
//  and possibly explodes it right there.
//
function P_CheckMissileSpawn(th: Pmobj_t): boolean;
begin
  th.tics := th.tics - (P_Random and 3);

  if th.tics < 1 then
    th.tics := 1;

  // move a little forward so an angle can
  // be computed if it immediately explodes
  if G_PlayingEngineVersion in [VERSION122..VERSION205] then
  begin
    th.x := th.x + th.momx div 2;
    th.y := th.y + th.momy div 2;
    th.z := th.z + th.momz div 2;
  end
  else
  begin
    th.x := th.x + _SHR1(th.momx);
    th.y := th.y + _SHR1(th.momy);
    th.z := th.z + _SHR1(th.momz);
  end;

  if not P_TryMove(th, th.x, th.y) then
  begin
    P_ExplodeMissile(th);
    result := false;
  end
  else
    result := true;
end;

//
// P_SpawnMissile
//

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

  A_SeeSound(th, th);

  th.target := source;  // where it came from
  an := R_PointToAngle2(source.x, source.y, dest.x, dest.y);

  // fuzzy player
  if dest.flags and MF_SHADOW <> 0 then
    if source.flags2_ex and MF2_EX_SEEINVISIBLE = 0 then
      an := an + _SHLW(P_Random - P_Random, 20);

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

function P_SpawnMissileXYZ(x, y, z: fixed_t; source: Pmobj_t; dest: Pmobj_t; _type: integer): Pmobj_t;
var
  flags_ex: integer;
  th: Pmobj_t;
  velocity: vec3_t;
  speed: float;
  an: angle_t;
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

  th := P_SpawnMobj(x, y, z, _type);

  A_SeeSound(th, th);

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

  A_SeeSound(mo, mo);

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

function P_SpawnMissileAngleZ(source: Pmobj_t; z: fixed_t; _type: integer; angle: angle_t;
  momz: fixed_t; speed: fixed_t): Pmobj_t;
begin
  result := P_SpawnMissileAngleZSpeed(source, z, _type, angle, momz, mobjinfo[Ord(_type)].speed, nil);
end;

//
// P_SpawnPlayerMissile
// Tries to aim at a nearby monster
//
procedure P_SpawnPlayerMissile(source: Pmobj_t; _type: integer);
var
  th: Pmobj_t;
  an: angle_t;
  x: fixed_t;
  y: fixed_t;
  z: fixed_t;
  slope: fixed_t;
  ver: integer;
  speed: fixed_t;
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

      ver := G_PlayingEngineVersion;
      if ver > VERSION110 then
      begin
        if ver < VERSION204 then
        begin
          if zaxisshift and (linetarget = nil) then
          begin
            an := source.angle;
            slope := (Pplayer_t(source.player).lookdir * FRACUNIT) div 173;
          end;
        end
        else
        begin
          if linetarget = nil then
          begin
            an := source.angle;
            slope := (Pplayer_t(source.player).lookdir * FRACUNIT) div 173;
          end;
        end;
      end
      else
      begin
        if linetarget = nil then
        begin
          an := source.angle;
          slope := 0;
        end;
      end;

    end;
  end;

  x := source.x;
  y := source.y;
  // Also z axis shift calculation
  if zaxisshift then
    z := source.z + 4 * 8 * FRACUNIT + (Pplayer_t(source.player).lookdir * FRACUNIT) div 173
  else
    z := source.z + 4 * 8 * FRACUNIT;

  th := P_SpawnMobj(x, y, z, _type);

  A_SeeSound(th, th);

  th.target := source;
  th.angle := an;
  an := an shr ANGLETOFINESHIFT;
  speed := th.info.speed;
  th.momx := FixedMul(speed, finecosine[an]);
  th.momy := FixedMul(speed, finesine[an]);
  th.momz := FixedMul(speed, slope);

  P_CheckMissileSpawn(th);
end;

//----------------------------------------------------------------------------
//
// FUNC P_FaceMobj
//
// Returns 1 if 'source' needs to turn clockwise, or 0 if 'source' needs
// to turn counter clockwise.  'delta' is set to the amount 'source'
// needs to turn.
//
//----------------------------------------------------------------------------
function P_FaceMobj(source: Pmobj_t; target: Pmobj_t; var delta: angle_t): integer;
var
  diff: angle_t;
  angle1: angle_t;
  angle2: angle_t;
begin
  angle1 := source.angle;
  angle2 := R_PointToAngle2(source.x, source.y, target.x, target.y);
  if angle2 > angle1 then
  begin
    diff := angle2 - angle1;
    if diff > ANG180 then
    begin
      delta := ANGLE_MAX - diff;
      result := 0;
    end
    else
    begin
      delta := diff;
      result := 1;
    end;
  end
  else
  begin
    diff := angle1 - angle2;
    if diff > ANG180 then
    begin
      delta := ANGLE_MAX - diff;
      result := 1;
    end
    else
    begin
      delta := diff;
      result := 0;
    end;
  end;
end;

//----------------------------------------------------------------------------
//
// FUNC P_SeekerMissile
//
// The missile's tracer field must be the target.  Returns true if
// target was tracked, false if not.
//
//----------------------------------------------------------------------------
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

//---------------------------------------------------------------------------
//
// FUNC P_GetThingFloorType
//
//---------------------------------------------------------------------------
// JVAL: 9 December 2007, Added terrain types
function P_GetThingFloorType(thing: Pmobj_t): integer;
begin
  result := flats[Psubsector_t(thing.subsector).sector.floorpic].terraintype;
end;

//---------------------------------------------------------------------------
//
// FUNC P_HitFloor
//
//---------------------------------------------------------------------------
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

  // Exit if playing DelphiDoom demo from version 114 or lower
  if G_PlayingEngineVersion <= VERSION114 then
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
          P_SpawnMobj(thing.x, thing.y, z, Ord(MT_NUKAGESPLASH));
          mo := P_SpawnMobj(thing.x, thing.y, z, Ord(MT_NUKAGECHUNK));
          mo.target := thing;
          mo.momx := (P_Random - P_Random) * 256;
          mo.momy := (P_Random - P_Random) * 256;
          mo.momz := FRACUNIT + (P_Random * 256);
          S_StartSound(mo, Ord(sfx_sgloo2));
        end;
        result := FLOOR_NUKAGE;
        exit;
      end;
  end;
end;

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
  tmp := strupper(strtrim(tmp));

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

procedure MObj_Init;
begin
  mobjlist := TMobjList.Create;
  C_AddCmd('spawnmobj, p_spawnmobj', @CmdSpawnMobj);
  C_AddCmd('summon', @CmdSummon);
end;

procedure MObj_ShutDown;
begin
  mobjlist.Free;
end;

//
// FUNC P_FindMobjFromKey
//
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

