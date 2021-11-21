//------------------------------------------------------------------------------
//
//  DelphiHeretic: A modified and improved Heretic port for Windows
//  based on original Linux Doom as published by "id Software", on
//  Heretic source as published by "Raven" software and DelphiDoom
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

function P_SetMobjStateNF(mobj: Pmobj_t; state: statenum_t): boolean;

procedure P_ExplodeMissile(mo: Pmobj_t);

procedure P_BlasterMobjThinker(mobj: Pmobj_t);

procedure P_MobjThinker(mobj: Pmobj_t);

function P_SpawnMobj(x, y, z: fixed_t; _type: integer; const mthing: Pmapthing_t = nil): Pmobj_t;

procedure P_RemoveMobj(mobj: Pmobj_t);

function P_SpawnPlayer(mthing: Pmapthing_t): Pmobj_t;

function P_SpawnMapThing(mthing: Pmapthing_t): Pmobj_t;

procedure P_SpawnPuff(x, y, z: fixed_t);

function P_SpawnMissile(source: Pmobj_t; dest: Pmobj_t; _type: integer): Pmobj_t;

function P_SpawnMissileAngle(source: Pmobj_t; _type: integer; angle: angle_t; momz: fixed_t): Pmobj_t;

function P_SpawnMissileXYZ(x, y, z: fixed_t; source: Pmobj_t; dest: Pmobj_t; _type: integer): Pmobj_t;

function P_SpawnMissileAngleZ(source: Pmobj_t; z: fixed_t; _type: integer; angle: angle_t;
  momz: fixed_t; speed: fixed_t): Pmobj_t;

function P_SpawnMissileAngleZSpeed(source: Pmobj_t; z: fixed_t; _type: integer; angle: angle_t;
  momz: fixed_t; speed: fixed_t; owner: Pmobj_t): Pmobj_t;

function P_SpawnPlayerMissile(source: Pmobj_t; _type: integer): Pmobj_t;

procedure P_RespawnSpecials;

procedure P_SpawnBlood(x, y, z: fixed_t; damage: integer);

procedure P_BloodSplatter(x, y, z: fixed_t; originator: Pmobj_t);

procedure P_RipperBlood(mo: Pmobj_t);

function P_CheckMissileSpawn(th: Pmobj_t): boolean;

function P_SeekerMissile(actor: Pmobj_t; thresh, turnMax: angle_t): boolean;

function P_HitFloor(thing: Pmobj_t): integer;

function P_GetThingFloorType(thing: Pmobj_t): integer;

function P_SPMAngle(source: Pmobj_t; _type: integer; angle: angle_t): Pmobj_t;

procedure P_ThrustMobj(mo: Pmobj_t; angle: angle_t; const move: fixed_t);

var
  iquehead: integer; // Initialized at p_setup
  iquetail: integer; // Initialized at p_setup

var
  PuffType: mobjtype_t;
  MissileMobj: Pmobj_t;

function P_FindMobjFromKey(const key: LongWord): Pmobj_t;

procedure MObj_Init;

procedure MObj_ShutDown;

implementation

uses
  d_delphi,
  c_cmds,
  sc_engine,
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
  p_extra,
  p_common,
  p_ambient,
  p_enemy,
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
  hu_stuff,
  s_sound,
  sounds,
  info,
  info_rnd,
  info_common,
  doomstat;

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

    mobj.prevstate := mobj.state;
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

//----------------------------------------------------------------------------
//
// FUNC P_SetMobjStateNF
//
// Same as P_SetMobjState, but does not call the state function.
//
//----------------------------------------------------------------------------

function P_SetMobjStateNF(mobj: Pmobj_t; state: statenum_t): boolean;
var
  st: Pstate_t;
begin
  if state = S_NULL then
  begin // Remove mobj
    mobj.state := @states[Ord(S_NULL)];
    P_RemoveMobj(mobj);
    result := false;
  end
  else
  begin
    st := @states[Ord(state)];
    mobj.state := st;
    mobj.tics := P_TicsFromState(st);
    mobj.sprite := st.sprite;
    mobj.frame := st.frame;
    result := true;
  end;
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

  if mo.info.deathsound <> 0 then
  begin
    if mo.info.flags_ex and MF_EX_RANDOMDEATHSOUND <> 0 then
      P_RandomSound(mo, mo.info.deathsound)
    else
      S_StartSound(mo, mo.info.deathsound);
  end;
end;

//
// P_XYMovement
//
const
  STOPSPEED = $1000;
  FRICTION_LOW = $f900;
  FRICTION_FLY = $eb00;
  windTab: array[0..2] of integer = (2048 * 5, 2048 * 10, 2048 * 25);

procedure P_XYMovement(mo: Pmobj_t);
var
  ptryx: fixed_t;
  ptryy: fixed_t;
  player: Pplayer_t;
  xmove: fixed_t;
  ymove: fixed_t;
  special: integer;
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

      P_SetMobjState(mo, statenum_t(mo.info.seestate));
    end;
    exit;
  end;

  special := Psubsector_t(mo.subsector).sector.special;
  if mo.flags2 and MF2_WINDTHRUST <> 0 then
  begin
    case special of
      40, 41, 42: // Wind_East
        P_ThrustMobj(mo, 0, windTab[special - 40]);
      43, 44, 45: // Wind_North
        P_ThrustMobj(mo, ANG90, windTab[special - 43]);
      46, 47, 48: // Wind_South
        P_ThrustMobj(mo, ANG270, windTab[special - 46]);
      49, 50, 51: // Wind_West
        P_ThrustMobj(mo, ANG180, windTab[special - 49]);
    end;
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
      else if (mo.flags3_ex and MF3_EX_SLIDING <> 0) or (mo.flags and MF_SLIDEONWALLS <> 0) or (mo.flags2 and MF2_SLIDE <> 0) and (mo.flags and MF_MISSILE = 0) then
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
          if mo._type = Ord(MT_BLOODYSKULL) then
          begin
            mo.momx := 0;
            mo.momy := 0;
            mo.momz := -FRACUNIT;
          end
          else
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

  if (mo.flags3_ex and MF3_EX_BOUNCE) <> 0 then
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
    if player <> nil then
    begin
      if player.chickenTics <> 0 then
      begin
        if (LongWord((pDiff(player.mo.state, @states[0], SizeOf(states[0]))) - Ord(S_CHICPLAY_RUN1)) < 4) then
          P_SetMobjState(player.mo, S_CHICPLAY);
      end
      else
      begin
        if (LongWord((pDiff(player.mo.state, @states[0], SizeOf(states[0]))) - Ord(S_PLAY_RUN1)) < 4) then
          P_SetMobjState(player.mo, S_PLAY);
      end;
    end;

    mo.momx := 0;
    mo.momy := 0;
  end
  else
  begin
    if (mo.flags2 and MF2_FLY <> 0) and (mo.z > mo.floorz) and
       (mo.flags2 and MF2_ONMOBJ = 0) then
    begin
      mo.momx := FixedMul(mo.momx, FRICTION_FLY);
      mo.momy := FixedMul(mo.momy, FRICTION_FLY);
    end
    else if special = 15 then // Friction_Low
    begin
      mo.momx := FixedMul(mo.momx, FRICTION_LOW);
      mo.momy := FixedMul(mo.momy, FRICTION_LOW);
    end
    else
    begin
      mo.momx := FixedMul(mo.momx, mo.friction);
      mo.momy := FixedMul(mo.momy, mo.friction);
    end;
  end;
end;

//----------------------------------------------------------------------------
//
// PROC P_FloorBounceMissile
//
//----------------------------------------------------------------------------

procedure P_FloorBounceMissile(mo: Pmobj_t);
begin
  mo.momz := -mo.momz;
  P_SetMobjState(mo, statenum_t(mobjinfo[mo._type].deathstate));
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
  ladderticks: integer;
  player: Pplayer_t;
begin
  ladderticks := 0;
  player := Pplayer_t(mo.player);
  if player <> nil then
    if player.laddertics > 0 then
    begin
      Dec(player.laddertics);
      ladderticks := player.laddertics;
      if ladderticks = 0 then
        mo.momz := 0;
    end;

  // check for smooth step up
  if (player <> nil) and (mo.z < mo.floorz) and (ladderticks = 0) then
  begin
    player.viewheight := player.viewheight - (mo.floorz - mo.z);
    player.deltaviewheight :=
      _SHR((PVIEWHEIGHT - player.crouchheight - player.viewheight), 3);
  end;

  // adjust height
  if ladderticks > 0 then
  begin
    mo.z := mo.z + mo.momz;
    mo.momz := (mo.momz * 7) div 8;
    if mo.momz < FRACUNIT div 8 then
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

  if (player <> nil) and (mo.flags2 and MF2_FLY <> 0) and (mo.z > mo.floorz) and (leveltime and 2 <> 0) then
    mo.z := mo.z + finesine[(FINEANGLES div 20 * leveltime div 4) and FINEMASK];

  // clip movement
  if mo.z <= mo.floorz then
  begin
    // hit the floor
    if mo.flags and MF_MISSILE <> 0 then
    begin
      mo.z := mo.floorz;
      if (mo.flags2 and MF2_FLOORBOUNCE <> 0) or (mo.flags3_ex and MF3_EX_FLOORBOUNCE <> 0) then
      begin
        P_FloorBounceMissile(mo);
        exit;
      end
      else if mo._type = Ord(MT_MNTRFX2) then
      begin // Minotaur floor fire can go up steps
        exit;
      end
      else
      begin
        P_ExplodeMissile(mo);
        exit;
      end;
    end;

    if mo.z - mo.momz > mo.floorz then
    begin // Spawn splashes, etc.
      P_HitFloor(mo);
    end;
    mo.z := mo.floorz;
    if mo.momz < 0 then
    begin
      if (player <> nil) and (mo.momz < - P_GetMobjGravity(mo) * 8) and (mo.flags2 and MF2_FLY = 0) then // squat down
      begin
        player.deltaviewheight := mo.momz div 8;
        S_StartSound(mo, Ord(sfx_plroof));
        player.centering := true; // JVAL: check
      end;
      if mo.flags3_ex and MF3_EX_FLOORBOUNCE <> 0 then
        mo.momz := -mo.momz div 2
      else
        mo.momz := 0;
    end;

    // Note (id):
    //  somebody left this after the setting momz to 0,
    //  kinda useless there.
    if mo.flags and MF_SKULLFLY <> 0 then
    begin
      // the skull slammed into something
      mo.momz := -mo.momz;
    end;

    if (mo.info.crashstate <> 0) and (mo.flags and MF_CORPSE <> 0) then
    begin
      P_SetMobjState(mo, statenum_t(mo.info.crashstate));
      exit;
    end;

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
          S_StartSound(mo, Ord(sfx_plroof));
          player.nextoof := leveltime + 4 * TICRATE;
        end;
      end;
      mo.momz := 0;
    end;
    mo.z := mo.floorz;

    if (mo.flags and MF_MISSILE <> 0) and (mo.flags and MF_NOCLIP = 0) then
    begin
      P_ExplodeMissile(mo);
      exit;
    end;
  end
  else if (mo.flags2 and MF2_LOGRAV <> 0) or (mo.flags_ex and MF_EX_LOWGRAVITY <> 0)  then
  begin
    if mo.momz = 0 then
      mo.momz := -(P_GetMobjGravity(mo) div 8) * 2
    else
      mo.momz := mo.momz - P_GetMobjGravity(mo) div 8;
  end
  else if mo.flags2_ex and MF2_EX_MEDIUMGRAVITY <> 0 then
  begin
    if mo.momz = 0 then
      mo.momz := -(P_GetMobjGravity(mo) div 8) * 4
    else
      mo.momz := mo.momz - P_GetMobjGravity(mo) div 4;
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

  end;

  ceilz := mo.ceilingz + P_SectorJumpOverhead(Psubsector_t(mo.subsector).sector, mo.player);

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

    if mo.flags and MF_SKULLFLY <> 0 then
      mo.momz := -mo.momz; // the skull slammed into something

//    if (mo.flags and MF_MISSILE <> 0) and (mo.flags and MF_NOCLIP = 0) then
    if mo.flags and MF_MISSILE <> 0 then
    begin
      if Psubsector_t(mo.subsector).sector.ceilingpic = skyflatnum then
      begin
        if mo._type = Ord(MT_BLOODYSKULL) then
        begin
          mo.momx := 0;
          mo.momy := 0;
          mo.momz := -FRACUNIT;
        end
        else
          P_RemoveMobj(mo);
        exit;
      end;
      P_ExplodeMissile(mo);
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
  ss := R_PointInSubsector(x, y);

// JVAL: 3d floors
  h := ss.sector.floorheight;
  if ss.sector.midsec >= 0 then
    if mobj.spawnpoint.options and MTF_ONMIDSECTOR <> 0 then
      h := sectors[ss.sector.midsec].ceilingheight;

  mo := P_SpawnMobj(x, y, h, Ord(MT_TFOG));

  S_StartSound(mo, Ord(sfx_telept));

  // spawn the new monster
  mthing := @(mobj.spawnpoint);

  // spawn it
  if mobj.info.flags and MF_SPAWNCEILING <> 0 then
    z := ONCEILINGZ
  else if mobj.info.flags_ex and MF_EX_SPAWNFLOAT <> 0 then
    z := ONFLOATZ
  else if mobj.info.flags2 and MF2_SPAWNFLOAT <> 0 then
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

  mo.reactiontime := 18;

  // remove the old monster,
  P_RemoveMobj(mobj);
end;

//----------------------------------------------------------------------------
//
// PROC P_BlasterMobjThinker
//
// Thinker for the ultra-fast blaster PL2 ripper-spawning missile.
//
//----------------------------------------------------------------------------

procedure P_BlasterMobjThinker(mobj: Pmobj_t);
var
  i: integer;
  xfrac: fixed_t;
  yfrac: fixed_t;
  zfrac: fixed_t;
  z: fixed_t;
  changexy: boolean;
begin
  // Handle movement
  if (mobj.momx <> 0) or (mobj.momy <> 0) or
     (mobj.z <> mobj.floorz) or (mobj.momz <> 0) then
  begin
    xfrac := mobj.momx div 8;
    yfrac := mobj.momy div 8;
    zfrac := mobj.momz div 8;
    changexy := (xfrac <> 0) or (yfrac <> 0);
    for i := 0 to 7 do
    begin
      if changexy then
      begin
        if not P_TryMove(mobj, mobj.x + xfrac, mobj.y + yfrac) then
        begin // Blocked move
          P_ExplodeMissile(mobj);
          exit;
        end;
      end;
      mobj.z := mobj.z + zfrac;
      if mobj.z <= mobj.floorz then
      begin // Hit the floor
        mobj.z := mobj.floorz;
        P_HitFloor(mobj);
        P_ExplodeMissile(mobj);
        exit;
      end;
      if mobj.z + mobj.height > mobj.ceilingz then
      begin // Hit the ceiling
        mobj.z := mobj.ceilingz - mobj.height;
        P_ExplodeMissile(mobj);
        exit;
      end;
      if changexy and (P_Random < 64) then
      begin
        z := mobj.z - 8 * FRACUNIT;
        if z < mobj.floorz then
        begin
          z := mobj.floorz;
        end;
        P_SpawnMobj(mobj.x, mobj.y, z, Ord(MT_BLASTERSMOKE));
      end;
    end;
  end;
  // Advance the state
  if mobj.tics <> -1 then
  begin
    dec(mobj.tics);
    while mobj.tics = 0 do
    begin
      if not P_SetMobjState(mobj, statenum_t(mobj.state.nextstate)) then
      begin // mobj was removed
        exit;
      end;
    end;
  end;
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

  if (mobj.flags2 and MF2_FLOATBOB <> 0) or (mobj.flags_ex and MF_EX_FLOATBOB <> 0) then
  begin
    mobj.z := mobj.floorz + FloatBobOffsets[mobj.bob];
    mobj.bob := (mobj.bob + 1) and FLOATBOBMASK;
  end
  else if (mobj.z <> mobj.floorz) or (mobj.momz <> 0) then
  begin
    if mobj.flags2 and MF2_PASSMOBJ <> 0 then
    begin
      onmo := P_CheckOnmobj(mobj);
      if onmo = nil then
        P_ZMovement(mobj)
      else
      begin
        if (mobj.player <> nil) and (mobj.momz < 0) then
        begin
          mobj.flags2 := mobj.flags2 or MF2_ONMOBJ;
          mobj.momz := 0;
        end;
        if (mobj.player <> nil) and ((onmo.player <> nil) or (onmo._type = Ord(MT_POD))) then
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
    while mobj.tics = 0 do
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
  mobj.flags2 := info.flags2;
  mobj.flags_ex := info.flags_ex;
  mobj.flags2_ex := info.flags2_ex;
  mobj.flags3_ex := info.flags3_ex;
  mobj.flags4_ex := info.flags4_ex;
  mobj.scale := info.scale;
  mobj.gravity := info.gravity;
  mobj.pushfactor := info.pushfactor;
  mobj.friction := info.friction;
  mobj.damage := info.damage;
  mobj.renderstyle := info.renderstyle;
  mobj.alpha := info.alpha;
  if (mobj.flags2 and MF2_FLOATBOB <> 0) or (mobj.flags_ex and MF_EX_FLOATBOB <> 0) then
    mobj.bob := P_Random and FLOATBOBMASK;
  mobj.health := info.spawnhealth;
  mobj.mass := info.mass;
  mobj.WeaveIndexXY := info.WeaveIndexXY;
  mobj.WeaveIndexZ := info.WeaveIndexZ;
  mobj.painchance := info.painchance;

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
  mobj.tics := P_TicsFromState(st);
  mobj.sprite := st.sprite;
  mobj.frame := st.frame;

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
      mobj.z := FixedMul(space, P_Random * 256) + mobj.floorz + 40 * FRACUNIT
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

    if (mobj.flags2_ex and MF2_FOOTCLIP <> 0) and
       (P_GetThingFloorType(mobj) > FLOOR_SOLID) and
       (mobj.z = mobj.floorz) then
      mobj.flags2 := mobj.flags2 or MF2_FEETARECLIPPED
    else
      mobj.flags2 := mobj.flags2 and not MF2_FEETARECLIPPED;
  end
  else
  begin
    if (mobj.flags2 and MF2_FOOTCLIP <> 0) and (P_GetThingFloorType(mobj) <> FLOOR_SOLID) and
       (mobj.floorz = sec.floorheight) then
      mobj.flags2 := mobj.flags2 or MF2_FEETARECLIPPED
    else
      mobj.flags2 := mobj.flags2 and not MF2_FEETARECLIPPED;
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
     ((mobj.flags and MF_DROPPED) = 0) then
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

  // wait at least 30 seconds
  if leveltime - itemrespawntime[iquetail] < 30 * TICRATE then
    exit;

  mthing := @itemrespawnque[iquetail];

  x := mthing.x * FRACUNIT;
  y := mthing.y * FRACUNIT;

  // spawn a teleport fog at the new spot
  ss := R_PointInSubsector(x, y);
  mo := P_SpawnMobj(x, y, ss.sector.floorheight, Ord(MT_TFOG), mthing);
  // initiate teleport sound
  S_StartSound(mo, Ord(sfx_telept));

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
  else if mobjinfo[i].flags2 and MF2_SPAWNFLOAT <> 0 then
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
  ss: Psubsector_t;
begin
  result := nil;
  // not playing?
  if not playeringame[mthing._type - 1] then
    exit;

  plnum := mthing._type - 1;
  p := @players[plnum];

  if p.playerstate = PST_REBORN then
    G_PlayerReborn(plnum);

  x := mthing.x * FRACUNIT;
  y := mthing.y * FRACUNIT;
  z := ONFLOORZ;

  // JVAL: 20191209 - 3d floors - Fixed Player spawned in 3d floor
  ss := R_PointInSubsector(x, y);
  if ss.sector.midsec >= 0 then
    if mthing.options and MTF_ONMIDSECTOR <> 0 then
      z := sectors[ss.sector.midsec].ceilingheight;

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
    for i := 0 to Ord(NUMKEYCARDS) - 1 do
      p.keys[i] := true;

  if plnum = consoleplayer then
  begin
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
  ss: Psubsector_t; // JVAL: 3d floors
  msec: Psector_t;  // JVAL: 3d floors
  musinfoparam: integer;
begin
  result := nil;
  // Count deathmatch start positions
  if mthing._type = 11 then
  begin
    if deathmatch_p < MAX_DEATHMATCH_STARTS then
    begin
      memcpy(@deathmatchstarts[deathmatch_p], mthing, SizeOf(mthing^));
      inc(deathmatch_p);
    end;
    exit;
  end;

  // check for players specially
  if mthing._type <= 4 then
  begin
    // save spots for respawning in network games
    playerstarts[mthing._type - 1] := mthing^;
    if deathmatch = 0 then
      result := P_SpawnPlayer(mthing);
    exit;
  end;

  // Ambient sound sequences
  if (mthing._type >= 1200) and (mthing._type < 1300) then
  begin
    P_AddAmbientSfx(mthing._type - 1200);
    exit;
  end;

  // Check for boss spots
  if mthing._type = 56 then // Monster_BossSpot
  begin
    P_AddBossSpot(mthing.x * FRACUNIT, mthing.y * FRACUNIT,  ANG1 * mthing.angle);
    exit;
  end;

  // check for apropriate skill level
  if not netgame and (mthing.options and 16 <> 0) then
    exit;

  if gameskill = sk_baby then
    bit := 1
  else if gameskill = sk_nightmare then
    bit := 4
  else
    bit := _SHL(1, Ord(gameskill) - 1);

  if mthing.options and bit = 0 then
    exit;

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
      exit;
  end;
  // don't spawn keycards and players in deathmatch
  if (deathmatch <> 0) and ((mobjinfo[i].flags and MF_NOTDMATCH) <> 0) then
    exit;

  // don't spawn any monsters if -nomonsters
  if nomonsters and (mobjinfo[i].flags and MF_COUNTKILL <> 0) then
    exit;

// spawn it
  case i of
  // Special stuff
    Ord(MT_WSKULLROD),
    Ord(MT_WPHOENIXROD),
    Ord(MT_AMSKRDWIMPY),
    Ord(MT_AMSKRDHEFTY),
    Ord(MT_AMPHRDWIMPY),
    Ord(MT_AMPHRDHEFTY),
    Ord(MT_AMMACEWIMPY),
    Ord(MT_AMMACEHEFTY),
    Ord(MT_ARTISUPERHEAL),
    Ord(MT_ARTITELEPORT),
    Ord(MT_ITEMSHIELD2):
      begin
        if gamemode = shareware then // Don't place on map in shareware version
          exit;
      end;

    Ord(MT_WMACE):
      begin
        if gamemode <> shareware then // Put in the mace spot list
          P_AddMaceSpot(mthing);
        exit;
      end;
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
  else if mobjinfo[i].flags2 and MF2_SPAWNFLOAT <> 0 then
    z := ONFLOATZ
  else
    z := ONFLOORZ;

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
  if result.flags and MF_COUNTITEM <> 0 then
    inc(totalitems);

  if result.flags2_ex and MF2_EX_PRECISESPAWNANGLE <> 0 then
    result.angle := ANG1 * mthing.angle
  else
    result.angle := ANG45 * (mthing.angle div 45);

  if mthing.options and MTF_DONOTTRIGGERSCRIPTS <> 0 then
    result.flags2_ex := result.flags2_ex or MF2_EX_DONTRUNSCRIPTS;

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
  puff: Pmobj_t;
begin
  z := z + _SHL(P_Random - P_Random, 10);

  puff := P_SpawnMobj(x, y, z, Ord(PuffType));
  if puff.info.attacksound <> 0 then
    S_StartSound(puff, puff.info.attacksound);

  case PuffType of
    MT_BEAKPUFF,
    MT_STAFFPUFF:
      puff.momz := FRACUNIT;
    MT_GAUNTLETPUFF1,
    MT_GAUNTLETPUFF2:
      puff.momz := 52428;
  end;
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

//---------------------------------------------------------------------------
//
// PROC P_BloodSplatter
//
//---------------------------------------------------------------------------

procedure P_BloodSplatter(x, y, z: fixed_t; originator: Pmobj_t);
var
  mo: Pmobj_t;
begin
  mo := P_SpawnMobj(x, y, z, Ord(MT_BLOODSPLATTER));
  mo.target := originator;
  mo.momx := (P_Random - P_Random) * 512;
  mo.momy := (P_Random - P_Random) * 512;
  mo.momz := FRACUNIT * 2;
end;

//---------------------------------------------------------------------------
//
// PROC P_RipperBlood
//
//---------------------------------------------------------------------------

procedure P_RipperBlood(mo: Pmobj_t);
var
  th: Pmobj_t;
  x, y, z: fixed_t;
begin
  x := mo.x + (P_Random - P_Random) * $1000;
  y := mo.y + (P_Random - P_Random) * $1000;
  z := mo.z + (P_Random - P_Random) * $1000;
  th := P_SpawnMobj(x, y, z, Ord(MT_BLOOD));
  th.flags := th.flags or MF_NOGRAVITY;
  th.momx := mo.momx div 2;
  th.momy := mo.momy div 2;
  th.tics := th.tics + P_Random and 3;
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

//
// P_SpawnMissile
//

function P_SpawnMissile(source: Pmobj_t; dest: Pmobj_t; _type: integer): Pmobj_t;
var
  th: Pmobj_t;
  an: angle_t;
  dist: integer;
  z: fixed_t;
begin
  // JVAL: Prevent savegame bug
  if dest = nil then
  begin
    result := nil;
    exit;
  end;

  case _type of
    Ord(MT_MNTRFX1): // Minotaur swing attack missile
      z := source.z + 40 * FRACUNIT;
    Ord(MT_MNTRFX2): // Minotaur floor fire missile
      z := ONFLOORZ;
    Ord(MT_SRCRFX1): // Sorcerer Demon fireball
      z := source.z + 48 * FRACUNIT;
    Ord(MT_KNIGHTAXE), // Knight normal axe
    Ord(MT_REDAXE): // Knight red power axe
      z := source.z + 36 * FRACUNIT;
    else
      z := source.z + 32 * FRACUNIT;
  end;
  if source.flags2 and MF2_FEETARECLIPPED <> 0 then
    z := z - FOOTCLIPSIZE;

  th := P_SpawnMobj(source.x, source.y, z, _type);

  if th.info.seesound <> 0 then
  begin
    if th.info.flags_ex and MF_EX_RANDOMSEESOUND <> 0 then
      P_RandomSound(th, th.info.seesound)
    else
      S_StartSound(th, th.info.seesound);
  end;

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

//---------------------------------------------------------------------------
//
// FUNC P_SpawnMissileAngle
//
// Returns NULL if the missile exploded immediately, otherwise returns
// a mobj_t pointer to the missile.
//
//---------------------------------------------------------------------------

function P_SpawnMissileAngle(source: Pmobj_t; _type: integer; angle: angle_t; momz: fixed_t): Pmobj_t;
var
  th: Pmobj_t;
  z: fixed_t;
begin
  case _type of
    Ord(MT_MNTRFX1): // Minotaur swing attack missile
      z := source.z + 40 * FRACUNIT;
    Ord(MT_MNTRFX2): // Minotaur floor fire missile
      z := ONFLOORZ;
    Ord(MT_SRCRFX1): // Sorcerer Demon fireball
      z := source.z + 48 * FRACUNIT;
    Ord(MT_KNIGHTAXE), // Knight normal axe
    Ord(MT_REDAXE): // Knight red power axe
      z := source.z + 36 * FRACUNIT;
    else
      z := source.z + 32 * FRACUNIT;
  end;
  if source.flags2 and MF2_FEETARECLIPPED <> 0 then
    z := z - FOOTCLIPSIZE;

  th := P_SpawnMobj(source.x, source.y, z, _type);
  if th.info.seesound <> 0 then
  begin
    if th.info.flags_ex and MF_EX_RANDOMSEESOUND <> 0 then
      P_RandomSound(th, th.info.seesound)
    else
      S_StartSound(th, th.info.seesound);
  end;

  th.target := source;  // where it came from
  th.angle := angle;
  angle := angle shr ANGLETOFINESHIFT;
  th.momx := FixedMul(th.info.speed, finecosine[angle]);
  th.momy := FixedMul(th.info.speed, finesine[angle]);
  th.momz := momz;
  if P_CheckMissileSpawn(th) then
    result := th
  else
    result := nil;
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
    if G_PlayingEngineVersion >= VERSION115 then
      z := source.floorz
    else
      z := ONFLOORZ;
  end
  else if flags_ex and MF_EX_CEILINGHUGGER <> 0 then
  begin
    if G_PlayingEngineVersion >= VERSION115 then
      z := source.ceilingz
    else
      z := ONCEILINGZ;
  end
  else if z <> ONFLOORZ then
    z := z - source.floorz;

  th := P_SpawnMobj(x, y, z, _type);

  if th.info.seesound <> 0 then
  begin
    if th.info.flags_ex and MF_EX_RANDOMSEESOUND <> 0 then
      P_RandomSound(th, th.info.seesound)
    else
      S_StartSound(th, th.info.seesound);
  end;

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
    if G_PlayingEngineVersion >= VERSION115 then
      z := source.floorz
    else
      z := ONFLOORZ;
  end
  else if flags_ex and MF_EX_CEILINGHUGGER <> 0 then
  begin
    if G_PlayingEngineVersion >= VERSION115 then
      z := source.ceilingz
    else
      z := ONCEILINGZ;
  end
  else if z <> ONFLOORZ then
    z := z - source.floorz;

  mo := P_SpawnMobj(source.x, source.y, z, _type);

  if mo.info.seesound <> 0 then
  begin
    if mo.info.flags_ex and MF_EX_RANDOMSEESOUND <> 0 then
      P_RandomSound(mo, mo.info.seesound)
    else
      S_StartSound(mo, mo.info.seesound);
  end;

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
function P_SpawnPlayerMissile(source: Pmobj_t; _type: integer): Pmobj_t;
var
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

  x := source.x;
  y := source.y;
  // Also z axis shift calculation
  if zaxisshift then
    z := source.z + 4 * 8 * FRACUNIT + (Pplayer_t(source.player).lookdir * FRACUNIT) div 173
  else
    z := source.z + 4 * 8 * FRACUNIT;

  if source.flags2 and MF2_FEETARECLIPPED <> 0 then
    z := z - FOOTCLIPSIZE;

  MissileMobj := P_SpawnMobj(x, y, z, _type);

  if MissileMobj <> nil then
  begin
    if MissileMobj.info.seesound <> 0 then
    begin
      if MissileMobj.info.flags_ex and MF_EX_RANDOMSEESOUND <> 0 then
        P_RandomSound(MissileMobj, MissileMobj.info.seesound)
      else
        S_StartSound(MissileMobj, MissileMobj.info.seesound);
    end;

    MissileMobj.target := source;
    MissileMobj.angle := an;
    MissileMobj.momx := FixedMul(MissileMobj.info.speed, finecosine[{$IFDEF FPC}_SHRW(an, ANGLETOFINESHIFT){$ELSE}an shr ANGLETOFINESHIFT{$ENDIF}]);
    MissileMobj.momy := FixedMul(MissileMobj.info.speed, finesine[{$IFDEF FPC}_SHRW(an, ANGLETOFINESHIFT){$ELSE}an shr ANGLETOFINESHIFT{$ENDIF}]);
    MissileMobj.momz := FixedMul(MissileMobj.info.speed, slope);

    if MissileMobj._type = Ord(MT_BLASTERFX1) then
    begin // Ultra-fast ripper spawning missile
      MissileMobj.x := MissileMobj.x + (MissileMobj.momx div 8);
      MissileMobj.y := MissileMobj.y + (MissileMobj.momy div 8);
      MissileMobj.z := MissileMobj.z + (MissileMobj.momz div 8);
    end
    else
    begin // Normal missile
      MissileMobj.x := MissileMobj.x + (MissileMobj.momx div 2);
      MissileMobj.y := MissileMobj.y + (MissileMobj.momy div 2);
      MissileMobj.z := MissileMobj.z + (MissileMobj.momz div 2);
    end;
    if not P_TryMove(MissileMobj, MissileMobj.x, MissileMobj.y) then
    begin // Exploded immediately
      P_ExplodeMissile(MissileMobj);
      result := nil;
      exit;
    end;
    result := MissileMobj;
  end
  else
    result := nil;
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
// FUNC P_HitFloor
//
//---------------------------------------------------------------------------
function P_HitFloor(thing: Pmobj_t): integer;
var
  mo: Pmobj_t;
  z: fixed_t; // JVAL: 3d Floors
  ss: Psubsector_t;
  sec: Psector_t;
begin
  // don't splash if has MF2_EX_NOHITFLOOR flag
  if thing.flags2_ex and MF2_EX_NOHITFLOOR <> 0 then
  begin
    result := FLOOR_SOLID;
    exit;
  end;

  ss := thing.subsector;
  sec := ss.sector;

  if thing.floorz <> sec.floorheight then
  begin // don't splash if landing on the edge above water/lava/etc....
    result := FLOOR_SOLID;
    exit;
  end;

  if ss.flags and SSF_BRIDGE <> 0 then
    if G_PlayingEngineVersion >= VERSION204 then
    begin
      result := FLOOR_SOLID;
      exit;
    end;

  // JVAL: 3d Floors
  if G_PlayingEngineVersion >= VERSION115 then
    z := thing.floorz
  else
    z := ONFLOORZ;
  case P_GetThingFloorType(thing) of
    FLOOR_WATER:
      begin
        P_SpawnMobj(thing.x, thing.y, z, Ord(MT_SPLASHBASE));
        mo := P_SpawnMobj(thing.x, thing.y, z, Ord(MT_SPLASH));
        mo.target := thing;
        mo.momx := (P_Random - P_Random) * 256;
        mo.momy := (P_Random - P_Random) * 256;
        mo.momz := 2 * FRACUNIT + (P_Random * 256);
        S_StartSound(mo, Ord(sfx_gloop));
        result := FLOOR_WATER;
        exit;
      end;
    FLOOR_LAVA:
      begin
        P_SpawnMobj(thing.x, thing.y, z, Ord(MT_LAVASPLASH));
        mo := P_SpawnMobj(thing.x, thing.y, z, Ord(MT_LAVASMOKE));
        mo.momz := FRACUNIT + (P_Random * 128);
        S_StartSound(mo, Ord(sfx_burn));
        result := FLOOR_LAVA;
        exit;
      end;
    FLOOR_SLUDGE:
      begin
        P_SpawnMobj(thing.x, thing.y, z, Ord(MT_SLUDGESPLASH));
        mo := P_SpawnMobj(thing.x, thing.y, z, Ord(MT_SLUDGECHUNK));
        mo.target := thing;
        mo.momx := (P_Random - P_Random) * 256;
        mo.momy := (P_Random - P_Random) * 256;
        mo.momz := FRACUNIT + (P_Random * 256);
        result := FLOOR_SLUDGE;
        exit;
      end;
  end;
  result := FLOOR_SOLID;
end;

//---------------------------------------------------------------------------
//
// FUNC P_GetThingFloorType
//
//---------------------------------------------------------------------------

function P_GetThingFloorType(thing: Pmobj_t): integer;
begin
  result := flats[Psubsector_t(thing.subsector).sector.floorpic].terraintype;
end;

//---------------------------------------------------------------------------
//
// PROC P_SPMAngle
//
//---------------------------------------------------------------------------

function P_SPMAngle(source: Pmobj_t; _type: integer; angle: angle_t): Pmobj_t;
var
  th: Pmobj_t;
  an: angle_t;
  x, y, z, slope: fixed_t;
begin
//
// see which target is to be aimed at
//
  an := angle;
  slope := P_AimLineAttack(source, an, 16 * 64 * FRACUNIT);
  if linetarget = nil then
  begin
    an := an + (1 shl 26);
    slope := P_AimLineAttack (source, an, 16 * 64 * FRACUNIT);
    if linetarget = nil then
    begin
      an := an - (2 shl 26);
      slope := P_AimLineAttack (source, an, 16 * 64 * FRACUNIT);
    end;
    if linetarget = nil then
    begin
      an := angle;
      slope := (Pplayer_t(source.player).lookdir * FRACUNIT) div 173;
    end
  end;
  x := source.x;
  y := source.y;
  z := source.z + 4 * 8 * FRACUNIT + (Pplayer_t(source.player).lookdir * FRACUNIT) div 173;
  if source.flags2 and MF2_FEETARECLIPPED <> 0 then
    z := z - FOOTCLIPSIZE;

  th := P_SpawnMobj(x, y, z, _type);
  if th.info.seesound <> 0 then
    S_StartSound(th, th.info.seesound);

  th.target := source;
  th.angle := an;
  th.momx := FixedMul(th.info.speed, finecosine[an shr ANGLETOFINESHIFT]);
  th.momy := FixedMul(th.info.speed, finesine[an shr ANGLETOFINESHIFT]);
  th.momz := FixedMul(th.info.speed, slope);
  if P_CheckMissileSpawn(th) then
    result := th
  else
    result := nil;
end;

procedure P_ThrustMobj(mo: Pmobj_t; angle: angle_t; const move: fixed_t);
begin
  angle := angle shr ANGLETOFINESHIFT;

  mo.momx := mo.momx + FixedMul(move, finecosine[angle]);
  mo.momy := mo.momy + FixedMul(move, finesine[angle]);
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
  else if mobjinfo[mobjno].flags2 and MF2_SPAWNFLOAT <> 0 then
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

