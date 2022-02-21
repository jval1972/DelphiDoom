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
//  Enemy thinking, AI.
//  Action Pointer Functions
//  that are associated with states/frames.
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit p_enemy;

interface

uses
  doomdef,
  p_local,
  p_mobj_h,
  s_sound,
  d_player,
  p_pspr_h,
// State.
  doomstat,
// Data.
  sounddata;

//==============================================================================
//
// A_Fall
//
//==============================================================================
procedure A_Fall(actor: Pmobj_t);

//==============================================================================
//
// A_KeenDie
//
//==============================================================================
procedure A_KeenDie(mo: Pmobj_t);

//==============================================================================
//
// A_Look
//
//==============================================================================
procedure A_Look(actor: Pmobj_t);

//==============================================================================
//
// A_Chase
//
//==============================================================================
procedure A_Chase(actor: Pmobj_t);

//==============================================================================
//
// A_FaceTarget
//
//==============================================================================
procedure A_FaceTarget(actor: Pmobj_t);

//==============================================================================
//
// A_PosAttack
//
//==============================================================================
procedure A_PosAttack(actor: Pmobj_t);

//==============================================================================
//
// A_SPosAttack
//
//==============================================================================
procedure A_SPosAttack(actor: Pmobj_t);

//==============================================================================
//
// A_CPosAttack
//
//==============================================================================
procedure A_CPosAttack(actor: Pmobj_t);

//==============================================================================
//
// A_CPosRefire
//
//==============================================================================
procedure A_CPosRefire(actor: Pmobj_t);

//==============================================================================
//
// A_SpidRefire
//
//==============================================================================
procedure A_SpidRefire(actor: Pmobj_t);

//==============================================================================
//
// A_BspiAttack
//
//==============================================================================
procedure A_BspiAttack(actor: Pmobj_t);

//==============================================================================
//
// A_TroopAttack
//
//==============================================================================
procedure A_TroopAttack(actor: Pmobj_t);

//==============================================================================
//
// A_SargAttack
//
//==============================================================================
procedure A_SargAttack(actor: Pmobj_t);

//==============================================================================
//
// A_HeadAttack
//
//==============================================================================
procedure A_HeadAttack(actor: Pmobj_t);

//==============================================================================
//
// A_CyberAttack
//
//==============================================================================
procedure A_CyberAttack(actor: Pmobj_t);

//==============================================================================
//
// A_BruisAttack
//
//==============================================================================
procedure A_BruisAttack(actor: Pmobj_t);

//==============================================================================
//
// A_SkelMissile
//
//==============================================================================
procedure A_SkelMissile(actor: Pmobj_t);

//==============================================================================
//
// A_Tracer
//
//==============================================================================
procedure A_Tracer(actor: Pmobj_t);

//==============================================================================
//
// A_SkelWhoosh
//
//==============================================================================
procedure A_SkelWhoosh(actor: Pmobj_t);

//==============================================================================
//
// A_SkelFist
//
//==============================================================================
procedure A_SkelFist(actor: Pmobj_t);

//==============================================================================
//
// A_VileChase
//
//==============================================================================
procedure A_VileChase(actor: Pmobj_t);

//==============================================================================
//
// A_VileStart
//
//==============================================================================
procedure A_VileStart(actor: Pmobj_t);

//==============================================================================
//
// A_Fire
//
//==============================================================================
procedure A_Fire(actor: Pmobj_t);

//==============================================================================
//
// A_StartFire
//
//==============================================================================
procedure A_StartFire(actor: Pmobj_t);

//==============================================================================
//
// A_FireCrackle
//
//==============================================================================
procedure A_FireCrackle(actor: Pmobj_t);

//==============================================================================
//
// A_VileTarget
//
//==============================================================================
procedure A_VileTarget(actor: Pmobj_t);

//==============================================================================
//
// A_VileAttack
//
//==============================================================================
procedure A_VileAttack(actor: Pmobj_t);

//==============================================================================
//
// A_FatRaise
//
//==============================================================================
procedure A_FatRaise(actor: Pmobj_t);

//==============================================================================
//
// A_FatAttack1
//
//==============================================================================
procedure A_FatAttack1(actor: Pmobj_t);

//==============================================================================
//
// A_FatAttack2
//
//==============================================================================
procedure A_FatAttack2(actor: Pmobj_t);

//==============================================================================
//
// A_FatAttack3
//
//==============================================================================
procedure A_FatAttack3(actor: Pmobj_t);

//==============================================================================
//
// A_SkullAttack
//
//==============================================================================
procedure A_SkullAttack(actor: Pmobj_t);

//==============================================================================
//
// A_PainAttack
//
//==============================================================================
procedure A_PainAttack(actor: Pmobj_t);

//==============================================================================
//
// A_PainDie
//
//==============================================================================
procedure A_PainDie(actor: Pmobj_t);

//==============================================================================
//
// A_Scream
//
//==============================================================================
procedure A_Scream(actor: Pmobj_t);

//==============================================================================
//
// A_XScream
//
//==============================================================================
procedure A_XScream(actor: Pmobj_t);

//==============================================================================
//
// A_Pain
//
//==============================================================================
procedure A_Pain(actor: Pmobj_t);

//==============================================================================
//
// A_Explode
//
//==============================================================================
procedure A_Explode(thingy: Pmobj_t);

//==============================================================================
//
// A_BossDeath
//
//==============================================================================
procedure A_BossDeath(mo: Pmobj_t);

//==============================================================================
//
// A_Hoof
//
//==============================================================================
procedure A_Hoof(mo: Pmobj_t);

//==============================================================================
//
// A_Metal
//
//==============================================================================
procedure A_Metal(mo: Pmobj_t);

//==============================================================================
//
// A_BabyMetal
//
//==============================================================================
procedure A_BabyMetal(mo: Pmobj_t);

//==============================================================================
//
// A_OpenShotgun2
//
//==============================================================================
procedure A_OpenShotgun2(player: Pplayer_t; psp: Pplayer_t);

//==============================================================================
//
// A_LoadShotgun2
//
//==============================================================================
procedure A_LoadShotgun2(player: Pplayer_t; psp: Ppspdef_t);

//==============================================================================
//
// A_CloseShotgun2
//
//==============================================================================
procedure A_CloseShotgun2(player: Pplayer_t; psp: Ppspdef_t);

//==============================================================================
//
// A_BrainAwakeVanilla
//
//==============================================================================
procedure A_BrainAwakeVanilla(mo: Pmobj_t);

//==============================================================================
//
// A_BrainAwake
//
//==============================================================================
procedure A_BrainAwake(mo: Pmobj_t);

//==============================================================================
//
// A_BrainPain
//
//==============================================================================
procedure A_BrainPain(mo: Pmobj_t);

//==============================================================================
//
// A_BrainScream
//
//==============================================================================
procedure A_BrainScream(mo: Pmobj_t);

//==============================================================================
//
// A_BrainExplode
//
//==============================================================================
procedure A_BrainExplode(mo: Pmobj_t);

//==============================================================================
//
// A_BrainDie
//
//==============================================================================
procedure A_BrainDie(mo: Pmobj_t);

//==============================================================================
//
// A_BrainSpitVanilla
//
//==============================================================================
procedure A_BrainSpitVanilla(mo: Pmobj_t);

//==============================================================================
//
// A_BrainSpit
//
//==============================================================================
procedure A_BrainSpit(mo: Pmobj_t);

//==============================================================================
//
// A_SpawnFly
//
//==============================================================================
procedure A_SpawnFly(mo: Pmobj_t);

//==============================================================================
//
// A_SpawnSound
//
//==============================================================================
procedure A_SpawnSound(mo: Pmobj_t);

//==============================================================================
//
// A_PlayerScream
//
//==============================================================================
procedure A_PlayerScream(mo: Pmobj_t);

//==============================================================================
//
// P_DoChase
//
//==============================================================================
procedure P_DoChase(actor: Pmobj_t; const fast: boolean);

//==============================================================================
//
// P_NoiseAlert
//
//==============================================================================
procedure P_NoiseAlert(target: Pmobj_t; emmiter: Pmobj_t);

//==============================================================================
//
// P_CheckMeleeRange
//
//==============================================================================
function P_CheckMeleeRange(actor: Pmobj_t; const factor: Integer = 1): boolean;

//==============================================================================
//
// P_TryWalk
//
//==============================================================================
function P_TryWalk(actor: Pmobj_t): boolean;

//==============================================================================
//
// P_Move
//
//==============================================================================
function P_Move(actor: Pmobj_t): boolean;

//==============================================================================
//
// P_Massacre
//
//==============================================================================
procedure P_Massacre;

//==============================================================================
//
// A_TurretChase
//
//==============================================================================
procedure A_TurretChase(actor: Pmobj_t);

type
  dirtype_t = (
    DI_EAST,
    DI_NORTHEAST,
    DI_NORTH,
    DI_NORTHWEST,
    DI_WEST,
    DI_SOUTHWEST,
    DI_SOUTH,
    DI_SOUTHEAST,
    DI_NODIR,
    NUMDIRS
  );

var
  continueafterplayerdeath: boolean;

implementation

uses
  d_delphi,
  doomdata,
  d_think,
  d_main,
  d_items,
  g_game,
  m_fixed,
  tables,
  i_system,
  info_h,
  info,
  info_common,
  m_rnd,
  p_playertrace,
  p_common,
  p_floor,
  p_friends,
  p_map,
  p_maputl,
  p_setup,
  p_sight,
  p_switch,
  p_tick,
  p_mobj,
  p_doors,
  p_spec,
  p_inter,
  p_pspr,
  p_sounds,
  p_udmf,
  udmf_spec,
  ps_main,
  psi_globals,
  r_defs,
  r_main;

const
  opposite: array[0..8] of dirtype_t = (
    DI_WEST, DI_SOUTHWEST, DI_SOUTH, DI_SOUTHEAST,
    DI_EAST, DI_NORTHEAST, DI_NORTH, DI_NORTHWEST, DI_NODIR
  );

  diags: array[0..3] of dirtype_t = (
    DI_NORTHWEST, DI_NORTHEAST, DI_SOUTHWEST, DI_SOUTHEAST
  );

//==============================================================================
//
// A_Fall
//
//==============================================================================
procedure A_Fall(actor: Pmobj_t);
begin
  // actor is on ground, it can be walked over
  actor.flags := actor.flags and not MF_SOLID;

  // So change this if corpse objects
  // are meant to be obstacles.
end;

//
// ENEMY THINKING
// Enemies are allways spawned
// with targetplayer = -1, threshold = 0
// Most monsters are spawned unaware of all players,
// but some can be made preaware
//

//
// Called by P_NoiseAlert.
// Recursively traverse adjacent sectors,
// sound blocking lines cut off traversal.
//

var
  soundtarget: Pmobj_t;

//==============================================================================
//
// P_RecursiveSound
//
//==============================================================================
procedure P_RecursiveSound(sec: Psector_t; soundblocks: integer);
var
  i: integer;
  check: Pline_t;
  other: Psector_t;
begin
  // wake up all monsters in this sector
  if (sec.validcount = validcount) and
     (sec.soundtraversed <= soundblocks + 1) then
    exit; // already flooded

  sec.validcount := validcount;
  sec.soundtraversed := soundblocks + 1;
  sec.soundtarget := soundtarget;

  for i := 0 to sec.linecount - 1 do
  begin
    check := sec.lines[i];
    if check.flags and ML_TWOSIDED = 0 then
      continue;
    // JVAL: 20200407 - Avoid false ML_TWOSIDED flag
    if (check.sidenum[1] < 0) or (check.sidenum[0] < 0) then
      continue;

    P_LineOpening(check, false);

    if openrange <= 0 then
      continue; // closed door

    if sides[check.sidenum[0]].sector = sec then
      other := sides[check.sidenum[1]].sector
    else
      other := sides[check.sidenum[0]].sector;

    if check.flags and ML_SOUNDBLOCK <> 0 then
    begin
      if soundblocks = 0 then
        P_RecursiveSound(other, 1);
    end
    else
      P_RecursiveSound(other, soundblocks);
  end;
end;

//==============================================================================
//
// P_NoiseAlert
// If a monster yells at a player,
// it will alert other monsters to the player.
//
//==============================================================================
procedure P_NoiseAlert(target: Pmobj_t; emmiter: Pmobj_t);
begin
  soundtarget := target;
  inc(validcount);
  P_RecursiveSound(Psubsector_t(emmiter.subsector).sector, 0);
end;

//==============================================================================
//
// P_MeleeRange
//
//==============================================================================
function P_MeleeRange(actor: Pmobj_t): fixed_t;
begin
  result := actor.info.meleerange;
  if result = 0 then
    result := MELEERANGE
  else if result < FRACUNIT then
    result := result * FRACUNIT;
end;

//==============================================================================
//
// P_CheckMeleeRange
//
//==============================================================================
function P_CheckMeleeRange(actor: Pmobj_t; const factor: Integer = 1): boolean;
var
  mo: Pmobj_t;
  dist: fixed_t;
  mrange: integer;
begin
  mo := actor.target;
  if mo = nil then
  begin
    result := false;
    exit;
  end;

  // Friendly monsters do not attack each other
  if P_BothFriends(mo, actor) then
  begin
    result := false;
    exit;
  end;

  dist := P_AproxDistance(mo.x - actor.x, mo.y - actor.y);

  mrange := P_MeleeRange(actor) * factor;

  if dist >= mrange - 20 * FRACUNIT + mo.info.radius then
  begin
    result := false;
    exit;
  end;

  // JVAL: 20210207 - Added MF3_EX_MELEECHECKZ
  if actor.flags3_ex and MF3_EX_MELEECHECKZ <> 0 then
  begin
    if mo.z > actor.z + actor.height then
    begin // Target is higher than the attacker
      result := false;
      exit;
    end;

    if actor.z > mo.z + mo.height then
    begin // Attacker is higher
      result := false;
      exit;
    end;
  end;

  result := P_CheckSight(actor, actor.target);
end;

//==============================================================================
//
// P_CheckMissileRange
//
//==============================================================================
function P_CheckMissileRange(actor: Pmobj_t): boolean;
var
  dist: fixed_t;
begin
  if not P_CheckSight(actor, actor.target) then
  begin
    result := false;
    exit;
  end;

  if actor.flags and MF_JUSTHIT <> 0 then
  begin
    // The target just hit the enemy,
    // so fight back!
    actor.flags := actor.flags and not MF_JUSTHIT;
    result := true;
    exit;
  end;

  if actor.reactiontime <> 0 then
  begin
    result := false; // Don't attack yet
    exit;
  end;

  // Friendly monsters do not attack each other
  if P_BothFriends(actor, actor.target) then
  begin
    result := false;
    exit;
  end;

  // OPTIMIZE: get this from a global checksight
  dist := P_AproxDistance(actor.x - actor.target.x, actor.y - actor.target.y) -
            64 * FRACUNIT;

  if actor.info.meleestate = 0 then
    dist := dist - 128 * FRACUNIT;  // no melee attack, so fire more

  dist := FixedInt(dist);

  if actor.flags4_ex and MF4_EX_SHORTMRANGE <> 0 then  // MT_VILE
    if dist > 14 * 64 then
    begin
      result := false; // too far away
      exit;
    end;

  if actor.info.meleethreshold > 0 then
    if dist < actor.info.meleethreshold then
    begin
      result := false; // close for fist attack
      exit;
    end;

  if actor.flags4_ex and MF4_EX_LONGMELEERANGE <> 0 then
    if dist < 196 then
    begin
      result := false; // close for fist attack
      exit;
    end;

  if actor.flags4_ex and MF4_EX_RANGEHALF <> 0 then
    dist := _SHR1(dist);

  if dist > 200 then
    dist := 200;

  if (actor.flags4_ex and MF4_EX_HIGHERMPROB <> 0) and (dist > 160) then
    dist := 160;

  if actor.flags3_ex and MF3_EX_MISSILEMORE <> 0 then
    dist := dist div 2;
  if actor.flags3_ex and MF3_EX_MISSILEEVENMORE <> 0 then
    dist := dist div 8;

  if actor.info.minmissilechance > 0 then
    if actor.info.minmissilechance < dist then
      dist := actor.info.minmissilechance;

  if P_Random < dist then
    result := false
  else
    result := true;
end;

//==============================================================================
//
// P_Move
// Move in the current direction,
// returns false if the move is blocked.
//
//==============================================================================
function P_Move(actor: Pmobj_t): boolean;
var
  tryx: fixed_t;
  tryy: fixed_t;
  ld: Pline_t;
  try_ok: boolean;
  ret1, ret2: boolean;
begin
  if actor.movedir = Ord(DI_NODIR) then
  begin
    result := false;
    exit;
  end;

  if Ord(actor.movedir) >= 8 then
    I_Error('P_Move(): Weird actor->movedir = %d', [Ord(actor.movedir)]);

  tryx := actor.x + actor.info.speed * xspeed[actor.movedir];
  tryy := actor.y + actor.info.speed * yspeed[actor.movedir];

  try_ok := P_TryMove(actor, tryx, tryy);

  if not try_ok then
  begin
    // open any specials
    if (actor.flags and MF_FLOAT <> 0) and floatok then
    begin
      // must adjust height
      if actor.z < tmfloorz then
        actor.z := actor.z + P_FloatSpeed(actor)
      else
        actor.z := actor.z - P_FloatSpeed(actor);

      actor.flags := actor.flags or MF_INFLOAT;
      result := true;
      exit;
    end;

    if numspechit = 0 then
    begin
      result := false;
      exit;
    end;

    actor.movedir := Ord(DI_NODIR);
    result := false;
    while numspechit > 0 do
    begin
      dec(numspechit);
      ld := spechit[numspechit];

      if ld.flags and ML_TRIGGERSCRIPTS <> 0 then
        if actor.flags2_ex and MF2_EX_DONTRUNSCRIPTS = 0 then
          PS_EventUseLine(actor, pDiff(ld, lines, SizeOf(line_t)), P_PointOnLineSide(actor.x, actor.y, ld));

      // if the special is not a door
      // that can be opened,
      // return false
      ret1 := P_ActivateLine(ld, actor, 0, ULAC_USE);
      ret2 := P_UseSpecialLine(actor, ld, 0);
      result := ret1 or ret2;
    end;
    exit;
  end
  else
    actor.flags := actor.flags and not MF_INFLOAT;

  if actor.flags and MF_FLOAT = 0 then
  begin
    if actor.z > actor.floorz then
      P_HitFloor(actor);
    actor.z := actor.floorz;
  end;

  result := true;
end;

//==============================================================================
// P_TryWalk
//
// TryWalk
// Attempts to move actor on
// in its current (ob->moveangle) direction.
// If blocked by either a wall or an actor
// returns FALSE
// If move is either clear or blocked only by a door,
// returns TRUE and sets...
// If a door is in the way,
// an OpenDoor call is made to start it opening.
//
//==============================================================================
function P_TryWalk(actor: Pmobj_t): boolean;
begin
  if not P_Move(actor) then
    result := false
  else
  begin
    actor.movecount := P_Random and 15;
    result := true;
  end;
end;

//==============================================================================
//
// P_NewChaseDir
//
//==============================================================================
procedure P_NewChaseDir(actor: Pmobj_t);
var
  deltax: fixed_t;
  deltay: fixed_t;
  target: Pmobj_t;
  d: array[0..2] of dirtype_t;
  olddir: dirtype_t;
  turnaround: dirtype_t;
  idx: integer;
  dist: fixed_t;

  procedure _DoNewChaseDir;
  var
    tdir: dirtype_t;
  begin
    if deltax > 10 * FRACUNIT then
      d[1] := DI_EAST
    else if deltax < -10 * FRACUNIT then
      d[1] := DI_WEST
    else
      d[1] := DI_NODIR;

    if deltay < -10 * FRACUNIT then
      d[2] := DI_SOUTH
    else if deltay > 10 * FRACUNIT then
      d[2] := DI_NORTH
    else
      d[2] := DI_NODIR;

    // try direct route
    if (d[1] <> DI_NODIR) and (d[2] <> DI_NODIR) then
    begin
      if deltay < 0 then
        idx := 2
      else
        idx := 0;
      if deltax > 0 then
        inc(idx);
      actor.movedir := Ord(diags[idx]);
      if (actor.movedir <> Ord(turnaround)) and P_TryWalk(actor) then
        exit;
    end;

    // try other directions
    if (P_Random > 200) or (abs(deltay) > abs(deltax)) then
    begin
      tdir := d[1];
      d[1] := d[2];
      d[2] := tdir;
    end;

    if d[1] = turnaround then
      d[1] := DI_NODIR;
    if d[2] = turnaround then
      d[2] := DI_NODIR;

    if d[1] <> DI_NODIR then
    begin
      actor.movedir := Ord(d[1]);
      if P_TryWalk(actor) then
        exit; // either moved forward or attacked
    end;

    if d[2] <> DI_NODIR then
    begin
      actor.movedir := Ord(d[2]);
      if P_TryWalk(actor) then
        exit;
    end;

    // there is no direct path to the player,
    // so pick another direction.
    if olddir <> DI_NODIR then
    begin
      actor.movedir := Ord(olddir);
      if P_TryWalk(actor) then
        exit;
    end;

    // randomly determine direction of search
    if P_Random and 1 <> 0 then
    begin
      for tdir := DI_EAST to DI_SOUTHEAST do
      begin
        if tdir <> turnaround then
        begin
          actor.movedir := Ord(tdir);
          if P_TryWalk(actor) then
            exit;
        end;
      end;
    end
    else
    begin
      for tdir := DI_SOUTHEAST downto DI_EAST do
      begin
        if tdir <> turnaround then
        begin
          actor.movedir := Ord(tdir);
          if P_TryWalk(actor) then
            exit;
        end;
      end;
    end;

    if turnaround <> DI_NODIR then
    begin
      actor.movedir := Ord(turnaround);
      if P_TryWalk(actor) then
        exit;
    end;

    actor.movedir := Ord(DI_NODIR); // can not move
  end;

begin
  target := actor.target;
  if target = nil then
    I_Error('P_NewChaseDir(): called with no target');

  olddir := dirtype_t(actor.movedir);
  turnaround := opposite[Ord(olddir)];

  deltax := target.x - actor.x;
  deltay := target.y - actor.y;

  actor.strafecount := 0;

  // JVAL: 20210209 - MF3_EX_CAUSEFEAR & MF3_EX_NOFEAR flags
  if target.flags3_ex and MF3_EX_CAUSEFEAR <> 0 then
    if actor.flags3_ex and MF3_EX_NOFEAR <> 0 then
      actor.flags2_ex := actor.flags2_ex or MF2_EX_FRIGHTENED;

  if actor.flags2_ex and MF2_EX_FRIGHTENED <> 0 then
  begin
    deltax := -deltax;
    deltay := -deltay;
  end
  else if (target.health > 0) and (actor.flags4_ex and MF4_EX_BACKINGMELEE <> 0) and not P_BothFriends(actor, actor.target) then
  begin
    if G_PlayingEngineVersion >= VERSION207 then
    begin
      dist := P_AproxDistance(deltax, deltay);
      if (actor.info.missilestate <> 0) and
         ((actor._type <> Ord(MT_SKULL)) or (Info_GetInheritance(actor.info) <> Ord(MT_SKULL))) and
        (((target.info.missilestate = 0) and (dist < P_MeleeRange(target) * 2)) or
         ((target.player <> nil) and (dist < P_MeleeRange(target) * 3) and
          (weaponinfo[Ord(Pplayer_t(target.player).readyweapon)].mbf21bits and WPF_FLEEMELEE <> 0))) then
      begin// Back away from melee attacker
        actor.strafecount := P_Random and 15;
        deltax := -deltax;
        deltay := -deltay;
      end;
    end;
  end;

  _DoNewChaseDir;

  if actor.strafecount > 0 then
    actor.movecount := actor.strafecount;
end;

//==============================================================================
//
// P_LookForPlayers
// If allaround is false, only look 180 degrees in front.
// Returns true if a player is targeted.
//
//==============================================================================
function P_LookForPlayers(actor: Pmobj_t; allaround: boolean): boolean;
var
  c: integer;
  stop: integer;
  player: Pplayer_t;
  an: angle_t;
  dist: fixed_t;
  initial: boolean;
begin
  if continueafterplayerdeath then
    if not netgame and (players[0].health <= 0) then
    begin
      result := P_LookForMonsters(actor);
      exit;
    end;

  c := 0;
  stop := (actor.lastlook - 1) and 3;

  initial := true;
  while true do
  begin
    if initial then
      initial := false
    else
      actor.lastlook := (actor.lastlook + 1) and 3;

    if not playeringame[actor.lastlook] then
      continue;

    if (c = 2) or (actor.lastlook = stop) then
    begin
      // done looking
      result := false;
      exit;
    end;
    inc(c);

    player := @players[actor.lastlook];

    if player.health <= 0 then
      continue;   // dead

    if actor.info.maxtargetrange > 0 then
      if P_AproxDistance(actor.x - player.mo.x, actor.y - player.mo.y) > actor.info.maxtargetrange * FRACUNIT then
        continue; // JVAL: 20210211 - Out of range

    if not P_CheckSight(actor, player.mo) then
      continue;   // out of sight

    if not allaround then
    begin
      an := R_PointToAngle2(actor.x, actor.y, player.mo.x, player.mo.y) - actor.angle;

      if (an > ANG90) and (an < ANG270) then
      begin
        dist := P_AproxDistance(player.mo.x - actor.x, player.mo.y - actor.y);
        // if real close, react anyway
        if dist > MELEERANGE then
          continue; // behind back
      end;
    end;

    actor.target := player.mo;
    result := true;
    exit;
  end;

  result := false;
end;

//==============================================================================
//
// P_LookForTargets
//
//==============================================================================
function P_LookForTargets(actor: Pmobj_t; allaround: boolean): boolean;
begin
  if actor.flags2_ex and MF2_EX_FRIEND <> 0 then
  begin
    result := P_LookForMonsters(actor);
    if not result then
      if P_Random < 200 then
        result := P_LookForPlayers(actor, true);
  end
  else
    result := P_LookForPlayers(actor, allaround);
end;

//==============================================================================
//
// A_KeenDie
// DOOM II special, map 32.
// Uses special tag 666.
//
//==============================================================================
procedure A_KeenDie(mo: Pmobj_t);
var
  th: Pthinker_t;
  mo2: Pmobj_t;
  junk: line_t;
begin
  A_Fall(mo);

  // scan the remaining thinkers
  // to see if all Keens are dead

  th := thinkercap.next;
  while th <> @thinkercap do
  begin
    if @th._function.acp1 = @P_MobjThinker then
    begin
      mo2 := Pmobj_t(th);
      if (mo2 <> mo) and (mo2._type = mo._type) and (mo2.health > 0) then
        exit; // other Keen not dead
    end;
    th := th.next;
  end;

  junk.tag := 666;
  EV_DoDoor(@junk, open);
end;

//==============================================================================
// P_DogFollowPlayerHandler
//
// ACTION ROUTINES
//
//==============================================================================
function P_DogFollowPlayerHandler(actor: Pmobj_t): boolean;
begin
  // JVAL: 20200512 - Drones follow player 's trace
  if actor.info.doomednum = 888 then
    if leveltime > actor.playerfollowtime then
      if actor.flags2_ex and MF2_EX_FRIEND <> 0 then
      begin
        if P_FollowPlayer(actor, P_NearestPlayer(actor)) then
        begin
          result := true;
          exit;
        end;
        if P_Random < 24 then
        begin
          actor.target := nil;
          A_Wander(actor);
          result := true;
          exit;
        end;
      end;
  result := false;
end;

//==============================================================================
//
// A_Look
// Stay in state until a player is sighted.
//
//==============================================================================
procedure A_Look(actor: Pmobj_t);
var
  targ: Pmobj_t;
  seeyou: boolean;
  sound: integer;
begin
  if P_DogFollowPlayerHandler(actor) then
    exit;

  actor.threshold := 0; // any shot will wake up
  targ := Psubsector_t(actor.subsector).sector.soundtarget;
  seeyou := false;
  if (targ <> nil) and (targ.flags and MF_SHOOTABLE <> 0) then
  begin
    actor.target := targ;

    if actor.flags and MF_AMBUSH <> 0 then
    begin
      if P_CheckSight(actor, targ) then
        seeyou := true;
    end
    else
      seeyou := true;
  end;

  if not seeyou then
  begin
    if not P_LookForTargets(actor, actor.flags_ex and MF_EX_LOOKALLAROUND <> 0) then
      exit;
  end;

  // go into chase state
  if actor.info.seesound <> 0 then
  begin
    case sfxenum_t(actor.info.seesound) of
      sfx_posit1,
      sfx_posit2,
      sfx_posit3:
        sound := Ord(sfx_posit1) + P_Random mod 3;

      sfx_bgsit1,
      sfx_bgsit2:
        sound := Ord(sfx_bgsit1) + P_Random mod 2;
    else
      sound := actor.info.seesound;
    end;

    if actor.flags_ex and MF_EX_RANDOMSEESOUND <> 0 then
    begin
      if (actor._type = Ord(MT_SPIDER)) or (actor._type = Ord(MT_CYBORG)) or (actor.flags_ex and MF_EX_BOSS <> 0) then
        // full volume
        P_RandomSound(nil, sound)
      else
        P_RandomSound(actor, sound)
    end
    else
    begin
      if (actor._type = Ord(MT_SPIDER)) or (actor._type = Ord(MT_CYBORG)) or (actor.flags_ex and MF_EX_BOSS <> 0) then
        // full volume
        S_StartSound(nil, sound)
      else
        S_StartSound(actor, sound);
    end;
  end;

  P_SetMobjState(actor, statenum_t(actor.info.seestate));
end;

//==============================================================================
// P_DoChase
//
// A_Chase
// Actor has a melee attack,
// so it tries to close as fast as possible
//
//==============================================================================
procedure P_DoChase(actor: Pmobj_t; const fast: boolean);
var
  delta: integer;
  nomissile: boolean;
  dist: fixed_t;
  ang: angle_t;
begin
  if actor.reactiontime <> 0 then
    actor.reactiontime := actor.reactiontime - 1;

  if P_DogFollowPlayerHandler(actor) then
    exit;

  // modify target threshold
  if actor.threshold <> 0 then
  begin
    if (actor.target = nil) or (actor.target.health <= 0) then
      actor.threshold := 0
    else
      actor.threshold := actor.threshold - 1;
  end;

  // turn towards movement direction if not there yet
  // killough 9/7/98: keep facing towards target if strafing or backing out
  if actor.strafecount > 0 then
    A_FaceTarget(actor)
  else if actor.movedir < 8 then
  begin
    actor.angle := actor.angle and $E0000000;
    delta := actor.angle - _SHLW(actor.movedir, 29);

    if delta > 0 then
      actor.angle := actor.angle - ANG90 div 2
    else if delta < 0 then
      actor.angle := actor.angle + ANG90 div 2;
  end;

  if (actor.target = nil) or
     (actor.target.flags and MF_SHOOTABLE = 0) then
  begin
    // look for a new target
    if P_LookForTargets(actor, true) then
      exit; // got a new target

    if actor.state <> @states[actor.info.spawnstate] then
      P_SetMobjState(actor, statenum_t(actor.info.spawnstate));
    exit;
  end;

  // do not attack twice in a row
  if actor.flags and MF_JUSTATTACKED <> 0 then
  begin
    actor.flags := actor.flags and not MF_JUSTATTACKED;
    if (gameskill <> sk_nightmare) and not fastparm then
      P_NewChaseDir(actor);
    exit;
  end;

  // check for melee attack
  if (actor.info.meleestate <> 0) and P_CheckMeleeRange(actor) then
  begin
    A_AttackSound(actor, actor);
    P_SetMobjState(actor, statenum_t(actor.info.meleestate));
    exit;
  end;

  // check for missile attack
  if actor.info.missilestate <> 0 then
  begin
    nomissile := false;
    if (gameskill < sk_nightmare) and not fastparm and (actor.movecount <> 0) then
      nomissile := true
    else if not P_CheckMissileRange(actor) then
      nomissile := true;
    if not nomissile then
    begin
      P_SetMobjState(actor, statenum_t(actor.info.missilestate));
      actor.flags := actor.flags or MF_JUSTATTACKED;
      exit;
    end;
  end;

  // possibly choose another target
  if netgame and
    (actor.threshold = 0) and
    not P_CheckSight(actor, actor.target) then
  begin
    if P_LookForTargets(actor, true) then
      exit;  // got a new target
  end;

  // chase towards player
  actor.movecount := actor.movecount - 1;
  if (actor.movecount < 0) or not P_Move(actor) then
    P_NewChaseDir(actor);

  if fast then
  begin
    if actor.fastchasetics > 0 then
      dec(actor.fastchasetics)
    else
    begin
      actor.fastchasetics := 0;
      actor.momx := 0;
      actor.momy := 0;
      dist := P_AproxDistance(actor.x - actor.target.x, actor.y - actor.target.y);
      if dist < 64 * 10 * FRACUNIT then
        if P_Random < 100 then
        begin
          ang := R_PointToAngle2(actor.x, actor.y, actor.target.x, actor.target.y);
          if P_Random < 128 then
            ang := ang + ANG90
          else
            ang := ang - ANG90;
          ang := ang shr ANGLETOFINESHIFT;
          actor.momx := 2 * actor.info.speed * finecosine[ang];
          actor.momy := 2 * actor.info.speed * finesine[ang];
          actor.fastchasetics := 3;
        end;
    end;
  end;

  if actor.strafecount > 0 then
    Dec(actor.strafecount);

  // make active sound
  if (actor.info.activesound <> 0) and (P_Random < 3) then
    A_ActiveSound(actor, actor);
end;

//==============================================================================
//
// P_DoChaseVanilla
//
//==============================================================================
procedure P_DoChaseVanilla(actor: Pmobj_t);
var
  delta: integer;
  nomissile: boolean;
begin
  if actor.reactiontime <> 0 then
    actor.reactiontime := actor.reactiontime - 1;

  // modify target threshold
  if actor.threshold <> 0 then
  begin
    if (actor.target = nil) or (actor.target.health <= 0) then
      actor.threshold := 0
    else
      actor.threshold := actor.threshold - 1;
  end;

  // turn towards movement direction if not there yet
  if actor.movedir < 8 then
  begin
    actor.angle := actor.angle and $E0000000;
    delta := actor.angle - _SHLW(actor.movedir, 29);

    if delta > 0 then
      actor.angle := actor.angle - ANG90 div 2
    else if delta < 0 then
      actor.angle := actor.angle + ANG90 div 2;
  end;

  if (actor.target = nil) or (actor.target.flags and MF_SHOOTABLE = 0) then
  begin
    // look for a new target
    if P_LookForPlayers(actor, True) then
      exit; // got a new target

    P_SetMobjState(actor, statenum_t(actor.info.spawnstate));
    exit;
  end;

  // do not attack twice in a row
  if actor.flags and MF_JUSTATTACKED <> 0 then
  begin
    actor.flags := actor.flags and not MF_JUSTATTACKED;
    if (gameskill <> sk_nightmare) and not fastparm then
      P_NewChaseDir(actor);
    exit;
  end;

  // check for melee attack
  if (actor.info.meleestate <> 0) and P_CheckMeleeRange(actor) then
  begin
    if actor.info.attacksound <> 0 then
      S_StartSound(actor, actor.info.attacksound);

    P_SetMobjState(actor, statenum_t(actor.info.meleestate));
    exit;
  end;

  // check for missile attack
  if actor.info.missilestate <> 0 then
  begin
    if (G_PlayingEngineVersion <= VERSION110) or (G_PlayingEngineVersion > VERSION205) then
    begin
      if not ((gameskill < sk_nightmare) and not fastparm and (actor.movecount <> 0)) then
        if P_CheckMissileRange(actor) then
        begin
          P_SetMobjState(actor, statenum_t(actor.info.missilestate));
          actor.flags := actor.flags or MF_JUSTATTACKED;
          exit;
        end;
    end
    else
    begin
      nomissile := False;
      if (gameskill < sk_nightmare) and not fastparm and (actor.movecount <> 0) then
        nomissile := True
      else if not P_CheckMissileRange(actor) then
        nomissile := True;
      if not nomissile then
      begin
        P_SetMobjState(actor, statenum_t(actor.info.missilestate));
        actor.flags := actor.flags or MF_JUSTATTACKED;
        exit;
      end;
    end;
  end;

  // possibly choose another target
  if netgame and (actor.threshold = 0) and not P_CheckSight(actor, actor.target) then
  begin
    if P_LookForPlayers(actor, True) then
      exit;  // got a new target
  end;

  // chase towards player
  actor.movecount := actor.movecount - 1;
  if (actor.movecount < 0) or not P_Move(actor) then
    P_NewChaseDir(actor);

  // make active sound
  if (actor.info.activesound <> 0) and (P_Random < 3) then
    S_StartSound(actor, actor.info.activesound);
end;

//==============================================================================
//
// A_Chase
//
//==============================================================================
procedure A_Chase(actor: Pmobj_t);
begin
  if G_PlayingEngineVersion <= VERSION110 then // JVAL: 20210103 - Vanilla demo
    P_DoChaseVanilla(actor)
  else
    P_DoChase(actor, false);
end;

//==============================================================================
//
// A_FaceTarget
//
//==============================================================================
procedure A_FaceTarget(actor: Pmobj_t);
begin
  if actor.target = nil then
    exit;

  actor.flags := actor.flags and not MF_AMBUSH;

  actor.angle :=
    R_PointToAngle2(actor.x, actor.y, actor.target.x, actor.target.y);

  if actor.target.flags and MF_SHADOW <> 0 then
    if actor.flags2_ex and MF2_EX_SEEINVISIBLE = 0 then
      actor.angle := actor.angle + _SHLW(P_Random - P_Random, 21);
end;

//==============================================================================
//
// A_PosAttack
//
//==============================================================================
procedure A_PosAttack(actor: Pmobj_t);
var
  angle: angle_t;
  damage: integer;
  slope: integer;
begin
  if actor.target = nil then
    exit;

  A_FaceTarget(actor);
  angle := actor.angle;
  slope := P_AimLineAttack(actor, angle, MISSILERANGE);

  S_StartSound(actor, Ord(sfx_pistol));
  angle := angle + _SHLW(P_Random - P_Random, 20);
  damage := ((P_Random mod 5) + 1) * 3;
  P_LineAttack(actor, angle, MISSILERANGE, slope, damage);
end;

//==============================================================================
//
// A_SPosAttack
//
//==============================================================================
procedure A_SPosAttack(actor: Pmobj_t);
var
  i: integer;
  angle: angle_t;
  bangle: angle_t;
  damage: integer;
  slope: integer;
begin
  if actor.target = nil then
    exit;

  S_StartSound(actor, Ord(sfx_shotgn));
  A_FaceTarget(actor);
  bangle := actor.angle;
  slope := P_AimLineAttack(actor, bangle, MISSILERANGE);

  for i := 0 to 2 do
  begin
    angle := bangle + _SHLW(P_Random - P_Random, 20);
    damage := ((P_Random mod 5) + 1) * 3;
    P_LineAttack(actor, angle, MISSILERANGE, slope, damage);
  end;
end;

//==============================================================================
//
// A_CPosAttack
//
//==============================================================================
procedure A_CPosAttack(actor: Pmobj_t);
var
  angle: angle_t;
  bangle: angle_t;
  damage: integer;
  slope: integer;
begin
  if actor.target = nil then
    exit;

  S_StartSound(actor, Ord(sfx_shotgn));
  A_FaceTarget(actor);
  bangle := actor.angle;

  slope := P_AimLineAttack(actor, bangle, MISSILERANGE);

  angle := bangle + _SHLW(P_Random - P_Random, 20);
  damage := ((P_Random mod 5) + 1) * 3;
  P_LineAttack(actor, angle, MISSILERANGE, slope, damage);
end;

//==============================================================================
//
// A_CPosRefire
//
//==============================================================================
procedure A_CPosRefire(actor: Pmobj_t);
begin
  // keep firing unless target got out of sight
  A_FaceTarget(actor);

  if P_Random < 40 then
    exit;

  if (actor.target = nil) or (actor.target.health <= 0) or
     not P_CheckSight(actor, actor.target) then
    P_SetMobjState(actor, statenum_t(actor.info.seestate));
end;

//==============================================================================
//
// A_SpidRefire
//
//==============================================================================
procedure A_SpidRefire(actor: Pmobj_t);
begin
  // keep firing unless target got out of sight
  A_FaceTarget(actor);

  if P_Random < 10 then
    exit;

  if (actor.target = nil) or (actor.target.health <= 0) or
     not P_CheckSight(actor, actor.target) then
    P_SetMobjState(actor, statenum_t(actor.info.seestate));
end;

//==============================================================================
//
// A_BspiAttack
//
//==============================================================================
procedure A_BspiAttack(actor: Pmobj_t);
begin
  if actor.target = nil then
    exit;

  A_FaceTarget(actor);

  // launch a missile
  P_SpawnMissile(actor, actor.target, Ord(MT_ARACHPLAZ));
end;

//==============================================================================
//
// A_TroopAttack
//
//==============================================================================
procedure A_TroopAttack(actor: Pmobj_t);
var
  damage: integer;
begin
  if actor.target = nil then
    exit;

  A_FaceTarget(actor);
  if P_CheckMeleeRange(actor) then
  begin
    S_StartSound(actor, Ord(sfx_claw));
    damage := (P_Random mod 8 + 1) * 3;
    P_DamageMobj(actor.target, actor, actor, damage);
    exit;
  end;

  // launch a missile
  P_SpawnMissile(actor, actor.target, Ord(MT_TROOPSHOT));
end;

//==============================================================================
//
// A_SargAttack
//
//==============================================================================
procedure A_SargAttack(actor: Pmobj_t);
var
  damage: integer;
begin
  if actor.target = nil then
    exit;

  A_FaceTarget(actor);
  if P_CheckMeleeRange(actor) then
  begin
    damage := ((P_Random mod 10) + 1) * 4;
    P_DamageMobj(actor.target, actor, actor, damage);
  end;
end;

//==============================================================================
//
// A_HeadAttack
//
//==============================================================================
procedure A_HeadAttack(actor: Pmobj_t);
var
  damage: integer;
begin
  if actor.target = nil then
    exit;

  A_FaceTarget(actor);
  if P_CheckMeleeRange(actor) then
  begin
    damage := (P_Random mod 6 + 1) * 10;
    P_DamageMobj(actor.target, actor, actor, damage);
    exit;
  end;

  // launch a missile
  P_SpawnMissile(actor, actor.target, Ord(MT_HEADSHOT));
end;

//==============================================================================
//
// A_CyberAttack
//
//==============================================================================
procedure A_CyberAttack(actor: Pmobj_t);
begin
  if actor.target = nil then
    exit;

  A_FaceTarget(actor);
  P_SpawnMissile(actor, actor.target, Ord(MT_ROCKET));
end;

//==============================================================================
//
// A_BruisAttack
//
//==============================================================================
procedure A_BruisAttack(actor: Pmobj_t);
var
  damage: integer;
begin
  if actor.target = nil then
    exit;

  if P_CheckMeleeRange(actor) then
  begin
    S_StartSound(actor, Ord(sfx_claw));
    damage := (P_Random mod 8 + 1) * 10;
    P_DamageMobj(actor.target, actor, actor, damage);
    exit;
  end;

  // launch a missile
  P_SpawnMissile(actor, actor.target, Ord(MT_BRUISERSHOT));
end;

//==============================================================================
//
// A_SkelMissile
//
//==============================================================================
procedure A_SkelMissile(actor: Pmobj_t);
var
  mo: Pmobj_t;
begin
  if actor.target = nil then
    exit;

  A_FaceTarget(actor);
  actor.z := actor.z + 16 * FRACUNIT; // so missile spawns higher

  mo := P_SpawnMissile(actor, actor.target, Ord(MT_TRACER));

  actor.z := actor.z - 16 * FRACUNIT; // back to normal

  if mo <> nil then
  begin
    mo.x := mo.x + mo.momx;
    mo.y := mo.y + mo.momy;
    mo.tracer := actor.target;
  end;
end;

const
  TRACEANGLE = $c000000;

//==============================================================================
//
// A_Tracer
//
//==============================================================================
procedure A_Tracer(actor: Pmobj_t);
var
  exact: angle_t;
  dist: fixed_t;
  slope: fixed_t;
  dest: Pmobj_t;
  th: Pmobj_t;
begin
  if (gametic - demostarttic) and 3 <> 0 then // [crispy] fix revenant internal demo bug
    exit;

  // spawn a puff of smoke behind the rocket
  P_SpawnPuff(actor.x, actor.y, actor.z);

  th := P_SpawnMobj(actor.x - actor.momx,
                    actor.y - actor.momy,
                    actor.z, Ord(MT_SMOKE));

  th.momz := FRACUNIT;
  th.tics := th.tics - P_Random and 3;
  if th.tics < 1 then
    th.tics := 1;

  // adjust direction
  dest := actor.tracer;

  if (dest = nil) or (dest.health <= 0) then
    exit;

  // change angle
  exact := R_PointToAngle2(actor.x, actor.y, dest.x, dest.y);

  if exact <> actor.angle then
  begin
    if exact - actor.angle > ANG180 then
    begin
      actor.angle := actor.angle - TRACEANGLE;
      if exact - actor.angle < ANG180 then
        actor.angle := exact;
    end
    else
    begin
      actor.angle := actor.angle + TRACEANGLE;
      if exact - actor.angle > ANG180 then
        actor.angle := exact;
    end;
  end;

  {$IFDEF FPC}
  exact := _SHRW(actor.angle, ANGLETOFINESHIFT);
  {$ELSE}
  exact := actor.angle shr ANGLETOFINESHIFT;
  {$ENDIF}
  actor.momx := FixedMul(actor.info.speed, finecosine[exact]);
  actor.momy := FixedMul(actor.info.speed, finesine[exact]);

  // change slope
  dist := P_AproxDistance(dest.x - actor.x, dest.y - actor.y);

  dist := dist div actor.info.speed;

  if dist < 1 then
    dist := 1;
  slope := (dest.z + 40 * FRACUNIT - actor.z) div dist;

  if slope < actor.momz then
    actor.momz := actor.momz - FRACUNIT div 8
  else
    actor.momz := actor.momz + FRACUNIT div 8;
end;

//==============================================================================
//
// A_SkelWhoosh
//
//==============================================================================
procedure A_SkelWhoosh(actor: Pmobj_t);
begin
  if actor.target = nil then
    exit;

  A_FaceTarget(actor);
  S_StartSound(actor, Ord(sfx_skeswg));
end;

//==============================================================================
//
// A_SkelFist
//
//==============================================================================
procedure A_SkelFist(actor: Pmobj_t);
var
  damage: integer;
begin
  if actor.target = nil then
    exit;

  A_FaceTarget(actor);

  if P_CheckMeleeRange(actor) then
  begin
    damage := ((P_Random mod 10) + 1) * 6;
    S_StartSound(actor, Ord(sfx_skepch));
    P_DamageMobj(actor.target, actor, actor, damage);
  end;
end;

//
// PIT_VileCheck
// Detect a corpse that could be raised.
//
var
  corpsehit: Pmobj_t;
  viletryx: fixed_t;
  viletryy: fixed_t;

//==============================================================================
//
// PIT_VileCheck
//
//==============================================================================
function PIT_VileCheck(thing: Pmobj_t): boolean;
var
  maxdist: integer;
  check: boolean;
begin
  if thing.flags and MF_CORPSE = 0 then
  begin
    result := true; // not a monster
    exit;
  end;

  if thing.tics <> -1 then
  begin
    result := true; // not lying still yet
    exit;
  end;

  if thing.info.raisestate = Ord(S_NULL) then
  begin
    result := true; // monster doesn't have a raise state
    exit;
  end;

  maxdist := thing.info.radius + mobjinfo[Ord(MT_VILE)].radius;

  if (abs(thing.x - viletryx) > maxdist) or
     (abs(thing.y - viletryy) > maxdist) then
  begin
    result := true; // not actually touching
    exit;
  end;

  corpsehit := thing;
  corpsehit.momx := 0;
  corpsehit.momy := 0;
  corpsehit.height := _SHL(corpsehit.height, 2);
  check := P_CheckPosition(corpsehit, corpsehit.x, corpsehit.y);
  corpsehit.height := _SHR2(corpsehit.height);

  if not check then
    result := true    // doesn't fit here
  else
    result := false;  // got one, so stop checking
end;

//==============================================================================
//
// A_VileChase
// Check for ressurecting a body
//
//==============================================================================
procedure A_VileChase(actor: Pmobj_t);
var
  xl: integer;
  xh: integer;
  yl: integer;
  yh: integer;

  bx: integer;
  by: integer;

  info: Pmobjinfo_t;
  temp: Pmobj_t;
begin
  if actor.movedir <> Ord(DI_NODIR) then
  begin
    // check for corpses to raise
    viletryx := actor.x + actor.info.speed * xspeed[actor.movedir];
    viletryy := actor.y + actor.info.speed * yspeed[actor.movedir];

    if internalblockmapformat then
    begin
      xl := MapBlockIntX(int64(viletryx) - int64(bmaporgx) - MAXRADIUS * 2);
      xh := MapBlockIntX(int64(viletryx) - int64(bmaporgx) + MAXRADIUS * 2);
      yl := MapBlockIntY(int64(viletryy) - int64(bmaporgy) - MAXRADIUS * 2);
      yh := MapBlockIntY(int64(viletryy) - int64(bmaporgy) + MAXRADIUS * 2);
    end
    else
    begin
      xl := MapBlockInt(viletryx - bmaporgx - MAXRADIUS * 2);
      xh := MapBlockInt(viletryx - bmaporgx + MAXRADIUS * 2);
      yl := MapBlockInt(viletryy - bmaporgy - MAXRADIUS * 2);
      yh := MapBlockInt(viletryy - bmaporgy + MAXRADIUS * 2);
    end;

    for bx := xl to xh do
    begin
      for by := yl to yh do
      begin
      // Call PIT_VileCheck to check
      // whether object is a corpse
      // that canbe raised.
        if not P_BlockThingsIterator(bx, by, PIT_VileCheck) then
        begin
        // got one!
          temp := actor.target;
          actor.target := corpsehit;
          A_FaceTarget(actor);
          actor.target := temp;

          if actor.info.healstate <> Ord(S_NULL) then
            P_SetMobjState(actor, statenum_t(actor.info.healstate))
          else
            P_SetMobjState(actor, S_VILE_HEAL1);

          S_StartSound(corpsehit, Ord(sfx_slop));
          info := corpsehit.info;

          P_SetMobjState(corpsehit, statenum_t(info.raisestate));

          if G_PlayingEngineVersion >= VERSION205 then
          begin
            corpsehit.height := info.height; // fix Ghost bug
            corpsehit.radius := info.radius; // fix Ghost bug
          end
          else
            corpsehit.height := _SHL(corpsehit.height, 2);
          corpsehit.flags := info.flags;
          corpsehit.flags_ex := info.flags_ex;
          corpsehit.flags2_ex := info.flags2_ex;
          // Inherit friend flag
          if actor.flags2_ex and MF2_EX_FRIEND = 0 then
            corpsehit.flags2_ex := corpsehit.flags2_ex and not MF2_EX_FRIEND
          else
            corpsehit.flags2_ex := corpsehit.flags2_ex or MF2_EX_FRIEND;
          corpsehit.flags3_ex := info.flags3_ex;
          corpsehit.flags4_ex := info.flags4_ex;
          corpsehit.flags5_ex := info.flags5_ex;
          corpsehit.flags6_ex := info.flags6_ex;
          corpsehit.health := info.spawnhealth;
          corpsehit.target := nil;
          exit;
        end;
      end;
    end;
  end;

  // Return to normal attack.
  A_Chase(actor);
end;

//==============================================================================
//
// A_VileStart
//
//==============================================================================
procedure A_VileStart(actor: Pmobj_t);
begin
  S_StartSound(actor, Ord(sfx_vilatk));
end;

//==============================================================================
//
// A_Fire
// Keep fire in front of player unless out of sight
//
//==============================================================================
procedure A_Fire(actor: Pmobj_t);
var
  dest: Pmobj_t;
  an: LongWord;
begin
  dest := actor.tracer;
  if dest = nil then
    exit;

  // don't move it if the vile lost sight
  if not P_CheckSight(actor.target, dest) then
    exit;

  {$IFDEF FPC}
  an := _SHRW(dest.angle, ANGLETOFINESHIFT);
  {$ELSE}
  an := dest.angle shr ANGLETOFINESHIFT;
  {$ENDIF}

  P_UnsetThingPosition(actor);
  actor.x := dest.x + FixedMul(24 * FRACUNIT, finecosine[an]);
  actor.y := dest.y + FixedMul(24 * FRACUNIT, finesine[an]);
  actor.z := dest.z;
  P_SetThingPosition(actor);
end;

//==============================================================================
//
// A_StartFire
//
//==============================================================================
procedure A_StartFire(actor: Pmobj_t);
begin
  S_StartSound(actor, Ord(sfx_flamst));
  A_Fire(actor);
end;

//==============================================================================
//
// A_FireCrackle
//
//==============================================================================
procedure A_FireCrackle(actor: Pmobj_t);
begin
  S_StartSound(actor, Ord(sfx_flame));
  A_Fire(actor);
end;

//==============================================================================
//
// A_VileTarget
// Spawn the hellfire
//
//==============================================================================
procedure A_VileTarget(actor: Pmobj_t);
var
  fog: Pmobj_t;
begin
  if actor.target = nil then
    exit;

  A_FaceTarget(actor);

  // JVAL: Correct the Arch-Vile fire spawned at the wrong location bug
  //       https://doomwiki.org/wiki/Arch-Vile_fire_spawned_at_the_wrong_location
  if G_PlayingEngineVersion <= VERSION203 then
    fog := P_SpawnMobj(actor.target.x, actor.target.x, actor.target.z, Ord(MT_FIRE))
  else
    fog := P_SpawnMobj(actor.target.x, actor.target.y, actor.target.z, Ord(MT_FIRE));

  actor.tracer := fog;
  fog.target := actor;
  fog.tracer := actor.target;
  A_Fire(fog);
end;

//==============================================================================
//
// A_VileAttack
//
//==============================================================================
procedure A_VileAttack(actor: Pmobj_t);
var
  fire: Pmobj_t;
  an: angle_t;
begin
  if actor.target = nil then
    exit;

  A_FaceTarget(actor);

  if not P_CheckSight(actor, actor.target) then
    exit;

  S_StartSound(actor, Ord(sfx_barexp));
  P_DamageMobj(actor.target, actor, actor, 20);
  actor.target.momz := 1000 * FRACUNIT div actor.target.mass;

  {$IFDEF FPC}
  an := _SHRW(actor.angle, ANGLETOFINESHIFT);
  {$ELSE}
  an := actor.angle shr ANGLETOFINESHIFT;
  {$ENDIF}

  fire := actor.tracer;

  if fire = nil then
    exit;

  // move the fire between the vile and the player
  fire.x := actor.target.x - FixedMul(24 * FRACUNIT, finecosine[an]);
  fire.y := actor.target.y - FixedMul(24 * FRACUNIT, finesine[an]);
  P_RadiusAttack(fire, actor, 70);
end;

//
// Mancubus attack,
// firing three missiles (bruisers)
// in three different directions?
// Doesn't look like it.
//
const
  FATSPREAD = ANG90 div 8;

//==============================================================================
//
// A_FatRaise
//
//==============================================================================
procedure A_FatRaise(actor: Pmobj_t);
begin
  A_FaceTarget(actor);
  S_StartSound(actor, Ord(sfx_manatk));
end;

//==============================================================================
//
// A_FatAttack1
//
//==============================================================================
procedure A_FatAttack1(actor: Pmobj_t);
var
  mo: Pmobj_t;
  an: angle_t;
begin
  A_FaceTarget(actor);
  // Change direction  to ...
  actor.angle := actor.angle + FATSPREAD;
  P_SpawnMissile(actor, actor.target, Ord(MT_FATSHOT));

  mo := P_SpawnMissile(actor, actor.target, Ord(MT_FATSHOT));
  if mo = nil then // JVAL: Prevent savegame bug
    exit;

  mo.angle := mo.angle + FATSPREAD;
  {$IFDEF FPC}
  an := _SHRW(mo.angle, ANGLETOFINESHIFT);
  {$ELSE}
  an := mo.angle shr ANGLETOFINESHIFT;
  {$ENDIF}
  mo.momx := FixedMul(mo.info.speed, finecosine[an]);
  mo.momy := FixedMul(mo.info.speed, finesine[an]);
end;

//==============================================================================
//
// A_FatAttack2
//
//==============================================================================
procedure A_FatAttack2(actor: Pmobj_t);
var
  mo: Pmobj_t;
  an: angle_t;
begin
  A_FaceTarget(actor);
  // Now here choose opposite deviation.
  actor.angle := actor.angle - FATSPREAD;
  P_SpawnMissile(actor, actor.target, Ord(MT_FATSHOT));

  mo := P_SpawnMissile(actor, actor.target, Ord(MT_FATSHOT));
  if mo = nil then  // JVAL: Prevent savegame bug
    exit;

  mo.angle := mo.angle - FATSPREAD * 2;
  {$IFDEF FPC}
  an := _SHRW(mo.angle, ANGLETOFINESHIFT);
  {$ELSE}
  an := mo.angle shr ANGLETOFINESHIFT;
  {$ENDIF}
  mo.momx := FixedMul(mo.info.speed, finecosine[an]);
  mo.momy := FixedMul(mo.info.speed, finesine[an]);
end;

//==============================================================================
//
// A_FatAttack3
//
//==============================================================================
procedure A_FatAttack3(actor: Pmobj_t);
var
  mo: Pmobj_t;
  an: angle_t;
begin
  A_FaceTarget(actor);

  mo := P_SpawnMissile(actor, actor.target, Ord(MT_FATSHOT));
  if mo = nil then  // JVAL: Prevent savegame bug
    exit;

  mo.angle := mo.angle - FATSPREAD div 2;
  {$IFDEF FPC}
  an := _SHRW(mo.angle, ANGLETOFINESHIFT);
  {$ELSE}
  an := mo.angle shr ANGLETOFINESHIFT;
  {$ENDIF}
  mo.momx := FixedMul(mo.info.speed, finecosine[an]);
  mo.momy := FixedMul(mo.info.speed, finesine[an]);

  mo := P_SpawnMissile(actor, actor.target, Ord(MT_FATSHOT));
  if mo = nil then
    exit;

  mo.angle := mo.angle + FATSPREAD div 2;
  {$IFDEF FPC}
  an := _SHRW(mo.angle, ANGLETOFINESHIFT);
  {$ELSE}
  an := mo.angle shr ANGLETOFINESHIFT;
  {$ENDIF}
  mo.momx := FixedMul(mo.info.speed, finecosine[an]);
  mo.momy := FixedMul(mo.info.speed, finesine[an]);
end;

//
// SkullAttack
// Fly at the player like a missile.
//
const
  SKULLSPEED = 20 * FRACUNIT;

//==============================================================================
//
// A_SkullAttack
//
//==============================================================================
procedure A_SkullAttack(actor: Pmobj_t);
var
  dest: Pmobj_t;
  an: angle_t;
  dist: integer;
begin
  if actor.target = nil then
    exit;

  dest := actor.target;
  actor.flags := actor.flags or MF_SKULLFLY;

  A_AttackSound(actor, actor);
  A_FaceTarget(actor);
  {$IFDEF FPC}
  an := _SHRW(actor.angle, ANGLETOFINESHIFT);
  {$ELSE}
  an := actor.angle shr ANGLETOFINESHIFT;
  {$ENDIF}
  actor.momx := FixedMul(SKULLSPEED, finecosine[an]);
  actor.momy := FixedMul(SKULLSPEED, finesine[an]);
  dist := P_AproxDistance(dest.x - actor.x, dest.y - actor.y);
  dist := dist div SKULLSPEED;

  if dist < 1 then
    dist := 1;
  actor.momz := (dest.z + _SHR1(dest.height) - actor.z) div dist;
end;

//==============================================================================
//
// A_PainShootSkull
// Spawn a lost soul and launch it at the target
//
//==============================================================================
procedure A_PainShootSkull(actor: Pmobj_t; angle: angle_t);
var
  x: fixed_t;
  y: fixed_t;
  z: fixed_t;

  newmobj: Pmobj_t;
  an: angle_t;
  prestep: integer;
  count: integer;
  currentthinker: Pthinker_t;
begin
  // count total number of skull currently on the level
  count := 0;

  currentthinker := thinkercap.next;
  while currentthinker <> @thinkercap do
  begin
    if (@currentthinker._function.acp1 = @P_MobjThinker) and
       (Pmobj_t(currentthinker)._type = Ord(MT_SKULL)) then
      inc(count);
    currentthinker := currentthinker.next;
  end;

  // if there are allready 20 skulls on the level,
  // don't spit another one
  if count > 20 then
    exit;

  // okay, there's playe for another one
  {$IFDEF FPC}
  an := _SHRW(angle, ANGLETOFINESHIFT);
  {$ELSE}
  an := angle shr ANGLETOFINESHIFT;
  {$ENDIF}

  prestep := 4 * FRACUNIT +
             3 * (actor.info.radius + mobjinfo[Ord(MT_SKULL)].radius) div 2;

  x := actor.x + FixedMul(prestep, finecosine[an]);
  y := actor.y + FixedMul(prestep, finesine[an]);
  z := actor.z + 8 * FRACUNIT;

  newmobj := P_SpawnMobj(x, y, z, Ord(MT_SKULL));

  // Check for movements.
  if not P_TryMove(newmobj, newmobj.x, newmobj.y) then
  begin
    // kill it immediately
    P_DamageMobj(newmobj, actor, actor, 10000);
    exit;
  end;

  // killough 7/20/98: PEs shoot lost souls with the same friendliness
  if actor.flags2_ex and MF2_EX_FRIEND = 0 then
    newmobj.flags2_ex := newmobj.flags2_ex and not MF2_EX_FRIEND
  else
    newmobj.flags2_ex := newmobj.flags2_ex or MF2_EX_FRIEND;

  newmobj.target := actor.target;
  A_SkullAttack(newmobj);
end;

//==============================================================================
//
// A_PainAttack
// Spawn a lost soul and launch it at the target
//
//==============================================================================
procedure A_PainAttack(actor: Pmobj_t);
begin
  if actor.target = nil then
    exit;

  A_FaceTarget(actor);
  A_PainShootSkull(actor, actor.angle);
end;

//==============================================================================
//
// A_PainDie
//
//==============================================================================
procedure A_PainDie(actor: Pmobj_t);
begin
  A_Fall(actor);
  A_PainShootSkull(actor, actor.angle + ANG90);
  A_PainShootSkull(actor, actor.angle + ANG180);
  A_PainShootSkull(actor, actor.angle + ANG270);
end;

//==============================================================================
//
// A_Scream
//
//==============================================================================
procedure A_Scream(actor: Pmobj_t);
var
  sound: integer;
begin
  case actor.info.deathsound of
    0: exit;
    Ord(sfx_podth1),
    Ord(sfx_podth2),
    Ord(sfx_podth3):
      sound := Ord(sfx_podth1) + P_Random mod 3;

    Ord(sfx_bgdth1),
    Ord(sfx_bgdth2):
      sound := Ord(sfx_bgdth1) + P_Random mod 2;
  else
    sound := actor.info.deathsound;
  end;

  // Check for bosses.
  if (actor._type = Ord(MT_SPIDER)) or
     (actor._type = Ord(MT_CYBORG)) or
     (actor.flags_ex and MF_EX_BOSS <> 0) or
     (actor.flags2_ex and MF2_EX_FULLVOLDEATH <> 0) then
    // full volume
    S_StartSound(nil, sound)
  else
    S_StartSound(actor, sound);
end;

//==============================================================================
//
// A_XScream
//
//==============================================================================
procedure A_XScream(actor: Pmobj_t);
begin
  S_StartSound(actor, Ord(sfx_slop));
end;

//==============================================================================
//
// A_Pain
//
//==============================================================================
procedure A_Pain(actor: Pmobj_t);
begin
  A_PainSound1(actor);
end;

//==============================================================================
//
// A_Explode
//
//==============================================================================
procedure A_Explode(thingy: Pmobj_t);
begin
  if G_PlayingEngineVersion >= VERSION205 then
  begin
    if thingy.flags_ex and MF_EX_CUSTOMEXPLODE <> 0 then
      P_RadiusAttackEx(thingy, thingy.target, thingy.info.explosiondamage, thingy.info.explosionradius)
    else if thingy.state.params <> nil then
      P_RadiusAttackEx(thingy, thingy.target, thingy.state.params.IntVal[0], thingy.state.params.IntVal[1])
    else
      P_RadiusAttack(thingy, thingy.target, 128);
    if thingy.z <= thingy.floorz then
      P_HitFloor(thingy);
  end
  else
  begin
    if thingy.flags_ex and MF_EX_CUSTOMEXPLODE <> 0 then
      P_RadiusAttackEx(thingy, thingy.target, thingy.info.explosiondamage, thingy.info.explosionradius)
    else if thingy.state.params <> nil then
      P_RadiusAttackEx(thingy, thingy.target, thingy.state.params.IntVal[0], thingy.state.params.IntVal[1])
    else
    begin
      P_RadiusAttack(thingy, thingy.target, 128);
      P_HitFloor(thingy);
    end;
  end;
end;

//==============================================================================
//
// A_BossDeath
// Possibly trigger special effects
// if on first boss level
//
//==============================================================================
procedure A_BossDeath(mo: Pmobj_t);
var
  th: Pthinker_t;
  mo2: Pmobj_t;
  junk: line_t;
  i: integer;
begin
  // numbossactions == 0 means to use the defaults.
  // numbossactions == -1 means to do nothing.
  // positive values mean to check the list of boss actions and run all that apply.
  if (gamemapinfo <> nil) and (gamemapinfo.numbossactions <> 0) then
  begin
    if gamemapinfo.numbossactions < 0 then
      exit;

    // make sure there is a player alive for victory
    i := 0;
    while i < MAXPLAYERS do
    begin
      if playeringame[i] and (players[i].health > 0) then
        break;
      Inc(i);
    end;

    if i = MAXPLAYERS then
      exit; // no one left alive, so do not end game

    i := 0;
    while i < gamemapinfo.numbossactions do
    begin
      if gamemapinfo.bossactions[i].typ = mo._type then
        break;
      Inc(i);
    end;

    if i >= gamemapinfo.numbossactions then
      exit; // no matches found

    // scan the remaining thinkers to see
    // if all bosses are dead
    th := thinkercap.next;
    while th <> @thinkercap do
    begin
      if @th._function.acp1 = @P_MobjThinker then
      begin
        mo2 := Pmobj_t(th);
        if (mo2 <> mo) and (mo2._type = mo._type) and (mo2.health > 0) then
        begin
          // other boss not dead
          exit;
        end;
      end;
      th := th.next;
    end;

    for i := 0 to gamemapinfo.numbossactions - 1 do
      if gamemapinfo.bossactions[i].typ = mo._type then
      begin
        junk := lines[0];
        junk.special := gamemapinfo.bossactions[i].special;
        junk.tag := gamemapinfo.bossactions[i].tag;
        // use special semantics for line activation to block problem types.
        if not P_UseSpecialLine(mo, @junk, 0, true) then
          P_CrossSpecialLinePtr(@junk, 0, mo, true);
      end;

    exit;
  end;

  if gamemode = commercial then
  begin
    if gamemap <> 7 then
      exit;

    if mo.flags4_ex and MF4_EX_MAP07BOSS = 0 then
      exit;
  end
  else
  begin
    case gameepisode of
      1:
        begin
          if gamemap <> 8 then
            exit;
          if mo.flags4_ex and MF4_EX_E1M8BOSS = 0 then
            exit;
        end;
      2:
        begin
          if gamemap <> 8 then
            exit;
          if mo.flags4_ex and MF4_EX_E2M8BOSS = 0 then
            exit;
        end;
      3:
        begin
          if gamemap <> 8 then
            exit;
          if mo.flags4_ex and MF4_EX_E3M8BOSS = 0 then
            exit;
        end;
      4:
        begin
          case gamemap of
            6: if mo.flags4_ex and MF4_EX_E4M6BOSS = 0 then
                 exit;
            8: if mo.flags4_ex and MF4_EX_E4M8BOSS = 0 then
                 exit;
          else  // JVAL 21/9/2007 Fixed bug that ended E4M2 after cyberdeamon death
            begin
              if not majorbossdeathendsdoom1level then
                exit;
            end;
          end;
        end;
    else
      begin
        if gamemap <> 8 then
          exit;
      end;
    end;
  end;

  // make sure there is a player alive for victory
  i := 0;
  while i < MAXPLAYERS do
  begin
    if playeringame[i] and (players[i].health > 0) then
      break;
    inc(i);
  end;

  if i = MAXPLAYERS then
    exit;  // no one left alive, so do not end game

  // scan the remaining thinkers to see
  // if all bosses are dead
  th := thinkercap.next;
  while th <> @thinkercap do
  begin
    if @th._function.acp1 = @P_MobjThinker then
    begin
      mo2 := Pmobj_t(th);
      if (mo2 <> mo) and (mo2._type = mo._type) and (mo2.health > 0) then
      begin
        // other boss not dead
        exit;
      end;
    end;
    th := th.next;
  end;

  // victory!
  if gamemode = commercial then
  begin
    if gamemap = 7 then
    begin
      if mo.flags4_ex and MF4_EX_MAP07BOSS1 <> 0 then
      begin
        junk.tag := 666;
        EV_DoFloor(@junk, lowerFloorToLowest);
        exit;
      end;
      if mo.flags4_ex and MF4_EX_MAP07BOSS2 <> 0 then
      begin
        junk.tag := 667;
        EV_DoFloor(@junk, raiseToTexture);
        exit;
      end;
    end;
  end
  else
  begin
    case gameepisode of
      1:
        begin
          junk.tag := 666;
          EV_DoFloor(@junk, lowerFloorToLowest);
          exit;
        end;
      4:
        begin
          if gamemap = 6 then
          begin
            junk.tag := 666;
            EV_DoDoor(@junk, blazeOpen);
            exit;
          end
          else if gamemap = 8 then
          begin
            junk.tag := 666;
            EV_DoFloor(@junk, lowerFloorToLowest);
            exit;
          end;
        end;
    end;
  end;
  G_ExitLevel;
end;

//==============================================================================
//
// A_Hoof
//
//==============================================================================
procedure A_Hoof(mo: Pmobj_t);
begin
  S_StartSound(mo, Ord(sfx_hoof));
  A_Chase(mo);
end;

//==============================================================================
//
// A_Metal
//
//==============================================================================
procedure A_Metal(mo: Pmobj_t);
begin
  S_StartSound(mo, Ord(sfx_metal));
  A_Chase(mo);
end;

//==============================================================================
//
// A_BabyMetal
//
//==============================================================================
procedure A_BabyMetal(mo: Pmobj_t);
begin
  S_StartSound(mo, Ord(sfx_bspwlk));
  A_Chase(mo);
end;

//==============================================================================
//
// A_OpenShotgun2
//
//==============================================================================
procedure A_OpenShotgun2(player: Pplayer_t; psp: Pplayer_t);
begin
  S_StartSound(player.mo, Ord(sfx_dbopn));
end;

//==============================================================================
//
// A_LoadShotgun2
//
//==============================================================================
procedure A_LoadShotgun2(player: Pplayer_t; psp: Ppspdef_t);
begin
  S_StartSound(player.mo, Ord(sfx_dbload));
end;

//==============================================================================
//
// A_CloseShotgun2
//
//==============================================================================
procedure A_CloseShotgun2(player: Pplayer_t; psp: Ppspdef_t);
begin
  S_StartSound(player.mo, Ord(sfx_dbcls));
  A_ReFire(player, psp);
end;

const
  MAXBRAINTARGETS = 32;

var
  braintargets: array[0..MAXBRAINTARGETS - 1] of Pmobj_t;
  numbraintargets: integer;
  braintargeton: integer;

//==============================================================================
//
// A_BrainAwakeVanilla
//
//==============================================================================
procedure A_BrainAwakeVanilla(mo: Pmobj_t);
var
  thinker: Pthinker_t;
  m: Pmobj_t;
begin
  // find all the target spots
  numbraintargets := 0;
  braintargeton := 0;

  thinker := thinkercap.next;
  while Pointer(thinker) <> Pointer(@thinkercap) do
  begin
    if @thinker._function.acp1 = @P_MobjThinker then // is a mobj
    begin
      m := Pmobj_t(thinker);
      if m._type = Ord(MT_BOSSTARGET) then
      begin
        if numbraintargets >= MAXBRAINTARGETS then
          I_Error('A_BrainAwakeVanilla(): numbraintargets = %d >= MAXBRAINTARGETS', [numbraintargets]);
        braintargets[numbraintargets] := m;
        inc(numbraintargets);
      end;
    end;
    thinker := thinker.next;
  end;

  S_StartSound(nil, Ord(sfx_bossit));
end;

const
  S_BRAINTARGETON = 'user_braintargeton';
  S_NUMBRAINSTARGETS = 'user_numbrainstargets';
  S_BRAINTARGETS = 'user_braintargets';

//==============================================================================
//
// A_BrainAwake
//
//==============================================================================
procedure A_BrainAwake(mo: Pmobj_t);
var
  thinker: Pthinker_t;
  m: Pmobj_t;
  fnumbraintargets: integer;
begin
  if G_NeedsCompatibilityMode then
  begin
    A_BrainAwakeVanilla(mo);
    Exit;
  end;

  // find all the target spots
  fnumbraintargets := 0;
  mapvars.IntVal[S_BRAINTARGETON] := 0;

  thinker := thinkercap.next;
  while Pointer(thinker) <> Pointer(@thinkercap) do
  begin
    if @thinker._function.acp1 = @P_MobjThinker then // is a mobj
    begin
      m := Pmobj_t(thinker);
      if m._type = Ord(MT_BOSSTARGET) then
      begin
        mapvars.IntValArray[S_BRAINTARGETS, fnumbraintargets] := m.key;
        inc(fnumbraintargets);
      end;
    end;
    thinker := thinker.next;
  end;

  mapvars.IntVal[S_NUMBRAINSTARGETS] := fnumbraintargets;

  S_StartSound(nil, Ord(sfx_bossit));
end;

//==============================================================================
//
// A_BrainPain
//
//==============================================================================
procedure A_BrainPain(mo: Pmobj_t);
begin
  S_StartSound(nil, Ord(sfx_bospn));
end;

//==============================================================================
//
// A_BrainScream
//
//==============================================================================
procedure A_BrainScream(mo: Pmobj_t);
var
  x: integer;
  y: integer;
  z: integer;
  th: Pmobj_t;
begin
  x := mo.x - 196 * FRACUNIT;
  while x < mo.x + 320 * FRACUNIT do
  begin
    y := mo.y - 320 * FRACUNIT;
    z := 128 + P_Random * 2 * FRACUNIT;
    th := P_SpawnMobj(x, y, z, Ord(MT_ROCKET));
    th.momz := P_Random * 512;

    P_SetMobjState(th, S_BRAINEXPLODE1);

    th.tics := th.tics - (P_Random and 7);
    if th.tics < 1 then
      th.tics := 1;
    x := x + FRACUNIT * 8;
  end;

  S_StartSound(nil, Ord(sfx_bosdth));
end;

//==============================================================================
//
// A_BrainExplode
//
//==============================================================================
procedure A_BrainExplode(mo: Pmobj_t);
var
  x: integer;
  y: integer;
  z: integer;
  th: Pmobj_t;
begin
  x := mo.x + (P_Random - P_Random) * 2048;
  y := mo.y;
  z := 128 + P_Random * 2 * FRACUNIT;
  th := P_SpawnMobj(x, y, z, Ord(MT_ROCKET));
  th.momz := P_Random * 512;

  P_SetMobjState(th, S_BRAINEXPLODE1);

  th.tics := th.tics - (P_Random and 7);
  if th.tics < 1 then
    th.tics := 1;
end;

//==============================================================================
//
// A_BrainDie
//
//==============================================================================
procedure A_BrainDie(mo: Pmobj_t);
begin
  G_ExitLevel;
end;

var
  easy: integer = 0;

//==============================================================================
//
// A_BrainSpitVanilla
//
//==============================================================================
procedure A_BrainSpitVanilla(mo: Pmobj_t);
var
  targ: Pmobj_t;
  newmobj: Pmobj_t;
begin
  easy := easy xor 1;
  if (gameskill <= sk_easy) and (easy = 0) then
    exit;

  if numbraintargets = 0 then // JVAL
  begin
    A_BrainAwakeVanilla(mo);
    exit;
  end;

  // shoot a cube at current target
  targ := braintargets[braintargeton];
  braintargeton := (braintargeton + 1) mod numbraintargets;

  // spawn brain missile
  newmobj := P_SpawnMissile(mo, targ, Ord(MT_SPAWNSHOT));
  if newmobj = nil then
    exit;

  newmobj.target := targ;
  newmobj.reactiontime := ((targ.y - mo.y) div newmobj.momy) div newmobj.state.tics;

  S_StartSound(nil, Ord(sfx_bospit));
end;

//==============================================================================
//
// A_BrainSpit
//
//==============================================================================
procedure A_BrainSpit(mo: Pmobj_t);
const
  S_EASY = 'user_braineasy';
var
  targ: Pmobj_t;
  newmobj: Pmobj_t;
  fbraineasy: integer;
  fbraintargeton: integer;
  fnumbraintargets: integer;
begin
  if G_NeedsCompatibilityMode then
  begin
    A_BrainSpitVanilla(mo);
    Exit;
  end;

  fbraineasy := mapvars.IntVal[S_EASY];
  fbraineasy := fbraineasy xor 1;
  mapvars.IntVal[S_EASY] := fbraineasy;
  if (gameskill <= sk_easy) and (fbraineasy = 0) then
    exit;

  fnumbraintargets := mapvars.IntVal[S_NUMBRAINSTARGETS];

  if fnumbraintargets = 0 then // JVAL
  begin
    A_BrainAwake(mo);
    exit;
  end;

  // shoot a cube at current target
  fbraintargeton := mapvars.IntVal[S_BRAINTARGETON];
  fbraintargeton := GetIntegerInRange(fbraintargeton, 0, fnumbraintargets - 1);
  targ := P_FindMobjFromKey(mapvars.IntValArray[S_BRAINTARGETS, fbraintargeton]);
  if targ = nil then
  begin
    A_BrainAwake(mo);
    exit;
  end;

  fbraintargeton := (fbraintargeton + 1) mod fnumbraintargets;
  mapvars.IntVal[S_BRAINTARGETON] := fbraintargeton;

  // spawn brain missile
  newmobj := P_SpawnMissile(mo, targ, Ord(MT_SPAWNSHOT));
  if newmobj = nil then
    exit;

  // killough 7/18/98: brain friendliness is transferred
  if mo.flags2_ex and MF2_EX_FRIEND = 0 then
    newmobj.flags2_ex := newmobj.flags2_ex and not MF2_EX_FRIEND
  else
    newmobj.flags2_ex := newmobj.flags2_ex or MF2_EX_FRIEND;

  newmobj.target := targ;
  newmobj.reactiontime := ((targ.y - mo.y) div newmobj.momy) div newmobj.state.tics;

  S_StartSound(nil, Ord(sfx_bospit));
end;

//==============================================================================
//
// A_SpawnFly
//
//==============================================================================
procedure A_SpawnFly(mo: Pmobj_t);
var
  newmobj: Pmobj_t;
  fog: Pmobj_t;
  targ: Pmobj_t;
  r: integer;
  _type: mobjtype_t;
begin
  mo.reactiontime := mo.reactiontime - 1;
  if mo.reactiontime > 0 then
    exit; // still flying

  targ := mo.target;

  if targ = nil then  // JVAL
  begin
    P_RemoveMobj(mo);
    exit;
  end;

  // First spawn teleport fog.
  fog := P_SpawnMobj(targ.x, targ.y, targ.z, Ord(MT_SPAWNFIRE));
  S_StartSound(fog, Ord(sfx_telept));

  // Randomly select monster to spawn.
  r := P_Random;

  // Probability distribution (kind of :),
  // decreasing likelihood.
  if r < 50 then
    _type := MT_TROOP
  else if r < 90 then
    _type := MT_SERGEANT
  else if r < 120 then
    _type := MT_SHADOWS
  else if r < 130 then
    _type := MT_PAIN
  else if r < 160 then
    _type := MT_HEAD
  else if r < 162 then
    _type := MT_VILE
  else if r < 172 then
    _type := MT_UNDEAD
  else if r < 192 then
    _type := MT_BABY
  else if r < 222 then
    _type := MT_FATSO
  else if r < 246 then
    _type := MT_KNIGHT
  else
    _type := MT_BRUISER;

  newmobj := P_SpawnMobj(targ.x, targ.y, targ.z, Ord(_type));
  if newmobj = nil then
    exit;

  // killough 7/18/98: brain friendliness is transferred
  if mo.flags2_ex and MF2_EX_FRIEND = 0 then
    newmobj.flags2_ex := newmobj.flags2_ex and not MF2_EX_FRIEND
  else
    newmobj.flags2_ex := newmobj.flags2_ex or MF2_EX_FRIEND;

  if P_LookForTargets(newmobj, true) then
    P_SetMobjState(newmobj, statenum_t(newmobj.info.seestate));

  // telefrag anything in this spot
  P_TeleportMove(newmobj, newmobj.x, newmobj.y);

  // remove self (i.e., cube).
  P_RemoveMobj(mo);
end;

//==============================================================================
// A_SpawnSound
//
// travelling cube sound
//
//==============================================================================
procedure A_SpawnSound(mo: Pmobj_t);
begin
  S_StartSound(mo, Ord(sfx_boscub));
  A_SpawnFly(mo);
end;

//==============================================================================
//
// A_PlayerScream
//
//==============================================================================
procedure A_PlayerScream(mo: Pmobj_t);
var
  sound: integer;
begin
  if (gamemode = commercial) and (mo.health < -50) then
  begin
    // IF THE PLAYER DIES
    // LESS THAN -50% WITHOUT GIBBING
    sound := Ord(sfx_pdiehi);
  end
  else
  begin
    // Default death sound.
    sound := Ord(sfx_pldeth);
  end;

  S_StartSound(mo, sound);
end;

//----------------------------------------------------------------------------
//
// PROC P_Massacre
//
// Kills all monsters.
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure P_Massacre;
var
  mo: Pmobj_t;
  think: Pthinker_t;
begin
  think := thinkercap.next;
  while think <> @thinkercap do
  begin
    if @think._function.acp1 = @P_MobjThinker then
    begin
      mo := Pmobj_t(think);
      if ((mo._type = Ord(MT_SKULL)) or (mo.flags and MF_COUNTKILL <> 0)) and (mo.health > 0) then
      begin
        mo.flags_ex := mo.flags_ex and not MF_EX_INVULNERABLE;
        mo.flags := mo.flags or MF_SHOOTABLE;
        P_DamageMobj(mo, nil, nil, 10000);
      end;
    end;
    think := think.next;
  end;
end;

//==============================================================================
//
// A_TurretChase
//
//==============================================================================
procedure A_TurretChase(actor: Pmobj_t);
var
  delta: integer;
  nomissile: boolean;
begin
  if actor.reactiontime <> 0 then
    actor.reactiontime := actor.reactiontime - 1;

  // modify target threshold
  if actor.threshold <> 0 then
  begin
    if (actor.target = nil) or (actor.target.health <= 0) then
      actor.threshold := 0
    else
      actor.threshold := actor.threshold - 1;
  end;

  // turn towards movement direction if not there yet
  if actor.movedir < 8 then
  begin
    actor.angle := actor.angle and $E0000000;
    delta := actor.angle - _SHLW(actor.movedir, 29);

    if delta > 0 then
      actor.angle := actor.angle - ANG90 div 2
    else if delta < 0 then
      actor.angle := actor.angle + ANG90 div 2;
  end;

  if P_BothFriends(actor, actor.target) then
  begin
    if P_LookForTargets(actor, actor.flags_ex and MF_EX_LOOKALLAROUND <> 0) then
      exit; // got a new target
    if actor.state <> @states[actor.info.spawnstate] then
      P_SetMobjStateNF(actor, statenum_t(actor.info.spawnstate));
    exit;
  end;

  if (actor.target = nil) or
     (actor.target.flags and MF_SHOOTABLE = 0) then
  begin
    // look for a new target
    if P_LookForTargets(actor, true) then
      exit; // got a new target

    if actor.state <> @states[actor.info.spawnstate] then
      P_SetMobjStateNF(actor, statenum_t(actor.info.spawnstate));
    exit;
  end;

  // do not attack twice in a row
  if actor.flags and MF_JUSTATTACKED <> 0 then
  begin
    actor.flags := actor.flags and not MF_JUSTATTACKED;
    exit;
  end;

  // check for melee attack
  if (actor.info.meleestate <> 0) and P_CheckMeleeRange(actor) then
  begin
    A_MeleeSound(actor, actor);
    if actor.state <> @states[actor.info.meleestate] then
      P_SetMobjState(actor, statenum_t(actor.info.meleestate));
    exit;
  end;

  nomissile := false;
  // check for missile attack
  if actor.info.missilestate <> 0 then
  begin
    if (gameskill < sk_nightmare) and not fastparm and (actor.movecount <> 0) then
      nomissile := true
    else if not P_CheckMissileRange(actor) then
      nomissile := true;
    if not nomissile then
    begin
      if actor.state <> @states[actor.info.missilestate] then
        P_SetMobjState(actor, statenum_t(actor.info.missilestate));
      actor.flags := actor.flags or MF_JUSTATTACKED;
      exit;
    end;
  end;

  // possibly choose another target
  if netgame and
    (actor.threshold = 0) and
    not P_CheckSight(actor, actor.target) then
  begin
    if P_LookForTargets(actor, true) then
      exit;  // got a new target
  end;

  // make active sound
  if (actor.info.activesound <> 0) and (P_Random < 3) then
    A_ActiveSound(actor, actor);
end;

end.

