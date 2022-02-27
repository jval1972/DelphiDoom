//------------------------------------------------------------------------------
//
//  DelphiHeretic is a source port of the game Heretic and it is
//  based on original Linux Doom as published by "id Software", on
//  Heretic source as published by "Raven" software and DelphiDoom
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
  m_fixed,
  p_mobj_h,
  tables;

//==============================================================================
//
// A_Fall
//
//==============================================================================
procedure A_Fall(actor: Pmobj_t);

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
// A_Pain
//
//==============================================================================
procedure A_Pain(actor: Pmobj_t);

//==============================================================================
//
// A_DripBlood
//
//==============================================================================
procedure A_DripBlood(actor: Pmobj_t);

//==============================================================================
//
// A_KnightAttack
//
//==============================================================================
procedure A_KnightAttack(actor: Pmobj_t);

//==============================================================================
//
// A_ImpExplode
//
//==============================================================================
procedure A_ImpExplode(actor: Pmobj_t);

//==============================================================================
//
// A_BeastPuff
//
//==============================================================================
procedure A_BeastPuff(actor: Pmobj_t);

//==============================================================================
//
// A_ImpMeAttack
//
//==============================================================================
procedure A_ImpMeAttack(actor: Pmobj_t);

//==============================================================================
//
// A_ImpMsAttack
//
//==============================================================================
procedure A_ImpMsAttack(actor: Pmobj_t);

//==============================================================================
//
// A_ImpMsAttack2
//
//==============================================================================
procedure A_ImpMsAttack2(actor: Pmobj_t);

//==============================================================================
//
// A_ImpDeath
//
//==============================================================================
procedure A_ImpDeath(actor: Pmobj_t);

//==============================================================================
//
// A_ImpXDeath1
//
//==============================================================================
procedure A_ImpXDeath1(actor: Pmobj_t);

//==============================================================================
//
// A_ImpXDeath2
//
//==============================================================================
procedure A_ImpXDeath2(actor: Pmobj_t);

//==============================================================================
//
// A_ChicAttack
//
//==============================================================================
procedure A_ChicAttack(actor: Pmobj_t);

//==============================================================================
//
// A_ChicLook
//
//==============================================================================
procedure A_ChicLook(actor: Pmobj_t);

//==============================================================================
//
// A_ChicChase
//
//==============================================================================
procedure A_ChicChase(actor: Pmobj_t);

//==============================================================================
//
// A_ChicPain
//
//==============================================================================
procedure A_ChicPain(actor: Pmobj_t);

//==============================================================================
//
// A_Feathers
//
//==============================================================================
procedure A_Feathers(actor: Pmobj_t);

//==============================================================================
//
// A_MummyAttack
//
//==============================================================================
procedure A_MummyAttack(actor: Pmobj_t);

//==============================================================================
//
// A_MummyAttack2
//
//==============================================================================
procedure A_MummyAttack2(actor: Pmobj_t);

//==============================================================================
//
// A_MummyFX1Seek
//
//==============================================================================
procedure A_MummyFX1Seek(actor: Pmobj_t);

//==============================================================================
//
// A_MummySoul
//
//==============================================================================
procedure A_MummySoul(mummy: Pmobj_t);

//==============================================================================
//
// A_Sor1Pain
//
//==============================================================================
procedure A_Sor1Pain(actor: Pmobj_t);

//==============================================================================
//
// A_Sor1Chase
//
//==============================================================================
procedure A_Sor1Chase(actor: Pmobj_t);

//==============================================================================
//
// A_Srcr1Attack
//
//==============================================================================
procedure A_Srcr1Attack(actor: Pmobj_t);

//==============================================================================
//
// A_SorcererRise
//
//==============================================================================
procedure A_SorcererRise(actor: Pmobj_t);

//==============================================================================
//
// A_Srcr2Decide
//
//==============================================================================
procedure A_Srcr2Decide(actor: Pmobj_t);

//==============================================================================
//
// A_Srcr2Attack
//
//==============================================================================
procedure A_Srcr2Attack(actor: Pmobj_t);

//==============================================================================
//
// A_BlueSpark
//
//==============================================================================
procedure A_BlueSpark(actor: Pmobj_t);

//==============================================================================
//
// A_GenWizard
//
//==============================================================================
procedure A_GenWizard(actor: Pmobj_t);

//==============================================================================
//
// A_Sor2DthInit
//
//==============================================================================
procedure A_Sor2DthInit(actor: Pmobj_t);

//==============================================================================
//
// A_Sor2DthLoop
//
//==============================================================================
procedure A_Sor2DthLoop(actor: Pmobj_t);

//==============================================================================
//
// A_SorZap
//
//==============================================================================
procedure A_SorZap(actor: Pmobj_t);

//==============================================================================
//
// A_SorRise
//
//==============================================================================
procedure A_SorRise(actor: Pmobj_t);

//==============================================================================
//
// A_SorDSph
//
//==============================================================================
procedure A_SorDSph(actor: Pmobj_t);

//==============================================================================
//
// A_SorDExp
//
//==============================================================================
procedure A_SorDExp(actor: Pmobj_t);

//==============================================================================
//
// A_SorDBon
//
//==============================================================================
procedure A_SorDBon(actor: Pmobj_t);

//==============================================================================
//
// A_SorSightSnd
//
//==============================================================================
procedure A_SorSightSnd(actor: Pmobj_t);

//==============================================================================
//
// A_MinotaurAtk1
//
//==============================================================================
procedure A_MinotaurAtk1(actor: Pmobj_t);

//==============================================================================
//
// A_MinotaurDecide
//
//==============================================================================
procedure A_MinotaurDecide(actor: Pmobj_t);

//==============================================================================
//
// A_MinotaurCharge
//
//==============================================================================
procedure A_MinotaurCharge(actor: Pmobj_t);

//==============================================================================
//
// A_MinotaurAtk2
//
//==============================================================================
procedure A_MinotaurAtk2(actor: Pmobj_t);

//==============================================================================
//
// A_MinotaurAtk3
//
//==============================================================================
procedure A_MinotaurAtk3(actor: Pmobj_t);

//==============================================================================
//
// A_MntrFloorFire
//
//==============================================================================
procedure A_MntrFloorFire(actor: Pmobj_t);

//==============================================================================
//
// A_BeastAttack
//
//==============================================================================
procedure A_BeastAttack(actor: Pmobj_t);

//==============================================================================
//
// A_HeadAttack
//
//==============================================================================
procedure A_HeadAttack(actor: Pmobj_t);

//==============================================================================
//
// A_WhirlwindSeek
//
//==============================================================================
procedure A_WhirlwindSeek(actor: Pmobj_t);

//==============================================================================
//
// A_HeadIceImpact
//
//==============================================================================
procedure A_HeadIceImpact(ice: Pmobj_t);

//==============================================================================
//
// A_HeadFireGrow
//
//==============================================================================
procedure A_HeadFireGrow(fire: Pmobj_t);

//==============================================================================
//
// A_SnakeAttack
//
//==============================================================================
procedure A_SnakeAttack(actor: Pmobj_t);

//==============================================================================
//
// A_SnakeAttack2
//
//==============================================================================
procedure A_SnakeAttack2(actor: Pmobj_t);

//==============================================================================
//
// A_ClinkAttack
//
//==============================================================================
procedure A_ClinkAttack(actor: Pmobj_t);

//==============================================================================
//
// A_WizAtk1
//
//==============================================================================
procedure A_WizAtk1(actor: Pmobj_t);

//==============================================================================
//
// A_WizAtk2
//
//==============================================================================
procedure A_WizAtk2(actor: Pmobj_t);

//==============================================================================
//
// A_WizAtk3
//
//==============================================================================
procedure A_WizAtk3(actor: Pmobj_t);

//==============================================================================
//
// A_Scream
//
//==============================================================================
procedure A_Scream(actor: Pmobj_t);

//==============================================================================
//
// A_NoBlocking
//
//==============================================================================
procedure A_NoBlocking(actor: Pmobj_t);

//==============================================================================
//
// A_Explode
//
//==============================================================================
procedure A_Explode(actor: Pmobj_t);

//==============================================================================
//
// A_PodPain
//
//==============================================================================
procedure A_PodPain(actor: Pmobj_t);

//==============================================================================
//
// A_RemovePod
//
//==============================================================================
procedure A_RemovePod(actor: Pmobj_t);

//==============================================================================
//
// A_MakePod
//
//==============================================================================
procedure A_MakePod(actor: Pmobj_t);

//==============================================================================
//
// A_BossDeath
//
//==============================================================================
procedure A_BossDeath(actor: Pmobj_t);

//==============================================================================
//
// A_ESound
//
//==============================================================================
procedure A_ESound(mo: Pmobj_t);

//==============================================================================
//
// A_SpawnTeleGlitter
//
//==============================================================================
procedure A_SpawnTeleGlitter(actor: Pmobj_t);

//==============================================================================
//
// A_SpawnTeleGlitter2
//
//==============================================================================
procedure A_SpawnTeleGlitter2(actor: Pmobj_t);

//==============================================================================
//
// A_AccTeleGlitter
//
//==============================================================================
procedure A_AccTeleGlitter(actor: Pmobj_t);

//==============================================================================
//
// A_InitKeyGizmo
//
//==============================================================================
procedure A_InitKeyGizmo(gizmo: Pmobj_t);

//==============================================================================
//
// A_VolcanoSet
//
//==============================================================================
procedure A_VolcanoSet(volcano: Pmobj_t);

//==============================================================================
//
// A_VolcanoBlast
//
//==============================================================================
procedure A_VolcanoBlast(volcano: Pmobj_t);

//==============================================================================
//
// A_VolcBallImpact
//
//==============================================================================
procedure A_VolcBallImpact(ball: Pmobj_t);

//==============================================================================
//
// A_SkullPop
//
//==============================================================================
procedure A_SkullPop(actor: Pmobj_t);

//==============================================================================
//
// A_CheckSkullFloor
//
//==============================================================================
procedure A_CheckSkullFloor(actor: Pmobj_t);

//==============================================================================
//
// A_CheckSkullDone
//
//==============================================================================
procedure A_CheckSkullDone(actor: Pmobj_t);

//==============================================================================
//
// A_CheckBurnGone
//
//==============================================================================
procedure A_CheckBurnGone(actor: Pmobj_t);

//==============================================================================
//
// A_FreeTargMobj
//
//==============================================================================
procedure A_FreeTargMobj(mo: Pmobj_t);

//==============================================================================
//
// A_AddPlayerCorpse
//
//==============================================================================
procedure A_AddPlayerCorpse(actor: Pmobj_t);

//==============================================================================
//
// A_FlameSnd
//
//==============================================================================
procedure A_FlameSnd(actor: Pmobj_t);

//==============================================================================
//
// A_HideThing
//
//==============================================================================
procedure A_HideThing(actor: Pmobj_t);

//==============================================================================
//
// A_UnHideThing
//
//==============================================================================
procedure A_UnHideThing(actor: Pmobj_t);

//==============================================================================
//
// A_ContMobjSound
//
//==============================================================================
procedure A_ContMobjSound(actor: Pmobj_t);

//==============================================================================
//
// P_DSparilTeleport
//
//==============================================================================
procedure P_DSparilTeleport(actor: Pmobj_t);

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

//==============================================================================
//
// P_InitMonsters
//
//==============================================================================
procedure P_InitMonsters;

//==============================================================================
//
// P_AddBossSpot
//
//==============================================================================
procedure P_AddBossSpot(x, y: fixed_t; angle: angle_t);

implementation

uses
  d_delphi,
  doomdata,
  s_sound,
  d_player,
  sounddata,
  d_think,
  d_main,
  doomdef,
  g_game,
  i_system,
  info_h,
  info,
  m_rnd,
  p_common,
  p_floor,
  p_friends,
  p_map,
  p_maputl,
  p_setup,
  p_local,
  p_sight,
  p_switch,
  p_tick,
  p_mobj,
  p_pspr_h,
  p_spec,
  p_inter,
  p_sounds,
  p_udmf,
  udmf_spec,
  ps_main,
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

const
  MAX_BOSS_SPOTS = 8;

type
  BossSpot_t = record
    x, y: fixed_t;
    angle: angle_t;
  end;

var
  BossSpotCount: integer;

  BossSpots: array[0..MAX_BOSS_SPOTS - 1] of BossSpot_t;

//----------------------------------------------------------------------------
//
// PROC P_InitMonsters
//
// Called at level load.
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure P_InitMonsters;
begin
  BossSpotCount := 0;
end;

//----------------------------------------------------------------------------
//
// PROC P_AddBossSpot
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure P_AddBossSpot(x, y: fixed_t; angle: angle_t);
begin
  if BossSpotCount = MAX_BOSS_SPOTS then
    I_Error('P_AddBossSpot(): Too many boss spots.');

  BossSpots[BossSpotCount].x := x;
  BossSpots[BossSpotCount].y := y;
  BossSpots[BossSpotCount].angle := angle;
  inc(BossSpotCount);
end;

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

  if dist >= mrange then
  begin
    result := false;
    exit;
  end;

  if not P_CheckSight(actor, actor.target) then
  begin
    result := false;
    exit;
  end;

  if mo.z > actor.z + actor.height then
  begin // Target is higher than the attacker
    result := false;
    exit;
  end
  else if actor.z > mo.z + mo.height then
  begin // Attacker is higher
    result := false;
    exit;
  end;

  result := true;

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

  if actor.flags4_ex and MF4_EX_SHORTMRANGE <> 0 then
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

//
// P_Move
// Move in the current direction,
// returns false if the move is blocked.
//
const
  MAXSPECIALCROSS = 8;

//==============================================================================
//
// P_Move
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
    I_Error('P_Move(): Weird actor.movedir = %d', [Ord(actor.movedir)]);

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
// in its current (ob.moveangle) direction.
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

  function _weaponinfo(const p: Pplayer_t): Pweaponinfo_t;
  begin
    if p.powers[Ord(pw_weaponlevel2)] <> 0 then
      result := @wpnlev2info[Ord(p.readyweapon)]
    else
      result := @wpnlev1info[Ord(p.readyweapon)];
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
        (((target.info.missilestate = 0) and (dist < P_MeleeRange(target) * 2)) or
         ((target.player <> nil) and (dist < P_MeleeRange(target) * 3) and
          (_weaponinfo(target.player).mbf21bits and WPF_FLEEMELEE <> 0))) then
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

    if player.mo.flags and MF_SHADOW <> 0 then
      if actor.flags2_ex and MF2_EX_SEEINVISIBLE = 0 then
      begin // Player is invisible
        if (P_AproxDistance(player.mo.x - actor.x, player.mo.y - actor.y) > 2 * MELEERANGE) and
           (P_AproxDistance(player.mo.momx, player.mo.momy) < 5 * FRACUNIT) then
        begin // Player is sneaking - can't detect
          result := false;
          exit;
        end;

        if P_Random < 225 then
        begin // Player isn't sneaking, but still didn't detect
          result := false;
          exit;
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
// ACTION ROUTINES
//
// A_Look
// Stay in state until a player is sighted.
//
//==============================================================================
procedure A_Look(actor: Pmobj_t);
var
  targ: Pmobj_t;
  seeyou: boolean;
begin
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

  A_SeeSound1(actor);
  // go into chase state
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
    A_AttackSound1(actor);
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
  if P_Random < 3 then
    A_ActiveSound1(actor);
end;

//==============================================================================
//
// A_Chase
//
//==============================================================================
procedure A_Chase(actor: Pmobj_t);
begin
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
// A_Pain
//
//==============================================================================
procedure A_Pain(actor: Pmobj_t);
begin
  A_PainSound1(actor);
end;

//----------------------------------------------------------------------------
//
// PROC A_DripBlood
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure A_DripBlood(actor: Pmobj_t);
var
  mo: Pmobj_t;
begin
  mo := P_SpawnMobj(actor.x + ((P_Random-P_Random) shl 11),
                    actor.y + ((P_Random - P_Random) shl 11),
                    actor.z, Ord(MT_BLOOD));
  mo.momx := (P_Random - P_Random) shl 10;
  mo.momy := (P_Random - P_Random) shl 10;
  mo.flags2 := mo.flags2 or MF2_LOGRAV;
end;

//----------------------------------------------------------------------------
//
// PROC A_KnightAttack
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure A_KnightAttack(actor: Pmobj_t);
begin
  if actor.target = nil then
    exit;

  if P_CheckMeleeRange(actor) then
  begin
    P_DamageMobj(actor.target, actor, actor, HITDICE(3));
    S_StartSound(actor, Ord(sfx_kgtat2));
    exit;
  end;
  A_AttackSound1(actor);
  // Throw axe
  if (actor._type = Ord(MT_KNIGHTGHOST)) or (P_Random < 40) then
  begin // Red axe
    P_SpawnMissile(actor, actor.target, Ord(MT_REDAXE));
    exit;
  end;
  // Green axe
  P_SpawnMissile(actor, actor.target, Ord(MT_KNIGHTAXE));
end;

//----------------------------------------------------------------------------
//
// PROC A_ImpExplode
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure A_ImpExplode(actor: Pmobj_t);
var
  mo: Pmobj_t;
begin
  mo := P_SpawnMobj(actor.x, actor.y, actor.z, Ord(MT_IMPCHUNK1));
  mo.momx := (P_Random - P_Random ) shl 10;
  mo.momy := (P_Random - P_Random ) shl 10;
  mo.momz := 9 * FRACUNIT;
  mo := P_SpawnMobj(actor.x, actor.y, actor.z, Ord(MT_IMPCHUNK2));
  mo.momx := (P_Random - P_Random ) shl 10;
  mo.momy := (P_Random - P_Random ) shl 10;
  mo.momz := 9 * FRACUNIT;
  if actor.special1 = 666 then
  begin // Extreme death crash
    P_SetMobjState(actor, S_IMP_XCRASH1);
  end;
end;

//----------------------------------------------------------------------------
//
// PROC A_BeastPuff
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure A_BeastPuff(actor: Pmobj_t);
begin
  if P_Random > 64 then
  begin
    P_SpawnMobj(actor.x + ((P_Random-P_Random) shl 10),
                actor.y + ((P_Random-P_Random) shl 10),
                actor.z + ((P_Random-P_Random) shl 10), Ord(MT_PUFFY));
  end;
end;

//----------------------------------------------------------------------------
//
// PROC A_ImpMeAttack
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure A_ImpMeAttack(actor: Pmobj_t);
begin
  if actor.target = nil then
    exit;

  A_AttackSound1(actor);
  if P_CheckMeleeRange(actor) then
    P_DamageMobj(actor.target, actor, actor, 5 + (P_Random and 7));
end;

//----------------------------------------------------------------------------
//
// PROC A_ImpMsAttack
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure A_ImpMsAttack(actor: Pmobj_t);
var
  dest: Pmobj_t;
  an: angle_t;
  dist: integer;
begin
  if (actor.target = nil) or (P_Random > 64) then
  begin
    P_SetMobjState(actor, statenum_t(actor.info.seestate));
    exit;
  end;

  dest := actor.target;
  actor.flags := actor.flags or MF_SKULLFLY;
  A_AttackSound1(actor);
  A_FaceTarget(actor);
  an := actor.angle  shr ANGLETOFINESHIFT;
  actor.momx := FixedMul(12 * FRACUNIT, finecosine[an]);
  actor.momy := FixedMul(12 * FRACUNIT, finesine[an]);
  dist := P_AproxDistance(dest.x - actor.x, dest.y - actor.y);
  dist := dist div (12 * FRACUNIT);
  if dist < 1 then
    dist := 1;
  actor.momz := (dest.z + (dest.height shr 1) - actor.z) div dist;
end;

//----------------------------------------------------------------------------
//
// PROC A_ImpMsAttack2
//
// Fireball attack of the imp leader.
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure A_ImpMsAttack2(actor: Pmobj_t);
begin
  if actor.target = nil then
    exit;

  A_AttackSound1(actor);
  if P_CheckMeleeRange(actor) then
  begin
    P_DamageMobj(actor.target, actor, actor, 5 + (P_Random and 7));
    exit;
  end;

  P_SpawnMissile(actor, actor.target, Ord(MT_IMPBALL));
end;

//----------------------------------------------------------------------------
//
// PROC A_ImpDeath
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure A_ImpDeath(actor: Pmobj_t);
begin
  actor.flags := actor.flags and not MF_SOLID;
  actor.flags2 := actor.flags2 or MF2_FOOTCLIP;
  if actor.z <= actor.floorz then
    P_SetMobjState(actor, S_IMP_CRASH1);
end;

//----------------------------------------------------------------------------
//
// PROC A_ImpXDeath1
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure A_ImpXDeath1(actor: Pmobj_t);
begin
  actor.flags := actor.flags and not MF_SOLID;
  actor.flags := actor.flags or MF_NOGRAVITY;
  actor.flags2 := actor.flags2 or MF2_FOOTCLIP;
  actor.special1 := 666; // Flag the crash routine
end;

//----------------------------------------------------------------------------
//
// PROC A_ImpXDeath2
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure A_ImpXDeath2(actor: Pmobj_t);
begin
  actor.flags := actor.flags and not MF_NOGRAVITY;
  if actor.z <= actor.floorz then
    P_SetMobjState(actor, S_IMP_CRASH1);
end;

//----------------------------------------------------------------------------
//
// FUNC P_UpdateChicken
//
// Returns true if the chicken morphs.
//
//----------------------------------------------------------------------------
//
//==============================================================================
function P_UpdateChicken(actor: Pmobj_t; tics: integer): boolean;
var
  fog: Pmobj_t;
  x: fixed_t;
  y: fixed_t;
  z: fixed_t;
  moType: integer;
  mo: Pmobj_t;
  oldChicken: mobj_t;
begin
  actor.special1 := actor.special1 - tics;
  if actor.special1 > 0 then
  begin
    result := false;
    exit;
  end;

  moType := actor.special2;
  x := actor.x;
  y := actor.y;
  z := actor.z;
  oldChicken := actor^;
  P_SetMobjState(actor, S_FREETARGMOBJ);
  mo := P_SpawnMobj(x, y, z, moType);
  if not P_TestMobjLocation(mo) then
  begin // Didn't fit
    P_RemoveMobj(mo);
    mo := P_SpawnMobj(x, y, z, Ord(MT_CHICKEN));
    mo.angle := oldChicken.angle;
    mo.flags := oldChicken.flags;
    mo.health := oldChicken.health;
    mo.target := oldChicken.target;
    mo.special1 := 5 * TICRATE; // Next try in 5 seconds
    mo.special2 := moType;
    result := false;
    exit;
  end;

  mo.angle := oldChicken.angle;
  mo.target := oldChicken.target;
  fog := P_SpawnMobj(x, y, z + TELEFOGHEIGHT, Ord(MT_TFOG));
  S_StartSound(fog, Ord(sfx_telept));
  result := true;
end;

//----------------------------------------------------------------------------
//
// PROC A_ChicAttack
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure A_ChicAttack(actor: Pmobj_t);
begin
  if P_UpdateChicken(actor, 18) then
    exit;

  if actor.target = nil then
    exit;

  if P_CheckMeleeRange(actor) then
    P_DamageMobj(actor.target, actor, actor, 1 + (P_Random and 1));

end;

//----------------------------------------------------------------------------
//
// PROC A_ChicLook
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure A_ChicLook(actor: Pmobj_t);
begin
  if P_UpdateChicken(actor, 10) then
    exit;

  A_Look(actor);
end;

//----------------------------------------------------------------------------
//
// PROC A_ChicChase
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure A_ChicChase(actor: Pmobj_t);
begin
  if P_UpdateChicken(actor, 3) then
    exit;

  A_Chase(actor);
end;

//----------------------------------------------------------------------------
//
// PROC A_ChicPain
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure A_ChicPain(actor: Pmobj_t);
begin
  if P_UpdateChicken(actor, 10) then
    exit;

  A_Pain(actor);
end;

//----------------------------------------------------------------------------
//
// PROC A_Feathers
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure A_Feathers(actor: Pmobj_t);
var
  i: integer;
  count: integer;
  mo: Pmobj_t;
begin
  if actor.health > 0 then
  begin // Pain
    if P_Random < 32 then
      count := 2
    else
      count := 1;
  end
  else
  begin // Death
    count := 5 + (P_Random and 3);
  end;

  for i := 0 to count - 1 do
  begin
    mo := P_SpawnMobj(actor.x, actor.y, actor.z+20 * FRACUNIT, Ord(MT_FEATHER));
    mo.target := actor;
    mo.momx := (P_Random - P_Random) * 256;
    mo.momy := (P_Random - P_Random) * 256;
    mo.momz := FRACUNIT + (P_Random * 512);
    P_SetMobjState(mo, statenum_t(Ord(S_FEATHER1) + (P_Random and 7)));
  end;
end;

//----------------------------------------------------------------------------
//
// PROC A_MummyAttack
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure A_MummyAttack(actor: Pmobj_t);
begin
  if actor.target = nil then
    exit;

  A_AttackSound1(actor);

  if P_CheckMeleeRange(actor) then
  begin
    P_DamageMobj(actor.target, actor, actor, HITDICE(2));
    S_StartSound(actor, Ord(sfx_mumat2));
    exit;
  end;
  S_StartSound(actor, Ord(sfx_mumat1));
end;

//----------------------------------------------------------------------------
//
// PROC A_MummyAttack2
//
// Mummy leader missile attack.
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure A_MummyAttack2(actor: Pmobj_t);
var
  mo: Pmobj_t;
begin
  if actor.target = nil then
    exit;

  if P_CheckMeleeRange(actor) then
  begin
    P_DamageMobj(actor.target, actor, actor, HITDICE(2));
    exit;
  end;

  mo := P_SpawnMissile(actor, actor.target, Ord(MT_MUMMYFX1));
  //mo = P_SpawnMissile(actor, actor.target, MT_EGGFX);
  if mo <> nil then
    mo.special1 := integer(actor.target);
end;

//----------------------------------------------------------------------------
//
// PROC A_MummyFX1Seek
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure A_MummyFX1Seek(actor: Pmobj_t);
begin
  P_SeekerMissile(actor, ANG1 * 10, ANG1 * 20);
end;

//----------------------------------------------------------------------------
//
// PROC A_MummySoul
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure A_MummySoul(mummy: Pmobj_t);
var
  mo: Pmobj_t;
begin
  mo := P_SpawnMobj(mummy.x, mummy.y, mummy.z + 10 * FRACUNIT, Ord(MT_MUMMYSOUL));
  mo.momz := FRACUNIT;
end;

//----------------------------------------------------------------------------
//
// PROC A_Sor1Pain
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure A_Sor1Pain(actor: Pmobj_t);
begin
  actor.special1 := 20; // Number of steps to walk fast
  A_Pain(actor);
end;

//----------------------------------------------------------------------------
//
// PROC A_Sor1Chase
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure A_Sor1Chase(actor: Pmobj_t);
begin
  if actor.special1 <> 0 then
  begin
    dec(actor.special1);
    actor.tics := actor.tics - 3;
  end;
  A_Chase(actor);
end;

//----------------------------------------------------------------------------
//
// PROC A_Srcr1Attack
//
// Sorcerer demon attack.
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure A_Srcr1Attack(actor: Pmobj_t);
var
  mo: Pmobj_t;
  momz: fixed_t;
  angle: angle_t;
begin
  if actor.target = nil then
    exit;

  A_AttackSound1(actor);

  if P_CheckMeleeRange(actor) then
  begin
    P_DamageMobj(actor.target, actor, actor, HITDICE(8));
    exit;
  end;

  if actor.health > (actor.info.spawnhealth div 3) * 2 then
  begin // Spit one fireball
    P_SpawnMissile(actor, actor.target, Ord(MT_SRCRFX1));
  end
  else // Spit three fireballs
  begin
    mo := P_SpawnMissile(actor, actor.target, Ord(MT_SRCRFX1));
    if mo <> nil then
    begin
      momz := mo.momz;
      angle := mo.angle;
      P_SpawnMissileAngle(actor, Ord(MT_SRCRFX1), angle - ANG1 * 3, momz);
      P_SpawnMissileAngle(actor, Ord(MT_SRCRFX1), angle + ANG1 * 3, momz);
    end;
    if actor.health < actor.info.spawnhealth div 3 then
    begin // Maybe attack again
      if actor.special1 <> 0 then
      begin // Just attacked, so don't attack again
        actor.special1 := 0;
      end
      else
      begin // Set state to attack again
        actor.special1 := 1;
        P_SetMobjState(actor, S_SRCR1_ATK4);
      end;
    end;
  end;
end;

//----------------------------------------------------------------------------
//
// PROC A_SorcererRise
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure A_SorcererRise(actor: Pmobj_t);
var
  mo: Pmobj_t;
begin
  actor.flags := actor.flags and not MF_SOLID;
  mo := P_SpawnMobj(actor.x, actor.y, actor.z, Ord(MT_SORCERER2));
  P_SetMobjState(mo, S_SOR2_RISE1);
  mo.angle := actor.angle;
  mo.target := actor.target;
end;

//----------------------------------------------------------------------------
//
// PROC P_DSparilTeleport
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure P_DSparilTeleport(actor: Pmobj_t);
var
  i: integer;
  x: fixed_t;
  y: fixed_t;
  prevX: fixed_t;
  prevY: fixed_t;
  prevZ: fixed_t;
  mo: Pmobj_t;
begin
  if BossSpotCount = 0 then
  begin // No spots
    exit;
  end;

  i := P_Random;
  repeat
    inc(i);
    x := BossSpots[i mod BossSpotCount].x;
    y := BossSpots[i mod BossSpotCount].y;
  until P_AproxDistance(actor.x - x, actor.y - y) >= 128 * FRACUNIT;

  prevX := actor.x;
  prevY := actor.y;
  prevZ := actor.z;
  if P_TeleportMove(actor, x, y) then
  begin
    mo := P_SpawnMobj(prevX, prevY, prevZ, Ord(MT_SOR2TELEFADE));
    S_StartSound(mo, Ord(sfx_telept));
    P_SetMobjState(actor, S_SOR2_TELE1);
    S_StartSound(actor, Ord(sfx_telept));
    actor.z := actor.floorz;
    actor.angle := BossSpots[i mod BossSpotCount].angle;
    actor.momx := 0;
    actor.momy := 0;
    actor.momz := 0;
  end;
end;

//----------------------------------------------------------------------------
//
// PROC A_Srcr2Decide
//
//----------------------------------------------------------------------------

const
  Srcr2DecideChance: array[0..8] of integer = (
    192, 120, 120, 120, 64, 64, 32, 16, 0
  );

//==============================================================================
//
// A_Srcr2Decide
//
//==============================================================================
procedure A_Srcr2Decide(actor: Pmobj_t);
begin
  if BossSpotCount = 0 then
  begin // No spots
    exit;
  end;

  if P_Random < Srcr2DecideChance[actor.health div (actor.info.spawnhealth div 8)] then
    P_DSparilTeleport(actor);
end;

//----------------------------------------------------------------------------
//
// PROC A_Srcr2Attack
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure A_Srcr2Attack(actor: Pmobj_t);
var
  chance: integer;
begin
  if actor.target = nil then
    exit;

  A_AttackSound1(actor);

  if P_CheckMeleeRange(actor) then
  begin
    P_DamageMobj(actor.target, actor, actor, HITDICE(20));
    exit;
  end;

  if actor.health < actor.info.spawnhealth div 2 then
    chance := 96
  else
    chance := 48;
  if P_Random < chance then
  begin // Wizard spawners
    P_SpawnMissileAngle(actor, Ord(MT_SOR2FX2), actor.angle - ANG45, FRACUNIT div 2);
    P_SpawnMissileAngle(actor, Ord(MT_SOR2FX2), actor.angle + ANG45, FRACUNIT div 2);
  end
  else
  begin // Blue bolt
    P_SpawnMissile(actor, actor.target, Ord(MT_SOR2FX1));
  end;
end;

//----------------------------------------------------------------------------
//
// PROC A_BlueSpark
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure A_BlueSpark(actor: Pmobj_t);
var
  i: integer;
  mo: Pmobj_t;
begin
  for i := 0 to 1 do
  begin
    mo := P_SpawnMobj(actor.x, actor.y, actor.z, Ord(MT_SOR2FXSPARK));
    mo.momx := (P_Random - P_Random) * 512;
    mo.momy := (P_Random - P_Random) * 512;
    mo.momz := FRACUNIT + (P_Random * 256);
  end;
end;

//----------------------------------------------------------------------------
//
// PROC A_GenWizard
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure A_GenWizard(actor: Pmobj_t);
var
  mo: Pmobj_t;
  fog: Pmobj_t;
begin
  mo := P_SpawnMobj(actor.x, actor.y, actor.z - mobjinfo[Ord(MT_WIZARD)].height div 2, Ord(MT_WIZARD));
  if not P_TestMobjLocation(mo) then
  begin // Didn't fit
    P_RemoveMobj(mo);
    exit;
  end;
  actor.momx := 0;
  actor.momy := 0;
  actor.momz := 0;
  P_SetMobjState(actor, statenum_t(mobjinfo[actor._type].deathstate));
  actor.flags := actor.flags and not MF_MISSILE;
  fog := P_SpawnMobj(actor.x, actor.y, actor.z, Ord(MT_TFOG));
  S_StartSound(fog, Ord(sfx_telept));
end;

//----------------------------------------------------------------------------
//
// PROC A_Sor2DthInit
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure A_Sor2DthInit(actor: Pmobj_t);
begin
  actor.special1 := 7; // Animation loop counter
  P_Massacre; // Kill monsters early
end;

//----------------------------------------------------------------------------
//
// PROC A_Sor2DthLoop
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure A_Sor2DthLoop(actor: Pmobj_t);
begin
  dec(actor.special1);
  if actor.special1 <> 0 then // JVAL check!
    P_SetMobjState(actor, S_SOR2_DIE4);
end;

//==============================================================================
// A_SorZap
//
//----------------------------------------------------------------------------
//
// D'Sparil Sound Routines
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure A_SorZap(actor: Pmobj_t);
begin
  S_StartSound(nil, Ord(sfx_sorzap));
end;

//==============================================================================
//
// A_SorRise
//
//==============================================================================
procedure A_SorRise(actor: Pmobj_t);
begin
  S_StartSound(nil, Ord(sfx_sorrise));
end;

//==============================================================================
//
// A_SorDSph
//
//==============================================================================
procedure A_SorDSph(actor: Pmobj_t);
begin
  S_StartSound(nil, Ord(sfx_sordsph));
end;

//==============================================================================
//
// A_SorDExp
//
//==============================================================================
procedure A_SorDExp(actor: Pmobj_t);
begin
  S_StartSound(nil, Ord(sfx_sordexp));
end;

//==============================================================================
//
// A_SorDBon
//
//==============================================================================
procedure A_SorDBon(actor: Pmobj_t);
begin
  S_StartSound(nil, Ord(sfx_sordbon));
end;

//==============================================================================
//
// A_SorSightSnd
//
//==============================================================================
procedure A_SorSightSnd(actor: Pmobj_t);
begin
  S_StartSound(nil, Ord(sfx_sorsit));
end;

//----------------------------------------------------------------------------
//
// PROC A_MinotaurAtk1
//
// Melee attack.
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure A_MinotaurAtk1(actor: Pmobj_t);
begin
  if actor.target = nil then
    exit;

  S_StartSound(actor, Ord(sfx_stfpow));
  if P_CheckMeleeRange(actor) then
  begin
    P_DamageMobj(actor.target, actor, actor, HITDICE(4));
    if actor.target.player <> nil then // Squish the player
      Pplayer_t(actor.target.player).deltaviewheight := -16 * FRACUNIT;
  end;
end;

//----------------------------------------------------------------------------
//
// PROC A_MinotaurDecide
//
// Choose a missile attack.
//
//----------------------------------------------------------------------------

const
  MNTR_CHARGE_SPEED = 13 * FRACUNIT;

//==============================================================================
//
// A_MinotaurDecide
//
//==============================================================================
procedure A_MinotaurDecide(actor: Pmobj_t);
var
  angle: angle_t;
  target: Pmobj_t;
  dist: integer;
begin
  target := actor.target;
  if target = nil then
    exit;

  S_StartSound(actor, Ord(sfx_minsit));
  dist := P_AproxDistance(actor.x - target.x, actor.y - target.y);
  if (target.z + target.height > actor.z) and
     (target.z + target.height < actor.z + actor.height) and
     (dist < 8 * 64 * FRACUNIT) and
     (dist > 1 * 64 * FRACUNIT) and
     (P_Random < 150) then
  begin // Charge attack
    // Don't call the state function right away
    P_SetMobjStateNF(actor, S_MNTR_ATK4_1);
    actor.flags := actor.flags or MF_SKULLFLY;
    A_FaceTarget(actor);
    angle := actor.angle shr ANGLETOFINESHIFT;
    actor.momx := FixedMul(MNTR_CHARGE_SPEED, finecosine[angle]);
    actor.momy := FixedMul(MNTR_CHARGE_SPEED, finesine[angle]);
    actor.special1 := 17; // Charge duration
  end
  else if (target.z = target.floorz) and
          (dist < 9 * 64 * FRACUNIT) and
          (P_Random < 220) then
  begin // Floor fire attack
    P_SetMobjState(actor, S_MNTR_ATK3_1);
    actor.special2 := 0;
  end
  else // Swing attack
    A_FaceTarget(actor);
    // Don't need to call P_SetMobjState because the current state
    // falls through to the swing attack
end;

//----------------------------------------------------------------------------
//
// PROC A_MinotaurCharge
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure A_MinotaurCharge(actor: Pmobj_t);
var
  puff: Pmobj_t;
begin
  if actor.special1 <> 0 then // JVAL: maybe > 0, not <> 0
  begin
    puff := P_SpawnMobj(actor.x, actor.y, actor.z, Ord(MT_PHOENIXPUFF));
    puff.momz := 2 * FRACUNIT;
    dec(actor.special1);
  end
  else
  begin
    actor.flags := actor.flags and not MF_SKULLFLY;
    P_SetMobjState(actor, statenum_t(actor.info.seestate));
  end;
end;

//----------------------------------------------------------------------------
//
// PROC A_MinotaurAtk2
//
// Swing attack.
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure A_MinotaurAtk2(actor: Pmobj_t);
var
  mo: Pmobj_t;
  angle: angle_t;
  momz: fixed_t;
begin
  if actor.target = nil then
    exit;

  S_StartSound(actor, Ord(sfx_minat2));
  if P_CheckMeleeRange(actor) then
  begin
    P_DamageMobj(actor.target, actor, actor, HITDICE(5));
    exit;
  end;
  mo := P_SpawnMissile(actor, actor.target, Ord(MT_MNTRFX1));
  if mo <> nil then
  begin
    S_StartSound(mo, Ord(sfx_minat2));
    momz := mo.momz;
    angle := mo.angle;
    P_SpawnMissileAngle(actor, Ord(MT_MNTRFX1), angle - (ANG45 div 8), momz);
    P_SpawnMissileAngle(actor, Ord(MT_MNTRFX1), angle + (ANG45 div 8), momz);
    P_SpawnMissileAngle(actor, Ord(MT_MNTRFX1), angle - (ANG45 div 16), momz);
    P_SpawnMissileAngle(actor, Ord(MT_MNTRFX1), angle + (ANG45 div 16), momz);
  end;
end;

//----------------------------------------------------------------------------
//
// PROC A_MinotaurAtk3
//
// Floor fire attack.
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure A_MinotaurAtk3(actor: Pmobj_t);
var
  mo: Pmobj_t;
begin
  if actor.target = nil then
    exit;

  if P_CheckMeleeRange(actor) then
  begin
    P_DamageMobj(actor.target, actor, actor, HITDICE(5));
    if actor.target.player <> nil then // Squish the player
      Pplayer_t(actor.target.player).deltaviewheight := -16 * FRACUNIT;
  end
  else
  begin
    mo := P_SpawnMissile(actor, actor.target, Ord(MT_MNTRFX2));
    if mo <> nil then
      S_StartSound(mo, Ord(sfx_minat1));
  end;
  if (P_Random < 192) and (actor.special2 = 0) then
  begin
    P_SetMobjState(actor, S_MNTR_ATK3_4);
    actor.special2 := 1;
  end;
end;

//----------------------------------------------------------------------------
//
// PROC A_MntrFloorFire
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure A_MntrFloorFire(actor: Pmobj_t);
var
  mo: Pmobj_t;
begin
  actor.z := actor.floorz;
  mo := P_SpawnMobj(actor.x + ((P_Random - P_Random) * 1024), actor.y + ((P_Random - P_Random) * 1024), ONFLOORZ, Ord(MT_MNTRFX3));
  mo.target := actor.target;
  mo.momx := 1; // Force block checking
  P_CheckMissileSpawn(mo);
end;

//----------------------------------------------------------------------------
//
// PROC A_BeastAttack
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure A_BeastAttack(actor: Pmobj_t);
begin
  if actor.target = nil then
    exit;

  A_AttackSound1(actor);

  if P_CheckMeleeRange(actor) then
    P_DamageMobj(actor.target, actor, actor, HITDICE(3))
  else
    P_SpawnMissile(actor, actor.target, Ord(MT_BEASTBALL));
end;

//----------------------------------------------------------------------------
//
// PROC A_HeadAttack
//
//----------------------------------------------------------------------------

const
  atkResolve1: array[0..1] of integer = (50, 150);
  atkResolve2: array[0..1] of integer = (150, 200);

//==============================================================================
//
// A_HeadAttack
//
//==============================================================================
procedure A_HeadAttack(actor: Pmobj_t);
var
  i: integer;
  fire: Pmobj_t;
  baseFire: Pmobj_t;
  mo: Pmobj_t;
  target: Pmobj_t;
  randAttack: integer;
  dist: integer;
begin
  // Ice ball    (close 20% : far 60%)
  // Fire column  (close 40% : far 20%)
  // Whirlwind  (close 40% : far 20%)
  // Distance threshold = 8 cells

  target := actor.target;
  if target = nil then
    exit;

  A_FaceTarget(actor);
  if P_CheckMeleeRange(actor) then
  begin
    P_DamageMobj(target, actor, actor, HITDICE(6));
    exit;
  end;

  dist := P_AproxDistance(actor.x - target.x, actor.y - target.y);
  if dist > 8 * 64 * FRACUNIT then
    dist := 1
  else
    dist := 0;
  randAttack := P_Random;
  if randAttack < atkResolve1[dist] then
  begin // Ice ball
    P_SpawnMissile(actor, target, Ord(MT_HEADFX1));
    S_StartSound(actor, Ord(sfx_hedat2));
  end
  else if randAttack < atkResolve2[dist] then
  begin // Fire column
    baseFire := P_SpawnMissile(actor, target, Ord(MT_HEADFX3));
    if baseFire <> nil then
    begin
      P_SetMobjState(baseFire, S_HEADFX3_4); // Don't grow
      for i := 0 to 4 do
      begin
        fire := P_SpawnMobj(baseFire.x, baseFire.y, baseFire.z, Ord(MT_HEADFX3));
        if i = 0 then
          S_StartSound(actor, Ord(sfx_hedat1));
        fire.target := baseFire.target;
        fire.angle := baseFire.angle;
        fire.momx := baseFire.momx;
        fire.momy := baseFire.momy;
        fire.momz := baseFire.momz;
        fire.damage := 0;
        fire.health := (i + 1) * 2;
        P_CheckMissileSpawn(fire);
      end;
    end;
  end
  else
  begin // Whirlwind
    mo := P_SpawnMissile(actor, target, Ord(MT_WHIRLWIND));
    if mo <> nil then
    begin
      mo.z := mo.z - 32 * FRACUNIT;
      mo.special1 := integer(target);
      mo.special2 := 50; // Timer for active sound
      mo.health := 20 * TICRATE; // Duration
      S_StartSound(actor, Ord(sfx_hedat3));
    end;
  end;
end;

//----------------------------------------------------------------------------
//
// PROC A_WhirlwindSeek
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure A_WhirlwindSeek(actor: Pmobj_t);
begin
  actor.health := actor.health - 3;
  if actor.health < 0 then
  begin
    actor.momx := 0;
    actor.momy := 0;
    actor.momz := 0;
    P_SetMobjState(actor, statenum_t(mobjinfo[actor._type].deathstate));
    actor.flags := actor.flags and not MF_MISSILE;
    exit;
  end;

  actor.special2 := actor.special2 - 3;
  if actor.special2 < 0 then
  begin
    actor.special2 := 58 + (P_Random and 31);
    S_StartSound(actor, Ord(sfx_hedat3));
  end;

  if (actor.special1 <> 0) and
     (Pmobj_t(actor.special1).flags and MF_SHADOW <> 0) then
    if actor.flags2_ex and MF2_EX_SEEINVISIBLE = 0 then
      exit;

  P_SeekerMissile(actor, ANG1 * 10, ANG1 * 30);
end;

//----------------------------------------------------------------------------
//
// PROC A_HeadIceImpact
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure A_HeadIceImpact(ice: Pmobj_t);
var
  i: integer;
  angle: angle_t;
  shard: Pmobj_t;
begin
  for i := 0 to 7 do
  begin
    shard := P_SpawnMobj(ice.x, ice.y, ice.z, Ord(MT_HEADFX2));
    angle := i * ANG45;
    shard.target := ice.target;
    shard.angle := angle;
    angle := angle shr ANGLETOFINESHIFT;
    shard.momx := FixedMul(shard.info.speed, finecosine[angle]);
    shard.momy := FixedMul(shard.info.speed, finesine[angle]);
    shard.momz := -39321;
    P_CheckMissileSpawn(shard);
  end;
end;

//----------------------------------------------------------------------------
//
// PROC A_HeadFireGrow
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure A_HeadFireGrow(fire: Pmobj_t);
begin
  fire.z := fire.z + 9 * FRACUNIT;
  dec(fire.health);
  if fire.health = 0 then
  begin
    fire.damage := fire.info.damage;
    P_SetMobjState(fire, S_HEADFX3_4);
  end;
end;

//==============================================================================
// P_SnakeAttack
//
//----------------------------------------------------------------------------
//
// PROC A_SnakeAttack
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure P_SnakeAttack(actor: Pmobj_t; missiletype: integer);
begin
  if actor.target = nil then
  begin
    P_SetMobjState(actor, S_SNAKE_WALK1);
    exit;
  end;

  A_AttackSound1(actor);

  A_FaceTarget(actor);
  P_SpawnMissile(actor, actor.target, missiletype);
end;

//==============================================================================
//
// A_SnakeAttack
//
//==============================================================================
procedure A_SnakeAttack(actor: Pmobj_t);
begin
  P_SnakeAttack(actor, Ord(MT_SNAKEPRO_A));
end;

//----------------------------------------------------------------------------
//
// PROC A_SnakeAttack2
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure A_SnakeAttack2(actor: Pmobj_t);
begin
  P_SnakeAttack(actor, Ord(MT_SNAKEPRO_B));
end;

//----------------------------------------------------------------------------
//
// PROC A_ClinkAttack
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure A_ClinkAttack(actor: Pmobj_t);
var
  damage: integer;
begin
  if actor.target = nil then
    exit;

  A_AttackSound1(actor);

  if P_CheckMeleeRange(actor) then
  begin
    damage := (P_Random mod 7) + 3;
    P_DamageMobj(actor.target, actor, actor, damage);
  end;
end;

//----------------------------------------------------------------------------
//
// PROC A_WizAtk1
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure A_WizAtk1(actor: Pmobj_t);
begin
  A_FaceTarget(actor);
  actor.flags := actor.flags and not MF_SHADOW;
end;

//----------------------------------------------------------------------------
//
// PROC A_WizAtk2
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure A_WizAtk2(actor: Pmobj_t);
begin
  A_FaceTarget(actor);
  actor.flags := actor.flags or MF_SHADOW;
end;

//----------------------------------------------------------------------------
//
// PROC A_WizAtk3
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure A_WizAtk3(actor: Pmobj_t);
var
  mo: Pmobj_t;
  angle: angle_t;
  momz: fixed_t;
begin
  actor.flags := actor.flags and not MF_SHADOW;

  if actor.target = nil then
    exit;

  A_AttackSound1(actor);

  if P_CheckMeleeRange(actor) then
  begin
    P_DamageMobj(actor.target, actor, actor, HITDICE(4));
    exit;
  end;

  mo := P_SpawnMissile(actor, actor.target, Ord(MT_WIZFX1));
  if mo <> nil then
  begin
    momz := mo.momz;
    angle := mo.angle;
    P_SpawnMissileAngle(actor, Ord(MT_WIZFX1), angle - (ANG45 div 8), momz);
    P_SpawnMissileAngle(actor, Ord(MT_WIZFX1), angle + (ANG45 div 8), momz);
  end;
end;

//----------------------------------------------------------------------------
//
// PROC A_Scream
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure A_Scream(actor: Pmobj_t);
begin
  case actor._type of
    Ord(MT_CHICPLAYER),
    Ord(MT_SORCERER1),
    Ord(MT_MINOTAUR):
      begin
        // Make boss death sounds full volume
        A_DeathSound(actor, nil);
      end;
    Ord(MT_PLAYER):
      begin
        // Handle the different player death screams
        if actor.special1 < 10 then
        begin // Wimpy death sound
          S_StartSound(actor, Ord(sfx_plrwdth));
        end
        else if actor.health > -50 then
        begin // Normal death sound
          A_DeathSound(actor, nil);
        end
        else if actor.health > -100 then
        begin // Crazy death sound
          S_StartSound(actor, Ord(sfx_plrcdth));
        end
        else
        begin // Extreme death sound
          S_StartSound(actor, Ord(sfx_gibdth));
        end
      end;
    else if actor.info.deathsound <> 0 then
      A_DeathSound(actor, nil);
  end
end;

//---------------------------------------------------------------------------
//
// PROC P_DropItem
//
//---------------------------------------------------------------------------
//
//==============================================================================
procedure P_DropItem(source: Pmobj_t; _type: integer; special: integer; chance: integer);
var
  mo: Pmobj_t;
begin
  if P_Random > chance then
    exit;

  mo := P_SpawnMobj(source.x, source.y, source.z + (source.height div 2), _type);
  mo.momx := (P_Random - P_Random) * 256;
  mo.momy := (P_Random - P_Random) * 256;
  mo.momz := FRACUNIT * 5 + (P_Random * 1024);
  mo.flags := mo.flags or MF_DROPPED;
  mo.health := special;
end;

//----------------------------------------------------------------------------
//
// PROC A_NoBlocking
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure A_NoBlocking(actor: Pmobj_t);
begin
  actor.flags := actor.flags and not MF_SOLID;
  // Check for monsters dropping things
  case actor._type of
    Ord(MT_MUMMY),
    Ord(MT_MUMMYLEADER),
    Ord(MT_MUMMYGHOST),
    Ord(MT_MUMMYLEADERGHOST):
      P_DropItem(actor, Ord(MT_AMGWNDWIMPY), 3, 84);
    Ord(MT_KNIGHT),
    Ord(MT_KNIGHTGHOST):
      P_DropItem(actor, Ord(MT_AMCBOWWIMPY), 5, 84);
    Ord(MT_WIZARD):
      begin
        P_DropItem(actor, Ord(MT_AMBLSRWIMPY), 10, 84);
        P_DropItem(actor, Ord(MT_ARTITOMEOFPOWER), 0, 4);
      end;
    Ord(MT_HEAD):
      begin
        P_DropItem(actor, Ord(MT_AMBLSRWIMPY), 10, 84);
        P_DropItem(actor, Ord(MT_ARTIEGG), 0, 51);
      end;
    Ord(MT_BEAST):
      P_DropItem(actor, Ord(MT_AMCBOWWIMPY), 10, 84);
    Ord(MT_CLINK):
      P_DropItem(actor, Ord(MT_AMSKRDWIMPY), 20, 84);
    Ord(MT_SNAKE):
      P_DropItem(actor, Ord(MT_AMPHRDWIMPY), 5, 84);
    Ord(MT_MINOTAUR):
      begin
        P_DropItem(actor, Ord(MT_ARTISUPERHEAL), 0, 51);
        P_DropItem(actor, Ord(MT_AMPHRDWIMPY), 10, 84);
      end;
  end;
end;

//----------------------------------------------------------------------------
//
// PROC A_Explode
//
// Handles a bunch of exploding things.
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure A_Explode(actor: Pmobj_t);
var
  damage: integer;
begin
  if actor.flags_ex and MF_EX_CUSTOMEXPLODE <> 0 then
    P_RadiusAttackEx(actor, actor.target, actor.info.explosiondamage, actor.info.explosionradius)
  else if actor.state.params <> nil then
    P_RadiusAttackEx(actor, actor.target, actor.state.params.IntVal[0], actor.state.params.IntVal[1])
  else
  begin
    damage := 128;
    case actor._type of
      Ord(MT_FIREBOMB): // Time Bombs
        begin
          actor.z := actor.z + 32 * FRACUNIT;
          actor.flags := actor.flags and not MF_SHADOW;
        end;
      Ord(MT_MNTRFX2): // Minotaur floor fire
        begin
          damage := 24;
        end;
      Ord(MT_SOR2FX1): // D'Sparil missile
        begin
          damage := 80 + (P_Random and 31);
        end;
    end;
    P_RadiusAttack(actor, actor.target, damage);
    P_HitFloor(actor);
  end;
end;

//----------------------------------------------------------------------------
//
// PROC A_PodPain
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure A_PodPain(actor: Pmobj_t);
var
  i: integer;
  count: integer;
  chance: integer;
  goo: Pmobj_t;
begin
  chance := P_Random;
  if chance < 128 then
    exit;

  if chance > 240 then
    count := 2
  else
    count := 1;

  for i := 0 to count - 1 do
  begin
    goo := P_SpawnMobj(actor.x, actor.y, actor.z + 48 * FRACUNIT, Ord(MT_PODGOO));
    goo.target := actor;
    goo.momx := (P_Random - P_Random) * 512;
    goo.momy := (P_Random - P_Random) * 512;
    goo.momz := FRACUNIT div 2 + (P_Random * 512);
  end;
end;

//----------------------------------------------------------------------------
//
// PROC A_RemovePod
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure A_RemovePod(actor: Pmobj_t);
var
  mo: Pmobj_t;
begin
  if actor.special2 <> 0 then
  begin
    mo := Pmobj_t(actor.special2);
    if mo.special1 > 0 then
      dec(mo.special1);
  end;
end;

//----------------------------------------------------------------------------
//
// PROC A_MakePod
//
//----------------------------------------------------------------------------

const
  MAX_GEN_PODS = 16;

//==============================================================================
//
// A_MakePod
//
//==============================================================================
procedure A_MakePod(actor: Pmobj_t);
var
  mo: Pmobj_t;
  x: fixed_t;
  y: fixed_t;
begin
  if actor.special1 >= MAX_GEN_PODS then
  begin // Too many generated pods
    exit;
  end;

  x := actor.x;
  y := actor.y;
  mo := P_SpawnMobj(x, y, ONFLOORZ, Ord(MT_POD));
  if not P_CheckPosition(mo, x, y) then
  begin // Didn't fit
    P_RemoveMobj(mo);
    exit;
  end;

  P_SetMobjState(mo, S_POD_GROW1);
  P_ThrustMobj(mo, LongWord(P_Random) * $1000000, $48000);
  S_StartSound(mo, Ord(sfx_newpod));
  inc(actor.special1); // Increment generated pod count
  mo.special2 := integer(actor); // Link the generator to the pod
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
      if (mo.flags and MF_COUNTKILL <> 0) and (mo.health > 0) then
      begin
        mo.flags_ex := mo.flags_ex and not MF_EX_INVULNERABLE;
        P_DamageMobj(mo, nil, nil, 10000);
      end;
    end;
   think := think.next;
  end;
end;

//----------------------------------------------------------------------------
//
// PROC A_BossDeath
//
// Trigger special effects if all bosses are dead.
//
//----------------------------------------------------------------------------

const
  bossType: array[0..5] of integer = (
    Ord(MT_HEAD),
    Ord(MT_MINOTAUR),
    Ord(MT_SORCERER2),
    Ord(MT_HEAD),
    Ord(MT_MINOTAUR),
    -1
  );

//==============================================================================
//
// A_BossDeath
//
//==============================================================================
procedure A_BossDeath(actor: Pmobj_t);
var
  mo, mo2: Pmobj_t;
  think: Pthinker_t;
  dummyLine: line_t;
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
      if gamemapinfo.bossactions[i].typ = actor._type then
        break;
      Inc(i);
    end;

    if i >= gamemapinfo.numbossactions then
      exit; // no matches found

    // scan the remaining thinkers to see
    // if all bosses are dead
    think := thinkercap.next;
    while think <> @thinkercap do
    begin
      if @think._function.acp1 = @P_MobjThinker then
      begin
        mo2 := Pmobj_t(think);
        if (mo2 <> actor) and (mo2._type = actor._type) and (mo2.health > 0) then
        begin
          // other boss not dead
          exit;
        end;
      end;
      think := think.next;
    end;

    for i := 0 to gamemapinfo.numbossactions - 1 do
      if gamemapinfo.bossactions[i].typ = actor._type then
      begin
        dummyLine := lines[0];
        dummyLine.special := gamemapinfo.bossactions[i].special;
        dummyLine.tag := gamemapinfo.bossactions[i].tag;
        // use special semantics for line activation to block problem types.
        if not P_UseSpecialLine(actor, @dummyLine, 0, true) then
          P_CrossSpecialLinePtr(@dummyLine, 0, actor);
      end;

    exit;
  end;

  if gamemap = 7 then
  begin
    if actor.flags4_ex and MF4_EX_MAP07BOSS = 0 then
      exit;
    if actor.flags4_ex and MF4_EX_MAP07BOSS1 <> 0 then
    begin
      dummyLine.tag := 666;
      EV_DoFloor(@dummyLine, lowerFloorToLowest);
    end
    else if actor.flags4_ex and MF4_EX_MAP07BOSS2 <> 0 then
    begin
      dummyLine.tag := 667;
      EV_DoFloor(@dummyLine, raiseToTexture);
    end;
    exit;
  end;

  if gamemap <> 8 then  // Not a boss level
    exit;

  if actor._type <> bossType[gameepisode - 1] then // Not considered a boss in this episode
    exit;

  // Make sure all bosses are dead
  think := thinkercap.next;
  while think <> @thinkercap do
  begin
    if @think._function.acp1 = @P_MobjThinker then
    begin
      mo := Pmobj_t(think);
      if (mo <> actor) and (mo._type = actor._type) and (mo.health > 0) then // Found a living boss
        exit;
    end;
    think := think.next;
  end;

  if gameepisode > 1 then // Kill any remaining monsters
    P_Massacre;

  dummyLine.tag := 666;
  EV_DoFloor(@dummyLine, lowerFloor);
end;

//----------------------------------------------------------------------------
//
// PROC A_ESound
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure A_ESound(mo: Pmobj_t);
var
  sound: integer;
begin
  case mo._type of
    Ord(MT_SOUNDWATERFALL):
      sound := Ord(sfx_waterfl);
    ord(MT_SOUNDWIND):
      sound := Ord(sfx_wind);
  else
    exit;
  end;
  S_StartSound(mo, sound);
end;

//==============================================================================
// P_SpawnTeleGlitter
//
//----------------------------------------------------------------------------
//
// PROC A_SpawnTeleGlitter
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure P_SpawnTeleGlitter(actor: Pmobj_t; _type: integer);
var
  mo: Pmobj_t;
begin
  mo := P_SpawnMobj(actor.x + ((P_Random and 31) - 16) * FRACUNIT,
                    actor.y + ((P_Random and 31) - 16) * FRACUNIT,
                    Psubsector_t(actor.subsector).sector.floorheight, _type);
  if mo <> nil then // JVAL: should never happen
    mo.momz := FRACUNIT div 4;
end;

//==============================================================================
//
// A_SpawnTeleGlitter
//
//==============================================================================
procedure A_SpawnTeleGlitter(actor: Pmobj_t);
begin
  P_SpawnTeleGlitter(actor, Ord(MT_TELEGLITTER));
end;

//----------------------------------------------------------------------------
//
// PROC A_SpawnTeleGlitter2
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure A_SpawnTeleGlitter2(actor: Pmobj_t);
begin
  P_SpawnTeleGlitter(actor, Ord(MT_TELEGLITTER2));
end;

//----------------------------------------------------------------------------
//
// PROC A_AccTeleGlitter
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure A_AccTeleGlitter(actor: Pmobj_t);
begin
  inc(actor.health);

  if actor.health > 35 then
    actor.momz := actor.momz + actor.momz div 2;
end;

//----------------------------------------------------------------------------
//
// PROC A_InitKeyGizmo
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure A_InitKeyGizmo(gizmo: Pmobj_t);
var
  mo: Pmobj_t;
  state: statenum_t;
begin
  case gizmo._type of
    Ord(MT_KEYGIZMOBLUE):
      state := S_KGZ_BLUEFLOAT1;
    Ord(MT_KEYGIZMOGREEN):
      state := S_KGZ_GREENFLOAT1;
    ord(MT_KEYGIZMOYELLOW):
      state := S_KGZ_YELLOWFLOAT1;
  else
    exit;
  end;

  mo := P_SpawnMobj(gizmo.x, gizmo.y, gizmo.z + 60 * FRACUNIT, Ord(MT_KEYGIZMOFLOAT));
  P_SetMobjState(mo, state);
end;

//----------------------------------------------------------------------------
//
// PROC A_VolcanoSet
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure A_VolcanoSet(volcano: Pmobj_t);
begin
  volcano.tics := 105 + (P_Random and 127);
end;

//----------------------------------------------------------------------------
//
// PROC A_VolcanoBlast
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure A_VolcanoBlast(volcano: Pmobj_t);
var
  i: integer;
  count: integer;
  blast: Pmobj_t;
  angle: angle_t;
begin
  count := 1 + (P_Random mod 3);
  for i := 0 to count - 1 do
  begin
    blast := P_SpawnMobj(volcano.x, volcano.y, volcano.z + 44 * FRACUNIT, Ord(MT_VOLCANOBLAST)); // MT_VOLCANOBLAST
    if blast <> nil then // JVAL: should never happen
    begin
      blast.target := volcano;
      angle := LongWord(P_Random) * $1000000;
      blast.angle := angle;
      angle := angle shr ANGLETOFINESHIFT;
      blast.momx := FixedMul(FRACUNIT, finecosine[angle]);
      blast.momy := FixedMul(FRACUNIT, finesine[angle]);
      blast.momz := $28000 + (P_Random * 1024);
      S_StartSound(blast, Ord(sfx_volsht));
      P_CheckMissileSpawn(blast);
    end;
  end;
end;

//----------------------------------------------------------------------------
//
// PROC A_VolcBallImpact
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure A_VolcBallImpact(ball: Pmobj_t);
var
  i: integer;
  tiny: Pmobj_t;
  angle: angle_t;
begin
  if ball.z <= ball.floorz then
  begin
    ball.flags := ball.flags or MF_NOGRAVITY;
    ball.flags2 := ball.flags2 and not MF2_LOGRAV;
    ball.z := ball.z + 28 * FRACUNIT;
    //ball.momz = 3*FRACUNIT;
  end;
  P_RadiusAttack(ball, ball.target, 25);
  for i := 0 to 3 do
  begin
    tiny := P_SpawnMobj(ball.x, ball.y, ball.z, Ord(MT_VOLCANOTBLAST));
    if tiny <> nil then // JVAL: Should never happen
    begin
      tiny.target := ball;
      angle := i * ANG90;
      tiny.angle := angle;
      angle := angle shr ANGLETOFINESHIFT;
      tiny.momx := FixedMul(45875, finecosine[angle]);
      tiny.momy := FixedMul(45875, finesine[angle]);
      tiny.momz := FRACUNIT + (P_Random * 512);
      P_CheckMissileSpawn(tiny);
    end;
  end;
end;

//----------------------------------------------------------------------------
//
// PROC A_SkullPop
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure A_SkullPop(actor: Pmobj_t);
var
  mo: Pmobj_t;
  player: Pplayer_t;
begin
  actor.flags := actor.flags and not MF_SOLID;
  mo := P_SpawnMobj(actor.x, actor.y, actor.z + 48 * FRACUNIT, Ord(MT_BLOODYSKULL));
  if mo <> nil then
  begin
    //mo.target = actor;
    mo.momx := (P_Random - P_Random) * 512;
    mo.momy := (P_Random - P_Random) * 512;
    mo.momz := FRACUNIT * 2 + (P_Random * 64);
    // Attach player mobj to bloody skull
    player := actor.player;
    actor.player := nil;
    mo.player := player;
    mo.health := actor.health;
    mo.angle := actor.angle;
    if player <> nil then
    begin
      player.mo := mo;
      player.lookdir := 0;
      player.damagecount := 32;
    end;
  end;
end;

//----------------------------------------------------------------------------
//
// PROC A_CheckSkullFloor
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure A_CheckSkullFloor(actor: Pmobj_t);
begin
  if actor.z <= actor.floorz then
    P_SetMobjState(actor, S_BLOODYSKULLX1);
end;

//----------------------------------------------------------------------------
//
// PROC A_CheckSkullDone
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure A_CheckSkullDone(actor: Pmobj_t);
begin
  if actor.special2 = 666 then
    P_SetMobjState(actor, S_BLOODYSKULLX2);
end;

//----------------------------------------------------------------------------
//
// PROC A_CheckBurnGone
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure A_CheckBurnGone(actor: Pmobj_t);
begin
  if actor.special2 = 666 then
    P_SetMobjState(actor, S_PLAY_FDTH20);
end;

//----------------------------------------------------------------------------
//
// PROC A_FreeTargMobj
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure A_FreeTargMobj(mo: Pmobj_t);
begin
  mo.momx := 0;
  mo.momy := 0;
  mo.momz := 0;
  mo.z := mo.ceilingz + 4 * FRACUNIT;
  mo.flags := mo.flags and not (MF_SHOOTABLE or MF_FLOAT or MF_SKULLFLY or MF_SOLID);
  mo.flags := mo.flags or MF_CORPSE or MF_DROPOFF or MF_NOGRAVITY;
  mo.flags2 := mo.flags2 and not (MF2_PASSMOBJ or MF2_LOGRAV);
  mo.player := nil;
end;

//----------------------------------------------------------------------------
//
// PROC A_AddPlayerCorpse
//
//----------------------------------------------------------------------------

const
  BODYQUESIZE = 32;

var
  bodyque: array[0..BODYQUESIZE - 1] of Pmobj_t;
  bodyqueslot: integer;

//==============================================================================
//
// A_AddPlayerCorpse
//
//==============================================================================
procedure A_AddPlayerCorpse(actor: Pmobj_t);
begin
  if bodyqueslot >= BODYQUESIZE then
  begin // Too many player corpses - remove an old one
    P_RemoveMobj(bodyque[bodyqueslot mod BODYQUESIZE]);
  end;

  bodyque[bodyqueslot mod BODYQUESIZE] := actor;
  inc(bodyqueslot);
end;

//----------------------------------------------------------------------------
//
// PROC A_FlameSnd
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure A_FlameSnd(actor: Pmobj_t);
begin
  S_StartSound(actor, Ord(sfx_hedat1)); // Burn sound
end;

//----------------------------------------------------------------------------
//
// PROC A_HideThing
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure A_HideThing(actor: Pmobj_t);
begin
  //P_UnsetThingPosition(actor);
  actor.flags2 := actor.flags2 or MF2_DONTDRAW;
end;

//----------------------------------------------------------------------------
//
// PROC A_UnHideThing
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure A_UnHideThing(actor: Pmobj_t);
begin
  //P_SetThingPosition(actor);
  actor.flags2 := actor.flags2 and not MF2_DONTDRAW;
end;

//---------------------------------------------------------------------------
//
// PROC A_ContMobjSound
//
//---------------------------------------------------------------------------
//
//==============================================================================
procedure A_ContMobjSound(actor: Pmobj_t);
begin
  case actor._type of
    Ord(MT_KNIGHTAXE):
      S_StartSound(actor, Ord(sfx_kgtatk));
    Ord(MT_MUMMYFX1):
      S_StartSound(actor, Ord(sfx_mumhed));
  end;
end;

end.

