//------------------------------------------------------------------------------
//
//  DelphiStrife: A modified and improved Strife source port for Windows.
//
//  Based on:
//    - Linux Doom by "id Software"
//    - Chocolate Strife by "Simon Howard"
//    - DelphiDoom by "Jim Valavanis"
//
//  Copyright (C) 1993-1996 by id Software, Inc.
//  Copyright (C) 2005 Simon Howard
//  Copyright (C) 2010 James Haley, Samuel Villarreal
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
//  Enemy thinking, AI.
//  Action Pointer Functions
//  that are associated with states/frames.
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
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
  sounds;

procedure A_Fall(actor: Pmobj_t);

procedure A_Look(actor: Pmobj_t);

procedure A_Chase(actor: Pmobj_t);

procedure A_FaceTarget(actor: Pmobj_t);

procedure A_BulletAttack(actor: Pmobj_t);

procedure A_Tracer(actor: Pmobj_t);

procedure A_Scream(actor: Pmobj_t);

procedure A_XScream(actor: Pmobj_t);

procedure A_Pain(actor: Pmobj_t);

procedure A_Explode(thingy: Pmobj_t);

procedure A_BossDeath(actor: Pmobj_t);

procedure A_PlayerScream(mo: Pmobj_t);

procedure A_Listen(actor: Pmobj_t);

procedure A_RandomWalk(actor: Pmobj_t);

procedure A_FriendLook(actor: Pmobj_t);

procedure A_PeasantPunch(actor: Pmobj_t);

procedure A_ReaverAttack(actor: Pmobj_t);

procedure A_CheckTargetVisible(actor: Pmobj_t);

procedure A_SentinelAttack(actor: Pmobj_t);

procedure A_StalkerThink(actor: Pmobj_t);

procedure A_StalkerSetLook(actor: Pmobj_t);

procedure A_StalkerDrop(actor: Pmobj_t);

procedure A_StalkerScratch(actor: Pmobj_t);

procedure A_FloatWeave(actor: Pmobj_t);

procedure A_RobotMelee(actor: Pmobj_t);

procedure A_TemplarMauler(actor: Pmobj_t);

procedure A_CrusaderAttack(actor: Pmobj_t);

procedure A_CrusaderLeft(actor: Pmobj_t);

procedure A_CrusaderRight(actor: Pmobj_t);

procedure A_CheckTargetVisible2(actor: Pmobj_t);

procedure A_InqFlyCheck(actor: Pmobj_t);

procedure A_InqGrenade(actor: Pmobj_t);

procedure A_InqTakeOff(actor: Pmobj_t);

procedure A_InqFly(actor: Pmobj_t);

procedure A_FireSigilWeapon(actor: Pmobj_t);

procedure A_ProgrammerAttack(actor: Pmobj_t);

procedure A_Sigil_A_Action(actor: Pmobj_t);

procedure A_SpectreEAttack(actor: Pmobj_t);

procedure A_SpectreCAttack(actor: Pmobj_t);

procedure A_AlertSpectreC(actor: Pmobj_t);

procedure A_Sigil_E_Action(actor: Pmobj_t);

procedure A_SigilTrail(actor: Pmobj_t);

procedure A_SpectreDAttack(actor: Pmobj_t);

procedure A_FireSigilEOffshoot(actor: Pmobj_t);

procedure A_ShadowOff(actor: Pmobj_t);

procedure A_ModifyVisibility(actor: Pmobj_t);

procedure A_ShadowOn(actor: Pmobj_t);

procedure A_SetTLOptions(actor: Pmobj_t);

procedure A_BossMeleeAtk(actor: Pmobj_t);

procedure A_BishopAttack(actor: Pmobj_t);

procedure A_FireHookShot(actor: Pmobj_t);

procedure A_FireChainShot(actor: Pmobj_t);

procedure A_MissileSmoke(actor: Pmobj_t);

procedure A_SpawnSparkPuff(actor: Pmobj_t);

procedure A_ProgrammerMelee(actor: Pmobj_t);

procedure A_PeasantCrash(actor: Pmobj_t);

procedure A_HideZombie(actor: Pmobj_t);

procedure A_MerchantPain(actor: Pmobj_t);

procedure A_ProgrammerDie(actor: Pmobj_t);

procedure A_InqTossArm(actor: Pmobj_t);

procedure A_SpawnSpectreA(actor: Pmobj_t);

procedure A_SpawnSpectreB(actor: Pmobj_t);

procedure A_SpawnSpectreC(actor: Pmobj_t);

procedure A_SpawnSpectreD(actor: Pmobj_t);

procedure A_SpawnSpectreE(actor: Pmobj_t);

procedure A_SpawnEntity(actor: Pmobj_t);

procedure A_EntityDeath(actor: Pmobj_t);

procedure A_SpawnZombie(actor: Pmobj_t);

procedure A_ZombieInSpecialSector(actor: Pmobj_t);

procedure A_CrystalExplode(actor: Pmobj_t);

procedure A_QuestMsg(actor: Pmobj_t);

procedure A_ExtraLightOff(actor: Pmobj_t);

procedure A_CrystalRadiusAtk(actor: Pmobj_t);

procedure A_DeathExplode1(actor: Pmobj_t);

procedure A_DeathExplode2(actor: Pmobj_t);

procedure A_DeathExplode3(actor: Pmobj_t);

procedure A_DeathExplode5(actor: Pmobj_t);

procedure A_RaiseAlarm(actor: Pmobj_t);

procedure A_MissileTick(actor: Pmobj_t);

procedure A_SpawnGrenadeFire(actor: Pmobj_t);

procedure A_NodeChunk(actor: Pmobj_t);

procedure A_HeadChunk(actor: Pmobj_t);

procedure A_BurnSpread(actor: Pmobj_t);

procedure A_AcolyteSpecial(actor: Pmobj_t);

procedure A_InqChase(actor: Pmobj_t);

procedure A_StalkerChase(actor: Pmobj_t);

procedure A_TeleportBeacon(actor: Pmobj_t);

procedure A_BodyParts(actor: Pmobj_t);

procedure A_ClaxonBlare(actor: Pmobj_t);

procedure A_ActiveSoundSTRF(actor: Pmobj_t);

procedure A_ClearSoundTarget(actor: Pmobj_t);

procedure A_DropBurnFlesh(actor: Pmobj_t);

procedure A_FlameDeath(actor: Pmobj_t);

procedure A_ClearForceField(actor: Pmobj_t);

procedure P_DoChase(actor: Pmobj_t; const fast: boolean);

procedure P_NoiseAlert(target: Pmobj_t; emmiter: Pmobj_t);

function P_CheckMeleeRange(actor: Pmobj_t): boolean;

function P_TryWalk(actor: Pmobj_t): boolean;

function P_Move(actor: Pmobj_t): boolean;

procedure P_Massacre;

procedure P_FreePrisoners;

procedure P_DestroyConverter;

procedure P_DoPunchAlert(puncher: Pmobj_t; punchee: Pmobj_t);

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

implementation

uses
  d_delphi,
  doomdata,
  deh_main,
  d_think,
  d_main,
  g_game,
  m_fixed,
  tables,
  i_system,
  info_h,
  info,
  f_finale,
  m_rnd,
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
  p_floor,
  p_pspr,
  p_extra,
  p_common,
  p_sounds,
  p_dialog,
  p_terrain,
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

//
// A_Fall
//
// [STRIFE]
// * Set NODIALOG, and clear NOGRAVITY and SHADOW
//
procedure A_Fall(actor: Pmobj_t);
begin
  // villsa [STRIFE] set NODIALOG flag to stop dialog
  actor.flags := actor.flags or MF_NODIALOG;
  // actor is on ground, it can be walked over
  actor.flags := actor.flags and not MF_SOLID;
  // So change this if corpse objects
  // are meant to be obstacles.

  // actor is on ground, it can be walked over
  // villsa [STRIFE] remove nogravity/shadow flags as well
  actor.flags := actor.flags and not MF_SHADOW;
  A_Gravity(actor);
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

//
// P_NoiseAlert
// If a monster yells at a player,
// it will alert other monsters to the player.
//
procedure P_NoiseAlert(target: Pmobj_t; emmiter: Pmobj_t);
begin
  soundtarget := target;
  inc(validcount);
  P_RecursiveSound(Psubsector_t(emmiter.subsector).sector, 0);
end;

//
// P_WakeUpThing
//
// villsa [STRIFE] New function
// Wakes up an mobj.nearby when somebody has been punched.
//
procedure P_WakeUpThing(puncher: Pmobj_t; bystander: Pmobj_t);
begin
  if bystander.flags and MF_NODIALOG = 0 then
  begin
    bystander.target := puncher;
    if bystander.info.seesound <> 0 then
      A_SeeSound1(bystander);
    P_SetMobjState(bystander, statenum_t(bystander.info.seestate));
  end;
end;

//
// P_DoPunchAlert
//
// villsa [STRIFE] New function (by Quasar ;)
// Wake up buddies nearby when the player thinks he's gotten too clever
// with the punch dagger. Walks sector links.
//
procedure P_DoPunchAlert(puncher: Pmobj_t; punchee: Pmobj_t);
var
  rover: Pmobj_t;
begin
  // don't bother with this crap if we're already on alert
  if Psubsector_t(punchee.subsector).sector.soundtarget <> nil then
    exit;

  // gotta still be alive to call for help
  if punchee.health <= 0 then
    exit;

  // has to be something you can wake up and kill too
  if (punchee.flags and MF_COUNTKILL = 0) or (punchee.flags and MF_NODIALOG <> 0) then
    exit;

  // make the punchee hurt - haleyjd 09/05/10: Fixed to use painstate.
  punchee.target := puncher;
  P_SetMobjState(punchee, statenum_t(punchee.info.painstate));

  // wake up everybody nearby

  // scan forward on sector list
  rover := punchee.snext;
  while rover <> nil do
  begin
    // we only wake up certain thing types (Acolytes and Templars?)
    if (rover.health > 0) and (rover._type >= Ord(MT_GUARD1)) and (rover._type <= Ord(MT_PGUARD)) and
       (P_CheckSight(rover, puncher) or P_CheckSight(rover, punchee)) then
    begin
      P_WakeUpThing(puncher, rover);
      rover.flags := rover.flags or MF_NODIALOG;
    end;
    rover := rover.snext;
  end;

  // scan backward on sector list
  rover := punchee.sprev;
  while rover <> nil do
  begin
    // we only wake up certain thing types (Acolytes and Templars?)
    if (rover.health > 0) and (rover._type >= Ord(MT_GUARD1)) and (rover._type <= Ord(MT_PGUARD)) and
       (P_CheckSight(rover, puncher) or P_CheckSight(rover, punchee)) then
    begin
      P_WakeUpThing(puncher, rover);
      rover.flags := rover.flags or MF_NODIALOG;
    end;
    rover := rover.sprev;
  end;
end;

//
// P_CheckMeleeRange
//
// [STRIFE] Minor change to meleerange.
//
function P_CheckMeleeRange(actor: Pmobj_t): boolean;
var
  pl: Pmobj_t;
  dist: fixed_t;
  mrange: integer;
begin
  if actor.target = nil then
  begin
    result := false;
    exit;
  end;

  pl := actor.target;
  if actor.z + 3 * actor.height div 2 < pl.z then // villsa [STRIFE]
  begin
    result := false;
    exit;
  end;

  dist := P_AproxDistance(pl.x - actor.x, pl.y - actor.y);

  mrange := actor.info.meleerange;
  if mrange = 0 then
    mrange := MELEERANGE
  else if mrange < FRACUNIT then
    mrange := mrange * FRACUNIT;

  if dist >= mrange - 20 * FRACUNIT + pl.info.radius then
  begin
    result := false;
    exit;
  end;

  result := P_CheckSight(actor, actor.target);
end;

//
// P_CheckMissileRange
//
// [STRIFE]
// Changes to eliminate DOOM-specific code and to allow for
// varying attack ranges for Strife monsters, as well as a general tweak
// to considered distance for all monsters.
//
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
    // the target just hit the enemy,
    // so fight back!
    actor.flags := actor.flags and not MF_JUSTHIT;
    result := true;
    exit;
  end;

  if actor.reactiontime <> 0 then
  begin
    result := false; // do not attack yet
    exit;
  end;

  // OPTIMIZE: get this from a global checksight
  dist := P_AproxDistance(actor.x - actor.target.x, actor.y - actor.target.y) -
            64 * FRACUNIT;

  if actor.info.meleestate = 0 then
    dist := dist - 128 * FRACUNIT;  // no melee attack, so fire more

  dist := FixedInt(dist);

  // villsa [STRIFE] checks for acolytes
  //  haleyjd 09/05/10: Repaired to match disassembly: Was including
  //  SHADOWGUARD in the wrong case, was missing MT_SENTINEL entirely.
  //  Structure of ASM also indicates this was probably a switch
  //  statement turned into a cascading if/else by the compiler.
  case actor._type of
    Ord(MT_GUARD1),
    Ord(MT_GUARD2),
    Ord(MT_GUARD3),
    Ord(MT_GUARD4),
    Ord(MT_GUARD5),
    Ord(MT_GUARD6):
      begin
        // oddly, not all Acolytes are included here...
        dist := dist div 16;
      end;
    Ord(MT_SHADOWGUARD),
    Ord(MT_CRUSADER),
    Ord(MT_SENTINEL):
      begin
        dist := dist div 2;
      end;
  end;

  // villsa [STRIFE] changed to 150
  if dist > 150 then
    dist := 150;

  // haleyjd 20100910: Hex-Rays was leaving this out completely:
  if (actor._type = Ord(MT_CRUSADER)) and (dist > 120) then
    dist := 120;

  if actor.flags3_ex and MF3_EX_MISSILEMORE <> 0 then
    dist := dist div 2;
  if actor.flags3_ex and MF3_EX_MISSILEEVENMORE <> 0 then
    dist := dist div 8;

  if actor.info.minmissilechance > 0 then
    if actor.info.minmissilechance < dist then
      dist := actor.info.minmissilechance;

  // haleyjd 20110224 [STRIFE]: reversed predicate
  result := dist < P_Random;
end;

//
// P_CheckRobotRange
//
// villsa [STRIFE] New function
//
function P_CheckRobotRange(actor: Pmobj_t): boolean;
var
  dist: fixed_t;
begin
  if not P_CheckSight(actor, actor.target) then
  begin
    result := false;
    exit;
  end;

  if actor.reactiontime <> 0 then
  begin
    result := false;    // do not attack yet
    exit;
  end;

  dist := P_AproxDistance(actor.x - actor.target.x,
                          actor.y - actor.target.y) - 64 * FRACUNIT;

  result := dist < 200 * FRACUNIT;
end;

//
// P_Move
// Move in the current direction,
// returns false if the move is blocked.
//
// [STRIFE]
// villsa/haleyjd 09/05/10: Modified for terrain types and 3D object
// clipping. Below constants are verified to be unmodified:
//
const
  xspeed: array[0..7] of fixed_t =
    (FRACUNIT, 47000, 0, -47000, -FRACUNIT, -47000, 0, 47000);

  yspeed: array[0..7] of fixed_t =
    (0, 47000, FRACUNIT, 47000, 0, -47000, -FRACUNIT, -47000);

function P_Move(actor: Pmobj_t): boolean;
var
  tryx: fixed_t;
  tryy: fixed_t;
  ld: Pline_t;
  try_ok: boolean;
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
      if P_UseSpecialLine(actor, ld, 0) then
        result := true;
    end;
    exit;
  end
  else
  begin
    actor.flags := actor.flags and not (MF_INFLOAT or MF_FEETCLIPPED);
    // villsa [STRIFE]
    if P_GetTerrainType(actor) <> FLOOR_SOLID then
    begin
      actor.flags := actor.flags or MF_FEETCLIPPED;
      actor.floorclip := FOOTCLIPSIZE;
    end
    else
      actor.floorclip := 0;
  end;

  if actor.flags and MF_FLOAT = 0 then
  begin
    if actor.z > actor.floorz then
      P_HitFloor(actor);
    actor.z := actor.floorz;  // JVAL: CHOCO removes this
  end;

  result := true;
end;

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

procedure P_NewChaseDir(actor: Pmobj_t);
var
  deltax: fixed_t;
  deltay: fixed_t;
  d: array[0..2] of dirtype_t;
  tdir: dirtype_t;
  olddir: dirtype_t;
  turnaround: dirtype_t;
  idx: integer;
begin
  // villsa [STRIFE] don't bomb out and instead set spawnstate
  if actor.target = nil then
  begin
    P_SetMobjState(actor, statenum_t(actor.info.spawnstate));
    exit;
  end;

  olddir := dirtype_t(actor.movedir);
  turnaround := opposite[Ord(olddir)];

  deltax := actor.target.x - actor.x;
  deltay := actor.target.y - actor.y;

  // JVAL: 20210209 - MF3_EX_CAUSEFEAR & MF3_EX_NOFEAR flags
  if actor.target.flags3_ex and MF3_EX_CAUSEFEAR <> 0 then
    if actor.flags3_ex and MF3_EX_NOFEAR <> 0 then
      actor.flags2_ex := actor.flags2_ex or MF2_EX_FRIGHTENED;

  if actor.flags2_ex and MF2_EX_FRIGHTENED <> 0 then
  begin
    deltax := -deltax;
    deltay := -deltay;
  end;

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

//
// P_NewRandomDir
//
// villsa [STRIFE] new function
//
// haleyjd: Almost identical to the tail-end of P_NewChaseDir, this function
// finds a purely random direction for an object to walk. Called from
// A_RandomWalk.
//
// Shockingly similar to the RandomWalk pointer in Eternity :)
//
procedure P_NewRandomDir(actor: Pmobj_t);
var
  dir: integer;
  omovedir: integer;
begin
  omovedir := Ord(opposite[actor.movedir]); // haleyjd 20110223: nerfed this...

  // randomly determine direction of search
  if P_Random and 1 <> 0 then
  begin
    // Try all non-reversal directions forward, first
    dir := 0;
    while dir < Ord(DI_NODIR) do
    begin
      if dir <> omovedir then
      begin
        actor.movedir := dir;
        if P_Random and 1 <> 0 then
        begin
          if P_TryWalk(actor) then
            break;
        end;
      end;
      inc(dir);
    end;

    // haleyjd 20110223: logic missing entirely:
    // failed all non-reversal directions? try reversing
    if dir > Ord(DI_SOUTHEAST) then
    begin
      if omovedir = Ord(DI_NODIR) then
      begin
        actor.movedir := Ord(DI_NODIR);
        exit;
      end;
      actor.movedir := omovedir;
      if not P_TryWalk(actor) then
        actor.movedir := Ord(DI_NODIR);
      exit;
    end;
  end
  else
  begin
    // Try directions one at a time in backward order
    dir := Ord(DI_SOUTHEAST);
    while true do
    begin
      // haleyjd 09/05/10: missing random code.
      if dir <> omovedir then
      begin
        actor.movedir := dir;

        // villsa 09/06/10: un-inlined code
        if P_TryWalk(actor) then
          exit;
      end;
      dec(dir);
      // Ran out of non-reversal directions to try? Reverse.
      if dir = -1 then
      begin
        if omovedir = Ord(DI_NODIR) then
        begin
          actor.movedir := Ord(DI_NODIR);
          exit;
        end;
        actor.movedir := omovedir;
        // villsa 09/06/10: un-inlined code
        if not P_TryWalk(actor) then
          actor.movedir := Ord(DI_NODIR);
        exit;
      end;  // end if dir = -1)
    end;    // end while(1)
  end;      // end else
end;

//
// P_LookForPlayers
// If allaround is false, only look 180 degrees in front.
// Returns true if a player is targeted.
//
// [STRIFE]
// haleyjd 09/05/10: Modifications to support friendly units.
//
function P_LookForPlayers(actor: Pmobj_t; allaround: boolean): boolean;
var
  c: integer;
  stop: integer;
  player: Pplayer_t;
  an: angle_t;
  dist: fixed_t;
  initial: boolean;
  master: Pmobj_t;
begin
  if actor.miscdata < MAXPLAYERS then
    master := players[actor.miscdata].mo
  else
    master := nil;

  // haleyjd 09/05/10: handle Allies
  if actor.flags and MF_ALLY <> 0 then
  begin
    // Deathmatch: support team behavior for Rebels.
    if netgame then
    begin
      // Rebels adopt the allied player's target if it is not of the same
      // allegiance. Other allies do it unconditionally.
      if (master <> nil) and (master.target <> nil) and
         ((master.target._type <> Ord(MT_REBEL1)) or
          (master.target.miscdata <> actor.miscdata)) then
        actor.target := master.target
      else
      begin
        // haleyjd 09/06/10: Note that this sets actor->target in Strife!
        P_BulletSlope(actor);

        // Clear target if nothing is visible, or if the target is a
        // friendly Rebel or the allied player.
        if (linetarget = nil) or
           ((actor.target._type = Ord(MT_REBEL1)) and
            (actor.target.miscdata = actor.miscdata)) or
           (actor.target = master) then
        begin
          actor.target := nil;
          result := false;
          exit;
        end;
      end;
    end
    else
    begin
      // Single-player: Adopt any non-allied player target.
      if (master <> nil) and (master.target <> nil) and (master.target.flags and MF_ALLY = 0) then
      begin
        actor.target := master.target;
        result := true;
        exit;
      end;

      // haleyjd 09/06/10: Note that this sets actor->target in Strife!
      P_BulletSlope(actor);

      // Clear target if nothing is visible, or if the target is an ally.
      if (linetarget = nil) or (actor.target.flags and MF_ALLY <> 0) then
      begin
        actor.target := nil;
        result := false;
        exit;
      end;
    end;

    result := true;
    exit;
  end;

  c := 0;
  stop := (actor.lastlook + MAXPLAYERS - 1) mod MAXPLAYERS;

  initial := true;
  while true do
  begin
    if initial then
      initial := false
    else
      actor.lastlook := (actor.lastlook + 1) mod MAXPLAYERS;

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

//
// ACTION ROUTINES
//

//
// A_Look
// Stay in state until a player is sighted.
//
procedure A_Look(actor: Pmobj_t);
var
  targ: Pmobj_t;
  seeyou: boolean;
  sound: integer;
begin
  actor.threshold := 0; // any shot will wake up
  targ := Psubsector_t(actor.subsector).sector.soundtarget;
  seeyou := false;
  if (targ <> nil) and (targ.flags and MF_SHOOTABLE <> 0) then
  begin
    // [STRIFE] Allies wander when they call this.
    if actor.flags and MF_ALLY <> 0 then
      A_RandomWalk(actor)
    else
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
  end;

  if not seeyou then
  begin
    // haleyjd 09/05/10: This is bizarre, as Rogue keeps using the GIVEQUEST flag
    // as a parameter to control allaround look behavior. Did they just run out of
    // flags, or what?
    // STRIFE-TODO: Needs serious verification.
    if not P_LookForPlayers(actor, (actor.flags_ex and MF_EX_LOOKALLAROUND <> 0) or
                                   (actor.flags and MF_GIVEQUEST <> 0)) then
      exit;
  end;

  // go into chase state
  if actor.info.seesound <> 0 then
  begin
    sound := actor.info.seesound;

    if actor.info.flags_ex and MF_EX_RANDOMSEESOUND <> 0 then
    begin
      if (actor._type = Ord(MT_INQUISITOR)) or (actor.info.flags_ex and MF_EX_BOSS <> 0) then
        // full volume
        P_RandomSound(nil, sound)
      else
        P_RandomSound(actor, sound)
    end
    else
    begin
      if (actor._type = Ord(MT_INQUISITOR)) or (actor.info.flags_ex and MF_EX_BOSS <> 0) then
        // full volume
        S_StartSound(nil, sound)
      else
        S_StartSound(actor, sound);
    end;
  end;

  // [STRIFE] Set threshold (kinda odd as it's still set to 0 above...)
  actor.threshold := 20;

  P_SetMobjState(actor, statenum_t(actor.info.seestate));
end;


//
// A_RandomWalk
//
// [STRIFE] New function.
// haleyjd 09/05/10: Action routine used to meander about.
//
procedure A_RandomWalk(actor: Pmobj_t);
var
  delta: integer;
begin
  // Standing actors do not wander.
  if actor.flags and MF_STAND <> 0 then
    exit;

  if actor.reactiontime <> 0 then
  begin
    dec(actor.reactiontime); // count down reaction time
    exit;
  end;

  // turn to a new angle
  if actor.movedir < Ord(DI_NODIR) then
  begin
    actor.angle := actor.angle and _SHLW(7, 29);
    delta := actor.angle - _SHLW(actor.movedir, 29);

    if delta < 0 then
      actor.angle := actor.angle + ANG90 div 2
    else if delta > 0 then
      actor.angle := actor.angle - ANG90 div 2;
  end;

  dec(actor.movecount);
  // try moving
  if (actor.movecount < 0) or not P_Move(actor) then
  begin
    P_NewRandomDir(actor);
    actor.movecount := actor.movecount + 5;
  end;
end;

//
// A_FriendLook
//
// [STRIFE] New function
// haleyjd 09/05/10: Action function used mostly by mundane characters such as
// peasants.
//
procedure A_FriendLook(actor: Pmobj_t);
var
  soundtarget: Pmobj_t;
begin
  soundtarget := Psubsector_t(actor.subsector).sector.soundtarget;

  actor.threshold := 0;

  if (soundtarget <> nil) and (soundtarget.flags and MF_SHOOTABLE <> 0) then
  begin
    // Handle allies, except on maps 3 and 34 (Front Base/Movement Base)
    if ((actor.flags and MF_ALLY) = (soundtarget.flags and MF_ALLY)) and
       ((gamemap <> 3) and (gamemap <> 34)) then
    begin
      // STRIFE-TODO: Needs serious verification.
      if P_LookForPlayers(actor, (actor.flags_ex and MF_EX_LOOKALLAROUND <> 0) or
                                 (actor.flags and MF_GIVEQUEST <> 0)) then
      begin
        P_SetMobjState(actor, statenum_t(actor.info.seestate));
        actor.flags := actor.flags or MF_NODIALOG;
        exit;
      end;
    end
    else
    begin
      actor.target := soundtarget;

      if (actor.flags and MF_AMBUSH = 0) or P_CheckSight(actor, actor.target) then
      begin
        actor.threshold := 10;
        P_SetMobjState(actor, statenum_t(actor.info.seestate));
        exit;
      end;
    end;
  end;

  // do some idle animation
  if P_Random < 30 then
    P_SetMobjState(actor, statenum_t((P_Random and 1) + actor.info.spawnstate + 1));

  // wander around a bit
  if (actor.flags and MF_STAND = 0) and (P_Random < 40) then
    P_SetMobjState(actor, statenum_t(actor.info.spawnstate + 3));
end;

//
// A_Listen
//
// [STRIFE] New function
// haleyjd 09/05/10: Action routine used to strictly listen for a target.
//
procedure A_Listen(actor: Pmobj_t);
var
  soundtarget: Pmobj_t;
begin
  actor.threshold := 0;

  soundtarget := Psubsector_t(actor.subsector).sector.soundtarget;

  if (soundtarget <> nil) and (soundtarget.flags and MF_SHOOTABLE <> 0) then
    if (actor.flags and MF_ALLY) <> (soundtarget.flags and MF_ALLY) then
    begin
      actor.target := soundtarget;

      if (actor.flags and MF_AMBUSH = 0) or P_CheckSight(actor, actor.target) then
      begin
        A_SeeSound1(actor);
        actor.threshold := 10;
        P_SetMobjState(actor, statenum_t(actor.info.seestate));
      end;
    end;
end;

//
// A_Chase
// Actor has a melee attack,
// so it tries to close as fast as possible
//
// haleyjd 09/05/10: [STRIFE] Various minor changes
//
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
    if actor.target = nil then
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

  if (actor.target = nil) or
     (actor.target.flags and MF_SHOOTABLE = 0) then
  begin
    // look for a new target
    if P_LookForPlayers(actor, true) then
      exit; // got a new target

    if actor.state <> @states[actor.info.spawnstate] then
      P_SetMobjState(actor, statenum_t(actor.info.spawnstate));
    exit;
  end;

  // do not attack twice in a row
  if actor.flags and MF_JUSTATTACKED <> 0 then
  begin
    actor.flags := actor.flags and not MF_JUSTATTACKED;
    if not fastparm then
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

  nomissile := false;
  // check for missile attack
  if actor.info.missilestate <> 0 then
  begin
    if not fastparm and (actor.movecount <> 0) then
      nomissile := true
    else if not P_CheckMissileRange(actor) then
      nomissile := true;
    if not nomissile then
    begin
      P_SetMobjState(actor, statenum_t(actor.info.missilestate));
      actor.flags := actor.flags or MF_JUSTATTACKED or MF_NODIALOG;
      exit;
    end;
  end;

  // possibly choose another target
  if netgame and
    (actor.threshold = 0) and
    not P_CheckSight(actor, actor.target) then
  begin
    if P_LookForPlayers(actor, true) then
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

  // [STRIFE] Changes to active sound behavior:
  // * Significantly more frequent
  // * Acolytes have randomized wandering sounds

  // make active sound
  if (actor.info.activesound <> 0) and (P_Random < 38) then
  begin
    if (actor.info.activesound >= Ord(sfx_agrac1)) and
       (actor.info.activesound <= Ord(sfx_agrac4)) then
      S_StartSound(actor, Ord(sfx_agrac1) + P_Random mod 4)
    else
      A_ActiveSound1(actor);
  end;
end;

procedure A_Chase(actor: Pmobj_t);
begin
  P_DoChase(actor, false);
end;

//
// A_FaceTarget
//
// [STRIFE]
// haleyjd 09/05/10: Handling for visibility-modifying flags.
//
procedure A_FaceTarget(actor: Pmobj_t);
begin
  if actor.target = nil then
    exit;

  actor.flags := actor.flags and not MF_AMBUSH;

  actor.angle :=
    R_PointToAngle2(actor.x, actor.y, actor.target.x, actor.target.y);

  if actor.flags2_ex and MF2_EX_SEEINVISIBLE = 0 then
  begin
    if actor.target.flags and MF_SHADOW <> 0 then
      actor.angle := actor.angle + _SHLW(P_Random - P_Random, 22)
    else if actor.target.flags and MF_MVIS <> 0 then
      actor.angle := actor.angle + _SHLW(P_Random - P_Random, 23);
  end;
end;

//
// A_PeasantPunch
//
// [STRIFE] New function
// haleyjd 09/05/10: Attack used by Peasants as a one-time retaliation
// when the player or a monster injures them. Weak doesn't begin to
// describe it :P
//
procedure A_PeasantPunch(actor: Pmobj_t);
begin
  if actor.target = nil then
    exit;

  A_FaceTarget(actor);
  if P_CheckMeleeRange(actor) then
    P_DamageMobj(actor.target, actor, actor, 2 * (P_Random mod 5) + 2);
end;

//
// A_ReaverAttack
//
// [STRIFE] New function
// haleyjd 09/06/10: Action routine used by Reavers to fire bullets.
// Also apparently used by Inquistors, though they don't seem to use
// it too often, as they're content to blow your face off with their
// HE grenades instead.
//
procedure A_ReaverAttack(actor: Pmobj_t);
var
  i: integer;
  slope: fixed_t;
  t: integer;
  shootangle: angle_t;
  damage: integer;
begin
  if actor.target = nil then
    exit;

  S_StartSound(actor, Ord(sfx_reavat));
  A_FaceTarget(actor);

  slope := P_AimLineAttack(actor, actor.angle, 2048 * FRACUNIT);

  for i := 0 to 2 do
  begin
    t := P_Random;
    shootangle := actor.angle + _SHLW(t - P_Random, 20);
    damage := 3 * (P_Random and 7) + 1;

    P_LineAttack(actor, shootangle, 2048 * FRACUNIT, slope, damage);
  end;
end;

//
// A_BulletAttack
//
// [STRIFE] New function
// haleyjd 09/06/10: Action function for generic bullet attacks. Used by
// a lot of different characters, including Acolytes, Rebels, and Macil.
//
procedure A_BulletAttack(actor: Pmobj_t);
var
  t, damage: integer;
  slope: fixed_t;
  shootangle: angle_t;
begin
  if actor.target = nil then
    exit;

  S_StartSound(actor, Ord(sfx_rifle));
  A_FaceTarget(actor);

  slope := P_AimLineAttack(actor, actor.angle, 2048 * FRACUNIT);
  t := P_Random;
  shootangle := actor.angle + _SHLW(t - P_Random, 19);
  damage := 3 * (P_Random mod 5 + 1);

  P_LineAttack(actor, shootangle, 2048 * FRACUNIT, slope, damage);
end;


//
// A_CheckTargetVisible
//
// [STRIFE] New function
// haleyjd 09/06/10: Action routine which sets a thing back to its
// seestate at random, or if it cannot see its target, or its target
// is dead. Used by diverse actors.
//
procedure A_CheckTargetVisible(actor: Pmobj_t);
var
  target: Pmobj_t;
begin
  A_FaceTarget(actor);

  if P_Random >= 30 then
  begin
    target := actor.target;

    if (target <> nil) or (target.health <= 0) or not P_CheckSight(actor, target) or (P_Random < 40) then
      P_SetMobjState(actor, statenum_t(actor.info.seestate));
  end;
end;


//
// A_SentinelAttack
//
// [STRIFE] New function
// haleyjd 09/06/10: Action function implementing the Sentinel's laser attack
// villsa 09/06/10 implemented
//
procedure A_SentinelAttack(actor: Pmobj_t);
var
  mo, mo2: Pmobj_t;
  x, y, z: fixed_t;
  an: angle_t;
  i: integer;
begin
  mo := P_SpawnFacingMissile(actor, actor.target, Ord(MT_L_LASER));
  an := actor.angle div ANGLETOFINEUNIT;

  if (mo.momy <> 0) or (mo.momx <> 0) then
    for i := 8 downto 2 do
    begin
      x := mo.x + FixedMul(mobjinfo[Ord(MT_L_LASER)].radius * i, finecosine[an]);
      y := mo.y + FixedMul(mobjinfo[Ord(MT_L_LASER)].radius * i, finesine[an]);
      z := mo.z + i * (mo.momz div 4);
      mo2 := P_SpawnMobj(x, y, z, Ord(MT_R_LASER));
      mo2.target := actor;
      mo2.momx := mo.momx;
      mo2.momy := mo.momy;
      mo2.momz := mo.momz;
      P_CheckMissileSpawn(mo2);
    end;

  mo.z := mo.z + mo.momz div 4;
end;

//
// A_StalkerThink
//
// [STRIFE] New function
// haleyjd 09/06/10: Action function to drive Stalker logic.
//
procedure A_StalkerThink(actor: Pmobj_t);
var
  statenum: statenum_t;
begin
  if actor.flags and MF_NOGRAVITY <> 0 then
  begin
    if actor.ceilingz - actor.info.height <= actor.z then
      exit;
    statenum := S_SPID_11; // 1020
  end
  else
    statenum := S_SPID_18; // 1027

  P_SetMobjState(actor, statenum);
end;

//
// A_StalkerSetLook
//
// [STRIFE] New function
// haleyjd 09/06/10: Action function to marshall transitions to the
// Stalker's spawnstate.
//
procedure A_StalkerSetLook(actor: Pmobj_t);
var
  statenum: statenum_t;
begin
  if actor = nil then // weird; totally unnecessary.
    exit;

  if actor.flags and MF_NOGRAVITY <> 0 then
  begin
    if actor.state.nextstate = S_SPID_01 then // 1010
      exit;
    statenum := S_SPID_01; // 1010
  end
  else
  begin
    if actor.state.nextstate = S_SPID_02 then // 1011
      exit;
    statenum := S_SPID_02; // 1011
  end;

  P_SetMobjState(actor, statenum);
end;

//
// A_StalkerDrop
//
// [STRIFE] New function
// haleyjd 09/06/10: Dead simple: removes NOGRAVITY status.
//
procedure A_StalkerDrop(actor: Pmobj_t);
begin
  A_NoGravity(actor);
end;

//
// A_StalkerScratch
//
// [STRIFE] New function
// haleyjd 09/06/10: Action function for Stalker's attack.
//
procedure A_StalkerScratch(actor: Pmobj_t);
begin
  if actor.flags and MF_NOGRAVITY <> 0 then
  begin
    // Drop him down before he can attack
    P_SetMobjState(actor, S_SPID_11); // 1020
    exit;
  end;

  if actor.target = nil then
    exit;

  A_FaceTarget(actor);
  if P_CheckMeleeRange(actor) then
    P_DamageMobj(actor.target, actor, actor, 2 * (P_Random mod 8) + 2);
end;

//
// A_FloatWeave
//
// [STRIFE] New function
// haleyjd 09/06/10: Action function which is responsible for floating
// actors' constant upward and downward movement. Probably a really bad
// idea in retrospect given how dodgy the 3D clipping implementation is.
//
procedure A_FloatWeave(actor: Pmobj_t);
var
  z, height: fixed_t;
begin
  if actor.threshold <> 0 then
    exit;

  if actor.flags and MF_INFLOAT <> 0 then
    exit;

  height := actor.info.height;         // v2
  z := actor.floorz + 96 * FRACUNIT; // v1

  if z > actor.ceilingz - height - 16 * FRACUNIT then
    z := actor.ceilingz - height - 16 * FRACUNIT;

  if z >= actor.z then
    actor.momz := actor.momz + FRACUNIT
  else
    actor.momz := actor.momz - FRACUNIT;

  if z = actor.z then
    actor.threshold := 4
  else
    actor.threshold := 8;
end;

//
// A_RobotMelee
//
// [STRIFE] New function
// haleyjd 09/06/10: Action function for Reaver and Templar melee attacks.
//
procedure A_RobotMelee(actor: Pmobj_t);
begin
  if actor.target = nil then
    exit;

  A_FaceTarget(actor);
  if P_CheckMeleeRange(actor) then
  begin
    S_StartSound(actor, Ord(sfx_revbld));
    P_DamageMobj(actor.target, actor, actor, 3 * (P_Random mod 8 + 1));
  end;
end;

//
// A_TemplarMauler
//
// [STRIFE] New function
// haleyjd 09/06/10: Exactly what it sounds like. Kicks your ass.
//
procedure A_TemplarMauler(actor: Pmobj_t);
var
  i, t: integer;
  angle, bangle: angle_t;
  damage: integer;
  slope: integer;
begin
  if actor.target = nil then
    exit;

  S_StartSound(actor, Ord(sfx_pgrdat));
  A_FaceTarget(actor);
  bangle := actor.angle;
  slope := P_AimLineAttack(actor, bangle, 2048 * FRACUNIT);

  for i := 1 to 9 do
  begin
    // haleyjd 09/06/10: Very carefully preserved order of P_Random calls
    damage := (P_Random and 4) * 2;
    t := P_Random;
    angle := bangle + _SHLW(t - P_Random, 19);
    t := P_Random;
    slope := (t - P_Random) * 32 + slope;
    P_LineAttack(actor, angle, 2112 * FRACUNIT, slope, damage);
  end;
end;

//
// A_CrusaderAttack
//
// villsa [STRIFE] new codepointer
// 09/06/10: Action function for the Crusader's Flamethrower.
// Very similar to the player's flamethrower, seeing how it was ripped
// off a Crusader by the Rat People ;)
//
procedure A_CrusaderAttack(actor: Pmobj_t);
begin
  if actor.target = nil then
    exit;

  actor.z := actor.z + (8 * FRACUNIT);

  if P_CheckRobotRange(actor) then
  begin
    A_FaceTarget(actor);
    actor.angle := actor.angle - (ANG90 div 8);
    P_SpawnFacingMissile(actor, actor.target, Ord(MT_C_FLAME));
  end
  else if P_CheckMissileRange(actor) then
  begin
    A_FaceTarget(actor);
    actor.z := actor.z + (16 * FRACUNIT);
    P_SpawnFacingMissile(actor, actor.target, Ord(MT_C_MISSILE));

    actor.angle := actor.angle - (ANG45 div 32);
    actor.z := actor.z - (16 * FRACUNIT);
    P_SpawnFacingMissile(actor, actor.target, Ord(MT_C_MISSILE));

    actor.angle := actor.angle + (ANG45 div 16);
    P_SpawnFacingMissile(actor, actor.target, Ord(MT_C_MISSILE));

    P_SetMobjState(actor, statenum_t(actor.info.seestate));
    actor.reactiontime := actor.reactiontime + 15;
  end
  else
    P_SetMobjState(actor, statenum_t(actor.info.seestate));

  actor.z := actor.z - (8 * FRACUNIT);
end;

//
// A_CrusaderLeft
//
// villsa [STRIFE] new codepointer
//
procedure A_CrusaderLeft(actor: Pmobj_t);
var
  mo: Pmobj_t;
begin
   actor.angle := actor.angle + (ANG90 div 16);
   mo := P_SpawnFacingMissile(actor, actor.target, Ord(MT_C_FLAME));
   mo.momz := FRACUNIT;
   mo.z := mo.z + (16 * FRACUNIT);
end;

//
// A_CrusaderRight
//
// villsa [STRIFE] new codepointer
//
procedure A_CrusaderRight(actor: Pmobj_t);
var
  mo: Pmobj_t;
begin
  actor.angle := actor.angle - (ANG90 div 16);
  mo := P_SpawnFacingMissile(actor, actor.target, Ord(MT_C_FLAME));
  mo.momz := FRACUNIT;
  mo.z := mo.z + (16 * FRACUNIT);
end;

//
// A_CheckTargetVisible2
//
// [STRIFE] New function
// haleyjd 09/06/10: Mostly the same as CheckTargetVisible, except without
// the randomness.
//
procedure A_CheckTargetVisible2(actor: Pmobj_t);
begin
  if (actor.target = nil) or (actor.target.health <= 0) or
     not P_CheckSight(actor, actor.target) then
    P_SetMobjState(actor, statenum_t(actor.info.seestate));
end;

//
// A_InqFlyCheck
//
// [STRIFE] New function
// haleyjd 09/06/10: Action function to check if an Inquisitor wishes
// to take to flight.
//
procedure A_InqFlyCheck(actor: Pmobj_t);
begin
  if actor.target = nil then
    exit;

  A_FaceTarget(actor);

  // if not in "robot" range, shoot grenades.
  if not P_CheckRobotRange(actor) then
    P_SetMobjState(actor, S_ROB3_14); // 1061

  if actor.z <> actor.target.z then
    // Take off all zig!
    if actor.z + actor.height + 54 * FRACUNIT < actor.ceilingz then
      P_SetMobjState(actor, S_ROB3_17); // 1064
end;

//
// A_InqGrenade
//
// villsa [STRIFE] new codepointer
// 09/06/10: Inquisitor grenade attack action routine.
//
procedure A_InqGrenade(actor: Pmobj_t);
var
  mo: Pmobj_t;
begin
  if actor.target = nil then
    exit;

  A_FaceTarget(actor);

  actor.z := actor.z + MAXRADIUS;

  // grenade 1
  actor.angle := actor.angle - (ANG45 div 32);
  mo := P_SpawnFacingMissile(actor, actor.target, Ord(MT_INQGRENADE));
  mo.momz := mo.momz + (9 * FRACUNIT);

  // grenade 2
  actor.angle := actor.angle + (ANG45 div 16);
  mo := P_SpawnFacingMissile(actor, actor.target, Ord(MT_INQGRENADE));
  mo.momz := mo.momz + (16 * FRACUNIT);

  actor.z := actor.z - MAXRADIUS;
end;

//
// A_InqTakeOff
//
// [STRIFE] New function
// haleyjd 09/06/10: Makes an Inquisitor start flying.
//
procedure A_InqTakeOff(actor: Pmobj_t);
var
  an: angle_t;
  speed, dist: fixed_t;
begin
  if actor.target = nil then
    exit;

  speed := actor.info.speed * (2 * FRACUNIT div 3);

  S_StartSound(actor, Ord(sfx_inqjmp));

  actor.z := actor.z + 64 * FRACUNIT;

  A_FaceTarget(actor);

  an := actor.angle div ANGLETOFINEUNIT;

  actor.momx := FixedMul(finecosine[an], speed);
  actor.momy := FixedMul(finesine[an], speed);

  dist := P_AproxDistance(actor.target.x - actor.x,
                          actor.target.y - actor.y);

  dist := dist div speed;
  if dist < 1 then
    dist := 1;

  actor.momz := (actor.target.z - actor.z) div dist;
  actor.reactiontime := 60;
  A_NoGravity(actor);
end;

// A_InqFly
//
// [STRIFE] New function
// haleyjd 09/06/10: Handles an Inquisitor in flight.
//
procedure A_InqFly(actor: Pmobj_t);
begin
  if leveltime and 7 = 0 then
    S_StartSound(actor, Ord(sfx_inqjmp));

  dec(actor.reactiontime);
  if (actor.reactiontime < 0) or (actor.momx = 0) or (actor.momy = 0) or
     (actor.z <= actor.floorz) then
  begin
    // Come in for a landing.
    P_SetMobjState(actor, statenum_t(actor.info.seestate));
    actor.reactiontime := 0;
    A_Gravity(actor);
  end;
end;

//
// A_FireSigilWeapon
//
// [STRIFE] New function
// haleyjd 09/06/10: Action function for the Entity's attack.
//
procedure A_FireSigilWeapon(actor: Pmobj_t);
var
  choice: integer;
begin
  choice := P_Random mod 5;

  // STRIFE-TODO: Needs verification. This switch is just weird.
  case choice of
    0:  A_ProgrammerAttack(actor);
    2:  A_FireSigilEOffshoot(actor);
    3:  A_SpectreCAttack(actor);
    4: A_SpectreDAttack(actor);
  end;
{    5: // BUG: never used? wtf were they thinking?
        A_SpectreEAttack(actor);}
end;

//
// A_ProgrammerAttack
//
// [STRIFE] New function
// haleyjd 09/06/10: Action function for the Programmer's main
// attack; equivalent to the player's first Sigil.
//
procedure A_ProgrammerAttack(actor: Pmobj_t);
var
  mo: Pmobj_t;
begin
  if actor.target = nil then
    exit;

  mo := P_SpawnMobj(actor.target.x, actor.target.y, ONFLOORZ, Ord(MT_SIGIL_A_GROUND));
  mo.threshold := 25;
  mo.target := actor;
  mo.health := -2;
  mo.tracer := actor.target;
end;

//
// A_Sigil_A_Action
//
// [STRIFE] New function
// haleyjd 09/06/10: Called by MT_SIGIL_A_GROUND to zot anyone nearby with
// corny looking lightning bolts.
//
procedure A_Sigil_A_Action(actor: Pmobj_t);
var
  t, x, y, _type: integer;
  mo: Pmobj_t;
begin
  if actor.threshold > 0 then
    dec(actor.threshold);

  t := P_Random;
  actor.momx := actor.momx + ((t and 3) - (P_Random and 3)) * FRACUNIT;
  t := P_Random;
  actor.momy := actor.momy + ((t and 3) - (P_Random and 3)) * FRACUNIT;

  t := P_Random;
  x := 50 * FRACUNIT * ((t and 3) - (P_Random and 3)) + actor.x;
  t := P_Random;
  y := 50 * FRACUNIT * ((t and 3) - (P_Random and 3)) + actor.y;

  if actor.threshold <= 25 then
    _type := Ord(MT_SIGIL_A_ZAP_LEFT)
  else
    _type := Ord(MT_SIGIL_A_ZAP_RIGHT);

  mo := P_SpawnMobj(x, y, ONCEILINGZ, _type);
  mo.momz := -18 * FRACUNIT;
  mo.target := actor.target;
  mo.health := actor.health;

  mo := P_SpawnMobj(actor.x, actor.y,  ONCEILINGZ, Ord(MT_SIGIL_A_ZAP_RIGHT));
  mo.momz := -18 * FRACUNIT;
  mo.target := actor.target;
  mo.health := actor.health;
end;

//
// A_SpectreEAttack
//
// [STRIFE] New function
// haleyjd 09/06/10: Action function for the Loremaster's Spectre.
// Equivalent to the player's final Sigil attack.
//
procedure A_SpectreEAttack(actor: Pmobj_t);
var
  mo: Pmobj_t;
begin
  if actor.target = nil then
    exit;

  mo := P_SpawnMissile(actor, actor.target, Ord(MT_SIGIL_SE_SHOT));
  mo.health := -2;
end;


//
// A_SpectreCAttack
//
// villsa [STRIFE] new codepointer
// 09/06/10: Action routine for the Oracle's Spectre. Equivalent to the player's
// third Sigil attack.
//
procedure A_SpectreCAttack(actor: Pmobj_t);
var
  mo: Pmobj_t;
  i: integer;
begin
  if actor.target = nil then
    exit;

  mo := P_SpawnMobj(actor.x, actor.y, actor.z + (32 * FRACUNIT), Ord(MT_SIGIL_A_ZAP_RIGHT));
  mo.momz := -18 * FRACUNIT;
  mo.target := actor;
  mo.health := -2;
  mo.tracer := actor.target;

  actor.angle := actor.angle - ANG90;
  for i := 0 to 19 do
  begin
    actor.angle := actor.angle + (ANG90 div 10);
    mo := P_SpawnMortar(actor, Ord(MT_SIGIL_C_SHOT));
    mo.health := -2;
    mo.z := actor.z + (32 * FRACUNIT);
  end;
  actor.angle := actor.angle - ANG90;
end;

//
// A_AlertSpectreC
//
// [STRIFE] New function
// haleyjd 09/06/10: Action function called by the Oracle when it is
// killed. Finds an MT_SPECTRE_C anywhere on the map and awakens it.
//
procedure A_AlertSpectreC(actor: Pmobj_t);
var
  th: Pthinker_t;
  mo: Pmobj_t;
begin
  th := thinkercap.next;
  while th <> @thinkercap do
  begin
    if @th._function.acp1 = @P_MobjThinker then
    begin
      mo := Pmobj_t(th);
      if mo._type = Ord(MT_SPECTRE_C) then
      begin
         // haleyjd 20140817: [SVE] Fix ghost spectre bug
        if G_NeedsCompatibilityMode or (mo.health > 0) then
        begin
          P_SetMobjState(mo, statenum_t(mo.info.seestate));
          mo.target := actor.target;
        end;
        exit;
      end;
    end;
    th := th.next;
  end;
end;

//
// A_Sigil_E_Action
//
// villsa [STRIFE] new codepointer
// 09/06/10: Action routine for Sigil "E" shots. Spawns the room-filling
// lightning bolts that seem to often do almost nothing.
//
procedure A_Sigil_E_Action(actor: Pmobj_t);
begin
  actor.angle := actor.angle + ANG90;
  P_SpawnMortar(actor, Ord(MT_SIGIL_E_OFFSHOOT));

  actor.angle := actor.angle - ANG180;
  P_SpawnMortar(actor, Ord(MT_SIGIL_E_OFFSHOOT));

  actor.angle := actor.angle + ANG90;
  P_SpawnMortar(actor, Ord(MT_SIGIL_E_OFFSHOOT));
end;

//
// A_SigilTrail
//
// villsa [STRIFE] new codepointer
//
procedure A_SigilTrail(actor: Pmobj_t);
var
  mo: Pmobj_t;
begin
  mo := P_SpawnMobj(actor.x - actor.momx,
                    actor.y - actor.momy,
                    actor.z, Ord(MT_SIGIL_TRAIL));

  mo.angle := actor.angle;
  mo.health := actor.health;
end;

//
// A_SpectreDAttack
//
// [STRIFE] New function
// haleyjd 09/06/10: Action function for Macil's Spectre.
// Equivalent of the player's fourth Sigil attack.
//
procedure A_SpectreDAttack(actor: Pmobj_t);
var
  mo: Pmobj_t;
begin
  if actor.target = nil then
    exit;

  mo := P_SpawnMissile(actor, actor.target, Ord(MT_SIGIL_SD_SHOT));
  mo.health := -2;
  mo.tracer := actor.target;
end;

//
// A_FireSigilEOffshoot
//
// [STRIFE] New function
// haleyjd 09/06/10: Action function to fire part of a Sigil E
// attack. Used at least by the Entity.
//
procedure A_FireSigilEOffshoot(actor: Pmobj_t);
var
  mo: Pmobj_t;
begin
  if actor.target = nil then
    exit;

  mo := P_SpawnMissile(actor, actor.target, Ord(MT_SIGIL_E_OFFSHOOT));
  mo.health := -2;
end;

//
// A_ShadowOff
//
// villsa [STRIFE] new codepointer
// 09/06/10: Disables SHADOW and MVIS flags.
//
procedure A_ShadowOff(actor: Pmobj_t);
begin
  actor.flags := actor.flags and not (MF_SHADOW or MF_MVIS);
end;

//
// A_ModifyVisibility
//
// villsa [STRIFE] new codepointer
// 09/06/10: Turns on SHADOW, and turns off MVIS.
//
procedure A_ModifyVisibility(actor: Pmobj_t);
begin
  actor.flags := actor.flags or MF_SHADOW;
  actor.flags := actor.flags and not MF_MVIS;
end;

//
// A_ShadowOn
//
// villsa [STRIFE] new codepointer
// 09/06/10: Turns on SHADOW and MVIS.
//
procedure A_ShadowOn(actor: Pmobj_t);
begin
  actor.flags := actor.flags or (MF_SHADOW or MF_MVIS);
end;

//
// A_SetTLOptions
//
// villsa [STRIFE] new codepointer
// 09/06/10: Sets SHADOW and/or MVIS based on the thing's spawnpoint options.
//
procedure A_SetTLOptions(actor: Pmobj_t);
begin
  if actor.spawnpoint.options and MTF_TRANSLUCENT <> 0 then
    actor.flags := actor.flags or MF_SHADOW;
  if actor.spawnpoint.options and MTF_MVIS <> 0 then
    actor.flags := actor.flags or MF_MVIS;
end;

//
// A_BossMeleeAtk
//
// villsa [STRIFE] new codepointer
// 09/06/10: Gratuitous melee attack used by multiple boss characters,
// just for the sake of having one. It's not like anybody in their right
// mind would get close to any of the maniacs that use this ;)
//
procedure A_BossMeleeAtk(actor: Pmobj_t);
begin
  if actor.target = nil then
    exit;

  P_DamageMobj(actor.target, actor, actor, 10 * (P_Random and 9));
end;

//
// A_BishopAttack
//
// villsa [STRIFE] new codepointer
// 09/06/10: Bishop's homing missile attack.
//
procedure A_BishopAttack(actor: Pmobj_t);
var
  mo: Pmobj_t;
begin
  if actor.target = nil then
    exit;

  actor.z := actor.z + MAXRADIUS;

  mo := P_SpawnMissile(actor, actor.target, Ord(MT_SEEKMISSILE));
  mo.tracer := actor.target;

  actor.z := actor.z - MAXRADIUS;
end;

//
// A_FireHookShot
//
// villsa [STRIFE] new codepointer
// 09/06/10: Action function for the Loremaster's hookshot attack.
//
procedure A_FireHookShot(actor: Pmobj_t);
begin
  if actor.target = nil then
    exit;

  P_SpawnMissile(actor, actor.target, Ord(MT_HOOKSHOT));
end;

//
// A_FireChainShot
//
// villsa [STRIFE] new codepointer
// 09/06/10: Action function for the hookshot projectile. Spawns echoes
// to create a chain-like appearance.
//
procedure A_FireChainShot(actor: Pmobj_t);
begin
  S_StartSound(actor, Ord(sfx_tend));

  P_SpawnMobj(actor.x, actor.y, actor.z, Ord(MT_CHAINSHOT)); // haleyjd: fixed type

  P_SpawnMobj(actor.x - (actor.momx div 2),
              actor.y - (actor.momy div 2),
              actor.z, Ord(MT_CHAINSHOT));

  P_SpawnMobj(actor.x - actor.momx,
              actor.y - actor.momy,
              actor.z, Ord(MT_CHAINSHOT));
end;

//
// A_MissileSmoke
//
// villsa [STRIFE] new codepointer
//
procedure A_MissileSmoke(actor: Pmobj_t);
var
  mo: Pmobj_t;
begin
  S_StartSound(actor, Ord(sfx_rflite));
  P_SpawnPuff(actor.x, actor.y, actor.z);
  mo := P_SpawnMobj(actor.x - actor.momx,
                    actor.y - actor.momy,
                    actor.z, Ord(MT_MISSILESMOKE));

  mo.momz := FRACUNIT;
end;

//
// A_SpawnSparkPuff
//
// villsa [STRIFE] new codepointer
//
procedure A_SpawnSparkPuff(actor: Pmobj_t);
var
  r: integer;
  mo: Pmobj_t;
  x, y: fixed_t;
begin
  r := P_Random;
  x := (10 * FRACUNIT) * ((r and 3) - (P_Random and 3)) + actor.x;
  r := P_Random;
  y := (10 * FRACUNIT) * ((r and 3) - (P_Random and 3)) + actor.y;

  mo := P_SpawnMobj(x, y, actor.z, Ord(MT_SPARKPUFF));
  P_SetMobjState(mo, S_BNG4_01); // 199
  mo.momz := FRACUNIT;
end;

//
// A_ProgrammerMelee
//
// villsa [STRIFE] new codepointer
// 09/08/10: Melee attack for the Programmer.
// haleyjd - fixed damage formula
//
procedure A_ProgrammerMelee(actor: Pmobj_t);
var
  damage: integer;
begin
  if actor.target = nil then
    exit;

  A_FaceTarget(actor);
  if P_CheckMeleeRange(actor) then
  begin
    damage := 6 * (P_Random mod 10 + 1);

    S_StartSound(actor, Ord(sfx_mtalht));
    P_DamageMobj(actor.target, actor, actor, damage);
  end;
end;

const
  TRACEANGLE = $E000000;  // villsa [STRIFE] changed from 0xC000000 to 0xE000000

procedure A_Tracer(actor: Pmobj_t);
var
  exact: angle_t;
  dist: fixed_t;
  slope: fixed_t;
  dest: Pmobj_t;
begin
  // adjust direction
  dest := actor.tracer;

  if (dest = nil) or (dest.health <= 0) then
    exit;

  // change angle
  exact := R_PointToAngle2(actor.x, actor.y, dest.x, dest.y);

  if exact <> actor.angle then
  begin
    if exact - actor.angle <= ANG180 then
    begin
      actor.angle := actor.angle + TRACEANGLE;
      if exact - actor.angle > ANG180 then
        actor.angle := exact;
    end
    else
    begin
      actor.angle := actor.angle - TRACEANGLE;
      if exact - actor.angle < ANG180 then
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

procedure A_Scream(actor: Pmobj_t);
begin
  A_DeathSound1(actor);
end;

procedure A_XScream(actor: Pmobj_t);
begin
  if actor.flags and MF_NOBLOOD <> 0 then
    if actor.info.deathsound <> 0 then
    begin
      A_DeathSound1(actor);
      exit;
    end;

  S_StartSound(actor, Ord(sfx_slop));
end;

//
// A_Pain
//
// villsa [STRIFE]
// * Play random peasant sounds; otherwise play painsound directly
//
// JVAL: adjusted for MF_EX_RANDOMPAINSOUND flag
//
procedure A_Pain(actor: Pmobj_t);
var
  sound: integer;
begin
  sound := actor.info.painsound;

  if (sound >= Ord(sfx_pespna)) and (sound <= Ord(sfx_pespnd)) then
  begin
    sound := Ord(sfx_pespna) + (P_Random mod 4);
    if actor.info.flags2_ex and MF2_EX_FULLVOLPAIN <> 0 then
     S_StartSound(nil, sound)
    else
     S_StartSound(actor, sound);
    exit;
  end;

  A_PainSound1(actor);
end;

//
// A_PeasantCrash
//
// villsa [STRIFE] new codepointer
// 09/08/10: Called from Peasant's "crash" state (not to be confused with
// Heretic crash states), which is invoked when the Peasant has taken
// critical but sub-fatal damage. It will "bleed out" the rest of its
// health by calling this function repeatedly.
//
procedure A_PeasantCrash(actor: Pmobj_t);
begin
  // Set NODIALOG, because you probably wouldn't feel like talking either
  // if somebody just stabbed you in the gut with a punch dagger...
  actor.flags := actor.flags or MF_NODIALOG;

  if P_Random mod 5 = 0 then
  begin
    A_Pain(actor);  // inlined in asm
    dec(actor.health);
  end;

  if actor.health <= 0 then
    P_KillMobj(actor.target, actor);
end;

//
// A_HideZombie
//
// villsa [STRIFE] new codepointer
// Used by the "Becoming" Acolytes on the Loremaster's level.
//
procedure A_HideZombie(actor: Pmobj_t);
var
  junk: line_t;
begin
  junk.tag := 999;
  EV_DoDoor(@junk, vld_blazeClose);

  if actor.target <> nil then
    if actor.target.player <> nil then
      P_NoiseAlert(actor.target, actor); // inlined in asm
end;

//
// A_MerchantPain
//
// villsa [STRIFE] new codepointer
// 09/08/10: Pain pointer for merchant characters. They close up shop for
// a while and set off the alarm.
//
procedure A_MerchantPain(actor: Pmobj_t);
var
  junk: line_t;
begin
  junk.tag := 999;
  EV_DoDoor(@junk, vld_shopClose);

  if actor.target <> nil then
    if actor.target.player <> nil then
      P_NoiseAlert(actor.target, actor); // inlined in asm
end;

//
// P_ThrustMobj
//
// villsa [STRIFE] new function
// Thrusts an thing in a specified force/direction
// Beware! This is inlined everywhere in the asm
//
procedure P_ThrustMobj(actor: Pmobj_t; angle: angle_t; force: fixed_t);
var
  an: angle_t;
begin
  an := angle div ANGLETOFINEUNIT;
  actor.momx := actor.momx + FixedMul(finecosine[an], force);
  actor.momy := actor.momy + FixedMul(finesine[an], force);
end;

//
// A_ProgrammerDie
//
// villsa [STRIFE] new codepointer
// 09/08/10: Action routine for the Programmer's grisly death. Spawns the
// separate mechanical base object and sends it flying off in some random
// direction.
//
procedure A_ProgrammerDie(actor: Pmobj_t);
var
  r: integer;
  an: angle_t;
  mo: Pmobj_t;
begin
  mo := P_SpawnMobj(actor.x, actor.y, actor.z + 24 * FRACUNIT, Ord(MT_PROGRAMMERBASE));

  // haleyjd 20110223: fix add w/ANG180
  r := P_Random;
  an := _SHLW(r - P_Random, 22) + actor.angle + ANG180;
  mo.angle := an;

  P_ThrustMobj(mo, an, mo.info.speed);  // inlined in asm

  mo.momz := P_Random * 512;
end;

//
// A_InqTossArm
//
// villsa [STRIFE] new codepointer
// 09/08/10: Inquisitor death action. Spawns an arm and tosses it.
//
procedure A_InqTossArm(actor: Pmobj_t);
var
  r: integer;
  an: angle_t;
  mo: Pmobj_t;
begin
  mo := P_SpawnMobj(actor.x, actor.y, actor.z + (24 * FRACUNIT), Ord(MT_INQARM));

  r := P_Random;
  an := _SHLW(r - P_Random, 22) + actor.angle - ANG90;
  mo.angle := an;

  P_ThrustMobj(mo, an, mo.info.speed);  // inlined in asm

  mo.momz := P_Random * 1024;
end;

//
// A_SpawnSpectreA
//
// villsa [STRIFE] new codepointer (unused)
// 09/08/10: Spawns Spectre A. Or would, if anything actually used this.
// This is evidence that the Programmer's spectre, which appears in the
// Catacombs in the final version, was originally meant to be spawned
// after his death.
//
procedure A_SpawnSpectreA(actor: Pmobj_t);
var
  mo: Pmobj_t;
begin
  mo := P_SpawnMobj(actor.x, actor.y, actor.z, Ord(MT_SPECTRE_A));
  mo.momz := P_Random * 512;
end;

//
// A_SpawnSpectreB
//
// villsa [STRIFE] new codepointer
// 09/08/10: Action function to spawn the Bishop's spectre.
//
procedure A_SpawnSpectreB(actor: Pmobj_t);
var
  mo: Pmobj_t;
begin
  mo := P_SpawnMobj(actor.x, actor.y, actor.z, Ord(MT_SPECTRE_B));
  mo.momz := P_Random * 512;
end;

//
// A_SpawnSpectreC
//
// villsa [STRIFE] new codepointer (unused)
// 09/08/10: Action function to spawn the Oracle's spectre. Also
// unused, because the Oracle's spectre is already present on the
// map and is awakened on his death. Also left over from the
// unreleased beta (and demo) versions.
//
procedure A_SpawnSpectreC(actor: Pmobj_t);
var
  mo: Pmobj_t;
begin
  mo := P_SpawnMobj(actor.x, actor.y, actor.z, Ord(MT_SPECTRE_C));
  mo.momz := P_Random * 512;
end;

//
// A_SpawnSpectreD
//
// villsa [STRIFE] new codepointer
// 09/08/10: Action function to spawn Macil's Spectre.
//
procedure A_SpawnSpectreD(actor: Pmobj_t);
var
  mo: Pmobj_t;
begin
  mo := P_SpawnMobj(actor.x, actor.y, actor.z, Ord(MT_SPECTRE_D));
  mo.momz := P_Random * 512;
end;

//
// A_SpawnSpectreE
//
// villsa [STRIFE] new codepointer
// 09/08/10: Action function to spawn the Loremaster's Spectre.
//
procedure A_SpawnSpectreE(actor: Pmobj_t);
var
  mo: Pmobj_t;
begin
  mo := P_SpawnMobj(actor.x, actor.y, actor.z, Ord(MT_SPECTRE_E));
  mo.momz := P_Random * 512;
end;

// [STRIFE] New statics - Remember the Entity's spawning position.
var
  entity_pos_x: fixed_t = 0;
  entity_pos_y: fixed_t = 0;
  entity_pos_z: fixed_t = 0;

//
// A_SpawnEntity
//
// villsa [STRIFE] new codepointer
// 09/08/10: You will fall on your knees before the True God, the One Light.
//
procedure A_SpawnEntity(actor: Pmobj_t);
var
  mo: Pmobj_t;
begin
  mo := P_SpawnMobj(actor.x, actor.y, actor.z + 70 * FRACUNIT, Ord(MT_ENTITY));
  mo.momz := 5 * FRACUNIT;

  entity_pos_x := mo.x;
  entity_pos_y := mo.y;
  entity_pos_z := mo.z;
end;

//
// A_EntityDeath
//
// [STRIFE]
// haleyjd 09/08/10: The death of the Entity's spectre brings forth
// three subentities, which are significantly less dangerous on their
// own but threatening together.
//
procedure A_EntityDeath(actor: Pmobj_t);
var
  subentity: Pmobj_t;
  an: angle_t;
  dist: fixed_t;
begin
  dist := 2 * mobjinfo[Ord(MT_SUBENTITY)].radius;

  // Subentity One
  an := actor.angle div ANGLETOFINEUNIT;
  subentity := P_SpawnMobj(FixedMul(finecosine[an], dist) + entity_pos_x,
                           FixedMul(finesine[an], dist) + entity_pos_y,
                           entity_pos_z, Ord(MT_SUBENTITY));
  subentity.target := actor.target;
  A_FaceTarget(subentity);
  P_ThrustMobj(subentity, subentity.angle, 5120000);

  // Subentity Two
  an := (actor.angle + ANG90) div ANGLETOFINEUNIT;
  subentity := P_SpawnMobj(FixedMul(finecosine[an], dist) + entity_pos_x,
                           FixedMul(finesine[an], dist) + entity_pos_y,
                            entity_pos_z, Ord(MT_SUBENTITY));
  subentity.target := actor.target;
  P_ThrustMobj(subentity, actor.angle + ANG90, 4);
  A_FaceTarget(subentity);

  // Subentity Three
  an := (actor.angle - ANG90) div ANGLETOFINEUNIT;
  subentity := P_SpawnMobj(FixedMul(finecosine[an], dist) + entity_pos_x,
                           FixedMul(finesine[an], dist) + entity_pos_y,
                           entity_pos_z, Ord(MT_SUBENTITY));
  subentity.target := actor.target;
  P_ThrustMobj(subentity, actor.angle - ANG90, 4);
  A_FaceTarget(subentity);
end;

//
// A_SpawnZombie
//
// villsa [STRIFE] new codepointer
//
procedure A_SpawnZombie(actor: Pmobj_t);
begin
  P_SpawnMobj(actor.x, actor.y, actor.z, Ord(MT_ZOMBIE));
end;

//
// A_ZombieInSpecialSector
//
// villsa [STRIFE] new codepointer
//
procedure A_ZombieInSpecialSector(actor: Pmobj_t);
var
  sector: Psector_t;
  force: fixed_t;
  angle: angle_t;
  tagval: integer;
begin
  sector := Psubsector_t(actor.subsector).sector;
  if actor.z <> sector.floorheight then
    exit;

  if sector.special <= 15 then
    P_DamageMobj(actor, nil, nil, 999)
  else if sector.special = 18 then
  begin
    tagval := sector.tag - 100;
    force := (tagval mod 10) * 4096;
    angle := _SHLW(tagval div 10, 29);
    P_ThrustMobj(actor, angle, force);  // inlined in asm
  end;
end;

//
// A_CrystalExplode
//
// villsa [STRIFE] new codepointer
// Throws out debris from the Power Crystal and sets its sector floorheight
// to the lowest surrounding floor (this is maybe the only time a direct
// level-changing action is done by an object in this fashion in any of
// the DOOM engine games... they usually call a line special instead)
//
procedure A_CrystalExplode(actor: Pmobj_t);
var
  sector: Psector_t;
  rubble: Pmobj_t;
  i, r: integer;
begin
  sector := Psubsector_t(actor.subsector).sector;
  sector.lightlevel := 0;
  sector.floorheight := P_FindLowestFloorSurrounding(sector);

  // spawn rubble
  for i := 0 to 7 do
  begin
    rubble := P_SpawnMobj(actor.x, actor.y, actor.z, Ord(MT_RUBBLE1) + i);
    r := P_Random;
    rubble.momx := ((r and $0f) - (P_Random and 7)) * FRACUNIT;
    r := P_Random;
    rubble.momy := ((r and 7) - (P_Random and 7)) * FRACUNIT;
    rubble.momz := ((P_Random and 3) + 7) * FRACUNIT;
  end;
end;

//
// P_FreePrisoners
//
// haleyjd 09/08/10: [STRIFE] New function
// * Called when the prisoners get freed, obviously. Gives a
//   message and awards quest token 13.
//
procedure P_FreePrisoners;
var
  i: integer;
  msg: string;
begin
  msg := DEH_GetString('You''ve freed the prisoners!');

  for i := 0 to MAXPLAYERS - 1 do
  begin
    P_GiveItemToPlayer(@players[i], Ord(SPR_TOKN), MT_TOKEN_QUEST13);
    players[i]._message := msg;
  end;
end;

//
// P_DestroyConverter
//
// haleyjd 09/08/10: [STRIFE] New function
// * Called when the converter is shut down in the factory.
//   Gives several items and a message.
//
procedure P_DestroyConverter;
var
  i: integer;
  msg: string;
begin
  msg := DEH_GetString('You''ve destroyed the Converter!');

  for i := 0 to MAXPLAYERS - 1 do
  begin
    P_GiveItemToPlayer(@players[i], Ord(SPR_TOKN), MT_TOKEN_QUEST25);
    P_GiveItemToPlayer(@players[i], Ord(SPR_TOKN), MT_TOKEN_STAMINA);
    P_GiveItemToPlayer(@players[i], Ord(SPR_TOKN), MT_TOKEN_NEW_ACCURACY);
    players[i]._message := msg;
  end;
end;

//
// A_QuestMsg
//
// villsa [STRIFE] new codepointer
// Displays text based on quest item's name
// Quest item is based on actor's speed
//
procedure A_QuestMsg(actor: Pmobj_t);
var
  name: string;
  i: integer;
  quest: LongWord;
begin
  // get name
  name := DEH_GetString(mobjinfo[Ord(MT_TOKEN_QUEST1) - 1 + actor.info.speed].name2);

  // give quest and display message to players
  quest := _SHLW(1, actor.info.speed - 1);
  for i := 0 to MAXPLAYERS - 1 do
  begin
    players[i]._message := name;
    players[i].questflags := players[i].questflags or quest;
  end;
end;

//
// A_ExtraLightOff
//
// villsa [STRIFE] new codepointer
// 09/08/10: Called by the Power Crystal to turn off the extended
// flash of light caused by its explosion.
//
procedure A_ExtraLightOff(actor: Pmobj_t);
var
  p: Pplayer_t;
begin
  if actor.target = nil then
    exit;

  p := actor.target.player;
  if p = nil then
    exit;

  p.extralight := 0;
end;

//
// A_CrystalRadiusAtk
//
// villsa [STRIFE] new codepointer
// 09/08/10: Called by the power crystal when it dies.
//
procedure A_CrystalRadiusAtk(actor: Pmobj_t);
var
  p: Pplayer_t;
begin
  P_RadiusAttack(actor, actor.target, 512);

  if actor.target = nil then
    exit;

  p := actor.target.player;
  if p = nil then
    exit;

  // set extralight to 5 for near full-bright
  p.extralight := 5;
end;

//
// A_DeathExplode5
//
// villsa [STRIFE] new codepointer
//
procedure P_DeathExplode(actor: Pmobj_t; damage: integer);
begin
  P_RadiusAttack(actor, actor.target, damage);
  if actor.target <> nil then
    if actor.target.player <> nil then
      P_NoiseAlert(actor.target, actor);
end;

procedure A_DeathExplode5(actor: Pmobj_t);
begin
  P_DeathExplode(actor, 192);
end;

//
// A_DeathExplode1
//
// villsa [STRIFE] new codepointer
//
procedure A_DeathExplode1(actor: Pmobj_t);
begin
  P_DeathExplode(actor, 128);
end;

//
// A_DeathExplode2
//
// villsa [STRIFE] new codepointer
//
procedure A_DeathExplode2(actor: Pmobj_t);
begin
  P_DeathExplode(actor, 64);
end;

//
// A_DeathExplode3
//
// villsa [STRIFE] new codepointer
//
procedure A_DeathExplode3(actor: Pmobj_t);
begin
  P_DeathExplode(actor, 32);
end;

//
// A_RaiseAlarm
//
// villsa [STRIFE] new codepointer
// 09/08/10: Set off the infamous alarm. This is just a noise alert.
//
procedure A_RaiseAlarm(actor: Pmobj_t);
begin
  if actor.target <> nil then
    if actor.target.player <> nil then
      P_NoiseAlert(actor.target, actor); // inlined in asm
end;

//
// A_MissileTick
// villsa [STRIFE] - new codepointer
//
procedure A_MissileTick(actor: Pmobj_t);
begin
  dec(actor.reactiontime);
  if actor.reactiontime <= 0 then
  begin
    P_ExplodeMissile(actor);
    actor.flags := actor.flags and not MF_MISSILE;
  end;
end;

//
// A_SpawnGrenadeFire
// villsa [STRIFE] - new codepointer
//
procedure A_SpawnGrenadeFire(actor: Pmobj_t);
begin
  P_SpawnMobj(actor.x, actor.y, actor.z, Ord(MT_PFLAME));
end;

//
// A_NodeChunk
//
// villsa [STRIFE] - new codepointer
// Throw out "nodes" from a spectral entity
//
procedure A_NodeChunk(actor: Pmobj_t);
var
  r: integer;
  mo: Pmobj_t;
begin
  mo := P_SpawnMobj(actor.x, actor.y, actor.z + 10 * FRACUNIT, Ord(MT_NODE));
  r := P_Random;
  mo.momx := ((r and $0f) - (P_Random and 7)) * FRACUNIT;
  r := P_Random;
  mo.momy := ((r and 7) - (P_Random and $0f)) * FRACUNIT;
  mo.momz := (P_Random and $0f) * FRACUNIT;
end;

//
// A_HeadChunk
//
// villsa [STRIFE] - new codepointer
// Throw out the little "eye"-like object from a spectral entity when it dies.
//
procedure A_HeadChunk(actor: Pmobj_t);
var
  r: integer;
  mo: Pmobj_t;
begin
  mo := P_SpawnMobj(actor.x, actor.y, actor.z + 10 * FRACUNIT, Ord(MT_SPECTREHEAD));
  r := P_Random;
  mo.momx := ((r and 7) - (P_Random and $0f)) * FRACUNIT;
  r := P_Random;
  mo.momy := ((r and $0f) - (P_Random and 7)) * FRACUNIT;
  mo.momz := (P_Random and 7) * FRACUNIT;
end;

//
// A_BurnSpread
// villsa [STRIFE] - new codepointer
//
procedure A_BurnSpread(actor: Pmobj_t);
var
  t: integer;
  mo: Pmobj_t;
  x, y: fixed_t;
begin
  actor.momz := actor.momz - (8 * FRACUNIT);

  t := P_Random;
  actor.momx := actor.momx + ((t and 3) - (P_Random and 3)) * FRACUNIT;
  t := P_Random;
  actor.momy := actor.momy + ((t and 3) - (P_Random and 3)) * FRACUNIT;

  S_StartSound(actor, Ord(sfx_lgfire));

  if actor.flags and MF_DROPPED <> 0 then
    exit; // not the parent

  // haleyjd 20110223: match order of calls in binary
  y := actor.y + (((P_Random + 12) and 31) * FRACUNIT);
  x := actor.x + (((P_Random + 12) and 31) * FRACUNIT);

  // spawn child
  mo := P_SpawnMobj(x, y, actor.z + (4 * FRACUNIT), Ord(MT_PFLAME));

  t := P_Random;
  mo.momx := mo.momx + ((t and 7) - (P_Random and 7)) * FRACUNIT;
  t := P_Random;
  mo.momy := mo.momy + ((t and 7) - (P_Random and 7)) * FRACUNIT;
  mo.momz := mo.momz - FRACUNIT;
  mo.flags := mo.flags or MF_DROPPED;
  mo.reactiontime := (P_Random and 3) + 2;
end;

//
// A_Explode
//
procedure A_Explode(thingy: Pmobj_t);
begin
  if thingy.info.flags_ex and MF_EX_CUSTOMEXPLODE <> 0 then
    P_RadiusAttackEx(thingy, thingy.target, thingy.info.explosiondamage, thingy.info.explosionradius)
  else if thingy.state.params <> nil then
    P_RadiusAttackEx(thingy, thingy.target, thingy.state.params.IntVal[0], thingy.state.params.IntVal[1])
  else
  begin
    P_RadiusAttack(thingy, thingy.target, 128);
    P_HitFloor(thingy);
  end;
end;

//
// A_BossDeath
//
// Possibly trigger special effects
// if on first boss level
//
// haleyjd 09/17/10: [STRIFE]
// * Modified to handle all Strife bosses.
//
procedure A_BossDeath(actor: Pmobj_t);
var
  th: Pthinker_t;
  mo: Pmobj_t;
  junk: line_t;
  i: integer;
begin
  // only the following types can be a boss:
  if actor._type <> Ord(MT_CRUSADER) then
    if actor._type <> Ord(MT_SPECTRE_A) then
      if actor._type <> Ord(MT_SPECTRE_B) then
        if actor._type <> Ord(MT_SPECTRE_C) then
          if actor._type <> Ord(MT_SPECTRE_D) then
            if actor._type <> Ord(MT_SPECTRE_E) then
              if actor._type <> Ord(MT_SUBENTITY) then
                if actor._type <> Ord(MT_PROGRAMMER) then
                  exit;

  // check for a living player
  i := 0;
  while i < MAXPLAYERS do
  begin
    if playeringame[i] and (players[i].health > 0) then
      break;
    inc(i);
  end;

  if i = MAXPLAYERS then
    exit; // everybody's dead.

  // check for a still living boss
  th := thinkercap.next;
  while th <> @thinkercap do
  begin
    if @th._function.acp1 = @P_MobjThinker then
    begin
      mo := Pmobj_t(th);
      if (mo <> actor) and (mo._type = actor._type) and (mo.health > 0) then
        exit; // one is still alive.
    end;
    th := th.next;
  end;

  // Victory!
  case actor._type of
    Ord(MT_CRUSADER):
      begin
        junk.tag := 667;
        EV_DoFloor(@junk, lowerFloorToLowest);
      end;

    Ord(MT_SPECTRE_A):
      begin
        P_GiveVoiceObjective('VOC95', 'LOG95', 0);
        junk.tag := 999;
        EV_DoFloor(@junk, lowerFloorToLowest);
      end;

    Ord(MT_SPECTRE_B):
      begin
        P_GiveItemToPlayer(@players[0], Ord(SPR_TOKN), MT_TOKEN_BISHOP);
        P_GiveVoiceObjective('VOC74', 'LOG74', 0);
      end;

    Ord(MT_SPECTRE_C):
      begin
        // Look for an MT_ORACLE - this is for in case the player awakened the
        // Oracle's spectre without killing the Oracle, which is possible by
        // looking up to max and firing the Sigil at it. If this were not done,
        // a serious sequence break possibility would arise where one could
        // kill both the Oracle AND Macil, possibly throwing the game out of
        // sorts entirely. Too bad they thought of it ;)  However this also
        // causes a bug sometimes! The Oracle, in its death state, sets the
        // Spectre C back to its seestate. If the Spectre C is already dead,
        // it becomes an undead ghost monster. Then it's a REAL spectre ;)
        th := thinkercap.next;
        while th <> @thinkercap do
        begin
          if @th._function.acp1 = @P_MobjThinker then
          begin
            mo := Pmobj_t(th);

            // KILL ALL ORACLES! RAWWR!
            if (mo <> actor) and (mo._type = Ord(MT_ORACLE)) and (mo.health > 0) then
              P_KillMobj(actor, mo);
          end;
          th := th.next;
        end;
        P_GiveItemToPlayer(@players[0], Ord(SPR_TOKN), MT_TOKEN_ORACLE);

        // Bishop is dead? - verify.
        if players[0].questflags and QF_QUEST21 <> 0 then
          P_GiveItemToPlayer(@players[0], Ord(SPR_TOKN), MT_TOKEN_QUEST22);

        // Macil is dead?
        if players[0].questflags and QF_QUEST24 <> 0 then
        begin
          // Loremaster is dead?
          if players[0].questflags and QF_QUEST26 <> 0 then
          begin
            // We wield the complete sigil, blahblah
            P_GiveVoiceObjective('VOC85', 'LOG85', 0);
          end;
        end
        else
        begin
          // So much for prognostication!
          P_GiveVoiceObjective('VOC87', 'LOG87', 0);
        end;
        junk.tag := 222;         // Open the exit door again;
        EV_DoDoor(@junk, vld_open); // Note this is NOT the Loremaster door...
      end;

    Ord(MT_SPECTRE_D):
      begin
        P_GiveItemToPlayer(@players[0], Ord(SPR_TOKN), MT_TOKEN_MACIL);
        if players[0].questflags and QF_QUEST25 <> 0 then // Destroyed converter?
          P_GiveVoiceObjective('VOC106', 'LOG106', 0)
        else
          P_GiveVoiceObjective('VOC79', 'LOG79', 0);
      end;

    Ord(MT_SPECTRE_E):
      begin
        P_GiveItemToPlayer(@players[0], Ord(SPR_TOKN), MT_TOKEN_LOREMASTER);
        if not netgame then
        begin
          P_GiveItemToPlayer(@players[0], Ord(SPR_TOKN), MT_TOKEN_STAMINA);
          P_GiveItemToPlayer(@players[0], Ord(SPR_TOKN), MT_TOKEN_NEW_ACCURACY);
        end;
        if players[0].sigiltype = 4 then
          P_GiveVoiceObjective('VOC85', 'LOG85', 0)
        else
          P_GiveVoiceObjective('VOC83', 'LOG83', 0);
        junk.tag := 666;
        EV_DoFloor(@junk, lowerFloorToLowest);
      end;

    Ord(MT_SUBENTITY):
      begin
        F_StartFinale();
      end;

    Ord(MT_PROGRAMMER):
      begin
        F_StartFinale();
        G_ExitLevel(0);
      end;

  end;
end;

//
// A_AcolyteSpecial
//
// villsa [STRIFE] - new codepointer
// Awards quest #7 when all the Blue Acolytes are killed in Tarnhill
//
procedure A_AcolyteSpecial(actor: Pmobj_t);
var
  i: integer;
  th: Pthinker_t;
  mo: Pmobj_t;
begin
  if actor._type <> Ord(MT_GUARD8) then
    exit; // must be MT_GUARD8

  i := 0;
  while i < MAXPLAYERS do
  begin
    if playeringame[i] and (players[i].health > 0) then
      break;
    inc(i);
  end;

  if i = 8 then
    exit;

  th := thinkercap.next;
  while th <> @thinkercap do
  begin
    if @th._function.acp1 = @P_MobjThinker then
    begin
      mo := Pmobj_t(th);
      // Found a living MT_GUARD8?
      if (mo <> actor) and (mo._type = actor._type) and (mo.health > 0) then
        exit;
    end;
    th := th.next;
  end;

  // All MT_GUARD8 are dead, give quest token #7 to all players
  for i := 0 to MAXPLAYERS - 1 do
    P_GiveItemToPlayer(@players[i], Ord(SPR_TOKN), MT_TOKEN_QUEST7);

  // play voice, give objective
  P_GiveVoiceObjective('VOC14', 'LOG14', 0);
end;

//
// A_InqChase
// villsa [STRIFE] - new codepointer
//
procedure A_InqChase(actor: Pmobj_t);
begin
  S_StartSound(actor, Ord(sfx_inqact));
  A_Chase(actor);
end;

//
// A_StalkerChase
// villsa [STRIFE] - new codepointer
//
procedure A_StalkerChase(actor: Pmobj_t);
begin
  S_StartSound(actor, Ord(sfx_spdwlk));
  A_Chase(actor);
end;

//
// A_PlayerScream
//
// [STRIFE]
// * Modified to eliminate gamemode check and to use Strife sound.
//
procedure A_PlayerScream(mo: Pmobj_t);
var
  sound: integer;
begin
  // villsa [STRIFE] don't check for gamemode
  if mo.health < -50 then
  // IF THE PLAYER DIES
  // LESS THAN -50% WITHOUT GIBBING
    sound := Ord(sfx_plxdth) // villsa [STRIFE] different sound
  else
    sound := Ord(sfx_pldeth);

  S_StartSound(mo, sound);
end;

//
// A_TeleportBeacon
//
// villsa [STRIFE] - new codepointer
//
procedure A_TeleportBeacon(actor: Pmobj_t);
var
  mobj, fog, targ: Pmobj_t;
  fog_x, fog_y: fixed_t;
begin
  if actor.target <> players[actor.miscdata].mo then
    actor.target := players[actor.miscdata].mo;

  mobj := P_SpawnMobj(actor.x, actor.y, ONFLOORZ, Ord(MT_REBEL1));

  // haleyjd 20141024: missing code from disassembly; transfer allegiance
  // originally from master player to the rebel.
  mobj.miscdata := actor.miscdata;

  if not P_TryMove(mobj, mobj.x, mobj.y) then
  begin
    // Rebel is probably stuck in something.. too bad
    P_RemoveMobj(mobj);
    exit;
  end;

  // beacon no longer special
  actor.flags := actor.flags and not MF_SPECIAL;

  // 20160306: set rebel threshold
  mobj.threshold := 100;

  // set color and flags
  mobj.flags := mobj.flags or ((actor.miscdata shl MF_TRANSSHIFT) or MF_NODIALOG);  // JVAL SOS maybe change flags to LongWord
  mobj.target := nil;

  // double Rebel's health in deathmatch mode
  if deathmatch <> 0 then
    mobj.health := mobj.health * 2;

  if actor.target <> nil then
  begin
    targ := actor.target.target;
    if targ <> nil then
      if (targ._type <> Ord(MT_REBEL1)) or (targ.miscdata <> mobj.miscdata) then
        mobj.target := targ;
  end;

  P_SetMobjState(mobj, statenum_t(mobj.info.seestate));
  mobj.angle := actor.angle;

  fog_x := mobj.x + FixedMul(20 * FRACUNIT, finecosine[actor.angle div ANGLETOFINEUNIT]);
  fog_y := mobj.y + FixedMul(20 * FRACUNIT, finesine[actor.angle div ANGLETOFINEUNIT]);

  fog := P_SpawnMobj(fog_x, fog_y, mobj.z, Ord(MT_TFOG));
  S_StartSound(fog, Ord(sfx_telept));

  dec(actor.health);
  if actor.health < 0 then
    P_RemoveMobj(actor);
end;

//
// A_BodyParts
//
// villsa [STRIFE] new codepointer
// 09/06/10: Spawns gibs when organic actors get splattered, or junk
// when robots explode.
//
procedure A_BodyParts(actor: Pmobj_t);
var
  typ: integer;
  mo: Pmobj_t;
  an: angle_t;
begin
  if actor.flags and MF_NOBLOOD <> 0 then // Robots are flagged NOBLOOD
    typ := Ord(MT_JUNK)
  else
    typ := Ord(MT_MEAT);

  mo := P_SpawnMobj(actor.x, actor.y, actor.z + (24 * FRACUNIT), typ);
  P_SetMobjState(mo, statenum_t(mo.info.spawnstate + (P_Random mod 19)));

  an := (P_Random * 8192) div 256;
  mo.angle := an * ANGLETOFINEUNIT;

  mo.momx := FixedMul(finecosine[an], (P_Random and $0f) * FRACUNIT);
  mo.momy := FixedMul(finesine[an], (P_Random and $0f) * FRACUNIT);
  mo.momz := (P_Random and $0f) * FRACUNIT;
end;

//
// A_ClaxonBlare
//
// [STRIFE] New function
// haleyjd 09/08/10: The ever-dreadful Strife alarm!
//
procedure A_ClaxonBlare(actor: Pmobj_t);
begin
  // Timer ran down?
  dec(actor.reactiontime);
  if actor.reactiontime < 0 then
  begin
    // reset to initial state
    actor.target := nil;
    actor.reactiontime := actor.info.reactiontime;

    // listen for more noise
    A_Listen(actor);

    // If we heard something, stay on for a while,
    // otherwise return to spawnstate.
    if actor.target <> nil then
      actor.reactiontime := 50
    else
      P_SetMobjState(actor, statenum_t(actor.info.spawnstate));
  end;

  // When almost ran down, clear the soundtarget so it doesn't
  // retrigger the alarm.
  // Also, play the harsh, grating claxon.
  if actor.reactiontime = 2 then
    Psubsector_t(actor.subsector).sector.soundtarget := nil
  else if actor.reactiontime > 50 then
    S_StartSound(actor, Ord(sfx_alarm));
end;

//
// A_ActiveSound
//
// villsa [STRIFE] new codepointer
// 09/06/10: Plays an object's active sound periodically.
//
// JVAL: Changed to A_ActiveSoundSTRF
procedure A_ActiveSoundSTRF(actor: Pmobj_t);
begin
  if actor.info.activesound <> 0 then
    if leveltime and 7 = 0 then // haleyjd: added parens
    begin
      if (actor.info.flags2_ex and MF2_EX_FULLVOLACTIVE <> 0) then
        S_StartSound(actor, actor.info.activesound)
      else
        S_StartSound(actor, actor.info.activesound);
    end;
end;

//
// A_ClearSoundTarget
//
// villsa [STRIFE] new codepointer
// 09/06/10: Clears the actor's sector soundtarget, so that the actor
// will not be continually alerted/awakened ad infinitum. Used by
// shopkeepers.
//
procedure A_ClearSoundTarget(actor: Pmobj_t);
begin
  Psubsector_t(actor.subsector).sector.soundtarget := nil;
end;

//
// A_DropBurnFlesh
//
// villsa [STRIFE] new codepointer
//
procedure A_DropBurnFlesh(actor: Pmobj_t);
var
  mo: Pmobj_t;
  typ: integer;
begin
  typ := actor._type;

  mo := P_SpawnMobj(actor.x, actor.y, actor.z + (24 * FRACUNIT), Ord(MT_BURNDROP));
  mo.momz := -FRACUNIT;

  actor._type := Ord(MT_SFIREBALL);
  P_RadiusAttack(actor, actor, 64);
  actor._type := typ;
end;

//
// A_FlameDeath
//
// villsa [STRIFE] new codepointer
// 09/06/10: Death animation for flamethrower fireballs.
//
procedure A_FlameDeath(actor: Pmobj_t);
begin
  A_NoGravity(actor);
  actor.momz := (P_Random and 3) * FRACUNIT;
end;

//
// A_ClearForceField
//
// villsa [STRIFE] new codepointer
// check for all matching lines in the sector
// and disable blocking/midtextures
//
procedure A_ClearForceField(actor: Pmobj_t);
var
  i: integer;
  sec: Psector_t;
  secline: Pline_t;
  sn: integer;
begin
  actor.flags := actor.flags and not (MF_SOLID or MF_SPECIAL);
  sec := Psubsector_t(actor.subsector).sector;

  for i := 0 to sec.linecount - 1 do
  begin
    secline := sec.lines[i];
    // BUG: will crash if 1S line has TWOSIDED flag!
    if secline.flags and ML_TWOSIDED = 0 then
      continue;
    if secline.special <> 148 then
      continue;

    secline.flags := secline.flags and not ML_BLOCKING;
    secline.special := 0;
    sn := secline.sidenum[0];
    if sn >= 0 then
      sides[sn].midtexture := 0;
    sn := secline.sidenum[1];
    if sn >= 0 then
      sides[sn].midtexture := 0;
  end;
end;

//----------------------------------------------------------------------------
//
// PROC P_Massacre
//
// Kills all monsters.
//
//----------------------------------------------------------------------------

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
        mo.flags := mo.flags or MF_SHOOTABLE;
        P_DamageMobj(mo, nil, nil, 10000);
      end;
    end;
   think := think.next;
  end;
end;

end.


