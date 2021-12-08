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
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit p_common;

interface

uses
  d_player,
  m_fixed,
  info_h,
  p_pspr_h,
  p_mobj_h;

const
  CSP_AT_LEAST = 1;
  CSP_AT_MOST = 2;

function P_CheckStateParams(actor: Pmobj_t; const numparms: integer = -1; const flags: LongWord = 0): boolean;

{$IFDEF HEXEN}
procedure P_BulletSlope(mo: Pmobj_t);
{$ENDIF}

procedure A_GoTo(actor: Pmobj_t);

procedure A_GoToIfCloser(actor: Pmobj_t);

procedure A_GoToIfHealthLower(actor: Pmobj_t);

procedure A_ConsoleCommand(actor: Pmobj_t);

procedure A_SetFrightened(actor: Pmobj_t);

procedure A_UnSetFrightened(actor: Pmobj_t);

procedure A_SetNoDamage(actor: Pmobj_t);

procedure A_UnSetNoDamage(actor: Pmobj_t);

procedure A_SetCustomParam(actor: Pmobj_t);

procedure A_AddCustomParam(actor: Pmobj_t);

procedure A_SubtractCustomParam(actor: Pmobj_t);

procedure A_SetTargetCustomParam(actor: Pmobj_t);

procedure A_AddTargetCustomParam(actor: Pmobj_t);

procedure A_SubtractTargetCustomParam(actor: Pmobj_t);

procedure A_JumpIf(actor: Pmobj_t);

procedure A_JumpIfCustomParam(actor: Pmobj_t);

procedure A_JumpIfCustomParamLess(actor: Pmobj_t);

procedure A_JumpIfCustomParamGreater(actor: Pmobj_t);

procedure A_JumpIfTargetCustomParam(actor: Pmobj_t);

procedure A_JumpIfTargetCustomParamLess(actor: Pmobj_t);

procedure A_JumpIfTargetCustomParamGreater(actor: Pmobj_t);

procedure A_JumpIfMapStringEqual(actor: Pmobj_t);
procedure A_JumpIfMapStringLess(actor: Pmobj_t);
procedure A_JumpIfMapStringGreater(actor: Pmobj_t);

procedure A_JumpIfMapIntegerEqual(actor: Pmobj_t);
procedure A_JumpIfMapIntegerLess(actor: Pmobj_t);
procedure A_JumpIfMapIntegerGreater(actor: Pmobj_t);

procedure A_JumpIfMapFloatEqual(actor: Pmobj_t);
procedure A_JumpIfMapFloatLess(actor: Pmobj_t);
procedure A_JumpIfMapFloatGreater(actor: Pmobj_t);

procedure A_JumpIfWorldStringEqual(actor: Pmobj_t);
procedure A_JumpIfWorldStringLess(actor: Pmobj_t);
procedure A_JumpIfWorldStringGreater(actor: Pmobj_t);

procedure A_JumpIfWorldIntegerEqual(actor: Pmobj_t);
procedure A_JumpIfWorldIntegerLess(actor: Pmobj_t);
procedure A_JumpIfWorldIntegerGreater(actor: Pmobj_t);

procedure A_JumpIfWorldFloatEqual(actor: Pmobj_t);
procedure A_JumpIfWorldFloatLess(actor: Pmobj_t);
procedure A_JumpIfWorldFloatGreater(actor: Pmobj_t);

procedure A_GoToIfCustomParam(actor: Pmobj_t);

procedure A_GoToIfCustomParamLess(actor: Pmobj_t);

procedure A_GoToIfCustomParamGreater(actor: Pmobj_t);

procedure A_GoToIfTargetCustomParam(actor: Pmobj_t);

procedure A_GoToIfTargetCustomParamLess(actor: Pmobj_t);

procedure A_GoToIfTargetCustomParamGreater(actor: Pmobj_t);

procedure A_GoToIfMapStringEqual(actor: Pmobj_t);
procedure A_GoToIfMapStringLess(actor: Pmobj_t);
procedure A_GoToIfMapStringGreater(actor: Pmobj_t);

procedure A_GoToIfMapIntegerEqual(actor: Pmobj_t);
procedure A_GoToIfMapIntegerLess(actor: Pmobj_t);
procedure A_GoToIfMapIntegerGreater(actor: Pmobj_t);

procedure A_GoToIfMapFloatEqual(actor: Pmobj_t);
procedure A_GoToIfMapFloatLess(actor: Pmobj_t);
procedure A_GoToIfMapFloatGreater(actor: Pmobj_t);

procedure A_GoToIfWorldStringEqual(actor: Pmobj_t);
procedure A_GoToIfWorldStringLess(actor: Pmobj_t);
procedure A_GoToIfWorldStringGreater(actor: Pmobj_t);

procedure A_GoToIfWorldIntegerEqual(actor: Pmobj_t);
procedure A_GoToIfWorldIntegerLess(actor: Pmobj_t);
procedure A_GoToIfWorldIntegerGreater(actor: Pmobj_t);

procedure A_GoToIfWorldFloatEqual(actor: Pmobj_t);
procedure A_GoToIfWorldFloatLess(actor: Pmobj_t);
procedure A_GoToIfWorldFloatGreater(actor: Pmobj_t);

procedure A_CustomSound1(mo: Pmobj_t);

procedure A_CustomSound2(mo: Pmobj_t);

procedure A_CustomSound3(mo: Pmobj_t);

procedure P_RandomSound(const actor: Pmobj_t; const soundnum: integer);

procedure A_RandomPainSound(actor: Pmobj_t);

procedure A_RandomSeeSound(actor: Pmobj_t);

procedure A_RandomAttackSound(actor: Pmobj_t);

procedure A_RandomDeathSound(actor: Pmobj_t);

procedure A_RandomActiveSound(actor: Pmobj_t);

procedure A_RandomCustomSound1(actor: Pmobj_t);

procedure A_RandomCustomSound2(actor: Pmobj_t);

procedure A_RandomCustomSound3(actor: Pmobj_t);

procedure A_RandomCustomSound(actor: Pmobj_t);

procedure A_RandomMeleeSound(actor: Pmobj_t);

procedure A_Playsound(actor: Pmobj_t);

procedure A_PlayWeaponsound(actor: Pmobj_t);

procedure A_RandomSound(actor: Pmobj_t);

procedure A_Stop(actor: Pmobj_t);

procedure A_Jump(actor: Pmobj_t);

procedure A_CustomMissile(actor: Pmobj_t);

procedure A_RandomMissile(actor: Pmobj_t);

procedure A_SpawnItem(actor: Pmobj_t);

procedure A_SpawnItemEx(actor: Pmobj_t);

procedure A_SeekerMissile(actor: Pmobj_t);

procedure A_CStaffMissileSlither(actor: Pmobj_t);

procedure A_SetTranslucent(actor: Pmobj_t);

procedure A_FadeOut(actor: Pmobj_t);

procedure A_FadeOut10(actor: Pmobj_t);

procedure A_FadeOut20(actor: Pmobj_t);

procedure A_FadeOut30(actor: Pmobj_t);

procedure A_FadeIn(actor: Pmobj_t);

procedure A_FadeIn10(actor: Pmobj_t);

procedure A_FadeIn20(actor: Pmobj_t);

procedure A_FadeIn30(actor: Pmobj_t);

procedure A_MissileAttack(actor: Pmobj_t);

procedure A_AdjustSideSpot(actor: Pmobj_t);

procedure A_ThrustZ(actor: Pmobj_t);

procedure A_ThrustXY(actor: Pmobj_t);

procedure A_Turn(actor: Pmobj_t);

procedure A_JumpIfCloser(actor: Pmobj_t);

procedure A_JumpIfHealthLower(actor: Pmobj_t);

procedure A_ScreamAndUnblock(actor: Pmobj_t);

procedure A_Missile(actor: Pmobj_t);

procedure A_NoMissile(actor: Pmobj_t);

procedure A_Wander(actor: Pmobj_t);

procedure A_GhostOn(actor: Pmobj_t);

procedure A_GhostOff(actor: Pmobj_t);

procedure A_Turn5(actor: Pmobj_t);

procedure A_Turn10(actor: Pmobj_t);

procedure A_Blocking(actor: Pmobj_t);

procedure A_DoNotRunScripts(actor: Pmobj_t);

procedure A_DoRunScripts(actor: Pmobj_t);

procedure A_SetDropItem(actor: Pmobj_t);

procedure A_SetDefaultDropItem(actor: Pmobj_t);

procedure A_TargetDropItem(actor: Pmobj_t);

procedure A_DefaultTargetDropItem(actor: Pmobj_t);

function P_ActorTarget(const actor: Pmobj_t): Pmobj_t;

procedure A_GlobalEarthQuake(actor: Pmobj_t);

procedure A_SetMapStr(actor: Pmobj_t);

procedure A_SetWorldStr(actor: Pmobj_t);

procedure A_SetMapInt(actor: Pmobj_t);

procedure A_SetWorldInt(actor: Pmobj_t);

procedure A_SetMapFloat(actor: Pmobj_t);

procedure A_SetWorldFloat(actor: Pmobj_t);

procedure A_RandomGoto(actor: Pmobj_t);

procedure A_ResetHealth(actor: Pmobj_t);

procedure A_SetHealth(actor: Pmobj_t);

procedure A_ResetTargetHealth(actor: Pmobj_t);

procedure A_SetTargetHealth(actor: Pmobj_t);

procedure A_Recoil(actor: Pmobj_t);

procedure A_SetSolid(actor: Pmobj_t);

procedure A_UnSetSolid(actor: Pmobj_t);

procedure A_SetFloat(actor: Pmobj_t);

procedure A_UnSetFloat(actor: Pmobj_t);

procedure A_ScaleVelocity(actor: Pmobj_t);

procedure A_ChangeVelocity(actor: Pmobj_t);

procedure A_SetPushFactor(actor: Pmobj_t);

procedure A_SetScale(actor: Pmobj_t);

procedure A_SetGravity(actor: Pmobj_t);

procedure A_SetFloorBounce(actor: Pmobj_t);

procedure A_UnSetFloorBounce(actor: Pmobj_t);

procedure A_SetCeilingBounce(actor: Pmobj_t);

procedure A_UnSetCeilingBounce(actor: Pmobj_t);

procedure A_SetWallBounce(actor: Pmobj_t);

procedure A_UnSetWallBounce(actor: Pmobj_t);

procedure A_GlowLight(actor: Pmobj_t);

procedure A_TraceNearestPlayer(actor: Pmobj_t);

procedure A_ChangeFlag(actor: Pmobj_t);

procedure A_CheckFloor(actor: Pmobj_t);

procedure A_CheckCeiling(actor: Pmobj_t);

procedure A_StopSound(actor: Pmobj_t);

procedure A_JumpIfTargetOutsideMeleeRange(actor: Pmobj_t);

procedure A_JumpIfTargetInsideMeleeRange(actor: Pmobj_t);

procedure A_JumpIfTracerCloser(actor: Pmobj_t);

procedure A_SetMass(actor: Pmobj_t);

procedure A_SetTargetMass(actor: Pmobj_t);

procedure A_SetTracerMass(actor: Pmobj_t);

procedure A_SetMasterMass(actor: Pmobj_t);

procedure A_CheckSight(actor: Pmobj_t);

procedure A_CheckSightOrRange(actor: Pmobj_t);

procedure A_CheckRange(actor: Pmobj_t);

procedure A_CountdownArg(actor: Pmobj_t);

procedure A_SetArg(actor: Pmobj_t);

procedure A_SetMasterArg(actor: Pmobj_t);

procedure A_SetTargetArg(actor: Pmobj_t);

procedure A_SetTracerArg(actor: Pmobj_t);

procedure A_SetSpecial(actor: Pmobj_t);

procedure A_CheckFlag(actor: Pmobj_t);

procedure A_SetAngle(actor: Pmobj_t);

procedure A_SetUserVar(actor: Pmobj_t);

procedure A_SetUserArray(actor: Pmobj_t);

procedure A_SetTics(actor: Pmobj_t);

procedure A_DropItem(actor: Pmobj_t);

procedure A_DamageSelf(actor: Pmobj_t);

procedure A_DamageTarget(actor: Pmobj_t);

procedure A_DamageTracer(actor: Pmobj_t);

procedure A_DamageMaster(actor: Pmobj_t);

procedure A_KillTarget(actor: Pmobj_t);

procedure A_KillTracer(actor: Pmobj_t);

procedure A_KillMaster(actor: Pmobj_t);

procedure A_RemoveTarget(actor: Pmobj_t);

procedure A_RemoveTracer(actor: Pmobj_t);

procedure A_RemoveMaster(actor: Pmobj_t);

procedure A_Remove(actor: Pmobj_t);

procedure A_SetFloatBobPhase(actor: Pmobj_t);

procedure A_Detonate(actor: Pmobj_t);

procedure A_Spawn(actor: Pmobj_t);

procedure A_Face(actor: Pmobj_t);

procedure A_Scratch(actor: Pmobj_t);

procedure A_RandomJump(obj: pointer; psp: Ppspdef_t);

{$IFNDEF HEXEN}
procedure A_LineEffect(actor: Pmobj_t);
{$ENDIF}

procedure A_FlipSprite(actor: Pmobj_t);

procedure A_RandomFlipSprite(actor: Pmobj_t);

procedure A_NoFlipSprite(actor: Pmobj_t);

procedure A_RandomNoFlipSprite(actor: Pmobj_t);

procedure A_CustomMeleeAttack(actor: Pmobj_t);

procedure A_CustomComboAttack(actor: Pmobj_t);

procedure A_SetRenderStyle(actor: Pmobj_t);

procedure A_FadeTo(actor: Pmobj_t);

procedure A_SetSize(actor: Pmobj_t);

procedure A_RaiseMaster(actor: Pmobj_t);

procedure A_RaiseChildren(actor: Pmobj_t);

procedure A_RaiseSiblings(actor: Pmobj_t);

procedure A_HealThing(actor: Pmobj_t);

procedure A_BasicAttack(actor: Pmobj_t);

procedure A_Tracer2(actor: Pmobj_t);

procedure A_MonsterRefire(actor: Pmobj_t);

procedure A_RearrangePointers(actor: Pmobj_t);

procedure A_TransferPointer(actor: Pmobj_t);

procedure A_AlertMonsters(actor: Pmobj_t);

procedure A_LocalEarthQuake(actor: Pmobj_t);

procedure A_RemoveChildren(actor: Pmobj_t);

procedure A_RemoveSiblings(actor: Pmobj_t);

procedure A_KillChildren(actor: Pmobj_t);

procedure A_KillSiblings(actor: Pmobj_t);

procedure A_Weave(actor: Pmobj_t);

procedure A_SetWeaveIndexXY(actor: Pmobj_t);

procedure A_SetWeaveIndexZ(actor: Pmobj_t);

procedure A_SetWeaveIndexes(actor: Pmobj_t);

procedure A_SetSpriteDX(actor: Pmobj_t);

procedure A_SetSpriteDY(actor: Pmobj_t);

procedure A_SetHeight(actor: Pmobj_t);

procedure A_SetFriction(actor: Pmobj_t);

procedure A_PlayerHurtExplode(actor: Pmobj_t);

procedure A_SetPainChance(actor: Pmobj_t);

procedure A_SetPushable(actor: Pmobj_t);

procedure A_UnSetPushable(actor: Pmobj_t);

procedure A_MatchTargetZ(actor: Pmobj_t);

procedure A_SetInteractive(actor: Pmobj_t);

procedure A_UnSetInteractive(actor: Pmobj_t);

const
  FLOATBOBSIZE = 64;
  FLOATBOBMASK = FLOATBOBSIZE - 1;

  FloatBobOffsets: array[0..FLOATBOBSIZE - 1] of fixed_t = (
         0,  51389, 102283, 152192,
    200636, 247147, 291278, 332604,
    370727, 405280, 435929, 462380,
    484378, 501712, 514213, 521763,
    524287, 521763, 514213, 501712,
    484378, 462380, 435929, 405280,
    370727, 332604, 291278, 247147,
    200636, 152192, 102283,  51389,
        -1, -51390,-102284,-152193,
   -200637,-247148,-291279,-332605,
   -370728,-405281,-435930,-462381,
   -484380,-501713,-514215,-521764,
   -524288,-521764,-514214,-501713,
   -484379,-462381,-435930,-405280,
   -370728,-332605,-291279,-247148,
   -200637,-152193,-102284, -51389
  );

const
// Sector Flags
// Ladder
  SF_LADDER = 1;
// Slip while descenting if sloped
  SF_SLIPSLOPEDESCENT = 2;

// A_SpawnItemEx Flags
const
  SIXF_TRANSFERTRANSLATION = 1;
  SIXF_ABSOLUTEPOSITION = 2;
  SIXF_ABSOLUTEANGLE = 4;
  SIXF_ABSOLUTEMOMENTUM = 8;
  SIXF_SETMASTER = 16;
  SIXF_NOCHECKPOSITION = 32;
  SIXF_TELEFRAG = 64;
  // 128 is used by Skulltag!
  SIXF_TRANSFERAMBUSHFLAG = 256;
  SIXF_TRANSFERPITCH = $200;
  SIXF_TRANSFERPOINTERS = $400;
  SIXF_USEBLOODCOLOR = $800;
  SIXF_CLEARCALLERTID = $1000;
  SIXF_MULTIPLYSPEED = $2000;
  SIXF_TRANSFERSCALE = $4000;
  SIXF_TRANSFERSPECIAL = $8000;
  SIXF_CLEARCALLERSPECIAL = $10000;
  SIXF_TRANSFERSTENCILCOL = $20000;
  SIXF_TRANSFERALPHA = $40000;
  SIXF_TRANSFERRENDERSTYLE = $80000;
  SIXF_SETTARGET = $100000;
  SIXF_SETTRACER = $200000;
  SIXF_NOPOINTERS = $400000;
  SIXF_ORIGINATOR = $800000;
  SIXF_TRANSFERSPRITEFRAME = $1000000;
  SIXF_TRANSFERROLL = $2000000;
  SIXF_ISTARGET = $4000000;
  SIXF_ISMASTER = $8000000;
  SIXF_ISTRACER = $10000000;
  SIXF_DROPPED = $20000000;

// A_CustomMissile
const
  CMF_AIMOFFSET = 1;
  CMF_AIMDIRECTION = 2;
  CMF_TRACKOWNER = 4;
  CMF_CHECKTARGETDEAD = 8;
  CMF_ABSOLUTEPITCH = 16;
  CMF_OFFSETPITCH = 32;
  CMF_SAVEPITCH = 64;
  CMF_ABSOLUTEANGLE = 128;

// P_DoRemoveThing
const
  RMVF_MISSILES = 0;
  RMVF_NOMONSTERS = 1;
  RMVF_MISC = 2;
  RMVF_EVERYTHING = 4;
  RMVF_EXFILTER = 8;
  RMVF_EXSPECIES = 16;
  RMVF_EITHER = 32;

const
  SPF_FORCECLAMP = 1; // players always clamp
  SPF_INTERPOLATE = 2;

const
  FTF_REMOVE = 1;
  FTF_CLAMP = 2;

// Flags for A_AlertMonsters
const
  AMF_TARGETEMITTER = 1;
  AMF_TARGETNONPLAYER = 2;
  AMF_EMITFROMTARGET = 4;

function P_TicsFromState(const st: Pstate_t): integer;

procedure P_SetMobjRelativeState(const mo: Pmobj_t; const offset: integer);

function PlayerToId(const p: Pplayer_t): integer;

procedure P_CopyFriendliness(const originator, mo: Pmobj_t);

function P_RaiseActor(const thing, raiser: Pmobj_t): boolean;

function P_FloatSpeed(const actor: Pmobj_t): fixed_t;

procedure P_NoiseAlertEx(target: Pmobj_t; emmiter: Pmobj_t; const maxdist: fixed_t);

procedure P_LocalEarthQuake(const actor: Pmobj_t; const tics: integer; const intensity: fixed_t; const maxdist: fixed_t);

implementation

uses
  d_delphi,
  doomdata,
  doomdef,
  deh_main,
  d_think,
  m_vectors,
  i_system,
  c_con,
  g_game,
  info,
  info_common,
  info_rnd,
  p_aaptr,
  p_enemy,
  p_extra,
  p_inter,
  p_local,
  p_mobj,
  p_pspr,
  p_map,
  p_maputl,
  p_params,
  p_setup,
  p_sight,
  p_spec,
  p_switch,
  p_tick,
  psi_globals,
  r_renderstyle,
  r_defs,
  r_main,
  sc_engine,
  sc_tokens,
  sc_states,
  tables,
  s_sound,
  sounds,
  m_rnd;

{$IFDEF HEXEN}
//
// P_BulletSlope
// Sets a slope so a near miss is at aproximately
// the height of the intended target
//
var
  bulletslope: fixed_t;


procedure P_BulletSlope(mo: Pmobj_t);
var
  an: angle_t;
begin
  // see which target is to be aimed at
  an := mo.angle;
  bulletslope := P_AimLineAttack(mo, an, 16 * 64 * FRACUNIT);

  if linetarget = nil then
  begin
    an := an + $4000000;
    bulletslope := P_AimLineAttack (mo, an, 16 * 64 * FRACUNIT);
    if linetarget = nil then
    begin
      an := an - $8000000;
      bulletslope := P_AimLineAttack(mo, an, 16 * 64 * FRACUNIT);
      if linetarget = nil then
        bulletslope := (Pplayer_t(mo.player).lookdir * FRACUNIT) div 173;
    end;
  end;
end;
{$ENDIF}

function P_CheckStateParams(actor: Pmobj_t; const numparms: integer = -1; const flags: LongWord = 0): boolean;
begin
  if numparms = 0 then
  begin
    if actor.state.flags_ex and MF_EX_STATE_PARAMS_ERROR = 0 then
    begin
      I_Warning('P_CheckStateParams(): Expected params can not be 0'#13#10);
      actor.state.flags_ex := actor.state.flags_ex or MF_EX_STATE_PARAMS_ERROR;
    end;
    result := false;
    exit;
  end;

  if actor.state.params = nil then
  begin
    if actor.state.flags_ex and MF_EX_STATE_PARAMS_ERROR = 0 then
    begin
      I_Warning('P_CheckStateParams(): Parameter list is null');
      if numparms > 0 then
        I_Warning(', %d parameters expected', [numparms]);
      I_Warning(#13#10);
      actor.state.flags_ex := actor.state.flags_ex or MF_EX_STATE_PARAMS_ERROR;
    end;
    result := false;
    exit;
  end;

  if numparms <> -1 then
  begin
    if (flags = 0) and (actor.state.params.Count <> numparms) then
    begin
      if actor.state.flags_ex and MF_EX_STATE_PARAMS_ERROR = 0 then
      begin
        I_Warning('P_CheckStateParams(): Parameter list has %d parameters, but %d parameters expected'#13#10, [actor.state.params.Count, numparms]);
        actor.state.flags_ex := actor.state.flags_ex or MF_EX_STATE_PARAMS_ERROR;
      end;
      result := false;
      exit;
    end
    else if (flags and CSP_AT_LEAST <> 0) and (actor.state.params.Count < numparms) then
    begin
      if actor.state.flags_ex and MF_EX_STATE_PARAMS_ERROR = 0 then
      begin
        I_Warning('P_CheckStateParams(): Parameter list has %d parameters, but at least %d parameters expected'#13#10, [actor.state.params.Count, numparms]);
        actor.state.flags_ex := actor.state.flags_ex or MF_EX_STATE_PARAMS_ERROR;
      end;
      result := false;
      exit;
    end
    else if (flags and CSP_AT_MOST <> 0) and (actor.state.params.Count > numparms) then
    begin
      if actor.state.flags_ex and MF_EX_STATE_PARAMS_ERROR = 0 then
      begin
        I_Warning('P_CheckStateParams(): Parameter list has %d parameters, but at most %d parameters expected'#13#10, [actor.state.params.Count, numparms]);
        actor.state.flags_ex := actor.state.flags_ex or MF_EX_STATE_PARAMS_ERROR;
      end;
      result := false;
      exit;
    end;
  end;

  result := true;
end;

//
// JVAL
// Change state
// A_GoTo(propability, newstate)
//
procedure A_GoTo(actor: Pmobj_t);
var
  propability: integer;
  newstate: integer;
begin
  if not P_CheckStateParams(actor, 2) then
    exit;

  propability := actor.state.params.IntVal[0];  // JVAL simple integer values are precalculated

  if N_Random < propability then
  begin
    if not actor.state.params.IsComputed[1] then
      actor.state.params.IntVal[1] := P_GetStateFromName(actor, actor.state.params.StrVal[1]);
    newstate := actor.state.params.IntVal[1];

    P_SetMobjState(actor, statenum_t(newstate));
  end;
end;

//
// JVAL
// A_GoToIfCloser(distancetotarget: float, newstate: integer)
// Jump conditionally to another state if distance to target is closer to first parameter
//
procedure A_GoToIfCloser(actor: Pmobj_t);
var
  dist: fixed_t;
  target: Pmobj_t;
  newstate: integer;
begin
  if not P_CheckStateParams(actor, 2) then
    exit;

  if actor.player = nil then
    target := actor.target
  else
  begin
    // Does the player aim at something that can be shot?
    P_BulletSlope(actor);
    target := linetarget;
  end;

  // No target - no jump
  if target = nil then
    exit;

  dist := actor.state.params.FixedVal[0];
  if P_AproxDistance(actor.x - target.x, actor.y - target.y) < dist then
  begin
    if not actor.state.params.IsComputed[1] then
      actor.state.params.IntVal[1] := P_GetStateFromName(actor, actor.state.params.StrVal[1]);
    newstate := actor.state.params.IntVal[1];

    P_SetMobjState(actor, statenum_t(newstate));
  end;
end;

//
// JVAL
// A_GoToIfHealthLower(health: integer; newstate: integer)
// Jump conditionally to another state if health is lower to first parameter
//
procedure A_GoToIfHealthLower(actor: Pmobj_t);
var
  newstate: integer;
begin
  if not P_CheckStateParams(actor, 2) then
    exit;

  if actor.health < actor.state.params.IntVal[0] then
  begin
    if not actor.state.params.IsComputed[1] then
      actor.state.params.IntVal[1] := P_GetStateFromName(actor, actor.state.params.StrVal[1]);
    newstate := actor.state.params.IntVal[1];

    P_SetMobjState(actor, statenum_t(newstate));
  end;
end;

procedure A_ConsoleCommand(actor: Pmobj_t);
var
  cmd: string;
  i: integer;
begin
  if not P_CheckStateParams(actor) then
    exit;

  cmd := actor.state.params.StrVal[0];
  for i := 1 to actor.state.params.Count - 1 do
    cmd := cmd + ' ' + actor.state.params.StrVal[i];

  C_AddCommand(cmd);
end;

procedure A_SetFrightened(actor: Pmobj_t);
begin
  actor.flags2_ex := actor.flags2_ex or MF2_EX_FRIGHTENED;
end;

procedure A_UnSetFrightened(actor: Pmobj_t);
begin
  actor.flags2_ex := actor.flags2_ex and not MF2_EX_FRIGHTENED;
end;

procedure A_SetNoDamage(actor: Pmobj_t);
begin
  actor.flags2_ex := actor.flags2_ex or MF2_EX_NODAMAGE;
end;

procedure A_UnSetNoDamage(actor: Pmobj_t);
begin
  actor.flags2_ex := actor.flags2_ex and not MF2_EX_NODAMAGE;
end;

procedure A_SetCustomParam(actor: Pmobj_t);
begin
  if not P_CheckStateParams(actor, 2) then
    exit;

  P_SetMobjCustomParam(actor, actor.state.params.StrVal[0], actor.state.params.IntVal[1]);
end;

procedure A_AddCustomParam(actor: Pmobj_t);
var
  parm: Pmobjcustomparam_t;
begin
  if not P_CheckStateParams(actor, 2) then
    exit;

  parm := P_GetMobjCustomParam(actor, actor.state.params.StrVal[0]);
  if parm = nil then
    P_SetMobjCustomParam(actor, actor.state.params.StrVal[0], actor.state.params.IntVal[1])
  else
    P_SetMobjCustomParam(actor, actor.state.params.StrVal[0], parm.value + actor.state.params.IntVal[1])
end;

procedure A_SubtractCustomParam(actor: Pmobj_t);
var
  parm: Pmobjcustomparam_t;
begin
  if not P_CheckStateParams(actor, 2) then
    exit;

  parm := P_GetMobjCustomParam(actor, actor.state.params.StrVal[0]);
  if parm <> nil then
    P_SetMobjCustomParam(actor, actor.state.params.StrVal[0], parm.value - actor.state.params.IntVal[1])
  else
    P_SetMobjCustomParam(actor, actor.state.params.StrVal[0], - actor.state.params.IntVal[1])
end;

procedure A_SetTargetCustomParam(actor: Pmobj_t);
begin
  if not P_CheckStateParams(actor, 2) then
    exit;

  if actor.target = nil then
    exit;

  P_SetMobjCustomParam(actor.target, actor.state.params.StrVal[0], actor.state.params.IntVal[1]);
end;

procedure A_AddTargetCustomParam(actor: Pmobj_t);
var
  parm: Pmobjcustomparam_t;
begin
  if not P_CheckStateParams(actor, 2) then
    exit;

  if actor.target = nil then
    exit;

  parm := P_GetMobjCustomParam(actor.target, actor.state.params.StrVal[0]);
  if parm = nil then
    P_SetMobjCustomParam(actor.target, actor.state.params.StrVal[0], actor.state.params.IntVal[1])
  else
    P_SetMobjCustomParam(actor.target, actor.state.params.StrVal[0], parm.value + actor.state.params.IntVal[1])
end;

procedure A_SubtractTargetCustomParam(actor: Pmobj_t);
var
  parm: Pmobjcustomparam_t;
begin
  if not P_CheckStateParams(actor, 2) then
    exit;

  if actor.target = nil then
    exit;

  parm := P_GetMobjCustomParam(actor.target, actor.state.params.StrVal[0]);
  if parm <> nil then
    P_SetMobjCustomParam(actor.target, actor.state.params.StrVal[0], parm.value - actor.state.params.IntVal[1])
  else
    P_SetMobjCustomParam(actor.target, actor.state.params.StrVal[0], - actor.state.params.IntVal[1])
end;

//
// JVAL
// Conditionally change state offset
// A_JumpIf(logical expression, offset to jump when true)
//
procedure A_JumpIf(actor: Pmobj_t);
var
  offset: integer;
  boolret: boolean;
  N: TDNumberList;
  i: integer;
begin
  if not P_CheckStateParams(actor, 2, CSP_AT_LEAST) then
    exit;

  boolret := actor.state.params.BoolVal[0];
  if boolret then
  begin
    N := TDNumberList.Create;
    for i := 1 to actor.state.params.Count - 1 do
    begin
      offset := P_GetStateFromNameWithOffsetCheck(actor, actor.state.params.StrVal[i]);
      N.Add(offset);
    end;
    if N.Count > 0 then
    begin
      offset := N.Numbers[N_Random mod N.Count];
      if @states[offset] <> actor.state then
        P_SetMobjState(actor, statenum_t(offset));
    end;
    N.Free;
  end;
end;

//
// JVAL
// Change state offset
// A_JumpIfCustomParam(customparam, value of customparam, offset)
//
procedure A_JumpIfCustomParam(actor: Pmobj_t);
var
  offset: integer;
begin
  if not P_CheckStateParams(actor, 3) then
    exit;

  if P_GetMobjCustomParamValue(actor, actor.state.params.StrVal[0]) = actor.state.params.IntVal[1] then
  begin
    offset := P_GetStateFromNameWithOffsetCheck(actor, actor.state.params.StrVal[2]);
    if @states[offset] <> actor.state then
      P_SetMobjState(actor, statenum_t(offset));
  end;
end;

//
// JVAL
// Change state offset
// A_JumpIfCustomParamLess(customparam, value of customparam, offset)
//
procedure A_JumpIfCustomParamLess(actor: Pmobj_t);
var
  offset: integer;
begin
  if not P_CheckStateParams(actor, 3) then
    exit;

  if P_GetMobjCustomParamValue(actor, actor.state.params.StrVal[0]) < actor.state.params.IntVal[1] then
  begin
    offset := P_GetStateFromNameWithOffsetCheck(actor, actor.state.params.StrVal[2]);
    if @states[offset] <> actor.state then
      P_SetMobjState(actor, statenum_t(offset));
  end;
end;

//
// JVAL
// Change state offset
// A_JumpIfCustomParamGreater(customparam, value of customparam, offset)
//
procedure A_JumpIfCustomParamGreater(actor: Pmobj_t);
var
  offset: integer;
begin
  if not P_CheckStateParams(actor, 3) then
    exit;

  if P_GetMobjCustomParamValue(actor, actor.state.params.StrVal[0]) > actor.state.params.IntVal[1] then
  begin
    offset := P_GetStateFromNameWithOffsetCheck(actor, actor.state.params.StrVal[2]);
    if @states[offset] <> actor.state then
      P_SetMobjState(actor, statenum_t(offset));
  end;
end;

//
// JVAL
// Change state offset
// A_JumpIfTargetCustomParam(customparam, value of customparam, offset)
//
procedure A_JumpIfTargetCustomParam(actor: Pmobj_t);
var
  offset: integer;
begin
  if actor.target = nil then
    exit;

  if not P_CheckStateParams(actor, 3) then
    exit;

  if P_GetMobjCustomParamValue(actor.target, actor.state.params.StrVal[0]) = actor.state.params.IntVal[1] then
  begin
    offset := P_GetStateFromNameWithOffsetCheck(actor, actor.state.params.StrVal[2]);
    if @states[offset] <> actor.state then
      P_SetMobjState(actor, statenum_t(offset));
  end;
end;

//
// JVAL
// Change state offset
// A_JumpIfTargetCustomParamLess(customparam, value of customparam, offset)
//
procedure A_JumpIfTargetCustomParamLess(actor: Pmobj_t);
var
  offset: integer;
begin
  if actor.target = nil then
    exit;

  if not P_CheckStateParams(actor, 3) then
    exit;

  if P_GetMobjCustomParamValue(actor.target, actor.state.params.StrVal[0]) < actor.state.params.IntVal[1] then
  begin
    offset := P_GetStateFromNameWithOffsetCheck(actor, actor.state.params.StrVal[2]);
    if @states[offset] <> actor.state then
      P_SetMobjState(actor, statenum_t(offset));
  end;
end;

//
// JVAL
// Change state offset
// A_JumpIfTargetCustomParamGreater(customparam, value of customparam, offset)
//
procedure A_JumpIfTargetCustomParamGreater(actor: Pmobj_t);
var
  offset: integer;
begin
  if actor.target = nil then
    exit;

  if not P_CheckStateParams(actor, 3) then
    exit;

  if P_GetMobjCustomParamValue(actor.target, actor.state.params.StrVal[0]) > actor.state.params.IntVal[1] then
  begin
    offset := P_GetStateFromNameWithOffsetCheck(actor, actor.state.params.StrVal[2]);
    if @states[offset] <> actor.state then
      P_SetMobjState(actor, statenum_t(offset));
  end;
end;

procedure A_JumpIfMapStringEqual(actor: Pmobj_t);
var
  offset: integer;
  cur: integer;
begin
  if actor.target = nil then
    exit;

  if not P_CheckStateParams(actor, 3) then
    exit;

  if mapvars.StrVal[actor.state.params.StrVal[0]] = actor.state.params.StrVal[1] then
  begin
    offset := actor.state.params.IntVal[2];

    cur := (integer(actor.state) - integer(states)) div SizeOf(state_t);

    P_SetMobjState(actor, statenum_t(cur + offset));
  end;
end;

procedure A_JumpIfMapStringLess(actor: Pmobj_t);
var
  offset: integer;
begin
  if not P_CheckStateParams(actor, 3) then
    exit;

  if mapvars.StrVal[actor.state.params.StrVal[0]] < actor.state.params.StrVal[1] then
  begin
    offset := P_GetStateFromNameWithOffsetCheck(actor, actor.state.params.StrVal[2]);
    if @states[offset] <> actor.state then
      P_SetMobjState(actor, statenum_t(offset));
  end;
end;

procedure A_JumpIfMapStringGreater(actor: Pmobj_t);
var
  offset: integer;
  cur: integer;
begin
  if not P_CheckStateParams(actor, 3) then
    exit;

  if mapvars.StrVal[actor.state.params.StrVal[0]] > actor.state.params.StrVal[1] then
  begin
    offset := actor.state.params.IntVal[2];

    cur := (integer(actor.state) - integer(states)) div SizeOf(state_t);

    P_SetMobjState(actor, statenum_t(cur + offset));
  end;
end;

procedure A_JumpIfMapIntegerEqual(actor: Pmobj_t);
var
  offset: integer;
begin
  if not P_CheckStateParams(actor, 3) then
    exit;

  if mapvars.IntVal[actor.state.params.StrVal[0]] = actor.state.params.IntVal[1] then
  begin
    offset := P_GetStateFromNameWithOffsetCheck(actor, actor.state.params.StrVal[2]);
    if @states[offset] <> actor.state then
      P_SetMobjState(actor, statenum_t(offset));
  end;
end;

procedure A_JumpIfMapIntegerLess(actor: Pmobj_t);
var
  offset: integer;
  cur: integer;
begin
  if not P_CheckStateParams(actor, 3) then
    exit;

  if mapvars.IntVal[actor.state.params.StrVal[0]] < actor.state.params.IntVal[1] then
  begin
    offset := actor.state.params.IntVal[2];

    cur := (integer(actor.state) - integer(states)) div SizeOf(state_t);

    P_SetMobjState(actor, statenum_t(cur + offset));
  end;
end;

procedure A_JumpIfMapIntegerGreater(actor: Pmobj_t);
var
  offset: integer;
begin
  if not P_CheckStateParams(actor, 3) then
    exit;

  if mapvars.IntVal[actor.state.params.StrVal[0]] > actor.state.params.IntVal[1] then
  begin
    offset := P_GetStateFromNameWithOffsetCheck(actor, actor.state.params.StrVal[2]);
    if @states[offset] <> actor.state then
      P_SetMobjState(actor, statenum_t(offset));
  end;
end;

procedure A_JumpIfMapFloatEqual(actor: Pmobj_t);
var
  offset: integer;
begin
  if not P_CheckStateParams(actor, 3) then
    exit;

  if mapvars.FloatVal[actor.state.params.StrVal[0]] = actor.state.params.FloatVal[1] then
  begin
    offset := P_GetStateFromNameWithOffsetCheck(actor, actor.state.params.StrVal[2]);
    if @states[offset] <> actor.state then
      P_SetMobjState(actor, statenum_t(offset));
  end;
end;

procedure A_JumpIfMapFloatLess(actor: Pmobj_t);
var
  offset: integer;
begin
  if not P_CheckStateParams(actor, 3) then
    exit;

  if mapvars.FloatVal[actor.state.params.StrVal[0]] < actor.state.params.FloatVal[1] then
  begin
    offset := P_GetStateFromNameWithOffsetCheck(actor, actor.state.params.StrVal[2]);
    if @states[offset] <> actor.state then
      P_SetMobjState(actor, statenum_t(offset));
  end;
end;

procedure A_JumpIfMapFloatGreater(actor: Pmobj_t);
var
  offset: integer;
begin
  if not P_CheckStateParams(actor, 3) then
    exit;

  if mapvars.FloatVal[actor.state.params.StrVal[0]] > actor.state.params.FloatVal[1] then
  begin
    offset := P_GetStateFromNameWithOffsetCheck(actor, actor.state.params.StrVal[2]);
    if @states[offset] <> actor.state then
      P_SetMobjState(actor, statenum_t(offset));
  end;
end;

procedure A_JumpIfWorldStringEqual(actor: Pmobj_t);
var
  offset: integer;
begin
  if not P_CheckStateParams(actor, 3) then
    exit;

  if Worldvars.StrVal[actor.state.params.StrVal[0]] = actor.state.params.StrVal[1] then
  begin
    offset := P_GetStateFromNameWithOffsetCheck(actor, actor.state.params.StrVal[2]);
    if @states[offset] <> actor.state then
      P_SetMobjState(actor, statenum_t(offset));
  end;
end;

procedure A_JumpIfWorldStringLess(actor: Pmobj_t);
var
  offset: integer;
begin
  if not P_CheckStateParams(actor, 3) then
    exit;

  if Worldvars.StrVal[actor.state.params.StrVal[0]] < actor.state.params.StrVal[1] then
  begin
    offset := P_GetStateFromNameWithOffsetCheck(actor, actor.state.params.StrVal[2]);
    if @states[offset] <> actor.state then
      P_SetMobjState(actor, statenum_t(offset));
  end;
end;

procedure A_JumpIfWorldStringGreater(actor: Pmobj_t);
var
  offset: integer;
begin
  if not P_CheckStateParams(actor, 3) then
    exit;

  if Worldvars.StrVal[actor.state.params.StrVal[0]] > actor.state.params.StrVal[1] then
  begin
    offset := P_GetStateFromNameWithOffsetCheck(actor, actor.state.params.StrVal[2]);
    if @states[offset] <> actor.state then
      P_SetMobjState(actor, statenum_t(offset));
  end;
end;

procedure A_JumpIfWorldIntegerEqual(actor: Pmobj_t);
var
  offset: integer;
  cur: integer;
begin
  if not P_CheckStateParams(actor, 3) then
    exit;

  if Worldvars.IntVal[actor.state.params.StrVal[0]] = actor.state.params.IntVal[1] then
  begin
    offset := actor.state.params.IntVal[2];

    cur := (integer(actor.state) - integer(states)) div SizeOf(state_t);

    P_SetMobjState(actor, statenum_t(cur + offset));
  end;
end;

procedure A_JumpIfWorldIntegerLess(actor: Pmobj_t);
var
  offset: integer;
begin
  if not P_CheckStateParams(actor, 3) then
    exit;

  if Worldvars.IntVal[actor.state.params.StrVal[0]] < actor.state.params.IntVal[1] then
  begin
    offset := P_GetStateFromNameWithOffsetCheck(actor, actor.state.params.StrVal[2]);
    if @states[offset] <> actor.state then
      P_SetMobjState(actor, statenum_t(offset));
  end;
end;

procedure A_JumpIfWorldIntegerGreater(actor: Pmobj_t);
var
  offset: integer;
begin
  if not P_CheckStateParams(actor, 3) then
    exit;

  if Worldvars.IntVal[actor.state.params.StrVal[0]] > actor.state.params.IntVal[1] then
  begin
    offset := P_GetStateFromNameWithOffsetCheck(actor, actor.state.params.StrVal[2]);
    if @states[offset] <> actor.state then
      P_SetMobjState(actor, statenum_t(offset));
  end;
end;

procedure A_JumpIfWorldFloatEqual(actor: Pmobj_t);
var
  offset: integer;
begin
  if not P_CheckStateParams(actor, 3) then
    exit;

  if Worldvars.FloatVal[actor.state.params.StrVal[0]] = actor.state.params.FloatVal[1] then
  begin
    offset := P_GetStateFromNameWithOffsetCheck(actor, actor.state.params.StrVal[2]);
    if @states[offset] <> actor.state then
      P_SetMobjState(actor, statenum_t(offset));
  end;
end;

procedure A_JumpIfWorldFloatLess(actor: Pmobj_t);
var
  offset: integer;
begin
  if not P_CheckStateParams(actor, 3) then
    exit;

  if Worldvars.FloatVal[actor.state.params.StrVal[0]] < actor.state.params.FloatVal[1] then
  begin
    offset := P_GetStateFromNameWithOffsetCheck(actor, actor.state.params.StrVal[2]);
    if @states[offset] <> actor.state then
      P_SetMobjState(actor, statenum_t(offset));
  end;
end;

procedure A_JumpIfWorldFloatGreater(actor: Pmobj_t);
var
  offset: integer;
begin
  if not P_CheckStateParams(actor, 3) then
    exit;

  if Worldvars.FloatVal[actor.state.params.StrVal[0]] > actor.state.params.FloatVal[1] then
  begin
    offset := P_GetStateFromNameWithOffsetCheck(actor, actor.state.params.StrVal[2]);
    if @states[offset] <> actor.state then
      P_SetMobjState(actor, statenum_t(offset));
  end;
end;

//
// JVAL
// Change state
// A_GoToIfCustomParam(customparam, value of customparam, newstate)
//
procedure A_GoToIfCustomParam(actor: Pmobj_t);
var
  newstate: integer;
begin
  if not P_CheckStateParams(actor, 3) then
    exit;

  if P_GetMobjCustomParamValue(actor, actor.state.params.StrVal[0]) = actor.state.params.IntVal[1] then
  begin
    if not actor.state.params.IsComputed[2] then
      actor.state.params.IntVal[2] := P_GetStateFromName(actor, actor.state.params.StrVal[2]);
    newstate := actor.state.params.IntVal[2];

    P_SetMobjState(actor, statenum_t(newstate));
  end;
end;

//
// JVAL
// Change state
// A_GoToIfCustomParamLess(customparam, value of customparam, newstate)
//
procedure A_GoToIfCustomParamLess(actor: Pmobj_t);
var
  newstate: integer;
begin
  if not P_CheckStateParams(actor, 3) then
    exit;

  if P_GetMobjCustomParamValue(actor, actor.state.params.StrVal[0]) < actor.state.params.IntVal[1] then
  begin
    if not actor.state.params.IsComputed[2] then
      actor.state.params.IntVal[2] := P_GetStateFromName(actor, actor.state.params.StrVal[2]);
    newstate := actor.state.params.IntVal[2];

    P_SetMobjState(actor, statenum_t(newstate));
  end;
end;

//
// JVAL
// Change state
// A_GoToIfCustomParamGreater(customparam, value of customparam, newstate)
//
procedure A_GoToIfCustomParamGreater(actor: Pmobj_t);
var
  newstate: integer;
begin
  if not P_CheckStateParams(actor, 3) then
    exit;

  if P_GetMobjCustomParamValue(actor, actor.state.params.StrVal[0]) > actor.state.params.IntVal[1] then
  begin
    if not actor.state.params.IsComputed[2] then
      actor.state.params.IntVal[2] := P_GetStateFromName(actor, actor.state.params.StrVal[2]);
    newstate := actor.state.params.IntVal[2];

    P_SetMobjState(actor, statenum_t(newstate));
  end;
end;

//
// JVAL
// Change state
// A_GoToIfTargetCustomParam(customparam, value of customparam, newstate)
//
procedure A_GoToIfTargetCustomParam(actor: Pmobj_t);
var
  newstate: integer;
begin
  if actor.target = nil then
    exit;

  if not P_CheckStateParams(actor, 3) then
    exit;

  if P_GetMobjCustomParamValue(actor.target, actor.state.params.StrVal[0]) = actor.state.params.IntVal[1] then
  begin
    if not actor.state.params.IsComputed[2] then
      actor.state.params.IntVal[2] := P_GetStateFromName(actor, actor.state.params.StrVal[2]);
    newstate := actor.state.params.IntVal[2];

    P_SetMobjState(actor, statenum_t(newstate));
  end;
end;

//
// JVAL
// Change state
// A_GoToIfTargetCustomParamLess(customparam, value of customparam, newstate)
//
procedure A_GoToIfTargetCustomParamLess(actor: Pmobj_t);
var
  newstate: integer;
begin
  if actor.target = nil then
    exit;

  if not P_CheckStateParams(actor, 3) then
    exit;

  if P_GetMobjCustomParamValue(actor.target, actor.state.params.StrVal[0]) < actor.state.params.IntVal[1] then
  begin
    if not actor.state.params.IsComputed[2] then
      actor.state.params.IntVal[2] := P_GetStateFromName(actor, actor.state.params.StrVal[2]);
    newstate := actor.state.params.IntVal[2];

    P_SetMobjState(actor, statenum_t(newstate));
  end;
end;

//
// JVAL
// Change state
// A_GoToIfTargetCustomParamGreater(customparam, value of customparam, newstate)
//
procedure A_GoToIfTargetCustomParamGreater(actor: Pmobj_t);
var
  newstate: integer;
begin
  if actor.target = nil then
    exit;

  if not P_CheckStateParams(actor, 3) then
    exit;

  if P_GetMobjCustomParamValue(actor.target, actor.state.params.StrVal[0]) > actor.state.params.IntVal[1] then
  begin
    if not actor.state.params.IsComputed[2] then
      actor.state.params.IntVal[2] := P_GetStateFromName(actor, actor.state.params.StrVal[2]);
    newstate := actor.state.params.IntVal[2];

    P_SetMobjState(actor, statenum_t(newstate));
  end;
end;

//
// JVAL
// Change state
// A_GoToIfMapStringEqual(map variable, value of map variable, newstate)
//
procedure A_GoToIfMapStringEqual(actor: Pmobj_t);
var
  newstate: integer;
begin
  if not P_CheckStateParams(actor, 3) then
    exit;

  if mapvars.StrVal[actor.state.params.StrVal[0]] = actor.state.params.StrVal[1] then
  begin
    if not actor.state.params.IsComputed[2] then
      actor.state.params.IntVal[2] := P_GetStateFromName(actor, actor.state.params.StrVal[2]);
    newstate := actor.state.params.IntVal[2];

    P_SetMobjState(actor, statenum_t(newstate));
  end;
end;

//
// JVAL
// Change state
// A_GoToIfMapStringLess(map variable, value of map variable, newstate)
//
procedure A_GoToIfMapStringLess(actor: Pmobj_t);
var
  newstate: integer;
begin
  if not P_CheckStateParams(actor, 3) then
    exit;

  if mapvars.StrVal[actor.state.params.StrVal[0]] < actor.state.params.StrVal[1] then
  begin
    if not actor.state.params.IsComputed[2] then
      actor.state.params.IntVal[2] := P_GetStateFromName(actor, actor.state.params.StrVal[2]);
    newstate := actor.state.params.IntVal[2];

    P_SetMobjState(actor, statenum_t(newstate));
  end;
end;

//
// JVAL
// Change state
// A_GoToIfMapStringGreater(map variable, value of map variable, newstate)
//
procedure A_GoToIfMapStringGreater(actor: Pmobj_t);
var
  newstate: integer;
begin
  if not P_CheckStateParams(actor, 3) then
    exit;

  if mapvars.StrVal[actor.state.params.StrVal[0]] > actor.state.params.StrVal[1] then
  begin
    if not actor.state.params.IsComputed[2] then
      actor.state.params.IntVal[2] := P_GetStateFromName(actor, actor.state.params.StrVal[2]);
    newstate := actor.state.params.IntVal[2];

    P_SetMobjState(actor, statenum_t(newstate));
  end;
end;

procedure A_GoToIfMapIntegerEqual(actor: Pmobj_t);
var
  newstate: integer;
begin
  if not P_CheckStateParams(actor, 3) then
    exit;

  if mapvars.IntVal[actor.state.params.StrVal[0]] = actor.state.params.IntVal[1] then
  begin
    if not actor.state.params.IsComputed[2] then
      actor.state.params.IntVal[2] := P_GetStateFromName(actor, actor.state.params.StrVal[2]);
    newstate := actor.state.params.IntVal[2];

    P_SetMobjState(actor, statenum_t(newstate));
  end;
end;

procedure A_GoToIfMapIntegerLess(actor: Pmobj_t);
var
  newstate: integer;
begin
  if not P_CheckStateParams(actor, 3) then
    exit;

  if mapvars.IntVal[actor.state.params.StrVal[0]] < actor.state.params.IntVal[1] then
  begin
    if not actor.state.params.IsComputed[2] then
      actor.state.params.IntVal[2] := P_GetStateFromName(actor, actor.state.params.StrVal[2]);
    newstate := actor.state.params.IntVal[2];

    P_SetMobjState(actor, statenum_t(newstate));
  end;
end;

procedure A_GoToIfMapIntegerGreater(actor: Pmobj_t);
var
  newstate: integer;
begin
  if not P_CheckStateParams(actor, 3) then
    exit;

  if mapvars.IntVal[actor.state.params.StrVal[0]] > actor.state.params.IntVal[1] then
  begin
    if not actor.state.params.IsComputed[2] then
      actor.state.params.IntVal[2] := P_GetStateFromName(actor, actor.state.params.StrVal[2]);
    newstate := actor.state.params.IntVal[2];

    P_SetMobjState(actor, statenum_t(newstate));
  end;
end;

procedure A_GoToIfMapFloatEqual(actor: Pmobj_t);
var
  newstate: integer;
begin
  if not P_CheckStateParams(actor, 3) then
    exit;

  if mapvars.FloatVal[actor.state.params.StrVal[0]] = actor.state.params.FloatVal[1] then
  begin
    if not actor.state.params.IsComputed[2] then
      actor.state.params.IntVal[2] := P_GetStateFromName(actor, actor.state.params.StrVal[2]);
    newstate := actor.state.params.IntVal[2];

    P_SetMobjState(actor, statenum_t(newstate));
  end;
end;

procedure A_GoToIfMapFloatLess(actor: Pmobj_t);
var
  newstate: integer;
begin
  if not P_CheckStateParams(actor, 3) then
    exit;

  if mapvars.FloatVal[actor.state.params.StrVal[0]] < actor.state.params.FloatVal[1] then
  begin
    if not actor.state.params.IsComputed[2] then
      actor.state.params.IntVal[2] := P_GetStateFromName(actor, actor.state.params.StrVal[2]);
    newstate := actor.state.params.IntVal[2];

    P_SetMobjState(actor, statenum_t(newstate));
  end;
end;

procedure A_GoToIfMapFloatGreater(actor: Pmobj_t);
var
  newstate: integer;
begin
  if not P_CheckStateParams(actor, 3) then
    exit;

  if mapvars.FloatVal[actor.state.params.StrVal[0]] > actor.state.params.FloatVal[1] then
  begin
    if not actor.state.params.IsComputed[2] then
      actor.state.params.IntVal[2] := P_GetStateFromName(actor, actor.state.params.StrVal[2]);
    newstate := actor.state.params.IntVal[2];

    P_SetMobjState(actor, statenum_t(newstate));
  end;
end;

//
// JVAL
// Change state
// A_GoToIfWorldStringEqual(World variable, value of World variable, newstate)
//
procedure A_GoToIfWorldStringEqual(actor: Pmobj_t);
var
  newstate: integer;
begin
  if not P_CheckStateParams(actor, 3) then
    exit;

  if Worldvars.StrVal[actor.state.params.StrVal[0]] = actor.state.params.StrVal[1] then
  begin
    if not actor.state.params.IsComputed[2] then
      actor.state.params.IntVal[2] := P_GetStateFromName(actor, actor.state.params.StrVal[2]);
    newstate := actor.state.params.IntVal[2];

    P_SetMobjState(actor, statenum_t(newstate));
  end;
end;

//
// JVAL
// Change state
// A_GoToIfWorldStringLess(World variable, value of World variable, newstate)
//
procedure A_GoToIfWorldStringLess(actor: Pmobj_t);
var
  newstate: integer;
begin
  if not P_CheckStateParams(actor, 3) then
    exit;

  if Worldvars.StrVal[actor.state.params.StrVal[0]] < actor.state.params.StrVal[1] then
  begin
    if not actor.state.params.IsComputed[2] then
      actor.state.params.IntVal[2] := P_GetStateFromName(actor, actor.state.params.StrVal[2]);
    newstate := actor.state.params.IntVal[2];

    P_SetMobjState(actor, statenum_t(newstate));
  end;
end;

//
// JVAL
// Change state
// A_GoToIfWorldStringGreater(World variable, value of World variable, newstate)
//
procedure A_GoToIfWorldStringGreater(actor: Pmobj_t);
var
  newstate: integer;
begin
  if not P_CheckStateParams(actor, 3) then
    exit;

  if Worldvars.StrVal[actor.state.params.StrVal[0]] > actor.state.params.StrVal[1] then
  begin
    if not actor.state.params.IsComputed[2] then
      actor.state.params.IntVal[2] := P_GetStateFromName(actor, actor.state.params.StrVal[2]);
    newstate := actor.state.params.IntVal[2];

    P_SetMobjState(actor, statenum_t(newstate));
  end;
end;

procedure A_GoToIfWorldIntegerEqual(actor: Pmobj_t);
var
  newstate: integer;
begin
  if not P_CheckStateParams(actor, 3) then
    exit;

  if Worldvars.IntVal[actor.state.params.StrVal[0]] = actor.state.params.IntVal[1] then
  begin
    if not actor.state.params.IsComputed[2] then
      actor.state.params.IntVal[2] := P_GetStateFromName(actor, actor.state.params.StrVal[2]);
    newstate := actor.state.params.IntVal[2];

    P_SetMobjState(actor, statenum_t(newstate));
  end;
end;

procedure A_GoToIfWorldIntegerLess(actor: Pmobj_t);
var
  newstate: integer;
begin
  if not P_CheckStateParams(actor, 3) then
    exit;

  if Worldvars.IntVal[actor.state.params.StrVal[0]] < actor.state.params.IntVal[1] then
  begin
    if not actor.state.params.IsComputed[2] then
      actor.state.params.IntVal[2] := P_GetStateFromName(actor, actor.state.params.StrVal[2]);
    newstate := actor.state.params.IntVal[2];

    P_SetMobjState(actor, statenum_t(newstate));
  end;
end;

procedure A_GoToIfWorldIntegerGreater(actor: Pmobj_t);
var
  newstate: integer;
begin
  if not P_CheckStateParams(actor, 3) then
    exit;

  if Worldvars.IntVal[actor.state.params.StrVal[0]] > actor.state.params.IntVal[1] then
  begin
    if not actor.state.params.IsComputed[2] then
      actor.state.params.IntVal[2] := P_GetStateFromName(actor, actor.state.params.StrVal[2]);
    newstate := actor.state.params.IntVal[2];

    P_SetMobjState(actor, statenum_t(newstate));
  end;
end;

procedure A_GoToIfWorldFloatEqual(actor: Pmobj_t);
var
  newstate: integer;
begin
  if not P_CheckStateParams(actor, 3) then
    exit;

  if Worldvars.FloatVal[actor.state.params.StrVal[0]] = actor.state.params.FloatVal[1] then
  begin
    if not actor.state.params.IsComputed[2] then
      actor.state.params.IntVal[2] := P_GetStateFromName(actor, actor.state.params.StrVal[2]);
    newstate := actor.state.params.IntVal[2];

    P_SetMobjState(actor, statenum_t(newstate));
  end;
end;

procedure A_GoToIfWorldFloatLess(actor: Pmobj_t);
var
  newstate: integer;
begin
  if not P_CheckStateParams(actor, 3) then
    exit;

  if Worldvars.FloatVal[actor.state.params.StrVal[0]] < actor.state.params.FloatVal[1] then
  begin
    if not actor.state.params.IsComputed[2] then
      actor.state.params.IntVal[2] := P_GetStateFromName(actor, actor.state.params.StrVal[2]);
    newstate := actor.state.params.IntVal[2];

    P_SetMobjState(actor, statenum_t(newstate));
  end;
end;

procedure A_GoToIfWorldFloatGreater(actor: Pmobj_t);
var
  newstate: integer;
begin
  if not P_CheckStateParams(actor, 3) then
    exit;

  if Worldvars.FloatVal[actor.state.params.StrVal[0]] > actor.state.params.FloatVal[1] then
  begin
    if not actor.state.params.IsComputed[2] then
      actor.state.params.IntVal[2] := P_GetStateFromName(actor, actor.state.params.StrVal[2]);
    newstate := actor.state.params.IntVal[2];

    P_SetMobjState(actor, statenum_t(newstate));
  end;
end;

procedure A_CustomSound1(mo: Pmobj_t);
begin
  if mo.info.customsound1 <> 0 then
  begin
    if mo.info.flags_ex and MF_EX_RANDOMCUSTOMSOUND1 <> 0 then
      A_RandomCustomSound1(mo)
    else
      S_StartSound(mo, mo.info.customsound1);
  end;
end;

procedure A_CustomSound2(mo: Pmobj_t);
begin
  if mo.info.customsound2 <> 0 then
  begin
    if mo.info.flags_ex and MF_EX_RANDOMCUSTOMSOUND2 <> 0 then
      A_RandomCustomSound2(mo)
    else
      S_StartSound(mo, mo.info.customsound2);
  end;
end;

procedure A_CustomSound3(mo: Pmobj_t);
begin
  if mo.info.customsound3 <> 0 then
  begin
    if mo.info.flags_ex and MF_EX_RANDOMCUSTOMSOUND3 <> 0 then
      A_RandomCustomSound3(mo)
    else
      S_StartSound(mo, mo.info.customsound3);
  end;
end;

procedure P_RandomSound(const actor: Pmobj_t; const soundnum: integer);
var
  randomlist: TDNumberList;
  rndidx: integer;
begin
  if soundnum <> 0 then
  begin
    randomlist := S_GetRandomSoundList(soundnum);
    if randomlist <> nil then
    begin
      if randomlist.Count > 0 then
      begin
        rndidx := N_Random mod randomlist.Count;
        S_StartSound(actor, randomlist[rndidx]);
      end
      else
      // JVAL: This should never happen, see S_GetRandomSoundList() in sounds.pas
        I_Error('P_RandomSound(): Random list is empty for sound no %d', [soundnum]);
    end;
  end;
end;

procedure A_RandomPainSound(actor: Pmobj_t);
begin
  if actor.flags2_ex and MF2_EX_FULLVOLPAIN <> 0 then
    P_RandomSound(nil, actor.info.painsound)
  else
    P_RandomSound(actor, actor.info.painsound);
end;

procedure A_RandomSeeSound(actor: Pmobj_t);
begin
  if actor.flags2_ex and MF2_EX_FULLVOLSEE <> 0 then
    P_RandomSound(nil, actor.info.seesound)
  else
    P_RandomSound(actor, actor.info.seesound);
end;

procedure A_RandomAttackSound(actor: Pmobj_t);
begin
  if actor.flags2_ex and MF2_EX_FULLVOLATTACK <> 0 then
    P_RandomSound(nil, actor.info.attacksound)
  else
    P_RandomSound(actor, actor.info.attacksound);
end;

procedure A_RandomDeathSound(actor: Pmobj_t);
begin
  if actor.flags2_ex and MF2_EX_FULLVOLDEATH <> 0 then
    P_RandomSound(nil, actor.info.deathsound)
  else
    P_RandomSound(actor, actor.info.deathsound);
end;

procedure A_RandomActiveSound(actor: Pmobj_t);
begin
  if actor.flags2_ex and MF2_EX_FULLVOLACTIVE <> 0 then
    P_RandomSound(nil, actor.info.activesound)
  else
    P_RandomSound(actor, actor.info.activesound);
end;

procedure A_RandomCustomSound1(actor: Pmobj_t);
begin
  P_RandomSound(actor, actor.info.customsound1);
end;

procedure A_RandomCustomSound2(actor: Pmobj_t);
begin
  P_RandomSound(actor, actor.info.customsound2);
end;

procedure A_RandomCustomSound3(actor: Pmobj_t);
begin
  P_RandomSound(actor, actor.info.customsound3);
end;

procedure A_RandomCustomSound(actor: Pmobj_t);
var
  list: TDNumberList;
  rndidx: integer;
begin
  list := TDNumberList.Create;
  try
    if actor.info.customsound1 > 0 then
      list.Add(actor.info.customsound1);
    if actor.info.customsound2 > 0 then
      list.Add(actor.info.customsound2);
    if actor.info.customsound3 > 0 then
      list.Add(actor.info.customsound3);
    if list.Count > 0 then
    begin
      rndidx := N_Random mod list.Count;
      P_RandomSound(actor, list[rndidx]);
    end;
  finally
    list.Free;
  end;
end;

procedure A_RandomMeleeSound(actor: Pmobj_t);
begin
  P_RandomSound(actor, actor.info.meleesound);
end;

//
// JVAL
// Play a sound
// A_Playsound(soundname)
//
procedure A_Playsound(actor: Pmobj_t);
var
  sndidx: integer;
begin
  // JVAL: 20210109 - DEHEXTRA support
  if actor.state.params = nil then
  begin
    if actor.state.misc2 <> 0 then
      S_StartSound(nil, actor.state.misc1)
    else
      S_StartSound(actor, actor.state.misc1);
    exit;
  end;

  if not P_CheckStateParams(actor, 1) then
    exit;

  if actor.state.params.IsComputed[0] then
    sndidx := actor.state.params.IntVal[0]
  else
  begin
    sndidx := S_GetSoundNumForName(actor.state.params.StrVal[0]);
    actor.state.params.IntVal[0] := sndidx;
  end;

  S_StartSound(actor, sndidx);
end;

procedure A_PlayWeaponsound(actor: Pmobj_t);
begin
  A_Playsound(actor);
end;

//
// JVAL
// Random sound
// A_RandomSound(sound1, sound2, ...)
//
procedure A_RandomSound(actor: Pmobj_t);
var
  sidxs: TDNumberList;
  sndidx: integer;
  i: integer;
begin
  if not P_CheckStateParams(actor) then
    exit;

  if actor.state.params.Count = 0 then // Should never happen
    exit;

  sidxs := TDNumberList.Create;
  try
    for i := 0 to actor.state.params.Count - 1 do
    begin
      if actor.state.params.IsComputed[i] then
        sndidx := actor.state.params.IntVal[i]
      else
      begin
        sndidx := S_GetSoundNumForName(actor.state.params.StrVal[i]);
        actor.state.params.IntVal[i] := sndidx;
      end;
      sidxs.Add(sndidx);
    end;
    sndidx := N_Random mod sidxs.Count;
    S_StartSound(actor, sidxs[sndidx]);
  finally
    sidxs.Free;
  end;
end;

//
// JVAL
// Set all momentum to zero
//
procedure A_Stop(actor: Pmobj_t);
begin
  actor.momx := 0;
  actor.momy := 0;
  actor.momz := 0;
end;

//
// JVAL
// Change state offset
// A_Jump(propability, offset1, offset2, ....)
//
procedure A_Jump(actor: Pmobj_t);
var
  propability: integer;
  offset: integer;
  N: TDNumberList;
  i: integer;
begin
  if not P_CheckStateParams(actor, 2, CSP_AT_LEAST) then
    exit;

  propability := actor.state.params.IntVal[0];  // JVAL simple integer values are precalculated

  if N_Random < propability then
  begin
    N := TDNumberList.Create;
    for i := 1 to actor.state.params.Count - 1 do
    begin
      offset := P_GetStateFromNameWithOffsetCheck(actor, actor.state.params.StrVal[i]);
      N.Add(offset);
    end;
    if N.Count > 0 then
    begin
      offset := N.Numbers[N_Random mod N.Count];
      if @states[offset] <> actor.state then
        P_SetMobjState(actor, statenum_t(offset));
    end;
    N.Free;
  end;
end;

procedure P_CustomMissile(const actor: Pmobj_t; const missiletype: integer;
  const spawnheight: fixed_t; const spawnoffs: fixed_t; const angle: angle_t;
  const aimmode: integer; const pitch: angle_t);
var
  missile: Pmobj_t;
  ang: angle_t;
  x, y, z: fixed_t;
  vx, vz: fixed_t;
  velocity: vec3_t;
  missilespeed: fixed_t;
  owner: Pmobj_t;
  pitchang: angle_t;
begin
  if missiletype < 0 then
    exit;

  if mobjinfo[missiletype].speed < 2048 then
    mobjinfo[missiletype].speed := mobjinfo[missiletype].speed * FRACUNIT;  // JVAL fix me!!!
  if (actor.target <> nil) or (aimmode = 2) then
  begin
    ang := (actor.angle - ANG90) shr ANGLETOFINESHIFT;
    x := FixedMul(spawnoffs, finecosine[ang]);
    y := FixedMul(spawnoffs, finesine[ang]);
    if aimmode <> 0 then
      z := spawnheight
    else
      z := spawnheight - 32 * FRACUNIT;
    case aimmode of
      1:
        begin
          missile := P_SpawnMissileXYZ(actor.x + x, actor.y + y, actor.z + z, actor, actor.target, missiletype);
        end;
      2:
        begin
          missile := P_SpawnMissileAngleZ(actor, actor.z + z, missiletype, actor.angle, 0, 0);

          // It is not necessary to use the correct angle here.
          // The only important thing is that the horizontal momentum is correct.
          // Therefore use 0 as the missile's angle and simplify the calculations accordingly.
          // The actual momentum vector is set below.
          if missile <> nil then
          begin
            pitchang := pitch shr ANGLETOFINESHIFT;
            vx := finecosine[pitchang];
            vz := finesine[pitchang];
            missile.momx := FixedMul(vx, missile.info.speed);
            missile.momy := 0;
            missile.momz := FixedMul(vz, missile.info.speed);
          end;
        end;
      else
      begin
        inc(actor.x, x);
        inc(actor.y, y);
        inc(actor.z, z);
        missile := P_SpawnMissile(actor, actor.target, missiletype);
        dec(actor.x, x);
        dec(actor.y, y);
        dec(actor.z, z);

      end;
    end;  // case

    if missile <> nil then
    begin
      // Use the actual momentum instead of the missile's Speed property
      // so that this can handle missiles with a high vertical velocity
      // component properly.
      velocity[0] := missile.momx;
      velocity[1] := missile.momy;
      velocity[2] := 0.0;

      missilespeed := round(VectorLength(@velocity));

      missile.angle := missile.angle + angle;
      ang := missile.angle shr ANGLETOFINESHIFT;
      missile.momx := FixedMul(missilespeed, finecosine[ang]);
      missile.momy := FixedMul(missilespeed, finesine[ang]);

      // handle projectile shooting projectiles - track the
      // links back to a real owner
      if (actor.info.flags and MF_MISSILE <> 0) or (aimmode and 4 <> 0) then
      begin
        owner := actor;
        while (owner.info.flags and MF_MISSILE <> 0) and (owner.target <> nil) do
          owner := owner.target;
         missile.target := owner;
        // automatic handling of seeker missiles
        if actor.info.flags_ex and missile.info.flags_ex and MF_EX_SEEKERMISSILE <> 0 then
          missile.tracer := actor.tracer;
      end
      else if missile.info.flags_ex and MF_EX_SEEKERMISSILE <> 0 then
      // automatic handling of seeker missiles
        missile.tracer := actor.target;

    end;
  end;
end;

//
// JVAL
// Custom missile, based on A_CustomMissile() of ZDoom
// A_CustomMissile(type, height, offset, angle, aimmode, pitch)
//
procedure A_CustomMissile(actor: Pmobj_t);
var
  mobj_no: integer;
  spawnheight: fixed_t;
  spawnoffs: fixed_t;
  angle: angle_t;
  aimmode: integer;
  pitch: angle_t;
begin
  if not P_CheckStateParams(actor) then
    exit;

  if actor.state.params.IsComputed[0] then
    mobj_no := actor.state.params.IntVal[0]
  else
  begin
    mobj_no := Info_GetMobjNumForName(actor.state.params.StrVal[0]);
    actor.state.params.IntVal[0] := mobj_no;
  end;
  if mobj_no = -1 then
  begin
    I_Warning('A_CustomMissile(): Unknown missile %s'#13#10, [actor.state.params.StrVal[0]]);
    exit;
  end;

  spawnheight := actor.state.params.FixedVal[1];
  spawnoffs := actor.state.params.FixedVal[2];
  angle := ANG1 * actor.state.params.IntVal[3];
  aimmode := actor.state.params.IntVal[4] and 3;
  pitch := ANG1 * actor.state.params.IntVal[5];

  P_CustomMissile(actor, mobj_no, spawnheight, spawnoffs, angle, aimmode, pitch);
end;

//
// JVAL
// Standard random missile procedure
// A_RandomMissile(type1, type2, type3, ...)
//
procedure A_RandomMissile(actor: Pmobj_t);
var
  ridx: integer;
  mobj_no: integer;
  spawnheight: fixed_t;
  spawnoffs: integer;
  angle: angle_t;
  missile: Pmobj_t;
  ang: angle_t;
  x, y, z: fixed_t;
  velocity: vec3_t;
  missilespeed: fixed_t;
  owner: Pmobj_t;
begin
  if not P_CheckStateParams(actor) then
    exit;

  // Random index
  ridx := N_Random mod actor.state.params.Count;

  if actor.state.params.IsComputed[ridx] then
    mobj_no := actor.state.params.IntVal[ridx]
  else
  begin
    mobj_no := Info_GetMobjNumForName(actor.state.params.StrVal[ridx]);
    actor.state.params.IntVal[ridx] := mobj_no;
  end;
  if mobj_no = -1 then
  begin
    I_Warning('A_RandomMissile(): Unknown missile %s'#13#10, [actor.state.params.StrVal[ridx]]);
    exit;
  end;

  if mobjinfo[mobj_no].speed < 2048 then
    mobjinfo[mobj_no].speed := mobjinfo[mobj_no].speed * FRACUNIT;  // JVAL fix me!!!
  spawnheight := 0;
  spawnoffs := 0;
  angle := 0;

  if actor.target <> nil then
  begin
    ang := (actor.angle - ANG90) shr ANGLETOFINESHIFT;
    x := spawnoffs * finecosine[ang];
    y := spawnoffs * finesine[ang];
    z := (spawnheight - 32) * FRACUNIT;
    inc(actor.x, x);
    inc(actor.y, y);
    inc(actor.z, z);
    missile := P_SpawnMissile(actor, actor.target, mobj_no);
    dec(actor.x, x);
    dec(actor.y, y);
    dec(actor.z, z);

    if missile <> nil then
    begin
      // Use the actual momentum instead of the missile's Speed property
      // so that this can handle missiles with a high vertical velocity
      // component properly.
      velocity[0] := missile.momx;
      velocity[1] := missile.momy;
      velocity[2] := 0.0;

      missilespeed := round(VectorLength(@velocity));

      missile.angle := missile.angle + angle;
      ang := missile.angle shr ANGLETOFINESHIFT;
      missile.momx := FixedMul(missilespeed, finecosine[ang]);
      missile.momy := FixedMul(missilespeed, finesine[ang]);

      owner := actor;
      while (owner.info.flags and MF_MISSILE <> 0) and (owner.target <> nil) do
        owner := owner.target;
       missile.target := owner;
      // automatic handling of seeker missiles
      if actor.info.flags_ex and missile.info.flags_ex and MF_EX_SEEKERMISSILE <> 0 then
        missile.tracer := actor.tracer;

    end;
  end;
end;

//
// A_SpawnItem(type, distance, zheight, angle)
//
procedure A_SpawnItem(actor: Pmobj_t);
var
  mobj_no: integer;
  distance: fixed_t;
  zheight: fixed_t;
  mo: Pmobj_t;
  ang: angle_t;
begin
  if not P_CheckStateParams(actor) then
    exit;

  if actor.state.params.IsComputed[0] then
    mobj_no := actor.state.params.IntVal[0]
  else
  begin
    mobj_no := Info_GetMobjNumForName(actor.state.params.StrVal[0]);
    actor.state.params.IntVal[0] := mobj_no;
  end;
  if mobj_no = -1 then
  begin
    I_Warning('A_SpawnItem(): Unknown item %s'#13#10, [actor.state.params.StrVal[0]]);
    exit;
  end;

  distance := actor.state.params.FixedVal[1] + actor.radius + mobjinfo[mobj_no].radius;

  zheight := actor.state.params.FixedVal[2];
  ang := ANG1 * actor.state.params.IntVal[3];

  ang := (ang + actor.angle) shr ANGLETOFINESHIFT;
  mo := P_SpawnMobj(actor.x + FixedMul(distance, finecosine[ang]),
                    actor.y + FixedMul(distance, finesine[ang]),
                    actor.z{ - actor.floorz }+ zheight, mobj_no);
  if mo <> nil then
    mo.angle := actor.angle;
end;

function InitSpawnedItem(const self, mo: Pmobj_t; flags: integer): boolean;
const
  MAXLOOP = 64;
var
  originator: Pmobj_t;
  loop: integer;
begin
  if mo = nil then
  begin
    result := false;
    exit;
  end;

  // JVAL: 20211118 - Dropped flag
  if flags and SIXF_DROPPED <> 0 then
  {$IFDEF HEXEN}
    mo.flags2 := mo.flags2 or MF2_DROPPED;
  {$ELSE}
    mo.flags := mo.flags or MF_DROPPED;
  {$ENDIF}

  if flags and SIXF_TRANSFERTRANSLATION <> 0 then
    mo.flags := (mo.flags and not MF_TRANSLATION) or (self.flags and MF_TRANSLATION);

  if flags and SIXF_TRANSFERPOINTERS <> 0 then
  begin
    mo.target := self.target;
    mo.master := self.master; // This will be overridden later if SIXF_SETMASTER is set
    mo.tracer := self.tracer;
  end;

  originator := self;

  if flags and SIXF_ORIGINATOR = 0 then
  begin
    loop := 0;
    while originator <> nil do
    begin
      if originator.flags and MF_MISSILE = 0 then
        break;
      if loop = MAXLOOP then
        break;
      originator := originator.target;
      inc(loop);
    end;
  end;

  if flags and SIXF_TELEFRAG <> 0 then
  begin
    P_TeleportMove(mo, mo.x, mo.y);
    // This is needed to ensure consistent behavior.
    // Otherwise it will only spawn if nothing gets telefragged
    flags := flags or SIXF_NOCHECKPOSITION;
  end;

  if Info_IsMonster(mo._type) then
  begin
    if (flags and SIXF_NOCHECKPOSITION = 0) and not P_TestMobjLocation(mo) then
    begin
      // The monster is blocked so don't spawn it at all!
      P_RemoveMobj(mo);
      result := false;
      exit;
    end
    else if (originator <> nil) and (flags and SIXF_NOPOINTERS = 0) then
      P_CopyFriendliness(originator, mo);
  end
  else if flags and SIXF_TRANSFERPOINTERS <> 0 then
  begin
    // If this is a missile or something else set the target to the originator
    if originator <> nil then
      mo.target := originator
    else
      mo.target := self;
  end;

  if flags and SIXF_NOPOINTERS <> 0 then
  begin
    //[MC]Intentionally eliminate pointers. Overrides TRANSFERPOINTERS, but is overridden by SETMASTER/TARGET/TRACER.
    mo.target := nil;
    mo.master := nil;
    mo.tracer := nil;
  end;

  if flags and SIXF_SETMASTER <> 0 then
  begin
    // don't let it attack you (optional)!
    mo.master := originator;
  end;

  if flags and SIXF_SETTARGET <> 0 then
  begin
    mo.target := originator;
  end;

  if flags and SIXF_SETTRACER <> 0 then
  begin
    mo.tracer := originator;
  end;

  if flags and SIXF_TRANSFERSCALE <> 0 then
  begin
    mo.scale := self.scale;
  end;

  if flags and SIXF_TRANSFERAMBUSHFLAG <> 0 then
  begin
    mo.flags := (mo.flags and not MF_AMBUSH) or (self.flags and MF_AMBUSH);
  end;

  {$IFDEF HEXEN}
  if flags and SIXF_CLEARCALLERTID <> 0 then
  begin
    P_RemoveMobjFromTIDList(self);
    self.tid := 0;
    P_InsertMobjIntoTIDList(self, 0); // ?
  end;
  {$ENDIF}

  if flags and SIXF_TRANSFERSPECIAL <> 0 then
  begin
    mo.special := self.special;
    mo.args := self.args;
  end;

  if flags and SIXF_CLEARCALLERSPECIAL <> 0 then
  begin
    self.special := 0;
    FillChar(self.args, SizeOf(self.args), Chr(0));
  end;

  if flags and SIXF_TRANSFERALPHA <> 0 then
  begin
    mo.alpha := self.alpha;
  end;

  if flags and SIXF_TRANSFERRENDERSTYLE <> 0 then
  begin
    mo.RenderStyle := self.RenderStyle;
  end;

  if flags and SIXF_TRANSFERSPRITEFRAME <> 0 then
  begin
    mo.sprite := self.sprite;
    mo.frame := self.frame;
  end;

  if flags and SIXF_ISTARGET <> 0 then
  begin
    self.target := mo;
  end;

  if flags and SIXF_ISMASTER <> 0 then
  begin
    self.master := mo;
  end;

  if flags and SIXF_ISTRACER <> 0 then
  begin
    self.tracer := mo;
  end;

  result := true;
end;

//
// A_SpawnItemEx(type, xofs, yofs, zofs, momx, momy, momz, Angle, flags, chance)
//
// type -> parm0
// xofs -> parm1
// yofs -> parm2
// zofs -> parm3
// momx -> parm4
// momy -> parm5
// momz -> parm6
// Angle -> parm7
// flags -> parm8
// chance -> parm9
//
procedure A_SpawnItemEx(actor: Pmobj_t);
var
  mobj_no: integer;
  x, y: fixed_t;
  xofs, yofs, zofs: fixed_t;
  momx, momy, momz: fixed_t;
  newxmom: fixed_t;
  mo: Pmobj_t;
  ang, ang1: angle_t;
  flags: integer;
  chance: integer;
begin
  if not P_CheckStateParams(actor) then
    exit;

  chance := actor.state.params.IntVal[9];

  if (chance > 0) and (chance < N_Random) then
    exit;

  if actor.state.params.IsComputed[0] then
    mobj_no := actor.state.params.IntVal[0]
  else
  begin
    mobj_no := Info_GetMobjNumForName(actor.state.params.StrVal[0]);
    actor.state.params.IntVal[0] := mobj_no;
  end;
  if mobj_no = -1 then
  begin
    I_Warning('A_SpawnItemEx(): Unknown item %s'#13#10, [actor.state.params.StrVal[0]]);
    exit;
  end;

  // JVAL 20180222 -> IntVal changed to FixedVal
  xofs := actor.state.params.FixedVal[1];
  yofs := actor.state.params.FixedVal[2];
  zofs := actor.state.params.FixedVal[3];
  momx := actor.state.params.FixedVal[4];
  momy := actor.state.params.FixedVal[5];
  momz := actor.state.params.FixedVal[6];
  ang1 := actor.state.params.IntVal[7];
  flags := actor.state.params.IntVal[8];

  if flags and SIXF_ABSOLUTEANGLE = 0 then
    ang1 := ang1 + Actor.angle;

  ang := ang1 shr ANGLETOFINESHIFT;

  if flags and SIXF_ABSOLUTEPOSITION <> 0 then
  begin
    x := actor.x + xofs;
    y := actor.y + yofs;
  end
  else
  begin
    // in relative mode negative y values mean 'left' and positive ones mean 'right'
    // This is the inverse orientation of the absolute mode!
    x := actor.x + FixedMul(xofs, finecosine[ang]) + FixedMul(yofs, finesine[ang]);
    y := actor.y + FixedMul(xofs, finesine[ang]) - FixedMul(yofs, finecosine[ang]);
  end;

  if flags and SIXF_ABSOLUTEMOMENTUM = 0 then
  begin
    // Same orientation issue here!
    newxmom := FixedMul(momx, finecosine[ang]) + FixedMul(momy, finesine[ang]);
    momy := FixedMul(momx, finesine[ang]) - FixedMul(momy, finecosine[ang]);
    momx := newxmom;
  end;

  mo := P_SpawnMobj(x, y, actor.z + zofs, mobj_no);

  if mo <> nil then
  begin
    mo.angle := ang1;
    InitSpawnedItem(actor, mo, flags);
    if flags and SIXF_MULTIPLYSPEED <> 0 then
    begin
      if mo.info.speed < 64 then
      begin
        mo.momx := momx * mo.info.speed;
        mo.momy := momy * mo.info.speed;
        mo.momz := momz * mo.info.speed;
      end
      else
      begin
        mo.momx := FixedMul(momx, mo.info.speed);
        mo.momy := FixedMul(momy, mo.info.speed);
        mo.momz := FixedMul(momz, mo.info.speed);
      end;
    end
    else
    begin
      mo.momx := momx;
      mo.momy := momy;
      mo.momz := momz;
    end;
  end;
end;

//
// Generic seeker missile function
//
// A_SeekerMissile(threshold: angle; turnMax: angle)
procedure A_SeekerMissile(actor: Pmobj_t);
begin
  if not P_CheckStateParams(actor) then
    exit;

  P_SeekerMissile(actor, actor.state.params.IntVal[0] * ANG1, actor.state.params.IntVal[1] * ANG1);
end;

procedure A_CStaffMissileSlither(actor: Pmobj_t);
var
  newX, newY: fixed_t;
  weaveXY: integer;
  angle: angle_t;
begin
  weaveXY := actor.bob;
  angle := (actor.angle + ANG90) shr ANGLETOFINESHIFT;
  newX := actor.x - FixedMul(finecosine[angle], FloatBobOffsets[weaveXY]);
  newY := actor.y - FixedMul(finesine[angle], FloatBobOffsets[weaveXY]);
  weaveXY := (weaveXY + 3) and 63;
  newX := newX + FixedMul(finecosine[angle], FloatBobOffsets[weaveXY]);
  newY := newY + FixedMul(finesine[angle], FloatBobOffsets[weaveXY]);
  P_TryMove(actor, newX, newY);
  actor.bob := weaveXY;
end;

procedure A_SetTranslucent(actor: Pmobj_t);
var
  newstyle: integer;
begin
  if not P_CheckStateParams(actor) then
    exit;

  actor.alpha := actor.state.params.FixedVal[0];

  if actor.alpha <= 0 then
  begin
    actor.renderstyle := mrs_normal;
    actor.flags := actor.flags or MF_SHADOW;
    actor.alpha := 0;
  end
  else if actor.alpha >= FRACUNIT then
  begin
    actor.renderstyle := mrs_normal;
    actor.flags := actor.flags and not MF_SHADOW;
    actor.alpha := FRACUNIT;
  end
  else
  begin
    if actor.renderstyle = mrs_normal then
      actor.renderstyle := mrs_translucent;
  end;

  if actor.state.params.Count = 1 then
    Exit;

  if not actor.state.params.IsComputed[1] then
    actor.state.params.IntVal[1] := Ord(R_GetRenderstyleForName(actor.state.params.StrVal[1]));

  newstyle := actor.state.params.IntVal[1];
  if newstyle = Ord(mrs_translucent) then
  begin
    actor.renderstyle := mrs_translucent;
    actor.flags := actor.flags and not MF_SHADOW;
  end
  else if newstyle = Ord(mrs_add) then
  begin
    actor.renderstyle := mrs_add;
    actor.flags := actor.flags and not MF_SHADOW;
  end
  else if newstyle = Ord(mrs_subtract) then
  begin
    actor.renderstyle := mrs_subtract;
    actor.flags := actor.flags and not MF_SHADOW;
  end
  else if newstyle = Ord(mrs_normal) then
  begin
    actor.renderstyle := mrs_normal;
    actor.flags := actor.flags and not MF_SHADOW;
  end;

end;

//
// FadeOut(reduce = 10%)
//
procedure A_FadeOut(actor: Pmobj_t);
var
  reduce: fixed_t;
begin
  reduce := FRACUNIT div 10;

  if actor.state.params <> nil then
    if actor.state.params.Count > 0 then
      reduce := actor.state.params.FixedVal[0];

  if actor.renderstyle = mrs_normal then
  begin
    actor.renderstyle := mrs_translucent;
    actor.alpha := FRACUNIT;
  end;

  actor.alpha := actor.alpha - reduce;
  if actor.alpha <= 0 then
    P_RemoveMobj(actor);
end;

// reduce -> percentage to reduce fading
procedure Do_FadeOut(actor: Pmobj_t; const reduce: integer);
begin
  if actor.renderstyle = mrs_normal then
  begin
    actor.renderstyle := mrs_translucent;
    actor.alpha := FRACUNIT;
  end;

  actor.alpha := actor.alpha - (reduce * FRACUNIT) div 100;
  if actor.alpha <= 0 then
    P_RemoveMobj(actor);
end;

procedure A_FadeOut10(actor: Pmobj_t);
begin
  Do_FadeOut(actor, 10);
end;

procedure A_FadeOut20(actor: Pmobj_t);
begin
  Do_FadeOut(actor, 20);
end;

procedure A_FadeOut30(actor: Pmobj_t);
begin
  Do_FadeOut(actor, 30);
end;

//
// FadeIn(incriment = 10%)
//
procedure A_FadeIn(actor: Pmobj_t);
var
  incriment: fixed_t;
begin
  if actor.renderstyle = mrs_normal then
    exit;

  incriment := FRACUNIT div 10;

  if actor.state.params <> nil then
    if actor.state.params.Count > 0 then
      incriment := actor.state.params.FixedVal[0];

  actor.alpha := actor.alpha + incriment;
  if actor.alpha >= FRACUNIT then
  begin
    actor.renderstyle := mrs_normal;
    actor.alpha := FRACUNIT;
  end;
end;

// incriment -> percentage to inscrease fading
procedure Do_FadeIn(actor: Pmobj_t; const incriment: integer);
begin
  actor.renderstyle := mrs_translucent;
  actor.alpha := actor.alpha + (incriment * FRACUNIT) div 100;
  if actor.alpha > FRACUNIT then
  begin
    actor.alpha := FRACUNIT;
    actor.renderstyle := mrs_normal
  end;
end;

procedure A_FadeIn10(actor: Pmobj_t);
begin
  Do_FadeIn(actor, 10);
end;

procedure A_FadeIn20(actor: Pmobj_t);
begin
  Do_FadeIn(actor, 20);
end;

procedure A_FadeIn30(actor: Pmobj_t);
begin
  Do_FadeIn(actor, 30);
end;

//
// A_MissileAttack(missilename = actor.info.missiletype)
//
procedure A_MissileAttack(actor: Pmobj_t);
var
  missile: Pmobj_t;
  mobj_no: integer;
begin
  mobj_no := actor.info.missiletype;

  if actor.state.params <> nil then
  begin
    if actor.state.params.IsComputed[0] then
      mobj_no := actor.state.params.IntVal[0]
    else
    begin
      mobj_no := Info_GetMobjNumForName(actor.state.params.StrVal[0]);
      actor.state.params.IntVal[0] := mobj_no;
    end;
    if mobj_no = -1 then
    begin
      I_Warning('A_MissileAttack(): Unknown missile %s'#13#10, [actor.state.params.StrVal[0]]);
      exit;
    end;
  end
  else if mobj_no <= 0 then
  begin
    I_Warning('A_MissileAttack(): Unknown missile'#13#10);
    exit;
  end;

  if mobjinfo[mobj_no].speed < 256 then
    mobjinfo[mobj_no].speed := mobjinfo[mobj_no].speed * FRACUNIT;  // JVAL fix me!!!

  missile := P_SpawnMissile(actor, actor.target, mobj_no);

  if missile <> nil then
  begin
    if missile.info.flags_ex and MF_EX_SEEKERMISSILE <> 0 then
      missile.tracer := actor.target;
  end;

end;

//
// A_AdjustSideSpot(sideoffset: float)
//
procedure A_AdjustSideSpot(actor: Pmobj_t);
var
  offs: fixed_t;
  ang: angle_t;
  x, y: fixed_t;
begin
  if not P_CheckStateParams(actor, 1) then
    exit;

  offs := actor.state.params.Fixedval[0];

  ang := actor.angle shr ANGLETOFINESHIFT;

  x := FixedMul(offs, finecosine[ang]);
  y := FixedMul(offs, finesine[ang]);

  actor.x := actor.x + x;
  actor.y := actor.y + y;
end;

//
// JVAL
// A_ThrustZ(momz: float)
// Changes z momentum
//
procedure A_ThrustZ(actor: Pmobj_t);
begin
  if not P_CheckStateParams(actor, 1) then
    exit;

  actor.momz := actor.momz + actor.state.params.FixedVal[0];
end;

//
// JVAL
// A_ThrustXY(mom: float; ang: angle)
// Changes x, y momentum
//
procedure A_ThrustXY(actor: Pmobj_t);
var
  ang: angle_t;
  thrust: fixed_t;
begin
  if not P_CheckStateParams(actor) then
    exit;

  thrust := actor.state.params.FixedVal[0];

  ang := actor.angle + round(actor.state.params.FloatVal[1] * ANG1);
  ang := ang shr ANGLETOFINESHIFT;

  actor.momx := actor.momx + FixedMul(thrust, finecosine[ang]);
  actor.momy := actor.momy + FixedMul(thrust, finesine[ang]);
end;

//
// JVAL
// A_Turn(angle: float)
// Changes the actor's angle
//
procedure A_Turn(actor: Pmobj_t);
var
  ang: angle_t;
begin
  // JVAL: 20210109 - DEHEXTRA support
  if actor.state.params = nil then
  begin
    actor.angle := actor.angle + actor.state.misc1 * ANG1;
    exit;
  end;

  if not P_CheckStateParams(actor, 1) then
    exit;

  ang := round(actor.state.params.FloatVal[0] * ANG1);
  actor.angle := actor.angle + ang;
end;

//
// JVAL
// A_JumpIfCloser(distancetotarget: float, offset: integer)
// Jump conditionally to another state if distance to target is closer to first parameter
//
procedure A_JumpIfCloser(actor: Pmobj_t);
var
  dist: fixed_t;
  target: Pmobj_t;
  offset: integer;
begin
  if not P_CheckStateParams(actor, 2) then
    exit;

  if actor.player = nil then
    target := actor.target
  else
  begin
    // Does the player aim at something that can be shot?
    P_BulletSlope(actor);
    target := linetarget;
  end;

  // No target - no jump
  if target = nil then
    exit;

  dist := actor.state.params.FixedVal[0];
  if P_AproxDistance(actor.x - target.x, actor.y - target.y) < dist then
  begin
    offset := P_GetStateFromNameWithOffsetCheck(actor, actor.state.params.StrVal[1]);
    if @states[offset] <> actor.state then
      P_SetMobjState(actor, statenum_t(offset));
  end;
end;

//
// JVAL
// A_JumpIfHealthLower(health: integer; offset: integer)
// Jump conditionally to another state if health is lower to first parameter
//
procedure A_JumpIfHealthLower(actor: Pmobj_t);
var
  offset: integer;
begin
  if not P_CheckStateParams(actor, 2) then
    exit;

  if actor.health < actor.state.params.IntVal[0] then
  begin
    offset := P_GetStateFromNameWithOffsetCheck(actor, actor.state.params.StrVal[1]);
    if @states[offset] <> actor.state then
      P_SetMobjState(actor, statenum_t(offset));
  end;
end;

procedure A_ScreamAndUnblock(actor: Pmobj_t);
begin
  A_Scream(actor);
  A_NoBlocking(actor);
end;

procedure A_Missile(actor: Pmobj_t);
begin
  actor.flags := actor.flags or MF_MISSILE;
end;

procedure A_NoMissile(actor: Pmobj_t);
begin
  actor.flags := actor.flags and not MF_MISSILE;
end;

//=============================================================================
//
// P_DoNewChaseDir
//
// killough 9/8/98:
//
// Most of P_NewChaseDir(), except for what
// determines the new direction to take
//
//=============================================================================

const
  opposite: array[0..8] of dirtype_t = (
    DI_WEST, DI_SOUTHWEST, DI_SOUTH, DI_SOUTHEAST,
    DI_EAST, DI_NORTHEAST, DI_NORTH, DI_NORTHWEST, DI_NODIR
  );

  diags: array[0..3] of dirtype_t = (
    DI_NORTHWEST, DI_NORTHEAST, DI_SOUTHWEST, DI_SOUTHEAST
  );

procedure P_DoNewChaseDir(actor: Pmobj_t; deltax, deltay: fixed_t);
var
  d: array[0..2] of dirtype_t;
  dt: dirtype_t;
  tdir: integer;
  olddir, turnaround: dirtype_t;
begin
  olddir := dirtype_t(actor.movedir);
  turnaround := opposite[Ord(olddir)];

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
    actor.movedir := Ord(diags[(intval(deltay < 0) shl 1) + intval(deltax > 0)]);
    if (actor.movedir <> Ord(turnaround)) and P_TryWalk(actor) then
      exit;
  end;

  // try other directions
  if (N_Random > 200) or (abs(deltay) > abs(deltax)) then
  begin
    dt := d[1];
    d[1] := d[2];
    d[2] := dt;
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

  // there is no direct path to the player, so pick another direction.
  if olddir <> DI_NODIR then
  begin
    actor.movedir := Ord(olddir);
    if P_TryWalk(actor) then
      exit;
  end;

  // randomly determine direction of search
  if N_Random and 1 <> 0 then
  begin
    for tdir := Ord(DI_EAST) to Ord(DI_SOUTHEAST) do
    begin
      if tdir <> Ord(turnaround) then
      begin
        actor.movedir := tdir;
        if P_TryWalk(actor) then
          exit;
      end;
    end;
  end
  else
  begin
    for tdir := Ord(DI_SOUTHEAST) downto Ord(DI_EAST) - 1 do
    begin
      if tdir <> Ord(turnaround) then
      begin
        actor.movedir := tdir;
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

  actor.movedir := Ord(DI_NODIR);  // can not move
end;

//=============================================================================
//
// P_RandomChaseDir
//
//=============================================================================

procedure P_RandomChaseDir(actor: Pmobj_t);
var
  turndir, tdir: integer;
  olddir: integer;
  turnaround: dirtype_t;
begin
  olddir := actor.movedir;
  turnaround := opposite[olddir];

  // If the actor elects to continue in its current direction, let it do
  // so unless the way is blocked. Then it must turn.
  if N_Random < 150 then
  begin
    if P_TryWalk(actor) then
      exit;
  end;

  turndir := 1 - 2 * (N_Random and 1);

  if olddir = Ord(DI_NODIR) then
    olddir := N_Random and 7;

  tdir := (Ord(olddir) + turndir) and 7;
  while tdir <> olddir do
  begin
    if tdir <> Ord(turnaround) then
    begin
      actor.movedir := tdir;
      if P_TryWalk(actor) then
        exit;
    end;
    tdir := (tdir + turndir) and 7;
  end;

  if turnaround <> DI_NODIR then
  begin
    actor.movedir := Ord(turnaround);
    if P_TryWalk(actor) then
    begin
      actor.movecount := N_Random and 15;
      exit;
    end;
  end;

  actor.movedir := Ord(DI_NODIR);  // cannot move
end;

//
// A_Wander
//
procedure A_Wander(actor: Pmobj_t);
var
  delta: integer;
begin
  // modify target threshold
  if actor.threshold <> 0 then
    actor.threshold := actor.threshold - 1;

  // turn towards movement direction if not there yet
  if actor.movedir < 8 then
  begin
    actor.angle := actor.angle and $E0000000;
    delta := actor.angle - _SHLW(actor.movedir, 29);

    if delta > 0 then
      actor.angle := actor.angle - ANG45
    else if delta < 0 then
      actor.angle := actor.angle + ANG45;
  end;

  dec(actor.movecount);
  if (actor.movecount < 0) or P_Move(actor) then
  begin
    P_RandomChaseDir(actor);
    actor.movecount := actor.movecount + 5;
  end;
end;

procedure A_GhostOn(actor: Pmobj_t);
begin
  actor.flags := actor.flags or MF_SHADOW;
end;

procedure A_GhostOff(actor: Pmobj_t);
begin
  actor.flags := actor.flags and not MF_SHADOW;
end;

procedure A_Turn5(actor: Pmobj_t);
var
  ang: angle_t;
begin
  ang := 5 * ANG1;
  actor.angle := actor.angle + ang;
end;

procedure A_Turn10(actor: Pmobj_t);
var
  ang: angle_t;
begin
  ang := 10 * ANG1;
  actor.angle := actor.angle + ang;
end;

//
// JVAL
// Set blocking flag
//
procedure A_Blocking(actor: Pmobj_t);
begin
  actor.flags := actor.flags or MF_SOLID;
end;

procedure A_DoNotRunScripts(actor: Pmobj_t);
begin
  actor.flags2_ex := actor.flags2_ex or MF2_EX_DONTRUNSCRIPTS;
end;

procedure A_DoRunScripts(actor: Pmobj_t);
begin
  actor.flags2_ex := actor.flags2_ex and not MF2_EX_DONTRUNSCRIPTS;
end;

procedure A_SetDropItem(actor: Pmobj_t);
var
  mobj_no: integer;
begin
  if not P_CheckStateParams(actor, 1) then
    exit;

  if actor.state.params.IsComputed[0] then
    mobj_no := actor.state.params.IntVal[0]
  else
  begin
    mobj_no := Info_GetMobjNumForName(actor.state.params.StrVal[0]);
    actor.state.params.IntVal[0] := mobj_no;
  end;
  if mobj_no = -1 then
  begin
    I_Warning('A_SetDropItem(): Unknown item %s'#13#10, [actor.state.params.StrVal[0]]);
    exit;
  end;

  actor.dropitem := mobj_no;
  actor.flags2_ex := actor.flags2_ex or MF2_EX_CUSTOMDROPITEM;
end;

procedure A_SetDefaultDropItem(actor: Pmobj_t);
begin
  actor.dropitem := 0;
  actor.flags2_ex := actor.flags2_ex and not MF2_EX_CUSTOMDROPITEM;
end;

procedure A_TargetDropItem(actor: Pmobj_t);
var
  mobj_no: integer;
begin
  if not P_CheckStateParams(actor, 1) then
    exit;

  if actor.target = nil then
    exit;

  if actor.state.params.IsComputed[0] then
    mobj_no := actor.state.params.IntVal[0]
  else
  begin
    mobj_no := Info_GetMobjNumForName(actor.state.params.StrVal[0]);
    actor.state.params.IntVal[0] := mobj_no;
  end;
  if mobj_no = -1 then
  begin
    I_Warning('A_TargetDropItem(): Unknown item %s'#13#10, [actor.state.params.StrVal[0]]);
    exit;
  end;

  actor.target.dropitem := mobj_no;
  actor.target.flags2_ex := actor.target.flags2_ex or MF2_EX_CUSTOMDROPITEM;
end;

procedure A_DefaultTargetDropItem(actor: Pmobj_t);
begin
  if actor.target = nil then
    exit;

  actor.target.dropitem := 0;
  actor.target.flags2_ex := actor.target.flags2_ex and not MF2_EX_CUSTOMDROPITEM;
end;

function P_ActorTarget(const actor: Pmobj_t): Pmobj_t;
begin
  if actor = nil then
  begin
    result := nil;
    exit;
  end;

  if actor.player = nil then
    result := actor.target
  else
  begin
    // Does the player aim at something that can be shot?
    P_BulletSlope(actor);
    result := linetarget;
  end;
end;

//
// A_GlobalEarthQuake (tics: integer);
//
procedure A_GlobalEarthQuake(actor: Pmobj_t);
var
  qtics: integer;
  i: integer;
begin
  if not P_CheckStateParams(actor, 1) then
    exit;

  qtics := actor.state.params.IntVal[0] * FRACUNIT;
  for i := 0 to MAXPLAYERS - 1 do
    if playeringame[i] then
    begin
      players[i].quaketics := qtics;
      players[i].quakeintensity := FRACUNIT;
    end;
end;

// A_SetMapStr(var: string; value1: string; [value2: string],...)
procedure A_SetMapStr(actor: Pmobj_t);
var
  s: string;
  i: integer;
begin
  if not P_CheckStateParams(actor) then
    exit;

  s := '';
  for i := 1 to actor.state.params.Count - 1 do
  begin
    s := s + actor.state.params.StrVal[i];
    if i < actor.state.params.Count - 1 then
      s := s + ' ';
  end;

  PS_SetMapStr(actor.state.params.StrVal[0], s);
end;

// A_SetWorldStr(var: string; value1: string; [value2: string],...)
procedure A_SetWorldStr(actor: Pmobj_t);
var
  s: string;
  i: integer;
begin
  if not P_CheckStateParams(actor) then
    exit;

  s := '';
  for i := 1 to actor.state.params.Count - 1 do
  begin
    s := s + actor.state.params.StrVal[i];
    if i < actor.state.params.Count - 1 then
      s := s + ' ';
  end;

  PS_SetWorldStr(actor.state.params.StrVal[0], s);
end;

// A_SetMapInt(var: string; value: integer);
procedure A_SetMapInt(actor: Pmobj_t);
begin
  if not P_CheckStateParams(actor, 2) then
    exit;

  PS_SetMapInt(actor.state.params.StrVal[0], actor.state.params.IntVal[1]);
end;

// A_SetWorldInt(var: string; value: integer);
procedure A_SetWorldInt(actor: Pmobj_t);
begin
  if not P_CheckStateParams(actor, 2) then
    exit;

  PS_SetWorldInt(actor.state.params.StrVal[0], actor.state.params.IntVal[1]);
end;

// A_SetMapFloat(var: string; value: float);
procedure A_SetMapFloat(actor: Pmobj_t);
begin
  if not P_CheckStateParams(actor, 2) then
    exit;

  PS_SetMapFloat(actor.state.params.StrVal[0], actor.state.params.FloatVal[1]);
end;

// A_SetWorldFloat(var: string; value: float);
procedure A_SetWorldFloat(actor: Pmobj_t);
begin
  if not P_CheckStateParams(actor, 2) then
    exit;

  PS_SetWorldFloat(actor.state.params.StrVal[0], actor.state.params.FloatVal[1]);
end;

//
// A_RandomGoto(state1, state2, ....)
//
procedure A_RandomGoto(actor: Pmobj_t);
var
  newstate: integer;
  idx: integer;
begin
  if not P_CheckStateParams(actor) then
    exit;

  idx := N_Random mod actor.state.params.Count;

  if not actor.state.params.IsComputed[idx] then
    actor.state.params.IntVal[idx] := P_GetStateFromName(actor, actor.state.params.StrVal[idx]);
  newstate := actor.state.params.IntVal[idx];

  P_SetMobjState(actor, statenum_t(newstate));
end;

procedure P_SetHealth(const mo: Pmobj_t; const h: integer);
var
  p: Pplayer_t;
begin
  if mo.health <= 0 then
    exit;

  if h <= 0 then
  begin
    P_DamageMobj(mo, nil, nil, 10000);
    exit;
  end;

  mo.health := h;
  p := mo.player;
  if p <> nil then
    p.health := h;
end;

procedure A_ResetHealth(actor: Pmobj_t);
begin
  P_SetHealth(actor, actor.info.spawnhealth);
end;

procedure A_SetHealth(actor: Pmobj_t);
begin
  if not P_CheckStateParams(actor, 1) then
    exit;

  P_SetHealth(actor, actor.state.params.IntVal[0]);
end;

procedure A_ResetTargetHealth(actor: Pmobj_t);
begin
  if actor.target <> nil then
    P_SetHealth(actor.target, actor.target.info.spawnhealth);
end;

procedure A_SetTargetHealth(actor: Pmobj_t);
begin
  if actor.target = nil then
    exit;

  if not P_CheckStateParams(actor, 1) then
    exit;

  P_SetHealth(actor.target, actor.state.params.IntVal[0]);
end;

procedure A_Recoil(actor: Pmobj_t);
var
  xymom: fixed_t;
  angle: angle_t;
begin
  if not P_CheckStateParams(actor, 1) then
    exit;

  xymom := actor.state.params.FixedVal[0];

  angle := (actor.angle + ANG180) shr ANGLETOFINESHIFT;
  actor.momx := actor.momx + FixedMul(xymom, finecosine[angle]);
  actor.momy := actor.momy + FixedMul(xymom, finesine[angle]);
end;

procedure A_SetSolid(actor: Pmobj_t);
begin
  actor.flags := actor.flags or MF_SOLID;
end;

procedure A_UnSetSolid(actor: Pmobj_t);
begin
  actor.flags := actor.flags and not MF_SOLID;
end;

procedure A_SetFloat(actor: Pmobj_t);
begin
  actor.flags := actor.flags or MF_FLOAT;
end;

procedure A_UnSetFloat(actor: Pmobj_t);
begin
  actor.flags := actor.flags and not (MF_FLOAT or MF_INFLOAT);
end;

//
// A_ScaleVelocity(scale: float)
// zDoom compatibility
//
procedure A_ScaleVelocity(actor: Pmobj_t);
var
  scale: fixed_t;
begin
  if not P_CheckStateParams(actor) then
    exit;

  scale := actor.state.params.FixedVal[0];

  actor.momx := FixedMul(actor.momx, scale);
  actor.momy := FixedMul(actor.momy, scale);
  actor.momz := FixedMul(actor.momz, scale);
end;

//
// A_ChangeVelocity(velx, vely, velz: float; flags: integer)
// zDoom compatibility
//
procedure A_ChangeVelocity(actor: Pmobj_t);
var
  vx, vy, vz: fixed_t;
  vx1, vy1: fixed_t;
  an: angle_t;
  sina, cosa: fixed_t;
  flags: integer;
  stmp: string;
  sc: TSCriptEngine;
  i: integer;
begin
  if not P_CheckStateParams(actor, 1, CSP_AT_LEAST) then
    exit;

  vx := actor.state.params.FixedVal[0];
  vy := actor.state.params.FixedVal[1];
  vz := actor.state.params.FixedVal[2];

  if not actor.state.params.IsComputed[3] then
  begin
    stmp := actor.state.params.StrVal[3];
    for i := 1 to Length(stmp) do
      if stmp[i] = '|' then
        stmp[i] := ' ';
    flags := 0;
    sc := TSCriptEngine.Create(stmp);
    while sc.GetString do
      flags := flags or (SC_EvalueateIntToken(sc._String, ['CVF_RELATIVE', 'CVF_REPLACE']) + 1);
    sc.Free;
    actor.state.params.IntVal[3] := flags;
  end
  else
    flags := actor.state.params.IntVal[3];

  if flags and 1 <> 0 then
  begin
    an := actor.angle shr ANGLETOFINESHIFT;
    sina := finesine[an];
    cosa := finecosine[an];
    vx1 := vx;
    vy1 := vy;
    vx := FixedMul(vx1, cosa) - FixedMul(vy1, sina);
    vy := FixedMul(vx1, sina) + FixedMul(vy1, cosa);
  end;

  if flags and 2 <> 0 then
  begin
    actor.momx := vx;
    actor.momy := vy;
    actor.momz := vz;
  end
  else
  begin
    actor.momx := actor.momx + vx;
    actor.momy := actor.momy + vy;
    actor.momz := actor.momz + vz;
  end;
end;

procedure A_SetPushFactor(actor: Pmobj_t);
begin
  if not P_CheckStateParams(actor, 1) then
    exit;

  actor.pushfactor := actor.state.params.FixedVal[0];
end;

procedure A_SetScale(actor: Pmobj_t);
begin
  if not P_CheckStateParams(actor, 1) then
    exit;

  actor.scale := actor.state.params.FixedVal[0];
end;

procedure A_SetGravity(actor: Pmobj_t);
begin
  if not P_CheckStateParams(actor, 1) then
    exit;

  actor.gravity := actor.state.params.FixedVal[0];
end;


procedure A_SetFloorBounce(actor: Pmobj_t);
begin
  actor.flags3_ex := actor.flags3_ex or MF3_EX_FLOORBOUNCE;
end;

procedure A_UnSetFloorBounce(actor: Pmobj_t);
begin
  actor.flags3_ex := actor.flags3_ex and not MF3_EX_FLOORBOUNCE;
end;

procedure A_SetCeilingBounce(actor: Pmobj_t);
begin
  actor.flags3_ex := actor.flags3_ex or MF3_EX_CEILINGBOUNCE;
end;

procedure A_UnSetCeilingBounce(actor: Pmobj_t);
begin
  actor.flags3_ex := actor.flags3_ex and not MF3_EX_CEILINGBOUNCE;
end;

procedure A_SetWallBounce(actor: Pmobj_t);
begin
  actor.flags3_ex := actor.flags3_ex or MF3_EX_WALLBOUNCE;
end;

procedure A_UnSetWallBounce(actor: Pmobj_t);
begin
  actor.flags3_ex := actor.flags3_ex and not MF3_EX_WALLBOUNCE;
end;

procedure A_GlowLight(actor: Pmobj_t);
const
  ACL_NONE = 0;
  ACL_WHITE = 1;
  ACL_RED = 2;
  ACL_GREEN = 3;
  ACL_BLUE = 4;
  ACL_YELLOW = 5;
var
  scolor: string;
begin
  if not P_CheckStateParams(actor, 1, CSP_AT_LEAST) then
    exit;

  if not actor.state.params.IsComputed[0] then
  begin
    scolor := strupper(strtrim(actor.state.params.StrVal[0]));
    if scolor = 'WHITE' then
      actor.state.params.IntVal[0] := ACL_WHITE
    else if scolor = 'RED' then
      actor.state.params.IntVal[0] := ACL_RED
    else if scolor = 'GREEN' then
      actor.state.params.IntVal[0] := ACL_GREEN
    else if scolor = 'BLUE' then
      actor.state.params.IntVal[0] := ACL_BLUE
    else if scolor = 'YELLOW' then
      actor.state.params.IntVal[0] := ACL_YELLOW
    else
      actor.state.params.IntVal[0] := ACL_NONE;
  end;

  actor.flags_ex := actor.flags_ex and not MF_EX_LIGHT;
  case actor.state.params.IntVal[0] of
    ACL_WHITE: actor.flags_ex := actor.flags_ex or MF_EX_WHITELIGHT;
    ACL_RED: actor.flags_ex := actor.flags_ex or MF_EX_REDLIGHT;
    ACL_GREEN: actor.flags_ex := actor.flags_ex or MF_EX_GREENLIGHT;
    ACL_BLUE: actor.flags_ex := actor.flags_ex or MF_EX_BLUELIGHT;
    ACL_YELLOW: actor.flags_ex := actor.flags_ex or MF_EX_YELLOWLIGHT;
  end;
end;

function P_TicsFromState(const st: Pstate_t): integer;
begin
  if st.flags_ex and MF_EX_STATE_RANDOM_SELECT <> 0 then
  begin
    if P_Random < 128 then
      result := st.tics
    else
      result := st.tics2;
  end
  else if st.flags_ex and MF_EX_STATE_RANDOM_RANGE <> 0 then
  begin
    if st.tics2 > st.tics then
      result := st.tics + P_Random mod (st.tics2 - st.tics + 1)
    else if st.tics2 < st.tics then
      result := st.tics + P_Random mod (st.tics - st.tics2 + 1)
    else
      result := st.tics;
  end
  else
    result := st.tics;
end;

const
  DEFTRACEANGLE = 15 * ANG1;

//
// A_TraceNearestPlayer(pct: integer, [maxturn: angle])
// pct -> propability
procedure A_TraceNearestPlayer(actor: Pmobj_t);
var
  pct: integer;
  exact: angle_t;
  dist: fixed_t;
  slope: fixed_t;
  dest: Pmobj_t;
  i: integer;
  nearest: integer;
  mindist: integer;
  maxturn: angle_t;
begin
  if not P_CheckStateParams(actor, 1, CSP_AT_LEAST) then
    exit;

  pct := actor.state.params.IntVal[0];
  if pct < P_Random then
    exit;

  dest := nil;
  nearest := MAXINT;

  for i := 0 to MAXPLAYERS - 1 do
    if playeringame[i] then
      if players[i].mo <> nil then
        if players[i].mo.health >= 0 then
        begin
          mindist := P_AproxDistance(players[i].mo.x - actor.x, players[i].mo.y - actor.y);
          if mindist < nearest then
          begin
            nearest := mindist;
            dest := players[i].mo;
          end;
        end;

  if dest = nil then
    exit;

  // change angle
  exact := R_PointToAngle2(actor.x, actor.y, dest.x, dest.y);

  if actor.state.params.Count >= 2 then
    maxturn := actor.state.params.IntVal[1] * ANG1
  else
    maxturn := DEFTRACEANGLE;

  if exact <> actor.angle then
  begin
    if exact - actor.angle > ANG180 then
    begin
      actor.angle := actor.angle - maxturn;
      if exact - actor.angle < ANG180 then
        actor.angle := exact;
    end
    else
    begin
      actor.angle := actor.angle + maxturn;
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

procedure A_ChangeFlag(actor: Pmobj_t);
var
  sflag: string;
  change: boolean;
  flg: LongWord;
  idx: integer;
begin
  if not P_CheckStateParams(actor, 2, CSP_AT_LEAST) then
    exit;

  sflag := strupper(actor.state.params.StrVal[0]);
  change := actor.state.params.BoolVal[1];

  idx := mobj_flags.IndexOf(sflag);
  if idx < 0 then
    idx := mobj_flags.IndexOf('MF_' + sflag);
  if idx >= 0 then
  begin
    flg := 1 shl idx;
    if change then
      actor.flags := actor.flags or flg
    else
      actor.flags := actor.flags and not flg;
    exit;
  end;

  {$IFDEF HERETIC_OR_HEXEN}
  idx := mobj_flags2.IndexOf(sflag);
  if idx < 0 then
    idx := mobj_flags2.IndexOf('MF2_' + sflag);
  if idx >= 0 then
  begin
    flg := 1 shl idx;
    if change then
      actor.flags2 := actor.flags2 or flg
    else
      actor.flags2 := actor.flags2 and not flg;
    exit;
  end;
  {$ENDIF}

  idx := mobj_flags_ex.IndexOf(sflag);
  if idx < 0 then
    idx := mobj_flags_ex.IndexOf('MF_EX_' + sflag);
  if idx >= 0 then
  begin
    flg := 1 shl idx;
    if change then
      actor.flags_ex := actor.flags_ex or flg
    else
      actor.flags_ex := actor.flags_ex and not flg;
    exit;
  end;

  idx := mobj_flags2_ex.IndexOf(sflag);
  if idx < 0 then
    idx := mobj_flags2_ex.IndexOf('MF2_EX_' + sflag);
  if idx >= 0 then
  begin
    flg := 1 shl idx;
    if change then
      actor.flags2_ex := actor.flags2_ex or flg
    else
      actor.flags2_ex := actor.flags2_ex and not flg;
    exit;
  end;

  idx := mobj_flags3_ex.IndexOf(sflag);
  if idx < 0 then
    idx := mobj_flags3_ex.IndexOf('MF3_EX_' + sflag);
  if idx >= 0 then
  begin
    flg := 1 shl idx;
    if change then
      actor.flags3_ex := actor.flags3_ex or flg
    else
      actor.flags3_ex := actor.flags3_ex and not flg;
    exit;
  end;

  idx := mobj_flags4_ex.IndexOf(sflag);
  if idx < 0 then
    idx := mobj_flags4_ex.IndexOf('MF4_EX_' + sflag);
  if idx >= 0 then
  begin
    flg := 1 shl idx;
    if change then
      actor.flags4_ex := actor.flags4_ex or flg
    else
      actor.flags4_ex := actor.flags4_ex and not flg;
    exit;
  end;
end;

procedure A_CheckFloor(actor: Pmobj_t);
var
  offset: integer;
begin
  if not P_CheckStateParams(actor, 1) then
    exit;

  if actor.z <= actor.floorz then
  begin
    offset := P_GetStateFromNameWithOffsetCheck(actor, actor.state.params.StrVal[0]);
    if @states[offset] <> actor.state then
      P_SetMobjState(actor, statenum_t(offset));
  end;
end;

procedure A_CheckCeiling(actor: Pmobj_t);
var
  offset: integer;
begin
  if not P_CheckStateParams(actor, 1) then
    exit;

  if actor.z >= actor.ceilingz then
  begin
    offset := P_GetStateFromNameWithOffsetCheck(actor, actor.state.params.StrVal[0]);
    if @states[offset] <> actor.state then
      P_SetMobjState(actor, statenum_t(offset));
  end;
end;

procedure A_StopSound(actor: Pmobj_t);
begin
  S_StopSound(actor);
end;

procedure A_JumpIfTargetOutsideMeleeRange(actor: Pmobj_t);
var
  offset: integer;
begin
  if not P_CheckStateParams(actor, 1) then
    exit;

  if not P_CheckMeleeRange(actor) then
  begin
    offset := P_GetStateFromNameWithOffsetCheck(actor, actor.state.params.StrVal[0]);
    if @states[offset] <> actor.state then
      P_SetMobjState(actor, statenum_t(offset));
  end;
end;

procedure A_JumpIfTargetInsideMeleeRange(actor: Pmobj_t);
var
  offset: integer;
begin
  if not P_CheckStateParams(actor, 1) then
    exit;

  if P_CheckMeleeRange(actor) then
  begin
    offset := P_GetStateFromNameWithOffsetCheck(actor, actor.state.params.StrVal[0]);
    if @states[offset] <> actor.state then
      P_SetMobjState(actor, statenum_t(offset));
  end;
end;

//
// A_JumpIfTracerCloser(distancetotarget: float, offset: integer)
//
procedure A_JumpIfTracerCloser(actor: Pmobj_t);
var
  dist: fixed_t;
  offset: integer;
begin
  if not P_CheckStateParams(actor, 2) then
    exit;

  // No tracer - no jump
  if actor.tracer = nil then
    exit;

  dist := actor.state.params.FixedVal[0];
  if P_AproxDistance(actor.x - actor.tracer.x, actor.y - actor.tracer.y) < dist then
  begin
    offset := P_GetStateFromNameWithOffsetCheck(actor, actor.state.params.StrVal[1]);
    if @states[offset] <> actor.state then
      P_SetMobjState(actor, statenum_t(offset));
  end;
end;

//
// A_SetMass(mass: integer)
//
procedure A_SetMass(actor: Pmobj_t);
begin
  if not P_CheckStateParams(actor, 1) then
    exit;

  actor.mass := actor.state.params.IntVal[0];
end;

//
// A_SetTargetMass(mass: integer)
//
procedure A_SetTargetMass(actor: Pmobj_t);
begin
  if not P_CheckStateParams(actor, 1) then
    exit;

  if actor.target = nil then
    exit;

  actor.target.mass := actor.state.params.IntVal[0];
end;

//
// A_SetTracerMass(mass: integer)
//
procedure A_SetTracerMass(actor: Pmobj_t);
begin
  if not P_CheckStateParams(actor, 1) then
    exit;

  if actor.tracer = nil then
    exit;

  actor.tracer.mass := actor.state.params.IntVal[0];
end;

//
// A_SetMasterMass(mass: integer)
//
procedure A_SetMasterMass(actor: Pmobj_t);
begin
  if not P_CheckStateParams(actor, 1) then
    exit;

  if actor.master = nil then
    exit;

  actor.master.mass := actor.state.params.IntVal[0];
end;

//
// A_CheckSight(offset: integer)
// Jumps to offset if no player can see this actor
//
procedure A_CheckSight(actor: Pmobj_t);
var
  i: integer;
  offset: integer;
begin
  if not P_CheckStateParams(actor, 1) then
    exit;

  for i := 0 to MAXPLAYERS - 1 do
    if playeringame[i] then
      if players[i].mo <> actor then
        if P_CheckSight(players[i].mo, actor) then
          exit;

  offset := P_GetStateFromNameWithOffsetCheck(actor, actor.state.params.StrVal[0]);
  if @states[offset] <> actor.state then
    P_SetMobjState(actor, statenum_t(offset));
end;

//
// A_CheckSightOrRange(distance: float; offset: integer; [twodi: boolean=false])
// Jumps to offset if no player can see this actor or out of player range
//
procedure A_CheckSightOrRange(actor: Pmobj_t);
var
  i: integer;
  offset: integer;
  distance: fixed64_t;
  range: fixed64_t;
  twodi: boolean;
  dx, dy, dz: fixed64_t;
begin
  if not P_CheckStateParams(actor, 2, CSP_AT_LEAST) then
    exit;

  distance := actor.state.params.FixedVal[0];
  distance := FixedMul64(distance, distance);
  twodi := actor.state.params.BoolVal[2];

  for i := 0 to MAXPLAYERS - 1 do
    if playeringame[i] then
      if players[i].mo <> actor then
      begin
        dx := players[i].mo.x - actor.x;
        dy := players[i].mo.y - actor.y;
        if twodi then
        begin
          dz := players[i].mo.z - actor.z;
          range := FixedMul64(dx, dx) + FixedMul64(dy, dy) + FixedMul64(dz, dz);
        end
        else
          range := FixedMul64(dx, dx) + FixedMul64(dy, dy);
        if distance <= range then
          exit;
      end;

  for i := 0 to MAXPLAYERS - 1 do
    if playeringame[i] then
      if players[i].mo <> actor then
        if P_CheckSight(players[i].mo, actor) then
          exit;

  offset := P_GetStateFromNameWithOffsetCheck(actor, actor.state.params.StrVal[1]);
  if @states[offset] <> actor.state then
    P_SetMobjState(actor, statenum_t(offset));
end;

//
// A_CheckRange(distance: float; offset: integer; [twodi: boolean=false])
// Jumps to offset if out of player range
//
procedure A_CheckRange(actor: Pmobj_t);
var
  i: integer;
  offset: integer;
  distance: fixed64_t;
  range: fixed64_t;
  twodi: boolean;
  dx, dy, dz: fixed64_t;
begin
  if not P_CheckStateParams(actor, 2, CSP_AT_LEAST) then
    exit;

  distance := actor.state.params.FixedVal[0];
  distance := FixedMul64(distance, distance);
  twodi := actor.state.params.BoolVal[2];

  for i := 0 to MAXPLAYERS - 1 do
    if playeringame[i] then
      if players[i].mo <> actor then
      begin
        dx := players[i].mo.x - actor.x;
        dy := players[i].mo.y - actor.y;
        if twodi then
        begin
          dz := players[i].mo.z - actor.z;
          range := FixedMul64(dx, dx) + FixedMul64(dy, dy) + FixedMul64(dz, dz);
        end
        else
          range := FixedMul64(dx, dx) + FixedMul64(dy, dy);
        if distance <= range then
          exit;
      end;

  offset := P_GetStateFromNameWithOffsetCheck(actor, actor.state.params.StrVal[1]);
  if @states[offset] <> actor.state then
    P_SetMobjState(actor, statenum_t(offset));
end;

//
// A_CountdownArg(arg: integer; offset: integer);
//
procedure A_CountdownArg(actor: Pmobj_t);
var
  arg: integer;
  sarg: string;
  offset: integer;
begin
  if not P_CheckStateParams(actor, 2) then
    exit;

  if not actor.state.params.IsComputed[0] then
  begin
    sarg := strupper(actor.state.params.StrVal[0]);
    if sarg = 'C_ARG1' then
      actor.state.params.IntVal[0] := 0
    else if sarg = 'C_ARG2' then
      actor.state.params.IntVal[0] := 1
    else if sarg = 'C_ARG3' then
      actor.state.params.IntVal[0] := 2
    else if sarg = 'C_ARG4' then
      actor.state.params.IntVal[0] := 3
    else if sarg = 'C_ARG5' then
      actor.state.params.IntVal[0] := 4;
  end;

  arg := actor.state.params.IntVal[0];
  if not IsIntegerInRange(arg, 0, 4) then
    exit;

  if actor.args[arg] = 0 then
    Exit;

  Dec(actor.args[arg]);
  if actor.args[arg] = 0 then
  begin
    if actor.state.params.Count = 1 then
      offset := -1
    else
      offset := P_GetStateFromNameWithOffsetCheck(actor, actor.state.params.StrVal[1]);
    if offset = -1 then
    begin
      if actor.flags and MF_MISSILE <> 0 then
      begin
        P_ExplodeMissile(actor);
        Exit;
      end
      else if actor.flags and MF_SHOOTABLE <> 0 then
      begin
        P_DamageMobj(actor, nil, nil, 10000);
        Exit;
      end
      else
      begin
        offset := actor.info.deathstate;
        if offset < 0 then
          offset := Ord(S_NULL);
      end;
    end;
    if @states[offset] <> actor.state then
      P_SetMobjState(actor, statenum_t(offset));
  end;
end;

//
// A_SetArg(arg: integer; value: integer)
//
procedure A_SetArg(actor: Pmobj_t);
var
  arg: integer;
  sarg: string;
begin
  if not P_CheckStateParams(actor, 2) then
    exit;

  if not actor.state.params.IsComputed[0] then
  begin
    sarg := strupper(actor.state.params.StrVal[0]);
    if sarg = 'C_ARG1' then
      actor.state.params.IntVal[0] := 0
    else if sarg = 'C_ARG2' then
      actor.state.params.IntVal[0] := 1
    else if sarg = 'C_ARG3' then
      actor.state.params.IntVal[0] := 2
    else if sarg = 'C_ARG4' then
      actor.state.params.IntVal[0] := 3
    else if sarg = 'C_ARG5' then
      actor.state.params.IntVal[0] := 4;
  end;

  arg := actor.state.params.IntVal[0];
  if not IsIntegerInRange(arg, 0, 4) then
    exit;

  actor.args[arg] := actor.state.params.IntVal[1];
end;

//
// A_SetMasterArg(arg: integer; value: integer)
//
procedure A_SetMasterArg(actor: Pmobj_t);
var
  arg: integer;
  sarg: string;
begin
  if actor.master = nil then
    exit;
  if not P_CheckStateParams(actor, 2) then
    exit;

  if not actor.state.params.IsComputed[0] then
  begin
    sarg := strupper(actor.state.params.StrVal[0]);
    if sarg = 'C_ARG1' then
      actor.state.params.IntVal[0] := 0
    else if sarg = 'C_ARG2' then
      actor.state.params.IntVal[0] := 1
    else if sarg = 'C_ARG3' then
      actor.state.params.IntVal[0] := 2
    else if sarg = 'C_ARG4' then
      actor.state.params.IntVal[0] := 3
    else if sarg = 'C_ARG5' then
      actor.state.params.IntVal[0] := 4;
  end;

  arg := actor.state.params.IntVal[0];
  if not IsIntegerInRange(arg, 0, 4) then
    exit;

  actor.master.args[arg] := actor.state.params.IntVal[1];
end;

//
// A_SetTargetArg(arg: integer; value: integer)
//
procedure A_SetTargetArg(actor: Pmobj_t);
var
  arg: integer;
  sarg: string;
begin
  if actor.target = nil then
    exit;
  if not P_CheckStateParams(actor, 2) then
    exit;

  if not actor.state.params.IsComputed[0] then
  begin
    sarg := strupper(actor.state.params.StrVal[0]);
    if sarg = 'C_ARG1' then
      actor.state.params.IntVal[0] := 0
    else if sarg = 'C_ARG2' then
      actor.state.params.IntVal[0] := 1
    else if sarg = 'C_ARG3' then
      actor.state.params.IntVal[0] := 2
    else if sarg = 'C_ARG4' then
      actor.state.params.IntVal[0] := 3
    else if sarg = 'C_ARG5' then
      actor.state.params.IntVal[0] := 4;
  end;

  arg := actor.state.params.IntVal[0];
  if not IsIntegerInRange(arg, 0, 4) then
    exit;

  actor.target.args[arg] := actor.state.params.IntVal[1];
end;

//
// A_SetTracerArg(arg: integer; value: integer)
//
procedure A_SetTracerArg(actor: Pmobj_t);
var
  arg: integer;
  sarg: string;
begin
  if actor.tracer = nil then
    exit;
  if not P_CheckStateParams(actor, 2) then
    exit;

  if not actor.state.params.IsComputed[0] then
  begin
    sarg := strupper(actor.state.params.StrVal[0]);
    if sarg = 'C_ARG1' then
      actor.state.params.IntVal[0] := 0
    else if sarg = 'C_ARG2' then
      actor.state.params.IntVal[0] := 1
    else if sarg = 'C_ARG3' then
      actor.state.params.IntVal[0] := 2
    else if sarg = 'C_ARG4' then
      actor.state.params.IntVal[0] := 3
    else if sarg = 'C_ARG5' then
      actor.state.params.IntVal[0] := 4;
  end;

  arg := actor.state.params.IntVal[0];
  if not IsIntegerInRange(arg, 0, 4) then
    exit;

  actor.tracer.args[arg] := actor.state.params.IntVal[1];
end;

//
// A_SetSpecial(special: integer; [arg1, arg2, arg3, arg4, arg5: integer]);
//
procedure A_SetSpecial(actor: Pmobj_t);
var
  cnt: integer;
begin
  if not P_CheckStateParams(actor, 1, CSP_AT_LEAST) then
    exit;

  actor.special := actor.state.params.IntVal[0];

  cnt := actor.state.params.Count;
  if cnt > 1 then
  begin
    actor.args[0] := actor.state.params.IntVal[1];
    if cnt > 2 then
    begin
      actor.args[1] := actor.state.params.IntVal[2];
      if cnt > 3 then
      begin
        actor.args[2] := actor.state.params.IntVal[3];
        if cnt > 4 then
        begin
          actor.args[3] := actor.state.params.IntVal[4];
          if cnt > 5 then
            actor.args[4] := actor.state.params.IntVal[5];
        end;
      end;
    end;
  end;
end;

//
// A_CheckFlag(flag: string; offset: integer; [aaprt: AAPTR]);
//
procedure A_CheckFlag(actor: Pmobj_t);
var
  sflag: string;
  dojump: boolean;
  flg: LongWord;
  idx: integer;
  offset: integer;
  mo: Pmobj_t;
begin
  if not P_CheckStateParams(actor, 2, CSP_AT_LEAST) then
    exit;

  sflag := strupper(actor.state.params.StrVal[0]);

  dojump := false;

  mo := COPY_AAPTR(actor, actor.state.params.IntVal[2]);
  if mo = nil then
    exit;

  idx := mobj_flags.IndexOf(sflag);
  if idx < 0 then
    idx := mobj_flags.IndexOf('MF_' + sflag);
  if idx >= 0 then
  begin
    flg := 1 shl idx;
    dojump := mo.flags and flg <> 0;
  end;

  {$IFDEF HERETIC_OR_HEXEN}
  if not dojump then
  begin
    idx := mobj_flags2.IndexOf(sflag);
    if idx < 0 then
      idx := mobj_flags2.IndexOf('MF2_' + sflag);
    if idx >= 0 then
    begin
      flg := 1 shl idx;
      dojump := mo.flags2 and flg <> 0;
    end;
  end;
  {$ENDIF}

  if not dojump then
  begin
    idx := mobj_flags_ex.IndexOf(sflag);
    if idx < 0 then
      idx := mobj_flags_ex.IndexOf('MF_EX_' + sflag);
    if idx >= 0 then
    begin
      flg := 1 shl idx;
      dojump := mo.flags_ex and flg <> 0;
    end;
  end;

  if not dojump then
  begin
    idx := mobj_flags2_ex.IndexOf(sflag);
    if idx < 0 then
      idx := mobj_flags2_ex.IndexOf('MF2_EX_' + sflag);
    if idx >= 0 then
    begin
      flg := 1 shl idx;
      dojump := mo.flags2_ex and flg <> 0;
    end;
  end;

  if not dojump then
  begin
    idx := mobj_flags3_ex.IndexOf(sflag);
    if idx < 0 then
      idx := mobj_flags3_ex.IndexOf('MF3_EX_' + sflag);
    if idx >= 0 then
    begin
      flg := 1 shl idx;
      dojump := mo.flags3_ex and flg <> 0;
    end;
  end;

  if not dojump then
  begin
    idx := mobj_flags4_ex.IndexOf(sflag);
    if idx < 0 then
      idx := mobj_flags4_ex.IndexOf('MF4_EX_' + sflag);
    if idx >= 0 then
    begin
      flg := 1 shl idx;
      dojump := mo.flags4_ex and flg <> 0;
    end;
  end;

  if not dojump then
    exit;

  offset := P_GetStateFromNameWithOffsetCheck(actor, actor.state.params.StrVal[1]);
  if @states[offset] <> actor.state then
      P_SetMobjState(actor, statenum_t(offset));
end;

//
// A_SetAngle(angle: integer: [flags: integer]; [aaprt: AAPTR]);
//
procedure A_SetAngle(actor: Pmobj_t);
var
  mo: Pmobj_t;
  ang: angle_t;
  flags: integer;
begin
  if not P_CheckStateParams(actor, 1, CSP_AT_LEAST) then
    exit;

  mo := COPY_AAPTR(actor, actor.state.params.IntVal[2]);
  if mo = nil then
    exit;

  ang := ANG1 * actor.state.params.IntVal[0];
  flags := actor.state.params.IntVal[1];
  if flags = SPF_FORCECLAMP then
    mo.flags3_ex := mo.flags3_ex or MF3_EX_NORENDERINTERPOLATION
  else if flags = SPF_INTERPOLATE then
    mo.flags3_ex := mo.flags3_ex and not MF3_EX_NORENDERINTERPOLATION;

  mo.angle := ang;
end;

//
// A_SetUserVar(varname: string; value: integer)
// Note: If the variable does not exist we create a new one with the name given.
// In ZDoom displays an error message.
//
procedure A_SetUserVar(actor: Pmobj_t);
begin
  if not P_CheckStateParams(actor, 2) then
    exit;

  P_SetMobjCustomParam(actor, actor.state.params.StrVal[0], actor.state.params.IntVal[1]);
end;

//
// A_SetUserArray(varname: string; index: integer; value: integer)
// Note #1: If the variable does not exist we create a new one with the name given.
// Note #2: No bounds check, since the array is stored as a sparse array.
// Note #3: A variable and an array can share the same name
// Note #4: The name is not nessesary to start with "user_"
// In ZDoom displays an error message.
//
procedure A_SetUserArray(actor: Pmobj_t);
var
  arr: string;
begin
  if not P_CheckStateParams(actor, 3) then
    exit;

  sprintf(arr, '%s[%d]', [actor.state.params.StrVal[0], actor.state.params.IntVal[1]]);
  P_SetMobjCustomParam(actor, arr, actor.state.params.IntVal[2]);
end;

//
// A_SetTics(tics: integer)
//
procedure A_SetTics(actor: Pmobj_t);
begin
  if not P_CheckStateParams(actor, 1) then
    exit;

  actor.tics := actor.state.params.IntVal[0];
end;

//
// A_DropItem(spawntype: string; amount: integer; chance: integer);
//
procedure A_DropItem(actor: Pmobj_t);
var
  mobj_no: integer;
  mo: Pmobj_t;
  propability: integer;
begin
  if not P_CheckStateParams(actor, 3) then
    exit;

  propability := actor.state.params.IntVal[0];
  if N_Random >= propability then
    exit;

  if actor.state.params.IsComputed[0] then
    mobj_no := actor.state.params.IntVal[0]
  else
  begin
    mobj_no := Info_GetMobjNumForName(actor.state.params.StrVal[0]);
    actor.state.params.IntVal[0] := mobj_no;
  end;
  if mobj_no = -1 then
  begin
    I_Warning('A_DropItem(): Unknown item %s'#13#10, [actor.state.params.StrVal[0]]);
    exit;
  end;

  mo := P_SpawnMobj(actor.x, actor.y, actor.z, mobj_no);
  {$IFNDEF HEXEN}
  mo.flags := mo.flags or MF_DROPPED; // special versions of items
  {$ENDIF}
  // JVAL Dropped items fall down to floor.
  mo.z := mo.z + 32 * FRACUNIT;
  mo.momz := 4 * FRACUNIT;
  mo.momx := 64 * N_Random;
  mo.momy := 64 * N_Random;
  mo.angle := actor.angle;
end;

procedure P_DoDamage(const mo: Pmobj_t; const damage: integer);
begin
  if damage > 0 then
    P_DamageMobj(mo, nil, nil, damage)
  else if damage < 0 then
    P_SetHealth(mo, mo.health - damage);
end;

//
// A_DamageSelf(const damage: integer);
// JVAL: incomplete
//
procedure A_DamageSelf(actor: Pmobj_t);
var
  damage: integer;
begin
  if not P_CheckStateParams(actor, 1, CSP_AT_LEAST) then
    exit;

  damage := actor.state.params.IntVal[0];
  P_DoDamage(actor, damage);
end;

//
// A_DamageTarget(const damage: integer);
// JVAL: incomplete
//
procedure A_DamageTarget(actor: Pmobj_t);
var
  damage: integer;
begin
  if not P_CheckStateParams(actor, 1, CSP_AT_LEAST) then
    exit;

  if actor.target = nil then
    exit;

  damage := actor.state.params.IntVal[0];
  P_DoDamage(actor.target, damage);
end;

//
// A_DamageTracer(const damage: integer);
// JVAL: incomplete
//
procedure A_DamageTracer(actor: Pmobj_t);
var
  damage: integer;
begin
  if not P_CheckStateParams(actor, 1, CSP_AT_LEAST) then
    exit;

  if actor.tracer = nil then
    exit;

  damage := actor.state.params.IntVal[0];
  P_DoDamage(actor.tracer, damage);
end;

//
// A_DamageMaster(const damage: integer);
//
procedure A_DamageMaster(actor: Pmobj_t);
var
  damage: integer;
begin
  if not P_CheckStateParams(actor, 1, CSP_AT_LEAST) then
    exit;

  if actor.master = nil then
    exit;

  damage := actor.state.params.IntVal[0];
  P_DoDamage(actor.master, damage);
end;

//
// A_KillTarget
// JVAL: incomplete
//
procedure A_KillTarget(actor: Pmobj_t);
begin
  if actor.target = nil then
    exit;

  P_DamageMobj(actor.target, actor, actor, actor.target.health);
end;

//
// A_KillTracer
// JVAL: incomplete
//
procedure A_KillTracer(actor: Pmobj_t);
begin
  if actor.tracer = nil then
    exit;

  P_DamageMobj(actor.tracer, actor, actor, actor.tracer.health);
end;

//
// A_KillMaster
//
procedure A_KillMaster(actor: Pmobj_t);
begin
  if actor.master = nil then
    exit;

  P_DamageMobj(actor.master, actor, actor, actor.master.health);
end;

function P_DoRemoveThing(const mo: Pmobj_t; const flags: integer): boolean;
begin
  result := true;
  if flags and RMVF_EVERYTHING <> 0 then
    P_RemoveMobj(mo)
  else if (flags and RMVF_MISC <> 0) and not (Info_IsMonster(mo._type) and (mo.flags and MF_MISSILE <> 0)) then
    P_RemoveMobj(mo)
  else if Info_IsMonster(mo._type) and (flags and RMVF_NOMONSTERS = 0) then
    P_RemoveMobj(mo)
  else if (mo.flags and MF_MISSILE <> 0) and (flags and RMVF_MISSILES <> 0) then
    P_RemoveMobj(mo)
  else
    result := false;
end;

//
// A_RemoveTarget([flags: integer]);
// JVAL: incomplete
//
procedure A_RemoveTarget(actor: Pmobj_t);
begin
  if actor.target = nil then
    exit;

  if actor.target.player <> nil then // No players
    exit;

  P_DoRemoveThing(actor.target, actor.state.params.IntVal[0]);
end;

//
// A_RemoveTracer([flags: integer]);
// JVAL: incomplete
//
procedure A_RemoveTracer(actor: Pmobj_t);
begin
  if actor.tracer = nil then
    exit;

  if actor.tracer.player <> nil then // No players
    exit;

  P_DoRemoveThing(actor.tracer, actor.state.params.IntVal[0]);
end;

//
// A_RemoveMaster([flags: integer]);
// JVAL: incomplete
//
procedure A_RemoveMaster(actor: Pmobj_t);
begin
  if actor.master = nil then
    exit;

  if actor.master.player <> nil then // No players
    exit;

  P_DoRemoveThing(actor.master, actor.state.params.IntVal[0]);
end;

//
// A_Remove(aaprt: AAPTR; [flags: integer]);
// JVAL: incomplete
//
procedure A_Remove(actor: Pmobj_t);
var
  mo: Pmobj_t;
begin
  mo := COPY_AAPTR(actor, actor.state.params.IntVal[0]);
  if mo = nil then
    exit;

  P_DoRemoveThing(mo, actor.state.params.IntVal[1]);
end;

//
// A_SetFloatBobPhase(bob: integer)
//
procedure A_SetFloatBobPhase(actor: Pmobj_t);
var
  bob: integer;
begin
  if not P_CheckStateParams(actor, 1, CSP_AT_LEAST) then
    exit;

  bob := actor.state.params.IntVal[0];
  if IsIntegerInRange(bob, 0, FLOATBOBSIZE - 1) then
    actor.bob := bob;
end;

//
// A_Detonate
// killough 8/9/98: same as A_Explode, except that the damage is variable
//
procedure A_Detonate(actor: Pmobj_t);
begin
  P_RadiusAttack(actor, actor.target, actor.info.damage{$IFDEF HEXEN}, actor.info.damage, true{$ENDIF});
end;

procedure P_SetMobjRelativeState(const mo: Pmobj_t; const offset: integer);
var
  cur: integer;
begin
  cur := (integer(mo.state) - integer(states)) div SizeOf(state_t);
  P_SetMobjState(mo, statenum_t(cur + offset));
end;

procedure P_TransferFriendliness(const src, dest: Pmobj_t);
begin
  {$IFDEF STRIFE}
  if src.flags and MF_ALLY <> 0 then
    dest.flags := dest.flags or MF_ALLY
  else
    dest.flags := dest.flags and not MF_ALLY;
  {$ENDIF}
  {$IFDEF DOOM}
  if src.flags2_ex and MF2_EX_FRIEND <> 0 then
    dest.flags2_ex := dest.flags2_ex or MF2_EX_FRIEND
  else
    dest.flags2_ex := dest.flags2_ex and not MF2_EX_FRIEND;
  {$ENDIF}
end;

//
// killough 11/98
//
// The following were inspired by Len Pitre
//
// A small set of highly-sought-after code pointers
//
procedure A_Spawn(actor: Pmobj_t);
var
  mo: Pmobj_t;
begin
  if actor.state.misc1 > 0 then
  begin
    mo := P_SpawnMobj(actor.x, actor.y, actor.state.misc2 * FRACUNIT + actor.z, actor.state.misc1 - 1);
    if mo <> nil then
      P_TransferFriendliness(actor, mo);
  end;
end;

//
// A_Face
//
procedure A_Face(actor: Pmobj_t);
begin
  actor.angle := actor.angle + actor.state.misc1 * ANG1;
end;

//
// A_Scratch
//
procedure A_Scratch(actor: Pmobj_t);
begin
  if actor.target <> nil then
  begin
    A_FaceTarget(actor);
    if P_CheckMeleeRange(actor) then
    begin
      if actor.state.misc2 > 0 then
        S_StartSound(actor, actor.state.misc2);
      P_DamageMobj(actor.target, actor, actor, actor.state.misc1);
    end;
  end;
end;

//
// PlayerToId
//
function PlayerToId(const p: Pplayer_t): integer;
var
  i: integer;
begin
  for i := 0 to MAXPLAYERS - 1 do
    if p = @players[i] then
    begin
      result := i;
      exit;
    end;

  result := -1;
end;

//
// A_RandomJump
//
// [crispy] this is pretty much the only action pointer that makes sense for both mobj and pspr states
// JVAL: modified to hold both a player_t and a mobj_t in first parameter
procedure A_RandomJump(obj: pointer; psp: Ppspdef_t);
var
  player: Pplayer_t;
  mo: Pmobj_t;
  id: integer;
begin
  if obj = nil then
    exit;

  // [crispy] first, try to apply to pspr states
  // JVAL: Check if obj is a player_t
  player := obj;
  id := PlayerToId(player);
  if (psp <> nil) and (id >= 0) then
  begin
    if N_Random < psp.state.misc2 then
      P_SetPSprite(player, pdiff(psp, @player.psprites[0], SizeOf(pspdef_t)), statenum_t(psp.state.misc1));
    exit;
  end;

  // [crispy] second, apply to mobj states
  // JVAL: Check if obj is a mobj_t
  mo := obj;
  if @mo.thinker._function.acp1 = @P_MobjThinker then
  begin
    if N_Random < mo.state.misc2 then
      P_SetMobjState(mo, statenum_t(mo.state.misc1));
  end;
end;

{$IFNDEF HEXEN}
//
// A_LineEffect
//
procedure A_LineEffect(actor: Pmobj_t);
var
  player: player_t;
  oldplayer: Pplayer_t;
  junk: line_t;
begin
  if actor.flags3_ex and MF3_EX_LINEDONE <> 0 then            // Unless already used up
    exit;

  junk := lines[0];                                           // Fake linedef set to 1st
  junk.special := actor.state.misc1;                          // Linedef type
  if junk.special <> 0 then
  begin
    oldplayer := actor.player;                                // Remember player status
    player.health := 100;                                     // Alive player
    actor.player := @player;                                  // Fake player
    junk.tag := actor.state.misc2;                            // Sector tag for linedef
    if not P_UseSpecialLine(actor, @junk, 0) then             // Try using it
      P_CrossSpecialLinePtr(@junk, 0, actor);                 // Try crossing it
    if junk.special = 0 then                                  // If type cleared,
      actor.flags3_ex := actor.flags3_ex or MF3_EX_LINEDONE;  // no more for this thing
    actor.player := oldplayer;
  end;
end;
{$ENDIF}

procedure A_FlipSprite(actor: Pmobj_t);
begin
  actor.flags3_ex := actor.flags3_ex or MF3_EX_FLIPSPRITE;
end;

procedure A_RandomFlipSprite(actor: Pmobj_t);
var
  chance: integer;
begin
  if not P_CheckStateParams(actor, 1, CSP_AT_LEAST) then
    exit;

  chance := actor.state.params.IntVal[0];
  if chance < P_Random then
    actor.flags3_ex := actor.flags3_ex or MF3_EX_FLIPSPRITE;
end;

procedure A_NoFlipSprite(actor: Pmobj_t);
begin
  actor.flags3_ex := actor.flags3_ex and not MF3_EX_FLIPSPRITE;
end;

procedure A_RandomNoFlipSprite(actor: Pmobj_t);
var
  chance: integer;
begin
  if not P_CheckStateParams(actor, 1, CSP_AT_LEAST) then
    exit;

  chance := actor.state.params.IntVal[0];
  if chance < P_Random then
    actor.flags3_ex := actor.flags3_ex and not MF3_EX_FLIPSPRITE;
end;

//
//  A_CustomMeleeAttack(damage: integer, meleesound: string, misssound: string)
//
procedure A_CustomMeleeAttack(actor: Pmobj_t);
var
  damage: integer;
  sndidx: integer;
begin
  if not P_CheckStateParams(actor, 1, CSP_AT_LEAST) then
    exit;

  if actor.target = nil then
    exit;

  A_FaceTarget(actor);
  if P_CheckMeleeRange(actor) then
  begin
    if actor.state.params.IsComputed[1] then
      sndidx := actor.state.params.IntVal[1]
    else
    begin
      sndidx := S_GetSoundNumForName(actor.state.params.StrVal[1]);
      actor.state.params.IntVal[1] := sndidx;
    end;
    S_StartSound(actor, sndidx);
    damage := actor.state.params.IntVal[0];
    P_DamageMobj(actor.target, actor, actor, damage);
  end
  else
  begin
    if actor.state.params.IsComputed[2] then
      sndidx := actor.state.params.IntVal[2]
    else
    begin
      sndidx := S_GetSoundNumForName(actor.state.params.StrVal[2]);
      actor.state.params.IntVal[2] := sndidx;
    end;
    S_StartSound(actor, sndidx);
  end;
end;

//
//  A_CustomComboAttack(missiletype: string, spawnheight: integer, damage: integer, meleesound: string)
//
procedure A_CustomComboAttack(actor: Pmobj_t);
var
  damage: integer;
  sndidx: integer;
  mobj_no: integer;
  ang: angle_t;
  x, y, z: fixed_t;
  missile: Pmobj_t;
begin
  if not P_CheckStateParams(actor, 2, CSP_AT_LEAST) then
    exit;

  if actor.target = nil then
    exit;

  A_FaceTarget(actor);
  if P_CheckMeleeRange(actor) then
  begin
    if actor.state.params.IsComputed[3] then
      sndidx := actor.state.params.IntVal[3]
    else
    begin
      sndidx := S_GetSoundNumForName(actor.state.params.StrVal[3]);
      actor.state.params.IntVal[3] := sndidx;
    end;
    S_StartSound(actor, sndidx);
    damage := actor.state.params.IntVal[2];
    P_DamageMobj(actor.target, actor, actor, damage);
  end
  else
  begin
    if actor.state.params.IsComputed[0] then
      mobj_no := actor.state.params.IntVal[0]
    else
    begin
      mobj_no := Info_GetMobjNumForName(actor.state.params.StrVal[0]);
      actor.state.params.IntVal[0] := mobj_no;
    end;
    if mobj_no = -1 then
      exit;

    ang := (actor.angle - ANG90) shr ANGLETOFINESHIFT;
    x := actor.x + 32 * finecosine[ang];
    y := actor.y + 32 * finesine[ang];
    z := actor.z + actor.state.params.FixedVal[1] - 32 * FRACUNIT;
    missile := P_SpawnMissileXYZ(x, y, z, actor, actor.target, mobj_no);
    if missile <> nil then
    begin
      if missile.flags_ex and MF_EX_SEEKERMISSILE <> 0 then
        missile.tracer := actor.target;
    end;
  end;
end;

//
//  A_SetRenderStyle(style: renderstyle_t, alpha: float)
//
procedure A_SetRenderStyle(actor: Pmobj_t);
begin
  if not P_CheckStateParams(actor, 1, CSP_AT_LEAST) then
    exit;

  actor.renderstyle := R_GetRenderstyleForName(actor.state.params.StrVal[0]);

  if actor.state.params.Count > 1 then
    actor.alpha := GetIntegerInRange(actor.state.params.FixedVal[1], 0, FRACUNIT);
end;

//
// A_FadeTo(targ: integer, ammount: integer, flags: integer)
//
procedure A_FadeTo(actor: Pmobj_t);
var
  targ: fixed_t;
  amount: fixed_t;
  flags: integer;
begin
  if not P_CheckStateParams(actor, 2, CSP_AT_LEAST) then
    exit;

  targ := actor.state.params.FixedVal[0];
  amount := actor.state.params.FixedVal[1];

  if actor.alpha > targ then
  begin
    actor.alpha := actor.alpha - amount;
    if actor.alpha < targ then
      actor.alpha := targ;
  end
  else if actor.alpha < targ then
  begin
    actor.alpha := actor.alpha + amount;
    if actor.alpha > targ then
      actor.alpha := targ;
  end;

  if actor.state.params.Count > 2 then
  begin
    if actor.state.params.BoolVal[2] then
      flags := FTF_REMOVE
    else
    begin
      flags := actor.state.params.IntVal[2];
      if flags and FTF_CLAMP <> 0 then
        actor.alpha := GetIntegerInRange(actor.alpha, 0, FRACUNIT);
    end;
    if (flags and FTF_REMOVE <> 0) and (actor.alpha = targ) then
      P_RemoveMobj(actor);
  end;
end;

//
// A_SetSize(newradius: integer, newheight: integer, testpos: boolean)
//
procedure A_SetSize(actor: Pmobj_t);
var
  newradius, newheight: fixed_t;
  oldradius, oldheight: fixed_t;
  testpos: boolean;
begin
  if not P_CheckStateParams(actor, 2, CSP_AT_LEAST) then
    exit;

  newradius := actor.state.params.IntVal[0];
  newheight := actor.state.params.IntVal[1];
  oldradius := actor.radius;
  oldheight := actor.height;
  if newradius >= 0 then
    actor.radius := newradius;
  if newheight >= 0 then
    actor.height := newheight;
  testpos := actor.state.params.BoolVal[2];
  if testpos then
    if not P_TestMobjLocation(actor) then
    begin
      actor.radius := oldradius;
      actor.height := oldheight;
    end;
end;

procedure P_CopyFriendliness(const originator, mo: Pmobj_t);
begin
  if Info_IsMonster(originator._type) then
  begin
    // If this is a monster transfer all friendliness information
    {$IFDEF STRIFE}
    mo.flags := (mo.flags and not MF_ALLY) or (originator.flags and MF_ALLY);
    {$ENDIF}
    {$IFDEF DOOM}
    mo.flags2_ex := (mo.flags2_ex and not MF2_EX_FRIEND) or (originator.flags2_ex and MF2_EX_FRIEND);
    {$ENDIF}
  end
  else if originator.player <> nil then
  begin
    // A player always spawns a monster friendly to him
    {$IFDEF STRIFE}
    mo.flags := mo.flags or MF_ALLY;
    {$ENDIF}
    {$IFDEF DOOM}
    mo.flags2_ex := mo.flags2_ex or MF2_EX_FRIEND;
    {$ENDIF}
  end;
end;

function P_RaiseActor(const thing, raiser: Pmobj_t): boolean;
var
  info: Pmobjinfo_t;
  oldheight: fixed_t;
  oldradius: fixed_t;
begin
  if thing = nil then
  begin
    result := false; // not a monster
    exit;
  end;

  if thing.flags and MF_CORPSE = 0 then
  begin
    result := false; // not a monster
    exit;
  end;

  if thing.info.raisestate = Ord(S_NULL) then
  begin
    result := false;
    exit;
  end;

  info := thing.info;
  thing.momx := 0;
  thing.momy := 0;

  // JVAL: Mass, gravity, pushfactor ??
  oldheight := thing.height;
  oldradius := thing.radius;

  thing.height := info.height;
  thing.radius := info.radius;

  if not P_CheckPosition(thing, thing.x, thing.y) then
  begin
    thing.height := oldheight;
    thing.radius := oldradius;
    result := false;
    exit;
  end;

  {$IFDEF DOOM_OR_STRIFE}
  S_StartSound(thing, Ord(sfx_slop));
  {$ENDIF}
  {$IFDEF HERETIC}
  S_StartSound(thing, Ord(sfx_respawn));
  {$ENDIF}
  {$IFDEF HEXEN}
  S_StartSound(thing, Ord(SFX_RESPAWN));
  {$ENDIF}

  P_SetMobjState(thing, statenum_t(info.raisestate));

  thing.flags := info.flags;
  {$IFDEF HERETIC_OR_HEXEN}
  thing.flags2 := info.flags2;
  {$ENDIF}
  thing.flags_ex := info.flags_ex;
  thing.flags2_ex := info.flags2_ex;
  thing.flags3_ex := info.flags3_ex;
  thing.flags4_ex := info.flags4_ex;

  if raiser <> nil then
    P_CopyFriendliness(raiser, thing);

  thing.health := info.spawnhealth;
  thing.target := nil;

  result := true;
end;

//
// A_RaiseMaster(copyfriendliness: boolean)
//
procedure A_RaiseMaster(actor: Pmobj_t);
var
  copy: boolean;
begin
  if actor.master = nil then
    exit;

  if actor.state.params <> nil then
    copy := actor.state.params.BoolVal[0] or (actor.state.params.IntVal[0] = 1)
  else
    copy := true;

  if copy then
    P_RaiseActor(actor.master, actor)
  else
    P_RaiseActor(actor.master, nil);
end;

//
// A_RaiseChildren(copyfriendliness: boolean)
//
procedure A_RaiseChildren(actor: Pmobj_t);
var
  copy: boolean;
  think: Pthinker_t;
  mo, friend: Pmobj_t;
begin
  if actor.state.params <> nil then
    copy := actor.state.params.BoolVal[0] or (actor.state.params.IntVal[0] = 1)
  else
    copy := true;

  if copy then
    friend := actor
  else
    friend := nil;

  think := thinkercap.next;
  while think <> @thinkercap do
  begin
    if @think._function.acp1 = @P_MobjThinker then
    begin
      mo := Pmobj_t(think);
      if mo.master = actor then
        P_RaiseActor(Pmobj_t(think), friend);
    end;
    think := think.next;
  end;
end;

//
// A_RaiseSiblings(copyfriendliness: boolean)
//
procedure A_RaiseSiblings(actor: Pmobj_t);
var
  copy: boolean;
  think: Pthinker_t;
  mo, friend: Pmobj_t;
begin
  if actor.master = nil then
    exit;

  if actor.state.params <> nil then
    copy := actor.state.params.BoolVal[0] or (actor.state.params.IntVal[0] = 1)
  else
    copy := true;

  if copy then
    friend := actor
  else
    friend := nil;

  think := thinkercap.next;
  while think <> @thinkercap do
  begin
    if @think._function.acp1 = @P_MobjThinker then
    begin
      mo := Pmobj_t(think);
      if mo <> actor then
        if mo.master = actor.master then
          P_RaiseActor(Pmobj_t(think), friend);
    end;
    think := think.next;
  end;
end;

//
// A_HealThing(amount: integer, max: integer)
//
procedure A_HealThing(actor: Pmobj_t);
var
  ammount: integer;
  mx: integer;
  p: Pplayer_t;
begin
  if not P_CheckStateParams(actor, 2, CSP_AT_LEAST) then
    exit;

  ammount := actor.state.params.IntVal[0];
  mx := actor.state.params.IntVal[1];

  if mx > actor.info.spawnhealth then
    mx := actor.info.spawnhealth;

  p := actor.player;

  if (mx = 0) or (actor.player = nil) then
  begin
    actor.health := actor.health + ammount;

    if actor.health > actor.info.spawnhealth then
      actor.health := actor.info.spawnhealth;
    if p <> nil then
      p.health := actor.health;
    exit;
  end
  else if mx = 1 then
    mx := {$IFDEF DOOM}p_soulspherehealth{$ELSE}100{$ENDIF};

  if actor.health < mx then
  begin
    actor.health := actor.health + ammount;
    if (actor.health > mx) and (mx > 0) then
      actor.health := mx;
    if p <> nil then
      p.health := actor.health;
  end;
end;

procedure P_DoAttack(const actor: Pmobj_t; const domelee, domissile: boolean;
  const meleedamage: integer; const meleesound: integer; const missiletype: integer;
  const missileheight: integer);
var
  damage: integer;
begin
  if actor.target = nil then
    exit;

  A_FaceTarget(actor);
  if domelee and (meleedamage > 0) and P_CheckMeleeRange(actor) then
  begin
    damage := (P_Random mod 8 + 1) * meleedamage;
    if meleesound > 0 then
      S_StartSound(actor, meleesound);
    P_DamageMobj(actor.target, actor, actor, damage);
  end
  else if domissile and (missiletype <> 0) then
    P_CustomMissile(actor, missiletype, missileheight, 0, 0, 0, 0);
end;

//
// A_BasicAttack(MeleeDamage: integer, MeleeSound: integer, MissileType: integer, MissileHeight: float)
//
procedure A_BasicAttack(actor: Pmobj_t);
var
  MeleeDamage: integer;
  MeleeSound: integer;
  MissileType: integer;
  MissileHeight: fixed_t;
begin
  if not P_CheckStateParams(actor, 4) then
    exit;

  MeleeDamage := actor.state.params.IntVal[0];

  if actor.state.params.IsComputed[1] then
    MeleeSound := actor.state.params.IntVal[0]
  else
  begin
    MeleeSound := S_GetSoundNumForName(actor.state.params.StrVal[1]);
    actor.state.params.IntVal[1] := MeleeSound;
  end;

  if actor.state.params.IsComputed[2] then
    MissileType := actor.state.params.IntVal[2]
  else
  begin
    MissileType := Info_GetMobjNumForName(actor.state.params.StrVal[2]);
    actor.state.params.IntVal[2] := MissileType;
  end;

  MissileHeight := actor.state.params.FixedVal[3];

  P_Doattack(actor, MeleeDamage <> 0, MissileType <> -1, MeleeDamage, MeleeSound,
    MissileType, MissileHeight);
end;

function P_FloatSpeed(const actor: Pmobj_t): fixed_t;
begin
  if actor.info.floatspeed > 0 then
  begin
    result := actor.info.floatspeed;
    if result < 256 then
      result := result * FRACUNIT;
  end
  else
    result := FLOATSPEED;
end;

const
  TRACEANGLE2 = $c000000;

//
// A_Tracer2
//
procedure A_Tracer2(actor: Pmobj_t);
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
    if exact - actor.angle > ANG180 then
    begin
      actor.angle := actor.angle - TRACEANGLE2;
      if exact - actor.angle < ANG180 then
        actor.angle := exact;
    end
    else
    begin
      actor.angle := actor.angle + TRACEANGLE2;
      if exact - actor.angle > ANG180 then
        actor.angle := exact;
    end;
  end;

  exact := actor.angle shr ANGLETOFINESHIFT;
  actor.momx := FixedMul(actor.info.speed, finecosine[exact]);
  actor.momy := FixedMul(actor.info.speed, finesine[exact]);

  if actor.flags_ex and (MF_EX_FLOORHUGGER or MF_EX_CEILINGHUGGER) = 0 then
  begin
    // change slope
    dist := P_AproxDistance(dest.x - actor.x, dest.y - actor.y);

    dist := dist div actor.info.speed;

    if dist < 1 then
      dist := 1;
    if dest.height >= 56 * FRACUNIT then
      slope := (dest.z + 40 * FRACUNIT - actor.z) div dist
    else
      slope := (dest.z + actor.height * 2 div 3 - actor.z) div dist;

    if slope < actor.momz then
      actor.momz := actor.momz - FRACUNIT div 8
    else
      actor.momz := actor.momz + FRACUNIT div 8;
  end;
end;

//
// A_MonsterRefire(prob: integer, offset: state_t)
//
procedure A_MonsterRefire(actor: Pmobj_t);
var
  prob: integer;
  offset: integer;
begin
  if not P_CheckStateParams(actor, 2) then
    exit;

  A_FaceTarget(actor);

  prob := actor.state.params.IntVal[0];
  if N_Random < prob then
    exit;

  if (actor.target = nil) or (actor.target.health <= 0) or not P_CheckSight(actor, actor.target) then
  begin
    offset := P_GetStateFromNameWithOffsetCheck(actor, actor.state.params.StrVal[1]);
    if @states[offset] <> actor.state then
      P_SetMobjState(actor, statenum_t(offset));
  end;
end;

//
// A_RearrangePointers(ptr_target: integer, ptr_master: integer, ptr_tracer: integer, flags: integer)
//
procedure A_RearrangePointers(actor: Pmobj_t);
var
  gettarget: Pmobj_t;
  getmaster: Pmobj_t;
  gettracer: Pmobj_t;
  ptr_target: integer;
  ptr_master: integer;
  ptr_tracer: integer;
  flags: integer;
begin
  if not P_CheckStateParams(actor, 3, CSP_AT_LEAST) then
    exit;

  gettarget := actor.target;
  getmaster := actor.master;
  gettracer := actor.tracer;

  ptr_target := actor.state.params.IntVal[0];
  ptr_master := actor.state.params.IntVal[1];
  ptr_tracer := actor.state.params.IntVal[2];
  flags := actor.state.params.IntVal[3];

  case ptr_target of // pick the new target
  AAPTR_MASTER:
    begin
      actor.target := getmaster;
      if flags and PTROP_UNSAFETARGET = 0 then
        VerifyTargetChain(actor);
    end;
  AAPTR_TRACER:
    begin
      actor.target := gettracer;
      if (flags and PTROP_UNSAFETARGET = 0) then
        VerifyTargetChain(actor);
    end;
  AAPTR_NULL:
    actor.target := nil;
    // THIS IS NOT "A_ClearTarget", so no other targeting info is removed
  end;

  // presently permitting non-monsters to set master
  case ptr_master of // pick the new master
  AAPTR_TARGET:
    begin
      actor.master := gettarget;
      if (flags and PTROP_UNSAFEMASTER = 0) then
        VerifyMasterChain(actor);
    end;
  AAPTR_TRACER:
    begin
      actor.master := gettracer;
      if (flags and PTROP_UNSAFEMASTER = 0) then
        VerifyMasterChain(actor);
    end;
  AAPTR_NULL:
    actor.master := nil;
  end;

  case ptr_tracer of // pick the new tracer
  AAPTR_TARGET:
    begin
      actor.tracer := gettarget;
    end; // no verification deemed necessary; the engine never follows a tracer chain(?)
  AAPTR_MASTER:
    begin
      actor.tracer := getmaster;
    end; // no verification deemed necessary; the engine never follows a tracer chain(?)
  AAPTR_NULL:
    actor.tracer := nil;
  end;
end;

//
// A_TransferPointer(ptr_source: integer, ptr_recipient: integer, ptr_sourcefield: integer, [ptr_recipientfield: integer], [flags: integer])
//
procedure A_TransferPointer(actor: Pmobj_t);
var
  source, recipient: Pmobj_t;
  ptr_source: integer;
  ptr_recipient: integer;
  ptr_sourcefield: integer;
  ptr_recipientfield: integer;
  flags: integer;
begin
  if not P_CheckStateParams(actor, 3, CSP_AT_LEAST) then
    exit;

  ptr_source := actor.state.params.IntVal[0];
  ptr_recipient := actor.state.params.IntVal[1];

  // Exchange pointers with actors to whom you have pointers (or with yourself, if you must)
  source := COPY_AAPTR(actor, ptr_source);
  recipient := COPY_AAPTR(actor, ptr_recipient); // pick an actor to store the provided pointer value
  if recipient = nil then
    exit;

  ptr_sourcefield := actor.state.params.IntVal[2];
  // convert source from dataprovider to data
  source := COPY_AAPTR(source, ptr_sourcefield);

  if source = recipient then
    source := nil;  // The recipient should not acquire a pointer to itself; will write nil

  ptr_recipientfield := actor.state.params.IntVal[3];
  if ptr_recipientfield = AAPTR_DEFAULT then
    ptr_recipientfield := ptr_sourcefield;  // If default: Write to same field as data was read from

  flags := actor.state.params.IntVal[4];
  ASSIGN_AAPTR(recipient, ptr_recipientfield, source, flags);
end;

var
  soundtargetex: Pmobj_t;

procedure P_RecursiveSoundEx(sec: Psector_t; soundblocks: integer; maxdist: fixed_t);
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

  if P_AproxDistance(soundtargetex.x - sec.soundorg.x, soundtargetex.y - sec.soundorg.y) > maxdist then
    exit;

  sec.soundtarget := soundtargetex;

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
        P_RecursiveSoundEx(other, 1, maxdist);
    end
    else
      P_RecursiveSoundEx(other, soundblocks, maxdist);
  end;
end;

procedure P_NoiseAlertEx(target: Pmobj_t; emmiter: Pmobj_t; const maxdist: fixed_t);
begin
  soundtargetex := target;
  inc(validcount);
  P_RecursiveSoundEx(Psubsector_t(emmiter.subsector).sector, 0, maxdist);
end;

//
// A_AlertMonsters(maxdist: integer, flags: integer)
//
procedure A_AlertMonsters(actor: Pmobj_t);
var
  target, emitter: Pmobj_t;
  maxdist: fixed_t;
  flags: integer;
begin
  if not P_CheckStateParams(actor, 3, CSP_AT_LEAST) then
    exit;

  target := nil;
  emitter := actor;

  maxdist := actor.state.params.FixedVal[0];
  flags := actor.state.params.IntVal[1];

  if (actor.player <> nil) or (flags and AMF_TARGETEMITTER <> 0) then
  begin
    target := actor;
  end
  else if (actor.target <> nil) and (flags and AMF_TARGETNONPLAYER <> 0) then
  begin
    target := actor.target;
  end
  else if (actor.target <> nil) and (actor.target.player <> nil) then
  begin
    target := actor.target;
  end;

  if flags and AMF_EMITFROMTARGET <> 0 then
    emitter := target;

  if (target <> nil) and (emitter <> nil) then
    P_NoiseAlertEx(target, emitter, maxdist);
end;

procedure P_LocalEarthQuake(const actor: Pmobj_t; const tics: integer; const intensity: fixed_t; const maxdist: fixed_t);
var
  i: integer;
  dist: fixed_t;
  frac: fixed_t;
  testintensity: fixed_t;
begin
  for i := 0 to MAXPLAYERS - 1 do
    if playeringame[i] then
    begin
      dist := P_AproxDistance(actor.x - players[i].mo.x, actor.y - players[i].mo.y);
      dist := P_AproxDistance(actor.z - players[i].mo.z, dist); // 3d distance
      if dist <= maxdist then
      begin
        if players[i].quaketics < tics then
          players[i].quaketics := tics;
        frac := FixedDiv(dist, maxdist) * (FINEANGLES div 4);
        testintensity := FixedMul(finecosine[frac shr ANGLETOFINESHIFT], intensity); // JVAL: 20200508 - Curved
        if players[i].quakeintensity < testintensity then
          players[i].quakeintensity := testintensity;
      end;
    end;
end;

//
// A_LocalEarthQuake(tics: integer; [intensity: float = 1.0]; [maxdist: float = MAXINT]);
//
procedure A_LocalEarthQuake(actor: Pmobj_t);
var
  tics: integer;
  intensity: integer;
  maxdist: fixed_t;
begin
  if not P_CheckStateParams(actor, 1, CSP_AT_LEAST) then
    exit;

  tics := actor.state.params.FixedVal[0];
  if actor.state.params.Count > 1 then
    intensity := actor.state.params.FixedVal[1]
  else
    intensity := FRACUNIT;
  if actor.state.params.Count > 2 then
    maxdist := actor.state.params.FixedVal[2]
  else
    maxdist := MAXINT;
  P_LocalEarthQuake(actor, tics, intensity, maxdist);
end;

//
// A_RemoveChildren([flags: integer]);
// JVAL: incomplete
//
procedure A_RemoveChildren(actor: Pmobj_t);
var
  flags: integer;
  think: Pthinker_t;
  mo: Pmobj_t;
begin
  if actor.state.params <> nil then
    flags := actor.state.params.IntVal[0]
  else
    flags := 0;

  think := thinkercap.next;
  while think <> @thinkercap do
  begin
    if @think._function.acp1 = @P_MobjThinker then
    begin
      mo := Pmobj_t(think);
      if mo.master = actor then
        P_DoRemoveThing(mo, flags);
    end;
    think := think.next;
  end;
end;

//
// A_RemoveSiblings([flags: integer]);
// JVAL: incomplete
//
procedure A_RemoveSiblings(actor: Pmobj_t);
var
  flags: integer;
  think: Pthinker_t;
  mo: Pmobj_t;
begin
  if actor.master = nil then
    exit;

  if actor.state.params <> nil then
    flags := actor.state.params.IntVal[0]
  else
    flags := 0;

  think := thinkercap.next;
  while think <> @thinkercap do
  begin
    if @think._function.acp1 = @P_MobjThinker then
    begin
      mo := Pmobj_t(think);
      if mo <> actor then
        if mo.master = actor.master then
          P_DoRemoveThing(mo, flags);
    end;
    think := think.next;
  end;
end;

//
// A_KillChildren
// JVAL: incomplete
//
procedure A_KillChildren(actor: Pmobj_t);
var
  think: Pthinker_t;
  mo: Pmobj_t;
begin
  think := thinkercap.next;
  while think <> @thinkercap do
  begin
    if @think._function.acp1 = @P_MobjThinker then
    begin
      mo := Pmobj_t(think);
      if mo.master = actor then
        P_DamageMobj(mo, actor, actor, mo.health);
    end;
    think := think.next;
  end;
end;

//
// A_KillSiblings
// JVAL: incomplete
//
procedure A_KillSiblings(actor: Pmobj_t);
var
  think: Pthinker_t;
  mo: Pmobj_t;
begin
  if actor.master = nil then
    exit;

  think := thinkercap.next;
  while think <> @thinkercap do
  begin
    if @think._function.acp1 = @P_MobjThinker then
    begin
      mo := Pmobj_t(think);
      if mo <> actor then
        if mo.master = actor.master then
          P_DamageMobj(mo, actor, actor, mo.health);
    end;
    think := think.next;
  end;
end;

//
// A_Weave(xyspeed: integer, zspeed: integer, xydist: float, zdist: float)
//
procedure A_Weave(actor: Pmobj_t);
var
  xyspeed: integer;
  zspeed: integer;
  xydist: fixed_t;
  zdist: fixed_t;
  newX, newY: fixed_t;
  weaveXY, weaveZ: integer;
  angle: angle_t;
  dist: fixed_t;
begin
  xyspeed := 2;
  zspeed := 2;
  xydist := 2 * FRACUNIT;
  zdist := FRACUNIT;
  if actor.state.params <> nil then
  begin
    if actor.state.params.Count >= 1 then
    begin
      xyspeed := actor.state.params.IntVal[0];
      if actor.state.params.Count >= 2 then
      begin
        zspeed := actor.state.params.IntVal[1];
        if actor.state.params.Count >= 3 then
        begin
          xydist := actor.state.params.FixedVal[2];
          if actor.state.params.Count >= 4 then
          begin
            zdist := actor.state.params.FixedVal[3];
          end;
        end;
      end;
    end;
  end;

  weaveXY := actor.WeaveIndexXY;
  weaveZ := actor.WeaveIndexZ;
  angle := (actor.angle + ANG90) shr ANGLETOFINESHIFT;

  dist := FixedMul(FloatBobOffsets[weaveXY], xydist);
  newX := actor.x - FixedMul(finecosine[angle], dist);
  dist := FixedMul(FloatBobOffsets[weaveZ], zdist);
  newY := actor.y - FixedMul(finesine[angle], dist);
  weaveXY := (weaveXY + xyspeed) and FLOATBOBMASK;
  dist := FixedMul(FloatBobOffsets[weaveXY], xydist);
  newX := newX - FixedMul(finecosine[angle], dist);
  dist := FixedMul(FloatBobOffsets[weaveZ], zdist);
  newY := newY - FixedMul(finesine[angle], dist);
  P_TryMove(actor, newX, newY);
  dist := FixedMul(FloatBobOffsets[weaveXY], zdist);
  actor.z := actor.z - dist;
  weaveZ := (weaveZ + zspeed) and FLOATBOBMASK;
  actor.z := actor.z + FloatBobOffsets[weaveZ];

  actor.WeaveIndexXY := weaveXY;
  actor.WeaveIndexZ := weaveZ;
end;

//
// A_SetWeaveIndexXY(weavexy: integer)
//
procedure A_SetWeaveIndexXY(actor: Pmobj_t);
begin
  if not P_CheckStateParams(actor, 1) then
    exit;

  actor.WeaveIndexXY := actor.state.params.IntVal[0] and FLOATBOBMASK;
end;

//
// A_SetWeaveIndexZ(weavez: integer)
//
procedure A_SetWeaveIndexZ(actor: Pmobj_t);
begin
  if not P_CheckStateParams(actor, 1) then
    exit;

  actor.WeaveIndexZ := actor.state.params.IntVal[0] and FLOATBOBMASK;
end;

//
// A_SetWeaveIndexes(weavexy: integer, weavez: integer)
//
procedure A_SetWeaveIndexes(actor: Pmobj_t);
begin
  if not P_CheckStateParams(actor, 2) then
    exit;

  actor.WeaveIndexXY := actor.state.params.IntVal[0] and FLOATBOBMASK;
  actor.WeaveIndexZ := actor.state.params.IntVal[1] and FLOATBOBMASK;
end;

procedure A_SetSpriteDX(actor: Pmobj_t);
begin
  if not P_CheckStateParams(actor, 1) then
    exit;

  actor.spriteDX := actor.state.params.FixedVal[0];
end;

procedure A_SetSpriteDY(actor: Pmobj_t);
begin
  if not P_CheckStateParams(actor, 1) then
    exit;

  actor.spriteDY := actor.state.params.FixedVal[0];
end;

//
// A_SetHeight(newheight: float)
//
procedure A_SetHeight(actor: Pmobj_t);
begin
  if not P_CheckStateParams(actor, 1) then
    exit;

  actor.height := actor.state.params.FixedVal[0];
end;

//
// A_SetFriction(newfriction: float)
//
procedure A_SetFriction(actor: Pmobj_t);
begin
  if not P_CheckStateParams(actor, 1) then
    exit;

  actor.friction := actor.state.params.FixedVal[0];
end;

//
//  A_PlayerHurtExplode(damage: integer; radius: integer);
//
procedure A_PlayerHurtExplode(actor: Pmobj_t);
var
  damage: integer;
  radius: fixed_t;
begin
  if not P_CheckStateParams(actor, 2, CSP_AT_LEAST) then
    exit;

  damage := actor.state.params.IntVal[0];
  radius := actor.state.params.IntVal[1];
  P_RadiusAttackPlayer(actor, actor.target, damage, radius);

  if actor.z <= actor.floorz then
    P_HitFloor(actor);
end;

//
//  A_SetPainChance(value: integer);
//
procedure A_SetPainChance(actor: Pmobj_t);
begin
  if not P_CheckStateParams(actor, 1, CSP_AT_LEAST) then
    exit;

  actor.painchance := actor.state.params.IntVal[0];
end;

//
// A_SetPushable
//
procedure A_SetPushable(actor: Pmobj_t);
begin
  {$IFDEF DOOM_OR_STRIFE}
  actor.flags2_ex := actor.flags2_ex or MF2_EX_PUSHABLE;
  {$ENDIF}
  {$IFDEF HERETIC_OR_HEXEN}
  actor.flags2 := actor.flags2 or MF2_PUSHABLE;
  {$ENDIF}
end;

//
// A_SetPushable
//
procedure A_UnSetPushable(actor: Pmobj_t);
begin
  {$IFDEF DOOM_OR_STRIFE}
  actor.flags2_ex := actor.flags2_ex and not MF2_EX_PUSHABLE;
  {$ENDIF}
  {$IFDEF HERETIC_OR_HEXEN}
  actor.flags2 := actor.flags2 and not MF2_PUSHABLE;
  {$ENDIF}
end;

//  A_MatchTargetZ(const zspeed, threshold, [maxmomz])
procedure A_MatchTargetZ(actor: Pmobj_t);
var
  speed: fixed_t;
  threshold: fixed_t;
  maxmomz: fixed_t;
begin
  if actor.target = nil then
    exit;

  if actor.state.params = nil then
  begin
    speed := FRACUNIT;
    threshold := FRACUNIT;
    maxmomz := actor.info.speed;
  end
  else
  begin
    if actor.state.params.Count > 0 then
    begin
      speed := actor.state.params.FixedVal[0];
      if speed = 0 then
        exit;
    end
    else
      speed := FRACUNIT;

    if actor.state.params.Count > 1 then
      threshold := actor.state.params.FixedVal[1]
    else
      threshold := FRACUNIT;

    if actor.state.params.Count > 2 then
      maxmomz := actor.state.params.FixedVal[2]
    else
      maxmomz := actor.info.speed;
  end;

  if maxmomz < 256 then
    maxmomz := maxmomz * FRACUNIT;

  if actor.z + actor.momz < actor.target.z - threshold then
  begin
    actor.momz := actor.momz + speed;
    if actor.momz > maxmomz then
      actor.momz := maxmomz;
    if actor.momz < 0 then
      actor.momz := 0;
  end
  else if actor.z + actor.momz > actor.target.z + threshold then
  begin
    actor.momz := actor.momz - speed;
    if actor.momz < -maxmomz then
      actor.momz := -maxmomz;
    if actor.momz > 0 then
      actor.momz := 0;
  end
  else
  begin
    actor.momz := actor.momz * 15 div 16;
    if actor.momz > maxmomz then
      actor.momz := maxmomz
    else if actor.momz < -maxmomz then
      actor.momz := -maxmomz;
  end;

  // JVAL: 20200421 - Do not slam to floor - ceiling
  if actor.z + actor.momz + actor.height >= actor.ceilingz then
    actor.momz := (actor.ceilingz - actor.z - actor.height) div 2
  else if actor.z + actor.momz <= actor.floorz then
    actor.momz := actor.floorz - actor.z;
end;

procedure A_SetInteractive(actor: Pmobj_t);
begin
  actor.flags2_ex := actor.flags2_ex or MF2_EX_INTERACTIVE;
end;

procedure A_UnSetInteractive(actor: Pmobj_t);
begin
  actor.flags2_ex := actor.flags2_ex and not MF2_EX_INTERACTIVE;
end;

end.

