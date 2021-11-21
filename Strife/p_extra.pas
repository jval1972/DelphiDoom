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
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit p_extra;

//
// JVAL
// Extra mobj functions

interface

uses
  m_fixed,
  p_mobj_h;

procedure A_LowGravity(actor: Pmobj_t);

procedure A_NoGravity(actor: Pmobj_t);

procedure A_Gravity(actor: Pmobj_t);

procedure A_NoBlocking(actor: Pmobj_t);

procedure A_MeleeAttack(actor: Pmobj_t);

procedure A_Die(actor: Pmobj_t);

procedure A_CustomBulletAttack(actor: Pmobj_t);

procedure A_Countdown(actor: Pmobj_t);

procedure A_FastChase(actor: Pmobj_t);

procedure A_SetInvulnerable(actor: Pmobj_t);

procedure A_UnSetInvulnerable(actor: Pmobj_t);

procedure A_FloatBob(actor: Pmobj_t);

procedure A_NoFloatBob(actor: Pmobj_t);

procedure A_ComboAttack(actor: Pmobj_t);

procedure A_MediumGravity(actor: Pmobj_t);

procedure A_HideThing(actor: Pmobj_t);

procedure A_UnHideThing(actor: Pmobj_t);

procedure A_SpawnDebris(actor: Pmobj_t);

procedure A_SpawnSmokeUp(actor: Pmobj_t);

procedure A_SpawnSmokeDown(actor: Pmobj_t);

procedure A_SpawnSmokeHorz(actor: Pmobj_t);

procedure A_SetInteractive(actor: Pmobj_t);

procedure A_UnSetInteractive(actor: Pmobj_t);

procedure A_SetMonsterInfight(actor: Pmobj_t);

procedure A_UnSetMonsterInfight(actor: Pmobj_t);

procedure A_NoiseAlert(actor: Pmobj_t);

procedure A_SetShootable(actor: Pmobj_t);

procedure A_UnSetShootable(actor: Pmobj_t);

procedure A_PlayerMessage(actor: Pmobj_t);

procedure A_PlayerFaceMe(actor: Pmobj_t);

procedure A_SetFloorClip(actor: Pmobj_t);

procedure A_UnSetFloorClip(actor: Pmobj_t);


implementation

uses
  d_delphi,
  doomdef,
  d_player,
  g_game,
  i_system,
  info_h,
  info,
  info_common,
  r_defs,
  m_rnd,
  m_vectors,
  p_enemy,
  p_mobj,
  p_inter,
  p_user,
  p_map,
  p_maputl,
  p_local,
  p_pspr,
  p_sounds,
  p_terrain,
  sounds,
  s_sound,
  sc_states,
  p_common,
  tables;

//
// JVAL
// Low gravity
//
procedure A_LowGravity(actor: Pmobj_t);
begin
  actor.flags := actor.flags and not MF_NOGRAVITY;
  actor.flags_ex := actor.flags_ex or MF_EX_LOWGRAVITY;
  actor.flags2_ex := actor.flags2_ex and not MF2_EX_MEDIUMGRAVITY;
end;

//
// JVAL
// Remove gravity
//
procedure A_NoGravity(actor: Pmobj_t);
begin
  actor.flags := actor.flags or MF_NOGRAVITY;
  actor.flags_ex := actor.flags_ex and not MF_EX_LOWGRAVITY;
  actor.flags2_ex := actor.flags2_ex and not MF2_EX_MEDIUMGRAVITY;
end;

//
// JVAL
// Normal gravity
//
procedure A_Gravity(actor: Pmobj_t);
begin
  actor.flags := actor.flags and not MF_NOGRAVITY;
  actor.flags_ex := actor.flags_ex and not MF_EX_LOWGRAVITY;
  actor.flags2_ex := actor.flags2_ex and not MF2_EX_MEDIUMGRAVITY;
end;

//
// JVAL
// Remove blocking flag
//
procedure A_NoBlocking(actor: Pmobj_t);
begin
  actor.flags := actor.flags and not MF_SOLID;
end;

//
// JVAL
// Close distance attack
// A_MeleeAttack(mindamage=0; maxdamage=0);
//
procedure A_MeleeAttack(actor: Pmobj_t);
var
  dmin, dmax: integer;  // Minimum and maximum damage
  damage: integer;
begin
  if actor.target = nil then
    exit;

  A_FaceTarget(actor);
  if P_CheckMeleeRange(actor) then
  begin
    A_MeleeSound(actor, actor);
    if actor.state.params = nil then
      damage := actor.info.meleedamage
    else
    begin
      if actor.state.params.Count = 1 then
      begin
        dmin := actor.state.params.IntVal[0];
        dmax := dmin;
      end
      else
      begin
        dmin := actor.state.params.IntVal[0];
        dmax := actor.state.params.IntVal[1];
      end;
      if dmax < dmin then
      begin
        damage := dmax;
        dmax := dmin;
        dmin := damage;
      end;
      damage := dmin + N_Random mod (dmax - dmin + 1);
    end;
    P_DamageMobj(actor.target, actor, actor, damage);
  end;
end;

procedure A_Die(actor: Pmobj_t);
begin
  actor.flags_ex := actor.flags_ex and not MF_EX_INVULNERABLE;  // Clear invulnerability flag
  actor.flags2_ex := actor.flags2_ex and not MF2_EX_NODAMAGE;   // Clear no damage flag
  if G_PlayingEngineVersion > VERSION121 then
    P_DamageMobj(actor, nil, nil, 10000)
  else
    P_DamageMobj(actor, nil, nil, actor.health);
end;

//
// CustomBulletAttack(spread_xy, numbullets, damageperbullet, range)
//
procedure A_CustomBulletAttack(actor: Pmobj_t);
var
  spread_xy: angle_t;
  numbullets: integer;
  damageperbullet: integer;
  range: fixed_t;
  i: integer;
  angle, bangle: angle_t;
  slope: fixed_t;
  rnd: byte;
begin
  if not P_CheckStateParams(actor) then
    exit;

  if actor.target = nil then
    exit;

  spread_xy := round(actor.state.params.FloatVal[0] * ANG1);
  numbullets := actor.state.params.IntVal[1];
  damageperbullet := actor.state.params.IntVal[2];
  range := actor.state.params.FixedVal[3];

  if range <= 0 then
    range := MISSILERANGE;

  A_FaceTarget(actor);
  bangle := actor.angle;

  slope := P_AimLineAttack(actor, bangle, range);

  A_AttackSound1(actor);

  spread_xy := spread_xy div 256;
  for i := 0 to numbullets - 1 do
  begin
    rnd := N_Random;
    angle := bangle + 128 * spread_xy - rnd * spread_xy;
    P_LineAttack(actor, angle, range, slope, damageperbullet);
  end;
end;

//
// A_Countdown(void)
//
procedure A_Countdown(actor: Pmobj_t);
begin
  dec(actor.reactiontime);
  if actor.reactiontime <= 0 then
  begin
    P_ExplodeMissile(actor);
  end;
end;

procedure A_FastChase(actor: Pmobj_t);
begin
  P_DoChase(actor, true);
end;

procedure A_SetInvulnerable(actor: Pmobj_t);
begin
  actor.flags_ex := actor.flags_ex or MF_EX_INVULNERABLE;
end;

procedure A_UnSetInvulnerable(actor: Pmobj_t);
begin
  actor.flags_ex := actor.flags_ex and not MF_EX_INVULNERABLE;
end;

procedure A_FloatBob(actor: Pmobj_t);
begin
  actor.flags_ex := actor.flags_ex or MF_EX_FLOATBOB;
end;

procedure A_NoFloatBob(actor: Pmobj_t);
begin
  actor.flags_ex := actor.flags_ex and not MF_EX_FLOATBOB;
end;

procedure A_Missile(actor: Pmobj_t);
begin
  actor.flags := actor.flags or MF_MISSILE;
end;

procedure A_NoMissile(actor: Pmobj_t);
begin
  actor.flags := actor.flags and not MF_MISSILE;
end;

//
// A_ComboAttack(void)
//
procedure A_ComboAttack(actor: Pmobj_t);
var
  missile: Pmobj_t;
  mobj_no: integer;
begin
  A_FaceTarget(actor);

  if P_CheckMeleeRange(actor) then
  begin
    A_MeleeSound(actor, actor);
    P_DamageMobj(actor.target, actor, actor, actor.info.meleedamage);
  end
  else
  begin
    mobj_no := actor.info.missiletype;

    if mobj_no <= 0 then
      exit;

    if mobjinfo[mobj_no].speed < 256 then
      mobjinfo[mobj_no].speed := mobjinfo[mobj_no].speed * FRACUNIT;  // JVAL fix me!!!

    actor.z := actor.z;
    missile := P_SpawnMissile(actor, actor.target, mobj_no);
    actor.z := actor.z;

    if missile <> nil then
    begin
      if missile.info.flags_ex and MF_EX_SEEKERMISSILE <> 0 then
        missile.tracer := actor.target;
    end;
  end;
end;

//
// JVAL
// Medium gravity
//
procedure A_MediumGravity(actor: Pmobj_t);
begin
  actor.flags := actor.flags and not MF_NOGRAVITY;
  actor.flags_ex := actor.flags_ex and not MF_EX_LOWGRAVITY;
  actor.flags2_ex := actor.flags2_ex or MF2_EX_MEDIUMGRAVITY;
end;

//
// PROC A_HideThing
//
procedure A_HideThing(actor: Pmobj_t);
begin
  actor.flags2_ex := actor.flags2_ex or MF2_EX_DONTDRAW;
end;

//
// A_UnHideThing
//
procedure A_UnHideThing(actor: Pmobj_t);
begin
  actor.flags2_ex := actor.flags2_ex and not MF2_EX_DONTDRAW;
end;

//
// A_SpawnDebris(mobj_no, count, mult_h, mult_v
//
procedure A_SpawnDebris(actor: Pmobj_t);
var
  i, count: integer;
  mult_h, mult_v: fixed_t;
  mobj_no: integer;
  mo: Pmobj_t;
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

  count := actor.state.params.IntVal[1];
  if count <= 0 then
    count := 1;

  // JVAL 20180222 -> IntVal Changed to FixedVal
  mult_h := actor.state.params.FixedVal[2];
  if mult_h <= 0 then
    mult_h := FRACUNIT;

  // JVAL 20180222 -> IntVal Changed to FixedVal
  mult_v := actor.state.params.FixedVal[3];
  if mult_v <= 0 then
    mult_v := FRACUNIT;

  for i := 0 to count - 1 do
  begin
    mo := P_SpawnMobj(actor.x + ((N_Random - 128) shr 12),
           actor.y + ((N_Random - 128) shr 12),
           actor.z + ((N_Random * actor.height) div 256), mobj_no);
    if mo <> nil then
    begin
      mo.momz := FixedMul(mult_v, ((N_Random and 7) + 5) * FRACUNIT);
      mo.momx := FixedMul(mult_h, (N_Random - N_Random) shl (FRACBITS - 6));
      mo.momy := FixedMul(mult_h, (N_Random - N_Random) shl (FRACBITS - 6));
    end;
  end;
end;

procedure A_SpawnSmokeUp(actor: Pmobj_t);
var
  mo: Pmobj_t;
begin
  mo := P_SpawnMobj(actor.x, actor.y, ONFLOORZ, Ord(MT_LAVASMOKE));
  mo.momz := FRACUNIT + (P_Random * 128);
end;

procedure A_SpawnSmokeDown(actor: Pmobj_t);
var
  mo: Pmobj_t;
begin
  mo := P_SpawnMobj(actor.x, actor.y, ONCEILINGZ, Ord(MT_LAVASMOKE));
  mo.momz := -2 * FRACUNIT + (P_Random * 128);
end;

procedure A_SpawnSmokeHorz(actor: Pmobj_t);
var
  mo: Pmobj_t;
  an: angle_t;
  speed: fixed_t;
  height: integer;
begin
  if not P_CheckStateParams(actor) then
    exit;

  // JVAL 20180222 -> IntVal Changed to FixedVal
  height := actor.state.params.FixedVal[0];

  mo := P_SpawnMobj(actor.x, actor.y, actor.z + height, Ord(MT_LAVASMOKE));
  an := actor.angle shr ANGLETOFINESHIFT;
  speed := (5 - (N_Random mod 3)) * FRACUNIT;
  mo.momx := FixedMul(speed, finecosine[an]);
  mo.momy := FixedMul(speed, finesine[an]);
end;

procedure A_SetInteractive(actor: Pmobj_t);
begin
  actor.flags2_ex := actor.flags2_ex or MF2_EX_INTERACTIVE;
end;

procedure A_UnSetInteractive(actor: Pmobj_t);
begin
  actor.flags2_ex := actor.flags2_ex and not MF2_EX_INTERACTIVE;
end;

procedure A_SetMonsterInfight(actor: Pmobj_t);
begin
  actor.flags2_ex := actor.flags2_ex or MF2_EX_DONTINFIGHTMONSTERS;
end;

procedure A_UnSetMonsterInfight(actor: Pmobj_t);
begin
  actor.flags2_ex := actor.flags2_ex and not MF2_EX_DONTINFIGHTMONSTERS;
end;

procedure A_NoiseAlert(actor: Pmobj_t);
begin
  if actor.target = nil then
    exit;
  if actor.target.player = nil then
    exit;

  P_NoiseAlert(actor.target, actor);
end;

procedure A_SetShootable(actor: Pmobj_t);
begin
  actor.flags := actor.flags or MF_SHOOTABLE;
end;

procedure A_UnSetShootable(actor: Pmobj_t);
begin
  actor.flags := actor.flags and not MF_SHOOTABLE;
end;

//
// A_PlayerMessage
//
procedure A_PlayerMessage(actor: Pmobj_t);
var
  p: Pplayer_t;
  msg: string;
  i: integer;
begin
  if not P_CheckStateParams(actor) then
    exit;

  p := actor.player;
  if p = nil then
  begin
    if actor.target = nil then
      Exit;
    p := actor.target.player;
    if p = nil then exit;
  end;

  msg := actor.state.params.StrVal[0];
  for i := 1 to actor.state.params.Count - 1 do
    msg := msg + ' ' + actor.state.params.StrVal[i];

  p._message := msg;
end;

procedure A_PlayerFaceMe(actor: Pmobj_t);
begin
  if not P_CheckStateParams(actor, 1) then
    exit;

  if actor.target = nil then
    Exit;

  if actor.target.player = nil then
    Exit;

  P_PlayerFaceMobj(actor.target.player, actor, actor.state.params.IntVal[0]);
end;

procedure A_SetFloorClip(actor: Pmobj_t);
begin
  actor.flags := actor.flags or MF_FEETCLIPPED;
  if (actor.z = Psubsector_t(actor.subsector).sector.floorheight) and
     (P_GetThingFloorType(actor) > FLOOR_SOLID) then
      actor.floorclip := FOOTCLIPSIZE
    else
      actor.floorclip := 0;
end;

procedure A_UnSetFloorClip(actor: Pmobj_t);
begin
  actor.flags := actor.flags and not MF_FEETCLIPPED;
end;

end.

