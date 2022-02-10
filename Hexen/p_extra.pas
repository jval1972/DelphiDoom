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

unit p_extra;

//
// JVAL
// Extra mobj functions

interface

uses
  m_fixed,
  p_mobj_h;

//==============================================================================
//
// A_LowGravity
//
//==============================================================================
procedure A_LowGravity(actor: Pmobj_t);

//==============================================================================
//
// A_NoGravity
//
//==============================================================================
procedure A_NoGravity(actor: Pmobj_t);

//==============================================================================
//
// A_Gravity
//
//==============================================================================
procedure A_Gravity(actor: Pmobj_t);

//==============================================================================
//
// A_NoBlocking
//
//==============================================================================
procedure A_NoBlocking(actor: Pmobj_t);

//==============================================================================
//
// A_MeleeAttack
//
//==============================================================================
procedure A_MeleeAttack(actor: Pmobj_t);

//==============================================================================
//
// A_Die
//
//==============================================================================
procedure A_Die(actor: Pmobj_t);

//==============================================================================
//
// A_CustomBulletAttack
//
//==============================================================================
procedure A_CustomBulletAttack(actor: Pmobj_t);

//==============================================================================
//
// A_Countdown
//
//==============================================================================
procedure A_Countdown(actor: Pmobj_t);

//==============================================================================
//
// A_SetInvulnerable
//
//==============================================================================
procedure A_SetInvulnerable(actor: Pmobj_t);

//==============================================================================
//
// A_UnSetInvulnerable
//
//==============================================================================
procedure A_UnSetInvulnerable(actor: Pmobj_t);

//==============================================================================
//
// A_FloatBob
//
//==============================================================================
procedure A_FloatBob(actor: Pmobj_t);

//==============================================================================
//
// A_NoFloatBob
//
//==============================================================================
procedure A_NoFloatBob(actor: Pmobj_t);

//==============================================================================
//
// A_ComboAttack
//
//==============================================================================
procedure A_ComboAttack(actor: Pmobj_t);

//==============================================================================
//
// A_BulletAttack
//
//==============================================================================
procedure A_BulletAttack(actor: Pmobj_t);

//==============================================================================
//
// A_MediumGravity
//
//==============================================================================
procedure A_MediumGravity(actor: Pmobj_t);

implementation

uses
  d_delphi,
  doomdef,
  g_game,
  info_h,
  info,
  m_rnd,
  p_enemy,
  p_mobj,
  p_inter,
  p_map,
  p_local,
  p_sounds,
  p_common,
  tables;

//==============================================================================
// A_LowGravity
//
// JVAL
// Low gravity
//
//==============================================================================
procedure A_LowGravity(actor: Pmobj_t);
begin
  actor.flags := actor.flags and not MF_NOGRAVITY;
  actor.flags2 := actor.flags2 or MF2_LOGRAV;
  actor.flags_ex := actor.flags_ex or MF_EX_LOWGRAVITY;
  actor.flags2_ex := actor.flags2_ex and not MF2_EX_MEDIUMGRAVITY;
end;

//==============================================================================
// A_NoGravity
//
// JVAL
// Remove gravity
//
//==============================================================================
procedure A_NoGravity(actor: Pmobj_t);
begin
  actor.flags := actor.flags or MF_NOGRAVITY;
  actor.flags2 := actor.flags2 and not MF2_LOGRAV;
  actor.flags_ex := actor.flags_ex and not MF_EX_LOWGRAVITY;
  actor.flags2_ex := actor.flags2_ex and not MF2_EX_MEDIUMGRAVITY;
end;

//==============================================================================
// A_Gravity
//
// JVAL
// Normal gravity
//
//==============================================================================
procedure A_Gravity(actor: Pmobj_t);
begin
  actor.flags := actor.flags and not MF_NOGRAVITY;
  actor.flags2 := actor.flags2 and not MF2_LOGRAV;
  actor.flags_ex := actor.flags_ex and not MF_EX_LOWGRAVITY;
  actor.flags2_ex := actor.flags2_ex and not MF2_EX_MEDIUMGRAVITY;
end;

//==============================================================================
// A_NoBlocking
//
// JVAL
// Remove blocking flag
//
//==============================================================================
procedure A_NoBlocking(actor: Pmobj_t);
begin
  actor.flags := actor.flags and not MF_SOLID;
end;

//==============================================================================
//
// JVAL
// Close distance attack
// A_MeleeAttack(mindamage=0; maxdamage=0);
//
//==============================================================================
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
    A_MeleeSound1(actor);
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

//==============================================================================
//
// A_Die
//
//==============================================================================
procedure A_Die(actor: Pmobj_t);
begin
  actor.flags_ex := actor.flags_ex and not MF_EX_INVULNERABLE;  // Clear invulnerability flag
  actor.flags2 := actor.flags2 and not MF2_INVULNERABLE;        // Clear invulnerability flag
  actor.flags2_ex := actor.flags2_ex and not MF2_EX_NODAMAGE;   // Clear no damage flag
  if G_PlayingEngineVersion > VERSION141 then
    P_DamageMobj(actor, nil, nil, 10000)
  else
    P_DamageMobj(actor, nil, nil, actor.health);
end;

//==============================================================================
// A_CustomBulletAttack
//
// CustomBulletAttack(spread_xy, numbullets, damageperbullet, range)
//
//==============================================================================
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

//==============================================================================
//
// A_Countdown(void)
//
//==============================================================================
procedure A_Countdown(actor: Pmobj_t);
begin
  dec(actor.reactiontime);
  if actor.reactiontime <= 0 then
  begin
    P_ExplodeMissile(actor);
    actor.flags := actor.flags and not MF_SKULLFLY;
  end;
end;

//==============================================================================
//
// A_SetInvulnerable
//
//==============================================================================
procedure A_SetInvulnerable(actor: Pmobj_t);
begin
  actor.flags_ex := actor.flags_ex or MF_EX_INVULNERABLE;
  actor.flags2 := actor.flags2 or MF2_INVULNERABLE;
end;

//==============================================================================
//
// A_UnSetInvulnerable
//
//==============================================================================
procedure A_UnSetInvulnerable(actor: Pmobj_t);
begin
  actor.flags_ex := actor.flags_ex and not MF_EX_INVULNERABLE;
  actor.flags2 := actor.flags2 and not MF2_INVULNERABLE;
end;

//==============================================================================
//
// A_FloatBob
//
//==============================================================================
procedure A_FloatBob(actor: Pmobj_t);
begin
  actor.flags_ex := actor.flags_ex or MF_EX_FLOATBOB;
  actor.flags2 := actor.flags2 or MF2_FLOATBOB;
end;

//==============================================================================
//
// A_NoFloatBob
//
//==============================================================================
procedure A_NoFloatBob(actor: Pmobj_t);
begin
  actor.flags_ex := actor.flags_ex and not MF_EX_FLOATBOB;
  actor.flags2 := actor.flags2 and not MF2_FLOATBOB;
end;

//==============================================================================
//
// A_Missile
//
//==============================================================================
procedure A_Missile(actor: Pmobj_t);
begin
  actor.flags := actor.flags or MF_MISSILE;
end;

//==============================================================================
//
// A_NoMissile
//
//==============================================================================
procedure A_NoMissile(actor: Pmobj_t);
begin
  actor.flags := actor.flags and not MF_MISSILE;
end;

//==============================================================================
//
// A_ComboAttack(void)
//
//==============================================================================
procedure A_ComboAttack(actor: Pmobj_t);
var
  missile: Pmobj_t;
  mobj_no: integer;
begin
  A_FaceTarget(actor);

  if P_CheckMeleeRange(actor) then
  begin
    A_MeleeSound1(actor);
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
      if missile.flags_ex and MF_EX_SEEKERMISSILE <> 0 then
        missile.tracer := actor.target;
    end;
  end;
end;

//==============================================================================
//
// A_BulletAttack(numbullets: integer [optional])
//
//==============================================================================
procedure A_BulletAttack(actor: Pmobj_t);
var
  i: integer;
  angle, bangle: angle_t;
  slope: fixed_t;
  damage: integer;
  numbullets: integer;
begin
  if actor.target = nil then
    exit;

  A_FaceTarget(actor);
  bangle := actor.angle;

  slope := P_AimLineAttack(actor, bangle, MISSILERANGE);

  A_AttackSound1(actor);

// Attack with a customizable amount of bullets (specified in damage)
  numbullets := actor.info.damage;
// If parameter specified, then use parameter as custom number of bullets
  if actor.state.params <> nil then
    if actor.state.params.Count = 1 then
      numbullets := actor.state.params.IntVal[0];

  for i := 0 to numbullets - 1 do
  begin
    angle := bangle + _SHLW(P_Random - P_Random, 20);
    damage := ((P_Random mod 5) + 1) * 3;
    P_LineAttack(actor, angle, MISSILERANGE, slope, damage);
  end;
end;

//==============================================================================
// A_MediumGravity
//
// JVAL
// Medium gravity
//
//==============================================================================
procedure A_MediumGravity(actor: Pmobj_t);
begin
  actor.flags := actor.flags and not MF_NOGRAVITY;
  actor.flags_ex := actor.flags_ex and not MF_EX_LOWGRAVITY;
  actor.flags2_ex := actor.flags2_ex or MF2_EX_MEDIUMGRAVITY;
end;

end.

