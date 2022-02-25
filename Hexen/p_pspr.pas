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

unit p_pspr;

interface

uses
  doomdef,
// Basic data types.
// Needs fixed point, and BAM angles.
  m_fixed,
  tables,
  info_h,
  p_pspr_h,
  p_mobj_h,
  d_player;

//-----------------------------------------------------------------------------
//
// DESCRIPTION:
//  Sprite animation.
//  Weapon sprite animation, weapon objects.
//  Action functions for weapons.
//
//-----------------------------------------------------------------------------

const
//
// Frame flags:
// handles maximum brightness (torches, muzzle flare, light sources)
//
  FF_FULLBRIGHT = $8000; // flag in thing.frame
  FF_FRAMEMASK = $7fff;

//==============================================================================
//
// P_SetPsprite
//
//==============================================================================
procedure P_SetPsprite(player: Pplayer_t; position: integer; stnum: statenum_t);

//==============================================================================
//
// P_SetPspriteNF
//
//==============================================================================
procedure P_SetPspriteNF(player: Pplayer_t; position: integer; stnum: statenum_t);

//==============================================================================
//
// P_DropWeapon
//
//==============================================================================
procedure P_DropWeapon(player: Pplayer_t);

//==============================================================================
//
// P_CheckMana
//
//==============================================================================
function P_CheckMana(player: Pplayer_t): boolean;

//==============================================================================
//
// P_FireWeapon
//
//==============================================================================
procedure P_FireWeapon(player: Pplayer_t);

//==============================================================================
//
// A_ReFire
//
//==============================================================================
procedure A_ReFire(player: Pplayer_t; psp: Ppspdef_t);

//==============================================================================
//
// A_Lower
//
//==============================================================================
procedure A_Lower(player: Pplayer_t; psp: Ppspdef_t);

//==============================================================================
//
// A_Raise
//
//==============================================================================
procedure A_Raise(player: Pplayer_t; psp: Ppspdef_t);

//==============================================================================
//
// A_Light0
//
//==============================================================================
procedure A_Light0(player: Pplayer_t; psp: Ppspdef_t);

//==============================================================================
//
// P_ActivateMorphWeapon
//
//==============================================================================
procedure P_ActivateMorphWeapon(player: Pplayer_t);

//==============================================================================
//
// P_PostMorphWeapon
//
//==============================================================================
procedure P_PostMorphWeapon(player: Pplayer_t; weapon: weapontype_t);

//==============================================================================
//
// A_WeaponReady
//
//==============================================================================
procedure A_WeaponReady(player: Pplayer_t; psp: Ppspdef_t);

//==============================================================================
//
// P_AdjustPlayerAngle
//
//==============================================================================
procedure P_AdjustPlayerAngle(pmo: Pmobj_t);

//==============================================================================
//
// A_SnoutAttack
//
//==============================================================================
procedure A_SnoutAttack(player: Pplayer_t; psp: Ppspdef_t);

//==============================================================================
//
// A_FHammerAttack
//
//==============================================================================
procedure A_FHammerAttack(player: Pplayer_t; psp: Ppspdef_t);

//==============================================================================
//
// A_FHammerThrow
//
//==============================================================================
procedure A_FHammerThrow(player: Pplayer_t; psp: Ppspdef_t);

//==============================================================================
//
// A_FSwordAttack
//
//==============================================================================
procedure A_FSwordAttack(player: Pplayer_t; psp: Ppspdef_t);

//==============================================================================
//
// A_FSwordAttack2
//
//==============================================================================
procedure A_FSwordAttack2(actor: Pmobj_t);

//==============================================================================
//
// A_FSwordFlames
//
//==============================================================================
procedure A_FSwordFlames(actor: Pmobj_t);

//==============================================================================
//
// A_MWandAttack
//
//==============================================================================
procedure A_MWandAttack(player: Pplayer_t; psp: Ppspdef_t);

//==============================================================================
//
// A_LightningReady
//
//==============================================================================
procedure A_LightningReady(player: Pplayer_t; psp: Ppspdef_t);

//==============================================================================
//
// A_LightningClip
//
//==============================================================================
procedure A_LightningClip(actor: Pmobj_t);

//==============================================================================
//
// A_LightningZap
//
//==============================================================================
procedure A_LightningZap(actor: Pmobj_t);

//==============================================================================
//
// A_MLightningAttack2
//
//==============================================================================
procedure A_MLightningAttack2(actor: Pmobj_t);

//==============================================================================
//
// A_MLightningAttack
//
//==============================================================================
procedure A_MLightningAttack(player: Pplayer_t; psp: Ppspdef_t);

//==============================================================================
//
// A_ZapMimic
//
//==============================================================================
procedure A_ZapMimic(actor: Pmobj_t);

//==============================================================================
//
// A_LastZap
//
//==============================================================================
procedure A_LastZap(actor: Pmobj_t);

//==============================================================================
//
// A_LightningRemove
//
//==============================================================================
procedure A_LightningRemove(actor: Pmobj_t);

//==============================================================================
//
// P_MStaffSpawn
//
//==============================================================================
procedure P_MStaffSpawn(pmo: Pmobj_t; angle: angle_t);

//==============================================================================
//
// A_MStaffAttack
//
//==============================================================================
procedure A_MStaffAttack(player: Pplayer_t; psp: Ppspdef_t);

//==============================================================================
//
// A_MStaffPalette
//
//==============================================================================
procedure A_MStaffPalette(player: Pplayer_t; psp: Ppspdef_t);

//==============================================================================
//
// A_MStaffWeave
//
//==============================================================================
procedure A_MStaffWeave(actor: Pmobj_t);

//==============================================================================
//
// A_MStaffTrack
//
//==============================================================================
procedure A_MStaffTrack(actor: Pmobj_t);

//==============================================================================
//
// P_MStaffSpawn2
//
//==============================================================================
procedure P_MStaffSpawn2(actor: Pmobj_t; angle: angle_t);

//==============================================================================
//
// A_MStaffAttack2
//
//==============================================================================
procedure A_MStaffAttack2(actor: Pmobj_t);

//==============================================================================
//
// A_FPunchAttack
//
//==============================================================================
procedure A_FPunchAttack(player: Pplayer_t; psp: Ppspdef_t);

//==============================================================================
//
// A_FAxeAttack
//
//==============================================================================
procedure A_FAxeAttack(player: Pplayer_t; psp: Ppspdef_t);

//==============================================================================
//
// A_CMaceAttack
//
//==============================================================================
procedure A_CMaceAttack(player: Pplayer_t; psp: Ppspdef_t);

//==============================================================================
//
// A_CStaffCheck
//
//==============================================================================
procedure A_CStaffCheck(player: Pplayer_t; psp: Ppspdef_t);

//==============================================================================
//
// A_CStaffAttack
//
//==============================================================================
procedure A_CStaffAttack(player: Pplayer_t; psp: Ppspdef_t);

//==============================================================================
//
// A_CStaffInitBlink
//
//==============================================================================
procedure A_CStaffInitBlink(player: Pplayer_t; psp: Ppspdef_t);

//==============================================================================
//
// A_CStaffCheckBlink
//
//==============================================================================
procedure A_CStaffCheckBlink(player: Pplayer_t; psp: Ppspdef_t);

//==============================================================================
//
// A_CFlameAttack
//
//==============================================================================
procedure A_CFlameAttack(player: Pplayer_t; psp: Ppspdef_t);

//==============================================================================
//
// A_CFlamePuff
//
//==============================================================================
procedure A_CFlamePuff(actor: Pmobj_t);

//==============================================================================
//
// A_CFlameMissile
//
//==============================================================================
procedure A_CFlameMissile(actor: Pmobj_t);

//==============================================================================
//
// A_CFlameRotate
//
//==============================================================================
procedure A_CFlameRotate(actor: Pmobj_t);

//==============================================================================
//
// A_CHolyAttack3
//
//==============================================================================
procedure A_CHolyAttack3(actor: Pmobj_t);

//==============================================================================
//
// A_CHolyAttack2
//
//==============================================================================
procedure A_CHolyAttack2(actor: Pmobj_t);

//==============================================================================
//
// A_CHolyAttack
//
//==============================================================================
procedure A_CHolyAttack(player: Pplayer_t; psp: Ppspdef_t);

//==============================================================================
//
// A_CHolyPalette
//
//==============================================================================
procedure A_CHolyPalette(player: Pplayer_t; psp: Ppspdef_t);

//==============================================================================
//
// P_CHolyFindTarget
//
//==============================================================================
procedure P_CHolyFindTarget(actor: Pmobj_t);

//==============================================================================
//
// P_CHolySeekerMissile
//
//==============================================================================
procedure P_CHolySeekerMissile(actor: Pmobj_t; thresh: angle_t; turnMax: angle_t);

//==============================================================================
//
// A_CHolyWeave
//
//==============================================================================
procedure A_CHolyWeave(actor: Pmobj_t);

//==============================================================================
//
// A_CHolySeek
//
//==============================================================================
procedure A_CHolySeek(actor: Pmobj_t);

//==============================================================================
//
// P_CHolyTailFollow
//
//==============================================================================
procedure P_CHolyTailFollow(actor: Pmobj_t; dist: fixed_t);

//==============================================================================
//
// P_CHolyTailRemove
//
//==============================================================================
procedure P_CHolyTailRemove(actor: Pmobj_t);

//==============================================================================
//
// A_CHolyTail
//
//==============================================================================
procedure A_CHolyTail(actor: Pmobj_t);

//==============================================================================
//
// A_CHolyCheckScream
//
//==============================================================================
procedure A_CHolyCheckScream(actor: Pmobj_t);

//==============================================================================
//
// A_CHolySpawnPuff
//
//==============================================================================
procedure A_CHolySpawnPuff(actor: Pmobj_t);

//==============================================================================
//
// A_FireConePL1
//
//==============================================================================
procedure A_FireConePL1(player: Pplayer_t; psp: Ppspdef_t);

//==============================================================================
//
// A_ShedShard
//
//==============================================================================
procedure A_ShedShard(actor: Pmobj_t);

//==============================================================================
//
// P_SetupPsprites
//
//==============================================================================
procedure P_SetupPsprites(player: Pplayer_t);

//==============================================================================
//
// P_MovePsprites
//
//==============================================================================
procedure P_MovePsprites(player: Pplayer_t);

//==============================================================================
//
// P_CheckAmmo
//
//==============================================================================
function P_CheckAmmo(player: Pplayer_t): boolean;

var
  WeaponManaUse: array[0..Ord(NUMCLASSES) - 1, 0..Ord(NUMWEAPONS) - 1] of integer = (
    (0, 2, 3, 14),
    (0, 1, 4, 18),
    (0, 3, 5, 15),
    (0, 0, 0, 0 )
  );

implementation

uses
  d_delphi,
  a_action,
  g_game,
  i_system,
  {$IFDEF OPENGL}
  gl_main,
  {$ELSE}
  i_video,
  {$ENDIF}
  p_user,
  r_hires,
  info,
//
// Needs to include the precompiled
//  sprite animation tables.
// Header generated by multigen utility.
// This includes all the data for thing animation,
// i.e. the Thing Atrributes table
// and the Frame Sequence table.
  d_event,
  m_rnd,
  p_local,
  p_tick,
  p_mobj,
  p_enemy,
  p_map,
  p_inter,
  p_maputl,
  p_common,
  r_main,
  s_sound,
// Data.
  sounddata,
  v_data,
  v_video,
  z_zone;

//
// Adjust weapon bottom and top
//

const
  WEAPONTOP = 32 * FRACUNIT;
  WEAPONBOTTOM = WEAPONTOP + 96 * FRACUNIT;

const
  LOWERSPEED = 6 * FRACUNIT;
  RAISESPEED = 6 * FRACUNIT;

//---------------------------------------------------------------------------
//
// PROC P_SetPsprite
//
//---------------------------------------------------------------------------
const
  PSPR_CYCLE_LIMIT = 1000000;

//==============================================================================
//
// P_SetPsprite
//
//==============================================================================
procedure P_SetPsprite(player: Pplayer_t; position: integer; stnum: statenum_t);
var
  psp: Ppspdef_t;
  state: Pstate_t;
  cycle_counter: integer;
begin
  cycle_counter := 0;
  psp := @player.psprites[position];
  repeat
    if Ord(stnum) = 0 then
    begin
      // object removed itself
      psp.state := nil;
      break;
    end;

    state := @states[Ord(stnum)];
    psp.state := state;
    psp.tics := P_TicsFromState(state); // could be 0

    // coordinate set
    if state.misc1 <> 0 then
      psp.sx := state.misc1 * FRACUNIT;

    if state.misc2 <> 0 then
      psp.sy := state.misc2 * FRACUNIT;

    // Call action routine.
    // Modified handling.
    if Assigned(state.action.acp2) then
    begin
      if state.params <> nil then
        state.params.actor := player.mo;
      state.action.acp2(player, psp);
      if psp.state = nil then
        break;
    end;

    stnum := psp.state.nextstate;

    inc(cycle_counter);
    if cycle_counter > PSPR_CYCLE_LIMIT then
      I_Error('P_SetPsprite(): Infinite state cycle detected in player sprites (readyweapon=%d, pendinfweapon=%d)!',
        [Ord(player.readyweapon), Ord(player.pendingweapon)]);
  until psp.tics <> 0;
  // an initial state of 0 could cycle through
end;

//---------------------------------------------------------------------------
//
// PROC P_SetPspriteNF
//
// Identical to P_SetPsprite, without calling the action function
//---------------------------------------------------------------------------
//
//==============================================================================
procedure P_SetPspriteNF(player: Pplayer_t; position: integer; stnum: statenum_t);
var
  psp: Ppspdef_t;
  state: Pstate_t;
  cycle_counter: integer;
begin
  cycle_counter := 0;
  psp := @player.psprites[position];
  repeat
    if Ord(stnum) = 0 then
    begin
      // object removed itself
      psp.state := nil;
      break;
    end;

    state := @states[Ord(stnum)];
    psp.state := state;
    psp.tics := state.tics; // could be 0

    // coordinate set
    if state.misc1 <> 0 then
      psp.sx := state.misc1 * FRACUNIT;

    if state.misc2 <> 0 then
      psp.sy := state.misc2 * FRACUNIT;

    stnum := psp.state.nextstate;

    inc(cycle_counter);
    if cycle_counter > PSPR_CYCLE_LIMIT then
      I_Error('P_SetPsprite(): Infinite state cycle detected in player sprites (readyweapon=%d, pendinfweapon=%d)!',
        [Ord(player.readyweapon), Ord(player.pendingweapon)]);
  until psp.tics <> 0;
  // an initial state of 0 could cycle through
end;

//---------------------------------------------------------------------------
//
// PROC P_BringUpWeapon
//
// Starts bringing the pending weapon up from the bottom of the screen.
//
//---------------------------------------------------------------------------
//
//==============================================================================
procedure P_BringUpWeapon(player: Pplayer_t);
var
  newstate: statenum_t;
begin
  if player.pendingweapon = WP_NOCHANGE then
    player.pendingweapon := player.readyweapon;

  if (player._class = PCLASS_FIGHTER) and
     (player.pendingweapon = WP_SECOND) and
     (player.mana[Ord(MANA_1)] <> 0) then
    newstate := S_FAXEUP_G
  else
    newstate := statenum_t(WeaponInfo[Ord(player.pendingweapon), Ord(player._class)].upstate);

  player.pendingweapon := WP_NOCHANGE;
  player.psprites[Ord(ps_weapon)].sy := WEAPONBOTTOM;

  P_SetPsprite(player, Ord(ps_weapon), newstate);
end;

//---------------------------------------------------------------------------
//
// PROC P_DropWeapon
//
// The player died, so put the weapon away.
//
//---------------------------------------------------------------------------
//
//==============================================================================
procedure P_DropWeapon(player: Pplayer_t);
begin
  P_SetPsprite(player, Ord(ps_weapon), statenum_t(WeaponInfo[Ord(player.readyweapon), Ord(player._class)].downstate))
end;

//---------------------------------------------------------------------------
//
// FUNC P_CheckMana
//
// Returns true if there is enough mana to shoot.  If not, selects the
// next weapon to use.
//
//---------------------------------------------------------------------------
//
//==============================================================================
function P_CheckMana(player: Pplayer_t): boolean;
var
  mana: manatype_t;
  count: integer;
begin
  mana := WeaponInfo[Ord(player.readyweapon), Ord(player._class)].mana;
  count := WeaponManaUse[Ord(player._class), Ord(player.readyweapon)];
  if mana = MANA_BOTH then
  begin
    if (player.mana[Ord(MANA_1)] >= count) and (player.mana[Ord(MANA_2)] >= count) then
    begin
      result := true;
      exit;
    end;
  end
  else if (mana = MANA_NONE) or (player.mana[Ord(mana)] >= count) then
  begin
    result := true;
    exit;
  end;

  // out of mana, pick a weapon to change to
  repeat
    if player.weaponowned[Ord(WP_THIRD)] and
       (player.mana[Ord(MANA_2)] >= WeaponManaUse[Ord(player._class), Ord(WP_THIRD)]) then
    begin
      player.pendingweapon := WP_THIRD;
    end
    else if player.weaponowned[Ord(WP_SECOND)] and
            (player.mana[Ord(MANA_1)] >= WeaponManaUse[Ord(player._class), Ord(WP_SECOND)]) then
    begin
      player.pendingweapon := WP_SECOND;
    end
    else if player.weaponowned[Ord(WP_FOURTH)] and
            (player.mana[Ord(MANA_1)] >= WeaponManaUse[Ord(player._class), Ord(WP_FOURTH)]) and
            (player.mana[Ord(MANA_2)] >= WeaponManaUse[Ord(player._class), Ord(WP_FOURTH)]) then
    begin
      player.pendingweapon := WP_FOURTH;
    end
    else
    begin
      player.pendingweapon := WP_FIRST;
    end;
  until player.pendingweapon <> WP_NOCHANGE;

  P_SetPsprite(player, Ord(ps_weapon), statenum_t(WeaponInfo[Ord(player.readyweapon), Ord(player._class)].downstate));
  result := false;
end;

//==============================================================================
//
// P_CheckAmmo
//
//==============================================================================
function P_CheckAmmo(player: Pplayer_t): boolean;
begin
  result := P_CheckMana(player);
end;

//---------------------------------------------------------------------------
//
// PROC P_FireWeapon
//
//---------------------------------------------------------------------------
//
//==============================================================================
procedure P_FireWeapon(player: Pplayer_t);
var
  attackState: statenum_t;
begin
  if not P_CheckMana(player) then
    exit;

  P_SetMobjState(player.mo, PStateAttack[Ord(player._class)]);
  if (player._class = PCLASS_FIGHTER) and
     (player.readyweapon = WP_SECOND) and
     (player.mana[Ord(MANA_1)] > 0) then
  begin // Glowing axe
    attackState := S_FAXEATK_G1;
  end
  else
  begin
    if player.refire <> 0 then
      attackState := statenum_t(WeaponInfo[Ord(player.readyweapon), Ord(player._class)].holdatkstate)
    else
      attackState := statenum_t(WeaponInfo[Ord(player.readyweapon), Ord(player._class)].atkstate);
  end;

  P_SetPsprite(player, Ord(ps_weapon), attackState);
  P_NoiseAlert(player.mo, player.mo);
end;

//---------------------------------------------------------------------------
//
// PROC A_ReFire
//
// The player can re fire the weapon without lowering it entirely.
//
//---------------------------------------------------------------------------
//
//==============================================================================
procedure A_ReFire(player: Pplayer_t; psp: Ppspdef_t);
begin
  // check for fire
  //  (if a weaponchange is pending, let it go through instead)
  if (player.cmd.buttons and BT_ATTACK <> 0) and
     (player.pendingweapon = wp_nochange) and
     (player.health > 0) then
  begin
    player.refire := player.refire + 1;
    P_FireWeapon(player);
  end
  else
  begin
    player.refire := 0;
    P_CheckMana(player);
  end;
end;

//---------------------------------------------------------------------------
//
// PROC A_Lower
//
//---------------------------------------------------------------------------
//
//==============================================================================
procedure A_Lower(player: Pplayer_t; psp: Ppspdef_t);
begin
  if player.morphTics <> 0 then
    psp.sy := WEAPONBOTTOM
  else
    psp.sy := psp.sy + LOWERSPEED;

  // Is already down.
  if psp.sy < WEAPONBOTTOM then
    exit;

  // Player is dead.
  if player.playerstate = PST_DEAD then
  begin
    psp.sy := WEAPONBOTTOM;
    // don't bring weapon back up
    exit;
  end;

  // The old weapon has been lowered off the screen,
  // so change the weapon and start raising it
  if player.health = 0 then
  begin
    // Player is dead, so keep the weapon off screen.
    P_SetPsprite(player, Ord(ps_weapon), S_NULL);
    exit;
  end;

  player.readyweapon := player.pendingweapon;

  P_BringUpWeapon(player);
end;

//---------------------------------------------------------------------------
//
// PROC A_Raise
//
//---------------------------------------------------------------------------
//
//==============================================================================
procedure A_Raise(player: Pplayer_t; psp: Ppspdef_t);
begin
  psp.sy := psp.sy - RAISESPEED;

  if psp.sy > WEAPONTOP then // Not raised all the way yet
    exit;

  psp.sy := WEAPONTOP;

  if (player._class = PCLASS_FIGHTER) and
     (player.readyweapon = WP_SECOND) and
     (player.mana[Ord(MANA_1)] <> 0) then
    P_SetPsprite(player, Ord(ps_weapon), S_FAXEREADY_G)
  else
    P_SetPsprite(player, Ord(ps_weapon),
      statenum_t(WeaponInfo[Ord(player.readyweapon), Ord(player._class)].readystate));
end;

//==============================================================================
// A_Light0
//
// ?
//
//==============================================================================
procedure A_Light0(player: Pplayer_t; psp: Ppspdef_t);
begin
  player.extralight := 0;
end;

//---------------------------------------------------------------------------
//
// PROC P_ActivateMorphWeapon
//
//---------------------------------------------------------------------------
//
//==============================================================================
procedure P_ActivateMorphWeapon(player: Pplayer_t);
begin
  player.pendingweapon := WP_NOCHANGE;
  player.psprites[Ord(ps_weapon)].sy := WEAPONTOP;
  player.readyweapon := WP_FIRST;  // Snout is the first weapon
  P_SetPsprite(player, Ord(ps_weapon), S_SNOUTREADY);
end;

//---------------------------------------------------------------------------
//
// PROC P_PostMorphWeapon
//
//---------------------------------------------------------------------------
//
//==============================================================================
procedure P_PostMorphWeapon(player: Pplayer_t; weapon: weapontype_t);
begin
  player.pendingweapon := WP_NOCHANGE;
  player.readyweapon := weapon;
  player.psprites[Ord(ps_weapon)].sy := WEAPONBOTTOM;
  P_SetPsprite(player, Ord(ps_weapon), statenum_t(WeaponInfo[Ord(weapon), Ord(player._class)].upstate));
end;

//---------------------------------------------------------------------------
//
// PROC A_WeaponReady
//
// The player can fire the weapon or change to another weapon at this time.
//
//---------------------------------------------------------------------------
//
//==============================================================================
procedure A_WeaponReady(player: Pplayer_t; psp: Ppspdef_t);
var
  angle: angle_t;
begin
  // Change player from attack state
  if (LongWord(player.mo.state) >= LongWord(@states[Ord(PStateAttack[Ord(player._class)])])) and
     (LongWord(player.mo.state) <= LongWord(@states[Ord(PStateAttackEnd[Ord(player._class)])])) then
  begin
    P_SetMobjState(player.mo, PStateNormal[Ord(player._class)]);
  end;
  // Put the weapon away if the player has a pending weapon or has
  // died.
  if (player.pendingweapon <> WP_NOCHANGE) or (player.health = 0) then
  begin
    P_SetPsprite(player, Ord(ps_weapon),
      statenum_t(WeaponInfo[Ord(player.readyweapon), Ord(player._class)].downstate));
    exit;
  end;

  // Check for fire.
  if player.cmd.buttons and BT_ATTACK <> 0 then
  begin
    player.attackdown := true;
    P_FireWeapon(player);
    exit;
  end;

  player.attackdown := false;

  if player.morphTics = 0 then
  begin
    // Bob the weapon based on movement speed.
    angle := (128 * leveltime) and FINEMASK;
    psp.sx := FRACUNIT + FixedMul(player.bob, finecosine[angle]);
    angle := angle and (FINEANGLES div 2-1);
    psp.sy := WEAPONTOP + FixedMul(player.bob, finesine[angle]);
  end;
end;

//****************************************************************************
//
// WEAPON ATTACKS
//
//****************************************************************************

//
//  P_AdjustPlayerAngle
//

const
  MAX_ANGLE_ADJUST = (5 * ANG1);

//==============================================================================
//
// P_AdjustPlayerAngle
//
//==============================================================================
procedure P_AdjustPlayerAngle(pmo: Pmobj_t);
var
  angle: angle_t;
  diff: angle_t;
begin
  angle := R_PointToAngle2(pmo.x, pmo.y, linetarget.x, linetarget.y);
  diff := angle - pmo.angle;
  if (diff > MAX_ANGLE_ADJUST) and (diff <= ANGLE_MAX - MAX_ANGLE_ADJUST) then // JVAL SOS
  begin
    if diff < ANG180 then
      pmo.angle := pmo.angle + MAX_ANGLE_ADJUST
    else
      pmo.angle := pmo.angle - MAX_ANGLE_ADJUST;
  end
  else
    pmo.angle := angle;
end;

//==============================================================================
//
// A_SnoutAttack
//
//==============================================================================
procedure A_SnoutAttack(player: Pplayer_t; psp: Ppspdef_t);
var
  angle: angle_t;
  damage: integer;
  slope: integer;
  mrange: integer;
begin
  damage := 3 + (P_Random and 3);
  angle := player.mo.angle;
  mrange := P_GetPlayerMeleeRange(player);
  slope := P_AimLineAttack(player.mo, angle, mrange);
  PuffType := MT_SNOUTPUFF;
  PuffSpawned := nil;
  P_LineAttack(player.mo, angle, mrange, slope, damage);
  S_StartSound(player.mo, Ord(SFX_PIG_ACTIVE1) + (P_Random and 1));
  if linetarget <> nil then
  begin
    P_AdjustPlayerAngle(player.mo);
    if PuffSpawned <> nil then
    begin // Bit something
      S_StartSound(player.mo, Ord(SFX_PIG_ATTACK));
    end;
  end;
end;

//==============================================================================
// HAMMER_RANGE
//
// A_FHammerAttack
//
//==============================================================================
function HAMMER_RANGE(const p: Pplayer_t): fixed_t;
var
  mrange: integer;
begin
  mrange := P_GetPlayerMeleeRange(p);
  result := (mrange + mrange div 2);
end;

//==============================================================================
//
// A_FHammerAttack
//
//==============================================================================
procedure A_FHammerAttack(player: Pplayer_t; psp: Ppspdef_t);
var
  angle: angle_t;
  d_an: angle_t;
  pmo: Pmobj_t;
  damage: integer;
  power: fixed_t;
  slope: integer;
  i: integer;

  procedure hammerdone;
  begin
    if player.mana[Ord(MANA_2)] < WeaponManaUse[Ord(player._class), Ord(player.readyweapon)] then
    begin // Don't spawn a hammer if the player doesn't have enough mana
      pmo.special1 := 0;
    end;
  end;

begin
  pmo := player.mo;
  damage := 60 + (P_Random and 63);
  power := 10 * FRACUNIT;
  PuffType := MT_HAMMERPUFF;
  d_an := 0;
  for i := 0 to 15 do
  begin
    angle := pmo.angle + d_an;
    slope := P_AimLineAttack(pmo, angle, HAMMER_RANGE(player));
    if linetarget <> nil then
    begin
      P_LineAttack(pmo, angle, HAMMER_RANGE(player), slope, damage);
      P_AdjustPlayerAngle(pmo);
      if (linetarget.flags and MF_COUNTKILL <> 0) or (linetarget.player <> nil) then
      begin
        P_ThrustMobj(linetarget, angle, power);
      end;
      pmo.special1 := 0; // Don't throw a hammer
      hammerdone;
      exit;
    end;
    angle := pmo.angle - d_an;
    slope := P_AimLineAttack(pmo, angle, HAMMER_RANGE(player));
    if linetarget <> nil then
    begin
      P_LineAttack(pmo, angle, HAMMER_RANGE(player), slope, damage);
      P_AdjustPlayerAngle(pmo);
      if (linetarget.flags and MF_COUNTKILL <> 0) or (linetarget.player <> nil) then
      begin
        P_ThrustMobj(linetarget, angle, power);
      end;
      pmo.special1 := 0; // Don't throw a hammer
      hammerdone;
      exit;
    end;
    d_an := d_an + (ANG45 div 32);
  end;

  // didn't find any targets in meleerange, so set to throw out a hammer
  PuffSpawned := nil;
  angle := pmo.angle;
  slope := P_AimLineAttack(pmo, angle, HAMMER_RANGE(player));
  P_LineAttack(pmo, angle, HAMMER_RANGE(player), slope, damage);
  if PuffSpawned <> nil then
  begin
    pmo.special1 := 0;
  end
  else
  begin
    pmo.special1 := 1;
  end;

  hammerdone;
end;

//==============================================================================
//
// A_FHammerThrow
//
//==============================================================================
procedure A_FHammerThrow(player: Pplayer_t; psp: Ppspdef_t);
var
  mo: Pmobj_t;
begin
  if player.mo.special1 = 0 then
    exit;

  player.mana[Ord(MANA_2)] := player.mana[Ord(MANA_2)] - WeaponManaUse[Ord(player._class), Ord(player.readyweapon)];
  mo := P_SpawnPlayerMissile(player.mo, Ord(MT_HAMMER_MISSILE));
  if mo <> nil then
    mo.special1 := 0;
end;

//==============================================================================
//
// A_FSwordAttack
//
//==============================================================================
procedure A_FSwordAttack(player: Pplayer_t; psp: Ppspdef_t);
var
  pmo: Pmobj_t;
begin
  player.mana[Ord(MANA_1)] := player.mana[Ord(MANA_1)] - WeaponManaUse[Ord(player._class), Ord(player.readyweapon)];
  player.mana[Ord(MANA_2)] := player.mana[Ord(MANA_2)] - WeaponManaUse[Ord(player._class), Ord(player.readyweapon)];
  pmo := player.mo;
  P_SPMAngleXYZ(pmo, pmo.x, pmo.y, pmo.z - 10 * FRACUNIT, Ord(MT_FSWORD_MISSILE), pmo.angle + ANG45 div 4);
  P_SPMAngleXYZ(pmo, pmo.x, pmo.y, pmo.z - 5 * FRACUNIT, Ord(MT_FSWORD_MISSILE), pmo.angle + ANG45 div 8);
  P_SPMAngleXYZ(pmo, pmo.x, pmo.y, pmo.z, Ord(MT_FSWORD_MISSILE), pmo.angle);
  P_SPMAngleXYZ(pmo, pmo.x, pmo.y, pmo.z + 5 * FRACUNIT, Ord(MT_FSWORD_MISSILE), pmo.angle - ANG45 div 8);
  P_SPMAngleXYZ(pmo, pmo.x, pmo.y, pmo.z + 10 * FRACUNIT, Ord(MT_FSWORD_MISSILE), pmo.angle - ANG45 div 4);
  S_StartSound(pmo, Ord(SFX_FIGHTER_SWORD_FIRE));
end;

//==============================================================================
//
// A_FSwordAttack2
//
//==============================================================================
procedure A_FSwordAttack2(actor: Pmobj_t);
var
  angle: angle_t;
begin
  angle := actor.angle;
  P_SpawnMissileAngle(actor, Ord(MT_FSWORD_MISSILE), angle + ANG45 div 4, 0);
  P_SpawnMissileAngle(actor, Ord(MT_FSWORD_MISSILE), angle + ANG45 div 8, 0);
  P_SpawnMissileAngle(actor, Ord(MT_FSWORD_MISSILE), angle, 0);
  P_SpawnMissileAngle(actor, Ord(MT_FSWORD_MISSILE), angle - ANG45 div 8, 0);
  P_SpawnMissileAngle(actor, Ord(MT_FSWORD_MISSILE), angle - ANG45 div 4, 0);
  S_StartSound(actor, Ord(SFX_FIGHTER_SWORD_FIRE));
end;

//==============================================================================
//
// A_FSwordFlames
//
//==============================================================================
procedure A_FSwordFlames(actor: Pmobj_t);
var
  i: integer;
begin
  for i := 1 + (P_Random and 3) downto 0 do
    P_SpawnMobj(actor.x + (P_Random - 128) * 4096,
                actor.y + (P_Random - 128) * 4096,
                actor.z + (P_Random - 128) * 2048,
                Ord(MT_FSWORD_FLAME));
end;

//==============================================================================
//
// A_MWandAttack
//
//==============================================================================
procedure A_MWandAttack(player: Pplayer_t; psp: Ppspdef_t);
var
  mo: Pmobj_t;
begin
  mo := P_SpawnPlayerMissile(player.mo, Ord(MT_MWAND_MISSILE));
  if mo <> nil then
    mo.thinker._function.acp1 := @P_BlasterMobjThinker;
  S_StartSound(player.mo, Ord(SFX_MAGE_WAND_FIRE));
end;

// ===== Mage Lightning Weapon =====

//==============================================================================
//
// A_LightningReady
//
//==============================================================================
procedure A_LightningReady(player: Pplayer_t; psp: Ppspdef_t);
begin
  A_WeaponReady(player, psp);
  if P_Random < 160 then
    S_StartSound(player.mo, Ord(SFX_MAGE_LIGHTNING_READY));
end;

//
// A_LightningClip
//

const
  ZAGSPEED = FRACUNIT;

//==============================================================================
//
// A_LightningClip
//
//==============================================================================
procedure A_LightningClip(actor: Pmobj_t);
var
  cMo: Pmobj_t;
  target: Pmobj_t;
  zigZag: integer;
begin
  target := nil;

  if actor._type = Ord(MT_LIGHTNING_FLOOR) then
  begin
    actor.z := actor.floorz;
    target := Pmobj_t(Pmobj_t(actor.special2).special1);

    // floor lightning zig-zags, and forces the ceiling lightning to mimic
    cMo := Pmobj_t(actor.special2);
    zigZag := P_Random;
    if ((zigZag > 128) and (actor.special1 < 2)) or (actor.special1 < -2) then
    begin
      P_ThrustMobj(actor, actor.angle + ANG90, ZAGSPEED);
      if cMo <> nil then
        P_ThrustMobj(cMo, actor.angle + ANG90, ZAGSPEED);
      inc(actor.special1);
    end
    else
    begin
      P_ThrustMobj(actor, actor.angle - ANG90, ZAGSPEED);
      if cMo <> nil then
        P_ThrustMobj(cMo, cMo.angle - ANG90, ZAGSPEED);
      dec(actor.special1);
    end;

  end
  else if actor._type = Ord(MT_LIGHTNING_CEILING) then
  begin
    actor.z := actor.ceilingz - actor.height;
    target := Pmobj_t(actor.special1);
  end;

  if target <> nil then
  begin
    if target.health <= 0 then
    begin
      P_ExplodeMissile(actor);
    end
    else
    begin
      actor.angle := R_PointToAngle2(actor.x, actor.y, target.x, target.y);
      actor.momx := 0;
      actor.momy := 0;
      P_ThrustMobj(actor, actor.angle, _SHR1(actor.info.speed));
    end;
  end;
end;

//==============================================================================
//
// A_LightningZap
//
//==============================================================================
procedure A_LightningZap(actor: Pmobj_t);
var
  mo: Pmobj_t;
  deltaZ: fixed_t;
begin
  A_LightningClip(actor);

  actor.health := actor.health - 8;
  if actor.health <= 0 then
  begin
    P_SetMobjState(actor, statenum_t(actor.info.deathstate));
    exit;
  end;

  if actor._type = Ord(MT_LIGHTNING_FLOOR) then
    deltaZ := 10 * FRACUNIT
  else
    deltaZ := -10 * FRACUNIT;

  mo := P_SpawnMobj(actor.x + (P_Random - 128) * actor.radius div 256,
                    actor.y + (P_Random - 128) * actor.radius div 256,
                    actor.z + deltaZ,
                    Ord(MT_LIGHTNING_ZAP));
  if mo <> nil then
  begin
    mo.special2 := integer(actor);
    mo.momx := actor.momx;
    mo.momy := actor.momy;
    mo.target := actor.target;
    if actor._type = Ord(MT_LIGHTNING_FLOOR) then
      mo.momz := 20 * FRACUNIT
    else
      mo.momz := -20 * FRACUNIT;
  end;

  if (actor._type = Ord(MT_LIGHTNING_FLOOR)) and (P_Random < 160) then
    S_StartSound(actor, Ord(SFX_MAGE_LIGHTNING_CONTINUOUS));
end;

//==============================================================================
//
// A_MLightningAttack2
//
//==============================================================================
procedure A_MLightningAttack2(actor: Pmobj_t);
var
  fmo, cmo: Pmobj_t;
begin
  fmo := P_SpawnPlayerMissile(actor, Ord(MT_LIGHTNING_FLOOR));
  cmo := P_SpawnPlayerMissile(actor, Ord(MT_LIGHTNING_CEILING));

  if fmo <> nil then
  begin
    fmo.special1 := 0;
    fmo.special2 := integer(cmo);
    A_LightningZap(fmo);
  end;

  if cmo <> nil then
  begin
    cmo.special1 := 0;  // mobj that it will track
    cmo.special2 := integer(fmo);
    A_LightningZap(cmo);
  end;

  S_StartSound(actor, Ord(SFX_MAGE_LIGHTNING_FIRE));
end;

//==============================================================================
//
// A_MLightningAttack
//
//==============================================================================
procedure A_MLightningAttack(player: Pplayer_t; psp: Ppspdef_t);
begin
  A_MLightningAttack2(player.mo);
  player.mana[Ord(MANA_2)] := player.mana[Ord(MANA_2)] - WeaponManaUse[Ord(player._class), Ord(player.readyweapon)];
end;

//==============================================================================
//
// A_ZapMimic
//
//==============================================================================
procedure A_ZapMimic(actor: Pmobj_t);
var
  mo: Pmobj_t;
begin
  mo := Pmobj_t(actor.special2);
  if mo <> nil then
  begin
    if (LongWord(mo.state) >= LongWord(@states[mo.info.deathstate])) or
       (mo.state = @states[Ord(S_FREETARGMOBJ)]) then
    begin
      P_ExplodeMissile(actor);
    end
    else
    begin
      actor.momx := mo.momx;
      actor.momy := mo.momy;
    end;
  end;
end;

//==============================================================================
//
// A_LastZap
//
//==============================================================================
procedure A_LastZap(actor: Pmobj_t);
var
  mo: Pmobj_t;
begin
  mo := P_SpawnMobj(actor.x, actor.y, actor.z, Ord(MT_LIGHTNING_ZAP));
  if mo <> nil then
  begin
    P_SetMobjState(mo, S_LIGHTNING_ZAP_X1);
    mo.momz := 40 * FRACUNIT;
  end;
end;

//==============================================================================
//
// A_LightningRemove
//
//==============================================================================
procedure A_LightningRemove(actor: Pmobj_t);
var
  mo: Pmobj_t;
begin
  mo := Pmobj_t(actor.special2);
  if mo <> nil then
  begin
    mo.special2 := 0;
    P_ExplodeMissile(mo);
  end;
end;

//==============================================================================
//
// P_MStaffSpawn
//
//==============================================================================
procedure P_MStaffSpawn(pmo: Pmobj_t; angle: angle_t);
var
  mo: Pmobj_t;
begin
  mo := P_SPMAngle(pmo, Ord(MT_MSTAFF_FX2), angle);
  if mo <> nil then
  begin
    mo.target := pmo;
    mo.special1 := integer(P_RoughMonsterSearch(mo, 10));
  end;
end;

//==============================================================================
//
// A_MStaffAttack
//
//==============================================================================
procedure A_MStaffAttack(player: Pplayer_t; psp: Ppspdef_t);
var
  angle: angle_t;
  pmo: Pmobj_t;
  palette, pal: PByteArray;
begin
  player.mana[Ord(MANA_1)] := player.mana[Ord(MANA_1)] - WeaponManaUse[Ord(player._class), Ord(player.readyweapon)];
  player.mana[Ord(MANA_2)] := player.mana[Ord(MANA_2)] - WeaponManaUse[Ord(player._class), Ord(player.readyweapon)];
  pmo := player.mo;
  angle := pmo.angle;

  P_MStaffSpawn(pmo, angle);
  P_MStaffSpawn(pmo, angle - ANG5);
  P_MStaffSpawn(pmo, angle + ANG5);
  S_StartSound(player.mo, Ord(SFX_MAGE_STAFF_FIRE));
  if player = @players[consoleplayer] then
  begin
    player.damagecount := 0;
    player.bonuscount := 0;

    palette := V_ReadPalette(PU_STATIC);
    pal := @palette[STARTSCOURGEPAL * 768];
    I_SetPalette(pal);
    V_SetPalette(pal);
    Z_ChangeTag(palette, PU_CACHE);
    R_SetPalette(STARTSCOURGEPAL);
  end;
end;

//==============================================================================
//
// A_MStaffPalette
//
//==============================================================================
procedure A_MStaffPalette(player: Pplayer_t; psp: Ppspdef_t);
var
  pl: integer;
  palette, pal: PByteArray;
begin
  if player = @players[consoleplayer] then
  begin
    pl := STARTSCOURGEPAL + (integer(psp.state) - integer(@states[Ord(S_MSTAFFATK_2)])) div SizeOf(state_t);
    R_SetPalette(pl);
    if pl = STARTSCOURGEPAL + 3 then
    begin // reset back to original playpal
      pl := 0;
    end;
    palette := V_ReadPalette(PU_STATIC);
    pal := @palette[pl * 768];
    I_SetPalette(pal);
    V_SetPalette(pal);
    Z_ChangeTag(palette, PU_CACHE);
    R_SetPalette(pl);
  end;
end;

//==============================================================================
//
// A_MStaffWeave
//
//==============================================================================
procedure A_MStaffWeave(actor: Pmobj_t);
var
  newX, newY: fixed_t;
  weaveXY, weaveZ: integer;
  angle: integer;
begin
  weaveXY := FixedInt(actor.special2);
  weaveZ := actor.special2 and $FFFF;
  angle := (actor.angle + ANG90) shr ANGLETOFINESHIFT;
  newX := actor.x - FixedMul(finecosine[angle], FloatBobOffsets[weaveXY] * 4);
  newY := actor.y - FixedMul(finesine[angle], FloatBobOffsets[weaveXY] * 4);
  weaveXY := (weaveXY + 6) and 63;
  newX := newX + FixedMul(finecosine[angle], FloatBobOffsets[weaveXY] * 4);
  newY := newY + FixedMul(finesine[angle], FloatBobOffsets[weaveXY] * 4);
  P_TryMove(actor, newX, newY);
  actor.z := actor.z - FloatBobOffsets[weaveZ] * 2;
  weaveZ := (weaveZ + 3) and 63;
  actor.z := actor.z + FloatBobOffsets[weaveZ] * 2;
  if actor.z <= actor.floorz then
    actor.z := actor.floorz + FRACUNIT;

  actor.special2 := weaveZ + (weaveXY * FRACUNIT);
end;

//==============================================================================
//
// A_MStaffTrack
//
//==============================================================================
procedure A_MStaffTrack(actor: Pmobj_t);
begin
  if (actor.special1 = 0) and (P_Random < 50) then
    actor.special1 := integer(P_RoughMonsterSearch(actor, 10));
  P_SeekerMissile(actor, ANG1 * 2, ANG1 * 10);
end;

//==============================================================================
//
// P_MStaffSpawn2 - for use by mage class boss
//
//==============================================================================
procedure P_MStaffSpawn2(actor: Pmobj_t; angle: angle_t);
var
  mo: Pmobj_t;
begin
  mo := P_SpawnMissileAngle(actor, Ord(MT_MSTAFF_FX2), angle, 0);
  if mo <> nil then
  begin
    mo.target := actor;
    mo.special1 := integer(P_RoughMonsterSearch(mo, 10));
  end;
end;

//==============================================================================
//
// A_MStaffAttack2 - for use by mage class boss
//
//==============================================================================
procedure A_MStaffAttack2(actor: Pmobj_t);
var
  angle: angle_t;
begin
  angle := actor.angle;
  P_MStaffSpawn2(actor, angle);
  P_MStaffSpawn2(actor, angle - ANG5);
  P_MStaffSpawn2(actor, angle + ANG5);
  S_StartSound(actor, Ord(SFX_MAGE_STAFF_FIRE));
end;

//==============================================================================
//
// A_FPunchAttack
//
//==============================================================================
procedure A_FPunchAttack(player: Pplayer_t; psp: Ppspdef_t);
var
  angle: angle_t;
  damage: integer;
  slope: integer;
  pmo: Pmobj_t;
  power: fixed_t;
  i: integer;
  d_an: angle_t;
  mrange: integer;

  procedure punchdone;
  begin
    if pmo.special1 = 3 then
    begin
      pmo.special1 := 0;
      P_SetPsprite(player, Ord(ps_weapon), S_PUNCHATK2_1);
      S_StartSound(pmo, Ord(SFX_FIGHTER_GRUNT));
    end;
  end;

begin
  pmo := player.mo;
  damage := 40 + (P_Random and 15);
  power := 2 * FRACUNIT;
  PuffType := MT_PUNCHPUFF;
  d_an := 0;
  mrange := P_GetPlayerMeleeRange(player);
  for i := 0 to 15 do
  begin
    angle := pmo.angle + d_an;
    slope := P_AimLineAttack(pmo, angle, 2 * mrange);
    if linetarget <> nil then
    begin
      inc(player.mo.special1);
      if pmo.special1 = 3 then
      begin
        damage := damage * 2;
        power := 6 * FRACUNIT;
        PuffType := MT_HAMMERPUFF;
      end;
      P_LineAttack(pmo, angle, 2 * mrange, slope, damage);
      if (linetarget.flags and MF_COUNTKILL <> 0) or (linetarget.player <> nil) then
        P_ThrustMobj(linetarget, angle, power);
      P_AdjustPlayerAngle(pmo);
      punchdone;
      exit;
    end;
    angle := pmo.angle - d_an;
    slope := P_AimLineAttack(pmo, angle, 2 * mrange);
    if linetarget <> nil then
    begin
      inc(pmo.special1);
      if pmo.special1 = 3 then
      begin
        damage := damage * 2;
        power := 6 * FRACUNIT;
        PuffType := MT_HAMMERPUFF;
      end;
      P_LineAttack(pmo, angle, 2 * mrange, slope, damage);
      if (linetarget.flags and MF_COUNTKILL <> 0) or (linetarget.player <> nil) then
        P_ThrustMobj(linetarget, angle, power);
      P_AdjustPlayerAngle(pmo);
      punchdone;
      exit;
    end;
    d_an := d_an + (ANG45 div 16);
  end;
  // didn't find any creatures, so try to strike any walls
  pmo.special1 := 0;

  angle := pmo.angle;
  slope := P_AimLineAttack(pmo, angle, mrange);
  P_LineAttack(pmo, angle, mrange, slope, damage);

  punchdone;
end;

//==============================================================================
// AXERANGE
//
// A_FAxeAttack
//
//==============================================================================
function AXERANGE(const p: Pplayer_t): fixed_t;
var
  mrange: integer;
begin
  mrange := P_GetPlayerMeleeRange(p);
  result := 10 * mrange div 4;
end;

//==============================================================================
//
// A_FAxeAttack
//
//==============================================================================
procedure A_FAxeAttack(player: Pplayer_t; psp: Ppspdef_t);
var
  angle: angle_t;
  d_an: angle_t;
  pmo: Pmobj_t;
  power: fixed_t;
  damage: integer;
  slope: integer;
  i: integer;
  useMana: integer;
  mrange: integer;

  procedure axedone;
  begin
    if useMana = 2 then
    begin
      player.mana[Ord(MANA_1)] := player.mana[Ord(MANA_1)] - WeaponManaUse[Ord(player._class), Ord(player.readyweapon)];
      if player.mana[Ord(MANA_1)] <= 0 then
        P_SetPsprite(player, Ord(ps_weapon), S_FAXEATK_5);
    end;
  end;

begin
  pmo := player.mo;
  damage := 40 + (P_Random and 15) + (P_Random and 7);
  power := 0;
  if player.mana[Ord(MANA_1)] > 0 then
  begin
    damage := damage * 2;
    power := 6 * FRACUNIT;
    PuffType := MT_AXEPUFF_GLOW;
    useMana := 1;
  end
  else
  begin
    PuffType := MT_AXEPUFF;
    useMana := 0;
  end;

  d_an := 0;
  for i := 0 to 15 do
  begin
    angle := pmo.angle + d_an;
    slope := P_AimLineAttack(pmo, angle, AXERANGE(player));
    if linetarget <> nil then
    begin
      P_LineAttack(pmo, angle, AXERANGE(player), slope, damage);
      if (linetarget.flags and MF_COUNTKILL <> 0) or (linetarget.player <> nil) then
        P_ThrustMobj(linetarget, angle, power);
      P_AdjustPlayerAngle(pmo);
      inc(useMana);
      axedone;
      exit;
    end;
    angle := pmo.angle - d_an;
    slope := P_AimLineAttack(pmo, angle, AXERANGE(player));
    if linetarget <> nil then
    begin
      P_LineAttack(pmo, angle, AXERANGE(player), slope, damage);
      if linetarget.flags and MF_COUNTKILL <> 0 then
        P_ThrustMobj(linetarget, angle, power);
      P_AdjustPlayerAngle(pmo);
      inc(useMana);
      axedone;
      exit;
    end;
    d_an := d_an + (ANG45 div 16);
  end;
  // didn't find any creatures, so try to strike any walls
  pmo.special1 := 0;

  angle := pmo.angle;
  mrange := P_GetPlayerMeleeRange(player);
  slope := P_AimLineAttack(pmo, angle, mrange);
  P_LineAttack(pmo, angle, mrange, slope, damage);

  axedone;
end;

//==============================================================================
//
// A_CMaceAttack
//
//==============================================================================
procedure A_CMaceAttack(player: Pplayer_t; psp: Ppspdef_t);
var
  angle: angle_t;
  d_an: angle_t;
  damage: integer;
  slope: integer;
  i: integer;
  mrange: integer;
begin
  damage := 25 + (P_Random and 15);
  PuffType := MT_HAMMERPUFF;
  d_an := 0;
  mrange := P_GetPlayerMeleeRange(player);
  for i := 0 to 15 do
  begin
    angle := player.mo.angle + d_an;
    slope := P_AimLineAttack(player.mo, angle, 2 * mrange);
    if linetarget <> nil then
    begin
      P_LineAttack(player.mo, angle, 2 * mrange, slope, damage);
      P_AdjustPlayerAngle(player.mo);
      exit;
    end;
    angle := player.mo.angle - d_an;
    slope := P_AimLineAttack(player.mo, angle, 2 * mrange);
    if linetarget <> nil then
    begin
      P_LineAttack(player.mo, angle, 2 * mrange, slope, damage);
      P_AdjustPlayerAngle(player.mo);
      exit;
    end;
    d_an := d_an + (ANG45 div 16);
  end;
  // didn't find any creatures, so try to strike any walls
  player.mo.special1 := 0;

  angle := player.mo.angle;
  slope := P_AimLineAttack(player.mo, angle, mrange);
  P_LineAttack(player.mo, angle, mrange, slope, damage);
end;

//==============================================================================
//
// A_CStaffCheck
//
//==============================================================================
procedure A_CStaffCheck(player: Pplayer_t; psp: Ppspdef_t);
var
  pmo: Pmobj_t;
  damage: integer;
  newLife: integer;
  angle: angle_t;
  d_an: angle_t;
  slope: integer;
  i: integer;
  mrange: integer;
begin
  pmo := player.mo;
  damage := 20 + (P_Random and 15);
  PuffType := MT_CSTAFFPUFF;
  d_an := 0;
  mrange := 3 * P_GetPlayerMeleeRange(player) div 2;
  for i := 0 to 2 do
  begin
    angle := pmo.angle + d_an;
    slope := P_AimLineAttack(pmo, angle, mrange);
    if linetarget <> nil then
    begin
      P_LineAttack(pmo, angle, mrange, slope, damage);
      pmo.angle := R_PointToAngle2(pmo.x, pmo.y, linetarget.x, linetarget.y);
      if ((linetarget.player <> nil) or (linetarget.flags and MF_COUNTKILL <> 0)) and
         (linetarget.flags2 and (MF2_DORMANT + MF2_INVULNERABLE) = 0) then
      begin
        newLife := player.health + _SHR3(damage);
        if newLife > 100 then
          newlife := 100;
        pmo.health := newLife;
        player.health := newLife;
        P_SetPsprite(player, Ord(ps_weapon), S_CSTAFFATK2_1);
      end;
      player.mana[Ord(MANA_1)] := player.mana[Ord(MANA_1)] - WeaponManaUse[Ord(player._class), Ord(player.readyweapon)];
      break;
    end;
    angle := pmo.angle - d_an;
    slope := P_AimLineAttack(player.mo, angle, mrange);
    if linetarget <> nil then
    begin
      P_LineAttack(pmo, angle, mrange, slope, damage);
      pmo.angle := R_PointToAngle2(pmo.x, pmo.y, linetarget.x, linetarget.y);
      if (linetarget.player <> nil) or (linetarget.flags and MF_COUNTKILL <> 0) then
      begin
        newLife := player.health + _SHR4(damage);
        if newLife > 100 then
          newLife := 100;
        pmo.health := newLife;
        player.health := newLife;
        P_SetPsprite(player, Ord(ps_weapon), S_CSTAFFATK2_1);
      end;
      player.mana[Ord(MANA_1)] := player.mana[Ord(MANA_1)] - WeaponManaUse[Ord(player._class), Ord(player.readyweapon)];
      break;
    end;
    d_an := d_an + (ANG45 div 16);
  end;
end;

//==============================================================================
//
// A_CStaffAttack
//
//==============================================================================
procedure A_CStaffAttack(player: Pplayer_t; psp: Ppspdef_t);
var
  mo: Pmobj_t;
  pmo: Pmobj_t;
begin
  player.mana[Ord(MANA_1)] := player.mana[Ord(MANA_1)] -  WeaponManaUse[Ord(player._class), Ord(player.readyweapon)];
  pmo := player.mo;
  mo := P_SPMAngle(pmo, Ord(MT_CSTAFF_MISSILE), pmo.angle - (ANG45 div 15));
  if mo <> nil then
    mo.special2 := 32;
  mo := P_SPMAngle(pmo, Ord(MT_CSTAFF_MISSILE), pmo.angle + (ANG45 div 15));
  if mo <> nil then
    mo.special2 := 0;
  S_StartSound(player.mo, Ord(SFX_CLERIC_CSTAFF_FIRE));
end;

//==============================================================================
//
// A_CStaffInitBlink
//
//==============================================================================
procedure A_CStaffInitBlink(player: Pplayer_t; psp: Ppspdef_t);
begin
  player.mo.special1 := _SHR1(P_Random) + 20;
end;

//==============================================================================
//
// A_CStaffCheckBlink
//
//==============================================================================
procedure A_CStaffCheckBlink(player: Pplayer_t; psp: Ppspdef_t);
begin
  if player.mo.special1 > 0 then
    dec(player.mo.special1);
  if player.mo.special1 = 0 then
  begin
    P_SetPsprite(player, Ord(ps_weapon), S_CSTAFFBLINK1);
    player.mo.special1 := _SHR2(P_Random + 50);
  end;
end;

//
// A_CFlameAttack
//

const
  FLAMESPEED = 9 * FRACUNIT div 20;
  CFLAMERANGE = 12 * 64 * FRACUNIT;

//==============================================================================
//
// A_CFlameAttack
//
//==============================================================================
procedure A_CFlameAttack(player: Pplayer_t; psp: Ppspdef_t);
var
  mo: Pmobj_t;
begin
  mo := P_SpawnPlayerMissile(player.mo, Ord(MT_CFLAME_MISSILE));
  if mo <> nil then
  begin
    mo.thinker._function.acp1 := @P_BlasterMobjThinker;
    mo.special1 := 2;
  end;

  player.mana[Ord(MANA_2)] := player.mana[Ord(MANA_2)] - WeaponManaUse[Ord(player._class), Ord(player.readyweapon)];
  S_StartSound(player.mo, Ord(SFX_CLERIC_FLAME_FIRE));
end;

//==============================================================================
//
// A_CFlamePuff
//
//==============================================================================
procedure A_CFlamePuff(actor: Pmobj_t);
begin
  A_UnHideThing(actor);
  actor.momx := 0;
  actor.momy := 0;
  actor.momz := 0;
  S_StartSound(actor, Ord(SFX_CLERIC_FLAME_EXPLODE));
end;

//==============================================================================
//
// A_CFlameMissile
//
//==============================================================================
procedure A_CFlameMissile(actor: Pmobj_t);
var
  i: integer;
  an: angle_t;
  dist: fixed_t;
  mo: Pmobj_t;
begin
  A_UnHideThing(actor);
  S_StartSound(actor, Ord(SFX_CLERIC_FLAME_EXPLODE));
  if (BlockingMobj <> nil) and (BlockingMobj.flags and MF_SHOOTABLE <> 0) then
  begin // Hit something, so spawn the flame circle around the thing
    dist := BlockingMobj.radius + 18 * FRACUNIT;
    for i := 0 to 3 do
    begin
      an := (i * ANG45) shr ANGLETOFINESHIFT;

      mo := P_SpawnMobj(BlockingMobj.x + FixedMul(dist, finecosine[an]),
                        BlockingMobj.y + FixedMul(dist, finesine[an]),
                        BlockingMobj.z + 5 * FRACUNIT,
                        Ord(MT_CIRCLEFLAME));
      if mo <> nil then
      begin
        mo.angle := an shl ANGLETOFINESHIFT;
        mo.target := actor.target;
        mo.momx := FixedMul(FLAMESPEED, finecosine[an]);
        mo.special1 := mo.momx;
        mo.momy := FixedMul(FLAMESPEED, finesine[an]);
        mo.special2 := mo.momy;
        mo.tics := mo.tics - (P_Random and 3);
      end;

      mo := P_SpawnMobj(BlockingMobj.x - FixedMul(dist, finecosine[an]),
                        BlockingMobj.y - FixedMul(dist, finesine[an]),
                        BlockingMobj.z + 5 * FRACUNIT,
                        Ord(MT_CIRCLEFLAME));
      if mo <> nil then
      begin
        mo.angle := ANG180 + (an shl ANGLETOFINESHIFT);
        mo.target := actor.target;
        mo.momx := FixedMul(-FLAMESPEED, finecosine[an]);
        mo.special1 := mo.momx;
        mo.momy := FixedMul(-FLAMESPEED, finesine[an]);
        mo.special2 := mo.momy;
        mo.tics := mo.tics - (P_Random and 3);
      end;
    end;
    P_SetMobjState(actor, S_FLAMEPUFF2_1);
  end;
end;

//
// A_CFlameRotate
//

const
  FLAMEROTSPEED = 2 * FRACUNIT;

//==============================================================================
//
// A_CFlameRotate
//
//==============================================================================
procedure A_CFlameRotate(actor: Pmobj_t);
var
  an: angle_t;
begin
  an := (actor.angle + ANG90) shr ANGLETOFINESHIFT;
  actor.momx := actor.special1 + FixedMul(FLAMEROTSPEED, finecosine[an]);
  actor.momy := actor.special2 + FixedMul(FLAMEROTSPEED, finesine[an]);
  actor.angle := actor.angle + ANG90 div 15;
end;

//==============================================================================
//
// A_CHolyAttack3
//
//   Spawns the spirits
//
//==============================================================================
procedure A_CHolyAttack3(actor: Pmobj_t);
begin
  P_SpawnMissile(actor, actor.target, Ord(MT_HOLY_MISSILE));
  S_StartSound(actor, Ord(SFX_CHOLY_FIRE));
end;

//==============================================================================
//
// A_CHolyAttack2
//
//   Spawns the spirits
//
//==============================================================================
procedure A_CHolyAttack2(actor: Pmobj_t);
var
  i, j: integer;
  mo: Pmobj_t;
  tail, next: Pmobj_t;
  d_an: angle_t;
begin
  d_an := 0;
  for j := 0 to 3 do
  begin
    mo := P_SpawnMobj(actor.x, actor.y, actor.z, Ord(MT_HOLY_FX));
    if mo = nil then
    begin
      d_an := d_an + ANG45;
      continue;
    end;

    case j of // float bob index
      0: mo.special2 := P_Random and 7; // upper-left
      1: mo.special2 := 32 + (P_Random and 7); // upper-right
      2: mo.special2 := (32 + (P_Random and 7)) * FRACUNIT; // lower-left
      3: mo.special2 := ((32 + (P_Random and 7)) * FRACUNIT) + 32 + (P_Random and 7);
    end;
    mo.z := actor.z;
    mo.angle := actor.angle + (ANG45 + ANG45 div 2) - d_an;
    P_ThrustMobj(mo, mo.angle, mo.info.speed);
    mo.target := actor.target;
    mo.args[0] := 10; // initial turn value
    mo.args[1] := 0; // initial look angle

    if deathmatch <> 0 then
    begin // Ghosts last slightly less longer in DeathMatch
      mo.health := 85;
    end;

    if linetarget <> nil then
    begin
      mo.special1 := integer(linetarget);
      mo.flags := mo.flags or MF_NOCLIP or MF_SKULLFLY;
      mo.flags := mo.flags and not MF_MISSILE;
    end;

    tail := P_SpawnMobj(mo.x, mo.y, mo.z, Ord(MT_HOLY_TAIL));
    tail.special2 := integer(mo); // parent
    for i := 0 to 1 do
    begin
      next := P_SpawnMobj(mo.x, mo.y, mo.z, Ord(MT_HOLY_TAIL));
      P_SetMobjState(next, statenum_t(next.info.spawnstate + 1));
      tail.special1 := integer(next);
      tail := next;
    end;
    tail.special1 := 0; // last tail bit
    d_an := d_an + ANG45;
  end;
end;

//==============================================================================
//
// A_CHolyAttack
//
//==============================================================================
procedure A_CHolyAttack(player: Pplayer_t; psp: Ppspdef_t);
var
  palette, pal: PByteArray;
begin
  player.mana[Ord(MANA_1)] := player.mana[Ord(MANA_1)] - WeaponManaUse[Ord(player._class), Ord(player.readyweapon)];
  player.mana[Ord(MANA_2)] := player.mana[Ord(MANA_2)] - WeaponManaUse[Ord(player._class), Ord(player.readyweapon)];
  P_SpawnPlayerMissile(player.mo, Ord(MT_HOLY_MISSILE));
  if player = @players[consoleplayer] then
  begin
    player.damagecount := 0;
    player.bonuscount := 0;

    palette := V_ReadPalette(PU_STATIC);
    pal := @palette[STARTHOLYPAL * 768];
    I_SetPalette(pal);
    V_SetPalette(pal);
    Z_ChangeTag(palette, PU_CACHE);
    R_SetPalette(STARTHOLYPAL);
  end;
  S_StartSound(player.mo, Ord(SFX_CHOLY_FIRE));
end;

//==============================================================================
//
// A_CHolyPalette
//
//==============================================================================
procedure A_CHolyPalette(player: Pplayer_t; psp: Ppspdef_t);
var
  pl: integer;
  palette, pal: PByteArray;
begin
  if player = @players[consoleplayer] then
  begin
    pl := STARTHOLYPAL + (integer(psp.state) - integer(@states[Ord(S_CHOLYATK_6)])) div SizeOf(state_t);
    if pl = STARTHOLYPAL + 3 then
    begin // reset back to original playpal
      pl := 0;
    end;
    palette := V_ReadPalette(PU_STATIC);
    pal := @palette[pl * 768];
    I_SetPalette(pal);
    V_SetPalette(pal);
    Z_ChangeTag(palette, PU_CACHE);
    R_SetPalette(pl);
  end;
end;

//==============================================================================
//
// P_CHolyFindTarget
//
//==============================================================================
procedure P_CHolyFindTarget(actor: Pmobj_t);
var
  target: Pmobj_t;
begin
  target := P_RoughMonsterSearch(actor, 6);
  if target <> nil then
  begin
    actor.special1 := integer(target);
    actor.flags := actor.flags or MF_NOCLIP or MF_SKULLFLY;
    actor.flags := actor.flags and not MF_MISSILE;
  end;
end;

//==============================================================================
//
// P_CHolySeekerMissile
//
//    Similar to P_SeekerMissile, but seeks to a random Z on the target
//
//==============================================================================
procedure P_CHolySeekerMissile(actor: Pmobj_t; thresh: angle_t; turnMax: angle_t);
var
  dir: integer;
  dist: integer;
  delta: angle_t;
  angle: angle_t;
  target: Pmobj_t;
  newZ: fixed_t;
  deltaZ: fixed_t;
begin
  target := Pmobj_t(actor.special1);
  if target = nil then
    exit;

  if (target.flags and MF_SHOOTABLE = 0) or
     ((target.flags and MF_COUNTKILL = 0) and (target.player = nil)) then
  begin // Target died/target isn't a player or creature
    actor.special1 := 0;
    actor.flags := actor.flags and not (MF_NOCLIP or MF_SKULLFLY);
    actor.flags := actor.flags or MF_MISSILE;
    P_CHolyFindTarget(actor);
    exit;
  end;

  dir := P_FaceMobj(actor, target, delta);
  if delta > thresh then
  begin
    delta := _SHR1(delta);
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
  actor.momx := FixedMul(actor.info.speed, finecosine[angle]);
  actor.momy := FixedMul(actor.info.speed, finesine[angle]);
  if (leveltime and 15 = 0) or
     (actor.z > target.z + target.height) or
     (actor.z + actor.height < target.z) then
  begin
    newZ := target.z + _SHR8(P_Random * target.height);
    deltaZ := newZ - actor.z;
    if abs(deltaZ) > 15 * FRACUNIT then
    begin
      if deltaZ > 0 then
      begin
        deltaZ := 15 * FRACUNIT;
      end
      else
      begin
        deltaZ := -15 * FRACUNIT;
      end;
    end;
    dist := P_AproxDistance(target.x - actor.x, target.y - actor.y);
    if actor.info.speed = 0 then
      dist := 1
    else
    begin
      dist := dist div actor.info.speed;
      if dist < 1 then
        dist := 1;
    end;
    actor.momz := deltaZ div dist;
  end;
end;

//==============================================================================
//
// A_CHolyWeave
//
//==============================================================================
procedure A_CHolyWeave(actor: Pmobj_t);
var
  newX, newY: fixed_t;
  weaveXY, weaveZ: integer;
  angle: angle_t;
begin
  weaveXY := FixedInt(actor.special2);
  weaveZ := actor.special2 and $FFFF;
  angle := (actor.angle + ANG90) shr ANGLETOFINESHIFT;
  newX := actor.x - FixedMul(finecosine[angle], FloatBobOffsets[weaveXY] * 4);
  newY := actor.y - FixedMul(finesine[angle], FloatBobOffsets[weaveXY] * 4);
  weaveXY := (weaveXY + (P_Random mod 5)) and 63;
  newX := newX + FixedMul(finecosine[angle], FloatBobOffsets[weaveXY] * 4);
  newY := newY + FixedMul(finesine[angle], FloatBobOffsets[weaveXY] * 4);
  P_TryMove(actor, newX, newY);
  actor.z := actor.z - FloatBobOffsets[weaveZ] * 2;
  weaveZ := (weaveZ + (P_Random mod 5)) and 63;
  actor.z := actor.z + FloatBobOffsets[weaveZ] * 2;
  actor.special2 := weaveZ + (weaveXY * FRACUNIT);
end;

//==============================================================================
//
// A_CHolySeek
//
//==============================================================================
procedure A_CHolySeek(actor: Pmobj_t);
begin
  dec(actor.health);
  if actor.health <= 0 then
  begin
    actor.momx := _SHR2(actor.momx);
    actor.momy := _SHR2(actor.momy);
    actor.momz := 0;
    P_SetMobjState(actor, statenum_t(actor.info.deathstate));
    actor.tics := actor.tics - P_Random and 3;
    exit;
  end;

  if actor.special1 <> 0 then
  begin
    P_CHolySeekerMissile(actor, actor.args[0] * ANG1, actor.args[0] * ANG1 * 2);
    if (leveltime + 7) and 15 <> 0 then
      actor.args[0] := 5 + (P_Random div 20);
  end;
  A_CHolyWeave(actor);
end;

//==============================================================================
//
// P_CHolyTailFollow
//
//==============================================================================
procedure P_CHolyTailFollow(actor: Pmobj_t; dist: fixed_t);
var
  child: Pmobj_t;
  an: angle_t;
  oldDistance, newDistance: fixed_t;
begin
  child := Pmobj_t(actor.special1);
  if child <> nil then
  begin
    an := R_PointToAngle2(actor.x, actor.y, child.x, child.y) shr ANGLETOFINESHIFT;
    oldDistance := P_AproxDistance(child.x - actor.x, child.y - actor.y);
    if P_TryMove(child, actor.x + FixedMul(dist, finecosine[an]), actor.y + FixedMul(dist, finesine[an])) then
    begin
      newDistance := P_AproxDistance(child.x - actor.x, child.y - actor.y) - FRACUNIT;
      if oldDistance < FRACUNIT then
      begin
        if child.z < actor.z then
        begin
          child.z := actor.z - dist;
        end
        else
        begin
          child.z := actor.z + dist;
        end;
      end
      else
      begin
        child.z := actor.z + FixedMul(FixedDiv(newDistance, oldDistance), child.z - actor.z);
      end;
    end;
    P_CHolyTailFollow(child, dist - FRACUNIT);
  end;
end;

//==============================================================================
//
// P_CHolyTailRemove
//
//==============================================================================
procedure P_CHolyTailRemove(actor: Pmobj_t);
var
  child: Pmobj_t;
begin
  child := Pmobj_t(actor.special1);
  if child <> nil then
    P_CHolyTailRemove(child);
  P_RemoveMobj(actor);
end;

//==============================================================================
//
// A_CHolyTail
//
//==============================================================================
procedure A_CHolyTail(actor: Pmobj_t);
var
  parent: Pmobj_t;
begin
  parent := Pmobj_t(actor.special2);

  if parent <> nil then
  begin
    if LongWord(parent.state) >= LongWord(@states[parent.info.deathstate]) then
    begin // Ghost removed, so remove all tail parts
      P_CHolyTailRemove(actor);
      exit;
    end;

    if P_TryMove(actor,
          parent.x - FixedMul(14 * FRACUNIT, finecosine[parent.angle shr ANGLETOFINESHIFT]),
          parent.y - FixedMul(14 * FRACUNIT, finesine[parent.angle shr ANGLETOFINESHIFT])) then
      actor.z := parent.z - 5 * FRACUNIT;
    P_CHolyTailFollow(actor, 10 * FRACUNIT);
  end;
end;

//==============================================================================
//
// A_CHolyCheckScream
//
//==============================================================================
procedure A_CHolyCheckScream(actor: Pmobj_t);
begin
  A_CHolySeek(actor);
  if P_Random < 20 then
    S_StartSound(actor, Ord(SFX_SPIRIT_ACTIVE));

  if actor.special1 = 0 then
    P_CHolyFindTarget(actor);
end;

//==============================================================================
//
// A_CHolySpawnPuff
//
//==============================================================================
procedure A_CHolySpawnPuff(actor: Pmobj_t);
begin
  P_SpawnMobj(actor.x, actor.y, actor.z, Ord(MT_HOLY_MISSILE_PUFF));
end;

//----------------------------------------------------------------------------
//
// PROC A_FireConePL1
//
//----------------------------------------------------------------------------

const
  SHARDSPAWN_LEFT = 1;
  SHARDSPAWN_RIGHT = 2;
  SHARDSPAWN_UP = 4;
  SHARDSPAWN_DOWN = 8;

//==============================================================================
//
// A_FireConePL1
//
//==============================================================================
procedure A_FireConePL1(player: Pplayer_t; psp: Ppspdef_t);
var
  angle: angle_t;
  damage: integer;
  i: integer;
  pmo, mo: Pmobj_t;
  conedone: boolean;
  mrange: integer;
begin
  conedone := false;
  pmo := player.mo;
  player.mana[Ord(MANA_1)] := player.mana[Ord(MANA_1)] - WeaponManaUse[Ord(player._class), Ord(player.readyweapon)];
  S_StartSound(pmo, Ord(SFX_MAGE_SHARDS_FIRE));

  damage := 90 + (P_Random and 15);
  angle := pmo.angle;
  mrange := P_GetPlayerMeleeRange(player);
  for i := 0 to 15 do
  begin
    P_AimLineAttack(pmo, angle, mrange);
    if linetarget <> nil then
    begin
      pmo.flags2 := pmo.flags2 or MF2_ICEDAMAGE;
      P_DamageMobj(linetarget, pmo, pmo, damage);
      pmo.flags2 := pmo.flags2 and not MF2_ICEDAMAGE;
      conedone := true;
      break;
    end;
    angle := angle + (ANG45 div 16);
  end;

  // didn't find any creatures, so fire projectiles
  if not conedone then
  begin
    mo := P_SpawnPlayerMissile(pmo, Ord(MT_SHARDFX1));
    if mo <> nil then
    begin
      mo.special1 := SHARDSPAWN_LEFT or SHARDSPAWN_DOWN or SHARDSPAWN_UP or SHARDSPAWN_RIGHT;
      mo.special2 := 3; // Set sperm count (levels of reproductivity)
      mo.target := pmo;
      mo.args[0] := 3;    // Mark Initial shard as super damage
    end;
  end;
end;

//==============================================================================
//
// A_ShedShard
//
//==============================================================================
procedure A_ShedShard(actor: Pmobj_t);
var
  mo: Pmobj_t;
  spawndir: integer;
  spermcount: integer;
begin
  spermcount := actor.special2;
  if spermcount <= 0 then // No sperm left
    exit;

  actor.special2 := 0;
  spawndir := actor.special1;
  dec(spermcount);

  // every so many calls, spawn a new missile in it's set directions
  if spawndir and SHARDSPAWN_LEFT <> 0 then
  begin
    mo := P_SpawnMissileAngleSpeed(
            actor, Ord(MT_SHARDFX1),
              actor.angle + (ANG45 div 9), 0, (20 + 2 * spermcount) * FRACUNIT);
    if mo <> nil then
    begin
      mo.special1 := SHARDSPAWN_LEFT;
      mo.special2 := spermcount;
      mo.momz := actor.momz;
      mo.target := actor.target;
      if spermcount = 3 then
        mo.args[0] := 2
      else
        mo.args[0] := 0;
    end;
  end;

  if spawndir and SHARDSPAWN_RIGHT <> 0 then
  begin
    mo := P_SpawnMissileAngleSpeed(
            actor, Ord(MT_SHARDFX1), actor.angle - (ANG45 div 9), 0, (20 + 2 * spermcount) * FRACUNIT);
    if mo <> nil then
    begin
      mo.special1 := SHARDSPAWN_RIGHT;
      mo.special2 := spermcount;
      mo.momz := actor.momz;
      mo.target := actor.target;
      if spermcount = 3 then
        mo.args[0] := 2
      else
        mo.args[0] := 0;
    end;
  end;

  if spawndir and SHARDSPAWN_UP <> 0 then
  begin
    mo := P_SpawnMissileAngleSpeed(
            actor, Ord(MT_SHARDFX1), actor.angle, 0, (15 + 2 * spermcount) * FRACUNIT);
    if mo <> nil then
    begin
      mo.momz := actor.momz;
      mo.z := mo.z + 8 * FRACUNIT;
      if spermcount and 1 <> 0 then // Every other reproduction
        mo.special1 := SHARDSPAWN_UP or SHARDSPAWN_LEFT or SHARDSPAWN_RIGHT
      else
        mo.special1 := SHARDSPAWN_UP;
      mo.special2 := spermcount;
      mo.target := actor.target;
      if spermcount = 3 then
        mo.args[0] := 2
      else
        mo.args[0] := 0;
    end;
  end;

  if spawndir and SHARDSPAWN_DOWN <> 0 then
  begin
    mo := P_SpawnMissileAngleSpeed(
            actor, Ord(MT_SHARDFX1),
              actor.angle, 0, (15 + 2 * spermcount) * FRACUNIT);
    if mo <> nil then
    begin
      mo.momz := actor.momz;
      mo.z := mo.z - 4 * FRACUNIT;
      if spermcount and 1 <> 0 then // Every other reproduction
        mo.special1 := SHARDSPAWN_DOWN or SHARDSPAWN_LEFT or SHARDSPAWN_RIGHT
      else
        mo.special1 := SHARDSPAWN_DOWN;
      mo.special2 := spermcount;
      mo.target := actor.target;
      if spermcount = 3 then
        mo.args[0] := 2
      else
        mo.args[0] := 0;
    end;
  end;
end;

//------------------------------------------------------------------------
//
// PROC P_SetupPsprites
//
// Called at start of level for each player
//
//------------------------------------------------------------------------
//
//==============================================================================
procedure P_SetupPsprites(player: Pplayer_t);
var
  i: integer;
begin
  // remove all psprites
  for i := 0 to Ord(NUMPSPRITES) - 1 do
    player.psprites[i].state := nil;

  // spawn the gun
  player.pendingweapon := player.readyweapon;
  P_BringUpWeapon(player);
end;

//------------------------------------------------------------------------
//
// PROC P_MovePsprites
//
// Called every tic by player thinking routine
//
//------------------------------------------------------------------------
//
//==============================================================================
procedure P_MovePsprites(player: Pplayer_t);
var
  i: integer;
  psp: Ppspdef_t;
  state: Pstate_t;
begin
  for i := 0 to Ord(NUMPSPRITES) - 1 do
  begin
    psp := @player.psprites[i];
    // a null state means not active
    state := psp.state;
    if state <> nil then
    begin
      // drop tic count and possibly change state
      // a -1 tic count never changes
      if psp.tics <> -1 then
      begin
        psp.tics := psp.tics - 1;
        if psp.tics = 0 then
          P_SetPsprite(player, i, psp.state.nextstate);
      end;
    end;
  end;

  player.psprites[Ord(ps_flash)].sx := player.psprites[Ord(ps_weapon)].sx;
  player.psprites[Ord(ps_flash)].sy := player.psprites[Ord(ps_weapon)].sy;
end;

end.
