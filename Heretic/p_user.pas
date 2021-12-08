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
//  Player related stuff.
//  Bobbing POV/weapon, movement.
//  Pending weapon.
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit p_user;

interface

uses
  doomdef,
  d_player,
  m_fixed,
  tables;

procedure P_PlayerThink(player: Pplayer_t);

procedure P_Thrust(player: Pplayer_t; angle: angle_t; const move: fixed_t);

procedure P_PlayerNextArtifact(player: Pplayer_t);

procedure P_PlayerUseArtifact(player: Pplayer_t; arti: artitype_t);

function P_GetPlayerNum(player: Pplayer_t): integer;

function P_UndoPlayerChicken(player: Pplayer_t): boolean;

function P_UseArtifact(player: Pplayer_t; arti: artitype_t): boolean;

procedure P_PlayerRemoveArtifact(player: Pplayer_t; slot: integer);

implementation

uses
  d_delphi,
  d_ticcmd,
  d_event,
  info_h,
  info,
  m_rnd,
{$IFDEF DEBUG}
  i_io,
{$ENDIF}
  g_game,
  p_mobj_h,
  p_mobj,
  p_tick,
  p_pspr,
  p_local,
  p_setup,    // JVAL: 3d Floors
  p_slopes,   // JVAL: Slopes
  p_3dfloors, // JVAL: Slopes
  p_spec,
  p_map,
  p_telept,
  p_inter,
  r_main,
  r_defs,
  doomstat,
  sb_bar,
  sounds,
  s_sound;

//
// Movement.
//
const
// 16 pixels of bob
  MAXBOB = $100000;

var
  onground: boolean;

//
// P_Thrust
// Moves the given origin along a given angle.
//
procedure P_Thrust(player: Pplayer_t; angle: angle_t; const move: fixed_t);
var
  mv: fixed_t;
begin
  angle := angle shr ANGLETOFINESHIFT;

  if (player.powers[Ord(pw_flight)] <> 0) and (player.mo.z > player.mo.floorz) then
  begin
    player.mo.momx := player.mo.momx + FixedMul(move, finecosine[angle]);
    player.mo.momy := player.mo.momy + FixedMul(move, finesine[angle]);
  end
  else if Psubsector_t(player.mo.subsector).sector.special = 15 then // Friction_Low
  begin
    mv := move div 4;
    player.mo.momx := player.mo.momx + FixedMul(mv, finecosine[angle]);
    player.mo.momy := player.mo.momy + FixedMul(mv, finesine[angle]);
  end
  else
  begin
    player.mo.momx := player.mo.momx + FixedMul(move, finecosine[angle]);
    player.mo.momy := player.mo.momy + FixedMul(move, finesine[angle]);
  end
end;

//
// P_CalcHeight
// Calculate the walking / running height adjustment
//
procedure P_CalcHeight(player: Pplayer_t);
var
  angle: integer;
  clheight: fixed_t; // JVAL: 20211101 - Crouch
  flheight: fixed_t; // JVAL: 20211101 - Crouch
  range: fixed_t; // JVAL: 20211101 - Crouch
begin
  // Regular movement bobbing
  // (needs to be calculated for gun swing
  // even if not on ground)
  // OPTIMIZE: tablify angle
  // Note: a LUT allows for effects
  //  like a ramp with low health.

  if (player.mo.flags2 and MF2_FLY <> 0) and not onground then
    player.bob := FRACUNIT div 2
  else
  begin
    player.bob := FixedMul(player.mo.momx, player.mo.momx) +
                  FixedMul(player.mo.momy, player.mo.momy);
    player.bob := player.bob div 4;

    if player.bob > MAXBOB then
      player.bob := MAXBOB;
  end;

  player.oldviewz := player.viewz;  // JVAL: Slopes

  // JVAL: 20211101 - Crouch
  if player.chickenTics <> 0 then
    player.crouchheight := 0
  else
  begin
    player.mo.height := player.mo.info.height - player.crouchheight;
    clheight := P_3dCeilingHeight(player.mo);
    if player.mo.z + player.mo.height > clheight then
    begin
      flheight := P_3dFloorHeight(player.mo);
      player.mo.z := clheight - player.mo.height;
      if player.mo.z < flheight then
      begin
        player.mo.z := flheight;
        player.lastautocrouchtime := leveltime;
        player.lastongroundtime := leveltime;
        range := clheight - flheight;
        player.crouchheight := player.mo.info.height - range;
        if player.crouchheight > PMAXCROUCHHEIGHT then
          player.crouchheight := PMAXCROUCHHEIGHT;
        player.mo.height := player.mo.info.height - player.crouchheight;
      end;
    end;
  end;

  if (player.cheats and CF_NOMOMENTUM <> 0) or not onground then
  begin
    player.viewz := player.mo.z + PVIEWHEIGHT - player.crouchheight;  // JVAL: 20211101 - Crouch;

    if player.viewz > player.mo.ceilingz - 4 * FRACUNIT then
      player.viewz := player.mo.ceilingz - 4 * FRACUNIT;

//    player.viewz := player.mo.z + player.viewheight;  JVAL removed!
    exit;
  end;

  angle := (FINEANGLES div 20 * leveltime) and FINEMASK;
  player.viewbob := FixedMul(player.bob div 2, finesine[angle]);

  // move viewheight
  if player.playerstate = PST_LIVE then
  begin
    player.viewheight := player.viewheight + player.deltaviewheight;

    if player.viewheight > PVIEWHEIGHT then
    begin
      player.viewheight := PVIEWHEIGHT;
      player.deltaviewheight := 0;
    end;

    if player.viewheight < PVIEWHEIGHT div 2 then
    begin
      player.viewheight := PVIEWHEIGHT div 2;
      if player.deltaviewheight <= 0 then
        player.deltaviewheight := 1;
    end;

    if player.deltaviewheight <> 0 then
    begin
      player.deltaviewheight := player.deltaviewheight + FRACUNIT div 4;
      if player.deltaviewheight = 0 then
        player.deltaviewheight := 1;
    end;
  end;

  if player.chickenTics <> 0 then
    player.viewz := player.mo.z + player.viewheight - (20 * FRACUNIT)
  else
    player.viewz := player.mo.z + player.viewheight + player.viewbob - player.crouchheight; // JVAL: 20211101 - Crouch;

{  if (player.mo.flags2 and MF2_FEETARECLIPPED <> 0) and
     (player.playerstate <> PST_DEAD) and
     (player.mo.z <= player.mo.floorz) then
    player.viewz := player.viewz - FOOTCLIPSIZE;}

  if player.viewz > player.mo.ceilingz - 4 * FRACUNIT then
    player.viewz := player.mo.ceilingz - 4 * FRACUNIT;
  if player.viewz < player.mo.floorz + 4 * FRACUNIT then
    player.viewz := player.mo.floorz + 4 * FRACUNIT;
end;

procedure P_CalcHeight205(player: Pplayer_t);
var
  angle: integer;
begin
  // Regular movement bobbing
  // (needs to be calculated for gun swing
  // even if not on ground)
  // OPTIMIZE: tablify angle
  // Note: a LUT allows for effects
  //  like a ramp with low health.

  if (player.mo.flags2 and MF2_FLY <> 0) and not onground then
    player.bob := FRACUNIT div 2
  else
  begin
    player.bob := FixedMul(player.mo.momx, player.mo.momx) +
                  FixedMul(player.mo.momy, player.mo.momy);
    player.bob := player.bob div 4;

    if player.bob > MAXBOB then
      player.bob := MAXBOB;
  end;

  player.oldviewz := player.viewz;  // JVAL: Slopes

  if (player.cheats and CF_NOMOMENTUM <> 0) or not onground then
  begin
    player.viewz := player.mo.z + PVIEWHEIGHT;

    if player.viewz > player.mo.ceilingz - 4 * FRACUNIT then
      player.viewz := player.mo.ceilingz - 4 * FRACUNIT;

//    player.viewz := player.mo.z + player.viewheight;  JVAL removed!
    exit;
  end;

  angle := (FINEANGLES div 20 * leveltime) and FINEMASK;
  player.viewbob := FixedMul(player.bob div 2, finesine[angle]);

  // move viewheight
  if player.playerstate = PST_LIVE then
  begin
    player.viewheight := player.viewheight + player.deltaviewheight;

    if player.viewheight > PVIEWHEIGHT then
    begin
      player.viewheight := PVIEWHEIGHT;
      player.deltaviewheight := 0;
    end;

    if player.viewheight < PVIEWHEIGHT div 2 then
    begin
      player.viewheight := PVIEWHEIGHT div 2;
      if player.deltaviewheight <= 0 then
        player.deltaviewheight := 1;
    end;

    if player.deltaviewheight <> 0 then
    begin
      player.deltaviewheight := player.deltaviewheight + FRACUNIT div 4;
      if player.deltaviewheight = 0 then
        player.deltaviewheight := 1;
    end;
  end;

  if player.chickenTics <> 0 then
    player.viewz := player.mo.z + player.viewheight - (20 * FRACUNIT)
  else
    player.viewz := player.mo.z + player.viewheight + player.viewbob;

{  if (player.mo.flags2 and MF2_FEETARECLIPPED <> 0) and
     (player.playerstate <> PST_DEAD) and
     (player.mo.z <= player.mo.floorz) then
    player.viewz := player.viewz - FOOTCLIPSIZE;}

  if player.viewz > player.mo.ceilingz - 4 * FRACUNIT then
    player.viewz := player.mo.ceilingz - 4 * FRACUNIT;
  if player.viewz < player.mo.floorz + 4 * FRACUNIT then
    player.viewz := player.mo.floorz + 4 * FRACUNIT;
end;

// JVAL: Slopes
procedure P_SlopesCalcHeight(player: Pplayer_t);
var
  angle: integer;
  oldviewz: fixed_t;
  oldviewz2: fixed_t;
begin
  // Regular movement bobbing
  // (needs to be calculated for gun swing
  // even if not on ground)
  // OPTIMIZE: tablify angle
  // Note: a LUT allows for effects
  //  like a ramp with low health.

  if  G_PlayingEngineVersion >= VERSION207 then
  begin
    P_CalcHeight(player);
    exit;
  end;

  if (G_PlayingEngineVersion < VERSION115) or
     (G_PlayingEngineVersion >= VERSION205) then
  begin
    P_CalcHeight205(player);
    exit;
  end;

  if (player.mo.flags2 and MF2_FLY <> 0) and not onground then
    player.bob := FRACUNIT div 2
  else
  begin
    player.bob := FixedMul(player.mo.momx, player.mo.momx) +
                  FixedMul(player.mo.momy, player.mo.momy);
    player.bob := player.bob div 4;

    if player.bob > MAXBOB then
      player.bob := MAXBOB;
  end;

  oldviewz := player.viewz;

  if (player.cheats and CF_NOMOMENTUM <> 0) or not onground then
  begin
    player.viewz := player.mo.z + PVIEWHEIGHT;

    if player.viewz > player.mo.ceilingz - 4 * FRACUNIT then
      player.viewz := player.mo.ceilingz - 4 * FRACUNIT;

    player.oldviewz := oldviewz;

//    player.viewz := player.mo.z + player.viewheight;  JVAL removed!
    exit;
  end;

  angle := (FINEANGLES div 20 * leveltime) and FINEMASK;
  player.viewbob := FixedMul(player.bob div 2, finesine[angle]) div (player.slopetics + 1);

  // move viewheight
  if player.playerstate = PST_LIVE then
  begin
    player.viewheight := player.viewheight + player.deltaviewheight;

    if player.viewheight > PVIEWHEIGHT then
    begin
      player.viewheight := PVIEWHEIGHT;
      player.deltaviewheight := 0;
    end;

    if player.viewheight < PVIEWHEIGHT div 2 then
    begin
      player.viewheight := PVIEWHEIGHT div 2;
      if player.deltaviewheight <= 0 then
        player.deltaviewheight := 1;
    end;

    if player.deltaviewheight <> 0 then
    begin
      if player.slopetics > 0 then
        player.deltaviewheight := player.deltaviewheight + (FRACUNIT div 4) * player.slopetics
      else
        player.deltaviewheight := player.deltaviewheight + FRACUNIT div 4;
      if player.deltaviewheight = 0 then
        player.deltaviewheight := 1;
    end;
  end;

  if player.chickenTics <> 0 then
    player.viewz := player.mo.z + player.viewheight - (20 * FRACUNIT)
  else if player.slopetics > 0 then
  begin
    oldviewz2 := player.oldviewz;

    player.viewz :=
      (player.slopetics * player.viewz +
       player.mo.z + player.viewheight + player.viewbob) div (player.slopetics + 1); // Extra smooth

    if oldviewz2 < oldviewz then
    begin
      if player.viewz < oldviewz then
        player.viewz := oldviewz;
    end
    else if oldviewz2 > oldviewz then
    begin
      if player.viewz > oldviewz then
        player.viewz := oldviewz;
    end;

    if player.viewz < player.mo.floorz + PVIEWHEIGHT div 2 - 4 * FRACUNIT then
      player.viewz := player.mo.floorz + PVIEWHEIGHT div 2 - 4 * FRACUNIT;
    if player.viewz < player.mo.floorz + 4 * FRACUNIT then
      player.viewz := player.mo.floorz + 4 * FRACUNIT;
  end
  else
    player.viewz := player.mo.z + player.viewheight + player.viewbob;

  if player.viewz > player.mo.ceilingz - 4 * FRACUNIT then
    player.viewz := player.mo.ceilingz - 4 * FRACUNIT;

  if player.viewz < player.mo.floorz + 4 * FRACUNIT then
    player.viewz := player.mo.floorz + 4 * FRACUNIT;

  player.oldviewz := oldviewz;
end;

//
// P_MovePlayer
//
procedure P_MovePlayer(player: Pplayer_t);
var
  cmd: Pticcmd_t;
  look: integer;
  look16: integer;
  look2: integer;
  fly: integer;
  onair: boolean;
  mfactor: integer;
  cmd_crouch, cmd_jump: byte;
begin
  cmd := @player.cmd;

  player.mo.angle := player.mo.angle + _SHLW(cmd.angleturn, 16);

  // Do not let the player control movement
  //  if not onground.
  onground := (player.mo.z <= player.mo.floorz) or (player.mo.flags2 and MF2_ONMOBJ <> 0);

  if onground then
    player.lastongroundtime := leveltime; // JVAL: 20211101 - Crouch

  cmd_jump := (cmd.jump_crouch and CMD_JUMP_MASK) shr CMD_JUMP_SHIFT;
  cmd_crouch := (cmd.jump_crouch and CMD_CROUCH_MASK) shr CMD_CROUCH_SHIFT;

  onair := (player.cheats and CF_LOWGRAVITY <> 0) or (player.mo.flags2 and MF2_FLY <> 0);
  if player.chickenTics <> 0 then
    mfactor := 2500
  else
    mfactor := 2048;

  if (cmd.forwardmove <> 0) and
     (onground or (onair and (player.powers[Ord(pw_flight)] > 0)) or ((cmd_jump > 0) and (player.mo.momx = 0) and (player.mo.momy = 0))) then
    P_Thrust(player, player.mo.angle, cmd.forwardmove * mfactor);

  if (cmd.sidemove <> 0) and
     (onground or (onair and (player.powers[Ord(pw_flight)] > 0)) or ((cmd_jump > 0) and (player.mo.momx = 0) and (player.mo.momy = 0))) then
    P_Thrust(player, player.mo.angle - ANG90, cmd.sidemove * mfactor);

  // JVAL: Adjust speed while flying, bye-bye compatibility :(
  if onair and (player.mo.z > player.mo.floorz) then
  begin
    if player.mo.momx > 18 * FRACUNIT then
      player.mo.momx := 18 * FRACUNIT
    else if player.mo.momx < -18 * FRACUNIT then
      player.mo.momx := -18 * FRACUNIT;
    if player.mo.momy > 18 * FRACUNIT then
      player.mo.momy := 18 * FRACUNIT
    else if player.mo.momy < -18 * FRACUNIT then
      player.mo.momy := -18 * FRACUNIT;

    if (cmd.forwardmove = 0) and (cmd.sidemove = 0) then
    begin
      player.mo.momx := player.mo.momx * 15 div 16;
      player.mo.momy := player.mo.momy * 15 div 16;
    end;
  end;

  if (cmd.forwardmove <> 0) or (cmd.sidemove <> 0) then
  begin
    if player.chickenTics <> 0 then
    begin
      if player.mo.state = @states[Ord(S_CHICPLAY)] then
        P_SetMobjState(player.mo, S_CHICPLAY_RUN1);
    end
    else
    begin
      if player.mo.state = @states[Ord(S_PLAY)] then
        P_SetMobjState(player.mo, S_PLAY_RUN1);
    end;
  end;

// JVAL Look UP and DOWN
  if zaxisshift then
  begin
    if G_PlayingEngineVersion < VERSION203 then // JVAL Smooth Look Up/Down
    begin
      look := cmd.lookfly and 15;
      if look > 7 then
        look := look - 16;

      if look <> 0 then
      begin
        if look = TOCENTER then
          player.centering := true
        else
        begin
          player.lookdir := player.lookdir + 5 * look;
          if player.lookdir > MAXLOOKDIR then
            player.lookdir := MAXLOOKDIR
          else if player.lookdir < MINLOOKDIR then
            player.lookdir := MINLOOKDIR;
        end;
      end;
      player.lookdir16 := player.lookdir * 16;
    end
    else
    begin // JVAL Smooth Look Up/Down
      look16 := cmd.lookupdown16;
      if look16 > 7 * 256 then
        look16 := look16 - 16 * 256;

      if look16 <> 0 then
      begin
        if look16 = TOCENTER * 256 then
          player.centering := true
        else
        begin
          player.lookdir16 := player.lookdir16 + Round(5 * look16 / 16);
          player.lookdir := player.lookdir16 div 16;

          if player.lookdir16 > MAXLOOKDIR * 16 then
            player.lookdir16 := MAXLOOKDIR * 16
          else if player.lookdir16 < MINLOOKDIR * 16 then
            player.lookdir16 := MINLOOKDIR * 16;

          if player.lookdir > MAXLOOKDIR then
            player.lookdir := MAXLOOKDIR
          else if player.lookdir < MINLOOKDIR then
            player.lookdir := MINLOOKDIR;
        end;
      end;
    end;

    if player.centering then
    begin
      if G_PlayingEngineVersion < VERSION203 then // JVAL Smooth Look Up/Down
      begin
        if player.lookdir > 0 then
          player.lookdir := player.lookdir - 8
        else if player.lookdir < 0 then
          player.lookdir := player.lookdir + 8;

        if abs(player.lookdir) < 8 then
        begin
          player.lookdir := 0;
          player.centering := false;
        end;

        player.lookdir16 := player.lookdir * 16;
      end
      else
      begin // JVAL Smooth Look Up/Down
        if player.lookdir16 > 0 then
          player.lookdir16 := player.lookdir16 - 8 * 16
        else if player.lookdir16 < 0 then
          player.lookdir16 := player.lookdir16 + 8 * 16;

        if abs(player.lookdir16) < 8 * 16 then
        begin
          player.lookdir16 := 0;
          player.centering := false;
        end;

        player.lookdir := player.lookdir16 div 16;
      end;
    end;
  end;

  if not G_NeedsCompatibilityMode then
  begin
  // JVAL Look LEFT and RIGHT
    look2 := cmd.lookleftright;
    if look2 > 7 then
      look2 := look2 - 16;

    if look2 <> 0 then
    begin
      if look2 = TOFORWARD then
        player.forwarding := true
      else
      begin
        player.lookdir2 := (player.lookdir2 + 2 * look2) and 255;
        if player.lookdir2 in [64..127] then
          player.lookdir2 := 63
        else if player.lookdir2 in [128..191] then
          player.lookdir2 := 192;
      end;
    end
    else
      if player.oldlook2 <> 0 then
        player.forwarding := true;

    if player.forwarding then
    begin
      if player.lookdir2 in [3..63] then
        player.lookdir2 := player.lookdir2 - 6
      else if player.lookdir2 in [192..251] then
        player.lookdir2 := player.lookdir2 + 6;

      if (player.lookdir2 < 8) or (player.lookdir2 > 247) then
      begin
        player.lookdir2 := 0;
        player.forwarding := false;
      end;
    end;
    player.mo.viewangle := player.lookdir2 shl 24;

    player.oldlook2 := look2;

    if (onground or (player.cheats and CF_LOWGRAVITY <> 0)) and (cmd_jump > 1) then
    begin
      // JVAL: 20211101 - Crouch
      if cmd_crouch > 0 then
        player.mo.momz := 4 * FRACUNIT
      else
        player.mo.momz := 8 * FRACUNIT;
    end;

    // JVAL: 20211101 - Crouch
    if (leveltime - player.lastongroundtime < TICRATE) and (cmd_crouch <> 0) then
    begin
      player.crouchheight := player.crouchheight + cmd_crouch * FRACUNIT;
      if player.crouchheight > PMAXCROUCHHEIGHT then
        player.crouchheight := PMAXCROUCHHEIGHT;
    end
    else if (leveltime - player.lastautocrouchtime > TICRATE) and (player.crouchheight <> 0) then
    begin
      player.crouchheight := player.crouchheight - 2 * FRACUNIT;
      if player.crouchheight < 0 then
        player.crouchheight := 0;
    end;
    if not onground and (cmd_crouch <> 0) then
      if player.mo.momz > -4 * FRACUNIT then
      begin
        player.mo.momz := player.mo.momz - FRACUNIT div 2;
        if player.mo.momz < -4 * FRACUNIT then
          player.mo.momz := -4 * FRACUNIT;
      end;
  end
  else
    player.lookdir2 := 0;

  fly := _SHR(cmd.lookfly, 4);
  if fly > 7 then
    fly := fly - 16;

  if (fly <> 0) and (player.powers[Ord(pw_flight)] <> 0) then
  begin
    if fly <> TOCENTER then
    begin
      player.flyheight := fly * 2;
      if player.mo.flags2 and MF2_FLY = 0 then
      begin
        player.mo.flags2 := player.mo.flags2 or MF2_FLY;
        player.mo.flags := player.mo.flags or MF_NOGRAVITY;
      end;
    end
    else
    begin
      player.mo.flags2 := player.mo.flags2 and not MF2_FLY;
      player.mo.flags := player.mo.flags and not MF_NOGRAVITY;
    end;
  end
  else if fly > 0 then
    P_PlayerUseArtifact(player, arti_fly);

  if player.mo.flags2 and MF2_FLY <> 0 then
  begin
    player.mo.momz := player.flyheight * FRACUNIT;
    if player.flyheight <> 0 then
      player.flyheight := player.flyheight div 2;
  end;

end;

//
// P_DeathThink
// Fall on your face when dying.
// Decrease POV height to floor height.
//
const
  ANG5 = ANG90 div 18;
  ANG355 = ANG270 +  ANG5 * 17; // add by JVAL

procedure P_DeathThink(player: Pplayer_t);
var
  angle: angle_t;
  delta: angle_t;
begin
  P_MovePsprites(player);

  // fall to the ground
  if player.viewheight > 6 * FRACUNIT then
    player.viewheight := player.viewheight - FRACUNIT;

  if player.viewheight < 6 * FRACUNIT then
    player.viewheight := 6 * FRACUNIT;

  if player.viewheight > 6 * FRACUNIT then
    if player.lookdir < 45 then
    begin
      player.lookdir := player.lookdir + 5;
      player.lookdir16 := player.lookdir * 16; // JVAL Smooth Look Up/Down
    end;

  player.deltaviewheight := 0;
  onground := player.mo.z <= player.mo.floorz;
  P_SlopesCalcHeight(player); // JVAL: Slopes

  if (player.attacker <> nil) and (player.attacker <> player.mo) then
  begin

    angle := R_PointToAngle2(
      player.mo.x, player.mo.y, player.attackerx, player.attackery);

    delta := angle - player.mo.angle;

    if (delta < ANG5) or (delta > ANG355) then
    begin
      // Looking at killer,
      //  so fade damage flash down.
      player.mo.angle := angle;

      if player.damagecount <> 0 then
        player.damagecount := player.damagecount - 1;
    end
    else if delta < ANG180 then
      player.mo.angle := player.mo.angle + ANG5
    else
      player.mo.angle := player.mo.angle - ANG5;

  end
  else if player.damagecount <> 0 then
    player.damagecount := player.damagecount - 1;

  if player.cmd.buttons and BT_USE <> 0 then
    player.playerstate := PST_REBORN;
end;

//----------------------------------------------------------------------------
//
// PROC P_ChickenPlayerThink
//
//----------------------------------------------------------------------------

procedure P_ChickenPlayerThink(player: Pplayer_t);
var
  pmo: Pmobj_t;
begin
  if player.health > 0 then
  begin // Handle beak movement
    P_UpdateBeak(player, @player.psprites[ps_weapon]);
  end;

  if player.chickenTics and 15 <> 0 then
    exit;

  pmo := player.mo;
  if (pmo.momx + pmo.momy = 0) and (P_Random < 160) then
  begin // Twitch view angle
    pmo.angle := pmo.angle + _SHLW(P_Random - P_Random, 19);
  end;

  if (pmo.z <= pmo.floorz) and (P_Random < 32) then
  begin // Jump and noise
    pmo.momz := pmo.momz + FRACUNIT;
    P_SetMobjState(pmo, S_CHICPLAY_PAIN);
    exit;
  end;

  if P_Random < 48 then // Just noise
    S_StartSound(pmo, Ord(sfx_chicact));
end;

//
// P_PlayerThink
//
procedure P_PlayerThink(player: Pplayer_t);
var
  cmd: Pticcmd_t;
  newweapon: weapontype_t;
  sec: Psector_t; // JVAL: 3d Floors
begin
  // fixme: do this in the cheat code
  if player.cheats and CF_NOCLIP <> 0 then
    player.mo.flags := player.mo.flags or MF_NOCLIP
  else
    player.mo.flags := player.mo.flags and not MF_NOCLIP;

  // chain saw run forward
  cmd := @player.cmd;
  if player.mo.flags and MF_JUSTATTACKED <> 0 then
  begin
    cmd.angleturn := 0;
    cmd.forwardmove := $c800 div 512;
    cmd.sidemove := 0;
    player.mo.flags := player.mo.flags and not MF_JUSTATTACKED;
  end;

  if player.quaketics > 0 then
  begin
    Dec(player.quaketics, FRACUNIT);
    if player.quaketics < 0 then
      player.quaketics := 0;
  end;

  if player.teleporttics > 0 then
  begin
    Dec(player.teleporttics, FRACUNIT);
    if player.teleporttics < 0 then
      player.teleporttics := 0;
  end;

  if player.playerstate = PST_DEAD then
  begin
    P_DeathThink(player);
    exit;
  end;

  if player.chickenTics <> 0 then
    P_ChickenPlayerThink(player);

  // Move around.
  // Reactiontime is used to prevent movement
  //  for a bit after a teleport.
  if player.mo.reactiontime <> 0 then
    player.mo.reactiontime := player.mo.reactiontime - 1
  else
    P_MovePlayer(player);

  P_SlopesCalcHeight(player); // JVAL: Slopes

  // JVAL: 3d Floors
  sec := Psubsector_t(player.mo.subsector).sector;
  if sec.special <> 0 then
    P_PlayerInSpecialSector(player, sec, P_FloorHeight(sec, player.mo.x, player.mo.y));    // JVAL: 3d Floors
  if sec.midsec >= 0 then
    if sectors[sec.midsec].special <> 0 then
      P_PlayerInSpecialSector(player, @sectors[sec.midsec], sectors[sec.midsec].ceilingheight);  // JVAL: 3d Floors

  if cmd.arti <> 0 then
  begin // Use an artifact
    if cmd.arti = $ff then
      P_PlayerNextArtifact(player)
    else
      P_PlayerUseArtifact(player, artitype_t(cmd.arti));
  end;

  // Check for weapon change.

  // A special event has no other buttons.
  if cmd.buttons and BT_SPECIAL <> 0 then
    cmd.buttons := 0;

  if cmd.buttons and BT_CHANGE <> 0 then
  begin
    // The actual changing of the weapon is done
    //  when the weapon psprite can do it
    //  (read: not in the middle of an attack).
    newweapon := weapontype_t(_SHR(cmd.buttons and BT_WEAPONMASK, BT_WEAPONSHIFT));

    if (newweapon = wp_staff) and
       (player.weaponowned[Ord(wp_gauntlets)] <> 0) and
       (player.readyweapon <> wp_gauntlets) then
    begin
      newweapon := wp_gauntlets;
      // JVAL: If readyweapon is already the wp_gauntlets return to wp_staff
      // Only if we don't have old compatibility mode suspended
      if not G_NeedsCompatibilityMode then
        if player.readyweapon = wp_gauntlets then
          newweapon := wp_staff;
    end;


    if (player.weaponowned[Ord(newweapon)] <> 0) and
       (newweapon <> player.readyweapon) then
      player.pendingweapon := newweapon;
  end;

  // check for use
  if cmd.buttons and BT_USE <> 0 then
  begin
    if not player.usedown then
    begin
      P_UseLines(player);
      player.usedown := true;
    end;
  end
  else
    player.usedown := false;

  // Chicken counter
  if player.chickenTics <> 0 then
  begin
    if player.chickenPeck <> 0 then // Chicken attack counter
      player.chickenPeck := player.chickenPeck - 3;

    dec(player.chickenTics);
    if player.chickenTics = 0 then  // Attempt to undo the chicken
      P_UndoPlayerChicken(player);
  end;

  // cycle psprites
  P_MovePsprites(player);

  // Counters, time dependend power ups.

  if player.powers[Ord(pw_invulnerability)] <> 0 then
    player.powers[Ord(pw_invulnerability)] := player.powers[Ord(pw_invulnerability)] - 1;

  if player.powers[Ord(pw_invisibility)] <> 0 then
  begin
    player.powers[Ord(pw_invisibility)] := player.powers[Ord(pw_invisibility)] - 1;
    if player.powers[Ord(pw_invisibility)] = 0 then
      player.mo.flags := player.mo.flags and not MF_SHADOW;
  end;

  if player.powers[Ord(pw_infrared)] <> 0 then
    player.powers[Ord(pw_infrared)] := player.powers[Ord(pw_infrared)] - 1;

  if player.powers[Ord(pw_flight)] <> 0 then
  begin
    player.powers[Ord(pw_flight)] := player.powers[Ord(pw_flight)] - 1;
    if player.powers[Ord(pw_flight)] = 0 then
    begin
      if player.mo.z <> player.mo.floorz then
        player.centering := true;

      player.mo.flags2 := player.mo.flags2 and not MF2_FLY;
      player.mo.flags := player.mo.flags and not MF_NOGRAVITY;
    end;
  end;

  if player.powers[Ord(pw_weaponlevel2)] <> 0 then
  begin
    player.powers[Ord(pw_weaponlevel2)] := player.powers[Ord(pw_weaponlevel2)] - 1;
    if player.powers[Ord(pw_weaponlevel2)] = 0 then
    begin
      if((player.readyweapon = wp_phoenixrod) and
         (player.psprites[ps_weapon].state <> @states[Ord(S_PHOENIXREADY)]) and
         (player.psprites[ps_weapon].state <> @states[Ord(S_PHOENIXUP)])) then
      begin
        P_SetPsprite(player, ps_weapon, S_PHOENIXREADY);
        player.ammo[Ord(am_phoenixrod)] := player.ammo[Ord(am_phoenixrod)] - USE_PHRD_AMMO_2;
        player.refire := 0;
      end
      else if (player.readyweapon = wp_gauntlets) or
              (player.readyweapon = wp_staff) then
        player.pendingweapon := player.readyweapon;
    end;
  end;

  if player.damagecount <> 0 then
    player.damagecount := player.damagecount - 1;

  if player.bonuscount <> 0 then
    player.bonuscount := player.bonuscount - 1;


  // Handling colormaps.
  if player.powers[Ord(pw_invulnerability)] <> 0 then
  begin
    if (player.powers[Ord(pw_invulnerability)] > 4 * 32) or
       (player.powers[Ord(pw_invulnerability)] and 8 <> 0) then
      player.fixedcolormap := INVERSECOLORMAP
    else
      player.fixedcolormap := 0;
  end
  else if player.powers[Ord(pw_infrared)] <> 0 then
  begin
    if (player.powers[Ord(pw_infrared)] > 4 * 32) or
       (player.powers[Ord(pw_infrared)] and 8 <> 0) then
      // almost full bright
      player.fixedcolormap := 1
    else
      player.fixedcolormap := 0;
  end
  else
    player.fixedcolormap := 0;
end;

//----------------------------------------------------------------------------
//
// PROC P_ArtiTele
//
//----------------------------------------------------------------------------

procedure P_ArtiTele(player: Pplayer_t);
var
  i: integer;
  selections: integer;
  destX: fixed_t;
  destY: fixed_t;
  destAngle: angle_t;
begin
  if deathmatch <> 0 then
  begin
    selections := deathmatch_p;
    i := P_Random mod selections;
    destX := deathmatchstarts[i].x * FRACUNIT;
    destY := deathmatchstarts[i].y * FRACUNIT;
    if player.mo.flags2_ex and MF2_EX_PRECISESPAWNANGLE <> 0 then
      destAngle := ANG1 * deathmatchstarts[i].angle
    else
      destAngle := ANG45 * (deathmatchstarts[i].angle div 45);
  end
  else
  begin
    destX := playerstarts[0].x * FRACUNIT;
    destY := playerstarts[0].y * FRACUNIT;
    if player.mo.flags2_ex and MF2_EX_PRECISESPAWNANGLE <> 0 then
      destAngle := ANG1 * playerstarts[0].angle
    else
      destAngle := ANG45 * (playerstarts[0].angle div 45);
  end;
  P_Teleport(player.mo, destX, destY, destAngle);
  S_StartSound(nil, Ord(sfx_wpnup)); // Full volume laugh
end;

//----------------------------------------------------------------------------
//
// PROC P_PlayerNextArtifact
//
//----------------------------------------------------------------------------

procedure P_PlayerNextArtifact(player: Pplayer_t);
begin
  if player = @players[consoleplayer] then
  begin
    dec(inv_ptr);
    if inv_ptr < 6 then
    begin
      dec(curpos);
      if curpos < 0 then
        curpos := 0;
    end;
    if inv_ptr < 0 then
    begin
      inv_ptr := player.inventorySlotNum - 1;
      if inv_ptr < 6 then
        curpos := inv_ptr
      else
        curpos := 6;
    end;

    player.readyArtifact := artitype_t(player.inventory[inv_ptr]._type);
  end;
end;


//----------------------------------------------------------------------------
//
// PROC P_PlayerRemoveArtifact
//
//----------------------------------------------------------------------------

procedure P_PlayerRemoveArtifact(player: Pplayer_t; slot: integer);
var
  i: integer;
begin
  dec(player.artifactCount);
  dec(player.inventory[slot].count);
  if player.inventory[slot].count = 0 then
  begin // Used last of a type - compact the artifact list
    player.readyArtifact := arti_none;
    player.inventory[slot]._type := Ord(arti_none);
    for i := slot + 1 to player.inventorySlotNum - 1 do
      player.inventory[i - 1] := player.inventory[i];

    dec(player.inventorySlotNum);
    if player = @players[consoleplayer] then
    begin // Set position markers and get next readyArtifact
      dec(inv_ptr);
      if inv_ptr < 6 then
      begin
        dec(curpos);
        if curpos < 0 then
          curpos := 0;
      end;
      if inv_ptr >= player.inventorySlotNum then
        inv_ptr := player.inventorySlotNum - 1;
      if inv_ptr < 0 then
        inv_ptr := 0;
      player.readyArtifact := artitype_t(player.inventory[inv_ptr]._type);
    end;
  end;
end;

//----------------------------------------------------------------------------
//
// PROC P_PlayerUseArtifact
//
//----------------------------------------------------------------------------

procedure P_PlayerUseArtifact(player: Pplayer_t; arti: artitype_t);
var
  i: integer;
begin
  for i := 0 to player.inventorySlotNum - 1 do
  begin
    if player.inventory[i]._type = Ord(arti) then
    begin // Found match - try to use
      if P_UseArtifact(player, arti) then
      begin // Artifact was used - remove it from inventory
        P_PlayerRemoveArtifact(player, i);
        if player = @players[consoleplayer] then
        begin
          S_StartSound(nil, Ord(sfx_artiuse));
          ArtifactFlash := 4;
        end
      end
      else // Unable to use artifact, advance pointer
        P_PlayerNextArtifact(player);
      break;
    end;
  end;
end;

//----------------------------------------------------------------------------
//
// FUNC P_UseArtifact
//
// Returns true if artifact was used.
//
//----------------------------------------------------------------------------

function P_UseArtifact(player: Pplayer_t; arti: artitype_t): boolean;
var
  mo: Pmobj_t;
  angle: angle_t;
  offs: integer;
begin
  case arti of
    arti_invulnerability:
      begin
        if not P_GivePower(player, Ord(pw_invulnerability)) then
        begin
          result := false;
          exit;
        end;
      end;
    arti_invisibility:
      begin
        if not P_GivePower(player, Ord(pw_invisibility)) then
        begin
          result := false;
          exit;
        end;
      end;
    arti_health:
      begin
        if not P_GiveBody(player, 25) then
        begin
          result := false;
          exit;
        end;
      end;
    arti_superhealth:
      begin
        if not P_GiveBody(player, 100) then
        begin
          result := false;
          exit;
        end;
      end;
    arti_tomeofpower:
      begin
        if player.chickenTics <> 0 then
        begin // Attempt to undo chicken
          if P_UndoPlayerChicken(player) = false then
            P_DamageMobj(player.mo, nil, nil, 10000)
          else
          begin // Succeeded
            player.chickenTics := 0;
            S_StartSound(player.mo, Ord(sfx_wpnup));
          end;
        end
        else
        begin
          if not P_GivePower(player, Ord(pw_weaponlevel2)) then
          begin
            result := false;
            exit;
          end;

          if player.readyweapon = wp_staff then
            P_SetPsprite(player, ps_weapon, S_STAFFREADY2_1)
          else if player.readyweapon = wp_gauntlets then
            P_SetPsprite(player, ps_weapon, S_GAUNTLETREADY2_1);
        end;
      end;
    arti_torch:
      begin
        if not P_GivePower(player, Ord(pw_infrared)) then
        begin
          result := false;
          exit;
        end;
      end;
    arti_firebomb:
      begin
        angle := player.mo.angle shr ANGLETOFINESHIFT;
        if player.mo.flags2 and MF2_FEETARECLIPPED <> 0 then
          offs := 1
        else
          offs := 0;
        mo := P_SpawnMobj(player.mo.x + 24 * finecosine[angle],
                          player.mo.y + 24 * finesine[angle],
                          player.mo.z - 15 * FRACUNIT * offs,
                          Ord(MT_FIREBOMB));
        mo.target := player.mo;
      end;
    arti_egg:
      begin
        mo := player.mo;
        P_SpawnPlayerMissile(mo, Ord(MT_EGGFX));
        P_SPMAngle(mo, Ord(MT_EGGFX), mo.angle - (ANG45 div 6));
        P_SPMAngle(mo, Ord(MT_EGGFX), mo.angle + (ANG45 div 6));
        P_SPMAngle(mo, Ord(MT_EGGFX), mo.angle - (ANG45 div 3));
        P_SPMAngle(mo, Ord(MT_EGGFX), mo.angle + (ANG45 div 3));
      end;
    arti_fly:
      begin
        if not P_GivePower(player, Ord(pw_flight)) then
        begin
          result := false;
          exit;
        end;
      end;
    arti_teleport:
      begin
        P_ArtiTele(player);
      end
    else
      begin
        result := false;
        exit;
      end;
  end;

  result := true;
end;


//----------------------------------------------------------------------------
//
// FUNC P_GetPlayerNum
//
//----------------------------------------------------------------------------

function P_GetPlayerNum(player: Pplayer_t): integer;
var
  i: integer;
begin
  for i := 0 to MAXPLAYERS - 1 do
    if player = @players[i] then
    begin
      result := i;
      exit;
    end;

  result := 0;
end;

//----------------------------------------------------------------------------
//
// FUNC P_UndoPlayerChicken
//
//----------------------------------------------------------------------------

function P_UndoPlayerChicken(player: Pplayer_t): boolean;
var
  fog: Pmobj_t;
  mo: Pmobj_t;
  pmo: Pmobj_t;
  x: fixed_t;
  y: fixed_t;
  z: fixed_t ;
  angle: angle_t;
  playerNum: integer;
  weapon: weapontype_t;
  oldFlags: integer;
  oldFlags2: integer;
begin
  pmo := player.mo;
  x := pmo.x;
  y := pmo.y;
  z := pmo.z;
  angle := pmo.angle;
  weapon := weapontype_t(pmo.special1);
  oldFlags := pmo.flags;
  oldFlags2 := pmo.flags2;
  P_SetMobjState(pmo, S_FREETARGMOBJ);
  mo := P_SpawnMobj(x, y, z, Ord(MT_PLAYER));
  if not P_TestMobjLocation(mo) then
  begin // Didn't fit
    P_RemoveMobj(mo);
    mo := P_SpawnMobj(x, y, z, Ord(MT_CHICPLAYER));
    mo.angle := angle;
    mo.health := player.health;
    mo.special1 := Ord(weapon);
    mo.player := player;
    mo.flags := oldFlags;
    mo.flags2 := oldFlags2;
    player.mo := mo;
    player.chickenTics := 2 * TICRATE;
    result := false;
    exit;
  end;

  playerNum := P_GetPlayerNum(player);
  if playerNum <> 0 then // Set color translation
    mo.flags := mo.flags or (playerNum shl MF_TRANSSHIFT);

  mo.angle := angle;
  mo.player := player;
  mo.reactiontime := 18;
  if oldFlags2 and MF2_FLY <> 0 then
  begin
    mo.flags2 := mo.flags2 or MF2_FLY;
    mo.flags := mo.flags or MF_NOGRAVITY;
  end;
  player.chickenTics := 0;
  player.powers[Ord(pw_weaponlevel2)] := 0;
  player.health := mobjinfo[Ord(MT_PLAYER)].spawnhealth;
  mo.health := mobjinfo[Ord(MT_PLAYER)].spawnhealth;
  player.mo := mo;
  angle := angle shr ANGLETOFINESHIFT;
  fog := P_SpawnMobj(x + 20 * finecosine[angle], y + 20 * finesine[angle], z + TELEFOGHEIGHT, Ord(MT_TFOG));
  S_StartSound(fog, Ord(sfx_telept));
  P_PostChickenWeapon(player, weapon);
  result := true;
end;

end.

