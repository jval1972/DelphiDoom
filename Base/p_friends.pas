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
//  Friends handling
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit p_friends;

interface

uses
  d_player,
  p_mobj_h;

//==============================================================================
//
// P_HandleFriendsNearMe
//
//==============================================================================
procedure P_HandleFriendsNearMe(const p: Pplayer_t);

//==============================================================================
//
// P_BothFriends
//
//==============================================================================
function P_BothFriends(mo1, mo2: Pmobj_t): boolean;

//==============================================================================
//
// P_LookForMonsters
//
//==============================================================================
function P_LookForMonsters(actor: Pmobj_t): boolean;

implementation

uses
  doomdef,
  d_think,
  g_game,
  info_common,
  {$IFDEF DOOM}
  info_h,
  {$ENDIF}
  m_fixed,
  m_rnd,
  p_local,
  p_maputl,
  p_mobj,
  p_setup,
  p_sight,
  p_tick,
  r_main,
  tables;

const
  MAXFRIENDRADIUS = 128 * FRACUNIT;

var
  dplayermo: Pmobj_t;

//==============================================================================
//
// RIT_HandleFriendsNearMe
//
//==============================================================================
function RIT_HandleFriendsNearMe(mo: Pmobj_t): boolean;
var
  dist1: fixed_t;
  dist2: fixed_t;
  speed: fixed_t;
  realangle: angle_t;
begin
  result := true;

  if mo.flags2_ex and MF2_EX_FRIEND = 0 then
    exit;

  if mo.health <= 0 then
    exit;

  dist1 := P_AproxDistance(mo.x - dplayermo.x, mo.y - dplayermo.y);
  if dist1 > MAXFRIENDRADIUS then
    exit;

  if mo.health < mo.info.spawnhealth then
    inc(mo.health);

  dist2 := P_AproxDistance(mo.x + mo.momx - dplayermo.x - dplayermo.momx, mo.y + mo.momx - dplayermo.y - dplayermo.momx);
  if dist2 > dist1 then // Going away
  begin
    mo.momx := mo.momx * 15 div 16;
    mo.momy := mo.momy * 15 div 16;
    mo.momz := mo.momz * 15 div 16;
    exit;
  end;

  if dist1 < MAXFRIENDRADIUS div 2 then
  begin
    if mo.x < dplayermo.x then
      mo.momx := -mo.info.speed * FRACUNIT
    else
      mo.momx := mo.info.speed * FRACUNIT;
    if mo.y < dplayermo.y then
      mo.momy := -mo.info.speed * FRACUNIT
    else
      mo.momy := mo.info.speed * FRACUNIT;
    exit;
  end;

  speed := mo.info.speed * FRACUNIT;

  realangle := R_PointToAngle2(mo.x, mo.y, dplayermo.x, dplayermo.y) - ANG180;

  mo.momx := FixedMul(speed, finecosine[realangle shr ANGLETOFINESHIFT]);
  mo.momy := FixedMul(speed, finesine[realangle shr ANGLETOFINESHIFT]);
end;

//==============================================================================
//
// P_HandleFriendsNearMe
//
//==============================================================================
procedure P_HandleFriendsNearMe(const p: Pplayer_t);
var
  x: integer;
  y: integer;
  xl: integer;
  xh: integer;
  yl: integer;
  yh: integer;
begin
  dplayermo := p.mo;
  if dplayermo = nil then
    exit;

  if internalblockmapformat then
  begin
    yh := MapBlockIntY(int64(viewy) + MAXFRIENDRADIUS - int64(bmaporgy));
    yl := MapBlockIntY(int64(viewy) - MAXFRIENDRADIUS - int64(bmaporgy));
    xh := MapBlockIntX(int64(viewx) + MAXFRIENDRADIUS - int64(bmaporgx));
    xl := MapBlockIntX(int64(viewx) - MAXFRIENDRADIUS - int64(bmaporgx));
  end
  else
  begin
    yh := MapBlockInt(viewy + MAXFRIENDRADIUS - bmaporgy);
    yl := MapBlockInt(viewy - MAXFRIENDRADIUS - bmaporgy);
    xh := MapBlockInt(viewx + MAXFRIENDRADIUS - bmaporgx);
    xl := MapBlockInt(viewx - MAXFRIENDRADIUS - bmaporgx);
  end;

  for y := yl to yh do
    for x := xl to xh do
      P_BlockThingsIterator(x, y, RIT_HandleFriendsNearMe);
end;

//==============================================================================
//
// P_BothFriends
//
// JVAL: New function
//
//==============================================================================
function P_BothFriends(mo1, mo2: Pmobj_t): boolean;
var
  f1, f2: boolean;
begin
  if (mo1 = nil) or (mo2 = nil) then
  begin
    result := false;
    exit;
  end;

  f1 := (mo1.player <> nil) or (mo1.flags2_ex and MF2_EX_FRIEND <> 0);
  if not f1 then
  begin
    result := false;
    exit;
  end;

  f2 := (mo2.player <> nil) or (mo2.flags2_ex and MF2_EX_FRIEND <> 0);
  if not f2 then
  begin
    result := false;
    exit;
  end;

  if deathmatch <> 0 then
    if mo1.player <> nil then
      if mo2.player <> nil then
      begin
        result := false;
        exit;
      end;

  result := true;
end;

const
  MONS_LOOK_RANGE = 20 * 64 * FRACUNIT;
  MONS_LOOK_LIMIT = 64;

//==============================================================================
//
// P_LookForMonsters
//
//==============================================================================
function P_LookForMonsters(actor: Pmobj_t): boolean;
var
  i: integer;
  visible: boolean;
  count: integer;
  mo: Pmobj_t;
  think: Pthinker_t;
  inher: integer;
  maxrange: fixed_t;
begin
  if actor.flags2_ex and MF2_EX_FRIEND = 0 then
  begin
    visible := false;
    for i := 0 to MAXPLAYERS - 1 do
      if playeringame[i] then
        if P_CheckSight(players[i].mo, actor) then
        begin
          visible := true;
          break;
        end;

    // Is there any player to see monster?
    if not visible then
    begin
      result := false;
      exit;
    end;
  end;

  count := 0;
  think := thinkercap.next;
  while think <> @thinkercap do
  begin
    if @think._function.acp1 <> @P_MobjThinker then
    begin
      think := think.next;
      continue;
    end;

    mo := Pmobj_t(think);

    if (mo.flags and MF_COUNTKILL = 0) or (mo = actor) or (mo.health <= 0) then
    begin // Not a valid monster
      think := think.next;
      continue;
    end;

    if P_BothFriends(mo, actor) then
    begin // Friendly monsters do not hurt each other
      think := think.next;
      continue;
    end;

    inher := Info_GetInheritance(mo.info);
    if inher = Info_GetInheritance(actor.info) then
    begin
      {$IFDEF DOOM}
      // JVAL
      // Same monsters does not kill each other,
      // only humanoids with weapons.
      if (inher <> Ord(MT_POSSESSED)) and
         (inher <> Ord(MT_SHOTGUY)) and
         (inher <> Ord(MT_CHAINGUY)) then
      {$ENDIF}
      begin
        think := think.next;
        continue;
      end;
    end;

    if actor.info.maxtargetrange > 0 then
      maxrange := actor.info.maxtargetrange * FRACUNIT
    else
      maxrange := MONS_LOOK_RANGE;

    if P_AproxDistance(actor.x - mo.x, actor.y - mo.y) > maxrange then
    begin // Out of range
      think := think.next;
      continue;
    end;

    if P_Random < 16 then
    begin // Skip
      think := think.next;
      continue;
    end;

    inc(count);
    if count > MONS_LOOK_LIMIT then
    begin // Stop searching
      result := false;
      exit;
    end;

    if not P_CheckSight(actor, mo) then
    begin // Out of sight
      think := think.next;
      continue;
    end;

    // Found a target monster
    actor.target := mo;
    result := true;
    exit;
  end;

  result := false;
end;

end.
