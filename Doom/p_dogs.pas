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
// DESCRIPTION:
//  Dogs handling
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit p_dogs;

interface

uses
  doomdef,
  d_player,
  p_mobj_h,
  m_fixed;

type
  playertrace_t = record
    x, y, z: fixed_t;
    leveltime: integer;
  end;
  Pplayertrace_t = ^playertrace_t;

const
  NUMPLAYERTRACEHISTORY = 1024;
  HISTORYIGNOREDISTANCE = 64 * FRACUNIT;

type
  playertracehistory_t = record
    numitems: integer;
    rover: integer;
    data: array[0..NUMPLAYERTRACEHISTORY - 1] of playertrace_t;
  end;
  Pplayertracehistory_t = ^playertracehistory_t;

var
  playerhistory: array[0..MAXPLAYERS - 1] of playertracehistory_t;

const
  PLAYERFOLLOWDISTANCE = 256 * FRACUNIT;

procedure P_PlayerHistoryNotify(const p: Pplayer_t);

procedure P_ClearPlayerHistory(const p: Pplayer_t);

function P_FollowPlayer(const mo: Pmobj_t; const p: Pplayer_t): boolean;

procedure P_HandleDogsNearMe(const p: Pplayer_t);

implementation

uses
  d_delphi,
  g_game,
  m_rnd,
  p_common,
  p_local,
  p_maputl,
  p_tick,
  p_setup,
  p_sight,
  r_main,
  tables;

procedure P_PlayerHistoryNotify(const p: Pplayer_t);
var
  pid: integer;
  history: Pplayertracehistory_t;
  nrover: integer;
  dist: fixed_t;
  pmo: Pmobj_t;
  item: Pplayertrace_t;
begin
  pid := PlayerToId(p);
  if (pid < 0) or not playeringame[pid] then
    exit;

  history := @playerhistory[pid];

  pmo := p.mo;
  if history.numitems = 0 then
    nrover := 0
  else
  begin
    item := @history.data[history.rover];
    dist := P_AproxDistance(pmo.x - item.x, pmo.y - item.y);
    if dist < HISTORYIGNOREDISTANCE then
      exit;
    nrover := history.rover + 1;
    if nrover >= NUMPLAYERTRACEHISTORY then
      nrover := nrover - NUMPLAYERTRACEHISTORY;
  end;

  item := @history.data[nrover];
  item.x := pmo.x;
  item.y := pmo.y;
  item.z := pmo.z;
  item.leveltime := leveltime;
  history.rover := nrover;
  if history.numitems < NUMPLAYERTRACEHISTORY then
    inc(history.numitems);
end;

procedure P_ClearPlayerHistory(const p: Pplayer_t);
var
  pid: integer;
  history: Pplayertracehistory_t;
begin
  pid := PlayerToId(p);
  if (pid < 0) or not playeringame[pid] then
    exit;

  history := @playerhistory[pid];
  ZeroMemory(history, SizeOf(playertracehistory_t));
end;

function P_FollowPlayer(const mo: Pmobj_t; const p: Pplayer_t): boolean;
var
  pid: integer;
  history: Pplayertracehistory_t;
  i: integer;
  hpos: integer;
  item: Pplayertrace_t;
  bestitem: Pplayertrace_t;
  bestitem2: Pplayertrace_t;
  bestitem3: Pplayertrace_t;
  bestleveltime: fixed_t;
  bestleveltime2: fixed_t;
  dist, maxdist: fixed_t;
  distfromplayer: fixed_t;
  tracefromplayer: fixed_t;
  tics: integer;
  ang: angle_t;
  speed: integer;
  newx, newy, newz: fixed_t;
  seeplayer: boolean;

  procedure _follow_item;
  begin
    mo.target := p.mo;
    ang := R_PointToAngle2(mo.x, mo.y, item.x, item.y);
    mo.angle := ang;
    speed := mo.info.speed;
    if speed > FRACUNIT then
      speed := speed div FRACUNIT;
    mo.momx := speed * finecosine[ang shr ANGLETOFINESHIFT];
    mo.momy := speed * finesine[ang shr ANGLETOFINESHIFT];
    dist := P_AproxDistance(item.x - mo.x, item.y - mo.y);
    tics := (dist - PLAYERFOLLOWDISTANCE) div (speed * FRACUNIT);
    if tics < TICRATE div 5 then
      tics := TICRATE div 5;
    mo.playerfollowtime := leveltime + tics;
    mo.tracefollowtimestamp := item.leveltime;
    mo.flags4_ex := mo.flags4_ex or MF4_EX_TRACEDEFINED;
    mo.tracex := item.x;
    mo.tracey := item.y;
  end;

begin
  result := false;
  if p = nil then
    exit;

  seeplayer := P_CheckSight(mo, p.mo);
  if seeplayer then
    if mo.target <> nil then
      if mo.target <> p.mo then
        exit;

  distfromplayer := P_AproxDistance(mo.x - p.mo.x, mo.y - p.mo.y);
  if distfromplayer < PLAYERFOLLOWDISTANCE then
    if seeplayer then
      exit;

  pid := PlayerToId(p);
  if (pid < 0) or not playeringame[pid] then
    exit;

  history := @playerhistory[pid];
  bestitem := nil;
  bestitem2 := nil;
  bestitem3 := nil;
  bestleveltime := -1;
  bestleveltime2 := -1;
  for i := history.rover downto history.rover - history.numitems + 1 do
  begin
    if i < 0 then
      hpos := NUMPLAYERTRACEHISTORY + i
    else
      hpos := i;
    item := @history.data[hpos];
    if item.leveltime > mo.tracefollowtimestamp then
    begin
      if P_CheckSightXYZ(item.x, item.y, item.z, mo) then
      begin
        if item.leveltime > bestleveltime then
        begin
          bestleveltime := item.leveltime;
          bestitem := item;
        end;
        tracefromplayer := P_AproxDistance(item.x - p.mo.x, item.y - p.mo.y);
        if tracefromplayer < distfromplayer then
        begin
{          bestitem3 := item;
          distfromplayer := tracefromplayer;
{          _follow_item;
          result := true;
          exit;}
        end;
      end
      else if Sys_Random < 32 then
      begin
        P_LineTrace(mo.x, mo.y, mo.z, item.x, item.y, item.z, newx, newy, newz);
        if (newx = item.x) and (newy = item.y) then
        begin
          if item.leveltime > bestleveltime2 then
          begin
            bestleveltime2 := item.leveltime;
            bestitem2 := item;
          end;
          tracefromplayer := P_AproxDistance(item.x - p.mo.x, item.y - p.mo.y);
          if tracefromplayer < distfromplayer then
          begin
{            bestitem3 := item;
            distfromplayer := tracefromplayer;
{            _follow_item;
            result := true;
            exit;}
          end;
        end;
      end;
    end;
  end;

  if bestitem <> nil then
  begin
    item := bestitem;
    _follow_item;
    result := true;
    exit;
  end;

  if bestitem2 <> nil then
  begin
    item := bestitem2;
    _follow_item;
    result := true;
    exit;
  end;

  if bestitem3 <> nil then
  begin
    item := bestitem3;
    _follow_item;
    result := true;
    exit;
  end;

  maxdist := MAXINT;

  for i := history.rover downto history.rover - history.numitems + 1 do
  begin
    if i < 0 then
      hpos := NUMPLAYERTRACEHISTORY + i
    else
      hpos := i;
    item := @history.data[hpos];
    if item.leveltime > mo.tracefollowtimestamp then
    begin
      dist := P_AproxDistance(item.x - mo.x, item.y - mo.y);
      if dist < maxdist then
        if dist <> 0 then
        begin
          bestitem := item;
          maxdist := dist;
        end;
    end;
  end;

  if bestitem <> nil then
  begin
    item := bestitem;
    _follow_item;
    result := true;
    exit;
  end;
end;

const
  MAXFRIENDRADIUS = 128 * FRACUNIT;

var
  dplayermo: Pmobj_t;

function RIT_HandleDogsNearMe(mo: Pmobj_t): boolean;
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

  if mo.info.doomednum <> 888 then
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

procedure P_HandleDogsNearMe(const p: Pplayer_t);
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

  yh := MapBlockIntY(int64(viewy) + MAXFRIENDRADIUS - int64(bmaporgy));
  yl := MapBlockIntY(int64(viewy) - MAXFRIENDRADIUS - int64(bmaporgy));
  xh := MapBlockIntX(int64(viewx) + MAXFRIENDRADIUS - int64(bmaporgx));
  xl := MapBlockIntX(int64(viewx) - MAXFRIENDRADIUS - int64(bmaporgx));

  for y := yl to yh do
    for x := xl to xh do
      P_BlockThingsIterator(x, y, RIT_HandleDogsNearMe);
end;

end.
