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
//  Player trace history
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit p_playertrace;

interface

uses
  doomdef,
  d_player,
  p_mobj_h,
  m_fixed,
  tables;

type
  playertrace_t = record
    x, y, z: fixed_t;
    angle: angle_t;
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

//==============================================================================
//
// P_PlayerHistoryNotify
//
//==============================================================================
procedure P_PlayerHistoryNotify(const p: Pplayer_t);

//==============================================================================
//
// P_ClearPlayerHistory
//
//==============================================================================
procedure P_ClearPlayerHistory(const p: Pplayer_t);

//==============================================================================
//
// P_FollowPlayer
//
//==============================================================================
function P_FollowPlayer(const mo: Pmobj_t; const p: Pplayer_t): boolean;

//==============================================================================
//
// P_GetPlayerTraceAtPos
//
//==============================================================================
function P_GetPlayerTraceAtPos(const p: Pplayer_t; const at: Integer): Pplayertrace_t;

//==============================================================================
//
// P_GetPlayerTraceAtTime
//
//==============================================================================
function P_GetPlayerTraceAtTime(const p: Pplayer_t; const tm: Integer): Pplayertrace_t;

implementation

uses
  d_delphi,
  g_game,
  m_rnd,
  p_common,
  p_maputl,
  p_tick,
  p_sight,
  r_main;

//==============================================================================
//
// P_PlayerHistoryNotify
//
//==============================================================================
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
    if dist < HISTORYIGNOREDISTANCE div 2 then
      exit;
    if dist < HISTORYIGNOREDISTANCE then
      if P_CheckSightXYZ(item.x, item.y, item.z, pmo) then
        exit;
    nrover := history.rover + 1;
    if nrover >= NUMPLAYERTRACEHISTORY then
      nrover := nrover - NUMPLAYERTRACEHISTORY;
  end;

  item := @history.data[nrover];
  item.x := pmo.x;
  item.y := pmo.y;
  item.z := pmo.z;
  item.angle := pmo.angle;
  item.leveltime := leveltime;
  history.rover := nrover;
  if history.numitems < NUMPLAYERTRACEHISTORY then
    inc(history.numitems);
end;

//==============================================================================
//
// P_ClearPlayerHistory
//
//==============================================================================
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

//==============================================================================
//
// P_FollowPlayer
//
//==============================================================================
function P_FollowPlayer(const mo: Pmobj_t; const p: Pplayer_t): boolean;
var
  pid: integer;
  history: Pplayertracehistory_t;
  i: integer;
  hpos: integer;
  item: Pplayertrace_t;
  tmp: playertrace_t;
  bestitem: Pplayertrace_t;
  bestitem2: Pplayertrace_t;
  bestitem3: Pplayertrace_t;
  bestleveltime: fixed_t;
  bestleveltime2: fixed_t;
  dist: fixed_t;
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
    mo.tracez := item.z;
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

  if mo.flags4_ex and MF4_EX_TRACEDEFINED <> 0 then
  begin
    dist := P_AproxDistance(mo.tracex - mo.x, mo.tracey - mo.y);
    if (dist > HISTORYIGNOREDISTANCE) and P_CheckSightXYZ(mo.tracex, mo.tracey, mo.tracez, mo) then
    begin
      tmp.x := mo.tracex;
      tmp.y := mo.tracey;
      tmp.z := mo.tracez;
      tmp.leveltime := mo.tracefollowtimestamp;
      item := @tmp;
      _follow_item;
      result := true;
      exit;
    end;
  end;

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
          bestitem3 := item;
          distfromplayer := tracefromplayer;
        end;
      end
      else if N_Random < 32 then
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
            bestitem3 := item;
            distfromplayer := tracefromplayer;
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
end;

//==============================================================================
//
// P_GetPlayerTraceAtPos
//
//==============================================================================
function P_GetPlayerTraceAtPos(const p: Pplayer_t; const at: Integer): Pplayertrace_t;
var
  pid: integer;
  history: Pplayertracehistory_t;
  rover: integer;
begin
  pid := PlayerToId(p);
  if (pid < 0) or not playeringame[pid] then
  begin
    result := nil;
    exit;
  end;

  history := @playerhistory[pid];
  if history.numitems = 0 then
  begin
    result := nil;
    exit;
  end;

  if at >= history.numitems then
    rover := history.numitems - 1
  else
    rover := at;

  rover := history.rover - rover;
  if rover > NUMPLAYERTRACEHISTORY then
    rover := rover - NUMPLAYERTRACEHISTORY
  else if rover < 0 then
    rover := rover + NUMPLAYERTRACEHISTORY;

  result := @history.data[rover];
end;

//==============================================================================
//
// P_GetPlayerTraceAtTime
//
//==============================================================================
function P_GetPlayerTraceAtTime(const p: Pplayer_t; const tm: Integer): Pplayertrace_t;
var
  pid: integer;
  history: Pplayertracehistory_t;
  hpos, i: integer;
  maxtm, looktm, dt: integer;
  item: Pplayertrace_t;
begin
  pid := PlayerToId(p);
  if (pid < 0) or not playeringame[pid] then
  begin
    result := nil;
    exit;
  end;

  history := @playerhistory[pid];
  if history.numitems = 0 then
  begin
    result := nil;
    exit;
  end;

  maxtm := MAXINT;
  looktm := leveltime - tm;
  result := nil;

  for i := history.rover downto history.rover - history.numitems + 1 do
  begin
    if i < 0 then
      hpos := NUMPLAYERTRACEHISTORY + i
    else
      hpos := i;
    item := @history.data[hpos];
    dt := Abs(looktm - item.leveltime);
    if dt < maxtm then
    begin
      result := item;
      if dt = 0 then
        exit;
      maxtm := dt;
    end;
  end;
end;

end.
