//------------------------------------------------------------------------------
//
//  DelphiDoom: A modified and improved DOOM engine for Windows
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 2004-2013 by Jim Valavanis
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
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit p_common;

interface

uses
  p_mobj_h;

function P_CheckStateParams(actor: Pmobj_t; const numparms: integer = -1): boolean;

{$IFDEF HEXEN}
procedure P_BulletSlope(mo: Pmobj_t);
{$ENDIF}

procedure A_GoTo(actor: Pmobj_t);

procedure A_GoToIfCloser(actor: Pmobj_t);

procedure A_GoToIfHealthLower(actor: Pmobj_t);

procedure A_ConsoleCommand(actor: Pmobj_t);

procedure A_SetFrightened(actor: Pmobj_t);

procedure A_UnSetFrightened(actor: Pmobj_t);

implementation

uses
  d_delphi,
  i_system,
  c_con,
  info_h,
  p_mobj,
  p_pspr,
  p_map,
  p_maputl,
  sc_states,
  {$IFDEF HEXEN}
  tables,
  d_player,
  {$ENDIF}
  m_fixed,
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

function P_CheckStateParams(actor: Pmobj_t; const numparms: integer = -1): boolean;
begin
  if numparms = 0 then
  begin
    I_Warning('P_CheckStateParams(): Expected params can not be 0'#13#10, [numparms]);
    result := false;
    exit;
  end;

  if actor.state.params = nil then
  begin
    I_Warning('P_CheckStateParams(): Parameter list is null');
    if numparms > 0 then
      I_Warning(', %d parameters expected', [numparms]);
    I_Warning(#13#10);
    result := false;
    exit;
  end;

  if numparms <> -1 then
    if actor.state.params.Count <> numparms then
    begin
      I_Warning('P_CheckStateParams(): Parameter list has %d parameters, but %d parameters expected'#13#10, [actor.state.params.Count, numparms]);
      result := false;
      exit;
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

end.


