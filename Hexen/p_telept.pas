//------------------------------------------------------------------------------
//
//  DelphiHexen: A modified and improved Hexen port for Windows
//  based on original Linux Doom as published by "id Software", on
//  Hexen source as published by "Raven" software and DelphiDoom
//  as published by Jim Valavanis.
//  Copyright (C) 2004-2016 by Jim Valavanis
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
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit p_telept;

interface

uses
  m_fixed,
  tables,
  p_mobj_h,
  r_defs;

//
// TELEPORTATION
//
function P_Teleport(thing: Pmobj_t; x, y: fixed_t; angle: angle_t; useFog: boolean): boolean;

function EV_Teleport(tid: integer; thing: Pmobj_t; fog: boolean): boolean;

implementation

uses
  d_player,
  i_system,
  info_h,
  m_rnd,
  p_map,
  p_mobj,
  p_local,
  s_sound,
  sounds,
  doomdef;

function P_Teleport(thing: Pmobj_t; x, y: fixed_t; angle: angle_t; useFog: boolean): boolean;
var
  oldx: fixed_t;
  oldy: fixed_t;
  oldz: fixed_t;
  aboveFloor: fixed_t;
  fogDelta: fixed_t;
  player: Pplayer_t;
  an: angle_t;
  fog: Pmobj_t;
begin
  oldx := thing.x;
  oldy := thing.y;
  oldz := thing.z;
  aboveFloor := thing.z - thing.floorz;
  if not P_TeleportMove(thing, x, y) then
  begin
    result := false;
    exit;
  end;

  if thing.player <> nil then
  begin
    player := thing.player;
    if (player.powers[Ord(pw_flight)] <> 0) and (aboveFloor <> 0) then
    begin
      thing.z := thing.floorz + aboveFloor;
      if thing.z + thing.height > thing.ceilingz then
        thing.z := thing.ceilingz - thing.height;
      player.viewz := thing.z + player.viewheight;
    end
    else
    begin
      thing.z := thing.floorz;
      player.viewz := thing.z + player.viewheight;
      if useFog then
        player.lookdir := 0;
    end;
  end
  else if thing.flags and MF_MISSILE <> 0 then
  begin
    thing.z := thing.floorz + aboveFloor;
    if thing.z + thing.height > thing.ceilingz then
      thing.z := thing.ceilingz - thing.height;
  end
  else
    thing.z := thing.floorz;

  // Spawn teleport fog at source and destination
  if useFog then
  begin
    if thing.flags and MF_MISSILE <> 0 then
      fogDelta := 0
    else
      fogDelta := TELEFOGHEIGHT;
    fog := P_SpawnMobj(oldx, oldy, oldz + fogDelta, Ord(MT_TFOG));
    S_StartSound(fog, Ord(SFX_TELEPORT));
    an := angle shr ANGLETOFINESHIFT;
    fog := P_SpawnMobj(x + 20 * finecosine[an],
                       y + 20 * finesine[an],
                       thing.z + fogDelta,
                       Ord(MT_TFOG));
    S_StartSound(fog, Ord(SFX_TELEPORT));
    if (thing.player <> nil) and (Pplayer_t(thing.player).powers[Ord(pw_speed)] = 0) then
    begin // Freeze player for about .5 sec
      thing.reactiontime := 18;
    end;
    thing.angle := angle;
  end;
  if thing.flags2 and MF2_FLOORCLIP <> 0 then
  begin
    if (thing.z = Psubsector_t(thing.subsector).sector.floorheight) and
       (P_GetThingFloorType(thing) > FLOOR_SOLID) then
      thing.floorclip := 10 * FRACUNIT
    else
      thing.floorclip := 0;
  end;

  if thing.flags and MF_MISSILE <> 0 then
  begin
    an := angle shr ANGLETOFINESHIFT;
    thing.momx := FixedMul(thing.info.speed, finecosine[an]);
    thing.momy := FixedMul(thing.info.speed, finesine[an]);
  end
  else if useFog then // no fog doesn't alter the player's momentums
  begin
    thing.momx := 0;
    thing.momy := 0;
    thing.momz := 0;
  end;
  result := true;
end;

//==========================================================================
//
// EV_Teleport
//
//==========================================================================

function EV_Teleport(tid: integer; thing: Pmobj_t; fog: boolean): boolean;
var
  i: integer;
  count: integer;
  mo: Pmobj_t;
  searcher: integer;
begin
  if thing = nil then
  begin // Teleport function called with an invalid mobj
    result := false;
    exit;
  end;

  if thing.flags2 and MF2_NOTELEPORT <> 0 then
  begin
    result := false;
    exit;
  end;

  count := 0;
  searcher := -1;
  while P_FindMobjFromTID(tid, @searcher) <> nil do
    inc(count);

  if count = 0 then
  begin
    result := false;
    exit;
  end;

  count := 1 + (P_Random mod count);
  searcher := -1;
  mo := nil;
  for i := 0 to count - 1 do
  begin
    mo := P_FindMobjFromTID(tid, @searcher);
  end;
  if mo = nil then
    I_Error('EV_Teleport(): Can''t find teleport mapspot');
  result := P_Teleport(thing, mo.x, mo.y, mo.angle, fog);
end;

end.
