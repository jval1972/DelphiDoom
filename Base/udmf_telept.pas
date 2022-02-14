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
//  Teleportation. (UDMF & Hexen)
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit udmf_telept;

interface

uses
  m_fixed,
  tables,
  p_mobj_h,
  r_defs;

//==============================================================================
//
// PH_Teleport
//
// TELEPORTATION
//
//==============================================================================
function PH_Teleport(thing: Pmobj_t; x, y: fixed_t; angle: angle_t; useFog: boolean; floorz, ceilingz: fixed_t): boolean;

//==============================================================================
//
// EVH_Teleport
//
//==============================================================================
function EVH_Teleport(tid: integer; thing: Pmobj_t; fog: boolean): boolean;

const
  TELEPORTZOOM = 15 * FRACUNIT;

const
  TELEFOGHEIGHT = 32 * FRACUNIT;

implementation

uses
  d_player,
  i_system,
  info_h,
  m_rnd,
  p_map,
  p_mobj,
  p_local,
  {$IFDEF DOOM_OR_STRIFE}
  p_terrain,
  {$ENDIF}
  s_sound,
  sounddata,
  {$IFNDEF HEXEN}
  d_think,
  p_tick,
  udmf_mobj,
  {$ENDIF}
  doomdef;

//==============================================================================
//
// PH_Teleport
//
//==============================================================================
function PH_Teleport(thing: Pmobj_t; x, y: fixed_t; angle: angle_t; useFog: boolean; floorz, ceilingz: fixed_t): boolean;
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

  thing.floorz := floorz;
  thing.ceilingz := ceilingz;

  if thing.player <> nil then
  begin
    player := thing.player;
    {$IFDEF HERETIC_OR_HEXEN}
    if (player.powers[Ord(pw_flight)] <> 0) and (aboveFloor <> 0) then
    begin
      thing.z := thing.floorz + aboveFloor;
      if thing.z + thing.height > thing.ceilingz then
        thing.z := thing.ceilingz - thing.height;
      player.viewz := thing.z + player.viewheight;
    end
    else
    {$ENDIF}
    begin
      thing.z := thing.floorz;
      player.viewz := thing.z + player.viewheight;
      if useFog then
      begin
        player.lookdir := 0;
        player.lookdir16 := 0; // JVAL Smooth Look Up/Down
      end;
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
    {$IFDEF HEXEN}
    S_StartSound(fog, Ord(SFX_TELEPORT));
    {$ELSE}
    S_StartSound(fog, Ord(sfx_telept));
    {$ENDIF}
    an := angle shr ANGLETOFINESHIFT;
    fog := P_SpawnMobj(x + 20 * finecosine[an],
                       y + 20 * finesine[an],
                       thing.z + fogDelta,
                       Ord(MT_TFOG));
    {$IFDEF HEXEN}
    S_StartSound(fog, Ord(SFX_TELEPORT));
    {$ELSE}
    S_StartSound(fog, Ord(sfx_telept));
    {$ENDIF}
    if (thing.player <> nil) {$IFDEF HEXEN}and (Pplayer_t(thing.player).powers[Ord(pw_speed)] = 0){$ENDIF} then
    begin // Freeze player for about .5 sec
      thing.reactiontime := 18;
    end;
    if thing.player <> nil then
      Pplayer_t(thing.player).teleporttics := TELEPORTZOOM;
    thing.angle := angle;
  end;

  {$IFDEF DOOM}
  if thing.flags2_ex and  MF2_EX_FLOORCLIP <> 0 then
  begin
    if (thing.z = thing.floorz) and
       (P_GetThingFloorType(thing) > FLOOR_SOLID) then
      thing.floorclip := FOOTCLIPSIZE
    else
      thing.floorclip := 0;
  end;
  {$ENDIF}
  {$IFDEF HERETIC}
  if (thing.flags2 and MF2_FOOTCLIP <> 0) and (P_GetThingFloorType(thing) <> FLOOR_SOLID) then
    thing.flags2 := thing.flags2 or MF2_FEETARECLIPPED
  else if thing.flags2 and MF2_FEETARECLIPPED <> 0 then
    thing.flags2 := thing.flags2 and not MF2_FEETARECLIPPED;
  {$ENDIF}
  {$IFDEF HEXEN}
  if thing.flags2 and MF2_FLOORCLIP <> 0 then
  begin
    if (thing.z = thing.floorz) and
       (P_GetThingFloorType(thing) > FLOOR_SOLID) then
      thing.floorclip := FOOTCLIPSIZE
    else
      thing.floorclip := 0;
  end;
  {$ENDIF}
  {$IFDEF STRIFE}
  if thing.flags and  MF_FEETCLIPPED <> 0 then
  begin
    if (thing.z = thing.floorz) and
       (P_GetThingFloorType(thing) > FLOOR_SOLID) then
      thing.floorclip := FOOTCLIPSIZE
    else
      thing.floorclip := 0;
  end;
  {$ENDIF}

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

//==============================================================================
//
// EVH_Teleport
//
//==============================================================================
function EVH_Teleport(tid: integer; thing: Pmobj_t; fog: boolean): boolean;
var
  i: integer;
  count: integer;
  mo: Pmobj_t;
  searcher: {$IFDEF HEXEN}integer{$ELSE}Pthinker_t{$ENDIF};
begin
  if thing = nil then
  begin // Teleport function called with an invalid mobj
    result := false;
    exit;
  end;

  {$IFDEF DOOM_OR_STRIFE}
  if thing.flags2_ex and MF2_EX_NOTELEPORT <> 0 then
  {$ELSE}
  if thing.flags2 and MF2_NOTELEPORT <> 0 then
  {$ENDIF}
  begin
    result := false;
    exit;
  end;

  count := 0;
  searcher := {$IFDEF HEXEN}-1{$ELSE}@thinkercap{$ENDIF};
  while P_FindMobjFromTID(tid, {$IFDEF HEXEN}@searcher{$ELSE}Pointer(searcher){$ENDIF}) <> nil do
    inc(count);

  if count = 0 then
  begin
    result := false;
    exit;
  end;

  count := 1 + (P_Random mod count);
  searcher := {$IFDEF HEXEN}-1{$ELSE}@thinkercap{$ENDIF};
  mo := nil;
  for i := 0 to count - 1 do
  begin
    mo := P_FindMobjFromTID(tid, {$IFDEF HEXEN}@searcher{$ELSE}Pointer(searcher){$ENDIF});
  end;
  if mo = nil then
    I_Error('EV_Teleport(): Can''t find teleport mapspot');

  result := PH_Teleport(thing, mo.x, mo.y, mo.angle, fog, mo.floorz, mo.ceilingz);
end;

end.
