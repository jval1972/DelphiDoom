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
//  Teleportation.
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit p_telept;

interface

uses
  m_fixed,
  p_mobj_h,
  r_defs;

//==============================================================================
// EV_Teleport
//
// TELEPORTATION
//
//==============================================================================
function EV_Teleport(line: Pline_t; side: integer; thing: Pmobj_t): integer;

//==============================================================================
//
// EV_SilentTeleport
//
//==============================================================================
function EV_SilentTeleport(line: Pline_t; side: integer; thing: Pmobj_t): integer;

//==============================================================================
//
// EV_SilentLineTeleport
//
//==============================================================================
function EV_SilentLineTeleport(line: Pline_t; side: integer; thing: Pmobj_t; reverse: boolean): integer;

implementation

uses
  d_delphi,
  doomdef,
  doomstat,
  d_think,
  d_player,
  info_h,
  g_game,
  p_3dfloors,
  p_setup,
  p_tick,
  p_mobj,
  p_map,
  p_maputl,
  p_spec,
  p_user,
  r_main,
  s_sound,
  sounddata,
  udmf_telept,
  tables;

//==============================================================================
//
// EV_Teleport
//
//==============================================================================
function EV_Teleport(line: Pline_t; side: integer; thing: Pmobj_t): integer;
var
  i: integer;
  tag: integer;
  m: Pmobj_t;
  fog: Pmobj_t;
  an: LongWord;
  thinker: Pthinker_t;
  sector: Psector_t;
  p: Pplayer_t;
  oldx: fixed_t;
  oldy: fixed_t;
  oldz: fixed_t;
  dz: fixed_t; // JVAL: 3d Floors
begin
  // don't teleport missiles
  if thing.flags and MF_MISSILE <> 0 then
  begin
    result := 0;
    exit;
  end;

  // 19/9/2009 JVAL: NO TELEPORT FLAG
  if thing.flags2_ex and MF2_EX_NOTELEPORT <> 0 then
  begin
    result := 0;
    exit;
  end;

  // Don't teleport if hit back of line,
  //  so you can get out of teleporter.
  if side = 1 then
  begin
    result := 0;
    exit;
  end;

  tag := line.tag;
  for i := 0 to numsectors - 1 do
  begin
    if sectors[i].tag = tag then
    begin
      thinker := thinkercap.next;
      while thinker <> @thinkercap do
      begin
        // not a mobj
        if @thinker._function.acp1 <> @P_MobjThinker then
        begin
          thinker := thinker.next;
          continue;
        end;

        m := Pmobj_t(thinker);

        // not a teleportman
        if m._type <> Ord(MT_TELEPORTMAN) then
        begin
          thinker := thinker.next;
          continue;
        end;

        sector := Psubsector_t(m.subsector).sector;
        // wrong sector
        if sector <> @sectors[i] then
        begin
          thinker := thinker.next;
          continue;
        end;

        oldx := thing.x;
        oldy := thing.y;
        oldz := thing.z;
        dz := oldz - thing.floorz;

        if not P_TeleportMove(thing, m.x, m.y) then
        begin
          result := 0;
          exit;
        end;

        // fraggle: this was changed in final doom,
        // problem between normal doom2 1.9 and final doom
        //
        // Note that although chex.exe is based on Final Doom,
        // it does not have this quirk.

        if (gameversion < exe_final) or (gameversion = exe_chex) or (G_PlayingEngineVersion in [VERSION111..VERSION118]) then
          thing.z := thing.floorz;  //fixme: not needed?

        // JVAL: 3d Floors
        if (G_PlayingEngineVersion >= VERSION122) and (P_3dFloorNumber(m) > 0) then
        begin
          thing.floorz := m.floorz;
          thing.ceilingz := m.ceilingz;
          thing.z := thing.floorz + dz;
        end;

        p := Pplayer_t(thing.player);
        if p <> nil then
        begin
          p.viewz := thing.z + p.viewheight;
          p.lookdir := 0;
          p.lookdir16 := 0; // JVAL Smooth Look Up/Down
        end;

        // spawn teleport fog at source and destination
        fog := P_SpawnMobj(oldx, oldy, oldz, Ord(MT_TFOG));
        S_StartSound(fog, Ord(sfx_telept));
        {$IFDEF FPC}
        an := _SHRW(m.angle, ANGLETOFINESHIFT);
        {$ELSE}
        an := m.angle shr ANGLETOFINESHIFT;
        {$ENDIF}
        fog := P_SpawnMobj(m.x + 20 * finecosine[an],
                           m.y + 20 * finesine[an],
                           thing.z, Ord(MT_TFOG));

        // emit sound, where?
        S_StartSound(fog, Ord(sfx_telept));

        // don't move for a bit
        if thing.player <> nil then
        begin
          thing.reactiontime := 18;
          Pplayer_t(thing.player).teleporttics := TELEPORTZOOM;
        end;

        thing.angle := m.angle;
        thing.momx := 0;
        thing.momy := 0;
        thing.momz := 0;
        result := 1;
        exit;
      end;
    end;
  end;
  result := 0;
end;

//==============================================================================
// EV_SilentTeleport
//
// Silent TELEPORTATION, by Lee Killough
// Primarily for rooms-over-rooms etc.
//
//==============================================================================
function EV_SilentTeleport(line: Pline_t; side: integer; thing: Pmobj_t): integer;
var
  i: integer;
  m: Pmobj_t;
  th: Pthinker_t;
  z: fixed_t;
  s: fixed_t;
  c: fixed_t;
  momx: fixed_t;
  momy: fixed_t;
  angle: angle_t;
  player: Pplayer_t;
  deltaviewheight: fixed_t;
begin

  // don't teleport missiles
  // Don't teleport if hit back of line,
  // so you can get out of teleporter.

  if (side <> 0) or (thing.flags and MF_MISSILE <> 0) then
  begin
    result := 0;
    exit;
  end;

  i := -1;
  while P_FindSectorFromLineTag2(line, i) >= 0 do
  begin
    th := thinkercap.next;
    while th <> @thinkercap do
    begin
      m := Pmobj_t(th);
      if (@th._function.acp1 = @P_MobjThinker) and
         (m._type = Ord(MT_TELEPORTMAN)) and
         (pDiff(Psubsector_t(m.subsector).sector, sectors, SizeOf(sector_t)) = i) then
        begin
          // Height of thing above ground, in of mid-air teleports:
          z := thing.z - thing.floorz;

          // Get the angle between the exit thing and source linedef.
          // Rotate 90 degrees, so that walking perpendicularly across
          // teleporter linedef causes thing to exit in the direction
          // indicated by the exit thing.
          angle := R_PointToAngle2(0, 0, line.dx, line.dy) - m.angle + ANG90;

          // Sine, cosine of angle adjustment
          s := finesine[angle shr ANGLETOFINESHIFT];
          c := finecosine[angle shr ANGLETOFINESHIFT];

          // Momentum of thing crossing teleporter linedef
          momx := thing.momx;
          momy := thing.momy;

          // Whether this is a player, and if so, a pointer to its player_t
          player := thing.player;

          // Attempt to teleport, aborting if blocked
          if not P_TeleportMove(thing, m.x, m.y) then
          begin
            result := 0;
            exit;
          end;

          // Rotate thing according to difference in angles
          thing.angle := thing.angle + angle;

          // Adjust z position to be same height above ground as before
          thing.z := z + thing.floorz;

          // Rotate thing's momentum to come out of exit just like it entered
          thing.momx := FixedMul(momx, c) - FixedMul(momy, s);
          thing.momy := FixedMul(momy, c) + FixedMul(momx, s);

          // Adjust player's view, in there has been a height change
          // Voodoo dolls are excluded by making sure player.mo = thing.
          if player <> nil then
            if player.mo = thing then
            begin
              // Save the current deltaviewheight, used in stepping
              deltaviewheight := player.deltaviewheight;

              // Clear deltaviewheight, since we don't want any changes
              player.deltaviewheight := 0;

              // Set player's view according to the newly set parameters
              P_CalcHeight(player);

              // Reset the delta to have the same dynamics as before
              player.deltaviewheight := deltaviewheight;
            end;
          result := 1;
          exit;
        end;
        th := th.next;
      end;
  end;
  result := 0;
end;

//
// Silent linedef-based TELEPORTATION, by Lee Killough
// Primarily for rooms-over-rooms etc.
// This is the complete player-preserving kind of teleporter.
// It has advantages over the teleporter with thing exits.
//

// maximum fixed_t units to move object to avoid hiccups
const
  FUDGEFACTOR = 10;

//==============================================================================
//
// EV_SilentLineTeleport
//
//==============================================================================
function EV_SilentLineTeleport(line: Pline_t; side: integer; thing: Pmobj_t; reverse: boolean): integer;
var
  i: integer;
  l: Pline_t;
  pos: fixed_t;
  angle: angle_t;
  x: fixed_t;
  y: fixed_t;
  z: fixed_t;
  s: fixed_t;
  c: fixed_t;
  fudge: integer;
  player: Pplayer_t;
  stepdown: boolean;
  deltaviewheight: fixed_t;
begin
  if (side <> 0) or (thing.flags and MF_MISSILE <> 0) then
  begin
    result := 0;
    exit;
  end;

  i := -1;
  while P_FindLineFromLineTag2(line, i) >= 0 do
  begin
    l := @lines[i];
    if (l <> line) and (l.backsector <> nil) then
    begin
      // Get the thing's position along the source linedef
      // proff: Changed abs to D_abs (see m_fixed.h)
      if abs(line.dx) > abs(line.dy) then
        pos := FixedDiv(thing.x - line.v1.x, line.dx)
      else
        pos := FixedDiv(thing.y - line.v1.y, line.dy);

      // Get the angle between the two linedefs, for rotating
      // orientation and momentum. Rotate 180 degrees, and flip
      // the position across the exit linedef, if reversed.
      angle := R_PointToAngle2(0, 0, l.dx, l.dy) -
               R_PointToAngle2(0, 0, line.dx, line.dy);
      if reverse then
        pos := FRACUNIT - pos
      else
        angle := angle + ANG180;

      // Interpolate position across the exit linedef
      x := l.v2.x - FixedMul(pos, l.dx);
      y := l.v2.y - FixedMul(pos, l.dy);

      // Sine, cosine of angle adjustment
      s := finesine[angle shr ANGLETOFINESHIFT];
      c := finecosine[angle shr ANGLETOFINESHIFT];

      // Maximum distance thing can be moved away from interpolated
      // exit, to ensure that it is on the correct side of exit linedef
      fudge := FUDGEFACTOR;

      // Whether this is a player, and if so, a pointer to its player_t.
      // Voodoo dolls are excluded by making sure thing.player.mo==thing.
      player := nil;
      if thing.player <> nil then
        if Pplayer_t(thing.player).mo = thing then
          player := thing.player;

      // Whether walking towards first side of exit linedef steps down
      stepdown := l.frontsector.floorheight < l.backsector.floorheight;

      // Height of thing above ground
      z := thing.z - thing.floorz;

      // Side to exit the linedef on positionally.
      //
      // Notes:
      //
      // This flag concerns exit position, not momentum. Due to
      // roundoff error, the thing can land on either the left or
      // the right side of the exit linedef, and steps must be
      // taken to make sure it does not end up on the wrong side.
      //
      // Exit momentum is always towards side 1 in a reversed
      // teleporter, and always towards side 0 otherwise.
      //
      // Exiting positionally on side 1 is always safe, as far
      // as avoiding oscillations and stuck-in-wall problems,
      // but may not be optimum for non-reversed teleporters.
      //
      // Exiting on side 0 can cause oscillations if momentum
      // is towards side 1, as it is with reversed teleporters.
      //
      // Exiting on side 1 slightly improves player viewing
      // when going down a step on a non-reversed teleporter.

      if reverse or ((player <> nil) and stepdown) then
        side := 1
      else
        side := 0;

      // Make sure we are on correct side of exit linedef.
      while (P_PointOnLineSide(x, y, l) <> side) and (fudge >= 0) do
      begin
        dec(fudge);
        // proff: Changed abs to D_abs (see m_fixed.h)
        if abs(l.dx) > abs(l.dy) then
          y := y - (1 - 2 * intval(intval(l.dx < 0) <> side))
        else
          x := x + (1 - 2 * intval(intval(l.dy < 0) <> side));
      end;

      // Attempt to teleport, aborting if blocked
      if not P_TeleportMove(thing, x, y) then
      begin
        result := 0;
        exit;
      end;

      // Adjust z position to be same height above ground as before.
      // Ground level at the exit is measured as the higher of the
      // two floor heights at the exit linedef.
      thing.z := z + sides[l.sidenum[intval(stepdown)]].sector.floorheight;

      // Rotate thing's orientation according to difference in linedef angles
      thing.angle := thing.angle + angle;

      // Momentum of thing crossing teleporter linedef
      x := thing.momx;
      y := thing.momy;

      // Rotate thing's momentum to come out of exit just like it entered
      thing.momx := FixedMul(x, c) - FixedMul(y, s);
      thing.momy := FixedMul(y, c) + FixedMul(x, s);

      // Adjust a player's view, in there has been a height change
      if player <> nil then
      begin
        // Save the current deltaviewheight, used in stepping
        deltaviewheight := player.deltaviewheight;

        // Clear deltaviewheight, since we don't want any changes now
        player.deltaviewheight := 0;

        // Set player's view according to the newly set parameters
        P_CalcHeight(player);

        // Reset the delta to have the same dynamics as before
        player.deltaviewheight := deltaviewheight;
      end;

      result := 1;
      exit;
    end;
  end;
  result := 0;
end;

end.

