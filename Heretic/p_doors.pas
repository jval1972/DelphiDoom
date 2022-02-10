//------------------------------------------------------------------------------
//
//  DelphiHeretic is a source port of the game Heretic and it is
//  based on original Linux Doom as published by "id Software", on
//  Heretic source as published by "Raven" software and DelphiDoom
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
// DESCRIPTION: Door animation code (opening/closing)
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit p_doors;

interface

uses
  doomdef,
  z_zone,
  m_fixed,
  p_mobj_h,
  p_spec,
  r_defs,
  s_sound,
// State.
  
// Data.
  sounddata;

//==============================================================================
//
// T_VerticalDoor
//
//==============================================================================
procedure T_VerticalDoor(door: Pvldoor_t);

//==============================================================================
//
// EV_DoDoor
//
//==============================================================================
function EV_DoDoor(line: Pline_t; _type: vldoor_e; speed: fixed_t = VDOORSPEED): integer;

//==============================================================================
//
// EV_VerticalDoor
//
//==============================================================================
procedure EV_VerticalDoor(line: Pline_t; thing: Pmobj_t);

//==============================================================================
//
// P_SpawnDoorCloseIn30
//
//==============================================================================
procedure P_SpawnDoorCloseIn30(sec: Psector_t);

//==============================================================================
//
// P_SpawnDoorRaiseIn5Mins
//
//==============================================================================
procedure P_SpawnDoorRaiseIn5Mins(sec: Psector_t; secnum: integer);

implementation

uses
  d_player,
  h_strings,
  p_tick,
  p_setup,
  p_floor;

//==============================================================================
//
// VERTICAL DOORS
//
// T_VerticalDoor
//
//==============================================================================
procedure T_VerticalDoor(door: Pvldoor_t);
var
  res: result_e;
begin
  case door.direction of
    0:
      begin
  // WAITING
        dec(door.topcountdown);
        if door.topcountdown = 0 then
        begin
          case door._type of
            normal:
              begin
                door.direction := -1; // time to go back down
                S_StartSound(@door.sector.soundorg, Ord(sfx_doropn));
              end;
            close30ThenOpen:
              begin
                door.direction := 1;
                S_StartSound(@door.sector.soundorg, Ord(sfx_doropn));
              end;
          end;
        end;
      end;
    2:
      begin
  //  INITIAL WAIT
        dec(door.topcountdown);
        if door.topcountdown = 0 then
        begin
          case door._type of
            raiseIn5Mins:
              begin
                door.direction := 1;
                door._type := normal;
                S_StartSound(@door.sector.soundorg, Ord(sfx_doropn));
              end;
          end;
        end;
      end;

   -1:
      begin
  // DOWN
        res := T_MovePlane(door.sector, door.speed, door.sector.floorheight,
                  false, 1, door.direction);
        if res = pastdest then
        begin
          case door._type of
            normal,
            close:
              begin
                door.sector.specialdata := nil;
                P_RemoveThinker(@door.thinker);  // unlink and free
                S_StartSound(@door.sector.soundorg, Ord(sfx_dorcls));
              end;
            close30ThenOpen:
              begin
                door.direction := 0;
                door.topcountdown := TICRATE * 30;
              end;
          end;
        end
        else if res = crushed then
        begin
          case door._type of
            close:    // DO NOT GO BACK UP!
              begin
              end;
          else
            begin
              door.direction := 1;
              S_StartSound(@door.sector.soundorg, Ord(sfx_doropn));
            end;
          end;
        end;
      end;
    1:
      begin
  // UP
        res := T_MovePlane(door.sector, door.speed, door.topheight,
                  false, 1, door.direction);
        if res = pastdest then
        begin
          case door._type of
            normal:
              begin
                door.direction := 0; // wait at top
                door.topcountdown := door.topwait;
              end;
            close30ThenOpen,
            open:
              begin
                door.sector.specialdata := nil;
                P_RemoveThinker(@door.thinker); // unlink and free
                S_StopSound(Pmobj_t(@door.sector.soundorg));
              end;
          end;
        end;
      end;
  end;
end;

//==============================================================================
//
// EV_DoDoor
//
//==============================================================================
function EV_DoDoor(line: Pline_t; _type: vldoor_e; speed: fixed_t = VDOORSPEED): integer;
var
  initial: boolean;
  secnum: integer;
  sec: Psector_t;
  door: Pvldoor_t;
begin
  secnum := -1;
  result := 0;

  initial := true;
  while (secnum >= 0) or initial do
  begin
    initial := false;
    secnum := P_FindSectorFromLineTag(line, secnum);
    if secnum < 0 then
      break;

    sec := @sectors[secnum];
    if sec.specialdata <> nil then
      continue;

    // new door thinker
    result := 1;
    door := Z_Malloc(SizeOf(vldoor_t), PU_LEVSPEC, nil);
    P_AddThinker(@door.thinker);
    sec.specialdata := door;

    door.thinker._function.acp1 := @T_VerticalDoor;
    door.sector := sec;
    door._type := _type;
    door.topwait := VDOORWAIT;
    door.speed := speed;

    case _type of
      close:
        begin
          door.topheight := P_FindLowestCeilingSurrounding(sec);
          door.topheight := door.topheight - 4 * FRACUNIT;
          door.direction := -1;
          S_StartSound(@door.sector.soundorg, Ord(sfx_doropn));
        end;

      close30ThenOpen:
        begin
          door.topheight := sec.ceilingheight;
          door.direction := -1;
          S_StartSound(@door.sector.soundorg, Ord(sfx_doropn));
        end;

      normal,
      open:
        begin
          door.direction := 1;
          door.topheight := P_FindLowestCeilingSurrounding(sec);
          door.topheight := door.topheight - 4 * FRACUNIT;
          if door.topheight <> sec.ceilingheight then
            S_StartSound(@door.sector.soundorg, Ord(sfx_doropn));
        end;
    end;
  end;
end;

//==============================================================================
//
// EV_VerticalDoor : open a door manually, no tag value
//
//==============================================================================
procedure EV_VerticalDoor(line: Pline_t; thing: Pmobj_t);
var
  player: Pplayer_t;
  sec: Psector_t;
  door: Pvldoor_t;
  side: integer;
begin
  side := 0;  // only front sides can be used

  // Check for locks
  player := thing.player;

  case line.special of
    26, // Blue Lock
    32:
      begin
        if player = nil then
          exit;

        if not player.keys[Ord(key_blue)] then
        begin
          player._message := TXT_NEEDBLUEKEY;
          S_StartSound(nil, Ord(sfx_plroof));
          exit;
        end;
      end;
    27, // Yellow Lock
    34:
      begin
        if player = nil then
          exit;

        if not player.keys[Ord(key_yellow)] then
        begin
          player._message := TXT_NEEDYELLOWKEY;
          S_StartSound(nil, Ord(sfx_plroof));
          exit;
        end;
      end;
    28, // Green Lock
    33:
      begin
        if player = nil then
          exit;

        if not player.keys[Ord(key_green)] then
        begin
          player._message := TXT_NEEDGREENKEY;
          S_StartSound(nil, Ord(sfx_plroof));
          exit;
        end;
      end;
  end;

  // if the sector has an active thinker, use it
  sec := sides[line.sidenum[side xor 1]].sector;

  if sec.specialdata <> nil then
  begin
    door := sec.specialdata;
    case line.special of
       1, // ONLY FOR "RAISE" DOORS, NOT "OPEN"s
      26,
      27,
      28:
        begin
          if door.direction = -1 then
            door.direction := 1 // go back up
          else
          begin
            if thing.player = nil then
              exit; // JDC: bad guys never close doors

            door.direction := -1; // start going down immediately
          end;
          exit;
        end;
    end;
  end;

  S_StartSound(@sec.soundorg, Ord(sfx_doropn));

  // new door thinker
  door := Z_Malloc(SizeOf(vldoor_t), PU_LEVSPEC, nil);
  P_AddThinker(@door.thinker);
  sec.specialdata := door;
  door.thinker._function.acp1 := @T_VerticalDoor;
  door.sector := sec;
  door.direction := 1;
  door.speed := VDOORSPEED;
  door.topwait := VDOORWAIT;

  case line.special of
     1,
    26,
    27,
    28:
      door._type := normal;
    31,
    32,
    33,
    34:
      begin
        door._type := open;
        line.special := 0;
      end;
  end;

  // find the top and bottom of the movement range
  door.topheight := P_FindLowestCeilingSurrounding(sec);
  door.topheight := door.topheight - 4 * FRACUNIT;
end;

//==============================================================================
// P_SpawnDoorCloseIn30
//
// Spawn a door that closes after 30 seconds
//
//==============================================================================
procedure P_SpawnDoorCloseIn30(sec: Psector_t);
var
  door: Pvldoor_t;
begin
  door := Z_Malloc(SizeOf(vldoor_t), PU_LEVSPEC, nil);

  P_AddThinker(@door.thinker);

  sec.specialdata := door;
  sec.special := 0;

  door.thinker._function.acp1 := @T_VerticalDoor;
  door.sector := sec;
  door.direction := 0;
  door._type := normal;
  door.speed := VDOORSPEED;
  door.topcountdown := 30 * TICRATE;
end;

//==============================================================================
// P_SpawnDoorRaiseIn5Mins
//
// Spawn a door that opens after 5 minutes
//
//==============================================================================
procedure P_SpawnDoorRaiseIn5Mins(sec: Psector_t; secnum: integer);
var
  door: Pvldoor_t;
begin
  door := Z_Malloc(SizeOf(vldoor_t), PU_LEVSPEC, nil);

  P_AddThinker(@door.thinker);

  sec.specialdata := door;
  sec.special := 0;

  door.thinker._function.acp1 := @T_VerticalDoor;
  door.sector := sec;
  door.direction := 2;
  door._type := raiseIn5Mins;
  door.speed := VDOORSPEED;
  door.topheight := P_FindLowestCeilingSurrounding(sec);
  door.topheight := door.topheight - 4 * FRACUNIT;
  door.topwait := VDOORWAIT;
  door.topcountdown := 5 * 60 * TICRATE;
end;

end.
