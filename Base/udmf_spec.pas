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
//  UDMF Special effects
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit udmf_spec;

interface

uses
  d_delphi,
  p_mobj_h,
  r_defs;

//==============================================================================
//
// P_FindSectorFromTag2
//
//==============================================================================
function P_FindSectorFromTag2(tag: integer; var start: integer): integer;

//==============================================================================
//
// P_ExecuteLineSpecial
//
//==============================================================================
function P_ExecuteLineSpecial(special: integer; args: PByteArray; line: Pline_t;
  side: integer; mo: Pmobj_t): boolean;

implementation

uses
  p_acs,
  p_setup,
  po_man;

//==============================================================================
//
// P_FindSectorFromTag2
//
//==============================================================================
function P_FindSectorFromTag2(tag: integer; var start: integer): integer;
var
  i: integer;
  b: Byte;
begin
  if IsIntegerInRange(tag, 0, 255) then
  begin
    b := tag;
    for i := start + 1 to numsectors - 1 do
    begin
      if b in sectors[i].moreids then
      begin
        result := i;
        start := result;
        exit;
      end;
      if sectors[i].tag = tag then
      begin
        result := i;
        start := result;
        exit;
      end;
    end;
  end
  else
  begin
    for i := start + 1 to numsectors - 1 do
      if sectors[i].tag = tag then
      begin
        result := i;
        start := result;
        exit;
      end;
  end;

  result := -1;
  start := -1;
end;

//==============================================================================
//
// P_ExecuteLineSpecial
//
//==============================================================================
function P_ExecuteLineSpecial(special: integer; args: PByteArray; line: Pline_t;
  side: integer; mo: Pmobj_t): boolean;
begin
  result := false;

  case special of

    1: // Poly Start Line
      begin
      end;

    2: // Poly Rotate Left
      begin
        result := EVH_RotatePoly(line, args, 1, false);
      end;

    3: // Poly Rotate Right
      begin
        result := EVH_RotatePoly(line, args, -1, false);
      end;

    4: // Poly Move
      begin
        result := EVH_MovePoly(line, args, false, false);
      end;

    5: // Poly Explicit Line:  Only used in initialization
      begin
      end;

    6: // Poly Move Times 8
      begin
        result := EVH_MovePoly(line, args, true, false);
      end;

    7: // Poly Door Swing
      begin
        result := EVH_OpenPolyDoor(line, args, PODOOR_SWING);
      end;

    8: // Poly Door Slide
      begin
        result := EVH_OpenPolyDoor(line, args, PODOOR_SLIDE);
      end;

    10: // Door Close
      begin
        result := EVH_DoDoor(line, args, DREV_CLOSE);
      end;

    11: // Door Open
      begin
        if args[0] = 0 then
          result := EVH_VerticalDoor(line, mo)
        else
          result := EVH_DoDoor(line, args, DREV_OPEN);
      end;

    12: // Door Raise
      begin
        if args[0] = 0 then
          result := EVH_VerticalDoor(line, mo)
        else
          result := EVH_DoDoor(line, args, DREV_NORMAL);
      end;

    13: // Door Locked_Raise
      begin
        if CheckedLockedDoor(mo, args[3]) then
        begin
          if args[0] = 0 then
            result := EVH_VerticalDoor(line, mo)
          else
            result := EVH_DoDoor(line, args, DREV_NORMAL);
        end;
      end;

    20: // Floor Lower by Value
      begin
        result := EVH_DoFloor(line, args, FLEV_LOWERFLOORBYVALUE);
      end;

    21: // Floor Lower to Lowest
      begin
        result := EVH_DoFloor(line, args, FLEV_LOWERFLOORTOLOWEST);
      end;

    22: // Floor Lower to Nearest
      begin
        result := EVH_DoFloor(line, args, FLEV_LOWERFLOOR);
      end;

    23: // Floor Raise by Value
      begin
        result := EVH_DoFloor(line, args, FLEV_RAISEFLOORBYVALUE);
      end;

    24: // Floor Raise to Highest
      begin
        result := EVH_DoFloor(line, args, FLEV_RAISEFLOOR);
      end;

    25: // Floor Raise to Nearest
      begin
        result := EVH_DoFloor(line, args, FLEV_RAISEFLOORTONEAREST);
      end;

    26: // Stairs Build Down Normal
      begin
        result := EVH_BuildStairs(line, args, -1, STAIRS_NORMAL);
      end;

    27: // Build Stairs Up Normal
      begin
        result := EVH_BuildStairs(line, args, 1, STAIRS_NORMAL);
      end;

    28: // Floor Raise and Crush
      begin
        result := EVH_DoFloor(line, args, FLEV_RAISEFLOORCRUSH);
      end;

    29: // Build Pillar (no crushing)
      begin
        result := EVH_BuildPillar(line, args, false);
      end;

    30: // Open Pillar
      begin
        result := EVH_OpenPillar(line, args);
      end;

    31: // Stairs Build Down Sync
      begin
        result := EVH_BuildStairs(line, args, -1, STAIRS_SYNC);
      end;

    32: // Build Stairs Up Sync
      begin
        result := EVH_BuildStairs(line, args, 1, STAIRS_SYNC);
      end;

    35: // Raise Floor by Value Times 8
      begin
        result := EVH_DoFloor(line, args, FLEV_RAISEBYVALUETIMES8);
      end;

    36: // Lower Floor by Value Times 8
      begin
        result := EVH_DoFloor(line, args, FLEV_LOWERBYVALUETIMES8);
      end;

    40: // Ceiling Lower by Value
      begin
        result := EVH_DoCeiling(line, args, CLEV_LOWERBYVALUE);
      end;

    41: // Ceiling Raise by Value
      begin
        result := EVH_DoCeiling(line, args, CLEV_RAISEBYVALUE);
      end;

    42: // Ceiling Crush and Raise
      begin
        result := EVH_DoCeiling(line, args, CLEV_CRUSHANDRAISE);
      end;

    43: // Ceiling Lower and Crush
      begin
        result := EVH_DoCeiling(line, args, CLEV_LOWERANDCRUSH);
      end;

    44: // Ceiling Crush Stop
      begin
        result := EVH_CeilingCrushStop(line, args);
      end;

    45: // Ceiling Crush Raise and Stay
      begin
        result := EVH_DoCeiling(line, args, CLEV_CRUSHRAISEANDSTAY);
      end;

    46: // Floor Crush Stop
      begin
        result := EVH_FloorCrushStop(line, args);
      end;

    60: // Plat Perpetual Raise
      begin
        result := EVH_DoPlat(line, args, PLAT_PERPETUALRAISE, 0);
      end;

    61: // Plat Stop
      begin
        EVH_StopPlat(line, args);
      end;

    62: // Plat Down-Wait-Up-Stay
      begin
        result := EVH_DoPlat(line, args, PLAT_DOWNWAITUPSTAY, 0);
      end;

    63: // Plat Down-by-Value*8-Wait-Up-Stay
      begin
        result := EVH_DoPlat(line, args, PLAT_DOWNBYVALUEWAITUPSTAY, 0);
      end;

    64: // Plat Up-Wait-Down-Stay
      begin
        result := EVH_DoPlat(line, args, PLAT_UPWAITDOWNSTAY, 0);
      end;

    65: // Plat Up-by-Value*8-Wait-Down-Stay
      begin
        result := EVH_DoPlat(line, args, PLAT_UPBYVALUEWAITDOWNSTAY, 0);
      end;

    66: // Floor Lower Instant * 8
      begin
        result := EVH_DoFloor(line, args, FLEV_LOWERTIMES8INSTANT);
      end;

    67: // Floor Raise Instant * 8
      begin
        result := EVH_DoFloor(line, args, FLEV_RAISETIMES8INSTANT);
      end;

    68: // Floor Move to Value * 8
      begin
        result := EVH_DoFloor(line, args, FLEV_MOVETOVALUETIMES8);
      end;

    69: // Ceiling Move to Value * 8
      begin
        result := EVH_DoCeiling(line, args, CLEV_MOVETOVALUETIMES8);
      end;

    70: // Teleport
      begin
        if side = 0 then
        begin // Only teleport when crossing the front side of a line
          result := EVH_Teleport(args[0], mo, true);
        end;
      end;

    71: // Teleport, no fog
      begin
        if side = 0 then
        begin // Only teleport when crossing the front side of a line
          result := EVH_Teleport(args[0], mo, false);
        end;
      end;

    72: // Thrust Mobj
      begin
        if side = 0 then // Only thrust on side 0
        begin
          P_ThrustMobj(mo, args[0] * (ANG90 div 64), args[1] * FRACUNIT);
          result := true;
        end;
      end;

    73: // Damage Mobj
      begin
        if args[0] <> 0 then
          P_DamageMobj(mo, nil, nil, args[0])
        else  // If arg1 is zero, then guarantee a kill
          P_DamageMobj(mo, nil, nil, 10000);
        result := true;
      end;

    74: // Teleport_NewMap
      begin
        if side = 0 then
        begin // Only teleport when crossing the front side of a line
          if not ((mo <> nil) and (mo.player <> nil) and (Pplayer_t(mo.player).playerstate = PST_DEAD)) then // Players must be alive to teleport
          begin
            if P_GetMapCluster(args[0]) = 0 then
            begin
              P_SetMessage(mo.player, GACCESSDENIEDDEMO);
              result := false;
            end
            else
            begin
              G_Completed(args[0], args[1]);
              result := true;
            end;
          end;
        end;
      end;

    75: // Teleport_EndGame
      begin
        if side = 0 then
        begin // Only teleport when crossing the front side of a line
          if not ((mo <> nil) and (mo.player <> nil) and (Pplayer_t(mo.player).playerstate = PST_DEAD)) then // Players must be alive to teleport
          begin
            result := true;
            if deathmatch <> 0 then
            begin // Winning in deathmatch just goes back to map 1
              G_Completed(1, 0);
            end
            else
            begin // Passing -1, -1 to G_Completed starts the Finale
              G_Completed(-1, -1);
            end;
          end;
        end;
      end;

    80: // ACS_Execute
      begin
        result := P_StartACS(args[0], args[1], @args[2], mo, line, side);
      end;

    81: // ACS_Suspend
      begin
        result := P_SuspendACS(args[0], args[1]);
      end;

    82: // ACS_Terminate
      begin
        result := P_TerminateACS(args[0], args[1]);
      end;

    83: // ACS_LockedExecute
      begin
        result := P_StartLockedACS(line, args, mo, side);
      end;

    90: // Poly Rotate Left Override
      begin
        result := EVH_RotatePoly(line, args, 1, true);
      end;

    91: // Poly Rotate Right Override
      begin
        result := EVH_RotatePoly(line, args, -1, true);
      end;

    92: // Poly Move Override
      begin
        result := EVH_MovePoly(line, args, false, true);
      end;

    93: // Poly Move Times 8 Override
      begin
        result := EVH_MovePoly(line, args, true, true);
      end;

    94: // Build Pillar Crush
      begin
        result := EVH_BuildPillar(line, args, true);
      end;

    95: // Lower Floor and Ceiling
      begin
        result := EVH_DoFloorAndCeiling(line, args, false);
      end;

    96: // Raise Floor and Ceiling
      begin
        result := EVH_DoFloorAndCeiling(line, args, true);
      end;

    109: // Force Lightning
      begin
        result := true;
        P_ForceLightning;
      end;

    110: // Light Raise by Value
      begin
        result := EVH_SpawnLight(line, args, LITE_RAISEBYVALUE);
      end;

    111: // Light Lower by Value
      begin
        result := EVH_SpawnLight(line, args, LITE_LOWERBYVALUE);
      end;

    112: // Light Change to Value
      begin
        result := EVH_SpawnLight(line, args, LITE_CHANGETOVALUE);
      end;

    113: // Light Fade
      begin
        result := EVH_SpawnLight(line, args, LITE_FADE);
      end;

    114: // Light Glow
      begin
        result := EVH_SpawnLight(line, args, LITE_GLOW);
      end;

    115: // Light Flicker
      begin
        result := EVH_SpawnLight(line, args, LITE_FLICKER);
      end;

    116: // Light Strobe
      begin
        result := EVH_SpawnLight(line, args, LITE_STROBE);
      end;

    120: // Quake Tremor
      begin
        result := A_LocalQuake(args, mo);
      end;

    129: // UsePuzzleItem
      begin
        result := EVH_LineSearchForPuzzleItem(line, args, mo);
      end;

    130: // Thing_Activate
      begin
        result := EVH_ThingActivate(args[0]);
      end;

    131: // Thing_Deactivate
      begin
        result := EVH_ThingDeactivate(args[0]);
      end;

    132: // Thing_Remove
      begin
        result := EVH_ThingRemove(args[0]);
      end;

    133: // Thing_Destroy
      begin
        result := EVH_ThingDestroy(args[0]);
      end;

    134: // Thing_Projectile
      begin
        result := EVH_ThingProjectile(args, false);
      end;

    135: // Thing_Spawn
      begin
        result := EVH_ThingSpawn(args, true);
      end;

    136: // Thing_ProjectileGravity
      begin
        result := EVH_ThingProjectile(args, true);
      end;

    137: // Thing_SpawnNoFog
      begin
        result := EVH_ThingSpawn(args, false);
      end;

    138: // Floor_Waggle
      begin
        result := EVH_StartFloorWaggle(args[0], args[1], args[2], args[3], args[4]);
      end;

    140: // Sector_SoundChange
      begin
        result := EVH_SectorSoundChange(args);
      end;

    // Line specials only processed during level initialization
    // 100: Scroll_Texture_Left
    // 101: Scroll_Texture_Right
    // 102: Scroll_Texture_Up
    // 103: Scroll_Texture_Down
    // 121: Line_SetIdentification

    // Inert Line specials
  end;
end;

end.
