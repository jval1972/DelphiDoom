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
//  Foundation, inc., 59 Temple Place - Suite 330, Boston, MA
//  02111-1307, USA.
//
// DESCRIPTION:
//  Ceiling aninmation (lowering, crushing, raising)
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit p_ceilng;

interface

uses
  z_zone,
  p_spec,
  r_defs,
  s_sound,
  sounddata;

var
  activeceilings: array[0..MAXCEILINGS - 1] of Pceiling_t;

//==============================================================================
//
// T_MoveCeiling
//
//==============================================================================
procedure T_MoveCeiling(ceiling: Pceiling_t);

//==============================================================================
//
// EV_DoCeiling
//
//==============================================================================
function EV_DoCeiling(line: Pline_t; _type: ceiling_e): integer;

//==============================================================================
//
// P_AddActiveCeiling
//
//==============================================================================
procedure P_AddActiveCeiling(c: Pceiling_t);

//==============================================================================
//
// EV_CeilingCrushStop
//
//==============================================================================
function EV_CeilingCrushStop(line: Pline_t): integer;

implementation

uses
  i_system,
  m_fixed,
  p_tick,
  p_setup,
  p_slopes,
  p_floor;

//==============================================================================
// P_AddActiveCeiling
//
// Add an active ceiling
//
//==============================================================================
procedure P_AddActiveCeiling(c: Pceiling_t);
var
  i: integer;
begin
  for i := 0 to MAXCEILINGS - 1 do
    if activeceilings[i] = nil then
    begin
      activeceilings[i] := c;
      exit;
    end;
  I_Warning('P_AddActiveCeiling(): Can not add ceiling, limit %d reached'#13#10, [MAXCEILINGS]);
end;

//==============================================================================
// P_RemoveActiveCeiling
//
// Remove a ceiling's thinker
//
//==============================================================================
procedure P_RemoveActiveCeiling(c: Pceiling_t);
var
  i: integer;
begin
  for i := 0 to MAXCEILINGS - 1 do
    if activeceilings[i] = c then
    begin
      activeceilings[i].sector.specialdata := nil;
      P_RemoveThinker(@activeceilings[i].thinker);
      activeceilings[i] := nil;
      exit;
    end;
end;

//==============================================================================
// P_ActivateInStasisCeiling
//
// Restart a ceiling that's in-stasis
//
//==============================================================================
procedure P_ActivateInStasisCeiling(line: Pline_t);
var
  i: integer;
begin
  for i := 0 to MAXCEILINGS - 1 do
    if (activeceilings[i] <> nil) and
       (activeceilings[i].tag = line.tag) and
       (activeceilings[i].direction = 0) then
    begin
      activeceilings[i].direction := activeceilings[i].olddirection;
      activeceilings[i].thinker._function.acp1 := @T_MoveCeiling;
    end;
end;

//==============================================================================
//
// T_MoveCeiling
//
//==============================================================================
procedure T_MoveCeiling(ceiling: Pceiling_t);
var
  res: result_e;
begin
  case ceiling.direction of
    0:
    // IN STASIS
      begin
      end;
    1:
    // UP
      begin
        res := T_MovePlane(ceiling.sector,
          ceiling.speed,
          ceiling.topheight,
          false, 1, ceiling.direction);

        if leveltime and 7 = 0 then
          S_StartSound(@ceiling.sector.soundorg, Ord(sfx_dormov));

        if res = pastdest then
        begin
          case ceiling._type of
            raiseToHighest:
              P_RemoveActiveCeiling(ceiling);
            fastCrushAndRaise,
            crushAndRaise:
              ceiling.direction := -1;
          end;
        end;
      end;
   -1:
    // DOWN
      begin
        res := T_MovePlane(ceiling.sector,
          ceiling.speed,
          ceiling.bottomheight,
          ceiling.crush, 1, ceiling.direction);

        if leveltime and 7 = 0 then
          S_StartSound(@ceiling.sector.soundorg, Ord(sfx_dormov));

        if res = pastdest then
        begin
          case ceiling._type of
            crushAndRaise:
              begin
                ceiling.speed := CEILSPEED;
                ceiling.direction := 1;
              end;
            fastCrushAndRaise:
              begin
                ceiling.direction := 1;
              end;
            lowerAndCrush,
            lowerToFloor:
              P_RemoveActiveCeiling(ceiling);
          end;
        end
        else // ( res <> pastdest )
        begin
          if res = crushed then
          begin
            case ceiling._type of
              crushAndRaise,
              lowerAndCrush:
                ceiling.speed := CEILSPEED div 8;
            end;
          end;
        end;
      end;
  end;
end;

//==============================================================================
//
// EV_DoCeiling
// Move a ceiling up/down and all around!
//
//==============================================================================
function EV_DoCeiling(line: Pline_t; _type: ceiling_e): integer;
var
  initial: boolean;
  secnum: integer;
  sec: Psector_t;
  ceiling: Pceiling_t;
begin
  secnum := -1;
  result := 0;

  // Reactivate in-stasis ceilings...for certain types.
  case _type of
    fastCrushAndRaise,
    crushAndRaise:
      P_ActivateInStasisCeiling(line);
  end;

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
    ceiling := Z_Malloc(SizeOf(ceiling_t), PU_LEVSPEC, nil);
    P_AddThinker(@ceiling.thinker);
    sec.specialdata := ceiling;
    ceiling.thinker._function.acp1 := @T_MoveCeiling;
    ceiling.sector := sec;
    ceiling.crush := false;

    case _type of
      fastCrushAndRaise:
        begin
          ceiling.crush := true;
          ceiling.topheight := sec.ceilingheight;
          ceiling.bottomheight := sec.floorheight + (8 * FRACUNIT);
          ceiling.direction := -1;
          ceiling.speed := CEILSPEED * 2;
        end;

      crushAndRaise:
        begin
          ceiling.crush := true;
          ceiling.topheight := sec.ceilingheight;
          ceiling.bottomheight := sec.floorheight;
          if _type <> lowerToFloor then
            ceiling.bottomheight := ceiling.bottomheight + 8 * FRACUNIT;
          ceiling.direction := -1;
          ceiling.speed := CEILSPEED;
        end;
      lowerAndCrush,
      lowerToFloor:
        begin
          ceiling.bottomheight := sec.floorheight;
          if _type <> lowerToFloor then
            ceiling.bottomheight := ceiling.bottomheight + 8 * FRACUNIT;
          ceiling.direction := -1;
          ceiling.speed := CEILSPEED;
        end;

      raiseToHighest:
        begin
          ceiling.topheight := P_FindHighestCeilingSurrounding(sec);
          ceiling.direction := 1;
          ceiling.speed := CEILSPEED;
        end;
    end;

    ceiling.tag := sec.tag;
    ceiling._type := _type;
    P_AddActiveCeiling(ceiling);
    P_DynamicSlope(sec);  // JVAL: Slopes
  end;
end;

//==============================================================================
//
// EV_CeilingCrushStop
// Stop a ceiling from crushing!
//
//==============================================================================
function EV_CeilingCrushStop(line: Pline_t): integer;
var
  i: integer;
begin
  result := 0;
  for i := 0 to MAXCEILINGS - 1 do
    if (activeceilings[i] <> nil) and
       (activeceilings[i].tag = line.tag) and
       (activeceilings[i].direction <> 0) then
    begin
      activeceilings[i].olddirection := activeceilings[i].direction;
      activeceilings[i].thinker._function.acv := nil;
      activeceilings[i].direction := 0; // in-stasis
      result := 1;
    end;
end;

end.
