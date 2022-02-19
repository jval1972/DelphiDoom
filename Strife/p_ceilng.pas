//------------------------------------------------------------------------------
//
//  DelphiStrife is a source port of the game Strife.
//
//  Based on:
//    - Linux Doom by "id Software"
//    - Chocolate Strife by "Simon Howard"
//    - DelphiDoom by "Jim Valavanis"
//
//  Copyright (C) 1993-1996 by id Software, Inc.
//  Copyright (C) 2005 Simon Howard
//  Copyright (C) 2010 James Haley, Samuel Villarreal
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

//==============================================================================
//
// P_ActivateInStasisCeiling
//
//==============================================================================
function P_ActivateInStasisCeiling(line: Pline_t): integer;

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
      activeceilings[i].sector.ceilingdata := nil;
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
function P_ActivateInStasisCeiling(line: Pline_t): integer;
var
  i: integer;
begin
  result := 0;
  for i := 0 to MAXCEILINGS - 1 do
    if (activeceilings[i] <> nil) and
       (activeceilings[i].tag = line.tag) and
       (activeceilings[i].direction = 0) then
    begin
      activeceilings[i].direction := activeceilings[i].olddirection;
      activeceilings[i].thinker._function.acp1 := @T_MoveCeiling;
      result := 1;
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
        begin
          if (ceiling._type <> silentCrushAndRaise) and
             (ceiling._type <> genSilentCrusher) then
            S_StartSound(@ceiling.sector.soundorg, Ord(sfx_stnmov));
        end;

        if res = pastdest then
        begin
          case ceiling._type of
            raiseToHighest,
            genCeiling:
              P_RemoveActiveCeiling(ceiling);
            genCeilingChgT,
            genCeilingChg0,
            genCeilingChg:
              begin
                if ceiling._type <> genCeilingChg then
                begin
                  ceiling.sector.special := ceiling.newspecial;
                  //jff 3/14/98 transfer old special field as well
                  ceiling.sector.oldspecial := ceiling.oldspecial;
                end;
                ceiling.sector.ceilingpic := ceiling.texture;
                P_RemoveActiveCeiling(ceiling);
              end;

            silentCrushAndRaise:
              begin
                S_StartSound(@ceiling.sector.soundorg, Ord(sfx_pstop));
                ceiling.direction := -1;
              end;
            fastCrushAndRaise,
            crushAndRaise,
            genSilentCrusher,
            genCrusher:
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
        begin
          if (ceiling._type <> silentCrushAndRaise) and
             (ceiling._type <> genSilentCrusher) then
            S_StartSound(@ceiling.sector.soundorg, Ord(sfx_stnmov));
        end;

        if res = pastdest then
        begin
          case ceiling._type of
          // 02/09/98 jff change slow crushers' speed back to normal
          // start back up
            genSilentCrusher,
            genCrusher:
              begin
                if ceiling.oldspeed < CEILSPEED * 3 then
                  ceiling.speed := ceiling.oldspeed;
                ceiling.direction := 1; //jff 2/22/98 make it go back up!
              end;
            silentCrushAndRaise:
              begin
                S_StartSound(@ceiling.sector.soundorg, Ord(sfx_pstop));
                ceiling.speed := CEILSPEED;
                ceiling.direction := 1;
              end;
            crushAndRaise:
              begin
                ceiling.speed := CEILSPEED;
                ceiling.direction := 1;
              end;
            fastCrushAndRaise:
              begin
                ceiling.direction := 1;
              end;
          // in the of ceiling mover/changer, change the texture
          // then remove the active ceiling
            genCeilingChgT,
            genCeilingChg0,
            genCeilingChg:
              begin
                if ceiling._type <> genCeilingChg then
                begin
                  ceiling.sector.special := ceiling.newspecial;
                  //jff add to fix bug in special transfers from changes
                  ceiling.sector.oldspecial := ceiling.oldspecial;
                end;
                ceiling.sector.ceilingpic := ceiling.texture;
                P_RemoveActiveCeiling(ceiling);
              end;
            lowerAndCrush,
            lowerToFloor,
            lowerToLowest,
            lowerToMaxFloor,
            genCeiling:
              P_RemoveActiveCeiling(ceiling);
          end;
        end
        else // ( res <> pastdest )
        begin
          if res = crushed then
          begin
            case ceiling._type of
            //jff 02/08/98 slow down slow crushers on obstacle
              genCrusher,
              genSilentCrusher:
                if ceiling.oldspeed < CEILSPEED * 3 then
                  ceiling.speed := CEILSPEED div 8;
              silentCrushAndRaise,
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
// haleyjd 10/04/10: [STRIFE] Changes:
// * Fast crushers were made 2x as fast.
// * lowerAndCrush was apparently "fixed" to actually crush, and was also
//   altered to lower all the way to the floor rather than remain 8 above.
// * silentCrushAndRaise and crushAndRaise no longer crush.
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

  // Reactivate in-stasis ceilings...for certain types.
  case _type of
    fastCrushAndRaise,
    silentCrushAndRaise,
    crushAndRaise:
      result := P_ActivateInStasisCeiling(line);
  else
    result := 0;
  end;

  initial := true;
  while (secnum >= 0) or initial do
  begin
    initial := false;
    secnum := P_FindSectorFromLineTag(line, secnum);
    if secnum < 0 then
      break;

    sec := @sectors[secnum];
    if P_SectorActive(ceiling_special, sec) then
      continue;

    // new door thinker
    result := 1;
    ceiling := Z_Malloc(SizeOf(ceiling_t), PU_LEVSPEC, nil);
    P_AddThinker(@ceiling.thinker);
    sec.ceilingdata := ceiling;
    ceiling.thinker._function.acp1 := @T_MoveCeiling;
    ceiling.sector := sec;
    ceiling.crush := false;

    case _type of
      fastCrushAndRaise:
        begin
          // [STRIFE]: Speed of fast crushers increased by 2x!
          ceiling.crush := true;
          ceiling.topheight := sec.ceilingheight;
          ceiling.bottomheight := sec.floorheight + (8 * FRACUNIT);
          ceiling.direction := -1;
          ceiling.speed := CEILSPEED * 4; // [STRIFE]
        end;

      lowerAndCrush:
        begin
          // [STRIFE] lowerAndCrush doesn't seem to have crushed in DOOM,
          // but it was certainly made to do so in Strife! It is also
          // changed to lower all the way to the floor.
          ceiling.crush := true;
          ceiling.direction := -1;
          ceiling.speed := CEILSPEED;
          ceiling.bottomheight := sec.floorheight;
        end;

      silentCrushAndRaise,
      crushAndRaise:
        begin
          ceiling.topheight := sec.ceilingheight;
          ceiling.bottomheight := sec.floorheight;
          if _type <> lowerToFloor then
            ceiling.bottomheight := ceiling.bottomheight + 8 * FRACUNIT;
          ceiling.direction := -1;
          ceiling.speed := CEILSPEED;
        end;
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

      lowerToLowest:
        begin
          ceiling.bottomheight := P_FindLowestCeilingSurrounding(sec);
          ceiling.direction := -1;
          ceiling.speed := CEILSPEED;
        end;

      lowerToMaxFloor:
        begin
          ceiling.bottomheight := P_FindHighestFloorSurrounding(sec);
          ceiling.direction := -1;
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
