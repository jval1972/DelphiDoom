//------------------------------------------------------------------------------
//
//  DelphiHexen is a source port of the game Hexen and it is
//  based on original Linux Doom as published by "id Software", on
//  Hexen source as published by "Raven" software and DelphiDoom
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
  d_delphi,
  z_zone,
  doomdef,
  p_local,
  p_spec,
  r_defs,
  s_sound,
// State.
  doomstat,
// Data.
  sounds;

//-----------------------------------------------------------------------------
//
// DESCRIPTION:
//  Ceiling aninmation (lowering, crushing, raising)
//
//-----------------------------------------------------------------------------

var
  activeceilings: array[0..MAXCEILINGS - 1] of Pceiling_t;

procedure T_MoveCeiling(ceiling: Pceiling_t);

function EV_DoCeiling(line: Pline_t; args: PByteArray; _type: ceiling_e): boolean;

procedure P_AddActiveCeiling(c: Pceiling_t);

function EV_CeilingCrushStop(line: Pline_t; args: PByteArray): boolean;

implementation

uses
  i_system,
  m_fixed,
  p_acs,
  p_mobj_h,
  p_tick,
  p_setup,
  p_slopes,
  p_floor,
  s_sndseq;

//
// Add an active ceiling
//
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

//
// Remove a ceiling's thinker
//
procedure P_RemoveActiveCeiling(c: Pceiling_t);
var
  i: integer;
begin
  for i := 0 to MAXCEILINGS - 1 do
    if activeceilings[i] = c then
    begin
      activeceilings[i].sector.specialdata := nil;
      P_RemoveThinker(@activeceilings[i].thinker);
      P_TagFinished(activeceilings[i].sector.tag);
      activeceilings[i] := nil;
      exit;
    end;
end;

//
// T_MoveCeiling
//

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

        if res = RES_PASTDEST then
        begin
          S_StopSequence(Pmobj_t(@ceiling.sector.soundorg));
          if ceiling._type = CLEV_CRUSHANDRAISE then
          begin
            ceiling.direction := -1;
            ceiling.speed := ceiling.speed * 2;
          end
          else
            P_RemoveActiveCeiling(ceiling);
        end;
      end;
   -1:
    // DOWN
      begin
        res := T_MovePlane(ceiling.sector,
          ceiling.speed,
          ceiling.bottomheight,
          ceiling.crush, 1, ceiling.direction);

        if res = RES_PASTDEST then
        begin
          S_StopSequence(Pmobj_t(@ceiling.sector.soundorg));
          case ceiling._type of
            CLEV_CRUSHANDRAISE,
            CLEV_CRUSHRAISEANDSTAY:
              begin
                ceiling.direction := 1;
                ceiling.speed := ceiling.speed div 2;
              end;
          else
            P_RemoveActiveCeiling(ceiling);
          end;
        end;
      end;
  end;
end;

//
// EV_DoCeiling
// Move a ceiling up/down and all around!
//
function EV_DoCeiling(line: Pline_t; args: PByteArray; _type: ceiling_e): boolean;
var
  initial: boolean;
  secnum: integer;
  sec: Psector_t;
  ceiling: Pceiling_t;
  destHeight: integer;
begin
  secnum := -1;
  result := false;

  initial := true;
  while (secnum >= 0) or initial do
  begin
    initial := false;
    secnum := P_FindSectorFromTag(args[0], secnum);
    if secnum < 0 then
      break;

    sec := @sectors[secnum];
    if sec.specialdata <> nil then
      continue;

    // new door thinker
    result := true;
    ceiling := Z_Malloc(SizeOf(ceiling_t), PU_LEVSPEC, nil);
    P_AddThinker(@ceiling.thinker);
    sec.specialdata := ceiling;
    ceiling.thinker._function.acp1 := @T_MoveCeiling;
    ceiling.sector := sec;
    ceiling.crush := false;
    ceiling.speed := args[1] * (FRACUNIT div 8);

    case _type of
      CLEV_CRUSHRAISEANDSTAY:
        begin
          ceiling.crush := args[2] <> 0; // arg[2] := crushing value
          ceiling.topheight := sec.ceilingheight;
          ceiling.bottomheight := sec.floorheight + (8 * FRACUNIT);
          ceiling.direction := -1;
        end;
      CLEV_CRUSHANDRAISE,
      CLEV_LOWERANDCRUSH,
      CLEV_LOWERTOFLOOR:
        begin
          if _type = CLEV_CRUSHANDRAISE then
            ceiling.topheight := sec.ceilingheight
          else if _type = CLEV_LOWERANDCRUSH then
          begin
            ceiling.topheight := sec.ceilingheight;
            if args[2] <> 0 then
              ceiling.crush := true // arg[2] := crushing value
            else
              ceiling.crush := false;
          end;
          ceiling.bottomheight := sec.floorheight;
          if _type <> CLEV_LOWERTOFLOOR then
            ceiling.bottomheight := ceiling.bottomheight + 8 * FRACUNIT;
          ceiling.direction := -1;
        end;
      CLEV_RAISETOHIGHEST:
        begin
          ceiling.topheight := P_FindHighestCeilingSurrounding(sec);
          ceiling.direction := 1;
        end;
      CLEV_LOWERBYVALUE:
        begin
          ceiling.bottomheight := sec.ceilingheight - args[2] * FRACUNIT;
          ceiling.direction := -1;
        end;
      CLEV_RAISEBYVALUE:
        begin
          ceiling.topheight := sec.ceilingheight + args[2] * FRACUNIT;
          ceiling.direction := 1;
        end;
      CLEV_MOVETOVALUETIMES8:
        begin
          destHeight := args[2] * FRACUNIT * 8;
          if args[3] <> 0 then
            destHeight := -destHeight;
          if sec.ceilingheight <= destHeight then
          begin
            ceiling.direction := 1;
            ceiling.topheight := destHeight;
            if sec.ceilingheight = destHeight then
              result := false;
          end
          else if sec.ceilingheight > destHeight then
          begin
            ceiling.direction := -1;
            ceiling.bottomheight := destHeight;
          end;
        end;
      else
        result := false;
    end;

    ceiling.tag := sec.tag;
    ceiling._type := _type;
    P_AddActiveCeiling(ceiling);
    if result then
      S_StartSequence(Pmobj_t(@ceiling.sector.soundorg),
        Ord(SEQ_PLATFORM) + Ord(ceiling.sector.seqType));
    P_DynamicSlope(sec);  // JVAL: Slopes
  end;
end;

//
// EV_CeilingCrushStop
// Stop a ceiling from crushing!
//
function EV_CeilingCrushStop(line: Pline_t; args: PByteArray): boolean;
var
  i: integer;
begin
  result := false;
  for i := 0 to MAXCEILINGS - 1 do
    if (activeceilings[i] <> nil) and
       (activeceilings[i].tag = args[0])  then
    begin
      S_StopSequence(Pmobj_t(@activeceilings[i].sector.soundorg));
      activeceilings[i].sector.specialdata := nil;
      P_RemoveThinker(@activeceilings[i].thinker);
      P_TagFinished(activeceilings[i].sector.tag);
      activeceilings[i] := nil;
      result := true;
      exit;
    end;
end;

end.
