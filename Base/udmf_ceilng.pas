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
//  Ceiling aninmation (lowering, crushing, raising) (UDMF & Hexen)
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit udmf_ceilng;

interface

uses
  d_delphi,
  z_zone,
  p_spec,
  r_defs;

var
  hactiveceilings: array[0..MAXCEILINGS - 1] of Pceiling_t;

//==============================================================================
//
// TH_MoveCeiling
//
//==============================================================================
procedure TH_MoveCeiling(ceiling: Pceiling_t);

//==============================================================================
//
// EVH_DoCeiling
//
//==============================================================================
function EVH_DoCeiling(line: Pline_t; args: PByteArray; _type: ceiling_e): boolean;

//==============================================================================
//
// PH_AddActiveCeiling
//
//==============================================================================
procedure PH_AddActiveCeiling(c: Pceiling_t);

//==============================================================================
//
// EVH_CeilingCrushStop
//
//==============================================================================
function EVH_CeilingCrushStop(line: Pline_t; args: PByteArray): boolean;

implementation

uses
  i_system,
  m_fixed,
  p_acs,
  p_mobj_h,
  p_tick,
  p_setup,
  p_slopes,
  {$IFDEF HEXEN}
  s_sndseq,
  {$ELSE}
  s_sound,
  udmf_spec,
  {$ENDIF}
  udmf_floor;

//==============================================================================
// PH_AddActiveCeiling
//
// Add an active ceiling
//
//==============================================================================
procedure PH_AddActiveCeiling(c: Pceiling_t);
var
  i: integer;
begin
  for i := 0 to MAXCEILINGS - 1 do
    if hactiveceilings[i] = nil then
    begin
      hactiveceilings[i] := c;
      exit;
    end;
  I_Warning('PH_AddActiveCeiling(): Can not add ceiling, limit %d reached'#13#10, [MAXCEILINGS]);
end;

//==============================================================================
// P_RemoveActiveCeiling
//
// Remove a ceiling's thinker
//
//==============================================================================
procedure PH_RemoveActiveCeiling(c: Pceiling_t);
var
  i: integer;
begin
  for i := 0 to MAXCEILINGS - 1 do
    if hactiveceilings[i] = c then
    begin
      hactiveceilings[i].sector.specialdata := nil;
      P_RemoveThinker(@hactiveceilings[i].thinker);
      P_TagFinished(hactiveceilings[i].sector.tag);
      hactiveceilings[i] := nil;
      exit;
    end;
end;

//==============================================================================
//
// TH_MoveCeiling
//
//==============================================================================
procedure TH_MoveCeiling(ceiling: Pceiling_t);
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
        res := TH_MovePlane(ceiling.sector,
          ceiling.speed,
          ceiling.topheight,
          false, 1, ceiling.direction);

        if res = RES_PASTDEST then
        begin
          {$IFDEF HEXEN}
          S_StopSequence(Pmobj_t(@ceiling.sector.soundorg));
          {$ELSE}
          S_StopSound(@ceiling.sector.soundorg);
          {$ENDIF}
          if ceiling._type = CLEV_CRUSHANDRAISE then
          begin
            ceiling.direction := -1;
            ceiling.speed := ceiling.speed * 2;
          end
          else
            PH_RemoveActiveCeiling(ceiling);
        end;
      end;
   -1:
    // DOWN
      begin
        res := TH_MovePlane(ceiling.sector,
          ceiling.speed,
          ceiling.bottomheight,
          ceiling.crush, 1, ceiling.direction);

        if res = RES_PASTDEST then
        begin
          {$IFDEF HEXEN}
          S_StopSequence(Pmobj_t(@ceiling.sector.soundorg));
          {$ELSE}
          S_StopSound(@ceiling.sector.soundorg);
          {$ENDIF}
          case ceiling._type of
            CLEV_CRUSHANDRAISE,
            CLEV_CRUSHRAISEANDSTAY:
              begin
                ceiling.direction := 1;
                ceiling.speed := ceiling.speed div 2;
              end;
          else
            PH_RemoveActiveCeiling(ceiling);
          end;
        end;
      end;
  end;
end;

//==============================================================================
//
// EVH_DoCeiling
// Move a ceiling up/down and all around!
//
//==============================================================================
function EVH_DoCeiling(line: Pline_t; args: PByteArray; _type: ceiling_e): boolean;
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
    ceiling.thinker._function.acp1 := @TH_MoveCeiling;
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
      CLEV_LOWERBYVALUETIMES8:
        begin
          ceiling.bottomheight := sec.ceilingheight - 8 * args[2] * FRACUNIT;
          ceiling.direction := -1;
        end;
      CLEV_LOWERTOMAXFLOOR:
        begin
          ceiling.bottomheight := P_FindHighestFloorSurrounding(sec);
          ceiling.direction := -1;
        end;
      CLEV_LOWERTOLOWEST:
        begin
          ceiling.bottomheight := P_FindLowestCeilingSurrounding(sec);
          ceiling.direction := -1;
        end;
      CLEV_RAISEBYVALUE:
        begin
          ceiling.topheight := sec.ceilingheight + args[2] * FRACUNIT;
          ceiling.direction := 1;
        end;
      CLEV_RAISEBYVALUETIMES8:
        begin
          ceiling.topheight := sec.ceilingheight + 8 * args[2] * FRACUNIT;
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
      CLEV_MOVETOVALUEANDCRUSH:
        begin
          destHeight := PSmallInt(@args[2])^ * FRACUNIT;
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
          if args[4] <> 0 then
            ceiling.crush := true // arg[4] := crushing value
          else
            ceiling.crush := false;
        end;
      CLEV_MOVETOVALUE:
        begin
          destHeight := PSmallInt(@args[2])^ * FRACUNIT;
          if args[4] <> 0 then
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
      CLEV_RAISETOHIGHESTFLOOR:
        begin
          ceiling.topheight := P_FindHighestFloorSurrounding(sec);
          ceiling.direction := 1;
        end;
      else
        result := false;
    end;

    ceiling.tag := sec.tag;
    ceiling._type := _type;
    PH_AddActiveCeiling(ceiling);
    if result then
      {$IFDEF HEXEN}
      S_StartSequence(Pmobj_t(@ceiling.sector.soundorg), Ord(SEQ_PLATFORM) + Ord(ceiling.sector.seqType));
      {$ELSE}
      S_StartSound(@ceiling.sector.soundorg, ceiling.sector.seqType);
      {$ENDIF}
    P_DynamicSlope(sec);  // JVAL: Slopes
  end;
end;

//==============================================================================
//
// EVH_CeilingCrushStop
// Stop a ceiling from crushing!
//
//==============================================================================
function EVH_CeilingCrushStop(line: Pline_t; args: PByteArray): boolean;
var
  i: integer;
begin
  result := false;
  for i := 0 to MAXCEILINGS - 1 do
    if (hactiveceilings[i] <> nil) and
       (hactiveceilings[i].tag = args[0]) then
    begin
      {$IFDEF HEXEN}
      S_StopSequence(Pmobj_t(@hactiveceilings[i].sector.soundorg));
      {$ELSE}
      S_StopSound(@hactiveceilings[i].sector.soundorg);
      {$ENDIF}
      hactiveceilings[i].sector.specialdata := nil;
      P_RemoveThinker(@hactiveceilings[i].thinker);
      P_TagFinished(hactiveceilings[i].sector.tag);
      hactiveceilings[i] := nil;
      result := true;
      exit;
    end;
end;

end.
