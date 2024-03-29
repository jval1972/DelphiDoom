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
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit udmf_plats;

interface

uses
  d_delphi,
  p_spec,
  r_defs;

var
  hactiveplats: array[0..MAXPLATS - 1] of Pplat_t;

//==============================================================================
//
// TH_PlatRaise
//
//==============================================================================
procedure TH_PlatRaise(plat: Pplat_t);

//==============================================================================
//
// EVH_DoPlat
//
//==============================================================================
function EVH_DoPlat(line: Pline_t; args: PByteArray; _type: plattype_e; amount: integer): boolean;

//==============================================================================
//
// EVH_StopPlat
//
//==============================================================================
procedure EVH_StopPlat(line: Pline_t; args: PByteArray);

//==============================================================================
//
// PH_AddActivePlat
//
//==============================================================================
procedure PH_AddActivePlat(plat: Pplat_t);

//==============================================================================
//
// PH_RemoveActivePlat
//
//==============================================================================
procedure PH_RemoveActivePlat(plat: Pplat_t);

implementation

uses
  i_system,
  m_fixed,
  m_rnd,
  udmf_floor,
  p_mobj_h,
  p_setup,
  p_tick,
  p_acs,
  {$IFDEF HEXEN}
  s_sndseq,
  {$ELSE}
  s_sound,
  udmf_spec,
  {$ENDIF}
  z_zone;

//==============================================================================
// TH_PlatRaise
//
//      Move a plat up and down
//
//==============================================================================
procedure TH_PlatRaise(plat: Pplat_t);
var
  res: result_e;
begin
  case plat.status of
    PLAT_UP:
      begin
        res := TH_MovePlane(plat.sector, plat.speed, plat.high, plat.crush, 0, 1);
        if (res = RES_CRUSHED) and not plat.crush then
        begin
          plat.count := plat.wait;
          plat.status := PLAT_DOWN;
          {$IFDEF HEXEN}
          S_StartSequence(Pmobj_t(@plat.sector.soundorg), Ord(SEQ_PLATFORM) + Ord(plat.sector.seqType));
          {$ELSE}
          S_StartSound(@plat.sector.soundorg, plat.sector.seqType);
          {$ENDIF}
        end
        else if res = RES_PASTDEST then
        begin
          plat.count := plat.wait;
          plat.status := PLAT_WAITING;
          {$IFDEF HEXEN}
          S_StopSequence(Pmobj_t(@plat.sector.soundorg));
          {$ELSE}
          S_StopSound(@plat.sector.soundorg);
          {$ENDIF}
          if (plat._type = PLAT_DOWNWAITUPSTAY) or (plat._type = PLAT_DOWNBYVALUEWAITUPSTAY) or
            (plat._type = PLAT_DOWNWAITUPSTAYLIP) then
            PH_RemoveActivePlat(plat);
        end;
      end;
    PLAT_DOWN:
      begin
        res := TH_MovePlane(plat.sector, plat.speed, plat.low, false, 0, -1);
        if res = RES_PASTDEST then
        begin
          plat.count := plat.wait;
          plat.status := PLAT_WAITING;
          if (plat._type = PLAT_UPWAITDOWNSTAY) or (plat._type = PLAT_UPBYVALUEWAITDOWNSTAY) or
            (plat._type = PLAT_UPNEARESTWAITDOWNSTAY) then
            PH_RemoveActivePlat(plat);
          {$IFDEF HEXEN}
          S_StopSequence(Pmobj_t(@plat.sector.soundorg));
          {$ELSE}
          S_StopSound(@plat.sector.soundorg);
          {$ENDIF}
        end;
      end;
    PLAT_WAITING:
      begin
        dec(plat.count);
        if plat.count <= 0 then
        begin
          if plat.sector.floorheight = plat.low then
            plat.status := PLAT_UP
          else
            plat.status := PLAT_DOWN;
          {$IFDEF HEXEN}
          S_StartSequence(Pmobj_t(@plat.sector.soundorg), Ord(SEQ_PLATFORM) + Ord(plat.sector.seqType));
          {$ELSE}
          S_StartSound(@plat.sector.soundorg, plat.sector.seqType);
          {$ENDIF}
        end;
      end;
  end;
end;

//==============================================================================
// EVH_DoPlat
//
//      Do Platforms
//      "amount" is only used for SOME platforms.
//
//==============================================================================
function EVH_DoPlat(line: Pline_t; args: PByteArray; _type: plattype_e; amount: integer): boolean;
var
  plat: Pplat_t;
  secnum: integer;
  sec: Psector_t;
begin
  result := false;
  secnum := -1;

  while P_FindSectorFromTag2(args[0], secnum) >= 0 do
  begin
    sec := @sectors[secnum];
    if sec.specialdata <> nil then
      continue;

    //
    // Find lowest & highest floors around sector
    //
    result := true;
    plat := Z_Malloc(SizeOf(plat_t), PU_LEVSPEC, nil);
    P_AddThinker(@plat.thinker);

    plat._type := _type;
    plat.sector := sec;
    plat.sector.specialdata := plat;
    plat.thinker._function.acp1 := @TH_PlatRaise;
    plat.crush := false;
    plat.tag := args[0];
    plat.speed := args[1] * (FRACUNIT div 8);
    case _type of
      PLAT_DOWNWAITUPSTAY:
        begin
          plat.low := P_FindLowestFloorSurrounding(sec) + 8 * FRACUNIT;
          if plat.low > sec.floorheight then
            plat.low := sec.floorheight;
          plat.high := sec.floorheight;
          plat.wait := args[2];
          plat.status := PLAT_DOWN;
        end;
      PLAT_DOWNWAITUPSTAYLIP:
        begin
          plat.low := P_FindLowestFloorSurrounding(sec) + args[3] * FRACUNIT;
          if plat.low > sec.floorheight then
            plat.low := sec.floorheight;
          plat.high := sec.floorheight;
          plat.wait := args[2];
          plat.status := PLAT_DOWN;
        end;
      PLAT_DOWNBYVALUEWAITUPSTAY:
        begin
          plat.low := sec.floorheight - args[3] * 8 * FRACUNIT;
          if plat.low > sec.floorheight then
            plat.low := sec.floorheight;
          plat.high := sec.floorheight;
          plat.wait := args[2];
          plat.status := PLAT_DOWN;
        end;
      PLAT_UPWAITDOWNSTAY:
        begin
          plat.high := P_FindHighestFloorSurrounding(sec);
          if plat.high < sec.floorheight then
            plat.high := sec.floorheight;
          plat.low := sec.floorheight;
          plat.wait := args[2];
          plat.status := PLAT_UP;
        end;
      PLAT_UPNEARESTWAITDOWNSTAY:
        begin
          plat.high := P_FindNextHighestFloor(sec, sec.floorheight);
          if plat.high < sec.floorheight then
            plat.high := sec.floorheight;
          plat.low := sec.floorheight;
          plat.wait := args[2];
          plat.status := PLAT_UP;
        end;
      PLAT_UPBYVALUEWAITDOWNSTAY:
        begin
          plat.high := sec.floorheight + args[3] * 8 * FRACUNIT;
          if plat.high < sec.floorheight then
            plat.high := sec.floorheight;
          plat.low := sec.floorheight;
          plat.wait := args[2];
          plat.status := PLAT_UP;
        end;
      PLAT_PERPETUALRAISE:
        begin
          plat.low := P_FindLowestFloorSurrounding(sec) + 8 * FRACUNIT;
          if plat.low > sec.floorheight then
            plat.low := sec.floorheight;
          plat.high := P_FindHighestFloorSurrounding(sec);
          if plat.high < sec.floorheight then
            plat.high := sec.floorheight;
          plat.wait := args[2];
          plat.status := plat_e((P_Random and 1) + Ord(PLAT_UP));
        end;
      PLAT_PERPETUALRAISELIP:
        begin
          plat.low := P_FindLowestFloorSurrounding(sec) + args[3] * FRACUNIT;
          if plat.low > sec.floorheight then
            plat.low := sec.floorheight;
          plat.high := P_FindHighestFloorSurrounding(sec);
          if plat.high < sec.floorheight then
            plat.high := sec.floorheight;
          plat.wait := args[2];
          plat.status := plat_e((P_Random and 1) + Ord(PLAT_UP));
        end;
    end;
    PH_AddActivePlat(plat);
    {$IFDEF HEXEN}
    S_StartSequence(Pmobj_t(@sec.soundorg), Ord(SEQ_PLATFORM) + Ord(sec.seqType));
    {$ELSE}
    S_StartSound(@sec.soundorg, sec.seqType);
    {$ENDIF}
  end;
end;

//==============================================================================
//
// EVH_StopPlat
//
//==============================================================================
procedure EVH_StopPlat(line: Pline_t; args: PByteArray);
var
  i: integer;
begin
  for i := 0 to MAXPLATS - 1 do
  begin
    hactiveplats[i].tag := args[0]; // JVAL SOS
    if args[0] <> 0 then
    begin
      hactiveplats[i].sector.specialdata := nil;
      P_TagFinished(hactiveplats[i].sector.tag);
      P_RemoveThinker(@hactiveplats[i].thinker);
      hactiveplats[i] := nil;

      exit;
    end;
  end;

end;

//==============================================================================
//
// PH_AddActivePlat
//
//==============================================================================
procedure PH_AddActivePlat(plat: Pplat_t);
var
  i: integer;
begin
  for i := 0 to MAXPLATS - 1 do
    if hactiveplats[i] = nil then
    begin
      hactiveplats[i] := plat;
      exit;
    end;
  I_Error('PH_AddActivePlat(): no more plats!');
end;

//==============================================================================
//
// PH_RemoveActivePlat
//
//==============================================================================
procedure PH_RemoveActivePlat(plat: Pplat_t);
var
  i: integer;
begin
  for i := 0 to MAXPLATS - 1 do
    if plat = hactiveplats[i] then
    begin
      hactiveplats[i].sector.specialdata := nil;
      P_TagFinished(plat.sector.tag);
      P_RemoveThinker(@hactiveplats[i].thinker);
      hactiveplats[i] := nil;
      exit;
    end;
  I_Error('PH_RemoveActivePlat(): can''t find plat!');
end;

end.
