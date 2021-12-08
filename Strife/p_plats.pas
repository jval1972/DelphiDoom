//------------------------------------------------------------------------------
//
//  DelphiStrife: A modified and improved Strife source port for Windows.
//
//  Based on:
//    - Linux Doom by "id Software"
//    - Chocolate Strife by "Simon Howard"
//    - DelphiDoom by "Jim Valavanis"
//
//  Copyright (C) 1993-1996 by id Software, Inc.
//  Copyright (C) 2005 Simon Howard
//  Copyright (C) 2010 James Haley, Samuel Villarreal
//  Copyright (C) 2004-2021 by Jim Valavanis
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
//  Plats (i.e. elevator platforms) code, raising/lowering.
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit p_plats;

interface

uses
  p_spec,
  r_defs;

var
  activeplats: array[0..MAXPLATS - 1] of Pplat_t;

procedure T_PlatRaise(plat: Pplat_t);

function EV_DoPlat(line: Pline_t; _type: plattype_e; amount: integer): integer;

procedure P_ActivateInStasis(tag: integer);

function EV_StopPlat(line: Pline_t): integer;

procedure P_AddActivePlat(plat: Pplat_t);

procedure P_RemoveActivePlat(plat: Pplat_t);

implementation

uses
  i_system,
  doomdef,
  m_fixed,
  m_rnd,
  p_mobj_h,
  p_tick,
  p_floor,
  p_setup,
  s_sound,
  sounds,
  z_zone;

procedure T_PlatRaise(plat: Pplat_t);
var
  res: result_e;
begin
  case plat.status of
    up:
      begin
        res := T_MovePlane(plat.sector, plat.speed, plat.high, plat.crush, 0, 1);

        if (plat._type = raiseAndChange) or
           (plat._type = raiseToNearestAndChange) or
           (plat._type = slowDWUS) then // villsa [STRIFE]
        begin
          if leveltime and 7 = 0 then
            S_StartSound(Pmobj_t(@plat.sector.soundorg), Ord(sfx_stnmov));
        end;

        if (res = crushed) and not plat.crush then
        begin
          plat.count := plat.wait;
          plat.status := down;
          S_StartSound(Pmobj_t(@plat.sector.soundorg), Ord(sfx_pstart));
        end
        else
        begin
          if res = pastdest then
          begin
            if plat._type = toggleUpDn then
            begin             // in stasis awaiting next toggle activation
              plat.oldstatus := plat.status;//jff 3/14/98 after action wait
              plat.status := in_stasis;      //for reactivation of toggle
            end
            else
            begin
              plat.count := plat.wait;
              plat.status := waiting;
              S_StartSound(Pmobj_t(@plat.sector.soundorg), Ord(sfx_pstop));
            end;

            // lift types and pure raise types are done at end of up stroke
            // only the perpetual type waits then goes back up
            case plat._type of
              blazeDWUS,
              downWaitUpStay,
              raiseAndChange,
              raiseToNearestAndChange,
              genLift,
              slowDWUS: // villsa [STRIFE]
                P_RemoveActivePlat(plat);
            end;
          end;
        end;
      end;

    down:
      begin
        res := T_MovePlane(plat.sector, plat.speed, plat.low, false, 0, -1);

        // villsa [STRIFE]
        if plat._type = slowDWUS then
          if leveltime and 7 = 0 then
            S_StartSound(Pmobj_t(@plat.sector.soundorg), Ord(sfx_stnmov));

        if res = pastdest then
        begin
          if plat._type = toggleUpDn then //jff 3/14/98 toggle up down
          begin // instant toggles go into stasis awaiting next activation
            plat.oldstatus := plat.status;//jff 3/14/98 after action wait
            plat.status := in_stasis;      //for reactivation of toggle
          end
          else
          begin
            plat.count := plat.wait;
            plat.status := waiting;
            S_StartSound(Pmobj_t(@plat.sector.soundorg), Ord(sfx_pstop));
          end;

          //jff 1/26/98 remove the plat if it bounced so it can be tried again
          //only affects plats that raise and bounce
          //killough 1/31/98: relax compatibility to demo_compatibility

          // remove the plat if its a pure raise type
          if plat._type in [raiseAndChange, raiseToNearestAndChange, upWaitDownStay] then
            P_RemoveActivePlat(plat);

        end;
      end;

    waiting: // plat is waiting
      begin
        plat.count := plat.count - 1;
        if plat.count = 0 then
        begin
          if plat.sector.floorheight = plat.low then
            plat.status := up
          else
            plat.status := down;
          S_StartSound(Pmobj_t(@plat.sector.soundorg), Ord(sfx_pstart));
        end;
      end;
  end;
end;

//
// Do Platforms
//  "amount" is only used for SOME platforms.
//
function EV_DoPlat(line: Pline_t; _type: plattype_e; amount: integer): integer;
var
  plat: Pplat_t;
  secnum: integer;
  sec: Psector_t;
begin
  // Activate all <type> plats that are in_stasis
  if _type = toggleUpDn then
  begin
    P_ActivateInStasis(line.tag);
    result := 1;
  end
  else
  begin
    if _type = perpetualRaise then
      P_ActivateInStasis(line.tag);
    result := 0;
  end;

  secnum := -1;
  while P_FindSectorFromLineTag2(line, secnum) >= 0 do
  begin
    sec := @sectors[secnum];

    // don't start a second floor function if already moving
    if P_SectorActive(floor_special, sec) then
      continue;

    // Find lowest & highest floors around sector
    result := 1;
    plat := Z_Malloc(SizeOf(plat_t), PU_LEVSPEC, nil);
    P_AddThinker(@plat.thinker);

    plat._type := _type;
    plat.sector := sec;
    plat.sector.floordata := plat;
    plat.thinker._function.acp1 := @T_PlatRaise;
    plat.crush := false;
    plat.tag := line.tag;

    //jff 1/26/98 Avoid raise plat bouncing a head off a ceiling and then
    //going down forever -- default low to plat height when triggered
    plat.low := sec.floorheight;

    case _type of
      raiseToNearestAndChange:
        begin
          plat.speed := PLATSPEED div 2;
          sec.floorpic := sides[line.sidenum[0]].sector.floorpic;
          plat.high := P_FindNextHighestFloor(sec, sec.floorheight);
          plat.wait := 0;
          plat.status := up;
          // NO MORE DAMAGE, IF APPLICABLE
          sec.special := 0;
          sec.oldspecial := 0;
          S_StartSound(Pmobj_t(@sec.soundorg), Ord(sfx_stnmov));
        end;

      raiseAndChange:
        begin
          plat.speed := PLATSPEED div 2;
          sec.floorpic := sides[line.sidenum[0]].sector.floorpic;
          plat.high := sec.floorheight + amount * FRACUNIT;
          plat.wait := 0;
          plat.status := up;
          S_StartSound(Pmobj_t(@sec.soundorg), Ord(sfx_stnmov));
        end;

      // villsa [STRIFE]
      upWaitDownStay:
        begin
          plat.speed := PLATSPEED * 4;
          plat.high := P_FindNextHighestFloor(sec, sec.floorheight);
          plat.low := sec.floorheight;
          plat.wait := TICRATE * PLATWAIT;
          plat.status := up;
          S_StartSound(Pmobj_t(@sec.soundorg), Ord(sfx_pstart));
        end;

      downWaitUpStay:
        begin
          plat.speed := PLATSPEED * 4;
          plat.low := P_FindLowestFloorSurrounding(sec);
          if plat.low > sec.floorheight then
            plat.low := sec.floorheight;
          plat.high := sec.floorheight;
          plat.wait := TICRATE * PLATWAIT;
          plat.status := down;
          S_StartSound(Pmobj_t(@sec.soundorg), Ord(sfx_pstart));
        end;

      // villsa [STRIFE]
      slowDWUS:
        begin
          plat.speed := PLATSPEED;
          plat.low := P_FindLowestFloorSurrounding(sec);

          if plat.low > sec.floorheight then
            plat.low := sec.floorheight;

          plat.high := sec.floorheight;
          plat.wait := TICRATE * (PLATWAIT * 10);
          plat.status := down;
          S_StartSound(Pmobj_t(@sec.soundorg), Ord(sfx_pstart));
        end;

      blazeDWUS:
        begin
          plat.speed := PLATSPEED * 8;
          plat.low := P_FindLowestFloorSurrounding(sec);
          if plat.low > sec.floorheight then
            plat.low := sec.floorheight;
          plat.high := sec.floorheight;
          plat.wait := TICRATE * PLATWAIT;
          plat.status := down;
          S_StartSound(Pmobj_t(@sec.soundorg), Ord(sfx_pstart));
        end;

      perpetualRaise:
        begin
          plat.speed := PLATSPEED;
          plat.low := P_FindLowestFloorSurrounding(sec);
          if plat.low > sec.floorheight then
            plat.low := sec.floorheight;
          plat.high := P_FindHighestFloorSurrounding(sec);
          if plat.high < sec.floorheight then
            plat.high := sec.floorheight;
          plat.wait := TICRATE * PLATWAIT;
          plat.status := plat_e(P_Random and 1);
          S_StartSound(Pmobj_t(@sec.soundorg), Ord(sfx_pstart));
        end;

      toggleUpDn: //jff 3/14/98 add new type to support instant toggle
        begin
          plat.speed := PLATSPEED;  //not used
          plat.wait := 35 * PLATWAIT; //not used
          plat.crush := true; //jff 3/14/98 crush anything in the way

          // set up toggling between ceiling, floor inclusive
          plat.low := sec.ceilingheight;
          plat.high := sec.floorheight;
          plat.status := down;
        end;

    end;
    P_AddActivePlat(plat);
  end;
end;

procedure P_ActivateInStasis(tag: integer);
var
  i: integer;
  plt: Pplat_t;
begin
  for i := 0 to MAXPLATS - 1 do
  begin
    plt := activeplats[i];
    if (plt <> nil) and
      (plt.tag = tag) and
      (plt.status = in_stasis) then
    begin
      plt.status := plt.oldstatus;
      plt.thinker._function.acp1 := @T_PlatRaise;
    end;
  end;
end;


function EV_StopPlat(line: Pline_t): integer;
var
  i: integer;
  plt: Pplat_t;
begin
  result := 0;
  for i := 0 to MAXPLATS - 1 do
  begin
    plt := activeplats[i];
    if (plt <> nil) and
      (plt.status <> in_stasis) and
      (plt.tag = line.tag) then
    begin
      result := 1;
      plt.oldstatus := plt.status;
      plt.status := in_stasis;
      plt.thinker._function.acv := nil;
    end;
  end;
end;

procedure P_AddActivePlat(plat: Pplat_t);
var
  i: integer;
begin
  for i := 0 to MAXPLATS - 1 do
    if activeplats[i] = nil then
    begin
      activeplats[i] := plat;
      exit;
    end;

  I_Error('P_AddActivePlat(): no more plats!');
end;

procedure P_RemoveActivePlat(plat: Pplat_t);
var
  i: integer;
begin
  for i := 0 to MAXPLATS - 1 do
    if plat = activeplats[i] then
    begin
      activeplats[i].sector.floordata := nil;
      P_RemoveThinker(@activeplats[i].thinker);
      activeplats[i] := nil;
      exit;
    end;

  I_Error('P_RemoveActivePlat(): can''t find plat!');
end;

end.
