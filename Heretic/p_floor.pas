//------------------------------------------------------------------------------
//
//  DelphiHeretic: A modified and improved Heretic port for Windows
//  based on original Linux Doom as published by "id Software", on
//  Heretic source as published by "Raven" software and DelphiDoom
//  as published by Jim Valavanis.
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
//  Floor animation: raising stairs.
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit p_floor;

interface

uses
  doomdef,
  z_zone,
  p_spec,
  m_fixed,
  p_local,
  r_defs,
  s_sound,
  doomstat,
  sounds;

//
// FLOORS
//

function T_MovePlane(sector: Psector_t; speed: fixed_t; dest: fixed_t;
  crush: boolean; floorOrCeiling: integer; direction: integer): result_e;

procedure T_MoveFloor(floor: Pfloormove_t);

function EV_DoFloor(line: Pline_t; floortype: floor_e): integer;

function EV_BuildStairs(line: Pline_t; step: fixed_t): integer;

implementation

uses
  d_delphi,
  doomdata,
  p_map,
  p_tick,
  p_mobj_h,
  p_setup,
  p_slopes,
  r_data;

//
// Move a plane (floor or ceiling) and check for crushing
//
function T_MovePlane(sector: Psector_t; speed: fixed_t; dest: fixed_t;
  crush: boolean; floorOrCeiling: integer; direction: integer): result_e;
var
  lastpos: fixed_t;
begin
  case floorOrCeiling of
    0:
      begin
        // FLOOR
        if direction = -1 then
        begin
        // DOWN
          if sector.floorheight - speed < dest then
          begin
            lastpos := sector.floorheight;
            sector.floorheight := dest;
            if P_ChangeSector(sector, crush) then
            begin
              sector.floorheight := lastpos;
              P_ChangeSector(sector, crush);
            //return crushed;
            end;
            result := pastdest;
            exit;
          end
          else
          begin
            lastpos := sector.floorheight;
            sector.floorheight := sector.floorheight - speed;
            if P_ChangeSector(sector, crush) then
            begin
              sector.floorheight := lastpos;
              P_ChangeSector(sector, crush);
              result := crushed;
              exit;
            end;
          end;
        end
        else if direction = 1 then
        begin
        // UP
          if sector.floorheight + speed > dest then
          begin
            lastpos := sector.floorheight;
            sector.floorheight := dest;
            if P_ChangeSector(sector, crush) then
            begin
              sector.floorheight := lastpos;
              P_ChangeSector(sector, crush);
            //return crushed;
            end;
            result := pastdest;
            exit;
          end
          else
          begin
          // COULD GET CRUSHED
            lastpos := sector.floorheight;
            sector.floorheight := sector.floorheight + speed;
            if P_ChangeSector(sector, crush) then
            begin
              if crush then
              begin
                result := crushed;
                exit;
              end;
              sector.floorheight := lastpos;
              P_ChangeSector(sector, crush);
              result := crushed;
              exit;
            end
          end;
        end;
      end;
    1:
      begin
      // CEILING
        if direction = -1 then
        begin
        // DOWN
          if sector.ceilingheight - speed < dest then
          begin
            lastpos := sector.ceilingheight;
            sector.ceilingheight := dest;
            if P_ChangeSector(sector, crush) then
            begin
              sector.ceilingheight := lastpos;
              P_ChangeSector(sector, crush);
            //return crushed;
            end;
            result := pastdest;
            exit;
          end
          else
          begin
          // COULD GET CRUSHED
            lastpos := sector.ceilingheight;
            sector.ceilingheight := sector.ceilingheight - speed;
            if P_ChangeSector(sector, crush) then
            begin
              if crush then
              begin
                result := crushed;
                exit;
              end;
              sector.ceilingheight := lastpos;
              P_ChangeSector(sector, crush);
              result := crushed;
              exit;
            end;
          end;
        end
        else if direction = 1 then
        begin
        // UP
          if sector.ceilingheight + speed > dest then
          begin
            lastpos := sector.ceilingheight;
            sector.ceilingheight := dest;
            if P_ChangeSector(sector, crush) then
            begin
              sector.ceilingheight := lastpos;
              P_ChangeSector(sector, crush);
            //return crushed;
            end;
            result := pastdest;
            exit;
          end
          else
          begin
//            lastpos := sector.ceilingheight;
            sector.ceilingheight := sector.ceilingheight + speed;
            P_ChangeSector(sector, crush);
// UNUSED
//#if 0
//    if (flag == true)
//    {
//        sector->ceilingheight = lastpos;
//        P_ChangeSector(sector,crush);
//        return crushed;
//    }
//#endif
          end;
        end;
      end;
  end;
  P_DynamicSlope(sector); // JVAL: Slopes
  result := ok;
end;

//
// MOVE A FLOOR TO IT'S DESTINATION (UP OR DOWN)
//
procedure T_MoveFloor(floor: Pfloormove_t);
var
  res: result_e;
begin
  res := T_MovePlane(floor.sector, floor.speed, floor.floordestheight,
            floor.crush, 0, floor.direction);

  if leveltime and 7 = 0 then
    S_StartSound(Pmobj_t(@floor.sector.soundorg), Ord(sfx_stnmov));

  if res = pastdest then
  begin
    floor.sector.specialdata := nil;

    if floor._type = raiseBuildStep then
      S_StartSound(Pmobj_t(@floor.sector.soundorg), Ord(sfx_pstop));

    if floor.direction = 1 then
    begin
      if floor._type = donutRaise then
      begin
        floor.sector.special := floor.newspecial;
        floor.sector.floorpic := floor.texture;
      end;
    end
    else if floor.direction = -1 then
    begin
      if floor._type = lowerAndChange then
      begin
        floor.sector.special := floor.newspecial;
        floor.sector.floorpic := floor.texture;
      end;
    end;

    P_RemoveThinker(@floor.thinker);
  end;
end;

//
// HANDLE FLOOR TYPES
//
function EV_DoFloor(line: Pline_t; floortype: floor_e): integer;
var
  secnum: integer;
  i: integer;
  sec: Psector_t;
  floor: Pfloormove_t;
  minsize: integer;
  side: Pside_t;
begin
  secnum := -1;
  result := 0;
  repeat
    secnum := P_FindSectorFromLineTag(line, secnum);
    if secnum < 0 then
      exit;

    sec := @sectors[secnum];

    // ALREADY MOVING?  IF SO, KEEP GOING...
    if sec.specialdata <> nil then
      continue;

    // new floor thinker
    result := 1;
    floor := Z_Malloc(SizeOf(floormove_t), PU_LEVSPEC, nil);
    P_AddThinker(@floor.thinker);
    sec.specialdata := floor;
    floor.thinker._function.acp1 := @T_MoveFloor;
    floor._type := floortype;
    floor.crush := false;

    case floortype of
      lowerFloor:
        begin
          floor.direction := -1;
          floor.sector := sec;
          floor.speed := FLOORSPEED;
          floor.floordestheight := P_FindHighestFloorSurrounding(sec);
        end;
      lowerFloorToLowest:
        begin
          floor.direction := -1;
          floor.sector := sec;
          floor.speed := FLOORSPEED;
          floor.floordestheight := P_FindLowestFloorSurrounding(sec);
        end;
      turboLower:
        begin
          floor.direction := -1;
          floor.sector := sec;
          floor.speed := FLOORSPEED * 4;
          floor.floordestheight :=
          P_FindHighestFloorSurrounding(sec);
          if floor.floordestheight <> sec.floorheight then
            floor.floordestheight := floor.floordestheight + 8 * FRACUNIT;
        end;
      raiseFloorCrush,
      raiseFloor:
        begin
          if floortype = raiseFloorCrush then
            floor.crush := true;
          floor.direction := 1;
          floor.sector := sec;
          floor.speed := FLOORSPEED;
          floor.floordestheight := P_FindLowestCeilingSurrounding(sec);
          if floor.floordestheight > sec.ceilingheight then
            floor.floordestheight := sec.ceilingheight;
          if floortype = raiseFloorCrush then // if floor.crush then
            floor.floordestheight := floor.floordestheight - 8 * FRACUNIT;
        end;
      raiseFloorToNearest:
        begin
          floor.direction := 1;
          floor.sector := sec;
          floor.speed := FLOORSPEED;
          floor.floordestheight := P_FindNextHighestFloor(sec, sec.floorheight);
        end;
      raiseFloor24:
        begin
          floor.direction := 1;
          floor.sector := sec;
          floor.speed := FLOORSPEED;
          floor.floordestheight := floor.sector.floorheight + 24 * FRACUNIT;
        end;
      raiseFloor24AndChange:
        begin
          floor.direction := 1;
          floor.sector := sec;
          floor.speed := FLOORSPEED;
          floor.floordestheight := floor.sector.floorheight + 24 * FRACUNIT;
          sec.floorpic := line.frontsector.floorpic;
          sec.special := line.frontsector.special;
        end;
      raiseToTexture:
        begin
          minsize := MAXINT;
          floor.direction := 1;
          floor.sector := sec;
          floor.speed := FLOORSPEED;
          for i := 0 to sec.linecount - 1 do
          begin
            if twoSided(secnum, i) <> 0 then
            begin
              side := getSide(secnum, i, 0);
              if side.bottomtexture >= 0 then
                if textureheight[side.bottomtexture] < minsize then
                  minsize := textureheight[side.bottomtexture];
              side := getSide(secnum, i, 1);
              if side.bottomtexture >= 0 then
                if textureheight[side.bottomtexture] < minsize then
                  minsize := textureheight[side.bottomtexture];
            end;
          end;
          floor.floordestheight := floor.sector.floorheight + minsize;
        end;
      lowerAndChange:
        begin
          floor.direction := -1;
          floor.sector := sec;
          floor.speed := FLOORSPEED;
          floor.floordestheight := P_FindLowestFloorSurrounding(sec);
          floor.texture := sec.floorpic;
          for i := 0 to sec.linecount - 1 do
          begin
            if twoSided(secnum, i) <> 0 then
            begin
              side := getSide(secnum, i, 0);
              if pDiff(side.sector, @sectors[0], SizeOf(side.sector^)) = secnum then
              begin
                sec := getSector(secnum, i, 1);
                if sec.floorheight = floor.floordestheight then
                begin
                  floor.texture := sec.floorpic;
                  floor.newspecial := sec.special;
                  break;
                end;
              end
              else
              begin
                sec := getSector(secnum, i, 0);
                if sec.floorheight = floor.floordestheight then
                begin
                  floor.texture := sec.floorpic;
                  floor.newspecial := sec.special;
                  break;
                end;
              end;
            end;
          end;
        end;
    end;
    P_DynamicSlope(sec);  // JVAL: Slopes
  until secnum < 0;
end;

//
// BUILD A STAIRCASE!
//
function EV_BuildStairs(line: Pline_t; step: fixed_t): integer;
var
  secnum: integer;
  height: integer;
  i: integer;
  newsecnum: integer;
  texture: integer;
  ok: boolean;
  sec: Psector_t;
  tsec: Psector_t;
  floor: Pfloormove_t;
begin
  secnum := -1;
  result := 0;

  repeat
    secnum := P_FindSectorFromLineTag(line, secnum);
    if secnum < 0 then
      exit;

    sec := @sectors[secnum];

    // ALREADY MOVING?  IF SO, KEEP GOING...
    if sec.specialdata <> nil then
      continue;

    // new floor thinker
    result := 1;
    floor := Z_Malloc(SizeOf(floormove_t), PU_LEVSPEC, nil);
    P_AddThinker(@floor.thinker);
    sec.specialdata := floor;
    height := sec.floorheight + step;
    floor.thinker._function.acp1 := @T_MoveFloor;
    floor._type := raiseBuildStep;
    floor.direction := 1;
    floor.sector := sec;
    floor.speed := FLOORSPEED;
    floor.floordestheight := sec.floorheight + step;

    texture := sec.floorpic;

    // Find next sector to raise
    // 1.  Find 2-sided line with same sector side[0]
    // 2.  Other side is the next sector to raise
    repeat
      ok := false;
      for i := 0 to sec.linecount - 1 do
      begin
        if sec.lines[i].flags and ML_TWOSIDED = 0 then
          continue;

        tsec := sec.lines[i].frontsector;
        newsecnum := pDiff(tsec, @sectors[0], SizeOf(sector_t));

        if secnum <> newsecnum then
          continue;

        tsec := sec.lines[i].backsector;
        if tsec = nil then
          continue;
        newsecnum := pDiff(tsec, @sectors[0], SizeOf(sector_t));

        if tsec.floorpic <> texture then
          continue;

        height := height + step;

        if tsec.specialdata <> nil then
          continue;

        sec := tsec;
        secnum := newsecnum;
        floor := Z_Malloc(SizeOf(floormove_t), PU_LEVSPEC, nil);

        P_AddThinker(@floor.thinker);

        sec.specialdata := floor;
        floor.thinker._function.acp1 := @T_MoveFloor;
        floor._type := raiseBuildStep;
        floor.direction := 1;
        floor.sector := sec;
        floor.speed := FLOORSPEED;
        floor.floordestheight := height;
        ok := true;
        break;
      end;
    until not ok;
  until secnum < 0;
end;

end.

