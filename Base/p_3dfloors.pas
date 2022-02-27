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

unit p_3dfloors;

interface

uses
  d_delphi,
  doomdata,
  m_fixed,
  p_mobj_h,
  r_defs;

//==============================================================================
//
// P_3dFloorNumber
//
//==============================================================================
function P_3dFloorNumber(const m: Pmobj_t): integer; overload;

//==============================================================================
//
// P_3dFloorNumber
//
//==============================================================================
function P_3dFloorNumber(const s: Psector_t; const z: fixed_t): integer; overload;

//==============================================================================
//
// P_3dFloorFindMapthingFloorDZ
//
//==============================================================================
function P_3dFloorFindMapthingFloorDZ(const m: Pmapthing_t): fixed_t;

//==============================================================================
//
// P_3dFloorFindMapthingFloorZ
//
//==============================================================================
function P_3dFloorFindMapthingFloorZ(const m: Pmapthing_t): fixed_t;

//==============================================================================
//
// P_3dFloorFindMapthingCeilingZ
//
//==============================================================================
function P_3dFloorFindMapthingCeilingZ(const m: Pmapthing_t): fixed_t;

//==============================================================================
//
// P_3dFloorHeight
//
//==============================================================================
function P_3dFloorHeight(const x, y, z: fixed_t): fixed_t; overload;

//==============================================================================
//
// P_3dCeilingHeight
//
//==============================================================================
function P_3dCeilingHeight(const x, y, z: fixed_t): fixed_t; overload;

//==============================================================================
//
// P_3dFloorHeight
//
//==============================================================================
function P_3dFloorHeight(const m: Pmobj_t): fixed_t; overload;

//==============================================================================
//
// P_3dCeilingHeight
//
//==============================================================================
function P_3dCeilingHeight(const m: Pmobj_t): fixed_t; overload;

//==============================================================================
//
// P_3dFloorHeight
//
//==============================================================================
function P_3dFloorHeight(const s: Psector_t; const x, y, z: fixed_t): fixed_t; overload;

//==============================================================================
//
// P_3dCeilingHeight
//
//==============================================================================
function P_3dCeilingHeight(const s: Psector_t; const x, y, z: fixed_t): fixed_t; overload;

//==============================================================================
//
// P_PtInSolidFloor
//
//==============================================================================
function P_PtInSolidFloor(const x, y, z: fixed_t): boolean;

//==============================================================================
//
// P_PtInSolidFloor2
//
//==============================================================================
function P_PtInSolidFloor2(const x, y, z: fixed_t; const radious: fixed_t): boolean;

//==============================================================================
//
// P_SubSectorCentroid
//
//==============================================================================
function P_SubSectorCentroid(const s: Psubsector_t): Psubsector_t;

//==============================================================================
//
// P_3dFloorSetupSegs
//
//==============================================================================
procedure P_3dFloorSetupSegs;

implementation

uses
  doomdef,
  p_setup,
  p_slopes,
  p_spec,
  r_main,
  tables;

//==============================================================================
// P_3dFloorNumber
//
// Which floor?
//
//==============================================================================
function P_3dFloorNumber(const m: Pmobj_t): integer;
var
  ss: Psubsector_t;
  msec: Psector_t;
begin
  ss := R_PointInSubsector(m.x, m.y);
  if ss.sector.midsec >= 0 then
  begin
    msec := @sectors[ss.sector.midsec];
    if m.z <= msec.floorheight then
      result := 0
    else
      result := 1;
  end
  else
    result := 0;
end;

//==============================================================================
//
// P_3dFloorNumber
//
//==============================================================================
function P_3dFloorNumber(const s: Psector_t; const z: fixed_t): integer; overload;
var
  msec: Psector_t;
begin
  if s.midsec >= 0 then
  begin
    msec := @sectors[s.midsec];
    if z <= msec.floorheight then
      result := 0
    else
      result := 1;
  end
  else
    result := 0;
end;

//==============================================================================
//
// P_3dFloorFindMapthingFloorDZ
//
//==============================================================================
function P_3dFloorFindMapthingFloorDZ(const m: Pmapthing_t): fixed_t;
var
  ss: Psubsector_t;
  msec: Psector_t;
  x, y: fixed_t;
begin
  if m.options and MTF_ONMIDSECTOR = 0 then
  begin
    result := 0;
    exit;
  end;

  x := m.x * FRACUNIT;
  y := m.y * FRACUNIT;
  ss := R_PointInSubsector(x, y);
  if ss.sector.midsec >= 0 then
  begin
    msec := @sectors[ss.sector.midsec];
    result := msec.ceilingheight - P_FloorHeight(ss.sector, x, y);
  end
  else
    result := 0;
end;

//==============================================================================
//
// P_3dFloorFindMapthingFloorZ
//
//==============================================================================
function P_3dFloorFindMapthingFloorZ(const m: Pmapthing_t): fixed_t;
var
  ss: Psubsector_t;
  msec: Psector_t;
  x, y: fixed_t;
begin
  x := m.x * FRACUNIT;
  y := m.y * FRACUNIT;
  ss := R_PointInSubsector(x, y);
  if m.options and MTF_ONMIDSECTOR = 0 then
  begin
    result := P_FloorHeight(ss.sector, x, y);
    exit;
  end;

  if ss.sector.midsec >= 0 then
  begin
    msec := @sectors[ss.sector.midsec];
    result := msec.ceilingheight;
  end
  else
    result := P_FloorHeight(ss.sector, x, y);
end;

//==============================================================================
//
// P_3dFloorFindMapthingCeilingZ
//
//==============================================================================
function P_3dFloorFindMapthingCeilingZ(const m: Pmapthing_t): fixed_t;
var
  ss: Psubsector_t;
  msec: Psector_t;
  x, y: fixed_t;
begin
  x := m.x * FRACUNIT;
  y := m.y * FRACUNIT;
  ss := R_PointInSubsector(x, y);
  if m.options and MTF_ONMIDSECTOR <> 0 then
  begin
    result := P_CeilingHeight(ss.sector, x, y);
    exit;
  end;

  if ss.sector.midsec >= 0 then
  begin
    msec := @sectors[ss.sector.midsec];
    result := msec.floorheight;
  end
  else
    result := P_CeilingHeight(ss.sector, x, y);
end;

//==============================================================================
//
// P_3dFloorHeight
//
//==============================================================================
function P_3dFloorHeight(const x, y, z: fixed_t): fixed_t;
var
  ss: Psubsector_t;
  msec: Psector_t;
begin
  ss := R_PointInSubsector(x, y);
  if ss.sector.midsec >= 0 then
  begin
    msec := @sectors[ss.sector.midsec];
    if z <= msec.floorheight then
      result := P_FloorHeight(ss.sector, x, y)
    else
      result := msec.ceilingheight;
  end
  else
    result := P_FloorHeight(ss.sector, x, y);
end;

//==============================================================================
//
// P_3dCeilingHeight
//
//==============================================================================
function P_3dCeilingHeight(const x, y, z: fixed_t): fixed_t;
var
  ss: Psubsector_t;
  msec: Psector_t;
begin
  ss := R_PointInSubsector(x, y);
  if ss.sector.midsec >= 0 then
  begin
    msec := @sectors[ss.sector.midsec];
    if z > msec.floorheight then
      result := P_CeilingHeight(ss.sector, x, y)
    else
      result := msec.floorheight;
  end
  else
    result := P_CeilingHeight(ss.sector, x, y);
end;

//==============================================================================
//
// P_3dFloorHeight
//
//==============================================================================
function P_3dFloorHeight(const m: Pmobj_t): fixed_t;
begin
  result := P_3dFloorHeight(m.x, m.y, m.z);
end;

//==============================================================================
//
// P_3dCeilingHeight
//
//==============================================================================
function P_3dCeilingHeight(const m: Pmobj_t): fixed_t; overload;
begin
  result := P_3dCeilingHeight(m.x, m.y, m.z);
end;

//==============================================================================
//
// P_3dFloorHeight
//
//==============================================================================
function P_3dFloorHeight(const s: Psector_t; const x, y, z: fixed_t): fixed_t; overload;
var
  msec: Psector_t;
begin
  if s.midsec >= 0 then
  begin
    msec := @sectors[s.midsec];
    if z <= msec.floorheight then
      result := P_FloorHeight(s, x, y)
    else
      result := msec.ceilingheight;
  end
  else
    result := P_FloorHeight(s, x, y);
end;

//==============================================================================
//
// P_3dCeilingHeight
//
//==============================================================================
function P_3dCeilingHeight(const s: Psector_t; const x, y, z: fixed_t): fixed_t; overload;
var
  msec: Psector_t;
begin
  if s.midsec >= 0 then
  begin
    msec := @sectors[s.midsec];
    if z > msec.floorheight then
      result := P_CeilingHeight(s, x, y)
    else
      result := msec.floorheight;
  end
  else
    result := P_CeilingHeight(s, x, y);
end;

//==============================================================================
//
// P_PtInSolidFloor
//
//==============================================================================
function P_PtInSolidFloor(const x, y, z: fixed_t): boolean;
var
  s: Psubsector_t;
  sec: Psector_t;
begin
  s := R_PointInSubsector(x, y);
  if s.sector.midsec >= 0 then
  begin
    sec := @sectors[s.sector.midsec];
    result := (z >= sec.floorheight) and (z <= sec.ceilingheight);
  end
  else
    result := false;
end;

//==============================================================================
//
// P_PtInSolidFloor2
//
//==============================================================================
function P_PtInSolidFloor2(const x, y, z: fixed_t; const radious: fixed_t): boolean;
var
  s: Psubsector_t;
  sec: Psector_t;
begin
  s := R_PointInSubsector(x, y);
  if s.sector.midsec >= 0 then
  begin
    sec := @sectors[s.sector.midsec];
    result := (z >= sec.floorheight - radious) and (z <= sec.ceilingheight + radious);
  end
  else
    result := false;
end;

//==============================================================================
// P_SubSectorCentroid
//
// JVAL: Calculate the centroid of a subsector
//
//==============================================================================
function P_SubSectorCentroid(const s: Psubsector_t): Psubsector_t;
var
  signedArea: Double;
  x0, y0: Double;
  x1, y1: Double;
  A: Double;
  i: integer;
  cx, cy: double;
begin
  result := s;
  if result.flags and SSF_CENTROIDCALCED <> 0 then
    exit;

  signedArea := 0.0;
  cx := 0.0;  // Temporary X
  cy := 0.0;  // Temporary Y

  for i := s.firstline to s.firstline + s.numlines - 1 do
  begin
    x0 := lines[i].v1.x / FRACUNIT;
    y0 := lines[i].v1.y / FRACUNIT;
    x1 := lines[i].v2.x / FRACUNIT;
    y1 := lines[i].v2.y / FRACUNIT;
    A := x0 * y1 - x1 * y0;
    signedArea := signedArea + A;
    cx := cx + (x0 + x1) * A;
    cy := cy + (y0 + y1) * A;
  end;

  signedArea := signedArea * 0.5 * 6.0;
  cx := cx / signedArea;
  cy := cy / signedArea;

  result.x := Round(cx * FRACUNIT);
  result.y := Round(cy * FRACUNIT);

  result.flags := result.flags or SSF_CENTROIDCALCED;
end;

//==============================================================================
//
// P_3dFloorSetupSegs
//
//==============================================================================
procedure P_3dFloorSetupSegs;
var
  i: integer;
  s, sec: integer;
begin
  for i := 0 to numlines - 1 do
  begin
    case lines[i].special of
      // JVAL: Middle sector (3d floor)
      {$IFDEF HEXEN}
      253:
      {$ELSE}
      281:
      {$ENDIF}
        begin
          sec := pDiff(sides[lines[i].sidenum[0]].sector, sectors, SizeOf(sector_t));
          s := -1;
          while P_FindSectorFromLineTag2(@lines[i], s) >= 0 do
          begin
            sectors[s].midsec := sec;
            sectors[s].midline := i;
          end;
        end;
    end;
  end;

end;

//==============================================================================
//
// P_3dFloorMapThingFromMobj
//
//==============================================================================
function P_3dFloorMapThingFromMobj(const mo: Pmobj_t; const x, y, z: integer): mapthing_t;
var
  ss: Psubsector_t;
  msec: Psector_t;
begin
  result.x := x div FRACUNIT;
  result.y := y div FRACUNIT;
  result.angle := mo.angle div ANG1;
  result._type := mo.spawnpoint._type;
  result.options := 0;
  ss := R_PointInSubsector(x, y);
  if ss.sector.midsec >= 0 then
  begin
    msec := @sectors[ss.sector.midsec];
    if z > msec.floorheight then
      result.options := MTF_ONMIDSECTOR
    else
      result.options := 0;
  end
  else
    result.options := 0;
end;

end.

