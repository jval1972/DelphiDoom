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
//  Boucnhing
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit p_bouncing;

interface

uses
  p_mobj_h,
  r_defs;

//==============================================================================
//
// P_WallBounceMobj
//
//==============================================================================
procedure P_WallBounceMobj(const mo: Pmobj_t; const line: Pline_t);

//==============================================================================
//
// P_MobjBounceMobj
//
//==============================================================================
procedure P_MobjBounceMobj(const mo: Pmobj_t; const othermo: Pmobj_t);

implementation

uses
  m_fixed,
  m_vectors,
  p_map,
  r_main;

//==============================================================================
//
// P_PointLineSqrDistance
//
//==============================================================================
function P_PointLineSqrDistance(const x, y: fixed_t; const line: Pline_t): integer;
var
  A, B, C, D: integer;
  dot: int64;
  len_sq: int64;
  param: int64;
  xx, yy: integer;
  dx, dy: integer;
  x1, x2, y1, y2: integer;
  ix, iy: integer;
begin
  x1 := line.v1.x div FRACUNIT;
  y1 := line.v1.y div FRACUNIT;
  x2 := line.v2.x div FRACUNIT;
  y2 := line.v2.y div FRACUNIT;
  ix := x div FRACUNIT;
  iy := y div FRACUNIT;

  A := ix - x1;
  B := iy - y1;
  C := x2 - x1;
  D := y2 - y1;

  dot := (A * C) + (B * D);
  len_sq := (C * C) + (D * D);
  param := -1;
  if len_sq <> 0 then
    param := (dot * FRACUNIT) div int64(len_sq);

  if param < 0 then
  begin
    xx := x1;
    yy := y1;
  end
  else if param > FRACUNIT then
  begin
    xx := x2;
    yy := y2;
  end
  else
  begin
    xx := x1 + (param * C) div FRACUNIT;
    yy := y1 + (param * D) div FRACUNIT;
  end;

  dx := ix - xx;
  dy := iy - yy;
  result := (dx * dx) + (dy * dy);
end;

//==============================================================================
//
// P_WallBounceMobj
//
//==============================================================================
procedure P_WallBounceMobj(const mo: Pmobj_t; const line: Pline_t);
var
  s1, s2: boolean;
  d, wall, reflect: vec2_t;
  dist1, dist2: integer;
  newx, newy: fixed_t;
begin
  dist1 := P_PointLineSqrDistance(mo.x, mo.y, line);
  dist2 := P_PointLineSqrDistance(mo.x + mo.momx, mo.y + mo.momy, line);
  s1 := R_PointOnLineSide(mo.x, mo.y, line);
  s2 := R_PointOnLineSide(mo.x + mo.momx, mo.y + mo.momy, line);
  if (s1 <> s2) or ((s1 = s2) and (dist2 < dist1)) then
  begin
    d[0] := mo.momx / FRACUNIT;
    d[1] := mo.momy / FRACUNIT;
    wall[0] := line.dx / FRACUNIT;
    wall[1] := line.dy / FRACUNIT;
    CalculateReflect2(d, wall, reflect);
    mo.momx := Round(reflect[0] * FRACUNIT);
    mo.momy := Round(reflect[1] * FRACUNIT);
    mo.angle := R_PointToAngle2(0, 0, mo.momx, mo.momy);
  end;
  newx := mo.x + mo.momx;
  newy := mo.y + mo.momy;
  if P_TryMove(mo, newx, newy) then
  begin
    mo.x := newx;
    mo.y := newy;
  end;
end;

//==============================================================================
//
// P_MobjBounceMobj
//
//==============================================================================
procedure P_MobjBounceMobj(const mo: Pmobj_t; const othermo: Pmobj_t);
var
  d, wall, reflect: vec2_t;
begin
  d[0] := mo.momx / FRACUNIT;
  d[1] := mo.momy / FRACUNIT;
  wall[0] := -othermo.y / FRACUNIT;
  wall[1] := othermo.x / FRACUNIT;
  CalculateReflect2(d, wall, reflect);
  mo.momx := Round(reflect[0] * FRACUNIT);
  mo.momy := Round(reflect[1] * FRACUNIT);
  mo.angle := R_PointToAngle2(0, 0, mo.momx, mo.momy);
end;

end.

