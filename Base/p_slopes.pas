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
//  Slopes.
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit p_slopes;

interface

uses
  d_delphi,
  m_fixed,
  r_defs;

//==============================================================================
//
// P_FloorHeight
//
//==============================================================================
function P_FloorHeight(const sec: Psector_t; const x, y: fixed_t): fixed_t; overload;

//==============================================================================
//
// P_FloorHeight
//
//==============================================================================
function P_FloorHeight(const x, y: fixed_t): fixed_t; overload;

//==============================================================================
//
// P_CeilingHeight
//
//==============================================================================
function P_CeilingHeight(const sec: Psector_t; const x, y: fixed_t): fixed_t; overload;

//==============================================================================
//
// P_CeilingHeight
//
//==============================================================================
function P_CeilingHeight(const x, y: fixed_t): fixed_t; overload;

//==============================================================================
//
// P_SlopesSetup
//
//==============================================================================
procedure P_SlopesSetup;

//==============================================================================
//
// P_DynamicSlope
//
//==============================================================================
procedure P_DynamicSlope(const sec: Psector_t);

//==============================================================================
//
// P_SlopesAlignPlane
//
//==============================================================================
procedure P_SlopesAlignPlane(const sec: Psector_t; const line: Pline_t; const flag: LongWord;
  const calcpivotline: boolean = true);

//==============================================================================
//
// P_ClosestFloorHeight
//
//==============================================================================
function P_ClosestFloorHeight(const sec: Psector_t; const line: Pline_t; const x, y: fixed_t): fixed_t;

//==============================================================================
//
// P_ClosestCeilingHeight
//
//==============================================================================
function P_ClosestCeilingHeight(const sec: Psector_t; const line: Pline_t; const x, y: fixed_t): fixed_t;

procedure calc_slope_plane(
  const x1, y1, z1: float;
  const x2, y2, z2: float;
  const x3, y3, z3: float;
  out fa, fb, fc, fd: float);

//==============================================================================
//
// PS_SetFloorSlope
//
//==============================================================================
procedure PS_SetFloorSlope(const secid: integer; const x1, y1, z1: fixed_t;
  const x2, y2, z2: fixed_t; const x3, y3, z3: fixed_t);

//==============================================================================
//
// PS_SetCeilingSlope
//
//==============================================================================
procedure PS_SetCeilingSlope(const secid: integer; const x1, y1, z1: fixed_t;
  const x2, y2, z2: fixed_t; const x3, y3, z3: fixed_t);

const
  SLOPECOUNTDOWN = 4;

implementation

uses
  doomdata,
  m_vectors,
  p_gravity,
  p_map,
  p_setup,
  p_mobj_h,
  p_spec,
  r_main;

//==============================================================================
//
// ZatPointFloor
//
//==============================================================================
function ZatPointFloor(const s: Psector_t; const x, y: fixed_t): fixed_t;
begin
  result := Round(((-s.fa * (x / FRACUNIT) - s.fb * (y / FRACUNIT) - s.fd) * s.fic) * FRACUNIT);
end;

//==============================================================================
//
// ZatPointCeiling
//
//==============================================================================
function ZatPointCeiling(const s: Psector_t; const x, y: fixed_t): fixed_t;
begin
  result := Round(((-s.ca * (x / FRACUNIT) - s.cb * (y / FRACUNIT) - s.cd) * s.cic) * FRACUNIT);
end;

//==============================================================================
//
// P_FloorHeight
//
//==============================================================================
function P_FloorHeight(const sec: Psector_t; const x, y: fixed_t): fixed_t; overload;
begin
  if sec.renderflags and SRF_SLOPEFLOOR <> 0 then
    result := ZatPointFloor(sec, x, y)
  else
    result := sec.floorheight;
end;

//==============================================================================
//
// P_FloorHeight
//
//==============================================================================
function P_FloorHeight(const x, y: fixed_t): fixed_t; overload;
begin
  result := P_FloorHeight(R_PointInSubSector(x, y).sector, x, y);
end;

//==============================================================================
//
// P_CeilingHeight
//
//==============================================================================
function P_CeilingHeight(const sec: Psector_t; const x, y: fixed_t): fixed_t; overload;
begin
  if sec.renderflags and SRF_SLOPECEILING <> 0 then
    result := ZatPointCeiling(sec, x, y)
  else
    result := sec.ceilingheight;
end;

//==============================================================================
//
// P_CeilingHeight
//
//==============================================================================
function P_CeilingHeight(const x, y: fixed_t): fixed_t; overload;
begin
  result := P_CeilingHeight(R_PointInSubSector(x, y).sector, x, y);
end;

type
  zvertex_t = record
    zc, zf: fixed_t;
  end;
  Pzvertex_t = ^zvertex_t;
  zvertex_tArray = array[0..$FFF] of zvertex_t;
  Pzvertex_tArray = ^zvertex_tArray;

//==============================================================================
//
// zvertex
//
//==============================================================================
function zvertex(const v: Pvertex_t; const A: Pzvertex_tArray): Pzvertex_t;
var
  id: integer;
begin
  id := pDiff(v, vertexes, SizeOf(vertex_t));
  if id < 0 then
    result := nil
  else if id >= numvertexes then
    result := nil
  else
    result := @A[id];
end;

//==============================================================================
//
// linelen
//
//==============================================================================
function linelen(const l: Pline_t): float;
var
  dx, dy: float;
begin
  dx := l.dx / FRACUNIT;
  dy := l.dy / FRACUNIT;
  result := sqrt(dx * dx + dy * dy);
end;

//==============================================================================
//
// P_SlopesAlignPlane
//
//==============================================================================
procedure P_SlopesAlignPlane(const sec: Psector_t; const line: Pline_t; const flag: LongWord;
  const calcpivotline: boolean = true);
var
  refsec: Psector_t;
  i, j: integer;
  side: integer;
  bestdist, dist: integer;
  refvert: Pvertex_t;
  refline: Pline_t;
  srcheight, destheight: fixed_t;
  v1, v2, cross: vec3_t;
  vert: Pvertex_t;
  sid: integer;
  zvertexes: Pzvertex_tArray;
  sd: Pside_t;
  start: integer;
begin
  if calcpivotline then
  begin
    {$IFDEF HEXEN}
    if line.arg1 <> 0 then
    {$ELSE}
    if line.tag <> 0 then
    {$ENDIF}
    begin
      start := -1;
      sid := P_FindSectorFromLineTag2(line, start);
      if sid < 0 then
        exit;
      refsec := @sectors[sid];
    end
    else if line.flags and ML_TWOSIDED <> 0 then
    begin
      refsec := line.backsector;
      if refsec = sec then
        refsec := line.frontsector;
      if refsec = nil then
        exit;
    end
    else
      exit;

    refvert := line.v1;

    sec.slopeline := line;
    refsec.slopesec := sec;
    if flag = SRF_SLOPEFLOOR then
    begin
      srcheight := sec.floorheight;
      destheight := refsec.floorheight;
    end
    else
    begin
      srcheight := sec.ceilingheight;
      destheight := refsec.ceilingheight;
    end;

    if srcheight = destheight then
    begin
      sec.renderflags := sec.renderflags and not SRF_SLOPED;
      for i := 0 to sec.linecount - 1 do
      begin
        refline := sec.lines[i];
        if refline.frontsector.renderflags and SRF_SLOPED = 0 then
        begin
          if refline.backsector = nil then
            refline.renderflags := refline.renderflags and not LRF_SLOPED
          else if refline.backsector.renderflags and SRF_SLOPED = 0 then // JVAL: Sos
            refline.renderflags := refline.renderflags and not LRF_SLOPED;
        end;
        exit;
      end;
    end;

    if flag = SRF_SLOPEFLOOR then
      sec.renderflags := sec.renderflags or SRF_SLOPEFLOOR
    else
      sec.renderflags := sec.renderflags or SRF_SLOPECEILING;

    bestdist := 0;
    for i := 0 to sec.linecount - 1 do
    begin
      // First vertex
      vert := sec.lines[i].v1;
      dist := abs(
        ((line.v1.y - vert.y) div FRACUNIT) * (line.dx div FRACUNIT) -
        ((line.v1.x - vert.x) div FRACUNIT) * (line.dy div FRACUNIT)
      );
      if dist > bestdist then
      begin
        bestdist := dist;
        refvert := vert;
      end;
      // Second vertex
      vert := sec.lines[i].v2;
      dist := abs(
        ((line.v1.y - vert.y) div FRACUNIT) * (line.dx div FRACUNIT) -
        ((line.v1.x - vert.x) div FRACUNIT) * (line.dy div FRACUNIT)
      );
      if dist > bestdist then
      begin
        bestdist := dist;
        refvert := vert;
      end;
    end;

    v1[0] := line.dx / FRACUNIT;
    v1[1] := line.dy / FRACUNIT;
    v1[2] := 0.0;
    v2[0] := (refvert.x - line.v1.x) / FRACUNIT;
    v2[1] := (refvert.y - line.v1.y) / FRACUNIT;
    v2[2] := (srcheight - destheight) / FRACUNIT;

    CrossProduct(@v1, @v2, @cross);
    VectorNormalize(@cross);

    if ((cross[2] < 0) and (flag = SRF_SLOPEFLOOR)) or ((cross[2] > 0) and (flag = SRF_SLOPECEILING)) then
    begin
      cross[0] := -cross[0];
      cross[1] := -cross[1];
      cross[2] := -cross[2];
    end;

    if flag = SRF_SLOPEFLOOR then
    begin
      sec.fa := cross[0];
      sec.fb := cross[1];
      sec.fic := 1.0 / cross[2];
      sec.fd := -cross[0] * (line.v1.x / FRACUNIT) -
                 cross[1] * (line.v1.y / FRACUNIT) -
                 cross[2] * (destheight / FRACUNIT);
    end
    else
    begin
      sec.ca := cross[0];
      sec.cb := cross[1];
      sec.cic := 1.0 / cross[2];
      sec.cd := -cross[0] * (line.v1.x / FRACUNIT) -
                 cross[1] * (line.v1.y / FRACUNIT) -
                 cross[2] * (destheight / FRACUNIT);
    end;
  end;

  zvertexes := mallocz(numvertexes * SizeOf(zvertex_t));

  for i := numsubsectors - 1 downto 0 do
    if subsectors[i].sector = sec then
      for j := subsectors[i].firstline + subsectors[i].numlines - 1 downto subsectors[i].firstline do
      begin
        if flag = SRF_SLOPEFLOOR then
          zvertex(segs[j].v1, zvertexes).zf := ZatPointFloor(sec, segs[j].v1.x, segs[j].v1.y)
        else
          zvertex(segs[j].v1, zvertexes).zc := ZatPointCeiling(sec, segs[j].v1.x, segs[j].v1.y);
      end;

  for i := sec.linecount - 1 downto 0 do
  begin
    refline := sec.lines[i];
    sd := @sides[refline.sidenum[0]];
    if sd.sector = sec then
      side := 0
    else
      side := 1;
    refline.renderflags := refline.renderflags or LRF_SLOPED;
    if flag = SRF_SLOPEFLOOR then
      refline.flslopestep[side] :=
        ((zvertex(refline.v1, zvertexes).zf - zvertex(refline.v2, zvertexes).zf) / FRACUNIT) / linelen(refline)
    else
      refline.clslopestep[side] :=
        ((zvertex(refline.v1, zvertexes).zc - zvertex(refline.v2, zvertexes).zc) / FRACUNIT) / linelen(refline);
  end;

  memfree(Pointer(zvertexes), SizeOf(zvertex_t));
end;

//==============================================================================
//
// P_FixSlopedMobjs
//
//==============================================================================
procedure P_FixSlopedMobjs(const s: Psector_t);
var
  mo: Pmobj_t;
  grav: fixed_t;
begin
  mo := s.thinglist;
  while mo <> nil do
  begin
    if mo.flags and MF_NOGRAVITY <> 0 then
      grav := 0
    else
      grav := FixedMul(P_GetSectorGravity(s), mo.gravity);

    mo.floorz := P_FloorHeight(s, mo.x, mo.y);
    mo.ceilingz := P_CeilingHeight(s, mo.x, mo.y);

    if mo.z - grav < mo.floorz then
      mo.z := mo.floorz
    else if mo.z + mo.height > mo.ceilingz then
      mo.z := mo.ceilingz - mo.height;

    mo := mo.snext;
  end;
end;

//==============================================================================
//
// P_DynamicSlope
//
//==============================================================================
procedure P_DynamicSlope(const sec: Psector_t);
var
 s: Psector_t;
 sl: Pline_t;
begin
  if sec.slopesec <> nil then
    s := sec.slopesec
  else
    s := sec;
  sl := s.slopeline;
  if sl <> nil then
  begin
    {$IFDEF HEXEN}
    if sl.special = 252 then
    begin
      if sl.arg2 <> 0 then
      begin
        if sl.arg4 <> 0 then
          P_SlopesAlignPlane(s, sl, SRF_SLOPEFLOOR);
        if sl.arg5 <> 0 then
          P_SlopesAlignPlane(s, sl, SRF_SLOPECEILING);
      end;
      if sl.arg3 <> 0 then
      begin
        if sl.arg4 <> 0 then
          P_SlopesAlignPlane(s, sl, SRF_SLOPEFLOOR);
        if sl.arg5 <> 0 then
          P_SlopesAlignPlane(s, sl, SRF_SLOPECEILING);
      end;
    end;
    {$ELSE}
    if (sl.special = 386) or
       (sl.special = 388) or
       (sl.special = 389) or
       (sl.special = 391) then
      P_SlopesAlignPlane(s, sl, SRF_SLOPEFLOOR);
    if (sl.special = 387) or
       (sl.special = 388) or
       (sl.special = 390) or
       (sl.special = 391) then
      P_SlopesAlignPlane(s, sl, SRF_SLOPECEILING);
    {$ENDIF}
    P_FixSlopedMobjs(s);
  end;
end;

//==============================================================================
//
// P_SlopesSetup
//
//==============================================================================
procedure P_SlopesSetup;
var
  i: integer;
begin
  for i := 0 to numlines - 1 do
  begin
    {$IFDEF HEXEN}
    if lines[i].special = 252 then
    begin
      if lines[i].arg2 <> 0 then
      begin
        if lines[i].arg4 <> 0 then
          P_SlopesAlignPlane(lines[i].frontsector, @lines[i], SRF_SLOPEFLOOR);
        if lines[i].arg5 <> 0 then
          P_SlopesAlignPlane(lines[i].frontsector, @lines[i], SRF_SLOPECEILING);
      end;
      if lines[i].arg3 <> 0 then
      begin
        if lines[i].arg4 <> 0 then
          P_SlopesAlignPlane(lines[i].backsector, @lines[i], SRF_SLOPEFLOOR);
        if lines[i].arg5 <> 0 then
          P_SlopesAlignPlane(lines[i].backsector, @lines[i], SRF_SLOPECEILING);
      end;
    end;
    {$ELSE}
    case lines[i].special of
    // JVAL: Use same specials as Eternity Engine
      386:  // The floor of the front sector is sloped to reach the height of
            // the back sector floor.
        P_SlopesAlignPlane(lines[i].frontsector, @lines[i], SRF_SLOPEFLOOR);
      387:  // The ceiling of the front sector is sloped to reach the height of
            // the back sector ceiling.
        P_SlopesAlignPlane(lines[i].frontsector, @lines[i], SRF_SLOPECEILING);
      388:  // The floor and the ceiling of the front sector are sloped to reach
            // the height of the back sector floor and ceiling respectively.
        begin
          P_SlopesAlignPlane(lines[i].frontsector, @lines[i], SRF_SLOPEFLOOR);
          P_SlopesAlignPlane(lines[i].frontsector, @lines[i], SRF_SLOPECEILING);
        end;
      389:  // The floor of the back sector is sloped to reach the height of
            // the front sector floor.
        P_SlopesAlignPlane(lines[i].backsector, @lines[i], SRF_SLOPEFLOOR);
      390:  // The ceiling of the front sector is sloped to reach the height of
            // the front sector ceiling.
        P_SlopesAlignPlane(lines[i].backsector, @lines[i], SRF_SLOPECEILING);
      391:  // The floor and the ceiling of the back sector are sloped to reach
            // the height of the front sector floor and ceiling respectively.
        begin
          P_SlopesAlignPlane(lines[i].backsector, @lines[i], SRF_SLOPEFLOOR);
          P_SlopesAlignPlane(lines[i].backsector, @lines[i], SRF_SLOPECEILING);
        end;
    end
    {$ENDIF}
  end;
end;

//==============================================================================
//
// P_FindNearestPointOnLine
//
//==============================================================================
procedure P_FindNearestPointOnLine(const x, y: fixed_t; const line: Pline_t; var xx, yy: fixed_t);
const
  NP_SCALE = 16;
var
  A, B, C, D: integer;
  dot: int64;
  len_sq: int64;
  param: int64;
  x1, x2, y1, y2: integer;
  ix, iy: integer;
begin
  x1 := line.v1.x div NP_SCALE;
  y1 := line.v1.y div NP_SCALE;
  x2 := line.v2.x div NP_SCALE;
  y2 := line.v2.y div NP_SCALE;
  ix := x div NP_SCALE;
  iy := y div NP_SCALE;

  A := ix - x1;
  B := iy - y1;
  C := x2 - x1;
  D := y2 - y1;

  dot := (A * C) + (B * D);
  len_sq := (C * C) + (D * D);
  param := -1;
  if len_sq <> 0 then
    param := (dot * NP_SCALE) div int64(len_sq);

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
    xx := x1 + (param * C) div NP_SCALE;
    yy := y1 + (param * D) div NP_SCALE;
  end;
  xx := xx * NP_SCALE;
  yy := yy * NP_SCALE;
end;

//==============================================================================
//
// P_ClosestFloorHeight
//
//==============================================================================
function P_ClosestFloorHeight(const sec: Psector_t; const line: Pline_t; const x, y: fixed_t): fixed_t;
var
  xx, yy: fixed_t;
  tmpsec: Psector_t;
begin
  if sec.renderflags and SRF_SLOPEFLOOR <> 0 then
  begin
    tmpsec := R_PointInSubSector(x, y).sector;
    if tmpsec = sec then
      result := ZatPointFloor(sec, x, y)
    else
    begin
      P_FindNearestPointOnLine(x, y, line, xx, yy);
      result := ZatPointFloor(sec, xx, yy)
    end;
  end
  else
    result := sec.floorheight;
end;

//==============================================================================
//
// P_ClosestCeilingHeight
//
//==============================================================================
function P_ClosestCeilingHeight(const sec: Psector_t; const line: Pline_t; const x, y: fixed_t): fixed_t;
var
  xx, yy: fixed_t;
  tmpsec: Psector_t;
begin
  if sec.renderflags and SRF_SLOPECEILING <> 0 then
  begin
    tmpsec := R_PointInSubSector(x, y).sector;
    if tmpsec = sec then
      result := ZatPointCeiling(sec, x, y)
    else
    begin
      P_FindNearestPointOnLine(x, y, line, xx, yy);
      result := ZatPointCeiling(sec, xx, yy)
    end;
  end
  else
    result := sec.ceilingheight;
end;

procedure calc_slope_plane(
  const x1, y1, z1: float;
  const x2, y2, z2: float;
  const x3, y3, z3: float;
  out fa, fb, fc, fd: float);
var
  a1, b1, c1: float;
  a2, b2, c2: float;
begin
  a1 := x2 - x1;
  b1 := y2 - y1;
  c1 := z2 - z1;
  a2 := x3 - x1;
  b2 := y3 - y1;
  c2 := z3 - z1;
  fa := b1 * c2 - b2 * c1;
  fb := a2 * c1 - a1 * c2;
  fc := a1 * b2 - b1 * a2;
  fd := (- fa * x1 - fb * y1 - fc * z1);
end;

//==============================================================================
//
// PS_SetFloorSlope
//
//==============================================================================
procedure PS_SetFloorSlope(const secid: integer; const x1, y1, z1: fixed_t;
  const x2, y2, z2: fixed_t; const x3, y3, z3: fixed_t);
var
  fx1, fy1, fz1: float;
  fx2, fy2, fz2: float;
  fx3, fy3, fz3: float;
  fa, fb, fc, fd: float;
  sec: Psector_t;
begin
  if (secid < 0) or (secid >= numsectors) then
    exit;

  sec := @sectors[secid];
  if (z1 = z2) and (z2 = z3) then
  begin
    sec.renderflags := sec.renderflags and not SRF_SLOPEFLOOR;
    sec.renderflags := sec.renderflags and not SRF_INTERPOLATE_FLOORSLOPE;
    sec.floorheight := z1;
    P_ChangeSector(sec, true);
  end
  else
  begin
    fx1 := x1 / FRACUNIT;
    fy1 := y1 / FRACUNIT;
    fz1 := z1 / FRACUNIT;
    fx2 := x2 / FRACUNIT;
    fy2 := y2 / FRACUNIT;
    fz2 := z2 / FRACUNIT;
    fx3 := x3 / FRACUNIT;
    fy3 := y3 / FRACUNIT;
    fz3 := z3 / FRACUNIT;
    calc_slope_plane(
      fx1, fy1, fz1,
      fx2, fy2, fz2,
      fx3, fy3, fz3,
      fa, fb, fc, fd);
    sec.fa := fa;
    sec.fb := fb;
    sec.fic := 1 / fc;
    sec.fd := fd;
    sec.renderflags := sec.renderflags or SRF_SLOPEFLOOR;
    P_SlopesAlignPlane(sec, nil, SRF_SLOPEFLOOR, false);
    sec.slopeline := sec.lines[0];
    sec.slopeline.renderflags := sec.slopeline.renderflags or LRF_SLOPED;
    sec.renderflags := sec.renderflags or SRF_INTERPOLATE_FLOORSLOPE;
    P_FixSlopedMobjs(sec);
  end;
end;

//==============================================================================
//
// PS_SetCeilingSlope
//
//==============================================================================
procedure PS_SetCeilingSlope(const secid: integer; const x1, y1, z1: fixed_t;
  const x2, y2, z2: fixed_t; const x3, y3, z3: fixed_t);
var
  fx1, fy1, fz1: float;
  fx2, fy2, fz2: float;
  fx3, fy3, fz3: float;
  ca, cb, cc, cd: float;
  sec: Psector_t;
begin
  if (secid < 0) or (secid >= numsectors) then
    exit;

  sec := @sectors[secid];
  if (z1 = z2) and (z2 = z3) then
  begin
    sec.renderflags := sec.renderflags and not SRF_SLOPECEILING;
    sec.renderflags := sec.renderflags and not SRF_INTERPOLATE_CEILINGSLOPE;
    sec.ceilingheight := z1;
    P_ChangeSector(sec, true);
  end
  else
  begin
    fx1 := x1 / FRACUNIT;
    fy1 := y1 / FRACUNIT;
    fz1 := z1 / FRACUNIT;
    fx2 := x2 / FRACUNIT;
    fy2 := y2 / FRACUNIT;
    fz2 := z2 / FRACUNIT;
    fx3 := x3 / FRACUNIT;
    fy3 := y3 / FRACUNIT;
    fz3 := z3 / FRACUNIT;
    calc_slope_plane(
      fx1, fy1, fz1,
      fx2, fy2, fz2,
      fx3, fy3, fz3,
      ca, cb, cc, cd);
    sec.ca := ca;
    sec.cb := cb;
    sec.cic := 1 / cc;
    sec.cd := cd;
    sec.renderflags := sec.renderflags or SRF_SLOPECEILING;
    P_SlopesAlignPlane(sec, nil, SRF_SLOPECEILING, false);
    sec.slopeline := sec.lines[0];
    sec.slopeline.renderflags := sec.slopeline.renderflags or LRF_SLOPED;
    sec.renderflags := sec.renderflags or SRF_INTERPOLATE_CEILINGSLOPE;
    P_FixSlopedMobjs(sec);
  end;
end;

end.

