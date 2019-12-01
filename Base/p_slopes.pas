//------------------------------------------------------------------------------
//
//  DelphiDoom: A modified and improved DOOM engine for Windows
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 1993-1996 by id Software, Inc.
//  Copyright (C) 2004-2019 by Jim Valavanis
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
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit p_slopes;

interface

uses
  m_fixed,
  r_defs;

function P_FloorHeight(const sec: Psector_t; const x, y: fixed_t): fixed_t; overload;

function P_FloorHeight(const x, y: fixed_t): fixed_t; overload;

function P_CeilingHeight(const sec: Psector_t; const x, y: fixed_t): fixed_t; overload;

function P_CeilingHeight(const x, y: fixed_t): fixed_t; overload;

procedure P_SlopesSetup;

procedure P_DynamicSlope(const sec: Psector_t);

const
  SLOPECOUNTDOWN = 4;

implementation

uses
  d_delphi,
  Math,
  doomdata,
  m_vectors,
  p_setup,
  p_mobj_h,
  p_spec,
  r_main,
  tables;

function ZatPointFloor(const s: Psector_t; const x, y: fixed_t): fixed_t;
begin
  result := Round(((-s.fa * (x / FRACUNIT) - s.fb * (y / FRACUNIT) - s.fd) * s.fic) * FRACUNIT);
end;

function ZatPointCeiling(const s: Psector_t; const x, y: fixed_t): fixed_t;
begin
  result := Round(((-s.ca * (x / FRACUNIT) - s.cb * (y / FRACUNIT) - s.cd) * s.cic) * FRACUNIT);
end;

function P_FloorHeight(const sec: Psector_t; const x, y: fixed_t): fixed_t; overload;
begin
  if sec.renderflags and SRF_SLOPEFLOOR <> 0 then
    result := ZatPointFloor(sec, x, y)
  else
    result := sec.floorheight;
end;

function P_FloorHeight(const x, y: fixed_t): fixed_t; overload;
begin
  result := P_FloorHeight(R_PointInSubSector(x, y).sector, x, y);
end;

function P_CeilingHeight(const sec: Psector_t; const x, y: fixed_t): fixed_t; overload;
begin
  if sec.renderflags and SRF_SLOPECEILING <> 0 then
    result := ZatPointCeiling(sec, x, y)
  else
    result := sec.ceilingheight;
end;

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

function linelen(const l: Pline_t): float;
var
  dx, dy: float;
begin
  dx := l.dx / FRACUNIT;
  dy := l.dy / FRACUNIT;
  result := sqrt(dx * dx + dy * dy);
end;

procedure P_SlopesAlignPlane(const sec: Psector_t; const line: Pline_t; const flag: LongWord);
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
  v2[0] := (refvert.x - line.v1.x) / FRACUNIT;;
  v2[1] := (refvert.y - line.v1.y) / FRACUNIT;;
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

procedure P_FixSlopedMobjs(const s: Psector_t);
var
 mo: Pmobj_t;
begin
  mo := s.thinglist;
  while mo <> nil do
  begin
    mo.floorz := P_FloorHeight(s, mo.x, mo.y);
    if  mo.z < mo.floorz then
      mo.z := mo.floorz;
    mo := mo.snext;
  end;
end;

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
  end;;
end;

end.

