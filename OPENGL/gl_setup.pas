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
//  OpenGL specific map loading
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit gl_setup;

interface

uses
  d_delphi;

//==============================================================================
//
// P_GLLoadVertexes
//
//==============================================================================
procedure P_GLLoadVertexes(lump, gllump: integer);

//==============================================================================
//
// GetDistance
//
//==============================================================================
function GetDistance(dx, dy: integer): float;

//==============================================================================
//
// P_LoadGLSegs
//
//==============================================================================
procedure P_LoadGLSegs(lump: integer);

//==============================================================================
//
// P_LoadSubsectorsV3V5
//
//==============================================================================
procedure P_LoadSubsectorsV3V5(lump: integer);

//==============================================================================
//
// P_LoadNodesV4V5
//
//==============================================================================
procedure P_LoadNodesV4V5(lump: integer);

var
  glmapnum: integer;

implementation

uses
  doomdata,
  i_system,
  m_fixed,
  nd_main,
  p_setup,
  r_defs,
  r_main,
  w_wad,
  z_zone;

var
  firstglvert: integer;

//==============================================================================
//
// P_GLLoadVertexes
//
//==============================================================================
procedure P_GLLoadVertexes(lump, gllump: integer);
var
  data: pointer;
  i: integer;
  ml: Pmapvertex_t;
  li: Pvertex_t;
  numglverts: integer;
  minx: integer;
  maxx: integer;
  miny: integer;
  maxy: integer;
  dx, dy: integer;
begin
  // Determine number of lumps:
  //  total lump length / vertex record length.
  if glnodesver = 1 then
    numglverts := W_LumpLength(gllump) div SizeOf(GLVertex1_t)
  else if glnodesver >= 2 then
    numglverts := (W_LumpLength(gllump) - 4) div SizeOf(GLVertex2_t)
  else
    numglverts := 0;

  firstglvert := W_LumpLength(lump) div SizeOf(mapvertex_t);
  numvertexes := firstglvert + numglverts;

  // Allocate zone memory for buffer.
  vertexes := Z_Malloc(numvertexes * SizeOf(vertex_t), PU_LEVEL, nil);

  // Load data into cache.
  data := W_CacheLumpNum(lump, PU_STATIC);

  ml := Pmapvertex_t(data);

  // JVAL: 20201228 -> Find map boundaries
  minx := 100000;
  maxx := -100000;
  miny := 100000;
  maxy := -100000;

  // Copy and convert vertex coordinates,
  // internal representation as fixed.
  li := @vertexes[0];
  for i := 0 to firstglvert - 1 do
  begin
    if ml.x > maxx then
      maxx := ml.x;
    if ml.x < minx then
      minx := ml.x;
    if ml.y > maxy then
      maxy := ml.y;
    if ml.y < miny then
      miny := ml.y;
    li.x := ml.x * FRACUNIT;
    li.y := ml.y * FRACUNIT;
    li.amvalidcount := 0;
    inc(ml);
    inc(li);
  end;

  dx := maxx - minx;
  dy := maxy - miny;

  largemap := (dx < -32767) or (dx > 32767) or (dy < -32767) or (dy > 32767);

  // Free buffer memory.
  Z_Free(data);

  gld_GetGLVertexes(li, gllump, numglverts, glnodesver);
end;

//==============================================================================
//
// GetDistance
//
//==============================================================================
function GetDistance(dx, dy: integer): float;
var
  fx, fy: float;
begin
  fx := dx / FRACUNIT;
  fy := dy / FRACUNIT;
  result := sqrt(fx * fx + fy * fy);
end;

//==============================================================================
//
// CheckGLVertex
//
//==============================================================================
function CheckGLVertex(num: integer): integer;
begin
  if glnodesver <= 3 then
  begin
    if num and (1 shl 15) <> 0 then
    begin
      result := num and (1 shl 15 - 1) + firstglvert;
      exit;
    end
  end
  else if glnodesver = 4 then
  begin
    if num and (1 shl 30) <> 0 then
    begin
      result := num and (1 shl 30 - 1) + firstglvert;
      exit;
    end
  end
  else if glnodesver = 5 then
  begin
    if num and (1 shl 31) <> 0 then
    begin
      result := LongWord(num) and LongWord(_SHLW(1, 31) - 1) + firstglvert;
      exit;
    end
  end;
  result := num;
end;

//==============================================================================
//
// GetOffset
//
//==============================================================================
function GetOffset(v1, v2: Pvertex_t): fixed_t;
var
  a, b: single;
begin
  a := (v1.x - v2.x) / FRACUNIT;
  b := (v1.y - v2.y) / FRACUNIT;
  result := round(sqrt(a * a + b * b) * FRACUNIT);
end;

//==============================================================================
//
// P_LoadGLSegs
//
//==============================================================================
procedure P_LoadGLSegs(lump: integer);
var
  data: pointer;
  i: integer;
  ml: PGLSeg1_t;
  ml3: PGLSeg3_t;
  li: Pseg_t;
  ldef: Pline_t;
  linedef: integer;
  side: integer;
  sidenum: integer;
begin
  // JVAL glbsp V5
  if glnodesver = 3 then
    numsegs := (W_LumpLength(lump) - 4) div SizeOf(GLSeg3_t)
  else if glnodesver > 3 then
    numsegs := W_LumpLength(lump) div SizeOf(GLSeg3_t)
  else
    numsegs := W_LumpLength(lump) div SizeOf(GLSeg1_t);
  segs := Z_Malloc(numsegs * SizeOf(seg_t), PU_LEVEL, nil);
  ZeroMemory(segs, numsegs * SizeOf(seg_t));
  data := W_CacheLumpNum(lump, PU_STATIC);

  if glnodesver < 3 then
  begin
    ml := PGLSeg1_t(data);
    li := @segs[0];
    for i := 0 to numsegs - 1 do
    begin
      li.v1 := @vertexes[CheckGLVertex(ml.start_vertex)];
      li.v2 := @vertexes[CheckGLVertex(ml.end_vertex)];
      li.iSegID := i;

      if PWord(@ml.linedef)^ = word(1 shl 16 - 1) then
      begin
        li.miniseg := true;
        li.angle := 0;
        li.offset := 0;
        li.length := 0;
        li.linedef := nil;
        li.sidedef := nil;
        li.frontsector := nil;
        li.backsector := nil;
      end
      else
      begin
        li.miniseg := false;
        li.angle := R_PointToAngle2(li.v1.x, li.v1.y, li.v2.x, li.v2.y);
        linedef := ml.linedef;
        ldef := @lines[linedef];
        if ml.side <> 0 then
          li.offset := GetOffset(li.v1, ldef.v2)
        else
          li.offset := GetOffset(li.v1, ldef.v1);
        li.linedef := ldef;
        side := ml.side;
        li.sidedef := @sides[ldef.sidenum[side]];
        li.frontsector := li.sidedef.sector;
        if ldef.flags and ML_TWOSIDED <> 0 then
        begin
          sidenum := ldef.sidenum[side xor 1];
          if sidenum = -1 then
          begin
            I_Warning('P_LoadGLSegs(): Line %d is marked with ML_TWOSIDED flag without backsector'#13#10, [linedef]);
            ldef.flags := ldef.flags and not ML_TWOSIDED;
            li.backsector := nil;
          end
          else
            li.backsector := sides[sidenum].sector;
        end
        else
          li.backsector := nil;
        li.length := GetDistance(li.v2.x - li.v1.x, li.v2.y - li.v1.y);
      end;
      inc(ml);
      inc(li);
    end;
  end
  else
  begin
    if glnodesver = 3 then
      ml3 := PGLSeg3_t(Integer(data) + 4)
    else
      ml3 := PGLSeg3_t(data);
    li := @segs[0];
    for i := 0 to numsegs - 1 do
    begin
      li.v1 := @vertexes[CheckGLVertex(ml3.start_vertex)];
      li.v2 := @vertexes[CheckGLVertex(ml3.end_vertex)];
      li.iSegID := i;

      if PWord(@ml3.linedef)^ = word(1 shl 16 - 1) then
      begin
        li.miniseg := true;
        li.angle := 0;
        li.offset := 0;
        li.length := 0;
        li.linedef := nil;
        li.sidedef := nil;
        li.frontsector := nil;
        li.backsector := nil;
      end
      else
      begin
        li.miniseg := false;
        li.angle := R_PointToAngle2(li.v1.x, li.v1.y, li.v2.x, li.v2.y);
        linedef := ml3.linedef;
        ldef := @lines[linedef];
        if ml3.side <> 0 then
          li.offset := GetOffset(li.v1, ldef.v2)
        else
          li.offset := GetOffset(li.v1, ldef.v1);
        li.linedef := ldef;
        side := ml3.side;
        li.sidedef := @sides[ldef.sidenum[side]];
        li.frontsector := li.sidedef.sector;
        if ldef.flags and ML_TWOSIDED <> 0 then
        begin
          sidenum := ldef.sidenum[side xor 1];
          if sidenum = -1 then
          begin
            I_Warning('P_LoadGLSegs(): Line %d is marked with ML_TWOSIDED flag without backsector'#13#10, [linedef]);
            ldef.flags := ldef.flags and not ML_TWOSIDED;
            li.backsector := nil;
          end
          else
            li.backsector := sides[sidenum].sector;
        end
        else
          li.backsector := nil;
        li.length := GetDistance(li.v2.x - li.v1.x, li.v2.y - li.v1.y);
      end;
      inc(ml3);
      inc(li);
    end;
  end;

  Z_Free(data);
end;

//==============================================================================
//
// P_LoadSubsectorsV3V5
//
//==============================================================================
procedure P_LoadSubsectorsV3V5(lump: integer);
var
  data: pointer;
  i: integer;
  ms: PGLSubSector3_t;
  ss: Psubsector_t;
begin
  if glnodesver = 3 then
    numsubsectors := (W_LumpLength(lump) - 4) div SizeOf(GLSubSector3_t)
  else
    numsubsectors := W_LumpLength(lump) div SizeOf(GLSubSector3_t);
  subsectors := Z_Malloc(numsubsectors * SizeOf(subsector_t), PU_LEVEL, nil);
  data := W_CacheLumpNum(lump, PU_STATIC);

  if glnodesver = 3 then
    ms := PGLSubSector3_t(integer(data) + 4)
  else
    ms := PGLSubSector3_t(data);
  ZeroMemory(subsectors, numsubsectors * SizeOf(subsector_t));

  ss := @subsectors[0];
  for i := 0 to numsubsectors - 1 do
  begin
    ss.numlines := ms.count;
    ss.firstline := ms.first_seg;
    inc(ms);
    inc(ss);
  end;

  Z_Free(data);
end;

//==============================================================================
//
// P_LoadNodesV4V5
//
//==============================================================================
procedure P_LoadNodesV4V5(lump: integer);
var
  data: pointer;
  i: integer;
  k: integer;
  mn: PGLNode4_t;
  no: Pnode_t;
begin
  numnodes := W_LumpLength(lump) div SizeOf(GLNode4_t);
  nodes := Z_Malloc(numnodes * SizeOf(node_t), PU_LEVEL, nil);
  data := W_CacheLumpNum(lump, PU_STATIC);

  mn := PGLNode4_t(data);
  no := @nodes[0];
  for i := 0 to numnodes - 1 do
  begin
    no.x := mn.x * FRACUNIT;
    no.y := mn.y * FRACUNIT;
    no.dx := mn.dx * FRACUNIT;
    no.dy := mn.dy * FRACUNIT;

    no.children[0] := mn.right_child;
    no.children[1] := mn.left_child;
    for k := 0 to 3 do
      no.bbox[0, k] := mn.right_bbox[k] * FRACUNIT;
    for k := 0 to 3 do
      no.bbox[1, k] := mn.left_bbox[k] * FRACUNIT;

    inc(mn);
    inc(no);
  end;

  Z_Free (data);
end;

end.
