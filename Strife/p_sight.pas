//------------------------------------------------------------------------------
//
//  DelphiStrife is a source port of the game Strife.
//
//  Based on:
//    - Linux Doom by "id Software"
//    - Chocolate Strife by "Simon Howard"
//    - DelphiDoom by "Jim Valavanis"
//
//  Copyright (C) 1993-1996 by id Software, Inc.
//  Copyright (C) 2005 Simon Howard
//  Copyright (C) 2010 James Haley, Samuel Villarreal
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
//  LineOfSight/Visibility checks, uses REJECT Lookup Table.
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit p_sight;

interface

uses
  m_fixed,
  p_mobj_h;

//==============================================================================
//
// P_CheckSight
//
//==============================================================================
function P_CheckSight(t1: Pmobj_t; t2: Pmobj_t): boolean;

//==============================================================================
//
// P_CheckSightXYZ
//
//==============================================================================
function P_CheckSightXYZ(const x, y, z: fixed_t; t2: Pmobj_t): boolean;

//==============================================================================
//
// P_CheckCameraSight
//
//==============================================================================
function P_CheckCameraSight(const camx, camy, camz: fixed_t; const mo: Pmobj_t): boolean;

//==============================================================================
//
// P_CheckVisibility
//
//==============================================================================
function P_CheckVisibility(const atx, aty, atz: fixed_t; const atradious: fixed_t): boolean;

implementation

uses
  d_delphi,
  doomdef,
  doomdata,
  g_game,
  m_bbox,
  p_local,
  p_map,
  p_setup,
  p_slopes,
  r_defs,
  r_main;

//==============================================================================
//
// P_CheckSight
//
// P_DivlineSide
// Returns side 0 (front), 1 (back), or 2 (on).
//
//==============================================================================
function P_DivlineSide(const x, y: fixed_t; const node: Pdivline_t): integer;
var
  dx: fixed_t;
  dy: fixed_t;
  left: fixed_t;
  right: fixed_t;
begin
  if node.dx = 0 then
  begin
    if x = node.x then
    begin
      result := 2;
      exit;
    end;
    if x <= node.x then
    begin
      if node.dy > 0 then
        result := 1
      else
        result := 0;
      exit;
    end;
    if node.dy < 0 then
      result := 1
    else
      result := 0;
    exit;
  end;

  if node.dy = 0 then
  begin
    // JVAL: 20220219 - Fix wrong coordinates check
    // From EE:
    // haleyjd 11/11/02: applied cph's bug fix:
    // !node->dy ? x == node->y ? 2 ...
    //             ^          ^
    // This bug compared the wrong coordinates to each other,
    // and caused line-of-sight miscalculations. Turns out the
    // P_CrossSubsector optimization demo sync problem was caused by
    // masking this bug.
    if decide(G_PlayingEngineVersion < VERSION207, x, y) = node.y then
    begin
      result := 2;
      exit;
    end;
    if y <= node.y then
    begin
      if node.dx < 0 then
        result := 1
      else
        result := 0;
      exit;
    end;
    if node.dx > 0 then
      result := 1
    else
      result := 0;
    exit;
  end;

  dx := x - node.x;
  dy := y - node.y;

  left := FixedInt(node.dy) * FixedInt(dx);
  right := FixedInt(dy) * FixedInt(node.dx);

  if right < left then
  begin
    result := 0; // front side
    exit;
  end;

  if left = right then
    result := 2
  else
    result := 1; // back side
end;

//==============================================================================
//
// P_InterceptVector2
// Returns the fractional intercept point
// along the first divline.
// This is only called by the addthings and addlines traversers.
//
//==============================================================================
function P_InterceptVector2(v2, v1: Pdivline_t): fixed_t;
var
  num: fixed_t;
  den: fixed_t;
begin
  den := FixedMul8(v1.dy, v2.dx) - FixedMul8(v1.dx, v2.dy);

  if den = 0 then
  begin
    result := 0;
    exit;
    //  I_Error ("P_InterceptVector: parallel");
  end;

  num := FixedMul8(v1.x - v2.x, v1.dy) +
         FixedMul8(v2.y - v1.y, v1.dx);

  result := FixedDiv(num , den);
end;

// JVAL: 3d Floors
type
  los_t = record
    sightzstart: fixed_t;
    t2x: fixed_t;
    t2y: fixed_t;
    strace: divline_t;
    topslope: fixed_t;
    bottomslope: fixed_t;
    bbox: array[0..3] of fixed_t;
  end;
  Plos_t = ^los_t;

//==============================================================================
//
// P_CrossSubsecPolyObj
//
// haleyjd:
// Checks a line of sight against lines belonging to the given polyobject.
//
//==============================================================================
function P_CrossSubsecPolyObj(const po: Ppolyobj_t; const los: Plos_t): boolean;
var
  i: integer;
  polySeg: PPseg_t;
  line: Pline_t;
  divl: divline_t;
  v1, v2: Pvertex_t;
begin
  polySeg := po.segs;
  for i := 0 to po.numsegs - 1 do
  begin
    if polySeg^.miniseg then // JVAL: skip minisegs
    begin
      inc(polySeg);
      continue;
    end;

    line := polySeg^.linedef;
    // already checked other side?
    if line.validcount = validcount then
    begin
      inc(polySeg);
      continue;
    end;

    line.validcount := validcount;

    // OPTIMIZE: killough 4/20/98: Added quick bounding-box rejection test
    if (line.bbox[BOXLEFT] > los.bbox[BOXRIGHT]) or
       (line.bbox[BOXRIGHT] < los.bbox[BOXLEFT]) or
       (line.bbox[BOXBOTTOM] > los.bbox[BOXTOP]) or
       (line.bbox[BOXTOP] < los.bbox[BOXBOTTOM]) then
    begin
      inc(polySeg);
      continue;
    end;

    v1 := line.v1;
    v2 := line.v2;

    // line isn't crossed?
    if P_DivlineSide(v1.x, v1.y, @los.strace) = P_DivlineSide(v2.x, v2.y, @los.strace) then
    begin
      inc(polySeg);
      continue;
    end;

    divl.x := v1.x;
    divl.y := v1.y;
    divl.dx := v2.x - v1.x;
    divl.dy := v2.y - v1.y;

    // line isn't crossed?
    if P_DivlineSide(los.strace.x, los.strace.y, @divl) = P_DivlineSide(los.t2x, los.t2y, @divl) then
    begin
      inc(polySeg);
      continue;
    end;

    // stop because it is not two sided
    result := false;
    exit;
  end;

  result := true;
end;

//==============================================================================
//
// P_CrossSubsector
// Returns true
//  if strace crosses the given subsector successfully.
//
//==============================================================================
function P_CrossSubsector(const num: integer; const los: Plos_t): boolean;
var
  seg: Pseg_t;
  line: Pline_t;
  s1: integer;
  s2: integer;
  i: integer;
  sub: Psubsector_t;
  front: Psector_t;
  back: Psector_t;
  mid: Psector_t;
  opentop: fixed_t;
  openbottom: fixed_t;
  divl: divline_t;
  v1: Pvertex_t;
  v2: Pvertex_t;
  frac: fixed_t;
  slope: fixed_t;
  front_floorheight: fixed_t;
  back_floorheight: fixed_t;
  front_ceilingheight: fixed_t;
  back_ceilingheight: fixed_t;
  vmidx: fixed_t;
  vmidy: fixed_t;
begin
  sub := @subsectors[num];

  if sub.poly <> nil then
  begin
    result := P_CrossSubsecPolyObj(sub.poly, los);
    if not result then
      exit;
  end;

  // check lines
  seg := @segs[sub.firstline - 1];
  for i := 0 to sub.numlines - 1 do
  begin
    inc(seg);
    if seg.miniseg then // JVAL: skip minisegs
      continue;

    line := seg.linedef;

    // allready checked other side?
    if line.validcount = validcount then
      continue;

    line.validcount := validcount;

    // JVAL: 3d Floors
    if G_PlayingEngineVersion >= VERSION122 then
      if (line.bbox[BOXLEFT] > los.bbox[BOXRIGHT]) or
         (line.bbox[BOXRIGHT] < los.bbox[BOXLEFT]) or
         (line.bbox[BOXBOTTOM] > los.bbox[BOXTOP]) or
         (line.bbox[BOXTOP] < los.bbox[BOXBOTTOM]) then
        continue;

    v1 := line.v1;
    v2 := line.v2;
    s1 := P_DivlineSide(v1.x, v1.y, @los.strace);
    s2 := P_DivlineSide(v2.x, v2.y, @los.strace);

    // line isn't crossed?
    if s1 = s2 then
      continue;

    divl.x := v1.x;
    divl.y := v1.y;
    divl.dx := v2.x - v1.x;
    divl.dy := v2.y - v1.y;
    s1 := P_DivlineSide(los.strace.x, los.strace.y, @divl);
    s2 := P_DivlineSide(los.t2x, los.t2y, @divl);

    // line isn't crossed?
    if s1 = s2 then
      continue;

    // stop because it is not two sided anyway
    // might do this after updating validcount?
    if line.flags and ML_TWOSIDED = 0 then
    begin
      result := false;
      exit;
    end;

    // crosses a two sided line
    back := seg.backsector;
    if back = nil then
    begin
      result := false;
      exit;
    end;

    // JVAL: 3d Floors
    if back.midsec >= 0 then
    begin
      mid := @sectors[back.midsec];
      if los.sightzstart <= mid.ceilingheight then
        if los.sightzstart >= mid.floorheight then
        begin
          result := false;
          exit;
        end;
    end;

    front := seg.frontsector;

    if G_PlayingEngineVersion >= VERSION122 then
    begin
      vmidx := (v1.x + v2.x) div 2;
      vmidy := (v1.y + v2.y) div 2;
      front_floorheight := P_FloorHeight(front, vmidx, vmidy);
      back_floorheight := P_FloorHeight(back, vmidx, vmidy);
      front_ceilingheight := P_CeilingHeight(front, vmidx, vmidy);
      back_ceilingheight := P_CeilingHeight(back, vmidx, vmidy);
    end
    else
    begin
      front_floorheight := front.floorheight;
      back_floorheight := back.floorheight;
      front_ceilingheight := front.ceilingheight;
      back_ceilingheight := back.ceilingheight;
    end;

    // no wall to block sight with?
    if (front_floorheight = back_floorheight) and
       (front_ceilingheight = back_ceilingheight) then
      continue;

    // possible occluder
    // because of ceiling height differences
    if front_ceilingheight < back_ceilingheight then
      opentop := front_ceilingheight + P_SectorJumpOverhead(front)
    else
      opentop := back_ceilingheight + P_SectorJumpOverhead(back);

    // because of ceiling height differences
    if front_floorheight > back_floorheight then
      openbottom := front_floorheight
    else
      openbottom := back_floorheight;

    // quick test for totally closed doors
    if openbottom >= opentop then
    begin
      result := false; // stop
      exit;
    end;

    frac := P_InterceptVector2(@los.strace, @divl);

    if front_floorheight <> back_floorheight then
    begin
      slope := FixedDiv(openbottom - los.sightzstart, frac);
      if slope > los.bottomslope then
        los.bottomslope := slope;
    end;

    if front_ceilingheight <> back_ceilingheight then
    begin
      slope := FixedDiv(opentop - los.sightzstart, frac);
      if slope < los.topslope then
        los.topslope := slope;
    end;

    if los.topslope <= los.bottomslope then
    begin
      result := false; // stop
      exit;
    end;
  end;

  // passed the subsector ok
  result := true;
end;

//==============================================================================
//
// P_CrossBSPNode
// Returns true
//  if strace crosses the given node successfully.
//
//==============================================================================
function P_CrossBSPNode(bspnum: integer; const los: Plos_t): boolean;
var
  bsp: Pnode_t;
  side: integer;
begin
  if bspnum and NF_SUBSECTOR_V5 <> 0 then
  begin
    if bspnum = -1 then
      result := P_CrossSubsector(0, los)
    else
      result := P_CrossSubsector(bspnum and not NF_SUBSECTOR_V5, los);
    exit;
  end;

  bsp := @nodes[bspnum];

  // decide which side the start point is on
  side := P_DivlineSide(los.strace.x, los.strace.y, Pdivline_t(bsp));
  if side = 2 then
    side := 0; // an "on" should cross both sides

  // cross the starting side
  if not P_CrossBSPNode(bsp.children[side], los) then
  begin
    result := false;
    exit;
  end;

  // the partition plane is crossed here
  if side = P_DivlineSide(los.t2x, los.t2y, Pdivline_t(bsp)) then
  begin
    // the line doesn't touch the other side
    result := true;
    exit;
  end;

  // cross the ending side
  result := P_CrossBSPNode(bsp.children[side xor 1], los);
end;

//==============================================================================
//
// P_CheckSight
// Returns true
//  if a straight line between t1 and t2 is unobstructed.
// Uses REJECT.
//
//==============================================================================
function P_CheckSight(t1: Pmobj_t; t2: Pmobj_t): boolean;
var
  s1: integer;
  s2: integer;
  hsec1: integer;
  hsec2: integer;
  pnum: integer;
  bytenum: integer;
  bitnum: integer;
  los: los_t;
  mid: Psector_t; // JVAL: 3d Floors
  midn: integer;
begin
  // First check for trivial rejection.

  // Determine subsector entries in REJECT table.
  s1 := pDiff(Psubsector_t(t1.subsector).sector, sectors, SizeOf(sector_t));
  s2 := pDiff(Psubsector_t(t2.subsector).sector, sectors, SizeOf(sector_t));

  if numsectors > 1 then
  begin
    pnum := s1 * numsectors + s2;
    bytenum := pnum div 8;
    bitnum := 1 shl (pnum and 7);

    // Check in REJECT table.
    if bytenum >= 0 then
      if bytenum < rejectmatrixsize then
        if rejectmatrix[bytenum] and bitnum <> 0 then
        begin
          // can't possibly be connected
          result := false;
          exit;
        end;

  end;

  // JVAL: 3D floors
  midn := sectors[s2].midsec;
  if midn > -1 then
  begin
    mid := @sectors[midn];
    if ((t1.z + t1.height <= mid.floorheight) and (t2.z >= mid.floorheight)) or
       ((t1.z >= mid.ceilingheight) and (t2.z + t1.height <= mid.ceilingheight)) then
    begin
      result := false;
      exit;
    end;
  end;

  hsec1 := sectors[s1].heightsec;
  if (hsec1 <> -1) and
       (((t1.z + t1.height <= sectors[hsec1].floorheight) and
         (t2.z >= sectors[hsec1].floorheight)) or
        ((t1.z >= sectors[hsec1].ceilingheight) and
         (t2.z + t1.height <= sectors[hsec1].ceilingheight))) then
  begin
    result := false;
    exit;
  end;

  hsec2 := sectors[s2].heightsec;
  if (hsec2 <> -1) and
       (((t2.z + t2.height <= sectors[hsec2].floorheight) and
         (t1.z >= sectors[hsec2].floorheight)) or
        ((t2.z >= sectors[hsec2].ceilingheight) and
         (t1.z + t2.height <= sectors[hsec2].ceilingheight))) then
  begin
    result := false;
    exit;
  end;

  // JVAL: 3D floors
  if G_PlayingEngineVersion >= VERSION122 then
  begin
    if (G_PlayingEngineVersion < VERSION204) or ((G_PlayingEngineVersion = VERSION204) and (demoplayback or demorecording)) then
    begin
      if Psubsector_t(t1.subsector).sector = Psubsector_t(t2.subsector).sector then
      begin
        result := t1.floorz = t2.floorz;
        exit;
      end;
    end
    // JVAL 20191206 - Fix problem reported by slayermbm
    // https://www.doomworld.com/forum/topic/92113-delphidoom-204720-updated-oct-12-2019/?do=findComment&comment=2051252
    else
    begin
      if midn > -1 then
        if Psubsector_t(t1.subsector).sector = Psubsector_t(t2.subsector).sector then
        begin
          result := t1.floorz = t2.floorz;
          exit;
        end;
    end;
  end;

  // An unobstructed LOS is possible.
  // Now look from eyes of t1 to any part of t2.
  inc(validcount);

  los.sightzstart := t1.z + t1.height - (t1.height div 4);
  los.topslope := (t2.z + t2.height) - los.sightzstart;
  los.bottomslope := t2.z - los.sightzstart;

  los.strace.x := t1.x;
  los.strace.y := t1.y;
  los.t2x := t2.x;
  los.t2y := t2.y;
  los.strace.dx := t2.x - t1.x;
  los.strace.dy := t2.y - t1.y;

  if t1.x > t2.x then
  begin
    los.bbox[BOXRIGHT] := t1.x;
    los.bbox[BOXLEFT] := t2.x;
  end
  else
  begin
    los.bbox[BOXRIGHT] := t2.x;
    los.bbox[BOXLEFT] := t1.x;
  end;

  if t1.y > t2.y then
  begin
    los.bbox[BOXTOP] := t1.y;
    los.bbox[BOXBOTTOM] := t2.y;
  end
  else
  begin
    los.bbox[BOXTOP] := t2.y;
    los.bbox[BOXBOTTOM] := t1.y;
  end;

  // the head node is the last node output
  result := P_CrossBSPNode(numnodes - 1, @los);
end;

//==============================================================================
//
// P_CheckCameraSight
//
// JVAL: To determine if camera chase view can see the player
//
//==============================================================================
function P_CheckCameraSight(const camx, camy, camz: fixed_t; const mo: Pmobj_t): boolean;
var
  los: los_t;
begin
  if mo = nil then
  begin
    result := false;
    exit;
  end;

  // An unobstructed LOS is possible.
  // Now look from eyes of t1 to any part of t2.
  inc(validcount);

  los.sightzstart := camz + mo.height - (mo.height div 4);
  los.topslope := (mo.z + mo.height) - los.sightzstart;
  los.bottomslope := mo.z - los.sightzstart;

  los.strace.x := camx;
  los.strace.y := camy;
  los.t2x := mo.x;
  los.t2y := mo.y;
  los.strace.dx := mo.x - camx;
  los.strace.dy := mo.y - camy;

  if camx > mo.x then
  begin
    los.bbox[BOXRIGHT] := camx;
    los.bbox[BOXLEFT] := mo.x;
  end
  else
  begin
    los.bbox[BOXRIGHT] := mo.x;
    los.bbox[BOXLEFT] := camx;
  end;

  if camy > mo.y then
  begin
    los.bbox[BOXTOP] := camy;
    los.bbox[BOXBOTTOM] := mo.y;
  end
  else
  begin
    los.bbox[BOXTOP] := mo.y;
    los.bbox[BOXBOTTOM] := camy;
  end;

  // the head node is the last node output
  result := P_CrossBSPNode(numnodes - 1, @los);
end;

//==============================================================================
//
// P_CheckSightXYZ
//
//==============================================================================
function P_CheckSightXYZ(const x, y, z: fixed_t; t2: Pmobj_t): boolean;
begin
  Result := P_CheckCameraSight(x, y, z, t2);
end;

//==============================================================================
//
// P_CheckVisibility
//
// JVAL: General visibility check
// Checks if an object at (atx, aty, atz) with radious = atradious can be
// possibly visible
//
//==============================================================================
function P_CheckVisibility(const atx, aty, atz: fixed_t; const atradious: fixed_t): boolean;
var
  los: los_t;
begin
  inc(validcount);

  los.sightzstart := viewz + atradious - (atradious div 4);
  los.topslope := (atz + atradious) - los.sightzstart;
  los.bottomslope := atz - los.sightzstart;

  los.strace.x := viewx;
  los.strace.y := viewy;
  los.t2x := atx;
  los.t2y := aty;
  los.strace.dx := atx - viewx;
  los.strace.dy := aty - viewy;

  if viewx > atx then
  begin
    los.bbox[BOXRIGHT] := viewx;
    los.bbox[BOXLEFT] := atx;
  end
  else
  begin
    los.bbox[BOXRIGHT] := atx;
    los.bbox[BOXLEFT] := viewx;
  end;

  if viewy > aty then
  begin
    los.bbox[BOXTOP] := viewy;
    los.bbox[BOXBOTTOM] := aty;
  end
  else
  begin
    los.bbox[BOXTOP] := aty;
    los.bbox[BOXBOTTOM] := viewy;
  end;

  // the head node is the last node output
  result := P_CrossBSPNode(numnodes - 1, @los);
end;

end.

