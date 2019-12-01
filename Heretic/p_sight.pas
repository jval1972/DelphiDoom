//------------------------------------------------------------------------------
//
//  DelphiHeretic: A modified and improved Heretic port for Windows
//  based on original Linux Doom as published by "id Software", on
//  Heretic source as published by "Raven" software and DelphiDoom
//  as published by Jim Valavanis.
//  Copyright (C) 2004-2013 by Jim Valavanis
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
//  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
//  02111-1307, USA.
//
// DESCRIPTION:
//  LineOfSight/Visibility checks, uses REJECT Lookup Table.
//
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit p_sight;

interface

uses
  m_fixed,
  p_mobj_h;

function P_CheckSight(t1: Pmobj_t; t2: Pmobj_t): boolean;

function P_CheckCameraSight(camx, camy, camz: fixed_t; mo: Pmobj_t): boolean;

function P_CheckVisibility(const atx, aty, atz: fixed_t; const atradious: fixed_t): boolean;

var
  bottomslope: fixed_t; // slopes to top and bottom of target
  topslope: fixed_t;
  sightzstart: fixed_t; // eye z of looker

implementation

uses
  d_delphi,
  doomdef, doomdata,
  i_system,
  p_local, p_setup, p_map,
  r_defs, r_main;

//
// P_CheckSight
//
var
  strace: divline_t; // from t1 to t2
  t2x: fixed_t;
  t2y: fixed_t;

  sightcounts: array[0..1] of integer;

//
// P_DivlineSide
// Returns side 0 (front), 1 (back), or 2 (on).
//
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
    if x = node.y then
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

  dx := (x - node.x);
  dy := (y - node.y);

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

//
// P_InterceptVector2
// Returns the fractional intercept point
// along the first divline.
// This is only called by the addthings and addlines traversers.
//
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

//
// P_CrossSubsector
// Returns true
//  if strace crosses the given subsector successfully.
//
function P_CrossSubsector(const num: integer): boolean;
var
  seg: Pseg_t;
  line: Pline_t;
  s1: integer;
  s2: integer;
  i: integer;
  sub: Psubsector_t;
  front: Psector_t;
  back: Psector_t;
  opentop: fixed_t;
  openbottom: fixed_t;
  divl: divline_t;
  v1: Pvertex_t;
  v2: Pvertex_t;
  frac: fixed_t;
  slope: fixed_t;
begin
  sub := @subsectors[num];

  // check lines
  seg := @segs[sub.firstline - 1];
  for i := 0 to sub.numlines - 1 do
  begin
    inc(seg);
    {$IFDEF OPENGL}
    if seg.miniseg then // JVAL: skip minisegs
      continue;
    {$ENDIF}

    line := seg.linedef;

    // allready checked other side?
    if line.validcount = validcount then
      continue;

    line.validcount := validcount;

    v1 := line.v1;
    v2 := line.v2;
    s1 := P_DivlineSide(v1.x, v1.y, @strace);
    s2 := P_DivlineSide(v2.x, v2.y, @strace);

    // line isn't crossed?
    if s1 = s2 then
      continue;

    divl.x := v1.x;
    divl.y := v1.y;
    divl.dx := v2.x - v1.x;
    divl.dy := v2.y - v1.y;
    s1 := P_DivlineSide(strace.x, strace.y, @divl);
    s2 := P_DivlineSide(t2x, t2y, @divl);

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
    front := seg.frontsector;
    back := seg.backsector;

    // no wall to block sight with?
    if (front.floorheight = back.floorheight) and
       (front.ceilingheight = back.ceilingheight) then
      continue;

    // possible occluder
    // because of ceiling height differences
    if front.ceilingheight < back.ceilingheight then
      opentop := front.ceilingheight + P_SectorJumpOverhead(front, nil)
    else
      opentop := back.ceilingheight + P_SectorJumpOverhead(back, nil);

    // because of ceiling height differences
    if front.floorheight > back.floorheight then
      openbottom := front.floorheight
    else
      openbottom := back.floorheight;

    // quick test for totally closed doors
    if openbottom >= opentop then
    begin
      result := false; // stop
      exit;
    end;

    frac := P_InterceptVector2(@strace, @divl);

    if front.floorheight <> back.floorheight then
    begin
      slope := FixedDiv(openbottom - sightzstart, frac);
      if slope > bottomslope then
        bottomslope := slope;
    end;

    if front.ceilingheight <> back.ceilingheight then
    begin
      slope := FixedDiv(opentop - sightzstart, frac);
      if slope < topslope then
        topslope := slope;
    end;

    if topslope <= bottomslope then
    begin
      result := false; // stop
      exit;
    end;
  end;

  // passed the subsector ok
  result := true;
end;

//
// P_CrossBSPNode
// Returns true
//  if strace crosses the given node successfully.
//
function P_CrossBSPNode(bspnum: integer): boolean;
var
  bsp: Pnode_t;
  side: integer;
begin
  if bspnum and NF_SUBSECTOR <> 0 then
  begin
    if bspnum = -1 then
      result := P_CrossSubsector(0)
    else
      result := P_CrossSubsector(bspnum and (not NF_SUBSECTOR));
    exit;
  end;

  bsp := @nodes[bspnum];

  // decide which side the start point is on
  side := P_DivlineSide(strace.x, strace.y, Pdivline_t(bsp));
  if side = 2 then
    side := 0; // an "on" should cross both sides

  // cross the starting side
  if not P_CrossBSPNode(bsp.children[side]) then
  begin
    result := false;
    exit;
  end;

  // the partition plane is crossed here
  if side = P_DivlineSide(t2x, t2y, Pdivline_t(bsp)) then
  begin
    // the line doesn't touch the other side
    result := true;
    exit;
  end;

  // cross the ending side
  result := P_CrossBSPNode(bsp.children[side xor 1]);
end;

//
// P_CheckSight
// Returns true
//  if a straight line between t1 and t2 is unobstructed.
// Uses REJECT.
//
function P_CheckSight(t1: Pmobj_t; t2: Pmobj_t): boolean;
var
  s1: integer;
  s2: integer;
  pnum: integer;
  bytenum: integer;
  bitnum: integer;
begin
  // First check for trivial rejection.

  // Determine subsector entries in REJECT table.
  s1 := pDiff(Psubsector_t(t1.subsector).sector, @sectors[0], SizeOf(sectors[0]));
  s2 := pDiff(Psubsector_t(t2.subsector).sector, @sectors[0], SizeOf(sectors[0]));
  pnum := s1 * numsectors + s2;
  bytenum := _SHR(pnum, 3);
  bitnum := 1 shl (pnum and 7);

  // Check in REJECT table.
  if rejectmatrix[bytenum] and bitnum <> 0 then
  begin
    sightcounts[0] := sightcounts[0] + 1;

    // can't possibly be connected
    result := false;
    exit;
  end;

  // An unobstructed LOS is possible.
  // Now look from eyes of t1 to any part of t2.
  sightcounts[1] := sightcounts[1] + 1;

  inc(validcount);

  sightzstart := t1.z + t1.height - _SHR(t1.height, 2);
  topslope := (t2.z + t2.height) - sightzstart;
  bottomslope := t2.z - sightzstart;

  strace.x := t1.x;
  strace.y := t1.y;
  t2x := t2.x;
  t2y := t2.y;
  strace.dx := t2.x - t1.x;
  strace.dy := t2.y - t1.y;

  // the head node is the last node output
  result := P_CrossBSPNode(numnodes - 1);
end;

//
// P_CheckCameraSight
//
// JVAL: To determine if camera chase view can see the player
//
function P_CheckCameraSight(camx, camy, camz: fixed_t; mo: Pmobj_t): boolean;
begin
  if mo = nil then
  begin
    result := false;
    exit;
  end;

  
  // An unobstructed LOS is possible.
  // Now look from eyes of t1 to any part of t2.
  sightcounts[1] := sightcounts[1] + 1;

  inc(validcount);

  sightzstart := camz + mo.height - _SHR2(mo.height);
  topslope := (mo.z + mo.height) - sightzstart;
  bottomslope := mo.z - sightzstart;

  strace.x := camx;
  strace.y := camy;
  t2x := mo.x;
  t2y := mo.y;
  strace.dx := mo.x - camx;
  strace.dy := mo.y - camy;

  // the head node is the last node output
  result := P_CrossBSPNode(numnodes - 1);
end;

//
// P_CheckVisibility
//
// JVAL: General visibility check
// Checks if an object at (atx, aty, atz) with radious = atradious can be
// possibly visible 
//
function P_CheckVisibility(const atx, aty, atz: fixed_t; const atradious: fixed_t): boolean;
begin
  inc(validcount);

  sightzstart := viewz + atradious div 2;
  topslope := (atz + atradious) - sightzstart;
  bottomslope := atz - sightzstart;

  strace.x := viewx;
  strace.y := viewy;
  t2x := atx;
  t2y := aty;
  strace.dx := atx - viewx;
  strace.dy := aty - viewy;

  // the head node is the last node output
  result := P_CrossBSPNode(numnodes - 1);
end;


end.


