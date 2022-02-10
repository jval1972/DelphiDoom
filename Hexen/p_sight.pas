//------------------------------------------------------------------------------
//
//  DelphiHexen is a source port of the game Hexen and it is
//  based on original Linux Doom as published by "id Software", on
//  Hexen source as published by "Raven" software and DelphiDoom
//  as published by Jim Valavanis.
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
// P_SightPathTraverse
//
//==============================================================================
function P_SightPathTraverse(x1, y1, x2, y2: fixed_t): boolean;

//==============================================================================
//
// P_CheckCameraSight
//
//==============================================================================
function P_CheckCameraSight(camx, camy, camz: fixed_t; mo: Pmobj_t): boolean;

var
  bottomslope: fixed_t; // slopes to top and bottom of target
  topslope: fixed_t;
  sightzstart: fixed_t; // eye z of looker

implementation

uses
  d_delphi,
  doomdef,
  doomtype,
  g_game,
  g_demo,
  p_local,
  p_maputl,
  p_setup,
  po_man,
  r_defs,
  r_main;

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
//==============
//
// PTR_SightTraverse
//
//==============
//
//==============================================================================
function PTR_SightTraverse(_in: Pintercept_t): boolean;
var
  li: Pline_t;
  slope: fixed_t;
begin
  li := _in.d.line;

//
// crosses a two sided line
//
  P_LineOpening(li, false);

  if openbottom >= opentop then // quick test for totally closed doors
  begin
    result := false;   // stop
    exit;
  end;

  if li.frontsector.floorheight <> li.backsector.floorheight then
  begin
    slope := FixedDiv(openbottom - sightzstart, _in.frac);
    if slope > bottomslope then
      bottomslope := slope;
  end;

  if li.frontsector.ceilingheight <> li.backsector.ceilingheight then
  begin
    slope := FixedDiv(opentop - sightzstart, _in.frac);
    if slope < topslope then
      topslope := slope;
  end;

  result := topslope > bottomslope;
end;

//==============================================================================
//
//==================
//
// P_SightBlockLinesIterator
//
//===================
//
//==============================================================================
function P_SightBlockLinesIterator(x, y: integer): boolean;
var
  offset: integer;
  list: PSmallInt;
  ld: Pline_t;
  s1, s2: integer;
  dl: divline_t;
  polyLink: Ppolyblock_t;
  segList: PPseg_t;
  i: integer;
begin
  offset := y * bmapwidth + x;

  polyLink := PolyBlockMap[offset];
  while polyLink <> nil do
  begin
    if polyLink.polyobj <> nil then
    begin // only check non-empty links
      if polyLink.polyobj.validcount <> validcount then
      begin
        segList := polyLink.polyobj.segs;
        for i := 0 to polyLink.polyobj.numsegs - 1 do
        begin
          if segList^.miniseg then
          begin
            inc(segList);
            continue; // line isn't crossed
          end;
          ld := segList^.linedef;
          if ld.validcount = validcount then
          begin
            inc(segList);
            continue;
          end;

          ld.validcount := validcount;
          s1 := P_PointOnDivlineSide(ld.v1.x, ld.v1.y, @trace);
          s2 := P_PointOnDivlineSide(ld.v2.x, ld.v2.y, @trace);
          if s1 = s2 then
          begin
            inc(segList);
            continue; // line isn't crossed
          end;

          P_MakeDivline(ld, @dl);
          s1 := P_PointOnDivlineSide(trace.x, trace.y, @dl);
          s2 := P_PointOnDivlineSide(trace.x + trace.dx, trace.y + trace.dy, @dl);
          if s1 = s2 then
          begin
            inc(segList);
            continue; // line isn't crossed
          end;

        // try to early out the check
          if ld.backsector = nil then
          begin
            result := false;  // stop checking
            exit;
          end;

        // store the line for later intersection testing
          P_GrowIntercepts;
          intercepts[intercept_p].d.line := ld;
          inc(intercept_p);
          inc(segList);
        end;
        polyLink.polyobj.validcount := validcount;
      end;
    end;
    polyLink := polyLink.next;
  end;

  offset := blockmap[offset];

  list := @blockmaplump[offset];
  while list^ <> -1 do
  begin
    ld := @lines[list^];
    if ld.validcount = validcount then
    begin
      inc(list);
      continue; // line has already been checked
    end;
    ld.validcount := validcount;

    s1 := P_PointOnDivlineSide (ld.v1.x, ld.v1.y, @trace);
    s2 := P_PointOnDivlineSide (ld.v2.x, ld.v2.y, @trace);
    if s1 = s2 then
    begin
      inc(list);
      continue; // line isn't crossed
    end;
    P_MakeDivline(ld, @dl);
    s1 := P_PointOnDivlineSide (trace.x, trace.y, @dl);
    s2 := P_PointOnDivlineSide (trace.x + trace.dx, trace.y + trace.dy, @dl);
    if s1 = s2 then
    begin
      inc(list);
      continue; // line isn't crossed
    end;

  // try to early out the check
    if ld.backsector = nil then
    begin
      result := false;  // stop checking
      exit;
    end;

    P_GrowIntercepts;
    // store the line for later intersection testing
    intercepts[intercept_p].d.line := ld;
    inc(intercept_p);
    inc(list);
  end;

  result := true; // everything was checked
end;

//==============================================================================
//
//====================
//
// P_SightTraverseIntercepts
//
// Returns true if the traverser function returns true for all lines
//====================
//
//==============================================================================
function P_SightTraverseIntercepts: boolean;
var
  i: integer;
  count: integer;
  dist: fixed_t;
  scan, _in: Pintercept_t;
  dl: divline_t;
begin
  count := intercept_p;
//
// calculate intercept distance
//
  for i := 0 to count - 1 do
  begin
    scan := @intercepts[i];
    P_MakeDivline(scan.d.line, @dl);
    scan.frac := P_InterceptVector (@trace, @dl);
  end;

//
// go through in order
//
  _in := nil; // shut up compiler warning

  while count > 0 do
  begin
    dist := MAXINT;
    for i := 0 to intercept_p - 1 do
    begin
      scan := @intercepts[i];
      if scan.frac < dist then
      begin
        dist := scan.frac;
        _in := scan;
      end;
    end;

    if not PTR_SightTraverse(_in) then
    begin
      result := false;  // don't bother going farther
      exit;
    end;
    _in.frac := MAXINT;
    dec(count);
  end;

  result := true; // everything was traversed
end;

//==============================================================================
// P_SightPathTraverse32
//
//==================
//
// P_SightPathTraverse
//
// Traces a line from x1,y1 to x2,y2, calling the traverser function for each
// Returns true if the traverser function returns true for all lines
//==================
//
//==============================================================================
function P_SightPathTraverse32(x1, y1, x2, y2: fixed_t): boolean;
var
  xt1, yt1, xt2, yt2: fixed_t;
  xstep, ystep: fixed_t;
  partial: fixed_t;
  xintercept, yintercept: fixed_t;
  mapx, mapy, mapxstep, mapystep: integer;
  count: integer;
begin
  inc(validcount);
  intercept_p := 0;

  if (x1 - bmaporgx) and (MAPBLOCKSIZE - 1) = 0 then
    x1 := x1 + FRACUNIT;  // don't side exactly on a line
  if (y1 - bmaporgy) and (MAPBLOCKSIZE - 1) = 0 then
    y1 := y1 + FRACUNIT;  // don't side exactly on a line
  trace.x := x1;
  trace.y := y1;
  trace.dx := x2 - x1;
  trace.dy := y2 - y1;

  x1 := x1 - bmaporgx;
  y1 := y1 - bmaporgy;
  xt1 := MapBlockInt(x1);
  yt1 := MapBlockInt(y1);

  x2 := x2 - bmaporgx;
  y2 := y2 - bmaporgy;
  xt2 := MapBlockInt(x2);
  yt2 := MapBlockInt(y2);

// points should never be out of bounds, but check once instead of
// each block
  if (xt1 < 0) or (yt1 < 0) or (xt1 >= bmapwidth) or (yt1 >= bmapheight) or
     (xt2 < 0) or (yt2 < 0) or (xt2 >= bmapwidth) or (yt2 >= bmapheight) then
  begin
    result := false;
    exit;
  end;

  if xt2 > xt1 then
  begin
    mapxstep := 1;
    partial := FRACUNIT - MapToFrac(x1) and (FRACUNIT - 1);
    ystep := FixedDiv(y2 - y1, abs(x2 - x1));
  end
  else if xt2 < xt1 then
  begin
    mapxstep := -1;
    partial := MapToFrac(x1) and (FRACUNIT - 1);
    ystep := FixedDiv(y2 - y1, abs(x2 - x1));
  end
  else
  begin
    mapxstep := 0;
    partial := FRACUNIT;
    ystep := 256 * FRACUNIT;
  end;
  yintercept := MapToFrac(y1) + FixedMul(partial, ystep);

  if yt2 > yt1 then
  begin
    mapystep := 1;
    partial := FRACUNIT - MapToFrac(y1) and (FRACUNIT - 1);
    xstep := FixedDiv(x2 - x1, abs(y2 - y1));
  end
  else if yt2 < yt1 then
  begin
    mapystep := -1;
    partial := MapToFrac(y1) and (FRACUNIT - 1);
    xstep := FixedDiv(x2 - x1, abs(y2 - y1));
  end
  else
  begin
    mapystep := 0;
    partial := FRACUNIT;
    xstep := 256 * FRACUNIT;
  end;
  xintercept := MapToFrac(x1) + FixedMul(partial, xstep);

//
// step through map blocks
// Count is present to prevent a round off error from skipping the break
  mapx := xt1;
  mapy := yt1;

  for count := 0 to 63 do
  begin
    if not P_SightBlockLinesIterator(mapx, mapy) then
    begin
      result := false;   // early out
      exit;
    end;

    if (mapx = xt2) and (mapy = yt2) then
      break;

    if FixedInt(yintercept) = mapy then
    begin
      yintercept := yintercept + ystep;
      mapx := mapx + mapxstep;
    end
    else if FixedInt(xintercept) = mapx then
    begin
      xintercept := xintercept + xstep;
      mapy := mapy + mapystep;
    end;

  end;

//
// couldn't early out, so go through the sorted list
//
  result := P_SightTraverseIntercepts;
end;

//==============================================================================
//
// P_SightPathTraverse64
//
//==============================================================================
function P_SightPathTraverse64(x1, y1, x2, y2: fixed_t): boolean;
var
  _x1, _x2, _y1, _y2: int64;
  xt1, yt1, xt2, yt2: fixed_t;
  xstep, ystep: fixed_t;
  partial: fixed_t;
  xintercept, yintercept: fixed_t;
  mapx, mapy, mapxstep, mapystep: integer;
  count: integer;
begin
  inc(validcount);
  intercept_p := 0;

  if (x1 - bmaporgx) and (MAPBLOCKSIZE - 1) = 0 then
    x1 := x1 + FRACUNIT;  // don't side exactly on a line
  if (y1 - bmaporgy) and (MAPBLOCKSIZE - 1) = 0 then
    y1 := y1 + FRACUNIT;  // don't side exactly on a line
  trace.x := x1;
  trace.y := y1;
  trace.dx := x2 - x1;
  trace.dy := y2 - y1;

  _x1 := x1 - bmaporgx;
  _y1 := y1 - bmaporgy;
  if (_x1 < MININT) or (_x1 > MAXINT) or (_y1 < MININT) or (_y1 > MAXINT) then
  begin
    result := false;
    exit;
  end;

  x1 := _x1;
  y1 := _y1;
  xt1 := MapBlockIntX(x1);
  yt1 := MapBlockIntY(y1);

  _x2 := x2 - bmaporgx;
  _y2 := y2 - bmaporgy;
  if (_x2 < MININT) or (_x2 > MAXINT) or (_y2 < MININT) or (_y2 > MAXINT) then
  begin
    result := false;
    exit;
  end;

  x1 := _x1;
  x2 := _x2;
  xt2 := MapBlockIntX(x2);
  yt2 := MapBlockIntY(y2);

// points should never be out of bounds, but check once instead of
// each block
  if (xt1 < 0) or (yt1 < 0) or (xt1 >= bmapwidth) or (yt1 >= bmapheight) or
     (xt2 < 0) or (yt2 < 0) or (xt2 >= bmapwidth) or (yt2 >= bmapheight) then
  begin
    result := false;
    exit;
  end;

  if xt2 > xt1 then
  begin
    mapxstep := 1;
    partial := FRACUNIT - MapToFrac(x1) and (FRACUNIT - 1);
    ystep := FixedDiv(y2 - y1, abs(x2 - x1));
  end
  else if xt2 < xt1 then
  begin
    mapxstep := -1;
    partial := MapToFrac(x1) and (FRACUNIT - 1);
    ystep := FixedDiv(y2 - y1, abs(x2 - x1));
  end
  else
  begin
    mapxstep := 0;
    partial := FRACUNIT;
    ystep := 256 * FRACUNIT;
  end;
  yintercept := MapToFrac(y1) + FixedMul(partial, ystep);

  if yt2 > yt1 then
  begin
    mapystep := 1;
    partial := FRACUNIT - MapToFrac(y1) and (FRACUNIT - 1);
    xstep := FixedDiv(x2 - x1, abs(y2 - y1));
  end
  else if yt2 < yt1 then
  begin
    mapystep := -1;
    partial := MapToFrac(y1) and (FRACUNIT - 1);
    xstep := FixedDiv(x2 - x1, abs(y2 - y1));
  end
  else
  begin
    mapystep := 0;
    partial := FRACUNIT;
    xstep := 256 * FRACUNIT;
  end;
  xintercept := MapToFrac(x1) + FixedMul(partial, xstep);

//
// step through map blocks
// Count is present to prevent a round off error from skipping the break
  mapx := xt1;
  mapy := yt1;

  for count := 0 to 63 do
  begin
    if not P_SightBlockLinesIterator(mapx, mapy) then
    begin
      result := false;   // early out
      exit;
    end;

    if (mapx = xt2) and (mapy = yt2) then
      break;

    if FixedInt(yintercept) = mapy then
    begin
      yintercept := yintercept + ystep;
      mapx := mapx + mapxstep;
    end
    else if FixedInt(xintercept) = mapx then
    begin
      xintercept := xintercept + xstep;
      mapy := mapy + mapystep;
    end;

  end;

//
// couldn't early out, so go through the sorted list
//
  result := P_SightTraverseIntercepts;
end;

//==============================================================================
//
// P_SightPathTraverse
//
//==============================================================================
function P_SightPathTraverse(x1, y1, x2, y2: fixed_t): boolean;
begin
  if largemap or internalblockmapformat then
    result := P_SightPathTraverse64(x1, y1, x2, y2)
  else
    result := P_SightPathTraverse32(x1, y1, x2, y2);
end;

//==============================================================================
//
//=====================
//
// P_CheckSight
//
// Returns true if a straight line between t1 and t2 is unobstructed
// look from eyes of t1 to any part of t2
//
//=====================
//
//==============================================================================
function P_CheckSight(t1: Pmobj_t; t2: Pmobj_t): boolean;
var
  s1: integer;
  s2: integer;
  pnum: integer;
  bytenum: integer;
  bitnum: integer;
  mid: Psector_t; // JVAL: 3d Floors
  midn: integer;
begin
  // First check for trivial rejection.

  // Determine subsector entries in REJECT table.
  s1 := (integer(Psubsector_t(t1.subsector).sector) - integer(sectors)) div SizeOf(sector_t);
  s2 := (integer(Psubsector_t(t2.subsector).sector) - integer(sectors)) div SizeOf(sector_t);

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

  // JVAL: 3D floors
  if G_PlayingEngineVersion >= VERSION142 then
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

//
// check precisely
//
  sightzstart := t1.z + t1.height - _SHR2(t1.height);
  topslope := (t2.z + t2.height) - sightzstart;
  bottomslope := t2.z - sightzstart;

  result := P_SightPathTraverse(t1.x, t1.y, t2.x, t2.y);
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
// P_CheckCameraSight
//
// JVAL: Check if the camera can see mo (=player.mo)
//
//==============================================================================
function P_CheckCameraSight(camx, camy, camz: fixed_t; mo: Pmobj_t): boolean;
begin
  if mo = nil then
  begin
    result := false;
    exit;
  end;

  inc(validcount);

  sightzstart := camz + mo.height - _SHR2(mo.height);
  topslope := (mo.z + mo.height) - sightzstart;
  bottomslope := mo.z - sightzstart;

  result := P_SightPathTraverse(camx, camy, mo.x, mo.y);
end;

end.

