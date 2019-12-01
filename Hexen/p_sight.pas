//------------------------------------------------------------------------------
//
//  DelphiHexen: A modified and improved Hexen port for Windows
//  based on original Linux Doom as published by "id Software", on
//  Hexen source as published by "Raven" software and DelphiDoom
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

//
//==============================================================================
//
//              P_CheckSight
//
//This uses specialized forms of the maputils routines for optimized performance
//
//==============================================================================
//

function P_CheckSight(t1: Pmobj_t; t2: Pmobj_t): boolean;

function P_SightPathTraverse (x1, y1, x2, y2: fixed_t): boolean;

var
  bottomslope: fixed_t; // slopes to top and bottom of target
  topslope: fixed_t;
  sightzstart: fixed_t; // eye z of looker

implementation

uses
  d_delphi,
  p_local,
  p_maputl,
  p_setup,
  po_man,
  r_defs,
  r_main;

var
  sightcounts: array[0..2] of integer;

//
//==============
//
// PTR_SightTraverse
//
//==============
//

function PTR_SightTraverse(_in: Pintercept_t): boolean;
var
  li: Pline_t;
  slope: fixed_t;
begin
  li := _in.d.line;

//
// crosses a two sided line
//
  P_LineOpening(li);

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



//
//==================
//
// P_SightBlockLinesIterator
//
//===================
//

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
          {$IFDEF OPENGL}
          if segList^.miniseg then
          begin
            inc(segList);
            continue; // line isn't crossed
          end;
          {$ENDIF}
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

  // store the line for later intersection testing
    intercepts[intercept_p].d.line := ld;
    inc(intercept_p);
    inc(list);
  end;

  result := true; // everything was checked
end;

//
//====================
//
// P_SightTraverseIntercepts
//
// Returns true if the traverser function returns true for all lines
//====================
//

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



//
//==================
//
// P_SightPathTraverse
//
// Traces a line from x1,y1 to x2,y2, calling the traverser function for each
// Returns true if the traverser function returns true for all lines
//==================
//

function P_SightPathTraverse (x1, y1, x2, y2: fixed_t): boolean;
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
      inc(sightcounts[1]);
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
  inc(sightcounts[2]);

  result := P_SightTraverseIntercepts;
end;



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

function P_CheckSight(t1: Pmobj_t; t2: Pmobj_t): boolean;
var
  s1, s2: integer;
  pnum, bytenum, bitnum: integer;
begin
//
// check for trivial rejection
//
  s1 := (integer(Psubsector_t(t1.subsector).sector) - integer(sectors)) div SizeOf(sector_t);
  s2 := (integer(Psubsector_t(t2.subsector).sector) - integer(sectors)) div SizeOf(sector_t);
  pnum := s1 * numsectors + s2;
  bytenum := _SHR3(pnum);
  bitnum := _SHL(1, pnum and 7);

  if rejectmatrix[bytenum] and bitnum <> 0 then
  begin
    inc(sightcounts[0]);
    result := false;  // can't possibly be connected
    exit;
  end;

//
// check precisely
//
  sightzstart := t1.z + t1.height - _SHR2(t1.height);
  topslope := (t2.z + t2.height) - sightzstart;
  bottomslope := t2.z - sightzstart;

  result := P_SightPathTraverse(t1.x, t1.y, t2.x, t2.y);
end;

end.
