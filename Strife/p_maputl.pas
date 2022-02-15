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
//  Movement/collision utility functions,
//  as used by function in p_map.c.
//  BLOCKMAP Iterator functions,
//  and some PIT_* functions to use for iteration.
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit p_maputl;

interface

uses
  m_bbox,
  p_local,
  p_mobj_h,
  m_fixed,
  r_defs;

//==============================================================================
//
// P_AproxDistance
//
//==============================================================================
function P_AproxDistance(dx: fixed_t; dy: fixed_t): fixed_t;

//==============================================================================
//
// P_PointOnLineSide
//
//==============================================================================
function P_PointOnLineSide(x: fixed_t; y: fixed_t; line: Pline_t): integer;

//==============================================================================
//
// P_BoxOnLineSide
//
//==============================================================================
function P_BoxOnLineSide(tmbox: Pfixed_tArray; ld: Pline_t): integer;

//==============================================================================
//
// P_InterceptVector
//
//==============================================================================
function P_InterceptVector(v2: Pdivline_t; v1: Pdivline_t): fixed_t;

//==============================================================================
//
// P_LineOpening
//
//==============================================================================
procedure P_LineOpening(linedef: Pline_t; check3dfloor: boolean);

//==============================================================================
//
// P_LineOpeningTM
//
//==============================================================================
procedure P_LineOpeningTM(linedef: Pline_t; check3dfloor: boolean); // JVAL: Slopes

//==============================================================================
//
// P_LineOpeningTM206
//
//==============================================================================
procedure P_LineOpeningTM206(linedef: Pline_t; check3dfloor: boolean);  // JVAL: VERSION 206

//==============================================================================
//
// P_UnsetThingPosition
//
//==============================================================================
procedure P_UnsetThingPosition(thing: Pmobj_t);

//==============================================================================
//
// P_SetThingPosition
//
//==============================================================================
procedure P_SetThingPosition(thing: Pmobj_t);

//==============================================================================
//
// P_BlockLinesIterator
//
//==============================================================================
function P_BlockLinesIterator(x, y: integer; func: ltraverser_t): boolean;

//==============================================================================
//
// P_BlockThingsIterator
//
//==============================================================================
function P_BlockThingsIterator(x, y: integer; func: ttraverser_t): boolean;

//==============================================================================
//
// P_PathTraverse
//
//==============================================================================
function P_PathTraverse(x1, y1, x2, y2: fixed_t; flags: integer;
  trav: traverser_t): boolean;

var
  opentop: fixed_t;
  openbottom: fixed_t;
  openrange: fixed_t;
  lowfloor: fixed_t;

  trace: divline_t;

//==============================================================================
//
// P_InitIntercepts
//
//==============================================================================
procedure P_InitIntercepts;

//==============================================================================
//
// P_LineTrace
//
//==============================================================================
procedure P_LineTrace(const fromx, fromy, fromz: fixed_t; const tox, toy, toz: fixed_t; out newx, newy, newz: fixed_t);

implementation

uses
  d_delphi,
  i_system,
  p_setup,
  p_3dfloors,
  p_slopes,
  p_map,
  po_man,
  r_main,
  z_zone;

//==============================================================================
// P_AproxDistance32
//
// P_AproxDistance
// Gives an estimation of distance (not exact)
//
//==============================================================================
function P_AproxDistance32(dx: fixed_t; dy: fixed_t): fixed_t;
begin
  dx := abs(dx);
  dy := abs(dy);
  if dx < dy then
    result := dx + dy - _SHR1(dx)
  else
    result := dx + dy - _SHR1(dy);
end;

//==============================================================================
//
// P_AproxDistance64
//
//==============================================================================
function P_AproxDistance64(dx: fixed_t; dy: fixed_t): fixed_t;
var
  dx64, dy64: int64;
  dist: int64;
begin
  dx64 := abs(dx);
  dy64 := abs(dy);
  if dx64 < dy64 then
    dist := dy64 + dx64 div 2
  else
    dist := dx64 + dy64 div 2;
  if dist > MAXINT then
    result := MAXINT
  else
    result := dist;
end;

//==============================================================================
//
// P_AproxDistance
//
//==============================================================================
function P_AproxDistance(dx: fixed_t; dy: fixed_t): fixed_t;
begin
  if largemap then
    result := P_AproxDistance64(dx, dy)
  else
    result := P_AproxDistance32(dx, dy);
end;

//==============================================================================
//
// P_PointOnLineSide
// Returns 0 or 1
//
//==============================================================================
function P_PointOnLineSide(x: fixed_t; y: fixed_t; line: Pline_t): integer;
var
  dx: fixed_t;
  dy: fixed_t;
  left: fixed_t;
  right: fixed_t;
begin
  if line.dx = 0 then
  begin
    if x <= line.v1.x then
      result := intval(line.dy > 0)
    else
      result := intval(line.dy < 0);
    exit;
  end;

  if line.dy = 0 then
  begin
    if y <= line.v1.y then
      result := intval(line.dx < 0)
    else
      result := intval(line.dx > 0);
    exit;
  end;

  dx := x - line.v1.x;
  dy := y - line.v1.y;

  left := IntFixedMul(line.dy, dx);
  right := FixedIntMul(dy, line.dx);

  if right < left then
    result := 0  // front side
  else
    result := 1; // back side
end;

//==============================================================================
//
// P_BoxOnLineSide
// Considers the line to be infinite
// Returns side 0 or 1, -1 if box crosses the line.
//
//==============================================================================
function P_BoxOnLineSide(tmbox: Pfixed_tArray; ld: Pline_t): integer;
var
  p1: integer;
  p2: integer;
begin
  case ld.slopetype of
    ST_HORIZONTAL:
      begin
        p1 := intval(tmbox[BOXTOP] > ld.v1.y);
        p2 := intval(tmbox[BOXBOTTOM] > ld.v1.y);
        if ld.dx < 0 then
        begin
          p1 := p1 xor 1;
          p2 := p2 xor 1;
        end;
      end;
    ST_VERTICAL:
      begin
        p1 := intval(tmbox[BOXRIGHT] < ld.v1.x);
        p2 := intval(tmbox[BOXLEFT] < ld.v1.x);
        if ld.dy < 0 then
        begin
          p1 := p1 xor 1;
          p2 := p2 xor 1;
        end;
      end;
    ST_POSITIVE:
      begin
        p1 := P_PointOnLineSide(tmbox[BOXLEFT], tmbox[BOXTOP], ld);
        p2 := P_PointOnLineSide(tmbox[BOXRIGHT], tmbox[BOXBOTTOM], ld);
      end;
    ST_NEGATIVE:
      begin
        p1 := P_PointOnLineSide(tmbox[BOXRIGHT], tmbox[BOXTOP], ld);
        p2 := P_PointOnLineSide(tmbox[BOXLEFT], tmbox[BOXBOTTOM], ld);
      end;
  else
    begin
      p1 := 0;
      p2 := 0;
      I_Error('P_BoxOnLineSide(): wierd slopetype %d', [Ord(ld.slopetype)]);
    end;
  end;
  if p1 = p2 then
    result := p1
  else
    result := -1;
end;

//==============================================================================
// P_PointOnDivlineSide32
//
// P_PointOnDivlineSide
// Returns 0 or 1.
//
//==============================================================================
function P_PointOnDivlineSide32(x: fixed_t; y: fixed_t; line: Pdivline_t): integer;
var
  dx: fixed_t;
  dy: fixed_t;
  left: fixed_t;
  right: fixed_t;
begin
  if line.dx = 0 then
  begin
    if x <= line.x then
    begin
      if line.dy > 0 then
        result := 1
      else
        result := 0;
    end
    else
    begin
      if line.dy < 0 then
        result := 1
      else
        result := 0;
    end;
    exit;
  end;

  if line.dy = 0 then
  begin
    if y <= line.y then
    begin
      if line.dx < 0 then
        result := 1
      else
        result := 0;
    end
    else
    begin
      if line.dx > 0 then
        result := 1
      else
        result := 0;
    end;
    exit;
  end;

  dx := x - line.x;
  dy := y - line.y;

  // try to quickly decide by looking at sign bits
  if ((line.dy xor line.dx xor dx xor dy) and $80000000) <> 0 then
  begin                                                              //(left is negative)
    result := (line.dy xor dx) and $80000000;
    if result <> 0 then
      result := 1;
    exit;
  end;

  left := FixedMul88(line.dy, dx);
  right := FixedMul88(dy, line.dx);

  if right < left then
    result := 0  // front side
  else
    result := 1; // back side
end;

//==============================================================================
//
// P_PointOnDivlineSide64
//
//==============================================================================
function P_PointOnDivlineSide64(x: fixed_t; y: fixed_t; line: Pdivline_t): integer;
var
  dx64: int64;
  dy64: int64;
  left64: int64;
  right64: int64;
begin
  if line.dx = 0 then
  begin
    if x <= line.x then
    begin
      if line.dy > 0 then
        result := 1
      else
        result := 0;
    end
    else
    begin
      if line.dy < 0 then
        result := 1
      else
        result := 0;
    end;
    exit;
  end;

  if line.dy = 0 then
  begin
    if y <= line.y then
    begin
      if line.dx < 0 then
        result := 1
      else
        result := 0;
    end
    else
    begin
      if line.dx > 0 then
        result := 1
      else
        result := 0;
    end;
    exit;
  end;

  dx64 := int64(x) - int64(line.x);
  dy64 := int64(y) - int64(line.y);

  left64 := int64(line.dy) * (dx64 div 256);
  right64 := (dy64 div 256) * int64(line.dx);

  if right64 < left64 then
    result := 0  // front side
  else
    result := 1; // back side
end;

//==============================================================================
//
// P_PointOnDivlineSide
//
//==============================================================================
function P_PointOnDivlineSide(x: fixed_t; y: fixed_t; line: Pdivline_t): integer;
begin
  if largemap then
    result := P_PointOnDivlineSide64(x, y, line)
  else
    result := P_PointOnDivlineSide32(x, y, line);
end;

//==============================================================================
//
// P_MakeDivline
//
//==============================================================================
procedure P_MakeDivline(li: Pline_t; dl: Pdivline_t);
begin
  dl.x := li.v1.x;
  dl.y := li.v1.y;
  dl.dx := li.dx;
  dl.dy := li.dy;
end;

//==============================================================================
//
// P_InterceptVector
// Returns the fractional intercept point
// along the first divline.
// This is only called by the addthings
// and addlines traversers.
//
//==============================================================================
function P_InterceptVector(v2: Pdivline_t; v1: Pdivline_t): fixed_t;
var
  num: fixed_t;
  den: fixed_t;
begin
  den := FixedMul8(v1.dy, v2.dx) -
         FixedMul8(v1.dx, v2.dy);

  if den = 0 then
    result := 0     // Parallel
  else
  begin
    num := FixedMul8(v1.x - v2.x, v1.dy)  +
           FixedMul8(v2.y - v1.y, v1.dx);
    result := FixedDiv(num, den);
  end;
end;

//==============================================================================
//
// P_LineOpening
// Sets opentop and openbottom to the window
// through a two sided line.
// OPTIMIZE: keep this precalculated
//
//==============================================================================
procedure P_LineOpening(linedef: Pline_t; check3dfloor: boolean);
var
  front: Psector_t;
  back: Psector_t;
  mid: Psector_t;
  lowestceiling: fixed_t;
  highestfloor: fixed_t;
  lowestfloor: fixed_t;
  delta1, delta2, rr: fixed_t;
  thingtop: fixed_t;
begin
  if linedef.sidenum[1] = -1 then
  begin
    // single sided line
    openrange := 0;
    exit;
  end;

  front := linedef.frontsector;
  back := linedef.backsector;

  if front.ceilingheight < back.ceilingheight then
    opentop := front.ceilingheight + P_SectorJumpOverhead(front)
  else
    opentop := back.ceilingheight + P_SectorJumpOverhead(back);

  if front.floorheight > back.floorheight then
  begin
    openbottom := front.floorheight;
    lowfloor := back.floorheight;
    tmfloorpic := front.floorpic;
  end
  else
  begin
    openbottom := back.floorheight;
    lowfloor := front.floorheight;
    tmfloorpic := back.floorpic;
  end;

  // JVAL: 3d Floors
  if check3dfloor then
  begin
    lowestceiling := opentop;
    highestfloor := openbottom;
    lowestfloor := lowfloor;
    thingtop := tmthing.z + tmthing.height;
    if front.midsec >= 0 then
    begin
      mid := @sectors[front.midsec];
      rr := (mid.ceilingheight + mid.floorheight) div 2;
      delta1 := abs(tmthing.z - rr);
      delta2 := abs(thingtop - rr);
      if (mid.floorheight < lowestceiling) and (delta1 >= delta2) then
        lowestceiling := mid.floorheight;
      if (mid.ceilingheight > highestfloor) and (delta1 < delta2) then
        highestfloor := mid.ceilingheight
      else if (mid.ceilingheight > lowestfloor) and (delta1 < delta2) then
        lowestfloor := mid.ceilingheight;
    end;
    if back.midsec >= 0 then
    begin
      mid := @sectors[back.midsec];
      rr := (mid.ceilingheight + mid.floorheight) div 2;
      delta1 := abs(tmthing.z - rr);
      delta2 := abs(thingtop - rr);
      if (mid.floorheight < lowestceiling) and (delta1 >= delta2) then
        lowestceiling := mid.floorheight;
      if (mid.ceilingheight > highestfloor) and (delta1 < delta2) then
        highestfloor := mid.ceilingheight
      else if (mid.ceilingheight > lowestfloor) and (delta1 < delta2) then
        lowestfloor := mid.ceilingheight;
    end;
    if highestfloor > openbottom then
      openbottom := highestfloor;
    if lowestceiling < opentop then
      opentop := lowestceiling;
    if lowestfloor > lowfloor then
      lowfloor := lowestfloor;
  end;

  openrange := opentop - openbottom;
end;

//==============================================================================
// P_LineOpeningTM
//
// JVAL: Slopes
//
//==============================================================================
procedure P_LineOpeningTM(linedef: Pline_t; check3dfloor: boolean);
var
  front: Psector_t;
  back: Psector_t;
  mid: Psector_t;
  lowestceiling: fixed_t;
  highestfloor: fixed_t;
  lowestfloor: fixed_t;
  delta1, delta2, rr: fixed_t;
  thingtop: fixed_t;
  // JVAL: Slopes
  frontfloorheight: fixed_t;
  frontceilingheight: fixed_t;
  backfloorheight: fixed_t;
  backceilingheight: fixed_t;
  x, y: fixed_t;  // JVAL: Slopes
  moside: integer;
  picfloor3d: integer;
begin
  if linedef.sidenum[1] = -1 then
  begin
    // single sided line
    openrange := 0;
    exit;
  end;

  front := linedef.frontsector;
  back := linedef.backsector;

  // JVAL: Slopes
  moside := P_PointOnLineSide(tmthing.x, tmthing.y, linedef);

  x := tmthing.x;
  y := tmthing.y;
  if moside = 0 then
  begin
    frontfloorheight := P_FloorHeight(front, x, y);
    frontceilingheight := P_CeilingHeight(front, x, y);
    backfloorheight := P_FloorHeight(back, tmx, tmy);
    backceilingheight := P_CeilingHeight(back, tmx, tmy);
  end
  else
  begin
    frontfloorheight := P_FloorHeight(front, tmx, tmy);
    frontceilingheight := P_CeilingHeight(front, tmx, tmy);
    backfloorheight := P_FloorHeight(back, x, y);
    backceilingheight := P_CeilingHeight(back, x, y);
  end;

  if frontceilingheight < backceilingheight then
    opentop := frontceilingheight + P_SectorJumpOverhead(front)
  else
    opentop := backceilingheight + P_SectorJumpOverhead(back);

  if frontfloorheight > backfloorheight then
  begin
    openbottom := frontfloorheight;
    lowfloor := backfloorheight;
    tmfloorpic := front.floorpic;
  end
  else
  begin
    openbottom := backfloorheight;
    lowfloor := frontfloorheight;
    tmfloorpic := back.floorpic;
  end;

  // JVAL: 3d Floors
  if check3dfloor then
  begin
    lowestceiling := opentop;
    highestfloor := openbottom;
    lowestfloor := lowfloor;
    thingtop := tmthing.z + tmthing.height;
    picfloor3d := tmfloorpic;
    if front.midsec >= 0 then
    begin
      mid := @sectors[front.midsec];
      rr := (mid.ceilingheight + mid.floorheight) div 2;
      delta1 := abs(tmthing.z - rr);
      delta2 := abs(thingtop - rr);
      if (mid.floorheight < lowestceiling) and (delta1 >= delta2) then
        lowestceiling := mid.floorheight;
      if (mid.ceilingheight > highestfloor) and (delta1 < delta2) then
      begin
        highestfloor := mid.ceilingheight;
        picfloor3d := mid.ceilingpic;
      end
      else if (mid.ceilingheight > lowestfloor) and (delta1 < delta2) then
        lowestfloor := mid.ceilingheight;
    end;
    if back.midsec >= 0 then
    begin
      mid := @sectors[back.midsec];
      rr := (mid.ceilingheight + mid.floorheight) div 2;
      delta1 := abs(tmthing.z - rr);
      delta2 := abs(thingtop - rr);
      if (mid.floorheight < lowestceiling) and (delta1 >= delta2) then
        lowestceiling := mid.floorheight;
      if (mid.ceilingheight > highestfloor) and (delta1 < delta2) then
      begin
        highestfloor := mid.ceilingheight;
        picfloor3d := mid.ceilingpic;
      end
      else if (mid.ceilingheight > lowestfloor) and (delta1 < delta2) then
        lowestfloor := mid.ceilingheight;
    end;
    if highestfloor > openbottom then
    begin
      openbottom := highestfloor;
      tmfloorpic := picfloor3d;
    end;
    if lowestceiling < opentop then
      opentop := lowestceiling;
    if lowestfloor > lowfloor then
      lowfloor := lowestfloor;
  end;

  openrange := opentop - openbottom;
end;

//==============================================================================
// P_LineOpeningTM206
//
// JVAL: VERSION 206
//
//==============================================================================
procedure P_LineOpeningTM206(linedef: Pline_t; check3dfloor: boolean);
var
  front: Psector_t;
  back: Psector_t;
  mid: Psector_t;
  lowestceiling: fixed_t;
  highestfloor: fixed_t;
  lowestfloor: fixed_t;
  delta1, delta2, rr: fixed_t;
  thingtop: fixed_t;
  // JVAL: Slopes
  frontfloorheight: fixed_t;
  frontceilingheight: fixed_t;
  backfloorheight: fixed_t;
  backceilingheight: fixed_t;
  picfloor3d: integer;
begin
  if linedef.sidenum[1] = -1 then
  begin
    // single sided line
    openrange := 0;
    exit;
  end;

  front := linedef.frontsector;
  back := linedef.backsector;

  frontfloorheight := P_ClosestFloorHeight(front, linedef, tmx, tmy);
  frontceilingheight := P_ClosestCeilingHeight(front, linedef, tmx, tmy);
  backfloorheight := P_ClosestFloorHeight(back, linedef, tmx, tmy);
  backceilingheight := P_ClosestCeilingHeight(back, linedef, tmx, tmy);

  if frontceilingheight < backceilingheight then
    opentop := frontceilingheight + P_SectorJumpOverhead(front)
  else
    opentop := backceilingheight + P_SectorJumpOverhead(back);

  if frontfloorheight > backfloorheight then
  begin
    openbottom := frontfloorheight;
    lowfloor := backfloorheight;
    tmfloorpic := front.floorpic;
  end
  else
  begin
    openbottom := backfloorheight;
    lowfloor := frontfloorheight;
    tmfloorpic := back.floorpic;
  end;

  // JVAL: 3d Floors
  if check3dfloor then
  begin
    lowestceiling := opentop;
    highestfloor := openbottom;
    lowestfloor := lowfloor;
    thingtop := tmthing.z + tmthing.height;
    picfloor3d := tmfloorpic;
    if front.midsec >= 0 then
    begin
      mid := @sectors[front.midsec];
      rr := (mid.ceilingheight + mid.floorheight) div 2;
      delta1 := abs(tmthing.z - rr);
      delta2 := abs(thingtop - rr);
      if (mid.floorheight < lowestceiling) and (delta1 >= delta2) then
        lowestceiling := mid.floorheight;
      if (mid.ceilingheight > highestfloor) and (delta1 < delta2) then
      begin
        highestfloor := mid.ceilingheight;
        picfloor3d := mid.ceilingpic;
      end
      else if (mid.ceilingheight > lowestfloor) and (delta1 < delta2) then
        lowestfloor := mid.ceilingheight;
    end;
    if back.midsec >= 0 then
    begin
      mid := @sectors[back.midsec];
      rr := (mid.ceilingheight + mid.floorheight) div 2;
      delta1 := abs(tmthing.z - rr);
      delta2 := abs(thingtop - rr);
      if (mid.floorheight < lowestceiling) and (delta1 >= delta2) then
        lowestceiling := mid.floorheight;
      if (mid.ceilingheight > highestfloor) and (delta1 < delta2) then
      begin
        highestfloor := mid.ceilingheight;
        picfloor3d := mid.ceilingpic;
      end
      else if (mid.ceilingheight > lowestfloor) and (delta1 < delta2) then
        lowestfloor := mid.ceilingheight;
    end;
    if highestfloor > openbottom then
    begin
      openbottom := highestfloor;
      tmfloorpic := picfloor3d;
    end;
    if lowestceiling < opentop then
      opentop := lowestceiling;
    if lowestfloor > lowfloor then
      lowfloor := lowestfloor;
  end;

  openrange := opentop - openbottom;
end;

//==============================================================================
//
// THING POSITION SETTING
//
// P_UnsetThingPosition
// Unlinks a thing from block map and sectors.
// On each position change, BLOCKMAP and other
// lookups maintaining lists ot things inside
// these structures need to be updated.
//
//==============================================================================
procedure P_UnsetThingPosition(thing: Pmobj_t);
var
  link: Pblocklinkitem_t;
begin
  if thing.flags and MF_NOSECTOR = 0 then
  begin
    // inert things don't need to be in blockmap?
    // unlink from subsector
    if thing.snext <> nil then
      thing.snext.sprev := thing.sprev;

    if thing.sprev <> nil then
      thing.sprev.snext := thing.snext
    else
      Psubsector_t(thing.subsector).sector.thinglist := thing.snext;

    // phares 3/14/98
    //
    // Save the sector list pointed to by touching_sectorlist.
    // In P_SetThingPosition, we'll keep any nodes that represent
    // sectors the Thing still touches. We'll add new ones then, and
    // delete any nodes for sectors the Thing has vacated. Then we'll
    // put it back into touching_sectorlist. It's done this way to
    // avoid a lot of deleting/creating for nodes, when most of the
    // time you just get back what you deleted anyway.
    //
    // If this Thing is being removed entirely, then the calling
    // routine will clear out the nodes in sector_list.

    sector_list := thing.touching_sectorlist;
    thing.touching_sectorlist := nil; //to be restored by P_SetThingPosition

  end;

  if thing.flags and MF_NOBLOCKMAP = 0 then
  begin
    // inert things don't need to be in blockmap
    // unlink from block map
    if (thing.bpos >= 0) and (thing.bpos < bmapsize) then
    begin
      link := @blocklinks[thing.bpos];
      dec(link.size);
      if link.size = 0 then
      begin
        Z_Free(link.links);
        link.links := nil;
        link.realsize := 0;
      end
      else
      begin
        link.links[thing.bidx] := link.links[link.size];
        link.links[thing.bidx].bidx := thing.bidx;
      end;
      thing.bpos := -1;
      thing.bidx := -1;
    end;
  end;
end;

//==============================================================================
//
// P_SetThingPosition
// Links a thing into both a block and a subsector
// based on it's x y.
// Sets thing->subsector properly
//
//==============================================================================
procedure P_SetThingPosition(thing: Pmobj_t);
var
  ss: Psubsector_t;
  sec: Psector_t;
  blockx: integer;
  blocky: integer;
  link: Pblocklinkitem_t;
begin
  // link into subsector
  ss := R_PointInSubsector(thing.x, thing.y);
  thing.subsector := ss;

  if thing.flags and MF_NOSECTOR = 0 then
  begin
    // invisible things don't go into the sector links
    sec := ss.sector;

    thing.sprev := nil;
    thing.snext := sec.thinglist;

    if sec.thinglist <> nil then
      sec.thinglist.sprev := thing;

    sec.thinglist := thing;

    // phares 3/16/98
    //
    // If sector_list isn't NULL, it has a collection of sector
    // nodes that were just removed from this Thing.

    // Collect the sectors the object will live in by looking at
    // the existing sector_list and adding new nodes and deleting
    // obsolete ones.

    // When a node is deleted, its sector links (the links starting
    // at sector_t->touching_thinglist) are broken. When a node is
    // added, new sector links are created.

    P_CreateSecNodeList(thing, thing.x, thing.y);
    thing.touching_sectorlist := sector_list; // Attach to Thing's mobj_t
    sector_list := nil; // clear for next time

  end;

  // link into blockmap
  if thing.flags and MF_NOBLOCKMAP = 0 then
  begin
    // inert things don't need to be in blockmap
    if internalblockmapformat then
    begin
      blockx := MapBlockIntX(int64(thing.x) - int64(bmaporgx));
      blocky := MapBlockIntY(int64(thing.y) - int64(bmaporgy));
    end
    else
    begin
      blockx := MapBlockInt(thing.x - bmaporgx);
      blocky := MapBlockInt(thing.y - bmaporgy);
    end;
    if (blockx >= 0) and (blockx < bmapwidth) and
       (blocky >= 0) and (blocky < bmapheight) then
    begin
      thing.bpos := blocky * bmapwidth + blockx;
      link := @blocklinks[thing.bpos];
      if link.size >= link.realsize then
      begin
        link.links := Z_ReAlloc(link.links, SizeOf(Pmobj_t) * (link.size + 8), PU_LEVEL, nil);
        link.realsize := link.size + 8;
      end;
      link.links[link.size] := thing;
      thing.bidx := link.size;
      inc(link.size);
    end
    else
    begin
      thing.bpos := -1;
      thing.bidx := -1;
    end;
  end
  else
  begin
    thing.bpos := -1;
    thing.bidx := -1;
  end;
end;

//==============================================================================
//
// BLOCK MAP ITERATORS
// For each line/thing in the given mapblock,
// call the passed PIT_* function.
// If the function returns false,
// exit with false without checking anything else.
//
// P_BlockLinesIterator
// The validcount flags are used to avoid checking lines
// that are marked in multiple mapblocks,
// so increment validcount before the first call
// to P_BlockLinesIterator, then make one or more calls
// to it.
//
// haleyjd 20110203:
// [STRIFE] Modified to track blockingline
//
//==============================================================================
function P_BlockLinesIterator(x, y: integer; func: ltraverser_t): boolean;
var
  offset: PInteger;
  ld: Pline_t;
  polyLink: Ppolyblock_t;
  tempSeg: PPseg_t;
  i: integer;
begin
  if (x < 0) or (y < 0) or (x >= bmapwidth) or (y >= bmapheight) then
  begin
    result := true;
    exit;
  end;

  if PolyBlockMap <> nil then
  begin
    polyLink := PolyBlockMap[y * bmapwidth + x];
    while polyLink <> nil do
    begin
      if polyLink.polyobj <> nil then
      begin
        if polyLink.polyobj.validcount <> validcount then
        begin
          polyLink.polyobj.validcount := validcount;
          tempSeg := polyLink.polyobj.segs;
          for i := 0 to polyLink.polyobj.numsegs - 1 do
          begin
            if not tempSeg^.miniseg then
            if tempSeg^.linedef.validcount <> validcount then
            begin
              tempSeg^.linedef.validcount := validcount;
              if not func(tempSeg^.linedef) then
              begin
                result := false;
                exit;
              end;
            end;
            inc(tempSeg);
          end;
        end;
      end;
      polyLink := polyLink.next;
    end;
  end;

  offset := @blockmaplump[blockmap[y * bmapwidth + x]];

  while offset^ <> - 1 do
  begin
    ld := @lines[offset^];
    inc(offset);

    // [STRIFE]: set blockingline (see P_XYMovement @ p_mobj.c)
    blockingline := ld;

    if ld.validcount = validcount then
      continue; // line has already been checked

    ld.validcount := validcount;

    if not func(ld) then
    begin
      result := false;
      exit;
    end;
  end;

  result := true; // everything was checked
end;

//==============================================================================
//
// P_BlockThingsIterator
//
//==============================================================================
function P_BlockThingsIterator(x, y: integer; func: ttraverser_t): boolean;
var
  i: integer;
  link: Pblocklinkitem_t;
begin
  if (x < 0) or (y < 0) or (x >= bmapwidth) or (y >= bmapheight) then
  begin
    result := true;
    exit;
  end;

  link := @blocklinks[y * bmapwidth + x];
  for i := 0 to link.size - 1 do
    if not func(link.links[i]) then
    begin
      result := false;
      exit;
    end;

  result := true;
end;

//
// INTERCEPT ROUTINES
//
var
  intercepts: Pintercept_tArray;
  intercept_p: integer;
  numintercepts: integer = 0;

  earlyout: boolean;

//==============================================================================
//
// P_InitIntercepts
//
//==============================================================================
procedure P_InitIntercepts;
begin
  intercepts := Z_Malloc(MAXINTERCEPTS * SizeOf(intercept_t), PU_LEVEL, nil);
  numintercepts := MAXINTERCEPTS;
end;

//==============================================================================
//
// P_GrowIntercepts
//
//==============================================================================
procedure P_GrowIntercepts;
begin
  if intercept_p >= numintercepts then
  begin
    numintercepts := numintercepts + 64;
    intercepts := Z_ReAlloc(intercepts, numintercepts * SizeOf(intercept_t), PU_LEVEL, nil);
  end;
end;

//==============================================================================
//
// PIT_AddLineIntercepts.
// Looks for lines in the given block
// that intercept the given trace
// to add to the intercepts list.
//
// A line is crossed if its endpoints
// are on opposite sides of the trace.
// Returns true if earlyout and a solid line hit.
//
//==============================================================================
function PIT_AddLineIntercepts(ld: Pline_t): boolean;
var
  s1: integer;
  s2: integer;
  frac: fixed_t;
  dl: divline_t;
  pinrc: Pintercept_t;
begin
  // avoid precision problems with two routines
  if (trace.dx > FRACUNIT * 16) or (trace.dy > FRACUNIT * 16) or
     (trace.dx < -FRACUNIT * 16) or (trace.dy < -FRACUNIT * 16) then
  begin
    s1 := P_PointOnDivlineSide(ld.v1.x, ld.v1.y, @trace);
    s2 := P_PointOnDivlineSide(ld.v2.x, ld.v2.y, @trace);
  end
  else
  begin
    s1 := P_PointOnLineSide(trace.x, trace.y, ld);
    s2 := P_PointOnLineSide(trace.x + trace.dx, trace.y + trace.dy, ld);
  end;

  if s1 = s2 then
  begin
    result := true; // line isn't crossed
    exit;
  end;

  // hit the line
  P_MakeDivline(ld, @dl);
  frac := P_InterceptVector(@trace, @dl);

  if frac < 0 then
  begin
    result := true; // behind source
    exit;
  end;

  // try to early out the check
  if earlyout and (frac < FRACUNIT) and (ld.backsector = nil) then
  begin
    result := false; // stop checking
    exit;
  end;

  P_GrowIntercepts;
  pinrc := @intercepts[intercept_p];
  pinrc.frac := frac;
  pinrc.isaline := true;
  pinrc.d.line := ld;
  inc(intercept_p);

  result := true; // continue
end;

//==============================================================================
//
// PIT_AddThingIntercepts
//
//==============================================================================
function PIT_AddThingIntercepts(thing: Pmobj_t): boolean;
var
  x1: fixed_t;
  y1: fixed_t;
  x2: fixed_t;
  y2: fixed_t;
  s1: integer;
  s2: integer;
  tracepositive: boolean;
  dl: divline_t;
  frac: fixed_t;
  pinrc: Pintercept_t;
  r: integer;
begin
  tracepositive := (trace.dx xor trace.dy) > 0;

  // check a corner to corner crossection for hit
  r := thing.radius;
  if tracepositive then
  begin
    x1 := thing.x - r;
    y1 := thing.y + r;
    x2 := thing.x + r;
    y2 := thing.y - r;
  end
  else
  begin
    x1 := thing.x - r;
    y1 := thing.y - r;
    x2 := thing.x + r;
    y2 := thing.y + r;
  end;

  s1 := P_PointOnDivlineSide(x1, y1, @trace);
  s2 := P_PointOnDivlineSide(x2, y2, @trace);

  if s1 = s2 then
  begin
    result := true; // line isn't crossed
    exit;
  end;

  dl.x := x1;
  dl.y := y1;
  dl.dx := x2 - x1;
  dl.dy := y2 - y1;

  frac := P_InterceptVector(@trace, @dl);

  if frac < 0 then
  begin
    result := true; // behind source
    exit;
  end;

  P_GrowIntercepts;
  pinrc := @intercepts[intercept_p];
  pinrc.frac := frac;
  pinrc.isaline := false;
  pinrc.d.thing := thing;
  inc(intercept_p);

  result := true; // keep going
end;

//==============================================================================
//
// P_TraverseIntercepts
// Returns true if the traverser function returns true
// for all lines.
//
//==============================================================================
function P_TraverseIntercepts(func: traverser_t; maxfrac: fixed_t): boolean;
var
  i: integer;
  dist: fixed_t;
  scan: integer;
  _in: Pintercept_t;
begin
  _in := nil; // shut up compiler warning

  for i := 0 to intercept_p - 1 do
  begin
    dist := MAXINT;

    for scan := 0 to intercept_p - 1 do
    begin
      if intercepts[scan].frac < dist then
      begin
        dist := intercepts[scan].frac;
        _in := @intercepts[scan];
      end;
    end;

    if dist > maxfrac then
    begin
      result := true; // checked everything in range
      exit;
    end;

    if not func(_in) then
    begin
      result := false; // don't bother going farther
      exit;
    end;
    _in.frac := MAXINT;
  end;

  result := true; // everything was traversed
end;

//==============================================================================
// P_PathTraverse32
//
// P_PathTraverse
// Traces a line from x1,y1 to x2,y2,
// calling the traverser function for each.
// Returns true if the traverser function returns true
// for all lines.
//
//==============================================================================
function P_PathTraverse32(x1, y1, x2, y2: fixed_t; flags: integer;
  trav: traverser_t): boolean;
var
  xt1: fixed_t;
  yt1: fixed_t;
  xt2: fixed_t;
  yt2: fixed_t;
  xstep: fixed_t;
  ystep: fixed_t;
  partial: fixed_t;
  xintercept: fixed_t;
  yintercept: fixed_t;
  mapx: integer;
  mapy: integer;
  mapxstep: integer;
  mapystep: integer;
  count: integer;
begin
  earlyout := flags and PT_EARLYOUT <> 0;

  inc(validcount);
  intercept_p := 0;

  if (x1 - bmaporgx) and (MAPBLOCKSIZE - 1) = 0 then
    x1 := x1 + FRACUNIT; // don't side exactly on a line

  if (y1 - bmaporgy) and (MAPBLOCKSIZE - 1) = 0 then
    y1 := y1 + FRACUNIT; // don't side exactly on a line

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

  if xt2 > xt1 then
  begin
    mapxstep := 1;
    partial := FRACUNIT - (MapToFrac(x1) and (FRACUNIT - 1));
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
    partial := FRACUNIT - (MapToFrac(y1) and (FRACUNIT - 1));
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

  // Step through map blocks.
  // Count is present to prevent a round off error
  // from skipping the break.
  mapx := xt1;
  mapy := yt1;

  for count := 0 to 63 do
  begin
    if flags and PT_ADDLINES <> 0 then
    begin
      if not P_BlockLinesIterator(mapx, mapy, PIT_AddLineIntercepts) then
      begin
        result := false; // early out
        exit;
      end;
    end;

    if flags and PT_ADDTHINGS <> 0 then
    begin
      if not P_BlockThingsIterator(mapx, mapy, PIT_AddThingIntercepts) then
      begin
        result := false;// early out
        exit;
      end;
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

  // go through the sorted list
  result := P_TraverseIntercepts(trav, FRACUNIT);
end;

//==============================================================================
//
// P_PathTraverse64
//
//==============================================================================
function P_PathTraverse64(x1, y1, x2, y2: fixed_t; flags: integer;
  trav: traverser_t): boolean;
var
  _x1, _x2, _y1, _y2: int64;
  xt1: fixed_t;
  yt1: fixed_t;
  xt2: fixed_t;
  yt2: fixed_t;
  xstep: fixed_t;
  ystep: fixed_t;
  partial: fixed_t;
  xintercept: fixed_t;
  yintercept: fixed_t;
  mapx: integer;
  mapx1: integer;
  mapy: integer;
  mapy1: integer;
  mapxstep: integer;
  mapystep: integer;
  count: integer;
begin
  earlyout := flags and PT_EARLYOUT <> 0;

  inc(validcount);
  intercept_p := 0;

  if (x1 - bmaporgx) and (MAPBLOCKSIZE - 1) = 0 then
    x1 := x1 + FRACUNIT; // don't side exactly on a line

  if (y1 - bmaporgy) and (MAPBLOCKSIZE - 1) = 0 then
    y1 := y1 + FRACUNIT; // don't side exactly on a line

  trace.x := x1;
  trace.y := y1;
  trace.dx := x2 - x1;
  trace.dy := y2 - y1;

  _x1 := int64(x1) - bmaporgx;
  _y1 := int64(y1) - bmaporgy;
  xt1 := _x1 shr MAPBLOCKSHIFT;
  yt1 := _y1 shr MAPBLOCKSHIFT;

  mapx1 := _x1 shr MAPBTOFRAC;
  mapy1 := _y1 shr MAPBTOFRAC;

  _x2 := int64(x2) - bmaporgx;
  _y2 := int64(y2) - bmaporgy;
  xt2 := _x2 shr MAPBLOCKSHIFT;
  yt2 := _y2 shr MAPBLOCKSHIFT;

  x1 := x1 - bmaporgx;
  y1 := y1 - bmaporgy;
  x2 := x2 - bmaporgx;
  y2 := y2 - bmaporgy;

  if xt2 > xt1 then
  begin
    mapxstep := 1;
    partial := FRACUNIT - (mapx1 and (FRACUNIT - 1));
    ystep := FixedDiv(y2 - y1, abs(x2 - x1));
  end
  else if xt2 < xt1 then
  begin
    mapxstep := -1;
    partial := mapx1 and (FRACUNIT - 1);
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
    partial := FRACUNIT - (mapy1 and (FRACUNIT - 1));
    xstep := FixedDiv(x2 - x1, abs(y2 - y1));
  end
  else if yt2 < yt1 then
  begin
    mapystep := -1;
    partial := mapy1 and (FRACUNIT - 1);
    xstep := FixedDiv(x2 - x1, abs(y2 - y1));
  end
  else
  begin
    mapystep := 0;
    partial := FRACUNIT;
    xstep := 256 * FRACUNIT;
  end;

  xintercept := MapToFrac(x1) + FixedMul(partial, xstep);

  // Step through map blocks.
  // Count is present to prevent a round off error
  // from skipping the break.
  mapx := xt1;
  mapy := yt1;

  for count := 0 to 63 do
  begin
    if flags and PT_ADDLINES <> 0 then
    begin
      if not P_BlockLinesIterator(mapx, mapy, PIT_AddLineIntercepts) then
      begin
        result := false; // early out
        exit;
      end;
    end;

    if flags and PT_ADDTHINGS <> 0 then
    begin
      if not P_BlockThingsIterator(mapx, mapy, PIT_AddThingIntercepts) then
      begin
        result := false;// early out
        exit;
      end;
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

  // go through the sorted list
  result := P_TraverseIntercepts(trav, FRACUNIT);
end;

//==============================================================================
//
// P_PathTraverse
//
//==============================================================================
function P_PathTraverse(x1, y1, x2, y2: fixed_t; flags: integer;
  trav: traverser_t): boolean;
begin
  if largemap or internalblockmapformat then
    result := P_PathTraverse64(x1, y1, x2, y2, flags, trav)
  else
    result := P_PathTraverse32(x1, y1, x2, y2, flags, trav)
end;

var
  LTfromx, LTfromy, LTfromz: fixed_t;
  LTtox, LTtoy, LTtoz: fixed_t;
  LTbbox: array[0..3] of fixed_t;
  LTline: Pline_t;

//==============================================================================
//
// PIT_LineTrace
//
//==============================================================================
function PIT_LineTrace(ld: Pline_t): boolean;
var
  A1, B1, C1: int64;
  A2, B2, C2: int64;
  det: int64;
  x, y: int64;
  dist1, dist2: int64;
begin
  if ld.backsector <> nil then
  begin
    result := true;
    exit;
  end;

  if (LTbbox[BOXRIGHT] <= ld.bbox[BOXLEFT]) or
     (LTbbox[BOXLEFT] >= ld.bbox[BOXRIGHT]) or
     (LTbbox[BOXTOP] <= ld.bbox[BOXBOTTOM]) or
     (LTbbox[BOXBOTTOM] >= ld.bbox[BOXTOP]) then
  begin
    result := true;
    exit;
  end;

  if P_BoxOnLineSide(@LTbbox, ld) <> -1 then
  begin
    result := true;
    exit;
  end;

  A1 := LTtoy - LTfromy;
  B1 := LTfromx - LTtox;
  C1 := (A1 * LTfromx) div FRACUNIT + (B1 * LTfromy) div FRACUNIT;

  A2 := ld.v2.y - ld.v1.y;
  B2 := ld.v1.x - ld.v2.x;
  C2 := (A2 * ld.v1.x) div FRACUNIT + (B2 * ld.v1.y) div FRACUNIT;

  det := (A1 * B2) div FRACUNIT - (A2 * B1) div FRACUNIT;
  if det <> 0 then
  begin
    x := (B2 * C1 - B1 * C2) div det;
    y := (A1 * C2 - A2 * C1) div det;
    dist1 := ((LTfromx - x) div FRACUNIT) * (LTfromx - x) + ((LTfromy - y) div FRACUNIT) * (LTfromy - y);
    dist2 := ((LTfromx - LTtox) div FRACUNIT) * (LTfromx - LTtox) + ((LTfromy - LTtoy) div FRACUNIT) * (LTfromy - LTtoy);
    if dist1 < dist2 then
    begin
      LTtox := x;
      LTtoy := y;
      LTline := ld;
    end;
  end;

  result := true;
end;

//==============================================================================
//
// P_LineTrace
//
//==============================================================================
procedure P_LineTrace(const fromx, fromy, fromz: fixed_t; const tox, toy, toz: fixed_t; out newx, newy, newz: fixed_t);
var
  xl: integer;
  xh: integer;
  yl: integer;
  yh: integer;
  bx: integer;
  by: integer;
  floor, ceiling: fixed_t;
begin
  LTbbox[BOXLEFT] := MinI(fromx, tox);
  LTbbox[BOXRIGHT] := MaxI(fromx, tox);
  LTbbox[BOXBOTTOM] := MinI(fromy, toy);
  LTbbox[BOXTOP] := MaxI(fromy, toy);

  xl := MapBlockIntX(int64(LTbbox[BOXLEFT]) - int64(bmaporgx) - MAXRADIUS);
  xh := MapBlockIntX(int64(LTbbox[BOXRIGHT]) - int64(bmaporgx) + MAXRADIUS);
  yl := MapBlockIntY(int64(LTbbox[BOXBOTTOM]) - int64(bmaporgy) - MAXRADIUS);
  yh := MapBlockIntY(int64(LTbbox[BOXTOP]) - int64(bmaporgy) + MAXRADIUS);

  LTfromx := fromx;
  LTfromy := fromy;
  LTfromz := fromz;

  LTtox := tox;
  LTtoy := toy;
  LTtoz := toz;

  LTline := nil;
  for bx := xl to xh do
    for by := yl to yh do
      P_BlockLinesIterator(bx, by, PIT_LineTrace);

  newx := LTtox;
  newy := LTtoy;
  if LTline = nil then
  begin
    floor := P_3dFloorHeight(newx, newy, LTfromz);
    ceiling := P_3dCeilingHeight(newx, newy, LTfromz);
    newz := GetIntegerInRange(LTtoz, floor, ceiling);
  end
  else
  begin
    floor := P_3dFloorHeight(LTline.frontsector, newx, newy, LTfromz);
    ceiling := P_3dCeilingHeight(LTline.frontsector, newx, newy, LTfromz);
    newz := GetIntegerInRange(LTtoz, floor, ceiling);
  end;
end;

end.

