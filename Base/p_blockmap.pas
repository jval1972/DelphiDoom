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
//  DESCRIPTION:
//   Blockmap loading & slime trails
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit p_blockmap;

interface

//==============================================================================
//
// P_LoadBlockMap
//
//==============================================================================
procedure P_LoadBlockMap(lump: integer);

//==============================================================================
//
// P_RemoveSlimeTrails
//
//==============================================================================
procedure P_RemoveSlimeTrails;  // killough 10/98

implementation

uses
  d_delphi,
  doomtype,
  m_argv,
  m_fixed,
  p_setup,
  r_defs,
  w_wad,
  z_zone;

//
// jff 10/6/98
// New code added to speed up calculation of internal blockmap
// Algorithm is order of nlines*(ncols+nrows) not nlines*ncols*nrows
//
const
  blkshift = 7;                   // places to shift rel position for cell num
  blkmask = (1 shl blkshift) - 1; // mask for rel position within cell
  blkmargin = 0;                  // size guardband around map used */
                                  // jff 10/8/98 use guardband>0
                                  // jff 10/12/98 0 ok with + 1 in rows,cols

type
  Plinelist_t = ^linelist_t;
  linelist_t = record // type used to list lines in each block
    num: integer;
    next: Plinelist_t;
  end;
  linelist_tPArray = array[0..$FFFF] of Plinelist_t;
  Plinelist_tPArray = ^linelist_tPArray;

//==============================================================================
// AddBlockLine
//
// Subroutine to add a line number to a block list
// It simply returns if the line is already in the block
//
//==============================================================================
procedure AddBlockLine(lists: Plinelist_tPArray; count: PIntegerArray; done: PIntegerArray;
  blockno: integer; lineno: integer);
var
  l: Plinelist_t;
begin
  if done[blockno] <> 0 then
    exit;

  l := malloc(SizeOf(linelist_t));
  l.num := lineno;
  l.next := lists[blockno];
  lists[blockno] := l;
  inc(count[blockno]);
  done[blockno] := 1;
end;

//==============================================================================
// P_CreateBlockMap
//
// Actually construct the blockmap lump from the level data
//
// This finds the intersection of each linedef with the column and
// row lines at the left and bottom of each blockmap cell. It then
// adds the line to all block lists touching the intersection.
//
//==============================================================================
procedure P_CreateBlockMap;
var
  xorg, yorg: integer;            // blockmap origin (lower left)
  nrows, ncols: integer;          // blockmap dimensions
  blocklists: Plinelist_tPArray;  // array of pointers to lists of lines
  blockcount: PIntegerArray;      // array of counters of line lists
  blockdone: PIntegerArray;       // array keeping track of blocks/line
  NBlocks: integer;               // number of cells := nrows*ncols
  linetotal: integer;             // total length of all blocklists
  i, j: integer;
  map_minx: integer;              // init for map limits search
  map_miny: integer;
  map_maxx: integer;
  map_maxy: integer;
  t: fixed_t;
  x1, y1, x2, y2: integer;
  dx, dy: integer;
  vert: boolean;
  horiz: boolean;
  spos: boolean;
  sneg: boolean;
  bx, by: integer;
  minx, maxx, miny, maxy: integer;
  x, y: integer;
  xb, xp: integer;
  yb, yp: integer;
  bl, tmp: Plinelist_t;
  offs: integer;
begin
  blocklists := nil;
  blockcount := nil;
  blockdone := nil;
  linetotal := 0;
  map_minx := MAXINT;
  map_miny := MAXINT;
  map_maxx := MININT;
  map_maxy := MININT;

  // scan for map limits, which the blockmap must enclose

  for i := 0 to numvertexes - 1 do
  begin
    t := vertexes[i].x;
    if t < map_minx then
      map_minx := t;
    if t > map_maxx then
      map_maxx := t;
    t := vertexes[i].y;
    if t < map_miny then
      map_miny := t;
    if t > map_maxy then
      map_maxy := t;
  end;
  map_minx := FixedInt(map_minx);    // work in map coords, not fixed_t
  map_maxx := FixedInt(map_maxx);
  map_miny := FixedInt(map_miny);
  map_maxy := FixedInt(map_maxy);

  // set up blockmap area to enclose level plus margin

  xorg := map_minx - blkmargin;
  yorg := map_miny - blkmargin;
  ncols := _SHR(map_maxx + blkmargin - xorg + 1 + blkmask, blkshift);  //jff 10/12/98
  nrows := _SHR(map_maxy + blkmargin - yorg + 1 + blkmask, blkshift);  //+1 needed for
  NBlocks := ncols * nrows;                                  //map exactly 1 cell

  // create the array of pointers on NBlocks to blocklists
  // also create an array of linelist counts on NBlocks
  // finally make an array in which we can mark blocks done per line

  // CPhipps - calloc's
  blocklists := mallocz(NBlocks * SizeOf(Plinelist_t));
  blockcount := mallocz(NBlocks * SizeOf(integer));
  blockdone := mallocz(NBlocks * SizeOf(integer));

  // initialize each blocklist, and enter the trailing -1 in all blocklists
  // note the linked list of lines grows backwards

  for i := 0 to NBlocks - 1 do
  begin
    blocklists[i] := malloc(SizeOf(linelist_t));
    blocklists[i].num := -1;
    blocklists[i].next := nil;
    inc(blockcount[i]);
  end;

  // For each linedef in the wad, determine all blockmap blocks it touches,
  // and add the linedef number to the blocklists for those blocks

  for i := 0 to numlines - 1 do
  begin
    x1 := FixedInt(lines[i].v1.x);         // lines[i] map coords
    y1 := FixedInt(lines[i].v1.y);
    x2 := FixedInt(lines[i].v2.x);
    y2 := FixedInt(lines[i].v2.y);
    dx := x2 - x1;
    dy := y2 - y1;
    vert := dx = 0;                            // lines[i] slopetype
    horiz := dy = 0;
    spos := (dx xor dy) > 0;
    sneg := (dx xor dy) < 0;
    if x1 > x2 then
    begin
      minx := x2;
      maxx := x1;
    end
    else
    begin
      minx := x1;
      maxx := x2;
    end;
    if y1 > y2 then
    begin
      miny := y2;
      maxy := y1;
    end
    else
    begin
      miny := y1;
      maxy := y2;
    end;

    // no blocks done for this linedef yet

    ZeroMemory(blockdone, NBlocks * SizeOf(integer));

    // The line always belongs to the blocks containing its endpoints

    bx := _SHR(x1 - xorg, blkshift);
    by := _SHR(y1 - yorg, blkshift);
    AddBlockLine(blocklists, blockcount, blockdone, by * ncols + bx, i);
    bx := _SHR(x2 - xorg, blkshift);
    by := _SHR(y2 - yorg, blkshift);
    AddBlockLine(blocklists, blockcount, blockdone, by * ncols + bx, i);

    // For each column, see where the line along its left edge, which
    // it contains, intersects the Linedef i. Add i to each corresponding
    // blocklist.

    if not vert then    // don't interesect vertical lines with columns
    begin
      for j := 0 to ncols - 1 do
      begin
        // intersection of Linedef with x=xorg+(j shl blkshift)
        // (y-y1)*dx := dy*(x-x1)
        // y := dy*(x-x1)+y1*dx;

        x := xorg + _SHL(j, blkshift);         // (x,y) is intersection
        y := (dy * (x - x1)) div dx + y1;
        yb := _SHR(y - yorg, blkshift);        // block row number
        yp := (y - yorg) and blkmask;         // y position within block

        if (yb < 0) or (yb > nrows - 1) then  // outside blockmap, continue
          continue;

        if (x < minx) or (x > maxx) then      // line doesn't touch column
          continue;

        // The cell that contains the intersection point is always added

        AddBlockLine(blocklists, blockcount, blockdone, ncols * yb + j, i);

        // if the intersection is at a corner it depends on the slope
        // (and whether the line extends past the intersection) which
        // blocks are hit

        if yp = 0 then        // intersection at a corner
        begin
          if sneg then        //   \ - blocks x,y-, x-,y
          begin
            if (yb > 0) and (miny < y) then
              AddBlockLine(blocklists, blockcount, blockdone, ncols * (yb - 1) + j, i);
            if (j > 0) and (minx < x) then
              AddBlockLine(blocklists, blockcount, blockdone, ncols * yb + j - 1, i);
          end
          else if spos then  //   / - block x-,y-
          begin
            if (yb > 0) and (j > 0) and (minx < x) then
              AddBlockLine(blocklists, blockcount, blockdone, ncols * (yb - 1) + j - 1, i);
          end
          else if horiz then //   - - block x-,y
          begin
            if (j > 0) and (minx < x) then
              AddBlockLine(blocklists, blockcount, blockdone, ncols * yb + j - 1, i);
          end;
        end
        else if (j > 0) and (minx < x) then // else not at corner: x-,y
          AddBlockLine(blocklists, blockcount, blockdone, ncols * yb + j - 1, i);
      end;
    end;

    // For each row, see where the line along its bottom edge, which
    // it contains, intersects the Linedef i. Add i to all the corresponding
    // blocklists.

    if not horiz then
    begin
      for j := 0 to nrows - 1 do
      begin
        // intersection of Linedef with y=yorg+(j shl blkshift)
        // (x,y) on Linedef i satisfies: (y-y1)*dx := dy*(x-x1)
        // x := dx*(y-y1)/dy+x1;

        y := yorg + _SHL(j, blkshift);         // (x,y) is intersection
        x := (dx * (y - y1)) div dy + x1;
        xb := _SHR(x - xorg, blkshift);        // block column number
        xp := (x - xorg) and blkmask;         // x position within block

        if (xb < 0) or (xb > ncols - 1) then  // outside blockmap, continue
          continue;

        if (y < miny) or (y > maxy) then      // line doesn't touch row
          continue;

        // The cell that contains the intersection point is always added

        AddBlockLine(blocklists, blockcount, blockdone, ncols * j + xb, i);

        // if the intersection is at a corner it depends on the slope
        // (and whether the line extends past the intersection) which
        // blocks are hit

        if xp = 0 then        // intersection at a corner
        begin
          if sneg then       //   \ - blocks x,y-, x-,y
          begin
            if (j > 0) and (miny < y) then
              AddBlockLine(blocklists, blockcount, blockdone, ncols * (j - 1) + xb, i);
            if (xb > 0) and (minx < x) then
              AddBlockLine(blocklists, blockcount, blockdone, ncols * j + xb - 1, i);
          end
          else if vert then  //   | - block x,y-
          begin
            if (j > 0) and (miny < y) then
              AddBlockLine(blocklists, blockcount, blockdone, ncols * (j - 1) + xb, i);
          end
          else if spos then  //   / - block x-,y-
          begin
            if (xb > 0) and (j > 0) and (miny < y) then
              AddBlockLine(blocklists, blockcount, blockdone, ncols * (j - 1) + xb - 1, i);
          end;
        end
        else if (j > 0) and (miny < y) then // else not on a corner: x,y-
          AddBlockLine(blocklists, blockcount, blockdone, ncols * (j - 1) + xb, i);
      end;
    end;
  end;

  // Add initial 0 to all blocklists
  // count the total number of lines (and 0's and -1's)

  ZeroMemory(blockdone, NBlocks * SizeOf(integer));
  linetotal := 0;
  for i := 0 to NBlocks - 1 do
  begin
    AddBlockLine(blocklists, blockcount, blockdone, i, 0);
    linetotal := linetotal + blockcount[i];
  end;

  // Create the blockmap lump

  blockmaplump := Z_Malloc(SizeOf(integer) * (4 + NBlocks + linetotal), PU_LEVEL, nil);
  // blockmap header

  blockmaplump[0] := xorg;
  blockmaplump[1] := yorg;
  blockmaplump[2] := ncols;
  blockmaplump[3] := nrows;

  // offsets to lists and block lists

  for i := 0 to NBlocks - 1 do
  begin
    bl := blocklists[i];
    if i <> 0 then
      offs := blockmaplump[4 + i - 1] + blockcount[i - 1]
    else
      offs := 4 + NBlocks;
    blockmaplump[4 + i] := offs; // set offset to block's list

    // add the lines in each block's list to the blockmaplump
    // delete each list node as we go

    while bl <> nil do
    begin
      tmp := bl.next;
      blockmaplump[offs] := bl.num;
      inc(offs);
      memfree(pointer(bl), SizeOf(linelist_t));
      bl := tmp;
    end;
  end;

  // MAES: set blockmapxneg and blockmapyneg
  // E.g. for a full 512x512 map, they should be both
  // -1. For a 257*257, they should be both -255 etc.
  if bmapwidth > 255 then
    blockmapxneg := bmapwidth - 512
  else
    blockmapxneg := -257;
  if bmapheight > 255 then
    blockmapyneg := bmapheight - 512
  else
    blockmapyneg := -257;

  internalblockmapformat := true;

  // free all temporary storage

  memfree(pointer(blocklists), NBlocks * SizeOf(Plinelist_t));
  memfree(pointer(blockcount), NBlocks * SizeOf(integer));
  memfree(pointer(blockdone), NBlocks * SizeOf(integer));
end;

//==============================================================================
//
// P_LoadBlockMap
//
//==============================================================================
procedure P_LoadBlockMap(lump: integer);
var
  i, count: integer;
  t: smallint;
  wadblockmaplump: PSmallIntArray;
begin
  blockmapxneg := -257;
  blockmapyneg := -257;
  internalblockmapformat := false;
  count := W_LumpLength(lump) div 2; // Number of smallint values
  if (M_CheckParm('-blockmap') > 0) or (count < 4) or (count >= $10000) or largemap then
  begin
    P_CreateBlockMap
  end
  else
  begin
    wadblockmaplump := W_CacheLumpNum(lump, PU_STATIC);
    blockmaplump := Z_Malloc(count * SizeOf(integer), PU_LEVEL, nil);

    blockmaplump[0] := wadblockmaplump[0];
    blockmaplump[1] := wadblockmaplump[1];
    blockmaplump[2] := wadblockmaplump[2];
    blockmaplump[3] := wadblockmaplump[3];

    for i := 4 to count - 1 do
    begin
      t := wadblockmaplump[i];
      if t = -1 then
        blockmaplump[i] := -1
      else if t < 0 then
        blockmaplump[i] := $10000 + t
      else
        blockmaplump[i] := t
    end;
    Z_Free(wadblockmaplump);
  end;

  blockmap := @blockmaplump[4];

  bmaporgx := blockmaplump[0] * FRACUNIT;
  bmaporgy := blockmaplump[1] * FRACUNIT;
  bmapwidth := blockmaplump[2];
  bmapheight := blockmaplump[3];
  bmapsize := bmapwidth * bmapheight;
  // clear out mobj chains
  count := SizeOf(blocklinkitem_t) * bmapwidth * bmapheight;
  blocklinks := Z_Malloc(count, PU_LEVEL, nil);
  ZeroMemory(blocklinks, count);
end;

//==============================================================================
// P_RemoveSlimeTrails
//
// killough 10/98
//
// Remove slime trails.
//
// Slime trails are inherent to Doom's coordinate system -- i.e. there is
// nothing that a node builder can do to prevent slime trails ALL of the time,
// because it's a product of the integer coodinate system, and just because
// two lines pass through exact integer coordinates, doesn't necessarily mean
// that they will intersect at integer coordinates. Thus we must allow for
// fractional coordinates if we are to be able to split segs with node lines,
// as a node builder must do when creating a BSP tree.
//
// A wad file does not allow fractional coordinates, so node builders are out
// of luck except that they can try to limit the number of splits (they might
// also be able to detect the degree of roundoff error and try to avoid splits
// with a high degree of roundoff error). But we can use fractional coordinates
// here, inside the engine. It's like the difference between square inches and
// square miles, in terms of granularity.
//
// For each vertex of every seg, check to see whether it's also a vertex of
// the linedef associated with the seg (i.e, it's an endpoint). If it's not
// an endpoint, and it wasn't already moved, move the vertex towards the
// linedef by projecting it using the law of cosines. Formula:
//
//      2        2                         2        2
//    dx  x0 + dy  x1 + dx dy (y0 - y1)  dy  y0 + dx  y1 + dx dy (x0 - x1)
//   {---------------------------------, ---------------------------------}
//                  2     2                            2     2
//                dx  + dy                           dx  + dy
//
// (x0,y0) is the vertex being moved, and (x1,y1)-(x1+dx,y1+dy) is the
// reference linedef.
//
// Segs corresponding to orthogonal linedefs (exactly vertical or horizontal
// linedefs), which comprise at least half of all linedefs in most wads, don't
// need to be considered, because they almost never contribute to slime trails
// (because then any roundoff error is parallel to the linedef, which doesn't
// cause slime). Skipping simple orthogonal lines lets the code finish quicker.
//
// Please note: This section of code is not interchangable with TeamTNT's
// code which attempts to fix the same problem.
//
// Firelines (TM) is a Rezistered Trademark of MBF Productions
//
//==============================================================================
procedure P_RemoveSlimeTrails;  // killough 10/98
var
  hit: PByteArray;
  i: integer;
  l: Pline_t;
  v: Pvertex_t;
  v_id: integer;
  dx2, dy2, dxy, s: int64;
  x0, y0, x1, y1: integer;
begin
  hit := mallocz(numvertexes);  // Hitlist for vertices
  for i := 0 to numsegs - 1 do  // Go through each seg
  begin
    if segs[i].miniseg then // skip minisegs
      continue;

    l := segs[i].linedef;               // The parent linedef
    if (l.dx <> 0) and (l.dy <> 0) then // We can ignore orthogonal lines
    begin
      v := segs[i].v1;
      while true do
      begin
        v_id := pDiff(v, vertexes, SizeOf(vertex_t));
        if hit[v_id] = 0 then // If we haven't processed vertex
        begin
          hit[v_id] := 1;        // Mark this vertex as processed
          if (v <> l.v1) and (v <> l.v2) then // Exclude endpoints of linedefs
          begin // Project the vertex back onto the parent linedef
            dx2 := (l.dx div FRACUNIT) * (l.dx div FRACUNIT);
            dy2 := (l.dy div FRACUNIT) * (l.dy div FRACUNIT);
            dxy := (l.dx div FRACUNIT) * (l.dy div FRACUNIT);
            s := dx2 + dy2;
            x0 := v.x;
            y0 := v.y;
            x1 := l.v1.x;
            y1 := l.v1.y;
            v.x := Round((dx2 * x0 + dy2 * x1 + dxy * (y0 - y1)) / s);
            v.y := Round((dy2 * y0 + dx2 * y1 + dxy * (x0 - x1)) / s);
          end;
        end;
        if v = segs[i].v2 then
          break;
        v := segs[i].v2;
      end;
    end;
  end;
  memfree(pointer(hit), numvertexes);
end;

end.
 
