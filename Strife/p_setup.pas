//------------------------------------------------------------------------------
//
//  DelphiStrife: A modified and improved Strife source port for Windows.
//
//  Based on:
//    - Linux Doom by "id Software"
//    - Chocolate Strife by "Simon Howard"
//    - DelphiDoom by "Jim Valavanis"
//
//  Copyright (C) 1993-1996 by id Software, Inc.
//  Copyright (C) 2005 Simon Howard
//  Copyright (C) 2010 James Haley, Samuel Villarreal
//  Copyright (C) 2004-2021 by Jim Valavanis
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
//   Setup a game, startup stuff.
//   Do all the WAD I/O, get map description,
//   set up initial state and misc. LUTs.
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit p_setup;

interface

uses
  d_delphi,
  doomdef,
  doomdata,
  m_fixed,
  p_mobj_h,
  r_defs;

const
  MAXRIFTSPOTS = 16;

var
// haleyjd 08/24/10: [STRIFE] rift spots for player spawning
  riftSpots: array[0..MAXRIFTSPOTS - 1] of mapthing_t;

function P_GetMapName(const map: integer): string;

// NOT called by W_Ticker. Fixme.
procedure P_SetupLevel(map, playermask: integer; skill: skill_t);

// Called by startup code.
procedure P_Init;

procedure P_ShutDown;

var
// origin of block map
  bmaporgx: fixed_t;
  bmaporgy: fixed_t;

  numvertexes: integer;
  vertexes: Pvertex_tArray;

  numsegs: integer;
  segs: Pseg_tArray;

  numsectors: integer;
  sectors: Psector_tArray;

  numsubsectors: integer;
  subsectors: Psubsector_tArray;

  numnodes: integer;
  nodes: Pnode_tArray;

  numlines: integer;
  lines: Pline_tArray;

  numsides: integer;
  sides: Pside_tArray;

//
// MAP related Lookup tables.
// Store VERTEXES, LINEDEFS, SIDEDEFS, etc.
//
var

// BLOCKMAP
// Created from axis aligned bounding box
// of the map, a rectangular array of
// blocks of size ...
// Used to speed up collision detection
// by spatial subdivision in 2D.
//
// Blockmap size.
  bmapwidth: integer;
  bmapheight: integer; // size in mapblocks
  bmapsize: integer;
  blockmap: PIntegerArray; // int for larger maps
// offsets in blockmap are from here
  blockmaplump: PIntegerArray;

  blockmapxneg: integer;
  blockmapyneg: integer;

  internalblockmapformat: boolean;

// for thing chains
type
  blocklinkitem_t = record
    size: integer;
    realsize: integer;
    links: Pmobj_tPArray;
  end;
  Pblocklinkitem_t = ^blocklinkitem_t;
  blocklinkarray_t = array[0..$FFFF] of blocklinkitem_t;
  Pblocklinkarray_t = ^blocklinkarray_t;

var
  blocklinks: Pblocklinkarray_t;

// REJECT
// For fast sight rejection.
// Speeds up enemy AI by skipping detailed
//  LineOf Sight calculation.
// Without special effect, this could be
//  used as a PVS lookup as well.
//
  rejectmatrix: PByteArray;
  rejectmatrixsize: integer;

  p_justspawned: boolean = false;

const
// Maintain single and multi player starting spots.
  MAX_DEATHMATCH_STARTS = 10;

var
  deathmatchstarts: array[0..MAX_DEATHMATCH_STARTS - 1] of mapthing_t;
  deathmatch_p: integer;

  playerstarts: array[0..MAXPLAYERS - 1] of mapthing_t;

function P_GameValidThing(const doomdnum: integer): boolean;

var
  useglnodesifavailable: boolean;

var
  largemap: boolean;

implementation

uses
  c_cmds,
  doomtype,
  d_player,
  d_main,
  d_think,
  z_zone,
  m_argv,
  m_bbox,
  g_game,
  i_system,
  w_wad,
  info,
  info_h,
  p_local,
  p_mobj,
  p_tick,
  p_spec,
  p_switch,
  p_inter,
  p_maputl,
  p_adjust,
  p_bridge,
  p_udmf,
  p_3dfloors, // JVAL: 3d Floors
  p_slopes,   // JVAL: Slopes
  p_easyslope,
  p_easyangle, // JVAL: 20201229 - Easy floor and ceiling texture angle
  p_affectees,
  p_musinfo,
  p_animdefs,
  ps_main,    // JVAL: Script Events
  r_data,
  r_things,
  info_rnd,
  m_rnd,
  mt_utils,
  r_colormaps,
{$IFNDEF OPENGL}
  r_cache_main,
  r_segs2,
{$ENDIF}
  r_intrpl,
{$IFDEF OPENGL}
  gl_render,  // JVAL OPENGL
  r_main,
{$ENDIF}
  r_subsectors,
  nd_main,
  s_sound,
  doomstat;

{$IFDEF OPENGL}
var
  glmapnum: integer;
{$ENDIF}

//
// P_LoadVertexes
//
procedure P_LoadVertexes(lump: integer);
var
  data: pointer;
  i: integer;
  ml: Pmapvertex_t;
  li: Pvertex_t;
  minx: integer;
  maxx: integer;
  miny: integer;
  maxy: integer;
  dx, dy: integer;
begin
  // Determine number of lumps:
  //  total lump length / vertex record length.
  numvertexes := W_LumpLength(lump) div SizeOf(mapvertex_t);

  // Allocate zone memory for buffer.
  vertexes := Z_Malloc(numvertexes * SizeOf(vertex_t), PU_LEVEL, nil);

  // Load data into cache.
  data := W_CacheLumpNum(lump, PU_STATIC);

  ml := Pmapvertex_t(data);

  // Copy and convert vertex coordinates,
  // internal representation as fixed.
  li := @vertexes[0];

  // JVAL: 20200414 -> Find map boundaries
  minx := 100000;
  maxx := -100000;
  miny := 100000;
  maxy := -100000;
  for i := 0 to numvertexes - 1 do
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
end;

{$IFDEF OPENGL}
var
  firstglvert: integer;

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

function GetDistance(dx, dy: integer): float;
var
  fx, fy: float;
begin
  fx := dx / FRACUNIT;
  fy := dy / FRACUNIT;
  result := sqrt(fx * fx + fy * fy);
end;
{$ENDIF}

//
// P_LoadSegs
//
procedure P_LoadSegs(lump: integer);
var
  data: pointer;
  i: integer;
  ml: Pmapseg_t;
  li: Pseg_t;
  ldef: Pline_t;
  linedef: integer;
  side: integer;
  sidenum: integer;
begin
  numsegs := W_LumpLength(lump) div SizeOf(mapseg_t);
  segs := Z_Malloc(numsegs * SizeOf(seg_t), PU_LEVEL, nil);
  ZeroMemory(segs, numsegs * SizeOf(seg_t));
  data := W_CacheLumpNum(lump, PU_STATIC);

  ml := Pmapseg_t(data);
  li := @segs[0];
  for i := 0 to numsegs - 1 do
  begin
    li.v1 := @vertexes[smallintwarp2(ml.v1)];
    li.v2 := @vertexes[smallintwarp2(ml.v2)];

    li.angle := ml.angle * FRACUNIT;
    li.offset := ml.offset * FRACUNIT;
    linedef := smallintwarp2(ml.linedef);
    ldef := @lines[linedef];
    li.linedef := ldef;
    side := smallintwarp2(ml.side);
    li.sidedef := @sides[ldef.sidenum[side]];
    li.frontsector := li.sidedef.sector;
    if ldef.flags and ML_TWOSIDED <> 0 then
    begin
      sidenum := ldef.sidenum[side xor 1];
      if sidenum = -1 then
      begin
        I_Warning('P_LoadSegs(): Line %d is marked with ML_TWOSIDED flag without backsector'#13#10, [linedef]);
        ldef.flags := ldef.flags and not ML_TWOSIDED;
        li.backsector := nil;
      end
      else
        li.backsector := sides[sidenum].sector
    end
    else
      li.backsector := nil;
    {$IFDEF OPENGL}
    li.length := GetDistance(li.v2.x - li.v1.x, li.v2.y - li.v1.y);
    li.iSegID := i;
    {$ENDIF}
    li.miniseg := false;
    inc(ml);
    inc(li);
  end;

  Z_Free(data);
end;

{$IFDEF OPENGL}
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

function GetOffset(v1, v2: Pvertex_t): fixed_t;
var
  a, b: single;
begin
  a := (v1.x - v2.x) / FRACUNIT;
  b := (v1.y - v2.y) / FRACUNIT;
  result := round(sqrt(a * a + b * b) * FRACUNIT);
end;

//
// P_LoadGLSegs
//
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
{$ENDIF}

//
// P_LoadSubsectors
//
procedure P_LoadSubsectors(lump: integer);
var
  data: pointer;
  i: integer;
  ms: Pmapsubsector_t;
  ss: Psubsector_t;
begin
  numsubsectors := W_LumpLength(lump) div SizeOf(mapsubsector_t);
  subsectors := Z_Malloc(numsubsectors * SizeOf(subsector_t), PU_LEVEL, nil);
  data := W_CacheLumpNum(lump, PU_STATIC);

  ms := Pmapsubsector_t(data);
  ZeroMemory(subsectors, numsubsectors * SizeOf(subsector_t));

  ss := @subsectors[0];
  for i := 0 to numsubsectors - 1 do
  begin
    ss.numlines := ms.numsegs;
    ss.firstline := ms.firstseg;
    inc(ms);
    inc(ss);
  end;

  Z_Free(data);
end;

{$IFDEF OPENGL}
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
{$ENDIF}

//
// P_LoadSectors
//
procedure P_LoadSectors(lump: integer);
var
  data: pointer;
  i: integer;
  ms: Pmapsector_t;
  ss: Psector_t;
begin
  numsectors := W_LumpLength(lump) div SizeOf(mapsector_t);
  sectors := Z_Malloc(numsectors * SizeOf(sector_t), PU_LEVEL, nil);
  ZeroMemory(sectors, numsectors * SizeOf(sector_t));
  data := W_CacheLumpNum(lump, PU_STATIC);

  ms := Pmapsector_t(data);
  ss := @sectors[0];
  for i := 0 to numsectors - 1 do
  begin
    ss.floorheight := ms.floorheight * FRACUNIT;
    ss.ceilingheight := ms.ceilingheight * FRACUNIT;
    ss.floorpic := R_FlatNumForName(ms.floorpic);
    ss.ceilingpic := R_FlatNumForName(ms.ceilingpic);
    ss.lightlevel := ms.lightlevel and $FF; // JVAL: Mars fog sectors
    ss.special := ms.special;
    ss.tag := ms.tag;
    ss.thinglist := nil;
    ss.touching_thinglist := nil;
    ss.stairlock := 0;
    ss.nextsec := -1; //jff 2/26/98 add fields to support locking out
    ss.prevsec := -1; // stair retriggering until build completes
    ss.floor_xoffs := 0;
    ss.floor_yoffs := 0;
    ss.ceiling_xoffs := 0;
    ss.ceiling_yoffs := 0;
    ss.heightsec := -1;
    ss.midsec := -1;    // JVAL: 3d floors
    ss.midline := -1;
    ss.floorlightsec := -1;   // sector used to get floor lighting
    ss.topmap := -1;
    ss.midmap := -1;
    ss.bottommap := -1;
    ss.renderflags := 0;
    if ms.lightlevel > $FF then // JVAL: Mars fog sectors
      ss.renderflags := ss.renderflags or SRF_FOG;
    ss.flags := 0;
    ss.gravity := GRAVITY;  // JVAL: sector gravity (VERSION 204)
    ss.floorangle := 0;     // JVAL: 20200221 - Texture angle
    ss.flooranglex := 0;    // JVAL: 20201229 - Texture angle rover
    ss.floorangley := 0;    // JVAL: 20201229 - Texture angle rover
    ss.ceilingangle := 0;   // JVAL: 20200221 - Texture angle
    ss.ceilinganglex := 0;  // JVAL: 20201229 - Texture angle rover
    ss.ceilingangley := 0;  // JVAL: 20201229 - Texture angle rover
{$IFDEF OPENGL}
    ss.floorlightlevel := ss.lightlevel;
    ss.ceilinglightlevel := ss.lightlevel;
{$ELSE}
    // [kb] For R_WiggleFix
    ss.cachedheight := 0;
    ss.scaleindex := 0;
    // JVAL: 20201225 - Speed up maps with large number of slopes
    ss.floorvisslope := -1;
    ss.ceilingvisslope := -1;
{$ENDIF}
    // killough 4/11/98 sector used to get ceiling lighting:
    ss.ceilinglightsec := -1;

    ss.iSectorID := i; // JVAL: 3d Floors
    inc(ms);
    inc(ss);
  end;

  Z_Free(data);
end;

//
// P_LoadNodes
//
procedure P_LoadNodes(lump: integer);
var
  data: pointer;
  i: integer;
  j: integer;
  k: integer;
  mn: Pmapnode_t;
  no: Pnode_t;
begin
  numnodes := W_LumpLength(lump) div SizeOf(mapnode_t);
  nodes := Z_Malloc(numnodes * SizeOf(node_t), PU_LEVEL, nil);
  data := W_CacheLumpNum(lump, PU_STATIC);

  mn := Pmapnode_t(data);
  no := @nodes[0];
  for i := 0 to numnodes - 1 do
  begin
    no.x := mn.x * FRACUNIT;
    no.y := mn.y * FRACUNIT;
    no.dx := mn.dx * FRACUNIT;
    no.dy := mn.dy * FRACUNIT;
    for j := 0 to 1 do
    begin
      // JVAL: glbsp
      if mn.children[j] and NF_SUBSECTOR <> 0 then
      begin
        mn.children[j] := mn.children[j] and not NF_SUBSECTOR;
        no.children[j] := mn.children[j];
        no.children[j] := no.children[j] or NF_SUBSECTOR_V5;
      end
      else
        no.children[j] := mn.children[j];

      for k := 0 to 3 do
        no.bbox[j, k] := mn.bbox[j, k] * FRACUNIT;
    end;
    inc(mn);
    inc(no);
  end;

  Z_Free(data);
end;

{$IFDEF OPENGL}
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
{$ENDIF}

function P_GameValidThing(const doomdnum: integer): boolean;
begin
  // Don't spawn DoomBuilder 3D Editing mode camera
  if doomdnum = 32000 then
  begin
    result := false;
    exit;
  end;
  if P_IsEasySlopeItem(doomdnum) then
  begin
    result := false;
    exit;
  end;
  result := true;
end;

//
// P_LoadThings
//
// haleyjd 08/24/10: [STRIFE]:
// * Added code to record rift spots
procedure P_LoadThings(lump: integer);
var
  data: pointer;
  i: integer;
  mt: Pmapthing_t;
  numthings: integer;
  riftnum: integer;
begin
  data := W_CacheLumpNum(lump, PU_STATIC);
  numthings := W_LumpLength(lump) div SizeOf(mapthing_t);

  P_EasySlopeInit;

  mt := Pmapthing_t(data);
  for i := 0 to numthings - 1 do
  begin
    if P_IsEasySlopeItem(mt._type) then // Do spawn easy slope items
      P_SpawnEasySlopeThing(mt);

    inc(mt);
  end;

  P_EasySlopeExecute;

  mt := Pmapthing_t(data);
  for i := 0 to numthings - 1 do
  begin
     // haleyjd 08/24/2010: Special Strife checks
     if (mt._type >= 118) and (mt._type < 128) then
     begin
      // initialize riftSpots
      riftnum := mt._type - 118;
      mt._type := 1;
      riftSpots[riftnum] := mt^;
     end
     else if (mt._type >= 9001) and (mt._type < 9011) then
     begin
      // STRIFE-TODO: mystery array of 90xx objects
     end
     else if P_GameValidThing(mt._type) then // Do spawn all other stuff.
      P_SpawnMapThing(mt);

    inc(mt);
  end;

  Z_Free(data);
end;

//
// P_LoadLineDefs
// Also counts secret lines for intermissions.
//
procedure P_LoadLineDefs(lump: integer);
var
  data: pointer;
  i: integer;
  mld: Pmaplinedef_t;
  ld: Pline_t;
  v1: Pvertex_t;
  v2: Pvertex_t;
begin
  numlines := W_LumpLength(lump) div SizeOf(maplinedef_t);
  lines := Z_Malloc(numlines * SizeOf(line_t), PU_LEVEL, nil);
  ZeroMemory(lines, numlines * SizeOf(line_t));
  data := W_CacheLumpNum(lump, PU_STATIC);

  mld := Pmaplinedef_t(data);
  ld := @lines[0];
  for i := 0 to numlines - 1 do
  begin
    ld.flags := mld.flags;
    ld.special := mld.special;
    ld.tag := mld.tag;
    ld.v1 := @vertexes[smallintwarp2(mld.v1)];
    v1 := ld.v1;
    ld.v2 := @vertexes[smallintwarp2(mld.v2)];
    v2 := ld.v2;
    ld.dx := v2.x - v1.x;
    ld.dy := v2.y - v1.y;

    if ld.dx = 0 then
      ld.slopetype := ST_VERTICAL
    else if ld.dy = 0 then
      ld.slopetype := ST_HORIZONTAL
    else if FixedDiv(ld.dy , ld.dx) > 0 then
      ld.slopetype := ST_POSITIVE
    else
      ld.slopetype := ST_NEGATIVE;

    if v1.x < v2.x then
    begin
      ld.bbox[BOXLEFT] := v1.x;
      ld.bbox[BOXRIGHT] := v2.x;
    end
    else
    begin
      ld.bbox[BOXLEFT] := v2.x;
      ld.bbox[BOXRIGHT] := v1.x;
    end;

    if v1.y < v2.y then
    begin
      ld.bbox[BOXBOTTOM] := v1.y;
      ld.bbox[BOXTOP] := v2.y;
    end
    else
    begin
      ld.bbox[BOXBOTTOM] := v2.y;
      ld.bbox[BOXTOP] := v1.y;
    end;

    ld.sidenum[0] := smallintwarp1(mld.sidenum[0]);
    ld.sidenum[1] := smallintwarp1(mld.sidenum[1]);

    if (ld.sidenum[0] >= 0) and (ld.sidenum[0] < numsides) then
      ld.frontsector := sides[ld.sidenum[0]].sector
    else
    begin
      if devparm then
        printf('P_LoadLineDefs(): Line %d does has invalid front sidedef %d'#13#10, [i, ld.sidenum[0]]);
      ld.sidenum[0] := 0;
      ld.frontsector := sides[0].sector;
    end;

    if (ld.sidenum[1] >= 0) and (ld.sidenum[1] < numsides) then
      ld.backsector := sides[ld.sidenum[1]].sector
    else
    begin
      ld.backsector := nil;
      if devparm then
        if ld.sidenum[1] >= numsides then
          printf('P_LoadLineDefs(): Line %d does has invalid back sidedef %d'#13#10, [i, ld.sidenum[0]]);
    end;

    ld.renderflags := 0;

    inc(mld);
    inc(ld);
  end;

  Z_Free (data);
end;

//
// P_LoadSideDefs
//
procedure P_LoadSideDefs(lump: integer);
var
  data: pointer;
  i: integer;
  msd: Pmapsidedef_t;
  sd: Pside_t;
begin
  numsides := W_LumpLength(lump) div SizeOf(mapsidedef_t);
  sides := Z_Malloc(numsides * SizeOf(side_t), PU_LEVEL, nil);
  ZeroMemory(sides, numsides * SizeOf(side_t));
  data := W_CacheLumpNum(lump, PU_STATIC);

  msd := Pmapsidedef_t(data);
  sd := @sides[0];
  for i := 0 to numsides - 1 do
  begin
    sd.textureoffset := msd.textureoffset * FRACUNIT;
    sd.rowoffset := msd.rowoffset * FRACUNIT;
    sd.toptexture := R_SafeTextureNumForName(msd.toptexture);
    if sd.toptexture = 0 then
      sd.toptexture := -1 - R_CustomColorMapForName(msd.toptexture);

    sd.bottomtexture := R_SafeTextureNumForName(msd.bottomtexture);
    if sd.bottomtexture = 0 then
      sd.bottomtexture := -1 - R_CustomColorMapForName(msd.bottomtexture);

    sd.midtexture := R_SafeTextureNumForName(msd.midtexture);
    if sd.midtexture = 0 then
      sd.midtexture := -1 - R_CustomColorMapForName(msd.midtexture);

    sd.sector := @sectors[smallintwarp2(msd.sector)];
    inc(msd);
    inc(sd);
  end;

  Z_Free(data);
end;

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

//
// Subroutine to add a line number to a block list
// It simply returns if the line is already in the block
//

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

//
// Actually construct the blockmap lump from the level data
//
// This finds the intersection of each linedef with the column and
// row lines at the left and bottom of each blockmap cell. It then
// adds the line to all block lists touching the intersection.
//

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

//
// P_LoadBlockMap
//
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

//
// P_GroupLines
// Builds sector line lists and subsector sector numbers.
// Finds block bounding boxes for sectors.
//
procedure P_GroupLines;
var
  linebuffer: Pline_tPArray; // pointer to an array of pointers Pline_t
  i: integer;
  j: integer;
  total: integer;
  li: Pline_t;
  sector: Psector_t;
  pss: Psubsector_t;
  seg: Pseg_t;
  bbox: array[0..3] of fixed_t;
  block: integer;
begin
  // look up sector number for each subsector
  pss := @subsectors[0];
  for i := 0 to numsubsectors - 1 do
  begin
    seg := @segs[pss.firstline];
    pss.sector := nil;
    for j := 0 to pss.numlines - 1 do
    begin
      if seg.sidedef <> nil then
      begin
      {$IFDEF DEBUG}
        printf('subsector %5d (%8d), line %2d (%8d), sector %4d (%8d) '#13#10,
          [i, integer(pss), j, integer(seg.sidedef), integer(seg.sidedef.sector), (integer(seg.sidedef.sector) - integer(sectors)) div SizeOf(sector_t)]);
      {$ENDIF}
        pss.sector := seg.sidedef.sector;
        break;
      end;
      inc(seg);
    end;
    if pss.sector = nil then
      I_Error('P_GroupLines(): Subsector %d is not part of a sector', [i]);
    inc(pss);
  end;

  // count number of lines in each sector
  total := 0;
  for i := 0 to numlines - 1 do
  begin
    li := @lines[i];
    inc(total);
    if li.frontsector <> nil then
      li.frontsector.linecount := li.frontsector.linecount + 1
    else
    begin
      if li.backsector = nil then
        I_Warning('P_GroupLines(): Line %d is missing frontsector & backsector'#13#10, [i])
      else
        I_Warning('P_GroupLines(): Line %d is missing frontsector'#13#10, [i]);
    end;

    if (li.backsector <> nil) and (li.backsector <> li.frontsector) then
    begin
      li.backsector.linecount := li.backsector.linecount + 1;
      inc(total);
    end;

    if li.special = 260 then
    begin
      if li.tag = 0 then
        li.renderflags := li.renderflags or LRF_TRANSPARENT
      else
      begin
        for j := 0 to numlines - 1 do
          if lines[j].tag = li.tag then
            lines[j].renderflags := lines[j].renderflags or LRF_TRANSPARENT
      end;
    end;

  end;

  // build line tables for each sector
  linebuffer := Z_Malloc(total * SizeOf(Pline_t), PU_LEVEL, nil);
  sector := @sectors[0];
  for i := 0 to numsectors - 1 do
  begin
    M_ClearBox(@bbox);
    sector.lines := linebuffer;
    li := @lines[0];
    for j := 0 to numlines - 1 do
    begin
      if (li.frontsector = sector) or (li.backsector = sector) then
      begin
        linebuffer[0] := li;
        linebuffer := @linebuffer[1];
        M_AddToBox(@bbox, li.v1.x, li.v1.y);
        M_AddToBox(@bbox, li.v2.x, li.v2.y);
      end;
      inc(li);
    end;
    if pDiff(linebuffer, sector.lines, SizeOf(pointer)) <> sector.linecount then
      I_Error('P_GroupLines(): miscounted');

    // set the degenmobj_t to the middle of the bounding box
    if largemap then
    begin
      sector.soundorg.x := bbox[BOXRIGHT] div 2 + bbox[BOXLEFT] div 2;
      sector.soundorg.y := bbox[BOXTOP] div 2 + bbox[BOXBOTTOM] div 2;
    end
    else
    begin
      sector.soundorg.x := (bbox[BOXRIGHT] + bbox[BOXLEFT]) div 2;
      sector.soundorg.y := (bbox[BOXTOP] + bbox[BOXBOTTOM]) div 2;
    end;

    // adjust bounding box to map blocks
    block := MapBlockIntY(int64(bbox[BOXTOP]) - int64(bmaporgy) + MAXRADIUS);
    if block >= bmapheight then
      block  := bmapheight - 1;
    sector.blockbox[BOXTOP] := block;

    block := MapBlockIntY(int64(bbox[BOXBOTTOM]) - int64(bmaporgy) - MAXRADIUS);
    if block < 0 then
      block  := 0;
    sector.blockbox[BOXBOTTOM] := block;

    block := MapBlockIntX(int64(bbox[BOXRIGHT]) - int64(bmaporgx) + MAXRADIUS);
    if block >= bmapwidth then
      block := bmapwidth - 1;
    sector.blockbox[BOXRIGHT] := block;

    block := MapBlockIntX(int64(bbox[BOXLEFT]) - int64(bmaporgx) - MAXRADIUS);
    if block < 0 then
      block := 0;
    sector.blockbox[BOXLEFT] := block;

    inc(sector);
  end;


  for i := 0 to numlines - 1 do
    if lines[i].special = 242 then
    begin
      if sides[lines[i].sidenum[0]].toptexture < 0 then
        sides[lines[i].sidenum[0]].sector.topmap := -sides[lines[i].sidenum[0]].toptexture - 1;
      if sides[lines[i].sidenum[0]].bottomtexture < 0 then
        sides[lines[i].sidenum[0]].sector.bottommap := -sides[lines[i].sidenum[0]].bottomtexture - 1;
      if sides[lines[i].sidenum[0]].midtexture < 0 then
        sides[lines[i].sidenum[0]].sector.midmap := -sides[lines[i].sidenum[0]].midtexture - 1;
    end;

  for i := 0 to numsides - 1 do
  begin
    if sides[i].toptexture < 0 then
      sides[i].toptexture := 0;
    if sides[i].bottomtexture < 0 then
      sides[i].bottomtexture := 0;
    if sides[i].midtexture < 0 then
      sides[i].midtexture := 0;
  end;
end;

function P_GetMapName(const map: integer): string;
begin
  // find map name
  if map < 10 then
    sprintf(result,'map0%d', [map])
  else
    sprintf(result,'map%d', [map]);
end;

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

//
// P_SetupLevel
//
procedure P_SetupLevel(map, playermask: integer; skill: skill_t);
var
  i: integer;
  lumpname: string;
  lumpnum: integer;
{$IFDEF OPENGL}
  glmapname: string;
{$ENDIF}
  gwa: TGWAFile;
  gwaname: string;
  gwaloaded: boolean;
begin
  totalkills := 0;
  totalsecret := 0;

  isgamefreezed := false;

  if not preparingdemoplayback then
    sysrndseed := I_Random;

  for i := 0 to MAXPLAYERS - 1 do
  begin
    players[i].allegiance := i;
    players[i].killcount := 0;
    players[i].lastbreath := 0;
  end;

  // Initial height of PointOfView
  // will be set by player think.
  players[consoleplayer].viewz := 1;

  // Make sure all sounds are stopped before Z_FreeTags.
  S_Start;

{$IFDEF OPENGL}
  gld_CleanMemory; // JVAL OPENGL
{$ENDIF}

  MT_WaitTasks;

  Z_FreeTags(PU_LEVEL, PU_PURGELEVEL - 1);

  R_SetupLevel;

  P_InitThinkers;
  P_InitIntercepts;

  // if working with a devlopment map, reload it
  W_Reload;

  // find map name
  lumpname := P_GetMapName(map);

  printf(#13#10'-------------'#13#10);
  printf('Loading %s'#13#10, [lumpname]);
  if spawnrandommonsters then
    printf(' Random monsters seed=%d'#13#10, [sysrndseed]);

  UDMF_Check(lumpname);
  ND_NodesCheck(lumpname);
  PS_LinkScriptEvents(lumpname);  // JVAL: Script Events

  lumpnum := W_GetNumForName(lumpname);
  gwaloaded := false;

{$IFDEF OPENGL}
  if useglnodesifavailable and not G_NeedsCompatibilityMode then
  begin
    glmapnum := gld_GetGLMapLump(lumpnum);
    glnodesver := gld_GetGLNodesVersion(glmapnum);
    gwaloaded := glnodesver > 0;
  end
  else
  begin
    glmapnum := -1;
    glnodesver := 0;
  end;
{$ENDIF}

  leveltime := 0;

  gwa := nil;
  if not gwaloaded then
  begin
    gwaname := ND_GetNodes(lumpname);
    if gwaname <> '' then
      gwa := TGWAFile.Create(gwaname);
  end;

  // note: most of this ordering is important
  {$IFDEF OPENGL}
  if gwa = nil then
    P_GLLoadVertexes(lumpnum + Ord(ML_VERTEXES), glmapnum + Ord(ML_GL_VERTS))
  else
    ND_LoadVertexes(lumpnum + Ord(ML_VERTEXES), gwa);
  {$ELSE}
  if gwa = nil then
    P_LoadVertexes(lumpnum + Ord(ML_VERTEXES))
  else
    ND_LoadVertexes(lumpnum + Ord(ML_VERTEXES), gwa);
  {$ENDIF}
  P_LoadSectors(lumpnum + Ord(ML_SECTORS));
  P_LoadSideDefs(lumpnum + Ord(ML_SIDEDEFS));
  P_LoadLineDefs(lumpnum + Ord(ML_LINEDEFS));
  P_LoadBlockMap(lumpnum + Ord(ML_BLOCKMAP));
  {$IFDEF OPENGL}
  if (glnodesver > 0) and (glmapnum <> - 1) then
  begin
    glmapname := W_GetNameForNum(glmapnum);
    printf(' GL nodes v%d found (%s)'#13#10, [glnodesver, glmapname]);
    if glnodesver >= 3 then
      P_LoadSubsectorsV3V5(glmapnum + Ord(ML_GL_SSECT))
    else
      P_LoadSubsectors(glmapnum + Ord(ML_GL_SSECT));
    if glnodesver >= 4 then
      P_LoadNodesV4V5(glmapnum + Ord(ML_GL_NODES))
    else
      P_LoadNodes(glmapnum + Ord(ML_GL_NODES));
    P_LoadGLSegs(glmapnum + Ord(ML_GL_SEGS));
  end
  else
  {$ENDIF}
  begin
    if gwa <> nil then
    begin
      ND_LoadSubsectors(gwa);
      ND_LoadNodes(gwa);
      ND_LoadSegs(gwa);
    end
    else
    begin
    {$IFDEF OPENGL}
      printf(' GL nodes not found, using standard nodes'#13#10);
    {$ENDIF}
      P_LoadSubsectors(lumpnum + Ord(ML_SSECTORS));
      P_LoadNodes(lumpnum + Ord(ML_NODES));
      P_LoadSegs(lumpnum + Ord(ML_SEGS));
    end;
  end;

  if gwa <> nil then
    gwa.Free;

  rejectmatrix := W_CacheLumpNum(lumpnum + Ord(ML_REJECT), PU_LEVEL);
  rejectmatrixsize := W_LumpLength(lumpnum + Ord(ML_REJECT));
  P_GroupLines;

  P_3dFloorSetupSegs; // JVAL: 3d Floors

  P_RemoveSlimeTrails;    // killough 10/98: remove slime trails from wad

{$IFNDEF OPENGL}
  R_CalcSectors; // JVAL 20200105 - Check the map boundaries
  R_PrecalcSegs; // https://www.doomworld.com/forum/topic/70288-dynamic-wiggletall-sector-fix-for-fixed-point-software-renderer/?do=findComment&comment=1340433
{$ENDIF}

  P_SlopesSetup;// JVAL: Slopes

  P_SetupSectorAffectees;

  if autoadjustmissingtextures then
    P_AdjustMissingTextures;

  P_CalcSubSectorsBridge;

  R_PrecalcPointInSubSector;

  bodyqueslot := 0;
  deathmatch_p := 0;

  if devparm then
    printf('P_LoadThings()'#13#10);
  P_LoadThings(lumpnum + Ord(ML_THINGS));

  // JVAL: 20201229 - Easy floor and ceiling texture angle
  P_AdjustEasyAngle;

  // if deathmatch, randomly spawn the active players
  if deathmatch <> 0 then
  begin
    for i := 0 to MAXPLAYERS - 1 do
      if playeringame[i] then
      begin
        players[i].mo := nil;
        G_DeathMatchSpawnPlayer(i);
      end;
  end;

  // clear special respawning que
  iquehead := 0;
  iquetail := 0;

  // set up world state
  if devparm then
    printf('P_SpawnSpecials()'#13#10);
  P_SpawnSpecials;

  {$IFNDEF OPENGL}
  if devparm then
    printf('R_Clear32Cache()'#13#10);
  R_Clear32Cache;
  {$ENDIF}

  // preload graphics
  // JVAL
  // Precache if we have external textures
  if precache or externalpakspresent then
  begin
    if devparm then
      printf('R_PrecacheLevel()'#13#10);
    R_PrecacheLevel;
    if devparm then
      printf('S_PrecacheSounds()'#13#10);
    S_PrecacheSounds;
  end;

{$IFDEF OPENGL}
  if devparm then
    printf('gld_PreprocessLevel()'#13#10);
  gld_PreprocessLevel; // JVAL OPENGL
{$ENDIF}

  R_SetInterpolateSkipTicks(2);
end;

//
// P_Init
//
procedure P_Init;
begin
  P_InitSwitchList;
  P_InitPicAnims;
  P_InitAnimations;
  P_InitFTAnims; // Init flat and texture animations
  R_InitSprites(sprnames);
  P_InitMusInfo;
  C_AddCmd('suicide', @P_CmdSuicide);
  C_AddCmd('doadjustmissingtextures', @P_AdjustMissingTextures);
end;

//==========================================================================
//
// P_ShutDown
//
//==========================================================================

procedure P_ShutDown;
begin
  P_ShutDownAnimations;
end;

end.

