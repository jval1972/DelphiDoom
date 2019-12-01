//------------------------------------------------------------------------------
//
//  DelphiHexen: A modified and improved Hexen port for Windows
//  based on original Linux Doom as published by "id Software", on
//  Hexen source as published by "Raven" software and DelphiDoom
//  as published by Jim Valavanis.
//  Copyright (C) 2004-2017 by Jim Valavanis
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
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
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

//-----------------------------------------------------------------------------
//
// DESCRIPTION:
//   Setup a game, startup stuff.
//  Do all the WAD I/O, get map description,
//  set up initial state and misc. LUTs.
//
//-----------------------------------------------------------------------------

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
  blockmap: PSmallIntArray; // int for larger maps
// offsets in blockmap are from here
  blockmaplump: PSmallIntArray;
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
  MAX_DEATHMATCH_STARTS = 16;
  MAX_PLAYER_STARTS = 8;

var
  deathmatchstarts: array[0..MAX_DEATHMATCH_STARTS - 1] of mapthing_t;
  deathmatch_p: integer;

  playerstarts: array[0..MAX_PLAYER_STARTS - 1, 0..MAXPLAYERS - 1] of mapthing_t;

function P_GameValidThing(const doomdnum: integer): boolean;

function P_GetMapSky1Texture(map: integer): integer;

function P_GetMapSky2Texture(map: integer): integer;

function P_GetMapSky1ScrollDelta(map: integer): fixed_t;

function P_GetMapSky2ScrollDelta(map: integer): fixed_t;

function P_GetMapDoubleSky(map: integer): boolean;

function P_GetMapLightning(map: integer): boolean;

function P_TranslateMap(map: integer): integer;

function P_GetMapNextMap(map: integer): integer;

function P_GetMapDescName(const map: integer): string;

function P_GetMapSongLump(map: integer): string;

function P_GetMapCluster(map: integer): integer;

function P_GetMapFadeTable(map: integer): integer;

var
  useglnodesifavailable: boolean;

implementation

uses
  c_cmds,
  d_player,
  doomtype,
  m_argv,
  z_zone,
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
  p_adjust,
  p_inter,
  p_enemy,
  p_pspr,
  p_acs,
  p_anim,
  p_udmf,
  p_3dfloors,
  p_slopes,   // JVAL: Slopes
  p_affectees,
  po_man,
  ps_main,    // JVAL: Script Events
  r_data,
  r_things,
{$IFNDEF OPENGL}
  r_cache,
{$ENDIF}
  r_intrpl,
{$IFDEF OPENGL}
  gl_tex,     // JVAL OPENGL
  gl_render,  // JVAL OPENGL
  r_main,
{$ENDIF}
  nd_main,
  s_sound,
  s_sndseq,
  sc_engine,
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
  for i := 0 to numvertexes - 1 do
  begin
    li.x := ml.x * FRACUNIT;
    li.y := ml.y * FRACUNIT;
    li.amvalidcount := 0;
    inc(ml);
    inc(li);
  end;

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

  // Copy and convert vertex coordinates,
  // internal representation as fixed.
  li := @vertexes[0];
  for i := 0 to firstglvert - 1 do
  begin
    li.x := ml.x * FRACUNIT;
    li.y := ml.y * FRACUNIT;
    li.amvalidcount := 0;
    inc(ml);
    inc(li);
  end;

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
      li.backsector := sides[ldef.sidenum[side xor 1]].sector
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
          li.backsector := sides[ldef.sidenum[side xor 1]].sector
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
          li.backsector := sides[ldef.sidenum[side xor 1]].sector
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
    ss.lightlevel := ms.lightlevel;
    ss.special := ms.special;
    ss.tag := ms.tag;
    ss.thinglist := nil;
    ss.seqType := SEQTYPE_STONE; // default seqType
    ss.midsec := -1;    // JVAL: 3d floors
    ss.midline := -1;
    ss.renderflags := 0;
    ss.flags := 0;
    ss.iSectorID := i;
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

  // Do not registered monsters if shareware
  if gamemode = shareware then
  begin
    case doomdnum of
      34,     // MT_WRAITG
      114,    // MT_BISHOP
      254,    // MT_DRAGON
      8101,   // MT_ZSHRUB1
      8080,   // MT_DEMON2
      8102,   // MT_ZSHRUB2
      10080,  // MT_SORCBOSS
      10200:  // MT_KORAX
        begin
          result := false;
          exit;
        end;
    end;
  end;

  result := true;
end;

//
// P_LoadThings
//
procedure P_LoadThings(lump: integer);
var
  data: pointer;
  i: integer;
  mt: Pmapthing_t;
  numthings: integer;
  playerCount: integer;
begin
  data := W_CacheLumpNum(lump, PU_STATIC);
  numthings := W_LumpLength(lump) div SizeOf(mapthing_t);

  mt := Pmapthing_t(data);
  for i := 0 to numthings - 1 do
  begin
    if P_GameValidThing(mt._type) then // Do spawn all other stuff.
      P_SpawnMapThing(mt);

    inc(mt);
  end;

  P_CreateTIDList;
  P_InitCreatureCorpseQueue(false); // false = do NOT scan for corpses

  Z_Free(data);

  if deathmatch = 0 then
    exit;

  playerCount := 0;
  for i := 0 to MAXPLAYERS - 1 do
    if playeringame[i] then
      inc(playerCount);

  if deathmatch_p < playerCount then
    I_Error('P_LoadThings(): Player count (%d) exceeds deathmatch spots (%d)', [playerCount, deathmatch_p]);

end;

//
// JVAL: Changed for compatibility with DelphiDoom ver 0.8

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
    ld.arg1 := mld.arg1;
    ld.arg2 := mld.arg2;
    ld.arg3 := mld.arg3;
    ld.arg4 := mld.arg4;
    ld.arg5 := mld.arg5;
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

    if ld.sidenum[0] <> -1 then
      ld.frontsector := sides[ld.sidenum[0]].sector
    else
      ld.frontsector := nil;

    if ld.sidenum[1] <> -1 then
      ld.backsector := sides[ld.sidenum[1]].sector
    else
      ld.backsector := nil;

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
    sd.toptexture := R_TextureNumForName(msd.toptexture);
    sd.bottomtexture := R_TextureNumForName(msd.bottomtexture);
    sd.midtexture := R_TextureNumForName(msd.midtexture);
    sd.sector := @sectors[smallintwarp2(msd.sector)];
    inc(msd);
    inc(sd);
  end;

  Z_Free(data);
end;

//
// P_LoadBlockMap
//
procedure P_LoadBlockMap(lump: integer);
var
  count: integer;
begin
  blockmaplump := W_CacheLumpNum(lump, PU_LEVEL);
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
  psd: Psubsector_t;
  seg: Pseg_t;
  bbox: array[0..3] of fixed_t;
  block: integer;
begin
  // look up sector number for each subsector
  psd := @subsectors[0];
  for i := 0 to numsubsectors - 1 do
  begin
    seg := @segs[psd.firstline];
    psd.sector := nil;
    for j := 0 to psd.numlines - 1 do
    begin
      if seg.sidedef <> nil then
      begin
      {$IFDEF DEBUG}
        printf('subsector %5d (%8d), line %2d (%8d), sector %4d (%8d) '#13#10,
          [i, integer(psd), j, integer(seg.sidedef), integer(seg.sidedef.sector), (integer(seg.sidedef.sector) - integer(sectors)) div SizeOf(sector_t)]);
      {$ENDIF}
        psd.sector := seg.sidedef.sector;
        break;
      end;
      inc(seg);
    end;
    if psd.sector = nil then
      I_Error('P_GroupLines(): Subsector %d is not part of a sector', [i]);
    inc(psd);
  end;

  // count number of lines in each sector
  total := 0;
  for i := 0 to numlines - 1 do
  begin
    li := @lines[i];
    inc(total);
    if li.frontsector <> nil then
      li.frontsector.linecount := li.frontsector.linecount + 1;

    if (li.backsector <> nil) and (li.backsector <> li.frontsector) then
    begin
      li.backsector.linecount := li.backsector.linecount + 1;
      inc(total);
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
    sector.soundorg.x := (bbox[BOXRIGHT] + bbox[BOXLEFT]) div 2;
    sector.soundorg.y := (bbox[BOXTOP] + bbox[BOXBOTTOM]) div 2;

    // adjust bounding box to map blocks
    block := MapBlockInt(bbox[BOXTOP] - bmaporgy + MAXRADIUS);
    if block >= bmapheight then
      block  := bmapheight - 1;
    sector.blockbox[BOXTOP] := block;

    block := MapBlockInt(bbox[BOXBOTTOM] - bmaporgy - MAXRADIUS);
    if block < 0 then
      block  := 0;
    sector.blockbox[BOXBOTTOM] := block;

    block := MapBlockInt(bbox[BOXRIGHT] - bmaporgx + MAXRADIUS);
    if block >= bmapwidth then
      block := bmapwidth - 1;
    sector.blockbox[BOXRIGHT] := block;

    block := MapBlockInt(bbox[BOXLEFT] - bmaporgx - MAXRADIUS);
    if block < 0 then
      block := 0;
    sector.blockbox[BOXLEFT] := block;

    inc(sector);
  end;
end;

//==========================================================================
//
// P_GetMapName
//
//==========================================================================

function P_GetMapName(const map: integer): string;
begin
  // find map name
  if map < 10 then
    sprintf(result,'MAP0%d', [map])
  else
    sprintf(result,'MAP%d', [map]);
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
begin                        exit;
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
  mobj: Pmobj_t;
  parm: integer;
{$IFDEF OPENGL}
  glmapname: string;
{$ENDIF}
  gwa: TGWAFile;
  gwaname: string;
  gwaloaded: boolean;
begin
  totalkills := 0;
  totalitems := 0;
  totalsecret := 0;

  wminfo.maxfrags := 0;
  wminfo.partime := 180;
  for i := 0 to MAXPLAYERS - 1 do
  begin
    players[i].killcount := 0;
    players[i].secretcount := 0;
    players[i].itemcount := 0;
  end;

  // Initial height of PointOfView
  // will be set by player think.
  players[consoleplayer].viewz := 1;

  // Make sure all sounds are stopped before Z_FreeTags.
  S_Start;

  Z_FreeTags(PU_LEVEL, PU_PURGELEVEL - 1);

{$IFDEF OPENGL}
  gld_CleanMemory; // JVAL OPENGL
{$ENDIF}

  R_SetupLevel;

  P_InitThinkers;

  // if working with a devlopment map, reload it
  W_Reload;

  // find map name
  lumpname := P_GetMapName(map);
  gwaloaded := false;

  printf(#13#10'-----------------------'#13#10);
  printf('Loading %s (%s)'#13#10, [lumpname, P_GetMapDescName(map)]);

  UDMF_Check(lumpname);
  ND_NodesCheck(lumpname);
  PS_LinkScriptEvents(lumpname);  // JVAL: Script Events

  lumpnum := W_GetNumForName(lumpname);

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
  P_LoadBlockMap(lumpnum + Ord(ML_BLOCKMAP));
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
  P_RemoveSlimeTrails;

  P_SlopesSetup;// JVAL: Slopes

  P_SetupSectorAffectees;

  if autoadjustmissingtextures then
    P_AdjustMissingTextures;

  bodyqueslot := 0;
  po_NumPolyobjs := 0;
  deathmatch_p := 0;
  P_LoadThings(lumpnum + Ord(ML_THINGS));
  PO_Init(lumpnum + Ord(ML_THINGS));            // Initialize the polyobjs
  P_LoadACScripts(lumpnum + Ord(ML_BEHAVIOR));  // ACS object code

  // if deathmatch, randomly spawn the active players
  TimerGame := 0;
  if deathmatch <> 0 then
  begin
    for i := 0 to MAXPLAYERS - 1 do
      if playeringame[i] then
      begin
        mobj := P_SpawnMobj(playerstarts[0][i].x * FRACUNIT, playerstarts[0][i].y * FRACUNIT, 0, Ord(MT_PLAYER_FIGHTER));
        players[i].mo := mobj;
        G_DeathMatchSpawnPlayer(i);
        P_RemoveMobj(mobj);
      end;
    parm := M_CheckParm('-timer');
    if (parm > 0) and (parm < myargc - 1) then
      TimerGame := atoi(myargv[parm + 1]) * TICRATE * 60;
  end;

  // clear special respawning que
  iquehead := 0;
  iquetail := 0;

  // set up world state
  P_SpawnSpecials;


  R_ChangeColormap(W_GetNameForNum(P_GetMapFadeTable(gamemap)));

  {$IFNDEF OPENGL}
  R_Clear32Cache;
  {$ENDIF}

  // preload graphics
  // JVAL
  // Precache if we have external textures
  if precache or externalpakspresent then
  begin
    R_PrecacheLevel;
    S_PrecacheSounds;
  end;

{$IFDEF OPENGL}
  gld_PreprocessLevel; // JVAL OPENGL
{$ENDIF}

  R_SetInterpolateSkipTicks(2);

  // Check if the level is a lightning level
  P_InitLightning;

  S_StopAllSounds;
  S_StopAllSequences;
//  S_StartSong(gamemap, true); JVAL SOS
end;

const
  MAPINFO_SCRIPT_NAME = 'MAPINFO';
  MCMD_SKY1 = 1;
  MCMD_SKY2 = 2;
  MCMD_LIGHTNING = 3;
  MCMD_FADETABLE = 4;
  MCMD_DOUBLESKY = 5;
  MCMD_CLUSTER = 6;
  MCMD_WARPTRANS = 7;
  MCMD_NEXT = 8;
  MCMD_CDTRACK = 9;
  MCMD_CD_STARTTRACK = 10;
  MCMD_CD_END1TRACK = 11;
  MCMD_CD_END2TRACK = 12;
  MCMD_CD_END3TRACK = 13;
  MCMD_CD_INTERTRACK = 14;
  MCMD_CD_TITLETRACK = 15;

const
  MapCmdIDs: array[0..14] of integer = (
    MCMD_SKY1,
    MCMD_SKY2,
    MCMD_DOUBLESKY,
    MCMD_LIGHTNING,
    MCMD_FADETABLE,
    MCMD_CLUSTER,
    MCMD_WARPTRANS,
    MCMD_NEXT,
    MCMD_CDTRACK,
    MCMD_CD_STARTTRACK,
    MCMD_CD_END1TRACK,
    MCMD_CD_END2TRACK,
    MCMD_CD_END3TRACK,
    MCMD_CD_INTERTRACK,
    MCMD_CD_TITLETRACK
  );

var
  cd_NonLevelTracks: array[0..5] of integer;

const
  UNKNOWN_MAP_NAME = 'DEVELOPMENT MAP';
  DEFAULT_SKY_NAME = 'SKY1';
  DEFAULT_SONG_LUMP = 'DEFSONG';
  DEFAULT_FADE_TABLE = 'COLORMAP';

type
  mapinfo_t = record
    cluster: smallint;
    warpTrans: smallint;
    nextMap: smallint;
    cdTrack: smallint;
    name: string[32];
    sky1Texture: smallint;
    sky2Texture: smallint;
    sky1ScrollDelta: fixed_t;
    sky2ScrollDelta: fixed_t;
    doubleSky: boolean;
    lightning: boolean;
    fadetable: integer;
    songLump: string[10];
  end;
  Pmapinfo_t = ^mapinfo_t;

var
  mapcount: integer;

var
  MapInfo: array[0..99] of mapinfo_t;


function P_GetMapDescName(const map: integer): string;
begin
  result := MapInfo[map].name;
end;


//==========================================================================
//
// P_InitMapInfo
//
//==========================================================================

procedure P_InitMapInfo;
var
  map: integer;
  mapMax: integer;
  mcmdValue: integer;
  info: Pmapinfo_t;
  sc: TScriptEngine;
  MapCmdNames: TDStringList;
begin
  MapCmdNames := TDStringList.Create;
  MapCmdNames.Add('SKY1');
  MapCmdNames.Add('SKY2');
  MapCmdNames.Add('DOUBLESKY');
  MapCmdNames.Add('LIGHTNING');
  MapCmdNames.Add('FADETABLE');
  MapCmdNames.Add('CLUSTER');
  MapCmdNames.Add('WARPTRANS');
  MapCmdNames.Add('NEXT');
  MapCmdNames.Add('CDTRACK');
  MapCmdNames.Add('CD_START_TRACK');
  MapCmdNames.Add('CD_END1_TRACK');
  MapCmdNames.Add('CD_END2_TRACK');
  MapCmdNames.Add('CD_END3_TRACK');
  MapCmdNames.Add('CD_INTERMISSION_TRACK');
  MapCmdNames.Add('CD_TITLE_TRACK');

  mapMax := 1;

  // Put defaults into MapInfo[0]
  info := @MapInfo[0];
  info.cluster := 0;
  info.warpTrans := 0;
  info.nextMap := 1; // Always go to map 1 if not specified
  info.cdTrack := 1;
  info.sky1Texture := R_CheckTextureNumForName(DEFAULT_SKY_NAME); // JVAL: Originally was R_TextureNumForName
  info.sky2Texture := info.sky1Texture;
  info.sky1ScrollDelta := 0;
  info.sky2ScrollDelta := 0;
  info.doubleSky := false;
  info.lightning := false;
  info.fadetable := W_GetNumForName(DEFAULT_FADE_TABLE);
  info.name := UNKNOWN_MAP_NAME;

  sc := TScriptEngine.Create(RemoveLineQuotes(W_TextLumpName(MAPINFO_SCRIPT_NAME)));
  while sc.GetString do
  begin
    if not sc.Compare('MAP') then
    begin
      sc.ScriptError('"MAP" expected'#13#10);
    end;
    sc.MustGetInteger;
    if (sc._Integer < 1) or (sc._Integer > 99) then
    begin //
      sc.ScriptError('Map number (%d) must be in [1..99] range'#13#10, [sc._Integer]);
    end;
    map := sc._Integer;

    info := @MapInfo[map];

    // Copy defaults to current map definition
    memcpy(info, @MapInfo[0], SizeOf(mapinfo_t));

    // The warp translation defaults to the map number
    info.warpTrans := map;

    // Map name must follow the number
    sc.MustGetString;
    info.name := sc._String;

    // Process optional tokens
    while sc.GetString do
    begin
      if sc.Compare('MAP') then
      begin // Start next map definition
        sc.UnGet;
        break;
      end;
      mcmdValue := MapCmdIDs[sc.MustMatchString(MapCmdNames)];
      case mcmdValue of
        MCMD_CLUSTER:
          begin
            sc.MustGetInteger;
            info.cluster := sc._Integer;
          end;
        MCMD_WARPTRANS:
          begin
            sc.MustGetInteger;
            info.warpTrans := sc._Integer;
          end;
        MCMD_NEXT:
          begin
            sc.MustGetInteger;
            info.nextMap := sc._Integer;
          end;
        MCMD_CDTRACK:
          begin
            sc.MustGetInteger;
            info.cdTrack := sc._Integer;
          end;
        MCMD_SKY1:
          begin
            sc.MustGetString;
            info.sky1Texture := R_TextureNumForName(sc._String);
            sc.MustGetInteger;
            info.sky1ScrollDelta := sc._Integer * 256;
          end;
        MCMD_SKY2:
          begin
            sc.MustGetString;
            info.sky2Texture := R_TextureNumForName(sc._String);
            sc.MustGetInteger;
            info.sky2ScrollDelta := sc._Integer * 256;
          end;
        MCMD_DOUBLESKY:
          begin
            info.doubleSky := true;
          end;
        MCMD_LIGHTNING:
          begin
            info.lightning := true;
          end;
        MCMD_FADETABLE:
          begin
            sc.MustGetString;
            info.fadetable := W_GetNumForName(sc._String);
          end;
        MCMD_CD_STARTTRACK,
        MCMD_CD_END1TRACK,
        MCMD_CD_END2TRACK,
        MCMD_CD_END3TRACK,
        MCMD_CD_INTERTRACK,
        MCMD_CD_TITLETRACK:
          begin
            sc.MustGetInteger;
            cd_NonLevelTracks[mcmdValue - MCMD_CD_STARTTRACK] := sc._Integer;
          end;
      end;
    end;
    if map > mapMax then
      mapMax := map;
  end;
  sc.Free;
  MapCmdNames.Free;
  MapCount := mapMax;
end;

//==========================================================================
//
// P_QualifyMap
//
//==========================================================================

function P_QualifyMap(map: integer): integer;
begin
  if (map < 1) or (map > MapCount) then
    result := 0
  else
    result := map;
end;


//==========================================================================
//
// P_GetMapCluster
//
//==========================================================================

function P_GetMapCluster(map: integer): integer;
begin
  result := MapInfo[P_QualifyMap(map)].cluster;
end;

//==========================================================================
//
// P_GetMapCDTrack
//
//==========================================================================

function P_GetMapCDTrack(map: integer): integer;
begin
  result := MapInfo[P_QualifyMap(map)].cdTrack;
end;

//==========================================================================
//
// P_GetMapWarpTrans
//
//==========================================================================

function P_GetMapWarpTrans(map: integer): integer;
begin
  result := MapInfo[P_QualifyMap(map)].warpTrans;
end;

//==========================================================================
//
// P_GetMapNextMap
//
//==========================================================================

function P_GetMapNextMap(map: integer): integer;
begin
  result := MapInfo[P_QualifyMap(map)].nextMap;
end;

//==========================================================================
//
// P_TranslateMap
//
// Returns the actual map number given a warp map number.
//
//==========================================================================

function P_TranslateMap(map: integer): integer;
var
  i: integer;
begin
  for i := 1 to 98 do // Make this a macro
  begin
    if MapInfo[i].warpTrans = map then
    begin
      result := i;
      exit;
    end;
  end;
  // Not found
  result := -1;
end;

//==========================================================================
//
// P_GetMapSky1Texture
//
//==========================================================================

function P_GetMapSky1Texture(map: integer): integer;
begin
  result := MapInfo[P_QualifyMap(map)].sky1Texture;
end;

//==========================================================================
//
// P_GetMapSky2Texture
//
//==========================================================================

function P_GetMapSky2Texture(map: integer): integer;
begin
  result := MapInfo[P_QualifyMap(map)].sky2Texture;
end;

//==========================================================================
//
// P_GetMapSky1ScrollDelta
//
//==========================================================================

function P_GetMapSky1ScrollDelta(map: integer): fixed_t;
begin
  result := MapInfo[P_QualifyMap(map)].sky1ScrollDelta;
end;

//==========================================================================
//
// P_GetMapSky2ScrollDelta
//
//==========================================================================

function P_GetMapSky2ScrollDelta(map: integer): fixed_t;
begin
  result := MapInfo[P_QualifyMap(map)].sky2ScrollDelta;
end;

//==========================================================================
//
// P_GetMapDoubleSky
//
//==========================================================================

function P_GetMapDoubleSky(map: integer): boolean;
begin
  result := MapInfo[P_QualifyMap(map)].doubleSky;
end;

//==========================================================================
//
// P_GetMapLightning
//
//==========================================================================

function P_GetMapLightning(map: integer): boolean;
begin
  result := MapInfo[P_QualifyMap(map)].lightning;
end;

//==========================================================================
//
// P_GetMapFadeTable
//
//==========================================================================

function P_GetMapFadeTable(map: integer): integer;
begin
  result := MapInfo[P_QualifyMap(map)].fadetable;
end;

//==========================================================================
//
// P_GetMapSongLump
//
//==========================================================================

function P_GetMapSongLump(map: integer): string;
begin
  if strupper(MapInfo[P_QualifyMap(map)].songLump) = strupper(DEFAULT_SONG_LUMP) then
    result := ''
  else
    result := MapInfo[P_QualifyMap(map)].songLump;
end;

//==========================================================================
//
// P_PutMapSongLump
//
//==========================================================================

procedure P_PutMapSongLump(map: integer; const lumpName: string);
begin
  if (map < 1) or (map > MapCount) then
    exit;

  MapInfo[map].songLump := lumpName;
end;

//==========================================================================
//
// P_GetCDStartTrack
//
//==========================================================================

function P_GetCDStartTrack: integer;
begin
  result := cd_NonLevelTracks[MCMD_CD_STARTTRACK - MCMD_CD_STARTTRACK];
end;

//==========================================================================
//
// P_GetCDEnd1Track
//
//==========================================================================

function P_GetCDEnd1Track: integer;
begin
  result := cd_NonLevelTracks[MCMD_CD_END1TRACK - MCMD_CD_STARTTRACK];
end;

//==========================================================================
//
// P_GetCDEnd2Track
//
//==========================================================================

function P_GetCDEnd2Track: integer;
begin
  result := cd_NonLevelTracks[MCMD_CD_END2TRACK - MCMD_CD_STARTTRACK];
end;

//==========================================================================
//
// P_GetCDEnd3Track
//
//==========================================================================

function P_GetCDEnd3Track: integer;
begin
  result := cd_NonLevelTracks[MCMD_CD_END3TRACK - MCMD_CD_STARTTRACK];
end;

//==========================================================================
//
// P_GetCDIntermissionTrack
//
//==========================================================================

function P_GetCDIntermissionTrack: integer;
begin
  result := cd_NonLevelTracks[MCMD_CD_INTERTRACK - MCMD_CD_STARTTRACK];
end;

//==========================================================================
//
// P_GetCDTitleTrack
//
//==========================================================================

function P_GetCDTitleTrack: integer;
begin
  result := cd_NonLevelTracks[MCMD_CD_TITLETRACK - MCMD_CD_STARTTRACK];
end;

//==========================================================================
//
// P_Init
//
//==========================================================================

procedure P_Init;
begin
  P_InitMapInfo;
  P_InitSwitchList;
  P_InitAnimations;
  P_InitFTAnims; // Init flat and texture animations
  P_InitLava;
  R_InitSprites(sprnames);
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

