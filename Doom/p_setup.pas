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
//   Setup a game, startup stuff.
//   Do all the WAD I/O, get map description,
//   set up initial state and misc. LUTs.
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
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
  p_udmf,
  r_defs;

//==============================================================================
//
// P_GetMapName
//
//==============================================================================
function P_GetMapName(const episode, map: integer): string;

//==============================================================================
// P_SetupLevel
//
// NOT called by W_Ticker. Fixme.
//
//==============================================================================
procedure P_SetupLevel(episode, map, playermask: integer; skill: skill_t);

//==============================================================================
// P_Init
//
// Called by startup code.
//
//==============================================================================
procedure P_Init;

//==============================================================================
//
// P_ShutDown
//
//==============================================================================
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

  // Vanilla nodes
  numvanillasegs: integer;
  vanillasegs: Pvanillaseg_tArray;

  numvanillasubsectors: integer;
  vanillasubsectors: Pvanillasubsector_tArray;

  numvanillanodes: integer;
  vanillanodes: Pvanillanode_tArray;

  hasvanillanodes: boolean;

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
  // for thing chains
  vanillablocklinks: Pmobj_tPArray;

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
  udeathmatchstarts: array[0..MAX_DEATHMATCH_STARTS - 1] of extrathing_t;
  deathmatch_p: integer;

  playerstarts: array[0..MAXPLAYERS - 1] of mapthing_t;

//==============================================================================
//
// P_GameValidThing
//
//==============================================================================
function P_GameValidThing(const doomdnum: integer): boolean;

var
  useglnodesifavailable: boolean;

var
  hidedoublicatedbarrels: boolean = true;

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
  m_rnd,
  m_bbox,
  m_sha1,
  g_game,
  i_system,
  w_wad,
  info,
  info_h,
  p_blockmap,
  p_local,
  p_mobj,
  p_tick,
  p_spec,
  p_switch,
  p_inter,
  p_maputl,
  p_adjust,
  p_bridge,
  p_acs,
  p_animdefs,
  p_3dfloors, // JVAL: 3d Floors
  p_slopes,   // JVAL: Slopes
  p_easyslope,
  p_easyangle, // JVAL: 20201229 - Easy floor and ceiling texture angle
  p_easywind, // JVAL: 20220222 - Easy wind
  p_affectees,
  p_musinfo,
  po_man,
  ps_main,    // JVAL: Script Events
  r_data,
  r_things,
  mt_utils,
  r_colormaps,
{$IFNDEF OPENGL}
  r_cache_main,
  r_segs2,
{$ENDIF}
  r_intrpl,
{$IFDEF OPENGL}
  gl_setup,
  gl_render,  // JVAL OPENGL
{$ENDIF}
  r_subsectors,
  nd_main,
  udmf_spec,
  s_sound,
  doomstat;

//==============================================================================
//
// P_LoadVertexes
//
//==============================================================================
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
    li.interpvalidcount := 0;

    inc(ml);
    inc(li);
  end;

  dx := maxx - minx;
  dy := maxy - miny;

  largemap := (dx < -32767) or (dx > 32767) or (dy < -32767) or (dy > 32767);

  // Free buffer memory.
  Z_Free(data);
end;

//==============================================================================
//
// P_LoadSegs
//
//==============================================================================
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

//==============================================================================
//
// P_LoadSubsectors
//
//==============================================================================
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

//==============================================================================
//
// P_LoadSectors
//
//==============================================================================
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
    // https://www.doomworld.com/forum/topic/118126-doom-sector-light-levels/
    // JVAL: 20220116 -> If sector lightlevel in WAD is 256 change it to 255,
    //                   do not treat as fog sector
    if ms.lightlevel = 256 then
      ms.lightlevel := $FF;
    ss.lightlevel := ms.lightlevel;
    ss.lightninglightlevel := 255; // JVAL: 20220218 - Lightning
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
    if ss.lightlevel >= 1000 then // JVAL: Fog sectors
    begin
      ss.renderflags := ss.renderflags or SRF_FOG;
      ss.lightlevel := ss.lightlevel - 1000;
    end;
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
{$ENDIF}
{$IFNDEF OPENGL}
    // [kb] For R_WiggleFix
    ss.cachedheight := 0;
    ss.scaleindex := 0;
    // JVAL: 20201225 - Speed up maps with large number of slopes
    ss.floorvisslope := -1;
    ss.ceilingvisslope := -1;
{$ENDIF}
    // killough 4/11/98 sector used to get ceiling lighting:
    ss.ceilinglightsec := -1;

    // JVAL: 20220327 - Group sectors to interpolate groups
    ss.interpolate_group := i mod NUM_SECTOR_INTERPOLATE_GROUPS;

    ss.iSectorID := i; // JVAL: 3d Floors
    inc(ms);
    inc(ss);
  end;

  Z_Free(data);
end;

//==============================================================================
//
// P_LoadNodes
//
//==============================================================================
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

//==============================================================================
//
// P_GameValidThing
//
//==============================================================================
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
  // Do not spawn cool, new monsters if !commercial
  if gamemode <> commercial then
  begin
    case doomdnum of
      68, // Arachnotron
      64, // Archvile
      88, // Boss Brain
      89, // Boss Shooter
      69, // Hell Knight
      67, // Mancubus
      71, // Pain Elemental
      65, // Former Human Commando
      66, // Revenant
      84: // Wolf SS
        begin
          result := false;
          exit;
        end;
    end;
  end;

  result := true;
end;

//==============================================================================
//
// P_DontDrawDuplicateThings
//
//==============================================================================
procedure P_DontDrawDuplicateThings(const _type: integer);
var
  i, j, count: integer;
  mobjs: Pmobj_tPArray;
  think: Pthinker_t;
begin
  count := 0;
  think := thinkercap.next;
  while think <> @thinkercap do
  begin
    if @think._function.acp1 = @P_MobjThinker then
      if Pmobj_t(think)._type = _type then
        inc(count);
    think := think.next;
  end;

  if count > 0 then
  begin
    mobjs := Z_Malloc(count * SizeOf(Pmobj_t), PU_STATIC, nil);
    i := 0;
    think := thinkercap.next;
    while think <> @thinkercap do
    begin
      if @think._function.acp1 = @P_MobjThinker then
        if Pmobj_t(think)._type = _type then
        begin
          mobjs[i] := Pmobj_t(think);
          inc(i);
        end;
      think := think.next;
    end;

    for i := 1 to count - 1 do
      for j := 0 to i - 1 do
        if (mobjs[j].x = mobjs[i].x) and
           (mobjs[j].y = mobjs[i].y) and
           (mobjs[j].z = mobjs[i].z) then
           mobjs[j].flags2_ex := mobjs[j].flags2_ex or MF2_EX_DONTDRAW;

    Z_Free(mobjs);
  end;
end;

//==============================================================================
//
// P_CheckThings
//
//==============================================================================
procedure P_CheckThings;
begin
  if hidedoublicatedbarrels then
    P_DontDrawDuplicateThings(Ord(MT_BARREL));
end;

//==============================================================================
//
// P_LoadThings
//
//==============================================================================
procedure P_LoadThings(lump: integer);
var
  data: pointer;
  i: integer;
  mt: Pmapthing_t;
  numthings: integer;
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

  if hasudmfdata then
  begin
    for i := 0 to numthings - 1 do
    begin
      if P_GameValidThing(mt._type) then // Do spawn all other stuff.
        P_SpawnMapThing(mt, @udmfthings[i]);

      inc(mt);
    end;
  end
  else
  begin
    for i := 0 to numthings - 1 do
    begin
      if P_GameValidThing(mt._type) then // Do spawn all other stuff.
        P_SpawnMapThing(mt, nil);

      inc(mt);
    end;
  end;

  Z_Free(data);

  P_CheckThings;
end;

//==============================================================================
//
// P_LoadLineDefs
// Also counts secret lines for intermissions.
//
//==============================================================================
procedure P_LoadLineDefs(lump: integer);
var
  data: pointer;
  i: integer;
  mld: Pmaplinedef_t;
  ld: Pline_t;
  v1: Pvertex_t;
  v2: Pvertex_t;
  sz: integer;
  sha: string;
begin
  sz := W_LumpLength(lump);
  numlines := sz div SizeOf(maplinedef_t);
  lines := Z_Malloc(numlines * SizeOf(line_t), PU_LEVEL, nil);
  ZeroMemory(lines, numlines * SizeOf(line_t));
  data := W_CacheLumpNum(lump, PU_STATIC);

  mld := Pmaplinedef_t(data);

  if numlines = 1764 then
  begin
    sha := strupper(readablestring(SHA1_CalcSHA1Buf(data^, sz)));
    if (sha = '86C7KC17C14E1B684A49A2481S9AA9LFB8FF4') or // ver 1.2 - 1.9
       (sha = 'SAF04WA061FB7AF3E2BF5KC89349D25F5A9') then // ver 1.1
    begin
      for i := 0 to numlines - 1 do
      begin
        PWord(@mld.flags)^ := PWord(@mld.flags)^ and 511;
        Inc(mld);
      end;
      mld := Pmaplinedef_t(data);
    end;
  end;

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

//==============================================================================
//
// P_LoadSideDefs
//
//==============================================================================
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

//==============================================================================
//
// P_InitVanillaBlockLinks
//
//==============================================================================
procedure P_InitVanillaBlockLinks;
var
  count: integer;
begin
  count := SizeOf(Pmobj_t) * bmapwidth * bmapheight;
  vanillablocklinks := Z_Malloc(count, PU_LEVEL, nil);
  ZeroMemory(vanillablocklinks, count);
end;

//==============================================================================
//
// P_GroupLines
// Builds sector line lists and subsector sector numbers.
// Finds block bounding boxes for sectors.
//
//==============================================================================
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
  vanillaseg: Pvanillaseg_t;
  bbox: array[0..3] of fixed_t;
  block: integer;
begin
  // look up sector number for each subsector - vanilla nodes
  if hasvanillanodes then
    for i := 0 to numvanillasubsectors - 1 do
    begin
      vanillaseg := @vanillasegs[vanillasubsectors[i].firstline];
      vanillasubsectors[i].sector := vanillaseg.sidedef.sector;
    end;

  // look up sector number for each subsector - normal nodes
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
    if internalblockmapformat then
      block := MapBlockIntY(int64(bbox[BOXTOP]) - int64(bmaporgy) + MAXRADIUS)
    else
      block := MapBlockInt(bbox[BOXTOP] - bmaporgy + MAXRADIUS);
    if block >= bmapheight then
      block := bmapheight - 1;
    sector.blockbox[BOXTOP] := block;

    if internalblockmapformat then
      block := MapBlockIntY(int64(bbox[BOXBOTTOM]) - int64(bmaporgy) - MAXRADIUS)
    else
      block := MapBlockInt(bbox[BOXBOTTOM] - bmaporgy - MAXRADIUS);
    if block < 0 then
      block := 0;
    sector.blockbox[BOXBOTTOM] := block;

    if internalblockmapformat then
      block := MapBlockIntX(int64(bbox[BOXRIGHT]) - int64(bmaporgx) + MAXRADIUS)
    else
      block := MapBlockInt(bbox[BOXRIGHT] - bmaporgx + MAXRADIUS);
    if block >= bmapwidth then
      block := bmapwidth - 1;
    sector.blockbox[BOXRIGHT] := block;

    if internalblockmapformat then
      block := MapBlockIntX(int64(bbox[BOXLEFT]) - int64(bmaporgx) - MAXRADIUS)
    else
      block := MapBlockInt(bbox[BOXLEFT] - bmaporgx - MAXRADIUS);
    if block < 0 then
      block := 0;
    sector.blockbox[BOXLEFT] := block;

    inc(sector);
  end;

  li := @lines[0];
  for i := 0 to numlines - 1 do
  begin
    if li.special = 242 then
    begin
      if sides[li.sidenum[0]].toptexture < 0 then
        sides[li.sidenum[0]].sector.topmap := -sides[li.sidenum[0]].toptexture - 1;
      if sides[li.sidenum[0]].bottomtexture < 0 then
        sides[li.sidenum[0]].sector.bottommap := -sides[li.sidenum[0]].bottomtexture - 1;
      if sides[li.sidenum[0]].midtexture < 0 then
        sides[li.sidenum[0]].sector.midmap := -sides[li.sidenum[0]].midtexture - 1;
    end;
    Inc(li);
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

//==============================================================================
//
// P_GetMapName
//
//==============================================================================
function P_GetMapName(const episode, map: integer): string;
begin
  // find map name
  if gamemode = commercial then
  begin
    if map < 10 then
      sprintf(result,'map0%d', [map])
    else
      sprintf(result,'map%d', [map]);
  end
  else
    sprintf(result, 'E%dM%d', [episode, map]);
end;

// P_LoadVanillaSegs
//
//==============================================================================
procedure P_LoadVanillaSegs(lump: integer);
var
  Data: pointer;
  i: integer;
  ml: Pmapseg_t;
  li: Pvanillaseg_t;
  ldef: Pline_t;
  linedef: integer;
  side: integer;
begin
  numvanillasegs := W_LumpLength(lump) div SizeOf(mapseg_t);
  if numvanillasegs >= 32768 then
  begin
    numvanillasegs := 0;
    exit;
  end;

  vanillasegs := Z_Malloc(numvanillasegs * SizeOf(vanillaseg_t), PU_LEVEL, nil);
  memset(vanillasegs, 0, numvanillasegs * SizeOf(vanillaseg_t));
  data := W_CacheLumpNum(lump, PU_STATIC);

  ml := Pmapseg_t(data);
  for i := 0 to numvanillasegs - 1 do
  begin
    li := @vanillasegs[i];
    li.v1 := @vertexes[ml.v1];
    li.v2 := @vertexes[ml.v2];

    li.angle := _SHL(ml.angle, 16);
    li.offset := _SHL(ml.offset, 16);
    linedef := ml.linedef;
    ldef := @lines[linedef];
    li.linedef := ldef;
    side := ml.side;
    li.sidedef := @sides[ldef.sidenum[side]];
    li.frontsector := sides[ldef.sidenum[side]].sector;
    if ldef.flags and ML_TWOSIDED <> 0 then
      li.backsector := sides[ldef.sidenum[side xor 1]].sector
    else
      li.backsector := nil;
    Inc(ml);
  end;

  Z_Free(data);
end;

// P_LoadVanillaSubsectors
//
//==============================================================================
procedure P_LoadVanillaSubsectors(lump: integer);
var
  data: pointer;
  i: integer;
  ms: Pmapsubsector_t;
  ss: Pvanillasubsector_t;
begin
  numvanillasubsectors := W_LumpLength(lump) div SizeOf(mapsubsector_t);
  if numvanillasubsectors >= 32768 then
  begin
    numvanillasubsectors := 0;
    exit;
  end;

  vanillasubsectors := Z_Malloc(numvanillasubsectors * SizeOf(vanillasubsector_t), PU_LEVEL, nil);
  data := W_CacheLumpNum(lump, PU_STATIC);

  ms := Pmapsubsector_t(data);
  memset(vanillasubsectors, 0, numvanillasubsectors * SizeOf(vanillasubsector_t));

  for i := 0 to numvanillasubsectors - 1 do
  begin
    ss := @vanillasubsectors[i];
    ss.numlines := ms.numsegs;
    ss.firstline := ms.firstseg;
    Inc(ms);
  end;

  Z_Free(data);
end;

//==============================================================================
// P_LoadVanillaNodes
//
// P_LoadNodes
//
//==============================================================================
procedure P_LoadVanillaNodes(lump: integer);
var
  data: pointer;
  i: integer;
  j: integer;
  k: integer;
  mn: Pmapnode_t;
  no: Pvanillanode_t;
begin
  numvanillanodes := W_LumpLength(lump) div SizeOf(mapnode_t);
  if numvanillanodes >= 32768 then
  begin
    numvanillanodes := 0;
    exit;
  end;

  vanillanodes := Z_Malloc(numvanillanodes * SizeOf(vanillanode_t), PU_LEVEL, nil);
  data := W_CacheLumpNum(lump, PU_STATIC);

  mn := Pmapnode_t(data);

  for i := 0 to numvanillanodes - 1 do
  begin
    no := @vanillanodes[i];
    no.x := mn.x * FRACUNIT;
    no.y := mn.y * FRACUNIT;
    no.dx := mn.dx * FRACUNIT;
    no.dy := mn.dy * FRACUNIT;
    for j := 0 to 1 do
    begin
      no.children[j] := mn.children[j];
      for k := 0 to 3 do
        no.bbox[j, k] := smallint(mn.bbox[j, k]) * FRACUNIT;
    end;
    Inc(mn);
  end;

  Z_Free(data);
end;

//==============================================================================
//
// P_SetupLevel
//
//==============================================================================
procedure P_SetupLevel(episode, map, playermask: integer; skill: skill_t);
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
  totalitems := 0;
  totalsecret := 0;

  isgamefreezed := false;

  if not preparingdemoplayback then
    sysrndseed := I_Random;

  wminfo.maxfrags := 0;
  wminfo.partime := 180;

  for i := 0 to MAXPLAYERS - 1 do
  begin
    players[i].killcount := 0;
    players[i].secretcount := 0;
    players[i].itemcount := 0;
    players[i].lastsoundstepx := 0;
    players[i].lastsoundstepy := 0;
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

  po_NumPolyobjs := 0;
  PolyBlockMap := nil;

  numvanillasegs := 0;
  vanillasegs := nil;

  numvanillasubsectors := 0;
  vanillasubsectors := nil;

  numvanillanodes := 0;
  vanillanodes := nil;

  hasvanillanodes := false;

  R_SetupLevel;

  P_InitThinkers;
  P_InitIntercepts;

  // if working with a devlopment map, reload it
  W_Reload;

  // find map name
  lumpname := P_GetMapName(episode, map);

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
  if useglnodesifavailable and not G_NeedsCompatibilityMode and not hasudmfdata then
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
  UDMF_MakeSides;
  P_LoadLineDefs(lumpnum + Ord(ML_LINEDEFS));
  P_LoadBlockMap(lumpnum + Ord(ML_BLOCKMAP));
  P_InitVanillaBlockLinks;
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

  if preparingdemoplayback and (G_PlayingEngineVersion < VERSION110) and (numlines < 32768) and (numsides < 32768) and not hasudmfdata then
  begin
    // JVAL: 20210131 - Load vanilla nodes
    if char8tostring(lumpinfo[lumpnum + Ord(ML_SSECTORS)].name) = 'SSECTORS' then
      P_LoadVanillaSubsectors(lumpnum + Ord(ML_SSECTORS));
    if char8tostring(lumpinfo[lumpnum + Ord(ML_NODES)].name) = 'NODES' then
      P_LoadVanillaNodes(lumpnum + Ord(ML_NODES));
    if char8tostring(lumpinfo[lumpnum + Ord(ML_SEGS)].name) = 'SEGS' then
      P_LoadVanillaSegs(lumpnum + Ord(ML_SEGS));
    hasvanillanodes := (vanillasubsectors <> nil) and (vanillanodes <> nil) and (vanillasegs <> nil);
  end;

  rejectmatrix := W_CacheLumpNum(lumpnum + Ord(ML_REJECT), PU_LEVEL);
  rejectmatrixsize := W_LumpLength(lumpnum + Ord(ML_REJECT));

  UDMF_MakeLines;
  P_GroupLines;

  UDMF_MakeSegs;

  P_3dFloorSetupSegs; // JVAL: 3d Floors

  P_RemoveSlimeTrails;    // killough 10/98: remove slime trails from wad

{$IFNDEF OPENGL}
  R_CalcSectors; // JVAL 20200105 - Check the map boundaries
  R_PrecalcSegs; // https://www.doomworld.com/forum/topic/70288-dynamic-wiggletall-sector-fix-for-fixed-point-software-renderer/?do=findComment&comment=1340433
{$ENDIF}

  P_SlopesSetup;// JVAL: Slopes

  UDMF_MakeSectors;

  P_SetupSectorAffectees;

  if autoadjustmissingtextures then
    P_AdjustMissingTextures;

  P_CalcSubSectorsBridge;

  R_PrecalcPointInSubSector;

  bodyqueslot := 0;
  po_NumPolyobjs := 0;
  deathmatch_p := 0;

  if devparm then
    printf('P_LoadThings()'#13#10);
  P_LoadThings(lumpnum + Ord(ML_THINGS));

  // JVAL: 20201229 - Easy floor and ceiling texture angle
  P_AdjustEasyAngle;

  // JVAL: 20220222 - Easy wind
  P_AdjustEasyWind;

  PO_Init(lumpnum + Ord(ML_THINGS));            // Initialize the polyobjs
  P_LoadACScripts(lumpnum + Ord(ML_BEHAVIOR));  // ACS object code

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

  // Check if the level is a lightning level
  P_InitLightning;
end;

//==============================================================================
//
// P_Init
//
//==============================================================================
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
//
//==============================================================================
procedure P_ShutDown;
begin
  P_ShutDownAnimations;
end;

end.

