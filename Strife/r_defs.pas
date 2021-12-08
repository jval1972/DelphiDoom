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
// DESCRIPTION:
//      Refresh/rendering module, shared data struct definitions.
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit r_defs;

interface

uses
  d_delphi,
  doomdef,
  tables,
{$IFNDEF OPENGL}
  t_main,
{$ENDIF}
// Some more or less basic data types
// we depend on.
  m_fixed,
// We rely on the thinker data struct
// to handle sound origins in sectors.
  d_think,
// SECTORS do store MObjs anyway.
  p_mobj_h,
{$IFNDEF OPENGL}
  r_range,  // JVAL: 3d Floors
  r_visplanes, // JVAL: 3d Floors
{$ENDIF}
  w_wad;

// Silhouette, needed for clipping Segs (mainly)
// and sprites representing things.
const
  SIL_NONE = 0;
  SIL_BOTTOM = 1;
  SIL_TOP = 2;
  SIL_BOTH = 3;

  MAXDRAWSEGS = $10000;

var
  needsbackscreen: boolean = false;

const
  NUMCOLORMAPS = 32;


type
//
// INTERNAL MAP TYPES
//  used by play and refresh
//

//
// Your plain vanilla vertex.
// Note: transformed values not buffered locally,
//  like some DOOM-alikes ("wt", "WebView") did.
//
  vertex_t = packed record
    x: fixed_t;
    y: fixed_t;
    amvalidcount: integer;
  end;
  Pvertex_t = ^vertex_t;
  vertex_tArray = packed array[0..$FFFF] of vertex_t;
  Pvertex_tArray = ^vertex_tArray;

// Each sector has a degenmobj_t in its center
//  for sound origin purposes.
// I suppose this does not handle sound from
//  moving objects (doppler), because
//  position is prolly just buffered, not
//  updated.
  degenmobj_t = packed record
    thinker: thinker_t; // not used for anything
    x: fixed_t;
    y: fixed_t;
    z: fixed_t;
  end;
  Pdegenmobj_t = ^degenmobj_t;

  Pline_t = ^line_t;
  Pline_tArray = ^line_tArray;
  Pline_tPArray = ^line_tPArray;

  Pmsecnode_t = ^msecnode_t;

//
// The SECTORS record, at runtime.
// Stores things/mobjs.
//
  Psector_t = ^sector_t;
  sector_t = packed record
    floorheight: fixed_t;
    ceilingheight: fixed_t;
    floorpic: smallint;
    ceilingpic: smallint;
    lightlevel: smallint;
    special: smallint;
    oldspecial: smallint;
    tag: smallint;

    // 0 = untraversed, 1,2 = sndlines -1
    soundtraversed: integer;

    // thing that made a sound (or null)
    soundtarget: Pmobj_t;

    // mapblock bounding box for height changes
    blockbox: array[0..3] of integer;

    // origin for any sounds played by the sector
    soundorg: degenmobj_t;

    // if == validcount, already checked
    validcount: integer;

    // list of mobjs in sector
    thinglist: Pmobj_t;

    // thinker_t for reversable actions
    floordata: pointer;    // jff 2/22/98 make thinkers on
    ceilingdata: pointer;  // floors, ceilings, lighting,
    lightingdata: pointer; // independent of one another

  // jff 2/26/98 lockout machinery for stairbuilding
    stairlock: integer;   // -2 on first locked -1 after thinker done 0 normally
    prevsec: integer;     // -1 or number of sector for previous step
    nextsec: integer;     // -1 or number of next step sector

    linecount: integer;
    lines: Pline_tPArray; // [linecount] size

    floor_xoffs: fixed_t;
    floor_yoffs: fixed_t;
    ceiling_xoffs: fixed_t;
    ceiling_yoffs: fixed_t;

    touching_thinglist: Pmsecnode_t;  // phares 3/14/98

    heightsec: integer;

    floorlightsec: integer;
    ceilinglightsec: integer;

    topmap: integer;
    midmap: integer;
    bottommap: integer;

    renderflags: LongWord;
    flags: LongWord;

    iSectorID: integer; // JVAL: 3d floors

    // JVAL: 3d floors
    midsec: integer;
    midline: integer;
    // JVAL: Slopes
    fa, fb, fd, fic, ca, cb, cd, cic: float;
    slopesec: Psector_t;
    slopeline: Pline_t;
    // JVAL: sector affectees
    num_saffectees: integer;
    saffectees: PIntegerArray;
    // JVAL: sector gravity (VERSION 204)
    gravity: fixed_t;
    floorangle: angle_t; // JVAL: 20200221 - Texture angle
    flooranglex: fixed_t; // JVAL: 20201229 - Texture angle rover
    floorangley: fixed_t; // JVAL: 20201229 - Texture angle rover
    ceilingangle: angle_t; // JVAL: 20200221 - Texture angle
    ceilinganglex: fixed_t; // JVAL: 20201229 - Texture angle rover
    ceilingangley: fixed_t; // JVAL: 20201229 - Texture angle rover
{$IFDEF OPENGL}
    floorlightlevel: smallint;
    ceilinglightlevel: smallint;
    floor_validcount: integer;
    ceil_validcount: integer;
    highestfloor_height: integer;
    highestfloor_lightlevel: integer;
    highestfloor_picnum: integer;
    lowestceil_height: integer;
    lowestceil_lightlevel: integer;
    lowestceil_picnum: integer;
    no_toptextures: boolean;
    no_bottomtextures: boolean;
{$ELSE}
    // [kb] For R_WiggleFix
    cachedheight: integer;
    scaleindex: integer;
    // JVAL: 20201225 - Speed up maps with large number of slopes
    floorvisslope: integer;
    ceilingvisslope: integer;
{$ENDIF}
  end;
  sector_tArray = packed array[0..$FFFF] of sector_t;
  Psector_tArray = ^sector_tArray;

  msecnode_t = record
    m_sector: Psector_t;  // a sector containing this object
    m_thing: Pmobj_t;     // this object
    m_tprev: Pmsecnode_t; // prev msecnode_t for this thing
    m_tnext: Pmsecnode_t; // next msecnode_t for this thing
    m_sprev: Pmsecnode_t; // prev msecnode_t for this sector
    m_snext: Pmsecnode_t; // next msecnode_t for this sector
    visited: boolean;     // killough 4/4/98, 4/7/98: used in search algorithms
  end;
//
// The SideDef.
//

  side_t = packed record
    // add this to the calculated texture column
    textureoffset: fixed_t;

    // add this to the calculated texture top
    rowoffset: fixed_t;

    // Texture indices.
    // We do not maintain names here.
    toptexture: smallint;
    bottomtexture: smallint;
    midtexture: smallint;

    // Sector the SideDef is facing.
    sector: Psector_t;
  end;
  Pside_t = ^side_t;
  side_tArray = packed array[0..$FFFF] of side_t;
  Pside_tArray = ^side_tArray;

//
// Move clipping aid for LineDefs.
//
  slopetype_t = (
    ST_HORIZONTAL,
    ST_VERTICAL,
    ST_POSITIVE,
    ST_NEGATIVE
  );

  line_t = packed record
    // Vertices, from v1 to v2.
    v1: Pvertex_t;
    v2: Pvertex_t;

    // Precalculated v2 - v1 for side checking.
    dx: fixed_t;
    dy: fixed_t;

    // Animation related.
    flags: word;
    special: smallint;
    tag: smallint;

    // Visual appearance: SideDefs.
    //  sidenum[1] will be -1 if one sided
    sidenum: packed array[0..1] of integer;  // JVAL glbsp was smallint

    // Neat. Another bounding box, for the extent
    //  of the LineDef.
    bbox: packed array[0..3] of fixed_t;

    // To aid move clipping.
    slopetype: slopetype_t;

    // Front and back sector.
    // Note: redundant? Can be retrieved from SideDefs.
    frontsector: Psector_t;
    backsector: Psector_t;

    // if == validcount, already checked
    validcount: integer;

    // thinker_t for reversable actions
    specialdata: pointer;
    renderflags: LongWord;

    clslopestep: array[0..1] of float; // JVAL: Slopes
    flslopestep: array[0..1] of float; // JVAL: Slopes
  end;
  PPline_t = ^Pline_t;
  line_tArray = packed array[0..$FFFF] of line_t;
  line_tPArray = packed array[0..$FFFF] of Pline_t;

const
  // Line rendering flags
  LRF_ISOLATED = 1;
  LRF_TRANSPARENT = 2;
  LRF_SLOPED = 4; // JVAL: Slopes

const
  // Sector rendering flags
  SRF_RIPPLE_FLOOR = 1;
  SRF_RIPPLE_CEILING = 2;
  SRF_RIPPLE = SRF_RIPPLE_FLOOR or SRF_RIPPLE_CEILING;
  SRF_NO_INTERPOLATE = 4;
  SRF_FFLOOR = 8; // JVAL: 3d Floors
  SRF_DONOTDRAW = 16; // JVAL: 3d Floors
  SRF_SLOPEFLOOR = 32; // JVAL: Slopes
  SRF_SLOPECEILING = 64; // JVAL: Slopes
  SRF_SLOPED = SRF_SLOPEFLOOR + SRF_SLOPECEILING; // JVAL: Slopes
  SRF_INTERPOLATE_ROTATE = 512;
  SRF_INTERPOLATE_FLOORSLOPE = 1024;
  SRF_INTERPOLATE_CEILINGSLOPE = 2048;
  SRF_FOG = 4096;

const
  // Vissprite render flags
  VSF_TRANSPARENCY = 1;

//
// A SubSector.
// References a Sector.
// Basically, this is a list of LineSegs,
//  indicating the visible walls that define
//  (all or some) sides of a convex BSP leaf.
//
const
  SSF_CENTROIDCALCED = 1;
  SSF_BRIDGE = 2;

type
  subsector_t = packed record
    sector: Psector_t;
    numlines: LongWord; // JVAL glbsp (was word)
    firstline: LongWord;// JVAL glbsp (was word)
    x, y: fixed_t; // JVAL 3d Floors (Subsector Centroid)
    flags: LongWord;
  end;
  Psubsector_t = ^subsector_t;
  subsector_tArray = packed array[0..$FFFF] of subsector_t;
  Psubsector_tArray = ^subsector_tArray;

//
// The LineSeg.
//
  seg_t = packed record
    v1: Pvertex_t;
    v2: Pvertex_t;

    offset: fixed_t;

    angle: angle_t;

    sidedef: Pside_t;
    linedef: Pline_t;

    // Sector references.
    // Could be retrieved from linedef, too.
    // backsector is NULL for one sided lines
    frontsector: Psector_t;
    backsector: Psector_t;
{$IFDEF OPENGL}
    length: single;
    iSegID: integer;
{$ELSE}
    map_length: integer;
    inv_length: double;
{$ENDIF}
    miniseg: boolean;
  end;
  Pseg_t = ^seg_t;
  seg_tArray = packed array[0..$FFFF] of seg_t;
  Pseg_tArray = ^seg_tArray;

//
// BSP node.
//
  node_t = packed record
    // Partition line.
    x: fixed_t;
    y: fixed_t;
    dx: fixed_t;
    dy: fixed_t;

    // Bounding box for each child.
    bbox: packed array[0..1, 0..3] of fixed_t;

    // If NF_SUBSECTOR its a subsector.
    children: packed array[0..1] of LongWord; // JVAL glbsp
  end;
  Pnode_t = ^node_t;
  node_tArray = packed array[0..$FFFF] of node_t;
  Pnode_tArray = ^node_tArray;

// posts are runs of non masked source pixels
  post_t = packed record
    topdelta: byte; // -1 is the last post in a column
    length: byte;   // length data bytes follows
  end;
  Ppost_t = ^post_t;

// column_t is a list of 0 or more post_t, (byte)-1 terminated
  column_t = post_t;
  Pcolumn_t = ^column_t;

//
// OTHER TYPES
//

  drawseg_t = packed record
    curline: Pseg_t;
    x1: integer;
    x2: integer;

    scale1: fixed_t;
    scale2: fixed_t;
    scalestep: fixed_t;
    {$IFNDEF OPENGL}
    scale_dbl: Double;      // JVAL: 3d Floors
    scalestep_dbl: Double;  // JVAL: 3d Floors
    use_double: boolean;
    {$ENDIF}

    // 0=none, 1=bottom, 2=top, 3=both
    silhouette: integer;

    // do not clip sprites above this
    bsilheight: fixed_t;

    // do not clip sprites below this
    tsilheight: fixed_t;

    // Pointers to lists for sprite clipping,
    //  all three adjusted so [x1] is first value.
    sprtopclip: PSmallIntArray;
    sprbottomclip: PSmallIntArray;
    maskedtexturecol: PSmallIntArray;
    // JVAL: 3d Floors
    thicksidecol: PSmallintArray;
    midsec: Psector_t;
    midside: Pside_t;
{$IFNDEF OPENGL}
    midvis: Pvisplane3d_t;
    midsiderange: midsiderange_t;
{$ENDIF}
  end;
  Pdrawseg_t = ^drawseg_t;
  drawsegsbuffer_t = array[0..$FFF] of Pdrawseg_t;
  Pdrawsegsbuffer_t = ^ drawsegsbuffer_t;

// Patches.
// A patch holds one or more columns.
// Patches are used for sprites and all masked pictures,
// and we compose textures from the TEXTURE1/2 lists
// of patches.
  patch_t = packed record
    width: smallint; // bounding box size
    height: smallint;
    leftoffset: smallint; // pixels to the left of origin
    topoffset: smallint;  // pixels below the origin
    columnofs: array[0..7] of integer; // only [width] used
    // the [0] is &columnofs[width]
  end;
  Ppatch_t = ^patch_t;
  patch_tArray = packed array[0..$FFFF] of patch_t; // JVAL: DO NOT USE
  Ppatch_tArray = ^patch_tArray;
  patch_tPArray = packed array[0..$FFFF] of Ppatch_t;
  Ppatch_tPArray = ^patch_tPArray;

// A vissprite_t is a thing
//  that will be drawn during a refresh.
// I.e. a sprite object that is partly visible.
  Pvissprite_t = ^vissprite_t;
  vissprite_t = packed record
    x1: integer;
    x2: integer;

    {$IFNDEF OPENGL}
    // for line side calculation
    gx: fixed_t;
    gy: fixed_t;

    // global bottom / top for silhouette clipping
    gz: fixed_t;
    gzt: fixed_t;

    footclip: fixed_t;  // foot clipping

    // horizontal position of x1
    startfrac: fixed_t;
    {$ENDIF}
    scale: fixed_t;

    {$IFNDEF OPENGL}
    // negative if flipped
    xiscale: fixed_t;
    {$ENDIF}
    texturemid: fixed_t;
    {$IFNDEF OPENGL}
    texturemid2: fixed_t; // JVAL For light boost
    heightsec: integer;   // killough 3/27/98: height sector for underwater/fake ceiling support
    voxelflag: integer;   // JVAL voxel support (1 for sprites, 0 for skipped spites (only light), 1.... for voxels
    vx1, vx2: integer;
    drawn: Boolean;       // JVAL 3d Floors
    ceilingz: fixed_t;    // JVAL 3d Floors
    {$ENDIF}
    patch: integer;

    // for color translation and shadow draw,
    //  maxbright frames as well
{$IFNDEF OPENGL} // JVAL: 3d Floors
    colormap: PByteArray;
    renderflags: LongWord;
{$ENDIF}
    mobjflags: LongWord;
    mobjflags_ex: LongWord;
    mobjflags2_ex: LongWord;
    mo: Pmobj_t;
{$IFDEF OPENGL}
    flip: boolean;
{$ENDIF}
    infoscale: fixed_t;
    fog: boolean; // JVAL: Mars fog sectors
  end;
  visspritebuffer_t = array[0..$FFFF] of Pvissprite_t;
  visspritebuffer_p = ^visspritebuffer_t;

//
// Sprites are patches with a special naming convention
//  so they can be recognized by R_InitSprites.
// The base name is NNNNFx or NNNNFxFx, with
//  x indicating the rotation, x = 0, 1-7.
// The sprite and frame specified by a thing_t
//  is range checked at run time.
// A sprite is a patch_t that is assumed to represent
//  a three dimensional object and may have multiple
//  rotations pre drawn.
// Horizontal flipping is used to save space,
//  thus NNNNF2F5 defines a mirrored patch.
// Some sprites will only have one picture used
// for all views: NNNNF0
//
  spriteframe_t = packed record
    // If false use 0 for any position.
    // Note: as eight entries are available,
    //  we might as well insert the same name eight times.
    rotate: integer;

    // Lump to use for view angles 0-7.
    lump: array[0..31] of integer; // JVAL: Up to 32 sprite rotations

    // Flip bit (1 = flip) to use for view angles 0-7.
    flip: array[0..31] of boolean; // JVAL: Up to 32 sprite rotations
  end;
  Pspriteframe_t = ^spriteframe_t;
  spriteframe_tArray = packed array[0..$FFFF] of spriteframe_t;
  Pspriteframe_tArray = ^spriteframe_tArray;


//
// A sprite definition:
//  a number of animation frames.
//
  spritedef_t = packed record
    numframes: integer;
    spriteframes: Pspriteframe_tArray;
  end;
  Pspritedef_t = ^spritedef_t;
  spritedef_tArray = packed array[0..$FFFF] of spritedef_t;
  Pspritedef_tArray = ^spritedef_tArray;

//
// Texture definition.
// Each texture is composed of one or more patches,
// with patches being lumps stored in the WAD.
// The lumps are referenced by number, and patched
// into the rectangular texture space using origin
// and possibly other attributes.
//
type
  mappatch_t = record
    originx: smallint;
    originy: smallint;
    patch: smallint;
  end;
  Pmappatch_t = ^mappatch_t;

  mappatch2_t = record
    originx: smallint;
    originy: smallint;
    patch: smallint;
    stepdir: smallint;
    colormap: smallint;
  end;
  Pmappatch2_t = ^mappatch2_t;

//
// Texture definition.
// A DOOM wall texture is a list of patches
// which are to be combined in a predefined order.
//
  maptexture_t = record
    name: char8_t;
    masked: integer;
    width: smallint;
    height: smallint;
    patchcount: smallint;
    patches: array[0..0] of mappatch_t;
  end;
  Pmaptexture_t = ^maptexture_t;

  maptexture2_t = packed record
    name: char8_t;
    masked: integer;
    width: smallint;
    height: smallint;
    columndirectory: LongWord; // unused
    patchcount: smallint;
    patches: array[0..0] of mappatch2_t;
  end;
  Pmaptexture2_t = ^maptexture2_t;

// A single patch from a texture definition,
//  basically a rectangular area within
//  the texture rectangle.
  texpatch_t = packed record
    // Block origin (allways UL),
    // which has allready accounted
    // for the internal origin of the patch.
    originx: integer;
    originy: integer;
    patch: integer;
  end;
  Ptexpatch_t = ^texpatch_t;

// A maptexturedef_t describes a rectangular texture,
//  which is composed of one or more mappatch_t structures
//  that arrange graphic patches.
  texture_t = packed record
    // Keep name for switch changing, etc.
    name: char8_t;
    width: smallint;
    height: smallint;
    factorbits: integer;

    // All the patches[patchcount]
    //  are drawn back to front into the cached texture.
    {$IFNDEF OPENGL}
    texture32: PTexture;  // JVAL: External texture reference
    {$ENDIF}
    patchcount: smallint;
    patches: array[0..0] of texpatch_t;
  end;
  Ptexture_t = ^texture_t;
  texture_tPArray = array[0..$FFFF] of Ptexture_t;
  Ptexture_tPArray = ^texture_tPArray;

  // JVAL added flat record
  flat_t = packed record
    // Keep name for switch changing, etc.
    name: char8_t;
    width: smallint;  // Optional ?? JVAL SOS maybe removed?
    height: smallint;
    {$IFNDEF OPENGL}
    flat32: PTexture; // External texture reference
    {$ENDIF}
    terraintype: integer; // JVAL: 9 December 2007, Added terrain types
    translation: integer;
    lump: integer;
    size: integer;
  end;
  Pflat_t = ^flat_t;
  flatPArray = array[0..$FFFF] of Pflat_t;
  PflatPArray = ^flatPArray;

var
  numspritelumps: integer;

  texturewidth: PIntegerArray;

  texturecolumnlump: PSmallIntPArray;
  texturecolumnofs: PIntegerPArray; // PWordPArray; //64k
  texturecomposite: PBytePArray;

//
// MAPTEXTURE_T CACHING
// When a texture is first needed,
//  it counts the number of composite columns
//  required in the texture and allocates space
//  for a column directory and any new columns.
// The directory will simply point inside other patches
//  if there is only one patch in a given column,
//  but any columns with multiple patches
//  will have new column_ts generated.
//

implementation

end.

