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
//  all external data is defined here
//  most of the data is loaded into different structures at run time
//  some internal structures shared by many modules are here
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit doomdata;

interface

uses
// The most basic types we use, portability.

// Some global defines, that configure the game.

// char8_t
  w_wad;

type
//
// Map level types.
// The following data structures define the persistent format
// used in the lumps of the WAD files.
//

// Lump order in a map WAD: each map needs a couple of lumps
// to provide a complete scene geometry description.
  maplumpdesc_t = (
    ML_LABEL,     // A separator, name, ExMx or MAPxx
    ML_THINGS,    // Monsters, items..
    ML_LINEDEFS,  // LineDefs, from editing
    ML_SIDEDEFS,  // SideDefs, from editing
    ML_VERTEXES,  // Vertices, edited and BSP splits generated
    ML_SEGS,      // LineSegs, from LineDefs split by BSP
    ML_SSECTORS,  // SubSectors, list of LineSegs
    ML_NODES,     // BSP nodes
    ML_SECTORS,   // Sectors, from editing
    ML_REJECT,    // LUT, sector-sector visibility
    ML_BLOCKMAP,  // LUT, motion clipping, walls/grid element
    ML_BEHAVIOR   // ACS Object Code
  );

  mapvertex_t = record
    x : smallint;
    y : smallint;
  end;
  Pmapvertex_t = ^mapvertex_t;
  mapvertex_tArray = array[0..$FFFF] of mapvertex_t;
  Pmapvertex_tArray = ^mapvertex_tArray;

// A SideDef, defining the visual appearance of a wall,
// by setting textures and offsets.
  mapsidedef_t = record
    textureoffset: smallint;
    rowoffset: smallint;
    toptexture: char8_t;
    bottomtexture: char8_t;
    midtexture: char8_t;
  // Front sector, towards viewer.
    sector: smallint;
  end;
  Pmapsidedef_t = ^mapsidedef_t;
  mapsidedef_tArray = array[0..$FFFF] of mapsidedef_t;
  Pmapsidedef_tArray = ^mapsidedef_tArray;

// A LineDef, as used for editing, and as input
// to the BSP builder.
  maplinedef_t = record
    v1: smallint;
    v2: smallint;
    flags: smallint;
    special: smallint;
    tag: smallint;
  // sidenum[1] will be -1 if one sided
    sidenum: array[0..1] of smallint;
  end;
  Pmaplinedef_t = ^maplinedef_t;
  maplinedef_tArray = array[0..$FFFF] of maplinedef_t;
  Pmaplinedef_tArray = ^maplinedef_tArray;

//
// LineDef attributes.
//

const
// Solid, is an obstacle.
  ML_BLOCKING = 1;

// Blocks monsters only.
  ML_BLOCKMONSTERS = 2;

// Backside will not be present at all
//  if not two sided.
  ML_TWOSIDED = 4;

// If a texture is pegged, the texture will have
// the end exposed to air held constant at the
// top or bottom of the texture (stairs or pulled
// down things) and will move with a height change
// of one of the neighbor sectors.
// Unpegged textures allways have the first row of
// the texture at the top pixel of the line for both
// top and bottom textures (use next to windows).

// upper texture unpegged
  ML_DONTPEGTOP = 8;

// lower texture unpegged
  ML_DONTPEGBOTTOM = 16;

// In AutoMap: don't map as two sided: IT'S A SECRET!
  ML_SECRET = 32;

// Sound rendering: don't let sound cross two of these.
  ML_SOUNDBLOCK = 64;

// Don't draw on the automap at all.
  ML_DONTDRAW = 128;

// Set if already seen, thus drawn in automap.
  ML_MAPPED = 256;

// villsa [STRIFE] jump over rails?
  ML_JUMPOVER = 512;

// villsa [STRIFE] block flying things
  ML_BLOCKFLOATERS = 1024;

// villsa [STRIFE] TODO - 25% or 75% transcluency?
  ML_TRANSPARENT1 = 2048;

// villsa [STRIFE] TODO - 25% or 75% transcluency?
  ML_TRANSPARENT2 = 4096;

//jff 3/21/98 Set if line absorbs use by player
//allow multiple push/switch triggers to be used on one push
//JVAL: Changed from 512 to 8192 // STRIFE
  ML_PASSUSE = 8192;
//JVAL: Script Events
  ML_TRIGGERSCRIPTS = 16384;
//
  ML_NOCLIP = 32768;

type
// Sector definition, from editing.
  mapsector_t = record
    floorheight: smallint;
    ceilingheight: smallint;
    floorpic: char8_t;
    ceilingpic: char8_t;
    lightlevel: smallint;
    special: smallint;
    tag: smallint;
  end;
  Pmapsector_t = ^mapsector_t;
  mapsector_tArray = array[0..$FFFF] of mapsector_t;
  Pmapsector_tArray = ^mapsector_tArray;

// SubSector, as generated by BSP.
  mapsubsector_t = record
    numsegs: word;
  // Index of first one, segs are stored sequentially.
    firstseg: word;
  end;
  Pmapsubsector_t = ^mapsubsector_t;
  mapsubsector_tArray = array[0..$FFFF] of mapsubsector_t;
  Pmapsubsector_tArray = ^mapsubsector_tArray;

// LineSeg, generated by splitting LineDefs
// using partition lines selected by BSP builder.
  mapseg_t = record
    v1: smallint;
    v2: smallint;
    angle: smallint;
    linedef: smallint;
    side: smallint;
    offset: smallint;
  end;
  Pmapseg_t = ^mapseg_t;
  mapseg_tArray = array[0..$FFFF] of mapseg_t;
  Pmapseg_tArray = ^mapseg_tArray;

// BSP node structure.

// Indicate a leaf.
const
  NF_SUBSECTOR = $8000;
  NF_SUBSECTOR_V5 = 1 shl 31;

type
  mapnode_t = record
  // Partition line from (x,y) to x+dx,y+dy)
    x: smallint;
    y: smallint;
    dx: smallint;
    dy: smallint;

  // Bounding box for each child,
  // clip against view frustum.
    bbox: packed array[0..1] of packed array[0..3] of smallint;

  // If NF_SUBSECTOR its a subsector,
  // else it's a node of another subtree.
    children: packed array[0..1] of word;
  end;
  Pmapnode_t = ^mapnode_t;
  mapnode_tArray = array[0..$FFFF] of mapnode_t;
  Pmapnode_tArray = ^mapnode_tArray;

// Thing definition, position, orientation and type,
// plus skill/visibility flags and attributes.
  mapthing_t = record
    x: smallint;
    y: smallint;
    angle: smallint;
    _type: word;
    options: smallint;
  end;
  Pmapthing_t = ^mapthing_t;
  mapthing_tArray = array[0..$FFFF] of mapthing_t;
  Pmapthing_tArray = ^mapthing_tArray;

implementation

end.

