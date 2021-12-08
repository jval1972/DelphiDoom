//------------------------------------------------------------------------------
//
//  DelphiDoom: A modified and improved DOOM engine for Windows
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 1993-1996 by id Software, Inc.
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
//  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
//  02111-1307, USA.
//
// DESCRIPTION:
//  Node builder (glbsp wrapper)
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit nd_main;

interface

uses
  d_delphi,
  doomdata,
  r_defs,
  m_fixed;

var
  glnodesver: integer = 0;

const
  gNd2 = $32644E67;
  gNd3 = $33644E67;
  gNd4 = $34644E67;
  gNd5 = $35644E67;
  ZNOD = $444F4E5A; // ZDOOM specific, not supported
  ZGLN = $4E4C475A; // ZDOOM specific, not supported

type
  glmaplumpdesc_t = (
    ML_GL_LABEL,  // A separator name, GL_ExMx or GL_MAPxx
    ML_GL_VERTS,  // Extra Vertices
    ML_GL_SEGS,   // Segs, from linedefs & minisegs
    ML_GL_SSECT,  // SubSectors, list of segs
    ML_GL_NODES,  // GL BSP nodes
    ML_GL_PVS     // Vavoom Engine GL_PVS
  );

// JVAL: glBSP GLVertex structures
//
// LUMP GL_VERT
// This lump contains extra vertices that are needed by the GL BSP tree. Often a
// GL seg will begin and/or end someplace where there isn't a normal vertex (in
// the VERTEXES lump), and these GL vertices are stored here.
//
// The V1 format of GL_VERT is the same as the normal VERTEXES lump.
//
// Version 2 of these specs introduces a new format for GL_VERT. The new format
// is distinguished from the old format by a 4-byte magic identifier at offset 0,
// namely: "gNd2". Following the magic ID are the vertices, each vertex is just
// two 32-bit integers for X and Y (actually 16.16 fixed point).
//
// Version 5 of these specs does not change the format of GL_VERT, but the magic
// identifier at offset 0 will be "gNd5" to indicate the presence of V5 GL nodes
// (and hence the format of the other GL lumps).

type
  GLVertex1_t = record
    x: smallint;
    y: smallint;
  end;
  PGLVertex1_t = ^GLVertex1_t;
  GLVertex1_tArray = array[0..$FFFF] of GLVertex1_t;
  PGLVertex1_tArray = ^GLVertex1_tArray;

  GLVertex2_t = record
    x: fixed_t;
    y: fixed_t;
  end;
  PGLVertex2_t = ^GLVertex2_t;
  GLVertex2_tArray = array[0..$FFFF] of GLVertex2_t;
  PGLVertex2_tArray = ^GLVertex2_tArray;

  GLVertex_t = ^GLVertex2_t;
  PGLVertex_t = ^GLVertex_t;
  GLVertex_tArray = array[0..$FFFF] of GLVertex_t;
  PGLVertex_tArray = ^GLVertex_tArray;

// JVAL: glBSP GLSeg structures
//
// LUMP GL_SEGS
//
// The GL_SEGS lump defines all the GL segs, which form the boundaries of each
// GL subsector. The format is different to the usual SEGS lump, but should look
// pretty familiar:
//
// struct GLSeg1
// {
//   unsigned short start_vertex;
//   unsigned short end_vertex;
//   unsigned short linedef;
//   unsigned short side;
//   unsigned short partner_seg;
// }
// #define VERT_IS_GL (1 << 15)

// The start and end vertex values define precisely where the seg lies. There is
// no 'offset' or 'angle' fields like in the normal SEGS lump, it is assumed
// that the engine can trivially compute these. Bit 15 plays a special role:
// when 0, the vertex is a normal one (from VERTEXES), when 1 the vertex is a
// GL vertex (from GL_VERT lump).

// The linedef number is the linedef that the seg lies along, or 0xFFFF if the
// seg does not lie along any linedef. Those segs are called "minisegs", since
// they are never drawn by the engine, they only serve to mark the boundary of
// the subsector (for creating plane polygons).

// The side number is 0 if the seg lies along the RIGHT side of the linedef,
// or 1 if the seg lies along the LEFT side. This is the same as the 'direction'
// field in the normal SEGS lump. Ignored for minisegs.

// The partner seg needs some explanation. For segs that lie along a one-sided
// linedef, this field is simply 0xFFFF. Otherwise this field contains the index
// for the GL seg in the adjacent subsector which borders on the current seg.
// There is guaranteed to be a one-to-one correspondence between the segs that
// lie on either side of the border between any two subsectors. A corollary is
// that a seg's start vertex is the same as its partner's end vertex, and vice
// versa.

// Version 3 of these specs introduces a new format for GL_SEGS. The new format is distinguished from the old format by a 4-byte magic identifier at offset 0, namely: "gNd3". After the magic ID are the segs, and each seg has this format:

// struct GLSeg3
// {
//    unsigned int start_vertex;
//    unsigned int end_vertex;
//    unsigned short linedef;
//    unsigned short side;
//    unsigned int partner_seg;
// }
// #define VERT_IS_GL (1 << 30)

// Bit 30 of the vertex number is used to indicate a GL Vertex (instead of bit
// 15 as in V1).

// Version 5 of these specs introduces a small change to the V3 format. The main
// difference is that there is no magic ID at the start of the lump, you must
// check the GL_VERT lump instead. Each seg has this format:

// struct GLSeg5
// {
//    unsigned int start_vertex;
//    unsigned int end_vertex;
//    unsigned short linedef;
//    unsigned short side;
//    unsigned int partner_seg;
// }
// #define VERT_IS_GL (1 << 31)

// Bit 31 of the vertex number is used to indicate a GL Vertex (instead of bit
// 30 as in V3).

// An Engine may use the same code to read both V3 and V5 segs by checking the
// top two bits of the vertex number (for a GL vertex), and clearing both bits
// to get the index. Builders, however, must take care to set the correct bit.

type
  GLSeg1_t = record
    start_vertex: word;
    end_vertex: word;
    linedef: word;
    side: word;
    partner_seg: word;
  end;
  PGLSeg1_t = ^GLSeg1_t;
  GLSeg1_tArray = array[0..$FFFF] of GLSeg1_t;
  PGLSeg1_tArray = ^GLSeg1_tArray;

  GLSeg3_t = record
    start_vertex: LongWord;
    end_vertex: LongWord;
    linedef: word;
    side: word;
    partner_seg: LongWord;
  end;
  PGLSeg3_t = ^GLSeg3_t;
  GLSeg3_tArray = array[0..$FFFF] of GLSeg3_t;
  PGLSeg3_tArray = ^GLSeg3_tArray;

  GLSeg_t = GLSeg3_t;
  PGLSeg_t = ^GLSeg_t;
  GLSeg_tArray = array[0..$FFFF] of GLSeg_t;
  PGLSeg_tArray = ^GLSeg_tArray;

// JVAL: glBSP GLSubSector structures
// LUMP GL_SSECT
//
// This lump contains the info for all GL subsectors. Each GL subsector is made
// up of a sequence of GL segs (defined in the GL_SEGS lump).
//
// The GL_SSECT lump has some important properties that the usual SSECTORS lump
// does not have:
//
//    1. All edges of the subsector are included, even those that do not lie
// along linedefs (i.e. the minisegs).
//
//    2. There is a strict ordering: all the segs are in clockwise order (when
// looking from above). In other words: one seg's start vertex is the same as
// the previous seg's end vertex.
//
//    3. The subsector must be closed: i.e. all the segs form a continuous path,
// and the last seg's end vertex is the same as the first seg's start vertex.
//
// The V1 format of GL_SSECT is the same as the normal SSECTORS lump, reproduced
// here for convenience:
//
// struct SubSector1
// {
//    unsigned short count;
//    unsigned short first_seg;
// }
//
// Version 3 of these specs introduces a new format for GL_SSECT. The new format
// is distinguished from the old format by a 4-byte magic identifier at offset 0,
// namely: "gNd3". It's just like the old format except that 32-bit values are
// used for everything (the seg count and first seg number) instead of 16-bit
// values, as shown below:
//
// struct GLSubSector3
// {
//    unsigned int count;
//    unsigned int first_seg;
// }
//
// Version 5 of these specs introduces a small change to the V3 format. The only
// difference is that there is no magic ID at the start of the lump, you must
// check the GL_VERT lump instead.

type
  GLSubSector1_t = record
   count: word;
   first_seg: word;
  end;
  PGLSubSector1_t = ^GLSubSector1_t;
  GLSubSector1_tArray = array[0..$FFFF] of GLSubSector1_t;
  PGLSubSector1_tArray = ^GLSubSector3_tArray;

  GLSubSector3_t = record
   count: LongWord;
   first_seg: LongWord;
  end;
  PGLSubSector3_t = ^GLSubSector3_t;
  GLSubSector3_tArray = array[0..$FFFF] of GLSubSector3_t;
  PGLSubSector3_tArray = ^GLSubSector3_tArray;

// JVAL: glBSP GLNode structures
// LUMP GL_NODES
//
// The GL_NODES lump contains the information for the GL BSP tree. An important property which the normal NODES lump lacks is that the bounding boxes are guaranteed to cover the whole subsector, not just the segs that lie along linedefs (as happens with NODES).
//
// The format of GL_NODES is the same as the normal NODES lump, reproduced here for convenience:
//
// struct Node1
// {
//    short x; // partition line
//    short y;
//    short dx;
//    short dy;
//    short right_bbox[4];
//    short left_bbox[4];
//    unsigned short right_child;
//    unsigned short left_child;
// }
// #define CHILD_IS_SUBSEC (1 << 15)
//
// Version 5 of these specs introduces a new format for GL_NODES which makes the child indices 32-bits instead of 16-bits. Each node has the following new format:
//
// struct GLNode4
// {
//    short x; // partition line
//    short y;
//    short dx;
//    short dy;
//    short right_bbox[4];
//    short left_bbox[4];
//    unsigned int right_child;
//    unsigned int left_child;
// }
// #define CHILD_IS_SUBSEC (1 << 31)

type
  GLNode1_t = record
    x: smallint; // partition line
    y: smallint;
    dx: smallint;
    dy: smallint;
    right_bbox: packed array[0..3] of smallint;
    left_bbox: packed array[0..3] of smallint;
    right_child: word;
    left_child: word;
  end;
  PGLNode1_t = ^GLNode1_t;
  GLNode1_tArray = array[0..$FFFF] of GLNode1_t;
  PGLNode1_tArray = ^GLNode1_tArray;

  GLNode4_t = record
    x: smallint; // partition line
    y: smallint;
    dx: smallint;
    dy: smallint;
    right_bbox: packed array[0..3] of smallint;
    left_bbox: packed array[0..3] of smallint;
    right_child: LongWord;
    left_child: LongWord;
  end;
  PGLNode4_t = ^GLNode4_t;
  GLNode4_tArray = array[0..$FFFF] of GLNode4_t;
  PGLNode4_tArray = ^GLNode4_tArray;

  GLNode_t = GLNode4_t;
  PGLNode_t = ^GLNode_t;
  GLNode_tArray = array[0..$FFFF] of GLNode_t;
  PGLNode_tArray = ^GLNode_tArray;

function gld_GetGLNodesVersion(const base: integer): integer;

procedure gld_GetGLVertexes(v: Pvertex_t; lump: integer; count: integer; ver: integer);

function gld_GetGLMapLump(const maplump: integer): integer;

function gld_BuildNodes(const wadfile, gwafile: string): boolean;

function ND_GetNodes(const mapname: string): string;

type
  TGWAFile = class
  private
    ffilename: string;
    fnumglvertexes: integer;
    fglvertexes: PGLVertex2_tArray;
    fnummapsubsectors: integer;
    fmapsubsectors: PGLSubSector3_tArray;
    fnummapnodes: integer;
    fmapnodes: PGLNode4_tArray;
    fnummapsegs: integer;
    fmapsegs: PGLSeg3_tArray;
  public
    constructor Create(const afilename: string); virtual;
    destructor Destroy; override;
    property filename: string read ffilename;
    property numglvertexes: integer read fnumglvertexes;
    property glvertexes: PGLVertex2_tArray read fglvertexes;
    property nummapsubsectors: integer read fnummapsubsectors;
    property mapsubsectors: PGLSubSector3_tArray read fmapsubsectors;
    property nummapnodes: integer read fnummapnodes;
    property mapnodes: PGLNode4_tArray read fmapnodes;
    property nummapsegs: integer read fnummapsegs;
    property mapsegs: PGLSeg3_tArray read fmapsegs;
  end;

procedure ND_LoadVertexes(lump: integer; gwa: TGWAFile);

procedure ND_LoadSubsectors(gwa: TGWAFile);

procedure ND_LoadNodes(gwa: TGWAFile);

procedure ND_LoadSegs(gwa: TGWAFile);

procedure ND_NodesCheck(const lumpname: string);

implementation

uses
  m_crc32,
  i_tmp,
  i_exec,
  i_system,
  p_setup,
  w_wad,
  m_misc,
  m_argv,
  r_main,
  z_zone;

//
// JVAL
//  Returns the glbsp nodes version.
//
function gld_GetGLNodesVersion(const base: integer): integer;
var
  data: pointer;
begin
  if base + 4 > W_NumLumps then
  begin
    result := 0;
    exit;
  end;

  if base < 0 then
  begin
    result := 0;
    exit;
  end;

  if (strupper(char8tostring(W_GetNameForNum(base + Ord(ML_GL_VERTS)))) <> 'GL_VERT') or
     (strupper(char8tostring(W_GetNameForNum(base + Ord(ML_GL_SEGS)))) <> 'GL_SEGS') or
     (strupper(char8tostring(W_GetNameForNum(base + Ord(ML_GL_SSECT)))) <> 'GL_SSECT') or
     (strupper(char8tostring(W_GetNameForNum(base + Ord(ML_GL_NODES)))) <> 'GL_NODES') then
  begin
    result := 0;
    exit;
  end;

  data := W_CacheLumpNum(base + Ord(ML_GL_VERTS), PU_STATIC);
  if PLongWord(data)^ = gNd2 then
  begin
    Z_ChangeTag(data, PU_CACHE);
    data := W_CacheLumpNum(base + Ord(ML_GL_SEGS), PU_STATIC);
    if PLongWord(data)^ = gNd3 then
      result := 3
    else
      result := 2;
    printf('gld_GetGLNodesVersion(): GL v%d nodes found.'#13#10, [result]);
    Z_ChangeTag(data, PU_CACHE);
    exit;
  end;

  if PLongWord(data)^ = gNd4 then
  begin
    Z_ChangeTag(data, PU_CACHE);
    result := 4;
    printf('gld_GetGLNodesVersion(): GL v%d nodes found.'#13#10, [result]);
    exit;
  end;

  if PLongWord(data)^ = gNd5 then
  begin
    Z_ChangeTag(data, PU_CACHE);
    result := 5;
    printf('gld_GetGLNodesVersion(): GL v%d nodes found.'#13#10, [result]);
    exit;
  end;

  Z_ChangeTag(data, PU_CACHE);
  result := 0;
end;

procedure gld_GetGLVertexes(v: Pvertex_t; lump: integer; count: integer; ver: integer);
var
  i: integer;
  data: pointer;
  glvert1: PGLVertex1_t;
  glvert2: PGLVertex2_t;
begin
  if (count = 0) or (ver = 0) then
    exit;

  data := W_CacheLumpNum(lump, PU_STATIC);
  if ver = 1 then
  begin
    glvert1 := data;
    for i := 0 to count - 1 do
    begin
      v.x := glvert1.x shl FRACBITS;
      v.y := glvert1.y shl FRACBITS;
      inc(v);
      inc(glvert1);
    end;
  end
  else
  begin
    glvert2 := PGLVertex2_t(integer(data) + 4);
    for i := 0 to count - 1 do
    begin
      v.x := glvert2.x;
      v.y := glvert2.y;
      inc(v);
      inc(glvert2);
    end;
  end;
  Z_Free(data);
end;

//
// GetMapAdler32()
// JVAL: Compute Adler-32 checksum
const
  MOD_ADLER = 65521;

function GetMapAdler32(maplump: integer): LongWord;
var
  data: PByteArray;
  A, B: LongWord;
  len: integer;

  procedure Adler32Loop(lump: integer);
  var
    b1, b2: PByte;
  begin
    data := W_CacheLumpNum(lump, PU_STATIC);
    len := W_LumpLength(lump);
    b1 := @data[0];
    b2 := @data[len];
    while b1 <> b2 do
    begin
      A := (A + b1^) mod MOD_ADLER;
      B := (B + A) mod MOD_ADLER;
      inc(b1);
    end;
    Z_ChangeTag(data, PU_CACHE);
  end;

begin
  A := 1;
  B := 0;

  Adler32Loop(maplump + Ord(ML_VERTEXES));
  Adler32Loop(maplump + Ord(ML_LINEDEFS));

  result := B * 65536 + A;

end;

//
// GetGLMapAdler32
// JVAL: This function returns the Adler-32 value of glBSP utility
//
function GetGLMapAdler32(glmaplump: integer; glmapname: string): LongWord;
var
  txt: string;
  sglinf: TDStringList;
  stmp: string;
  i: integer;
  s1, s2: string;
  check: boolean;
begin
  txt := W_TextLumpNum(glmaplump);

  sglinf := TDStringList.Create;

  stmp := '';

  for i := 1 to Length(txt) do
  begin
    if txt[i] in [#10, #13] then
    begin
      if stmp <> '' then
      begin
        sglinf.Add(stmp);
        stmp := '';
      end;
    end
    else
      stmp := stmp + toupper(txt[i]);
  end;
  if stmp <> '' then
    sglinf.Add(stmp);

  result := 0;
  check := strupper(glmapname) = strupper(char8tostring(W_GetNameForNum(glmaplump)));
  for i := 0 to sglinf.Count - 1 do
  begin
    splitstring(sglinf.Strings[i], s1, s2, '=');
    if s1 = 'CHECKSUM' then
      result := atoui(s2)
    else if not check and (s1 = 'MAPNAME') then
    begin // Wrong map!
      check := (strupper(glmapname) = strupper(s2)) or (strupper(glmapname) = 'GL_' + strupper(s2));
      if not check then
        break;
    end;
  end;

  sglinf.Free;

  if not check then
    result := 0;
end;


function gld_GetGLMapLump(const maplump: integer): integer;
var
  adler32: LongWord;
  glmapname: string;
  glmapname8: name8_t;
  defglmapname8: name8_t;
  glmaplump: integer;
  i: integer;
begin
  adler32 := GetMapAdler32(maplump);
  if adler32 = 0 then // JVAL: unused?
  begin
    result := -1;
    exit;
  end;

  glmapname8.s := W_GetNameForNum(maplump);
  glmapname := 'GL_' + char8tostring(glmapname8.s);
  if Length(glmapname) <= 8 then
  begin
  // Check the first lump to find
    glmaplump := W_CheckNumForName(glmapname);
    if GetGLMapAdler32(glmaplump, glmapname) = adler32 then
    begin
      result := glmaplump;
      exit;
    end;
  // Check all the lumps after the map
    for i := maplump + Ord(ML_BLOCKMAP) + 1 to W_NumLumps - 1 do
      if (lumpinfo[i].v1 = glmapname8.x[0]) and
         (lumpinfo[i].v2 = glmapname8.x[1]) then
        if i <> glmaplump then // We've already checked this!
        begin
          if GetGLMapAdler32(i, glmapname) = adler32 then
          begin
            result := i;
            exit;
          end;
        end;
  end;

  // Check for GL_LEVEL lumps
  defglmapname8.s := stringtochar8('GL_LEVEL');
  for i := 0 to W_NumLumps - 1 do
    if (lumpinfo[i].v1 = glmapname8.x[0]) and
       (lumpinfo[i].v2 = glmapname8.x[1]) then
      begin
        if GetGLMapAdler32(i, glmapname) = adler32 then
        begin
          result := i;
          exit;
        end;
      end;

  // No luck so far? Look for the lumps previous to map
  // Check all the lumps after the map
  for i := 0 to maplump -1 do
    if (lumpinfo[i].v1 = glmapname8.x[0]) and
       (lumpinfo[i].v2 = glmapname8.x[1]) then
      begin
        if GetGLMapAdler32(i, glmapname) = adler32 then
        begin
          result := i;
          exit;
        end;
      end;

  result := -1;
end;

const
  glbsp: packed array[0..57403] of byte = (
    $4D, $5A, $90, $00, $03, $00, $00, $00, $04, $00, $00, $00, $FF, $FF, $00, $00,
    $B8, $00, $00, $00, $00, $00, $00, $00, $40, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $80, $00, $00, $00,
    $0E, $1F, $BA, $0E, $00, $B4, $09, $CD, $21, $B8, $01, $4C, $CD, $21, $54, $68,
    $69, $73, $20, $70, $72, $6F, $67, $72, $61, $6D, $20, $63, $61, $6E, $6E, $6F,
    $74, $20, $62, $65, $20, $72, $75, $6E, $20, $69, $6E, $20, $44, $4F, $53, $20,
    $6D, $6F, $64, $65, $2E, $0D, $0D, $0A, $24, $00, $00, $00, $00, $00, $00, $00,
    $50, $45, $00, $00, $4C, $01, $03, $00, $3C, $3F, $A8, $46, $00, $98, $01, $00,
    $3E, $00, $00, $00, $E0, $00, $07, $03, $0B, $01, $02, $38, $00, $E0, $00, $00,
    $00, $10, $00, $00, $00, $50, $01, $00, $A0, $31, $02, $00, $00, $60, $01, $00,
    $00, $40, $02, $00, $00, $00, $40, $00, $00, $10, $00, $00, $00, $02, $00, $00,
    $04, $00, $00, $00, $01, $00, $00, $00, $04, $00, $00, $00, $00, $00, $00, $00,
    $00, $50, $02, $00, $00, $10, $00, $00, $00, $00, $00, $00, $03, $00, $00, $00,
    $00, $00, $20, $00, $00, $10, $00, $00, $00, $00, $10, $00, $00, $10, $00, $00,
    $00, $00, $00, $00, $10, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $40, $02, $00, $B4, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $55, $50, $58, $30, $00, $00, $00, $00,
    $00, $50, $01, $00, $00, $10, $00, $00, $00, $00, $00, $00, $00, $02, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $80, $00, $00, $E0,
    $55, $50, $58, $31, $00, $00, $00, $00, $00, $E0, $00, $00, $00, $60, $01, $00,
    $00, $D4, $00, $00, $00, $02, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $40, $00, $00, $E0, $55, $50, $58, $32, $00, $00, $00, $00,
    $00, $10, $00, $00, $00, $40, $02, $00, $00, $02, $00, $00, $00, $D6, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $40, $00, $00, $C0,
    $32, $2E, $30, $33, $00, $55, $50, $58, $21, $0D, $09, $02, $08, $12, $D2, $BD,
    $FC, $74, $31, $A7, $58, $2B, $14, $02, $00, $87, $D1, $00, $00, $3C, $A0, $01,
    $00, $26, $0A, $00, $2A, $F7, $BF, $FD, $FF, $55, $89, $E5, $56, $53, $31, $F6,
    $8B, $45, $08, $31, $DB, $8B, $00, $01, $3D, $91, $00, $00, $C0, $77, $3B, $3D,
    $8D, $06, $FF, $FF, $7F, $79, $72, $52, $BE, $01, $00, $52, $52, $6A, $00, $6A,
    $08, $E8, $0A, $01, $31, $9C, $83, $C4, $10, $83, $F8, $01, $74, $5F, $85, $C0,
    $FD, $6F, $7E, $FB, $74, $26, $83, $EC, $0C, $15, $FF, $D0, $83, $CB, $FF, $89,
    $D8, $8D, $65, $F8, $5B, $5E, $5D, $C2, $04, $6B, $EE, $7B, $B3, $41, $93, $74,
    $C5, $3D, $94, $06, $C3, $16, $1B, $18, $DD, $9B, $AD, $3D, $90, $8D, $34, $1D,
    $05, $75, $E9, $56, $56, $53, $0B, $FD, $4F, $73, $20, $2C, $D2, $0B, $EB, $AA,
    $50, $50, $6A, $01, $B9, $DD, $EE, $97, $71, $85, $F6, $75, $05, $65, $EB, $B4,
    $10, $2D, $AC, $09, $6F, $ED, $99, $B9, $20, $51, $51, $3E, $E9, $0A, $50, $AD,
    $76, $00, $8D, $BB, $BF, $75, $DD, $BC, $27, $AB, $00, $CF, $53, $45, $20, $68,
    $00, $10, $40, $00, $40, $34, $5C, $AF, $7D, $9B, $2D, $0C, $37, $5A, $8D, $45,
    $F8, $C7, $02, $25, $77, $DB, $FE, $B7, $50, $0A, $F4, $8B, $1D, $D0, $50, $41,
    $00, $53, $50, $2A, $D0, $06, $68, $04, $04, $B7, $6F, $FE, $3F, $8E, $CC, $A1,
    $40, $06, $42, $00, $83, $C4, $20, $48, $8B, $15, $78, $11, $0C, $A3, $E0, $F7,
    $DF, $FF, $77, $27, $85, $D2, $75, $7C, $83, $FA, $E0, $74, $1B, $53, $53, $8B,
    $0D, $22, $51, $8B, $52, $30, $52, $A2, $B5, $AD, $CD, $CC, $BC, $28, $1F, $33,
    $14, $BB, $F6, $0D, $6B, $8F, $41, $50, $8B, $42, $09, $1E, $07, $AC, $23, $B1,
    $76, $FB, $BE, $47, $89, $0C, $2D, $4A, $E4, $F0, $90, $17, $D7, $5E, $2B, $B8,
    $8C, $A0, $76, $34, $85, $05, $86, $DA, $F7, $EF, $BB, $36, $00, $02, $DC, $89,
    $C3, $1D, $7C, $89, $1C, $24, $C6, $6C, $F1, $EF, $BB, $85, $ED, $51, $10, $50,
    $70, $FB, $01, $28, $8D, $B4, $26, $D2, $B9, $17, $B6, $65, $FF, $58, $14, $1D,
    $FF, $15, $68, $20, $29, $E4, $CA, $DE, $49, $00, $CC, $8D, $B6, $1F, $02, $DD,
    $E1, $CD, $6D, $DD, $98, $18, $25, $5D, $FF, $E1, $AF, $0F, $80, $7B, $EB, $DC,
    $21, $90, $00, $3F, $5D, $76, $01, $2E, $EC, $0C, $ED, $2D, $B8, $D9, $0F, $C8,
    $43, $70, $B7, $9C, $63, $86, $86, $CD, $32, $2C, $31, $D2, $3C, $0E, $70, $11,
    $0C, $F0, $23, $F6, $8F, $BA, $44, $89, $D0, $C9, $C3, $4F, $6F, $89, $C6, $6F,
    $EE, $F6, $B6, $23, $E0, $30, $3D, $E7, $03, $18, $7F, $21, $28, $EC, $8D, $40,
    $01, $0C, $C3, $EF, $DE, $56, $A3, $14, $92, $0A, $9C, $89, $04, $9D, $90, $09,
    $53, $3B, $D6, $BC, $F0, $C3, $50, $A3, $E8, $03, $00, $70, $1E, $08, $BC, $3D,
    $EF, $A7, $32, $17, $68, $39, $89, $F6, $55, $B8, $10, $7D, $DD, $83, $A1, $E3,
    $E5, $57, $56, $16, $1C, $76, $41, $3C, $04, $2E, $6C, $F7, $9D, $8D, $3C, $0B,
    $F0, $51, $4C, $49, $68, $2C, $5E, $93, $6B, $FF, $1F, $8C, $8B, $55, $08, $8B,
    $4D, $0C, $4A, $83, $C1, $04, $89, $09, $66, $1C, $7B, $6B, $DF, $5A, $89, $0F,
    $D3, $0F, $8E, $3C, $02, $2F, $0D, $0C, $BF, $67, $2A, $64, $EE, $FF, $BE, $FC,
    $8B, $18, $B9, $03, $5B, $DE, $F3, $A6, $74, $10, $BF, $6A, $12, $0F, $C1, $FE,
    $90, $EF, $75, $2B, $50, $70, $C7, $04, $24, $38, $72, $0B, $F7, $EF, $E3, $73,
    $4F, $07, $6C, $EA, $01, $8C, $EC, $BF, $F8, $73, $3A, $1F, $F2, $7D, $58, $06,
    $4A, $C5, $BF, $FE, $0F, $07, $B5, $BF, $05, $77, $20, $3F, $B0, $74, $1F, $A5,
    $BF, $0B, $74, $95, $AE, $B7, $8F, $BF, $B5, $93, $AA, $EC, $89, $45, $E8, $EB,
    $20, $3C, $6C, $0F, $E8, $83, $3F, $B3, $DD, $CC, $C0, $04, $08, $EC, $48, $06,
    $EC, $BE, $96, $01, $B2, $F4, $BF, $35, $55, $14, $02, $80, $38, $40, $75, $D6,
    $8D, $58, $01, $0E, $E4, $23, $8D, $8C, $30, $50, $C0, $53, $31, $CF, $C6, $EB,
    $1A, $EE, $76, $2F, $84, $C3, $CF, $C6, $05, $50, $5B, $00, $AE, $56, $AD, $FF,
    $F8, $A5, $EF, $FC, $1C, $C3, $B0, $FF, $74, $51, $A1, $5C, $F3, $38, $01, $0F,
    $85, $E3, $DF, $70, $BB, $D9, $43, $A1, $84, $0D, $C8, $66, $8B, $04, $58, $83,
    $E0, $08, $00, $DE, $FB, $97, $AD, $67, $74, $E4, $75, $60, $85, $FF, $7E, $0A,
    $B8, $43, $99, $5E, $58, $0B, $F6, $F6, $74, $67, $50, $75, $AF, $27, $7C, $28,
    $C4, $30, $4A, $3A, $14, $74, $2E, $BC, $61, $7E, $29, $33, $40, $37, $32, $0C,
    $3C, $65, $0F, $DB, $FF, $03, $D2, $86, $75, $68, $82, $0B, $8B, $4D, $B2, $FD,
    $ED, $B6, $C9, $C7, $3F, $FF, $00, $83, $FB, $0A, $0F, $94, $C2, $05, $0D, $6B,
    $5A, $D8, $FA, $C0, $09, $D0, $A8, $70, $07, $09, $E4, $D5, $91, $12, $EC, $EF,
    $8E, $7F, $22, $74, $3C, $81, $FF, $9F, $0F, $8B, $49, $88, $9F, $68, $47, $C6,
    $87, $F4, $F9, $76, $15, $5C, $5F, $B9, $8C, $08, $53, $71, $1C, $17, $3B, $21,
    $DD, $11, $53, $EB, $97, $B6, $55, $6E, $CC, $DA, $61, $25, $FF, $92, $0C, $35,
    $B3, $A0, $0F, $E4, $C7, $01, $6B, $40, $C8, $EB, $A1, $15, $7C, $74, $7B, $8C,
    $AF, $BC, $5C, $76, $C4, $68, $D1, $57, $6A, $60, $13, $48, $D7, $DF, $63, $85,
    $BE, $00, $68, $90, $79, $97, $10, $77, $F7, $DB, $3D, $DC, $A1, $70, $0E, $8B,
    $35, $27, $A3, $70, $14, $A1, $74, $79, $09, $CF, $F3, $3C, $4F, $74, $78, $78,
    $7C, $7C, $80, $AF, $25, $F3, $3C, $80, $84, $20, $84, $E4, $B3, $1E, $EC, $7D,
    $5A, $68, $3C, $56, $00, $0C, $DC, $E9, $AD, $37, $C7, $DF, $24, $A1, $15, $BA,
    $98, $77, $04, $0E, $53, $74, $06, $5F, $81, $DB, $B2, $7F, $10, $52, $68, $BA,
    $77, $BE, $13, $18, $D0, $08, $EE, $5E, $C7, $C2, $4F, $07, $47, $A1, $42, $3A,
    $11, $8B, $0A, $0C, $F9, $36, $14, $62, $69, $52, $04, $19, $C0, $07, $EF, $7D,
    $3C, $AB, $3C, $43, $94, $11, $38, $5F, $42, $AC, $99, $EC, $EB, $16, $17, $4A,
    $43, $18, $AE, $D9, $6F, $43, $C7, $98, $27, $E6, $07, $07, $3C, $6B, $7C, $50,
    $A3, $85, $F7, $B8, $EC, $D0, $78, $A8, $14, $26, $A8, $57, $6C, $81, $BD, $67,
    $AA, $C5, $77, $7D, $A8, $55, $F9, $B6, $8F, $5F, $63, $AE, $00, $EF, $F0, $60,
    $90, $0D, $A0, $1A, $1B, $E4, $83, $80, $E4, $77, $58, $A9, $50, $EF, $45, $BA,
    $FF, $37, $BC, $58, $8B, $3D, $14, $0C, $57, $40, $38, $FE, $6E, $C4, $FA, $5E,
    $8B, $1C, $90, $53, $56, $48, $F0, $A3, $2C, $41, $C7, $05, $68, $E0, $DA, $F7,
    $26, $00, $89, $12, $6B, $0E, $F2, $92, $66, $42, $50, $0A, $10, $24, $78, $83,
    $1C, $21, $CB, $51, $51, $24, $0A, $71, $AC, $E6, $1C, $A0, $FF, $25, $1A, $EB,
    $18, $2F, $36, $C6, $EF, $0E, $AB, $8B, $14, $0E, $52, $95, $D9, $BD, $B5, $7B,
    $7D, $17, $48, $ED, $DF, $79, $D9, $E5, $F4, $31, $C0, $FB, $1F, $6B, $6D, $E7,
    $5F, $E8, $B1, $57, $57, $8B, $34, $98, $56, $68, $48, $78, $62, $B0, $AD, $F0,
    $2D, $54, $79, $33, $A4, $6C, $14, $4C, $D9, $AC, $BC, $04, $1D, $FD, $2E, $61,
    $D8, $09, $ED, $24, $A2, $33, $FE, $95, $0A, $00, $13, $B2, $CD, $9C, $79, $85,
    $1F, $3A, $15, $3C, $94, $23, $71, $06, $43, $4F, $0F, $A1, $0E, $C0, $06, $0B,
    $9C, $C1, $20, $6A, $14, $8C, $FB, $90, $A6, $0C, $1E, $3C, $08, $3F, $08, $6F,
    $EF, $41, $B9, $90, $50, $00, $F0, $22, $19, $0C, $6C, $0D, $06, $4F, $8D, $CF,
    $50, $75, $D8, $E7, $C3, $C6, $3C, $7C, $4C, $89, $04, $24, $6C, $C9, $3F, $58,
    $03, $E6, $4B, $2F, $0C, $05, $50, $29, $80, $6B, $1A, $3B, $40, $52, $38, $5C,
    $60, $05, $22, $25, $5A, $0A, $8B, $64, $7F, $0F, $5A, $7B, $6F, $A5, $61, $37,
    $D7, $B1, $01, $68, $F4, $D3, $3F, $34, $57, $78, $B0, $3C, $C9, $3F, $25, $9E,
    $24, $C4, $5B, $F7, $36, $D3, $09, $A1, $A9, $08, $02, $41, $5D, $EB, $C6, $8B,
    $5B, $BC, $98, $36, $6F, $0A, $5D, $08, $CA, $AF, $15, $36, $7F, $00, $09, $1C,
    $8D, $43, $FF, $76, $10, $94, $D5, $36, $DB, $1B, $DB, $7E, $00, $5A, $5B, $2D,
    $B8, $DE, $1D, $12, $18, $20, $B0, $95, $BD, $25, $5F, $8F, $0F, $D7, $6B, $5C,
    $B6, $7F, $34, $2F, $80, $B3, $AC, $ED, $D6, $6E, $2A, $48, $66, $0F, $96, $CD,
    $7D, $08, $06, $95, $78, $AE, $D9, $1C, $FC, $85, $C2, $75, $13, $06, $5F, $30,
    $FF, $00, $6D, $64, $E1, $BE, $A3, $68, $90, $4C, $1F, $AF, $98, $A4, $DB, $62,
    $53, $B4, $0C, $54, $6F, $65, $BF, $40, $FE, $96, $E9, $20, $5E, $C9, $7E, $5B,
    $44, $39, $CB, $7F, $E0, $97, $F8, $F6, $4C, $8D, $14, $9B, $02, $92, $C1, $E2,
    $02, $D0, $C1, $FA, $1F, $F7, $F9, $09, $BB, $16, $5B, $BA, $3B, $71, $B1, $4C,
    $27, $51, $82, $3D, $30, $58, $3C, $68, $15, $79, $11, $4C, $E0, $1D, $FE, $28,
    $6C, $EF, $23, $D6, $75, $FC, $56, $31, $FC, $EB, $EA, $F1, $6C, $19, $9F, $8C,
    $28, $79, $40, $80, $EB, $9C, $C9, $91, $2C, $66, $59, $5F, $08, $E9, $16, $AE,
    $B0, $E3, $5D, $5F, $73, $1C, $5A, $D1, $B0, $80, $BD, $B0, $77, $BF, $05, $1F,
    $72, $EC, $EF, $4A, $06, $83, $63, $50, $A1, $1C, $38, $BD, $BB, $9F, $ED, $5F,
    $68, $F0, $72, $78, $4C, $8B, $03, $74, $C7, $43, $5C, $27, $EF, $25, $BA, $B3,
    $06, $58, $48, $05, $1C, $00, $75, $19, $9B, $63, $B1, $38, $D8, $02, $7A, $28,
    $BA, $02, $C1, $BB, $52, $13, $E6, $B1, $4B, $C3, $1B, $7A, $DA, $00, $5A, $CC,
    $57, $6B, $33, $DD, $67, $85, $12, $43, $04, $3C, $09, $0F, $DE, $69, $FD, $39,
    $98, $35, $59, $09, $30, $8B, $0B, $51, $C9, $6D, $6C, $D9, $32, $5B, $5C, $14,
    $17, $9C, $78, $3C, $01, $B9, $E6, $5A, $8C, $1D, $3E, $4C, $2B, $46, $C9, $76,
    $E4, $B7, $03, $03, $5D, $74, $3C, $75, $0E, $28, $30, $DF, $31, $D8, $CE, $5C,
    $20, $44, $D9, $43, $24, $79, $C7, $6F, $73, $0C, $9E, $00, $3C, $EB, $4B, $38,
    $85, $DA, $23, $0D, $23, $CF, $F3, $53, $34, $85, $D2, $38, $B4, $0C, $D4, $1F,
    $6D, $74, $C7, $DB, $77, $78, $1A, $2C, $8D, $42, $A5, $04, $77, $7E, $AE, $04,
    $43, $0B, $8D, $2D, $E3, $BF, $1C, $54, $B2, $2D, $62, $76, $C2, $F1, $F6, $3D,
    $18, $F6, $04, $0F, $86, $11, $3E, $A8, $17, $98, $49, $E8, $E0, $80, $3E, $88,
    $1F, $07, $03, $26, $B1, $12, $36, $66, $0B, $26, $05, $65, $CF, $3C, $7C, $C1,
    $D4, $23, $8A, $42, $38, $0C, $63, $9B, $E6, $B1, $1C, $4E, $0C, $0B, $00, $6D,
    $EB, $AF, $10, $2C, $F3, $1C, $79, $9E, $02, $80, $9E, $24, $00, $A0, $EC, $D9,
    $C8, $C8, $8D, $38, $D0, $46, $0C, $24, $13, $E4, $79, $20, $07, $04, $7B, $2C,
    $05, $3D, $08, $2D, $A1, $34, $58, $FF, $FA, $B2, $39, $82, $D0, $18, $ED, $05,
    $09, $EC, $D0, $F7, $C8, $38, $75, $10, $0F, $5D, $14, $CA, $DB, $7E, $DB, $C2,
    $B7, $28, $A9, $1F, $06, $CB, $2D, $D5, $B2, $0A, $4D, $EC, $77, $80, $E0, $1E,
    $5A, $60, $AD, $FE, $3E, $17, $0F, $98, $67, $21, $61, $F0, $2C, $B7, $06, $A1,
    $C7, $84, $06, $ED, $E5, $FC, $86, $FC, $FB, $E2, $C6, $7E, $48, $08, $8A, $E7,
    $4E, $01, $EB, $04, $43, $D3, $C3, $0D, $1B, $94, $99, $DC, $F8, $B8, $0A, $FC,
    $DA, $4B, $1F, $0E, $2E, $7C, $89, $C7, $89, $D8, $F7, $76, $FA, $09, $0E, $4F,
    $7D, $D8, $C1, $31, $F8, $E8, $C7, $44, $99, $04, $69, $3C, $99, $8C, $42, $89,
    $9D, $6B, $DF, $3A, $C9, $7D, $A1, $C6, $04, $4F, $89, $06, $AB, $A8, $8F, $A5,
    $20, $5B, $86, $57, $73, $52, $40, $1C, $37, $14, $03, $07, $11, $0F, $DB, $80,
    $78, $01, $23, $72, $84, $76, $BE, $1C, $01, $DE, $5A, $7B, $54, $FE, $BE, $E7,
    $37, $B3, $CD, $00, $EC, $0A, $E9, $B3, $7D, $F0, $01, $E1, $77, $19, $DB, $4D,
    $C3, $09, $14, $8E, $ED, $1E, $46, $04, $01, $12, $BF, $D6, $19, $E1, $0A, $D6,
    $47, $04, $2D, $18, $86, $59, $1B, $1B, $DF, $4C, $BE, $6B, $6D, $AE, $B5, $19,
    $A2, $9B, $25, $EA, $19, $08, $47, $BB, $5F, $43, $83, $0D, $30, $6A, $28, $24,
    $72, $6C, $A1, $5F, $EC, $3F, $B4, $CE, $0A, $38, $89, $42, $08, $C7, $40, $D5,
    $23, $A3, $F6, $93, $F8, $0C, $18, $57, $38, $77, $5B, $8B, $0E, $51, $55, $6A,
    $8B, $7D, $2E, $30, $2D, $02, $23, $BE, $8D, $1E, $3C, $3C, $5A, $D4, $2C, $A6,
    $46, $40, $15, $DD, $E7, $06, $DB, $53, $AC, $22, $C1, $50, $25, $95, $E4, $5C,
    $BD, $3B, $7B, $21, $75, $4F, $CB, $4D, $A0, $53, $6A, $0A, $6A, $36, $DB, $7B,
    $BD, $C4, $4E, $04, $51, $91, $3B, $36, $0C, $0E, $46, $30, $7E, $8C, $15, $28,
    $64, $E0, $AC, $64, $7B, $08, $43, $78, $5C, $42, $51, $B8, $A0, $54, $77, $2F,
    $EC, $12, $5F, $68, $87, $62, $74, $9D, $37, $0F, $BE, $03, $A3, $C7, $80, $58,
    $E8, $C9, $76, $75, $49, $91, $2E, $F9, $E8, $16, $15, $53, $01, $95, $7F, $50,
    $E1, $C3, $AD, $59, $04, $29, $1A, $43, $01, $50, $E8, $30, $36, $F1, $3D, $A4,
    $6F, $2C, $7D, $AA, $2A, $EB, $80, $09, $8C, $93, $21, $E9, $9E, $C5, $0F, $0C,
    $D3, $B7, $4C, $5C, $76, $78, $1F, $A0, $50, $D3, $88, $D3, $54, $6E, $94, $E4,
    $8B, $43, $C0, $AC, $04, $52, $3B, $20, $63, $49, $5F, $EC, $18, $7C, $D0, $F7,
    $42, $BA, $0B, $A0, $13, $1A, $75, $1C, $AC, $82, $9D, $EC, $70, $C7, $47, $18,
    $E7, $AD, $1C, $7C, $43, $90, $65, $CA, $1A, $14, $31, $2F, $45, $40, $0F, $C9,
    $0F, $B9, $37, $51, $51, $68, $4E, $0F, $8B, $55, $65, $4D, $49, $2E, $42, $28,
    $63, $50, $22, $CA, $BA, $65, $2F, $82, $22, $A5, $55, $22, $04, $76, $60, $5F,
    $75, $1C, $22, $57, $68, $64, $02, $9B, $B0, $22, $5C, $68, $24, $03, $99, $40,
    $0E, $5E, $24, $E4, $40, $26, $90, $63, $38, $65, $B0, $86, $65, $09, $22, $47,
    $6C, $68, $20, $0D, $49, $26, $10, $3A, $6F, $13, $C8, $81, $4C, $10, $78, $14,
    $C8, $04, $72, $20, $7B, $14, $07, $32, $81, $1C, $82, $48, $84, $C8, $81, $4C,
    $20, $48, $8E, $04, $72, $20, $13, $40, $90, $32, $81, $1C, $C8, $40, $99, $50,
    $81, $4C, $20, $07, $9B, $50, $72, $20, $13, $C8, $A4, $4C, $A6, $81, $1C, $C8,
    $04, $4C, $B2, $4B, $20, $07, $32, $44, $B5, $22, $40, $26, $B0, $09, $BD, $68,
    $34, $39, $90, $09, $E4, $C0, $34, $C9, $21, $13, $42, $02, $C6, $D4, $AC, $A1,
    $6C, $22, $A4, $1D, $FD, $2F, $DE, $17, $23, $6B, $28, $8F, $63, $FE, $24, $E8,
    $17, $E6, $C8, $1A, $CA, $8A, $F9, $19, $EE, $17, $72, $E4, $C9, $91, $75, $5F,
    $F8, $17, $5A, $00, $7D, $1C, $60, $4F, $86, $42, $09, $7D, $B2, $3C, $47, $1E,
    $D9, $81, $12, $3A, $D3, $FC, $1E, $7D, $76, $20, $07, $08, $10, $30, $26, $5D,
    $96, $22, $C9, $04, $20, $BB, $0E, $C5, $A3, $60, $DD, $E3, $2C, $7D, $8A, $8F,
    $E4, $E2, $47, $87, $1F, $0C, $21, $55, $10, $1E, $DD, $9E, $DA, $6C, $AB, $48,
    $A3, $44, $22, $03, $3D, $0C, $00, $8C, $1A, $9C, $4F, $BB, $15, $40, $6A, $23,
    $D0, $86, $3E, $DB, $08, $04, $74, $12, $ED, $74, $0D, $F2, $83, $1F, $9D, $F9,
    $06, $36, $1B, $90, $47, $40, $7D, $DF, $97, $30, $08, $3D, $11, $9C, $04, $CC,
    $67, $73, $4F, $6C, $30, $58, $77, $73, $1D, $0A, $5C, $18, $8B, $F7, $9A, $53,
    $A1, $76, $88, $5C, $60, $9D, $FB, $5A, $1C, $AE, $12, $1C, $7D, $BF, $11, $5F,
    $ED, $E4, $33, $BB, $3C, $29, $8E, $C4, $63, $6A, $77, $2E, $D5, $3B, $D8, $F6,
    $4B, $0C, $5E, $58, $D4, $6C, $0F, $4C, $A1, $B8, $F3, $61, $0B, $CF, $F9, $F4,
    $36, $0C, $0E, $97, $7D, $41, $FD, $67, $7F, $6C, $10, $58, $65, $5A, $8B, $30,
    $56, $68, $AC, $31, $73, $CC, $59, $5B, $89, $C6, $50, $6F, $81, $AD, $88, $27,
    $16, $50, $BC, $1D, $0A, $DB, $7D, $89, $63, $70, $59, $5A, $79, $80, $01, $C0,
    $50, $17, $53, $18, $3A, $3E, $F1, $B1, $67, $44, $6A, $25, $14, $89, $34, $24,
    $86, $EC, $A1, $70, $A1, $CF, $11, $18, $40, $14, $EB, $2B, $76, $89, $D7, $8D,
    $91, $86, $1B, $CF, $85, $D6, $33, $8B, $8B, $8F, $15, $A8, $C1, $56, $0D, $0A,
    $99, $17, $E1, $3B, $B4, $CC, $0A, $66, $69, $63, $6C, $E1, $5A, $BC, $63, $EE,
    $A7, $2E, $48, $BA, $06, $38, $1F, $39, $72, $C4, $1A, $BB, $B6, $2A, $AA, $97,
    $74, $85, $D8, $B2, $31, $01, $18, $10, $4A, $80, $01, $14, $06, $5C, $CC, $2F,
    $44, $20, $35, $DA, $EC, $9E, $C5, $4D, $3C, $04, $A8, $EC, $7E, $BC, $CF, $BC,
    $38, $B8, $5B, $98, $A1, $60, $21, $40, $C0, $D5, $0D, $F4, $07, $24, $F0, $52,
    $6A, $FA, $EC, $50, $53, $27, $F8, $36, $D3, $72, $82, $C6, $C6, $0A, $7B, $B5,
    $DB, $3F, $CD, $EE, $F6, $74, $61, $45, $4F, $FC, $8D, $7B, $9C, $89, $F2, $E9,
    $0B, $16, $6F, $B1, $CD, $9B, $4C, $78, $AC, $89, $F8, $15, $5E, $84, $7E, $20,
    $85, $FF, $16, $C1, $9E, $6D, $B6, $CA, $77, $6A, $DC, $20, $36, $64, $92, $2D,
    $C8, $48, $09, $40, $44, $0B, $47, $2E, $16, $3E, $86, $4C, $E4, $A0, $03, $3E,
    $FC, $86, $3C, $A1, $54, $1F, $15, $58, $FE, $D0, $19, $5B, $70, $37, $DC, $A1,
    $6C, $09, $21, $68, $05, $64, $10, $86, $86, $F0, $68, $B8, $D5, $AA, $FC, $70,
    $93, $ED, $A9, $CC, $E4, $10, $26, $52, $5C, $9E, $1F, $CD, $CE, $59, $17, $0A,
    $48, $38, $24, $3C, $C2, $9E, $0D, $F9, $F8, $B8, $E4, $28, $EC, $28, $15, $86,
    $8C, $0B, $FB, $AC, $63, $68, $24, $7E, $17, $7B, $7F, $A9, $20, $3B, $9E, $70,
    $04, $56, $80, $66, $DC, $26, $71, $59, $36, $5E, $19, $A9, $58, $DB, $97, $00,
    $DB, $91, $8D, $90, $7F, $51, $40, $40, $0C, $53, $4C, $59, $B6, $D9, $21, $5E,
    $15, $14, $08, $68, $12, $06, $D9, $0D, $AE, $71, $4C, $3C, $37, $40, $38, $E4,
    $D3, $31, $82, $7E, $01, $29, $AD, $6B, $E5, $F7, $40, $28, $D1, $82, $6C, $5C,
    $F0, $57, $83, $58, $90, $ED, $D8, $10, $53, $10, $5A, $59, $57, $9C, $52, $B3,
    $ED, $D0, $77, $B1, $31, $48, $E1, $59, $8C, $13, $F3, $8E, $77, $C0, $18, $30,
    $3F, $56, $8B, $35, $90, $4A, $53, $F7, $C6, $FF, $30, $32, $1B, $6E, $74, $30,
    $27, $E3, $94, $9D, $63, $DF, $0B, $EF, $15, $1E, $80, $B3, $42, $A1, $15, $89,
    $0E, $8B, $D4, $4B, $33, $E8, $44, $90, $FC, $44, $03, $B5, $00, $10, $CC, $CD,
    $28, $F6, $F9, $50, $1F, $7E, $60, $56, $BA, $65, $C1, $EF, $A3, $13, $EB, $AB,
    $5F, $6F, $88, $64, $90, $67, $4A, $8C, $44, $88, $77, $02, $19, $39, $8C, $88,
    $8B, $0D, $1D, $CF, $73, $D9, $AD, $B2, $50, $51, $70, $88, $8C, $AA, $3C, $53,
    $32, $61, $6F, $80, $84, $34, $C8, $C8, $21, $83, $80, $84, $80, $FB, $9D, $9D,
    $5A, $5F, $71, $35, $1F, $50, $56, $6F, $92, $2B, $79, $2E, $80, $84, $78, $0E,
    $19, $E4, $99, $7C, $48, $78, $7C, $B2, $10, $42, $46, $78, $4F, $7C, $36, $80,
    $3C, $17, $78, $7C, $70, $6F, $21, $83, $3C, $53, $74, $14, $70, $21, $53, $C8,
    $C8, $74, $70, $74, $20, $03, $C8, $73, $70, $74, $6C, $49, $1D, $30, $32, $33,
    $98, $29, $80, $F0, $64, $E4, $90, $8F, $32, $6C, $98, $6C, $E7, $B9, $B0, $29,
    $98, $72, $6C, $98, $94, $9C, $71, $AA, $A8, $0F, $2F, $68, $72, $C8, $20, $CF,
    $A8, $20, $68, $A8, $4C, $C9, $10, $32, $68, $A8, $68, $4C, $C9, $95, $3C, $BF,
    $A8, $64, $C8, $8C, $1C, $C6, $81, $64, $2F, $C8, $64, $7C, $43, $32, $85, $C8,
    $64, $6F, $C8, $81, $4C, $C9, $95, $60, $B8, $85, $8C, $1C, $C2, $60, $BF, $B8,
    $60, $80, $3C, $17, $32, $B8, $60, $B8, $83, $3C, $53, $36, $5C, $6F, $D8, $18,
    $5C, $53, $C8, $C8, $21, $D8, $5C, $D8, $1F, $C9, $73, $21, $5C, $D8, $53, $31,
    $DB, $FD, $C5, $FA, $EB, $52, $3B, $1D, $0B, $7D, $1E, $90, $22, $71, $04, $98,
    $43, $20, $F6, $CC, $8C, $39, $EC, $1D, $7C, $E3, $1C, $90, $49, $36, $68, $51,
    $19, $1F, $94, $90, $B0, $99, $7B, $44, $5D, $39, $35, $21, $85, $11, $EF, $0D,
    $83, $24, $FF, $41, $06, $D9, $DC, $53, $80, $7F, $88, $8C, $92, $93, $4B, $96,
    $0C, $51, $88, $8C, $59, $08, $99, $64, $8C, $88, $21, $2C, $DB, $08, $5B, $7F,
    $FF, $50, $80, $9C, $4D, $20, $83, $84, $80, $7F, $84, $42, $C8, $24, $93, $84,
    $80, $20, $13, $D9, $CA, $21, $7F, $78, $24, $27, $13, $C8, $7C, $78, $7C, $B2,
    $10, $32, $C9, $7C, $78, $21, $32, $C8, $44, $B6, $7F, $70, $74, $32, $C9, $C9,
    $04, $70, $74, $74, $AD, $2C, $84, $4C, $70, $21, $32, $C8, $72, $85, $7F, $51,
    $6C, $98, $9C, $5C, $B2, $0C, $14, $52, $6C, $98, $42, $C8, $24, $93, $98, $6C,
    $20, $13, $D9, $CA, $21, $FF, $68, $24, $67, $13, $C8, $A8, $68, $7F, $A8, $B2,
    $10, $32, $C9, $A8, $68, $21, $32, $C8, $44, $B6, $7F, $64, $C8, $32, $C9, $C9,
    $04, $64, $C8, $C8, $AD, $2C, $84, $4C, $64, $21, $81, $0C, $32, $91, $7F, $60,
    $B8, $93, $4C, $72, $32, $60, $B8, $B8, $60, $09, $2B, $0B, $21, $21, $FF, $13,
    $C8, $20, $13, $5C, $D8, $5C, $32, $C9, $24, $67, $7F, $D8, $D8, $5C, $11, $B6,
    $B2, $10, $21, $7F, $58, $37, $2A, $C0, $50, $3B, $1B, $78, $08, $E6, $68, $43,
    $1B, $35, $14, $6E, $E4, $9C, $68, $EB, $33, $C4, $32, $4F, $A1, $76, $8C, $FA,
    $14, $40, $99, $B2, $59, $34, $DF, $3F, $88, $00, $72, $24, $0F, $01, $7F, $8C,
    $D8, $94, $1C, $72, $80, $09, $84, $7F, $CB, $C8, $24, $57, $51, $78, $52, $11,
    $72, $00, $32, $25, $7C, $70, $01, $D8, $94, $1C, $18, $74, $FF, $20, $53, $72,
    $C8, $6C, $1E, $98, $4C, $C9, $21, $07, $68, $22, $A8, $65, $87, $1C, $80, $64,
    $2C, $7F, $1D, $72, $00, $32, $C8, $60, $31, $08, $52, $C8, $94, $3F, $B8, $67,
    $04, $C7, $DE, $8F, $68, $3C, $7F, $2A, $57, $0C, $3E, $75, $6C, $AC, $3E, $82,
    $07, $30, $76, $6B, $42, $1C, $FD, $25, $A0, $05, $1A, $B8, $48, $14, $85, $C9,
    $75, $06, $7B, $35, $18, $AF, $27, $23, $B1, $5F, $AE, $37, $50, $BF, $90, $AF,
    $EA, $30, $47, $D6, $8B, $50, $14, $F7, $08, $06, $B6, $0F, $CF, $2A, $94, $1B,
    $56, $16, $BD, $5B, $98, $45, $EB, $B8, $8F, $2F, $A8, $03, $B6, $A9, $45, $68,
    $50, $4B, $89, $C3, $03, $DA, $6F, $65, $E2, $96, $14, $C1, $E8, $02, $89, $1F,
    $0B, $02, $DA, $46, $30, $2C, $F0, $FE, $FF, $B3, $80, $D6, $5C, $94, $5B, $75,
    $75, $31, $FF, $8B, $73, $20, $3B, $F6, $8F, $60, $F7, $7D, $F0, $7D, $45, $3B,
    $18, $DC, $A8, $66, $8B, $06, $25, $50, $F6, $F6, $FF, $B5, $62, $83, $79, $3C,
    $50, $DF, $04, $24, $DD, $1B, $58, $14, $46, $02, $5A, $16, $C0, $5F, $89, $02,
    $DE, $19, $89, $7B, $10, $7C, $D5, $BB, $6F, $9D, $DD, $5B, $08, $47, $44, $7C,
    $BB, $A1, $90, $3A, $54, $59, $70, $C6, $9E, $A3, $58, $0E, $50, $61, $4F, $80,
    $93, $6F, $FA, $59, $76, $01, $25, $53, $C9, $94, $0D, $EC, $EB, $0D, $90, $00,
    $DF, $74, $5D, $74, $FB, $1F, $0E, $B8, $4F, $EC, $C4, $4E, $F7, $63, $E4, $EA,
    $03, $72, $75, $16, $82, $C9, $60, $DC, $F6, $EE, $D5, $1E, $7B, $2D, $56, $FB,
    $E5, $FD, $E8, $0F, $8D, $B7, $03, $C8, $AE, $21, $27, $F6, $EE, $1A, $2C, $98,
    $76, $00, $62, $EF, $89, $43, $10, $EB, $12, $C9, $14, $65, $D9, $1A, $74, $14,
    $C0, $05, $18, $08, $9A, $6D, $59, $96, $1C, $0C, $20, $10, $2C, $24, $14, $69,
    $B6, $58, $42, $0C, $18, $28, $16, $3E, $92, $EE, $00, $2C, $5E, $18, $30, $2D,
    $84, $FC, $F7, $7F, $8E, $03, $DA, $3B, $83, $F8, $63, $C7, $43, $40, $F3, $0F,
    $96, $43, $04, $47, $03, $82, $FB, $2D, $68, $1A, $B6, $8C, $4B, $10, $40, $68,
    $46, $56, $BD, $90, $7C, $26, $34, $4F, $F2, $2B, $B9, $10, $3F, $96, $CD, $CC,
    $CC, $CC, $60, $04, $83, $5C, $7D, $FF, $78, $D9, $B0, $27, $DF, $1C, $66, $9D,
    $77, $9C, $7B, $66, $20, $0F, $A7, $84, $57, $7D, $DF, $51, $9C, $34, $1C, $20,
    $4C, $03, $50, $04, $1F, $06, $B6, $48, $77, $80, $08, $18, $08, $FA, $0A, $D8,
    $CD, $1C, $C3, $1B, $67, $12, $BC, $43, $0C, $B0, $57, $C3, $C6, $6C, $8F, $91,
    $9F, $EF, $92, $41, $9E, $8B, $04, $45, $C0, $90, $23, $7B, $AB, $DE, $F0, $04,
    $06, $39, $42, $0E, $0A, $0C, $14, $80, $25, $1B, $48, $8E, $92, $EF, $2B, $E4,
    $57, $72, $B8, $89, $88, $88, $88, $87, $4C, $20, $4B, $1F, $D4, $B6, $19, $C7,
    $5A, $F2, $B0, $BC, $DD, $1C, $25, $F8, $66, $40, $C0, $42, $E6, $AA, $74, $CE,
    $1A, $DC, $02, $AE, $0F, $71, $22, $FC, $DD, $89, $13, $CF, $07, $91, $F4, $1A,
    $F5, $42, $0C, $40, $C7, $30, $6B, $F4, $20, $06, $C8, $04, $CB, $A7, $33, $32,
    $F2, $6E, $0C, $10, $14, $80, $4D, $A6, $85, $91, $14, $18, $20, $24, $83, $1E,
    $1E, $85, $35, $24, $23, $4C, $C1, $5F, $19, $81, $2B, $E4, $29, $54, $EC, $3F,
    $DC, $16, $FE, $9B, $23, $12, $B9, $93, $24, $49, $92, $D1, $E8, $F7, $E1, $43,
    $02, $20, $8F, $64, $59, $EC, $EC, $5E, $02, $7F, $08, $C0, $87, $41, $5B, $20,
    $BD, $EC, $89, $5D, $E8, $39, $20, $1C, $B8, $86, $45, $4E, $02, $20, $1F, $37,
    $84, $30, $D6, $CE, $FF, $02, $2F, $3C, $20, $52, $B6, $64, $61, $89, $C7, $0D,
    $42, $22, $9A, $BB, $15, $DE, $96, $8B, $47, $14, $49, $03, $8B, $43, $06, $03,
    $6F, $FF, $17, $1E, $A3, $4C, $8C, $DD, $03, $DC, $2F, $89, $C6, $D9, $05, $00,
    $80, $85, $D9, $C9, $89, $FE, $BB, $F6, $85, $78, $38, $58, $08, $D9, $E1, $09,
    $C8, $DD, $E1, $DF, $E0, $DD, $D9, $F6, $C4, $45, $1D, $BB, $B0, $DC, $A8, $82,
    $01, $DD, $5A, $DC, $6F, $1B, $DA, $E9, $DC, $27, $F4, $AE, $18, $16, $75, $08,
    $B2, $01, $25, $88, $56, $16, $9F, $66, $6F, $59, $E7, $84, $82, $04, $A3, $96,
    $83, $CB, $12, $D2, $15, $E9, $9C, $06, $1B, $DE, $F0, $42, $9A, $20, $08, $56,
    $1C, $33, $83, $E2, $01, $98, $AB, $35, $6B, $DB, $59, $14, $20, $24, $DF, $DA,
    $D3, $1F, $0B, $69, $76, $46, $15, $65, $0B, $56, $C9, $F4, $DD, $9A, $88, $66,
    $AF, $1D, $E6, $81, $E3, $1A, $98, $B8, $BF, $C1, $27, $27, $0F, $9D, $C2, $C6,
    $1F, $85, $C2, $0F, $84, $DB, $A7, $C0, $26, $DD, $27, $47, $89, $4E, $0C, $A7,
    $0C, $3F, $A5, $A3, $7B, $A6, $88, $56, $3C, $10, $95, $1C, $68, $A0, $CD, $F1,
    $28, $8B, $5A, $30, $98, $06, $7E, $26, $9F, $2B, $D1, $1E, $23, $40, $09, $C3,
    $89, $15, $21, $1F, $5A, $4A, $9A, $A5, $41, $59, $41, $15, $B5, $D0, $75, $8D,
    $C9, $21, $0A, $45, $06, $D0, $39, $AD, $DD, $E4, $BE, $39, $58, $88, $5E, $17,
    $98, $62, $83, $C2, $0E, $EC, $33, $B9, $71, $B9, $6D, $0B, $40, $0F, $EC, $89,
    $39, $D0, $9A, $01, $FE, $74, $A5, $1D, $2F, $DD, $D8, $8C, $2B, $5C, $5F, $C4,
    $53, $D9, $EE, $01, $3C, $D4, $BC, $1C, $14, $2C, $44, $12, $5E, $C8, $C7, $5F,
    $04, $B3, $01, $EB, $A4, $CD, $E5, $2A, $98, $72, $12, $01, $30, $BF, $FF, $04,
    $2E, $90, $03, $8C, $B6, $45, $55, $33, $4E, $6D, $46, $55, $EB, $01, $69, $B3,
    $91, $7C, $8C, $90, $03, $B0, $43, $04, $32, $91, $5C, $04, $71, $52, $18, $07,
    $CA, $07, $EF, $AF, $02, $16, $CA, $15, $38, $B5, $34, $AE, $F6, $F0, $B6, $E0,
    $65, $E9, $1C, $6D, $8A, $43, $06, $12, $46, $E4, $CD, $CD, $78, $8D, $85, $E4,
    $EF, $25, $1F, $6E, $04, $5F, $E8, $8A, $44, $13, $BF, $44, $96, $28, $42, $62,
    $7E, $ED, $B8, $D6, $EF, $16, $72, $02, $52, $88, $C8, $83, $E0, $01, $88, $65,
    $D9, $0C, $19, $47, $61, $5F, $C6, $9F, $0E, $47, $84, $0C, $85, $99, $56, $5B,
    $F9, $B2, $3C, $45, $5D, $C0, $10, $89, $5E, $0B, $43, $48, $96, $8D, $58, $A2,
    $9F, $C3, $15, $4A, $B6, $91, $F1, $2E, $0C, $8A, $12, $A4, $D7, $01, $A4, $E3,
    $E4, $9D, $A6, $2D, $4F, $EF, $23, $81, $29, $03, $9F, $3C, $5F, $80, $A3, $05,
    $9E, $10, $25, $5E, $24, $A4, $21, $88, $E1, $A0, $60, $A1, $A3, $E0, $26, $83,
    $93, $78, $04, $0F, $9E, $90, $0B, $F0, $76, $6E, $0E, $DA, $32, $AC, $4E, $A8,
    $3E, $7E, $E3, $76, $BB, $7D, $E7, $CD, $4F, $1C, $5C, $4B, $75, $F8, $EE, $90,
    $3B, $0D, $7D, $D0, $A3, $43, $06, $63, $0C, $24, $98, $06, $56, $07, $92, $EE,
    $00, $06, $6A, $47, $04, $14, $04, $84, $8E, $A4, $39, $08, $06, $0C, $B6, $E2,
    $36, $60, $BD, $5F, $40, $B5, $66, $8B, $78, $25, $58, $B4, $40, $4E, $0B, $18,
    $E9, $43, $B6, $E4, $EC, $47, $10, $24, $1A, $3E, $1A, $86, $0D, $4E, $24, $28,
    $43, $EA, $1C, $5D, $1D, $C1, $21, $D0, $30, $1C, $8F, $53, $67, $18, $3B, $1A,
    $A5, $6A, $04, $5B, $0D, $13, $78, $1A, $C5, $C6, $05, $03, $75, $F4, $78, $2A,
    $BA, $1C, $30, $59, $14, $6E, $23, $29, $D0, $3C, $BF, $52, $57, $EE, $84, $0B,
    $2C, $08, $80, $A7, $CC, $26, $71, $E6, $5C, $C2, $FE, $D2, $79, $D6, $50, $50,
    $56, $16, $39, $19, $39, $23, $A3, $41, $A0, $B2, $76, $4F, $7C, $28, $40, $84,
    $1C, $88, $48, $8B, $55, $0C, $07, $C1, $D4, $51, $14, $89, $A8, $79, $5E, $6C,
    $E2, $E3, $55, $57, $50, $0D, $04, $EF, $F6, $47, $82, $F7, $AD, $64, $39, $CF,
    $7D, $29, $AF, $57, $1C, $B0, $6B, $C7, $5B, $73, $AD, $F1, $65, $10, $0F, $95,
    $F5, $1E, $31, $8C, $66, $8B, $7D, $F7, $84, $90, $5F, $46, $39, $CE, $7C, $DB,
    $1C, $50, $46, $32, $83, $FD, $D5, $3B, $3D, $44, $74, $20, $50, $13, $3D, $AF,
    $18, $F6, $8F, $88, $50, $57, $68, $2C, $B0, $EC, $78, $B8, $2F, $E2, $A0, $06,
    $42, $23, $F6, $52, $42, $EE, $EE, $07, $8B, $07, $74, $0C, $81, $FF, $FE, $0A,
    $0F, $8F, $0B, $3B, $86, $89, $58, $6E, $0B, $FF, $7F, $7E, $2F, $62, $73, $05,
    $25, $FE, $0C, $01, $19, $C0, $24, $01, $30, $73, $48, $B7, $42, $42, $A7, $6D,
    $EF, $A3, $D9, $EE, $BA, $FB, $7A, $75, $2B, $DD, $76, $8D, $8F, $13, $D8, $25,
    $50, $1B, $28, $DC, $FE, $B1, $7D, $EE, $66, $94, $EE, $B4, $B2, $5A, $D9, $6D,
    $20, $98, $B6, $D6, $EC, $DB, $49, $05, $EE, $5E, $BE, $83, $ED, $9A, $2B, $B7,
    $3E, $1F, $F0, $58, $40, $75, $21, $EC, $1B, $B2, $5F, $3C, $46, $3D, $47, $3E,
    $0D, $B7, $EA, $6B, $8E, $5C, $F2, $08, $6A, $04, $00, $E4, $8E, $42, $12, $7E,
    $15, $5C, $8C, $58, $2A, $07, $B3, $B2, $83, $F7, $31, $DF, $D8, $05, $51, $4F,
    $32, $86, $0C, $59, $F8, $0E, $C2, $EB, $9F, $88, $5B, $DC, $22, $A4, $45, $58,
    $13, $74, $B0, $9F, $0A, $F2, $A1, $09, $32, $05, $D1, $24, $1C, $F8, $BE, $F6,
    $EC, $0E, $5A, $0F, $DF, $0C, $E7, $93, $61, $DC, $D8, $54, $80, $D5, $BD, $F0,
    $41, $18, $EF, $20, $DC, $60, $74, $65, $51, $61, $B6, $11, $BD, $94, $8B, $15,
    $0C, $23, $52, $97, $F2, $1F, $08, $9F, $A4, $15, $39, $D7, $7D, $17, $EF, $F6,
    $43, $13, $4B, $47, $78, $E1, $40, $75, $42, $DD, $D6, $7C, $ED, $BE, $39, $C7,
    $F8, $51, $C3, $21, $20, $0D, $C3, $5C, $9B, $A7, $9B, $DD, $65, $C0, $96, $23,
    $50, $64, $3D, $08, $57, $8B, $36, $B8, $6F, $F8, $75, $DC, $56, $EB, $96, $8F,
    $7D, $E6, $DC, $0D, $80, $36, $40, $E6, $B4, $65, $59, $36, $BC, $A2, $42, $E4,
    $46, $E4, $E0, $B9, $CC, $08, $DB, $E6, $47, $16, $E0, $7D, $6C, $E1, $E4, $92,
    $BD, $90, $2F, $2C, $2B, $2A, $CB, $82, $A9, $82, $C2, $B3, $6A, $EC, $E8, $73,
    $C8, $C9, $CB, $6A, $08, $5D, $DC, $53, $15, $D6, $A5, $8C, $C7, $61, $33, $BD,
    $0C, $2D, $2C, $33, $A0, $30, $25, $1F, $38, $02, $AA, $81, $24, $B4, $0C, $83,
    $86, $4D, $19, $58, $18, $02, $1D, $35, $60, $F6, $19, $46, $70, $5D, $40, $3A,
    $61, $10, $A0, $C8, $12, $DB, $D6, $25, $0C, $4C, $14, $CA, $13, $18, $05, $CC,
    $14, $CB, $B2, $2C, $1C, $D0, $20, $D4, $3B, $92, $66, $2B, $54, $2C, $D8, $28,
    $DC, $14, $09, $69, $8E, $A4, $2C, $DE, $30, $BC, $C2, $CB, $19, $60, $E0, $C8,
    $6A, $1A, $8C, $F2, $B9, $C1, $C3, $17, $FB, $78, $A0, $C6, $11, $56, $FF, $48,
    $7D, $F8, $69, $3D, $3B, $7F, $0F, $3D, $9A, $7F, $1D, $5A, $46, $76, $1E, $0E,
    $6A, $02, $05, $EC, $14, $99, $10, $5E, $1F, $1A, $EB, $E9, $9F, $0F, $B8, $63,
    $EF, $70, $02, $EB, $72, $55, $95, $92, $46, $34, $47, $D2, $74, $BD, $E4, $04,
    $C8, $08, $91, $91, $87, $95, $17, $0C, $10, $EC, $8C, $3C, $9C, $14, $29, $D8,
    $DC, $E1, $14, $47, $06, $E4, $1E, $80, $7D, $23, $68, $EE, $EB, $EB, $66, $8E,
    $03, $5C, $73, $EF, $65, $42, $78, $98, $70, $97, $8F, $AA, $D8, $00, $B6, $E4,
    $00, $B9, $04, $04, $FF, $7E, $81, $34, $07, $28, $DC, $27, $8B, $53, $10, $A1,
    $74, $01, $EF, $C7, $E2, $83, $C8, $FF, $8B, $D2, $03, $99, $84, $E5, $ED, $50,
    $B3, $D8, $6A, $0E, $46, $96, $88, $21, $94, $09, $38, $8D, $C7, $37, $42, $46,
    $03, $B8, $BB, $3D, $40, $42, $1D, $47, $C2, $EB, $FD, $08, $15, $45, $DA, $2D,
    $1C, $75, $80, $1C, $20, $20, $24, $A4, $3B, $71, $21, $CB, $53, $B4, $AA, $E0,
    $E4, $8B, $53, $F1, $84, $43, $77, $E2, $24, $32, $C8, $49, $B7, $26, $10, $E2,
    $2E, $85, $F0, $A4, $04, $0A, $36, $E2, $5F, $44, $C8, $01, $72, $08, $08, $45,
    $1C, $5C, $84, $33, $2C, $36, $8D, $56, $80, $D0, $83, $90, $37, $51, $19, $14,
    $3E, $66, $E5, $61, $88, $2D, $2F, $8C, $D1, $5F, $AA, $97, $93, $28, $88, $44,
    $15, $DF, $4A, $F2, $67, $27, $7C, $38, $31, $03, $75, $46, $E3, $E4, $0D, $5B,
    $69, $25, $23, $0D, $DF, $E6, $10, $76, $4B, $4E, $98, $64, $58, $49, $15, $57,
    $08, $CF, $52, $08, $6C, $5A, $74, $A5, $68, $D2, $28, $84, $13, $EB, $8E, $4F,
    $AF, $42, $29, $49, $C3, $FF, $51, $42, $60, $52, $13, $82, $C6, $1F, $68, $10,
    $41, $40, $00, $E6, $FD, $80, $87, $C2, $40, $E6, $D0, $DC, $6B, $C0, $45, $3D,
    $63, $89, $FA, $14, $17, $03, $9B, $88, $10, $63, $B5, $75, $3E, $F7, $12, $D2,
    $0C, $F2, $EC, $A1, $50, $52, $88, $CC, $96, $E4, $24, $9F, $68, $0B, $7B, $C1,
    $D0, $6C, $C3, $A6, $44, $20, $BB, $AF, $98, $61, $31, $FE, $83, $C5, $40, $74,
    $05, $0D, $00, $80, $54, $E7, $20, $91, $05, $61, $C0, $26, $9F, $30, $12, $DF,
    $15, $60, $D8, $0D, $A8, $83, $D6, $D8, $45, $D6, $B4, $96, $0E, $DC, $1F, $D8,
    $D8, $35, $AC, $15, $1F, $D4, $D4, $36, $82, $65, $59, $D0, $D6, $D0, $C3, $88,
    $25, $77, $61, $4C, $29, $DC, $DB, $37, $34, $12, $58, $C9, $40, $40, $BC, $70,
    $74, $E4, $82, $34, $16, $EE, $7E, $13, $DB, $62, $8B, $25, $24, $DD, $59, $40,
    $1B, $C9, $D3, $DD, $ED, $4B, $90, $ED, $46, $47, $DC, $28, $0B, $DC, $68, $10,
    $D8, $DE, $87, $EA, $BF, $C8, $03, $DE, $C1, $D9, $FA, $DD, $49, $32, $EC, $A6,
    $00, $81, $91, $C5, $A2, $99, $0B, $41, $DE, $E0, $63, $E2, $53, $0C, $BF, $4D,
    $CC, $51, $C7, $5A, $1C, $90, $6C, $39, $B5, $1F, $7D, $04, $EB, $06, $34, $92,
    $8F, $80, $0F, $10, $93, $26, $F9, $F8, $C5, $01, $1B, $3A, $75, $19, $10, $DF,
    $16, $1F, $03, $0C, $DF, $1F, $B0, $18, $32, $44, $86, $1B, $9A, $1F, $20, $0E,
    $20, $03, $C8, $74, $B8, $FD, $00, $80, $91, $90, $4B, $09, $18, $18, $42, $1A,
    $12, $BB, $DA, $DE, $53, $76, $70, $C7, $C7, $74, $5A, $30, $F2, $5B, $DC, $1C,
    $59, $D7, $6C, $1F, $18, $55, $1C, $1C, $7B, $0C, $90, $E6, $69, $28, $DD, $99,
    $E0, $0A, $D4, $21, $5B, $12, $0F, $3B, $D5, $03, $54, $96, $34, $7C, $D8, $75,
    $AB, $1A, $EB, $AE, $50, $18, $D8, $5E, $F2, $C5, $92, $C7, $96, $02, $EB, $E6,
    $25, $E1, $C8, $82, $4F, $38, $AB, $44, $53, $27, $0B, $C4, $75, $D4, $2A, $84,
    $19, $0E, $11, $4C, $AC, $B9, $CF, $BD, $C2, $20, $44, $98, $97, $9E, $C8, $52,
    $BE, $45, $72, $25, $17, $10, $2B, $1E, $D1, $40, $A0, $9F, $FC, $AD, $38, $CC,
    $82, $56, $B6, $1C, $B0, $6B, $A9, $09, $91, $A5, $E2, $F6, $BF, $81, $61, $10,
    $FA, $78, $DC, $9D, $52, $95, $10, $B9, $90, $85, $54, $36, $29, $BC, $DC, $39,
    $42, $4E, $86, $E2, $55, $E0, $0C, $A3, $3F, $38, $50, $15, $58, $1C, $53, $0D,
    $42, $B3, $50, $32, $AA, $02, $B8, $E4, $50, $B3, $E1, $21, $9C, $70, $46, $03,
    $47, $B5, $3D, $94, $66, $D3, $C9, $00, $42, $B7, $E0, $B0, $43, $19, $B5, $F7,
    $E0, $FF, $EB, $66, $59, $BF, $94, $45, $5F, $CC, $BA, $CE, $58, $CF, $5E, $3E,
    $55, $30, $00, $6C, $44, $04, $D6, $67, $07, $DF, $30, $28, $0D, $08, $5A, $50,
    $4F, $C0, $92, $4B, $80, $6F, $0C, $47, $4B, $80, $27, $0A, $9D, $00, $6F, $BB,
    $F5, $87, $C1, $05, $BF, $A1, $68, $ED, $7D, $58, $8D, $7D, $F0, $B4, $58, $46,
    $23, $7F, $A8, $68, $E4, $25, $74, $C2, $78, $9F, $F1, $47, $02, $6A, $6C, $1B,
    $34, $2B, $49, $27, $EC, $93, $57, $79, $80, $2F, $9B, $57, $7C, $AF, $3D, $3F,
    $1C, $DB, $6A, $14, $0F, $25, $20, $EE, $38, $04, $53, $27, $AB, $95, $C1, $5E,
    $83, $3F, $14, $1C, $C9, $40, $56, $5F, $F9, $E4, $84, $A5, $0B, $D6, $6D, $CF,
    $CC, $4C, $E8, $0F, $0E, $82, $0B, $D9, $CF, $CE, $55, $47, $04, $F5, $D4, $24,
    $5C, $5B, $04, $BC, $89, $C7, $57, $54, $1C, $D9, $52, $4E, $51, $C0, $BE, $90,
    $93, $68, $A2, $63, $82, $07, $59, $20, $13, $96, $8C, $D5, $CC, $04, $86, $0A,
    $46, $F6, $DE, $3F, $E7, $9E, $DC, $DB, $61, $23, $77, $40, $BF, $53, $89, $EA,
    $65, $D6, $D3, $14, $3D, $4D, $85, $27, $59, $28, $C5, $FB, $D8, $87, $69, $15,
    $08, $F1, $B2, $8D, $42, $01, $A3, $0A, $12, $42, $16, $2F, $48, $89, $53, $40,
    $76, $86, $2A, $45, $05, $9E, $8B, $4B, $44, $AB, $82, $62, $03, $55, $F8, $95,
    $58, $EC, $4B, $89, $D0, $A1, $65, $C0, $37, $20, $8D, $04, $02, $D1, $23, $94,
    $D9, $0E, $D4, $D7, $2B, $58, $67, $27, $54, $46, $83, $C1, $08, $99, $18, $10,
    $80, $1C, $20, $47, $E4, $E2, $D2, $1C, $49, $77, $E6, $14, $24, $E0, $30, $CD,
    $91, $34, $47, $EC, $34, $EA, $38, $00, $D2, $74, $24, $EE, $B3, $3C, $33, $45,
    $11, $CA, $E8, $4B, $D7, $0A, $DF, $51, $98, $67, $14, $82, $0B, $40, $50, $68,
    $02, $81, $F9, $9C, $21, $58, $5D, $66, $2F, $28, $8D, $1E, $0E, $B3, $1B, $2C,
    $0A, $84, $8E, $E8, $59, $60, $25, $41, $04, $E5, $50, $02, $88, $B0, $61, $8B,
    $38, $56, $E8, $C5, $29, $84, $41, $79, $D0, $53, $85, $AC, $14, $76, $47, $2B,
    $18, $1F, $0E, $41, $2A, $90, $58, $AC, $61, $E5, $BE, $B5, $F0, $8F, $47, $6D,
    $0D, $19, $6C, $17, $35, $42, $2F, $B6, $1D, $B0, $09, $AC, $CC, $86, $82, $1F,
    $36, $5D, $1A, $EA, $71, $EB, $A3, $5F, $81, $F2, $40, $8C, $8C, $2D, $D9, $DB,
    $46, $40, $AE, $1F, $0D, $20, $B6, $7F, $8A, $E4, $92, $21, $39, $1E, $C6, $21,
    $CD, $C6, $57, $08, $DA, $49, $7F, $8D, $08, $8F, $14, $A7, $2E, $AB, $B9, $08,
    $92, $8D, $58, $7F, $41, $CD, $47, $32, $18, $0F, $75, $76, $7B, $37, $75, $0E,
    $86, $71, $8C, $7A, $77, $74, $7B, $19, $95, $20, $F9, $B8, $AC, $50, $87, $F4,
    $6C, $20, $43, $36, $06, $C2, $40, $6C, $BF, $30, $06, $B0, $C3, $43, $AA, $EF,
    $54, $10, $68, $19, $8C, $0F, $F7, $74, $86, $1A, $2C, $90, $60, $55, $07, $23,
    $64, $21, $76, $5F, $8B, $50, $7C, $25, $06, $F2, $40, $52, $EB, $B3, $1F, $6F,
    $2E, $B4, $10, $98, $EC, $0D, $23, $AF, $0B, $28, $C5, $02, $C0, $10, $5D, $45,
    $60, $0B, $13, $35, $5F, $46, $14, $4F, $D7, $35, $60, $BE, $A8, $64, $1D, $57,
    $9B, $5E, $C7, $5C, $03, $7A, $8F, $05, $62, $00, $9F, $31, $18, $5B, $75, $81,
    $CD, $75, $4E, $00, $F0, $B8, $37, $1D, $F1, $8C, $8B, $A1, $64, $00, $C2, $52,
    $68, $74, $12, $5F, $D4, $65, $B3, $10, $94, $75, $DF, $98, $2D, $04, $DE, $3D,
    $25, $9F, $63, $28, $E1, $66, $3B, $4B, $EB, $9A, $4D, $A6, $5B, $C9, $88, $2D,
    $96, $7F, $C7, $4F, $40, $2E, $42, $56, $C0, $81, $E9, $A5, $3F, $24, $1E, $11,
    $03, $1F, $F7, $82, $68, $B3, $47, $3C, $EC, $58, $84, $0E, $37, $8D, $F8, $3B,
    $37, $EE, $E8, $8D, $0F, $6C, $3C, $59, $04, $62, $09, $C0, $B0, $5B, $0C, $17,
    $E1, $84, $1A, $3C, $0B, $94, $02, $1A, $3F, $84, $4C, $18, $31, $DF, $26, $11,
    $2B, $D9, $D0, $56, $08, $94, $81, $A9, $83, $3C, $DF, $C3, $DE, $DC, $0D, $B8,
    $81, $DE, $DC, $46, $DD, $64, $23, $96, $65, $DC, $D8, $DE, $C3, $D8, $2F, $02,
    $6E, $36, $29, $32, $6D, $2F, $37, $C9, $25, $7B, $2C, $2B, $2A, $B3, $D8, $30,
    $62, $F2, $ED, $E4, $58, $BC, $E0, $5A, $6A, $08, $BE, $27, $02, $12, $9D, $46,
    $9D, $8F, $5D, $7B, $45, $33, $09, $02, $5B, $F0, $66, $F5, $8C, $6A, $16, $2C,
    $9E, $7D, $68, $D4, $A7, $E2, $3B, $9B, $7D, $75, $1A, $EF, $01, $7D, $92, $82,
    $3C, $90, $8B, $4F, $BF, $79, $C5, $06, $59, $9A, $F0, $F0, $04, $1F, $3A, $56,
    $0C, $E8, $D8, $0B, $47, $AF, $22, $0E, $07, $4C, $B7, $85, $AA, $E3, $1B, $46,
    $0F, $F2, $97, $3B, $1B, $74, $16, $78, $61, $8B, $1A, $45, $8B, $EC, $25, $53,
    $68, $C0, $8A, $AE, $FD, $C0, $05, $4D, $EC, $41, $89, $03, $69, $3B, $F9, $73,
    $46, $BF, $3A, $7C, $8B, $5A, $C6, $56, $68, $F4, $81, $1D, $8C, $01, $2B, $21,
    $A1, $C9, $39, $C6, $0B, $26, $E8, $F0, $74, $13, $7B, $7B, $68, $1C, $82, $59,
    $EA, $1A, $DE, $06, $69, $1B, $40, $46, $E8, $E9, $92, $BD, $84, $0E, $BA, $B2,
    $CF, $28, $B0, $50, $E0, $D4, $10, $16, $58, $D1, $12, $C2, $80, $C6, $81, $07,
    $16, $7F, $E8, $1C, $61, $A7, $64, $26, $54, $82, $AE, $D3, $6A, $C9, $F2, $F4,
    $F8, $05, $57, $82, $B5, $48, $A6, $40, $17, $EC, $C0, $00, $37, $6B, $77, $2B,
    $F4, $AA, $60, $EC, $EE, $BA, $E6, $DA, $88, $45, $E5, $E7, $EC, $5A, $FD, $65,
    $92, $A5, $4B, $19, $E6, $02, $E5, $8A, $17, $56, $93, $01, $BF, $48, $9E, $20,
    $6F, $10, $B0, $5F, $8C, $24, $89, $C3, $4D, $01, $72, $C9, $9E, $89, $AC, $07,
    $C9, $10, $08, $0C, $A4, $59, $2D, $E9, $86, $75, $DC, $D8, $DB, $E8, $DA, $A1,
    $24, $03, $D2, $19, $DC, $DE, $40, $F5, $74, $03, $08, $DB, $E8, $E0, $88, $4B,
    $BA, $86, $05, $12, $E8, $43, $25, $97, $3C, $E4, $2F, $5A, $F2, $87, $83, $85,
    $5C, $C9, $37, $8B, $91, $37, $21, $4B, $29, $29, $F4, $5B, $0D, $42, $27, $62,
    $58, $F4, $0C, $3F, $5C, $92, $87, $C1, $41, $5C, $EF, $4A, $37, $0F, $26, $0B,
    $8D, $58, $75, $F3, $25, $77, $30, $81, $83, $E6, $F9, $4B, $40, $51, $BB, $C1,
    $86, $B2, $89, $90, $3D, $A3, $12, $49, $9C, $04, $BC, $C9, $76, $75, $49, $BB,
    $09, $B0, $7F, $58, $11, $E0, $20, $7F, $0D, $64, $6E, $0C, $30, $48, $49, $73,
    $F8, $F8, $F0, $30, $88, $BD, $B5, $E7, $07, $89, $D8, $4E, $E7, $42, $2F, $20,
    $07, $A0, $82, $8A, $BF, $4A, $52, $FC, $02, $94, $75, $08, $6D, $43, $06, $6C,
    $83, $FF, $47, $0B, $3C, $AD, $41, $35, $13, $D6, $81, $28, $F5, $15, $C4, $22,
    $03, $B8, $A1, $DA, $6C, $76, $3F, $6F, $00, $6B, $AC, $04, $46, $2C, $47, $3C,
    $48, $4C, $84, $81, $1A, $83, $EE, $4C, $6D, $7E, $27, $1B, $98, $FC, $FF, $6C,
    $DC, $56, $39, $5F, $AC, $1B, $05, $5D, $D5, $47, $54, $7C, $DB, $48, $B4, $2D,
    $E2, $5F, $F6, $D2, $8B, $48, $3C, $A5, $16, $06, $85, $1F, $3E, $04, $96, $C8,
    $01, $E9, $34, $09, $F0, $74, $C0, $54, $47, $AA, $BD, $D7, $89, $15, $61, $01,
    $01, $8B, $58, $A9, $82, $AB, $B8, $5F, $21, $48, $6E, $2F, $1C, $A3, $90, $04,
    $7B, $F7, $B6, $01, $71, $1A, $18, $52, $BA, $C9, $28, $75, $05, $BA, $D2, $06,
    $15, $D8, $6D, $95, $73, $D4, $06, $EB, $17, $2F, $76, $EE, $50, $8D, $16, $51,
    $91, $56, $18, $F9, $82, $6A, $AF, $4D, $A2, $D6, $42, $3D, $95, $6B, $EC, $07,
    $89, $F2, $40, $20, $09, $C1, $89, $13, $53, $74, $A0, $36, $0C, $42, $70, $1C,
    $0D, $13, $83, $4D, $A0, $6E, $4F, $47, $58, $5A, $3D, $16, $83, $AC, $76, $4F,
    $18, $D2, $18, $1A, $13, $72, $EC, $04, $25, $0C, $DC, $DE, $EC, $77, $09, $29,
    $0C, $60, $B6, $DF, $84, $34, $1F, $F1, $9F, $CE, $1F, $2D, $28, $1C, $52, $52,
    $A1, $70, $AE, $88, $F3, $7C, $50, $63, $05, $80, $D8, $78, $90, $9A, $5A, $6C,
    $D8, $1C, $5B, $9C, $9B, $1B, $20, $2F, $C2, $25, $FB, $C4, $DF, $FB, $54, $74,
    $EA, $EF, $C3, $AA, $95, $77, $14, $83, $3D, $42, $05, $0F, $8F, $EC, $61, $02,
    $0E, $84, $1F, $1D, $46, $43, $1A, $9F, $85, $F6, $70, $24, $2D, $67, $1C, $58,
    $48, $3B, $EC, $DC, $F7, $D9, $0A, $01, $48, $44, $9D, $E6, $29, $6E, $BE, $6C,
    $34, $B2, $B4, $9D, $50, $14, $D2, $7C, $C1, $40, $93, $EF, $7D, $F6, $34, $A6,
    $1F, $CA, $8C, $A1, $D2, $11, $84, $08, $75, $30, $D8, $C2, $7C, $A0, $50, $60,
    $63, $F7, $69, $78, $06, $35, $BA, $A4, $4D, $73, $7F, $D2, $FE, $B5, $19, $9F,
    $0B, $CA, $7F, $A3, $F2, $64, $0F, $8E, $63, $8D, $68, $D8, $93, $5A, $68, $5C,
    $E9, $46, $CE, $EC, $88, $9D, $2F, $DE, $3F, $4E, $BC, $1A, $75, $32, $7E, $9F,
    $2A, $27, $2C, $11, $52, $90, $5D, $C9, $0C, $C8, $66, $AB, $62, $BF, $4F, $B5,
    $4F, $18, $66, $93, $3C, $BB, $0F, $C0, $7C, $06, $BF, $A3, $9C, $EC, $DB, $FD,
    $3F, $FB, $90, $0F, $BD, $0C, $0A, $E6, $1F, $BB, $6C, $82, $C9, $3E, $93, $DB,
    $BC, $3C, $0F, $D0, $F7, $E9, $28, $39, $EF, $F2, $E7, $1D, $1E, $3C, $09, $9E,
    $EF, $FC, $CE, $BC, $04, $1F, $3C, $09, $20, $20, $06, $12, $EC, $7E, $21, $3C,
    $C9, $38, $21, $BC, $AF, $12, $41, $70, $DF, $D5, $80, $8D, $B5, $78, $F0, $5E,
    $54, $15, $98, $85, $85, $30, $2C, $E4, $85, $BD, $53, $D5, $38, $AB, $8B, $53,
    $FC, $0D, $52, $C1, $48, $67, $17, $C4, $BC, $85, $6C, $FF, $3E, $F0, $84, $05,
    $3A, $20, $2D, $66, $00, $1D, $38, $34, $53, $48, $33, $6E, $48, $6D, $24, $D8,
    $B2, $30, $85, $70, $E7, $4B, $C9, $1D, $84, $95, $1B, $43, $75, $10, $53, $4C,
    $59, $74, $53, $48, $33, $73, $50, $79, $1D, $34, $E9, $FE, $04, $D2, $1C, $78,
    $6E, $C6, $44, $06, $04, $22, $10, $0A, $19, $E4, $0A, $72, $44, $9F, $5D, $40,
    $E4, $75, $52, $95, $83, $5D, $1C, $47, $20, $19, $10, $DF, $5F, $60, $8D, $58,
    $45, $B3, $E9, $5D, $B4, $53, $B7, $75, $FC, $50, $FD, $63, $35, $FC, $4C, $16,
    $7E, $0F, $51, $94, $20, $E5, $76, $38, $02, $FB, $78, $19, $9B, $DC, $29, $53,
    $C2, $42, $9B, $06, $05, $7C, $5D, $B8, $FA, $B4, $DA, $5B, $25, $22, $CD, $9D,
    $53, $1A, $35, $A4, $01, $2D, $81, $61, $96, $88, $FF, $64, $81, $81, $1F, $FD,
    $48, $8B, $41, $2C, $83, $F8, $03, $B2, $81, $86, $4C, $D8, $5A, $00, $05, $F8,
    $05, $7C, $B6, $C9, $DC, $E8, $1A, $11, $48, $0F, $F8, $84, $B9, $36, $C8, $AD,
    $5C, $DC, $B7, $58, $F0, $0C, $0B, $B1, $A5, $3F, $C8, $54, $7E, $0A, $83, $79,
    $2C, $02, $60, $12, $21, $D8, $C8, $25, $BB, $6C, $15, $FE, $FF, $34, $64, $2B,
    $05, $3A, $B3, $1D, $04, $15, $46, $09, $01, $7C, $78, $7F, $BC, $93, $83, $A1,
    $65, $F7, $33, $5C, $A1, $80, $5F, $AF, $4D, $22, $6A, $A8, $17, $31, $49, $7C,
    $2A, $78, $67, $27, $8B, $8E, $7F, $3D, $2C, $BE, $62, $5D, $AC, $2B, $28, $0D,
    $3F, $B0, $B1, $C1, $EC, $61, $C8, $4F, $3F, $BA, $53, $0E, $6B, $78, $C2, $46,
    $4B, $AD, $19, $45, $6C, $97, $B6, $0B, $03, $E0, $55, $3A, $5E, $8E, $00, $DC,
    $5A, $B6, $93, $3B, $9C, $F4, $35, $9E, $78, $74, $FD, $4B, $3E, $BC, $B9, $77,
    $8B, $4C, $8A, $9C, $5A, $59, $6A, $00, $5D, $BB, $83, $01, $87, $31, $7C, $04,
    $34, $9C, $AC, $35, $D9, $49, $87, $9D, $AC, $47, $C4, $EB, $38, $89, $C1, $0E,
    $4B, $BE, $59, $C4, $02, $F8, $20, $BB, $4E, $7C, $51, $4C, $33, $DC, $AB, $EC,
    $4C, $C8, $8F, $87, $FC, $66, $10, $BD, $83, $52, $43, $D8, $2C, $6C, $5F, $50,
    $EB, $C4, $68, $CF, $E2, $83, $DC, $0A, $76, $04, $50, $3C, $70, $75, $8C, $18,
    $18, $0C, $D9, $68, $23, $02, $D7, $2C, $7E, $7D, $4D, $62, $96, $2C, $51, $37,
    $3B, $7C, $82, $C3, $3F, $BB, $7F, $53, $2A, $B3, $BC, $EB, $A9, $50, $1C, $4E,
    $B2, $EB, $1D, $06, $D2, $AF, $52, $FE, $36, $AC, $F3, $06, $D3, $C3, $C3, $85,
    $3C, $00, $02, $39, $5C, $50, $50, $70, $BA, $F6, $49, $47, $3E, $EC, $53, $0C,
    $0E, $3C, $26, $C1, $E7, $E0, $5B, $3B, $5C, $F5, $8F, $0C, $1F, $78, $2D, $41,
    $3E, $A3, $68, $01, $F7, $B1, $6F, $C6, $E9, $CF, $6E, $84, $6A, $BA, $7D, $BC,
    $B0, $EF, $9D, $F9, $4D, $3B, $4E, $37, $00, $06, $21, $33, $70, $80, $D0, $AF,
    $27, $36, $10, $0E, $90, $08, $E6, $C5, $16, $2A, $BE, $EF, $5D, $98, $9F, $2D,
    $2C, $71, $E9, $71, $09, $62, $42, $2F, $0C, $1E, $3D, $5C, $EE, $37, $1A, $72,
    $E4, $9C, $83, $CF, $7E, $22, $8B, $35, $58, $0D, $E8, $52, $89, $E0, $90, $51,
    $51, $FA, $8F, $04, $9E, $20, $FE, $09, $BB, $35, $33, $AC, $97, $0F, $43, $39,
    $DF, $7F, $E5, $65, $91, $5D, $12, $FD, $09, $B0, $01, $4F, $BE, $F8, $84, $FE,
    $89, $C7, $31, $F6, $BB, $01, $D1, $04, $9D, $0C, $E1, $57, $4E, $D8, $7F, $C0,
    $42, $11, $46, $43, $83, $FE, $0B, $7E, $E2, $44, $89, $D8, $D0, $46, $52, $74,
    $DF, $4D, $8A, $F4, $84, $CA, $E8, $3C, $89, $18, $E3, $70, $72, $61, $87, $0F,
    $1F, $28, $58, $6C, $0E, $C2, $15, $B2, $99, $06, $0C, $2F, $00, $43, $9D, $1C,
    $4A, $22, $15, $9D, $9C, $7C, $06, $18, $20, $24, $89, $03, $81, $86, $79, $FF,
    $C6, $C2, $68, $16, $68, $68, $24, $FE, $26, $98, $04, $13, $D8, $AE, $5D, $2A,
    $7A, $12, $89, $0A, $60, $56, $8C, $12, $03, $4C, $C6, $E1, $75, $ED, $62, $67,
    $46, $FE, $00, $EE, $D4, $53, $1B, $AE, $46, $96, $15, $74, $41, $20, $58, $9E,
    $27, $2D, $50, $4A, $60, $A7, $96, $11, $26, $E6, $8E, $0E, $1F, $CF, $A1, $54,
    $84, $44, $8B, $40, $24, $D0, $00, $03, $6F, $88, $04, $EB, $15, $7A, $1A, $14,
    $4B, $F0, $81, $81, $06, $8B, $1B, $78, $E7, $94, $23, $21, $18, $DD, $0F, $0C,
    $B6, $88, $61, $01, $FE, $69, $14, $C3, $68, $12, $EA, $06, $50, $74, $14, $ED,
    $4C, $EA, $2C, $57, $2D, $77, $0A, $4B, $21, $72, $CC, $66, $BF, $9A, $D4, $0B,
    $63, $EE, $E6, $A0, $6A, $19, $10, $A1, $30, $93, $7E, $6C, $B7, $29, $D7, $66,
    $0A, $4B, $3B, $A9, $20, $51, $14, $5D, $AC, $41, $D3, $9C, $00, $48, $3C, $B9,
    $4D, $21, $12, $74, $A9, $1D, $33, $53, $08, $21, $6C, $84, $72, $04, $DA, $47,
    $62, $77, $1C, $80, $E4, $FE, $50, $A1, $ED, $7E, $CD, $C0, $DF, $CD, $D1, $68,
    $88, $23, $EB, $DA, $DD, $8A, $90, $11, $FF, $FA, $15, $4C, $FC, $D6, $D0, $D1,
    $4C, $53, $44, $DB, $80, $3A, $BF, $1C, $21, $F8, $39, $F0, $4E, $6C, $83, $41,
    $04, $01, $DD, $42, $6D, $B6, $15, $58, $D8, $4D, $48, $1D, $13, $D4, $07, $09,
    $1E, $0D, $85, $8E, $16, $F8, $E9, $B2, $A7, $C6, $31, $92, $2A, $18, $52, $3F,
    $0B, $50, $23, $1C, $F9, $F4, $FC, $4F, $C7, $16, $34, $FC, $78, $74, $8B, $30,
    $89, $F3, $EE, $69, $67, $52, $EB, $B2, $99, $21, $61, $B8, $50, $6D, $1F, $1E,
    $4A, $F0, $A1, $F8, $10, $75, $E1, $8B, $13, $92, $43, $04, $23, $8E, $14, $B0,
    $6A, $04, $8D, $29, $89, $FC, $7E, $52, $B4, $9D, $57, $33, $C7, $0F, $3D, $03,
    $89, $5E, $04, $43, $82, $6E, $EC, $14, $89, $DE, $C3, $4F, $79, $F4, $90, $F0,
    $B8, $7D, $11, $D6, $2D, $EB, $D5, $F5, $BF, $01, $A1, $10, $61, $B5, $78, $BF,
    $0B, $18, $C2, $52, $59, $20, $97, $37, $6C, $E1, $6D, $3F, $39, $D0, $74, $24,
    $A1, $15, $16, $84, $52, $18, $09, $E1, $04, $43, $C7, $6A, $AC, $8E, $B3, $17,
    $76, $00, $BA, $74, $F6, $8F, $04, $75, $73, $45, $E8, $55, $5D, $73, $12, $5C,
    $05, $20, $1E, $C8, $59, $21, $9A, $00, $48, $79, $3A, $D9, $3F, $71, $89, $5F,
    $89, $C2, $83, $C2, $44, $88, $91, $41, $E2, $FC, $29, $16, $DA, $AC, $89, $C2,
    $B3, $16, $44, $41, $F6, $68, $EA, $71, $9A, $45, $42, $12, $24, $23, $89, $6A,
    $27, $D1, $5A, $E6, $8C, $86, $23, $18, $65, $63, $68, $65, $C1, $8C, $84, $AD,
    $92, $65, $B0, $9D, $C3, $B4, $56, $4E, $D3, $5E, $DE, $68, $F4, $96, $7D, $9B,
    $1B, $11, $06, $59, $7F, $50, $06, $0A, $BB, $38, $19, $47, $42, $CF, $10, $F5,
    $4B, $08, $6A, $C7, $DA, $0D, $53, $56, $6A, $42, $56, $5C, $5A, $60, $B8, $25,
    $F8, $A4, $CA, $8D, $75, $E8, $6B, $58, $76, $B3, $0F, $1B, $56, $57, $D6, $6A,
    $10, $56, $D3, $5A, $76, $2C, $1E, $E9, $BB, $18, $85, $7C, $ED, $84, $10, $4D,
    $BE, $BF, $8B, $4D, $60, $A1, $B7, $3D, $42, $4A, $5A, $8D, $78, $B0, $45, $EF,
    $57, $D0, $A8, $BF, $35, $5C, $EB, $2D, $85, $FF, $78, $43, $B7, $43, $1B, $FB,
    $54, $BC, $04, $3A, $87, $2C, $0F, $F5, $1E, $DC, $21, $3A, $0E, $32, $24, $10,
    $CF, $C3, $CD, $AD, $C0, $B7, $56, $4F, $4E, $79, $D0, $22, $31, $7E, $09, $80,
    $01, $02, $C3, $F8, $3C, $3A, $2E, $74, $12, $3B, $81, $B2, $31, $12, $A2, $0F,
    $4F, $81, $15, $6F, $0F, $88, $21, $F7, $9D, $F8, $FD, $31, $D6, $C1, $B7, $FA,
    $53, $5E, $4C, $84, $2E, $09, $1C, $DC, $81, $8F, $40, $83, $1D, $C6, $40, $01,
    $AC, $AA, $35, $E9, $24, $C6, $16, $0C, $49, $73, $B2, $F6, $39, $3A, $9B, $EF,
    $C1, $66, $1A, $48, $2E, $2E, $EB, $B0, $A4, $15, $6F, $7F, $19, $63, $96, $EB,
    $15, $CC, $D2, $CF, $56, $46, $93, $01, $EB, $56, $CF, $48, $7F, $65, $03, $72,
    $6E, $44, $15, $69, $56, $C4, $20, $ED, $66, $3B, $38, $40, $04, $4A, $89, $37,
    $AC, $99, $0A, $DB, $4C, $48, $1A, $0A, $52, $34, $25, $01, $08, $A7, $BA, $8E,
    $4E, $16, $98, $31, $1E, $70, $F9, $43, $43, $1D, $40, $A0, $45, $25, $92, $EB,
    $D4, $32, $44, $B4, $85, $AF, $AA, $B0, $27, $7E, $2C, $6A, $B2, $47, $2B, $4A,
    $89, $F0, $81, $FE, $71, $39, $66, $B4, $D3, $7C, $67, $9B, $49, $BC, $B9, $C0,
    $86, $37, $26, $10, $8F, $20, $01, $D0, $BF, $51, $56, $ED, $77, $8D, $4F, $5C,
    $31, $DC, $63, $13, $18, $01, $F1, $29, $F2, $89, $09, $D9, $7F, $09, $94, $89,
    $DF, $39, $73, $18, $7D, $D0, $89, $F2, $4E, $A9, $C2, $B5, $F7, $7D, $05, $BA,
    $06, $3B, $22, $60, $6A, $BF, $0F, $92, $2A, $AC, $EB, $A5, $B8, $17, $EB, $92,
    $96, $82, $43, $98, $24, $AF, $15, $44, $7D, $B9, $26, $32, $14, $F1, $47, $AA,
    $55, $09, $5A, $1B, $52, $ED, $1B, $0C, $B6, $D4, $2D, $28, $BB, $38, $AB, $F0,
    $0C, $64, $37, $85, $81, $4C, $F6, $2C, $58, $3C, $24, $22, $57, $2F, $8B, $20,
    $C9, $39, $29, $5D, $AC, $2E, $90, $A2, $67, $6F, $EB, $9E, $5F, $38, $63, $06,
    $6F, $81, $5F, $35, $21, $F2, $18, $BF, $FD, $37, $35, $4F, $76, $5E, $C7, $45,
    $C8, $47, $4C, $5F, $4C, $06, $CC, $45, $56, $45, $4C, $0D, $4B, $BD, $F8, $C6,
    $45, $D0, $00, $BF, $06, $B9, $C8, $DD, $53, $DE, $8B, $20, $24, $A1, $0B, $B0,
    $B5, $A2, $85, $B7, $73, $18, $D6, $06, $A2, $B1, $D0, $92, $DD, $58, $D8, $1E,
    $F6, $39, $77, $59, $6C, $D0, $75, $22, $1B, $50, $6F, $4A, $C5, $C3, $90, $5B,
    $01, $3B, $85, $C0, $A4, $61, $F4, $C2, $06, $7B, $76, $04, $68, $41, $85, $06,
    $23, $83, $25, $D0, $D8, $9F, $5F, $0B, $68, $58, $08, $4F, $4C, $40, $C4, $E0,
    $6C, $7C, $E5, $89, $0D, $78, $A8, $47, $10, $72, $65, $B8, $DF, $AA, $3C, $D1,
    $4A, $07, $ED, $8B, $57, $AF, $97, $04, $C2, $86, $6D, $C7, $82, $A9, $C7, $89,
    $5A, $02, $D8, $89, $90, $42, $4E, $3A, $9E, $C6, $2F, $13, $C6, $AE, $94, $84,
    $D0, $5E, $8C, $AF, $C3, $A0, $3D, $89, $25, $29, $31, $C9, $D7, $05, $FD, $49,
    $EC, $83, $1B, $90, $18, $0D, $8B, $00, $D1, $E8, $1E, $89, $5B, $A1, $EF, $90,
    $02, $D1, $00, $09, $E6, $89, $C8, $22, $18, $16, $27, $06, $5F, $6B, $DD, $B1,
    $77, $57, $39, $2D, $50, $24, $21, $17, $34, $15, $0B, $16, $BC, $AD, $F3, $03,
    $A3, $22, $F8, $34, $88, $60, $DD, $7E, $F6, $02, $02, $74, $EA, $1B, $EB, $E2,
    $DF, $8F, $65, $38, $5B, $5C, $54, $38, $05, $38, $1A, $68, $48, $85, $EF, $14,
    $9B, $14, $0D, $1A, $15, $19, $62, $0C, $8A, $17, $CF, $7B, $E5, $BF, $4B, $B0,
    $21, $82, $4A, $14, $03, $1E, $B8, $B6, $7E, $12, $FB, $37, $5A, $EF, $8B, $52,
    $20, $4F, $D0, $01, $75, $0C, $40, $39, $C1, $7F, $F5, $29, $80, $54, $B4, $DC,
    $6E, $D1, $7F, $C4, $70, $4D, $31, $54, $50, $67, $65, $E8, $77, $88, $07, $0F,
    $1B, $A3, $23, $B3, $37, $06, $0B, $9C, $08, $7C, $A5, $0C, $29, $03, $06, $3A,
    $18, $BC, $3A, $F3, $E0, $C6, $C7, $0C, $CA, $7C, $55, $EF, $50, $22, $1C, $E3,
    $FD, $C9, $6C, $78, $4C, $5E, $8B, $1D, $42, $44, $0C, $B8, $06, $FC, $43, $38,
    $B0, $7B, $8A, $4D, $D8, $80, $F9, $49, $14, $FE, $C1, $A2, $B6, $05, $50, $45,
    $06, $80, $7D, $D9, $57, $74, $0A, $F6, $6D, $B3, $35, $67, $41, $94, $EB, $BC,
    $0F, $DA, $41, $75, $F0, $05, $C1, $B0, $80, $FB, $DB, $44, $75, $EA, $2D, $EF,
    $DC, $32, $0B, $63, $85, $3B, $82, $35, $A3, $40, $59, $6B, $5A, $76, $C3, $2A,
    $DC, $A3, $44, $0B, $C4, $84, $A3, $48, $C2, $42, $F0, $1E, $23, $3D, $22, $70,
    $56, $A7, $3E, $C4, $A1, $D4, $F9, $D2, $8A, $BF, $C0, $50, $CD, $6C, $04, $D9,
    $0C, $50, $E5, $C4, $0F, $20, $93, $74, $93, $54, $09, $58, $5C, $23, $4C, $77,
    $8A, $2D, $54, $A1, $DE, $62, $D8, $64, $73, $44, $F1, $DC, $85, $18, $22, $49,
    $6C, $43, $5E, $F2, $44, $64, $2E, $7E, $38, $2B, $6B, $DC, $9F, $DF, $EB, $0F,
    $95, $21, $E1, $16, $DE, $89, $1D, $79, $51, $C9, $12, $2E, $03, $D2, $3C, $BC,
    $36, $5E, $C8, $10, $10, $F8, $D3, $20, $0F, $F6, $FC, $8C, $6C, $55, $D0, $53,
    $53, $05, $43, $9E, $96, $55, $52, $14, $6C, $52, $10, $4A, $1F, $6F, $C8, $17,
    $0A, $E1, $A1, $7A, $CF, $BA, $0E, $CC, $0C, $C7, $03, $BE, $07, $E0, $6C, $AF,
    $C5, $D1, $77, $07, $04, $78, $C6, $56, $1E, $78, $6F, $8D, $4C, $FC, $61, $88,
    $7E, $08, $1B, $47, $C7, $83, $38, $87, $28, $D4, $5B, $8B, $08, $51, $B3, $08,
    $72, $40, $1A, $86, $0A, $41, $8C, $E6, $C3, $60, $84, $A7, $38, $86, $E9, $E5,
    $84, $5D, $34, $56, $C9, $D5, $3F, $45, $C4, $60, $27, $3E, $96, $B0, $77, $48,
    $9A, $25, $BE, $F8, $08, $C0, $5D, $C4, $10, $0B, $89, $4D, $C4, $2D, $7B, $08,
    $10, $DB, $25, $73, $3F, $47, $1B, $0E, $21, $0B, $54, $6B, $2B, $1D, $3C, $94,
    $84, $4B, $73, $B5, $46, $4B, $F8, $C9, $3E, $05, $1E, $78, $39, $29, $DC, $B7,
    $55, $7C, $40, $8D, $5F, $E1, $30, $1A, $E6, $47, $AE, $3E, $E2, $F9, $CC, $01,
    $D7, $CD, $5C, $87, $B3, $E7, $1D, $CD, $EA, $FF, $85, $E8, $B5, $91, $F9, $84,
    $A3, $FC, $0B, $BC, $7B, $87, $C4, $8C, $12, $CE, $62, $86, $8B, $1F, $4F, $C0,
    $60, $66, $8B, $41, $94, $22, $04, $76, $E0, $4D, $51, $D7, $CC, $6D, $C1, $5E,
    $F8, $08, $0F, $9F, $A9, $7B, $1B, $01, $FB, $36, $3B, $BC, $D3, $F8, $08, $2D,
    $50, $D6, $ED, $AD, $14, $78, $0D, $E0, $09, $C2, $14, $C0, $40, $B8, $5A, $E8,
    $AD, $B5, $FC, $2E, $0F, $83, $7D, $C0, $BE, $9E, $4D, $42, $85, $21, $54, $5C,
    $AF, $3B, $D9, $B7, $D0, $14, $04, $21, $A9, $31, $47, $D1, $85, $23, $A4, $0B,
    $81, $03, $85, $78, $E4, $E8, $85, $0D, $88, $13, $C9, $4F, $80, $68, $54, $86,
    $7B, $73, $8F, $4B, $04, $A1, $91, $15, $63, $B0, $B8, $14, $78, $42, $45, $5B,
    $18, $B0, $42, $B8, $9D, $A8, $E3, $5D, $AB, $6B, $60, $29, $39, $14, $8D, $04,
    $95, $50, $CB, $EF, $04, $8A, $B3, $50, $A1, $4B, $0D, $3B, $1F, $C0, $F6, $4E,
    $D3, $13, $35, $B0, $9F, $B8, $E1, $0E, $CC, $1A, $AC, $22, $65, $38, $40, $8C,
    $1D, $FB, $2A, $04, $93, $66, $40, $A3, $6C, $3F, $82, $D4, $C6, $36, $05, $0A,
    $02, $B2, $C7, $F5, $0E, $38, $AC, $0D, $1F, $14, $75, $E3, $36, $63, $23, $A8,
    $1A, $4B, $32, $6C, $4B, $2E, $D3, $E3, $16, $53, $0D, $86, $70, $60, $19, $A7,
    $BF, $10, $3C, $04, $84, $91, $30, $01, $BF, $87, $F6, $2B, $37, $FE, $64, $93,
    $63, $13, $80, $7F, $01, $4C, $A8, $E8, $FD, $09, $2C, $DA, $CC, $72, $02, $5F,
    $DE, $84, $B4, $C4, $64, $C0, $AD, $DA, $08, $28, $85, $40, $F5, $17, $16, $28,
    $37, $1D, $17, $A5, $B4, $6C, $B4, $04, $0A, $B8, $C9, $F8, $7E, $D9, $8D, $47,
    $03, $3A, $A0, $3D, $FC, $A1, $63, $D2, $03, $61, $A3, $01, $4B, $DC, $08, $43,
    $8C, $98, $0B, $02, $13, $5E, $12, $C4, $0C, $BD, $56, $77, $86, $92, $20, $6E,
    $7C, $5F, $58, $ED, $89, $86, $4F, $2C, $C6, $62, $99, $01, $58, $4B, $5A, $69,
    $28, $1D, $12, $44, $5B, $58, $34, $82, $38, $D9, $80, $D3, $53, $5F, $24, $70,
    $21, $59, $54, $01, $B1, $34, $7C, $43, $EF, $C4, $EA, $F6, $10, $3D, $47, $F6,
    $46, $1D, $89, $DF, $99, $70, $5A, $9C, $CE, $22, $F6, $00, $02, $75, $1D, $29,
    $1B, $CD, $D6, $81, $D6, $16, $19, $43, $04, $64, $EB, $6C, $E9, $DD, $B1, $F1,
    $C8, $57, $4C, $0A, $E6, $8B, $36, $EB, $FE, $F0, $0E, $07, $01, $88, $75, $EB,
    $CA, $9D, $22, $98, $86, $BA, $25, $BC, $24, $86, $93, $D1, $20, $DB, $15, $3D,
    $95, $2D, $C6, $89, $B7, $C8, $04, $FF, $E5, $18, $C2, $EC, $E3, $0A, $88, $4A,
    $48, $27, $AC, $7E, $99, $CC, $86, $02, $64, $8D, $ED, $32, $C2, $3D, $BE, $21,
    $6D, $43, $22, $4E, $8F, $6E, $8A, $71, $75, $39, $0E, $B9, $80, $CD, $01, $36,
    $32, $2C, $32, $6A, $08, $54, $82, $B9, $5A, $B0, $41, $7E, $50, $15, $4E, $0F,
    $BB, $86, $17, $B6, $BA, $F8, $75, $65, $08, $F0, $47, $8D, $1C, $87, $C0, $25,
    $74, $21, $38, $BF, $38, $67, $40, $1C, $6A, $6C, $6A, $B4, $5A, $22, $18, $83,
    $6D, $44, $4A, $2C, $0C, $71, $A7, $D9, $38, $89, $00, $0A, $9A, $40, $01, $6E,
    $92, $AD, $18, $6C, $AD, $44, $48, $09, $40, $0C, $38, $98, $0C, $6F, $2F, $17,
    $87, $5A, $2A, $C2, $F9, $84, $DD, $96, $12, $DD, $52, $C4, $F0, $43, $5C, $58,
    $57, $25, $F7, $5E, $83, $4C, $20, $F3, $C0, $59, $0C, $DE, $C3, $6C, $CD, $8C,
    $48, $06, $D3, $56, $60, $AA, $D3, $C4, $0C, $9D, $13, $48, $E2, $3D, $22, $56,
    $D0, $0F, $40, $42, $00, $43, $62, $8C, $F1, $51, $63, $30, $02, $76, $D2, $09,
    $10, $5F, $F5, $A4, $C7, $F9, $22, $27, $7B, $87, $F4, $8D, $A4, $08, $DA, $C8,
    $C6, $5F, $A3, $58, $56, $52, $1A, $EE, $42, $65, $8E, $D0, $B8, $08, $CF, $BC,
    $93, $10, $91, $32, $A3, $F8, $01, $96, $31, $23, $24, $97, $A9, $E7, $A1, $E1,
    $08, $A3, $A0, $11, $6C, $BD, $EF, $7C, $E5, $36, $85, $F6, $E0, $20, $B6, $6C,
    $4B, $67, $69, $98, $E3, $30, $62, $18, $B4, $D4, $91, $E0, $9D, $04, $47, $15,
    $FF, $4A, $E1, $4E, $8D, $B5, $1F, $8A, $38, $52, $52, $1B, $26, $04, $EC, $04,
    $F5, $EC, $9F, $59, $92, $13, $1A, $8B, $10, $85, $42, $73, $17, $F0, $07, $1B,
    $52, $F8, $75, $05, $3B, $7D, $D4, $D0, $8A, $24, $4D, $C5, $D4, $FA, $87, $34,
    $22, $91, $A4, $3A, $AF, $E6, $15, $8B, $5C, $12, $68, $D6, $18, $DA, $58, $2C,
    $76, $67, $73, $24, $40, $55, $40, $E1, $89, $CD, $56, $2C, $5E, $40, $FD, $F6,
    $06, $21, $F0, $EB, $26, $60, $62, $8C, $46, $04, $BA, $7B, $6A, $05, $1F, $CC,
    $38, $D2, $C4, $58, $56, $04, $2F, $96, $D2, $9C, $BD, $B1, $7D, $2A, $42, $B1,
    $74, $E1, $12, $EB, $EE, $6F, $42, $D3, $F0, $FE, $B2, $42, $C1, $83, $C1, $03,
    $78, $2F, $11, $5A, $44, $2E, $CE, $B7, $FD, $E1, $FC, $46, $01, $C8, $26, $A3,
    $12, $89, $F9, $96, $FE, $CF, $60, $61, $10, $0C, $EB, $9C, $8D, $48, $06, $EB,
    $CC, $E3, $47, $72, $03, $70, $ED, $6B, $59, $0C, $18, $05, $84, $81, $DD, $0F,
    $1A, $AE, $17, $FF, $AF, $74, $FE, $6F, $43, $BA, $59, $E9, $3A, $1C, $EE, $29,
    $0B, $E2, $6F, $94, $18, $09, $AD, $69, $0B, $5A, $49, $57, $41, $AA, $E1, $58,
    $BB, $44, $0B, $67, $7D, $3C, $5A, $5C, $22, $40, $F2, $3C, $5F, $04, $12, $4F,
    $A5, $70, $F6, $2D, $59, $73, $C9, $98, $36, $F0, $2F, $0E, $18, $0D, $E9, $76,
    $31, $EB, $DC, $4D, $50, $04, $08, $11, $17, $12, $7A, $10, $88, $F0, $03, $21,
    $BE, $32, $05, $83, $06, $7F, $F1, $51, $BC, $E8, $68, $30, $88, $30, $EF, $5C,
    $05, $4D, $08, $6A, $BF, $FB, $8F, $07, $B5, $67, $05, $0C, $89, $19, $5C, $88,
    $E6, $DE, $D9, $4B, $71, $53, $68, $34, $4E, $77, $18, $22, $1B, $44, $68, $84,
    $4A, $1F, $51, $43, $A9, $27, $99, $1F, $60, $9B, $00, $91, $75, $77, $B9, $10,
    $3B, $5B, $31, $5F, $19, $0D, $89, $56, $C1, $66, $00, $18, $64, $23, $A6, $30,
    $5B, $17, $70, $46, $12, $5C, $0C, $8F, $84, $82, $F7, $7F, $07, $EB, $1D, $16,
    $E4, $39, $1D, $D1, $EC, $3D, $7E, $22, $7F, $E3, $7B, $1F, $18, $45, $C1, $00,
    $96, $32, $A7, $93, $0F, $60, $08, $17, $EC, $20, $06, $E4, $C2, $76, $AE, $6A,
    $F7, $1A, $30, $EE, $93, $8E, $2E, $21, $4F, $50, $A1, $90, $75, $90, $CD, $05,
    $54, $0C, $94, $A3, $60, $0D, $AD, $83, $8C, $AE, $98, $6A, $38, $E3, $17, $6A,
    $6C, $27, $F0, $EC, $FF, $68, $70, $D9, $E8, $7C, $87, $1B, $23, $7C, $2B, $CA,
    $33, $D6, $B0, $2D, $80, $04, $7F, $60, $4B, $7E, $34, $55, $50, $88, $2A, $45,
    $90, $B0, $5A, $1F, $A4, $25, $5E, $D7, $3F, $0C, $88, $A3, $66, $30, $74, $93,
    $CC, $55, $34, $E1, $2D, $CF, $3C, $7F, $EF, $09, $9F, $3C, $D2, $EC, $4B, $A1,
    $80, $56, $12, $A1, $2C, $81, $61, $11, $74, $11, $D9, $16, $E9, $C5, $FB, $2A,
    $5E, $C4, $22, $68, $75, $8B, $3E, $C2, $5E, $3D, $88, $EB, $BF, $90, $7B, $86,
    $50, $04, $AC, $74, $AF, $E8, $4B, $02, $0F, $CB, $9F, $2F, $70, $C2, $71, $0C,
    $40, $B3, $D8, $1F, $26, $1E, $47, $57, $51, $C2, $9F, $8B, $54, $2B, $2D, $B0,
    $F7, $CE, $92, $69, $9F, $96, $1F, $60, $6E, $80, $93, $42, $53, $75, $AC, $B8,
    $7B, $FB, $15, $78, $1D, $CC, $27, $29, $C8, $53, $7E, $15, $52, $54, $83, $1D,
    $16, $36, $E6, $7F, $3A, $CF, $82, $44, $41, $B6, $5C, $AD, $60, $1F, $13, $11,
    $90, $11, $9F, $9C, $A1, $DE, $89, $25, $41, $1A, $5F, $28, $F4, $08, $28, $45,
    $03, $52, $01, $04, $A8, $95, $58, $CB, $09, $FE, $04, $5F, $85, $3C, $47, $96,
    $1F, $10, $10, $8E, $58, $C9, $73, $14, $14, $11, $75, $B7, $FE, $06, $0C, $8B,
    $48, $0C, $80, $CE, $80, $A4, $FE, $89, $0B, $50, $02, $B4, $B5, $B6, $BC, $E2,
    $08, $11, $0B, $10, $CF, $A6, $A2, $A7, $17, $BE, $C2, $CD, $4D, $48, $85, $7B,
    $83, $1C, $2A, $1C, $05, $BE, $E0, $11, $BF, $21, $75, $D4, $43, $48, $C4, $FE,
    $BC, $FE, $FB, $40, $97, $8D, $A0, $F6, $74, $6C, $7F, $32, $06, $9A, $BE, $7F,
    $74, $15, $9B, $A5, $6A, $0A, $B5, $19, $FB, $02, $E8, $16, $D1, $26, $36, $42,
    $A5, $05, $89, $D4, $07, $06, $A9, $E5, $E9, $C2, $81, $FB, $7D, $BA, $67, $33,
    $8B, $34, $95, $5B, $0D, $02, $F0, $00, $DB, $EA, $9C, $D9, $05, $8F, $C0, $11,
    $3A, $75, $48, $75, $3C, $B0, $B3, $C3, $E6, $1F, $EB, $0E, $36, $0F, $3A, $4C,
    $DE, $9E, $6D, $37, $88, $FB, $10, $49, $CF, $70, $0A, $08, $75, $8E, $BE, $A7,
    $7B, $CE, $34, $4F, $D9, $5A, $20, $A4, $1E, $36, $D9, $5C, $B4, $E9, $0D, $10,
    $85, $68, $E9, $47, $68, $9C, $3D, $67, $0B, $37, $91, $A1, $0B, $A8, $39, $33,
    $4F, $F7, $95, $35, $40, $32, $17, $C0, $26, $D9, $B6, $9D, $32, $6F, $06, $6F,
    $20, $17, $1A, $D3, $0E, $FB, $74, $43, $32, $04, $FF, $FE, $1A, $ED, $99, $90,
    $67, $43, $0E, $0A, $8A, $22, $39, $39, $D9, $0D, $29, $3C, $8A, $4E, $64, $10,
    $E8, $90, $0B, $78, $90, $3F, $7E, $6C, $41, $19, $0C, $2D, $75, $6F, $90, $8A,
    $26, $92, $44, $10, $A0, $EE, $E7, $F4, $9C, $D1, $A4, $2C, $DF, $88, $19, $14,
    $F2, $76, $A3, $17, $09, $A2, $8B, $78, $1F, $FF, $75, $16, $30, $24, $BE, $08,
    $03, $DA, $5B, $68, $FB, $1B, $2E, $8F, $BB, $00, $30, $EB, $04, $D1, $FB, $74,
    $E5, $85, $03, $F8, $ED, $D9, $F4, $89, $E5, $53, $56, $BD, $6E, $5C, $E7, $3C,
    $D8, $D0, $DE, $06, $D1, $C1, $74, $39, $29, $89, $90, $C3, $6E, $25, $A1, $5F,
    $C6, $7C, $8B, $87, $3A, $13, $ED, $90, $1D, $DB, $7A, $8F, $8E, $0C, $D5, $55,
    $F0, $26, $7B, $75, $07, $8B, $4E, $19, $0E, $60, $F8, $B0, $09, $9C, $74, $CA,
    $C1, $EA, $38, $E9, $46, $87, $01, $C1, $D0, $88, $F6, $C1, $90, $88, $8E, $BE,
    $51, $85, $E5, $04, $74, $D5, $39, $73, $A8, $89, $65, $DC, $B7, $00, $8C, $83,
    $18, $C0, $89, $36, $B0, $93, $78, $30, $16, $5B, $66, $43, $F6, $29, $5C, $99,
    $4D, $68, $60, $19, $AD, $77, $49, $75, $1E, $EF, $CF, $0D, $A5, $66, $05, $B8,
    $A2, $07, $06, $75, $37, $0B, $D1, $9A, $EA, $51, $39, $8B, $09, $A7, $F3, $8A,
    $EE, $C9, $E8, $E7, $93, $8D, $04, $3B, $4B, $01, $14, $0D, $FB, $9A, $B1, $3E,
    $18, $98, $61, $33, $DD, $88, $81, $EF, $0C, $75, $C2, $D6, $40, $3B, $E2, $26,
    $57, $54, $BE, $83, $DB, $FF, $64, $DF, $FF, $3B, $00, $FF, $4C, $AA, $08, $DE,
    $FF, $EB, $A5, $C9, $9F, $67, $8A, $36, $2C, $25, $7E, $14, $3C, $10, $15, $47,
    $78, $EE, $70, $0C, $8C, $C1, $C5, $7E, $13, $83, $82, $7C, $63, $22, $39, $19,
    $9C, $EA, $EF, $9D, $EF, $6C, $14, $16, $34, $35, $01, $46, $94, $1E, $12, $36,
    $92, $D6, $C2, $C1, $B5, $B0, $B0, $62, $CA, $11, $BA, $BF, $0B, $47, $1B, $B1,
    $1E, $52, $4C, $52, $78, $10, $06, $0D, $81, $E0, $EC, $57, $43, $20, $B0, $0B,
    $18, $E9, $F0, $8C, $D3, $01, $31, $61, $30, $E5, $6F, $56, $92, $EE, $40, $ED,
    $70, $42, $6A, $41, $DC, $5E, $03, $B9, $BC, $51, $51, $1C, $8D, $8F, $65, $91,
    $30, $20, $AF, $45, $7A, $06, $FB, $BA, $43, $CC, $B5, $47, $31, $CC, $E2, $94,
    $AB, $09, $01, $AF, $18, $3F, $B4, $A0, $4A, $8D, $D4, $BC, $38, $71, $19, $DE,
    $A1, $5E, $8F, $3F, $7A, $47, $65, $C5, $CF, $D7, $8B, $57, $8C, $31, $C6, $04,
    $33, $48, $8F, $8C, $33, $83, $8F, $1F, $45, $0C, $DE, $9D, $FD, $77, $54, $39,
    $C3, $7D, $16, $80, $3F, $00, $74, $11, $6F, $43, $0F, $06, $80, $3C, $1F, $9E,
    $EC, $87, $B6, $AA, $F5, $E6, $43, $01, $50, $67, $C6, $85, $DB, $49, $96, $4B,
    $6E, $53, $57, $56, $1E, $F0, $9C, $FE, $BE, $B0, $B6, $5F, $F6, $BF, $80, $76,
    $1C, $3F, $50, $60, $2A, $6A, $1E, $3E, $27, $89, $DF, $29, $C6, $2F, $97, $CC,
    $67, $41, $60, $19, $FC, $89, $C2, $25, $1F, $39, $E6, $3E, $2B, $E0, $D8, $0F,
    $9D, $EE, $C2, $75, $C4, $59, $5E, $C4, $40, $98, $84, $E5, $68, $4F, $7A, $40,
    $23, $A3, $78, $EF, $C1, $45, $A8, $87, $5A, $BB, $22, $53, $C0, $52, $3F, $21,
    $54, $43, $4F, $06, $4E, $17, $47, $46, $8A, $07, $49, $A8, $ED, $8D, $84, $5A,
    $D8, $80, $3E, $E2, $D3, $40, $B4, $0B, $20, $63, $36, $07, $22, $29, $C3, $5D,
    $91, $88, $19, $7C, $6F, $99, $89, $DB, $47, $D1, $FE, $C8, $83, $F9, $02, $7E,
    $28, $49, $07, $8C, $01, $BF, $D1, $33, $8A, $47, $E0, $FA, $EB, $14, $CF, $D0,
    $09, $D1, $A0, $58, $B5, $75, $B4, $F8, $15, $C3, $EE, $B6, $01, $7F, $04, $DF,
    $36, $48, $76, $DD, $F9, $D9, $EE, $D9, $C9, $08, $7E, $FB, $4D, $14, $80, $E4,
    $45, $80, $F4, $40, $10, $10, $75, $1D, $DD, $D9, $19, $CA, $7D, $90, $0C, $0A,
    $D9, $05, $8C, $ED, $46, $D5, $3B, $3A, $B2, $09, $90, $DD, $DA, $C9, $DD, $5C,
    $24, $08, $73, $37, $C4, $18, $46, $33, $89, $D8, $0D, $94, $1B, $49, $F2, $C1,
    $DA, $FB, $DC, $35, $98, $09, $51, $40, $C5, $D8, $05, $A0, $6E, $37, $AB, $28,
    $CF, $B2, $68, $A4, $10, $67, $A1, $A4, $A8, $51, $22, $2F, $24, $06, $F4, $6B,
    $14, $61, $18, $34, $7C, $9F, $8A, $50, $95, $A0, $20, $00, $42, $B1, $45, $02,
    $8B, $10, $F4, $FC, $93, $6C, $B3, $2E, $1A, $F2, $09, $12, $F0, $EE, $7C, $72,
    $C8, $64, $1C, $EA, $E8, $68, $A8, $8D, $51, $48, $68, $02, $FB, $BF, $74, $38,
    $3C, $E0, $43, $EF, $26, $CF, $97, $70, $51, $F3, $60, $55, $10, $EA, $74, $B7,
    $D8, $BE, $00, $B8, $0A, $50, $2B, $B0, $7E, $23, $B9, $F1, $AB, $0B, $AD, $E0,
    $4A, $B2, $BE, $C4, $4E, $01, $48, $B4, $76, $03, $70, $F7, $F1, $3E, $D3, $01,
    $D0, $10, $0A, $D8, $6D, $77, $10, $6A, $F6, $7F, $E2, $34, $F0, $51, $C1, $E7,
    $6E, $AD, $7D, $D7, $69, $08, $75, $F0, $09, $DE, $89, $04, $4E, $25, $10, $5B,
    $C2, $DA, $02, $A9, $1F, $13, $0F, $51, $1F, $86, $C6, $AF, $2E, $A0, $51, $03,
    $1C, $E6, $08, $42, $C0, $F5, $A5, $88, $36, $BB, $3B, $36, $10, $68, $D0, $CD,
    $FF, $10, $C9, $4F, $3F, $38, $54, $32, $40, $E5, $03, $8E, $56, $46, $86, $26,
    $54, $04, $4B, $6F, $2F, $08, $D9, $90, $5D, $3F, $68, $11, $4C, $F4, $44, $9B,
    $18, $BC, $86, $73, $EF, $2D, $6B, $2C, $4E, $5E, $53, $22, $56, $98, $4C, $81,
    $25, $8F, $06, $78, $40, $04, $03, $B6, $19, $D6, $34, $01, $F6, $4F, $8F, $1C,
    $28, $15, $3A, $6C, $02, $D1, $1C, $08, $08, $C6, $9F, $B9, $64, $01, $99, $06,
    $28, $3B, $23, $EC, $C8, $EA, $2B, $0F, $52, $6F, $12, $CF, $80, $62, $36, $84,
    $0A, $BC, $C5, $50, $CD, $62, $C1, $40, $9F, $20, $6A, $65, $50, $34, $BC, $03,
    $5F, $5A, $D9, $22, $09, $1F, $0F, $59, $50, $2E, $5A, $3F, $2C, $8D, $7C, $2E,
    $97, $DB, $F6, $34, $0C, $C6, $0C, $70, $03, $D9, $71, $DA, $72, $DB, $73, $E0,
    $7F, $FF, $F6, $F1, $12, $3D, $73, $72, $71, $70, $74, $30, $09, $70, $71, $72,
    $73, $74, $16, $61, $59, $37, $D7, $33, $0B, $B2, $14, $8E, $E6, $3F, $CC, $25,
    $B9, $94, $82, $B0, $F5, $66, $0F, $F5, $0A, $95, $E4, $01, $FF, $10, $D4, $6A,
    $E5, $0C, $FD, $F1, $D1, $73, $54, $60, $81, $8F, $6D, $C1, $E0, $08, $66, $4F,
    $08, $45, $60, $93, $70, $02, $AA, $5D, $CF, $A1, $2E, $82, $7E, $43, $D1, $C0,
    $B0, $D1, $74, $20, $C1, $E9, $18, $05, $EA, $BE, $64, $52, $30, $E2, $08, $25,
    $00, $29, $09, $B0, $AD, $96, $B9, $6C, $0B, $10, $00, $FF, $CC, $E2, $A5, $6E,
    $1F, $49, $1F, $DD, $40, $50, $02, $48, $DC, $4E, $73, $5B, $37, $5C, $77, $18,
    $DE, $E1, $A2, $50, $B3, $0C, $52, $C1, $DB, $BF, $40, $70, $DC, $70, $58, $D9,
    $C0, $D9, $11, $CA, $DD, $EA, $EF, $96, $A1, $DE, $22, $E6, $06, $DD, $D8, $D1,
    $A5, $D5, $02, $38, $AC, $DC, $05, $D8, $09, $90, $9B, $05, $92, $59, $2F, $E9,
    $66, $9B, $D4, $30, $9A, $4A, $54, $DB, $40, $0C, $F2, $BF, $40, $DB, $5A, $30,
    $06, $04, $02, $28, $CB, $DD, $5D, $F0, $DD, $7C, $2B, $5D, $DB, $43, $48, $0A,
    $10, $DF, $E3, $5C, $D8, $E5, $D9, $CC, $DA, $1B, $E8, $0C, $37, $06, $C9, $3B,
    $DE, $C5, $75, $4E, $63, $F7, $4E, $60, $EB, $01, $DA, $25, $28, $02, $BE, $5C,
    $6B, $FD, $60, $5F, $EB, $72, $DD, $1E, $77, $BA, $10, $74, $03, $83, $CA, $FF,
    $F0, $93, $55, $B0, $26, $50, $77, $04, $F7, $DE, $F7, $DA, $9B, $00, $B4, $6F,
    $65, $39, $D6, $74, $02, $A6, $A6, $20, $5B, $F7, $04, $9E, $68, $9A, $75, $33,
    $5A, $DB, $12, $30, $4D, $37, $59, $3B, $08, $5A, $66, $E4, $EB, $DA, $B7, $2E,
    $CC, $64, $5B, $AA, $FC, $CE, $04, $89, $DE, $2B, $E8, $26, $B8, $CA, $88, $02,
    $2A, $40, $A0, $12, $58, $8A, $9A, $44, $DD, $C9, $C1, $0C, $94, $B8, $7D, $00,
    $79, $AC, $5A, $9C, $D8, $59, $0E, $50, $50, $AC, $3D, $E0, $11, $44, $50, $24,
    $18, $7D, $05, $0E, $52, $40, $7A, $81, $0F, $4E, $95, $97, $0B, $19, $AC, $6A,
    $41, $C8, $59, $C8, $5E, $56, $D6, $7C, $0B, $C7, $F4, $44, $F0, $52, $41, $C1,
    $0D, $A0, $07, $4F, $57, $E1, $05, $42, $7E, $BB, $0D, $75, $69, $21, $A5, $B9,
    $0A, $AF, $FC, $A3, $11, $89, $D7, $4A, $C6, $16, $DC, $F3, $F3, $AB, $7D, $7D,
    $F5, $4D, $7A, $4B, $80, $34, $14, $BF, $B6, $33, $41, $93, $3F, $22, $6F, $2F,
    $52, $14, $69, $C3, $82, $05, $B1, $13, $7C, $21, $09, $48, $09, $C5, $75, $DF,
    $7F, $4F, $13, $20, $4E, $AA, $08, $A7, $07, $AB, $C0, $26, $60, $17, $7C, $44,
    $9E, $59, $20, $43, $36, $22, $62, $2F, $5F, $7E, $F2, $A1, $8D, $02, $35, $44,
    $17, $42, $F5, $08, $3F, $50, $98, $04, $3A, $08, $21, $EB, $D2, $4F, $1C, $4A,
    $41, $0B, $BD, $BD, $3F, $90, $F7, $DF, $B6, $9A, $00, $59, $53, $14, $4B, $08,
    $55, $E0, $01, $CA, $E6, $4D, $94, $72, $9D, $50, $95, $8B, $7B, $10, $01, $7B,
    $44, $2F, $05, $E0, $8D, $14, $3E, $34, $0E, $E8, $5B, $6B, $5B, $33, $DC, $11,
    $75, $13, $E8, $0F, $84, $A3, $6D, $A3, $B5, $9E, $FB, $1C, $3F, $03, $E2, $8B,
    $39, $DC, $63, $FF, $E9, $29, $D0, $3D, $56, $7F, $0F, $89, $F8, $29, $F0, $0A,
    $0F, $8E, $51, $05, $FD, $A2, $FF, $D6, $33, $1A, $F7, $29, $C1, $39, $F9, $0F,
    $8C, $80, $D5, $0C, $DB, $EA, $24, $BE, $14, $A8, $42, $04, $DD, $00, $6C, $05,
    $64, $D9, $0E, $89, $79, $C6, $15, $08, $DA, $04, $A3, $1B, $9C, $31, $C9, $8E,
    $88, $C1, $D4, $5F, $42, $75, $14, $C4, $C9, $6F, $0F, $09, $CE, $30, $22, $DC,
    $56, $13, $F1, $0B, $59, $86, $46, $61, $E1, $F0, $12, $83, $00, $12, $26, $D6,
    $36, $40, $B4, $42, $D9, $AF, $7C, $42, $EF, $A2, $CC, $EC, $22, $CC, $20, $5F,
    $7C, $80, $7F, $96, $C0, $4A, $67, $E8, $1A, $80, $16, $0C, $0E, $1E, $BE, $2B,
    $CD, $33, $5C, $F6, $16, $8A, $96, $BE, $1B, $09, $89, $44, $93, $5A, $89, $EA,
    $CA, $5F, $83, $BA, $E0, $8D, $20, $29, $C2, $5B, $E9, $39, $AE, $65, $68, $EF,
    $C2, $7C, $3C, $26, $0D, $DE, $FE, $DA, $B4, $12, $26, $B6, $55, $26, $72, $7C,
    $30, $63, $70, $E1, $B0, $4D, $74, $C8, $75, $14, $42, $5F, $7A, $10, $39, $37,
    $E1, $EC, $75, $9B, $3C, $E6, $19, $3B, $62, $5F, $69, $AC, $13, $2E, $DB, $48,
    $43, $E8, $2E, $32, $C7, $13, $B7, $5D, $4A, $0C, $77, $F8, $15, $14, $BB, $08,
    $DC, $42, $C3, $BD, $C1, $E7, $02, $89, $09, $00, $0B, $B7, $5A, $04, $53, $24,
    $D6, $1C, $7F, $DD, $86, $D6, $0B, $63, $9B, $D8, $EB, $0F, $FF, $4A, $EB, $E8,
    $16, $DA, $1C, $41, $48, $ED, $12, $9E, $11, $30, $EF, $10, $10, $AD, $9D, $66,
    $20, $20, $0C, $37, $81, $E8, $DA, $24, $CF, $D7, $FF, $F4, $1B, $3A, $21, $70,
    $75, $9D, $40, $10, $2F, $39, $54, $AF, $B6, $EE, $0B, $DB, $4C, $45, $02, $7B,
    $04, $89, $73, $0B, $14, $05, $78, $31, $B2, $14, $D5, $77, $76, $18, $1C, $05,
    $6C, $0A, $17, $24, $E6, $8E, $2C, $74, $59, $8A, $92, $65, $3C, $46, $58, $8E,
    $30, $81, $83, $09, $EC, $24, $67, $6C, $0F, $77, $08, $8F, $5D, $F0, $C9, $A1,
    $06, $7B, $60, $10, $AF, $68, $88, $2C, $4C, $44, $5F, $00, $11, $7A, $68, $F0,
    $A9, $B3, $1B, $4E, $EC, $1F, $83, $C0, $08, $06, $04, $4E, $F6, $24, $A2, $E6,
    $A5, $14, $40, $74, $AC, $EB, $6F, $F4, $02, $87, $3E, $B0, $07, $8B, $7A, $48,
    $F8, $31, $FF, $B8, $7B, $B1, $0D, $68, $5E, $53, $19, $13, $1C, $4A, $08, $01,
    $D0, $65, $44, $86, $23, $A3, $3B, $09, $95, $83, $4F, $E4, $7D, $27, $F6, $47,
    $8F, $D4, $27, $EA, $22, $7C, $9C, $7F, $4C, $61, $80, $78, $16, $04, $70, $CF,
    $6E, $3E, $15, $47, $26, $7C, $DC, $79, $C3, $68, $D9, $1A, $3E, $8B, $50, $3C,
    $38, $E4, $21, $17, $13, $14, $84, $86, $B3, $79, $4C, $57, $D2, $7F, $0A, $BC,
    $A3, $1B, $28, $8B, $4E, $9D, $DD, $01, $DD, $02, $DA, $65, $21, $1A, $8F, $A1,
    $61, $00, $20, $18, $2F, $70, $03, $07, $01, $DC, $2A, $A2, $24, $8F, $41, $E5,
    $2B, $34, $9E, $6D, $18, $1C, $41, $76, $EE, $2D, $66, $33, $BF, $D7, $F1, $2F,
    $85, $6A, $29, $20, $1D, $0A, $1C, $02, $6A, $05, $48, $2F, $02, $41, $07, $CD,
    $57, $2D, $2B, $2A, $1F, $D9, $91, $6D, $C9, $6A, $2C, $28, $D9, $FA, $2E, $1E,
    $02, $9E, $BA, $73, $40, $7D, $46, $1A, $BD, $36, $5A, $AF, $E9, $9A, $50, $6A,
    $10, $7A, $AE, $5E, $44, $99, $E8, $81, $99, $7E, $92, $4D, $DE, $40, $7D, $DE,
    $E4, $0D, $7C, $2C, $8B, $46, $7E, $5B, $F0, $57, $C8, $80, $7E, $14, $B1, $11,
    $46, $4E, $18, $18, $2D, $68, $80, $54, $EA, $BC, $23, $11, $8A, $06, $8A, $94,
    $C3, $88, $B5, $10, $14, $80, $47, $7C, $83, $01, $8E, $F8, $88, $11, $70, $1C,
    $43, $B6, $C2, $28, $1F, $14, $A4, $18, $21, $00, $2D, $D8, $6E, $58, $69, $5A,
    $53, $6B, $77, $21, $DB, $74, $7B, $BC, $24, $58, $18, $10, $7F, $31, $EF, $18,
    $5F, $C1, $6D, $79, $E4, $6E, $34, $1B, $EC, $50, $73, $27, $1B, $32, $9C, $A8,
    $84, $57, $5C, $DA, $44, $DD, $5A, $A1, $08, $31, $D4, $4D, $F0, $53, $DF, $CA,
    $DA, $7B, $24, $DA, $A0, $6D, $30, $25, $9B, $6C, $9B, $73, $55, $BF, $B7, $53,
    $79, $5A, $64, $0D, $08, $07, $43, $54, $4E, $9B, $01, $03, $A3, $0E, $F2, $77,
    $E6, $C0, $8E, $98, $85, $03, $F2, $DD, $80, $1B, $17, $5E, $40, $53, $68, $F4,
    $C6, $46, $2D, $5B, $48, $00, $7C, $1B, $62, $8F, $94, $1C, $34, $29, $6A, $E5,
    $31, $AA, $B5, $23, $45, $A8, $54, $91, $41, $FF, $16, $42, $A8, $88, $BC, $12,
    $75, $F3, $89, $AF, $E0, $A5, $4A, $C5, $FF, $97, $EB, $15, $90, $40, $29, $30,
    $D4, $86, $70, $D7, $9F, $A8, $29, $B2, $F9, $42, $F7, $17, $D3, $75, $E5, $17,
    $A2, $0C, $58, $F9, $74, $43, $3A, $89, $50, $2A, $76, $AD, $F2, $2D, $8E, $81,
    $CC, $51, $B4, $05, $6D, $A3, $12, $5A, $D0, $26, $21, $4D, $4C, $12, $EC, $A8,
    $46, $7A, $2C, $8F, $1C, $56, $7D, $A6, $81, $18, $11, $32, $C7, $43, $5A, $EC,
    $8A, $72, $D5, $0B, $40, $20, $46, $7E, $AE, $0C, $30, $87, $14, $59, $1C, $9F,
    $05, $83, $58, $2C, $78, $94, $10, $D9, $50, $D4, $73, $24, $E7, $46, $10, $3D,
    $20, $D6, $B0, $7B, $28, $75, $51, $0B, $92, $92, $88, $46, $6F, $0E, $39, $D3,
    $7C, $CA, $DF, $94, $30, $BA, $C1, $49, $EB, $F0, $1F, $64, $C3, $E0, $55, $32,
    $94, $D2, $50, $07, $D1, $80, $8E, $42, $C7, $45, $C7, $2A, $8A, $B9, $A3, $07,
    $00, $B3, $5A, $D1, $3C, $EF, $04, $2F, $1D, $1E, $0D, $6A, $00, $99, $18, $B0,
    $9D, $B5, $A2, $54, $91, $98, $68, $6F, $2A, $17, $3B, $CB, $1F, $46, $C7, $06,
    $A4, $B0, $81, $07, $93, $B7, $EC, $C1, $55, $DC, $AD, $E1, $8A, $32, $6F, $F4,
    $AC, $08, $96, $65, $59, $96, $08, $08, $0C, $0C, $0C, $10, $F6, $CB, $11, $51,
    $96, $7A, $52, $57, $5D, $16, $1A, $EE, $C8, $E0, $9E, $EC, $33, $D7, $56, $72,
    $09, $38, $2B, $5A, $F0, $EC, $5A, $39, $41, $F1, $2D, $DB, $12, $5F, $D8, $0F,
    $6A, $CC, $B9, $8A, $96, $3C, $25, $31, $40, $D7, $6A, $BA, $DD, $3F, $A1, $CC,
    $44, $1B, $E0, $5E, $DE, $35, $D4, $53, $C4, $5A, $EF, $30, $DE, $5C, $E2, $F4,
    $6C, $ED, $46, $25, $DA, $14, $CE, $6E, $B3, $55, $BD, $55, $8E, $3D, $D9, $7D,
    $EA, $DD, $EA, $C7, $F6, $85, $88, $0A, $E8, $DB, $5D, $E4, $05, $EA, $D2, $5A,
    $7B, $99, $6B, $89, $06, $2D, $10, $4D, $E4, $2E, $C1, $64, $AE, $02, $37, $F5,
    $50, $DB, $64, $16, $28, $AE, $F1, $6E, $CB, $5D, $5E, $17, $0C, $24, $16, $DA,
    $78, $67, $36, $92, $6D, $70, $04, $1D, $E9, $F0, $3B, $7C, $B4, $3D, $36, $4A,
    $58, $EF, $3F, $0C, $6C, $3B, $ED, $06, $8A, $42, $56, $44, $2C, $F6, $C3, $0C,
    $60, $6A, $51, $8A, $05, $65, $B1, $D4, $63, $C1, $7D, $DB, $3D, $01, $C1, $CD,
    $06, $01, $C3, $53, $0D, $70, $1E, $F8, $0D, $8A, $72, $72, $50, $8F, $DC, $3F,
    $FD, $96, $88, $CD, $44, $01, $46, $30, $83, $57, $EB, $9C, $58, $60, $9B, $6D,
    $B8, $0C, $18, $5A, $99, $DC, $50, $0E, $09, $18, $EA, $7D, $06, $9E, $31, $11,
    $A9, $F2, $55, $E4, $5A, $28, $DA, $A6, $A9, $90, $52, $29, $B9, $14, $B7, $15,
    $5A, $A2, $1C, $D8, $7D, $0B, $28, $40, $C5, $F3, $07, $A8, $DA, $20, $89, $C3,
    $57, $59, $CB, $63, $CE, $6D, $0B, $37, $40, $85, $43, $10, $8B, $55, $D2, $3B,
    $B5, $8C, $2C, $1B, $28, $52, $F7, $3A, $B3, $64, $B3, $6E, $14, $03, $10, $73,
    $41, $0D, $50, $91, $0F, $76, $44, $89, $DA, $9C, $D0, $54, $D9, $0D, $19, $A9,
    $E5, $21, $1B, $7C, $5A, $0B, $2D, $4B, $F9, $68, $48, $5C, $89, $D9, $BA, $05,
    $27, $B5, $49, $48, $0E, $1E, $C0, $EB, $16, $5F, $B8, $55, $B6, $1F, $8B, $51,
    $08, $C3, $02, $8B, $09, $A2, $0C, $74, $B1, $C0, $DC, $D9, $C1, $27, $DC, $B3,
    $C3, $0F, $ED, $2E, $94, $75, $E3, $53, $97, $64, $F9, $89, $37, $62, $21, $6D,
    $AD, $37, $64, $DE, $FA, $DD, $89, $02, $18, $38, $01, $8B, $90, $85, $CD, $08,
    $0C, $A9, $21, $36, $AF, $84, $03, $B3, $88, $B4, $AB, $06, $81, $61, $66, $9D,
    $96, $81, $B0, $0B, $48, $5F, $1F, $B0, $D4, $34, $83, $8C, $1A, $93, $ED, $8B,
    $10, $3E, $BC, $09, $10, $8C, $DB, $35, $05, $D0, $26, $7D, $D4, $2B, $68, $DF,
    $30, $13, $42, $2C, $C0, $54, $1C, $FF, $81, $AB, $6A, $59, $C1, $EC, $BC, $B2,
    $FE, $F9, $00, $30, $70, $85, $64, $56, $E7, $05, $9D, $10, $70, $BA, $0F, $8D,
    $6B, $71, $2F, $BD, $B2, $19, $E2, $E1, $1C, $9C, $23, $45, $2B, $58, $B2, $B9,
    $0B, $0C, $8B, $50, $85, $60, $0B, $11, $54, $47, $6B, $76, $0B, $09, $50, $FF,
    $00, $09, $5A, $A6, $AD, $D7, $45, $22, $8F, $B5, $2D, $D3, $46, $A9, $FD, $DB,
    $82, $62, $0A, $0A, $BD, $07, $20, $0F, $8F, $DA, $6D, $21, $F6, $FB, $63, $95,
    $68, $2D, $95, $5C, $33, $8A, $EB, $6E, $EF, $27, $E2, $B1, $B5, $0B, $04, $9E,
    $43, $5F, $F0, $3B, $9D, $2E, $6B, $01, $F9, $1A, $06, $FF, $F4, $2B, $48, $3F,
    $3B, $75, $39, $01, $D7, $01, $22, $28, $7D, $38, $8B, $8D, $50, $F5, $A8, $7A,
    $40, $FB, $7B, $F7, $60, $A8, $B4, $17, $41, $49, $6B, $30, $D2, $3B, $95, $9F,
    $B3, $B1, $6F, $E9, $7E, $F7, $9D, $B1, $89, $0D, $43, $37, $58, $6C, $6C, $A3,
    $7C, $D4, $9D, $CC, $03, $DA, $13, $54, $B0, $BF, $F6, $EB, $37, $90, $8D, $29,
    $8D, $0C, $33, $96, $FB, $31, $53, $B0, $2F, $C8, $9E, $79, $89, $D1, $8B, $14,
    $96, $E0, $6C, $B1, $C6, $67, $60, $06, $89, $2A, $C4, $72, $8A, $87, $D3, $64,
    $2F, $E2, $4B, $79, $C7, $E5, $A6, $EE, $27, $63, $ED, $A3, $41, $58, $63, $9C,
    $9F, $31, $19, $66, $DB, $70, $84, $31, $24, $75, $24, $B4, $88, $5A, $21, $44,
    $01, $4E, $8D, $17, $19, $68, $A0, $4C, $07, $1D, $34, $BE, $FC, $40, $75, $2F,
    $46, $5F, $46, $30, $55, $46, $DB, $C0, $41, $D1, $3B, $0C, $D9, $CF, $99, $09,
    $17, $B3, $C6, $49, $75, $2E, $1E, $3F, $36, $F6, $08, $30, $7B, $74, $B9, $47,
    $2F, $75, $03, $D6, $EE, $B7, $B5, $E4, $B1, $95, $67, $56, $57, $A6, $7A, $1C,
    $97, $70, $BC, $B4, $F5, $72, $18, $56, $85, $14, $D1, $52, $10, $52, $DD, $4C,
    $B3, $20, $DD, $24, $10, $90, $D2, $C1, $3F, $C6, $68, $6A, $97, $E8, $89, $DF,
    $90, $B4, $47, $74, $63, $C5, $6D, $8B, $9E, $70, $3F, $1A, $EF, $A7, $EB, $10,
    $8B, $D8, $A2, $87, $EF, $70, $0C, $20, $31, $FA, $D6, $AD, $60, $16, $D6, $DB,
    $48, $1C, $2E, $41, $80, $AB, $F1, $50, $18, $52, $80, $D4, $DE, $E1, $74, $ED,
    $F7, $03, $61, $54, $AD, $5B, $0A, $B1, $06, $31, $61, $73, $06, $01, $42, $8E,
    $63, $FC, $05, $3B, $0D, $6A, $8C, $99, $DC, $6F, $23, $C6, $02, $4D, $43, $42,
    $3B, $61, $83, $62, $76, $78, $62, $29, $B9, $85, $54, $CA, $6D, $E9, $06, $9B,
    $98, $3C, $98, $FC, $98, $04, $42, $18, $A4, $CE, $00, $0D, $88, $04, $30, $DD,
    $09, $10, $8E, $00, $6F, $0F, $10, $DC, $28, $E5, $74, $EC, $5F, $1C, $36, $C2,
    $6D, $46, $15, $9D, $48, $53, $18, $28, $46, $9A, $2A, $02, $B2, $41, $67, $85,
    $B6, $5C, $16, $FF, $22, $D8, $05, $04, $91, $89, $45, $76, $73, $40, $7D, $53,
    $2F, $79, $A3, $34, $9A, $89, $7C, $9A, $15, $DB, $43, $B7, $91, $62, $0E, $4B,
    $AC, $DD, $EF, $18, $D1, $C1, $A8, $1C, $57, $57, $87, $3A, $D3, $B0, $CD, $CD,
    $1E, $C8, $84, $9E, $BA, $02, $13, $39, $62, $D0, $46, $B1, $C8, $96, $02, $14,
    $EC, $CE, $1E, $CC, $98, $07, $D4, $19, $3B, $86, $D8, $06, $C2, $35, $B4, $04,
    $75, $93, $EB, $9D, $53, $69, $02, $BE, $91, $F3, $37, $B5, $87, $3F, $A1, $5C,
    $44, $2D, $E3, $00, $96, $B0, $FA, $06, $E3, $F3, $6B, $1E, $84, $71, $4F, $F1,
    $BF, $86, $60, $57, $79, $E0, $44, $75, $DA, $8B, $08, $39, $4A, $46, $B7, $44,
    $BB, $04, $D3, $89, $04, $99, $0C, $A1, $45, $D2, $70, $A9, $B2, $7D, $FF, $73,
    $1C, $02, $18, $43, $5B, $10, $D1, $DA, $51, $B9, $AB, $49, $40, $1C, $1A, $53,
    $6D, $78, $B1, $0D, $93, $01, $36, $A1, $EB, $9D, $B1, $99, $41, $F8, $0D, $09,
    $23, $C8, $B2, $51, $50, $B1, $CB, $F6, $DB, $59, $1C, $3C, $59, $18, $53, $89,
    $CB, $42, $14, $4B, $10, $46, $91, $50, $9B, $85, $02, $E6, $CC, $EB, $B7, $4F,
    $C9, $0A, $1A, $D6, $FF, $5E, $F8, $EB, $41, $CF, $52, $87, $68, $63, $EE, $50,
    $54, $BC, $2A, $30, $B3, $1C, $AB, $FA, $0E, $A8, $8F, $81, $EB, $55, $90, $C7,
    $02, $25, $54, $35, $36, $06, $89, $87, $03, $BA, $84, $0C, $D5, $2B, $FE, $C1,
    $D0, $1A, $44, $6E, $BB, $40, $26, $7A, $0C, $E9, $D9, $16, $B1, $E1, $A2, $73,
    $B8, $34, $E9, $94, $B7, $33, $F5, $34, $15, $A9, $A2, $5B, $C3, $68, $9D, $7A,
    $46, $7D, $18, $4C, $D6, $EB, $B8, $1C, $08, $91, $B2, $21, $BB, $2C, $2E, $CD,
    $AF, $2C, $A5, $40, $81, $80, $C0, $16, $AE, $3F, $00, $CC, $81, $13, $29, $AA,
    $E4, $56, $23, $34, $48, $02, $0C, $70, $85, $38, $52, $70, $36, $17, $86, $0B,
    $D8, $1E, $75, $18, $D9, $62, $7F, $D0, $FA, $D5, $A9, $5A, $03, $17, $FB, $58,
    $E4, $4D, $50, $71, $A1, $78, $E0, $3F, $89, $E0, $5D, $CC, $47, $41, $0B, $86,
    $01, $93, $32, $81, $42, $C4, $68, $EB, $4C, $0F, $95, $F5, $10, $0F, $A1, $62,
    $C1, $0F, $DD, $03, $68, $88, $AD, $B1, $36, $E2, $95, $73, $E8, $19, $59, $04,
    $84, $8A, $85, $CC, $E1, $2C, $A3, $4C, $6D, $05, $14, $5B, $7D, $49, $2B, $54,
    $06, $B8, $5D, $D0, $FF, $E4, $8A, $90, $00, $5C, $B1, $F4, $4A, $04, $E4, $20,
    $B5, $CE, $5C, $6C, $06, $71, $0B, $5A, $F0, $0E, $C3, $CF, $37, $6B, $2E, $FF,
    $62, $55, $52, $8E, $EB, $AB, $67, $4B, $BA, $8E, $77, $03, $5A, $75, $A5, $7B,
    $61, $8E, $A7, $D9, $29, $15, $31, $EF, $80, $EC, $99, $A1, $22, $DE, $9E, $4D,
    $E8, $21, $C7, $F6, $8D, $64, $1E, $47, $C1, $17, $59, $3F, $62, $12, $55, $A1,
    $02, $DB, $66, $7B, $1F, $F8, $11, $51, $3F, $55, $D4, $B1, $1E, $4E, $B1, $39,
    $22, $08, $8F, $99, $4D, $D0, $86, $CF, $D7, $36, $66, $32, $8B, $1E, $CE, $D6,
    $4D, $D4, $C4, $0B, $7C, $80, $6B, $8C, $03, $11, $CC, $44, $5D, $66, $D4, $01,
    $A7, $D4, $48, $AA, $E0, $BF, $C1, $30, $6E, $B6, $AB, $38, $0D, $EB, $4B, $1B,
    $0D, $6D, $AD, $B0, $33, $39, $7D, $39, $9C, $3F, $C7, $89, $60, $C6, $C2, $89,
    $39, $F9, $46, $D8, $60, $70, $3F, $45, $A7, $16, $5D, $D8, $89, $13, $42, $B6,
    $B1, $60, $B8, $8E, $69, $45, $C7, $17, $DC, $0B, $94, $A1, $06, $9A, $16, $45,
    $9C, $52, $6C, $2F, $60, $2C, $30, $3C, $53, $1F, $89, $9A, $A5, $62, $2C, $C5,
    $19, $73, $4B, $E4, $E4, $04, $E4, $EB, $E1, $06, $28, $D1, $D7, $C0, $A6, $42,
    $33, $58, $B7, $59, $49, $8C, $80, $87, $1B, $A4, $6B, $7F, $0E, $43, $A2, $5D,
    $9A, $B6, $2F, $20, $4D, $42, $58, $1A, $F2, $8F, $DC, $8C, $0C, $46, $EC, $07,
    $8D, $50, $4F, $70, $21, $1C, $C8, $46, $9C, $12, $4E, $70, $70, $03, $A4, $00,
    $8E, $E0, $C9, $4F, $72, $B0, $21, $9A, $14, $43, $83, $53, $28, $60, $4F, $11,
    $DF, $66, $0E, $08, $30, $0D, $38, $85, $DE, $1B, $7B, $2B, $B6, $E3, $09, $40,
    $98, $C1, $D8, $CA, $03, $CA, $0E, $B8, $03, $BE, $6E, $50, $15, $C2, $08, $48,
    $63, $DD, $5B, $58, $DF, $09, $04, $A4, $49, $3A, $43, $13, $5B, $C5, $46, $C1,
    $8A, $60, $A9, $3B, $33, $75, $45, $2B, $36, $F5, $43, $28, $7C, $32, $C3, $DE,
    $DC, $1B, $D0, $01, $E0, $AC, $CB, $D9, $CC, $52, $4D, $CB, $DE, $EB, $30, $82,
    $6D, $EB, $03, $E2, $31, $70, $02, $68, $3F, $07, $52, $03, $D0, $8F, $90, $98,
    $BB, $83, $7A, $81, $54, $7F, $89, $D3, $81, $1F, $9C, $85, $1F, $EB, $09, $39,
    $58, $08, $BC, $D3, $84, $F1, $8E, $10, $4F, $BD, $A1, $FA, $8E, $F2, $D3, $00,
    $A3, $33, $E5, $BE, $FD, $10, $DD, $47, $84, $47, $50, $DC, $4B, $DD, $7E, $3F,
    $0B, $99, $BA, $D1, $A1, $DC, $0B, $21, $CA, $DC, $B6, $0B, $18, $FA, $47, $68,
    $DC, $77, $58, $B3, $3B, $54, $8B, $23, $81, $C8, $B2, $35, $FA, $37, $80, $8C,
    $0D, $4C, $48, $6D, $B3, $8B, $6A, $71, $CE, $9C, $46, $1C, $16, $0E, $7B, $E9,
    $DC, $54, $50, $28, $03, $4C, $4F, $48, $51, $1B, $20, $D7, $46, $06, $58, $20,
    $8D, $3C, $CA, $B0, $0C, $C5, $57, $70, $2E, $8B, $01, $EA, $C2, $E8, $83, $02,
    $1B, $C6, $9A, $F4, $B1, $18, $DD, $C0, $9D, $AA, $C6, $60, $94, $68, $27, $4C,
    $96, $E8, $68, $1C, $0E, $E4, $3E, $56, $C6, $06, $E2, $50, $14, $E8, $22, $24,
    $44, $4B, $97, $2C, $A0, $05, $AE, $32, $7F, $18, $CC, $D4, $C1, $4A, $DC, $23,
    $75, $DC, $04, $26, $8A, $61, $0D, $95, $C3, $30, $29, $B4, $E3, $05, $C9, $80,
    $CA, $35, $8F, $24, $0B, $1C, $57, $42, $BF, $44, $A1, $BE, $12, $8E, $2A, $C5,
    $F4, $26, $C6, $40, $BC, $88, $8E, $AF, $52, $C4, $DB, $68, $AE, $8F, $A4, $7C,
    $25, $7E, $43, $D9, $DF, $2B, $57, $F1, $F6, $E6, $5A, $5B, $F4, $01, $37, $0A,
    $E0, $01, $C2, $89, $0F, $34, $44, $92, $88, $6D, $60, $77, $93, $BB, $35, $2E,
    $39, $5F, $18, $B7, $22, $C6, $89, $0A, $D3, $11, $B1, $6E, $2D, $53, $0F, $EB,
    $DB, $BF, $FD, $A1, $2E, $D8, $DC, $48, $24, $3C, $E0, $DD, $14, $B6, $8B, $1F,
    $2A, $DE, $0C, $5B, $7B, $AC, $E0, $0A, $92, $D9, $E8, $2B, $0A, $B8, $D7, $CB,
    $94, $53, $37, $63, $52, $7C, $10, $70, $E0, $05, $9B, $04, $6A, $EE, $9B, $D8,
    $B6, $19, $F6, $99, $C4, $2A, $29, $F0, $8D, $34, $00, $07, $5A, $40, $05, $E8,
    $89, $75, $D4, $7D, $CF, $6D, $DF, $99, $17, $EC, $8F, $37, $04, $B7, $42, $78,
    $39, $41, $78, $59, $3D, $8D, $52, $C3, $01, $26, $EA, $57, $6A, $70, $6C, $6F,
    $DB, $46, $50, $02, $48, $08, $30, $D8, $C9, $07, $70, $68, $CE, $D1, $1A, $DC,
    $6C, $CA, $DC, $E0, $04, $CC, $08, $DD, $1D, $6F, $B4, $38, $14, $A3, $CB, $D8,
    $C4, $7D, $E2, $D8, $F2, $05, $C3, $34, $5A, $A3, $44, $6D, $83, $09, $8F, $A2,
    $19, $A2, $73, $EC, $03, $CD, $82, $13, $C4, $9F, $55, $1E, $3E, $D2, $EA, $05,
    $74, $0D, $DD, $E6, $B0, $40, $CB, $23, $54, $85, $EC, $02, $51, $2A, $BC, $D6,
    $35, $34, $DE, $90, $12, $80, $7A, $15, $FC, $39, $D8, $F2, $1F, $EF, $B6, $AD,
    $63, $21, $19, $BA, $2F, $BF, $AA, $C8, $22, $CC, $39, $08, $74, $06, $CA, $52,
    $E5, $2E, $C3, $00, $2F, $91, $99, $AC, $CC, $0E, $B2, $18, $DC, $DD, $DD, $D0,
    $FB, $68, $40, $70, $AF, $E1, $5A, $FB, $62, $21, $56, $17, $22, $B0, $80, $CA,
    $7F, $18, $6C, $89, $0E, $11, $C5, $E3, $81, $EA, $A3, $08, $B6, $3D, $3B, $D7,
    $C9, $1B, $8C, $CD, $73, $4B, $16, $1F, $46, $28, $03, $0C, $0C, $E0, $C0, $87,
    $78, $CD, $49, $BF, $02, $49, $0A, $6F, $B9, $49, $B8, $1B, $22, $78, $DE, $F1,
    $DB, $75, $83, $87, $C1, $23, $41, $7A, $D8, $E2, $79, $17, $6F, $20, $F0, $DE,
    $C9, $AE, $D9, $5F, $D9, $01, $78, $67, $93, $7C, $C0, $77, $47, $BB, $67, $B0,
    $E6, $2F, $01, $EC, $0C, $24, $DA, $CD, $75, $BC, $68, $C7, $E2, $03, $4D, $37,
    $02, $8C, $42, $89, $03, $7C, $B3, $24, $47, $13, $10, $C0, $3F, $D5, $C9, $34,
    $2F, $3B, $93, $37, $D3, $B8, $01, $CA, $63, $C3, $62, $93, $71, $1D, $D5, $15,
    $05, $8C, $C5, $A2, $21, $57, $44, $73, $7D, $B1, $CB, $E0, $9F, $58, $F5, $E6,
    $D4, $37, $48, $77, $D8, $C1, $92, $3C, $9F, $72, $E6, $19, $B0, $56, $C9, $31,
    $D8, $D9, $01, $DA, $4F, $97, $23, $AA, $B5, $95, $CB, $42, $49, $50, $04, $48,
    $18, $D0, $B3, $D1, $10, $73, $FB, $15, $41, $81, $66, $1B, $42, $85, $89, $10,
    $10, $BF, $18, $D8, $1C, $81, $08, $1E, $66, $E0, $26, $3A, $B9, $32, $5F, $AF,
    $32, $9A, $E1, $89, $4C, $53, $2F, $6B, $9E, $EB, $3F, $B6, $13, $6A, $B2, $90,
    $72, $69, $DA, $41, $04, $68, $41, $47, $68, $04, $FA, $74, $7A, $64, $9B, $50,
    $16, $C6, $4A, $71, $70, $C4, $FD, $C6, $B0, $0E, $B7, $C5, $17, $BC, $7E, $7E,
    $E4, $87, $A1, $B7, $14, $61, $AD, $82, $71, $08, $1D, $12, $D0, $D1, $BE, $5A,
    $07, $44, $6B, $52, $C5, $F1, $14, $0E, $6D, $8B, $C8, $EB, $C0, $BD, $CD, $CB,
    $AD, $D2, $4A, $28, $CF, $CD, $56, $30, $80, $07, $69, $08, $D0, $9F, $DC, $EB,
    $8D, $19, $30, $D0, $E1, $E3, $FF, $C2, $DA, $99, $20, $B6, $3A, $DB, $1C, $91,
    $49, $06, $5A, $EF, $71, $4D, $C1, $E2, $5B, $05, $BC, $0B, $8C, $8F, $36, $FB,
    $68, $C8, $CD, $6E, $26, $EB, $91, $CF, $1F, $70, $41, $F6, $5E, $AC, $59, $E9,
    $14, $14, $57, $E2, $01, $0C, $62, $0B, $45, $18, $5C, $D8, $17, $0F, $6B, $B6,
    $AF, $D9, $F7, $1C, $1C, $50, $D2, $0B, $18, $18, $17, $F7, $78, $C9, $21, $93,
    $7C, $92, $A0, $E9, $08, $18, $C6, $70, $D4, $03, $2C, $0B, $36, $6B, $C4, $D9,
    $D7, $26, $E4, $05, $19, $2C, $20, $D7, $62, $97, $E1, $B2, $9B, $08, $57, $52,
    $C7, $C4, $90, $2C, $87, $47, $94, $93, $ED, $7B, $B2, $83, $18, $CB, $5E, $A9,
    $74, $3F, $98, $0F, $71, $C7, $CE, $4C, $0C, $46, $EB, $49, $12, $BA, $92, $37,
    $F1, $3C, $83, $75, $64, $E1, $1C, $29, $CA, $E6, $B2, $65, $B4, $AF, $6A, $A3,
    $0F, $D3, $57, $8B, $50, $A3, $48, $75, $93, $13, $8F, $64, $3B, $8C, $5C, $6C,
    $D0, $8D, $42, $01, $DC, $06, $62, $37, $39, $39, $E0, $E4, $A6, $95, $BC, $50,
    $9C, $6C, $33, $96, $06, $F0, $F4, $4A, $35, $EC, $EE, $74, $CB, $52, $89, $F2,
    $C7, $17, $D1, $D5, $8A, $0E, $3D, $78, $B4, $73, $6E, $EF, $A2, $54, $B3, $32,
    $EC, $67, $0A, $AB, $29, $C1, $78, $7F, $8D, $6A, $01, $0B, $D2, $89, $59, $C5,
    $03, $B3, $FF, $E0, $8D, $1C, $83, $09, $C1, $29, $D1, $78, $63, $14, $0B, $80,
    $0B, $B8, $00, $DD, $54, $43, $DD, $58, $D6, $C0, $C3, $42, $74, $3D, $24, $DA,
    $0E, $D0, $5C, $14, $37, $38, $42, $19, $52, $AD, $01, $59, $20, $8A, $DF, $1F,
    $89, $2B, $3D, $2C, $73, $7B, $12, $FF, $D8, $C3, $2C, $B1, $73, $BF, $FD, $EB,
    $D6, $29, $C2, $61, $92, $EB, $99, $29, $D0, $68, $FB, $96, $01, $AE, $30, $70,
    $59, $BF, $57, $F3, $C0, $6D, $36, $DC, $03, $F7, $A6, $45, $D1, $B4, $EB, $0C,
    $C0, $64, $7C, $5D, $BF, $09, $13, $90, $39, $EF, $42, $B4, $A0, $54, $2C, $F1,
    $3F, $A1, $BE, $45, $81, $B5, $C4, $14, $24, $DC, $C9, $5D, $06, $CA, $36, $76,
    $01, $F0, $0F, $48, $02, $70, $B1, $70, $01, $6D, $00, $71, $D8, $D6, $C0, $99,
    $D8, $35, $27, $17, $D9, $CE, $F8, $29, $6E, $28, $3A, $56, $D2, $0C, $D8, $CD,
    $20, $04, $D8, $C2, $74, $A0, $A3, $EB, $0A, $CB, $03, $F4, $9F, $E6, $CA, $A9,
    $54, $E0, $D4, $BF, $CD, $37, $61, $30, $25, $85, $19, $B7, $82, $AE, $14, $75,
    $37, $38, $BC, $CF, $93, $1E, $4F, $55, $4B, $9B, $BF, $70, $FF, $31, $A0, $BC,
    $A9, $6E, $9D, $44, $B7, $55, $18, $46, $83, $FE, $11, $8F, $91, $8C, $DD, $D0,
    $D9, $EB, $D2, $CF, $52, $C0, $23, $57, $89, $DA, $51, $40, $96, $C1, $EE, $D6,
    $6D, $2F, $D1, $EB, $28, $1C, $56, $8D, $DB, $98, $DE, $CA, $8C, $A8, $81, $D5,
    $0C, $BE, $07, $3F, $65, $05, $0F, $F4, $61, $BC, $C8, $56, $28, $8F, $24, $80,
    $4F, $58, $24, $23, $2D, $C1, $AE, $F9, $75, $14, $77, $32, $9F, $0D, $C3, $57,
    $3C, $01, $74, $41, $F6, $EE, $36, $E8, $82, $D2, $D2, $17, $68, $D0, $4C, $74,
    $FE, $22, $46, $82, $E8, $0E, $6F, $68, $38, $88, $46, $88, $78, $32, $D2, $9D,
    $DB, $21, $47, $B6, $3E, $83, $F0, $87, $9A, $9E, $00, $2D, $81, $62, $B8, $7B,
    $FB, $CF, $05, $B1, $BD, $46, $4D, $D4, $98, $DA, $BD, $BB, $9D, $CE, $25, $D6,
    $95, $36, $78, $AE, $19, $39, $02, $7E, $A7, $89, $68, $A0, $A0, $B3, $3E, $18,
    $54, $A7, $9A, $6B, $6C, $9E, $8F, $41, $F9, $03, $7D, $77, $B6, $84, $80, $10,
    $12, $A6, $58, $14, $D7, $76, $23, $7E, $5A, $BA, $1F, $85, $EB, $51, $E4, $F7,
    $EA, $A3, $05, $72, $7B, $6B, $38, $77, $C1, $F8, $1F, $0D, $08, $30, $1E, $14,
    $01, $D0, $35, $C5, $67, $34, $64, $02, $DD, $97, $FD, $56, $FD, $DA, $E3, $16,
    $99, $0C, $51, $F4, $03, $83, $97, $BC, $43, $C2, $12, $9E, $19, $98, $23, $82,
    $2F, $1C, $1D, $C9, $55, $0D, $AC, $55, $C1, $47, $99, $04, $31, $48, $23, $15,
    $EE, $7F, $47, $BF, $5B, $D5, $C6, $7F, $54, $83, $FA, $06, $7F, $31, $4D, $1C,
    $16, $01, $F0, $56, $5D, $FC, $F1, $8B, $34, $95, $70, $50, $75, $79, $65, $FA,
    $89, $F8, $2D, $42, $EC, $88, $3D, $A0, $CA, $47, $BE, $14, $25, $C0, $2D, $80,
    $C1, $FE, $8C, $D3, $ED, $E8, $4F, $D5, $A8, $F2, $04, $09, $81, $7B, $1C, $AB,
    $0D, $6A, $3B, $86, $7F, $35, $6F, $13, $F0, $2F, $30, $05, $8F, $3A, $57, $99,
    $E6, $7D, $C5, $B0, $AD, $37, $D8, $B2, $9F, $03, $1C, $94, $AC, $30, $A7, $6C,
    $9D, $C3, $1D, $D4, $F1, $20, $2E, $59, $9C, $E8, $74, $B8, $6F, $21, $BC, $42,
    $B4, $E6, $1C, $17, $6F, $82, $AE, $1D, $E0, $0F, $88, $A6, $1F, $41, $96, $83,
    $5A, $44, $10, $A0, $4D, $8B, $65, $01, $B8, $56, $D5, $09, $BE, $08, $DF, $1E,
    $8C, $12, $59, $10, $58, $40, $5A, $C1, $FB, $1F, $70, $91, $E1, $C2, $84, $6D,
    $5F, $29, $DA, $61, $9E, $7D, $60, $18, $42, $90, $99, $8F, $4E, $29, $C6, $5B,
    $9C, $C2, $CE, $50, $CE, $52, $58, $5A, $D9, $10, $A1, $00, $31, $47, $0F, $50,
    $04, $C1, $86, $E0, $46, $06, $E0, $E0, $08, $C1, $1D, $E1, $09, $CB, $5F, $A7,
    $99, $4D, $85, $9A, $8A, $6A, $80, $15, $F1, $0B, $67, $6F, $4C, $99, $7A, $AE,
    $CF, $0F, $B1, $7F, $D6, $55, $57, $A5, $8A, $76, $50, $5B, $C9, $09, $0B, $D8,
    $0A, $C1, $30, $0C, $02, $70, $A7, $76, $07, $4D, $AF, $D0, $11, $A2, $5D, $C3,
    $DC, $6D, $D0, $15, $26, $1A, $6A, $DB, $58, $1E, $38, $0F, $B0, $CA, $FB, $B4,
    $B9, $0B, $4D, $E9, $CA, $1D, $D8, $F1, $27, $16, $40, $B4, $FD, $1F, $BA, $EC,
    $DE, $EA, $C1, $DE, $FA, $74, $07, $0F, $BE, $40, $17, $A1, $CC, $F6, $44, $F3,
    $6C, $12, $0C, $47, $86, $FA, $45, $4F, $0F, $A9, $CC, $D6, $E1, $79, $B8, $59,
    $75, $15, $6C, $C9, $36, $07, $E5, $14, $C9, $10, $2A, $A0, $5F, $AF, $9D, $CD,
    $25, $BC, $30, $17, $1B, $4F, $C5, $21, $81, $33, $3E, $15, $7D, $01, $AD, $89,
    $83, $E6, $75, $76, $BC, $DB, $01, $93, $CC, $87, $62, $9E, $4C, $84, $2D, $E7,
    $AA, $D0, $D5, $74, $0C, $4B, $53, $FE, $4D, $62, $42, $AD, $21, $3D, $76, $ED,
    $82, $34, $D9, $B0, $59, $4C, $FB, $76, $46, $88, $20, $37, $20, $BF, $48, $77,
    $02, $F8, $0C, $35, $89, $1F, $0D, $DE, $3E, $EB, $B5, $4F, $7C, $90, $A2, $A1,
    $4E, $23, $EB, $DB, $8D, $4D, $E0, $8D, $2B, $D9, $AB, $1B, $00, $75, $46, $D8,
    $14, $E0, $38, $14, $82, $44, $7C, $C4, $0C, $62, $89, $5F, $00, $85, $5A, $30,
    $8B, $72, $34, $C1, $DC, $38, $41, $5F, $D4, $49, $A2, $7F, $D2, $8B, $4C, $B5,
    $D0, $BD, $43, $A8, $24, $84, $B7, $CD, $DE, $9B, $E7, $A8, $EB, $6B, $CD, $42,
    $23, $61, $A0, $78, $A5, $5C, $AB, $DE, $FF, $25, $5D, $08, $F5, $D9, $23, $33,
    $1C, $DC, $7D, $D0, $5C, $82, $D1, $8B, $ED, $DC, $CA, $D1, $DE, $2F, $88, $AF,
    $DA, $1E, $D6, $CE, $22, $DD, $19, $E2, $E7, $ED, $5A, $73, $DF, $0F, $42, $C0,
    $DD, $1B, $85, $5D, $0A, $91, $85, $0E, $78, $38, $BA, $A9, $81, $FE, $10, $3A,
    $0C, $C0, $5C, $80, $57, $38, $CB, $7C, $7A, $50, $1C, $02, $18, $50, $A3, $C0,
    $45, $04, $9A, $8A, $7A, $E4, $00, $68, $D5, $09, $7B, $3E, $23, $72, $31, $EB,
    $E3, $1C, $0A, $23, $4B, $CC, $6C, $B1, $1A, $50, $5E, $73, $92, $44, $05, $D0,
    $D1, $FB, $43, $18, $51, $45, $7D, $18, $01, $DC, $AB, $82, $4E, $64, $32, $53,
    $96, $A4, $1B, $AC, $04, $40, $43, $0D, $08, $2B, $B8, $1E, $10, $B8, $10, $B3,
    $DA, $1E, $88, $16, $74, $4A, $57, $5B, $05, $18, $78, $14, $1C, $F0, $1F, $DF,
    $7E, $11, $96, $B0, $9B, $AC, $13, $E9, $62, $56, $56, $EA, $C4, $6E, $94, $20,
    $7E, $DC, $14, $57, $0B, $38, $34, $60, $50, $DC, $D9, $5D, $7C, $0D, $7E, $F1,
    $DB, $5C, $9B, $17, $8E, $06, $56, $BD, $EA, $24, $3C, $60, $E2, $73, $85, $8F,
    $EA, $DB, $4B, $55, $98, $01, $27, $51, $28, $8B, $56, $EE, $51, $BD, $D9, $AE,
    $2A, $86, $9C, $99, $18, $01, $D8, $81, $5D, $A1, $71, $4C, $9D, $04, $53, $31,
    $9D, $36, $1A, $59, $77, $58, $B1, $5A, $B2, $D0, $0C, $46, $CD, $99, $41, $4F,
    $91, $07, $D3, $D9, $A7, $8F, $07, $75, $5C, $5E, $7D, $BE, $9D, $8C, $59, $85,
    $CC, $55, $B9, $56, $5D, $F7, $CC, $02, $32, $F6, $AE, $D8, $E8, $14, $37, $F4,
    $15, $4A, $4B, $CF, $DE, $A8, $E4, $A3, $9C, $E3, $73, $DF, $60, $DF, $DA, $0C,
    $7F, $5B, $20, $89, $6B, $F9, $7C, $43, $7E, $50, $8B, $40, $24, $50, $6C, $09,
    $50, $50, $D2, $90, $75, $DB, $B5, $50, $EC, $56, $6C, $8F, $40, $02, $F8, $DB,
    $B9, $CB, $09, $9E, $4B, $26, $08, $EB, $D7, $E0, $52, $30, $C0, $7F, $13, $56,
    $24, $16, $DA, $84, $68, $A0, $31, $02, $93, $D0, $56, $8B, $C0, $19, $3A, $6A,
    $14, $91, $FB, $9B, $66, $09, $DA, $C7, $4A, $51, $82, $9F, $9A, $13, $17, $AA,
    $63, $30, $75, $CF, $8F, $5C, $BE, $61, $BC, $CA, $D0, $DE, $4C, $47, $7C, $4A,
    $45, $D8, $4E, $AA, $0D, $F2, $50, $51, $EC, $43, $2C, $53, $82, $3E, $55, $01,
    $40, $E9, $7E, $10, $84, $80, $24, $3B, $68, $C0, $91, $89, $67, $45, $6D, $E3,
    $7B, $A2, $44, $48, $1C, $47, $8C, $00, $BC, $C4, $83, $FF, $62, $A6, $C7, $46,
    $3B, $46, $E2, $94, $C0, $84, $76, $0F, $89, $D7, $D9, $CA, $04, $74, $EE, $D9,
    $52, $A7, $2D, $B0, $B5, $E0, $02, $0A, $0F, $AC, $7F, $A8, $58, $B6, $AD, $50,
    $0B, $F6, $0C, $0B, $B4, $B0, $18, $85, $87, $6E, $06, $A0, $FD, $8B, $9F, $DC,
    $83, $CE, $6E, $08, $10, $E3, $34, $2C, $87, $B4, $7A, $A6, $72, $08, $50, $71,
    $BC, $74, $BC, $B5, $37, $19, $A3, $2E, $58, $2D, $3C, $D4, $51, $51, $8C, $0C,
    $21, $93, $C8, $B8, $D8, $DC, $03, $3B, $93, $01, $79, $32, $EC, $5B, $04, $19,
    $19, $C2, $3C, $E8, $D0, $31, $24, $B7, $56, $9E, $7C, $A1, $00, $32, $50, $4F,
    $76, $C2, $93, $26, $40, $39, $55, $AC, $B6, $D0, $0E, $19, $2C, $7E, $05, $F4,
    $03, $17, $28, $FB, $46, $C3, $DB, $B8, $1C, $A8, $7E, $06, $0D, $89, $28, $39,
    $75, $B4, $7D, $0A, $A0, $B9, $BB, $5A, $04, $0A, $39, $45, $B0, $0A, $04, $A2,
    $2F, $5E, $34, $07, $F2, $FF, $04, $92, $AB, $35, $0C, $3B, $53, $68, $69, $38,
    $07, $AD, $B9, $0D, $12, $0F, $46, $CE, $02, $05, $62, $08, $62, $FC, $58, $B0,
    $15, $4C, $34, $B0, $50, $03, $60, $CC, $B2, $A3, $65, $DB, $5A, $9F, $E7, $0A,
    $C0, $C4, $BF, $A0, $A1, $1E, $36, $B9, $2C, $0F, $24, $2A, $A0, $61, $4F, $16,
    $1A, $09, $DF, $DB, $59, $E4, $75, $0E, $7B, $18, $34, $42, $55, $EF, $5A, $FA,
    $E8, $51, $1C, $0B, $8B, $B8, $AC, $EB, $E9, $FF, $8C, $70, $40, $6C, $2A, $A2,
    $42, $04, $D1, $99, $80, $8E, $B5, $3E, $05, $42, $0C, $30, $18, $41, $DF, $B3,
    $06, $08, $5D, $89, $AC, $0F, $87, $47, $E5, $58, $90, $76, $84, $A6, $1C, $B7,
    $5B, $56, $8B, $1F, $EB, $EB, $1A, $46, $7F, $24, $69, $20, $BA, $DF, $0C, $FF,
    $0F, $DB, $0B, $AF, $A8, $52, $00, $16, $5B, $C0, $0C, $1C, $BC, $C0, $DC, $67,
    $D2, $05, $B0, $92, $CE, $4B, $06, $66, $7B, $3A, $AC, $97, $D9, $14, $B8, $71,
    $2C, $6C, $83, $03, $74, $B3, $F6, $C0, $12, $35, $B5, $2B, $12, $53, $82, $77,
    $18, $9B, $2A, $0A, $70, $8B, $0A, $39, $D7, $75, $23, $9B, $04, $86, $68, $EB,
    $ED, $5B, $14, $20, $A8, $2C, $2C, $98, $0A, $AC, $75, $A6, $D6, $98, $20, $CF,
    $FD, $1A, $11, $55, $B4, $EB, $06, $22, $1E, $80, $5D, $03, $04, $B2, $43, $2C,
    $74, $08, $20, $57, $8B, $50, $CB, $2F, $31, $CD, $38, $45, $53, $CF, $6A, $47,
    $20, $FF, $13, $9F, $CD, $B6, $7B, $5F, $E1, $89, $03, $89, $1D, $09, $FB, $A1,
    $F0, $53, $1C, $9E, $5A, $71, $23, $4A, $C6, $5B, $EB, $97, $9F, $D2, $D4, $0A,
    $1C, $C8, $E9, $55, $94, $47, $DE, $19, $2E, $E9, $68, $E4, $7B, $20, $D3, $33,
    $A2, $26, $39, $CE, $EE, $A3, $56, $08, $4D, $7F, $E1, $35, $B4, $F0, $54, $C0,
    $11, $68, $E3, $4D, $DC, $06, $86, $D8, $64, $09, $C3, $06, $B0, $5F, $A6, $4E,
    $31, $82, $67, $59, $BF, $4C, $85, $58, $41, $81, $7C, $A5, $33, $82, $C6, $AB,
    $9B, $38, $A5, $E5, $5A, $73, $77, $13, $89, $46, $04, $25, $05, $28, $AE, $D9,
    $4A, $D0, $50, $5A, $10, $F7, $08, $78, $68, $06, $67, $70, $78, $53, $84, $34,
    $32, $11, $4C, $60, $D9, $EC, $4D, $53, $12, $A9, $1F, $9C, $BC, $69, $0C, $5A,
    $56, $3F, $46, $7D, $E4, $5F, $1D, $9A, $A8, $05, $08, $F0, $AC, $C6, $6D, $10,
    $4D, $0B, $45, $5D, $EE, $75, $2F, $14, $3A, $B4, $07, $32, $ED, $A2, $EB, $07,
    $DE, $51, $9F, $1B, $3E, $19, $14, $13, $FA, $89, $3D, $75, $EA, $B8, $1A, $1E,
    $86, $FF, $FB, $07, $3D, $D0, $2A, $6C, $E0, $4E, $38, $4F, $84, $5B, $ED, $F7,
    $1C, $DE, $D5, $E0, $F1, $F5, $CF, $BD, $D7, $94, $80, $7E, $44, $01, $57, $35,
    $DA, $3A, $8E, $CA, $12, $36, $C7, $00, $5A, $50, $F3, $A4, $DC, $0D, $80, $D7,
    $51, $8F, $C8, $0D, $F0, $41, $18, $A5, $7A, $98, $39, $C0, $13, $62, $06, $F9,
    $6C, $05, $76, $93, $92, $F2, $EB, $62, $41, $5A, $4B, $45, $87, $CE, $FC, $84,
    $40, $FF, $0D, $43, $64, $61, $DF, $D6, $7E, $EC, $80, $79, $61, $09, $28, $17,
    $43, $63, $18, $C8, $95, $1C, $43, $01, $0F, $14, $22, $58, $69, $C6, $6F, $8D,
    $3F, $83, $87, $1B, $0C, $25, $CA, $A3, $9C, $50, $97, $70, $08, $A3, $FE, $1A,
    $94, $4E, $0A, $50, $04, $52, $4B, $70, $C3, $9E, $45, $23, $68, $3C, $39, $32,
    $2B, $F0, $99, $7A, $F8, $52, $39, $E1, $58, $82, $03, $F7, $DC, $B0, $65, $08,
    $42, $2F, $66, $3C, $17, $31, $C9, $97, $B0, $47, $E2, $74, $77, $8D, $6E, $47,
    $34, $E5, $30, $A0, $D9, $FD, $C5, $4D, $34, $F0, $39, $7E, $9F, $59, $85, $CB,
    $0D, $41, $1C, $A3, $91, $13, $B1, $B0, $5F, $73, $EC, $4E, $09, $B5, $90, $00,
    $30, $C0, $C7, $56, $2F, $A0, $BF, $E5, $15, $A5, $3B, $9B, $89, $CE, $0F, $B0,
    $0A, $10, $41, $91, $65, $90, $C0, $14, $D0, $30, $84, $68, $25, $2F, $6F, $75,
    $61, $A9, $74, $A8, $18, $37, $75, $09, $5D, $24, $D1, $00, $FE, $23, $40, $A5,
    $CD, $39, $FA, $7E, $53, $39, $FB, $24, $E2, $D9, $0B, $7C, $F8, $F0, $28, $F2,
    $29, $C8, $19, $E7, $50, $AE, $DB, $80, $2D, $6D, $D0, $63, $89, $02, $D8, $07,
    $2E, $C8, $95, $68, $0C, $7A, $0B, $45, $82, $1A, $05, $76, $0D, $DE, $F9, $4D,
    $3D, $2C, $F0, $10, $98, $72, $E5, $F0, $4F, $EC, $AD, $B7, $0B, $B0, $F2, $D1,
    $01, $C1, $52, $7F, $AD, $3B, $82, $B2, $17, $C1, $FE, $7D, $4C, $3B, $5D, $0C,
    $7C, $51, $54, $29, $4E, $C2, $5C, $CF, $85, $48, $4F, $BD, $4F, $7A, $FB, $C6,
    $EE, $E9, $8D, $C4, $3B, $FB, $7E, $14, $3B, $75, $04, $53, $31, $C0, $2D, $59,
    $51, $0B, $DF, $DF, $18, $F3, $B5, $37, $F7, $08, $0F, $8D, $82, $BD, $1C, $08,
    $7C, $E3, $5A, $6D, $D0, $11, $F2, $0C, $72, $08, $29, $C8, $F0, $C8, $11, $C8,
    $65, $B7, $3E, $EB, $34, $35, $10, $D1, $8F, $4C, $D8, $10, $A3, $C2, $FF, $4D,
    $E8, $74, $11, $DF, $02, $34, $2D, $56, $F1, $FD, $D0, $A6, $C3, $15, $CB, $12,
    $88, $83, $10, $FC, $A6, $7F, $6F, $7F, $E9, $A1, $15, $86, $B2, $D3, $AE, $8B,
    $34, $B8, $78, $6F, $3B, $07, $A2, $8F, $DD, $3D, $E0, $0E, $7D, $67, $2A, $A7,
    $41, $FD, $93, $A8, $B6, $06, $02, $4E, $28, $CA, $83, $C0, $03, $81, $E2, $26,
    $AA, $05, $E0, $0A, $6C, $5C, $2B, $DF, $A0, $D9, $7F, $41, $C2, $66, $C1, $E8,
    $0C, $16, $C4, $09, $D0, $31, $D8, $40, $00, $6A, $AE, $66, $4D, $CD, $68, $A4,
    $DB, $6D, $76, $8B, $16, $20, $19, $44, $56, $50, $FF, $06, $E8, $65, $D1, $1B,
    $31, $BA, $68, $D0, $92, $F7, $7B, $1B, $83, $C1, $87, $45, $41, $1D, $15, $98,
    $4A, $D8, $80, $D6, $99, $7C, $01, $2B, $3D, $52, $97, $6B, $93, $0A, $B6, $DB,
    $F0, $70, $A0, $99, $DB, $B5, $B3, $D4, $A7, $80, $87, $C7, $3B, $A1, $82, $2D,
    $BC, $40, $BC, $F0, $24, $DB, $8F, $57, $41, $75, $53, $04, $BA, $07, $40, $00,
    $DF, $5D, $8B, $02, $A3, $05, $04, $34, $12, $32, $65, $60, $30, $C3, $95, $AF,
    $56, $B0, $76, $8B, $8F, $95, $62, $1B, $AF, $81, $E1, $B3, $4C, $81, $8B, $1C,
    $8A, $09, $47, $FC, $0B, $22, $82, $31, $D2, $39, $F3, $74, $2C, $9B, $85, $A2,
    $5D, $0F, $60, $25, $FA, $98, $1C, $66, $05, $60, $E1, $86, $54, $0E, $66, $39,
    $76, $C2, $A1, $56, $82, $63, $B3, $C8, $EC, $35, $29, $C2, $12, $EB, $75, $D9,
    $0A, $31, $66, $F0, $03, $4E, $39, $CA, $76, $89, $B3, $77, $98, $C1, $8D, $43,
    $06, $5F, $8D, $EF, $01, $C9, $AB, $6F, $F5, $06, $FF, $FC, $39, $C9, $F3, $A6,
    $0F, $92, $D3, $97, $C0, $28, $6E, $BE, $D0, $EB, $94, $62, $00, $A1, $C7, $FF,
    $5C, $54, $9D, $38, $D9, $4E, $49, $06, $D8, $E4, $49, $59, $08, $77, $B2, $06,
    $E0, $C4, $2A, $3B, $88, $F8, $0C, $38, $5B, $0C, $C8, $7F, $39, $4D, $C4, $28,
    $49, $95, $DB, $9F, $A1, $AD, $8F, $1A, $8A, $47, $F1, $86, $02, $FB, $C2, $8B,
    $57, $B0, $65, $0E, $69, $5C, $59, $99, $64, $69, $26, $42, $B8, $B0, $A0, $91,
    $11, $BA, $64, $B8, $A8, $6C, $F1, $A8, $67, $92, $97, $B8, $BC, $57, $57, $B0,
    $D6, $81, $01, $35, $24, $53, $B3, $B2, $3D, $64, $A1, $2F, $A0, $7D, $D0, $3F,
    $D1, $D9, $80, $2C, $34, $B0, $E9, $B4, $A8, $3F, $64, $CD, $21, $13, $B8, $75,
    $81, $7F, $64, $93, $50, $A9, $40, $AF, $A0, $7F, $92, $34, $87, $4C, $B0, $5D,
    $21, $6A, $03, $95, $84, $AF, $F3, $7D, $D8, $95, $F8, $02, $09, $71, $55, $D0,
    $7E, $C3, $E5, $6F, $AE, $B0, $1D, $39, $C3, $07, $04, $39, $5D, $E0, $7D, $03,
    $89, $F8, $26, $DC, $42, $B7, $E4, $07, $73, $F8, $01, $D8, $0F, $88, $D5, $22,
    $B8, $A3, $18, $C1, $F8, $05, $C4, $D6, $0A, $10, $1B, $40, $F0, $A9, $08, $14,
    $15, $AA, $D0, $E4, $F7, $00, $68, $F9, $4A, $70, $82, $A1, $04, $80, $AA, $40,
    $C4, $DB, $5D, $C0, $17, $A0, $CC, $0F, $8C, $66, $FD, $C9, $7E, $24, $21, $C8,
    $63, $F7, $19, $04, $FE, $C1, $E0, $04, $2E, $A3, $90, $DA, $3B, $82, $3B, $D9,
    $11, $A3, $80, $0E, $C1, $E4, $50, $03, $60, $A9, $95, $66, $E0, $DC, $91, $49,
    $F0, $DB, $DE, $8A, $41, $EE, $4C, $A9, $83, $E2, $F8, $7D, $DC, $1E, $C0, $09,
    $34, $B0, $83, $E1, $41, $89, $BE, $0D, $5A, $F7, $91, $ED, $B4, $88, $9B, $07,
    $8D, $50, $01, $42, $29, $6B, $08, $7C, $77, $04, $C0, $78, $6B, $12, $40, $0F,
    $AF, $D0, $A3, $C3, $1B, $25, $89, $D7, $15, $07, $8F, $1F, $A0, $D9, $06, $D9,
    $2C, $A4, $AA, $51, $A8, $0F, $AC, $61, $93, $17, $B2, $A9, $91, $AA, $11, $2F,
    $60, $DF, $56, $E4, $A9, $D1, $38, $1F, $1C, $B5, $07, $9D, $5B, $61, $CB, $A0,
    $7F, $6D, $1C, $24, $AB, $2D, $80, $01, $8C, $3E, $FF, $7C, $5F, $BC, $93, $08,
    $CC, $06, $42, $71, $35, $38, $78, $85, $4B, $1B, $FC, $54, $94, $7D, $6F, $4D,
    $C7, $B7, $A8, $6D, $30, $83, $C8, $5B, $C2, $17, $8F, $72, $01, $FA, $DF, $D9,
    $05, $00, $0C, $A3, $29, $50, $52, $51, $68, $14, $93, $9E, $EB, $10, $A4, $19,
    $DC, $20, $03, $BF, $C3, $59, $E1, $B0, $06, $A1, $6B, $30, $45, $A3, $35, $B3,
    $53, $D5, $EC, $A1, $80, $19, $09, $70, $67, $82, $60, $C2, $A3, $B0, $81, $71,
    $00, $01, $AF, $31, $02, $D7, $A3, $38, $91, $87, $20, $59, $28, $38, $D0, $25,
    $23, $3D, $40, $77, $2D, $74, $D0, $3A, $62, $F5, $C8, $C6, $84, $11, $03, $30,
    $05, $D0, $7B, $9D, $00, $99, $10, $7D, $D3, $D1, $6A, $39, $58, $A9, $D6, $F7,
    $7B, $5F, $00, $06, $E4, $77, $82, $99, $13, $C0, $73, $10, $06, $C6, $FB, $AF,
    $25, $B6, $1F, $39, $D3, $7D, $0E, $A1, $19, $B9, $F8, $D8, $46, $F1, $1C, $58,
    $43, $8B, $F7, $36, $68, $50, $B8, $40, $82, $7A, $B1, $8D, $BF, $50, $1A, $55,
    $28, $05, $BC, $67, $D0, $19, $3C, $53, $6D, $C4, $1E, $8A, $7B, $A0, $4D, $9E,
    $A4, $50, $1A, $CE, $B7, $02, $9C, $50, $35, $5D, $5C, $0B, $6F, $84, $23, $60,
    $4D, $88, $DF, $3D, $43, $E2, $88, $FE, $E4, $06, $4B, $09, $94, $1F, $04, $5F,
    $45, $98, $8B, $3C, $82, $6F, $73, $D5, $86, $F7, $85, $30, $A9, $19, $A1, $B1,
    $15, $D8, $C1, $1B, $85, $98, $8D, $56, $AF, $14, $48, $7C, $AD, $5A, $ED, $A2,
    $28, $6E, $7D, $A0, $40, $0F, $B7, $D6, $68, $4B, $32, $84, $3B, $24, $05, $5B,
    $7C, $B1, $3D, $14, $0A, $EF, $81, $7D, $A4, $40, $AD, $2A, $0B, $9C, $29, $88,
    $4B, $B4, $58, $A0, $67, $14, $92, $B3, $B4, $AE, $68, $01, $2C, $2E, $9E, $20,
    $C6, $09, $47, $2E, $7D, $20, $06, $68, $44, $C9, $5B, $E6, $81, $8F, $A0, $C7,
    $1D, $15, $23, $8C, $0C, $03, $06, $6E, $E5, $00, $6C, $45, $DE, $BB, $2F, $B0,
    $33, $9B, $81, $AA, $92, $01, $33, $66, $A1, $B6, $09, $2B, $E8, $D2, $03, $E8,
    $14, $B0, $B6, $90, $55, $74, $A9, $16, $C0, $8C, $64, $19, $40, $EC, $D0, $A3,
    $AA, $DB, $5C, $0B, $D6, $8C, $1C, $EE, $39, $71, $10, $02, $1E, $57, $31, $FF,
    $19, $3B, $1D, $F0, $85, $E2, $23, $50, $6A, $8D, $5D, $E6, $4F, $55, $8C, $47,
    $51, $30, $32, $59, $AB, $81, $A7, $1D, $3D, $4C, $24, $3F, $00, $88, $29, $11,
    $04, $78, $2E, $51, $E0, $6E, $57, $E6, $75, $C4, $1D, $2A, $50, $93, $81, $0E,
    $61, $4B, $4B, $7C, $B4, $9F, $08, $F6, $AB, $6A, $E0, $28, $8C, $50, $86, $44,
    $5A, $78, $4D, $C4, $D5, $45, $79, $8F, $33, $28, $E0, $66, $1D, $6C, $F9, $AB,
    $D0, $C1, $89, $DF, $81, $FA, $05, $74, $56, $45, $E9, $90, $B1, $C8, $9D, $D9,
    $32, $84, $41, $4C, $DE, $6A, $02, $4A, $30, $48, $38, $07, $E8, $EC, $91, $50,
    $88, $3E, $2C, $73, $C3, $1A, $04, $75, $8C, $DB, $D9, $B0, $11, $33, $56, $94,
    $7A, $47, $78, $7C, $F0, $D5, $C3, $84, $8C, $A4, $8B, $75, $B8, $1D, $C1, $A4,
    $43, $E3, $C5, $54, $4B, $7C, $E8, $D2, $35, $6A, $B2, $B0, $E0, $E9, $B2, $3B,
    $D2, $40, $47, $E1, $C4, $3D, $DA, $81, $DA, $5A, $2C, $CB, $32, $D8, $D8, $CC,
    $DA, $76, $03, $33, $E6, $82, $37, $0E, $C8, $99, $7C, $0F, $3E, $9F, $19, $C4,
    $55, $CC, $B8, $0E, $C0, $73, $6C, $51, $E2, $0F, $A7, $7E, $3E, $65, $91, $5B,
    $AD, $43, $F4, $E8, $F9, $CB, $02, $D4, $BC, $6D, $0B, $AC, $C8, $51, $BC, $1F,
    $C0, $39, $F8, $F8, $54, $98, $A5, $7B, $CC, $1F, $D0, $A3, $7C, $66, $73, $BF,
    $3F, $7D, $CC, $C1, $FB, $07, $3E, $1E, $7D, $38, $82, $5F, $F7, $34, $83, $4D,
    $B8, $C8, $19, $C8, $32, $80, $AD, $6A, $C1, $61, $52, $BC, $BA, $D8, $67, $35,
    $E8, $C6, $83, $F0, $E2, $76, $40, $3C, $8E, $21, $1B, $B8, $52, $10, $1F, $12,
    $29, $B0, $BD, $C0, $CD, $41, $4F, $24, $21, $C3, $EA, $B8, $7C, $06, $7B, $1B,
    $63, $13, $91, $7D, $B8, $49, $39, $36, $7C, $04, $48, $3C, $40, $BB, $F5, $36,
    $0B, $B8, $0F, $9C, $B7, $5D, $B4, $05, $D7, $22, $9E, $14, $9A, $E2, $FB, $22,
    $0F, $36, $B8, $05, $6E, $84, $3F, $67, $1F, $89, $DF, $69, $BC, $00, $EE, $B6,
    $D9, $0D, $67, $3B, $7D, $16, $8F, $C2, $1F, $BA, $C1, $37, $6A, $7B, $77, $B8,
    $7F, $6B, $89, $1D, $E1, $07, $57, $28, $78, $5F, $A1, $A8, $EB, $06, $46, $0F,
    $13, $81, $1B, $B4, $6F, $B5, $C0, $FD, $C3, $FF, $75, $C0, $02, $77, $F0, $91,
    $21, $B6, $09, $0A, $0A, $C8, $73, $2B, $11, $C4, $1E, $3E, $FB, $AC, $36, $44,
    $85, $FF, $5B, $15, $CC, $01, $D1, $8D, $58, $7F, $8D, $51, $7F, $52, $8C, $26,
    $22, $50, $7C, $72, $AC, $E4, $1D, $F4, $DD, $6E, $6B, $02, $30, $46, $12, $A7,
    $3C, $5A, $7E, $A5, $47, $EB, $81, $DB, $50, $BC, $2D, $A3, $07, $D2, $03, $29,
    $C1, $6A, $78, $1F, $0A, $00, $DA, $41, $E0, $C2, $75, $7E, $22, $08, $C0, $7E,
    $B2, $2D, $7F, $DE, $1E, $79, $E1, $8D, $42, $07, $21, $1F, $0E, $13, $F8, $7F,
    $DE, $89, $A6, $23, $F1, $AC, $2A, $96, $AD, $18, $0B, $E9, $07, $56, $85, $02,
    $90, $ED, $AD, $D6, $22, $FF, $05, $1B, $F0, $7D, $1A, $7A, $3F, $62, $6B, $81,
    $94, $66, $5A, $9E, $02, $D0, $A8, $4C, $0C, $AB, $DC, $B6, $5A, $8F, $59, $FF,
    $A5, $00, $5E, $DE, $6E, $6C, $2D, $7A, $A4, $47, $A4, $23, $3C, $48, $A3, $90,
    $01, $C3, $26, $4A, $74, $D0, $11, $7B, $9C, $77, $64, $76, $65, $EB, $6A, $15,
    $60, $89, $0F, $89, $63, $AD, $C7, $15, $18, $04, $BE, $34, $A8, $FE, $4C, $01,
    $C4, $8C, $AD, $25, $17, $DF, $11, $F9, $30, $EA, $FA, $54, $D4, $36, $43, $94,
    $EF, $53, $C9, $DE, $58, $C4, $06, $58, $6D, $0B, $13, $46, $54, $92, $10, $E8,
    $43, $20, $D9, $B3, $64, $14, $5B, $EB, $A5, $A4, $03, $DB, $01, $57, $51, $1F,
    $06, $0C, $94, $09, $AE, $E0, $5E, $45, $59, $4B, $2B, $BC, $AE, $6B, $B7, $DF,
    $12, $60, $3D, $6A, $01, $D6, $C1, $98, $89, $10, $3F, $7B, $B0, $6D, $24, $23,
    $88, $2A, $87, $44, $BF, $C8, $60, $4F, $E2, $C4, $19, $B0, $71, $07, $58, $7C,
    $50, $0D, $32, $38, $8B, $01, $40, $18, $EC, $24, $92, $2D, $01, $3E, $FE, $E0,
    $2A, $6E, $CC, $27, $33, $8E, $F9, $65, $AE, $CD, $78, $C1, $D3, $8B, $C1, $EB,
    $0E, $A7, $0F, $0C, $0F, $97, $D0, $70, $12, $22, $E5, $E5, $98, $CC, $C1, $62,
    $3B, $1A, $71, $7F, $7E, $0B, $0B, $2B, $6C, $27, $87, $B9, $F8, $49, $2E, $47,
    $7D, $22, $E2, $C5, $90, $66, $8B, $73, $20, $00, $D0, $2E, $60, $75, $43, $D4,
    $B6, $14, $AF, $65, $04, $A3, $02, $06, $3F, $79, $05, $F0, $18, $00, $6E, $C3,
    $7D, $1F, $EC, $F6, $CD, $60, $5A, $FC, $58, $34, $89, $40, $3C, $02, $38, $43,
    $1E, $28, $18, $14, $1C, $7C, $E1, $35, $1D, $05, $9B, $5B, $2E, $49, $D8, $85,
    $C0, $EE, $07, $43, $E1, $8A, $4A, $87, $3B, $70, $A8, $00, $70, $83, $2C, $03,
    $52, $D4, $F0, $D9, $64, $A9, $20, $72, $3B, $C7, $7D, $76, $65, $45, $6F, $01,
    $90, $FE, $7D, $6B, $FF, $1A, $56, $6A, $C1, $FC, $D6, $EF, $6D, $4A, $B1, $DF,
    $20, $E9, $39, $43, $34, $74, $40, $2F, $4C, $7C, $82, $DE, $D1, $5B, $AC, $C6,
    $01, $F1, $BB, $E7, $14, $38, $55, $5C, $EA, $68, $BB, $C2, $83, $61, $D3, $E3,
    $2D, $54, $AA, $BE, $7F, $5D, $EC, $08, $0C, $02, $66, $07, $6F, $85, $B6, $8B,
    $1E, $19, $88, $D1, $18, $DB, $16, $1C, $08, $46, $23, $DC, $C6, $6E, $65, $7C,
    $9D, $44, $47, $75, $7C, $8A, $95, $94, $41, $2E, $49, $44, $BC, $24, $BA, $38,
    $70, $6A, $10, $61, $24, $85, $07, $CC, $70, $F5, $F1, $1C, $92, $5A, $D5, $41,
    $16, $88, $87, $15, $68, $48, $10, $54, $59, $01, $78, $81, $2D, $29, $14, $9C,
    $53, $8B, $32, $B7, $E2, $40, $82, $60, $F6, $B2, $3F, $FA, $ED, $0B, $34, $0F,
    $2C, $46, $34, $3B, $41, $B7, $33, $7E, $09, $89, $F2, $7F, $6B, $BB, $8D, $E8,
    $D1, $0F, $8B, $79, $AE, $12, $89, $FA, $39, $CF, $EB, $0B, $6F, $5D, $DD, $76,
    $0E, $89, $42, $11, $52, $38, $78, $F3, $1B, $38, $89, $7E, $4F, $78, $61, $33,
    $02, $48, $57, $77, $3C, $43, $4C, $7D, $1C, $40, $F4, $62, $08, $75, $99, $DE,
    $C1, $16, $A2, $0A, $58, $84, $1B, $7C, $E4, $CA, $58, $72, $F4, $12, $69, $89,
    $96, $A6, $8F, $0E, $51, $0A, $12, $6C, $C9, $B4, $13, $DF, $35, $53, $0B, $8E,
    $54, $31, $74, $3D, $A4, $42, $6F, $75, $AA, $E0, $31, $C9, $F7, $80, $43, $08,
    $01, $36, $4B, $00, $3C, $F1, $65, $8C, $14, $88, $6C, $DF, $55, $28, $A9, $53,
    $04, $39, $18, $74, $18, $0A, $10, $20, $01, $B7, $83, $0D, $41, $22, $7C, $DD,
    $6F, $32, $A0, $9E, $FD, $C6, $42, $15, $01, $41, $40, $EB, $E7, $AF, $70, $9B,
    $54, $E1, $5C, $DE, $33, $D7, $7D, $28, $F6, $8C, $62, $55, $9E, $6F, $04, $BB,
    $77, $3E, $22, $0A, $C5, $20, $FF, $D4, $05, $DA, $ED, $F6, $10, $1D, $05, $47,
    $27, $7C, $E3, $03, $74, $11, $37, $BE, $25, $5E, $01, $2B, $88, $25, $70, $D3,
    $0E, $0C, $46, $42, $8A, $DF, $0F, $23, $BF, $12, $ED, $4E, $71, $C0, $2D, $55,
    $45, $4D, $86, $59, $5A, $6B, $38, $B7, $03, $32, $47, $A1, $C9, $D9, $45, $05,
    $EB, $0E, $D8, $2B, $8F, $23, $76, $39, $0F, $47, $56, $5D, $29, $92, $3D, $7D,
    $A9, $48, $7E, $4B, $DB, $20, $03, $6C, $55, $95, $04, $39, $47, $7E, $4B, $61,
    $A0, $EB, $C7, $38, $B9, $0B, $77, $CA, $50, $03, $43, $A3, $D9, $18, $F9, $E5,
    $B1, $11, $83, $76, $7F, $D8, $E1, $1C, $A2, $66, $01, $86, $41, $EF, $B1, $E3,
    $9E, $C4, $33, $22, $CA, $36, $25, $D0, $B0, $63, $AD, $E2, $6C, $3A, $84, $2F,
    $25, $B4, $44, $5B, $C4, $33, $44, $AB, $DC, $CC, $8C, $BD, $CA, $26, $4B, $19,
    $B0, $08, $D2, $8C, $D5, $66, $AC, $7F, $5C, $36, $D4, $D0, $05, $A3, $03, $60,
    $3B, $30, $7E, $6F, $D9, $22, $17, $2B, $DF, $B6, $8B, $10, $80, $2F, $68, $89,
    $B5, $B2, $43, $35, $AB, $20, $DC, $88, $9E, $31, $CC, $AD, $CC, $46, $7E, $15,
    $C0, $6B, $C0, $CE, $4E, $86, $13, $D4, $84, $80, $FD, $DF, $04, $03, $06, $99,
    $20, $BE, $DE, $50, $5C, $D9, $90, $7D, $12, $00, $11, $04, $24, $04, $8A, $68,
    $B6, $90, $03, $AC, $B0, $B4, $0C, $2E, $71, $17, $35, $50, $51, $92, $70, $9C,
    $A2, $51, $38, $8E, $3E, $76, $BA, $17, $A9, $38, $AA, $15, $55, $BC, $2A, $2A,
    $C0, $6E, $CA, $B7, $6E, $64, $E1, $3A, $93, $D9, $5E, $8F, $FC, $B5, $FB, $09,
    $39, $1A, $83, $98, $38, $B3, $3F, $F2, $D5, $46, $03, $1C, $ED, $32, $99, $B3,
    $7F, $2E, $80, $49, $20, $24, $9C, $C0, $D8, $E3, $06, $A2, $89, $70, $90, $21,
    $CE, $3E, $51, $A6, $7A, $C0, $DD, $DE, $F0, $64, $B4, $0E, $02, $92, $99, $A9,
    $C5, $06, $FB, $69, $A2, $4C, $BB, $65, $9D, $BC, $D9, $EE, $49, $A9, $66, $AE,
    $C5, $25, $44, $4A, $40, $06, $D1, $6B, $24, $9F, $09, $48, $45, $2F, $21, $78,
    $F2, $01, $26, $39, $C2, $A2, $3B, $B5, $19, $48, $3F, $21, $4D, $B4, $30, $CC,
    $7C, $87, $DD, $09, $AA, $D4, $1F, $06, $39, $FE, $9F, $D9, $C4, $D8, $C6, $0E,
    $C1, $75, $72, $D9, $3A, $B3, $90, $EC, $C3, $D8, $C7, $65, $07, $5B, $24, $E6,
    $DC, $93, $14, $60, $DF, $61, $D9, $CB, $11, $CA, $8C, $3D, $00, $7E, $24, $38,
    $E7, $5E, $22, $BA, $EF, $E4, $0E, $E2, $DE, $CB, $B0, $EA, $B8, $51, $14, $08,
    $52, $1C, $65, $D8, $74, $5D, $42, $02, $91, $F7, $10, $2D, $46, $B5, $9D, $A3,
    $45, $01, $5B, $85, $B8, $87, $F4, $D4, $06, $E2, $91, $60, $74, $9F, $E5, $EB,
    $AC, $EE, $80, $12, $7C, $A7, $D4, $5D, $81, $55, $D8, $56, $6F, $EC, $C8, $87,
    $D9, $58, $94, $B7, $4D, $DE, $10, $41, $BE, $59, $41, $A4, $DE, $57, $18, $D8,
    $73, $84, $98, $10, $80, $4F, $56, $8F, $8A, $7A, $22, $79, $94, $F0, $41, $C1,
    $74, $2F, $D9, $18, $E9, $68, $4B, $F6, $89, $F6, $F4, $44, $F4, $92, $35, $E2,
    $65, $F0, $F6, $DD, $06, $07, $0D, $1B, $E2, $56, $D5, $1F, $B8, $74, $0A, $92,
    $59, $5B, $68, $D9, $48, $00, $B2, $08, $2A, $84, $A0, $43, $D6, $56, $2B, $EB,
    $D6, $FF, $60, $3B, $53, $2C, $D0, $7D, $89, $C1, $8B, $7A, $05, $50, $60, $14,
    $3D, $1A, $71, $EB, $9D, $46, $93, $DD, $04, $90, $85, $02, $74, $81, $37, $5C,
    $39, $D1, $74, $AE, $7B, $30, $46, $B5, $40, $4D, $C5, $32, $1D, $70, $7D, $75,
    $00, $2E, $F0, $B6, $B2, $75, $09, $13, $8B, $0E, $88, $74, $21, $0D, $9C, $62,
    $84, $A8, $0F, $98, $E2, $06, $12, $6C, $B4, $44, $1B, $11, $4F, $5F, $6F, $A0,
    $45, $DD, $20, $6A, $EB, $F3, $C1, $9F, $AE, $94, $03, $A0, $85, $B3, $06, $EB,
    $E2, $85, $6E, $AD, $7D, $3E, $53, $A6, $A9, $12, $75, $ED, $22, $CA, $37, $62,
    $57, $74, $F0, $FC, $E5, $C7, $0C, $B9, $08, $B0, $F1, $86, $23, $3E, $2B, $97,
    $C2, $28, $48, $BE, $F9, $CC, $D6, $41, $43, $B3, $21, $14, $20, $14, $64, $90,
    $65, $02, $92, $1C, $1C, $C2, $08, $36, $AC, $91, $BA, $D5, $7F, $A6, $4A, $57,
    $D4, $2F, $90, $FB, $C0, $0A, $20, $12, $5F, $C2, $BD, $70, $E0, $D8, $19, $EB,
    $09, $8F, $84, $34, $77, $46, $F8, $2B, $2D, $55, $66, $F7, $6A, $14, $68, $E0,
    $C9, $A8, $62, $C0, $0E, $79, $53, $57, $72, $1F, $10, $5D, $11, $41, $31, $48,
    $65, $00, $40, $98, $78, $C2, $7E, $58, $E5, $89, $FB, $B7, $0F, $FD, $2A, $8E,
    $8F, $43, $02, $7B, $B9, $DC, $EE, $75, $F0, $04, $83, $AB, $2C, $76, $9C, $F6,
    $0C, $82, $C1, $50, $70, $40, $66, $76, $55, $74, $98, $72, $0B, $07, $14, $90,
    $25, $18, $CF, $91, $0D, $6A, $13, $1B, $E8, $18, $43, $46, $48, $B3, $21, $71,
    $E0, $46, $3C, $7F, $B8, $8E, $4F, $A6, $90, $01, $2C, $CF, $80, $80, $07, $F2,
    $39, $40, $80, $70, $CA, $80, $39, $81, $0C, $20, $84, $BA, $6C, $42, $0E, $39,
    $02, $2C, $2C, $0A, $96, $11, $0D, $6F, $60, $2B, $16, $CC, $13, $4F, $F7, $F8,
    $92, $73, $C1, $20, $2D, $77, $A9, $39, $04, $C1, $6E, $C5, $90, $8A, $EB, $1B,
    $3F, $B5, $B8, $B5, $6A, $E0, $81, $03, $8B, $49, $D2, $AA, $A1, $9E, $AE, $15,
    $0C, $40, $51, $5E, $E5, $65, $E9, $B6, $AD, $64, $EB, $0B, $42, $1F, $42, $52,
    $6C, $C3, $0A, $6A, $0E, $5D, $1F, $C0, $2C, $AA, $05, $DA, $5E, $0C, $0A, $2B,
    $FD, $70, $AD, $3C, $6C, $24, $4F, $43, $28, $03, $14, $87, $0A, $7E, $A1, $77,
    $46, $0C, $FF, $40, $28, $01, $E1, $A4, $D9, $BE, $40, $D5, $E3, $31, $10, $29,
    $22, $1F, $FE, $85, $0C, $D9, $2F, $10, $80, $7E, $16, $00, $75, $47, $A4, $6A,
    $79, $81, $F0, $89, $7E, $40, $47, $B5, $9B, $22, $88, $C4, $23, $54, $8C, $24,
    $41, $EB, $86, $5C, $99, $DE, $18, $29, $F8, $32, $8A, $D8, $21, $B4, $CE, $8C,
    $4D, $89, $FA, $5C, $10, $DC, $A1, $8A, $98, $30, $DC, $02, $3A, $8B, $19, $26,
    $EC, $D3, $56, $C9, $AE, $8A, $43, $1E, $A7, $9B, $AC, $94, $96, $10, $14, $6C,
    $33, $4F, $06, $42, $00, $13, $55, $2B, $10, $4D, $5F, $3C, $CE, $1D, $2B, $30,
    $BF, $47, $D4, $A8, $19, $5F, $3C, $8C, $5E, $EC, $17, $05, $6A, $59, $46, $53,
    $62, $27, $8B, $EA, $24, $00, $96, $29, $2F, $EB, $7E, $04, $05, $28, $78, $72,
    $EC, $CD, $12, $5D, $08, $28, $10, $5F, $75, $1C, $B8, $AE, $D0, $75, $8C, $2A,
    $47, $6F, $D7, $D6, $43, $12, $F1, $95, $72, $55, $EF, $29, $FB, $29, $06, $DA,
    $52, $04, $88, $61, $77, $07, $56, $68, $C8, $E7, $2B, $5A, $B6, $2B, $62, $10,
    $51, $13, $95, $13, $86, $C3, $2B, $94, $A5, $00, $47, $5F, $01, $6A, $D8, $A1,
    $82, $25, $7E, $D2, $9D, $4B, $B2, $89, $42, $31, $95, $88, $BE, $BE, $36, $2C,
    $C8, $87, $CB, $02, $4B, $95, $B1, $39, $46, $C6, $8B, $12, $3F, $6F, $59, $28,
    $42, $10, $16, $0F, $61, $80, $3A, $1C, $C6, $EE, $39, $4F, $C0, $43, $0F, $03,
    $CF, $2B, $59, $68, $3B, $57, $4A, $89, $03, $1C, $2C, $69, $0E, $19, $90, $80,
    $84, $28, $67, $06, $E4, $39, $B9, $C0, $A1, $84, $24, $80, $3D, $C8, $95, $1C,
    $42, $66, $95, $81, $1C, $C2, $C3, $1B, $A4, $16, $1D, $11, $51, $E4, $C7, $13,
    $D2, $9F, $28, $EB, $84, $11, $BA, $95, $67, $B0, $91, $AD, $FF, $F8, $78, $5F,
    $77, $08, $E2, $18, $EB, $43, $CF, $AF, $3B, $AE, $78, $61, $0E, $E9, $26, $D7,
    $7C, $0C, $6F, $C8, $80, $F0, $DA, $D8, $00, $78, $D7, $48, $77, $19, $DD, $7C,
    $3B, $9A, $1F, $7F, $52, $9C, $D5, $29, $80, $3C, $4F, $0C, $EB, $BF, $EF, $06,
    $F8, $0D, $EC, $72, $53, $53, $09, $96, $ED, $5D, $EB, $AB, $9E, $B8, $14, $32,
    $CF, $2D, $31, $DB, $62, $08, $0D, $62, $3D, $02, $FA, $1D, $B5, $28, $0A, $55,
    $5B, $19, $88, $D2, $9A, $27, $3E, $94, $8B, $B6, $5E, $04, $59, $F2, $F2, $26,
    $26, $A3, $46, $F0, $70, $4E, $2E, $6F, $10, $30, $EA, $EC, $DD, $01, $81, $95,
    $7D, $57, $89, $07, $C8, $64, $EF, $DE, $8B, $5F, $34, $27, $4F, $7C, $96, $AC,
    $44, $AB, $7E, $CD, $9B, $13, $1E, $52, $B1, $93, $B9, $23, $0D, $89, $D3, $23,
    $B4, $BB, $13, $06, $88, $41, $A8, $D3, $74, $51, $58, $66, $E2, $56, $D8, $EB,
    $A5, $A3, $33, $C4, $CB, $5E, $01, $D5, $08, $56, $EB, $CF, $1A, $B2, $B7, $EC,
    $CF, $1F, $78, $9E, $8B, $A3, $EC, $C1, $AA, $EF, $D9, $EB, $80, $2C, $68, $47,
    $33, $5B, $57, $30, $76, $C7, $C1, $98, $2A, $56, $08, $2B, $CF, $84, $68, $56,
    $31, $2C, $DA, $EB, $C3, $21, $14, $8E, $64, $4F, $E0, $7F, $FA, $57, $C5, $20,
    $01, $D2, $C2, $B8, $EB, $15, $C4, $2F, $00, $9B, $5F, $55, $E8, $84, $89, $14,
    $91, $42, $3B, $C5, $7A, $8B, $50, $56, $0F, $7C, $EB, $8B, $AF, $0C, $04, $BC,
    $04, $4A, $68, $40, $D1, $72, $01, $22, $10, $D0, $61, $72, $85, $D4, $D9, $57,
    $D0, $99, $48, $42, $FB, $C9, $0B, $94, $4A, $38, $41, $D4, $8E, $5E, $01, $50,
    $4F, $C5, $BA, $71, $C8, $52, $A3, $D0, $82, $0A, $B4, $28, $35, $60, $F1, $0A,
    $DF, $40, $70, $BC, $64, $E8, $8D, $1C, $98, $8D, $D2, $C1, $06, $51, $22, $0B,
    $91, $57, $77, $12, $D5, $F9, $86, $C1, $3C, $FC, $45, $16, $D8, $A0, $6D, $34,
    $3D, $52, $07, $F2, $8B, $93, $90, $67, $61, $DB, $9F, $83, $28, $D4, $FE, $9A,
    $59, $DC, $C3, $4A, $AE, $49, $32, $06, $74, $18, $BB, $7A, $D8, $3C, $DA, $04,
    $DE, $68, $8B, $58, $0B, $07, $35, $81, $4C, $48, $08, $DD, $CC, $52, $18, $86,
    $93, $11, $5C, $3B, $8A, $4E, $14, $F1, $37, $75, $27, $DA, $12, $BF, $09, $3F,
    $B7, $04, $91, $2E, $83, $BB, $B8, $45, $E2, $D6, $B9, $04, $42, $3C, $33, $FF,
    $41, $3C, $DB, $E2, $A1, $68, $76, $40, $BC, $DC, $DA, $04, $09, $B5, $6C, $E0,
    $AA, $DC, $07, $2A, $DC, $FA, $C0, $B0, $A1, $38, $A0, $33, $D0, $48, $73, $66,
    $8D, $5D, $A2, $8F, $8F, $C4, $11, $E0, $3B, $00, $02, $B2, $E4, $08, $39, $24,
    $95, $B1, $90, $F8, $B2, $E4, $73, $38, $57, $19, $0F, $39, $8D, $00, $C3, $A1,
    $65, $06, $DF, $20, $F9, $33, $2A, $D0, $DD, $46, $22, $F6, $A4, $62, $18, $00,
    $BF, $8F, $00, $07, $88, $46, $85, $04, $FB, $90, $F8, $6E, $3E, $C7, $01, $0F,
    $74, $CF, $83, $78, $10, $0C, $07, $EA, $5B, $70, $06, $83, $35, $75, $0A, $FF,
    $02, $86, $08, $6C, $15, $B6, $EC, $B8, $FF, $54, $04, $F4, $BF, $19, $6E, $56,
    $1D, $5C, $33, $41, $7D, $38, $72, $2D, $88, $38, $FD, $74, $AA, $59, $40, $4B,
    $FD, $08, $96, $1C, $60, $A3, $D8, $0D, $02, $A1, $C1, $11, $51, $74, $B5, $51,
    $96, $CD, $22, $E4, $8A, $EE, $0B, $D4, $AE, $E8, $86, $DC, $C1, $DE, $E2, $0B,
    $55, $47, $51, $28, $16, $2B, $DE, $14, $64, $6C, $AF, $5A, $A9, $F1, $06, $88,
    $4B, $94, $FD, $50, $C0, $CA, $E1, $23, $D4, $39, $FE, $18, $8C, $5B, $B5, $20,
    $C8, $57, $7D, $70, $B9, $A4, $F8, $37, $9E, $2A, $0F, $A4, $78, $B2, $39, $CB,
    $74, $56, $80, $7B, $83, $A0, $04, $99, $EC, $1B, $6F, $1A, $D0, $80, $A3, $49,
    $41, $FD, $78, $3D, $73, $CE, $6C, $A6, $09, $4E, $98, $85, $76, $8F, $56, $B4,
    $B5, $AD, $E1, $39, $9C, $98, $9C, $DE, $C3, $90, $CF, $02, $0C, $E5, $A1, $DD,
    $7E, $01, $05, $68, $88, $17, $EF, $91, $9B, $A5, $A8, $5E, $C1, $D2, $7D, $C4,
    $5E, $B4, $46, $55, $23, $38, $90, $A3, $0E, $EC, $30, $2C, $51, $6C, $00, $39,
    $3F, $57, $58, $AA, $15, $CB, $02, $C2, $39, $E7, $36, $60, $76, $84, $6A, $10,
    $E2, $7C, $82, $C9, $0A, $66, $33, $74, $91, $59, $77, $90, $A0, $D2, $8B, $DD,
    $9E, $7D, $DC, $EC, $8A, $D4, $40, $BD, $E7, $84, $C3, $DB, $3F, $04, $EC, $00,
    $31, $22, $53, $B3, $6E, $03, $BC, $F6, $86, $A5, $1B, $36, $5E, $45, $80, $19,
    $5B, $00, $89, $C4, $C3, $A6, $50, $EC, $CA, $25, $CF, $31, $26, $DC, $22, $C1,
    $1D, $E0, $C1, $8F, $6C, $CC, $75, $09, $5F, $04, $05, $8A, $3D, $D8, $F1, $CB,
    $D5, $0C, $78, $88, $6E, $96, $D8, $E4, $9B, $C9, $6F, $C2, $03, $22, $40, $C2,
    $71, $8C, $57, $6B, $6F, $07, $33, $2A, $33, $55, $AC, $C4, $5B, $45, $9B, $FD,
    $EA, $80, $64, $4E, $30, $5B, $0C, $58, $75, $7D, $B6, $AE, $1A, $BF, $90, $D2,
    $C5, $62, $45, $B8, $C1, $30, $98, $87, $17, $85, $A7, $C2, $20, $B1, $F5, $D4,
    $45, $2B, $9B, $45, $1B, $39, $B4, $1D, $13, $1A, $DF, $A6, $C6, $2C, $90, $D9,
    $C5, $24, $07, $30, $E4, $BD, $0C, $70, $63, $33, $C6, $29, $7B, $60, $FE, $39,
    $61, $13, $8E, $FA, $D9, $C3, $50, $FB, $B6, $00, $C6, $F3, $CB, $09, $35, $FE,
    $D9, $C3, $DC, $62, $08, $D8, $68, $0E, $32, $D8, $6C, $2F, $FB, $F2, $B0, $DC,
    $3B, $BC, $04, $A6, $46, $54, $DC, $65, $E8, $94, $EC, $B6, $D0, $01, $01, $D7,
    $31, $09, $E3, $51, $08, $EF, $14, $0D, $E1, $4C, $60, $10, $A5, $D7, $54, $4D,
    $B0, $85, $A2, $D1, $2E, $05, $B9, $40, $B5, $42, $5D, $FF, $C7, $55, $01, $31,
    $E1, $9B, $BA, $DD, $BE, $CF, $BF, $08, $85, $CF, $63, $21, $B3, $5B, $DE, $1A,
    $E4, $DD, $06, $80, $7B, $93, $D9, $18, $9F, $69, $32, $10, $7D, $BF, $09, $58,
    $48, $05, $4F, $9D, $5B, $5D, $00, $B7, $A7, $53, $8B, $79, $40, $66, $48, $59,
    $F8, $CC, $20, $DC, $3C, $5A, $61, $C8, $1E, $B2, $3C, $27, $45, $FD, $C8, $C4,
    $39, $FD, $A4, $9E, $45, $4E, $C4, $9D, $4D, $90, $FF, $75, $0C, $1A, $A4, $5A,
    $50, $39, $D6, $DB, $E0, $5D, $B1, $85, $7E, $B6, $7D, $F0, $EB, $07, $6F, $46,
    $0F, $2B, $34, $08, $71, $7D, $72, $9F, $F7, $EB, $EF, $03, $40, $05, $62, $FF,
    $6E, $82, $E0, $54, $80, $A9, $8F, $18, $EB, $57, $AB, $83, $20, $8D, $72, $EA,
    $C4, $FC, $7B, $3E, $2B, $C2, $80, $47, $92, $83, $E0, $FE, $28, $D9, $7E, $89,
    $C6, $C8, $59, $48, $74, $37, $23, $08, $58, $50, $DD, $EC, $29, $80, $71, $7C,
    $AD, $04, $67, $C5, $0F, $01, $C2, $4A, $7E, $C1, $64, $C5, $5E, $B6, $D6, $17,
    $8C, $D7, $74, $15, $CA, $CF, $EB, $E8, $63, $44, $F1, $8A, $3F, $D4, $1C, $C4,
    $18, $C5, $A1, $CC, $D1, $95, $8B, $14, $A7, $6A, $42, $90, $6B, $11, $ED, $20,
    $76, $B1, $4E, $1C, $98, $45, $1C, $92, $31, $AA, $23, $FE, $DD, $53, $72, $35,
    $B4, $59, $3A, $E4, $29, $1F, $9D, $9C, $DE, $C1, $C8, $19, $D5, $E3, $FF, $64,
    $7B, $26, $54, $74, $96, $0F, $53, $04, $89, $03, $23, $74, $83, $04, $F1, $76,
    $1A, $CD, $D8, $17, $DD, $AC, $51, $1D, $AC, $22, $75, $DD, $0C, $23, $14, $C9,
    $A2, $3A, $5E, $C8, $08, $49, $22, $64, $DF, $F7, $04, $49, $05, $CF, $82, $DB,
    $EB, $17, $32, $B6, $83, $57, $2A, $87, $8F, $BC, $41, $89, $03, $21, $52, $B4,
    $80, $34, $47, $9A, $B2, $40, $0B, $DE, $14, $3F, $00, $17, $E2, $40, $69, $D6,
    $CF, $F2, $F5, $7C, $3E, $F7, $55, $BD, $3F, $AF, $10, $C7, $0D, $D0, $98, $A1,
    $2D, $E0, $47, $D5, $38, $10, $EF, $0A, $16, $45, $7B, $C5, $30, $1B, $1A, $83,
    $55, $99, $6E, $D0, $57, $94, $EC, $10, $9D, $56, $F5, $B9, $E2, $A8, $58, $31,
    $A8, $B2, $21, $38, $75, $34, $C9, $3C, $18, $C9, $61, $D9, $1E, $C5, $92, $26,
    $D0, $3E, $07, $D8, $D9, $32, $28, $5A, $BE, $29, $28, $65, $C0, $8E, $D9, $20,
    $C4, $68, $CA, $BC, $6F, $41, $C4, $A1, $20, $0A, $E1, $16, $76, $F4, $26, $DD,
    $3D, $4E, $18, $B6, $DD, $59, $0E, $C0, $F1, $2F, $8B, $F9, $01, $DD, $58, $08,
    $19, $C0, $10, $4F, $D5, $BD, $A8, $0A, $A5, $36, $A4, $0F, $91, $0A, $86, $45,
    $2F, $98, $80, $85, $03, $01, $1F, $B3, $40, $6F, $01, $E2, $23, $A8, $40, $5B,
    $A3, $10, $60, $8A, $1A, $6E, $C0, $31, $C0, $D2, $03, $67, $C9, $AA, $DA, $AE,
    $71, $03, $56, $54, $06, $9C, $86, $15, $1A, $F2, $56, $4C, $8B, $46, $12, $D1,
    $44, $DB, $46, $D4, $7E, $84, $51, $1A, $18, $75, $6E, $33, $7D, $88, $50, $60,
    $36, $52, $37, $42, $4C, $3A, $17, $1B, $D6, $03, $48, $2A, $8B, $D8, $8E, $34,
    $2D, $C4, $1A, $49, $33, $C5, $6E, $14, $15, $85, $03, $C8, $C8, $11, $BB, $B9,
    $5B, $EC, $DB, $CC, $0D, $10, $42, $CD, $C5, $20, $89, $14, $78, $87, $50, $11,
    $A9, $A1, $0D, $88, $0D, $07, $E8, $8B, $40, $1A, $21, $CB, $D5, $75, $44, $A4,
    $2C, $BF, $9F, $75, $25, $4E, $FC, $10, $F7, $06, $DC, $28, $2E, $DF, $A9, $81,
    $4D, $01, $82, $5D, $E0, $C8, $D8, $EA, $EB, $DA, $5E, $50, $1D, $07, $DC, $D2,
    $FA, $0C, $84, $AE, $82, $98, $84, $5E, $14, $34, $66, $8A, $A0, $B0, $29, $0C,
    $6C, $F0, $45, $D1, $16, $4C, $04, $13, $53, $12, $A2, $CE, $DA, $0A, $C9, $05,
    $0E, $5A, $0A, $67, $36, $2F, $53, $5E, $0D, $DC, $59, $4A, $52, $B4, $5D, $10,
    $0B, $C4, $3B, $E7, $DC, $87, $08, $E8, $74, $1A, $85, $B5, $CD, $E0, $52, $14,
    $38, $08, $CF, $D9, $C2, $7A, $09, $A0, $05, $13, $48, $3B, $28, $12, $52, $1C,
    $59, $8A, $93, $D8, $E3, $43, $49, $AA, $5E, $CC, $17, $DA, $15, $29, $9A, $70,
    $50, $DC, $96, $E4, $50, $F7, $E4, $66, $01, $1E, $48, $78, $7B, $47, $6C, $90,
    $5B, $48, $E4, $13, $AF, $E8, $E2, $ED, $5E, $73, $8A, $94, $D3, $03, $5B, $08,
    $D5, $70, $18, $55, $6B, $21, $CB, $01, $D2, $5D, $74, $A7, $AF, $73, $FD, $A2,
    $E4, $B2, $50, $3F, $92, $C3, $61, $12, $EE, $90, $84, $6D, $24, $F1, $A6, $E2,
    $C1, $D7, $25, $8F, $C4, $33, $97, $5A, $48, $D8, $BB, $B0, $06, $85, $29, $2D,
    $5C, $6F, $B7, $C0, $91, $3F, $CC, $F7, $27, $BC, $5A, $12, $E0, $06, $91, $68,
    $A0, $96, $86, $AA, $F1, $75, $40, $8D, $DD, $59, $CA, $64, $19, $25, $68, $64,
    $14, $5F, $63, $16, $61, $88, $CE, $5D, $08, $2E, $52, $F8, $8A, $75, $D6, $52,
    $C9, $4B, $C0, $BA, $7E, $08, $30, $9B, $59, $98, $E0, $05, $E4, $BA, $67, $07,
    $6A, $21, $E3, $74, $F7, $74, $47, $0A, $E9, $F0, $1E, $80, $4E, $3E, $7A, $DD,
    $87, $EB, $D7, $DB, $44, $02, $74, $45, $38, $DD, $EB, $0C, $4F, $C3, $72, $AD,
    $1B, $11, $13, $27, $19, $42, $E7, $2F, $80, $16, $B4, $EC, $29, $EB, $61, $E9,
    $66, $A5, $61, $E9, $5A, $0E, $18, $10, $AB, $70, $05, $04, $14, $69, $AB, $E2,
    $36, $6C, $C8, $D4, $22, $EB, $DB, $EF, $73, $63, $55, $04, $6F, $07, $AE, $7A,
    $AA, $82, $65, $B8, $0D, $B8, $FE, $5F, $F7, $84, $36, $43, $5F, $1F, $04, $83,
    $FE, $2A, $46, $05, $45, $47, $2F, $19, $D9, $06, $18, $14, $49, $5B, $75, $55,
    $02, $13, $B5, $43, $DF, $97, $A8, $C0, $C8, $B3, $99, $2D, $0A, $44, $8F, $40,
    $2E, $50, $CD, $D6, $AC, $73, $06, $38, $5C, $9D, $43, $1C, $4C, $80, $F8, $86,
    $F9, $28, $52, $FF, $53, $24, $85, $43, $10, $65, $33, $27, $4C, $71, $23, $C8,
    $F6, $DE, $39, $0E, $7E, $8D, $44, $5B, $FD, $82, $7E, $67, $A0, $8E, $0D, $C8,
    $71, $99, $A3, $81, $FE, $00, $B1, $89, $1A, $91, $74, $2A, $CF, $74, $1F, $98,
    $B7, $61, $EF, $58, $4B, $28, $51, $79, $95, $EF, $73, $93, $BF, $90, $0F, $43,
    $28, $50, $E0, $2B, $C9, $14, $D9, $A4, $CB, $66, $7D, $14, $C2, $D6, $3F, $2B,
    $2B, $B8, $6C, $1A, $B7, $9F, $5A, $1F, $41, $85, $2C, $33, $2E, $7D, $48, $E1,
    $A4, $3E, $35, $6A, $BF, $14, $01, $5C, $71, $43, $0E, $69, $0A, $62, $F6, $73,
    $FF, $FA, $45, $B7, $A5, $23, $26, $46, $02, $25, $EE, $00, $80, $1E, $66, $0F,
    $58, $DD, $12, $1E, $E9, $B1, $1D, $41, $A7, $17, $3C, $86, $D6, $15, $51, $82,
    $37, $C0, $2C, $2D, $5A, $39, $D8, $73, $07, $15, $74, $AB, $AB, $36, $4E, $DF,
    $50, $43, $57, $E8, $38, $52, $DB, $06, $3A, $52, $EC, $5E, $38, $CA, $7E, $6C,
    $02, $5C, $5A, $2F, $B0, $6D, $7B, $DC, $3B, $8A, $08, $4E, $58, $89, $FB, $00,
    $6F, $EA, $0D, $66, $E8, $D3, $E0, $77, $31, $C9, $8A, $4B, $77, $50, $F9, $AD,
    $54, $07, $CA, $8B, $46, $54, $89, $5E, $7D, $D4, $83, $B4, $ED, $ED, $F6, $E9,
    $03, $21, $C7, $44, $34, $89, $0A, $8B, $7E, $40, $52, $D4, $08, $40, $0B, $28,
    $40, $40, $8D, $43, $C0, $8F, $02, $D4, $A4, $4D, $EC, $29, $6A, $17, $A2, $76,
    $7E, $44, $8A, $45, $11, $D3, $65, $3E, $1D, $6C, $ED, $BB, $6D, $D4, $8A, $44,
    $1A, $02, $0E, $22, $C8, $B2, $21, $81, $73, $6F, $41, $EA, $E0, $21, $DA, $61,
    $6C, $81, $A9, $5B, $1B, $52, $47, $6F, $04, $51, $B8, $00, $BA, $D5, $5B, $7B,
    $47, $43, $39, $BD, $73, $C4, $0C, $05, $EA, $4B, $2E, $7E, $04, $2A, $49, $A4,
    $9B, $00, $44, $30, $4D, $DD, $C7, $0A, $68, $77, $83, $34, $48, $30, $51, $CA,
    $2A, $6C, $C1, $88, $0D, $4F, $23, $42, $30, $65, $D0, $E1, $6F, $13, $54, $1A,
    $70, $21, $DD, $1C, $33, $6A, $11, $F8, $58, $15, $3F, $83, $7A, $18, $02, $60,
    $2B, $23, $8A, $9D, $08, $61, $14, $76, $D2, $35, $A2, $D9, $2F, $E1, $32, $07,
    $82, $88, $05, $3B, $75, $0C, $1D, $4F, $36, $2D, $B4, $47, $CE, $8A, $BC, $16,
    $05, $1C, $48, $9D, $21, $C8, $CD, $4E, $1C, $DE, $AF, $82, $B8, $16, $74, $D6,
    $3F, $5C, $B8, $20, $97, $3C, $07, $51, $98, $6C, $0F, $F0, $DB, $38, $8C, $3E,
    $82, $80, $39, $14, $80, $18, $34, $2C, $C2, $B6, $7C, $14, $90, $B3, $7F, $45,
    $FC, $26, $57, $AA, $41, $8D, $51, $07, $8D, $41, $3F, $96, $0A, $85, $4A, $54,
    $27, $C0, $F2, $DB, $DB, $48, $D0, $37, $BF, $02, $0B, $74, $10, $F9, $09, $C0,
    $30, $5D, $F8, $86, $EE, $0F, $74, $06, $2F, $5B, $1B, $7A, $50, $0F, $75, $F7,
    $70, $08, $5B, $B0, $A8, $5D, $10, $A9, $26, $3C, $4F, $8C, $B1, $20, $0E, $1C,
    $7E, $58, $52, $90, $A0, $36, $06, $83, $6B, $04, $C4, $50, $5C, $20, $43, $C0,
    $8C, $78, $39, $D0, $60, $B1, $35, $08, $7A, $09, $FF, $D8, $36, $90, $86, $9D,
    $2E, $EA, $81, $F9, $15, $2F, $50, $BB, $FE, $AA, $FA, $85, $DA, $74, $2E, $3D,
    $40, $98, $FA, $7E, $18, $54, $22, $48, $08, $96, $3D, $23, $95, $2B, $DC, $F8,
    $E0, $EB, $C1, $0F, $C8, $6B, $84, $CA, $45, $6D, $B8, $F9, $45, $6B, $28, $D0,
    $94, $B8, $63, $8B, $79, $3E, $E6, $89, $56, $28, $F4, $E6, $A2, $9A, $36, $D6,
    $60, $06, $AF, $F9, $1A, $22, $65, $33, $CB, $36, $14, $0B, $49, $EE, $08, $5B,
    $5E, $45, $97, $91, $FC, $67, $0A, $03, $88, $80, $28, $1C, $2A, $BA, $5B, $25,
    $A2, $54, $50, $AF, $8E, $A8, $53, $E7, $52, $EF, $82, $B3, $02, $17, $7F, $EE,
    $07, $01, $47, $82, $5D, $51, $07, $4F, $28, $FF, $1D, $D3, $59, $74, $AE, $D4,
    $78, $82, $E0, $7B, $93, $C5, $39, $52, $04, $43, $BA, $0F, $9E, $F8, $6C, $C6,
    $F6, $8E, $7D, $46, $04, $3D, $E3, $84, $15, $42, $B9, $1D, $B7, $05, $07, $53,
    $D0, $74, $D2, $14, $97, $1B, $AC, $6C, $CE, $58, $20, $4D, $0A, $F9, $7C, $E9,
    $DD, $06, $A0, $96, $84, $41, $51, $56, $8D, $1C, $F6, $14, $9D, $38, $97, $00,
    $97, $8A, $53, $82, $9B, $40, $FE, $2B, $BC, $B7, $35, $A0, $07, $7F, $04, $30,
    $71, $9F, $F9, $27, $97, $CA, $53, $BB, $D8, $D5, $C5, $03, $79, $C7, $0A, $C1,
    $BA, $10, $5F, $07, $A5, $FA, $A7, $8D, $79, $D1, $05, $15, $B6, $02, $FC, $08,
    $4B, $EC, $7A, $D0, $42, $B2, $4A, $E4, $9F, $8D, $03, $4E, $96, $B3, $AE, $11,
    $13, $10, $09, $04, $1C, $08, $82, $B6, $3F, $59, $24, $10, $88, $44, $19, $01,
    $B5, $1C, $7B, $21, $C0, $95, $9A, $43, $2C, $88, $04, $19, $0E, $DD, $1A, $1B,
    $03, $4C, $01, $46, $5B, $56, $14, $F9, $48, $A6, $EC, $27, $18, $10, $8A, $47,
    $07, $BF, $6D, $B0, $98, $7D, $1A, $B0, $7F, $56, $19, $8D, $4A, $44, $D0, $04,
    $F7, $01, $83, $C2, $02, $14, $00, $F2, $E6, $5E, $A8, $BE, $9F, $83, $BE, $88,
    $0A, $DD, $8E, $20, $0B, $6D, $D7, $CC, $D8, $B0, $7A, $56, $5E, $CA, $42, $0C,
    $0E, $D0, $B6, $4B, $BD, $0F, $54, $04, $8D, $59, $60, $50, $6A, $D3, $6D, $AD,
    $A3, $DF, $C0, $2D, $1B, $1E, $40, $74, $39, $57, $22, $F7, $59, $14, $2E, $8D,
    $5F, $01, $1C, $5C, $A9, $D1, $EF, $5B, $36, $18, $0F, $5E, $35, $43, $13, $48,
    $2C, $0F, $83, $72, $0D, $BA, $19, $5E, $2F, $04, $45, $70, $22, $E0, $2B, $B0,
    $4F, $8F, $23, $42, $75, $63, $20, $57, $A1, $14, $0A, $5E, $DB, $B6, $0D, $7F,
    $EC, $39, $56, $20, $72, $37, $59, $DC, $27, $18, $10, $29, $20, $62, $EB, $6E,
    $67, $BB, $8A, $04, $3A, $8E, $0B, $1B, $41, $42, $3D, $89, $0A, $87, $2A, $45,
    $65, $89, $37, $51, $D4, $7E, $77, $17, $0C, $83, $04, $06, $3C, $CB, $3B, $5E,
    $0C, $75, $C9, $16, $62, $B8, $B7, $5D, $67, $09, $3B, $4A, $0F, $87, $A7, $16,
    $48, $58, $38, $59, $36, $7B, $53, $47, $73, $08, $C3, $37, $E7, $DA, $FB, $79,
    $22, $1C, $79, $73, $37, $8B, $30, $0A, $0D, $70, $A4, $63, $99, $EB, $88, $31,
    $84, $9D, $7F, $35, $2D, $72, $C3, $A6, $4C, $27, $B8, $FB, $EE, $89, $73, $18,
    $3A, $04, $B1, $D3, $7D, $06, $D3, $C9, $49, $EC, $33, $76, $33, $6C, $1E, $78,
    $DB, $51, $96, $01, $E3, $5B, $75, $4E, $9E, $B3, $16, $24, $0E, $67, $CC, $46,
    $D9, $68, $D2, $45, $2C, $93, $49, $8E, $F7, $5B, $A3, $26, $D2, $0C, $39, $D0,
    $76, $20, $A1, $60, $6D, $E2, $52, $B4, $47, $34, $C1, $78, $14, $B0, $39, $F8,
    $6F, $44, $EC, $D7, $A7, $C7, $3E, $35, $07, $29, $BC, $87, $B3, $FC, $16, $2E,
    $94, $2B, $27, $94, $41, $30, $13, $13, $E1, $69, $08, $11, $85, $82, $B3, $30,
    $85, $02, $30, $34, $1C, $17, $01, $2C, $41, $25, $A6, $C0, $F5, $35, $8F, $01,
    $E8, $60, $1B, $FA, $5C, $7B, $74, $84, $71, $1C, $26, $B8, $C7, $BB, $6D, $37,
    $1C, $24, $AF, $50, $2C, $DE, $08, $39, $CB, $62, $0A, $BC, $90, $85, $CF, $97,
    $62, $51, $2F, $41, $C3, $01, $59, $B1, $23, $9C, $D9, $2E, $84, $05, $E4, $B6,
    $C1, $68, $70, $86, $98, $EB, $A7, $42, $7D, $E8, $0D, $50, $B4, $20, $BA, $BD,
    $FA, $88, $14, $03, $32, $4B, $6D, $EB, $E1, $40, $C6, $89, $05, $47, $8A, $19,
    $B0, $1F, $3E, $F2, $1C, $A8, $CB, $EB, $89, $76, $69, $4D, $26, $64, $0A, $A2,
    $17, $17, $32, $95, $8C, $70, $1C, $69, $8F, $08, $65, $93, $E1, $49, $75, $45,
    $DD, $D1, $E5, $07, $1B, $E9, $CA, $2F, $AA, $29, $18, $DC, $18, $03, $7D, $D6,
    $11, $76, $48, $08, $87, $5A, $A8, $84, $57, $FB, $C1, $9B, $C7, $38, $FB, $24,
    $85, $C1, $FA, $56, $18, $B0, $01, $68, $A2, $E4, $6E, $49, $8E, $B4, $0C, $59,
    $DE, $87, $6A, $6C, $3A, $CD, $75, $7A, $2F, $4B, $30, $98, $5C, $2A, $54, $02,
    $3E, $B9, $ED, $0C, $3A, $BB, $52, $50, $7B, $C4, $13, $EC, $9E, $88, $5C, $A9,
    $94, $7B, $A7, $39, $2D, $6D, $12, $C9, $9E, $D3, $08, $83, $F4, $51, $30, $08,
    $D0, $C8, $3B, $67, $F0, $5E, $B6, $A7, $BF, $4F, $30, $4E, $54, $1F, $01, $54,
    $29, $E3, $73, $2C, $49, $42, $2F, $2E, $50, $D4, $AD, $D1, $83, $C5, $B5, $05,
    $53, $7E, $05, $F7, $3A, $08, $31, $66, $81, $73, $2F, $9B, $F2, $C1, $75, $41,
    $BC, $12, $7D, $71, $AD, $79, $8A, $C4, $0B, $57, $5C, $4B, $80, $3E, $60, $AA,
    $68, $F4, $01, $BF, $A8, $52, $1C, $4A, $0D, $5A, $38, $84, $73, $11, $F8, $30,
    $BB, $F6, $3B, $8C, $78, $10, $29, $DF, $01, $D9, $09, $22, $14, $29, $DD, $1E,
    $78, $08, $6F, $06, $82, $5A, $85, $5A, $10, $42, $86, $00, $D9, $55, $D7, $85,
    $86, $7F, $6C, $D8, $62, $FC, $40, $18, $4F, $40, $03, $61, $30, $83, $EA, $3A,
    $58, $5B, $40, $7E, $0C, $B6, $8D, $9A, $81, $48, $7C, $2A, $49, $8E, $D2, $84,
    $C2, $0D, $14, $2E, $7D, $C1, $E1, $06, $09, $6F, $5E, $6C, $03, $58, $C9, $F0,
    $C1, $F8, $E3, $20, $BF, $1F, $15, $C8, $31, $D2, $F2, $F7, $F7, $29, $D1, $A1,
    $94, $6B, $CB, $0A, $CC, $1F, $4F, $FE, $5C, $9A, $85, $2F, $F7, $04, $0A, $10,
    $C0, $7A, $8C, $7E, $0A, $FA, $50, $DA, $46, $7E, $41, $FF, $6C, $9E, $74, $5D,
    $C2, $85, $AE, $79, $94, $56, $30, $5E, $E8, $C0, $E9, $10, $90, $46, $8E, $70,
    $C8, $13, $47, $C1, $4C, $1F, $7D, $E0, $42, $0E, $59, $4E, $57, $0B, $4A, $3A,
    $B2, $4D, $10, $65, $7B, $54, $03, $9A, $93, $89, $D3, $D0, $01, $5D, $B5, $99,
    $0D, $0C, $81, $07, $A3, $94, $A6, $9D, $1F, $E7, $C6, $D8, $43, $3D, $01, $0D,
    $05, $63, $13, $FF, $ED, $1C, $0C, $83, $06, $64, $60, $2F, $DA, $1C, $99, $70,
    $91, $82, $E5, $D2, $4C, $35, $EB, $AC, $23, $D0, $0A, $D4, $62, $E7, $F8, $32,
    $15, $CA, $92, $4E, $43, $2E, $51, $48, $82, $CF, $B9, $AF, $F2, $2C, $03, $99,
    $51, $24, $8B, $23, $17, $BA, $F0, $F8, $B2, $29, $CB, $52, $53, $C4, $BD, $01,
    $F9, $E5, $58, $30, $9F, $91, $A5, $A1, $53, $B1, $26, $4D, $41, $D7, $6B, $44,
    $8E, $6C, $C6, $89, $4A, $02, $4F, $03, $64, $47, $92, $1B, $00, $0C, $01, $2A,
    $D6, $A7, $F0, $DA, $F6, $A0, $7A, $14, $09, $89, $04, $56, $73, $B1, $FD, $5C,
    $54, $EB, $7A, $08, $10, $10, $AD, $AF, $59, $60, $21, $E2, $DB, $A6, $4F, $FE,
    $C9, $E6, $20, $06, $07, $10, $04, $5E, $52, $D5, $06, $40, $CE, $DE, $E8, $2E,
    $D5, $A0, $E2, $9C, $00, $8C, $41, $FC, $D8, $75, $51, $90, $1B, $EC, $64, $D4,
    $D6, $55, $85, $A9, $61, $6C, $2D, $62, $F6, $B8, $42, $B9, $25, $96, $57, $28,
    $77, $E7, $B8, $7D, $EC, $2D, $72, $01, $C7, $89, $0A, $CF, $C8, $61, $17, $03,
    $8B, $59, $9F, $7D, $47, $D6, $DD, $A9, $E8, $9D, $38, $4D, $E8, $09, $DB, $84,
    $4B, $62, $92, $53, $A3, $6E, $49, $74, $8B, $35, $30, $18, $97, $09, $9B, $4B,
    $60, $7D, $E3, $C1, $08, $C5, $C6, $04, $10, $1F, $66, $B3, $0D, $92, $79, $C6,
    $44, $1B, $42, $10, $46, $CB, $51, $24, $36, $35, $08, $7E, $C5, $BB, $B5, $B1,
    $E7, $96, $F7, $07, $F7, $08, $1B, $11, $76, $C6, $68, $BE, $65, $D5, $10, $39,
    $56, $F3, $5A, $32, $64, $A6, $D9, $10, $5E, $02, $5F, $00, $85, $9B, $F1, $46,
    $21, $F5, $27, $45, $73, $41, $AE, $B6, $29, $34, $13, $D8, $2C, $0E, $F2, $AA,
    $0B, $7E, $27, $A6, $6D, $C1, $98, $C4, $B7, $B5, $39, $0B, $B9, $71, $83, $FE,
    $FA, $AD, $87, $17, $74, $4B, $9D, $10, $4A, $7F, $D6, $EB, $D2, $D1, $12, $40,
    $A1, $D5, $FC, $53, $20, $5C, $20, $17, $D7, $82, $0C, $50, $BD, $CB, $C8, $F2,
    $72, $5A, $FB, $89, $5A, $5D, $43, $F8, $B5, $C6, $B2, $3C, $43, $43, $F8, $15,
    $09, $38, $CB, $96, $D9, $22, $14, $29, $96, $DB, $0C, $D9, $61, $A2, $CD, $68,
    $62, $B2, $45, $D6, $FD, $64, $59, $96, $65, $50, $46, $55, $C1, $5A, $80, $3D,
    $C3, $25, $26, $48, $DB, $65, $26, $E0, $10, $20, $5D, $B5, $15, $72, $01, $16,
    $87, $77, $40, $40, $D9, $2A, $DF, $2E, $2F, $5B, $AD, $64, $CD, $43, $39, $0B,
    $9B, $AF, $51, $C9, $65, $11, $3A, $CB, $15, $10, $5B, $D9, $0D, $18, $58, $43,
    $33, $2A, $3D, $90, $09, $6C, $00, $08, $53, $08, $5F, $B0, $CD, $25, $11, $56,
    $0B, $14, $A8, $06, $90, $75, $01, $48, $66, $7F, $10, $2B, $CF, $80, $E0, $D8,
    $84, $AB, $0E, $DC, $10, $D3, $93, $8B, $E0, $6A, $00, $6B, $C3, $4E, $4B, $93,
    $3A, $47, $53, $6B, $42, $4D, $67, $58, $50, $42, $8F, $CE, $09, $9A, $5D, $66,
    $27, $8F, $D5, $3B, $1E, $DC, $37, $51, $0F, $8F, $D9, $F4, $9B, $D5, $5D, $90,
    $2F, $46, $EE, $49, $7C, $DE, $86, $24, $AA, $0F, $36, $DE, $92, $0E, $6E, $D2,
    $7B, $2B, $4C, $D4, $FC, $D2, $5F, $44, $66, $C7, $44, $5A, $FE, $85, $8D, $44,
    $1B, $FE, $51, $52, $76, $38, $EC, $44, $ED, $8D, $57, $11, $88, $C1, $30, $36,
    $C7, $51, $20, $98, $5F, $1B, $11, $9C, $E0, $25, $10, $D5, $BC, $0F, $82, $5E,
    $B9, $85, $8F, $32, $E5, $8A, $1C, $F0, $84, $DD, $8B, $77, $1A, $23, $5B, $81,
    $07, $24, $83, $BB, $08, $C3, $09, $01, $AA, $45, $6D, $46, $10, $5C, $2F, $0D,
    $2A, $02, $54, $2A, $FB, $F8, $6D, $05, $BE, $9D, $18, $7D, $15, $CA, $93, $36,
    $8E, $84, $03, $BC, $F5, $AB, $E8, $5B, $8D, $26, $8B, $14, $95, $0D, $0B, $14,
    $BD, $42, $3B, $44, $06, $94, $47, $F7, $EF, $22, $9A, $08, $83, $A7, $D9, $74,
    $54, $8D, $3C, $5B, $89, $9E, $2D, $CD, $DD, $08, $E2, $C1, $E7, $3A, $87, $32,
    $23, $06, $29, $73, $E4, $C8, $97, $30, $9F, $34, $8F, $36, $8F, $67, $A4, $7B,
    $10, $FF, $05, $E3, $E1, $A8, $CF, $67, $F3, $89, $86, $80, $00, $96, $8C, $9E,
    $90, $4E, $7C, $41, $C0, $E9, $A7, $69, $F0, $89, $8E, $DA, $D0, $D8, $03, $96,
    $AA, $92, $0C, $BB, $D5, $6C, $80, $C0, $46, $09, $E1, $2E, $57, $42, $CC, $82,
    $15, $A6, $B6, $13, $B0, $04, $0A, $BE, $1A, $78, $4F, $C0, $33, $82, $4E, $2B,
    $10, $5E, $1C, $D7, $7C, $04, $75, $02, $35, $1B, $20, $2A, $0A, $18, $6C, $A6,
    $80, $18, $0A, $1F, $8F, $AA, $C4, $81, $A2, $D4, $28, $9C, $6C, $6F, $EB, $18,
    $18, $0C, $14, $44, $06, $08, $18, $D1, $7D, $49, $01, $71, $89, $4B, $10, $33,
    $88, $44, $07, $45, $A2, $26, $8E, $E7, $47, $70, $D5, $30, $B4, $74, $2A, $E6,
    $1C, $24, $AD, $81, $84, $09, $FE, $C8, $CC, $FC, $13, $45, $B6, $30, $37, $28,
    $56, $FE, $FC, $8B, $73, $4C, $DF, $68, $0F, $40, $EA, $2C, $B9, $C8, $D2, $89,
    $53, $3C, $91, $8E, $23, $1D, $0E, $71, $36, $FE, $0D, $51, $E1, $D9, $C9, $EF,
    $8B, $93, $84, $78, $43, $6C, $5C, $06, $74, $00, $9E, $2B, $50, $6F, $1B, $52,
    $09, $78, $AB, $19, $21, $85, $73, $32, $72, $60, $02, $68, $48, $8F, $E1, $2C,
    $98, $93, $B2, $82, $70, $E6, $95, $5C, $C2, $EC, $67, $8A, $89, $B3, $80, $20,
    $9B, $E6, $8D, $8C, $30, $20, $95, $83, $8F, $8B, $73, $30, $8B, $C8, $01, $08,
    $DB, $06, $06, $6E, $F2, $87, $F8, $0B, $39, $15, $0A, $45, $1B, $72, $E2, $A4,
    $09, $33, $88, $1A, $09, $F5, $09, $81, $0F, $5E, $50, $0B, $41, $B6, $D8, $20,
    $FC, $E2, $2E, $1E, $02, $DC, $5F, $07, $80, $38, $31, $77, $52, $B4, $9B, $BE,
    $7D, $24, $38, $06, $10, $48, $16, $74, $84, $0E, $31, $64, $A9, $E2, $4B, $20,
    $66, $8A, $9A, $40, $6F, $1D, $0A, $09, $1E, $84, $F6, $20, $C0, $0E, $B5, $4E,
    $8B, $7B, $24, $7A, $3A, $4C, $99, $9B, $14, $24, $E0, $7C, $E6, $27, $8D, $CD,
    $85, $2F, $5F, $F6, $9B, $2F, $03, $FE, $0F, $D4, $76, $8D, $7F, $82, $06, $A6,
    $18, $49, $83, $F9, $08, $F8, $6C, $10, $08, $0B, $80, $B1, $49, $22, $23, $07,
    $CF, $DE, $DE, $60, $5F, $29, $9F, $15, $0C, $30, $4B, $C1, $EF, $1F, $DB, $93,
    $61, $EB, $51, $09, $16, $C1, $18, $2A, $CF, $1A, $F1, $CA, $10, $CD, $9B, $02,
    $55, $1C, $C1, $EA, $1C, $70, $44, $E7, $36, $0B, $6F, $4A, $D7, $02, $11, $A5,
    $20, $B7, $7B, $C5, $08, $34, $AE, $62, $45, $A3, $44, $9D, $2D, $84, $57, $D4,
    $BD, $0D, $45, $B8, $FC, $D4, $A0, $6B, $5D, $F7, $27, $E2, $50, $F1, $BC, $85,
    $77, $30, $D3, $E0, $9E, $FF, $91, $A0, $7E, $89, $57, $18, $83, $C1, $07, $4B,
    $47, $2C, $89, $4F, $50, $D3, $E2, $48, $60, $6B, $5F, $7E, $16, $47, $34, $09,
    $B8, $AB, $AA, $00, $21, $4C, $DF, $B6, $7F, $23, $77, $7B, $1C, $F7, $E1, $D1,
    $EA, $4E, $36, $54, $0F, $58, $89, $1F, $C7, $78, $50, $74, $A0, $47, $21, $C9,
    $5B, $51, $0D, $F8, $74, $02, $E7, $15, $28, $16, $6E, $73, $B8, $56, $76, $5B,
    $3E, $38, $12, $FB, $F9, $CE, $62, $C5, $51, $D2, $12, $40, $25, $4C, $BC, $6D,
    $CE, $0D, $64, $15, $64, $06, $18, $44, $D3, $65, $A5, $61, $DF, $0F, $04, $89,
    $8F, $9C, $BD, $04, $34, $D2, $76, $04, $35, $7A, $0F, $ED, $29, $77, $38, $34,
    $16, $B3, $22, $B6, $73, $0C, $0A, $9E, $EF, $F3, $59, $EA, $77, $40, $0A, $DF,
    $47, $05, $1F, $95, $A7, $6E, $D4, $68, $CC, $8D, $1B, $F5, $53, $76, $8D, $34,
    $52, $0A, $01, $C8, $B7, $6D, $8A, $0D, $F7, $31, $89, $87, $A4, $4D, $04, $1C,
    $B1, $97, $98, $D3, $48, $AD, $D5, $0B, $8A, $91, $14, $BD, $B7, $F5, $A7, $E7,
    $F7, $F0, $88, $57, $24, $FB, $86, $56, $73, $1C, $7B, $B0, $96, $E5, $59, $96,
    $7B, $FF, $70, $4B, $C9, $65, $D4, $76, $31, $79, $43, $74, $56, $10, $05, $72,
    $72, $72, $B2, $B3, $43, $43, $43, $46, $32, $53, $F2, $29, $B0, $40, $02, $46,
    $CB, $81, $2C, $B5, $4E, $B0, $52, $43, $46, $2C, $CB, $20, $27, $34, $46, $7E,
    $5E, $4F, $03, $37, $8A, $4F, $5E, $3C, $BF, $47, $4C, $E0, $9C, $2C, $95, $00,
    $B0, $57, $96, $05, $22, $27, $9D, $51, $B3, $46, $26, $A9, $1D, $38, $B5, $52,
    $09, $B6, $27, $09, $B6, $72, $D2, $C9, $16, $06, $60, $68, $AC, $9F, $E9, $34,
    $81, $73, $87, $23, $98, $AA, $64, $4E, $E0, $64, $9E, $8F, $4D, $9F, $AB, $D9,
    $A4, $B3, $A0, $86, $AC, $2A, $E9, $8A, $B3, $1D, $A9, $8E, $AE, $5E, $7C, $50,
    $48, $A2, $25, $C5, $31, $AF, $B8, $FA, $C7, $13, $80, $4C, $3B, $EF, $EE, $10,
    $89, $94, $63, $C3, $81, $ED, $E4, $26, $90, $0C, $0C, $4C, $62, $89, $D2, $9D,
    $C6, $3E, $A4, $BB, $90, $F7, $DE, $1E, $00, $66, $BE, $09, $08, $95, $A7, $68,
    $24, $04, $7E, $70, $0C, $B3, $45, $BB, $E2, $8B, $3D, $48, $2B, $7B, $18, $1B,
    $51, $4A, $2E, $B0, $E8, $74, $4A, $0E, $3C, $44, $03, $48, $F4, $74, $27, $B9,
    $12, $17, $ED, $22, $9D, $B4, $0A, $2B, $7B, $E7, $21, $32, $19, $8B, $9A, $ED,
    $75, $36, $06, $44, $62, $2A, $49, $49, $D4, $15, $18, $33, $FF, $78, $51, $C1,
    $C7, $66, $51, $2C, $35, $32, $3C, $56, $56, $90, $E8, $84, $44, $AF, $EB, $B8,
    $DC, $3A, $97, $FC, $3D, $EB, $BE, $20, $7B, $28, $57, $A5, $BC, $B0, $4E, $76,
    $17, $11, $8C, $51, $E8, $E6, $55, $18, $15, $5F, $C3, $68, $09, $32, $8F, $A8,
    $AD, $2A, $C1, $55, $7A, $6B, $4D, $50, $DB, $DD, $09, $C5, $2A, $08, $6A, $0F,
    $03, $51, $D1, $C8, $82, $DA, $E4, $E3, $9C, $0F, $07, $15, $93, $14, $AF, $75,
    $9D, $03, $78, $A0, $48, $94, $B6, $0A, $D8, $3E, $61, $09, $B5, $4D, $F0, $65,
    $B8, $FC, $B9, $0E, $AE, $97, $AC, $1F, $DB, $DF, $F3, $A5, $56, $A1, $56, $95,
    $6E, $D0, $90, $C6, $0C, $D1, $89, $2B, $40, $E5, $80, $9A, $1F, $0C, $B0, $6E,
    $6A, $9A, $66, $ED, $1E, $EC, $7E, $2C, $57, $89, $6C, $C2, $CF, $57, $12, $46,
    $56, $2C, $52, $38, $12, $D1, $9A, $90, $EF, $40, $25, $4C, $44, $00, $B5, $68,
    $D8, $A9, $CF, $F8, $28, $64, $95, $A1, $AA, $3D, $F4, $C7, $1A, $29, $27, $0D,
    $CB, $16, $AE, $40, $7C, $FF, $66, $5F, $38, $19, $58, $20, $65, $B3, $23, $CF,
    $C8, $76, $C7, $0A, $9B, $44, $7B, $40, $06, $F3, $8C, $3C, $5B, $38, $3E, $D7,
    $C4, $82, $B0, $E4, $73, $1C, $56, $D2, $E9, $6B, $4E, $58, $45, $3F, $46, $44,
    $79, $17, $1A, $6F, $76, $80, $A1, $71, $FF, $2C, $01, $C9, $53, $14, $9D, $50,
    $3B, $22, $50, $38, $53, $14, $4E, $00, $51, $A8, $23, $B5, $C9, $51, $2B, $80,
    $1C, $6C, $73, $D7, $C8, $40, $18, $4E, $4C, $D9, $E6, $26, $CD, $44, $44, $15,
    $1B, $0C, $16, $A8, $A1, $26, $CD, $08, $08, $2C, $41, $1D, $1A, $A8, $DC, $58,
    $59, $08, $70, $8E, $1D, $73, $75, $54, $6C, $D1, $03, $BD, $10, $DE, $0A, $F4,
    $2A, $DD, $6A, $49, $2F, $01, $D3, $9F, $E0, $B7, $D5, $E1, $B7, $B9, $98, $1D,
    $8E, $94, $FD, $A8, $09, $05, $FC, $6D, $9E, $74, $9E, $7C, $0A, $AB, $18, $0B,
    $BE, $24, $05, $9E, $30, $0B, $07, $01, $A4, $63, $AC, $E8, $DA, $25, $4C, $A6,
    $5B, $49, $0E, $08, $29, $18, $AC, $87, $64, $B6, $49, $2D, $0B, $A0, $31, $E9,
    $91, $F4, $25, $8C, $EF, $E2, $11, $D3, $06, $BE, $81, $71, $BE, $26, $C8, $FA,
    $29, $BD, $20, $BE, $81, $71, $FD, $14, $B2, $7A, $55, $AC, $A4, $1F, $7D, $17,
    $2A, $7C, $0B, $CD, $38, $89, $75, $CC, $67, $B3, $4F, $6C, $A0, $6D, $BF, $5A,
    $B9, $80, $01, $CE, $19, $E8, $13, $02, $2E, $01, $D5, $44, $AB, $7F, $2C, $AA,
    $37, $11, $CF, $F7, $8D, $87, $6E, $39, $C1, $76, $0C, $9C, $CD, $51, $97, $2A,
    $F8, $05, $C0, $3F, $E4, $03, $13, $E1, $1A, $89, $05, $A0, $E4, $7F, $22, $4A,
    $55, $6C, $9F, $EC, $8D, $8C, $0F, $FE, $A5, $87, $82, $02, $F6, $EC, $0B, $D8,
    $8A, $4C, $30, $FF, $88, $4D, $FF, $F5, $EF, $08, $22, $8A, $04, $37, $88, $45,
    $D6, $3B, $B1, $FA, $72, $03, $C1, $EB, $02, $77, $B7, $DB, $08, $8B, $74, $33,
    $D0, $39, $71, $76, $2A, $76, $EB, $25, $90, $74, $AB, $C6, $BB, $8B, $41, $7D,
    $E0, $21, $C2, $90, $2C, $89, $CA, $58, $2E, $04, $67, $70, $3B, $5C, $86, $C3,
    $4B, $FD, $DB, $87, $5A, $28, $BC, $A9, $CC, $8A, $48, $01, $D1, $38, $04, $31,
    $75, $CF, $A5, $BB, $2D, $BE, $09, $D7, $38, $44, $31, $41, $C6, $6E, $8A, $07,
    $38, $83, $25, $1A, $B0, $FD, $BD, $8A, $47, $01, $38, $41, $07, $B5, $66, $12,
    $34, $4D, $F7, $ED, $02, $47, $8A, $0B, $38, $07, $75, $3F, $07, $02, $37, $03,
    $D3, $34, $4D, $D3, $2F, $04, $27, $05, $1F, $CA, $D6, $34, $4D, $06, $17, $07,
    $0F, $3A, $08, $14, $2F, $05, $F0, $39, $B2, $D8, $72, $B9, $3E, $DD, $ED, $2D,
    $5C, $B9, $D4, $FD, $29, $C1, $0B, $2D, $39, $F1, $10, $3D, $5E, $82, $7F, $0F,
    $8E, $49, $B3, $7B, $8B, $2F, $2C, $D7, $4D, $36, $56, $70, $89, $CE, $54, $8A,
    $44, $08, $10, $33, $2C, $DC, $5A, $34, $EC, $D7, $8A, $09, $FE, $38, $FF, $EA,
    $5B, $B3, $61, $D6, $86, $EC, $0C, $89, $27, $75, $D0, $76, $03, $08, $55, $0B,
    $05, $B6, $9E, $28, $9F, $31, $46, $95, $16, $9F, $21, $40, $28, $95, $A1, $04,
    $D6, $1E, $B8, $86, $02, $B3, $09, $2F, $ED, $F0, $84, $82, $C4, $83, $82, $E8,
    $83, $48, $DD, $1A, $54, $2D, $1D, $FF, $FC, $47, $74, $A6, $1B, $6E, $A1, $04,
    $27, $F0, $59, $F0, $31, $C9, $D1, $85, $82, $AE, $88, $49, $B1, $DA, $64, $39,
    $A0, $2D, $E8, $89, $50, $04, $92, $41, $C6, $DA, $FB, $2A, $B3, $32, $45, $A8,
    $58, $3B, $B1, $AB, $9B, $75, $01, $9C, $11, $52, $69, $0D, $8B, $96, $BE, $DA,
    $5F, $6A, $71, $0F, $D8, $01, $DE, $89, $01, $89, $08, $29, $D9, $5B, $6C, $2C,
    $E1, $58, $F8, $08, $0F, $E4, $AD, $74, $D5, $B6, $D5, $1B, $BF, $02, $76, $27,
    $CB, $38, $95, $6C, $E2, $2D, $16, $6E, $3C, $4F, $58, $D0, $13, $3F, $48, $8A,
    $54, $F7, $13, $A8, $97, $3A, $5F, $54, $D3, $0A, $31, $D0, $21, $D8, $15, $5B,
    $87, $CD, $0D, $D8, $05, $01, $87, $CC, $AF, $68, $70, $09, $BA, $37, $AB, $04,
    $C7, $B0, $D5, $3C, $C0, $3D, $FE, $CE, $45, $A5, $9F, $77, $6C, $29, $F2, $8D,
    $84, $08, $5E, $5C, $A0, $A2, $10, $62, $2D, $82, $1A, $20, $B6, $E8, $3E, $4E,
    $57, $38, $1E, $53, $A7, $0A, $EB, $98, $01, $85, $6A, $33, $91, $70, $74, $5C,
    $D9, $0A, $7C, $5C, $29, $D6, $29, $27, $09, $89, $3B, $44, $AD, $B9, $D8, $0F,
    $4C, $AB, $12, $10, $2E, $01, $75, $C9, $04, $BC, $48, $90, $FF, $31, $DB, $5D,
    $18, $A4, $80, $8E, $3B, $3B, $72, $06, $73, $FF, $5B, $01, $E9, $2B, $5D, $EC,
    $49, $66, $89, $1A, $75, $E2, $54, $AA, $A1, $85, $B2, $6D, $40, $27, $4B, $26,
    $89, $67, $E0, $8E, $14, $50, $EB, $01, $C2, $96, $B5, $80, $55, $9F, $6F, $85,
    $90, $FE, $E9, $3C, $87, $1C, $56, $2D, $68, $4A, $E8, $E8, $ED, $C6, $3E, $88,
    $86, $78, $79, $51, $1C, $48, $1E, $26, $18, $60, $55, $48, $0F, $60, $A1, $C8,
    $06, $FF, $FF, $FF, $93, $83, $00, $78, $BA, $ED, $79, $2C, $1D, $0D, $73, $03,
    $A6, $5F, $D4, $BD, $80, $BE, $84, $86, $22, $F6, $75, $4B, $61, $A8, $D0, $75,
    $6C, $A5, $87, $8D, $D0, $2A, $30, $15, $0B, $30, $AF, $4C, $E8, $BA, $B0, $D0,
    $79, $49, $CA, $78, $74, $6A, $9E, $52, $28, $E4, $58, $00, $B3, $05, $19, $AC,
    $17, $6A, $3F, $0A, $7D, $1A, $7C, $42, $0F, $5F, $5C, $61, $A8, $13, $11, $C4,
    $2E, $1B, $36, $E8, $0B, $02, $72, $14, $39, $F0, $59, $C6, $96, $85, $0B, $3A,
    $28, $4E, $45, $B2, $2C, $5D, $80, $DB, $0E, $E7, $59, $62, $2F, $5F, $2C, $4D,
    $81, $EB, $05, $66, $16, $3E, $A9, $DA, $93, $79, $58, $5E, $41, $B8, $DE, $1F,
    $4F, $3B, $86, $33, $B7, $CC, $E0, $55, $97, $58, $CF, $58, $3B, $84, $53, $6D,
    $0A, $37, $57, $3D, $C7, $A7, $00, $75, $8B, $50, $99, $6D, $EB, $93, $28, $C8,
    $84, $FB, $F2, $ED, $0C, $E5, $7F, $80, $60, $01, $C9, $C7, $74, $7A, $22, $B3,
    $15, $69, $3D, $05, $97, $94, $DB, $6D, $74, $93, $C8, $8A, $51, $23, $5C, $D6,
    $29, $D3, $5D, $AD, $25, $E4, $6C, $53, $93, $D0, $5A, $5D, $57, $2C, $73, $B3,
    $5C, $E9, $99, $93, $2D, $66, $66, $2B, $D5, $7B, $BA, $0A, $C4, $55, $28, $B1,
    $A9, $CE, $6D, $01, $F7, $EF, $EA, $79, $14, $41, $E4, $2F, $AD, $4B, $0C, $51,
    $46, $6C, $D5, $C0, $DF, $61, $FD, $F2, $44, $3B, $EB, $5A, $6B, $EE, $0E, $61,
    $89, $F2, $42, $13, $6D, $63, $C1, $66, $4A, $01, $F1, $A7, $5F, $6F, $08, $26,
    $CA, $FE, $36, $A3, $72, $10, $4A, $84, $89, $F6, $4C, $1C, $B2, $4D, $E2, $40,
    $DD, $2F, $5B, $4A, $14, $C3, $9E, $6C, $09, $DD, $B8, $03, $C8, $06, $60, $B1,
    $3A, $BD, $96, $C9, $66, $E4, $B2, $E6, $7A, $6B, $7C, $5A, $F3, $9B, $1D, $90,
    $97, $5A, $DB, $75, $0D, $F0, $71, $6D, $09, $A4, $0B, $56, $D6, $AF, $BF, $3C,
    $0F, $E6, $A0, $B0, $DC, $1C, $26, $81, $7D, $E4, $82, $76, $EA, $58, $08, $86,
    $3E, $B6, $0C, $B9, $BE, $80, $16, $06, $D9, $27, $48, $40, $DE, $41, $73, $3D,
    $C1, $57, $39, $0A, $85, $E8, $8A, $5C, $31, $F6, $31, $80, $AE, $E7, $34, $CF,
    $21, $CE, $31, $D8, $0C, $E0, $92, $CB, $F4, $44, $21, $D0, $AF, $4E, $AA, $14,
    $41, $3A, $CD, $60, $09, $72, $73, $10, $92, $AB, $21, $53, $57, $52, $0F, $41,
    $0C, $38, $30, $BB, $8F, $BC, $D1, $C4, $E8, $57, $B4, $D9, $81, $EA, $D1, $77,
    $1B, $7F, $FD, $82, $56, $F3, $80, $42, $FE, $F9, $87, $95, $03, $F6, $DD, $A8,
    $57, $36, $07, $A4, $03, $7E, $60, $14, $26, $36, $70, $BA, $8A, $5E, $4F, $8F,
    $A0, $E8, $F1, $13, $5B, $4B, $FF, $5F, $60, $B7, $F8, $8D, $50, $FD, $B0, $05,
    $9C, $28, $0C, $FB, $FA, $4E, $CF, $36, $AB, $45, $02, $D9, $1B, $A0, $4B, $1F,
    $3B, $F5, $1B, $C7, $DB, $46, $8A, $9A, $70, $3B, $11, $66, $FF, $84, $9F, $18,
    $98, $DB, $DD, $98, $04, $07, $81, $F9, $FF, $E8, $87, $B2, $B9, $EF, $9F, $09,
    $7E, $8A, $82, $70, $99, $9F, $87, $34, $85, $80, $09, $2C, $B6, $81, $B3, $C5,
    $44, $37, $76, $8F, $DC, $49, $39, $79, $EF, $0C, $0C, $23, $80, $EB, $99, $29,
    $C2, $3B, $87, $6B, $2E, $82, $57, $32, $83, $22, $B9, $A0, $60, $BA, $73, $29,
    $02, $AD, $20, $36, $A8, $C0, $A1, $35, $32, $23, $3F, $88, $C2, $B0, $10, $0B,
    $91, $45, $54, $36, $69, $A1, $11, $7C, $89, $57, $44, $30, $49, $D0, $EE, $4A,
    $DC, $40, $F5, $4D, $E0, $F0, $55, $D4, $8A, $2C, $EA, $82, $7A, $8B, $F5, $D3,
    $65, $C8, $33, $DE, $91, $86, $C0, $D4, $E5, $73, $F9, $C8, $D8, $0A, $4B, $98,
    $D0, $6E, $D9, $67, $30, $C4, $08, $5F, $50, $23, $0F, $7E, $C4, $70, $41, $39,
    $82, $ED, $04, $41, $0F, $D8, $02, $AD, $11, $BC, $8E, $DC, $4F, $60, $8A, $6D,
    $21, $C2, $8E, $DA, $86, $49, $16, $42, $89, $10, $3B, $A1, $8D, $42, $60, $0C,
    $A2, $DA, $89, $6D, $A0, $84, $58, $A0, $8D, $D5, $EC, $26, $D2, $8B, $A6, $C3,
    $00, $72, $E9, $DA, $BF, $53, $84, $57, $C2, $52, $DA, $F9, $60, $72, $93, $D7,
    $D9, $F9, $15, $80, $87, $E1, $FF, $3D, $87, $C2, $B3, $AC, $60, $EA, $61, $28,
    $4F, $74, $78, $6B, $18, $28, $14, $05, $68, $96, $FE, $7E, $60, $1B, $42, $50,
    $40, $06, $49, $9A, $29, $40, $6E, $3D, $B6, $34, $8E, $FD, $5C, $0B, $39, $C9,
    $36, $02, $12, $02, $D2, $11, $6F, $19, $B5, $37, $CF, $42, $46, $1B, $EC, $57,
    $94, $F2, $3B, $0F, $38, $AD, $60, $83, $FC, $D1, $08, $90, $5C, $8A, $1C, $30,
    $C7, $97, $6C, $2C, $C1, $04, $4A, $2A, $1C, $5E, $E3, $46, $48, $F0, $68, $19,
    $53, $76, $B7, $8C, $C1, $B8, $81, $1B, $4A, $39, $97, $29, $4B, $2A, $C3, $B5,
    $42, $D4, $1E, $15, $CB, $33, $86, $09, $C0, $AE, $43, $6E, $F3, $BF, $AF, $8C,
    $A8, $7C, $0D, $B4, $77, $D6, $61, $60, $0E, $89, $65, $24, $4C, $17, $E7, $04,
    $1E, $6C, $AA, $1E, $20, $9E, $39, $90, $77, $F0, $3D, $95, $3F, $DC, $17, $FA,
    $E9, $07, $89, $CB, $89, $B5, $83, $70, $9A, $43, $26, $CA, $FA, $C3, $E0, $90,
    $BF, $1D, $D6, $40, $0E, $06, $04, $27, $FE, $52, $94, $0F, $86, $38, $EB, $E4,
    $40, $D9, $EB, $6C, $E5, $F6, $73, $74, $B6, $E8, $DD, $7A, $72, $49, $2F, $55,
    $FC, $37, $28, $99, $0E, $40, $A1, $E7, $EF, $18, $8D, $14, $DC, $0E, $FD, $0C,
    $30, $8A, $02, $8D, $9A, $7B, $E2, $74, $0D, $8F, $D0, $C3, $5E, $3C, $2E, $8A,
    $42, $F5, $EB, $83, $C2, $4D, $D3, $FD, $3C, $42, $02, $75, $3E, $07, $02, $36,
    $03, $34, $4D, $D3, $34, $2E, $04, $26, $05, $1E, $B2, $35, $4D, $D3, $06, $16,
    $07, $0E, $3A, $08, $D0, $C6, $0D, $BE, $39, $04, $F6, $72, $BA, $27, $BA, $7A,
    $F4, $E9, $8D, $F3, $DA, $69, $83, $7E, $91, $B3, $81, $12, $B0, $D1, $DC, $FC,
    $70, $DB, $DE, $39, $20, $30, $6C, $D0, $7D, $82, $1D, $FF, $EE, $14, $20, $2F,
    $F5, $EF, $F4, $A6, $09, $10, $78, $10, $93, $4A, $AF, $7F, $99, $A4, $59, $86,
    $55, $42, $D4, $D4, $48, $B5, $19, $39, $C3, $D4, $81, $04, $5E, $E2, $86, $81,
    $ED, $73, $58, $80, $48, $77, $D8, $38, $8A, $52, $4F, $6D, $0E, $5A, $A4, $6C,
    $AF, $54, $BC, $86, $63, $86, $81, $C4, $21, $2C, $24, $DE, $B6, $63, $B8, $52,
    $27, $89, $CF, $9F, $34, $91, $40, $21, $DF, $D2, $2C, $F0, $DA, $07, $44, $1B,
    $93, $43, $7E, $98, $82, $76, $4B, $0C, $0C, $43, $17, $DC, $0B, $6D, $6D, $41,
    $83, $51, $A3, $59, $A5, $51, $F8, $FA, $43, $FC, $78, $89, $59, $64, $C7, $41,
    $28, $74, $5B, $3B, $91, $D7, $73, $81, $12, $40, $8B, $8F, $66, $AE, $A0, $7B,
    $E4, $65, $71, $C3, $81, $EE, $F3, $77, $3E, $5E, $89, $57, $B0, $59, $46, $B0,
    $71, $FE, $D0, $04, $67, $03, $23, $97, $00, $B8, $F2, $04, $41, $B3, $9A, $E6,
    $ED, $7D, $05, $77, $16, $83, $B9, $25, $20, $84, $22, $05, $F8, $06, $ED, $CE,
    $3C, $F0, $08, $20, $DF, $BB, $39, $57, $60, $3E, $88, $9D, $1B, $83, $D3, $65,
    $74, $A7, $EB, $01, $C1, $ED, $7A, $6B, $94, $1E, $43, $B2, $2B, $77, $64, $2E,
    $8B, $8B, $F1, $85, $9A, $3B, $4B, $17, $B1, $EC, $89, $F7, $8B, $83, $C1, $8D,
    $4E, $1F, $CE, $B3, $94, $18, $03, $4F, $C2, $6C, $01, $07, $80, $F5, $31, $25,
    $08, $3C, $F0, $0A, $70, $41, $F3, $0E, $D1, $98, $DB, $69, $60, $43, $C6, $4D,
    $E2, $F6, $83, $59, $02, $97, $40, $0E, $02, $F9, $60, $03, $BB, $47, $96, $43,
    $60, $8D, $1C, $FC, $19, $B9, $71, $78, $B8, $38, $76, $57, $FB, $4F, $39, $B9,
    $89, $59, $6C, $F6, $1C, $A8, $3A, $6B, $08, $6B, $40, $4A, $6A, $94, $DE, $0A,
    $8A, $1E, $8D, $46, $FE, $E7, $63, $C3, $5B, $B5, $BB, $DA, $D0, $47, $89, $03,
    $88, $E4, $77, $62, $E0, $F0, $60, $E0, $D2, $5A, $CC, $42, $78, $EA, $C5, $42,
    $FB, $D2, $B0, $B4, $97, $7A, $39, $53, $C2, $DC, $8A, $4C, $3A, $02, $0B, $0D,
    $0F, $07, $6B, $54, $EE, $C8, $21, $BB, $89, $E4, $FB, $0D, $DF, $DA, $1B, $56,
    $34, $21, $CB, $04, $5A, $4D, $BC, $8B, $52, $BC, $BC, $C2, $D6, $34, $6C, $63,
    $C0, $BF, $42, $59, $0D, $C0, $C2, $D5, $D4, $B6, $03, $3C, $41, $75, $FF, $72,
    $BD, $89, $6C, $8F, $EF, $0C, $99, $6D, $75, $EC, $C7, $40, $78, $4B, $06, $68,
    $DA, $04, $44, $98, $CE, $28, $42, $6C, $CB, $1F, $F8, $2A, $78, $FD, $F6, $89,
    $D1, $8B, $52, $D0, $3B, $0D, $34, $40, $F6, $D2, $5B, $41, $3E, $30, $52, $4D,
    $A0, $52, $5E, $14, $59, $0E, $20, $B0, $2C, $5F, $59, $19, $FE, $90, $02, $1C,
    $51, $A6, $2B, $14, $10, $FD, $59, $6E, $F0, $00, $EA, $01, $4D, $D4, $D4, $81,
    $2B, $80, $39, $40, $3F, $93, $C1, $11, $80, $98, $05, $08, $4E, $74, $65, $F4,
    $81, $9B, $08, $29, $0B, $FF, $38, $85, $19, $64, $83, $07, $8F, $06, $62, $B1,
    $15, $8C, $96, $68, $3D, $47, $73, $8C, $15, $11, $B6, $52, $9F, $77, $C7, $8F,
    $DE, $34, $F0, $57, $C1, $54, $32, $FF, $91, $58, $14, $0B, $81, $E2, $88, $3C,
    $3C, $40, $97, $94, $04, $47, $B2, $41, $97, $32, $58, $B0, $53, $5B, $7B, $06,
    $EE, $74, $CB, $D7, $57, $FF, $43, $76, $5E, $2E, $B4, $33, $6A, $F2, $1A, $F3,
    $01, $72, $5C, $0D, $82, $1C, $C0, $FF, $13, $03, $61, $8E, $E0, $A5, $90, $EC,
    $FF, $C1, $3B, $0A, $EF, $89, $68, $25, $C8, $A1, $65, $D5, $8B, $99, $EE, $6D,
    $23, $BF, $A0, $C6, $B9, $A4, $30, $EC, $81, $D0, $CC, $90, $93, $41, $9E, $5F,
    $03, $B1, $B1, $16, $8C, $19, $67, $91, $8B, $85, $79, $44, $3B, $E5, $A0, $15,
    $E6, $61, $ED, $08, $61, $B2, $25, $59, $C9, $19, $16, $2C, $CB, $B2, $6C, $C9,
    $41, $4B, $53, $71, $F2, $25, $A2, $E1, $D2, $D6, $75, $75, $99, $40, $AF, $A4,
    $80, $82, $FB, $89, $46, $6C, $4F, $89, $06, $BF, $DB, $DA, $2B, $21, $F0, $72,
    $FB, $FC, $A7, $41, $C3, $0F, $1F, $2A, $0B, $F7, $CC, $3D, $55, $D8, $42, $F2,
    $55, $60, $59, $28, $01, $01, $49, $18, $44, $F8, $5A, $C9, $8C, $03, $10, $1F,
    $5C, $BE, $8C, $3C, $06, $2F, $B7, $1F, $EB, $A2, $A7, $CF, $81, $E7, $87, $02,
    $A6, $81, $83, $F8, $DA, $66, $41, $4D, $00, $2F, $CE, $80, $2D, $43, $62, $1A,
    $F8, $C1, $07, $FB, $14, $8E, $01, $F7, $60, $89, $73, $23, $70, $D9, $0E, $89,
    $8B, $7A, $63, $F7, $C4, $17, $1E, $AF, $09, $40, $5A, $95, $50, $47, $54, $7B,
    $00, $5D, $8B, $B8, $91, $0E, $B9, $E4, $EE, $FB, $CE, $54, $7A, $10, $57, $7B,
    $43, $64, $19, $B9, $59, $F7, $F0, $89, $7B, $43, $42, $42, $96, $E5, $64, $F0,
    $42, $C0, $92, $1B, $09, $83, $5F, $51, $FA, $08, $EB, $86, $78, $56, $3D, $03,
    $03, $6F, $41, $F8, $23, $60, $66, $61, $64, $0B, $7C, $1A, $4B, $31, $07, $E0,
    $42, $18, $78, $41, $2D, $69, $46, $38, $48, $BC, $D4, $DA, $FE, $18, $48, $E8,
    $01, $8D, $B1, $01, $CD, $0A, $8F, $F3, $03, $74, $3F, $12, $89, $44, $BA, $00,
    $D8, $51, $02, $71, $70, $BF, $10, $E7, $22, $C6, $07, $F3, $E7, $FA, $F9, $0B,
    $27, $22, $F6, $99, $BE, $21, $44, $3F, $69, $18, $8E, $E5, $97, $97, $78, $24,
    $43, $33, $B9, $83, $C1, $C3, $02, $41, $8A, $43, $34, $4D, $D3, $95, $01, $07,
    $02, $36, $03, $2E, $D3, $34, $4D, $D3, $04, $26, $05, $1E, $06, $BC, $B2, $35,
    $4D, $16, $07, $0E, $3A, $08, $29, $44, $46, $36, $4B, $33, $F1, $CE, $B9, $F1,
    $9D, $8A, $DD, $50, $6B, $8E, $5B, $FF, $86, $A1, $11, $6C, $97, $72, $D6, $64,
    $39, $C8, $9A, $47, $72, $40, $36, $D1, $AE, $C8, $3B, $FD, $3F, $DE, $FD, $34,
    $0C, $04, $A2, $E4, $D7, $42, $2B, $E1, $C5, $90, $4B, $6F, $D7, $48, $4C, $18,
    $12, $3B, $82, $3E, $C6, $5C, $17, $C2, $10, $7E, $FB, $31, $FC, $1F, $A7, $9E,
    $54, $C5, $0C, $FF, $B8, $58, $C8, $58, $99, $AC, $AA, $E5, $1F, $55, $0F, $F5,
    $01, $C4, $1B, $C6, $BF, $5D, $80, $5B, $A8, $A0, $DC, $D3, $30, $2E, $68, $C0,
    $90, $45, $BF, $10, $B9, $53, $B1, $51, $2A, $45, $CD, $08, $F1, $C4, $C8, $A8,
    $68, $34, $3C, $5F, $07, $02, $9E, $14, $FF, $33, $CC, $D4, $C5, $6E, $61, $55,
    $7F, $5A, $8D, $82, $6E, $AE, $D8, $7C, $63, $8A, $89, $82, $5B, $11, $7C, $0A,
    $1B, $8A, $5D, $F1, $0B, $5B, $C7, $82, $C8, $B8, $50, $9B, $69, $C6, $76, $A7,
    $8A, $24, $0F, $2C, $A4, $09, $38, $5E, $45, $C9, $34, $90, $ED, $F4, $9A, $33,
    $03, $F4, $1C, $0B, $09, $B4, $08, $FC, $E0, $6D, $B1, $9F, $1E, $84, $62, $5D,
    $3D, $1D, $FC, $62, $D4, $36, $5D, $7E, $EE, $13, $9F, $18, $88, $09, $7B, $B3,
    $97, $88, $4B, $1D, $7E, $F0, $2A, $7B, $11, $69, $96, $5D, $19, $12, $59, $94,
    $04, $01, $AC, $64, $92, $C9, $26, $A8, $09, $B0, $A0, $6A, $25, $04, $11, $6F,
    $BE, $68, $15, $A1, $9F, $7C, $CF, $0A, $0A, $5D, $15, $28, $31, $22, $46, $B4,
    $16, $4D, $10, $0D, $77, $BB, $0E, $B9, $9B, $FD, $50, $C4, $B8, $A8, $83, $50,
    $14, $40, $09, $0B, $00, $86, $66, $54, $3D, $78, $E8, $33, $40, $81, $DE, $0A,
    $B4, $92, $83, $3C, $97, $24, $F7, $B7, $77, $A2, $E9, $A5, $25, $42, $B8, $46,
    $C6, $84, $1A, $58, $30, $D8, $DF, $F0, $D9, $89, $11, $94, $B3, $5C, $0A, $42,
    $3B, $55, $BC, $7C, $CD, $8B, $4A, $57, $2F, $6E, $80, $F6, $8E, $51, $06, $12,
    $DA, $05, $0C, $B6, $50, $B8, $B3, $04, $3C, $F0, $D9, $E4, $2E, $4F, $A0, $06,
    $85, $78, $78, $AC, $8E, $E3, $51, $F5, $9D, $94, $AF, $BD, $12, $89, $54, $E9,
    $ED, $FD, $22, $8B, $94, $BB, $53, $6C, $B0, $89, $FA, $B1, $F6, $BC, $B9, $83,
    $5F, $BC, $93, $13, $06, $8C, $93, $60, $45, $C2, $FF, $2D, $C0, $EF, $BD, $3C,
    $30, $4D, $AC, $31, $B8, $AB, $39, $04, $8F, $0F, $82, $E6, $DE, $B3, $B9, $85,
    $AA, $B0, $5B, $8D, $1F, $28, $1F, $3A, $DB, $AD, $FC, $3C, $88, $8B, $4D, $B0,
    $06, $72, $4C, $1B, $76, $FD, $8D, $CD, $A2, $3D, $41, $95, $0B, $89, $8C, $83,
    $DD, $6D, $89, $FA, $5E, $01, $D2, $74, $27, $03, $7F, $90, $68, $5C, $C0, $62,
    $B3, $20, $48, $05, $4B, $DB, $36, $F2, $7D, $73, $B4, $8B, $95, $B2, $8B, $4A,
    $28, $B0, $49, $78, $BC, $87, $8D, $3D, $84, $BC, $8B, $E2, $8F, $22, $24, $84,
    $B1, $ED, $4D, $F8, $4D, $B4, $8D, $56, $08, $BB, $BE, $B4, $34, $97, $6D, $C1,
    $68, $B9, $DD, $CA, $74, $88, $93, $AB, $5C, $E1, $08, $2A, $75, $A8, $5E, $29,
    $9C, $3B, $76, $81, $66, $A4, $EB, $89, $84, $8B, $39, $94, $8B, $5D, $FE, $22,
    $54, $1D, $75, $C0, $B9, $F8, $8B, $34, $96, $B4, $33, $B2, $DC, $FC, $34, $87,
    $BB, $9B, $16, $76, $F7, $75, $50, $50, $75, $FC, $97, $AD, $86, $72, $3F, $18,
    $1E, $DE, $DA, $C8, $A9, $A4, $88, $52, $C9, $39, $8D, $68, $89, $65, $B3, $67,
    $4F, $7C, $22, $0E, $7F, $9A, $5E, $3E, $DA, $A3, $AB, $7A, $73, $C1, $5F, $BB,
    $1E, $15, $E1, $3B, $E1, $D6, $18, $CA, $89, $B4, $1E, $B5, $B9, $8B, $93, $C5,
    $4A, $F7, $16, $12, $BB, $58, $0B, $CF, $EF, $28, $03, $DE, $C6, $82, $5C, $A1,
    $04, $45, $03, $04, $B7, $D4, $B6, $67, $7C, $7C, $8F, $8A, $8C, $1E, $3D, $8A,
    $44, $38, $C1, $B5, $84, $BD, $BB, $56, $60, $E8, $54, $88, $84, $19, $14, $C2,
    $DB, $C1, $FE, $31, $89, $4C, $97, $02, $5C, $55, $B4, $0D, $54, $B7, $02, $09,
    $0F, $C6, $48, $C0, $9C, $07, $B4, $4D, $45, $A0, $40, $87, $C3, $02, $6D, $33,
    $A0, $1B, $89, $19, $4C, $CE, $CE, $68, $7C, $14, $B4, $30, $4F, $84, $C9, $BC,
    $94, $8C, $86, $13, $36, $55, $2F, $40, $60, $B9, $5C, $30, $34, $81, $82, $D9,
    $35, $14, $B8, $C8, $04, $E7, $B6, $77, $F2, $A8, $04, $95, $25, $1B, $A0, $22,
    $F9, $56, $68, $67, $7B, $14, $87, $72, $46, $1B, $C0, $7E, $E9, $41, $3B, $8E,
    $F1, $36, $4D, $87, $C9, $89, $BC, $2E, $39, $CE, $2C, $CE, $7F, $CD, $DE, $EC,
    $B0, $93, $6C, $7D, $65, $3F, $48, $32, $50, $B4, $87, $45, $73, $BA, $AA, $93,
    $9A, $1B, $13, $9A, $60, $89, $62, $17, $EE, $FD, $B3, $CC, $9A, $BE, $11, $52,
    $83, $A3, $4E, $2E, $89, $0F, $03, $3A, $E1, $16, $09, $84, $B3, $E7, $C4, $9B,
    $F0, $65, $69, $53, $BA, $45, $94, $B8, $08, $98, $C7, $85, $7C, $C4, $AD, $20,
    $00, $34, $FC, $48, $04, $B6, $50, $D4, $40, $32, $70, $7A, $90, $1A, $AD, $8D,
    $16, $C4, $8C, $E9, $36, $84, $47, $19, $E4, $AE, $49, $2F, $4D, $43, $3C, $0B,
    $0F, $4E, $86, $88, $59, $69, $46, $B0, $1D, $4D, $30, $FC, $1F, $44, $91, $31,
    $D3, $E4, $80, $08, $D8, $C0, $EE, $81, $FF, $3C, $0F, $7E, $1B, $48, $05, $CF,
    $DB, $6B, $AD, $6D, $5F, $80, $42, $57, $15, $18, $16, $AD, $C5, $4F, $B6, $36,
    $12, $55, $80, $16, $40, $8C, $8B, $7C, $23, $D0, $EF, $C5, $8A, $02, $0F, $0A,
    $44, $BA, $D1, $8D, $68, $8B, $B5, $ED, $70, $01, $3B, $83, $7E, $9A, $A6, $2F,
    $84, $F4, $5F, $78, $D6, $F8, $09, $7D, $98, $C4, $74, $8F, $02, $3B, $4D, $94,
    $7F, $A5, $C5, $5B, $EB, $35, $04, $73, $9A, $8C, $0E, $88, $7C, $0D, $5B, $D8,
    $40, $13, $03, $C8, $E2, $7D, $8C, $2C, $87, $28, $6D, $B6, $F5, $C6, $2D, $8F,
    $89, $C7, $27, $16, $5C, $B8, $AD, $A8, $05, $5E, $F3, $E5, $C7, $25, $7B, $F0,
    $A8, $7B, $0A, $85, $90, $FC, $DC, $0A, $62, $66, $AB, $9D, $7B, $8E, $02, $31,
    $AC, $81, $D6, $7D, $73, $85, $32, $B9, $2D, $F8, $42, $01, $F9, $BE, $89, $18,
    $51, $23, $3D, $2C, $C4, $8E, $3B, $AF, $7C, $BE, $F0, $27, $9A, $F3, $22, $4D,
    $84, $8D, $51, $D3, $D1, $EB, $01, $49, $9C, $75, $F4, $DD, $49, $84, $4B, $96,
    $66, $AD, $F2, $25, $48, $9A, $A3, $D9, $0A, $00, $13, $3E, $02, $D5, $C0, $6F,
    $C1, $F0, $CE, $1F, $89, $1A, $E2, $F3, $B5, $08, $75, $ED, $76, $6E, $CD, $8C,
    $43, $93, $7F, $B8, $B6, $3B, $78, $28, $46, $30, $D6, $48, $7B, $28, $C6, $1B,
    $2B, $B4, $3D, $23, $CF, $74, $5C, $6C, $80, $5F, $03, $C3, $43, $B0, $5C, $A0,
    $5D, $0E, $E6, $52, $12, $10, $93, $06, $40, $8A, $A4, $85, $0F, $ED, $D6, $30,
    $F4, $29, $C2, $5E, $07, $88, $66, $3E, $0E, $5B, $8B, $99, $4A, $1A, $D4, $DA,
    $83, $00, $0D, $FD, $17, $DB, $E2, $89, $07, $23, $89, $7C, $88, $02, $4E, $FD,
    $A4, $74, $08, $46, $F8, $4F, $75, $88, $8D, $8B, $55, $C0, $08, $B6, $DE, $D4,
    $B8, $6F, $44, $51, $6C, $48, $44, $55, $C8, $42, $63, $0F, $80, $AE, $88, $0E,
    $78, $F6, $D2, $25, $05, $A8, $23, $CD, $9F, $96, $5C, $B7, $BF, $A8, $B9, $60,
    $56, $83, $26, $E6, $69, $58, $7C, $E7, $A8, $24, $7E, $E2, $81, $C4, $BC, $09,
    $8D, $C8, $31, $81, $F2, $C7, $44, $E3, $1D, $2C, $95, $11, $D8, $B6, $8B, $7A,
    $8B, $C5, $1F, $66, $76, $71, $8A, $B7, $F7, $87, $88, $5F, $BD, $16, $2B, $0C,
    $42, $04, $E2, $C0, $2B, $60, $8E, $6C, $F2, $36, $DD, $33, $70, $1B, $F1, $5B,
    $25, $49, $41, $01, $62, $F8, $52, $98, $9A, $BD, $76, $08, $07, $62, $FF, $5F,
    $6D, $C7, $D9, $A0, $05, $ED, $93, $18, $CE, $AC, $D4, $61, $04, $18, $B3, $F5,
    $05, $0C, $3C, $9B, $C1, $5B, $E5, $54, $81, $02, $98, $E9, $29, $D6, $B0, $79,
    $7B, $1D, $A0, $89, $22, $B1, $8F, $59, $F9, $50, $01, $17, $3A, $74, $A0, $34,
    $77, $0D, $02, $7F, $A2, $83, $5D, $6B, $6C, $81, $B8, $B7, $03, $06, $EB, $96,
    $C0, $14, $F6, $5A, $95, $67, $0B, $73, $8E, $FC, $CE, $CE, $93, $61, $4A, $5A,
    $93, $A8, $FE, $F8, $19, $59, $F8, $4A, $BB, $EB, $AF, $05, $44, $1A, $41, $A0,
    $39, $39, $1F, $4B, $41, $7D, $C9, $64, $06, $76, $E1, $02, $19, $66, $23, $1E,
    $57, $F6, $FA, $CC, $40, $1B, $2C, $5B, $42, $3E, $F8, $BD, $46, $9E, $FB, $BC,
    $89, $AA, $2C, $03, $10, $8F, $EC, $59, $43, $EF, $D8, $7D, $A0, $36, $23, $FB,
    $68, $38, $BE, $67, $28, $6C, $40, $5B, $8D, $8A, $30, $09, $10, $37, $02, $C9,
    $C1, $40, $9F, $F1, $DF, $54, $8F, $40, $3F, $CF, $4A, $D1, $E9, $83, $E7, $01,
    $09, $DF, $1A, $50, $FA, $46, $B0, $1C, $3F, $7F, $EF, $32, $BF, $01, $70, $77,
    $2B, $90, $B1, $46, $37, $06, $15, $55, $01, $93, $A8, $58, $98, $C4, $03, $51,
    $21, $A2, $D1, $72, $23, $81, $C0, $00, $D4, $C6, $83, $D6, $01, $F0, $8E, $31,
    $C7, $BA, $04, $28, $9A, $11, $F5, $5F, $23, $04, $B1, $8A, $B2, $03, $C9, $6A,
    $A1, $41, $B9, $8A, $EB, $36, $BD, $08, $57, $4D, $00, $DF, $E6, $D5, $AE, $14,
    $A6, $E5, $D5, $93, $BB, $06, $5D, $8B, $10, $6C, $9D, $47, $D7, $08, $3D, $39,
    $D1, $00, $5A, $F5, $4D, $E0, $7D, $09, $BF, $C2, $1F, $FA, $A8, $7F, $AF, $55,
    $E0, $0F, $8D, $E5, $8E, $85, $EB, $77, $8A, $43, $BF, $12, $BF, $36, $9C, $86,
    $50, $7B, $23, $52, $94, $90, $5F, $EB, $D3, $E2, $66, $21, $16, $34, $A0, $66,
    $B8, $09, $BB, $85, $22, $0A, $C4, $B1, $0B, $88, $0C, $A6, $2E, $42, $E1, $DE,
    $35, $8A, $86, $B9, $0D, $5A, $1D, $E2, $7C, $97, $8A, $88, $87, $B9, $10, $53,
    $86, $3F, $9B, $FB, $A5, $5E, $29, $C1, $42, $D3, $FB, $1C, $37, $9E, $8D, $4C,
    $38, $D9, $C1, $2B, $D0, $F0, $FF, $EE, $72, $41, $C6, $01, $1F, $ED, $36, $C0,
    $AB, $13, $00, $7A, $BE, $7E, $47, $DF, $C5, $60, $FB, $59, $C4, $29, $FA, $39,
    $D1, $DA, $68, $CE, $2B, $F6, $B3, $F3, $97, $96, $94, $D3, $E3, $66, $09, $4E,
    $B0, $FB, $AA, $2F, $77, $EB, $B1, $3F, $99, $C1, $24, $CC, $E3, $B3, $79, $BA,
    $5D, $DE, $20, $05, $56, $5D, $68, $94, $9E, $28, $7A, $A5, $8A, $93, $98, $B5,
    $3C, $D8, $DB, $0A, $96, $7B, $63, $F8, $39, $C2, $16, $AD, $EC, $BD, $33, $D4,
    $FB, $88, $D1, $01, $FA, $FF, $67, $35, $18, $9E, $CD, $A3, $B2, $96, $4F, $49,
    $0E, $BE, $C1, $CE, $36, $D6, $22, $BB, $4E, $CF, $57, $D8, $4F, $73, $33, $DC,
    $A8, $FB, $81, $8E, $83, $AD, $24, $9E, $BC, $BE, $2B, $98, $35, $53, $49, $31,
    $D8, $89, $5D, $C8, $D3, $9A, $70, $61, $E6, $3D, $51, $3B, $09, $C3, $14, $97,
    $BF, $8A, $05, $56, $11, $D7, $08, $31, $DB, $8A, $9E, $A1, $70, $88, $0E, $52,
    $99, $4C, $87, $35, $C1, $2D, $80, $52, $40, $0D, $51, $0B, $45, $B4, $42, $86,
    $0E, $F2, $D3, $19, $59, $D8, $A0, $7D, $FB, $C8, $54, $96, $54, $F9, $6D, $5B,
    $6F, $BF, $84, $B3, $0E, $7D, $EC, $CD, $5D, $E0, $E6, $52, $36, $7B, $83, $EB,
    $03, $B4, $79, $6A, $7C, $41, $48, $56, $48, $C6, $7E, $CC, $37, $3B, $18, $A0,
    $59, $2F, $C9, $72, $67, $EE, $9F, $04, $6A, $29, $C7, $1B, $F9, $43, $83, $E8,
    $0E, $D3, $FA, $22, $70, $1C, $DE, $60, $7D, $89, $25, $6E, $7E, $C2, $75, $6F,
    $C1, $B3, $6E, $CC, $B9, $8A, $07, $BA, $8A, $DE, $F8, $47, $2A, $B4, $7D, $E4,
    $B1, $06, $39, $FB, $74, $BC, $07, $B2, $04, $07, $A3, $15, $DD, $2A, $EC, $43,
    $EE, $BC, $8E, $46, $10, $A1, $DA, $41, $FA, $C4, $2C, $AA, $D0, $7C, $B8, $B0,
    $5D, $0A, $8F, $C3, $00, $3F, $96, $C2, $B8, $27, $9C, $86, $94, $B3, $C8, $D4,
    $BA, $68, $3B, $83, $C8, $87, $02, $6A, $86, $C0, $CC, $FA, $E9, $72, $19, $84,
    $B3, $09, $86, $13, $0D, $BF, $02, $A6, $81, $4C, $15, $0D, $DE, $97, $1C, $21,
    $DD, $0B, $12, $C2, $C6, $D0, $0E, $5C, $0E, $92, $91, $01, $C4, $09, $65, $01,
    $C8, $10, $21, $93, $0B, $09, $05, $7B, $04, $03, $8C, $45, $63, $B5, $86, $00,
    $66, $DE, $43, $C8, $87, $0C, $C8, $46, $33, $DC, $7C, $C6, $08, $0A, $EA, $2C,
    $DB, $02, $2B, $5E, $72, $4E, $96, $20, $47, $7E, $14, $CB, $C6, $54, $0B, $55,
    $DC, $1B, $8F, $0D, $20, $A9, $DC, $B6, $CA, $DC, $4D, $BF, $59, $3B, $C7, $43,
    $75, $09, $F6, $3B, $C4, $C2, $46, $32, $21, $14, $74, $D8, $CC, $B5, $59, $72,
    $7A, $6E, $77, $74, $C2, $C9, $85, $1C, $D0, $D0, $D0, $57, $42, $30, $C3, $2C,
    $79, $5D, $4B, $8B, $C5, $87, $0F, $82, $50, $7D, $64, $EF, $0B, $2D, $61, $E1,
    $56, $3C, $D3, $E7, $86, $BE, $0C, $85, $1C, $01, $B2, $96, $C0, $D4, $93, $4C,
    $C8, $C9, $D4, $D4, $0D, $41, $B6, $90, $65, $92, $FD, $03, $03, $18, $60, $B4,
    $91, $5D, $02, $AF, $5F, $54, $E0, $9B, $B1, $4B, $1D, $0A, $98, $8F, $0A, $03,
    $13, $50, $21, $81, $CF, $2F, $4B, $07, $66, $44, $10, $11, $A8, $43, $F9, $F9,
    $D8, $C9, $34, $91, $47, $F8, $F8, $8B, $5E, $08, $35, $19, $9C, $C1, $18, $C1,
    $15, $1A, $E4, $40, $BE, $47, $03, $F5, $41, $19, $5D, $33, $0D, $E4, $F0, $32,
    $F8, $F4, $F0, $66, $7D, $3B, $DB, $D3, $FF, $3E, $BE, $4E, $1D, $F2, $9F, $41,
    $50, $12, $D8, $A0, $0E, $06, $66, $35, $BB, $5B, $8F, $C1, $AC, $C1, $89, $8C,
    $17, $8E, $20, $02, $10, $78, $7B, $7E, $2F, $15, $F0, $D0, $29, $55, $DE, $25,
    $66, $C0, $05, $AC, $C4, $1A, $98, $76, $FE, $65, $B0, $CA, $B1, $AC, $C3, $42,
    $88, $D9, $8D, $44, $AD, $9A, $2D, $14, $8A, $82, $EC, $85, $2C, $5A, $70, $B3,
    $60, $3F, $7C, $34, $0C, $10, $B5, $B0, $C5, $80, $01, $08, $07, $9A, $18, $2A,
    $E2, $C2, $FB, $59, $86, $B4, $97, $B2, $C0, $0B, $84, $1D, $9C, $EE, $DB, $8F,
    $55, $01, $E2, $7E, $10, $3F, $39, $C4, $CE, $9D, $66, $34, $BB, $8D, $C5, $7E,
    $08, $2C, $68, $B2, $80, $52, $9D, $38, $95, $14, $2C, $68, $B6, $A3, $AD, $A8,
    $CD, $60, $9E, $AE, $7D, $E8, $79, $07, $42, $08, $46, $09, $57, $01, $1B, $56,
    $1F, $C6, $A4, $D9, $66, $48, $A6, $DC, $AF, $A1, $46, $13, $88, $B9, $28, $3B,
    $B3, $97, $13, $47, $FE, $58, $70, $C7, $39, $F9, $0F, $8E, $BB, $7D, $DE, $E3,
    $C1, $1C, $1C, $C2, $E7, $35, $FB, $EB, $93, $43, $9E, $67, $92, $D9, $10, $56,
    $59, $40, $6D, $8F, $59, $42, $DB, $52, $43, $AD, $14, $A1, $33, $ED, $09, $9E,
    $96, $0B, $11, $15, $66, $0D, $0B, $3B, $13, $D1, $E0, $8B, $D7, $1A, $0A, $41,
    $98, $BA, $DD, $1A, $37, $6D, $44, $8A, $7E, $0A, $D3, $07, $7E, $35, $79, $64,
    $A2, $C6, $72, $A2, $AC, $DB, $07, $45, $27, $3E, $53, $7A, $2F, $47, $01, $AC,
    $60, $AE, $CA, $DD, $CF, $E3, $F1, $36, $38, $14, $E1, $C7, $68, $34, $30, $18,
    $21, $35, $62, $44, $90, $6B, $12, $DC, $7C, $CC, $38, $0F, $5C, $CF, $4F, $F0,
    $8D, $04, $39, $C6, $4A, $E2, $75, $4A, $DE, $94, $D8, $DF, $87, $EA, $74, $0B,
    $9A, $85, $2D, $FB, $2C, $10, $F2, $81, $09, $08, $42, $0C, $3A, $E7, $5E, $A3,
    $B7, $98, $31, $C9, $47, $D3, $BD, $00, $A7, $09, $8E, $45, $36, $0A, $BC, $81,
    $10, $14, $4F, $77, $D6, $56, $16, $4D, $3E, $71, $43, $51, $5E, $A4, $59, $BA,
    $E3, $E9, $5E, $86, $19, $1B, $36, $06, $CC, $3E, $8F, $BB, $DC, $AD, $46, $33,
    $F1, $0D, $1B, $96, $4B, $52, $EF, $D2, $3D, $37, $81, $B7, $DB, $D7, $D1, $E2,
    $3A, $CD, $C8, $C8, $96, $86, $F8, $93, $67, $45, $93, $70, $8F, $67, $D2, $B8,
    $5E, $E9, $45, $1D, $02, $A1, $79, $0F, $9F, $98, $96, $4D, $36, $CE, $40, $CC,
    $AF, $A8, $23, $6B, $25, $74, $2F, $58, $36, $46, $A6, $58, $F5, $80, $51, $C2,
    $E4, $37, $0C, $9D, $00, $3A, $45, $9C, $B1, $09, $8E, $1A, $74, $B2, $68, $13,
    $EC, $1A, $97, $13, $88, $82, $D0, $9F, $02, $33, $BF, $44, $C9, $11, $89, $DC,
    $21, $C2, $41, $18, $DC, $FA, $DC, $0F, $8E, $86, $03, $04, $9F, $B7, $89, $C7,
    $77, $C2, $E0, $35, $63, $A6, $77, $72, $C2, $84, $EA, $C1, $E5, $40, $A2, $B5,
    $21, $9D, $0C, $5A, $0A, $36, $0C, $96, $CD, $C0, $12, $AC, $43, $63, $08, $89,
    $8B, $E3, $AD, $0E, $B5, $51, $3C, $63, $54, $18, $F0, $59, $5C, $EA, $BD, $64,
    $BE, $8B, $BE, $BF, $26, $9C, $8B, $D8, $74, $E8, $CD, $BB, $0F, $86, $E8, $45,
    $41, $BE, $A4, $62, $C6, $B1, $46, $53, $98, $9E, $1C, $4F, $3E, $10, $4D, $BD,
    $E1, $BD, $B2, $8A, $1C, $01, $6D, $41, $03, $7C, $64, $2A, $77, $69, $81, $1E,
    $E0, $05, $C0, $54, $DD, $E8, $19, $99, $77, $7D, $6C, $12, $EF, $6A, $1B, $89,
    $84, $99, $06, $04, $F2, $F2, $EE, $3C, $3F, $B9, $D4, $7D, $D4, $29, $F8, $23,
    $28, $AE, $BD, $94, $99, $04, $F6, $F3, $F0, $E1, $CF, $34, $0C, $0E, $A2, $5D,
    $93, $DA, $50, $1D, $AA, $64, $F6, $F5, $22, $7E, $9D, $F4, $E7, $9E, $F2, $D0,
    $CE, $D8, $42, $C0, $81, $73, $D0, $B0, $ED, $1B, $72, $4C, $A5, $52, $49, $67,
    $84, $A5, $BE, $34, $44, $D0, $DE, $CE, $85, $10, $A2, $A7, $16, $92, $F5, $CF,
    $D6, $9A, $5C, $A7, $CC, $2B, $3C, $85, $90, $A1, $77, $79, $AC, $42, $A9, $35,
    $D8, $39, $C1, $AF, $02, $8A, $CE, $58, $77, $B6, $99, $96, $91, $C2, $8E, $96,
    $B6, $18, $AF, $06, $7E, $CA, $43, $BF, $6E, $CA, $0B, $14, $E4, $04, $6A, $56,
    $42, $42, $87, $16, $04, $7E, $80, $4D, $C4, $6E, $45, $B1, $ED, $41, $3A, $E1,
    $7D, $CC, $42, $0C, $56, $F6, $29, $D7, $89, $13, $3D, $D9, $6E, $04, $01, $D3,
    $F8, $A6, $86, $1A, $F0, $97, $82, $3A, $9A, $53, $31, $4B, $56, $81, $FB, $EA,
    $C7, $05, $70, $2F, $7E, $C8, $01, $CA, $8A, $93, $C8, $C6, $B6, $B9, $B5, $5D,
    $A0, $1D, $A0, $58, $08, $23, $CE, $80, $30, $BC, $65, $C0, $C7, $B0, $11, $AF,
    $B7, $29, $D0, $C1, $7B, $5C, $9F, $2C, $A1, $1D, $9A, $5D, $92, $7A, $62, $FC,
    $84, $11, $58, $C9, $5F, $55, $BC, $29, $DA, $5C, $6E, $DB, $0E, $CD, $07, $40,
    $B2, $BC, $1B, $EC, $72, $79, $82, $01, $DA, $5F, $54, $0B, $7D, $A0, $96, $E5,
    $79, $F2, $BD, $10, $A1, $E2, $00, $A0, $A4, $B8, $C2, $E5, $92, $43, $A0, $C2,
    $5F, $01, $FB, $39, $42, $98, $31, $F8, $8F, $67, $B6, $9C, $1C, $C9, $82, $61,
    $B6, $B0, $B0, $68, $03, $C8, $85, $B8, $B8, $9C, $E7, $63, $B2, $50, $55, $65,
    $B8, $F9, $9E, $2C, $78, $FA, $78, $75, $D7, $9C, $A7, $E0, $EF, $35, $35, $57,
    $30, $A9, $94, $8D, $BB, $D9, $6C, $64, $B1, $0B, $DC, $46, $33, $30, $98, $75,
    $59, $AC, $AF, $10, $D7, $FF, $D3, $D4, $41, $60, $9B, $96, $C0, $39, $90, $19,
    $1E, $C8, $AE, $CD, $14, $39, $16, $28, $12, $6E, $33, $D5, $2F, $30, $C1, $EF,
    $07, $8A, $9F, $1F, $80, $A0, $4F, $4F, $0E, $32, $C8, $15, $93, $8E, $15, $75,
    $7D, $E0, $60, $3A, $1D, $2B, $8F, $9C, $B8, $2A, $3F, $B2, $2B, $30, $24, $CE,
    $8D, $62, $3F, $14, $C8, $00, $DF, $C9, $60, $14, $01, $D9, $44, $6F, $34, $E1,
    $4B, $B6, $E0, $DA, $13, $7D, $D1, $17, $46, $BA, $45, $C2, $A9, $31, $57, $81,
    $02, $AA, $A8, $16, $7E, $61, $40, $AC, $02, $AC, $29, $DF, $39, $FA, $A1, $92,
    $C9, $C6, $49, $C7, $22, $99, $00, $A4, $DF, $3D, $08, $10, $E0, $31, $8C, $4F,
    $44, $D5, $31, $06, $83, $96, $D9, $3E, $9E, $44, $E7, $23, $CB, $91, $41, $5C,
    $11, $96, $5D, $A8, $29, $D3, $20, $C8, $20, $CD, $5D, $A8, $A8, $AC, $9D, $EE,
    $DC, $6A, $2B, $8D, $C6, $F0, $89, $99, $97, $16, $38, $30, $48, $AB, $96, $90,
    $8C, $58, $B6, $ED, $10, $23, $6B, $A1, $2C, $AC, $99, $83, $1B, $24, $21, $76,
    $B5, $A2, $1E, $3A, $84, $40, $2A, $73, $47, $43, $CF, $08, $45, $78, $AA, $C5,
    $14, $10, $FD, $C1, $A3, $83, $0A, $C4, $89, $F2, $9A, $C5, $42, $61, $0E, $34,
    $74, $D0, $B9, $84, $16, $FD, $D3, $FF, $C7, $6A, $71, $58, $9C, $E9, $F9, $E2,
    $0C, $D3, $96, $69, $42, $1F, $40, $8E, $EC, $2B, $7E, $F0, $62, $D9, $0C, $12,
    $B8, $62, $F4, $4C, $D5, $E9, $8A, $19, $71, $2E, $A4, $A3, $66, $F0, $1A, $10,
    $C5, $5C, $86, $AA, $65, $31, $39, $86, $D2, $96, $84, $A3, $5A, $07, $66, $29,
    $D1, $86, $22, $57, $C6, $16, $8A, $12, $39, $64, $CE, $B5, $65, $15, $12, $18,
    $12, $2F, $E0, $F7, $6B, $2E, $72, $6C, $79, $EB, $12, $38, $07, $47, $54, $73,
    $9B, $29, $08, $92, $42, $E3, $05, $57, $62, $C2, $0B, $FC, $FF, $75, $E8, $5B,
    $BF, $D8, $CD, $22, $1D, $33, $E9, $A7, $0B, $F0, $81, $26, $D4, $18, $2C, $14,
    $D5, $67, $3B, $96, $D1, $DC, $98, $71, $3F, $A3, $19, $91, $88, $03, $59, $C4,
    $8D, $76, $BF, $9F, $84, $80, $47, $70, $84, $A9, $8E, $C5, $79, $00, $53, $ED,
    $9E, $94, $0B, $0A, $8D, $97, $99, $36, $55, $6B, $1F, $C0, $FF, $DC, $06, $0E,
    $8C, $8D, $B7, $F8, $B6, $98, $A9, $57, $12, $38, $1C, $1F, $75, $83, $C0, $84,
    $AB, $37, $B3, $4A, $74, $44, $29, $EA, $C9, $5C, $76, $07, $6E, $8C, $88, $83,
    $BE, $04, $96, $0B, $14, $2B, $3E, $C1, $8F, $8A, $FD, $BE, $03, $E8, $55, $EC,
    $DE, $07, $D0, $6D, $77, $9A, $06, $32, $DB, $3B, $0E, $7F, $5E, $46, $71, $DB,
    $40, $09, $09, $81, $98, $06, $BC, $45, $BA, $01, $35, $22, $56, $A8, $6C, $F4,
    $07, $8D, $77, $44, $3B, $39, $F1, $0F, $5F, $E0, $01, $8C, $97, $C1, $A3, $D6,
    $17, $91, $C9, $AF, $90, $39, $D3, $9D, $74, $20, $BE, $16, $0F, $06, $2F, $AA,
    $ED, $BA, $F3, $6B, $07, $7C, $43, $5D, $7E, $A2, $B5, $95, $68, $B1, $9B, $F4,
    $8B, $8F, $28, $B0, $CC, $C0, $1C, $20, $07, $B2, $B0, $D4, $D0, $D0, $C9, $80,
    $2C, $97, $D8, $DC, $D8, $97, $0C, $32, $C9, $DC, $B8, $D0, $31, $B9, $B0, $37,
    $24, $D4, $50, $B0, $D0, $0B, $65, $40, $2E, $D0, $D8, $7B, $30, $81, $63, $F0,
    $25, $B9, $12, $00, $AD, $91, $70, $EF, $DF, $71, $47, $BC, $66, $83, $BC, $97,
    $8C, $00, $75, $06, $22, $02, $7F, $E7, $8D, $34, $BF, $D4, $3E, $E8, $49, $8B,
    $87, $A0, $C6, $D7, $F0, $5E, $11, $8D, $4E, $1B, $F0, $7F, $7E, $FC, $C1, $E9,
    $03, $89, $9F, $A8, $25, $9F, $AC, $C3, $0A, $C1, $EB, $03, $39, $CB, $77, $BD,
    $01, $74, $45, $80, $55, $27, $0C, $83, $C2, $10, $A2, $17, $B1, $A7, $CA, $4E,
    $A2, $1D, $E0, $74, $1C, $CF, $02, $57, $8F, $34, $60, $E3, $11, $23, $B5, $32,
    $D9, $A9, $77, $CB, $C8, $52, $9B, $AA, $57, $08, $E1, $8F, $C1, $8F, $2C, $2F,
    $CB, $27, $16, $8B, $77, $57, $C9, $8D, $46, $8F, $AA, $56, $6E, $8C, $47, $DF,
    $4C, $16, $06, $D4, $DE, $89, $16, $38, $40, $8D, $13, $A5, $B9, $D8, $DF, $8C,
    $B5, $C3, $88, $D9, $D3, $FE, $89, $F3, $89, $10, $8D, $B3, $34, $47, $9A, $19,
    $B7, $A6, $02, $47, $4F, $21, $CB, $72, $49, $9F, $4F, $5F, $87, $D2, $D7, $CC,
    $C9, $57, $5F, $03, $1C, $57, $B7, $32, $C9, $20, $CD, $45, $87, $87, $87, $32,
    $20, $CD, $4E, $04, $11, $28, $B7, $5F, $B4, $6C, $49, $6F, $16, $B7, $15, $27,
    $27, $07, $F2, $B7, $57, $5F, $5F, $5F, $64, $E4, $B9, $3C, $57, $06, $46, $57,
    $57, $06, $70, $19, $15, $0F, $87, $88, $47, $06, $F0, $42, $13, $83, $87, $70,
    $05, $2A, $20, $A0, $CB, $D9, $24, $DD, $03, $86, $CB, $AC, $A8, $09, $80, $82,
    $4C, $32, $B0, $A0, $86, $72, $40, $D8, $CE, $51, $47, $59, $05, $32, $05, $13,
    $4C, $70, $96, $B1, $58, $A2, $C1, $70, $1A, $0E, $AA, $47, $8F, $AE, $A8, $56,
    $82, $8B, $1D, $1E, $01, $BB, $28, $3E, $80, $F8, $C3, $08, $BE, $17, $83, $7A,
    $2C, $02, $78, $8E, $47, $50, $DD, $D4, $82, $D4, $C4, $C6, $76, $F7, $82, $AF,
    $08, $7E, $EF, $04, $09, $3C, $04, $EB, $21, $15, $1F, $9F, $20, $DC, $42, $2C,
    $7B, $82, $1B, $51, $B7, $1A, $A8, $97, $19, $D1, $74, $08, $9D, $6B, $ED, $B9,
    $51, $E4, $07, $5E, $06, $1C, $1B, $D9, $90, $EE, $85, $42, $2B, $0A, $7F, $46,
    $10, $C0, $2D, $80, $3C, $13, $C8, $D4, $F3, $0C, $40, $0C, $20, $DD, $F3, $0B,
    $C4, $42, $44, $BC, $DB, $68, $CC, $F2, $05, $0A, $1C, $7C, $83, $BF, $00, $D4,
    $54, $D0, $3E, $04, $79, $CB, $8A, $AF, $C1, $60, $DB, $78, $C6, $E0, $BE, $68,
    $9C, $AA, $41, $DB, $8F, $B4, $AA, $CE, $06, $CD, $85, $1C, $C6, $02, $E6, $FB,
    $B7, $9C, $29, $E8, $33, $A0, $BA, $F0, $9B, $39, $04, $03, $BB, $5B, $9C, $8F,
    $99, $0C, $58, $59, $1D, $E4, $71, $2F, $69, $96, $E6, $81, $FD, $57, $4F, $9F,
    $1D, $49, $C0, $0D, $62, $0F, $C1, $8B, $11, $EC, $09, $D3, $93, $3C, $B2, $CF,
    $C0, $15, $09, $77, $5D, $02, $04, $81, $86, $68, $25, $96, $87, $62, $54, $A6,
    $5E, $DB, $CF, $9F, $B8, $05, $11, $AD, $68, $B6, $E8, $43, $44, $C6, $6A, $33,
    $1F, $AC, $38, $C2, $0B, $0F, $8E, $51, $47, $2D, $2E, $20, $98, $01, $33, $78,
    $6C, $48, $76, $BA, $68, $9A, $50, $4F, $9E, $34, $8C, $C6, $E8, $04, $5F, $EF,
    $8F, $95, $2E, $64, $39, $F2, $AD, $13, $55, $BC, $E2, $C6, $89, $CB, $B6, $5F,
    $0B, $89, $F1, $D3, $FA, $40, $97, $99, $BB, $D9, $BD, $62, $37, $19, $8F, $7C,
    $B4, $02, $0C, $B5, $12, $B1, $D1, $42, $4A, $61, $08, $6B, $F4, $F0, $34, $C9,
    $61, $27, $D1, $30, $51, $77, $AB, $91, $CC, $59, $9D, $87, $C6, $31, $6F, $C0,
    $B1, $D4, $64, $B7, $63, $F3, $89, $1A, $DD, $68, $45, $48, $1F, $1A, $EE, $0B,
    $60, $10, $CE, $C8, $E6, $21, $78, $87, $B7, $40, $90, $6D, $3E, $9F, $0C, $0F,
    $8E, $22, $45, $C4, $E8, $04, $F6, $86, $7C, $BD, $22, $C9, $43, $57, $86, $43,
    $EA, $92, $CC, $87, $8C, $C6, $17, $B9, $B6, $9C, $15, $C9, $F1, $0C, $F3, $22,
    $5B, $C5, $B3, $83, $F6, $31, $F6, $F8, $EB, $72, $5E, $86, $D2, $DC, $A2, $03,
    $86, $E5, $87, $7E, $47, $9A, $CC, $25, $5B, $F4, $68, $F7, $10, $45, $6E, $60,
    $C5, $BE, $BB, $1D, $E2, $9B, $3C, $93, $E5, $87, $57, $EF, $9F, $55, $AF, $7A,
    $13, $91, $72, $19, $D0, $B1, $84, $0B, $02, $07, $01, $70, $38, $76, $83, $B5,
    $72, $7C, $BA, $D2, $56, $5B, $8C, $97, $D2, $E7, $32, $3A, $CA, $9F, $EB, $BD,
    $08, $A2, $2B, $DC, $8F, $5C, $C3, $04, $71, $68, $BC, $66, $8A, $60, $3A, $D6,
    $9F, $1D, $C9, $6C, $03, $B9, $0A, $61, $1D, $0D, $32, $7E, $85, $1D, $DB, $20,
    $22, $86, $6A, $C0, $02, $7D, $66, $4A, $9A, $2B, $5D, $E3, $0D, $01, $5D, $7E,
    $B3, $FB, $40, $68, $1F, $AA, $CC, $8D, $91, $26, $B1, $F0, $2C, $48, $72, $50,
    $D8, $97, $0D, $41, $14, $E0, $08, $4C, $5A, $3F, $53, $51, $AF, $00, $D0, $62,
    $0D, $1D, $09, $31, $25, $69, $9A, $A5, $BA, $62, $C4, $EE, $04, $6D, $5B, $09,
    $DA, $18, $21, $B3, $DC, $EB, $05, $47, $9F, $26, $AC, $00, $23, $51, $C6, $EE,
    $1F, $3B, $76, $9E, $6A, $91, $4E, $3A, $0B, $87, $40, $73, $94, $4F, $DF, $05,
    $EB, $BC, $FA, $CE, $FA, $CE, $CE, $F0, $9B, $C7, $9A, $F1, $1C, $88, $04, $33,
    $1E, $91, $B0, $0E, $EB, $0A, $02, $AE, $05, $34, $16, $1F, $6A, $22, $2B, $40,
    $08, $FA, $4E, $74, $EB, $1D, $DB, $8F, $14, $A6, $80, $36, $B7, $05, $2D, $59,
    $44, $07, $01, $81, $DA, $C9, $F6, $DB, $1C, $11, $89, $0B, $98, $5B, $14, $38,
    $1A, $1C, $6B, $54, $70, $40, $D0, $08, $75, $25, $F9, $EE, $B3, $45, $D4, $B1,
    $91, $9C, $18, $5B, $4A, $5E, $39, $91, $23, $14, $43, $E0, $5F, $80, $3F, $81,
    $B6, $72, $71, $82, $B0, $DB, $B5, $20, $9E, $D4, $2E, $76, $88, $C4, $11, $3C,
    $99, $98, $03, $74, $FC, $F7, $CA, $FA, $2A, $76, $38, $C1, $EA, $07, $D9, $82,
    $40, $40, $76, $12, $22, $B9, $31, $08, $5B, $EC, $32, $36, $A9, $EF, $34, $99,
    $EB, $C9, $CF, $23, $1F, $05, $0C, $3F, $C8, $0C, $82, $8D, $EE, $6C, $D9, $14,
    $E2, $07, $81, $AC, $0C, $28, $43, $9B, $62, $4D, $B4, $30, $1B, $4F, $A8, $C2,
    $C5, $F9, $3C, $F0, $A2, $15, $6F, $B4, $72, $7B, $C7, $6C, $18, $52, $53, $40,
    $AA, $4E, $42, $3D, $74, $0D, $3F, $10, $E4, $52, $C0, $40, $04, $53, $10, $E5,
    $A2, $A2, $B1, $3D, $BB, $14, $99, $83, $77, $CF, $5C, $1F, $75, $CD, $32, $13,
    $8A, $47, $81, $20, $7A, $EA, $D9, $57, $55, $00, $17, $23, $EB, $B0, $2E, $61,
    $03, $0C, $8F, $AF, $AC, $6C, $73, $31, $03, $0B, $AF, $AC, $B2, $55, $AF, $81,
    $1D, $CC, $AD, $A8, $B2, $6A, $FF, $AD, $B0, $17, $46, $C5, $0F, $6F, $51, $51,
    $AF, $1B, $B9, $90, $01, $CB, $75, $37, $23, $22, $12, $52, $27, $EB, $AB, $BF,
    $7F, $DB, $54, $60, $9A, $93, $0C, $01, $04, $EA, $0E, $8D, $44, $10, $0B, $97,
    $88, $24, $11, $61, $DF, $00, $A3, $6A, $46, $61, $20, $A0, $61, $E0, $10, $F0,
    $8A, $E3, $C8, $B0, $54, $8A, $9E, $4F, $10, $19, $12, $F3, $F4, $4B, $FC, $42,
    $90, $74, $1E, $F6, $C1, $03, $74, $8A, $01, $41, $31, $D8, $FE, $8E, $00, $6D,
    $24, $08, $C2, $14, $85, $46, $31, $D3, $4F, $98, $ED, $D8, $80, $2F, $CE, $B7,
    $28, $73, $CF, $85, $B6, $FD, $06, $BF, $1E, $31, $DA, $0F, $B6, $CA, $02, $C6,
    $7D, $28, $B6, $AD, $B9, $F6, $1C, $8D, $00, $AF, $28, $0F, $0C, $55, $04, $35,
    $AB, $34, $76, $EC, $6B, $D8, $15, $A7, $08, $0C, $95, $47, $C8, $BC, $6C, $AD,
    $40, $8F, $0D, $35, $C8, $D4, $C1, $E8, $90, $91, $CB, $97, $C8, $C1, $E8, $14,
    $95, $DA, $CC, $45, $67, $E5, $DA, $7D, $CA, $05, $6B, $5B, $2A, $E4, $80, $0C,
    $33, $10, $3D, $58, $02, $6B, $6E, $6B, $68, $83, $EF, $20, $B5, $02, $AC, $CC,
    $75, $6E, $C3, $71, $28, $EC, $0C, $90, $6E, $18, $DA, $C0, $B6, $38, $5B, $6B,
    $1C, $2B, $20, $6E, $9B, $E1, $B5, $58, $61, $68, $3F, $AF, $77, $D0, $7D, $C8,
    $83, $3B, $2F, $49, $CB, $CB, $83, $FF, $2B, $74, $2B, $6F, $8E, $87, $40, $FE,
    $08, $03, $76, $EB, $51, $65, $5B, $92, $6F, $1E, $47, $04, $DE, $04, $4A, $B4,
    $76, $C3, $18, $12, $41, $77, $BE, $9B, $30, $FE, $88, $6A, $38, $4F, $8A, $02,
    $42, $36, $8B, $6A, $39, $19, $34, $F3, $E7, $3C, $A1, $58, $46, $D4, $A8, $1F,
    $01, $07, $A2, $0B, $1C, $E3, $DD, $2E, $8A, $15, $AA, $EA, $85, $E8, $9C, $20,
    $AC, $00, $53, $F0, $83, $B8, $ED, $B9, $45, $53, $F8, $25, $B6, $77, $8C, $95,
    $14, $1E, $C9, $EA, $1F, $7E, $F1, $8D, $BD, $B0, $89, $B7, $09, $C7, $8D, $B5,
    $13, $51, $14, $86, $2F, $DD, $D1, $42, $2D, $F1, $2E, $0E, $F6, $C2, $53, $02,
    $33, $19, $0B, $BC, $41, $04, $3E, $D1, $EA, $75, $FD, $1C, $20, $CD, $F7, $81,
    $87, $7C, $7E, $DE, $23, $87, $F9, $23, $6F, $83, $1C, $86, $DB, $47, $9E, $31,
    $C0, $13, $FE, $90, $B3, $58, $5F, $28, $01, $04, $9F, $43, $83, $FB, $00, $A6,
    $AD, $5D, $D9, $F6, $AA, $17, $1D, $39, $AA, $E0, $E8, $B4, $C8, $FA, $E6, $45,
    $31, $41, $F1, $05, $58, $50, $D9, $47, $D1, $E8, $75, $F1, $0B, $50, $14, $A0,
    $FD, $57, $7E, $95, $6C, $41, $6A, $BB, $E4, $9E, $74, $67, $58, $2F, $5F, $9F,
    $37, $C8, $14, $D2, $F9, $9E, $85, $37, $BF, $21, $64, $B2, $62, $F2, $8D, $25,
    $D1, $F9, $7A, $04, $25, $6A, $0A, $99, $57, $87, $9B, $FB, $0B, $15, $31, $FE,
    $89, $07, $38, $81, $C4, $55, $2B, $2B, $80, $08, $73, $BF, $B5, $C3, $C1, $49,
    $F5, $C2, $67, $70, $0F, $B7, $0A, $68, $AD, $12, $49, $92, $71, $76, $04, $20,
    $C6, $48, $BE, $D6, $74, $40, $11, $D0, $00, $2A, $FC, $0F, $77, $5C, $8D, $77,
    $DB, $96, $DB, $25, $DE, $01, $60, $13, $4E, $01, $D1, $A3, $CF, $89, $0A, $83,
    $7B, $07, $85, $6F, $79, $75, $EB, $81, $F9, $D3, $2D, $06, $81, $E9, $F1, $07,
    $FF, $AD, $04, $AF, $BB, $04, $B5, $2A, $F7, $F3, $89, $D6, $C1, $E6, $10, $09,
    $A3, $B1, $86, $80, $CE, $86, $F0, $81, $89, $9B, $77, $D6, $EA, $5E, $1E, $1B,
    $F6, $68, $24, $F6, $72, $9B, $AA, $01, $7A, $81, $FF, $AF, $15, $AA, $B1, $93,
    $EF, $B0, $15, $1F, $81, $76, $0B, $BE, $5B, $8A, $13, $8D, $04, $11, $6B, $6D,
    $E1, $09, $EA, $EC, $01, $25, $C8, $0B, $05, $0D, $90, $EF, $6D, $6F, $02, $08,
    $06, $03, $11, $08, $04, $43, $0E, $39, $E4, $05, $06, $07, $0E, $39, $E4, $90,
    $08, $09, $0A, $0B, $39, $E4, $90, $43, $0C, $0D, $0E, $6E, $2F, $54, $0E, $0F,
    $31, $10, $8D, $0C, $46, $CA, $4E, $3A, $52, $B0, $E0, $B1, $69, $5F, $D3, $2A,
    $E5, $1D, $47, $A8, $26, $A1, $DC, $9E, $2C, $63, $5F, $75, $8E, $DD, $4A, $0B,
    $20, $03, $39, $2A, $14, $01, $B8, $01, $D6, $1C, $72, $C8, $D7, $31, $08, $02,
    $03, $04, $72, $C8, $21, $87, $05, $06, $07, $CE, $21, $87, $1C, $08, $09, $0A,
    $3B, $64, $6C, $D6, $03, $B7, $1A, $0C, $0D, $08, $2D, $F2, $4D, $0E, $0E, $B5,
    $D6, $34, $DC, $B2, $54, $88, $CE, $91, $EF, $0F, $5A, $B7, $14, $22, $AC, $10,
    $CB, $BD, $A5, $82, $33, $87, $34, $A0, $F3, $F3, $D4, $C9, $08, $9B, $89, $2F,
    $F0, $DB, $DD, $C3, $C1, $F5, $EC, $D8, $82, $5B, $2A, $27, $3D, $11, $05, $2D,
    $43, $18, $AD, $B1, $6C, $C6, $30, $6F, $46, $71, $50, $3A, $11, $61, $FF, $F6,
    $3B, $1C, $1B, $50, $A3, $FA, $70, $2E, $E8, $7C, $6E, $44, $94, $AA, $45, $7F,
    $89, $D7, $DA, $82, $56, $E7, $6E, $14, $C3, $3A, $8D, $E8, $B5, $F7, $6D, $83,
    $20, $D9, $46, $29, $F9, $81, $C6, $57, $28, $FA, $FE, $5A, $10, $7E, $3C, $8D,
    $84, $11, $06, $76, $5E, $E3, $67, $1B, $19, $F0, $4E, $12, $77, $25, $3D, $E2,
    $B5, $7A, $49, $C7, $97, $FD, $06, $3D, $F1, $86, $C1, $E0, $10, $7D, $09, $B2,
    $45, $B9, $B5, $30, $42, $EE, $F2, $97, $D0, $AA, $E7, $49, $62, $1F, $B9, $00,
    $C4, $EB, $2D, $98, $4A, $4F, $1F, $8B, $51, $63, $8D, $7C, $82, $96, $F4, $6B,
    $0D, $5B, $00, $E9, $1D, $72, $EA, $40, $22, $58, $48, $9B, $3F, $DB, $E3, $4F,
    $06, $80, $CA, $05, $8E, $F0, $50, $AF, $05, $39, $7A, $DB, $17, $FF, $BE, $15,
    $0D, $5A, $62, $A9, $E2, $2D, $9E, $A3, $0A, $75, $E9, $D0, $7E, $C0, $A0, $4F,
    $53, $53, $A1, $D0, $44, $16, $53, $15, $4D, $00, $33, $22, $37, $54, $BF, $42,
    $C5, $0A, $B1, $11, $4B, $75, $91, $88, $E0, $13, $7E, $C0, $3D, $BE, $41, $EE,
    $0F, $A0, $0C, $D9, $8B, $0D, $D4, $1A, $40, $03, $8D, $1C, $4B, $29, $5F, $40,
    $07, $44, $77, $CF, $77, $10, $54, $F4, $EB, $C1, $CF, $E7, $D6, $BA, $72, $50,
    $06, $42, $46, $04, $96, $0F, $C7, $05, $D1, $B9, $DD, $4F, $BE, $5D, $EB, $93,
    $9F, $A1, $C0, $0E, $74, $5D, $8D, $41, $EE, $46, $A5, $FF, $E1, $2F, $82, $E5,
    $60, $8B, $2E, $53, $62, $94, $55, $B5, $42, $D0, $BA, $C7, $0A, $AB, $8C, $BA,
    $1F, $34, $59, $47, $AC, $8D, $2A, $22, $70, $49, $39, $9F, $DD, $ED, $BF, $40,
    $6C, $4A, $78, $0E, $80, $7C, $15, $A8, $41, $6F, $09, $CB, $0D, $79, $11, $FF,
    $1C, $FC, $F2, $83, $3B, $3C, $75, $07, $32, $9D, $50, $68, $EA, $00, $F6, $C9,
    $E5, $16, $9B, $24, $C3, $90, $C3, $52, $68, $E4, $14, $2C, $22, $86, $4C, $C0,
    $AF, $D4, $F5, $1E, $2E, $AC, $6B, $1D, $9C, $66, $08, $19, $DC, $21, $10, $FB,
    $00, $2E, $03, $8F, $BC, $B9, $0D, $5E, $A1, $04, $0A, $07, $BC, $A1, $08, $C0,
    $A1, $0C, $3C, $F2, $C8, $23, $C4, $A1, $10, $C8, $A1, $14, $CC, $A1, $18, $14,
    $EE, $23, $8F, $D0, $A1, $1C, $D4, $66, $A1, $20, $08, $E6, $1A, $95, $A2, $81,
    $D1, $DD, $00, $06, $C9, $C9, $C9, $C9, $9C, $A0, $A4, $A8, $68, $CF, $C9, $C9,
    $AC, $B0, $B4, $8D, $37, $9D, $2A, $3A, $46, $11, $9C, $3E, $66, $C0, $84, $BE,
    $23, $55, $90, $6A, $3C, $15, $3C, $B7, $10, $7D, $F3, $41, $A7, $6F, $FC, $89,
    $C7, $B9, $0F, $86, $85, $88, $5F, $B7, $F3, $AB, $AB, $50, $99, $FC, $EE, $2E,
    $6F, $06, $08, $60, $3E, $A1, $70, $DB, $C7, $06, $3C, $E7, $15, $74, $6D, $B7,
    $35, $C8, $A6, $DE, $51, $A3, $56, $18, $10, $D8, $B6, $DF, $BF, $04, $08, $46,
    $1C, $A1, $80, $18, $56, $20, $0A, $2C, $36, $30, $70, $CD, $6C, $AE, $7F, $1A,
    $0C, $F4, $04, $C2, $B2, $4D, $28, $BE, $1E, $38, $34, $9D, $B1, $A3, $0B, $3C,
    $B1, $01, $90, $4D, $21, $C8, $34, $19, $C0, $82, $FB, $4B, $34, $05, $E0, $1F,
    $C0, $41, $88, $84, $15, $48, $37, $97, $1C, $02, $4A, $C2, $E5, $3C, $0D, $67,
    $E4, $D9, $22, $3F, $0A, $6C, $08, $70, $79, $46, $9E, $91, $0C, $74, $10, $78,
    $14, $0C, $72, $C2, $19, $7C, $4E, $80, $84, $93, $11, $DE, $93, $88, $8D, $85,
    $67, $19, $AC, $2C, $88, $8E, $D2, $75, $59, $B4, $87, $35, $89, $03, $E1, $00,
    $D0, $C4, $3C, $D8, $C8, $86, $18, $6A, $33, $2E, $6C, $EB, $10, $FB, $70, $61,
    $0B, $0D, $8D, $2D, $89, $35, $FF, $A3, $B0, $04, $40, $30, $DE, $9B, $0D, $08,
    $A3, $D0, $07, $08, $9F, $19, $66, $04, $4B, $57, $2E, $DA, $39, $F0, $D3, $13,
    $D2, $F7, $75, $92, $EB, $92, $59, $4C, $BF, $51, $89, $E1, $AD, $FF, $5A, $9F,
    $22, $EF, $72, $10, $90, $07, $83, $09, $00, $2D, $16, $D4, $BB, $3F, $EB, $E9,
    $29, $C1, $0B, $89, $E0, $89, $CC, $18, $82, $82, $DD, $C6, $40, $FE, $E0, $2F,
    $FF, $25, $92, $2D, $D8, $66, $77, $07, $00, $00, $0F, $70, $19, $92, $21, $1B,
    $60, $1F, $F0, $64, $21, $19, $92, $21, $88, $58, $92, $21, $19, $92, $D8, $A4,
    $B0, $19, $92, $21, $19, $A8, $7C, $B8, $21, $19, $92, $21, $D0, $BC, $C9, $90,
    $5C, $C8, $20, $12, $AC, $24, $0E, $C9, $90, $0D, $EC, $3F, $18, $14, $12, $48,
    $86, $E4, $42, $08, $F4, $64, $48, $86, $6C, $E0, $0F, $A0, $E8, $17, $72, $48,
    $86, $F8, $10, $12, $1C, $24, $43, $36, $24, $CC, $2F, $00, $39, $24, $43, $32,
    $0C, $C8, $74, $11, $21, $19, $92, $0B, $04, $C0, $92, $21, $19, $B2, $FC, $0F,
    $94, $C4, $19, $92, $21, $19, $E4, $9C, $8C, $21, $19, $92, $21, $DC, $6C, $92,
    $21, $19, $92, $B4, $D4, $90, $19, $92, $21, $19, $4C, $3C, $48, $21, $19, $92,
    $21, $44, $40, $1D, $7C, $90, $90, $38, $5A, $E9, $AC, $2C, $BD, $D8, $2B, $48,
    $8F, $C3, $27, $07, $2A, $0B, $11, $91, $00, $03, $9A, $E6, $0F, $82, $D0, $7E,
    $41, $00, $D5, $DA, $DF, $36, $4D, $B3, $6C, $E0, $83, $E7, $F0, $F9, $02, $84,
    $69, $9A, $A6, $59, $07, $10, $16, $1E, $25, $A6, $69, $9A, $A6, $2E, $37, $3F,
    $47, $4F, $DD, $41, $66, $9A, $58, $61, $00, $F8, $03, $64, $34, $4D, $D3, $35,
    $1E, $23, $0A, $06, $04, $02, $98, $EE, $EC, $80, $90, $A2, $43, $13, $03, $07,
    $A6, $39, $DB, $05, $E9, $10, $A1, $13, $1E, $0F, $6A, $D2, $5C, $9E, $F0, $A2,
    $01, $01, $01, $D9, $0B, $99, $40, $F7, $40, $DC, $44, $9F, $8A, $A0, $B0, $5A,
    $33, $40, $50, $49, $31, $FF, $29, $10, $54, $F2, $C0, $44, $41, $CB, $58, $29,
    $00, $00, $29, $7F, $79, $04, $01, $72, $62, $45, $72, $72, $6F, $72, $3A, $FF,
    $FF, $FF, $FF, $20, $54, $6F, $6F, $20, $6D, $61, $6E, $79, $20, $6F, $70, $74,
    $69, $6F, $6E, $73, $21, $20, $28, $6C, $69, $6D, $69, $74, $20, $69, $73, $20,
    $25, $64, $29, $FF, $DD, $12, $51, $A0, $2A, $00, $20, $47, $4C, $42, $53, $50,
    $20, $4E, $6F, $B7, $DB, $DF, $FE, $64, $65, $20, $42, $75, $69, $6C, $06, $72,
    $20, $32, $2E, $32, $34, $2C, $43, $29, $08, $30, $30, $ED, $7B, $6B, $FF, $37,
    $20, $41, $6E, $64, $72, $65, $77, $06, $45, $65, $64, $20, $32, $0A, $3A, $BF,
    $FD, $A9, $F8, $2F, $3F, $00, $2D, $68, $AF, $73, $61, $67, $65, $6B, $67, $6C,
    $62, $73, $70, $6D, $FF, $36, $D8, $20, $5B, $69, $5D, $62, $6E, $70, $75, $74,
    $2E, $77, $61, $30, $2E, $0D, $D6, $B6, $AD, $00, $17, $2D, $89, $6F, $0F, $12,
    $5D, $F7, $CF, $BE, $BD, $3F, $47, $65, $6E, $66, $61, $6C, $20, $4F, $2F, $3A,
    $0A, $20, $20, $2D, $71, $03, $EE, $1D, $D6, $B6, $80, $65, $9F, $00, $51, $0D,
    $8E, $FB, $ED, $DB, $67, $3A, $2C, $20, $6E, $45, $6C, $65, $76, $65, $36, $73,
    $74, $61, $74, $73, $33, $6B, $7F, $D9, $EF, $66, $03, $61, $73, $32, $20, $52,
    $65, $75, $73, $C8, $FE, $DB, $E6, $BA, $76, $69, $67, $82, $60, $6E, $D6, $ED,
    $74, $38, $62, $FD, $7D, $ED, $7D, $DA, $20, $2B, $50, $39, $77, $03, $61, $72,
    $6E, $7B, $DB, $DB, $97, $39, $53, $68, $6F, $14, $65, $78, $74, $97, $20, $18,
    $3E, $37, $F6, $5A, $61, $67, $3D, $3B, $DE, $6A, $6E, $03, $B7, $0D, $E6, $B6,
    $57, $6D, $54, $00, $46, $0D, $63, $5C, $D0, $12, $F6, $DA, $68, $6F, $18, $6D,
    $12, $D6, $46, $BE, $AF, $32, $72, $74, $A5, $63, $63, $17, $CA, $F7, $AC, $C5,
    $DA, $23, $00, $6B, $D7, $38, $63, $6F, $BC, $F6, $B7, $D0, $36, $94, $73, $AE,
    $10, $64, $39, $53, $45, $47, $DE, $B6, $BD, $B3, $C2, $70, $A7, $DF, $70, $03,
    $39, $6B, $A5, $6D, $6B, $ED, $5D, $50, $0D, $2E, $67, $01, $66, $42, $2B, $74,
    $B7, $B7, $28, $65, $6D, $6F, $76, $46, $64, $75, $32, $63, $14, $7D, $B3, $F6,
    $C8, $6F, $29, $AB, $78, $72, $65, $6A, $65, $74, $36, $63, $DB, $B7, $85, $44,
    $65, $27, $0A, $63, $6C, $6F, $62, $9A, $72, $79, $52, $F1, $66, $E8, $F8, $45,
    $4A, $45, $43, $54, $20, $70, $CE, $00, $41, $64, $76, $0C, $27, $84, $AD, $2A,
    $CE, $64, $92, $76, $31, $BE, $82, $DB, $26, $EB, $06, $35, $7E, $56, $3D, $73,
    $1E, $60, $FF, $30, $6C, $C7, $66, $37, $2D, $4E, $EF, $73, $28, $31, $2C, $32,
    $2C, $61, $7B, $63, $DD, $33, $20, $E4, $35, $84, $6D, $03, $2E, $67, $B1, $DF,
    $65, $EF, $AC, $72, $85, $4D, $0D, $AB, $20, $18, $69, $B0, $70, $1E, $0C, $42,
    $8F, $79, $73, $64, $58, $61, $3B, $BB, $83, $66, $78, $6A, $48, $8F, $64, $EE,
    $AB, $6D, $EC, $77, $E1, $27, $4F, $1D, $2D, $53, $F4, $20, $57, $23, $27, $16,
    $FB, $3C, $14, $0A, $E1, $08, $22, $75, $72, $75, $20, $16, $61, $18, $5B, $82,
    $63, $02, $13, $12, $7C, $0D, $AE, $B9, $95, $30, $16, $78, $73, $91, $62, $E1,
    $60, $28, $FC, $61, $78, $62, $05, $43, $89, $87, $73, $CD, $FD, $2F, $42, $4C,
    $4F, $43, $4B, $4D, $41, $50, $57, $37, $9C, $E3, $C2, $19, $21, $D2, $2C, $4E,
    $6E, $FC, $DC, $1A, $13, $06, $4E, $CA, $5E, $28, $69, $8C, $0D, $43, $2B, $09,
    $26, $C0, $2E, $29, $0A, $DE, $ED, $8C, $A5, $3C, $70, $A1, $6F, $67, $38, $75,
    $7C, $8F, $CB, $3C, $73, $6E, $16, $72, $64, $20, $4D, $C6, $DA, $C1, $D5, $13,
    $3B, $32, $75, $36, $7B, $18, $CC, $D4, $32, $4E, $44, $20, $11, $96, $34, $19,
    $6D, $C2, $6E, $FC, $2C, $08, $0A, $FE, $60, $6D, $C9, $8D, $17, $70, $05, $06,
    $48, $45, $4C, $50, $00, $2D, $A1, $B9, $64, $4D, $0F, $D8, $F9, $12, $72, $38,
    $C7, $D2, $65, $6F, $29, $73, $04, $B6, $51, $6A, $5C, $FA, $4F, $4E, $73, $18,
    $AA, $65, $44, $A4, $6C, $D2, $B2, $2B, $C8, $22, $1A, $D6, $66, $32, $EB, $83,
    $18, $67, $4A, $9D, $A3, $B4, $D1, $47, $54, $68, $2D, $76, $3B, $54, $B2, $F5,
    $1A, $AE, $68, $59, $16, $FB, $4B, $62, $6D, $27, $A9, $69, $4B, $73, $8E, $6C,
    $79, $16, $61, $6B, $B7, $5E, $EE, $81, $5A, $74, $67, $33, $2C, $20, $35, $E4,
    $28, $FC, $5A, $42, $26, $0A, $63, $0A, $16, $5E, $7B, $87, $CD, $A5, $82, $6D,
    $4B, $2C, $1B, $09, $DB, $BB, $77, $DA, $40, $79, $F4, $1C, $92, $44, $45, $55,
    $35, $80, $4F, $B6, $8D, $D6, $8F, $42, $C0, $53, $2E, $A6, $C0, $43, $39, $64,
    $3F, $36, $3D, $98, $32, $54, $75, $E6, $67, $6F, $94, $3A, $2D, $D1, $DE, $95,
    $8D, $0A, $45, $4A, $94, $4C, $65, $67, $7A, $20, $BA, $36, $60, $AD, $D5, $67,
    $00, $66, $55, $D4, $78, $AB, $D4, $68, $9C, $E8, $92, $6E, $FA, $94, $AE, $43,
    $DB, $5A, $40, $09, $70, $16, $F5, $F5, $5B, $6B, $74, $6F, $73, $42, $61, $6B,
    $C2, $26, $B9, $E2, $87, $50, $BA, $DD, $CC, $6D, $5A, $6C, $22, $38, $94, $69,
    $72, $8C, $76, $B0, $3B, $18, $DE, $D5, $75, $61, $62, $19, $78, $36, $43, $6F,
    $76, $73, $9D, $C3, $9F, $82, $A9, $26, $76, $17, $4B, $69, $35, $EF, $C5, $66,
    $B7, $94, $67, $68, $36, $E2, $71, $DE, $D8, $9C, $21, $59, $A5, $0B, $3A, $64,
    $74, $58, $CA, $5A, $5B, $50, $46, $46, $0F, $00, $71, $47, $63, $83, $23, $3A,
    $19, $53, $70, $D2, $5A, $1C, $68, $85, $D2, $52, $BA, $7A, $2E, $26, $A6, $86,
    $94, $6D, $11, $74, $2F, $25, $20, $42, $03, $AE, $63, $61, $69, $2F, $61, $3A,
    $94, $D6, $D0, $7E, $BC, $1B, $56, $79, $6F, $3D, $84, $6F, $C1, $53, $D6, $4A,
    $2E, $B6, $85, $31, $9E, $2E, $6B, $67, $1C, $5E, $24, $DF, $B2, $61, $85, $0A,
    $06, $B6, $BA, $82, $A7, $DF, $73, $0A, $74, $2F, $1B, $78, $07, $C7, $65, $2C,
    $5F, $EF, $39, $63, $6D, $73, $41, $A0, $FD, $5E, $23, $0C, $47, $4E, $87, $EF,
    $98, $1A, $0A, $85, $0A, $42, $2A, $D1, $4C, $F5, $03, $05, $9E, $2B, $8C, $32,
    $83, $02, $66, $C6, $C6, $B7, $33, $75, $41, $69, $4F, $4C, $55, $54, $B5, $59,
    $F1, $58, $A7, $FF, $D2, $4F, $EA, $41, $52, $52, $41, $4E, $54, $59, $2E, $A7,
    $08, $0E, $0E, $BA, $D6, $0A, $FC, $2D, $70, $D3, $42, $7D, $93, $6E, $B7, $07,
    $63, $75, $3A, $6E, $74, $AD, $F2, $6D, $70, $0B, $6D, $73, $F3, $16, $65, $11,
    $73, $73, $B1, $73, $8E, $09, $32, $9B, $4F, $FD, $00, $34, $A0, $F1, $71, $3B,
    $40, $D8, $67, $5F, $F8, $2E, $E2, $0D, $53, $33, $60, $27, $F1, $B2, $80, $8A,
    $B0, $08, $EE, $61, $DC, $61, $76, $6B, $FF, $5E, $14, $29, $D9, $5B, $D3, $74,
    $79, $70, $74, $8E, $9C, $B6, $A5, $8F, $57, $28, $55, $6E, $6B, $17, $77, $A7,
    $65, $60, $72, $E7, $E6, $30, $6F, $49, $C8, $72, $73, $64, $C8, $4C, $87, $54,
    $40, $79, $25, $CC, $2C, $5A, $A4, $B7, $40, $31, $72, $26, $51, $27, $58, $0B,
    $94, $DC, $00, $0A, $2D, $C5, $8B, $96, $6D, $94, $12, $82, $54, $6B, $8C, $6B,
    $6B, $85, $C3, $8D, $43, $6B, $29, $ED, $66, $9F, $EC, $2C, $56, $83, $8F, $44,
    $04, $A3, $00, $23, $36, $4B, $58, $09, $2D, $28, $4B, $13, $1B, $A5, $54, $CA,
    $F9, $79, $31, $2A, $B8, $87, $0D, $88, $75, $C5, $67, $23, $93, $65, $D3, $FD,
    $F7, $50, $00, $C0, $18, $40, $00, $90, $03, $F0, $40, $19, $9D, $A6, $69, $9A,
    $80, $F0, $A0, $90, $20, $1B, $08, $C9, $90, $00, $08, $09, $25, $33, $8D, $85,
    $64, $ED, $64, $25, $25, $06, $11, $43, $49, $59, $81, $ED, $AD, $F5, $45, $52,
    $4E, $41, $AD, $05, $52, $4F, $52, $87, $50, $8A, $68, $30, $38, $0E, $77, $52,
    $9A, $E3, $74, $10, $A0, $D0, $64, $0B, $58, $21, $32, $7D, $B0, $8B, $64, $02,
    $0B, $02, $47, $25, $61, $C9, $21, $80, $3E, $CB, $CC, $60, $A1, $C3, $BF, $86,
    $6D, $BA, $4D, $8D, $95, $4A, $11, $66, $E6, $64, $C3, $26, $31, $70, $DC, $B8,
    $C5, $00, $67, $DF, $00, $42, $F5, $FF, $DB, $DE, $6B, $02, $BE, $BD, $1A, $00,
    $49, $2D, $34, $4C, $68, $24, $0A, $A7, $91, $47, $80, $A9, $54, $28, $A6, $9C,
    $71, $62, $86, $31, $56, $D7, $3B, $20, $14, $68, $BA, $6F, $8A, $78, $C9, $66,
    $F1, $4D, $D8, $49, $12, $A5, $C1, $62, $B2, $19, $76, $2D, $6E, $A7, $29, $19,
    $BC, $17, $76, $67, $D8, $FA, $CB, $5E, $8B, $C5, $DB, $21, $2A, $74, $AD, $35,
    $B8, $13, $B5, $63, $79, $68, $33, $2F, $C4, $9B, $C4, $A1, $66, $65, $54, $33,
    $16, $96, $C2, $96, $89, $32, $33, $A2, $7B, $AF, $FB, $1B, $2C, $B0, $74, $CD,
    $E0, $6C, $64, $5F, $48, $94, $46, $81, $68, $BC, $AE, $00, $56, $34, $0D, $83,
    $D8, $65, $BB, $69, $F0, $10, $84, $24, $9C, $BD, $43, $00, $6F, $00, $EA, $00,
    $61, $5B, $58, $AF, $21, $90, $9E, $4A, $06, $B6, $BD, $84, $F0, $00, $63, $00,
    $53, $25, $4F, $6D, $8B, $B2, $86, $1A, $8E, $59, $64, $66, $65, $2F, $6F, $D2,
    $61, $11, $96, $00, $24, $B5, $C3, $D6, $1D, $78, $1D, $15, $36, $6C, $23, $70,
    $98, $B0, $AF, $B5, $04, $D1, $DD, $5F, $54, $42, $DC, $81, $91, $30, $0E, $0B,
    $64, $DB, $6A, $33, $D1, $14, $6C, $70, $00, $16, $71, $B7, $22, $35, $20, $2C,
    $B2, $00, $72, $6E, $B0, $61, $C9, $1A, $87, $1C, $66, $F3, $A1, $C5, $53, $6F,
    $B3, $00, $77, $00, $11, $00, $58, $AF, $51, $E1, $09, $ED, $00, $08, $34, $00,
    $78, $0B, $36, $8B, $D6, $86, $65, $0B, $1C, $E5, $CA, $87, $D0, $93, $D0, $00,
    $F5, $00, $75, $00, $9A, $00, $D6, $B6, $87, $D0, $79, $00, $DE, $00, $73, $01,
    $6B, $D0, $14, $F6, $CE, $D6, $C1, $6C, $40, $66, $39, $75, $26, $0A, $3C, $CD,
    $82, $BD, $5A, $5C, $4D, $91, $00, $E1, $5A, $B2, $64, $25, $4D, $59, $3A, $EC,
    $C6, $4B, $2C, $CD, $00, $9D, $65, $70, $64, $5C, $6D, $79, $09, $C7, $E3, $CD,
    $5E, $66, $07, $20, $03, $F2, $A2, $D1, $06, $8C, $0C, $00, $DA, $9E, $61, $48,
    $4F, $EC, $86, $00, $68, $EC, $00, $3A, $40, $42, $96, $E1, $35, $8A, $B3, $16,
    $AD, $85, $F4, $92, $4D, $2F, $6F, $4E, $14, $19, $62, $0C, $89, $43, $CC, $3D,
    $76, $13, $62, $D0, $22, $1D, $A1, $0F, $64, $0D, $5A, $A1, $07, $27, $23, $2A,
    $0E, $93, $75, $80, $17, $EF, $5F, $69, $00, $D0, $3B, $04, $81, $46, $00, $CB,
    $1A, $FC, $2E, $E1, $E0, $4E, $F8, $B8, $53, $2C, $09, $53, $53, $E6, $6A, $D6,
    $5C, $DF, $83, $0C, $17, $08, $D5, $D5, $B6, $76, $56, $98, $A1, $58, $22, $93,
    $2A, $FB, $98, $3D, $28, $AC, $B8, $95, $B0, $73, $99, $1B, $2C, $C3, $D0, $6F,
    $28, $DF, $55, $74, $AF, $60, $63, $95, $2D, $F9, $CA, $37, $4E, $AD, $8B, $CB,
    $1E, $F3, $3A, $73, $51, $69, $2E, $6A, $74, $2F, $C8, $65, $64, $94, $29, $54,
    $4E, $00, $AD, $B0, $60, $1A, $4D, $72, $1D, $DB, $CD, $62, $AD, $82, $14, $19,
    $D0, $1B, $00, $43, $66, $68, $43, $58, $19, $4E, $22, $B4, $B9, $98, $A5, $1F,
    $36, $6E, $B0, $16, $85, $4C, $27, $6B, $FC, $62, $50, $52, $0B, $F7, $E2, $68,
    $74, $18, $21, $35, $CE, $CD, $C5, $A6, $37, $72, $12, $65, $62, $74, $4C, $1A,
    $B3, $07, $6C, $AD, $3D, $6F, $43, $2C, $9D, $00, $3E, $DF, $0B, $86, $5A, $0F,
    $3D, $4E, $64, $33, $04, $32, $35, $00, $8E, $DA, $2B, $3C, $5A, $C0, $68, $37,
    $3A, $A4, $B6, $C3, $6A, $AB, $60, $23, $38, $00, $1E, $8A, $34, $F6, $18, $00,
    $30, $07, $65, $B4, $04, $6B, $CE, $C5, $BA, $0C, $03, $6B, $13, $08, $B1, $5D,
    $3B, $B6, $0E, $C7, $65, $5F, $0A, $7A, $21, $1C, $C2, $C3, $00, $69, $00, $7B,
    $00, $6E, $06, $AF, $47, $85, $EC, $59, $36, $BF, $1B, $AC, $9A, $20, $56, $56,
    $BE, $00, $2B, $98, $D5, $95, $0D, $22, $53, $7C, $D2, $00, $DB, $58, $2D, $48,
    $57, $47, $20, $C2, $D6, $B6, $06, $87, $38, $61, $43, $49, $7B, $01, $B6, $CD,
    $12, $D8, $46, $44, $C9, $45, $4C, $44, $63, $CC, $8E, $B0, $23, $4C, $F5, $4A,
    $3C, $56, $65, $61, $7B, $03, $79, $67, $18, $70, $38, $B2, $CE, $84, $23, $A7,
    $DF, $29, $20, $FC, $D5, $B0, $C2, $43, $8F, $50, $FF, $C3, $B6, $83, $CB, $F0,
    $C3, $63, $AE, $E8, $81, $1D, $3D, $2C, $70, $82, $0B, $22, $82, $3F, $13, $5F,
    $31, $00, $1B, $3B, $06, $2F, $32, $31, $1E, $64, $CE, $2D, $A1, $40, $2B, $82,
    $25, $80, $47, $D6, $DE, $C1, $DC, $5B, $B4, $43, $70, $5B, $0A, $31, $8B, $C0,
    $B2, $40, $1F, $13, $18, $E1, $75, $ED, $BF, $28, $3E, $20, $36, $35, $35, $33,
    $34, $29, $D4, $16, $00, $48, $88, $A9, $CD, $88, $C1, $81, $55, $F3, $6E, $C6,
    $73, $07, $C7, $7F, $DD, $32, $1A, $79, $EC, $95, $AD, $6C, $19, $34, $56, $35,
    $37, $10, $21, $60, $81, $1C, $47, $08, $5D, $0A, $7B, $33, $EC, $1F, $5A, $36,
    $8A, $F0, $84, $04, $37, $5A, $53, $A4, $86, $F7, $02, $66, $4E, $2B, $E3, $78,
    $C0, $01, $33, $2D, $7B, $FC, $3A, $28, $33, $92, $8D, $2C, $65, $32, $87, $5B,
    $D6, $7A, $36, $E7, $1D, $4F, $B5, $78, $6D, $C7, $DA, $48, $98, $8D, $B0, $5E,
    $37, $CC, $58, $02, $1B, $16, $1F, $54, $4A, $7F, $90, $10, $5B, $2B, $2C, $42,
    $45, $48, $41, $56, $87, $03, $B4, $A5, $49, $4D, $00, $14, $91, $1A, $80, $C5,
    $2B, $02, $36, $C5, $06, $73, $75, $CD, $80, $66, $4C, $6E, $00, $01, $72, $A5,
    $6C, $59, $24, $19, $C9, $9C, $33, $B0, $3D, $87, $01, $4C, $03, $6D, $6C, $A4,
    $08, $04, $0B, $79, $51, $10, $BD, $66, $AF, $D9, $0B, $35, $09, $47, $04, $1E,
    $C5, $9A, $09, $3A, $C8, $4A, $6B, $20, $65, $C4, $5B, $3E, $65, $26, $5B, $82,
    $06, $0C, $4A, $8C, $76, $36, $D4, $3F, $26, $84, $F2, $22, $4F, $50, $54, $49,
    $F8, $C6, $AD, $DF, $15, $00, $30, $78, $25, $30, $38, $B8, $43, $48, $A7, $4B,
    $53, $55, $4D, $88, $B6, $9B, $F5, $B3, $73, $08, $50, $56, $06, $76, $8A, $19,
    $44, $93, $2C, $84, $D5, $31, $6E, $85, $F6, $55, $49, $4C, $20, $0D, $3F, $4D,
    $45, $21, $92, $B5, $AC, $59, $49, $0A, $37, $36, $4B, $59, $B3, $A8, $BF, $5F,
    $0D, $21, $58, $90, $6C, $60, $ED, $6D, $9A, $F4, $DA, $2F, $53, $43, $52, $49,
    $A4, $88, $EA, $8D, $2C, $49, $43, $96, $55, $AA, $70, $3D, $28, $8C, $8B, $72,
    $BF, $EA, $5E, $0B, $5B, $5B, $44, $79, $31, $6D, $71, $27, $6B, $27, $B0, $7A,
    $04, $B2, $0A, $1B, $65, $3F, $85, $02, $14, $60, $43, $2F, $4B, $E4, $A1, $8B,
    $D6, $C4, $63, $79, $2E, $69, $22, $B1, $77, $5D, $36, $BC, $8A, $44, $52, $16,
    $2C, $6C, $58, $86, $25, $BB, $19, $82, $06, $58, $0A, $50, $20, $11, $8E, $B0,
    $F7, $50, $48, $87, $25, $9A, $83, $94, $BD, $1B, $61, $70, $79, $22, $3E, $92,
    $39, $BA, $08, $DB, $2C, $72, $FA, $21, $3D, $1E, $50, $B9, $B0, $C3, $D9, $2D,
    $4C, $45, $00, $4C, $00, $36, $4C, $7C, $B0, $14, $4C, $C5, $4E, $52, $D7, $6E,
    $C6, $E0, $D5, $E6, $49, $9D, $72, $19, $DB, $00, $EE, $4B, $99, $81, $66, $77,
    $53, $68, $0B, $B6, $7D, $8F, $1A, $5C, $11, $81, $5B, $03, $5D, $27, $46, $67,
    $F4, $D1, $25, $71, $65, $4F, $61, $4E, $C4, $3B, $2B, $B6, $21, $72, $23, $61,
    $32, $63, $A3, $E1, $9E, $82, $28, $62, $09, $DC, $59, $63, $F1, $41, $80, $8F,
    $6D, $4F, $22, $6E, $A7, $63, $25, $B1, $C1, $46, $80, $20, $49, $52, $67, $25,
    $BA, $70, $B4, $AE, $B9, $20, $8D, $CC, $56, $61, $55, $58, $46, $CB, $56, $76,
    $22, $8F, $E3, $59, $97, $10, $C0, $1D, $6F, $5C, $7E, $A4, $83, $C9, $A5, $B0,
    $1D, $E6, $37, $54, $1C, $62, $71, $A0, $79, $FB, $C6, $D8, $B0, $48, $48, $DC,
    $DF, $A0, $6F, $D8, $B0, $76, $57, $06, $AC, $43, $0E, $19, $2E, $D2, $65, $8C,
    $20, $F9, $05, $00, $BC, $58, $90, $58, $FF, $77, $72, $A7, $17, $B3, $65, $39,
    $72, $B8, $A5, $C5, $08, $B3, $07, $F6, $94, $DF, $44, $40, $B3, $87, $61, $55,
    $16, $79, $30, $B0, $C3, $E8, $A8, $55, $9F, $64, $72, $4C, $C2, $38, $2C, $5C,
    $27, $74, $77, $09, $A5, $2D, $44, $21, $1C, $95, $6D, $9A, $06, $43, $87, $D7,
    $CA, $91, $1E, $CB, $0C, $52, $13, $12, $18, $3F, $3F, $77, $62, $46, $F2, $76,
    $08, $76, $08, $03, $57, $19, $03, $0B, $6F, $94, $71, $08, $03, $EF, $E5, $78,
    $0C, $63, $09, $1F, $FB, $FA, $02, $21, $7C, $13, $37, $4A, $65, $40, $A4, $08,
    $C2, $10, $43, $D7, $5A, $F6, $A9, $00, $42, $F7, $21, $44, $65, $11, $8C, $20,
    $C0, $81, $86, $20, $6D, $B6, $C9, $4A, $F1, $31, $2E, $9E, $F0, $73, $21, $74,
    $9A, $5B, $38, $29, $29, $7A, $68, $62, $79, $AF, $D9, $B0, $57, $8F, $C4, $BC,
    $23, $13, $5F, $57, $3C, $23, $A3, $62, $79, $65, $28, $2E, $29, $A0, $B9, $48,
    $48, $83, $B5, $B4, $81, $C9, $06, $76, $2D, $45, $28, $6F, $9C, $38, $84, $DB,
    $A8, $66, $AE, $77, $90, $61, $F6, $00, $D7, $36, $AF, $6F, $D7, $BB, $B1, $1D,
    $7C, $96, $04, $46, $8C, $1D, $25, $2D, $38, $E8, $D6, $05, $1C, $85, $A8, $4E,
    $4B, $AC, $57, $4E, $6B, $A5, $2E, $50, $15, $35, $49, $54, $75, $03, $13, $33,
    $72, $F4, $40, $7A, $36, $33, $28, $76, $C9, $E0, $41, $BE, $05, $EE, $2E, $0A,
    $A5, $EC, $41, $C0, $1A, $9F, $14, $06, $05, $6B, $08, $61, $73, $91, $26, $81,
    $66, $C4, $20, $71, $91, $68, $C1, $C8, $2C, $39, $28, $D3, $62, $7D, $7B, $43,
    $28, $61, $41, $C7, $41, $88, $19, $E0, $B2, $0B, $6E, $70, $42, $7D, $16, $36,
    $AC, $12, $94, $C4, $41, $EB, $52, $36, $84, $BD, $9D, $44, $73, $1C, $CA, $92,
    $45, $0A, $7B, $34, $2C, $7B, $EF, $C5, $23, $16, $D2, $7B, $AE, $03, $AF, $52,
    $CE, $9A, $41, $A7, $B5, $20, $91, $AB, $69, $3E, $17, $C0, $B0, $91, $BD, $85,
    $90, $3E, $A4, $84, $50, $8A, $07, $38, $00, $46, $53, $C2, $62, $18, $96, $1C,
    $AC, $70, $6C, $BA, $EE, $02, $87, $C7, $AC, $21, $B3, $C9, $0D, $30, $56, $1C,
    $2A, $2F, $13, $77, $38, $9D, $6E, $4D, $A0, $0A, $05, $72, $6B, $98, $E9, $5E,
    $C3, $04, $AF, $F9, $89, $27, $20, $A2, $39, $BA, $D6, $A5, $12, $42, $6E, $D8,
    $A6, $25, $CE, $A2, $ED, $B9, $74, $29, $6C, $5F, $4F, $05, $25, $2E, $40, $89,
    $4E, $2D, $C0, $46, $41, $3A, $83, $52, $58, $83, $55, $F0, $7B, $2B, $6B, $2A,
    $4C, $36, $64, $0A, $CB, $C6, $6E, $69, $7B, $2F, $16, $2C, $35, $69, $A6, $77,
    $6B, $EF, $32, $43, $EE, $72, $29, $33, $52, $31, $B4, $8E, $DE, $98, $2B, $73,
    $7E, $14, $46, $EC, $B0, $D2, $37, $4A, $57, $72, $55, $50, $47, $52, $6A, $7B,
    $1D, $25, $28, $AC, $5F, $76, $39, $61, $0B, $22, $D1, $65, $6D, $0A, $07, $6F,
    $0B, $16, $B8, $36, $22, $4C, $8B, $88, $5E, $B0, $B9, $60, $6F, $D8, $40, $73,
    $36, $CD, $96, $B5, $D6, $8F, $09, $4F, $94, $8F, $5A, $09, $93, $82, $FF, $01,
    $E3, $D9, $21, $52, $48, $37, $2E, $9F, $2F, $7A, $84, $AD, $8C, $2B, $30, $5A,
    $44, $87, $36, $63, $F6, $E4, $E6, $EF, $CE, $BD, $ED, $C1, $0B, $0C, $17, $76,
    $F8, $D8, $60, $33, $1A, $85, $67, $59, $43, $D6, $97, $20, $6C, $C1, $DC, $41,
    $B1, $FC, $52, $72, $EC, $96, $68, $67, $80, $BF, $6B, $73, $66, $59, $79, $30,
    $23, $5C, $E5, $5C, $8E, $C7, $38, $56, $45, $70, $4B, $ED, $6E, $0A, $45, $2F,
    $32, $58, $67, $F3, $9D, $70, $1C, $72, $BF, $08, $B4, $AF, $04, $01, $18, $6D,
    $14, $36, $7A, $18, $4E, $88, $04, $61, $6F, $AD, $84, $B0, $03, $C9, $86, $0B,
    $2B, $46, $B4, $2B, $73, $EB, $2D, $54, $20, $EA, $99, $2F, $3C, $68, $E4, $A7,
    $4E, $55, $4C, $4C, $34, $80, $35, $A8, $14, $DB, $4B, $66, $A0, $02, $13, $78,
    $99, $21, $8E, $2F, $B7, $27, $65, $AD, $D2, $B4, $42, $03, $87, $43, $34, $43,
    $18, $AD, $17, $AF, $FE, $2D, $44, $54, $FB, $21, $09, $40, $F3, $3C, $8D, $49,
    $9E, $93, $ED, $34, $64, $2D, $04, $32, $20, $3A, $38, $90, $8B, $BD, $2E, $1D,
    $40, $F9, $3C, $59, $31, $C1, $74, $07, $51, $0E, $BC, $41, $07, $09, $4B, $1D,
    $6C, $57, $17, $EA, $30, $38, $91, $78, $53, $03, $74, $79, $86, $41, $8A, $40,
    $E2, $72, $3A, $85, $68, $54, $8B, $1A, $80, $00, $77, $6C, $46, $5D, $61, $06,
    $F7, $28, $9D, $B8, $AC, $3F, $60, $D9, $00, $4F, $C0, $40, $42, $D4, $04, $C1,
    $3A, $B5, $58, $F2, $AE, $52, $EF, $CD, $71, $38, $5A, $5F, $43, $5A, $AF, $3A,
    $6B, $09, $F1, $41, $52, $29, $1B, $22, $2C, $FB, $28, $D1, $67, $04, $4C, $34,
    $6C, $12, $1C, $7A, $3A, $55, $FE, $58, $17, $34, $16, $F6, $21, $0A, $23, $EF,
    $ED, $30, $A9, $7D, $54, $59, $70, $2C, $0D, $8B, $1D, $18, $86, $A3, $A8, $67,
    $08, $8A, $60, $16, $E9, $94, $33, $32, $73, $B8, $61, $EF, $FE, $86, $1D, $B7,
    $60, $2D, $40, $1C, $46, $03, $C6, $00, $60, $EA, $04, $38, $81, $B1, $46, $A3,
    $65, $54, $3A, $DB, $D1, $B0, $3D, $DC, $42, $14, $5A, $70, $31, $21, $51, $A9,
    $17, $7C, $FB, $73, $B0, $CB, $31, $92, $BD, $CD, $00, $04, $79, $FC, $94, $E1,
    $05, $0B, $0C, $8B, $3A, $B2, $25, $3E, $00, $4B, $D4, $92, $BD, $F6, $DD, $61,
    $54, $B0, $17, $9D, $86, $2D, $32, $90, $56, $D8, $8B, $BD, $68, $47, $48, $54,
    $91, $BB, $27, $FD, $7D, $B0, $8D, $88, $46, $26, $73, $72, $63, $2F, $69, $2E,
    $63, $00, $5E, $10, $5A, $2B, $68, $20, $66, $7D, $3F, $5E, $BF, $0E, $C4, $C7,
    $B7, $1D, $4F, $72, $8F, $83, $56, $24, $01, $58, $25, $32, $B8, $D6, $22, $24,
    $1D, $36, $6C, $A3, $60, $C5, $D8, $4B, $DA, $31, $66, $2C, $05, $29, $E7, $42,
    $03, $CC, $AA, $1A, $89, $67, $C6, $65, $EE, $35, $AB, $F8, $38, $CA, $43, $C3,
    $3B, $05, $5B, $B1, $49, $34, $37, $30, $92, $30, $83, $05, $EA, $3A, $8B, $F1,
    $28, $C0, $13, $14, $36, $C5, $CA, $C8, $65, $DF, $09, $29, $52, $30, $30, $23,
    $6C, $95, $3D, $2F, $43, $38, $0C, $A7, $B7, $EC, $00, $80, $3A, $B3, $26, $9D,
    $5B, $DB, $EA, $37, $0C, $6A, $3E, $45, $4D, $EF, $59, $DB, $1C, $D5, $70, $03,
    $27, $54, $C3, $52, $D8, $C8, $BE, $69, $66, $28, $27, $9A, $AC, $C5, $0B, $8E,
    $6D, $FC, $73, $F8, $26, $C4, $42, $3C, $87, $BF, $7A, $DA, $B6, $E2, $56, $9B,
    $53, $70, $5F, $40, $67, $32, $2E, $8E, $20, $C4, $49, $5B, $80, $40, $B3, $0F,
    $23, $8C, $D3, $90, $BC, $36, $93, $58, $0F, $0E, $4F, $8B, $6F, $EA, $83, $91,
    $35, $B4, $47, $BD, $9A, $97, $32, $06, $33, $B4, $81, $70, $B6, $95, $3E, $07,
    $0A, $43, $1E, $B0, $12, $92, $01, $02, $C4, $E1, $AA, $F7, $E8, $E7, $FA, $F2,
    $15, $D6, $3D, $18, $38, $1F, $7F, $CE, $28, $B2, $3D, $4F, $04, $B3, $04, $36,
    $29, $38, $63, $04, $0B, $7B, $87, $F2, $3A, $6D, $23, $BF, $23, $A4, $CC, $DD,
    $14, $A9, $9A, $99, $00, $B9, $BF, $F1, $92, $86, $67, $07, $C9, $3F, $3D, $3C,
    $E0, $3F, $C1, $A2, $AE, $63, $93, $41, $64, $B0, $62, $BA, $EB, $21, $82, $14,
    $ED, $7B, $4D, $73, $E9, $01, $18, $A0, $67, $5C, $34, $89, $01, $8B, $25, $0F,
    $74, $6F, $D7, $16, $26, $FE, $C2, $54, $4F, $A7, $4C, $41, $52, $47, $45, $21,
    $97, $D0, $82, $B5, $DA, $1B, $2C, $28, $FB, $5D, $90, $1A, $12, $B8, $52, $1E,
    $77, $60, $EA, $70, $D8, $40, $3A, $F1, $09, $72, $F4, $E0, $86, $91, $0A, $C7,
    $43, $C5, $82, $E3, $7B, $93, $41, $41, $28, $2C, $AC, $EE, $43, $C0, $68, $B0,
    $56, $28, $E5, $9F, $44, $32, $D7, $28, $88, $95, $93, $4D, $E2, $D0, $80, $67,
    $28, $F3, $DC, $30, $B2, $37, $A1, $DF, $EE, $BF, $83, $C6, $18, $4C, $98, $04,
    $D0, $96, $21, $6F, $CB, $30, $09, $02, $E1, $31, $52, $7A, $33, $82, $EF, $69,
    $98, $20, $08, $5D, $34, $78, $8C, $8C, $C9, $4B, $D6, $C4, $82, $05, $4A, $7D,
    $4E, $6A, $C1, $80, $01, $24, $07, $4E, $51, $49, $1F, $00, $D9, $3A, $E9, $69,
    $2B, $50, $F0, $23, $74, $49, $18, $26, $2A, $6C, $05, $20, $40, $AD, $BE, $93,
    $D4, $8C, $8F, $FA, $2D, $F8, $2D, $0A, $B0, $6C, $90, $15, $C6, $3B, $B9, $49,
    $6A, $42, $C7, $AC, $28, $7A, $00, $3E, $62, $42, $9E, $0D, $69, $26, $2C, $0E,
    $44, $89, $20, $5B, $62, $58, $66, $30, $D5, $47, $9C, $40, $4A, $A0, $42, $2B,
    $23, $7A, $B4, $71, $60, $8B, $F4, $4C, $D5, $30, $5F, $38, $05, $12, $68, $02,
    $20, $F1, $58, $12, $82, $94, $9D, $88, $6D, $0B, $56, $CB, $06, $1D, $03, $6E,
    $69, $08, $B4, $00, $F8, $60, $58, $2C, $04, $36, $34, $2B, $A2, $73, $34, $CC,
    $06, $18, $A1, $44, $AB, $0B, $80, $42, $8F, $59, $1C, $7E, $AB, $8D, $24, $96,
    $E0, $F1, $98, $53, $60, $58, $60, $A2, $39, $69, $B2, $0B, $17, $84, $A4, $8B,
    $BA, $57, $18, $CB, $E0, $10, $9C, $27, $0B, $3E, $D6, $A4, $23, $0F, $3F, $EB,
    $3A, $4E, $65, $77, $ED, $05, $6C, $13, $71, $84, $67, $14, $0A, $64, $2C, $B4,
    $49, $DA, $50, $B3, $61, $CA, $55, $30, $58, $C2, $3F, $96, $5C, $86, $F1, $EE,
    $2D, $02, $73, $1E, $43, $E0, $FF, $B3, $43, $BF, $7E, $C0, $A1, $E4, $37, $3D,
    $63, $AE, $20, $43, $C4, $9F, $45, $21, $EF, $53, $31, $39, $39, $35, $2D, $A2,
    $AD, $A5, $1A, $BF, $46, $4A, $B2, $6E, $02, $17, $1B, $D0, $FF, $B9, $47, $FB,
    $F3, $29, $3B, $35, $83, $01, $01, $FF, $40, $00, $04, $0D, $F2, $DD, $5D, $08,
    $03, $C0, $01, $41, $0B, $05, $00, $10, $00, $08, $06, $BB, $D9, $66, $AE, $67,
    $01, $0B, $01, $10, $40, $07, $41, $1D, $0C, $F2, $64, $AF, $10, $17, $0B, $80,
    $00, $80, $20, $9E, $B5, $DF, $D9, $00, $01, $09, $02, $01, $34, $0B, $02, $01,
    $2B, $BA, $D8, $69, $10, $AE, $76, $6E, $3A, $A2, $A3, $4A, $81, $FB, $81, $61,
    $1D, $DA, $1C, $15, $A5, $3D, $AE, $6D, $97, $B2, $08, $1D, $A2, $1B, $FA, $50,
    $16, $80, $60, $DB, $58, $0C, $64, $EF, $61, $0A, $1C, $B1, $B5, $16, $97, $75,
    $92, $3E, $69, $9A, $6B, $AE, $C5, $8C, $8C, $23, $11, $1D, $20, $13, $18, $7A,
    $25, $7D, $8A, $78, $7B, $60, $13, $00, $8F, $F6, $B0, $97, $8B, $A6, $69, $9A,
    $6E, $C0, $03, $CB, $CC, $D7, $E4, $5B, $76, $6E, $9B, $EF, $03, $98, $1B, $10,
    $98, $AF, $00, $1B, $FD, $6F, $29, $D1, $03, $04, $05, $06, $07, $08, $08, $09,
    $09, $7B, $0B, $48, $D3, $34, $DD, $0B, $0C, $00, $0D, $0E, $0F, $10, $20, $83,
    $0C, $32, $11, $12, $13, $C9, $90, $0C, $C9, $14, $15, $16, $43, $C9, $90, $0C,
    $17, $18, $19, $32, $94, $0C, $25, $1A, $1B, $0A, $85, $D8, $45, $1C, $FF, $00,
    $01, $06, $19, $A4, $E9, $00, $07, $08, $09, $0A, $4A, $86, $64, $48, $0B, $0C,
    $A8, $64, $28, $19, $0D, $0E, $C3, $46, $A8, $64, $0F, $00, $10, $D3, $10, $06,
    $C2, $04, $C7, $AF, $9F, $B6, $45, $20, $8C, $7F, $00, $1C, $86, $4A, $86, $4A,
    $1D, $00, $34, $4D, $D3, $BD, $27, $03, $08, $18, $04, $14, $D3, $34, $4D, $D3,
    $0C, $1C, $02, $12, $0A, $4D, $D3, $34, $4D, $1A, $06, $16, $0E, $1E, $01, $34,
    $4D, $D3, $34, $11, $09, $19, $05, $15, $D3, $34, $4D, $D3, $0D, $1D, $03, $13,
    $0B, $3B, $CC, $34, $4D, $1B, $07, $17, $00, $0C, $79, $A6, $69, $9A, $AE, $8C,
    $03, $4C, $CC, $2C, $AC, $9A, $A6, $69, $9A, $6C, $EC, $1C, $9C, $5C, $DC, $69,
    $9A, $A6, $69, $3C, $BC, $7C, $FC, $02, $A6, $69, $9A, $A6, $82, $42, $C2, $22,
    $A2, $9A, $A6, $69, $9A, $62, $E2, $12, $92, $52, $D2, $69, $9A, $A6, $69, $32,
    $B2, $72, $F2, $0A, $A6, $69, $9A, $A6, $8A, $4A, $CA, $2A, $AA, $9A, $A6, $69,
    $9A, $6A, $EA, $1A, $9A, $5A, $DA, $69, $9A, $A6, $69, $3A, $BA, $7A, $FA, $06,
    $A6, $69, $9A, $A6, $86, $46, $C6, $26, $A6, $9A, $A6, $69, $9A, $66, $E6, $16,
    $96, $56, $D6, $69, $9A, $A6, $69, $36, $B6, $76, $F6, $0E, $A6, $69, $9A, $A6,
    $8E, $4E, $CE, $2E, $AE, $9A, $A6, $69, $9A, $6E, $EE, $1E, $9E, $5E, $DE, $69,
    $9A, $A6, $69, $3E, $BE, $7E, $FE, $01, $A6, $69, $9A, $A6, $81, $41, $C1, $21,
    $A1, $9A, $A6, $69, $9A, $61, $E1, $11, $91, $51, $D1, $69, $9A, $A6, $69, $31,
    $B1, $71, $F1, $09, $A6, $69, $9A, $A6, $89, $49, $C9, $29, $A9, $9A, $A6, $69,
    $9A, $69, $E9, $19, $99, $59, $D9, $69, $9A, $A6, $69, $39, $B9, $79, $F9, $05,
    $A6, $69, $9A, $A6, $85, $45, $C5, $25, $A5, $9A, $A6, $69, $9A, $65, $E5, $15,
    $95, $55, $D5, $69, $9A, $A6, $69, $35, $B5, $75, $F5, $0D, $A6, $69, $9A, $A6,
    $8D, $4D, $CD, $2D, $AD, $9A, $A6, $69, $9A, $6D, $ED, $1D, $9D, $5D, $DD, $6E,
    $9A, $A6, $69, $3D, $BD, $7D, $FD, $13, $00, $09, $69, $9A, $6E, $59, $01, $93,
    $07, $93, $53, $53, $A6, $69, $9A, $A6, $D3, $D3, $33, $33, $B3, $9A, $A6, $69,
    $9A, $B3, $73, $73, $F3, $F3, $0B, $69, $9A, $A6, $69, $0B, $8B, $8B, $4B, $4B,
    $A6, $69, $9A, $A6, $CB, $CB, $2B, $2B, $AB, $9A, $A6, $69, $9A, $AB, $6B, $6B,
    $EB, $EB, $1B, $69, $9A, $A6, $69, $1B, $9B, $9B, $5B, $5B, $A6, $69, $9A, $A6,
    $DB, $DB, $3B, $3B, $BB, $9A, $A6, $69, $9A, $BB, $7B, $7B, $FB, $FB, $07, $69,
    $9A, $A6, $69, $07, $87, $87, $47, $47, $A6, $69, $9A, $A6, $C7, $C7, $27, $27,
    $A7, $9A, $A6, $69, $9A, $A7, $67, $67, $E7, $E7, $17, $69, $9A, $A6, $69, $17,
    $97, $97, $57, $57, $A6, $69, $9A, $A6, $D7, $D7, $37, $37, $B7, $9A, $A6, $69,
    $9A, $B7, $77, $77, $F7, $F7, $0F, $69, $9A, $A6, $69, $0F, $8F, $8F, $4F, $4F,
    $A6, $69, $9A, $A6, $CF, $CF, $2F, $2F, $AF, $9A, $A6, $69, $9A, $AF, $6F, $6F,
    $EF, $EF, $1F, $69, $9A, $A6, $69, $1F, $9F, $9F, $5F, $5F, $A6, $69, $9A, $A6,
    $DF, $DF, $3F, $3F, $BF, $98, $A6, $69, $9A, $BF, $7F, $7F, $FF, $FF, $4D, $D3,
    $35, $41, $51, $40, $03, $20, $60, $10, $34, $4D, $D3, $34, $50, $30, $70, $08,
    $48, $D3, $34, $4D, $D3, $28, $68, $18, $58, $38, $4D, $D3, $34, $4D, $78, $04,
    $44, $24, $64, $14, $75, $4D, $D3, $34, $54, $34, $74, $03, $41, $83, $D3, $34,
    $4D, $D3, $03, $43, $C3, $23, $A3, $63, $FF, $DF, $F5, $45, $E3, $F5, $11, $21,
    $07, $09, $06, $0A, $05, $0B, $04, $0C, $03, $0D, $02, $0E, $01, $D3, $74, $2B,
    $8B, $0F, $22, $03, $02, $03, $4D, $D3, $34, $4D, $04, $06, $08, $0C, $10, $18,
    $E9, $0C, $D1, $34, $20, $30, $9E, $3C, $60, $80, $6C, $90, $A6, $80, $C0, $80,
    $40, $2C, $60, $43, $D8, $00, $01, $8F, $52, $64, $90, $6F, $D9, $56, $03, $05,
    $06, $90, $41, $06, $19, $07, $08, $09, $41, $06, $19, $64, $0A, $0B, $0C, $D9,
    $2C, $94, $9D, $0D, $FF, $5F, $57, $60, $83, $7D, $6F, $47, $3B, $0E, $CE, $14,
    $D2, $83, $0D, $36, $D8, $1C, $D6, $28, $DA, $38, $DE, $50, $20, $0C, $C2, $60,
    $E2, $70, $27, $A0, $2B, $24, $43, $21, $DD, $E0, $03, $00, $01, $32, $24, $43,
    $32, $02, $03, $04, $0A, $2C, $24, $43, $05, $00, $28, $84, $1D, $36, $83, $07,
    $1F, $FF, $FF, $3F, $C0, $97, $96, $30, $07, $77, $2C, $61, $0E, $EE, $BA, $51,
    $09, $99, $19, $C4, $6D, $07, $8F, $F4, $6A, $70, $FF, $FF, $FF, $FF, $35, $A5,
    $63, $E9, $A3, $95, $64, $9E, $32, $88, $DB, $0E, $A4, $B8, $DC, $79, $1E, $E9,
    $D5, $E0, $88, $D9, $D2, $97, $2B, $4C, $B6, $09, $BD, $7C, $B1, $7E, $FF, $FF,
    $FF, $FF, $07, $2D, $B8, $E7, $91, $1D, $BF, $90, $64, $10, $B7, $1D, $F2, $20,
    $B0, $6A, $48, $71, $B9, $F3, $DE, $41, $BE, $84, $7D, $D4, $DA, $1A, $EB, $E4,
    $DD, $6D, $FF, $FF, $FF, $FF, $51, $B5, $D4, $F4, $C7, $85, $D3, $83, $56, $98,
    $6C, $13, $C0, $A8, $6B, $64, $7A, $F9, $62, $FD, $EC, $C9, $65, $8A, $4F, $5C,
    $01, $14, $D9, $6C, $06, $63, $FF, $7F, $AB, $FF, $63, $3D, $0F, $FA, $F5, $0D,
    $08, $8D, $C8, $AB, $3B, $5E, $10, $69, $4C, $E4, $41, $60, $D5, $72, $71, $67,
    $A2, $D1, $FF, $FF, $FF, $FF, $E4, $03, $3C, $47, $D4, $04, $4B, $FD, $85, $0D,
    $D2, $6B, $B5, $0A, $A5, $FA, $A8, $B5, $35, $6C, $98, $B2, $42, $D6, $C9, $BB,
    $DB, $40, $F9, $BC, $AC, $E3, $5F, $E2, $FF, $FF, $6C, $D8, $32, $75, $5C, $DF,
    $45, $CF, $0D, $D6, $DC, $59, $3D, $D1, $AB, $AC, $30, $D9, $26, $23, $DE, $51,
    $80, $51, $FF, $FF, $FF, $FF, $D7, $C8, $16, $61, $D0, $BF, $B5, $F4, $B4, $21,
    $23, $C4, $B3, $56, $99, $95, $BA, $CF, $0F, $A5, $BD, $B8, $9E, $B8, $02, $28,
    $08, $88, $05, $5F, $B2, $D9, $F0, $FF, $FF, $FF, $0C, $C6, $24, $E9, $0B, $B1,
    $87, $7C, $6F, $2F, $11, $4C, $68, $58, $AB, $1D, $61, $C1, $3D, $2D, $66, $B6,
    $90, $41, $DC, $76, $06, $71, $FF, $FF, $FF, $2F, $39, $BC, $20, $D2, $98, $2A,
    $10, $D5, $EF, $89, $85, $B1, $71, $1F, $B5, $B6, $06, $A5, $E4, $BF, $9F, $33,
    $D4, $B8, $E8, $A2, $C9, $07, $FF, $FF, $7F, $EB, $78, $34, $F9, $B2, $8E, $A8,
    $09, $96, $18, $98, $0E, $E1, $BB, $0D, $6A, $7F, $2D, $3D, $6D, $08, $97, $6C,
    $64, $91, $01, $5C, $FF, $4B, $FC, $FF, $63, $E6, $F4, $51, $6B, $6B, $62, $61,
    $6C, $1C, $D8, $30, $65, $85, $53, $62, $F2, $ED, $95, $06, $6C, $7B, $A5, $01,
    $FF, $FF, $FF, $FF, $1B, $C1, $F4, $08, $82, $57, $C4, $0F, $F5, $C6, $D9, $B0,
    $65, $50, $E9, $B7, $12, $EA, $B8, $BE, $8B, $7C, $88, $B9, $FC, $DF, $1D, $DD,
    $62, $49, $2D, $DA, $6F, $2D, $FC, $FF, $15, $F3, $7C, $D3, $8C, $65, $4C, $D4,
    $FB, $58, $61, $B2, $4D, $CE, $2C, $3A, $3B, $BC, $A3, $E2, $FF, $FF, $DB, $FF,
    $30, $BB, $D4, $41, $A5, $DF, $4A, $D7, $95, $D8, $61, $C4, $D1, $A4, $FB, $F4,
    $D6, $D3, $6A, $E9, $69, $43, $FC, $D9, $6E, $34, $46, $88, $FD, $FF, $FF, $FF,
    $67, $AD, $D0, $B8, $60, $DA, $73, $2D, $04, $44, $E5, $1D, $03, $33, $5F, $4C,
    $0A, $AA, $C9, $7C, $0D, $DD, $3C, $71, $05, $50, $AA, $41, $02, $27, $FF, $FF,
    $FF, $5B, $31, $0B, $BE, $86, $20, $0C, $C9, $25, $B5, $68, $57, $B3, $85, $6F,
    $20, $09, $D4, $66, $B9, $9F, $E4, $61, $CE, $0E, $F9, $DE, $FA, $FF, $FF, $FF,
    $5E, $98, $C9, $D9, $29, $22, $98, $D0, $B0, $B4, $A8, $D7, $C7, $17, $3D, $B3,
    $59, $81, $0D, $B4, $2E, $3B, $5C, $BD, $B7, $AD, $6C, $BA, $C0, $FF, $FF, $6F,
    $2A, $E2, $B6, $B3, $BF, $9A, $0C, $E2, $B6, $03, $9A, $D2, $B1, $74, $39, $47,
    $D5, $EA, $AF, $77, $D2, $9D, $FF, $FF, $FF, $FF, $15, $26, $DB, $04, $83, $16,
    $DC, $73, $12, $0B, $63, $E3, $84, $3B, $64, $94, $3E, $6A, $6D, $0D, $A8, $5A,
    $6A, $7A, $0B, $CF, $0E, $E4, $9D, $FF, $09, $93, $FF, $FF, $FF, $D2, $27, $AE,
    $86, $B1, $9E, $07, $7D, $44, $93, $0F, $F0, $D2, $A3, $08, $87, $68, $F2, $01,
    $1E, $FE, $C2, $06, $69, $5D, $57, $62, $FF, $FF, $FF, $FF, $F7, $CB, $67, $65,
    $80, $71, $36, $6C, $19, $E7, $06, $6B, $6E, $76, $1B, $D4, $FE, $E0, $2B, $D3,
    $89, $5A, $7A, $DA, $10, $CC, $4A, $DD, $67, $6F, $DF, $B9, $FF, $FF, $FF, $FF,
    $F9, $F9, $EF, $BE, $8E, $43, $BE, $B7, $17, $D5, $8E, $B0, $60, $E8, $A3, $D6,
    $D6, $7E, $93, $D1, $A1, $C4, $C2, $D8, $38, $52, $F2, $DF, $4F, $F1, $67, $BB,
    $FF, $FF, $FF, $FF, $D1, $67, $57, $BC, $A6, $DD, $06, $B5, $3F, $4B, $36, $B2,
    $48, $DA, $2B, $0D, $D8, $4C, $1B, $0A, $AF, $F6, $4A, $03, $36, $60, $7A, $04,
    $41, $C3, $EF, $60, $FF, $FF, $FF, $FF, $DF, $55, $DF, $67, $A8, $EF, $8E, $6E,
    $31, $79, $BE, $69, $46, $8C, $B3, $61, $CB, $1A, $83, $66, $BC, $A0, $D2, $6F,
    $25, $36, $E2, $68, $52, $95, $77, $0C, $FF, $FF, $FF, $FF, $CC, $03, $47, $0B,
    $BB, $B9, $16, $02, $22, $2F, $26, $05, $55, $BE, $3B, $BA, $C5, $28, $0B, $BD,
    $B2, $92, $5A, $B4, $2B, $04, $6A, $B3, $5C, $A7, $FF, $D7, $FF, $FF, $FF, $FF,
    $C2, $31, $CF, $D0, $B5, $8B, $9E, $D9, $2C, $1D, $AE, $DE, $5B, $B0, $C2, $64,
    $9B, $26, $F2, $63, $EC, $9C, $A3, $6A, $75, $0A, $93, $6D, $02, $A9, $06, $09,
    $FF, $7F, $E9, $FF, $9C, $3F, $36, $0E, $EB, $85, $67, $07, $72, $13, $57, $A2,
    $82, $4A, $BF, $95, $14, $7A, $B8, $E2, $AE, $2B, $B1, $7B, $38, $1B, $FF, $FF,
    $FF, $FF, $B6, $0C, $9B, $8E, $D2, $92, $0D, $BE, $D5, $E5, $B7, $EF, $DC, $7C,
    $21, $DF, $DB, $0B, $D4, $D2, $D3, $86, $42, $E2, $D4, $F1, $F8, $B3, $DD, $68,
    $6E, $83, $BF, $F1, $FF, $FF, $DA, $1F, $CD, $16, $BE, $81, $5B, $26, $B9, $F6,
    $E1, $77, $B0, $6F, $77, $47, $B7, $18, $E6, $5A, $7D, $70, $6A, $0F, $FF, $CA,
    $C6, $FF, $2F, $F1, $3B, $06, $66, $5C, $F8, $11, $FF, $9E, $65, $8F, $69, $AE,
    $62, $F8, $D3, $FF, $6B, $61, $C4, $FF, $FF, $FF, $FF, $6C, $16, $78, $E2, $0A,
    $A0, $EE, $D2, $0D, $D7, $54, $83, $04, $4E, $C2, $B3, $03, $39, $61, $26, $67,
    $A7, $F7, $16, $60, $D0, $4D, $47, $69, $49, $DB, $FF, $FF, $FF, $FF, $77, $6E,
    $3E, $4A, $6A, $D1, $AE, $DC, $5A, $D6, $D9, $66, $0B, $DF, $40, $F0, $3B, $D8,
    $37, $53, $AE, $BC, $A9, $C5, $9E, $BB, $DE, $7F, $CF, $B2, $47, $E9, $FF, $FF,
    $FF, $FF, $FF, $B5, $30, $1C, $F2, $BD, $BD, $8A, $C2, $BA, $CA, $30, $93, $B3,
    $53, $A6, $A3, $B4, $24, $05, $36, $D0, $BA, $93, $06, $D7, $CD, $29, $57, $DE,
    $54, $BF, $FF, $FF, $FF, $FF, $67, $D9, $23, $2E, $7A, $66, $B3, $B8, $4A, $61,
    $C4, $02, $1B, $68, $5D, $94, $2B, $6F, $2A, $37, $BE, $0B, $B4, $A1, $8E, $0C,
    $C3, $1B, $DF, $05, $5A, $8D, $0B, $FF, $3F, $EA, $EF, $02, $2D, $6B, $31, $1B,
    $19, $82, $62, $36, $32, $C3, $53, $2D, $23, $C5, $FF, $FF, $FF, $1B, $D8, $45,
    $F4, $77, $7D, $86, $A7, $5A, $56, $C7, $96, $41, $4F, $08, $8A, $D9, $C8, $49,
    $BB, $C2, $D1, $8A, $E8, $EF, $FA, $CB, $FF, $FF, $FF, $FF, $D9, $F4, $E3, $0C,
    $4F, $B5, $AC, $4D, $7E, $AE, $B5, $8E, $2D, $83, $9E, $CF, $1C, $98, $87, $51,
    $12, $C2, $4A, $10, $23, $D9, $53, $D3, $70, $F4, $78, $92, $42, $FF, $FF, $FF,
    $41, $EF, $61, $55, $D7, $AE, $2E, $14, $E6, $B5, $37, $D7, $B5, $98, $1C, $96,
    $84, $83, $05, $59, $98, $1B, $82, $18, $E0, $FF, $FF, $FF, $FF, $9B, $DB, $FA,
    $2D, $B0, $9A, $CB, $36, $A9, $5D, $5D, $77, $E6, $1C, $6C, $6C, $FF, $DF, $3F,
    $41, $D4, $9E, $0E, $5A, $CD, $A2, $24, $84, $95, $E3, $15, $F6, $FF, $FF, $FF,
    $9F, $8C, $20, $46, $B2, $A7, $61, $77, $A9, $BE, $A6, $E1, $E8, $F1, $E7, $D0,
    $F3, $E8, $24, $83, $DE, $C3, $65, $B2, $C5, $DA, $AA, $AE, $31, $FF, $FF, $FF,
    $FF, $EB, $9F, $46, $44, $28, $CC, $6B, $6F, $69, $FD, $70, $76, $AE, $6B, $31,
    $39, $EF, $5A, $2A, $20, $2C, $09, $07, $0B, $6D, $38, $1C, $12, $F3, $36, $46,
    $17, $FE, $A5, $FF, $DF, $B2, $07, $5D, $C6, $71, $54, $70, $ED, $82, $6B, $F4,
    $F7, $F3, $2A, $BB, $B6, $E1, $A2, $75, $FF, $FF, $FF, $FF, $91, $1C, $89, $34,
    $A0, $07, $90, $FB, $BC, $9F, $17, $BA, $8D, $84, $0E, $79, $DE, $A9, $25, $38,
    $EF, $B2, $3C, $FF, $79, $F3, $73, $BE, $48, $E8, $6A, $7D, $6F, $FD, $FF, $FF,
    $1B, $C5, $41, $3C, $2A, $DE, $58, $05, $4F, $79, $F0, $44, $7E, $62, $E9, $87,
    $2D, $4F, $C2, $C6, $1C, $54, $08, $8A, $15, $94, $F0, $FF, $FF, $FF, $40, $BB,
    $0E, $8D, $83, $E8, $23, $A6, $C2, $D9, $38, $BF, $0D, $C5, $A0, $38, $4C, $F4,
    $BB, $21, $8F, $A7, $96, $0A, $CE, $96, $8D, $13, $FE, $85, $FF, $6F, $CB, $CC,
    $5C, $48, $31, $D7, $45, $8B, $62, $FA, $6E, $CA, $53, $E5, $54, $5D, $BB, $BA,
    $15, $6C, $A0, $EB, $FF, $FF, $37, $C5, $3F, $8D, $88, $97, $0E, $96, $91, $50,
    $98, $D7, $DE, $11, $A9, $CC, $C7, $D2, $FA, $E1, $EC, $93, $CB, $E2, $FF, $FF,
    $FF, $7F, $5C, $D7, $62, $72, $1D, $E6, $79, $6B, $DE, $B5, $54, $40, $9F, $84,
    $4F, $59, $58, $12, $0E, $16, $19, $23, $15, $0F, $DA, $70, $38, $24, $9B, $41,
    $AD, $FF, $6F, $FC, $23, $3D, $A7, $6B, $FD, $65, $23, $E6, $7C, $25, $09, $CB,
    $57, $64, $38, $D0, $4E, $A3, $AE, $56, $BF, $F0, $FF, $FF, $E2, $9F, $8A, $18,
    $21, $CC, $A7, $33, $60, $FD, $BC, $2A, $AF, $E1, $24, $AD, $EE, $D0, $3F, $B4,
    $6E, $12, $9F, $6C, $B2, $09, $FF, $FF, $FF, $FF, $86, $AB, $24, $48, $C9, $EA,
    $15, $53, $D0, $29, $46, $7E, $FB, $68, $77, $65, $E2, $F6, $79, $3F, $2F, $B7,
    $48, $24, $36, $74, $1B, $09, $1D, $35, $2A, $12, $FF, $FF, $FF, $FF, $04, $F2,
    $BC, $53, $4B, $B3, $8D, $48, $52, $70, $DE, $65, $79, $31, $EF, $7E, $60, $FE,
    $F3, $E6, $E7, $BF, $C2, $FD, $FE, $7C, $91, $D0, $D5, $3D, $A0, $CB, $FF, $FF,
    $FF, $FF, $CC, $FA, $36, $8A, $83, $BB, $07, $91, $9A, $78, $54, $BC, $B1, $39,
    $65, $A7, $A8, $4B, $98, $83, $3B, $0A, $A9, $98, $22, $C9, $FA, $B5, $09, $88,
    $CB, $AE, $FF, $FF, $FF, $FF, $10, $4F, $5D, $EF, $5F, $0E, $6C, $F4, $46, $CD,
    $3F, $D9, $6D, $8C, $0E, $C2, $74, $43, $12, $5A, $F3, $02, $23, $41, $EA, $C1,
    $70, $6C, $C1, $80, $41, $77, $FF, $BF, $D4, $FF, $D8, $47, $D7, $36, $97, $06,
    $E6, $2D, $8E, $C5, $54, $A5, $84, $84, $1B, $BC, $1A, $8A, $41, $71, $5B, $BB,
    $5A, $68, $F1, $FF, $17, $F8, $98, $E8, $77, $43, $D9, $D8, $5A, $1E, $4F, $2D,
    $15, $5F, $7E, $36, $0C, $9C, $2D, $1B, $27, $DD, $FF, $FF, $5B, $2C, $90, $3E,
    $ED, $98, $B9, $53, $31, $83, $A0, $90, $62, $AE, $8B, $D1, $53, $B5, $92, $16,
    $C5, $F4, $5F, $F8, $FF, $FF, $DD, $57, $F4, $EF, $C4, $94, $A7, $C2, $EF, $D5,
    $96, $D9, $F6, $E9, $BC, $07, $AE, $A8, $8D, $1C, $B7, $20, $31, $9C, $2A, $EF,
    $E1, $FF, $7F, $E1, $2A, $85, $ED, $2A, $CA, $AC, $48, $70, $D3, $6F, $1B, $5D,
    $F8, $2E, $2A, $46, $E1, $E1, $36, $DE, $66, $C6, $FF, $FF, $FF, $7F, $C5, $7F,
    $63, $54, $E8, $54, $22, $65, $F3, $4D, $E5, $F3, $B2, $02, $A4, $C2, $A9, $1B,
    $67, $91, $84, $30, $26, $A0, $9F, $29, $B8, $AE, $C5, $E4, $FF, $FF, $FF, $FF,
    $F9, $9F, $DE, $FD, $3A, $CC, $F3, $D6, $7B, $FD, $E8, $CF, $BC, $6B, $A9, $80,
    $FD, $5A, $B2, $99, $3E, $09, $9F, $B2, $7F, $38, $84, $AB, $B0, $24, $1C, $2C,
    $FC, $FF, $FF, $FF, $F1, $15, $07, $35, $32, $46, $2A, $1E, $73, $77, $31, $07,
    $B4, $E1, $70, $48, $F5, $D0, $6B, $51, $36, $83, $46, $7A, $77, $B2, $5D, $63,
    $4E, $D7, $E2, $DF, $F8, $6F, $D6, $0F, $E6, $E1, $D2, $CC, $B5, $CC, $F9, $26,
    $D7, $E0, $4A, $12, $96, $AF, $0B, $FF, $FF, $B7, $22, $AB, $C8, $70, $A0, $9D,
    $89, $41, $BB, $84, $46, $5D, $23, $03, $07, $6C, $38, $1A, $C4, $3F, $FF, $FF,
    $FF, $FF, $15, $31, $85, $0E, $0E, $28, $42, $98, $4F, $67, $03, $A9, $54, $7E,
    $C0, $FA, $79, $55, $81, $CB, $62, $4C, $1F, $C5, $38, $81, $5E, $F4, $23, $98,
    $9D, $A7, $FF, $FF, $4B, $FD, $0E, $B3, $DC, $96, $15, $AA, $9F, $54, $E5, $5A,
    $31, $4F, $FC, $99, $62, $62, $D7, $D8, $53, $79, $CE, $17, $4F, $E1, $FF, $FF,
    $FF, $FF, $49, $56, $7E, $FA, $50, $95, $2D, $D7, $7B, $D4, $1C, $CC, $62, $13,
    $8A, $8D, $2D, $52, $BB, $96, $34, $91, $E8, $BB, $1F, $D0, $D9, $A0, $06, $EC,
    $F3, $7E, $BF, $D1, $FF, $FF, $5E, $AD, $C2, $65, $47, $6E, $91, $48, $6C, $2F,
    $A0, $53, $75, $E8, $36, $12, $3A, $A9, $1F, $23, $6A, $54, $24, $08, $FF, $FF,
    $6F, $FC, $2B, $65, $3F, $11, $E4, $79, $78, $A5, $48, $BC, $8F, $66, $1B, $91,
    $A4, $27, $2A, $8A, $BD, $E0, $BC, $CB, $F2, $A1, $8D, $D0, $FF, $FF, $FF, $DB,
    $EB, $62, $F2, $C0, $23, $EF, $E6, $D9, $BD, $E1, $BC, $14, $FC, $D0, $A7, $0D,
    $3F, $83, $8A, $26, $7E, $B2, $91, $3F, $B9, $24, $D0, $70, $FF, $FF, $6F, $FF,
    $F8, $15, $CB, $69, $3B, $46, $E6, $42, $E0, $FD, $5B, $B5, $6B, $65, $DC, $F4,
    $5A, $7E, $C5, $37, $09, $53, $EE, $76, $38, $48, $F7, $B1, $FF, $9F, $FE, $FF,
    $AE, $09, $B8, $F0, $9F, $12, $A1, $33, $CC, $3F, $8A, $72, $FD, $24, $93, $FF,
    $37, $6A, $C2, $01, $6E, $D4, $84, $03, $59, $BE, $FF, $FF, $BF, $F4, $46, $02,
    $DC, $A8, $58, $EB, $C2, $CB, $06, $B2, $7C, $8D, $04, $85, $16, $4F, $05, $B8,
    $51, $13, $0E, $8F, $3B, $D1, $0F, $D6, $5B, $FF, $DB, $FF, $85, $97, $0D, $E1,
    $EF, $55, $0C, $64, $F9, $1A, $49, $93, $D8, $08, $0A, $2D, $9E, $0A, $3D, $47,
    $D1, $70, $FF, $FF, $FF, $FF, $A3, $26, $1C, $47, $C9, $E4, $1D, $1E, $77, $A2,
    $1F, $29, $1D, $60, $1E, $AC, $0B, $2F, $1B, $9B, $61, $ED, $1A, $C2, $DF, $AB,
    $18, $F5, $B5, $69, $19, $C8, $0A, $FC, $FF, $FF, $F2, $35, $12, $FF, $98, $F7,
    $13, $A6, $26, $B1, $11, $91, $4C, $73, $10, $14, $5A, $3C, $15, $23, $30, $FE,
    $52, $FF, $FF, $8D, $00, $63, $4D, $E4, $7A, $17, $E0, $46, $4D, $38, $D7, $2C,
    $8F, $39, $8E, $92, $C9, $3B, $FF, $12, $FF, $FF, $B9, $F8, $0B, $3A, $3C, $EE,
    $44, $3F, $0B, $84, $86, $3E, $52, $3A, $C0, $3C, $38, $02, $3D, $58, $17, $5E,
    $36, $6F, $02, $FF, $FF, $FF, $7D, $9C, $37, $36, $C3, $DA, $35, $01, $A9, $18,
    $34, $84, $BF, $57, $31, $B3, $D5, $95, $30, $EA, $6B, $D3, $32, $DD, $4E, $FF,
    $1B, $4B, $FF, $33, $90, $E5, $6B, $24, $A7, $8F, $DB, $FE, $EC, $27, $C9, $5B,
    $2D, $26, $4C, $4D, $62, $23, $FF, $FF, $12, $FF, $7B, $27, $A0, $22, $22, $99,
    $E6, $20, $64, $24, $21, $28, $B4, $78, $2A, $1F, $DE, $BA, $2B, $46, $60, $FC,
    $29, $71, $F8, $FF, $FF, $FF, $0A, $3E, $28, $F4, $1C, $71, $2D, $C3, $76, $B3,
    $2C, $9A, $C8, $F5, $2E, $AD, $A2, $37, $2F, $C0, $8D, $9A, $70, $F7, $E7, $58,
    $71, $AE, $59, $FF, $FF, $FF, $DF, $1E, $99, $33, $DC, $72, $1C, $25, $93, $77,
    $2B, $4F, $51, $76, $72, $F1, $17, $74, $45, $9B, $D5, $75, $78, $DC, $89, $7E,
    $4F, $B6, $4B, $7F, $FA, $FF, $FF, $FF, $16, $08, $0D, $7D, $21, $62, $CF, $7C,
    $A4, $74, $80, $79, $93, $1E, $42, $78, $CA, $A0, $04, $7A, $FD, $CA, $C6, $7B,
    $B0, $2E, $BC, $6C, $87, $BF, $70, $EB, $DF, $40, $6D, $DE, $FA, $38, $6F, $E9,
    $90, $13, $6C, $86, $71, $5B, $EC, $77, $6A, $02, $FF, $7F, $E3, $FF, $52, $31,
    $68, $35, $38, $F3, $69, $08, $7F, $AF, $62, $3A, $6D, $63, $66, $AB, $2B, $61,
    $51, $C1, $E9, $60, $D4, $D7, $A6, $65, $2D, $FD, $FF, $FF, $E3, $BD, $64, $64,
    $BA, $03, $22, $66, $8D, $69, $E0, $67, $20, $CB, $D7, $48, $17, $A1, $15, $49,
    $4E, $1F, $B7, $79, $FF, $FF, $FF, $6F, $B4, $4A, $FC, $63, $DE, $4F, $CB, $09,
    $1C, $4E, $92, $B7, $5A, $4C, $A5, $DD, $98, $4D, $98, $9A, $C4, $46, $AF, $F0,
    $06, $47, $F6, $4E, $20, $EA, $FF, $FF, $40, $45, $C1, $24, $82, $44, $44, $32,
    $CD, $41, $73, $58, $0F, $40, $2A, $E6, $49, $42, $1D, $FF, $FF, $FF, $5F, $56,
    $50, $68, $F1, $54, $67, $02, $33, $55, $3E, $BC, $75, $57, $09, $D6, $B7, $56,
    $8C, $C0, $F8, $53, $BB, $AA, $3A, $52, $E2, $14, $7C, $50, $E3, $16, $FF, $FF,
    $D5, $7E, $BE, $51, $E8, $39, $E2, $5A, $DF, $53, $20, $5B, $86, $ED, $66, $59,
    $FC, $A4, $58, $78, $FF, $FF, $FF, $7F, $EB, $5D, $03, $FB, $29, $5C, $5A, $45,
    $6F, $5E, $6D, $2F, $AD, $5F, $80, $1B, $35, $E1, $B7, $71, $F7, $E0, $EE, $CF,
    $B1, $E2, $D9, $A5, $73, $E3, $FF, $FF, $FF, $FF, $5C, $B3, $3C, $E6, $6B, $D9,
    $FE, $E7, $32, $67, $B8, $E5, $05, $0D, $7A, $E4, $38, $4A, $26, $EF, $0F, $20,
    $E4, $EE, $56, $9E, $A2, $EC, $61, $F4, $60, $ED, $BF, $F5, $FF, $FF, $E4, $E2,
    $2F, $E8, $D3, $88, $ED, $E9, $8A, $36, $AB, $EB, $BD, $5C, $69, $EA, $F0, $B8,
    $13, $FD, $EC, $D1, $FC, $9E, $6C, $97, $FF, $FF, $7F, $89, $FE, $3B, $55, $FF,
    $2C, $10, $1A, $FA, $1B, $7A, $D8, $FB, $42, $C4, $9E, $F9, $75, $AE, $5C, $F8,
    $48, $E9, $00, $F3, $FF, $FF, $FF, $FF, $7F, $83, $C2, $F2, $26, $3D, $84, $F0,
    $11, $57, $46, $F1, $94, $41, $09, $F4, $A3, $2B, $CB, $F5, $FA, $95, $8D, $F7,
    $CD, $FF, $4F, $F6, $60, $5D, $78, $D9, $FF, $FF, $FF, $FF, $57, $37, $BA, $D8,
    $0E, $89, $FC, $DA, $39, $E3, $3E, $DB, $BC, $F5, $71, $DE, $8B, $9F, $B3, $DF,
    $D2, $21, $F5, $DD, $E5, $4B, $37, $DC, $D8, $0C, $6B, $D7, $6F, $F4, $FF, $FF,
    $EF, $66, $A9, $D6, $B6, $D8, $EF, $D4, $81, $B2, $2D, $D5, $04, $A4, $62, $D0,
    $33, $CE, $A0, $D1, $A1, $E6, $D3, $5D, $FF, $FF, $FF, $FF, $1A, $24, $D2, $10,
    $FE, $5E, $C5, $27, $94, $9C, $C4, $7E, $2A, $DA, $C6, $49, $40, $18, $C7, $CC,
    $56, $57, $C2, $FB, $3C, $95, $C3, $A2, $82, $D3, $C1, $95, $FF, $86, $BF, $D1,
    $E8, $11, $7A, $AF, $4D, $CB, $9F, $C5, $8F, $A9, $C9, $C8, $F1, $11, $0B, $C9,
    $74, $07, $FF, $97, $FE, $FF, $44, $CC, $43, $6D, $86, $CD, $1A, $D3, $C0, $CF,
    $2D, $B9, $02, $CE, $40, $EE, $91, $77, $FC, $6D, $90, $2E, $42, $2B, $92, $19,
    $37, $F8, $FF, $FF, $28, $E9, $93, $9C, $3E, $A6, $96, $AB, $54, $64, $97, $F2,
    $EA, $22, $95, $C5, $80, $E0, $94, $F8, $C7, $40, $CF, $AD, $FF, $6F, $F0, $FF,
    $7E, $9E, $96, $13, $38, $9C, $A1, $79, $FA, $9D, $24, $6F, $D8, $13, $05, $77,
    $99, $4A, $BB, $31, $9B, $7D, $D1, $F3, $C0, $FF, $FF, $FF, $9A, $30, $35, $89,
    $8D, $07, $5F, $4B, $8C, $5E, $E1, $0D, $8E, $69, $8B, $CF, $8F, $EC, $9D, $80,
    $8A, $DB, $F7, $42, $8B, $82, $FF, $FF, $6F, $50, $53, $B5, $23, $C6, $88, $88,
    $64, $9A, $83, $BF, $0E, $58, $82, $E6, $B0, $1E, $80, $D1, $DA, $DC, $FF, $FF,
    $FF, $FF, $81, $54, $CC, $93, $84, $63, $A6, $51, $85, $3A, $18, $17, $87, $0D,
    $72, $D5, $86, $A0, $D0, $E2, $A9, $97, $BA, $20, $A8, $CE, $04, $66, $AA, $F9,
    $6E, $A4, $FF, $FF, $FF, $FF, $AB, $7C, $78, $EB, $AE, $4B, $12, $29, $AF, $12,
    $AC, $6F, $AD, $25, $C6, $AD, $AC, $18, $81, $F1, $A7, $2F, $EB, $33, $A6, $76,
    $55, $75, $A4, $41, $3F, $B7, $FF, $FF, $FF, $FF, $A5, $C4, $29, $F8, $A0, $F3,
    $43, $3A, $A1, $AA, $FD, $7C, $A3, $9D, $97, $BE, $A2, $D0, $73, $C4, $B5, $E7,
    $19, $06, $B4, $BE, $A7, $40, $B6, $89, $CD, $82, $F8, $FF, $FF, $FF, $B7, $0C,
    $DB, $CD, $B2, $3B, $B1, $0F, $B3, $62, $0F, $49, $B1, $55, $65, $8B, $B0, $68,
    $22, $D7, $BB, $5F, $48, $15, $BA, $06, $F6, $53, $B8, $4F, $FF, $FF, $17, $61,
    $91, $B9, $B4, $8A, $DE, $BC, $83, $E0, $1C, $BD, $DA, $5E, $5A, $BF, $ED, $34,
    $98, $BE, $FF, $65, $FA, $C2, $FF, $FF, $67, $BC, $B8, $8B, $C8, $09, $AA, $EE,
    $AF, $B5, $12, $57, $97, $62, $8F, $32, $F0, $DE, $78, $5F, $6B, $25, $B9, $FF,
    $4B, $2D, $5D, $99, $9D, $EF, $40, $C5, $B4, $08, $7D, $64, $E0, $BD, $6F, $01,
    $87, $01, $FF, $FF, $2F, $F4, $D7, $B8, $BF, $D6, $C5, $D8, $6A, $F2, $33, $77,
    $DF, $E0, $56, $10, $63, $58, $9F, $57, $19, $50, $FA, $30, $A5, $E8, $FF, $FF,
    $FF, $FF, $14, $9F, $10, $FA, $71, $F8, $AC, $42, $C8, $C0, $7B, $DF, $AD, $A7,
    $C7, $67, $43, $08, $72, $75, $26, $6F, $CE, $CD, $70, $7F, $AD, $95, $15, $18,
    $11, $2D, $FF, $7F, $81, $FF, $FB, $B7, $A4, $3F, $9E, $D0, $18, $87, $27, $A2,
    $1A, $42, $8F, $73, $A2, $AC, $20, $C6, $B0, $C9, $47, $7A, $08, $3E, $28, $60,
    $83, $FF, $AF, $32, $A0, $5B, $C8, $8E, $18, $B5, $67, $86, $D0, $97, $FA, $FF,
    $17, $1C, $69, $38, $50, $2F, $0C, $5F, $EC, $97, $E2, $F0, $59, $85, $87, $97,
    $E5, $E1, $87, $86, $46, $FF, $ED, $FF, $65, $B4, $E0, $3A, $DD, $5A, $4F, $8F,
    $CF, $3F, $28, $75, $86, $10, $E4, $EA, $E3, $77, $58, $52, $15, $89, $DB, $FF,
    $FF, $ED, $40, $68, $BF, $51, $F8, $A1, $F8, $2B, $F0, $C4, $9F, $97, $48, $2A,
    $30, $22, $22, $57, $9E, $0F, $BF, $F0, $C2, $7F, $6F, $49, $7F, $93, $08, $F5,
    $C7, $7D, $0F, $D5, $18, $C0, $D8, $4E, $D0, $9F, $35, $2B, $52, $FF, $7F, $81,
    $B7, $D0, $C5, $18, $96, $9F, $A0, $7F, $2A, $27, $19, $47, $FD, $BA, $7C, $20,
    $30, $FA, $5B, $BF, $D5, $92, $E4, $10, $F7, $E8, $48, $A8, $60, $14, $9B, $58,
    $3F, $A8, $23, $BD, $C1, $FF, $B7, $08, $1D, $31, $D3, $F7, $A1, $89, $6A, $CF,
    $76, $14, $0F, $A8, $82, $E1, $07, $7F, $37, $DA, $E0, $5B, $CC, $60, $C3, $06,
    $D2, $07, $5E, $A9, $1C, $E6, $FC, $BF, $D0, $FF, $59, $B8, $A9, $F4, $3C, $DF,
    $15, $4C, $85, $E7, $0B, $E0, $80, $7E, $69, $0E, $2F, $CB, $7B, $6B, $48, $77,
    $FF, $FF, $FF, $6F, $67, $0F, $0D, $CB, $C7, $68, $B1, $73, $29, $C7, $04, $61,
    $4C, $A0, $B8, $D9, $F5, $98, $6F, $44, $90, $FF, $D3, $FC, $7E, $50, $66, $EE,
    $FF, $FF, $0D, $FE, $1B, $37, $DA, $56, $4D, $27, $B9, $34, $40, $05, $B6, $C6,
    $EF, $B0, $A4, $A3, $88, $0C, $1C, $1A, $B0, $DB, $81, $7F, $89, $8D, $FF, $FF,
    $D7, $67, $39, $91, $78, $D2, $2B, $F4, $1F, $6E, $93, $03, $F7, $26, $3B, $66,
    $90, $23, $88, $C6, $D5, $DF, $E0, $7F, $91, $ED, $58, $93, $29, $54, $60, $44,
    $B4, $A5, $F8, $0C, $DF, $A8, $4D, $1E, $4B, $FF, $FF, $BF, $C1, $F1, $A6, $EC,
    $DF, $92, $FE, $89, $B8, $2E, $46, $67, $17, $9B, $54, $02, $70, $27, $EC, $BB,
    $48, $F0, $20, $2F, $FF, $FF, $FF, $FF, $4C, $C9, $30, $80, $F9, $DB, $55, $E7,
    $45, $63, $9C, $A0, $3F, $6B, $F9, $C7, $83, $D3, $17, $68, $36, $C1, $72, $0F,
    $8A, $79, $CB, $37, $5D, $E4, $AE, $50, $FE, $FF, $25, $FE, $E1, $5C, $40, $FF,
    $54, $4E, $25, $94, $F6, $73, $88, $8B, $AE, $16, $EF, $37, $16, $F8, $40, $82,
    $04, $9D, $27, $FF, $0D, $FE, $B7, $25, $24, $1F, $E9, $21, $41, $78, $55, $99,
    $AF, $EA, $8B, $CA, $B0, $5C, $33, $3B, $B6, $59, $FF, $5F, $AA, $FD, $ED, $5E,
    $D1, $E5, $55, $B0, $B0, $F9, $19, $EC, $FF, $6C, $21, $3B, $62, $09, $46, $87,
    $DA, $E7, $FF, $FF, $85, $FE, $E9, $32, $C8, $82, $8E, $8E, $70, $A4, $ED, $28,
    $B1, $F9, $51, $90, $5F, $56, $E4, $82, $3A, $31, $58, $3A, $83, $09, $B7, $FF,
    $FF, $42, $09, $E6, $6E, $33, $1F, $08, $C1, $86, $0D, $6D, $A6, $3A, $B5, $A4,
    $E1, $40, $BD, $0A, $FC, $05, $D1, $FF, $FF, $FF, $2F, $29, $49, $17, $4A, $4E,
    $F5, $AF, $F3, $76, $22, $32, $96, $11, $9E, $8A, $78, $BE, $2B, $98, $1D, $D9,
    $97, $20, $4B, $C9, $0B, $FF, $FF, $BF, $17, $2E, $AE, $48, $C0, $C0, $01, $FD,
    $D2, $A5, $66, $41, $6A, $1C, $5E, $96, $F7, $79, $39, $2A, $4F, $97, $8E, $5D,
    $FF, $FF, $7F, $EB, $F2, $F1, $23, $63, $19, $6B, $4D, $60, $7E, $D7, $F5, $8E,
    $D1, $62, $E7, $EB, $B6, $DE, $5F, $52, $8E, $09, $C2, $37, $E9, $B5, $FF, $FF,
    $FF, $FF, $7A, $D9, $46, $00, $68, $BC, $21, $BC, $D0, $EA, $31, $DF, $88, $8F,
    $56, $63, $30, $61, $F9, $D6, $22, $04, $9E, $6A, $9A, $BD, $A6, $BD, $07, $D8,
    $C1, $01, $FF, $5F, $E8, $FF, $BF, $36, $6E, $B4, $AD, $53, $09, $08, $15, $9A,
    $4E, $5E, $FF, $29, $CE, $A5, $11, $86, $7B, $B7, $74, $E1, $C7, $0F, $FD, $FF,
    $2D, $FE, $CD, $D9, $10, $92, $A8, $BE, $AC, $39, $11, $19, $38, $23, $76, $A5,
    $80, $75, $66, $C6, $D8, $10, $01, $7A, $FF, $FF, $FF, $0B, $07, $AE, $CF, $72,
    $9B, $C9, $73, $CA, $22, $F1, $A4, $57, $47, $96, $18, $EF, $A9, $39, $AD, $FD,
    $CC, $5E, $11, $45, $06, $EE, $C6, $FF, $FF, $FF, $4D, $76, $63, $89, $F1, $CE,
    $8D, $26, $44, $DC, $E8, $41, $F8, $64, $51, $79, $2F, $F9, $34, $1E, $93, $41,
    $DA, $B1, $26, $53, $EA, $5F, $E0, $FF, $FF, $9A, $EB, $E9, $C6, $F9, $B3, $8C,
    $A1, $45, $0B, $62, $0E, $F0, $19, $07, $69, $4C, $A1, $51, $9B, $3C, $DB, $36,
    $F4, $FF, $FF, $FF, $27, $84, $35, $99, $92, $96, $50, $FE, $2E, $2E, $99, $B9,
    $54, $26, $FC, $DE, $E8, $9E, $12, $71, $5D, $8C, $77, $16, $E1, $34, $CE, $2E,
    $BF, $D0, $6F, $6F, $E3, $AB, $49, $8A, $58, $E6, $3F, $03, $20, $81, $6A, $76,
    $91, $E0, $E3, $13, $FF, $FF, $FF, $FF, $F6, $5C, $5B, $FD, $59, $E9, $49, $98,
    $3E, $55, $F1, $21, $06, $82, $6C, $44, $61, $3E, $D4, $AA, $CE, $8B, $C6, $CF,
    $A9, $37, $7E, $38, $41, $7F, $D6, $5D, $A1, $FF, $FF, $FF, $26, $C3, $6E, $B3,
    $89, $76, $7C, $D6, $EE, $CA, $C4, $6F, $D6, $1D, $59, $0A, $B1, $A1, $E1, $E4,
    $1E, $14, $F3, $81, $79, $9E, $5F, $E0, $FF, $7F, $D7, $69, $CB, $13, $B2, $0E,
    $77, $AB, $5C, $A1, $C2, $B9, $39, $C6, $7E, $01, $80, $65, $9C, $E5, $99, $15,
    $FF, $FF, $BF, $F0, $24, $0B, $36, $A0, $02, $51, $1C, $8E, $A7, $16, $66, $86,
    $C2, $71, $DA, $3E, $2C, $DE, $6F, $2C, $49, $B9, $D3, $94, $F0, $81, $FF, $FF,
    $B7, $FA, $04, $09, $95, $E6, $B8, $AC, $49, $0D, $A3, $1E, $2E, $B1, $1B, $48,
    $3E, $D2, $43, $2D, $59, $6E, $FB, $C3, $F6, $DB, $7F, $FA, $FF, $FF, $E9, $A6,
    $91, $67, $51, $1F, $A9, $B0, $CC, $7A, $CE, $0C, $74, $94, $61, $B9, $66, $F1,
    $06, $05, $DE, $FF, $77, $07, $30, $96, $FF, $6F, $F5, $FF, $EE, $0E, $61, $2C,
    $99, $09, $51, $BA, $07, $6D, $C4, $19, $AF, $F4, $8F, $E9, $63, $A5, $35, $9E,
    $64, $95, $A3, $0E, $D4, $FF, $FF, $FF, $DB, $88, $32, $79, $DC, $B8, $A4, $E0,
    $D5, $E9, $1E, $97, $D2, $D9, $88, $09, $B6, $4C, $2B, $7E, $B1, $7C, $BD, $E7,
    $B8, $2D, $FF, $FF, $FF, $BF, $5C, $BF, $1D, $91, $1D, $B7, $10, $64, $6A, $B0,
    $20, $F2, $F3, $B9, $71, $48, $84, $BE, $41, $DE, $1A, $DA, $D4, $7D, $6D, $DD,
    $E4, $EB, $F4, $D4, $FF, $FF, $FF, $C6, $B5, $51, $95, $85, $C7, $13, $6C, $98,
    $56, $64, $6B, $A8, $C0, $FD, $62, $F9, $7A, $8A, $65, $C9, $EC, $14, $01, $5C,
    $4F, $63, $FF, $FF, $12, $FF, $06, $6C, $D9, $FA, $0F, $3D, $63, $8D, $53, $F5,
    $3B, $6E, $20, $C8, $4C, $69, $10, $5E, $D5, $60, $41, $E4, $A2, $67, $FF, $12,
    $FF, $FF, $71, $72, $3C, $03, $E4, $D1, $4B, $04, $D4, $47, $D2, $0D, $85, $FD,
    $A5, $0A, $4B, $35, $B5, $A8, $FA, $42, $B2, $98, $FD, $FF, $FF, $FF, $6C, $DB,
    $BB, $C9, $D6, $AC, $BC, $F9, $40, $32, $D8, $6C, $E3, $45, $DF, $5C, $75, $DC,
    $D6, $0D, $CF, $AB, $D1, $3D, $59, $26, $D9, $30, $AC, $51, $FF, $FF, $FF, $BF,
    $C5, $3A, $C8, $D7, $51, $80, $BF, $D0, $61, $16, $21, $B4, $F4, $B5, $56, $B3,
    $C4, $23, $CF, $BA, $95, $99, $B8, $BD, $A5, $0F, $28, $02, $B8, $9E, $FF, $FF,
    $FF, $FF, $5F, $05, $88, $08, $C6, $0C, $D9, $B2, $B1, $0B, $E9, $24, $2F, $6F,
    $7C, $87, $58, $68, $4C, $11, $C1, $61, $1D, $AB, $B6, $66, $2D, $3D, $76, $DC,
    $41, $90, $FD, $FF, $46, $FF, $01, $DB, $71, $06, $98, $D2, $20, $BC, $90, $10,
    $2A, $71, $B1, $85, $89, $06, $B6, $B5, $1F, $9F, $BF, $E4, $7F, $63, $E9, $6F,
    $D4, $B8, $D4, $33, $78, $07, $C9, $E2, $00, $12, $96, $09, $A8, $8E, $E1, $0E,
    $D1, $6F, $F0, $FF, $98, $18, $7F, $6A, $0D, $BB, $08, $6D, $3D, $2D, $91, $64,
    $F0, $E6, $63, $5C, $01, $6B, $FF, $56, $D5, $A2, $4A, $52, $D1, $85, $65, $30,
    $D8, $F2, $62, $00, $FF, $FF, $FF, $FF, $4E, $6C, $06, $95, $ED, $1B, $01, $A5,
    $7B, $82, $08, $F4, $C1, $F5, $0F, $C4, $57, $65, $B0, $D9, $C6, $12, $B7, $E9,
    $50, $8B, $BE, $B8, $EA, $FC, $B9, $88, $A5, $FF, $BF, $F5, $7C, $62, $DD, $1D,
    $45, $DA, $2D, $49, $8C, $D3, $7C, $F3, $FB, $D4, $4C, $65, $4D, $B2, $61, $54,
    $FF, $FF, $FF, $0B, $2A, $CE, $A3, $BC, $00, $74, $D4, $BB, $30, $E2, $4A, $DF,
    $A5, $41, $3D, $D8, $95, $D7, $A4, $D1, $C4, $6D, $D3, $D6, $F4, $FB, $FF, $FF,
    $FF, $FF, $43, $69, $E9, $6A, $34, $6E, $D9, $FC, $AD, $67, $88, $46, $DA, $60,
    $B8, $D0, $44, $04, $2D, $73, $33, $03, $1D, $E5, $AA, $0A, $4C, $5F, $DD, $0D,
    $7C, $C9, $0B, $FF, $FF, $FF, $50, $05, $71, $3C, $27, $02, $41, $AA, $BE, $0B,
    $10, $10, $C9, $0C, $20, $86, $57, $68, $B5, $25, $20, $6F, $85, $B3, $DD, $D4,
    $FF, $A5, $FE, $FF, $09, $CE, $61, $E4, $9F, $5E, $DE, $F9, $0E, $29, $D9, $C9,
    $98, $B0, $D0, $E3, $C7, $D7, $A8, $B4, $59, $B3, $3D, $17, $FC, $FF, $97, $F8,
    $2E, $B4, $0D, $81, $B7, $BC, $3B, $C0, $BA, $6C, $AD, $ED, $B8, $83, $20, $9A,
    $BF, $B3, $B6, $03, $B6, $E2, $FF, $FF, $FF, $6F, $14, $B1, $D2, $9A, $EA, $D5,
    $47, $39, $9D, $D2, $77, $AF, $04, $DB, $26, $15, $73, $DC, $16, $83, $E3, $63,
    $0B, $12, $94, $64, $3B, $84, $FF, $FF, $FF, $4B, $EC, $6A, $3E, $7A, $6A, $5A,
    $A8, $E4, $0E, $CF, $0B, $93, $09, $FF, $9D, $0A, $00, $AE, $27, $7D, $07, $9E,
    $B1, $F0, $0F, $93, $BF, $C1, $FF, $FF, $44, $87, $08, $A3, $D2, $1E, $01, $F2,
    $68, $69, $06, $C2, $FE, $F7, $62, $57, $5D, $80, $50, $CB, $19, $6C, $36, $71,
    $5F, $EA, $FF, $FF, $6E, $6B, $06, $E7, $FE, $D4, $1B, $76, $89, $D3, $2B, $E0,
    $10, $DA, $7A, $5A, $67, $DD, $4A, $64, $B9, $DF, $6F, $8E, $FF, $D2, $FF, $FF,
    $BE, $EF, $F9, $17, $B7, $BE, $43, $60, $B0, $8E, $D5, $D6, $D6, $A3, $E8, $A1,
    $D1, $93, $07, $D8, $C2, $C4, $4F, $DF, $F2, $52, $FF, $FF, $7F, $EB, $D1, $BB,
    $67, $F4, $BC, $57, $67, $3F, $B5, $06, $DD, $48, $B2, $36, $4B, $D8, $0D, $2B,
    $DA, $AF, $0A, $1B, $4C, $36, $03, $4A, $AD, $FF, $7F, $E9, $F6, $41, $04, $C7,
    $DF, $60, $EF, $C3, $A8, $67, $DF, $55, $31, $6E, $8E, $EF, $46, $69, $BE, $EF,
    $E8, $FF, $FF, $A5, $61, $9E, $BC, $66, $83, $1A, $25, $6F, $D2, $A0, $52, $68,
    $E2, $36, $CC, $0C, $77, $95, $BB, $0B, $47, $2D, $FE, $FF, $5F, $5D, $02, $16,
    $B9, $55, $05, $26, $2F, $C5, $BA, $3B, $BE, $B2, $BD, $0B, $28, $2B, $B4, $5A,
    $92, $DB, $16, $FF, $FF, $FF, $6A, $04, $C2, $D7, $FF, $A7, $B5, $D0, $CF, $31,
    $2C, $D9, $9E, $8B, $5B, $DE, $AE, $1D, $9B, $64, $C2, $B0, $EC, $63, $9E, $10,
    $F1, $FF, $FF, $75, $6A, $A3, $9C, $02, $6D, $93, $0A, $9C, $09, $06, $A9, $EB,
    $0E, $36, $3F, $72, $07, $67, $FC, $FF, $FF, $2F, $43, $57, $13, $95, $BF, $4A,
    $82, $E2, $B8, $7A, $14, $7B, $B1, $2B, $AE, $0C, $B6, $1B, $38, $92, $D2, $8E,
    $9B, $E5, $D5, $BE, $FF, $FF, $FF, $2F, $79, $DC, $EF, $B7, $0B, $DB, $DF, $21,
    $86, $D3, $D2, $D4, $F1, $D4, $E2, $42, $68, $DD, $B3, $F8, $1F, $DA, $83, $6E,
    $81, $BE, $16, $CD, $2D, $BE, $F1, $FF, $F6, $B9, $26, $5B, $6F, $B0, $77, $E1,
    $18, $B7, $47, $77, $79, $5A, $E6, $FF, $0F, $AD, $BF, $C1, $FF, $FF, $66, $06,
    $3B, $CA, $11, $01, $0B, $5C, $8F, $65, $9E, $FF, $F8, $62, $AE, $69, $61, $6B,
    $24, $16, $6C, $CF, $45, $A0, $FF, $FF, $FF, $FF, $0A, $E2, $78, $D7, $0D, $D2,
    $EE, $4E, $04, $83, $54, $39, $03, $B3, $C2, $A7, $67, $26, $61, $D0, $60, $16,
    $F7, $49, $69, $47, $4D, $3E, $6E, $77, $DB, $AE, $FF, $FF, $FF, $16, $E9, $4A,
    $D9, $D6, $5A, $DC, $40, $DF, $0B, $66, $37, $D8, $3B, $F0, $A9, $BC, $AE, $53,
    $DE, $BB, $9E, $C5, $47, $B2, $FF, $FF, $FF, $FF, $CF, $7F, $30, $B5, $FF, $E9,
    $BD, $BD, $F2, $1C, $CA, $BA, $C2, $8A, $53, $B3, $93, $30, $24, $B4, $A3, $A6,
    $BA, $D0, $36, $05, $CD, $D7, $06, $93, $54, $DE, $FF, $FF, $5F, $F8, $57, $29,
    $23, $D9, $67, $D9, $66, $7A, $2E, $C4, $61, $4A, $B8, $5D, $68, $1B, $02, $2A,
    $6F, $2B, $94, $B4, $0B, $BE, $37, $C3, $FC, $FF, $E9, $FF, $0C, $8E, $A1, $5A,
    $05, $DF, $1B, $2D, $02, $EF, $8D, $FF, $19, $1B, $31, $41, $32, $36, $62, $82,
    $2B, $2D, $53, $C3, $FF, $FF, $FF, $6F, $D6, $C5, $04, $7D, $77, $F4, $45, $56,
    $5A, $A7, $86, $4F, $41, $96, $C7, $C8, $D9, $8A, $08, $D1, $C2, $BB, $49, $FA,
    $EF, $E8, $8A, $E3, $FF, $FF, $FF, $FF, $F4, $D9, $CB, $AC, $B5, $4F, $0C, $B5,
    $AE, $7E, $4D, $9E, $83, $2D, $8E, $87, $98, $1C, $CF, $4A, $C2, $12, $51, $53,
    $D9, $23, $10, $78, $F4, $70, $D3, $61, $FF, $FF, $5F, $E0, $EF, $41, $92, $EB,
    $D7, $55, $37, $B5, $E6, $14, $1C, $98, $B5, $D7, $05, $83, $84, $96, $82, $1B,
    $98, $59, $9B, $00, $37, $F8, $FF, $52, $B8, $B0, $2D, $FA, $DB, $A9, $36, $CB,
    $9A, $E6, $77, $5D, $5D, $64, $6C, $1C, $FF, $FF, $7F, $8B, $D4, $CC, $DF, $CD,
    $5A, $0E, $9E, $95, $84, $24, $A2, $8C, $9F, $15, $E3, $A7, $B2, $46, $20, $BE,
    $A9, $77, $61, $F1, $FF, $DF, $FE, $FF, $E8, $E1, $A6, $E8, $F3, $D0, $E7, $C3,
    $DE, $83, $24, $DA, $C5, $B2, $65, $2D, $AE, $AA, $44, $46, $9F, $EB, $6F, $6B,
    $CC, $28, $76, $70, $FF, $FF, $FF, $FF, $FD, $69, $39, $31, $6B, $AE, $20, $2A,
    $5A, $EF, $0B, $07, $09, $2C, $12, $1C, $38, $6D, $DF, $46, $36, $F3, $C6, $5D,
    $07, $B2, $ED, $70, $54, $71, $F4, $6B, $FF, $0B, $FD, $4B, $84, $BB, $2A, $F3,
    $F7, $A2, $31, $C2, $0B, $1C, $91, $75, $90, $07, $A0, $34, $17, $9F, $55, $E2,
    $FF, $FF, $BC, $FB, $0E, $84, $8D, $BA, $25, $A9, $DE, $79, $3C, $B2, $EF, $38,
    $73, $F3, $79, $FF, $6A, $F7, $FF, $6F, $F5, $BF, $A9, $C5, $1B, $7D, $58, $DE,
    $2A, $3C, $F0, $79, $E3, $E9, $62, $7E, $44, $C2, $4F, $2D, $87, $DB, $54, $1C,
    $D0, $DF, $28, $E2, $C6, $94, $15, $DF, $0E, $BB, $40, $A6, $23, $E8, $FF, $56,
    $FF, $BF, $B7, $38, $D9, $C2, $38, $A0, $C5, $0D, $21, $BB, $F4, $4C, $0A, $96,
    $65, $13, $8D, $96, $CE, $5C, $CC, $00, $FF, $85, $85, $FF, $09, $45, $D7, $31,
    $48, $6E, $FA, $62, $8B, $E1, $53, $7A, $BB, $5D, $54, $A3, $A0, $6C, $15, $88,
    $D1, $DF, $E8, $FF, $8D, $3F, $D6, $91, $96, $0E, $97, $DE, $D7, $98, $50, $84,
    $A9, $11, $EC, $E1, $FA, $D2, $52, $FF, $FF, $BF, $E4, $CB, $93, $72, $62, $D7,
    $5C, $6B, $79, $E6, $1D, $40, $54, $B5, $DE, $59, $4F, $84, $9F, $16, $0E, $12,
    $BD, $DA, $82, $FF, $FF, $15, $23, $19, $24, $38, $70, $DA, $3D, $23, $41, $9B,
    $65, $FD, $6B, $A7, $7C, $00, $57, $FF, $FF, $2D, $96, $F4, $25, $AB, $38, $64,
    $01, $91, $AE, $A3, $18, $8A, $9F, $E2, $33, $A7, $CC, $21, $2A, $BC, $FF, $BF,
    $F0, $FF, $FD, $60, $AD, $24, $E1, $AF, $B4, $3F, $D0, $EE, $9F, $12, $70, $86,
    $09, $B2, $6C, $C9, $48, $24, $AB, $D0, $53, $15, $EA, $FB, $D2, $FF, $FF, $FF,
    $7E, $46, $29, $E2, $65, $77, $68, $2F, $3F, $79, $F6, $36, $24, $48, $B7, $1D,
    $09, $1B, $74, $04, $12, $2A, $35, $4B, $53, $BC, $44, $FF, $FF, $12, $FF, $48,
    $8D, $B3, $79, $65, $DE, $70, $57, $EF, $31, $E7, $E6, $F3, $FE, $FE, $FD, $C2,
    $BF, $D5, $D0, $91, $7C, $CC, $FF, $FF, $2F, $F5, $CB, $A0, $3D, $83, $B8, $FA,
    $9A, $91, $07, $BB, $B1, $BC, $54, $78, $A8, $A7, $65, $39, $3B, $83, $98, $4B,
    $22, $98, $FF, $FF, $FF, $FF, $A9, $0A, $09, $B5, $FA, $C9, $10, $AE, $CB, $88,
    $5F, $EF, $5D, $4F, $46, $F4, $6C, $0E, $6D, $D9, $3F, $CD, $74, $C2, $0E, $8C,
    $F3, $5A, $12, $43, $EA, $41, $FF, $FF, $FF, $FF, $23, $02, $C1, $6C, $70, $C1,
    $D8, $77, $41, $80, $97, $36, $D7, $47, $8E, $2D, $E6, $06, $A5, $00, $B5, $C5,
    $BC, $1B, $84, $84, $71, $41, $8A, $1A, $68, $5A, $C6, $FF, $05, $FE, $BB, $5B,
    $43, $77, $E8, $98, $5A, $D6, $D9, $15, $2D, $4F, $1E, $0C, $36, $7E, $5F, $27,
    $5D, $B7, $F8, $7F, $A9, $4A, $00, $1C, $DD, $B9, $98, $00, $12, $A0, $83, $31,
    $53, $85, $62, $90, $FF, $DF, $F8, $FF, $92, $B5, $53, $D1, $DD, $F4, $C5, $16,
    $C4, $EF, $F4, $57, $EF, $E9, $94, $F6, $D9, $96, $D5, $AE, $07, $BC, $E9, $B7,
    $1C, $8D, $FF, $C2, $16, $FC, $A8, $9C, $31, $DE, $6B, $85, $00, $CA, $2C, $ED,
    $D3, $70, $48, $AC, $F8, $5D, $97, $A0, $F0, $FF, $1B, $6F, $E1, $46, $2A, $2E,
    $66, $DE, $36, $E1, $7F, $C5, $C8, $FE, $63, $4D, $B4, $C1, $FF, $D2, $F3, $65,
    $D6, $B2, $F3, $E5, $1B, $A9, $C2, $A4, $30, $84, $CC, $29, $FF, $B7, $F8, $2F,
    $D1, $26, $E4, $C5, $AE, $B8, $FD, $DE, $9F, $1D, $F3, $CC, $3A, $CF, $E8, $FD,
    $7B, $80, $A9, $6B, $5B, $FC, $FF, $FF, $BC, $99, $B2, $5A, $FD, $B2, $9F, $09,
    $3E, $AB, $84, $38, $7F, $2C, $1C, $24, $B0, $35, $07, $15, $F1, $1E, $11, $32,
    $F8, $FF, $FF, $FF, $07, $31, $77, $73, $48, $70, $E1, $B4, $51, $6B, $D0, $F5,
    $7A, $46, $83, $36, $63, $5D, $B2, $77, $CB, $FA, $D7, $4E, $D2, $E1, $E6, $0F,
    $F9, $FF, $FF, $C6, $2D, $00, $E0, $D7, $28, $AF, $96, $12, $4A, $B6, $8D, $23,
    $0B, $9D, $A0, $70, $C8, $84, $BB, $41, $FA, $FF, $6F, $FF, $89, $03, $23, $5D,
    $46, $1A, $38, $6C, $3A, $15, $3F, $C4, $28, $0E, $0E, $85, $67, $4F, $98, $42,
    $7E, $54, $A9, $03, $55, $FF, $FF, $FF, $B7, $0B, $C0, $4C, $62, $CB, $81, $81,
    $38, $C5, $1F, $98, $23, $F4, $5E, $B3, $0E, $A7, $9D, $AA, $15, $96, $DC, $E5,
    $54, $00, $1B, $FC, $FF, $A5, $FE, $FF, $4F, $31, $5A, $D7, $62, $62, $99, $CE,
    $79, $53, $D8, $49, $E1, $4F, $17, $20, $7E, $56, $7B, $D7, $2D, $95, $62, $CC,
    $FF, $DF, $E0, $1B, $F6, $2D, $8D, $8A, $13, $4C, $BB, $52, $1F, $BB, $E8, $91,
    $06, $A0, $D9, $D0, $5E, $7E, $FC, $FF, $FF, $FF, $F3, $EC, $47, $65, $C2, $AD,
    $6C, $48, $91, $6E, $75, $53, $A0, $2F, $3A, $12, $36, $E8, $23, $09, $07, $A9,
    $08, $24, $54, $6A, $11, $3F, $65, $2B, $7F, $A3, $FF, $6F, $76, $79, $E4, $8F,
    $BC, $48, $A5, $A4, $91, $1B, $66, $BD, $8A, $BF, $F2, $CB, $BC, $E0, $EB, $D0,
    $FF, $FF, $FF, $ED, $8D, $A1, $C0, $F4, $62, $D9, $E6, $EF, $23, $14, $BC, $E1,
    $BD, $0D, $A7, $D0, $FC, $26, $8A, $83, $3F, $3F, $91, $B2, $7E, $70, $D0, $24,
    $FC, $16, $7F, $8B, $B9, $32, $15, $F8, $42, $E6, $46, $3B, $74, $77, $7A, $DC,
    $65, $6B, $C5, $7F, $A1, $2F, $A8, $7E, $5A, $F4, $EE, $2C, $37, $F7, $48, $38,
    $76, $B8, $09, $AE, $FF, $17, $FD, $BF, $5D, $12, $9F, $F0, $8A, $3F, $CC, $33,
    $93, $24, $FD, $72, $D3, $C2, $6A, $37, $03, $84, $D4, $6E, $FF, $FF, $BF, $F4,
    $02, $46, $BE, $59, $56, $A8, $DC, $06, $CB, $C2, $EB, $04, $8D, $7C, $B2, $05,
    $4F, $16, $85, $0E, $13, $51, $B8, $0F, $D1, $3B, $DF, $FA, $FF, $FF, $8F, $0D,
    $97, $85, $D6, $0C, $55, $EF, $E1, $09, $1A, $F9, $64, $08, $D8, $93, $53, $0A,
    $9E, $2D, $0A, $CD, $47, $3D, $1C, $26, $FF, $FF, $2F, $F5, $A3, $70, $1D, $E4,
    $C9, $1F, $A2, $77, $1E, $1E, $60, $1D, $29, $1B, $2F, $0B, $AC, $1A, $ED, $61,
    $9B, $18, $AB, $DF, $FF, $0B, $FD, $FF, $C2, $19, $69, $B5, $F5, $12, $35, $F2,
    $C8, $13, $F7, $98, $FF, $11, $4F, $A6, $10, $73, $4C, $91, $15, $3C, $5A, $14,
    $FF, $FF, $FF, $FF, $14, $FE, $30, $23, $16, $B8, $8E, $7A, $17, $7A, $E4, $4D,
    $38, $4D, $46, $E0, $39, $8F, $2C, $D7, $3B, $C9, $92, $8E, $3A, $0B, $F8, $B9,
    $3F, $44, $EE, $3C, $FF, $FF, $FF, $FF, $3E, $86, $84, $0B, $3C, $C0, $3A, $52,
    $3D, $02, $50, $65, $36, $5E, $17, $58, $37, $9C, $7D, $6F, $35, $DA, $C3, $36,
    $34, $18, $A9, $01, $31, $57, $BF, $84, $95, $7E, $81, $FF, $30, $95, $D5, $B3,
    $32, $D3, $6B, $EA, $33, $50, $DD, $24, $6B, $E5, $90, $DB, $FF, $FF, $1B, $1B,
    $93, $27, $EA, $FE, $26, $2D, $5B, $C9, $23, $62, $4D, $4C, $22, $A0, $27, $7B,
    $20, $E6, $99, $22, $FF, $FF, $05, $FE, $21, $24, $F3, $15, $2A, $78, $B4, $FB,
    $BA, $DE, $1F, $29, $FC, $60, $46, $28, $3E, $0A, $71, $2D, $71, $1C, $F4, $2C,
    $FF, $FF, $FF, $FF, $B3, $76, $C3, $2E, $F5, $C8, $9A, $2F, $37, $A2, $AD, $70,
    $9A, $8D, $C0, $71, $58, $E7, $F7, $73, $1E, $59, $AE, $72, $DC, $33, $99, $77,
    $93, $25, $1C, $76, $FF, $FF, $5B, $FD, $51, $4F, $2B, $74, $17, $F1, $C0, $D5,
    $9B, $45, $7E, $89, $DC, $78, $7F, $4B, $B6, $4F, $7D, $0D, $08, $16, $7C, $CF,
    $FD, $FF, $46, $FF, $62, $21, $79, $80, $74, $A4, $78, $42, $20, $7A, $04, $A0,
    $CA, $7B, $C6, $CA, $FD, $6C, $BC, $2E, $B0, $6D, $7F, $E1, $D6, $6F, $3E, $87,
    $6F, $38, $FA, $DE, $13, $90, $E9, $6D, $86, $6C, $6A, $77, $EC, $5B, $FF, $6F,
    $FC, $FF, $68, $31, $52, $02, $69, $F3, $38, $35, $62, $AF, $7F, $08, $63, $6D,
    $3C, $61, $2B, $AB, $66, $60, $E9, $C1, $51, $65, $A6, $D7, $5E, $FA, $FF, $FF,
    $D4, $64, $64, $BD, $E3, $66, $22, $03, $BA, $67, $E0, $69, $8D, $48, $D7, $CB,
    $20, $49, $15, $A1, $17, $B7, $1F, $4E, $4A, $FF, $FF, $6F, $B5, $B2, $8D, $DE,
    $63, $FC, $4E, $1C, $09, $CB, $4C, $5A, $B7, $92, $4D, $98, $DD, $A5, $46, $C4,
    $9A, $98, $E2, $FF, $FF, $FF, $47, $06, $F0, $AF, $45, $40, $4E, $F6, $44, $82,
    $24, $C1, $41, $CD, $32, $44, $40, $0F, $58, $73, $42, $49, $E6, $2A, $43, $8B,
    $8C, $FF, $FF, $17, $0A, $CC, $68, $50, $55, $33, $02, $67, $57, $75, $BC, $3E,
    $56, $B7, $D6, $09, $53, $F8, $C0, $8C, $FF, $FF, $FF, $FF, $52, $3A, $AA, $BB,
    $50, $7C, $14, $E2, $51, $BE, $7E, $D5, $5A, $E2, $39, $E8, $5B, $20, $53, $DF,
    $59, $66, $ED, $86, $58, $A4, $87, $B1, $5D, $EB, $91, $34, $E3, $FF, $FF, $FF,
    $5C, $29, $FB, $03, $5E, $6F, $45, $5A, $5F, $AD, $2F, $6D, $E1, $35, $1B, $80,
    $E0, $F7, $71, $B7, $E2, $B1, $CF, $EE, $E3, $73, $A5, $52, $FF, $17, $96, $78,
    $3C, $B3, $5C, $B4, $D9, $55, $B8, $67, $32, $E4, $7A, $0D, $05, $EF, $26, $4A,
    $FF, $FF, $FF, $FF, $38, $EE, $E4, $20, $0F, $EC, $A2, $9E, $56, $ED, $60, $F4,
    $61, $E8, $2F, $E2, $E4, $E9, $ED, $88, $D3, $EB, $AB, $36, $8A, $EA, $69, $5C,
    $BD, $FD, $13, $B8, $BF, $C4, $06, $FF, $F0, $FC, $D1, $D2, $C7, $FE, $97, $6C,
    $D8, $55, $3B, $FA, $1A, $10, $2C, $FB, $FF, $FF, $FF, $FF, $D8, $7A, $1B, $F9,
    $9E, $C4, $42, $F8, $5C, $AE, $75, $F3, $00, $E9, $48, $F2, $C2, $83, $7F, $F0,
    $84, $3D, $26, $F1, $46, $57, $11, $F4, $09, $41, $94, $F5, $FA, $BF, $D4, $FF,
    $CB, $2B, $A3, $F7, $8D, $95, $FA, $F6, $4F, $FF, $AE, $78, $5D, $60, $D8, $BA,
    $37, $57, $DA, $FC, $89, $FF, $FF, $FF, $37, $6A, $3E, $E3, $39, $DE, $71, $F5,
    $BC, $DF, $B3, $9F, $8B, $DD, $F5, $21, $D2, $DC, $37, $4B, $E5, $D7, $6B, $0C,
    $D8, $D6, $A9, $66, $DF, $E8, $FF, $FF, $EF, $D4, $EF, $D8, $B6, $D5, $2D, $B2,
    $81, $D0, $62, $A4, $04, $D1, $A0, $CE, $33, $D3, $E6, $A5, $D2, $24, $1A, $5D,
    $FF, $FF, $FF, $FF, $C5, $5E, $FE, $10, $C4, $9C, $94, $27, $C6, $DA, $2A, $7E,
    $C7, $18, $40, $49, $C2, $57, $56, $CC, $C3, $95, $3C, $FB, $C1, $D3, $82, $A2,
    $C0, $11, $E8, $95, $FF, $FF, $C2, $FF, $CB, $4D, $AF, $A8, $CA, $8F, $C5, $9F,
    $C8, $C9, $AD, $C9, $0B, $11, $F1, $CC, $44, $07, $74, $CD, $86, $6D, $43, $CF,
    $C0, $D3, $FF, $5F, $2A, $F4, $1A, $CE, $02, $B9, $C7, $F0, $40, $90, $6D, $FC,
    $77, $92, $2B, $42, $2E, $93, $E9, $28, $FF, $6F, $FF, $FF, $19, $96, $A6, $3E,
    $9C, $97, $64, $54, $AB, $95, $22, $EA, $F2, $94, $E0, $80, $38, $BC, $C7, $F8,
    $9E, $7E, $AD, $CF, $9C, $38, $13, $96, $FF, $37, $6E, $F0, $9D, $FA, $79, $A1,
    $D6, $6F, $24, $24, $05, $13, $9B, $31, $BB, $4A, $9A, $F3, $D1, $7D, $A2, $E0,
    $FF, $FF, $8D, $89, $35, $30, $8C, $4B, $5F, $07, $8E, $0D, $E1, $5E, $8F, $CF,
    $8B, $69, $8A, $80, $9D, $FF, $FF, $FF, $17, $9D, $F7, $DB, $89, $04, $49, $82,
    $88, $C6, $23, $B5, $83, $9A, $64, $88, $82, $58, $0E, $BF, $80, $1E, $B0, $E6,
    $81, $DC, $DA, $D1, $FF, $FF, $FF, $FF, $84, $93, $CC, $54, $85, $51, $A6, $63,
    $87, $17, $18, $3A, $86, $D5, $72, $0D, $A9, $E2, $D0, $A0, $A8, $20, $BA, $97,
    $AA, $66, $04, $CE, $AB, $A4, $6E, $F9, $EA, $FF, $FF, $FF, $AE, $EB, $78, $7C,
    $AF, $29, $12, $4B, $AD, $6F, $AC, $12, $AC, $AD, $C6, $25, $A7, $F1, $81, $18,
    $A6, $33, $EB, $2F, $A4, $75, $55, $FF, $FF, $FF, $DF, $CD, $B7, $3F, $41, $A0,
    $F8, $29, $C4, $A1, $3A, $43, $F3, $A3, $7C, $FD, $AA, $A2, $BE, $97, $9D, $B5,
    $C4, $73, $D0, $B4, $06, $19, $E7, $B6, $FF, $FF, $FF, $FF, $40, $A7, $BE, $B7,
    $82, $CD, $89, $B2, $CD, $DB, $0C, $B3, $0F, $B1, $3B, $B1, $49, $0F, $62, $B0,
    $8B, $65, $55, $BB, $D7, $22, $68, $BA, $15, $48, $5F, $B8, $37, $F8, $17, $F8,
    $53, $F6, $06, $B9, $91, $65, $BC, $DE, $8A, $B4, $BD, $1C, $E0, $DB, $5A, $5E,
    $10, $C4, $7F, $FA, $DA, $BE, $98, $34, $ED, $FF, $B8, $BC, $67, $65, $AA, $09,
    $A5, $FF, $FF, $2F, $65, $B5, $AF, $EE, $8F, $62, $97, $57, $37, $DE, $F0, $32,
    $25, $6B, $5F, $DC, $9D, $D7, $38, $B9, $C5, $3E, $FF, $6F, $C1, $FF, $EF, $7D,
    $08, $4F, $8A, $6F, $BD, $E0, $64, $D7, $00, $4A, $D6, $BF, $B8, $F2, $6A, $D8,
    $DD, $E0, $DF, $77, $FF, $B7, $88, $E2, $33, $58, $63, $92, $19, $57, $9F, $E8,
    $A5, $30, $FA, $FA, $10, $9F, $FF, $FF, $BF, $C0, $14, $42, $B5, $71, $DF, $7B,
    $C0, $C8, $67, $C7, $A7, $AD, $75, $72, $08, $43, $CD, $CE, $6F, $26, $95, $AD,
    $7F, $70, $BF, $C0, $7F, $E9, $2D, $11, $18, $00, $A4, $B7, $FB, $87, $18, $D0,
    $9E, $1A, $A4, $27, $A2, $73, $8F, $42, $FF, $FF, $FF, $FF, $B0, $C6, $20, $AC,
    $08, $7A, $47, $C9, $A0, $32, $AF, $3E, $18, $8E, $C8, $5B, $0A, $3B, $67, $B5,
    $B2, $87, $00, $D0, $2F, $50, $38, $69, $97, $EC, $5F, $0C, $ED, $DF, $E0, $FF,
    $85, $59, $F0, $E2, $3D, $E5, $97, $87, $65, $86, $87, $33, $3A, $E0, $B4, $CF,
    $8F, $4F, $5A, $71, $FF, $6F, $F4, $FF, $28, $3F, $EA, $E4, $10, $86, $52, $58,
    $77, $E3, $40, $ED, $19, $F8, $51, $BF, $68, $F0, $2B, $F8, $A1, $48, $97, $9F,
    $C2, $C2, $FF, $FF, $C4, $5A, $22, $30, $2A, $E2, $9E, $57, $4F, $7F, $49, $6F,
    $F6, $C7, $F5, $08, $93, $D5, $0F, $7D, $D6, $FD, $FF, $05, $FE, $C0, $18, $35,
    $9F, $D0, $4E, $CE, $B7, $2B, $9F, $96, $18, $C5, $27, $2A, $7F, $A0, $BA, $FD,
    $47, $19, $E2, $BF, $D5, $4B, $2E, $20, $7C, $10, $E2, $92, $A8, $48, $E8, $F7,
    $9B, $14, $58, $FF, $BF, $C5, $5F, $86, $A8, $3F, $58, $31, $1D, $90, $33, $A1,
    $F7, $D3, $14, $76, $CF, $6A, $AC, $CA, $A8, $0F, $BE, $7F, $DF, $E8, $0D, $FE,
    $07, $E1, $06, $C3, $60, $84, $5E, $07, $D2, $E6, $1C, $AD, $F4, $A9, $B8, $59,
    $FF, $A5, $2F, $F4, $4C, $15, $DF, $3C, $0B, $E7, $85, $69, $7E, $32, $7B, $CB,
    $2F, $0E, $C3, $77, $48, $6B, $FF, $4B, $FD, $ED, $CB, $0D, $0F, $CE, $B1, $68,
    $C7, $61, $04, $C7, $66, $B8, $A0, $4C, $44, $6F, $98, $F5, $FC, $D3, $FF, $FF,
    $17, $F8, $FF, $90, $EE, $66, $50, $F4, $DA, $37, $1B, $0E, $B9, $27, $4D, $B6,
    $05, $40, $28, $A4, $B0, $EF, $C6, $1C, $0C, $88, $DE, $F8, $FF, $FF, $A3, $81,
    $DB, $B0, $1A, $39, $67, $D7, $7F, $2B, $D2, $78, $91, $93, $6E, $1F, $F4, $3B,
    $26, $F7, $03, $23, $90, $66, $91, $DF, $E0, $FF, $25, $C8, $88, $29, $93, $58,
    $ED, $B4, $44, $60, $54, $0C, $F8, $6E, $1E, $4D, $A8, $DF, $FF, $FF, $BF, $D5,
    $A6, $F1, $C5, $FE, $92, $DF, $EC, $46, $2E, $B8, $89, $54, $9B, $17, $67, $EC,
    $27, $70, $02, $71, $F0, $48, $BB, $C9, $FF, $FF, $FF, $FF, $4C, $2F, $DE, $DB,
    $F9, $80, $30, $63, $45, $E7, $55, $6B, $3F, $A0, $9C, $D3, $83, $C7, $F9, $C1,
    $36, $68, $17, $79, $8A, $0F, $72, $E4, $5D, $37, $CB, $5C, $FD, $FF, $12, $FF,
    $E1, $50, $AE, $4E, $54, $FF, $40, $F6, $92, $25, $AE, $8B, $88, $73, $16, $37,
    $EF, $16, $04, $82, $40, $F8, $37, $F8, $C6, $6F, $21, $27, $9D, $21, $E9, $1F,
    $BF, $55, $78, $41, $8B, $EC, $AF, $33, $F8, $FF, $F6, $FF, $5C, $B0, $CA, $ED,
    $59, $B6, $3B, $55, $E5, $D1, $5E, $47, $AE, $B0, $FF, $EC, $19, $D5, $62, $3B,
    $21, $6C, $DA, $87, $46, $0B, $FF, $FF, $DF, $0D, $32, $E9, $E7, $70, $8E, $8E,
    $82, $28, $ED, $9E, $D4, $90, $51, $F9, $B1, $82, $E4, $56, $5F, $3A, $25, $3A,
    $FB, $FF, $FF, $42, $05, $09, $83, $1F, $33, $6E, $E6, $0D, $86, $C1, $08, $B5,
    $3A, $A6, $6D, $BD, $40, $E1, $A4, $05, $FC, $0C, $FE, $FF, $5B, $7F, $17, $49,
    $29, $2F, $AF, $F5, $C2, $32, $22, $76, $F3, $8A, $9E, $11, $96, $98, $2B, $BE,
    $78, $20, $97, $D9, $1D, $FE, $FF, $FF, $8D, $17, $C9, $4B, $C0, $48, $AE, $2E,
    $D2, $FD, $01, $C0, $6A, $41, $66, $A5, $F7, $96, $5E, $1C, $4F, $2A, $39, $79,
    $5D, $FF, $FF, $FF, $17, $90, $97, $E5, $23, $F1, $F2, $4D, $6B, $19, $05, $F5,
    $D7, $7E, $60, $E7, $62, $D1, $8E, $5F, $DE, $B6, $EB, $C2, $09, $8E, $52, $7A,
    $7F, $0B, $4A, $FC, $B5, $E9, $37, $68, $00, $46, $1C, $00, $88, $DF, $31, $EA,
    $30, $63, $BF, $C5, $E5, $FF, $56, $8F, $22, $D6, $F9, $61, $9A, $6A, $9E, $04,
    $07, $BF, $01, $8A, $AD, $B4, $6E, $36, $15, $FF, $FF, $FF, $AD, $08, $EE, $1D,
    $72, $4E, $9A, $A5, $CE, $29, $FF, $B7, $7B, $86, $11, $0F, $C7, $E1, $74, $92,
    $10, $D9, $CD, $2A, $AC, $BE, $A8, $FF, $FF, $FF, $FF, $38, $19, $11, $46, $80,
    $A5, $76, $23, $D8, $C6, $66, $75, $60, $7A, $01, $10, $72, $CF, $AE, $FE, $CA,
    $73, $C9, $9B, $57, $A4, $F1, $22, $EF, $18, $96, $47, $FF, $FF, $FF, $FF, $FD,
    $AD, $39, $A9, $45, $11, $5E, $CC, $76, $4D, $EE, $06, $CE, $F1, $89, $63, $DC,
    $44, $26, $8D, $64, $F8, $41, $E8, $F9, $2F, $79, $51, $41, $93, $1E, $34, $FE,
    $FF, $6F, $FC, $53, $26, $B1, $DA, $EB, $9A, $EC, $B3, $F9, $C6, $E9, $0B, $45,
    $A1, $8C, $19, $F0, $0E, $62, $A1, $4C, $69, $07, $3C, $9B, $FF, $FF, $FF, $05,
    $55, $84, $27, $36, $DB, $96, $92, $99, $35, $2E, $2E, $FE, $50, $26, $54, $B9,
    $99, $9E, $E8, $DE, $FC, $8C, $5D, $71, $12, $FF, $FF, $6F, $F4, $34, $E1, $16,
    $77, $E3, $2E, $CE, $11, $8A, $49, $AB, $03, $3F, $E6, $45, $BB, $83, $81, $20,
    $E3, $E0, $91, $76, $5B, $FF, $FF, $FF, $FF, $5C, $F6, $13, $49, $E9, $59, $FD,
    $F1, $55, $3E, $98, $6C, $82, $06, $21, $D4, $3E, $61, $44, $C6, $8B, $CE, $AA,
    $7E, $37, $A9, $CF, $D6, $7F, $41, $38, $6E, $FF, $5B, $FC, $FF, $C3, $26, $5D,
    $7C, $76, $89, $B3, $C4, $CA, $EE, $D6, $59, $1D, $D6, $F7, $A1, $B1, $0A, $F3,
    $14, $1E, $E4, $4B, $A8, $C1, $FF, $FF, $FF, $79, $81, $13, $CB, $69, $D7, $AB,
    $77, $0E, $B2, $B9, $C2, $A1, $5C, $01, $7E, $C6, $39, $9C, $A9, $FE, $80, $24,
    $15, $99, $E5, $FF, $FF, $FF, $2F, $FE, $0B, $8E, $1C, $51, $6E, $86, $66, $16,
    $A7, $3E, $DA, $71, $C2, $2C, $6F, $DE, $2C, $94, $D3, $B9, $49, $09, $04, $81,
    $F0, $B1, $B8, $FF, $FF, $FF, $FF, $E6, $95, $A3, $0D, $49, $7B, $1B, $B1, $2E,
    $1E, $43, $D2, $3E, $48, $FB, $6E, $59, $2D, $E9, $DB, $F6, $C3, $51, $67, $91,
    $A6, $CC, $B0, $A9, $1F, $74, $0C, $6F, $FF, $FF, $FF, $CE, $7A, $66, $B9, $61,
    $94, $DE, $05, $06, $F1, $2D, $4C, $49, $42, $47, $43, $43, $57, $33, $32, $2D,
    $45, $48, $2D, $04, $53, $4A, $4C, $E5, $4A, $00, $FF, $4A, $2D, $47, $54, $48,
    $52, $2D, $4D, $38, $16, $DB, $EC, $45, $D0, $1F, $2F, $BE, $05, $06, $6D, $05,
    $DC, $6D, $40, $C8, $77, $2E, $33, $2E, $34, $AD, $D4, $AD, $A2, $4B, $31, $34,
    $30, $94, $F5, $8A, $96, $AA, $B3, $31, $1F, $5F, $72, $2F, $DF, $43, $CE, $6E,
    $80, $67, $63, $63, $22, $2D, $2D, $31, $14, $2F, $FA, $FD, $2F, $55, $83, $66,
    $69, $67, $2F, $69, $33, $38, $36, $2F, $47, $73, $68, $5B, $8A, $7E, $A1, $3E,
    $64, $2D, $70, $74, $72, $AF, $00, $11, $BF, $A0, $DD, $A4, $5F, $10, $2D, $3E,
    $16, $3D, $3D, $BF, $74, $7F, $41, $1E, $6F, $66, $28, $A6, $5F, $5F, $53, $48,
    $41, $52, $37, $00, $0E, $D4, $45, $44, $79, $77, $41, $74, $5A, $54, $07, $C0,
    $6F, $6D, $76, $41, $EA, $7A, $D8, $16, $E4, $0A, $4B, $2C, $2F, $73, $29, $00,
    $8B, $E2, $21, $4A, $30, $0D, $40, $64, $85, $20, $27, $A2, $22, $24, $05, $05,
    $44, $60, $23, $13, $95, $4C, $C9, $13, $04, $15, $02, $E4, $4D, $B7, $9C, $B0,
    $1F, $2C, $12, $38, $03, $48, $54, $66, $B2, $D2, $34, $64, $74, $1F, $94, $9A,
    $A6, $69, $BA, $A4, $23, $B4, $C4, $D4, $E8, $F4, $69, $9A, $66, $7B, $13, $03,
    $0C, $14, $20, $2C, $A6, $69, $9A, $A6, $38, $44, $54, $5C, $64, $9A, $A6, $69,
    $9A, $70, $7C, $84, $8C, $98, $A4, $69, $9A, $A6, $69, $AC, $B4, $BC, $C8, $D0,
    $D3, $34, $DD, $A6, $D8, $E0, $13, $63, $13, $13, $14, $9A, $A6, $69, $3A, $03,
    $18, $20, $2C, $38, $44, $69, $9A, $A6, $69, $50, $5C, $68, $74, $80, $A6, $69,
    $9A, $A6, $8C, $98, $A4, $B0, $BC, $05, $59, $69, $9A, $C8, $D4, $E0, $F7, $32,
    $99, $04, $19, $85, $9D, $C9, $64, $00, $01, $D2, $4E, $00, $10, $02, $03, $04,
    $59, $1B, $B2, $00, $14, $13, $03, $24, $80, $42, $4A, $BB, $D1, $10, $11, $65,
    $C4, $58, $D1, $2A, $60, $D9, $EF, $38, $01, $01, $01, $22, $F8, $D6, $55, $48,
    $41, $09, $45, $78, $69, $74, $DE, $4C, $11, $94, $55, $68, $0C, $46, $69, $6E,
    $6C, $59, $15, $C3, $17, $6A, $0D, $6B, $6F, $51, $F0, $53, $79, $4A, $6D, $54,
    $69, $0F, $0E, $7B, $80, $E2, $65, $53, $55, $6E, $68, $82, $64, $40, $6C, $A7,
    $20, $5A, $3B, $22, $3C, $6C, $FD, $74, $1F, $11, $FE, $65, $64, $58, $5F, $5F,
    $67, $CF, $45, $44, $BB, $27, $6D, $61, $53, $35, $0E, $EF, $56, $88, $E0, $6D,
    $62, $5F, $27, $06, $61, $78, $0D, $C1, $60, $DB, $B7, $70, $02, $65, $6E, $76,
    $69, $7B, $6E, $0D, $66, $B4, $97, $CF, $40, $63, $36, $73, $5F, $61, $70, $11,
    $5B, $EB, $0C, $45, $2C, $0F, $61, $9A, $57, $74, $99, $EF, $5C, $73, $08, $68,
    $AC, $07, $65, $72, $72, $6E, $6F, $B0, $16, $EC, $B7, $74, $62, $05, $73, $63,
    $28, $49, $20, $7E, $30, $37, $6C, $70, $11, $49, $54, $76, $73, $6E, $70, $72,
    $6F, $FB, $BD, $DD, $8A, $74, $66, $0B, $61, $62, $6F, $4E, $61, $74, $C0, $32,
    $06, $BA, $53, $04, $67, $34, $63, $E8, $07, $65, $09, $82, $B3, $D6, $C7, $05,
    $13, $66, $10, $CD, $39, $F3, $DD, $07, $66, $6C, $75, $73, $68, $D0, $22, $0D,
    $39, $53, $05, $E6, $6F, $6F, $72, $0F, $06, $31, $53, $05, $87, $57, $66, $5A,
    $06, $AE, $35, $77, $7B, $32, $37, $65, $6B, $06, $5D, $56, $06, $CD, $3A, $43,
    $05, $C9, $15, $6D, $64, $E4, $E6, $22, $BF, $07, $65, $6D, $63, $70, $79, $A2,
    $92, $71, $73, $4E, $50, $CC, $5C, $40, $1F, $B9, $17, $14, $6E, $E7, $2F, $1C,
    $62, $75, $5A, $02, $B6, $30, $CC, $35, $1A, $07, $6B, $8B, $72, $B0, $20, $F2,
    $E4, $D9, $74, $07, $6D, $70, $70, $79, $BA, $F7, $45, $28, $DA, $09, $6C, $9E,
    $07, $6E, $57, $98, $6D, $C3, $1A, $23, $68, $19, $D9, $4C, $EC, $F9, $6B, $3B,
    $04, $6F, $77, $2D, $08, $75, $70, $70, $76, $CB, $97, $27, $34, $9B, $76, $08,
    $87, $50, $45, $4C, $01, $42, $81, $80, $FF, $06, $00, $3C, $3F, $A8, $46, $00,
    $98, $01, $26, $E5, $00, $B8, $BD, $FC, $E0, $00, $07, $03, $0B, $38, $00, $36,
    $0F, $00, $5E, $76, $6F, $B9, $B4, $7F, $D0, $11, $10, $30, $13, $00, $40, $69,
    $9A, $85, $CE, $0B, $02, $D6, $04, $01, $04, $D9, $99, $01, $0E, $20, $8A, $0A,
    $03, $10, $0B, $96, $6C, $B0, $0F, $10, $17, $06, $2F, $55, $64, $74, $39, $10,
    $F0, $05, $13, $7D, $B3, $5F, $C7, $2E, $1A, $74, $EB, $34, $01, $90, $EB, $04,
    $7D, $8E, $BB, $85, $23, $B9, $60, $2E, $64, $4B, $A6, $01, $99, $90, $9B, $9B,
    $FB, $F3, $27, $3A, $01, $40, $CE, $22, $40, $70, $C0, $2E, $7C, $42, $60, $C1,
    $06, $90, $C1, $27, $3C, $72, $50, $A7, $A4, $69, $BA, $54, $27, $70, $54, $3E,
    $40, $2E, $16, $D2, $7D, $A6, $62, $B4, $00, $37, $1F, $D0, $38, $4C, $9E, $C3,
    $4F, $80, $69, $43, $D8, $4A, $BA, $27, $02, $06, $77, $92, $1B, $00, $80, $52,
    $BC, $4E, $AE, $02, $00, $00, $00, $00, $00, $00, $40, $FF, $00, $00, $00, $00,
    $60, $BE, $15, $60, $41, $00, $8D, $BE, $EB, $AF, $FE, $FF, $57, $83, $CD, $FF,
    $EB, $10, $90, $90, $90, $90, $90, $90, $8A, $06, $46, $88, $07, $47, $01, $DB,
    $75, $07, $8B, $1E, $83, $EE, $FC, $11, $DB, $72, $ED, $B8, $01, $00, $00, $00,
    $01, $DB, $75, $07, $8B, $1E, $83, $EE, $FC, $11, $DB, $11, $C0, $01, $DB, $73,
    $EF, $75, $09, $8B, $1E, $83, $EE, $FC, $11, $DB, $73, $E4, $31, $C9, $83, $E8,
    $03, $72, $0D, $C1, $E0, $08, $8A, $06, $46, $83, $F0, $FF, $74, $74, $89, $C5,
    $01, $DB, $75, $07, $8B, $1E, $83, $EE, $FC, $11, $DB, $11, $C9, $01, $DB, $75,
    $07, $8B, $1E, $83, $EE, $FC, $11, $DB, $11, $C9, $75, $20, $41, $01, $DB, $75,
    $07, $8B, $1E, $83, $EE, $FC, $11, $DB, $11, $C9, $01, $DB, $73, $EF, $75, $09,
    $8B, $1E, $83, $EE, $FC, $11, $DB, $73, $E4, $83, $C1, $02, $81, $FD, $00, $F3,
    $FF, $FF, $83, $D1, $01, $8D, $14, $2F, $83, $FD, $FC, $76, $0F, $8A, $02, $42,
    $88, $07, $47, $49, $75, $F7, $E9, $63, $FF, $FF, $FF, $90, $8B, $02, $83, $C2,
    $04, $89, $07, $83, $C7, $04, $83, $E9, $04, $77, $F1, $01, $CF, $E9, $4C, $FF,
    $FF, $FF, $5E, $89, $F7, $B9, $6F, $06, $00, $00, $8A, $07, $47, $2C, $E8, $3C,
    $01, $77, $F7, $80, $3F, $0A, $75, $F2, $8B, $07, $8A, $5F, $04, $66, $C1, $E8,
    $08, $C1, $C0, $10, $86, $C4, $29, $F8, $80, $EB, $E8, $01, $F0, $89, $07, $83,
    $C7, $05, $88, $D8, $E2, $D9, $8D, $BE, $00, $10, $02, $00, $8B, $07, $09, $C0,
    $74, $3C, $8B, $5F, $04, $8D, $84, $30, $00, $30, $02, $00, $01, $F3, $50, $83,
    $C7, $08, $FF, $96, $3C, $30, $02, $00, $95, $8A, $07, $47, $08, $C0, $74, $DC,
    $89, $F9, $57, $48, $F2, $AE, $55, $FF, $96, $40, $30, $02, $00, $09, $C0, $74,
    $07, $89, $03, $83, $C3, $04, $EB, $E1, $FF, $96, $48, $30, $02, $00, $8B, $AE,
    $44, $30, $02, $00, $8D, $BE, $00, $F0, $FF, $FF, $BB, $00, $10, $00, $00, $50,
    $54, $6A, $04, $53, $57, $FF, $D5, $8D, $87, $9F, $01, $00, $00, $80, $20, $7F,
    $80, $60, $28, $7F, $58, $50, $54, $50, $53, $57, $FF, $D5, $58, $61, $8D, $44,
    $24, $80, $6A, $00, $39, $C4, $75, $FA, $83, $EC, $80, $E9, $A0, $DE, $FD, $FF,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $58, $40, $02, $00,
    $3C, $40, $02, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $65, $40, $02, $00, $50, $40, $02, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $70, $40, $02, $00,
    $7E, $40, $02, $00, $8E, $40, $02, $00, $9E, $40, $02, $00, $00, $00, $00, $00,
    $AC, $40, $02, $00, $00, $00, $00, $00, $4B, $45, $52, $4E, $45, $4C, $33, $32,
    $2E, $44, $4C, $4C, $00, $6D, $73, $76, $63, $72, $74, $2E, $64, $6C, $6C, $00,
    $00, $00, $4C, $6F, $61, $64, $4C, $69, $62, $72, $61, $72, $79, $41, $00, $00,
    $47, $65, $74, $50, $72, $6F, $63, $41, $64, $64, $72, $65, $73, $73, $00, $00,
    $56, $69, $72, $74, $75, $61, $6C, $50, $72, $6F, $74, $65, $63, $74, $00, $00,
    $45, $78, $69, $74, $50, $72, $6F, $63, $65, $73, $73, $00, $00, $00, $5F, $69,
    $6F, $62, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $04, $00, $00, $00, $24, $02, $00, $00, $06, $00, $00, $00,
    $02, $00, $00, $00, $00, $00, $13, $00, $00, $00, $B4, $01, $00, $00, $06, $00,
    $00, $00, $02, $00, $00, $00, $00, $00, $20, $00, $00, $00, $88, $01, $00, $00,
    $06, $00, $00, $00, $02, $00, $00, $00, $00, $00, $30, $00, $00, $00, $80, $01,
    $00, $00, $06, $00, $00, $00, $02, $00, $00, $00, $00, $00, $3F, $00, $00, $00,
    $EC, $01, $00, $00, $06, $00, $00, $00, $02, $00, $00, $00, $00, $00, $4D, $00,
    $00, $00, $E4, $05, $00, $00, $06, $00, $00, $00, $02, $00, $00, $00, $00, $00,
    $61, $00, $00, $00, $40, $01, $00, $00, $06, $00, $00, $00, $02, $00, $00, $00,
    $00, $00, $74, $00, $00, $00, $90, $01, $00, $00, $06, $00, $00, $00, $02, $00,
    $00, $00, $00, $00, $81, $00, $00, $00, $5C, $01, $00, $00, $06, $00, $00, $00,
    $02, $00, $00, $00, $00, $00, $95, $00, $00, $00, $60, $01, $00, $00, $06, $00,
    $00, $00, $02, $00, $00, $00, $00, $00, $A9, $00, $00, $00, $84, $01, $00, $00,
    $06, $00, $00, $00, $02, $00, $00, $00, $00, $00, $B8, $00, $00, $00, $78, $01,
    $00, $00, $06, $00, $00, $00, $02, $00, $00, $00, $00, $00, $C4, $00, $00, $00,
    $7C, $01, $00, $00, $06, $00, $00, $00, $02, $00, $00, $00, $00, $00, $D4, $00,
    $00, $00, $94, $01, $00, $00, $06, $00, $00, $00, $02, $00, $00, $00, $00, $00,
    $E1, $00, $00, $00, $74, $01, $00, $00, $06, $00, $00, $00, $02, $00, $00, $00,
    $00, $00, $EF, $00, $00, $00, $F0, $01, $00, $00, $06, $00, $00, $00, $02, $00,
    $00, $00, $00, $00, $FD, $00, $00, $00, $98, $01, $00, $00, $06, $00, $00, $00,
    $02, $00, $00, $00, $00, $00, $0B, $01, $00, $00, $14, $00, $00, $00, $06, $00,
    $00, $00, $02, $00, $00, $00, $00, $00, $1E, $01, $00, $00, $A0, $01, $00, $00,
    $06, $00, $00, $00, $02, $00, $00, $00, $00, $00, $2A, $01, $00, $00, $A4, $01,
    $00, $00, $06, $00, $00, $00, $02, $00, $00, $00, $00, $00, $36, $01, $00, $00,
    $C8, $01, $00, $00, $06, $00, $00, $00, $02, $00, $00, $00, $00, $00, $43, $01,
    $00, $00, $1C, $02, $00, $00, $06, $00, $00, $00, $02, $00, $00, $00, $00, $00,
    $52, $01, $00, $00, $48, $01, $00, $00, $06, $00, $00, $00, $02, $00, $00, $00,
    $00, $00, $69, $01, $00, $00, $64, $01, $00, $00, $06, $00, $00, $00, $02, $00,
    $00, $00, $00, $00, $7B, $01, $00, $00, $E0, $01, $00, $00, $06, $00, $00, $00,
    $02, $00, $00, $00, $00, $00, $88, $01, $00, $00, $F4, $01, $00, $00, $06, $00,
    $00, $00, $02, $00, $00, $00, $00, $00, $97, $01, $00, $00, $44, $01, $00, $00,
    $06, $00, $00, $00, $02, $00, $00, $00, $00, $00, $AE, $01, $00, $00, $C0, $01,
    $00, $00, $06, $00, $00, $00, $02, $00, $00, $00, $00, $00, $BB, $01, $00, $00,
    $D8, $01, $00, $00, $06, $00, $00, $00, $02, $00, $00, $00, $00, $00, $C9, $01,
    $00, $00, $B0, $01, $00, $00, $06, $00, $00, $00, $02, $00, $00, $00, $00, $00,
    $D6, $01, $00, $00, $FC, $01, $00, $00, $06, $00, $00, $00, $02, $00, $00, $00,
    $00, $00, $E4, $01, $00, $00, $10, $02, $00, $00, $06, $00, $00, $00, $02, $00,
    $00, $00, $00, $00, $F3, $01, $00, $00, $D4, $01, $00, $00, $06, $00, $00, $00,
    $02, $00, $00, $00, $00, $00, $01, $02, $00, $00, $0C, $02, $00, $00, $06, $00,
    $00, $00, $02, $00, $00, $00, $00, $00, $10, $02, $00, $00, $E4, $01, $00, $00,
    $06, $00, $00, $00, $02, $00, $00, $00, $00, $00, $1F, $02, $00, $00, $6C, $01,
    $00, $00, $06, $00, $00, $00, $02, $00, $00, $00, $00, $00, $2E, $02, $00, $00,
    $BC, $01, $00, $00, $06, $00, $00, $00, $02, $00, $00, $00, $00, $00, $3D, $02,
    $00, $00, $DC, $01, $00, $00, $06, $00, $00, $00, $02, $00, $00, $00, $00, $00,
    $4B, $02, $00, $00, $A8, $01, $00, $00, $06, $00, $00, $00, $02, $00, $00, $00,
    $00, $00, $59, $02, $00, $00, $08, $02, $00, $00, $06, $00, $00, $00, $02, $00,
    $00, $00, $00, $00, $67, $02, $00, $00, $AC, $01, $00, $00, $06, $00, $00, $00,
    $02, $00, $00, $00, $00, $00, $75, $02, $00, $00, $14, $02, $00, $00, $06, $00,
    $00, $00, $02, $00, $00, $00, $00, $00, $83, $02, $00, $00, $00, $02, $00, $00,
    $06, $00, $00, $00, $02, $00, $00, $00, $00, $00, $91, $02, $00, $00, $9C, $01,
    $00, $00, $06, $00, $00, $00, $02, $00, $00, $00, $00, $00, $9F, $02, $00, $00,
    $B8, $01, $00, $00, $06, $00, $00, $00, $02, $00, $00, $00, $00, $00, $AC, $02,
    $00, $00, $58, $01, $00, $00, $06, $00, $00, $00, $02, $00, $00, $00, $00, $00,
    $C1, $02, $00, $00, $E8, $01, $00, $00, $06, $00, $00, $00, $02, $00, $00, $00,
    $00, $00, $CF, $02, $00, $00, $3C, $01, $00, $00, $06, $00, $00, $00, $02, $00,
    $00, $00, $00, $00, $E4, $02, $00, $00, $04, $02, $00, $00, $06, $00, $00, $00,
    $02, $00, $00, $00, $00, $00, $F4, $02, $00, $00, $C4, $01, $00, $00, $06, $00,
    $00, $00, $02, $00, $00, $00, $00, $00, $00, $03, $00, $00, $4C, $01, $00, $00,
    $06, $00, $00, $00, $02, $00, $00, $00, $00, $00, $25, $03, $00, $00, $18, $02,
    $00, $00, $06, $00, $00, $00, $02, $00, $00, $00, $00, $00, $34, $03, $00, $00,
    $CC, $01, $00, $00, $06, $00, $00, $00, $02, $00, $00, $00, $00, $00, $41, $03,
    $00, $00, $38, $01, $00, $00, $06, $00, $00, $00, $02, $00, $00, $00, $00, $00,
    $53, $03, $00, $00, $00, $00, $00, $00, $06, $00, $00, $00, $02, $00, $00, $00,
    $00, $00, $68, $03, $00, $00, $70, $01, $00, $00, $06, $00, $00, $00, $02, $00,
    $00, $00, $00, $00, $76, $03, $00, $00, $20, $02, $00, $00, $06, $00, $00, $00,
    $02, $00, $00, $00, $00, $00, $86, $03, $00, $00, $68, $01, $00, $00, $06, $00,
    $00, $00, $02, $00, $00, $00, $00, $00, $9C, $03, $00, $00, $04, $05, $00, $00,
    $06, $00, $00, $00, $02, $00, $00, $00, $00, $00, $B2, $03, $00, $00, $8C, $01,
    $00, $00, $06, $00, $00, $00, $02, $00, $00, $00, $00, $00, $C4, $03, $00, $00,
    $F8, $01, $00, $00, $06, $00, $00, $00, $02, $00, $00, $00, $00, $00, $D2, $03,
    $00, $00, $D0, $01, $00, $00, $06, $00, $00, $00, $02, $00, $E0, $03, $00, $00,
    $5F, $5F, $69, $6D, $70, $5F, $5F, $76, $70, $72, $69, $6E, $74, $66, $00, $5F,
    $5F, $69, $6D, $70, $5F, $5F, $66, $6C, $6F, $6F, $72, $00, $5F, $5F, $69, $6D,
    $70, $5F, $5F, $5F, $73, $65, $74, $6D, $6F, $64, $65, $00, $5F, $5F, $69, $6D,
    $70, $5F, $5F, $5F, $6F, $6E, $65, $78, $69, $74, $00, $5F, $5F, $69, $6D, $70,
    $5F, $5F, $73, $65, $74, $62, $75, $66, $00, $5F, $5F, $6C, $69, $62, $6D, $73,
    $76, $63, $72, $74, $5F, $61, $5F, $69, $6E, $61, $6D, $65, $00, $5F, $5F, $69,
    $6D, $70, $5F, $5F, $46, $69, $6E, $64, $41, $74, $6F, $6D, $41, $40, $34, $00,
    $5F, $5F, $69, $6D, $70, $5F, $5F, $61, $62, $6F, $72, $74, $00, $5F, $5F, $69,
    $6D, $70, $5F, $5F, $5F, $5F, $6D, $62, $5F, $63, $75, $72, $5F, $6D, $61, $78,
    $00, $5F, $5F, $69, $6D, $70, $5F, $5F, $5F, $5F, $70, $5F, $5F, $65, $6E, $76,
    $69, $72, $6F, $6E, $00, $5F, $5F, $69, $6D, $70, $5F, $5F, $5F, $70, $63, $74,
    $79, $70, $65, $00, $5F, $5F, $69, $6D, $70, $5F, $5F, $5F, $69, $6F, $62, $00,
    $5F, $5F, $69, $6D, $70, $5F, $5F, $5F, $69, $73, $63, $74, $79, $70, $65, $00,
    $5F, $5F, $69, $6D, $70, $5F, $5F, $61, $74, $61, $6E, $32, $00, $5F, $5F, $69,
    $6D, $70, $5F, $5F, $5F, $65, $72, $72, $6E, $6F, $00, $5F, $5F, $69, $6D, $70,
    $5F, $5F, $73, $69, $67, $6E, $61, $6C, $00, $5F, $5F, $69, $6D, $70, $5F, $5F,
    $61, $74, $65, $78, $69, $74, $00, $5F, $5F, $68, $65, $61, $64, $5F, $6C, $69,
    $62, $6D, $73, $76, $63, $72, $74, $5F, $61, $00, $5F, $5F, $69, $6D, $70, $5F,
    $5F, $63, $65, $69, $6C, $00, $5F, $5F, $69, $6D, $70, $5F, $5F, $65, $78, $69,
    $74, $00, $5F, $5F, $69, $6D, $70, $5F, $5F, $66, $73, $65, $65, $6B, $00, $5F,
    $5F, $69, $6D, $70, $5F, $5F, $74, $6F, $75, $70, $70, $65, $72, $00, $5F, $5F,
    $69, $6D, $70, $5F, $5F, $47, $65, $74, $53, $79, $73, $74, $65, $6D, $54, $69,
    $6D, $65, $40, $34, $00, $5F, $5F, $69, $6D, $70, $5F, $5F, $5F, $5F, $70, $5F,
    $5F, $66, $6D, $6F, $64, $65, $00, $5F, $5F, $69, $6D, $70, $5F, $5F, $71, $73,
    $6F, $72, $74, $00, $5F, $5F, $69, $6D, $70, $5F, $5F, $73, $70, $72, $69, $6E,
    $74, $66, $00, $5F, $5F, $69, $6D, $70, $5F, $5F, $47, $65, $74, $41, $74, $6F,
    $6D, $4E, $61, $6D, $65, $41, $40, $31, $32, $00, $5F, $5F, $69, $6D, $70, $5F,
    $5F, $66, $72, $65, $61, $64, $00, $5F, $5F, $69, $6D, $70, $5F, $5F, $6D, $65,
    $6D, $63, $70, $79, $00, $5F, $5F, $69, $6D, $70, $5F, $5F, $66, $67, $65, $74,
    $63, $00, $5F, $5F, $69, $6D, $70, $5F, $5F, $73, $74, $72, $63, $6D, $70, $00,
    $5F, $5F, $69, $6D, $70, $5F, $5F, $73, $74, $72, $72, $63, $68, $72, $00, $5F,
    $5F, $69, $6D, $70, $5F, $5F, $6D, $61, $6C, $6C, $6F, $63, $00, $5F, $5F, $69,
    $6D, $70, $5F, $5F, $73, $74, $72, $6E, $63, $70, $79, $00, $5F, $5F, $69, $6D,
    $70, $5F, $5F, $72, $65, $61, $6C, $6C, $6F, $63, $00, $5F, $5F, $69, $6D, $70,
    $5F, $5F, $5F, $61, $73, $73, $65, $72, $74, $00, $5F, $5F, $69, $6D, $70, $5F,
    $5F, $66, $70, $72, $69, $6E, $74, $66, $00, $5F, $5F, $69, $6D, $70, $5F, $5F,
    $6D, $65, $6D, $73, $65, $74, $00, $5F, $5F, $69, $6D, $70, $5F, $5F, $66, $63,
    $6C, $6F, $73, $65, $00, $5F, $5F, $69, $6D, $70, $5F, $5F, $73, $74, $72, $6C,
    $65, $6E, $00, $5F, $5F, $69, $6D, $70, $5F, $5F, $66, $66, $6C, $75, $73, $68,
    $00, $5F, $5F, $69, $6D, $70, $5F, $5F, $73, $74, $72, $74, $6F, $6C, $00, $5F,
    $5F, $69, $6D, $70, $5F, $5F, $73, $74, $72, $63, $70, $79, $00, $5F, $5F, $69,
    $6D, $70, $5F, $5F, $63, $61, $6C, $6C, $6F, $63, $00, $5F, $5F, $69, $6D, $70,
    $5F, $5F, $66, $6F, $70, $65, $6E, $00, $5F, $5F, $69, $6D, $70, $5F, $5F, $5F,
    $5F, $67, $65, $74, $6D, $61, $69, $6E, $61, $72, $67, $73, $00, $5F, $5F, $69,
    $6D, $70, $5F, $5F, $72, $65, $6D, $6F, $76, $65, $00, $5F, $5F, $69, $6D, $70,
    $5F, $5F, $45, $78, $69, $74, $50, $72, $6F, $63, $65, $73, $73, $40, $34, $00,
    $5F, $5F, $69, $6D, $70, $5F, $5F, $73, $74, $72, $65, $72, $72, $6F, $72, $00,
    $5F, $5F, $69, $6D, $70, $5F, $5F, $66, $72, $65, $65, $00, $5F, $5F, $69, $6D,
    $70, $5F, $5F, $53, $65, $74, $55, $6E, $68, $61, $6E, $64, $6C, $65, $64, $45,
    $78, $63, $65, $70, $74, $69, $6F, $6E, $46, $69, $6C, $74, $65, $72, $40, $34,
    $00, $5F, $5F, $69, $6D, $70, $5F, $5F, $74, $6F, $6C, $6F, $77, $65, $72, $00,
    $5F, $5F, $69, $6D, $70, $5F, $5F, $66, $74, $65, $6C, $6C, $00, $5F, $5F, $69,
    $6D, $70, $5F, $5F, $41, $64, $64, $41, $74, $6F, $6D, $41, $40, $34, $00, $5F,
    $5F, $68, $65, $61, $64, $5F, $6C, $69, $62, $6B, $65, $72, $6E, $65, $6C, $33,
    $32, $5F, $61, $00, $5F, $5F, $69, $6D, $70, $5F, $5F, $5F, $63, $65, $78, $69,
    $74, $00, $5F, $5F, $69, $6D, $70, $5F, $5F, $76, $66, $70, $72, $69, $6E, $74,
    $66, $00, $5F, $5F, $69, $6D, $70, $5F, $5F, $5F, $5F, $73, $65, $74, $5F, $61,
    $70, $70, $5F, $74, $79, $70, $65, $00, $5F, $5F, $6C, $69, $62, $6B, $65, $72,
    $6E, $65, $6C, $33, $32, $5F, $61, $5F, $69, $6E, $61, $6D, $65, $00, $5F, $5F,
    $69, $6D, $70, $5F, $5F, $5F, $76, $73, $6E, $70, $72, $69, $6E, $74, $66, $00,
    $5F, $5F, $69, $6D, $70, $5F, $5F, $73, $74, $72, $63, $61, $74, $00, $5F, $5F,
    $69, $6D, $70, $5F, $5F, $66, $77, $72, $69, $74, $65, $00
  );

function gld_BuildNodes(const wadfile, gwafile: string): boolean;
var
  size: integer;
  p: pointer;
  prog: string;
begin
  prog := I_NewTempFile('glbsp.exe');

  p := @glbsp;
  size := SizeOf(glbsp);
  result := M_WriteFile(prog, p, size);
  if not result then
  begin
    I_Warning('gld_BuildNodes(): Failed to allocate glbsp program.'#13#10);
    exit;
  end;

  printf(#13#10'  gld_BuildNodes: Building GL-Friendly nodes for %s'#13#10, [wadfile]);
  result := I_ExecProgram(prog + ' "' + wadfile + '" -q -xp -o "' + gwafile + '"' {$IFDEF HEXEN} + ' -hexen'{$ENDIF}, false);
  printf(#13#10);
  if not result then
    I_Warning('gld_BuildNodes(): Failed to build GL-Friendly nodes for %s'#13#10, [wadfile]);
end;

function HextW(w: Word): string;
const
  h: array[0..15] of Char = '0123456789abcdef';
begin
  result := h[Hi(w) shr 4] + h[Hi(w) and $F] + h[Lo(w) shr 4] + h[Lo(w) and $F];
end;

function HextL(l: Longint): string;
type
  Long = record
    LoWord: Word;
    HiWord: Word;
  end;
begin
  with Long(l) do
    result := HextW(HiWord) + HextW(LoWord);
end;

function ND_GetNodes(const mapname: string): string;
var
  header: wadinfo_t;
  lump, maplump: integer;
  i, len: integer;
  f: TFile;
  adler32: LongWord;
  path: string;
  mapfilename: string;
  gwafilename: string;
  infotable: array[0..{$IFDEF HEXEN}11{$ELSE}10{$ENDIF}] of filelump_t;
  buf: PByteArray;
begin
  maplump := W_GetNumForName(mapname);
  adler32 := GetMapAdler32(maplump);

  path := M_SaveFileName('DATA\');
  MkDir(path);
  path := path + 'BSP\';
  MkDir(path);

  gwafilename := fexpand(path + mapname + '_' + HextL(adler32) + '.gwa');

  if not fexists(gwafilename) then
  begin
    mapfilename := I_NewTempFile(mapname + '_' + HextL(adler32) + '.wad');

    header.identification :=
      integer(Ord('P') or (Ord('W') shl 8) or (Ord('A') shl 16) or (Ord('D') shl 24));
    header.numlumps := {$IFDEF HEXEN}12{$ELSE}11{$ENDIF};

    f := TFile.Create(mapfilename, fCreate);
    f.Write(header, SizeOf(header));

    ZeroMemory(@infotable, SizeOf(infotable));

    for i := 0 to {$IFDEF HEXEN}11{$ELSE}10{$ENDIF} do
    begin
      lump := maplump + i;
      if lump >= W_NumLumps then
        Break;

      len := W_LumpLength(lump);

      infotable[i].filepos := f.Position;
      infotable[i].size := len;
      infotable[i].name := lumpinfo[lump].name;

      buf := malloc(len);
      W_ReadLump(lump, buf);
      f.Write(buf^, len);
      memfree(pointer(buf), len);
    end;

    header.infotableofs := f.Position;
    f.Write(infotable, SizeOf(infotable));
    f.Seek(0, sFromBeginning);
    f.Write(header, SizeOf(header));
    f.Free;

    gld_BuildNodes(mapfilename, gwafilename);

    if not fexists(gwafilename) then
    begin
      result := '';
      exit;
    end;

  end;

  result := gwafilename;
end;

var
  firstglvert: integer;

constructor TGWAFile.Create(const afilename: string);
var
  f: TFile;
  h: wadinfo_t;
  infotable: array[0..9] of filelump_t;
  i, j: integer;
  ver, ver2: LongWord;
  seg1: GLSeg1_t;
  node1: GLNode1_t;
  subsector1: GLSubSector1_t;
begin
  ffilename := afilename;
  f := TFile.Create(ffilename, fOpenReadOnly);
  f.Read(h, SizeOf(wadinfo_t));
  if (h.numlumps > 10) or (h.numlumps < 1) then
  begin
    f.Free;
    I_Error('TGWAFile.Create(): Standalone GWA constains %d lumps', [h.numlumps]);
    Exit;
  end;

  f.Seek(h.infotableofs, sFromBeginning);
  f.Read(infotable, h.numlumps * SizeOf(filelump_t));

  fnumglvertexes := 0;
  fglvertexes := nil;
  fnummapsubsectors := 0;
  fmapsubsectors := nil;
  fnummapnodes := 0;
  fmapnodes := nil;
  fnummapsegs := 0;
  fmapsegs := nil;

  ver := 0;
  for i := 0 to h.numlumps - 1 do
  begin
    if strupper(char8tostring(infotable[i].name)) = 'GL_VERT' then
    begin
      f.Seek(infotable[i].filepos, sFromBeginning);
      f.Read(ver, SizeOf(ver));
    end
    else if strupper(char8tostring(infotable[i].name)) = 'GL_SEGS' then
    begin
      f.Seek(infotable[i].filepos, sFromBeginning);
      f.Read(ver2, SizeOf(ver2));
      if ver2 = gNd3 then
        ver := gNd3;
    end;
  end;

  if ver = gNd2 then
    glnodesver := 2
  else if ver = gNd3 then
    glnodesver := 3
  else if ver = gNd4 then
    glnodesver := 4
  else if ver = gNd5 then
    glnodesver := 5
  else
    glnodesver := 0;

  printf('TGWAFile.Create(): GL v%d nodes found.'#13#10, [glnodesver]);

  for i := 0 to h.numlumps - 1 do
  begin
    if strupper(char8tostring(infotable[i].name)) = 'GL_VERT' then
    begin
      if glnodesver > 0 then
      begin
        f.Seek(infotable[i].filepos + 4, sFromBeginning);
        fnumglvertexes := (infotable[i].size - 4) div SizeOf(GLVertex2_t);
      end
      else
        fnumglvertexes := infotable[i].size div SizeOf(GLVertex2_t);
      fglvertexes := malloc(fnumglvertexes * SizeOf(GLVertex2_t));
      f.Read(fglvertexes^, fnumglvertexes * SizeOf(GLVertex2_t));
    end
    else if strupper(char8tostring(infotable[i].name)) = 'GL_SSECT' then
    begin
      f.Seek(infotable[i].filepos, sFromBeginning);
      if glnodesver >= 3 then
      begin
        if glnodesver = 3 then
        begin
          fnummapsubsectors := (infotable[i].size - 4) div SizeOf(GLSubSector3_t);
          f.Seek(infotable[i].filepos + 4, sFromBeginning);
        end
        else
          fnummapsubsectors := infotable[i].size div SizeOf(GLSubSector3_t);
        fmapsubsectors := malloc(fnummapsubsectors * SizeOf(GLSubSector3_t));
        f.Read(fmapsubsectors^, fnummapsubsectors * SizeOf(GLSubSector3_t));
      end
      else
      begin
        fnummapsubsectors := infotable[i].size div SizeOf(GLSubSector1_t);
        fmapsubsectors := malloc(fnummapsubsectors * SizeOf(GLSubSector3_t));
        for j := 0 to fnummapsubsectors - 1 do
        begin
          f.Read(subsector1, SizeOf(GLSubSector1_t));
          fmapsubsectors[j].count := subsector1.count;
          fmapsubsectors[j].first_seg := subsector1.first_seg;
        end;
      end;
    end
    else if strupper(char8tostring(infotable[i].name)) = 'GL_NODES' then
    begin
      f.Seek(infotable[i].filepos, sFromBeginning);
      if glnodesver >= 4 then
      begin
        fnummapnodes := infotable[i].size div SizeOf(GLNode4_t);
        fmapnodes := malloc(fnummapnodes * SizeOf(GLNode4_t));
        f.Read(fmapnodes^, fnummapnodes * SizeOf(GLNode4_t));
      end
      else
      begin
        fnummapnodes := infotable[i].size div SizeOf(GLNode1_t);
        fmapnodes := malloc(fnummapnodes * SizeOf(GLNode4_t));
        for j := 0 to fnummapnodes - 1 do
        begin
          f.Read(node1, SizeOf(GLNode1_t));
          fmapnodes[j].x := node1.x;
          fmapnodes[j].y := node1.y;
          fmapnodes[j].dx := node1.dx;
          fmapnodes[j].dy := node1.dy;
          fmapnodes[j].right_bbox[0] := node1.right_bbox[0];
          fmapnodes[j].right_bbox[1] := node1.right_bbox[1];
          fmapnodes[j].right_bbox[2] := node1.right_bbox[2];
          fmapnodes[j].right_bbox[3] := node1.right_bbox[3];
          fmapnodes[j].left_bbox[0] := node1.left_bbox[0];
          fmapnodes[j].left_bbox[1] := node1.left_bbox[1];
          fmapnodes[j].left_bbox[2] := node1.left_bbox[2];
          fmapnodes[j].left_bbox[3] := node1.left_bbox[3];
          if node1.right_child and NF_SUBSECTOR <> 0 then
          begin
            node1.right_child := node1.right_child and not NF_SUBSECTOR;
            fmapnodes[j].right_child := node1.right_child;
            fmapnodes[j].right_child := fmapnodes[j].right_child or NF_SUBSECTOR_V5;
          end
          else
            fmapnodes[j].right_child := node1.right_child;
          if node1.left_child and NF_SUBSECTOR <> 0 then
          begin
            node1.left_child := node1.left_child and not NF_SUBSECTOR;
            fmapnodes[j].left_child := node1.left_child;
            fmapnodes[j].left_child := fmapnodes[j].left_child or NF_SUBSECTOR_V5;
          end
          else
            fmapnodes[j].left_child := node1.left_child;
        end;
      end;
    end
    else if strupper(char8tostring(infotable[i].name)) = 'GL_SEGS' then
    begin
      if glnodesver = 3 then
      begin
        f.Seek(infotable[i].filepos + 4, sFromBeginning);
        fnummapsegs := (infotable[i].size - 4) div SizeOf(GLSeg3_t);
      end
      else
      begin
        f.Seek(infotable[i].filepos, sFromBeginning);
        if glnodesver < 3 then
          fnummapsegs := infotable[i].size div SizeOf(GLSeg1_t)
        else
          fnummapsegs := infotable[i].size div SizeOf(GLSeg3_t);
      end;

      fmapsegs := malloc(fnummapsegs * SizeOf(GLSeg3_t));
      if glnodesver >= 3 then
        f.Read(fmapsegs^, fnummapsegs * SizeOf(GLSeg3_t))
      else
      begin
        for j := 0 to fnummapsegs - 1 do
        begin
          f.Read(seg1, SizeOf(GLSeg1_t));
          fmapsegs[j].start_vertex := seg1.start_vertex;
          fmapsegs[j].end_vertex := seg1.end_vertex;
          fmapsegs[j].linedef := seg1.linedef;
          fmapsegs[j].side := seg1.side;
          fmapsegs[j].partner_seg := seg1.partner_seg;
        end;
      end;
    end;

  end;
  f.Free;
end;

destructor TGWAFile.Destroy;
begin
  memfree(pointer(fglvertexes), fnumglvertexes * SizeOf(GLVertex2_t));
  memfree(pointer(fmapsubsectors), fnummapsubsectors * SizeOf(GLSubSector3_t));
  memfree(pointer(fmapnodes), fnummapnodes * SizeOf(GLNode4_t));
  memfree(pointer(fmapsegs), fnummapsegs * SizeOf(GLSeg3_t));
  inherited;
end;

procedure ND_LoadVertexes(lump: integer; gwa: TGWAFile);
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
  if gwa = nil then
    numglverts := 0
  else
    numglverts := gwa.numglvertexes;

  firstglvert := W_LumpLength(lump) div SizeOf(mapvertex_t);
  numvertexes := firstglvert + numglverts;

  // Allocate zone memory for buffer.
  vertexes := Z_Malloc(numvertexes * SizeOf(vertex_t), PU_LEVEL, nil);

  // Load data into cache.
  data := W_CacheLumpNum(lump, PU_STATIC);

  ml := Pmapvertex_t(data);

  // JVAL: 20200414 -> Find map boundaries
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


  for i := 0 to numglverts - 1 do
  begin
    li.x := gwa.glvertexes[i].x;
    li.y := gwa.glvertexes[i].y;
    li.amvalidcount := 0;
    inc(li);
  end;
end;

//
// P_LoadSubsectors
//
procedure ND_LoadSubsectors(gwa: TGWAFile);
var
  i: integer;
  ss: Psubsector_t;
begin
  numsubsectors := gwa.nummapsubsectors;
  subsectors := Z_Malloc(numsubsectors * SizeOf(subsector_t), PU_LEVEL, nil);
  ZeroMemory(subsectors, numsubsectors * SizeOf(subsector_t));

  ss := @subsectors[0];
  for i := 0 to numsubsectors - 1 do
  begin
    ss.numlines :=  gwa.mapsubsectors[i].count;
    ss.firstline := gwa.mapsubsectors[i].first_seg;
    inc(ss);
  end;
end;

procedure ND_LoadNodes(gwa: TGWAFile);
var
  i: integer;
  k: integer;
  no: Pnode_t;
begin
  numnodes := gwa.nummapnodes;
  nodes := Z_Malloc(numnodes * SizeOf(node_t), PU_LEVEL, nil);
  ZeroMemory(nodes, numnodes * SizeOf(node_t));

  no := @nodes[0];
  for i := 0 to numnodes - 1 do
  begin
    no.x := gwa.mapnodes[i].x * FRACUNIT;
    no.y := gwa.mapnodes[i].y * FRACUNIT;
    no.dx := gwa.mapnodes[i].dx * FRACUNIT;
    no.dy := gwa.mapnodes[i].dy * FRACUNIT;
    no.children[0] := gwa.mapnodes[i].right_child;
    no.children[1] := gwa.mapnodes[i].left_child;
    for k := 0 to 3 do
      no.bbox[0, k] := gwa.mapnodes[i].right_bbox[k] * FRACUNIT;
    for k := 0 to 3 do
      no.bbox[1, k] := gwa.mapnodes[i].left_bbox[k] * FRACUNIT;
    inc(no);
  end;
end;

{$IFDEF OPENGL}
function gldist(dx, dy: integer): float;
var
  fx, fy: float;
begin
  fx := dx / FRACUNIT;
  fy := dy / FRACUNIT;
  result := sqrt(fx * fx + fy * fy);
end;
{$ENDIF}

function CheckGLVertex(num: integer): integer;
begin
  if glnodesver <= 2 then
  begin
    if num and (1 shl 15) <> 0 then
    begin
      result := num and (1 shl 15 - 1) + firstglvert;
      exit;
    end
  end
  else if glnodesver <= 4 then
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

procedure ND_LoadSegs(gwa: TGWAFile);
var
  i: integer;
  ml: PGLSeg3_t;
  li: Pseg_t;
  ldef: Pline_t;
  linedef: integer;
  side: integer;
  sidenum: integer;
begin
  numsegs := gwa.nummapsegs;
  segs := Z_Malloc(numsegs * SizeOf(seg_t), PU_LEVEL, nil);
  ZeroMemory(segs, numsegs * SizeOf(seg_t));

  ml := @gwa.mapsegs[0];
  li := @segs[0];
  for i := 0 to numsegs - 1 do
  begin
    li.v1 := @vertexes[CheckGLVertex(ml.start_vertex)];
    li.v2 := @vertexes[CheckGLVertex(ml.end_vertex)];
    {$IFDEF OPENGL}
    li.iSegID := i;
    {$ENDIF}

    if PWord(@ml.linedef)^ = word(1 shl 16 - 1) then
    begin
      li.miniseg := true;
      li.angle := 0;
      li.offset := 0;
      {$IFDEF OPENGL}
      li.length := 0;
      {$ENDIF}
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
          I_Warning('ND_LoadSegs(): Line %d is marked with ML_TWOSIDED flag without backsector'#13#10, [linedef]);
          ldef.flags := ldef.flags and not ML_TWOSIDED;
          li.backsector := nil;
        end
        else
          li.backsector := sides[sidenum].sector;
      end
      else
        li.backsector := nil;
      {$IFDEF OPENGL}
      li.length := gldist(li.v2.x - li.v1.x, li.v2.y - li.v1.y);
      {$ENDIF}
    end;
    inc(ml);
    inc(li);
  end;
end;

//
// ND_NodesCheck
//
// Checks is nodes are available in wad map
// If not we will build the nodes.
// This routine allows to load levels without building the nodes (eg slige.out)
procedure ND_NodesCheck(const lumpname: string);
var
  lumpnum: integer;
  candonodes: boolean;
  has_things, has_lines, has_sides, has_vertexes, has_sectors: integer;
  has_segs, has_ssectors, has_nodes, has_reject, has_blockmap: integer;
  {$IFDEF HEXEN}
  has_behavior: integer;
  {$ENDIF}
  i: integer;
  crc32a, crc32b, crc32c, crc32d: string;
  outfilemap: string;
  wadfilemap: string;
  mapdir: string;
  lname: string;
  f: TFile;
  header: wadinfo_t;
  infotable: array[0..{$IFDEF HEXEN}11{$ELSE}10{$ENDIF}] of filelump_t;
  ldata: PByteArray;
  llen: integer;
  size: integer;
  p: pointer;
  prog: string;
begin
  lumpnum := W_GetNumForName(lumpname);
  if (lumpnum < 0) or (lumpnum >= W_NumLumps - 1) then
    exit;

  has_things := -1;
  has_lines := -1;
  has_sides := -1;
  has_vertexes := -1;
  has_sectors := -1;
  has_segs := -1;
  has_ssectors := -1;
  has_nodes := -1;
  has_reject := -1;
  has_blockmap := -1;
  {$IFDEF HEXEN}
  has_behavior := -1;
  {$ENDIF}

  if lumpnum <= W_NumLumps - {$IFDEF HEXEN}11{$ELSE}10{$ENDIF} then
  begin
    for i := 1 to {$IFDEF HEXEN}11{$ELSE}10{$ENDIF} do
    begin
      lname := strupper(char8tostring(W_GetNameForNum(lumpnum + i)));
      // JVAL: 20200222 - Fix bug of continous maps without all assets
      if (has_things = -1) and (lname = 'THINGS') then
        has_things := i
      else if (has_lines = -1) and (lname = 'LINEDEFS') then
        has_lines := i
      else if (has_sides = -1) and (lname = 'SIDEDEFS') then
        has_sides := i
      else if (has_vertexes = -1) and (lname = 'VERTEXES') then
        has_vertexes := i
      else if (has_segs = -1) and (lname = 'SEGS') then
        has_segs := i
      else if (has_ssectors = -1) and (lname = 'SSECTORS') then
        has_ssectors := i
      else if (has_nodes = -1) and (lname = 'NODES') then
        has_nodes := i
      else if (has_sectors = -1) and (lname = 'SECTORS') then
        has_sectors := i
      else if (has_reject = -1) and (lname = 'REJECT') then
        has_reject := i
      else if (has_blockmap = -1) and (lname = 'BLOCKMAP') then
        has_blockmap := i
      {$IFDEF HEXEN}
      else if (has_behavior = -1) and (lname = 'BEHAVIOR') then
        has_behavior := i{$ENDIF};
    end;
  end
  else if lumpnum <= W_NumLumps - {$IFDEF HEXEN}7{$ELSE}6{$ENDIF} then
  begin
    for i := 1 to {$IFDEF HEXEN}6{$ELSE}5{$ENDIF} do
    begin
      lname := strupper(char8tostring(W_GetNameForNum(lumpnum + i)));
      // JVAL: 20200222 - Fix bug of continous maps without all assets
      if (has_things = -1) and (lname = 'THINGS') then
        has_things := i
      else if (has_lines = -1) and (lname = 'LINEDEFS') then
        has_lines := i
      else if (has_sides = -1) and (lname = 'SIDEDEFS') then
        has_sides := i
      else if (has_vertexes = -1) and (lname = 'VERTEXES') then
        has_vertexes := i
      else if (has_sectors = -1) and (lname = 'SECTORS') then
        has_sectors := i
      {$IFDEF HEXEN}
      else if (has_behavior = -1) and (lname = 'BEHAVIOR') then
        has_behavior := i{$ENDIF};
    end;
  end;

  if has_things = 1 then
    if has_lines = 2 then
      if has_sides = 3 then
        if has_vertexes = 4 then
          if has_segs = 5 then
            if has_ssectors = 6 then
              if has_nodes = 7 then
                if has_sectors = 8 then
                  if has_reject = 9 then
                    if has_blockmap = 10 then
                    {$IFDEF HEXEN}
                     if has_behavior = 11 then
                     {$ENDIF}
                       exit; // Nothing to do

  // Check if we can build the nodes
  candonodes := false;

  if has_things > 0 then
    if has_lines > 0 then
      if has_sides > 0 then
        if has_vertexes > 0 then
          if has_sectors > 0 then
            candonodes := true;

  if not candonodes then
  begin
    I_Warning('ND_NodesCheck(): Missing critical map data to play the map'#13#10);
    exit;
  end;

  crc32a := GetLumpCRC32(lumpnum + has_lines);
  crc32b := GetLumpCRC32(lumpnum + has_sides);
  crc32c := GetLumpCRC32(lumpnum + has_vertexes);
  crc32d := GetLumpCRC32(lumpnum + has_sectors);

  mapdir := M_SaveFileName('DATA\');
  MkDir(mapdir);
  mapdir := mapdir + 'WADS\';
  MkDir(mapdir);
  wadfilemap := mapdir + lumpname + '_' + crc32a + '_' + crc32b + '_' + crc32c + '_' + crc32d + '.wad';

  if not fexists(wadfilemap) then
  begin
    outfilemap := mapdir + lumpname + '_' + crc32a + '_' + crc32b + '_' + crc32c + '_' + crc32d + '.out';
    I_DeclareTempFile(outfilemap);

    header.identification :=
      integer(Ord('P') or (Ord('W') shl 8) or (Ord('A') shl 16) or (Ord('D') shl 24));
    header.numlumps := {$IFDEF HEXEN}12{$ELSE}11{$ENDIF};

    f := TFile.Create(outfilemap, fCreate);
    f.Write(header, SizeOf(header));

    ZeroMemory(@infotable, SizeOf(infotable));

    infotable[0].filepos := f.Position;
    infotable[0].size := 0;
    infotable[0].name := stringtochar8(strupper(lumpname));

    if has_things > 0 then
      llen := W_LumpLength(lumpnum + has_things)
    else
      llen := 0;
    infotable[1].filepos := f.Position;
    infotable[1].size := llen;
    infotable[1].name := stringtochar8('THINGS');
    if llen > 0 then
    begin
      ldata := malloc(llen);
      W_ReadLump(lumpnum + has_things, ldata);
      f.Write(ldata^, llen);
      memfree(pointer(ldata), llen);
    end;

    if has_lines > 0 then
      llen := W_LumpLength(lumpnum + has_lines)
    else
      llen := 0;
    infotable[2].filepos := f.Position;
    infotable[2].size := llen;
    infotable[2].name := stringtochar8('LINEDEFS');
    if llen > 0 then
    begin
      ldata := malloc(llen);
      W_ReadLump(lumpnum + has_lines, ldata);
      f.Write(ldata^, llen);
      memfree(pointer(ldata), llen);
    end;

    if has_sides > 0 then
      llen := W_LumpLength(lumpnum + has_sides)
    else
      llen := 0;
    infotable[3].filepos := f.Position;
    infotable[3].size := llen;
    infotable[3].name := stringtochar8('SIDEDEFS');
    if llen > 0 then
    begin
      ldata := malloc(llen);
      W_ReadLump(lumpnum + has_sides, ldata);
      f.Write(ldata^, llen);
      memfree(pointer(ldata), llen);
    end;

    if has_sides > 0 then
      llen := W_LumpLength(lumpnum + has_sides)
    else
      llen := 0;
    infotable[4].filepos := f.Position;
    infotable[4].size := llen;
    infotable[4].name := stringtochar8('VERTEXES');
    if llen > 0 then
    begin
      ldata := malloc(llen);
      W_ReadLump(lumpnum + has_vertexes, ldata);
      f.Write(ldata^, llen);
      memfree(pointer(ldata), llen);
    end;

    infotable[5].filepos := f.Position;
    infotable[5].size := 0;
    infotable[5].name := stringtochar8('SEGS');

    infotable[6].filepos := f.Position;
    infotable[6].size := 0;
    infotable[6].name := stringtochar8('SSECTORS');

    infotable[7].filepos := f.Position;
    infotable[7].size := 0;
    infotable[7].name := stringtochar8('NODES');

    if has_sectors > 0 then
      llen := W_LumpLength(lumpnum + has_sectors)
    else
      llen := 0;
    infotable[8].filepos := f.Position;
    infotable[8].size := llen;
    infotable[8].name := stringtochar8('SECTORS');
    if llen > 0 then
    begin
      ldata := malloc(llen);
      W_ReadLump(lumpnum + has_sectors, ldata);
      f.Write(ldata^, llen);
      memfree(pointer(ldata), llen);
    end;

    infotable[9].filepos := f.Position;
    infotable[9].size := 0;
    infotable[9].name := stringtochar8('REJECT');

    infotable[10].filepos := f.Position;
    infotable[10].size := 0;
    infotable[10].name := stringtochar8('BLOCKMAP');

    {$IFDEF HEXEN}
    infotable[11].filepos := f.Position;
    if has_behavior > 0 then
    begin
      llen := W_LumpLength(lumpnum + has_behavior);
      infotable[11].size := llen;
      ldata := malloc(llen);
      W_ReadLump(lumpnum + has_behavior, ldata);
      f.Write(ldata^, llen);
      memfree(pointer(ldata), llen);
    end
    else
      infotable[11].size := 0;
    infotable[11].name := stringtochar8('BEHAVIOR');
    {$ENDIF}

    header.infotableofs := f.Position;
    f.Write(infotable, SizeOf(infotable));
    f.Seek(0, sFromBeginning);
    f.Write(header, SizeOf(header));
    f.Free;

    // Build the nodes
    prog := I_NewTempFile('glbsp.exe');

    p := @glbsp;
    size := SizeOf(glbsp);
    if not M_WriteFile(prog, p, size) then
    begin
      I_Warning('ND_NodesCheck(): Failed to allocate glbsp program.'#13#10);
      exit;
    end;

    printf(#13#10'  ND_NodesCheck: Building GL-Friendly nodes for %s'#13#10, [lumpname]);

    if not I_ExecProgram(prog + ' "' + outfilemap + '" -o "' + wadfilemap + '"' {$IFDEF HEXEN} + ' -hexen'{$ENDIF}, false) then
    begin
      printf(#13#10);
      I_Warning('ND_NodesCheck(): Failed to build GL-Friendly nodes for %s'#13#10, [outfilemap]);
      exit;
    end;
    printf(#13#10);

  end;

  W_RuntimeLoad(wadfilemap, F_ORIGIN_WAD);
end;

end.

