//------------------------------------------------------------------------------
//
//  DelphiDoom: A modified and improved DOOM engine for Windows
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 2004-2011 by Jim Valavanis
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
//  Site  : http://delphidoom.sitesled.com/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit gl_data;

interface

uses
  d_delphi,
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
  PGLSeg3_t = ^GLSeg1_t;
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

  GLSubSector_t = GLSubSector3_t;
  PGLSubSector_t = ^GLSubSector_t;
  GLSubSector_tArray = array[0..$FFFF] of GLSubSector_t;
  PGLSubSector_tArray = ^GLSubSector_tArray;

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
    right_bbox: packed array[0..1] of smallint;
    left_bbox: packed array[0..1] of smallint;
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
    right_bbox: packed array[0..1] of smallint;
    left_bbox: packed array[0..1] of smallint;
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

implementation

uses
  doomdata,
  i_tmp,
  i_exec,
  i_system,
  gl_bsp,
  w_wad,
  m_misc,
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

  data := W_CacheLumpNum(base + Ord(ML_GL_VERTS), PU_CACHE);
  if PLongWord(data)^ = gNd2 then
  begin
    data := W_CacheLumpNum(base + Ord(ML_GL_SEGS), PU_CACHE);
    if PLongWord(data)^ = gNd3 then
      result := 3
    else
      result := 2;
    exit;
  end;

  if PLongWord(data)^ = gNd4 then
  begin
    result := 4;
    exit;
  end;

  if PLongWord(data)^ = gNd5 then
  begin
    result := 5;
    exit;
  end;

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

function GetMapAdler32(maplump: integer): longword;
var
  data: PByteArray;
  A, B: longword;
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
function GetGLMapAdler32(glmaplump: integer; glmapname: string): longword;
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
  adler32: longword;
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
    I_Warning('gld_BuildNodes(): Failed to alocate glbsp program.'#13#10);
    exit;
  end;

  printf(#13#10'  gld_BuildNodes: Building GL-Friendly nodes for %s'#13#10, [wadfile]);
  result := I_ExecProgram(prog + ' ' + wadfile + ' -q -xp -o ' + gwafile, false);
  printf(#13#10);
  if not result then
    I_Warning('gld_BuildNodes(): Failed to build GL-Friendly nodes for %s'#13#10, [wadfile]);

end;

end.
