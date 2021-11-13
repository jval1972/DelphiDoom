//------------------------------------------------------------------------------
//
//  DelphiHeretic: A modified and improved Heretic port for Windows
//  based on original Linux Doom as published by "id Software", on
//  Heretic source as published by "Raven" software and DelphiDoom
//  as published by Jim Valavanis.
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
//  Refresh module, data I/O, caching, retrieval of graphics
//  by name.
//  Preparation of data for rendering,
//  generation of lookups, caching, retrieval by name.
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit r_data;

interface

uses
  d_delphi,
  m_fixed,
  r_defs,
  w_wad;

// Retrieve column data for span blitting.
{$IFNDEF OPENGL}
function R_GetColumn(const tex: integer; col: integer): PByteArray;
{$ENDIF}

{$IFNDEF OPENGL}
// Retrieve ds_sources
procedure R_GetDSs(const flat: integer);
{$ENDIF}

function R_GetLumpForFlat(const flat: integer): integer;

{$IFNDEF OPENGL}
// Retrieve dc_sources
procedure R_GetDCs(const tex: integer; const col: integer);
{$ENDIF}

// I/O, setting up the stuff.
procedure R_InitData;
procedure R_PrecacheLevel;

// Retrieval.
// Floor/ceiling opaque texture tiles,
// lookup by name. For animation?
function R_FlatNumForName(const name: string): integer;
function R_SafeFlatNumForName(const name: string): integer;
function R_CacheFlat(const lump: integer; const tag: integer): pointer;

// Called by P_Ticker for switches and animations,
// returns the texture number for the texture name.
function R_CheckTextureNumForName(const name: string): integer;
function R_SafeTextureNumForName(const name: string): integer;
function R_NameForSideTexture(const sn: SmallInt): char8_t;
function R_TextureNumForName(const name: string): integer;

var
// for global animation
  texturetranslation: PIntegerArray;

// needed for texture pegging
  textureheight: Pfixed_tArray;
// JVAL: 20200112 - For tall textures
  texturecolumnheight: PIntegerArray;
  texturecolumnheightfrac: PIntegerArray;
  texturecompositesize: PIntegerArray;

  firstspritelump: integer;
  lastspritelump: integer;

// needed for pre rendering
  spritewidth: Pfixed_tArray;
  spriteoffset: Pfixed_tArray;
  spritetopoffset: Pfixed_tArray;
  spritepresent: PBooleanArray;

  fog_colormaps: PByteArray;  // JVAL: Mars fog sectors
  colormaps: PByteArray;
  colormaps32: PLongWordArray;
  fog_colormaps32: PLongWordArray;  // JVAL: Mars fog sectors

var
  firstflat: integer;
  lastflat: integer;
  numflats: integer;
  maxvisplane: integer = -1;

procedure R_SetupLevel;

var
  numtextures: integer;
  textures: Ptexture_tPArray;
  flats: PflatPArray;
  aprox_black: byte = 254;
  aprox_red: byte = 160;

implementation

uses
  doomdef,
  doomstat,
{$IFDEF FPC}
  d_fpc,
{$ENDIF}
  d_think,
  m_hash,
  g_game,
  i_system,
  p_local,
  p_setup,
  p_tick,
  p_mobj_h,
  p_mobj,
  p_terrain,
  r_sky,
  r_things,
  r_bsp,
  r_hires,
{$IFNDEF OPENGL}
  r_column,
  r_tallcolumn,
  r_span,
  r_cache_walls,
  r_cache_flats,
  r_col_fz,
  r_voxels,
  r_3dfloors, // JVAL: 3d Floors
  r_slopes, // JVAL: Slopes
  r_patch,
{$ENDIF}
  r_flatinfo,
  v_data,
  v_video,
  vx_voxelsprite,
  w_sprite,
  z_zone;

//
// Graphics.
// DOOM graphics for walls and sprites
// is stored in vertical runs of opaque pixels (posts).
// A column is composed of zero or more posts,
// a patch or sprite is composed of zero or more columns.
//



//
// R_DrawColumnInCache
// Clip and draw a column
//  from a patch into a cached post.
//
procedure R_DrawColumnInCache(patch: Pcolumn_t; cache: PByteArray;
  originy: integer; cacheheight: integer);
var
  count: integer;
  position: integer;
  delta, prevdelta: integer;
  tallpatch: boolean;
begin
  delta := 0;
  tallpatch := false;
  while patch.topdelta <> $ff do
  begin
    count := patch.length;
    position := originy + delta + patch.topdelta;

    if position < 0 then
    begin
      count := count + position;
      position := 0;
    end;

    if position + count > cacheheight then
      count := cacheheight - position;

    if count > 0 then
      memcpy(@cache[position], PByteArray(integer(patch) + 3), count);

    if not tallpatch then
    begin
      prevdelta := patch.topdelta;
      patch := Pcolumn_t(integer(patch) + patch.length + 4);
      if patch.topdelta > prevdelta then
        delta := 0
      else
        tallpatch := true;
    end
    else
      patch := Pcolumn_t(integer(patch) + patch.length + 4);
  end;
end;


procedure R_DrawColumnInCacheMultipatch(patch: Pcolumn_t; cache: PByteArray;
  originy: integer; cacheheight: integer; marks: PByteArray);
var
  count: integer;
  position: integer;
  delta, prevdelta: integer;
  tallpatch: boolean;
begin
  delta := 0;
  tallpatch := false;
  while patch.topdelta <> $ff do
  begin
    count := patch.length;
    position := originy + delta + patch.topdelta;

    if position < 0 then
    begin
      count := count + position;
      position := 0;
    end;

    if position + count > cacheheight then
      count := cacheheight - position;

    if count > 0 then
    begin
      memcpy(@cache[position], PByteArray(integer(patch) + 3), count);
      memset(@marks[position], $ff, count);
    end;

    if not tallpatch then
    begin
      prevdelta := patch.topdelta;
      patch := Pcolumn_t(integer(patch) + patch.length + 4);
      if patch.topdelta > prevdelta then
        delta := 0
      else
        tallpatch := true;
    end
    else
      patch := Pcolumn_t(integer(patch) + patch.length + 4);

  end;
end;

//
// R_GenerateComposite
// Using the texture definition,
//  the composite texture is created from the patches,
//  and each column is cached.
//
procedure R_GenerateComposite(const texnum: integer);
var
  block: PByteArray;
  texture: Ptexture_t;
  patch: Ptexpatch_t;
  realpatch: Ppatch_t;
  x1: integer;
  x2: integer;
  i, j: integer;
  cofs: PIntegerArray;
  col, patchcol: Pcolumn_t;
  collump: PSmallIntArray;
  colofs: PIntegerArray; //PWordArray; // 64k
  twidth: integer;
  theight: integer;
  theight4: integer;
  source, mark1, marks: PByteArray;
  marksize: integer;
begin
  texture := textures[texnum];
  twidth := texture.width;
  theight := texture.height;
  marksize := twidth * theight;

  block := Z_Malloc(texturecompositesize[texnum], PU_STATIC, @texturecomposite[texnum]);

  collump := texturecolumnlump[texnum];
  colofs := texturecolumnofs[texnum];

  // Composite the columns together.
  patch := @(texture.patches[0]);

  if texture.patchcount = 1 then
  begin

    realpatch := W_CacheLumpNum(patch.patch, PU_STATIC);
    x1 := patch.originx;
    x2 := x1 + realpatch.width;

    cofs := PIntegerArray(integer(@realpatch.columnofs) - x1 * SizeOf(integer));

    if x1 < 0 then
      x1 := 0;

    if x2 > twidth then
      x2 := twidth;

    while x1 < x2 do
    begin
      // Column does not have multiple patches?
      if collump[x1] < 0 then
      begin
        patchcol := Pcolumn_t(integer(realpatch) + cofs[x1]);
        R_DrawColumnInCache(
          patchcol, @block[colofs[x1]], patch.originy, theight);
      end;
      inc(x1);
    end;
    Z_ChangeTag(realpatch, PU_CACHE);

  end
  else
  begin
    marks := mallocz(marksize);

    for i := 0 to texture.patchcount - 1 do
    begin
      realpatch := W_CacheLumpNum(patch.patch, PU_STATIC);
      x1 := patch.originx;
      x2 := x1 + realpatch.width;

      cofs := PIntegerArray(integer(@realpatch.columnofs) - x1 * SizeOf(integer));

      if x1 < 0 then
        x1 := 0;

      if x2 > twidth then
        x2 := twidth;

      while x1 < x2 do
      begin
        // Column does not have multiple patches?
        if collump[x1] < 0 then
        begin
          patchcol := Pcolumn_t(integer(realpatch) + cofs[x1]);
          R_DrawColumnInCacheMultipatch(
            patchcol, @block[colofs[x1]], patch.originy, theight, @marks[x1 * theight]);
        end;
        inc(x1);
      end;
      Z_ChangeTag(realpatch, PU_CACHE);
      inc(patch);
    end;

    source := malloc(theight);  // temporary column
    for i := 0 to twidth - 1 do
      if collump[i] < 0 then // process only multipatched columns
      begin
        col := Pcolumn_t(@block[colofs[i] - 3]);  // cached column
        mark1 := @marks[i * theight];
        j := 0;

        // save column in temporary so we can shuffle it around
        memcpy(source, PByteArray(integer(col) + 3), theight);

        while true do  // reconstruct the column by scanning transparency marks
        begin
          // skip transparent cells
          // JVAL: Fast skip, check 4 bytes at first
          theight4 := theight - 4;
          while (j < theight4) and (PInteger(@mark1[j])^ = 0) do
            j := j + 4;
          // JVAL: Finally check each of the remaining bytes
          while (j < theight) and (mark1[j] = 0) do
            inc(j);
          if j >= theight then    // if at end of column
          begin
            col.topdelta := $FF;  // end-of-column marker
            break;
          end;
          col.topdelta := j;      // starting offset of post
          // JVAL: 20200118 - Added check for walls with height >= 256
          while (j < theight) and (mark1[j] <> 0) and (j - col.topdelta < 255) do
            inc(j);
          col.length := j - col.topdelta;
          // copy opaque cells from the temporary back into the column
          memcpy(PByteArray(integer(col) + 3), @source[col.topdelta], col.length);
          col := Pcolumn_t(integer(col) + col.length + 4); // next post
        end;
      end;

    memfree(pointer(source), theight);  // free temporary column
    memfree(pointer(marks), marksize);  // free transparency marks
  end;


  // Now that the texture has been built in column cache,
  //  it is purgable from zone memory.
  Z_ChangeTag(block, PU_CACHE);
end;

//
// R_GenerateLookup
//
procedure R_GenerateLookup(const texnum: integer);
var
  texture: Ptexture_t;
  patchcount: PIntegerArray; // patchcount[texture->width]
  postcount: PIntegerArray; // postcount[texture->width]
  patch: Ptexpatch_t;
  realpatch: Ppatch_t;
  x: integer;
  x1: integer;
  x2: integer;
  i: integer;
  collump: PSmallIntArray;
  colofs: PIntegerArray;  //PWordArray; // 64k
  col: Pcolumn_t;
  cofs: PIntegerArray;
  csize: integer;
  pat: integer;
begin
  texture := textures[texnum];

  // Composited texture not created yet.
  texturecomposite[texnum] := nil;

  texturecompositesize[texnum] := 0;
  collump := texturecolumnlump[texnum];
  colofs := texturecolumnofs[texnum];

  // Now count the number of columns
  //  that are covered by more than one patch.
  // Fill in the lump / offset, so columns
  //  with only a single patch are all done.
  patchcount := mallocz(texture.width * SizeOf(integer));
  postcount := mallocz(texture.width * SizeOf(integer));
  patch := @texture.patches[0];

  for i := 0 to texture.patchcount - 1 do
  begin
    pat := patch.patch;
    realpatch := W_CacheLumpNum(pat, PU_STATIC);
    x1 := patch.originx;
    x2 := x1 + realpatch.width;
    cofs := PIntegerArray(@realpatch.columnofs[-x1]);

    if x1 < 0 then
      x := 0
    else
      x := x1;

    if x2 > texture.width then
      x2 := texture.width;

    while x < x2 do
    begin
      col := Pcolumn_t(integer(realpatch) + cofs[x]);
      while col.topdelta <> $ff do
      begin
        postcount[x] := postcount[x] + 1;
        col := Pcolumn_t(integer(col) + col.length + 4);
      end;
      patchcount[x] := patchcount[x] + 1;
      collump[x] := pat;
      colofs[x] := cofs[x] + 3;
      inc(x);
    end;
    Z_ChangeTag(realpatch, PU_CACHE);
    inc(patch);
  end;

  if texture.patchcount > 1 then
  begin

    csize := 0;
    for x := 0 to texture.width - 1 do
    begin
      if patchcount[x] <= 0 then
      begin
        I_DevWarning('R_GenerateLookup(): column (%d) without a patch (%s, column=%d)'#13#10, [x, char8tostring(texture.name), x]);
        collump[x] := -2;
      end
      else
      begin
        // Use the cached block.
        collump[x] := -1;
        colofs[x] := csize + 3;
        csize := csize + 4 * postcount[x] + 1;
      end;
      csize := csize + texture.height;
    end;
    texturecompositesize[texnum] := csize;

  end
  else
  begin

    for x := 0 to texture.width - 1 do
    begin
      if patchcount[x] = 0 then
      begin
        I_DevWarning('R_GenerateLookup(): column (%d) without a patch (%s)'#13#10, [x, char8tostring(texture.name)]);
        collump[x] := -2;
      end
      else if patchcount[x] > 1 then
      begin
        // Use the cached block.
        collump[x] := -1;
        colofs[x] := texturecompositesize[texnum];

        if texturecompositesize[texnum] > $10000 - texture.height then
          I_DevWarning('R_GenerateLookup(): texture %d is > 64k'#13#10, [texnum]);

        texturecompositesize[texnum] := texturecompositesize[texnum] + texture.height;
      end;
    end;

  end;


  if texturecompositesize[texnum] > $10000 - texture.height then
    I_DevWarning('R_GenerateLookup(): texture %d is > 64k'#13#10, [texnum]);

  memfree(pointer(patchcount), texture.width * SizeOf(integer));
  memfree(pointer(postcount), texture.width * SizeOf(integer));
end;

//
// R_GetColumn
//
{$IFNDEF OPENGL}
type
  blancpost_t = packed record
    topdelta: byte;
    length: byte;
    pad: byte;
    data: byte;
  end;

const
  blancpost: blancpost_t = (
    topdelta: $ff;
    length: 0;
    pad: 0;
    data: 0
  );

function R_GetColumn(const tex: integer; col: integer): PByteArray;
var
  lump: integer;
  ofs: integer;
begin
// JVAL: 20200105 - Texture width is not requiered to be power of 2
  col := col mod texturewidth[tex];
  if col < 0 then
    col := col + texturewidth[tex];
  lump := texturecolumnlump[tex][col];
  ofs := texturecolumnofs[tex][col];

// JVAL: 20200112 - For tall textures
  dc_height := texturecolumnheight[tex];

  if lump = -2 then
  begin
    result := @blancpost.data;
    exit;
  end;

  if lump > 0 then
  begin
    result := R_GetFixedColumn(PByteArray(integer(W_CacheLumpNum(lump, PU_LEVEL)) + ofs), tex, col, false);
    exit;
  end;

  if texturecomposite[tex] = nil then
    R_GenerateComposite(tex);

  result := R_GetFixedColumn(PByteArray(integer(texturecomposite[tex]) + ofs), tex, col, true);
end;
{$ENDIF}

{$IFNDEF OPENGL}
procedure R_GetDSs(const flat: integer);
var
  lump: integer;
begin
  if videomode = vm8bit then
  begin
    lump := R_GetLumpForFlat(flat);
    ds_source := W_CacheLumpNum(lump, PU_STATIC);
    ds_scale := R_FlatScaleFromSize(flat, W_LumpLength(lump));
    ds_size := flats[flats[flat].translation].size;
  end
  else
    R_ReadDS32Cache(flat);
end;
{$ENDIF}

function R_GetLumpForFlat(const flat: integer): integer;
begin
  result := flats[flats[flat].translation].lump;
end;

{$IFNDEF OPENGL}
procedure R_GetDCs(const tex: integer; const col: integer);
begin
  if videomode = vm8bit then
    dc_source := R_GetColumn(tex, col)
  else
    R_ReadDC32Cache(tex, col);
end;
{$ENDIF}

//
// R_InitTextures
// Initializes the texture list
//  with the textures from the world map.
//
procedure R_InitTextures;
var
  mtexture: Pmaptexture_t;
  texture: Ptexture_t;
  mpatch: Pmappatch_t;
  patch: Ptexpatch_t;
  i: integer;
  j: integer;
  maptex: PIntegerArray;
  maptex1: PIntegerArray;
  maptex2: PIntegerArray;
  maptex3: PIntegerArray;
  name: char8_t;
  names: PByteArray;
  name_p: PByteArray;
  patchlookup: PIntegerArray;
  nummappatches: integer;
  offset: integer;
  maxoff: integer;
  maxoff2: integer;
  maxoff3: integer;
  numtextures1: integer;
  numtextures2: integer;
  numtextures3: integer;
  directory: PIntegerArray;
  t2lump: integer;
  t3lump: integer;
  pname: string;
begin
  {$IFNDEF OPENGL}
  R_InitFixedColumn;
  {$ENDIF}
  // Load the patch names from pnames.lmp.
  ZeroMemory(@name, SizeOf(char8_t));
  names := W_CacheLumpName('PNAMES', PU_STATIC);
  nummappatches := PInteger(names)^;
  name_p := PByteArray(integer(names) + 4);

  patchlookup := malloc(nummappatches * SizeOf(integer));

  for i := 0 to nummappatches - 1 do
  begin
    j := 0;
    while j < 8 do
    begin
      name[j] := toupper(Chr(name_p[i * 8 + j]));
      if name[j] = #0 then
      begin
        inc(j);
        break;
      end;
      inc(j);
    end;
    while j < 8 do
    begin
      name[j] := #0;
      inc(j);
    end;
    pname := strtrim(char8tostring(name));
    patchlookup[i] := W_CheckNumForName(pname, TYPE_PATCH or TYPE_SPRITE);
    if patchlookup[i] = -1 then
    begin
      I_DevWarning('R_InitTextures(): Can not find patch "%s" inside patch or sprite markers, retrying...'#13#10, [pname]);
      patchlookup[i] := W_CheckNumForName(pname);
      if patchlookup[i] = -1 then
      begin
        I_Warning('R_InitTextures(): Can not find patch "%s"'#13#10, [pname]);
        patchlookup[i] := W_CheckNumForName('NULLA0');
      end;
    end;
  end;
  Z_Free(names);

  // Load the map texture definitions from textures.lmp.
  // The data is contained in one or two lumps,
  //  TEXTURE1 for shareware, plus TEXTURE2 for commercial.
  // JVAL: TEXTURE3 for more textures
  maptex1 := W_CacheLumpName('TEXTURE1', PU_STATIC);
  maptex := maptex1;
  numtextures1 := maptex[0];
  maxoff := W_LumpLength(W_GetNumForName('TEXTURE1'));
  directory := PintegerArray(integer(maptex) + SizeOf(integer));

  t2lump := W_CheckNumForName('TEXTURE2');
  if t2lump <> -1 then
  begin
    maptex2 := W_CacheLumpNum(t2lump, PU_STATIC);
    numtextures2 := maptex2[0];
    maxoff2 := W_LumpLength(t2lump);
  end
  else
  begin
    maptex2 := nil;
    numtextures2 := 0;
    maxoff2 := 0;
  end;

  t3lump := W_CheckNumForName('TEXTURE3');
  if t3lump <> -1 then
  begin
    maptex3 := W_CacheLumpNum(t3lump, PU_STATIC);
    numtextures3 := maptex3[0];
    maxoff3 := W_LumpLength(t3lump);
  end
  else
  begin
    maptex3 := nil;
    numtextures3 := 0;
    maxoff3 := 0;
  end;

  numtextures := numtextures1 + numtextures2 + numtextures3;

  textures := Z_Malloc(numtextures * SizeOf(Ptexture_t), PU_STATIC, nil);
  texturecolumnlump := Z_Malloc(numtextures * SizeOf(PSmallIntArray), PU_STATIC, nil);
  texturecolumnofs := Z_Malloc(numtextures * SizeOf(PIntegerArray), PU_STATIC, nil);
  texturecomposite := Z_Malloc(numtextures * SizeOf(PByteArray), PU_STATIC, nil);
  texturecompositesize := Z_Malloc(numtextures * SizeOf(integer), PU_STATIC, nil);
  texturewidth := Z_Malloc(numtextures * SizeOf(integer), PU_STATIC, nil);
  textureheight := Z_Malloc(numtextures * SizeOf(fixed_t), PU_STATIC, nil);
// JVAL: 20200112 - For tall textures
  texturecolumnheight := Z_Malloc(numtextures * SizeOf(integer), PU_STATIC, nil);
  texturecolumnheightfrac := Z_Malloc(numtextures * SizeOf(integer), PU_STATIC, nil);

  for i := 0 to numtextures - 1 do
  begin
    if i = numtextures1 then
    begin
      // Start looking in second texture file.
      maptex := maptex2;
      maxoff := maxoff2;
      directory := PIntegerArray(integer(maptex) + SizeOf(integer));
    end;
    if i = numtextures1 + numtextures2 then
    begin
      // Start looking in third texture file.
      maptex := maptex3;
      maxoff := maxoff3;
      directory := PIntegerArray(integer(maptex) + SizeOf(integer));
    end;

    offset := directory[0];

    if offset > maxoff then
      I_Error('R_InitTextures(): bad texture directory');

    mtexture := Pmaptexture_t(integer(maptex) + offset);

    textures[i] :=
      Z_Malloc(
        SizeOf(texture_t) + SizeOf(texpatch_t) * (mtexture.patchcount - 1),
          PU_STATIC, nil);
    texture := textures[i];

    texture.width := mtexture.width;
    texture.height := mtexture.height;
    texture.patchcount := mtexture.patchcount;
    {$IFNDEF OPENGL}
    texture.texture32 := nil;
    {$ENDIF}

    j := 0;
    while j < 8 do
    begin
      if mtexture.name[j] = #0 then
        break;
      texture.name[j] := toupper(mtexture.name[j]);
      inc(j);
    end;
    while j < 8 do
    begin
      texture.name[j] := #0;
      inc(j);
    end;

    mpatch := @mtexture.patches[0];
    patch := @texture.patches[0];

    for j := 0 to texture.patchcount - 1 do
    begin
      patch.originx := mpatch.originx;
      patch.originy := mpatch.originy;
      patch.patch := patchlookup[mpatch.patch];
      if patch.patch = -1 then
        I_Error('R_InitTextures(): Missing patch in texture %s', [char8tostring(texture.name)]);
      inc(mpatch);
      inc(patch);
    end;
    texturecolumnlump[i] := Z_Malloc(texture.width * SizeOf(texturecolumnlump[0][0]), PU_STATIC, nil);
    texturecolumnofs[i] := Z_Malloc(texture.width * SizeOf(texturecolumnofs[0][0]), PU_STATIC, nil);

    texturewidth[i] := texture.width;
    textureheight[i] := texture.height * FRACUNIT;
    // JVAL: 20200112 - For tall textures
    texturecolumnheight[i] := texture.height;
    if texturecolumnheight[i] < 128 then
      texturecolumnheight[i] := 128;
    texturecolumnheightfrac[i] := texturecolumnheight[i] * FRACUNIT;

    incp(pointer(directory), SizeOf(integer));
  end;

  memfree(pointer(patchlookup), nummappatches * SizeOf(integer));

  Z_Free(maptex1);
  if maptex2 <> nil then
    Z_Free(maptex2);
  if maptex3 <> nil then
    Z_Free(maptex3);

  // Precalculate whatever possible.
  for i := 0 to numtextures - 1 do
    R_GenerateLookup(i);

  // Create translation table for global animation.
  texturetranslation := Z_Malloc((numtextures + 1) * SizeOf(integer), PU_STATIC, nil);

  for i := 0 to numtextures - 1 do
    texturetranslation[i] := i;
end;

//
// R_InitFlats
//
procedure R_InitFlats;
var
  i: integer;
  lump: integer;
  flat: Pflat_t;
begin
  firstflat := W_GetFirstNumForName('F_START') + 1;
  lump := W_CheckFirstNumForName('FF_START') + 1;
  if lump > 0 then
    if lump < firstflat then
      firstflat := lump;
  lastflat := W_GetNumForName('F_END') - 1;
  lump := W_CheckNumForName('FF_END');
  if lump > lastflat then
    lastflat := lump;
  numflats := lastflat - firstflat + 1;

  // Create translation table for global animation.
  flats := Z_Malloc(numflats * SizeOf(pointer), PU_STATIC, nil);

  for i := 0 to numflats - 1 do
  begin
    flat := Z_Malloc(SizeOf(flat_t), PU_STATIC, nil);
    flat.name := W_GetNameForNum(firstflat + i);
    flat.translation := i;
    flat.lump := W_GetNumForName(flat.name);
    {$IFNDEF OPENGL}
    flat.flat32 := nil;
    {$ENDIF}
    // JVAL: 9 December 2007, Added terrain types
    flat.terraintype := P_TerrainTypeForName(flat.name);
    flats[i] := flat;
    flats[i].size := 0;
  end;
  R_ParseFlatInfoLumps;
end;

//
// R_InitSpriteLumps
// Finds the width and hoffset of all sprites in the wad,
//  so the sprite does not need to be cached completely
//  just for having the header info ready during rendering.
//
procedure R_InitSpriteLumps;
var
  i: integer;
  in_loop: boolean;
  patch: Ppatch_t;
  tmp: integer;
  lumpname: string;
begin
  VX_VoxelToSprite;

  firstspritelump := 0;
  for i := 0 to W_NumLumps - 1 do
  begin
    lumpname := char8tostring(W_GetNameForNum(i));
    if (lumpname = 'S_START') or (lumpname = 'SS_START') then
    begin
      firstspritelump := i + 1;
      break;
    end;
  end;

  lastspritelump := W_GetNumForName('S_END') - 1;
  tmp := W_CheckNumForName('SS_END');
  if tmp > 0 then
  begin
    dec(tmp);
    if lastspritelump < tmp then
      lastspritelump := tmp;
  end;

  if lastspritelump < firstspritelump then
  begin
    I_Warning('R_InitSpriteLumps(): WAD files have missplaced sprite markers (start=%d, end=%d)'#13#10, [firstspritelump, lastspritelump]);
    lastspritelump := W_NumLumps;
  end;
  W_InitSprites; // JVAL: Images as sprites
  numspritelumps := lastspritelump - firstspritelump + 1;
  spritewidth := Z_Malloc(numspritelumps * SizeOf(fixed_t), PU_STATIC, nil);
  spriteoffset := Z_Malloc(numspritelumps * SizeOf(fixed_t), PU_STATIC, nil);
  spritetopoffset := Z_Malloc(numspritelumps * SizeOf(fixed_t), PU_STATIC, nil);
  spritepresent := Z_Malloc(numspritelumps * SizeOf(boolean), PU_STATIC, nil);

  in_loop := true;

  for i := 0 to numspritelumps - 1 do
  begin
    spritewidth[i] := 0;
    spriteoffset[i] := 0;
    spritetopoffset[i] := 0;
    spritepresent[i] := false;
    lumpname := char8tostring(W_GetNameForNum(firstspritelump + i));
    if (lumpname = 'SS_START') or (lumpname = 'S_START') then
      in_loop := true
    else if (lumpname = 'SS_END') or (lumpname = 'S_END') then
      in_loop := false
    else if in_loop then
    begin
      patch := W_CacheSpriteNum(firstspritelump + i, PU_STATIC); // JVAL: Images as sprites
      spritewidth[i] := patch.width * FRACUNIT;
      spriteoffset[i] := patch.leftoffset * FRACUNIT;
      spritetopoffset[i] := patch.topoffset * FRACUNIT;
      spritepresent[i] := true;
      Z_ChangeTag(patch, PU_CACHE);
    end;
  end;
end;

//
// R_InitColormaps
//
procedure R_InitColormaps;
var
  lump: integer;
  length: integer;
  i: integer;
  palette: PByteArray;
  cpal: array[0..255] of LongWord;
  src: PByteArray;
  dest: PLongWord;
begin
  palette := V_ReadPalette(PU_STATIC);

  dest := @cpal[0];
  src := palette;
  while integer(src) < integer(@palette[256 * 3]) do
  begin
    dest^ := (LongWord(src[0]) shl 16) or
             (LongWord(src[1]) shl 8) or
             (LongWord(src[2]));
    inc(dest);
    src := PByteArray(integer(src) + 3);
  end;

  aprox_black := V_FindAproxColorIndex(@cpal, $0, 1, 255);
  aprox_red := V_FindAproxColorIndex(@cpal, $FF0000, 1, 255);

  Z_ChangeTag(palette, PU_CACHE);

  // Load in the light tables,
  //  256 byte align tables.
  lump := W_GetNumForName('COLORMAP');
  length := W_LumpLength(lump);
  colormaps := Z_Malloc(length, PU_STATIC, nil);
  colormaps32 := Z_Malloc(length * SizeOf(LongWord), PU_STATIC, nil);
  W_ReadLump(lump, colormaps);
  for i := 0 to length - 1 do
    if colormaps[i] = 0 then
      colormaps[i] := aprox_black;
  v_translation := colormaps;

  // JVAL: Mars fog sectors
  lump := W_GetNumForName('FOGMAP');
  length := W_LumpLength(lump);
  fog_colormaps := Z_Malloc(length, PU_STATIC, nil);
  fog_colormaps32 := Z_Malloc(length * SizeOf(LongWord), PU_STATIC, nil);
  W_ReadLump(lump, fog_colormaps);
  for i := 0 to length - 1 do
  begin
    if fog_colormaps[i] = 0 then
      fog_colormaps[i] := aprox_black;
    if colormaps[i] = aprox_black then
      fog_colormaps[i] := aprox_black;
    fog_colormaps32[i] := cpal[fog_colormaps[i]]; // JVAL: Mars fog sectors
  end;
end;

//
// R_InitData
// Locates all the lumps
//  that will be used by all views
// Must be called after W_Init.
//
procedure R_InitData;
begin
  R_InitHiRes;
  R_InitTextures;
  R_InitFlats;
  R_InitSpriteLumps;
  R_InitColormaps;
{$IFNDEF OPENGL}
  R_InitFuzzTable;
{$ENDIF}
end;

//
// R_FlatNumForName
// Retrieval, get a flat number for a flat name.
//
function R_FlatNumForName(const name: string): integer;
var
  i: integer;
  s: string;
begin
  i := W_CheckNumForName2(name, firstflat, lastflat);
  if i > -1 then
    result := i - firstflat
  else
  begin
    i := W_CheckNumForName(name);
    if i = -1 then  // JVAL: VERSION 204
    begin
      i := W_CheckNumForName('-NO-FLAT');
      if i = -1 then
        I_Error('R_FlatNumForName(): %s not found!', [name]);
    end;

    s := strupper(name);
    result := M_HashIndex(s);
    if result >= 0 then
      if result < numflats then
        if flats[result].lump = i then
          Exit;

    result := numflats;
    while result > 0 do
    begin
      dec(result);
      if flats[result].lump = i then
      begin
        M_HashUpdate(s, result);
        exit;
      end;
    end;

    // JVAL: Found a flat outside F_START, F_END
    result := numflats;
    inc(numflats);
    flats := Z_ReAlloc(flats, numflats * SizeOf(pointer), PU_STATIC, nil);

    flats[result] := Z_Malloc(SizeOf(flat_t), PU_STATIC, nil);
    flats[result].name := W_GetNameForNum(i);
    flats[result].translation := result;
    flats[result].lump := i;
    {$IFNDEF OPENGL}
    flats[result].flat32 := nil;
    {$ENDIF}
    // JVAL: 9 December 2007, Added terrain types
    flats[result].terraintype := P_TerrainTypeForName(flats[result].name);
    M_HashUpdate(s, result);
  end;
end;

function R_SafeFlatNumForName(const name: string): integer;
var
  i: integer;
  s: string;
begin
  i := W_CheckNumForName2(name, firstflat, lastflat);
  if i > -1 then
    result := i - firstflat
  else
  begin
    i := W_CheckNumForName(name);
    if i = -1 then
    begin
      result := -1;
      exit;
    end;

    s := strupper(name);
    result := M_HashIndex(s);
    if result >= 0 then
      if result < numflats then
        if flats[result].lump = i then
          Exit;

    result := numflats;
    while result > 0 do
    begin
      dec(result);
      if flats[result].lump = i then
      begin
        M_HashUpdate(s, result);
        exit;
      end;
    end;

    // JVAL: Found a flat outside F_START, F_END
    result := numflats;
    inc(numflats);
    flats := Z_ReAlloc(flats, numflats * SizeOf(pointer), PU_STATIC, nil);

    flats[result] := Z_Malloc(SizeOf(flat_t), PU_STATIC, nil);
    flats[result].name := W_GetNameForNum(i);
    flats[result].translation := result;
    flats[result].lump := i;
    {$IFNDEF OPENGL}
    flats[result].flat32 := nil;
    {$ENDIF}
    // JVAL: 9 December 2007, Added terrain types
    flats[result].terraintype := P_TerrainTypeForName(flats[result].name);
    M_HashUpdate(s, result);
  end
end;

//
// R_CheckTextureNumForName
// Check whether texture is available.
// Filter out NoTexture indicator.
//
function R_CheckTextureNumForName(const name: string): integer;
var
  i: integer;
  s: string;
  check: name8_t;
begin
  // "NoTexture" marker.
  if name = '' then
  begin
    result := -1;
    exit;
  end;

  // "NoTexture" marker.
  if name[1] = '-' then
  begin
    result := 0;
    exit;
  end;

  s := strupper(name);
  check.s := stringtochar8(s);
  result := M_HashIndex(s);
  if result >= 0 then
    if result < numtextures then
      if name8_t(textures[result].name).x[0] = check.x[0] then
        if name8_t(textures[result].name).x[1] = check.x[1] then
          exit;

  for i := 0 to numtextures - 1 do
    if name8_t(textures[i].name).x[0] = check.x[0] then
      if name8_t(textures[i].name).x[1] = check.x[1] then
      begin
        result := i;
        M_HashUpdate(s, result);
        exit;
      end;

  result := -1;
end;

function R_SafeTextureNumForName(const name: string): integer;
var
  i: integer;
  s: string;
  check: name8_t;
begin
  if name = '' then
  begin
    I_Warning('R_SafeTextureNumForName(): Texture name is null.'#13#10);
    result := -1;
    exit;
  end;

  // "NoTexture" marker.
  if name[1] = '-' then
  begin
    result := -1;
    exit;
  end;

  s := strupper(name);
  check.s := stringtochar8(s);
  result := M_HashIndex(s);
  if result >= 0 then
    if result < numtextures then
      if name8_t(textures[result].name).x[0] = check.x[0] then
        if name8_t(textures[result].name).x[1] = check.x[1] then
          exit;

  for i := 0 to numtextures - 1 do
    if name8_t(textures[i].name).x[0] = check.x[0] then
      if name8_t(textures[i].name).x[1] = check.x[1] then
      begin
        result := i;
        M_HashUpdate(s, result);
        exit;
      end;

  I_Warning('R_SafeTextureNumForName(): %s not found.'#13#10, [name]);
  result := -1;
end;

//
// R_TextureNumForName
// Calls R_CheckTextureNumForName,
//  aborts with error message.
//
function R_TextureNumForName(const name: string): integer;
begin
  result := R_CheckTextureNumForName(name);

  if result = -1 then
    I_Error('R_TextureNumForName(): %s not found!', [name]);
end;

function R_NameForSideTexture(const sn: SmallInt): char8_t;
begin
  ZeroMemory(@result, SizeOf(Result));
  Result[0] := '-';

  if sn < 0 then
    exit;

  if sn >= numtextures then
    exit;

  Result := textures[sn].name;
end;

function R_CacheFlat(const lump: integer; const tag: integer): pointer;
begin
  result := W_CacheLumpNum(lump, tag);
end;

//
// R_PrecacheLevel
// Preloads all relevant graphics for the level.
//
procedure R_PrecacheLevel;
var
  flatpresent: PByteArray;
  texturepresent: PByteArray;
  sprpresent: PByteArray;
  i: integer;
  j: integer;
  k: integer;
  lump: integer;
  texture: Ptexture_t;
  th: Pthinker_t;
  sf: Pspriteframe_t;
  flatmemory: integer;
  texturememory: integer;
  spritememory: integer;
  allocmemory: integer;
  flat: pointer;
  sd: Pside_t;
begin
  printf('R_PrecacheLevel()'#13#10);

  // Precache flats.
  flatpresent := mallocz(numflats);

  for i := 0 to numsectors - 1 do
  begin
    flatpresent[sectors[i].floorpic] := 1;
    flatpresent[sectors[i].ceilingpic] := 1;
  end;

  flatmemory := 0;
  allocmemory := GetAllocMemSize;

  printf(' Precaching flats'#13#10);
  for i := 0 to numflats - 1 do
  begin
    if flatpresent[i] <> 0 then
    begin
      flat := W_CacheLumpNum(R_GetLumpForFlat(i), PU_STATIC);
{$IFNDEF OPENGL}
      R_ReadDS32Cache(i);
{$ENDIF}
      Z_ChangeTag(flat, PU_CACHE);
      flatmemory := flatmemory + 64 * 64;
    end;
  end;

  allocmemory := GetAllocMemSize - allocmemory;
  printf('%6d KB memory usage for flats'#13#10, [(flatmemory + allocmemory) div 1024]);

  // Precache textures.
  texturepresent := mallocz(numtextures);

  sd := @sides[numsides];
  while sd <> @sides[0] do
  begin
    dec(sd);
    texturepresent[sd.toptexture] := 1;
    texturepresent[sd.midtexture] := 1;
    texturepresent[sd.bottomtexture] := 1;
  end;

  // Sky texture is always present.
  // Note that F_SKY1 is the name used to
  //  indicate a sky floor/ceiling as a flat,
  //  while the sky texture is stored like
  //  a wall texture, with an episode dependend
  //  name.
  texturepresent[skytexture] := 1;

  texturememory := 0;
  allocmemory := GetAllocMemSize;

  printf(' Precaching textures'#13#10);
{$IFNDEF OPENGL}
  dc_mod := 0;
  dc_texturemod := 0;
{$ENDIF}
  for i := 0 to numtextures - 1 do
  begin
    if texturepresent[i] = 0 then
      continue;

    texture := textures[i];

    for j := 0 to texture.patchcount - 1 do
    begin
      lump := texture.patches[j].patch;
      texturememory := texturememory + lumpinfo[lump].size;
      W_CacheLumpNum(lump, PU_CACHE);
    end;
{$IFNDEF OPENGL}
    R_Precache32bittexture(i);
{$ENDIF}
  end;
  allocmemory := GetAllocMemSize - allocmemory;
  printf('%6d KB memory usage for textures'#13#10, [(texturememory + allocmemory) div 1024]);

  // Precache sprites.
  sprpresent := mallocz(numspritespresent);

  th := thinkercap.next;
  while th <> @thinkercap do
  begin
    if @th._function.acp1 = @P_MobjThinker then
      sprpresent[Pmobj_t(th).sprite] := 1;
    th := th.next;
  end;

  spritememory := 0;
  allocmemory := GetAllocMemSize;

  printf(' Precaching sprites'#13#10);
  for i := 0 to numspritespresent - 1 do
  begin
    if sprpresent[i] <> 0 then
    begin
      for j := 0 to sprites[i].numframes - 1 do
      begin
        sf := @sprites[i].spriteframes[j];
        for k := 0 to 7 do
        begin
          lump := firstspritelump + sf.lump[k];
          spritememory := spritememory + lumpinfo[lump].size;
          W_CacheLumpNum(lump, PU_CACHE);
        end;
      end;
    end;
  end;
  allocmemory := GetAllocMemSize - allocmemory;
  printf('%6d KB memory usage for sprites'#13#10, [(spritememory + allocmemory) div 1024]);

  memfree(pointer(flatpresent), numflats);
  memfree(pointer(texturepresent), numtextures);
  memfree(pointer(sprpresent), numspritespresent);
end;

procedure R_SetupLevel;
begin
  maxvisplane := -1;
  {$IFNDEF OPENGL}
  R_InitFixedColumn;
  maxvisplane3d := -1;  // JVAL: 3d Floors
  maxvisslope := -1;  // JVAL: Slopes
  {$ENDIF}
  max_ds_p := -1;
  maxvissprite := -1;
end;

end.

