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
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit gl_tex;

interface

uses
  d_delphi,
  dglOpenGL,
  r_defs,
  t_main,
  gl_defs,
  w_wad,
  z_zone;

var
// TEXTURES
  gld_GLTextures: PGLTexturePArray = nil;
// PATCHES FLATS SPRITES
  gld_GLPatchTextures: PGLTexturePArray = nil;

  use_mipmapping: boolean = false;
  use_multitexturing: boolean = false;
  ActiveDetailTexture: boolean = false;

  gld_max_texturesize: integer = 0;
  gld_max_texturesize3d: integer = 0;

  last_gltexture: PGLTexture = nil;
  last_cm: integer = -1;

  transparent_pal_index: integer;
  gld_palmap: array[0..255] of byte;

procedure gld_InitPalettedTextures;

function gld_GetTexDimension(value: integer): integer;

function gld_AddNewGLTexture(texture_num: integer): PGLTexture;

function gld_AddNewGLPatchTexture(lump: integer): PGLTexture;

procedure gld_SetTexturePalette(target: TGLenum);

procedure gld_AddPatchToTexture_UnTranslated(gltexture: PGLTexture; buffer: PByteArray;
  const patch: Ppatch_t; originx, originy: integer; paletted: boolean);

procedure gld_AddPatchToTexture(gltexture: PGLTexture; buffer: PByteArray;
  const patch: Ppatch_t; originx, originy: integer; cm: integer; paletted: boolean);

procedure gld_AddFlatToTexture(gltexture: PGLTexture; buffer: PByteArray; const flat: PByteArray;
  paletted: boolean);

function gld_RegisterTexture(texture_num: integer; mipmap:  boolean): PGLTexture;

procedure gld_BindTexture(gltexture: PGLTexture);

function gld_RegisterPatch(lump: integer; cm: integer; const unload: boolean = true): PGLTexture;

procedure gld_BindPatch(gltexture: PGLTexture; cm: integer);

function gld_RegisterFlat(lump: integer; mipmap: boolean; flat: integer): PGLTexture;

procedure gld_BindFlat(gltexture: PGLTexture);

procedure gld_CleanTextures;

procedure gld_CleanPatchTextures;

procedure gld_ShutDownTextures;

procedure gld_Precache;

procedure gld_ResetLastTexture;

function gld_LoadExternalTexture(const texname: string; const transparent: boolean; const texmode: GLUint): GLUint; overload;

function gld_LoadExternalTexture(const t: PTexture; const transparent: boolean; const texmode: GLUint): GLUint; overload;

function gld_LoadExternalTextureAlpha(const texname: string; const alpha: byte; const texmode: GLUint): GLUint; overload;

function gld_LoadExternalTextureAlpha(const texname: string; const alphatexname: string; const texmode: GLUint): GLUint; overload;

procedure gld_ClearTextureMemory;

function gld_GetCurrTexFiltering: gl_filter_t;

procedure gld_SetCurrTexFiltering(const flt: gl_filter_t);

implementation

uses
  {$IFDEF HEXEN}
  g_demo,
  {$ENDIF}
  doomdef,
  d_think,
  g_game,
  gl_misc,
  mt_utils,
  info,
  p_mobj,
  p_mobj_h,
  p_setup,
  p_tick,
  r_data,
  r_draw,
  r_flatinfo,
  r_sky,
  r_things,
  r_hires,
  v_data,
  w_sprite;

procedure gld_InitPalettedTextures;
var
  playpal: PByteArray;
  pal: array[0..255] of LongWord;
  i, j: integer;
begin
  playpal := V_ReadPalette(PU_STATIC);
  for i := 0 to 255 do
  begin
    pal[i] := _SHL(playpal[i * 3], 16) or _SHL(playpal[i * 3 + 1], 8) or playpal[i * 3 + 2];
    gld_palmap[i] := i;
  end;
//  Z_ChangeTag(playpal, PU_CACHE);
  transparent_pal_index := -1;
  for i := 0 to 255 do
  begin
    for j := i + 1 to 255 do
      if pal[i] = pal[j] then
      begin
        transparent_pal_index := j;
        gld_palmap[j] := i;
        break;
      end;
    if transparent_pal_index >= 0 then
      break;
  end;
end;

function gld_GetTexDimension(value: integer): integer;
begin
  result := 1;
  while result < value do
    result := result * 2;
  if result > gld_max_texturesize then
    result := gld_max_texturesize;
end;

function gld_AddNewGLTexture(texture_num: integer): PGLTexture;
begin
  if (texture_num < 0) or (texture_num >= numtextures) then
  begin
    result := nil;
    exit;
  end;
  if gld_GLTextures = nil then
    gld_GLTextures := mallocz(numtextures * SizeOf(PGLTexture));
  if gld_GLTextures[texture_num] = nil then
  begin
    gld_GLTextures[texture_num] := mallocz(SizeOf(GLTexture));
    gld_GLTextures[texture_num].textype := GLDT_UNREGISTERED;
    gld_GLTextures[texture_num].texturescale := 1.0;
  end;
  result := gld_GLTextures[texture_num];
end;

function gld_AddNewGLPatchTexture(lump: integer): PGLTexture;
var
  numlumps: integer;
begin
  numlumps := W_NumLumps;
  if (lump < 0) or (lump >= numlumps) then
  begin
    result := nil;
    exit;
  end;
  if gld_GLPatchTextures = nil then
    gld_GLPatchTextures := mallocz(numlumps * SizeOf(PGLTexture));
  if gld_GLPatchTextures[lump] = nil then
  begin
    gld_GLPatchTextures[lump] := mallocz(SizeOf(GLTexture));
    gld_GLPatchTextures[lump].textype := GLDT_UNREGISTERED;
    gld_GLPatchTextures[lump].texturescale := 1.0;
  end;
  result := gld_GLPatchTextures[lump];
end;

procedure gld_SetTexturePalette(target: TGLenum);
var
  playpal: PByteArray;
  pal: array[0..1023] of byte;
  pl, pp: PLongWord;
  i: integer;
begin
  // JVAL: 28/9/2009 - General routine optimization
  playpal := V_ReadPalette(PU_STATIC);
  pl := PLongWord(playpal);
  pp := PLongWord(@pal);
  for i := 0 to 255 do
  begin
    pp^ := pl^ or $FF000000;
    pp := PLongWord(integer(pp) + 4);
    pl := PLongWord(integer(pl) + 3);
{ Old code:
    pal[i * 4] := playpal[i * 3];
    pal[i * 4 + 1] := playpal[i * 3 + 1];
    pal[i * 4 + 2] := playpal[i * 3 + 2];
    pal[i * 4 + 3] := 255;
}
  end;
  Z_ChangeTag(playpal, PU_CACHE);
  PLongWordArray(@pal)[transparent_pal_index] := 0;
  gld_ColorTableEXT(target, GL_RGBA, 256, GL_RGBA, GL_UNSIGNED_BYTE, @pal);
end;

procedure gld_AddPatchToTexture_UnTranslated(gltexture: PGLTexture; buffer: PByteArray;
  const patch: Ppatch_t; originx, originy: integer; paletted: boolean);
var
  x, y, j: integer;
  xs, xe: integer;
  js,je: integer;
  column: Pcolumn_t;
  source: PByteArray;
  pos: integer;
  playpal: PByteArray;
  ss: integer;
  delta, prevdelta: integer;
  tallpatch: boolean;
begin
  if (gltexture = nil) or (patch = nil) then
    exit;

  playpal := V_ReadPalette(PU_STATIC);
  xs := 0;
  xe := patch.width;
  if (xs + originx) >= gltexture.realtexwidth then
    exit;
  if (xe + originx) <= 0 then
    exit;
  if (xs + originx) < 0 then
    xs := -originx;
  if (xe + originx) > gltexture.realtexwidth then
    xe := xe + gltexture.realtexwidth - (xe + originx);
  for x := xs to xe - 1 do
  begin
    delta := 0;
    tallpatch := false;
    column := Pcolumn_t(integer(patch) + patch.columnofs[x]);
    while column.topdelta <> 255 do
    begin
      delta := delta + column.topdelta;
      y := (delta + originy);
      js := 0;
      je := column.length;
      if (js + y >= gltexture.realtexheight) or (je + y <= 0) then
      begin
        column := Pcolumn_t(integer(column) + column.length + 4);
        delta := 0;
        continue;
      end;
      if js + y < 0 then
        js := -y;
      if je + y > gltexture.realtexheight then
        je := je + gltexture.realtexheight - (je + y);
      source := PByteArray(integer(column) + 3);
      if paletted then
      begin
        pos := (js + y) * gltexture.buffer_width + x + originx;
        j := js;
        while j < je do
        begin
          buffer[pos] := gld_palmap[source[j]];
          inc(j);
          pos := pos + gltexture.buffer_width;
        end;
      end
      else
      begin
        pos := 4 * ((js + y) * gltexture.buffer_width + x + originx);
        j := js;
        while j < je do
        begin
          ss := source[j] * 3;
          buffer[pos] := playpal[ss];
          buffer[pos + 1] := playpal[ss + 1];
          buffer[pos + 2] := playpal[ss + 2];
          buffer[pos + 3] := 255;
          inc(j);
          pos := pos + 4 * gltexture.buffer_width;
        end;
      end;
      if not tallpatch then
      begin
        prevdelta := column.topdelta;
        column := Pcolumn_t(integer(column) + column.length + 4);
        if column.topdelta > prevdelta then
          delta := 0
        else
          tallpatch := true;
      end
      else
        column := Pcolumn_t(integer(column) + column.length + 4);
    end;
  end;
  Z_ChangeTag(playpal, PU_CACHE);
end;

procedure gld_AddPatchToTexture(gltexture: PGLTexture; buffer: PByteArray;
  const patch: Ppatch_t; originx, originy: integer; cm: integer; paletted: boolean);
var
  x, y, j: integer;
  xs, xe: integer;
  js,je: integer;
  column: Pcolumn_t;
  source: PByteArray;
  pos: integer;
  playpal: PByteArray;
  trans: PByteArray;
  ss: integer;
  delta, prevdelta: integer;
  tallpatch: boolean;
begin
  if cm = Ord(CR_LIMIT) then
  begin
    gld_AddPatchToTexture_Untranslated(gltexture, buffer, patch, originx, originy, paletted);
    exit;
  end;

  if (gltexture = nil) or (patch = nil) then
    exit;

  playpal := V_ReadPalette(PU_STATIC);
  xs := 0;
  xe := patch.width;
  if xs + originx >= gltexture.realtexwidth then
    exit;
  if xe + originx <= 0 then
    exit;
  if cm < Ord(CR_LIMIT) then
    trans := @colorregions[cm]
  else
    trans := @translationtables[256 * (cm - Ord(CR_LIMIT) - 1)];
  if xs + originx < 0 then
    xs := -originx;
  if xe + originx > gltexture.realtexwidth then
    xe := xe + gltexture.realtexwidth - (xe + originx);
  for x := xs to xe - 1 do
  begin
    delta := 0;
    tallpatch := false;
    column := Pcolumn_t(integer(patch) + patch.columnofs[x]);
    while column.topdelta <> 255 do
    begin
      delta := delta + column.topdelta;
      y := (delta + originy);
      js := 0;
      je := column.length;
      if (js + y >= gltexture.realtexheight) or (je + y <= 0) then
      begin
        column := Pcolumn_t(integer(column) + column.length + 4);
        delta := 0;
        continue;
      end;
      if js + y < 0 then
        js := -y;
      if je + y > gltexture.realtexheight then
        je := je + gltexture.realtexheight - (je + y);
      source := PByteArray(integer(column) + 3);
      if paletted then
      begin
        pos := (js + y) * gltexture.buffer_width + x + originx;
        j := js;
        while j < je do
        begin
          buffer[pos] := gld_palmap[trans[source[j]]];
          inc(j);
          pos := pos + gltexture.buffer_width;
        end;
      end
      else
      begin
        pos := 4 * ((js + y) * gltexture.buffer_width + x + originx);
        j := js;
        while j < je do
        begin
          ss := trans[source[j]] * 3;
          buffer[pos] := playpal[ss];
          buffer[pos + 1] := playpal[ss + 1];
          buffer[pos + 2] := playpal[ss + 2];
          buffer[pos + 3] := 255;
          inc(j);
          pos := pos + 4 * gltexture.buffer_width;
        end;
      end;
      if not tallpatch then
      begin
        prevdelta := column.topdelta;
        column := Pcolumn_t(integer(column) + column.length + 4);
        if column.topdelta > prevdelta then
          delta := 0
        else
          tallpatch := true;
      end
      else
        column := Pcolumn_t(integer(column) + column.length + 4);
    end;
  end;
  Z_ChangeTag(playpal, PU_CACHE);
end;

procedure gld_AddFlatToTexture(gltexture: PGLTexture; buffer: PByteArray; const flat: PByteArray;
  paletted: boolean);
var
  x, y, pos: integer;
  yy: integer;
  playpal: PByteArray;
begin
  if (gltexture = nil) or (flat = nil) then
    exit;

  if paletted then
  begin
    for y := 0 to gltexture.realtexheight - 1 do
    begin
      pos := y * gltexture.buffer_width;
      yy := y * gltexture.realtexwidth;
      for x := 0 to gltexture.realtexwidth - 1 do
      begin
        buffer[pos] := gld_palmap[flat[yy + x]];
        inc(pos);
      end;
    end;
  end
  else
  begin
    playpal := V_ReadPalette(PU_STATIC);
    for y := 0 to gltexture.realtexheight - 1 do
    begin
      pos := 4 * (y * gltexture.buffer_width);
      for x := 0 to gltexture.realtexwidth - 1 do
      begin
        yy := flat[y * gltexture.realtexwidth + x] * 3;
        buffer[pos] := playpal[yy];
        buffer[pos + 1] := playpal[yy + 1];
        buffer[pos + 2] := playpal[yy + 2];
        buffer[pos + 3] := 255;
        inc(pos, 4);
      end;
    end;
    Z_ChangeTag(playpal, PU_CACHE);
  end;
end;

procedure gld_AddPatchForTransparencyCheck(gltexture: PGLTexture; buffer: PByteArray;
  const patch: Ppatch_t; originx, originy: integer);
var
  x, y, j: integer;
  xs, xe: integer;
  js,je: integer;
  column: Pcolumn_t;
  pos: integer;
  delta, prevdelta: integer;
  tallpatch: boolean;
begin
  xs := 0;
  xe := patch.width;
  if xs + originx >= gltexture.realtexwidth then
    exit;
  if xe + originx <= 0 then
    exit;
  if xs + originx < 0 then
    xs := -originx;
  if xe + originx > gltexture.realtexwidth then
    xe := xe + gltexture.realtexwidth - (xe + originx);
  for x := xs to xe - 1 do
  begin
    delta := 0;
    tallpatch := false;
    column := Pcolumn_t(integer(patch) + patch.columnofs[x]);
    while column.topdelta <> 255 do
    begin
      delta := delta + column.topdelta;
      y := (delta + originy);
      js := 0;
      je := column.length;
      if (js + y >= gltexture.realtexheight) or (je + y <= 0) then
      begin
        column := Pcolumn_t(integer(column) + column.length + 4);
        delta := 0;
        continue;
      end;
      if js + y < 0 then
        js := -y;
      if je + y > gltexture.realtexheight then
        je := je + gltexture.realtexheight - (je + y);
      pos := ((js + y) * gltexture.realtexwidth + x + originx);
      j := js;
      while j < je do
      begin
        buffer[pos] := 255;
        inc(j);
        pos := pos + gltexture.realtexwidth;
      end;
      if not tallpatch then
      begin
        prevdelta := column.topdelta;
        column := Pcolumn_t(integer(column) + column.length + 4);
        if column.topdelta > prevdelta then
          delta := 0
        else
          tallpatch := true;
      end
      else
        column := Pcolumn_t(integer(column) + column.length + 4);
    end;
  end;
end;

function gld_CheckTextureTransparency(gltexture: PGLTexture): boolean;
var
  buffer: PByteArray;
  pl: PLongWord;
  spot: integer;
  bufsize: integer;
  i: integer;
  patch: Ppatch_t;
  texpatch: Ptexpatch_t;
begin
  // JVAL: GLDT_LIGHT unused, light texture has default handling
  // Patches are always transparent
  if gltexture.textype in [GLDT_LIGHT, GLDT_PATCH] then
  begin
    result := true;
    exit;
  end;

  // JVAL: Flats are never transparent
  if gltexture.textype in [GLDT_FLAT, GLDT_SKY] then
  begin
    result := false;
    exit;
  end;

  // JVAL SKY is never transparent
  if gltexture.index = skytexture then
  begin
    result := false;
    exit;
  end;


  bufsize := gltexture.realtexwidth * gltexture.realtexheight;
  buffer := mallocz(bufsize);

  for i := 0 to textures[gltexture.index].patchcount - 1 do
  begin
    texpatch := @textures[gltexture.index].patches[i];
    patch := W_CacheLumpNum(texpatch.patch, PU_STATIC);
    gld_AddPatchForTransparencyCheck(gltexture, buffer, patch, texpatch.originx, texpatch.originy);
    Z_ChangeTag(patch, PU_CACHE);
  end;

  result := false;
  if bufsize > 3 then // JVAL: prevent extra small textures, unused ?
  begin
    spot := (bufsize - 3) and (not 3);
    pl := @buffer[spot];
    for i := 0 to (spot shr 2) - 1 do
    begin
      if pl^ <> $FFFFFFFF then
      begin
        result := true;
        break;
      end;
      dec(pl);
    end;

    for i := spot to bufsize - 1 do
    begin
      if buffer[i] <> 255 then
      begin
        result := true;
        break;
      end;
    end;
  end
  else
  begin
    for i := 0 to bufsize - 1 do
    begin
      if buffer[i] <> 255 then
      begin
        result := true;
        break;
      end;
    end;
  end;

  memfree(pointer(buffer), bufsize);
end;


function gld_LoadExternalTexture(const texname: string; const transparent: boolean; const texmode: GLUint): GLUint;
var
  t: PTexture;
  twidth: integer;
  theight: integer;
begin
  t := T_LoadHiResTexture(texname);
  if t = nil then
  begin
    result := 0;
    exit;
  end;

  t.ConvertTo32bit;
  t.SwapRGB;
  if not t.ExternalAlphaPresent then
  begin
    if transparent then
      t.SetDefaultAlphaChannel
    else
    begin
      t.Adjust32bitTransparency;
      t.SetAlphaChannel($FF);
    end;
  end;

  twidth := gld_GetTexDimension(t.GetWidth);
  theight := gld_GetTexDimension(t.GetHeight);
  t.ScaleTo(twidth, theight);

  glGenTextures(1, @result);
  glBindTexture(GL_TEXTURE_2D, result);

  if use_mipmapping then
  begin
    gluBuild2DMipmaps(GL_TEXTURE_2D, gl_tex_format,
                      twidth, theight,
                      GL_RGBA, GL_UNSIGNED_BYTE, t.GetImage);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, texmode);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, texmode);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, gl_tex_filter);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, gl_mipmap_filter);
    if gl_texture_filter_anisotropic then
      glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAX_ANISOTROPY_EXT, 2.0);
  end
  else
  begin
    glTexImage2D(GL_TEXTURE_2D, 0, gl_tex_format,
                 twidth, theight,
                 0, GL_RGBA, GL_UNSIGNED_BYTE, t.GetImage);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, texmode);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, texmode);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, gl_tex_filter);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, gl_tex_filter);
  end;
  dispose(t, destroy);
end;

function gld_LoadExternalTexture(const t: PTexture; const transparent: boolean; const texmode: GLUint): GLUint; overload;
var
  twidth: integer;
  theight: integer;
begin
  if t = nil then
  begin
    result := 0;
    exit;
  end;

  t.ConvertTo32bit;
  t.SwapRGB;
  if not t.ExternalAlphaPresent then
  begin
    if transparent then
      t.SetDefaultAlphaChannel
    else
    begin
      t.Adjust32bitTransparency;
      t.SetAlphaChannel($FF);
    end;
  end;
  twidth := gld_GetTexDimension(t.GetWidth);
  theight := gld_GetTexDimension(t.GetHeight);
  t.ScaleTo(twidth, theight);

  glGenTextures(1, @result);
  glBindTexture(GL_TEXTURE_2D, result);

  if use_mipmapping then
  begin
    gluBuild2DMipmaps(GL_TEXTURE_2D, gl_tex_format,
                      twidth, theight,
                      GL_RGBA, GL_UNSIGNED_BYTE, t.GetImage);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, texmode);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, texmode);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, gl_tex_filter);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, gl_mipmap_filter);
    if gl_texture_filter_anisotropic then
      glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAX_ANISOTROPY_EXT, 2.0);
  end
  else
  begin
    glTexImage2D(GL_TEXTURE_2D, 0, gl_tex_format,
                 twidth, theight,
                 0, GL_RGBA, GL_UNSIGNED_BYTE, t.GetImage);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, texmode);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, texmode);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, gl_tex_filter);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, gl_tex_filter);
  end;
end;

function gld_LoadExternalTextureAlpha(const texname: string; const alpha: byte; const texmode: GLUint): GLUint;
var
  t: PTexture;
  twidth: integer;
  theight: integer;
begin
  t := T_LoadHiResTexture(texname);
  if t = nil then
  begin
    result := 0;
    exit;
  end;

  t.ConvertTo32bit;
  t.SwapRGB;
  t.SetAlphaChannel(alpha);

  twidth := gld_GetTexDimension(t.GetWidth);
  theight := gld_GetTexDimension(t.GetHeight);
  t.ScaleTo(twidth, theight);

  glGenTextures(1, @result);
  glBindTexture(GL_TEXTURE_2D, result);

  if use_mipmapping then
  begin
    gluBuild2DMipmaps(GL_TEXTURE_2D, gl_tex_format,
                      twidth, theight,
                      GL_RGBA, GL_UNSIGNED_BYTE, t.GetImage);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, texmode);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, texmode);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, gl_tex_filter);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, gl_mipmap_filter);
    if gl_texture_filter_anisotropic then
      glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAX_ANISOTROPY_EXT, 2.0);
  end
  else
  begin
    glTexImage2D(GL_TEXTURE_2D, 0, gl_tex_format,
                 twidth, theight,
                 0, GL_RGBA, GL_UNSIGNED_BYTE, t.GetImage);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, texmode);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, texmode);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, gl_tex_filter);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, gl_tex_filter);
  end;
  dispose(t, destroy);
end;

function gld_LoadExternalTextureAlpha(const texname: string; const alphatexname: string; const texmode: GLUint): GLUint;
var
  t: PTexture;
  a: PTexture;
  twidth: integer;
  theight: integer;
begin
  t := T_LoadHiResTexture(texname);
  if t = nil then
  begin
    result := 0;
    exit;
  end;

  a := T_LoadHiResTexture(alphatexname);
  if a = nil then
  begin
    result := 0;
    exit;
  end;

  t.ConvertTo32bit;
  t.SwapRGB;
  t.SetAlphaChannelFromImage(a);
  dispose(a, destroy);

  twidth := gld_GetTexDimension(t.GetWidth);
  theight := gld_GetTexDimension(t.GetHeight);
  t.ScaleTo(twidth, theight);

  glGenTextures(1, @result);
  glBindTexture(GL_TEXTURE_2D, result);

  if use_mipmapping then
  begin
    gluBuild2DMipmaps(GL_TEXTURE_2D, gl_tex_format,
                      twidth, theight,
                      GL_RGBA, GL_UNSIGNED_BYTE, t.GetImage);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, texmode);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, texmode);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, gl_tex_filter);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, gl_mipmap_filter);
    if gl_texture_filter_anisotropic then
      glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAX_ANISOTROPY_EXT, 2.0);
  end
  else
  begin
    glTexImage2D(GL_TEXTURE_2D, 0, gl_tex_format,
                 twidth, theight,
                 0, GL_RGBA, GL_UNSIGNED_BYTE, t.GetImage);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, texmode);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, texmode);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, gl_tex_filter);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, gl_tex_filter);
  end;
  dispose(t, destroy);
end;

function gld_LoadHiresTexture(gltexture: PGLTexture; const texname: string): boolean;
var
  t: PTexture;
  buffer: PByteArray;
  twidth: integer;
  theight: integer;
  tfactor: integer;
  i: integer;
  gltexture_width: integer;
  gltexture_height: integer;
  gltexture_realtexwidth: integer;
  gltexture_realtexheight: integer;
  gltexture_tex_width: integer;
  gltexture_tex_height: integer;
  gltexture_buffer_width: integer;
  gltexture_buffer_height: integer;
  gltexture_buffer_size: integer;
begin
  if not useexternaltextures then
  begin
    result := false;
    exit;
  end;

  t := T_LoadHiResTexture(texname);
  if t = nil then
  begin
    result := false;
    exit;
  end;

  t.ConvertTo32bit;
  t.SwapRGB;
  if gltexture.textype in [GLDT_FLAT, GLDT_SKY] then
  begin
    t.RemoveTransparency;
  end
  else if not t.ExternalAlphaPresent then
  begin
    if gld_CheckTextureTransparency(gltexture) then
      t.SetDefaultAlphaChannel
    else
    begin
      t.Adjust32bitTransparency;
      // JVAL:
      //  Set non transparent texture, setting Alpha Value to a lower value
      //  makes the texture transparent
      t.SetAlphaChannel($FF);
    end;
  end;

  theight := t.GetHeight;
  tfactor := theight div gltexture.height; // Scaling
  i := 0;
  while 1 shl i < gld_max_texturesize do
  begin
    if tfactor <= 1 shl i then
      break;
    inc(i);
  end;
  // JVAL Final adjustment of hi resolution textures
  twidth := (1 shl i) * gltexture.width;
  theight := (1 shl i) * gltexture.height;
  while (twidth > gld_max_texturesize) or (theight > gld_max_texturesize) do
  begin
    dec(i);
    twidth := (1 shl i) * gltexture.width;
    theight := (1 shl i) * gltexture.height;
  end;
  t.ScaleTo(twidth, theight); // JVAL Scale the texture if needed

  gltexture_width := t.GetWidth;
  gltexture_height := t.GetHeight;
  gltexture_realtexwidth := gltexture_width;
  gltexture_realtexheight := gltexture_height;
  gltexture_tex_width := gld_GetTexDimension(gltexture_realtexwidth);
  gltexture_tex_height := gld_GetTexDimension(gltexture_realtexheight);
  if gltexture.mipmap and use_mipmapping then
  begin
    gltexture_width := gltexture_tex_width;
    gltexture_height := gltexture_tex_height;
    gltexture_buffer_width := gltexture_realtexwidth;
    gltexture_buffer_height := gltexture_realtexheight;
  end
  else
  begin
    gltexture_width := gl_i_min(gltexture_realtexwidth, gltexture_tex_width);
    gltexture_height := gl_i_min(gltexture_realtexheight, gltexture_tex_height);
    gltexture_buffer_width := gltexture_tex_width;
    gltexture_buffer_height := gltexture_tex_height;
  end;
  gltexture_buffer_size := gltexture_buffer_width * gltexture_buffer_height * 4;

  if gltexture.glTexID[Ord(CR_DEFAULT)] = 0 then
    glGenTextures(1, @gltexture.glTexID[Ord(CR_DEFAULT)]);
  glBindTexture(GL_TEXTURE_2D, gltexture.glTexID[Ord(CR_DEFAULT)]);

  if (gltexture_tex_width = gltexture_width) and (gltexture_tex_height = gltexture_height) then
  begin
    if gltexture.mipmap and use_mipmapping then
    begin
      gluBuild2DMipmaps(GL_TEXTURE_2D, gl_tex_format,
                        gltexture_buffer_width, gltexture_buffer_height,
                        GL_RGBA, GL_UNSIGNED_BYTE, t.GetImage);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, gl_tex_filter);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, gl_mipmap_filter);
      if gl_texture_filter_anisotropic then
        glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAX_ANISOTROPY_EXT, 2.0);
    end
    else
    begin
      glTexImage2D(GL_TEXTURE_2D, 0, gl_tex_format,
                   gltexture_buffer_width, gltexture_buffer_height,
                   0, GL_RGBA, GL_UNSIGNED_BYTE, t.GetImage);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, gl_tex_filter);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, gl_tex_filter);
    end;
  end
  else
  begin
    if gltexture_buffer_width = gltexture_width then
    begin
      buffer := malloc(gltexture_buffer_size);
      memcpy(buffer, t.GetImage, gltexture_width * gltexture_height * 4);
    end
    else
    begin
      buffer := mallocz(gltexture_buffer_size);
      theight := t.GetHeight;
      twidth := t.GetWidth;
      for i := 0 to theight - 1 do
        t.GetRow32(i, tWidth, @buffer[i * gltexture_buffer_width * 4]);
    end;

    if gltexture.mipmap and use_mipmapping then
    begin
      gluBuild2DMipmaps(GL_TEXTURE_2D, gl_tex_format,
                        gltexture_buffer_width, gltexture_buffer_height,
                        GL_RGBA, GL_UNSIGNED_BYTE, buffer);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, gl_tex_filter);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, gl_mipmap_filter);
      if gl_texture_filter_anisotropic then
        glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAX_ANISOTROPY_EXT, 2.0);
    end
    else
    begin
      glTexImage2D(GL_TEXTURE_2D, 0, gl_tex_format,
                   gltexture_buffer_width, gltexture_buffer_height,
                   0, GL_RGBA, GL_UNSIGNED_BYTE, buffer);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, gl_tex_filter);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, gl_tex_filter);
    end;

    memfree(pointer(buffer), gltexture_buffer_size);
  end;
  dispose(t, destroy);

  result := true;
end;

function gld_RegisterTexture(texture_num: integer; mipmap:  boolean): PGLTexture;
var
  texture: Ptexture_t;
begin
  result := gld_AddNewGLTexture(texture_num);
  if result = nil then
    exit;

  if result.textype = GLDT_UNREGISTERED then
  begin
    if (texture_num >= 0) or (texture_num < numtextures) then
      texture := textures[texture_num]
    else
      exit;
    result.textype := GLDT_BROKEN;
    result.index := texture_num;
    result.mipmap := mipmap;
    result.realtexwidth := texture.width;
    result.realtexheight := texture.height;
    result.leftoffset := 0;
    result.topoffset := 0;
    result.tex_width := gld_GetTexDimension(result.realtexwidth);
    result.tex_height := gld_GetTexDimension(result.realtexheight);
    if result.mipmap and use_mipmapping then
    begin
      result.width := result.tex_width;
      result.height := result.tex_height;
      result.buffer_width := result.realtexwidth;
      result.buffer_height := result.realtexheight;
    end
    else
    begin
      result.width := gl_i_min(result.realtexwidth, result.tex_width);
      result.height := gl_i_min(result.realtexheight, result.tex_height);
      result.buffer_width := result.tex_width;
      result.buffer_height := result.tex_height;
    end;
    result.buffer_size := result.buffer_width * result.buffer_height * 4;
    result.heightscale := result.height / result.tex_height;
    if result.realtexwidth > result.buffer_width then
      exit;
    if result.realtexheight > result.buffer_height then
      exit;
    result.textype := GLDT_TEXTURE;
  end;
end;

procedure gld_BindTexture(gltexture: PGLTexture);
var
  patch: Ppatch_t;
  texpatch: Ptexpatch_t;
  i: integer;
  buffer: PByteArray;
  skyhack: boolean;
begin
  if gltexture = last_gltexture then
    exit;
  last_gltexture := gltexture;
  if gltexture = nil then
  begin
    glBindTexture(GL_TEXTURE_2D, 0);
{$IFDEF DEBUG}
    glColor4f(1.0, 0.0, 0.0, 1.0);
{$ENDIF}
    last_cm := -1;
    exit;
  end;
  if gltexture.textype <> GLDT_TEXTURE then
    if gltexture.textype <> GLDT_SKY then
    begin
      glBindTexture(GL_TEXTURE_2D, 0);
      last_gltexture := nil;
      last_cm := -1;
      exit;
    end;
  if gltexture.glTexID[Ord(CR_DEFAULT)] <> 0 then
  begin
    glBindTexture(GL_TEXTURE_2D, gltexture.glTexID[Ord(CR_DEFAULT)]);
    glGetTexParameteriv(GL_TEXTURE_2D, GL_TEXTURE_RESIDENT, @i);
{    if i = GL_TRUE then    }
      exit;
  end;

  if gld_LoadHiresTexture(gltexture, textures[gltexture.index].name) then
    exit;

  buffer := malloc(gltexture.buffer_size);
  if (not (gltexture.mipmap and use_mipmapping)) and gl_paletted_texture then
    memset(buffer, transparent_pal_index, gltexture.buffer_size)
  else
    MT_ZeroMemory(buffer, gltexture.buffer_size);

  // JVAL: This fixes originx <> 0 or originy <> 0 for sky textures.
  skyhack := false;
  if (gltexture.index = skytexture) and (textures[skytexture].patchcount = 1) then
    if (textures[skytexture].patches[0].originx <> 0) or (textures[skytexture].patches[0].originy <> 0) then
    begin
      patch := W_CacheLumpNum(textures[skytexture].patches[0].patch, PU_STATIC);
      gld_AddPatchToTexture_Untranslated(gltexture, buffer, patch, 0, 0,
                                         not (gltexture.mipmap and use_mipmapping) and gl_paletted_texture);
      Z_ChangeTag(patch, PU_CACHE);
      skyhack := true;
    end;

  if not skyhack then
    for i := 0 to textures[gltexture.index].patchcount - 1 do
    begin
      texpatch := @textures[gltexture.index].patches[i];
      patch := W_CacheLumpNum(texpatch.patch, PU_STATIC);
      gld_AddPatchToTexture_Untranslated(gltexture, buffer, patch, texpatch.originx, texpatch.originy, {13,}
                                         not (gltexture.mipmap and use_mipmapping) and gl_paletted_texture);
      Z_ChangeTag(patch, PU_CACHE);
    end;

  if gltexture.glTexID[Ord(CR_DEFAULT)] = 0 then
    glGenTextures(1, @gltexture.glTexID[Ord(CR_DEFAULT)]);
  glBindTexture(GL_TEXTURE_2D, gltexture.glTexID[Ord(CR_DEFAULT)]);
  if gltexture.mipmap and use_mipmapping then
  begin
    gluBuild2DMipmaps(GL_TEXTURE_2D, gl_tex_format,
                      gltexture.buffer_width, gltexture.buffer_height,
                      GL_RGBA, GL_UNSIGNED_BYTE, buffer);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, gl_tex_filter);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, gl_mipmap_filter);
    if gl_texture_filter_anisotropic then
      glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAX_ANISOTROPY_EXT, 2.0);
  end
  else
  begin
    if gl_paletted_texture then
    begin
      gld_SetTexturePalette(GL_TEXTURE_2D);
      glTexImage2D(GL_TEXTURE_2D, 0, GL_COLOR_INDEX8_EXT,
                   gltexture.buffer_width, gltexture.buffer_height,
                   0, GL_COLOR_INDEX, GL_UNSIGNED_BYTE, buffer);
    end
    else
      glTexImage2D(GL_TEXTURE_2D, 0, gl_tex_format,
                   gltexture.buffer_width, gltexture.buffer_height,
                   0, GL_RGBA, GL_UNSIGNED_BYTE, buffer);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, gl_tex_filter);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, gl_tex_filter);
  end;
  memfree(pointer(buffer), gltexture.buffer_size);
end;

function gld_RegisterPatch(lump: integer; cm: integer; const unload: boolean = true): PGLTexture;
var
  patch: Ppatch_t;
begin
  result := gld_AddNewGLPatchTexture(lump);
  if result = nil then
    exit;
  if result.textype = GLDT_UNREGISTERED then
  begin
    patch := W_CacheSpriteNum(lump, PU_STATIC);
    if patch = nil then
    begin
      result := nil;
      exit;
    end;
    result.textype := GLDT_BROKEN;
    result.index := lump;
    result.mipmap := false;
    result.realtexwidth := patch.width;
    result.realtexheight := patch.height;
    result.leftoffset := patch.leftoffset;
    result.topoffset := patch.topoffset;
    result.tex_width := gld_GetTexDimension(result.realtexwidth);
    result.tex_height := gld_GetTexDimension(result.realtexheight);
    result.width := gl_i_min(result.realtexwidth, result.tex_width);
    result.height := gl_i_min(result.realtexheight, result.tex_height);
    result.buffer_width := result.tex_width;
    result.buffer_height := result.tex_height;
    result.buffer_size := result.buffer_width * result.buffer_height * 4;
    result.heightscale := result.height / result.tex_height;
    if unload then
      Z_ChangeTag(patch, PU_CACHE);
    if result.realtexwidth > result.buffer_width then
      exit;
    if result.realtexheight > result.buffer_height then
      exit;
    result.textype := GLDT_PATCH;
  end;
end;

procedure gld_BindPatch(gltexture: PGLTexture; cm: integer);
var
  patch: Ppatch_t;
  i: integer;
  buffer: PByteArray;
begin
  if (gltexture = last_gltexture) and (cm = last_cm) then
    exit;
  last_gltexture := gltexture;
  last_cm := cm;
  if gltexture = nil then
    exit;
  if gltexture.textype <> GLDT_PATCH then
  begin
    glBindTexture(GL_TEXTURE_2D, 0);
    last_gltexture := nil;
    last_cm := -1;
    exit;
  end;
  if gltexture.glTexID[cm] <> 0 then
  begin
    glBindTexture(GL_TEXTURE_2D, gltexture.glTexID[cm]);
    glGetTexParameteriv(GL_TEXTURE_2D, GL_TEXTURE_RESIDENT, @i);
{    if i = GL_TRUE then   }
      exit;
  end;

  if cm = Ord(CR_DEFAULT) then
    if gld_LoadHiresTexture(gltexture, W_GetNameForNum(gltexture.index)) then
      exit;

  patch := W_CacheSpriteNum(gltexture.index, PU_STATIC);
  buffer := malloc(gltexture.buffer_size);
  if gl_paletted_texture then
    memset(buffer, transparent_pal_index, gltexture.buffer_size)
  else
    MT_ZeroMemory(buffer, gltexture.buffer_size);
  gld_AddPatchToTexture(gltexture, buffer, patch, 0, 0, cm, gl_paletted_texture); // JVAL SOS
  if gltexture.glTexID[cm] = 0 then
    glGenTextures(1, @gltexture.glTexID[cm]);
  glBindTexture(GL_TEXTURE_2D, gltexture.glTexID[cm]);

  if gl_paletted_texture then
  begin
    gld_SetTexturePalette(GL_TEXTURE_2D);
    glTexImage2D(GL_TEXTURE_2D, 0, GL_COLOR_INDEX8_EXT,
                 gltexture.buffer_width, gltexture.buffer_height,
                 0, GL_COLOR_INDEX, GL_UNSIGNED_BYTE, buffer);
  end
  else
    glTexImage2D(GL_TEXTURE_2D, 0, gl_tex_format,
                 gltexture.buffer_width, gltexture.buffer_height,
                 0, GL_RGBA, GL_UNSIGNED_BYTE, buffer);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, gl_tex_filter);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, gl_tex_filter);
  memfree(pointer(buffer), gltexture.buffer_size);
  Z_ChangeTag(patch, PU_CACHE);
end;

function gld_RegisterFlat(lump: integer; mipmap: boolean; flat: integer): PGLTexture;
var
  lumplen: integer;
begin
  result := gld_AddNewGLPatchTexture(lump);
  if result = nil then
    exit;
  if result.textype = GLDT_UNREGISTERED then
  begin
    result.textype := GLDT_BROKEN;
    result.index := lump;
    result.mipmap := mipmap;
    lumplen := W_LumpLength(lump);
    // JVAL
    // Use 8192 to preserve Heretic and Hexen scrolling flats :)
    if lumplen <= 8192 then
    begin
      result.realtexwidth := 64;
      result.realtexheight := 64;
    end
    else
    begin
      result.realtexwidth := round(sqrt(lumplen));
      result.realtexheight := result.realtexwidth;
    end;
    result.leftoffset := 0;
    result.topoffset := 0;
    result.tex_width := gld_GetTexDimension(result.realtexwidth);
    result.tex_height := gld_GetTexDimension(result.realtexheight);
    if flat < 0 then
      result.texturescale := 1.0
    else
    begin
      if flats[flats[flat].translation].size = 0 then
        result.texturescale := 1.0
      else
        result.texturescale := 64 / dsscalesize[flats[flats[flat].translation].size].flatsize;
    end;
    if result.mipmap and use_mipmapping then
    begin
      result.width := result.tex_width;
      result.height := result.tex_height;
      result.buffer_width := result.realtexwidth;
      result.buffer_height := result.realtexheight;
    end
    else
    begin
      result.width := gl_i_min(result.realtexwidth, result.tex_width);
      result.height := gl_i_min(result.realtexheight, result.tex_height);
      result.buffer_width := result.tex_width;
      result.buffer_height := result.tex_height;
    end;
    result.buffer_size := result.buffer_width * result.buffer_height * 4;
    result.heightscale := result.height / result.tex_height;
    if result.realtexwidth > result.buffer_width then
      exit;
    if result.realtexheight > result.buffer_height then
      exit;
    result.textype := GLDT_FLAT;
  end;
end;

procedure gld_BindFlat(gltexture: PGLTexture);
var
  flat: PByteArray;
  i: integer;
  buffer: PByteArray;
begin
  if gltexture = last_gltexture then
    exit;
  last_gltexture := gltexture;
  if gltexture = nil then
    exit;
  if gltexture.textype <> GLDT_FLAT then
  begin
    glBindTexture(GL_TEXTURE_2D, 0);
    last_gltexture := nil;
    last_cm := -1;
    exit;
  end;
  if gltexture.glTexID[Ord(CR_DEFAULT)] <> 0 then
  begin
    glBindTexture(GL_TEXTURE_2D, gltexture.glTexID[Ord(CR_DEFAULT)]);
    glGetTexParameteriv(GL_TEXTURE_2D, GL_TEXTURE_RESIDENT, @i);
{    if i = GL_TRUE then  }
      exit;
  end;

  if gld_LoadHiresTexture(gltexture, W_GetNameForNum(gltexture.index)) then
    exit;

  flat := W_CacheLumpNum(gltexture.index, PU_STATIC);
  buffer := malloc(gltexture.buffer_size);
  if (not (gltexture.mipmap and use_mipmapping)) and gl_paletted_texture then
    memset(buffer, transparent_pal_index, gltexture.buffer_size)
  else
    MT_ZeroMemory(buffer, gltexture.buffer_size);
  gld_AddFlatToTexture(gltexture, buffer, flat, not (gltexture.mipmap and use_mipmapping) and gl_paletted_texture);
  if gltexture.glTexID[Ord(CR_DEFAULT)] = 0 then
    glGenTextures(1, @gltexture.glTexID[Ord(CR_DEFAULT)]);
  glBindTexture(GL_TEXTURE_2D, gltexture.glTexID[Ord(CR_DEFAULT)]);
  if gltexture.mipmap and use_mipmapping then
  begin
    gluBuild2DMipmaps(GL_TEXTURE_2D, gl_tex_format,
                      gltexture.buffer_width, gltexture.buffer_height,
                      GL_RGBA, GL_UNSIGNED_BYTE, buffer);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, gl_tex_filter);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, gl_mipmap_filter);
    if gl_texture_filter_anisotropic then
      glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAX_ANISOTROPY_EXT, 2.0);
  end
  else
  begin
    if gl_paletted_texture then
    begin
      gld_SetTexturePalette(GL_TEXTURE_2D);
      glTexImage2D(GL_TEXTURE_2D, 0, GL_COLOR_INDEX8_EXT,
                   gltexture.buffer_width, gltexture.buffer_height,
                   0, GL_COLOR_INDEX, GL_UNSIGNED_BYTE, buffer);
    end
    else
      glTexImage2D(GL_TEXTURE_2D, 0, gl_tex_format,
                   gltexture.buffer_width, gltexture.buffer_height,
                   0, GL_RGBA, GL_UNSIGNED_BYTE, buffer);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, gl_tex_filter);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, gl_tex_filter);
  end;
  memfree(pointer(buffer), gltexture.buffer_size);
  Z_ChangeTag(flat, PU_CACHE);
end;

procedure gld_CleanTextures;
var
  i, j: integer;
begin
  if gld_GLTextures = nil then
    exit;
  for i := 0 to numtextures - 1 do
  begin
    if gld_GLTextures[i] <> nil then
    begin
      for j := 0 to Ord(CR_LIMIT) + MAXPLAYERS - 1 do
        glDeleteTextures(1, @gld_GLTextures[i].glTexID[j]);
      memfree(pointer(gld_GLTextures[i]), SizeOf(GLTexture));
    end;
  end;
  ZeroMemory(gld_GLTextures, numtextures * SizeOf(PGLTexture));
end;

procedure gld_CleanPatchTextures;
var
  i, j: integer;
  numlumps: integer;
begin
  if gld_GLPatchTextures = nil then
    exit;
  numlumps := W_NumLumps;
  for i := 0 to numlumps - 1 do
  begin
    if gld_GLPatchTextures[i] <> nil then
    begin
      for j := 0 to Ord(CR_LIMIT) + MAXPLAYERS - 1 do
        glDeleteTextures(1, @gld_GLPatchTextures[i].glTexID[j]);
      memfree(pointer(gld_GLPatchTextures[i]), SizeOf(GLTexture));
    end;
  end;
  ZeroMemory(gld_GLPatchTextures, numlumps * SizeOf(PGLTexture));
end;

procedure gld_ShutDownTextures;
var
  i, j: integer;
  numlumps: integer;
begin

  if gld_GLTextures <> nil then
  begin
    for i := 0 to numtextures - 1 do
    begin
      if gld_GLTextures[i] <> nil then
      begin
        for j := 0 to Ord(CR_LIMIT) + MAXPLAYERS - 1 do
          glDeleteTextures(1, @gld_GLTextures[i].glTexID[j]);
        memfree(pointer(gld_GLTextures[i]), SizeOf(GLTexture));
      end;
    end;
    memfree(pointer(gld_GLTextures), numtextures * SizeOf(PGLTexture));
  end;

  if gld_GLPatchTextures <> nil then
  begin
    numlumps := W_NumLumps;
    for i := 0 to numlumps - 1 do
    begin
      if gld_GLPatchTextures[i] <> nil then
      begin
        for j := 0 to Ord(CR_LIMIT) + MAXPLAYERS - 1 do
          glDeleteTextures(1, @gld_GLPatchTextures[i].glTexID[j]);
        memfree(pointer(gld_GLPatchTextures[i]), SizeOf(GLTexture));
      end;
    end;
    memfree(pointer(gld_GLPatchTextures), numlumps * SizeOf(PGLTexture));
  end;
end;

procedure gld_Precache;
var
  i, j, k: integer;
  hitlist: PBooleanArray;
  size: integer;
  th: Pthinker_t;
  sflump: PIntegerArray;
  psd: Pside_t;
  psec: Psector_t;
begin
  if demoplayback then
    exit;

  if numflats > numsprites then
    size := numflats
  else
    size := numsprites;
  if numtextures > size then
    size := numtextures;

  hitlist := malloc(size);

  // Precache flats.

  ZeroMemory(hitlist, numflats);

  psec := @sectors[numsectors];
  while integer(psec) <> integer(sectors) do
  begin
    dec(psec);
    hitlist[psec.floorpic] := true;
    hitlist[psec.ceilingpic] := true;
  end;

  for i := numflats - 1 downto 0 do
    if hitlist[i] then
      gld_BindFlat(gld_RegisterFlat(flats[i].lump, true, i));

  // Precache textures.

  ZeroMemory(hitlist, numtextures);

  psd := @sides[numsides];
  while integer(psd) <> integer(sides) do
  begin
    dec(psd);
    hitlist[psd.bottomtexture] := true;
    hitlist[psd.toptexture] := true;
    hitlist[psd.midtexture] := true;
  end;

  // Sky texture is always present.
  // Note that F_SKY1 is the name used to
  //  indicate a sky floor/ceiling as a flat,
  //  while the sky texture is stored like
  //  a wall texture, with an episode dependend
  //  name.

  hitlist[skytexture] := true;

  for i := numtextures - 1 downto 0 do
    if hitlist[i] then
      gld_BindTexture(gld_RegisterTexture(i, true));

  // Precache sprites.
  ZeroMemory(hitlist, numsprites);

  th := thinkercap.next;
  while th <> @thinkercap do
  begin
    if @th._function.acp1 = @P_MobjThinker then
      hitlist[Ord(Pmobj_t(th).sprite)] := true;
    th := th.next;
  end;

  for i := numsprites - 1 downto 0 do
    if hitlist[i] then
    begin
      for j := sprites[i].numframes - 1 downto 0 do
      begin
        if sprites[i].spriteframes[j].rotate >= 0 then
        begin
          sflump := @sprites[i].spriteframes[j].lump;
          for k := 7 downto 0 do
            gld_BindPatch(gld_RegisterPatch(firstspritelump + sflump[k], Ord(CR_DEFAULT)), Ord(CR_DEFAULT));
        end;
      end;
    end;

  memfree(pointer(hitlist), size);
end;

procedure gld_ResetLastTexture;
begin
  last_gltexture := nil;
end;

procedure gld_ClearTextureMemory;
begin
  gld_CleanTextures;
  gld_CleanPatchTextures;
end;

function gld_GetCurrTexFiltering: gl_filter_t;
begin
  gl_tex_filter_string := strupper(gl_tex_filter_string);
  if gl_tex_filter_string = gl_tex_filters[Ord(FLT_NEAREST_MIPMAP_NEAREST)] then
    result := FLT_NEAREST_MIPMAP_NEAREST
  else if gl_tex_filter_string = gl_tex_filters[Ord(FLT_LINEAR_MIPMAP_NEAREST)] then
    result := FLT_LINEAR_MIPMAP_NEAREST
  else if gl_tex_filter_string = gl_tex_filters[Ord(FLT_NEAREST_MIPMAP_LINEAR)] then
    result := FLT_NEAREST_MIPMAP_LINEAR
  else if gl_tex_filter_string = gl_tex_filters[Ord(FLT_LINEAR_MIPMAP_LINEAR)] then
    result := FLT_LINEAR_MIPMAP_LINEAR
  else if gl_tex_filter_string = gl_tex_filters[Ord(FLT_NEAREST)] then
    result := FLT_NEAREST
  else
    result := FLT_LINEAR;
end;

procedure gld_SetCurrTexFiltering(const flt: gl_filter_t);
begin
  if flt = FLT_NEAREST_MIPMAP_NEAREST then
  begin
    use_mipmapping := true;
    gl_shared_texture_palette := false;
    printf('Using GL_NEAREST for normal textures.'#13#10);
    printf('Using GL_NEAREST_MIPMAP_NEAREST for mipmap textures.'#13#10);
    gl_tex_filter := GL_NEAREST;
    gl_mipmap_filter := GL_NEAREST_MIPMAP_NEAREST;
    gl_tex_filter_string := gl_tex_filters[Ord(FLT_NEAREST_MIPMAP_NEAREST)];
  end
  else if flt = FLT_LINEAR_MIPMAP_NEAREST then
  begin
    use_mipmapping := true;
    gl_shared_texture_palette := false;
    printf('Using GL_LINEAR for normal textures.'#13#10);
    printf('Using GL_LINEAR_MIPMAP_NEAREST for mipmap textures.'#13#10);
    gl_tex_filter := GL_LINEAR;
    gl_mipmap_filter := GL_LINEAR_MIPMAP_NEAREST;
    gl_tex_filter_string := gl_tex_filters[Ord(FLT_LINEAR_MIPMAP_NEAREST)];
  end
  else if flt = FLT_NEAREST_MIPMAP_LINEAR then
  begin
    use_mipmapping := true;
    gl_shared_texture_palette := false;
    printf('Using GL_NEAREST for normal textures.'#13#10);
    printf('Using GL_NEAREST_MIPMAP_LINEAR for mipmap textures.'#13#10);
    gl_tex_filter := GL_NEAREST;
    gl_mipmap_filter := GL_NEAREST_MIPMAP_LINEAR;
    gl_tex_filter_string := gl_tex_filters[Ord(FLT_NEAREST_MIPMAP_LINEAR)];
  end
  else if flt = FLT_LINEAR_MIPMAP_LINEAR then
  begin
    use_mipmapping := true;
    gl_shared_texture_palette := false;
    printf('Using GL_LINEAR for normal textures.'#13#10);
    printf('Using GL_LINEAR_MIPMAP_LINEAR for mipmap textures.'#13#10);
    gl_tex_filter := GL_LINEAR;
    gl_mipmap_filter := GL_LINEAR_MIPMAP_LINEAR;
    gl_tex_filter_string := gl_tex_filters[Ord(FLT_LINEAR_MIPMAP_LINEAR)];
  end
  else if flt = FLT_NEAREST then
  begin
    use_mipmapping := false;
    printf('Using GL_NEAREST for textures.'#13#10);
    gl_tex_filter := GL_NEAREST;
    gl_mipmap_filter := GL_NEAREST;
    gl_tex_filter_string := gl_tex_filters[Ord(FLT_NEAREST)];
  end
  else // Default
  begin
    use_mipmapping := false;
    printf('Using GL_LINEAR for textures.'#13#10);
    gl_tex_filter := GL_LINEAR;
    gl_mipmap_filter := GL_LINEAR;
    gl_tex_filter_string := gl_tex_filters[Ord(FLT_LINEAR)];
  end;
end;

end.

