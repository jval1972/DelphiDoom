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

unit gl_defs;

interface

uses
  doomdef,
  dglOpenGL,
  m_fixed,
  r_draw;

type
  GLTexType = (
    GLDT_UNREGISTERED,
    GLDT_BROKEN,
    GLDT_PATCH,
    GLDT_TEXTURE,
    GLDT_FLAT,
    GLDT_SKY,
    GLDT_LIGHT
  );

const
  CR_INVUL = Ord(CR_LIMIT) + MAXPLAYERS;

type
  GLTexture = record
    index: integer;
    width, height: integer;
    leftoffset, topoffset: integer;
    tex_width, tex_height: integer;
    realtexwidth, realtexheight: integer;
    texturescale: single;
    buffer_width, buffer_height: integer;
    buffer_size: integer;
    heightscale: single;
    glTexID: array[0..CR_INVUL] of integer;
    textype: GLTexType;
    mipmap: boolean;
  end;
  PGLTexture = ^GLTexture;
  GLTexturePArray = array[0..$FFFF] of PGLTexture;
  PGLTexturePArray = ^GLTexturePArray;

const
  CR_DEFAULT = CR_LIMIT;

var
  tran_filter_pct: integer = 66;
  use_fog: boolean = false;
  fog_density: integer = 100; // 200
  use_white_fog: boolean = true;
  white_fog_density: integer = 200;
  gl_nearclip: integer = 5;
  gl_tex_filter_string: string;
  gl_tex_filter: integer;
  gl_mipmap_filter: integer;
  gl_drawsky: boolean = true;
  gl_stencilsky: boolean = true;
  gl_screensync: boolean = true;
  gl_texture_filter_anisotropic: boolean = false;
  gl_use_paletted_texture: integer = 0;
  gl_use_shared_texture_palette: integer = 0;
  gl_paletted_texture: boolean = false;
  gl_shared_texture_palette: boolean = false;
  gl_linear_hud: boolean = true;
  gl_add_all_lines: boolean = false;
  gl_renderwireframe: Boolean = false;
  // Hack to avoid Intel HD4000 problem with Win10
  // https://communities.intel.com/thread/117626
  gl_no_glfinish_hack: Boolean = true;

type
  lp3DFXFUNC = procedure(i1, i2, i3, i4, i5: integer; const p: pointer);

var
  gld_ColorTableEXT: lp3DFXFUNC = nil;

const
  NO_TEXTURE = 0;
  GL_BAD_LIST = $FFFFFFFF;

type
  gl_filter_t = (
    FLT_NEAREST,
    FLT_LINEAR,
    FLT_LINEAR_MIPMAP_NEAREST,
    FLT_NEAREST_MIPMAP_NEAREST,
    FLT_NEAREST_MIPMAP_LINEAR,
    FLT_LINEAR_MIPMAP_LINEAR,
    NUM_GL_FILTERS
  );

const
  gl_tex_filters: array[0..Ord(NUM_GL_FILTERS) - 1] of string = (
    'GL_NEAREST',
    'GL_LINEAR',
    'GL_LINEAR_MIPMAP_NEAREST',
    'GL_NEAREST_MIPMAP_NEAREST',
    'GL_NEAREST_MIPMAP_LINEAR',
    'GL_LINEAR_MIPMAP_LINEAR'
  );

var
  gl_tex_format_string: string;
  gl_tex_format: integer = GL_RGBA8;
  gl_fakecontrast: boolean;

type
  tex_format_lookup_t = record
    tex_format: integer;
    desc: string
  end;

const
  DEF_TEX_FORMAT = GL_RGBA;
  NUM_GL_TEX_FORMATS = 5;
  gl_tex_formats: array[0..NUM_GL_TEX_FORMATS - 1] of tex_format_lookup_t = (
    (tex_format: GL_RGBA8; desc: 'GL_RGBA8'),
    (tex_format: GL_RGB5_A1; desc: 'GL_RGB5_A1'),
    (tex_format: GL_RGBA4; desc: 'GL_RGBA4'),
    (tex_format: GL_RGBA2; desc: 'GL_RGBA2'),
    (tex_format: GL_RGBA; desc: 'GL_RGBA')
  );

const
  VPT_STRETCH = 1;
  VPT_FLIP = 2;
  VPT_TRANS = 4;
  VPT_NOUNLOAD = 8;

const
  __glPi = 3.14159265358979323846;
  gl_whitecolor: array[0..3] of TGLfloat = (1.0, 1.0, 1.0, 1.0);

type
  gld_camera_t = record
    position: TGLVectorf3;
    rotation: TGLVectorf3;
  end;
  Pgld_camera_t = ^gld_camera_t;

var
  camera: gld_camera_t;

const
  MAP_COEFF = 128.0; // If this is changed, must be changed and to r_softgl
  MAP_SCALE = MAP_COEFF * FRACUNIT;
  FLATUVSCALE = FRACUNIT * 64.0;

const
  COORDMIN = -1.0E38;
  COORDMAX =  1.0E38;

const
  GLEPSILON = 0.000001;

implementation

end.

