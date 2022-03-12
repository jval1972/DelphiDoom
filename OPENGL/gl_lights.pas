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
//  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
//  02111-1307, USA.
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit gl_lights;

interface

uses
  d_delphi;

//==============================================================================
//
// gld_SetUplight
//
//==============================================================================
procedure gld_SetUplight(const r, g, b: float);

const
  GL_LIGHTBOOSTFACTOR = 192;

implementation

uses
  m_fixed,
  dglOpenGl,
  gl_defs,
  gl_tex,
  r_lights,
  z_zone;

var
  lighttexture: PGLTexture;

//==============================================================================
//
// gld_BindLightTexture
//
//==============================================================================
procedure gld_BindLightTexture;
var
  buffer: PLongWordArray;
  i: integer;
  c: byte;
begin
  last_gltexture := nil;
  if lighttexture = nil then
  begin
    lighttexture := Z_Malloc(SizeOf(GLTexture), PU_STATIC, nil);
    ZeroMemory(lighttexture, SizeOf(GLTexture));
    lighttexture.mipmap := use_mipmapping;
    lighttexture.realtexwidth := LIGHTBOOSTSIZE;
    lighttexture.realtexheight := LIGHTBOOSTSIZE;
    lighttexture.leftoffset := 0;
    lighttexture.topoffset := 0;
    lighttexture.tex_width := LIGHTBOOSTSIZE;
    lighttexture.tex_height := LIGHTBOOSTSIZE;
    lighttexture.width := LIGHTBOOSTSIZE;
    lighttexture.height := LIGHTBOOSTSIZE;
    lighttexture.buffer_width := LIGHTBOOSTSIZE;
    lighttexture.buffer_height := LIGHTBOOSTSIZE;
    if lighttexture.buffer_width <> 0 then
      lighttexture.inv_buffer_width := 1.0 / lighttexture.buffer_width
    else
      lighttexture.inv_buffer_width := 0.0;
    if lighttexture.buffer_height <> 0 then
      lighttexture.inv_buffer_height := 1.0 / lighttexture.buffer_height
    else
      lighttexture.inv_buffer_height := 0.0;
    lighttexture.buffer_size := LIGHTBOOSTSIZE * LIGHTBOOSTSIZE * SizeOf(LongWord);
    lighttexture.textype := GLDT_LIGHT;
    glGenTextures(1, @lighttexture.glTexID[Ord(CR_DEFAULT)]);
    glBindTexture(GL_TEXTURE_2D, lighttexture.glTexID[Ord(CR_DEFAULT)]);
    if lightboost = nil then
      R_InitLightBoost;
    buffer := malloc(lighttexture.buffer_size);
    // JVAL: Translate lightboost to texture
    for i := 0 to LIGHTBOOSTSIZE * LIGHTBOOSTSIZE - 1 do
    begin
      c := (lightboost[i] - FRACUNIT) shr 8;
      buffer[i] := c + c shl 8 + c shl 16 + c shl 24;
    end;
    if use_mipmapping then
    begin
      gluBuild2DMipmaps(GL_TEXTURE_2D, gl_tex_format,
                        lighttexture.buffer_width, lighttexture.buffer_height,
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
                   lighttexture.buffer_width, lighttexture.buffer_height,
                   0, GL_RGBA, GL_UNSIGNED_BYTE, buffer);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, gl_tex_filter);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, gl_tex_filter);
    end;
    memfree(pointer(buffer), lighttexture.buffer_size);
  end;
  glBindTexture(GL_TEXTURE_2D, lighttexture.glTexID[Ord(CR_DEFAULT)]);
end;

//==============================================================================
//
// gld_SetUplight
//
//==============================================================================
procedure gld_SetUplight(const r, g, b: float);
begin
  gld_BindLightTexture;
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glAlphaFunc(GL_GEQUAL, 0.01);
  glColor4f(r, g, b, 0.5);
end;

end.
