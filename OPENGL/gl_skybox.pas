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
//  Site  : http://delphidoom.sitesled.com/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit gl_sky;

interface

uses
  r_sky;

//==============================================================================
//
// gld_InitSky
//
//==============================================================================
procedure gld_InitSky;

//==============================================================================
//
// gld_DrawSky
//
//==============================================================================
procedure gld_DrawSky;

//==============================================================================
//
// gld_SkyDone
//
//==============================================================================
procedure gld_SkyDone;

implementation

uses
  d_delphi,
  dglOpenGL,
  gl_tex,
  gl_defs;

var
  skytexture1: GLuint;
  skytexture2: GLuint;
  skytexture3: GLuint;
  skytexture4: GLuint;
  skytexture5: GLuint;
  skytexture6: GLuint;

//==============================================================================
//
// gld_InitSky
//
//==============================================================================
procedure gld_InitSky;
begin
  skytexture1 := gld_LoadExternalTexture('sky1side1.png', False, GL_REPEAT);
  skytexture2 := gld_LoadExternalTexture('sky1side2.png', False, GL_REPEAT);
  skytexture3 := gld_LoadExternalTexture('sky1side3.png', False, GL_REPEAT);
  skytexture4 := gld_LoadExternalTexture('sky1side4.png', False, GL_REPEAT);
  skytexture5 := gld_LoadExternalTexture('sky1side5.png', False, GL_REPEAT);
  skytexture6 := gld_LoadExternalTexture('sky1side6.png', False, GL_REPEAT);
end;

//==============================================================================
//
// gld_DrawSky
//
//==============================================================================
procedure gld_DrawSky;
begin
  glDisable(GL_DEPTH_TEST);
  glDepthMask(FALSE);

  glBindTexture(GL_TEXTURE_2D, skytexture1);
  glBegin(GL_QUADS);
    glTexCoord2f(1, 1);
    glVertex3f(50000.00, -49500.00, -50000.00);
    glTexCoord2f(0, 1);
    glVertex3f(-50000.00, -49500.00, -50000.00);
    glTexCoord2f(0, 0);
    glVertex3f(-50000.00, -49500.00, 50000.00);
    glTexCoord2f(1, 0);
    glVertex3f(50000.00, -49500.00, 50000.00);
  glEnd;

    // Render the front quad
    glBindTexture(GL_TEXTURE_2D, skytexture2);
    glBegin(GL_QUADS);
      glTexCoord2f(1, 1); glVertex3f(  50000.00, -50000.00, -49500.00 );
      glTexCoord2f(0, 1); glVertex3f( -50000.00, -50000.00, -49500.00 );
      glTexCoord2f(0, 0); glVertex3f( -50000.00,  50000.00, -49500.00 );
      glTexCoord2f(1, 0); glVertex3f(  50000.00,  50000.00, -49500.00 );
    glEnd();

    // Render the left quad
    glBindTexture(GL_TEXTURE_2D, skytexture3);
    glBegin(GL_QUADS);
      glTexCoord2f(1, 1); glVertex3f(  49500.00, -50000.00,  50000.00 );
      glTexCoord2f(0, 1); glVertex3f(  49500.00, -50000.00, -50000.00 );
      glTexCoord2f(0, 0); glVertex3f(  49500.00,  50000.00, -50000.00 );
      glTexCoord2f(1, 0); glVertex3f(  49500.00,  50000.00,  50000.00 );
    glEnd();

    // Render the back quad
    glBindTexture(GL_TEXTURE_2D, skytexture4);
    glBegin(GL_QUADS);
      glTexCoord2f(1, 1); glVertex3f( -50000.00, -50000.00,  49500.00 );
      glTexCoord2f(0, 1); glVertex3f(  50000.00, -50000.00,  49500.00 );
      glTexCoord2f(0, 0); glVertex3f(  50000.00,  50000.00,  49500.00 );
      glTexCoord2f(1, 0); glVertex3f( -50000.00,  50000.00,  49500.00 );
    glEnd();

    // Render the right quad
    glBindTexture(GL_TEXTURE_2D, skytexture5);
    glBegin(GL_QUADS);
      glTexCoord2f(1, 1); glVertex3f( -49500.00, -50000.00, -50000.00 );
      glTexCoord2f(0, 1); glVertex3f( -49500.00, -50000.00,  50000.00 );
      glTexCoord2f(0, 0); glVertex3f( -49500.00,  50000.00,  50000.00 );
      glTexCoord2f(1, 0); glVertex3f( -49500.00,  50000.00, -50000.00 );
    glEnd();

  glBindTexture(GL_TEXTURE_2D, skytexture6);
  glBegin(GL_QUADS);
    glTexCoord2f(1, 1);
    glVertex3f(50000.00, 49500.00, -50000.00);
    glTexCoord2f(0, 1);
    glVertex3f(-50000.00, 49500.00, -50000.00);
    glTexCoord2f(0, 0);
    glVertex3f(-50000.00, 49500.00, 50000.00);
    glTexCoord2f(1, 0);
    glVertex3f(50000.00, 49500.00, 50000.00);
  glEnd;

  glDepthMask(TRUE);
  glEnable(GL_DEPTH_TEST);
end;

//==============================================================================
//
// gld_SkyDone
//
//==============================================================================
procedure gld_SkyDone;
begin
  glDeleteTextures(1, @skytexture1);
  glDeleteTextures(1, @skytexture2);
  glDeleteTextures(1, @skytexture3);
  glDeleteTextures(1, @skytexture4);
  glDeleteTextures(1, @skytexture5);
  glDeleteTextures(1, @skytexture6);
end;

end.
