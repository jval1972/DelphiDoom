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

unit gl_ambient;

interface

//==============================================================================
//
// gld_InitAmbient
//
//==============================================================================
procedure gld_InitAmbient;

//==============================================================================
//
// gld_AmbientDone
//
//==============================================================================
procedure gld_AmbientDone;

//==============================================================================
//
// gld_AmbientExecute
//
//==============================================================================
procedure gld_AmbientExecute;

implementation

uses
  d_delphi,
  dglOpenGL,
  doomdef,
  gl_tex,
  t_main;

const
  AMBIENTPRECISION = 8;

var
  xstep, ystep: float;
  ambient_tex: PTexture;
  tex: GLUint;

//==============================================================================
//
// gld_AmbientExecute
//
//==============================================================================
procedure gld_AmbientExecute;
var
  x, y: integer;
  i, j: integer;
  f: float;
  l: LongWord;
  b, b2: byte;
  pw: PLongWord;
begin
  pw := ambient_tex.GetImage;
  for i := 0 to AMBIENTPRECISION - 1 do
  begin
    x := round(i * xstep);
    if x < 0 then
      x := 0
    else if x >= SCREENWIDTH then
      x := SCREENWIDTH - 1;
    for j := 0 to AMBIENTPRECISION - 1 do
    begin
      y := Round (j * ystep);
      if y < 0 then
        y := 0
      else if y >= SCREENHEIGHT then
        x := SCREENHEIGHT - 1;
      glReadPixels(x, y, 1, 1, GL_DEPTH_COMPONENT, GL_FLOAT, @f);
      l := Round((f * f * f) * 255);
      if l > 255 then
        l := 255;
      b := 192;
      b2 := l div 2;
      pw^ := b2 + b2 shl 8 + b2 shl 16 + b shl 24;
      Inc(pw);
    end;
  end;

  glAlphaFunc(GL_LESS, 0.99);

  glBindTexture(GL_TEXTURE_2D, tex);
  glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, AMBIENTPRECISION, AMBIENTPRECISION, GL_RGBA, GL_UNSIGNED_BYTE, ambient_tex.GetImage);

  glDisable(GL_ALPHA_TEST);
  glBegin(GL_TRIANGLE_STRIP);
    glTexCoord2f(0.0, 0.0); glVertex2f(0.0, SCREENHEIGHT);
    glTexCoord2f(0.0, 1.0); glVertex2f(SCREENWIDTH, SCREENHEIGHT);
    glTexCoord2f(1.0, 0.0); glVertex2f(0.0, 0.0);
    glTexCoord2f(1.0, 1.0); glVertex2f(SCREENWIDTH, 0);
  glEnd;
  glEnable(GL_ALPHA_TEST);

  glAlphaFunc(GL_GEQUAL, 0.5);

end;

//==============================================================================
//
// gld_InitAmbient
//
//==============================================================================
procedure gld_InitAmbient;
begin
  xstep := SCREENWIDTH / (AMBIENTPRECISION - 1);
  ystep := SCREENHEIGHT / (AMBIENTPRECISION - 1);
  ambient_tex := new(PTexture, Create);
  ambient_tex.ScaleTo(AMBIENTPRECISION, AMBIENTPRECISION);
  ambient_tex.ConvertTo32bit;
  tex := gld_LoadExternalTexture(ambient_tex, True, GL_CLAMP);
end;

//==============================================================================
//
// gld_AmbientDone
//
//==============================================================================
procedure gld_AmbientDone;
begin
  glDeleteTextures(1, @tex);
  if ambient_tex <> nil then
    Dispose(ambient_tex, destroy);
end;

end.
