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
//  DESCRIPTION:
//    Dynamic lights for OpenGL rendering (why not in software mode??)
//    LIGHTDEF lump parsing, light animation
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit gl_types;

interface

uses
  dglOpenGL;

type
  GLVertex = record
    x: TGLfloat;
    y: TGLfloat;
    z: TGLfloat;
  end;
  PGLVertex = ^GLVertex;
  GLVertexArray = array[0..$FFFF] of GLVertex;
  PGLVertexArray = ^GLVertexArray;
  GLVertexArraysP = array[0..$FFFF] of PGLVertexArray;
  PGLVertexArraysP = ^GLVertexArraysP;

  GLTexcoord = record
    u: TGLfloat;
    v: TGLfloat;
  end;
  PGLTexcoord = ^GLTexcoord;
  GLTexcoordArray = array[0..$FFFF] of GLTexcoord;
  PGLTexcoordArray = ^GLTexcoordArray;

  GLuintArray = array[0..$FFFF] of GLuint;
  PGLuintArray = ^GLuintArray;

  GLVertexUV = record
    x: TGLfloat;
    y: TGLfloat;
    z: TGLfloat;
    u: TGLfloat;
    v: TGLfloat;
  end;

function MakeGLVertex(const x, y, z: single): GLVertex;

implementation

function MakeGLVertex(const x, y, z: single): GLVertex;
begin
  result.x := x;
  result.y := y;
  result.z := z;
end;

end.

