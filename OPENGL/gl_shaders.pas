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

//
// JVAL
//
// GLSL Shaders
//

unit gl_shaders;

interface

uses
  dglOpenGL;

//==============================================================================
//
// gld_InitShaders
//
//==============================================================================
procedure gld_InitShaders;

//==============================================================================
//
// gld_ShadersDone
//
//==============================================================================
procedure gld_ShadersDone;

implementation

uses
  d_delphi,
  w_pak;

const
  MAXSHADERFILENAMESIZE = 128;

type
  shader_t = record
    fs, vs: string[MAXSHADERFILENAMESIZE];  // Fragment and vertex shader filenames
    handle: GLHandleARB;
  end;
  Pshader_t = ^shader_t;
  shader_tArray = array[0..$FFF] of shader_t;
  Pshader_tArray = ^shader_tArray;

var
  shaders: Pshader_tArray;
  numshaders: integer;

//==============================================================================
//
// gld_LoadShader
//
//==============================================================================
function gld_LoadShader(const fs, vs: string): GLHandleARB;
var
  fsObj, vsObj: GLHandleARB;
  l: TDStringList;
  strm: TPakStream;
  fText: String;
  fLen: integer;
  vText: String;
  vLen: integer;
begin
  result := 0;

  l := TDStringList.Create;

  strm := TPakStream.Create(fs, pm_short);
  if strm.IOResult <> 0 then
  begin
    l.Free;
    strm.Free;
    exit;
  end;

  l.LoadFromStream(strm);
  strm.Free;

  fText := l.Text;
  fLen := Length(fText);

  strm := TPakStream.Create(vs, pm_short);
  if strm.IOResult <> 0 then
  begin
    l.Free;
    strm.Free;
    exit;
  end;

  l.LoadFromStream(strm);
  strm.Free;

  vText := l.Text;
  vLen := Length(fText);

  l.Free;

  result:= glCreateProgramObjectARB;

  fsObj:= glCreateShaderObjectARB(GL_FRAGMENT_SHADER_ARB);
  vsObj:= glCreateShaderObjectARB(GL_VERTEX_SHADER_ARB);

  glShaderSourceARB(vsObj, 1, @vText, @vLen);
  glShaderSourceARB(fsObj, 1, @fText, @fLen);

  glCompileShaderARB(fsObj);
  glCompileShaderARB(vsObj);

  glAttachObjectARB(result, fsObj);
  glAttachObjectARB(result, vsObj);

  glDeleteObjectARB(fsObj);
  glDeleteObjectARB(vsObj);

  glLinkProgramARB(result);

end;

//==============================================================================
//
// gld_AddShader
//
//==============================================================================
function gld_AddShader(const fs, vs: string): integer;
var
  i: integer;
  fs1, vs1: string[MAXSHADERFILENAMESIZE];
  ps: Pshader_t;
begin
  fs1 := strupper(fs);
  vs1 := strupper(vs);
  ps := @shaders[0];
  for i := 0 to numshaders - 1 do
  begin
    if (ps.fs = fs1) and (ps.vs = vs1) then
    begin
      result := i;
      exit;
    end;
    Inc(ps);
  end;

  result := numshaders;
  realloc(Pointer(shaders), numshaders * SizeOf(shader_t), (1 + numshaders) * SizeOf(shader_t));
  ps.fs := fs1;
  ps.vs := vs1;
  ps.handle := gld_LoadShader(fs1, vs1);
end;

//==============================================================================
//
// gld_InitShaders
//
//==============================================================================
procedure gld_InitShaders;
begin
  Read_GL_ARB_Shader_Objects;
  shaders := nil;
  numshaders := 0;
end;

//==============================================================================
//
// gld_ShadersDone
//
//==============================================================================
procedure gld_ShadersDone;
var
  i: integer;
begin
  if numshaders = 0 then
    exit;

  for i := 0 to numshaders - 1 do
    glDeleteProgramsARB(1, @shaders[i].handle);
  memfree(Pointer(shaders), numshaders * SizeOf(shader_t));
end;

end.
