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
//  DESCRIPTION:
//    External DMX & DDModel model support
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit mdl_ddmodel;

interface

uses
  d_delphi,
  gl_types,
  mdl_base,
  mdl_script_model;

type
  TDDModel = class(TBaseModel)
  protected
    fmdl: TDDModelLoader;
    fNumFrames: integer;
    precalc: PGLuintArray;
    fname: string;
    fxoffset, fyoffset, fzoffset: float;
    fxscale, fyscale, fzscale: float;
    procedure LoadFrom(const fn: string); virtual;
    procedure DrawSimpleIdx(const idx: integer); virtual;
  public
    constructor Create(const name: string;
      const xoffset, yoffset, zoffset: float;
      const xscale, yscale, zscale: float;
      const additionalframes: TDStringList); override;
    destructor Destroy; override;
    procedure Draw(const frm1, frm2: integer; const offset: float); override;
    procedure DrawSimple(const frm: integer); override;
  end;

implementation

uses
  dglOpenGL,
  doomdef,
  gl_defs,
  i_system,
  w_folders,
  w_pak;

//==============================================================================
//
// TDDModel.Create
//
//==============================================================================
constructor TDDModel.Create(const name: string;
  const xoffset, yoffset, zoffset: float;
  const xscale, yscale, zscale: float;
  const additionalframes: TDStringList);
var
  i: integer;
begin
  Inherited;
  printf('  Found external model %s'#13#10, [name]);
  fname := name;
  fxoffset := xoffset / MAP_COEFF;
  fyoffset := yoffset / MAP_COEFF;
  fzoffset := zoffset / MAP_COEFF;
  fxscale := xscale / MAP_COEFF;
  fyscale := yscale / MAP_COEFF;
  fzscale := zscale / MAP_COEFF;

  fmdl := TDDModelLoader.Create;
  fNumFrames := 0;
  precalc := nil;

  LoadFrom(name);

  if additionalframes <> nil then
    for i := 0 to additionalframes.Count - 1 do
      LoadFrom(additionalframes.Strings[i]);

  fNumFrames := fmdl.Frames.Count;

  precalc := mallocz(fNumFrames * SizeOf(GLuint));
end;

//==============================================================================
//
// TDDModel.LoadFrom
//
//==============================================================================
procedure TDDModel.LoadFrom(const fn: string);
var
  strm: TPakStream;
  ext: string;
  source: TDStringList;
begin
  strm := TPakStream.Create(fname, pm_prefered, gamedirectories);
  if strm.IOResult <> 0 then
  begin
    strm.Free;
    strm := TPakStream.Create(fname, pm_short, '', FOLDER_MODELS);
  end;
  if strm.IOResult <> 0 then
    I_Error('TDDModel.LoadFrom(): Can not find model %s!', [fname]);

  ext := strupper(fext(fn));
  if (ext = '.DDMODEL') or (ext = '.TXT') then
  begin
    source := TDStringList.Create;
    source.LoadFromStream(strm);
    fmdl.AppendFromScript(source.Text);
    source.Free;
  end
  else if ext = '.DMX' then
    fmdl.AppendFromStream(strm)
  else
    I_Error('TDDModel.LoadFrom(): Invalid model type %s!', [fname]);
  strm.Free;
end;

//==============================================================================
//
// TDDModel.Destroy
//
//==============================================================================
destructor TDDModel.Destroy;
var
  i: integer;
begin
  for i := 0 to fNumFrames - 1 do
    if precalc[i] > 0 then
      glDeleteLists(precalc[i], 1);

  memfree(pointer(precalc), fNumFrames * SizeOf(GLuint));
  fmdl.Free;

  Inherited;
end;

//==============================================================================
//
// TDDModel.Draw
//
//==============================================================================
procedure TDDModel.Draw(const frm1, frm2: integer; const offset: float);
var
  idx1, idx2: integer;
  idx: integer;
begin
  if (frm1 = frm2) or (frm2 < 0) or (offset < 0.01) then
  begin
    DrawSimple(frm1);
    Exit;
  end;
  if offset > 0.999 then
  begin
    DrawSimple(frm2);
    exit;
  end;

  idx1 := fmdl.Frames.IndexOf(frm1);
  idx2 := fmdl.Frames.IndexOf(frm2);

  if (idx1 >= 0) and (idx2 >= 0) then
  begin
    idx := Round(idx1 * (1.0 - offset) + idx2 * offset);
    DrawSimpleIdx(idx);
    Exit;
  end;

  if idx1 >= 0 then
  begin
    DrawSimpleIdx(idx1);
    Exit;
  end;

  if idx2 >= 0 then
  begin
    DrawSimpleIdx(idx2);
    Exit;
  end;
end;

//==============================================================================
//
// TDDModel.DrawSimpleIdx
//
//==============================================================================
procedure TDDModel.DrawSimpleIdx(const idx: integer);
begin
  if precalc[idx] > 0 then
    glCallList(precalc[idx])
  else
  begin
    precalc[idx] := glGenLists(1);

    glNewList(precalc[idx], GL_COMPILE_AND_EXECUTE);

    glMatrixMode(GL_MODELVIEW);
    glPushMatrix;
    glScalef(fxscale, fyscale, fzscale);
    glTranslatef(fxoffset, fyoffset, fzoffset);

    fmdl.RenderFrame(fmdl.Frames[idx]);

    glPopMatrix;

    glEndList;
  end;
end;

//==============================================================================
//
// TDDModel.DrawSimple
//
//==============================================================================
procedure TDDModel.DrawSimple(const frm: integer);
var
  idx: integer;
begin
  idx := fmdl.Frames.IndexOf(frm);
  if idx >= 0 then
  begin
    DrawSimpleIdx(idx);
    Exit;
  end;
  if fmdl.Frames.Count = 1 then
    DrawSimpleIdx(0);
end;

end.
