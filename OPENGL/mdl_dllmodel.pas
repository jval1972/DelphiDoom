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
//    External DLL model support
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit mdl_dllmodel;

interface

uses
  d_delphi,
  mdl_base,
  m_smartpointerlist,
  ps_dll;

type
  drawproc_t = function (const frm1, frm2: integer; const offs: float): boolean; stdcall;

type
  TDLLModel = class(TBaseModel)
  protected
    fname: string;
    drawprocs: TSmartPointerList;
    libs: TDStringList;
    fxoffset, fyoffset, fzoffset: float;
    fxscale, fyscale, fzscale: float;
    finitialprocname: string;
    procedure LoadFrom(const libname: string);
  public
    constructor Create(const name: string;
      const xoffset, yoffset, zoffset: float;
      const xscale, yscale, zscale: float;
      const additionalframes: TDStringList); override;
    constructor CreateWithProc(const name, proc: string;
      const xoffset, yoffset, zoffset: float;
      const xscale, yscale, zscale: float;
      const additionalframes: TDStringList); virtual;
    destructor Destroy; override;
    procedure Draw(const frm1, frm2: integer; const offset: float); override;
    procedure DrawSimple(const frm: integer); override;
  end;

implementation

uses
  Windows,
  dglOpenGL,
  gl_defs,
  i_system;

//==============================================================================
//
// TDLLModel.Create
//
//==============================================================================
constructor TDLLModel.Create(const name: string;
  const xoffset, yoffset, zoffset: float;
  const xscale, yscale, zscale: float;
  const additionalframes: TDStringList);
begin
  CreateWithProc(name, '', xoffset, yoffset, zoffset, xscale, yscale, zscale, additionalframes);
end;

//==============================================================================
//
// TDLLModel.CreateWithProc
//
//==============================================================================
constructor TDLLModel.CreateWithProc(const name, proc: string;
  const xoffset, yoffset, zoffset: float;
  const xscale, yscale, zscale: float;
  const additionalframes: TDStringList);
var
  i: integer;
begin
  Inherited Create(name, xoffset, yoffset, zoffset, xscale, yscale, zscale, additionalframes);
  printf('  Found external model %s'#13#10, [name]);
  fname := name;
  finitialprocname := proc;
  fxoffset := xoffset / MAP_COEFF;
  fyoffset := yoffset / MAP_COEFF;
  fzoffset := zoffset / MAP_COEFF;
  fxscale := xscale / MAP_COEFF;
  fyscale := yscale / MAP_COEFF;
  fzscale := zscale / MAP_COEFF;

  drawprocs := TSmartPointerList.Create;
  libs := TDStringList.Create;

  LoadFrom(name);

  if additionalframes <> nil then
    for i := 0 to additionalframes.Count - 1 do
      LoadFrom(additionalframes.Strings[i]);
end;

//==============================================================================
//
// TDLLModel.LoadFrom
//
//==============================================================================
procedure TDLLModel.LoadFrom(const libname: string);
var
  handle: LongWord;
  proc: drawproc_t;
begin
  proc := nil;
  handle := PS_PAKLoadDll(libname);
  if handle <> 0 then
  begin
    if finitialprocname <> '' then
    begin
      proc := GetProcAddress(handle, PAnsiChar(finitialprocname));
      if not assigned(proc) then
        proc := GetProcAddress(handle, PAnsiChar('_' + finitialprocname));
    end;
    if not assigned(proc) then
    begin
      proc := GetProcAddress(handle, 'draw');
      if not assigned(proc) then
        proc := GetProcAddress(handle, '_draw');
    end;
    if assigned(proc) then
      drawprocs.AddItem(@proc);
  end;
  if not Assigned(proc) then
    I_Error('TDLLModel.LoadFrom(): Can not load model %s', [libname]);
  libs.Add(libname);
end;

//==============================================================================
//
// TDLLModel.Destroy
//
//==============================================================================
destructor TDLLModel.Destroy;
var
  i: integer;
begin
  for i := 0 to libs.Count - 1 do
    PS_PAKUnLoadDll(libs.Strings[i]);

  libs.Free;
  drawprocs.Free;
  Inherited;
end;

//==============================================================================
//
// TDLLModel.Draw
//
//==============================================================================
procedure TDLLModel.Draw(const frm1, frm2: integer; const offset: float);
var
  i: integer;
begin
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix;
  glScalef(fxscale, fyscale, fzscale);
  glTranslatef(fxoffset, fyoffset, fzoffset);

  for i := 0 to drawprocs.Count - 1 do
    if drawproc_t(drawprocs.List[i])(frm1, frm2, offset) then
      Break;

  glPopMatrix;
end;

//==============================================================================
//
// TDLLModel.DrawSimple
//
//==============================================================================
procedure TDLLModel.DrawSimple(const frm: integer);
begin
  Draw(frm, frm, 0.0);
end;

end.
