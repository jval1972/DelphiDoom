//------------------------------------------------------------------------------
//
//  DelphiDoom: A modified and improved DOOM engine for Windows
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 2004-2019 by Jim Valavanis
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
//  Dll loading (inside PK3 files)
//
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit ps_dll;

interface

procedure PS_InitDLLLoader;

function PS_PAKLoadDll(const dllname: string): LongWord;

function PS_PAKUnLoadDll(const dllname: string): boolean;

procedure PS_ShutDownDLLLoader;

implementation

uses
  Windows,
  d_delphi,
  w_folders,
  w_pak,
  doomdef,
  i_tmp,
  i_system;

var
  dlls: TDStringList;

type
  THandleClass = class(TObject)
  private
    fHandle: LongWord;
    fLoadCount: integer;
    fFilename: string;
  public
    constructor Create(const aHandle: LongWord);
    property Handle: LongWord read fHandle;
    property LoadCount: integer read fLoadCount write fLoadCount;
    property Filename: string read fFilename write fFilename;
  end;

constructor THandleClass.Create(const aHandle: LongWord);
begin
  inherited Create;
  fHandle := aHandle;
  fLoadCount := 0;
  fFilename := '';
end;

procedure PS_InitDLLLoader;
begin
  dlls := TDStringList.Create;
end;

function PS_PAKLoadDll(const dllname: string): LongWord;
var
  ps: TPakStream;
  dllname2: string;
  fname: string;
  tmpname: string;
  idx: integer;
  hc: THandleClass;
begin
  fname := fshortname(dllname);
  idx := dlls.IndexOf(strupper(fname));
  if idx >= 0 then
  begin
    hc := dlls.Objects[idx] as THandleClass;
    Result := hc.Handle;
    hc.LoadCount := hc.LoadCount + 1;
    Exit;
  end;
  dllname2 := dllname;
  fname := fshortname(dllname);
  ps := TPakStream.Create(dllname2, pm_prefered, gamedirectories);
  if ps.IOResult <> 0 then
  begin
    ps.Free;
    ps := TPakStream.Create(fname, pm_short, '', FOLDER_DLL);
    if ps.IOResult <> 0 then
    begin
      ps.Free;
      Result := 0;
      exit;
    end;
  end;

  tmpname := I_NewTempFile(fname);
  SaveStreamToFile(ps, tmpname);
  ps.Free;

  Result := LoadLibrary(PChar(tmpname));
  if Result = 0 then
    Exit;

  hc := THandleClass.Create(result);
  hc.LoadCount := 1;
  hc.Filename := tmpname;
  dlls.AddObject(strupper(fname), hc);
end;

function PS_PAKUnLoadDll(const dllname: string): boolean;
var
  fname: string;
  idx: integer;
  hc: THandleClass;
begin
  fname := strupper(fshortname(dllname));
  idx := dlls.IndexOf(fname);
  if idx < 0 then
  begin
    result := false;
    exit;
  end;

  hc := dlls.Objects[idx] as THandleClass;
  if hc.LoadCount <= 0 then
    I_Warning('PS_PAKUnLoadDll(): Zero LoadCount on dll "%s"'#13#10, [dlls.Strings[idx]])
  else
    hc.LoadCount := hc.LoadCount - 1;

  if hc.LoadCount = 0 then
  begin
    FreeLibrary(hc.Handle);
    hc.Free;
    dlls.Delete(idx);
  end;

  Result := True;
end;

procedure PS_ShutDownDLLLoader;
var
  i: integer;
begin
  for i := 0 to dlls.Count - 1 do
    (dlls.Objects[i] as THandleClass).Free;
  dlls.Free;
end;

end.

