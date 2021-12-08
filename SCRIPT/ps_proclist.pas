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
//  Foundation, inc., 59 Temple Place - Suite 330, Boston, MA
//  02111-1307, USA.
//
// DESCRIPTION:
//  Procedure list container for Pascal Script.
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit ps_proclist;

interface

uses
  d_delphi,
  ps_compiler,
  ps_runtime;

type
  procitem_t = record
    proc: Pointer;
    decl: TString;
    name: TString;
    customresult: TString;
    exportdecl: TString;
    iscomputed: boolean;
  end;
  Pprocitem_t = ^procitem_t;
  procitem_tArray = array[0..$FFF] of procitem_t;
  Pprocitem_tArray = ^procitem_tArray;

type
  TProcedureList = class(TObject)
  private
    fList: Pprocitem_tArray;
    fNumItems: integer;
    fRealSize: integer;
    fName: string;
  public
    constructor Create(const aName: string); virtual;
    destructor Destroy; override;
    procedure Add(const decl: string; const proc: pointer); virtual;
    procedure AddWithCustomResult(const decl: string; const ret: string;
      const decl2: string; proc: pointer); virtual;
    procedure RegisterProcsComp(Sender: TPSPascalCompiler); virtual;
    procedure RegisterProcsExec(Sender: TPSExec); virtual;
    procedure Reset;
    function GetDeclarations: string;
    function GetFunctionNames: string;
    property Count: integer read fNumItems;
    property Name: string read fName;
  end;

implementation

constructor TProcedureList.Create(const aName: string);
begin
  fList := nil;
  fNumItems := 0;
  fRealSize := 0;
  fName := aName;
  inherited Create;
end;


destructor TProcedureList.Destroy;
var
  i: integer;
begin
  if fRealSize > 0 then
  begin
    for i := 0 to fRealSize - 1 do
    begin
      fList[i].decl.Free;
      fList[i].name.Free;
      fList[i].customresult.Free;
      fList[i].exportdecl.Free;
    end;
    realloc(pointer(fList), fRealSize * SizeOf(procitem_t), 0);
  end;

  inherited;
end;

procedure TProcedureList.Add(const decl: string; const proc: pointer);
const
  REALLOCSTEP = 16;
var
  i: integer;
  decl1: string;
begin
  decl1 := strtrim(decl);
  if decl1 = '' then
    Exit;

  if decl1[Length(decl1)] <> ';' then
    decl1 := decl1 + ';';
  if fNumItems >= fRealSize then
  begin
    realloc(pointer(fList), fRealSize * SizeOf(procitem_t), (fRealSize + REALLOCSTEP) * SizeOf(procitem_t));
    for i := fRealSize to fRealSize + REALLOCSTEP - 1 do
    begin
      fList[i].decl := TString.Create('');
      fList[i].name := TString.Create('');
      fList[i].customresult := TString.Create('');
      fList[i].exportdecl := TString.Create('');
    end;
    fRealSize := fRealSize + REALLOCSTEP;
  end;
  fList[fNumItems].proc := proc;
  fList[fNumItems].decl.str := decl1;
  fList[fNumItems].exportdecl.str := decl1;
  fList[fNumItems].iscomputed := true;
  inc(fNumItems);
end;

procedure TProcedureList.AddWithCustomResult(const decl: string; const ret: string;
      const decl2: string; proc: pointer);
const
  REALLOCSTEP = 16;
var
  i: integer;
  decl1: string;
begin
  decl1 := strtrim(decl);
  if decl1 = '' then
    Exit;

  if decl1[Length(decl1)] <> ';' then
    decl1 := decl1 + ';';
  if fNumItems >= fRealSize then
  begin
    realloc(pointer(fList), fRealSize * SizeOf(procitem_t), (fRealSize + REALLOCSTEP) * SizeOf(procitem_t));
    for i := fRealSize to fRealSize + REALLOCSTEP - 1 do
    begin
      fList[i].decl := TString.Create('');
      fList[i].name := TString.Create('');
      fList[i].customresult := TString.Create('');
      fList[i].exportdecl := TString.Create('');
    end;
    fRealSize := fRealSize + REALLOCSTEP;
  end;
  fList[fNumItems].proc := proc;
  fList[fNumItems].decl.str := decl1;
  fList[fNumItems].customresult.str := ret;
  fList[fNumItems].exportdecl.str := decl2;
  fList[fNumItems].iscomputed := true;
  inc(fNumItems);
end;

procedure TProcedureList.RegisterProcsComp(Sender: TPSPascalCompiler);
var
  i: integer;
  reg: TPSRegProc;
begin
  for i := 0 to fNumItems - 1 do
  begin
    if fList[i].customresult.str <> '' then
      reg := Sender.AddDelphiFunctionWithRTLObjectResult(fList[i].decl.str, fList[i].customresult.str)
    else
      reg := Sender.AddDelphiFunction(fList[i].decl.str);
    if reg <> nil then
    begin
      fList[i].name.str := reg.Name;
      fList[i].iscomputed := true;
    end;
  end;
end;

procedure TProcedureList.RegisterProcsExec(Sender: TPSExec);
var
  i: integer;
begin
  for i := 0 to fNumItems - 1 do
    if fList[i].iscomputed then
      Sender.RegisterDelphiFunction(fList[i].proc, fList[i].name.str, cdRegister);
end;

procedure TProcedureList.Reset;
var
  i: integer;
begin
  for i := 0 to fNumItems - 1 do
    fList[i].iscomputed := false;
end;

function TProcedureList.GetDeclarations: string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to fNumItems - 1 do
    Result := Result + flist[i].exportdecl.str + #13#10;
end;

function TProcedureList.GetFunctionNames: string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to fNumItems - 1 do
    Result := Result + flist[i].name.str + #13#10;
end;

end.
