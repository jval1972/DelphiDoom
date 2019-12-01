//------------------------------------------------------------------------------
//
//  DelphiDoom: A modified and improved DOOM engine for Windows
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 2004-2017 by Jim Valavanis
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
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit ddc_base;

interface

uses
  Classes;

const
  DDCVERSION = 101;

function dll_compile(const game: string; const code: string; var pcode: string; var msgs: string): Boolean;

function dll_getuntisfuncdeclarations(const game: string): TStringList;

function dll_getconstants(const game: string): TStringList;

function dll_getvariables(const game: string): TStringList;

function dll_gettypes(const game: string): TStringList;

function dll_getclassesdeclarations(const game: string): TStringList;

function dll_getdisassembly(const game: string; const pcode: string): string;

function dll_getevents(const game: string): string;

implementation

uses
  Windows,
  SysUtils;
  
type
  dllcompilefunc_t =
    function (
      const _inp: PChar; const _inpsize: Integer;
      var _out: PChar; var _outsize: Integer;
      var _msgs: PChar; var _msgssize: Integer): Boolean; stdcall;

function dll_compile(const game: string; const code: string; var pcode: string; var msgs: string): Boolean;
var
  func: dllcompilefunc_t;
  inst: THandle;
  libname: string;
  funcname: string;
  clen, plen, mlen: integer;
  plen1, mlen1: integer;
  i: integer;
  _inp, _out, _msgs: PChar;
begin
  libname := 'ddc_' + game + '.dll';
  inst := LoadLibrary(PChar(libname));
  if inst = 0 then
  begin
    msgs := 'ERROR: ' + libname + ' not found.';
    Result := False;
    Exit;
  end;

  funcname := 'dd_compile_' + game;
  func := GetProcAddress(inst, PChar(funcname));
  if not Assigned(func) then
  begin
    FreeLibrary(inst);
    msgs := 'ERROR: ' + funcname + '() could not be loaded. (incorrect version?)';
    Result := False;
    Exit;
  end;

  clen := Length(code);
  GetMem(_inp, clen);
  for i := 1 to clen do
    _inp[i - 1] := code[i];

  // Guess ?
  plen1 := ((2 * clen) + 1024 * 16) and not 1023;
  plen := plen1;
  mlen := plen1;
  mlen1 := plen1;

  GetMem(_out, plen);
  GetMem(_msgs, mlen);

  Result := func(_inp, clen, _out, plen, _msgs, mlen);
  if (plen > plen1) or (mlen > mlen1) then
  begin
    FreeMem(_out, plen1);
    FreeMem(_msgs, mlen1);
    GetMem(_out, plen);
    GetMem(_msgs, mlen);
    plen1 := plen;
    mlen1 := mlen;
    Result := func(_inp, clen, _out, plen, _msgs, mlen);
  end;

  SetLength(pcode, plen);
  for i := 0 to plen - 1 do
    pcode[i + 1] := _out[i];

  SetLength(msgs, mlen);
  for i := 0 to mlen - 1 do
    msgs[i + 1] := _msgs[i];

  FreeMem(_out, plen1);
  FreeMem(_msgs, mlen1);
  FreeMem(_inp, clen);

  FreeLibrary(inst);
end;

// Returns a TStringList with the unit names
// The Object list of each unit string is a TStrinList with the unit declarations
type
  dll_getavailableunits_t =
    procedure (
      var _out: PChar; var _outsize: Integer); stdcall;

  dll_getunitfunctions_t =
    procedure (
      const _inp: PChar; const _inpsize: Integer;
      var _out: PChar; var _outsize: Integer); stdcall;

const
  MAXALLOCSIZE = $100000; // 1 MB :)

function dll_getuntisfuncdeclarations(const game: string): TStringList;
var
  funcunits: dll_getavailableunits_t;
  funcunitfunctions: dll_getunitfunctions_t;
  inst: THandle;
  libname: string;
  funcname: string;
  unitnames: PChar;
  unitnameslen: integer;
  unitdecls: PChar;
  unitdeclslen: integer;
  i, j: integer;
  tmpstr: string;
  funclist: TStringList;
begin
  libname := 'ddc_' + game + '.dll';
  inst := LoadLibrary(PChar(libname));
  if inst = 0 then
  begin
    Result := nil;
    Exit;
  end;

  funcname := 'dd_getavailableunits_' + game;
  funcunits := GetProcAddress(inst, PChar(funcname));
  if not Assigned(funcunits) then
  begin
    FreeLibrary(inst);
    Result := nil;
    Exit;
  end;

  funcname := 'dd_getunitfunctions_' + game;
  funcunitfunctions := GetProcAddress(inst, PChar(funcname));
  if not Assigned(funcunitfunctions) then
  begin
    FreeLibrary(inst);
    Result := nil;
    Exit;
  end;

  Result := TStringList.Create;

  GetMem(unitnames, MAXALLOCSIZE);
  GetMem(unitdecls, MAXALLOCSIZE);

  unitnameslen := MAXALLOCSIZE;
  funcunits(unitnames, unitnameslen);

  SetLength(tmpstr, unitnameslen);
  for i := 0 to unitnameslen - 1 do
    tmpstr[i + 1] := unitnames[i];

  Result.Text := tmpstr;

  for j := 0 to Result.Count - 1 do
  begin
    tmpstr := Result.Strings[j];
    for i := 1 to Length(tmpstr) do
      unitnames[i - 1] := tmpstr[i];
    funclist := TStringList.Create;

    unitdeclslen := MAXALLOCSIZE;
    funcunitfunctions(unitnames, Length(tmpstr), unitdecls, unitdeclslen);

    SetLength(tmpstr, unitdeclslen);
    for i := 0 to unitdeclslen - 1 do
      tmpstr[i + 1] := unitdecls[i];
    funclist.Text := tmpstr;

    Result.Objects[j] := funclist;
  end;

  FreeMem(unitnames, MAXALLOCSIZE);
  FreeMem(unitdecls, MAXALLOCSIZE);

  FreeLibrary(inst);
end;

type
  dll_getpchar_t =
    procedure (
      var _out: PChar; var _outsize: Integer); stdcall;

function dll_getpcharfunc(const game: string; const funcpr: string): TStringList;
var
  func: dll_getpchar_t;
  inst: THandle;
  libname: string;
  funcname: string;
  outp: PChar;
  outlen: integer;
  tmpstr: string;
  i: integer;
begin
  libname := 'ddc_' + game + '.dll';
  inst := LoadLibrary(PChar(libname));
  if inst = 0 then
  begin
    Result := nil;
    Exit;
  end;

  funcname := funcpr + game;
  func := GetProcAddress(inst, PChar(funcname));
  if not Assigned(func) then
  begin
    FreeLibrary(inst);
    Result := nil;
    Exit;
  end;

  Result := TStringList.Create;

  GetMem(outp, MAXALLOCSIZE);
  outlen := MAXALLOCSIZE;
  func(outp, outlen);

  SetLength(tmpstr, outlen);
  for i := 0 to outlen - 1 do
    if outp[i] = #0 then
      tmpstr[i + 1] := ' '
    else
      tmpstr[i + 1] := outp[i];

  Result.Text := tmpstr;

  FreeMem(outp, MAXALLOCSIZE);
  FreeLibrary(inst);
end;

function dll_getconstants(const game: string): TStringList;
begin
  Result := dll_getpcharfunc(game, 'dd_getconstants_');
end;

function dll_getvariables(const game: string): TStringList;
begin
  Result := dll_getpcharfunc(game, 'dd_getvariables_');
end;

function dll_gettypes(const game: string): TStringList;
begin
  Result := dll_getpcharfunc(game, 'dd_gettypes_');
end;

function dll_getclassesdeclarations(const game: string): TStringList;
var
  lst: TStringList;
  i, j, k: integer;
  b: boolean;
  s, s1, s2: string;
  decl: TStringList;
  declu: TStringList;
begin
  lst := dll_getpcharfunc(game, 'dd_getclasses_');
  if lst = nil then
  begin
    Result := nil;
    Exit;
  end;
  decl := TStringList.Create;
  Result := TStringList.Create;
  for i := 0 to lst.Count - 1 do
  begin
    s := lst.Strings[i];
    s1 := '';
    s2 := '';
    b := False;
    for j := 1 to Length(s) do
    begin
      if b then
        s2 := s2 + s[j]
      else if s[j] = '=' then
        b := True
      else
        s1 := s1 + s[j];
    end;
    s2 := StringReplace(s2, '|', #13#10, [rfReplaceAll]);
    decl.Text := s2;
    declu := TStringList.Create;
    for k := 0 to decl.Count - 1 do
      if declu.IndexOf(decl.Strings[k]) < 0 then
        declu.Add(decl.Strings[k]);
    Result.AddObject(s1, declu);
  end;
  lst.Free;
  decl.Free;
end;

type
  dll_getdisassembly_t =
    procedure (
      const _inp: PChar; const _inpsize: Integer;
      var _out: PChar; var _outsize: Integer); stdcall;

function dll_getdisassembly(const game: string; const pcode: string): string;
var
  func: dll_getdisassembly_t;
  inst: THandle;
  libname: string;
  funcname: string;
  outp: PChar;
  outlen: integer;
  inpp: PChar;
  inplen: integer;
  i: integer;
begin
  Result := '';

  libname := 'ddc_' + game + '.dll';
  inst := LoadLibrary(PChar(libname));
  if inst = 0 then
  begin
    Exit;
  end;

  funcname := 'dd_getdisassembly_' + game;
  func := GetProcAddress(inst, PChar(funcname));
  if not Assigned(func) then
  begin
    FreeLibrary(inst);
    Exit;
  end;

  inplen := Length(pcode);
  GetMem(inpp, inplen);
  for i := 0 to inplen - 1 do
    inpp[i] := pcode[i + 1];
  GetMem(outp, MAXALLOCSIZE);
  outlen := MAXALLOCSIZE;
  func(inpp, inplen, outp, outlen);

  SetLength(Result, outlen);
  for i := 0 to outlen - 1 do
    if outp[i] = #0 then
      Result[i + 1] := ' '
    else
      Result[i + 1] := outp[i];

  FreeMem(outp, MAXALLOCSIZE);
  FreeMem(inpp, inplen);
  FreeLibrary(inst);
end;

function dll_getevents(const game: string): string;
var
  lst: TStringList;
begin
  lst := dll_getpcharfunc(game, 'dd_getevents_');
  if lst = nil then
  begin
    Result := '';
    Exit;
  end;
  Result := lst.Text;
  lst.Free;
end;

end.

