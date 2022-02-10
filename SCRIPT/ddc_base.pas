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
//  Foundation, inc., 59 Temple Place - Suite 330, Boston, MA
//  02111-1307, USA.
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit ddc_base;

interface

uses
  Classes;

const
  DDCVERSION = 103;

//==============================================================================
//
// dll_loadlibrary
//
//==============================================================================
procedure dll_loadlibrary(const game: string);

//==============================================================================
//
// dll_freelibrary
//
//==============================================================================
procedure dll_freelibrary;

//==============================================================================
//
// dll_compile
//
//==============================================================================
function dll_compile(const game: string; const code: string; var pcode: string; var msgs: string): Boolean;

//==============================================================================
//
// dll_getuntisfuncdeclarations
//
//==============================================================================
function dll_getuntisfuncdeclarations(const game: string): TStringList;

//==============================================================================
//
// dll_getpcharfunc
//
//==============================================================================
function dll_getpcharfunc(const game: string; const funcpr: string): TStringList;

//==============================================================================
//
// dll_getconstants
//
//==============================================================================
function dll_getconstants(const game: string): TStringList;

//==============================================================================
//
// dll_getvariables
//
//==============================================================================
function dll_getvariables(const game: string): TStringList;

//==============================================================================
//
// dll_gettypes
//
//==============================================================================
function dll_gettypes(const game: string): TStringList;

//==============================================================================
//
// dll_getclassesdeclarations
//
//==============================================================================
function dll_getclassesdeclarations(const game: string): TStringList;

//==============================================================================
//
// dll_getdisassembly
//
//==============================================================================
function dll_getdisassembly(const game: string; const pcode: string): string;

//==============================================================================
//
// dll_getevents
//
//==============================================================================
function dll_getevents(const game: string): string;

//==============================================================================
//
// dll_getactordeffunctions
//
//==============================================================================
function dll_getactordeffunctions(const game: string): TStringList;

//==============================================================================
//
// csvlinetolist
//
//==============================================================================
function csvlinetolist(const s: string): TStringList;

//==============================================================================
//
// dll_getmobjinfodeclarations
//
//==============================================================================
function dll_getmobjinfodeclarations(const game: string): TStringList;

//==============================================================================
//
// dll_getstatesdeclarations
//
//==============================================================================
function dll_getstatesdeclarations(const game: string): TStringList;

//==============================================================================
//
// dll_getspritenames
//
//==============================================================================
function dll_getspritenames(const game: string): TStringList;

//==============================================================================
//
// dll_getstatesdeclarations2
//
//==============================================================================
function dll_getstatesdeclarations2(const game: string): TStringList;

//==============================================================================
//
// getcolumnfromcsv
//
//==============================================================================
function getcolumnfromcsv(const csv: TStringList; const cname: string): TStringList;

implementation

uses
  Windows,
  SysUtils;

var
  inst: THandle = 0;

//==============================================================================
//
// dll_loadlibrary
//
//==============================================================================
procedure dll_loadlibrary(const game: string);
var
  libname: string;
begin
  libname := 'ddc_' + game + '.dll';
  inst := LoadLibrary(PChar(libname));
end;

//==============================================================================
//
// dll_freelibrary
//
//==============================================================================
procedure dll_freelibrary;
begin
  if inst <> 0 then
  begin
    FreeLibrary(inst);
    inst := 0;
  end;
end;

var
  localload: Boolean = False;

//==============================================================================
//
// localLoadLibrary
//
//==============================================================================
function localLoadLibrary(lpLibFileName: PChar): HMODULE; stdcall;
begin
  if inst = 0 then
  begin
    inst := LoadLibrary(lpLibFileName);
    localload := True;
  end
  else
    localload := False;
  Result := inst;
end;

//==============================================================================
//
// localFreeLibrary
//
//==============================================================================
function localFreeLibrary(var hLibModule: HMODULE): BOOL; stdcall;
begin
  if localload then
  begin
    Result := FreeLibrary(hLibModule);
    localload := False;
    hLibModule := 0;
  end
  else
    Result := False;
end;

type
  dllcompilefunc_t =
    function (
      const _inp: PChar; const _inpsize: Integer;
      var _out: PChar; var _outsize: Integer;
      var _msgs: PChar; var _msgssize: Integer): Boolean; stdcall;

//==============================================================================
//
// dll_compile
//
//==============================================================================
function dll_compile(const game: string; const code: string; var pcode: string; var msgs: string): Boolean;
var
  func: dllcompilefunc_t;
  libname: string;
  funcname: string;
  clen, plen, mlen: integer;
  plen1, mlen1: integer;
  i: integer;
  _inp, _out, _msgs: PChar;
begin
  dll_LoadLibrary(PChar(game));
  if inst = 0 then
  begin
    libname := 'ddc_' + game + '.dll';
    msgs := 'ERROR: ' + libname + ' not found.';
    Result := False;
    Exit;
  end;

  funcname := 'dd_compile_' + game;
  func := GetProcAddress(inst, PChar(funcname));
  if not Assigned(func) then
  begin
    localFreeLibrary(inst);
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

  dll_FreeLibrary;
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

//==============================================================================
//
// dll_getuntisfuncdeclarations
//
//==============================================================================
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
  inst := localLoadLibrary(PChar(libname));
  if inst = 0 then
  begin
    Result := nil;
    Exit;
  end;

  funcname := 'dd_getavailableunits_' + game;
  funcunits := GetProcAddress(inst, PChar(funcname));
  if not Assigned(funcunits) then
  begin
    localFreeLibrary(inst);
    Result := nil;
    Exit;
  end;

  funcname := 'dd_getunitfunctions_' + game;
  funcunitfunctions := GetProcAddress(inst, PChar(funcname));
  if not Assigned(funcunitfunctions) then
  begin
    localFreeLibrary(inst);
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

  localFreeLibrary(inst);
end;

type
  dll_getpchar_t =
    procedure (
      var _out: PChar; var _outsize: Integer); stdcall;

//==============================================================================
//
// dll_getpcharfunc
//
//==============================================================================
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
  inst := localLoadLibrary(PChar(libname));
  if inst = 0 then
  begin
    Result := nil;
    Exit;
  end;

  funcname := funcpr + game;
  func := GetProcAddress(inst, PChar(funcname));
  if not Assigned(func) then
  begin
    localFreeLibrary(inst);
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
  localFreeLibrary(inst);
end;

//==============================================================================
//
// dll_getconstants
//
//==============================================================================
function dll_getconstants(const game: string): TStringList;
begin
  Result := dll_getpcharfunc(game, 'dd_getconstants_');
end;

//==============================================================================
//
// dll_getvariables
//
//==============================================================================
function dll_getvariables(const game: string): TStringList;
begin
  Result := dll_getpcharfunc(game, 'dd_getvariables_');
end;

//==============================================================================
//
// dll_gettypes
//
//==============================================================================
function dll_gettypes(const game: string): TStringList;
begin
  Result := dll_getpcharfunc(game, 'dd_gettypes_');
end;

//==============================================================================
//
// dll_getclassesdeclarations
//
//==============================================================================
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

//==============================================================================
//
// dll_getdisassembly
//
//==============================================================================
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
  inst := localLoadLibrary(PChar(libname));
  if inst = 0 then
  begin
    Exit;
  end;

  funcname := 'dd_getdisassembly_' + game;
  func := GetProcAddress(inst, PChar(funcname));
  if not Assigned(func) then
  begin
    localFreeLibrary(inst);
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
  localFreeLibrary(inst);
end;

//==============================================================================
//
// dll_getevents
//
//==============================================================================
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

//==============================================================================
//
// dll_getactordeffunctions
//
//==============================================================================
function dll_getactordeffunctions(const game: string): TStringList;
begin
  Result := dll_getpcharfunc(game, 'dd_getactordeffunctions_');
end;

//==============================================================================
//
// stripquotes
//
//==============================================================================
function stripquotes(const s: string): string;
begin
  if Length(s) < 2 then
  begin
    Result := s;
    Exit;
  end;
  if (s[1] = '"') and (s[Length(s)] = '"') then
    Result := Copy(s, 2, Length(s) - 2)
  else
    Result := s;
end;

//==============================================================================
//
// csvlinetolist
//
//==============================================================================
function csvlinetolist(const s: string): TStringList;
var
  i: integer;
  token: string;
begin
  Result := TStringList.Create;
  token := '';
  for i := 1 to Length(s) do
  begin
    if s[i] = ';' then
    begin
      Result.Add(stripquotes(token));
      token := '';
    end
    else
      token := token + s[i];
  end;
  token := stripquotes(token);
  if token <> '' then
    Result.Add(token);
end;

//==============================================================================
//
// csvexpandfracunit
//
//==============================================================================
function csvexpandfracunit(const s: string): string;
var
  check: string;
  num: integer;
begin
  num := StrToIntDef(s, -1);
  if num < 65536 then
  begin
    Result := s;
    Exit;
  end;

  check := IntToStr(num);
  if check <> s then
  begin
    Result := s;
    Exit;
  end;

  if num mod 65536 <> 0 then
  begin
    Result := s;
    Exit;
  end;

  if num = 65536 then
    Result := 'FRACUNIT'
  else
    Result := IntToStr(num div 65536) + ' * FRACUNIT';
end;

//==============================================================================
//
// dll_getmobjinfodeclarations
//
//==============================================================================
function dll_getmobjinfodeclarations(const game: string): TStringList;
var
  mlst: TStringList;
  i, j: integer;
  declu: TStringList;
  mheader: TStringList;
  mline: TStringList;
begin
  mlst := dll_getpcharfunc(game, 'dd_getmobjinfocsv_');
  if mlst = nil then
  begin
    Result := nil;
    Exit;
  end;
  if mlst.Count = 0 then
  begin
    mlst.Free;
    Result := nil;
    Exit;
  end;

  Result := TStringList.Create;
  mheader := csvlinetolist(mlst.Strings[0]);

  for i := 1 to mlst.Count - 1 do
  begin
    mline := csvlinetolist(mlst.Strings[i]);
    if mline.Count = mheader.Count then
    begin
      declu := TStringList.Create;
      for j := 0 to mline.Count - 1 do
        declu.Add(mheader[j] + ': ' + csvexpandfracunit(mline[j]));
      Result.AddObject(mline[1], declu);
    end;
    mline.Free;
  end;
  mlst.Free;
  mheader.Free;
end;

//==============================================================================
//
// dll_getstatesdeclarations
//
//==============================================================================
function dll_getstatesdeclarations(const game: string): TStringList;
var
  slst: TStringList;
  i, j: integer;
  declu: TStringList;
  sheader: TStringList;
  sline: TStringList;
begin
  slst := dll_getpcharfunc(game, 'dd_getstatescsv_');
  if slst = nil then
  begin
    Result := nil;
    Exit;
  end;
  if slst.Count = 0 then
  begin
    slst.Free;
    Result := nil;
    Exit;
  end;

  Result := TStringList.Create;
  sheader := csvlinetolist(slst.Strings[0]);

  for i := 1 to slst.Count - 1 do
  begin
    sline := csvlinetolist(slst.Strings[i]);
    if sline.Count = sheader.Count then
    begin
      declu := TStringList.Create;
      for j := 0 to sline.Count - 1 do
        declu.Add(sheader[j] + ': ' + sline[j]);
      Result.AddObject('State #' + sline[0], declu);
    end;
    sline.Free;
  end;
  slst.Free;
  sheader.Free;
end;

//==============================================================================
//
// dll_getspritenames
//
//==============================================================================
function dll_getspritenames(const game: string): TStringList;
var
  sprlst: TStringList;
  i: integer;
  sprheader: TStringList;
  sprline: TStringList;
begin
  sprlst := dll_getpcharfunc(game, 'dd_getspritescsv_');
  if sprlst = nil then
  begin
    Result := nil;
    Exit;
  end;
  if sprlst.Count = 0 then
  begin
    sprlst.Free;
    Result := nil;
    Exit;
  end;

  Result := TStringList.Create;
  sprheader := csvlinetolist(sprlst.Strings[0]);

  for i := 1 to sprlst.Count - 1 do
  begin
    sprline := csvlinetolist(sprlst.Strings[i]);
    if sprline.Count = sprheader.Count then
      if sprline.Count = 2 then
        Result.Add(sprline.Strings[1]);
    sprline.Free;
  end;
  sprlst.Free;
  sprheader.Free;
end;

//==============================================================================
//
// dll_getstatesdeclarations2
//
//==============================================================================
function dll_getstatesdeclarations2(const game: string): TStringList;
var
  slst: TStringList;
  i, j: integer;
  declu: TStringList;
  sheader: TStringList;
  sline: TStringList;
begin
  slst := dll_getpcharfunc(game, 'dd_getstatescsv_');
  if slst = nil then
  begin
    Result := nil;
    Exit;
  end;
  if slst.Count = 0 then
  begin
    slst.Free;
    Result := nil;
    Exit;
  end;

  Result := TStringList.Create;
  sheader := csvlinetolist(slst.Strings[0]);

  for i := 1 to slst.Count - 1 do
  begin
    sline := csvlinetolist(slst.Strings[i]);
    if sline.Count = sheader.Count then
    begin
      declu := TStringList.Create;
      for j := 0 to sline.Count - 1 do
        declu.Add(sheader[j] + ': ' + sline[j]);
      Result.AddObject(sline[0], declu);
    end;
    sline.Free;
  end;
  slst.Free;
  sheader.Free;
end;

//==============================================================================
//
// getcolumnfromcsv
//
//==============================================================================
function getcolumnfromcsv(const csv: TStringList; const cname: string): TStringList;
var
  i, idx: integer;
  sheader: TStringList;
  lst: TStringList;
begin
  Result := TStringList.Create;

  if csv = nil then
    Exit;

  if csv.Count = 0 then
    Exit;

  sheader := csvlinetolist(csv.Strings[0]);

  if sheader = nil then
    Exit;

  idx := -1;
  for i := 0 to sheader.Count - 1 do
    if UpperCase(cname) = UpperCase(sheader.Strings[i]) then
    begin
      idx := i;
      Break;
    end;

  sheader.Free;

  if idx = -1 then
    Exit;

  for i := 1 to csv.Count - 1 do
  begin
    lst := csvlinetolist(csv.Strings[i]);
    if idx < lst.Count then
      Result.Add(lst.Strings[idx])
    else
      Result.Add('');
    lst.Free;
  end;
end;

end.

