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
//  DESCRIPTION:
//    Stand alone script compiler
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

unit dd_compiler;

interface

{$IFDEF DOOM}
function dd_compile_doom(
{$ENDIF}
{$IFDEF HERETIC}
function dd_compile_heretic(
{$ENDIF}
{$IFDEF HEXEN}
function dd_compile_hexen(
{$ENDIF}
{$IFDEF STRIFE}
function dd_compile_strife(
{$ENDIF}
  const _inp: PChar; const _inpsize: Integer;
  var _out: PChar; var _outsize: Integer;
  var _msgs: PChar; var _msgssize: Integer): Boolean; stdcall;

{$IFDEF DOOM}
procedure dd_getavailableunits_doom(
{$ENDIF}
{$IFDEF HERETIC}
procedure dd_getavailableunits_heretic(
{$ENDIF}
{$IFDEF HEXEN}
procedure dd_getavailableunits_hexen(
{$ENDIF}
{$IFDEF STRIFE}
procedure dd_getavailableunits_strife(
{$ENDIF}
  var _out: PChar; var _outsize: Integer); stdcall;

{$IFDEF DOOM}
procedure dd_getunitfunctions_doom(
{$ENDIF}
{$IFDEF HERETIC}
procedure dd_getunitfunctions_heretic(
{$ENDIF}
{$IFDEF HEXEN}
procedure dd_getunitfunctions_hexen(
{$ENDIF}
{$IFDEF STRIFE}
procedure dd_getunitfunctions_strife(
{$ENDIF}
  const _inp: PChar; const _inpsize: Integer;
  var _out: PChar; var _outsize: Integer); stdcall;

{$IFDEF DOOM}
procedure dd_getconstants_doom(
{$ENDIF}
{$IFDEF HERETIC}
procedure dd_getconstants_heretic(
{$ENDIF}
{$IFDEF HEXEN}
procedure dd_getconstants_hexen(
{$ENDIF}
{$IFDEF STRIFE}
procedure dd_getconstants_strife(
{$ENDIF}
  var _out: PChar; var _outsize: Integer); stdcall;

{$IFDEF DOOM}
procedure dd_getvariables_doom(
{$ENDIF}
{$IFDEF HERETIC}
procedure dd_getvariables_heretic(
{$ENDIF}
{$IFDEF HEXEN}
procedure dd_getvariables_hexen(
{$ENDIF}
{$IFDEF STRIFE}
procedure dd_getvariables_strife(
{$ENDIF}
  var _out: PChar; var _outsize: Integer); stdcall;

{$IFDEF DOOM}
procedure dd_gettypes_doom(
{$ENDIF}
{$IFDEF HERETIC}
procedure dd_gettypes_heretic(
{$ENDIF}
{$IFDEF HEXEN}
procedure dd_gettypes_hexen(
{$ENDIF}
{$IFDEF STRIFE}
procedure dd_gettypes_strife(
{$ENDIF}
  var _out: PChar; var _outsize: Integer); stdcall;

{$IFDEF DOOM}
procedure dd_getclasses_doom(
{$ENDIF}
{$IFDEF HERETIC}
procedure dd_getclasses_heretic(
{$ENDIF}
{$IFDEF HEXEN}
procedure dd_getclasses_hexen(
{$ENDIF}
{$IFDEF STRIFE}
procedure dd_getclasses_strife(
{$ENDIF}
  var _out: PChar; var _outsize: Integer); stdcall;

{$IFDEF DOOM}
procedure dd_getdisassembly_doom(
{$ENDIF}
{$IFDEF HERETIC}
procedure dd_getdisassembly_heretic(
{$ENDIF}
{$IFDEF HEXEN}
procedure dd_getdisassembly_hexen(
{$ENDIF}
{$IFDEF STRIFE}
procedure dd_getdisassembly_strife(
{$ENDIF}
  const _inp: PChar; const _inpsize: Integer;
  var _out: PChar; var _outsize: Integer); stdcall;

{$IFDEF DOOM}
procedure dd_getevents_doom(
{$ENDIF}
{$IFDEF HERETIC}
procedure dd_getevents_heretic(
{$ENDIF}
{$IFDEF HEXEN}
procedure dd_getevents_hexen(
{$ENDIF}
{$IFDEF STRIFE}
procedure dd_getevents_strife(
{$ENDIF}
  var _out: PChar; var _outsize: Integer); stdcall;

{$IFDEF DOOM}
procedure dd_getactordeffunctions_doom(
{$ENDIF}
{$IFDEF HERETIC}
procedure dd_getactordeffunctions_heretic(
{$ENDIF}
{$IFDEF HEXEN}
procedure dd_getactordeffunctions_hexen(
{$ENDIF}
{$IFDEF STRIFE}
procedure dd_getactordeffunctions_strife(
{$ENDIF}
  var _out: PChar; var _outsize: Integer); stdcall;

{$IFDEF DOOM}
procedure dd_getmobjinfocsv_doom(
{$ENDIF}
{$IFDEF HERETIC}
procedure dd_getmobjinfocsv_heretic(
{$ENDIF}
{$IFDEF HEXEN}
procedure dd_getmobjinfocsv_hexen(
{$ENDIF}
{$IFDEF STRIFE}
procedure dd_getmobjinfocsv_strife(
{$ENDIF}
  var _out: PChar; var _outsize: Integer); stdcall;

{$IFDEF DOOM}
procedure dd_getstatescsv_doom(
{$ENDIF}
{$IFDEF HERETIC}
procedure dd_getstatescsv_heretic(
{$ENDIF}
{$IFDEF HEXEN}
procedure dd_getstatescsv_hexen(
{$ENDIF}
{$IFDEF STRIFE}
procedure dd_getstatescsv_strife(
{$ENDIF}
  var _out: PChar; var _outsize: Integer); stdcall;

{$IFDEF DOOM}
procedure dd_getspritescsv_doom(
{$ENDIF}
{$IFDEF HERETIC}
procedure dd_getspritescsv_heretic(
{$ENDIF}
{$IFDEF HEXEN}
procedure dd_getspritescsv_hexen(
{$ENDIF}
{$IFDEF STRIFE}
procedure dd_getspritescsv_strife(
{$ENDIF}
  var _out: PChar; var _outsize: Integer); stdcall;

{$IFDEF DOOM}
procedure dd_getactordef_doom(
{$ENDIF}
{$IFDEF HERETIC}
procedure dd_getactordef_heretic(
{$ENDIF}
{$IFDEF HEXEN}
procedure dd_getactordef_hexen(
{$ENDIF}
{$IFDEF STRIFE}
procedure dd_getactordef_strife(
{$ENDIF}
  var m: integer; var _out: PChar; var _outsize: Integer); stdcall;

implementation

uses
  Classes,
  SysUtils,
  d_delphi,
  deh_base,
  deh_main,
  info,
  info_h,
  info_common,
  sc_actordef,
  sc_states,
  ps_events,
  ps_import,
  ps_main,
  ps_proclist,
  ps_compiler,
  uPSDisassembly,
  ps_utils,
  ps_defs;

//==============================================================================
//
// DD_InitDoomEngine
//
//==============================================================================
procedure DD_InitDoomEngine;
begin
  ThousandSeparator := #0;
  DecimalSeparator := '.';

  PS_InitProcLists;
  SC_DefaultStatedefLump;
  Info_Init(true);
  DEH_Init;
  SC_Init;
  SC_ParseStatedefLump;
end;

//==============================================================================
//
// DD_ShutDownDoomEngine
//
//==============================================================================
procedure DD_ShutDownDoomEngine;
begin
  SC_ShutDown;
  DEH_ShutDown;
  Info_ShutDown;
  PS_ShutDownProcLists;
end;

//==============================================================================
//
// DD_Compile
//
//==============================================================================
function DD_Compile(const _inp: string; var _out: string; var _msgs: string): boolean;
var
  c: TDoomCompiler;
  i: integer;
begin
  try
    c := TDoomCompiler.CreateDoomCompiler;
    try
      c.OnExportCheck := ScriptOnExportCheck;
      Result := c.CompileDoomScript(_inp, _out);
      _msgs := '';

      for i := 0 to c.MsgCount - 1 do
      begin
        _msgs := _msgs + ' Pos: ' + IntToStrZFill(6, c.Msg[i].Pos) +
                         ' Compiler: ' + c.Msg[i].MessageToString + #13#10;
      end;
    finally
      c.Free;
    end;
  finally
  end;
end;

{$IFDEF DOOM}
function dd_compile_doom(
{$ENDIF}
{$IFDEF HERETIC}
function dd_compile_heretic(
{$ENDIF}
{$IFDEF HEXEN}
function dd_compile_hexen(
{$ENDIF}
{$IFDEF STRIFE}
function dd_compile_strife(
{$ENDIF}
  const _inp: PChar; const _inpsize: Integer;
  var _out: PChar; var _outsize: Integer;
  var _msgs: PChar; var _msgssize: Integer): Boolean; stdcall;
var
  _linp: string;
  _lout: string;
  _lmsgs: string;
  i: integer;
  _loutsize: integer;
  _lmsgssize: integer;
begin
  SetLength(_linp, _inpsize);
  for i := 0 to _inpsize - 1 do
    _linp[i + 1] := _inp[i];
  _lout := '';
  _lmsgs := '';
  Result := DD_Compile(_linp, _lout, _lmsgs);
  _loutsize := Length(_lout);
  for i := 0 to MinI(_loutsize, _outsize) - 1 do
    _out[i] := _lout[i + 1];
  _outsize := _loutsize;
  _lmsgssize := Length(_lmsgs);
  for i := 0 to MinI(_lmsgssize, _msgssize) - 1 do
    _msgs[i] := _lmsgs[i + 1];
  _msgssize := _lmsgssize;
end;

//==============================================================================
//
// DD_CopyStringToPChar
//
//==============================================================================
procedure DD_CopyStringToPChar(const inps: string; var _out: PChar; var _outsize: Integer);
var
  i: integer;
  maxsize: integer;
  reallen: integer;
  copylen: integer;
begin
  maxsize := _outsize;
  reallen := Length(inps);
  _outsize := reallen;
  if reallen > maxsize then
    copylen := maxsize
  else
    copylen := reallen;
  for i := 1 to copylen do
    _out[i - 1] := inps[i];
end;

//==============================================================================
//
// DD_CopyPCharToString
//
//==============================================================================
procedure DD_CopyPCharToString(const _inp: PChar; const _inpsize: Integer; var outstr: string);
var
  i: integer;
begin
  SetLength(outstr, _inpsize);
  for i := 0 to _inpsize - 1 do
    outstr[i + 1] := _inp[i];
end;

{$IFDEF DOOM}
procedure dd_getavailableunits_doom(
{$ENDIF}
{$IFDEF HERETIC}
procedure dd_getavailableunits_heretic(
{$ENDIF}
{$IFDEF HEXEN}
procedure dd_getavailableunits_hexen(
{$ENDIF}
{$IFDEF STRIFE}
procedure dd_getavailableunits_strife(
{$ENDIF}
  var _out: PChar; var _outsize: Integer); stdcall;
var
  unitnames: string;
begin
  try
    unitnames := (PS_ImportUnits as TStringList).Text;
    DD_CopyStringToPChar(unitnames, _out, _outsize);
  finally
  end;
end;

{$IFDEF DOOM}
procedure dd_getunitfunctions_doom(
{$ENDIF}
{$IFDEF HERETIC}
procedure dd_getunitfunctions_heretic(
{$ENDIF}
{$IFDEF HEXEN}
procedure dd_getunitfunctions_hexen(
{$ENDIF}
{$IFDEF STRIFE}
procedure dd_getunitfunctions_strife(
{$ENDIF}
  const _inp: PChar; const _inpsize: Integer;
  var _out: PChar; var _outsize: Integer); stdcall;
var
  unitname: string;
  lst: TStringList;
  funcdecls: string;
  idx: integer;
begin
  try
    DD_CopyPCharToString(_inp, _inpsize, unitname);
    lst := PS_ImportUnits as TStringList;
    idx := lst.IndexOf(unitname);
    if idx < 0 then
    begin
      unitname := strupper(unitname);
      idx := lst.IndexOf(unitname);
    end;
    if idx < 0 then
      funcdecls := ''
    else
      funcdecls := (lst.Objects[idx] as TProcedureList).GetDeclarations;
    DD_CopyStringToPChar(funcdecls, _out, _outsize);
  finally
  end;
end;

//==============================================================================
//
// IfRVariantStringValue
//
//==============================================================================
function IfRVariantStringValue(const p: PIfRVariant): string;
var
  i: integer;
begin
  if p = nil then
  begin
    Result := 'nil';
    Exit;
  end;

  case p.FType.BaseType of
    btType:
      Result := p^.ttype.OriginalName;
    {$IFNDEF PS_NOWIDESTRING}
    btWideString:
      Result := TbtWideString(p^.twidestring);
    btUnicodeString:
      Result := TbtUnicodeString(p^.twidestring);
    btWideChar:
      Result := p^.twidechar;
    {$ENDIF}
    btSingle:
      Result := FloatToStr(p^.tsingle);
    btDouble:
      Result := FloatToStr(p^.tdouble);
    btExtended:
      Result := FloatToStr(p^.textended);
    btCurrency:
      Result := FloatToStr(p^.tcurrency);
    btChar:
      Result := '#' + IntToHex(Ord(p^.tchar) , 2);
    btSet:
      begin
        Result := '[';
        for i := 1 to Length(TbtString(p^.tstring)) do
          if i = Length(TbtString(p^.tstring)) then
            Result := Result + ' ' + IntToHex(Ord(TbtString(p^.tstring)[i]), 2)
          else
            Result := Result + ' ' + IntToHex(Ord(TbtString(p^.tstring)[i]), 2) + ',';
        Result := Result + ']';
      end;
    btString:
      begin
        Result := TbtString(p^.tstring);
      end;
    btEnum:
      begin
        Result := IntToStr(p^.tu32);
      end;
    btS8:
      Result := IntToStr(p^.ts8);
    btU8:
      Result := IntToStr(p^.tu8);
    btS16:
      Result := IntToStr(p^.ts16);
    btU16:
      Result := IntToStr(p^.tu16);
    btS32:
      Result := IntToStr(p^.ts32);
    btU32:
      Result := IntToStr(p^.tu32);
    {$IFNDEF PS_NOINT64}
    bts64:
      Result := SysUtils.IntToStr(p^.ts64);
    {$ENDIF}
    btProcPtr:
      Result := IntToHex(p^.tu32, 8);
  else
    Result := '';
  end;
end;

//==============================================================================
//
// IfBaseTypeToString
//
//==============================================================================
function IfBaseTypeToString(const b: byte): string;
begin
  case b of
    btType:
      Result := 'TypeDef';
    {$IFNDEF PS_NOWIDESTRING}
    btWideString:
      Result := 'WideString';
    btUnicodeString:
      Result := 'UnicodeString';
    btWideChar:
      Result := 'WideChar';
    {$ENDIF}
    btSingle:
      Result := 'Single';
    btDouble:
      Result := 'Double';
    btExtended:
      Result := 'Extended';
    btCurrency:
      Result := 'Currency';
    btChar:
      Result := 'Char';
    btSet:
      Result := 'Set';
    btString:
      Result := 'String';
    btEnum:
      Result := 'Enum';
    btS8:
      Result := 'Signed Byte';
    btU8:
      Result := 'Unsigned Byte';
    btS16:
      Result := 'Signed Int16';
    btU16:
      Result := 'Unsigned Int16';
    btS32:
      Result := 'Signed Int32';
    btU32:
      Result := 'Unsigned Int32';
    {$IFNDEF PS_NOINT64}
    bts64:
      Result := 'Signed Int64';
    {$ENDIF}
    btProcPtr:
      Result := 'Proc Pointer';
    btClass:
      Result := 'Class';
    btArray:
      Result := 'Array';
    btStaticArray:
      Result := 'Static Array';
    btRecord:
      Result := 'Record';
    btPointer:
      Result := 'Pointer';
    btPChar:
      Result := 'PChar';
    btResourcePointer:
      Result := 'Resource Pointer';
    btVariant:
      Result := 'Variant';
    btInterface:
      Result := 'Interface';
    btNotificationVariant:
      Result := 'Notification Variant';
    btExtClass:
      Result := 'External Class';
  else
    Result := '';
  end;
end;

const
  STUB_SCRIPT_ALL = 'uses all; begin end.';

var
  cconsts: string;
  cvars: string;
  ctypes: string;
  cclasses: string;

//==============================================================================
//
// PS_ScriptOnUsesEx
//
//==============================================================================
function PS_ScriptOnUsesEx(Sender: TPSPascalCompiler; const Name: string): Boolean;
var
  i: integer;
  fconst: TPSConstant;
  fvar: TPSVar;
  ftyp: TPSType;
  basetyp: string;
begin
  Result := PS_ScriptOnUses(Sender, Name);
  cconsts := '';
  cvars := '';
  ctypes := '';
  for i := 0 to Sender.GetConstCount - 1 do
  begin
    fconst := Sender.GetConst(i);
    cconsts := cconsts + fconst.OrgName + '=' + IfRVariantStringValue(fconst.Value) + #13#10;
  end;
  for i := 0 to Sender.GetVarCount - 1 do
  begin
    fvar := Sender.GetVar(i);
    cvars := cvars + fvar.OrgName + '=' + fvar.aType.OriginalName + #13#10;
  end;
  for i := 0 to Sender.GetTypeCount - 1 do
  begin
    ftyp := Sender.GetType(i);
    if ftyp <> nil then
    begin
      if ftyp.BaseType = btTypeCopy then
        basetyp := IfBaseTypeToString(TPSTypeLink(ftyp).LinkTypeNo.BaseType)
      else
      basetyp := IfBaseTypeToString(ftyp.BaseType);
      if basetyp = '' then
        basetyp := 'type #' + IntToStr(ftyp.BaseType);
      ctypes := ctypes + ftyp.OriginalName + '=' + basetyp + #13#10;
    end;
  end;
end;

//==============================================================================
//
// PS_ScriptOnUsesExClasses
//
//==============================================================================
function PS_ScriptOnUsesExClasses(Sender: TPSPascalCompiler; const Name: string): Boolean;
var
  i, j: integer;
  fclass: TPSCompileTimeClass;
  ficlass: TPSCompileTimeClass;
  tmp: string;
  decl: string;
  c: TPSDelphiClassItem;
  s1, sx, s2, s3: string;
begin
  Result := PS_ScriptOnUses(Sender, Name);
  cclasses := '';
  for i := 0 to Sender.Classes.Count - 1 do
  begin
    fclass := Sender.Classes[i];
    ficlass := fclass.ClassInheritsFrom;
    if ficlass <> nil then
      cclasses := cclasses + fclass.RTLClassName + '=Inherits from ' + ficlass.RTLClassName + ';'
    else
      cclasses := cclasses + fclass.RTLClassName + '=Inherits from TObject;';
    while fclass <> nil do
    begin
      for j := fclass.Count - 1 downto 0 do
      begin
        c := fclass.Items[j];
        if c <> nil then
        begin
          if c is TPSDelphiClassItemProperty then
          begin
            tmp := 'property ';
            decl := Trim(c.RTLDeclaration);
            decl := StringReplace(decl, '  ', ' ', [rfReplaceAll]);
            splitstring(decl, s1, sx, ' ');
            splitstring(sx, s2, s3, ' ');
            s3 := Trim(s3);
            if s3 <> '' then
              decl := s1 + '[' + s3 + ']: ' + s2
            else
              decl := s1 + ': ' + s2;
          end
          else
          begin
            tmp := '';
            decl := Trim(c.RTLDeclaration);
          end;
          tmp := tmp + decl;
          if Length(tmp) > 0 then
          begin
            if tmp[Length(tmp)] <> ';' then
              tmp := tmp + ';';
            cclasses := cclasses + '|' + tmp;
          end;
        end;
      end;
      fclass := fclass.ClassInheritsFrom;
    end;
    cclasses := cclasses + #13#10;
  end;
end;

{$IFDEF DOOM}
procedure dd_getconstants_doom(
{$ENDIF}
{$IFDEF HERETIC}
procedure dd_getconstants_heretic(
{$ENDIF}
{$IFDEF HEXEN}
procedure dd_getconstants_hexen(
{$ENDIF}
{$IFDEF STRIFE}
procedure dd_getconstants_strife(
{$ENDIF}
  var _out: PChar; var _outsize: Integer); stdcall;
var
  pcode: string;
  c: TDoomCompiler;
begin
  try
    c := TDoomCompiler.CreateDoomCompiler;
    c.OnUses := PS_ScriptOnUsesEx;
    try
      if c.CompileDoomScript(STUB_SCRIPT_ALL, pcode) then
        DD_CopyStringToPChar(cconsts, _out, _outsize);
    finally
      c.Free;
    end;
  finally
  end;
end;

{$IFDEF DOOM}
procedure dd_getvariables_doom(
{$ENDIF}
{$IFDEF HERETIC}
procedure dd_getvariables_heretic(
{$ENDIF}
{$IFDEF HEXEN}
procedure dd_getvariables_hexen(
{$ENDIF}
{$IFDEF STRIFE}
procedure dd_getvariables_strife(
{$ENDIF}
  var _out: PChar; var _outsize: Integer); stdcall;
var
  pcode: string;
  c: TDoomCompiler;
begin
  try
    c := TDoomCompiler.CreateDoomCompiler;
    c.OnUses := PS_ScriptOnUsesEx;
    try
      if c.CompileDoomScript(STUB_SCRIPT_ALL, pcode) then
        DD_CopyStringToPChar(cvars, _out, _outsize);
    finally
      c.Free;
    end;
  finally
  end;
end;

{$IFDEF DOOM}
procedure dd_gettypes_doom(
{$ENDIF}
{$IFDEF HERETIC}
procedure dd_gettypes_heretic(
{$ENDIF}
{$IFDEF HEXEN}
procedure dd_gettypes_hexen(
{$ENDIF}
{$IFDEF STRIFE}
procedure dd_gettypes_strife(
{$ENDIF}
  var _out: PChar; var _outsize: Integer); stdcall;
var
  pcode: string;
  c: TDoomCompiler;
begin
  try
    c := TDoomCompiler.CreateDoomCompiler;
    c.OnUses := PS_ScriptOnUsesEx;
    try
      if c.CompileDoomScript(STUB_SCRIPT_ALL, pcode) then
        DD_CopyStringToPChar(ctypes, _out, _outsize);
    finally
      c.Free;
    end;
  finally
  end;
end;

{$IFDEF DOOM}
procedure dd_getclasses_doom(
{$ENDIF}
{$IFDEF HERETIC}
procedure dd_getclasses_heretic(
{$ENDIF}
{$IFDEF HEXEN}
procedure dd_getclasses_hexen(
{$ENDIF}
{$IFDEF STRIFE}
procedure dd_getclasses_strife(
{$ENDIF}
  var _out: PChar; var _outsize: Integer); stdcall;
var
  pcode: string;
  c: TDoomCompiler;
begin
  try
    c := TDoomCompiler.CreateDoomCompiler;
    c.OnUses := PS_ScriptOnUsesExClasses;
    try
      if c.CompileDoomScript(STUB_SCRIPT_ALL, pcode) then
        DD_CopyStringToPChar(cclasses, _out, _outsize);
    finally
      c.Free;
    end;
  finally
  end;
end;

{$IFDEF DOOM}
procedure dd_getdisassembly_doom(
{$ENDIF}
{$IFDEF HERETIC}
procedure dd_getdisassembly_heretic(
{$ENDIF}
{$IFDEF HEXEN}
procedure dd_getdisassembly_hexen(
{$ENDIF}
{$IFDEF STRIFE}
procedure dd_getdisassembly_strife(
{$ENDIF}
  const _inp: PChar; const _inpsize: Integer;
  var _out: PChar; var _outsize: Integer); stdcall;
var
  pcode: string;
  i: integer;
  disasm: string;
begin
  try
    SetLength(pcode, _inpsize);
    for i := 0 to _inpsize - 1 do
      pcode[i + 1] := _inp[i];
    if not IFPS3DataToText(pcode, disasm) then
    begin
      _outsize := 0;
      Exit;
    end;
    DD_CopyStringToPChar(disasm, _out, _outsize);
  finally
  end;
end;

{$IFDEF DOOM}
procedure dd_getevents_doom(
{$ENDIF}
{$IFDEF HERETIC}
procedure dd_getevents_heretic(
{$ENDIF}
{$IFDEF HEXEN}
procedure dd_getevents_hexen(
{$ENDIF}
{$IFDEF STRIFE}
procedure dd_getevents_strife(
{$ENDIF}
  var _out: PChar; var _outsize: Integer); stdcall;
var
  estr: string;
  i: integer;
begin
  estr := '';
  for i := Low(EventExportedProcs) to High(EventExportedProcs) do
    estr := estr + EventExportedProcs[i].Name + '|' + EventExportedProcs[i].Template + #13#10;
  DD_CopyStringToPChar(estr, _out, _outsize);
end;

{$IFDEF DOOM}
procedure dd_getactordeffunctions_doom(
{$ENDIF}
{$IFDEF HERETIC}
procedure dd_getactordeffunctions_heretic(
{$ENDIF}
{$IFDEF HEXEN}
procedure dd_getactordeffunctions_hexen(
{$ENDIF}
{$IFDEF STRIFE}
procedure dd_getactordeffunctions_strife(
{$ENDIF}
  var _out: PChar; var _outsize: Integer); stdcall;
var
  afstr: string;
  i: integer;
begin
  try
    afstr := '';
    for i := 0 to DEHNUMACTIONS - 1 do
      afstr := afstr + deh_actions[i].decl + #13#10;
    DD_CopyStringToPChar(afstr, _out, _outsize);
  finally
  end;
end;

{$IFDEF DOOM}
procedure dd_getmobjinfocsv_doom(
{$ENDIF}
{$IFDEF HERETIC}
procedure dd_getmobjinfocsv_heretic(
{$ENDIF}
{$IFDEF HEXEN}
procedure dd_getmobjinfocsv_hexen(
{$ENDIF}
{$IFDEF STRIFE}
procedure dd_getmobjinfocsv_strife(
{$ENDIF}
  var _out: PChar; var _outsize: Integer); stdcall;
var
  csvstr: string;
  lst: TDStringList;
begin
  try
    lst := DEH_MobjInfoCSV;
    csvstr := lst.Text;
    lst.Free;
    DD_CopyStringToPChar(csvstr, _out, _outsize);
  finally
  end;
end;

{$IFDEF DOOM}
procedure dd_getstatescsv_doom(
{$ENDIF}
{$IFDEF HERETIC}
procedure dd_getstatescsv_heretic(
{$ENDIF}
{$IFDEF HEXEN}
procedure dd_getstatescsv_hexen(
{$ENDIF}
{$IFDEF STRIFE}
procedure dd_getstatescsv_strife(
{$ENDIF}
  var _out: PChar; var _outsize: Integer); stdcall;
var
  csvstr: string;
  lst: TDStringList;
begin
  try
    lst := DEH_StatesCSV;
    csvstr := lst.Text;
    lst.Free;
    DD_CopyStringToPChar(csvstr, _out, _outsize);
  finally
  end;
end;

{$IFDEF DOOM}
procedure dd_getspritescsv_doom(
{$ENDIF}
{$IFDEF HERETIC}
procedure dd_getspritescsv_heretic(
{$ENDIF}
{$IFDEF HEXEN}
procedure dd_getspritescsv_hexen(
{$ENDIF}
{$IFDEF STRIFE}
procedure dd_getspritescsv_strife(
{$ENDIF}
  var _out: PChar; var _outsize: Integer); stdcall;
var
  csvstr: string;
  lst: TDStringList;
begin
  try
    lst := DEH_SpritesCSV;
    csvstr := lst.Text;
    lst.Free;
    DD_CopyStringToPChar(csvstr, _out, _outsize);
  finally
  end;
end;

{$IFDEF DOOM}
procedure dd_getactordef_doom(
{$ENDIF}
{$IFDEF HERETIC}
procedure dd_getactordef_heretic(
{$ENDIF}
{$IFDEF HEXEN}
procedure dd_getactordef_hexen(
{$ENDIF}
{$IFDEF STRIFE}
procedure dd_getactordef_strife(
{$ENDIF}
  var m: integer; var _out: PChar; var _outsize: Integer); stdcall;
var
  actorstr: string;
begin
  try
    if IsIntegerInRange(m, 0, Ord(DO_NUMMOBJTYPES) - 1) then
    begin
      actorstr := SC_GetActordefDeclaration(@mobjinfo[m]);
      DD_CopyStringToPChar(actorstr, _out, _outsize);
    end
    else
      _outsize := 0;
  finally
  end;
end;

initialization
  DD_InitDoomEngine;

finalization
  DD_ShutDownDoomEngine;

end.

