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
// DESCRIPTION:
//  Pascal Script wrapper.
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit ps_main;

interface

uses
  p_mobj_h,
  ps_compiler,
  ps_runtime;

type
  psscripttype_t = (pst_normal, pst_global, pst_none);

//==============================================================================
//
// PS_Init
//
//==============================================================================
procedure PS_Init;

//==============================================================================
//
// PS_ShutDown
//
//==============================================================================
procedure PS_ShutDown;

//==============================================================================
//
// PS_AddSourceScript
//
//==============================================================================
procedure PS_AddSourceScript(const sname: string; const sc: string;
  const typ: psscripttype_t);

//==============================================================================
//
// PS_AddCompiledScript
//
//==============================================================================
procedure PS_AddCompiledScript(const sname: string; const sc: string);

//==============================================================================
//
// PS_CompileAllScripts
//
//==============================================================================
procedure PS_CompileAllScripts;

//==============================================================================
//
// PS_NewWorld
//
//==============================================================================
procedure PS_NewWorld;

//==============================================================================
//
// PS_NewMap
//
//==============================================================================
procedure PS_NewMap;

//==============================================================================
//
// PS_LinkScriptEvents
//
//==============================================================================
procedure PS_LinkScriptEvents(const mapname: string);

//==============================================================================
// PS_EventActorDied
//
// Events
// Must be in sync with ScriptOnExportCheck and TScriptEvents class
//
//==============================================================================
procedure PS_EventActorDied(actor: Pmobj_t; killer: Pmobj_t);

//==============================================================================
//
// PS_EventPlayerDied
//
//==============================================================================
procedure PS_EventPlayerDied(playerNO: Integer; killer: Pmobj_t);

//==============================================================================
//
// PS_EventPlayerEnter
//
//==============================================================================
procedure PS_EventPlayerEnter(playerNO: Integer);

//==============================================================================
//
// PS_EventCrossLine
//
//==============================================================================
procedure PS_EventCrossLine(actor: Pmobj_t; line: Integer; oldside: Integer);

//==============================================================================
//
// PS_EventShootLine
//
//==============================================================================
procedure PS_EventShootLine(actor: Pmobj_t; line: Integer; side: Integer);

//==============================================================================
//
// PS_EventUseLine
//
//==============================================================================
procedure PS_EventUseLine(actor: Pmobj_t; line: Integer; side: Integer);

//==============================================================================
//
// PS_EventTick
//
//==============================================================================
procedure PS_EventTick(tick: integer);

//==============================================================================
//
// PS_EventTimerEverySecond
//
//==============================================================================
procedure PS_EventTimerEverySecond(second: integer);

//==============================================================================
//
// PS_EventTimerEveryMinute
//
//==============================================================================
procedure PS_EventTimerEveryMinute(minute: integer);

//==============================================================================
//
// PS_EventMapStart
//
//==============================================================================
procedure PS_EventMapStart;

//-------------------- Map Script Serialization --------------------------------

//==============================================================================
//
// PS_MapScriptSaveToFile
//
//==============================================================================
function PS_MapScriptSaveToFile(const fname: string): boolean;

//==============================================================================
//
// PS_MapScriptSaveSize
//
//==============================================================================
function PS_MapScriptSaveSize: integer;

//==============================================================================
//
// PS_MapScriptAppendToFile
//
//==============================================================================
function PS_MapScriptAppendToFile(const fname: string): boolean;

//==============================================================================
//
// PS_MapScriptLoadFromFile
//
//==============================================================================
function PS_MapScriptLoadFromFile(const fname: string): boolean;

//==============================================================================
//
// PS_MapScriptLoadFromFilePos
//
//==============================================================================
function PS_MapScriptLoadFromFilePos(const fname: string; var position: integer): boolean;

//==============================================================================
//
// A_RunScript
//
//==============================================================================
procedure A_RunScript(actor: Pmobj_t);

//==============================================================================
//
// ScriptOnExportCheck
//
//==============================================================================
function ScriptOnExportCheck(Sender: TPSPascalCompiler; Proc: TPSInternalProcedure; const ProcDecl: AnsiString): Boolean;

implementation

uses
  d_delphi,
  c_cmds,
  d_main,
  d_player,
  i_system,
  sc_params,
  p_common,
  p_setup,
  ps_import,
  ps_events,
  ps_dll,
  ps_serializer,
  psi_game,
  psi_globals,
  psi_overlay,
  uPSR_dll,
  w_folders,
  w_pak,
  ps_utils;

type
  psscriptitem_t = record
    exec: TDoomExec;
    id: TString;
    source: TString;
    data: TString;
    scripttype: psscripttype_t;
    valid: Boolean;
  end;
  Ppsscriptitem_t = ^psscriptitem_t;
  psscriptitem_tArray = array[0..$FFF] of psscriptitem_t;
  Ppsscriptitem_tArray = ^psscriptitem_tArray;

  TPSExecManager = class
  private
    fList: Ppsscriptitem_tArray;
    fNumItems: integer;
    compiler: TDoomCompiler;
    importer: TPSDoomRuntimeClassImporter;
    globalexec: TDoomExec;
    feventsexec: TDoomExec;
    fevents: TScriptEvents;
    globalindex: integer;
    fGlobalInterface: string;
    fGlobalImplementation: string;
    fGlobalInitialization: string;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function IndexOf(const id: string): integer;
    function AddNormalSourceFile(const id: string; const value: string): integer; virtual;
    function AddGlobalSourceFile(const id: string; const value: string): integer; virtual;
    function AddSourceFile(const id: string; const value: string;
      const typ: psscripttype_t): integer; virtual;
    function AddCompiledFile(const id: string; const value: string): integer; virtual;
    procedure CompileScript(const id: Integer);
    procedure CompileAllScripts;
    function RunScript(actor: Pmobj_t; const scid: integer): boolean;
    procedure LinkScriptEvents(const mapname: string);
    property Count: integer read fNumItems;
    property Events: TScriptEvents read fevents;
    property EventsExec: TDoomExec read feventsexec;
  end;

//==============================================================================
//
// ScriptOnExportCheck
//
//==============================================================================
function ScriptOnExportCheck(Sender: TPSPascalCompiler; Proc: TPSInternalProcedure; const ProcDecl: AnsiString): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := Low(EventExportedProcs) to High(EventExportedProcs) do
    if (Proc.Name = EventExportedProcs[I].Name) then
      if not ExportCheck(Sender, Proc,
        Slice(EventExportedProcs[I].Typ, EventExportedProcs[I].ParamCount + 1),
        Slice(EventExportedProcs[I].Dir, EventExportedProcs[I].ParamCount)) then
      begin
        //  Parameter missmatch
        Sender.MakeErrorEx(EventExportedProcs[I].Name, ecInvalidEventParameters, '',
          Proc.DeclarePos, Proc.DeclareRow, Proc.DeclareCol);
        Result := False;
        Exit;
      end;
end;

//==============================================================================
//
// TPSExecManager.Create
//
//==============================================================================
constructor TPSExecManager.Create;
begin
  fList := nil;
  fNumItems := 0;
  globalindex := -1;
  fGlobalInterface := '';
  fGlobalImplementation := '';
  fGlobalInitialization := '';
  compiler := TDoomCompiler.CreateDoomCompiler;
  compiler.OnExportCheck := ScriptOnExportCheck;
  importer := TPSDoomRuntimeClassImporter.Create;
  globalexec := nil;
  feventsexec := TDoomExec.CreateDoomExec(importer);
  fevents := TScriptEvents.Create(feventsexec);
  AddNormalSourceFile('GLOBAL', '');
end;

//==============================================================================
//
// TPSExecManager.Destroy
//
//==============================================================================
destructor TPSExecManager.Destroy;
var
  i: integer;
begin
  if fNumItems > 0 then
  begin
    for i := 0 to fNumItems - 1 do
    begin
      fList[i].source.Free;
      fList[i].data.Free;
      fList[i].id.Free;
      fList[i].exec.Free;
    end;
    memfree(pointer(fList), fNumItems * SizeOf(psscriptitem_t));
  end;
  compiler.Free;
  importer.Free;
  feventsexec.Free;
  fevents.Free;
end;

//==============================================================================
//
// TPSExecManager.AddNormalSourceFile
//
//==============================================================================
function TPSExecManager.AddNormalSourceFile(const id: string; const value: string): integer;
var
  uid: string;
begin
  uid := strupper(id);
  Result := IndexOf(uid);
  if Result <> - 1 then
  begin
    I_Warning('TPSExecManager.AddNormalSourceFile(): Script id="' + uid + '" already exists.'#13#10);
    exit;
  end;

  realloc(pointer(fList), fNumItems * SizeOf(psscriptitem_t), (fNumItems + 1) * SizeOf(psscriptitem_t));
  fList[fNumItems].source := TString.Create(value);
  fList[fNumItems].exec := nil;
  fList[fNumItems].id := TString.Create(uid);
  fList[fNumItems].data := nil;
  fList[fNumItems].scripttype := pst_normal;
  fList[fNumItems].valid := False;
  Result := fNumItems;

  inc(fNumItems);
end;

//==============================================================================
//
// TPSExecManager.AddGlobalSourceFile
//
//==============================================================================
function TPSExecManager.AddGlobalSourceFile(const id: string; const value: string): integer;
var
  slist: TDStringList;
  i: integer;
  section: integer;
  s, token: string;
begin
  slist := TDStringList.Create;
  slist.Text := value;

  // JVAL: Split the input source to sections
  fGlobalInterface :=
    fGlobalInterface +
      '// --- "' + id + '" Interface --- '#13#10;
  fGlobalImplementation :=
    fGlobalImplementation +
      '// --- "' + id + '" Implementation --- '#13#10;
  fGlobalInitialization :=
    fGlobalInitialization +
      '// --- "' + id + '" Initialization --- '#13#10;

  section := 0; // Means "interface"
  for i := 0 to slist.Count - 1 do
  begin
    s := slist.Strings[i];
    token := strupper(strtrim(s));
    if token = 'INTERFACE' then
      section := 0
    else if token = 'IMPLEMENTATION' then
      section := 1
    else if token = 'INITIALIZATION' then
      section := 2
    else
    begin
      case section of
        0:
          begin
            fGlobalInterface := fGlobalInterface + s + #13#10;
          end;
        1:
          begin
            fGlobalImplementation := fGlobalImplementation + s + #13#10;
          end;
        2:
          begin
            if strupper(strtrim(s)) <> 'END.' then
              fGlobalInitialization := fGlobalInitialization + s + #13#10;
          end;
      end;
    end;
  end;
  slist.Free;
  Result := IndexOf('GLOBAL');
end;

//==============================================================================
//
// TPSExecManager.AddSourceFile
//
//==============================================================================
function TPSExecManager.AddSourceFile(const id: string; const value: string;
      const typ: psscripttype_t): integer;
begin
  if typ = pst_global then
    result := AddGlobalSourceFile(id, value)
  else if typ = pst_normal then
    result := AddNormalSourceFile(id, value)
  else
  begin
    I_Warning('TPSExecManager.AddSourceFile(): Internal error! Unidentified script type.'#13#10);
    Result := -1;
  end;
end;

//==============================================================================
//
// TPSExecManager.AddCompiledFile
//
//==============================================================================
function TPSExecManager.AddCompiledFile(const id: string; const value: string): integer;
var
  uid: string;
begin
  uid := strupper(id);
  Result := IndexOf(uid);
  if Result <> - 1 then
  begin
    I_Warning('TPSExecManager.AddCompiledFile(): Script id="' + uid + '" already exists.'#13#10);
    exit;
  end;

  realloc(pointer(fList), fNumItems * SizeOf(psscriptitem_t), (fNumItems + 1) * SizeOf(psscriptitem_t));
  fList[fNumItems].source := nil;
  fList[fNumItems].exec := nil;
  fList[fNumItems].id := TString.Create(uid);
  fList[fNumItems].data := TString.Create(value);
  fList[fNumItems].scripttype := pst_normal;
  fList[fNumItems].valid := False;
  Result := fNumItems;

  inc(fNumItems);
end;

//==============================================================================
//
// TPSExecManager.CompileScript
//
//==============================================================================
procedure TPSExecManager.CompileScript(const id: Integer);
var
  clist: TDStringList;
  i: integer;
  data: string;
  DE: TDoomExec;
begin
  if fList[id].valid then
    exit;

  if flist[id].source = nil then
  begin
    if fList[id].exec = nil then
      fList[id].exec := TDoomExec.CreateDoomExec(importer);
    DE := fList[id].exec;
    fList[id].valid := DE.LoadData(fList[id].data.str); // Load the data from the Data string.
    if not fList[id].valid then
    begin
      { For some reason the script could not be loaded. This is usually the case when a
        library that has been used at compile time isn't registered at runtime. }
      // JVAL: This could happen if script was compiled with previous version of pscomp
      I_Warning('TPSExecManager.Add(): Internal error! Can not load script assembly.'#13#10);
      I_Warning(TIFErrorToString(DE.ExceptionCode, DE.ExceptionString) + #13#10);
    end;
    exit;
  end;

  if compiler.CompileDoomScript(flist[id].source.str, data) then
  begin
    fList[id].data := TString.Create(data);
    if fList[id].exec = nil then
      fList[id].exec := TDoomExec.CreateDoomExec(importer);
    DE := fList[id].exec;
    fList[id].valid := DE.LoadData(data); // Load the data from the Data string.
    if not fList[id].valid then
    begin
      { For some reason the script could not be loaded. This is usually the case when a
        library that has been used at compile time isn't registered at runtime. }
      // JVAL: This could happen if script was compiled with previous version of pscomp
      I_Warning('TPSExecManager.Add(): Internal error! Can not load script assembly.'#13#10);
      I_Warning(TIFErrorToString(DE.ExceptionCode, DE.ExceptionString) + #13#10);
    end;
    if devparm then
    begin
      clist := TDStringList.Create;
      clist.Text := flist[id].source.str;
      for i := 0 to clist.Count - 1 do
        printf(IntToStrZfill(6, i + 1) + ': ' + clist.Strings[i] + #13#10);
      clist.Free;
    end;
  end
  else
  begin
    I_Warning('TPSExecManager.Add(): Can not compile script:'#13#10);
    clist := TDStringList.Create;
    clist.Text := flist[id].source.str;
    for i := 0 to clist.Count - 1 do
      I_Warning(IntToStrZfill(6, i + 1) + ': ' + clist.Strings[i] + #13#10);
    for i := 0 to compiler.MsgCount - 1 do
    begin
      I_Warning('//---------------------------------------------------------'#13#10);
      I_Warning(' Pos: ' + IntToStrZFill(6, compiler.Msg[i].Pos) +
                ' Compiler: ' + compiler.Msg[i].MessageToString + #13#10);
      I_Warning(' <<' + Copy(flist[id].source.str, compiler.Msg[i].Pos, 80) + '>>'#13#10);
      I_Warning('//---------------------------------------------------------'#13#10);
    end;
    clist.Free;
  end;
end;

//==============================================================================
//
// TPSExecManager.CompileAllScripts
//
//==============================================================================
procedure TPSExecManager.CompileAllScripts;
var
  i: integer;
  globalsource: string;
begin
  globalindex := IndexOf('GLOBAL');
  if globalindex < 0 then // JVAL: Should not happen
    I_Warning('TPSExecManager.CompileAllScripts(): GLOBAL Script not found.'#13#10)
  else
  begin
    globalsource :=
      'uses all;'#13#10 +
      fGlobalInterface +
      fGlobalImplementation +
      'begin'#13#10 +
      fGlobalInitialization +
      'end.'#13#10;
    fList[globalindex].source.str := globalsource;
    fList[globalindex].exec := globalexec;
    fList[globalindex].data := nil;
    fList[globalindex].scripttype := pst_global;
    fList[globalindex].valid := False;
  end;

  for i := 0 to fNumItems - 1 do
  begin
    CompileScript(i);
    if fList[i].valid then
      printf(' "%s"...success'#13#10, [fList[i].id.str])
    else
    begin
      I_Warning(' "%s"...failed'#13#10, [fList[i].id.str]);
      I_Warning('//---------------------------------------------------------'#13#10);
      I_Warning(#13#10);
    end;
  end;

  globalexec := fList[globalindex].exec;
end;

//==============================================================================
//
// TPSExecManager.RunScript
//
//==============================================================================
function TPSExecManager.RunScript(actor: Pmobj_t; const scid: integer): boolean;
var
  DE: TDoomExec;
begin
  if scid > - 1 then
    if fList[scid].valid then
    begin
      ps_currentactor := actor;
      DE := fList[scid].exec;
      result := DE.RunScript;
      if DE.ExceptionCode <> erNoError then
        I_Warning('[Runtime Error] : ' +
          TIFErrorToString(DE.ExceptionCode, DE.ExceptionString) +
          ' in ' +
          itoa(DE.ExceptionProcNo) +
          ' at ' +
          itoa(DE.ExceptionPos));
      ps_currentactor := nil;
      exit;
    end;

  result := false;
end;

//==============================================================================
//
// TPSExecManager.LinkScriptEvents
//
//==============================================================================
procedure TPSExecManager.LinkScriptEvents(const mapname: string);
var
  idx: Integer;

  function ValidateVarType(aType: TPSTypeRec): string;
  var
    I: Integer;
  begin
    //Check against our set of allowed types
    if not (aType.BaseType in VALID_GLOBAL_VAR_TYPES) then
    begin
      sprintf(Result, 'TPSExecManager.LinkScriptEvents(): Unsupported global variable type %d (%s)'#13#10,
        [aType.BaseType, aType.ExportName]);
      Exit;
    end;

    //Check elements of arrays/records are valid too
    case aType.BaseType of
      btArray,
      btStaticArray:
        Result := ValidateVarType(TPSTypeRec_Array(aType).ArrayType);
      btRecord:
        begin
          Result := '';
          for I := 0 to TPSTypeRec_Record(aType).FieldTypes.Count - 1 do
            Result := Result + ValidateVarType(TPSTypeRec_Record(aType).FieldTypes[I]);
        end;
    end;
  end;

var
  i, j: integer;
  V: PIFVariant;
  errstr: string;
  errlst: TDStringList;
  data: string;
  uS: string;
begin
  fevents.Clear;
  idx := IndexOf(mapname);
  if idx < 0 then
  begin
    data := PAK_ReadFileAsString(mapname + '.ddout');
    if data <> '' then
      idx := AddCompiledFile(mapname, data);
    if idx < 0 then
    begin
      data := PAK_ReadFileAsString(mapname + '.ddscript');
      if data <> '' then
        idx := AddNormalSourceFile(mapname, data);
      if idx < 0 then
      begin
        feventsexec.Clear;
        Exit;
      end;
      CompileScript(idx);
    end;
  end;

  if not fList[idx].valid then
    Exit;

  if not feventsexec.LoadData(fList[idx].data.str) then
  begin
    I_Warning('TPSExecManager.LinkScriptEvents(): Internal error! Can not load script assembly.'#13#10);
    I_Warning(TIFErrorToString(feventsexec.ExceptionCode, feventsexec.ExceptionString) + #13#10);
    feventsexec.Clear;
    Exit;
  end;

  errstr := '';
  for i := 0 to feventsexec.GetVarCount - 1 do
  begin
    V := feventsexec.GetVarNo(I);
    uS := strupper(V.FType.ExportName);
    if (uS = '!TACTORS') or
       (uS = '!TMAPFLOATS') or
       (uS = '!TMAPINTEGERS') or
       (uS = '!TMAPSTRINGS') or
       (uS = '!TWORLDFLOATS') or
       (uS = '!TWORLDINTEGERS') or
       (uS = '!TWORLDSTRINGS') or
       (uS = '!TVERTEXES') or
       (uS = '!TSIDES') or
       (uS = '!TLINES') or
       (uS = '!TSECTORS') or
       (uS = '!TOVERLAY') or
       (uS = '!TMOBJINFO') then
      Continue;

    errstr := ValidateVarType(V.FType);
    if errstr <> '' then
    begin
      errlst := TDStringList.Create;
      errlst.Text := errstr;
      for j := 0 to errlst.Count - 1 do
        I_Warning(errlst.Strings[j]);
      errlst.Free;
      //Don't allow the script to run
      feventsexec.Clear;
      Exit;
    end;
  end;
  fevents.LinkEvents;
end;

//==============================================================================
//
// TPSExecManager.IndexOf
//
//==============================================================================
function TPSExecManager.IndexOf(const id: string): integer;
var
  check: string;
  i: integer;
begin
  check := strupper(id);
  for i := 0 to fNumItems - 1 do
    if flist[i].id.str = check then
    begin
      result := i;
      exit;
    end;
  result := -1;
end;

var
  psmanager: TPSExecManager = nil;

//==============================================================================
//
// CmdCompileScript
//
//==============================================================================
procedure CmdCompileScript(const src, dest: string);
var
  data: string;
  f: TFile;
  i: integer;
  clist: TDStringList;
begin
  if src = '' then
  begin
    printf('Please specify the source file.'#13#10);
    Exit;
  end;
  if dest = '' then
  begin
    printf('Please specify the output file.'#13#10);
    Exit;
  end;
  if strupper(fexpand(src)) = strupper(fexpand(dest)) then
  begin
    printf('Source file and destination file can not be the same.'#13#10);
    Exit;
  end;
  if not fexists(src) then
  begin
    I_Warning('CmdCompileScript(): Source file %s not found.'#13#10, [src]);
    Exit;
  end;

  if psmanager = nil then
  begin
    printf('Compiler not initialized.'#13#10);
    Exit;
  end;

  clist := TDStringList.Create;
  clist.LoadFromFile(src);
  if psmanager.compiler.CompileDoomScript(clist.Text, data) then
  begin
    f := TFile.Create(dest, fCreate);
    for i := 1 to Length(data) do
      f.Write(Byte(data[i]), SizeOf(byte));
    f.Free;
    printf(' "%s"...success'#13#10, [src])
  end
  else
  begin
    I_Warning('pscomp: Can not compile script:'#13#10);
    for i := 0 to clist.Count - 1 do
    begin
      I_Warning(IntToStrZfill(6, i + 1) + ': ' + clist.Strings[i] + #13#10);
    end;
    for i := 0 to psmanager.compiler.MsgCount - 1 do
    begin
      I_Warning('//---------------------------------------------------------'#13#10);
      I_Warning(' Pos: ' + IntToStrZFill(6, psmanager.compiler.Msg[i].Pos) +
                ' Compiler: ' + psmanager.compiler.Msg[i].MessageToString + #13#10);
      I_Warning(' <<' + Copy(clist.Text, psmanager.compiler.Msg[i].Pos, 80) + '>>'#13#10);
      I_Warning('//---------------------------------------------------------'#13#10);
    end;
    I_Warning(' "%s"...failed'#13#10, [src]);
    I_Warning('//---------------------------------------------------------'#13#10);
  end;
  clist.Free;
end;

//==============================================================================
//
// PS_Init
//
//==============================================================================
procedure PS_Init;
begin
  PS_InitDLLLoader;
  PS_InitProcLists;
  PS_InitGlobals;
  PS_InitGameImport;
  psmanager := TPSExecManager.Create;
  C_AddCmd('compile, compilescript, pscomp', @CmdCompileScript);
end;

//==============================================================================
//
// PS_ShutDown
//
//==============================================================================
procedure PS_ShutDown;
begin
  psmanager.Free;
  psmanager := nil;
  PS_ShutDownGameImport;
  PS_ShutDownOverlay;
  PS_ShutDownGlobals;
  PS_ShutDownProcLists;
  PS_ShutDownDLLLoader;
end;

//==============================================================================
//
// PS_AddSourceScript
//
//==============================================================================
procedure PS_AddSourceScript(const sname: string; const sc: string;
  const typ: psscripttype_t);
begin
  printf('PS_AddSourceScript(): Adding script "%s"'#13#10, [sname]);
  psmanager.AddSourceFile(sname, sc, typ);
end;

//==============================================================================
//
// PS_AddCompiledScript
//
//==============================================================================
procedure PS_AddCompiledScript(const sname: string; const sc: string);
var
  strm: TPakStream;
  data: string;
  b: PByteArray;
  size: integer;
  i: integer;
begin
  data := '';
  strm := TPakStream.Create(sc, pm_short, '', FOLDER_SCRIPTS);
  if strm.IOResult = 0 then
  begin
    printf('PS_AddCompiledScript(): Adding script "%s" (file "%s")'#13#10, [sname, sc]);
    size := strm.Size;
    b := malloc(size);
    strm.Seek(0, sFromBeginning);
    strm.Read(b^, size);
    for i := 0 to size - 1 do
      data := data + Chr(b[i]);
    memfree(pointer(b), size);
  end;
  strm.Free;
  psmanager.AddCompiledFile(sname, data);
end;

//==============================================================================
//
// PS_CompileAllScripts
//
//==============================================================================
procedure PS_CompileAllScripts;
begin
  PS_InitOverlay;
  psmanager.CompileAllScripts;
end;

//==============================================================================
//
// PS_DoRunScript
//
//==============================================================================
procedure PS_DoRunScript(actor: Pmobj_t; const scid: integer);
begin
  psmanager.RunScript(actor, scid);
end;

//==============================================================================
//
// PS_GetScriptIDFromName
//
//==============================================================================
function PS_GetScriptIDFromName(const scname: string): integer;
begin
  result := psmanager.IndexOf(scname);
end;

//==============================================================================
//
// PS_NewWorld
//
//==============================================================================
procedure PS_NewWorld;
begin
  worldvars.Clear;
  mapvars.Clear;
  overlay.Clear;
  psmanager.Events.Clear;
end;

//==============================================================================
//
// PS_NewMap
//
//==============================================================================
procedure PS_NewMap;
begin
  mapvars.Clear;
  overlay.Clear;
  psmanager.Events.Clear;
end;

//==============================================================================
//
// PS_LinkScriptEvents
//
//==============================================================================
procedure PS_LinkScriptEvents(const mapname: string);
begin
  psmanager.LinkScriptEvents(mapname);
end;

// Events

//==============================================================================
//
// PS_EventActorDied
//
//==============================================================================
procedure PS_EventActorDied(actor: Pmobj_t; killer: Pmobj_t);
begin
  ps_currentactor := actor;
  psmanager.Events.ProcActorDied(PS_KeyFromMobj(actor), PS_KeyFromMobj(killer));
  ps_currentactor := nil;
end;

//==============================================================================
//
// PS_EventPlayerEnter
//
//==============================================================================
procedure PS_EventPlayerEnter(playerNO: Integer);
begin
  ps_currentactor := players[playerNO].mo;
  psmanager.Events.ProcPlayerEnter(playerNO);
  ps_currentactor := nil;
end;

//==============================================================================
//
// PS_EventPlayerDied
//
//==============================================================================
procedure PS_EventPlayerDied(playerNO: Integer; killer: Pmobj_t);
begin
  ps_currentactor := players[playerNO].mo;
  psmanager.Events.ProcPlayerDied(playerNO, PS_KeyFromMobj(killer));
  ps_currentactor := nil;
end;

//==============================================================================
//
// PS_EventCrossLine
//
//==============================================================================
procedure PS_EventCrossLine(actor: Pmobj_t; line: Integer; oldside: Integer);
begin
  ps_currentactor := actor;
  psmanager.Events.ProcCrossLine(PS_KeyFromMobj(actor), line, oldside);
  ps_currentactor := nil;
end;

//==============================================================================
//
// PS_EventShootLine
//
//==============================================================================
procedure PS_EventShootLine(actor: Pmobj_t; line: Integer; side: Integer);
begin
  ps_currentactor := actor;
  psmanager.Events.ProcShootLine(PS_KeyFromMobj(actor), line, side);
  ps_currentactor := nil;
end;

//==============================================================================
//
// PS_EventUseLine
//
//==============================================================================
procedure PS_EventUseLine(actor: Pmobj_t; line: Integer; side: Integer);
begin
  ps_currentactor := actor;
  psmanager.Events.ProcUseLine(PS_KeyFromMobj(actor), line, side);
  ps_currentactor := nil;
end;

//==============================================================================
//
// PS_EventTick
//
//==============================================================================
procedure PS_EventTick(tick: integer);
begin
  psmanager.Events.ProcTick(tick);
end;

//==============================================================================
//
// PS_EventTimerEverySecond
//
//==============================================================================
procedure PS_EventTimerEverySecond(second: integer);
begin
  psmanager.Events.ProcTimerEverySecond(second);
end;

//==============================================================================
//
// PS_EventTimerEveryMinute
//
//==============================================================================
procedure PS_EventTimerEveryMinute(minute: integer);
begin
  psmanager.Events.ProcTimerEveryMinute(minute);
end;

//==============================================================================
//
// PS_EventMapStart
//
//==============================================================================
procedure PS_EventMapStart;
begin
  psmanager.Events.ProcMapStart;
end;

//-------------------- Map Script Serialization --------------------------------

//==============================================================================
//
// PS_MapScriptSaveToFile
//
//==============================================================================
function PS_MapScriptSaveToFile(const fname: string): boolean;
var
  psser: TScriptSerializer;
begin
  psser := TScriptSerializer.Create(psmanager.EventsExec);
  try
    result := psser.SaveToFile(fname);
  finally
    psser.Free;
  end;
end;

//==============================================================================
//
// PS_MapScriptSaveSize
//
//==============================================================================
function PS_MapScriptSaveSize: integer;
var
  psser: TScriptSerializer;
begin
  psser := TScriptSerializer.Create(psmanager.EventsExec);
  try
    Result := psser.SaveSize;
  finally
    psser.Free;
  end;
end;

//==============================================================================
//
// PS_MapScriptAppendToFile
//
//==============================================================================
function PS_MapScriptAppendToFile(const fname: string): boolean;
var
  psser: TScriptSerializer;
begin
  psser := TScriptSerializer.Create(psmanager.EventsExec);
  try
    result := psser.AppendToFile(fname);
  finally
    psser.Free;
  end;
end;

//==============================================================================
//
// PS_MapScriptLoadFromFile
//
//==============================================================================
function PS_MapScriptLoadFromFile(const fname: string): boolean;
var
  psser: TScriptSerializer;
begin
  psser := TScriptSerializer.Create(psmanager.EventsExec);
  try
    result := psser.LoadFromFile(fname);
  finally
    psser.Free;
  end;
end;

//==============================================================================
//
// PS_MapScriptLoadFromFilePos
//
//==============================================================================
function PS_MapScriptLoadFromFilePos(const fname: string; var position: integer): boolean;
var
  psser: TScriptSerializer;
begin
  psser := TScriptSerializer.Create(psmanager.EventsExec);
  try
    result := psser.LoadFromFilePos(fname, position);
  finally
    psser.Free;
  end;
end;

//==============================================================================
//
// A_RunScript
//
//==============================================================================
procedure A_RunScript(actor: Pmobj_t);
var
  scid: integer;
  i: integer;
begin
  if actor.flags2_ex and MF2_EX_DONTRUNSCRIPTS <> 0 then
    Exit;

  if not P_CheckStateParams(actor) then
    Exit;

  for i := 0 to actor.state.params.Count - 1 do
  begin
    if not actor.state.params.IsComputed[i] then
      actor.state.params.IntVal[i] := PS_GetScriptIDFromName(actor.state.params.StrVal[i]);

    scid := actor.state.params.IntVal[i];
    if scid > -1 then
      PS_DoRunScript(actor, scid);
  end;
end;

end.

