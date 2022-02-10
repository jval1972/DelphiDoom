unit uPSR_dll;

{$I PascalScript.inc}
interface

uses
  ps_runtime, ps_utils, ps_defs;

//==============================================================================
//
// RegisterDLLRuntime
//
//==============================================================================
procedure RegisterDLLRuntime(Caller: TPSExec);

//==============================================================================
//
// RegisterDLLRuntimeEx
//
//==============================================================================
procedure RegisterDLLRuntimeEx(Caller: TPSExec; AddDllProcImport, RegisterUnloadDLL: Boolean);

//==============================================================================
//
// ProcessDllImport
//
//==============================================================================
function ProcessDllImport(Caller: TPSExec; P: TPSExternalProcRec): Boolean;

//==============================================================================
//
// ProcessDllImportEx
//
//==============================================================================
function ProcessDllImportEx(Caller: TPSExec; P: TPSExternalProcRec; ForceDelayLoad: Boolean): Boolean;

//==============================================================================
//
// UnloadDLL
//
//==============================================================================
procedure UnloadDLL(Caller: TPSExec; const sname: TbtString);

//==============================================================================
//
// UnloadProc
//
//==============================================================================
function UnloadProc(Caller: TPSExec; p: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;

implementation

uses
  ps_dll,
  {$IFDEF UNIX}
  Unix, baseunix, dynlibs, termio, sockets;
  {$ELSE}
  {$IFDEF KYLIX}SysUtils;{$ELSE}Windows;{$ENDIF}
  {$ENDIF}

{
p^.Ext1 contains the pointer to the Proc function
p^.ExportDecl:
  'dll:'+DllName+#0+FunctionName+#0+chr(Cc)+Chr(DelayLoad)+Chr(AlternateSearchPath)+VarParams
}

type
  PLoadedDll = ^TLoadedDll;
  TLoadedDll = record
    dllnamehash: Longint;
    dllname: TbtString;
    dllhandle: THandle;
    pakdll: boolean;
  end;
  TMyExec = class(TPSExec);
  PInteger = ^Integer;

//==============================================================================
//
// LastErrorFree
//
//==============================================================================
procedure LastErrorFree(Sender: TPSExec; P: PInteger);
begin
  dispose(p);
end;

//==============================================================================
//
// DLLSetLastError
//
//==============================================================================
procedure DLLSetLastError(Sender: TPSExec; P: Integer);
var
  pz: PInteger;
begin
  pz := Sender.FindProcResource(@LastErrorFree);
  if pz = nil then
  begin
    new(pz);
    Sender.AddResource(@LastErrorFree, PZ);
  end;
  pz^ := p;
end;

//==============================================================================
//
// DLLGetLastError
//
//==============================================================================
function DLLGetLastError(Sender: TPSExec): Integer;
var
  pz: PInteger;
begin
  pz := Sender.FindProcResource(@LastErrorFree);
  if pz = nil then
    Result := 0
  else
    Result := pz^;
end;

//==============================================================================
//
// DllFree
//
//==============================================================================
procedure DllFree(Sender: TPSExec; P: PLoadedDll);
begin
  FreeLibrary(p^.dllhandle);
  Dispose(p);
end;

//==============================================================================
//
// LoadDll
//
//==============================================================================
function LoadDll(Caller: TPSExec; P: TPSExternalProcRec): Boolean;
var
  s, s2, s3: TbtString;
  h, i: Longint;
  ph: PLoadedDll;
  dllhandle: THandle;
  loadwithalteredsearchpath: Boolean;
  pakdll: boolean;
  {$IFNDEF UNIX}
  FileName: String;
  {$ENDIF}
begin
  s := p.Decl;
  Delete(s, 1, 4);
  s2 := Copy(s, 1, Pos(TbtChar(#0), s) - 1);
  Delete(s, 1, Length(s2) + 1);
  h := makehash(s2);
  s3 := Copy(s, 1, Pos(TbtChar(#0), s) - 1);
  Delete(s, 1, Length(s3) + 1);
  loadwithalteredsearchpath := bytebool(s[3]);
  i := 2147483647; // maxint
  dllhandle := 0;
  repeat
    ph := Caller.FindProcResource2(@dllFree, i);
    if ph = nil then
    begin
      if s2 = '' then
      begin
        // don't pass an empty FileName to LoadLibrary, just treat it as uncallable
        p.Ext2 := Pointer(1);
        Result := False;
        Exit;
      end;

      {$IFDEF UNIX}
      {$DEFINE UNIX_OR_KYLIX}
      {$ENDIF}
      {$IFDEF KYLIX}
      {$DEFINE UNIX_OR_KYLIX}
      {$ENDIF}

      {$IFDEF UNIX_OR_KYLIX}
      dllhandle := LoadLibrary(PChar(s2));
      {$ELSE}
      {$IFDEF UNICODE}
      if Copy(s2, 1, 6) = '<utf8>' then
        FileName := UTF8ToUnicodeString(Copy(s2, 7, Maxint))
      else
        FileName := string(s2);
      {$ELSE}
      FileName := s2;
      {$ENDIF}
      dllhandle := PS_PAKLoadDll(FileName);
      if dllhandle = 0 then
      begin
        pakdll := false;
        if loadwithalteredsearchpath then
          dllhandle := LoadLibraryEx(PChar(FileName), 0, LOAD_WITH_ALTERED_SEARCH_PATH)
        else
          dllhandle := LoadLibrary(PChar(FileName));
      end
      else
        pakdll := true;
      {$ENDIF}
      if dllhandle = 0 then
      begin
        p.Ext2 := Pointer(1);
        Result := False;
        Exit;
      end;
      new(ph);
      ph^.dllnamehash := h;
      ph^.dllname := s2;
      ph^.dllhandle := dllhandle;
      ph^.pakdll := pakdll;
      Caller.AddResource(@DllFree, ph);
    end;
    if (ph^.dllnamehash = h) and (ph^.dllname = s2) then
    begin
      dllhandle := ph^.dllhandle;
    end;
  until dllhandle <> 0;
  p.Ext1 := GetProcAddress(dllhandle, pansichar(s3));
  if p.Ext1 = nil then
  begin
    p.Ext2 := Pointer(1);
    Result := False;
    Exit;
  end;
  Result := True;
end;

//==============================================================================
//
// DllProc
//
//==============================================================================
function DllProc(Caller: TPSExec; p: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;
var
  i: Longint;
  MyList: TIfList;
  n: PPSVariantIFC;
  CurrStack: Cardinal;
  cc: TPSCallingConvention;
  s: TbtString;
begin
  if p.Ext2 <> nil then // error
  begin
    Result := False;
    Exit;
  end;
  if p.Ext1 = nil then
  begin
    if not LoadDll(Caller, P) then
    begin
      Result := False;
      Exit;
    end;
  end;
  s := p.Decl;
  Delete(S, 1, Pos(TbtChar(#0), s));
  Delete(S, 1, Pos(TbtChar(#0), s));
  if Length(S) < 2 then
  begin
    Result := False;
    Exit;
  end;
  cc := TPSCallingConvention(s[1]);
  Delete(s, 1, 3); // cc + delayload + alternatesearchpath (delayload might also be forced!)
  CurrStack := Cardinal(Stack.Count) - Cardinal(Length(s));
  if s[1] = #0 then
    Inc(CurrStack);
  MyList := tIfList.Create;
  for i := 2 to Length(s) do
  begin
    MyList.Add(nil);
  end;
  for i := Length(s) downto 2 do
  begin
    MyList[i - 2] := NewPPSVariantIFC(Stack[CurrStack], s[i] <> #0);
    Inc(CurrStack);
  end;
  if s[1] <> #0 then
  begin
    n := NewPPSVariantIFC(Stack[CurrStack], True);
  end
  else
    n := nil;
  try
    TMYExec(Caller).InnerfuseCall(nil, p.Ext1, cc, MyList, n);
    {$IFNDEF UNIX}
    DLLSetLastError(Caller, GetLastError);
    {$ENDIF}
  finally
    DisposePPSvariantIFC(n);
    DisposePPSVariantIFCList(MyList);
  end;
  Result := True;
end;

//==============================================================================
//
// ProcessDllImport
//
//==============================================================================
function ProcessDllImport(Caller: TPSExec; P: TPSExternalProcRec): Boolean;
begin
  Result := ProcessDllImportEx(Caller, P, False);
end;

//==============================================================================
//
// ProcessDllImportEx
//
//==============================================================================
function ProcessDllImportEx(Caller: TPSExec; P: TPSExternalProcRec; ForceDelayLoad: Boolean): Boolean;
var
  DelayLoad: Boolean;
  s: TbtString;
begin
  if not ForceDelayLoad then
  begin
    s := p.Decl;
    Delete(s, 1, Pos(TbtChar(#0), s));
    Delete(s, 1, Pos(TbtChar(#0), s));
    DelayLoad := bytebool(s[2]);
  end
  else
    DelayLoad := True;

  if DelayLoad then
  begin
    p.ProcPtr := DllProc;
    Result := True;
  end
  else
  begin
    p.ProcPtr := DllProc;
    Result := LoadDll(Caller, p);
  end;
end;

//==============================================================================
//
// GetLastErrorProc
//
//==============================================================================
function GetLastErrorProc(Caller: TPSExec; p: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;
begin
  Stack.SetInt(-1, DLLGetLastError(Caller));
  Result := True;
end;

//==============================================================================
//
// UnloadDLL
//
//==============================================================================
procedure UnloadDLL(Caller: TPSExec; const sname: TbtString);
var
  h, i: Longint;
  pv: TPSProcRec;
  ph: PLoadedDll;
  s: TbtString;
begin
  for i := Caller.GetProcCount - 1 downto 0 do
  begin
    pv := Caller.GetProcNo(i);
    if not (pv is TPSExternalProcRec) then
      Continue;
    if @TPSExternalProcRec(pv).ProcPtr <> @DllProc then
      Continue;
    s := (TPSExternalProcRec(pv).Decl);
    Delete(s, 1, 4);
    if Copy(s, 1, Pos(TbtChar(#0), s) - 1) = sname then
    begin
      TPSExternalProcRec(pv).Ext1 := nil;
    end;
  end;
  h := MakeHash(sname);
  i := 2147483647; // maxint
  repeat
    ph := Caller.FindProcResource2(@dllFree, i);
    if ph = nil then
      Break;
    if (ph.dllnamehash = h) and (ph.dllname = sname) then
    begin
      if ph.pakdll then
        PS_PAKUnLoadDll(ph.dllname)
      else
        FreeLibrary(ph^.dllhandle);
      Caller.DeleteResource(ph);
      dispose(ph);
    end;
  until False;
end;

//==============================================================================
//
// UnloadProc
//
//==============================================================================
function UnloadProc(Caller: TPSExec; p: TPSExternalProcRec; Global, Stack: TPSStack): Boolean;
begin
  UnloadDLL(Caller, Stack.GetAnsiString(-1));
  Result := True;
end;

//==============================================================================
//
// RegisterDLLRuntime
//
//==============================================================================
procedure RegisterDLLRuntime(Caller: TPSExec);
begin
  RegisterDLLRuntimeEx(Caller, True, True);
end;

//==============================================================================
//
// RegisterDLLRuntimeEx
//
//==============================================================================
procedure RegisterDLLRuntimeEx(Caller: TPSExec; AddDllProcImport, RegisterUnloadDLL: Boolean);
begin
  if AddDllProcImport then
    Caller.AddSpecialProcImport('dll', @ProcessDllImport, nil);
  if RegisterUnloadDLL then
    Caller.RegisterFunctionName('UnloadDll', UnloadProc, nil, nil);
  Caller.RegisterFunctionName('DllGetLastError', GetLastErrorProc, nil, nil);
end;

end.

