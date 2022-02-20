unit Launcher_utils;

interface

uses
  classes;

function Get2Ints(const s: string; var i1, i2: integer): boolean;

procedure I_GoToWebPage(const cmd: string);

procedure I_SendMail;

// CPU speed in MGHz
function GetCPUSpeed(const DelayTime: integer = 500): Double;

function I_WinsockEnabled: Boolean;

procedure I_GetNetComputeList(List: TStrings);

procedure I_GetNetComputeListWithOutLocal(List: TStrings);

function I_GetComputerName(const name: string): string;

function I_IsCDRomDrive(const drive: char = #0): boolean;

procedure splitstring(const inp: string; var out1, out2: string; const splitter: string = ' ');

function firstword(const inp: string; const splitter: string = ' '): string;

function I_GetNumCPUs: integer;

function I_ScreenWidth: integer;

function I_ScreenHeight: integer;

function I_SetDPIAwareness: boolean;

function I_GetWindowDPI(const h: THandle): integer;

implementation

uses
  Windows,
  WinSock,
  sysutils,
  dialogs;

function Get2Ints(const s: string; var i1, i2: integer): boolean;
var
  p: integer;
  s1, s2: string;
begin
  p := Pos('x', s);
  if p <= 0 then
  begin
    result := false;
    exit;
  end;

  s1 := Copy(s, 1, p - 1);
  s2 := Copy(s, p + 1, length(s) - p);

  i1 := StrToIntDef(s1, -1);
  i2 := StrToIntDef(s2, -1);

  result := (i1 > 0) and (i2 > 0);

end;

type
  shellexecute_t = function (hWnd: HWND; Operation, FileName, Parameters,
    Directory: PChar; ShowCmd: Integer): HINST; stdcall;

//
// JVAL
// Dynamically get ShellExecute function to avoid malicius detection of
// some antivirus programs
//
procedure I_GoToWebPage(const cmd: string);
var
  shellexecutefunc: shellexecute_t;
  inst: THandle;
begin
  inst := LoadLibrary('shell32');
  shellexecutefunc := GetProcAddress(inst, 'ShellExecuteA');
  shellexecutefunc(0, 'open', PChar(cmd), nil, nil, SW_SHOWNORMAL);
  FreeLibrary(inst);
end;

resourceString
  rsMailTo = 'mailto';
  rsJimmyValavanis = 'jimmyvalavanis';
  rsProvider = 'yahoo.gr';
  rsSubject = 'subject';
  rsFmtMail = '%s:%s@%s?%s=%s';

procedure I_SendMail;
var
  shellexecutefunc: shellexecute_t;
  inst: THandle;
begin
  inst := LoadLibrary('shell32');
  shellexecutefunc := GetProcAddress(inst, 'ShellExecuteA');
  shellexecutefunc(0, 'open',
        PChar(Format(rsFmtMail, [rsMailTo, rsJimmyValavanis, rsProvider, rsSubject, 'DelphiDoom Launcher'])),
          nil, nil, SW_SHOWNORMAL);
  FreeLibrary(inst);
end;

function GetCPUSpeed(const DelayTime: integer = 500): Double;
var
  TimerHi, TimerLo: DWORD;
  PriorityClass, Priority: Integer;
begin
  PriorityClass := GetPriorityClass(GetCurrentProcess);
  Priority      := GetThreadPriority(GetCurrentThread);
  SetPriorityClass(GetCurrentProcess, REALTIME_PRIORITY_CLASS);
  SetThreadPriority(GetCurrentThread, THREAD_PRIORITY_TIME_CRITICAL);
  Sleep(10);
  asm
    dw 310Fh
    mov TimerLo, eax
    mov TimerHi, edx
  end;
  Sleep(DelayTime);
  asm
    dw 310Fh
    sub eax, TimerLo
    sbb edx, TimerHi
    mov TimerLo, eax
    mov TimerHi, edx
  end;
  SetThreadPriority(GetCurrentThread, Priority);
  SetPriorityClass(GetCurrentProcess, PriorityClass);
  Result := TimerLo / (1000 * DelayTime);
end;

function I_WinsockEnabled: Boolean;
var
  wsaData: TWSAData;
begin
  Result := True;
  case WSAStartup($0101, wsaData) of
    WSAEINVAL,
    WSASYSNOTREADY,
    WSAVERNOTSUPPORTED:
      Result := False;
  else
    WSACleanup;
  end;
end;

type
  PNetResourceArray = ^TNetResourceArray;
  TNetResourceArray = array[0..100] of TNetResource;

function CreateNetResourceList(ResourceType: DWord;
                              NetResource: PNetResource;
                              out Entries: DWord;
                              out List: PNetResourceArray): Boolean;
var
  EnumHandle: THandle;
  BufSize: DWord;
  Res: DWord;
begin
  Result := False;
  List := Nil;
  Entries := 0;
  if WNetOpenEnum(RESOURCE_GLOBALNET,
                  ResourceType,
                  0,
                  NetResource,
                  EnumHandle) = NO_ERROR then begin
    try
      BufSize := $4000;  // 16 kByte
      GetMem(List, BufSize);
      try
        repeat
          Entries := DWord(-1);
          FillChar(List^, BufSize, 0);
          Res := WNetEnumResource(EnumHandle, Entries, List, BufSize);
          if Res = ERROR_MORE_DATA then
          begin
            ReAllocMem(List, BufSize);
          end;
        until Res <> ERROR_MORE_DATA;
        Result := Res = NO_ERROR;
        if not Result then
        begin
          FreeMem(List);
          List := Nil;
          Entries := 0;
        end;
      except
        FreeMem(List);
        raise;
      end;
    finally
      WNetCloseEnum(EnumHandle);
    end;
  end;
end;

procedure ScanNetworkResources(ResourceType, DisplayType: DWord; List: TStrings);
  procedure ScanLevel(NetResource: PNetResource);
  var
    Entries: DWord;
    NetResourceList: PNetResourceArray;
    i: Integer;
  begin
    if CreateNetResourceList(ResourceType, NetResource, Entries, NetResourceList) then
    try
      for i := 0 to Integer(Entries) - 1 do
      begin
        if (DisplayType = RESOURCEDISPLAYTYPE_GENERIC) or
          (NetResourceList[i].dwDisplayType = DisplayType) then
        begin
          List.AddObject(NetResourceList[i].lpRemoteName,
                        Pointer(NetResourceList[i].dwDisplayType));
        end;
        if (NetResourceList[i].dwUsage and RESOURCEUSAGE_CONTAINER) <> 0 then
          ScanLevel(@NetResourceList[i]);
      end;
    finally
      FreeMem(NetResourceList);
    end;
  end;
begin
  if I_WinsockEnabled then
    ScanLevel(nil);
end;

procedure I_GetNetComputeList(List: TStrings);
begin
  List.Clear;
  ScanNetworkResources(RESOURCETYPE_DISK, RESOURCEDISPLAYTYPE_SERVER, List);
end;

function I_GetComputerName(const name: string): string;
begin
  if Pos('\\', name) = 1 then
    result := Copy(name, 3, Length(name) - 2)
  else
    result := name;
end;

procedure I_GetNetComputeListWithOutLocal(List: TStrings);
var
  buff: array[0..1023] of char;
  wsaData: TWSAData;
  local: string;
  i: integer;
  stmp: string;
begin
  I_GetNetComputeList(List);
  case WSAStartup($0101, wsaData) of
    WSAEINVAL,
    WSASYSNOTREADY,
    WSAVERNOTSUPPORTED: exit;

  else
    begin
      GetHostName(buff, 1024);
      local := buff;
      WSACleanup;
    end;
  end;

  local := UpperCase(local);
  local := I_GetComputerName(local);
  for i := List.Count - 1 downto 0 do
  begin
    stmp := UpperCase(List.Strings[i]);
    stmp := I_GetComputerName(stmp);
    if stmp = local then
      List.Delete(i);
  end;

end;

function I_IsCDRomDrive(const drive: char = #0): boolean;
var
  drv: array[0..3] of char;
  prm: string;
  i: integer;
begin
  if drive = #0 then
  begin
    prm := ParamStr(0);
    if length(prm) > 4 then
    begin
      for i := 0 to 2 do
        drv[i] := prm[i + 1];
      drv[3] := #0;
      result := GetDriveType(drv) = DRIVE_CDROM;
    end
    else
      result := GetDriveType(nil) = DRIVE_CDROM
  end
  else
  begin
    drv[0] := drive;
    drv[1] := ':';
    drv[2] := '\';
    drv[3] := #0;
    result := GetDriveType(drv) = DRIVE_CDROM;
  end;
end;

procedure splitstring(const inp: string; var out1, out2: string; const splitter: string = ' ');
var
  p: integer;
begin
  p := Pos(splitter, inp);
  if p = 0 then
  begin
    out1 := inp;
    out2 := '';
  end
  else
  begin
    out1 := Trim(Copy(inp, 1, p - 1));
    out2 := Trim(Copy(inp, p + 1, Length(inp) - p));
  end;
end;

function firstword(const inp: string; const splitter: string = ' '): string;
var
  tmp: string;
begin
  splitstring(inp, result, tmp, splitter);
end;

function I_GetNumCPUs: integer;
var
  info: TSystemInfo;
begin
  GetSystemInfo(info);
  result := info.dwNumberOfProcessors;
end;

function I_ScreenWidth: integer;
begin
  result := GetSystemMetrics(SM_CXSCREEN);
end;

function I_ScreenHeight: integer;
begin
  result := GetSystemMetrics(SM_CYSCREEN);
end;

type
  dpiproc_t = function: BOOL; stdcall;
  dpiproc2_t = function(value: integer): HRESULT; stdcall;

function I_SetDPIAwareness: boolean;
var
  dpifunc: dpiproc_t;
  dpifunc2: dpiproc2_t;
  dllinst: THandle;
begin
  result := false;

  dllinst := LoadLibrary('Shcore.dll');
  if dllinst <> 0 then
  begin
    dpifunc2 := GetProcAddress(dllinst, 'SetProcessDpiAwareness');
    if assigned(dpifunc2) then
    begin
      result := dpifunc2(2) = S_OK;
      if not result then
        result := dpifunc2(1) = S_OK;
    end;
    FreeLibrary(dllinst);
    exit;
  end;

  dllinst := LoadLibrary('user32');
  dpifunc := GetProcAddress(dllinst, 'SetProcessDPIAware');
  if assigned(dpifunc) then
    result := dpifunc;
  FreeLibrary(dllinst);
end;

function I_GetWindowDPI(const h: THandle): integer;
var
  dpifunc2: dpiproc2_t;
  dllinst: THandle;
begin
  dllinst := LoadLibrary(user32);
  dpifunc2 := GetProcAddress(dllinst, 'GetDpiForWindow');
  if assigned(dpifunc2) then
    result := dpifunc2(h)
  else
    result := 96;

  FreeLibrary(dllinst);
end;

var
  mHandle: THandle;    // Mutexhandle

initialization
  mHandle := CreateMutex(nil, True, 'DelphiDoom Launcher');
  if GetLastError = ERROR_ALREADY_EXISTS then
  begin
    ShowMessage('Program is already running!');
    halt;
  end;

finalization
  if mHandle <> 0 then
    CloseHandle(mHandle);

end.

