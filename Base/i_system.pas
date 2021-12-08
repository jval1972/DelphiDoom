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
//  System specific interface stuff.
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit i_system;

interface

uses
  d_delphi,
  d_ticcmd;

procedure I_Init;

// Called by startup code
// to get the ammount of memory to malloc
// for the zone management.
function I_ZoneBase(var size: integer): pointer;
procedure I_ZoneFree(var p: pointer);

// Called by D_DoomLoop,
// Called by HU_DoFPSStuff
// returns current time in tics.
function I_GetSysTime: extended;
function I_GetTime: integer;
function I_GetFracTime: integer;

//
//  Called by D_DoomLoop,
//  called before processing any tics in a frame
//  (just after displaying a frame).
//  Time consuming syncronous operations
//  are performed here (joystick reading).
//  Can call D_PostEvent.
//
procedure I_StartFrame;

//
//  Called by D_DoomLoop, }
//  called before processing each tic in a frame.
//  Quick syncronous operations are performed here.
//  Can call D_PostEvent.
procedure I_StartTic;

//  Asynchronous interrupt functions should maintain private queues
//  that are read by the synchronous functions
//  to be converted into events.
//  Either returns a null ticcmd,
//  or calls a loadable driver to build it.
//  This ticcmd will then be modified by the gameloop
//  for normal input.
function I_BaseTiccmd: Pticcmd_t;
function I_BaseTiccmd202: Pticcmd_t202;

//  Called by M_Responder when quit is selected.
//  Clean exit, displays sell blurb.
procedure I_Quit;

procedure I_Destroy(const code: integer);

procedure I_FlashCachedOutput;

procedure I_Error(const error: string; const Args: array of const); overload;

procedure I_Error(const error: string); overload;

procedure I_DevError(const error: string; const Args: array of const); overload;

procedure I_DevError(const error: string); overload;

procedure I_Warning(const warning: string; const Args: array of const); overload;

procedure I_Warning(const warning: string); overload;

procedure I_DevWarning(const error: string; const Args: array of const); overload;

procedure I_DevWarning(const error: string); overload;

function I_GameFinished: boolean;

procedure I_ProcessWindows;

procedure I_WaitVBL(const count: integer);

procedure I_Sleep(const msecs: integer);

var
  zonesize: integer;
  mb_used: integer;
  InBackground: boolean = true;
  in_i_error: boolean = false;

procedure I_BeginDiskBusy;

function I_IsCDRomDrive(const drive: char = #0): boolean;

function I_GetExeImageSize(fname: string = ''): LongWord;

function I_VersionBuilt(fname: string = ''): string;

function I_DirectoryExists(const Name: string): Boolean;

procedure I_SetCriticalCPUPriority;

procedure I_SetNormalCPUPriority;

procedure I_DetectOS;

procedure I_DetectCPU;

function I_GetNumCPUs: integer;

procedure I_ClearInterface(var Dest: IInterface);

type
  process_t = function(p: pointer): LongInt; stdcall;

function I_CreateProcess(p: process_t; parm: pointer; suspended: boolean): integer;

procedure I_WaitForProcess(pid: integer; msec: integer);

procedure I_GoToWebPage(const cmd: string);


type
  osplatform_t = (os_unknown, os_Win95, os_WinNT4, os_Win2k);

var
  osplatform: osplatform_t;

var
  isdiskbusy: boolean = false;

var
  safemode: boolean = false;
  usemmx: boolean = true;
  usemultithread: boolean;
  criticalcpupriority: boolean;
  win_vista_or_newer: boolean = true;

function I_ScreenWidth: integer;

function I_ScreenHeight: integer;

function I_SetDPIAwareness: boolean;

function I_GetWindowDPI(const h: THandle): integer;

implementation

uses
{$IFDEF FPC}
  d_fpc,
{$ENDIF}
  c_cmds,
  Windows,
  Messages,
  doomdef,
  m_argv,
  m_misc,
  m_fixed,
  i_sound,
  i_music,
  i_input,
  i_io,
  i_net,
  i_tmp,
{$IFNDEF FPC}
  i_startup,
{$ENDIF}
{$IFDEF OPENGL}
  gl_main,
{$ELSE}
  i_video,
  i_main,
{$ENDIF}
  i_mainwindow,
  d_main,
  d_net,
{$IFDEF HEXEN}
  g_demo,
{$ENDIF}
  g_game,
  m_base;

var
  finished: boolean = false;

function I_GameFinished: boolean;
begin
  result := finished;
end;

//
// I_StartFrame
//
procedure I_StartFrame;
begin
  I_ProcessWindows;
  I_ProcessMusic;
  I_ProcessInput;
end;

//
// I_StartTic
//
procedure I_StartTic;
begin
  if not InBackground then   // JVAL ?
    I_ProcessInput;          // JVAL ?
end;

var
  emptycmd: ticcmd_t;

function I_BaseTiccmd: Pticcmd_t;
begin
  result := @emptycmd;
end;

var
  emptycmd202: ticcmd_t202;

function I_BaseTiccmd202: Pticcmd_t202;
begin
  result := @emptycmd202;
end;

function I_GetHeapSize: integer;
begin
  // Zone is depricated
  result := $10000; //mb_used * 1024 * 1024;
end;

function I_ZoneBase(var size: integer): pointer;
begin
  size := I_GetHeapSize;
  result := malloc(size);
end;

procedure I_ZoneFree(var p: pointer);
begin
  memfree(p, I_GetHeapSize);
end;

//
// I_GetTime
// returns time in 1/70th second tics
//
var
  basetime: int64 = 0;
  Freq: int64;

function I_GetSysTime: extended;
var
  _time: int64;
begin
  if Freq = 1000 then
    _time := GetTickCount
  else
  begin
    if not QueryPerformanceCounter(_time) then
    begin
      _time := GetTickCount;
      Freq := 1000;
      basetime := 0;
      I_Warning('QueryPerformanceCounter() failed, basetime reset.'#13#10);
    end;
  end;
  if basetime = 0 then
    basetime := _time;
  result := (_time - basetime) / Freq;
end;

function I_GetTime: integer;
begin
  result := trunc(I_GetSysTime * TICRATE);
end;

function I_GetFracTime: integer;
begin
  result := trunc(I_GetSysTime * TICRATE * FRACUNIT);
end;

//
procedure I_CmdUseMMX(const parm: string = '');
var
  newusemmx: boolean;
begin
  if parm = '' then
  begin
    printf('Current setting: usemmx = %s.'#13#10, [truefalseStrings[usemmx]]);
    exit;
  end;

  newusemmx := C_BoolEval(parm, usemmx);
  if usemmx <> newusemmx then
  begin
    usemmx := newusemmx;
    if usemmx then
      I_DetectCPU
    else
    begin
      mmxmachine := 0;
      AMD3DNowMachine := 0;
    end;
  end;
  I_CmdUseMMX;
end;

//
// I_Init
//
procedure I_Init;
begin
  printf('I_DetectOS: Detecting operating system.'#13#10);
  I_DetectOS;
  printf('I_InitSound: Initializing DirectSound.'#13#10);
  I_InitSound;
  printf('I_InitMusic: Initializing music.'#13#10);
  I_InitMusic;
  printf('I_InitInput: Initializing Input Devices.'#13#10);
  I_InitInput;

  C_AddCmd('usemmx, mmx', @I_CmdUseMMX);
end;

//
// I_Quit
//
procedure I_Quit;
begin
  PostMessage(hMainWnd, WM_DESTROY, 0, 0);
end;

procedure I_RestoreDesktop;
begin
  InvalidateRect(0, nil, true)
end;

procedure I_Destroy(const code: integer);
begin
  printf(#13#10'I_Destroy: Game finished.'#13#10#13#10);
  finished := true;
  printf('D_QuitNetGame: Quit network game.'#13#10);
  D_QuitNetGame;
  printf('I_ShutDownSound: Shut down sound.'#13#10);
  I_ShutDownSound;
  printf('I_ShutDownMusic: Shut down music.'#13#10);
  I_ShutDownMusic;
  printf('I_ShutDownInput: Shut down input.'#13#10);
  I_ShutDownInput;
  if M_CheckParm('-dontsavedefaults') = 0 then
  begin
    printf('M_SaveDefaults: Saving defaults.'#13#10);
    M_SaveDefaults;
  end;
  printf('I_ShutDownGraphics: Shut down graphics.'#13#10);
  I_ShutDownGraphics;
  printf('D_ShutDown: Shut down doom engine.'#13#10);
  D_ShutDown;
  printf('I_ShutDownNetwork: Shut down network.'#13#10);
  I_ShutDownNetwork;
  printf('I_RestoreDesktop: Restoring desktop.'#13#10);
  I_RestoreDesktop;
  printf('I_ShutDownTempFiles: Shut down temporary file managment.'#13#10);
  I_ShutDownTempFiles;
  printf('I_ShutDownIO: Shut down input/output.'#13#10'Halt(%d)'#13#10, [code]);
  I_ShutDownIO;
  Halt(code);
end;

procedure I_FlashCachedOutput;
begin
  if stdoutbuffer <> nil then
    stdoutbuffer.SaveToFile({$IFDEF OPENGL}'GL' + {$ENDIF}_GAME + '_stdout.cachedbuffer.txt');
end;

//
// I_Error
//
procedure I_Error(const error: string; const Args: array of const);
var
  soutproc: TOutProc;
begin
// JVAL: Avoid recursive calls
  if in_i_error then
    exit;

  I_FlashCachedOutput;

  in_i_error := true;

  fprintf(stderr, 'I_Error: ' + error + #13#10, Args);

  // ShutDown. Here might be other errors.
  if demorecording then
    G_CheckDemoStatus;

  soutproc := outproc;
  I_IOSetWindowHandle({$IFDEF FPC}0{$ELSE}SUC_GetHandle{$ENDIF});
  outproc := I_IOErrorMessageBox;
  printf(error, Args);
  outproc := soutproc;
  printf('I_Error: ' + error + #13#10, Args);

  I_Destroy(1);
end;

procedure I_Error(const error: string);
begin
  I_Error(error, []);
end;

procedure I_DevError(const error: string; const Args: array of const);
begin
  if devparm then
    I_Warning(error, Args)
  else
    I_Error(error + #13#10#13#10'Specify -devparm from the command line if you want this error to be ignored', Args);
end;

procedure I_DevError(const error: string);
begin
  I_DevError(error, []);
end;


procedure I_Warning(const warning: string; const Args: array of const);
var
  msg: string;
begin
  sprintf(msg, warning, Args);
  I_Warning(msg);
end;

procedure I_Warning(const warning: string);
var
  wrstr: string;
begin
  wrstr := 'I_Warning: ' + warning;
  fprintf(stderr, wrstr);
  printf(wrstr);
end;

procedure I_DevWarning(const error: string; const Args: array of const);
begin
  if devparm then
    I_Warning(error, Args);
end;

procedure I_DevWarning(const error: string);
begin
  if devparm then
    I_Warning(error);
end;

procedure I_ProcessWindows;
var
  msg: TMsg;
begin
  while PeekMessage(msg, 0, 0, 0, PM_REMOVE) do
  begin
    if msg.message <> WM_QUIT then
    begin
      TranslateMessage(msg);
      DispatchMessage(msg);
    end;
  end;
end;

// Wait for vertical retrace or pause a bit.
procedure I_WaitVBL(const count: integer);
var
  waituntil: extended;
begin
  waituntil := I_GetSysTime + count / 1000;
  repeat
    sleep(0);
  until I_GetSysTime >= waituntil;
end;

procedure I_Sleep(const msecs: integer);
begin
  sleep(msecs);
end;

procedure I_BeginDiskBusy;
begin
  isdiskbusy := true;
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

const
  IMAGE_NT_OPTIONAL_HDR32_MAGIC = $10b;

function I_GetOptHeader(PEOptHeader: PImageOptionalHeader; fname: string = ''): boolean;
var
  f: file;
  PEHeaderOffset, PESig: Cardinal;
  EXESig: Word;
  PEHeader: TImageFileHeader;
begin
  if fname = '' then
    fname := ParamStr(0);

  if not fopen(f, fname, fOpenReadOnly) then
  begin
    result := false;
    exit;
  end;

  {$I-}
  BlockRead(f, EXESig, SizeOf(EXESig));
  if EXESig <> $5A4D {'MZ'} then
  begin
    close(f);
    result := false;
    exit;
  end;
  seek(f, $3C);
  BlockRead(f, PEHeaderOffset, SizeOf(PEHeaderOffset));
  if PEHeaderOffset = 0 then
  begin
    close(f);
    result := false;
    exit;
  end;
  seek(f, PEHeaderOffset);
  BlockRead(f, PESig, SizeOf(PESig));
  if PESig <> $00004550 {'PE'#0#0} then
  begin
    close(f);
    result := false;
    exit;
  end;
  BlockRead(f, PEHeader, SizeOf(PEHeader));
  if PEHeader.SizeOfOptionalHeader <> SizeOf(TImageOptionalHeader) then
  begin
    close(f);
    result := false;
    exit;
  end;
  BlockRead(f, PEOptHeader^, SizeOf(TImageOptionalHeader));
  if PEOptHeader.Magic <> IMAGE_NT_OPTIONAL_HDR32_MAGIC then
  begin
    close(f);
    result := false;
    exit;
  end;
  close(f);
  {$I+}
  result := IOResult = 0;
end;

function I_GetExeImageSize(fname: string = ''): LongWord;
var
  PEOptHeader: TImageOptionalHeader;
begin
  if I_GetOptHeader(@PEOptHeader, fname) then
    result := PEOptHeader.SizeOfImage
  else
    result := 0;
end;

function I_VersionBuilt(fname: string = ''): string;
var
  vsize: LongWord;
  zero: LongWord;
  buffer: PByteArray;
  res: pointer;
  len: LongWord;
  i: integer;
begin
  if fname = '' then
    fname := ParamStr(0);
  vsize := GetFileVersionInfoSize(PChar(fname), zero);
  if vsize = 0 then
  begin
    result := '';
    exit;
  end;

  buffer := PByteArray(malloc(vsize + 1));
  GetFileVersionInfo(PChar(fname), 0, vsize, buffer);
  VerQueryValue(buffer, '\StringFileInfo\040904E4\FileVersion', res, len);
  result := '';
  for i := 0 to len - 1 do
  begin
    if PChar(res)^ = #0 then
      break;
    result := result + PChar(res)^;
    res := pointer(integer(res) + 1);
  end;
  memfree(pointer(buffer), vsize + 1);
end;

function I_DirectoryExists(const Name: string): Boolean;
var
  Code: Integer;
begin
  Code := GetFileAttributes(PChar(Name));
  result := (Code <> -1) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0);
end;

var
  hThread: THandle;
  iPriority: integer = THREAD_PRIORITY_ERROR_RETURN;
  prioritycheck: integer = 0;

procedure I_SetCriticalCPUPriority;
begin
  if prioritycheck = 0 then
  begin
    iPriority := GetThreadPriority(hThread);
    if iPriority <> THREAD_PRIORITY_ERROR_RETURN then
      SetThreadPriority(hThread, THREAD_PRIORITY_TIME_CRITICAL);
    prioritycheck := 1;
  end;
end;

procedure I_SetNormalCPUPriority;
begin
  if prioritycheck = 1 then
  begin
    if iPriority <> THREAD_PRIORITY_ERROR_RETURN then
      SetThreadPriority(hThread, iPriority);
    prioritycheck := 0;
  end;
end;

procedure I_DetectOS;
var
  info: TOSVersionInfo;
  osname: string;
  osbuilt: integer;
begin
  ZeroMemory(@info, SizeOf(TOSVersionInfo));
  info.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(info);
  osname := '';
  case info.dwPlatformId of
    VER_PLATFORM_WIN32_WINDOWS:
      begin
        osplatform := os_Win95;
        if  info.dwMinorVersion < 10 then
          osname := '95'
        else if info.dwMinorVersion < 90 then
          osname := '98'
        else
          osname := 'Me';
        win_vista_or_newer := false;
      end;
    VER_PLATFORM_WIN32_NT:
      begin
        if info.dwMajorVersion < 5 then
          osplatform := os_WinNT4
        else
          osplatform := os_Win2k;
        if info.dwMajorVersion < 5 then
          osname := 'NT'
        else if info.dwMajorVersion = 5 then
        begin
          if info.dwMinorVersion = 0 then
            osname := '2000'
          else if info.dwMinorVersion = 1 then
            osname := 'XP'
          else if info.dwMinorVersion = 2 then
            osname := 'Server 2003';
        end
        else if (info.dwMajorVersion = 6) and (info.dwMinorVersion = 0) then
          osname := 'Vista'
        else if (info.dwMajorVersion = 6) and (info.dwMinorVersion = 1) then
          osname := '7';
        win_vista_or_newer := info.dwMajorVersion >= 6;
      end;
    else
      begin
        OSPlatform := os_unknown;
        osname := 'Unknown OS';
      end;
  end;

  if osplatform = os_Win95 then
    osbuilt := info.dwBuildNumber and $FFFF
  else
    osbuilt := info.dwBuildNumber;
  printf(' OS: Windows %s %u.%u (Build %u)'#13#10, [osname,
      info.dwMajorVersion, info.dwMinorVersion, osbuilt]);
  if info.szCSDVersion[0] <> #0 then
    printf('     %s'#13#10, [info.szCSDVersion]);

end;

var
  numcpus: Integer = 0;

procedure I_DetectCPU;
var
  info: TSystemInfo;
begin
  try
  // detect MMX and 3DNow! capable CPU (adapted from AMD's "3DNow! Porting Guide")
    asm
      pusha
      mov  eax, $80000000
      cpuid
      cmp  eax, $80000000
      jbe @@NoMMX3DNow
      mov mmxMachine, 1
      mov  eax, $80000001
      cpuid
      test edx, $80000000
      jz @@NoMMX3DNow
      mov AMD3DNowMachine, 1
  @@NoMMX3DNow:
      popa
    end;
  except
  // trap for old/exotics CPUs
    mmxMachine := 0;
    AMD3DNowMachine := 0;
  end;

  if mmxMachine <> 0 then
    printf(' MMX extentions detected'#13#10);
  if AMD3DNowMachine <> 0 then
    printf(' AMD 3D Now! extentions detected'#13#10);

  GetSystemInfo(info);
  numcpus := info.dwNumberOfProcessors;

  if numcpus > 1 then
    printf(' Multi-core system detected (%d CPUs)'#13#10, [numcpus]);

  if confignotfound then
    usemultithread := numcpus > 1;

  if usemultithread then
    printf(' Multithreading mode ON'#13#10)
  else
  begin
    if numcpus > 1 then
      printf(' Multithreading mode OFF (will not use all cores)'#13#10)
    else
      printf(' Multithreading mode OFF'#13#10);
  end;
end;

function I_GetNumCPUs: integer;
begin
  result := numcpus;
end;

procedure I_ClearInterface(var Dest: IInterface);
var
  P: Pointer;
begin
  if safemode then
    exit;
  if Dest <> nil then
  begin
    P := Pointer(Dest);
    Pointer(Dest) := nil;
    IInterface(P)._Release;
  end;
end;

function I_CreateProcess(p: process_t; parm: pointer; suspended: boolean): integer;
var
  id: LongWord;
begin
  if suspended then
    result := CreateThread(nil, $1000, @p, parm, CREATE_SUSPENDED, id)
  else
    result := CreateThread(nil, $1000, @p, parm, 0, id);
end;

procedure I_WaitForProcess(pid: integer; msec: integer);
begin
  WaitForSingleObject(pid, msec);
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

initialization
  if not QueryPerformanceFrequency(Freq) then
    Freq := 1000;

  hThread := GetCurrentThread;

end.
