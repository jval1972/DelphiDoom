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
//  Run a (console) program in background, capture program output.
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit i_exec;

interface

function I_ExecProgram(const cmd: string; const quiet: boolean): boolean;

implementation

uses
  d_delphi,
  Windows;

function I_ExecProgram(const cmd: string; const quiet: boolean): boolean;
var
  chReadBuffer: array[0..1023] of char; // pipe read buffer
  hReadPipe,
  hWritePipe,
  hWritePipe2: THandle;         // handles to the anonymous pipe
  szArgs: string;               // child process argument buffer
  cchReadBuffer: DWORD;         // number of bytes read or to be written
  si: TStartupInfo;             // for CreateProcess call
  pinf: TProcessInformation;    // for CreateProcess call
  saPipe: TSecurityAttributes;  // security for anonymous pipe
  os: TOSVersionInfo;           // operating system specs
  i: integer;
  rsOutPut: string;
begin
  os.dwOSVersionInfoSize := SizeOf(TOSVersionInfo); // required assignment

  // check if running on Windows NT, if not, display notice and terminate
  if GetVersionEx(os) then
  begin
      if (os.dwPlatformId <> VER_PLATFORM_WIN32_WINDOWS) and
         (os.dwPlatformId <> VER_PLATFORM_WIN32_NT) then
      begin
        result := false;
        exit;
      end;
  end
  else
  begin
    result := false;
    exit;
  end;

  rsOutPut := '';

  // set up the security attributes for the anonymous pipe */
  saPipe.nLength := SizeOf(TSecurityAttributes);
  saPipe.lpSecurityDescriptor := nil;
  // In order for the child to be able to write to the pipe, the handle
  // must be marked as inheritable by setting this flag:
  saPipe.bInheritHandle := true;

  // create the anonymous pipe
  result := CreatePipe(
    hReadPipe,  // read handle
    hWritePipe, // write handle, used as stdout by child
    @saPipe,    // security descriptor
    0);         // pipe buffer size
  if not result then
    exit;

  (* Now we need to change the inheritable property for the readable
  end of the pipe so that the child will not inherit that handle as
  a "garbage" handle. This will keep us from having extra,
  unclosable handles to the pipe. Alternatively, we could have
  opened the pipe with saPipe.bInheritHandle = FALSE and changed the
  inherit property on the *write* handle of the pipe to TRUE. *)

  result := DuplicateHandle(
    GetCurrentProcess, // source process
    hReadPipe,         // handle to duplicate
    GetCurrentProcess, // destination process
    nil,               // new handle - don't want one, change original handle
    0,                 // new access flags - ignored since DUPLICATE_SAME_ACCESS
    false,             // make it *not* inheritable
    DUPLICATE_SAME_ACCESS);
  if not result then
    exit;

  (* In most cases you can get away with using the same anonymous
  pipe write handle for both the child's standard output and
  standard error, but this may cause problems if the child app
  explicitly closes one of its standard output or error handles. If
  that happens, the anonymous pipe will close, since the child's
  standard output and error handles are really the same handle. The
  child won't be able to write to the other write handle since the
  pipe is now gone, and parent reads from the pipe will return
  ERROR_BROKEN_PIPE and child output will be lost. To solve this
  problem, simply duplicate the write end of the pipe to create
  another distinct, separate handle to the write end of the pipe.
  One pipe write handle will serve as standard out, the other as
  standard error. Now *both* write handles must be closed before the
  write end of the pipe actually closes. *)

  result := DuplicateHandle(
    GetCurrentProcess, // source process
    hWritePipe,        // handle to duplicate
    GetCurrentProcess, // destination process
    @hWritePipe2,      // new handle, used as stderr by child
    0,                 // new access flags - ignored since DUPLICATE_SAME_ACCESS
    TRUE,              // it's inheritable
    DUPLICATE_SAME_ACCESS);
  if not result then
    exit;

  // Set up the STARTUPINFO structure for the CreateProcess() call
  FillChar(si, SizeOf(si), Chr(0));
  si.cb := SizeOf(si);

  // Set up the command-line buffer for the child for CreateProcess()
  szArgs := cmd;

  (* If using the STARTUPINFO STARTF_USESTDHANDLES flag, be sure to
  set the CreateProcess fInheritHandles parameter too TRUE so that
  the file handles specified in the STARTUPINFO structure will be
  inheritied by the child. Note that we don't specify a standard
  input handle; the child will not inherit a valid input handle, so
  if it reads from stdin, it will encounter errors. *)

  si.hStdInput := hWritePipe2; // hStdInput needs a valid handle in case it is checked by the child
  si.hStdOutput := hWritePipe; // write end of the pipe
  si.hStdError := hWritePipe2; // duplicate of write end of the pipe
  si.dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;

  // Now create the child process, inheriting handles

  result := CreateProcess(
    nil,           // filename
    PChar(szArgs), // full command line for child
    nil,           // process security descriptor
    nil,           // thread security descriptor
    true,          // inherit handles? Also use if STARTF_USESTDHANDLES
    0,             // creation flags
    nil,           // inherited environment address
    nil,           // startup dir; NULL = start in current
    si,            // pointer to startup info (input)
    pInf);         // pointer to process info (output)
  if not result then
    exit;

  (* We can close the returned child process handle and thread
  handle as we won't be needing them; you could, however, wait on
  the process handle to wait until the child process terminates. *)

  CloseHandle(pInf.hThread);
  CloseHandle(pInf.hProcess);

  (* We need to close our instances of the inheritable pipe write
  handle now that it's been inherited so that all open handles to
  the pipe are closed when the child process ends and closes its
  handles to the pipe. *)

  result := CloseHandle(hWritePipe);
  if not result then
    exit;
  result := CloseHandle(hWritePipe2);
  if not result then
    exit;

  (* read from the pipe until we get an ERROR_BROKEN_PIPE *)
  while true do
  begin
    rsOutPut := '';
    result := ReadFile(hReadPipe,  // read handle
        chReadBuffer,              // buffer for incoming data
        SizeOf(chReadBuffer),      // number of bytes to read
        cchReadBuffer,             // number of bytes actually read
        nil);                      // no overlapped reading
    if result and (cchReadBuffer > 0) and not quiet then
    begin
      for i := 0 to cchReadBuffer - 1 do
        rsOutPut := rsOutPut + chReadBuffer[i];
      printf(rsOutPut);
    end;

    if not result and (GetLastError = ERROR_BROKEN_PIPE) then
      break;  // child has died

    if not result then
      exit;
  end;
  // close the trace file, pipe handles
  CloseHandle(hReadPipe);
  result := true;
end;

end.
