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

unit i_threads;

interface

type
  threadfunc_t = function(p: pointer): integer; stdcall;

type
  TDThread = class;

  threadinfo_t = record
    thread: TDThread;
  end;
  Pthreadinfo_t = ^threadinfo_t;

  TDThread = class
  private
    suspended: boolean;
  protected
    ffunc: threadfunc_t;
    fparms: Pointer;
    fid: Integer;
    info: threadinfo_t;
    fstatus: integer;
    fterminated: boolean;
    frunning: boolean;
  public
    constructor Create(const func: threadfunc_t = nil);
    destructor Destroy; override;
    procedure Activate(const parms: pointer); overload;
    procedure Activate(const func: threadfunc_t; const parms: pointer); overload;
    procedure Wait;
    function CheckJobDone: Boolean;
    function IsIdle: Boolean;
  end;

const
  THR_DEAD = 0;
  THR_ACTIVE = 1;
  THR_IDLE = 2;

var
  dotestactivethreads: boolean = true;

//==============================================================================
//
// TestActiveThreads
//
//==============================================================================
procedure TestActiveThreads;

//==============================================================================
//
// ThreadInc
//
//==============================================================================
function ThreadInc(var x: Integer): Integer;

//==============================================================================
//
// ThreadDec
//
//==============================================================================
function ThreadDec(var x: Integer): Integer;

//==============================================================================
//
// ThreadSet
//
//==============================================================================
function ThreadSet(var x: Integer; const newvalue: Integer): Integer;

implementation

uses
  d_delphi,
  Windows,
  i_system;

//==============================================================================
//
// ThreadWorker
//
//==============================================================================
function ThreadWorker(p: Pointer): integer; stdcall;
var
  th: TDThread;
begin
  result := 0;
  th := Pthreadinfo_t(p).thread;
  while true do
  begin
    while (th.fstatus = THR_IDLE) and not th.fterminated do
    begin
      I_Sleep(0);
    end;
    if th.fterminated then
      exit;
    th.frunning := true;
    th.ffunc(th.fparms);
    th.frunning := false;
    if th.fterminated then
      exit;
    th.fstatus := THR_IDLE;
  end;
end;

var
  threadpool: TDPointerList;

//==============================================================================
//
// TDThread.Create
//
//==============================================================================
constructor TDThread.Create(const func: threadfunc_t = nil);
begin
  fterminated := false;
  ffunc := func;
  fparms := nil;
  fstatus := THR_IDLE;
  frunning := false;
  info.thread := Self;
  fid := I_CreateProcess(@ThreadWorker, @info, true);
  suspended := true;
  threadpool.Add(self);
end;

//==============================================================================
//
// TDThread.Destroy
//
//==============================================================================
destructor TDThread.Destroy;
var
  id: Integer;
begin
  while frunning do
    I_Sleep(0);
  fterminated := true;
  fstatus := THR_DEAD;
  I_WaitForProcess(fid, 1);
  id := threadpool.IndexOf(self);
  if id >= 0 then
    threadpool.Delete(id);
  Inherited Destroy;
end;

//==============================================================================
// TDThread.Activate
//
// JVAL: Should check for fstatus, but it is not called while active
//
//==============================================================================
procedure TDThread.Activate(const parms: pointer);
begin
  if not Assigned(ffunc) then
    I_Error('TDThread.Activate(): Null function pointer');
  fparms := parms;
  fstatus := THR_ACTIVE;
  suspended := false;
  ResumeThread(fid);
end;

//==============================================================================
//
// TDThread.Activate
//
//==============================================================================
procedure TDThread.Activate(const func: threadfunc_t; const parms: pointer);
begin
  ffunc := func;
  Activate(parms);
end;

//==============================================================================
//
// TDThread.Wait
//
//==============================================================================
procedure TDThread.Wait;
begin
  if suspended then
    Exit;

  while fstatus = THR_ACTIVE do
  begin
    //I_Sleep(0);
  end;
  suspended := true;
  SuspendThread(fid);
end;

//==============================================================================
//
// TDThread.CheckJobDone
//
//==============================================================================
function TDThread.CheckJobDone: Boolean;
begin
  if fstatus = THR_IDLE then
  begin
    if not suspended then
    begin
      suspended := true;
      SuspendThread(fid);
    end;
    result := true;
  end
  else
    result := false;
end;

//==============================================================================
//
// TDThread.IsIdle
//
//==============================================================================
function TDThread.IsIdle: Boolean;
begin
  result := fstatus = THR_IDLE;
end;

//==============================================================================
//
// TestActiveThreads
//
//==============================================================================
procedure TestActiveThreads;
var
  i: integer;
  th: TDThread;
begin
  if not dotestactivethreads then
    exit;

  for i := 0 to threadpool.Count - 1 do
  begin
    th := threadpool.Pointers[i];
    th.CheckJobDone;
  end;
end;

//==============================================================================
//
// ThreadInc
//
//==============================================================================
function ThreadInc(var x: Integer): Integer;
begin
  Result := InterlockedIncrement(x);
end;

//==============================================================================
//
// ThreadDec
//
//==============================================================================
function ThreadDec(var x: Integer): Integer;
begin
  Result := InterlockedDecrement(x);
end;

//==============================================================================
//
// ThreadSet
//
//==============================================================================
function ThreadSet(var x: Integer; const newvalue: Integer): Integer;
begin
  Result := InterlockedExchange(x, newvalue);
end;

initialization
  threadpool := TDPointerList.Create;

finalization
  threadpool.Free;

end.

