//------------------------------------------------------------------------------
//
//  DelphiDoom: A modified and improved DOOM engine for Windows
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 2004-2013 by Jim Valavanis
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
//  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
//  02111-1307, USA.
//
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : http://sourceforge.net/projects/delphidoom/
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
  public
    constructor Create(const func: threadfunc_t);
    destructor Destroy; override;
    procedure Activate(const parms: pointer);
    procedure Wait;
    function CheckJobDone: Boolean;
    function IsIdle: Boolean;
  end;

const
  THR_DEAD = 0;
  THR_ACTIVE = 1;
  THR_IDLE = 2;

implementation

uses
  d_delphi,
  Windows,
  i_system;

function ThreadWorker(p: Pointer): integer; stdcall;
begin
  Result := 0;
  while true do
  begin
    while (Pthreadinfo_t(p).thread.fstatus = THR_IDLE) and (not Pthreadinfo_t(p).thread.fterminated) do
    begin
      I_Sleep(0);
    end;
    if Pthreadinfo_t(p).thread.fterminated then
      exit;
    Pthreadinfo_t(p).thread.ffunc(Pthreadinfo_t(p).thread.fparms);
    if Pthreadinfo_t(p).thread.fterminated then
      exit;
    Pthreadinfo_t(p).thread.fstatus := THR_IDLE;
  end;
end;

constructor TDThread.Create(const func: threadfunc_t);
begin
  fterminated := false;
  ffunc := func;
  fparms := nil;
  fstatus := THR_IDLE;
  info.thread := Self;
  fid := I_CreateProcess(@ThreadWorker, @info, true);
  suspended := true;
end;

destructor TDThread.Destroy;
begin
  fterminated := true;
  fstatus := THR_DEAD;
  I_WaitForProcess(fid, 100);
  Inherited Destroy;
end;

// JVAL: Should check for fstatus, but it is not called while active 
procedure TDThread.Activate(const parms: pointer);
begin
  fparms := parms;
  fstatus := THR_ACTIVE;
  suspended := false;
  ResumeThread(fid);
end;

procedure TDThread.Wait;
begin
  if suspended then
    Exit;

  while fstatus = THR_ACTIVE do
  begin
    I_Sleep(0);
  end;
  suspended := true;
  SuspendThread(fid);
end;

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
    Result := false;
end;

function TDThread.IsIdle: Boolean;
begin
  Result := fstatus = THR_IDLE;
end;

end.

