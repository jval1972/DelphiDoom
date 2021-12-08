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
//  MultiThreading Utility functions
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit mt_utils;

interface

uses
  d_delphi,
  i_threads;

procedure MT_Init;

procedure MT_ShutDown;

procedure MT_ZeroMemory(const dest0: pointer;
  const count0: integer; threadsuse: integer = 0);

procedure MT_memset(const dest0: pointer; const val: integer;
  const count0: integer; threadsuse: integer = 0);

procedure MT_memseti(const dest0: pointer; const val: integer;
  const count0: integer; threadsuse: integer = 0);

procedure MT_memcpy(const dest0: pointer; const src0: pointer;
  const count0: integer; threadsuse: integer = 0);

procedure MT_Execute(
  const func1: threadfunc_t; const parms1: pointer;
  const func2: threadfunc_t = nil; const parms2: pointer = nil
  ); overload;

procedure MT_Execute(
  const func1: threadfunc_t; const parms1: pointer;
  const func2: threadfunc_t; const parms2: pointer;
  const func3: threadfunc_t; const parms3: pointer;
  const func4: threadfunc_t = nil; const parms4: pointer = nil
  ); overload;

procedure MT_Execute(
  const func1: threadfunc_t; const parms1: pointer;
  const func2: threadfunc_t; const parms2: pointer;
  const func3: threadfunc_t; const parms3: pointer;
  const func4: threadfunc_t; const parms4: pointer;
  const func5: threadfunc_t; const parms5: pointer;
  const func6: threadfunc_t = nil; const parms6: pointer = nil
  ); overload;

procedure MT_Execute(
  const func1: threadfunc_t; const parms1: pointer;
  const func2: threadfunc_t; const parms2: pointer;
  const func3: threadfunc_t; const parms3: pointer;
  const func4: threadfunc_t; const parms4: pointer;
  const func5: threadfunc_t; const parms5: pointer;
  const func6: threadfunc_t; const parms6: pointer;
  const func7: threadfunc_t; const parms7: pointer;
  const func8: threadfunc_t = nil; const parms8: pointer = nil
  ); overload;

procedure MT_Execute(
  const func1: threadfunc_t; const parms1: pointer;
  const func2: threadfunc_t; const parms2: pointer;
  const func3: threadfunc_t; const parms3: pointer;
  const func4: threadfunc_t; const parms4: pointer;
  const func5: threadfunc_t; const parms5: pointer;
  const func6: threadfunc_t; const parms6: pointer;
  const func7: threadfunc_t; const parms7: pointer;
  const func8: threadfunc_t; const parms8: pointer;
  const func9: threadfunc_t; const parms9: pointer;
  const func10: threadfunc_t = nil; const parms10: pointer = nil;
  const func11: threadfunc_t = nil; const parms11: pointer = nil;
  const func12: threadfunc_t = nil; const parms12: pointer = nil
  ); overload;

procedure MT_Execute(
  const func1: threadfunc_t; const parms1: pointer;
  const func2: threadfunc_t; const parms2: pointer;
  const func3: threadfunc_t; const parms3: pointer;
  const func4: threadfunc_t; const parms4: pointer;
  const func5: threadfunc_t; const parms5: pointer;
  const func6: threadfunc_t; const parms6: pointer;
  const func7: threadfunc_t; const parms7: pointer;
  const func8: threadfunc_t; const parms8: pointer;
  const func9: threadfunc_t; const parms9: pointer;
  const func10: threadfunc_t; const parms10: pointer;
  const func11: threadfunc_t; const parms11: pointer;
  const func12: threadfunc_t; const parms12: pointer;
  const func13: threadfunc_t; const parms13: pointer;
  const func14: threadfunc_t = nil; const parms14: pointer = nil;
  const func15: threadfunc_t = nil; const parms15: pointer = nil;
  const func16: threadfunc_t = nil; const parms16: pointer = nil;
  const func17: threadfunc_t = nil; const parms17: pointer = nil
  ); overload;

procedure MT_Execute4(
  const func1: threadfunc_t; const parms1: pointer;
  const func2: threadfunc_t; const parms2: pointer;
  const func3: threadfunc_t; const parms3: pointer;
  const func4: threadfunc_t; const parms4: pointer
  );

procedure MT_Execute6(
  const func1: threadfunc_t; const parms1: pointer;
  const func2: threadfunc_t; const parms2: pointer;
  const func3: threadfunc_t; const parms3: pointer;
  const func4: threadfunc_t; const parms4: pointer;
  const func5: threadfunc_t; const parms5: pointer;
  const func6: threadfunc_t; const parms6: pointer
  );

procedure MT_Execute8(
  const func1: threadfunc_t; const parms1: pointer;
  const func2: threadfunc_t; const parms2: pointer;
  const func3: threadfunc_t; const parms3: pointer;
  const func4: threadfunc_t; const parms4: pointer;
  const func5: threadfunc_t; const parms5: pointer;
  const func6: threadfunc_t; const parms6: pointer;
  const func7: threadfunc_t; const parms7: pointer;
  const func8: threadfunc_t; const parms8: pointer
  );

procedure MT_Execute12(
  const func1: threadfunc_t; const parms1: pointer;
  const func2: threadfunc_t; const parms2: pointer;
  const func3: threadfunc_t; const parms3: pointer;
  const func4: threadfunc_t; const parms4: pointer;
  const func5: threadfunc_t; const parms5: pointer;
  const func6: threadfunc_t; const parms6: pointer;
  const func7: threadfunc_t; const parms7: pointer;
  const func8: threadfunc_t; const parms8: pointer;
  const func9: threadfunc_t; const parms9: pointer;
  const func10: threadfunc_t; const parms10: pointer;
  const func11: threadfunc_t; const parms11: pointer;
  const func12: threadfunc_t; const parms12: pointer
  );

procedure MT_Execute16(
  const func1: threadfunc_t; const parms1: pointer;
  const func2: threadfunc_t; const parms2: pointer;
  const func3: threadfunc_t; const parms3: pointer;
  const func4: threadfunc_t; const parms4: pointer;
  const func5: threadfunc_t; const parms5: pointer;
  const func6: threadfunc_t; const parms6: pointer;
  const func7: threadfunc_t; const parms7: pointer;
  const func8: threadfunc_t; const parms8: pointer;
  const func9: threadfunc_t; const parms9: pointer;
  const func10: threadfunc_t; const parms10: pointer;
  const func11: threadfunc_t; const parms11: pointer;
  const func12: threadfunc_t; const parms12: pointer;
  const func13: threadfunc_t; const parms13: pointer;
  const func14: threadfunc_t; const parms14: pointer;
  const func15: threadfunc_t; const parms15: pointer;
  const func16: threadfunc_t; const parms16: pointer
  );

type
  mt_range_t = record
    start, finish: integer;
  end;

  mt_range_p = ^mt_range_t;

type
  iterator_t = record
    idx: integer;
    numidxs: integer;
    data: pointer;
  end;
  iterator_p = ^iterator_t;

procedure MT_Iterate(const func: threadfunc_t; const data: pointer;
  const nthreads: integer = 0);

// Background tasks
function MT_ScheduleTask(const proc: PProcedure): integer;

procedure MT_ExecutePendingTask(const id: integer);

procedure MT_ExecutePendingTasks;

procedure MT_WaitTask(const id: integer);

procedure MT_WaitTasks;

// JVAL: Execute code threads
const
  NUMEXECTHREADS = 64;

implementation

uses
  i_system;

var
  mt_initialized: boolean = false;

// JVAL: General purpose threads
const
  MAXGPTHREADS = 256;

var
  numgpthreads: integer;
  gp_threads: array[0..MAXGPTHREADS - 1] of TDThread;

var
  exec_threads: array[0..NUMEXECTHREADS - 1] of TDThread;

// JVAL: Tasks
const
  NUMTASKTHREADS = 8;

var
  task_threads: array[0..NUMTASKTHREADS - 1] of TDThread;

//
// MT_ZeroMemory
//
type
  zmparams_t = record
    dest: pointer;
    size: integer;
  end;
  zmparams_p = ^zmparams_t;
  zmparams_a = array[0..MAXGPTHREADS - 1] of zmparams_t;

function MT_ZeroMemory_thr(p: pointer): integer; stdcall;
begin
  ZeroMemory(zmparams_p(p).dest, zmparams_p(p).size);
  result := 1;
end;

procedure MT_ZeroMemory(const dest0: pointer;
  const count0: integer; threadsuse: integer = 0);
var
  parms: zmparams_a;
  sz: integer;
  dst: pointer;
  i: integer;
begin
  if (numgpthreads < 2) or (count0 < 1024) or not mt_initialized then
  begin
    ZeroMemory(dest0, count0);
    exit;
  end;

  if threadsuse < 2 then
  begin
    threadsuse := count0 div 1024;
    if threadsuse < 2 then
      threadsuse := 2;
  end;

  if threadsuse > numgpthreads then
    threadsuse := numgpthreads;

  sz := (count0 div threadsuse) and not 7;
  dst := dest0;
  for i := 0 to threadsuse - 1 do
  begin
    if i = threadsuse - 1 then
      ZeroMemory(dst, count0 - i * sz)
    else
    begin
      parms[i].dest := dst;
      parms[i].size := sz;
      gp_threads[i].Activate(MT_ZeroMemory_thr, @parms[i]);
      dst := pointer(integer(dst) + sz);
    end;
  end;
  for i := 0 to threadsuse - 2 do
    gp_threads[i].Wait;
end;


//
// MT_memset, MT_memseti
//
type
  msparams_t = record
    dest: pointer;
    value: integer;
    size: integer;
  end;
  msparams_p = ^msparams_t;
  msparams_a = array[0..MAXGPTHREADS - 1] of msparams_t;

function MT_memset_thr(p: pointer): integer; stdcall;
begin
  memset(msparams_p(p).dest, msparams_p(p).value, msparams_p(p).size);
  result := 1;
end;

function MT_memseti_thr(p: pointer): integer; stdcall;
begin
  memseti(msparams_p(p).dest, msparams_p(p).value, msparams_p(p).size);
  result := 1;
end;

procedure MT_memset(const dest0: pointer; const val: integer;
  const count0: integer; threadsuse: integer = 0);
var
  parms: msparams_a;
  sz: integer;
  dst: pointer;
  i: integer;
begin
  if (numgpthreads < 2) or (count0 < 1024) or not mt_initialized then
  begin
    memset(dest0, val, count0);
    exit;
  end;

  if threadsuse < 2 then
  begin
    threadsuse := count0 div 1024;
    if threadsuse < 2 then
      threadsuse := 2;
  end;

  if threadsuse > numgpthreads then
    threadsuse := numgpthreads;

  sz := (count0 div threadsuse) and not 7;
  dst := dest0;
  for i := 0 to threadsuse - 1 do
  begin
    if i = threadsuse - 1 then
      memset(dst, val, count0 - i * sz)
    else
    begin
      parms[i].dest := dst;
      parms[i].value := val;
      parms[i].size := sz;
      gp_threads[i].Activate(MT_memset_thr, @parms[i]);
      dst := pointer(integer(dst) + sz);
    end;
  end;
  for i := 0 to threadsuse - 2 do
    gp_threads[i].Wait;
end;

procedure MT_memseti(const dest0: pointer; const val: integer;
  const count0: integer; threadsuse: integer = 0);
var
  parms: msparams_a;
  sz, sz4: integer;
  dst: pointer;
  i: integer;
begin
  if (numgpthreads < 2) or (count0 < 1024) or not mt_initialized then
  begin
    memseti(dest0, val, count0);
    exit;
  end;

  if threadsuse < 2 then
  begin
    threadsuse := count0 div 1024;
    if threadsuse < 2 then
      threadsuse := 2;
  end;

  if threadsuse > numgpthreads then
    threadsuse := numgpthreads;

  sz := (count0 div threadsuse) and not 7;
  sz4 := sz * 4;
  dst := dest0;
  for i := 0 to threadsuse - 1 do
  begin
    if i = threadsuse - 1 then
      memseti(dst, val, count0 - i * sz)
    else
    begin
      parms[i].dest := dst;
      parms[i].value := val;
      parms[i].size := sz;
      gp_threads[i].Activate(MT_memseti_thr, @parms[i]);
      dst := pointer(integer(dst) + sz4);
    end;
  end;
  for i := 0 to threadsuse - 2 do
    gp_threads[i].Wait;
end;

type
  mcparams_t = record
    dest: pointer;
    src: pointer;
    size: integer;
  end;
  mcparams_p = ^mcparams_t;
  mcparams_a = array[0..MAXGPTHREADS - 1] of mcparams_t;

function MT_memcpy_thr(p: pointer): integer; stdcall;
begin
  memcpy(mcparams_p(p).dest, mcparams_p(p).src, mcparams_p(p).size);
  result := 1;
end;

procedure MT_memcpy(const dest0: pointer; const src0: pointer;
  const count0: integer; threadsuse: integer = 0);
var
  parms: mcparams_a;
  sz: integer;
  dst, src: pointer;
  i: integer;
begin
  if (numgpthreads < 2) or (count0 < 1024) or not mt_initialized then
  begin
    memcpy(dest0, src0, count0);
    exit;
  end;

  if threadsuse < 2 then
  begin
    threadsuse := count0 div 1024;
    if threadsuse < 2 then
      threadsuse := 2;
  end;

  if threadsuse > numgpthreads then
    threadsuse := numgpthreads;

  sz := (count0 div threadsuse) and not 7;
  dst := dest0;
  src := src0;
  for i := 0 to threadsuse - 1 do
  begin
    if i = threadsuse - 1 then
      memcpy(dst, src, count0 - i * sz)
    else
    begin
      parms[i].dest := dst;
      parms[i].src := src;
      parms[i].size := sz;
      gp_threads[i].Activate(MT_memcpy_thr, @parms[i]);
      dst := pointer(integer(dst) + sz);
      src := pointer(integer(src) + sz);
    end;
  end;
  for i := 0 to threadsuse - 2 do
    gp_threads[i].Wait;
end;

procedure MT_Init;
var
  i: integer;
begin
  numgpthreads := I_GetNumCPUs;
  if numgpthreads < 2 then
    numgpthreads := 2;
  if numgpthreads > MAXGPTHREADS then
    numgpthreads := MAXGPTHREADS;
  for i := 0 to numgpthreads - 1 do
    gp_threads[i] := TDThread.Create;
  for i := 0 to NUMEXECTHREADS - 1 do
    exec_threads[i] := TDThread.Create;
  for i := 0 to NUMTASKTHREADS - 1 do
    task_threads[i] := TDThread.Create;
  mt_initialized := true;
end;

procedure MT_ShutDown;
var
  i: integer;
begin
  for i := 0 to numgpthreads - 1 do
    gp_threads[i].Free;
  for i := 0 to NUMEXECTHREADS - 1 do
    exec_threads[i].Free;
  for i := 0 to NUMTASKTHREADS - 1 do
    task_threads[i].Free;
  mt_initialized := false;
end;

var
  mt_execute_fetched: boolean = False;

procedure MT_Execute(
  const func1: threadfunc_t; const parms1: pointer;
  const func2: threadfunc_t = nil; const parms2: pointer = nil
  );
begin
  if mt_execute_fetched then
    I_Error('MT_Execute(): Invalid recoursive call.'#13#10);

  mt_execute_fetched := True;

  if @func2 <> nil then
  begin
    exec_threads[0].Activate(func2, parms2);
    if @func1 = nil then
      I_Warning('MT_Execute(): Called with null application thread function.'#13#10)
    else
      func1(parms1);
    exec_threads[0].Wait;
  end
  else if @func1 = nil then
    I_Warning('MT_Execute(): Called with null application thread function.'#13#10)
  else
    func1(parms1);

  mt_execute_fetched := False;
end;

procedure MT_Execute(
  const func1: threadfunc_t; const parms1: pointer;
  const func2: threadfunc_t; const parms2: pointer;
  const func3: threadfunc_t; const parms3: pointer;
  const func4: threadfunc_t = nil; const parms4: pointer = nil
  );
var
  nt: integer;
  i: integer;
begin
  if mt_execute_fetched then
    I_Error('MT_Execute(): Invalid recoursive call.'#13#10);

  mt_execute_fetched := True;

  nt := 0;
  if @func2 <> nil then
  begin
    exec_threads[nt].Activate(func2, parms2);
    inc(nt);
  end;
  if @func3 <> nil then
  begin
    exec_threads[nt].Activate(func3, parms3);
    inc(nt);
  end;
  if @func4 <> nil then
  begin
    exec_threads[nt].Activate(func4, parms4);
    inc(nt);
  end;
  if @func1 = nil then
    I_Warning('MT_Execute(): Called with null application thread function.'#13#10)
  else
    func1(parms1);
  for i := 0 to nt - 1 do
    exec_threads[i].Wait;
  mt_execute_fetched := False;
end;

procedure MT_Execute(
  const func1: threadfunc_t; const parms1: pointer;
  const func2: threadfunc_t; const parms2: pointer;
  const func3: threadfunc_t; const parms3: pointer;
  const func4: threadfunc_t; const parms4: pointer;
  const func5: threadfunc_t; const parms5: pointer;
  const func6: threadfunc_t = nil; const parms6: pointer = nil
  ); overload;
var
  nt: integer;
  i: integer;
begin
  if mt_execute_fetched then
    I_Error('MT_Execute(): Invalid recoursive call.'#13#10);

  mt_execute_fetched := True;

  nt := 0;
  if @func2 <> nil then
  begin
    exec_threads[nt].Activate(func2, parms2);
    inc(nt);
  end;
  if @func3 <> nil then
  begin
    exec_threads[nt].Activate(func3, parms3);
    inc(nt);
  end;
  if @func4 <> nil then
  begin
    exec_threads[nt].Activate(func4, parms4);
    inc(nt);
  end;
  if @func5 <> nil then
  begin
    exec_threads[nt].Activate(func5, parms5);
    inc(nt);
  end;
  if @func6 <> nil then
  begin
    exec_threads[nt].Activate(func6, parms6);
    inc(nt);
  end;
  if @func1 = nil then
    I_Warning('MT_Execute(): Called with null application thread function.'#13#10)
  else
    func1(parms1);
  for i := 0 to nt - 1 do
    exec_threads[i].Wait;
  mt_execute_fetched := False;
end;

procedure MT_Execute(
  const func1: threadfunc_t; const parms1: pointer;
  const func2: threadfunc_t; const parms2: pointer;
  const func3: threadfunc_t; const parms3: pointer;
  const func4: threadfunc_t; const parms4: pointer;
  const func5: threadfunc_t; const parms5: pointer;
  const func6: threadfunc_t; const parms6: pointer;
  const func7: threadfunc_t; const parms7: pointer;
  const func8: threadfunc_t = nil; const parms8: pointer = nil
  ); overload;
var
  nt: integer;
  i: integer;
begin
  if mt_execute_fetched then
    I_Error('MT_Execute(): Invalid recoursive call.'#13#10);

  mt_execute_fetched := True;

  nt := 0;
  if @func2 <> nil then
  begin
    exec_threads[nt].Activate(func2, parms2);
    inc(nt);
  end;
  if @func3 <> nil then
  begin
    exec_threads[nt].Activate(func3, parms3);
    inc(nt);
  end;
  if @func4 <> nil then
  begin
    exec_threads[nt].Activate(func4, parms4);
    inc(nt);
  end;
  if @func5 <> nil then
  begin
    exec_threads[nt].Activate(func5, parms5);
    inc(nt);
  end;
  if @func6 <> nil then
  begin
    exec_threads[nt].Activate(func6, parms6);
    inc(nt);
  end;
  if @func7 <> nil then
  begin
    exec_threads[nt].Activate(func7, parms7);
    inc(nt);
  end;
  if @func8 <> nil then
  begin
    exec_threads[nt].Activate(func8, parms8);
    inc(nt);
  end;
  if @func1 = nil then
    I_Warning('MT_Execute(): Called with null application thread function.'#13#10)
  else
    func1(parms1);
  for i := 0 to nt - 1 do
    exec_threads[i].Wait;
  mt_execute_fetched := False;
end;

procedure MT_Execute(
  const func1: threadfunc_t; const parms1: pointer;
  const func2: threadfunc_t; const parms2: pointer;
  const func3: threadfunc_t; const parms3: pointer;
  const func4: threadfunc_t; const parms4: pointer;
  const func5: threadfunc_t; const parms5: pointer;
  const func6: threadfunc_t; const parms6: pointer;
  const func7: threadfunc_t; const parms7: pointer;
  const func8: threadfunc_t; const parms8: pointer;
  const func9: threadfunc_t; const parms9: pointer;
  const func10: threadfunc_t = nil; const parms10: pointer = nil;
  const func11: threadfunc_t = nil; const parms11: pointer = nil;
  const func12: threadfunc_t = nil; const parms12: pointer = nil
  ); overload;
var
  nt: integer;
  i: integer;
begin
  if mt_execute_fetched then
    I_Error('MT_Execute(): Invalid recoursive call.'#13#10);

  mt_execute_fetched := True;

  nt := 0;
  if @func2 <> nil then
  begin
    exec_threads[nt].Activate(func2, parms2);
    inc(nt);
  end;
  if @func3 <> nil then
  begin
    exec_threads[nt].Activate(func3, parms3);
    inc(nt);
  end;
  if @func4 <> nil then
  begin
    exec_threads[nt].Activate(func4, parms4);
    inc(nt);
  end;
  if @func5 <> nil then
  begin
    exec_threads[nt].Activate(func5, parms5);
    inc(nt);
  end;
  if @func6 <> nil then
  begin
    exec_threads[nt].Activate(func6, parms6);
    inc(nt);
  end;
  if @func7 <> nil then
  begin
    exec_threads[nt].Activate(func7, parms7);
    inc(nt);
  end;
  if @func8 <> nil then
  begin
    exec_threads[nt].Activate(func8, parms8);
    inc(nt);
  end;
  if @func9 <> nil then
  begin
    exec_threads[nt].Activate(func9, parms9);
    inc(nt);
  end;
  if @func10 <> nil then
  begin
    exec_threads[nt].Activate(func10, parms10);
    inc(nt);
  end;
  if @func11 <> nil then
  begin
    exec_threads[nt].Activate(func11, parms11);
    inc(nt);
  end;
  if @func12 <> nil then
  begin
    exec_threads[nt].Activate(func12, parms12);
    inc(nt);
  end;
  if @func1 = nil then
    I_Warning('MT_Execute(): Called with null application thread function.'#13#10)
  else
    func1(parms1);
  for i := 0 to nt - 1 do
    exec_threads[i].Wait;
  mt_execute_fetched := False;
end;

procedure MT_Execute(
  const func1: threadfunc_t; const parms1: pointer;
  const func2: threadfunc_t; const parms2: pointer;
  const func3: threadfunc_t; const parms3: pointer;
  const func4: threadfunc_t; const parms4: pointer;
  const func5: threadfunc_t; const parms5: pointer;
  const func6: threadfunc_t; const parms6: pointer;
  const func7: threadfunc_t; const parms7: pointer;
  const func8: threadfunc_t; const parms8: pointer;
  const func9: threadfunc_t; const parms9: pointer;
  const func10: threadfunc_t; const parms10: pointer;
  const func11: threadfunc_t; const parms11: pointer;
  const func12: threadfunc_t; const parms12: pointer;
  const func13: threadfunc_t; const parms13: pointer;
  const func14: threadfunc_t = nil; const parms14: pointer = nil;
  const func15: threadfunc_t = nil; const parms15: pointer = nil;
  const func16: threadfunc_t = nil; const parms16: pointer = nil;
  const func17: threadfunc_t = nil; const parms17: pointer = nil
  ); overload;
var
  nt: integer;
  i: integer;
begin
  if mt_execute_fetched then
    I_Error('MT_Execute(): Invalid recoursive call.'#13#10);

  mt_execute_fetched := True;

  nt := 0;
  if @func2 <> nil then
  begin
    exec_threads[nt].Activate(func2, parms2);
    inc(nt);
  end;
  if @func3 <> nil then
  begin
    exec_threads[nt].Activate(func3, parms3);
    inc(nt);
  end;
  if @func4 <> nil then
  begin
    exec_threads[nt].Activate(func4, parms4);
    inc(nt);
  end;
  if @func5 <> nil then
  begin
    exec_threads[nt].Activate(func5, parms5);
    inc(nt);
  end;
  if @func6 <> nil then
  begin
    exec_threads[nt].Activate(func6, parms6);
    inc(nt);
  end;
  if @func7 <> nil then
  begin
    exec_threads[nt].Activate(func7, parms7);
    inc(nt);
  end;
  if @func8 <> nil then
  begin
    exec_threads[nt].Activate(func8, parms8);
    inc(nt);
  end;
  if @func9 <> nil then
  begin
    exec_threads[nt].Activate(func9, parms9);
    inc(nt);
  end;
  if @func10 <> nil then
  begin
    exec_threads[nt].Activate(func10, parms10);
    inc(nt);
  end;
  if @func11 <> nil then
  begin
    exec_threads[nt].Activate(func11, parms11);
    inc(nt);
  end;
  if @func12 <> nil then
  begin
    exec_threads[nt].Activate(func12, parms12);
    inc(nt);
  end;
  if @func13 <> nil then
  begin
    exec_threads[nt].Activate(func13, parms13);
    inc(nt);
  end;
  if @func14 <> nil then
  begin
    exec_threads[nt].Activate(func14, parms14);
    inc(nt);
  end;
  if @func15 <> nil then
  begin
    exec_threads[nt].Activate(func15, parms15);
    inc(nt);
  end;
  if @func16 <> nil then
  begin
    exec_threads[nt].Activate(func16, parms16);
    inc(nt);
  end;
  if @func17 <> nil then
  begin
    exec_threads[nt].Activate(func17, parms17);
    inc(nt);
  end;
  if @func1 = nil then
    I_Warning('MT_Execute(): Called with null application thread function.'#13#10)
  else
    func1(parms1);
  for i := 0 to nt - 1 do
    exec_threads[i].Wait;
  mt_execute_fetched := False;
end;

procedure MT_Execute4(
  const func1: threadfunc_t; const parms1: pointer;
  const func2: threadfunc_t; const parms2: pointer;
  const func3: threadfunc_t; const parms3: pointer;
  const func4: threadfunc_t; const parms4: pointer
  );
begin
  if mt_execute_fetched then
    I_Error('MT_Execute(): Invalid recoursive call.'#13#10);

  mt_execute_fetched := True;

  exec_threads[0].Activate(func2, parms2);
  exec_threads[1].Activate(func3, parms3);
  exec_threads[2].Activate(func4, parms4);
  func1(parms1);

  exec_threads[0].Wait;
  exec_threads[1].Wait;
  exec_threads[2].Wait;

  mt_execute_fetched := False;
end;

procedure MT_Execute6(
  const func1: threadfunc_t; const parms1: pointer;
  const func2: threadfunc_t; const parms2: pointer;
  const func3: threadfunc_t; const parms3: pointer;
  const func4: threadfunc_t; const parms4: pointer;
  const func5: threadfunc_t; const parms5: pointer;
  const func6: threadfunc_t; const parms6: pointer
  );
begin
  if mt_execute_fetched then
    I_Error('MT_Execute(): Invalid recoursive call.'#13#10);

  mt_execute_fetched := True;

  exec_threads[0].Activate(func2, parms2);
  exec_threads[1].Activate(func3, parms3);
  exec_threads[2].Activate(func4, parms4);
  exec_threads[3].Activate(func5, parms5);
  exec_threads[4].Activate(func6, parms6);
  func1(parms1);

  exec_threads[0].Wait;
  exec_threads[1].Wait;
  exec_threads[2].Wait;
  exec_threads[3].Wait;
  exec_threads[4].Wait;

  mt_execute_fetched := False;
end;

procedure MT_Execute8(
  const func1: threadfunc_t; const parms1: pointer;
  const func2: threadfunc_t; const parms2: pointer;
  const func3: threadfunc_t; const parms3: pointer;
  const func4: threadfunc_t; const parms4: pointer;
  const func5: threadfunc_t; const parms5: pointer;
  const func6: threadfunc_t; const parms6: pointer;
  const func7: threadfunc_t; const parms7: pointer;
  const func8: threadfunc_t; const parms8: pointer
  );
begin
  if mt_execute_fetched then
    I_Error('MT_Execute(): Invalid recoursive call.'#13#10);

  mt_execute_fetched := True;

  exec_threads[0].Activate(func2, parms2);
  exec_threads[1].Activate(func3, parms3);
  exec_threads[2].Activate(func4, parms4);
  exec_threads[3].Activate(func5, parms5);
  exec_threads[4].Activate(func6, parms6);
  exec_threads[5].Activate(func7, parms7);
  exec_threads[6].Activate(func8, parms8);
  func1(parms1);

  exec_threads[0].Wait;
  exec_threads[1].Wait;
  exec_threads[2].Wait;
  exec_threads[3].Wait;
  exec_threads[4].Wait;
  exec_threads[5].Wait;
  exec_threads[6].Wait;

  mt_execute_fetched := False;
end;

procedure MT_Execute12(
  const func1: threadfunc_t; const parms1: pointer;
  const func2: threadfunc_t; const parms2: pointer;
  const func3: threadfunc_t; const parms3: pointer;
  const func4: threadfunc_t; const parms4: pointer;
  const func5: threadfunc_t; const parms5: pointer;
  const func6: threadfunc_t; const parms6: pointer;
  const func7: threadfunc_t; const parms7: pointer;
  const func8: threadfunc_t; const parms8: pointer;
  const func9: threadfunc_t; const parms9: pointer;
  const func10: threadfunc_t; const parms10: pointer;
  const func11: threadfunc_t; const parms11: pointer;
  const func12: threadfunc_t; const parms12: pointer
  );
begin
  if mt_execute_fetched then
    I_Error('MT_Execute(): Invalid recoursive call.'#13#10);

  mt_execute_fetched := True;

  exec_threads[0].Activate(func2, parms2);
  exec_threads[1].Activate(func3, parms3);
  exec_threads[2].Activate(func4, parms4);
  exec_threads[3].Activate(func5, parms5);
  exec_threads[4].Activate(func6, parms6);
  exec_threads[5].Activate(func7, parms7);
  exec_threads[6].Activate(func8, parms8);
  exec_threads[7].Activate(func9, parms9);
  exec_threads[8].Activate(func10, parms10);
  exec_threads[9].Activate(func11, parms11);
  exec_threads[10].Activate(func12, parms12);
  func1(parms1);

  exec_threads[0].Wait;
  exec_threads[1].Wait;
  exec_threads[2].Wait;
  exec_threads[3].Wait;
  exec_threads[4].Wait;
  exec_threads[5].Wait;
  exec_threads[6].Wait;
  exec_threads[7].Wait;
  exec_threads[8].Wait;
  exec_threads[9].Wait;
  exec_threads[10].Wait;

  mt_execute_fetched := False;
end;

procedure MT_Execute16(
  const func1: threadfunc_t; const parms1: pointer;
  const func2: threadfunc_t; const parms2: pointer;
  const func3: threadfunc_t; const parms3: pointer;
  const func4: threadfunc_t; const parms4: pointer;
  const func5: threadfunc_t; const parms5: pointer;
  const func6: threadfunc_t; const parms6: pointer;
  const func7: threadfunc_t; const parms7: pointer;
  const func8: threadfunc_t; const parms8: pointer;
  const func9: threadfunc_t; const parms9: pointer;
  const func10: threadfunc_t; const parms10: pointer;
  const func11: threadfunc_t; const parms11: pointer;
  const func12: threadfunc_t; const parms12: pointer;
  const func13: threadfunc_t; const parms13: pointer;
  const func14: threadfunc_t; const parms14: pointer;
  const func15: threadfunc_t; const parms15: pointer;
  const func16: threadfunc_t; const parms16: pointer
  );
begin
  if mt_execute_fetched then
    I_Error('MT_Execute(): Invalid recoursive call.'#13#10);

  mt_execute_fetched := True;

  exec_threads[0].Activate(func2, parms2);
  exec_threads[1].Activate(func3, parms3);
  exec_threads[2].Activate(func4, parms4);
  exec_threads[3].Activate(func5, parms5);
  exec_threads[4].Activate(func6, parms6);
  exec_threads[5].Activate(func7, parms7);
  exec_threads[6].Activate(func8, parms8);
  exec_threads[7].Activate(func9, parms9);
  exec_threads[8].Activate(func10, parms10);
  exec_threads[9].Activate(func11, parms11);
  exec_threads[10].Activate(func12, parms12);
  exec_threads[11].Activate(func13, parms13);
  exec_threads[12].Activate(func14, parms14);
  exec_threads[13].Activate(func15, parms15);
  exec_threads[14].Activate(func16, parms16);
  func1(parms1);

  exec_threads[0].Wait;
  exec_threads[1].Wait;
  exec_threads[2].Wait;
  exec_threads[3].Wait;
  exec_threads[4].Wait;
  exec_threads[5].Wait;
  exec_threads[6].Wait;
  exec_threads[7].Wait;
  exec_threads[8].Wait;
  exec_threads[9].Wait;
  exec_threads[10].Wait;
  exec_threads[11].Wait;
  exec_threads[12].Wait;
  exec_threads[13].Wait;
  exec_threads[14].Wait;

  mt_execute_fetched := False;
end;

procedure MT_Iterate(const func: threadfunc_t; const data: pointer;
  const nthreads: integer = 0);
var
  parms: array[0..NUMEXECTHREADS] of iterator_t;
  i, nt: integer;
begin
  if mt_execute_fetched then
    I_Error('MT_Iterate(): Invalid recoursive call.'#13#10);

  mt_execute_fetched := True;

  if nthreads > 0 then
    nt := nthreads
  else
    nt := I_GetNumCPUs;
  if nt < 2 then
    nt := 2
  else if nt > NUMEXECTHREADS + 1 then
    nt := NUMEXECTHREADS + 1;

  for i := 0 to nt - 2 do
  begin
    parms[i].idx := i;
    parms[i].numidxs := nt;
    parms[i].data := data;
    exec_threads[i].Activate(func, @parms[i]);
  end;
  parms[nt - 1].idx := nt - 1;
  parms[nt - 1].numidxs := nt;
  parms[nt - 1].data := data;
  func(@parms[nt - 1]);

  for i := 0 to nt - 2 do
    exec_threads[i].Wait;

  mt_execute_fetched := False;
end;

type
  taskinfo_t = record
    id: integer;
    proc: PProcedure;
  end;
  Ptaskinfo_t = ^taskinfo_t;

var
  tasks: array[0..NUMTASKTHREADS - 1] of taskinfo_t;

function _execute_task(p: pointer): integer; stdcall;
var
  pt: Ptaskinfo_t;
begin
  pt := p;
  pt.proc;
  result := pt.id;
  pt.id := -1;
  pt.proc := nil;
end;

function MT_ScheduleTask(const proc: PProcedure): integer;
var
  i: integer;
begin
  for i := 0 to NUMTASKTHREADS - 1 do
    if not Assigned(tasks[i].proc) then
    begin
      tasks[i].id := i;
      tasks[i].proc := proc;
      result := i;
      exit;
    end;
  proc;
  result := -1;
end;

procedure MT_ExecutePendingTask(const id: integer);
begin
  if Assigned(tasks[id].proc) then
    task_threads[id].Activate(_execute_task, @tasks[id]);
end;

procedure MT_ExecutePendingTasks;
var
  i: integer;
begin
  for i := 0 to NUMTASKTHREADS - 1 do
    if Assigned(tasks[i].proc) then
      task_threads[i].Activate(_execute_task, @tasks[i]);
end;


procedure MT_WaitTask(const id: integer);
begin
  if (id < 0) or (id >= NUMTASKTHREADS) then
    exit;
  if Assigned(tasks[id].proc) then
    task_threads[id].Wait;
end;

procedure MT_WaitTasks;
var
  i: integer;
begin
  for i := 0 to NUMTASKTHREADS - 1 do
    if Assigned(tasks[i].proc) then
      task_threads[i].Wait;
end;

end.
