//------------------------------------------------------------------------------
//
//  DelphiDoom: A modified and improved DOOM engine for Windows
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 2004-2017 by Jim Valavanis
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
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit mt_utils;

interface

uses
  i_threads;

procedure MT_Init;

procedure MT_ShutDown;

procedure MT_ZeroMemory(const dest0: pointer; const count0: integer);

procedure MT_memset(const dest0: pointer; const val: integer; const count0: integer);

procedure MT_memseti(const dest0: pointer; const val: integer; const count0: integer);

procedure MT_memcpy(const dest0: pointer; const src0: pointer; const count0: integer);

procedure MT_Execute(
  const func1: threadfunc_t; const parms1: pointer;
  const func2: threadfunc_t; const parms2: pointer;
  const func3: threadfunc_t = nil; const parms3: pointer = nil;
  const func4: threadfunc_t = nil; const parms4: pointer = nil
  );

implementation

uses
  d_delphi,
  i_system;

var
  mt_initialized: boolean = false;

// JVAL: General purpose threads
const
  MAXGPTHREADS = 256;

var
  numgpthreads: integer;
  gp_threads: array[0..MAXGPTHREADS - 1] of TDThread;

// JVAL: Execute code threads
const
  NUMEXECTHREADS = 4;

var
  exec_threads: array[0..NUMEXECTHREADS - 1] of TDThread;

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

procedure MT_ZeroMemory(const dest0: pointer; const count0: integer);
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

  sz := (count0 div numgpthreads) and not 7;
  dst := dest0;
  for i := 0 to numgpthreads - 1 do
  begin
    if i = numgpthreads - 1 then
      ZeroMemory(dst, count0 - i * sz)
    else
    begin
      parms[i].dest := dst;
      parms[i].size := sz;
      gp_threads[i].Activate(MT_ZeroMemory_thr, @parms[i]);
      dst := pointer(integer(dst) + sz);
    end;
  end;
  for i := 0 to numgpthreads - 2 do
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

procedure MT_memset(const dest0: pointer; const val: integer; const count0: integer);
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

  sz := (count0 div numgpthreads) and not 7;
  dst := dest0;
  for i := 0 to numgpthreads - 1 do
  begin
    if i = numgpthreads - 1 then
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
  for i := 0 to numgpthreads - 2 do
    gp_threads[i].Wait;
end;

procedure MT_memseti(const dest0: pointer; const val: integer; const count0: integer);
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

  sz := (count0 div numgpthreads) and not 7;
  sz4 := sz * 4;
  dst := dest0;
  for i := 0 to numgpthreads - 1 do
  begin
    if i = numgpthreads - 1 then
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
  for i := 0 to numgpthreads - 2 do
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

procedure MT_memcpy(const dest0: pointer; const src0: pointer; const count0: integer);
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

  sz := (count0 div numgpthreads) and not 7;
  dst := dest0;
  src := src0;
  for i := 0 to numgpthreads - 1 do
  begin
    if i = numgpthreads - 1 then
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
  for i := 0 to numgpthreads - 2 do
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
  mt_initialized := false;
end;

procedure MT_Execute(
  const func1: threadfunc_t; const parms1: pointer;
  const func2: threadfunc_t; const parms2: pointer;
  const func3: threadfunc_t = nil; const parms3: pointer = nil;
  const func4: threadfunc_t = nil; const parms4: pointer = nil
  );
var
  nt: integer;
  i: integer;
begin
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
end;

end.
