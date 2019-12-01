//------------------------------------------------------------------------------
//
//  DelphiDoom: A modified and improved DOOM engine for Windows
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 2004-2016 by Jim Valavanis
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
//  Depth buffer for the software renderer.
//
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit r_depthbuffer;

interface

uses
  d_delphi,
  m_fixed;

var
// db_distance: the depth buffer value
// Lower value means far away
// Depth buffer initialization -> Set buffer to 0
// Draw with depth buffer chech ->
//  if (current depth > depth buffer) { draw(); updatedepthbuffer();}
  db_distance: LongWord;
  depthbufferactive: boolean;

procedure R_DrawSpanToDepthBuffer;

procedure R_DrawColumnWithDepthBufferCheck(const cfunc: Pprocedure);

procedure R_DrawBatchColumnWithDepthBufferCheck(const cfunc: Pprocedure); overload;

procedure R_DrawBatchColumnWithDepthBufferCheck(const cfunc: Pprocedure; const depth: LongWord); overload;

function R_DepthBufferAt(const x, y: integer): LongWord;

procedure R_InitDepthBuffer;

procedure R_ShutDownDepthBuffer;

procedure R_StartDepthBuffer;

procedure R_StopDepthBuffer;

implementation

uses
  doomdef,
  i_threads,
  mt_utils,
  r_draw,
  r_column,
  r_batchcolumn,
  r_span,
  v_data;

const
  NUMDEPTHBUFFERS = 1;

var
  depthbuffer: array[0..NUMDEPTHBUFFERS - 1] of PLongWordArray;
  depthbuffertic: Byte = 255;
  lastdepthbuffertic: Byte = 255;
  depthbufferthread: TDThread;
  ylookupdb: array[0..NUMDEPTHBUFFERS - 1] of array[0..MAXHEIGHT - 1] of PLongWordArray;
  curbuffer: integer = NUMDEPTHBUFFERS - 1;


procedure R_DrawSpanToDepthBuffer;
var
  destl: PLongWord;
begin
  destl := @((ylookupdb[curbuffer][ds_y]^)[columnofs[ds_x1]]);
  memseti(destl, db_distance, ds_x2 - ds_x1 + 1);
end;

procedure R_DrawColumnWithDepthBufferCheck(const cfunc: Pprocedure);
// db_distance := rw_scale/fracunit;
var
  count: integer;
  i: integer;
  destl: PLongWord;
  l: LongWord;
  old_yh, old_yl: integer;
  start_y: integer;
  check, oldcheck: boolean;
  swidth: LongWord;
begin
  count := dc_yh - dc_yl;

  if count < 0 then
    exit;

  old_yh := dc_yh;
  old_yl := dc_yl;

  destl := @((ylookupdb[curbuffer][dc_yl]^)[columnofs[dc_x]]);

  swidth := SCREENWIDTH;
  db_distance := trunc((FRACUNIT / dc_iscale) * FRACUNIT);
  l := db_distance;
  start_y := old_yl;
  oldcheck := false;
  for i := old_yl to old_yh do
  begin
    check := destl^ <= l;
    if check then
    begin
      destl^ := l;
      if not oldcheck then
        start_y := i;
    end
    else if oldcheck then
    begin
      dc_yl := start_y;
      dc_yh := i - 1;
      cfunc;
    end;
    oldcheck := check;

    inc(destl, swidth);
  end;
  if oldcheck then
  begin
    dc_yl := start_y;
    dc_yh := old_yh;
    cfunc;
  end;

  dc_yh := old_yh;
  dc_yl := old_yl;

end;

procedure R_DrawBatchColumnWithDepthBufferCheck(const cfunc: Pprocedure); overload;
begin
  R_DrawBatchColumnWithDepthBufferCheck(cfunc, trunc((FRACUNIT / dc_iscale) * FRACUNIT));
end;

procedure R_DrawBatchColumnWithDepthBufferCheck(const cfunc: Pprocedure; const depth: LongWord); overload;
// db_distance := rw_scale/fracunit;
var
  count: integer;
  i: integer;
  destl: PLongWord;
  l: LongWord;
  old_yh, old_yl, old_x, old_n: integer;
  start_y: integer;
  check, oldcheck: boolean;
  xx: integer;
  swidth: LongWord;
begin
  count := dc_yh - dc_yl;

  if count < 0 then
    exit;

  old_yh := dc_yh;
  old_yl := dc_yl;
  old_x := dc_x;
  old_n := num_batch_columns;
  num_batch_columns := 1; // Ouch!

  swidth := SCREENWIDTH;
  db_distance := depth;
  l := db_distance;

  for xx := old_x to old_x + old_n do
  begin
    destl := @((ylookupdb[curbuffer][old_yl]^)[columnofs[xx]]);
    start_y := old_yl;
    oldcheck := false;
    for i := old_yl to old_yh do
    begin
      check := destl^ <= l;
      if check then
      begin
        destl^ := l;
        if not oldcheck then
          start_y := i;
      end
      else if oldcheck then
      begin
        dc_yl := start_y;
        dc_yh := i - 1;
        cfunc;
      end;
      oldcheck := check;

      inc(destl, swidth);
    end;
    if oldcheck then
    begin
      dc_yl := start_y;
      dc_yh := old_yh;
      cfunc;
    end;
    inc(dc_x);
  end;

  dc_yh := old_yh;
  dc_yl := old_yl;
  dc_x := old_x;
  num_batch_columns := old_n;

end;

function R_DepthBufferAt(const x, y: integer): LongWord;
begin
  result := PLongWord(@((ylookupdb[curbuffer][y]^)[columnofs[x]]))^;
end;

var
  dbsize: integer;
  depthbuffertmp: array[0..NUMDEPTHBUFFERS - 1] of pointer;

type
  dbclear_t = record
    curbuffer: integer;
  end;
  Pdbclear_t = ^dbclear_t;

// JVAL: Thread function to clean depth buffer in the background
function _cleardb_thread_worker(parms: Pdbclear_t): integer; stdcall;
begin
  ZeroMemory(depthbuffer[parms.curbuffer], SCREENWIDTH * viewheight * SizeOf(LongWord));
  result := 0;
end;

procedure R_InitDepthBuffer;
var
  i: integer;
begin
  dbsize := SCREENWIDTH * (SCREENHEIGHT + 20) * SizeOf(LongWord) and not (4095);
  for i := 0 to NUMDEPTHBUFFERS - 1 do
  begin
    depthbuffer[i] := mallocA(dbsize, $10000, depthbuffertmp[i]); // JVAL: Memory padding may increase performance until 4%
    ZeroMemory(depthbuffer[i], SCREENWIDTH * SCREENHEIGHT * SizeOf(LongWord));
  end;
  depthbufferthread := TDThread.Create(@_cleardb_thread_worker);
  depthbufferactive := false;
end;

procedure R_WaitDepthBuffer;
begin
  depthbufferthread.Wait;
end;

procedure R_ShutDownDepthBuffer;
var
  i: integer;
begin
  R_WaitDepthBuffer;
  depthbufferthread.Free;
  for i := 0 to NUMDEPTHBUFFERS - 1 do
    memfree(Pointer(depthbuffertmp[i]), dbsize + $10000);
end;

// Called in each render tic before we start depth buffer
var
  lastviewwindowy: Integer = -1;
  lastviewheight: Integer = -1;

procedure R_StartDepthBuffer;
var
  i, n: integer;
begin
  if depthbufferactive then // JVAL: Slopes
    exit;
  R_WaitDepthBuffer;  // Wait thread to clear depthbuffer
  if (lastviewwindowy <> viewwindowy) or (lastviewheight <> viewheight) then
  begin
    lastviewwindowy := viewwindowy;
    lastviewheight := viewheight;
    for n := 0 to NUMDEPTHBUFFERS - 1 do
      for i := 0 to viewheight - 1 do
        ylookupdb[n][i] := PLongWordArray(@depthbuffer[n][(i) * SCREENWIDTH]);
        // Draw depth buffer to screen
        // ylookupdb[n][i] := PLongWordArray(@screen32[(i + viewwindowy) * SCREENWIDTH]);
  end;

  Inc(depthbuffertic);
  curbuffer := (curbuffer + 1) mod NUMDEPTHBUFFERS;
  depthbufferactive := true;
end;

var
  parms: dbclear_t;

procedure R_StopDepthBuffer;
begin
  depthbufferactive := false;
  if depthbuffertic  <> lastdepthbuffertic then
  begin
    lastdepthbuffertic := depthbuffertic;
    parms.curbuffer := (curbuffer + 1) mod NUMDEPTHBUFFERS;
    depthbufferthread.Activate(@parms);
  end;
end;

end.
