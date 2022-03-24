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
//  Depth buffer for the software renderer.
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
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

//==============================================================================
//
// R_DrawSpanToDepthBuffer
//
//==============================================================================
procedure R_DrawSpanToDepthBuffer;

//==============================================================================
//
// R_StoreSpanToDepthBufferMT
//
//==============================================================================
procedure R_StoreSpanToDepthBufferMT; // Store span information for Multithread depthbuffer drawing

//==============================================================================
//
// R_FlashSpansToDepthBufferMT
//
//==============================================================================
procedure R_FlashSpansToDepthBufferMT; // Flash stored span information using multiple threads

//==============================================================================
//
// R_DrawColumnWithDepthBufferCheckWrite
//
//==============================================================================
procedure R_DrawColumnWithDepthBufferCheckWrite(const cfunc: Pprocedure); overload;

//==============================================================================
//
// R_DrawColumnWithDepthBufferCheckWrite
//
//==============================================================================
procedure R_DrawColumnWithDepthBufferCheckWrite(const cfunc: Pprocedure; const depth: LongWord); overload;

//==============================================================================
//
// R_DrawBatchColumnWithDepthBufferCheckWrite
//
//==============================================================================
procedure R_DrawBatchColumnWithDepthBufferCheckWrite(const cfunc: Pprocedure); overload;

//==============================================================================
//
// R_DrawBatchColumnWithDepthBufferCheckWrite
//
//==============================================================================
procedure R_DrawBatchColumnWithDepthBufferCheckWrite(const cfunc: Pprocedure; const depth: LongWord); overload;

//==============================================================================
//
// R_DrawColumnWithDepthBufferCheckOnly
//
//==============================================================================
procedure R_DrawColumnWithDepthBufferCheckOnly(const cfunc: Pprocedure); overload;

//==============================================================================
//
// R_DrawColumnWithDepthBufferCheckOnly
//
//==============================================================================
procedure R_DrawColumnWithDepthBufferCheckOnly(const cfunc: Pprocedure; const depth: LongWord); overload;

//==============================================================================
//
// R_DrawBatchColumnWithDepthBufferCheckOnly
//
//==============================================================================
procedure R_DrawBatchColumnWithDepthBufferCheckOnly(const cfunc: Pprocedure); overload;

//==============================================================================
//
// R_DrawBatchColumnWithDepthBufferCheckOnly
//
//==============================================================================
procedure R_DrawBatchColumnWithDepthBufferCheckOnly(const cfunc: Pprocedure; const depth: LongWord); overload;

//==============================================================================
//
// R_DepthBufferAt
//
//==============================================================================
function R_DepthBufferAt(const x, y: integer): LongWord;

//==============================================================================
//
// R_InitDepthBuffer
//
//==============================================================================
procedure R_InitDepthBuffer;

//==============================================================================
//
// R_ShutDownDepthBuffer
//
//==============================================================================
procedure R_ShutDownDepthBuffer;

//==============================================================================
//
// R_StartDepthBuffer
//
//==============================================================================
procedure R_StartDepthBuffer;

//==============================================================================
//
// R_StopDepthBuffer
//
//==============================================================================
procedure R_StopDepthBuffer;

var
  spandepthbufferproc: PProcedure;

implementation

uses
  doomdef,
  i_threads,
  i_system,
  mt_utils,
  r_draw,
  r_column,
  r_batchcolumn,
  r_span;

const
  NUMDEPTHBUFFERS = 1;

var
  depthbuffer: array[0..NUMDEPTHBUFFERS - 1] of PLongWordArray;
  depthbuffertic: Byte = 255;
  lastdepthbuffertic: Byte = 255;
  depthbufferthread: TDThread;
  ylookupdb: array[0..NUMDEPTHBUFFERS - 1] of array[0..MAXHEIGHT - 1] of PLongWordArray;
  curbuffer: integer = NUMDEPTHBUFFERS - 1;

//==============================================================================
//
// R_DrawSpanToDepthBuffer
//
//==============================================================================
procedure R_DrawSpanToDepthBuffer;
var
  destl: PLongWord;
begin
  destl := @((ylookupdb[curbuffer][ds_y]^)[columnofs[ds_x1]]);
  FillDWord(destl, ds_x2 - ds_x1 + 1, db_distance);
end;

type
  PdbSpanCacheInfo_t = ^dbSpanCacheInfo_t;
  dbSpanCacheInfo_t = record
    destl: PLongWord;
    distance: LongWord;
    len: integer;
    y: integer;
  end;
  dbSpanCacheInfo_tArray = array[0..$FFFF] of dbSpanCacheInfo_t;
  PdbSpanCacheInfo_tArray = ^dbSpanCacheInfo_tArray;

var
  dbspancacheinfo: PdbSpanCacheInfo_tArray = nil;
  dbspancacheinfo_size: integer = 0;
  dbspancacheinfo_realsize: integer = 0;
  dbspan_y_min: integer;
  dbspan_y_max: integer;

//==============================================================================
//
// R_GrowDBSpanCacheInfo
//
//==============================================================================
procedure R_GrowDBSpanCacheInfo;
var
  newsize: integer;
begin
  if dbspancacheinfo_size >= dbspancacheinfo_realsize then
  begin
    newsize := dbspancacheinfo_realsize + 16;
    realloc(pointer(dbspancacheinfo), dbspancacheinfo_realsize * SizeOf(dbSpanCacheInfo_t), newsize * SizeOf(dbSpanCacheInfo_t));
    dbspancacheinfo_realsize := newsize;
  end;
  inc(dbspancacheinfo_size);
end;

//==============================================================================
// R_StoreSpanToDepthBufferMT
//
// Store span information for Multithread depthbuffer drawing
//
//==============================================================================
procedure R_StoreSpanToDepthBufferMT;
var
  info: PdbSpanCacheInfo_t;
begin
  if ds_y < dbspan_y_min then
    dbspan_y_min := ds_y;
  if ds_y > dbspan_y_max then
    dbspan_y_max := ds_y;

  R_GrowDBSpanCacheInfo;
  info := @dbspancacheinfo[dbspancacheinfo_size - 1];
  info.destl := @((ylookupdb[curbuffer][ds_y]^)[columnofs[ds_x1]]);
  info.distance := db_distance;
  info.len := ds_x2 - ds_x1 + 1;
  info.y := ds_y;
end;

//==============================================================================
//
// _thr_span_db_writer
//
//==============================================================================
function _thr_span_db_writer(p: mt_range_p): integer; stdcall;
var
  i: integer;
  info: PdbSpanCacheInfo_t;
begin
  for i := 0 to dbspancacheinfo_size - 1 do
  begin
    info := @dbspancacheinfo[i];
    if (info.y >= p.start) and (info.y <= p.finish) then
      FillDWord(info.destl, info.len, info.distance);
  end;
  result := 0;
end;

//==============================================================================
// R_FlashSpansToDepthBufferMT
//
// Flash stored span information using multiple threads
//
//==============================================================================
procedure R_FlashSpansToDepthBufferMT;
var
  r1, r2, r3, r4: mt_range_t;
  r5, r6, r7, r8: mt_range_t;
  ts: integer;
  size: integer;
begin
  size := dbspan_y_max - dbspan_y_min + 1;
  if size < 0 then
    exit;

  if size < 16 then
  begin
    r1.start := 0;
    r1.finish := size - 1;
    _thr_span_db_writer(@r1);
    dbspancacheinfo_size := 0;
    exit;
  end;

  // Quickly decide how many threads to use
  if I_GetNumCPUs <= 4 then
  begin
    ts := size div 4;
    r1.start := dbspan_y_min;
    r1.finish := r1.start + ts;
    r2.start := r1.finish + 1;
    r2.finish := r2.start + ts;
    r3.start := r2.finish + 1;
    r3.finish := r3.start + ts;
    r4.start := r3.finish + 1;
    r4.finish := dbspan_y_max;
    MT_Execute(
      @_thr_span_db_writer, @r1,
      @_thr_span_db_writer, @r2,
      @_thr_span_db_writer, @r3,
      @_thr_span_db_writer, @r4
    );
  end
  else
  begin
    ts := size div 8;
    r1.start := dbspan_y_min;
    r1.finish := r1.start + ts;
    r2.start := r1.finish + 1;
    r2.finish := r2.start + ts;
    r3.start := r2.finish + 1;
    r3.finish := r3.start + ts;
    r4.start := r3.finish + 1;
    r4.finish := r4.start + ts;
    r5.start := r4.finish + 1;
    r5.finish := r5.start + ts;
    r6.start := r5.finish + 1;
    r6.finish := r6.start + ts;
    r7.start := r6.finish + 1;
    r7.finish := r7.start + ts;
    r8.start := r7.finish + 1;
    r8.finish := dbspan_y_max;
    MT_Execute(
      @_thr_span_db_writer, @r1,
      @_thr_span_db_writer, @r2,
      @_thr_span_db_writer, @r3,
      @_thr_span_db_writer, @r4,
      @_thr_span_db_writer, @r5,
      @_thr_span_db_writer, @r6,
      @_thr_span_db_writer, @r7,
      @_thr_span_db_writer, @r8
    );
  end;

  dbspancacheinfo_size := 0;
end;

//==============================================================================
//
// R_DrawColumnWithDepthBufferCheckWrite
//
//==============================================================================
procedure R_DrawColumnWithDepthBufferCheckWrite(const cfunc: Pprocedure);
// db_distance := rw_scale/fracunit;
begin
  R_DrawColumnWithDepthBufferCheckWrite(cfunc, trunc((FRACUNIT / dc_iscale) * FRACUNIT));
end;

//==============================================================================
//
// R_DrawColumnWithDepthBufferCheckWrite
//
//==============================================================================
procedure R_DrawColumnWithDepthBufferCheckWrite(const cfunc: Pprocedure; const depth: LongWord); overload;
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
  db_distance := depth;
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

//==============================================================================
//
// R_DrawBatchColumnWithDepthBufferCheckWrite
//
//==============================================================================
procedure R_DrawBatchColumnWithDepthBufferCheckWrite(const cfunc: Pprocedure); overload;
begin
  R_DrawBatchColumnWithDepthBufferCheckWrite(cfunc, trunc((FRACUNIT / dc_iscale) * FRACUNIT));
end;

//==============================================================================
//
// R_DrawBatchColumnWithDepthBufferCheckWrite
//
//==============================================================================
procedure R_DrawBatchColumnWithDepthBufferCheckWrite(const cfunc: Pprocedure; const depth: LongWord); overload;
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

//==============================================================================
//
// R_DrawColumnWithDepthBufferCheckOnly
//
//==============================================================================
procedure R_DrawColumnWithDepthBufferCheckOnly(const cfunc: Pprocedure);
begin
  R_DrawColumnWithDepthBufferCheckOnly(cfunc, trunc((FRACUNIT / dc_iscale) * FRACUNIT));
end;

//==============================================================================
//
// R_DrawColumnWithDepthBufferCheckOnly
//
//==============================================================================
procedure R_DrawColumnWithDepthBufferCheckOnly(const cfunc: Pprocedure; const depth: LongWord);
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
  db_distance := depth;
  l := db_distance;
  start_y := old_yl;
  oldcheck := false;
  for i := old_yl to old_yh do
  begin
    check := destl^ <= l;
    if check then
    begin
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

//==============================================================================
//
// R_DrawBatchColumnWithDepthBufferCheckOnly
//
//==============================================================================
procedure R_DrawBatchColumnWithDepthBufferCheckOnly(const cfunc: Pprocedure); overload;
begin
  R_DrawBatchColumnWithDepthBufferCheckOnly(cfunc, trunc((FRACUNIT / dc_iscale) * FRACUNIT));
end;

//==============================================================================
//
// R_DrawBatchColumnWithDepthBufferCheckOnly
//
//==============================================================================
procedure R_DrawBatchColumnWithDepthBufferCheckOnly(const cfunc: Pprocedure; const depth: LongWord); overload;
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

//==============================================================================
//
// R_DepthBufferAt
//
//==============================================================================
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

//==============================================================================
// _cleardb_thread_worker
//
// JVAL: Thread function to clean depth buffer in the background
//
//==============================================================================
function _cleardb_thread_worker(parms: Pdbclear_t): integer; stdcall;
begin
  ZeroMemory(depthbuffer[parms.curbuffer], SCREENWIDTH * viewheight * SizeOf(LongWord));
  dbspan_y_min := MAXHEIGHT + 1;
  dbspan_y_max := -1;
  result := 0;
end;

//==============================================================================
//
// R_InitDepthBuffer
//
//==============================================================================
procedure R_InitDepthBuffer;
var
  i: integer;
begin
  dbsize := (SCREENWIDTH * (SCREENHEIGHT + 20) * SizeOf(LongWord) + $10000) and not (4095);
  for i := 0 to NUMDEPTHBUFFERS - 1 do
  begin
    depthbuffer[i] := mallocA(dbsize, $10000, depthbuffertmp[i]); // JVAL: Memory padding may increase performance until 4%
    ZeroMemory(depthbuffer[i], SCREENWIDTH * SCREENHEIGHT * SizeOf(LongWord));
  end;
  dbspan_y_min := MAXHEIGHT + 1;
  dbspan_y_max := -1;
  depthbufferthread := TDThread.Create(@_cleardb_thread_worker);
  depthbufferactive := false;
end;

//==============================================================================
//
// R_WaitDepthBuffer
//
//==============================================================================
procedure R_WaitDepthBuffer;
begin
  if depthbufferthread <> nil then
    depthbufferthread.Wait;
end;

//==============================================================================
//
// R_ShutDownDepthBuffer
//
//==============================================================================
procedure R_ShutDownDepthBuffer;
var
  i: integer;
begin
  R_WaitDepthBuffer;
  depthbufferthread.Free;
  for i := 0 to NUMDEPTHBUFFERS - 1 do
    memfree(Pointer(depthbuffertmp[i]), dbsize + $10000);

  if dbspancacheinfo <> nil then
    memfree(Pointer(dbspancacheinfo), dbspancacheinfo_realsize * SizeOf(dbSpanCacheInfo_t));
  dbspancacheinfo_realsize := 0;
end;

// Called in each render tic before we start depth buffer
var
  lastviewwindowy: Integer = -1;
  lastviewheight: Integer = -1;
  lastscreenwidth: Integer = -1;
  lastscreenheight: Integer = -1;

//==============================================================================
//
// R_StartDepthBuffer
//
//==============================================================================
procedure R_StartDepthBuffer;
var
  i, n: integer;
begin
  if depthbufferactive then // JVAL: Slopes
    exit;
  R_WaitDepthBuffer;  // Wait thread to clear depthbuffer
  if usemultithread then
    @spandepthbufferproc := @R_StoreSpanToDepthBufferMT
  else
    @spandepthbufferproc := @R_DrawSpanToDepthBuffer;
  if (lastviewwindowy <> viewwindowy) or (lastviewheight <> viewheight) or
     (lastscreenwidth <> SCREENWIDTH) or (lastscreenheight <> SCREENHEIGHT) then
  begin
    lastviewwindowy := viewwindowy;
    lastviewheight := viewheight;
    lastscreenwidth := SCREENWIDTH;
    lastscreenheight := SCREENHEIGHT;
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

//==============================================================================
//
// R_StopDepthBuffer
//
//==============================================================================
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
