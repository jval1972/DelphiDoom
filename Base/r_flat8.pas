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
//  Multithreading flat rendering - 8 bit color
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit r_flat8;

interface

uses
  d_delphi,
  m_fixed,
  r_flatinfo;

type
  flatrenderinfo8_t = record
    ds_source: PByteArray;
    ds_colormap: PByteArray;
    ds_y, ds_x1, ds_x2: integer;
    ds_xfrac: fixed_t;
    ds_yfrac: fixed_t;
    ds_xstep: fixed_t;
    ds_ystep: fixed_t;
    ds_ripple: PIntegerArray;
    ds_scale: dsscale_t;
    ds_size: integer;
    func: PPointerParmProcedure;
  end;
  Pflatrenderinfo8_t = ^flatrenderinfo8_t;

  flatrenderinfo8_tArray = array[0..$FFF] of flatrenderinfo8_t;
  Pflatrenderinfo8_tArray = ^flatrenderinfo8_tArray;

//==============================================================================
//
// R_StoreFlatSpan8
//
//==============================================================================
procedure R_StoreFlatSpan8;

//==============================================================================
//
// R_InitFlatsCache8
//
//==============================================================================
procedure R_InitFlatsCache8;

//==============================================================================
//
// R_ShutDownFlatsCache8
//
//==============================================================================
procedure R_ShutDownFlatsCache8;

//==============================================================================
//
// R_RenderMultiThreadFlats8
//
//==============================================================================
procedure R_RenderMultiThreadFlats8;

//==============================================================================
//
// R_RenderMultiThreadFFloors8
//
//==============================================================================
procedure R_RenderMultiThreadFFloors8;

var
  force_numflatrenderingthreads_8bit: integer = 0;

//==============================================================================
//
// R_DrawSpanLowMT
//
//==============================================================================
procedure R_DrawSpanLowMT(const fi: pointer);

//==============================================================================
//
// R_DrawSpanMediumMT
//
//==============================================================================
procedure R_DrawSpanMediumMT(const fi: pointer);

//==============================================================================
//
// R_DrawSpanMedium_RippleMT
//
//==============================================================================
procedure R_DrawSpanMedium_RippleMT(const fi: pointer);

type
  Pflatthreadparams8_t = ^flatthreadparams8_t;
  flatthreadparams8_t = record
    start, stop: integer;
    next: Pflatthreadparams8_t;
  end;

implementation

uses
  i_system,
  mt_utils,
  r_draw,
  r_main,
  r_span,
  r_ripple;

var
  flatcache8: Pflatrenderinfo8_tArray;
  flatcachesize8: integer;
  flatcacherealsize8: integer;

//==============================================================================
//
// R_GrowFlatsCache8
//
//==============================================================================
procedure R_GrowFlatsCache8;
begin
  if flatcachesize8 >= flatcacherealsize8 then
  begin
    realloc(Pointer(flatcache8), flatcacherealsize8 * SizeOf(flatrenderinfo8_t), (64 + flatcacherealsize8) * SizeOf(flatrenderinfo8_t));
    flatcacherealsize8 := flatcacherealsize8 + 64;
  end;
end;

//==============================================================================
//
// R_StoreFlatSpan8
//
//==============================================================================
procedure R_StoreFlatSpan8;
var
  flat: Pflatrenderinfo8_t;
begin
  if ds_x2 - ds_x1 < 0 then
    exit;

  R_GrowFlatsCache8;
  flat := @flatcache8[flatcachesize8];
  flat.ds_source := ds_source;
  flat.ds_colormap := ds_colormap;
  flat.ds_y := ds_y;
  flat.ds_x1 := ds_x1;
  flat.ds_x2 := ds_x2;
  flat.ds_xfrac := ds_xfrac;
  flat.ds_yfrac := ds_yfrac;
  flat.ds_xstep := ds_xstep;
  flat.ds_ystep := ds_ystep;
  flat.ds_ripple := ds_ripple;
  flat.ds_scale := ds_scale;
  flat.ds_size := ds_size;
  flat.func := spanfuncMT;
  inc(flatcachesize8);
end;

//==============================================================================
//
// R_InitFlatsCache8
//
//==============================================================================
procedure R_InitFlatsCache8;
begin
  flatcache8 := nil;
  flatcachesize8 := 0;
  flatcacherealsize8 := 0;
end;

//==============================================================================
//
// R_ShutDownFlatsCache8
//
//==============================================================================
procedure R_ShutDownFlatsCache8;
begin
  if flatcacherealsize8 <> 0 then
  begin
    memfree(pointer(flatcache8), flatcacherealsize8 * SizeOf(flatrenderinfo8_t));
    flatcacherealsize8 := 0;
  end;
end;

const
  MAXFLATRENDERINGTHREADS8 = 16;

var
  R: array[0..MAXFLATRENDERINGTHREADS8 - 1] of flatthreadparams8_t; // JVAL: 20220320 - Made global

//==============================================================================
//
// _flat_thread_worker8
//
//==============================================================================
function _flat_thread_worker8(parms: Pflatthreadparams8_t): integer; stdcall;
var
  item: Pflatrenderinfo8_t;
  start, stop, part: integer;
  i: integer;
begin
  while parms.start <= parms.stop do
  begin
    item := @flatcache8[parms.start];
    item.func(item);
    Inc(parms.start);
  end;

  if parms = @R[0] then
  begin
    Result := 0;
    Exit;
  end;

  while true do
  begin
    parms := parms.next;
    start := parms.start;
    stop := parms.stop;
    part := (stop - start) div 2;
    if part > 2 then
    begin
      parms.stop := parms.stop - part;
      start := parms.stop + 1;
      for i := start to stop do
      begin
        item := @flatcache8[i];
        item.func(item);
      end;
    end
    else if part < 1 then
      Break;
  end;

  result := 0;
end;

//==============================================================================
//
// R_RenderMultiThreadFlats8
//
//==============================================================================
procedure R_RenderMultiThreadFlats8;
var
  step: float;
  numthreads: integer;
  i: integer;
begin
  if flatcachesize8 = 0 then
    exit;

  if force_numflatrenderingthreads_8bit > 0 then
  begin
    numthreads := force_numflatrenderingthreads_8bit;
    if numthreads < 2 then
    begin
      numthreads := 2;
      force_numflatrenderingthreads_8bit := 2;
    end
    else if numthreads > MAXFLATRENDERINGTHREADS8 then
    begin
      numthreads := MAXFLATRENDERINGTHREADS8;
      force_numflatrenderingthreads_8bit := MAXFLATRENDERINGTHREADS8;
    end;
  end
  else
  begin
    numthreads := I_GetNumCPUs;
    if numthreads < 2 then
      numthreads := 2
    else if numthreads > MAXFLATRENDERINGTHREADS8 then
      numthreads := MAXFLATRENDERINGTHREADS8;
  end;

  if flatcachesize8 < numthreads then
  begin
    R[0].start := 0;
    R[0].stop := flatcachesize8 - 1;
    R[0].next := @R[0];
    _flat_thread_worker8(@R[0]);
    flatcachesize8 := 0;
    exit;
  end;

  step := flatcachesize8 / numthreads;
  R[0].start := 0;
  for i := 1 to numthreads - 1 do
    R[i].start := Round(step * i);
  for i := 0 to numthreads - 2 do
    R[i].stop := R[i + 1].start - 1;
  R[numthreads - 1].stop := flatcachesize8 - 1;

  for i := 0 to numthreads - 2 do
    R[i].next := @R[i + 1];
  R[numthreads - 1].next := @R[0];

  case numthreads of
   2:
    MT_Execute(
      @_flat_thread_worker8, @R[0],
      @_flat_thread_worker8, @R[1]
    );
   3:
    MT_Execute(
      @_flat_thread_worker8, @R[0],
      @_flat_thread_worker8, @R[1],
      @_flat_thread_worker8, @R[2]
    );
   4:
    MT_Execute4(
      @_flat_thread_worker8, @R[0],
      @_flat_thread_worker8, @R[1],
      @_flat_thread_worker8, @R[2],
      @_flat_thread_worker8, @R[3]
    );
   5:
    MT_Execute(
      @_flat_thread_worker8, @R[0],
      @_flat_thread_worker8, @R[1],
      @_flat_thread_worker8, @R[2],
      @_flat_thread_worker8, @R[3],
      @_flat_thread_worker8, @R[4]
    );
   6:
    MT_Execute6(
      @_flat_thread_worker8, @R[0],
      @_flat_thread_worker8, @R[1],
      @_flat_thread_worker8, @R[2],
      @_flat_thread_worker8, @R[3],
      @_flat_thread_worker8, @R[4],
      @_flat_thread_worker8, @R[5]
    );
   7:
    MT_Execute(
      @_flat_thread_worker8, @R[0],
      @_flat_thread_worker8, @R[1],
      @_flat_thread_worker8, @R[2],
      @_flat_thread_worker8, @R[3],
      @_flat_thread_worker8, @R[4],
      @_flat_thread_worker8, @R[5],
      @_flat_thread_worker8, @R[6]
    );
   8:
    MT_Execute8(
      @_flat_thread_worker8, @R[0],
      @_flat_thread_worker8, @R[1],
      @_flat_thread_worker8, @R[2],
      @_flat_thread_worker8, @R[3],
      @_flat_thread_worker8, @R[4],
      @_flat_thread_worker8, @R[5],
      @_flat_thread_worker8, @R[6],
      @_flat_thread_worker8, @R[7]
    );
   9:
    MT_Execute(
      @_flat_thread_worker8, @R[0],
      @_flat_thread_worker8, @R[1],
      @_flat_thread_worker8, @R[2],
      @_flat_thread_worker8, @R[3],
      @_flat_thread_worker8, @R[4],
      @_flat_thread_worker8, @R[5],
      @_flat_thread_worker8, @R[6],
      @_flat_thread_worker8, @R[7],
      @_flat_thread_worker8, @R[8]
    );
  10:
    MT_Execute(
      @_flat_thread_worker8, @R[0],
      @_flat_thread_worker8, @R[1],
      @_flat_thread_worker8, @R[2],
      @_flat_thread_worker8, @R[3],
      @_flat_thread_worker8, @R[4],
      @_flat_thread_worker8, @R[5],
      @_flat_thread_worker8, @R[6],
      @_flat_thread_worker8, @R[7],
      @_flat_thread_worker8, @R[8],
      @_flat_thread_worker8, @R[9]
    );
  11:
    MT_Execute(
      @_flat_thread_worker8, @R[0],
      @_flat_thread_worker8, @R[1],
      @_flat_thread_worker8, @R[2],
      @_flat_thread_worker8, @R[3],
      @_flat_thread_worker8, @R[4],
      @_flat_thread_worker8, @R[5],
      @_flat_thread_worker8, @R[6],
      @_flat_thread_worker8, @R[7],
      @_flat_thread_worker8, @R[8],
      @_flat_thread_worker8, @R[9],
      @_flat_thread_worker8, @R[10]
    );
  12:
    MT_Execute12(
      @_flat_thread_worker8, @R[0],
      @_flat_thread_worker8, @R[1],
      @_flat_thread_worker8, @R[2],
      @_flat_thread_worker8, @R[3],
      @_flat_thread_worker8, @R[4],
      @_flat_thread_worker8, @R[5],
      @_flat_thread_worker8, @R[6],
      @_flat_thread_worker8, @R[7],
      @_flat_thread_worker8, @R[8],
      @_flat_thread_worker8, @R[9],
      @_flat_thread_worker8, @R[10],
      @_flat_thread_worker8, @R[11]
    );
  13:
    MT_Execute(
      @_flat_thread_worker8, @R[0],
      @_flat_thread_worker8, @R[1],
      @_flat_thread_worker8, @R[2],
      @_flat_thread_worker8, @R[3],
      @_flat_thread_worker8, @R[4],
      @_flat_thread_worker8, @R[5],
      @_flat_thread_worker8, @R[6],
      @_flat_thread_worker8, @R[7],
      @_flat_thread_worker8, @R[8],
      @_flat_thread_worker8, @R[9],
      @_flat_thread_worker8, @R[10],
      @_flat_thread_worker8, @R[11],
      @_flat_thread_worker8, @R[12]
    );
  14:
    MT_Execute(
      @_flat_thread_worker8, @R[0],
      @_flat_thread_worker8, @R[1],
      @_flat_thread_worker8, @R[2],
      @_flat_thread_worker8, @R[3],
      @_flat_thread_worker8, @R[4],
      @_flat_thread_worker8, @R[5],
      @_flat_thread_worker8, @R[6],
      @_flat_thread_worker8, @R[7],
      @_flat_thread_worker8, @R[8],
      @_flat_thread_worker8, @R[9],
      @_flat_thread_worker8, @R[10],
      @_flat_thread_worker8, @R[11],
      @_flat_thread_worker8, @R[12],
      @_flat_thread_worker8, @R[13]
    );
  15:
    MT_Execute(
      @_flat_thread_worker8, @R[0],
      @_flat_thread_worker8, @R[1],
      @_flat_thread_worker8, @R[2],
      @_flat_thread_worker8, @R[3],
      @_flat_thread_worker8, @R[4],
      @_flat_thread_worker8, @R[5],
      @_flat_thread_worker8, @R[6],
      @_flat_thread_worker8, @R[7],
      @_flat_thread_worker8, @R[8],
      @_flat_thread_worker8, @R[9],
      @_flat_thread_worker8, @R[10],
      @_flat_thread_worker8, @R[11],
      @_flat_thread_worker8, @R[12],
      @_flat_thread_worker8, @R[13],
      @_flat_thread_worker8, @R[14]
    );
  else
    MT_Execute16(
      @_flat_thread_worker8, @R[0],
      @_flat_thread_worker8, @R[1],
      @_flat_thread_worker8, @R[2],
      @_flat_thread_worker8, @R[3],
      @_flat_thread_worker8, @R[4],
      @_flat_thread_worker8, @R[5],
      @_flat_thread_worker8, @R[6],
      @_flat_thread_worker8, @R[7],
      @_flat_thread_worker8, @R[8],
      @_flat_thread_worker8, @R[9],
      @_flat_thread_worker8, @R[10],
      @_flat_thread_worker8, @R[11],
      @_flat_thread_worker8, @R[12],
      @_flat_thread_worker8, @R[13],
      @_flat_thread_worker8, @R[14],
      @_flat_thread_worker8, @R[15]
    );
  end;

  flatcachesize8 := 0;
end;

//==============================================================================
//
// _flat3D_thread_worker8
//
//==============================================================================
function _flat3D_thread_worker8(parms: Pflatthreadparams8_t): integer; stdcall;
var
  item1, item2: Pflatrenderinfo8_t;
  start, finish: integer;
begin
  item1 := @flatcache8[0];
  item2 := @flatcache8[flatcachesize8 - 1];
  start := parms.start;
  finish := parms.stop;
  while integer(item1) <= integer(item2) do
  begin
    if item1.ds_y >= start then
      if item1.ds_y <= finish then
        item1.func(item1);
    inc(item1);
  end;

  Result := 0;
end;

//==============================================================================
//
// R_RenderMultiThreadFFloors8
//
//==============================================================================
procedure R_RenderMultiThreadFFloors8;
var
  step: float;
  numthreads: integer;
  i: integer;
begin
  if flatcachesize8 = 0 then
    exit;

  if force_numflatrenderingthreads_8bit > 0 then
  begin
    numthreads := force_numflatrenderingthreads_8bit;
    if numthreads < 2 then
    begin
      numthreads := 2;
      force_numflatrenderingthreads_8bit := 2;
    end
    else if numthreads > MAXFLATRENDERINGTHREADS8 then
    begin
      numthreads := MAXFLATRENDERINGTHREADS8;
      force_numflatrenderingthreads_8bit := MAXFLATRENDERINGTHREADS8;
    end;
  end
  else
  begin
    numthreads := I_GetNumCPUs;
    if numthreads < 2 then
      numthreads := 2
    else if numthreads > MAXFLATRENDERINGTHREADS8 then
      numthreads := MAXFLATRENDERINGTHREADS8;
  end;

  if viewheight < numthreads then
  begin
    R[0].start := 0;
    R[0].stop := viewheight - 1;
    R[0].next := @R[0];
    _flat3D_thread_worker8(@R[0]);
    flatcachesize8 := 0;
    exit;
  end;

  step := viewheight / numthreads;
  R[0].start := 0;
  for i := 1 to numthreads - 1 do
    R[i].start := Round(step * i);
  for i := 0 to numthreads - 2 do
    R[i].stop := R[i + 1].start - 1;
  R[numthreads - 1].stop := viewheight - 1;

  for i := 0 to numthreads - 2 do
    R[i].next := @R[i + 1];
  R[numthreads - 1].next := @R[0];

  case numthreads of
   2:
    MT_Execute(
      @_flat3D_thread_worker8, @R[0],
      @_flat3D_thread_worker8, @R[1]
    );
   3:
    MT_Execute(
      @_flat3D_thread_worker8, @R[0],
      @_flat3D_thread_worker8, @R[1],
      @_flat3D_thread_worker8, @R[2]
    );
   4:
    MT_Execute4(
      @_flat3D_thread_worker8, @R[0],
      @_flat3D_thread_worker8, @R[1],
      @_flat3D_thread_worker8, @R[2],
      @_flat3D_thread_worker8, @R[3]
    );
   5:
    MT_Execute(
      @_flat3D_thread_worker8, @R[0],
      @_flat3D_thread_worker8, @R[1],
      @_flat3D_thread_worker8, @R[2],
      @_flat3D_thread_worker8, @R[3],
      @_flat3D_thread_worker8, @R[4]
    );
   6:
    MT_Execute6(
      @_flat3D_thread_worker8, @R[0],
      @_flat3D_thread_worker8, @R[1],
      @_flat3D_thread_worker8, @R[2],
      @_flat3D_thread_worker8, @R[3],
      @_flat3D_thread_worker8, @R[4],
      @_flat3D_thread_worker8, @R[5]
    );
   7:
    MT_Execute(
      @_flat3D_thread_worker8, @R[0],
      @_flat3D_thread_worker8, @R[1],
      @_flat3D_thread_worker8, @R[2],
      @_flat3D_thread_worker8, @R[3],
      @_flat3D_thread_worker8, @R[4],
      @_flat3D_thread_worker8, @R[5],
      @_flat3D_thread_worker8, @R[6]
    );
   8:
    MT_Execute8(
      @_flat3D_thread_worker8, @R[0],
      @_flat3D_thread_worker8, @R[1],
      @_flat3D_thread_worker8, @R[2],
      @_flat3D_thread_worker8, @R[3],
      @_flat3D_thread_worker8, @R[4],
      @_flat3D_thread_worker8, @R[5],
      @_flat3D_thread_worker8, @R[6],
      @_flat3D_thread_worker8, @R[7]
    );
   9:
    MT_Execute(
      @_flat3D_thread_worker8, @R[0],
      @_flat3D_thread_worker8, @R[1],
      @_flat3D_thread_worker8, @R[2],
      @_flat3D_thread_worker8, @R[3],
      @_flat3D_thread_worker8, @R[4],
      @_flat3D_thread_worker8, @R[5],
      @_flat3D_thread_worker8, @R[6],
      @_flat3D_thread_worker8, @R[7],
      @_flat3D_thread_worker8, @R[8]
    );
  10:
    MT_Execute(
      @_flat3D_thread_worker8, @R[0],
      @_flat3D_thread_worker8, @R[1],
      @_flat3D_thread_worker8, @R[2],
      @_flat3D_thread_worker8, @R[3],
      @_flat3D_thread_worker8, @R[4],
      @_flat3D_thread_worker8, @R[5],
      @_flat3D_thread_worker8, @R[6],
      @_flat3D_thread_worker8, @R[7],
      @_flat3D_thread_worker8, @R[8],
      @_flat3D_thread_worker8, @R[9]
    );
  11:
    MT_Execute(
      @_flat3D_thread_worker8, @R[0],
      @_flat3D_thread_worker8, @R[1],
      @_flat3D_thread_worker8, @R[2],
      @_flat3D_thread_worker8, @R[3],
      @_flat3D_thread_worker8, @R[4],
      @_flat3D_thread_worker8, @R[5],
      @_flat3D_thread_worker8, @R[6],
      @_flat3D_thread_worker8, @R[7],
      @_flat3D_thread_worker8, @R[8],
      @_flat3D_thread_worker8, @R[9],
      @_flat3D_thread_worker8, @R[10]
    );
  12:
    MT_Execute12(
      @_flat3D_thread_worker8, @R[0],
      @_flat3D_thread_worker8, @R[1],
      @_flat3D_thread_worker8, @R[2],
      @_flat3D_thread_worker8, @R[3],
      @_flat3D_thread_worker8, @R[4],
      @_flat3D_thread_worker8, @R[5],
      @_flat3D_thread_worker8, @R[6],
      @_flat3D_thread_worker8, @R[7],
      @_flat3D_thread_worker8, @R[8],
      @_flat3D_thread_worker8, @R[9],
      @_flat3D_thread_worker8, @R[10],
      @_flat3D_thread_worker8, @R[11]
    );
  13:
    MT_Execute(
      @_flat3D_thread_worker8, @R[0],
      @_flat3D_thread_worker8, @R[1],
      @_flat3D_thread_worker8, @R[2],
      @_flat3D_thread_worker8, @R[3],
      @_flat3D_thread_worker8, @R[4],
      @_flat3D_thread_worker8, @R[5],
      @_flat3D_thread_worker8, @R[6],
      @_flat3D_thread_worker8, @R[7],
      @_flat3D_thread_worker8, @R[8],
      @_flat3D_thread_worker8, @R[9],
      @_flat3D_thread_worker8, @R[10],
      @_flat3D_thread_worker8, @R[11],
      @_flat3D_thread_worker8, @R[12]
    );
  14:
    MT_Execute(
      @_flat3D_thread_worker8, @R[0],
      @_flat3D_thread_worker8, @R[1],
      @_flat3D_thread_worker8, @R[2],
      @_flat3D_thread_worker8, @R[3],
      @_flat3D_thread_worker8, @R[4],
      @_flat3D_thread_worker8, @R[5],
      @_flat3D_thread_worker8, @R[6],
      @_flat3D_thread_worker8, @R[7],
      @_flat3D_thread_worker8, @R[8],
      @_flat3D_thread_worker8, @R[9],
      @_flat3D_thread_worker8, @R[10],
      @_flat3D_thread_worker8, @R[11],
      @_flat3D_thread_worker8, @R[12],
      @_flat3D_thread_worker8, @R[13]
    );
  15:
    MT_Execute(
      @_flat3D_thread_worker8, @R[0],
      @_flat3D_thread_worker8, @R[1],
      @_flat3D_thread_worker8, @R[2],
      @_flat3D_thread_worker8, @R[3],
      @_flat3D_thread_worker8, @R[4],
      @_flat3D_thread_worker8, @R[5],
      @_flat3D_thread_worker8, @R[6],
      @_flat3D_thread_worker8, @R[7],
      @_flat3D_thread_worker8, @R[8],
      @_flat3D_thread_worker8, @R[9],
      @_flat3D_thread_worker8, @R[10],
      @_flat3D_thread_worker8, @R[11],
      @_flat3D_thread_worker8, @R[12],
      @_flat3D_thread_worker8, @R[13],
      @_flat3D_thread_worker8, @R[14]
    );
  else
    MT_Execute16(
      @_flat3D_thread_worker8, @R[0],
      @_flat3D_thread_worker8, @R[1],
      @_flat3D_thread_worker8, @R[2],
      @_flat3D_thread_worker8, @R[3],
      @_flat3D_thread_worker8, @R[4],
      @_flat3D_thread_worker8, @R[5],
      @_flat3D_thread_worker8, @R[6],
      @_flat3D_thread_worker8, @R[7],
      @_flat3D_thread_worker8, @R[8],
      @_flat3D_thread_worker8, @R[9],
      @_flat3D_thread_worker8, @R[10],
      @_flat3D_thread_worker8, @R[11],
      @_flat3D_thread_worker8, @R[12],
      @_flat3D_thread_worker8, @R[13],
      @_flat3D_thread_worker8, @R[14],
      @_flat3D_thread_worker8, @R[15]
    );
  end;

  flatcachesize8 := 0;
end;

//==============================================================================
// R_DrawSpanLowMT
//
// Draws the actual span (Low resolution).
//
//==============================================================================
procedure R_DrawSpanLowMT(const fi: pointer);
var
  ds_source: PByteArray;
  ds_colormap: PByteArray;
  ds_y, ds_x1, ds_x2: integer;
  ds_xfrac: fixed_t;
  ds_yfrac: fixed_t;
  ds_xstep: fixed_t;
  ds_ystep: fixed_t;
  ds_scale: dsscale_t;
  ds_size: integer;
  xfrac: fixed_t;
  yfrac: fixed_t;
  dest: PByte;
  bdest: byte;
  ds_xstep2: fixed_t;
  ds_ystep2: fixed_t;
  count: integer;
  i: integer;
  spot: integer;
  _shift, _and1, _and2: integer;
  rpl: PIntegerArray;
  rpl_shift, rpl_factor: Integer;
begin
  ds_source := Pflatrenderinfo8_t(fi).ds_source;
  ds_colormap := Pflatrenderinfo8_t(fi).ds_colormap;
  ds_y := Pflatrenderinfo8_t(fi).ds_y;
  ds_x1 := Pflatrenderinfo8_t(fi).ds_x1;
  ds_x2 := Pflatrenderinfo8_t(fi).ds_x2;
  ds_xfrac := Pflatrenderinfo8_t(fi).ds_xfrac;
  ds_yfrac := Pflatrenderinfo8_t(fi).ds_yfrac;
  ds_xstep := Pflatrenderinfo8_t(fi).ds_xstep;
  ds_ystep := Pflatrenderinfo8_t(fi).ds_ystep;
  ds_scale := Pflatrenderinfo8_t(fi).ds_scale;
  ds_size := Pflatrenderinfo8_t(fi).ds_size;

  dest := @((ylookup[ds_y]^)[columnofs[ds_x1]]);

  case ds_size of
  FS64x64:
    begin
      if ds_scale = ds512x512 then
      begin
        _shift := 7;
        _and1 := 261632;
        _and2 := 511;
        xfrac := ds_xfrac * 8;
        yfrac := ds_yfrac * 8;
        // Blocky mode, multiply by 3 (!!).
        ds_xstep2 := ds_xstep * 24;
        ds_ystep2 := ds_ystep * 24;
      end
      else if ds_scale = ds256x256 then
      begin
        _shift := 8;
        _and1 := 65280;
        _and2 := 255;
        xfrac := ds_xfrac * 4;
        yfrac := ds_yfrac * 4;
        // Blocky mode, multiply by 3 (!!).
        ds_xstep2 := ds_xstep * 12;
        ds_ystep2 := ds_ystep * 12;
      end
      else if ds_scale = ds128x128 then
      begin
        _shift := 9;
        _and1 := 16256;
        _and2 := 127;
        xfrac := ds_xfrac * 2;
        yfrac := ds_yfrac * 2;
        // Blocky mode, multiply by 3 (!!).
        ds_xstep2 := ds_xstep * 6;
        ds_ystep2 := ds_ystep * 6;
      end
      else
      begin
        _shift := 10;
        _and1 := 4032;
        _and2 := 63;
        xfrac := ds_xfrac;
        yfrac := ds_yfrac;
        // Blocky mode, multiply by 3 (!!).
        ds_xstep2 := ds_xstep * 3;
        ds_ystep2 := ds_ystep * 3;
      end;
    end;
  FS128x128:
    begin
      _shift := 9;
      _and1 := 16256;
      _and2 := 127;
      xfrac := ds_xfrac;
      yfrac := ds_yfrac;
      // Blocky mode, multiply by 3 (!!).
      ds_xstep2 := ds_xstep * 3;
      ds_ystep2 := ds_ystep * 3;
    end;
  FS256x256:
    begin
      _shift := 8;
      _and1 := 65280;
      _and2 := 255;
      xfrac := ds_xfrac;
      yfrac := ds_yfrac;
      // Blocky mode, multiply by 3 (!!).
      ds_xstep2 := ds_xstep * 3;
      ds_ystep2 := ds_ystep * 3;
    end;
  FS512x512:
    begin
      _shift := 7;
      _and1 := 261632;
      _and2 := 511;
      xfrac := ds_xfrac;
      yfrac := ds_yfrac;
      // Blocky mode, multiply by 3 (!!).
      ds_xstep2 := ds_xstep * 3;
      ds_ystep2 := ds_ystep * 3;
    end;
  FS1024x1024:
    begin
      _shift := 6;
      _and1 := 1047552;
      _and2 := 1023;
      xfrac := ds_xfrac;
      yfrac := ds_yfrac;
      // Blocky mode, multiply by 3 (!!).
      ds_xstep2 := ds_xstep * 3;
      ds_ystep2 := ds_ystep * 3;
    end;
  FS2048x2048:
    begin
      _shift := 5;
      _and1 := 4192256;
      _and2 := 2047;
      xfrac := ds_xfrac;
      yfrac := ds_yfrac;
      // Blocky mode, multiply by 3 (!!).
      ds_xstep2 := ds_xstep * 3;
      ds_ystep2 := ds_ystep * 3;
    end;
  else
    begin
      _shift := 4;
      _and1 := 16773120;
      _and2 := 4095;
      xfrac := ds_xfrac;
      yfrac := ds_yfrac;
      // Blocky mode, multiply by 3 (!!).
      ds_xstep2 := ds_xstep * 3;
      ds_ystep2 := ds_ystep * 3;
    end;
  end;

  if ds_ripple <> nil then
  begin
    rpl := ds_ripple;

    case ds_size of
    FS64x64:
      begin
        if ds_scale = ds512x512 then
        begin
          rpl_shift := 19;
          rpl_factor := 8;
        end
        else if ds_scale = ds256x256 then
        begin
          rpl_shift := 18;
          rpl_factor := 4;
        end
        else if ds_scale = ds128x128 then
        begin
          rpl_shift := 17;
          rpl_factor := 2;
        end
        else
        begin
          rpl_shift := 16;
          rpl_factor := 1;
        end;
      end;
    else
      begin
        rpl_shift := 16;
        rpl_factor := 1;
      end;
    end;

    count := (ds_x2 - ds_x1) div 3;
    if count < 0 then
      exit;

    for i := 0 to count do
    begin
      spot := (LongWord(yfrac + rpl_factor * rpl[(LongWord(xfrac) shr rpl_shift) and 127]) shr _shift) and (_and1) or
              (LongWord(xfrac + rpl_factor * rpl[(LongWord(yfrac) shr rpl_shift) and 127]) shr FRACBITS) and _and2;
      // Lowres/blocky mode does it twice,
      //  while scale is adjusted appropriately.
      bdest := ds_colormap[ds_source[spot]];
      dest^ := bdest;
      inc(dest);
      dest^ := bdest;
      inc(dest);
      dest^ := bdest;
      inc(dest);

      xfrac := xfrac + ds_xstep2;
      yfrac := yfrac + ds_ystep2;
    end;

    count := (ds_x2 - ds_x1) mod 3;

    for i := 0 to count do
    begin
      spot := (LongWord(yfrac) shr _shift) and (_and1) or (LongWord(xfrac) shr FRACBITS) and _and2;
      // Lowres/blocky mode does it twice,
      //  while scale is adjusted appropriately.
      dest^ := ds_colormap[ds_source[spot]];
      inc(dest);

      xfrac := xfrac + ds_xstep;
      yfrac := yfrac + ds_ystep;
    end;

  end
  else
  begin
    count := (ds_x2 - ds_x1) div 3;
    if count < 0 then
      exit;

    for i := 0 to count do
    begin
      spot := (LongWord(yfrac) shr _shift) and (_and1) or (LongWord(xfrac) shr FRACBITS) and _and2;
      // Lowres/blocky mode does it twice,
      //  while scale is adjusted appropriately.
      bdest := ds_colormap[ds_source[spot]];
      dest^ := bdest;
      inc(dest);
      dest^ := bdest;
      inc(dest);
      dest^ := bdest;
      inc(dest);

      xfrac := xfrac + ds_xstep2;
      yfrac := yfrac + ds_ystep2;
    end;

    count := (ds_x2 - ds_x1) mod 3;

    for i := 0 to count do
    begin
      spot := (LongWord(yfrac) shr _shift) and (_and1) or (LongWord(xfrac) shr FRACBITS) and _and2;
      // Lowres/blocky mode does it twice,
      //  while scale is adjusted appropriately.
      dest^ := ds_colormap[ds_source[spot]];
      inc(dest);

      xfrac := xfrac + ds_xstep;
      yfrac := yfrac + ds_ystep;
    end;
  end;
end;

//==============================================================================
// R_DrawSpanMediumMT
//
// Draws the actual span (Medium resolution).
//
//==============================================================================
procedure R_DrawSpanMediumMT(const fi: pointer);
var
  ds_source: PByteArray;
  ds_colormap: PByteArray;
  ds_y, ds_x1, ds_x2: integer;
  ds_xfrac: fixed_t;
  ds_yfrac: fixed_t;
  ds_xstep: fixed_t;
  ds_ystep: fixed_t;
  ds_scale: dsscale_t;
  ds_size: integer;
  xfrac: fixed_t;
  yfrac: fixed_t;
  xstep: fixed_t;
  ystep: fixed_t;
  dest: PByte;
  count: integer;
  i: integer;
  spot: integer;
begin
  ds_source := Pflatrenderinfo8_t(fi).ds_source;
  ds_colormap := Pflatrenderinfo8_t(fi).ds_colormap;
  ds_y := Pflatrenderinfo8_t(fi).ds_y;
  ds_x1 := Pflatrenderinfo8_t(fi).ds_x1;
  ds_x2 := Pflatrenderinfo8_t(fi).ds_x2;
  ds_xfrac := Pflatrenderinfo8_t(fi).ds_xfrac;
  ds_yfrac := Pflatrenderinfo8_t(fi).ds_yfrac;
  ds_xstep := Pflatrenderinfo8_t(fi).ds_xstep;
  ds_ystep := Pflatrenderinfo8_t(fi).ds_ystep;
  ds_scale := Pflatrenderinfo8_t(fi).ds_scale;
  ds_size := Pflatrenderinfo8_t(fi).ds_size;

  dest := @((ylookup[ds_y]^)[columnofs[ds_x1]]);

  // We do not check for zero spans here?
  count := ds_x2 - ds_x1;
  {$UNDEF RIPPLE}
  {$I R_DrawSpanMedium.inc}
end;

//==============================================================================
//
// R_DrawSpanMedium_RippleMT
//
//==============================================================================
procedure R_DrawSpanMedium_RippleMT(const fi: pointer);
var
  ds_source: PByteArray;
  ds_colormap: PByteArray;
  ds_y, ds_x1, ds_x2: integer;
  ds_xfrac: fixed_t;
  ds_yfrac: fixed_t;
  ds_xstep: fixed_t;
  ds_ystep: fixed_t;
  ds_scale: dsscale_t;
  ds_size: integer;
  xfrac: fixed_t;
  yfrac: fixed_t;
  xstep: fixed_t;
  ystep: fixed_t;
  dest: PByte;
  count: integer;
  i: integer;
  spot: integer;
  rpl: PIntegerArray;
begin
  ds_source := Pflatrenderinfo8_t(fi).ds_source;
  ds_colormap := Pflatrenderinfo8_t(fi).ds_colormap;
  ds_y := Pflatrenderinfo8_t(fi).ds_y;
  ds_x1 := Pflatrenderinfo8_t(fi).ds_x1;
  ds_x2 := Pflatrenderinfo8_t(fi).ds_x2;
  ds_xfrac := Pflatrenderinfo8_t(fi).ds_xfrac;
  ds_yfrac := Pflatrenderinfo8_t(fi).ds_yfrac;
  ds_xstep := Pflatrenderinfo8_t(fi).ds_xstep;
  ds_ystep := Pflatrenderinfo8_t(fi).ds_ystep;
  rpl := Pflatrenderinfo8_t(fi).ds_ripple;
  ds_scale := Pflatrenderinfo8_t(fi).ds_scale;
  ds_size := Pflatrenderinfo8_t(fi).ds_size;

  dest := @((ylookup[ds_y]^)[columnofs[ds_x1]]);

  // We do not check for zero spans here?
  count := ds_x2 - ds_x1;

  {$DEFINE RIPPLE}
  {$I R_DrawSpanMedium.inc}
end;

end.

