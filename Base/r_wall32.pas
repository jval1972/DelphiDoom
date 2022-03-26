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
//  Multithreading wall rendering - 32 bit color
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit r_wall32;

interface

uses
  d_delphi,
  m_fixed,
  mt_utils,
  r_cache_walls;

type
  wallrenderinfo32_t = record
    dc_source32: PLongWordArray;
    dc_texturefactorbits: integer;
    dc_yh, dc_yl, dc_x: integer;
    dc_iscale: fixed_t;
    dc_texturemid: fixed_t;
    dc_lightlevel: fixed_t;
    dc_fog: boolean;  // JVAL: Mars fog sectors
  end;
  Pwallrenderinfo32_t = ^wallrenderinfo32_t;

  batchwallrenderinfo32_t = record
    numwalls: integer;
    dc_height: integer;
    walls: array[0..MAXBATCHWALLS - 1] of wallrenderinfo32_t;
  end;
  Pbatchwallrenderinfo32_t = ^batchwallrenderinfo32_t;

  batchwallrenderinfo32_tArray = array[0..$FFF] of batchwallrenderinfo32_t;
  Pbatchwallrenderinfo32_tArray = ^batchwallrenderinfo32_tArray;

//==============================================================================
//
// R_StoreWallColumn32
//
//==============================================================================
procedure R_StoreWallColumn32(const idx: PInteger);

//==============================================================================
//
// R_FlashWallColumns32
//
//==============================================================================
procedure R_FlashWallColumns32(const idx: PInteger);

//==============================================================================
//
// R_InitWallsCache32
//
//==============================================================================
procedure R_InitWallsCache32;

//==============================================================================
//
// R_ShutDownWallsCache32
//
//==============================================================================
procedure R_ShutDownWallsCache32;

//==============================================================================
//
// R_ClearWallsCache32
//
//==============================================================================
procedure R_ClearWallsCache32;

//==============================================================================
//
// R_RenderMultiThreadWallParams32
//
//==============================================================================
procedure R_RenderMultiThreadWallParams32(const parms: mt_linkedrange_pa);

var
  midwalls32: integer;
  lowerwalls32: integer;
  upperwalls32: integer;
  // JVAL: 3d Floors
  midwalls32b: integer;
  lowerwalls32b: integer;
  upperwalls32b: integer;

implementation

uses
  {$IFDEF HEXEN}
  r_data,
  {$ENDIF}
  doomdef,
  i_threads,
  i_system,
  r_column,
  r_tallcolumn,
  r_draw,
  r_precalc,
  r_render,
  r_main;

{$IFNDEF OPTIMIZE_FOR_SIZE}

{$DEFINE WALL32_128}
{$UNDEF WALL32_256}
{$UNDEF WALL32_512}
{$UNDEF WALL32_TC}
{$I R_Wall32_BatchFuncs.inc}

{$UNDEF WALL32_128}
{$DEFINE WALL32_256}
{$UNDEF WALL32_512}
{$UNDEF WALL32_TC}
{$I R_Wall32_BatchFuncs.inc}

{$UNDEF WALL32_128}
{$UNDEF WALL32_256}
{$DEFINE WALL32_512}
{$UNDEF WALL32_TC}
{$I R_Wall32_BatchFuncs.inc}

{$ENDIF}

{$UNDEF WALL32_128}
{$UNDEF WALL32_256}
{$UNDEF WALL32_512}
{$DEFINE WALL32_TC}
{$I R_Wall32_BatchFuncs.inc}

var
  wallcache32: Pbatchwallrenderinfo32_tArray;
  wallcachesize32: integer;
  wallcacherealsize32: integer;

//==============================================================================
//
// R_GrowWallsCache32
//
//==============================================================================
procedure R_GrowWallsCache32;
begin
  if wallcachesize32 >= wallcacherealsize32 then
  begin
    realloc(Pointer(wallcache32), wallcacherealsize32 * SizeOf(batchwallrenderinfo32_t), (64 + wallcacherealsize32) * SizeOf(batchwallrenderinfo32_t));
    wallcacherealsize32 := wallcacherealsize32 + 64;
  end;
end;

//==============================================================================
//
// R_AddWallsToCache32
//
//==============================================================================
procedure R_AddWallsToCache32(const idx: PInteger);
begin
  R_GrowWallsCache32;
  idx^ := wallcachesize32;
  wallcache32[wallcachesize32].numwalls := 0;
  inc(wallcachesize32);
end;

//==============================================================================
//
// R_DrawSingleThreadWall32
//
//==============================================================================
procedure R_DrawSingleThreadWall32(const w: Pwallrenderinfo32_t);
begin
  dc_source32 := w.dc_source32;
  dc_texturefactorbits := w.dc_texturefactorbits;
  dc_yh := w.dc_yh;
  dc_yl := w.dc_yl;
  dc_x := w.dc_x;
  dc_iscale := w.dc_iscale;
  dc_texturemid := w.dc_texturemid;
  dc_lightlevel := w.dc_lightlevel;
  dc_fog := w.dc_fog; // JVAL: Mars fog sectors
  wallcolfunc;
end;

//==============================================================================
//
// R_RenderWall32
//
//==============================================================================
procedure R_RenderWall32(walls: Pbatchwallrenderinfo32_t);
var
  nwalls: integer;
  w: Pwallrenderinfo32_t;
  i: integer;
  {$IFNDEF OPTIMIZE_FOR_SIZE}
  w_height: integer;
  {$ENDIF}
begin
  nwalls := walls.numwalls;
  {$IFNDEF OPTIMIZE_FOR_SIZE}
  w_height := walls.dc_height;
  {$ENDIF}
  if nwalls = MAXBATCHWALLS then
  begin
    {$IFNDEF OPTIMIZE_FOR_SIZE}
    if w_height = 128 then
      R_DrawBatchColumnHi8_128(walls)
    else if w_height = 256 then
      R_DrawBatchColumnHi8_256(walls)
    else if w_height = 512 then
      R_DrawBatchColumnHi8_512(walls)
    else {$ENDIF}
      R_DrawBatchColumnHi8_TC(walls);
    walls.numwalls := 0;
  end
  else case nwalls of
    7:
      begin
        {$IFNDEF OPTIMIZE_FOR_SIZE}
        if w_height = 128 then
          R_DrawBatchColumnHi7_128(walls)
        else if w_height = 256 then
          R_DrawBatchColumnHi7_256(walls)
        else if w_height = 512 then
          R_DrawBatchColumnHi7_512(walls)
        else {$ENDIF}
          R_DrawBatchColumnHi7_TC(walls);
        walls.numwalls := 0;
      end;
    6:
      begin
        {$IFNDEF OPTIMIZE_FOR_SIZE}
        if w_height = 128 then
          R_DrawBatchColumnHi6_128(walls)
        else if w_height = 256 then
          R_DrawBatchColumnHi6_256(walls)
        else if w_height = 512 then
          R_DrawBatchColumnHi6_512(walls)
        else {$ENDIF}
          R_DrawBatchColumnHi6_TC(walls);
        walls.numwalls := 0;
      end;
    5:
      begin
        {$IFNDEF OPTIMIZE_FOR_SIZE}
        if w_height = 128 then
          R_DrawBatchColumnHi5_128(walls)
        else if w_height = 256 then
          R_DrawBatchColumnHi5_256(walls)
        else if w_height = 512 then
          R_DrawBatchColumnHi5_512(walls)
        else {$ENDIF}
          R_DrawBatchColumnHi5_TC(walls);
        walls.numwalls := 0;
      end;
    4:
      begin
        {$IFNDEF OPTIMIZE_FOR_SIZE}
        if w_height = 128 then
          R_DrawBatchColumnHi4_128(walls)
        else if w_height = 256 then
          R_DrawBatchColumnHi4_256(walls)
        else if w_height = 512 then
          R_DrawBatchColumnHi4_512(walls)
        else {$ENDIF}
          R_DrawBatchColumnHi4_TC(walls);
        walls.numwalls := 0;
      end;
    3:
      begin
        {$IFNDEF OPTIMIZE_FOR_SIZE}
        if w_height = 128 then
          R_DrawBatchColumnHi3_128(walls)
        else if w_height = 256 then
          R_DrawBatchColumnHi3_256(walls)
        else if w_height = 512 then
          R_DrawBatchColumnHi3_512(walls)
        else {$ENDIF}
          R_DrawBatchColumnHi3_TC(walls);
        walls.numwalls := 0;
      end;
    2:
      begin
        {$IFNDEF OPTIMIZE_FOR_SIZE}
        if w_height = 128 then
          R_DrawBatchColumnHi2_128(walls)
        else if w_height = 256 then
          R_DrawBatchColumnHi2_256(walls)
        else if w_height = 512 then
          R_DrawBatchColumnHi2_512(walls)
        else {$ENDIF}
          R_DrawBatchColumnHi2_TC(walls);
        walls.numwalls := 0;
      end;
    1:
      begin
        {$IFNDEF OPTIMIZE_FOR_SIZE}
        if w_height = 128 then
          R_DrawBatchColumnHi1_128(walls)
        else if w_height = 256 then
          R_DrawBatchColumnHi1_256(walls)
        else if w_height = 512 then
          R_DrawBatchColumnHi1_512(walls)
        else {$ENDIF}
          R_DrawBatchColumnHi1_TC(walls);
        walls.numwalls := 0;
      end;
  else  // JVAL: unreachable code
    begin
      w := @walls.walls[0];
      for i := 0 to walls.numwalls - 1 do
      begin
        R_DrawSingleThreadWall32(w);
        inc(w);
      end;
      walls.numwalls := 0;
    end;
  end;
end;
//==============================================================================
//
// R_FlashWallColumns32
//
//==============================================================================
procedure R_FlashWallColumns32(const idx: PInteger);
var
  walls: Pbatchwallrenderinfo32_t;
begin
  walls := @wallcache32[idx^];
  if walls.numwalls = 0 then
    exit;

  if usemultithread then
  begin
    R_AddWallsToCache32(idx);
    exit;
  end;

  R_RenderWall32(walls);
end;

//==============================================================================
//
// R_StoreWallColumn32
//
//==============================================================================
procedure R_StoreWallColumn32(const idx: PInteger);
var
  w: Pwallrenderinfo32_t;
  walls: Pbatchwallrenderinfo32_t;
  nwalls: integer;
  w_height: integer;
begin
  walls := @wallcache32[idx^];
  nwalls := walls.numwalls;
  w_height := dc_height;
  if nwalls > 0 then
    if (walls.walls[nwalls - 1].dc_x + 1 <> dc_x) or (walls.dc_height <> w_height) then
    begin
      R_FlashWallColumns32(idx);
      walls := @wallcache32[idx^];
      nwalls := 0;
    end;

  w := @walls.walls[nwalls];
  w.dc_source32 := dc_source32;
  w.dc_texturefactorbits := dc_texturefactorbits;
  w.dc_yh := dc_yh;
  w.dc_yl := dc_yl;
  w.dc_x := dc_x;
  w.dc_iscale := dc_iscale;
  w.dc_texturemid := dc_texturemid;
  w.dc_lightlevel := dc_lightlevel;
  w.dc_fog := dc_fog; // JVAL: Mars fog sectors
  walls.dc_height := w_height;
  inc(walls.numwalls);
  if walls.numwalls = MAXBATCHWALLS then
  begin
    if usemultithread then
      R_AddWallsToCache32(idx)
    else
    begin
      {$IFNDEF OPTIMIZE_FOR_SIZE}
      if dc_height = 128 then
        R_DrawBatchColumnHi8_128(walls)
      else if dc_height = 256 then
        R_DrawBatchColumnHi8_256(walls)
      else if dc_height = 512 then
        R_DrawBatchColumnHi8_512(walls)
      else {$ENDIF}
        R_DrawBatchColumnHi8_TC(walls);
      walls.numwalls := 0;
    end
  end;
end;

//==============================================================================
//
// _wall_thread_worker32
//
//==============================================================================
function _wall_thread_worker32(parms: mt_linkedrange_p): integer; stdcall;
var
  start, stop, part: integer;
  i: integer;
begin
  while parms.start <= parms.stop do
  begin
    R_RenderWall32(@wallcache32[parms.start]);
    Inc(parms.start);
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
        R_RenderWall32(@wallcache32[i]);
    end
    else if part < 1 then
      Break;
  end;

  result := 0;
end;

//==============================================================================
//
// R_InitWallsCache32
//
//==============================================================================
procedure R_InitWallsCache32;
var
  i: integer;
begin
  wallcache32 := nil;
  wallcachesize32 := 0;
  wallcacherealsize32 := 0;
  R_GrowWallsCache32;
  midwalls32 := 0;
  lowerwalls32 := 1;
  upperwalls32 := 2;
  midwalls32b := 3;
  lowerwalls32b := 4;
  upperwalls32b := 5;
  wallcache32[midwalls32].numwalls := 0;
  wallcache32[lowerwalls32].numwalls := 0;
  wallcache32[upperwalls32].numwalls := 0;
  wallcache32[midwalls32b].numwalls := 0;
  wallcache32[lowerwalls32b].numwalls := 0;
  wallcache32[upperwalls32b].numwalls := 0;
  wallcachesize32 := 6;
end;

//==============================================================================
//
// R_ShutDownWallsCache32
//
//==============================================================================
procedure R_ShutDownWallsCache32;
begin
  memfree(Pointer(wallcache32), wallcacherealsize32 * SizeOf(batchwallrenderinfo32_t));
end;

//==============================================================================
//
// R_ClearWallsCache32
//
//==============================================================================
procedure R_ClearWallsCache32;
begin
  midwalls32 := 0;
  lowerwalls32 := 1;
  upperwalls32 := 2;
  // JVAL: 3d Floors
  midwalls32b := 3;
  lowerwalls32b := 4;
  upperwalls32b := 5;
  wallcachesize32 := 6;
end;

//==============================================================================
//
// R_RenderMultiThreadWallParams32
//
//==============================================================================
procedure R_RenderMultiThreadWallParams32(const parms: mt_linkedrange_pa);
var
  i: integer;
  step: float;
begin
  step := wallcachesize32 / numrenderingthreads;
  parms[0].start := 0;
  for i := 1 to numrenderingthreads - 1 do
    parms[i].start := Round(step * i);
  for i := 0 to numrenderingthreads - 2 do
    parms[i].stop := parms[i + 1].start - 1;
  parms[numrenderingthreads - 1].stop := wallcachesize32 - 1;
end;

end.

