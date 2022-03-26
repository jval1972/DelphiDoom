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
// R_RenderMultiThreadWalls32
//
//==============================================================================
procedure R_RenderMultiThreadWalls32;

//==============================================================================
//
// R_WaitWallsCache32
//
//==============================================================================
procedure R_WaitWallsCache32;

var
  midwalls32: integer;
  lowerwalls32: integer;
  upperwalls32: integer;
  // JVAL: 3d Floors
  midwalls32b: integer;
  lowerwalls32b: integer;
  upperwalls32b: integer;

var
  force_numwallrenderingthreads_32bit: integer = 0;

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
  wallcache: Pbatchwallrenderinfo32_tArray;
  wallcachesize: integer;
  wallcacherealsize: integer;

//==============================================================================
//
// R_GrowWallsCache32
//
//==============================================================================
procedure R_GrowWallsCache32;
begin
  if wallcachesize >= wallcacherealsize then
  begin
    realloc(Pointer(wallcache), wallcacherealsize * SizeOf(batchwallrenderinfo32_t), (64 + wallcacherealsize) * SizeOf(batchwallrenderinfo32_t));
    wallcacherealsize := wallcacherealsize + 64;
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
  idx^ := wallcachesize;
  wallcache[wallcachesize].numwalls := 0;
  inc(wallcachesize);
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
  walls := @wallcache[idx^];
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
  walls := @wallcache[idx^];
  nwalls := walls.numwalls;
  w_height := dc_height;
  if nwalls > 0 then
    if (walls.walls[nwalls - 1].dc_x + 1 <> dc_x) or (walls.dc_height <> w_height) then
    begin
      R_FlashWallColumns32(idx);
      walls := @wallcache[idx^];
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

const
  MAXWALLTHREADS32 = 256;

var
  wallthreads32: array[0..MAXWALLTHREADS32 - 1] of TDThread;
  numwallthreads32: Integer = 0;

type
  Pwallthreadparms32_t = ^wallthreadparms32_t;
  wallthreadparms32_t = record
    start, stop: integer;
    next: Pwallthreadparms32_t;
  end;

//==============================================================================
//
// _wall_thread_worker32
//
//==============================================================================
function _wall_thread_worker32(parms: Pwallthreadparms32_t): integer; stdcall;
var
  start, stop, part: integer;
  i: integer;
begin
  while parms.start <= parms.stop do
  begin
    R_RenderWall32(@wallcache[parms.start]);
    ThreadInc(parms.start);
  end;

  while true do
  begin
    parms := parms.next;
    start := parms.start;
    stop := parms.stop;
    part := (stop - start) div 2;
    if part > 2 then
    begin
      ThreadSet(parms.stop, parms.stop - part);
      start := parms.stop + 1;
      for i := start to stop do
        R_RenderWall32(@wallcache[i]);
    end
    else if part < 1 then
      Break;
  end;

  result := 0;
end;

var
  default_numwallrenderingthreads_32bit: integer = 0;

//==============================================================================
//
// R_InitWallsCache32
//
//==============================================================================
procedure R_InitWallsCache32;
var
  i: integer;
begin
  wallcache := nil;
  wallcachesize := 0;
  wallcacherealsize := 0;
  R_GrowWallsCache32;
  midwalls32 := 0;
  lowerwalls32 := 1;
  upperwalls32 := 2;
  midwalls32b := 3;
  lowerwalls32b := 4;
  upperwalls32b := 5;
  wallcache[midwalls32].numwalls := 0;
  wallcache[lowerwalls32].numwalls := 0;
  wallcache[upperwalls32].numwalls := 0;
  wallcache[midwalls32b].numwalls := 0;
  wallcache[lowerwalls32b].numwalls := 0;
  wallcache[upperwalls32b].numwalls := 0;
  wallcachesize := 6;

  if force_numwallrenderingthreads_32bit > 0 then
    numwallthreads32 := force_numwallrenderingthreads_32bit
  else
    numwallthreads32 := I_GetNumCPUs - 1;

  if numwallthreads32 < 1 then
    numwallthreads32 := 1
  else if numwallthreads32 > MAXWALLTHREADS32 then
    numwallthreads32 := MAXWALLTHREADS32;

  default_numwallrenderingthreads_32bit := numwallthreads32;
  for i := 0 to numwallthreads32 - 1 do
    wallthreads32[i] := TDThread.Create(@_wall_thread_worker32);
end;

//==============================================================================
//
// R_ShutDownWallsCache32
//
//==============================================================================
procedure R_ShutDownWallsCache32;
var
  i: integer;
begin
  for i := 0 to numwallthreads32 - 1 do
    wallthreads32[i].Free;

  memfree(Pointer(wallcache), wallcacherealsize * SizeOf(batchwallrenderinfo32_t));
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
  wallcachesize := 6;
end;

var
  parms: array[0..MAXWALLTHREADS32 - 1] of wallthreadparms32_t;

//==============================================================================
//
// R_RenderMultiThreadWalls32
//
//==============================================================================
procedure R_RenderMultiThreadWalls32;
var
  i: integer;
  newnumthreads: integer;
  step: float;
begin
  if force_numwallrenderingthreads_32bit > 0 then
  begin
    if force_numwallrenderingthreads_32bit <> numwallthreads32 then
    begin
      newnumthreads := force_numwallrenderingthreads_32bit;
      if newnumthreads > MAXWALLTHREADS32 then
      begin
        newnumthreads := MAXWALLTHREADS32;
        force_numwallrenderingthreads_32bit := MAXWALLTHREADS32;
      end;
    end
    else
      newnumthreads := numwallthreads32;
  end
  else
    newnumthreads := default_numwallrenderingthreads_32bit;

  if newnumthreads <= 0 then
  begin
    newnumthreads := I_GetNumCPUs - 1;
    if newnumthreads <= 0 then
      newnumthreads := 1;
  end;

  if newnumthreads <> numwallthreads32 then
  begin
    for i := numwallthreads32 to newnumthreads - 1 do
      wallthreads32[i] := TDThread.Create(@_wall_thread_worker32);
    for i := newnumthreads to numwallthreads32 - 1 do
      wallthreads32[i].Free;
    numwallthreads32 := newnumthreads;
  end;

  step := wallcachesize / numwallthreads32;
  parms[0].start := 0;
  for i := 1 to numwallthreads32 - 1 do
    parms[i].start := Round(step * i);
  for i := 0 to numwallthreads32 - 2 do
    parms[i].stop := parms[i + 1].start - 1;
  parms[numwallthreads32 - 1].stop := wallcachesize - 1;

  for i := 0 to numwallthreads32 - 2 do
    parms[i].next := @parms[i + 1];
  parms[numwallthreads32 - 1].next := @parms[0];

  for i := 0 to numwallthreads32 - 1 do
    if parms[i].start <= parms[i].stop then
      wallthreads32[i].Activate(@parms[i]);
end;

//==============================================================================
//
// R_WaitWallsCache32
//
//==============================================================================
procedure R_WaitWallsCache32;
var
  doneid: integer;

  function _alldone: boolean;
  var
    i: integer;
    ret: boolean;
  begin
    result := true;
    for i := doneid to numwallthreads32 - 1 do
    begin
      ret := wallthreads32[i].CheckJobDone;
      result := ret and result;
      if not result then
      begin
        doneid := i;
        exit;
      end;
    end;
  end;

begin
  doneid := 0;
  while not _alldone do
    I_Sleep(0);
end;

end.

