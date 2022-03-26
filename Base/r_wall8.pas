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
//  Multithreading wall rendering - 8 bit color
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit r_wall8;

interface

uses
  d_delphi,
  m_fixed,
  r_cache_walls;

type
  wallrenderinfo8_t = record
    dc_source: PByteArray;
    dc_colormap: PByteArray;
    dc_yh, dc_yl, dc_x: Integer;
    dc_iscale: fixed_t;
    dc_texturemid: fixed_t;
  end;
  Pwallrenderinfo8_t = ^wallrenderinfo8_t;

  batchwallrenderinfo8_t = record
    numwalls: integer;
    dc_height: integer;
    walls: array[0..MAXBATCHWALLS - 1] of wallrenderinfo8_t;
  end;
  Pbatchwallrenderinfo8_t = ^batchwallrenderinfo8_t;

  batchwallrenderinfo8_tArray = array[0..$FFF] of batchwallrenderinfo8_t;
  Pbatchwallrenderinfo8_tArray = ^batchwallrenderinfo8_tArray;

//==============================================================================
//
// R_StoreWallColumn8
//
//==============================================================================
procedure R_StoreWallColumn8(const idx: PInteger);

//==============================================================================
//
// R_FlashWallColumns8
//
//==============================================================================
procedure R_FlashWallColumns8(const idx: PInteger);

//==============================================================================
//
// R_InitWallsCache8
//
//==============================================================================
procedure R_InitWallsCache8;

//==============================================================================
//
// R_ShutDownWallsCache8
//
//==============================================================================
procedure R_ShutDownWallsCache8;

//==============================================================================
//
// R_ClearWallsCache8
//
//==============================================================================
procedure R_ClearWallsCache8;

//==============================================================================
//
// R_RenderMultiThreadWalls8
//
//==============================================================================
procedure R_RenderMultiThreadWalls8;

//==============================================================================
//
// R_WaitWallsCache8
//
//==============================================================================
procedure R_WaitWallsCache8;

var
  midwalls8: integer;
  lowerwalls8: integer;
  upperwalls8: integer;
  // JVAL: 3d Floors
  midwalls8b: integer;
  lowerwalls8b: integer;
  upperwalls8b: integer;

var
  force_numwallrenderingthreads_8bit: integer = 0;

implementation

uses
  doomdef,
  i_system,
  i_threads,
  r_column,
  r_tallcolumn,
  r_draw,
  r_main;

{$IFNDEF OPTIMIZE_FOR_SIZE}

{$DEFINE WALL8_128}
{$UNDEF WALL8_256}
{$UNDEF WALL8_512}
{$UNDEF WALL8_TC}
{$I R_Wall8_BatchFuncs.inc}

{$UNDEF WALL8_128}
{$DEFINE WALL8_256}
{$UNDEF WALL8_512}
{$UNDEF WALL8_TC}
{$I R_Wall8_BatchFuncs.inc}

{$UNDEF WALL8_128}
{$UNDEF WALL8_256}
{$DEFINE WALL8_512}
{$UNDEF WALL8_TC}
{$I R_Wall8_BatchFuncs.inc}

{$ENDIF}

{$UNDEF WALL8_128}
{$UNDEF WALL8_256}
{$UNDEF WALL8_512}
{$DEFINE WALL8_TC}
{$I R_Wall8_BatchFuncs.inc}

var
  wallcache: Pbatchwallrenderinfo8_tArray;
  wallcachesize: integer;
  wallcacherealsize: integer;

//==============================================================================
//
// R_GrowWallsCache8
//
//==============================================================================
procedure R_GrowWallsCache8;
begin
  if wallcachesize >= wallcacherealsize then
  begin
    realloc(Pointer(wallcache), wallcacherealsize * SizeOf(batchwallrenderinfo8_t), (64 + wallcacherealsize) * SizeOf(batchwallrenderinfo8_t));
    wallcacherealsize := wallcacherealsize + 64;
  end;
end;

//==============================================================================
//
// R_AddWallsToCache8
//
//==============================================================================
procedure R_AddWallsToCache8(const idx: PInteger);
begin
  R_GrowWallsCache8;
  idx^ := wallcachesize;
  wallcache[wallcachesize].numwalls := 0;
  inc(wallcachesize);
end;

//==============================================================================
//
// R_RenderWall8
//
//==============================================================================
procedure R_RenderWall8(walls: Pbatchwallrenderinfo8_t);
var
  nwalls: integer;
  {$IFNDEF OPTIMIZE_FOR_SIZE}
  w_height: integer;
  {$ENDIF}
begin
  nwalls := walls.numwalls;
  {$IFNDEF OPTIMIZE_FOR_SIZE}
  w_height := walls.dc_height;
  if w_height = 128 then
  begin
    if nwalls = MAXBATCHWALLS then
    begin
      R_DrawBatchColumn8_128(walls);
      walls.numwalls := 0;
    end
    else case nwalls of
      7:
        begin
          R_DrawBatchColumn7_128(walls);
          walls.numwalls := 0;
        end;
      6:
        begin
          R_DrawBatchColumn6_128(walls);
          walls.numwalls := 0;
        end;
      5:
        begin
          R_DrawBatchColumn5_128(walls);
          walls.numwalls := 0;
        end;
      4:
        begin
          R_DrawBatchColumn4_128(walls);
          walls.numwalls := 0;
        end;
      3:
        begin
          R_DrawBatchColumn3_128(walls);
          walls.numwalls := 0;
        end;
      2:
        begin
          R_DrawBatchColumn2_128(walls);
          walls.numwalls := 0;
        end;
      1:
        begin
          R_DrawBatchColumn1_128(walls);
          walls.numwalls := 0;
        end;
    end;
  end
  else if w_height = 256 then
  begin
    if nwalls = MAXBATCHWALLS then
    begin
      R_DrawBatchColumn8_256(walls);
      walls.numwalls := 0;
    end
    else case nwalls of
      7:
        begin
          R_DrawBatchColumn7_256(walls);
          walls.numwalls := 0;
        end;
      6:
        begin
          R_DrawBatchColumn6_256(walls);
          walls.numwalls := 0;
        end;
      5:
        begin
          R_DrawBatchColumn5_256(walls);
          walls.numwalls := 0;
        end;
      4:
        begin
          R_DrawBatchColumn4_256(walls);
          walls.numwalls := 0;
        end;
      3:
        begin
          R_DrawBatchColumn3_256(walls);
          walls.numwalls := 0;
        end;
      2:
        begin
          R_DrawBatchColumn2_256(walls);
          walls.numwalls := 0;
        end;
      1:
        begin
          R_DrawBatchColumn1_256(walls);
          walls.numwalls := 0;
        end;
    end;
  end
  else if w_height = 512 then
  begin
    if nwalls = MAXBATCHWALLS then
    begin
      R_DrawBatchColumn8_512(walls);
      walls.numwalls := 0;
    end
    else case nwalls of
      7:
        begin
          R_DrawBatchColumn7_512(walls);
          walls.numwalls := 0;
        end;
      6:
        begin
          R_DrawBatchColumn6_512(walls);
          walls.numwalls := 0;
        end;
      5:
        begin
          R_DrawBatchColumn5_512(walls);
          walls.numwalls := 0;
        end;
      4:
        begin
          R_DrawBatchColumn4_512(walls);
          walls.numwalls := 0;
        end;
      3:
        begin
          R_DrawBatchColumn3_512(walls);
          walls.numwalls := 0;
        end;
      2:
        begin
          R_DrawBatchColumn2_512(walls);
          walls.numwalls := 0;
        end;
      1:
        begin
          R_DrawBatchColumn1_512(walls);
          walls.numwalls := 0;
        end;
    end;
  end
  else {$ENDIF}
  begin
    if nwalls = MAXBATCHWALLS then
    begin
      R_DrawBatchColumn8_TC(walls);
      walls.numwalls := 0;
    end
    else case nwalls of
      7:
        begin
          R_DrawBatchColumn7_TC(walls);
          walls.numwalls := 0;
        end;
      6:
        begin
          R_DrawBatchColumn6_TC(walls);
          walls.numwalls := 0;
        end;
      5:
        begin
          R_DrawBatchColumn5_TC(walls);
          walls.numwalls := 0;
        end;
      4:
        begin
          R_DrawBatchColumn4_TC(walls);
          walls.numwalls := 0;
        end;
      3:
      begin
        R_DrawBatchColumn3_TC(walls);
        walls.numwalls := 0;
      end;
      2:
        begin
          R_DrawBatchColumn2_TC(walls);
          walls.numwalls := 0;
        end;
      1:
        begin
          R_DrawBatchColumn1_TC(walls);
          walls.numwalls := 0;
        end;
    end;
  end;
end;

//==============================================================================
//
// R_FlashWallColumns8
//
//==============================================================================
procedure R_FlashWallColumns8(const idx: PInteger);
var
  walls: Pbatchwallrenderinfo8_t;
begin
  walls := @wallcache[idx^];
  if walls.numwalls = 0 then
    exit;

  if usemultithread then
  begin
    R_AddWallsToCache8(idx);
    exit;
  end;

  R_RenderWall8(walls);
end;

//==============================================================================
//
// R_StoreWallColumn8
//
//==============================================================================
procedure R_StoreWallColumn8(const idx: PInteger);
var
  w: Pwallrenderinfo8_t;
  walls: Pbatchwallrenderinfo8_t;
  nwalls: integer;
  w_height: integer;
begin
  walls := @wallcache[idx^];
  nwalls := walls.numwalls;
  w_height := dc_height;
  if nwalls > 0 then
    if (walls.walls[nwalls - 1].dc_x + 1 <> dc_x) or (walls.dc_height <> w_height) then
    begin
      R_FlashWallColumns8(idx);
      walls := @wallcache[idx^];
      nwalls := 0;
    end;

  w := @walls.walls[nwalls];
  w.dc_source := dc_source;
  w.dc_colormap := dc_colormap;
  w.dc_yh := dc_yh;
  w.dc_yl := dc_yl;
  w.dc_x := dc_x;
  w.dc_iscale := dc_iscale;
  w.dc_texturemid := dc_texturemid;
  walls.dc_height := w_height;
  inc(walls.numwalls);
  if walls.numwalls = MAXBATCHWALLS then
  begin
    if usemultithread then
      R_AddWallsToCache8(idx)
    else
    begin
      {$IFNDEF OPTIMIZE_FOR_SIZE}
      if dc_height = 128 then
        R_DrawBatchColumn8_128(walls)
      else if dc_height = 256 then
        R_DrawBatchColumn8_256(walls)
      else if dc_height = 512 then
        R_DrawBatchColumn8_512(walls)
      else {$ENDIF}
        R_DrawBatchColumn8_TC(walls);
      walls.numwalls := 0;
    end
  end;
end;

const
  MAXWALLTHREADS8 = 256;

var
  wallthreads8: array[0..MAXWALLTHREADS8 - 1] of TDThread;
  numwallthreads8: Integer = 0;

type
  Pwallthreadparms8_t = ^wallthreadparms8_t;
  wallthreadparms8_t = record
    start, stop: integer;
    next: Pwallthreadparms8_t;
  end;

//==============================================================================
//
// _wall_thread_worker8
//
//==============================================================================
function _wall_thread_worker8(parms: Pwallthreadparms8_t): integer; stdcall;
var
  start, stop, part: integer;
  i: integer;
begin
  while parms.start <= parms.stop do
  begin
    R_RenderWall8(@wallcache[parms.start]);
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
        R_RenderWall8(@wallcache[i]);
    end
    else if part < 1 then
      Break;
  end;

  result := 0;
end;

var
  default_numwallrenderingthreads_8bit: integer = 0;

//==============================================================================
//
// R_InitWallsCache8
//
//==============================================================================
procedure R_InitWallsCache8;
var
  i: integer;
begin
  wallcache := nil;
  wallcachesize := 0;
  wallcacherealsize := 0;
  R_GrowWallsCache8;
  midwalls8 := 0;
  lowerwalls8 := 1;
  upperwalls8 := 2;
  // JVAL: 3d Floors
  midwalls8b := 3;
  lowerwalls8b := 4;
  upperwalls8b := 5;
  wallcache[midwalls8].numwalls := 0;
  wallcache[lowerwalls8].numwalls := 0;
  wallcache[upperwalls8].numwalls := 0;
  wallcache[midwalls8b].numwalls := 0;
  wallcache[lowerwalls8b].numwalls := 0;
  wallcache[upperwalls8b].numwalls := 0;
  wallcachesize := 6;

  if force_numwallrenderingthreads_8bit > 0 then
    numwallthreads8 := force_numwallrenderingthreads_8bit
  else
    numwallthreads8 := I_GetNumCPUs - 1;

  if numwallthreads8 < 1 then
    numwallthreads8 := 1
  else if numwallthreads8 > MAXWALLTHREADS8 then
    numwallthreads8 := MAXWALLTHREADS8;

  default_numwallrenderingthreads_8bit := numwallthreads8;
  for i := 0 to numwallthreads8 - 1 do
    wallthreads8[i] := TDThread.Create(@_wall_thread_worker8);
end;

//==============================================================================
//
// R_ShutDownWallsCache8
//
//==============================================================================
procedure R_ShutDownWallsCache8;
var
  i: integer;
begin
  for i := 0 to numwallthreads8 - 1 do
    wallthreads8[i].Free;

  memfree(Pointer(wallcache), wallcacherealsize * SizeOf(batchwallrenderinfo8_t));
end;

//==============================================================================
//
// R_ClearWallsCache8
//
//==============================================================================
procedure R_ClearWallsCache8;
begin
  midwalls8 := 0;
  lowerwalls8 := 1;
  upperwalls8 := 2;
  // JVAL: 3d Floors
  midwalls8b := 3;
  lowerwalls8b := 4;
  upperwalls8b := 5;
  wallcachesize := 6;
end;

var
  parms: array[0..MAXWALLTHREADS8 - 1] of wallthreadparms8_t;

//==============================================================================
//
// R_RenderMultiThreadWalls8
//
//==============================================================================
procedure R_RenderMultiThreadWalls8;
var
  i: integer;
  newnumthreads: integer;
  step: float;
begin
  if force_numwallrenderingthreads_8bit > 0 then
  begin
    if force_numwallrenderingthreads_8bit <> numwallthreads8 then
    begin
      newnumthreads := force_numwallrenderingthreads_8bit;
      if newnumthreads > MAXWALLTHREADS8 then
      begin
        newnumthreads := MAXWALLTHREADS8;
        force_numwallrenderingthreads_8bit := MAXWALLTHREADS8;
      end;
    end
    else
      newnumthreads := numwallthreads8;
  end
  else
    newnumthreads := default_numwallrenderingthreads_8bit;

  if newnumthreads <= 0 then
  begin
    newnumthreads := I_GetNumCPUs - 1;
    if newnumthreads <= 0 then
      newnumthreads := 1;
  end;

  if newnumthreads <> numwallthreads8 then
  begin
    for i := numwallthreads8 to newnumthreads - 1 do
      wallthreads8[i] := TDThread.Create(@_wall_thread_worker8);
    for i := newnumthreads to numwallthreads8 - 1 do
      wallthreads8[i].Free;
    numwallthreads8 := newnumthreads;
  end;

  step := wallcachesize / numwallthreads8;
  parms[0].start := 0;
  for i := 1 to numwallthreads8 - 1 do
    parms[i].start := Round(step * i);
  for i := 0 to numwallthreads8 - 2 do
    parms[i].stop := parms[i + 1].start - 1;
  parms[numwallthreads8 - 1].stop := wallcachesize - 1;

  for i := 0 to numwallthreads8 - 2 do
    parms[i].next := @parms[i + 1];
  parms[numwallthreads8 - 1].next := @parms[0];

  for i := 0 to numwallthreads8 - 1 do
    if parms[i].start <= parms[i].stop then
      wallthreads8[i].Activate(@parms[i]);
end;

//==============================================================================
//
// R_WaitWallsCache8
//
//==============================================================================
procedure R_WaitWallsCache8;
var
  doneid: integer;

  function _alldone: boolean;
  var
    i: integer;
    ret: boolean;
  begin
    result := true;
    for i := doneid to numwallthreads8 - 1 do
    begin
      ret := wallthreads8[i].CheckJobDone;
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

