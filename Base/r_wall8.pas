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
  mt_utils,
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
// R_RenderMultiThreadWallParams8
//
//==============================================================================
procedure R_RenderMultiThreadWallParams8(const parms: mt_linkedrange_pa);

var
  midwalls8: integer;
  lowerwalls8: integer;
  upperwalls8: integer;
  // JVAL: 3d Floors
  midwalls8b: integer;
  lowerwalls8b: integer;
  upperwalls8b: integer;

implementation

uses
  doomdef,
  i_system,
  i_threads,
  r_column,
  r_tallcolumn,
  r_draw,
  r_render,
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
  wallcache8: Pbatchwallrenderinfo8_tArray;
  wallcachesize8: integer;
  wallcacherealsize8: integer;

//==============================================================================
//
// R_GrowWallsCache8
//
//==============================================================================
procedure R_GrowWallsCache8;
begin
  if wallcachesize8 >= wallcacherealsize8 then
  begin
    realloc(Pointer(wallcache8), wallcacherealsize8 * SizeOf(batchwallrenderinfo8_t), (64 + wallcacherealsize8) * SizeOf(batchwallrenderinfo8_t));
    wallcacherealsize8 := wallcacherealsize8 + 64;
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
  idx^ := wallcachesize8;
  wallcache8[wallcachesize8].numwalls := 0;
  inc(wallcachesize8);
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
  walls := @wallcache8[idx^];
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
  walls := @wallcache8[idx^];
  nwalls := walls.numwalls;
  w_height := dc_height;
  if nwalls > 0 then
    if (walls.walls[nwalls - 1].dc_x + 1 <> dc_x) or (walls.dc_height <> w_height) then
    begin
      R_FlashWallColumns8(idx);
      walls := @wallcache8[idx^];
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

//==============================================================================
//
// _wall_thread_worker8
//
//==============================================================================
function _wall_thread_worker8(parms: mt_linkedrange_p): integer; stdcall;
var
  start, stop, part: integer;
  i: integer;
begin
  while parms.start <= parms.stop do
  begin
    R_RenderWall8(@wallcache8[parms.start]);
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
        R_RenderWall8(@wallcache8[i]);
    end
    else if part < 1 then
      Break;
  end;

  result := 0;
end;

//==============================================================================
//
// R_InitWallsCache8
//
//==============================================================================
procedure R_InitWallsCache8;
var
  i: integer;
begin
  wallcache8 := nil;
  wallcachesize8 := 0;
  wallcacherealsize8 := 0;
  R_GrowWallsCache8;
  midwalls8 := 0;
  lowerwalls8 := 1;
  upperwalls8 := 2;
  // JVAL: 3d Floors
  midwalls8b := 3;
  lowerwalls8b := 4;
  upperwalls8b := 5;

  wallcache8[midwalls8].numwalls := 0;
  wallcache8[lowerwalls8].numwalls := 0;
  wallcache8[upperwalls8].numwalls := 0;
  wallcache8[midwalls8b].numwalls := 0;
  wallcache8[lowerwalls8b].numwalls := 0;
  wallcache8[upperwalls8b].numwalls := 0;
  wallcachesize8 := 6;
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
  memfree(Pointer(wallcache8), wallcacherealsize8 * SizeOf(batchwallrenderinfo8_t));
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
  wallcachesize8 := 6;
end;

//==============================================================================
//
// R_RenderMultiThreadWalls8
//
//==============================================================================
procedure R_RenderMultiThreadWallParams8(const parms: mt_linkedrange_pa);
var
  i: integer;
  step: float;
begin
  step := wallcachesize8 / numrenderingthreads;
  parms[0].start := 0;
  for i := 1 to numrenderingthreads - 1 do
    parms[i].start := Round(step * i);
  for i := 0 to numrenderingthreads - 2 do
    parms[i].stop := parms[i + 1].start - 1;
  parms[numrenderingthreads - 1].stop := wallcachesize8 - 1;
end;

end.

