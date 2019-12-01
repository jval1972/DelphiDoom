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
//  Site  : http://delphidoom.sitesled.com/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit r_wall8;

interface

uses
  d_delphi,
  m_fixed,
  r_ccache;

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
    walls: array[0..MAXBATCHWALLS - 1] of wallrenderinfo8_t;
  end;
  Pbatchwallrenderinfo8_t = ^batchwallrenderinfo8_t;

  batchwallrenderinfo8_tArray = array[0..$FFFF] of batchwallrenderinfo8_t;
  Pbatchwallrenderinfo8_tArray = ^batchwallrenderinfo8_tArray;

procedure R_StoreWallColumn8(const walls: Pbatchwallrenderinfo8_t);

procedure R_FlashWallColumns8(const walls: Pbatchwallrenderinfo8_t);

procedure R_InitWallsCache8;

procedure R_ShutDownWallsCache8;

procedure R_ClearWallsCache8;

procedure R_RenderMultiThreadWalls8;

procedure R_WaitWallsCache8;

var
  midwalls8: batchwallrenderinfo8_t = (numwalls: 0);
  lowerwalls8: batchwallrenderinfo8_t = (numwalls: 0);
  upperwalls8: batchwallrenderinfo8_t = (numwalls: 0);

implementation

uses
  {$IFDEF HEXEN}
  xn_defs,
  {$ELSE}
  doomdef,
  {$ENDIF}
  i_system,
  i_threads,
  r_column,
  r_draw,
  r_precalc,
  r_main;

procedure R_StoreWallColumn8(const walls: Pbatchwallrenderinfo8_t);
var
  w: Pwallrenderinfo8_t;
begin
  w := @walls.walls[walls.numwalls];
  w.dc_source := dc_source;
  w.dc_colormap := dc_colormap;
  w.dc_yh := dc_yh;
  w.dc_yl := dc_yl;
  w.dc_x := dc_x;
  w.dc_iscale := dc_iscale;
  w.dc_texturemid := dc_texturemid;
  Inc(walls.numwalls);
  if walls.numwalls = MAXBATCHWALLS then
    R_FlashWallColumns8(walls);
end;

procedure R_DrawBatchColumn(const walls: Pbatchwallrenderinfo8_t);
var
  w: Pwallrenderinfo8_t;
  ypos: integer;
  dest: PByte;
  frac1, frac2, frac3, frac4, frac5, frac6, frac7, frac8: fixed_t;
  fracstep1, fracstep2, fracstep3, fracstep4, fracstep5, fracstep6, fracstep7, fracstep8: fixed_t;
  swidth: integer;
  swidth2: integer;

  dc_source1: PByteArray;
  dc_source2: PByteArray;
  dc_source3: PByteArray;
  dc_source4: PByteArray;
  dc_source5: PByteArray;
  dc_source6: PByteArray;
  dc_source7: PByteArray;
  dc_source8: PByteArray;

  dc_colormap1: PByteArray;
  dc_colormap2: PByteArray;
  dc_colormap3: PByteArray;
  dc_colormap4: PByteArray;
  dc_colormap5: PByteArray;
  dc_colormap6: PByteArray;
  dc_colormap7: PByteArray;
  dc_colormap8: PByteArray;

  xx: integer;
  i: integer;

  min_yh, max_yl: integer;
  max_yh, min_yl: integer;

  buf: fourbytes;
begin
  w := @walls.walls[0];
  min_yh := w.dc_yh;
  max_yl := w.dc_yl;
  max_yh := w.dc_yh;
  min_yl := w.dc_yl;

  for i := 1 to MAXBATCHWALLS - 1 do
  begin
    Inc(w);
    if w.dc_yh < min_yh then
      min_yh := w.dc_yh;
    if w.dc_yl > max_yl then
      max_yl := w.dc_yl;
    if w.dc_yh > max_yh then
      max_yh := w.dc_yh;
    if w.dc_yl < min_yl then
      min_yl := w.dc_yl;
  end;

  if max_yl < 0 then
    max_yl := 0
  else if max_yl >= SCREENHEIGHT then
    max_yl := SCREENHEIGHT - 1;
  if min_yl < 0 then
    min_yl := 0
  else if min_yl >= SCREENHEIGHT then
    min_yl := SCREENHEIGHT - 1;
  if min_yh < 0 then
    min_yh := 0
  else if min_yh >= SCREENHEIGHT then
    min_yh := SCREENHEIGHT - 1;
  if max_yh < 0 then
    max_yh := 0
  else if max_yh >= SCREENHEIGHT then
    max_yh := SCREENHEIGHT - 1;

  w := @walls.walls[0];
  dest := @((ylookup[min_yl]^)[columnofs[w.dc_x]]);

  xx := min_yl - centery;

  fracstep1 := w.dc_iscale;
  frac1 := w.dc_texturemid + xx * fracstep1;
  dc_source1 := w.dc_source;
  dc_colormap1 := w.dc_colormap;
  Inc(w);

  fracstep2 := w.dc_iscale;
  frac2 := w.dc_texturemid + xx * fracstep2;
  dc_source2 := w.dc_source;
  dc_colormap2 := w.dc_colormap;
  Inc(w);

  fracstep3 := w.dc_iscale;
  frac3 := w.dc_texturemid + xx * fracstep3;
  dc_source3 := w.dc_source;
  dc_colormap3 := w.dc_colormap;
  Inc(w);

  fracstep4 := w.dc_iscale;
  frac4 := w.dc_texturemid + xx * fracstep4;
  dc_source4 := w.dc_source;
  dc_colormap4 := w.dc_colormap;
  Inc(w);

  fracstep5 := w.dc_iscale;
  frac5 := w.dc_texturemid + xx * fracstep5;
  dc_source5 := w.dc_source;
  dc_colormap5 := w.dc_colormap;
  Inc(w);

  fracstep6 := w.dc_iscale;
  frac6 := w.dc_texturemid + xx * fracstep6;
  dc_source6 := w.dc_source;
  dc_colormap6 := w.dc_colormap;
  Inc(w);

  fracstep7 := w.dc_iscale;
  frac7 := w.dc_texturemid + xx * fracstep7;
  dc_source7 := w.dc_source;
  dc_colormap7 := w.dc_colormap;
  Inc(w);

  fracstep8 := w.dc_iscale;
  frac8 := w.dc_texturemid + xx * fracstep8;
  dc_source8 := w.dc_source;
  dc_colormap8 := w.dc_colormap;

  swidth := SCREENWIDTH - (MAXBATCHWALLS - 1);
  swidth2 := SCREENWIDTH - (MAXBATCHWALLS - 4);

  ypos := min_yl;
  while ypos < max_yl do
  begin
    w := @walls.walls[0];
    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap1[dc_source1[(LongWord(frac1) shr FRACBITS) and 127]];
    inc(frac1, fracstep1);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap2[dc_source2[(LongWord(frac2) shr FRACBITS) and 127]];
    inc(frac2, fracstep2);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap3[dc_source3[(LongWord(frac3) shr FRACBITS) and 127]];
    inc(frac3, fracstep3);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap4[dc_source4[(LongWord(frac4) shr FRACBITS) and 127]];
    inc(frac4, fracstep4);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap5[dc_source5[(LongWord(frac5) shr FRACBITS) and 127]];
    inc(frac5, fracstep5);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap6[dc_source6[(LongWord(frac6) shr FRACBITS) and 127]];
    inc(frac6, fracstep6);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap7[dc_source7[(LongWord(frac7) shr FRACBITS) and 127]];
    inc(frac7, fracstep7);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap8[dc_source8[(LongWord(frac8) shr FRACBITS) and 127]];
    inc(frac8, fracstep8);

    dest := PByte(integer(dest) + swidth);
    inc(ypos);
  end;

  while ypos <= min_yh do
  begin
{    dest^ := dc_colormap1[dc_source1[(LongWord(frac1) shr FRACBITS) and 127]];
    inc(dest);
    inc(frac1, fracstep1);

    dest^ := dc_colormap2[dc_source2[(LongWord(frac2) shr FRACBITS) and 127]];
    inc(dest);
    inc(frac2, fracstep2);

    dest^ := dc_colormap3[dc_source3[(LongWord(frac3) shr FRACBITS) and 127]];
    inc(dest);
    inc(frac3, fracstep3);

    dest^ := dc_colormap4[dc_source4[(LongWord(frac4) shr FRACBITS) and 127]];
    inc(dest);
    inc(frac4, fracstep4);

    dest^ := dc_colormap5[dc_source5[(LongWord(frac5) shr FRACBITS) and 127]];
    inc(dest);
    inc(frac5, fracstep5);

    dest^ := dc_colormap6[dc_source6[(LongWord(frac6) shr FRACBITS) and 127]];
    inc(dest);
    inc(frac6, fracstep6);

    dest^ := dc_colormap7[dc_source7[(LongWord(frac7) shr FRACBITS) and 127]];
    inc(dest);
    inc(frac7, fracstep7);

    dest^ := dc_colormap8[dc_source8[(LongWord(frac8) shr FRACBITS) and 127]];
    inc(frac8, fracstep8);

    dest := PByte(integer(dest) + swidth);
    inc(ypos);}

    buf.byte1 := dc_colormap1[dc_source1[(LongWord(frac1) shr FRACBITS) and 127]];
    inc(frac1, fracstep1);

    buf.byte2 := dc_colormap2[dc_source2[(LongWord(frac2) shr FRACBITS) and 127]];
    inc(frac2, fracstep2);

    buf.byte3 := dc_colormap3[dc_source3[(LongWord(frac3) shr FRACBITS) and 127]];
    inc(frac3, fracstep3);

    buf.byte4 := dc_colormap4[dc_source4[(LongWord(frac4) shr FRACBITS) and 127]];
    inc(frac4, fracstep4);
    PLongWord(dest)^ := PLongWord(@buf)^;
    inc(dest, 4);

    buf.byte1 := dc_colormap5[dc_source5[(LongWord(frac5) shr FRACBITS) and 127]];
    inc(frac5, fracstep5);

    buf.byte2 := dc_colormap6[dc_source6[(LongWord(frac6) shr FRACBITS) and 127]];
    inc(frac6, fracstep6);

    buf.byte3 := dc_colormap7[dc_source7[(LongWord(frac7) shr FRACBITS) and 127]];
    inc(frac7, fracstep7);

    buf.byte4 := dc_colormap8[dc_source8[(LongWord(frac8) shr FRACBITS) and 127]];
    inc(frac8, fracstep8);
    PLongWord(dest)^ := PLongWord(@buf)^;

    dest := PByte(integer(dest) + swidth2);
    inc(ypos);

  end;

  while ypos <= max_yh do
  begin
    w := @walls.walls[0];
    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap1[dc_source1[(LongWord(frac1) shr FRACBITS) and 127]];
    inc(frac1, fracstep1);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap2[dc_source2[(LongWord(frac2) shr FRACBITS) and 127]];
    inc(frac2, fracstep2);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap3[dc_source3[(LongWord(frac3) shr FRACBITS) and 127]];
    inc(frac3, fracstep3);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap4[dc_source4[(LongWord(frac4) shr FRACBITS) and 127]];
    inc(frac4, fracstep4);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap5[dc_source5[(LongWord(frac5) shr FRACBITS) and 127]];
    inc(frac5, fracstep5);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap6[dc_source6[(LongWord(frac6) shr FRACBITS) and 127]];
    inc(frac6, fracstep6);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap7[dc_source7[(LongWord(frac7) shr FRACBITS) and 127]];
    inc(frac7, fracstep7);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap8[dc_source8[(LongWord(frac8) shr FRACBITS) and 127]];
    inc(frac8, fracstep8);

    dest := PByte(integer(dest) + swidth);
    inc(ypos);
  end;

end;

const
  MAXWALLTHREADS8 = 16;

var
  wallcache: Pbatchwallrenderinfo8_tArray;
  wallcachesize: integer;
  wallcacherealsize: integer;
  wallthreads8: array[0..MAXWALLTHREADS8 - 1] of TDThread;
  numwallthreads8: Integer = 0;

type
  wallthreadparms8_t = record
    start, stop: integer;
  end;
  Pwallthreadparms8_t = ^wallthreadparms8_t;

function _wall_thread_worker8(parms: Pwallthreadparms8_t): integer; stdcall;
var
  i: integer;
  walls: Pbatchwallrenderinfo8_t;
begin
  for i := parms.start to parms.stop do
  begin
    walls := @wallcache[i];
    if walls.numwalls = MAXBATCHWALLS then
    begin
      R_DrawBatchColumn(walls);
      walls.numwalls := 0;
    end;
  end;
  result := 0;
end;


procedure R_InitWallsCache8;
var
  i: integer;
begin
  wallcache := nil;
  wallcachesize := 0;
  wallcacherealsize := 0;

  numwallthreads8 := I_GetNumCPUs - 1;
  if numwallthreads8 < 1 then
    numwallthreads8 := 1
  else if numwallthreads8 > MAXWALLTHREADS8 then
    numwallthreads8 := MAXWALLTHREADS8;

  for i := 0 to numwallthreads8 - 1 do
    wallthreads8[i] := TDThread.Create(@_wall_thread_worker8);
end;

procedure R_ShutDownWallsCache8;
var
  i: integer;
begin
  for i := 0 to numwallthreads8 - 1 do
    wallthreads8[i].Free;

  memfree(Pointer(wallcache), wallcacherealsize * SizeOf(batchwallrenderinfo8_t));
end;

procedure R_ClearWallsCache8;
begin
  wallcachesize := 0;
end;

procedure R_GrowWallsCache8;
begin
  if wallcachesize >= wallcacherealsize then
  begin
    realloc(Pointer(wallcache), wallcacherealsize * SizeOf(batchwallrenderinfo8_t), (64 + wallcacherealsize) * SizeOf(batchwallrenderinfo8_t));
    wallcacherealsize := wallcacherealsize + 64;
  end;
end;

procedure R_AddWallsToCache8(const walls: Pbatchwallrenderinfo8_t);
begin
  R_GrowWallsCache8;
  wallcache[wallcachesize] := walls^;
  Inc(wallcachesize);
end;

var
  parms: array[0..MAXWALLTHREADS8 - 1] of wallthreadparms8_t;
  
procedure R_RenderMultiThreadWalls8;
var
  i: integer;
begin
  for i := 0 to numwallthreads8 - 1 do
    parms[i].start := (wallcachesize div numwallthreads8) * i;
  for i := 0 to numwallthreads8 - 2 do
    parms[i].stop := parms[i + 1].start - 1;
  parms[numwallthreads8 - 1].stop := wallcachesize - 1;

  for i := 0 to numwallthreads8 - 1 do
    wallthreads8[i].Activate(@parms[i]);
end;

procedure R_WaitWallsCache8;

  function _alldone: boolean;
  var
    i: integer;
  begin
    Result := true;
    for i := 0 to numwallthreads8 - 1 do
      Result := Result and wallthreads8[i].CheckJobDone;
  end;

begin
  while not _alldone do
    I_Sleep(0);
end;

procedure R_FlashWallColumns8(const walls: Pbatchwallrenderinfo8_t);
var
  i: integer;
  w: Pwallrenderinfo8_t;
begin
  if walls.numwalls = 0 then
    exit;

  if walls.numwalls = MAXBATCHWALLS then
  begin
    if usemultithread then
      R_AddWallsToCache8(walls)
    else
      R_DrawBatchColumn(walls);
  end
  else
  begin
    w := @walls.walls[0];
    for i := 0 to walls.numwalls - 1 do
    begin
      dc_source := w.dc_source;
      dc_yh := w.dc_yh;
      dc_yl := w.dc_yl;
      dc_x := w.dc_x;
      dc_iscale := w.dc_iscale;
      dc_texturemid := w.dc_texturemid;
      dc_colormap := w.dc_colormap;
      wallcolfunc;
      Inc(w);
    end;
  end;

  walls.numwalls := 0;
end;

end.
