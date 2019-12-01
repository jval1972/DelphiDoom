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

unit r_fake3d;

// JVAL: Fake 3D Emulation

interface

uses
  d_player;

var
  usefake3d: boolean;

procedure R_Set3DLookup(p: Pplayer_t);

procedure R_Wait3DLookup;

procedure R_Execute3DTransform;

procedure R_ShutDownFake3D;

procedure R_InitFake3D;

implementation

uses           
  d_delphi,
  {$IFDEF HEXEN}
  xn_defs,
  {$ELSE}
  doomdef,
  {$ENDIF}
  i_system,
  i_threads,
  r_draw,
  r_hires,
  v_data;

var
  fake3dlookdir: integer;
  lookup3dtable: PIntegerArray = nil;

var
  oldfake3dlookdir: integer = 0;
  oldviewwindowx: integer = -1;
  oldviewheight: integer = -1;

const
  MINLOOKDIRFACTOR = 1024;  // JVAL For MINLOOKDIR
  MAXLOOKDIRFACTOR = 384;   // JVAL For MAXLOOKDIR

//
// JVAL
// Setup lookup table
//
var
  setup3dworker: TDThread;

function do_Set3DLookup(p: Pplayer_t): Integer; stdcall;
var
  i, j: integer;
  stretch: float;
  stretchstep: float;
  stretchfactor: float;
  spot: float;
  spotfrac: float;
  spotstep: float;
  plt: PInteger;
begin
  if lookup3dtable = nil then
    lookup3dtable := malloc(SCREENWIDTH * SCREENHEIGHT * SizeOf(integer));

  fake3dlookdir := p.lookdir;

  oldfake3dlookdir := fake3dlookdir;
  oldviewwindowx := viewwindowx;
  oldviewheight := viewheight;

  stretchfactor := MINLOOKDIRFACTOR + (MINLOOKDIRFACTOR - MAXLOOKDIRFACTOR) /
                                      (MINLOOKDIR - MAXLOOKDIR) *
                                      (fake3dlookdir - MINLOOKDIR);

  if fake3dlookdir > 0 then
  begin
  // JVAL
  // How much do we stretch????
    stretch := 0;
  // Step for increasing stretch for every line
    spotfrac := fake3dlookdir / stretchfactor * viewheight;
    stretchstep :=  spotfrac / viewwidth;
    plt := @lookup3dtable[0];
    for j := 0 to viewheight - 1 do
    begin
    // JVAL
    // Initial spot
      spot := j * spotfrac / (viewwidth + stretch);
      spotstep := 1 - (spot * 2) / viewwidth;
      for i := 0 to viewwidth - 1 do
      begin
        plt^ := trunc(spot);
        inc(plt);
        spot := spot + spotstep;
      end;
      stretch := stretch + stretchstep;
    end;
  end
  else if fake3dlookdir < 0 then // The same as above, just because I don't want to mess with abs() function etc...
  begin
  // JVAL
  // Step for reducing stretch for every line,
    spotfrac := -fake3dlookdir / stretchfactor * viewheight;
    stretchstep := spotfrac / viewwidth;
  // How much do we stretch????
    stretch := spotfrac;
    plt := @lookup3dtable[0];
    for j := 0 to viewheight - 1 do
    begin
    // JVAL
    // Initial spot
      spot := j * spotfrac / (viewwidth + stretch);
      spotstep := 1 - (spot * 2) / viewwidth;
      for i := 0 to viewwidth - 1 do
      begin
        plt^ := trunc(spot);
        inc(plt);
        spot := spot + spotstep;
      end;
      stretch := stretch - stretchstep;
    end;
  end;
  result := 0;
end;

procedure R_Set3DLookup(p: Pplayer_t);
begin
  if (oldfake3dlookdir = p.lookdir) and
     (oldviewwindowx = viewwindowx) and
     (oldviewheight = viewheight) then
     exit;

  if usemultithread then
    setup3dworker.Activate(p)
  else
    do_Set3DLookup(p);
end;

procedure R_Wait3DLookup;
begin
  setup3dworker.Wait;
end;
//
// JVAL
// Execute 3D Transform in 8 bit mode
//
procedure R_Execute3DTransform8(const start, stop: integer; buffer: PByteArray);
var
  i, idx: integer;
  plt: PInteger;
  dest: PByte;
  dest2: PByte;
begin
  idx := viewwindowx + (start + viewwindowy) * SCREENWIDTH;
  plt := @lookup3dtable[start * viewwidth];
  for i := start to stop do
  begin
    dest := @screens[SCN_FG][idx];
    memcpy(buffer, dest, viewwidth);
    dest2 := PByte(integer(dest) + viewwidth - 16);
    while integer(dest) < integer(dest2) do
    begin
      dest^ := buffer[plt^];
      inc(dest);
      inc(plt);
      dest^ := buffer[plt^];
      inc(dest);
      inc(plt);
      dest^ := buffer[plt^];
      inc(dest);
      inc(plt);
      dest^ := buffer[plt^];
      inc(dest);
      inc(plt);
      dest^ := buffer[plt^];
      inc(dest);
      inc(plt);
      dest^ := buffer[plt^];
      inc(dest);
      inc(plt);
      dest^ := buffer[plt^];
      inc(dest);
      inc(plt);
      dest^ := buffer[plt^];
      inc(dest);
      inc(plt);
      dest^ := buffer[plt^];
      inc(dest);
      inc(plt);
      dest^ := buffer[plt^];
      inc(dest);
      inc(plt);
      dest^ := buffer[plt^];
      inc(dest);
      inc(plt);
      dest^ := buffer[plt^];
      inc(dest);
      inc(plt);
      dest^ := buffer[plt^];
      inc(dest);
      inc(plt);
      dest^ := buffer[plt^];
      inc(dest);
      inc(plt);
      dest^ := buffer[plt^];
      inc(dest);
      inc(plt);
      dest^ := buffer[plt^];
      inc(dest);
      inc(plt);
    end;
    dest2 := PByte(integer(dest2) + 16);
    while integer(dest) < integer(dest2) do
    begin
      dest^ := buffer[plt^];
      inc(dest);
      inc(plt);
    end;
    idx := idx + SCREENWIDTH;
  end;
end;

//
// JVAL
// Execute 3D Transform in 32 bit mode
//
procedure R_Execute3DTransform32(const start, stop: integer; buffer: PLongWordArray);
var
  i, idx: integer;
  plt: PInteger;
  dest: PLongWord;
  dest2: PLongWord;
begin
  idx := viewwindowx + (start + viewwindowy) * SCREENWIDTH;
  plt := @lookup3dtable[start * viewwidth];
  for i := start to stop do
  begin
    dest := @screen32[idx];
    memcpy(buffer, dest, viewwidth * SizeOf(LongWord));
    dest2 := PLongWord(integer(dest) + (viewwidth - 16) * SizeOf(LongWord));
    while integer(dest) < integer(dest2) do
    begin
      dest^ := buffer[plt^];
      inc(dest);
      inc(plt);
      dest^ := buffer[plt^];
      inc(dest);
      inc(plt);
      dest^ := buffer[plt^];
      inc(dest);
      inc(plt);
      dest^ := buffer[plt^];
      inc(dest);
      inc(plt);
      dest^ := buffer[plt^];
      inc(dest);
      inc(plt);
      dest^ := buffer[plt^];
      inc(dest);
      inc(plt);
      dest^ := buffer[plt^];
      inc(dest);
      inc(plt);
      dest^ := buffer[plt^];
      inc(dest);
      inc(plt);
      dest^ := buffer[plt^];
      inc(dest);
      inc(plt);
      dest^ := buffer[plt^];
      inc(dest);
      inc(plt);
      dest^ := buffer[plt^];
      inc(dest);
      inc(plt);
      dest^ := buffer[plt^];
      inc(dest);
      inc(plt);
      dest^ := buffer[plt^];
      inc(dest);
      inc(plt);
      dest^ := buffer[plt^];
      inc(dest);
      inc(plt);
      dest^ := buffer[plt^];
      inc(dest);
      inc(plt);
      dest^ := buffer[plt^];
      inc(dest);
      inc(plt);
    end;
    dest2 := PLongWord(integer(dest2) + 16 * SizeOf(LongWord));
    while integer(dest) < integer(dest2) do
    begin
      dest^ := buffer[plt^];
      inc(dest);
      inc(plt);
    end;
    idx := idx + SCREENWIDTH;
  end;
end;

type
  exec3dtransparms_t = record
    start, stop: integer;
    buffer: pointer;
  end;
  Pexec3dtransparms_t = ^exec3dtransparms_t;

//
// JVAL
// Execute 3D Transform in 8 bit mode thread function
//
function R_Thr_Execute3DTransform8(p: pointer): integer; stdcall;
begin
  R_Execute3DTransform8(Pexec3dtransparms_t(p).start, Pexec3dtransparms_t(p).stop, PByteArray(Pexec3dtransparms_t(p).buffer));
  result := 0;
end;

//
// JVAL
// Execute 3D Transform in 32 bit mode thread function
//
function R_Thr_Execute3DTransform32(p: pointer): integer; stdcall;
begin
  R_Execute3DTransform32(Pexec3dtransparms_t(p).start, Pexec3dtransparms_t(p).stop, PLongWordArray(Pexec3dtransparms_t(p).buffer));
  result := 0;
end;


var
  threadworker8, threadworker32: TDThread;

//
// JVAL
//  R_Execute3DTransform
//  Transforms current view depending on lookup3dtable
//
var
  buffer1: array[0..MAXWIDTH - 1] of LongWord;
  buffer2: array[0..MAXWIDTH - 1] of LongWord;

procedure R_Execute3DTransform;
var
//  h1: integer;
  parms1: exec3dtransparms_t;
begin
  // If we don't use fake 3d return
  if not usefake3d then
    exit;

  // If we don't use z-axis shift return again
  if not zaxisshift then
    exit;

  // If we look straigh ahead return
  if fake3dlookdir = 0 then
    exit;

  // If we use experimental multithreading
  if usemultithread then
  begin
    parms1.start := 0;
    parms1.stop := viewheight div 2;
    parms1.buffer := @buffer1;

    if videomode = vm32bit then
    begin
    // JVAL
    // Create a thread to process the half screen
    //  h1 := I_CreateProcess(@R_Thr_Execute3DTransform32, @parms1);
    // The other half is processed by application thread
      threadworker32.Activate(@parms1);
      R_Execute3DTransform32(parms1.stop + 1, viewheight - 1, @buffer2);
      threadworker32.Wait;
    end
    else
    begin
    // JVAL
    // As above
    //  h1 := I_CreateProcess(@R_Thr_Execute3DTransform8, @parms1);
      threadworker8.Activate(@parms1);
      R_Execute3DTransform8(parms1.stop + 1, viewheight - 1, PByteArray(@buffer2));
      threadworker8.Wait;
    end;

    // Wait for extra thread to terminate.
   // I_WaitForProcess(h1);

  end
  else
  begin
  // JVAL: The simple stuff
    if videomode = vm32bit then
      R_Execute3DTransform32(0, viewheight - 1, @buffer1)
    else
      R_Execute3DTransform8(0, viewheight - 1, PByteArray(@buffer1));
  end;

end;

procedure R_InitFake3D;
begin
  threadworker8 := TDThread.Create(@R_Thr_Execute3DTransform8);
  threadworker32 := TDThread.Create(@R_Thr_Execute3DTransform32);
  setup3dworker := TDThread.Create(@do_Set3DLookup);
end;

procedure R_ShutDownFake3D;
begin
  threadworker8.Free;
  threadworker32.Free;
  setup3dworker.Free;
  if lookup3dtable <> nil then
    memfree(pointer(lookup3dtable), SCREENWIDTH * SCREENHEIGHT * SizeOf(integer));
end;

end.

