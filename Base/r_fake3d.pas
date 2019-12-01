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
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit r_fake3d;

// JVAL: Fake 3D Emulation

interface

uses
  d_delphi,
  d_player;

var
  usefake3d: boolean;

procedure R_Set3DLookup(p: Pplayer_t);

procedure R_Wait3DLookup;

procedure R_Execute3DTransform;

procedure R_ShutDownFake3D;

procedure R_InitFake3D;

function R_Fake3DAspectCorrection(const p: Pplayer_t): Double;

procedure R_Fake3DAdjustPlanes(const p: Pplayer_t);

var
  fake3dspanpresent: PBooleanArray = nil;

implementation

uses
  doomdef,
  tables,
  m_fixed,
  i_system,
  i_threads,
  r_draw,
  r_hires,
  r_main,
  r_plane,
  v_data,
  z_zone;

type
  f3dinfo_t = record
    ylookup: PIntegerArray;  // Row translation
    yPresent: PBooleanArray; // Must draw y spans?
    left: PIntegerArray;     // Left limit
    right: PIntegerArray;    // Right limit
    fracstep: PIntegerArray; // Right limit
    xclip: PIntegerArray;    // Precomputed floorclip/ceilingclip
    aspect: double;          // Aspect correction
    lefttop, righttop: integer;
    leftbottom, rightbottom: Integer;
    computed: boolean;
  end;
  Pf3dinfo_t = ^f3dinfo_t;
  f3dinfobuffer_t = array[MINLOOKDIR..MAXLOOKDIR] of Pf3dinfo_t;

var
  f3dinfobuffer: f3dinfobuffer_t;

const
  LOOKDIR_TO_ANGLE = 0.5;

procedure R_ComputeFake3DTable(const l: integer);
var
  f3d: Pf3dinfo_t;
  ang: double;
  i: integer;
  buffer: array[0..MAXHEIGHT - 1] of double;
  buffer2: array[0..MAXHEIGHT - 1] of double;
  c, c1, c2: Double;
  r: double;
  leftstep: double;
  rightstep: double;
begin
  if l = 0 then
    exit;

  if f3dinfobuffer[l] = nil then
  begin
    f3dinfobuffer[l] := Z_Malloc(SizeOf(f3dinfo_t), PU_STATIC, nil);
    f3dinfobuffer[l].ylookup := Z_Malloc(SCREENHEIGHT * SizeOf(integer), PU_STATIC, nil);
    f3dinfobuffer[l].yPresent := Z_Malloc(SCREENHEIGHT * SizeOf(boolean), PU_STATIC, nil);
    f3dinfobuffer[l].left := Z_Malloc(SCREENHEIGHT * SizeOf(integer), PU_STATIC, nil);
    f3dinfobuffer[l].right := Z_Malloc(SCREENHEIGHT * SizeOf(integer), PU_STATIC, nil);
    f3dinfobuffer[l].fracstep := Z_Malloc(SCREENHEIGHT * SizeOf(integer), PU_STATIC, nil);
    f3dinfobuffer[l].xclip := Z_Malloc(SCREENWIDTH * SizeOf(integer), PU_STATIC, nil);
  end;

  f3d := f3dinfobuffer[l];

  if l < 0 then
    ang := l * LOOKDIR_TO_ANGLE * D_PI / 270
  else
    ang := l * LOOKDIR_TO_ANGLE * D_PI / 180;

  c := Cos(ang);
  c1 := 2 * (c - 1);
  c2 := 2 - c;
  buffer[0] := 0;
  for i := 1 to viewheight - 1 do
    buffer[i] := buffer[i - 1] + i * c1 / viewheight;

  for i := 0 to viewheight - 1 do
    buffer2[i] := buffer[i] + i * c2;

  r := Sqrt(3);
  for i := 0 to viewheight - 1 do
    f3d.ylookup[i] := Round(i + r * (buffer2[i] - i) );

  if l < 0 then
    for i := 0 to viewheight - 1 do
      f3d.ylookup[i] := 2 * i - f3d.ylookup[i];

  for i := 0 to viewheight - 1 do
    if f3d.ylookup[i] < 0 then
      f3d.ylookup[i] := 0
    else if f3d.ylookup[i] >= viewheight then
      f3d.ylookup[i] := viewheight - 1;

  // JVAL
  // Create yPresent table, determine if we must draw a span at y position
  for i := 0 to viewheight - 1 do
    f3d.yPresent[i] := false;
  // Only spans present at ylookup needed 
  for i := 0 to viewheight - 1 do
    f3d.yPresent[f3d.ylookup[i]] := true;

  f3d.aspect := c2;

  if l >= 0 then
  begin
    f3d.lefttop := 0;
    f3d.righttop := viewwidth - 1;
    r := viewwidth / f3d.aspect;
    f3d.leftbottom := Round(viewwidth - r);
    f3d.rightbottom := viewwidth - f3d.leftbottom - 1;
    leftstep := f3d.leftbottom - f3d.lefttop;
    rightstep := f3d.rightbottom - f3d.righttop;
    for i := 0 to viewheight - 1 do
    begin
      f3d.left[i] := Round(f3d.lefttop + leftstep * i / viewheight);
      f3d.right[i] := Round(f3d.righttop + rightstep * i / viewheight);
      f3d.fracstep[i] := Round((f3d.right[i] - f3d.left[i]) * FRACUNIT / viewwidth);
    end;
    // JVAL
    // Adjust floorclip
    for i := 0 to viewwidth - 1 do
      f3d.xclip[i] := viewheight;
    if f3d.leftbottom > 0 then
    begin
      r := viewheight / f3d.leftbottom;
      for i := 0 to f3d.leftbottom - 1 do
        f3d.xclip[i] := Round(r * i);
      for i := f3d.rightbottom + 1 to viewwidth - 1 do
        f3d.xclip[i] := f3d.xclip[viewwidth - i];
    end;
  end
  else
  begin
    r := viewwidth / f3d.aspect;
    f3d.lefttop := Round(viewwidth - r);
    f3d.righttop := viewwidth - f3d.lefttop - 1;
    f3d.leftbottom := 0;
    f3d.rightbottom := viewwidth - 1;
    leftstep := f3d.leftbottom - f3d.lefttop;
    rightstep := f3d.rightbottom - f3d.righttop;
    for i := 0 to viewheight - 1 do
    begin
      f3d.left[i] := Round(f3d.lefttop + leftstep * i / viewheight);
      f3d.right[i] := Round(f3d.righttop + rightstep * i / viewheight);
      f3d.fracstep[i] := Round((f3d.right[i] - f3d.left[i]) * FRACUNIT / viewwidth);
    end;
    // JVAL
    // Adjust ceilingclip
    for i := 0 to viewwidth - 1 do
      f3d.xclip[i] := -1;
    if f3d.lefttop > 0 then
    begin
      r := viewheight / f3d.lefttop;
      for i := 0 to f3d.lefttop - 1 do
        f3d.xclip[i] := Round(r * (f3d.lefttop - i - 1));
      for i := f3d.righttop + 1 to viewwidth - 1 do
        f3d.xclip[i] := f3d.xclip[viewwidth - i - 1];
    end;
  end;

  f3d.computed := true;
end;

procedure R_Fake3DAdjustPlanes(const p: Pplayer_t);
var
  i: integer;
  f3d: Pf3dinfo_t;
  l: integer;
begin
  if not usefake3d or not zaxisshift then
    Exit;

  l := p.lookdir;
  if l = 0 then
    Exit;

  if f3dinfobuffer[l] = nil then
     R_ComputeFake3DTable(l)
  else if not f3dinfobuffer[l].computed then
     R_ComputeFake3DTable(l);

  f3d := f3dinfobuffer[l];
  if l > 0 then
  begin
    for i := 0 to f3d.leftbottom - 1 do
      floorclip[i] := f3d.xclip[i];
    for i := f3d.rightbottom + 1 to viewwidth - 1 do
      floorclip[i] := f3d.xclip[i];
  end
  else
  begin
    for i := 0 to f3d.lefttop - 1 do
      ceilingclip[i] := f3d.xclip[i];
    for i := f3d.righttop + 1 to viewwidth - 1 do
      ceilingclip[i] := f3d.xclip[i];
  end;
end;

var
  fake3dlookdir: integer;

var
  oldfake3dlookdir: integer = 0;
  oldviewwindowx: integer = -1;
  oldviewheight: integer = -1;

//
// JVAL
// Setup lookup table
//
var
  setup3dworker: TDThread;

function do_Set3DLookup(p: Pplayer_t): Integer; stdcall;
begin
  oldfake3dlookdir := fake3dlookdir;
  oldviewwindowx := viewwindowx;
  oldviewheight := viewheight;

  fake3dlookdir := p.lookdir;
  R_ComputeFake3DTable(fake3dlookdir);

  if fake3dlookdir <> 0 then
    fake3dspanpresent := f3dinfobuffer[fake3dlookdir].yPresent
  else
    fake3dspanpresent := nil;

  result := 0;
end;

procedure R_Set3DLookup(p: Pplayer_t);
begin
  if (oldfake3dlookdir = p.lookdir) and
     (oldviewwindowx = viewwindowx) and
     (oldviewheight = viewheight) then
     exit;

  if usemultithread or false then
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
  f3d: Pf3dinfo_t;
  i: integer;
  idx: Integer;
  dest: PByte;
  limit: PByte;
  limit2: PByte;
  src: PByte;
  frac, fracstep: fixed_t;
  range: integer;
  fb: fourbytes;
begin
  f3d := f3dinfobuffer[fake3dlookdir];

  range := stop - start + 1;
  if fake3dlookdir >= 0 then
  begin
    for i := 0 to viewheight - 1 do
    begin
      idx := viewwindowx + (f3d.ylookup[i] + viewwindowy) * SCREENWIDTH + start;
      src := @screens[SCN_FG][idx];
      memcpy(buffer, src, range);
      idx := viewwindowx + (i + viewwindowy) * SCREENWIDTH + start;
      dest := @screens[SCN_FG][idx];
      fracstep := f3d.fracstep[f3d.ylookup[i]];
      frac := f3d.left[f3d.ylookup[i]] * FRACUNIT + start * (fracstep - FRACUNIT);
      limit := @PByteArray(dest)[range];
      limit2 := PByte(Integer(limit) - 16);

      while integer(dest) < integer(limit2) do
      begin
        fb.byte1 := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        fb.byte2 := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        fb.byte3 := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        fb.byte4 := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        PLongWord(dest)^ := PLongWord(@fb)^;
        Inc(dest, 4);

        fb.byte1 := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        fb.byte2 := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        fb.byte3 := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        fb.byte4 := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        PLongWord(dest)^ := PLongWord(@fb)^;
        Inc(dest, 4);

        fb.byte1 := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        fb.byte2 := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        fb.byte3 := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        fb.byte4 := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        PLongWord(dest)^ := PLongWord(@fb)^;
        Inc(dest, 4);

        fb.byte1 := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        fb.byte2 := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        fb.byte3 := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        fb.byte4 := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        PLongWord(dest)^ := PLongWord(@fb)^;
        Inc(dest, 4);
      end;

      while integer(dest) < integer(limit) do
      begin
        dest^ := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        Inc(dest);
      end;
    end;
  end
  else
  begin
    for i := viewheight - 1 downto 0 do
    begin
      idx := viewwindowx + (f3d.ylookup[i] + viewwindowy) * SCREENWIDTH + start;
      src := @screens[SCN_FG][idx];
      memcpy(buffer, src, range);
      idx := viewwindowx + (i + viewwindowy) * SCREENWIDTH + start;
      dest := @screens[SCN_FG][idx];
      fracstep := f3d.fracstep[f3d.ylookup[i]];
      frac := f3d.left[f3d.ylookup[i]] * FRACUNIT + start * (fracstep - FRACUNIT);
      limit := @PByteArray(dest)[range];
      limit2 := PByte(Integer(limit) - 16);

      while integer(dest) < integer(limit2) do
      begin
        fb.byte1 := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        fb.byte2 := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        fb.byte3 := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        fb.byte4 := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        PLongWord(dest)^ := PLongWord(@fb)^;
        Inc(dest, 4);

        fb.byte1 := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        fb.byte2 := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        fb.byte3 := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        fb.byte4 := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        PLongWord(dest)^ := PLongWord(@fb)^;
        Inc(dest, 4);

        fb.byte1 := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        fb.byte2 := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        fb.byte3 := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        fb.byte4 := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        PLongWord(dest)^ := PLongWord(@fb)^;
        Inc(dest, 4);

        fb.byte1 := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        fb.byte2 := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        fb.byte3 := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        fb.byte4 := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        PLongWord(dest)^ := PLongWord(@fb)^;
        Inc(dest, 4);
      end;

      while integer(dest) < integer(limit) do
      begin
        dest^ := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        Inc(dest);
      end;
    end;
  end
end;

//
// JVAL
// Execute 3D Transform in 32 bit mode
//
procedure R_Execute3DTransform32(const start, stop: integer; buffer: PLongWordArray);
var
  f3d: Pf3dinfo_t;
  i: integer;
  idx: Integer;
  dest: PLongWord;
  limit: PLongWord;
  limit2: PLongWord;
  src: PLongWord;
  frac, fracstep: fixed_t;
  range: integer;
  bsize: Integer;
begin
  f3d := f3dinfobuffer[fake3dlookdir];

  range := stop - start + 1;
  bsize := range * SizeOf(LongWord);
  if fake3dlookdir >= 0 then
  begin
    for i := 0 to viewheight - 1 do
    begin
      idx := viewwindowx + (f3d.ylookup[i] + viewwindowy) * SCREENWIDTH + start;
      src := @screen32[idx];
      memcpy(buffer, src, bsize);
      idx := viewwindowx + (i + viewwindowy) * SCREENWIDTH + start;
      dest := @screen32[idx];
      fracstep := f3d.fracstep[f3d.ylookup[i]];
      frac := f3d.left[f3d.ylookup[i]] * FRACUNIT + start * (fracstep - FRACUNIT);
      limit := @PLongWordArray(dest)[range];
      limit2 := @PLongWordArray(dest)[range - 16];

      while integer(dest) < integer(limit2) do
      begin
        dest^ := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        Inc(dest);
        dest^ := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        Inc(dest);
        dest^ := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        Inc(dest);
        dest^ := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        Inc(dest);
        dest^ := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        Inc(dest);
        dest^ := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        Inc(dest);
        dest^ := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        Inc(dest);
        dest^ := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        Inc(dest);
        dest^ := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        Inc(dest);
        dest^ := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        Inc(dest);
        dest^ := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        Inc(dest);
        dest^ := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        Inc(dest);
        dest^ := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        Inc(dest);
        dest^ := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        Inc(dest);
        dest^ := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        Inc(dest);
        dest^ := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        Inc(dest);
      end;

      while integer(dest) < integer(limit) do
      begin
        dest^ := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        Inc(dest);
      end;
    end;
  end
  else
  begin
    for i := viewheight - 1 downto 0 do
    begin
      idx := viewwindowx + (f3d.ylookup[i] + viewwindowy) * SCREENWIDTH + start;
      src := @screen32[idx];
      memcpy(buffer, src, bsize);
      idx := viewwindowx + (i + viewwindowy) * SCREENWIDTH + start;
      dest := @screen32[idx];
      fracstep := f3d.fracstep[f3d.ylookup[i]];
      frac := f3d.left[f3d.ylookup[i]] * FRACUNIT + start * (fracstep - FRACUNIT);
      limit := @PLongWordArray(dest)[range];
      limit2 := @PLongWordArray(dest)[range - 16];

      while integer(dest) < integer(limit2) do
      begin
        dest^ := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        Inc(dest);
        dest^ := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        Inc(dest);
        dest^ := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        Inc(dest);
        dest^ := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        Inc(dest);
        dest^ := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        Inc(dest);
        dest^ := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        Inc(dest);
        dest^ := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        Inc(dest);
        dest^ := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        Inc(dest);
        dest^ := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        Inc(dest);
        dest^ := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        Inc(dest);
        dest^ := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        Inc(dest);
        dest^ := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        Inc(dest);
        dest^ := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        Inc(dest);
        dest^ := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        Inc(dest);
        dest^ := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        Inc(dest);
        dest^ := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        Inc(dest);
      end;

      while integer(dest) < integer(limit) do
      begin
        dest^ := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        Inc(dest);
      end;
    end;
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
//  Transforms current view 
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
    parms1.stop := viewwidth div 2;
    parms1.buffer := @buffer1;

    if videomode = vm32bit then
    begin
    // JVAL
    // Activate the thread to process the half screen
    // The other half is processed by application thread
      threadworker32.Activate(@parms1);
      R_Execute3DTransform32(parms1.stop + 1, viewwidth - 1, @buffer2);
      threadworker32.Wait;
    end
    else
    begin
    // JVAL
    // As above
      threadworker8.Activate(@parms1);
      R_Execute3DTransform8(parms1.stop + 1, viewwidth - 1, PByteArray(@buffer2));
      threadworker8.Wait;
    end;

  end
  else
  begin
  // JVAL: The simple stuff
    if videomode = vm32bit then
      R_Execute3DTransform32(0, viewwidth - 1, @buffer1)
    else
      R_Execute3DTransform8(0, viewwidth - 1, PByteArray(@buffer1));
  end;

end;

procedure R_InitFake3D;
var
  i: integer;
begin
  for i := MINLOOKDIR to MAXLOOKDIR do
    f3dinfobuffer[i] := nil;
  threadworker8 := TDThread.Create(@R_Thr_Execute3DTransform8);
  threadworker32 := TDThread.Create(@R_Thr_Execute3DTransform32);
  setup3dworker := TDThread.Create(@do_Set3DLookup);
end;

procedure R_ShutDownFake3D;
begin
  threadworker8.Free;
  threadworker32.Free;
  setup3dworker.Free;
end;

function R_Fake3DAspectCorrection(const p: Pplayer_t): Double;
begin
  if p = nil then
  begin
    result := 1.0;
    exit;
  end;

  if zaxisshift and usefake3d and (p.lookdir <> 0) then
  begin
    if f3dinfobuffer[p.lookdir] = nil then
       R_ComputeFake3DTable(p.lookdir)
    else if not f3dinfobuffer[p.lookdir].computed then
       R_ComputeFake3DTable(p.lookdir);
    result := f3dinfobuffer[p.lookdir].aspect;
  end
  else
    result := 1.0;
end;

end.

