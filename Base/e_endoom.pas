//------------------------------------------------------------------------------
//
//  DelphiDoom: A modified and improved DOOM engine for Windows
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 1993-1996 by id Software, Inc.
//  Copyright (C) 2004-2021 by Jim Valavanis
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
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit e_endoom;

interface

uses
  d_delphi,
  d_event;

//-----------------------------------------------------------------------------
//
// DESCRIPTION:
// Game completion, endoom screen
//
//-----------------------------------------------------------------------------
var
{$IFDEF DOOM}
  EndLumpName: string = 'ENDOOM';
{$ENDIF}
{$IFDEF HERETIC}
  EndLumpName: string = 'ENDTEXT';
{$ENDIF}
{$IFDEF STRIFE}
  EndLumpName: string = 'ENDSTRF';
{$ENDIF}
  displayendscreen: boolean;

function E_Responder(ev: Pevent_t): boolean;

procedure E_Init;

procedure E_Drawer;

procedure E_ShutDown;

procedure E_Ticker;

{$IFDEF OPENGL}
const
  ESCREENWIDTH = 1024;
  ESCREENHEIGHT = 256;

var
  e_screen32: PLongWordArray = nil;
{$ENDIF}

implementation

uses
  doomdef,
  i_system,
{$IFDEF OPENGL}
  gl_main,
{$ELSE}
  i_video,
{$ENDIF}
  v_data,
  v_video,
  w_wad,
  z_zone;

var
  e_tick: integer = 0;
  e_ticks: integer = 0;
  e_blink: boolean = true;
  e_needsupdate: boolean = true;

function E_Responder(ev: Pevent_t): boolean;
begin
  if ev._type <> ev_keyup then
  begin
    result := false;
    exit;
  end;

  if gamestate = GS_ENDOOM then
  begin
    result := e_ticks > TICRATE div 2;
    if result then
      I_Quit;
  end
  else
    result := false;
end;

var
  e_screen: PByteArray;
  dosfont: PByteArray;

const
  fontcolors: array[0..15] of LongWord = (
    0,                            // black
    170,                          // blue
    170 shl 8,                    // green
    170 shl 8 + 170,              // cyan
    170 shl 16,                   // red
    170 shl 16 + 170,             // magent
    170 shl 16 + 85 shl 8,        // brown
    170 shl 16 + 170 shl 8 + 170, // light gray
    85 shl 16 + 85 shl 8 + 85,    // dark gray
    85 shl 16 + 85 shl 8 + 255,   // light blue
    85 shl 16 + 255 shl 8 + 85,   // light green
    85 shl 16 + 255 shl 8 + 255,  // light cyan
    255 shl 16 + 85 shl 8 + 85,   // light red
    255 shl 16 + 85 shl 8 + 255,  // light magenta
    255 shl 16 + 255 shl 8 + 85,  // yellow
    255 shl 16 + 255 shl 8 + 255  // white
  );

var
  fontpalcolors: array[0..15] of byte;

procedure E_Init;
var
  dosfontlump: integer;
  i: integer;
  pal: PByteArray;
begin
  e_ticks := 1;
  e_screen := malloc(640 * 200 * SizeOf(byte));
  dosfontlump := W_CheckNumForName('DOSFONT');
  if dosfontlump >= 0 then
    dosfont := W_CacheLumpNum(dosfontlump, PU_STATIC)
  else
  begin
    I_Warning('E_Init(): Can not find DOSFONT lump'#13#10);
    dosfont := Z_Malloc(128 * 128, PU_STATIC, nil);
    ZeroMemory(dosfont, 128 * 128);
  end;

  pal := V_ReadPalette(PU_STATIC);
  V_SetPalette(pal);
  I_SetPalette(pal);
  Z_ChangeTag(pal, PU_CACHE);

  for i := 0 to 15 do
    fontpalcolors[i] := V_FindAproxColorIndex(@curpal, fontcolors[i]);

  e_needsupdate := true;
end;

type
  endoomchar_t = packed record
    code: byte;
    flags: byte;
  end;
  Pendoomchar_t = ^endoomchar_t;
  endoomchar_tArray = array[0..1999] of endoomchar_t;
  Pendoomchar_tArray = ^endoomchar_tArray;

procedure E_Drawer;
var
  endoom: Pendoomchar_tArray;
  i: integer;
  sp, fp: integer; // Screen offset, font offset
  ix, iy: integer;
  x, y: integer;
  cx, cy: integer;
  bcolor, fcolor: byte;
  pe_char: Pendoomchar_t;
  blink: boolean;
  pal: PByteArray;
begin
  pal := V_ReadPalette(PU_STATIC);
  V_SetPalette(pal);
  I_SetPalette(pal);
  Z_ChangeTag(pal, PU_CACHE);

  if e_needsupdate then
  begin
    endoom := W_CacheLumpName(EndLumpName, PU_STATIC);
    pe_char := @endoom[0];
    for i := 0 to 1999 do
    begin
      x := i mod 80;
      y := i div 80;
      fcolor := fontpalcolors[pe_char.flags and 15];
      bcolor := fontpalcolors[(pe_char.flags shr 4) and 7];
      cx := (pe_char.code - 1) mod 16;
      cy := (pe_char.code - 1) div 16;
      if e_blink then
        blink := true
      else
        blink := pe_char.flags shr 7 = 0;
      for iy := 0 to 7 do
      begin
        sp := x * 8 + (y * 8 + iy) * 640;
        fp := cx * 8 + (cy * 8 + iy) * 128;
        for ix := 0 to 7 do
        begin
          if pe_char.code = 0 then
            e_screen[sp] := bcolor
          else if dosfont[fp] = 0 then
            e_screen[sp] := bcolor
          else
          begin
            if blink then
              e_screen[sp] := fcolor
            else
              e_screen[sp] := bcolor
          end;
          inc(sp);
          inc(fp);
        end;
      end;
      inc(pe_char);
    end;
    Z_ChangeTag(endoom, PU_CACHE);
    e_needsupdate := false;
  end;

{$IFDEF OPENGL}
  if e_screen32 = nil then
    e_screen32 := mallocz(ESCREENWIDTH * ESCREENHEIGHT * SizeOf(LongWord));

  for ix := 0 to 639 do
    for iy := 0 to 199 do
      e_screen32[iy * ESCREENWIDTH + ix] := videopal[e_screen[iy * 640 + ix]] or $FF000000;
{$ELSE}
  V_CopyCustomScreen(e_screen, 640, 200, SCN_FG);
{$ENDIF}
end;

procedure E_ShutDown;
begin
  if e_ticks > 0 then
  begin
    memfree(pointer(e_screen), 640 * 200 * SizeOf(byte));
    Z_Free(dosfont);
  end;
{$IFDEF OPENGL}
  if e_screen32 <> nil then
    memfree(pointer(e_screen32), ESCREENWIDTH * ESCREENHEIGHT * SizeOf(LongWord));
{$ENDIF}
end;

procedure E_Ticker;
var
  blink: boolean;
begin
  if e_ticks > 0 then
  begin
    inc(e_ticks);
    if e_ticks > 10 * TICRATE then
      I_Quit;
    inc(e_tick);
    if e_tick >= TICRATE then
      e_tick := 0;
    blink := e_tick > TICRATE div 2;
    if blink <> e_blink then
    begin
      e_blink := not e_blink;
      e_needsupdate := true;
    end;
  end;
end;

end.
