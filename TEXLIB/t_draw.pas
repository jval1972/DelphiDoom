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

unit t_draw;

interface

uses
  d_delphi;

function T_DrawFullScreenPatch(const texname: string; const dest: PLongWordArray): boolean; overload;

function T_DrawFullScreenPatch(const texid: integer; const dest: PLongWordArray): boolean; overload;

procedure T_InitDrawTextures;

procedure T_ShutDownDrawTextures;

implementation

uses
  doomdef,
  m_fixed,
  r_hires,
  r_mmx,
  t_main,
  v_video,
  v_data,
  w_wad;

type
  drawtexture_t = record
    texture32: PTexture;
    lump: integer;
  end;
  Pdrawtexture_t = ^drawtexture_t;
  drawtexture_tArray = array[0..$FFFF] of drawtexture_t;
  Pdrawtexture_tArray = ^drawtexture_tArray;

var
  drawtextures: Pdrawtexture_tArray;
  numdrawtextures: integer;

function T_GetDrawTextNumForName(const texname: string): integer;
var
  lump: integer;
  i: integer;
begin
  lump := W_GetNumForName(texname);
  for i := 0 to numdrawtextures - 1 do
  begin
    if drawtextures[i].lump = lump then
    begin
      result := i;
      exit;
    end;
  end;

  realloc(pointer(drawtextures), numdrawtextures * SizeOf(drawtexture_t), (numdrawtextures + 1) * SizeOf(drawtexture_t));
  result := numdrawtextures;
  drawtextures[result].texture32 := nil;
  drawtextures[result].lump := lump;
  inc(numdrawtextures);
end;

function T_DrawFullScreenPatch(const texname: string; const dest: PLongWordArray): boolean; overload;
begin
  result := T_DrawFullScreenPatch(T_GetDrawTextNumForName(texname), dest);
end;

function T_DrawFullScreenPatch(const texid: integer; const dest: PLongWordArray): boolean; overload;
var
  t: PTexture;
  src: PLongWordArray;
  dst: PLongWord;
  i, j: integer;
  fracrow, fraccol: fixed_t;
  fracrowstep, fraccolstep: fixed_t;
  r1, g1, b1: byte;
  c: LongWord;
  curgamma: PByteArray;
  twidth: integer;
begin
  if texid < 0 then // JVAL: Should never happen
  begin
    result := false;
    exit;
  end;

  if drawtextures[texid].texture32 = nil  then
  begin
    t := T_LoadHiResTexture(W_GetNameForNum(drawtextures[texid].lump));
    if t = nil then
    begin
      drawtextures[texid].texture32 := pointer($1);
      result := false;
      exit;
    end
    else
      drawtextures[texid].texture32 := t;
  end
  else
    t := drawtextures[texid].texture32;

  if LongWord(t) = 1 then
  begin
    result := false;
    exit;
  end;

  twidth := t.GetWidth;

  src := malloc(twidth * 4);

  dst := @dest[0];

  fracrowstep := t.GetHeight * FRACUNIT div {$IFDEF OPENGL}V_GetScreenHeight(SCN_FG){$ELSE}SCREENHEIGHT{$ENDIF};
  fraccolstep := twidth * FRACUNIT div {$IFDEF OPENGL}V_GetScreenWidth(SCN_FG){$ELSE}SCREENWIDTH{$ENDIF};

  if (pal_color = 0) and (usegamma = 0) then
    c := 0
  else
  begin
    curgamma := @gammatable[usegamma]; // To Adjust gamma
    r1 := pal_color;
    g1 := pal_color shr 8;
    b1 := pal_color shr 16;
    c := curgamma[r1] + curgamma[g1] shl 8 + curgamma[b1] shl 16;
  end;

  fracrow := 0;
  for i := 0 to {$IFDEF OPENGL}V_GetScreenHeight(SCN_FG){$ELSE}SCREENHEIGHT{$ENDIF} - 1 do
  begin
    fraccol := 0;

    // Adjust gamma
    if t.GetBytesPerPixel = 1 then
    begin
      t.GetPalettedRow32(fracrow shr FRACBITS, twidth, src, c);
    end
    else
    begin
      t.GetRow32(fracrow shr FRACBITS, twidth, src);
      if not R_BatchColorAdd32_MMX(@src[0], c, twidth) then
      begin
        for j := 0 to twidth - 1 do
          src[j] := R_ColorAdd(src[j], c);
      end;
    end;

    for j := 0 to {$IFDEF OPENGL}V_GetScreenWidth(SCN_FG){$ELSE}SCREENWIDTH{$ENDIF} - 1 do
    begin
      dst^ := src[fraccol shr FRACBITS]{$IFDEF OPENGL} or $FF000000{$ENDIF};
      inc(dst);
      inc(fraccol, fraccolstep);
    end;
    inc(fracrow, fracrowstep);
  end;

  memfree(pointer(src), twidth * 4);
  result := true;
end;

procedure T_InitDrawTextures;
begin
  drawtextures := nil;
  numdrawtextures := 0;
end;

procedure T_ShutDownDrawTextures;
var
  i: integer;
begin
  for i := 0 to numdrawtextures - 1 do
    if LongWord(drawtextures[i].texture32) > 1 then
      dispose(drawtextures[i].texture32, destroy);

  realloc(pointer(drawtextures), numdrawtextures * SizeOf(drawtexture_t), 0);
end;

end.
