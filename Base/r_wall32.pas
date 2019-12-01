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

unit r_wall32;

interface

uses
  d_delphi,
  m_fixed,
  r_ccache;

type
  wallrenderinfo32_t = record
    dc_source32: PLongWordArray;
    dc_texturefactorbits: integer;
    dc_yh, dc_yl, dc_x: Integer;
    dc_iscale: fixed_t;
    dc_texturemid: fixed_t;
    dc_lightlevel: fixed_t;
  end;
  Pwallrenderinfo32_t = ^wallrenderinfo32_t;

  batchwallrenderinfo32_t = record
    numwalls: integer;
    walls: array[0..MAXBATCHWALLS - 1] of wallrenderinfo32_t;
  end;
  Pbatchwallrenderinfo32_t = ^batchwallrenderinfo32_t;

  batchwallrenderinfo32_tArray = array[0..$FFFF] of batchwallrenderinfo32_t;
  Pbatchwallrenderinfo32_tArray = ^batchwallrenderinfo32_tArray;

procedure R_StoreWallColumn32(const idx: PInteger);

procedure R_FlashWallColumns32(const idx: PInteger);

procedure R_InitWallsCache32;

procedure R_ShutDownWallsCache32;

procedure R_ClearWallsCache32;

procedure R_RenderMultiThreadWalls32;

procedure R_WaitWallsCache32;


var
  midwalls32: integer;
  lowerwalls32: integer;
  upperwalls32: integer;

implementation

uses
  {$IFDEF HEXEN}
  xn_defs,
  {$ELSE}
  doomdef,
  {$ENDIF}
  i_threads,
  i_system,
  r_column,
  r_draw,
  r_precalc,
  r_main;

procedure R_DrawBatchColumnHi(const walls: Pbatchwallrenderinfo32_t);
var
  w: Pwallrenderinfo32_t;
  ypos: integer;
  destl: PLongWord;
  frac1, frac2, frac3, frac4, frac5, frac6, frac7, frac8: fixed_t;
  fracstep1, fracstep2, fracstep3, fracstep4, fracstep5, fracstep6, fracstep7, fracstep8: fixed_t;
  spot: integer;
  swidth: integer;

  r1, g1, b1: byte;
  c : LongWord;
  lfactor1, lfactor2, lfactor3, lfactor4, lfactor5, lfactor6, lfactor7, lfactor8: integer;
  and_mask: integer;
  bf_r1, bf_g1, bf_b1: PIntegerArray;
  bf_r2, bf_g2, bf_b2: PIntegerArray;
  bf_r3, bf_g3, bf_b3: PIntegerArray;
  bf_r4, bf_g4, bf_b4: PIntegerArray;
  bf_r5, bf_g5, bf_b5: PIntegerArray;
  bf_r6, bf_g6, bf_b6: PIntegerArray;
  bf_r7, bf_g7, bf_b7: PIntegerArray;
  bf_r8, bf_g8, bf_b8: PIntegerArray;
  dc_source32_1: PLongWordArray;
  dc_source32_2: PLongWordArray;
  dc_source32_3: PLongWordArray;
  dc_source32_4: PLongWordArray;
  dc_source32_5: PLongWordArray;
  dc_source32_6: PLongWordArray;
  dc_source32_7: PLongWordArray;
  dc_source32_8: PLongWordArray;

  xx: integer;
  i: integer;

  min_yh, max_yl: integer;
  max_yh, min_yl: integer;
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
  destl := @((ylookupl[min_yl]^)[columnofs[w.dc_x]]);

  and_mask := 127;

  xx := min_yl - centery;

  fracstep1 := w.dc_iscale;
  frac1 := w.dc_texturemid + xx * fracstep1;
  dc_source32_1 := w.dc_source32;
  if w.dc_texturefactorbits > 0 then
  begin
    fracstep1 := fracstep1 * (1 shl w.dc_texturefactorbits);
    frac1 := frac1 * (1 shl w.dc_texturefactorbits);
    and_mask := 128 * (1 shl w.dc_texturefactorbits) - 1;
  end;
  lfactor1 := w.dc_lightlevel;
  if lfactor1 >= 0 then
    R_GetPrecalc32Tables(lfactor1, bf_r1, bf_g1, bf_b1);
  Inc(w);

  fracstep2 := w.dc_iscale;
  frac2 := w.dc_texturemid + xx * fracstep2;
  dc_source32_2 := w.dc_source32;
  if w.dc_texturefactorbits > 0 then
  begin
    fracstep2 := fracstep2 * (1 shl w.dc_texturefactorbits);
    frac2 := frac2 * (1 shl w.dc_texturefactorbits);
  end;
  lfactor2 := w.dc_lightlevel;
  if lfactor2 >= 0 then
    R_GetPrecalc32Tables(lfactor2, bf_r2, bf_g2, bf_b2);
  Inc(w);

  fracstep3 := w.dc_iscale;
  frac3 := w.dc_texturemid + xx * fracstep3;
  dc_source32_3 := w.dc_source32;
  if w.dc_texturefactorbits > 0 then
  begin
    fracstep3 := fracstep3 * (1 shl w.dc_texturefactorbits);
    frac3 := frac3 * (1 shl w.dc_texturefactorbits);
  end;
  lfactor3 := w.dc_lightlevel;
  if lfactor3 >= 0 then
    R_GetPrecalc32Tables(lfactor3, bf_r3, bf_g3, bf_b3);
  Inc(w);

  fracstep4 := w.dc_iscale;
  frac4 := w.dc_texturemid + xx * fracstep4;
  dc_source32_4 := w.dc_source32;
  if w.dc_texturefactorbits > 0 then
  begin
    fracstep4 := fracstep4 * (1 shl w.dc_texturefactorbits);
    frac4 := frac4 * (1 shl w.dc_texturefactorbits);
  end;
  lfactor4 := w.dc_lightlevel;
  if lfactor4 >= 0 then
    R_GetPrecalc32Tables(lfactor4, bf_r4, bf_g4, bf_b4);
  Inc(w);

  fracstep5 := w.dc_iscale;
  frac5 := w.dc_texturemid + xx * fracstep5;
  dc_source32_5 := w.dc_source32;
  if w.dc_texturefactorbits > 0 then
  begin
    fracstep5 := fracstep5 * (1 shl w.dc_texturefactorbits);
    frac5 := frac5 * (1 shl w.dc_texturefactorbits);
  end;
  lfactor5 := w.dc_lightlevel;
  if lfactor5 >= 0 then
    R_GetPrecalc32Tables(lfactor5, bf_r5, bf_g5, bf_b5);
  Inc(w);

  fracstep6 := w.dc_iscale;
  frac6 := w.dc_texturemid + xx * fracstep6;
  dc_source32_6 := w.dc_source32;
  if w.dc_texturefactorbits > 0 then
  begin
    fracstep6 := fracstep6 * (1 shl w.dc_texturefactorbits);
    frac6 := frac6 * (1 shl w.dc_texturefactorbits);
  end;
  lfactor6 := w.dc_lightlevel;
  if lfactor6 >= 0 then
    R_GetPrecalc32Tables(lfactor6, bf_r6, bf_g6, bf_b6);
  Inc(w);

  fracstep7 := w.dc_iscale;
  frac7 := w.dc_texturemid + xx * fracstep7;
  dc_source32_7 := w.dc_source32;
  if w.dc_texturefactorbits > 0 then
  begin
    fracstep7 := fracstep7 * (1 shl w.dc_texturefactorbits);
    frac7 := frac7 * (1 shl w.dc_texturefactorbits);
  end;
  lfactor7 := w.dc_lightlevel;
  if lfactor7 >= 0 then
    R_GetPrecalc32Tables(lfactor7, bf_r7, bf_g7, bf_b7);
  Inc(w);

  fracstep8 := w.dc_iscale;
  frac8 := w.dc_texturemid + xx * fracstep8;
  dc_source32_8 := w.dc_source32;
  if w.dc_texturefactorbits > 0 then
  begin
    fracstep8 := fracstep8 * (1 shl w.dc_texturefactorbits);
    frac8 := frac8 * (1 shl w.dc_texturefactorbits);
  end;
  lfactor8 := w.dc_lightlevel;
  if lfactor8 >= 0 then
    R_GetPrecalc32Tables(lfactor8, bf_r8, bf_g8, bf_b8);

  swidth := SCREENWIDTH32PITCH - (MAXBATCHWALLS - 1) * SizeOf(LongWord);

  ypos := min_yl;
  if lfactor1 >= 0 then
  begin
    if max_yl < min_yh then
    begin
      while ypos < max_yl do
      begin
        w := @walls.walls[0];
        if (ypos >= w.dc_yl) then
        begin
          spot := (LongWord(frac1) shr FRACBITS) and and_mask;
          c := dc_source32_1[spot];
          destl^ := bf_r1[c and $FF] + bf_g1[(c shr 8) and $FF] + bf_b1[(c shr 16) and $FF];
        end;
        inc(frac1, fracstep1);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) then
        begin
          spot := (LongWord(frac2) shr FRACBITS) and and_mask;
          c := dc_source32_2[spot];
          destl^ := bf_r2[c and $FF] + bf_g2[(c shr 8) and $FF] + bf_b2[(c shr 16) and $FF];
        end;
        inc(frac2, fracstep2);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) then
        begin
          spot := (LongWord(frac3) shr FRACBITS) and and_mask;
          c := dc_source32_3[spot];
          destl^ := bf_r3[c and $FF] + bf_g3[(c shr 8) and $FF] + bf_b3[(c shr 16) and $FF];
        end;
        inc(frac3, fracstep3);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) then
        begin
          spot := (LongWord(frac4) shr FRACBITS) and and_mask;
          c := dc_source32_4[spot];
          destl^ := bf_r4[c and $FF] + bf_g4[(c shr 8) and $FF] + bf_b4[(c shr 16) and $FF];
        end;
        inc(frac4, fracstep4);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) then
        begin
          spot := (LongWord(frac5) shr FRACBITS) and and_mask;
          c := dc_source32_5[spot];
          destl^ := bf_r5[c and $FF] + bf_g5[(c shr 8) and $FF] + bf_b5[(c shr 16) and $FF];
        end;
        inc(frac5, fracstep5);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) then
        begin
          spot := (LongWord(frac6) shr FRACBITS) and and_mask;
          c := dc_source32_6[spot];
          destl^ := bf_r6[c and $FF] + bf_g6[(c shr 8) and $FF] + bf_b6[(c shr 16) and $FF];
        end;
        inc(frac6, fracstep6);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) then
        begin
          spot := (LongWord(frac7) shr FRACBITS) and and_mask;
          c := dc_source32_7[spot];
          destl^ := bf_r7[c and $FF] + bf_g7[(c shr 8) and $FF] + bf_b7[(c shr 16) and $FF];
        end;
        inc(frac7, fracstep7);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) then
        begin
          spot := (LongWord(frac8) shr FRACBITS) and and_mask;
          c := dc_source32_8[spot];
          destl^ := bf_r8[c and $FF] + bf_g8[(c shr 8) and $FF] + bf_b8[(c shr 16) and $FF];
        end;
        inc(frac8, fracstep8);

        destl := PLongWord(integer(destl) + swidth);
        inc(ypos);
      end;

      while ypos <= min_yh do
      begin
        spot := (LongWord(frac1) shr FRACBITS) and and_mask;
        c := dc_source32_1[spot];
        destl^ := bf_r1[c and $FF] + bf_g1[(c shr 8) and $FF] + bf_b1[(c shr 16) and $FF];
        inc(frac1, fracstep1);
        inc(destl);

        spot := (LongWord(frac2) shr FRACBITS) and and_mask;
        c := dc_source32_2[spot];
        destl^ := bf_r2[c and $FF] + bf_g2[(c shr 8) and $FF] + bf_b2[(c shr 16) and $FF];
        inc(frac2, fracstep2);
        inc(destl);

        spot := (LongWord(frac3) shr FRACBITS) and and_mask;
        c := dc_source32_3[spot];
        destl^ := bf_r3[c and $FF] + bf_g3[(c shr 8) and $FF] + bf_b3[(c shr 16) and $FF];
        inc(frac3, fracstep3);
        inc(destl);

        spot := (LongWord(frac4) shr FRACBITS) and and_mask;
        c := dc_source32_4[spot];
        destl^ := bf_r4[c and $FF] + bf_g4[(c shr 8) and $FF] + bf_b4[(c shr 16) and $FF];
        inc(frac4, fracstep4);
        inc(destl);

        spot := (LongWord(frac5) shr FRACBITS) and and_mask;
        c := dc_source32_5[spot];
        destl^ := bf_r5[c and $FF] + bf_g5[(c shr 8) and $FF] + bf_b5[(c shr 16) and $FF];
        inc(frac5, fracstep5);
        inc(destl);

        spot := (LongWord(frac6) shr FRACBITS) and and_mask;
        c := dc_source32_6[spot];
        destl^ := bf_r6[c and $FF] + bf_g6[(c shr 8) and $FF] + bf_b6[(c shr 16) and $FF];
        inc(frac6, fracstep6);
        inc(destl);

        spot := (LongWord(frac7) shr FRACBITS) and and_mask;
        c := dc_source32_7[spot];
        destl^ := bf_r7[c and $FF] + bf_g7[(c shr 8) and $FF] + bf_b7[(c shr 16) and $FF];
        inc(frac7, fracstep7);
        inc(destl);

        spot := (LongWord(frac8) shr FRACBITS) and and_mask;
        c := dc_source32_8[spot];
        destl^ := bf_r8[c and $FF] + bf_g8[(c shr 8) and $FF] + bf_b8[(c shr 16) and $FF];
        inc(frac8, fracstep8);

        destl := PLongWord(integer(destl) + swidth);
        inc(ypos);
      end;

      while ypos <= max_yh do
      begin
        w := @walls.walls[0];
        if (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac1) shr FRACBITS) and and_mask;
          c := dc_source32_1[spot];
          destl^ := bf_r1[c and $FF] + bf_g1[(c shr 8) and $FF] + bf_b1[(c shr 16) and $FF];
        end;
        inc(frac1, fracstep1);
        inc(destl);
        inc(w);

        if (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac2) shr FRACBITS) and and_mask;
          c := dc_source32_2[spot];
          destl^ := bf_r2[c and $FF] + bf_g2[(c shr 8) and $FF] + bf_b2[(c shr 16) and $FF];
        end;
        inc(frac2, fracstep2);
        inc(destl);
        inc(w);

        if (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac3) shr FRACBITS) and and_mask;
          c := dc_source32_3[spot];
          destl^ := bf_r3[c and $FF] + bf_g3[(c shr 8) and $FF] + bf_b3[(c shr 16) and $FF];
        end;
        inc(frac3, fracstep3);
        inc(destl);
        inc(w);

        if (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac4) shr FRACBITS) and and_mask;
          c := dc_source32_4[spot];
          destl^ := bf_r4[c and $FF] + bf_g4[(c shr 8) and $FF] + bf_b4[(c shr 16) and $FF];
        end;
        inc(frac4, fracstep4);
        inc(destl);
        inc(w);

        if (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac5) shr FRACBITS) and and_mask;
          c := dc_source32_5[spot];
          destl^ := bf_r5[c and $FF] + bf_g5[(c shr 8) and $FF] + bf_b5[(c shr 16) and $FF];
        end;
        inc(frac5, fracstep5);
        inc(destl);
        inc(w);

        if (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac6) shr FRACBITS) and and_mask;
          c := dc_source32_6[spot];
          destl^ := bf_r6[c and $FF] + bf_g6[(c shr 8) and $FF] + bf_b6[(c shr 16) and $FF];
        end;
        inc(frac6, fracstep6);
        inc(destl);
        inc(w);

        if (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac7) shr FRACBITS) and and_mask;
          c := dc_source32_7[spot];
          destl^ := bf_r7[c and $FF] + bf_g7[(c shr 8) and $FF] + bf_b7[(c shr 16) and $FF];
        end;
        inc(frac7, fracstep7);
        inc(destl);
        inc(w);

        if (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac8) shr FRACBITS) and and_mask;
          c := dc_source32_8[spot];
          destl^ := bf_r8[c and $FF] + bf_g8[(c shr 8) and $FF] + bf_b8[(c shr 16) and $FF];
        end;
        inc(frac8, fracstep8);

        destl := PLongWord(integer(destl) + swidth);
        inc(ypos);
      end;
    end
    else
    begin
      while ypos < max_yl do
      begin
        w := @walls.walls[0];
        if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac1) shr FRACBITS) and and_mask;
          c := dc_source32_1[spot];
          destl^ := bf_r1[c and $FF] + bf_g1[(c shr 8) and $FF] + bf_b1[(c shr 16) and $FF];
        end;
        inc(frac1, fracstep1);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac2) shr FRACBITS) and and_mask;
          c := dc_source32_2[spot];
          destl^ := bf_r2[c and $FF] + bf_g2[(c shr 8) and $FF] + bf_b2[(c shr 16) and $FF];
        end;
        inc(frac2, fracstep2);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac3) shr FRACBITS) and and_mask;
          c := dc_source32_3[spot];
          destl^ := bf_r3[c and $FF] + bf_g3[(c shr 8) and $FF] + bf_b3[(c shr 16) and $FF];
        end;
        inc(frac3, fracstep3);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac4) shr FRACBITS) and and_mask;
          c := dc_source32_4[spot];
          destl^ := bf_r4[c and $FF] + bf_g4[(c shr 8) and $FF] + bf_b4[(c shr 16) and $FF];
        end;
        inc(frac4, fracstep4);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac5) shr FRACBITS) and and_mask;
          c := dc_source32_5[spot];
          destl^ := bf_r5[c and $FF] + bf_g5[(c shr 8) and $FF] + bf_b5[(c shr 16) and $FF];
        end;
        inc(frac5, fracstep5);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac6) shr FRACBITS) and and_mask;
          c := dc_source32_6[spot];
          destl^ := bf_r6[c and $FF] + bf_g6[(c shr 8) and $FF] + bf_b6[(c shr 16) and $FF];
        end;
        inc(frac6, fracstep6);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac7) shr FRACBITS) and and_mask;
          c := dc_source32_7[spot];
          destl^ := bf_r7[c and $FF] + bf_g7[(c shr 8) and $FF] + bf_b7[(c shr 16) and $FF];
        end;
        inc(frac7, fracstep7);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac8) shr FRACBITS) and and_mask;
          c := dc_source32_8[spot];
          destl^ := bf_r8[c and $FF] + bf_g8[(c shr 8) and $FF] + bf_b8[(c shr 16) and $FF];
        end;
        inc(frac8, fracstep8);

        destl := PLongWord(integer(destl) + swidth);
        inc(ypos);
      end;

      while ypos <= max_yh do
      begin
        w := @walls.walls[0];
        if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac1) shr FRACBITS) and and_mask;
          c := dc_source32_1[spot];
          destl^ := bf_r1[c and $FF] + bf_g1[(c shr 8) and $FF] + bf_b1[(c shr 16) and $FF];
        end;
        inc(frac1, fracstep1);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac2) shr FRACBITS) and and_mask;
          c := dc_source32_2[spot];
          destl^ := bf_r2[c and $FF] + bf_g2[(c shr 8) and $FF] + bf_b2[(c shr 16) and $FF];
        end;
        inc(frac2, fracstep2);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac3) shr FRACBITS) and and_mask;
          c := dc_source32_3[spot];
          destl^ := bf_r3[c and $FF] + bf_g3[(c shr 8) and $FF] + bf_b3[(c shr 16) and $FF];
        end;
        inc(frac3, fracstep3);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac4) shr FRACBITS) and and_mask;
          c := dc_source32_4[spot];
          destl^ := bf_r4[c and $FF] + bf_g4[(c shr 8) and $FF] + bf_b4[(c shr 16) and $FF];
        end;
        inc(frac4, fracstep4);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac5) shr FRACBITS) and and_mask;
          c := dc_source32_5[spot];
          destl^ := bf_r5[c and $FF] + bf_g5[(c shr 8) and $FF] + bf_b5[(c shr 16) and $FF];
        end;
        inc(frac5, fracstep5);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac6) shr FRACBITS) and and_mask;
          c := dc_source32_6[spot];
          destl^ := bf_r6[c and $FF] + bf_g6[(c shr 8) and $FF] + bf_b6[(c shr 16) and $FF];
        end;
        inc(frac6, fracstep6);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac7) shr FRACBITS) and and_mask;
          c := dc_source32_7[spot];
          destl^ := bf_r7[c and $FF] + bf_g7[(c shr 8) and $FF] + bf_b7[(c shr 16) and $FF];
        end;
        inc(frac7, fracstep7);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac8) shr FRACBITS) and and_mask;
          c := dc_source32_8[spot];
          destl^ := bf_r8[c and $FF] + bf_g8[(c shr 8) and $FF] + bf_b8[(c shr 16) and $FF];
        end;
        inc(frac8, fracstep8);

        destl := PLongWord(integer(destl) + swidth);
        inc(ypos);
      end;
    end
  end
  else
  begin
    if max_yl < min_yh then
    begin
      while ypos < max_yl do
      begin
        w := @walls.walls[0];
        if (ypos >= w.dc_yl) then
        begin
          spot := (LongWord(frac1) shr FRACBITS) and and_mask;
          c := dc_source32_1[spot];
          r1 := c;
          g1 := c shr 8;
          b1 := c shr 16;
          destl^ := precal32_ic[r1 + g1 + b1];
        end;
        inc(frac1, fracstep1);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) then
        begin
          spot := (LongWord(frac2) shr FRACBITS) and and_mask;
          c := dc_source32_2[spot];
          r1 := c;
          g1 := c shr 8;
          b1 := c shr 16;
          destl^ := precal32_ic[r1 + g1 + b1];
        end;
        inc(frac2, fracstep2);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) then
        begin
          spot := (LongWord(frac3) shr FRACBITS) and and_mask;
          c := dc_source32_3[spot];
          r1 := c;
          g1 := c shr 8;
          b1 := c shr 16;
          destl^ := precal32_ic[r1 + g1 + b1];
        end;
        inc(frac3, fracstep3);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) then
        begin
          spot := (LongWord(frac4) shr FRACBITS) and and_mask;
          c := dc_source32_4[spot];
          r1 := c;
          g1 := c shr 8;
          b1 := c shr 16;
          destl^ := precal32_ic[r1 + g1 + b1];
        end;
        inc(frac4, fracstep4);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) then
        begin
          spot := (LongWord(frac5) shr FRACBITS) and and_mask;
          c := dc_source32_5[spot];
          r1 := c;
          g1 := c shr 8;
          b1 := c shr 16;
          destl^ := precal32_ic[r1 + g1 + b1];
        end;
        inc(frac5, fracstep5);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) then
        begin
          spot := (LongWord(frac6) shr FRACBITS) and and_mask;
          c := dc_source32_6[spot];
          r1 := c;
          g1 := c shr 8;
          b1 := c shr 16;
          destl^ := precal32_ic[r1 + g1 + b1];
        end;
        inc(frac6, fracstep6);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) then
        begin
          spot := (LongWord(frac7) shr FRACBITS) and and_mask;
          c := dc_source32_7[spot];
          r1 := c;
          g1 := c shr 8;
          b1 := c shr 16;
          destl^ := precal32_ic[r1 + g1 + b1];
        end;
        inc(frac7, fracstep7);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) then
        begin
          spot := (LongWord(frac8) shr FRACBITS) and and_mask;
          c := dc_source32_8[spot];
          r1 := c;
          g1 := c shr 8;
          b1 := c shr 16;
          destl^ := precal32_ic[r1 + g1 + b1];
        end;
        inc(frac8, fracstep8);

        destl := PLongWord(integer(destl) + swidth);
        inc(ypos);
      end;

      while ypos <= min_yh do
      begin
        spot := (LongWord(frac1) shr FRACBITS) and and_mask;
        c := dc_source32_1[spot];
        r1 := c;
        g1 := c shr 8;
        b1 := c shr 16;
        destl^ := precal32_ic[r1 + g1 + b1];
        inc(frac1, fracstep1);
        inc(destl);

        spot := (LongWord(frac2) shr FRACBITS) and and_mask;
        c := dc_source32_2[spot];
        r1 := c;
        g1 := c shr 8;
        b1 := c shr 16;
        destl^ := precal32_ic[r1 + g1 + b1];
        inc(frac2, fracstep2);
        inc(destl);

        spot := (LongWord(frac3) shr FRACBITS) and and_mask;
        c := dc_source32_3[spot];
        r1 := c;
        g1 := c shr 8;
        b1 := c shr 16;
        destl^ := precal32_ic[r1 + g1 + b1];
        inc(frac3, fracstep3);
        inc(destl);

        spot := (LongWord(frac4) shr FRACBITS) and and_mask;
        c := dc_source32_4[spot];
        r1 := c;
        g1 := c shr 8;
        b1 := c shr 16;
        destl^ := precal32_ic[r1 + g1 + b1];
        inc(frac4, fracstep4);
        inc(destl);

        spot := (LongWord(frac5) shr FRACBITS) and and_mask;
        c := dc_source32_5[spot];
        r1 := c;
        g1 := c shr 8;
        b1 := c shr 16;
        destl^ := precal32_ic[r1 + g1 + b1];
        inc(frac5, fracstep5);
        inc(destl);

        spot := (LongWord(frac6) shr FRACBITS) and and_mask;
        c := dc_source32_6[spot];
        r1 := c;
        g1 := c shr 8;
        b1 := c shr 16;
        destl^ := precal32_ic[r1 + g1 + b1];
        inc(frac6, fracstep6);
        inc(destl);

        spot := (LongWord(frac7) shr FRACBITS) and and_mask;
        c := dc_source32_7[spot];
        r1 := c;
        g1 := c shr 8;
        b1 := c shr 16;
        destl^ := precal32_ic[r1 + g1 + b1];
        inc(frac7, fracstep7);
        inc(destl);

        spot := (LongWord(frac8) shr FRACBITS) and and_mask;
        c := dc_source32_8[spot];
        r1 := c;
        g1 := c shr 8;
        b1 := c shr 16;
        destl^ := precal32_ic[r1 + g1 + b1];
        inc(frac8, fracstep8);

        destl := PLongWord(integer(destl) + swidth);
        inc(ypos);
      end;

      while ypos <= max_yh do
      begin
        w := @walls.walls[0];
        if (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac1) shr FRACBITS) and and_mask;
          c := dc_source32_1[spot];
          r1 := c;
          g1 := c shr 8;
          b1 := c shr 16;
          destl^ := precal32_ic[r1 + g1 + b1];
        end;
        inc(frac1, fracstep1);
        inc(destl);
        inc(w);

        if (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac2) shr FRACBITS) and and_mask;
          c := dc_source32_2[spot];
          r1 := c;
          g1 := c shr 8;
          b1 := c shr 16;
          destl^ := precal32_ic[r1 + g1 + b1];
        end;
        inc(frac2, fracstep2);
        inc(destl);
        inc(w);

        if (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac3) shr FRACBITS) and and_mask;
          c := dc_source32_3[spot];
          r1 := c;
          g1 := c shr 8;
          b1 := c shr 16;
          destl^ := precal32_ic[r1 + g1 + b1];
        end;
        inc(frac3, fracstep3);
        inc(destl);
        inc(w);

        if (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac4) shr FRACBITS) and and_mask;
          c := dc_source32_4[spot];
          r1 := c;
          g1 := c shr 8;
          b1 := c shr 16;
          destl^ := precal32_ic[r1 + g1 + b1];
        end;
        inc(frac4, fracstep4);
        inc(destl);
        inc(w);

        if (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac5) shr FRACBITS) and and_mask;
          c := dc_source32_5[spot];
          r1 := c;
          g1 := c shr 8;
          b1 := c shr 16;
          destl^ := precal32_ic[r1 + g1 + b1];
        end;
        inc(frac5, fracstep5);
        inc(destl);
        inc(w);

        if (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac6) shr FRACBITS) and and_mask;
          c := dc_source32_6[spot];
          r1 := c;
          g1 := c shr 8;
          b1 := c shr 16;
          destl^ := precal32_ic[r1 + g1 + b1];
        end;
        inc(frac6, fracstep6);
        inc(destl);
        inc(w);

        if (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac7) shr FRACBITS) and and_mask;
          c := dc_source32_7[spot];
          r1 := c;
          g1 := c shr 8;
          b1 := c shr 16;
          destl^ := precal32_ic[r1 + g1 + b1];
        end;
        inc(frac7, fracstep7);
        inc(destl);
        inc(w);

        if (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac8) shr FRACBITS) and and_mask;
          c := dc_source32_8[spot];
          r1 := c;
          g1 := c shr 8;
          b1 := c shr 16;
          destl^ := precal32_ic[r1 + g1 + b1];
        end;
        inc(frac8, fracstep8);

        destl := PLongWord(integer(destl) + swidth);
        inc(ypos);
      end
    end
    else
    begin
      while ypos < max_yl do
      begin
        w := @walls.walls[0];
        if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac1) shr FRACBITS) and and_mask;
          c := dc_source32_1[spot];
          r1 := c;
          g1 := c shr 8;
          b1 := c shr 16;
          destl^ := precal32_ic[r1 + g1 + b1];
        end;
        inc(frac1, fracstep1);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac2) shr FRACBITS) and and_mask;
          c := dc_source32_2[spot];
          r1 := c;
          g1 := c shr 8;
          b1 := c shr 16;
          destl^ := precal32_ic[r1 + g1 + b1];
        end;
        inc(frac2, fracstep2);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac3) shr FRACBITS) and and_mask;
          c := dc_source32_3[spot];
          r1 := c;
          g1 := c shr 8;
          b1 := c shr 16;
          destl^ := precal32_ic[r1 + g1 + b1];
        end;
        inc(frac3, fracstep3);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac4) shr FRACBITS) and and_mask;
          c := dc_source32_4[spot];
          r1 := c;
          g1 := c shr 8;
          b1 := c shr 16;
          destl^ := precal32_ic[r1 + g1 + b1];
        end;
        inc(frac4, fracstep4);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac5) shr FRACBITS) and and_mask;
          c := dc_source32_5[spot];
          r1 := c;
          g1 := c shr 8;
          b1 := c shr 16;
          destl^ := precal32_ic[r1 + g1 + b1];
        end;
        inc(frac5, fracstep5);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac6) shr FRACBITS) and and_mask;
          c := dc_source32_6[spot];
          r1 := c;
          g1 := c shr 8;
          b1 := c shr 16;
          destl^ := precal32_ic[r1 + g1 + b1];
        end;
        inc(frac6, fracstep6);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac7) shr FRACBITS) and and_mask;
          c := dc_source32_7[spot];
          r1 := c;
          g1 := c shr 8;
          b1 := c shr 16;
          destl^ := precal32_ic[r1 + g1 + b1];
        end;
        inc(frac7, fracstep7);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac8) shr FRACBITS) and and_mask;
          c := dc_source32_8[spot];
          r1 := c;
          g1 := c shr 8;
          b1 := c shr 16;
          destl^ := precal32_ic[r1 + g1 + b1];
        end;
        inc(frac8, fracstep8);

        destl := PLongWord(integer(destl) + swidth);
        inc(ypos);
      end;

      while ypos <= max_yh do
      begin
        w := @walls.walls[0];
        if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac1) shr FRACBITS) and and_mask;
          c := dc_source32_1[spot];
          r1 := c;
          g1 := c shr 8;
          b1 := c shr 16;
          destl^ := precal32_ic[r1 + g1 + b1];
        end;
        inc(frac1, fracstep1);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac2) shr FRACBITS) and and_mask;
          c := dc_source32_2[spot];
          r1 := c;
          g1 := c shr 8;
          b1 := c shr 16;
          destl^ := precal32_ic[r1 + g1 + b1];
        end;
        inc(frac2, fracstep2);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac3) shr FRACBITS) and and_mask;
          c := dc_source32_3[spot];
          r1 := c;
          g1 := c shr 8;
          b1 := c shr 16;
          destl^ := precal32_ic[r1 + g1 + b1];
        end;
        inc(frac3, fracstep3);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac4) shr FRACBITS) and and_mask;
          c := dc_source32_4[spot];
          r1 := c;
          g1 := c shr 8;
          b1 := c shr 16;
          destl^ := precal32_ic[r1 + g1 + b1];
        end;
        inc(frac4, fracstep4);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac5) shr FRACBITS) and and_mask;
          c := dc_source32_5[spot];
          r1 := c;
          g1 := c shr 8;
          b1 := c shr 16;
          destl^ := precal32_ic[r1 + g1 + b1];
        end;
        inc(frac5, fracstep5);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac6) shr FRACBITS) and and_mask;
          c := dc_source32_6[spot];
          r1 := c;
          g1 := c shr 8;
          b1 := c shr 16;
          destl^ := precal32_ic[r1 + g1 + b1];
        end;
        inc(frac6, fracstep6);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac7) shr FRACBITS) and and_mask;
          c := dc_source32_7[spot];
          r1 := c;
          g1 := c shr 8;
          b1 := c shr 16;
          destl^ := precal32_ic[r1 + g1 + b1];
        end;
        inc(frac7, fracstep7);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac8) shr FRACBITS) and and_mask;
          c := dc_source32_8[spot];
          r1 := c;
          g1 := c shr 8;
          b1 := c shr 16;
          destl^ := precal32_ic[r1 + g1 + b1];
        end;
        inc(frac8, fracstep8);

        destl := PLongWord(integer(destl) + swidth);
        inc(ypos);
      end;
    end;
  end;
end;

var
  wallcache: Pbatchwallrenderinfo32_tArray;
  wallcachesize: integer;
  wallcacherealsize: integer;

procedure R_GrowWallsCache32;
begin
  if wallcachesize >= wallcacherealsize then
  begin
    realloc(Pointer(wallcache), wallcacherealsize * SizeOf(batchwallrenderinfo32_t), (64 + wallcacherealsize) * SizeOf(batchwallrenderinfo32_t));
    wallcacherealsize := wallcacherealsize + 64;
  end;
end;

procedure R_AddWallsToCache32(const idx: PInteger);
begin
  R_GrowWallsCache32;
  idx^ := wallcachesize;
  wallcache[wallcachesize].numwalls := 0;
  Inc(wallcachesize);
end;

procedure R_DoFlashWallColumns32(const walls: Pbatchwallrenderinfo32_t; const idx: PInteger);
var
  i: integer;
  w: Pwallrenderinfo32_t;
begin
  if walls.numwalls = 0 then
    exit;

  if walls.numwalls = MAXBATCHWALLS then
  begin
    if usemultithread then
      R_AddWallsToCache32(idx)
    else
    begin
      R_DrawBatchColumnHi(walls);
      walls.numwalls := 0;
    end
  end
  else
  begin
    w := @walls.walls[0];
    for i := 0 to walls.numwalls - 1 do
    begin
      dc_source32 := w.dc_source32;
      dc_texturefactorbits := w.dc_texturefactorbits;
      dc_yh := w.dc_yh;
      dc_yl := w.dc_yl;
      dc_x := w.dc_x;
      dc_iscale := w.dc_iscale;
      dc_texturemid := w.dc_texturemid;
      dc_lightlevel := w.dc_lightlevel;
      wallcolfunc;
      Inc(w);
    end;
    walls.numwalls := 0;
  end;
end;

procedure R_FlashWallColumns32(const idx: PInteger);
begin
  R_DoFlashWallColumns32(@wallcache[idx^], idx);
end;

procedure R_StoreWallColumn32(const idx: PInteger);
var
  w: Pwallrenderinfo32_t;
  walls: Pbatchwallrenderinfo32_t;
begin
  walls := @wallcache[idx^];
  w := @walls.walls[walls.numwalls];
  w.dc_source32 := dc_source32;
  w.dc_texturefactorbits := dc_texturefactorbits;
  w.dc_yh := dc_yh;
  w.dc_yl := dc_yl;
  w.dc_x := dc_x;
  w.dc_iscale := dc_iscale;
  w.dc_texturemid := dc_texturemid;
  w.dc_lightlevel := dc_lightlevel;
  Inc(walls.numwalls);
  if walls.numwalls = MAXBATCHWALLS then
    R_DoFlashWallColumns32(walls, idx);
end;

const
  MAXWALLTHREADS32 = 16;

var
  wallthreads32: array[0..MAXWALLTHREADS32 - 1] of TDThread;
  numwallthreads32: Integer = 0;


type
  wallthreadparms32_t = record
    start, stop: integer;
  end;
  Pwallthreadparms32_t = ^wallthreadparms32_t;

function _wall_thread_worker32(parms: Pwallthreadparms32_t): integer; stdcall;
var
  i: integer;
  walls: Pbatchwallrenderinfo32_t;
begin
  for i := parms.start to parms.stop do
  begin
    walls := @wallcache[i];
    if walls.numwalls = MAXBATCHWALLS then
    begin
      R_DrawBatchColumnHi(walls);
      walls.numwalls := 0;
    end;
  end;
  result := 0;
end;


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
  wallcache[midwalls32].numwalls := 0;
  wallcache[lowerwalls32].numwalls := 0;
  wallcache[upperwalls32].numwalls := 0;
  wallcachesize := 3;

  numwallthreads32 := I_GetNumCPUs - 1;
  if numwallthreads32 < 1 then
    numwallthreads32 := 1
  else if numwallthreads32 > MAXWALLTHREADS32 then
    numwallthreads32 := MAXWALLTHREADS32;

  for i := 0 to numwallthreads32 - 1 do
    wallthreads32[i] := TDThread.Create(@_wall_thread_worker32);
end;

procedure R_ShutDownWallsCache32;
var
  i: integer;
begin
  for i := 0 to numwallthreads32 - 1 do
    wallthreads32[i].Free;

  memfree(Pointer(wallcache), wallcacherealsize * SizeOf(batchwallrenderinfo32_t));
end;

procedure R_ClearWallsCache32;
begin
  midwalls32 := 0;
  lowerwalls32 := 1;
  upperwalls32 := 2;
  wallcachesize := 3;
end;

var
  parms: array[0..MAXWALLTHREADS32 - 1] of wallthreadparms32_t;

procedure R_RenderMultiThreadWalls32;
var
  i: integer;
begin
  for i := 0 to numwallthreads32 - 1 do
    parms[i].start := (wallcachesize div numwallthreads32) * i;
  for i := 0 to numwallthreads32 - 2 do
    parms[i].stop := parms[i + 1].start - 1;
  parms[numwallthreads32 - 1].stop := wallcachesize - 1;

  for i := 0 to numwallthreads32 - 1 do
    if parms[i].start <= parms[i].stop then
      wallthreads32[i].Activate(@parms[i]);
end;

procedure R_WaitWallsCache32;

  function _alldone: boolean;
  var
    i: integer;
  begin
    Result := true;
    for i := 0 to numwallthreads32 - 1 do
      Result := Result and wallthreads32[i].CheckJobDone;
  end;

begin
  while not _alldone do
    I_Sleep(0);
end;


end.
