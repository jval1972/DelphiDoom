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
//  Foundation, inc., 59 Temple Place - Suite 330, Boston, MA
//  02111-1307, USA.
//
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

unit r_colorcolumn;

interface

procedure R_DrawColorColumnMedium;
procedure R_DrawColorColumnHi;
{$IFDEF DOOM}
procedure R_DrawColorColumnAverageMedium_Batch;
{$ENDIF}
procedure R_DrawColorColumnAverageHi_Batch;
{$IFDEF DOOM}
procedure R_DrawColorColumnAlphaMedium_Batch;
{$ENDIF}
procedure R_DrawColorColumnAlphaHi_Batch;

var
  dc_color: byte;
  dc_color32: byte;

type
  putpixelfunc_t = procedure(const x, y: integer);

var
  putpixelfunc: putpixelfunc_t;

procedure R_PutPixel8(const x, y: integer);
procedure R_PutPixel32(const x, y: integer);

implementation

uses
  d_delphi,
  doomdef,
  r_precalc,
  r_column,
  r_batchcolumn,
  {$IFDEF DOOM}
  r_colormaps,
  {$ENDIF}
  v_video,
  v_data,
  r_draw;

procedure R_PutPixel8(const x, y: integer);
begin
  PByte(@((ylookup[y]^)[columnofs[x]]))^ := dc_colormap[dc_color];
end;

procedure R_PutPixel32(const x, y: integer);
var
  c: LongWord;
  r1, g1, b1: byte;
  pal: PLongWordArray;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
begin
  {$IFDEF DOOM}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;

  if dc_lightlevel >= 0 then
  begin
    R_GetPrecalc32Tables(dc_lightlevel, bf_r, bf_g, bf_b);
    c := pal[dc_color];
    PLongWord(@((ylookup[y]^)[columnofs[x]]))^ := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
  end
  else
  begin
    c := pal[dc_color];
    r1 := c;
    g1 := c shr 8;
    b1 := c shr 16;
    PLongWord(@((ylookup[y]^)[columnofs[x]]))^ := precal32_ic[r1 + g1 + b1];
  end;
end;

procedure R_DrawColorColumnMedium_cnt0;
var
  dest: PByte;
  bdest: byte;
  ldest: LongWord;
  rest_batch_columns: integer;
  num_iters: integer;
  cnt: integer;
begin
  // Framebuffer destination address.
  // Use ylookup LUT to avoid multiply with ScreenWidth.
  // Use columnofs LUT for subwindows?
  dest := @((ylookup[dc_yl]^)[columnofs[dc_x]]);

  bdest := dc_colormap[dc_color];

  if num_batch_columns = 2 then
  begin
    PWord(dest)^ := precal8_toword[bdest];
    exit;
  end;

  if num_batch_columns = 3 then
  begin
    dest^ := bdest;
    inc(dest);
    PWord(dest)^ := precal8_toword[bdest];
    exit;
  end;

  if num_batch_columns = 4 then
  begin
    PLongWord(dest)^ := precal8_tolong[bdest];
    exit;
  end;

  if num_batch_columns = 1 then
  begin
    dest^ := bdest;
    exit;
  end;

  ldest := precal8_tolong[bdest];

  if num_batch_columns > 4 then
  begin
    rest_batch_columns := num_batch_columns mod 4;
    num_iters := num_batch_columns div 4;

    if rest_batch_columns = 0 then
    begin
      // Re-map color indices from wall texture column
      //  using a lighting/special effects LUT.
      cnt := num_iters;
      while cnt > 0 do
      begin
        PLongWord(dest)^ := ldest;
        inc(dest, 4);
        dec(cnt);
      end;

      exit;
    end;

    cnt := num_iters;
    while cnt > 0 do
    begin
      PLongWord(dest)^ := ldest;
      inc(dest, 4);
      dec(cnt);
    end;
    cnt := rest_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      inc(dest);
      dec(cnt);
    end;

    exit;
  end;

  cnt := num_batch_columns;
  while cnt > 0 do
  begin
    dest^ := bdest;
    inc(dest);
    dec(cnt);
  end;
end;

procedure R_DrawColorColumnMedium_cnt1;
var
  dest: PByte;
  swidth: integer;
  bdest: byte;
  wdest: word;
  ldest: LongWord;
  rest_batch_columns: integer;
  num_iters: integer;
  cnt: integer;
begin
  // Framebuffer destination address.
  // Use ylookup LUT to avoid multiply with ScreenWidth.
  // Use columnofs LUT for subwindows?
  dest := @((ylookup[dc_yl]^)[columnofs[dc_x]]);

  bdest := dc_colormap[dc_color];

  if num_batch_columns = 2 then
  begin
    swidth := SCREENWIDTH;
    wdest := precal8_toword[bdest];
    PWord(dest)^ := wdest;
    inc(dest, swidth);
    PWord(dest)^ := wdest;
    exit;
  end;

  if num_batch_columns = 3 then
  begin
    swidth := SCREENWIDTH - 1;
    wdest := precal8_toword[bdest];
    dest^ := bdest;
    inc(dest);
    PWord(dest)^ := wdest;
    inc(dest, swidth);
    dest^ := bdest;
    inc(dest);
    PWord(dest)^ := wdest;
    exit;
  end;

  ldest := precal8_tolong[bdest];

  if num_batch_columns = 4 then
  begin
    swidth := SCREENWIDTH;
    PLongWord(dest)^ := ldest;
    inc(dest, swidth);
    PLongWord(dest)^ := ldest;
    exit;
  end;

  if num_batch_columns = 1 then
  begin
    dest^ := bdest;
    inc(dest, SCREENWIDTH);
    dest^ := bdest;
    exit;
  end;

  swidth := SCREENWIDTH - num_batch_columns;

  if num_batch_columns > 4 then
  begin
    rest_batch_columns := num_batch_columns mod 4;
    num_iters := num_batch_columns div 4;

    if rest_batch_columns = 0 then
    begin
      // Re-map color indices from wall texture column
      //  using a lighting/special effects LUT.
      cnt := num_iters;
      while cnt > 0 do
      begin
        PLongWord(dest)^ := ldest;
        inc(dest, 4);
        dec(cnt);
      end;
      inc(dest, swidth);

      cnt := num_iters;
      while cnt > 0 do
      begin
        PLongWord(dest)^ := ldest;
        inc(dest, 4);
        dec(cnt);
      end;

      exit;
    end;

    cnt := num_iters;
    while cnt > 0 do
    begin
      PLongWord(dest)^ := ldest;
      inc(dest, 4);
      dec(cnt);
    end;
    cnt := rest_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      inc(dest);
      dec(cnt);
    end;
    inc(dest, swidth);

    cnt := num_iters;
    while cnt > 0 do
    begin
      PLongWord(dest)^ := ldest;
      inc(dest, 4);
      dec(cnt);
    end;
    cnt := rest_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      inc(dest);
      dec(cnt);
    end;

    exit;
  end;

  cnt := num_batch_columns;
  while cnt > 0 do
  begin
    dest^ := bdest;
    inc(dest);
    dec(cnt);
  end;
  inc(dest, swidth);

  cnt := num_batch_columns;
  while cnt > 0 do
  begin
    dest^ := bdest;
    inc(dest);
    dec(cnt);
  end;
end;

procedure R_DrawColorColumnMedium;
var
  count: integer;
  dest: PByte;
  swidth: integer;
  bdest: byte;
  wdest: word;
  ldest: LongWord;
  rest_batch_columns: integer;
  num_iters: integer;
  cnt: integer;
begin
  count := dc_yh - dc_yl;

  if count = 1 then
  begin
    R_DrawColorColumnMedium_cnt1;
    exit;
  end;

  if count = 0 then
  begin
    R_DrawColorColumnMedium_cnt0;
    exit;
  end;

  // Zero length, column does not exceed a pixel.
  if count < 0 then
    exit;

  // Framebuffer destination address.
  // Use ylookup LUT to avoid multiply with ScreenWidth.
  // Use columnofs LUT for subwindows?
  dest := @((ylookup[dc_yl]^)[columnofs[dc_x]]);

  bdest := dc_colormap[dc_color];

  if num_batch_columns = 2 then
  begin
    swidth := SCREENWIDTH;
    wdest := precal8_toword[bdest];
    while count >= 0 do
    begin
      PWord(dest)^ := wdest;
      inc(dest, swidth);
      dec(count);
    end;
    exit;
  end;

  if num_batch_columns = 3 then
  begin
    swidth := SCREENWIDTH - 1;
    wdest := precal8_toword[bdest];
    while count >= 0 do
    begin
      dest^ := bdest;
      inc(dest);
      PWord(dest)^ := wdest;
      inc(dest, swidth);
      dec(count);
    end;
    exit;
  end;

  ldest := precal8_tolong[bdest];

  if num_batch_columns = 4 then
  begin
    swidth := SCREENWIDTH;
    while count >= 0 do
    begin
      PLongWord(dest)^ := ldest;
      inc(dest, swidth);
      dec(count);
    end;
    exit;
  end;

  swidth := SCREENWIDTH - num_batch_columns;

  if num_batch_columns > 4 then
  begin
    rest_batch_columns := num_batch_columns mod 4;
    num_iters := num_batch_columns div 4;

    if rest_batch_columns = 0 then
    begin
      while count >= 8 do
      begin
        // #1
        cnt := num_iters;
        while cnt > 0 do
        begin
          PLongWord(dest)^ := ldest;
          inc(dest, 4);
          dec(cnt);
        end;
        inc(dest, swidth);

        // #2
        cnt := num_iters;
        while cnt > 0 do
        begin
          PLongWord(dest)^ := ldest;
          inc(dest, 4);
          dec(cnt);
        end;
        inc(dest, swidth);

        // #3
        cnt := num_iters;
        while cnt > 0 do
        begin
          PLongWord(dest)^ := ldest;
          inc(dest, 4);
          dec(cnt);
        end;
        inc(dest, swidth);

        // #4
        cnt := num_iters;
        while cnt > 0 do
        begin
          PLongWord(dest)^ := ldest;
          inc(dest, 4);
          dec(cnt);
        end;
        inc(dest, swidth);

        // #5
        cnt := num_iters;
        while cnt > 0 do
        begin
          PLongWord(dest)^ := ldest;
          inc(dest, 4);
          dec(cnt);
        end;
        inc(dest, swidth);

        // #6
        cnt := num_iters;
        while cnt > 0 do
        begin
          PLongWord(dest)^ := ldest;
          inc(dest, 4);
          dec(cnt);
        end;
        inc(dest, swidth);

        // #7
        cnt := num_iters;
        while cnt > 0 do
        begin
          PLongWord(dest)^ := ldest;
          inc(dest, 4);
          dec(cnt);
        end;
        inc(dest, swidth);

        // #8
        cnt := num_iters;
        while cnt > 0 do
        begin
          PLongWord(dest)^ := ldest;
          inc(dest, 4);
          dec(cnt);
        end;
        inc(dest, swidth);

        count := count - 8;
      end;

      while count >= 0 do
      begin
      // Re-map color indices from wall texture column
      //  using a lighting/special effects LUT.
        cnt := num_iters;
        while cnt > 0 do
        begin
          PLongWord(dest)^ := ldest;
          inc(dest, 4);
          dec(cnt);
        end;
        inc(dest, swidth);

        dec(count);
      end;
      exit;
    end;

    while count >= 8 do
    begin
      // #1
      cnt := num_iters;
      while cnt > 0 do
      begin
        PLongWord(dest)^ := ldest;
        inc(dest, 4);
        dec(cnt);
      end;
      cnt := rest_batch_columns;
      while cnt > 0 do
      begin
        dest^ := bdest;
        inc(dest);
        dec(cnt);
      end;
      inc(dest, swidth);

      // #2
      cnt := num_iters;
      while cnt > 0 do
      begin
        PLongWord(dest)^ := ldest;
        inc(dest, 4);
        dec(cnt);
      end;
      cnt := rest_batch_columns;
      while cnt > 0 do
      begin
        dest^ := bdest;
        inc(dest);
        dec(cnt);
      end;
      inc(dest, swidth);

      // #3
      cnt := num_iters;
      while cnt > 0 do
      begin
        PLongWord(dest)^ := ldest;
        inc(dest, 4);
        dec(cnt);
      end;
      cnt := rest_batch_columns;
      while cnt > 0 do
      begin
        dest^ := bdest;
        inc(dest);
        dec(cnt);
      end;
      inc(dest, swidth);

      // #4
      cnt := num_iters;
      while cnt > 0 do
      begin
        PLongWord(dest)^ := ldest;
        inc(dest, 4);
        dec(cnt);
      end;
      cnt := rest_batch_columns;
      while cnt > 0 do
      begin
        dest^ := bdest;
        inc(dest);
        dec(cnt);
      end;
      inc(dest, swidth);

      // #5
      cnt := num_iters;
      while cnt > 0 do
      begin
        PLongWord(dest)^ := ldest;
        inc(dest, 4);
        dec(cnt);
      end;
      cnt := rest_batch_columns;
      while cnt > 0 do
      begin
        dest^ := bdest;
        inc(dest);
        dec(cnt);
      end;
      inc(dest, swidth);

      // #6
      cnt := num_iters;
      while cnt > 0 do
      begin
        PLongWord(dest)^ := ldest;
        inc(dest, 4);
        dec(cnt);
      end;
      cnt := rest_batch_columns;
      while cnt > 0 do
      begin
        dest^ := bdest;
        inc(dest);
        dec(cnt);
      end;
      inc(dest, swidth);

      // #7
      cnt := num_iters;
      while cnt > 0 do
      begin
        PLongWord(dest)^ := ldest;
        inc(dest, 4);
        dec(cnt);
      end;
      cnt := rest_batch_columns;
      while cnt > 0 do
      begin
        dest^ := bdest;
        inc(dest);
        dec(cnt);
      end;
      inc(dest, swidth);

      // #8
      cnt := num_iters;
      while cnt > 0 do
      begin
        PLongWord(dest)^ := ldest;
        inc(dest, 4);
        dec(cnt);
      end;
      cnt := rest_batch_columns;
      while cnt > 0 do
      begin
        dest^ := bdest;
        inc(dest);
        dec(cnt);
      end;
      inc(dest, swidth);

      count := count - 8;
    end;

    while count >= 0 do
    begin
      cnt := num_iters;
      while cnt > 0 do
      begin
        PLongWord(dest)^ := ldest;
        inc(dest, 4);
        dec(cnt);
      end;
      cnt := rest_batch_columns;
      while cnt > 0 do
      begin
        dest^ := bdest;
        inc(dest);
        dec(cnt);
      end;
      inc(dest, swidth);

      dec(count);
    end;
    exit;
  end;

  if num_batch_columns = 1 then
  begin
    while count >= 8 do
    begin
      dest^ := bdest;
      inc(dest, SCREENWIDTH);
      dest^ := bdest;
      inc(dest, SCREENWIDTH);
      dest^ := bdest;
      inc(dest, SCREENWIDTH);
      dest^ := bdest;
      inc(dest, SCREENWIDTH);
      dest^ := bdest;
      inc(dest, SCREENWIDTH);
      dest^ := bdest;
      inc(dest, SCREENWIDTH);
      dest^ := bdest;
      inc(dest, SCREENWIDTH);
      dest^ := bdest;
      inc(dest, SCREENWIDTH);
      count := count - 8;
    end;
    if count >= 4 then
    begin
      dest^ := bdest;
      inc(dest, SCREENWIDTH);
      dest^ := bdest;
      inc(dest, SCREENWIDTH);
      dest^ := bdest;
      inc(dest, SCREENWIDTH);
      dest^ := bdest;
      inc(dest, SCREENWIDTH);
      count := count - 4;
    end;
    while count >= 0 do
    begin
      dest^ := bdest;
      inc(dest, SCREENWIDTH);
      dec(count);
    end;

    exit;
  end;

  while count >= 8 do
  begin
    cnt := num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      inc(dest);
      dec(cnt);
    end;
    inc(dest, swidth);

    cnt := num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      inc(dest);
      dec(cnt);
    end;
    inc(dest, swidth);

    cnt := num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      inc(dest);
      dec(cnt);
    end;
    inc(dest, swidth);

    cnt := num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      inc(dest);
      dec(cnt);
    end;
    inc(dest, swidth);

    cnt := num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      inc(dest);
      dec(cnt);
    end;
    inc(dest, swidth);

    cnt := num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      inc(dest);
      dec(cnt);
    end;
    inc(dest, swidth);

    cnt := num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      inc(dest);
      dec(cnt);
    end;
    inc(dest, swidth);

    cnt := num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      inc(dest);
      dec(cnt);
    end;
    inc(dest, swidth);

    count := count - 8;
  end;

  while count >= 0 do
  begin
    cnt := num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      inc(dest);
      dec(cnt);
    end;
    inc(dest, swidth);
    dec(count);
  end;
end;


procedure R_DrawColorColumnHi_cnt0;
var
  destl: PLongWord;
  ldest: LongWord;
  c: LongWord;
  cnt: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
begin
  {$IFDEF DOOM}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;

  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);

  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b);
    c := pal[dc_color];
    ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];

    cnt := num_batch_columns;
    while cnt > 0 do
    begin
      destl^ := ldest;
      inc(destl);
      dec(cnt);
    end;
  end
  else
  begin
    c := pal[dc_color];
    r1 := c;
    g1 := c shr 8;
    b1 := c shr 16;
    ldest := precal32_ic[r1 + g1 + b1];

    cnt := num_batch_columns;
    while cnt > 0 do
    begin
      destl^ := ldest;
      inc(destl);
      dec(cnt);
    end;
  end;
end;

procedure R_DrawColorColumnHi_cnt1;
var
  destl: PLongWord;
  ldest: LongWord;
  c: LongWord;
  cnt: integer;
  swidth: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
begin
  {$IFDEF DOOM}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;

  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);
  swidth := SCREENWIDTH32PITCH - num_batch_columns * SizeOf(LongWord);

  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b);
    c := pal[dc_color];
    ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];

    cnt := num_batch_columns;
    while cnt > 0 do
    begin
      destl^ := ldest;
      inc(destl);
      dec(cnt);
    end;
    destl := PLongWord(integer(destl) + swidth);
    cnt := num_batch_columns;
    while cnt > 0 do
    begin
      destl^ := ldest;
      inc(destl);
      dec(cnt);
    end;
  end
  else
  begin
    c := pal[dc_color];
    r1 := c;
    g1 := c shr 8;
    b1 := c shr 16;
    ldest := precal32_ic[r1 + g1 + b1];

    cnt := num_batch_columns;
    while cnt > 0 do
    begin
      destl^ := ldest;
      inc(destl);
      dec(cnt);
    end;
    destl := PLongWord(integer(destl) + swidth);
    cnt := num_batch_columns;
    while cnt > 0 do
    begin
      destl^ := ldest;
      inc(destl);
      dec(cnt);
    end;
  end;
end;

procedure R_DrawColorColumnHi_bc1(const count: integer);
var
  destl: PLongWord;
  ldest: LongWord;
  c: LongWord;
  cnt: integer;
  swidth: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
begin
  {$IFDEF DOOM}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;

  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);
  swidth := SCREENWIDTH32PITCH;

  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b);
    c := pal[dc_color];
    ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];

    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end
  else
  begin
    c := pal[dc_color];
    r1 := c;
    g1 := c shr 8;
    b1 := c shr 16;
    ldest := precal32_ic[r1 + g1 + b1];

    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end;
end;

procedure R_DrawColorColumnHi_bc2(const count: integer);
var
  destl: PLongWord;
  ldest: LongWord;
  c: LongWord;
  cnt: integer;
  swidth: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
begin
  {$IFDEF DOOM}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;

  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);
  swidth := SCREENWIDTH32PITCH - SizeOf(LongWord);

  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b);
    c := pal[dc_color];
    ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];

    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end
  else
  begin
    c := pal[dc_color];
    r1 := c;
    g1 := c shr 8;
    b1 := c shr 16;
    ldest := precal32_ic[r1 + g1 + b1];

    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end;
end;

procedure R_DrawColorColumnHi_bc3(const count: integer);
var
  destl: PLongWord;
  ldest: LongWord;
  c: LongWord;
  cnt: integer;
  swidth: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
begin
  {$IFDEF DOOM}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;

  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);
  swidth := SCREENWIDTH32PITCH - 2 * SizeOf(LongWord);

  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b);
    c := pal[dc_color];
    ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];

    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end
  else
  begin
    c := pal[dc_color];
    r1 := c;
    g1 := c shr 8;
    b1 := c shr 16;
    ldest := precal32_ic[r1 + g1 + b1];

    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end;
end;

procedure R_DrawColorColumnHi_bc4(const count: integer);
var
  destl: PLongWord;
  ldest: LongWord;
  c: LongWord;
  cnt: integer;
  swidth: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
begin
  {$IFDEF DOOM}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;

  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);
  swidth := SCREENWIDTH32PITCH - 3 * SizeOf(LongWord);

  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b);
    c := pal[dc_color];
    ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];

    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end
  else
  begin
    c := pal[dc_color];
    r1 := c;
    g1 := c shr 8;
    b1 := c shr 16;
    ldest := precal32_ic[r1 + g1 + b1];

    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end;
end;

procedure R_DrawColorColumnHi;
var
  count: integer;
  destl: PLongWord;
  ldest: LongWord;
  c: LongWord;
  cnt: integer;
  swidth: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
begin
  count := dc_yh - dc_yl;

  if count < 0 then
    exit;

  if num_batch_columns = 2 then
  begin
    R_DrawColorColumnHi_bc2(count);
    exit;
  end
  else if num_batch_columns = 3 then
  begin
    R_DrawColorColumnHi_bc3(count);
    exit;
  end
  else if num_batch_columns = 1 then
  begin
    R_DrawColorColumnHi_bc1(count);
    exit;
  end
  else if num_batch_columns = 4 then
  begin
    R_DrawColorColumnHi_bc4(count);
    exit;
  end
  else if count = 0 then
  begin
    R_DrawColorColumnHi_cnt0;
    exit;
  end
  else if count = 1 then
  begin
    R_DrawColorColumnHi_cnt1;
    exit;
  end;

  {$IFDEF DOOM}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;

  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);

  swidth := SCREENWIDTH32PITCH - num_batch_columns * SizeOf(LongWord);
  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b);
    c := pal[dc_color];
    ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];

    while count >= 8 do
    begin
      cnt := num_batch_columns;
      while cnt > 0 do
      begin
        destl^ := ldest;
        inc(destl);
        dec(cnt);
      end;
      destl := PLongWord(integer(destl) + swidth);

      cnt := num_batch_columns;
      while cnt > 0 do
      begin
        destl^ := ldest;
        inc(destl);
        dec(cnt);
      end;
      destl := PLongWord(integer(destl) + swidth);

      cnt := num_batch_columns;
      while cnt > 0 do
      begin
        destl^ := ldest;
        inc(destl);
        dec(cnt);
      end;
      destl := PLongWord(integer(destl) + swidth);

      cnt := num_batch_columns;
      while cnt > 0 do
      begin
        destl^ := ldest;
        inc(destl);
        dec(cnt);
      end;
      destl := PLongWord(integer(destl) + swidth);

      cnt := num_batch_columns;
      while cnt > 0 do
      begin
        destl^ := ldest;
        inc(destl);
        dec(cnt);
      end;
      destl := PLongWord(integer(destl) + swidth);

      cnt := num_batch_columns;
      while cnt > 0 do
      begin
        destl^ := ldest;
        inc(destl);
        dec(cnt);
      end;
      destl := PLongWord(integer(destl) + swidth);

      cnt := num_batch_columns;
      while cnt > 0 do
      begin
        destl^ := ldest;
        inc(destl);
        dec(cnt);
      end;
      destl := PLongWord(integer(destl) + swidth);

      cnt := num_batch_columns;
      while cnt > 0 do
      begin
        destl^ := ldest;
        inc(destl);
        dec(cnt);
      end;
      destl := PLongWord(integer(destl) + swidth);

      count := count - 8;
    end;

    while count >= 0 do
    begin
      cnt := num_batch_columns;
      while cnt > 0 do
      begin
        destl^ := ldest;
        inc(destl);
        dec(cnt);
      end;

      destl := PLongWord(integer(destl) + swidth);
      dec(count);
    end;
  end
  else
  begin
    c := pal[dc_color];
    r1 := c;
    g1 := c shr 8;
    b1 := c shr 16;
    ldest := precal32_ic[r1 + g1 + b1];

    while count >= 8 do
    begin
      cnt := num_batch_columns;
      while cnt > 0 do
      begin
        destl^ := ldest;
        inc(destl);
        dec(cnt);
      end;
      destl := PLongWord(integer(destl) + swidth);

      cnt := num_batch_columns;
      while cnt > 0 do
      begin
        destl^ := ldest;
        inc(destl);
        dec(cnt);
      end;
      destl := PLongWord(integer(destl) + swidth);

      cnt := num_batch_columns;
      while cnt > 0 do
      begin
        destl^ := ldest;
        inc(destl);
        dec(cnt);
      end;
      destl := PLongWord(integer(destl) + swidth);

      cnt := num_batch_columns;
      while cnt > 0 do
      begin
        destl^ := ldest;
        inc(destl);
        dec(cnt);
      end;
      destl := PLongWord(integer(destl) + swidth);

      cnt := num_batch_columns;
      while cnt > 0 do
      begin
        destl^ := ldest;
        inc(destl);
        dec(cnt);
      end;
      destl := PLongWord(integer(destl) + swidth);

      cnt := num_batch_columns;
      while cnt > 0 do
      begin
        destl^ := ldest;
        inc(destl);
        dec(cnt);
      end;
      destl := PLongWord(integer(destl) + swidth);

      cnt := num_batch_columns;
      while cnt > 0 do
      begin
        destl^ := ldest;
        inc(destl);
        dec(cnt);
      end;
      destl := PLongWord(integer(destl) + swidth);

      cnt := num_batch_columns;
      while cnt > 0 do
      begin
        destl^ := ldest;
        inc(destl);
        dec(cnt);
      end;
      destl := PLongWord(integer(destl) + swidth);

      count := count - 8;
    end;

    while count >= 0 do
    begin
      cnt := num_batch_columns;
      while cnt > 0 do
      begin
        destl^ := ldest;
        inc(destl);
        dec(cnt);
      end;
      destl := PLongWord(integer(destl) + swidth);

      dec(count);
    end;
  end;
end;

{$IFDEF DOOM}
procedure R_DrawColorColumnAverageMedium_Batch;
begin

end;
{$ENDIF}

procedure R_DrawColorColumnAverageHi_Batch;
begin

end;

{$IFDEF DOOM}
procedure R_DrawColorColumnAlphaMedium_Batch;
begin

end;

{$ENDIF}
procedure R_DrawColorColumnAlphaHi_Batch;
begin

end;

end.


