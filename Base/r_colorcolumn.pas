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
//  Draw Color Func
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit r_colorcolumn;

interface

//==============================================================================
//
// R_DrawColorColumnMedium
//
//==============================================================================
procedure R_DrawColorColumnMedium;

//==============================================================================
//
// R_DrawColorColumnHi
//
//==============================================================================
procedure R_DrawColorColumnHi;
{$IFDEF DOOM}

//==============================================================================
//
// R_DrawColorColumnAverageMedium_Batch
//
//==============================================================================
procedure R_DrawColorColumnAverageMedium_Batch;
{$ENDIF}

//==============================================================================
//
// R_DrawColorColumnAverageHi_Batch
//
//==============================================================================
procedure R_DrawColorColumnAverageHi_Batch;
{$IFDEF DOOM}

//==============================================================================
//
// R_DrawColorColumnAlphaMedium_Batch
//
//==============================================================================
procedure R_DrawColorColumnAlphaMedium_Batch;
{$ENDIF}

//==============================================================================
//
// R_DrawColorColumnAlphaHi_Batch
//
//==============================================================================
procedure R_DrawColorColumnAlphaHi_Batch;

var
  dc_color: byte;
  dc_color32: LongWord;

type
  putpixelfunc_t = procedure(const x, y: integer);

var
  putpixelfunc: putpixelfunc_t;

//==============================================================================
//
// R_PutPixel8
//
//==============================================================================
procedure R_PutPixel8(const x, y: integer);

//==============================================================================
//
// R_PutPixel32
//
//==============================================================================
procedure R_PutPixel32(const x, y: integer);

implementation

uses
  d_delphi,
  doomdef,
  r_precalc,
  r_column,
  r_batchcolumn,
  {$IFDEF DOOM_OR_STRIFE}
  r_colormaps,
  {$ENDIF}
  v_video,
  r_draw;

//==============================================================================
//
// R_PutPixel8
//
//==============================================================================
procedure R_PutPixel8(const x, y: integer);
begin
  PByte(@((ylookup[y]^)[columnofs[x]]))^ := dc_colormap[dc_color];
end;

//==============================================================================
//
// R_PutPixel32
//
//==============================================================================
procedure R_PutPixel32(const x, y: integer);
var
  c: LongWord;
  r1, g1, b1: byte;
  pal: PLongWordArray;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
begin
  {$IFDEF DOOM_OR_STRIFE}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;

  if dc_lightlevel >= 0 then
  begin
    R_GetPrecalc32Tables(dc_lightlevel, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
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

//==============================================================================
//
// R_DrawColorColumnMedium_cnt0
//
//==============================================================================
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

  case num_batch_columns of
    1:
      begin
        dest^ := bdest;
        exit;
      end;
    2:
      begin
        PWord(dest)^ := precal8_toword[bdest];
        exit;
      end;
    3:
      begin
        dest^ := bdest;
        inc(dest);
        PWord(dest)^ := precal8_toword[bdest];
        exit;
      end;
    4:
      begin
        PLongWord(dest)^ := precal8_tolong[bdest];
        exit;
      end;
    5:
      begin
        dest^ := bdest;
        inc(dest);
        PLongWord(dest)^ := precal8_tolong[bdest];
        exit;
      end;
    6:
      begin
        PWord(dest)^ := precal8_toword[bdest];
        inc(dest, 2);
        PLongWord(dest)^ := precal8_tolong[bdest];
        exit;
      end;
    7:
      begin
        dest^ := bdest;
        inc(dest);
        PWord(dest)^ := precal8_toword[bdest];
        inc(dest, 2);
        PLongWord(dest)^ := precal8_tolong[bdest];
        exit;
      end;
    8:
      begin
        ldest := precal8_tolong[bdest];
        PLongWord(dest)^ := ldest;
        inc(dest, 4);
        PLongWord(dest)^ := ldest;
        exit;
      end;
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

//==============================================================================
//
// R_DrawColorColumnMedium_cnt1
//
//==============================================================================
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

  case num_batch_columns of
    1:
      begin
        dest^ := bdest;
        inc(dest, SCREENWIDTH);
        dest^ := bdest;
        exit;
      end;
    2:
      begin
        wdest := precal8_toword[bdest];
        PWord(dest)^ := wdest;
        inc(dest, SCREENWIDTH);
        PWord(dest)^ := wdest;
        exit;
      end;
    3:
      begin
        wdest := precal8_toword[bdest];
        dest^ := bdest;
        inc(dest);
        PWord(dest)^ := wdest;
        inc(dest, SCREENWIDTH - 1);
        dest^ := bdest;
        inc(dest);
        PWord(dest)^ := wdest;
        exit;
      end;
    4:
      begin
        ldest := precal8_tolong[bdest];
        PLongWord(dest)^ := ldest;
        inc(dest, SCREENWIDTH);
        PLongWord(dest)^ := ldest;
        exit;
      end;
  end;

  ldest := precal8_tolong[bdest];

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

//==============================================================================
//
// R_DrawColorColumnMedium
//
//==============================================================================
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

  case num_batch_columns of
    1:
      begin
        swidth := SCREENWIDTH;
        while count >= 4 do
        begin
          dest^ := bdest;
          inc(dest, swidth);
          dest^ := bdest;
          inc(dest, swidth);
          dest^ := bdest;
          inc(dest, swidth);
          dest^ := bdest;
          inc(dest, swidth);
          count := count - 4;
        end;
        while count >= 0 do
        begin
          dest^ := bdest;
          inc(dest, swidth);
          dec(count);
        end;

        exit;
      end;
    2:
      begin
        swidth := SCREENWIDTH;
        wdest := precal8_toword[bdest];
        while count >= 4 do
        begin
          PWord(dest)^ := wdest;
          inc(dest, swidth);
          PWord(dest)^ := wdest;
          inc(dest, swidth);
          PWord(dest)^ := wdest;
          inc(dest, swidth);
          PWord(dest)^ := wdest;
          inc(dest, swidth);
          dec(count, 4);
        end;
        while count >= 0 do
        begin
          PWord(dest)^ := wdest;
          inc(dest, swidth);
          dec(count);
        end;
        exit;
      end;
    3:
      begin
        swidth := SCREENWIDTH - 1;
        wdest := precal8_toword[bdest];
        while count >= 4 do
        begin
          dest^ := bdest;
          inc(dest);
          PWord(dest)^ := wdest;
          inc(dest, swidth);
          dest^ := bdest;
          inc(dest);
          PWord(dest)^ := wdest;
          inc(dest, swidth);
          dest^ := bdest;
          inc(dest);
          PWord(dest)^ := wdest;
          inc(dest, swidth);
          dest^ := bdest;
          inc(dest);
          PWord(dest)^ := wdest;
          inc(dest, swidth);
          dec(count, 4);
        end;
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
    4:
      begin
        ldest := precal8_tolong[bdest];
        swidth := SCREENWIDTH;
        while count >= 4 do
        begin
          PLongWord(dest)^ := ldest;
          inc(dest, swidth);
          PLongWord(dest)^ := ldest;
          inc(dest, swidth);
          PLongWord(dest)^ := ldest;
          inc(dest, swidth);
          PLongWord(dest)^ := ldest;
          inc(dest, swidth);
          dec(count, 4);
        end;
        while count >= 0 do
        begin
          PLongWord(dest)^ := ldest;
          inc(dest, swidth);
          dec(count);
        end;
        exit;
      end;
  end;

  ldest := precal8_tolong[bdest];
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

//==============================================================================
//
// R_DrawColorColumnHi_cnt0
//
//==============================================================================
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
  {$IFDEF DOOM_OR_STRIFE}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;

  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);

  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
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

//==============================================================================
//
// R_DrawColorColumnHi_cnt1
//
//==============================================================================
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
  {$IFDEF DOOM_OR_STRIFE}
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
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
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

//==============================================================================
//
// R_DrawColorColumnHi_bc1
//
//==============================================================================
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
  {$IFDEF DOOM_OR_STRIFE}
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
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
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

//==============================================================================
//
// R_DrawColorColumnHi_bc2
//
//==============================================================================
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
  {$IFDEF DOOM_OR_STRIFE}
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
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
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

//==============================================================================
//
// R_DrawColorColumnHi_bc3
//
//==============================================================================
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
  {$IFDEF DOOM_OR_STRIFE}
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
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
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

//==============================================================================
//
// R_DrawColorColumnHi_bc4
//
//==============================================================================
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
  {$IFDEF DOOM_OR_STRIFE}
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
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
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

{$IFNDEF OPTIMIZE_FOR_SIZE}

{$I R_DrawColorColumnHi_bc.inc}

type
  DrawColorColumnHi_bc_proc = procedure (const count: integer);

const
  R_DrawColorColumnHi_bc_cnt = 64;

var
  R_DrawColorColumnHi_bcTable: array[1..R_DrawColorColumnHi_bc_cnt] of DrawColorColumnHi_bc_proc;

procedure R_DrawColorColumnHi_bc_len(const count: integer; const len: integer);
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
  {$IFDEF DOOM_OR_STRIFE}
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
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
    c := pal[dc_color];
    ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];

    cnt := count;
    while cnt > 0 do
    begin
      FillDWord(destl, len, ldest);
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
      FillDWord(destl, len, ldest);
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end;
end;

{$ENDIF}

//==============================================================================
//
// R_DrawColorColumnHi
//
//==============================================================================
procedure R_DrawColorColumnHi;
var
  count: integer;
  destl: PLongWord;
  ldest: LongWord;
  c: LongWord;
  swidth: integer;
  lfactor: integer;
  batch_pitch: integer;
  deststop: PLongWord;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
begin
  count := dc_yh - dc_yl;

  if count < 0 then
    exit;

{$IFNDEF OPTIMIZE_FOR_SIZE}
  if num_batch_columns > 0 then
  begin
    if num_batch_columns <= R_DrawColorColumnHi_bc_cnt then
      R_DrawColorColumnHi_bcTable[num_batch_columns](count + 1)
    else
      R_DrawColorColumnHi_bc_len(count + 1, num_batch_columns);
    exit;
  end;

  if count = 0 then
  begin
    R_DrawColorColumnHi_cnt0;
    exit;
  end
  else if count = 1 then
  begin
    R_DrawColorColumnHi_cnt1;
    exit;
  end;
{$ENDIF}

  {$IFDEF DOOM_OR_STRIFE}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;

  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);

  swidth := SCREENWIDTH32PITCH - num_batch_columns * SizeOf(LongWord);
  lfactor := dc_lightlevel;
  batch_pitch := (num_batch_columns - 1) * SizeOf(LongWord);
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
    c := pal[dc_color];
    ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];

    while count >= 8 do
    begin
      deststop := PLongWord(integer(destl) + batch_pitch);
      while integer(destl) <= integer(deststop) do
      begin
        destl^ := ldest;
        inc(destl);
      end;
      destl := PLongWord(integer(destl) + swidth);

      deststop := PLongWord(integer(destl) + batch_pitch);
      while integer(destl) <= integer(deststop) do
      begin
        destl^ := ldest;
        inc(destl);
      end;
      destl := PLongWord(integer(destl) + swidth);

      deststop := PLongWord(integer(destl) + batch_pitch);
      while integer(destl) <= integer(deststop) do
      begin
        destl^ := ldest;
        inc(destl);
      end;
      destl := PLongWord(integer(destl) + swidth);

      deststop := PLongWord(integer(destl) + batch_pitch);
      while integer(destl) <= integer(deststop) do
      begin
        destl^ := ldest;
        inc(destl);
      end;
      destl := PLongWord(integer(destl) + swidth);

      deststop := PLongWord(integer(destl) + batch_pitch);
      while integer(destl) <= integer(deststop) do
      begin
        destl^ := ldest;
        inc(destl);
      end;
      destl := PLongWord(integer(destl) + swidth);

      deststop := PLongWord(integer(destl) + batch_pitch);
      while integer(destl) <= integer(deststop) do
      begin
        destl^ := ldest;
        inc(destl);
      end;
      destl := PLongWord(integer(destl) + swidth);

      deststop := PLongWord(integer(destl) + batch_pitch);
      while integer(destl) <= integer(deststop) do
      begin
        destl^ := ldest;
        inc(destl);
      end;
      destl := PLongWord(integer(destl) + swidth);

      deststop := PLongWord(integer(destl) + batch_pitch);
      while integer(destl) <= integer(deststop) do
      begin
        destl^ := ldest;
        inc(destl);
      end;
      destl := PLongWord(integer(destl) + swidth);

      count := count - 8;
    end;

    while count >= 0 do
    begin
      deststop := PLongWord(integer(destl) + batch_pitch);
      while integer(destl) <= integer(deststop) do
      begin
        destl^ := ldest;
        inc(destl);
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
      deststop := PLongWord(integer(destl) + batch_pitch);
      while integer(destl) <= integer(deststop) do
      begin
        destl^ := ldest;
        inc(destl);
      end;
      destl := PLongWord(integer(destl) + swidth);

      deststop := PLongWord(integer(destl) + batch_pitch);
      while integer(destl) <= integer(deststop) do
      begin
        destl^ := ldest;
        inc(destl);
      end;
      destl := PLongWord(integer(destl) + swidth);

      deststop := PLongWord(integer(destl) + batch_pitch);
      while integer(destl) <= integer(deststop) do
      begin
        destl^ := ldest;
        inc(destl);
      end;
      destl := PLongWord(integer(destl) + swidth);

      deststop := PLongWord(integer(destl) + batch_pitch);
      while integer(destl) <= integer(deststop) do
      begin
        destl^ := ldest;
        inc(destl);
      end;
      destl := PLongWord(integer(destl) + swidth);

      deststop := PLongWord(integer(destl) + batch_pitch);
      while integer(destl) <= integer(deststop) do
      begin
        destl^ := ldest;
        inc(destl);
      end;
      destl := PLongWord(integer(destl) + swidth);

      deststop := PLongWord(integer(destl) + batch_pitch);
      while integer(destl) <= integer(deststop) do
      begin
        destl^ := ldest;
        inc(destl);
      end;
      destl := PLongWord(integer(destl) + swidth);

      deststop := PLongWord(integer(destl) + batch_pitch);
      while integer(destl) <= integer(deststop) do
      begin
        destl^ := ldest;
        inc(destl);
      end;
      destl := PLongWord(integer(destl) + swidth);

      deststop := PLongWord(integer(destl) + batch_pitch);
      while integer(destl) <= integer(deststop) do
      begin
        destl^ := ldest;
        inc(destl);
      end;
      destl := PLongWord(integer(destl) + swidth);

      count := count - 8;
    end;

    while count >= 0 do
    begin
      deststop := PLongWord(integer(destl) + batch_pitch);
      while integer(destl) <= integer(deststop) do
      begin
        destl^ := ldest;
        inc(destl);
      end;
      destl := PLongWord(integer(destl) + swidth);

      dec(count);
    end;
  end;
end;

{$IFDEF DOOM}

//==============================================================================
//
// R_DrawColorColumnAverageMedium_Batch
//
//==============================================================================
procedure R_DrawColorColumnAverageMedium_Batch;
begin

end;
{$ENDIF}

//==============================================================================
//
// R_DrawColorColumnAverageHi_Batch
//
//==============================================================================
procedure R_DrawColorColumnAverageHi_Batch;
begin

end;

{$IFDEF DOOM}

//==============================================================================
//
// R_DrawColorColumnAlphaMedium_Batch
//
//==============================================================================
procedure R_DrawColorColumnAlphaMedium_Batch;
begin

end;

{$ENDIF}

//==============================================================================
//
// R_DrawColorColumnAlphaHi_Batch
//
//==============================================================================
procedure R_DrawColorColumnAlphaHi_Batch;
begin

end;

{$IFNDEF OPTIMIZE_FOR_SIZE}
initialization
  R_DrawColorColumnHi_bcTable[1] := @R_DrawColorColumnHi_bc1;
  R_DrawColorColumnHi_bcTable[2] := @R_DrawColorColumnHi_bc2;
  R_DrawColorColumnHi_bcTable[3] := @R_DrawColorColumnHi_bc3;
  R_DrawColorColumnHi_bcTable[4] := @R_DrawColorColumnHi_bc4;
  R_DrawColorColumnHi_bcTable[5] := @R_DrawColorColumnHi_bc5;
  R_DrawColorColumnHi_bcTable[6] := @R_DrawColorColumnHi_bc6;
  R_DrawColorColumnHi_bcTable[7] := @R_DrawColorColumnHi_bc7;
  R_DrawColorColumnHi_bcTable[8] := @R_DrawColorColumnHi_bc8;
  R_DrawColorColumnHi_bcTable[9] := @R_DrawColorColumnHi_bc9;
  R_DrawColorColumnHi_bcTable[10] := @R_DrawColorColumnHi_bc10;
  R_DrawColorColumnHi_bcTable[11] := @R_DrawColorColumnHi_bc11;
  R_DrawColorColumnHi_bcTable[12] := @R_DrawColorColumnHi_bc12;
  R_DrawColorColumnHi_bcTable[13] := @R_DrawColorColumnHi_bc13;
  R_DrawColorColumnHi_bcTable[14] := @R_DrawColorColumnHi_bc14;
  R_DrawColorColumnHi_bcTable[15] := @R_DrawColorColumnHi_bc15;
  R_DrawColorColumnHi_bcTable[16] := @R_DrawColorColumnHi_bc16;
  R_DrawColorColumnHi_bcTable[17] := @R_DrawColorColumnHi_bc17;
  R_DrawColorColumnHi_bcTable[18] := @R_DrawColorColumnHi_bc18;
  R_DrawColorColumnHi_bcTable[19] := @R_DrawColorColumnHi_bc19;
  R_DrawColorColumnHi_bcTable[20] := @R_DrawColorColumnHi_bc20;
  R_DrawColorColumnHi_bcTable[21] := @R_DrawColorColumnHi_bc21;
  R_DrawColorColumnHi_bcTable[22] := @R_DrawColorColumnHi_bc22;
  R_DrawColorColumnHi_bcTable[23] := @R_DrawColorColumnHi_bc23;
  R_DrawColorColumnHi_bcTable[24] := @R_DrawColorColumnHi_bc24;
  R_DrawColorColumnHi_bcTable[25] := @R_DrawColorColumnHi_bc25;
  R_DrawColorColumnHi_bcTable[26] := @R_DrawColorColumnHi_bc26;
  R_DrawColorColumnHi_bcTable[27] := @R_DrawColorColumnHi_bc27;
  R_DrawColorColumnHi_bcTable[28] := @R_DrawColorColumnHi_bc28;
  R_DrawColorColumnHi_bcTable[29] := @R_DrawColorColumnHi_bc29;
  R_DrawColorColumnHi_bcTable[30] := @R_DrawColorColumnHi_bc30;
  R_DrawColorColumnHi_bcTable[31] := @R_DrawColorColumnHi_bc31;
  R_DrawColorColumnHi_bcTable[32] := @R_DrawColorColumnHi_bc32;
  R_DrawColorColumnHi_bcTable[33] := @R_DrawColorColumnHi_bc33;
  R_DrawColorColumnHi_bcTable[34] := @R_DrawColorColumnHi_bc34;
  R_DrawColorColumnHi_bcTable[35] := @R_DrawColorColumnHi_bc35;
  R_DrawColorColumnHi_bcTable[36] := @R_DrawColorColumnHi_bc36;
  R_DrawColorColumnHi_bcTable[37] := @R_DrawColorColumnHi_bc37;
  R_DrawColorColumnHi_bcTable[38] := @R_DrawColorColumnHi_bc38;
  R_DrawColorColumnHi_bcTable[39] := @R_DrawColorColumnHi_bc39;
  R_DrawColorColumnHi_bcTable[40] := @R_DrawColorColumnHi_bc40;
  R_DrawColorColumnHi_bcTable[41] := @R_DrawColorColumnHi_bc41;
  R_DrawColorColumnHi_bcTable[42] := @R_DrawColorColumnHi_bc42;
  R_DrawColorColumnHi_bcTable[43] := @R_DrawColorColumnHi_bc43;
  R_DrawColorColumnHi_bcTable[44] := @R_DrawColorColumnHi_bc44;
  R_DrawColorColumnHi_bcTable[45] := @R_DrawColorColumnHi_bc45;
  R_DrawColorColumnHi_bcTable[46] := @R_DrawColorColumnHi_bc46;
  R_DrawColorColumnHi_bcTable[47] := @R_DrawColorColumnHi_bc47;
  R_DrawColorColumnHi_bcTable[48] := @R_DrawColorColumnHi_bc48;
  R_DrawColorColumnHi_bcTable[49] := @R_DrawColorColumnHi_bc49;
  R_DrawColorColumnHi_bcTable[50] := @R_DrawColorColumnHi_bc50;
  R_DrawColorColumnHi_bcTable[51] := @R_DrawColorColumnHi_bc51;
  R_DrawColorColumnHi_bcTable[52] := @R_DrawColorColumnHi_bc52;
  R_DrawColorColumnHi_bcTable[53] := @R_DrawColorColumnHi_bc53;
  R_DrawColorColumnHi_bcTable[54] := @R_DrawColorColumnHi_bc54;
  R_DrawColorColumnHi_bcTable[55] := @R_DrawColorColumnHi_bc55;
  R_DrawColorColumnHi_bcTable[56] := @R_DrawColorColumnHi_bc56;
  R_DrawColorColumnHi_bcTable[57] := @R_DrawColorColumnHi_bc57;
  R_DrawColorColumnHi_bcTable[58] := @R_DrawColorColumnHi_bc58;
  R_DrawColorColumnHi_bcTable[59] := @R_DrawColorColumnHi_bc59;
  R_DrawColorColumnHi_bcTable[60] := @R_DrawColorColumnHi_bc60;
  R_DrawColorColumnHi_bcTable[61] := @R_DrawColorColumnHi_bc61;
  R_DrawColorColumnHi_bcTable[62] := @R_DrawColorColumnHi_bc62;
  R_DrawColorColumnHi_bcTable[63] := @R_DrawColorColumnHi_bc63;
  R_DrawColorColumnHi_bcTable[64] := @R_DrawColorColumnHi_bc64;
{$ENDIF}

end.

