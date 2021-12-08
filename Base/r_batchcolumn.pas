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

unit r_batchcolumn;

// JVAL
// Batch column drawing for sprites

interface

uses
  d_delphi,
  m_fixed,
  r_main;

// Batch column drawers
procedure R_DrawColumnLow_Batch;
procedure R_DrawColumnMedium_Batch;
procedure R_DrawColumnHi_Batch;
{$IFNDEF STRIFE}
procedure R_DrawFuzzColumn_Batch;
procedure R_DrawFuzzColumn32_Batch;
procedure R_DrawFuzzColumnHi_Batch;
{$ENDIF}
procedure R_DrawTranslatedColumn_Batch;
procedure R_DrawTranslatedColumnHi_Batch;
{$IFDEF DOOM_OR_STRIFE}
procedure R_DrawColumnAverageMedium_Batch;
{$ENDIF}
procedure R_DrawColumnAverageHi_Batch;
{$IFDEF STRIFE}
procedure R_DrawFuzzColumn1_Batch;
procedure R_DrawFuzzColumn2_Batch;
procedure R_DrawFuzzColumnTL_Batch;
procedure R_DrawFuzzColumn1Hi_Batch;
procedure R_DrawFuzzColumn2Hi_Batch;
procedure R_DrawFuzzColumnHiTL_Batch;
{$ENDIF}
{$IFDEF DOOM_OR_STRIFE}
procedure R_DrawColumnAlphaMedium_Batch;
{$ENDIF}
procedure R_DrawColumnAlphaHi_Batch;
procedure R_DrawWhiteLightColumnHi_Batch;
procedure R_DrawRedLightColumnHi_Batch;
procedure R_DrawGreenLightColumnHi_Batch;
procedure R_DrawBlueLightColumnHi_Batch;
procedure R_DrawYellowLightColumnHi_Batch;

procedure R_DrawColumnAddMedium_Batch;
procedure R_DrawColumnAddHi_Batch;

procedure R_DrawColumnSubtractMedium_Batch;
procedure R_DrawColumnSubtractHi_Batch;

var
// JVAL: batch column drawing
  num_batch_columns: integer;
  optimizedthingsrendering: Boolean = true;

implementation

uses
  doomdef,
  doomtype,
  r_column,
  r_col_fz,
  r_precalc,
  r_trans8,
  {$IFDEF DOOM_OR_STRIFE}
  r_colormaps,
  {$ENDIF}
  r_data,
  r_draw,
  r_hires,
  v_video;

procedure R_DrawColumnLow_Batch;
var
  count: integer;
  i: integer;
  dest: PByte;
  bdest: byte;
  frac: fixed_t;
  fracstep: fixed_t;
  swidth: integer;
  cnt: integer;
begin
  count := (dc_yh - dc_yl) div 3;

  if count < 0 then
    exit;

  dest := @((ylookup[dc_yl]^)[columnofs[dc_x]]);

  frac := dc_texturemid + (dc_yl - centery) * dc_iscale;
  fracstep := 3 * dc_iscale;
  swidth := SCREENWIDTH - num_batch_columns;

  for i := 0 to count - 1 do
  begin
    bdest := dc_colormap[dc_source[(LongWord(frac) shr FRACBITS) and 127]];

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

    inc(frac, fracstep);
  end;

  count := (dc_yh - dc_yl) mod 3;
  for i := 0 to count do
  begin
    bdest := dc_colormap[dc_source[(LongWord(frac) shr FRACBITS) and 127]];

    cnt := num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      inc(dest);
      dec(cnt);
    end;
    inc(dest, swidth);

    inc(frac, dc_iscale);
  end;
end;

procedure R_DrawColumnMedium_Batch;
var
  count: integer;
  dest: PByte;
  frac: fixed_t;
  fracstep: fixed_t;
  fraclimit: fixed_t;
  fraclimit2: fixed_t;
  swidth: integer;
  dc_local: PByteArray;
  bdest: byte;
  ldest: LongWord;
  rest_batch_columns: integer;
  num_iters: integer;
  cnt: integer;
begin
  count := dc_yh - dc_yl;

  // Zero length, column does not exceed a pixel.
  if count < 0 then
    exit;

  // Framebuffer destination address.
  // Use ylookup LUT to avoid multiply with ScreenWidth.
  // Use columnofs LUT for subwindows?
  dest := @((ylookup[dc_yl]^)[columnofs[dc_x]]);

  // Determine scaling,
  //  which is the only mapping to be done.
  fracstep := dc_iscale;
  frac := dc_texturemid + (dc_yl - centery) * fracstep;
  fraclimit := frac + count * fracstep;
  fraclimit2 := frac + (count - 16) * fracstep;
  swidth := SCREENWIDTH - num_batch_columns;
  dc_local := dc_source;

  if num_batch_columns >= 4 then
  begin
    rest_batch_columns := num_batch_columns mod 4;
    num_iters := num_batch_columns div 4;

    if rest_batch_columns = 0 then
    begin
      while frac <= fraclimit2 do
      begin
      // Re-map color indices from wall texture column
      //  using a lighting/special effects LUT.
        bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
        ldest := precal8_tolong[bdest];
        cnt := num_iters;
        while cnt > 0 do
        begin
          PLongWord(dest)^ := ldest;
          inc(dest, 4);
          dec(cnt);
        end;
        inc(dest, swidth);
        inc(frac, fracstep);

        bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
        ldest := precal8_tolong[bdest];
        cnt := num_iters;
        while cnt > 0 do
        begin
          PLongWord(dest)^ := ldest;
          inc(dest, 4);
          dec(cnt);
        end;
        inc(dest, swidth);
        inc(frac, fracstep);

        bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
        ldest := precal8_tolong[bdest];
        cnt := num_iters;
        while cnt > 0 do
        begin
          PLongWord(dest)^ := ldest;
          inc(dest, 4);
          dec(cnt);
        end;
        inc(dest, swidth);
        inc(frac, fracstep);

        bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
        ldest := precal8_tolong[bdest];
        cnt := num_iters;
        while cnt > 0 do
        begin
          PLongWord(dest)^ := ldest;
          inc(dest, 4);
          dec(cnt);
        end;
        inc(dest, swidth);
        inc(frac, fracstep);

        bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
        ldest := precal8_tolong[bdest];
        cnt := num_iters;
        while cnt > 0 do
        begin
          PLongWord(dest)^ := ldest;
          inc(dest, 4);
          dec(cnt);
        end;
        inc(dest, swidth);
        inc(frac, fracstep);

        bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
        ldest := precal8_tolong[bdest];
        cnt := num_iters;
        while cnt > 0 do
        begin
          PLongWord(dest)^ := ldest;
          inc(dest, 4);
          dec(cnt);
        end;
        inc(dest, swidth);
        inc(frac, fracstep);

        bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
        ldest := precal8_tolong[bdest];
        cnt := num_iters;
        while cnt > 0 do
        begin
          PLongWord(dest)^ := ldest;
          inc(dest, 4);
          dec(cnt);
        end;
        inc(dest, swidth);
        inc(frac, fracstep);

        bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
        ldest := precal8_tolong[bdest];
        cnt := num_iters;
        while cnt > 0 do
        begin
          PLongWord(dest)^ := ldest;
          inc(dest, 4);
          dec(cnt);
        end;
        inc(dest, swidth);
        inc(frac, fracstep);

        bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
        ldest := precal8_tolong[bdest];
        cnt := num_iters;
        while cnt > 0 do
        begin
          PLongWord(dest)^ := ldest;
          inc(dest, 4);
          dec(cnt);
        end;
        inc(dest, swidth);
        inc(frac, fracstep);

        bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
        ldest := precal8_tolong[bdest];
        cnt := num_iters;
        while cnt > 0 do
        begin
          PLongWord(dest)^ := ldest;
          inc(dest, 4);
          dec(cnt);
        end;
        inc(dest, swidth);
        inc(frac, fracstep);

        bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
        ldest := precal8_tolong[bdest];
        cnt := num_iters;
        while cnt > 0 do
        begin
          PLongWord(dest)^ := ldest;
          inc(dest, 4);
          dec(cnt);
        end;
        inc(dest, swidth);
        inc(frac, fracstep);

        bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
        ldest := precal8_tolong[bdest];
        cnt := num_iters;
        while cnt > 0 do
        begin
          PLongWord(dest)^ := ldest;
          inc(dest, 4);
          dec(cnt);
        end;
        inc(dest, swidth);
        inc(frac, fracstep);

        bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
        ldest := precal8_tolong[bdest];
        cnt := num_iters;
        while cnt > 0 do
        begin
          PLongWord(dest)^ := ldest;
          inc(dest, 4);
          dec(cnt);
        end;
        inc(dest, swidth);
        inc(frac, fracstep);

        bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
        ldest := precal8_tolong[bdest];
        cnt := num_iters;
        while cnt > 0 do
        begin
          PLongWord(dest)^ := ldest;
          inc(dest, 4);
          dec(cnt);
        end;
        inc(dest, swidth);
        inc(frac, fracstep);

        bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
        ldest := precal8_tolong[bdest];
        cnt := num_iters;
        while cnt > 0 do
        begin
          PLongWord(dest)^ := ldest;
          inc(dest, 4);
          dec(cnt);
        end;
        inc(dest, swidth);
        inc(frac, fracstep);

        bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
        ldest := precal8_tolong[bdest];
        cnt := num_iters;
        while cnt > 0 do
        begin
          PLongWord(dest)^ := ldest;
          inc(dest, 4);
          dec(cnt);
        end;
        inc(dest, swidth);
        inc(frac, fracstep);
      end;

      while frac <= fraclimit do
      begin
      // Re-map color indices from wall texture column
      //  using a lighting/special effects LUT.
        bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
        ldest := precal8_tolong[bdest];
        cnt := num_iters;
        while cnt > 0 do
        begin
          PLongWord(dest)^ := ldest;
          inc(dest, 4);
          dec(cnt);
        end;
        inc(dest, swidth);
        inc(frac, fracstep);
      end;
      exit;
    end;

    while frac <= fraclimit2 do
    begin
    // Re-map color indices from wall texture column
    //  using a lighting/special effects LUT.
      bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
      ldest := precal8_tolong[bdest];
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
      inc(frac, fracstep);

      bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
      ldest := precal8_tolong[bdest];
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
      inc(frac, fracstep);

      bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
      ldest := precal8_tolong[bdest];
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
      inc(frac, fracstep);

      bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
      ldest := precal8_tolong[bdest];
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
      inc(frac, fracstep);

      bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
      ldest := precal8_tolong[bdest];
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
      inc(frac, fracstep);

      bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
      ldest := precal8_tolong[bdest];
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
      inc(frac, fracstep);

      bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
      ldest := precal8_tolong[bdest];
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
      inc(frac, fracstep);

      bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
      ldest := precal8_tolong[bdest];
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
      inc(frac, fracstep);

      bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
      ldest := precal8_tolong[bdest];
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
      inc(frac, fracstep);

      bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
      ldest := precal8_tolong[bdest];
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
      inc(frac, fracstep);

      bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
      ldest := precal8_tolong[bdest];
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
      inc(frac, fracstep);

      bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
      ldest := precal8_tolong[bdest];
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
      inc(frac, fracstep);

      bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
      ldest := precal8_tolong[bdest];
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
      inc(frac, fracstep);

      bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
      ldest := precal8_tolong[bdest];
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
      inc(frac, fracstep);

      bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
      ldest := precal8_tolong[bdest];
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
      inc(frac, fracstep);

      bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
      ldest := precal8_tolong[bdest];
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
      inc(frac, fracstep);
    end;

    while frac <= fraclimit do
    begin
    // Re-map color indices from wall texture column
    //  using a lighting/special effects LUT.
      bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
      ldest := precal8_tolong[bdest];
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
      inc(frac, fracstep);
    end;
    exit;
  end;

  // Inner loop that does the actual texture mapping,
  //  e.g. a DDA-lile scaling.
  // This is as fast as it gets.
  while frac <= fraclimit2 do
  begin
  // Re-map color indices from wall texture column
  //  using a lighting/special effects LUT.
    bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    cnt := num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      inc(dest);
      dec(cnt);
    end;
    inc(dest, swidth);
    inc(frac, fracstep);

    bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    cnt := num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      inc(dest);
      dec(cnt);
    end;
    inc(dest, swidth);
    inc(frac, fracstep);

    bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    cnt := num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      inc(dest);
      dec(cnt);
    end;
    inc(dest, swidth);
    inc(frac, fracstep);

    bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    cnt := num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      inc(dest);
      dec(cnt);
    end;
    inc(dest, swidth);
    inc(frac, fracstep);

    bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    cnt := num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      inc(dest);
      dec(cnt);
    end;
    inc(dest, swidth);
    inc(frac, fracstep);

    bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    cnt := num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      inc(dest);
      dec(cnt);
    end;
    inc(dest, swidth);
    inc(frac, fracstep);

    bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    cnt := num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      inc(dest);
      dec(cnt);
    end;
    inc(dest, swidth);
    inc(frac, fracstep);

    bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    cnt := num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      inc(dest);
      dec(cnt);
    end;
    inc(dest, swidth);
    inc(frac, fracstep);

    bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    cnt := num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      inc(dest);
      dec(cnt);
    end;
    inc(dest, swidth);
    inc(frac, fracstep);

    bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    cnt := num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      inc(dest);
      dec(cnt);
    end;
    inc(dest, swidth);
    inc(frac, fracstep);

    bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    cnt := num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      inc(dest);
      dec(cnt);
    end;
    inc(dest, swidth);
    inc(frac, fracstep);

    bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    cnt := num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      inc(dest);
      dec(cnt);
    end;
    inc(dest, swidth);
    inc(frac, fracstep);

    bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    cnt := num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      inc(dest);
      dec(cnt);
    end;
    inc(dest, swidth);
    inc(frac, fracstep);

    bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    cnt := num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      inc(dest);
      dec(cnt);
    end;
    inc(dest, swidth);
    inc(frac, fracstep);

    bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    cnt := num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      inc(dest);
      dec(cnt);
    end;
    inc(dest, swidth);
    inc(frac, fracstep);

    bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    cnt := num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      inc(dest);
      dec(cnt);
    end;
    inc(dest, swidth);
    inc(frac, fracstep);
  end;

  while frac <= fraclimit do
  begin
  // Re-map color indices from wall texture column
  //  using a lighting/special effects LUT.
    bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    cnt := num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      inc(dest);
      dec(cnt);
    end;
    inc(dest, swidth);
    inc(frac, fracstep);
  end;
end;

procedure R_DrawColumnHi_Batch;
var
  count: integer;
  destl: PLongWord;
  deststop: PLongWord;
  deststopX4: PLongWord;
  ldest: LongWord;
  c: LongWord;
  frac: fixed_t;
  fracstep: fixed_t;
  spot: integer;
  lastspot: integer;
  swidth: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
  pitch: integer;
  buf: twolongwords_t;
begin
  count := dc_yh - dc_yl;

  if count < 0 then
    exit;

  {$IFDEF DOOM_OR_STRIFE}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;

  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);
  pitch := num_batch_columns * SizeOf(LongWord);
  deststop := PLongWord(integer(destl) + pitch);
  swidth := SCREENWIDTH32PITCH - pitch;
  fracstep := dc_iscale;
  frac := dc_texturemid + (dc_yl - centery) * fracstep;
  lfactor := dc_lightlevel;

  if num_batch_columns > 4 then
  begin
    lastspot := 128;
    ldest := 0; // JVAL: avoid compiler warning
    deststopX4 := PLongWord(integer(deststop) - 4 * SizeOf(pointer));
    if lfactor >= 0 then
    begin
      R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
      while count >= 0 do
      begin
        spot := (LongWord(frac) shr FRACBITS) and 127;
        if lastspot <> spot then
        begin
          c := pal[dc_source[spot]];
          ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
          buf.longword1 := ldest;
          buf.longword2 := ldest;
          lastspot := spot;
        end;

        while integer(destl) <= integer(deststopX4) do
        begin
          PInt64(destl)^ := PInt64(@buf)^;
          inc(destl, 2);
          PInt64(destl)^ := PInt64(@buf)^;
          inc(destl, 2);
        end;

        while integer(destl) < integer(deststop) do
        begin
          destl^ := ldest;
          inc(destl);
        end;

        destl := PLongWord(integer(destl) + swidth);
        deststop := PLongWord(integer(destl) + pitch);
        deststopX4 := PLongWord(integer(deststop) - 4 * SizeOf(pointer));
        inc(frac, fracstep);
        dec(count);
      end;
    end
    else
    begin
      while count >= 0 do
      begin
        spot := (LongWord(frac) shr FRACBITS) and 127;
        if lastspot <> spot then
        begin
          c := pal[dc_source[spot]];
          r1 := c;
          g1 := c shr 8;
          b1 := c shr 16;
          ldest := precal32_ic[r1 + g1 + b1];
          buf.longword1 := ldest;
          buf.longword2 := ldest;
          lastspot := spot;
        end;

        while integer(destl) < integer(deststopX4) do
        begin
          PInt64(destl)^ := PInt64(@buf)^;
          inc(destl, 2);
          PInt64(destl)^ := PInt64(@buf)^;
          inc(destl, 2);
        end;

        while integer(destl) < integer(deststop) do
        begin
          destl^ := ldest;
          inc(destl);
        end;

        destl := PLongWord(integer(destl) + swidth);
        deststop := PLongWord(integer(destl) + pitch);
        deststopX4 := PLongWord(integer(deststop) - 4 * SizeOf(pointer));
        inc(frac, fracstep);
        dec(count);
      end;
    end;
  end
  else
  begin
    if lfactor >= 0 then
    begin
      R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
      while count >= 0 do
      begin
        spot := (LongWord(frac) shr FRACBITS) and 127;
        c := pal[dc_source[spot]];
        ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];

        while integer(destl) < integer(deststop) do
        begin
          destl^ := ldest;
          inc(destl);
        end;

        destl := PLongWord(integer(destl) + swidth);
        deststop := PLongWord(integer(destl) + pitch);
        inc(frac, fracstep);
        dec(count);
      end;
    end
    else
    begin
      while count >= 0 do
      begin
        spot := (LongWord(frac) shr FRACBITS) and 127;
        c := pal[dc_source[spot]];
        r1 := c;
        g1 := c shr 8;
        b1 := c shr 16;
        ldest := precal32_ic[r1 + g1 + b1];

        while integer(destl) < integer(deststop) do
        begin
          destl^ := ldest;
          inc(destl);
        end;

        destl := PLongWord(integer(destl) + swidth);
        deststop := PLongWord(integer(destl) + pitch);
        inc(frac, fracstep);
        dec(count);
      end;
    end;
  end;
end;

{$IFNDEF STRIFE}
procedure R_DrawFuzzColumn_Batch;
var
  count: integer;
  i: integer;
  dest: PByteArray;
  cnt: integer;
  swidth: integer;
begin
  // Adjust borders. Low...
  if dc_yl = 0 then
    dc_yl := 1;

  // .. and high.
  if dc_yh = viewheight - 1 then
    dc_yh := viewheight - 2;

  count := dc_yh - dc_yl;

  // Zero length.
  if count < 0 then
    exit;

  // Does not work with blocky mode.
  dest := @((ylookup[dc_yl]^)[columnofs[dc_x]]);
  swidth := SCREENWIDTH - num_batch_columns;

  // Looks like an attempt at dithering,
  //  using the colormap #6 (of 0-31, a bit
  //  brighter than average).
  for i := 0 to count do
  begin
    // Lookup framebuffer, and retrieve
    //  a pixel that is either one column
    //  left or right of the current one.
    // Add index from colormap to index.
    cnt := num_batch_columns;
    while cnt > 0 do
    begin
      dest[0] := colormaps[6 * 256 + dest[sfuzzoffset[fuzzpos]]];
      dest := @dest[1];
    // Clamp table lookup index.
      inc(fuzzpos);
      if fuzzpos = FUZZTABLE then
        fuzzpos := 0;
      dec(cnt);
    end;
    dest := @dest[swidth];
  end;
end;

procedure R_DrawFuzzColumn32_Batch;
var
  count: integer;
  i: integer;
  destl: PLongWordArray;
  cnt: integer;
  swidth: integer;
begin
  // Adjust borders. Low...
  if dc_yl = 0 then
    dc_yl := 1;

  // .. and high.
  if dc_yh = viewheight - 1 then
    dc_yh := viewheight - 2;

  count := dc_yh - dc_yl;

  // Zero length.
  if count < 0 then
    exit;

  // Does not work with blocky mode.
  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);
  swidth := SCREENWIDTH - num_batch_columns;

  // Looks like an attempt at dithering,
  //  using the colormap #6 (of 0-31, a bit
  //  brighter than average).
  for i := 0 to count do
  begin
    // Lookup framebuffer, and retrieve
    //  a pixel that is either one column
    //  left or right of the current one.
    // Add index from colormap to index.
    cnt := num_batch_columns;
    while cnt > 0 do
    begin
      destl[0] := R_ColorLight(destl[sfuzzoffset[fuzzpos]], $C000);
      destl := @destl[1];
    // Clamp table lookup index.
      inc(fuzzpos);
      if fuzzpos = FUZZTABLE then
        fuzzpos := 0;
      dec(cnt);
    end;
    destl := @destl[swidth];
  end;
end;

procedure R_DrawFuzzColumnHi_Batch;
var
  count: integer;
  i: integer;
  destl: PLongWordArray;
  swidth: integer;
  cnt: integer;

  r1, g1, b1: byte;
  c, r, g, b: LongWord;
begin
  // Adjust borders. Low...
  if dc_yl = 0 then
    dc_yl := 1;

  // .. and high.
  if dc_yh = viewheight - 1 then
    dc_yh := viewheight - 2;

  count := dc_yh - dc_yl;

  // Zero length.
  if count < 0 then
    exit;

  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);

  swidth := SCREENWIDTH - num_batch_columns;
  for i := 0 to count do
  begin

    cnt := num_batch_columns;
    while cnt > 0 do
    begin
      c := destl[0];
      r1 := c;
      g1 := c shr 8;
      b1 := c shr 16;
      r := r1 shr 3 * 7;
      g := g1 shr 3 * 7;
      b := b1 shr 3 * 7;

      destl[0] := r + g shl 8 + b shl 16;
      destl := @destl[1];
      dec(cnt);
    end;

    destl := @destl[swidth];

  end;
end;
{$ENDIF}


procedure R_DrawTranslatedColumn_Batch;
var
  count: integer;
  dest: PByte;
  bdest: byte;
  frac: fixed_t;
  fracstep: fixed_t;
  i: integer;
  cnt: integer;
  swidth: integer;
begin
  count := dc_yh - dc_yl;

  if count < 0 then
    exit;

  // FIXME. As above.
  dest := @((ylookup[dc_yl]^)[columnofs[dc_x]]);
  swidth := SCREENWIDTH - num_batch_columns;

  // Looks familiar.
  fracstep := dc_iscale;
  frac := dc_texturemid + (dc_yl - centery) * fracstep;

  // Here we do an additional index re-mapping.
  for i := 0 to count do
  begin
    // Translation tables are used
    //  to map certain colorramps to other ones,
    //  used with PLAY sprites.
    // Thus the "green" ramp of the player 0 sprite
    //  is mapped to gray, red, black/indigo.
    bdest := dc_colormap[dc_translation[dc_source[(LongWord(frac) shr FRACBITS) and 127]]];

    cnt := num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      inc(dest);
      dec(cnt);
    end;

    inc(dest, swidth);

    inc(frac, fracstep);
  end;
end;

procedure R_DrawTranslatedColumnHi_Batch;
var
  count: integer;
  destl: PLongWord;
  ldest: LongWord;
  frac: fixed_t;
  fracstep: fixed_t;
  i: integer;
  swidth: integer;
  cnt: integer;
begin
  count := dc_yh - dc_yl;

  if count < 0 then
    exit;

  // FIXME. As above.
  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);

  // Looks familiar.
  fracstep := dc_iscale;
  frac := dc_texturemid + (dc_yl - centery) * fracstep;

  swidth := SCREENWIDTH32PITCH - num_batch_columns * SizeOf(LongWord);
  // Here we do an additional index re-mapping.
  for i := 0 to count do
  begin
    // Translation tables are used
    //  to map certain colorramps to other ones,
    //  used with PLAY sprites.
    // Thus the "green" ramp of the player 0 sprite
    //  is mapped to gray, red, black/indigo.
    ldest := dc_colormap32[dc_translation[dc_source[(LongWord(frac) shr FRACBITS) and 127]]];
    cnt := num_batch_columns;
    while cnt > 0 do
    begin
      destl^ := ldest;
      inc(destl);
      dec(cnt);
    end;

    destl := PLongWord(integer(destl) + swidth);

    inc(frac, fracstep);
  end;
end;

////////////////////////////////////////////////////////////////////////////////

{$IFDEF DOOM_OR_STRIFE}
procedure R_DrawColumnAverageMedium_Batch;
var
  count: integer;
  dest: PByte;
  b: byte;
  u: integer;
  frac: fixed_t;
  fracstep: fixed_t;
  fraclimit: fixed_t;
  swidth: integer;
  cnt: integer;
begin
  count := dc_yh - dc_yl;

  // Zero length, column does not exceed a pixel.
  if count < 0 then
    exit;

  // Framebuffer destination address.
  // Use ylookup LUT to avoid multiply with ScreenWidth.
  // Use columnofs LUT for subwindows?
  dest := @((ylookup[dc_yl]^)[columnofs[dc_x]]);

  // Determine scaling,
  //  which is the only mapping to be done.
  fracstep := dc_iscale;
  frac := dc_texturemid + (dc_yl - centery) * fracstep;
  fraclimit := frac + count * fracstep;
  swidth := SCREENWIDTH - num_batch_columns;

  // Inner loop that does the actual texture mapping,
  //  e.g. a DDA-lile scaling.
  // This is as fast as it gets.
  while frac <= fraclimit do
  begin
  // Re-map color indices from wall texture column
  //  using a lighting/special effects LUT.

    b := dc_colormap[dc_source[(LongWord(frac) shr FRACBITS) and 127]];
    u := b shl 8;
    cnt := num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := averagetrans8table[dest^ + u];
      inc(dest);
      dec(cnt);
    end;

    inc(dest, swidth);
    inc(frac, fracstep);
  end;
end;
{$ENDIF}

procedure R_DrawColumnAverageHi_Batch;
var
  count: integer;
  destl: PLongWord;
  frac: fixed_t;
  fracstep: fixed_t;
  swidth: integer;
  cnt: integer;

// For inline color averaging
  r1, g1, b1: byte;
  r2, g2, b2: byte;
  c3, c4, r, g, b: LongWord;

begin
  count := dc_yh - dc_yl;

  if count < 0 then
    exit;

  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);

  fracstep := dc_iscale;
  frac := dc_texturemid + (dc_yl - centery) * fracstep;

  // Inner loop that does the actual texture mapping,
  //  e.g. a DDA-lile scaling.
  // This is as fast as it gets.
  swidth := SCREENWIDTH32PITCH - num_batch_columns * SizeOf(LongWord);

  while count >= 0 do
  begin
    c3 := dc_colormap32[dc_source[(LongWord(frac) shr FRACBITS) and 127]];
    r1 := c3;
    g1 := c3 shr 8;
    b1 := c3 shr 16;

    cnt := num_batch_columns;
    while cnt > 0 do
    begin
      c4 := destl^;
      r2 := c4;
      g2 := c4 shr 8;
      b2 := c4 shr 16;
      r := (r1 + r2) shr 1;
      g := (g1 + g2) shr 1;
      b := (b1 + b2) shr 1;
      destl^ := r + g shl 8 + b shl 16;
      inc(destl);
      dec(cnt);
    end;

    destl := PLongWord(integer(destl) + swidth);
    inc(frac, fracstep);
    dec(count);
  end;
end;

{$IFDEF DOOM_OR_STRIFE}
procedure R_DrawColumnAlphaMedium_Batch;
var
  count: integer;
  dest: PByte;
  b: byte;
  u: integer;
  frac: fixed_t;
  fracstep: fixed_t;
  fraclimit: fixed_t;
  swidth: integer;
  cnt: integer;
begin
  count := dc_yh - dc_yl;

  // Zero length, column does not exceed a pixel.
  if count < 0 then
    exit;

  // Framebuffer destination address.
  // Use ylookup LUT to avoid multiply with ScreenWidth.
  // Use columnofs LUT for subwindows?
  dest := @((ylookup[dc_yl]^)[columnofs[dc_x]]);

  // Determine scaling,
  //  which is the only mapping to be done.
  fracstep := dc_iscale;
  frac := dc_texturemid + (dc_yl - centery) * fracstep;
  fraclimit := frac + count * fracstep;
  swidth := SCREENWIDTH - num_batch_columns;

  // Inner loop that does the actual texture mapping,
  //  e.g. a DDA-lile scaling.
  // This is as fast as it gets.
  while frac <= fraclimit do
  begin
  // Re-map color indices from wall texture column
  //  using a lighting/special effects LUT.

    b := dc_colormap[dc_source[(LongWord(frac) shr FRACBITS) and 127]];
    u := b shl 8;
    cnt := num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := curtrans8table[dest^ + u];
      inc(dest);
      dec(cnt);
    end;


    inc(dest, swidth);
    inc(frac, fracstep);
  end;
end;
{$ENDIF}

procedure R_DrawColumnAlphaHi_Batch;
var
  count: integer;
  destl: PLongWord;
  frac: fixed_t;
  fracstep: fixed_t;
  fraclimit: fixed_t;
  swidth: integer;
  cfrac2: fixed_t;
  factor1: fixed_t;

// For inline color averaging
  r1, g1, b1: byte;
  r2, g2, b2: byte;
  fr2, fg2, fb2: integer;
  c1, c2, r, g, b: LongWord;

  cnt: integer;
begin
  count := dc_yh - dc_yl;

  if count < 0 then
    exit;

  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);

  fracstep := dc_iscale;
  frac := dc_texturemid + (dc_yl - centery) * fracstep;

  // Inner loop that does the actual texture mapping,
  //  e.g. a DDA-lile scaling.
  // This is as fast as it gets.
  swidth := SCREENWIDTH32PITCH - num_batch_columns * SizeOf(LongWord);
  cfrac2 := dc_alpha;
  factor1 := FRACUNIT - 1 - cfrac2;

  fraclimit := frac + fracstep * count;
  while frac <= fraclimit do
  begin
    c2 := dc_colormap32[dc_source[(LongWord(frac) shr FRACBITS) and 127]];
    r2 := c2;
    g2 := c2 shr 8;
    b2 := c2 shr 16;
    fr2 := cfrac2 * r2;
    fg2 := cfrac2 * g2;
    fb2 := cfrac2 * b2;

    cnt := num_batch_columns;
    while cnt > 0 do
    begin
    // Color averaging
      c1 := destl^;
      r1 := c1;
      g1 := c1 shr 8;
      b1 := c1 shr 16;

      r := ((fr2) + (r1 * factor1)) shr FRACBITS;
      g := ((fg2) + (g1 * factor1)) shr FRACBITS;
      b := ((fb2) + (b1 * factor1)) and $FF0000;

      destl^ := r + g shl 8 + b;
      inc(destl);
      dec(cnt);
    end;

    destl := PLongWord(integer(destl) + swidth);
    inc(frac, fracstep);
  end;
end;


{$IFDEF STRIFE}
procedure R_DrawFuzzColumn1_Batch;
var
  count: integer;
  dest: PByte;
  u: integer;
  frac: fixed_t;
  fracstep: fixed_t;
  fraclimit: fixed_t;
  swidth: integer;
  cnt: integer;
begin
  count := dc_yh - dc_yl;

  // Zero length, column does not exceed a pixel.
  if count < 0 then
    exit;

  // Framebuffer destination address.
  // Use ylookup LUT to avoid multiply with ScreenWidth.
  // Use columnofs LUT for subwindows?
  dest := @((ylookup[dc_yl]^)[columnofs[dc_x]]);

  // Determine scaling,
  //  which is the only mapping to be done.
  fracstep := dc_iscale;
  frac := dc_texturemid + (dc_yl - centery) * fracstep;
  fraclimit := frac + count * fracstep;
  swidth := SCREENWIDTH - num_batch_columns;

  // Inner loop that does the actual texture mapping,
  //  e.g. a DDA-lile scaling.
  // This is as fast as it gets.
  while frac <= fraclimit do
  begin
  // Re-map color indices from wall texture column
  //  using a lighting/special effects LUT.

    u := dc_colormap[dc_source[(LongWord(frac) shr FRACBITS) and 127]];
    cnt := num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := xlatab[dest^ * 256 + u];
      inc(dest);
      dec(cnt);
    end;

    inc(dest, swidth);
    inc(frac, fracstep);
  end;
end;

procedure R_DrawFuzzColumn2_Batch;
var
  count: integer;
  dest: PByte;
  b: byte;
  u: integer;
  frac: fixed_t;
  fracstep: fixed_t;
  fraclimit: fixed_t;
  swidth: integer;
  cnt: integer;
begin
  count := dc_yh - dc_yl;

  // Zero length, column does not exceed a pixel.
  if count < 0 then
    exit;

  // Framebuffer destination address.
  // Use ylookup LUT to avoid multiply with ScreenWidth.
  // Use columnofs LUT for subwindows?
  dest := @((ylookup[dc_yl]^)[columnofs[dc_x]]);

  // Determine scaling,
  //  which is the only mapping to be done.
  fracstep := dc_iscale;
  frac := dc_texturemid + (dc_yl - centery) * fracstep;
  fraclimit := frac + count * fracstep;
  swidth := SCREENWIDTH - num_batch_columns;

  // Inner loop that does the actual texture mapping,
  //  e.g. a DDA-lile scaling.
  // This is as fast as it gets.
  while frac <= fraclimit do
  begin
  // Re-map color indices from wall texture column
  //  using a lighting/special effects LUT.

    b := dc_colormap[dc_source[(LongWord(frac) shr FRACBITS) and 127]];
    u := b shl 8;
    cnt := num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := xlatab[dest^ + u];
      inc(dest);
      dec(cnt);
    end;

    inc(dest, swidth);
    inc(frac, fracstep);
  end;
end;

procedure R_DrawFuzzColumnTL_Batch;
var
  count: integer;
  dest: PByte;
  u: integer;
  frac: fixed_t;
  fracstep: fixed_t;
  fraclimit: fixed_t;
  swidth: integer;
  cnt: integer;
begin
  count := dc_yh - dc_yl;

  // Zero length, column does not exceed a pixel.
  if count < 0 then
    exit;

  // Framebuffer destination address.
  // Use ylookup LUT to avoid multiply with ScreenWidth.
  // Use columnofs LUT for subwindows?
  dest := @((ylookup[dc_yl]^)[columnofs[dc_x]]);

  // Determine scaling,
  //  which is the only mapping to be done.
  fracstep := dc_iscale;
  frac := dc_texturemid + (dc_yl - centery) * fracstep;
  fraclimit := frac + count * fracstep;
  swidth := SCREENWIDTH - num_batch_columns;

  // Inner loop that does the actual texture mapping,
  //  e.g. a DDA-lile scaling.
  // This is as fast as it gets.
  while frac <= fraclimit do
  begin
  // Re-map color indices from wall texture column
  //  using a lighting/special effects LUT.

    u := dc_colormap[dc_translation[dc_source[(LongWord(frac) shr FRACBITS) and 127]]];
    cnt := num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := xlatab[dest^ * 256 + u];
      inc(dest);
      dec(cnt);
    end;

    inc(dest, swidth);
    inc(frac, fracstep);
  end;
end;

procedure R_DrawFuzzColumn1Hi_Batch;
var
  count: integer;
  destl: PLongWord;
  frac: fixed_t;
  fracstep: fixed_t;
  fraclimit: fixed_t;
  swidth: integer;

// For inline color averaging
  r1, g1, b1: byte;
  r2, g2, b2: byte;
  fr2, fg2, fb2: integer;
  c1, c2, r, g, b: LongWord;

  cnt: integer;
begin
  count := dc_yh - dc_yl;

  if count < 0 then
    exit;

  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);

  fracstep := dc_iscale;
  frac := dc_texturemid + (dc_yl - centery) * fracstep;

  // Inner loop that does the actual texture mapping,
  //  e.g. a DDA-lile scaling.
  // This is as fast as it gets.
  swidth := SCREENWIDTH32PITCH - num_batch_columns * SizeOf(LongWord);

  fraclimit := frac + fracstep * count;
  while frac <= fraclimit do
  begin
    c2 := dc_colormap32[dc_source[(LongWord(frac) shr FRACBITS) and 127]];
    r2 := c2;
    g2 := c2 shr 8;
    b2 := c2 shr 16;
    fr2 := (FRACUNIT div 4 - 1) * r2;
    fg2 := (FRACUNIT div 4 - 1) * g2;
    fb2 := (FRACUNIT div 4 - 1) * b2;

    cnt := num_batch_columns;
    while cnt > 0 do
    begin
    // Color averaging
      c1 := destl^;
      r1 := c1;
      g1 := c1 shr 8;
      b1 := c1 shr 16;

      r := ((fr2) + (r1 * (3 * (FRACUNIT div 4)))) shr FRACBITS;
      g := ((fg2) + (g1 * (3 * (FRACUNIT div 4)))) shr FRACBITS;
      b := ((fb2) + (b1 * (3 * (FRACUNIT div 4)))) and $FF0000;

      destl^ := r + g shl 8 + b;
      inc(destl);
      dec(cnt);
    end;

    destl := PLongWord(integer(destl) + swidth);
    inc(frac, fracstep);
  end;
end;

procedure R_DrawFuzzColumn2Hi_Batch;
var
  count: integer;
  destl: PLongWord;
  frac: fixed_t;
  fracstep: fixed_t;
  fraclimit: fixed_t;
  swidth: integer;

// For inline color averaging
  r1, g1, b1: byte;
  r2, g2, b2: byte;
  fr2, fg2, fb2: integer;
  c1, c2, r, g, b: LongWord;

  cnt: integer;
begin
  count := dc_yh - dc_yl;

  if count < 0 then
    exit;

  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);

  fracstep := dc_iscale;
  frac := dc_texturemid + (dc_yl - centery) * fracstep;

  // Inner loop that does the actual texture mapping,
  //  e.g. a DDA-lile scaling.
  // This is as fast as it gets.
  swidth := SCREENWIDTH32PITCH - num_batch_columns * SizeOf(LongWord);

  fraclimit := frac + fracstep * count;
  while frac <= fraclimit do
  begin
    c2 := dc_colormap32[dc_source[(LongWord(frac) shr FRACBITS) and 127]];
    r2 := c2;
    g2 := c2 shr 8;
    b2 := c2 shr 16;
    fr2 := (3 * (FRACUNIT div 4)) * r2;
    fg2 := (3 * (FRACUNIT div 4)) * g2;
    fb2 := (3 * (FRACUNIT div 4))  * b2;

    cnt := num_batch_columns;
    while cnt > 0 do
    begin
    // Color averaging
      c1 := destl^;
      r1 := c1;
      g1 := c1 shr 8;
      b1 := c1 shr 16;

      r := ((fr2) + (r1 * (FRACUNIT div 4 - 1))) shr FRACBITS;
      g := ((fg2) + (g1 * (FRACUNIT div 4 - 1))) shr FRACBITS;
      b := ((fb2) + (b1 * (FRACUNIT div 4 - 1))) and $FF0000;

      destl^ := r + g shl 8 + b;
      inc(destl);
      dec(cnt);
    end;

    destl := PLongWord(integer(destl) + swidth);
    inc(frac, fracstep);
  end;
end;

procedure R_DrawFuzzColumnHiTL_Batch;
var
  count: integer;
  destl: PLongWord;
  frac: fixed_t;
  fracstep: fixed_t;
  fraclimit: fixed_t;
  swidth: integer;

// For inline color averaging
  r1, g1, b1: byte;
  r2, g2, b2: byte;
  fr2, fg2, fb2: integer;
  c1, c2, r, g, b: LongWord;

  cnt: integer;
begin
  count := dc_yh - dc_yl;

  if count < 0 then
    exit;

  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);

  fracstep := dc_iscale;
  frac := dc_texturemid + (dc_yl - centery) * fracstep;

  // Inner loop that does the actual texture mapping,
  //  e.g. a DDA-lile scaling.
  // This is as fast as it gets.
  swidth := SCREENWIDTH32PITCH - num_batch_columns * SizeOf(LongWord);

  fraclimit := frac + fracstep * count;
  while frac <= fraclimit do
  begin
    c2 := dc_colormap32[dc_translation[dc_source[(LongWord(frac) shr FRACBITS) and 127]]];
    r2 := c2;
    g2 := c2 shr 8;
    b2 := c2 shr 16;
    fr2 := (3 * (FRACUNIT div 4)) * r2;
    fg2 := (3 * (FRACUNIT div 4)) * g2;
    fb2 := (3 * (FRACUNIT div 4)) * b2;

    cnt := num_batch_columns;
    while cnt > 0 do
    begin
    // Color averaging
      c1 := destl^;
      r1 := c1;
      g1 := c1 shr 8;
      b1 := c1 shr 16;

      r := ((fr2) + (r1 * (FRACUNIT div 4 - 1))) shr FRACBITS;
      g := ((fg2) + (g1 * (FRACUNIT div 4 - 1))) shr FRACBITS;
      b := ((fb2) + (b1 * (FRACUNIT div 4 - 1))) and $FF0000;

      destl^ := r + g shl 8 + b;
      inc(destl);
      dec(cnt);
    end;

    destl := PLongWord(integer(destl) + swidth);
    inc(frac, fracstep);
  end;
end;
{$ENDIF}

procedure R_DrawWhiteLightColumnHi_Batch;
{$DEFINE WHITE}
{$I R_DrawLightColumnHi_Batch.inc}
{$UNDEF WHITE}

procedure R_DrawRedLightColumnHi_Batch;
{$DEFINE RED}
{$I R_DrawLightColumnHi_Batch.inc}
{$UNDEF RED}

procedure R_DrawGreenLightColumnHi_Batch;
{$DEFINE GREEN}
{$I R_DrawLightColumnHi_Batch.inc}
{$UNDEF GREEN}

procedure R_DrawBlueLightColumnHi_Batch;
{$DEFINE BLUE}
{$I R_DrawLightColumnHi_Batch.inc}
{$UNDEF BLUE}

procedure R_DrawYellowLightColumnHi_Batch;
{$DEFINE YELLOW}
{$I R_DrawLightColumnHi_Batch.inc}
{$UNDEF YELLOW}

procedure R_DrawColumnAddMedium_Batch;
var
  count: integer;
  dest: PByte;
  b: byte;
  u: integer;
  frac: fixed_t;
  fracstep: fixed_t;
  fraclimit: fixed_t;
  swidth: integer;
  cnt: integer;
begin
  count := dc_yh - dc_yl;

  // Zero length, column does not exceed a pixel.
  if count < 0 then
    exit;

  // Framebuffer destination address.
  // Use ylookup LUT to avoid multiply with ScreenWidth.
  // Use columnofs LUT for subwindows?
  dest := @((ylookup[dc_yl]^)[columnofs[dc_x]]);

  // Determine scaling,
  //  which is the only mapping to be done.
  fracstep := dc_iscale;
  frac := dc_texturemid + (dc_yl - centery) * fracstep;
  fraclimit := frac + count * fracstep;
  swidth := SCREENWIDTH - num_batch_columns;

  // Inner loop that does the actual texture mapping,
  //  e.g. a DDA-lile scaling.
  // This is as fast as it gets.
  while frac <= fraclimit do
  begin
  // Re-map color indices from wall texture column
  //  using a lighting/special effects LUT.

    b := dc_colormap[dc_source[(LongWord(frac) shr FRACBITS) and 127]];
    u := b shl 8;
    cnt := num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := curadd8table[dest^ + u];
      inc(dest);
      dec(cnt);
    end;


    inc(dest, swidth);
    inc(frac, fracstep);
  end;
end;

procedure R_DrawColumnAddHi_Batch;
var
  count: integer;
  destl: PLongWord;
  frac: fixed_t;
  fracstep: fixed_t;
  fraclimit: fixed_t;
  swidth: integer;

  addfactor: integer;

// For inline color averaging
  r1, g1, b1: byte;
  r2, g2, b2: byte;
  c1, c2, r, g, b: LongWord;

  cnt: integer;
begin
  count := dc_yh - dc_yl;

  if count < 0 then
    exit;

  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);

  fracstep := dc_iscale;
  frac := dc_texturemid + (dc_yl - centery) * fracstep;

  // Inner loop that does the actual texture mapping,
  //  e.g. a DDA-lile scaling.
  // This is as fast as it gets.
  swidth := SCREENWIDTH32PITCH - num_batch_columns * SizeOf(LongWord);

  addfactor := dc_alpha;

  fraclimit := frac + fracstep * count;
  while frac <= fraclimit do
  begin
    c2 := dc_colormap32[dc_source[(LongWord(frac) shr FRACBITS) and 127]];
    r2 := c2;
    g2 := c2 shr 8;
    b2 := c2 shr 16;
    if addfactor < FRACUNIT then
    begin
      r2 := (r2 * addfactor) shr FRACBITS;
      g2 := (g2 * addfactor) shr FRACBITS;
      b2 := (b2 * addfactor) shr FRACBITS;
    end;

    cnt := num_batch_columns;
    while cnt > 0 do
    begin
    // Color averaging
      c1 := destl^;
      r1 := c1;
      g1 := c1 shr 8;
      b1 := c1 shr 16;

      r := r2 + r1;
      if r > 255 then
        r := 255;
      g := g2 + g1;
      if g > 255 then
        g := 255;
      b := b2 + b1;
      if b > 255 then
        b := 255;

      destl^ := r + g shl 8 + b shl 16;
      inc(destl);
      dec(cnt);
    end;

    destl := PLongWord(integer(destl) + swidth);
    inc(frac, fracstep);
  end;
end;

procedure R_DrawColumnSubtractMedium_Batch;
var
  count: integer;
  dest: PByte;
  b: byte;
  u: integer;
  frac: fixed_t;
  fracstep: fixed_t;
  fraclimit: fixed_t;
  swidth: integer;
  cnt: integer;
begin
  count := dc_yh - dc_yl;

  // Zero length, column does not exceed a pixel.
  if count < 0 then
    exit;

  // Framebuffer destination address.
  // Use ylookup LUT to avoid multiply with ScreenWidth.
  // Use columnofs LUT for subwindows?
  dest := @((ylookup[dc_yl]^)[columnofs[dc_x]]);

  // Determine scaling,
  //  which is the only mapping to be done.
  fracstep := dc_iscale;
  frac := dc_texturemid + (dc_yl - centery) * fracstep;
  fraclimit := frac + count * fracstep;
  swidth := SCREENWIDTH - num_batch_columns;

  // Inner loop that does the actual texture mapping,
  //  e.g. a DDA-lile scaling.
  // This is as fast as it gets.
  while frac <= fraclimit do
  begin
  // Re-map color indices from wall texture column
  //  using a lighting/special effects LUT.

    b := dc_colormap[dc_source[(LongWord(frac) shr FRACBITS) and 127]];
    u := b shl 8;
    cnt := num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := cursubtract8table[dest^ + u];
      inc(dest);
      dec(cnt);
    end;


    inc(dest, swidth);
    inc(frac, fracstep);
  end;
end;

procedure R_DrawColumnSubtractHi_Batch;
var
  count: integer;
  destl: PLongWord;
  frac: fixed_t;
  fracstep: fixed_t;
  fraclimit: fixed_t;
  swidth: integer;

  subfactor: integer;

// For inline color averaging
  r1, g1, b1: byte;
  r2, g2, b2: byte;
  c1, c2, r, g, b: LongWord;

  cnt: integer;
begin
  count := dc_yh - dc_yl;

  if count < 0 then
    exit;

  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);

  fracstep := dc_iscale;
  frac := dc_texturemid + (dc_yl - centery) * fracstep;

  // Inner loop that does the actual texture mapping,
  //  e.g. a DDA-lile scaling.
  // This is as fast as it gets.
  swidth := SCREENWIDTH32PITCH - num_batch_columns * SizeOf(LongWord);

  subfactor := dc_alpha;

  fraclimit := frac + fracstep * count;
  while frac <= fraclimit do
  begin
    c2 := dc_colormap32[dc_source[(LongWord(frac) shr FRACBITS) and 127]];
    r2 := c2;
    g2 := c2 shr 8;
    b2 := c2 shr 16;
    if subfactor < FRACUNIT then
    begin
      r2 := (r2 * subfactor) shr FRACBITS;
      g2 := (g2 * subfactor) shr FRACBITS;
      b2 := (b2 * subfactor) shr FRACBITS;
    end;

    cnt := num_batch_columns;
    while cnt > 0 do
    begin
    // Color averaging
      c1 := destl^;
      r1 := c1;
      g1 := c1 shr 8;
      b1 := c1 shr 16;

      if r2 > r1 then
        r := 0
      else
        r := r1 - r2;
      if g2 > g1 then
        g := 0
      else
        g := g1 - g2;
      if b2 > b1 then
        b := 0
      else
        b := b1 - b2;

      destl^ := r + g shl 8 + b shl 16;
      inc(destl);
      dec(cnt);
    end;

    destl := PLongWord(integer(destl) + swidth);
    inc(frac, fracstep);
  end;
end;

end.

