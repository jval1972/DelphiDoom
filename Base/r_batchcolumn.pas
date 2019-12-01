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
procedure R_DrawFuzzColumn_Batch;
procedure R_DrawFuzzColumn32_Batch;
procedure R_DrawFuzzColumnHi_Batch;
procedure R_DrawTranslatedColumn_Batch;
procedure R_DrawTranslatedColumnHi_Batch;
{$IFDEF DOOM}
procedure R_DrawColumnAverageMedium_Batch;
{$ENDIF}
procedure R_DrawColumnAverageHi_Batch;
{$IFDEF DOOM}
procedure R_DrawColumnAlphaMedium_Batch;
{$ENDIF}
procedure R_DrawColumnAlphaHi_Batch;
procedure R_DrawWhiteLightColumnHi_Batch;
procedure R_DrawRedLightColumnHi_Batch;
procedure R_DrawGreenLightColumnHi_Batch;
procedure R_DrawBlueLightColumnHi_Batch;
procedure R_DrawYellowLightColumnHi_Batch;

var
// JVAL: batch column drawing
  num_batch_columns: integer;
  optimizedthingsrendering: Boolean = true;

implementation

uses
  {$IFDEF HEXEN}
  xn_defs,
  {$ELSE}
  doomdef,
  {$ENDIF}
  doomtype,
  r_column,
  r_col_fz,
  r_precalc,
  {$IFDEF DOOM}
  r_trans8,
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
      Inc(dest);
      Dec(cnt);
    end;
    inc(dest, swidth);

    cnt := num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      Inc(dest);
      Dec(cnt);
    end;
    inc(dest, swidth);

    cnt := num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      Inc(dest);
      Dec(cnt);
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
      Inc(dest);
      Dec(cnt);
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
          Inc(dest, 4);
          Dec(cnt);
        end;
        inc(dest, swidth);
        inc(frac, fracstep);

        bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
        ldest := precal8_tolong[bdest];
        cnt := num_iters;
        while cnt > 0 do
        begin
          PLongWord(dest)^ := ldest;
          Inc(dest, 4);
          Dec(cnt);
        end;
        inc(dest, swidth);
        inc(frac, fracstep);

        bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
        ldest := precal8_tolong[bdest];
        cnt := num_iters;
        while cnt > 0 do
        begin
          PLongWord(dest)^ := ldest;
          Inc(dest, 4);
          Dec(cnt);
        end;
        inc(dest, swidth);
        inc(frac, fracstep);

        bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
        ldest := precal8_tolong[bdest];
        cnt := num_iters;
        while cnt > 0 do
        begin
          PLongWord(dest)^ := ldest;
          Inc(dest, 4);
          Dec(cnt);
        end;
        inc(dest, swidth);
        inc(frac, fracstep);

        bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
        ldest := precal8_tolong[bdest];
        cnt := num_iters;
        while cnt > 0 do
        begin
          PLongWord(dest)^ := ldest;
          Inc(dest, 4);
          Dec(cnt);
        end;
        inc(dest, swidth);
        inc(frac, fracstep);

        bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
        ldest := precal8_tolong[bdest];
        cnt := num_iters;
        while cnt > 0 do
        begin
          PLongWord(dest)^ := ldest;
          Inc(dest, 4);
          Dec(cnt);
        end;
        inc(dest, swidth);
        inc(frac, fracstep);

        bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
        ldest := precal8_tolong[bdest];
        cnt := num_iters;
        while cnt > 0 do
        begin
          PLongWord(dest)^ := ldest;
          Inc(dest, 4);
          Dec(cnt);
        end;
        inc(dest, swidth);
        inc(frac, fracstep);

        bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
        ldest := precal8_tolong[bdest];
        cnt := num_iters;
        while cnt > 0 do
        begin
          PLongWord(dest)^ := ldest;
          Inc(dest, 4);
          Dec(cnt);
        end;
        inc(dest, swidth);
        inc(frac, fracstep);

        bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
        ldest := precal8_tolong[bdest];
        cnt := num_iters;
        while cnt > 0 do
        begin
          PLongWord(dest)^ := ldest;
          Inc(dest, 4);
          Dec(cnt);
        end;
        inc(dest, swidth);
        inc(frac, fracstep);

        bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
        ldest := precal8_tolong[bdest];
        cnt := num_iters;
        while cnt > 0 do
        begin
          PLongWord(dest)^ := ldest;
          Inc(dest, 4);
          Dec(cnt);
        end;
        inc(dest, swidth);
        inc(frac, fracstep);

        bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
        ldest := precal8_tolong[bdest];
        cnt := num_iters;
        while cnt > 0 do
        begin
          PLongWord(dest)^ := ldest;
          Inc(dest, 4);
          Dec(cnt);
        end;
        inc(dest, swidth);
        inc(frac, fracstep);

        bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
        ldest := precal8_tolong[bdest];
        cnt := num_iters;
        while cnt > 0 do
        begin
          PLongWord(dest)^ := ldest;
          Inc(dest, 4);
          Dec(cnt);
        end;
        inc(dest, swidth);
        inc(frac, fracstep);

        bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
        ldest := precal8_tolong[bdest];
        cnt := num_iters;
        while cnt > 0 do
        begin
          PLongWord(dest)^ := ldest;
          Inc(dest, 4);
          Dec(cnt);
        end;
        inc(dest, swidth);
        inc(frac, fracstep);

        bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
        ldest := precal8_tolong[bdest];
        cnt := num_iters;
        while cnt > 0 do
        begin
          PLongWord(dest)^ := ldest;
          Inc(dest, 4);
          Dec(cnt);
        end;
        inc(dest, swidth);
        inc(frac, fracstep);

        bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
        ldest := precal8_tolong[bdest];
        cnt := num_iters;
        while cnt > 0 do
        begin
          PLongWord(dest)^ := ldest;
          Inc(dest, 4);
          Dec(cnt);
        end;
        inc(dest, swidth);
        inc(frac, fracstep);

        bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
        ldest := precal8_tolong[bdest];
        cnt := num_iters;
        while cnt > 0 do
        begin
          PLongWord(dest)^ := ldest;
          Inc(dest, 4);
          Dec(cnt);
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
          Inc(dest, 4);
          Dec(cnt);
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
        Inc(dest, 4);
        Dec(cnt);
      end;
      cnt := rest_batch_columns;
      while cnt > 0 do
      begin
        dest^ := bdest;
        Inc(dest);
        Dec(cnt);
      end;
      inc(dest, swidth);
      inc(frac, fracstep);

      bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
      ldest := precal8_tolong[bdest];
      cnt := num_iters;
      while cnt > 0 do
      begin
        PLongWord(dest)^ := ldest;
        Inc(dest, 4);
        Dec(cnt);
      end;
      cnt := rest_batch_columns;
      while cnt > 0 do
      begin
        dest^ := bdest;
        Inc(dest);
        Dec(cnt);
      end;
      inc(dest, swidth);
      inc(frac, fracstep);

      bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
      ldest := precal8_tolong[bdest];
      cnt := num_iters;
      while cnt > 0 do
      begin
        PLongWord(dest)^ := ldest;
        Inc(dest, 4);
        Dec(cnt);
      end;
      cnt := rest_batch_columns;
      while cnt > 0 do
      begin
        dest^ := bdest;
        Inc(dest);
        Dec(cnt);
      end;
      inc(dest, swidth);
      inc(frac, fracstep);

      bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
      ldest := precal8_tolong[bdest];
      cnt := num_iters;
      while cnt > 0 do
      begin
        PLongWord(dest)^ := ldest;
        Inc(dest, 4);
        Dec(cnt);
      end;
      cnt := rest_batch_columns;
      while cnt > 0 do
      begin
        dest^ := bdest;
        Inc(dest);
        Dec(cnt);
      end;
      inc(dest, swidth);
      inc(frac, fracstep);

      bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
      ldest := precal8_tolong[bdest];
      cnt := num_iters;
      while cnt > 0 do
      begin
        PLongWord(dest)^ := ldest;
        Inc(dest, 4);
        Dec(cnt);
      end;
      cnt := rest_batch_columns;
      while cnt > 0 do
      begin
        dest^ := bdest;
        Inc(dest);
        Dec(cnt);
      end;
      inc(dest, swidth);
      inc(frac, fracstep);

      bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
      ldest := precal8_tolong[bdest];
      cnt := num_iters;
      while cnt > 0 do
      begin
        PLongWord(dest)^ := ldest;
        Inc(dest, 4);
        Dec(cnt);
      end;
      cnt := rest_batch_columns;
      while cnt > 0 do
      begin
        dest^ := bdest;
        Inc(dest);
        Dec(cnt);
      end;
      inc(dest, swidth);
      inc(frac, fracstep);

      bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
      ldest := precal8_tolong[bdest];
      cnt := num_iters;
      while cnt > 0 do
      begin
        PLongWord(dest)^ := ldest;
        Inc(dest, 4);
        Dec(cnt);
      end;
      cnt := rest_batch_columns;
      while cnt > 0 do
      begin
        dest^ := bdest;
        Inc(dest);
        Dec(cnt);
      end;
      inc(dest, swidth);
      inc(frac, fracstep);

      bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
      ldest := precal8_tolong[bdest];
      cnt := num_iters;
      while cnt > 0 do
      begin
        PLongWord(dest)^ := ldest;
        Inc(dest, 4);
        Dec(cnt);
      end;
      cnt := rest_batch_columns;
      while cnt > 0 do
      begin
        dest^ := bdest;
        Inc(dest);
        Dec(cnt);
      end;
      inc(dest, swidth);
      inc(frac, fracstep);

      bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
      ldest := precal8_tolong[bdest];
      cnt := num_iters;
      while cnt > 0 do
      begin
        PLongWord(dest)^ := ldest;
        Inc(dest, 4);
        Dec(cnt);
      end;
      cnt := rest_batch_columns;
      while cnt > 0 do
      begin
        dest^ := bdest;
        Inc(dest);
        Dec(cnt);
      end;
      inc(dest, swidth);
      inc(frac, fracstep);

      bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
      ldest := precal8_tolong[bdest];
      cnt := num_iters;
      while cnt > 0 do
      begin
        PLongWord(dest)^ := ldest;
        Inc(dest, 4);
        Dec(cnt);
      end;
      cnt := rest_batch_columns;
      while cnt > 0 do
      begin
        dest^ := bdest;
        Inc(dest);
        Dec(cnt);
      end;
      inc(dest, swidth);
      inc(frac, fracstep);

      bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
      ldest := precal8_tolong[bdest];
      cnt := num_iters;
      while cnt > 0 do
      begin
        PLongWord(dest)^ := ldest;
        Inc(dest, 4);
        Dec(cnt);
      end;
      cnt := rest_batch_columns;
      while cnt > 0 do
      begin
        dest^ := bdest;
        Inc(dest);
        Dec(cnt);
      end;
      inc(dest, swidth);
      inc(frac, fracstep);

      bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
      ldest := precal8_tolong[bdest];
      cnt := num_iters;
      while cnt > 0 do
      begin
        PLongWord(dest)^ := ldest;
        Inc(dest, 4);
        Dec(cnt);
      end;
      cnt := rest_batch_columns;
      while cnt > 0 do
      begin
        dest^ := bdest;
        Inc(dest);
        Dec(cnt);
      end;
      inc(dest, swidth);
      inc(frac, fracstep);

      bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
      ldest := precal8_tolong[bdest];
      cnt := num_iters;
      while cnt > 0 do
      begin
        PLongWord(dest)^ := ldest;
        Inc(dest, 4);
        Dec(cnt);
      end;
      cnt := rest_batch_columns;
      while cnt > 0 do
      begin
        dest^ := bdest;
        Inc(dest);
        Dec(cnt);
      end;
      inc(dest, swidth);
      inc(frac, fracstep);

      bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
      ldest := precal8_tolong[bdest];
      cnt := num_iters;
      while cnt > 0 do
      begin
        PLongWord(dest)^ := ldest;
        Inc(dest, 4);
        Dec(cnt);
      end;
      cnt := rest_batch_columns;
      while cnt > 0 do
      begin
        dest^ := bdest;
        Inc(dest);
        Dec(cnt);
      end;
      inc(dest, swidth);
      inc(frac, fracstep);

      bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
      ldest := precal8_tolong[bdest];
      cnt := num_iters;
      while cnt > 0 do
      begin
        PLongWord(dest)^ := ldest;
        Inc(dest, 4);
        Dec(cnt);
      end;
      cnt := rest_batch_columns;
      while cnt > 0 do
      begin
        dest^ := bdest;
        Inc(dest);
        Dec(cnt);
      end;
      inc(dest, swidth);
      inc(frac, fracstep);

      bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
      ldest := precal8_tolong[bdest];
      cnt := num_iters;
      while cnt > 0 do
      begin
        PLongWord(dest)^ := ldest;
        Inc(dest, 4);
        Dec(cnt);
      end;
      cnt := rest_batch_columns;
      while cnt > 0 do
      begin
        dest^ := bdest;
        Inc(dest);
        Dec(cnt);
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
        Inc(dest, 4);
        Dec(cnt);
      end;
      cnt := rest_batch_columns;
      while cnt > 0 do
      begin
        dest^ := bdest;
        Inc(dest);
        Dec(cnt);
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
      Inc(dest);
      Dec(cnt);
    end;
    inc(dest, swidth);
    inc(frac, fracstep);

    bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    cnt := num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      Inc(dest);
      Dec(cnt);
    end;
    inc(dest, swidth);
    inc(frac, fracstep);

    bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    cnt := num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      Inc(dest);
      Dec(cnt);
    end;
    inc(dest, swidth);
    inc(frac, fracstep);

    bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    cnt := num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      Inc(dest);
      Dec(cnt);
    end;
    inc(dest, swidth);
    inc(frac, fracstep);

    bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    cnt := num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      Inc(dest);
      Dec(cnt);
    end;
    inc(dest, swidth);
    inc(frac, fracstep);

    bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    cnt := num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      Inc(dest);
      Dec(cnt);
    end;
    inc(dest, swidth);
    inc(frac, fracstep);

    bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    cnt := num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      Inc(dest);
      Dec(cnt);
    end;
    inc(dest, swidth);
    inc(frac, fracstep);

    bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    cnt := num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      Inc(dest);
      Dec(cnt);
    end;
    inc(dest, swidth);
    inc(frac, fracstep);

    bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    cnt := num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      Inc(dest);
      Dec(cnt);
    end;
    inc(dest, swidth);
    inc(frac, fracstep);

    bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    cnt := num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      Inc(dest);
      Dec(cnt);
    end;
    inc(dest, swidth);
    inc(frac, fracstep);

    bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    cnt := num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      Inc(dest);
      Dec(cnt);
    end;
    inc(dest, swidth);
    inc(frac, fracstep);

    bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    cnt := num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      Inc(dest);
      Dec(cnt);
    end;
    inc(dest, swidth);
    inc(frac, fracstep);

    bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    cnt := num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      Inc(dest);
      Dec(cnt);
    end;
    inc(dest, swidth);
    inc(frac, fracstep);

    bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    cnt := num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      Inc(dest);
      Dec(cnt);
    end;
    inc(dest, swidth);
    inc(frac, fracstep);

    bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    cnt := num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      Inc(dest);
      Dec(cnt);
    end;
    inc(dest, swidth);
    inc(frac, fracstep);

    bdest := dc_colormap[dc_local[(LongWord(frac) shr FRACBITS) and 127]];
    cnt := num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      Inc(dest);
      Dec(cnt);
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
      Inc(dest);
      Dec(cnt);
    end;
    inc(dest, swidth);
    inc(frac, fracstep);
  end;
end;

procedure R_DrawColumnHi_Batch;
var
  count: integer;
  destl: PLongWord;
  ldest: LongWord;
  c: LongWord;
  frac: fixed_t;
  fracstep: fixed_t;
  spot: integer;
  cnt: integer;
  swidth: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
begin
  count := dc_yh - dc_yl;

  if count < 0 then
    exit;

  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);

  swidth := SCREENWIDTH32PITCH - num_batch_columns * SizeOf(LongWord);
  fracstep := dc_iscale;
  frac := dc_texturemid + (dc_yl - centery) * fracstep;
  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b);
    while count >= 0 do
    begin
      spot := (LongWord(frac) shr FRACBITS) and 127;
      c := curpal[dc_source[spot]];
      ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];

      cnt := num_batch_columns;
      while cnt > 0 do
      begin
        destl^ := ldest;
        Inc(destl);
        Dec(cnt);
      end;

      destl := PLongWord(integer(destl) + swidth);
      inc(frac, fracstep);
      Dec(count);
    end;
  end
  else
  begin
    while count >= 0 do
    begin
      spot := (LongWord(frac) shr FRACBITS) and 127;
      c := curpal[dc_source[spot]];
      r1 := c;
      g1 := c shr 8;
      b1 := c shr 16;
      ldest := precal32_ic[r1 + g1 + b1];

      cnt := num_batch_columns;
      while cnt > 0 do
      begin
        destl^ := ldest;
        Inc(destl);
        Dec(cnt);
      end;

      destl := PLongWord(integer(destl) + swidth);
      inc(frac, fracstep);
      Dec(count);
    end;
  end;
end;

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
      Dec(cnt);
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
      Dec(cnt);
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
      Dec(cnt);
    end;

    destl := @destl[swidth];

  end;
end;


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
    bdest := dc_colormap[dc_translation[dc_source[LongWord(frac) shr FRACBITS]]];

    cnt := num_batch_columns;
    while cnt > 0 do
    begin
      dest^ := bdest;
      Inc(dest);
      Dec(cnt);
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
    ldest := dc_colormap32[dc_translation[dc_source[LongWord(frac) shr FRACBITS]]];
    cnt := num_batch_columns;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      Dec(cnt);
    end;

    destl := PLongWord(integer(destl) + swidth);

    inc(frac, fracstep);
  end;
end;

////////////////////////////////////////////////////////////////////////////////

{$IFDEF DOOM}
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
      Inc(dest);
      Dec(cnt);
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
      Inc(destl);
      Dec(cnt);
    end;

    destl := PLongWord(integer(destl) + swidth);
    inc(frac, fracstep);
    dec(count);
  end;
end;

{$IFDEF DOOM}
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
      Inc(dest);
      Dec(cnt);
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
      Inc(destl);
      Dec(cnt);
    end;

    destl := PLongWord(integer(destl) + swidth);
    inc(frac, fracstep);
  end;
end;

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

end.

