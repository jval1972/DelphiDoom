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
// DESCRIPTION:
//  32 bit software rendering span cache (flats)
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit r_cache_flats;

interface

uses
  r_flatinfo;

const
// Flat cache
  FLAT32CACHESIZE = 2048;
  CACHEFLATMASK = FLAT32CACHESIZE - 1;

procedure R_ReadDS32Cache(const flat: integer);

type
  ds32_t = array[0..4096 * 4096 - 1] of LongWord;
  Pds32_t = ^ds32_t;

  ds32cacheinfo_t = record
    ds32: array[FS64x64..FS4096x4096] of Pds32_t;
    lump: integer;
    scale: dsscale_t;
    size: integer;
  end;
  Pds32cacheinfo_t = ^ds32cacheinfo_t;
  ds32cacheinfo_tArray = array[0..FLAT32CACHESIZE - 1] of ds32cacheinfo_t;
  ds32cacheinfo_tPArray = array[0..FLAT32CACHESIZE - 1] of Pds32cacheinfo_t;

function R_Get_ds32(p: Pds32cacheinfo_t): Pds32_t;

function R_FlatScaleFromSize(const flat, size: integer): dsscale_t;

procedure R_ClearDS32Cache;
procedure R_ResetDS32Cache;
procedure R_InitDS32Cache;
procedure R_ShutDownDS32Cache;

implementation

uses
  d_delphi,
  mt_utils,
  i_system,
  t_main,
  r_cache_main,
{$IFDEF DOOM_OR_STRIFE}
  r_colormaps,
  r_diher,
{$ENDIF}
  r_data,
  r_span,
  r_flat32,
  r_grow,
  r_span32,
  r_hires,
  r_mmx,
  v_video,
  w_wad,
  z_zone;

var
  ds32cache: ds32cacheinfo_tPArray;

procedure R_ReadDS32Cache(const flat: integer);
var
  cachemiss: boolean;
  hash: integer;
  t: PTexture;
  pds: Pds32cacheinfo_t;
  pds32: Pds32_t;
  plw: PLongWord;
  src1: PByte;
  i: integer;
  fsize: integer;
  r1, b1, g1: byte;
  r2, g2, b2: byte;
  r, g, b: LongWord;
  c: LongWord;
{$IFDEF DOOM_OR_STRIFE}
  dihertable: Pdihertable_t;
{$ENDIF}
  lump: integer;
  lumplen: integer;
  loops: integer;
  curgamma: PByteArray;
  pb: PByte;
  pbstop: PByte;
  tpal: PLongWordArray;
  numpixels: integer;
  flatname: string;
begin
  ds_source := nil;
  cachemiss := false;
  hash := flat and CACHEFLATMASK;
  if ds32cache[hash] = nil then
  begin
    ds32cache[hash] := mallocz(SizeOf(ds32cacheinfo_t));
    cachemiss := true;
  end;
  pds := ds32cache[hash];
  lump := R_GetLumpForFlat(flat);
  if cachemiss or (pds.lump <> lump) then
  begin
    // JVAL
    // 32 bit span cache miss
    // We render the flats buffer in a single thread (why not multiple threads here?)
    // This is just a tiny performance loss because:
    //  1) the column drawing threads are still working parallel
    //  2) we will render in single thread only a portion of the total flats of the frame
    if usemultithread then
      if pds.lump <> lump then
        R_RenderSingleThreadFlats32;

    inc(c_smiss);
    pds.lump := lump;
    t := flats[flats[flat].translation].flat32;
    if useexternaltextures and (t = nil) then
    begin
      flatname := W_GetNameForNum(lump);
      t := T_LoadHiResTexture(flatname);
      if t = nil then // JVAL: This allow to use Doomsday resource pack
        t := T_LoadHiResTexture('flat-' + flatname);
      if t = nil then
        flats[flats[flat].translation].flat32 := pointer($1) // Mark as missing
      else
      begin
        if flats[flats[flat].translation].size > 0 then
          fsize := dsscalesize[flats[flats[flat].translation].size].flatsize
        else
        begin
          if t.GetWidth <= 64 then
            fsize := 64
          else if t.GetWidth <= 128 then
            fsize := 128
          else if t.GetWidth <= 256 then
            fsize := 256
          else
            fsize := 512;
        end;
        t.ScaleTo(fsize, fsize);
        flats[flats[flat].translation].flat32 := t;
      end;
    end;

    pds.size := flats[flats[flat].translation].size;
    if useexternaltextures and (integer(t) > $1) then // if we have a hi resolution flat
    begin
      fsize := t.GetWidth;
      if pds.size > 0 then
        pds.scale := ds64x64
      else
      begin
        if fsize = 512 then
          pds.scale := ds512x512
        else if fsize = 256 then
          pds.scale := ds256x256
        else if fsize = 128 then
          pds.scale := ds128x128
        else
          pds.scale := ds64x64;
      end;
      numpixels := fsize * fsize;
      pds32 := R_Get_ds32(pds);
      curgamma := @gammatable[usegamma]; // To Adjust gamma

      if (t.GetBytesPerPixel = 1) {$IFDEF DOOM_OR_STRIFE} and (customcolormap = nil){$ENDIF} then
      begin
        r1 := pal_color;
        g1 := pal_color shr 8;
        b1 := pal_color shr 16;
        c := curgamma[r1] + curgamma[g1] shl 8 + curgamma[b1] shl 16;
        t.SetPalColor(c);
        plw := @pds32[0];
        tpal := PLongWordArray(t.GetTransformedPalette);
        pb := PByte(t.GetImage);
        pbstop := PByte(integer(pb) + numpixels);
        while integer(pb) < integer(pbstop) do
        begin
          plw^ := tpal[pb^];
          inc(plw);
          inc(pb);
        end;
      end
      else
      begin
      {$IFDEF DOOM_OR_STRIFE}
        if t.GetBytesPerPixel = 1 then
          t.ConvertTo32bit;
      {$ENDIF}

        MT_memcpy(pds32, t.GetImage, numpixels * SizeOf(LongWord), 2);

        // Simulate palette changes
        plw := @pds32[0];

      {$IFDEF DOOM_OR_STRIFE}
{        if customcolormap <> nil then
        begin
          for i := 0 to numpixels - 1 do
          begin
            plw^ := R_CustomColorMapColor32(customcolormap, plw^);
            inc(plw);
          end;
          dec(plw, numpixels);
        end;}
        if customcolormap <> nil then
        begin
          dihertable := @customcolormap.dihertable;
          for i := 0 to numpixels - 1 do
          begin
            c := plw^;
            b2 := (c shr 16) shr DIHERSHIFT;
            g2 := ((c shr 8) and $FF) shr DIHERSHIFT;
            r2 := (c and $FF) shr DIHERSHIFT;
            plw^ := dihertable[b2, g2, r2];
            inc(plw);
          end;
          dec(plw, numpixels);
        end;
      {$ENDIF}


        if dc_32bittexturepaletteeffects and (pal_color <> 0) then
        begin
          r1 := pal_color;
          g1 := pal_color shr 8;
          b1 := pal_color shr 16;
          loops := numpixels div 64;
          if usegamma > 0 then
          begin
            {$DEFINE USEGAMMA}
            {$UNDEF TRANSPARENCYNEEDED}
            for i := 0 to loops - 1 do
            begin
              {$I R_CachePaletteSimulation64.inc}
            end;
          end
          else
          begin
            if not R_BatchColorAdd32_MMX(plw, pal_color, numpixels) then
            begin
              {$UNDEF USEGAMMA}
              {$UNDEF TRANSPARENCYNEEDED}
              for i := 0 to loops - 1 do
              begin
                {$I R_CachePaletteSimulation64.inc}
              end;
            end;
          end
        end
        else
        begin
          if usegamma > 0 then
          begin
            pb := PByte(plw);
            loops := numpixels;
            while loops > 0 do
            begin
              pb^ := curgamma[pb^];
              inc(pb);
              pb^ := curgamma[pb^];
              inc(pb);
              pb^ := curgamma[pb^];
              inc(pb, 2);
              dec(loops);
            end;
          end;
        end;
      end;

    end
    else
    begin
      ds_source := W_CacheLumpNum(lump, PU_STATIC);
      lumplen := W_LumpLength(lump);
      pds.size := flats[flats[flat].translation].size;
      if pds.size = 0 then
        pds.scale := R_FlatScaleFromSize(flat, lumplen)
      else
        pds.scale := ds64x64;

      src1 := @ds_source[0];
      pds32 := R_Get_ds32(pds);
      plw := @pds32[0];
      if lumplen < $1000 then
        loops := lumplen div 64
      else
{$IFNDEF NO_INLINE_LOOPS}
        loops := dsscalesize[MaxI(Ord(pds.scale), pds.size)].memsize div 64;
      for i := 0 to loops - 1 do
      begin
        {$UNDEF LASTLOOP}
        {$I R_ReadDC32Cache_Loop1.inc}
        {$I R_ReadDC32Cache_Loop1.inc}
        {$I R_ReadDC32Cache_Loop1.inc}
        {$I R_ReadDC32Cache_Loop1.inc}
        {$I R_ReadDC32Cache_Loop1.inc}
        {$I R_ReadDC32Cache_Loop1.inc}
        {$I R_ReadDC32Cache_Loop1.inc}
        {$I R_ReadDC32Cache_Loop1.inc}
        {$I R_ReadDC32Cache_Loop1.inc}
        {$I R_ReadDC32Cache_Loop1.inc}
        {$I R_ReadDC32Cache_Loop1.inc}
        {$I R_ReadDC32Cache_Loop1.inc}
        {$I R_ReadDC32Cache_Loop1.inc}
        {$I R_ReadDC32Cache_Loop1.inc}
        {$I R_ReadDC32Cache_Loop1.inc}
        {$I R_ReadDC32Cache_Loop1.inc}
        {$I R_ReadDC32Cache_Loop1.inc}
        {$I R_ReadDC32Cache_Loop1.inc}
        {$I R_ReadDC32Cache_Loop1.inc}
        {$I R_ReadDC32Cache_Loop1.inc}
        {$I R_ReadDC32Cache_Loop1.inc}
        {$I R_ReadDC32Cache_Loop1.inc}
        {$I R_ReadDC32Cache_Loop1.inc}
        {$I R_ReadDC32Cache_Loop1.inc}
        {$I R_ReadDC32Cache_Loop1.inc}
        {$I R_ReadDC32Cache_Loop1.inc}
        {$I R_ReadDC32Cache_Loop1.inc}
        {$I R_ReadDC32Cache_Loop1.inc}
        {$I R_ReadDC32Cache_Loop1.inc}
        {$I R_ReadDC32Cache_Loop1.inc}
        {$I R_ReadDC32Cache_Loop1.inc}
        {$I R_ReadDC32Cache_Loop1.inc}
        {$I R_ReadDC32Cache_Loop1.inc}
        {$I R_ReadDC32Cache_Loop1.inc}
        {$I R_ReadDC32Cache_Loop1.inc}
        {$I R_ReadDC32Cache_Loop1.inc}
        {$I R_ReadDC32Cache_Loop1.inc}
        {$I R_ReadDC32Cache_Loop1.inc}
        {$I R_ReadDC32Cache_Loop1.inc}
        {$I R_ReadDC32Cache_Loop1.inc}
        {$I R_ReadDC32Cache_Loop1.inc}
        {$I R_ReadDC32Cache_Loop1.inc}
        {$I R_ReadDC32Cache_Loop1.inc}
        {$I R_ReadDC32Cache_Loop1.inc}
        {$I R_ReadDC32Cache_Loop1.inc}
        {$I R_ReadDC32Cache_Loop1.inc}
        {$I R_ReadDC32Cache_Loop1.inc}
        {$I R_ReadDC32Cache_Loop1.inc}
        {$I R_ReadDC32Cache_Loop1.inc}
        {$I R_ReadDC32Cache_Loop1.inc}
        {$I R_ReadDC32Cache_Loop1.inc}
        {$I R_ReadDC32Cache_Loop1.inc}
        {$I R_ReadDC32Cache_Loop1.inc}
        {$I R_ReadDC32Cache_Loop1.inc}
        {$I R_ReadDC32Cache_Loop1.inc}
        {$I R_ReadDC32Cache_Loop1.inc}
        {$I R_ReadDC32Cache_Loop1.inc}
        {$I R_ReadDC32Cache_Loop1.inc}
        {$I R_ReadDC32Cache_Loop1.inc}
        {$I R_ReadDC32Cache_Loop1.inc}
        {$I R_ReadDC32Cache_Loop1.inc}
        {$I R_ReadDC32Cache_Loop1.inc}
        {$I R_ReadDC32Cache_Loop1.inc}
        {$I R_ReadDC32Cache_Loop1.inc}
{$ELSE}
        loops := dsscalesize[MaxI(Ord(pds.scale), pds.size)].memsize;
      for i := 0 to loops - 1 do
      begin
        {$UNDEF LASTLOOP}
        {$I R_ReadDC32Cache_Loop1.inc}
{$ENDIF}
      end;
      Z_ChangeTag(ds_source, PU_CACHE);
    end;
    if (detailLevel >= DL_NORMAL) and (pds.scale = ds64x64) and (pds.size = FS64x64) then
    begin
      if extremeflatfiltering then
      begin
        if pds.size = 0 then
        begin
          if detailLevel = DL_ULTRARES then
            R_GrowSpan64to512(pds)
          else if detailLevel = DL_HIRES then
            R_GrowSpan64to256(pds)
          else
            R_GrowSpan64to128(pds);
        end;
      end
      else
      begin
        if pds.size = 0 then
        begin
          if detailLevel = DL_ULTRARES then
            R_GrowSpan64to256(pds)
          else if detailLevel = DL_HIRES then
            R_GrowSpan64to128(pds);
        end;
      end;
      pds32 := R_Get_ds32(pds);
    end;
  end
  else
    pds32 := R_Get_ds32(pds);
  ds_source32 := PLongWordArray(pds32);
  ds_scale := pds.scale;
  ds_size := pds.size;
  inc(c_stot);
end;

function R_Get_ds32(p: Pds32cacheinfo_t): Pds32_t;
begin
  result := p.ds32[MaxI(Ord(p.scale), p.size)];
  if result = nil then
  begin
    result := malloc(dsscalesize[MaxI(Ord(p.scale), p.size)].memsize * SizeOf(LongWord));
    p.ds32[MaxI(Ord(p.scale), p.size)] := result;
  end;
end;

procedure R_ResetDS32Cache;
var
  i: integer;
begin
  for i := 0 to FLAT32CACHESIZE - 1 do
    if ds32cache[i] <> nil then
      ds32cache[i].lump := -1;
end;

procedure R_ClearDS32Cache;
var
  i, j: integer;
begin
  for i := 0 to numflats - 1 do
  begin
    if LongWord(flats[i].flat32) > 1 then
      dispose(flats[i].flat32, destroy);
    flats[i].flat32 := nil;
  end;

  for i := 0 to FLAT32CACHESIZE - 1 do
    if ds32cache[i] <> nil then
    begin
      for j := FS64x64 to FS4096x4096 do
        if ds32cache[i].ds32[j] <> nil then
          memfree(pointer(ds32cache[i].ds32[j]), dsscalesize[j].memsize * SizeOf(LongWord));
      memfree(pointer(ds32cache[i]), SizeOf(ds32cacheinfo_t));
    end
end;

function R_FlatScaleFromSize(const flat, size: integer): dsscale_t;
var
  i: integer;
begin
  // JVAL
  // Determine hi-resolution flats inside wad
  // The lump size of a hi resolution flat must fit dsscalesize
  if flats[flats[flat].translation].size = 0 then
    for i := FS64x64 to FS4096x4096 do
      if size = dsscalesize[i].memsize then
      begin
        result := dsscale_t(i);
        exit;
      end;
  result := ds64x64;
end;

procedure R_InitDS32Cache;
var
  i: integer;
begin
  for i := 0 to FLAT32CACHESIZE - 1 do
    ds32cache[i] := nil;
end;

procedure R_ShutDownDS32Cache;
var
  i: integer;
begin
  R_ClearDS32Cache;
  for i := 0 to FLAT32CACHESIZE - 1 do
    if ds32cache[i] <> nil then
      memfree(pointer(ds32cache[i]), SizeOf(ds32cacheinfo_t));
end;

end.

