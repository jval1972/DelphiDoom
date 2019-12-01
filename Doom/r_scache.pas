//------------------------------------------------------------------------------
//
//  DelphiDoom: A modified and improved DOOM engine for Windows
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 2004-2008 by Jim Valavanis
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

unit r_scache;

interface

uses
  r_span;

const
// Flat cache
  FLAT32CACHESIZE = 256;
  CACHEFLATMASK = FLAT32CACHESIZE - 1;

procedure R_ReadDS32Cache(const flat: integer);

type
  ds32_t = array[0..512 * 512 - 1] of LongWord;
  Pds32_t = ^ds32_t;

  ds32cacheinfo_t = record
    ds32: array[0..Ord(NUMDSSCALES) - 1] of Pds32_t;
    lump: integer;
    scale: dsscale_t;
  end;
  Pds32cacheinfo_t = ^ds32cacheinfo_t;
  ds32cacheinfo_tArray = array[0..FLAT32CACHESIZE - 1] of ds32cacheinfo_t;
  ds32cacheinfo_tPArray = array[0..FLAT32CACHESIZE - 1] of Pds32cacheinfo_t;

function R_Get_ds32(p: Pds32cacheinfo_t): Pds32_t;

function R_FlatScaleFromSize(const size: integer): dsscale_t;

procedure R_ClearDS32Cache;
procedure R_ResetDS32Cache;
procedure R_InitDS32Cache;
procedure R_ShutDownDS32Cache;

implementation

uses
  d_delphi,
  t_main,
  r_cache, r_data, r_grow, r_span32, r_hires, r_mmx,
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
        if t.GetWidth <= 64 then
          fsize := 64
        else if t.GetWidth <= 128 then
          fsize := 128
        else if t.GetWidth <= 256 then
          fsize := 256
        else
          fsize := 512;
        t.ScaleTo(fsize, fsize);
        flats[flats[flat].translation].flat32 := t;
      end;
    end;

    if useexternaltextures and (integer(t) > $1) then // if we have a hi resolution flat
    begin
      fsize := t.GetWidth;
      if fsize = 512 then
        pds.scale := ds512x512
      else if fsize = 256 then
        pds.scale := ds256x256
      else if fsize = 128 then
        pds.scale := ds128x128
      else
        pds.scale := ds64x64;
      pds32 := R_Get_ds32(pds);
      numpixels := fsize * fsize;
      curgamma := @gammatable[usegamma]; // To Adjust gamma

      if t.GetBytesPerPixel = 1 then
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
        memcpy(pds32, t.GetImage, numpixels * SizeOf(LongWord));

        // Simutate palette changes
        plw := @pds32[0];

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
      pds.scale := R_FlatScaleFromSize(lumplen);

      src1 := @ds_source[0];
      pds32 := R_Get_ds32(pds);
      plw := @pds32[0];
      if lumplen < $1000 then
        loops := 0
      else
{$IFNDEF NO_INLINE_LOOPS}
        loops := dsscalesize[Ord(pds.scale)] div 64;
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
        loops := dsscalesize[Ord(pds.scale)];
      for i := 0 to loops - 1 do
      begin
        {$UNDEF LASTLOOP}
        {$I R_ReadDC32Cache_Loop1.inc}
{$ENDIF}
      end;
      Z_ChangeTag(ds_source, PU_CACHE);
    end;
    if (detailLevel >= DL_NORMAL) and (pds.scale = ds64x64) then
    begin
      if extremeflatfiltering then
      begin
        if detailLevel = DL_ULTRARES then
          R_GrowSpan64to512(pds)
        else if detailLevel = DL_HIRES then
          R_GrowSpan64to256(pds)
        else
          R_GrowSpan64to128(pds)
      end
      else
      begin
        if detailLevel = DL_ULTRARES then
          R_GrowSpan64to256(pds)
        else if detailLevel = DL_HIRES then
          R_GrowSpan64to128(pds)
      end;
      pds32 := R_Get_ds32(pds);
    end;
  end
  else
    pds32 := R_Get_ds32(pds);
  ds_source32 := PLongWordArray(pds32);
  ds_scale := pds.scale;
  inc(c_stot);
end;

function R_Get_ds32(p: Pds32cacheinfo_t): Pds32_t;
begin
  result := p.ds32[Ord(p.scale)];
  if result = nil then
  begin
    result := malloc(dsscalesize[Ord(p.scale)] * SizeOf(LongWord));
    p.ds32[Ord(p.scale)] := result;
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
      for j := 0 to Ord(NUMDSSCALES) - 1 do
        if ds32cache[i].ds32[j] <> nil then
          memfree(pointer(ds32cache[i].ds32[j]), dsscalesize[j] * SizeOf(LongWord));
      memfree(pointer(ds32cache[i]), SizeOf(ds32cacheinfo_t));
    end
end;

function R_FlatScaleFromSize(const size: integer): dsscale_t;
var
  i: integer;
begin
  result := ds64x64;
  // JVAL
  // Determine hi-resolution flats inside wad
  // The lump size of a hi resolution flat must fit dsscalesize
  for i := 1 to Ord(NUMDSSCALES) - 1 do
    if size = dsscalesize[i] then
    begin
      result := dsscale_t(i);
      exit;
    end;
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
