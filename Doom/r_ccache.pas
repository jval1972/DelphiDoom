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
// DESCRIPTION:
//  32 bit software rendering column cache
//
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit r_ccache;

interface

uses
  d_delphi,
  w_wad;

const
  MAXBATCHWALLS = 8;

const
// Columns cache
  COL32CACHESIZE = $2000;
  CACHECOLSHIFT = 15;
  CACHETEXTMASK = 1 shl CACHECOLSHIFT - 1;
  CACHECOLBITS = 10;
  CACHECOLMASK = 1 shl CACHECOLBITS - 1;

procedure R_ReadDC32Cache(const rtex, rcol: integer);

procedure R_Precache32bittexture(const rtex: integer);

const
  MAXTEXTUREHEIGHT = 1024;
  MAXTEXTUREWIDTH = 1 shl CACHECOLBITS;
  MAXEQUALHASH = MAXBATCHWALLS * 2; // Allow MAXEQUALHASH same hash values to increase performance.

type
  dc32_t = array[0..MAXTEXTUREHEIGHT] of LongWord;
  Pdc32_t = ^dc32_t;

  dc32cacheitem_t = record
    dc32: Pdc32_t;
    columnsize: integer;
    UID: LongWord;
    tic: integer;
  end;
  Pdc32cacheitem_t = ^dc32cacheitem_t;

  dc32cacheinfo_t = array[0..MAXEQUALHASH - 1] of Pdc32cacheitem_t;
  Pdc32cacheinfo_t = ^dc32cacheinfo_t;

  dc32cacheinfo_tArray = array[0..COL32CACHESIZE - 1] of dc32cacheinfo_t;
  dc32cacheinfo_tPArray = array[0..COL32CACHESIZE - 1] of Pdc32cacheinfo_t;

procedure R_ClearDC32Cache;
procedure R_ResetDC32Cache;
procedure R_InitDC32Cache;
procedure R_ShutDownDC32Cache;

implementation

uses
  i_system,
  m_fixed,
  g_game,
  r_wall32,
  r_cache,
  r_colormaps,
  r_diher,
  r_defs,
  r_hires,
  r_column,
  r_sky,
  r_data,
  r_mmx,
  t_main,
  v_video,
  z_zone;

function R_GetHash(const tex, col, dmod: integer): integer;
// JVAL
// Get a hash value depending on tex, col and dc_mod.
// Although the followng hash is elementary, simple
// and it 's not the best gives good results,
// (about 98-99.9% hits correctly to cache (for standard resolution textures))
begin
  result := (97 * tex + col * 3833 + dmod * 7867) and (COL32CACHESIZE - 1);
end;

function R_GetUID(const tex, col, dmod: integer): LongWord;
// JVAL
// In addition the UID depending on tex, col and dc_mod
// gives 100% correct hit to cache (of course we can't afford
// a table of some billions elements for direct addressing :)
// bits  0-14 -> texture (larger than safe limit...)  (32768 textures max)
// bits 15-24 -> column (1024 columns max) (=MAXTEXTUREWIDTH)
// next bits -> dc_mod
// NOTE: In order to have a unique ID, DC_HIRESBITS can not be
// greater than 8, but considering that a value of DC_HIRESBITS of 8 gives
// all the range of 3 byte color (~16M different colors)
// it's meaningless to set DC_HIRESBITS in a higher value
// In fact a value of DC_HIRESBITS equal to 5 is far beyond the eye can see.
// ->  UID := tex + _SHL(col and 1023, 15) + _SHL(dc_mod, 25);
begin
  result := tex + _SHL(col, CACHECOLSHIFT) + _SHL(dmod, CACHECOLSHIFT + CACHECOLBITS);
end;

var
  dc32cache: dc32cacheinfo_tPArray;

function R_Get_dc32(p: Pdc32cacheitem_t; columnsize: integer): Pdc32_t;
begin
  if p.dc32 = nil then
  begin
    p.dc32 := malloc((columnsize + 1) * SizeOf(LongWord));
    p.columnsize := columnsize;
  end
  else if p.columnsize <> columnsize then
  begin
    realloc(pointer(p.dc32), (p.columnsize + 1) * SizeOf(LongWord), (columnsize + 1) * SizeOf(LongWord));
    p.columnsize := columnsize;
  end;
  result := p.dc32;
end;

//
// R_ReadDC32ExternalCache
//
// JVAL
//  Create dc_source32 from an external texture
//
function R_ReadDC32ExternalCache(const rtex, rcol: integer): boolean;
var
  plw: PLongWord;
  plw2: PLongWord;
{$IFDEF FPC}
  pdc32: Pdc32_t;
{$ENDIF}
  cachemiss: boolean;
  t: PTexture;
  col: integer;
  i: integer;
  dc32_a: dc32_t;
  cfrac2: fixed_t;
  r1, b1, g1: byte;
  r2, g2, b2: byte;
  r, g, b: LongWord;
  c: LongWord;
  dihertable: Pdihertable_t;
  twidth: integer;
  theight: integer;
  tfactor: integer;
  columnsize: integer;
  mod_c, mod_d: integer;
  loops: integer;
  UID: LongWord;
  hash: integer;
  curgamma: PByteArray;
  pb: PByte;
  index: integer;
  ptex: Ptexture_t;
begin
  if not useexternaltextures then
  begin
    result := false;
    exit;
  end;

  // Cache read of the caclulated dc_source32, 98-99% propability not to recalc...
  hash := R_GetHash(rtex, rcol, dc_texturemod);
  UID := R_GetUID(rtex, rcol, dc_texturemod);
  index := 0;
  cachemiss := true;
  if dc32cache[hash] <> nil then
  begin
    while dc32cache[hash][index] <> nil do
    begin
      if dc32cache[hash][index].UID = $FFFFFFFF then
        break;
      cachemiss := dc32cache[hash][index].UID <> UID;
      if not cachemiss then
        break;
      if index = MAXEQUALHASH - 1 then
        break;
      inc(index);
    end;
  end
  else
    dc32cache[hash] := mallocz(SizeOf(dc32cacheinfo_t));

  ptex := textures[rtex];
  if cachemiss then
  begin
    if usemultithread then
      if dc32cache[hash][index] <> nil then
        if dc32cache[hash][index].tic = gametic then
        begin
          R_RenderMultiThreadWalls32;
          R_WaitWallsCache32;
          R_ClearWallsCache32;
        end;

    inc(c_cmiss); // Cache miss
    t := ptex.texture32;

    if t = nil then
    begin
      t := T_LoadHiResTexture(ptex.name);
      if t = nil then
        ptex.texture32 := pointer($1) // Mark as missing
      else
      begin
        ptex.texture32 := t;
        // JVAL Adjust very big textures
        theight := t.GetHeight;
        tfactor := theight div ptex.height; // Scaling
        i := 0;
        while i < MAXTEXTUREFACTORBITS do
        begin
          if tfactor <= 1 shl i then
            break;
          inc(i);
        end;
        // JVAL Final adjustment of hi resolution textures
        twidth := (1 shl i) * ptex.width;
        theight := (1 shl i) * ptex.height;
        while (twidth > MAXTEXTUREHEIGHT) or (theight > MAXTEXTUREHEIGHT) do
        begin
          dec(i);
          twidth := (1 shl i) * ptex.width;
          theight := (1 shl i) * ptex.height;
        end;
        t.ScaleTo(twidth, theight); // JVAL Scale the texture if needed
        if rtex = skytexture then
          t.Mirror;
        ptex.factorbits := i;
      end;
    end;

    if LongWord(t) > $1 then // if we have a hi resolution texture
    begin
      // JVAL
      // Does not use [and (t.GetWidth - 1)] but [mod (t.GetWidth - 1)] because
      // we don't require textures to have width as power of 2.
      if rcol < 0 then
        col := abs(rcol - ptex.width) mod ptex.width
      else
        col := rcol mod ptex.width;
      if ptex.factorbits > 0 then
      begin
      // JVAL: Handle high resolution texture
        tfactor := 1 shl ptex.factorbits;
        columnsize := 128 * tfactor;
        mod_c := (dc_texturemod  * tfactor) shr DC_HIRESBITS;
        mod_d := dc_texturemod - mod_c * (1 shl (DC_HIRESBITS - ptex.factorbits));
        col := col * tfactor + mod_c;
        dc_texturemod := mod_d;
      end
      else
      begin
        dc_texturemod := dc_mod;
        columnsize := 128;
      end;

      if dc32cache[hash][index] = nil then
        dc32cache[hash][index] := mallocz(SizeOf(dc32cacheitem_t));
      {$IFDEF FPC}
      pdc32 := R_Get_dc32(dc32cache[hash][index], columnsize);
      plw := @pdc32[0];
      {$ELSE}
      plw := @R_Get_dc32(dc32cache[hash][index], columnsize)[0];
      {$ENDIF}

      curgamma := @gammatable[usegamma]; // To Adjust gamma

      c := 0;
      if (t.GetBytesPerPixel = 1) and (customcolormap = nil) then
      begin
        r1 := pal_color;
        g1 := pal_color shr 8;
        b1 := pal_color shr 16;
        c := curgamma[r1] + curgamma[g1] shl 8 + curgamma[b1] shl 16;
        t.GetPalettedColumn32(col, columnsize, plw, c);
      end
      else
        t.GetColumn32(col, columnsize, plw);

      // Texture filtering if dc_texturemod <> 0
      if dc_texturemod <> 0 then
      begin
        if (t.GetBytesPerPixel = 1) and (customcolormap = nil) then
          t.GetPalettedColumn32(col + 1, columnsize, @dc32_a, c)
        else
          t.GetColumn32(col + 1, columnsize, @dc32_a);
        plw2 := @dc32_a;
        cfrac2 := dc_texturemod shl (FRACBITS - DC_HIRESBITS);
        for i := 0 to columnsize - 1 do
        begin
          plw^ := R_ColorAverage(plw^, plw2^, cfrac2);
          inc(plw);
          inc(plw2);
        end;
      end;

      if (t.GetBytesPerPixel <> 1) or (customcolormap <> nil) then
      begin
        {$IFDEF FPC}
        pdc32 := R_Get_dc32(dc32cache[hash][index], columnsize);
        plw := @pdc32[0];
        {$ELSE}
        plw := @R_Get_dc32(dc32cache[hash][index], columnsize)[0];
        {$ENDIF}

{        if customcolormap <> nil then
        begin
          for i := 0 to columnsize - 1 do
          begin
            plw^ := R_CustomColorMapColor32(customcolormap, plw^);
            inc(plw);
          end;
          dec(plw, columnsize);
        end;}

        if customcolormap <> nil then
        begin
          dihertable := @customcolormap.dihertable;
          for i := 0 to columnsize - 1 do
          begin
            c := plw^;
            b2 := (c shr 16) shr DIHERSHIFT;
            g2 := ((c shr 8) and $FF) shr DIHERSHIFT;
            r2 := (c and $FF) shr DIHERSHIFT;
            plw^ := dihertable[b2, g2, r2];
            inc(plw);
          end;
          dec(plw, columnsize);
        end;

        // Simulate palette changes
        if dc_32bittexturepaletteeffects and (pal_color <> 0) then
        begin
          dc_palcolor := pal_color; // JVAL: needed for transparent textures.
          r1 := pal_color;
          g1 := pal_color shr 8;
          b1 := pal_color shr 16;
          loops := columnsize;
          if usegamma > 0 then
          begin
            {$DEFINE USEGAMMA}
            {$DEFINE TRANSPARENCYNEEDED}
            while loops >= 64 do
            begin
              {$I R_CachePaletteSimulation64.inc}
              loops := loops - 64;
            end;
            while loops >= 8 do
            begin
              {$I R_CachePaletteSimulation8.inc}
              loops := loops - 8;
            end;
            while loops >= 0 do
            begin
              {$I R_CachePaletteSimulation_Loop.inc}
              loops := loops - 1;
            end;
          end
          else
          begin
            if not R_BatchColorAdd32_MMX(plw, pal_color, columnsize) then
            begin
              {$UNDEF USEGAMMA}
              {$DEFINE TRANSPARENCYNEEDED}
              while loops >= 64 do
              begin
                {$I R_CachePaletteSimulation64.inc}
                loops := loops - 64;
              end;
              while loops >= 8 do
              begin
                {$I R_CachePaletteSimulation8.inc}
                loops := loops - 8;
              end;
              while loops >= 0 do
              begin
                {$I R_CachePaletteSimulation_Loop.inc}
                loops := loops - 1;
              end;
            end;
          end;
        end
        else
        begin
          dc_palcolor := 0;
          if usegamma > 0 then
          begin
            pb := PByte(plw);
            loops := columnsize;
            while loops > 0 do
            begin
              if PLongWord(pb)^ <> 0 then
              begin
                pb^ := curgamma[pb^];
                inc(pb);
                pb^ := curgamma[pb^];
                inc(pb);
                pb^ := curgamma[pb^];
                inc(pb, 2);
              end
              else
                inc(pb, 4);
              dec(loops);
            end;
          end;
        end;
      end;
    end
    else // We don't have hi resolution texture
    begin
      result := false;
      exit;
    end;

    if rtex = skytexture then
      dc32cache[hash][index].dc32[columnsize] := dc32cache[hash][index].dc32[columnsize - 1]
    else
      dc32cache[hash][index].dc32[columnsize] := dc32cache[hash][index].dc32[0];

    dc32cache[hash][index].UID := UID;
  end;
  dc_mod := dc_texturemod;
  dc_texturefactorbits := ptex.factorbits;
  dc32cache[hash][index].tic := gametic;
  dc_source32 := PLongWordArray(dc32cache[hash][index].dc32);
  result := true;
end;

//
// R_ReadDC32InternalCache
//
// JVAL
//  Create dc_source32 from internal (IWAD) texture
//
procedure R_ReadDC32InternalCache(const rtex, rcol: integer);
var
  plw: PLongWord;
{$IFDEF FPC}
  pdc32: Pdc32_t;
{$ENDIF}
  src1, src2: PByte;
  tbl: Phiresmodtable_t;
  cachemiss: boolean;
  UID: LongWord;
  hash: integer;
  index: integer;
  dc_source2: PByteArray;
begin
  // Cache read of the caclulated dc_source32, 98-99% propability not to recalc...
  hash := R_GetHash(rtex, rcol, dc_mod);
  UID := R_GetUID(rtex, rcol, dc_mod);
  index := 0;
  cachemiss := true;
  if dc32cache[hash] <> nil then
  begin
    while dc32cache[hash][index] <> nil do
    begin
      if dc32cache[hash][index].UID = $FFFFFFFF then
        break;
      cachemiss := dc32cache[hash][index].UID <> UID;
      if not cachemiss then
        break;
      if index = MAXEQUALHASH - 1 then
        break;
      inc(index);
    end;
  end
  else
    dc32cache[hash] := mallocz(SizeOf(dc32cacheinfo_t));

  if cachemiss then
  begin
    if dc32cache[hash][index] = nil then
      dc32cache[hash][index] := mallocz(SizeOf(dc32cacheitem_t))
    else
    begin
      if usemultithread then
        if dc32cache[hash][index].tic = gametic then
        begin
          R_RenderMultiThreadWalls32;
          R_WaitWallsCache32;
          R_ClearWallsCache32;
        end;
    end;

    inc(c_cmiss); // Cache miss
    {$IFDEF FPC}
    pdc32 := R_Get_dc32(dc32cache[hash][index], 128);
    plw := @pdc32[0];
    {$ELSE}
    plw := @R_Get_dc32(dc32cache[hash][index], 128)[0];
    {$ENDIF}
    textures[rtex].factorbits := 0;
    if dc_mod = 0 then
    begin
      dc_source := R_GetColumn(rtex, rcol);
      src1 := @dc_source[0];
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
      {$DEFINE LASTLOOP}
      {$I R_ReadDC32Cache_Loop1.inc}
    end
    else
    begin
      tbl := @hirestable[dc_mod];
      dc_source := R_GetColumn(rtex, rcol);
      src1 := @dc_source[0];
      dc_source2 := R_GetColumn(rtex, rcol + 1);
      src2 := @dc_source2[0];
      {$UNDEF LASTLOOP}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$I R_ReadDC32Cache_Loop2.inc}
      {$DEFINE LASTLOOP}
      {$I R_ReadDC32Cache_Loop2.inc}
    end;
    if rtex = skytexture then
      plw^ := dc32cache[hash][index].dc32[127]
    else
      plw^ := dc32cache[hash][index].dc32[0];
    dc32cache[hash][index].UID := UID;
  end;
  dc_texturefactorbits := 0;
  dc32cache[hash][index].tic := gametic;
  dc_source32 := PLongWordArray(dc32cache[hash][index].dc32);
end;

procedure R_ReadDC32Cache(const rtex, rcol: integer);
begin
  if not R_ReadDC32ExternalCache(rtex, rcol) then
    R_ReadDC32InternalCache(rtex, rcol);
  inc(c_ctot);
end;

procedure R_Precache32bittexture(const rtex: integer);
begin
  R_ReadDC32Cache(rtex, 0);
end;

procedure R_ResetDC32Cache;
var
  i, j: integer;
begin
  for i := 0 to COL32CACHESIZE - 1 do
    if dc32cache[i] <> nil then
      for j := 0 to MAXEQUALHASH - 1 do
        if dc32cache[i][j] <> nil then
          dc32cache[i][j].UID := $FFFFFFFF;
end;

procedure R_ClearDC32Cache;
var
  i, j: integer;
begin
  for i := 0 to numtextures - 1 do
  begin
    if LongWord(textures[i].texture32) > 1 then
      dispose(textures[i].texture32, destroy);
    textures[i].texture32 := nil;
  end;

  for i := 0 to COL32CACHESIZE - 1 do
    if dc32cache[i] <> nil then
    begin
      for j := 0 to MAXEQUALHASH - 1 do
        if dc32cache[i][j] <> nil then
        begin
          if dc32cache[i][j].dc32 <> nil then
            memfree(pointer(dc32cache[i][j].dc32), (dc32cache[i][j].columnsize + 1) * SizeOf(LongWord));
          memfree(pointer(dc32cache[i][j]), SizeOf(dc32cacheitem_t));
        end;
      memfree(pointer(dc32cache[i]), SizeOf(dc32cacheinfo_t));
    end;
end;

procedure R_InitDC32Cache;
var
  i: integer;
begin
  for i := 0 to COL32CACHESIZE - 1 do
    dc32cache[i] := nil;
end;

procedure R_ShutDownDC32Cache;
var
  i: integer;
begin
  R_ClearDC32Cache;
  for i := 0 to COL32CACHESIZE - 1 do
    if dc32cache[i] <> nil then
      memfree(pointer(dc32cache[i]), SizeOf(dc32cacheinfo_t));
end;

end.

