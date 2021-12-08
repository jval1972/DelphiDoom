//------------------------------------------------------------------------------
//
//  DelphiHexen: A modified and improved Hexen port for Windows
//  based on original Linux Doom as published by "id Software", on
//  Hexen source as published by "Raven" software and DelphiDoom
//  as published by Jim Valavanis.
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
//  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
//  02111-1307, USA.
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit r_cache_sky2;

interface

// Sky Cache, dc_mod <> 0
procedure R_ReadDC32InternalSkyCache2(const rtex, rcol: integer);

implementation

uses
  d_delphi,
  m_fixed,
  r_hires,
  r_cache_main,
  r_cache_walls,
  r_column,
  r_data,
  r_sky,
  v_video;

//
// R_ReadDC32InternalCache
//
// JVAL
//  Create dc_source32 from internal (IWAD) texture
//
procedure R_ReadDC32InternalSkyCache2(const rtex, rcol: integer);
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
  frac, fracstep: fixed_t;
{$IFDEF NO_INLINE_LOOPS}
  i: integer;
{$ENDIF}
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
    textures[rtex].factorbits := 0;
    if dc32cache[hash][index] = nil then
      dc32cache[hash][index] := mallocz(SizeOf(dc32cacheitem_t));

    inc(c_cmiss); // Cache miss
    {$IFDEF FPC}
    pdc32 := R_Get_dc32(dc32cache[hash][index], 256);
    plw := @pdc32[0];
    {$ELSE}
    plw := @R_Get_dc32(dc32cache[hash][index], 256)[0];
    {$ENDIF}
    textures[rtex].factorbits := 0;
    tbl := @hirestable[dc_mod];
    dc_source := R_GetColumn(rtex, rcol);
    src1 := @dc_source[0];
    dc_source2 := R_GetColumn(rtex, rcol + 1);
    src2 := @dc_source2[0];
    frac := 0;
    fracstep := FRACUNIT * 200 div 256 - 1;
    {$UNDEF LASTLOOP}
    {$UNDEF PRELASTLOOP}
{$IFDEF NO_INLINE_LOOPS}
    for i := 0 to 254 do
    begin
      {$I R_ReadDC32SkyCache_Loop2.inc}
    end;
{$ELSE}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$DEFINE PRELASTLOOP}
    {$I R_ReadDC32SkyCache_Loop2.inc}
    {$DEFINE LASTLOOP}
    {$I R_ReadDC32SkyCache_Loop2.inc}
{$ENDIF}
    plw^ := dc32cache[hash][index].dc32[254];
    dc32cache[hash][index].UID := UID;
  end;
  dc_texturefactorbits := 0;
  dc_columnsize := 256;
  dc_source32 := PLongWordArray(dc32cache[hash][index].dc32);
end;

end.

