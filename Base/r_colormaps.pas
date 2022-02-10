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
//  DESCRIPTION:
//   Custom colormaps for fake flat (BOOM) or whatever else.
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit r_colormaps;

interface

uses
  d_delphi,
  m_fixed,
  r_diher,
  r_main;

const
  C_START = 'C_START';
  C_END = 'C_END';

//==============================================================================
//
// R_InitCustomColormaps
//
//==============================================================================
procedure R_InitCustomColormaps;

//==============================================================================
//
// R_ShutDownCustomColormaps
//
//==============================================================================
procedure R_ShutDownCustomColormaps;

//==============================================================================
//
// R_CustomColorMapForName
//
//==============================================================================
function R_CustomColorMapForName(const s: string): integer;

//==============================================================================
//
// R_RecalcColormaps
//
//==============================================================================
procedure R_RecalcColormaps;

type
  customcolormap_t = record
    colormap: PByteArray;
    scalelight: scalelight_t;
    zlight: zlight_t;
    nummaps: integer;
    lump: integer;
    name: string[8];
    lastviewwidth: integer;
    {$IFDEF OPENGL}
    fog_r, fog_g, fog_b: float;
    fog_density: float;
    {$ELSE}
    dihertable: dihertable_t;
    {$ENDIF}
    basepal: array[0..255] of LongWord;
  end;
  Pcustomcolormap_t = ^customcolormap_t;

  customcolormap_tArray = array[0..$FF] of customcolormap_t;
  Pcustomcolormap_tArray = ^customcolormap_tArray;

var
  customcolormap: Pcustomcolormap_t = nil;

var
  numcustomcolormaps: integer = 0;
  customcolormaps: Pcustomcolormap_tArray = nil;

{$IFNDEF OPENGL}

//==============================================================================
//
// R_CustomColorMapColor32
//
//==============================================================================
function R_CustomColorMapColor32(const cm: Pcustomcolormap_t; const color: LongWord): LongWord;
{$ENDIF}

implementation

uses
  doomdef,
  r_data,
  r_defs,
  r_draw,
  v_data,
  w_wad,
  z_zone;

//==============================================================================
//
// R_InitCustomColormaps
//
//==============================================================================
procedure R_InitCustomColormaps;
var
  in_c: Boolean;
  i, j: integer;
  lumpname: string;
  lumps: TDNumberList;
  lumplen: integer;
  pal: PByteArray;
  cpal: array[0..255] of LongWord;
  src: PByteArray;
  dest: PLongWord;
begin
  in_c := false;
  lumps := TDNumberList.Create;
  for i := 0 to W_NumLumps - 1 do
  begin
    lumpname := strupper(char8tostring(W_GetNameForNum(i)));
    if lumpname = C_START then
    begin
      in_c := True;
      Continue;
    end;
    if lumpname = C_END then
    begin
      in_c := false;
      Continue;
    end;
    if in_c then
    begin
      for j := lumps.Count - 1 downto 0 do
        if strupper(char8tostring(W_GetNameForNum(lumps.Numbers[j]))) = lumpname then
          lumps.Delete(j);
      lumps.Add(i);
    end;
  end;

  numcustomcolormaps := lumps.Count;
  if numcustomcolormaps > 0 then
  begin
    R_InitNonUniformDiherFactor;

  // JVAL: Read default palette
    pal := V_ReadPalette(PU_STATIC);

    dest := @cpal[0];
    src := pal;
    while integer(src) < integer(@pal[256 * 3]) do
    begin
      dest^ := (LongWord(src[0]) shl 16) or
               (LongWord(src[1]) shl 8) or
               (LongWord(src[2]));
      inc(dest);
      src := PByteArray(integer(src) + 3);
    end;
    Z_ChangeTag(pal, PU_CACHE);

    customcolormaps := mallocz(numcustomcolormaps * SizeOf(customcolormap_t));
    for i := 0 to numcustomcolormaps - 1 do
    begin
      lumplen := W_LumpLength(lumps.Numbers[i]);
      customcolormaps[i].colormap := Z_Malloc(lumplen, PU_STATIC, nil);
      W_ReadLump(lumps.Numbers[i], customcolormaps[i].colormap);
      for j := 0 to lumplen - 1 do
        if customcolormaps[i].colormap[j] = 0 then
          customcolormaps[i].colormap[j] := aprox_black;
      customcolormaps[i].lump := lumps.Numbers[i];
      customcolormaps[i].name := strupper(char8tostring(W_GetNameForNum(lumps.Numbers[i])));
      customcolormaps[i].nummaps := lumplen div 256;
      if customcolormaps[i].nummaps < NUMCOLORMAPS then
      begin
        customcolormaps[i].colormap := Z_Realloc(customcolormaps[i].colormap, NUMCOLORMAPS * 256, PU_STATIC, nil);
        for j := customcolormaps[i].nummaps to NUMCOLORMAPS - 1 do
          memmove(@customcolormaps[i].colormap[j * 256],
                  @customcolormaps[i].colormap[(customcolormaps[i].nummaps - 1) * 256],
                   256);
        customcolormaps[i].nummaps := NUMCOLORMAPS;
      end;

      // JVAL: Create default colormap palette
      for j := 0 to 255 do
        customcolormaps[i].basepal[j] := cpal[customcolormaps[i].colormap[j]];
      {$IFDEF OPENGL}
      R_GetColormapFogColors(customcolormaps[i].fog_r,
                             customcolormaps[i].fog_g,
                             customcolormaps[i].fog_b,
                             customcolormaps[i].fog_density,
                             @customcolormaps[i].basepal);
      {$ELSE}
      R_CreateDiherTable(@customcolormaps[i].dihertable, @customcolormaps[i].basepal);
      {$ENDIF}

    end;
  end;
  lumps.Free;
end;

//==============================================================================
//
// R_ShutDownCustomColormaps
//
//==============================================================================
procedure R_ShutDownCustomColormaps;
begin
  if numcustomcolormaps > 0 then
  begin
    memfree(Pointer(customcolormaps), numcustomcolormaps * SizeOf(customcolormap_t));
    numcustomcolormaps := 0;
  end;
end;

//==============================================================================
//
// R_CustomColorMapForName
//
//==============================================================================
function R_CustomColorMapForName(const s: string): integer;
var
  i: integer;
  check: string;
begin
  check := strupper(s);
  for i := 0 to numcustomcolormaps - 1 do
    if customcolormaps[i].name = check then
    begin
      result := i;
      exit;
    end;

  result := -1;
end;

//==============================================================================
//
// R_RecalcColormaps
//
//==============================================================================
procedure R_RecalcColormaps;
var
  i, j: integer;
  startmap: integer;
  level: integer;
  scale: fixed_t;
begin
  if customcolormap = nil then
    Exit;

  if customcolormap.lastviewwidth = 0 then
  begin
    for i := 0 to LIGHTLEVELS - 1 do
    begin
      startmap := ((LIGHTLEVELS - 1 - i) * 2 * NUMCOLORMAPS) div LIGHTLEVELS;
      for j := 0 to MAXLIGHTZ - 1 do
      begin
        scale := FixedDiv(160 * FRACUNIT, _SHL(j + 1, LIGHTZSHIFT));
        scale := _SHR(scale, LIGHTSCALESHIFT);
        level := startmap - scale div DISTMAP;

        if level < 0 then
          level := 0
        else if level >= NUMCOLORMAPS then
          level := NUMCOLORMAPS - 1;

        customcolormap.zlight[i][j] := PByteArray(integer(customcolormap.colormap) + level * 256);
      end;
    end;
  end;

  if customcolormap.lastviewwidth <> viewwidth then
  begin
    customcolormap.lastviewwidth := viewwidth;
    // Calculate the light levels to use
    //  for each level / scale combination.
    for i := 0 to LIGHTLEVELS - 1 do
    begin
      startmap := ((LIGHTLEVELS - 1 - i) * 2) * NUMCOLORMAPS div LIGHTLEVELS;
      for j := 0 to MAXLIGHTSCALE - 1 do
      begin
        level := startmap - j * SCREENWIDTH div viewwidth div DISTMAP;

        if level < 0 then
          level := 0
        else
        begin
          if level >= NUMCOLORMAPS then
            level := NUMCOLORMAPS - 1;
        end;

        customcolormap.scalelight[i][j] := PByteArray(integer(customcolormap.colormap) + level * 256);
      end;
    end;
  end;

end;

{$IFNDEF OPENGL}

//==============================================================================
//
// R_CustomColorMapColor32
//
//==============================================================================
function R_CustomColorMapColor32(const cm: Pcustomcolormap_t; const color: LongWord): LongWord;
var
  i, j, k: byte;
begin
  i := (color shr 16) shr DIHERSHIFT;
  j := ((color shr 8) and $FF) shr DIHERSHIFT;
  k := (color and $FF) shr DIHERSHIFT;
  result := cm.dihertable[i, j, k];
end;
{$ENDIF}

end.
