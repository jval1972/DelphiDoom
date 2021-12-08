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

unit r_hires;

// Description
// Hi resolution support

interface

uses
  d_delphi,
  m_fixed;

type
  videomode_t = (
    vm8bit,
    vm32bit
  );

var
  detailLevel: integer;
  extremeflatfiltering: boolean;
  setdetail: integer = -1;
  videomode: videomode_t = vm8bit;
  allowlowdetails: boolean = true;
  allowhidetails: boolean = true;
  optimizedcolumnrendering: Boolean = true;
  usetransparentsprites: boolean;
  useexternaltextures: boolean;
  dc_32bittexturepaletteeffects: boolean;

const
  DL_LOWEST = 0;
  DL_LOW = 1;
  DL_MEDIUM = 2;
  DL_NORMAL = 3;
  DL_HIRES = 4;
  DL_ULTRARES = 5;
  DL_NUMRESOLUTIONS = 6;

const
  detailStrings: array[0..DL_NUMRESOLUTIONS - 1] of string = ('LOWEST', 'LOW', 'MEDIUM', 'NORMAL', 'HIGH', 'ULTRA');
  flatfilteringstrings: array[boolean] of string = ('NORMAL', 'EXTREME');

procedure R_CmdLowestRes(const parm1: string = '');
procedure R_CmdLowRes(const parm1: string = '');
procedure R_CmdMediumRes(const parm1: string = '');
procedure R_CmdNormalRes(const parm1: string = '');
procedure R_CmdHiRes(const parm1: string = '');
procedure R_CmdUltraRes(const parm1: string = '');
procedure R_CmdDetailLevel(const parm1: string = '');
procedure R_CmdExtremeflatfiltering(const parm1: string = '');
procedure R_CmdFullScreen(const parm1: string = '');
procedure R_Cmd32bittexturepaletteeffects(const parm1: string = '');
procedure R_CmdUseExternalTextures(const parm1: string = '');

function R_ColorAdd(const c1, c2: LongWord): LongWord; register;
function R_ColorSubtract(const c1, c2: LongWord): LongWord; register;

function R_ColorAverage(const c1, c2: LongWord; const factor: fixed_t): LongWord; register;
function R_ColorMean(const c1, c2: LongWord): LongWord; register;

function R_ColorLightAverage(const c1, c2: LongWord; const factor, lfactor: fixed_t): LongWord;
function R_InverseLightAverage(const c1, c2: LongWord; const factor: fixed_t): LongWord;

function R_ColorMidAverage(const c1, c2: LongWord): LongWord;

function R_ColorLight(const c: LongWord; const lfactor: fixed_t): LongWord;
function R_ColorBoost(const c: LongWord; const lfactor: fixed_t): LongWord;
function R_InverseLight(const c: LongWord): LongWord;
function R_FuzzLight(const c: LongWord): LongWord;

function R_ColorLightAdd(const c1, r, g, b: LongWord): LongWord; register;

{$IFNDEF OPENGL}
procedure R_CalcHiResTables_SingleThread;
procedure R_CalcHiResTables_MultiThread;
{$ENDIF}

const
  DC_HIRESBITS = 3;
  DC_HIRESFACTOR = 1 shl DC_HIRESBITS;

type
  hirestable_t = array[0..DC_HIRESFACTOR - 1, 0..255, 0..255] of LongWord;
  Phirestable_t = ^hirestable_t;
  hiresmodtable_t = array[0..255, 0..255] of LongWord;
  Phiresmodtable_t = ^hiresmodtable_t;

var
  hirestable: hirestable_t;
  recalctables32needed: boolean = true;

procedure R_InitHiRes;

procedure R_SetPalette(palette: integer);

var
  pal_color: LongWord;
{$IFNDEF OPENGL}
  cur_pal_index: integer;
{$ENDIF}

implementation

uses
  c_cmds,
  doomdef,
  m_misc,
{$IFDEF OPENGL}
  gl_main,
  gl_tex,
{$ELSE}
  mt_utils,
  i_system,
  i_video,
  i_displaymodes,
  v_video,
{$ENDIF}
  r_main,
{$IFNDEF OPENGL}
  r_cache_flats,
  r_cache_main,
  r_grow,
  r_defs,
  r_data,
  r_softlights,
{$ENDIF}
  r_lights;

////////////////////////////////////////////////////////////////////////////////
//
// Commands
//

//
// R_CmdLowestRes
//
procedure R_CmdLowestRes(const parm1: string = '');
var
  newres: boolean;
begin
  if parm1 = '' then
  begin
    printf('Current setting: lowestres = %s.'#13#10, [truefalseStrings[detailLevel = DL_LOWEST]]);
    exit;
  end;

  newres := C_BoolEval(parm1, detailLevel = DL_LOWEST);
  if newres <> (detailLevel = DL_LOWEST) then
  begin
    if newres then
      detailLevel := DL_LOWEST
    else
      detailLevel := DL_MEDIUM;
    R_SetViewSize;
  end;
  R_CmdLowestRes;
end;


//
// R_CmdLowRes
//
procedure R_CmdLowRes(const parm1: string = '');
var
  newres: boolean;
begin
  if parm1 = '' then
  begin
    printf('Current setting: lowres = %s.'#13#10, [truefalseStrings[detailLevel = DL_LOW]]);
    exit;
  end;

  newres := C_BoolEval(parm1, detailLevel = DL_LOW);
  if newres <> (detailLevel = DL_LOW) then
  begin
    if newres then
      detailLevel := DL_LOW
    else
      detailLevel := DL_MEDIUM;
    R_SetViewSize;
  end;
  R_CmdLowRes;
end;

//
// R_CmdMediumRes
//
procedure R_CmdMediumRes(const parm1: string = '');
var
  newres: boolean;
begin
  if parm1 = '' then
  begin
    printf('Current setting: mediumres = %s.'#13#10, [truefalseStrings[detailLevel = DL_MEDIUM]]);
    exit;
  end;

  newres := C_BoolEval(parm1, detailLevel = DL_MEDIUM);
  if newres <> (detailLevel = DL_MEDIUM) then
  begin
    if newres then
      detailLevel := DL_MEDIUM
    else
      detailLevel := DL_NORMAL;
    R_SetViewSize;
  end;
  R_CmdNormalRes;
end;

//
// R_CmdNormalRes
//
procedure R_CmdNormalRes(const parm1: string = '');
var
  newres: boolean;
begin
  if parm1 = '' then
  begin
    printf('Current setting: normalres = %s.'#13#10, [truefalseStrings[detailLevel = DL_NORMAL]]);
    exit;
  end;

  newres := C_BoolEval(parm1, detailLevel = DL_NORMAL);
  if newres <> (detailLevel = DL_NORMAL) then
  begin
    if newres then
      detailLevel := DL_NORMAL
    else
      detailLevel := DL_MEDIUM;
    R_SetViewSize;
  end;
  R_CmdNormalRes;
end;

//
// R_CmdHiRes
//
procedure R_CmdHiRes(const parm1: string = '');
var
  newres: boolean;
begin
  if parm1 = '' then
  begin
    printf('Current setting: hires = %s.'#13#10, [truefalseStrings[detailLevel = DL_HIRES]]);
    exit;
  end;

  newres := C_BoolEval(parm1, detailLevel = DL_HIRES);
  if newres <> (detailLevel = DL_HIRES) then
  begin
    if newres then
      detailLevel := DL_HIRES
    else
      detailLevel := DL_NORMAL;
    R_SetViewSize;
  end;
  R_CmdHiRes;
end;

//
// R_CmdUltraRes
//
procedure R_CmdUltraRes(const parm1: string = '');
var
  newres: boolean;
begin
  if parm1 = '' then
  begin
    printf('Current setting: ultrares = %s.'#13#10, [truefalseStrings[detailLevel = DL_ULTRARES]]);
    if detailLevel = DL_ULTRARES then
      printf('true.'#13#10)
    else
      printf('false.'#13#10);
    exit;
  end;

  newres := C_BoolEval(parm1, detailLevel = DL_ULTRARES);
  if newres <> (detailLevel = DL_ULTRARES) then
  begin
    if newres then
      detailLevel := DL_ULTRARES
    else
      detailLevel := DL_HIRES;
    R_SetViewSize;
  end;
  R_CmdUltraRes;
end;

//
// R_CmdDetailLevel
//
procedure R_CmdDetailLevel(const parm1: string = '');
var
  i, newdetail: integer;
  s_det: string;
begin
  if parm1 = '' then
  begin
    printf('Current setting: detailLevel = %s.'#13#10, [detailStrings[detailLevel]]);
    exit;
  end;

  s_det := strupper(parm1);
  newdetail := -1;
  for i := 0 to DL_NUMRESOLUTIONS - 1 do
    if s_det = detailStrings[i] then
    begin
      newdetail := i;
      break;
    end;

  if newdetail = -1 then
    newdetail := atoi(parm1, detailLevel);
  if newdetail <> detailLevel then
  begin
    detailLevel := newdetail;
    R_SetViewSize;
  end;

  R_CmdDetailLevel;
end;

procedure R_CmdFullScreen(const parm1: string = '');
{$IFDEF OPENGL}
var
  newfullscreen: boolean;
begin
  if parm1 = '' then
  begin
    printf('Current setting: fullscreen = ');
    if fullscreen then
      printf('true.'#13#10)
    else
      printf('false.'#13#10);
    exit;
  end;

  newfullscreen := C_BoolEval(parm1, fullscreen);

  if newfullscreen <> fullscreen then
    GL_ChangeFullScreen(newfullscreen);
  R_CmdFullScreen;
end;
{$ELSE}
var
  newfullscreen: integer;
  check: string;
begin
  if parm1 = '' then
  begin
    printf('Current setting: fullscreen = ');
    if fullscreen = FULLSCREEN_OFF then
      printf('false.'#13#10)
    else
      printf('true(' + decide(fullscreen = FULLSCREEN_SHARED, 'shared', 'exclusive') + ').'#13#10);
    exit;
  end;

  check := strupper(parm1);
  if (check = 'EXCLUSIVE') or (check = 'FULLSCREEN_EXCLUSIVE') or (check = '1') then
    newfullscreen := FULLSCREEN_EXCLUSIVE
  else if (check = 'SHARED') or (check = 'FULLSCREEN_SHARED') or (check = '0') or (check = 'YES') or (check = 'TRUE') then
    newfullscreen := FULLSCREEN_SHARED
  else
    newfullscreen := FULLSCREEN_OFF;

  if newfullscreen <> fullscreen then
    I_ChangeFullScreen(newfullscreen);
  R_CmdFullScreen;
end;
{$ENDIF}

procedure R_CmdExtremeflatfiltering(const parm1: string = '');
var
  newflatfiltering: boolean;
  parm: string;
begin
  if parm1 = '' then
  begin
    printf('Current setting: extremeflatfiltering = %s.'#13#10, [flatfilteringstrings[extremeflatfiltering]]);
    exit;
  end;

  parm := strupper(parm1);
  if parm = flatfilteringstrings[true] then
    newflatfiltering := true
  else if parm = flatfilteringstrings[false] then
    newflatfiltering := false
  else
    newflatfiltering := C_BoolEval(parm1, extremeflatfiltering);

  if extremeflatfiltering <> newflatfiltering then
  begin
    extremeflatfiltering := newflatfiltering;
    {$IFNDEF OPENGL}
    R_ResetDS32Cache;
    {$ENDIF}
  end;
  R_CmdExtremeflatfiltering;
end;

procedure R_Cmd32bittexturepaletteeffects(const parm1: string = '');
var
  new_32bittexturepaletteeffects: boolean;
begin
  if parm1 = '' then
  begin
    printf('Current setting: 32bittexturepaletteeffects = %s.'#13#10, [truefalseStrings[dc_32bittexturepaletteeffects]]);
    exit;
  end;

 new_32bittexturepaletteeffects := C_BoolEval(parm1, dc_32bittexturepaletteeffects);

  if dc_32bittexturepaletteeffects <> new_32bittexturepaletteeffects then
  begin
    dc_32bittexturepaletteeffects := new_32bittexturepaletteeffects;
    {$IFNDEF OPENGL}
    R_Reset32Cache;
    {$ENDIF}
  end;
  R_Cmd32bittexturepaletteeffects;
end;

procedure R_CmdUseExternalTextures(const parm1: string = '');
var
  new_useexternaltextures: boolean;
begin
  if parm1 = '' then
  begin
    printf('Current setting: useexternaltextures = %s.'#13#10, [truefalseStrings[useexternaltextures]]);
    exit;
  end;

  new_useexternaltextures := C_BoolEval(parm1, useexternaltextures);

  if useexternaltextures <> new_useexternaltextures then
  begin
    useexternaltextures := new_useexternaltextures;
    {$IFDEF OPENGL}
    gld_ClearTextureMemory;
    {$ELSE}
    R_Clear32Cache;
    {$ENDIF}
  end;
  R_CmdUseExternalTextures;
end;

////////////////////////////////////////////////////////////////////////////////

function R_ColorAdd(const c1, c2: LongWord): LongWord; register;
var
  r1, g1, b1: byte;
  r2, g2, b2: byte;
  r, g, b: LongWord;
begin
  r1 := c1;
  g1 := c1 shr 8;
  b1 := c1 shr 16;
  r2 := c2;
  g2 := c2 shr 8;
  b2 := c2 shr 16;

  r := r1 + r2;
  if r > 255 then
    r := 255;
  g := g1 + g2;
  if g > 255 then
    g := 255;
  b := b1 + b2;
  if b > 255 then
    b := 255;
  result := r + g shl 8 + b shl 16;
end;

function R_ColorSubtract(const c1, c2: LongWord): LongWord; register;
var
  r1, g1, b1: byte;
  r2, g2, b2: byte;
  r, g, b: LongWord;
begin
  r1 := c1;
  g1 := c1 shr 8;
  b1 := c1 shr 16;
  r2 := c2;
  g2 := c2 shr 8;
  b2 := c2 shr 16;

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
  result := r + g shl 8 + b shl 16;
end;

function R_ColorMean(const c1, c2: LongWord): LongWord; register;
var
  r1, g1, b1: byte;
  r2, g2, b2: byte;
  r, g, b: LongWord;
begin
  r1 := c1;
  g1 := c1 shr 8;
  b1 := c1 shr 16;
  r2 := c2;
  g2 := c2 shr 8;
  b2 := c2 shr 16;

  r := (r1 + r2) shr 1;
  g := (g1 + g2) shr 1;
  b := (b1 + b2) shr 1;
  result := r + g shl 8 + b shl 16;
end;

//
// R_ColorAverage
//
// Returns the average of 2 colors, c1 and c2 depending on factor
// If factor = 0 then returns c1
// If factor = FRACUNIT returns c2.
//
function R_ColorAverage(const c1, c2: LongWord; const factor: fixed_t): LongWord;
var
  r1, g1, b1: byte;
  r2, g2, b2: byte;
  r, g, b: LongWord;
  factor1: fixed_t;
begin
  r1 := c1;
  g1 := c1 shr 8;
  b1 := c1 shr 16;
  r2 := c2;
  g2 := c2 shr 8;
  b2 := c2 shr 16;

  factor1 := FRACUNIT - 1 - factor;
  r := ((r2 * factor) + (r1 * factor1)) shr FRACBITS;
  g := ((g2 * factor) + (g1 * factor1)) shr FRACBITS;
  b := ((b2 * factor) + (b1 * factor1)) and $FF0000;
  result := r + g shl 8 + b;
end;

function R_ColorLightAverage(const c1, c2: LongWord;
  const factor, lfactor: fixed_t): LongWord;
var
  r1, g1, b1: byte;
  r2, g2, b2: byte;
  r, g, b: LongWord;
  factor1: fixed_t;
  factor2: fixed_t;
begin
  r1 := c1;
  g1 := c1 shr 8;
  b1 := c1 shr 16;
  r2 := c2;
  g2 := c2 shr 8;
  b2 := c2 shr 16;

  factor1 := ((FRACUNIT - 1 - factor) * lfactor) shr FRACBITS;
  factor2 := (factor * lfactor) shr FRACBITS;

  r := ((r2 * factor2) + (r1 * factor1)) shr FRACBITS;
  g := ((g2 * factor2) + (g1 * factor1)) shr FRACBITS;
  b := ((b2 * factor2) + (b1 * factor1)) shr FRACBITS;
  result := r + g shl 8 + b shl 16;
end;

function R_InverseLightAverage(const c1, c2: LongWord;
  const factor: fixed_t): LongWord;
var
  r1, g1, b1: byte;
  r2, g2, b2: byte;
  r, g, b: LongWord;
  factor1: fixed_t;
begin
  r1 := c1;
  g1 := c1 shr 8;
  b1 := c1 shr 16;
  r2 := c2;
  g2 := c2 shr 8;
  b2 := c2 shr 16;

  factor1 := FRACUNIT - 1 - factor;

  r := ((r2 * factor) + (r1 * factor1));
  g := ((g2 * factor) + (g1 * factor1));
  b := ((b2 * factor) + (b1 * factor1));
  r := 255 - (r + g + b) div (FRACUNIT * 3);
  result := r + r shl 8 + r shl 16;
end;

function R_ColorMidAverage(const c1, c2: LongWord): LongWord;
{
assembler;
  asm
    movd mm0, [eax]
    pavgusb mm0, [edx]
//  paddusb mm0, [v2]
    movd  [eax], mm0
    femms
  end;
}
var
  r1, g1, b1: byte;
  r2, g2, b2: byte;
  r, g, b: LongWord;
begin
  r1 := c1;
  g1 := c1 shr 8;
  b1 := c1 shr 16;
  r2 := c2;
  g2 := c2 shr 8;
  b2 := c2 shr 16;
  r := (r1 + r2) shr 1;
  g := (g1 + g2) shr 1;
  b := (b1 + b2) shr 1;
  result := r + g shl 8 + b shl 16;
end;

//
// R_ColorLight
//
// Returns a dynamic color value of c depending on light factor lfactor.
// If lfactor is zero returns black.
// If lfactor is equal to FRACUNIT - 1 returns the input color c.
//
function R_ColorLight(const c: LongWord; const lfactor: fixed_t): LongWord;
var
  r1, g1, b1: byte;
  r, g, b: LongWord;
begin
  r1 := c;
  g1 := c shr 8;
  b1 := c shr 16;
  r := (r1 * lfactor) shr FRACBITS;
  g := (g1 * lfactor) shr FRACBITS;
  b := (b1 * lfactor) shr FRACBITS;
  result := r + g shl 8 + b shl 16;
end;

//
// R_ColorBoost
// Same as R_ColorLight but clip r, g, b values to allow lfactor greater than FRACUNIT
function R_ColorBoost(const c: LongWord; const lfactor: fixed_t): LongWord;
var
  r1, g1, b1: byte;
  r, g, b: LongWord;
begin
  r1 := c;
  g1 := c shr 8;
  b1 := c shr 16;
  r := (r1 * lfactor) shr FRACBITS;
  if r > 255 then
    r := 255;
  g := (g1 * lfactor) shr FRACBITS;
  if g > 255 then
    g := 255;
  b := (b1 * lfactor) shr FRACBITS;
  if b > 255 then
    b := 255;
  result := r + g shl 8 + b shl 16;
end;

function R_InverseLight(const c: LongWord): LongWord;
var
  r1, g1, b1: byte;
  c1: LongWord;
begin
  r1 := c;
  g1 := c shr 8;
  b1 := c shr 16;
  c1 := 255 - (r1 + g1 + b1) div 3;
  result := c1 + c1 shl 8 + c1 shl 16;
end;

function R_FuzzLight(const c: LongWord): LongWord;
var
  r1, g1, b1: byte;
  r, g, b: LongWord;
begin
  r1 := c;
  g1 := c shr 8;
  b1 := c shr 16;
  r := r1 shr 3 * 7;
  g := g1 shr 3 * 7;
  b := b1 shr 3 * 7;
  result := r + g shl 8 + b shl 16;
end;

function R_ColorLightAdd(const c1, r, g, b: LongWord): LongWord; register;
var
  r1, g1, b1: LongWord;
begin
  b1 := c1 and $ff;
  if b > 0 then
    b1 := b1 + ((255 - b1) * b) div 256;

  g1 := (c1 shr 8) and $ff;
  if g > 0 then
    g1 := g1 + ((255 - g1) * g) div 256;

  r1 := (c1 shr 16) and $ff;
  if r > 0 then
    r1 := r1 + ((255 - r1) * r) div 256;

  result := b1 + g1 shl 8 + r1 shl 16;
end;

{$IFNDEF OPENGL}
procedure R_CalcHiResTables_SingleThread;
var
  i, j, k: integer;
  vpli, vplj: PLongWord;
  hrtp: PLongWord;

  c1, c2: LongWord;
  r1, g1, b1,
  r2, g2, b2: byte;
  r, g, b: LongWord;
  rA, gA, bA: fixed_t;
  factor1, factor2: fixed_t;
  vplc: LongWord;
  cf2: array[0..255] of integer;
begin
  if not recalctables32needed then
    exit;

  if videomode = vm8bit then
    exit;

  R_Reset32Cache;

  for i := 0 to NUMCOLORMAPS * 256 - 1 do
  begin
    colormaps32[i] := curpal[colormaps[i]];
    fog_colormaps32[i] := curpal[fog_colormaps[i]]; // JVAL: Mars fog sectors
  end;

  hrtp := @hirestable[0, 0, 0];

  vpli := {$IFDEF DOOM_OR_STRIFE}@cvideopal[0]{$ELSE}@videopal[0]{$ENDIF};
  for i := 0 to 255 do
  begin
    vplc := vpli^;
    {$UNDEF LASTLOOP}
    for j := 0 to 3 do
    begin
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
    end;
    inc(vpli);
  end;

  for k := 1 to DC_HIRESFACTOR - 1 do
  begin
    vpli := {$IFDEF DOOM_OR_STRIFE}@cvideopal[0]{$ELSE}@videopal[0]{$ENDIF};
    factor2 := k shl (FRACBITS - DC_HIRESBITS);
    factor1 := (FRACUNIT - 1) - factor2;
    for i := 0 to 255 do
      cf2[i] := i * factor2;
    for i := 0 to 255 do
    begin
      c1 := vpli^;
      r1 := c1;
      g1 := c1 shr 8;
      b1 := c1 shr 16;
      rA := r1 * factor1;
      gA := g1 * factor1;
      bA := b1 * factor1;

      vplj := {$IFDEF DOOM_OR_STRIFE}@cvideopal[0]{$ELSE}@videopal[0]{$ENDIF};
      {$UNDEF LASTLOOP}
      for j := 0 to 3 do
      begin
        {$I R_CalcHiResTables_Loop2.inc}
        {$I R_CalcHiResTables_Loop2.inc}
        {$I R_CalcHiResTables_Loop2.inc}
        {$I R_CalcHiResTables_Loop2.inc}
        {$I R_CalcHiResTables_Loop2.inc}
        {$I R_CalcHiResTables_Loop2.inc}
        {$I R_CalcHiResTables_Loop2.inc}
        {$I R_CalcHiResTables_Loop2.inc}
        {$I R_CalcHiResTables_Loop2.inc}
        {$I R_CalcHiResTables_Loop2.inc}
        {$I R_CalcHiResTables_Loop2.inc}
        {$I R_CalcHiResTables_Loop2.inc}
        {$I R_CalcHiResTables_Loop2.inc}
        {$I R_CalcHiResTables_Loop2.inc}
        {$I R_CalcHiResTables_Loop2.inc}
        {$I R_CalcHiResTables_Loop2.inc}
        {$I R_CalcHiResTables_Loop2.inc}
        {$I R_CalcHiResTables_Loop2.inc}
        {$I R_CalcHiResTables_Loop2.inc}
        {$I R_CalcHiResTables_Loop2.inc}
        {$I R_CalcHiResTables_Loop2.inc}
        {$I R_CalcHiResTables_Loop2.inc}
        {$I R_CalcHiResTables_Loop2.inc}
        {$I R_CalcHiResTables_Loop2.inc}
        {$I R_CalcHiResTables_Loop2.inc}
        {$I R_CalcHiResTables_Loop2.inc}
        {$I R_CalcHiResTables_Loop2.inc}
        {$I R_CalcHiResTables_Loop2.inc}
        {$I R_CalcHiResTables_Loop2.inc}
        {$I R_CalcHiResTables_Loop2.inc}
        {$I R_CalcHiResTables_Loop2.inc}
        {$I R_CalcHiResTables_Loop2.inc}
        {$I R_CalcHiResTables_Loop2.inc}
        {$I R_CalcHiResTables_Loop2.inc}
        {$I R_CalcHiResTables_Loop2.inc}
        {$I R_CalcHiResTables_Loop2.inc}
        {$I R_CalcHiResTables_Loop2.inc}
        {$I R_CalcHiResTables_Loop2.inc}
        {$I R_CalcHiResTables_Loop2.inc}
        {$I R_CalcHiResTables_Loop2.inc}
        {$I R_CalcHiResTables_Loop2.inc}
        {$I R_CalcHiResTables_Loop2.inc}
        {$I R_CalcHiResTables_Loop2.inc}
        {$I R_CalcHiResTables_Loop2.inc}
        {$I R_CalcHiResTables_Loop2.inc}
        {$I R_CalcHiResTables_Loop2.inc}
        {$I R_CalcHiResTables_Loop2.inc}
        {$I R_CalcHiResTables_Loop2.inc}
        {$I R_CalcHiResTables_Loop2.inc}
        {$I R_CalcHiResTables_Loop2.inc}
        {$I R_CalcHiResTables_Loop2.inc}
        {$I R_CalcHiResTables_Loop2.inc}
        {$I R_CalcHiResTables_Loop2.inc}
        {$I R_CalcHiResTables_Loop2.inc}
        {$I R_CalcHiResTables_Loop2.inc}
        {$I R_CalcHiResTables_Loop2.inc}
        {$I R_CalcHiResTables_Loop2.inc}
        {$I R_CalcHiResTables_Loop2.inc}
        {$I R_CalcHiResTables_Loop2.inc}
        {$I R_CalcHiResTables_Loop2.inc}
        {$I R_CalcHiResTables_Loop2.inc}
        {$I R_CalcHiResTables_Loop2.inc}
        {$I R_CalcHiResTables_Loop2.inc}
        {$I R_CalcHiResTables_Loop2.inc}
      end;
      inc(vpli);
    end;
  end;

  recalctables32needed := false;
end;

function R_CalcHiResTables_thr1x8(foo: pointer): integer; stdcall;
var
  i, j: integer;
  vpli: PLongWord;
  hrtp: PLongWord;
  vplc: LongWord;
begin
  R_Reset32Cache;

  for i := 0 to NUMCOLORMAPS * 256 - 1 do
  begin
    colormaps32[i] := curpal[colormaps[i]];
    fog_colormaps32[i] := curpal[fog_colormaps[i]]; // JVAL: Mars fog sectors
  end;

  hrtp := @hirestable[0, 0, 0];

  vpli := {$IFDEF DOOM_OR_STRIFE}@cvideopal[0]{$ELSE}@videopal[0]{$ENDIF};
  for i := 0 to 255 do
  begin
    vplc := vpli^;
    {$UNDEF LASTLOOP}
    for j := 0 to 3 do
    begin
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
      {$I R_CalcHiResTables_Loop1.inc}
    end;
    inc(vpli);
  end;
  result := 0;
end;

function R_CalcHiResTables_thr2x8(foo: pointer): integer; stdcall;
var
  i, j, k: integer;
  vpli, vplj: PLongWord;
  hrtp: PLongWord;

  c1, c2: LongWord;
  r1, g1, b1,
  r2, g2, b2: byte;
  r, g, b: LongWord;
  rA, gA, bA: fixed_t;
  factor1, factor2: fixed_t;
  cf2: array[0..255] of integer;
begin
  k := PInteger(foo)^;
  hrtp := @hirestable[k, 0, 0];
  vpli := {$IFDEF DOOM_OR_STRIFE}@cvideopal[0]{$ELSE}@videopal[0]{$ENDIF};
  factor2 := k shl (FRACBITS - DC_HIRESBITS);
  factor1 := (FRACUNIT - 1) - factor2;
  for i := 0 to 255 do
    cf2[i] := i * factor2;
  for i := 0 to 255 do
  begin
    c1 := vpli^;
    r1 := c1;
    g1 := c1 shr 8;
    b1 := c1 shr 16;
    rA := r1 * factor1;
    gA := g1 * factor1;
    bA := b1 * factor1;

    vplj := {$IFDEF DOOM_OR_STRIFE}@cvideopal[0]{$ELSE}@videopal[0]{$ENDIF};
    {$UNDEF LASTLOOP}
    for j := 0 to 3 do
    begin
      {$I R_CalcHiResTables_Loop2.inc}
      {$I R_CalcHiResTables_Loop2.inc}
      {$I R_CalcHiResTables_Loop2.inc}
      {$I R_CalcHiResTables_Loop2.inc}
      {$I R_CalcHiResTables_Loop2.inc}
      {$I R_CalcHiResTables_Loop2.inc}
      {$I R_CalcHiResTables_Loop2.inc}
      {$I R_CalcHiResTables_Loop2.inc}
      {$I R_CalcHiResTables_Loop2.inc}
      {$I R_CalcHiResTables_Loop2.inc}
      {$I R_CalcHiResTables_Loop2.inc}
      {$I R_CalcHiResTables_Loop2.inc}
      {$I R_CalcHiResTables_Loop2.inc}
      {$I R_CalcHiResTables_Loop2.inc}
      {$I R_CalcHiResTables_Loop2.inc}
      {$I R_CalcHiResTables_Loop2.inc}
      {$I R_CalcHiResTables_Loop2.inc}
      {$I R_CalcHiResTables_Loop2.inc}
      {$I R_CalcHiResTables_Loop2.inc}
      {$I R_CalcHiResTables_Loop2.inc}
      {$I R_CalcHiResTables_Loop2.inc}
      {$I R_CalcHiResTables_Loop2.inc}
      {$I R_CalcHiResTables_Loop2.inc}
      {$I R_CalcHiResTables_Loop2.inc}
      {$I R_CalcHiResTables_Loop2.inc}
      {$I R_CalcHiResTables_Loop2.inc}
      {$I R_CalcHiResTables_Loop2.inc}
      {$I R_CalcHiResTables_Loop2.inc}
      {$I R_CalcHiResTables_Loop2.inc}
      {$I R_CalcHiResTables_Loop2.inc}
      {$I R_CalcHiResTables_Loop2.inc}
      {$I R_CalcHiResTables_Loop2.inc}
      {$I R_CalcHiResTables_Loop2.inc}
      {$I R_CalcHiResTables_Loop2.inc}
      {$I R_CalcHiResTables_Loop2.inc}
      {$I R_CalcHiResTables_Loop2.inc}
      {$I R_CalcHiResTables_Loop2.inc}
      {$I R_CalcHiResTables_Loop2.inc}
      {$I R_CalcHiResTables_Loop2.inc}
      {$I R_CalcHiResTables_Loop2.inc}
      {$I R_CalcHiResTables_Loop2.inc}
      {$I R_CalcHiResTables_Loop2.inc}
      {$I R_CalcHiResTables_Loop2.inc}
      {$I R_CalcHiResTables_Loop2.inc}
      {$I R_CalcHiResTables_Loop2.inc}
      {$I R_CalcHiResTables_Loop2.inc}
      {$I R_CalcHiResTables_Loop2.inc}
      {$I R_CalcHiResTables_Loop2.inc}
      {$I R_CalcHiResTables_Loop2.inc}
      {$I R_CalcHiResTables_Loop2.inc}
      {$I R_CalcHiResTables_Loop2.inc}
      {$I R_CalcHiResTables_Loop2.inc}
      {$I R_CalcHiResTables_Loop2.inc}
      {$I R_CalcHiResTables_Loop2.inc}
      {$I R_CalcHiResTables_Loop2.inc}
      {$I R_CalcHiResTables_Loop2.inc}
      {$I R_CalcHiResTables_Loop2.inc}
      {$I R_CalcHiResTables_Loop2.inc}
      {$I R_CalcHiResTables_Loop2.inc}
      {$I R_CalcHiResTables_Loop2.inc}
      {$I R_CalcHiResTables_Loop2.inc}
      {$I R_CalcHiResTables_Loop2.inc}
      {$I R_CalcHiResTables_Loop2.inc}
      {$I R_CalcHiResTables_Loop2.inc}
    end;
    inc(vpli);
  end;
  result := 0;
end;

function R_CalcHiResTables_thr1x4(foo: pointer): integer; stdcall;
var
  a: integer;
begin
  R_CalcHiResTables_thr1x8(nil);
  a := 1;
  R_CalcHiResTables_thr2x8(@a);
  result := 0;
end;

function R_CalcHiResTables_thr2x4(foo: pointer): integer; stdcall;
var
  a: integer;
begin
  a := 2;
  R_CalcHiResTables_thr2x8(@a);
  a := 3;
  R_CalcHiResTables_thr2x8(@a);
  result := 0;
end;

function R_CalcHiResTables_thr3x4(foo: pointer): integer; stdcall;
var
  a: integer;
begin
  a := 4;
  R_CalcHiResTables_thr2x8(@a);
  a := 5;
  R_CalcHiResTables_thr2x8(@a);
  result := 0;
end;

function R_CalcHiResTables_thr4x4(foo: pointer): integer; stdcall;
var
  a: integer;
begin
  a := 6;
  R_CalcHiResTables_thr2x8(@a);
  a := 7;
  R_CalcHiResTables_thr2x8(@a);
  result := 0;
end;

procedure R_CalcHiResTables_MultiThread;
var
  k: integer;
  A: array[1..DC_HIRESFACTOR - 1] of integer;
begin
  if not recalctables32needed then
    exit;

  if videomode = vm8bit then
    exit;

  if I_GetNumCPUs >= 8 then
  begin
    for k := 1 to DC_HIRESFACTOR - 1 do
      A[k] := k;
    MT_Execute8(
      @R_CalcHiResTables_thr1x8, nil,
      @R_CalcHiResTables_thr2x8, @A[1],
      @R_CalcHiResTables_thr2x8, @A[2],
      @R_CalcHiResTables_thr2x8, @A[3],
      @R_CalcHiResTables_thr2x8, @A[4],
      @R_CalcHiResTables_thr2x8, @A[5],
      @R_CalcHiResTables_thr2x8, @A[6],
      @R_CalcHiResTables_thr2x8, @A[7]
    );
  end
  else
    MT_Execute4(
      @R_CalcHiResTables_thr1x4, nil,
      @R_CalcHiResTables_thr2x4, nil,
      @R_CalcHiResTables_thr3x4, nil,
      @R_CalcHiResTables_thr4x4, nil
    );

  recalctables32needed := false;
end;
{$ENDIF}

procedure R_InitHiRes;
begin
  R_InitLightBoost;
{$IFNDEF OPENGL}
  R_InitLightTexture;
  R_InitSpanTables;
{$ENDIF}
end;

procedure R_SetPalette(palette: integer);
var
  r_extra_red: LongWord;
  r_extra_green: LongWord;
  r_extra_blue: LongWord;
begin
{$IFNDEF OPENGL}
  cur_pal_index := palette;
{$ENDIF}
  if palette > 0 then
  begin
    if palette <= 8 then
    begin
      r_extra_red := palette * 24;
      r_extra_green := 0;
      r_extra_blue := 0;
    end
    else if palette <= 12 then
    begin
      palette := palette - 8;
      r_extra_red := palette * 32;
      r_extra_green := palette * 25;
      r_extra_blue := palette * 8;
    end
    else {$IFDEF HEXEN} if palette = 13 then {$ENDIF}
    begin
      r_extra_red := 32;
      r_extra_green := 80;
      r_extra_blue := 0;
    end
    {$IFDEF HEXEN}
    else if palette < 21 then
    begin
      palette := palette - 13;
      r_extra_red := 0;
      r_extra_green := palette * 24;
      r_extra_blue := 0;
    end
    else if palette = 21 then
    begin
      r_extra_red := 0;
      r_extra_green := 0;
      r_extra_blue := 128;
    end
    else
    begin
      r_extra_red := 0;
      r_extra_green := 0;
      r_extra_blue := 0;
    end
    {$ENDIF}
    ;
    pal_color := r_extra_red shl 16 + r_extra_green shl 8 + r_extra_blue;
  end
  else
    pal_color := 0;
end;

end.

