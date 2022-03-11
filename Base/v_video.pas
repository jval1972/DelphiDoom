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
//  Gamma correction LUT.
//  Functions to draw patches (by post) directly to screen.
//  Functions to blit a block to the screen.
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit v_video;

interface

uses
  d_delphi,
  doomdef,
  m_fixed,
// Needed because we are refering to patches.
  r_defs,
  v_data;

//==============================================================================
//
// V_GetScreenWidth
//
//==============================================================================
function V_GetScreenWidth(scrn: integer): integer;

//==============================================================================
//
// V_GetScreenHeight
//
//==============================================================================
function V_GetScreenHeight(scrn: integer): integer;

//==============================================================================
//
// V_SetPalette
//
//==============================================================================
procedure V_SetPalette(const palette: PByteArray);

{$IFNDEF OPENGL}
{$IFDEF DOOM_OR_STRIFE}

//==============================================================================
//
// V_CalcColorMapPalette
//
//==============================================================================
procedure V_CalcColorMapPalette;
{$ENDIF}
{$ENDIF}

//==============================================================================
// V_Init
//
// Allocates buffer screens, call before R_Init.
//
//==============================================================================
procedure V_Init;

//==============================================================================
//
// V_ReInit
//
//==============================================================================
procedure V_ReInit;

//==============================================================================
//
// V_ShutDown
//
//==============================================================================
procedure V_ShutDown;

//==============================================================================
//
// V_ScreensSize
//
//==============================================================================
function V_ScreensSize(const scrn: integer = -1): integer;

//==============================================================================
//
// V_CopyCustomScreen
//
//==============================================================================
procedure V_CopyCustomScreen(
  src: PByteArray;
  width: integer;
  height: integer;
  destscrn: integer);

//==============================================================================
//
// V_CopyRect
//
//==============================================================================
procedure V_CopyRect(
  srcx: integer;
  srcy: integer;
  srcscrn: integer;
  width: integer;
  height: integer;
  destx: integer;
  desty: integer;
  destscrn: integer;
  preserve: boolean);

//==============================================================================
//
// V_CopyAddRect
//
//==============================================================================
procedure V_CopyAddRect(
  srcx: integer;
  srcy: integer;
  srcscrn: integer;
  width: integer;
  height: integer;
  destx: integer;
  desty: integer;
  destscrn: integer;
  preserve: boolean;
  addfactor: fixed_t);

//==============================================================================
//
// V_CopyRectTransparent
//
//==============================================================================
procedure V_CopyRectTransparent(
  srcx: integer;
  srcy: integer;
  srcscrn: integer;
  width: integer;
  height: integer;
  destx: integer;
  desty: integer;
  destscrn: integer;
  preserve: boolean);

//==============================================================================
//
// V_CopyScreenTransparent
//
//==============================================================================
procedure V_CopyScreenTransparent(
  srcscrn: integer;
  destscrn: integer; srcoffs: integer = 0; destoffs: integer = 0; size: integer = -1);

{$IFDEF OPENGL}

//==============================================================================
//
// V_ShadeBackground
//
//==============================================================================
procedure V_ShadeBackground(const ofs: integer = 0;
  const count: integer = -1);
{$ELSE}

//==============================================================================
//
// V_ShadeScreen
//
//==============================================================================
procedure V_ShadeScreen(const scn: integer; const ofs: integer = 0;
  const count: integer = -1);
{$ENDIF}

//==============================================================================
//
// V_CopyRawDataToScreen
//
//==============================================================================
procedure V_CopyRawDataToScreen(scrn: integer; const lumpname: string);

//==============================================================================
//
// V_RemoveTransparency
//
//==============================================================================
procedure V_RemoveTransparency(const scn: integer; const ofs: integer;
  const count: integer = -1);

//==============================================================================
//
// V_DrawPatch
//
//==============================================================================
procedure V_DrawPatch(x, y: integer; scrn: integer; patch: Ppatch_t; preserve: boolean); overload;

//==============================================================================
//
// V_DrawPatch
//
//==============================================================================
procedure V_DrawPatch(x, y: integer; scrn: integer; const patchname: string; preserve: boolean); overload;

//==============================================================================
//
// V_DrawPatch
//
//==============================================================================
procedure V_DrawPatch(x, y: integer; scrn: integer; const lump: integer; preserve: boolean); overload;

//==============================================================================
//
// V_DrawPatchTransparent
//
//==============================================================================
procedure V_DrawPatchTransparent(x, y: integer; scrn: integer; patch: Ppatch_t; preserve: boolean);

{$IFNDEF OPENGL}

//==============================================================================
//
// V_DrawPatchTransparentMT
//
//==============================================================================
procedure V_DrawPatchTransparentMT(x, y: integer; scrn: integer; patch: Ppatch_t; preserve: boolean);
{$ENDIF}

//==============================================================================
//
// V_DrawPatchZoomed
//
//==============================================================================
procedure V_DrawPatchZoomed(x, y: integer; scrn: integer; patch: Ppatch_t; preserve: boolean; fraczoom: fixed_t);

//==============================================================================
//
// V_DrawPatchFlipped
//
//==============================================================================
procedure V_DrawPatchFlipped(x, y: integer; scrn: integer; patch: Ppatch_t);

//==============================================================================
//
// V_DrawPatchFullScreenTMP320x200
//
//==============================================================================
procedure V_DrawPatchFullScreenTMP320x200(p: Ppatch_t); overload;

//==============================================================================
//
// V_DrawPatchFullScreenTMP320x200
//
//==============================================================================
procedure V_DrawPatchFullScreenTMP320x200(const pname: string); overload;

//==============================================================================
//
// V_DrawPatchFullScreenTMP320x200
//
//==============================================================================
procedure V_DrawPatchFullScreenTMP320x200(const lump: integer); overload;

//==============================================================================
//
// V_DrawPatchFullScreenTMP426x200
//
//==============================================================================
procedure V_DrawPatchFullScreenTMP426x200(p: Ppatch_t); overload;

//==============================================================================
//
// V_DrawPatchFullScreenTMP426x200
//
//==============================================================================
procedure V_DrawPatchFullScreenTMP426x200(const pname: string); overload;

//==============================================================================
//
// V_DrawPatchFullScreenTMP426x200
//
//==============================================================================
procedure V_DrawPatchFullScreenTMP426x200(const lump: integer); overload;

//==============================================================================
//
// V_PageDrawer
//
//==============================================================================
procedure V_PageDrawer(const pagename: string);

//==============================================================================
// V_DrawBlock
//
// Draw a linear block of pixels into the view buffer.
//
//==============================================================================
procedure V_DrawBlock(x, y: integer; scrn: integer; width, height: integer; src: PByteArray);

//==============================================================================
//
// V_DrawLongBlock
//
//==============================================================================
procedure V_DrawLongBlock(x, y: integer; width, height: integer; src: PLongWordArray);

//==============================================================================
// V_GetBlock
//
// Reads a linear block of pixels into the view buffer.
//
//==============================================================================
procedure V_GetBlock(x, y: integer; scrn: integer; width, height: integer; dest: PByteArray);

//==============================================================================
//
// V_PreserveX
//
//==============================================================================
function V_PreserveX(const x: integer): integer; overload;

//==============================================================================
//
// V_PreserveX
//
//==============================================================================
function V_PreserveX(const SCN: integer; const x: integer): integer; overload;

//==============================================================================
//
// V_PreserveY
//
//==============================================================================
function V_PreserveY(const y: integer): integer;

{$IFDEF OPENGL}

//==============================================================================
//
// V_PreserveGLX
//
//==============================================================================
function V_PreserveGLX(const x: integer): integer;

//==============================================================================
//
// V_PreserveGLY
//
//==============================================================================
function V_PreserveGLY(const y: integer): integer;
{$ENDIF}

//==============================================================================
//
// V_PreserveW
//
//==============================================================================
function V_PreserveW(const x: integer; const w: integer): integer; overload;

//==============================================================================
//
// V_PreserveW
//
//==============================================================================
function V_PreserveW(const SCN: integer; const x: integer; const w: integer): integer; overload;

//==============================================================================
//
// V_PreserveH
//
//==============================================================================
function V_PreserveH(const y: integer; const h: integer): integer;

//==============================================================================
//
// V_NeedsPreserve
//
//==============================================================================
function V_NeedsPreserve(const destscrn, srcscrn: integer): boolean; overload;

//==============================================================================
//
// V_NeedsPreserve
//
//==============================================================================
function V_NeedsPreserve(const destscrn, srcscrn: integer; preserve: boolean): boolean; overload;

//==============================================================================
//
// V_CalcPreserveTables
//
//==============================================================================
procedure V_CalcPreserveTables;

//==============================================================================
//
// V_TileScreen8
//
//==============================================================================
function V_TileScreen8(const lumpname: string; const dstscn: integer): boolean;

const
  GAMMASIZE = 5;

// Now where did these came from?
  gammatable: array[0..GAMMASIZE - 1, 0..255] of byte = (
    (  1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,  15,  16,
      17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,  32,
      33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,  46,  47,  48,
      49,  50,  51,  52,  53,  54,  55,  56,  57,  58,  59,  60,  61,  62,  63,  64,
      65,  66,  67,  68,  69,  70,  71,  72,  73,  74,  75,  76,  77,  78,  79,  80,
      81,  82,  83,  84,  85,  86,  87,  88,  89,  90,  91,  92,  93,  94,  95,  96,
      97,  98,  99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112,
     113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128,
     128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143,
     144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159,
     160, 161, 162, 163, 164, 165, 166, 167, 168, 169, 170, 171, 172, 173, 174, 175,
     176, 177, 178, 179, 180, 181, 182, 183, 184, 185, 186, 187, 188, 189, 190, 191,
     192, 193, 194, 195, 196, 197, 198, 199, 200, 201, 202, 203, 204, 205, 206, 207,
     208, 209, 210, 211, 212, 213, 214, 215, 216, 217, 218, 219, 220, 221, 222, 223,
     224, 225, 226, 227, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237, 238, 239,
     240, 241, 242, 243, 244, 245, 246, 247, 248, 249, 250, 251, 252, 253, 254, 255),

    (  2,   4,   5,   7,   8,  10,  11,  12,  14,  15,  16,  18,  19,  20,  21,  23,
      24,  25,  26,  27,  29,  30,  31,  32,  33,  34,  36,  37,  38,  39,  40,  41,
      42,  44,  45,  46,  47,  48,  49,  50,  51,  52,  54,  55,  56,  57,  58,  59,
      60,  61,  62,  63,  64,  65,  66,  67,  69,  70,  71,  72,  73,  74,  75,  76,
      77,  78,  79,  80,  81,  82,  83,  84,  85,  86,  87,  88,  89,  90,  91,  92,
      93,  94,  95,  96,  97,  98,  99, 100, 101, 102, 103, 104, 105, 106, 107, 108,
     109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124,
     125, 126, 127, 128, 129, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139,
     140, 141, 142, 143, 144, 145, 146, 147, 148, 148, 149, 150, 151, 152, 153, 154,
     155, 156, 157, 158, 159, 160, 161, 162, 163, 163, 164, 165, 166, 167, 168, 169,
     170, 171, 172, 173, 174, 175, 175, 176, 177, 178, 179, 180, 181, 182, 183, 184,
     185, 186, 186, 187, 188, 189, 190, 191, 192, 193, 194, 195, 196, 196, 197, 198,
     199, 200, 201, 202, 203, 204, 205, 205, 206, 207, 208, 209, 210, 211, 212, 213,
     214, 214, 215, 216, 217, 218, 219, 220, 221, 222, 222, 223, 224, 225, 226, 227,
     228, 229, 230, 230, 231, 232, 233, 234, 235, 236, 237, 237, 238, 239, 240, 241,
     242, 243, 244, 245, 245, 246, 247, 248, 249, 250, 251, 252, 252, 253, 254, 255),

    (  4,   7,   9,  11,  13,  15,  17,  19,  21,  22,  24,  26,  27,  29,  30,  32,
      33,  35,  36,  38,  39,  40,  42,  43,  45,  46,  47,  48,  50,  51,  52,  54,
      55,  56,  57,  59,  60,  61,  62,  63,  65,  66,  67,  68,  69,  70,  72,  73,
      74,  75,  76,  77,  78,  79,  80,  82,  83,  84,  85,  86,  87,  88,  89,  90,
      91,  92,  93,  94,  95,  96,  97,  98, 100, 101, 102, 103, 104, 105, 106, 107,
     108, 109, 110, 111, 112, 113, 114, 114, 115, 116, 117, 118, 119, 120, 121, 122,
     123, 124, 125, 126, 127, 128, 129, 130, 131, 132, 133, 133, 134, 135, 136, 137,
     138, 139, 140, 141, 142, 143, 144, 144, 145, 146, 147, 148, 149, 150, 151, 152,
     153, 153, 154, 155, 156, 157, 158, 159, 160, 160, 161, 162, 163, 164, 165, 166,
     166, 167, 168, 169, 170, 171, 172, 172, 173, 174, 175, 176, 177, 178, 178, 179,
     180, 181, 182, 183, 183, 184, 185, 186, 187, 188, 188, 189, 190, 191, 192, 193,
     193, 194, 195, 196, 197, 197, 198, 199, 200, 201, 201, 202, 203, 204, 205, 206,
     206, 207, 208, 209, 210, 210, 211, 212, 213, 213, 214, 215, 216, 217, 217, 218,
     219, 220, 221, 221, 222, 223, 224, 224, 225, 226, 227, 228, 228, 229, 230, 231,
     231, 232, 233, 234, 235, 235, 236, 237, 238, 238, 239, 240, 241, 241, 242, 243,
     244, 244, 245, 246, 247, 247, 248, 249, 250, 251, 251, 252, 253, 254, 254, 255),

    (  8,  12,  16,  19,  22,  24,  27,  29,  31,  34,  36,  38,  40,  41,  43,  45,
      47,  49,  50,  52,  53,  55,  57,  58,  60,  61,  63,  64,  65,  67,  68,  70,
      71,  72,  74,  75,  76,  77,  79,  80,  81,  82,  84,  85,  86,  87,  88,  90,
      91,  92,  93,  94,  95,  96,  98,  99, 100, 101, 102, 103, 104, 105, 106, 107,
     108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123,
     124, 125, 126, 127, 128, 129, 130, 131, 132, 133, 134, 135, 135, 136, 137, 138,
     139, 140, 141, 142, 143, 143, 144, 145, 146, 147, 148, 149, 150, 150, 151, 152,
     153, 154, 155, 155, 156, 157, 158, 159, 160, 160, 161, 162, 163, 164, 165, 165,
     166, 167, 168, 169, 169, 170, 171, 172, 173, 173, 174, 175, 176, 176, 177, 178,
     179, 180, 180, 181, 182, 183, 183, 184, 185, 186, 186, 187, 188, 189, 189, 190,
     191, 192, 192, 193, 194, 195, 195, 196, 197, 197, 198, 199, 200, 200, 201, 202,
     202, 203, 204, 205, 205, 206, 207, 207, 208, 209, 210, 210, 211, 212, 212, 213,
     214, 214, 215, 216, 216, 217, 218, 219, 219, 220, 221, 221, 222, 223, 223, 224,
     225, 225, 226, 227, 227, 228, 229, 229, 230, 231, 231, 232, 233, 233, 234, 235,
     235, 236, 237, 237, 238, 238, 239, 240, 240, 241, 242, 242, 243, 244, 244, 245,
     246, 246, 247, 247, 248, 249, 249, 250, 251, 251, 252, 253, 253, 254, 254, 255),

    ( 16,  23,  28,  32,  36,  39,  42,  45,  48,  50,  53,  55,  57,  60,  62,  64,
      66,  68,  69,  71,  73,  75,  76,  78,  80,  81,  83,  84,  86,  87,  89,  90,
      92,  93,  94,  96,  97,  98, 100, 101, 102, 103, 105, 106, 107, 108, 109, 110,
     112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 128,
     128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143,
     143, 144, 145, 146, 147, 148, 149, 150, 150, 151, 152, 153, 154, 155, 155, 156,
     157, 158, 159, 159, 160, 161, 162, 163, 163, 164, 165, 166, 166, 167, 168, 169,
     169, 170, 171, 172, 172, 173, 174, 175, 175, 176, 177, 177, 178, 179, 180, 180,
     181, 182, 182, 183, 184, 184, 185, 186, 187, 187, 188, 189, 189, 190, 191, 191,
     192, 193, 193, 194, 195, 195, 196, 196, 197, 198, 198, 199, 200, 200, 201, 202,
     202, 203, 203, 204, 205, 205, 206, 207, 207, 208, 208, 209, 210, 210, 211, 211,
     212, 213, 213, 214, 214, 215, 216, 216, 217, 217, 218, 219, 219, 220, 220, 221,
     221, 222, 223, 223, 224, 224, 225, 225, 226, 227, 227, 228, 228, 229, 229, 230,
     230, 231, 232, 232, 233, 233, 234, 234, 235, 235, 236, 236, 237, 237, 238, 239,
     239, 240, 240, 241, 241, 242, 242, 243, 243, 244, 244, 245, 245, 246, 246, 247,
     247, 248, 248, 249, 249, 250, 250, 251, 251, 252, 252, 253, 254, 254, 255, 255)
  );

var
  usegamma: byte;

  curpal: array[0..255] of LongWord;
  videopal: array[0..255] of LongWord;
  cvideopal: array[0..255] of LongWord;
{$IFNDEF OPENGL}
  {$IFDEF HEXEN}
  skvideopal: array[0..255] of LongWord;
  {$ENDIF}
{$ENDIF}

//==============================================================================
//
// V_FindAproxColorIndex
//
//==============================================================================
function V_FindAproxColorIndex(const pal: PLongWordArray; const c: LongWord;
  const start: integer = 0; const finish: integer = 255): integer;

//==============================================================================
//
// V_FindAproxColorIndexExcluding
//
//==============================================================================
function V_FindAproxColorIndexExcluding(const pal: PLongWordArray; const c: LongWord;
  const start: integer = 0; const finish: integer = 255; const exclude: integer = -1): integer;

//==============================================================================
//
// V_FullScreenStretch
//
//==============================================================================
procedure V_FullScreenStretch;

var
  v_translation: PByteArray;

var
  default_palette: PLongWordArray = nil;
  intermissionstretch: boolean = true;

implementation

uses
  i_system,
  mt_utils,
  psi_overlay,
  r_hires,
  r_precalc,
  r_data,
  {$IFNDEF OPENGL}
  r_mmx,
  r_trans8,
  {$IFDEF DOOM_OR_STRIFE}
  r_colormaps,
  st_stuff,
  {$ENDIF}
  {$ENDIF}
  r_flatinfo,
  r_aspect,
  t_patch,
  t_draw,
  w_wad,
  z_zone;

// x and y translation tables for stretcing
var
  preserveX: array[0..319] of integer;
  preserveX426: array[0..425] of integer;
  preserveY: array[0..199] of integer;
  widthintmultiplier: Integer = 0;
  heightintmultiplier: Integer = 0;

//==============================================================================
//
// V_NeedsPreserve
//
//==============================================================================
function V_NeedsPreserve(const destscrn, srcscrn: integer): boolean; overload;
begin
  result := (V_GetScreenWidth(srcscrn) <> V_GetScreenWidth(destscrn)) or
            (V_GetScreenHeight(srcscrn) <> V_GetScreenHeight(destscrn));
end;

//==============================================================================
//
// V_NeedsPreserve
//
//==============================================================================
function V_NeedsPreserve(const destscrn, srcscrn: integer; preserve: boolean): boolean; overload;
begin
  result := preserve and V_NeedsPreserve(destscrn, srcscrn);
end;

//==============================================================================
// V_PreserveX
//
// preserve x coordinates
//
//==============================================================================
function V_PreserveX(const x: integer): integer;
begin
  if x <= 0 then
    result := 0
  else if x >= 320 then
    result := {$IFDEF OPENGL}V_GetScreenWidth(SCN_FG){$ELSE}SCREENWIDTH{$ENDIF}
{$IFNDEF OPENGL}
  else if SCREENWIDTH = 320 then
    result := x
{$ENDIF}
  else
    result := preserveX[x];
end;

//==============================================================================
// V_PreserveX
//
// preserve x coordinates
//
//==============================================================================
function V_PreserveX(const SCN: integer; const x: integer): integer;
var
  swidth: integer;
begin
  swidth := V_GetScreenWidth(SCN);
  if swidth = 320 then
  begin
    if x <= 0 then
      result := 0
    else if x >= 320 then
      result := {$IFDEF OPENGL}V_GetScreenWidth(SCN_FG){$ELSE}SCREENWIDTH{$ENDIF}
  {$IFNDEF OPENGL}
    else if SCREENWIDTH = 320 then
      result := x
  {$ENDIF}
    else
      result := preserveX[x];
  end
  else if swidth = 426 then
  begin
    if x <= 0 then
      result := 0
    else if x >= 426 then
      result := {$IFDEF OPENGL}V_GetScreenWidth(SCN_FG){$ELSE}SCREENWIDTH{$ENDIF}
  {$IFNDEF OPENGL}
    else if SCREENWIDTH = 426 then
      result := x
  {$ENDIF}
    else
      result := preserveX426[x];
  end
  else
  begin
    if x <= 0 then
      result := 0
    else if x >= swidth then
      result := {$IFDEF OPENGL}V_GetScreenWidth(SCN_FG){$ELSE}SCREENWIDTH{$ENDIF}
  {$IFNDEF OPENGL}
    else if SCREENWIDTH = swidth then
      result := x
  {$ENDIF}
    else
      result := trunc(x * {$IFDEF OPENGL}V_GetScreenWidth(SCN_FG){$ELSE}SCREENWIDTH{$ENDIF} / swidth);
  end
end;

//==============================================================================
// V_PreserveY
//
// preserve y coordinates
//
//==============================================================================
function V_PreserveY(const y: integer): integer;
begin
  if y <= 0 then
    result := 0
  else if y >= 200 then
    result := {$IFDEF OPENGL}V_GetScreenHeight(SCN_FG){$ELSE}SCREENHEIGHT{$ENDIF}
{$IFNDEF OPENGL}
  else if SCREENHEIGHT = 200 then
    result := y
{$ENDIF}
  else
    result := preserveY[y];
end;

{$IFDEF OPENGL}

//==============================================================================
//
// V_PreserveGLX
//
//==============================================================================
function V_PreserveGLX(const x: integer): integer;
begin
  result := Round(x * GLDRAWWIDTH / SCREENWIDTH);
end;

//==============================================================================
//
// V_PreserveGLY
//
//==============================================================================
function V_PreserveGLY(const y: integer): integer;
begin
  result := Round(y * GLDRAWHEIGHT / SCREENHEIGHT);
end;
{$ENDIF}

//==============================================================================
// V_PreserveW
//
// preserve width coordinates
//
//==============================================================================
function V_PreserveW(const x: integer; const w: integer): integer;
begin
  result := V_PreserveX(x + w) - V_PreserveX(x);
end;

//==============================================================================
// V_PreserveW
//
// preserve width coordinates
//
//==============================================================================
function V_PreserveW(const SCN: integer; const x: integer; const w: integer): integer;
begin
  result := V_PreserveX(SCN, x + w) - V_PreserveX(SCN, x);
end;

//==============================================================================
// V_PreserveH
//
// preserve height coordinates
//
//==============================================================================
function V_PreserveH(const y: integer; const h: integer): integer;
begin
  result := V_PreserveY(y + h) - V_PreserveY(y);
end;

//==============================================================================
//
// V_CopyCustomScreen8
//
//==============================================================================
procedure V_CopyCustomScreen8(
  scrA: PByteArray;
  width: integer;
  height: integer;
  destscrn: integer);
var
  src: PByteArray;
  dest: PByte;
  destw: integer;
  desth: integer;
  fracxstep: fixed_t;
  fracystep: fixed_t;
  fracx: fixed_t;
  fracy: fixed_t;
  col: integer;
  row: integer;
begin
  destw := V_GetScreenWidth(destscrn);
  desth := V_GetScreenHeight(destscrn);

  fracy := 0;
  fracxstep := FRACUNIT * width div destw;
  fracystep := FRACUNIT * height div desth;

  for row := 0 to desth - 1 do
  begin
    fracx := 0;
    dest := PByte(integer(screens[destscrn]) + destw * row);
    src := @scrA[(fracy div FRACUNIT) * width];
    for col := 0 to destw - 1 do
    begin
      dest^ := src[LongWord(fracx) shr FRACBITS];
      inc(dest);
      fracx := fracx + fracxstep;
    end;
    fracy := fracy + fracystep;
  end
end;

//==============================================================================
//
// V_CopyCustomScreen32
//
//==============================================================================
procedure V_CopyCustomScreen32(
  scrA: PByteArray;
  width: integer;
  height: integer);
var
  src: PByteArray;
  dest: PLongWord;
  destw: integer;
  desth: integer;
  fracxstep: fixed_t;
  fracystep: fixed_t;
  fracx: fixed_t;
  fracy: fixed_t;
  col: integer;
  row: integer;
begin
  destw := V_GetScreenWidth(SCN_FG);
  desth := V_GetScreenHeight(SCN_FG);

  fracy := 0;
  fracxstep := FRACUNIT * width div destw;
  fracystep := FRACUNIT * height div desth;

  dest := @screen32[0];
  for row := 0 to desth - 1 do
  begin
    fracx := 0;
    src := @scrA[(fracy div FRACUNIT) * width];
    for col := 0 to destw - 1 do
    begin
      dest^ := videopal[src[LongWord(fracx) shr FRACBITS]];
      inc(dest);
      fracx := fracx + fracxstep;
    end;
    fracy := fracy + fracystep;
  end
end;

//==============================================================================
//
// V_CopyCustomScreen
//
//==============================================================================
procedure V_CopyCustomScreen(
  src: PByteArray;
  width: integer;
  height: integer;
  destscrn: integer);
begin
  if {$IFNDEF OPENGL}(videomode = vm32bit) and{$ENDIF} (destscrn = SCN_FG) then
    V_CopyCustomScreen32(src, width, height)
  else
    V_CopyCustomScreen8(src, width, height, destscrn)
end;

//==============================================================================
//
// V_CopyRect8
//
//==============================================================================
procedure V_CopyRect8(
  srcx: integer;
  srcy: integer;
  srcscrn: integer;
  width: integer;
  height: integer;
  destx: integer;
  desty: integer;
  destscrn: integer;
  preserve: boolean);
var
  src: PByteArray;
  dest: PByte;
  destA: PByteArray;
  destw: integer;
  desth: integer;
  fracxstep: fixed_t;
  fracystep: fixed_t;
  fracx: fixed_t;
  fracy: fixed_t;
  col: integer;
  row: integer;
  swidth: integer;
  dwidth: integer;
  l: LongWord;
  b: byte;
begin
  swidth := V_GetScreenWidth(srcscrn);
  dwidth := V_GetScreenWidth(destscrn);
  if V_NeedsPreserve(destscrn, srcscrn, preserve) then
  begin
    destw := V_PreserveW(srcscrn, destx, width);

    desth := V_PreserveH(desty, height);

    if (destw <> 0) and (desth <> 0) then
    begin
      destx := V_PreserveX(srcscrn, destx);

      desty := V_PreserveY(desty);

      fracy := srcy * FRACUNIT;
      fracystep := FRACUNIT * height div desth;

      if swidth = 320 then
        case widthintmultiplier of
          2: // Width: 640
            begin
              destw := destw div 2;
              for row := desty to desty + desth - 1 do
              begin
                dest := PByte(integer(screens[destscrn]) + dwidth * row + destx);
                src := PByteArray(integer(screens[srcscrn]) + swidth * (fracy div FRACUNIT) + srcx);
                col := 0;
                while col < destw do
                begin
                  b := src[col];
                  PWord(dest)^ := precal8_toword[b];
                  inc(dest, 2);
                  inc(col);
                end;
                fracy := fracy + fracystep;
              end;
              Exit;
            end;
          4: // Width: 1280
            begin
              destw := destw div 4;
              for row := desty to desty + desth - 1 do
              begin
                dest := PByte(integer(screens[destscrn]) + dwidth * row + destx);
                src := PByteArray(integer(screens[srcscrn]) + swidth * (fracy div FRACUNIT) + srcx);
                col := 0;
                while col < destw do
                begin
                  b := src[col];
                  PLongWord(dest)^ := precal8_tolong[b];
                  inc(dest, 4);
                  inc(col);
                end;
                fracy := fracy + fracystep;
              end;
              Exit;
            end;
          6: // Width: 1920
            begin
              destw := destw div 6;
              for row := desty to desty + desth - 1 do
              begin
                dest := PByte(integer(screens[destscrn]) + dwidth * row + destx);
                src := PByteArray(integer(screens[srcscrn]) + swidth * (fracy div FRACUNIT) + srcx);
                col := 0;
                while col < destw do
                begin
                  b := src[col];
                  PLongWord(dest)^ := precal8_tolong[b];
                  inc(dest, 4);
                  PWord(dest)^ := precal8_toword[b];
                  inc(dest, 2);
                  inc(col);
                end;
                fracy := fracy + fracystep;
              end;
              Exit;
            end;
          8: // Width: 2560
            begin
              destw := destw div 8;
              for row := desty to desty + desth - 1 do
              begin
                dest := PByte(integer(screens[destscrn]) + dwidth * row + destx);
                src := PByteArray(integer(screens[srcscrn]) + swidth * (fracy div FRACUNIT) + srcx);
                col := 0;
                while col < destw do
                begin
                  b := src[col];
                  l := precal8_tolong[b];
                  PLongWord(dest)^ := l;
                  inc(dest, 4);
                  PLongWord(dest)^ := l;
                  inc(dest, 4);
                  inc(col);
                end;
                fracy := fracy + fracystep;
              end;
              Exit;
            end;
         12: // Width: 3840
            begin
              destw := destw div 12;
              for row := desty to desty + desth - 1 do
              begin
                dest := PByte(integer(screens[destscrn]) + dwidth * row + destx);
                src := PByteArray(integer(screens[srcscrn]) + swidth * (fracy div FRACUNIT) + srcx);
                col := 0;
                while col < destw do
                begin
                  b := src[col];
                  l := precal8_tolong[b];
                  PLongWord(dest)^ := l;
                  inc(dest, 4);
                  PLongWord(dest)^ := l;
                  inc(dest, 4);
                  PLongWord(dest)^ := l;
                  inc(dest, 4);
                  inc(col);
                end;
                fracy := fracy + fracystep;
              end;
              Exit;
            end;

        end;

      fracxstep := FRACUNIT * width div destw;

      for row := desty to desty + desth - 1 do
      begin
        fracx := 0;
        dest := PByte(integer(screens[destscrn]) + dwidth * row + destx);
        src := PByteArray(integer(screens[srcscrn]) + swidth * (fracy div FRACUNIT) + srcx);
        for col := 0 to destw - 1 do
        begin
          dest^ := src[LongWord(fracx) shr FRACBITS];
          inc(dest);
          fracx := fracx + fracxstep;
        end;
        fracy := fracy + fracystep;
      end;
    end;
  end
  else
  begin

    src := PByteArray(integer(screens[srcscrn]) + swidth * srcy + srcx);
    destA := PByteArray(integer(screens[destscrn]) + dwidth * desty + destx);

    while height > 0 do
    begin
      memcpy(destA, src, width);
      src := PByteArray(integer(src) + swidth);
      destA := PByteArray(integer(destA) + dwidth);
      dec(height);
    end;
  end;
end;

//==============================================================================
//
// V_CopyRect8_MT
//
//==============================================================================
procedure V_CopyRect8_MT(
  idx, numidxs: integer;
  srcx: integer;
  srcy: integer;
  srcscrn: integer;
  width: integer;
  height: integer;
  destx: integer;
  desty: integer;
  destscrn: integer;
  preserve: boolean);
var
  src: PByteArray;
  dest: PByte;
  destA: PByteArray;
  destw: integer;
  desth: integer;
  fracxstep: fixed_t;
  fracystep: fixed_t;
  fracx: fixed_t;
  fracy: fixed_t;
  col: integer;
  row: integer;
  swidth: integer;
  dwidth: integer;
  l: LongWord;
  b: byte;
begin
  swidth := V_GetScreenWidth(srcscrn);
  dwidth := V_GetScreenWidth(destscrn);
  if V_NeedsPreserve(destscrn, srcscrn, preserve) then
  begin
    destw := V_PreserveW(srcscrn, destx, width);

    desth := V_PreserveH(desty, height);

    if (destw <> 0) and (desth <> 0) then
    begin
      destx := V_PreserveX(srcscrn, destx);

      desty := V_PreserveY(desty);

      fracy := srcy * FRACUNIT;
      fracystep := FRACUNIT * height div desth;

      if swidth = 320 then
        case widthintmultiplier of
          2: // Width: 640
            begin
              destw := destw div 2;
              for row := desty to desty + desth - 1 do
              begin
                if row mod numidxs = idx then
                begin
                  dest := PByte(integer(screens[destscrn]) + dwidth * row + destx);
                  src := PByteArray(integer(screens[srcscrn]) + swidth * (fracy div FRACUNIT) + srcx);
                  col := 0;
                  while col < destw do
                  begin
                    b := src[col];
                    PWord(dest)^ := precal8_toword[b];
                    inc(dest, 2);
                    inc(col);
                  end;
                end;
                fracy := fracy + fracystep;
              end;
              Exit;
            end;
          4: // Width: 1280
            begin
              destw := destw div 4;
              for row := desty to desty + desth - 1 do
              begin
                if row mod numidxs = idx then
                begin
                  dest := PByte(integer(screens[destscrn]) + dwidth * row + destx);
                  src := PByteArray(integer(screens[srcscrn]) + swidth * (fracy div FRACUNIT) + srcx);
                  col := 0;
                  while col < destw do
                  begin
                    b := src[col];
                    PLongWord(dest)^ := precal8_tolong[b];
                    inc(dest, 4);
                    inc(col);
                  end;
                end;
                fracy := fracy + fracystep;
              end;
              Exit;
            end;
          6: // Width: 1920
            begin
              destw := destw div 6;
              for row := desty to desty + desth - 1 do
              begin
                if row mod numidxs = idx then
                begin
                  dest := PByte(integer(screens[destscrn]) + dwidth * row + destx);
                  src := PByteArray(integer(screens[srcscrn]) + swidth * (fracy div FRACUNIT) + srcx);
                  col := 0;
                  while col < destw do
                  begin
                    b := src[col];
                    PLongWord(dest)^ := precal8_tolong[b];
                    inc(dest, 4);
                    PWord(dest)^ := precal8_toword[b];
                    inc(dest, 2);
                    inc(col);
                  end;
                end;
                fracy := fracy + fracystep;
              end;
              Exit;
            end;
          8: // Width: 2560
            begin
              destw := destw div 8;
              for row := desty to desty + desth - 1 do
              begin
                if row mod numidxs = idx then
                begin
                  dest := PByte(integer(screens[destscrn]) + dwidth * row + destx);
                  src := PByteArray(integer(screens[srcscrn]) + swidth * (fracy div FRACUNIT) + srcx);
                  col := 0;
                  while col < destw do
                  begin
                    b := src[col];
                    l := precal8_tolong[b];
                    PLongWord(dest)^ := l;
                    inc(dest, 4);
                    PLongWord(dest)^ := l;
                    inc(dest, 4);
                    inc(col);
                  end;
                end;
                fracy := fracy + fracystep;
              end;
              Exit;
            end;
         12: // Width: 3840
            begin
              destw := destw div 12;
              for row := desty to desty + desth - 1 do
              begin
                if row mod numidxs = idx then
                begin
                  dest := PByte(integer(screens[destscrn]) + dwidth * row + destx);
                  src := PByteArray(integer(screens[srcscrn]) + swidth * (fracy div FRACUNIT) + srcx);
                  col := 0;
                  while col < destw do
                  begin
                    b := src[col];
                    l := precal8_tolong[b];
                    PLongWord(dest)^ := l;
                    inc(dest, 4);
                    PLongWord(dest)^ := l;
                    inc(dest, 4);
                    PLongWord(dest)^ := l;
                    inc(dest, 4);
                    inc(col);
                  end;
                end;
                fracy := fracy + fracystep;
              end;
              Exit;
            end;

        end;

      fracxstep := FRACUNIT * width div destw;

      for row := desty to desty + desth - 1 do
      begin
        if row mod numidxs = idx then
        begin
          fracx := 0;
          dest := PByte(integer(screens[destscrn]) + dwidth * row + destx);
          src := PByteArray(integer(screens[srcscrn]) + swidth * (fracy div FRACUNIT) + srcx);
          for col := 0 to destw - 1 do
          begin
            dest^ := src[LongWord(fracx) shr FRACBITS];
            inc(dest);
            fracx := fracx + fracxstep;
          end;
        end;
        fracy := fracy + fracystep;
      end;
    end;
  end
  else
  begin

    src := PByteArray(integer(screens[srcscrn]) + swidth * srcy + srcx);
    destA := PByteArray(integer(screens[destscrn]) + dwidth * desty + destx);

    while height > 0 do
    begin
      if height mod numidxs = idx then
        memcpy(destA, src, width);
      src := PByteArray(integer(src) + swidth);
      destA := PByteArray(integer(destA) + dwidth);
      dec(height);
    end;
  end;
end;

//==============================================================================
//
// V_CopyRect32
//
//==============================================================================
procedure V_CopyRect32(
  srcx: integer;
  srcy: integer;
  srcscrn: integer;
  width: integer;
  height: integer;
  destx: integer;
  desty: integer;
  preserve: boolean);
var
  src: PByteArray;
  dest: PLongWord;
  destw: integer;
  desth: integer;
  fracxstep: fixed_t;
  fracystep: fixed_t;
  fracx: fixed_t;
  fracy: fixed_t;
  col: integer;
  row: integer;
  swidth: integer;
  dwidth: integer;
  ldest: LongWord;
  ldest64: twolongwords_t;
  pv: boolean;
begin
  swidth := V_GetScreenWidth(srcscrn);
  dwidth := V_GetScreenWidth(SCN_FG);

  pv := V_NeedsPreserve(SCN_FG, srcscrn, preserve);
  if pv then
  begin
    destw := V_PreserveW(srcscrn, destx, width);
    desth := V_PreserveH(desty, height);
  end
  else
  begin
    destw := width;
    desth := height;
  end;

  if (destw <> 0) and (desth <> 0) then
  begin
    if pv then
    begin
      destx := V_PreserveX(srcscrn, destx);
      desty := V_PreserveY(desty);
      fracxstep := FRACUNIT * width div destw;
      fracystep := FRACUNIT * height div desth;
    end
    else
    begin
      fracxstep := FRACUNIT;
      fracystep := FRACUNIT;
    end;

    fracy := srcy * FRACUNIT;

    if pv and (swidth = 320) then
      case widthintmultiplier of
        2: // Width: 640
          begin
            destw := destw div 2;
            for row := desty to desty + desth - 1 do
            begin
              dest := @screen32[dwidth * row + destx];
              src := PByteArray(integer(screens[srcscrn]) + swidth * (fracy div FRACUNIT) + srcx);
              col := 0;
              while col < destw do
              begin
                ldest := videopal[src[col]];
                dest^ := ldest;
                inc(dest);
                dest^ := ldest;
                inc(dest);
                inc(col);
              end;
              fracy := fracy + fracystep;
            end;
            Exit;
          end;
  // JVAL: Unused if we use stallhack to prevent 1280 width
       4: // Width: 1280
          begin
            destw := destw div 4;
            for row := desty to desty + desth - 1 do
            begin
              dest := @screen32[dwidth * row + destx];
              src := PByteArray(integer(screens[srcscrn]) + swidth * (fracy div FRACUNIT) + srcx);
              col := 0;
              while col < destw do
              begin
                ldest64.longword1 := videopal[src[col]];
                ldest64.longword2 := ldest64.longword1;
                PInt64(dest)^ := PInt64(@ldest64)^;
                inc(dest, 2);
                PInt64(dest)^ := PInt64(@ldest64)^;
                inc(dest, 2);
                inc(col);
              end;
              fracy := fracy + fracystep;
            end;
            Exit;
          end;
        5: // Width: 1600
          begin
            destw := destw div 5;
            for row := desty to desty + desth - 1 do
            begin
              dest := @screen32[dwidth * row + destx];
              src := PByteArray(integer(screens[srcscrn]) + swidth * (fracy div FRACUNIT) + srcx);
              col := 0;
              while col < destw do
              begin
                ldest64.longword1 := videopal[src[col]];
                ldest64.longword2 := ldest64.longword1;
                PInt64(dest)^ := PInt64(@ldest64)^;
                inc(dest, 2);
                PInt64(dest)^ := PInt64(@ldest64)^;
                inc(dest, 2);
                dest^ := ldest64.longword1;
                inc(dest);
                inc(col);
              end;
              fracy := fracy + fracystep;
            end;
            Exit;
          end;
        6: // Width: 1920
          begin
            destw := destw div 6;
            for row := desty to desty + desth - 1 do
            begin
              dest := @screen32[dwidth * row + destx];
              src := PByteArray(integer(screens[srcscrn]) + swidth * (fracy div FRACUNIT) + srcx);
              col := 0;
              while col < destw do
              begin
                ldest64.longword1 := videopal[src[col]];
                ldest64.longword2 := ldest64.longword1;
                PInt64(dest)^ := PInt64(@ldest64)^;
                inc(dest, 2);
                PInt64(dest)^ := PInt64(@ldest64)^;
                inc(dest, 2);
                PInt64(dest)^ := PInt64(@ldest64)^;
                inc(dest, 2);
                inc(col);
              end;
              fracy := fracy + fracystep;
            end;
            Exit;
          end;
        8: // Width: 2560
          begin
            destw := destw div 8;
            for row := desty to desty + desth - 1 do
            begin
              dest := @screen32[dwidth * row + destx];
              src := PByteArray(integer(screens[srcscrn]) + swidth * (fracy div FRACUNIT) + srcx);
              col := 0;
              while col < destw do
              begin
                ldest64.longword1 := videopal[src[col]];
                ldest64.longword2 := ldest64.longword1;
                PInt64(dest)^ := PInt64(@ldest64)^;
                inc(dest, 2);
                PInt64(dest)^ := PInt64(@ldest64)^;
                inc(dest, 2);
                PInt64(dest)^ := PInt64(@ldest64)^;
                inc(dest, 2);
                PInt64(dest)^ := PInt64(@ldest64)^;
                inc(dest, 2);
                inc(col);
              end;
              fracy := fracy + fracystep;
            end;
            Exit;
          end;
       12: // Width: 3840
          begin
            destw := destw div 12;
            for row := desty to desty + desth - 1 do
            begin
              dest := @screen32[dwidth * row + destx];
              src := PByteArray(integer(screens[srcscrn]) + swidth * (fracy div FRACUNIT) + srcx);
              col := 0;
              while col < destw do
              begin
                ldest64.longword1 := videopal[src[col]];
                ldest64.longword2 := ldest64.longword1;
                PInt64(dest)^ := PInt64(@ldest64)^;
                inc(dest, 2);
                PInt64(dest)^ := PInt64(@ldest64)^;
                inc(dest, 2);
                PInt64(dest)^ := PInt64(@ldest64)^;
                inc(dest, 2);
                PInt64(dest)^ := PInt64(@ldest64)^;
                inc(dest, 2);
                PInt64(dest)^ := PInt64(@ldest64)^;
                inc(dest, 2);
                PInt64(dest)^ := PInt64(@ldest64)^;
                inc(dest, 2);
                inc(col);
              end;
              fracy := fracy + fracystep;
            end;
            Exit;
          end;
      end;

    for row := desty to desty + desth - 1 do
    begin
      fracx := 0;
      dest := @screen32[dwidth * row + destx];
      src := PByteArray(integer(screens[srcscrn]) + swidth * (fracy div FRACUNIT) + srcx);
      for col := 0 to destw - 1 do
      begin
        dest^ := videopal[src[LongWord(fracx) shr FRACBITS]];
        inc(dest);
        fracx := fracx + fracxstep;
      end;
      fracy := fracy + fracystep;
    end;
  end;
end;

//==============================================================================
//
// V_CopyRect32_MT
//
//==============================================================================
procedure V_CopyRect32_MT(
  idx, numidxs: integer;
  srcx: integer;
  srcy: integer;
  srcscrn: integer;
  width: integer;
  height: integer;
  destx: integer;
  desty: integer;
  preserve: boolean);
var
  src: PByteArray;
  dest: PLongWord;
  destw: integer;
  desth: integer;
  fracxstep: fixed_t;
  fracystep: fixed_t;
  fracx: fixed_t;
  fracy: fixed_t;
  col: integer;
  row: integer;
  swidth: integer;
  dwidth: integer;
  ldest: LongWord;
  ldest64: twolongwords_t;
  pv: boolean;
begin
  swidth := V_GetScreenWidth(srcscrn);
  dwidth := V_GetScreenWidth(SCN_FG);

  pv := V_NeedsPreserve(SCN_FG, srcscrn, preserve);
  if pv then
  begin
    destw := V_PreserveW(srcscrn, destx, width);
    desth := V_PreserveH(desty, height);
  end
  else
  begin
    destw := width;
    desth := height;
  end;

  if (destw <> 0) and (desth <> 0) then
  begin
    if pv then
    begin
      destx := V_PreserveX(srcscrn, destx);
      desty := V_PreserveY(desty);
      fracxstep := FRACUNIT * width div destw;
      fracystep := FRACUNIT * height div desth;
    end
    else
    begin
      fracxstep := FRACUNIT;
      fracystep := FRACUNIT;
    end;

    fracy := srcy * FRACUNIT;

    if pv and (swidth = 320) then
      case widthintmultiplier of
        2: // Width: 640
          begin
            destw := destw div 2;
            for row := desty to desty + desth - 1 do
            begin
              if row mod numidxs = idx then
              begin
                dest := @screen32[dwidth * row + destx];
                src := PByteArray(integer(screens[srcscrn]) + swidth * (fracy div FRACUNIT) + srcx);
                col := 0;
                while col < destw do
                begin
                  ldest := videopal[src[col]];
                  dest^ := ldest;
                  inc(dest);
                  dest^ := ldest;
                  inc(dest);
                  inc(col);
                end;
              end;
              fracy := fracy + fracystep;
            end;
            Exit;
          end;
  // JVAL: Unused if we use stallhack to prevent 1280 width
       4: // Width: 1280
          begin
            destw := destw div 4;
            for row := desty to desty + desth - 1 do
            begin
              if row mod numidxs = idx then
              begin
                dest := @screen32[dwidth * row + destx];
                src := PByteArray(integer(screens[srcscrn]) + swidth * (fracy div FRACUNIT) + srcx);
                col := 0;
                while col < destw do
                begin
                  ldest64.longword1 := videopal[src[col]];
                  ldest64.longword2 := ldest64.longword1;
                  PInt64(dest)^ := PInt64(@ldest64)^;
                  inc(dest, 2);
                  PInt64(dest)^ := PInt64(@ldest64)^;
                  inc(dest, 2);
                  inc(col);
                end;
              end;
              fracy := fracy + fracystep;
            end;
            Exit;
          end;
        5: // Width: 1600
          begin
            destw := destw div 5;
            for row := desty to desty + desth - 1 do
            begin
              if row mod numidxs = idx then
              begin
                dest := @screen32[dwidth * row + destx];
                src := PByteArray(integer(screens[srcscrn]) + swidth * (fracy div FRACUNIT) + srcx);
                col := 0;
                while col < destw do
                begin
                  ldest64.longword1 := videopal[src[col]];
                  ldest64.longword2 := ldest64.longword1;
                  PInt64(dest)^ := PInt64(@ldest64)^;
                  inc(dest, 2);
                  PInt64(dest)^ := PInt64(@ldest64)^;
                  inc(dest, 2);
                  dest^ := ldest64.longword1;
                  inc(dest);
                  inc(col);
                end;
              end;
              fracy := fracy + fracystep;
            end;
            Exit;
          end;
        6: // Width: 1920
          begin
            destw := destw div 6;
            for row := desty to desty + desth - 1 do
            begin
              if row mod numidxs = idx then
              begin
                dest := @screen32[dwidth * row + destx];
                src := PByteArray(integer(screens[srcscrn]) + swidth * (fracy div FRACUNIT) + srcx);
                col := 0;
                while col < destw do
                begin
                  ldest64.longword1 := videopal[src[col]];
                  ldest64.longword2 := ldest64.longword1;
                  PInt64(dest)^ := PInt64(@ldest64)^;
                  inc(dest, 2);
                  PInt64(dest)^ := PInt64(@ldest64)^;
                  inc(dest, 2);
                  PInt64(dest)^ := PInt64(@ldest64)^;
                  inc(dest, 2);
                  inc(col);
                end;
              end;
              fracy := fracy + fracystep;
            end;
            Exit;
          end;
        8: // Width: 2560
          begin
            destw := destw div 8;
            for row := desty to desty + desth - 1 do
            begin
              if row mod numidxs = idx then
              begin
                dest := @screen32[dwidth * row + destx];
                src := PByteArray(integer(screens[srcscrn]) + swidth * (fracy div FRACUNIT) + srcx);
                col := 0;
                while col < destw do
                begin
                  ldest64.longword1 := videopal[src[col]];
                  ldest64.longword2 := ldest64.longword1;
                  PInt64(dest)^ := PInt64(@ldest64)^;
                  inc(dest, 2);
                  PInt64(dest)^ := PInt64(@ldest64)^;
                  inc(dest, 2);
                  PInt64(dest)^ := PInt64(@ldest64)^;
                  inc(dest, 2);
                  PInt64(dest)^ := PInt64(@ldest64)^;
                  inc(dest, 2);
                  inc(col);
                end;
              end;
              fracy := fracy + fracystep;
            end;
            Exit;
          end;
       12: // Width: 3840
          begin
            destw := destw div 12;
            for row := desty to desty + desth - 1 do
            begin
              if row mod numidxs = idx then
              begin
                dest := @screen32[dwidth * row + destx];
                src := PByteArray(integer(screens[srcscrn]) + swidth * (fracy div FRACUNIT) + srcx);
                col := 0;
                while col < destw do
                begin
                  ldest64.longword1 := videopal[src[col]];
                  ldest64.longword2 := ldest64.longword1;
                  PInt64(dest)^ := PInt64(@ldest64)^;
                  inc(dest, 2);
                  PInt64(dest)^ := PInt64(@ldest64)^;
                  inc(dest, 2);
                  PInt64(dest)^ := PInt64(@ldest64)^;
                  inc(dest, 2);
                  PInt64(dest)^ := PInt64(@ldest64)^;
                  inc(dest, 2);
                  PInt64(dest)^ := PInt64(@ldest64)^;
                  inc(dest, 2);
                  PInt64(dest)^ := PInt64(@ldest64)^;
                  inc(dest, 2);
                  inc(col);
                end;
              end;
              fracy := fracy + fracystep;
            end;
            Exit;
          end;
      end;

    for row := desty to desty + desth - 1 do
    begin
      if row mod numidxs = idx then
      begin
        fracx := 0;
        dest := @screen32[dwidth * row + destx];
        src := PByteArray(integer(screens[srcscrn]) + swidth * (fracy div FRACUNIT) + srcx);
        for col := 0 to destw - 1 do
        begin
          dest^ := videopal[src[LongWord(fracx) shr FRACBITS]];
          inc(dest);
          fracx := fracx + fracxstep;
        end;
      end;
      fracy := fracy + fracystep;
    end;
  end;
end;

type
  th_copyrect_t = record
    srcx: integer;
    srcy: integer;
    srcscrn: integer;
    width: integer;
    height: integer;
    destx: integer;
    desty: integer;
    destscrn: integer;
    preserve: boolean;
  end;
  th_copyrect_p = ^th_copyrect_t;

//==============================================================================
//
// V_CopyRect8_thr
//
//==============================================================================
function V_CopyRect8_thr(p: iterator_p): integer; stdcall;
var
  crp: th_copyrect_p;
begin
  crp := p.data;
    V_CopyRect8_MT(
      p.idx, p.numidxs,
      crp.srcx,
      crp.srcy,
      crp.srcscrn,
      crp.width,
      crp.height,
      crp.destx,
      crp.desty,
      crp.destscrn,
      crp.preserve
    );
  result := 0;
end;

//==============================================================================
//
// V_CopyRect32_thr
//
//==============================================================================
function V_CopyRect32_thr(p: iterator_p): integer; stdcall;
var
  crp: th_copyrect_p;
begin
  crp := p.data;
    V_CopyRect32_MT(
      p.idx, p.numidxs,
      crp.srcx,
      crp.srcy,
      crp.srcscrn,
      crp.width,
      crp.height,
      crp.destx,
      crp.desty,
      crp.preserve
    );
  result := 0;
end;

//==============================================================================
//
// V_CopyRect
//
//==============================================================================
procedure V_CopyRect(
  srcx: integer;
  srcy: integer;
  srcscrn: integer;
  width: integer;
  height: integer;
  destx: integer;
  desty: integer;
  destscrn: integer;
  preserve: boolean);
var
  cr: th_copyrect_t;
begin
  if usemultithread then
  begin
    cr.srcx := srcx;
    cr.srcy := srcy;
    cr.srcscrn := srcscrn;
    cr.width := width;
    cr.height := height;
    cr.destx := destx;
    cr.desty := desty;
    cr.destscrn := destscrn;
    cr.preserve := preserve;
    if {$IFNDEF OPENGL}(videomode = vm32bit) and{$ENDIF} (destscrn = SCN_FG) then
      MT_Iterate(@V_CopyRect32_thr, @cr)
    else
      MT_Iterate(@V_CopyRect8_thr, @cr);
  end
  else
  begin
    if {$IFNDEF OPENGL}(videomode = vm32bit) and{$ENDIF} (destscrn = SCN_FG) then
      V_CopyRect32(
        srcx,
        srcy,
        srcscrn,
        width,
        height,
        destx,
        desty,
        preserve)
    else
      V_CopyRect8(
        srcx,
        srcy,
        srcscrn,
        width,
        height,
        destx,
        desty,
        destscrn,
        preserve);
  end;
end;

//==============================================================================
//
// V_CopyAddRect
//
//==============================================================================
procedure V_CopyAddRect(
  srcx: integer;
  srcy: integer;
  srcscrn: integer;
  width: integer;
  height: integer;
  destx: integer;
  desty: integer;
  destscrn: integer;
  preserve: boolean;
  addfactor: fixed_t);
var
  src: PByteArray;
  dest: PLongWord;
  destw: integer;
  desth: integer;
  fracxstep: fixed_t;
  fracystep: fixed_t;
  fracx: fixed_t;
  fracy: fixed_t;
  col: integer;
  row: integer;
  swidth: integer;
  dwidth: integer;
{$IFDEF OPENGL}
  pb: PByte;
  bfactor: byte;
{$ENDIF}
begin
  if addfactor <= 0 then
  begin
    V_CopyRect(srcx, srcy, srcscrn, width, height, destx, desty, destscrn, preserve);
    exit;
  end;

  if addfactor > FRACUNIT then
    exit;

{$IFDEF OPENGL}
  bfactor := 255 - (addfactor shr 16);
{$ENDIF}

  if {$IFNDEF OPENGL}(videomode = vm32bit) and{$ENDIF} (destscrn = SCN_FG) then
  begin
    swidth := V_GetScreenWidth(srcscrn);
    dwidth := V_GetScreenWidth(SCN_FG);

    if V_NeedsPreserve(SCN_FG, srcscrn, preserve) then
    begin
      destw := V_PreserveW(destx, width);
      desth := V_PreserveH(desty, height);
    end
    else
    begin
      destw := width;
      desth := height;
    end;

    if (destw <> 0) and (desth <> 0) then
    begin
      if V_NeedsPreserve(SCN_FG, srcscrn, preserve) then
      begin
        destx := V_PreserveX(destx);
        desty := V_PreserveY(desty);
        fracxstep := FRACUNIT * width div destw;
        fracystep := FRACUNIT * height div desth;
      end
      else
      begin
        fracxstep := FRACUNIT;
        fracystep := FRACUNIT;
      end;

      fracy := srcy * FRACUNIT;

      for row := desty to desty + desth - 1 do
      begin
        fracx := 0;
        dest := @screen32[dwidth * row + destx];
        src := PByteArray(integer(screens[srcscrn]) + swidth * (fracy div FRACUNIT) + srcx);
        for col := 0 to destw - 1 do
        begin
          dest^ := R_ColorAverage(videopal[src[LongWord(fracx) shr FRACBITS]], dest^, addfactor);
          {$IFDEF OPENGL}
          pb := PByte(integer(dest) + 3);
          pb^ := bfactor;
          {$ENDIF}
          inc(dest);
          fracx := fracx + fracxstep;
        end;
        fracy := fracy + fracystep;
      end;
    end;
  end
  else
    V_CopyRect8(srcx, srcy, srcscrn, width, height, destx, desty, destscrn, preserve);
end;

//==============================================================================
//
// V_CopyRectTransparent
//
//==============================================================================
procedure V_CopyRectTransparent8(
  srcx: integer;
  srcy: integer;
  srcscrn: integer;
  width: integer;
  height: integer;
  destx: integer;
  desty: integer;
  destscrn: integer;
  preserve: boolean);
var
  src: PByteArray;
  dest: PByteArray;
  destw: integer;
  desth: integer;
  fracxstep: fixed_t;
  fracystep: fixed_t;
  fracx: fixed_t;
  fracy: fixed_t;
  col: integer;
  row: integer;
  srcb: byte;
  swidth: integer;
  dwidth: integer;
begin
  swidth := V_GetScreenWidth(srcscrn);
  dwidth := V_GetScreenWidth(destscrn);
  if V_NeedsPreserve(destscrn, srcscrn, preserve) then
  begin
    destw := V_PreserveW(destx, width);

    desth := V_PreserveH(desty, height);

    if (destw <> 0) and (desth <> 0) then
    begin
      destx := V_PreserveX(destx);

      desty := V_PreserveY(desty);

      fracy := srcy * FRACUNIT;
      fracxstep := FRACUNIT * width div destw;
      fracystep := FRACUNIT * height div desth;

      for row := desty to desty + desth - 1 do
      begin
        fracx := 0;
        dest := PByteArray(integer(screens[destscrn]) + dwidth * row + destx);
        // Source is a 320 width screen
        src := PByteArray(integer(screens[srcscrn]) + swidth * (fracy div FRACUNIT) + srcx);
        for col := 0 to destw - 1 do
        begin
          srcb := src[LongWord(fracx) shr FRACBITS];
          if srcb <> 0 then
            dest[col] := srcb;
          fracx := fracx + fracxstep;
        end;
        fracy := fracy + fracystep;
      end;
    end;
  end
  else
  begin

    src := PByteArray(integer(screens[srcscrn]) + swidth * srcy + srcx);
    dest := PByteArray(integer(screens[destscrn]) + dwidth * desty + destx);

    while height > 0 do
    begin
      for col := 0 to width - 1 do
      begin
        srcb := src[col];
        if srcb <> 0 then
          dest[col] := srcb;
      end;
      src := PByteArray(integer(src) + swidth);
      dest := PByteArray(integer(dest) + dwidth);
      dec(height);
    end;
  end;
end;

//==============================================================================
//
// V_CopyRectTransparent8_MT
//
//==============================================================================
procedure V_CopyRectTransparent8_MT(
  idx, numidxs: integer;
  srcx: integer;
  srcy: integer;
  srcscrn: integer;
  width: integer;
  height: integer;
  destx: integer;
  desty: integer;
  destscrn: integer;
  preserve: boolean);
var
  src: PByteArray;
  dest: PByteArray;
  destw: integer;
  desth: integer;
  fracxstep: fixed_t;
  fracystep: fixed_t;
  fracx: fixed_t;
  fracy: fixed_t;
  col: integer;
  row: integer;
  srcb: byte;
  swidth: integer;
  dwidth: integer;
begin
  swidth := V_GetScreenWidth(srcscrn);
  dwidth := V_GetScreenWidth(destscrn);
  if V_NeedsPreserve(destscrn, srcscrn, preserve) then
  begin
    destw := V_PreserveW(destx, width);

    desth := V_PreserveH(desty, height);

    if (destw <> 0) and (desth <> 0) then
    begin
      destx := V_PreserveX(destx);

      desty := V_PreserveY(desty);

      fracy := srcy * FRACUNIT;
      fracxstep := FRACUNIT * width div destw;
      fracystep := FRACUNIT * height div desth;

      for row := desty to desty + desth - 1 do
      begin
        if row mod numidxs = idx then
        begin
          fracx := 0;
          dest := PByteArray(integer(screens[destscrn]) + dwidth * row + destx);
          // Source is a 320 width screen
          src := PByteArray(integer(screens[srcscrn]) + swidth * (fracy div FRACUNIT) + srcx);
          for col := 0 to destw - 1 do
          begin
            srcb := src[LongWord(fracx) shr FRACBITS];
            if srcb <> 0 then
              dest[col] := srcb;
            fracx := fracx + fracxstep;
          end;
        end;
        fracy := fracy + fracystep;
      end;
    end;
  end
  else
  begin

    src := PByteArray(integer(screens[srcscrn]) + swidth * srcy + srcx);
    dest := PByteArray(integer(screens[destscrn]) + dwidth * desty + destx);

    while height > 0 do
    begin
      if height mod numidxs = idx then
      begin
        for col := 0 to width - 1 do
        begin
          srcb := src[col];
          if srcb <> 0 then
            dest[col] := srcb;
        end;
      end;
      src := PByteArray(integer(src) + swidth);
      dest := PByteArray(integer(dest) + dwidth);
      dec(height);
    end;
  end;
end;

//==============================================================================
//
// V_CopyRectTransparent32
//
//==============================================================================
procedure V_CopyRectTransparent32(
  srcx: integer;
  srcy: integer;
  srcscrn: integer;
  width: integer;
  height: integer;
  destx: integer;
  desty: integer;
  preserve: boolean);
var
  src: PByteArray;
  dest: PLongWordArray;
  destw: integer;
  destw1: integer;
  desth: integer;
  fracxstep: fixed_t;
  fracxstep4: fixed_t;
  fracystep: fixed_t;
  fracx: fixed_t;
  fracy: fixed_t;
  col: integer;
  row: integer;
  swidth: integer;
  dwidth: integer;
  srcb: byte;
  psrcl: PLongWord;
begin
  swidth := V_GetScreenWidth(srcscrn);
  dwidth := V_GetScreenWidth(SCN_FG);

  if V_NeedsPreserve(SCN_FG, srcscrn, preserve) then
  begin
    destw := V_PreserveW(destx, width);
    desth := V_PreserveH(desty, height);
  end
  else
  begin
    destw := width;
    desth := height;
  end;
  destw1 := destw and $3;

  if (destw <> 0) and (desth <> 0) then
  begin
    if V_NeedsPreserve(SCN_FG, srcscrn, preserve) then
    begin
      destx := V_PreserveX(destx);
      desty := V_PreserveY(desty);
      fracxstep := FRACUNIT * width div destw;
      fracystep := FRACUNIT * height div desth;
      fracxstep4 := 4 * fracxstep;
    end
    else
    begin
      fracxstep := FRACUNIT;
      fracystep := FRACUNIT;
      fracxstep4 := 4 * FRACUNIT;
    end;
    fracy := srcy * FRACUNIT;

    destw := destw - destw1;
    for row := desty to desty + desth - 1 do
    begin
      fracx := 0;
      dest := @screen32[dwidth * row + destx];
      src := PByteArray(integer(screens[srcscrn]) + swidth * (fracy div FRACUNIT) + srcx);
      col := 0;
      while col < destw do
      begin
        psrcl := @src[LongWord(fracx) shr FRACBITS];
        if psrcl^ = 0 then
        begin
          inc(col, 4);
          fracx := fracx + fracxstep4;
        end
        else
        begin
          srcb := PByte(psrcl)^;
          if srcb <> 0 then
            dest[col] := videopal[srcb];
          inc(col);
          fracx := fracx + fracxstep;
        end;
      end;

      for col := destw to destw + destw1 - 1 do
      begin
        srcb := src[LongWord(fracx) shr FRACBITS];
        if srcb <> 0 then
          dest[col] := videopal[srcb];
        fracx := fracx + fracxstep;
      end;

      fracy := fracy + fracystep;
    end;
  end;
end;

//==============================================================================
//
// V_CopyRectTransparent32_MT
//
//==============================================================================
procedure V_CopyRectTransparent32_MT(
  idx, numidxs: integer;
  srcx: integer;
  srcy: integer;
  srcscrn: integer;
  width: integer;
  height: integer;
  destx: integer;
  desty: integer;
  preserve: boolean);
var
  src: PByteArray;
  dest: PLongWordArray;
  destw: integer;
  destw1: integer;
  desth: integer;
  fracxstep: fixed_t;
  fracxstep4: fixed_t;
  fracystep: fixed_t;
  fracx: fixed_t;
  fracy: fixed_t;
  col: integer;
  row: integer;
  swidth: integer;
  dwidth: integer;
  srcb: byte;
  psrcl: PLongWord;
begin
  swidth := V_GetScreenWidth(srcscrn);
  dwidth := V_GetScreenWidth(SCN_FG);

  if V_NeedsPreserve(SCN_FG, srcscrn, preserve) then
  begin
    destw := V_PreserveW(destx, width);
    desth := V_PreserveH(desty, height);
  end
  else
  begin
    destw := width;
    desth := height;
  end;
  destw1 := destw and $3;

  if (destw <> 0) and (desth <> 0) then
  begin
    if V_NeedsPreserve(SCN_FG, srcscrn, preserve) then
    begin
      destx := V_PreserveX(destx);
      desty := V_PreserveY(desty);
      fracxstep := FRACUNIT * width div destw;
      fracystep := FRACUNIT * height div desth;
      fracxstep4 := 4 * fracxstep;
    end
    else
    begin
      fracxstep := FRACUNIT;
      fracystep := FRACUNIT;
      fracxstep4 := 4 * FRACUNIT;
    end;
    fracy := srcy * FRACUNIT;

    destw := destw - destw1;
    for row := desty to desty + desth - 1 do
    begin
      if row mod numidxs = idx then
      begin
        fracx := 0;
        dest := @screen32[dwidth * row + destx];
        src := PByteArray(integer(screens[srcscrn]) + swidth * (fracy div FRACUNIT) + srcx);
        col := 0;
        while col < destw do
        begin
          psrcl := @src[LongWord(fracx) shr FRACBITS];
          if psrcl^ = 0 then
          begin
            inc(col, 4);
            fracx := fracx + fracxstep4;
          end
          else
          begin
            srcb := PByte(psrcl)^;
            if srcb <> 0 then
              dest[col] := videopal[srcb];
            inc(col);
            fracx := fracx + fracxstep;
          end;
        end;

        for col := destw to destw + destw1 - 1 do
        begin
          srcb := src[LongWord(fracx) shr FRACBITS];
          if srcb <> 0 then
            dest[col] := videopal[srcb];
          fracx := fracx + fracxstep;
        end;
      end;

      fracy := fracy + fracystep;
    end;
  end;
end;

type
  th_copyrecttransparent8_t = record
    srcx: integer;
    srcy: integer;
    srcscrn: integer;
    width: integer;
    height: integer;
    destx: integer;
    desty: integer;
    destscrn: integer;
    preserve: boolean;
  end;
  th_copyrecttransparent8_p = ^th_copyrecttransparent8_t;

  th_copyrecttransparent32_t = record
    srcx: integer;
    srcy: integer;
    srcscrn: integer;
    width: integer;
    height: integer;
    destx: integer;
    desty: integer;
    preserve: boolean;
  end;
  th_copyrecttransparent32_p = ^th_copyrecttransparent32_t;

//==============================================================================
//
// V_CopyRectTransparent8_thr
//
//==============================================================================
function V_CopyRectTransparent8_thr(it: iterator_p): integer; stdcall;
var
  p8: th_copyrecttransparent8_p;
begin
  p8 := it.data;
  V_CopyRectTransparent8_MT(
    it.idx, it.numidxs,
    p8.srcx, p8.srcy, p8.srcscrn, p8.width, p8.height, p8.destx, p8.desty, p8.destscrn, p8.preserve);
  result := 0;
end;

//==============================================================================
//
// V_CopyRectTransparent32_thr
//
//==============================================================================
function V_CopyRectTransparent32_thr(it: iterator_p): integer; stdcall;
var
  p32: th_copyrecttransparent32_p;
begin
  p32 := it.data;
  V_CopyRectTransparent32_MT(
    it.idx, it.numidxs,
    p32.srcx, p32.srcy, p32.srcscrn, p32.width, p32.height, p32.destx, p32.desty, p32.preserve);
  result := 0;
end;

//==============================================================================
//
// V_CopyRectTransparent
//
//==============================================================================
procedure V_CopyRectTransparent(
  srcx: integer;
  srcy: integer;
  srcscrn: integer;
  width: integer;
  height: integer;
  destx: integer;
  desty: integer;
  destscrn: integer;
  preserve: boolean);
var
  r8: th_copyrecttransparent8_t;
  r32: th_copyrecttransparent32_t;
begin
  if usemultithread then
  begin
    if {$IFNDEF OPENGL}(videomode = vm32bit) and{$ENDIF} (destscrn = SCN_FG) then
    begin
      r32.srcx := srcx;
      r32.srcy := srcy;
      r32.srcscrn := srcscrn;
      r32.width := width;
      r32.height := height;
      r32.destx := destx;
      r32.desty := desty;
      r32.preserve := preserve;
      MT_Iterate(@V_CopyRectTransparent32_thr, @r32);
    end
    else
    begin
      r8.srcx := srcx;
      r8.srcy := srcy;
      r8.srcscrn := srcscrn;
      r8.width := width;
      r8.height := height;
      r8.destx := destx;
      r8.desty := desty;
      r8.destscrn := destscrn;
      r8.preserve := preserve;
      MT_Iterate(@V_CopyRectTransparent8_thr, @r8);
    end;
  end
  else
  begin
    if {$IFNDEF OPENGL}(videomode = vm32bit) and{$ENDIF} (destscrn = SCN_FG) then
      V_CopyRectTransparent32(srcx, srcy, srcscrn, width, height, destx, desty, preserve)
    else
      V_CopyRectTransparent8(srcx, srcy, srcscrn, width, height, destx, desty, destscrn, preserve);
  end;
end;

{$IFDEF OPENGL}

//==============================================================================
//
// V_ShadeBackground
//
//==============================================================================
procedure V_ShadeBackground(const ofs: integer = 0;
  const count: integer = -1);
var
  src: PByte;
  cnt: integer;
begin
  if count = -1 then
    cnt := V_GetScreenWidth(SCN_FG) * V_GetScreenHeight(SCN_FG)
  else
    cnt := count;
  src := PByte(@screen32[ofs]);
  while cnt > 0 do
  begin
    src^ := src^ shr 1;
    inc(src);
    src^ := src^ shr 1;
    inc(src);
    src^ := src^ shr 1;
    inc(src);
    src^ := $80;
    inc(src);
    dec(cnt);
  end;
end;
{$ELSE}

//==============================================================================
//
// V_ShadeScreen
//
//==============================================================================
procedure V_ShadeScreen(const scn: integer; const ofs: integer = 0;
  const count: integer = -1);
var
  src: PByte;
  cnt: integer;
  cmap: PByteArray;
begin
  if count = -1 then
    cnt := V_GetScreenWidth(scn) * V_GetScreenHeight(scn)
  else
    cnt := count;
  if {$IFNDEF OPENGL}(videomode = vm32bit) and{$ENDIF} (scn = SCN_FG) then
  begin
    cnt := cnt * 4;
    src := PByte(@screen32[0]);
    inc(src, ofs * 4);
  end
  else
  begin
    src := PByte(screens[scn]);
    inc(src, ofs);
  end;

  if {$IFNDEF OPENGL}(videomode = vm32bit) and{$ENDIF} (scn = SCN_FG) then
  begin
    if R_BatchColorShade_AMD(src, cnt) then
      exit;

    while cnt > 16 do
    begin
      src^ := src^ shr 1;
      inc(src);
      src^ := src^ shr 1;
      inc(src);
      src^ := src^ shr 1;
      inc(src);
      src^ := src^ shr 1;
      inc(src);
      src^ := src^ shr 1;
      inc(src);
      src^ := src^ shr 1;
      inc(src);
      src^ := src^ shr 1;
      inc(src);
      src^ := src^ shr 1;
      inc(src);
      src^ := src^ shr 1;
      inc(src);
      src^ := src^ shr 1;
      inc(src);
      src^ := src^ shr 1;
      inc(src);
      src^ := src^ shr 1;
      inc(src);
      src^ := src^ shr 1;
      inc(src);
      src^ := src^ shr 1;
      inc(src);
      src^ := src^ shr 1;
      inc(src);
      src^ := src^ shr 1;
      inc(src);

      dec(cnt, 16);
    end;

    while cnt > 4 do
    begin
      src^ := src^ shr 1;
      inc(src);
      src^ := src^ shr 1;
      inc(src);
      src^ := src^ shr 1;
      inc(src);
      src^ := src^ shr 1;
      inc(src);

      dec(cnt, 4);
    end;

    while cnt > 0 do
    begin
      src^ := src^ shr 1;
      inc(src);
      dec(cnt);
    end;
  end
  else
  begin
    cmap := @{$IFDEF DOOM_OR_STRIFE}def_colormaps{$ELSE}colormaps{$ENDIF}[(NUMCOLORMAPS div 2) * 256];
    while cnt > 0 do
    begin
      src^ := cmap[src^];
      inc(src);
      dec(cnt);
    end;
  end;
end;
{$ENDIF}

//==============================================================================
//
// V_CopyScreenTransparent8
//
//==============================================================================
procedure V_CopyScreenTransparent8(
  srcscrn: integer;
  destscrn: integer; srcoffs: integer = 0; destoffs: integer = 0; size: integer = -1);
var
  src: PByte;
  dest: PByte;
  cnt: integer;
begin
  src := PByte(integer(screens[srcscrn]) +  srcoffs);
  dest := PByte(integer(screens[destscrn]) + destoffs);

  if size = -1 then
    cnt := V_GetScreenWidth(srcscrn) * V_GetScreenHeight(srcscrn)
  else
    cnt := size;
  while cnt > 4 do
  begin
    if PLongWord(src)^ = 0 then
    begin
      inc(dest, 4);
      inc(src, 4);
      dec(cnt, 4)
    end
    else
    begin
      if src^ <> 0 then
        dest^ := src^;
      inc(dest);
      inc(src);
      dec(cnt);
    end;
  end;
  while cnt > 0 do
  begin
    if src^ <> 0 then
      dest^ := src^;
    inc(dest);
    inc(src);
    dec(cnt);
  end;
end;

//==============================================================================
//
// V_CopyScreenTransparent32
//
//==============================================================================
procedure V_CopyScreenTransparent32(
  srcscrn: integer;
  srcoffs: integer = 0; destoffs: integer = 0; size: integer = -1);
var
  src: PByte;
  dest: PLongWord;
  cnt: integer;
begin
  src := PByte(integer(screens[srcscrn]) +  srcoffs);
  dest := @screen32[destoffs];

  if size = -1 then
    cnt := V_GetScreenWidth(srcscrn) * V_GetScreenHeight(srcscrn)
  else
    cnt := size;
  while cnt > 0 do
  begin
    if src^ <> 0 then
      dest^ := videopal[src^];
    inc(dest);
    inc(src);
    dec(cnt);
  end;
end;

//==============================================================================
//
// V_CopyScreenTransparent
//
//==============================================================================
procedure V_CopyScreenTransparent(
  srcscrn: integer;
  destscrn: integer; srcoffs: integer = 0; destoffs: integer = 0; size: integer = -1);
begin
  if {$IFNDEF OPENGL}(videomode = vm32bit) and{$ENDIF} (destscrn = SCN_FG) then
    V_CopyScreenTransparent32(srcscrn, srcoffs, destoffs, size)
  else
    V_CopyScreenTransparent8(srcscrn, destscrn, srcoffs, destoffs, size);
end;

//==============================================================================
//
// V_CopyRawDataToScreen
//
//==============================================================================
procedure V_CopyRawDataToScreen(scrn: integer; const lumpname: string);
var
  len: integer;
  lump: integer;
  lmpdata: PByteArray;
begin
  lump := W_GetNumForName(lumpname);
  len := W_LumpLength(lump);
  if len <> 320 * 200 then
  begin
    {$IFDEF HEXEN}
    I_DevError('V_CopyRawDataToScreen(): Lump "%s" has invalid size: %d (does not have 320x200 size).'#13#10, [lumpname, len]);
    {$ELSE}
    V_DrawPatchFullScreenTMP320x200(lumpname);
    V_CopyRect(0, 0, SCN_TMP, 320, 200, 0, 0, scrn, true);
    exit;
    {$ENDIF}
  end;

  lmpdata := W_CacheLumpNum(lump, PU_STATIC);
  memcpy(@screens[scrn][0], lmpdata, len);
  Z_ChangeTag(lmpdata, PU_CACHE);

{$IFNDEF OPENGL}
  if (videomode = vm32bit) and (scrn = SCN_FG) then
    V_CopyRect32(0, 0, SCN_FG, 320, 200, 0, 0, true);
{$ENDIF}
end;

//==============================================================================
//
// V_RemoveTransparency
//
//==============================================================================
procedure V_RemoveTransparency(const scn: integer; const ofs: integer;
  const count: integer = -1);
var
  src: PByte;
  cnt: integer;
  approx: byte;
begin
  src := PByte(screens[scn]);
  inc(src, ofs);
  if count = -1 then
    cnt := V_GetScreenWidth(scn) * V_GetScreenHeight(scn)
  else
    cnt := count;

  approx := V_FindAproxColorIndex(@curpal, $0, 1);
  while cnt > 0 do
  begin
    if src^ = 0 then
      src^ := approx;
    inc(src);
    dec(cnt);
  end;
end;

//==============================================================================
//
// V_GetScreenWidth
//
//==============================================================================
function V_GetScreenWidth(scrn: integer): integer;
begin
  result := screendimentions[scrn].width;
end;

//==============================================================================
//
// V_GetScreenHeight
//
//==============================================================================
function V_GetScreenHeight(scrn: integer): integer;
begin
  result := screendimentions[scrn].height;
end;

//==============================================================================
//
// V_DrawPatch8
//
//==============================================================================
procedure V_DrawPatch8(x, y: integer; scrn: integer; patch: Ppatch_t; preserve: boolean);
var
  count: integer;
  col: integer;
  column: Pcolumn_t;
  desttop: PByte;
  dest: PByte;
  vs: byte;
  source: PByte;
  w: integer;
  pw: integer;
  ph: integer;
  fracx: fixed_t;
  fracy: fixed_t;
  fracxstep: fixed_t;
  fracystep: fixed_t;
  lasty: integer;
  cury: integer;
  swidth: integer;
  sheight: integer;
  delta, prevdelta: integer;
  tallpatch: boolean;
begin
  swidth := V_GetScreenWidth(scrn);
  if not V_NeedsPreserve(scrn, SCN_320x200, preserve) then
  begin
    y := y - patch.topoffset;
    x := x - patch.leftoffset;

    col := 0;

    desttop := PByte(integer(screens[scrn]) + y * swidth + x);

    w := patch.width;

    while col < w do
    begin
      column := Pcolumn_t(integer(patch) + patch.columnofs[col]);
      delta := 0;
      tallpatch := false;
    // step through the posts in a column
      while column.topdelta <> $ff do
      begin
        source := PByte(integer(column) + 3);
        delta := delta + column.topdelta;
        dest := PByte(integer(desttop) + delta * swidth);
        count := column.length;

        while count > 0 do
        begin
          dest^ := v_translation[source^];
          inc(source);
          inc(dest, swidth);
          dec(count);
        end;
        if not tallpatch then
        begin
          prevdelta := column.topdelta;
          column := Pcolumn_t(integer(column) + column.length + 4);
          if column.topdelta > prevdelta then
            delta := 0
          else
            tallpatch := true;
        end
        else
          column := Pcolumn_t(integer(column) + column.length + 4);
      end;
      inc(col);
      inc(desttop);
    end;

  end
////////////////////////////////////////////////////
// Streching Draw, preserving original dimentions
////////////////////////////////////////////////////
  else
  begin

    y := y - patch.topoffset;
    x := x - patch.leftoffset;

    pw := V_PreserveW(x, patch.width);
    ph := V_PreserveH(y, patch.height);

    if (pw > 0) and (ph > 0) then
    begin

      x := V_PreserveX(x);
      y := V_PreserveY(y);

      fracx := 0;
      fracxstep := FRACUNIT * patch.width div pw;
      fracystep := FRACUNIT * patch.height div ph;

      col := 0;
      desttop := PByte(integer(screens[scrn]) + y * swidth + x);

      sheight := V_GetScreenHeight(scrn);

      while col < pw do
      begin
        column := Pcolumn_t(integer(patch) + patch.columnofs[LongWord(fracx) shr FRACBITS]);
        delta := 0;
        tallpatch := false;
      // step through the posts in a column
        while column.topdelta <> $ff do
        begin
          source := PByte(integer(column) + 3);
          vs := v_translation[source^];
          delta := delta + column.topdelta;
          dest := PByte(integer(desttop) + ((delta * sheight) div 200) * swidth);
          count := column.length;
          fracy := 0;
          lasty := 0;

          while count > 0 do
          begin
            dest^ := vs;
            inc(dest, swidth);
            fracy := fracy + fracystep;
            cury := LongWord(fracy) shr FRACBITS;
            if cury > lasty then
            begin
              lasty := cury;
              inc(source);
              vs := v_translation[source^];
              dec(count);
            end;
          end;
          if not tallpatch then
          begin
            prevdelta := column.topdelta;
            column := Pcolumn_t(integer(column) + column.length + 4);
            if column.topdelta > prevdelta then
              delta := 0
            else
              tallpatch := true;
          end
          else
            column := Pcolumn_t(integer(column) + column.length + 4);
        end;
        inc(col);
        inc(desttop);

        fracx := fracx + fracxstep;
      end;
    end;
  end;
end;

//==============================================================================
//
// V_DrawPatch32
//
//==============================================================================
procedure V_DrawPatch32(x, y: integer; patch: Ppatch_t; preserve: boolean);
var
  count: integer;
  col: integer;
  column: Pcolumn_t;
  desttop: PLongWordArray;
  dest: PLongWord;
  source: PByte;
  w: integer;
  pw: integer;
  ph: integer;
  fracx: fixed_t;
  fracy: fixed_t;
  fracxstep: fixed_t;
  fracystep: fixed_t;
  lasty: integer;
  cury: integer;
  swidth: integer;
  sheight: integer;
  vs: LongWord;
  delta, prevdelta: integer;
  tallpatch: boolean;
begin
  swidth := V_GetScreenWidth(SCN_FG);
  x := x - patch.leftoffset;
  y := y - patch.topoffset;

  if not V_NeedsPreserve(SCN_FG, SCN_320x200, preserve) then
  begin

    col := 0;

    desttop := @screen32[y * swidth + x];

    w := patch.width;

    while col < w do
    begin
      column := Pcolumn_t(integer(patch) + patch.columnofs[col]);
      delta := 0;
      tallpatch := false;
    // step through the posts in a column
      while column.topdelta <> $ff do
      begin
        source := PByte(integer(column) + 3);
        delta := delta + column.topdelta;
        dest := @desttop[delta * swidth];
        count := column.length;

        while count > 0 do
        begin
          dest^ := videopal[source^];
          inc(source);
          inc(dest, swidth);
          dec(count);
        end;
        if not tallpatch then
        begin
          prevdelta := column.topdelta;
          column := Pcolumn_t(integer(column) + column.length + 4);
          if column.topdelta > prevdelta then
            delta := 0
          else
            tallpatch := true;
        end
        else
          column := Pcolumn_t(integer(column) + column.length + 4);
      end;
      inc(col);
      desttop := @desttop[1];
    end;

  end
////////////////////////////////////////////////////
// Streching Draw, preserving original dimentions
////////////////////////////////////////////////////
  else
  begin

    pw := V_PreserveW(x, patch.width);
    ph := V_PreserveH(y, patch.height);

    if (pw > 0) and (ph > 0) then
    begin

      x := V_PreserveX(x);
      y := V_PreserveY(y);

      fracx := 0;
      fracxstep := FRACUNIT * patch.width div pw;
      fracystep := FRACUNIT * patch.height div ph;

      col := 0;
      desttop := @screen32[y * swidth + x];

      sheight := {$IFDEF OPENGL}V_GetScreenHeight(SCN_FG){$ELSE}SCREENHEIGHT{$ENDIF};

      while col < pw do
      begin
        column := Pcolumn_t(integer(patch) + patch.columnofs[fracx div FRACUNIT]);
        delta := 0;
        tallpatch := false;
      // step through the posts in a column
        while column.topdelta <> $ff do
        begin
          source := PByte(integer(column) + 3);
          vs := videopal[source^];
          delta := delta + column.topdelta;
          dest := @desttop[((delta * sheight div 200) * swidth)];
          count := column.length;
          fracy := 0;
          lasty := 0;

          while count > 0 do
          begin
            dest^ := vs;
            inc(dest, swidth);
            fracy := fracy + fracystep;
            cury := LongWord(fracy) shr FRACBITS;
            if cury > lasty then
            begin
              lasty := cury;
              inc(source);
              vs := videopal[source^];
              dec(count);
            end;
          end;
          if not tallpatch then
          begin
            prevdelta := column.topdelta;
            column := Pcolumn_t(integer(column) + column.length + 4);
            if column.topdelta > prevdelta then
              delta := 0
            else
              tallpatch := true;
          end
          else
            column := Pcolumn_t(integer(column) + column.length + 4);
        end;
        inc(col);
        desttop := @desttop[1];

        fracx := fracx + fracxstep;
      end;
    end;
  end;
end;

//==============================================================================
//
// V_DrawPatch
//
//==============================================================================
procedure V_DrawPatch(x, y: integer; scrn: integer; patch: Ppatch_t; preserve: boolean);
begin
  if {$IFNDEF OPENGL}(videomode = vm32bit) and{$ENDIF} (scrn = SCN_FG) then
    V_DrawPatch32(x, y, patch, preserve)
  else
    V_DrawPatch8(x, y, scrn, patch, preserve);
end;

//==============================================================================
//
// V_DrawPatch
//
//==============================================================================
procedure V_DrawPatch(x, y: integer; scrn: integer; const patchname: string; preserve: boolean);
var
  patch: Ppatch_t;
begin
  patch := W_CacheLumpName(patchname, PU_STATIC);
  V_DrawPatch(x, y, scrn, patch, preserve);
  Z_ChangeTag(patch, PU_CACHE);
end;

//==============================================================================
//
// V_DrawPatch
//
//==============================================================================
procedure V_DrawPatch(x, y: integer; scrn: integer; const lump: integer; preserve: boolean); overload;
var
  patch: Ppatch_t;
begin
  patch := W_CacheLumpNum(lump, PU_STATIC);
  V_DrawPatch(x, y, scrn, patch, preserve);
  Z_ChangeTag(patch, PU_CACHE);
end;

//
// V_DrawPatchTransparent
//
{$IFNDEF OPENGL}

//==============================================================================
//
// V_DrawPatchTransparent8
//
//==============================================================================
procedure V_DrawPatchTransparent8(x, y: integer; scrn: integer; patch: Ppatch_t; preserve: boolean);
var
  count: integer;
  col: integer;
  column: Pcolumn_t;
  desttop: PByte;
  dest: PByte;
  vs: byte;
  source: PByte;
  w: integer;
  pw: integer;
  ph: integer;
  fracx: fixed_t;
  fracy: fixed_t;
  fracxstep: fixed_t;
  fracystep: fixed_t;
  lasty: integer;
  cury: integer;
  swidth: integer;
  sheight: integer;
  trans: Ptrans8table_t;
  delta, prevdelta: integer;
  tallpatch: boolean;
begin
  trans := R_GetTransparency8table;
  swidth := V_GetScreenWidth(scrn);
  if not V_NeedsPreserve(scrn, SCN_320x200, preserve) then
  begin
    y := y - patch.topoffset;
    x := x - patch.leftoffset;

    col := 0;

    desttop := PByte(integer(screens[scrn]) + y * swidth + x);

    w := patch.width;

    while col < w do
    begin
      column := Pcolumn_t(integer(patch) + patch.columnofs[col]);
      delta := 0;
      tallpatch := false;
    // step through the posts in a column
      while column.topdelta <> $ff do
      begin
        source := PByte(integer(column) + 3);
        delta := delta + column.topdelta;
        dest := PByte(integer(desttop) + delta * swidth);
        count := column.length;

        while count > 0 do
        begin
          dest^ := trans[dest^ shl 8 + v_translation[source^]];
          inc(source);
          inc(dest, swidth);
          dec(count);
        end;
        if not tallpatch then
        begin
          prevdelta := column.topdelta;
          column := Pcolumn_t(integer(column) + column.length + 4);
          if column.topdelta > prevdelta then
            delta := 0
          else
            tallpatch := true;
        end
        else
          column := Pcolumn_t(integer(column) + column.length + 4);
      end;
      inc(col);
      inc(desttop);
    end;

  end
////////////////////////////////////////////////////
// Streching Draw, preserving original dimentions
////////////////////////////////////////////////////
  else
  begin

    y := y - patch.topoffset;
    x := x - patch.leftoffset;

    pw := V_PreserveW(x, patch.width);
    ph := V_PreserveH(y, patch.height);

    if (pw > 0) and (ph > 0) then
    begin

      x := V_PreserveX(x);
      y := V_PreserveY(y);

      fracx := 0;
      fracxstep := FRACUNIT * patch.width div pw;
      fracystep := FRACUNIT * patch.height div ph;

      col := 0;
      desttop := PByte(integer(screens[scrn]) + y * swidth + x);

      sheight := V_GetScreenHeight(scrn);

      while col < pw do
      begin
        column := Pcolumn_t(integer(patch) + patch.columnofs[LongWord(fracx) shr FRACBITS]);
        delta := 0;
        tallpatch := false;
      // step through the posts in a column
        while column.topdelta <> $ff do
        begin
          source := PByte(integer(column) + 3);
          vs := v_translation[source^];
          delta := delta + column.topdelta;
          dest := PByte(integer(desttop) + ((delta * sheight) div 200) * swidth);
          count := column.length;
          fracy := 0;
          lasty := 0;

          while count > 0 do
          begin
            dest^ := trans[dest^ shl 8 + vs];
            inc(dest, swidth);
            fracy := fracy + fracystep;
            cury := LongWord(fracy) shr FRACBITS;
            if cury > lasty then
            begin
              lasty := cury;
              inc(source);
              vs := v_translation[source^];
              dec(count);
            end;
          end;
          if not tallpatch then
          begin
            prevdelta := column.topdelta;
            column := Pcolumn_t(integer(column) + column.length + 4);
            if column.topdelta > prevdelta then
              delta := 0
            else
              tallpatch := true;
          end
          else
            column := Pcolumn_t(integer(column) + column.length + 4);
        end;
        inc(col);
        inc(desttop);

        fracx := fracx + fracxstep;
      end;
    end;
  end;
end;

//==============================================================================
//
// V_DrawPatchTransparent8_MT
//
//==============================================================================
procedure V_DrawPatchTransparent8_MT(
  idx, numidxs: integer;
  x, y: integer; scrn: integer; patch: Ppatch_t; preserve: boolean);
var
  count: integer;
  col: integer;
  column: Pcolumn_t;
  desttop: PByte;
  dest: PByte;
  vs: byte;
  source: PByte;
  w: integer;
  pw: integer;
  ph: integer;
  fracx: fixed_t;
  fracy: fixed_t;
  fracxstep: fixed_t;
  fracystep: fixed_t;
  lasty: integer;
  cury: integer;
  swidth: integer;
  sheight: integer;
  trans: Ptrans8table_t;
  delta, prevdelta: integer;
  tallpatch: boolean;
begin
  trans := R_GetTransparency8table;
  swidth := V_GetScreenWidth(scrn);
  if not V_NeedsPreserve(scrn, SCN_320x200, preserve) then
  begin
    y := y - patch.topoffset;
    x := x - patch.leftoffset;

    col := 0;

    desttop := PByte(integer(screens[scrn]) + y * swidth + x);

    w := patch.width;

    while col < w do
    begin
      if col mod numidxs = idx then
      begin
        column := Pcolumn_t(integer(patch) + patch.columnofs[col]);
        delta := 0;
        tallpatch := false;
      // step through the posts in a column
        while column.topdelta <> $ff do
        begin
          source := PByte(integer(column) + 3);
          delta := delta + column.topdelta;
          dest := PByte(integer(desttop) + delta * swidth);
          count := column.length;

          while count > 0 do
          begin
            dest^ := trans[dest^ shl 8 + v_translation[source^]];
            inc(source);
            inc(dest, swidth);
            dec(count);
          end;
          if not tallpatch then
          begin
            prevdelta := column.topdelta;
            column := Pcolumn_t(integer(column) + column.length + 4);
            if column.topdelta > prevdelta then
              delta := 0
            else
              tallpatch := true;
          end
          else
            column := Pcolumn_t(integer(column) + column.length + 4);
        end;
      end;
      inc(col);
      inc(desttop);
    end;

  end
////////////////////////////////////////////////////
// Streching Draw, preserving original dimentions
////////////////////////////////////////////////////
  else
  begin

    y := y - patch.topoffset;
    x := x - patch.leftoffset;

    pw := V_PreserveW(x, patch.width);
    ph := V_PreserveH(y, patch.height);

    if (pw > 0) and (ph > 0) then
    begin

      x := V_PreserveX(x);
      y := V_PreserveY(y);

      fracx := 0;
      fracxstep := FRACUNIT * patch.width div pw;
      fracystep := FRACUNIT * patch.height div ph;

      col := 0;
      desttop := PByte(integer(screens[scrn]) + y * swidth + x);

      sheight := V_GetScreenHeight(scrn);

      while col < pw do
      begin
        if col mod numidxs = idx then
        begin
          column := Pcolumn_t(integer(patch) + patch.columnofs[LongWord(fracx) shr FRACBITS]);
          delta := 0;
          tallpatch := false;
        // step through the posts in a column
          while column.topdelta <> $ff do
          begin
            source := PByte(integer(column) + 3);
            vs := v_translation[source^];
            delta := delta + column.topdelta;
            dest := PByte(integer(desttop) + ((delta * sheight) div 200) * swidth);
            count := column.length;
            fracy := 0;
            lasty := 0;

            while count > 0 do
            begin
              dest^ := trans[dest^ shl 8 + vs];
              inc(dest, swidth);
              fracy := fracy + fracystep;
              cury := LongWord(fracy) shr FRACBITS;
              if cury > lasty then
              begin
                lasty := cury;
                inc(source);
                vs := v_translation[source^];
                dec(count);
              end;
            end;
            if not tallpatch then
            begin
              prevdelta := column.topdelta;
              column := Pcolumn_t(integer(column) + column.length + 4);
              if column.topdelta > prevdelta then
                delta := 0
              else
                tallpatch := true;
            end
            else
              column := Pcolumn_t(integer(column) + column.length + 4);
          end;
        end;
        inc(col);
        inc(desttop);

        fracx := fracx + fracxstep;
      end;
    end;
  end;
end;
{$ENDIF}

//==============================================================================
//
// V_DrawPatchTransparent32
//
//==============================================================================
procedure V_DrawPatchTransparent32(x, y: integer; patch: Ppatch_t; preserve: boolean);
var
  count: integer;
  col: integer;
  column: Pcolumn_t;
  desttop: PLongWordArray;
  dest: PLongWord;
{$IFDEF OPENGL}
  pb: PByte;
{$ENDIF}
  source: PByte;
  w: integer;
  pw: integer;
  ph: integer;
  fracx: fixed_t;
  fracy: fixed_t;
  fracxstep: fixed_t;
  fracystep: fixed_t;
  lasty: integer;
  cury: integer;
  swidth: integer;
  sheight: integer;
  vs: LongWord;
  delta, prevdelta: integer;
  tallpatch: boolean;
begin
  swidth := V_GetScreenWidth(SCN_FG);
  x := x - patch.leftoffset;
  y := y - patch.topoffset;

  if not V_NeedsPreserve(SCN_FG, SCN_320x200, preserve) then
  begin

    col := 0;

    desttop := @screen32[y * swidth + x];

    w := patch.width;

    while col < w do
    begin
      column := Pcolumn_t(integer(patch) + patch.columnofs[col]);
      delta := 0;
      tallpatch := false;
    // step through the posts in a column
      while column.topdelta <> $ff do
      begin
        source := PByte(integer(column) + 3);
        delta := delta + column.topdelta;
        dest := @desttop[delta * swidth];
        count := column.length;

        while count > 0 do
        begin
          dest^ := R_ColorMean(videopal[source^], dest^);
          {$IFDEF OPENGL}
          pb := PByte(integer(dest) + 3);
          pb^ := $80;
          {$ENDIF}
          inc(source);
          inc(dest, swidth);
          dec(count);
        end;
        if not tallpatch then
        begin
          prevdelta := column.topdelta;
          column := Pcolumn_t(integer(column) + column.length + 4);
          if column.topdelta > prevdelta then
            delta := 0
          else
            tallpatch := true;
        end
        else
          column := Pcolumn_t(integer(column) + column.length + 4);
      end;
      inc(col);
      desttop := @desttop[1];
    end;

  end
////////////////////////////////////////////////////
// Streching Draw, preserving original dimentions
////////////////////////////////////////////////////
  else
  begin

    pw := V_PreserveW(x, patch.width);
    ph := V_PreserveH(y, patch.height);

    if (pw > 0) and (ph > 0) then
    begin

      x := V_PreserveX(x);
      y := V_PreserveY(y);

      fracx := 0;
      fracxstep := FRACUNIT * patch.width div pw;
      fracystep := FRACUNIT * patch.height div ph;

      col := 0;
      desttop := @screen32[y * swidth + x];

      sheight := {$IFDEF OPENGL}V_GetScreenHeight(SCN_FG){$ELSE}SCREENHEIGHT{$ENDIF};

      while col < pw do
      begin
        column := Pcolumn_t(integer(patch) + patch.columnofs[fracx div FRACUNIT]);
        delta := 0;
        tallpatch := false;
      // step through the posts in a column
        while column.topdelta <> $ff do
        begin
          source := PByte(integer(column) + 3);
          vs := videopal[source^];
          delta := delta + column.topdelta;
          dest := @desttop[((delta * sheight div 200) * swidth)];
          count := column.length;
          fracy := 0;
          lasty := 0;

          while count > 0 do
          begin
            dest^ := R_ColorMean(vs, dest^);
           {$IFDEF OPENGL}
            pb := PByte(integer(dest) + 3);
            pb^ := $80;
           {$ENDIF}
            inc(dest, swidth);
            fracy := fracy + fracystep;
            cury := LongWord(fracy) shr FRACBITS;
            if cury > lasty then
            begin
              lasty := cury;
              inc(source);
              vs := videopal[source^] ;
              dec(count);
            end;
          end;
          if not tallpatch then
          begin
            prevdelta := column.topdelta;
            column := Pcolumn_t(integer(column) + column.length + 4);
            if column.topdelta > prevdelta then
              delta := 0
            else
              tallpatch := true;
          end
          else
            column := Pcolumn_t(integer(column) + column.length + 4);
        end;
        inc(col);
        desttop := @desttop[1];

        fracx := fracx + fracxstep;
      end;
    end;
  end;
end;

{$IFNDEF OPENGL}
//==============================================================================
//
// V_DrawPatchTransparent32_MT
//
//==============================================================================
procedure V_DrawPatchTransparent32_MT(
  idx, numidxs: integer;
  x, y: integer; patch: Ppatch_t; preserve: boolean);
var
  count: integer;
  col: integer;
  column: Pcolumn_t;
  desttop: PLongWordArray;
  dest: PLongWord;
{$IFDEF OPENGL}
  pb: PByte;
{$ENDIF}
  source: PByte;
  w: integer;
  pw: integer;
  ph: integer;
  fracx: fixed_t;
  fracy: fixed_t;
  fracxstep: fixed_t;
  fracystep: fixed_t;
  lasty: integer;
  cury: integer;
  swidth: integer;
  sheight: integer;
  vs: LongWord;
  delta, prevdelta: integer;
  tallpatch: boolean;
begin
  swidth := V_GetScreenWidth(SCN_FG);
  x := x - patch.leftoffset;
  y := y - patch.topoffset;

  if not V_NeedsPreserve(SCN_FG, SCN_320x200, preserve) then
  begin

    col := 0;

    desttop := @screen32[y * swidth + x];

    w := patch.width;

    while col < w do
    begin
      if col mod numidxs = idx then
      begin
        column := Pcolumn_t(integer(patch) + patch.columnofs[col]);
        delta := 0;
        tallpatch := false;
      // step through the posts in a column
        while column.topdelta <> $ff do
        begin
          source := PByte(integer(column) + 3);
          delta := delta + column.topdelta;
          dest := @desttop[delta * swidth];
          count := column.length;

          while count > 0 do
          begin
            dest^ := R_ColorMean(videopal[source^], dest^);
            {$IFDEF OPENGL}
            pb := PByte(integer(dest) + 3);
            pb^ := $80;
            {$ENDIF}
            inc(source);
            inc(dest, swidth);
            dec(count);
          end;
          if not tallpatch then
          begin
            prevdelta := column.topdelta;
            column := Pcolumn_t(integer(column) + column.length + 4);
            if column.topdelta > prevdelta then
              delta := 0
            else
              tallpatch := true;
          end
          else
            column := Pcolumn_t(integer(column) + column.length + 4);
        end;
      end;
      inc(col);
      desttop := @desttop[1];
    end;

  end
////////////////////////////////////////////////////
// Streching Draw, preserving original dimentions
////////////////////////////////////////////////////
  else
  begin

    pw := V_PreserveW(x, patch.width);
    ph := V_PreserveH(y, patch.height);

    if (pw > 0) and (ph > 0) then
    begin

      x := V_PreserveX(x);
      y := V_PreserveY(y);

      fracx := 0;
      fracxstep := FRACUNIT * patch.width div pw;
      fracystep := FRACUNIT * patch.height div ph;

      col := 0;
      desttop := @screen32[y * swidth + x];

      sheight := {$IFDEF OPENGL}V_GetScreenHeight(SCN_FG){$ELSE}SCREENHEIGHT{$ENDIF};;

      while col < pw do
      begin
        if col mod numidxs = idx then
        begin
          column := Pcolumn_t(integer(patch) + patch.columnofs[fracx div FRACUNIT]);
          delta := 0;
          tallpatch := false;
        // step through the posts in a column
          while column.topdelta <> $ff do
          begin
            source := PByte(integer(column) + 3);
            vs := videopal[source^];
            delta := delta + column.topdelta;
            dest := @desttop[((delta * sheight div 200) * swidth)];
            count := column.length;
            fracy := 0;
            lasty := 0;

            while count > 0 do
            begin
              dest^ := R_ColorMean(vs, dest^);
             {$IFDEF OPENGL}
              pb := PByte(integer(dest) + 3);
              pb^ := $80;
             {$ENDIF}
              inc(dest, swidth);
              fracy := fracy + fracystep;
              cury := LongWord(fracy) shr FRACBITS;
              if cury > lasty then
              begin
                lasty := cury;
                inc(source);
                vs := videopal[source^] ;
                dec(count);
              end;
            end;
            if not tallpatch then
            begin
              prevdelta := column.topdelta;
              column := Pcolumn_t(integer(column) + column.length + 4);
              if column.topdelta > prevdelta then
                delta := 0
              else
                tallpatch := true;
            end
            else
              column := Pcolumn_t(integer(column) + column.length + 4);
          end;
        end;
        inc(col);
        desttop := @desttop[1];

        fracx := fracx + fracxstep;
      end;
    end;
  end;
end;
{$ENDIF}

//==============================================================================
//
// V_DrawPatchTransparent
//
//==============================================================================
procedure V_DrawPatchTransparent(x, y: integer; scrn: integer; patch: Ppatch_t; preserve: boolean);
begin
  {$IFDEF OPENGL}
  if scrn = SCN_FG then
    V_DrawPatchTransparent32(x, y, patch, preserve)
  else // JVAL unsupported :(
    V_DrawPatch(x, y, scrn, patch, preserve);
  {$ELSE}
  if (videomode = vm32bit) and (scrn = SCN_FG) then
    V_DrawPatchTransparent32(x, y, patch, preserve)
  else
    V_DrawPatchTransparent8(x, y, scrn, patch, preserve);
  {$ENDIF}
end;

{$IFNDEF OPENGL}
type
  th_drawpathctransparent_t = record
    x, y: integer;
    scrn: integer;
    patch: Ppatch_t;
    preserve: boolean;
  end;
  th_drawpathctransparent_p = ^th_drawpathctransparent_t;

//==============================================================================
//
// V_DrawPatchTransparent8_thr
//
//==============================================================================
function V_DrawPatchTransparent8_thr(p: iterator_p): integer; stdcall;
var
  param: th_drawpathctransparent_p;
begin
  param := p.data;
  V_DrawPatchTransparent8_MT(
    p.idx, p.numidxs, param.x, param.y, param.scrn, param.patch, param.preserve);
  result := 0;
end;

//==============================================================================
//
// V_DrawPatchTransparent32_thr
//
//==============================================================================
function V_DrawPatchTransparent32_thr(p: iterator_p): integer; stdcall;
var
  param: th_drawpathctransparent_p;
begin
  param := p.data;
  V_DrawPatchTransparent32_MT(
    p.idx, p.numidxs, param.x, param.y, param.patch, param.preserve);
  result := 0;
end;

//==============================================================================
//
// V_DrawPatchTransparentMT
//
//==============================================================================
procedure V_DrawPatchTransparentMT(x, y: integer; scrn: integer; patch: Ppatch_t; preserve: boolean);
var
  parm: th_drawpathctransparent_t;
begin
  parm.x := x;
  parm.y := y;
  parm.scrn := scrn;
  parm.patch := patch;
  parm.preserve := preserve;

  if (videomode = vm32bit) and (scrn = SCN_FG) then
    MT_Iterate(
      @V_DrawPatchTransparent32_thr, @parm)
  else
    MT_Iterate(
      @V_DrawPatchTransparent8_thr, @parm)
end;
{$ENDIF}

//==============================================================================
// V_DrawPatchZoomed8
//
// V_DrawPatchZoomed
//
//==============================================================================
procedure V_DrawPatchZoomed8(x, y: integer; scrn: integer; patch: Ppatch_t; preserve: boolean; fraczoom: fixed_t);
var
  count: integer;
  col: integer;
  column: Pcolumn_t;
  desttop: PByte;
  dest: PByte;
  source: PByte;
  pw: integer;
  ph: integer;
  fracx: fixed_t;
  fracy: fixed_t;
  fracxstep: fixed_t;
  fracystep: fixed_t;
  lasty: integer;
  cury: integer;
  swidth: integer;
  sheight: integer;
  delta, prevdelta: integer;
  tallpatch: boolean;
begin
  if fraczoom = FRACUNIT then
  begin
    V_DrawPatch(x, y, scrn, patch, preserve);
    exit;
  end;

  y := y - patch.topoffset;
  x := x - patch.leftoffset;

  if V_NeedsPreserve(scrn, SCN_320x200, preserve) then
  begin
    pw := V_PreserveW(x, patch.width) * fraczoom div FRACUNIT;
    ph := V_PreserveH(y, patch.height) * fraczoom div FRACUNIT;
    x := V_PreserveX(x);
    y := V_PreserveY(y);
  end
  else
  begin
    pw := patch.width * fraczoom div FRACUNIT;
    ph := patch.height * fraczoom div FRACUNIT;
  end;

  if (pw > 0) and (ph > 0) then
  begin

    fracx := 0;
    fracxstep := FRACUNIT * patch.width div pw;
    fracystep := FRACUNIT * patch.height div ph;

    swidth := V_GetScreenWidth(scrn);
    sheight := V_GetScreenHeight(scrn);

    desttop := PByte(integer(screens[scrn]) + y * swidth + x);

    col := 0;

    while col < pw do
    begin
      column := Pcolumn_t(integer(patch) + patch.columnofs[fracx shr FRACBITS]);
      delta := 0;
      tallpatch := false;
      // step through the posts in a column
      while column.topdelta <> $ff do
      begin
        source := PByte(integer(column) + 3);
        delta := delta + column.topdelta;
        if preserve then
          dest := PByte(integer(desttop) + ((delta * sheight) * fraczoom div FRACUNIT div 200) * swidth)
        else
          dest := PByte(integer(desttop) + (delta * fraczoom div FRACUNIT) * swidth);
        count := column.length;
        fracy := 0;
        lasty := 0;

        while count > 0 do
        begin
          dest^ := v_translation[source^];
          inc(dest, swidth);
          fracy := fracy + fracystep;
          cury := LongWord(fracy) shr FRACBITS;
          if cury > lasty then
          begin
            lasty := cury;
            inc(source);
            dec(count);
          end;
        end;
        if not tallpatch then
        begin
          prevdelta := column.topdelta;
          column := Pcolumn_t(integer(column) + column.length + 4);
          if column.topdelta > prevdelta then
            delta := 0
          else
            tallpatch := true;
        end
        else
          column := Pcolumn_t(integer(column) + column.length + 4);
      end;
      inc(desttop);
      inc(col);

      fracx := fracx + fracxstep;
    end;
  end;

end;

//==============================================================================
//
// V_DrawPatchZoomed32
//
//==============================================================================
procedure V_DrawPatchZoomed32(x, y: integer; patch: Ppatch_t; preserve: boolean; fraczoom: fixed_t);
var
  count: integer;
  col: integer;
  column: Pcolumn_t;
  desttop: PLongWordArray;
  dest: PLongWord;
  source: PByte;
  pw: integer;
  ph: integer;
  fracx: fixed_t;
  fracy: fixed_t;
  fracxstep: fixed_t;
  fracystep: fixed_t;
  lasty: integer;
  cury: integer;
  swidth: integer;
  sheight: integer;
  delta, prevdelta: integer;
  tallpatch: boolean;
begin
  if fraczoom = FRACUNIT then
  begin
    V_DrawPatch32(x, y, patch, preserve);
    exit;
  end;

  y := y - patch.topoffset;
  x := x - patch.leftoffset;

  if V_NeedsPreserve(SCN_FG, SCN_320x200, preserve) then
  begin
    pw := V_PreserveW(x, patch.width) * fraczoom div FRACUNIT;
    ph := V_PreserveH(y, patch.height) * fraczoom div FRACUNIT;
    x := V_PreserveX(x);
    y := V_PreserveY(y);
  end
  else
  begin
    pw := patch.width * fraczoom div FRACUNIT;
    ph := patch.height * fraczoom div FRACUNIT;
  end;

  if (pw > 0) and (ph > 0) then
  begin

    fracx := 0;
    fracxstep := FRACUNIT * patch.width div pw;
    fracystep := FRACUNIT * patch.height div ph;

    swidth := V_GetScreenWidth(SCN_FG);
    sheight := V_GetScreenHeight(SCN_FG);

    desttop := @screen32[y * swidth + x];

    col := 0;

    while col < pw do
    begin
      column := Pcolumn_t(integer(patch) + patch.columnofs[fracx div FRACUNIT]);
      delta := 0;
      tallpatch := false;
      // step through the posts in a column
      while column.topdelta <> $ff do
      begin
        source := PByte(integer(column) + 3);
        delta := delta + column.topdelta;
        if preserve then
          dest := @desttop[((delta * sheight) * fraczoom div FRACUNIT div 200) * swidth]
        else
          dest := @desttop[(delta * fraczoom div FRACUNIT) * swidth];
        count := column.length;
        fracy := 0;
        lasty := 0;

        while count > 0 do
        begin
          dest^ := videopal[source^];
          inc(dest, swidth);
          fracy := fracy + fracystep;
          cury := LongWord(fracy) shr FRACBITS;
          if cury > lasty then
          begin
            lasty := cury;
            inc(source);
            dec(count);
          end;
        end;
        if not tallpatch then
        begin
          prevdelta := column.topdelta;
          column := Pcolumn_t(integer(column) + column.length + 4);
          if column.topdelta > prevdelta then
            delta := 0
          else
            tallpatch := true;
        end
        else
          column := Pcolumn_t(integer(column) + column.length + 4);
      end;
      desttop := @desttop[1];
      inc(col);

      fracx := fracx + fracxstep;
    end;
  end;

end;

//==============================================================================
//
// V_DrawPatchZoomed
//
//==============================================================================
procedure V_DrawPatchZoomed(x, y: integer; scrn: integer; patch: Ppatch_t; preserve: boolean; fraczoom: fixed_t);
begin
  if {$IFNDEF OPENGL}(videomode = vm32bit) and{$ENDIF} (scrn = SCN_FG) then
    V_DrawPatchZoomed32(x, y, patch, preserve, fraczoom)
  else
    V_DrawPatchZoomed8(x, y, scrn, patch, preserve, fraczoom);
end;

//==============================================================================
//
// V_DrawPatchFlipped
// Masks a column based masked pic to the screen.
// Flips horizontally, e.g. to mirror face.
//
//==============================================================================
procedure V_DrawPatchFlipped(x, y: integer; scrn: integer; patch: Ppatch_t);
var
  count: integer;
  col: integer;
  column: Pcolumn_t;
  desttop: PByte;
  dest: PByte;
  source: PByte;
  w: integer;
  delta, prevdelta: integer;
  tallpatch: boolean;
begin
  y := y - patch.topoffset;
  x := x - patch.leftoffset;

  col := 0;

  desttop := PByte(integer(screens[scrn]) + y * 320 + x);

  w := patch.width;

  while col < w do
  begin
    column := Pcolumn_t(integer(patch) + patch.columnofs[w - 1 - col]);
    delta := 0;
    tallpatch := false;
  // step through the posts in a column
    while column.topdelta <> $ff do
    begin
      source := PByte(integer(column) + 3);
      delta := delta + column.topdelta;
      dest := PByte(integer(desttop) + delta * 320);
      count := column.length;

      while count > 0 do
      begin
        dest^ := v_translation[source^];
        inc(source);
        inc(dest, 320);
        dec(count);
      end;
      if not tallpatch then
      begin
        prevdelta := column.topdelta;
        column := Pcolumn_t(integer(column) + column.length + 4);
        if column.topdelta > prevdelta then
          delta := 0
        else
          tallpatch := true;
      end
      else
        column := Pcolumn_t(integer(column) + column.length + 4);
    end;
    inc(col);
    inc(desttop);
  end;
end;

//==============================================================================
//
// V_ScaleBuffer8
//
//==============================================================================
procedure V_ScaleBuffer8(var FData: pointer; const FWidth, FHeight: integer; const AWidth, AHeight: integer);
var
  xs, ys, xi, yi, x, y: integer;
  newimage: pointer;
  edi, esi: PByteArray;
begin
  if (AWidth = FWidth) and (AHeight = FHeight) then
    exit;

  xi := (FWidth shl 16) div AWidth;
  yi := (FHeight shl 16) div AHeight;
  newimage := malloc(AWidth * AHeight);
  edi := newimage;
  ys := 0;
  for y := 0 to AHeight - 1 do
  begin
    esi := @PByteArray(FData)[(ys shr 16) * fWidth];
    xs := 0;
    for x := 0 to AWidth - 1 do
    begin
      edi[x] := esi[xs shr 16];
      xs := xs + xi;
    end;
    edi := @edi[AWidth];
    ys := ys + yi;
  end;
  memfree(FData, FWidth * FHeight);
  FData := newimage;
end;

//==============================================================================
//
// V_DrawPatchFullScreenTMP320x200
//
//==============================================================================
procedure V_DrawPatchFullScreenTMP320x200(p: Ppatch_t);
var
  oldscreen, newscreen: PByteArray;
begin
  if (p.width = 320) and (p.height = 200) then
  begin
    V_DrawPatch8(0, 0, SCN_TMP, p, false);
    exit;
  end;

  oldscreen := screens[SCN_TMP];
  screendimentions[SCN_TMP].width := p.width;
  screendimentions[SCN_TMP].height := p.height;

  newscreen := mallocz(p.width * p.height);
  screens[SCN_TMP] := newscreen;

  V_DrawPatch(0, 0, SCN_TMP, p, false);

  V_ScaleBuffer8(pointer(screens[SCN_TMP]), p.width, p.height, 320, 200);
  memcpy(oldscreen, screens[SCN_TMP], 320 * 200);

  memfree(pointer(screens[SCN_TMP]), p.width * p.height);
  screens[SCN_TMP] := oldscreen;
  screendimentions[SCN_TMP].width := 320;
  screendimentions[SCN_TMP].height := 200;
end;

//==============================================================================
//
// V_DrawPatchFullScreenTMP320x200
//
//==============================================================================
procedure V_DrawPatchFullScreenTMP320x200(const pname: string); overload;
var
  p: Ppatch_t;
begin
  p := W_CacheLumpName(pname, PU_STATIC);
  V_DrawPatchFullScreenTMP320x200(p);
  Z_ChangeTag(p, PU_CACHE);
end;

//==============================================================================
//
// V_DrawPatchFullScreenTMP320x200
//
//==============================================================================
procedure V_DrawPatchFullScreenTMP320x200(const lump: integer); overload;
var
  p: Ppatch_t;
begin
  p := W_CacheLumpNum(lump, PU_STATIC);
  V_DrawPatchFullScreenTMP320x200(p);
  Z_ChangeTag(p, PU_CACHE);
end;

//==============================================================================
//
// V_DrawPatchFullScreenTMP426x200
//
//==============================================================================
procedure V_DrawPatchFullScreenTMP426x200(p: Ppatch_t);
var
  oldscreen, newscreen: PByteArray;
begin
  if (p.width = 426) and (p.height = 200) then
  begin
    V_DrawPatch8(0, 0, SCN_TMP426, p, false);
    exit;
  end;

  oldscreen := screens[SCN_TMP426];
  screendimentions[SCN_TMP426].width := p.width;
  screendimentions[SCN_TMP426].height := p.height;

  newscreen := mallocz(p.width * p.height);
  screens[SCN_TMP426] := newscreen;

  V_DrawPatch(0, 0, SCN_TMP426, p, false);

  V_ScaleBuffer8(pointer(screens[SCN_TMP426]), p.width, p.height, 426, 200);
  memcpy(oldscreen, screens[SCN_TMP426], 426 * 200);

  memfree(pointer(screens[SCN_TMP426]), p.width * p.height);
  screens[SCN_TMP426] := oldscreen;
  screendimentions[SCN_TMP426].width := 426;
  screendimentions[SCN_TMP426].height := 200;
end;

//==============================================================================
//
// V_DrawPatchFullScreenTMP426x200
//
//==============================================================================
procedure V_DrawPatchFullScreenTMP426x200(const pname: string); overload;
var
  p: Ppatch_t;
begin
  p := W_CacheLumpName(pname, PU_STATIC);
  V_DrawPatchFullScreenTMP426x200(p);
  Z_ChangeTag(p, PU_CACHE);
end;

//==============================================================================
//
// V_DrawPatchFullScreenTMP426x200
//
//==============================================================================
procedure V_DrawPatchFullScreenTMP426x200(const lump: integer); overload;
var
  p: Ppatch_t;
begin
  p := W_CacheLumpNum(lump, PU_STATIC);
  V_DrawPatchFullScreenTMP426x200(p);
  Z_ChangeTag(p, PU_CACHE);
end;

{$IFDEF DOOM_OR_STRIFE}

//==============================================================================
//
// V_PageDrawer
//
//==============================================================================
procedure V_PageDrawer(const pagename: string);
var
  p: Ppatch_t;
  bigpatch: boolean;
  oldscreen, newscreen: PByteArray;
  oldw, oldh: integer;
begin
  if {$IFNDEF OPENGL}(videomode = vm32bit) and{$ENDIF} useexternaltextures then
    if T_DrawFullScreenPatch(pagename, screen32) then
    begin
      V_FullScreenStretch;
      exit;
    end;
  p := W_CacheLumpName(pagename, PU_STATIC);
  bigpatch := (p.width > 320) or (p.height > 200);
  if bigpatch then
  begin
    oldscreen := screens[SCN_TMP];
    oldw := screendimentions[SCN_TMP].width;
    oldh := screendimentions[SCN_TMP].height;
    screendimentions[SCN_TMP].width := p.width;
    screendimentions[SCN_TMP].height := p.height;

    newscreen := mallocz(p.width * p.height);
    screens[SCN_TMP] := newscreen;

    V_DrawPatch(0, 0, SCN_TMP, p, false);

    if (p.width > SCREENWIDTH) or (p.height > SCREENHEIGHT) then
    begin
      V_ScaleBuffer8(pointer(screens[SCN_TMP]), p.width, p.height, SCREENWIDTH, SCREENHEIGHT);
      screendimentions[SCN_TMP].width := SCREENWIDTH;
      screendimentions[SCN_TMP].height := SCREENHEIGHT;
    end;

    V_CopyRect(0, 0, SCN_TMP, screendimentions[SCN_TMP].width, screendimentions[SCN_TMP].height, 0, 0, SCN_FG, true);

    memfree(pointer(screens[SCN_TMP]), screendimentions[SCN_TMP].width * screendimentions[SCN_TMP].height);
    screens[SCN_TMP] := oldscreen;
    screendimentions[SCN_TMP].width := oldw;
    screendimentions[SCN_TMP].height := oldh;
  end
  else
  begin
    V_DrawPatch(0, 0, SCN_TMP, p, false);
    V_CopyRect(0, 0, SCN_TMP, 320, 200, 0, 0, SCN_FG, true);
  end;
  {$IFDEF DOOM}
  if (p.width <> 426) or (p.height <> 200) then
  {$ENDIF}
  V_FullScreenStretch;
  Z_ChangeTag(p, PU_CACHE);
end;
{$ELSE}

//==============================================================================
//
// V_PageDrawer
//
//==============================================================================
procedure V_PageDrawer(const pagename: string);
{$IFDEF OPENGL}
var
  len: integer;
  lump: integer;
  lmpdata: pointer;
{$ENDIF}
begin
{$IFDEF OPENGL}
  if T_DrawFullScreenPatch(pagename, screen32) then
  begin
    V_FullScreenStretch;
    exit;
  end;
  lump := W_GetNumForName(pagename);
  len := W_LumpLength(lump);
  if len <> 320 * 200 then
    I_DevError('V_PageDrawer(): Lump "%s" has invalid size: %d (does not have 320x200 size).'#13#10, [pagename, len]);

  lmpdata := W_CacheLumpNum(lump, PU_STATIC);
  memcpy(screens[SCN_TMP], lmpdata, len);
  Z_ChangeTag(lmpdata, PU_CACHE);

  V_CopyRect32(0, 0, SCN_TMP, 320, 200, 0, 0, true);

{$ELSE}
  if (videomode = vm32bit) and useexternaltextures then
    if T_DrawFullScreenPatch(pagename, screen32) then
    begin
      V_FullScreenStretch;
      exit;
    end;
  V_CopyRawDataToScreen(SCN_TMP, pagename);
  V_CopyRect(0, 0, SCN_TMP, 320, 200, 0, 0, SCN_FG, true);
{$ENDIF}
  V_FullScreenStretch;
end;
{$ENDIF}

//==============================================================================
// V_DrawBlock8
//
// V_DrawBlock
// Draw a linear block of pixels into the view buffer.
//
//==============================================================================
procedure V_DrawBlock8(x, y: integer; scrn: integer; width, height: integer; src: PByteArray);
var
  dest: PByteArray;
  swidth: integer;
begin
  swidth := V_GetScreenWidth(scrn);
  dest := PByteArray(integer(screens[scrn]) + y * swidth + x);

  while height <> 0 do
  begin
    memcpy(dest, src, width);
    src := PByteArray(integer(src) + width);
    dest := PByteArray(integer(dest) + swidth);
    dec(height);
  end;
end;

//==============================================================================
//
// V_DrawBlock32
//
//==============================================================================
procedure V_DrawBlock32(x, y: integer; width, height: integer; src: PByteArray);
var
  dest: PLongWordArray;
  swidth: integer;
  i: integer;
begin
  swidth := V_GetScreenWidth(SCN_FG);
  dest := @screen32[y * swidth + x];

  while height <> 0 do
  begin
    for i := 0 to width - 1 do
      dest[i] := videopal[src[i]];
    src := PByteArray(integer(src) + width);
    dest := @dest[swidth];
    dec(height);
  end;
end;

//==============================================================================
//
// V_DrawBlock
//
//==============================================================================
procedure V_DrawBlock(x, y: integer; scrn: integer; width, height: integer; src: PByteArray);
begin
  if {$IFNDEF OPENGL}(videomode = vm32bit) and{$ENDIF} (scrn = SCN_FG) then
    V_DrawBlock32(x, y, width, height, src)
  else
    V_DrawBlock8(x, y, scrn, width, height, src)
end;

//==============================================================================
//
// V_DrawLongBlock
//
//==============================================================================
procedure V_DrawLongBlock(x, y: integer; width, height: integer; src: PLongWordArray);
var
  dest: PLongWordArray;
  swidth: integer;
begin
  swidth := V_GetScreenWidth(SCN_FG);
  dest := PLongWordArray(@screen32[y * swidth + x]);

  while height <> 0 do
  begin
    memcpy(dest, src, width * SizeOf(LongWord));
    src := PLongWordArray(@src[width]);
    dest := PLongWordArray(@dest[swidth]);
    dec(height);
  end;
end;

//==============================================================================
//
// V_GetBlock
// Gets a linear block of pixels from the view buffer.
//
//==============================================================================
procedure V_GetBlock(x, y: integer; scrn: integer; width, height: integer; dest: PByteArray);
var
  src: PByteArray;
  swidth: integer;
begin
  swidth := V_GetScreenWidth(scrn);

  src := PByteArray(integer(screens[scrn]) + y * swidth + x);

  while height <> 0 do
  begin
    memcpy(dest, src, width);
    src := PByteArray(integer(src) + swidth);
    dest := PByteArray(integer(dest) + width);
    dec(height);
  end;
end;

//==============================================================================
//
// V_CalcPreserveTables
//
//==============================================================================
procedure V_CalcPreserveTables;
var
  i: integer;
begin
  // initialize translation tables
  for i := 0 to 319 do
    preserveX[i] := trunc(i * {$IFDEF OPENGL}V_GetScreenWidth(SCN_FG){$ELSE}SCREENWIDTH{$ENDIF} / 320);

  for i := 0 to 425 do
    preserveX426[i] := trunc(i * {$IFDEF OPENGL}V_GetScreenWidth(SCN_FG){$ELSE}SCREENWIDTH{$ENDIF} / 426);

  for i := 0 to 199 do
    preserveY[i] := trunc(i * {$IFDEF OPENGL}V_GetScreenHeight(SCN_FG){$ELSE}SCREENHEIGHT{$ENDIF} / 200);

  if {$IFDEF OPENGL}V_GetScreenWidth(SCN_FG){$ELSE}SCREENWIDTH{$ENDIF} mod 320 = 0 then
    widthintmultiplier := {$IFDEF OPENGL}V_GetScreenWidth(SCN_FG){$ELSE}SCREENWIDTH{$ENDIF} div 320
  else
    widthintmultiplier := 0;

  if {$IFDEF OPENGL}V_GetScreenHeight(SCN_FG){$ELSE}SCREENHEIGHT{$ENDIF} mod 200 = 0 then
    heightintmultiplier := {$IFDEF OPENGL}V_GetScreenHeight(SCN_FG){$ELSE}SCREENHEIGHT{$ENDIF} div 200
  else
    heightintmultiplier := 0;

end;

//==============================================================================
//
// V_Init
//
// V_SetPalette
//
//==============================================================================
procedure V_SetPalette(const palette: PByteArray);
var
  dest: PLongWord;
{$IFNDEF OPENGL}
  {$IFDEF HEXEN}
  destsk: PLongWord;
  {$ENDIF}
{$ENDIF}
  src: PByteArray;
  curgamma: PByteArray;
begin
  dest := @videopal[0];
{$IFNDEF OPENGL}
  {$IFDEF HEXEN}
  destsk := @skvideopal[0];
  {$ENDIF}
{$ENDIF}
  src := palette;
  curgamma := @gammatable[usegamma];
  while integer(src) < integer(@palette[256 * 3]) do
  begin
    dest^ := (LongWord(curgamma[src[0]]) shl 16) or
             (LongWord(curgamma[src[1]]) shl 8) or
             (LongWord(curgamma[src[2]])) or $FF000000;
    {$IFNDEF OPENGL}
    {$IFDEF HEXEN}
    destsk^ := dest^;
    inc(destsk);
    {$ENDIF}
    {$ENDIF}
    inc(dest);
    src := PByteArray(integer(src) + 3);
  end;
{$IFNDEF OPENGL}
{$IFDEF HEXEN}
  skvideopal[0] := 0;
{$ENDIF}
{$ENDIF}
{$IFDEF DOOM_OR_STRIFE}
  videopal[0] := videopal[0] and $FFFFFF;
{$ENDIF}
  recalctables32needed := true;
  needsbackscreen := true; // force background redraw
{$IFNDEF OPENGL}
{$IFDEF DOOM_OR_STRIFE}
  V_CalcColorMapPalette;
{$ENDIF}
{$ENDIF}
end;

{$IFNDEF OPENGL}
{$IFDEF DOOM_OR_STRIFE}

//==============================================================================
//
// V_CalcColorMapPalette
//
//==============================================================================
procedure V_CalcColorMapPalette;
var
  p: pointer;
  dest: PLongWord;
  src: PByteArray;
  curgamma: PByteArray;
  palette: PByteArray;
  i: integer;
begin
  if customcolormap = nil then
  begin
    memmove(@cvideopal, @curpal, SizeOf(cvideopal));
    exit;
  end;

  p := W_CacheLumpNum(lu_palette, PU_STATIC);
  palette := PByteArray(integer(p) + st_palette * 768);

  dest := @cvideopal[0];
  curgamma := @gammatable[usegamma];

  i := 0;
  while i < 256 do
  begin
    src := @palette[3 * customcolormap.colormap[i]];
    dest^ := (LongWord(curgamma[src[0]]) shl 16) or
             (LongWord(curgamma[src[1]]) shl 8) or
             (LongWord(curgamma[src[2]])) or $FF000000;
    inc(dest);
    inc(i);
  end;

  cvideopal[0] := cvideopal[0] and $FFFFFF;
  recalctables32needed := true;

  Z_ChangeTag(p, PU_CACHE);
end;
{$ENDIF}
{$ENDIF}

//==============================================================================
//
// V_InitDefaultPalette
//
//==============================================================================
procedure V_InitDefaultPalette;
var
  playpal: PByteArray;
  i: integer;
begin
  if default_palette <> nil then
    exit;

  default_palette := Z_Malloc(256 * SizeOf(LongWord), PU_STATIC, nil);

  playpal := V_ReadPalette(PU_STATIC);
  for i := 0 to 255 do
    default_palette[i] := _SHL(playpal[i * 3], 16) or _SHL(playpal[i * 3 + 1], 8) or playpal[i * 3 + 2];
end;

var
  vsize: integer = 0;

//==============================================================================
//
// V_Init
//
//==============================================================================
procedure V_Init;
var
  i: integer;
  base: PByteArray;
  st: integer;
  pal: PByteArray;
begin
  SCREENWIDTH32PITCH := SCREENWIDTH * SizeOf(LongWord);
  pal := V_ReadPalette(PU_STATIC);
  V_SetPalette(pal);
  Z_ChangeTag(pal, PU_CACHE);
  V_InitDefaultPalette;
{$IFDEF OPENGL}
  if SCREENWIDTH < 1024 then
  begin
    if SCREENWIDTH <= SCREENHEIGHT then
    begin
      GLDRAWWIDTH := 512;
      GLDRAWHEIGHT := 512;
    end
    else
    begin
      GLDRAWWIDTH := 512;
      GLDRAWHEIGHT := round(SCREENHEIGHT * 512 / SCREENWIDTH);
    end;
    GLDRAWTEXWIDTH := 512;
    GLDRAWTEXHEIGHT := 512;
  end
  else
  begin
    if SCREENWIDTH <= SCREENHEIGHT then
    begin
      GLDRAWWIDTH := 1024;
      GLDRAWHEIGHT := 1024;
    end
    else
    begin
      GLDRAWWIDTH := 1024;
      GLDRAWHEIGHT := round(SCREENHEIGHT * 1024 / SCREENWIDTH);
    end;
    GLDRAWTEXWIDTH := 1024;
    GLDRAWTEXHEIGHT := 1024;
  end;
{$ENDIF}
  {$IFDEF DOOM_OR_STRIFE}
  for i := SCN_FG to SCN_ST do
  {$ELSE}
  for i := SCN_FG to SCN_TMP do
  {$ENDIF}
  begin
    if FIXED_DIMENTIONS[i].width = -1 then
    {$IFDEF OPENGL}
      screendimentions[i].width := GLDRAWWIDTH
    {$ELSE}
      screendimentions[i].width := SCREENWIDTH
    {$ENDIF}
    else
      screendimentions[i].width := FIXED_DIMENTIONS[i].width;
    if FIXED_DIMENTIONS[i].height = -1 then
    {$IFDEF OPENGL}
      screendimentions[i].height := GLDRAWHEIGHT
    {$ELSE}
      screendimentions[i].height := SCREENHEIGHT
    {$ENDIF}
    else
      screendimentions[i].height := FIXED_DIMENTIONS[i].height;
  end;
  // stick these in low dos memory on PCs
  vsize := V_ScreensSize;
  base := malloc(vsize);

  st := 0;
  {$IFDEF DOOM_OR_STRIFE}
  for i := SCN_FG to SCN_ST do
  {$ELSE}
  for i := SCN_FG to SCN_TMP do
  {$ENDIF}
  begin
    screens[i] := @base[st];
    st := st + screendimentions[i].width * screendimentions[i].height;
  end;

  V_CalcPreserveTables;

end;

//==============================================================================
//
// V_ReInit
//
//==============================================================================
procedure V_ReInit;
begin
  V_ShutDown;
  V_Init;
end;

//==============================================================================
//
// V_ShutDown
//
//==============================================================================
procedure V_ShutDown;
var
  base: pointer;
begin
  base := screens[SCN_FG];
  memfree(base, vsize);
end;

//==============================================================================
//
// V_ScreensSize
//
//==============================================================================
function V_ScreensSize(const scrn: integer = -1): integer;
var
  i: integer;
  w, h: integer;
  il, ih: integer;
begin
  if scrn = -1 then
  begin
    il := SCN_FG;
    ih := {$IFDEF DOOM_OR_STRIFE}SCN_ST{$ELSE}SCN_TMP{$ENDIF};
  end
  else
  begin
    il := scrn;
    ih := scrn;
  end;

  result := 0;
  for i := il to ih do
  begin
    w := FIXED_DIMENTIONS[i].width;
    if w = -1 then
      w := SCREENWIDTH;
    h := FIXED_DIMENTIONS[i].height;
    if h = -1 then
      h := SCREENHEIGHT;
    result := result + w * h * FIXED_DIMENTIONS[i].depth;
  end;
end;

//==============================================================================
//
// V_TileScreen8
//  Global use screen filler with patch or flat
//
//==============================================================================
function V_TileScreen8(const lumpname: string; const dstscn: integer): boolean;
var
  lump: integer;
  sz: integer;
  flat: Pointer;
  isflat: boolean;
  src: packed array[0..4095] of byte;
  dest: integer;
  ovr: TOverlayDrawer;
  x, y: integer;
  xstart, ystart: integer;
  p: Ppatch_t;
begin
  result := false;
  lump := W_CheckNumForName(lumpname);
  if lump >= 0 then
  begin
    sz := W_LumpLength(lump);
    flat := W_CacheLumpNum(R_GetLumpForFlat(R_FlatNumForName(lumpname)), PU_STATIC);
    isflat := R_ShrinkFlatTo64x64(flat, sz, @src);
    Z_ChangeTag(flat, PU_CACHE);
    if isflat then
    begin
      dest := 0;

      for y := 0 to 200 - 1 do
      begin
        for x := 0 to (320 div 64) - 1 do
        begin
          memcpy(@screens[dstscn, dest], @src[_SHL(y and 63, 6)], 64);
          dest := dest + 64;
        end;

        if 320 and 63 <> 0 then
        begin
          memcpy(@screens[dstscn, dest], @src[_SHL(y and 63, 6)], 320 and 63);
          dest := dest + (320 and 63);
        end;

        result := true;
      end;
    end
    else
    begin
      p := W_CacheLumpNum(lump, PU_STATIC);
      if T_IsValidPatchImage(p, sz) then
        if (p.width > 0) and (p.height > 0) then
        begin
          ovr := TOverlayDrawer.Create;

          if p.leftoffset = 0 then
            xstart := 0
          else
            xstart := -p.width;
          if p.topoffset = 0 then
            ystart := 0
          else
            ystart := -p.height;

          x := xstart;
          while x < 320 do
          begin
            y := ystart;
            while y < 200 do
            begin
              ovr.AddPatch($FF, lumpname, x, y);
              y := y + p.height;
            end;
            x := x + p.width;
          end;

          ovr.DrawDrawers;
          memcpy(@screens[dstscn, 0], ovr.overlayscreen, 320 * 200);
          ovr.Free;

          result := true;
        end;
    end;
  end;
end;

//==============================================================================
//
// V_FindAproxColorIndex
//
// JVAL: Calculates the euclidian square distance of a given color from all
//       pal items and return the nearest
//
//==============================================================================
function V_FindAproxColorIndex(const pal: PLongWordArray; const c: LongWord;
  const start: integer = 0; const finish: integer = 255): integer;
var
  r, g, b: integer;
  rc, gc, bc: integer;
  dr, dg, db: integer;
  i: integer;
  cc: LongWord;
  dist: LongWord;
  mindist: LongWord;
begin
  r := c and $FF;
  g := (c shr 8) and $FF;
  b := (c shr 16) and $FF;
  result := start;
  mindist := LongWord($ffffffff);
  for i := start to finish do
  begin
    cc := pal[i];
    rc := cc and $FF;
    gc := (cc shr 8) and $FF;
    bc := (cc shr 16) and $FF;
    dr := r - rc;
    dg := g - gc;
    db := b - bc;
    dist := dr * dr + dg * dg + db * db;
    if dist < mindist then
    begin
      result := i;
      if dist = 0 then
        exit
      else
        mindist := dist;
    end;
  end;
end;

//==============================================================================
//
// V_FindAproxColorIndexExcluding
//
//==============================================================================
function V_FindAproxColorIndexExcluding(const pal: PLongWordArray; const c: LongWord;
  const start: integer = 0; const finish: integer = 255; const exclude: integer = -1): integer;
var
  r, g, b: integer;
  rc, gc, bc: integer;
  dr, dg, db: integer;
  i: integer;
  cc: LongWord;
  dist: LongWord;
  mindist: LongWord;
begin
  r := c and $FF;
  g := (c shr 8) and $FF;
  b := (c shr 16) and $FF;
  result := start;
  mindist := LongWord($ffffffff);
  for i := start to finish do
  begin
    if i = exclude then
      continue;
    cc := pal[i];
    rc := cc and $FF;
    gc := (cc shr 8) and $FF;
    bc := (cc shr 16) and $FF;
    dr := r - rc;
    dg := g - gc;
    db := b - bc;
    dist := dr * dr + dg * dg + db * db;
    if dist < mindist then
    begin
      result := i;
      if dist = 0 then
        exit
      else
        mindist := dist;
    end;
  end;
end;

{$IFDEF OPENGL}

//==============================================================================
//
// V_FullScreenStretch
//
//==============================================================================
procedure V_FullScreenStretch;
var
  x, y: integer;
  bufl, pl: PLongWordArray;
  start, stop: integer;
  pct: integer;
  w, h: integer;
begin
  if not intermissionstretch then
    exit;

  w := V_GetScreenWidth(SCN_FG);
  h := V_GetScreenHeight(SCN_FG);

  pct := round((1 - (4 / 3) / (w / h)) * 100);
  if (pct <= 0) or (pct >= 100) then
    exit;

  start := (pct div 2) * w div 100;
  stop := w - start;

  bufl := malloc(w * SizeOf(LongWord));

  for x := 0 to h - 1 do
  begin
    pl := @screen32[x * w];
    for y := 0 to start - 1 do
      bufl[y] := $FF000000;
    for y := start to stop do
      bufl[y] := pl[ibetween(w * (y - start) div (stop - start + 1), 0, w - 1)];
    for y := stop + 1 to w - 1 do
      bufl[y] := $FF000000;
    memcpy(pl, bufl, w * SizeOf(LongWord));
  end;

  memfree(pointer(bufl), w * SizeOf(LongWord));
end;

{$ELSE}

//==============================================================================
//
// V_FullScreenStretch
//
//==============================================================================
procedure V_FullScreenStretch;
var
  x, y: integer;
  bufb, pb: PByteArray;
  bufl, pl: PLongWordArray;
  start, stop: integer;
  pct: integer;
begin
  if not intermissionstretch then
    exit;

  pct := round((1 - (4 / 3) / (SCREENWIDTH / SCREENHEIGHT)) * 100);
  if (pct <= 0) or (pct >= 100) then
    exit;

  start := (pct div 2) * SCREENWIDTH div 100;
  stop := SCREENWIDTH - start;

  if videomode = vm32bit then
  begin
    bufl := mallocz(SCREENWIDTH * SizeOf(LongWord));

    for x := 0 to SCREENHEIGHT - 1 do
    begin
      pl := @screen32[x * SCREENWIDTH];
      for y := start to stop do
        bufl[y] := pl[ibetween(SCREENWIDTH * (y - start) div (stop - start + 1), 0, SCREENWIDTH - 1)];
      memcpy(pl, bufl, SCREENWIDTH * SizeOf(LongWord));
    end;

    memfree(pointer(bufl), SCREENWIDTH * SizeOf(LongWord));
  end
  else
  begin
    bufb := malloc(SCREENWIDTH * SizeOf(byte));

    for x := 0 to start - 1 do
      bufb[x] := aprox_black;
    for x := stop + 1 to SCREENWIDTH - 1 do
      bufb[x] := aprox_black;

    for x := 0 to SCREENHEIGHT - 1 do
    begin
      pb := @screens[SCN_FG][x * SCREENWIDTH];
      for y := start to stop do
        bufb[y] := pb[ibetween(SCREENWIDTH * (y - start) div (stop - start + 1), 0, SCREENWIDTH - 1)];
      memcpy(pb, bufb, SCREENWIDTH * SizeOf(byte));
    end;

    memfree(pointer(bufb), SCREENWIDTH * SizeOf(byte));
  end;
end;
{$ENDIF}

end.

