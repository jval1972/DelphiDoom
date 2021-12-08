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

unit r_draw;

interface

uses
  d_delphi,
  doomdef,
  r_defs,
// Needs access to LFB (guess what).
  v_video;

procedure R_VideoErase(const ofs: integer; const count: integer);

procedure R_VideoBlanc(const scn: integer; const ofs: integer; const count: integer; const black: byte = 0);

procedure R_PlayerViewBlanc(const black: byte);

procedure R_InitBuffer(width, height: integer);

// Initialize color translation tables,
//  for player rendering etc.
procedure R_InitTranslationTables;

// Rendering function.
procedure R_FillBackScreen;

// If the view size is not full screen, draws a border around it.
procedure R_DrawViewBorder;

// Draw disk busy patch
procedure R_DrawDiskBusy;

var
  displaydiskbusyicon: boolean = true;

  translationtables: PByteArray;
  tinttable: PByteArray;
  dc_translation: PByteArray;

  viewwidth: integer;
  viewheight: integer;
  scaledviewwidth: integer;

  viewwindowx: integer;
  viewwindowy: integer;

  borderneedsrefresh: boolean = false;

//
// All drawing to the view buffer is accomplished in this file.
// The other refresh files only know about ccordinates,
//  not the architecture of the frame buffer.
// Conveniently, the frame buffer is a linear one,
//  and we need only the base address,
//  and the total size == width*height*depth/8.,
//

var
  ylookup: array[0..MAXHEIGHT - 1] of PByteArray;
  ylookupl: array[0..MAXHEIGHT - 1] of PLongWordArray;
  columnofs: array[0..MAXWIDTH - 1] of integer;

type
  crange_idx_e = (
    CR_BRICK,   //0
    CR_TAN,     //1
    CR_GRAY,    //2
    CR_GREEN,   //3
    CR_BROWN,   //4
    CR_GOLD,    //5
    CR_RED,     //6
    CR_BLUE,    //7
    CR_ORANGE,  //8
    CR_YELLOW,  //9
    CR_BLUE2,   //10
    CR_LIMIT    //11
  );

var
  colorregions: array[0..Ord(CR_LIMIT) - 1] of PByteArray;

implementation

uses
  am_map,
  i_system,
  m_argv,
  r_hires,
  w_wad,
  z_zone,
  sb_bar,
{$IFDEF OPENGL}
  gl_render,
{$ENDIF}
// State.
  doomstat,
  v_data;

//
// R_InitTranslationTables
// Creates the translation tables to map
//  the green color ramp to gray, brown, red.
// Assumes a given structure of the PLAYPAL.
// Could be read from a lump instead.
//
procedure R_InitTranslationTables;
var
  i{$IFDEF OPENGL}, j{$ENDIF}: integer;
  transLump: PByteArray;
  trlmp: integer;
  idx: integer;
{$IFDEF OPENGL}
  lump: integer;
{$ENDIF}
begin
  tinttable := W_CacheLumpName('TINTTAB', PU_STATIC);

  translationtables := Z_Malloc(256 * 3 * (MAXPLAYERS - 1), PU_STATIC, nil);

  trlmp := W_GetNumForName('TRANTBL0');
  idx := trlmp;
  for i := 0 to 3 * (MAXPLAYERS - 1) - 1 do
  begin
    transLump := W_CacheLumpNum(idx, PU_STATIC);
    memcpy(@translationtables[i * 256], transLump, 256);
    Z_Free(transLump);
    // JVAL: Circle thru translation tables
    inc(idx);
    if Pos('TRANTBL', strupper(char8tostring(W_GetNameForNum(idx)))) = 0 then
      idx := trlmp;
  end;

{$IFDEF OPENGL}
  // JVAL: Initialize ColorRegions
  lump := W_CheckNumForName('CR_START');
  for i := 0 to Ord(CR_LIMIT) - 1 do
    colorregions[i] := Z_Malloc(256, PU_STATIC, nil);
  if lump = -1 then
  begin
    I_Warning('Colormap extensions not found, using default translations'#13#10);
    for i := 0 to Ord(CR_LIMIT) - 1 do
      for j := 0 to 255 do
        colorregions[i][j] := j;
  end
  else
  begin
    for i := 0 to Ord(CR_LIMIT) - 1 do
    begin
      inc(lump);
      W_ReadLump(lump, colorregions[i]);
    end;
  end;
{$ENDIF}
end;

//
// R_InitBuffer
// Creats lookup tables that avoid
//  multiplies and other hazzles
//  for getting the framebuffer address
//  of a pixel to draw.
//
procedure R_InitBuffer(width, height: integer);
var
  i: integer;
begin
  // Handle resize,
  //  e.g. smaller view windows
  //  with border and/or status bar.
  viewwindowx := (SCREENWIDTH - width) div 2;

  // Column offset. For windows.
  for i := 0 to width - 1 do
    columnofs[i] := viewwindowx + i;

  // Same with base row offset.
  if width = SCREENWIDTH then
  begin
    viewwindowy := 0;
  end
  else
  begin
  {$IFDEF OPENGL}
    viewwindowy := (trunc(SB_Y * SCREENHEIGHT / 200) - height) div 2;
  {$ELSE}
    viewwindowy := (V_PreserveY(SB_Y) - height) div 2;
  {$ENDIF}
  end;

{$IFNDEF OPENGL}
  // Preclaculate all row offsets.
  for i := 0 to height - 1 do
  begin
    ylookup[i] := PByteArray(integer(screens[SCN_FG]) + (i + viewwindowy) * SCREENWIDTH);
    ylookupl[i] := PLongWordArray(@screen32[(i + viewwindowy) * SCREENWIDTH]);
  end;
{$ENDIF}
end;

procedure R_ScreenBlanc(const scn: integer; const black: byte = 0);
var
  x, i: integer;
begin
  x := viewwindowy * SCREENWIDTH + viewwindowx;
  for i := 0 to viewheight - 1 do
  begin
    R_VideoBlanc(scn, x, scaledviewwidth, black);
    inc(x, SCREENWIDTH);
  end;
end;


//
// R_FillBackScreen
// Fills the back screen with a pattern
//  for variable screen sizes
// Also draws a beveled edge.
//
procedure R_FillBackScreen;
{$IFNDEF OPENGL}
var
  src: PByteArray;
  dest: PByteArray;
  x: integer;
  y: integer;
  patch: Ppatch_t;
  tviewwindowx: integer;
  tviewwindowy: integer;
  tviewheight: integer;
  tscaledviewwidth: integer;
{$ENDIF}
begin
  needsbackscreen := false;

  if scaledviewwidth = SCREENWIDTH then
    exit;

{$IFDEF OPENGL}
  gld_DrawBackground('F_022');
{$ELSE}
  src := W_CacheLumpName('F_022', PU_CACHE);

  dest := screens[SCN_TMP];

  for y := 0 to 200 - SB_HEIGHT do
  begin
    for x := 0 to 320 div 64 - 1 do
    begin
      memcpy(dest, PByteArray(integer(src) + _SHL(y and 63, 6)), 64);
      dest := @dest[64];
    end;
    if 320 and 63 <> 0 then
    begin
      memcpy(dest, PByteArray(integer(src) + _SHL(y and 63, 6)), 320 and 63);
      dest := @dest[64];
    end;
  end;

  tviewwindowx := viewwindowx * 320 div SCREENWIDTH + 1;
  tviewwindowy := viewwindowy * 200 div SCREENHEIGHT + 1;
  tviewheight := viewheight * 200 div SCREENHEIGHT - 2;
  tscaledviewwidth := scaledviewwidth * 320 div SCREENWIDTH - 2;

  patch := W_CacheLumpName('bordt', PU_STATIC);
  x := 0;
  while x < tscaledviewwidth do
  begin
    V_DrawPatch(tviewwindowx + x, tviewwindowy - 4, SCN_TMP, patch, false);
    x := x + 16;
  end;
  Z_ChangeTag(patch, PU_CACHE);

  patch := W_CacheLumpName('bordb', PU_STATIC);
  x := 0;
  while x < tscaledviewwidth do
  begin
    V_DrawPatch(tviewwindowx + x, tviewwindowy + tviewheight, SCN_TMP, patch, false);
    x := x + 16;
  end;
  Z_ChangeTag(patch, PU_CACHE);

  patch := W_CacheLumpName('bordl', PU_STATIC);
  y := 0;
  while y < tviewheight do
  begin
    V_DrawPatch(tviewwindowx - 4, tviewwindowy + y, SCN_TMP, patch, false);
    y := y + 16;
  end;
  Z_ChangeTag(patch, PU_CACHE);

  patch := W_CacheLumpName('bordr', PU_STATIC);
  y := 0;
  while y < tviewheight do
  begin
    V_DrawPatch(tviewwindowx + tscaledviewwidth, tviewwindowy + y, SCN_TMP, patch, false);
    y := y + 16;
  end;
  Z_ChangeTag(patch, PU_CACHE);

  // Draw beveled edge.
  V_DrawPatch(tviewwindowx - 4, tviewwindowy - 4, SCN_TMP,
    'bordtl', false);

  V_DrawPatch(tviewwindowx + tscaledviewwidth, tviewwindowy - 4, SCN_TMP,
    'bordtr', false);

  V_DrawPatch(tviewwindowx - 4, tviewwindowy + tviewheight, SCN_TMP,
    'bordbl', false);

  V_DrawPatch(tviewwindowx + tscaledviewwidth, tviewwindowy + tviewheight, SCN_TMP,
    'bordbr', false);

  V_RemoveTransparency(SCN_TMP, 0, -1);
  V_CopyRect(0, 0, SCN_TMP, V_GetScreenWidth(SCN_TMP), V_GetScreenHeight(SCN_TMP), 0, 0, SCN_BG, true);

  R_ScreenBlanc(SCN_BG);
  x := V_PreserveY(SB_Y) * SCREENWIDTH;
  R_VideoBlanc(SCN_BG, x, (SCREENHEIGHT - V_PreserveY(SB_Y)) * SCREENWIDTH);
{$ENDIF}
end;

//
// Copy a screen buffer.
//
procedure R_VideoErase(const ofs: integer; const count: integer);
var
  i: integer;
  src: PByte;
  dest: PLongWord;
begin
  // LFB copy.
  // This might not be a good idea if memcpy
  //  is not optiomal, e.g. byte by byte on
  //  a 32bit CPU, as GNU GCC/Linux libc did
  //  at one point.
{$IFNDEF OPENGL}
  if videomode = vm32bit then
  begin
{$ENDIF}
    src := PByte(integer(screens[SCN_BG]) + ofs);
    dest := @screen32[ofs];
    for i := 1 to count do
    begin
      dest^ := videopal[src^];
      inc(dest);
      inc(src);
    end;
{$IFNDEF OPENGL}
  end
  else
    memcpy(Pointer(integer(screens[SCN_FG]) + ofs), Pointer(integer(screens[SCN_BG]) + ofs), count);
{$ENDIF}
end;

procedure R_VideoBlanc(const scn: integer; const ofs: integer; const count: integer; const black: byte = 0);
var
  start: PByte;
  lstrart: PLongWord;
  i: integer;
  lblack: LongWord;
begin
  if {$IFNDEF OPENGL}(videomode = vm32bit) and{$ENDIF} (scn = SCN_FG) then
  begin
    lblack := curpal[black];
    lstrart := @screen32[ofs];
    for i := 0 to count -1 do
    begin
      lstrart^ := lblack;
      inc(lstrart);
    end;
  end
  else
  begin
    start := @screens[scn][ofs];
    memset(start, black, count);
  end;
end;

procedure R_PlayerViewBlanc(const black: byte);
begin
  R_ScreenBlanc(SCN_FG, black);
end;

//
// R_DrawViewBorder
// Draws the border around the view
//  for different size windows?
//
procedure R_DrawViewBorder;
begin
  if scaledviewwidth < SCREENWIDTH then
    if (gamestate = GS_LEVEL) and (amstate <> am_only) then
      V_CopyScreenTransparent(SCN_BG, SCN_FG);
end;

var
  disklump: integer = -2;
  diskpatch: Ppatch_t = nil;

procedure R_DrawDiskBusy;
begin
  if not displaydiskbusyicon then
    exit;

// Draw disk busy patch
  if disklump = -2 then
  begin
    if M_CheckParmCDROM then
      disklump := W_CheckNumForName('STCDROM');
    if disklump < 0 then
      disklump := W_CheckNumForName('STDISK');
    if disklump >= 0 then
      diskpatch := W_CacheLumpNum(disklump, PU_STATIC);
  end;

  if diskpatch <> nil then
    V_DrawPatch(318 - diskpatch.width, 2, SCN_FG,
      diskpatch, true);
end;

end.
