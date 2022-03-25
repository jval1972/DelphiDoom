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
//  System specific interface stuff.
//  Rendering main loop and setup functions,
//  utility functions (BSP, geometry, trigonometry).
//  See tables.c, too.
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit r_main;

interface

uses
  d_delphi,
  doomdef,
  d_player,
  m_fixed,
  tables,
  r_data,
  {$IFNDEF OPENGL}
  r_sprite,
  {$ENDIF}
  r_defs;

const
//
// Lighting LUT.
// Used for z-depth cuing per column/row,
//  and other lighting effects (sector ambient, flash).
//

// Lighting constants.
// Now why not 32 levels here?
  LIGHTSEGSHIFT = 4;
  LIGHTLEVELS = (256 div (1 shl LIGHTSEGSHIFT));

  MAXLIGHTSCALE = 48;
  LIGHTSCALESHIFT = 12;
  LIGHTSCALEUNIT = 1 shl LIGHTSCALESHIFT;
  HLL_MAXLIGHTSCALE = MAXLIGHTSCALE * 64;

  MAXLIGHTZ = 128;
  LIGHTZSHIFT = 20;

  HLL_MAXLIGHTZ = MAXLIGHTZ * 64; // Hi resolution light level for z depth
  HLL_LIGHTZSHIFT = 12;
  HLL_LIGHTSCALESHIFT = 3;
  HLL_ZDISTANCESHIFT = 14;

  LIGHTDISTANCESHIFT = 12;

// Colormap constants
// Number of diminishing brightness levels.
// There a 0-31, i.e. 32 LUT in the COLORMAP lump.
// Index of the special effects (INVUL inverse) map.
  INVERSECOLORMAP = 32;

  DISTMAP = 2;

var
  forcecolormaps: boolean;
  use32bitfuzzeffect: boolean;
  diher8bittransparency: boolean;

//==============================================================================
// R_ApplyColormap
//
// Utility functions.
//
//==============================================================================
procedure R_ApplyColormap(const ofs, count: integer; const scrn: integer; const cmap: integer);

//==============================================================================
//
// R_PointOnSide
//
//==============================================================================
function R_PointOnSide(const x: fixed_t; const y: fixed_t; const node: Pnode_t): boolean;

//==============================================================================
//
// R_PointOnSideVanilla
//
//==============================================================================
function R_PointOnSideVanilla(const x: fixed_t; const y: fixed_t; const node: Pvanillanode_t): boolean;

//==============================================================================
//
// R_PointOnSegSide
//
//==============================================================================
function R_PointOnSegSide(x: fixed_t; y: fixed_t; line: Pseg_t): boolean;

//==============================================================================
//
// R_PointOnLineSide
//
//==============================================================================
function R_PointOnLineSide(x: fixed_t; y: fixed_t; line: Pline_t): boolean;

//==============================================================================
//
// R_PointToAngle
//
//==============================================================================
function R_PointToAngle(x: fixed_t; y: fixed_t): angle_t;

//==============================================================================
//
// R_PointToAngleEx
//
//==============================================================================
function R_PointToAngleEx(x: fixed_t; y: fixed_t): angle_t;

//==============================================================================
//
// R_PointToAngle2
//
//==============================================================================
function R_PointToAngle2(const x1: fixed_t; const y1: fixed_t; const x2: fixed_t; const y2: fixed_t): angle_t;

//==============================================================================
//
// R_PointToDist
//
//==============================================================================
function R_PointToDist(const x: fixed_t; const y: fixed_t): fixed_t;

//==============================================================================
//
// R_PointInSubsectorClassic
//
//==============================================================================
function R_PointInSubsectorClassic(const x: fixed_t; const y: fixed_t): Psubsector_t;

//==============================================================================
//
// R_PointInSubsector
//
//==============================================================================
function R_PointInSubsector(const x: fixed_t; const y: fixed_t): Psubsector_t;

//==============================================================================
//
// R_AddPointToBox
//
//==============================================================================
procedure R_AddPointToBox(const x: integer; const y: integer; box: Pfixed_tArray);

//==============================================================================
// R_RenderPlayerView
//
// REFRESH - the actual rendering functions.
//
// Called by G_Drawer.
//
//==============================================================================
procedure R_RenderPlayerView(player: Pplayer_t);

//==============================================================================
// R_Init
//
// Called by startup code.
//
//==============================================================================
procedure R_Init;

//==============================================================================
//
// R_ShutDown
//
//==============================================================================
procedure R_ShutDown;

//==============================================================================
// R_SetViewSize
//
// Called by M_Responder.
//
//==============================================================================
procedure R_SetViewSize;

//==============================================================================
//
// R_ExecuteSetViewSize
//
//==============================================================================
procedure R_ExecuteSetViewSize;

//==============================================================================
//
// R_SetViewAngleOffset
//
//==============================================================================
procedure R_SetViewAngleOffset(const angle: angle_t);

//==============================================================================
//
// R_FullStOn
//
//==============================================================================
function R_FullStOn: boolean;

var
  {$IFNDEF OPENGL}
  basebatchcolfunc: PProcedure;
  batchcolfunc: PProcedure;
  batchfuzzcolfunc: PProcedure;
  batchlightcolfunc: PProcedure;
  batchwhitelightcolfunc: PProcedure;
  batchredlightcolfunc: PProcedure;
  batchgreenlightcolfunc: PProcedure;
  batchbluelightcolfunc: PProcedure;
  batchyellowlightcolfunc: PProcedure;
  batchtranscolfunc: PProcedure;
  batchtaveragecolfunc: PProcedure;
  batchtalphacolfunc: PProcedure;
  batchaddcolfunc: PProcedure;
  batchsubtractcolfunc: PProcedure;

  // JVAL: Multithreading sprite funcs
  basebatchcolfunc_mt: spritefunc_t;
  batchcolfunc_mt: spritefunc_t;
  batchtaveragecolfunc_mt: spritefunc_t;
  batchtalphacolfunc_mt: spritefunc_t;
  batchaddcolfunc_mt: spritefunc_t;
  batchsubtractcolfunc_mt: spritefunc_t;
  maskedcolfunc_mt: spritefunc_t;
  colfunc_mt: spritefunc_t;
  averagecolfunc_mt: spritefunc_t;
  alphacolfunc_mt: spritefunc_t;
  addcolfunc_mt: spritefunc_t;
  addcolfunc_smallstep_mt: spritefunc_t;
  subtractcolfunc_mt: spritefunc_t;

  colfunc: PProcedure;
  wallcolfunc: PProcedure;
  basewallcolfunc: PProcedure;
  tallwallcolfunc: PProcedure;
  skycolfunc: PProcedure;
  transcolfunc: PProcedure;
  averagecolfunc: PProcedure;
  alphacolfunc: PProcedure;
  addcolfunc: PProcedure;
  addcolfunc_smallstep: PProcedure;
  subtractcolfunc: PProcedure;
  maskedcolfunc: PProcedure;
  maskedcolfunc2: PProcedure; // For hi res textures
  fuzzcolfunc: PProcedure;
  lightcolfunc: PProcedure;
  whitelightcolfunc: PProcedure;
  redlightcolfunc: PProcedure;
  greenlightcolfunc: PProcedure;
  bluelightcolfunc: PProcedure;
  yellowlightcolfunc: PProcedure;
  spanfunc: PProcedure;
  basespanfunc: PProcedure;
  ripplespanfunc: PProcedure;

  // JVAL: Slopes
  slopefunc: PProcedure;
  baseslopefunc: PProcedure;
  rippleslopefunc: PProcedure;

  // JVAL: Multithreading flats
  spanfuncMT: PPointerParmProcedure;
  basespanfuncMT: PPointerParmProcedure;
  ripplespanfuncMT: PPointerParmProcedure;
  slopefuncMT: PPointerParmProcedure;
  baseslopefuncMT: PPointerParmProcedure;
  rippleslopefuncMT: PPointerParmProcedure;
  {$ENDIF}

  centerxfrac: fixed_t;
  centeryfrac: fixed_t;
  centerxshift: fixed_t;

  viewx: fixed_t;
  viewy: fixed_t;
  viewz: fixed_t;

  viewangle: angle_t;

  shiftangle: byte;

  viewcos: fixed_t;
  viewsin: fixed_t;
{$IFNDEF OPENGL}
  // for precise plane drawing in hi-res
  dviewsin, dviewcos: Double;
  planerelativeaspect: Double;
{$ENDIF}
  projection: fixed_t;
  projectiony: fixed_t; // JVAL For correct aspect

  centerx: integer;
  centery: integer;

  fixedcolormap: PByteArray;
  fixedcolormapnum: integer = 0;

// increment every time a check is made
  validcount: integer = 1;

// bumped light from gun blasts
  extralight: integer;

type
  scalelight_t = array[0..LIGHTLEVELS - 1, 0..MAXLIGHTSCALE - 1] of PByteArray;
  Pscalelight_t = ^scalelight_t;

var
  def_scalelight: scalelight_t;
  fog_scalelight: scalelight_t; // JVAL: Mars fog sectors
  scalelight: Pscalelight_t;

var
  scalelightlevels: array[0..LIGHTLEVELS - 1, 0..HLL_MAXLIGHTSCALE - 1] of fixed_t;
  scalelightfixed: array[0..MAXLIGHTSCALE - 1] of PByteArray;

type
  zlight_t = array[0..LIGHTLEVELS - 1, 0..MAXLIGHTZ - 1] of PByteArray;
  Pzlight_t = ^zlight_t;

var
  def_zlight: zlight_t;
  fog_zlight: zlight_t;
  zlight: Pzlight_t;
  renderlightscale: integer;

var
  zlightlevels: array[0..LIGHTLEVELS - 1, 0..HLL_MAXLIGHTZ - 1] of fixed_t;

var
  viewplayer: Pplayer_t;

// The viewangletox[viewangle + FINEANGLES/4] lookup
// maps the visible view angles to screen X coordinates,
// flattening the arc to a flat projection plane.
// There will be many angles mapped to the same X.
  viewangletox: array[0..FINEANGLES div 2 - 1] of integer;

// The xtoviewangleangle[] table maps a screen pixel
// to the lowest viewangle that maps back to x ranges
// from clipangle to -clipangle.
  xtoviewangle: array[0..MAXWIDTH] of angle_t;

//
// precalculated math tables
//
  clipangle: angle_t;

// UNUSED.
// The finetangentgent[angle+FINEANGLES/4] table
// holds the fixed_t tangent values for view angles,
// ranging from MININT to 0 to MAXINT.
// fixed_t    finetangent[FINEANGLES/2];

// fixed_t    finesine[5*FINEANGLES/4];
// fixed_t*    finecosine = &finesine[FINEANGLES/4]; // JVAL -> moved to tables.pas

  linecount: integer;
  loopcount: integer;

  viewangleoffset: angle_t = 0; // Net drone angle

  setsizeneeded: boolean;

// Blocky mode, has default, 0 = high, 1 = normal
  screenblocks: integer;  // has default

//==============================================================================
//
// R_GetColormapLightLevel
//
//==============================================================================
function R_GetColormapLightLevel(const cmap: PByteArray): fixed_t;

//==============================================================================
//
// R_GetColormap32
//
//==============================================================================
function R_GetColormap32(const cmap: PByteArray): PLongWordArray;

//==============================================================================
//
// R_Ticker
//
//==============================================================================
procedure R_Ticker;

{$IFDEF OPENGL}
var
  viewpitch: integer;
  absviewpitch: integer;
{$ENDIF}

var
  monitor_relative_aspect: Double = 1.0;
  fov: fixed_t; // JVAL: 3d Floors (Made global - moved from R_InitTextureMapping)
{$IFNDEF OPENGL}
  xfocallen: float; // JVAL: Slopes
{$ENDIF}

{$IFNDEF OPENGL}

//==============================================================================
//
// R_SetRenderingFunctions
//
//==============================================================================
procedure R_SetRenderingFunctions;
{$ENDIF}

var
  rendervalidcount: integer = 0; // Don't bother reseting this // version 205

implementation

uses
  Math,
  doomdata,
  c_cmds,
  d_net,
  mt_utils,
  mn_screenshot,
  m_bbox,
  m_misc,
  p_setup,
   // JVAL: 3d floors
  {$IFNDEF OPENGL}
  i_threads,
  i_video,
  i_system,
  {$ENDIF}
  r_colormaps,
  r_aspect,
  r_draw,
  r_bsp,
  r_earthquake,
  r_things,
  {$IFNDEF OPENGL}
  r_things_sortvissprites,
  r_dynlights,
  r_softlights,
  {$ENDIF}
  r_plane,
  r_sky,
  {$IFNDEF OPENGL}
  r_segs,
  {$ENDIF}
  r_hires,
  r_camera,
  r_precalc,
  r_ripple,
  {$IFNDEF OPENGL}
  r_cache_main,
  r_fake3d,
  r_trans8,
  r_voxels,
  r_3dfloors, // JVAL: 3d Floors
  r_slopes, // JVAL: Slopes
  {$ENDIF}
  r_lights,
  r_intrpl,
  {$IFDEF OPENGL}
  gl_render, // JVAL OPENGL
  gl_clipper,
  gl_tex,
  {$ELSE}
  r_segs2,
  r_wall8,
  r_wall32,
  r_flat8,
  r_flat32,
  r_flat32_ripple,
  r_span,
  r_span32,
  r_span32_ripple,
  r_column,
  r_tallcolumn,
  r_batchcolumn,
  r_col_l,
  r_col_ms,
  r_col_sk,
  r_col_fz,
  r_draw_average,
  r_draw_alpha,
  r_col_tr,
  r_draw_additive,
  r_draw_subtractive,
  r_depthbuffer,  // JVAL: 3d Floors
  r_zbuffer, // JVAL: version 205
  v_video,
  {$ENDIF}
  r_subsectors,
  r_translations,
  v_data,
  st_stuff,
  z_zone;

var
// just for profiling purposes
  framecount: integer;

//==============================================================================
//
// R_ApplyColormap
//
//==============================================================================
procedure R_ApplyColormap(const ofs, count: integer; const scrn: integer; const cmap: integer);
var
  src: PByte;
  cnt: integer;
  colormap: PByteArray;
begin
  src := PByte(screens[scrn]);
  inc(src, ofs);
  cnt := count;
  colormap := @def_colormaps[cmap * 256];

  while cnt > 0 do
  begin
    src^ := colormap[src^];
    inc(src);
    dec(cnt);
  end;
end;

//==============================================================================
//
// R_AddPointToBox
// Expand a given bbox
// so that it encloses a given point.
//
//==============================================================================
procedure R_AddPointToBox(const x: integer; const y: integer; box: Pfixed_tArray);
begin
  if x < box[BOXLEFT] then
    box[BOXLEFT] := x;
  if x > box[BOXRIGHT] then
    box[BOXRIGHT] := x;
  if y < box[BOXBOTTOM] then
    box[BOXBOTTOM] := y;
  if y > box[BOXTOP] then
    box[BOXTOP] := y;
end;

//==============================================================================
// R_PointOnSide32
//
// R_PointOnSide
// Traverse BSP (sub) tree,
//  check point against partition plane.
// Returns side 0 (front) or 1 (back).
//
//==============================================================================
function R_PointOnSide32(const x: fixed_t; const y: fixed_t; const node: Pnode_t): boolean;
var
  dx: fixed_t;
  dy: fixed_t;
  left: fixed_t;
  right: fixed_t;
begin
  if node.dx = 0 then
  begin
    if x <= node.x then
      result := node.dy > 0
    else
      result := node.dy < 0;
    exit;
  end;

  if node.dy = 0 then
  begin
    if y <= node.y then
      result := node.dx < 0
    else
      result := node.dx > 0;
    exit;
  end;

  dx := x - node.x;
  dy := y - node.y;

  // Try to quickly decide by looking at sign bits.
  if ((node.dy xor node.dx xor dx xor dy) and $80000000) <> 0 then
  begin
    result := ((node.dy xor dx) and $80000000) <> 0;
    exit;
  end;

  left := IntFixedMul(node.dy, dx);
  right := FixedIntMul(dy, node.dx);

  result := right >= left;
end;

//==============================================================================
//
// R_PointOnSide64
//
//==============================================================================
function R_PointOnSide64(const x: fixed_t; const y: fixed_t; const node: Pnode_t): boolean;
var
  dx64: int64;
  dy64: int64;
  left64: int64;
  right64: int64;
begin
  if node.dx = 0 then
  begin
    if x <= node.x then
      result := node.dy > 0
    else
      result := node.dy < 0;
    exit;
  end;

  if node.dy = 0 then
  begin
    if y <= node.y then
      result := node.dx < 0
    else
      result := node.dx > 0;
    exit;
  end;

  dx64 := int64(x) - int64(node.x);
  dy64 := int64(y) - int64(node.y);

  left64 := int64(node.dy div 256) * (dx64 div 256);
  right64 := (dy64 div 256) * int64(node.dx div 256);

  result := right64 >= left64;
end;

//==============================================================================
//
// R_PointOnSide
//
//==============================================================================
function R_PointOnSide(const x: fixed_t; const y: fixed_t; const node: Pnode_t): boolean;
begin
  if largemap then
    result := R_PointOnSide64(x, y, node)
  else
    result := R_PointOnSide32(x, y, node);
end;

//==============================================================================
//
// R_PointOnSideVanilla
//
//==============================================================================
function R_PointOnSideVanilla(const x: fixed_t; const y: fixed_t; const node: Pvanillanode_t): boolean;
var
  dx: fixed_t;
  dy: fixed_t;
  left: fixed_t;
  right: fixed_t;
begin
  if node.dx = 0 then
  begin
    if x <= node.x then
      result := node.dy > 0
    else
      result := node.dy < 0;
    exit;
  end;

  if node.dy = 0 then
  begin
    if y <= node.y then
      result := node.dx < 0
    else
      result := node.dx > 0;
    exit;
  end;

  dx := x - node.x;
  dy := y - node.y;

  // Try to quickly decide by looking at sign bits.
  if ((node.dy xor node.dx xor dx xor dy) and $80000000) <> 0 then
  begin
    result := ((node.dy xor dx) and $80000000) <> 0;
    exit;
  end;

  left := IntFixedMul(node.dy, dx);
  right := FixedIntMul(dy, node.dx);

  result := right >= left;
end;

//==============================================================================
//
// R_PointOnSegSide32
//
//==============================================================================
function R_PointOnSegSide32(x: fixed_t; y: fixed_t; line: Pseg_t): boolean;
var
  lx: fixed_t;
  ly: fixed_t;
  ldx: fixed_t;
  ldy: fixed_t;
  dx: fixed_t;
  dy: fixed_t;
  left: fixed_t;
  right: fixed_t;
begin
  lx := line.v1.x;
  ly := line.v1.y;

  ldx := line.v2.x - lx;
  ldy := line.v2.y - ly;

  if ldx = 0 then
  begin
    if x <= lx then
      result := ldy > 0
    else
      result := ldy < 0;
    exit;
  end;

  if ldy = 0 then
  begin
    if y <= ly then
      result := ldx < 0
    else
      result := ldx > 0;
    exit;
  end;

  dx := x - lx;
  dy := y - ly;

  // Try to quickly decide by looking at sign bits.
  if ((ldy xor ldx xor dx xor dy) and $80000000) <> 0 then
  begin
    result := ((ldy xor dx) and $80000000) <> 0;
    exit;
  end;

  left := IntFixedMul(ldy, dx);
  right := FixedIntMul(dy, ldx);

  result := left <= right;
end;

//==============================================================================
//
// R_PointOnSegSide64
//
//==============================================================================
function R_PointOnSegSide64(x: fixed_t; y: fixed_t; line: Pseg_t): boolean;
var
  lx: fixed_t;
  ly: fixed_t;
  ldx: fixed_t;
  ldy: fixed_t;
  dx64: int64;
  dy64: int64;
  left64: int64;
  right64: int64;
begin
  lx := line.v1.x;
  ly := line.v1.y;

  ldx := line.v2.x - lx;
  ldy := line.v2.y - ly;

  if ldx = 0 then
  begin
    if x <= lx then
      result := ldy > 0
    else
      result := ldy < 0;
    exit;
  end;

  if ldy = 0 then
  begin
    if y <= ly then
      result := ldx < 0
    else
      result := ldx > 0;
    exit;
  end;

  dx64 := int64(x) - int64(lx);
  dy64 := int64(y) - int64(ly);

  left64 := int64(ldy div 256) * (dx64 div 256);
  right64 := (dy64 div 256) * int64(ldx div 256);

  result := left64 <= right64;
end;

//==============================================================================
//
// R_PointOnSegSide
//
//==============================================================================
function R_PointOnSegSide(x: fixed_t; y: fixed_t; line: Pseg_t): boolean;
begin
  if largemap then
    result := R_PointOnSegSide64(x, y, line)
  else
    result := R_PointOnSegSide32(x, y, line);
end;

//==============================================================================
//
// R_PointOnLineSide32
//
//==============================================================================
function R_PointOnLineSide32(x: fixed_t; y: fixed_t; line: Pline_t): boolean;
var
  lx: fixed_t;
  ly: fixed_t;
  ldx: fixed_t;
  ldy: fixed_t;
  dx: fixed_t;
  dy: fixed_t;
  left: fixed_t;
  right: fixed_t;
begin
  lx := line.v1.x;
  ly := line.v1.y;

  ldx := line.v2.x - lx;
  ldy := line.v2.y - ly;

  if ldx = 0 then
  begin
    if x <= lx then
      result := ldy > 0
    else
      result := ldy < 0;
    exit;
  end;

  if ldy = 0 then
  begin
    if y <= ly then
      result := ldx < 0
    else
      result := ldx > 0;
    exit;
  end;

  dx := x - lx;
  dy := y - ly;

  left := IntFixedMul(ldy, dx);
  right := FixedIntMul(dy, ldx);

  result := left <= right;
end;

//==============================================================================
//
// R_PointOnLineSide64
//
//==============================================================================
function R_PointOnLineSide64(x: fixed_t; y: fixed_t; line: Pline_t): boolean;
var
  lx: fixed_t;
  ly: fixed_t;
  ldx: fixed_t;
  ldy: fixed_t;
  dx64: int64;
  dy64: int64;
  left64: int64;
  right64: int64;
begin
  lx := line.v1.x;
  ly := line.v1.y;

  ldx := line.v2.x - lx;
  ldy := line.v2.y - ly;

  if ldx = 0 then
  begin
    if x <= lx then
      result := ldy > 0
    else
      result := ldy < 0;
    exit;
  end;

  if ldy = 0 then
  begin
    if y <= ly then
      result := ldx < 0
    else
      result := ldx > 0;
    exit;
  end;

  dx64 := int64(x) - int64(lx);
  dy64 := int64(y) - int64(ly);

  left64 := int64(ldy div 256) * (dx64 div 256);
  right64 := (dy64 div 256) * int64(ldx div 256);

  result := left64 <= right64;
end;

//==============================================================================
//
// R_PointOnLineSide
//
//==============================================================================
function R_PointOnLineSide(x: fixed_t; y: fixed_t; line: Pline_t): boolean;
begin
  if largemap then
    result := R_PointOnLineSide64(x, y, line)
  else
    result := R_PointOnLineSide32(x, y, line)
end;

//==============================================================================
//
// R_PointToAngle
// To get a global angle from cartesian coordinates,
//  the coordinates are flipped until they are in
//  the first octant of the coordinate system, then
//  the y (<=x) is scaled and divided by x to get a
//  tangent (slope) value which is looked up in the
//  tantoangle[] table.
//
// JVAL  -> Calculates: result := round(683565275 * (arctan2(y, x)));
//
//==============================================================================
function R_PointToAngle(x: fixed_t; y: fixed_t): angle_t;
begin
  x := x - viewx;
  y := y - viewy;

  if (x = 0) and (y = 0) then
  begin
    result := 0;
    exit;
  end;

  if x >= 0 then
  begin
    // x >=0
    if y >= 0 then
    begin
      // y>= 0
      if x > y then
      begin
        // octant 0
        result := tantoangle[SlopeDiv(y, x)];
        exit;
      end
      else
      begin
        // octant 1
        result := ANG90 - 1 - tantoangle[SlopeDiv(x, y)];
        exit;
      end;
    end
    else
    begin
      // y<0
      y := -y;
      if x > y then
      begin
        // octant 8
        result := -tantoangle[SlopeDiv(y, x)];
        exit;
      end
      else
      begin
        // octant 7
        result := ANG270 + tantoangle[SlopeDiv(x, y)];
        exit;
      end;
    end;
  end
  else
  begin
    // x<0
    x := -x;
    if y >= 0 then
    begin
      // y>= 0
      if x > y then
      begin
        // octant 3
        result := ANG180 - 1 - tantoangle[SlopeDiv(y, x)];
        exit;
      end
      else
      begin
        // octant 2
        result := ANG90 + tantoangle[SlopeDiv(x, y)];
        exit;
      end;
    end
    else
    begin
      // y<0
      y := -y;
      if x > y then
      begin
        // octant 4
        result := ANG180 + tantoangle[SlopeDiv(y, x)];
        exit;
      end
      else
      begin
        // octant 5
        result := ANG270 - 1 - tantoangle[SlopeDiv(x, y)];
        exit;
      end;
    end;
  end;

  result := 0;
end;

//==============================================================================
//
// R_PointToAngleEx1
//
//==============================================================================
function R_PointToAngleEx1(x: fixed_t; y: fixed_t): angle_t;
begin
  if (x = 0) and (y = 0) then
  begin
    result := 0;
    exit;
  end;

  if x >= 0 then
  begin
    // x >=0
    if y >= 0 then
    begin
      // y>= 0
      if x > y then
      begin
        // octant 0
        result := tantoangle_ex[SlopeDivEx(y, x)];
        exit;
      end
      else
      begin
        // octant 1
        result := ANG90 - 1 - tantoangle_ex[SlopeDivEx(x, y)];
        exit;
      end;
    end
    else
    begin
      // y<0
      y := -y;
      if x > y then
      begin
        // octant 8
        result := -tantoangle_ex[SlopeDivEx(y, x)];
        exit;
      end
      else
      begin
        // octant 7
        result := ANG270 + tantoangle_ex[SlopeDivEx(x, y)];
        exit;
      end;
    end;
  end
  else
  begin
    // x<0
    x := -x;
    if y >= 0 then
    begin
      // y>= 0
      if x > y then
      begin
        // octant 3
        result := ANG180 - 1 - tantoangle_ex[SlopeDivEx(y, x)];
        exit;
      end
      else
      begin
        // octant 2
        result := ANG90 + tantoangle_ex[SlopeDivEx(x, y)];
        exit;
      end;
    end
    else
    begin
      // y<0
      y := -y;
      if x > y then
      begin
        // octant 4
        result := ANG180 + tantoangle_ex[SlopeDivEx(y, x)];
        exit;
      end
      else
      begin
        // octant 5
        result := ANG270 - 1 - tantoangle_ex[SlopeDivEx(x, y)];
        exit;
      end;
    end;
  end;

  result := 0;
end;

var
  pta_x, pta_y: fixed_t;
  pta_ret: angle_t;

//==============================================================================
//
// R_PointToAngleEx
//
//==============================================================================
function R_PointToAngleEx(x: fixed_t; y: fixed_t): angle_t;
begin
  x := x - viewx;
  y := y - viewy;

  if x = pta_x then
    if y = pta_y then
    begin
      Result := pta_ret;
      Exit;
    end;

  result := R_PointToAngleEx1(x, y);
  pta_x := x;
  pta_y := y;
  pta_ret := Result;
end;

//==============================================================================
//
// R_PointToAngleDbl
// JVAL: very slow, do not use
//
//==============================================================================
function R_PointToAngleDbl(const x: fixed_t; const y: fixed_t): angle_t;
var
  xx, yy: fixed_t;
begin
  xx := x - viewx;
  yy := y - viewy;
  result := Round(arctan2(yy, xx) * (ANG180 / D_PI));
end;

//==============================================================================
//
// R_PointToAngle2
//
//==============================================================================
function R_PointToAngle2(const x1: fixed_t; const y1: fixed_t; const x2: fixed_t; const y2: fixed_t): angle_t;
begin
  result := R_PointToAngle(x2 - x1 + viewx, y2 - y1 + viewy);
end;

//==============================================================================
//
// R_PointToDist
//
//==============================================================================
function R_PointToDist(const x: fixed_t; const y: fixed_t): fixed_t;
var
  angle: integer;
  dx: fixed_t;
  dy: fixed_t;
  temp: fixed_t;
begin
  dx := abs(x - viewx);
  dy := abs(y - viewy);
  if dx = 0 then
  begin
    result := dy;
    exit;
  end;
  if dy = 0 then
  begin
    result := dx;
    exit;
  end;

  if dy > dx then
  begin
    temp := dx;
    dx := dy;
    dy := temp;
  end;

  {$IFDEF FPC}
  angle := _SHRW(tantoangle[FixedDiv(dy, dx) shr DBITS], ANGLETOFINESHIFT);
  {$ELSE}
  angle := tantoangle[FixedDiv(dy, dx) shr DBITS] shr ANGLETOFINESHIFT;
  {$ENDIF}

  result := FixedDiv(dx, finecosine[angle]);
end;

//==============================================================================
//
// R_InitPointToAngle
//
//==============================================================================
procedure R_InitPointToAngle;
{var
  i: integer;
  t: angle_t;
  f: extended;}
begin
//
// slope (tangent) to angle lookup
//
{  for i := 0 to SLOPERANGE do
  begin
    f := arctan(i / SLOPERANGE) / (D_PI * 2);
    t := round(LongWord($ffffffff) * f);
    tantoangle[i] := t;
  end;}
end;

//==============================================================================
//
// R_InitTables
//
//==============================================================================
procedure R_InitTables;
// JVAL: Caclulate tables constants
{var
  i: integer;
  a: extended;
  fv: extended;
  t: integer;}
begin              {
// viewangle tangent table
  for i := 0 to FINEANGLES div 2 - 1 do
  begin
    a := (i - FINEANGLES / 4 + 0.5) * D_PI * 2 / FINEANGLES;
    fv := FRACUNIT * tan(a);
    t := round(fv);
    finetangent[i] := t;
  end;

  // finesine table
  for i := 0 to 5 * FINEANGLES div 4 - 1 do
  begin
    // OPTIMIZE: mirror...
    a := (i + 0.5) * D_PI * 2 / FINEANGLES;
    t := round(FRACUNIT * sin(a));
    finesine[i] := t;
  end;              }

  finecosine := Pfixed_tArray(@finesine[FINEANGLES div 4]);
  fixedcosine := Pfixed_tArray(@fixedsine[FIXEDANGLES div 4]);
end;

var
  oldfocallength: fixed_t = -1;

//==============================================================================
//
// R_InitTextureMapping
//
//==============================================================================
procedure R_InitTextureMapping;
var
  i: integer;
  x: integer;
  t: integer;
  focallength: fixed_t;
  an: angle_t;
begin
  // Use tangent table to generate viewangletox:
  //  viewangletox will give the next greatest x
  //  after the view angle.
  //
  // Calc focallength

// JVAL: Widescreen support
  if monitor_relative_aspect = 1.0 then
    fov := ANG90 shr ANGLETOFINESHIFT
  else
    fov := round(arctan(monitor_relative_aspect) * FINEANGLES / D_PI);
  focallength := FixedDiv(centerxfrac, finetangent[FINEANGLES div 4 + fov div 2]);

  if focallength = oldfocallength then
    exit;
  oldfocallength := focallength;

  for i := 0 to FINEANGLES div 2 - 1 do
  begin
    if finetangent[i] > FRACUNIT * 2 then
      t := -1
    else if finetangent[i] < -FRACUNIT * 2 then
      t := viewwidth + 1
    else
    begin
      t := FixedMul(finetangent[i], focallength);
      t := (centerxfrac - t + (FRACUNIT - 1)) div FRACUNIT;

      if t < -1 then
        t := -1
      else if t > viewwidth + 1 then
        t := viewwidth + 1;
    end;
    viewangletox[i] := t;
  end;

  // Scan viewangletox[] to generate xtoviewangle[]:
  //  xtoviewangle will give the smallest view angle
  //  that maps to x.
  for x := 0 to viewwidth do
  begin
    an := 0;
    while viewangletox[an] > x do
      inc(an);
    xtoviewangle[x] := an * ANGLETOFINEUNIT - ANG90;
  end;

  // Take out the fencepost cases from viewangletox.
  for i := 0 to FINEANGLES div 2 - 1 do
  begin
    if viewangletox[i] = -1 then
      viewangletox[i] := 0
    else if viewangletox[i] = viewwidth + 1 then
      viewangletox[i] := viewwidth;
  end;
  clipangle := xtoviewangle[0];
end;

//==============================================================================
//
// R_InitLightTables
// Only inits the zlight table,
//  because the scalelight table changes with view size.
//
//==============================================================================
procedure R_InitLightTables;
var
  i: integer;
  j: integer;
  level: integer;
  startmap: integer;
  scale: integer;
  levelhi: integer;
  startmaphi: integer;
  scalehi: integer;
begin
  // Calculate the light levels to use
  //  for each level / distance combination.
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

      def_zlight[i][j] := @def_colormaps[level * 256];
      fog_zlight[i][j] := @fog_colormaps[level * 256];
    end;

    startmaphi := ((LIGHTLEVELS - 1 - i) * 2 * FRACUNIT) div LIGHTLEVELS;
    for j := 0 to HLL_MAXLIGHTZ - 1 do
    begin

      scalehi := FixedDiv(160 * FRACUNIT, _SHL(j + 1, HLL_LIGHTZSHIFT));
      scalehi := _SHR(scalehi, HLL_LIGHTSCALESHIFT);
      levelhi := FRACUNIT - startmaphi + scalehi div DISTMAP;

      if levelhi < 0 then
        levelhi := 0
      else if levelhi >= FRACUNIT then
        levelhi := FRACUNIT - 1;

      zlightlevels[i][j] := levelhi;
    end;
  end;

end;

//
// R_SetViewSize
// Do not really change anything here,
//  because it might be in the middle of a refresh.
// The change will take effect next refresh.
//
var
  setblocks: integer = -1;
  olddetail: integer = -1;

//==============================================================================
//
// R_SetViewSize
//
//==============================================================================
procedure R_SetViewSize;
begin
  if not allowlowdetails then
    if detailLevel < DL_MEDIUM then
      detailLevel := DL_MEDIUM;
  if not allowhidetails then
    if detailLevel > DL_NORMAL then
    begin
      if allowlowdetails then
        detailLevel := DL_LOWEST
      else
        detailLevel := DL_MEDIUM;
    end;

  if (setblocks <> screenblocks) or (setdetail <> detailLevel) then
  begin
    if setdetail <> detailLevel then
    begin
      recalctables32needed := true;
    end;
    setsizeneeded := true;
    setblocks := screenblocks;
    setdetail := detailLevel;
  end;
end;

{$IFNDEF OPENGL}

//==============================================================================
//
// R_SetPalette64
//
//==============================================================================
procedure R_SetPalette64;
begin
  if setdetail in [DL_LOWEST, DL_LOW, DL_MEDIUM] then
    I_SetPalette64;
end;

//==============================================================================
//
// R_SetRenderingFunctions
//
//==============================================================================
procedure R_SetRenderingFunctions;
begin
  case setdetail of
    DL_LOWEST:
      begin
        basebatchcolfunc := R_DrawColumnLow_Batch;
        batchcolfunc := R_DrawColumnLow_Batch;
        batchfuzzcolfunc := R_DrawFuzzColumn_Batch;
        batchlightcolfunc := nil;
        batchwhitelightcolfunc := nil;
        batchredlightcolfunc := nil;
        batchgreenlightcolfunc := nil;
        batchbluelightcolfunc := nil;
        batchyellowlightcolfunc := nil;
        batchtranscolfunc := R_DrawTranslatedColumn_Batch;

        if usemultithread then
        begin
          basebatchcolfunc_mt := R_DrawColumnLow_BatchMT;
          batchcolfunc_mt := R_DrawColumnLow_BatchMT;
          batchtaveragecolfunc_mt := R_DrawColumnAverageMedium_BatchMT;
          if diher8bittransparency then
            batchtalphacolfunc_mt := nil
          else
            batchtalphacolfunc_mt := R_DrawColumnAlphaMedium_BatchMT;
          batchaddcolfunc_mt := R_DrawColumnAddMedium_BatchMT;
          batchsubtractcolfunc_mt := R_DrawColumnSubtractMedium_BatchMT;
          maskedcolfunc_mt := R_DrawColumnLowestMT;
          colfunc_mt := nil;
          averagecolfunc_mt := R_DrawColumnAverageLowestMT;
          alphacolfunc_mt := nil;
          addcolfunc_mt := R_DrawColumnAddLowestMT;
          addcolfunc_smallstep_mt := R_DrawColumnAddLowestMT;
          subtractcolfunc_mt := R_DrawColumnSubtractLowestMT;
        end
        else
        begin
          basebatchcolfunc_mt := nil;
          batchcolfunc_mt := nil;
          batchtaveragecolfunc_mt := nil;
          batchtalphacolfunc_mt := nil;
          batchaddcolfunc_mt := nil;
          batchsubtractcolfunc_mt := nil;
          maskedcolfunc_mt := nil;
          colfunc_mt := nil;
          averagecolfunc_mt := nil;
          alphacolfunc_mt := nil;
          addcolfunc_mt := nil;
          addcolfunc_smallstep_mt := nil;
          subtractcolfunc_mt := nil;
        end;

        colfunc := R_DrawColumnLowest;
        wallcolfunc := R_DrawColumnLowest;
        basewallcolfunc := R_DrawColumnLowest;
        tallwallcolfunc := R_DrawTallColumnLowest;
        transcolfunc := R_DrawTranslatedColumn;
        if diher8bittransparency then
        begin
          averagecolfunc := R_DrawColumnAlphaMediumDiher;
          alphacolfunc := R_DrawColumnAlphaMediumDiher;
          batchtaveragecolfunc := nil;
          batchtalphacolfunc := nil;
        end
        else
        begin
          averagecolfunc := R_DrawColumnAverageLowest;
          alphacolfunc := R_DrawColumnAlphaLowest;
          batchtaveragecolfunc := R_DrawColumnAverageMedium_Batch;
          batchtalphacolfunc := R_DrawColumnAlphaMedium_Batch;
        end;
        addcolfunc := R_DrawColumnAddLowest;
        addcolfunc_smallstep := R_DrawColumnAddLowest;
        batchaddcolfunc := R_DrawColumnAddMedium_Batch;
        subtractcolfunc := R_DrawColumnSubtractLowest;
        batchsubtractcolfunc := R_DrawColumnSubtractMedium_Batch;

        maskedcolfunc := R_DrawColumnLowest;
        maskedcolfunc2 := R_DrawColumnLowest;

        if usemultithread then
        begin
          spanfunc := R_StoreFlatSpan8;
          basespanfunc := R_StoreFlatSpan8;
          ripplespanfunc := R_StoreFlatSpan8;
          slopefunc := R_StoreFlatSpan8;
          baseslopefunc := R_StoreFlatSpan8;
          rippleslopefunc := R_StoreFlatSpan8;

          spanfuncMT := R_DrawSpanLowMT;
          basespanfuncMT := R_DrawSpanLowMT;
          ripplespanfuncMT := R_DrawSpanLowMT;
          slopefuncMT := R_DrawSpanLowMT;
          baseslopefuncMT := R_DrawSpanLowMT;
          rippleslopefuncMT := R_DrawSpanLowMT;
        end
        else
        begin
          spanfunc := R_DrawSpanLow;
          basespanfunc := R_DrawSpanLow;
          ripplespanfunc := R_DrawSpanLow;
          slopefunc := R_DrawSpanLow; // JVAL: Slopes
          baseslopefunc := R_DrawSpanLow;
          rippleslopefunc := R_DrawSpanLow;
        end;

        fuzzcolfunc := R_DrawFuzzColumn;
        lightcolfunc := R_DrawFuzzColumn;
        whitelightcolfunc := R_DrawFuzzColumn;
        redlightcolfunc := R_DrawFuzzColumn;
        greenlightcolfunc := R_DrawFuzzColumn;
        bluelightcolfunc := R_DrawFuzzColumn;
        yellowlightcolfunc := R_DrawFuzzColumn;
        skycolfunc := R_DrawSkyColumnLow;
        videomode := vm8bit;
      end;
    DL_LOW:
      begin
        basebatchcolfunc := R_DrawColumnLow_Batch;
        batchcolfunc := R_DrawColumnLow_Batch;
        batchfuzzcolfunc := R_DrawFuzzColumn_Batch;
        batchlightcolfunc := nil;
        batchwhitelightcolfunc := nil;
        batchredlightcolfunc := nil;
        batchgreenlightcolfunc := nil;
        batchbluelightcolfunc := nil;
        batchyellowlightcolfunc := nil;
        batchtranscolfunc := R_DrawTranslatedColumn_Batch;

        if usemultithread then
        begin
          basebatchcolfunc_mt := R_DrawColumnLow_BatchMT;
          batchcolfunc_mt := R_DrawColumnLow_BatchMT;
          batchtaveragecolfunc_mt := R_DrawColumnAverageMedium_BatchMT;
          if diher8bittransparency then
            batchtalphacolfunc_mt := nil
          else
            batchtalphacolfunc_mt := R_DrawColumnAlphaMedium_BatchMT;
          batchaddcolfunc_mt := R_DrawColumnAddMedium_BatchMT;
          batchsubtractcolfunc_mt := R_DrawColumnSubtractMedium_BatchMT;
          maskedcolfunc_mt := R_DrawColumnLowMT;
          colfunc_mt := nil;
          averagecolfunc_mt := R_DrawColumnAverageLowMT;
          alphacolfunc_mt := nil;
          addcolfunc_mt := R_DrawColumnAddLowMT;
          addcolfunc_smallstep_mt := R_DrawColumnAddLowMT;
          subtractcolfunc_mt := R_DrawColumnSubtractLowMT;
        end
        else
        begin
          basebatchcolfunc_mt := nil;
          batchcolfunc_mt := nil;
          batchtaveragecolfunc_mt := nil;
          batchtalphacolfunc_mt := nil;
          batchaddcolfunc_mt := nil;
          batchsubtractcolfunc_mt := nil;
          maskedcolfunc_mt := nil;
          colfunc_mt := nil;
          averagecolfunc_mt := nil;
          alphacolfunc_mt := nil;
          addcolfunc_mt := nil;
          addcolfunc_smallstep_mt := nil;
          subtractcolfunc_mt := nil;
        end;

        colfunc := R_DrawColumnLow;
        wallcolfunc := R_DrawColumnLow;
        basewallcolfunc := R_DrawColumnLow;
        tallwallcolfunc := R_DrawTallColumnLow;
        transcolfunc := R_DrawTranslatedColumn;
        if diher8bittransparency then
        begin
          averagecolfunc := R_DrawColumnAlphaMediumDiher;
          alphacolfunc := R_DrawColumnAlphaMediumDiher;
          batchtaveragecolfunc := nil;
          batchtalphacolfunc := nil;
        end
        else
        begin
          averagecolfunc := R_DrawColumnAverageLow;
          alphacolfunc := R_DrawColumnAlphaLow;
          batchtaveragecolfunc := R_DrawColumnAverageMedium_Batch;
          batchtalphacolfunc := R_DrawColumnAlphaMedium_Batch;
        end;
        addcolfunc := R_DrawColumnAddLow;
        addcolfunc_smallstep := R_DrawColumnAddLow;
        batchaddcolfunc := R_DrawColumnAddMedium_Batch;
        subtractcolfunc := R_DrawColumnSubtractLow;
        batchsubtractcolfunc := R_DrawColumnSubtractMedium_Batch;

        maskedcolfunc := R_DrawColumnLow;
        maskedcolfunc2 := R_DrawColumnLow;

        if usemultithread then
        begin
          spanfunc := R_StoreFlatSpan8;
          basespanfunc := R_StoreFlatSpan8;
          ripplespanfunc := R_StoreFlatSpan8;
          slopefunc := R_StoreFlatSpan8;
          baseslopefunc := R_StoreFlatSpan8;
          rippleslopefunc := R_StoreFlatSpan8;

          spanfuncMT := R_DrawSpanLowMT;
          basespanfuncMT := R_DrawSpanLowMT;
          ripplespanfuncMT := R_DrawSpanLowMT;
          slopefuncMT := R_DrawSpanLowMT;
          baseslopefuncMT := R_DrawSpanLowMT;
          rippleslopefuncMT := R_DrawSpanLowMT;
        end
        else
        begin
          spanfunc := R_DrawSpanLow;
          basespanfunc := R_DrawSpanLow;
          ripplespanfunc := R_DrawSpanLow;
          slopefunc := R_DrawSpanLow; // JVAL: Slopes
          baseslopefunc := R_DrawSpanLow;
          rippleslopefunc := R_DrawSpanLow;
        end;

        fuzzcolfunc := R_DrawFuzzColumn;
        lightcolfunc := R_DrawFuzzColumn;
        whitelightcolfunc := R_DrawFuzzColumn;
        redlightcolfunc := R_DrawFuzzColumn;
        greenlightcolfunc := R_DrawFuzzColumn;
        bluelightcolfunc := R_DrawFuzzColumn;
        yellowlightcolfunc := R_DrawFuzzColumn;
        skycolfunc := R_DrawSkyColumnLow;
        videomode := vm8bit;
      end;
    DL_MEDIUM:
      begin
        basebatchcolfunc := R_DrawColumnMedium_Batch;
        batchcolfunc := R_DrawColumnMedium_Batch;
        batchfuzzcolfunc := R_DrawFuzzColumn_Batch;
        batchlightcolfunc := nil;
        batchwhitelightcolfunc := nil;
        batchredlightcolfunc := nil;
        batchgreenlightcolfunc := nil;
        batchbluelightcolfunc := nil;
        batchyellowlightcolfunc := nil;
        batchtranscolfunc := R_DrawTranslatedColumn_Batch;

        if usemultithread then
        begin
          basebatchcolfunc_mt := R_DrawColumnMedium_BatchMT;
          batchcolfunc_mt := R_DrawColumnMedium_BatchMT;
          batchtaveragecolfunc_mt := R_DrawColumnAverageMedium_BatchMT;
          if diher8bittransparency then
            batchtalphacolfunc_mt := nil
          else
            batchtalphacolfunc_mt := R_DrawColumnAlphaMedium_BatchMT;
          batchaddcolfunc_mt := R_DrawColumnAddMedium_BatchMT;
          batchsubtractcolfunc_mt := R_DrawColumnSubtractMedium_BatchMT;
          maskedcolfunc_mt := R_DrawColumnMediumMT;
          colfunc_mt := nil;
          averagecolfunc_mt := R_DrawColumnAverageMediumMT;
          alphacolfunc_mt := nil;
          addcolfunc_mt := R_DrawColumnAddMediumMT;
          addcolfunc_smallstep_mt := R_DrawColumnAddMediumMT;
          subtractcolfunc_mt := R_DrawColumnSubtractMediumMT;
        end
        else
        begin
          basebatchcolfunc_mt := nil;
          batchcolfunc_mt := nil;
          batchtaveragecolfunc_mt := nil;
          batchtalphacolfunc_mt := nil;
          batchaddcolfunc_mt := nil;
          batchsubtractcolfunc_mt := nil;
          maskedcolfunc_mt := nil;
          colfunc_mt := nil;
          averagecolfunc_mt := nil;
          alphacolfunc_mt := nil;
          addcolfunc_mt := nil;
          addcolfunc_smallstep_mt := nil;
          subtractcolfunc_mt := nil;
        end;

        colfunc := R_DrawColumnMedium;
        wallcolfunc := R_DrawColumnMedium;
        basewallcolfunc := R_DrawColumnMedium;
        tallwallcolfunc := R_DrawTallColumnMedium;
        transcolfunc := R_DrawTranslatedColumn;
        if diher8bittransparency then
        begin
          averagecolfunc := R_DrawColumnAlphaMediumDiher;
          alphacolfunc := R_DrawColumnAlphaMediumDiher;
          batchtaveragecolfunc := nil;
          batchtalphacolfunc := nil;
        end
        else
        begin
          averagecolfunc := R_DrawColumnAverageMedium;
          alphacolfunc := R_DrawColumnAlphaMedium;
          batchtaveragecolfunc := R_DrawColumnAverageMedium_Batch;
          batchtalphacolfunc := R_DrawColumnAlphaMedium_Batch;
        end;
        addcolfunc := R_DrawColumnAddMedium;
        addcolfunc_smallstep := R_DrawColumnAddMedium;
        batchaddcolfunc := R_DrawColumnAddMedium_Batch;
        subtractcolfunc := R_DrawColumnSubtractMedium;
        batchsubtractcolfunc := R_DrawColumnSubtractMedium_Batch;

        maskedcolfunc := R_DrawColumnMedium;
        maskedcolfunc2 := R_DrawColumnMedium;

        if usemultithread then
        begin
          spanfunc := R_StoreFlatSpan8;
          basespanfunc := R_StoreFlatSpan8;
          ripplespanfunc := R_StoreFlatSpan8;
          slopefunc := R_StoreFlatSpan8;
          baseslopefunc := R_StoreFlatSpan8;
          rippleslopefunc := R_StoreFlatSpan8;

          spanfuncMT := R_DrawSpanMediumMT;
          basespanfuncMT := R_DrawSpanMediumMT;
          ripplespanfuncMT := R_DrawSpanMedium_RippleMT;
          slopefuncMT := R_DrawSpanMediumMT;
          baseslopefuncMT := R_DrawSpanMediumMT;
          rippleslopefuncMT := R_DrawSpanMedium_RippleMT;
        end
        else
        begin
          spanfunc := R_DrawSpanMedium;
          basespanfunc := R_DrawSpanMedium;
          ripplespanfunc := R_DrawSpanMedium_Ripple;
          slopefunc := R_DrawSlopeMedium; // JVAL: Slopes
          baseslopefunc := R_DrawSlopeMedium;
          rippleslopefunc := R_DrawSlopeMedium_Ripple;
        end;

        fuzzcolfunc := R_DrawFuzzColumn;
        lightcolfunc := R_DrawFuzzColumn;
        whitelightcolfunc := R_DrawFuzzColumn;
        redlightcolfunc := R_DrawFuzzColumn;
        greenlightcolfunc := R_DrawFuzzColumn;
        bluelightcolfunc := R_DrawFuzzColumn;
        yellowlightcolfunc := R_DrawFuzzColumn;
        skycolfunc := R_DrawSkyColumn;
        videomode := vm8bit;
      end;
    DL_NORMAL:  // JVAL: 32 bit color - Default
      begin
        basebatchcolfunc := R_DrawColumnHi_Batch;
        batchcolfunc := R_DrawColumnHi_Batch;
        if use32bitfuzzeffect then
          batchfuzzcolfunc := R_DrawFuzzColumn32_Batch
        else
          batchfuzzcolfunc := R_DrawFuzzColumnHi_Batch;
        batchlightcolfunc := R_DrawWhiteLightColumnHi_Batch;
        batchwhitelightcolfunc := R_DrawWhiteLightColumnHi_Batch;
        batchredlightcolfunc := R_DrawRedLightColumnHi_Batch;
        batchgreenlightcolfunc := R_DrawGreenLightColumnHi_Batch;
        batchbluelightcolfunc := R_DrawBlueLightColumnHi_Batch;
        batchyellowlightcolfunc := R_DrawYellowLightColumnHi_Batch;
        batchtranscolfunc := R_DrawTranslatedColumnHi_Batch;
        batchtaveragecolfunc := R_DrawColumnAverageHi_Batch;
        batchtalphacolfunc := R_DrawColumnAlphaHi_Batch;
        batchaddcolfunc := R_DrawColumnAddHi_Batch;
        batchsubtractcolfunc := R_DrawColumnSubtractHi_Batch;

        if usemultithread then
        begin
          basebatchcolfunc_mt := R_DrawColumnHi_BatchMT;
          batchcolfunc_mt := R_DrawColumnHi_BatchMT;
          batchtaveragecolfunc_mt := R_DrawColumnAverageHi_BatchMT;
          batchtalphacolfunc_mt := R_DrawColumnAlphaHi_BatchMT;
          batchaddcolfunc_mt := R_DrawColumnAddHi_BatchMT;
          batchsubtractcolfunc_mt := R_DrawColumnSubtractHi_BatchMT;
          maskedcolfunc_mt := R_DrawMaskedColumnNormalMT;
          colfunc_mt := nil;
          averagecolfunc_mt := R_DrawColumnAverageHiMT;
          alphacolfunc_mt := nil;
          addcolfunc_mt := R_DrawColumnAddHiMT;
          addcolfunc_smallstep_mt := R_DrawColumnAddHi_SmallStepMT;
          subtractcolfunc_mt := R_DrawColumnSubtractHiMT;
        end
        else
        begin
          basebatchcolfunc_mt := nil;
          batchcolfunc_mt := nil;
          batchtaveragecolfunc_mt := nil;
          batchtalphacolfunc_mt := nil;
          batchaddcolfunc_mt := nil;
          batchsubtractcolfunc_mt := nil;
          maskedcolfunc_mt := nil;
          colfunc_mt := nil;
          averagecolfunc_mt := nil;
          alphacolfunc_mt := nil;
          addcolfunc_mt := nil;
          addcolfunc_smallstep_mt := nil;
          subtractcolfunc_mt := nil;
        end;

        colfunc := R_DrawColumnHi;
        wallcolfunc := R_DrawColumnHi;
        basewallcolfunc := R_DrawColumnHi;
        tallwallcolfunc := R_DrawTallColumnHi;
        transcolfunc := R_DrawTranslatedColumnHi;
        averagecolfunc := R_DrawColumnAverageHi;
        alphacolfunc := R_DrawColumnAlphaHi;
        addcolfunc := R_DrawColumnAddHi;
        addcolfunc_smallstep := R_DrawColumnAddHi_SmallStep;
        subtractcolfunc := R_DrawColumnSubtractHi;
        maskedcolfunc := R_DrawMaskedColumnNormal;
        maskedcolfunc2 := R_DrawMaskedColumnHi32;

        if usemultithread then
        begin
          spanfunc := R_StoreFlatSpan32;
          basespanfunc := R_StoreFlatSpan32;
          ripplespanfunc := R_StoreFlatSpan32;
          slopefunc := R_StoreFlatSpan32;
          baseslopefunc := R_StoreFlatSpan32;
          rippleslopefunc := R_StoreFlatSpan32;

          spanfuncMT := R_DrawSpanNormalMT;
          basespanfuncMT := R_DrawSpanNormalMT;
          ripplespanfuncMT := R_DrawSpanNormal_RippleMT;
          slopefuncMT := R_DrawSpanNormalMT;
          baseslopefuncMT := R_DrawSpanNormalMT;
          rippleslopefuncMT := R_DrawSpanNormal_RippleMT;
        end
        else
        begin
          spanfunc := R_DrawSpanNormal;
          basespanfunc := R_DrawSpanNormal;
          ripplespanfunc := R_DrawSpanNormal_Ripple;
          slopefunc := R_DrawSpanNormal;  // JVAL: Slopes
          baseslopefunc := R_DrawSpanNormal;
          rippleslopefunc := R_DrawSpanNormal_Ripple;
        end;

        if use32bitfuzzeffect then
          fuzzcolfunc := R_DrawFuzzColumn32
        else
          fuzzcolfunc := R_DrawFuzzColumnHi;
        lightcolfunc := R_DrawWhiteLightColumnHi;
        whitelightcolfunc := R_DrawWhiteLightColumnHi;
        redlightcolfunc := R_DrawRedLightColumnHi;
        greenlightcolfunc := R_DrawGreenLightColumnHi;
        bluelightcolfunc := R_DrawBlueLightColumnHi;
        yellowlightcolfunc := R_DrawYellowLightColumnHi;
        skycolfunc := R_DrawSkyColumnHi;
        videomode := vm32bit;
      end;
    DL_HIRES:
      begin
        basebatchcolfunc := R_DrawColumnHi_Batch;
        batchcolfunc := R_DrawColumnHi_Batch;
        if use32bitfuzzeffect then
          batchfuzzcolfunc := R_DrawFuzzColumn32_Batch
        else
          batchfuzzcolfunc := R_DrawFuzzColumnHi_Batch;
        batchlightcolfunc := R_DrawWhiteLightColumnHi_Batch;
        batchwhitelightcolfunc := R_DrawWhiteLightColumnHi_Batch;
        batchredlightcolfunc := R_DrawRedLightColumnHi_Batch;
        batchgreenlightcolfunc := R_DrawGreenLightColumnHi_Batch;
        batchbluelightcolfunc := R_DrawBlueLightColumnHi_Batch;
        batchyellowlightcolfunc := R_DrawYellowLightColumnHi_Batch;
        batchtranscolfunc := R_DrawTranslatedColumnHi_Batch;
        batchtaveragecolfunc := R_DrawColumnAverageHi_Batch;
        batchtalphacolfunc := R_DrawColumnAlphaHi_Batch;
        batchaddcolfunc := R_DrawColumnAddHi_Batch;
        batchsubtractcolfunc := R_DrawColumnSubtractHi_Batch;

        if usemultithread then
        begin
          basebatchcolfunc_mt := R_DrawColumnHi_BatchMT;
          batchcolfunc_mt := R_DrawColumnHi_BatchMT;
          batchtaveragecolfunc_mt := R_DrawColumnAverageHi_BatchMT;
          batchtalphacolfunc_mt := R_DrawColumnAlphaHi_BatchMT;
          batchaddcolfunc_mt := R_DrawColumnAddHi_BatchMT;
          batchsubtractcolfunc_mt := R_DrawColumnSubtractHi_BatchMT;
          maskedcolfunc_mt := nil;
          colfunc_mt := nil;
          averagecolfunc_mt := R_DrawColumnAverageHiMT;
          alphacolfunc_mt := nil;
          addcolfunc_mt := R_DrawColumnAddHiMT;
          addcolfunc_smallstep_mt := R_DrawColumnAddHi_SmallStepMT;
          subtractcolfunc_mt := R_DrawColumnSubtractHiMT;
        end
        else
        begin
          basebatchcolfunc_mt := nil;
          batchcolfunc_mt := nil;
          batchtaveragecolfunc_mt := nil;
          batchtalphacolfunc_mt := nil;
          batchaddcolfunc_mt := nil;
          batchsubtractcolfunc_mt := nil;
          maskedcolfunc_mt := nil;
          colfunc_mt := nil;
          averagecolfunc_mt := nil;
          alphacolfunc_mt := nil;
          addcolfunc_mt := nil;
          addcolfunc_smallstep_mt := nil;
          subtractcolfunc_mt := nil;
        end;

        colfunc := R_DrawColumnHi;
        wallcolfunc := R_DrawColumnUltra;
        basewallcolfunc := R_DrawColumnUltra;
        tallwallcolfunc := R_DrawTallColumnUltra;
        transcolfunc := R_DrawTranslatedColumnHi;
        averagecolfunc := R_DrawColumnAverageHi;
        alphacolfunc := R_DrawColumnAlphaHi;
        addcolfunc := R_DrawColumnAddHi;
        addcolfunc_smallstep := R_DrawColumnAddHi_SmallStep;
        subtractcolfunc := R_DrawColumnSubtractHi;
        maskedcolfunc := R_DrawMaskedColumnHi;
        maskedcolfunc2 := R_DrawMaskedColumnHi32;

        if usemultithread then
        begin
          spanfunc := R_StoreFlatSpan32;
          basespanfunc := R_StoreFlatSpan32;
          ripplespanfunc := R_StoreFlatSpan32;
          slopefunc := R_StoreFlatSpan32;
          baseslopefunc := R_StoreFlatSpan32;
          rippleslopefunc := R_StoreFlatSpan32;

          spanfuncMT := R_DrawSpanNormalMT;
          basespanfuncMT := R_DrawSpanNormalMT;
          ripplespanfuncMT := R_DrawSpanNormal_RippleMT;
          slopefuncMT := R_DrawSpanNormalMT;
          baseslopefuncMT := R_DrawSpanNormalMT;
          rippleslopefuncMT := R_DrawSpanNormal_RippleMT;
        end
        else
        begin
          spanfunc := R_DrawSpanNormal;
          basespanfunc := R_DrawSpanNormal;
          ripplespanfunc := R_DrawSpanNormal_Ripple;
          slopefunc := R_DrawSpanNormal;  // JVAL: Slopes
          baseslopefunc := R_DrawSpanNormal;
          rippleslopefunc := R_DrawSpanNormal_Ripple;
        end;

        if use32bitfuzzeffect then
          fuzzcolfunc := R_DrawFuzzColumn32
        else
          fuzzcolfunc := R_DrawFuzzColumnHi;
        lightcolfunc := R_DrawWhiteLightColumnHi;
        whitelightcolfunc := R_DrawWhiteLightColumnHi;
        redlightcolfunc := R_DrawRedLightColumnHi;
        greenlightcolfunc := R_DrawGreenLightColumnHi;
        bluelightcolfunc := R_DrawBlueLightColumnHi;
        yellowlightcolfunc := R_DrawYellowLightColumnHi;
        skycolfunc := R_DrawSkyColumnHi;
        videomode := vm32bit;
      end;
    DL_ULTRARES:
      begin
        basebatchcolfunc := R_DrawColumnHi_Batch;
        batchcolfunc := R_DrawColumnHi_Batch;
        if use32bitfuzzeffect then
          batchfuzzcolfunc := R_DrawFuzzColumn32_Batch
        else
          batchfuzzcolfunc := R_DrawFuzzColumnHi_Batch;
        batchlightcolfunc := R_DrawWhiteLightColumnHi_Batch;
        batchwhitelightcolfunc := R_DrawWhiteLightColumnHi_Batch;
        batchredlightcolfunc := R_DrawRedLightColumnHi_Batch;
        batchgreenlightcolfunc := R_DrawGreenLightColumnHi_Batch;
        batchbluelightcolfunc := R_DrawBlueLightColumnHi_Batch;
        batchyellowlightcolfunc := R_DrawYellowLightColumnHi_Batch;
        batchtranscolfunc := R_DrawTranslatedColumnHi_Batch;
        batchtaveragecolfunc := R_DrawColumnAverageHi_Batch;
        batchtalphacolfunc := R_DrawColumnAlphaHi_Batch;
        batchaddcolfunc := R_DrawColumnAddHi_Batch;
        batchsubtractcolfunc := R_DrawColumnSubtractHi_Batch;

        if usemultithread then
        begin
          basebatchcolfunc_mt := R_DrawColumnHi_BatchMT;
          batchcolfunc_mt := R_DrawColumnHi_BatchMT;
          batchtaveragecolfunc_mt := R_DrawColumnAverageHi_BatchMT;
          batchtalphacolfunc_mt := R_DrawColumnAlphaHi_BatchMT;
          batchaddcolfunc_mt := R_DrawColumnAddHi_BatchMT;
          batchsubtractcolfunc_mt := R_DrawColumnSubtractHi_BatchMT;
          maskedcolfunc_mt := nil;
          colfunc_mt := nil;
          averagecolfunc_mt := R_DrawColumnAverageHiMT;
          alphacolfunc_mt := nil;
          addcolfunc_mt := R_DrawColumnAddHiMT;
          addcolfunc_smallstep_mt := R_DrawColumnAddHi_SmallStepMT;
          subtractcolfunc_mt := R_DrawColumnSubtractHiMT;
        end
        else
        begin
          basebatchcolfunc_mt := nil;
          batchcolfunc_mt := nil;
          batchtaveragecolfunc_mt := nil;
          batchtalphacolfunc_mt := nil;
          batchaddcolfunc_mt := nil;
          batchsubtractcolfunc_mt := nil;
          maskedcolfunc_mt := nil;
          colfunc_mt := nil;
          averagecolfunc_mt := nil;
          alphacolfunc_mt := nil;
          addcolfunc_mt := nil;
          addcolfunc_smallstep_mt := nil;
          subtractcolfunc_mt := nil;
        end;

        colfunc := R_DrawColumnUltra;
        wallcolfunc := R_DrawColumnUltra;
        basewallcolfunc := R_DrawColumnUltra;
        tallwallcolfunc := R_DrawTallColumnUltra;
        transcolfunc := R_DrawTranslatedColumnHi;
        averagecolfunc := R_DrawColumnAverageUltra;
        addcolfunc := R_DrawColumnAddHi;
        addcolfunc_smallstep := R_DrawColumnAddHi_SmallStep;
        subtractcolfunc := R_DrawColumnSubtractHi;
        alphacolfunc := R_DrawColumnAlphaHi;
        maskedcolfunc := R_DrawMaskedColumnHi;
        maskedcolfunc2 := R_DrawMaskedColumnUltra32;

        if usemultithread then
        begin
          spanfunc := R_StoreFlatSpan32;
          basespanfunc := R_StoreFlatSpan32;
          ripplespanfunc := R_StoreFlatSpan32;
          slopefunc := R_StoreFlatSpan32;
          baseslopefunc := R_StoreFlatSpan32;
          rippleslopefunc := R_StoreFlatSpan32;

          spanfuncMT := R_DrawSpanNormalMT;
          basespanfuncMT := R_DrawSpanNormalMT;
          ripplespanfuncMT := R_DrawSpanNormal_RippleMT;
          slopefuncMT := R_DrawSpanNormalMT;
          baseslopefuncMT := R_DrawSpanNormalMT;
          rippleslopefuncMT := R_DrawSpanNormal_RippleMT;
        end
        else
        begin
          spanfunc := R_DrawSpanNormal; //R_DrawSpanUltra;
          basespanfunc := R_DrawSpanNormal;
          ripplespanfunc := R_DrawSpanNormal_Ripple;
          slopefunc := R_DrawSpanNormal;  // JVAL: Slopes
          baseslopefunc := R_DrawSpanNormal;
          rippleslopefunc := R_DrawSpanNormal_Ripple;
        end;

        if use32bitfuzzeffect then
          fuzzcolfunc := R_DrawFuzzColumn32
        else
          fuzzcolfunc := R_DrawFuzzColumnHi;
        lightcolfunc := R_DrawWhiteLightColumnHi;
        whitelightcolfunc := R_DrawWhiteLightColumnHi;
        redlightcolfunc := R_DrawRedLightColumnHi;
        greenlightcolfunc := R_DrawGreenLightColumnHi;
        bluelightcolfunc := R_DrawBlueLightColumnHi;
        yellowlightcolfunc := R_DrawYellowLightColumnHi;
        skycolfunc := R_DrawSkyColumnUltra;
        videomode := vm32bit;
      end;
  end;

end;
{$ENDIF}

//==============================================================================
//
// R_ExecuteSetViewSize
//
//==============================================================================
procedure R_ExecuteSetViewSize;
var
{$IFNDEF OPENGL}
  cosadj: fixed_t;
  dy, dy1: fixed_t;
{$ENDIF}
  i: integer;
  j: integer;
  level: integer;
  startmap: integer;
  levelhi: integer;
  startmaphi: integer;
begin
  setsizeneeded := false;

  if setblocks > 10 then
  begin
    scaledviewwidth := SCREENWIDTH;
    viewheight := SCREENHEIGHT;
  end
  else
  begin
    if setblocks = 10 then
      scaledviewwidth := SCREENWIDTH
    else
      scaledviewwidth := (setblocks * SCREENWIDTH div 10) and not 7;
    if setblocks = 10 then
    {$IFDEF OPENGL}
      viewheight := trunc(ST_Y * SCREENHEIGHT / 200)
    {$ELSE}
      viewheight := V_PreserveY(ST_Y)
    {$ENDIF}
    else
    {$IFDEF OPENGL}
      viewheight := (setblocks * trunc(ST_Y * SCREENHEIGHT / 2000)) and not 7;
    {$ELSE}
      viewheight := (setblocks * V_PreserveY(ST_Y) div 10) and not 7;
    {$ENDIF}
  end;

  viewwidth := scaledviewwidth;
  centery := viewheight div 2;
  centerx := viewwidth div 2;
  {$IFNDEF OPENGL}
  xfocallen := centerx / tan(fov * ANGLE_T_TO_RAD / 2); // JVAL: Slopes
  {$ENDIF}

  centerxfrac := centerx * FRACUNIT;
  centeryfrac := centery * FRACUNIT;

// JVAL: Widescreen support
  monitor_relative_aspect := R_GetRelativeAspect{$IFNDEF OPENGL} * R_Fake3DAspectCorrection(viewplayer){$ENDIF};
  projection := Round(centerx / monitor_relative_aspect * FRACUNIT);
  projectiony := (((SCREENHEIGHT * centerx * 320) div 200) div SCREENWIDTH * FRACUNIT); // JVAL for correct aspect

  if olddetail <> setdetail then
  begin
    olddetail := setdetail;
{$IFDEF OPENGL}
    videomode := vm32bit;
{$ELSE}
    R_SetRenderingFunctions;
    R_SetPalette64;
{$ENDIF}
  end;

  R_InitBuffer(scaledviewwidth, viewheight);

  R_InitTextureMapping;

// psprite scales
// JVAL: Widescreen support
  pspritescale := Round((centerx / monitor_relative_aspect * FRACUNIT) / 160);
  pspriteyscale := Round((((SCREENHEIGHT * viewwidth) / SCREENWIDTH) * FRACUNIT) / 200);
  pspriteiscale := FixedDiv(FRACUNIT, pspritescale);

  if excludewidescreenplayersprites then
    pspritescalep := Round((centerx * FRACUNIT) / 160)
  else
    pspritescalep := Round((centerx / R_GetRelativeAspect * FRACUNIT) / 160);
  pspriteiscalep := FixedDiv(FRACUNIT, pspritescalep);

  // thing clipping
  for i := 0 to viewwidth - 1 do
    screenheightarray[i] := viewheight;

{$IFNDEF OPENGL}
  // planes
  dy := centeryfrac + FRACUNIT div 2;
  for i := 0 to viewheight - 1 do
  begin
    dy := dy - FRACUNIT;
    dy1 := abs(dy);
    yslope[i] := FixedDiv(projectiony, dy1); // JVAL for correct aspect

    // JVAL: 20200430 - For slope lightmap
    if dy1 < 4 * FRACUNIT then
      slyslope[i] := FixedDiv(projectiony, 4 * FRACUNIT)
    else
      slyslope[i] := yslope[i];
  end;

  for i := 0 to viewwidth - 1 do
  begin
    cosadj := abs(fixedcosine[xtoviewangle[i] div FRACUNIT]);
    distscale[i] := FixedDiv(FRACUNIT, cosadj);
  end;
{$ENDIF}

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

      def_scalelight[i][j] := @def_colormaps[level * 256];
      fog_scalelight[i][j] := @fog_colormaps[level * 256];
    end;
  end;

  if setdetail >= DL_NORMAL then
    for i := 0 to LIGHTLEVELS - 1 do
    begin
      startmaphi := ((LIGHTLEVELS - 1 - i) * 2 * FRACUNIT) div LIGHTLEVELS;
      for j := 0 to HLL_MAXLIGHTSCALE - 1 do
      begin
        levelhi := startmaphi - (j * SCREENWIDTH) div viewwidth * 16;

        if levelhi < 0 then
          scalelightlevels[i][j] := FRACUNIT
        else if levelhi >= FRACUNIT then
          scalelightlevels[i][j] := 1
        else
          scalelightlevels[i][j] := FRACUNIT - levelhi;
      end;
    end;

  R_RecalcColormaps;
end;

//==============================================================================
//
// R_CmdZAxisShift
//
//==============================================================================
procedure R_CmdZAxisShift(const parm1: string = '');
var
  newz: boolean;
begin
  if parm1 = '' then
  begin
    printf('Current setting: zaxisshift = %s.'#13#10, [truefalseStrings[zaxisshift]]);
    exit;
  end;

  newz := C_BoolEval(parm1, zaxisshift);
  if newz <> zaxisshift then
  begin
    zaxisshift := newz;
    setsizeneeded := true;
  end;
  R_CmdZAxisShift;
end;

{$IFNDEF OPENGL}

//==============================================================================
//
// R_CmdUseFake3D
//
//==============================================================================
procedure R_CmdUseFake3D(const parm1: string = '');
var
  newf: boolean;
begin
  if parm1 = '' then
  begin
    printf('Current setting: usefake3d = %s.'#13#10, [truefalseStrings[usefake3d]]);
    exit;
  end;

  newf := C_BoolEval(parm1, usefake3d);
  if newf <> usefake3d then
  begin
    usefake3d := newf;
    R_InitTextureMapping;
    setsizeneeded := true;
  end;
  R_CmdUseFake3D;
end;
{$ENDIF}

//==============================================================================
//
// R_CmdUse32bitfuzzeffect
//
//==============================================================================
procedure R_CmdUse32bitfuzzeffect(const parm1: string = '');
var
  newusefz: boolean;
begin
  if parm1 = '' then
  begin
    printf('Current setting: use32bitfuzzeffect = %s.'#13#10, [truefalseStrings[use32bitfuzzeffect]]);
    exit;
  end;

  newusefz := C_BoolEval(parm1, use32bitfuzzeffect);
  if newusefz <> use32bitfuzzeffect then
  begin
    use32bitfuzzeffect := newusefz;
{$IFNDEF OPENGL}
    R_SetRenderingFunctions;
{$ENDIF}
  end;
  R_CmdUse32bitfuzzeffect;
end;

//==============================================================================
//
// R_CmdDiher8bitTransparency
//
//==============================================================================
procedure R_CmdDiher8bitTransparency(const parm1: string = '');
var
  newdih: boolean;
begin
  if parm1 = '' then
  begin
    printf('Current setting: diher8bittransparency = %s.'#13#10, [truefalseStrings[diher8bittransparency]]);
    exit;
  end;

  newdih := C_BoolEval(parm1, diher8bittransparency);
  if newdih <> diher8bittransparency then
  begin
    diher8bittransparency := newdih;
{$IFNDEF OPENGL}
    R_SetRenderingFunctions;
{$ENDIF}
  end;
  R_CmdDiher8bitTransparency;
end;

//==============================================================================
//
// R_CmdScreenWidth
//
//==============================================================================
procedure R_CmdScreenWidth;
begin
  {$IFDEF OPENGL}
  printf('ScreenWidth = %d.'#13#10, [SCREENWIDTH]);
  {$ELSE}
  printf('ScreenWidth = %d.'#13#10, [WINDOWWIDTH]);
  {$ENDIF}
end;

//==============================================================================
//
// R_CmdScreenHeight
//
//==============================================================================
procedure R_CmdScreenHeight;
begin
  {$IFDEF OPENGL}
  printf('ScreenHeight = %d.'#13#10, [SCREENHEIGHT]);
  {$ELSE}
  printf('ScreenHeight = %d.'#13#10, [WINDOWHEIGHT]);
  {$ENDIF}
end;

//==============================================================================
//
// R_CmdClearCache
//
//==============================================================================
procedure R_CmdClearCache;
begin
  {$IFDEF OPENGL}
  gld_ClearTextureMemory;
  {$ELSE}
  R_Clear32Cache;
  {$ENDIF}
  Z_FreeTags(PU_CACHE, PU_CACHE);
  printf('Texture cache clear'#13#10);
end;

//==============================================================================
//
// R_CmdResetCache
//
//==============================================================================
procedure R_CmdResetCache;
begin
  {$IFNDEF OPENGL}
  R_Reset32Cache;
  {$ENDIF}
  Z_FreeTags(PU_CACHE, PU_CACHE);
  printf('Texture cache reset'#13#10);
end;

//==============================================================================
//
// R_Init
//
//==============================================================================
procedure R_Init;
begin
{$IFNDEF OPENGL}
  printf('R_Init32Cache'#13#10);
  R_Init32Cache;
  printf('R_InitFake3D'#13#10);
  R_InitFake3D;
{$ENDIF}
  printf('R_InitAspect'#13#10);
  R_InitAspect;
  printf('R_InitData'#13#10);
  R_InitData;
  printf('R_InitCustomColormaps'#13#10);
  R_InitCustomColormaps;
  printf('R_InitRippleEffects'#13#10);
  R_InitRippleEffects;
  printf('R_InitInterpolations'#13#10);
  R_InitInterpolations;
  printf('R_InitPointToAngle'#13#10);
  R_InitPointToAngle;
  printf('R_InitTables'#13#10);
  R_InitTables;
  printf('R_SetViewSize'#13#10);
  // viewwidth / viewheight / detailLevel are set by the defaults
  R_SetViewSize;
  printf('R_InitPlanes'#13#10);
  R_InitPlanes;
  printf('R_InitLightTables'#13#10);
  R_InitLightTables;
  printf('R_InitSkyMap'#13#10);
  R_InitSkyMap;
  printf('R_InitTranslationsTables'#13#10);
  R_InitTranslationTables;
  printf('R_InitTranslations'#13#10);
  R_InitTranslations;
{$IFNDEF OPENGL}
  printf('R_InitTransparency8Tables'#13#10);
  R_InitTransparency8Tables;
{$ENDIF}
  printf('R_InitPrecalc'#13#10);
  R_InitPrecalc;
{$IFNDEF OPENGL}
  printf('R_InitVoxels'#13#10);
  R_InitVoxels;
  printf('R_InitWallsCache8'#13#10);
  R_InitWallsCache8;
  printf('R_InitWallsCache32'#13#10);
  R_InitWallsCache32;
  printf('R_InitFlatsCache8'#13#10);
  R_InitFlatsCache8;
  printf('R_InitFlatsCache32'#13#10);
  R_InitFlatsCache32;
  printf('R_InitDepthBuffer'#13#10); // JVAL: 3d Floors
  R_InitDepthBuffer;
  printf('R_InitZBuffer'#13#10); // JVAL: version 205
  R_InitZBuffer;
  printf('R_InitDynamicLights'#13#10);
  R_InitDynamicLights;
{$ENDIF}

  framecount := 0;

  C_AddCmd('zaxisshift', @R_CmdZAxisShift);
{$IFNDEF OPENGL}
  C_AddCmd('fake3d, usefake3d', @R_CmdUseFake3D);
{$ENDIF}
  C_AddCmd('lowestres, lowestresolution', @R_CmdLowestRes);
  C_AddCmd('lowres, lowresolution', @R_CmdLowRes);
  C_AddCmd('mediumres, mediumresolution', @R_CmdMediumRes);
  C_AddCmd('normalres, normalresolution', @R_CmdNormalRes);
  C_AddCmd('hires, hiresolution', @R_CmdHiRes);
  C_AddCmd('ultrares, ultraresolution', @R_CmdUltraRes);
  C_AddCmd('detaillevel, displayresolution', @R_CmdDetailLevel);
  C_AddCmd('fullscreen', @R_CmdFullScreen);
  C_AddCmd('extremeflatfiltering', @R_CmdExtremeflatfiltering);
  C_AddCmd('32bittexturepaletteeffects, use32bittexturepaletteeffects', @R_Cmd32bittexturepaletteeffects);
  C_AddCmd('useexternaltextures', @R_CmdUseExternalTextures);
  C_AddCmd('use32bitfuzzeffect', @R_CmdUse32bitfuzzeffect);
  C_AddCmd('diher8bittransparency', @R_CmdDiher8bitTransparency);
  C_AddCmd('lightboostfactor', @R_CmdLightBoostFactor);
  C_AddCmd('screenwidth', @R_CmdScreenWidth);
  C_AddCmd('screenheight', @R_CmdScreenHeight);
  C_AddCmd('clearcache, cleartexturecache', @R_CmdClearCache);
  C_AddCmd('resetcache, resettexturecache', @R_CmdResetCache);
end;

//==============================================================================
//
// R_ShutDown
//
//==============================================================================
procedure R_ShutDown;
begin
  printf(#13#10 + 'R_ShutDownLightBoost');
  R_ShutDownLightBoost;
{$IFNDEF OPENGL}
  printf(#13#10 + 'R_ShutDownLightTexture');
  R_ShutDownLightTexture;
  printf(#13#10 + 'R_ShutDownFake3D');
  R_ShutDownFake3D;
  printf(#13#10 + 'R_ShutDown32Cache');
  R_ShutDown32Cache;
{$ENDIF}
  printf(#13#10 + 'R_ShutDownInterpolation');
  R_ResetInterpolationBuffer;
{$IFNDEF OPENGL}
  printf(#13#10 + 'R_FreeTransparency8Tables');
  R_FreeTransparency8Tables;
{$ENDIF}
  printf('R_ShutDownTranslations'#13#10);
  R_ShutDownTranslations;
  printf(#13#10 + 'R_ShutDownPrecalc');
  R_ShutDownPrecalc;
{$IFNDEF OPENGL}
  printf(#13#10 + 'R_ShutDownWallsCache8');
  R_ShutDownWallsCache8;
  printf(#13#10 + 'R_ShutDownWallsCache32');
  R_ShutDownWallsCache32;
  printf(#13#10 + 'R_ShutDownFlatsCache8');
  R_ShutDownFlatsCache8;
  printf(#13#10 + 'R_ShutDownFlatsCache32');
  R_ShutDownFlatsCache32;
{$ENDIF}
  printf(#13#10 + 'R_ShutDownCustomColormaps');
  R_ShutDownCustomColormaps;
  printf(#13#10 + 'R_ShutDownSprites');
  R_ShutDownSprites;
  printf(#13#10 + 'R_FreeMemory');
  R_FreeMemory;
{$IFNDEF OPENGL}
  printf(#13#10 + 'R_VoxelsDone');
  R_VoxelsDone;
  printf(#13#10 + 'R_ShutDownDepthBuffer'); // JVAL: 3d Floors
  R_ShutDownDepthBuffer;
  printf(#13#10 + 'R_ShutDownZBuffer'); // JVAL: version 205
  R_ShutDownZBuffer;
  printf(#13#10 + 'R_DynamicLightsDone');
  R_DynamicLightsDone;
{$ENDIF}
{$IFDEF OPENGL}
  printf(#13#10 + 'R_ShutDownOpenGL');
  R_ShutDownOpenGL;
{$ENDIF}
  printf(#13#10);
end;

//==============================================================================
// R_PointInSubsectorClassic
//
// R_PointInSubsector
//
//==============================================================================
function R_PointInSubsectorClassic(const x: fixed_t; const y: fixed_t): Psubsector_t;
var
  node: Pnode_t;
  nodenum: LongWord;
begin
  // single subsector is a special case
  if numnodes = 0 then
  begin
    result := @subsectors[0];
    exit;
  end;

  nodenum := numnodes - 1;

  while nodenum and NF_SUBSECTOR_V5 = 0 do
  begin
    node := @nodes[nodenum];
    if R_PointOnSide(x, y, node) then
      nodenum := node.children[1]
    else
      nodenum := node.children[0]
  end;

  result := @subsectors[nodenum and not NF_SUBSECTOR_V5]; // JVAL: glbsp
end;

//==============================================================================
//
// R_PointInSubsector
//
//==============================================================================
function R_PointInSubsector(const x: fixed_t; const y: fixed_t): Psubsector_t;
begin
  result := R_PointInSubsectorPrecalc(x, y);
  if result = nil then
    result := R_PointInSubSectorClassic(x, y);
end;

var
  lastcm: integer = -2;

//==============================================================================
//
// R_SetupFrame
//
//==============================================================================
procedure R_SetupFrame(player: Pplayer_t);
var
  i: integer;
  cy: fixed_t;
{$IFNDEF OPENGL}
  dy, dy1: fixed_t;
{$ENDIF}
  sblocks: integer;
  sec: Psector_t;
  cm: integer;
  vangle: angle_t;
begin
  viewplayer := player;

  viewx := player.mo.x;
  viewy := player.mo.y;
  shiftangle := player.lookdir2;
  viewangle := player.mo.angle + shiftangle * DIR256TOANGLEUNIT + viewangleoffset;
  extralight := player.extralight;
  renderlightscale := (SCREENWIDTH * LIGHTSCALEUNIT) div 320;

  viewz := player.viewz;

  // JVAL: 20220309 - Reset R_PointToAngleEx
  pta_x := 0;
  pta_y := 0;
  pta_ret := 0;

  R_AdjustTeleportZoom(player);
  R_AdjustChaseCamera;
  R_AdjustGlobalEarthQuake(player);

{$IFNDEF OPENGL}
  viewsubsector := R_PointInSubSector(viewx, viewy); // JVAL: 3d Floors
  hasExtraFloors := viewsubsector.sector.midsec >= 0;  // JVAL: 3d Floors
{$ENDIF}

  {$IFDEF OPENGL}
  viewpitch := 0;
  absviewpitch := 0;
  {$ENDIF}
//******************************
// JVAL Enabled z axis shift
  if zaxisshift and ((player.lookdir16 <> 0) or p_justspawned) and (viewangleoffset = 0) then
  begin
    sblocks := screenblocks;
    if sblocks > 11 then
      sblocks := 11;
    cy := Round((viewheight + ((player.lookdir16 * sblocks) / 16) * SCREENHEIGHT / 1000) / 2);   // JVAL Smooth Look Up/Down
    if centery <> cy then
    begin
      centery := cy;
      centeryfrac := centery * FRACUNIT;
      {$IFNDEF OPENGL}
      dy := -centeryfrac - FRACUNIT div 2;
      for i := 0 to viewheight - 1 do
      begin
        dy := dy + FRACUNIT;
        dy1 := abs(dy);
        yslope[i] := FixedDiv(projectiony, dy1);

        // JVAL: 20200430 - For slope lightmap
        if dy1 < 4 * FRACUNIT then
          slyslope[i] := FixedDiv(projectiony, 4 * FRACUNIT)
        else
          slyslope[i] := yslope[i];
      end;
      {$ENDIF}

    end;

{$IFDEF OPENGL}
    viewpitch := player.lookdir;
    absviewpitch := abs(viewpitch);
{$ELSE}
    if usefake3d then
      R_Set3DLookup(player);
{$ENDIF}
  end
  else
    p_justspawned := false;
//******************************

  vangle := viewangle div FRACUNIT;
  viewsin := fixedsine[vangle];
  viewcos := fixedcosine[vangle];
{$IFNDEF OPENGL}
  dviewsin := Sin(viewangle / $FFFFFFFF * 2 * pi);
  dviewcos := Cos(viewangle / $FFFFFFFF * 2 * pi);
// JVAL: Widescreen support
  planerelativeaspect := 320 / 200 * SCREENHEIGHT / SCREENWIDTH * monitor_relative_aspect;
{$ENDIF}

  cm := -1;
  if Psubsector_t(player.mo.subsector).sector.heightsec > -1 then
  begin
    sec := @sectors[Psubsector_t(player.mo.subsector).sector.heightsec];
    if viewz < sec.floorheight then
      cm := sec.bottommap
    else if viewz > sec.ceilingheight then
      cm := sec.topmap
    else
      cm := sec.midmap;
  end;

  if cm >= 0 then
  begin
    customcolormap := @customcolormaps[cm];
    R_RecalcColormaps;
    if cm <> lastcm then
    begin
      zlight := @customcolormap.zlight;
      scalelight := @customcolormap.scalelight;
      colormaps := customcolormap.colormap;
     {$IFNDEF OPENGL}
      V_CalcColorMapPalette;
     {$ENDIF}
      lastcm := cm;
      recalctables32needed := true;
    end;
  end
  else
  begin
    if cm <> lastcm then
    begin
      customcolormap := nil;
      zlight := @def_zlight;
      scalelight := @def_scalelight;
      colormaps := def_colormaps;
     {$IFNDEF OPENGL}
      V_CalcColorMapPalette;
     {$ENDIF}
      lastcm := cm;
      recalctables32needed := true;
    end;
  end;

  fixedcolormapnum := player.fixedcolormap;
  if fixedcolormapnum <> 0 then
  begin
    if customcolormap <> nil then
      fixedcolormap := PByteArray(
        integer(customcolormap.colormap) + fixedcolormapnum * 256)
    else
      fixedcolormap := PByteArray(
        integer(colormaps) + fixedcolormapnum * 256);

    {$IFNDEF OPENGL}
    walllights := @scalelightfixed;
    {$ENDIF}

    for i := 0 to MAXLIGHTSCALE - 1 do
      scalelightfixed[i] := fixedcolormap;
  end
  else
  begin
    fixedcolormap := nil;
  end;

  inc(framecount);
  inc(validcount);
end;

//==============================================================================
//
// R_SetViewAngleOffset
//
//==============================================================================
procedure R_SetViewAngleOffset(const angle: angle_t);
begin
  viewangleoffset := angle;
end;

//==============================================================================
//
// R_FullStOn
//
//==============================================================================
function R_FullStOn: boolean;
begin
  result := setblocks = 11;
end;

//==============================================================================
//
// R_GetColormapLightLevel
//
//==============================================================================
function R_GetColormapLightLevel(const cmap: PByteArray): fixed_t;
var
  m: integer;
begin
  if cmap = nil then
    result := -1
  else
  begin
    // JVAL: Mars fog sectors
    m := (integer(cmap) - integer(colormaps));
    if (m >= 0) and (m <= NUMCOLORMAPS * 256) then
      result := FRACUNIT - (integer(cmap) - integer(colormaps)) div 256 * FRACUNIT div NUMCOLORMAPS
    else
      result := FRACUNIT - (integer(cmap) - integer(fog_colormaps)) div 256 * FRACUNIT div NUMCOLORMAPS;
  end;
end;

//==============================================================================
//
// R_GetColormap32
//
//==============================================================================
function R_GetColormap32(const cmap: PByteArray): PLongWordArray;
var
  m: integer;
begin
  if fixedcolormapnum = INVERSECOLORMAP then
    result := @inversecolormap32
  else if cmap = nil then
    result := @colormaps32[6 * 256] // FuzzLight
  else
  begin
    // JVAL: Mars fog sectors
    m := (integer(cmap) - integer(colormaps));
    if (m >= 0) and (m <= NUMCOLORMAPS * 256) then
      result := @colormaps32[m]
    else
    begin
      m := (integer(cmap) - integer(fog_colormaps));
      result := @fog_colormaps32[m];
    end;
  end;
end;

//
// R_RenderView
//

{$IFNDEF OPENGL}
var
  oldlookdir16: integer = MAXINT;

//==============================================================================
//
// R_Fake3DPrepare
//
//==============================================================================
procedure R_Fake3DPrepare(player: Pplayer_t);
begin
  if oldlookdir16 = player.lookdir16 then
    Exit;

  oldlookdir16 := player.lookdir16;

  viewplayer := player;
  R_ExecuteSetViewSize;
end;

var
  task_clearplanes: integer = -1;
  task_8bitlights: integer = -1;
  task_maskedstuff: integer = -1;

procedure R_MaskedStuffMT;
begin
  R_SortVisSprites;
  R_SetUpDrawSegLists;
  R_PrepareMasked;
end;

//==============================================================================
//
// R_DoRenderPlayerView8_MultiThread
//
//==============================================================================
procedure R_DoRenderPlayerView8_MultiThread(player: Pplayer_t);
begin
  R_Fake3DPrepare(player);
  R_SetupFrame(player);
  task_8bitlights := MT_ScheduleTask(@R_Calc8bitTables);
  MT_ExecutePendingTask(task_8bitlights);

  // Clear buffers.
  R_ClearClipSegs;
  R_ClearDrawSegs;
  R_ClearPlanes;
  R_Wait3DLookup;
  R_Fake3DAdjustPlanes(player);
  R_ClearSprites;

  // Check for completed thread tasks
  TestActiveThreads;

  // check for new console commands.
  NetUpdate;

  R_ClearWallsCache8;

  // The head node is the last node output.
  R_RenderBSPNode(numnodes - 1);

  // Check for completed thread tasks
  TestActiveThreads;

  R_ProjectAdditionalThings;

  R_SortVisSpritesMT;

  task_maskedstuff := MT_ScheduleTask(@R_MaskedStuffMT);
  MT_ExecutePendingTask(task_maskedstuff);

  R_RenderMultiThreadWalls8;

  R_DrawPlanes;

  // Check for completed thread tasks
  TestActiveThreads;

  R_RenderMultiThreadFlats8;

  R_WaitWallsCache8;

  R_DrawFFloorsMultiThread;  // JVAL: 3d Floors

  // Check for completed thread tasks
  TestActiveThreads;

  R_RenderMultiThreadFFloors8;

  MT_WaitTask(task_8bitlights);
  R_SignalPrepareMasked;
  MT_WaitTask(task_maskedstuff);
  R_DrawMasked_MultiThread;

  // Check for completed thread tasks
  TestActiveThreads;

  // Check for new console commands.
  NetUpdate;

  R_Execute3DTransform;

  R_DrawPlayer;

  // Check for new console commands.
  NetUpdate;

  task_clearplanes := MT_ScheduleTask(@R_InitializeVisplanes);
  MT_ExecutePendingTask(task_clearplanes);
end;

//==============================================================================
//
// R_DoRenderPlayerView32_MultiThread
//
//==============================================================================
procedure R_DoRenderPlayerView32_MultiThread(player: Pplayer_t);
begin
  R_Fake3DPrepare(player);
  R_SetupFrame(player);
  R_CalcHiResTables_MultiThread;

  // Clear buffers.
  R_ClearClipSegs;
  R_ClearDrawSegs;
  R_ClearPlanes;
  R_Wait3DLookup;
  R_Fake3DAdjustPlanes(player);
  R_ClearSprites;

  // Check for completed thread tasks
  TestActiveThreads;

  // check for new console commands.
  NetUpdate;

  R_ClearWallsCache32;

  // The head node is the last node output.
  R_RenderBSPNode(numnodes - 1);

  // Check for completed thread tasks
  TestActiveThreads;

  R_ProjectAdditionalThings;

  R_SortVisSpritesMT;

  task_maskedstuff := MT_ScheduleTask(@R_MaskedStuffMT);
  MT_ExecutePendingTask(task_maskedstuff);

  R_RenderMultiThreadWalls32;

  R_DrawPlanes;

  // Check for completed thread tasks
  TestActiveThreads;

  R_RenderMultiThreadFlats32;

  R_WaitWallsCache32;

  R_DrawFFloorsMultiThread;  // JVAL: 3d Floors

  // Check for completed thread tasks
  TestActiveThreads;

  R_RenderMultiThreadFFloors32;

  R_SignalPrepareMasked;
  MT_WaitTask(task_maskedstuff);
  R_DrawMasked_MultiThread;

  // Check for completed thread tasks
  TestActiveThreads;

  // Check for new console commands.
  NetUpdate;

  R_Execute3DTransform;

  R_DrawPlayer;

  // Check for new console commands.
  NetUpdate;

  task_clearplanes := MT_ScheduleTask(@R_InitializeVisplanes);
  MT_ExecutePendingTask(task_clearplanes);
end;
{$ENDIF}

//==============================================================================
//
// R_DoRenderPlayerView_SingleThread
//
//==============================================================================
procedure R_DoRenderPlayerView_SingleThread(player: Pplayer_t);
begin
{$IFNDEF OPENGL}
  R_Fake3DPrepare(player);
{$ENDIF}
  R_SetupFrame(player);

{$IFNDEF OPENGL}
  R_Calc8bitTables;
  R_CalcHiResTables_SingleThread;
{$ENDIF}

  // Clear buffers.
{$IFNDEF OPENGL}
  R_ClearClipSegs;
  R_ClearDrawSegs;
{$ENDIF}
  R_ClearPlanes;
{$IFNDEF OPENGL}
  R_Fake3DAdjustPlanes(player);
{$ENDIF}
  R_ClearSprites;

{$IFDEF OPENGL}
  gld_StartDrawScene; // JVAL OPENGL
{$ENDIF}

  // check for new console commands.
  NetUpdate;

{$IFDEF OPENGL}
  gld_ClipperAddViewRange;
{$ENDIF}

  // The head node is the last node output.
  R_RenderBSPNode(numnodes - 1);

  R_ProjectAdditionalThings;

  // Check for new console commands.
  NetUpdate;

{$IFDEF OPENGL}
  gld_DrawScene(player);

  NetUpdate;

  gld_EndDrawScene;

{$ELSE}
  R_SetUpDrawSegLists;

  R_DrawPlanes;

  // Check for new console commands.
  NetUpdate;

  R_DrawFFloors;  // JVAL: 3d Floors

  R_DrawMasked_SingleThread;

  R_Execute3DTransform;

  R_DrawPlayer;
{$ENDIF}

  // Check for new console commands.
  NetUpdate;

{$IFNDEF OPENGL}
  task_clearplanes := MT_ScheduleTask(@R_InitializeVisplanes);
  MT_ExecutePendingTask(task_clearplanes);
{$ENDIF}
end;

//==============================================================================
//
// R_RenderPlayerView
//
//==============================================================================
procedure R_RenderPlayerView(player: Pplayer_t);
begin
  // new render validcount
  Inc(rendervalidcount);

{$IFNDEF OPENGL}
  MT_WaitTask(task_clearplanes);
  zbufferactive := r_uselightmaps;
  R_SetDrawSegFunctions;  // version 205
  if usemultithread then
  begin
    if videomode = vm8bit then
      R_DoRenderPlayerView8_MultiThread(player)
    else
      R_DoRenderPlayerView32_MultiThread(player);
  end
  else
{$ENDIF}
    R_DoRenderPlayerView_SingleThread(player);
{$IFNDEF OPENGL}
  if zbufferactive then
    R_StopZBuffer;
{$ENDIF}
  if mn_makescreenshot then
    MN_ScreenShotFromBlitBuffer;
end;

//==============================================================================
//
// R_Ticker
//
//==============================================================================
procedure R_Ticker;
begin
  R_InterpolateTicker;
end;

end.

