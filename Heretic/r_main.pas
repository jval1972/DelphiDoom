//------------------------------------------------------------------------------
//
//  DelphiHeretic: A modified and improved Heretic port for Windows
//  based on original Linux Doom as published by "id Software", on
//  Heretic source as published by "Raven" software and DelphiDoom
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
//  Site  : http://sourceforge.net/projects/delphidoom/
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

//
// Utility functions.
//
procedure R_ApplyColormap(const ofs, count: integer; const scrn: integer; const cmap: integer);

function R_PointOnSide(const x: fixed_t; const y: fixed_t; const node: Pnode_t): boolean;

function R_PointOnSegSide(x: fixed_t; y: fixed_t; line: Pseg_t): boolean;

function R_PointOnLineSide(x: fixed_t; y: fixed_t; line: Pline_t): boolean;

function R_PointToAngle(x: fixed_t; y: fixed_t): angle_t;

function R_PointToAngleEx(const x: fixed_t; const y: fixed_t): angle_t;

function R_PointToAngle2(const x1: fixed_t; const y1: fixed_t; const x2: fixed_t; const y2: fixed_t): angle_t;

function R_PointToDist(const x: fixed_t; const y: fixed_t): fixed_t;

function R_PointInSubsectorClassic(const x: fixed_t; const y: fixed_t): Psubsector_t;

function R_PointInSubsector(const x: fixed_t; const y: fixed_t): Psubsector_t;

procedure R_AddPointToBox(const x: integer; const y: integer; box: Pfixed_tArray);


//
// REFRESH - the actual rendering functions.
//

// Called by G_Drawer.
procedure R_RenderPlayerView(player: Pplayer_t);

// Called by startup code.
procedure R_Init;
procedure R_ShutDown;

// Called by M_Responder.
procedure R_SetViewSize;

procedure R_ExecuteSetViewSize;

procedure R_SetViewAngleOffset(const angle: angle_t);

function R_FullStOn: boolean;

function R_StOff: boolean;

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
  batchtalphacolfunc_mt: spritefunc_t;
  batchaddcolfunc_mt: spritefunc_t;
  batchsubtractcolfunc_mt: spritefunc_t;
  maskedcolfunc_mt: spritefunc_t;
  colfunc_mt: spritefunc_t;
  alphacolfunc_mt: spritefunc_t;
  addcolfunc_mt: spritefunc_t;
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
  fog_scalelight: scalelight_t; // JVAL: Mars fog sectors
  scalelight: scalelight_t;

var
  scalelightlevels: array[0..LIGHTLEVELS - 1, 0..HLL_MAXLIGHTSCALE - 1] of fixed_t;
  scalelightfixed: array[0..MAXLIGHTSCALE - 1] of PByteArray;

type
  zlight_t = array[0..LIGHTLEVELS - 1, 0..MAXLIGHTZ - 1] of PByteArray;
  Pzlight_t = ^zlight_t;

var
  fog_zlight: zlight_t;
  zlight: zlight_t;

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

function R_GetColormapLightLevel(const cmap: PByteArray): fixed_t;

function R_GetColormap32(const cmap: PByteArray): PLongWordArray;

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
  i_io,
  mt_utils,
  mn_screenshot,
  m_bbox,
  m_menu,
  m_misc,
  p_setup,
  p_sight,
  p_map,
  p_3dfloors,  // JVAL: 3d floors
  {$IFNDEF OPENGL}
  i_video,
  i_system,
  {$ENDIF}
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
{$IFNDEF OPENGL}
  r_cache_main,
  r_fake3d,
  r_ripple,
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
  r_col_av,
  r_col_al,
  r_col_tr,
  r_draw_additive,
  r_draw_subtractive,
  r_depthbuffer,  // JVAL: 3d Floors
  r_zbuffer, // JVAL: version 205
  v_video,
{$ENDIF}
  r_subsectors,
  v_data,
  sb_bar,
  w_sprite,
  z_zone;

var
// just for profiling purposes
  framecount: integer;

procedure R_ApplyColormap(const ofs, count: integer; const scrn: integer; const cmap: integer);
var
  src: PByte;
  cnt: integer;
  colormap: PByteArray;
begin
  src := PByte(screens[scrn]);
  inc(src, ofs);
  cnt := count;
  colormap := @colormaps[cmap * 256];

  while cnt > 0 do
  begin
    src^ := colormap[src^];
    inc(src);
    dec(cnt);
  end;
end;

//
// R_AddPointToBox
// Expand a given bbox
// so that it encloses a given point.
//
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

//
// R_PointOnSide
// Traverse BSP (sub) tree,
//  check point against partition plane.
// Returns side 0 (front) or 1 (back).
//
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

function R_PointOnSide(const x: fixed_t; const y: fixed_t; const node: Pnode_t): boolean;
begin
  if largemap then
    result := R_PointOnSide64(x, y, node)
  else
    result := R_PointOnSide32(x, y, node);
end;

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

function R_PointOnSegSide(x: fixed_t; y: fixed_t; line: Pseg_t): boolean;
begin
  if largemap then
    result := R_PointOnSegSide64(x, y, line)
  else
    result := R_PointOnSegSide32(x, y, line);
end;

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

function R_PointOnLineSide(x: fixed_t; y: fixed_t; line: Pline_t): boolean;
begin
  if largemap then
    result := R_PointOnLineSide64(x, y, line)
  else
    result := R_PointOnLineSide32(x, y, line)
end;

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

function R_PointToAngleEx(const x: fixed_t; const y: fixed_t): angle_t;
var
  xx, yy: fixed_t;
begin
  xx := x - viewx;
  yy := y - viewy;
  result := Round(arctan2(yy, xx) * (ANG180 / D_PI));
end;

function R_PointToAngle2(const x1: fixed_t; const y1: fixed_t; const x2: fixed_t; const y2: fixed_t): angle_t;
begin
  result := R_PointToAngle(x2 - x1 + viewx, y2 - y1 + viewy);
end;

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

//
// R_InitPointToAngle
//
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

//
// R_InitTables
//
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
//
// R_InitTextureMapping
//
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

//
// R_InitLightTables
// Only inits the zlight table,
//  because the scalelight table changes with view size.
//
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

      zlight[i][j] := PByteArray(integer(colormaps) + level * 256);
      fog_zlight[i][j] := PByteArray(integer(fog_colormaps) + level * 256);
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
procedure R_SetPalette64;
begin
  if setdetail in [DL_LOWEST, DL_LOW, DL_MEDIUM] then
    I_SetPalette64;
end;

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
        batchtaveragecolfunc := nil;
        batchtalphacolfunc := nil;

        if usemultithread then
        begin
          basebatchcolfunc_mt := R_DrawColumnLow_BatchMT;
          batchcolfunc_mt := R_DrawColumnLow_BatchMT;
          batchtalphacolfunc_mt := nil;
          batchaddcolfunc_mt := R_DrawColumnAddMedium_BatchMT;
          batchsubtractcolfunc_mt := R_DrawColumnSubtractMedium_BatchMT;
          maskedcolfunc_mt := nil;
          colfunc_mt := nil;
          alphacolfunc_mt := nil;
          addcolfunc_mt := nil;
          subtractcolfunc_mt := nil;
        end
        else
        begin
          basebatchcolfunc_mt := nil;
          batchcolfunc_mt := nil;
          batchtalphacolfunc_mt := nil;
          batchaddcolfunc_mt := nil;
          batchsubtractcolfunc_mt := nil;
          maskedcolfunc_mt := nil;
          colfunc_mt := nil;
          alphacolfunc_mt := nil;
          addcolfunc_mt := nil;
          subtractcolfunc_mt := nil;
        end;

        colfunc := R_DrawColumnLowest;
        wallcolfunc := R_DrawColumnLowest;
        basewallcolfunc := R_DrawColumnLowest;
        tallwallcolfunc := R_DrawTallColumnLowest;
        transcolfunc := R_DrawTranslatedColumn;
        averagecolfunc := R_DrawColumnLowest;
        alphacolfunc := R_DrawColumnAlphaMedium;
        addcolfunc := R_DrawColumnAddLowest;
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
        batchtaveragecolfunc := nil;
        batchtalphacolfunc := nil;

        if usemultithread then
        begin
          basebatchcolfunc_mt := R_DrawColumnLow_BatchMT;
          batchcolfunc_mt := R_DrawColumnLow_BatchMT;
          batchtalphacolfunc_mt := nil;
          batchaddcolfunc_mt := R_DrawColumnAddMedium_BatchMT;
          batchsubtractcolfunc_mt := R_DrawColumnSubtractMedium_BatchMT;
          maskedcolfunc_mt := nil;
          colfunc_mt := nil;
          alphacolfunc_mt := nil;
          addcolfunc_mt := nil;
          subtractcolfunc_mt := nil;
        end
        else
        begin
          basebatchcolfunc_mt := nil;
          batchcolfunc_mt := nil;
          batchtalphacolfunc_mt := nil;
          batchaddcolfunc_mt := nil;
          batchsubtractcolfunc_mt := nil;
          maskedcolfunc_mt := nil;
          colfunc_mt := nil;
          alphacolfunc_mt := nil;
          addcolfunc_mt := nil;
          subtractcolfunc_mt := nil;
        end;

        colfunc := R_DrawColumnLow;
        wallcolfunc := R_DrawColumnLow;
        basewallcolfunc := R_DrawColumnLow;
        tallwallcolfunc := R_DrawTallColumnLow;
        transcolfunc := R_DrawTranslatedColumn;
        averagecolfunc := R_DrawColumnLow;
        alphacolfunc := R_DrawColumnAlphaMedium;
        addcolfunc := R_DrawColumnAddLow;
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
        batchtaveragecolfunc := nil;
        batchtalphacolfunc := nil;

        if usemultithread then
        begin
          basebatchcolfunc_mt := R_DrawColumnMedium_BatchMT;
          batchcolfunc_mt := R_DrawColumnMedium_BatchMT;
          batchtalphacolfunc_mt := nil;
          batchaddcolfunc_mt := R_DrawColumnAddMedium_BatchMT;
          batchsubtractcolfunc_mt := R_DrawColumnSubtractMedium_BatchMT;
          maskedcolfunc_mt := nil;
          colfunc_mt := nil;
          alphacolfunc_mt := nil;
          addcolfunc_mt := nil;
          subtractcolfunc_mt := nil;
        end
        else
        begin
          basebatchcolfunc_mt := nil;
          batchcolfunc_mt := nil;
          batchtalphacolfunc_mt := nil;
          batchaddcolfunc_mt := nil;
          batchsubtractcolfunc_mt := nil;
          maskedcolfunc_mt := nil;
          colfunc_mt := nil;
          alphacolfunc_mt := nil;
          addcolfunc_mt := nil;
          subtractcolfunc_mt := nil;
        end;

        colfunc := R_DrawColumnMedium;
        wallcolfunc := R_DrawColumnMedium;
        basewallcolfunc := R_DrawColumnMedium;
        tallwallcolfunc := R_DrawTallColumnMedium;
        transcolfunc := R_DrawTranslatedColumn;
        averagecolfunc := R_DrawColumnMedium;
        alphacolfunc := R_DrawColumnAlphaMedium;
        addcolfunc := R_DrawColumnAddMedium;
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
    DL_NORMAL:
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
          batchtalphacolfunc_mt := R_DrawColumnAlphaHi_BatchMT;
          batchaddcolfunc_mt := R_DrawColumnAddHi_BatchMT;
          batchsubtractcolfunc_mt := R_DrawColumnSubtractHi_BatchMT;
          maskedcolfunc_mt := R_DrawMaskedColumnNormalMT;
          colfunc_mt := nil;
          alphacolfunc_mt := nil;
          addcolfunc_mt := nil;
          subtractcolfunc_mt := nil;
        end
        else
        begin
          basebatchcolfunc_mt := nil;
          batchcolfunc_mt := nil;
          batchtalphacolfunc_mt := nil;
          batchaddcolfunc_mt := nil;
          batchsubtractcolfunc_mt := nil;
          maskedcolfunc_mt := nil;
          colfunc_mt := nil;
          alphacolfunc_mt := nil;
          addcolfunc_mt := nil;
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
          batchtalphacolfunc_mt := R_DrawColumnAlphaHi_BatchMT;
          batchaddcolfunc_mt := R_DrawColumnAddHi_BatchMT;
          batchsubtractcolfunc_mt := R_DrawColumnSubtractHi_BatchMT;
          maskedcolfunc_mt := nil;
          colfunc_mt := nil;
          alphacolfunc_mt := nil;
          addcolfunc_mt := nil;
          subtractcolfunc_mt := nil;
        end
        else
        begin
          basebatchcolfunc_mt := nil;
          batchcolfunc_mt := nil;
          batchtalphacolfunc_mt := nil;
          batchaddcolfunc_mt := nil;
          batchsubtractcolfunc_mt := nil;
          maskedcolfunc_mt := nil;
          colfunc_mt := nil;
          alphacolfunc_mt := nil;
          addcolfunc_mt := nil;
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
          batchtalphacolfunc_mt := R_DrawColumnAlphaHi_BatchMT;
          batchaddcolfunc_mt := R_DrawColumnAddHi_BatchMT;
          batchsubtractcolfunc_mt := R_DrawColumnSubtractHi_BatchMT;
          maskedcolfunc_mt := nil;
          colfunc_mt := nil;
          alphacolfunc_mt := nil;
          addcolfunc_mt := nil;
          subtractcolfunc_mt := nil;
        end
        else
        begin
          basebatchcolfunc_mt := nil;
          batchcolfunc_mt := nil;
          batchtalphacolfunc_mt := nil;
          batchaddcolfunc_mt := nil;
          batchsubtractcolfunc_mt := nil;
          maskedcolfunc_mt := nil;
          colfunc_mt := nil;
          alphacolfunc_mt := nil;
          addcolfunc_mt := nil;
          subtractcolfunc_mt := nil;
        end;

        colfunc := R_DrawColumnUltra;
        wallcolfunc := R_DrawColumnUltra;
        basewallcolfunc := R_DrawColumnUltra;
        tallwallcolfunc := R_DrawTallColumnUltra;
        transcolfunc := R_DrawTranslatedColumnHi;
        averagecolfunc := R_DrawColumnAverageUltra;
        addcolfunc := R_DrawColumnAddHi;
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

//
// R_ExecuteSetViewSize
//
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
      viewheight := trunc(SB_Y * SCREENHEIGHT / 200)
    {$ELSE}
      viewheight := V_PreserveY(SB_Y)
    {$ENDIF}
    else
    {$IFDEF OPENGL}
      viewheight := (setblocks * trunc(SB_Y * SCREENHEIGHT / 2000)) and not 7;
    {$ELSE}
      viewheight := (setblocks * V_PreserveY(SB_Y) div 10) and not 7;
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

      scalelight[i][j] := PByteArray(integer(colormaps) + level * 256);
      fog_scalelight[i][j] := PByteArray(integer(fog_colormaps) + level * 256);
    end;
  end;

  if setdetail >= DL_NORMAL then
    for i := 0 to LIGHTLEVELS - 1 do
    begin
      startmaphi := ((LIGHTLEVELS - 1 - i) * 2 * FRACUNIT) div LIGHTLEVELS;
      for j := 0 to HLL_MAXLIGHTSCALE - 1 do
      begin
        levelhi := startmaphi - j * 16 * SCREENWIDTH div viewwidth;

        if levelhi < 0 then
          scalelightlevels[i][j] := FRACUNIT
        else if levelhi >= FRACUNIT then
          scalelightlevels[i][j] := 1
        else
          scalelightlevels[i][j] := FRACUNIT - levelhi;
      end;
    end;

end;


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

procedure R_CmdScreenWidth;
begin
  {$IFDEF OPENGL}
  printf('ScreenWidth = %d.'#13#10, [SCREENWIDTH]);
  {$ELSE}
  printf('ScreenWidth = %d.'#13#10, [WINDOWWIDTH]);
  {$ENDIF}
end;

procedure R_CmdScreenHeight;
begin
  {$IFDEF OPENGL}
  printf('ScreenHeight = %d.'#13#10, [SCREENHEIGHT]);
  {$ELSE}
  printf('ScreenHeight = %d.'#13#10, [WINDOWHEIGHT]);
  {$ENDIF}
end;

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

procedure R_CmdResetCache;
begin
  {$IFNDEF OPENGL}
  R_Reset32Cache;
  {$ENDIF}
  Z_FreeTags(PU_CACHE, PU_CACHE);
  printf('Texture cache reset'#13#10);
end;



//
// R_Init
//
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
{$IFNDEF OPENGL}
  printf('R_InitRippleEffects'#13#10);
  R_InitRippleEffects;
{$ENDIF}
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
  C_AddCmd('lightboostfactor', @R_CmdLightBoostFactor);
  C_AddCmd('screenwidth', @R_CmdScreenWidth);
  C_AddCmd('screenheight', @R_CmdScreenHeight);
  C_AddCmd('clearcache, cleartexturecache', @R_CmdClearCache);
  C_AddCmd('resetcache, resettexturecache', @R_CmdResetCache);
end;

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
  printf(#13#10 + 'R_ShutDownSprites');
  R_ShutDownSprites;
{$IFDEF OPENGL}
  printf(#13#10 + 'R_ShutDownOpenGL');
  R_ShutDownOpenGL;
{$ENDIF}
{$IFNDEF OPENGL}
  printf(#13#10 + 'R_FreeTransparency8Tables');
  R_FreeTransparency8Tables;
{$ENDIF}
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
  printf(#13#10 + 'R_VoxelsDone');
  R_VoxelsDone;
  printf(#13#10 + 'R_ShutDownDepthBuffer'); // JVAL: 3d Floors
  R_ShutDownDepthBuffer;
  printf(#13#10 + 'R_ShutDownZBuffer'); // JVAL: version 205
  R_ShutDownZBuffer;
  printf(#13#10 + 'R_DynamicLightsDone');
  R_DynamicLightsDone;
{$ENDIF}
  printf(#13#10 + 'W_ShutDownSprites'); // JVAL: Images as sprites
  W_ShutDownSprites; // JVAL: Images as sprites
  printf(#13#10);
end;

//
// R_PointInSubsector
//
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

function R_PointInSubsector(const x: fixed_t; const y: fixed_t): Psubsector_t;
begin
  result := R_PointInSubsectorPrecalc(x, y);
  if result = nil then
    result := R_PointInSubSectorClassic(x, y);
end;

//
// R_SetupFrame
//
procedure R_SetupFrame(player: Pplayer_t);
var
  i: integer;
  cy{$IFNDEF OPENGL}, dy, dy1{$ENDIF}: fixed_t;
  sblocks: integer;
  vangle: angle_t;
begin
  viewplayer := player;

  viewx := player.mo.x;
  viewy := player.mo.y;
  shiftangle := player.lookdir2;
  viewangle := player.mo.angle + shiftangle * DIR256TOANGLEUNIT + viewangleoffset;
  extralight := player.extralight;

  viewz := player.viewz;

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

  fixedcolormapnum := player.fixedcolormap;
  if fixedcolormapnum <> 0 then
  begin
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

procedure R_SetViewAngleOffset(const angle: angle_t);
begin
  viewangleoffset := angle;
end;

function R_FullStOn: boolean;
begin
  result := setblocks = 11;
end;

function R_StOff: boolean;
begin
  result := setblocks = 12;
end;

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

function R_GetColormap32(const cmap: PByteArray): PLongWordArray;
var
  m: integer;
begin
  if cmap = nil then
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

procedure R_DoRenderPlayerView8_MultiThread(player: Pplayer_t);
begin
  R_Fake3DPrepare(player);
  R_SetupFrame(player);
  R_Calc8bitTables;

  // Clear buffers.
  R_ClearClipSegs;
  R_ClearDrawSegs;
  R_ClearPlanes;
  R_Wait3DLookup;
  R_Fake3DAdjustPlanes(player);
  R_ClearSprites;

  // check for new console commands.
  NetUpdate;

  R_ClearWallsCache8;

  // The head node is the last node output.
  R_RenderBSPNode(numnodes - 1);

  R_ProjectAdditionalThings;

  R_SortVisSpritesMT;

  R_RenderMultiThreadWalls8;

  R_SetUpDrawSegLists;

  R_DrawPlanes;

  R_RenderMultiThreadFlats8;

  R_WaitWallsCache8;

  R_DrawFFloorsMultiThread;  // JVAL: 3d Floors

  R_RenderMultiThreadFFloors8;

  R_DrawMasked_MultiThread;

  // Check for new console commands.
  NetUpdate;

  R_Execute3DTransform;

  R_DrawPlayer;

  // Check for new console commands.
  NetUpdate;

  task_clearplanes := MT_ScheduleTask(@R_InitializeVisplanes);
  MT_ExecutePendingTask(task_clearplanes);
end;

procedure R_DoRenderPlayerView32_MultiThread(player: Pplayer_t);
begin
  R_Fake3DPrepare(player);
  R_CalcHiResTables_MultiThread;
  R_SetupFrame(player);

  // Clear buffers.
  R_ClearClipSegs;
  R_ClearDrawSegs;
  R_ClearPlanes;
  R_Wait3DLookup;
  R_Fake3DAdjustPlanes(player);
  R_ClearSprites;

  // check for new console commands.
  NetUpdate;

  R_ClearWallsCache32;

  // The head node is the last node output.
  R_RenderBSPNode(numnodes - 1);

  R_ProjectAdditionalThings;

  R_SortVisSpritesMT;

  R_RenderMultiThreadWalls32;

  R_SetUpDrawSegLists;

  R_DrawPlanes;

  R_RenderMultiThreadFlats32;

  R_WaitWallsCache32;

  R_DrawFFloorsMultiThread;  // JVAL: 3d Floors

  R_RenderMultiThreadFFloors32;

  R_DrawMasked_MultiThread;

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

procedure R_DoRenderPlayerView_SingleThread(player: Pplayer_t);
begin
{$IFNDEF OPENGL}
  R_Fake3DPrepare(player);
  R_Calc8bitTables;
  R_CalcHiResTables_SingleThread;
{$ENDIF}

  R_SetupFrame(player);

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

procedure R_Ticker;
begin
  R_InterpolateTicker;
end;

end.

