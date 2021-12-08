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
//  Refresh module, drawing LineSegs from BSP.
//  All the clipping: columns, horizontal spans, sky columns.
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit r_segs;

interface

uses
  d_delphi,
  m_fixed,
  tables,
  r_defs;

{$IFNDEF OPENGL}
procedure R_RenderMaskedSegRange(const ds: Pdrawseg_t; const x1, x2: integer);

procedure R_StoreWallRange(const start: integer; const stop: integer);
{$ENDIF}

var
// angle to line origin
  rw_angle1: angle_t;

  rw_normalangle: angle_t;

//
// Regular world public
//
  rw_distance: fixed_t;

  walllights: PBytePArray;

//
// regular wall
//
{$IFNDEF OPENGL}
var
  HEIGHTBITS: integer = 12;
  HEIGHTUNIT: integer = 1 shl 12;
  WORLDBITS: integer = 4;
  WORLDUNIT: integer = 1 shl 4;

var
  rw_x: integer;
  rw_stopx: integer;
  rw_centerangle: angle_t;
  rw_offset: fixed_t;
  rw_scale: fixed_t;
  rw_scalestep: fixed_t;
  rw_midtexturemid: fixed_t;
  rw_toptexturemid: fixed_t;
  rw_bottomtexturemid: fixed_t;

  worldtop: integer;
  worldbottom: integer;
  worldhigh: integer;
  worldlow: integer;

  pixhigh: int64; // R_WiggleFix
  pixlow: int64; // R_WiggleFix
  pixhighstep: fixed_t;
  pixlowstep: fixed_t;

  topfrac: int64; // R_WiggleFix
  topstep: fixed_t;

  bottomfrac: int64; // R_WiggleFix
  bottomstep: fixed_t;
{$ENDIF}


{$IFNDEF OPENGL}
var
  maskedtexturecol: PSmallIntArray; // JVAL: declared in r_defs

// True if any of the segs textures might be visible.
  segtextured: boolean;

// False if the back side is the same plane.
  markfloor: boolean;
  markceiling: boolean;

  maskedtexture: boolean;
  toptexture: integer;
  bottomtexture: integer;
  midtexture: integer;

  rw_toptextureheight: integer;
  rw_bottomtextureheight: integer;
  rw_midtextureheight: integer;

  topwallcolfunc: PProcedure;
  bottomwallcolfunc: PProcedure;
  midwallcolfunc: PProcedure;
{$ENDIF}

function R_NewDrawSeg: Pdrawseg_t;

{$IFNDEF OPENGL}
procedure R_WiggleFix(sec: Psector_t);
{$ENDIF}

var
  r_fakecontrast: boolean;

implementation

uses
  i_system,
  g_game,
  r_bsp,
{$IFNDEF OPENGL}
  doomdef,
  doomdata,
  doomtype,
  p_setup, // JVAL: 3d floors
  r_3dfloors, // JVAL: 3d floors
  r_slopes, // JVAL: Slopes
  r_column,
  r_data,
  r_hires,
  r_main,
  r_things,
  r_plane,
  r_draw,
  r_sky,
  r_cache_walls,
  r_wall8,
  r_wall32,
  r_scale,
  r_segs2,
{$IFDEF STRIFE}
  r_col_fz,
{$ENDIF}
  r_patch,
{$ENDIF}
{$IFDEF DEBUG}
  r_debug,
{$ENDIF}
  z_zone;


{$IFNDEF OPENGL}


//
// R_FixWiggle()
// Dynamic wall/texture rescaler, AKA "WiggleHack II"
//  by Kurt "kb1" Baumgardner ("kb")
//
//  [kb] When the rendered view is positioned, such that the viewer is
//   looking almost parallel down a wall, the result of the scale
//   calculation in R_ScaleFromGlobalAngle becomes very large. And, the
//   taller the wall, the larger that value becomes. If these large
//   values were used as-is, subsequent calculations would overflow
//   and crash the program.
//
//  Therefore, vanilla Doom clamps this scale calculation, preventing it
//   from becoming larger than 0x400000 (64*FRACUNIT). This number was
//   chosen carefully, to allow reasonably-tight angles, with reasonably
//   tall sectors to be rendered, within the limits of the fixed-point
//   math system being used. When the scale gets clamped, Doom cannot
//   properly render the wall, causing an undesirable wall-bending
//   effect that I call "floor wiggle".
//
//  Modern source ports offer higher video resolutions, which worsens
//   the issue. And, Doom is simply not adjusted for the taller walls
//   found in many PWADs.
//
//  WiggleHack II attempts to correct these issues, by dynamically
//   adjusting the fixed-point math, and the maximum scale clamp,
//   on a wall-by-wall basis. This has 2 effects:
//
//  1. Floor wiggle is greatly reduced and/or eliminated.
//  2. Overflow is not longer possible, even in levels with maximum
//     height sectors.
//
//  It is not perfect across all situations. Some floor wiggle can be
//   seen, and some texture strips may be slight misaligned in extreme
//   cases. These effects cannot be corrected without increasing the
//   precision of various renderer variables, and, possibly, suffering
//   a performance penalty.
//

var
  lastheight: integer = 0;

type
  wiggle_t = record
    clamp: integer;
    heightbits: integer;
  end;

var
  scale_values: array[0..8] of wiggle_t = (
    (clamp: 2048 * FRACUNIT; heightbits: 12),
    (clamp: 1024 * FRACUNIT; heightbits: 12),
    (clamp: 1024 * FRACUNIT; heightbits: 11),
    (clamp:  512 * FRACUNIT; heightbits: 11),
    (clamp:  512 * FRACUNIT; heightbits: 10),
    (clamp:  256 * FRACUNIT; heightbits: 10),
    (clamp:  256 * FRACUNIT; heightbits:  9),
    (clamp:  128 * FRACUNIT; heightbits:  9),
    (clamp:   64 * FRACUNIT; heightbits:  9)
  );

procedure R_WiggleFix(sec: Psector_t);
var
  height: integer;
begin
  height := (sec.ceilingheight - sec.floorheight) shr FRACBITS;

  // disallow negative heights, force cache initialization
  if height < 1 then
    height := 1;

  // early out?
  if height <> lastheight then
  begin
    lastheight := height;

    // initialize, or handle moving sector
    if height <> sec.cachedheight then
    begin
      frontsector.cachedheight := height;
      frontsector.scaleindex := 0;
      height := height shr 7;
      // calculate adjustment
      while true do
      begin
        height := height shr 1;
        if height <> 0 then
          inc(frontsector.scaleindex)
        else
          break;
      end;
    end;

    // fine-tune renderer for this wall
    max_rwscale := scale_values[frontsector.scaleindex].clamp;
    HEIGHTBITS := scale_values[frontsector.scaleindex].heightbits;
    HEIGHTUNIT := 1 shl HEIGHTBITS;
    WORLDBITS := 16 - HEIGHTBITS;
    WORLDUNIT := 1 shl WORLDBITS;
  end;
end;


// OPTIMIZE: closed two sided lines as single sided

//
// R_RenderMaskedSegRange
//
procedure R_RenderMaskedSegRange(const ds: Pdrawseg_t; const x1, x2: integer);
var
  index: integer;
  col: Pcolumn_t;
  lightnum: integer;
  texnum: integer;
  i: integer;
  use32bittexture: boolean;
  mc2height: integer;
  texturecolumn: integer;
  t: double;
{$IFDEF DOOM_OR_STRIFE}
  tempsec: sector_t;
{$ENDIF}
begin
  R_DisableFixedColumn;
  // Calculate light table.
  // Use different light tables
  //   for horizontal / vertical / diagonal. Diagonal?
  // OPTIMIZE: get rid of LIGHTSEGSHIFT globally
  curline := ds.curline;
  frontsector := curline.frontsector;
  backsector := curline.backsector;
  texnum := texturetranslation[curline.sidedef.midtexture];

  R_GetDCs(texnum, 0); // JVAL Also precache external texture if not loaded
  use32bittexture := (videomode = vm32bit) and (integer(textures[texnum].texture32) > $1);
  if use32bittexture then
  begin
    mc2height := textures[texnum].height;
    {$IFNDEF HEXEN}
    if curline.linedef.renderflags and LRF_TRANSPARENT <> 0 then
      colfunc := averagecolfunc
    {$IFDEF STRIFE}
    // villsa [STRIFE] render as transparent (25% or 75%?)
    else if curline.linedef.flags and ML_TRANSPARENT2 <> 0 then
      colfunc := @R_DrawFuzzColumn2Hi32
    else if curline.linedef.flags and ML_TRANSPARENT1 <> 0 then
      colfunc := @R_DrawFuzzColumn1Hi32
    {$ENDIF}
    else
    {$ENDIF}
      colfunc := maskedcolfunc2;
  end
  else
  begin
    mc2height := 0;
    {$IFNDEF HEXEN}
    if curline.linedef.renderflags and LRF_TRANSPARENT <> 0 then
      colfunc := averagecolfunc
    {$IFDEF STRIFE}
    else if curline.linedef.flags and ML_TRANSPARENT2 <> 0 then
      colfunc := fuzzcolfunc2
    else if curline.linedef.flags and ML_TRANSPARENT1 <> 0 then
      colfunc := fuzzcolfunc1
    {$ENDIF}
    else
    {$ENDIF}
      colfunc := maskedcolfunc;
  end;

  lightnum := _SHR(
        {$IFDEF DOOM_OR_STRIFE}R_FakeFlat(frontsector, @tempsec, nil, nil, False).lightlevel
        {$ELSE}frontsector.lightlevel
        {$ENDIF},
        LIGHTSEGSHIFT) + extralight;

  if r_fakecontrast then
  begin
    if curline.v1.y = curline.v2.y then
      dec(lightnum)
    else if curline.v1.x = curline.v2.x then
      inc(lightnum);
  end;

  if lightnum < 0 then
    lightnum := 0
  else if lightnum >= LIGHTLEVELS then
    lightnum := LIGHTLEVELS - 1;

  if curline.sidedef.sector.renderflags and SRF_FOG <> 0 then // JVAL: Mars fog sectors
  begin
    walllights := @fog_scalelight[lightnum];
    dc_fog := true;
  end
  else
  begin
    walllights := @scalelight[lightnum];
    dc_fog := false;
  end;

  dc_llindex := lightnum;

  maskedtexturecol := ds.maskedtexturecol;

  if ds.use_double then
  begin
    rw_scalestep_dbl := ds.scalestep_dbl;
    spryscale := trunc(ds.scale_dbl + (x1 - ds.x1) * rw_scalestep_dbl);
  end
  else
  begin
    rw_scalestep := ds.scalestep;
    spryscale := ds.scale1 + (x1 - ds.x1) * rw_scalestep;
  end;
  mfloorclip := ds.sprbottomclip;
  mceilingclip := ds.sprtopclip;

  // find positioning
  if curline.linedef.flags and ML_DONTPEGBOTTOM <> 0 then
  begin
    if frontsector.floorheight > backsector.floorheight then
      dc_texturemid := frontsector.floorheight
    else
      dc_texturemid := backsector.floorheight;
    dc_texturemid := dc_texturemid + textureheight[texnum] - viewz;
  end
  else
  begin
    if frontsector.ceilingheight < backsector.ceilingheight then
      dc_texturemid := frontsector.ceilingheight
    else
      dc_texturemid := backsector.ceilingheight;
    dc_texturemid := dc_texturemid - viewz;
  end;
  dc_texturemid := dc_texturemid + curline.sidedef.rowoffset;

  if fixedcolormap <> nil then
    dc_colormap := fixedcolormap;

  if videomode = vm32bit then
  begin
    dc_colormap32 := R_GetColormap32(dc_colormap);
    if fixedcolormapnum = INVERSECOLORMAP then
      dc_lightlevel := -1
    else
      dc_lightlevel := R_GetColormapLightLevel(dc_colormap);
  end;

  // draw the columns
  for i := x1 to x2 do
  begin
    dc_x := i;
    // calculate lighting
    if maskedtexturecol[dc_x] <> MAXSHORT then
    begin
      if fixedcolormap = nil then
      begin
        {$IFDEF HEXEN}
        if videomode = vm32bit then
        begin
          if not forcecolormaps or LevelUseFog then
        {$ELSE}
        if not forcecolormaps then
        {$ENDIF}
        begin
          if spryscale > 256 * FRACUNIT then
            index := (_SHR(spryscale, HLL_LIGHTSCALESHIFT + 2) div SCREENWIDTH) * 320
          else
            index := (_SHR(spryscale, HLL_LIGHTSCALESHIFT + 2) * 320) div SCREENWIDTH;
          if index >= HLL_MAXLIGHTSCALE then
            index := HLL_MAXLIGHTSCALE - 1
          else if index < 0 then
            index := 0;
          dc_lightlevel := scalelightlevels[dc_llindex, index];
        end;
        {$IFDEF HEXEN}
        end;
        {$ENDIF}
        index := _SHR(spryscale, LIGHTSCALESHIFT) * 320 div SCREENWIDTH;

        if index >=  MAXLIGHTSCALE then
          index := MAXLIGHTSCALE - 1
        else if index < 0 then
          index := 0;

        dc_colormap := walllights[index];
        if videomode = vm32bit then
          dc_colormap32 := R_GetColormap32(dc_colormap);
      end;

      // killough 3/2/98:
      //
      // This calculation used to overflow and cause crashes in Doom:
      //
      // sprtopscreen = centeryfrac - FixedMul(dcvars.texturemid, spryscale);
      //
      // This code fixes it, by using double-precision intermediate
      // arithmetic and by skipping the drawing of 2s normals whose
      // mapping to screen coordinates is totally out of range:
     //t -> int64 (boom)
 {     t := int64(centeryfrac) * FRACUNIT - int64(dc_texturemid) * int64(spryscale);
      if (t + int64(textureheight[texnum]) * int64(spryscale) < 0) or (t > int64(SCREENHEIGHT) * FRACUNIT * 2) then
        continue;
      sprtopscreen := FixedInt64(t); }

     //t -> double (delphidoom)
      t := (centeryfrac / FRACUNIT) - (dc_texturemid / FRACUNIT) * (spryscale / FRACUNIT);
      if (t + (textureheight[texnum] / FRACUNIT) * (spryscale / FRACUNIT) < 0) or (t > SCREENHEIGHT * 2) then
      begin
        if ds.use_double then
          spryscale := Trunc(ds.scale_dbl + (dc_x - ds.x1) * rw_scalestep_dbl)
        else
          spryscale := spryscale + rw_scalestep;
        continue;
      end;
      sprtopscreen := trunc(t * FRACUNIT);

{     original Doom code
      sprtopscreen := centeryfrac - FixedMul(dc_texturemid, spryscale); }

      dc_iscale := LongWord($ffffffff) div LongWord(spryscale);

      texturecolumn := maskedtexturecol[dc_x] shr DC_HIRESBITS;
      if use32bittexture then
      begin
        dc_mod := 0;
        dc_texturemod := maskedtexturecol[dc_x] and (DC_HIRESFACTOR - 1);
        R_GetDCs(texnum, texturecolumn);
        R_DrawMaskedColumn2(mc2height);
      end
      else
      begin
        // draw the texture
        col := Pcolumn_t(integer(R_GetColumn(texnum, texturecolumn)) - 3);
        R_DrawMaskedColumn(col, spryscale);
      end;

      maskedtexturecol[dc_x] := MAXSHORT;
    end;
    if ds.use_double then
      spryscale := Trunc(ds.scale_dbl + (dc_x - ds.x1) * rw_scalestep_dbl)
    else
      spryscale := spryscale + rw_scalestep;
  end;
  R_EnableFixedColumn;
end;
{$ENDIF}

// JVAL: Drawseg buffers
const
  NUMDRAWSEGBUFFERS = 16;
  DRAWSEGBUFFERSIZE = MAXDRAWSEGS div NUMDRAWSEGBUFFERS;

type
  drawsegbuffer_t = array[0..DRAWSEGBUFFERSIZE - 1] of drawseg_t;
  Pdrawsegbuffer_t = ^drawsegbuffer_t;

var
  drawsegbuffers: array[0..NUMDRAWSEGBUFFERS - 1] of Pdrawsegbuffer_t;

var
  rndsmap: integer = -1;
{$IFNDEF STRIFE}
  rndsepi: integer = -1;
{$ENDIF}

function R_NewDrawSeg: Pdrawseg_t;
var
  i, bufid: integer;
begin
  // don't overflow and crash
  if ds_p = MAXDRAWSEGS then
  begin
    if (rndsmap <> gamemap) {$IFNDEF STRIFE}or (rndsepi <> gameepisode){$ENDIF} then
      I_Warning('R_NewDrawSeg(): ds_p = MAXDRAWSEGS'#13#10);
    rndsmap := gamemap;
    {$IFNDEF STRIFE}
    rndsepi := gameepisode;
    {$ENDIF}
    result := nil;
    exit;
  end;

  // JVAL
  // Now drawsegs is an array of pointer to drawseg_t
  // Dynamically allocation using zone
  if ds_p > max_ds_p then
  begin
    bufid := ds_p div DRAWSEGBUFFERSIZE;
    drawsegbuffers[bufid] := Z_Malloc(SizeOf(drawseg_t) * DRAWSEGBUFFERSIZE, PU_LEVEL, nil);
    for i := 0 to DRAWSEGBUFFERSIZE - 1 do
      drawsegs[bufid * DRAWSEGBUFFERSIZE + i] := @drawsegbuffers[bufid][i];
    max_ds_p := (bufid + 1) * DRAWSEGBUFFERSIZE - 1;
  end;
  result := drawsegs[ds_p];
  {$IFNDEF OPENGL}
  result.use_double := false; // JVAL: 3d Floors
  {$ENDIF}
end;

//
// R_StoreWallRange
// A wall segment will be drawn
//  between start and stop pixels (inclusive).
//
{$IFNDEF OPENGL}
procedure R_StoreWallRange_DBL(const pds: Pdrawseg_t; const start: integer; const stop: integer);
var
  vtop: fixed_t;
  lightnum: integer;
  lightnum2: integer;
  rw_scale_dbl2: Double;
  worldtop_dbl: Double;
  worldbottom_dbl: Double;
  sec2: Psector_t;
begin
  // calculate scale at both ends and step
  rw_scale_dbl := R_ScaleFromGlobalAngle_DBL(viewangle + xtoviewangle[start]);
  rw_scale := Round(rw_scale_dbl);

  pds.use_double := true;
  pds.scale1 := rw_scale;
  pds.scale_dbl := rw_scale_dbl;

  if stop > start then
  begin
    rw_scale_dbl2 := R_ScaleFromGlobalAngle_DBL(viewangle + xtoviewangle[stop]);
    rw_scalestep_dbl := (rw_scale_dbl2 - rw_scale_dbl) / (stop - start);
    pds.scale2 := Round(rw_scale_dbl2);
    rw_scalestep := Round(rw_scalestep_dbl);
    pds.scalestep := rw_scalestep;
    pds.scalestep_dbl := rw_scalestep_dbl;
  end
  else
  begin
    pds.scale2 := pds.scale1;
    pds.scalestep_dbl := 0.0;
  end;

  // calculate texture boundaries
  //  and decide if floor / ceiling marks are needed
  worldtop := frontsector.ceilingheight - viewz;
  worldbottom := frontsector.floorheight - viewz;

  R_WiggleFix(frontsector);

  midtexture := 0;
  toptexture := 0;
  bottomtexture := 0;
  maskedtexture := false;
  pds.maskedtexturecol := nil;
  pds.thicksidecol := nil;  // JVAL: 3d Floors
  pds.midsec := nil;        // JVAL: 3d Floors

  if backsector = nil then
  begin
    // single sided line
    midtexture := texturetranslation[sidedef.midtexture];
    rw_midtextureheight := texturecolumnheight[midtexture];
    if rw_midtextureheight = 128 then
      midwallcolfunc  := basewallcolfunc
    else
      midwallcolfunc := tallwallcolfunc;
    // a single sided line is terminal, so it must mark ends
    markfloor := true;
    markceiling := true;
    if linedef.flags and ML_DONTPEGBOTTOM <> 0 then
    begin
      vtop := frontsector.floorheight + textureheight[sidedef.midtexture];
      // bottom of texture at bottom
      rw_midtexturemid := vtop - viewz;
    end
    else
    begin
      // top of texture at top
      rw_midtexturemid := worldtop;
    end;
    rw_midtexturemid := rw_midtexturemid + FixedMod(sidedef.rowoffset, textureheight[midtexture]);
    rw_midtexturemid := FixedMod(rw_midtexturemid, texturecolumnheightfrac[midtexture]);

    pds.silhouette := SIL_BOTH;
    pds.sprtopclip := @screenheightarray;
    pds.sprbottomclip := @negonearray;
    pds.bsilheight := MAXINT;
    pds.tsilheight := MININT;
  end
  else
  begin
    // two sided line
    pds.sprtopclip := nil;
    pds.sprbottomclip := nil;
    pds.silhouette := 0;

    if frontsector.floorheight > backsector.floorheight then
    begin
      pds.silhouette := SIL_BOTTOM;
      pds.bsilheight := frontsector.floorheight;
    end
    else if backsector.floorheight > viewz then
    begin
      pds.silhouette := SIL_BOTTOM;
      pds.bsilheight := MAXINT;
    end;

    if frontsector.ceilingheight < backsector.ceilingheight then
    begin
      pds.silhouette := pds.silhouette or SIL_TOP;
      pds.tsilheight := frontsector.ceilingheight;
    end
    else if backsector.ceilingheight < viewz then
    begin
      pds.silhouette := pds.silhouette or SIL_TOP;
      pds.tsilheight := MININT;
    end;

    // killough 1/17/98: this test is required if the fix
    // for the automap bug (r_bsp.c) is used, or else some
    // sprites will be displayed behind closed doors. That
    // fix prevents lines behind closed doors with dropoffs
    // from being displayed on the automap.
    //
    // killough 4/7/98: make doorclosed external variable

    if doorclosed or (backsector.ceilingheight <= frontsector.floorheight) then
    begin
      pds.sprbottomclip := @negonearray;
      pds.bsilheight := MAXINT;
      pds.silhouette := pds.silhouette or SIL_BOTTOM;
    end;

    if doorclosed or (backsector.floorheight >= frontsector.ceilingheight) then
    begin                   // killough 1/17/98, 2/8/98
      pds.sprtopclip := @screenheightarray;
      pds.tsilheight := MININT;
      pds.silhouette := pds.silhouette or SIL_TOP;
    end;

    worldhigh := backsector.ceilingheight - viewz;
    worldlow := backsector.floorheight - viewz;

    // hack to allow height changes in outdoor areas
    if (frontsector.ceilingpic = skyflatnum) and
       (backsector.ceilingpic = skyflatnum) then
    begin
      worldtop := worldhigh;
    end;

    // JVAL: 20200118
    if curline.frontsector = curline.backsector then
    begin
      markfloor := false;
      markceiling := false;
    end
    else if (backsector.ceilingheight <= frontsector.floorheight) or
       (backsector.floorheight >= frontsector.ceilingheight) then
    begin
      // closed door
      markceiling := true;
      markfloor := true;
    end
    else
    begin
      markfloor := (worldlow <> worldbottom) or
                   (backsector.floorpic <> frontsector.floorpic) or
                   (backsector.lightlevel <> frontsector.lightlevel) or
                   (backsector.floorangle <> frontsector.floorangle) or
                   (backsector.flooranglex <> frontsector.flooranglex) or
                   (backsector.floorangley <> frontsector.floorangley) or
                   {$IFDEF DOOM_OR_STRIFE}
                   // killough 3/7/98: Add checks for (x,y) offsets
                   (backsector.floor_xoffs <> frontsector.floor_xoffs) or
                   (backsector.floor_yoffs <> frontsector.floor_yoffs) or
                   // killough 4/15/98: prevent 2s normals
                   // from bleeding through deep water
                   (frontsector.heightsec <> -1) or
                   // killough 4/17/98: draw floors if different light levels
                   (backsector.floorlightsec <> frontsector.floorlightsec) or
                   {$ENDIF}
                   {$IFDEF HEXEN}
                   (backsector.special <> frontsector.special) or
                   {$ENDIF}
                   ((frontsector.midsec <> backsector.midsec) and (frontsector.tag <> backsector.tag)) or // JVAL: 3d floors
                   (backsector.renderflags <> frontsector.renderflags);

      markceiling := (worldhigh <> worldtop) or
                     (backsector.ceilingpic <> frontsector.ceilingpic) or
                     (backsector.lightlevel <> frontsector.lightlevel) or
                     (backsector.ceilingangle <> frontsector.ceilingangle) or
                     (backsector.ceilinganglex <> frontsector.ceilinganglex) or
                     (backsector.ceilingangley <> frontsector.ceilingangley) or
                     {$IFDEF DOOM_OR_STRIFE}
                     // killough 3/7/98: Add checks for (x,y) offsets
                     (backsector.ceiling_xoffs <> frontsector.ceiling_xoffs) or
                     (backsector.ceiling_yoffs <> frontsector.ceiling_yoffs) or
                     // killough 4/15/98: prevent 2s normals
                     // from bleeding through fake ceilings
                     ((frontsector.heightsec <> -1) and (frontsector.ceilingpic <> skyflatnum)) or
                     // killough 4/17/98: draw ceilings if different light levels
                     (backsector.ceilinglightsec <> frontsector.ceilinglightsec) or
                     {$ENDIF}
                     ((frontsector.midsec <> backsector.midsec) and (frontsector.tag <> backsector.tag)) or // JVAL: 3d floors
                     (backsector.renderflags <> frontsector.renderflags);
    end;

    if worldhigh < worldtop then
    begin
      // top texture
      toptexture := texturetranslation[sidedef.toptexture];
      rw_toptextureheight := texturecolumnheight[toptexture];
      if rw_toptextureheight = 128 then
        topwallcolfunc  := basewallcolfunc
      else
        topwallcolfunc := tallwallcolfunc;
      if linedef.flags and ML_DONTPEGTOP <> 0 then
      begin
        // top of texture at top
        rw_toptexturemid := worldtop;
      end
      else
      begin
        vtop := backsector.ceilingheight + textureheight[sidedef.toptexture];

        // bottom of texture
        rw_toptexturemid := vtop - viewz;
      end
    end;

    if worldlow > worldbottom then
    begin
      // bottom texture
      bottomtexture := texturetranslation[sidedef.bottomtexture];
      rw_bottomtextureheight := texturecolumnheight[bottomtexture];
      if rw_bottomtextureheight = 128 then
        bottomwallcolfunc  := basewallcolfunc
      else
        bottomwallcolfunc := tallwallcolfunc;
      if linedef.flags and ML_DONTPEGBOTTOM <> 0 then
      begin
        // bottom of texture at bottom
        // top of texture at top
        rw_bottomtexturemid := worldtop;
      end
      else // top of texture at top
        rw_bottomtexturemid := worldlow;
    end;
    rw_toptexturemid := rw_toptexturemid + FixedMod(sidedef.rowoffset, textureheight[toptexture]);
    rw_toptexturemid := FixedMod(rw_toptexturemid, texturecolumnheightfrac[toptexture]);
    rw_bottomtexturemid := rw_bottomtexturemid + FixedMod(sidedef.rowoffset, textureheight[bottomtexture]);
    rw_bottomtexturemid := FixedMod(rw_bottomtexturemid, texturecolumnheightfrac[bottomtexture]);

    // JVAL: 3d Floors
    R_StoreThickSideRange(pds, frontsector, backsector);

    // allocate space for masked texture tables
    if sidedef.midtexture <> 0 then
    begin
      // masked midtexture
      maskedtexture := true;
      maskedtexturecol := PSmallIntArray(@openings[lastopening - rw_x]);
      pds.maskedtexturecol := maskedtexturecol;
      lastopening := lastopening + rw_stopx - rw_x;
    end;
  end;

  // calculate rw_offset (only needed for textured lines)
  segtextured := ((midtexture or toptexture or bottomtexture) <> 0) or maskedtexture;

  if segtextured then
  begin
    rw_offset := R_CalcSegOffset(curline);

    if LongWord(rw_normalangle - rw_angle1) < ANG180 then
      rw_offset := -rw_offset;

    rw_offset := rw_offset + sidedef.textureoffset + curline.offset;
    rw_centerangle := ANG90 + viewangle - rw_normalangle;

    // calculate light table
    //  use different light tables
    //  for horizontal / vertical / diagonal
    // OPTIMIZE: get rid of LIGHTSEGSHIFT globally
    if fixedcolormap = nil then
    begin
      sec2 := frontsector;
      lightnum := _SHR(sec2.lightlevel, LIGHTSEGSHIFT) + extralight;

      if r_fakecontrast then
      begin
        if curline.v1.y = curline.v2.y then
          dec(lightnum)
        else if curline.v1.x = curline.v2.x then
          inc(lightnum);
      end;

      if lightnum < 0 then
        lightnum := 0
      else if lightnum >= LIGHTLEVELS then
        lightnum := LIGHTLEVELS - 1;

      if sec2.renderflags and SRF_FOG <> 0 then // JVAL: Mars fog sectors
      begin
        walllights := @fog_scalelight[lightnum];
        dc_fog := true;
      end
      else
      begin
        walllights := @scalelight[lightnum];
        dc_fog := false;
      end;

      dc_llindex := lightnum;

      // JVAL: 3d Floors
      if frontsector.midsec >= 0 then
      begin
        sec2 := @sectors[frontsector.midsec];
        lightnum2 := _SHR(sec2.lightlevel, LIGHTSEGSHIFT) + extralight;
        if r_fakecontrast then
        begin
          if curline.v1.y = curline.v2.y then
            dec(lightnum2)
          else if curline.v1.x = curline.v2.x then
            inc(lightnum2);
        end;

        if lightnum2 < 0 then
          lightnum2 := 0
        else if lightnum2 >= LIGHTLEVELS then
          lightnum2 := LIGHTLEVELS - 1;

        if sec2.renderflags and SRF_FOG <> 0 then // JVAL: Mars fog sectors
        begin
          walllights2 := @fog_scalelight[lightnum2];
          dc_fog2 := true;
        end
        else
        begin
          walllights2 := @scalelight[lightnum2];
          dc_fog2 := false;
        end;

        dc_llindex2 := lightnum2;
      end
      else if pds.midsec <> nil then
      begin
        sec2 := pds.midsec;
        lightnum2 := _SHR(sec2.lightlevel, LIGHTSEGSHIFT) + extralight;
        if r_fakecontrast then
        begin
          if curline.v1.y = curline.v2.y then
            dec(lightnum2)
          else if curline.v1.x = curline.v2.x then
            inc(lightnum2);
        end;

        if lightnum2 < 0 then
          lightnum2 := 0
        else if lightnum2 >= LIGHTLEVELS then
          lightnum2 := LIGHTLEVELS - 1;

        if sec2.renderflags and SRF_FOG <> 0 then // JVAL: Mars fog sectors
        begin
          walllights2 := @fog_scalelight[lightnum2];
          dc_fog2 := true;
        end
        else
        begin
          walllights2 := @scalelight[lightnum2];
          dc_fog2 := false;
        end;

        dc_llindex2 := lightnum2;
      end
      else if pds.midvis <> nil then
      begin
        if backsector <> nil then
        begin
          if backsector.midsec >= 0 then
          begin
            sec2 := @sectors[backsector.midsec];
            lightnum2 := _SHR(sec2.lightlevel, LIGHTSEGSHIFT) + extralight
          end
          else
          begin
            sec2 := backsector;
            lightnum2 := _SHR(sec2.lightlevel, LIGHTSEGSHIFT) + extralight;
          end;
        end
        else
        begin
          sec2 := PSubsector_t(pds.midvis.ssector).sector; // JVAL: 20211112 - Fix lightlevel
          lightnum2 := _SHR(sec2.lightlevel, LIGHTSEGSHIFT) + extralight;
        end;
        if r_fakecontrast then
        begin
          if curline.v1.y = curline.v2.y then
            dec(lightnum2)
          else if curline.v1.x = curline.v2.x then
            inc(lightnum2);
        end;

        if lightnum2 < 0 then
          lightnum2 := 0
        else if lightnum2 >= LIGHTLEVELS then
          lightnum2 := LIGHTLEVELS - 1;

        if sec2.renderflags and SRF_FOG <> 0 then // JVAL: Mars fog sectors
        begin
          walllights2 := @fog_scalelight[lightnum2];
          dc_fog2 := true;
        end
        else
        begin
          walllights2 := @scalelight[lightnum2];
          dc_fog2 := false;
        end;

        dc_llindex2 := lightnum2;
      end;

    end;
  end;

  // if a floor / ceiling plane is on the wrong side
  //  of the view plane, it is definitely invisible
  //  and doesn't need to be marked.

  {$IFDEF DOOM_OR_STRIFE}
  // killough 3/7/98: add deep water check
  if frontsector.heightsec = -1 then
  {$ENDIF}
  begin
    if frontsector.floorheight >= viewz then
    begin
      // above view plane
      markfloor := false;
    end;

    if (frontsector.ceilingheight <= viewz) and
       (frontsector.ceilingpic <> skyflatnum) then
    begin
      // below view plane
      markceiling := false;
    end;
  end;

  // calculate incremental stepping values for texture edges
  worldtop_dbl := worldtop / WORLDUNIT;
  worldbottom_dbl := worldbottom / WORLDUNIT;
  worldtop := worldtop div WORLDUNIT;
//  worldtop := worldtop shr WORLDBITS;
  worldbottom := worldbottom div WORLDUNIT;
//  worldbottom := worldbottom shr WORLDBITS;

  topstep_dbl := - rw_scalestep_dbl / FRACUNIT * worldtop_dbl;

  topfrac_dbl := (centeryfrac / WORLDUNIT) - worldtop_dbl / FRACUNIT * rw_scale_dbl;

  bottomstep_dbl := - rw_scalestep_dbl / FRACUNIT * worldbottom_dbl;

  bottomfrac_dbl := (centeryfrac / WORLDUNIT) - worldbottom_dbl / FRACUNIT * rw_scale_dbl;

  topstep := round(topstep_dbl);
  topfrac := round(topfrac_dbl);
  bottomstep := round(bottomstep_dbl);
  bottomfrac := round(bottomfrac_dbl);


  if backsector <> nil then
  begin
    worldhigh_dbl := worldhigh / WORLDUNIT;
    worldlow_dbl := worldlow / WORLDUNIT;
    worldhigh := worldhigh div WORLDUNIT;
//    worldhigh := worldhigh shr WORLDBITS;
    worldlow := worldlow div WORLDUNIT;
//    worldlow := worldlow shr WORLDBITS;

    if worldhigh_dbl < worldtop_dbl then
    begin
      pixhigh := (centeryfrac div WORLDUNIT) - FixedMul(worldhigh, rw_scale);
//      pixhigh := (centeryfrac shr WORLDBITS) - FixedMul(worldhigh, rw_scale);
      pixhighstep := -FixedMul(rw_scalestep, worldhigh);
      pixhigh_dbl := (centeryfrac / WORLDUNIT) - worldhigh_dbl / FRACUNIT * rw_scale_dbl;
      pixhighstep_dbl := -rw_scalestep_dbl / FRACUNIT * worldhigh_dbl;
    end;

    if worldlow_dbl > worldbottom_dbl then
    begin
      pixlow := (centeryfrac div WORLDUNIT) - FixedMul(worldlow, rw_scale);
//      pixlow := (centeryfrac shr WORLDBITS) - FixedMul(worldlow, rw_scale);
      pixlowstep := -FixedMul(rw_scalestep, worldlow);
      pixlow_dbl := (centeryfrac / WORLDUNIT) - worldlow_dbl / FRACUNIT * rw_scale_dbl;
      pixlowstep_dbl := -rw_scalestep_dbl / FRACUNIT * worldlow_dbl;
    end;
  end;

  // render it
  if markceiling then
  begin
    if ceilingplane <> nil then
      ceilingplane := R_CheckPlane(ceilingplane, rw_x, rw_stopx - 1)
    else
      markceiling := false;
  end;

  if markfloor then
  begin
    if floorplane <> nil then
    begin
      // cph 2003/04/18  - ceilingplane and floorplane might be the same
      // visplane (e.g. if both skies); R_CheckPlane doesn't know about
      // modifications to the plane that might happen in parallel with the check
      // being made, so we have to override it and split them anyway if that is
      // a possibility, otherwise the floor marking would overwrite the ceiling
      // marking, resulting in HOM.
      if markceiling and (ceilingplane = floorplane) then
        floorplane := R_DupPlane(floorplane, rw_x, rw_stopx - 1)
      else
        floorplane := R_CheckPlane(floorplane, rw_x, rw_stopx - 1);
    end
    else
      markfloor := false;
  end;

  if pds.midvis <> nil then
  begin
    if pds.midsec <> nil then
    begin
      f_RenderSegLoop_dbl_3dFloors_Vis(pds);  // version 205
    end
    else
    begin
      f_RenderSegLoop_dbl_Vis(pds);  // version 205
    end;
  end
  else
  begin
    if pds.midsec <> nil then
    begin
      f_RenderSegLoop_dbl_3dFloors(pds);  // version 205
    end
    else
    begin
      f_RenderSegLoop_dbl;  // version 205
    end;
  end;

  // JVAL: Changed to fix accuracy for masked textures
  // This fixes some glitches in 2s lines with midtexture (eg BOOMEDIT.WAD)
  if maskedtexture then
  begin
    rw_scale := R_ScaleFromGlobalAngle_Fixed(viewangle + xtoviewangle[start]);
    pds.scale1 := rw_scale;

    if stop > start then
    begin
      pds.scale2 := R_ScaleFromGlobalAngle_Fixed(viewangle + xtoviewangle[stop]);
      rw_scalestep := (pds.scale2 - rw_scale) div (stop - start);
      pds.scalestep := rw_scalestep;
    end
    else
    begin
      pds.scale2 := pds.scale1;
      pds.scalestep_dbl := 0.0;
    end;
  end;

  // save sprite clipping info
  if ((pds.silhouette and SIL_TOP <> 0) or maskedtexture) and
     (pds.sprtopclip = nil) then
  begin
    memcpy(@openings[lastopening], @ceilingclip[start], SizeOf(ceilingclip[0]) * (rw_stopx - start));
    {$IFDEF DEBUG}
    R_CheckClipTable(@ceilingclip, start, rw_stopx - 1);
    {$ENDIF}
    pds.sprtopclip := PSmallIntArray(@openings[lastopening - start]);
    lastopening := lastopening + rw_stopx - start;
  end;

  if ((pds.silhouette and SIL_BOTTOM <> 0) or maskedtexture) and
     (pds.sprbottomclip = nil) then
  begin
    memcpy(@openings[lastopening], @floorclip[start], SizeOf(floorclip[0]) * (rw_stopx - start));
    {$IFDEF DEBUG}
    R_CheckClipTable(@floorclip, start, rw_stopx - 1);
    {$ENDIF}
    pds.sprbottomclip := PSmallIntArray(@openings[lastopening - start]);
    lastopening := lastopening + rw_stopx - start;
  end;

  if maskedtexture and (pds.silhouette and SIL_TOP = 0) then
  begin
    pds.silhouette := pds.silhouette or SIL_TOP;
    pds.tsilheight := MININT;
  end;
  if maskedtexture and (pds.silhouette and SIL_BOTTOM = 0) then
  begin
    pds.silhouette := pds.silhouette or SIL_BOTTOM;
    pds.bsilheight := MAXINT;
  end;
  inc(ds_p);
end;

//
// R_StoreWallRange
// A wall segment will be drawn
//  between start and stop pixels (inclusive).
//
procedure R_StoreWallRange(const start: integer; const stop: integer);
var
  vtop: fixed_t;
  lightnum: integer;
  lightnum2: integer; // JVAL: 3d Floors
  pds: Pdrawseg_t;
  overflow: boolean;
  sec2: Psector_t;
begin
  if curline.linedef.renderflags and LRF_SLOPED <> 0 then
  begin
    R_StoreSlopeRange(start, stop); // JVAL: Slopes
    Exit;
  end;

  pds := R_NewDrawSeg;
  if pds = nil then
    exit;
  pds.curline := curline;
  pds.midvis := curmidvis;  // JVAL: 3d Floors

  sidedef := curline.sidedef;
  linedef := curline.linedef;

  // mark the segment as visible for auto map
  linedef.flags := linedef.flags or ML_MAPPED;

  // calculate rw_distance for scale calculation
  rw_normalangle := curline.angle + ANG90;

  rw_distance := R_DistToSeg(curline);

  rw_x := start;
  pds.x1 := rw_x;
  pds.x2 := stop;
  rw_stopx := stop + 1;

  // JVAL: 20200417 - Use double arithmetic for large segs
  if curline.map_length >= 1024 then
  begin
    R_StoreWallRange_DBL(pds, start, stop);
    exit;
  end;

  // calculate scale at both ends and step
  rw_scale := R_ScaleFromGlobalAngle(viewangle + xtoviewangle[start], overflow);
  if overflow or (precisescalefromglobalangle and (pds.midsec <> nil) and (rw_scale > 64 * FRACUNIT)) then
  begin
    R_StoreWallRange_DBL(pds, start, stop);
    exit;
  end;

  pds.scale1 := rw_scale;

  if stop > start then
  begin
    pds.scale2 := R_ScaleFromGlobalAngle(viewangle + xtoviewangle[stop], overflow);
    if overflow or (precisescalefromglobalangle and (pds.midsec <> nil) and (pds.scale2 > 64 * FRACUNIT)) then
    begin
      R_StoreWallRange_DBL(pds, start, stop);
      exit;
    end;
    rw_scalestep := (pds.scale2 - rw_scale) div (stop - start);
    pds.scalestep := rw_scalestep
  end
  else
  begin
    pds.scale2 := pds.scale1;
  end;

  // calculate texture boundaries
  //  and decide if floor / ceiling marks are needed
  worldtop := frontsector.ceilingheight - viewz;
  worldbottom := frontsector.floorheight - viewz;

  R_WiggleFix(frontsector);

  midtexture := 0;
  toptexture := 0;
  bottomtexture := 0;
  maskedtexture := false;
  pds.maskedtexturecol := nil;
  pds.thicksidecol := nil;  // JVAL: 3d Floors
  pds.midsec := nil;        // JVAL: 3d Floors

  if backsector = nil then
  begin
    // single sided line
    midtexture := texturetranslation[sidedef.midtexture];
    rw_midtextureheight := texturecolumnheight[midtexture];
    if rw_midtextureheight = 128 then
      midwallcolfunc  := basewallcolfunc
    else
      midwallcolfunc := tallwallcolfunc;
    // a single sided line is terminal, so it must mark ends
    markfloor := true;
    markceiling := true;
    if linedef.flags and ML_DONTPEGBOTTOM <> 0 then
    begin
      vtop := frontsector.floorheight + textureheight[sidedef.midtexture];
      // bottom of texture at bottom
      rw_midtexturemid := vtop - viewz;
    end
    else
    begin
      // top of texture at top
      rw_midtexturemid := worldtop;
    end;
    rw_midtexturemid := rw_midtexturemid + FixedMod(sidedef.rowoffset, textureheight[midtexture]);
    rw_midtexturemid := FixedMod(rw_midtexturemid, texturecolumnheightfrac[midtexture]);

    pds.silhouette := SIL_BOTH;
    pds.sprtopclip := @screenheightarray;
    pds.sprbottomclip := @negonearray;
    pds.bsilheight := MAXINT;
    pds.tsilheight := MININT;
  end
  else
  begin
    // two sided line
    pds.sprtopclip := nil;
    pds.sprbottomclip := nil;
    pds.silhouette := 0;

    if frontsector.floorheight > backsector.floorheight then
    begin
      pds.silhouette := SIL_BOTTOM;
      pds.bsilheight := frontsector.floorheight;
    end
    else if backsector.floorheight > viewz then
    begin
      pds.silhouette := SIL_BOTTOM;
      pds.bsilheight := MAXINT;
    end;

    if frontsector.ceilingheight < backsector.ceilingheight then
    begin
      pds.silhouette := pds.silhouette or SIL_TOP;
      pds.tsilheight := frontsector.ceilingheight;
    end
    else if backsector.ceilingheight < viewz then
    begin
      pds.silhouette := pds.silhouette or SIL_TOP;
      pds.tsilheight := MININT;
    end;

    // killough 1/17/98: this test is required if the fix
    // for the automap bug (r_bsp.c) is used, or else some
    // sprites will be displayed behind closed doors. That
    // fix prevents lines behind closed doors with dropoffs
    // from being displayed on the automap.
    //
    // killough 4/7/98: make doorclosed external variable

    if doorclosed or (backsector.ceilingheight <= frontsector.floorheight) then
    begin
      pds.sprbottomclip := @negonearray;
      pds.bsilheight := MAXINT;
      pds.silhouette := pds.silhouette or SIL_BOTTOM;
    end;

    if doorclosed or (backsector.floorheight >= frontsector.ceilingheight) then
    begin                   // killough 1/17/98, 2/8/98
      pds.sprtopclip := @screenheightarray;
      pds.tsilheight := MININT;
      pds.silhouette := pds.silhouette or SIL_TOP;
    end;

    worldhigh := backsector.ceilingheight - viewz;
    worldlow := backsector.floorheight - viewz;

    // hack to allow height changes in outdoor areas
    if (frontsector.ceilingpic = skyflatnum) and
       (backsector.ceilingpic = skyflatnum) then
    begin
      worldtop := worldhigh;
    end;

    // JVAL: 20200118
    if curline.frontsector = curline.backsector then
    begin
      markfloor := false;
      markceiling := false;
    end
    else if (backsector.ceilingheight <= frontsector.floorheight) or
       (backsector.floorheight >= frontsector.ceilingheight) then
    begin
      // closed door
      markceiling := true;
      markfloor := true;
    end
    else
    begin
      markfloor := (worldlow <> worldbottom) or
                   (backsector.floorpic <> frontsector.floorpic) or
                   (backsector.lightlevel <> frontsector.lightlevel) or
                   (backsector.floorangle <> frontsector.floorangle) or
                   (backsector.flooranglex <> frontsector.flooranglex) or
                   (backsector.floorangley <> frontsector.floorangley) or
                   {$IFDEF DOOM_OR_STRIFE}
                   // killough 3/7/98: Add checks for (x,y) offsets
                   (backsector.floor_xoffs <> frontsector.floor_xoffs) or
                   (backsector.floor_yoffs <> frontsector.floor_yoffs) or
                   // killough 4/15/98: prevent 2s normals
                   // from bleeding through deep water
                   (frontsector.heightsec <> -1) or
                   // killough 4/17/98: draw floors if different light levels
                   (backsector.floorlightsec <> frontsector.floorlightsec) or
                   {$ENDIF}
                   {$IFDEF HEXEN}
                   (backsector.special <> frontsector.special) or
                   {$ENDIF}
                   ((frontsector.midsec <> backsector.midsec) and (frontsector.tag <> backsector.tag)) or // JVAL: 3d floors
                   (backsector.renderflags <> frontsector.renderflags);

      markceiling := (worldhigh <> worldtop) or
                     (backsector.ceilingpic <> frontsector.ceilingpic) or
                     (backsector.lightlevel <> frontsector.lightlevel) or
                     (backsector.ceilingangle <> frontsector.ceilingangle) or
                     (backsector.ceilinganglex <> frontsector.ceilinganglex) or
                     (backsector.ceilingangley <> frontsector.ceilingangley) or
                     {$IFDEF DOOM_OR_STRIFE}
                     // killough 3/7/98: Add checks for (x,y) offsets
                     (backsector.ceiling_xoffs <> frontsector.ceiling_xoffs) or
                     (backsector.ceiling_yoffs <> frontsector.ceiling_yoffs) or
                     // killough 4/15/98: prevent 2s normals
                     // from bleeding through fake ceilings
                     ((frontsector.heightsec <> -1) and (frontsector.ceilingpic <> skyflatnum)) or
                     // killough 4/17/98: draw ceilings if different light levels
                     (backsector.ceilinglightsec <> frontsector.ceilinglightsec) or
                     {$ENDIF}
                     ((frontsector.midsec <> backsector.midsec) and (frontsector.tag <> backsector.tag)) or // JVAL: 3d floors
                     (backsector.renderflags <> frontsector.renderflags);
    end;

    if worldhigh < worldtop then
    begin
      // top texture
      toptexture := texturetranslation[sidedef.toptexture];
      rw_toptextureheight := texturecolumnheight[toptexture];
      if rw_toptextureheight = 128 then
        topwallcolfunc  := basewallcolfunc
      else
        topwallcolfunc := tallwallcolfunc;
      if linedef.flags and ML_DONTPEGTOP <> 0 then
      begin
        // top of texture at top
        rw_toptexturemid := worldtop;
      end
      else
      begin
        vtop := backsector.ceilingheight + textureheight[sidedef.toptexture];

        // bottom of texture
        rw_toptexturemid := vtop - viewz;
      end
    end;

    if worldlow > worldbottom then
    begin
      // bottom texture
      bottomtexture := texturetranslation[sidedef.bottomtexture];
      rw_bottomtextureheight := texturecolumnheight[bottomtexture];
      if rw_bottomtextureheight = 128 then
        bottomwallcolfunc  := basewallcolfunc
      else
        bottomwallcolfunc := tallwallcolfunc;
      if linedef.flags and ML_DONTPEGBOTTOM <> 0 then
      begin
        // bottom of texture at bottom
        // top of texture at top
        rw_bottomtexturemid := worldtop;
      end
      else // top of texture at top
        rw_bottomtexturemid := worldlow;
    end;
    rw_toptexturemid := rw_toptexturemid + FixedMod(sidedef.rowoffset, textureheight[toptexture]);
    rw_toptexturemid := FixedMod(rw_toptexturemid, texturecolumnheightfrac[toptexture]);
    rw_bottomtexturemid := rw_bottomtexturemid + FixedMod(sidedef.rowoffset, textureheight[bottomtexture]);
    rw_bottomtexturemid := FixedMod(rw_bottomtexturemid, texturecolumnheightfrac[bottomtexture]);

    // JVAL: 3d Floors
    R_StoreThickSideRange(pds, frontsector, backsector);

    // allocate space for masked texture tables
    if sidedef.midtexture <> 0 then
    begin
      // masked midtexture
      maskedtexture := true;
      maskedtexturecol := PSmallIntArray(@openings[lastopening - rw_x]);
      pds.maskedtexturecol := maskedtexturecol;
      lastopening := lastopening + rw_stopx - rw_x;
    end;
  end;

  // calculate rw_offset (only needed for textured lines)
  segtextured := ((midtexture or toptexture or bottomtexture) <> 0) or maskedtexture;

  if segtextured then
  begin
    rw_offset := R_CalcSegOffset(curline);

    if LongWord(rw_normalangle - rw_angle1) < ANG180 then
      rw_offset := -rw_offset;

    rw_offset := rw_offset + sidedef.textureoffset + curline.offset;
    rw_centerangle := ANG90 + viewangle - rw_normalangle;

    // calculate light table
    //  use different light tables
    //  for horizontal / vertical / diagonal
    // OPTIMIZE: get rid of LIGHTSEGSHIFT globally
    if fixedcolormap = nil then
    begin
      sec2 := frontsector;
      lightnum := _SHR(sec2.lightlevel, LIGHTSEGSHIFT) + extralight;

      if r_fakecontrast then
      begin
        if curline.v1.y = curline.v2.y then
          dec(lightnum)
        else if curline.v1.x = curline.v2.x then
          inc(lightnum);
      end;

      if lightnum < 0 then
        lightnum := 0
      else if lightnum >= LIGHTLEVELS then
        lightnum := LIGHTLEVELS - 1;

      if sec2.renderflags and SRF_FOG <> 0 then // JVAL: Mars fog sectors
      begin
        walllights := @fog_scalelight[lightnum];
        dc_fog := true;
      end
      else
      begin
        walllights := @scalelight[lightnum];
        dc_fog := false;
      end;

      dc_llindex := lightnum;

      // JVAL: 3d Floors
      if frontsector.midsec >= 0 then
      begin
        sec2 := @sectors[frontsector.midsec];
        lightnum2 := _SHR(sec2.lightlevel, LIGHTSEGSHIFT) + extralight;
        if r_fakecontrast then
        begin
          if curline.v1.y = curline.v2.y then
            dec(lightnum2)
          else if curline.v1.x = curline.v2.x then
            inc(lightnum2);
        end;

        if lightnum2 < 0 then
          lightnum2 := 0
        else if lightnum2 >= LIGHTLEVELS then
          lightnum2 := LIGHTLEVELS - 1;

        if sec2.renderflags and SRF_FOG <> 0 then // JVAL: Mars fog sectors
        begin
          walllights2 := @fog_scalelight[lightnum2];
          dc_fog2 := true;
        end
        else
        begin
          walllights2 := @scalelight[lightnum2];
          dc_fog2 := false;
        end;

        dc_llindex2 := lightnum2;
      end
      else

      // JVAL: 3d Floors
      if pds.midsec <> nil then
      begin
        sec2 := pds.midsec;
        lightnum2 := _SHR(sec2.lightlevel, LIGHTSEGSHIFT) + extralight;
        if r_fakecontrast then
        begin
          if curline.v1.y = curline.v2.y then
            dec(lightnum2)
          else if curline.v1.x = curline.v2.x then
            inc(lightnum2);
        end;

        if lightnum2 < 0 then
          lightnum2 := 0
        else if lightnum2 >= LIGHTLEVELS then
          lightnum2 := LIGHTLEVELS - 1;

        if sec2.renderflags and SRF_FOG <> 0 then // JVAL: Mars fog sectors
        begin
          walllights2 := @fog_scalelight[lightnum2];
          dc_fog2 := true;
        end
        else
        begin
          walllights2 := @scalelight[lightnum2];
          dc_fog2 := false;
        end;

        dc_llindex2 := lightnum2;
      end
      else if pds.midvis <> nil then // SOS DEBUG!
      begin
        if backsector <> nil then
        begin
          if backsector.midsec >= 0 then
          begin
            sec2 := @sectors[backsector.midsec];
            lightnum2 := _SHR(sec2.lightlevel, LIGHTSEGSHIFT) + extralight
          end
          else
          begin
            sec2 := backsector;
            lightnum2 := _SHR(sec2.lightlevel, LIGHTSEGSHIFT) + extralight;
          end;
        end
        else
        begin
          sec2 := PSubsector_t(pds.midvis.ssector).sector; // JVAL: 20211112 - Fix lightlevel
          lightnum2 := _SHR(sec2.lightlevel, LIGHTSEGSHIFT) + extralight;
        end;
        if r_fakecontrast then
        begin
          if curline.v1.y = curline.v2.y then
            dec(lightnum2)
          else if curline.v1.x = curline.v2.x then
            inc(lightnum2);
        end;

        if lightnum2 < 0 then
          lightnum2 := 0
        else if lightnum2 >= LIGHTLEVELS then
          lightnum2 := LIGHTLEVELS - 1;

        if sec2.renderflags and SRF_FOG <> 0 then // JVAL: Mars fog sectors
        begin
          walllights2 := @fog_scalelight[lightnum2];
          dc_fog2 := true;
        end
        else
        begin
          walllights2 := @scalelight[lightnum2];
          dc_fog2 := false;
        end;

        dc_llindex2 := lightnum2;
      end;

    end;
  end;

  // if a floor / ceiling plane is on the wrong side
  //  of the view plane, it is definitely invisible
  //  and doesn't need to be marked.

  {$IFDEF DOOM_OR_STRIFE}
  // killough 3/7/98: add deep water check
  if frontsector.heightsec = -1 then
  {$ENDIF}
  begin
    if frontsector.floorheight >= viewz then
    begin
      // above view plane
      markfloor := false;
    end;

    if (frontsector.ceilingheight <= viewz) and
       (frontsector.ceilingpic <> skyflatnum) then
    begin
      // below view plane
      markceiling := false;
    end;
  end;

  // calculate incremental stepping values for texture edges
  worldtop := worldtop div WORLDUNIT;
//  worldtop := worldtop shr WORLDBITS;
  worldbottom := worldbottom div WORLDUNIT;
//  worldbottom := worldbottom shr WORLDBITS;

  topstep := - FixedMul(rw_scalestep, worldtop);

  topfrac := (int64(centeryfrac) div WORLDUNIT) - (int64(worldtop) * int64(rw_scale)) shr FRACBITS; // R_WiggleFix

  bottomstep := - FixedMul(rw_scalestep, worldbottom);

  bottomfrac := (int64(centeryfrac) div WORLDUNIT) - int64(worldbottom) * int64(rw_scale) shr FRACBITS;

  if backsector <> nil then
  begin
    worldhigh := worldhigh div WORLDUNIT;
//    worldhigh := worldhigh shr WORLDBITS;
    worldlow := worldlow div WORLDUNIT;
//    worldlow := worldlow shr WORLDBITS;

    if worldhigh < worldtop then
    begin
      pixhigh := (int64(centeryfrac) div WORLDUNIT) - int64(worldhigh) * int64(rw_scale) shr FRACBITS;  // R_WiggleFix
      pixhighstep := -FixedMul(rw_scalestep, worldhigh);
    end;

    if worldlow > worldbottom then
    begin
      pixlow := (int64(centeryfrac) div WORLDUNIT) - int64(worldlow) * int64(rw_scale) shr FRACBITS;  // R_WiggleFix
      pixlowstep := -FixedMul(rw_scalestep, worldlow);
    end;
  end;

  // render it
  if markceiling then
  begin
    if ceilingplane <> nil then
      ceilingplane := R_CheckPlane(ceilingplane, rw_x, rw_stopx - 1)
    else
      markceiling := false;
  end;

  if markfloor then
  begin
    if floorplane <> nil then
    begin
      // cph 2003/04/18  - ceilingplane and floorplane might be the same
      // visplane (e.g. if both skies); R_CheckPlane doesn't know about
      // modifications to the plane that might happen in parallel with the check
      // being made, so we have to override it and split them anyway if that is
      // a possibility, otherwise the floor marking would overwrite the ceiling
      // marking, resulting in HOM.
      if markceiling and (ceilingplane = floorplane) then
        floorplane := R_DupPlane(floorplane, rw_x, rw_stopx - 1)
      else
        floorplane := R_CheckPlane(floorplane, rw_x, rw_stopx - 1);
    end
    else
      markfloor := false;
  end;

  if pds.midvis <> nil then
  begin
    if pds.midsec <> nil then
    begin
      f_RenderSegLoop_3dFloors_Vis(pds); // version 205
    end
    else
    begin
      f_RenderSegLoop_Vis(pds); // version 205
    end;
  end
  else
  begin
    if pds.midsec <> nil then
    begin
      f_RenderSegLoop_3dFloors(pds); // version 205
    end
    else
    begin
      f_RenderSegLoop; // version 205
    end;
  end;

  // save sprite clipping info
  if ((pds.silhouette and SIL_TOP <> 0) or maskedtexture) and
     (pds.sprtopclip = nil) then
  begin
    memcpy(@openings[lastopening], @ceilingclip[start], SizeOf(ceilingclip[0]) * (rw_stopx - start));
    {$IFDEF DEBUG}
    R_CheckClipTable(@ceilingclip, start, rw_stopx - 1);
    {$ENDIF}
    pds.sprtopclip := PSmallIntArray(@openings[lastopening - start]);
    lastopening := lastopening + rw_stopx - start;
  end;

  if ((pds.silhouette and SIL_BOTTOM <> 0) or maskedtexture) and
     (pds.sprbottomclip = nil) then
  begin
    memcpy(@openings[lastopening], @floorclip[start], SizeOf(floorclip[0]) * (rw_stopx - start));
    {$IFDEF DEBUG}
    R_CheckClipTable(@floorclip, start, rw_stopx - 1);
    {$ENDIF}
    pds.sprbottomclip := PSmallIntArray(@openings[lastopening - start]);
    lastopening := lastopening + rw_stopx - start;
  end;

  if maskedtexture and (pds.silhouette and SIL_TOP = 0) then
  begin
    pds.silhouette := pds.silhouette or SIL_TOP;
    pds.tsilheight := MININT;
  end;
  if maskedtexture and (pds.silhouette and SIL_BOTTOM = 0) then
  begin
    pds.silhouette := pds.silhouette or SIL_BOTTOM;
    pds.bsilheight := MAXINT;
  end;
  inc(ds_p);
end;
{$ENDIF}

end.

