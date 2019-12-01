//------------------------------------------------------------------------------
//
//  DelphiHeretic: A modified and improved Heretic port for Windows
//  based on original Linux Doom as published by "id Software", on
//  Heretic source as published by "Raven" software and DelphiDoom
//  as published by Jim Valavanis.
//  Copyright (C) 2004-2012 by Jim Valavanis
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
// DESCRIPTION:
//  Refresh module, drawing LineSegs from BSP.
//  All the clipping: columns, horizontal spans, sky columns.
//
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : http://delphidoom.sitesled.com/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit r_segs;

interface

uses
  d_delphi,
  m_fixed, tables,
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

{$IFDEF OPENGL}
function R_NewDrawSeg: Pdrawseg_t;
{$ENDIF}

implementation

uses
  doomtype, doomdef, doomstat, doomdata,
  i_system,
  r_main, r_data, r_bsp, r_sky, r_things, r_draw, r_plane, r_hires,
{$IFNDEF OPENGL}
  r_ccache,
  r_column,
{$ENDIF}
{$IFDEF OPENGL}
  gl_render, // JVAL OPENGL
{$ENDIF}
  z_zone;

{$IFNDEF OPENGL}
var
  maskedtexturecol: PSmallIntArray; // JVAL : declared in r_defs

// True if any of the segs textures might be visible.
  segtextured: boolean;

// False if the back side is the same plane.
  markfloor: boolean;
  markceiling: boolean;

  maskedtexture: boolean;
  toptexture: integer;
  bottomtexture: integer;
  midtexture: integer;

//
// regular wall
//
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

  pixhigh: fixed_t;
  pixlow: fixed_t;
  pixhighstep: fixed_t;
  pixlowstep: fixed_t;

  topfrac: fixed_t;
  topstep: fixed_t;

  bottomfrac: fixed_t;
  bottomstep: fixed_t;
{$ENDIF}


// OPTIMIZE: closed two sided lines as single sided

{$IFNDEF OPENGL}
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
  use32: boolean;
  mc2height: integer;
  texturecolumn: integer;
begin
  // Calculate light table.
  // Use different light tables
  //   for horizontal / vertical / diagonal. Diagonal?
  // OPTIMIZE: get rid of LIGHTSEGSHIFT globally
  curline := ds.curline;
  frontsector := curline.frontsector;
  backsector := curline.backsector;
  texnum := texturetranslation[curline.sidedef.midtexture];

  R_GetDCs(texnum, 0); // JVAL Also precache external texture if not loaded
  use32 := (videomode = vm32bit) and (integer(textures[texnum].texture32) > $1);
  if use32 then
  begin
    mc2height := textures[texnum].height;
    colfunc := maskedcolfunc2;
  end
  else
  begin
    mc2height := 0;
    colfunc := maskedcolfunc;
  end;

  lightnum := _SHR(frontsector.lightlevel, LIGHTSEGSHIFT) + extralight;

  if curline.v1.y = curline.v2.y then
    dec(lightnum)
  else if curline.v1.x = curline.v2.x then
    inc(lightnum);

  if lightnum < 0 then
    lightnum := 0
  else if lightnum >= LIGHTLEVELS then
    lightnum := LIGHTLEVELS - 1;

  walllights := @scalelight[lightnum];

  dc_llindex := lightnum;

  maskedtexturecol := ds.maskedtexturecol;

  rw_scalestep := ds.scalestep;
  spryscale := ds.scale1 + (x1 - ds.x1) * rw_scalestep;
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
        if not forcecolormaps then
        begin
          index := _SHR(spryscale, HLL_LIGHTSCALESHIFT + 3) * 320 div SCREENWIDTH;
          if index >= HLL_MAXLIGHTSCALE then
            index := HLL_MAXLIGHTSCALE - 1;
          dc_lightlevel := scalelightlevels[dc_llindex, index];
        end;
        index := _SHR(spryscale, LIGHTSCALESHIFT) * 320 div SCREENWIDTH;

        if index >=  MAXLIGHTSCALE then
          index := MAXLIGHTSCALE - 1;

        dc_colormap := walllights[index];
        if videomode = vm32bit then
          dc_colormap32 := R_GetColormap32(dc_colormap);
      end;

      sprtopscreen := centeryfrac - FixedMul(dc_texturemid, spryscale);
      dc_iscale := LongWord($ffffffff) div LongWord(spryscale);

      texturecolumn := maskedtexturecol[dc_x] shr DC_HIRESBITS;
      if use32 then
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
        R_DrawMaskedColumn(col);
      end;

      maskedtexturecol[dc_x] := MAXSHORT;
    end;
    spryscale := spryscale + rw_scalestep;
  end;
end;
{$ENDIF}

//
// R_RenderSegLoop
// Draws zero, one, or two textures (and possibly a masked
//  texture) for walls.
// Can draw or mark the starting pixel of floor and ceiling
//  textures.
// CALLED: CORE LOOPING ROUTINE.
//
{$IFNDEF OPENGL}
const
  HEIGHTBITS = 12;
  HEIGHTUNIT = 1 shl HEIGHTBITS;
  WORLDBIT = 16 - HEIGHTBITS;
  WORLDUNIT = 1 shl WORLDBIT;

procedure R_RenderSegLoop;
var
  angle: angle_t;
  index: integer;
  yl: integer;
  yh: integer;
  mid: integer;
  texturecolumn: fixed_t;
  texturecolumnhi: smallint;
  top: integer;
  bottom: integer;
  pceilingclip: PSmallInt;
  pfloorclip: PSmallInt;
  rwx, rwstopx: integer;
begin
  texturecolumn := 0; // shut up compiler warning
  texturecolumnhi := 0;
  rwx := rw_x;
  rwstopx := rw_stopx;
  pceilingclip := @ceilingclip[rwx];
  pfloorclip := @floorclip[rwx];
  while rwx < rwstopx do
  begin
    // mark floor / ceiling areas
    yl := (topfrac + (HEIGHTUNIT - 1)) div HEIGHTUNIT;

    // no space above wall?
    if yl <= pceilingclip^ then
      yl := pceilingclip^ + 1;

    if markceiling then
    begin
      top := pceilingclip^ + 1;
      bottom := yl - 1;

      if bottom >= pfloorclip^ then
        bottom := pfloorclip^ - 1;

      if top <= bottom then
      begin
        ceilingplane.top[rwx] := top;
        ceilingplane.bottom[rwx] := bottom;
      end;
    end;

    yh := bottomfrac div HEIGHTUNIT;

    if yh >= pfloorclip^ then
      yh := pfloorclip^ - 1;

    if markfloor then
    begin
      top := yh + 1;
      bottom := pfloorclip^ - 1;
      if top <= pceilingclip^ then
        top := pceilingclip^ + 1;
      if top <= bottom then
      begin
        floorplane.top[rwx] := top;
        floorplane.bottom[rwx] := bottom;
      end;
    end;

    // texturecolumn and lighting are independent of wall tiers
    if segtextured then
    begin
      // calculate texture offset
      {$IFDEF FPC}
      angle := _SHRW(rw_centerangle + xtoviewangle[rwx], ANGLETOFINESHIFT);
      {$ELSE}
      angle := (rw_centerangle + xtoviewangle[rwx]) shr ANGLETOFINESHIFT;
      {$ENDIF}
      texturecolumn := rw_offset - FixedMul(finetangent[angle], rw_distance);
      if detailLevel >= DL_NORMAL then
      begin
        dc_texturemod := (LongWord(texturecolumn) and (FRACUNIT - 1)) shr (FRACBITS - DC_HIRESBITS); // JVAL for hi resolution
        if detailLevel = DL_ULTRARES then
          dc_mod := dc_texturemod
        else if detailLevel = DL_HIRES then
          dc_mod := dc_texturemod and (not $1)
        else
          dc_mod := 0;
      end;

      texturecolumnhi := texturecolumn shr (FRACBITS - DC_HIRESBITS);
      texturecolumn := texturecolumn shr FRACBITS;
      // calculate lighting
      index := _SHR(rw_scale * 320 div SCREENWIDTH, LIGHTSCALESHIFT);

      if index >=  MAXLIGHTSCALE then
        index := MAXLIGHTSCALE - 1;

      dc_colormap := walllights[index];
      if videomode = vm32bit then
      begin
        dc_colormap32 := R_GetColormap32(dc_colormap);
        if (not forcecolormaps) and (fixedcolormap = nil) then
        begin
          index := _SHR(rw_scale * 320, HLL_LIGHTSCALESHIFT + 3) div SCREENWIDTH;
          if index >= HLL_MAXLIGHTSCALE then
            index := HLL_MAXLIGHTSCALE - 1;
          dc_lightlevel := scalelightlevels[dc_llindex, index];
        end
        else if fixedcolormapnum = INVERSECOLORMAP then
          dc_lightlevel := -1
        else
          dc_lightlevel := R_GetColormapLightLevel(dc_colormap);
      end;

      dc_x := rwx;
      dc_iscale := LongWord($ffffffff) div LongWord(rw_scale);
    end;

    // draw the wall tiers
    if midtexture <> 0 then
    begin
      // single sided line
      dc_yl := yl;
      dc_yh := yh;
      dc_texturemid := rw_midtexturemid;
      R_GetDCs(midtexture, texturecolumn);
      wallcolfunc;
      pceilingclip^ := viewheight;
      pfloorclip^ := -1;
    end
    else
    begin
      // two sided line
      if toptexture <> 0 then
      begin
        // top wall
        mid := pixhigh div HEIGHTUNIT;
        pixhigh := pixhigh + pixhighstep;

        if mid >= pfloorclip^ then
          mid := pfloorclip^ - 1;

        if mid >= yl then
        begin
          dc_yl := yl;
          dc_yh := mid;
          dc_texturemid := rw_toptexturemid;
          R_GetDCs(toptexture, texturecolumn);
          wallcolfunc;
          pceilingclip^ := mid;
        end
        else
          pceilingclip^ := yl - 1;
      end
      else
      begin
        // no top wall
        if markceiling then
          pceilingclip^ := yl - 1;
      end;

      if bottomtexture <> 0 then
      begin
        // bottom wall
        mid := (pixlow + HEIGHTUNIT - 1) div HEIGHTUNIT;
        pixlow := pixlow + pixlowstep;

        // no space above wall?
        if mid <= pceilingclip^ then
          mid := pceilingclip^ + 1;

        if mid <= yh then
        begin
          dc_yl := mid;
          dc_yh := yh;
          dc_texturemid := rw_bottomtexturemid;
          R_GetDCs(bottomtexture, texturecolumn);
          wallcolfunc;
          pfloorclip^ := mid;
        end
        else
          pfloorclip^ := yh + 1;
      end
      else
      begin
        // no bottom wall
        if markfloor then
          pfloorclip^ := yh + 1;
      end;

      if maskedtexture then
      begin
        // save texturecol
        // for backdrawing of masked mid texture
        maskedtexturecol[rwx] := texturecolumnhi;
      end;
    end;

    rw_scale := rw_scale + rw_scalestep;
    topfrac := topfrac + topstep;
    bottomfrac := bottomfrac + bottomstep;
    inc(rwx);
    inc(pceilingclip);
    inc(pfloorclip);
  end;

end;

procedure R_RenderSegLoop8;
var
  angle: angle_t;
  index: integer;
  yl: integer;
  yh: integer;
  mid: integer;
  texturecolumn: fixed_t;
  texturecolumnhi: smallint;
  top: integer;
  bottom: integer;
  pceilingclip: PSmallInt;
  pfloorclip: PSmallInt;
  rwx, rwstopx: integer;
begin
  texturecolumn := 0; // shut up compiler warning
  texturecolumnhi := 0;
  rwx := rw_x;
  rwstopx := rw_stopx;
  pceilingclip := @ceilingclip[rwx];
  pfloorclip := @floorclip[rwx];
  while rwx < rwstopx do
  begin
    // mark floor / ceiling areas
    yl := (topfrac + (HEIGHTUNIT - 1)) div HEIGHTUNIT;

    // no space above wall?
    if yl <= pceilingclip^ then
      yl := pceilingclip^ + 1;

    if markceiling then
    begin
      top := pceilingclip^ + 1;
      bottom := yl - 1;

      if bottom >= pfloorclip^ then
        bottom := pfloorclip^ - 1;

      if top <= bottom then
      begin
        ceilingplane.top[rwx] := top;
        ceilingplane.bottom[rwx] := bottom;
      end;
    end;

    yh := bottomfrac div HEIGHTUNIT;

    if yh >= pfloorclip^ then
      yh := pfloorclip^ - 1;

    if markfloor then
    begin
      top := yh + 1;
      bottom := pfloorclip^ - 1;
      if top <= pceilingclip^ then
        top := pceilingclip^ + 1;
      if top <= bottom then
      begin
        floorplane.top[rwx] := top;
        floorplane.bottom[rwx] := bottom;
      end;
    end;

    // texturecolumn and lighting are independent of wall tiers
    if segtextured then
    begin
      // calculate texture offset
      {$IFDEF FPC}
      angle := _SHRW(rw_centerangle + xtoviewangle[rwx], ANGLETOFINESHIFT);
      {$ELSE}
      angle := (rw_centerangle + xtoviewangle[rwx]) shr ANGLETOFINESHIFT;
      {$ENDIF}
      texturecolumn := rw_offset - FixedMul(finetangent[angle], rw_distance);

      texturecolumnhi := texturecolumn shr (FRACBITS - DC_HIRESBITS);
      texturecolumn := texturecolumn shr FRACBITS;
      // calculate lighting
      index := _SHR(rw_scale * 320 div SCREENWIDTH, LIGHTSCALESHIFT);

      if index >=  MAXLIGHTSCALE then
        index := MAXLIGHTSCALE - 1;

      dc_colormap := walllights[index];
      dc_x := rwx;
      dc_iscale := LongWord($ffffffff) div LongWord(rw_scale);
    end;

    // draw the wall tiers
    if midtexture <> 0 then
    begin
      // single sided line
      dc_yl := yl;
      dc_yh := yh;
      dc_texturemid := rw_midtexturemid;
      dc_source := R_GetColumn(midtexture, texturecolumn);
      wallcolfunc;
      pceilingclip^ := viewheight;
      pfloorclip^ := -1;
    end
    else
    begin
      // two sided line
      if toptexture <> 0 then
      begin
        // top wall
        mid := pixhigh div HEIGHTUNIT;
        pixhigh := pixhigh + pixhighstep;

        if mid >= pfloorclip^ then
          mid := pfloorclip^ - 1;

        if mid >= yl then
        begin
          dc_yl := yl;
          dc_yh := mid;
          dc_texturemid := rw_toptexturemid;
          dc_source := R_GetColumn(toptexture, texturecolumn);
          wallcolfunc;
          pceilingclip^ := mid;
        end
        else
          pceilingclip^ := yl - 1;
      end
      else
      begin
        // no top wall
        if markceiling then
          pceilingclip^ := yl - 1;
      end;

      if bottomtexture <> 0 then
      begin
        // bottom wall
        mid := (pixlow + HEIGHTUNIT - 1) div HEIGHTUNIT;
        pixlow := pixlow + pixlowstep;

        // no space above wall?
        if mid <= pceilingclip^ then
          mid := pceilingclip^ + 1;

        if mid <= yh then
        begin
          dc_yl := mid;
          dc_yh := yh;
          dc_texturemid := rw_bottomtexturemid;
          dc_source := R_GetColumn(bottomtexture, texturecolumn);
          wallcolfunc;
          pfloorclip^ := mid;
        end
        else
          pfloorclip^ := yh + 1;
      end
      else
      begin
        // no bottom wall
        if markfloor then
          pfloorclip^ := yh + 1;
      end;

      if maskedtexture then
      begin
        // save texturecol
        // for backdrawing of masked mid texture
        maskedtexturecol[rwx] := texturecolumnhi;
      end;
    end;

    rw_scale := rw_scale + rw_scalestep;
    topfrac := topfrac + topstep;
    bottomfrac := bottomfrac + bottomstep;
    inc(rwx);
    inc(pceilingclip);
    inc(pfloorclip);
  end;

end;

procedure R_RenderSegLoop32;
var
  angle: angle_t;
  index: integer;
  yl: integer;
  yh: integer;
  mid: integer;
  texturecolumn: fixed_t;
  texturecolumnhi: smallint;
  top: integer;
  bottom: integer;
  pceilingclip: PSmallInt;
  pfloorclip: PSmallInt;
  rwx, rwstopx: integer;
begin
  texturecolumn := 0; // shut up compiler warning
  texturecolumnhi := 0;
  rwx := rw_x;
  rwstopx := rw_stopx;
  pceilingclip := @ceilingclip[rwx];
  pfloorclip := @floorclip[rwx];
  while rwx < rwstopx do
  begin
    // mark floor / ceiling areas
    yl := (topfrac + (HEIGHTUNIT - 1)) div HEIGHTUNIT;

    // no space above wall?
    if yl <= pceilingclip^ then
      yl := pceilingclip^ + 1;

    if markceiling then
    begin
      top := pceilingclip^ + 1;
      bottom := yl - 1;

      if bottom >= pfloorclip^ then
        bottom := pfloorclip^ - 1;

      if top <= bottom then
      begin
        ceilingplane.top[rwx] := top;
        ceilingplane.bottom[rwx] := bottom;
      end;
    end;

    yh := bottomfrac div HEIGHTUNIT;

    if yh >= pfloorclip^ then
      yh := pfloorclip^ - 1;

    if markfloor then
    begin
      top := yh + 1;
      bottom := pfloorclip^ - 1;
      if top <= pceilingclip^ then
        top := pceilingclip^ + 1;
      if top <= bottom then
      begin
        floorplane.top[rwx] := top;
        floorplane.bottom[rwx] := bottom;
      end;
    end;

    // texturecolumn and lighting are independent of wall tiers
    if segtextured then
    begin
      // calculate texture offset
      {$IFDEF FPC}
      angle := _SHRW(rw_centerangle + xtoviewangle[rwx], ANGLETOFINESHIFT);
      {$ELSE}
      angle := (rw_centerangle + xtoviewangle[rwx]) shr ANGLETOFINESHIFT;
      {$ENDIF}
      texturecolumn := rw_offset - FixedMul(finetangent[angle], rw_distance);

      dc_texturemod := (LongWord(texturecolumn) and (FRACUNIT - 1)) shr (FRACBITS - DC_HIRESBITS); // JVAL for hi resolution
      if detailLevel = DL_ULTRARES then
        dc_mod := dc_texturemod
      else if detailLevel = DL_HIRES then
        dc_mod := dc_texturemod and (not $1)
      else
        dc_mod := 0;

      texturecolumnhi := texturecolumn shr (FRACBITS - DC_HIRESBITS);
      texturecolumn := texturecolumn shr FRACBITS;
      // calculate lighting
      index := _SHR(rw_scale * 320 div SCREENWIDTH, LIGHTSCALESHIFT);

      if index >=  MAXLIGHTSCALE then
        index := MAXLIGHTSCALE - 1;

      dc_colormap := walllights[index];

      dc_colormap32 := R_GetColormap32(dc_colormap);
      if (not forcecolormaps) and (fixedcolormap = nil) then
      begin
        index := _SHR(rw_scale * 320, HLL_LIGHTSCALESHIFT + 3) div SCREENWIDTH;
        if index >= HLL_MAXLIGHTSCALE then
          index := HLL_MAXLIGHTSCALE - 1;
        dc_lightlevel := scalelightlevels[dc_llindex, index];
      end
      else if fixedcolormapnum = INVERSECOLORMAP then
        dc_lightlevel := -1
      else
        dc_lightlevel := R_GetColormapLightLevel(dc_colormap);

      dc_x := rwx;
      dc_iscale := LongWord($ffffffff) div LongWord(rw_scale);
    end;

    // draw the wall tiers
    if midtexture <> 0 then
    begin
      // single sided line
      dc_yl := yl;
      dc_yh := yh;
      dc_texturemid := rw_midtexturemid;

      R_ReadDC32Cache(midtexture, texturecolumn);
      wallcolfunc;
      pceilingclip^ := viewheight;
      pfloorclip^ := -1;
    end
    else
    begin
      // two sided line
      if toptexture <> 0 then
      begin
        // top wall
        mid := pixhigh div HEIGHTUNIT;
        pixhigh := pixhigh + pixhighstep;

        if mid >= pfloorclip^ then
          mid := pfloorclip^ - 1;

        if mid >= yl then
        begin
          dc_yl := yl;
          dc_yh := mid;
          dc_texturemid := rw_toptexturemid;
          R_ReadDC32Cache(toptexture, texturecolumn);
          wallcolfunc;
          pceilingclip^ := mid;
        end
        else
          pceilingclip^ := yl - 1;
      end
      else
      begin
        // no top wall
        if markceiling then
          pceilingclip^ := yl - 1;
      end;

      if bottomtexture <> 0 then
      begin
        // bottom wall
        mid := (pixlow + HEIGHTUNIT - 1) div HEIGHTUNIT;
        pixlow := pixlow + pixlowstep;

        // no space above wall?
        if mid <= pceilingclip^ then
          mid := pceilingclip^ + 1;

        if mid <= yh then
        begin
          dc_yl := mid;
          dc_yh := yh;
          dc_texturemid := rw_bottomtexturemid;
          R_ReadDC32Cache(bottomtexture, texturecolumn);
          wallcolfunc;
          pfloorclip^ := mid;
        end
        else
          pfloorclip^ := yh + 1;
      end
      else
      begin
        // no bottom wall
        if markfloor then
          pfloorclip^ := yh + 1;
      end;

      if maskedtexture then
      begin
        // save texturecol
        // for backdrawing of masked mid texture
        maskedtexturecol[rwx] := texturecolumnhi;
      end;
    end;

    rw_scale := rw_scale + rw_scalestep;
    topfrac := topfrac + topstep;
    bottomfrac := bottomfrac + bottomstep;
    inc(rwx);
    inc(pceilingclip);
    inc(pfloorclip);
  end;

end;


{$ENDIF}

function R_NewDrawSeg: Pdrawseg_t;
begin
  // don't overflow and crash
  if ds_p = MAXDRAWSEGS then
  begin
    I_Warning('R_NewDrawSeg(): MAXDRAWSEGS limit reached (%d)'#13#10, [MAXDRAWSEGS]);
    result := nil;
    exit;
  end;

  // JVAL
  // Now drawsegs is an array of pointer to drawseg_t
  // Dynamically allocation using zone
  if ds_p > max_ds_p then
  begin
    drawsegs[ds_p] := Z_Malloc(SizeOf(drawseg_t), PU_LEVEL, nil);
    max_ds_p := ds_p;
  end;
  result := drawsegs[ds_p];
end;

//
// R_StoreWallRange
// A wall segment will be drawn
//  between start and stop pixels (inclusive).
//
{$IFNDEF OPENGL}
procedure R_StoreWallRange(const start: integer; const stop: integer);
var
  hyp: fixed_t;
  sineval: fixed_t;
  distangle,
  offsetangle: angle_t;
  vtop: fixed_t;
  lightnum: integer;
  pds: Pdrawseg_t;
begin
  // don't overflow and crash
  if ds_p = MAXDRAWSEGS then
    exit;

  // JVAL
  // Now drawsegs is an array of pointer to drawseg_t
  // Dynamically allocation using zone
  if ds_p > max_ds_p then
  begin
    drawsegs[ds_p] := Z_Malloc(SizeOf(drawseg_t), PU_LEVEL, nil);
    max_ds_p := ds_p;
  end;
  pds := drawsegs[ds_p];

  sidedef := curline.sidedef;
  linedef := curline.linedef;

  // mark the segment as visible for auto map
  linedef.flags := linedef.flags or ML_MAPPED;

  // calculate rw_distance for scale calculation
  rw_normalangle := curline.angle + ANG90;
  offsetangle := abs(rw_normalangle - rw_angle1);
  {$IFDEF FPC}
  PInteger(@offsetangle)^ := abs(PInteger(@offsetangle)^);
  {$ENDIF}

  if offsetangle > ANG90 then
    offsetangle := ANG90;

  distangle := ANG90 - offsetangle;

  hyp := R_PointToDist(curline.v1.x, curline.v1.y);
  {$IFDEF FPC}
  sineval := finesine[_SHRW(distangle, ANGLETOFINESHIFT)];
  {$ELSE}
  sineval := finesine[distangle shr ANGLETOFINESHIFT];
  {$ENDIF}
  rw_distance := FixedMul(hyp, sineval);

  rw_x := start;
  pds.x1 := rw_x;
  pds.x2 := stop;
  pds.curline := curline;
  rw_stopx := stop + 1;

  // calculate scale at both ends and step
  rw_scale := R_ScaleFromGlobalAngle(viewangle + xtoviewangle[start]);
  pds.scale1 := rw_scale;

  if stop > start then
  begin
    pds.scale2 := R_ScaleFromGlobalAngle(viewangle + xtoviewangle[stop]);
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

  midtexture := 0;
  toptexture := 0;
  bottomtexture := 0;
  maskedtexture := false;
  pds.maskedtexturecol := nil;

  if backsector = nil then
  begin
    // single sided line
    midtexture := texturetranslation[sidedef.midtexture];
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
    rw_midtexturemid := rw_midtexturemid + sidedef.rowoffset;

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

    if backsector.ceilingheight <= frontsector.floorheight then
    begin
      pds.sprbottomclip := @negonearray;
      pds.bsilheight := MAXINT;
      pds.silhouette := pds.silhouette or SIL_BOTTOM;
    end;

    if backsector.floorheight >= frontsector.ceilingheight then
    begin
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

    if (backsector.ceilingheight <= frontsector.floorheight) or
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
                   (backsector.lightlevel <> frontsector.lightlevel);

      markceiling := (worldhigh <> worldtop) or
                     (backsector.ceilingpic <> frontsector.ceilingpic) or
                     (backsector.lightlevel <> frontsector.lightlevel);
    end;

    if worldhigh < worldtop then
    begin
      // top texture
      toptexture := texturetranslation[sidedef.toptexture];
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

      if linedef.flags and ML_DONTPEGBOTTOM <> 0 then
      begin
        // bottom of texture at bottom
        // top of texture at top
        rw_bottomtexturemid := worldtop;
      end
      else // top of texture at top
        rw_bottomtexturemid := worldlow;
    end;
    rw_toptexturemid := rw_toptexturemid + sidedef.rowoffset;
    rw_bottomtexturemid := rw_bottomtexturemid + sidedef.rowoffset;

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
    offsetangle := rw_normalangle - rw_angle1;

    if offsetangle > ANG180 then
      offsetangle := LongWord($ffffffff) - offsetangle + 1;

    if offsetangle > ANG90 then
      offsetangle := ANG90;

   {$IFDEF FPC}
    sineval := finesine[_SHRW(offsetangle, ANGLETOFINESHIFT)];
   {$ELSE}
    sineval := finesine[offsetangle shr ANGLETOFINESHIFT];
    {$ENDIF}

    rw_offset := FixedMul(hyp, sineval);

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
      lightnum := _SHR(frontsector.lightlevel, LIGHTSEGSHIFT) + extralight;

      if curline.v1.y = curline.v2.y then
        dec(lightnum)
      else if curline.v1.x = curline.v2.x then
        inc(lightnum);

      if lightnum < 0 then
        lightnum := 0
      else if lightnum >= LIGHTLEVELS then
        lightnum := LIGHTLEVELS - 1;
      walllights := @scalelight[lightnum];

      dc_llindex := lightnum;
    end;
  end;

  // if a floor / ceiling plane is on the wrong side
  //  of the view plane, it is definitely invisible
  //  and doesn't need to be marked.


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


  // calculate incremental stepping values for texture edges
  worldtop := worldtop div WORLDUNIT;
  worldbottom := worldbottom div WORLDUNIT;

  topstep := - FixedMul(rw_scalestep, worldtop);

  topfrac := (centeryfrac div WORLDUNIT) - FixedMul(worldtop, rw_scale);

  bottomstep := - FixedMul(rw_scalestep, worldbottom);

  bottomfrac := (centeryfrac div WORLDUNIT) - FixedMul(worldbottom, rw_scale);

  if backsector <> nil then
  begin
    worldhigh := worldhigh div WORLDUNIT;
    worldlow := worldlow div WORLDUNIT;

    if worldhigh < worldtop then
    begin
      pixhigh := (centeryfrac div WORLDUNIT) - FixedMul(worldhigh, rw_scale);
      pixhighstep := -FixedMul(rw_scalestep, worldhigh);
    end;

    if worldlow > worldbottom then
    begin
      pixlow := (centeryfrac div WORLDUNIT) - FixedMul(worldlow, rw_scale);
      pixlowstep := -FixedMul(rw_scalestep, worldlow);
    end;
  end;

  // render it
  if markceiling then
    ceilingplane := R_CheckPlane(ceilingplane, rw_x, rw_stopx - 1);

  if markfloor then
    floorplane := R_CheckPlane(floorplane, rw_x, rw_stopx - 1);

  if videomode = vm32bit then
    R_RenderSegLoop32
  else
    R_RenderSegLoop8;

  // save sprite clipping info
  if ((pds.silhouette and SIL_TOP <> 0) or maskedtexture) and
     (pds.sprtopclip = nil) then
  begin
    memcpy(@openings[lastopening], @ceilingclip[start], SizeOf(ceilingclip[0]) * (rw_stopx - start));
    pds.sprtopclip := PSmallIntArray(@openings[lastopening - start]);
    lastopening := lastopening + rw_stopx - start;
  end;

  if ((pds.silhouette and SIL_BOTTOM <> 0) or maskedtexture) and
     (pds.sprbottomclip = nil) then
  begin
    memcpy(@openings[lastopening], @floorclip[start], SizeOf(floorclip[0]) * (rw_stopx - start));
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
