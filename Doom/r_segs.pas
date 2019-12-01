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
//  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
//  02111-1307, USA.
//
//  DESCRIPTION:
//   Refresh module, drawing LineSegs from BSP.
//   All the clipping: columns, horizontal spans, sky columns.
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

implementation

uses
  i_system,
  r_bsp,
{$IFNDEF OPENGL}
  doomdef,
  doomdata,
  doomtype,
  r_column,
  r_data,
  r_hires,
  r_main,
  r_things,
  r_plane,
  r_draw,
  r_sky,
  r_ccache,
  r_wall8,
  r_wall32,
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



{$IFNDEF OPENGL}
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
    if ds.curline.linedef.renderflags and LRF_TRANSPARENT <> 0 then
      colfunc := averagecolfunc
    else
      colfunc := maskedcolfunc2;
  end
  else
  begin
    mc2height := 0;
    if ds.curline.linedef.renderflags and LRF_TRANSPARENT <> 0 then
      colfunc := averagecolfunc
    else
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

//
// R_RenderSegLoop
// Draws zero, one, or two textures (and possibly a masked
//  texture) for walls.
// Can draw or mark the starting pixel of floor and ceiling
//  textures.
// CALLED: CORE LOOPING ROUTINE.
//
const
  HEIGHTBITS = 12;
  HEIGHTUNIT = 1 shl HEIGHTBITS;
  WORLDBIT = 16 - HEIGHTBITS;
  WORLDUNIT = 1 shl WORLDBIT;

procedure R_RenderSegLoop8;
{$UNDEF RENDERSEGOPTIMIZED}
{$I R_RenderSegLoop8.inc}

procedure R_RenderSegLoop8Optimized;
{$DEFINE RENDERSEGOPTIMIZED}
{$I R_RenderSegLoop8.inc}

procedure R_RenderSegLoop32;
{$UNDEF RENDERSEGOPTIMIZED}
{$I R_RenderSegLoop32.inc}

procedure R_RenderSegLoop32Optimized;
{$DEFINE RENDERSEGOPTIMIZED}
{$I R_RenderSegLoop32.inc}


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
  pds := R_NewDrawSeg;
  if pds = nil then
    exit;
  pds.curline := curline;

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

 {
  if frontsector.floorheight > backsector.floorheight then
  begin
      pds.silhouette := SIL_BOTTOM;
      pds.bsilheight := frontsector.floorheight;
  end
  else if backsector.floorheight > viewz then
  begin
        pds.silhouette := SIL_BOTTOM;
        pds.bsilheight := MININT;
  end;

  if frontsector.ceilingheight < backsector.ceilingheight then
  begin
      pds.silhouette := pds.silhouette or SIL_TOP;
      pds.tsilheight := frontsector.ceilingheight;
  end
  else
    if backsector.ceilingheight < viewz then
    begin
        pds.silhouette := pds.silhouette or SIL_TOP;
        pds.tsilheight := MININT;
      end;
  }

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
                   (backsector.lightlevel <> frontsector.lightlevel) or
                   // killough 3/7/98: Add checks for (x,y) offsets
                   (backsector.floor_xoffs <> frontsector.floor_xoffs) or
                   (backsector.floor_yoffs <> frontsector.floor_yoffs) or
                   // killough 4/15/98: prevent 2s normals
                   // from bleeding through deep water
                   (frontsector.heightsec <> -1) or
                   // killough 4/17/98: draw floors if different light levels
                   (backsector.floorlightsec <> frontsector.floorlightsec);

      markceiling := (worldhigh <> worldtop) or
                     (backsector.ceilingpic <> frontsector.ceilingpic) or
                     (backsector.lightlevel <> frontsector.lightlevel) or
                     // killough 3/7/98: Add checks for (x,y) offsets
                     (backsector.ceiling_xoffs <> frontsector.ceiling_xoffs) or
                     (backsector.ceiling_yoffs <> frontsector.ceiling_yoffs) or
                     // killough 4/15/98: prevent 2s normals
                     // from bleeding through fake ceilings
                     ((frontsector.heightsec <> -1) and (frontsector.ceilingpic <> skyflatnum)) or
                     // killough 4/17/98: draw ceilings if different light levels
                     (backsector.ceilinglightsec <> frontsector.ceilinglightsec);
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


  // killough 3/7/98: add deep water check
  if frontsector.heightsec = -1 then
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
  begin
    if ceilingplane <> nil then
      ceilingplane := R_CheckPlane(ceilingplane, rw_x, rw_stopx - 1)
    else
      markceiling := false;
  end;

  if markfloor then
  begin
    if floorplane <> nil then
      floorplane := R_CheckPlane(floorplane, rw_x, rw_stopx - 1)
    else
      markfloor := false;
  end;

  if videomode = vm32bit then
  begin
    if optimizedcolumnrendering then
      R_RenderSegLoop32Optimized
    else
      R_RenderSegLoop32;
  end
  else
  begin
    if optimizedcolumnrendering then
      R_RenderSegLoop8Optimized
    else
      R_RenderSegLoop8;
  end;

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
