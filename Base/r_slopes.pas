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
//  Slopes software rendering.
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit r_slopes;

interface

uses
  r_defs,
  r_visplanes;

procedure R_StoreSlopeRange(const start: integer; const stop: integer);

procedure R_VisSlopesFromSubsector(const ssector: Psubsector_t);

procedure R_DoDrawSlope(const pl: Pvisplane_t);

const
  MAXVISSLOPES = $10000;

var
  visslopes: array[0..MAXVISSLOPES - 1] of visslope_t;
  maxvisslope: integer = -1;
  lastvisslope: integer = 0;

var
  floorslope: Pvisslope_t;
  ceilingslope: Pvisslope_t;

procedure R_DrawSlopeMedium;

procedure R_DrawSlopeMedium_Ripple;

procedure R_ClearVisSlopes;

var
  preciseslopedrawing: Boolean = false;

implementation

uses
  d_delphi,
  doomdata,
  doomdef,
  doomtype,
  m_fixed,
  p_setup,
  p_slopes,
  r_3dfloors,
  r_bsp,
  r_clipper,
  r_cliputils,
  r_column,
  r_data,
  r_depthbuffer,
  r_draw,
  r_hires,
  r_fake3d,
  r_main,
  r_plane,
  r_ripple,
  r_scale,
  r_segs,
  r_segs2,
  r_sky,
  r_subsectors,
  r_span,
  r_span32,
  r_things,
  r_zbuffer,
  r_flatinfo,
  r_utils,
  tables,
  i_system,
  z_zone;

//
// Draws the actual span (Medium resolution).
//
procedure R_DrawSlopeMedium;
var
  xfrac: fixed_t;
  yfrac: fixed_t;
  xstep: fixed_t;
  ystep: fixed_t;
  dest: PByte;
  count: integer;
  i: integer;
  spot: integer;
  fb: fourbytes_t;
begin
  dest := @((ylookup[ds_y]^)[columnofs[ds_x1]]);

  count := ds_x2 - ds_x1;

  {$UNDEF RIPPLE}
  {$I R_DrawSlopeMedium.inc}
end;

procedure R_DrawSlopeMedium_Ripple;
var
  xfrac: fixed_t;
  yfrac: fixed_t;
  xstep: fixed_t;
  ystep: fixed_t;
  dest: PByte;
  count: integer;
  i: integer;
  spot: integer;
  rpl: PIntegerArray;
  fb: fourbytes_t;
begin
  dest := @((ylookup[ds_y]^)[columnofs[ds_x1]]);

  count := ds_x2 - ds_x1;

  rpl := ds_ripple;

  {$DEFINE RIPPLE}
  {$I R_DrawSlopeMedium.inc}
end;

function R_NewVisSlope: Pvisslope_t;
begin
  // JVAL: Do not overflow and crash - Unneeded (?) we'll have a visplane overflow before
  if lastvisslope = MAXVISSLOPES then
  begin
    result := nil;
    exit;
  end;

  if lastvisslope > maxvisslope then
  begin
    visslopes[lastvisslope].screenleft :=
      Z_Malloc((SCREENHEIGHT) * SizeOf(smallint), PU_LEVEL, nil);
    visslopes[lastvisslope].screenright :=
      Z_Malloc((SCREENHEIGHT) * SizeOf(smallint), PU_LEVEL, nil);

    visslopes[lastvisslope].ds_zleft :=
      Z_Malloc((SCREENHEIGHT) * SizeOf(integer), PU_LEVEL, nil);
    visslopes[lastvisslope].ds_zright :=
      Z_Malloc((SCREENHEIGHT) * SizeOf(integer), PU_LEVEL, nil);

    {$IFDEF DEBUG}
    visslopes[lastvisslope].id := lastvisslope;
    {$ENDIF}

    maxvisslope := lastvisslope;
  end;

  memsetsi(visslopes[lastvisslope].screenleft, 32767, SCREENHEIGHT);
  memsetsi(visslopes[lastvisslope].screenright, -32768, SCREENHEIGHT);
  visslopes[lastvisslope].miny := SCREENHEIGHT;
  visslopes[lastvisslope].maxy := -1;

  result := @visslopes[lastvisslope];
  inc(lastvisslope);
end;

procedure R_ClearVisSlopes;
var
  i: integer;
begin
  for i := 0 to maxvisslope do
  begin
    Z_Free(visslopes[i].screenleft);
    Z_Free(visslopes[i].screenright);
    Z_Free(visslopes[i].ds_zleft);
    Z_Free(visslopes[i].ds_zright);
  end;
  maxvisslope := -1;
end;

function R_FindExistingVisSlope(const sectorID: Integer; const virtualfloor: Boolean): Pvisslope_t;
var
  sec: Psector_t;
begin
  sec := @sectors[sectorID];
  if virtualfloor then
  begin
    if (sec.floorvisslope >= 0) and (sec.floorvisslope < lastvisslope) then
      if (visslopes[sec.floorvisslope].sectorID = sectorID) and visslopes[sec.floorvisslope].virtualfloor then
      begin
        Result := @visslopes[sec.floorvisslope];
        Exit;
      end;
  end
  else
  begin
    if (sec.ceilingvisslope >= 0) and (sec.ceilingvisslope < lastvisslope) then
      if (visslopes[sec.ceilingvisslope].sectorID = sectorID) and not visslopes[sec.ceilingvisslope].virtualfloor then
      begin
        Result := @visslopes[sec.ceilingvisslope];
        Exit;
      end;
  end;

  Result := nil;
end;

function R_FindVisSlope(const sectorID: Integer; const virtualfloor: Boolean): Pvisslope_t;
begin
  Result := R_FindExistingVisSlope(sectorID, virtualfloor);

  if Result = nil then
  begin
    Result := R_NewVisSlope;
    Result.sectorID := sectorID;
    Result.virtualfloor := virtualfloor;

    // JVAL: 20201225 - Save vislope information in sector structure
    if virtualfloor then
      sectors[sectorID].floorvisslope := lastvisslope - 1
    else
      sectors[sectorID].ceilingvisslope := lastvisslope - 1;
  end;
end;

var
  visslope: Pvisslope_t;

//
//  R_MapSlopePerPixelLight
//  Slow, but accurate light
//  Light calculated in every pixel
//
procedure R_MapSlopePerPixelLight(const y: integer; const x1, x2: integer);
var
  angle: angle_t;
  distance: fixed_t;
  yslopey: fixed_t;
  len: fixed_t;
  index: LongWord;
  ncolornum: integer;
  x: integer;
  Theta, rTheta: integer;
  yidx: integer;
  screenDX: integer;
  zleft, zright: Double;
  xleft, xright: fixed_t;
begin
  if y >= viewheight then
    exit;

  if x2 - x1 < 0 then
    exit;

  if usefake3d and zaxisshift then
    if fake3dspanpresent <> nil then
      if not fake3dspanpresent[y] then
        Exit;

  if y < visslope.miny then
    yidx := visslope.miny
  else if y > visslope.maxy then
    yidx := visslope.maxy
  else
    yidx := y;

  zleft := visslope.ds_zleft[yidx];
  zright := visslope.ds_zright[yidx];

  xleft := visslope.screenleft[yidx];
  xright := visslope.screenright[yidx];

  screenDX := xright - xleft + 1;

  zleft := zleft * screenDX;
  zright := zright * screenDX;

  ds_y := y;

  if fixedcolormap <> nil then
  begin
    ds_colormap := fixedcolormap;
    if videomode = vm32bit then
    begin
      if fixedcolormapnum = INVERSECOLORMAP then
        ds_lightlevel := -1  // Negative value -> Use colormaps
      else
        ds_lightlevel := R_GetColormapLightLevel(ds_colormap);
    end;
  end;

  ds_xstep := 0;
  ds_ystep := 0;

  // JVAL: 20200430 - For slope lightmap
  yslopey := slyslope[y];

  for x := x1 to x2 do
  begin
    if x = xleft then
      distance := Abs(Round(zleft / screenDX))
    else if x = xright then
      distance := Abs(Round(zright / screenDX))
    else
    begin
      Theta := x - xleft;
      rTheta := screenDX - Theta;

      distance := Abs(Round(1.0 / (rTheta / zleft + Theta / zright)));
    end;

    len := FixedMul(distance, distscale[x]);

    angle := (viewangle + xtoviewangle[x]) shr FRACBITS;

    ds_xfrac := viewx + FixedMul(fixedcosine[angle], len)
    {$IFDEF DOOM_OR_STRIFE} + xoffs{$ENDIF} {$IFDEF HEXEN} + ds_xoffset{$ENDIF};
    ds_yfrac := -viewy - FixedMul(fixedsine[angle], len)
    {$IFDEF DOOM_OR_STRIFE} + yoffs{$ENDIF} {$IFDEF HEXEN} + ds_yoffset{$ENDIF};

    if fixedcolormap = nil then
    begin
      index := _SHR(distance, LIGHTZSHIFT);

      if index >= MAXLIGHTZ then
        index := MAXLIGHTZ - 1;

      ds_colormap := planezlight[index];
      if videomode = vm32bit then
      begin
        if not forcecolormaps then
        begin
          ncolornum := _SHR(distance, HLL_ZDISTANCESHIFT);
          if ncolornum >= HLL_MAXLIGHTZ then
            ncolornum := HLL_MAXLIGHTZ - 1;
          ds_lightlevel := zlightlevels[ds_llzindex, ncolornum];
        end
        else
        begin
          ds_lightlevel := R_GetColormapLightLevel(ds_colormap);
        end;
      end;
    end;

    ds_x1 := x;
    ds_x2 := x;

    // high or low detail
    slopefunc;

    // JVAL: 20200430 - Cast lightmap to slopes
    if zbufferactive then
    begin
      planeheight := FixedDivEx(distance, yslopey);
      R_DrawSlopeToZBuffer;
    end;

  end;

end;


const
  SLOPESRECALCSTEP = 16;

//
//  R_MapSlope
//  Fast, but not accurate light
//  Light calculated after SLOPESRECALCSTEP pixels and in start and stop pixels
//
procedure R_MapSlope(const y: integer; const x1, x2: integer);
var
  angle: angle_t;
  distance: fixed_t;
  yslopey: fixed_t;
  len: fixed_t;
  index: LongWord;
  ncolornum: integer;
  x: integer;
  Theta, rTheta: integer;
  yidx: integer;
  screenDX: integer;
  zleft, zright: Double;
  xleft, xright: fixed_t;
  xfrac1: fixed_t;
  xfrac2: fixed_t;
  yfrac1: fixed_t;
  yfrac2: fixed_t;
  cnt: integer;
begin
  if y >= viewheight then
    exit;

  if x2 - x1 < 0 then
    exit;

  if usefake3d and zaxisshift then
    if fake3dspanpresent <> nil then
      if not fake3dspanpresent[y] then
        Exit;

  if y < visslope.miny then
    yidx := visslope.miny
  else if y > visslope.maxy then
    yidx := visslope.maxy
  else
    yidx := y;

  zleft := visslope.ds_zleft[yidx];
  zright := visslope.ds_zright[yidx];

  xleft := visslope.screenleft[yidx];
  xright := visslope.screenright[yidx];

  screenDX := xright - xleft + 1;

  zleft := zleft * screenDX;
  zright := zright * screenDX;

  ds_y := y;

  if fixedcolormap <> nil then
  begin
    ds_colormap := fixedcolormap;
    if videomode = vm32bit then
    begin
      if fixedcolormapnum = INVERSECOLORMAP then
        ds_lightlevel := -1  // Negative value -> Use colormaps
      else
        ds_lightlevel := R_GetColormapLightLevel(ds_colormap);
    end;
  end;

  xfrac2 := 0;
  yfrac2 := 0;

  ds_x1 := x1;
  cnt := 0;

  // JVAL: 20200430 - For slope lightmap
  yslopey := slyslope[y];

  for x := x1 to x2 do
  begin
    Inc(cnt);
    if (x = x1) or (x = x2) or (x mod SLOPESRECALCSTEP = 0) then
    begin
      if x = xleft then
        distance := Abs(Round(zleft / ScreenDX))
      else if x = xright then
        distance := Abs(Round(zright / ScreenDX))
      else
      begin
        Theta := x - xleft;
        rTheta := screenDX - Theta;

        distance := Abs(Round(1.0 / (rTheta / zleft + Theta / zright)));
      end;

      len := FixedMul(distance, distscale[x]);

      angle := (viewangle + xtoviewangle[x]) shr FRACBITS;

      xfrac1 := viewx + FixedMul(fixedcosine[angle], len)
      {$IFDEF DOOM_OR_STRIFE} + xoffs{$ENDIF} {$IFDEF HEXEN} + ds_xoffset{$ENDIF};
      yfrac1 := -viewy - FixedMul(fixedsine[angle], len)
      {$IFDEF DOOM_OR_STRIFE} + yoffs{$ENDIF} {$IFDEF HEXEN} + ds_yoffset{$ENDIF};

      if x = x1 then
      begin
        xfrac2 := xfrac1;
        yfrac2 := yfrac1;
        ds_xstep := 0;
        ds_ystep := 0;
      end
      else if cnt = SLOPESRECALCSTEP then
      begin
        ds_xstep := (xfrac1 - xfrac2) div SLOPESRECALCSTEP;
        ds_ystep := (yfrac1 - yfrac2) div SLOPESRECALCSTEP;
      end
      else
      begin
        ds_xstep := (xfrac1 - xfrac2) div cnt;
        ds_ystep := (yfrac1 - yfrac2) div cnt;
      end;

      if fixedcolormap = nil then
      begin
        index := _SHR(distance, LIGHTZSHIFT);

        if index >= MAXLIGHTZ then
          index := MAXLIGHTZ - 1;

        ds_colormap := planezlight[index];
        if videomode = vm32bit then
        begin
          if not forcecolormaps then
          begin
            ncolornum := _SHR(distance, HLL_ZDISTANCESHIFT);
            if ncolornum >= HLL_MAXLIGHTZ then
              ncolornum := HLL_MAXLIGHTZ - 1;
            ds_lightlevel := zlightlevels[ds_llzindex, ncolornum];
          end
          else
          begin
            ds_lightlevel := R_GetColormapLightLevel(ds_colormap);
          end;
        end;
      end;

      ds_x2 := x;
      ds_xfrac := xfrac2;
      ds_yfrac := yfrac2;

      // high or low detail
      slopefunc;

      // JVAL: 20200430 - Cast lightmap to slopes
      if zbufferactive then
      begin
        planeheight := FixedDivEx(distance, yslopey);
        R_DrawSlopeToZBuffer;
      end;

      cnt := 0;

      ds_x1 := x + 1;

      xfrac2 := xfrac1;
      yfrac2 := yfrac1;
    end;
  end;

end;

const
  ANGLESLOPESRECALCSTEP = 8;

procedure R_MapSlopeAngle(const y: integer; const x1, x2: integer);
var
  angle: fixed_t;
  distance: fixed_t;
  yslopey: fixed_t;
  len: fixed_t;
  index: LongWord;
  ncolornum: integer;
  x: integer;
  Theta, rTheta: integer;
  yidx: integer;
  screenDX: integer;
  zleft, zright: float;
  xleft, xright: fixed_t;
  xfrac1: fixed_t;
  xfrac2: fixed_t;
  yfrac1: fixed_t;
  yfrac2: fixed_t;
  cnt: integer;
  pviewsin, pviewcos: float;
  tcos, tsin: float;
  tviewx, tviewy: fixed_t;
begin
  if y >= viewheight then
    exit;

  if x2 - x1 < 0 then
    exit;

  if usefake3d and zaxisshift then
    if fake3dspanpresent <> nil then
      if not fake3dspanpresent[y] then
        Exit;

  if y < visslope.miny then
    yidx := visslope.miny
  else if y > visslope.maxy then
    yidx := visslope.maxy
  else
    yidx := y;

  zleft := visslope.ds_zleft[yidx];
  zright := visslope.ds_zright[yidx];

  xleft := visslope.screenleft[yidx];
  xright := visslope.screenright[yidx];

  screenDX := xright - xleft + 1;

  zleft := zleft * screenDX;
  zright := zright * screenDX;

  ds_y := y;

  if fixedcolormap <> nil then
  begin
    ds_colormap := fixedcolormap;
    if videomode = vm32bit then
    begin
      if fixedcolormapnum = INVERSECOLORMAP then
        ds_lightlevel := -1  // Negative value -> Use colormaps
      else
        ds_lightlevel := R_GetColormapLightLevel(ds_colormap);
    end;
  end;

  xfrac2 := 0;
  yfrac2 := 0;

  ds_x1 := x1;
  cnt := 0;

{  tsin := sin(-ds_angle / ANGLE_MAX * 2 * pi);
  tcos := cos(-ds_angle / ANGLE_MAX * 2 * pi);

  tviewx := Round(viewx * tcos - viewy * tsin);
  tviewy := Round(viewx * tsin + viewy * tcos);}
  tsin := ds_sine;
  tcos := ds_cosine;

  tviewx := Round((viewx - ds_anglex) * tcos - (viewy - ds_angley) * tsin) + ds_anglex;
  tviewy := Round((viewx - ds_anglex) * tsin + (viewy - ds_angley) * tcos) + ds_angley;

  // JVAL: 20200430 - For slope lightmap
  yslopey := slyslope[y];

  for x := x1 to x2 do
  begin
    Inc(cnt);
    if (x = x1) or (x = x2) or (x mod ANGLESLOPESRECALCSTEP = 0) then
    begin
      if x = xleft then
        distance := Abs(Round(zleft / screenDX))
      else if x = xright then
        distance := Abs(Round(zright / screenDX))
      else
      begin
        Theta := x - xleft;
        rTheta := screenDX - Theta;

        distance := Abs(Round(1.0 / (rTheta / zleft + Theta / zright)));
      end;

      len := FixedMul(distance, distscale[x]);
      angle := (viewangle + xtoviewangle[x] - ds_angle) shr FRACBITS;

      xfrac1 := tviewx + FixedMul(fixedcosine[angle], len)
      {$IFDEF DOOM_OR_STRIFE} + xoffs{$ENDIF} {$IFDEF HEXEN} + ds_xoffset{$ENDIF};
      yfrac1 := -tviewy - FixedMul(fixedsine[angle], len)
      {$IFDEF DOOM_OR_STRIFE} + yoffs{$ENDIF} {$IFDEF HEXEN} + ds_yoffset{$ENDIF};

      if x = x1 then
      begin
        xfrac2 := xfrac1;
        yfrac2 := yfrac1;
        ds_xstep := 0;
        ds_ystep := 0;
      end
      else if cnt = SLOPESRECALCSTEP then
      begin
        ds_xstep := (xfrac1 - xfrac2) div SLOPESRECALCSTEP;
        ds_ystep := (yfrac1 - yfrac2) div SLOPESRECALCSTEP;
      end
      else
      begin
        ds_xstep := (xfrac1 - xfrac2) div cnt;
        ds_ystep := (yfrac1 - yfrac2) div cnt;
      end;

      if fixedcolormap = nil then
      begin
        index := _SHR(distance, LIGHTZSHIFT);

        if index >= MAXLIGHTZ then
          index := MAXLIGHTZ - 1;

        ds_colormap := planezlight[index];
        if videomode = vm32bit then
        begin
          if not forcecolormaps then
          begin
            ncolornum := _SHR(distance, HLL_ZDISTANCESHIFT);
            if ncolornum >= HLL_MAXLIGHTZ then
              ncolornum := HLL_MAXLIGHTZ - 1;
            ds_lightlevel := zlightlevels[ds_llzindex, ncolornum];
          end
          else
          begin
            ds_lightlevel := R_GetColormapLightLevel(ds_colormap);
          end;
        end;
      end;

      ds_x2 := x;
      ds_xfrac := xfrac2;
      ds_yfrac := yfrac2;

      // high or low detail
      slopefunc;

      // JVAL: 20200430 - Cast lightmap to slopes
      if zbufferactive then
      begin
        planeheight := FixedDivEx(distance, yslopey);
        R_DrawSlopeToZBuffer;
      end;

      cnt := 0;

      ds_x1 := x + 1;

      xfrac2 := xfrac1;
      yfrac2 := yfrac1;
    end;
  end;

end;

procedure R_DoDrawSlope(const pl: Pvisplane_t);
var
  light: integer;
  x: integer;
  stop: integer;
begin
  visslope := pl.slope;
  if visslope = nil then
  begin
    visslope := R_FindExistingVisSlope(pl.slopeSID, pl.renderflags and SRF_SLOPEFLOOR <> 0);
    if visslope = nil then
    begin
      // JVAL: Render a small fraction, possible 1-2 pixels
      // This could happen because the clipper does not
      // return a solution (due to round errors)
      R_DoDrawPlane(pl);
      Exit;
    end;
  end;

  R_GetDSs(pl.picnum);

  light := _SHR(pl.lightlevel, LIGHTSEGSHIFT) + extralight;

  if light >= LIGHTLEVELS then
    light := LIGHTLEVELS - 1;

  if light < 0 then
    light := 0;

  if pl.renderflags and SRF_FOG <> 0 then // JVAL: Mars fog sectors
  begin
    planezlight := @fog_zlight[light];
    ds_fog := true;
  end
  else
  begin
    planezlight := @zlight[light];
    ds_fog := false;
  end;
  ds_llzindex := light;
  {$IFDEF DOOM_OR_STRIFE}
  xoffs := pl.xoffs;
  yoffs := pl.yoffs;
  {$ENDIF}
  {$IFDEF HEXEN}
  R_CalcPlaneOffsets(pl);
  {$ENDIF}

  if pl.renderflags and SRF_RIPPLE <> 0 then
  begin
    slopefunc := rippleslopefunc;
    ds_ripple := curripple;
    spanfuncMT := rippleslopefuncMT;
  end
  else
  begin
    slopefunc := baseslopefunc;
    ds_ripple := nil;
    spanfuncMT := baseslopefuncMT;
  end;

  stop := pl.maxx + 1;

  pl.top[pl.maxx + 1] := VISEND;
  pl.top[pl.minx - 1] := VISEND;

  ds_angle := pl.angle;
  if ds_angle <> 0 then
  begin
    ds_anglex := pl.anglex;
    ds_angley := pl.angley;
    ds_sine := sin(-ds_angle / ANGLE_MAX * 2 * pi);    // JVAL: 20200225 - Texture angle
    ds_cosine := cos(ds_angle / ANGLE_MAX * 2 * pi);  // JVAL: 20200225 - Texture angle
    ds_viewsine := sin((viewangle - ds_angle) / ANGLE_MAX * 2 * pi);    // JVAL: 20200225 - Texture angle
    ds_viewcosine := cos((viewangle - ds_angle) / ANGLE_MAX * 2 * pi);  // JVAL: 20200225 - Texture angle
    // Slope with angle
    for x := pl.minx to stop do
    begin
      R_MakeSpans(x, pl.top[x - 1], pl.bottom[x - 1], pl.top[x], pl.bottom[x], @R_MapSlopeAngle);
    end;
  end
  else if preciseslopedrawing then
  begin
    // Slow but accurate lighting
    for x := pl.minx to stop do
    begin
      R_MakeSpans(x, pl.top[x - 1], pl.bottom[x - 1], pl.top[x], pl.bottom[x], @R_MapSlopePerPixelLight);
    end;
  end
  else
  begin
    // Fast but the lighting is not that accurate
    for x := pl.minx to stop do
    begin
      R_MakeSpans(x, pl.top[x - 1], pl.bottom[x - 1], pl.top[x], pl.bottom[x], @R_MapSlope);
    end;
  end;

  if ds_source <> nil then
    Z_ChangeTag(ds_source, PU_CACHE);
end;

//
// R_StoreSlopeRange
// A wall segment will be drawn
//  between start and stop pixels (inclusive).
//
procedure R_StoreSlopeRange(const start: integer; const stop: integer);
var
  lightnum: integer;
  lightnum2: integer; // JVAL: 3d Floors
  pds: Pdrawseg_t;
  overflow: boolean;
  sc1, sc2, sf1, sf2: integer;
  sc11, sc22, sf11, sf22: integer;
  s11, s22: integer;
  side: integer;
  texturecolumn: integer;
  texturecolumn2: Integer;
  rw_scale_dbl2: Double;
  sec2: Psector_t;
begin
  pds := R_NewDrawSeg;
  if pds = nil then
    exit;
  pds.curline := curline;
  pds.midvis := curmidvis;  // JVAL: 3d Floors
  pds.maskedtexturecol := nil;
  pds.thicksidecol := nil;  // JVAL: 3d Floors
  pds.midsec := nil;        // JVAL: 3d Floors

  markfloor := true;
  markceiling := true;

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

  rw_scale_dbl := R_ScaleFromGlobalAngle_DBL(viewangle + xtoviewangle[start]);
  rw_scale := Round(rw_scale_dbl);

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

  R_ScaleFromGlobalAngle(viewangle + xtoviewangle[start], overflow);
  if not overflow then
    R_ScaleFromGlobalAngle(viewangle + xtoviewangle[stop], overflow);
  pds.use_double := overflow;

  R_WiggleFix(frontsector);

  rw_offset := R_CalcSegOffset(curline);

  if LongWord(rw_normalangle - rw_angle1) < ANG180 then
    rw_offset := -rw_offset;

  rw_centerangle := ANG90 + viewangle - rw_normalangle;

  // calculate light table
  //  use different light tables
  //  for horizontal / vertical / diagonal
  // OPTIMIZE: get rid of LIGHTSEGSHIFT globally
  if fixedcolormap = nil then
  begin
    lightnum := _SHR(frontsector.lightlevel, LIGHTSEGSHIFT) + extralight;

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
    walllights := @scalelight[lightnum];

    dc_llindex := lightnum;

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

  texturecolumn := rw_offset -
    FixedMul(finetangent[(rw_centerangle + xtoviewangle[rw_x]) div ANGLETOFINEUNIT],
      rw_distance);
  texturecolumn2 := rw_offset -
    FixedMul(finetangent[(rw_centerangle + xtoviewangle[stop]) div ANGLETOFINEUNIT],
      rw_distance);

  sc11 := P_CeilingHeight(frontsector, curline.v1.x, curline.v1.y) - viewz;
  sc22 := sc11;
  sf11 := P_FloorHeight(frontsector, curline.v1.x, curline.v1.y) - viewz;
  sf22 := sf11;

  // JVAL: Shut up compiler warning
  sc1 := 0;
  sc2 := 0;
  sf1 := 0;
  sf2 := 0;

  if backsector <> nil then
  begin
    sc1 := P_CeilingHeight(backsector, curline.v1.x, curline.v1.y) - viewz;
    sc2 := sc1;
    sf1 := P_FloorHeight(backsector, curline.v1.x, curline.v1.y) - viewz;
    sf2 := sf1;
    if (frontsector.ceilingpic = skyflatnum) and (backsector.ceilingpic = skyflatnum) then  // JVAL: SOS check floorpic = skyflatnum
    begin
      sc11 := sc1;
      sc22 := sc2;
    end;
    side := intval(@sides[linedef.sidenum[0]] = sidedef);
    if linedef.clslopestep[side] <> 0.0 then
    begin
      s11 := Round(texturecolumn * linedef.clslopestep[side]);
      s22 := Round(texturecolumn2 * linedef.clslopestep[side]);
      if side <> 0 then
      begin
        s11 := -s11;
        s22 := -s22;
      end;
      sc2 := sc2 + s22;
      sc1 := sc1 + s11;
    end;

    pixhigh_dbl := (centeryfrac / WORLDUNIT) - (rw_scale_dbl / FRACUNIT) * (sc1 / WORLDUNIT);
    pixhighstep_dbl := - (rw_scalestep_dbl / FRACUNIT) * (sc2 / WORLDUNIT) +
        (rw_scale_dbl / FRACUNIT) * ((sc1 - sc2) / WORLDUNIT) / (rw_stopx - rw_x);

    pixhigh := (int64(centeryfrac) div WORLDUNIT) - int64(sc1 div WORLDUNIT) * int64(rw_scale) div FRACUNIT;
    pixhighstep := -FixedMul(rw_scalestep, sc2 div WORLDUNIT) +
        Round(FixedMul(rw_scale, (sc1 - sc2) div WORLDUNIT) / (rw_stopx - rw_x));

    if linedef.flslopestep[side] <> 0.0 then
    begin
      s11 := Round(texturecolumn * linedef.flslopestep[side]);
      s22 := Round(texturecolumn2 * linedef.flslopestep[side]);
      if side <> 0 then
      begin
        s11 := -s11;
        s22 := -s22;
      end;
      sf2 := sf2 + s22;
      sf1 := sf1 + s11;
    end;

    pixlow_dbl := (centeryfrac / WORLDUNIT) - (rw_scale / FRACUNIT) * (sf1 / WORLDUNIT);
    pixlowstep_dbl := -(rw_scalestep_dbl / FRACUNIT) * (sf2 / WORLDUNIT) +
        (rw_scale_dbl / FRACUNIT) * ((sf1 - sf2) / WORLDUNIT) / (rw_stopx - rw_x);

    pixlow := (int64(centeryfrac) div WORLDUNIT) - int64(sf1 div WORLDUNIT) * int64(rw_scale) div FRACUNIT;
    pixlowstep := -FixedMul(rw_scalestep, sf2 div WORLDUNIT) +
        Round(FixedMul(rw_scale, (sf1 - sf2) div WORLDUNIT) / (rw_stopx - rw_x));
  end;

  side := intval(@sides[linedef.sidenum[0]] <> sidedef);

  if linedef.clslopestep[side] <> 0.0 then
  begin
    s11 := Round(texturecolumn * linedef.clslopestep[side]);
    s22 := Round(texturecolumn2 * linedef.clslopestep[side]);
    if side <> 0 then
    begin
      s11 := -s11;
      s22 := -s22;
    end;
    sc22 := sc22 - s22;
    sc11 := sc11 - s11;
  end;
  topfrac_dbl := (centeryfrac / WORLDUNIT) - (rw_scale_dbl / FRACUNIT) * (sc11 / WORLDUNIT);
  topstep_dbl := -(rw_scalestep_dbl / FRACUNIT) * (sc22 / WORLDUNIT) +
      (rw_scale_dbl / FRACUNIT) * ((sc11 - sc22) / WORLDUNIT) / (rw_stopx - rw_x);

  topfrac := (int64(centeryfrac) div WORLDUNIT) - int64(sc11 div WORLDUNIT) * int64(rw_scale) div FRACUNIT;
  topstep := -FixedMul(rw_scalestep, sc22 div WORLDUNIT) +
      Round(FixedMul(rw_scale, (sc11 - sc22) div WORLDUNIT) / (rw_stopx - rw_x));

  if linedef.flslopestep[side] <> 0.0 then
  begin
    s11 := Round(texturecolumn * linedef.flslopestep[side]);
    s22 := Round(texturecolumn2 * linedef.flslopestep[side]);
    if side <> 0 then
    begin
      s11 := -s11;
      s22 := -s22;
    end;
    sf22 := sf22 - s22;
    sf11 := sf11 - s11;
  end;
  bottomfrac_dbl := (centeryfrac / WORLDUNIT) - (rw_scale_dbl / FRACUNIT) * (sf11 / WORLDUNIT);
  bottomstep_dbl := -(rw_scalestep_dbl / FRACUNIT) * (sf22 / WORLDUNIT) +
      (rw_scale_dbl / FRACUNIT) * ((sf11 - sf22) / WORLDUNIT) / (rw_stopx - rw_x);

  bottomfrac := (int64(centeryfrac) div WORLDUNIT) - int64(sf11 div WORLDUNIT) * int64(rw_scale) div FRACUNIT;
  bottomstep := -FixedMul(rw_scalestep, sf22 div WORLDUNIT) +
      Round(FixedMul(rw_scale, (sf11 - sf22) div WORLDUNIT) / (rw_stopx - rw_x));

  midtexture := 0;
  toptexture := 0;
  bottomtexture := 0;
  maskedtexture := false;

  pds.silhouette := SIL_BOTH;
  pds.bsilheight := MAXINT;
  pds.tsilheight := MININT;

  if backsector = nil then
  begin
    // single sided line
    midtexture := texturetranslation[sidedef.midtexture];

    rw_midtexturemid := frontsector.ceilingheight - textureheight[sidedef.midtexture] - viewz;

    rw_midtexturemid := rw_midtexturemid + sidedef.rowoffset;

    pds.sprtopclip := @screenheightarray;
    pds.sprbottomclip := @negonearray;
  end
  else
  begin
    // two sided line
    pds.sprtopclip := nil;
    pds.sprbottomclip := nil;


    if (sc11 > sc1) or (sc22 > sc2) then
    begin
      toptexture := texturetranslation[sidedef.toptexture];
      rw_toptexturemid := frontsector.ceilingheight - viewz;
    end;

    if (sf11 < sf1) or (sf22 < sf2) then
    begin
      bottomtexture := texturetranslation[sidedef.bottomtexture];
      rw_bottomtexturemid := frontsector.floorheight - viewz;
    end;

    rw_toptexturemid := rw_toptexturemid + sidedef.rowoffset;
    rw_bottomtexturemid := rw_bottomtexturemid + sidedef.rowoffset;

    // JVAL: 3d Floors
    R_StoreThickSideRange(pds, frontsector, backsector);

    if sf1 < sf2 then
      sf1 := sf2;
    if sc1 > sc2 then
      sc1 := sc2;
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
    rw_offset := rw_offset + sidedef.textureoffset + curline.offset;

  // killough 3/7/98: add deep water check
  {$IFDEF DOOM_OR_STRIFE}
  if frontsector.heightsec = -1 then
  {$ENDIF}
  begin
    if P_FloorHeight(frontsector, viewx, viewy) >= viewz then
    begin
      // above view plane
      markfloor := false;
    end;

    if (P_CeilingHeight(frontsector, viewx, viewy) <= viewz) and
       (frontsector.ceilingpic <> skyflatnum) then
    begin
      // below view plane
      markceiling := false;
    end;
  end;

  if sc11 > sc22 then
    sc11 := sc22;
  if sf11 < sf22 then
    sf11 := sf22;

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

  if pds.use_double then
  begin
    if pds.midvis <> nil then
    begin
      if (pds.midsec <> nil) then
      begin
        f_RenderSegLoop_dbl_3dFloors_Vis(pds);
      end
      else
      begin
        f_RenderSegLoop_dbl_Vis(pds);
      end;
    end
    else
    begin
      if (pds.midsec <> nil) then
      begin
        f_RenderSegLoop_dbl_3dFloors(pds);
      end
      else
      begin
        f_RenderSegLoop_dbl;
      end;
    end;
  end
  else
  begin
    if pds.midvis <> nil then
    begin
      if (pds.midsec <> nil) then
      begin
        f_RenderSegLoop_3dFloors_Vis(pds);
      end
      else
      begin
        f_RenderSegLoop_Vis(pds);
      end;
    end
    else
    begin
      if (pds.midsec <> nil) then
      begin
        f_RenderSegLoop_3dFloors(pds);
      end
      else
      begin
        f_RenderSegLoop;
      end;
    end;
  end;

  if (sf11 > sf1) and (sf1 < 0) and (pds.bsilheight < sf11) then
    pds.bsilheight := sf11;
  if (sc11 < sc1) and (sc1 > 0) and (pds.tsilheight > sc11) then
    pds.tsilheight := sc11;

  // save sprite clipping info
  if pds.sprtopclip = nil then
  begin
    memcpy(@openings[lastopening], @ceilingclip[start], SizeOf(ceilingclip[0]) * (rw_stopx - start));
    pds.sprtopclip := PSmallIntArray(@openings[lastopening - start]);
    lastopening := lastopening + rw_stopx - start;
  end;

  if pds.sprbottomclip = nil then
  begin
    memcpy(@openings[lastopening], @floorclip[start], SizeOf(floorclip[0]) * (rw_stopx - start));
    pds.sprbottomclip := PSmallIntArray(@openings[lastopening - start]);
    lastopening := lastopening + rw_stopx - start;
  end;

  inc(ds_p);
end;

type
  sspoint_t = record
    screenX, screenY, screenZ: Double; // SCREEN UNITS
    worldX, worldY, worldZ: Double;    // WORLD UNITS
  end;
  Psspoint_t = ^sspoint_t;
  sspoint_tArray = array[0..MAXSUBSECTORPOINTS - 1] of sspoint_t;
  ssPpoint_tArray = ^sspoint_tArray;

var
  sspoints: sspoint_tArray;

// input: wx, wy, wz -> World coordinates
// output: x, y, z -> Screen coordinates
procedure R_SlopesCalcSSPoint(const wx, wy, wz: fixed_t; var x, y, z: Double);
var
  x1, y1: Double;
  tr_x: Double;
  tr_y: Double;
  tr_z: Double;
  tx: Double;
  tz: Double;
  xscale: Double;
  yscale: Double;
  dcos, dsin: Double;
begin
  tr_x := (wx - viewx) / FRACUNIT;
  tr_y := (wy - viewy) / FRACUNIT;
  tr_z := (wz - viewz) / FRACUNIT;
  dcos := dviewcos;
  dsin := dviewsin;

  tz := tr_x * dcos + tr_y * dsin;

  z := projection / tz;
  xscale := projection / FRACUNIT / tz;

  tx := tr_x * dsin - tr_y * dcos;

  x1 := centerx + tx * xscale;

  yscale := projectiony / FRACUNIT / tz;
  y1 := centery - tr_z * yscale;

  x := x1;
  y := y1;
end;

const
  SLOPESPLITFACTOR = 2;
  SLOPECOEFF = 256 * FRACUNIT;

function R_VisSlopeFromSubsector(const ssector: Psubsector_t;
  const virtualfloor: boolean): Pvisslope_t;
var
  i, j, k, l: integer;
  seg: Pseg_t;
  h: fixed_t;
  p, p1, p2: Psspoint_t;
  numsspoints: integer;
  plane: Pvisslope_t;
  y1, y2, x: integer;
  xpos: Double;
  xfrac: Double;
  tpos: Double;
  tfrac: Double;
  top, bottom: integer;
  pic: integer;
  subjI: TPaths;
  clipI: TPaths;
  solutionI: TPaths;
  cnt: integer;
  ret: boolean;
  fovhangle: angle_t;
  y2stop: integer;
  sX, sY: integer;
  sec: Psector_t;
  Theta, rTheta: Double;
  xscale, yscale: fixed_t;
  {$IFDEF DEBUG}
  slopeheight: fixed_t;
  {$ENDIF}
  tX, tY, tZ: Double;
  splitfactor: integer;
begin
  if map_max_bound > 4096 then
    splitfactor := 1
  else
    splitfactor := SLOPESPLITFACTOR;
  // JVAL: Get Points from segs
  // SPEEDUP -> TO BE MOVED IN P_SETUP, ALONG WITH TClipper contruction ?
  SetLength(subjI, 1);
  SetLength(subjI[0], ssector.numlines * splitfactor);
  k := 0;
  seg := @segs[ssector.firstline];
  for j := 0 to ssector.numlines - 1 do
  begin
    subjI[0][k] := R_MakeClipperPoint(seg.v1);
    Inc(k);
    for l := 1 to splitfactor - 1 do
    begin
      subjI[0][k] := R_MakeClipperPoint(
        (seg.v1.x * (splitfactor - l) + seg.v2.x * l) div splitfactor,
        (seg.v1.y * (splitfactor - l) + seg.v2.y * l) div splitfactor
       );
      Inc(k);
    end;
    Inc(seg);
  end;

  fovhangle := fov * ANGLETOFINEUNIT div 2;
  // MOVE THIS TO R_RenderPlayerView ?
  SetLength(clipI, 1);
  SetLength(clipI[0], 4);
  clipI[0][0] := R_MakeClipperPoint(
    viewx + Round(cos((viewangle + fovhangle) * ANGLE_T_TO_RAD) * MINZ),
    viewy + Round(sin((viewangle + fovhangle) * ANGLE_T_TO_RAD) * MINZ));
  clipI[0][1] := R_MakeClipperPoint(
    viewx + Round(cos((viewangle - fovhangle) * ANGLE_T_TO_RAD) * MINZ),
    viewy + Round(sin((viewangle - fovhangle) * ANGLE_T_TO_RAD) * MINZ));
  clipI[0][2] := R_MakeClipperPoint(
    viewx + Round(cos((viewangle - fovhangle) * ANGLE_T_TO_RAD) * MAXZ),
    viewy + Round(sin((viewangle - fovhangle) * ANGLE_T_TO_RAD) * MAXZ));
  clipI[0][3] := R_MakeClipperPoint(
    viewx + Round(cos((viewangle + fovhangle) * ANGLE_T_TO_RAD) * MAXZ),
    viewy + Round(sin((viewangle + fovhangle) * ANGLE_T_TO_RAD) * MAXZ));

  with TClipper.Create([ioPreserveCollinear]) do
  try
    AddPaths(subjI, ptSubject, true);
    AddPaths(clipI, ptClip, true);
    ret := Execute(ctIntersection, solutionI, pftNonZero, pftNonZero);
  finally
    Free;
  end;

  SetLength(subjI[0], 0);
  SetLength(subjI, 0);
  SetLength(clipI[0], 0);
  SetLength(clipI, 0);

  sec := ssector.sector;

  if length(solutionI) = 0 then
  begin
    Result := R_FindExistingVisSlope(sec.iSectorID, virtualfloor);
    exit;
  end;

  if not ret then
  begin
    SetLength(solutionI[0], 0);
    SetLength(solutionI, 0);
    Result := R_FindExistingVisSlope(sec.iSectorID, virtualfloor);
    exit;
  end;

  // JVAL: Rebuild point list
  numsspoints := 0;

  cnt := Length(solutionI[0]);
  if cnt >= MAXSUBSECTORPOINTS then
    cnt := MAXSUBSECTORPOINTS - 1;  // JVAL: Do not overflow and crash
  if virtualfloor then
  begin
    {$IFDEF DEBUG}
    slopeheight := P_FloorHeight(sec, viewx, viewy);
    {$ENDIF}
    for j := 0 to cnt - 1 do
    begin
      sX := solutionI[0][j].x * POINTUNIT + viewx;
      sY := solutionI[0][j].y * POINTUNIT + viewy;
      h := P_FloorHeight(sec, sX, sY);
      R_SlopesCalcSSPoint(
        sX,
        sY,
        h,
        sspoints[numsspoints].screenX,
        sspoints[numsspoints].screenY,
        sspoints[numsspoints].screenZ,
      );

      sspoints[numsspoints].worldX := sX;
      sspoints[numsspoints].worldY := sY;

      R_SlopesCalcSSPoint(
        sX,
        sY,
        h + SLOPECOEFF,
        tX,
        tY,
        tZ,
      );

      // Find best spot
      if Abs(centery - tY) > Abs(centery - sspoints[numsspoints].screenY) then
        sspoints[numsspoints].worldZ := (h - viewz + SLOPECOEFF) /
                           (-centeryfrac - FRACUNIT div 2 + tY * FRACUNIT) *
                            projectiony
      else
        sspoints[numsspoints].worldZ := (h - viewz) /
                           (-centeryfrac - FRACUNIT div 2 + sspoints[numsspoints].screenY * FRACUNIT) *
                            projectiony;

      Inc(numsspoints);
    end;
  end
  else
  begin
    {$IFDEF DEBUG}
    slopeheight := P_CeilingHeight(sec, viewx, viewy);
    {$ENDIF}
    for j := 0 to cnt - 1 do
    begin
      sX := solutionI[0][j].x * POINTUNIT + viewx;
      sY := solutionI[0][j].y * POINTUNIT + viewy;
      h := P_CeilingHeight(sec, sX, sY);
      R_SlopesCalcSSPoint(
        sX,
        sY,
        h,
        sspoints[numsspoints].screenX,
        sspoints[numsspoints].screenY,
        sspoints[numsspoints].screenZ
      );

      sspoints[numsspoints].worldX := sX;
      sspoints[numsspoints].worldY := sY;

      R_SlopesCalcSSPoint(
        sX,
        sY,
        h + SLOPECOEFF,
        tX,
        tY,
        tZ,
      );

      // Find best spot
      if Abs(centery - tY) > Abs(centery - sspoints[numsspoints].screenY) then
        sspoints[numsspoints].worldZ := (h - viewz + SLOPECOEFF) /
                           (-centeryfrac - FRACUNIT div 2 + tY * FRACUNIT) *
                            projectiony
      else
        sspoints[numsspoints].worldZ := (h - viewz) /
                           (-centeryfrac - FRACUNIT div 2 + sspoints[numsspoints].screenY * FRACUNIT) *
                            projectiony;

      Inc(numsspoints);

    end;
  end;
  sspoints[numsspoints] := sspoints[0];
  Inc(numsspoints);

  plane := R_FindVisSlope(sec.iSectorID, virtualfloor);
  if plane = nil then
  begin
    SetLength(solutionI[0], 0);
    SetLength(solutionI, 0);
    exit; // Ouch!
  end;

  {$IFDEF DEBUG}
  plane.slopeviewheight := viewz - slopeheight;
  plane.slopeviewZ := slopeheight;
  {$ENDIF}

  for i := 0 to numsspoints - 2 do
  begin
    p1 := @sspoints[i];
    p2 := @sspoints[i + 1];
    if p1.screenY > p2.screenY then
    begin
      p := p1;
      p1 := p2;
      p2 := p;
    end;

    y1 := Round(p1.screenY);
    if y1 < 0 then
      y1 := 0
    else if y1 >= viewheight then
      y1 := viewheight - 1;

    y2 := Round(p2.screenY);
    if y2 < 0 then
    begin
      y2 := 0;
      y2stop := 0;
    end
    else if y2 >= viewheight then
    begin
      y2 := viewheight;
      y2stop := y2;
    end
    else
      y2stop := y2 + 1;

    if y1 < plane.miny then
      plane.miny := y1;
    if y2 > plane.maxy then
      plane.maxy := y2;

    if y1 = y2 then
    begin
      xfrac := 0.0;
      tfrac := 0.0;
    end
    else
    begin
      xfrac := (p1.screenX - p2.screenX) / (p1.screenY - p2.screenY);
      tfrac := (p1.screenZ - p2.screenZ) / (p1.screenY - p2.screenY);
    end;

    xpos := p1.screenX + xfrac * (y1 - p1.screenY);
    tpos := p1.screenZ + tfrac * (y1 - p1.screenY);

    // For all columns in span
    repeat
      x := Round(xpos);
      if x < plane.screenleft[y1] then
      begin
        plane.screenleft[y1] := x;
        if (y1 = p1.screenY) then
        begin
          plane.ds_zleft[y1] := Round(p1.worldZ);
        end
        else if (y1 = p2.screenY) then
        begin
          plane.ds_zleft[y1] := Round(p2.worldZ);
        end
        else if (p1.worldZ = p2.worldZ) then
        begin
          plane.ds_zleft[y1] := Round(p2.worldZ);
        end
        else if (p1.screenZ = p2.screenZ) then
        begin
          plane.ds_zleft[y1] := Round(p2.worldZ);
        end
        else
        begin
          Theta := (1.0 / tpos - 1.0 / p1.screenZ) / (1.0 / p2.screenZ - 1.0 / p1.screenZ);
          rTheta := 1.0 - Theta;

          plane.ds_zleft[y1] :=
            Round(rTheta * p1.worldZ + Theta * p2.worldZ);
        end;
      end;
      if x > plane.screenright[y1] then
      begin
        plane.screenright[y1] := x;
        if (y1 = p1.screenY) then
        begin
          plane.ds_zright[y1] := Round(p1.worldZ);
        end
        else if (y1 = p2.screenY) then
        begin
          plane.ds_zright[y1] := Round(p2.worldZ);
        end
        else if (p1.worldZ = p2.worldZ) then
        begin
          plane.ds_zright[y1] := Round(p2.worldZ);
        end
        else if (p1.screenZ = p2.screenZ) then
        begin
          plane.ds_zright[y1] := Round(p2.worldZ);
        end
        else
        begin
          Theta := (1.0 / tpos - 1.0 / p1.screenZ) / (1.0 / p2.screenZ - 1.0 / p1.screenZ);
          rTheta := 1.0 - Theta;

          plane.ds_zright[y1] :=
            Round(rTheta * p1.worldZ + Theta * p2.worldZ);
        end;
      end;

      xpos := xpos + xfrac;
      tpos := tpos + tfrac;

      Inc(y1);
    until y1 >= y2stop;
  end;

  // JVAL: MINZ correction
  if plane.maxy = viewheight then
    if plane.miny < viewheight - 2 then
    begin
      plane.screenleft[viewheight - 1] := 2 * plane.screenleft[viewheight - 2] - plane.screenleft[viewheight - 3];
      plane.screenright[viewheight - 1] := 2 * plane.screenright[viewheight - 2] - plane.screenright[viewheight - 3];
      plane.ds_zleft[viewheight - 1] := 2 * plane.ds_zleft[viewheight - 2] - plane.ds_zleft[viewheight - 3];
      plane.ds_zright[viewheight - 1] := 2 * plane.ds_zright[viewheight - 2] - plane.ds_zright[viewheight - 3];
    end;

  if plane.miny = 0 then
    if plane.maxy > 1 then
    begin
      plane.screenleft[0] := 2 * plane.screenleft[1] - plane.screenleft[2];
      plane.screenright[0] := 2 * plane.screenright[1] - plane.screenright[2];
      plane.ds_zleft[0] := 2 * plane.ds_zleft[1] - plane.ds_zleft[2];
      plane.ds_zright[0] := 2 * plane.ds_zright[1] - plane.ds_zright[2];
    end;

  SetLength(solutionI[0], 0);
  SetLength(solutionI, 0);

  result := plane;
end;

procedure R_VisSlopesFromSubsector(const ssector: Psubsector_t);
var
  sec: Psector_t;
begin
  sec := ssector.sector;
  if sec.renderflags and SRF_SLOPEFLOOR <> 0 then
    floorslope := R_VisSlopeFromSubsector(ssector, True)
  else
    floorslope := nil;
  if sec.renderflags and SRF_SLOPECEILING <> 0 then
    ceilingslope := R_VisSlopeFromSubsector(ssector, False)
  else
    ceilingslope := nil;
end;

end.

