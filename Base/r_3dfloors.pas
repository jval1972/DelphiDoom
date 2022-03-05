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
//   3D floors software rendering.
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit r_3dfloors;

interface

uses
  d_delphi,
  m_fixed,
  r_visplanes,
  r_defs;

var
  hasExtraFloors: Boolean = False;
  viewsubsector: Psubsector_t;

//==============================================================================
//
// R_RenderThickSideRange
//
//==============================================================================
procedure R_RenderThickSideRange(const ds: Pdrawseg_t; const x1, x2: integer);

//==============================================================================
//
// R_StoreThickSideRange
//
//==============================================================================
procedure R_StoreThickSideRange(const ds: Pdrawseg_t; const frontsector, backsector: Psector_t);

//==============================================================================
//
// R_3dVisplaneFromSubsector
//
//==============================================================================
procedure R_3dVisplaneFromSubsector(const ssector: Psubsector_t; const floorlightlevel: Psmallint);

var
  spritelights2: PBytePArray;
  walllights2: PBytePArray;
  dc_colormap2: PByteArray;
  dc_colormap322: PLongWordArray;
  dc_llindex2: integer;
  dc_lightlevel2: integer;

const
  MAXVISPLANES3D = $10000;

var
  visplanes3d: array[0..MAXVISPLANES3D - 1] of visplane3d_t;
  maxvisplane3d: Integer = -1;
  lastvisplane3d: integer = 0;
  curmidvis: Pvisplane3d_t; // JVAL: 3d Floors
  seglooplightlevel1, seglooplightlevel2: integer;
  segloopcolormap1, segloopcolormap2: PByteArray;

//==============================================================================
//
// R_DrawFFloors
//
//==============================================================================
procedure R_DrawFFloors;  // JVAL: 3d Floors

//==============================================================================
//
// R_DrawFFloorsMultiThread
//
//==============================================================================
procedure R_DrawFFloorsMultiThread;  // JVAL: 3d Floors

//==============================================================================
//
// R_ClearVisPlanes3d
//
//==============================================================================
procedure R_ClearVisPlanes3d;

implementation

uses
  doomtype,
  doomdef,
  p_setup,
  r_column,
  r_data,
  r_draw,
  r_hires,
  r_main,
  r_plane,
  r_segs,
  r_things,
  r_utils,
  r_clipper,
  r_cliputils,
  r_depthbuffer,
  r_zbuffer,
  tables,
  z_zone;

//==============================================================================
//
// R_StoreThickSideRange
//
//==============================================================================
procedure R_StoreThickSideRange(const ds: Pdrawseg_t; const frontsector, backsector: Psector_t);
var
  hicut, lowcut: fixed_t; // JVAL: 3d Floors
  backmidsec, frontmidsec: Psector_t; // JVAL: 3d Floors
  ceil1, floor1: integer;
begin
  if (backsector.midsec >= 0) and (frontsector.midsec >= 0) then
  begin
    if frontsector.floorheight > backsector.floorheight then
      lowcut :=  frontsector.floorheight
    else
      lowcut :=  backsector.floorheight;
    if frontsector.ceilingheight < backsector.ceilingheight then
      hicut := frontsector.ceilingheight
    else
      hicut := backsector.ceilingheight;
    backmidsec := @sectors[backsector.midsec];
    frontmidsec := @sectors[frontsector.midsec];
    if (backmidsec.ceilingheight < lowcut) or (backmidsec.floorheight > hicut) then
    else if (frontmidsec.ceilingheight < lowcut) or (frontmidsec.floorheight > hicut) then
    else if (backmidsec.ceilingheight > frontsector.ceilingheight) or (backmidsec.floorheight < frontsector.floorheight) then
    else if (frontmidsec.ceilingheight = backmidsec.ceilingheight) and (frontmidsec.floorheight = backmidsec.floorheight) then
    else
    begin
      maskedtexture := true;
      maskedtexturecol := PSmallIntArray(@openings[lastopening - rw_x]);
      ds.thicksidecol := maskedtexturecol;
      lastopening := lastopening + rw_stopx - rw_x;
      ds.midsec := backmidsec;
      ds.midside := @sides[lines[backsector.midline].sidenum[0]];
      ceil1 := MinI(frontsector.ceilingheight, backmidsec.ceilingheight);
      floor1 := MaxI(frontsector.floorheight, backmidsec.floorheight);
      // Store clipping info. (In case of multiple midfloors -> overlapping ranges intersection problem)
      if (frontmidsec.ceilingheight <= floor1) or (frontmidsec.floorheight >= ceil1) then
      begin
        ds.midsiderange.count := 1;
        ds.midsiderange.ceilingheight[0] := ceil1;
        ds.midsiderange.floorheight[0] := floor1;
        if ceil1 <= frontmidsec.floorheight then
        begin
          ds.midsiderange.lightlevel[0] := frontmidsec.lightlevel;
          ds.midsiderange.fog[0] := frontmidsec.renderflags and SRF_FOG <> 0; // JVAL: Mars fog sectors
        end
        else
        begin
          ds.midsiderange.lightlevel[0] := frontsector.lightlevel;
          ds.midsiderange.fog[0] := frontsector.renderflags and SRF_FOG <> 0; // JVAL: Mars fog sectors
        end;
      end
      else if (frontmidsec.ceilingheight > floor1) and (frontmidsec.floorheight < ceil1) then
      begin
        ds.midsiderange.count := 0;
        if frontmidsec.ceilingheight < ceil1 then
        begin
          inc(ds.midsiderange.count);
          ds.midsiderange.ceilingheight[0] := ceil1;
          ds.midsiderange.floorheight[0] := frontmidsec.ceilingheight;
          ds.midsiderange.lightlevel[0] := frontsector.lightlevel;
          ds.midsiderange.fog[0] := frontsector.renderflags and SRF_FOG <> 0; // JVAL: Mars fog sectors
        end;
        if frontmidsec.floorheight > floor1 then
        begin
          ds.midsiderange.ceilingheight[ds.midsiderange.count] := frontmidsec.floorheight;
          ds.midsiderange.floorheight[ds.midsiderange.count] := floor1;
          ds.midsiderange.lightlevel[ds.midsiderange.count] := frontmidsec.lightlevel;
          ds.midsiderange.fog[ds.midsiderange.count] := frontmidsec.renderflags and SRF_FOG <> 0; // JVAL: Mars fog sectors
          inc(ds.midsiderange.count);
        end;
      end
      else
      begin
        ds.midsiderange.count := 1;
        if frontmidsec.ceilingheight >= ceil1 then
        begin
          ds.midsiderange.ceilingheight[0] := frontmidsec.floorheight;
          ds.midsiderange.floorheight[0] := floor1;
          ds.midsiderange.lightlevel[0] := frontmidsec.lightlevel;
          ds.midsiderange.fog[0] := frontmidsec.renderflags and SRF_FOG <> 0; // JVAL: Mars fog sectors
        end
        else
        begin
          ds.midsiderange.ceilingheight[0] := ceil1;
          ds.midsiderange.floorheight[0] := frontmidsec.ceilingheight;
          ds.midsiderange.lightlevel[0] := frontsector.lightlevel;
          ds.midsiderange.fog[0] := frontsector.renderflags and SRF_FOG <> 0; // JVAL: Mars fog sectors
        end;
      end;
    end;
  end
  else if backsector.midsec >= 0 then
  begin
    backmidsec := @sectors[backsector.midsec];
    if (backmidsec.ceilingheight <= frontsector.floorheight) or (backmidsec.floorheight >= frontsector.ceilingheight) then
    else
    begin
      maskedtexture := true;
      maskedtexturecol := PSmallIntArray(@openings[lastopening - rw_x]);
      ds.thicksidecol := maskedtexturecol;
      lastopening := lastopening + rw_stopx - rw_x;
      ds.midsec := backmidsec;
      ds.midside := @sides[lines[backsector.midline].sidenum[0]];
      ds.midsiderange.count := 1;
      ds.midsiderange.ceilingheight[0] := MaxI(frontsector.ceilingheight, backmidsec.ceilingheight);
      ds.midsiderange.floorheight[0] := MinI(frontsector.floorheight, backmidsec.floorheight);
      ds.midsiderange.lightlevel[0] := frontsector.lightlevel;
      ds.midsiderange.fog[0] := frontsector.renderflags and SRF_FOG <> 0; // JVAL: Mars fog sectors
    end;
  end;
end;

//==============================================================================
// R_DoRenderThickSideRange1_DBL
//
// R_RenderThickSideRange
//
//==============================================================================
procedure R_DoRenderThickSideRange1_DBL(const ds: Pdrawseg_t; const x1, x2: integer);
var
  index: integer;
  lightnum: integer;
  texnum: integer;
  i: integer;
  texturecolumn: integer;
  curline: Pseg_t;
  mid: Psector_t;
  midside: Pside_t;
  roverscale_dbl, roverstep_dbl: Double;
  texscale_dbl, texstep_dbl: Double;
  spryscale_dbl: Double;
  sprtopscreen_dbl: Double;
begin
  // Calculate light table.
  // Use different light tables
  //   for horizontal / vertical / diagonal. Diagonal?
  // OPTIMIZE: get rid of LIGHTSEGSHIFT globally

  mid := ds.midsec;
  midside := ds.midside; //@sides[mid.midline.sidenum[0]];
  curline := ds.curline;
  texnum := texturetranslation[midside.midtexture];

  R_GetDCs(texnum, 0); // JVAL Also precache external texture if not loaded

  lightnum := _SHR(ds.midsiderange.lightlevel[0], LIGHTSEGSHIFT) + extralight;

  if r_fakecontrast then
    inc(lightnum, curline.fakecontrastlight);

  if lightnum < 0 then
    lightnum := 0
  else if lightnum >= LIGHTLEVELS then
    lightnum := LIGHTLEVELS - 1;

  dc_fog := ds.midsiderange.fog[0]; // JVAL: Mars fog sectors
  if dc_fog then // JVAL: Mars fog sectors
    walllights := @fog_scalelight[lightnum]
  else
    walllights := @scalelight[lightnum];

  dc_llindex := lightnum;

  spryscale_dbl := ds.scale_dbl + (x1 - ds.x1) * ds.scalestep_dbl;
  mfloorclip := ds.sprbottomclip;
  mceilingclip := ds.sprtopclip;
  maskedtexturecol := ds.thicksidecol;

  // find positioning
  dc_texturemid := mid.ceilingheight - viewz;

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

  texscale_dbl := dc_texturemid / FRACUNIT * ds.scale_dbl;

  // draw the columns
  roverscale_dbl := (mid.ceilingheight - mid.floorheight) / FRACUNIT;
  roverstep_dbl := roverscale_dbl * ds.scalestep_dbl;
  roverscale_dbl := roverscale_dbl * ds.scale_dbl + (x1 - ds.x1) * roverstep_dbl;
  texstep_dbl := dc_texturemid / FRACUNIT * ds.scalestep_dbl;
  texscale_dbl := texscale_dbl + (x1 - ds.x1) * texstep_dbl;
  // Thick side gets row offset from control line
  dc_texturemid := dc_texturemid + midside.rowoffset + midside.midrowoffset;
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
          if spryscale_dbl > 256 * FRACUNIT then
            index := (_SHR(trunc(spryscale_dbl), HLL_LIGHTSCALESHIFT + 2) div SCREENWIDTH) * 320
          else
            index := (_SHR(trunc(spryscale_dbl), HLL_LIGHTSCALESHIFT + 2) * 320) div SCREENWIDTH;
          if index >= HLL_MAXLIGHTSCALE then
            index := HLL_MAXLIGHTSCALE - 1
          else if index < 0 then
            index := 0;
          dc_lightlevel := scalelightlevels[dc_llindex, index];
        end;
        index := _SHR(trunc(spryscale_dbl), LIGHTSCALESHIFT) * 320 div SCREENWIDTH;

        if index >=  MAXLIGHTSCALE then
          index := MAXLIGHTSCALE - 1
        else if index < 0 then
          index := 0;

        dc_colormap := walllights[index];
        if videomode = vm32bit then
          dc_colormap32 := R_GetColormap32(dc_colormap);
      end;

      sprtopscreen_dbl := centeryfrac - texscale_dbl;

      if (spryscale_dbl < 4) and (spryscale_dbl > -4) then
      begin
        if spryscale_dbl > 0 then
          dc_iscale := MAXINT div 2
        else
          dc_iscale := - MAXINT div 2
      end
      else
      begin
        dc_iscale := trunc($100000000 / spryscale_dbl);
        if dc_iscale > MAXINT div 2 then
          dc_iscale := MAXINT div 2
        else if dc_iscale < -MAXINT div 2 then
          dc_iscale := -MAXINT div 2
      end;
      if (dc_iscale < 4) and (dc_iscale > -4) then
      begin
        if dc_iscale > 0 then
          dc_iscale := 4
        else
          dc_iscale := -4
      end;

      texturecolumn := maskedtexturecol[dc_x] shr DC_HIRESBITS;
      dc_yl := trunc((sprtopscreen_dbl + FRACUNIT) / FRACUNIT);
      dc_yh := trunc((sprtopscreen_dbl + roverscale_dbl) / FRACUNIT);
      if dc_yh >= mfloorclip[dc_x] then
        dc_yh := mfloorclip[dc_x] - 1;
      if dc_yl <= mceilingclip[dc_x] then
        dc_yl := mceilingclip[dc_x] + 1;

      if dc_yh < 0 then
        dc_yh := 0;
      if dc_yl <= dc_yh then
      begin
        if videomode = vm32bit then
        begin
          dc_mod := 0;
          dc_texturemod := maskedtexturecol[dc_x] and (DC_HIRESFACTOR - 1);
          R_GetDCs(texnum, texturecolumn);
        end
        else
        begin
          dc_source := R_GetColumn(texnum, texturecolumn);
        end;
        if depthbufferactive then
          R_DrawColumnWithDepthBufferCheckWrite(wallcolfunc)
        else
          wallcolfunc;

        if domaskedzbuffer then
          R_DrawColumnToZBuffer;
      end;

      maskedtexturecol[dc_x] := MAXSHORT;
    end;
    spryscale_dbl := spryscale_dbl + ds.scalestep_dbl;
    roverscale_dbl := roverscale_dbl + roverstep_dbl;
    texscale_dbl := texscale_dbl + texstep_dbl;
  end;
end;

//==============================================================================
//
// R_DoRenderThickSideRange2_DBL
//
//==============================================================================
procedure R_DoRenderThickSideRange2_DBL(const ds: Pdrawseg_t; const x1, x2: integer);
var
  index: integer;
  lightnum: array[0..1] of integer;
  texnum: integer;
  i: integer;
  texturecolumn: integer;
  curline: Pseg_t;
  midside: Pside_t;
  roverscale_dbl, roverstep_dbl: array[0..1] of Double;
  texscale_dbl, texstep_dbl: array[0..1] of Double;
  texturemid: array[0..1] of fixed_t;
  spryscale_dbl: Double;
  sprtopscreen_dbl: Double;
begin
  // Calculate light table.
  // Use different light tables
  //   for horizontal / vertical / diagonal. Diagonal?
  // OPTIMIZE: get rid of LIGHTSEGSHIFT globally

  midside := ds.midside; //@sides[mid.midline.sidenum[0]];
  curline := ds.curline;
  texnum := texturetranslation[midside.midtexture];

  R_GetDCs(texnum, 0); // JVAL Also precache external texture if not loaded

  lightnum[0] := _SHR(ds.midsiderange.lightlevel[0], LIGHTSEGSHIFT) + extralight;
  lightnum[1] := _SHR(ds.midsiderange.lightlevel[1], LIGHTSEGSHIFT) + extralight;

  if r_fakecontrast then
  begin
    inc(lightnum[0], curline.fakecontrastlight);
    inc(lightnum[1], curline.fakecontrastlight);
  end;

  if lightnum[0] < 0 then
    lightnum[0] := 0
  else if lightnum[0] >= LIGHTLEVELS then
    lightnum[0] := LIGHTLEVELS - 1;
  if lightnum[1] < 0 then
    lightnum[1] := 0
  else if lightnum[1] >= LIGHTLEVELS then
    lightnum[1] := LIGHTLEVELS - 1;

  if ds.midsiderange.fog[0] then  // JVAL: Mars fog sectors
    walllights := @fog_scalelight[lightnum[0]]
  else
    walllights := @scalelight[lightnum[0]];

  if ds.midsiderange.fog[1] then  // JVAL: Mars fog sectors
    walllights2 := @fog_scalelight[lightnum[1]]
  else
    walllights2 := @scalelight[lightnum[1]];

  dc_llindex := lightnum[0];
  dc_llindex2 := lightnum[1];

  spryscale_dbl := ds.scale_dbl + (x1 - ds.x1) * ds.scalestep_dbl;
  mfloorclip := ds.sprbottomclip;
  mceilingclip := ds.sprtopclip;
  maskedtexturecol := ds.thicksidecol;

  // find positioning
  texturemid[0] := ds.midsiderange.ceilingheight[0] - viewz;
  texturemid[1] := ds.midsiderange.ceilingheight[1] - viewz;

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

  texscale_dbl[0] := texturemid[0] / FRACUNIT * ds.scale_dbl;
  texscale_dbl[1] := texturemid[1] / FRACUNIT * ds.scale_dbl;

  // draw the columns
  roverscale_dbl[0] := (ds.midsiderange.ceilingheight[0] - ds.midsiderange.floorheight[0]) / FRACUNIT;
  roverscale_dbl[1] := (ds.midsiderange.ceilingheight[1] - ds.midsiderange.floorheight[1]) / FRACUNIT;
  roverstep_dbl[0] := roverscale_dbl[0] * ds.scalestep_dbl;
  roverstep_dbl[1] := roverscale_dbl[1] * ds.scalestep_dbl;
  roverscale_dbl[0] := roverscale_dbl[0] * ds.scale_dbl + (x1 - ds.x1) * roverstep_dbl[0];
  roverscale_dbl[1] := roverscale_dbl[1] * ds.scale_dbl + (x1 - ds.x1) * roverstep_dbl[1];
  texstep_dbl[0] := texturemid[0] / FRACUNIT * ds.scalestep_dbl;
  texstep_dbl[1] := texturemid[1] / FRACUNIT * ds.scalestep_dbl;
  texscale_dbl[0] := texscale_dbl[0] + (x1 - ds.x1) * texstep_dbl[0];
  texscale_dbl[1] := texscale_dbl[1] + (x1 - ds.x1) * texstep_dbl[1];
  // Thick side gets row offset from control line
  texturemid[0] := texturemid[0] + midside.rowoffset + midside.midrowoffset;
  texturemid[1] := texturemid[1] + midside.rowoffset + midside.midrowoffset;
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
          if spryscale_dbl > 256 * FRACUNIT then
            index := (_SHR(trunc(spryscale_dbl), HLL_LIGHTSCALESHIFT + 2) div SCREENWIDTH) * 320
          else
            index := (_SHR(trunc(spryscale_dbl), HLL_LIGHTSCALESHIFT + 2) * 320) div SCREENWIDTH;
          if index >= HLL_MAXLIGHTSCALE then
            index := HLL_MAXLIGHTSCALE - 1
          else if index < 0 then
            index := 0;
          dc_lightlevel := scalelightlevels[dc_llindex, index];
          dc_lightlevel2 := scalelightlevels[dc_llindex2, index];
        end;
        index := _SHR(trunc(spryscale_dbl), LIGHTSCALESHIFT) * 320 div SCREENWIDTH;

        if index >=  MAXLIGHTSCALE then
          index := MAXLIGHTSCALE - 1
        else if index < 0 then
          index := 0;

        dc_colormap := walllights[index];
        dc_colormap2 := walllights2[index];
        if videomode = vm32bit then
        begin
          dc_colormap32 := R_GetColormap32(dc_colormap);
          dc_colormap322 := R_GetColormap32(dc_colormap2);
        end;
      end;

      sprtopscreen_dbl := centeryfrac - texscale_dbl[0];

      if (spryscale_dbl < 4) and (spryscale_dbl > -4) then
      begin
        if spryscale_dbl > 0 then
          dc_iscale := MAXINT div 2
        else
          dc_iscale := - MAXINT div 2
      end
      else
      begin
        dc_iscale := trunc($100000000 / spryscale_dbl);
        if dc_iscale > MAXINT div 2 then
          dc_iscale := MAXINT div 2
        else if dc_iscale < -MAXINT div 2 then
          dc_iscale := -MAXINT div 2
      end;
      if (dc_iscale < 4) and (dc_iscale > -4) then
      begin
        if dc_iscale > 0 then
          dc_iscale := 4
        else
          dc_iscale := -4
      end;

      texturecolumn := maskedtexturecol[dc_x] shr DC_HIRESBITS;
      dc_yl := trunc((sprtopscreen_dbl + FRACUNIT) / FRACUNIT);
      dc_yh := trunc((sprtopscreen_dbl + roverscale_dbl[0]) / FRACUNIT);
      if dc_yh >= mfloorclip[dc_x] then
        dc_yh := mfloorclip[dc_x] - 1;
      if dc_yl <= mceilingclip[dc_x] then
        dc_yl := mceilingclip[dc_x] + 1;

      if videomode = vm32bit then
      begin
        dc_mod := 0;
        dc_texturemod := maskedtexturecol[dc_x] and (DC_HIRESFACTOR - 1);
        R_GetDCs(texnum, texturecolumn);
      end
      else
      begin
        dc_source := R_GetColumn(texnum, texturecolumn);
      end;
      dc_texturemid := texturemid[0];

      if dc_yh < 0 then
        dc_yh := 0;
      if dc_yl <= dc_yh then
      begin
        dc_fog := ds.midsiderange.fog[0]; // JVAL: Mars fog sectors
        if depthbufferactive then
          R_DrawColumnWithDepthBufferCheckWrite(wallcolfunc)
        else
          wallcolfunc;

        if domaskedzbuffer then
          R_DrawColumnToZBuffer;
      end;

      if fixedcolormap = nil then
      begin
        dc_lightlevel := dc_lightlevel2;
        dc_colormap := dc_colormap2;
        dc_colormap32 := dc_colormap322;
      end;
      sprtopscreen_dbl := centeryfrac - texscale_dbl[1];
      dc_yl := trunc((sprtopscreen_dbl + FRACUNIT) / FRACUNIT);
      dc_yh := trunc((sprtopscreen_dbl + roverscale_dbl[1]) / FRACUNIT);
      if dc_yh >= mfloorclip[dc_x] then
        dc_yh := mfloorclip[dc_x] - 1;
      if dc_yl <= mceilingclip[dc_x] then
        dc_yl := mceilingclip[dc_x] + 1;

      if dc_yh < 0 then
        dc_yh := 0;
      if dc_yl <= dc_yh then
      begin
        dc_texturemid := texturemid[1];
        dc_fog := ds.midsiderange.fog[1]; // JVAL: Mars fog sectors
        if depthbufferactive then
          R_DrawColumnWithDepthBufferCheckWrite(wallcolfunc)
        else
          wallcolfunc;

        if domaskedzbuffer then
          R_DrawColumnToZBuffer;
      end;

      maskedtexturecol[dc_x] := MAXSHORT;
    end;
    spryscale_dbl := spryscale_dbl + ds.scalestep_dbl;
    roverscale_dbl[0] := roverscale_dbl[0] + roverstep_dbl[0];
    roverscale_dbl[1] := roverscale_dbl[1] + roverstep_dbl[1];
    texscale_dbl[0] := texscale_dbl[0] + texstep_dbl[0];
    texscale_dbl[1] := texscale_dbl[1] + texstep_dbl[1];
  end;
end;

//==============================================================================
//
// R_DoRenderThickSideRange1
//
//==============================================================================
procedure R_DoRenderThickSideRange1(const ds: Pdrawseg_t; const x1, x2: integer);
var
  index: integer;
  lightnum: integer;
  texnum: integer;
  i: integer;
  texturecolumn: integer;
  curline: Pseg_t;
  mid: Psector_t;
  midside: Pside_t;
  roverscale, roverstep: fixed_t;
  texscale, texstep: fixed_t;
begin
  // Calculate light table.
  // Use different light tables
  //   for horizontal / vertical / diagonal. Diagonal?
  // OPTIMIZE: get rid of LIGHTSEGSHIFT globally

  mid := ds.midsec;
  midside := ds.midside; //@sides[mid.midline.sidenum[0]];
  curline := ds.curline;
  texnum := texturetranslation[midside.midtexture];

  R_GetDCs(texnum, 0); // JVAL Also precache external texture if not loaded

  lightnum := _SHR(ds.midsiderange.lightlevel[0], LIGHTSEGSHIFT) + extralight;

  if r_fakecontrast then
    inc(lightnum, curline.fakecontrastlight);

  if lightnum < 0 then
    lightnum := 0
  else if lightnum >= LIGHTLEVELS then
    lightnum := LIGHTLEVELS - 1;

  dc_fog := ds.midsiderange.fog[0]; // JVAL: Mars fog sectors
  if dc_fog then // JVAL: Mars fog sectors
    walllights := @fog_scalelight[lightnum]
  else
    walllights := @scalelight[lightnum];

  dc_llindex := lightnum;

  rw_scalestep := ds.scalestep;
  spryscale := ds.scale1 + (x1 - ds.x1) * rw_scalestep;
  mfloorclip := ds.sprbottomclip;
  mceilingclip := ds.sprtopclip;
  maskedtexturecol := ds.thicksidecol;

  // find positioning
  dc_texturemid := mid.ceilingheight - viewz;

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

  texscale := FixedMul(dc_texturemid, ds.scale1);

  // draw the columns
  roverscale := (mid.ceilingheight - mid.floorheight) div FRACUNIT;
  roverstep := roverscale * ds.scalestep;
  roverscale := roverscale * ds.scale1 + (x1 - ds.x1) * roverstep;
  texstep := FixedMul(dc_texturemid, ds.scalestep);
  texscale := texscale + (x1 - ds.x1) * texstep;
  // Thick side gets row offset from control line
  dc_texturemid := dc_texturemid + midside.rowoffset + midside.midrowoffset;
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
        index := _SHR(spryscale, LIGHTSCALESHIFT) * 320 div SCREENWIDTH;

        if index >=  MAXLIGHTSCALE then
          index := MAXLIGHTSCALE - 1
        else if index < 0 then
          index := 0;

        dc_colormap := walllights[index];
        if videomode = vm32bit then
          dc_colormap32 := R_GetColormap32(dc_colormap);
      end;

      sprtopscreen := centeryfrac - texscale;
      dc_iscale := LongWord($ffffffff) div LongWord(spryscale);

      texturecolumn := maskedtexturecol[dc_x] shr DC_HIRESBITS;
      dc_yl := (sprtopscreen + FRACUNIT) div FRACUNIT;  // JVAL: use  -FRACUNIT to prevent some glitches
      dc_yh := (sprtopscreen + roverscale) div FRACUNIT; // JVAL: use  +FRACUNIT to prevent some glitches
      if dc_yh >= mfloorclip[dc_x] then
        dc_yh := mfloorclip[dc_x] - 1;
      if dc_yl <= mceilingclip[dc_x] then
        dc_yl := mceilingclip[dc_x] + 1;

      if dc_yh < 0 then
        dc_yh := 0;
      if dc_yl <= dc_yh then
      begin
        if videomode = vm32bit then
        begin
          dc_mod := 0;
          dc_texturemod := maskedtexturecol[dc_x] and (DC_HIRESFACTOR - 1);
          R_GetDCs(texnum, texturecolumn);
        end
        else
        begin
          dc_source := R_GetColumn(texnum, texturecolumn);
        end;
        if depthbufferactive then
          R_DrawColumnWithDepthBufferCheckWrite(wallcolfunc)
        else
          wallcolfunc;

        if domaskedzbuffer then
          R_DrawColumnToZBuffer;
      end;

      maskedtexturecol[dc_x] := MAXSHORT;
    end;
    spryscale := spryscale + rw_scalestep;
    roverscale := roverscale + roverstep;
    texscale := texscale + texstep;
  end;
end;

//==============================================================================
//
// R_DoRenderThickSideRange2
//
//==============================================================================
procedure R_DoRenderThickSideRange2(const ds: Pdrawseg_t; const x1, x2: integer);
var
  index: integer;
  lightnum: array[0..1] of integer;
  texnum: integer;
  i: integer;
  texturecolumn: integer;
  curline: Pseg_t;
  mid: Psector_t;
  midside: Pside_t;
  roverscale, roverstep: array[0..1] of fixed_t;
  texscale, texstep: array[0..1] of fixed_t;
  texturemid: array[0..1] of fixed_t;
begin
  // Calculate light table.
  // Use different light tables
  //   for horizontal / vertical / diagonal. Diagonal?
  // OPTIMIZE: get rid of LIGHTSEGSHIFT globally

  mid := ds.midsec;
  midside := ds.midside; //@sides[mid.midline.sidenum[0]];
  curline := ds.curline;
  texnum := texturetranslation[midside.midtexture];

  R_GetDCs(texnum, 0); // JVAL Also precache external texture if not loaded

  lightnum[0] := _SHR(ds.midsiderange.lightlevel[0], LIGHTSEGSHIFT) + extralight;
  lightnum[1] := _SHR(ds.midsiderange.lightlevel[1], LIGHTSEGSHIFT) + extralight;

  if r_fakecontrast then
  begin
    inc(lightnum[0], curline.fakecontrastlight);
    inc(lightnum[1], curline.fakecontrastlight);
  end;

  if lightnum[0] < 0 then
    lightnum[0] := 0
  else if lightnum[0] >= LIGHTLEVELS then
    lightnum[0] := LIGHTLEVELS - 1;
  if lightnum[1] < 0 then
    lightnum[1] := 0
  else if lightnum[1] >= LIGHTLEVELS then
    lightnum[1] := LIGHTLEVELS - 1;

  if ds.midsiderange.fog[0] then  // JVAL: Mars fog sectors
    walllights := @fog_scalelight[lightnum[0]]
  else
    walllights := @scalelight[lightnum[0]];

  if ds.midsiderange.fog[1] then  // JVAL: Mars fog sectors
    walllights2 := @fog_scalelight[lightnum[1]]
  else
    walllights2 := @scalelight[lightnum[1]];

  dc_llindex := lightnum[0];
  dc_llindex2 := lightnum[1];

  rw_scalestep := ds.scalestep;
  spryscale := ds.scale1 + (x1 - ds.x1) * rw_scalestep;
  mfloorclip := ds.sprbottomclip;
  mceilingclip := ds.sprtopclip;
  maskedtexturecol := ds.thicksidecol;

  // find positioning
  texturemid[0] := ds.midsiderange.ceilingheight[0] - viewz;
  texturemid[1] := ds.midsiderange.ceilingheight[1] - viewz;

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

  texscale[0] := FixedMul(texturemid[0], ds.scale1);
  texscale[1] := FixedMul(texturemid[1], ds.scale1);

  // draw the columns
  roverscale[0] := (ds.midsiderange.ceilingheight[0] - ds.midsiderange.floorheight[0]) div FRACUNIT;
  roverscale[1] := (ds.midsiderange.ceilingheight[1] - ds.midsiderange.floorheight[1]) div FRACUNIT;
  roverstep[0] := roverscale[0] * ds.scalestep;
  roverstep[1] := roverscale[1] * ds.scalestep;
  roverscale[0] := roverscale[0] * ds.scale1 + (x1 - ds.x1) * roverstep[0];
  roverscale[1] := roverscale[1] * ds.scale1 + (x1 - ds.x1) * roverstep[1];
  texstep[0] := FixedMul(texturemid[0], ds.scalestep);
  texstep[1] := FixedMul(texturemid[1], ds.scalestep);
  texscale[0] := texscale[0] + (x1 - ds.x1) * texstep[0];
  texscale[1] := texscale[1] + (x1 - ds.x1) * texstep[1];
  // Thick side gets row offset from control line
  texturemid[0] := texturemid[0] + midside.rowoffset + midside.midrowoffset;
  texturemid[1] := texturemid[1] + midside.rowoffset + midside.midrowoffset;
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
          if spryscale > 256 * FRACUNIT then
            index := (_SHR(spryscale, HLL_LIGHTSCALESHIFT + 2) div SCREENWIDTH) * 320
          else
            index := (_SHR(spryscale, HLL_LIGHTSCALESHIFT + 2) * 320) div SCREENWIDTH;
          if index >= HLL_MAXLIGHTSCALE then
            index := HLL_MAXLIGHTSCALE - 1
          else if index < 0 then
            index := 0;
          dc_lightlevel := scalelightlevels[dc_llindex, index];
          dc_lightlevel2 := scalelightlevels[dc_llindex2, index];
        end;
        index := _SHR(spryscale, LIGHTSCALESHIFT) * 320 div SCREENWIDTH;

        if index >=  MAXLIGHTSCALE then
          index := MAXLIGHTSCALE - 1
        else if index < 0 then
          index := 0;

        dc_colormap := walllights[index];
        dc_colormap2 := walllights2[index];
        if videomode = vm32bit then
        begin
          dc_colormap32 := R_GetColormap32(dc_colormap);
          dc_colormap322 := R_GetColormap32(dc_colormap2);
        end;
      end;

      sprtopscreen := centeryfrac - texscale[0];
      dc_iscale := LongWord($ffffffff) div LongWord(spryscale);

      texturecolumn := maskedtexturecol[dc_x] shr DC_HIRESBITS;
      dc_yl := (sprtopscreen + FRACUNIT) div FRACUNIT;  // JVAL: use  -FRACUNIT to prevent some glitches
      dc_yh := (sprtopscreen + roverscale[0]) div FRACUNIT;  // JVAL: use  +FRACUNIT to prevent some glitches
      if dc_yh >= mfloorclip[dc_x] then
        dc_yh := mfloorclip[dc_x] - 1;
      if dc_yl <= mceilingclip[dc_x] then
        dc_yl := mceilingclip[dc_x] + 1;

      if videomode = vm32bit then
      begin
        dc_mod := 0;
        dc_texturemod := maskedtexturecol[dc_x] and (DC_HIRESFACTOR - 1);
        R_GetDCs(texnum, texturecolumn);
      end
      else
      begin
        dc_source := R_GetColumn(texnum, texturecolumn);
      end;
      dc_texturemid := texturemid[0];

      if dc_yh < 0 then
        dc_yh := 0;
      if dc_yl <= dc_yh then
      begin
        dc_fog := ds.midsiderange.fog[0]; // JVAL: Mars fog sectors
        if depthbufferactive then
          R_DrawColumnWithDepthBufferCheckWrite(wallcolfunc)
        else
          wallcolfunc;

        if domaskedzbuffer then
          R_DrawColumnToZBuffer;
      end;

      sprtopscreen := centeryfrac - texscale[1];

      if fixedcolormap = nil then
      begin
        dc_lightlevel := dc_lightlevel2;
        dc_colormap := dc_colormap2;
        dc_colormap32 := dc_colormap322;
      end;
      dc_yl := (sprtopscreen + FRACUNIT) div FRACUNIT;  // JVAL: use  -FRACUNIT to prevent some glitches
      dc_yh := (sprtopscreen + roverscale[1]) div FRACUNIT;  // JVAL: use  +FRACUNIT to prevent some glitches
      if dc_yh >= mfloorclip[dc_x] then
        dc_yh := mfloorclip[dc_x] - 1;
      if dc_yl <= mceilingclip[dc_x] then
        dc_yl := mceilingclip[dc_x] + 1;

      if dc_yh < 0 then
        dc_yh := 0;
      if dc_yl <= dc_yh then
      begin
        dc_texturemid := texturemid[1];
        dc_fog := ds.midsiderange.fog[1]; // JVAL: Mars fog sectors
        if depthbufferactive then
          R_DrawColumnWithDepthBufferCheckWrite(wallcolfunc)
        else
          wallcolfunc;

        if domaskedzbuffer then
          R_DrawColumnToZBuffer;
      end;

      maskedtexturecol[dc_x] := MAXSHORT;
    end;
    spryscale := spryscale + rw_scalestep;
    roverscale[0] := roverscale[0] + roverstep[0];
    roverscale[1] := roverscale[1] + roverstep[1];
    texscale[0] := texscale[0] + texstep[0];
    texscale[1] := texscale[1] + texstep[1];
  end;
end;

//==============================================================================
//
// R_RenderThickSideRange
//
//==============================================================================
procedure R_RenderThickSideRange(const ds: Pdrawseg_t; const x1, x2: integer);
begin
  if ds.midsec = nil then
    exit;

  if ds.midsiderange.count = 1 then
  begin
    if ds.use_double then
      R_DoRenderThickSideRange1_DBL(ds, x1, x2)
    else
      R_DoRenderThickSideRange1(ds, x1, x2)
  end
  else if ds.midsiderange.count = 2 then
  begin
    if ds.use_double then
      R_DoRenderThickSideRange2_DBL(ds, x1, x2)
    else
      R_DoRenderThickSideRange2(ds, x1, x2);
  end;
end;

type
  ffpoint_t = record
    x, y: integer;
  end;
  Pffpoint_t = ^ffpoint_t;
  ffpoint_tArray = array[0..MAXSUBSECTORPOINTS - 1] of ffpoint_t;
  ffPpoint_tArray = ^ffpoint_tArray;

var
  ffpoints: ffpoint_tArray;

//==============================================================================
//
// R_ClearVisPlanes3d
//
//==============================================================================
procedure R_ClearVisPlanes3d;
var
  i: integer;
begin
  for i := 0 to maxvisplane3d do
  begin
    Z_Free(visplanes3d[i].vis.top);
    Z_Free(visplanes3d[i].vis.bottom);
    Z_Free(visplanes3d[i].vis);
    Z_Free(visplanes3d[i].realtop);
    Z_Free(visplanes3d[i].realbottom);
  end;
  maxvisplane3d := -1;
end;

//==============================================================================
//
// R_NewVisPlane3d
//
//==============================================================================
function R_NewVisPlane3d: Pvisplane3d_t;
begin
  if lastvisplane3d = MAXVISPLANES3D then // JVAL: Do not overflow and crash
  begin
    result := nil;
    exit;
  end;

  if lastvisplane3d > maxvisplane3d then
  begin
    visplanes3d[lastvisplane3d].vis :=
      Z_Malloc(SizeOf(visplane_t), PU_LEVEL, nil);
    visplanes3d[lastvisplane3d].vis.top := Pvisindex_tArray(
      Z_Malloc((SCREENWIDTH + 2) * SizeOf(visindex_t), PU_LEVEL, nil));
    visplanes3d[lastvisplane3d].vis.bottom := Pvisindex_tArray(
      Z_Malloc((SCREENWIDTH + 2) * SizeOf(visindex_t), PU_LEVEL, nil));
    visplanes3d[lastvisplane3d].realtop :=
      Z_Malloc((SCREENWIDTH) * SizeOf(smallint), PU_LEVEL, nil);
    visplanes3d[lastvisplane3d].realbottom :=
      Z_Malloc((SCREENWIDTH) * SizeOf(smallint), PU_LEVEL, nil);

    maxvisplane3d := lastvisplane3d;
  end;

  result := @visplanes3d[lastvisplane3d];
  inc(lastvisplane3d);
end;

//==============================================================================
//
// R_3dVisplaneFromSubsector
//
//==============================================================================
procedure R_3dVisplaneFromSubsector(const ssector: Psubsector_t; const floorlightlevel: Psmallint);
var
  i, j: integer;
  seg: Pseg_t;
  h: fixed_t;
  p, p1, p2: Pffpoint_t;
  numffpoints: integer;
  plane: Pvisplane3d_t;
  vis: Pvisplane_t;
  mid: Psector_t;
  x1, x2, y: integer;
  ypos: int64;
  yfrac: int64;
  virtualfloor: boolean; // floor or ceiling ?
  dodraw: Boolean;
  top, bottom: integer;
  pic: integer;
  subjI: TPaths;
  clipI: TPaths;
  solutionI: TPaths;
  cnt: integer;
  ret: boolean;
  fovhangle: angle_t;
  x2stop: integer;
begin
  curmidvis := nil;
  if ssector.sector.midsec < 0 then
    exit;

  mid := @sectors[ssector.sector.midsec];

  // JVAL: Control sector transfer light to lower floor
  floorlightlevel^ := mid.lightlevel;

  dodraw := true;
  virtualfloor := true;
  if mid.floorheight > viewz then
  begin
    h := mid.floorheight;
    pic := mid.floorpic;
    virtualfloor := false;
  end
  else if mid.ceilingheight < viewz then
  begin
    h := mid.ceilingheight;
    pic := mid.ceilingpic;
  end
  else
  begin
    h := mid.ceilingheight;
    pic := 0;
    dodraw := false;
  end;

  // JVAL: Get Points from segs
  // SPEEDUP -> TO BE MOVED IN P_SETUP, ALONG WITH TClipper contruction ?
  SetLength(subjI, 1);
  SetLength(subjI[0], ssector.numlines);
  seg := @segs[ssector.firstline];
  for j := 0 to ssector.numlines - 1 do
  begin
    subjI[0][j] := R_MakeClipperPoint(seg.v1);
    Inc(seg);
  end;

  fovhangle := fov * ANGLETOFINEUNIT div 2;
  // MOVE THIS TO R_RenderPlayerView ?
  SetLength(clipI, 1);
  SetLength(clipI[0], 4);
  clipI[0][0] := R_MakeClipperPoint(
    viewx + trunc(cos((viewangle + fovhangle + ANG1) * ANGLE_T_TO_RAD) * MINZ),
    viewy + trunc(sin((viewangle + fovhangle + ANG1) * ANGLE_T_TO_RAD) * MINZ));
  clipI[0][1] := R_MakeClipperPoint(
    viewx + trunc(cos((viewangle - fovhangle - ANG1) * ANGLE_T_TO_RAD) * MINZ),
    viewy + trunc(sin((viewangle - fovhangle - ANG1) * ANGLE_T_TO_RAD) * MINZ));
  clipI[0][2] := R_MakeClipperPoint(
    viewx + trunc(cos((viewangle - fovhangle) * ANGLE_T_TO_RAD) * MAXZ),
    viewy + trunc(sin((viewangle - fovhangle) * ANGLE_T_TO_RAD) * MAXZ));
  clipI[0][3] := R_MakeClipperPoint(
    viewx + trunc(cos((viewangle + fovhangle) * ANGLE_T_TO_RAD) * MAXZ),
    viewy + trunc(sin((viewangle + fovhangle) * ANGLE_T_TO_RAD) * MAXZ));

  with TClipper.Create do
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

  if length(solutionI) = 0 then
    exit;

  // JVAL: Rebuild point list
  numffpoints := 0;
  if ret then
  begin
    cnt := Length(solutionI[0]);
    if cnt >= MAXSUBSECTORPOINTS then
      cnt := MAXSUBSECTORPOINTS - 1;  // JVAL: Do not overflow and crash
    for j := 0 to cnt - 1 do
    begin
      R_PointToScreenBufferEx(
        solutionI[0][j].x * POINTUNIT + viewx,
        solutionI[0][j].y * POINTUNIT + viewy,
        h,
        ffpoints[numffpoints].x,
        ffpoints[numffpoints].y
      );
      Inc(numffpoints);
    end;
    ffpoints[numffpoints] := ffpoints[0];
    Inc(numffpoints);
  end;

  if not ret then
  begin
    SetLength(solutionI[0], 0);
    SetLength(solutionI, 0);
    exit;
  end;

  plane := R_NewVisPlane3d;
  if plane = nil then
  begin
    SetLength(solutionI[0], 0);
    SetLength(solutionI, 0);
    exit; // Ouch!
  end;

  vis := plane.vis;
  memset(@vis.top[-1], iVISEND, (2 + SCREENWIDTH) * SizeOf(visindex_t));
  memset(@vis.bottom[-1], 0, (2 + SCREENWIDTH) * SizeOf(visindex_t));
  memsetsi(plane.realtop, 32767, SCREENWIDTH);
  memsetsi(plane.realbottom, -32768, SCREENWIDTH);

  vis.height := h;
  vis.picnum := pic;
  // JVAL: Lower floor gets light from control sector
  if virtualfloor then
  // We are in upper floor, get light level from current sector
    vis.lightlevel := ssector.sector.lightlevel
  else
  // We are in lower floor, get light level from control sector
    vis.lightlevel := mid.lightlevel;
  vis.minx := SCREENWIDTH;
  vis.maxx := -1;
  {$IFDEF DOOM_OR_STRIFE}
  if virtualfloor then
  begin
    vis.xoffs := mid.ceiling_xoffs;
    vis.yoffs := mid.ceiling_yoffs;
  end
  else
  begin
    vis.xoffs := mid.floor_xoffs;
    vis.yoffs := mid.floor_yoffs;
  end;
  {$ENDIF}

  // JVAL: 20201229 - Texture angle
  // If virtualfloor we take the angle from the mid ceiling
  // If not virtualfloor we take the angle from the mid floor
  if virtualfloor then
  begin
    vis.angle := mid.ceilingangle;
    vis.anglex := mid.ceilinganglex;
    vis.angley := mid.ceilingangley;
  end
  else
  begin
    vis.angle := mid.floorangle;
    vis.anglex := mid.flooranglex;
    vis.angley := mid.floorangley;
  end;

  vis.renderflags := mid.renderflags or SRF_FFLOOR;
  if virtualfloor then
  begin
    vis.renderflags := vis.renderflags and not (SRF_RIPPLE_FLOOR or SRF_FOG);
    if ssector.sector.renderflags and SRF_FOG <> 0 then
      vis.renderflags := vis.renderflags or SRF_FOG;  // JVAL: Mars fog sectors
  end
  else
    vis.renderflags := vis.renderflags and not SRF_RIPPLE_CEILING;

  if not dodraw then
    vis.renderflags := vis.renderflags or SRF_DONOTDRAW;

  for i := 0 to numffpoints - 2 do
  begin
    p1 := @ffpoints[i];
    p2 := @ffpoints[i + 1];
    if p1.x > p2.x then
    begin
      p := p1;
      p1 := p2;
      p2 := p;
    end;

    x1 := p1.x;
    if x1 < 0 then
      x1 := 0
    else if x1 >= viewwidth then
      x1 := viewwidth - 1;

    x2 := p2.x;
    if x2 < 0 then
    begin
      x2 := 0;
      x2stop := 0;
    end
    else if x2 >= viewwidth then
    begin
      x2 := viewwidth - 1;
      x2stop := x2
    end
    else
      x2stop := x2 + 1;

    if x1 < vis.minx then
      vis.minx := x1;
    if x1 > vis.maxx then
      vis.maxx := x1;
    if x2 < vis.minx then
      vis.minx := x2;
    if x2 > vis.maxx then
      vis.maxx := x2;

    if p1.x = p2.x then
      yfrac := 0
    else
      yfrac := FixedDivEx(p1.y - p2.y, p1.x - p2.x);

    ypos := int64(p1.y) * FRACUNIT + yfrac * (x1 - p1.x);
    repeat
      y := GetInt64InRange(ypos div FRACUNIT, -32000, 32000);
      if y < plane.realtop[x1] then
        plane.realtop[x1] := y;
      if y > plane.realbottom[x1] then
        plane.realbottom[x1] := y;

      if y < 0 then
        y := 0
      else if y >= viewheight then
        y := viewheight - 1;

      if y < vis.top[x1] then
        vis.top[x1] := y;
      if y > vis.bottom[x1] then
        vis.bottom[x1] := y;

      ypos := ypos + yfrac;
      Inc(x1);
    until x1 >= x2stop;
  end;

  // JVAL: Get clipping info of the other surface (floor/ceiling)
  if virtualfloor then
    h := mid.floorheight
  else
    h := mid.ceilingheight;

  numffpoints := 0;
  if ret then
  begin
    cnt := Length(solutionI[0]);
    if cnt >= MAXSUBSECTORPOINTS then
      cnt := MAXSUBSECTORPOINTS - 1;  // JVAL: Do not overflow and crash
    for j := 0 to cnt - 1 do
    begin
      R_PointToScreenBufferEx(
        solutionI[0][j].x * POINTUNIT + viewx,
        solutionI[0][j].y * POINTUNIT + viewy,
        h,
        ffpoints[numffpoints].x,
        ffpoints[numffpoints].y
      );
      Inc(numffpoints);
    end;
    ffpoints[numffpoints] := ffpoints[0];
    Inc(numffpoints);
  end;

  for i := 0 to numffpoints - 2 do
  begin
    p1 := @ffpoints[i];
    p2 := @ffpoints[i + 1];
    if p1.x > p2.x then
    begin
      p := p1;
      p1 := p2;
      p2 := p;
    end;

    x1 := p1.x;
    if x1 < 0 then
      x1 := 0
    else if x1 >= viewwidth then
      x1 := viewwidth - 1;

    x2 := p2.x;
    if x2 < 0 then
    begin
      x2stop := 0;
    end
    else if x2 >= viewwidth then
    begin
      x2 := viewwidth - 1;
      x2stop := x2
    end
    else
      x2stop := x2 + 1;

    if p1.x = p2.x then
      yfrac := 0
    else
      yfrac := FixedDivEx(p1.y - p2.y, p1.x - p2.x);

    ypos := int64(p1.y) * FRACUNIT + yfrac * (x1 - p1.x);
    repeat
      y := GetInt64InRange(ypos div FRACUNIT, -32000, 32000);
      if y < plane.realtop[x1] then
        plane.realtop[x1] := y;
      if y > plane.realbottom[x1] then
        plane.realbottom[x1] := y;

      ypos := ypos + yfrac;
      Inc(x1);
    until x1 >= x2stop;
  end;

  SetLength(solutionI[0], 0);
  SetLength(solutionI, 0);

  // Adjust plane to floorclip & ceilingclip
  for x1 := vis.minx to vis.maxx do
  begin
    if vis.top[x1] < vis.bottom[x1] then
    begin
      if vis.top[x1] > 0 then
        Dec(vis.top[x1]);
      if vis.bottom[x1] < viewheight - 1 then
        Inc(vis.bottom[x1]);
    end;

    top := ceilingclip[x1] + 1;
    bottom := floorclip[x1] - 1;

    if vis.top[x1] >= bottom then
    begin
      vis.top[x1] := 1;
      vis.bottom[x1] := 0;
    end
    else if vis.bottom[x1] <= top then
    begin
      vis.top[x1] := 1;
      vis.bottom[x1] := 0;
    end
    else
    begin
      if vis.bottom[x1] > bottom then
      begin
        vis.bottom[x1] := bottom;
      end;
      if vis.top[x1] < top then
      begin
        vis.top[x1] := top;
      end;
    end;
    if vis.top[x1] <= ceilingclip[x1] + 1 then
      if vis.bottom[x1] > ceilingclip[x1] then
        ceilingclip[x1] := vis.bottom[x1];
    if vis.bottom[x1] >= floorclip[x1] - 1 then
      if vis.top[x1] < floorclip[x1] then
        floorclip[x1] := vis.top[x1];
  end;

  plane.ssector := ssector; // JVAL: Mars fog sectors
  curmidvis := plane;
end;

//==============================================================================
//
// R_DrawFFloors
//
//==============================================================================
procedure R_DrawFFloors;  // JVAL: 3d Floors
var
  i: integer;
  pl: Pvisplane_t;
begin
  if lastvisplane3d = 0 then
    exit;

  R_StartDepthBuffer;
  for i := lastvisplane3d - 1 downto 0 do
  begin
    pl := visplanes3d[i].vis;
    if pl.renderflags and SRF_DONOTDRAW = 0 then
      R_DoDrawPlane(pl);
  end;
end;

//==============================================================================
//
// R_DrawFFloorsMultiThread
//
//==============================================================================
procedure R_DrawFFloorsMultiThread;  // JVAL: 3d Floors
var
  i: integer;
  pl: Pvisplane_t;
begin
  if lastvisplane3d = 0 then
    exit;

  R_StartDepthBuffer;
  for i := lastvisplane3d - 1 downto 0 do
  begin
    pl := visplanes3d[i].vis;
    if pl.renderflags and SRF_DONOTDRAW = 0 then
      R_DoDrawPlane(pl);
  end;
  R_FlashSpansToDepthBufferMT;
end;

end.

