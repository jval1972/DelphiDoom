{----------------------------------------------------------------------------}
{                                                                            }
{ File(s): gl_light.c                                                        }
{                                                                            }
{ Initial conversion by : Lars Middendorf (lmid@gmx.de)                      }
{ Initial conversion on : 24-Jan-2002                                        }
{                                                                            }
{ This File contains part of convertion of Quake2 source to ObjectPascal.    }
{ More information about this project can be found at:                       }
{ http://www.sulaco.co.za/quake2/                                            }
{                                                                            }
{ Copyright (C) 1997-2001 Id Software, Inc.                                  }
{                                                                            }
{ This program is free software; you can redistribute it and/or              }
{ modify it under the terms of the GNU General Public License                }
{ as published by the Free Software Foundation; either version 2             }
{ of the License, or (at your option) any later version.                     }
{                                                                            }
{ This program is distributed in the hope that it will be useful,            }
{ but WITHOUT ANY WARRANTY; without even the implied warranty of             }
{ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                       }
{                                                                            }
{ See the GNU General Public License for more details.                       }
{                                                                            }
{----------------------------------------------------------------------------}
{ Updated on : 27/07/02                                                      }
{ Updated by : Fabrizio Rossini rossini.f@libero.it                          }
{ Updated on :08/08/02                                                       }
{ Updated by : Fabrizio Rossini rossini.f@libero.it                          }
{            Changed some type definitions to var and added some typecasting }
{      added definition of pSingleArray and corrected some pointer increment }
{----------------------------------------------------------------------------}
{ * Still dependent (to compile correctly) on:                               }
{                                                                            }
{----------------------------------------------------------------------------}
{ 28.06.2003 Juha: Proofreaded }
unit gl_light;

interface

uses
  DelphiTypes,
  SysUtils,
  OpenGL,
  q_shared,
  CPas,
  ref,
  qgl_h,
  gl_model_h,
  gl_local;

var
  r_dlightframecount: integer;
  s_blocklights: array[0..(34 * 34 * 3) - 1] of single;
  pointcolor: vec3_t;
  lightspot: vec3_t;
  lightplane: cplane_p; // used as shadow plane

const
  DLIGHT_CUTOFF = 64;

{
=============================================================================

DYNAMIC LIGHTS BLEND RENDERING

=============================================================================
}


procedure R_RenderDlight(light: dlight_p);
procedure R_RenderDlights;
procedure R_MarkLights(light: dlight_p; bit: integer; node: mnode_p);
procedure R_PushDlights;
function RecursiveLightPoint(node: mnode_p; const start, _end: vec3_t): integer;
procedure R_LightPoint(const p: vec3_t; var color: vec3_t);
procedure R_AddDynamicLights(surf: msurface_p);
procedure R_SetCacheState(surf: msurface_p);
procedure R_BuildLightMap(surf: msurface_p; dest: PByteArray; stride: integer);


implementation

uses
  QFiles,
  qgl_win,
  gl_rmain;

procedure R_RenderDlight(light: dlight_p);
var
  i, j: integer;
  a: single;
  v: vec3_t;
  rad: single;
begin
  rad := light^.intensity * 0.35;
  VectorSubtract(light^.origin, r_origin, v);

(*
 // FIXME?
        if VectorLength(v) < rad then
        begin
  // view is inside the dlight
  V_AddBlend(light.color[0],light.color[1],light.color[2],light.intensity * 0.0003, v_blend);
  exit;
        end;
*)
  qglBegin(GL_TRIANGLE_FAN);
  qglColor3f(light^.color[0] * 0.2, light^.color[1] * 0.2, light^.color[2] * 0.2);
  for i := 0 to 2 do
    v[i] := light^.origin[i] - vpn[i] * rad;
  qglVertex3fv(@v);
  qglColor3f(0, 0, 0);
  for i := 16 downto 0 do
  begin
    a := i / 16 * M_PI * 2;
    for j := 0 to 2 do
      v[j] := light^.origin[j] + vright[j] * cos(a) * rad
        + vup[j] * sin(a) * rad;
    qglVertex3fv(@v);
  end;
  qglEnd;

end;

procedure R_RenderDlights;
var
  i: Integer;
  l: dlight_p;
begin
  if (gl_flashblend^.value = 0) then
    exit;
  r_dlightframecount := r_framecount + 1; // because the count hasn't
                                          // advanced yet for this frame
  qglDepthMask(False);
  qglDisable(GL_TEXTURE_2D);
  qglShadeModel(GL_SMOOTH);
  qglEnable(GL_BLEND);
  qglBlendFunc(GL_ONE, GL_ONE);

  l := r_newrefdef.dlights;
  for i := 0 to (r_newrefdef.num_dlights - 1) do
  begin
    R_RenderDlight(l);
    inc(l);
  end;
  qglColor3f(1, 1, 1);
  qglDisable(GL_BLEND);
  qglEnable(GL_TEXTURE_2D);
  qglBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  qglDepthMask(True);
end;

(*
=============================================================================

DYNAMIC LIGHTS

=============================================================================
*)


(*
=============
R_MarkLights
=============
*)
procedure R_MarkLights(light: dlight_p; bit: integer; node: mnode_p);
var
  splitplane: cplane_p;
  dist: single;
  surf: msurface_p;
  i: integer;
begin
  if (node^.contents <> -1) then
    exit;
  splitplane := node^.plane;
  dist := DotProduct(light^.origin, splitplane^.normal) - splitplane^.dist;

  if (dist > light^.intensity - DLIGHT_CUTOFF) then
  begin
    R_MarkLights(light, bit, node^.children[0]);
    exit;
  end;
  if (dist < -light^.intensity + DLIGHT_CUTOFF) then
  begin
    R_MarkLights(light, bit, node^.children[1]);
    exit;
  end;
// mark the polygons
  surf := Pointer(Cardinal(r_worldmodel.surfaces) + node^.firstsurface * sizeof(mSurface_t));
  for i := 0 to node^.numsurfaces - 1 do
  begin
    if surf^.dlightframe <> r_dlightframecount then
    begin
      surf^.dlightbits := 0;
      surf^.dlightframe := r_dlightframecount;
    end;
    surf^.dlightbits := surf^.dlightbits or bit;
    inc(surf);
  end;

  R_MarkLights(light, bit, node^.children[0]);
  R_MarkLights(light, bit, node^.children[1]);
end;


(*
=============
R_PushDlights
=============
*)
procedure R_PushDlights;
var
  i: integer;
  l: dlight_p;
begin
  if (gl_flashblend^.value <> 0) then
    exit;

  r_dlightframecount := r_framecount + 1; // because the count hasn't
                                                //  advanced yet for this frame
  l := r_newrefdef.dlights;

  for i := 0 to r_newrefdef.num_dlights - 1 do
  begin
    R_MarkLights(l, 1 shl i, r_worldmodel^.nodes);
    inc(l);
  end;
end;

(*
=============================================================================

LIGHT SAMPLING

=============================================================================
*)

function RecursiveLightPoint(node: mnode_p; const start, _end: vec3_t): integer;
var
  front, back, frac: single;
  side: integer;
  plane: cplane_p;
  mid, scale: vec3_t;
  surf: msurface_p;
  s, t, ds, dt, i: integer;
  ii: Integer;
  tex: mtexinfo_p;
  lightmap: pByteArray;
  maps, r: integer;
  notside: Integer;
label
  continue_;
begin
  if (node^.contents <> -1) then
  begin
    result := -1; // didn't hit anything
    exit;
  end;
  // calculate mid point

  // FIXME: optimize for axial
  plane := node^.plane;
  front := DotProduct(start, plane^.normal) - plane^.dist;
  back := DotProduct(_end, plane^.normal) - plane^.dist;
  if front < 0 then
    side := 1
  else
    side := 0;


  // Juha: This translation looks a bit wicked, but should do the thing
  // if ( (back < 0) == side)
  if ((back < 0) and (side = 1)) or ((back >= 0) and (side = 0)) then
  begin
    result := RecursiveLightPoint(node^.children[side], start, _end);
    exit;
  end;

  frac := front / (front - back);
  mid[0] := start[0] + (_end[0] - start[0]) * frac;
  mid[1] := start[1] + (_end[1] - start[1]) * frac;
  mid[2] := start[2] + (_end[2] - start[2]) * frac;

// go down front side
  r := RecursiveLightPoint(node^.children[side], start, mid);
  if (r >= 0) then
  begin
    result := r; // hit something
    exit;
  end;

  if ((back < 0) and (side = 1)) or ((back >= 0) and (side = 0)) then
  begin
    result := -1; // didn't hit anything
    exit;
  end;

  // check for impact on this node
  //
  VectorCopy(mid, lightspot);
  lightplane := plane;

  surf := Pointer(Cardinal(r_worldmodel^.surfaces) + node^.firstsurface * sizeof(mSurface_t));
  for ii := 0 to node^.numsurfaces - 1 do
  begin
    if (surf^.flags and (SURF_DRAWTURB or SURF_DRAWSKY)) <> 0 then
      goto continue_; // no lightmaps

    tex := surf^.texinfo;

    s := trunc(DotProduct(mid, vec3_p(@tex^.vecs[0])^) + tex^.vecs[0][3]);
    t := trunc(DotProduct(mid, vec3_p(@tex^.vecs[1])^) + tex^.vecs[1][3]);

    if (s < surf^.texturemins[0]) or (t < surf^.texturemins[1]) then
      goto continue_;

    ds := s - surf^.texturemins[0];
    dt := t - surf^.texturemins[1];

    if (ds > surf^.extents[0]) or (dt > surf^.extents[1]) then
      goto continue_;

    if (surf^.samples = nil) then
    begin
      result := 0;
      exit;
    end;

    ds := ds shr 4;
    dt := dt shr 4;

    lightmap := PByteArray(surf^.samples);
    VectorCopy(vec3_origin, pointcolor);
    if lightmap<>nil then
    begin
      inc(PByte(lightmap), 3 * (dt * ((surf^.extents[0] shr 4) + 1) + ds));

      maps := 0;
      while (maps < MAXLIGHTMAPS) and (surf^.styles[maps] <> 255) do
      begin
        for i := 0 to 2 do
          scale[i] := gl_modulate_^.value * r_newrefdef.lightstyles[surf^.styles[maps]].rgb[i];

        pointcolor[0] := pointcolor[0] + lightmap[0] * scale[0] * (1.0 / 255);
        pointcolor[1] := pointcolor[1] + lightmap[1] * scale[1] * (1.0 / 255);
        pointcolor[2] := pointcolor[2] + lightmap[2] * scale[2] * (1.0 / 255);
        inc(PByte(lightmap), 3 * ((surf^.extents[0] shr 4) + 1) * ((surf.extents[1] shr 4) + 1));
        inc(maps);
      end;
    end;
    result := 1;
    exit;

    continue_:
    inc(surf);
  end;
  if side = 0 then
    notside := 1
  else
    notside := 0;
  result := RecursiveLightPoint(node^.children[notside], mid, _end);
end;


(*
===============
R_LightPoint
===============
*)
procedure R_LightPoint(const p: vec3_t; var color: vec3_t);
var
  _end: vec3_t;
  r: single;
  lnum: integer;
  dl: dlight_p;
  light: single;
  dist: vec3_t;
  add: single;
begin
  if (r_worldmodel^.lightdata = nil) then
  begin
    color[0] := 1;
    color[1] := 1;
    color[2] := 1;
    exit;
  end;

  _end[0] := p[0];
  _end[1] := p[1];
  _end[2] := p[2] - 2048;

  r := RecursiveLightPoint(r_worldmodel^.nodes, p, _end);

  if (r = -1) then
    VectorCopy(vec3_origin, color)
  else
    VectorCopy(pointcolor, color);

 //
 // add dynamic lights
 //
  light := 0;
  dl := r_newrefdef.dlights;
  for lnum := 0 to r_newrefdef.num_dlights - 1 do
  begin
    VectorSubtract(vec3_p(@currententity^.origin)^, dl^.origin, dist);
    add := dl^.intensity - (VectorLength(dist));
    add := add * (1 / 256);
    if (add > 0) then
      VectorMA(color, add, dl^.color, color);

    Inc(dl);
  end;

  VectorScale(color, gl_modulate_^.value, color);
end;


(*
===============
R_AddDynamicLights
===============
*)
procedure R_AddDynamicLights(surf: msurface_p);
var
  lnum,
    sd,
    td: Integer;
  fdist,
    frad,
    fminlight: Single;
  impact,
    local: vec3_t;
  s, t, i, smax,
    tmax: Integer;
  tex: mtexinfo_p;
  dl: dlight_p;
  pfBL: PSingleArray;
  fsacc,
    ftacc: Single;
begin
  smax := (surf^.extents[0] shr 4) + 1;
  tmax := (surf^.extents[1] shr 4) + 1;
  tex := surf^.texinfo;
  for lnum := 0 to r_newrefdef.num_dlights - 1 do
  begin
    if ((surf^.dlightbits and (1 shl lnum)) = 0) then
      continue; // not lit by this light

    dl := @dlight_arrp(r_newrefdef.dlights)^[lnum];
    frad := dl^.intensity;
    fdist := DotProduct(dl^.origin, surf^.plane^.normal) - surf^.plane^.dist;
    frad := frad - fabs(fdist);
     // rad is now the highest intensity on the plane

    fminlight := DLIGHT_CUTOFF; // FIXME: make configurable?

    if (frad < fminlight) then
      continue;
    fminlight := frad - fminlight;

    for i := 0 to 2 do
    begin
      impact[i] := dl^.origin[i] - surf^.plane^.normal[i] * fdist;
    end;

    local[0] := trunc(DotProduct(impact, vec3_p(@tex^.vecs[0])^) + tex^.vecs[0][3] - surf^.texturemins[0]);
    local[1] := trunc(DotProduct(impact, vec3_p(@tex^.vecs[1])^) + tex^.vecs[1][3] - surf^.texturemins[1]);

    pfBL := @s_blocklights;
    ftacc := 0;
    for t := 0 to tmax - 1 do
    begin
      td := trunc(local[1] - ftacc);
      if (td < 0) then
        td := -td;
      fsacc := 0;
      for s := 0 to smax - 1 do
      begin
        sd := Q_ftol(local[0] - fsacc);
        if (sd < 0) then
          sd := -sd;
        if (sd > td) then
          fdist := sd + (td shr 1)
        else
          fdist := td + (sd shr 1);
        if (fdist < fminlight) then
        begin
          pfBL[0] := pfBL[0] + (frad - fdist) * dl^.color[0];
          pfBL[1] := pfBL[1] + (frad - fdist) * dl^.color[1];
          pfBL[2] := pfBL[2] + (frad - fdist) * dl^.color[2];
        end;
        fsacc := fsacc + 16;
        pfBL := Pointer(Cardinal(pfBL) + 3 * SizeOf(Single));
      end;
      ftacc := ftacc + 16;
    end;
  end;
end;


(*
** R_SetCacheState
*)
procedure R_SetCacheState(surf: msurface_p);
var
  maps: integer;
begin
  maps := 0;
  while (maps < MAXLIGHTMAPS) and (surf^.styles[maps] <> 255) do
  begin
    surf^.cached_light[maps] := r_newrefdef.lightstyles[surf^.styles[maps]].white;
    inc(maps);
  end;
end;


(*
===============
R_BuildLightMap

Combine and scale multiple lightmaps into the floating format in blocklights
===============
*)
procedure R_BuildLightMap(surf: msurface_p; dest: PByteArray; stride: integer);
var
  smax, tmax: integer;
  r, g, b, a, max: integer;
  i, j, size: integer;
  lightmap: PByteArray;
  scale: array[0..3] of Single;
  nummaps: integer;
  bl: PSingleArray;
  style: lightstyle_p;
  monolightmap: integer;
  maps: integer;
  t: Single;
label
  Store;
begin
  if (surf^.TexInfo^.Flags and (SURF_SKY or SURF_TRANS33 or SURF_TRANS66 or SURF_WARP)) <> 0 then
    ri.Sys_Error(ERR_DROP, 'R_BuildLightMap called for non-lit surface');

  smax := (surf^.extents[0] shr 4) + 1;
  tmax := (surf^.extents[1] shr 4) + 1;
  size := smax * tmax;
  if (size > (sizeof(s_blocklights) shr 4)) then
    ri.Sys_Error(ERR_DROP, 'Bad s_blocklights size');

 // set to full bright if no light data
  if (surf^.samples = nil) then
  begin
    for i := 0 to (size * 3) - 1 do
      s_blocklights[i] := 255;

    maps := 0;
    while (maps < MAXLIGHTMAPS) and (surf^.styles[maps] <> 255) do
    begin
      style := @r_newrefdef.lightstyles[surf^.styles[maps]];
      Inc(maps);
    end;
    goto store;
  end;

  // count the # of maps
  nummaps := 0;
  while (nummaps < MAXLIGHTMAPS) and (surf^.styles[nummaps] <> 255) do
    Inc(nummaps);

  lightmap := Pointer(surf^.samples);

  // add all the lightmaps
  if (nummaps = 1) then
  begin
    maps := 0;
    while (maps < MAXLIGHTMAPS) and (surf^.styles[maps] <> 255) do
    begin
      bl := @s_blocklights;
      for i := 0 to 2 do
        scale[i] := gl_modulate_^.value * r_newrefdef.lightstyles[surf^.styles[maps]].rgb[i];
      if (scale[0] = 1) and
        (scale[1] = 1) and
        (scale[2] = 1) then
      begin
        for i := 0 to size - 1 do
        begin
          bl[0] := lightmap[i * 3 + 0];
          bl[1] := lightmap[i * 3 + 1];
          bl[2] := lightmap[i * 3 + 2];
          inc(PSingle(bl), 3);
        end;
      end
      else
      begin
        for i := 0 to size - 1 do
        begin
          bl[0] := lightmap[i * 3 + 0] * scale[0];
          bl[1] := lightmap[i * 3 + 1] * scale[1];
          bl[2] := lightmap[i * 3 + 2] * scale[2];
          inc(pSingle(bl), 3);
        end;
      end;
      inc(pByte(lightmap), size * 3); // skip to next lightmap
      Inc(maps);
    end;
  end
  else
  begin
    memset(@s_blocklights, 0, sizeof(s_blocklights[0]) * size * 3);
    maps := 0;
    while (maps < MAXLIGHTMAPS) and (surf^.styles[maps] <> 255) do
    begin
      bl := @s_blocklights;
      for i := 0 to 2 do
        scale[i] := gl_modulate_^.value * r_newrefdef.lightstyles[surf^.styles[maps]].rgb[i];

      if (scale[0] = 1) and
        (scale[1] = 1) and
        (scale[2] = 1) then
      begin
        for i := 0 to size - 1 do
        begin
          bl[0] := lightmap[i * 3 + 0];
          bl[1] := lightmap[i * 3 + 1];
          bl[2] := lightmap[i * 3 + 2];
          inc(pSingle(bl), 3);
        end;
      end
      else
      begin
        for i := 0 to size - 1 do
        begin
          bl[0] := lightmap[i * 3 + 0] * scale[0];
          bl[1] := lightmap[i * 3 + 1] * scale[1];
          bl[2] := lightmap[i * 3 + 2] * scale[2];
          inc(pSingle(bl), 3);
        end;
      end;
      inc(PByte(lightmap), size * 3); // skip to next lightmap
      Inc(maps);
    end;
  end;
  // add all the dynamic lights
  if (surf^.dlightframe = r_framecount) then
    R_AddDynamicLights(surf);

  // put into texture format
  Store: // Label

  stride := stride - (smax shl 2);
  bl := @s_blocklights;

  monolightmap := Byte(gl_monolightmap^.string_[0]);
  if (monolightmap = Byte('0')) then
  begin
    for i := 0 to tmax - 1 do
    begin
      for j := 0 to smax - 1 do
      begin
        r := Q_ftol(bl[0]);
        g := Q_ftol(bl[1]);
        b := Q_ftol(bl[2]);

        // catch negative lights
        if (r < 0) then
          r := 0;
        if (g < 0) then
          g := 0;
        if (b < 0) then
          b := 0;
        (*
        ** determine the brightest of the three color components
        *)
        if (r > g) then
          max := r
        else
          max := g;
        if (b > max) then
          max := b;
        (*
        ** alpha is ONLY used for the mono lightmap case.  For this reason
        ** we set it to the brightest of the color components so that
        ** things don't get too dim.
        *)

        a := max;

        (*
        ** rescale all the color components if the intensity of the greatest
        ** channel exceeds 1.0
        *)
        if (max > 255) then
        begin
          t := 255 / max;

          r := Trunc(r * t);
          g := Trunc(g * t);
          b := Trunc(b * t);
          a := Trunc(a * t);
        end;
        dest[0] := r;
        dest[1] := g;
        dest[2] := b;
        dest[3] := a;

        inc(pSingle(bl), 3);
        inc(PByte(dest), 4); //dest :=dest + 4;
      end;
      inc(PByte(dest), stride); // dest :=dest + stride;
    end
  end
  else
  begin
    for i := 0 to tmax - 1 do
    begin
      for j := 0 to smax - 1 do
      begin
        r := Q_ftol(bl[0]);
        g := Q_ftol(bl[1]);
        b := Q_ftol(bl[2]);

        // catch negative lights
        if (r < 0) then
          r := 0;
        if (g < 0) then
          g := 0;
        if (b < 0) then
          b := 0;
        (*/*
        //** determine the brightest of the three color components
        //*/*)
        if (r > g) then
          max := r
        else
          max := g;
        if (b > max) then
          max := b;
        (*
        ** alpha is ONLY used for the mono lightmap case.  For this reason
        ** we set it to the brightest of the color components so that
        ** things don't get too dim.
        *)

        a := max;

        (*
        ** rescale all the color components if the intensity of the greatest
        ** channel exceeds 1.0
        *)
        if (max > 255) then
        begin
          t := 255 / max;

          r := trunc(r * t);
          g := trunc(g * t);
          b := trunc(b * t);
          a := trunc(a * t);
        end;
        (*
        ** So if we are doing alpha lightmaps we need to set the R, G, and B
        ** components to 0 and we need to set alpha to 1-alpha.
        *)
        case Char(monolightmap) of
          'L',
            'I':
            begin
              r := a;
              g := 0;
              b := 0;
            end;
          'C':
            begin
              // try faking colored lighting
              a := Trunc(255 - ((r + g + b) / 3));
              r := Trunc(r * (a / 255));
              g := Trunc(g * (a / 255));
              b := Trunc(b * (a / 255));
            end;
        else
          //'A':
          begin
            r := 0;
            g := 0;
            b := 0;
            a := 255 - a;
          end;
        end;
        dest[0] := r;
        dest[1] := g;
        dest[2] := b;
        dest[3] := a;

        //bl :=bl + 3;
        inc(PSingle(bl), 3);

        inc(PByte(dest), 4); // dest :=dest + 4;
      end;
    end;
    inc(PByte(dest), stride); // dest := dest + stride ;
  end;
end;

end.
