// Quake To Delphi Total Convertion
// Initial convertion: November, 7, 2005 - Valavanis Jim
// gl_rlight.c


(*
Copyright (C) 1996-1997 Id Software, Inc.

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

*)

{$INCLUDE GlQuakeD.inc}

unit gl_rlight;

// r_light.c

interface

uses
  q_delphi,
  quakedef,
  client,
  gl_planes,
  mathlib_h;

procedure R_AnimateLight;
procedure AddLightBlend(r: float; g: float; b: float; a2: float);
procedure R_RenderDlight(light: Pdlight_t);
procedure R_RenderDlights;
procedure R_MarkLights(light: Pdlight_t; bit: integer; node: Pmnode_t);
procedure R_PushDlights;
function RecursiveLightPoint(node: Pmnode_t; start, _end: Pvec3_t): integer;
function R_LightPoint(p: Pvec3_t): integer;

var
  lightspot: vec3_t;

implementation

uses
  q_types,
  OpenGL12,
  cl_main,
  cl_main_h,
  gl_rmain_h,
  view,
  mathlib,
  gl_rmain,
  gl_model_h,
  bspfile;

var
  r_dlightframecount: integer;


(*
==================
R_AnimateLight
==================
*)
procedure R_AnimateLight;
var
  i, j, k: integer;
begin
//
// light animations
// 'm' is normal light, 'a' is no light, 'z' is double bright
  i := int(cl.time * 10);
  for j := 0 to MAX_LIGHTSTYLES - 1 do
  begin
    if cl_lightstyle[j].length = 0 then
    begin
      d_lightstylevalue[j] := 256;
      continue;
    end;
    k := i mod cl_lightstyle[j].length;
    k := Ord(cl_lightstyle[j].map[k]) - Ord('a');
    k := k * 22;
    d_lightstylevalue[j] := k;
  end;
end;

(*
=============================================================================

DYNAMIC LIGHTS BLEND RENDERING

=============================================================================
*)

procedure AddLightBlend(r: float; g: float; b: float; a2: float);
var
  a: float;
begin
  a := v_blend[3] + a2 * (1 - v_blend[3]);
  v_blend[3] := a;

  a2 := a2 / a;

  v_blend[0] := v_blend[1] * (1 - a2) + r * a2;
  v_blend[1] := v_blend[1] * (1 - a2) + g * a2;
  v_blend[2] := v_blend[2] * (1 - a2) + b * a2;
end;

procedure R_RenderDlight(light: Pdlight_t);
var
  i, j: integer;
  a: float;
  v: vec3_t;
  rad: float;
begin
  rad := light.radius * 0.35;

  VectorSubtract(@light.origin, @r_origin, @v);
  if VectorLength(@v) < rad then
  begin  // view is inside the dlight
    AddLightBlend(1, 0.5, 0, light.radius * 0.0003);
    exit;
  end;

  glBegin(GL_TRIANGLE_FAN);
  glColor3f(0.2, 0.1, 0.0);
  for i := 0 to 2 do
    v[i] := light.origin[i] - vpn[i] * rad;
  glVertex3fv(@v[0]);
  glColor3f(0, 0, 0);
  for i := 16 downto 0 do // VJ check loop
  begin
    a := i / 16.0 * M_PI * 2; // VJ: mayby sin, cos from table ???
    for j := 0 to 2 do
      v[j] := light.origin[j] + vright[j] * cos(a) * rad +
              vup[j] * sin(a) * rad;
    glVertex3fv(@v[0]);
  end;
  glEnd;
end;

(*
=============
R_RenderDlights
=============
*)
procedure R_RenderDlights;
var
  i: integer;
  l: Pdlight_t;
begin
  if gl_flashblend.value = 0 then
    exit;

  r_dlightframecount := r_framecount + 1; // because the count hasn't
                                          //  advanced yet for this frame
  glDepthMask(false);
  glDisable(GL_TEXTURE_2D);
  glShadeModel(GL_SMOOTH);
  glEnable(GL_BLEND);
  glBlendFunc(GL_ONE, GL_ONE);

  l := @cl_dlights[0];
  for i := 0 to MAX_DLIGHTS - 1 do
  begin
    if (l.die >= cl.time) and (l.radius <> 0) then 
      R_RenderDlight(l);
    inc(l);
  end;

  glColor3f(1, 1, 1);
  glDisable(GL_BLEND);
  glEnable(GL_TEXTURE_2D);
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
  glDepthMask(true);
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
procedure R_MarkLights(light: Pdlight_t; bit: integer; node: Pmnode_t);
var
  splitplane: Pmplane_t;
  dist: float;
  surf: Pmsurface_t;
  i: integer;
begin
  if node.contents < 0 then
    exit;

  splitplane := node.plane;
  dist := DotProduct(@light.origin, @splitplane.normal) - splitplane.dist;

  if dist > light.radius then
  begin
    R_MarkLights(light, bit, node.children[0]);
    exit;
  end;
  if dist < -light.radius then
  begin
    R_MarkLights(light, bit, node.children[1]);
    exit;
  end;

// mark the polygons
  surf := Pmsurface_t(integer(cl.worldmodel.surfaces) + node.firstsurface * SizeOf(surf^)); // VJ check this
  for i := 0 to node.numsurfaces - 1 do
  begin
    if surf.dlightframe <> r_dlightframecount then
    begin
      surf.dlightbits := 0;
      surf.dlightframe := r_dlightframecount;
    end;
    surf.dlightbits := surf.dlightbits or bit;
    inc(surf);
  end;

  R_MarkLights(light, bit, node.children[0]);
  R_MarkLights(light, bit, node.children[1]);
end;


(*
=============
R_PushDlights
=============
*)
procedure R_PushDlights;
var
  i: integer;
  l: Pdlight_t;
begin
  if gl_flashblend.value <> 0 then
    exit;

  r_dlightframecount := r_framecount + 1; // because the count hasn't
                                          //  advanced yet for this frame
  l := @cl_dlights[0];

  for i := 0 to MAX_DLIGHTS - 1 do
  begin
    if (l.die >= cl.time) and (l.radius <> 0) then 
      R_MarkLights(l, _SHL(1, i), cl.worldmodel.nodes);
    inc(l);
  end;
end;


(*
=============================================================================

LIGHT SAMPLING

=============================================================================
*)

var
  lightplane: Pmplane_t;

function RecursiveLightPoint(node: Pmnode_t; start, _end: Pvec3_t): integer;
label
  continue1;
var
  r: integer;
  front, back, frac: float;
  side: qboolean;
  plane: Pmplane_t;
  mid: vec3_t;
  surf: Pmsurface_t;
  s, t, ds, dt: integer;
  i: integer;
  tex: Pmtexinfo_t;
  lightmap: PByte;
  scale: integer; // unsigned;
  maps: integer;
begin
  if node.contents < 0 then
  begin
    result := -1;    // didn't hit anything
    exit;
  end;

// calculate mid point

// FIXME: optimize for axial
  plane := node.plane;
  front := DotProduct(start, @plane.normal) - plane.dist;
  back := DotProduct(_end, @plane.normal) - plane.dist;
  side := front < 0;

  if (back < 0) = side then
  begin
    result := RecursiveLightPoint(node.children[intval(side)], start, _end);
    exit;
  end;

  frac := front / (front - back);
  mid[0] := start[0] + (_end[0] - start[0]) * frac;  // VJ maybe change to loop
  mid[1] := start[1] + (_end[1] - start[1]) * frac;
  mid[2] := start[2] + (_end[2] - start[2]) * frac;

// go down front side
  r := RecursiveLightPoint(node.children[intval(side)], start, @mid);
  if r >= 0 then
  begin
    result := r;    // hit something
    exit;
  end;

  if (back < 0) = side then
  begin
    result := -1;    // didn't hit anuthing
    exit;
  end;

// check for impact on this node
  VectorCopy(@mid, @lightspot);
  lightplane := plane;

  surf := Pmsurface_t(integer(cl.worldmodel.surfaces) + node.firstsurface * SizeOf(surf^)); // VJ check this!!
  for i := 0 to node.numsurfaces - 1 do
  begin
    if surf.flags and SURF_DRAWTILED <> 0 then
      goto continue1;  // no lightmaps

    tex := surf.texinfo;

    s := int(DotProduct(@mid, @tex.vecs[0]) + tex.vecs[0][3]);
    t := int(DotProduct(@mid, @tex.vecs[1]) + tex.vecs[1][3]);

    if (s < surf.texturemins[0]) or (t < surf.texturemins[1]) then
      goto continue1;

    ds := s - surf.texturemins[0];
    dt := t - surf.texturemins[1];

    if (ds > surf.extents[0]) or (dt > surf.extents[1]) then
      goto continue1;

    if surf.samples = nil then
    begin
      result := 0;
      exit;
    end;

    ds := _SHR(ds, 4);
    dt := _SHR(dt, 4);

    lightmap := @surf.samples[0];
    r := 0;
    if lightmap <> nil then
    begin

//      lightmap += dt * ((surf->extents[0]>>4)+1) + ds; // VJ SOS
      lightmap := @PByteArray(lightmap)[dt * (surf.extents[0] div 16 + 1) + ds]; // VJ SOS

      maps := 0;
      while (maps < MAXLIGHTMAPS) and (surf.styles[maps] <> 255) do
      begin
        scale := d_lightstylevalue[surf.styles[maps]];
        r := r + lightmap^ * scale;
        lightmap := @PByteArray(lightmap)[((surf.extents[0] div 16) + 1) *
                                          ((surf.extents[1] div 16) + 1)];
        inc(maps);
      end;

      r := r div 256
    end;

    result := r;
    exit;

continue1:
    inc(surf);
  end;

// go down back side
  result := RecursiveLightPoint(node.children[intval(not side)], @mid, _end);
end;

function R_LightPoint(p: Pvec3_t): integer;
var
  _end: vec3_t;
begin
  if cl.worldmodel.lightdata = nil then
  begin
    result := 255;
    exit;
  end;

  _end[0] := p[0];
  _end[1] := p[1];
  _end[2] := p[2] - 2048;

  result := RecursiveLightPoint(cl.worldmodel.nodes, p, @_end);

  if result = -1 then
    result := 0;
end;


end.
 