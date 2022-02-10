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
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit m_vectors;

interface

uses
  d_delphi;

type
  vec_t = float;
  Pvec_t = ^vec_t;

  vec2_t = array[0..1] of vec_t;
  Pvec2_t = ^vec2_t;

  vec3_t = array[0..2] of vec_t;
  Pvec3_t = ^vec3_t;

  mat3_t = array[0..2, 0..2] of float;
  Pmat3_t = ^mat3_t;

  vec5_t = array[0..4] of vec_t;
  Pvec5_t = ^vec5_t;

const
  M_PI = 3.14159265358979323846;  // matches value in gcc v2 math.h

//==============================================================================
//
// IS_NAN
//
//==============================================================================
function IS_NAN(x: float): boolean;

//==============================================================================
//
// DEG2RAD
//
//==============================================================================
function DEG2RAD(a: float): float;

//==============================================================================
//
// ProjectPointOnPlane
//
//==============================================================================
procedure ProjectPointOnPlane(dst: Pvec3_t; p: Pvec3_t; normal: Pvec3_t);

//==============================================================================
//
// PerpendicularVector
//
//==============================================================================
procedure PerpendicularVector(dst: Pvec3_t; src: Pvec3_t);

//==============================================================================
//
// RotatePointAroundVector
//
//==============================================================================
procedure RotatePointAroundVector(dst: Pvec3_t; dir: Pvec3_t;
  point: Pvec3_t; degrees: float);

//==============================================================================
//
// VectorCompare
//
//==============================================================================
function VectorCompare(v1, v2: Pvec3_t): integer;

//==============================================================================
//
// VectorMA
//
//==============================================================================
procedure VectorMA(veca: Pvec3_t; scale: float; vecb: Pvec3_t; vecc: Pvec3_t);

//==============================================================================
//
// DotProduct
//
//==============================================================================
function DotProduct(v1, v2: Pvec3_t): vec_t;

//==============================================================================
//
// VectorSubtract
//
//==============================================================================
procedure VectorSubtract(veca, vecb: Pvec3_t; _out: Pvec3_t);

//==============================================================================
//
// VectorAdd
//
//==============================================================================
procedure VectorAdd(veca, vecb: Pvec3_t; _out: Pvec3_t);

//==============================================================================
//
// VectorCopy
//
//==============================================================================
procedure VectorCopy(_in: Pvec3_t; _out: Pvec3_t);

//==============================================================================
//
// CrossProduct
//
//==============================================================================
procedure CrossProduct(v1, v2: Pvec3_t; cross: Pvec3_t);

//==============================================================================
//
// VectorLength
//
//==============================================================================
function VectorLength(v: Pvec3_t): vec_t;

//==============================================================================
//
// VectorNormalize
//
//==============================================================================
function VectorNormalize(v: Pvec3_t): float;

//==============================================================================
//
// VectorInverse
//
//==============================================================================
procedure VectorInverse(v: Pvec3_t);

//==============================================================================
//
// VectorScale
//
//==============================================================================
procedure VectorScale(_in: Pvec3_t; const scale: vec_t; _out: Pvec3_t);

//==============================================================================
//
// ConcatRotations
//
//==============================================================================
procedure ConcatRotations(in1, in2: Pmat3_t; _out: Pmat3_t);

//==============================================================================
//
// DotProduct2
//
//==============================================================================
function DotProduct2(v1, v2: vec2_t): float;

//==============================================================================
//
// VectorSubtract2
//
//==============================================================================
function VectorSubtract2(veca, vecb: vec2_t): vec2_t;

//==============================================================================
//
// VectorAdd2
//
//==============================================================================
function VectorAdd2(veca, vecb: vec2_t): vec2_t;

//==============================================================================
//
// VectorScale2
//
//==============================================================================
function VectorScale2(veca: vec2_t; scale: float): vec2_t;

//==============================================================================
//
// VectorLength2
//
//==============================================================================
function VectorLength2(v: vec2_t): float;

//==============================================================================
//
// VectorNormalize2
//
//==============================================================================
function VectorNormalize2(var v: vec2_t): float;

//==============================================================================
//
// CalculateReflect2
//
//==============================================================================
procedure CalculateReflect2(const d, wall: vec2_t; var reflect: vec2_t);

implementation

const
  nanmask = 255 shl 23;

//==============================================================================
//
// IS_NAN
//
//==============================================================================
function IS_NAN(x: float): boolean;
begin
  result := (Pinteger(@x)^ and nanmask) = nanmask;
end;

//==============================================================================
//
// DEG2RAD
//
//==============================================================================
function DEG2RAD(a: float): float;
begin
  result := (a * M_PI) / 180.0;
end;

//==============================================================================
//
// ProjectPointOnPlane
//
//==============================================================================
procedure ProjectPointOnPlane(dst: Pvec3_t; p: Pvec3_t; normal: Pvec3_t);
var
  d: float;
  n: vec3_t;
  inv_denom: float;
begin
  inv_denom := 1.0 / DotProduct(normal, normal);

  d := DotProduct(normal, p) * inv_denom;

  n[0] := normal[0] * inv_denom;
  n[1] := normal[1] * inv_denom;
  n[2] := normal[2] * inv_denom;

  dst[0] := p[0] - d * n[0];
  dst[1] := p[1] - d * n[1];
  dst[2] := p[2] - d * n[2];
end;

//==============================================================================
// PerpendicularVector
//
// assumes "src" is normalized
//
//==============================================================================
procedure PerpendicularVector(dst: Pvec3_t; src: Pvec3_t);
var
  pos: integer;
  i: integer;
  minelem: float;
  tempvec: vec3_t;
begin
  minelem := 1.0;

  // find the smallest magnitude axially aligned vector
  pos := 0;
  for i := 0 to 2 do
  begin
    if fabs(src[i]) < minelem then
    begin
      pos := i;
      minelem := fabs(src[i]);
    end;
  end;
  tempvec[0] := 0.0;
  tempvec[1] := 0.0;
  tempvec[2] := 0.0;
  tempvec[pos] := 1.0;

  // project the point onto the plane defined by src
  ProjectPointOnPlane(dst, @tempvec, src);

  // normalize the result
  VectorNormalize(dst);
end;

//==============================================================================
//
// RotatePointAroundVector
//
//==============================================================================
procedure RotatePointAroundVector(dst: Pvec3_t; dir: Pvec3_t;
  point: Pvec3_t; degrees: float);
var
  m: mat3_t;
  im: mat3_t;
  zrot: mat3_t;
  tmpmat: mat3_t;
  rot: mat3_t;
  i: integer;
  vr, vup, vf: vec3_t;
begin
  vf[0] := dir[0];
  vf[1] := dir[1];
  vf[2] := dir[2];

  PerpendicularVector(@vr, dir);
  CrossProduct(@vr, @vf, @vup);

  m[0][0] := vr[0];
  m[1][0] := vr[1];
  m[2][0] := vr[2];

  m[0][1] := vup[0];
  m[1][1] := vup[1];
  m[2][1] := vup[2];

  m[0][2] := vf[0];
  m[1][2] := vf[1];
  m[2][2] := vf[2];

  memcpy(@im, @m, SizeOf(im));

  im[0][1] := m[1][0];
  im[0][2] := m[2][0];
  im[1][0] := m[0][1];
  im[1][2] := m[2][1];
  im[2][0] := m[0][2];
  im[2][1] := m[1][2];

  ZeroMemory(@zrot, SizeOf(zrot));
  zrot[0, 0] := 1.0;
  zrot[1, 1] := 1.0;
  zrot[2, 2] := 1.0;

  zrot[0][0] := cos(DEG2RAD(degrees));
  zrot[0][1] := sin(DEG2RAD(degrees));
  zrot[1][0] := -sin(DEG2RAD(degrees));
  zrot[1][1] := cos(DEG2RAD(degrees));

  ConcatRotations(@m, @zrot, @tmpmat);
  ConcatRotations(@tmpmat, @im, @rot);

  for i := 0 to 2 do
  begin
    dst[i] := rot[i, 0] * point[0] + rot[i, 1] * point[1] + rot[i, 2] * point[2];
  end;
end;

//==============================================================================
//
// VectorCompare
//
//==============================================================================
function VectorCompare(v1, v2: Pvec3_t): integer;
var
  i: integer;
begin
  for i := 0 to 2 do
    if v1[i] <> v2[i] then
    begin
      result := 0;
      exit;
    end;

  result := 1;
end;

//==============================================================================
//
// VectorMA
//
//==============================================================================
procedure VectorMA(veca: Pvec3_t; scale: float; vecb: Pvec3_t; vecc: Pvec3_t);
begin
  vecc[0] := veca[0] + scale * vecb[0];
  vecc[1] := veca[1] + scale * vecb[1];
  vecc[2] := veca[2] + scale * vecb[2];
end;

//==============================================================================
//
// DotProduct
//
//==============================================================================
function DotProduct(v1, v2: Pvec3_t): vec_t;
begin
  result := v1[0] * v2[0] + v1[1] * v2[1] + v1[2] * v2[2];
end;

//==============================================================================
//
// VectorSubtract
//
//==============================================================================
procedure VectorSubtract(veca, vecb: Pvec3_t; _out: Pvec3_t);
begin
  _out[0] := veca[0] - vecb[0];
  _out[1] := veca[1] - vecb[1];
  _out[2] := veca[2] - vecb[2];
end;

//==============================================================================
//
// VectorAdd
//
//==============================================================================
procedure VectorAdd(veca, vecb: Pvec3_t; _out: Pvec3_t);
begin
  _out[0] := veca[0] + vecb[0];
  _out[1] := veca[1] + vecb[1];
  _out[2] := veca[2] + vecb[2];
end;

//==============================================================================
//
// VectorCopy
//
//==============================================================================
procedure VectorCopy(_in: Pvec3_t; _out: Pvec3_t);
begin
  _out[0] := _in[0];
  _out[1] := _in[1];
  _out[2] := _in[2];
end;

//==============================================================================
//
// CrossProduct
//
//==============================================================================
procedure CrossProduct(v1, v2: Pvec3_t; cross: Pvec3_t);
begin
  cross[0] := v1[1] * v2[2] - v1[2] * v2[1];
  cross[1] := v1[2] * v2[0] - v1[0] * v2[2];
  cross[2] := v1[0] * v2[1] - v1[1] * v2[0];
end;

//==============================================================================
//
// VectorLength
//
//==============================================================================
function VectorLength(v: Pvec3_t): vec_t; // VJ mayby add VectorSquareLength ?
begin
  result := v[0] * v[0] + v[1] * v[1] + v[2] * v[2];
  result := sqrt(result);    // FIXME
end;

//==============================================================================
//
// VectorNormalize
//
//==============================================================================
function VectorNormalize(v: Pvec3_t): float;
var
  ilength: float;
begin
  result := v[0] * v[0] + v[1] * v[1] + v[2] * v[2];
  result := sqrt(result);    // FIXME

  if result > 0.0 then
  begin
    ilength := 1 / result;
    v[0] := v[0] * ilength;
    v[1] := v[1] * ilength;
    v[2] := v[2] * ilength;
  end;
end;

//==============================================================================
//
// VectorInverse
//
//==============================================================================
procedure VectorInverse(v: Pvec3_t);
begin
  v[0] := -v[0];
  v[1] := -v[1];
  v[2] := -v[2];
end;

//==============================================================================
//
// VectorScale
//
//==============================================================================
procedure VectorScale(_in: Pvec3_t; const scale: vec_t; _out: Pvec3_t);
begin
  _out[0] := _in[0] * scale;
  _out[1] := _in[1] * scale;
  _out[2] := _in[2] * scale;
end;

(*
================
ConcatRotations
================
*)

//==============================================================================
//
// ConcatRotations
//
//==============================================================================
procedure ConcatRotations(in1, in2: Pmat3_t; _out: Pmat3_t);
begin
  _out[0][0] := in1[0][0] * in2[0][0] + in1[0][1] * in2[1][0] +
        in1[0][2] * in2[2][0];
  _out[0][1] := in1[0][0] * in2[0][1] + in1[0][1] * in2[1][1] +
        in1[0][2] * in2[2][1];
  _out[0][2] := in1[0][0] * in2[0][2] + in1[0][1] * in2[1][2] +
        in1[0][2] * in2[2][2];
  _out[1][0] := in1[1][0] * in2[0][0] + in1[1][1] * in2[1][0] +
        in1[1][2] * in2[2][0];
  _out[1][1] := in1[1][0] * in2[0][1] + in1[1][1] * in2[1][1] +
        in1[1][2] * in2[2][1];
  _out[1][2] := in1[1][0] * in2[0][2] + in1[1][1] * in2[1][2] +
        in1[1][2] * in2[2][2];
  _out[2][0] := in1[2][0] * in2[0][0] + in1[2][1] * in2[1][0] +
        in1[2][2] * in2[2][0];
  _out[2][1] := in1[2][0] * in2[0][1] + in1[2][1] * in2[1][1] +
        in1[2][2] * in2[2][1];
  _out[2][2] := in1[2][0] * in2[0][2] + in1[2][1] * in2[1][2] +
        in1[2][2] * in2[2][2];
end;

//==============================================================================
//
// DotProduct2
//
//==============================================================================
function DotProduct2(v1, v2: vec2_t): float;
begin
  result := v1[0] * v2[0] + v1[1] * v2[1];
end;

//==============================================================================
//
// VectorSubtract2
//
//==============================================================================
function VectorSubtract2(veca, vecb: vec2_t): vec2_t;
begin
  result[0] := veca[0] - vecb[0];
  result[1] := veca[1] - vecb[1];
end;

//==============================================================================
//
// VectorAdd2
//
//==============================================================================
function VectorAdd2(veca, vecb: vec2_t): vec2_t;
begin
  result[0] := veca[0] + vecb[0];
  result[1] := veca[1] + vecb[1];
end;

//==============================================================================
//
// VectorScale2
//
//==============================================================================
function VectorScale2(veca: vec2_t; scale: float): vec2_t;
begin
  result[0] := veca[0] * scale;
  result[1] := veca[1] * scale;
end;

//==============================================================================
//
// VectorLength2
//
//==============================================================================
function VectorLength2(v: vec2_t): float;
begin
  result := v[0] * v[0] + v[1] * v[1];
  result := sqrt(result);    // FIXME
end;

//==============================================================================
//
// VectorNormalize2
//
//==============================================================================
function VectorNormalize2(var v: vec2_t): float;
var
  ilength: float;
begin
  result := v[0] * v[0] + v[1] * v[1];
  result := sqrt(result);    // FIXME

  if result > 0.0 then
  begin
    ilength := 1 / result;
    v[0] := v[0] * ilength;
    v[1] := v[1] * ilength;
  end;
end;

//==============================================================================
//
// CalculateReflect2
//
//==============================================================================
procedure CalculateReflect2(const d, wall: vec2_t; var reflect: vec2_t);
var
  n: vec2_t;
begin
  n[0] := -wall[1];
  n[1] := wall[0];

  VectorNormalize2(n);

  reflect := VectorSubtract2(d, VectorScale2(n, 2 * DotProduct2(d, n)));
  VectorNormalize2(reflect);
  reflect := VectorScale2(reflect, VectorLength2(d));
end;

end.

