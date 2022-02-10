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
//  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
//  02111-1307, USA.
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit gl_frustum;

interface

//==============================================================================
//
// fr_CalculateFrustum
//
//==============================================================================
procedure fr_CalculateFrustum;

//==============================================================================
//
// fr_PointInFrustum
//
//==============================================================================
function fr_PointInFrustum(X, Y, Z: single): boolean;

//==============================================================================
//
// fr_SphereInFrustum
//
//==============================================================================
function fr_SphereInFrustum(X, Y, Z, Radius: single): boolean;

//==============================================================================
//
// fr_CubeInFrustum
//
//==============================================================================
function fr_CubeInFrustum(X, Y, Z, Size: single): boolean;

//==============================================================================
//
// fr_BoxInFrustum
//
//==============================================================================
function fr_BoxInFrustum(X, Y, Z, sizeX, sizeY, sizeZ: single): boolean;

implementation

uses
  dglOpenGL;

type
  frustum_t = array[0..5, 0..3] of single;

var
  frustum: frustum_t;

const
  RIGHT   = 0;  // The RIGHT  side of the frustum
  LEFT    = 1;  // The LEFT   side of the frustum
  BOTTOM  = 2;  // The BOTTOM side of the frustum
  TOP     = 3;  // The TOP    side of the frustum
  BACK    = 4;  // The BACK   side of the frustum
  FRONT   = 5;  // The FRONT  side of the frustum

{------------------------------------------------------------------}
{--- This normalizes a plane (A side) from a given frustum.     ---}
{------------------------------------------------------------------}

//==============================================================================
//
// NormalizeFrustum
//
//==============================================================================
procedure NormalizeFrustum(side: integer);
var
  magnitude: single;
begin
  magnitude := sqrt(frustum[side][0] * frustum[side][0] +
                    frustum[side][1] * frustum[side][1] +
                    frustum[side][2] * frustum[side][2]);
  frustum[side][0] := frustum[side][0] / magnitude;
  frustum[side][1] := frustum[side][1] / magnitude;
  frustum[side][2] := frustum[side][2] / magnitude;
  frustum[side][3] := frustum[side][3] / magnitude;
end;

{ TFrustum }

{---------------------------------------------------------------------}
{---------------------------------------------------------------------}

//==============================================================================
//
// fr_CalculateFrustum
//
//==============================================================================
procedure fr_CalculateFrustum;
var
  proj, modl, clip: array[0..15] of single;
begin
  // glGetFloatv() is used to extract information about our OpenGL world.
  // Below, we pass in GL_PROJECTION_MATRIX to abstract our projection matrix.
  glGetFloatv(GL_PROJECTION_MATRIX, @proj);

  // By passing in GL_MODELVIEW_MATRIX, we can abstract our model view matrix.
  glGetFloatv(GL_MODELVIEW_MATRIX, @modl);

  // Now that we have our modelview and projection matrix, if we combine these 2 matrices,
  // it will give us our clipping planes.  To combine 2 matrices, we multiply them.
  clip[ 0] := modl[ 0] * proj[ 0] + modl[ 1] * proj[ 4] + modl[ 2] * proj[ 8] + modl[ 3] * proj[12];
  clip[ 1] := modl[ 0] * proj[ 1] + modl[ 1] * proj[ 5] + modl[ 2] * proj[ 9] + modl[ 3] * proj[13];
  clip[ 2] := modl[ 0] * proj[ 2] + modl[ 1] * proj[ 6] + modl[ 2] * proj[10] + modl[ 3] * proj[14];
  clip[ 3] := modl[ 0] * proj[ 3] + modl[ 1] * proj[ 7] + modl[ 2] * proj[11] + modl[ 3] * proj[15];

  clip[ 4] := modl[ 4] * proj[ 0] + modl[ 5] * proj[ 4] + modl[ 6] * proj[ 8] + modl[ 7] * proj[12];
  clip[ 5] := modl[ 4] * proj[ 1] + modl[ 5] * proj[ 5] + modl[ 6] * proj[ 9] + modl[ 7] * proj[13];
  clip[ 6] := modl[ 4] * proj[ 2] + modl[ 5] * proj[ 6] + modl[ 6] * proj[10] + modl[ 7] * proj[14];
  clip[ 7] := modl[ 4] * proj[ 3] + modl[ 5] * proj[ 7] + modl[ 6] * proj[11] + modl[ 7] * proj[15];

  clip[ 8] := modl[ 8] * proj[ 0] + modl[ 9] * proj[ 4] + modl[10] * proj[ 8] + modl[11] * proj[12];
  clip[ 9] := modl[ 8] * proj[ 1] + modl[ 9] * proj[ 5] + modl[10] * proj[ 9] + modl[11] * proj[13];
  clip[10] := modl[ 8] * proj[ 2] + modl[ 9] * proj[ 6] + modl[10] * proj[10] + modl[11] * proj[14];
  clip[11] := modl[ 8] * proj[ 3] + modl[ 9] * proj[ 7] + modl[10] * proj[11] + modl[11] * proj[15];

  clip[12] := modl[12] * proj[ 0] + modl[13] * proj[ 4] + modl[14] * proj[ 8] + modl[15] * proj[12];
  clip[13] := modl[12] * proj[ 1] + modl[13] * proj[ 5] + modl[14] * proj[ 9] + modl[15] * proj[13];
  clip[14] := modl[12] * proj[ 2] + modl[13] * proj[ 6] + modl[14] * proj[10] + modl[15] * proj[14];
  clip[15] := modl[12] * proj[ 3] + modl[13] * proj[ 7] + modl[14] * proj[11] + modl[15] * proj[15];

  // Now we actually want to get the sides of the frustum.  To do this we take
  // the clipping planes we received above and extract the sides from them.

  // This will extract the RIGHT side of the frustum
  frustum[RIGHT][0] := clip[ 3] - clip[ 0];
  frustum[RIGHT][1] := clip[ 7] - clip[ 4];
  frustum[RIGHT][2] := clip[11] - clip[ 8];
  frustum[RIGHT][3] := clip[15] - clip[12];

  // Now that we have a normal (A,B,C) and a distance (D) to the plane,
  // we want to normalize that normal and distance.
  NormalizeFrustum(RIGHT);

  // This will extract the LEFT side of the frustum
  frustum[LEFT][0] := clip[ 3] + clip[ 0];
  frustum[LEFT][1] := clip[ 7] + clip[ 4];
  frustum[LEFT][2] := clip[11] + clip[ 8];
  frustum[LEFT][3] := clip[15] + clip[12];

  // Normalize the LEFT side
  NormalizeFrustum(LEFT);

  // This will extract the BOTTOM side of the frustum
  frustum[BOTTOM][0] := clip[ 3] + clip[ 1];
  frustum[BOTTOM][1] := clip[ 7] + clip[ 5];
  frustum[BOTTOM][2] := clip[11] + clip[ 9];
  frustum[BOTTOM][3] := clip[15] + clip[13];

  // Normalize the BOTTOM side
  NormalizeFrustum(BOTTOM);

  // This will extract the TOP side of the frustum
  frustum[TOP][0] := clip[ 3] - clip[ 1];
  frustum[TOP][1] := clip[ 7] - clip[ 5];
  frustum[TOP][2] := clip[11] - clip[ 9];
  frustum[TOP][3] := clip[15] - clip[13];

  // Normalize the TOP side
  NormalizeFrustum(TOP);

  // This will extract the BACK side of the frustum
  frustum[BACK][0] := clip[ 3] - clip[ 2];
  frustum[BACK][1] := clip[ 7] - clip[ 6];
  frustum[BACK][2] := clip[11] - clip[10];
  frustum[BACK][3] := clip[15] - clip[14];

  // Normalize the BACK side
  NormalizeFrustum(BACK);

  // This will extract the FRONT side of the frustum
  frustum[FRONT][0] := clip[ 3] + clip[ 2];
  frustum[FRONT][1] := clip[ 7] + clip[ 6];
  frustum[FRONT][2] := clip[11] + clip[10];
  frustum[FRONT][3] := clip[15] + clip[14];

  // Normalize the FRONT side
  NormalizeFrustum(FRONT);
end;

{----------------------------------------------------------------}
{--- This determines if a point is inside of the view frustum ---}
{----------------------------------------------------------------}

//==============================================================================
//
// fr_PointInFrustum
//
//==============================================================================
function fr_PointInFrustum(X, Y, Z: single): boolean;
var
  i: integer;
begin
  // Go through all the sides of the frustum
  for i := 0 to 5 do
    // Calculate the plane equation and check if the point is behind a side of the frustum
    if frustum[i][0] * x + frustum[i][1] * y + frustum[i][2] * z + frustum[i][3] <= 0 then
    begin
      result := false;
      exit;  // The point was behind a side, so it ISN'T in the frustum
    end;

  // The point was inside of the frustum (In front of ALL the sides of the frustum)
  result := true;
end;

{----------------------------------------------------------------}
{--- This determines if a sphere is inside our view frustum   ---}
{----------------------------------------------------------------}

//==============================================================================
//
// fr_SphereInFrustum
//
//==============================================================================
function fr_SphereInFrustum(X, Y, Z, Radius: single): boolean;
var
  i: integer;
begin
  for i := 0 to 5 do
    // Calculate the plane equation and check if the point is behind a side of the frustum
    if frustum[i][0] * x + frustum[i][1] * y + frustum[i][2] * z + frustum[i][3] <= -radius then
    begin
      result := false;
      exit;  // The point was behind a side, so it ISN'T in the frustum
    end;

  // The point was inside of the frustum (In front of ALL the sides of the frustum)
  result := true;
end;

{----------------------------------------------------------------}
{--- This determines if a BOX is in or around our view        ---}
{--- frustum by it's min and max points                       ---}
{----------------------------------------------------------------}

//==============================================================================
//
// fr_BoxInFrustum
//
//==============================================================================
function fr_BoxInFrustum(X, Y, Z, sizeX, sizeY, sizeZ: single): boolean;
var
  i: integer;
begin
  for i := 0 to 5 do
  begin
    if frustum[i][0] * x + frustum[i][1] * y + frustum[i][2] * z + frustum[i][3] > 0 then
      continue;
    if frustum[i][0] * sizeX + frustum[i][1] * y + frustum[i][2] * z + frustum[i][3] > 0 then
      continue;
    if frustum[i][0] * x + frustum[i][1] * sizeY + frustum[i][2] * z + frustum[i][3] > 0 then
      continue;
    if frustum[i][0] * sizeX + frustum[i][1] * sizeY + frustum[i][2] * z + frustum[i][3] > 0 then
      continue;
    if frustum[i][0] * x + frustum[i][1] * y + frustum[i][2] * sizeZ + frustum[i][3] > 0 then
      continue;
    if frustum[i][0] * sizeX + frustum[i][1] * y + frustum[i][2] * sizeZ + frustum[i][3] > 0 then
      continue;
    if frustum[i][0] * x + frustum[i][1] * sizeY + frustum[i][2] * sizeZ + frustum[i][3] > 0 then
      continue;
    if frustum[i][0] * sizeX + frustum[i][1] * sizeY + frustum[i][2] * sizeZ + frustum[i][3] > 0 then
      continue;

    // If we get here, it isn't in the frustum
    result := false;
    exit;
  end;

  // Return a true for the box being inside of the frustum
  result := true;
end;

{----------------------------------------------------------------}
{---    This determines if a cube is in or around our view    ---}
{---    frustum by using it's center and 1/2 it's length      ---}
{----------------------------------------------------------------}

//==============================================================================
//
// fr_CubeInFrustum
//
//==============================================================================
function fr_CubeInFrustum(X, Y, Z, Size: single): boolean;
var
  i: integer;
begin
  for i := 0 to 5 do
  begin
    if frustum[i][0] * (x - size) + frustum[i][1] * (y - size) + frustum[i][2] * (z - size) + frustum[i][3] > 0 then
       continue;
    if frustum[i][0] * (x + size) + frustum[i][1] * (y - size) + frustum[i][2] * (z - size) + frustum[i][3] > 0 then
       continue;
    if frustum[i][0] * (x - size) + frustum[i][1] * (y + size) + frustum[i][2] * (z - size) + frustum[i][3] > 0 then
       continue;
    if frustum[i][0] * (x + size) + frustum[i][1] * (y + size) + frustum[i][2] * (z - size) + frustum[i][3] > 0 then
       continue;
    if frustum[i][0] * (x - size) + frustum[i][1] * (y - size) + frustum[i][2] * (z + size) + frustum[i][3] > 0 then
       continue;
    if frustum[i][0] * (x + size) + frustum[i][1] * (y - size) + frustum[i][2] * (z + size) + frustum[i][3] > 0 then
       continue;
    if frustum[i][0] * (x - size) + frustum[i][1] * (y + size) + frustum[i][2] * (z + size) + frustum[i][3] > 0 then
       continue;
    if frustum[i][0] * (x + size) + frustum[i][1] * (y + size) + frustum[i][2] * (z + size) + frustum[i][3] > 0 then
       continue;

    // If we get here, it isn't in the frustum
    result := false;
    exit;
  end;

  result := true;
end;

end.

