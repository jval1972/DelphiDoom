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

unit r_scale;

interface

uses
  m_fixed,
  tables;

//==============================================================================
//
// R_ScaleFromGlobalAngle
//
//==============================================================================
function R_ScaleFromGlobalAngle(const visangle: angle_t; out overflow: boolean): fixed_t;

//==============================================================================
//
// R_ScaleFromGlobalAngle_Fixed
//
//==============================================================================
function R_ScaleFromGlobalAngle_Fixed(const visangle: angle_t): fixed_t;

//==============================================================================
//
// R_ScaleFromGlobalAngle_DBL
//
//==============================================================================
function R_ScaleFromGlobalAngle_DBL(const visangle: angle_t): double;

var
  max_rwscale: integer = 64 * FRACUNIT;

var
  precisescalefromglobalangle: Boolean = true;

implementation

uses
  doomtype,
  r_segs,
  r_main;

//==============================================================================
//
// R_ScaleFromGlobalAngle
// Returns the texture mapping scale
//  for the current line (horizontal span)
//  at the given angle.
// rw_distance must be calculated first.
//
// JVAL: SOS -> Here lays a problem with rendering accuracy
//
//==============================================================================
function R_ScaleFromGlobalAngle(const visangle: angle_t; out overflow: boolean): fixed_t;
var
  anglea: angle_t;
  angleb: angle_t;
  num: fixed_t;
  den: integer;
begin
  anglea := ANG90 + (visangle - viewangle);
  angleb := ANG90 + (visangle - rw_normalangle);

  num := FixedMul(projectiony, fixedsine[angleb shr FRACBITS]); // JVAL For correct aspect
  den := FixedMul(rw_distance, fixedsine[anglea shr FRACBITS]);

// JVAL: SOS -> Using  result := FixedDivEx(num, den); Exit; eliminates rendering
//        precision erros but crash the game as rw_scale & rw_scalestep are 16:16
//        fixed point
  if den > FixedInt(num) then
  begin
    result := FixedDiv(num, den);
    // JVAL: Change it to 256 * FRACUNIT ??  - original
    // [kb] use R_WiggleFix clamp
    if result > max_rwscale then
    begin
      result := max_rwscale;
      overflow := precisescalefromglobalangle;
    end
    else if result < 256 then
    begin
      result := 256;
      overflow := precisescalefromglobalangle;
    end
    else
      overflow := false;
  end
  else
  begin
    result := max_rwscale;
    overflow := precisescalefromglobalangle;
  end;
end;

//==============================================================================
//
// R_ScaleFromGlobalAngle_Fixed
//
//==============================================================================
function R_ScaleFromGlobalAngle_Fixed(const visangle: angle_t): fixed_t;
var
  anglea: angle_t;
  angleb: angle_t;
  num: fixed_t;
  den: integer;
begin
  anglea := ANG90 + (visangle - viewangle);
  angleb := ANG90 + (visangle - rw_normalangle);

  num := FixedMul(projectiony, fixedsine[angleb shr FRACBITS]); // JVAL For correct aspect
  den := FixedMul(rw_distance, fixedsine[anglea shr FRACBITS]);

// JVAL: SOS -> Using  result := FixedDivEx(num, den); Exit; eliminates rendering
//        precision erros but crash the game as rw_scale & rw_scalestep are 16:16
//        fixed point
  if den > FixedInt(num) then
  begin
    result := FixedDiv(num, den);
  // JVAL: Change it to 256 * FRACUNIT ??  - original
    if result > max_rwscale {64 * FRACUNIT} then
    begin
      result := max_rwscale{64 * FRACUNIT};
    end
    else if result < 256 then
    begin
      result := 256;
    end;
  end
  else
  begin
    result := max_rwscale{64 * FRACUNIT};
  end;
end;

//==============================================================================
// R_ScaleFromGlobalAngle_DBL
//
//const
//  MINSCALE = 16;
//  MAXSCALE = 2048 * FRACUNIT;
//
//==============================================================================
function R_ScaleFromGlobalAngle_DBL(const visangle: angle_t): double;
var
  anglea: angle_t;
  angleb: angle_t;
  num: Double;
  den: Double;
begin
  anglea := ANG90 + (visangle - viewangle);
  angleb := ANG90 + (visangle - rw_normalangle);

  num := projectiony * Sin(angleb * ANGLE_T_TO_RAD);
  den := rw_distance * Sin(anglea * ANGLE_T_TO_RAD);

  if den = 0 then
  begin
    if num < 0 then
      result := 256 //MINSCALE
    else
      result := max_rwscale; //MAXSCALE;
  end
  else
  begin
    result := (num / den) * FRACUNIT;
    if result < 256 {MINSCALE} then
      result := 256 {MINSCALE}
    else if result > max_rwscale {MAXSCALE} then
      result := max_rwscale {MAXSCALE};
  end;

end;

end.

