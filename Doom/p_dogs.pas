//------------------------------------------------------------------------------
//
//  DelphiDoom: A modified and improved DOOM engine for Windows
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
// DESCRIPTION:
//  Dogs handling
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit p_dogs;

interface

uses
  d_player;

procedure P_HandleDogsNearMe(const p: Pplayer_t);

implementation

uses
  m_fixed,
  p_local,
  p_mobj_h,
  p_maputl,
  p_playertrace,
  p_setup,
  r_main,
  tables;

const
  MAXFRIENDRADIUS = 128 * FRACUNIT;

var
  dplayermo: Pmobj_t;

function RIT_HandleDogsNearMe(mo: Pmobj_t): boolean;
var
  dist1: fixed_t;
  dist2: fixed_t;
  speed: fixed_t;
  realangle: angle_t;
begin
  result := true;

  if mo.flags2_ex and MF2_EX_FRIEND = 0 then
    exit;

  if mo.health <= 0 then
    exit;

  if mo.info.doomednum <> 888 then
    exit;

  dist1 := P_AproxDistance(mo.x - dplayermo.x, mo.y - dplayermo.y);
  if dist1 > MAXFRIENDRADIUS then
    exit;

  if mo.health < mo.info.spawnhealth then
    inc(mo.health);

  dist2 := P_AproxDistance(mo.x + mo.momx - dplayermo.x - dplayermo.momx, mo.y + mo.momx - dplayermo.y - dplayermo.momx);
  if dist2 > dist1 then // Going away
  begin
    mo.momx := mo.momx * 15 div 16;
    mo.momy := mo.momy * 15 div 16;
    mo.momz := mo.momz * 15 div 16;
    exit;
  end;

  if dist1 < MAXFRIENDRADIUS div 2 then
  begin
    if mo.x < dplayermo.x then
      mo.momx := -mo.info.speed * FRACUNIT
    else
      mo.momx := mo.info.speed * FRACUNIT;
    if mo.y < dplayermo.y then
      mo.momy := -mo.info.speed * FRACUNIT
    else
      mo.momy := mo.info.speed * FRACUNIT;
    exit;
  end;

  speed := mo.info.speed * FRACUNIT;

  realangle := R_PointToAngle2(mo.x, mo.y, dplayermo.x, dplayermo.y) - ANG180;

  mo.momx := FixedMul(speed, finecosine[realangle shr ANGLETOFINESHIFT]);
  mo.momy := FixedMul(speed, finesine[realangle shr ANGLETOFINESHIFT]);
end;

procedure P_HandleDogsNearMe(const p: Pplayer_t);
var
  x: integer;
  y: integer;
  xl: integer;
  xh: integer;
  yl: integer;
  yh: integer;
begin
  dplayermo := p.mo;
  if dplayermo = nil then
    exit;

  yh := MapBlockIntY(int64(viewy) + MAXFRIENDRADIUS - int64(bmaporgy));
  yl := MapBlockIntY(int64(viewy) - MAXFRIENDRADIUS - int64(bmaporgy));
  xh := MapBlockIntX(int64(viewx) + MAXFRIENDRADIUS - int64(bmaporgx));
  xl := MapBlockIntX(int64(viewx) - MAXFRIENDRADIUS - int64(bmaporgx));

  for y := yl to yh do
    for x := xl to xh do
      P_BlockThingsIterator(x, y, RIT_HandleDogsNearMe);
end;

end.
