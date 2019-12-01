//------------------------------------------------------------------------------
//
//  DelphiDoom: A modified and improved DOOM engine for Windows
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 2004-2016 by Jim Valavanis
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
//  Chase camera, teleport zoom effect
//
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit r_camera;

interface

uses
  d_player,
  m_fixed,
  p_mobj_h;

procedure R_AdjustChaseCamera;

procedure R_AdjustTeleportZoom(const player: Pplayer_t);

var
  chasecamera: boolean;
  chasecamera_viewxy: integer;
  chasecamera_viewz: integer;

implementation

uses
  d_delphi,
  tables,
  p_3dfloors,
  p_sight,
  p_map,
  p_telept,
  r_defs,
  r_main,
  r_data,
  r_draw;

const
  CAMERARADIOUS = 8 * FRACUNIT;

//
// P_AdjustChaseCamera
//
// JVAL: Adjust the chace camera position
//       A bit clumsy but works OK
//
procedure R_AdjustChaseCamera;
var
  c_an: angle_t;
  cx, cy, cz: fixed_t;
  dx, dy: fixed_t;
  loops: integer;
  sec: Psector_t;
  sec2: Psector_t;
  ceilz, floorz: fixed_t;
begin
  if chasecamera then
  begin
    c_an := (viewangle + ANG180) shr ANGLETOFINESHIFT;
    dx := chasecamera_viewxy * finecosine[c_an];
    dy := chasecamera_viewxy * finesine[c_an];

    loops := 0;
    repeat
      cx := viewx + dx;
      cy := viewy + dy;

      sec := R_PointInSubSector(cx, cy).sector;
      cz := viewz + chasecamera_viewz * FRACUNIT; // JVAL: Slopes
      ceilz := P_3dCeilingHeight(sec, cx, cy, cz) +
               P_SectorJumpOverhead(sec{$IFDEF HERETIC_OR_HEXEN}, viewplayer{$ENDIF}) - 1 - CAMERARADIOUS; // JVAL: 3d floors
      if cz > ceilz then
        cz := ceilz
      else
      begin
        floorz := P_3dFloorHeight(sec, cx, cy, cz) + CAMERARADIOUS; // JVAL: Slopes
        if cz < floorz then
          cz := floorz
      end;

      if {(sec.renderflags and SRF_SLOPED <> 0) or} P_CheckCameraSight(cx, cy, cz, viewplayer.mo) then // JVAL: Slopes
        if not P_PtInSolidFloor2(cx, cy, cz, CAMERARADIOUS) then  // JVAL: 3d Floors
          break;

      dx := dx * 31 div 32;
      dy := dy * 31 div 32;
      inc(loops);
    until loops > 64;
    {$IFNDEF OPENGL}
    if loops > 1 then
      R_PlayerViewBlanc(aprox_black);
    {$ENDIF}
    viewx := cx;
    viewy := cy;

    sec2 := R_PointInSubsector(viewx, viewy).sector;
    floorz := P_3dFloorHeight(sec2, viewx, viewy, cz);  // JVAL: Slopes
    if cz < floorz then
      cz := floorz + CAMERARADIOUS; // JVAL: Slopes
    ceilz := P_3dCeilingHeight(sec2, viewx, viewy, cz) +
             P_SectorJumpOverhead(sec2{$IFDEF HERETIC_OR_HEXEN}, viewplayer{$ENDIF});  // JVAL: Slopes
    if cz > ceilz then
      cz := ceilz - CAMERARADIOUS;  // JVAL: Slopes

    viewz := cz;
  end;

end;

procedure R_AdjustTeleportZoom(const player: Pplayer_t);
var
  mo: Pmobj_t;
  an: angle_t;
  distf: float;
  dist: fixed_t;
begin
  if player.teleporttics <= 0 then
    exit;

  mo := player.mo;
  if mo = nil then
    exit;

  an := mo.angle div ANGLETOFINEUNIT;

  distf := Sqr(player.teleporttics / FRACUNIT) / (TELEPORTZOOM / FRACUNIT);
  dist := Round(distf * FRACUNIT);
  viewx := viewx - FixedMul(dist, finecosine[an]);
  viewy := viewy - FixedMul(dist, finesine[an]);
end;

end.
