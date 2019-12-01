//------------------------------------------------------------------------------
//
//  DelphiHexen: A modified and improved Hexen port for Windows
//  based on original Linux Doom as published by "id Software", on
//  Hexen source as published by "Raven" software and DelphiDoom
//  as published by Jim Valavanis.
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

procedure R_AdjustChaseCamera(const p: Pplayer_t);

var
  chasecamera: boolean;
  chasecamera_viewxy: integer;
  chasecamera_viewz: integer;

implementation

uses
  d_delphi,
  tables,
  p_sight, p_map,
  r_defs, r_main, r_data, r_draw;

//
// P_CheckCameraSight
//
// JVAL: Check if the camera can see mo (=player.mo)
//

function R_CheckCameraSight(camx, camy, camz: fixed_t; mo: Pmobj_t): boolean;
begin
  if mo = nil then
  begin
    result := false;
    exit;
  end;

  inc(validcount);

  sightzstart := camz + mo.height - _SHR2(mo.height);
  topslope := (mo.z + mo.height) - sightzstart;
  bottomslope := mo.z - sightzstart;

  result := P_SightPathTraverse(camx, camy, mo.x, mo.y);
end;

const
  CAMERARADIOUS = 8 * FRACUNIT;

//
// P_AdjustChaseCamera
//
// JVAL: Adjust the chace camera position
//       A bit clumsy but works OK
//
procedure R_AdjustChaseCamera(const p: Pplayer_t);
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
    sec := Psubsector_t(p.mo.subsector).sector;
    ceilz := sec.ceilingheight + P_SectorJumpOverhead(sec, p) - CAMERARADIOUS;
    cz := viewz + chasecamera_viewz * FRACUNIT;
    if cz > ceilz then
      cz := ceilz
    else
    begin
      floorz := sec.floorheight + CAMERARADIOUS;
      if cz < floorz then
        cz := floorz
    end;


    c_an := (viewangle + ANG180) shr ANGLETOFINESHIFT;
    dx := chasecamera_viewxy * finecosine[c_an];
    dy := chasecamera_viewxy * finesine[c_an];

    loops := 0;
    repeat
      cx := viewx + dx;
      cy := viewy + dy;
      if R_CheckCameraSight(cx, cy, cz, p.mo) then
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
    floorz := sec2.floorheight;
    if cz < floorz then
      cz := floorz + 1 * FRACUNIT;
    ceilz := sec2.ceilingheight + P_SectorJumpOverhead(sec2, p);
    if cz > ceilz then
      cz := ceilz - 1 * FRACUNIT;

    viewz := cz;
  end;

end;


end.
