//------------------------------------------------------------------------------
//
//  DelphiDoom: A modified and improved DOOM engine for Windows
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 2004-2017 by Jim Valavanis
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
//   Seg rendering loops, fixed point & double precision 
//
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit r_segs2;

interface

uses
  m_fixed,
  r_defs;

procedure R_RenderSegLoop8;

procedure R_RenderSegLoop8Optimized;

procedure R_RenderSegLoop32;

procedure R_RenderSegLoop32Optimized;

procedure R_RenderSegLoop8_dbl;

procedure R_RenderSegLoop8Optimized_dbl;

procedure R_RenderSegLoop32_dbl;

procedure R_RenderSegLoop32Optimized_dbl;

procedure R_RenderSegLoop8_3dFloors(const pds: Pdrawseg_t);

procedure R_RenderSegLoop8Optimized_3dFloors(const pds: Pdrawseg_t);

procedure R_RenderSegLoop32_3dFloors(const pds: Pdrawseg_t);

procedure R_RenderSegLoop32Optimized_3dFloors(const pds: Pdrawseg_t);

procedure R_RenderSegLoop8_dbl_3dFloors(const pds: Pdrawseg_t);

procedure R_RenderSegLoop8Optimized_dbl_3dFloors(const pds: Pdrawseg_t);

procedure R_RenderSegLoop32_dbl_3dFloors(const pds: Pdrawseg_t);

procedure R_RenderSegLoop32Optimized_dbl_3dFloors(const pds: Pdrawseg_t);

procedure R_RenderSegLoop8_Vis(const pds: Pdrawseg_t);

procedure R_RenderSegLoop8Optimized_Vis(const pds: Pdrawseg_t);

procedure R_RenderSegLoop32_Vis(const pds: Pdrawseg_t);

procedure R_RenderSegLoop32Optimized_Vis(const pds: Pdrawseg_t);

procedure R_RenderSegLoop8_dbl_Vis(const pds: Pdrawseg_t);

procedure R_RenderSegLoop8Optimized_dbl_Vis(const pds: Pdrawseg_t);

procedure R_RenderSegLoop32_dbl_Vis(const pds: Pdrawseg_t);

procedure R_RenderSegLoop32Optimized_dbl_Vis(const pds: Pdrawseg_t);

procedure R_RenderSegLoop8_3dFloors_Vis(const pds: Pdrawseg_t);

procedure R_RenderSegLoop8Optimized_3dFloors_Vis(const pds: Pdrawseg_t);

procedure R_RenderSegLoop32_3dFloors_Vis(const pds: Pdrawseg_t);

procedure R_RenderSegLoop32Optimized_3dFloors_Vis(const pds: Pdrawseg_t);

procedure R_RenderSegLoop8_dbl_3dFloors_Vis(const pds: Pdrawseg_t);

procedure R_RenderSegLoop8Optimized_dbl_3dFloors_Vis(const pds: Pdrawseg_t);

procedure R_RenderSegLoop32_dbl_3dFloors_Vis(const pds: Pdrawseg_t);

procedure R_RenderSegLoop32Optimized_dbl_3dFloors_Vis(const pds: Pdrawseg_t);

var
  rw_scale_dbl: double;
  rw_scalestep_dbl: double;

  worldhigh_dbl: double;
  worldlow_dbl: double;

  pixhigh_dbl: double;
  pixlow_dbl: double;
  pixhighstep_dbl: double;
  pixlowstep_dbl: double;

  topfrac_dbl: double;
  topstep_dbl: double;

  bottomfrac_dbl: double;
  bottomstep_dbl: double;

procedure R_PrecalcSegs;

function R_DistToSeg(const seg: Pseg_t): fixed_t;

function R_CalcSegOffset(const seg: Pseg_t): fixed_t;

implementation

uses
  d_delphi,
  doomdef,
  p_setup,
  r_column,
  r_data,
  r_draw,
  r_segs,
  r_plane,
  r_main,
  r_hires,
  r_cache_walls,
  r_wall8,
  r_wall32,
  r_3dfloors,
  r_range,
{$IFDEF DEBUG}
  r_debug,
{$ENDIF}
  tables;

//
// R_RenderSegLoop
// Draws zero, one, or two textures (and possibly a masked
//  texture) for walls.
// Can draw or mark the starting pixel of floor and ceiling
//  textures.
// CALLED: CORE LOOPING ROUTINE.
//
procedure R_RenderSegLoop8;
{$UNDEF FLOORS3D}
{$UNDEF FLOORS3DVIS}
{$UNDEF RENDERSEGOPTIMIZED}
{$UNDEF USEDOUBLE}
{$I R_RenderSegLoop8.inc}

procedure R_RenderSegLoop8Optimized;
{$UNDEF FLOORS3D}
{$UNDEF FLOORS3DVIS}
{$DEFINE RENDERSEGOPTIMIZED}
{$UNDEF USEDOUBLE}
{$I R_RenderSegLoop8.inc}

procedure R_RenderSegLoop32;
{$UNDEF FLOORS3D}
{$UNDEF FLOORS3DVIS}
{$UNDEF RENDERSEGOPTIMIZED}
{$UNDEF USEDOUBLE}
{$I R_RenderSegLoop32.inc}

procedure R_RenderSegLoop32Optimized;
{$UNDEF FLOORS3D}
{$UNDEF FLOORS3DVIS}
{$DEFINE RENDERSEGOPTIMIZED}
{$UNDEF USEDOUBLE}
{$I R_RenderSegLoop32.inc}

procedure R_RenderSegLoop8_dbl;
{$UNDEF FLOORS3D}
{$UNDEF FLOORS3DVIS}
{$UNDEF RENDERSEGOPTIMIZED}
{$DEFINE USEDOUBLE}
{$I R_RenderSegLoop8.inc}

procedure R_RenderSegLoop8Optimized_dbl;
{$UNDEF FLOORS3D}
{$UNDEF FLOORS3DVIS}
{$DEFINE RENDERSEGOPTIMIZED}
{$DEFINE USEDOUBLE}
{$I R_RenderSegLoop8.inc}

procedure R_RenderSegLoop32_dbl;
{$UNDEF FLOORS3D}
{$UNDEF FLOORS3DVIS}
{$UNDEF RENDERSEGOPTIMIZED}
{$DEFINE USEDOUBLE}
{$I R_RenderSegLoop32.inc}

procedure R_RenderSegLoop32Optimized_dbl;
{$UNDEF FLOORS3D}
{$UNDEF FLOORS3DVIS}
{$DEFINE RENDERSEGOPTIMIZED}
{$DEFINE USEDOUBLE}
{$I R_RenderSegLoop32.inc}

procedure R_RenderSegLoop8_3dFloors(const pds: Pdrawseg_t);
{$DEFINE FLOORS3D}
{$UNDEF FLOORS3DVIS}
{$UNDEF RENDERSEGOPTIMIZED}
{$UNDEF USEDOUBLE}
{$I R_RenderSegLoop8.inc}

procedure R_RenderSegLoop8Optimized_3dFloors(const pds: Pdrawseg_t);
{$DEFINE FLOORS3D}
{$UNDEF FLOORS3DVIS}
{$DEFINE RENDERSEGOPTIMIZED}
{$UNDEF USEDOUBLE}
{$I R_RenderSegLoop8.inc}

procedure R_RenderSegLoop32_3dFloors(const pds: Pdrawseg_t);
{$DEFINE FLOORS3D}
{$UNDEF FLOORS3DVIS}
{$UNDEF RENDERSEGOPTIMIZED}
{$UNDEF USEDOUBLE}
{$I R_RenderSegLoop32.inc}

procedure R_RenderSegLoop32Optimized_3dFloors(const pds: Pdrawseg_t);
{$DEFINE FLOORS3D}
{$UNDEF FLOORS3DVIS}
{$DEFINE RENDERSEGOPTIMIZED}
{$UNDEF USEDOUBLE}
{$I R_RenderSegLoop32.inc}

procedure R_RenderSegLoop8_dbl_3dFloors(const pds: Pdrawseg_t);
{$DEFINE FLOORS3D}
{$UNDEF FLOORS3DVIS}
{$UNDEF RENDERSEGOPTIMIZED}
{$DEFINE USEDOUBLE}
{$I R_RenderSegLoop8.inc}

procedure R_RenderSegLoop8Optimized_dbl_3dFloors(const pds: Pdrawseg_t);
{$DEFINE FLOORS3D}
{$UNDEF FLOORS3DVIS}
{$DEFINE RENDERSEGOPTIMIZED}
{$DEFINE USEDOUBLE}
{$I R_RenderSegLoop8.inc}

procedure R_RenderSegLoop32_dbl_3dFloors(const pds: Pdrawseg_t);
{$DEFINE FLOORS3D}
{$UNDEF FLOORS3DVIS}
{$UNDEF RENDERSEGOPTIMIZED}
{$DEFINE USEDOUBLE}
{$I R_RenderSegLoop32.inc}

procedure R_RenderSegLoop32Optimized_dbl_3dFloors(const pds: Pdrawseg_t);
{$DEFINE FLOORS3D}
{$UNDEF FLOORS3DVIS}
{$DEFINE RENDERSEGOPTIMIZED}
{$DEFINE USEDOUBLE}
{$I R_RenderSegLoop32.inc}

////////////////////////////////////////////////////////////////////////////////
procedure R_RenderSegLoop8_Vis(const pds: Pdrawseg_t);
{$UNDEF FLOORS3D}
{$DEFINE FLOORS3DVIS}
{$UNDEF RENDERSEGOPTIMIZED}
{$UNDEF USEDOUBLE}
{$I R_RenderSegLoop8.inc}

procedure R_RenderSegLoop8Optimized_Vis(const pds: Pdrawseg_t);
{$UNDEF FLOORS3D}
{$DEFINE FLOORS3DVIS}
{$DEFINE RENDERSEGOPTIMIZED}
{$UNDEF USEDOUBLE}
{$I R_RenderSegLoop8.inc}

procedure R_RenderSegLoop32_Vis(const pds: Pdrawseg_t);
{$UNDEF FLOORS3D}
{$DEFINE FLOORS3DVIS}
{$UNDEF RENDERSEGOPTIMIZED}
{$UNDEF USEDOUBLE}
{$I R_RenderSegLoop32.inc}

procedure R_RenderSegLoop32Optimized_Vis(const pds: Pdrawseg_t);
{$UNDEF FLOORS3D}
{$DEFINE FLOORS3DVIS}
{$DEFINE RENDERSEGOPTIMIZED}
{$UNDEF USEDOUBLE}
{$I R_RenderSegLoop32.inc}

procedure R_RenderSegLoop8_dbl_Vis(const pds: Pdrawseg_t);
{$UNDEF FLOORS3D}
{$DEFINE FLOORS3DVIS}
{$UNDEF RENDERSEGOPTIMIZED}
{$DEFINE USEDOUBLE}
{$I R_RenderSegLoop8.inc}

procedure R_RenderSegLoop8Optimized_dbl_Vis(const pds: Pdrawseg_t);
{$UNDEF FLOORS3D}
{$DEFINE FLOORS3DVIS}
{$DEFINE RENDERSEGOPTIMIZED}
{$DEFINE USEDOUBLE}
{$I R_RenderSegLoop8.inc}

procedure R_RenderSegLoop32_dbl_Vis(const pds: Pdrawseg_t);
{$UNDEF FLOORS3D}
{$DEFINE FLOORS3DVIS}
{$UNDEF RENDERSEGOPTIMIZED}
{$DEFINE USEDOUBLE}
{$I R_RenderSegLoop32.inc}

procedure R_RenderSegLoop32Optimized_dbl_Vis(const pds: Pdrawseg_t);
{$UNDEF FLOORS3D}
{$DEFINE FLOORS3DVIS}
{$DEFINE RENDERSEGOPTIMIZED}
{$DEFINE USEDOUBLE}
{$I R_RenderSegLoop32.inc}

procedure R_RenderSegLoop8_3dFloors_Vis(const pds: Pdrawseg_t);
{$DEFINE FLOORS3D}
{$DEFINE FLOORS3DVIS}
{$UNDEF RENDERSEGOPTIMIZED}
{$UNDEF USEDOUBLE}
{$I R_RenderSegLoop8.inc}

procedure R_RenderSegLoop8Optimized_3dFloors_Vis(const pds: Pdrawseg_t);
{$DEFINE FLOORS3D}
{$DEFINE FLOORS3DVIS}
{$DEFINE RENDERSEGOPTIMIZED}
{$UNDEF USEDOUBLE}
{$I R_RenderSegLoop8.inc}

procedure R_RenderSegLoop32_3dFloors_Vis(const pds: Pdrawseg_t);
{$DEFINE FLOORS3D}
{$DEFINE FLOORS3DVIS}
{$UNDEF RENDERSEGOPTIMIZED}
{$UNDEF USEDOUBLE}
{$I R_RenderSegLoop32.inc}

procedure R_RenderSegLoop32Optimized_3dFloors_Vis(const pds: Pdrawseg_t);
{$DEFINE FLOORS3D}
{$DEFINE FLOORS3DVIS}
{$DEFINE RENDERSEGOPTIMIZED}
{$UNDEF USEDOUBLE}
{$I R_RenderSegLoop32.inc}

procedure R_RenderSegLoop8_dbl_3dFloors_Vis(const pds: Pdrawseg_t);
{$DEFINE FLOORS3D}
{$DEFINE FLOORS3DVIS}
{$UNDEF RENDERSEGOPTIMIZED}
{$DEFINE USEDOUBLE}
{$I R_RenderSegLoop8.inc}

procedure R_RenderSegLoop8Optimized_dbl_3dFloors_Vis(const pds: Pdrawseg_t);
{$DEFINE FLOORS3D}
{$DEFINE FLOORS3DVIS}
{$DEFINE RENDERSEGOPTIMIZED}
{$DEFINE USEDOUBLE}
{$I R_RenderSegLoop8.inc}

procedure R_RenderSegLoop32_dbl_3dFloors_Vis(const pds: Pdrawseg_t);
{$DEFINE FLOORS3D}
{$DEFINE FLOORS3DVIS}
{$UNDEF RENDERSEGOPTIMIZED}
{$DEFINE USEDOUBLE}
{$I R_RenderSegLoop32.inc}

procedure R_RenderSegLoop32Optimized_dbl_3dFloors_Vis(const pds: Pdrawseg_t);
{$DEFINE FLOORS3D}
{$DEFINE FLOORS3DVIS}
{$DEFINE RENDERSEGOPTIMIZED}
{$DEFINE USEDOUBLE}
{$I R_RenderSegLoop32.inc}

procedure R_PrecalcSegs;
var
  i: integer;
  dx, dy: double;
  li: Pseg_t;
begin
  for i := 0 to numsegs - 1 do
  begin
    li := @segs[i];
    dx := li.v2.x - li.v1.x;
    dy := li.v2.y - li.v1.y;
    li.inv_length := 1 / sqrt(dx * dx + dy * dy);
  end;
end;

//
// R_DistToSeg by entryway
//
// https://www.doomworld.com/forum/topic/70288-dynamic-wiggletall-sector-fix-for-fixed-point-software-renderer/?do=findComment&comment=1340433
function R_DistToSeg(const seg: Pseg_t): fixed_t;
var
  dx, dy, dx1, dy1: double;
begin
  if seg.v1.y = seg.v2.y then
  begin
    result := viewy - seg.v1.y;
    if result < 0 then
      result := -result;
    exit;
  end;

  if seg.v1.x = seg.v2.x then
  begin
    result := viewx - seg.v1.x;
    if result < 0 then
      result := -result;
    exit;
  end;

  dx := seg.v2.x - seg.v1.x;
  dy := seg.v2.y - seg.v1.y;
  dx1 := viewx - seg.v1.x;
  dy1 := viewy - seg.v1.y;
  result := round((dy * dx1 - dx * dy1) * seg.inv_length);
  if result < 0 then
    result := -result;
end;

function R_CalcSegOffset(const seg: Pseg_t): fixed_t;
var
  dx, dy, dx1, dy1: double;
begin
  dx := seg.v2.x - seg.v1.x;
  dy := seg.v2.y - seg.v1.y;
  dx1 := viewx - seg.v1.x;
  dy1 := viewy - seg.v1.y;
  result := round((dx * dx1 + dy * dy1) * seg.inv_length);
  if result < 0 then
    result := -result;
end;

end.

