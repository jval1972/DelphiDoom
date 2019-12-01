//------------------------------------------------------------------------------
//
//  DelphiDoom: A modified and improved DOOM engine for Windows
//  based on original Linux Doom as published by "id Software"
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

procedure R_RenderSegLoop8;

procedure R_RenderSegLoop8Optimized;

procedure R_RenderSegLoop32;

procedure R_RenderSegLoop32Optimized;

procedure R_RenderSegLoop8_dbl;

procedure R_RenderSegLoop8Optimized_dbl;

procedure R_RenderSegLoop32_dbl;

procedure R_RenderSegLoop32Optimized_dbl;

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

implementation

uses
  d_delphi,
  doomdef,
  r_column,
  r_data,
  r_draw,
  r_segs,
  r_plane,
  r_main,
  r_hires,
  r_ccache,
  r_wall8,
  r_wall32,
  tables,
  m_fixed;

//
// R_RenderSegLoop
// Draws zero, one, or two textures (and possibly a masked
//  texture) for walls.
// Can draw or mark the starting pixel of floor and ceiling
//  textures.
// CALLED: CORE LOOPING ROUTINE.
//
procedure R_RenderSegLoop8;
{$UNDEF RENDERSEGOPTIMIZED}
{$I R_RenderSegLoop8.inc}

procedure R_RenderSegLoop8Optimized;
{$DEFINE RENDERSEGOPTIMIZED}
{$I R_RenderSegLoop8.inc}

procedure R_RenderSegLoop32;
{$UNDEF RENDERSEGOPTIMIZED}
{$I R_RenderSegLoop32.inc}

procedure R_RenderSegLoop32Optimized;
{$DEFINE RENDERSEGOPTIMIZED}
{$I R_RenderSegLoop32.inc}

procedure R_RenderSegLoop8_dbl;
{$UNDEF RENDERSEGOPTIMIZED}
{$I R_RenderSegLoop8_dbl.inc}

procedure R_RenderSegLoop8Optimized_dbl;
{$DEFINE RENDERSEGOPTIMIZED}
{$I R_RenderSegLoop8_dbl.inc}

procedure R_RenderSegLoop32_dbl;
{$UNDEF RENDERSEGOPTIMIZED}
{$I R_RenderSegLoop32_dbl.inc}

procedure R_RenderSegLoop32Optimized_dbl;
{$DEFINE RENDERSEGOPTIMIZED}
{$I R_RenderSegLoop32_dbl.inc}

end.

