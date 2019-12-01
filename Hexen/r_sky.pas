//------------------------------------------------------------------------------
//
//  DelphiHexen: A modified and improved Hexen port for Windows
//  based on original Linux Doom as published by "id Software", on
//  Hexen source as published by "Raven" software and DelphiDoom
//  as published by Jim Valavanis.
//  Copyright (C) 2004-2008 by Jim Valavanis
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
//  Site  : http://delphidoom.sitesled.com/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit r_sky;

interface

uses
  m_fixed;


// Emacs style mode select   -*- C++ -*-
//-----------------------------------------------------------------------------
//
// $Id:$
//
// Copyright (C) 1993-1996 by id Software, Inc.
//
// This source is available for distribution and/or modification
// only under the terms of the DOOM Source Code License as
// published by id Software. All rights reserved.
//
// The source is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// FITNESS FOR A PARTICULAR PURPOSE. See the DOOM Source Code License
// for more details.
//
// DESCRIPTION:
//  Sky rendering.
//
//-----------------------------------------------------------------------------

// SKY, store the number for name.
const
  SKYFLATNAME = 'F_SKY';

// The sky map is 256*128*4 maps.
  ANGLETOSKYSHIFT = 22;
  ANGLETOSKYUNIT = 1 shl 22;

var
  skyflatnum: integer;
  skytexturemid: integer;
  Sky1Texture: integer;
  Sky2Texture: integer;
  Sky1ColumnOffset: fixed_t;
  Sky2ColumnOffset: fixed_t;
  Sky1ScrollDelta: fixed_t;
  Sky2ScrollDelta: fixed_t;
  DoubleSky: boolean;

procedure R_InitSkyMap;

procedure R_InitSky(map: integer);

implementation

uses
  p_setup;

//
// R_InitSkyMap
// Called whenever the view size changes.
//
procedure R_InitSkyMap;
begin
  skytexturemid := 200 * FRACUNIT;
end;

//==========================================================================
//
// R_InitSky
//
// Called at level load.
//
//==========================================================================

procedure R_InitSky(map: integer);
begin
  Sky1Texture := P_GetMapSky1Texture(map);
  Sky2Texture := P_GetMapSky2Texture(map);
  Sky1ScrollDelta := P_GetMapSky1ScrollDelta(map);
  Sky2ScrollDelta := P_GetMapSky2ScrollDelta(map);
  Sky1ColumnOffset := 0;
  Sky2ColumnOffset := 0;
  DoubleSky := P_GetMapDoubleSky(map);
end;

end.
