//------------------------------------------------------------------------------
//
//  DelphiHeretic: A modified and improved Heretic port for Windows
//  based on original Linux Doom as published by "id Software", on
//  Heretic source as published by "Raven" software and DelphiDoom
//  as published by Jim Valavanis.
//  Copyright (C) 2004-2021 by Jim Valavanis
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
// DESCRIPTION:
//  Sky rendering.
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit r_sky;

interface

// SKY, store the number for name.
const
  SKYFLATNAME = 'F_SKY1';

// The sky map is 256*128*4 maps.
  ANGLETOSKYSHIFT = 22;
  ANGLETOSKYUNIT = 1 shl 22;

var
  skyflatnum: integer;
  skytexture: integer;
  skytexturemid: integer;

procedure R_InitSkyMap;

implementation

uses
  m_fixed; // Needed for FRACUNIT.

//
// R_InitSkyMap
// Called whenever the view size changes.
//
procedure R_InitSkyMap;
begin
  skytexturemid := 100 * FRACUNIT;
end;

end.
