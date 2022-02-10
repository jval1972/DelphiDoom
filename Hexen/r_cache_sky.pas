//------------------------------------------------------------------------------
//
//  DelphiHexen is a source port of the game Hexen and it is
//  based on original Linux Doom as published by "id Software", on
//  Hexen source as published by "Raven" software and DelphiDoom
//  as published by Jim Valavanis.
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

unit r_cache_sky;

interface

//==============================================================================
// R_ReadDC32InternalSkyCache
//
// Sky Cache
//
//==============================================================================
procedure R_ReadDC32InternalSkyCache(const rtex, rcol: integer);

implementation

uses
  r_column,
  r_sky,
  r_cache_sky1,
  r_cache_sky1_dbl,
  r_cache_sky2;

//==============================================================================
// R_ReadDC32InternalSkyCache
//
// R_ReadDC32InternalCache
//
// JVAL
//  Create dc_source32 from internal (IWAD) texture
//
//==============================================================================
procedure R_ReadDC32InternalSkyCache(const rtex, rcol: integer);
begin
  if dc_mod = 0 then
  begin
    if DoubleSky then
      R_ReadDC32InternalSkyCache1dbl(rtex, rcol)
    else
      R_ReadDC32InternalSkyCache1(rtex, rcol);
  end
  else
    R_ReadDC32InternalSkyCache2(rtex, rcol)
end;

end.
