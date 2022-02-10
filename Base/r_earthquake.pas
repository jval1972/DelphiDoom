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
// DESCRIPTION:
//  Global earthquake effect
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit r_earthquake;

interface

uses
  d_player;

//==============================================================================
//
// R_AdjustGlobalEarthQuake
//
//==============================================================================
procedure R_AdjustGlobalEarthQuake(const player: Pplayer_t);

implementation

uses
  m_fixed,
  m_rnd,
  r_main;

//==============================================================================
//
// R_AdjustGlobalEarthQuake
//
//==============================================================================
procedure R_AdjustGlobalEarthQuake(const player: Pplayer_t);
var
  seed1, seed2: integer;
  rnd1a, rnd1b, rnd2a, rnd2b: integer;
  vx1, vx2: integer;
  vy1, vy2: integer;
  frac1, frac2: integer;
begin
  if player.quaketics = 0 then
  begin
    player.quakeintensity := 0;
    exit;
  end;
  seed1 := player.quaketics div FRACUNIT;
  seed2 := seed1 + 1;
  frac1 := player.quaketics mod FRACUNIT;
  frac2 := FRACUNIT - frac1;
  rnd1a := C_Random(seed1);
  rnd1b := C_Random(seed1);
  rnd2a := C_Random(seed2);
  rnd2b := C_Random(seed2);
  vx1 := viewx + (4 - (rnd1a mod 8)) * player.quakeintensity;
  vy1 := viewy + (4 - (rnd1b mod 8)) * player.quakeintensity;
  vx2 := viewx + (4 - (rnd2a mod 8)) * player.quakeintensity;
  vy2 := viewy + (4 - (rnd2b mod 8)) * player.quakeintensity;
  viewx := Round(vx1 / FRACUNIT * frac1 + vx2 / FRACUNIT * frac2);
  viewy := Round(vy1 / FRACUNIT * frac1 + vy2 / FRACUNIT * frac2);
end;

end.
