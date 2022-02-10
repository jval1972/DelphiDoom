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

unit p_terrain;

interface

//==============================================================================
//
// P_TerrainTypeForName
//
//==============================================================================
function P_TerrainTypeForName(flatname: string): integer;

var
  allowterrainsplashes: boolean;

const
  FLOOR_SOLID = 0;
  FLOOR_WATER = 1;
  FLOOR_LAVA = 2;
  FLOOR_SLUDGE = 3;
  FLOOR_NUKAGE = 4;

implementation

uses
  d_delphi;

type
  terraintypedef_t = record
    name: string;
    _type: integer;
  end;

var
  terraintypedefs: array[0..23] of terraintypedef_t = (
    (name: 'FWATER1'; _type: FLOOR_WATER),
    (name: 'FWATER2'; _type: FLOOR_WATER),
    (name: 'FWATER3'; _type: FLOOR_WATER),
    (name: 'FWATER4'; _type: FLOOR_WATER),
    (name: 'LAVA1'; _type: FLOOR_LAVA),
    (name: 'LAVA2'; _type: FLOOR_LAVA),
    (name: 'LAVA3'; _type: FLOOR_LAVA),
    (name: 'LAVA4'; _type: FLOOR_LAVA),
    (name: 'SLIME01'; _type: FLOOR_SLUDGE),
    (name: 'SLIME02'; _type: FLOOR_SLUDGE),
    (name: 'SLIME03'; _type: FLOOR_SLUDGE),
    (name: 'SLIME04'; _type: FLOOR_SLUDGE),
    (name: 'SLIME05'; _type: FLOOR_SLUDGE),
    (name: 'SLIME06'; _type: FLOOR_SLUDGE),
    (name: 'SLIME07'; _type: FLOOR_SLUDGE),
    (name: 'SLIME08'; _type: FLOOR_SLUDGE),
    (name: 'SLIME09'; _type: FLOOR_SLUDGE),
    (name: 'SLIME10'; _type: FLOOR_SLUDGE),
    (name: 'SLIME11'; _type: FLOOR_SLUDGE),
    (name: 'SLIME12'; _type: FLOOR_SLUDGE),
    (name: 'NUKAGE1'; _type: FLOOR_NUKAGE),
    (name: 'NUKAGE2'; _type: FLOOR_NUKAGE),
    (name: 'NUKAGE3'; _type: FLOOR_NUKAGE),
    (name: 'END'; _type: -1)
  );

//==============================================================================
//
// P_TerrainTypeForName
//
//==============================================================================
function P_TerrainTypeForName(flatname: string): integer;
var
  i: integer;
begin
  i := 0;
  strupperproc(flatname);
  while terraintypedefs[i]._type <> -1 do
  begin
    if terraintypedefs[i].name = flatname then
    begin
      result := terraintypedefs[i]._type;
      exit;
    end;
    inc(i);
  end;
  result := 0;
end;

end.
