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

unit p_terrain;

interface

//==============================================================================
//
// P_TerrainTypeForName
//
//==============================================================================
function P_TerrainTypeForName(flatname: string): integer;

implementation

uses
  d_delphi,
  p_local;

type
  terraintypedef_t = record
    name: string;
    _type: integer;
  end;

var
  terraintypedefs: array[0..4] of terraintypedef_t = (
    (name: 'X_005'; _type: FLOOR_WATER),
    (name: 'X_001'; _type: FLOOR_LAVA),
    (name: 'X_009'; _type: FLOOR_SLUDGE),
    (name: 'F_033'; _type: FLOOR_ICE),
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
  flatname := strupper(flatname);
  i := 0;
  while terraintypedefs[i]._type <> -1 do
  begin
    if terraintypedefs[i].name = flatname then
    begin
      result := terraintypedefs[i]._type;
      exit;
    end;
    inc(i);
  end;
  result := FLOOR_SOLID;
end;

end.
