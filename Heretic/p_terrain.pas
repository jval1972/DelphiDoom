//------------------------------------------------------------------------------
//
//  DelphiHeretic is a source port of the game Heretic and it is
//  based on original Linux Doom as published by "id Software", on
//  Heretic source as published by "Raven" software and DelphiDoom
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

{function P_TerrainTypeForNum(flatnum: integer): integer;

//==============================================================================
//
// P_InitTerrainTypes
//
//==============================================================================
procedure P_InitTerrainTypes;}

implementation

uses
  d_delphi,
  p_local;

type
  terraintypedef_t = record
    name: string;
    lump: integer;
    _type: integer;
  end;

var
  terraintypedefs: array[0..5] of terraintypedef_t = (
    (name: 'FLTWAWA1'; lump: -1; _type: FLOOR_WATER),
    (name: 'FLTFLWW1'; lump: -1; _type: FLOOR_WATER),
    (name: 'FLTLAVA1'; lump: -1; _type: FLOOR_LAVA),
    (name: 'FLATHUH1'; lump: -1; _type: FLOOR_LAVA),
    (name: 'FLTSLUD1'; lump: -1; _type: FLOOR_SLUDGE),
    (name: 'END'; lump: -1; _type: -1)
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
  flatname := strupper(flatname);
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
{

//==============================================================================
//
// P_TerrainTypeForNum
//
//==============================================================================
function P_TerrainTypeForNum(flatnum: integer): integer;
var
  i: integer;
  lump: integer;
begin
  i := 0;
  lump := flats[flatnum].lump;
  while terraintypedefs[i]._type <> -1 do
  begin
    if terraintypedefs[i].lump = lump then
    begin
      result := terraintypedefs[i]._type;
      exit;
    end;
    inc(i);
  end;
  result := 0;
end;

//----------------------------------------------------------------------------
//
// PROC P_InitTerrainTypes
//
//----------------------------------------------------------------------------
//
//==============================================================================
procedure P_InitTerrainTypes;
var
  i: integer;
begin
  i := 0;
  while terraintypedefs[i]._type <> -1 do
  begin
    terraintypedefs[i].lump := W_CheckNumForName(terraintypedefs[i].name);
    inc(i);
  end;
end;
}
end.
