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

unit sc_globals;

interface

//==============================================================================
//
// SC_InitGlobals
//
//==============================================================================
procedure SC_InitGlobals;

//==============================================================================
//
// SC_ShutDownGlobals
//
//==============================================================================
procedure SC_ShutDownGlobals;

//==============================================================================
//
// SC_AddGlobalPrecalc
//
//==============================================================================
procedure SC_AddGlobalPrecalc(const name: string);

implementation

uses
  d_delphi;

const
  NUM_SCGLOBALLISTS = 64;
var
  scgloballists: array[0..NUM_SCGLOBALLISTS - 1] of TDStringList;

//==============================================================================
//
// SC_InitGlobals
//
//==============================================================================
procedure SC_InitGlobals;
var
  i: integer;
begin
  for i := 0 to NUM_SCGLOBALLISTS - 1 do
    scgloballists[i] := TDStringList.Create;
end;

//==============================================================================
//
// SC_ShutDownGlobals
//
//==============================================================================
procedure SC_ShutDownGlobals;
var
  i: integer;
begin
  for i := 0 to NUM_SCGLOBALLISTS - 1 do
    scgloballists[i].Free;
end;

//==============================================================================
//
// SC_AddGlobalPrecalc
//
//==============================================================================
procedure SC_AddGlobalPrecalc(const name: string);
begin
end;

end.

