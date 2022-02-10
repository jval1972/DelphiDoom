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

unit p_affectees;

interface

//==============================================================================
//
// P_SetupSectorAffectees
//
//==============================================================================
procedure P_SetupSectorAffectees;

implementation

uses
  d_delphi,
  p_setup,
  r_defs,
  z_zone;

//==============================================================================
//
// P_SetupSectorAffectees
//
//==============================================================================
procedure P_SetupSectorAffectees;
var
  l: TDStringList;
  n: TDNumberList;
  i, j: integer;
  sec: Psector_t;
begin
  l := TDStringList.Create;
  for i := 0 to numsectors - 1 do
  begin
    n := TDNumberList.Create;
    n.Add(i);
    l.AddObject('', n);
  end;

  for i := 0 to numsectors - 1 do
  begin
    sec := @sectors[i];
    if sec.midsec >= 0 then
    begin
      n := l.Objects[sec.midsec] as TDNumberList;
      if n.IndexOf(i) < 0 then
        n.Add(i);
    end;
    if sec.slopesec <> nil then
    begin
      n := l.Objects[sec.slopesec.iSectorID] as TDNumberList;
      if n.IndexOf(i) < 0 then
        n.Add(i);
    end;
  end;

  for i := 0 to numsectors - 1 do
  begin
    sec := @sectors[i];
    n := l.Objects[i] as TDNumberList;
    sec.num_saffectees := n.Count;
    sec.saffectees := Z_Realloc(sec.saffectees, n.Count * SizeOf(Integer), PU_LEVEL, nil);
    for j := 0 to n.Count - 1 do
      sec.saffectees[j] := n.Numbers[j];
  end;

  for i := 0 to l.Count - 1 do
    l.Objects[i].Free;
  l.Free;
end;

end.
