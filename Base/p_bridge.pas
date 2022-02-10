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
//  Bridge stuff
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit p_bridge;

interface

//==============================================================================
//
// P_CalcSubSectorsBridge
//
//==============================================================================
procedure P_CalcSubSectorsBridge;

implementation

uses
  r_defs,
  p_setup;

//==============================================================================
//
// P_CalcSubSectorBridge
//
//==============================================================================
procedure P_CalcSubSectorBridge(const ss: Psubsector_t);
var
  i: integer;
  numsr: integer; // Number of lines that have both sides at the same sector
  seg: Pseg_t;
begin
  numsr := 0;
  for i := ss.firstline to ss.firstline + ss.numlines - 1 do
  begin
    seg := @segs[i];
    if seg.miniseg then
      Continue;
    if seg.linedef.frontsector = seg.linedef.backsector then
      inc(numsr)
    else if (seg.linedef.frontsector <> nil) and (seg.linedef.backsector <> nil) then
      exit; // 2s line encountered
  end;
  if numsr > 0 then
    ss.flags := ss.flags or SSF_BRIDGE;
end;

//==============================================================================
//
// P_CalcSubSectorsBridge
//
//==============================================================================
procedure P_CalcSubSectorsBridge;
var
  i: integer;
begin
  for i := 0 to numsubsectors - 1 do
    P_CalcSubSectorBridge(@subsectors[i]);
end;

end.
