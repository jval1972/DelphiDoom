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
//  Ladder Move (climp ladder)
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit p_ladder;

interface

uses
  m_fixed,
  p_mobj_h;

const
  MAXLADDERMOVE = 4 * FRACUNIT;
  MINLADDERMOVE = FRACUNIT * 3 div 2;
  LADDERCOUNTDOWN = 5;

//==============================================================================
//
// P_LadderMove
//
//==============================================================================
function P_LadderMove(mo: Pmobj_t): boolean;

implementation

uses
  r_defs,
  p_common,
  p_maputl,
  d_player;

//==============================================================================
//
// P_LadderMove
//
//==============================================================================
function P_LadderMove(mo: Pmobj_t): boolean;
var
  player: Pplayer_t;
  topz: fixed_t;
  sec: Psector_t;
  other: Psector_t;
  i: integer;
  l: Pline_t;
begin
  player := mo.player;
  if player = nil then
  begin
    Result := false;
    exit;
  end;

  sec := Psubsector_t(mo.subsector).sector;
  if sec.flags and SF_LADDER <> 0 then
  begin
    topz := sec.floorheight;
    for i := 0 to sec.linecount - 1 do
    begin
      l := sec.lines[i];
      if l.frontsector = sec then
        other := l.backsector
      else
        other := l.frontsector;
      if other <> nil then
        if other.floorheight > topz then
          topz := other.floorheight;
    end;
    if mo.z < topz then
      if player.cmd.forwardmove > 0 then
      begin
        mo.momz := P_AproxDistance(mo.momx, mo.momy);
        if mo.momz > MAXLADDERMOVE then
          mo.momz := MAXLADDERMOVE;
        if mo.momz < MINLADDERMOVE then
          mo.momz := MINLADDERMOVE;
        player.laddertics := LADDERCOUNTDOWN;
        result := True;
        exit;
      end;
  end;

  result := false;
end;

end.
