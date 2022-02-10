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
//  Easy floor and ceiling texture rotation
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit p_easyangle;

interface

const
  MT_FLOORTOANGLE = 1158;
  MT_CEILINGTOANGLE = 1159;

//==============================================================================
//
// P_AdjustEasyAngle
//
//==============================================================================
procedure P_AdjustEasyAngle;

implementation

uses
  d_think,
  p_mobj,
  p_mobj_h,
  p_tick,
  r_defs;

//==============================================================================
//
// P_AdjustEasyAngle
//
//==============================================================================
procedure P_AdjustEasyAngle;
var
  th: Pthinker_t;
  mo: Pmobj_t;
begin
  th := thinkercap.next;
  while th <> @thinkercap do
  begin
    if @th._function.acp1 = @P_MobjThinker then
    begin
      mo := Pmobj_t(th);
      if mo.spawnpoint._type = MT_FLOORTOANGLE then
      begin
        Psubsector_t(mo.subsector).sector.floorangle := mo.angle;
        Psubsector_t(mo.subsector).sector.flooranglex := mo.x;
        Psubsector_t(mo.subsector).sector.floorangley := mo.y;
      end
      else if mo.spawnpoint._type = MT_CEILINGTOANGLE then
      begin
        Psubsector_t(mo.subsector).sector.ceilingangle := mo.angle;
        Psubsector_t(mo.subsector).sector.ceilinganglex := mo.x;
        Psubsector_t(mo.subsector).sector.ceilingangley := mo.y;
      end;
    end;
    th := th.next;
  end;
end;

end.
