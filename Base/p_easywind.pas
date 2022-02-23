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
//  Easy sector wind
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit p_easywind;

interface

const
  MT_WIND_5 = 1160;
  MT_WIND_10 = 1161;
  MT_WIND_15 = 1162;
  MT_WIND_20 = 1163;
  MT_WIND_25 = 1164;

//==============================================================================
//
// P_AdjustEasyWind
//
//==============================================================================
procedure P_AdjustEasyWind;

implementation

uses
  d_delphi,
  d_think,
  p_mobj,
  p_mobj_h,
  p_tick,
  r_defs;

//==============================================================================
//
// P_AdjustEasyWind
//
//==============================================================================
procedure P_AdjustEasyWind;
const
  EASYWINDTAB: array[0..4] of integer = (2048 * 5, 2048 * 10, 2048 * 15, 2048 * 20, 2048 * 25);
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
      if IsIntegerInRange(mo.spawnpoint._type, MT_WIND_5, MT_WIND_25) then
      begin
        Psubsector_t(mo.subsector).sector.windthrust := EASYWINDTAB[mo.spawnpoint._type - MT_WIND_5];
        Psubsector_t(mo.subsector).sector.windangle := mo.angle;
      end;
    end;
    th := th.next;
  end;
end;

end.

