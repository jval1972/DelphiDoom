//------------------------------------------------------------------------------
//
//  DelphiDoom: A modified and improved DOOM engine for Windows
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 2004-2016 by Jim Valavanis
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
//  Range struct definition and funcs.
//
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit r_range;

interface

type
//
// midsiderange_t // JVAL 3d Floors
//
  midsiderange_t = record
    count: integer;
    floorheight: array[0..1] of integer;
    ceilingheight: array[0..1] of integer;
    lightlevel: array[0..1] of integer;
  end;
  Pmidsiderange_t = ^midsiderange_t;

procedure R_SubtractRange(const floorheight1, ceilingheight1, lightlevel1: integer;
                          const floorheight2, ceilingheight2, lightlevel2: integer;
                          const r: Pmidsiderange_t; var totalclip: boolean);

implementation

uses
  d_delphi;

// Subtract range ceil2 - floor2 from range ceil1 - floor1
// Note floor is greater from ceiling :)
procedure R_SubtractRange(const floorheight1, ceilingheight1, lightlevel1: integer;
                          const floorheight2, ceilingheight2, lightlevel2: integer;
                          const r: Pmidsiderange_t; var totalclip: boolean);
begin
  r.floorheight[0] := MinI(ceilingheight2, floorheight1);
  r.ceilingheight[0] := ceilingheight1;
  r.lightlevel[0] := lightlevel1;
  if ceilingheight2 > floorheight2 then
  begin
    r.floorheight[1] := 0;
    r.ceilingheight[1] := 1;
    r.lightlevel[1] := lightlevel1;
    totalclip := true;
  end
  else
  begin
    r.floorheight[1] := floorheight1;
    r.ceilingheight[1] := MaxI(ceilingheight1, floorheight2);
    r.lightlevel[1] := lightlevel2;
    totalclip := r.ceilingheight[1] > r.floorheight[1];
  end;
  r.count := 2;
end;

end.
