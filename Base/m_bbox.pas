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
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit m_bbox;

interface

uses
  m_fixed;

// Bounding box coordinate storage.
const
  BOXTOP = 0;
  BOXBOTTOM = 1;
  BOXLEFT = 2;
  BOXRIGHT = 3;

// Bounding box functions.
procedure M_ClearBox(box: Pfixed_tArray);

procedure M_AddToBox(box: Pfixed_tArray; x: fixed_t; y: fixed_t);


implementation

uses
  doomtype;

procedure M_ClearBox(box: Pfixed_tArray);
begin
  box[BOXTOP] := MININT;
  box[BOXRIGHT] := MININT;
  box[BOXBOTTOM] := MAXINT;
  box[BOXLEFT] := MAXINT;
end;

procedure M_AddToBox(box: Pfixed_tArray; x: fixed_t; y: fixed_t);
begin
  if x < box[BOXLEFT] then
    box[BOXLEFT] := x
  else if x > box[BOXRIGHT] then
    box[BOXRIGHT] := x;
  if y < box[BOXBOTTOM] then
    box[BOXBOTTOM] := y
  else if y > box[BOXTOP] then
    box[BOXTOP] := y;
end;

end.
