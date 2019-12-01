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
//  Clipper utilities for 3d floors & slopes
//
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit r_cliputils;

interface

uses
  m_fixed,
  r_clipper,
  r_defs;

const
  POINTBITS = 13;
  POINTUNIT = 1 shl POINTBITS;

const
  MAXZ = FRACUNIT * 16384;

function R_MakeClipperPoint(v: Pvertex_t): TIntPoint; overload;

function R_MakeClipperPoint(const x1, y1: fixed_t): TIntPoint; overload;

const
  MAXSUBSECTORPOINTS = 16384;

implementation

uses
  r_main;

function R_MakeClipperPoint(v: Pvertex_t): TIntPoint; overload;
begin
  result.X := (v.x - viewx) div POINTUNIT;
  result.Y := (v.y - viewy)  div POINTUNIT;
end;

function R_MakeClipperPoint(const x1, y1: fixed_t): TIntPoint; overload;
begin
  result.X := (x1  - viewx) div POINTUNIT;
  result.Y := (y1  - viewy) div POINTUNIT;
end;


end.
