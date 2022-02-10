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
//  vislight_t struct
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit r_vislight;

interface

uses
  m_fixed,
  p_mobj_h;

type
  Pvislight_t = ^vislight_t;
  vislight_t = record
    x1: integer;
    x2: integer;
    mo: Pmobj_t;

    // for line side calculation
    gx: fixed_t;
    gy: fixed_t;

    // global bottom / top for silhouette clipping
    gz: fixed_t;

    // horizontal position of x1
    startfrac: fixed_t;

    scale: fixed_t;
    xiscale: fixed_t;

    dbmin: LongWord;
    dbmax: LongWord;
    dbdmin: LongWord;
    dbdmax: LongWord;

    texturemid: fixed_t;

    color32: LongWord;
  end;

const
  MAXVISLIGHTS = 1024;

var
  vislight_p: integer = 0;
  vislights: array[0..MAXVISLIGHTS - 1] of vislight_t;

//==============================================================================
//
// R_NewVisLight
//
//==============================================================================
function R_NewVisLight: Pvislight_t;

implementation

//==============================================================================
//
// R_NewVisLight
//
//==============================================================================
function R_NewVisLight: Pvislight_t;
begin
  if vislight_p = MAXVISLIGHTS then
    result := @vislights[MAXVISLIGHTS - 1]
  else
  begin
    result := @vislights[vislight_p];
    inc(vislight_p);
  end;
end;

end.

