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

unit libm2m;

interface

{$Z+}
{$H+}

//==============================================================================
//
// _convertm2m
//
//==============================================================================
function _convertm2m(buff: Pointer; buffsize: integer; outbuff: Pointer; outsize: PInteger): integer; cdecl;

implementation

uses
  c_lib;

{$L libm2m\obj\m2m.obj}
{$L libm2m\obj\convert.obj}
{$L libm2m\obj\midimerge.obj}
{$L libm2m\obj\parsemidi.obj}
{$L libm2m\obj\smalloc.obj}

//==============================================================================
//
// _convertm2m
//
//==============================================================================
function _convertm2m(buff: Pointer; buffsize: integer; outbuff: Pointer; outsize: PInteger): integer; cdecl; external;

end.

