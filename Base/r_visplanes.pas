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
//  DESCRIPTION:
//   Visplanes definitions.
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit r_visplanes;

interface

uses
  d_delphi,
  doomdef,
  m_fixed,
  tables;

{$IFNDEF OPENGL}
type
  iheightarray_t = array[0..MAXHEIGHT - 1] of integer;
  Piheightarray_t = ^iheightarray_t;

  siheightarray_t = array[0..MAXHEIGHT - 1] of smallint;
  Psiheightarray_t = ^siheightarray_t;

  visslope_t = record
    miny: integer;
    maxy: integer;
    {$IFDEF DEBUG}
    id: integer;
    slopeviewheight: fixed_t;
    slopeviewZ: fixed_t;
    {$ENDIF}
    screenleft: Psiheightarray_t;
    screenright: Psiheightarray_t;
    ds_zleft: Piheightarray_t;
    ds_zright: Piheightarray_t;
    sectorID: integer;
    virtualfloor: boolean;
  end;
  Pvisslope_t = ^visslope_t;
{$ENDIF}

const
  VISEND = $FFFF;
  iVISEND = integer($FFFFFFFF);

type
  {$IFNDEF OPENGL}
  visindex_t = word;
  Pvisindex_t = ^visindex_t;
  visindex_tArray = packed array[-1..MAXWIDTH] of visindex_t;
  Pvisindex_tArray = ^visindex_tArray;
  {$ENDIF}

//
// Now what is a visplane, anyway?
//
  Pvisplane_t = ^visplane_t;
  visplane_t = packed record
    height: fixed_t;
    picnum: integer;
    lightlevel: integer;
    {$IFDEF HEXEN}
    special: integer;
    {$ENDIF}
    {$IFNDEF OPENGL}
    minx: integer;
    maxx: integer;
    {$ENDIF}
    {$IFDEF DOOM_OR_STRIFE}
    xoffs: fixed_t;
    yoffs: fixed_t;
    {$ENDIF}
    {$IFDEF HEXEN}
    {$IFDEF OPENGL}
    xoffs: fixed_t;
    yoffs: fixed_t;
    {$ENDIF}
    {$ENDIF}
    renderflags: LongWord;
    slopeSID: integer;  // JVAL: Slopes
    angle: angle_t;     // JVAL: 20200221 - Texture angle
    anglex, angley: fixed_t;  // JVAL: 20201229 - Texture angle rover
    {$IFNDEF OPENGL}
    slope: Pvisslope_t; // JVAL: Slopes
    // leave pads for [minx-1] and [maxx+1]
    top: Pvisindex_tArray;    // Now allocated dynamically!
    // See above.
    bottom: Pvisindex_tArray; // Now allocated dynamically!
    {$ENDIF}
  end;

{$IFNDEF OPENGL}
type
  visplane3d_t = packed record
    vis: Pvisplane_t;
    realtop: PSmallIntArray;      // JVAL: 3d Floors
    realbottom: PSmallIntArray;   // JVAL: 3d Floors
    ssector: pointer;
  end;
  Pvisplane3d_t = ^visplane3d_t;
{$ENDIF}

implementation

end.

