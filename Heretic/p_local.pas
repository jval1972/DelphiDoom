//------------------------------------------------------------------------------
//
//  DelphiHeretic is a source port of the game Heretic and it is
//  based on original Linux Doom as published by "id Software", on
//  Heretic source as published by "Raven" software and DelphiDoom
//  as published by Jim Valavanis.
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
//  Play functions, animation, global header.
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit p_local;

interface

uses
  doomtype,
  m_fixed,
  p_mobj_h,
  r_defs;

const
  TOCENTER = -8;
  TOFORWARD = -8;

  FLOATSPEED = FRACUNIT * 4;

// Player VIEWHEIGHT
  PVIEWHEIGHT = 41 * FRACUNIT;

// JVAL: 20211101 - Crouch
  PMAXCROUCHHEIGHT = 18 * FRACUNIT;

// mapblocks are used to check movement
// against lines and things
  MAPBLOCKUNITS = 128;
  MAPBLOCKSIZE = MAPBLOCKUNITS * FRACUNIT;

  MAPBLOCKSHIFT = FRACBITS + 7;
  MAPBMASK = MAPBLOCKSIZE - 1;
  MAPBTOFRAC = MAPBLOCKSHIFT - FRACBITS;

// MAXRADIUS is for precalculated sector block boxes
// the spider demon is larger,
// but we do not have any moving sectors nearby
  MAXRADIUS = 32 * FRACUNIT;

  GRAVITY = FRACUNIT;
  MAXMOVE = 30 * FRACUNIT;

  USERANGEINT = 64;
  USERANGE = USERANGEINT * FRACUNIT;
  USETHINGRANGEINT = 96;
  USETHINGRANGE = USETHINGRANGEINT * FRACUNIT;
  MELEERANGE = 64 * FRACUNIT;
  MISSILERANGE = (32 * 64) * FRACUNIT;

// follow a player exlusively for 3 seconds
  BASETHRESHOLD = 100;

const
  FLOOR_SOLID = 0;
  FLOOR_WATER = 1;
  FLOOR_LAVA = 2;
  FLOOR_SLUDGE = 3;

const
  STARTREDPALS = 1;
  STARTBONUSPALS = 9;
  NUMREDPALS = 8;
  NUMBONUSPALS = 4;

const
  ONFLOORZ = MININT;
  ONCEILINGZ = MAXINT;
  FLOATRANDZ = MAXINT - 1;
  ONFLOATZ = MAXINT - 2;

type
  divline_t = record
    x: fixed_t;
    y: fixed_t;
    dx: fixed_t;
    dy: fixed_t;
  end;
  Pdivline_t = ^divline_t;
  divline_tArray = array[0..$FFFF] of divline_t;
  Pdivline_tArray = ^divline_tArray;

  thingORline_t = record
    case integer of
      0: (thing: Pmobj_t);
      1: (line: Pline_t);
    end;

  intercept_t = record
    frac: fixed_t; // along trace line
    isaline: boolean;
    d: thingORline_t;
  end;
  Pintercept_t = ^intercept_t;
  intercept_tArray = array[0..$FFFF] of intercept_t;
  Pintercept_tArray = ^intercept_tArray;

const
  MAXINTERCEPTS = 128;

type
  traverser_t = function(f: Pintercept_t): boolean;
  ltraverser_t = function(p: Pline_t): boolean;
  ttraverser_t = function(p: Pmobj_t): boolean;

const
  PT_ADDLINES = 1;
  PT_ADDTHINGS = 2;
  PT_EARLYOUT = 4;

const
  FOOTCLIPSIZE = 10 * FRACUNIT;

//==============================================================================
//
// MapBlockInt
//
//==============================================================================
function MapBlockInt(const x: integer): integer;

//==============================================================================
//
// MapBlockIntX
//
//==============================================================================
function MapBlockIntX(const x: int64): integer;

//==============================================================================
//
// MapBlockIntY
//
//==============================================================================
function MapBlockIntY(const y: int64): integer;

//==============================================================================
//
// MapToFrac
//
//==============================================================================
function MapToFrac(const x: integer): integer;

//==============================================================================
//
// HITDICE
//
//==============================================================================
function HITDICE(a: integer): integer;

implementation

uses
  m_rnd,
  p_setup;

//==============================================================================
//
// MapBlockInt
//
//==============================================================================
function MapBlockInt(const x: integer): integer; assembler;
asm
  sar eax, MAPBLOCKSHIFT
end;

//==============================================================================
//
// MapBlockIntX
//
//==============================================================================
function MapBlockIntX(const x: int64): integer;
begin
  result := x shr MAPBLOCKSHIFT;
  if result <= blockmapxneg then
    result := result and $1FF;
end;

//==============================================================================
//
// MapBlockIntY
//
//==============================================================================
function MapBlockIntY(const y: int64): integer;
begin
  result := y shr MAPBLOCKSHIFT;
  if result <= blockmapyneg then
    result := result and $1FF;
end;

//==============================================================================
//
// MapToFrac
//
//==============================================================================
function MapToFrac(const x: integer): integer; assembler;
asm
  sar eax, MAPBTOFRAC
end;

//==============================================================================
//
// HITDICE
//
//==============================================================================
function HITDICE(a: integer): integer;
begin
  result :=  (1 + (P_Random and 7)) * a;
end;

end.

