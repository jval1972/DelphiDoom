//------------------------------------------------------------------------------
//
//  DelphiStrife: A modified and improved Strife source port for Windows.
//
//  Based on:
//    - Linux Doom by "id Software"
//    - Chocolate Strife by "Simon Howard"
//    - DelphiDoom by "Jim Valavanis"
//
//  Copyright (C) 1993-1996 by id Software, Inc.
//  Copyright (C) 2005 Simon Howard
//  Copyright (C) 2010 James Haley, Samuel Villarreal
//  Copyright (C) 2004-2021 by Jim Valavanis
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
//   The status bar widget code.
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit st_lib;

interface

uses
  d_delphi,
// We are referring to patches.
  r_defs;

type
//
// Typedefs of widgets
//

// Number widget

  st_number_t = record
    // upper right-hand corner
    //  of the number (right-justified)
    x: integer;
    y: integer;

    // max # of digits in number
    width: integer;

    // last number value
    oldnum: integer;

    // pointer to current value
    num: PInteger;

    // list of patches for 0-9
    p: Ppatch_tPArray;

    // user data
    data: integer;

  end;
  Pst_number_t = ^st_number_t;

// Percent widget ("child" of number widget,
//  or, more precisely, contains a number widget.)
  st_percent_t = record
    // number information
    n: st_number_t;

    // percent sign graphic
    p: Ppatch_t;
  end;
  Pst_percent_t = ^st_percent_t;

// Multiple Icon widget
  st_multicon_t = record
    // center-justified location of icons
    x: integer;
    y: integer;

    // last icon number
    oldinum: integer;

    // pointer to current icon
    inum: PInteger;

    // list of icons
    p: Ppatch_tPArray;

    // user data
    data: integer;
  end;
  Pst_multicon_t = ^st_multicon_t;

// Binary Icon widget

  st_binicon_t = record
    // center-justified location of icon
    x: integer;
    y: integer;

    // last icon value
    oldval: boolean;

    // pointer to current icon status
    val: PBoolean;

    p: Ppatch_t;   // icon
    data: integer; // user data
  end;
  Pst_binicon_t = ^st_binicon_t;

//
// Widget creation, access, and update routines
//

// Initializes widget library.
// More precisely, initialize STMINUS,
//  everything else is done somewhere else.
//
procedure STlib_init;

// Number widget routines
procedure STlib_initNum(n: Pst_number_t; x, y: integer; pl: Ppatch_tPArray;
  num: PInteger; width: integer);

procedure STlib_drawNumPositive(n: Pst_number_t; screen: integer; transparent: boolean);

var
  largeammo: integer = 1994; // means "n/a"

implementation

uses
  doomdef,
  z_zone,
  v_data,
  v_video,
  i_system,
  w_wad,
  st_stuff;

//
// Hack display negative frags.
//  Loads and store the stminus lump.
//
var
  sttminus: Ppatch_t;

procedure STlib_init;
begin
  sttminus := W_CacheLumpName('STCFN045', PU_STATIC);
end;

// ?
procedure STlib_initNum(n: Pst_number_t; x, y: integer; pl: Ppatch_tPArray;
  num: PInteger; width: integer);
begin
  n.x := x;
  n.y := y;
  n.oldnum := 0;
  n.width := width;
  n.num := num;
  n.p := pl;
end;

//
// A fairly efficient way to draw a number
//  based on differences from the old number.
// Note: worth the trouble?
//
procedure STlib_drawNum(n: Pst_number_t; screen: integer; transparent: boolean);
var
  numdigits: integer;
  num: integer;
  w: integer;
  x: integer;
  neg: boolean;
begin
  numdigits := n.width;
  num := n.num^;

  w := n.p[0].width;

  n.oldnum := num;

  neg := num < 0;

  if neg then
  begin
    if (numdigits = 2) and (num < -9) then
      num := -9
    else if (numdigits = 3) and (num < -99) then
      num := -99;

    num := -num;
  end;

  // if non-number, do not draw it
  if num = largeammo then
    exit;

  x := n.x;

  // in the special case of 0, you draw 0
  if num = 0 then
    V_DrawPatch(x - w, n.y, screen, n.p[0], screen = SCN_FG);

  // draw the new number
  while (num <> 0) and (numdigits <> 0) do
  begin
    x := x - w;
    V_DrawPatch(x, n.y, screen, n.p[num mod 10], screen = SCN_FG);
    num := num div 10;
    dec(numdigits);
  end;

  // draw a minus sign if necessary
  if neg then
    V_DrawPatch(x - 8, n.y, screen, sttminus, screen = SCN_FG);
end;

procedure STlib_drawNumPositive(n: Pst_number_t; screen: integer; transparent: boolean);
var
  zero: st_number_t;
  n1: integer;
begin
  if n.num^ >= 0 then
    STlib_drawNum(n, screen, transparent)
  else
  begin
    zero := n^;
    n1 := 0;
    zero.num := @n1;
    STlib_drawNum(@zero, screen, transparent);
  end;
end;

end.

