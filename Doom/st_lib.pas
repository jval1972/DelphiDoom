//------------------------------------------------------------------------------
//
//  DelphiDoom: A modified and improved DOOM engine for Windows
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 1993-1996 by id Software, Inc.
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

    // pointer to boolean stating
    //  whether to update number
    _on: PBoolean;

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

    // pointer to boolean stating
    //  whether to update icon
    _on: PBoolean;

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

    // pointer to boolean
    //  stating whether to update icon
    _on: PBoolean;


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
  num: PInteger; _on: PBoolean; width: integer);

procedure STlib_updateNum(n: Pst_number_t; transparent: boolean);

// Percent widget routines
procedure STlib_initPercent(p: Pst_percent_t; x, y: integer; pl: Ppatch_tPArray;
  num: PInteger; _on: PBoolean; percent: Ppatch_t);

procedure STlib_updatePercent(per: Pst_percent_t; refresh: boolean; transparent: boolean);

// Multiple Icon widget routines
procedure STlib_initMultIcon(i: Pst_multicon_t; x, y: integer; il: Ppatch_tPArray;
  inum: PInteger; _on: PBoolean);

procedure STlib_updateMultIcon(mi: Pst_multicon_t; refresh: boolean);

// Binary Icon widget routines
procedure STlib_initBinIcon(b: Pst_binicon_t; x, y: integer; i: Ppatch_t;
  val: PBoolean; _on: PBoolean);

procedure STlib_updateBinIcon(bi: Pst_binicon_t; refresh: boolean);

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
  st_stuff;  // automapactive

//
// Hack display negative frags.
//  Loads and store the stminus lump.
//
var
  sttminus: Ppatch_t;

procedure STlib_init;
var
  lump: integer;
begin
  lump := W_CheckNumForName('STTMINUS');
  if lump = -1 then
    oldversion := true;

  if oldsharewareversion or oldversion then
    sttminus := W_CacheLumpName('STCFN046', PU_STATIC)
  else
    sttminus := W_CacheLumpNum(lump, PU_STATIC);
end;

// ?
procedure STlib_initNum(n: Pst_number_t; x, y: integer; pl: Ppatch_tPArray;
  num: PInteger; _on: PBoolean; width: integer);
begin
  n.x := x;
  n.y := y;
  n.oldnum := 0;
  n.width := width;
  n.num := num;
  n._on := _on;
  n.p := pl;
end;

//
// A fairly efficient way to draw a number
//  based on differences from the old number.
// Note: worth the trouble?
//
procedure STlib_drawNum(n: Pst_number_t; transparent: boolean);
var
  numdigits: integer;
  num: integer;
  w: integer;
  h: integer;
  x: integer;
  neg: boolean;
begin
  numdigits := n.width;
  num := n.num^;

  w := n.p[0].width;
  h := n.p[0].height;

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

  // clear the area
  x := n.x - numdigits * w;

  if n.y - ST_Y < 0 then
    I_Error('STlib_drawNum() : n.y - ST_Y < 0');

  if transparent then
    V_CopyRectTransparent(x, n.y - ST_Y, SCN_ST, w * numdigits, h, x, n.y, SCN_FG, true)
  else
    V_CopyRect(x, n.y - ST_Y, SCN_ST, w * numdigits, h, x, n.y, SCN_FG, true);

  // if non-number, do not draw it
  if num = largeammo then
    exit;

  x := n.x;

  // in the special case of 0, you draw 0
  if num = 0 then
    V_DrawPatch(x - w, n.y - ST_Y, SCN_ST, n.p[0], false);

  // draw the new number
  while (num <> 0) and (numdigits <> 0) do
  begin
    x := x - w;
    V_DrawPatch(x, n.y - ST_Y, SCN_ST, n.p[num mod 10], false);
    num := num div 10;
    dec(numdigits);
  end;

  // draw a minus sign if necessary
  if neg then
    V_DrawPatch(x - 8, n.y - ST_Y, SCN_ST, sttminus, false);
end;

//
procedure STlib_updateNum(n: Pst_number_t; transparent: boolean);
begin
  if n._on^ then
    STlib_drawNum(n, transparent);
end;

//
procedure STlib_initPercent(p: Pst_percent_t; x, y: integer; pl: Ppatch_tPArray;
  num: PInteger; _on: PBoolean; percent: Ppatch_t);
begin
  STlib_initNum(@p.n, x, y, pl, num, _on, 3);
  p.p := percent;
end;

procedure STlib_updatePercent(per: Pst_percent_t; refresh: boolean;transparent: boolean);
begin
  if refresh and per.n._on^ then
    V_DrawPatch(per.n.x, per.n.y - ST_Y, SCN_ST, per.p, false);

  STlib_updateNum(@per.n, transparent);
end;

procedure STlib_initMultIcon(i: Pst_multicon_t; x, y: integer; il: Ppatch_tPArray;
  inum: PInteger; _on: PBoolean);
begin
  i.x  := x;
  i.y  := y;
  i.oldinum := -1;
  i.inum := inum;
  i._on := _on;
  i.p := il;
end;

procedure STlib_updateMultIcon(mi: Pst_multicon_t; refresh: boolean);
var
  y: integer;
begin
  if mi._on^ and ((mi.oldinum <> mi.inum^) or refresh) and (mi.inum^ <> -1) then
  begin
    if mi.oldinum <> -1 then
    begin
      y := mi.y - mi.p[mi.oldinum].topoffset;

      if y - ST_Y < 0 then
        I_Error('STlib_updateMultIcon(): y - ST_Y < 0');

    end;
    V_DrawPatch(mi.x, mi.y - ST_Y, SCN_ST, mi.p[mi.inum^], false);
    mi.oldinum := mi.inum^;
  end;
end;

procedure STlib_initBinIcon(b: Pst_binicon_t; x, y: integer; i: Ppatch_t;
  val: PBoolean; _on: PBoolean);
begin
  b.x  := x;
  b.y  := y;
  b.oldval := false;
  b.val := val;
  b._on := _on;
  b.p := i;
end;

procedure STlib_updateBinIcon(bi: Pst_binicon_t; refresh: boolean);
var
  y: integer;
begin
  if bi._on^ and ((bi.oldval <> bi.val^) or refresh) then
  begin
    y := bi.y - bi.p.topoffset;

    if y - ST_Y < 0 then
      I_Error('STlib_updateBinIcon(): y - ST_Y < 0');

    if bi.val^ then
      V_DrawPatch(bi.x, bi.y - ST_Y, SCN_ST, bi.p, false);

    bi.oldval := bi.val^;
  end;
end;

end.

