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
//  Change display mode
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit v_displaymode;

interface

//==============================================================================
//
// V_SetDisplayMode
//
//==============================================================================
function V_SetDisplayMode(const newwidth, newheight: integer): boolean;

//==============================================================================
//
// V_DoSetDisplayMode
//
//==============================================================================
function V_DoSetDisplayMode(const newwidth, newheight: integer): boolean;

implementation

uses
  am_map,
  doomdef,
  c_con,
  mt_utils,
  i_video,
  psi_overlay,
  r_main,
  r_depthbuffer,
  r_cache_main,
  r_slopes,
  r_3dfloors,
  r_col_fz,
  r_fake3d,
  r_things,
  r_plane,
  v_video;

//==============================================================================
//
// V_SetDisplayMode
//
//==============================================================================
function V_SetDisplayMode(const newwidth, newheight: integer): boolean;
var
  nwidth, nheight: integer;
begin
  result := false;

  if (SCREENWIDTH = newwidth) and (SCREENHEIGHT = newheight) then
    exit;

  nwidth := newwidth and not 1;
  if nwidth > MAXWIDTH then
    nwidth := MAXWIDTH
  else if nwidth < MINWIDTH then
    nwidth := MINWIDTH;

  nheight := newheight and not 1;
  if nheight > MAXHEIGHT then
    nwidth := MAXHEIGHT
  else if nheight < MINHEIGHT then
    nheight := MINHEIGHT;

  if nheight > nwidth then
    nheight := nwidth;

  if (SCREENWIDTH <> nwidth) or (SCREENHEIGHT <> nheight) then
    result := V_DoSetDisplayMode(nwidth, nheight);
end;

//==============================================================================
//
// V_DoSetDisplayMode
//
//==============================================================================
function V_DoSetDisplayMode(const newwidth, newheight: integer): boolean;
begin
  MT_WaitTasks;            // Wait for running tasks to stop
  R_ShutDownDepthBuffer;   // Shut down depthbuffer
  R_ClearVisPlanes;        // Clear visplanes (free ::top & ::bottom arrays)
  R_ClearVisSlopes;        // Clear vissplopes (free screenleft, screenright, ds_zleft & ds_zright arrays)
  R_ClearVisPlanes3d;      // Clear arrays
  R_ShutDownFake3D;        // Clear fake 3d planes
  R_Clear32Cache;          // JVAL: unneeded ?
  AM_Stop;                 // Stop the automap

  I_ShutDownGraphics;      // Shut down graphics

  SCREENWIDTH := newwidth;
  SCREENHEIGHT := newheight;

  V_ReInit;                // Recreate screens

  I_InitGraphics;          // Initialize graphics

  AM_Start;                // Start the automap
  C_AdjustScreenSize;      // Notify console for screen resolution change
  setsizeneeded := true;   // Set-up new SCREENWIDTH & SCREENHEIGHT
  R_InitDepthBuffer;       // Initialize the depth-buffer
  R_InitFake3D;            // Initialize fake 3d
  {$IFNDEF STRIFE}
  R_InitFuzzTable;         // Re-calculate fuzz tabble offsets
  {$ENDIF}
  R_InitNegoArray;         // Re-calculate the nego-array
  overlay.ReCalcOverlayLookUp;
  result := true;
end;

end.

