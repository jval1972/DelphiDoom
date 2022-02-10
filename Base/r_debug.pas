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
//  Rendering debug
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit r_debug;

interface

{$IFDEF DEBUG}
uses
  d_delphi,
  r_visplanes,
  r_defs;

//==============================================================================
//
// R_DebugCheckVisPlane
//
//==============================================================================
procedure R_DebugCheckVisPlane(const pl: Pvisplane_t);

//==============================================================================
//
// R_CheckClipTable
//
//==============================================================================
procedure R_CheckClipTable(const tbl: PSmallIntArray; const a1, a2: integer);

//==============================================================================
//
// R_CheckClipItem
//
//==============================================================================
procedure R_CheckClipItem(const x: PSmallInt);
{$ENDIF}

implementation

{$IFDEF DEBUG}
uses
  i_system,
  r_draw;

//==============================================================================
//
// R_DebugCheckVisPlane
//
//==============================================================================
procedure R_DebugCheckVisPlane(const pl: Pvisplane_t);
var
  i: integer;
begin
  for i := pl.minx - 1 to pl.maxx + 1 do
  begin
    if pl.top[i] > viewheight then
      if pl.top[i] <> VISEND then
      begin
        I_Warning('R_DebugCheckVisPlane(): Invalid visplane'#13#10);
        exit;
      end;
    if pl.bottom[i] > viewheight then
      if pl.bottom[i] <> VISEND then
      begin
        I_Warning('R_DebugCheckVisPlane(): Invalid visplane'#13#10);
        exit;
      end;
  end;
end;

//==============================================================================
//
// R_CheckClipTable
//
//==============================================================================
procedure R_CheckClipTable(const tbl: PSmallIntArray; const a1, a2: integer);
var
  i: integer;
begin
  if a1 < 0 then
  begin
    I_Warning('R_CheckClipTable(): Invalid cliptable'#13#10);
    exit;
  end;

  if a2 >= viewwidth then
  begin
    I_Warning('R_CheckClipTable(): Invalid cliptable'#13#10);
    exit;
  end;

  for i := a1 to a2 do
    if tbl[i] > viewheight then
    begin
      I_Warning('R_CheckClipTable(): Invalid cliptable'#13#10);
      exit;
    end;
end;

//==============================================================================
//
// R_CheckClipItem
//
//==============================================================================
procedure R_CheckClipItem(const x: PSmallInt);
begin
  if x^ > viewheight then
    I_Warning('R_CheckClipItem(): Invalid clipitem'#13#10);
end;

{$ENDIF}
end.
