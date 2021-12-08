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
//  Refresh module, data I/O, caching, retrieval of graphics
//  by name.
//  Preparation of data for rendering,
//  generation of lookups, caching, retrieval by name.
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit r_palette;

interface

uses
  d_delphi;

type
  palette_t = array[0..255] of LongWord;
  palette_p = ^palette_t;

function R_DefaultPalette: palette_p;

implementation

uses
  v_data,
  z_zone;

var
  dpal: palette_t;
  flag: boolean = false;

function R_DefaultPalette: palette_p;
var
  palette: PByteArray;
  src: PByteArray;
  dest: PLongWord;
begin
  if flag then
  begin
    result := @dpal;
    exit;
  end;

  palette := V_ReadPalette(PU_STATIC);

  dest := @dpal[0];
  src := palette;
  while integer(src) < integer(@palette[256 * 3]) do
  begin
    dest^ := (LongWord(src[0]) shl 16) or
             (LongWord(src[1]) shl 8) or
             (LongWord(src[2]));
    inc(dest);
    src := PByteArray(integer(src) + 3);
  end;
  Z_ChangeTag(palette, PU_CACHE);

  flag := True;
  result := @dpal;
end;

end.
