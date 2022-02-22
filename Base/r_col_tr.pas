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

{$I Doom32.inc}

unit r_col_tr;

interface

//==============================================================================
// R_DrawTranslatedColumn
//
// Draw with color translation tables,
//  for player sprite rendering,
//  Green/Red/Blue/Indigo shirts.
//
//==============================================================================
procedure R_DrawTranslatedColumn;

//==============================================================================
//
// R_DrawTranslatedColumnHi
//
//==============================================================================
procedure R_DrawTranslatedColumnHi;

implementation

uses
  d_delphi,
  doomdef,
  m_fixed,
  r_draw,
  r_main,
  r_column;

//==============================================================================
//
// R_DrawTranslatedColumn
// Used to draw player sprites
//  with the green colorramp mapped to others.
// Could be used with different translation
//  tables, e.g. the lighter colored version
//  of the BaronOfHell, the HellKnight, uses
//  identical sprites, kinda brightened up.
//
//==============================================================================
procedure R_DrawTranslatedColumn;
var
  count: integer;
  dest: PByte;
  frac: fixed_t;
  fracstep: fixed_t;
  i: integer;
begin
  count := dc_yh - dc_yl;

  if count < 0 then
    exit;

  // FIXME. As above.
  dest := @((ylookup[dc_yl]^)[columnofs[dc_x]]);

  // Looks familiar.
  fracstep := dc_iscale;
  frac := dc_texturemid + (dc_yl - centery) * fracstep;

  // Here we do an additional index re-mapping.
  for i := 0 to count do
  begin
    // Translation tables are used
    //  to map certain colorramps to other ones,
    //  used with PLAY sprites.
    // Thus the "green" ramp of the player 0 sprite
    //  is mapped to gray, red, black/indigo.
    dest^ := dc_colormap[dc_translation[dc_source[(LongWord(frac) shr FRACBITS) and 127]]];
    inc(dest, SCREENWIDTH);

    inc(frac, fracstep);
  end;
end;

//==============================================================================
//
// R_DrawTranslatedColumnHi
//
//==============================================================================
procedure R_DrawTranslatedColumnHi;
var
  count: integer;
  destl: PLongWord;
  frac: fixed_t;
  fracstep: fixed_t;
  i: integer;
  swidth: integer;
begin
  count := dc_yh - dc_yl;

  if count < 0 then
    exit;

  // FIXME. As above.
  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);

  // Looks familiar.
  fracstep := dc_iscale;
  frac := dc_texturemid + (dc_yl - centery) * fracstep;

  swidth := SCREENWIDTH32PITCH;
  // Here we do an additional index re-mapping.
  for i := 0 to count do
  begin
    // Translation tables are used
    //  to map certain colorramps to other ones,
    //  used with PLAY sprites.
    // Thus the "green" ramp of the player 0 sprite
    //  is mapped to gray, red, black/indigo.
    destl^ := dc_colormap32[dc_translation[dc_source[(LongWord(frac) shr FRACBITS) and 127]]];
    destl := PLongWord(integer(destl) + swidth);

    inc(frac, fracstep);
  end;
end;

end.
