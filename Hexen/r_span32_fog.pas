//------------------------------------------------------------------------------
//
//  DelphiHexen is a source port of the game Hexen and it is
//  based on original Linux Doom as published by "id Software", on
//  Hexen source as published by "Raven" software and DelphiDoom
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
//  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
//  02111-1307, USA.
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit r_span32_fog;

interface

//==============================================================================
//
// R_DrawSpanNormal_Fog
//
//==============================================================================
procedure R_DrawSpanNormal_Fog;

implementation
{$DEFINE FOG}

uses
  d_delphi,
  m_fixed,
  r_precalc,
  r_flatinfo,
  r_span,
  r_span32,
  r_draw,
  r_hires,
  r_grow,
  v_video;

//==============================================================================
// R_DrawSpanNormal_Fog
//
// Draws the actual span (Normal resolution).
//
//==============================================================================
procedure R_DrawSpanNormal_Fog;
var
  xfrac: fixed_t;
  yfrac: fixed_t;
  xstep: fixed_t;
  ystep: fixed_t;
  destl: PLongWord;
  count: integer;
  i: integer;
  spot: integer;

  c: LongWord;
  lfactor: integer;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
begin
  destl := @((ylookupl[ds_y]^)[columnofs[ds_x1]]);

  // We do not check for zero spans here?
  count := ds_x2 - ds_x1;
  if count < 0 then
    exit;

  lfactor := ds_lightlevel;
  if lfactor >= 0 then // Use hi detail lightlevel
  begin
    R_GetFogPrecalc32Tables(lfactor, bf_r, bf_g, bf_b);
    {$UNDEF RIPPLE}
    {$I R_DrawSpanNormalFog.inc}
  end
  else // Use inversecolormap
  begin
    {$UNDEF RIPPLE}
    {$I R_DrawSpanNormalFog.inc}
  end;
end;

end.

