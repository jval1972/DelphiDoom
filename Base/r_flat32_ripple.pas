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
//  Multithreading flat rendering - 32 bit color (ripple)
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit r_flat32_ripple;

interface

procedure R_DrawSpanNormal_RippleMT(const fi: pointer);

implementation

uses
  d_delphi,
  m_fixed,
  r_draw,
  r_main,
  r_precalc,
  r_ripple,
  r_flatinfo,
  r_span,
  r_span32,
  r_flat32;

procedure R_DrawSpanNormal_RippleMT(const fi: pointer);
var
  ds_source32: PLongWordArray;
  ds_y, ds_x1, ds_x2: integer;
  ds_xfrac: fixed_t;
  ds_yfrac: fixed_t;
  ds_xstep: fixed_t;
  ds_ystep: fixed_t;
  ds_scale: dsscale_t;
  ds_size: integer; // JVAL: 20211112 - Fix size bug
  ds_fog: boolean;
  xfrac: fixed_t;
  yfrac: fixed_t;
  xstep: fixed_t;
  ystep: fixed_t;
  destl: PLongWord;
  count: integer;
  i: integer;
  spot: integer;

  r1, g1, b1: byte;
  c: LongWord;
  lfactor: integer;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  rpl: PIntegerArray;
begin
  ds_source32 := Pflatrenderinfo32_t(fi).ds_source32;
  ds_y := Pflatrenderinfo32_t(fi).ds_y;
  ds_x1 := Pflatrenderinfo32_t(fi).ds_x1;
  ds_x2 := Pflatrenderinfo32_t(fi).ds_x2;
  ds_xfrac := Pflatrenderinfo32_t(fi).ds_xfrac;
  ds_yfrac := Pflatrenderinfo32_t(fi).ds_yfrac;
  ds_xstep := Pflatrenderinfo32_t(fi).ds_xstep;
  ds_ystep := Pflatrenderinfo32_t(fi).ds_ystep;
  ds_scale := Pflatrenderinfo32_t(fi).ds_scale;
  ds_size := Pflatrenderinfo32_t(fi).ds_size;
  ds_fog := Pflatrenderinfo32_t(fi).ds_fog;

  destl := @((ylookupl[ds_y]^)[columnofs[ds_x1]]);

  // We do not check for zero spans here?
  count := ds_x2 - ds_x1;

  rpl := Pflatrenderinfo32_t(fi).ds_ripple;
  lfactor := Pflatrenderinfo32_t(fi).ds_lightlevel;
  if lfactor >= 0 then // Use hi detail lightlevel
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, ds_fog);  // JVAL: Mars fog sectors
    {$DEFINE RIPPLE}
    {$UNDEF INVERSECOLORMAPS}
    {$UNDEF TRANSPARENTFLAT}
    {$I R_DrawSpanNormal.inc}
  end
  else // Use inversecolormap
  begin
    {$DEFINE RIPPLE}
    {$DEFINE INVERSECOLORMAPS}
    {$UNDEF TRANSPARENTFLAT}
    {$I R_DrawSpanNormal.inc}
  end;
end;


end.

