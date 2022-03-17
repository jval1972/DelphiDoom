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

unit r_batchsky;

interface

//==============================================================================
//
// R_StoreSkyColumn8
//
//==============================================================================
procedure R_StoreSkyColumn8;

//==============================================================================
//
// R_FlashSkyColumns8
//
//==============================================================================
procedure R_FlashSkyColumns8;

//==============================================================================
//
// R_StoreSkyColumn32
//
//==============================================================================
procedure R_StoreSkyColumn32;

//==============================================================================
//
// R_FlashSkyColumns32
//
//==============================================================================
procedure R_FlashSkyColumns32;

implementation

uses
  d_delphi,
  doomdef,
  m_fixed,
  r_cache_walls,
  r_column,
  r_draw,
  r_main;

type
  skyrenderinfo8_t = record
    dc_source: PByteArray;
    dc_yh, dc_yl, dc_x: Integer;
  end;
  Pskyrenderinfo8_t = ^skyrenderinfo8_t;

  batchskyrenderinfo8_t = record
    numskies: integer;
    skies: array[0..MAXBATCHWALLS - 1] of skyrenderinfo8_t;
  end;
  Pbatchskyrenderinfo8_t = ^batchskyrenderinfo8_t;

var
  skies8: batchskyrenderinfo8_t = (numskies: 0);

//==============================================================================
//
// R_StoreSkyColumn8
//
//==============================================================================
procedure R_StoreSkyColumn8;
var
  sk: Pskyrenderinfo8_t;
begin
  if skies8.numskies > 0 then
    if dc_x <> skies8.skies[skies8.numskies - 1].dc_x + 1 then
      R_FlashSkyColumns8;

  sk := @skies8.skies[skies8.numskies];
  sk.dc_source := dc_source;
  sk.dc_yh := dc_yh;
  sk.dc_yl := dc_yl;
  sk.dc_x := dc_x;
  inc(skies8.numskies);
  if skies8.numskies = MAXBATCHWALLS then
    R_FlashSkyColumns8;
end;

//==============================================================================
//
// R_DrawBatchSkyColumns8
//
//==============================================================================
procedure R_DrawBatchSkyColumns8;
var
  sk: Pskyrenderinfo8_t;
  ypos: integer;
  dest: PByte;
  frac: fixed_t;
  fracstep: fixed_t;
  swidth: integer;
  swidth2: integer;
  spot: integer;

  dc_source1: PByteArray;
  dc_source2: PByteArray;
  dc_source3: PByteArray;
  dc_source4: PByteArray;
  dc_source5: PByteArray;
  dc_source6: PByteArray;
  dc_source7: PByteArray;
  dc_source8: PByteArray;

  i: integer;

  min_yh, max_yl: integer;
  max_yh, min_yl: integer;

  buf: fourbytes_t;
begin
  sk := @skies8.skies[0];
  min_yh := sk.dc_yh;
  max_yl := sk.dc_yl;
  max_yh := sk.dc_yh;
  min_yl := sk.dc_yl;

  for i := 1 to MAXBATCHWALLS - 1 do
  begin
    inc(sk);
    if sk.dc_yh < min_yh then
      min_yh := sk.dc_yh;
    if sk.dc_yl > max_yl then
      max_yl := sk.dc_yl;
    if sk.dc_yh > max_yh then
      max_yh := sk.dc_yh;
    if sk.dc_yl < min_yl then
      min_yl := sk.dc_yl;
  end;

  if max_yl < 0 then
    max_yl := 0
  else if max_yl >= SCREENHEIGHT then
    max_yl := SCREENHEIGHT - 1;
  if min_yl < 0 then
    min_yl := 0
  else if min_yl >= SCREENHEIGHT then
    min_yl := SCREENHEIGHT - 1;
  if min_yh < 0 then
    min_yh := 0
  else if min_yh >= SCREENHEIGHT then
    min_yh := SCREENHEIGHT - 1;
  if max_yh < 0 then
    max_yh := 0
  else if max_yh >= SCREENHEIGHT then
    max_yh := SCREENHEIGHT - 1;

  sk := @skies8.skies[0];
  dest := @((ylookup[min_yl]^)[columnofs[sk.dc_x]]);

  fracstep := dc_iscale;
  frac := dc_texturemid + (min_yl - centery) * fracstep;
  dc_source1 := sk.dc_source;
  inc(sk);

  dc_source2 := sk.dc_source;
  inc(sk);

  dc_source3 := sk.dc_source;
  inc(sk);

  dc_source4 := sk.dc_source;
  inc(sk);

  dc_source5 := sk.dc_source;
  inc(sk);

  dc_source6 := sk.dc_source;
  inc(sk);

  dc_source7 := sk.dc_source;
  inc(sk);

  dc_source8 := sk.dc_source;

  swidth := SCREENWIDTH - (MAXBATCHWALLS - 1);
  swidth2 := SCREENWIDTH - (MAXBATCHWALLS - 4);

  ypos := min_yl;
  while ypos < max_yl do
  begin
    sk := @skies8.skies[0];
    {$IFDEF STRIFE}
    spot := (LongWord(frac) shr FRACBITS) and 127;
    {$ELSE}
    spot := LongWord(frac) shr FRACBITS;
    if spot > 127 then
      spot := 127 - (spot and 127);
    {$ENDIF}
    if (ypos >= sk.dc_yl) and (ypos <= sk.dc_yh) then
      dest^ := dc_source1[spot];
    inc(dest);
    inc(sk);

    if (ypos >= sk.dc_yl) and (ypos <= sk.dc_yh) then
      dest^ := dc_source2[spot];
    inc(dest);
    inc(sk);

    if (ypos >= sk.dc_yl) and (ypos <= sk.dc_yh) then
      dest^ := dc_source3[spot];
    inc(dest);
    inc(sk);

    if (ypos >= sk.dc_yl) and (ypos <= sk.dc_yh) then
      dest^ := dc_source4[spot];
    inc(dest);
    inc(sk);

    if (ypos >= sk.dc_yl) and (ypos <= sk.dc_yh) then
      dest^ := dc_source5[spot];
    inc(dest);
    inc(sk);

    if (ypos >= sk.dc_yl) and (ypos <= sk.dc_yh) then
      dest^ := dc_source6[spot];
    inc(dest);
    inc(sk);

    if (ypos >= sk.dc_yl) and (ypos <= sk.dc_yh) then
      dest^ := dc_source7[spot];
    inc(dest);
    inc(sk);

    if (ypos >= sk.dc_yl) and (ypos <= sk.dc_yh) then
      dest^ := dc_source8[spot];

    inc(frac, fracstep);
    dest := PByte(integer(dest) + swidth);
    inc(ypos);
  end;

  while ypos <= min_yh do
  begin
    {$IFDEF STRIFE}
    spot := (LongWord(frac) shr FRACBITS) and 127;
    {$ELSE}
    spot := LongWord(frac) shr FRACBITS;
    if spot > 127 then
      spot := 127 - (spot and 127);
    {$ENDIF}
    buf.byte1 := dc_source1[spot];
    buf.byte2 := dc_source2[spot];
    buf.byte3 := dc_source3[spot];
    buf.byte4 := dc_source4[spot];
    PLongWord(dest)^ := PLongWord(@buf)^;
    inc(dest, 4);

    buf.byte1 := dc_source5[spot];
    buf.byte2 := dc_source6[spot];
    buf.byte3 := dc_source7[spot];
    buf.byte4 := dc_source8[spot];
    PLongWord(dest)^ := PLongWord(@buf)^;

    dest := PByte(integer(dest) + swidth2);
    inc(frac, fracstep);
    inc(ypos);

  end;

  while ypos <= max_yh do
  begin
    sk := @skies8.skies[0];
    {$IFDEF STRIFE}
    spot := (LongWord(frac) shr FRACBITS) and 127;
    {$ELSE}
    spot := LongWord(frac) shr FRACBITS;
    if spot > 127 then
      spot := 127 - (spot and 127);
    {$ENDIF}
    if (ypos >= sk.dc_yl) and (ypos <= sk.dc_yh) then
      dest^ := dc_source1[spot];
    inc(dest);
    inc(sk);

    if (ypos >= sk.dc_yl) and (ypos <= sk.dc_yh) then
      dest^ := dc_source2[spot];
    inc(dest);
    inc(sk);

    if (ypos >= sk.dc_yl) and (ypos <= sk.dc_yh) then
      dest^ := dc_source3[spot];
    inc(dest);
    inc(sk);

    if (ypos >= sk.dc_yl) and (ypos <= sk.dc_yh) then
      dest^ := dc_source4[spot];
    inc(dest);
    inc(sk);

    if (ypos >= sk.dc_yl) and (ypos <= sk.dc_yh) then
      dest^ := dc_source5[spot];
    inc(dest);
    inc(sk);

    if (ypos >= sk.dc_yl) and (ypos <= sk.dc_yh) then
      dest^ := dc_source6[spot];
    inc(dest);
    inc(sk);

    if (ypos >= sk.dc_yl) and (ypos <= sk.dc_yh) then
      dest^ := dc_source7[spot];
    inc(dest);
    inc(sk);

    if (ypos >= sk.dc_yl) and (ypos <= sk.dc_yh) then
      dest^ := dc_source8[spot];

    dest := PByte(integer(dest) + swidth);
    inc(frac, fracstep);
    inc(ypos);
  end;

end;

//==============================================================================
//
// R_FlashSkyColumns8
//
//==============================================================================
procedure R_FlashSkyColumns8;
var
  i: integer;
  sk: Pskyrenderinfo8_t;
  old_dc_source: PByteArray;
  old_dc_yh: integer;
  old_dc_yl: integer;
  old_dc_x: integer;
begin
  if skies8.numskies = 0 then
    exit;

  if skies8.numskies = MAXBATCHWALLS then
  begin
    R_DrawBatchSkyColumns8;
  end
  else
  begin
    old_dc_source := dc_source;
    old_dc_yh := dc_yh;
    old_dc_yl := dc_yl;
    old_dc_x := dc_x;
    sk := @skies8.skies[0];
    for i := 0 to skies8.numskies - 1 do
    begin
      dc_source := sk.dc_source;
      dc_yh := sk.dc_yh;
      dc_yl := sk.dc_yl;
      dc_x := sk.dc_x;
      skycolfunc;
      inc(sk);
    end;
    dc_source := old_dc_source;
    dc_yh := old_dc_yh;
    dc_yl := old_dc_yl;
    dc_x := old_dc_x;
  end;

  skies8.numskies := 0;
end;

type
  skyrenderinfo32_t = record
    dc_source32: PLongWordArray;
    dc_yh, dc_yl, dc_x: Integer;
  end;
  Pskyrenderinfo32_t = ^skyrenderinfo32_t;

  batchskyrenderinfo32_t = record
    numskies: integer;
    skies: array[0..MAXBATCHWALLS - 1] of skyrenderinfo32_t;
  end;
  Pbatchskyrenderinfo32_t = ^batchskyrenderinfo32_t;

var
  skies32: batchskyrenderinfo32_t = (numskies: 0);

//==============================================================================
//
// R_StoreSkyColumn32
//
//==============================================================================
procedure R_StoreSkyColumn32;
var
  sk: Pskyrenderinfo32_t;
begin
  if skies32.numskies > 0 then
    if dc_x <> skies32.skies[skies32.numskies - 1].dc_x + 1 then
      R_FlashSkyColumns32;

  sk := @skies32.skies[skies32.numskies];
  sk.dc_source32 := dc_source32;
  sk.dc_yh := dc_yh;
  sk.dc_yl := dc_yl;
  sk.dc_x := dc_x;
  inc(skies32.numskies);
  if skies32.numskies = MAXBATCHWALLS then
    R_FlashSkyColumns32;
end;

//==============================================================================
//
// R_DrawBatchSkyColumns32
//
//==============================================================================
procedure R_DrawBatchSkyColumns32;
var
  sk: Pskyrenderinfo32_t;
  ypos: integer;
  destl: PLongWord;
  frac: fixed_t;
  fracstep: fixed_t;
  swidth: integer;
  and_mask: integer;
  spot: integer;

  dc_source1: PLongWordArray;
  dc_source2: PLongWordArray;
  dc_source3: PLongWordArray;
  dc_source4: PLongWordArray;
  dc_source5: PLongWordArray;
  dc_source6: PLongWordArray;
  dc_source7: PLongWordArray;
  dc_source8: PLongWordArray;

  i: integer;

  min_yh, max_yl: integer;
  max_yh, min_yl: integer;

begin
  sk := @skies32.skies[0];
  min_yh := sk.dc_yh;
  max_yl := sk.dc_yl;
  max_yh := sk.dc_yh;
  min_yl := sk.dc_yl;

  for i := 1 to MAXBATCHWALLS - 1 do
  begin
    inc(sk);
    if sk.dc_yh < min_yh then
      min_yh := sk.dc_yh;
    if sk.dc_yl > max_yl then
      max_yl := sk.dc_yl;
    if sk.dc_yh > max_yh then
      max_yh := sk.dc_yh;
    if sk.dc_yl < min_yl then
      min_yl := sk.dc_yl;
  end;

  if max_yl < 0 then
    max_yl := 0
  else if max_yl >= SCREENHEIGHT then
    max_yl := SCREENHEIGHT - 1;
  if min_yl < 0 then
    min_yl := 0
  else if min_yl >= SCREENHEIGHT then
    min_yl := SCREENHEIGHT - 1;
  if min_yh < 0 then
    min_yh := 0
  else if min_yh >= SCREENHEIGHT then
    min_yh := SCREENHEIGHT - 1;
  if max_yh < 0 then
    max_yh := 0
  else if max_yh >= SCREENHEIGHT then
    max_yh := SCREENHEIGHT - 1;

  sk := @skies32.skies[0];
  destl := @((ylookupl[min_yl]^)[columnofs[sk.dc_x]]);

  fracstep := dc_iscale;
  frac := dc_texturemid + (min_yl - centery) * fracstep;

  fracstep := fracstep * (1 shl dc_texturefactorbits);
  frac := frac * (1 shl dc_texturefactorbits);
  and_mask := 128 * (1 shl dc_texturefactorbits) - 1;

  dc_source1 := sk.dc_source32;
  inc(sk);

  dc_source2 := sk.dc_source32;
  inc(sk);

  dc_source3 := sk.dc_source32;
  inc(sk);

  dc_source4 := sk.dc_source32;
  inc(sk);

  dc_source5 := sk.dc_source32;
  inc(sk);

  dc_source6 := sk.dc_source32;
  inc(sk);

  dc_source7 := sk.dc_source32;
  inc(sk);

  dc_source8 := sk.dc_source32;

  swidth := SCREENWIDTH - (MAXBATCHWALLS - 1);

  ypos := min_yl;
  while ypos < max_yl do
  begin
    sk := @skies32.skies[0];
    {$IFDEF STRIFE}
    spot := (LongWord(frac) shr FRACBITS) and and_mask;
    {$ELSE}
    spot := LongWord(frac) shr FRACBITS;
    if spot > and_mask then
      spot := and_mask - (spot and and_mask);
    {$ENDIF}
    if (ypos >= sk.dc_yl) and (ypos <= sk.dc_yh) then
      destl^ := dc_source1[spot];
    inc(destl);
    inc(sk);

    if (ypos >= sk.dc_yl) and (ypos <= sk.dc_yh) then
      destl^ := dc_source2[spot];
    inc(destl);
    inc(sk);

    if (ypos >= sk.dc_yl) and (ypos <= sk.dc_yh) then
      destl^ := dc_source3[spot];
    inc(destl);
    inc(sk);

    if (ypos >= sk.dc_yl) and (ypos <= sk.dc_yh) then
      destl^ := dc_source4[spot];
    inc(destl);
    inc(sk);

    if (ypos >= sk.dc_yl) and (ypos <= sk.dc_yh) then
      destl^ := dc_source5[spot];
    inc(destl);
    inc(sk);

    if (ypos >= sk.dc_yl) and (ypos <= sk.dc_yh) then
      destl^ := dc_source6[spot];
    inc(destl);
    inc(sk);

    if (ypos >= sk.dc_yl) and (ypos <= sk.dc_yh) then
      destl^ := dc_source7[spot];
    inc(destl);
    inc(sk);

    if (ypos >= sk.dc_yl) and (ypos <= sk.dc_yh) then
      destl^ := dc_source8[spot];

    inc(destl, swidth);
    inc(frac, fracstep);
    inc(ypos);
  end;

  while ypos <= min_yh do
  begin
    {$IFDEF STRIFE}
    spot := (LongWord(frac) shr FRACBITS) and and_mask;
    {$ELSE}
    spot := LongWord(frac) shr FRACBITS;
    if spot > and_mask then
      spot := and_mask - (spot and and_mask);
    {$ENDIF}
    destl^ := dc_source1[spot];
    inc(destl);

    destl^ := dc_source2[spot];
    inc(destl);

    destl^ := dc_source3[spot];
    inc(destl);

    destl^ := dc_source4[spot];
    inc(destl);

    destl^ := dc_source5[spot];
    inc(destl);

    destl^ := dc_source6[spot];
    inc(destl);

    destl^ := dc_source7[spot];
    inc(destl);

    destl^ := dc_source8[spot];

    inc(destl, swidth);
    inc(frac, fracstep);
    inc(ypos);

  end;

  while ypos <= max_yh do
  begin
    sk := @skies32.skies[0];
    {$IFDEF STRIFE}
    spot := (LongWord(frac) shr FRACBITS) and and_mask;
    {$ELSE}
    spot := LongWord(frac) shr FRACBITS;
    if spot > and_mask then
      spot := and_mask - (spot and and_mask);
    {$ENDIF}
    if (ypos >= sk.dc_yl) and (ypos <= sk.dc_yh) then
      destl^ := dc_source1[spot];
    inc(destl);
    inc(sk);

    if (ypos >= sk.dc_yl) and (ypos <= sk.dc_yh) then
      destl^ := dc_source2[spot];
    inc(destl);
    inc(sk);

    if (ypos >= sk.dc_yl) and (ypos <= sk.dc_yh) then
      destl^ := dc_source3[spot];
    inc(destl);
    inc(sk);

    if (ypos >= sk.dc_yl) and (ypos <= sk.dc_yh) then
      destl^ := dc_source4[spot];
    inc(destl);
    inc(sk);

    if (ypos >= sk.dc_yl) and (ypos <= sk.dc_yh) then
      destl^ := dc_source5[spot];
    inc(destl);
    inc(sk);

    if (ypos >= sk.dc_yl) and (ypos <= sk.dc_yh) then
      destl^ := dc_source6[spot];
    inc(destl);
    inc(sk);

    if (ypos >= sk.dc_yl) and (ypos <= sk.dc_yh) then
      destl^ := dc_source7[spot];
    inc(destl);
    inc(sk);

    if (ypos >= sk.dc_yl) and (ypos <= sk.dc_yh) then
      destl^ := dc_source8[spot];

    inc(destl, swidth);
    inc(frac, fracstep);
    inc(ypos);
  end;

end;

//==============================================================================
//
// R_FlashSkyColumns32
//
//==============================================================================
procedure R_FlashSkyColumns32;
var
  i: integer;
  sk: Pskyrenderinfo32_t;
  old_dc_source32: PLongWordArray;
  old_dc_yh: integer;
  old_dc_yl: integer;
  old_dc_x: integer;
begin
  if skies32.numskies = 0 then
    exit;

  if skies32.numskies = MAXBATCHWALLS then
  begin
    R_DrawBatchSkyColumns32;
  end
  else
  begin
    old_dc_source32 := dc_source32;
    old_dc_yh := dc_yh;
    old_dc_yl := dc_yl;
    old_dc_x := dc_x;
    sk := @skies32.skies[0];
    for i := 0 to skies32.numskies - 1 do
    begin
      dc_source32 := sk.dc_source32;
      dc_yh := sk.dc_yh;
      dc_yl := sk.dc_yl;
      dc_x := sk.dc_x;
      skycolfunc;
      inc(sk);
    end;
    dc_source32 := old_dc_source32;
    dc_yh := old_dc_yh;
    dc_yl := old_dc_yl;
    dc_x := old_dc_x;
  end;

  skies32.numskies := 0;
end;

end.
