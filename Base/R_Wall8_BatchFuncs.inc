{$IFDEF WALL8_128}
procedure R_DrawBatchColumn8_128(const walls: Pbatchwallrenderinfo8_t);
{$ENDIF}
{$IFDEF WALL8_256}
procedure R_DrawBatchColumn8_256(const walls: Pbatchwallrenderinfo8_t);
{$ENDIF}
{$IFDEF WALL8_512}
procedure R_DrawBatchColumn8_512(const walls: Pbatchwallrenderinfo8_t);
{$ENDIF}
{$IFDEF WALL8_TC}
procedure R_DrawBatchColumn8_TC(const walls: Pbatchwallrenderinfo8_t);
{$ENDIF}
var
  w: Pwallrenderinfo8_t;
  ypos: integer;
  dest: PByte;
  frac1, frac2, frac3, frac4, frac5, frac6, frac7, frac8: fixed_t;
  fracstep1, fracstep2, fracstep3, fracstep4, fracstep5, fracstep6, fracstep7, fracstep8: fixed_t;
  swidth: integer;
  swidth2: integer;

  dc_source1: PByteArray;
  dc_source2: PByteArray;
  dc_source3: PByteArray;
  dc_source4: PByteArray;
  dc_source5: PByteArray;
  dc_source6: PByteArray;
  dc_source7: PByteArray;
  dc_source8: PByteArray;

  dc_colormap1: PByteArray;
  dc_colormap2: PByteArray;
  dc_colormap3: PByteArray;
  dc_colormap4: PByteArray;
  dc_colormap5: PByteArray;
  dc_colormap6: PByteArray;
  dc_colormap7: PByteArray;
  dc_colormap8: PByteArray;

  xx: integer;
  i: integer;

  min_yh, max_yl: integer;
  max_yh, min_yl: integer;

  buf: eightbytes_t;
{$IFDEF WALL8_TC}
  mod_height: LongWord;
{$ENDIF}
begin
  w := @walls.walls[0];
  min_yh := w.dc_yh;
  max_yl := w.dc_yl;
  max_yh := w.dc_yh;
  min_yl := w.dc_yl;

  for i := 1 to MAXBATCHWALLS - 1 do
  begin
    inc(w);
    if w.dc_yh < min_yh then
      min_yh := w.dc_yh;
    if w.dc_yl > max_yl then
      max_yl := w.dc_yl;
    if w.dc_yh > max_yh then
      max_yh := w.dc_yh;
    if w.dc_yl < min_yl then
      min_yl := w.dc_yl;
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

{$IFDEF WALL8_TC}
  mod_height := walls.dc_height;
{$ENDIF}
  w := @walls.walls[0];
  dest := @((ylookup[min_yl]^)[columnofs[w.dc_x]]);

  xx := min_yl - centery;

  fracstep1 := w.dc_iscale;
  frac1 := w.dc_texturemid + xx * fracstep1;
  dc_source1 := w.dc_source;
  dc_colormap1 := w.dc_colormap;
  inc(w);

  fracstep2 := w.dc_iscale;
  frac2 := w.dc_texturemid + xx * fracstep2;
  dc_source2 := w.dc_source;
  dc_colormap2 := w.dc_colormap;
  inc(w);

  fracstep3 := w.dc_iscale;
  frac3 := w.dc_texturemid + xx * fracstep3;
  dc_source3 := w.dc_source;
  dc_colormap3 := w.dc_colormap;
  inc(w);

  fracstep4 := w.dc_iscale;
  frac4 := w.dc_texturemid + xx * fracstep4;
  dc_source4 := w.dc_source;
  dc_colormap4 := w.dc_colormap;
  inc(w);

  fracstep5 := w.dc_iscale;
  frac5 := w.dc_texturemid + xx * fracstep5;
  dc_source5 := w.dc_source;
  dc_colormap5 := w.dc_colormap;
  inc(w);

  fracstep6 := w.dc_iscale;
  frac6 := w.dc_texturemid + xx * fracstep6;
  dc_source6 := w.dc_source;
  dc_colormap6 := w.dc_colormap;
  inc(w);

  fracstep7 := w.dc_iscale;
  frac7 := w.dc_texturemid + xx * fracstep7;
  dc_source7 := w.dc_source;
  dc_colormap7 := w.dc_colormap;
  inc(w);

  fracstep8 := w.dc_iscale;
  frac8 := w.dc_texturemid + xx * fracstep8;
  dc_source8 := w.dc_source;
  dc_colormap8 := w.dc_colormap;

  swidth := SCREENWIDTH - (MAXBATCHWALLS - 1);
  swidth2 := SCREENWIDTH - (MAXBATCHWALLS - 4);

  ypos := min_yl;
  while ypos < max_yl do
  begin
    w := @walls.walls[0];
    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap1[dc_source1[(LongWord(frac1) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac1, fracstep1);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap2[dc_source2[(LongWord(frac2) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac2, fracstep2);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap3[dc_source3[(LongWord(frac3) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac3, fracstep3);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap4[dc_source4[(LongWord(frac4) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac4, fracstep4);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap5[dc_source5[(LongWord(frac5) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac5, fracstep5);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap6[dc_source6[(LongWord(frac6) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac6, fracstep6);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap7[dc_source7[(LongWord(frac7) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac7, fracstep7);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap8[dc_source8[(LongWord(frac8) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac8, fracstep8);

    dest := PByte(integer(dest) + swidth);
    inc(ypos);
  end;

  while ypos <= min_yh do
  begin
    buf.byte1 := dc_colormap1[dc_source1[(LongWord(frac1) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac1, fracstep1);

    buf.byte2 := dc_colormap2[dc_source2[(LongWord(frac2) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac2, fracstep2);

    buf.byte3 := dc_colormap3[dc_source3[(LongWord(frac3) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac3, fracstep3);

    buf.byte4 := dc_colormap4[dc_source4[(LongWord(frac4) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac4, fracstep4);
    PLongWord(dest)^ := PLongWord(@buf)^;
    inc(dest, 4);

    buf.byte1 := dc_colormap5[dc_source5[(LongWord(frac5) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac5, fracstep5);

    buf.byte2 := dc_colormap6[dc_source6[(LongWord(frac6) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac6, fracstep6);

    buf.byte3 := dc_colormap7[dc_source7[(LongWord(frac7) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac7, fracstep7);

    buf.byte4 := dc_colormap8[dc_source8[(LongWord(frac8) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac8, fracstep8);
    PLongWord(dest)^ := PLongWord(@buf)^;
    dest := PByte(integer(dest) + swidth2);

    inc(ypos);
  end;

  while ypos <= max_yh do
  begin
    w := @walls.walls[0];
    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap1[dc_source1[(LongWord(frac1) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac1, fracstep1);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap2[dc_source2[(LongWord(frac2) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac2, fracstep2);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap3[dc_source3[(LongWord(frac3) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac3, fracstep3);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap4[dc_source4[(LongWord(frac4) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac4, fracstep4);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap5[dc_source5[(LongWord(frac5) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac5, fracstep5);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap6[dc_source6[(LongWord(frac6) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac6, fracstep6);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap7[dc_source7[(LongWord(frac7) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac7, fracstep7);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap8[dc_source8[(LongWord(frac8) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac8, fracstep8);

    dest := PByte(integer(dest) + swidth);
    inc(ypos);
  end;

end;

{$IFDEF WALL8_128}
procedure R_DrawBatchColumn7_128(const walls: Pbatchwallrenderinfo8_t);
{$ENDIF}
{$IFDEF WALL8_256}
procedure R_DrawBatchColumn7_256(const walls: Pbatchwallrenderinfo8_t);
{$ENDIF}
{$IFDEF WALL8_512}
procedure R_DrawBatchColumn7_512(const walls: Pbatchwallrenderinfo8_t);
{$ENDIF}
{$IFDEF WALL8_TC}
procedure R_DrawBatchColumn7_TC(const walls: Pbatchwallrenderinfo8_t);
{$ENDIF}
var
  w: Pwallrenderinfo8_t;
  ypos: integer;
  dest: PByte;
  frac1, frac2, frac3, frac4, frac5, frac6, frac7: fixed_t;
  fracstep1, fracstep2, fracstep3, fracstep4, fracstep5, fracstep6, fracstep7: fixed_t;
  swidth: integer;

  dc_source1: PByteArray;
  dc_source2: PByteArray;
  dc_source3: PByteArray;
  dc_source4: PByteArray;
  dc_source5: PByteArray;
  dc_source6: PByteArray;
  dc_source7: PByteArray;

  dc_colormap1: PByteArray;
  dc_colormap2: PByteArray;
  dc_colormap3: PByteArray;
  dc_colormap4: PByteArray;
  dc_colormap5: PByteArray;
  dc_colormap6: PByteArray;
  dc_colormap7: PByteArray;

  xx: integer;
  i: integer;

  min_yh, max_yl: integer;
  max_yh, min_yl: integer;

  buf: fourbytes_t;
  buf2: twobytes_t;
{$IFDEF WALL8_TC}
  mod_height: LongWord;
{$ENDIF}
begin
  w := @walls.walls[0];
  min_yh := w.dc_yh;
  max_yl := w.dc_yl;
  max_yh := w.dc_yh;
  min_yl := w.dc_yl;

  for i := 1 to 6 do
  begin
    inc(w);
    if w.dc_yh < min_yh then
      min_yh := w.dc_yh;
    if w.dc_yl > max_yl then
      max_yl := w.dc_yl;
    if w.dc_yh > max_yh then
      max_yh := w.dc_yh;
    if w.dc_yl < min_yl then
      min_yl := w.dc_yl;
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

{$IFDEF WALL8_TC}
  mod_height := walls.dc_height;
{$ENDIF}
  w := @walls.walls[0];
  dest := @((ylookup[min_yl]^)[columnofs[w.dc_x]]);

  xx := min_yl - centery;

  fracstep1 := w.dc_iscale;
  frac1 := w.dc_texturemid + xx * fracstep1;
  dc_source1 := w.dc_source;
  dc_colormap1 := w.dc_colormap;
  inc(w);

  fracstep2 := w.dc_iscale;
  frac2 := w.dc_texturemid + xx * fracstep2;
  dc_source2 := w.dc_source;
  dc_colormap2 := w.dc_colormap;
  inc(w);

  fracstep3 := w.dc_iscale;
  frac3 := w.dc_texturemid + xx * fracstep3;
  dc_source3 := w.dc_source;
  dc_colormap3 := w.dc_colormap;
  inc(w);

  fracstep4 := w.dc_iscale;
  frac4 := w.dc_texturemid + xx * fracstep4;
  dc_source4 := w.dc_source;
  dc_colormap4 := w.dc_colormap;
  inc(w);

  fracstep5 := w.dc_iscale;
  frac5 := w.dc_texturemid + xx * fracstep5;
  dc_source5 := w.dc_source;
  dc_colormap5 := w.dc_colormap;
  inc(w);

  fracstep6 := w.dc_iscale;
  frac6 := w.dc_texturemid + xx * fracstep6;
  dc_source6 := w.dc_source;
  dc_colormap6 := w.dc_colormap;
  inc(w);

  fracstep7 := w.dc_iscale;
  frac7 := w.dc_texturemid + xx * fracstep7;
  dc_source7 := w.dc_source;
  dc_colormap7 := w.dc_colormap;

  swidth := SCREENWIDTH - 6;

  ypos := min_yl;
  while ypos < max_yl do
  begin
    w := @walls.walls[0];
    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap1[dc_source1[(LongWord(frac1) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac1, fracstep1);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap2[dc_source2[(LongWord(frac2) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac2, fracstep2);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap3[dc_source3[(LongWord(frac3) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac3, fracstep3);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap4[dc_source4[(LongWord(frac4) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac4, fracstep4);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap5[dc_source5[(LongWord(frac5) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac5, fracstep5);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap6[dc_source6[(LongWord(frac6) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac6, fracstep6);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap7[dc_source7[(LongWord(frac7) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac7, fracstep7);

    dest := PByte(integer(dest) + swidth);
    inc(ypos);
  end;

  while ypos <= min_yh do
  begin
    buf.byte1 := dc_colormap1[dc_source1[(LongWord(frac1) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac1, fracstep1);

    buf.byte2 := dc_colormap2[dc_source2[(LongWord(frac2) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac2, fracstep2);

    buf.byte3 := dc_colormap3[dc_source3[(LongWord(frac3) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac3, fracstep3);

    buf.byte4 := dc_colormap4[dc_source4[(LongWord(frac4) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac4, fracstep4);
    PLongWord(dest)^ := PLongWord(@buf)^;
    inc(dest, 4);

    buf2.byte1 := dc_colormap5[dc_source5[(LongWord(frac5) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac5, fracstep5);

    buf2.byte2 := dc_colormap6[dc_source6[(LongWord(frac6) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac6, fracstep6);
    PWord(dest)^ := PWord(@buf2)^;
    inc(dest, 2);

    dest^ := dc_colormap7[dc_source7[(LongWord(frac7) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac7, fracstep7);

    dest := PByte(integer(dest) + swidth);
    inc(ypos);

  end;

  while ypos <= max_yh do
  begin
    w := @walls.walls[0];
    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap1[dc_source1[(LongWord(frac1) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac1, fracstep1);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap2[dc_source2[(LongWord(frac2) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac2, fracstep2);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap3[dc_source3[(LongWord(frac3) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac3, fracstep3);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap4[dc_source4[(LongWord(frac4) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac4, fracstep4);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap5[dc_source5[(LongWord(frac5) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac5, fracstep5);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap6[dc_source6[(LongWord(frac6) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac6, fracstep6);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap7[dc_source7[(LongWord(frac7) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac7, fracstep7);

    dest := PByte(integer(dest) + swidth);
    inc(ypos);
  end;

end;

{$IFDEF WALL8_128}
procedure R_DrawBatchColumn6_128(const walls: Pbatchwallrenderinfo8_t);
{$ENDIF}
{$IFDEF WALL8_256}
procedure R_DrawBatchColumn6_256(const walls: Pbatchwallrenderinfo8_t);
{$ENDIF}
{$IFDEF WALL8_512}
procedure R_DrawBatchColumn6_512(const walls: Pbatchwallrenderinfo8_t);
{$ENDIF}
{$IFDEF WALL8_TC}
procedure R_DrawBatchColumn6_TC(const walls: Pbatchwallrenderinfo8_t);
{$ENDIF}
var
  w: Pwallrenderinfo8_t;
  ypos: integer;
  dest: PByte;
  frac1, frac2, frac3, frac4, frac5, frac6: fixed_t;
  fracstep1, fracstep2, fracstep3, fracstep4, fracstep5, fracstep6: fixed_t;
  swidth: integer;
  swidth2: integer;

  dc_source1: PByteArray;
  dc_source2: PByteArray;
  dc_source3: PByteArray;
  dc_source4: PByteArray;
  dc_source5: PByteArray;
  dc_source6: PByteArray;

  dc_colormap1: PByteArray;
  dc_colormap2: PByteArray;
  dc_colormap3: PByteArray;
  dc_colormap4: PByteArray;
  dc_colormap5: PByteArray;
  dc_colormap6: PByteArray;

  xx: integer;
  i: integer;

  min_yh, max_yl: integer;
  max_yh, min_yl: integer;

  buf: fourbytes_t;
  buf2: twobytes_t;
{$IFDEF WALL8_TC}
  mod_height: LongWord;
{$ENDIF}
begin
  w := @walls.walls[0];
  min_yh := w.dc_yh;
  max_yl := w.dc_yl;
  max_yh := w.dc_yh;
  min_yl := w.dc_yl;

  for i := 1 to 5 do
  begin
    inc(w);
    if w.dc_yh < min_yh then
      min_yh := w.dc_yh;
    if w.dc_yl > max_yl then
      max_yl := w.dc_yl;
    if w.dc_yh > max_yh then
      max_yh := w.dc_yh;
    if w.dc_yl < min_yl then
      min_yl := w.dc_yl;
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

{$IFDEF WALL8_TC}
  mod_height := walls.dc_height;
{$ENDIF}
  w := @walls.walls[0];
  dest := @((ylookup[min_yl]^)[columnofs[w.dc_x]]);

  xx := min_yl - centery;

  fracstep1 := w.dc_iscale;
  frac1 := w.dc_texturemid + xx * fracstep1;
  dc_source1 := w.dc_source;
  dc_colormap1 := w.dc_colormap;
  inc(w);

  fracstep2 := w.dc_iscale;
  frac2 := w.dc_texturemid + xx * fracstep2;
  dc_source2 := w.dc_source;
  dc_colormap2 := w.dc_colormap;
  inc(w);

  fracstep3 := w.dc_iscale;
  frac3 := w.dc_texturemid + xx * fracstep3;
  dc_source3 := w.dc_source;
  dc_colormap3 := w.dc_colormap;
  inc(w);

  fracstep4 := w.dc_iscale;
  frac4 := w.dc_texturemid + xx * fracstep4;
  dc_source4 := w.dc_source;
  dc_colormap4 := w.dc_colormap;
  inc(w);

  fracstep5 := w.dc_iscale;
  frac5 := w.dc_texturemid + xx * fracstep5;
  dc_source5 := w.dc_source;
  dc_colormap5 := w.dc_colormap;
  inc(w);

  fracstep6 := w.dc_iscale;
  frac6 := w.dc_texturemid + xx * fracstep6;
  dc_source6 := w.dc_source;
  dc_colormap6 := w.dc_colormap;

  swidth := SCREENWIDTH - 5;
  swidth2 := SCREENWIDTH - 4;

  ypos := min_yl;
  while ypos < max_yl do
  begin
    w := @walls.walls[0];
    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap1[dc_source1[(LongWord(frac1) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac1, fracstep1);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap2[dc_source2[(LongWord(frac2) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac2, fracstep2);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap3[dc_source3[(LongWord(frac3) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac3, fracstep3);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap4[dc_source4[(LongWord(frac4) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac4, fracstep4);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap5[dc_source5[(LongWord(frac5) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac5, fracstep5);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap6[dc_source6[(LongWord(frac6) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac6, fracstep6);

    dest := PByte(integer(dest) + swidth);
    inc(ypos);
  end;

  while ypos <= min_yh do
  begin
    buf.byte1 := dc_colormap1[dc_source1[(LongWord(frac1) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac1, fracstep1);

    buf.byte2 := dc_colormap2[dc_source2[(LongWord(frac2) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac2, fracstep2);

    buf.byte3 := dc_colormap3[dc_source3[(LongWord(frac3) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac3, fracstep3);

    buf.byte4 := dc_colormap4[dc_source4[(LongWord(frac4) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac4, fracstep4);
    PLongWord(dest)^ := PLongWord(@buf)^;
    inc(dest, 4);

    buf2.byte1 := dc_colormap5[dc_source5[(LongWord(frac5) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac5, fracstep5);

    buf2.byte2 := dc_colormap6[dc_source6[(LongWord(frac6) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac6, fracstep6);
    PWord(dest)^ := PWord(@buf2)^;

    dest := PByte(integer(dest) + swidth2);
    inc(ypos);

  end;

  while ypos <= max_yh do
  begin
    w := @walls.walls[0];
    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap1[dc_source1[(LongWord(frac1) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac1, fracstep1);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap2[dc_source2[(LongWord(frac2) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac2, fracstep2);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap3[dc_source3[(LongWord(frac3) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac3, fracstep3);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap4[dc_source4[(LongWord(frac4) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac4, fracstep4);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap5[dc_source5[(LongWord(frac5) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac5, fracstep5);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap6[dc_source6[(LongWord(frac6) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac6, fracstep6);

    dest := PByte(integer(dest) + swidth);
    inc(ypos);
  end;

end;

{$IFDEF WALL8_128}
procedure R_DrawBatchColumn5_128(const walls: Pbatchwallrenderinfo8_t);
{$ENDIF}
{$IFDEF WALL8_256}
procedure R_DrawBatchColumn5_256(const walls: Pbatchwallrenderinfo8_t);
{$ENDIF}
{$IFDEF WALL8_512}
procedure R_DrawBatchColumn5_512(const walls: Pbatchwallrenderinfo8_t);
{$ENDIF}
{$IFDEF WALL8_TC}
procedure R_DrawBatchColumn5_TC(const walls: Pbatchwallrenderinfo8_t);
{$ENDIF}
var
  w: Pwallrenderinfo8_t;
  ypos: integer;
  dest: PByte;
  frac1, frac2, frac3, frac4, frac5: fixed_t;
  fracstep1, fracstep2, fracstep3, fracstep4, fracstep5: fixed_t;
  swidth: integer;

  dc_source1: PByteArray;
  dc_source2: PByteArray;
  dc_source3: PByteArray;
  dc_source4: PByteArray;
  dc_source5: PByteArray;

  dc_colormap1: PByteArray;
  dc_colormap2: PByteArray;
  dc_colormap3: PByteArray;
  dc_colormap4: PByteArray;
  dc_colormap5: PByteArray;

  xx: integer;
  i: integer;

  min_yh, max_yl: integer;
  max_yh, min_yl: integer;

  buf: fourbytes_t;
{$IFDEF WALL8_TC}
  mod_height: LongWord;
{$ENDIF}
begin
  w := @walls.walls[0];
  min_yh := w.dc_yh;
  max_yl := w.dc_yl;
  max_yh := w.dc_yh;
  min_yl := w.dc_yl;

  for i := 1 to 4 do
  begin
    inc(w);
    if w.dc_yh < min_yh then
      min_yh := w.dc_yh;
    if w.dc_yl > max_yl then
      max_yl := w.dc_yl;
    if w.dc_yh > max_yh then
      max_yh := w.dc_yh;
    if w.dc_yl < min_yl then
      min_yl := w.dc_yl;
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

{$IFDEF WALL8_TC}
  mod_height := walls.dc_height;
{$ENDIF}
  w := @walls.walls[0];
  dest := @((ylookup[min_yl]^)[columnofs[w.dc_x]]);

  xx := min_yl - centery;

  fracstep1 := w.dc_iscale;
  frac1 := w.dc_texturemid + xx * fracstep1;
  dc_source1 := w.dc_source;
  dc_colormap1 := w.dc_colormap;
  inc(w);

  fracstep2 := w.dc_iscale;
  frac2 := w.dc_texturemid + xx * fracstep2;
  dc_source2 := w.dc_source;
  dc_colormap2 := w.dc_colormap;
  inc(w);

  fracstep3 := w.dc_iscale;
  frac3 := w.dc_texturemid + xx * fracstep3;
  dc_source3 := w.dc_source;
  dc_colormap3 := w.dc_colormap;
  inc(w);

  fracstep4 := w.dc_iscale;
  frac4 := w.dc_texturemid + xx * fracstep4;
  dc_source4 := w.dc_source;
  dc_colormap4 := w.dc_colormap;
  inc(w);

  fracstep5 := w.dc_iscale;
  frac5 := w.dc_texturemid + xx * fracstep5;
  dc_source5 := w.dc_source;
  dc_colormap5 := w.dc_colormap;

  swidth := SCREENWIDTH - 4;

  ypos := min_yl;
  while ypos < max_yl do
  begin
    w := @walls.walls[0];
    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap1[dc_source1[(LongWord(frac1) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac1, fracstep1);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap2[dc_source2[(LongWord(frac2) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac2, fracstep2);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap3[dc_source3[(LongWord(frac3) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac3, fracstep3);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap4[dc_source4[(LongWord(frac4) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac4, fracstep4);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap5[dc_source5[(LongWord(frac5) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac5, fracstep5);

    dest := PByte(integer(dest) + swidth);
    inc(ypos);
  end;

  while ypos <= min_yh do
  begin
    buf.byte1 := dc_colormap1[dc_source1[(LongWord(frac1) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac1, fracstep1);

    buf.byte2 := dc_colormap2[dc_source2[(LongWord(frac2) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac2, fracstep2);

    buf.byte3 := dc_colormap3[dc_source3[(LongWord(frac3) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac3, fracstep3);

    buf.byte4 := dc_colormap4[dc_source4[(LongWord(frac4) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac4, fracstep4);
    PLongWord(dest)^ := PLongWord(@buf)^;
    inc(dest, 4);

    dest^ := dc_colormap5[dc_source5[(LongWord(frac5) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac5, fracstep5);

    dest := PByte(integer(dest) + swidth);
    inc(ypos);

  end;

  while ypos <= max_yh do
  begin
    w := @walls.walls[0];
    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap1[dc_source1[(LongWord(frac1) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac1, fracstep1);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap2[dc_source2[(LongWord(frac2) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac2, fracstep2);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap3[dc_source3[(LongWord(frac3) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac3, fracstep3);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap4[dc_source4[(LongWord(frac4) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac4, fracstep4);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap5[dc_source5[(LongWord(frac5) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac5, fracstep5);

    dest := PByte(integer(dest) + swidth);
    inc(ypos);
  end;

end;

{$IFDEF WALL8_128}
procedure R_DrawBatchColumn4_128(const walls: Pbatchwallrenderinfo8_t);
{$ENDIF}
{$IFDEF WALL8_256}
procedure R_DrawBatchColumn4_256(const walls: Pbatchwallrenderinfo8_t);
{$ENDIF}
{$IFDEF WALL8_512}
procedure R_DrawBatchColumn4_512(const walls: Pbatchwallrenderinfo8_t);
{$ENDIF}
{$IFDEF WALL8_TC}
procedure R_DrawBatchColumn4_TC(const walls: Pbatchwallrenderinfo8_t);
{$ENDIF}
var
  w: Pwallrenderinfo8_t;
  ypos: integer;
  dest: PByte;
  frac1, frac2, frac3, frac4: fixed_t;
  fracstep1, fracstep2, fracstep3, fracstep4: fixed_t;
  swidth: integer;

  dc_source1: PByteArray;
  dc_source2: PByteArray;
  dc_source3: PByteArray;
  dc_source4: PByteArray;

  dc_colormap1: PByteArray;
  dc_colormap2: PByteArray;
  dc_colormap3: PByteArray;
  dc_colormap4: PByteArray;

  xx: integer;
  i: integer;

  min_yh, max_yl: integer;
  max_yh, min_yl: integer;

  buf: fourbytes_t;
{$IFDEF WALL8_TC}
  mod_height: LongWord;
{$ENDIF}
begin
  w := @walls.walls[0];
  min_yh := w.dc_yh;
  max_yl := w.dc_yl;
  max_yh := w.dc_yh;
  min_yl := w.dc_yl;

  for i := 1 to 3 do
  begin
    inc(w);
    if w.dc_yh < min_yh then
      min_yh := w.dc_yh;
    if w.dc_yl > max_yl then
      max_yl := w.dc_yl;
    if w.dc_yh > max_yh then
      max_yh := w.dc_yh;
    if w.dc_yl < min_yl then
      min_yl := w.dc_yl;
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

{$IFDEF WALL8_TC}
  mod_height := walls.dc_height;
{$ENDIF}
  w := @walls.walls[0];
  dest := @((ylookup[min_yl]^)[columnofs[w.dc_x]]);

  xx := min_yl - centery;

  fracstep1 := w.dc_iscale;
  frac1 := w.dc_texturemid + xx * fracstep1;
  dc_source1 := w.dc_source;
  dc_colormap1 := w.dc_colormap;
  inc(w);

  fracstep2 := w.dc_iscale;
  frac2 := w.dc_texturemid + xx * fracstep2;
  dc_source2 := w.dc_source;
  dc_colormap2 := w.dc_colormap;
  inc(w);

  fracstep3 := w.dc_iscale;
  frac3 := w.dc_texturemid + xx * fracstep3;
  dc_source3 := w.dc_source;
  dc_colormap3 := w.dc_colormap;
  inc(w);

  fracstep4 := w.dc_iscale;
  frac4 := w.dc_texturemid + xx * fracstep4;
  dc_source4 := w.dc_source;
  dc_colormap4 := w.dc_colormap;

  swidth := SCREENWIDTH - 3;

  ypos := min_yl;
  while ypos < max_yl do
  begin
    w := @walls.walls[0];
    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap1[dc_source1[(LongWord(frac1) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac1, fracstep1);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap2[dc_source2[(LongWord(frac2) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac2, fracstep2);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap3[dc_source3[(LongWord(frac3) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac3, fracstep3);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap4[dc_source4[(LongWord(frac4) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac4, fracstep4);
    dest := PByte(integer(dest) + swidth);
    inc(ypos);
  end;

  while ypos <= min_yh do
  begin
    buf.byte1 := dc_colormap1[dc_source1[(LongWord(frac1) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac1, fracstep1);

    buf.byte2 := dc_colormap2[dc_source2[(LongWord(frac2) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac2, fracstep2);

    buf.byte3 := dc_colormap3[dc_source3[(LongWord(frac3) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac3, fracstep3);

    buf.byte4 := dc_colormap4[dc_source4[(LongWord(frac4) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac4, fracstep4);
    PLongWord(dest)^ := PLongWord(@buf)^;
    inc(dest, SCREENWIDTH);
    inc(ypos);

  end;

  while ypos <= max_yh do
  begin
    w := @walls.walls[0];
    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap1[dc_source1[(LongWord(frac1) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac1, fracstep1);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap2[dc_source2[(LongWord(frac2) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac2, fracstep2);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap3[dc_source3[(LongWord(frac3) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac3, fracstep3);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap4[dc_source4[(LongWord(frac4) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac4, fracstep4);
    dest := PByte(integer(dest) + swidth);
    inc(ypos);
  end;

end;

{$IFDEF WALL8_128}
procedure R_DrawBatchColumn3_128(const walls: Pbatchwallrenderinfo8_t);
{$ENDIF}
{$IFDEF WALL8_256}
procedure R_DrawBatchColumn3_256(const walls: Pbatchwallrenderinfo8_t);
{$ENDIF}
{$IFDEF WALL8_512}
procedure R_DrawBatchColumn3_512(const walls: Pbatchwallrenderinfo8_t);
{$ENDIF}
{$IFDEF WALL8_TC}
procedure R_DrawBatchColumn3_TC(const walls: Pbatchwallrenderinfo8_t);
{$ENDIF}
var
  w: Pwallrenderinfo8_t;
  ypos: integer;
  dest: PByte;
  frac1, frac2, frac3: fixed_t;
  fracstep1, fracstep2, fracstep3: fixed_t;
  swidth: integer;

  dc_source1: PByteArray;
  dc_source2: PByteArray;
  dc_source3: PByteArray;

  dc_colormap1: PByteArray;
  dc_colormap2: PByteArray;
  dc_colormap3: PByteArray;

  xx: integer;
  i: integer;

  min_yh, max_yl: integer;
  max_yh, min_yl: integer;

  buf: twobytes_t;
{$IFDEF WALL8_TC}
  mod_height: LongWord;
{$ENDIF}
begin
  w := @walls.walls[0];
  min_yh := w.dc_yh;
  max_yl := w.dc_yl;
  max_yh := w.dc_yh;
  min_yl := w.dc_yl;

  for i := 1 to 2 do
  begin
    inc(w);
    if w.dc_yh < min_yh then
      min_yh := w.dc_yh;
    if w.dc_yl > max_yl then
      max_yl := w.dc_yl;
    if w.dc_yh > max_yh then
      max_yh := w.dc_yh;
    if w.dc_yl < min_yl then
      min_yl := w.dc_yl;
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

{$IFDEF WALL8_TC}
  mod_height := walls.dc_height;
{$ENDIF}
  w := @walls.walls[0];
  dest := @((ylookup[min_yl]^)[columnofs[w.dc_x]]);

  xx := min_yl - centery;

  fracstep1 := w.dc_iscale;
  frac1 := w.dc_texturemid + xx * fracstep1;
  dc_source1 := w.dc_source;
  dc_colormap1 := w.dc_colormap;
  inc(w);

  fracstep2 := w.dc_iscale;
  frac2 := w.dc_texturemid + xx * fracstep2;
  dc_source2 := w.dc_source;
  dc_colormap2 := w.dc_colormap;
  inc(w);

  fracstep3 := w.dc_iscale;
  frac3 := w.dc_texturemid + xx * fracstep3;
  dc_source3 := w.dc_source;
  dc_colormap3 := w.dc_colormap;

  swidth := SCREENWIDTH - 2;

  ypos := min_yl;
  while ypos < max_yl do
  begin
    w := @walls.walls[0];
    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap1[dc_source1[(LongWord(frac1) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac1, fracstep1);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap2[dc_source2[(LongWord(frac2) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac2, fracstep2);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap3[dc_source3[(LongWord(frac3) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac3, fracstep3);

    dest := PByte(integer(dest) + swidth);
    inc(ypos);
  end;

  while ypos < min_yh do
  begin
    buf.byte1 := dc_colormap1[dc_source1[(LongWord(frac1) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac1, fracstep1);

    buf.byte2 := dc_colormap2[dc_source2[(LongWord(frac2) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac2, fracstep2);
    PWord(dest)^ := PWord(@buf)^;
    inc(dest, 2);

    dest^ := dc_colormap3[dc_source3[(LongWord(frac3) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac3, fracstep3);

    dest := PByte(integer(dest) + swidth);

    buf.byte1 := dc_colormap1[dc_source1[(LongWord(frac1) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac1, fracstep1);

    buf.byte2 := dc_colormap2[dc_source2[(LongWord(frac2) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac2, fracstep2);
    PWord(dest)^ := PWord(@buf)^;
    inc(dest, 2);

    dest^ := dc_colormap3[dc_source3[(LongWord(frac3) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac3, fracstep3);

    dest := PByte(integer(dest) + swidth);

    inc(ypos, 2);
  end;

  while ypos <= min_yh do
  begin
    buf.byte1 := dc_colormap1[dc_source1[(LongWord(frac1) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac1, fracstep1);

    buf.byte2 := dc_colormap2[dc_source2[(LongWord(frac2) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac2, fracstep2);
    PWord(dest)^ := PWord(@buf)^;
    inc(dest, 2);

    dest^ := dc_colormap3[dc_source3[(LongWord(frac3) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac3, fracstep3);

    dest := PByte(integer(dest) + swidth);
    inc(ypos);
  end;

  while ypos <= max_yh do
  begin
    w := @walls.walls[0];
    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap1[dc_source1[(LongWord(frac1) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac1, fracstep1);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap2[dc_source2[(LongWord(frac2) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac2, fracstep2);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap3[dc_source3[(LongWord(frac3) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac3, fracstep3);
    dest := PByte(integer(dest) + swidth);
    inc(ypos);
  end;
end;

{$IFDEF WALL8_128}
procedure R_DrawBatchColumn2_128(const walls: Pbatchwallrenderinfo8_t);
{$ENDIF}
{$IFDEF WALL8_256}
procedure R_DrawBatchColumn2_256(const walls: Pbatchwallrenderinfo8_t);
{$ENDIF}
{$IFDEF WALL8_512}
procedure R_DrawBatchColumn2_512(const walls: Pbatchwallrenderinfo8_t);
{$ENDIF}
{$IFDEF WALL8_TC}
procedure R_DrawBatchColumn2_TC(const walls: Pbatchwallrenderinfo8_t);
{$ENDIF}
var
  w: Pwallrenderinfo8_t;
  ypos: integer;
  dest: PByte;
  frac1, frac2: fixed_t;
  fracstep1, fracstep2: fixed_t;
  swidth: integer;

  dc_source1: PByteArray;
  dc_source2: PByteArray;

  dc_colormap1: PByteArray;
  dc_colormap2: PByteArray;

  xx: integer;

  min_yh, max_yl: integer;
  max_yh, min_yl: integer;

  buf: twobytes_t;
{$IFDEF WALL8_TC}
  mod_height: LongWord;
{$ENDIF}
begin
  w := @walls.walls[0];
  min_yh := w.dc_yh;
  max_yl := w.dc_yl;
  max_yh := w.dc_yh;
  min_yl := w.dc_yl;

  inc(w);
  if w.dc_yh < min_yh then
    min_yh := w.dc_yh;
  if w.dc_yl > max_yl then
    max_yl := w.dc_yl;
  if w.dc_yh > max_yh then
    max_yh := w.dc_yh;
  if w.dc_yl < min_yl then
    min_yl := w.dc_yl;

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

{$IFDEF WALL8_TC}
  mod_height := walls.dc_height;
{$ENDIF}
  w := @walls.walls[0];
  dest := @((ylookup[min_yl]^)[columnofs[w.dc_x]]);

  xx := min_yl - centery;

  fracstep1 := w.dc_iscale;
  frac1 := w.dc_texturemid + xx * fracstep1;
  dc_source1 := w.dc_source;
  dc_colormap1 := w.dc_colormap;
  inc(w);

  fracstep2 := w.dc_iscale;
  frac2 := w.dc_texturemid + xx * fracstep2;
  dc_source2 := w.dc_source;
  dc_colormap2 := w.dc_colormap;

  swidth := SCREENWIDTH - 1;

  ypos := min_yl;
  while ypos < max_yl do
  begin
    w := @walls.walls[0];
    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap1[dc_source1[(LongWord(frac1) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac1, fracstep1);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap2[dc_source2[(LongWord(frac2) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac2, fracstep2);
    dest := PByte(integer(dest) + swidth);
    inc(ypos);
  end;

  while ypos < min_yh do
  begin
    buf.byte1 := dc_colormap1[dc_source1[(LongWord(frac1) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac1, fracstep1);

    buf.byte2 := dc_colormap2[dc_source2[(LongWord(frac2) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac2, fracstep2);

    PWord(dest)^ := PWord(@buf)^;
    inc(dest, SCREENWIDTH);

    buf.byte1 := dc_colormap1[dc_source1[(LongWord(frac1) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac1, fracstep1);

    buf.byte2 := dc_colormap2[dc_source2[(LongWord(frac2) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac2, fracstep2);

    PWord(dest)^ := PWord(@buf)^;
    inc(dest, SCREENWIDTH);

    inc(ypos, 2);
  end;

  while ypos <= min_yh do
  begin
    buf.byte1 := dc_colormap1[dc_source1[(LongWord(frac1) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac1, fracstep1);

    buf.byte2 := dc_colormap2[dc_source2[(LongWord(frac2) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac2, fracstep2);

    PWord(dest)^ := PWord(@buf)^;
    inc(dest, SCREENWIDTH);
    inc(ypos);
  end;


  while ypos <= max_yh do
  begin
    w := @walls.walls[0];
    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap1[dc_source1[(LongWord(frac1) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac1, fracstep1);
    inc(dest);
    inc(w);

    if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
      dest^ := dc_colormap2[dc_source2[(LongWord(frac2) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac2, fracstep2);
    dest := PByte(integer(dest) + swidth);
    inc(ypos);
  end;

end;


{$IFDEF WALL8_128}
procedure R_DrawBatchColumn1_128(const walls: Pbatchwallrenderinfo8_t);
{$ENDIF}
{$IFDEF WALL8_256}
procedure R_DrawBatchColumn1_256(const walls: Pbatchwallrenderinfo8_t);
{$ENDIF}
{$IFDEF WALL8_512}
procedure R_DrawBatchColumn1_512(const walls: Pbatchwallrenderinfo8_t);
{$ENDIF}
{$IFDEF WALL8_TC}
procedure R_DrawBatchColumn1_TC(const walls: Pbatchwallrenderinfo8_t);
{$ENDIF}
var
  w: Pwallrenderinfo8_t;
  ypos: integer;
  dest: PByte;
  frac1: fixed_t;
  fracstep1: fixed_t;
  swidth: integer;

  dc_source1: PByteArray;

  dc_colormap1: PByteArray;

  xx: integer;

  min_y, max_y: integer;

{$IFDEF WALL8_TC}
  mod_height: LongWord;
{$ENDIF}
begin
  w := @walls.walls[0];
  min_y := w.dc_yl;
  max_y := w.dc_yh;

{$IFDEF WALL8_TC}
  mod_height := walls.dc_height;
{$ENDIF}
  dest := @((ylookup[min_y]^)[columnofs[w.dc_x]]);

  xx := min_y - centery;

  fracstep1 := w.dc_iscale;
  frac1 := w.dc_texturemid + xx * fracstep1;
  dc_source1 := w.dc_source;
  dc_colormap1 := w.dc_colormap;

  swidth := SCREENWIDTH;

  ypos := min_y;

  while ypos < max_y do
  begin
    dest^ := dc_colormap1[dc_source1[(LongWord(frac1) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac1, fracstep1);

    inc(dest, swidth);

    dest^ := dc_colormap1[dc_source1[(LongWord(frac1) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
    inc(frac1, fracstep1);

    inc(dest, swidth);

    inc(ypos, 2);
  end;

  if ypos <= max_y then
  begin
    dest^ := dc_colormap1[dc_source1[(LongWord(frac1) shr FRACBITS)
        {$IFDEF WALL8_TC}mod mod_height{$ELSE}and {$IFDEF WALL8_128}127{$ENDIF}{$IFDEF WALL8_256}255{$ENDIF}{$IFDEF WALL8_512}511{$ENDIF}{$ENDIF}]];
  end;

end;
