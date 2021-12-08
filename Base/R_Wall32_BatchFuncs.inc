{$IFDEF WALL32_128}
procedure R_DrawBatchColumnHi8_128(const walls: Pbatchwallrenderinfo32_t);
{$ENDIF}
{$IFDEF WALL32_256}
procedure R_DrawBatchColumnHi8_256(const walls: Pbatchwallrenderinfo32_t);
{$ENDIF}
{$IFDEF WALL32_512}
procedure R_DrawBatchColumnHi8_512(const walls: Pbatchwallrenderinfo32_t);
{$ENDIF}
{$IFDEF WALL32_TC}
procedure R_DrawBatchColumnHi8_TC(const walls: Pbatchwallrenderinfo32_t);
{$ENDIF}
var
  w: Pwallrenderinfo32_t;
  ypos: integer;
  destl: PLongWord;
  frac1, frac2, frac3, frac4, frac5, frac6, frac7, frac8: fixed_t;
  fracstep1, fracstep2, fracstep3, fracstep4, fracstep5, fracstep6, fracstep7, fracstep8: fixed_t;
  spot: integer;
  swidth: integer;

  r1, g1, b1: byte;
  c : LongWord;
  lfactor1, lfactor2, lfactor3, lfactor4, lfactor5, lfactor6, lfactor7, lfactor8: integer;
  mod_height: LongWord;
  and_mask: integer;
  bf_r1, bf_g1, bf_b1: PIntegerArray;
  bf_r2, bf_g2, bf_b2: PIntegerArray;
  bf_r3, bf_g3, bf_b3: PIntegerArray;
  bf_r4, bf_g4, bf_b4: PIntegerArray;
  bf_r5, bf_g5, bf_b5: PIntegerArray;
  bf_r6, bf_g6, bf_b6: PIntegerArray;
  bf_r7, bf_g7, bf_b7: PIntegerArray;
  bf_r8, bf_g8, bf_b8: PIntegerArray;
  dc_source32_1: PLongWordArray;
  dc_source32_2: PLongWordArray;
  dc_source32_3: PLongWordArray;
  dc_source32_4: PLongWordArray;
  dc_source32_5: PLongWordArray;
  dc_source32_6: PLongWordArray;
  dc_source32_7: PLongWordArray;
  dc_source32_8: PLongWordArray;

  xx: integer;
  i: integer;

  min_yh, max_yl: integer;
  max_yh, min_yl: integer;
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

  w := @walls.walls[0];
  destl := @((ylookupl[min_yl]^)[columnofs[w.dc_x]]);

  xx := min_yl - centery;

  fracstep1 := w.dc_iscale;
  frac1 := w.dc_texturemid + xx * fracstep1;
  dc_source32_1 := w.dc_source32;
  if w.dc_texturefactorbits > 0 then
  begin
    fracstep1 := fracstep1 * (1 shl w.dc_texturefactorbits);
    frac1 := frac1 * (1 shl w.dc_texturefactorbits);
    {$IFDEF WALL32_TC}
    mod_height := walls.dc_height * (1 shl w.dc_texturefactorbits);
    {$ELSE}
    and_mask := {$IFDEF WALL32_128}128{$ENDIF}{$IFDEF WALL32_256}256{$ENDIF}{$IFDEF WALL32_512}512{$ENDIF} * (1 shl w.dc_texturefactorbits) - 1;
    {$ENDIF}
  end
  else
    {$IFDEF WALL32_TC}
    mod_height := walls.dc_height;
    and_mask := mod_height - 1;
    {$ELSE}
    and_mask := {$IFDEF WALL32_128}127{$ENDIF}{$IFDEF WALL32_256}255{$ENDIF}{$IFDEF WALL32_512}511{$ENDIF};
    {$ENDIF}
  lfactor1 := w.dc_lightlevel;
  if lfactor1 >= 0 then
  {$IFDEF HEXEN}
    if LevelUseFog then
      R_GetFogPrecalc32Tables(lfactor1, bf_r1, bf_g1, bf_b1)
    else
  {$ENDIF}
    R_GetPrecalc32Tables(lfactor1, bf_r1, bf_g1, bf_b1, w.dc_fog);  // JVAL: Mars fog sectors
  inc(w);

  fracstep2 := w.dc_iscale;
  frac2 := w.dc_texturemid + xx * fracstep2;
  dc_source32_2 := w.dc_source32;
  if w.dc_texturefactorbits > 0 then
  begin
    fracstep2 := fracstep2 * (1 shl w.dc_texturefactorbits);
    frac2 := frac2 * (1 shl w.dc_texturefactorbits);
  end;
  lfactor2 := w.dc_lightlevel;
  if lfactor2 >= 0 then
  {$IFDEF HEXEN}
    if LevelUseFog then
      R_GetFogPrecalc32Tables(lfactor2, bf_r2, bf_g2, bf_b2)
    else
  {$ENDIF}
    R_GetPrecalc32Tables(lfactor2, bf_r2, bf_g2, bf_b2, w.dc_fog);  // JVAL: Mars fog sectors
  inc(w);

  fracstep3 := w.dc_iscale;
  frac3 := w.dc_texturemid + xx * fracstep3;
  dc_source32_3 := w.dc_source32;
  if w.dc_texturefactorbits > 0 then
  begin
    fracstep3 := fracstep3 * (1 shl w.dc_texturefactorbits);
    frac3 := frac3 * (1 shl w.dc_texturefactorbits);
  end;
  lfactor3 := w.dc_lightlevel;
  if lfactor3 >= 0 then
  {$IFDEF HEXEN}
    if LevelUseFog then
      R_GetFogPrecalc32Tables(lfactor3, bf_r3, bf_g3, bf_b3)
    else
  {$ENDIF}
    R_GetPrecalc32Tables(lfactor3, bf_r3, bf_g3, bf_b3, w.dc_fog);  // JVAL: Mars fog sectors
  inc(w);

  fracstep4 := w.dc_iscale;
  frac4 := w.dc_texturemid + xx * fracstep4;
  dc_source32_4 := w.dc_source32;
  if w.dc_texturefactorbits > 0 then
  begin
    fracstep4 := fracstep4 * (1 shl w.dc_texturefactorbits);
    frac4 := frac4 * (1 shl w.dc_texturefactorbits);
  end;
  lfactor4 := w.dc_lightlevel;
  if lfactor4 >= 0 then
  {$IFDEF HEXEN}
    if LevelUseFog then
      R_GetFogPrecalc32Tables(lfactor4, bf_r4, bf_g4, bf_b4)
    else
  {$ENDIF}
    R_GetPrecalc32Tables(lfactor4, bf_r4, bf_g4, bf_b4, w.dc_fog);  // JVAL: Mars fog sectors
  inc(w);

  fracstep5 := w.dc_iscale;
  frac5 := w.dc_texturemid + xx * fracstep5;
  dc_source32_5 := w.dc_source32;
  if w.dc_texturefactorbits > 0 then
  begin
    fracstep5 := fracstep5 * (1 shl w.dc_texturefactorbits);
    frac5 := frac5 * (1 shl w.dc_texturefactorbits);
  end;
  lfactor5 := w.dc_lightlevel;
  if lfactor5 >= 0 then
  {$IFDEF HEXEN}
    if LevelUseFog then
      R_GetFogPrecalc32Tables(lfactor5, bf_r5, bf_g5, bf_b5)
    else
  {$ENDIF}
    R_GetPrecalc32Tables(lfactor5, bf_r5, bf_g5, bf_b5, w.dc_fog);  // JVAL: Mars fog sectors
  inc(w);

  fracstep6 := w.dc_iscale;
  frac6 := w.dc_texturemid + xx * fracstep6;
  dc_source32_6 := w.dc_source32;
  if w.dc_texturefactorbits > 0 then
  begin
    fracstep6 := fracstep6 * (1 shl w.dc_texturefactorbits);
    frac6 := frac6 * (1 shl w.dc_texturefactorbits);
  end;
  lfactor6 := w.dc_lightlevel;
  if lfactor6 >= 0 then
  {$IFDEF HEXEN}
    if LevelUseFog then
      R_GetFogPrecalc32Tables(lfactor6, bf_r6, bf_g6, bf_b6)
    else
  {$ENDIF}
    R_GetPrecalc32Tables(lfactor6, bf_r6, bf_g6, bf_b6, w.dc_fog);  // JVAL: Mars fog sectors
  inc(w);

  fracstep7 := w.dc_iscale;
  frac7 := w.dc_texturemid + xx * fracstep7;
  dc_source32_7 := w.dc_source32;
  if w.dc_texturefactorbits > 0 then
  begin
    fracstep7 := fracstep7 * (1 shl w.dc_texturefactorbits);
    frac7 := frac7 * (1 shl w.dc_texturefactorbits);
  end;
  lfactor7 := w.dc_lightlevel;
  if lfactor7 >= 0 then
  {$IFDEF HEXEN}
    if LevelUseFog then
      R_GetFogPrecalc32Tables(lfactor7, bf_r7, bf_g7, bf_b7)
    else
  {$ENDIF}
    R_GetPrecalc32Tables(lfactor7, bf_r7, bf_g7, bf_b7, w.dc_fog);  // JVAL: Mars fog sectors
  inc(w);

  fracstep8 := w.dc_iscale;
  frac8 := w.dc_texturemid + xx * fracstep8;
  dc_source32_8 := w.dc_source32;
  if w.dc_texturefactorbits > 0 then
  begin
    fracstep8 := fracstep8 * (1 shl w.dc_texturefactorbits);
    frac8 := frac8 * (1 shl w.dc_texturefactorbits);
  end;
  lfactor8 := w.dc_lightlevel;
  if lfactor8 >= 0 then
  {$IFDEF HEXEN}
    if LevelUseFog then
      R_GetFogPrecalc32Tables(lfactor8, bf_r8, bf_g8, bf_b8)
    else
  {$ENDIF}
    R_GetPrecalc32Tables(lfactor8, bf_r8, bf_g8, bf_b8, w.dc_fog);  // JVAL: Mars fog sectors

  swidth := SCREENWIDTH32PITCH - (MAXBATCHWALLS - 1) * SizeOf(LongWord);

  ypos := min_yl;

  if and_mask = 127 then
  begin
    {$UNDEF MASK_TC}
    {$DEFINE MASK_127}
    {$UNDEF MASK_255}
    {$UNDEF MASK_511}
    {$UNDEF MASK_1023}
    {$I R_Wall32_BatchFunc8.inc}
  end
  else if and_mask = 255 then
  begin
    {$UNDEF MASK_TC}
    {$UNDEF MASK_127}
    {$DEFINE MASK_255}
    {$UNDEF MASK_511}
    {$UNDEF MASK_1023}
    {$I R_Wall32_BatchFunc8.inc}
  end
  else if and_mask = 511 then
  begin
    {$UNDEF MASK_TC}
    {$UNDEF MASK_127}
    {$UNDEF MASK_255}
    {$DEFINE MASK_511}
    {$UNDEF MASK_1023}
    {$I R_Wall32_BatchFunc8.inc}
  end
  else if and_mask = 1023 then
  begin
    {$UNDEF MASK_TC}
    {$UNDEF MASK_127}
    {$UNDEF MASK_255}
    {$UNDEF MASK_511}
    {$DEFINE MASK_1023}
    {$I R_Wall32_BatchFunc8.inc}
  end
  else
  begin
    {$IFNDEF WALL32_TC}
    mod_height := and_mask + 1;
    {$ENDIF}
    {$DEFINE MASK_TC}
    {$UNDEF MASK_127}
    {$UNDEF MASK_255}
    {$UNDEF MASK_511}
    {$UNDEF MASK_1023}
    {$I R_Wall32_BatchFunc8.inc}
  end;
end;

{$IFDEF WALL32_128}
procedure R_DrawBatchColumnHi7_128(const walls: Pbatchwallrenderinfo32_t);
{$ENDIF}
{$IFDEF WALL32_256}
procedure R_DrawBatchColumnHi7_256(const walls: Pbatchwallrenderinfo32_t);
{$ENDIF}
{$IFDEF WALL32_512}
procedure R_DrawBatchColumnHi7_512(const walls: Pbatchwallrenderinfo32_t);
{$ENDIF}
{$IFDEF WALL32_TC}
procedure R_DrawBatchColumnHi7_TC(const walls: Pbatchwallrenderinfo32_t);
{$ENDIF}
var
  w: Pwallrenderinfo32_t;
  ypos: integer;
  destl: PLongWord;
  frac1, frac2, frac3, frac4, frac5, frac6, frac7: fixed_t;
  fracstep1, fracstep2, fracstep3, fracstep4, fracstep5, fracstep6, fracstep7: fixed_t;
  spot: integer;
  swidth: integer;

  r1, g1, b1: byte;
  c : LongWord;
  lfactor1, lfactor2, lfactor3, lfactor4, lfactor5, lfactor6, lfactor7: integer;
  mod_height: LongWord;
  and_mask: integer;
  bf_r1, bf_g1, bf_b1: PIntegerArray;
  bf_r2, bf_g2, bf_b2: PIntegerArray;
  bf_r3, bf_g3, bf_b3: PIntegerArray;
  bf_r4, bf_g4, bf_b4: PIntegerArray;
  bf_r5, bf_g5, bf_b5: PIntegerArray;
  bf_r6, bf_g6, bf_b6: PIntegerArray;
  bf_r7, bf_g7, bf_b7: PIntegerArray;
  dc_source32_1: PLongWordArray;
  dc_source32_2: PLongWordArray;
  dc_source32_3: PLongWordArray;
  dc_source32_4: PLongWordArray;
  dc_source32_5: PLongWordArray;
  dc_source32_6: PLongWordArray;
  dc_source32_7: PLongWordArray;

  xx: integer;
  i: integer;

  min_yh, max_yl: integer;
  max_yh, min_yl: integer;
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

  w := @walls.walls[0];
  destl := @((ylookupl[min_yl]^)[columnofs[w.dc_x]]);

  xx := min_yl - centery;

  fracstep1 := w.dc_iscale;
  frac1 := w.dc_texturemid + xx * fracstep1;
  dc_source32_1 := w.dc_source32;
  if w.dc_texturefactorbits > 0 then
  begin
    fracstep1 := fracstep1 * (1 shl w.dc_texturefactorbits);
    frac1 := frac1 * (1 shl w.dc_texturefactorbits);
    {$IFDEF WALL32_TC}
    mod_height := walls.dc_height * (1 shl w.dc_texturefactorbits);
    {$ELSE}
    and_mask := {$IFDEF WALL32_128}128{$ENDIF}{$IFDEF WALL32_256}256{$ENDIF}{$IFDEF WALL32_512}512{$ENDIF} * (1 shl w.dc_texturefactorbits) - 1;
    {$ENDIF}
  end
  else
    {$IFDEF WALL32_TC}
    mod_height := walls.dc_height;
    and_mask := mod_height - 1;
    {$ELSE}
    and_mask := {$IFDEF WALL32_128}127{$ENDIF}{$IFDEF WALL32_256}255{$ENDIF}{$IFDEF WALL32_512}511{$ENDIF};
    {$ENDIF}
  lfactor1 := w.dc_lightlevel;
  if lfactor1 >= 0 then
  {$IFDEF HEXEN}
    if LevelUseFog then
      R_GetFogPrecalc32Tables(lfactor1, bf_r1, bf_g1, bf_b1)
    else
  {$ENDIF}
    R_GetPrecalc32Tables(lfactor1, bf_r1, bf_g1, bf_b1, w.dc_fog);  // JVAL: Mars fog sectors
  inc(w);

  fracstep2 := w.dc_iscale;
  frac2 := w.dc_texturemid + xx * fracstep2;
  dc_source32_2 := w.dc_source32;
  if w.dc_texturefactorbits > 0 then
  begin
    fracstep2 := fracstep2 * (1 shl w.dc_texturefactorbits);
    frac2 := frac2 * (1 shl w.dc_texturefactorbits);
  end;
  lfactor2 := w.dc_lightlevel;
  if lfactor2 >= 0 then
  {$IFDEF HEXEN}
    if LevelUseFog then
      R_GetFogPrecalc32Tables(lfactor2, bf_r2, bf_g2, bf_b2)
    else
  {$ENDIF}
    R_GetPrecalc32Tables(lfactor2, bf_r2, bf_g2, bf_b2, w.dc_fog);  // JVAL: Mars fog sectors
  inc(w);

  fracstep3 := w.dc_iscale;
  frac3 := w.dc_texturemid + xx * fracstep3;
  dc_source32_3 := w.dc_source32;
  if w.dc_texturefactorbits > 0 then
  begin
    fracstep3 := fracstep3 * (1 shl w.dc_texturefactorbits);
    frac3 := frac3 * (1 shl w.dc_texturefactorbits);
  end;
  lfactor3 := w.dc_lightlevel;
  if lfactor3 >= 0 then
  {$IFDEF HEXEN}
    if LevelUseFog then
      R_GetFogPrecalc32Tables(lfactor3, bf_r3, bf_g3, bf_b3)
    else
  {$ENDIF}
    R_GetPrecalc32Tables(lfactor3, bf_r3, bf_g3, bf_b3, w.dc_fog);  // JVAL: Mars fog sectors
  inc(w);

  fracstep4 := w.dc_iscale;
  frac4 := w.dc_texturemid + xx * fracstep4;
  dc_source32_4 := w.dc_source32;
  if w.dc_texturefactorbits > 0 then
  begin
    fracstep4 := fracstep4 * (1 shl w.dc_texturefactorbits);
    frac4 := frac4 * (1 shl w.dc_texturefactorbits);
  end;
  lfactor4 := w.dc_lightlevel;
  if lfactor4 >= 0 then
  {$IFDEF HEXEN}
    if LevelUseFog then
      R_GetFogPrecalc32Tables(lfactor4, bf_r4, bf_g4, bf_b4)
    else
  {$ENDIF}
    R_GetPrecalc32Tables(lfactor4, bf_r4, bf_g4, bf_b4, w.dc_fog);  // JVAL: Mars fog sectors
  inc(w);

  fracstep5 := w.dc_iscale;
  frac5 := w.dc_texturemid + xx * fracstep5;
  dc_source32_5 := w.dc_source32;
  if w.dc_texturefactorbits > 0 then
  begin
    fracstep5 := fracstep5 * (1 shl w.dc_texturefactorbits);
    frac5 := frac5 * (1 shl w.dc_texturefactorbits);
  end;
  lfactor5 := w.dc_lightlevel;
  if lfactor5 >= 0 then
  {$IFDEF HEXEN}
    if LevelUseFog then
      R_GetFogPrecalc32Tables(lfactor5, bf_r5, bf_g5, bf_b5)
    else
  {$ENDIF}
    R_GetPrecalc32Tables(lfactor5, bf_r5, bf_g5, bf_b5, w.dc_fog);  // JVAL: Mars fog sectors
  inc(w);

  fracstep6 := w.dc_iscale;
  frac6 := w.dc_texturemid + xx * fracstep6;
  dc_source32_6 := w.dc_source32;
  if w.dc_texturefactorbits > 0 then
  begin
    fracstep6 := fracstep6 * (1 shl w.dc_texturefactorbits);
    frac6 := frac6 * (1 shl w.dc_texturefactorbits);
  end;
  lfactor6 := w.dc_lightlevel;
  if lfactor6 >= 0 then
  {$IFDEF HEXEN}
    if LevelUseFog then
      R_GetFogPrecalc32Tables(lfactor6, bf_r6, bf_g6, bf_b6)
    else
  {$ENDIF}
    R_GetPrecalc32Tables(lfactor6, bf_r6, bf_g6, bf_b6, w.dc_fog);  // JVAL: Mars fog sectors
  inc(w);

  fracstep7 := w.dc_iscale;
  frac7 := w.dc_texturemid + xx * fracstep7;
  dc_source32_7 := w.dc_source32;
  if w.dc_texturefactorbits > 0 then
  begin
    fracstep7 := fracstep7 * (1 shl w.dc_texturefactorbits);
    frac7 := frac7 * (1 shl w.dc_texturefactorbits);
  end;
  lfactor7 := w.dc_lightlevel;
  if lfactor7 >= 0 then
  {$IFDEF HEXEN}
    if LevelUseFog then
      R_GetFogPrecalc32Tables(lfactor7, bf_r7, bf_g7, bf_b7)
    else
  {$ENDIF}
    R_GetPrecalc32Tables(lfactor7, bf_r7, bf_g7, bf_b7, w.dc_fog);  // JVAL: Mars fog sectors

  swidth := SCREENWIDTH32PITCH - 6 * SizeOf(LongWord);

  ypos := min_yl;
  if and_mask = 127 then
  begin
    {$UNDEF MASK_TC}
    {$DEFINE MASK_127}
    {$UNDEF MASK_255}
    {$UNDEF MASK_511}
    {$UNDEF MASK_1023}
    {$I R_Wall32_BatchFunc7.inc}
  end
  else if and_mask = 255 then
  begin
    {$UNDEF MASK_TC}
    {$UNDEF MASK_127}
    {$DEFINE MASK_255}
    {$UNDEF MASK_511}
    {$UNDEF MASK_1023}
    {$I R_Wall32_BatchFunc7.inc}
  end
  else if and_mask = 511 then
  begin
    {$UNDEF MASK_TC}
    {$UNDEF MASK_127}
    {$UNDEF MASK_255}
    {$DEFINE MASK_511}
    {$UNDEF MASK_1023}
    {$I R_Wall32_BatchFunc7.inc}
  end
  else if and_mask = 1023 then
  begin
    {$UNDEF MASK_TC}
    {$UNDEF MASK_127}
    {$UNDEF MASK_255}
    {$UNDEF MASK_511}
    {$DEFINE MASK_1023}
    {$I R_Wall32_BatchFunc7.inc}
  end
  else
  begin
    {$IFNDEF WALL32_TC}
    mod_height := and_mask + 1;
    {$ENDIF}
    {$DEFINE MASK_TC}
    {$UNDEF MASK_127}
    {$UNDEF MASK_255}
    {$UNDEF MASK_511}
    {$UNDEF MASK_1023}
    {$I R_Wall32_BatchFunc7.inc}
  end;
end;

{$IFDEF WALL32_128}
procedure R_DrawBatchColumnHi6_128(const walls: Pbatchwallrenderinfo32_t);
{$ENDIF}
{$IFDEF WALL32_256}
procedure R_DrawBatchColumnHi6_256(const walls: Pbatchwallrenderinfo32_t);
{$ENDIF}
{$IFDEF WALL32_512}
procedure R_DrawBatchColumnHi6_512(const walls: Pbatchwallrenderinfo32_t);
{$ENDIF}
{$IFDEF WALL32_TC}
procedure R_DrawBatchColumnHi6_TC(const walls: Pbatchwallrenderinfo32_t);
{$ENDIF}
var
  w: Pwallrenderinfo32_t;
  ypos: integer;
  destl: PLongWord;
  frac1, frac2, frac3, frac4, frac5, frac6: fixed_t;
  fracstep1, fracstep2, fracstep3, fracstep4, fracstep5, fracstep6: fixed_t;
  spot: integer;
  swidth: integer;

  r1, g1, b1: byte;
  c : LongWord;
  lfactor1, lfactor2, lfactor3, lfactor4, lfactor5, lfactor6: integer;
  mod_height: LongWord;
  and_mask: integer;
  bf_r1, bf_g1, bf_b1: PIntegerArray;
  bf_r2, bf_g2, bf_b2: PIntegerArray;
  bf_r3, bf_g3, bf_b3: PIntegerArray;
  bf_r4, bf_g4, bf_b4: PIntegerArray;
  bf_r5, bf_g5, bf_b5: PIntegerArray;
  bf_r6, bf_g6, bf_b6: PIntegerArray;
  dc_source32_1: PLongWordArray;
  dc_source32_2: PLongWordArray;
  dc_source32_3: PLongWordArray;
  dc_source32_4: PLongWordArray;
  dc_source32_5: PLongWordArray;
  dc_source32_6: PLongWordArray;

  xx: integer;
  i: integer;

  min_yh, max_yl: integer;
  max_yh, min_yl: integer;
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

  w := @walls.walls[0];
  destl := @((ylookupl[min_yl]^)[columnofs[w.dc_x]]);

  xx := min_yl - centery;

  fracstep1 := w.dc_iscale;
  frac1 := w.dc_texturemid + xx * fracstep1;
  dc_source32_1 := w.dc_source32;
  if w.dc_texturefactorbits > 0 then
  begin
    fracstep1 := fracstep1 * (1 shl w.dc_texturefactorbits);
    frac1 := frac1 * (1 shl w.dc_texturefactorbits);
    {$IFDEF WALL32_TC}
    mod_height := walls.dc_height * (1 shl w.dc_texturefactorbits);
    {$ELSE}
    and_mask := {$IFDEF WALL32_128}128{$ENDIF}{$IFDEF WALL32_256}256{$ENDIF}{$IFDEF WALL32_512}512{$ENDIF} * (1 shl w.dc_texturefactorbits) - 1;
    {$ENDIF}
  end
  else
    {$IFDEF WALL32_TC}
    mod_height := walls.dc_height;
    and_mask := mod_height - 1;
    {$ELSE}
    and_mask := {$IFDEF WALL32_128}127{$ENDIF}{$IFDEF WALL32_256}255{$ENDIF}{$IFDEF WALL32_512}511{$ENDIF};
    {$ENDIF}
  lfactor1 := w.dc_lightlevel;
  if lfactor1 >= 0 then
  {$IFDEF HEXEN}
    if LevelUseFog then
      R_GetFogPrecalc32Tables(lfactor1, bf_r1, bf_g1, bf_b1)
    else
  {$ENDIF}
    R_GetPrecalc32Tables(lfactor1, bf_r1, bf_g1, bf_b1, w.dc_fog);  // JVAL: Mars fog sectors
  inc(w);

  fracstep2 := w.dc_iscale;
  frac2 := w.dc_texturemid + xx * fracstep2;
  dc_source32_2 := w.dc_source32;
  if w.dc_texturefactorbits > 0 then
  begin
    fracstep2 := fracstep2 * (1 shl w.dc_texturefactorbits);
    frac2 := frac2 * (1 shl w.dc_texturefactorbits);
  end;
  lfactor2 := w.dc_lightlevel;
  if lfactor2 >= 0 then
  {$IFDEF HEXEN}
    if LevelUseFog then
      R_GetFogPrecalc32Tables(lfactor2, bf_r2, bf_g2, bf_b2)
    else
  {$ENDIF}
    R_GetPrecalc32Tables(lfactor2, bf_r2, bf_g2, bf_b2, w.dc_fog);  // JVAL: Mars fog sectors
  inc(w);

  fracstep3 := w.dc_iscale;
  frac3 := w.dc_texturemid + xx * fracstep3;
  dc_source32_3 := w.dc_source32;
  if w.dc_texturefactorbits > 0 then
  begin
    fracstep3 := fracstep3 * (1 shl w.dc_texturefactorbits);
    frac3 := frac3 * (1 shl w.dc_texturefactorbits);
  end;
  lfactor3 := w.dc_lightlevel;
  if lfactor3 >= 0 then
  {$IFDEF HEXEN}
    if LevelUseFog then
      R_GetFogPrecalc32Tables(lfactor3, bf_r3, bf_g3, bf_b3)
    else
  {$ENDIF}
    R_GetPrecalc32Tables(lfactor3, bf_r3, bf_g3, bf_b3, w.dc_fog);  // JVAL: Mars fog sectors
  inc(w);

  fracstep4 := w.dc_iscale;
  frac4 := w.dc_texturemid + xx * fracstep4;
  dc_source32_4 := w.dc_source32;
  if w.dc_texturefactorbits > 0 then
  begin
    fracstep4 := fracstep4 * (1 shl w.dc_texturefactorbits);
    frac4 := frac4 * (1 shl w.dc_texturefactorbits);
  end;
  lfactor4 := w.dc_lightlevel;
  if lfactor4 >= 0 then
  {$IFDEF HEXEN}
    if LevelUseFog then
      R_GetFogPrecalc32Tables(lfactor4, bf_r4, bf_g4, bf_b4)
    else
  {$ENDIF}
    R_GetPrecalc32Tables(lfactor4, bf_r4, bf_g4, bf_b4, w.dc_fog);  // JVAL: Mars fog sectors
  inc(w);

  fracstep5 := w.dc_iscale;
  frac5 := w.dc_texturemid + xx * fracstep5;
  dc_source32_5 := w.dc_source32;
  if w.dc_texturefactorbits > 0 then
  begin
    fracstep5 := fracstep5 * (1 shl w.dc_texturefactorbits);
    frac5 := frac5 * (1 shl w.dc_texturefactorbits);
  end;
  lfactor5 := w.dc_lightlevel;
  if lfactor5 >= 0 then
  {$IFDEF HEXEN}
    if LevelUseFog then
      R_GetFogPrecalc32Tables(lfactor5, bf_r5, bf_g5, bf_b5)
    else
  {$ENDIF}
    R_GetPrecalc32Tables(lfactor5, bf_r5, bf_g5, bf_b5, w.dc_fog);  // JVAL: Mars fog sectors
  inc(w);

  fracstep6 := w.dc_iscale;
  frac6 := w.dc_texturemid + xx * fracstep6;
  dc_source32_6 := w.dc_source32;
  if w.dc_texturefactorbits > 0 then
  begin
    fracstep6 := fracstep6 * (1 shl w.dc_texturefactorbits);
    frac6 := frac6 * (1 shl w.dc_texturefactorbits);
  end;
  lfactor6 := w.dc_lightlevel;
  if lfactor6 >= 0 then
  {$IFDEF HEXEN}
    if LevelUseFog then
      R_GetFogPrecalc32Tables(lfactor6, bf_r6, bf_g6, bf_b6)
    else
  {$ENDIF}
    R_GetPrecalc32Tables(lfactor6, bf_r6, bf_g6, bf_b6, w.dc_fog);  // JVAL: Mars fog sectors

  swidth := SCREENWIDTH32PITCH - 5 * SizeOf(LongWord);

  ypos := min_yl;
  if and_mask = 127 then
  begin
    {$UNDEF MASK_TC}
    {$DEFINE MASK_127}
    {$UNDEF MASK_255}
    {$UNDEF MASK_511}
    {$UNDEF MASK_1023}
    {$I R_Wall32_BatchFunc6.inc}
  end
  else if and_mask = 255 then
  begin
    {$UNDEF MASK_TC}
    {$UNDEF MASK_127}
    {$DEFINE MASK_255}
    {$UNDEF MASK_511}
    {$UNDEF MASK_1023}
    {$I R_Wall32_BatchFunc6.inc}
  end
  else if and_mask = 511 then
  begin
    {$UNDEF MASK_TC}
    {$UNDEF MASK_127}
    {$UNDEF MASK_255}
    {$DEFINE MASK_511}
    {$UNDEF MASK_1023}
    {$I R_Wall32_BatchFunc6.inc}
  end
  else if and_mask = 1023 then
  begin
    {$UNDEF MASK_TC}
    {$UNDEF MASK_127}
    {$UNDEF MASK_255}
    {$UNDEF MASK_511}
    {$DEFINE MASK_1023}
    {$I R_Wall32_BatchFunc6.inc}
  end
  else
  begin
    {$IFNDEF WALL32_TC}
    mod_height := and_mask + 1;
    {$ENDIF}
    {$DEFINE MASK_TC}
    {$UNDEF MASK_127}
    {$UNDEF MASK_255}
    {$UNDEF MASK_511}
    {$UNDEF MASK_1023}
    {$I R_Wall32_BatchFunc6.inc}
  end;
end;

{$IFDEF WALL32_128}
procedure R_DrawBatchColumnHi5_128(const walls: Pbatchwallrenderinfo32_t);
{$ENDIF}
{$IFDEF WALL32_256}
procedure R_DrawBatchColumnHi5_256(const walls: Pbatchwallrenderinfo32_t);
{$ENDIF}
{$IFDEF WALL32_512}
procedure R_DrawBatchColumnHi5_512(const walls: Pbatchwallrenderinfo32_t);
{$ENDIF}
{$IFDEF WALL32_TC}
procedure R_DrawBatchColumnHi5_TC(const walls: Pbatchwallrenderinfo32_t);
{$ENDIF}
var
  w: Pwallrenderinfo32_t;
  ypos: integer;
  destl: PLongWord;
  frac1, frac2, frac3, frac4, frac5: fixed_t;
  fracstep1, fracstep2, fracstep3, fracstep4, fracstep5: fixed_t;
  spot: integer;
  swidth: integer;

  r1, g1, b1: byte;
  c : LongWord;
  lfactor1, lfactor2, lfactor3, lfactor4, lfactor5: integer;
  mod_height: LongWord;
  and_mask: integer;
  bf_r1, bf_g1, bf_b1: PIntegerArray;
  bf_r2, bf_g2, bf_b2: PIntegerArray;
  bf_r3, bf_g3, bf_b3: PIntegerArray;
  bf_r4, bf_g4, bf_b4: PIntegerArray;
  bf_r5, bf_g5, bf_b5: PIntegerArray;
  dc_source32_1: PLongWordArray;
  dc_source32_2: PLongWordArray;
  dc_source32_3: PLongWordArray;
  dc_source32_4: PLongWordArray;
  dc_source32_5: PLongWordArray;

  xx: integer;
  i: integer;

  min_yh, max_yl: integer;
  max_yh, min_yl: integer;
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

  w := @walls.walls[0];
  destl := @((ylookupl[min_yl]^)[columnofs[w.dc_x]]);

  xx := min_yl - centery;

  fracstep1 := w.dc_iscale;
  frac1 := w.dc_texturemid + xx * fracstep1;
  dc_source32_1 := w.dc_source32;
  if w.dc_texturefactorbits > 0 then
  begin
    fracstep1 := fracstep1 * (1 shl w.dc_texturefactorbits);
    frac1 := frac1 * (1 shl w.dc_texturefactorbits);
    {$IFDEF WALL32_TC}
    mod_height := walls.dc_height * (1 shl w.dc_texturefactorbits);
    {$ELSE}
    and_mask := {$IFDEF WALL32_128}128{$ENDIF}{$IFDEF WALL32_256}256{$ENDIF}{$IFDEF WALL32_512}512{$ENDIF} * (1 shl w.dc_texturefactorbits) - 1;
    {$ENDIF}
  end
  else
    {$IFDEF WALL32_TC}
    mod_height := walls.dc_height;
    and_mask := mod_height - 1;
    {$ELSE}
    and_mask := {$IFDEF WALL32_128}127{$ENDIF}{$IFDEF WALL32_256}255{$ENDIF}{$IFDEF WALL32_512}511{$ENDIF};
    {$ENDIF}
  lfactor1 := w.dc_lightlevel;
  if lfactor1 >= 0 then
  {$IFDEF HEXEN}
    if LevelUseFog then
      R_GetFogPrecalc32Tables(lfactor1, bf_r1, bf_g1, bf_b1)
    else
  {$ENDIF}
    R_GetPrecalc32Tables(lfactor1, bf_r1, bf_g1, bf_b1, w.dc_fog);  // JVAL: Mars fog sectors
  inc(w);

  fracstep2 := w.dc_iscale;
  frac2 := w.dc_texturemid + xx * fracstep2;
  dc_source32_2 := w.dc_source32;
  if w.dc_texturefactorbits > 0 then
  begin
    fracstep2 := fracstep2 * (1 shl w.dc_texturefactorbits);
    frac2 := frac2 * (1 shl w.dc_texturefactorbits);
  end;
  lfactor2 := w.dc_lightlevel;
  if lfactor2 >= 0 then
  {$IFDEF HEXEN}
    if LevelUseFog then
      R_GetFogPrecalc32Tables(lfactor2, bf_r2, bf_g2, bf_b2)
    else
  {$ENDIF}
    R_GetPrecalc32Tables(lfactor2, bf_r2, bf_g2, bf_b2, w.dc_fog);  // JVAL: Mars fog sectors
  inc(w);

  fracstep3 := w.dc_iscale;
  frac3 := w.dc_texturemid + xx * fracstep3;
  dc_source32_3 := w.dc_source32;
  if w.dc_texturefactorbits > 0 then
  begin
    fracstep3 := fracstep3 * (1 shl w.dc_texturefactorbits);
    frac3 := frac3 * (1 shl w.dc_texturefactorbits);
  end;
  lfactor3 := w.dc_lightlevel;
  if lfactor3 >= 0 then
  {$IFDEF HEXEN}
    if LevelUseFog then
      R_GetFogPrecalc32Tables(lfactor3, bf_r3, bf_g3, bf_b3)
    else
  {$ENDIF}
    R_GetPrecalc32Tables(lfactor3, bf_r3, bf_g3, bf_b3, w.dc_fog);  // JVAL: Mars fog sectors
  inc(w);

  fracstep4 := w.dc_iscale;
  frac4 := w.dc_texturemid + xx * fracstep4;
  dc_source32_4 := w.dc_source32;
  if w.dc_texturefactorbits > 0 then
  begin
    fracstep4 := fracstep4 * (1 shl w.dc_texturefactorbits);
    frac4 := frac4 * (1 shl w.dc_texturefactorbits);
  end;
  lfactor4 := w.dc_lightlevel;
  if lfactor4 >= 0 then
  {$IFDEF HEXEN}
    if LevelUseFog then
      R_GetFogPrecalc32Tables(lfactor4, bf_r4, bf_g4, bf_b4)
    else
  {$ENDIF}
    R_GetPrecalc32Tables(lfactor4, bf_r4, bf_g4, bf_b4, w.dc_fog);  // JVAL: Mars fog sectors
  inc(w);

  fracstep5 := w.dc_iscale;
  frac5 := w.dc_texturemid + xx * fracstep5;
  dc_source32_5 := w.dc_source32;
  if w.dc_texturefactorbits > 0 then
  begin
    fracstep5 := fracstep5 * (1 shl w.dc_texturefactorbits);
    frac5 := frac5 * (1 shl w.dc_texturefactorbits);
  end;
  lfactor5 := w.dc_lightlevel;
  if lfactor5 >= 0 then
  {$IFDEF HEXEN}
    if LevelUseFog then
      R_GetFogPrecalc32Tables(lfactor5, bf_r5, bf_g5, bf_b5)
    else
  {$ENDIF}
    R_GetPrecalc32Tables(lfactor5, bf_r5, bf_g5, bf_b5, w.dc_fog);  // JVAL: Mars fog sectors

  swidth := SCREENWIDTH32PITCH - 4 * SizeOf(LongWord);

  ypos := min_yl;
  if and_mask = 127 then
  begin
    {$UNDEF MASK_TC}
    {$DEFINE MASK_127}
    {$UNDEF MASK_255}
    {$UNDEF MASK_511}
    {$UNDEF MASK_1023}
    {$I R_Wall32_BatchFunc5.inc}
  end
  else if and_mask = 255 then
  begin
    {$UNDEF MASK_TC}
    {$UNDEF MASK_127}
    {$DEFINE MASK_255}
    {$UNDEF MASK_511}
    {$UNDEF MASK_1023}
    {$I R_Wall32_BatchFunc5.inc}
  end
  else if and_mask = 511 then
  begin
    {$UNDEF MASK_TC}
    {$UNDEF MASK_127}
    {$UNDEF MASK_255}
    {$DEFINE MASK_511}
    {$UNDEF MASK_1023}
    {$I R_Wall32_BatchFunc5.inc}
  end
  else if and_mask = 1023 then
  begin
    {$UNDEF MASK_TC}
    {$UNDEF MASK_127}
    {$UNDEF MASK_255}
    {$UNDEF MASK_511}
    {$DEFINE MASK_1023}
    {$I R_Wall32_BatchFunc5.inc}
  end
  else
  begin
    {$IFNDEF WALL32_TC}
    mod_height := and_mask + 1;
    {$ENDIF}
    {$DEFINE MASK_TC}
    {$UNDEF MASK_127}
    {$UNDEF MASK_255}
    {$UNDEF MASK_511}
    {$UNDEF MASK_1023}
    {$I R_Wall32_BatchFunc5.inc}
  end;
end;

{$IFDEF WALL32_128}
procedure R_DrawBatchColumnHi4_128(const walls: Pbatchwallrenderinfo32_t);
{$ENDIF}
{$IFDEF WALL32_256}
procedure R_DrawBatchColumnHi4_256(const walls: Pbatchwallrenderinfo32_t);
{$ENDIF}
{$IFDEF WALL32_512}
procedure R_DrawBatchColumnHi4_512(const walls: Pbatchwallrenderinfo32_t);
{$ENDIF}
{$IFDEF WALL32_TC}
procedure R_DrawBatchColumnHi4_TC(const walls: Pbatchwallrenderinfo32_t);
{$ENDIF}
var
  w: Pwallrenderinfo32_t;
  ypos: integer;
  destl: PLongWord;
  frac1, frac2, frac3, frac4: fixed_t;
  fracstep1, fracstep2, fracstep3, fracstep4: fixed_t;
  spot: integer;
  swidth: integer;

  r1, g1, b1: byte;
  c : LongWord;
  lfactor1, lfactor2, lfactor3, lfactor4: integer;
  mod_height: LongWord;
  and_mask: integer;
  bf_r1, bf_g1, bf_b1: PIntegerArray;
  bf_r2, bf_g2, bf_b2: PIntegerArray;
  bf_r3, bf_g3, bf_b3: PIntegerArray;
  bf_r4, bf_g4, bf_b4: PIntegerArray;
  dc_source32_1: PLongWordArray;
  dc_source32_2: PLongWordArray;
  dc_source32_3: PLongWordArray;
  dc_source32_4: PLongWordArray;

  xx: integer;
  i: integer;

  min_yh, max_yl: integer;
  max_yh, min_yl: integer;
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

  w := @walls.walls[0];
  destl := @((ylookupl[min_yl]^)[columnofs[w.dc_x]]);

  xx := min_yl - centery;

  fracstep1 := w.dc_iscale;
  frac1 := w.dc_texturemid + xx * fracstep1;
  dc_source32_1 := w.dc_source32;
  if w.dc_texturefactorbits > 0 then
  begin
    fracstep1 := fracstep1 * (1 shl w.dc_texturefactorbits);
    frac1 := frac1 * (1 shl w.dc_texturefactorbits);
    {$IFDEF WALL32_TC}
    mod_height := walls.dc_height * (1 shl w.dc_texturefactorbits);
    {$ELSE}
    and_mask := {$IFDEF WALL32_128}128{$ENDIF}{$IFDEF WALL32_256}256{$ENDIF}{$IFDEF WALL32_512}512{$ENDIF} * (1 shl w.dc_texturefactorbits) - 1;
    {$ENDIF}
  end
  else
    {$IFDEF WALL32_TC}
    mod_height := walls.dc_height;
    and_mask := mod_height - 1;
    {$ELSE}
    and_mask := {$IFDEF WALL32_128}127{$ENDIF}{$IFDEF WALL32_256}255{$ENDIF}{$IFDEF WALL32_512}511{$ENDIF};
    {$ENDIF}
  lfactor1 := w.dc_lightlevel;
  if lfactor1 >= 0 then
  {$IFDEF HEXEN}
    if LevelUseFog then
      R_GetFogPrecalc32Tables(lfactor1, bf_r1, bf_g1, bf_b1)
    else
  {$ENDIF}
    R_GetPrecalc32Tables(lfactor1, bf_r1, bf_g1, bf_b1, w.dc_fog);  // JVAL: Mars fog sectors
  inc(w);

  fracstep2 := w.dc_iscale;
  frac2 := w.dc_texturemid + xx * fracstep2;
  dc_source32_2 := w.dc_source32;
  if w.dc_texturefactorbits > 0 then
  begin
    fracstep2 := fracstep2 * (1 shl w.dc_texturefactorbits);
    frac2 := frac2 * (1 shl w.dc_texturefactorbits);
  end;
  lfactor2 := w.dc_lightlevel;
  if lfactor2 >= 0 then
  {$IFDEF HEXEN}
    if LevelUseFog then
      R_GetFogPrecalc32Tables(lfactor2, bf_r2, bf_g2, bf_b2)
    else
  {$ENDIF}
    R_GetPrecalc32Tables(lfactor2, bf_r2, bf_g2, bf_b2, w.dc_fog);  // JVAL: Mars fog sectors
  inc(w);

  fracstep3 := w.dc_iscale;
  frac3 := w.dc_texturemid + xx * fracstep3;
  dc_source32_3 := w.dc_source32;
  if w.dc_texturefactorbits > 0 then
  begin
    fracstep3 := fracstep3 * (1 shl w.dc_texturefactorbits);
    frac3 := frac3 * (1 shl w.dc_texturefactorbits);
  end;
  lfactor3 := w.dc_lightlevel;
  if lfactor3 >= 0 then
  {$IFDEF HEXEN}
    if LevelUseFog then
      R_GetFogPrecalc32Tables(lfactor3, bf_r3, bf_g3, bf_b3)
    else
  {$ENDIF}
    R_GetPrecalc32Tables(lfactor3, bf_r3, bf_g3, bf_b3, w.dc_fog);  // JVAL: Mars fog sectors
  inc(w);

  fracstep4 := w.dc_iscale;
  frac4 := w.dc_texturemid + xx * fracstep4;
  dc_source32_4 := w.dc_source32;
  if w.dc_texturefactorbits > 0 then
  begin
    fracstep4 := fracstep4 * (1 shl w.dc_texturefactorbits);
    frac4 := frac4 * (1 shl w.dc_texturefactorbits);
  end;
  lfactor4 := w.dc_lightlevel;
  if lfactor4 >= 0 then
  {$IFDEF HEXEN}
    if LevelUseFog then
      R_GetFogPrecalc32Tables(lfactor4, bf_r4, bf_g4, bf_b4)
    else
  {$ENDIF}
    R_GetPrecalc32Tables(lfactor4, bf_r4, bf_g4, bf_b4, w.dc_fog);  // JVAL: Mars fog sectors

  swidth := SCREENWIDTH32PITCH - 3 * SizeOf(LongWord);

  ypos := min_yl;
  if and_mask = 127 then
  begin
    {$UNDEF MASK_TC}
    {$DEFINE MASK_127}
    {$UNDEF MASK_255}
    {$UNDEF MASK_511}
    {$UNDEF MASK_1023}
    {$I R_Wall32_BatchFunc4.inc}
  end
  else if and_mask = 255 then
  begin
    {$UNDEF MASK_TC}
    {$UNDEF MASK_127}
    {$DEFINE MASK_255}
    {$UNDEF MASK_511}
    {$UNDEF MASK_1023}
    {$I R_Wall32_BatchFunc4.inc}
  end
  else if and_mask = 511 then
  begin
    {$UNDEF MASK_TC}
    {$UNDEF MASK_127}
    {$UNDEF MASK_255}
    {$DEFINE MASK_511}
    {$UNDEF MASK_1023}
    {$I R_Wall32_BatchFunc4.inc}
  end
  else if and_mask = 1023 then
  begin
    {$UNDEF MASK_TC}
    {$UNDEF MASK_127}
    {$UNDEF MASK_255}
    {$UNDEF MASK_511}
    {$DEFINE MASK_1023}
    {$I R_Wall32_BatchFunc4.inc}
  end
  else
  begin
    {$IFNDEF WALL32_TC}
    mod_height := and_mask + 1;
    {$ENDIF}
    {$DEFINE MASK_TC}
    {$UNDEF MASK_127}
    {$UNDEF MASK_255}
    {$UNDEF MASK_511}
    {$UNDEF MASK_1023}
    {$I R_Wall32_BatchFunc4.inc}
  end;
end;

{$IFDEF WALL32_128}
procedure R_DrawBatchColumnHi3_128(const walls: Pbatchwallrenderinfo32_t);
{$ENDIF}
{$IFDEF WALL32_256}
procedure R_DrawBatchColumnHi3_256(const walls: Pbatchwallrenderinfo32_t);
{$ENDIF}
{$IFDEF WALL32_512}
procedure R_DrawBatchColumnHi3_512(const walls: Pbatchwallrenderinfo32_t);
{$ENDIF}
{$IFDEF WALL32_TC}
procedure R_DrawBatchColumnHi3_TC(const walls: Pbatchwallrenderinfo32_t);
{$ENDIF}
var
  w: Pwallrenderinfo32_t;
  ypos: integer;
  destl: PLongWord;
  frac1, frac2, frac3: fixed_t;
  fracstep1, fracstep2, fracstep3: fixed_t;
  spot: integer;
  swidth: integer;

  r1, g1, b1: byte;
  c : LongWord;
  lfactor1, lfactor2, lfactor3: integer;
  mod_height: LongWord;
  and_mask: integer;
  bf_r1, bf_g1, bf_b1: PIntegerArray;
  bf_r2, bf_g2, bf_b2: PIntegerArray;
  bf_r3, bf_g3, bf_b3: PIntegerArray;
  dc_source32_1: PLongWordArray;
  dc_source32_2: PLongWordArray;
  dc_source32_3: PLongWordArray;

  xx: integer;
  i: integer;

  min_yh, max_yl: integer;
  max_yh, min_yl: integer;
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

  w := @walls.walls[0];
  destl := @((ylookupl[min_yl]^)[columnofs[w.dc_x]]);

  xx := min_yl - centery;

  fracstep1 := w.dc_iscale;
  frac1 := w.dc_texturemid + xx * fracstep1;
  dc_source32_1 := w.dc_source32;
  if w.dc_texturefactorbits > 0 then
  begin
    fracstep1 := fracstep1 * (1 shl w.dc_texturefactorbits);
    frac1 := frac1 * (1 shl w.dc_texturefactorbits);
    {$IFDEF WALL32_TC}
    mod_height := walls.dc_height * (1 shl w.dc_texturefactorbits);
    {$ELSE}
    and_mask := {$IFDEF WALL32_128}128{$ENDIF}{$IFDEF WALL32_256}256{$ENDIF}{$IFDEF WALL32_512}512{$ENDIF} * (1 shl w.dc_texturefactorbits) - 1;
    {$ENDIF}
  end
  else
    {$IFDEF WALL32_TC}
    mod_height := walls.dc_height;
    and_mask := mod_height - 1;
    {$ELSE}
    and_mask := {$IFDEF WALL32_128}127{$ENDIF}{$IFDEF WALL32_256}255{$ENDIF}{$IFDEF WALL32_512}511{$ENDIF};
    {$ENDIF}
  lfactor1 := w.dc_lightlevel;
  if lfactor1 >= 0 then
  {$IFDEF HEXEN}
    if LevelUseFog then
      R_GetFogPrecalc32Tables(lfactor1, bf_r1, bf_g1, bf_b1)
    else
  {$ENDIF}
    R_GetPrecalc32Tables(lfactor1, bf_r1, bf_g1, bf_b1, w.dc_fog);  // JVAL: Mars fog sectors
  inc(w);

  fracstep2 := w.dc_iscale;
  frac2 := w.dc_texturemid + xx * fracstep2;
  dc_source32_2 := w.dc_source32;
  if w.dc_texturefactorbits > 0 then
  begin
    fracstep2 := fracstep2 * (1 shl w.dc_texturefactorbits);
    frac2 := frac2 * (1 shl w.dc_texturefactorbits);
  end;
  lfactor2 := w.dc_lightlevel;
  if lfactor2 >= 0 then
  {$IFDEF HEXEN}
    if LevelUseFog then
      R_GetFogPrecalc32Tables(lfactor2, bf_r2, bf_g2, bf_b2)
    else
  {$ENDIF}
    R_GetPrecalc32Tables(lfactor2, bf_r2, bf_g2, bf_b2, w.dc_fog);  // JVAL: Mars fog sectors
  inc(w);

  fracstep3 := w.dc_iscale;
  frac3 := w.dc_texturemid + xx * fracstep3;
  dc_source32_3 := w.dc_source32;
  if w.dc_texturefactorbits > 0 then
  begin
    fracstep3 := fracstep3 * (1 shl w.dc_texturefactorbits);
    frac3 := frac3 * (1 shl w.dc_texturefactorbits);
  end;
  lfactor3 := w.dc_lightlevel;
  if lfactor3 >= 0 then
  {$IFDEF HEXEN}
    if LevelUseFog then
      R_GetFogPrecalc32Tables(lfactor3, bf_r3, bf_g3, bf_b3)
    else
  {$ENDIF}
    R_GetPrecalc32Tables(lfactor3, bf_r3, bf_g3, bf_b3, w.dc_fog);  // JVAL: Mars fog sectors

  swidth := SCREENWIDTH32PITCH - 2 * SizeOf(LongWord);

  ypos := min_yl;
  if and_mask = 127 then
  begin
    {$UNDEF MASK_TC}
    {$DEFINE MASK_127}
    {$UNDEF MASK_255}
    {$UNDEF MASK_511}
    {$UNDEF MASK_1023}
    {$I R_Wall32_BatchFunc3.inc}
  end
  else if and_mask = 255 then
  begin
    {$UNDEF MASK_TC}
    {$UNDEF MASK_127}
    {$DEFINE MASK_255}
    {$UNDEF MASK_511}
    {$UNDEF MASK_1023}
    {$I R_Wall32_BatchFunc3.inc}
  end
  else if and_mask = 511 then
  begin
    {$UNDEF MASK_TC}
    {$UNDEF MASK_127}
    {$UNDEF MASK_255}
    {$DEFINE MASK_511}
    {$UNDEF MASK_1023}
    {$I R_Wall32_BatchFunc3.inc}
  end
  else if and_mask = 1023 then
  begin
    {$UNDEF MASK_TC}
    {$UNDEF MASK_127}
    {$UNDEF MASK_255}
    {$UNDEF MASK_511}
    {$DEFINE MASK_1023}
    {$I R_Wall32_BatchFunc3.inc}
  end
  else
  begin
    {$IFNDEF WALL32_TC}
    mod_height := and_mask + 1;
    {$ENDIF}
    {$DEFINE MASK_TC}
    {$UNDEF MASK_127}
    {$UNDEF MASK_255}
    {$UNDEF MASK_511}
    {$UNDEF MASK_1023}
    {$I R_Wall32_BatchFunc3.inc}
  end;
end;

{$IFDEF WALL32_128}
procedure R_DrawBatchColumnHi2_128(const walls: Pbatchwallrenderinfo32_t);
{$ENDIF}
{$IFDEF WALL32_256}
procedure R_DrawBatchColumnHi2_256(const walls: Pbatchwallrenderinfo32_t);
{$ENDIF}
{$IFDEF WALL32_512}
procedure R_DrawBatchColumnHi2_512(const walls: Pbatchwallrenderinfo32_t);
{$ENDIF}
{$IFDEF WALL32_TC}
procedure R_DrawBatchColumnHi2_TC(const walls: Pbatchwallrenderinfo32_t);
{$ENDIF}
var
  w: Pwallrenderinfo32_t;
  ypos: integer;
  destl: PLongWord;
  frac1, frac2: fixed_t;
  fracstep1, fracstep2: fixed_t;
  spot: integer;
  swidth: integer;

  r1, g1, b1: byte;
  c : LongWord;
  lfactor1, lfactor2: integer;
  mod_height: LongWord;
  and_mask: integer;
  bf_r1, bf_g1, bf_b1: PIntegerArray;
  bf_r2, bf_g2, bf_b2: PIntegerArray;
  dc_source32_1: PLongWordArray;
  dc_source32_2: PLongWordArray;

  xx: integer;

  min_yh, max_yl: integer;
  max_yh, min_yl: integer;
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

  w := @walls.walls[0];
  destl := @((ylookupl[min_yl]^)[columnofs[w.dc_x]]);

  xx := min_yl - centery;

  fracstep1 := w.dc_iscale;
  frac1 := w.dc_texturemid + xx * fracstep1;
  dc_source32_1 := w.dc_source32;
  if w.dc_texturefactorbits > 0 then
  begin
    fracstep1 := fracstep1 * (1 shl w.dc_texturefactorbits);
    frac1 := frac1 * (1 shl w.dc_texturefactorbits);
    {$IFDEF WALL32_TC}
    mod_height := walls.dc_height * (1 shl w.dc_texturefactorbits);
    {$ELSE}
    and_mask := {$IFDEF WALL32_128}128{$ENDIF}{$IFDEF WALL32_256}256{$ENDIF}{$IFDEF WALL32_512}512{$ENDIF} * (1 shl w.dc_texturefactorbits) - 1;
    {$ENDIF}
  end
  else
    {$IFDEF WALL32_TC}
    mod_height := walls.dc_height;
    and_mask := mod_height - 1;
    {$ELSE}
    and_mask := {$IFDEF WALL32_128}127{$ENDIF}{$IFDEF WALL32_256}255{$ENDIF}{$IFDEF WALL32_512}511{$ENDIF};
    {$ENDIF}
  lfactor1 := w.dc_lightlevel;
  if lfactor1 >= 0 then
  {$IFDEF HEXEN}
    if LevelUseFog then
      R_GetFogPrecalc32Tables(lfactor1, bf_r1, bf_g1, bf_b1)
    else
  {$ENDIF}
    R_GetPrecalc32Tables(lfactor1, bf_r1, bf_g1, bf_b1, w.dc_fog);  // JVAL: Mars fog sectors
  inc(w);

  fracstep2 := w.dc_iscale;
  frac2 := w.dc_texturemid + xx * fracstep2;
  dc_source32_2 := w.dc_source32;
  if w.dc_texturefactorbits > 0 then
  begin
    fracstep2 := fracstep2 * (1 shl w.dc_texturefactorbits);
    frac2 := frac2 * (1 shl w.dc_texturefactorbits);
  end;
  lfactor2 := w.dc_lightlevel;
  if lfactor2 >= 0 then
  {$IFDEF HEXEN}
    if LevelUseFog then
      R_GetFogPrecalc32Tables(lfactor2, bf_r2, bf_g2, bf_b2)
    else
  {$ENDIF}
    R_GetPrecalc32Tables(lfactor2, bf_r2, bf_g2, bf_b2, w.dc_fog);  // JVAL: Mars fog sectors);

  swidth := SCREENWIDTH32PITCH - SizeOf(LongWord);

  ypos := min_yl;
  if and_mask = 127 then
  begin
    {$UNDEF MASK_TC}
    {$DEFINE MASK_127}
    {$UNDEF MASK_255}
    {$UNDEF MASK_511}
    {$UNDEF MASK_1023}
    {$I R_Wall32_BatchFunc2.inc}
  end
  else if and_mask = 255 then
  begin
    {$UNDEF MASK_TC}
    {$UNDEF MASK_127}
    {$DEFINE MASK_255}
    {$UNDEF MASK_511}
    {$UNDEF MASK_1023}
    {$I R_Wall32_BatchFunc2.inc}
  end
  else if and_mask = 511 then
  begin
    {$UNDEF MASK_TC}
    {$UNDEF MASK_127}
    {$UNDEF MASK_255}
    {$DEFINE MASK_511}
    {$UNDEF MASK_1023}
    {$I R_Wall32_BatchFunc2.inc}
  end
  else if and_mask = 1023 then
  begin
    {$UNDEF MASK_TC}
    {$UNDEF MASK_127}
    {$UNDEF MASK_255}
    {$UNDEF MASK_511}
    {$DEFINE MASK_1023}
    {$I R_Wall32_BatchFunc2.inc}
  end
  else
  begin
    {$IFNDEF WALL32_TC}
    mod_height := and_mask + 1;
    {$ENDIF}
    {$DEFINE MASK_TC}
    {$UNDEF MASK_127}
    {$UNDEF MASK_255}
    {$UNDEF MASK_511}
    {$UNDEF MASK_1023}
    {$I R_Wall32_BatchFunc2.inc}
  end;
end;

{$IFDEF WALL32_128}
procedure R_DrawBatchColumnHi1_128(const walls: Pbatchwallrenderinfo32_t);
{$ENDIF}
{$IFDEF WALL32_256}
procedure R_DrawBatchColumnHi1_256(const walls: Pbatchwallrenderinfo32_t);
{$ENDIF}
{$IFDEF WALL32_512}
procedure R_DrawBatchColumnHi1_512(const walls: Pbatchwallrenderinfo32_t);
{$ENDIF}
{$IFDEF WALL32_TC}
procedure R_DrawBatchColumnHi1_TC(const walls: Pbatchwallrenderinfo32_t);
{$ENDIF}
var
  w: Pwallrenderinfo32_t;
  ypos: integer;
  destl: PLongWord;
  frac1: fixed_t;
  fracstep1: fixed_t;
  spot: integer;
  swidth: integer;

  r1, g1, b1: byte;
  c : LongWord;
  lfactor1: integer;
  mod_height: LongWord;
  and_mask: integer;
  bf_r1, bf_g1, bf_b1: PIntegerArray;
  dc_source32_1: PLongWordArray;

  xx: integer;
begin
  w := @walls.walls[0];
  if w.dc_yh >= SCREENHEIGHT then
    w.dc_yh := SCREENHEIGHT - 1
  else if w.dc_yh < 0 then
    w.dc_yh := 0;
  if w.dc_yl >= SCREENHEIGHT then
    w.dc_yl := SCREENHEIGHT - 1
  else if w.dc_yl < 0 then
    w.dc_yl := 0;

  destl := @((ylookupl[w.dc_yl]^)[columnofs[w.dc_x]]);

  xx := w.dc_yl - centery;

  fracstep1 := w.dc_iscale;
  frac1 := w.dc_texturemid + xx * fracstep1;
  dc_source32_1 := w.dc_source32;
  if w.dc_texturefactorbits > 0 then
  begin
    fracstep1 := fracstep1 * (1 shl w.dc_texturefactorbits);
    frac1 := frac1 * (1 shl w.dc_texturefactorbits);
    {$IFDEF WALL32_TC}
    mod_height := walls.dc_height * (1 shl w.dc_texturefactorbits);
    {$ELSE}
    and_mask := {$IFDEF WALL32_128}128{$ENDIF}{$IFDEF WALL32_256}256{$ENDIF}{$IFDEF WALL32_512}512{$ENDIF} * (1 shl w.dc_texturefactorbits) - 1;
    {$ENDIF}
  end
  else
    {$IFDEF WALL32_TC}
    mod_height := walls.dc_height;
    and_mask := mod_height - 1;
    {$ELSE}
    and_mask := {$IFDEF WALL32_128}127{$ENDIF}{$IFDEF WALL32_256}255{$ENDIF}{$IFDEF WALL32_512}511{$ENDIF};
    {$ENDIF}
  lfactor1 := w.dc_lightlevel;
  if lfactor1 >= 0 then
  {$IFDEF HEXEN}
    if LevelUseFog then
      R_GetFogPrecalc32Tables(lfactor1, bf_r1, bf_g1, bf_b1)
    else
  {$ENDIF}
    R_GetPrecalc32Tables(lfactor1, bf_r1, bf_g1, bf_b1, w.dc_fog);  // JVAL: Mars fog sectors);

  swidth := SCREENWIDTH32PITCH;

  ypos := w.dc_yl;
  if and_mask = 127 then
  begin
    {$UNDEF MASK_TC}
    {$DEFINE MASK_127}
    {$UNDEF MASK_255}
    {$UNDEF MASK_511}
    {$UNDEF MASK_1023}
    {$I R_Wall32_BatchFunc1.inc}
  end
  else if and_mask = 255 then
  begin
    {$UNDEF MASK_TC}
    {$UNDEF MASK_127}
    {$DEFINE MASK_255}
    {$UNDEF MASK_511}
    {$UNDEF MASK_1023}
    {$I R_Wall32_BatchFunc1.inc}
  end
  else if and_mask = 511 then
  begin
    {$UNDEF MASK_TC}
    {$UNDEF MASK_127}
    {$UNDEF MASK_255}
    {$DEFINE MASK_511}
    {$UNDEF MASK_1023}
    {$I R_Wall32_BatchFunc1.inc}
  end
  else if and_mask = 1023 then
  begin
    {$UNDEF MASK_TC}
    {$UNDEF MASK_127}
    {$UNDEF MASK_255}
    {$UNDEF MASK_511}
    {$DEFINE MASK_1023}
    {$I R_Wall32_BatchFunc1.inc}
  end
  else
  begin
    {$IFNDEF WALL32_TC}
    mod_height := and_mask + 1;
    {$ENDIF}
    {$DEFINE MASK_TC}
    {$UNDEF MASK_127}
    {$UNDEF MASK_255}
    {$UNDEF MASK_511}
    {$UNDEF MASK_1023}
    {$I R_Wall32_BatchFunc1.inc}
  end;
end;
