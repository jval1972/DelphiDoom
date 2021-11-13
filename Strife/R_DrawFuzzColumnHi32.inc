var
  count: integer;
  destl: PLongWord;
  frac: fixed_t;
  fracstep: fixed_t;
  fraclimit: fixed_t;
  spot: integer;
  swidth: integer;

  r1, g1, b1: byte;
  r2, g2, b2: byte;
  c, c1, r, g, b: LongWord;
  lfactor: integer;
  and_mask: integer;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
begin
  count := dc_yh - dc_yl;

  if count < 0 then
    exit;

  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);

  fracstep := dc_iscale;
  frac := dc_texturemid + (dc_yl - centery) * fracstep;

  if dc_texturefactorbits > 0 then
  begin
    fracstep := fracstep * (1 shl dc_texturefactorbits);
    frac := frac * (1 shl dc_texturefactorbits);
    and_mask := 128 * (1 shl dc_texturefactorbits) - 1;
  end
  else
    and_mask := 127;

  swidth := SCREENWIDTH32PITCH;
  lfactor := dc_lightlevel;

  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
    while count > 0 do
    begin
      spot := (LongWord(frac) shr FRACBITS) and and_mask;
      c1 := destl^;

      // Color averaging
      r1 := c1;
      g1 := c1 shr 8;
      b1 := c1 shr 16;

      c := dc_source32[spot];
      r2 := bf_r[c and $FF];
      g2 := bf_r[(c shr 8) and $FF];
      b2 := bf_r[(c shr 16) and $FF];

{$IFDEF ONE}
      r := ((r2 * (FRACUNIT div 4 - 1)) + (r1 * (3 * (FRACUNIT div 4)))) shr FRACBITS;
      g := ((g2 * (FRACUNIT div 4 - 1)) + (g1 * (3 * (FRACUNIT div 4)))) shr FRACBITS;
      b := ((b2 * (FRACUNIT div 4 - 1)) + (b1 * (3 * (FRACUNIT div 4)))) and $FF0000;
{$ELSE}
      r := ((r2 * (3 * (FRACUNIT div 4))) + (r1 * (FRACUNIT div 4 - 1))) shr FRACBITS;
      g := ((g2 * (3 * (FRACUNIT div 4))) + (g1 * (FRACUNIT div 4 - 1))) shr FRACBITS;
      b := ((b2 * (3 * (FRACUNIT div 4))) + (b1 * (FRACUNIT div 4 - 1))) and $FF0000;
{$ENDIF}

      destl^ := r + g shl 8 + b;

      destl := PLongWord(integer(destl) + swidth);
      inc(frac, fracstep);

      dec(count);
    end;

  end
  else
  begin
    {$DEFINE INVERSECOLORMAPS}
    {$UNDEF MASKEDCOLUMN}
    {$UNDEF SMALLSTEPOPTIMIZER}
    {$I R_DrawColumnHi.inc}
  end;
end;

