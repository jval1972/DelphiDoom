var
  count: integer;
  i: integer;
  destl: PLongWord;
  frac: fixed_t;
  fracstep: fixed_t;
  spot: integer;
  swidth: integer;

  c: LongWord;
  lfactor: fixed_t;
  r1, g1, b1: byte;
  r, g, b: LongWord;
begin
  count := dc_yh - dc_yl;

  if count < 0 then
    exit;

  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);

  swidth := SCREENWIDTH32PITCH;

  fracstep := dc_iscale;
  frac := dc_texturemid + (dc_yl - centery) * fracstep;

  for i := 0 to count do
  begin
    //  Average with the next column and next pixel
    spot := (LongWord(frac) shr FRACBITS) and 255;
    lfactor := dc_source32[spot];
    c := destl^;
    {$I R_ColorBoost.inc}
    destl^ := r + g shl 8 + b shl 16;
    destl := PLongWord(integer(destl) + swidth);
    inc(frac, fracstep);
  end;
end;

