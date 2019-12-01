var
  count: integer;
  i: integer;
  destl: PLongWord;
  frac: fixed_t;
  fracstep: fixed_t;
  spot: integer;
  swidth: integer;
  cnt: integer;

  c: LongWord;
  lfactor: fixed_t;
  r1, g1, b1: byte;
  r, g, b: LongWord;

  lc: LongWord;
  lnc: LongWord;
begin
  count := dc_yh - dc_yl;

  if count < 0 then
    exit;

  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);

  swidth := SCREENWIDTH32PITCH - num_batch_columns * SizeOf(LongWord);

  fracstep := dc_iscale;
  frac := dc_texturemid + (dc_yl - centery) * fracstep;

  for i := 0 to count do
  begin
    //  Average with the next column and next pixel
    spot := (LongWord(frac) shr FRACBITS) and 255;
    lfactor := dc_source32[spot];

    lnc := $FFFFFFFF;
    lc := $FFFFFFFF;
    cnt := num_batch_columns;
    while cnt > 0 do
    begin
      c := destl^;
      if c <> lc then
      begin
        r1 := c;
        g1 := c shr 8;
        b1 := c shr 16;
    {$IFDEF WHITE}
        r := (r1 * lfactor) shr FRACBITS;
        if r > 255 then
          r := 255;
        g := (g1 * lfactor) shr FRACBITS;
        if g > 255 then
          g := 255;
        b := (b1 * lfactor) shr FRACBITS;
        if b > 255 then
          b := 255;
    {$ENDIF}
    {$IFDEF RED}
        r := r1;
        g := g1;
        b := (b1 * lfactor) shr FRACBITS;
        if b > 255 then
          b := 255;
    {$ENDIF}
    {$IFDEF GREEN}
        r := r1;
        g := (g1 * lfactor) shr FRACBITS;
        if g > 255 then
          g := 255;
        b := b1;
    {$ENDIF}
    {$IFDEF BLUE}
        r := (r1 * lfactor) shr FRACBITS;
        if r > 255 then
          r := 255;
        g := g1;
        b := b1;
    {$ENDIF}
    {$IFDEF YELLOW}
        r := r1;
        g := (g1 * lfactor) shr FRACBITS;
        if g > 255 then
          g := 255;
        b := (b1 * lfactor) shr FRACBITS;
        if b > 255 then
          b := 255;
    {$ENDIF}
        lnc := r + g shl 8 + b shl 16;
        destl^ := lnc;
        lc := c;
      end
      else
        destl^ := lnc;
      Inc(destl);
      Dec(cnt);
    end;

    destl := PLongWord(integer(destl) + swidth);
    inc(frac, fracstep);
  end;
end;
