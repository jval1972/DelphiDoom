    //  Average with the next column and next pixel
    spot := (LongWord(frac) shr FRACBITS) and and_mask;
    if lspot = spot then
    begin
      fraclimit2 := (LongWord(frac) and not (FRACUNIT - 1)) + FRACUNIT;
      if fraclimit2 > fraclimit then
        fraclimit2 := fraclimit;
      while frac <= fraclimit2 do
      begin
        destl^ := ldest;
        destl := PLongWord(integer(destl) + swidth);
        inc(frac, fracstep);
      end;
    end
    else
    begin
      lspot := spot;

      c := dc_source32[spot];
{$IFDEF MASKEDCOLUMN}
      if c <> 0 then
      begin
{$ENDIF}
{$IFDEF INVERSECOLORMAPS}
        r1 := c;
        g1 := c shr 8;
        b1 := c shr 16;
        destl^ := precal32_ic[r1 + g1 + b1];
{$ELSE}
      destl^ := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
{$ENDIF}
{$IFDEF MASKEDCOLUMN}
      end;
{$ENDIF}

      ldest := destl^;

      destl := PLongWord(integer(destl) + swidth);
      inc(frac, fracstep);

    end;

