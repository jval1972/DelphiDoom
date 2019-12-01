    spot := (LongWord(frac) shr FRACBITS) and 127;
    c := curpal[dc_source[spot]];
{$IFDEF INVERSECOLORMAPS}
    r1 := c;
    g1 := c shr 8;
    b1 := c shr 16;
    destl^ := precal32_ic[r1 + g1 + b1];
{$ELSE}
    {$I R_ColorLight.inc}
{$ENDIF}

    destl := PLongWord(integer(destl) + swidth);
    inc(frac, fracstep);
