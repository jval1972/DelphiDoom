    cfrac2 := frac and (FRACUNIT - 1);
    factor1 := FRACUNIT - 1 - cfrac2;
    spot := (LongWord(frac) shr FRACBITS) and and_mask;
    c1 := dc_source32[spot];
    c2 := dc_source32[spot + 1];
    {$I ..\Base\R_ColorAverage.inc}
    destl^ := r + g shl 8 + b;

    destl := PLongWord(integer(destl) + swidth);
    inc(frac, fracstep);
