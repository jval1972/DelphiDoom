    spot := (LongWord(frac) shr FRACBITS) and 127;
    c := curpal[dc_source[spot]];
{$IFDEF INVERSECOLORMAPS}
    {$I R_InverseLight.inc}
{$ELSE}
    {$I R_ColorLight.inc}
{$ENDIF}

    destl := PLongWord(integer(destl) + swidth);
    inc(frac, fracstep);
