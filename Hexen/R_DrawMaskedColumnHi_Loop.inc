    fmod := (LongWord(frac) and (FRACUNIT - 1)) * DC_HIRESFACTOR div FRACUNIT;
    spot := (LongWord(frac) shr FRACBITS) and 127;
    spot2 := (LongWord(spot) + 1) and 127;
    c := hirestable[fmod, dc_source[spot], dc_source[spot2]];
{$IFDEF INVERSECOLORMAPS}
    {$I R_InverseLight.inc}
{$ELSE}
    {$I R_ColorLight.inc}
{$ENDIF}

    destl := PLongWord(integer(destl) + swidth);
    inc(frac, fracstep);
