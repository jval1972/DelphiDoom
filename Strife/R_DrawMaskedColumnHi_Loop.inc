    fmod := (LongWord(frac) and (FRACUNIT - 1)) * DC_HIRESFACTOR div FRACUNIT;
    spot := (LongWord(frac) shr FRACBITS) and 127;
    spot2 := (LongWord(spot) + 1) and 127;
    c := hirestable[fmod, dc_source[spot], dc_source[spot2]];
{$IFDEF INVERSECOLORMAPS}
        r1 := c;
        g1 := c shr 8;
        b1 := c shr 16;
        destl^ := precal32_ic[r1 + g1 + b1];
{$ELSE}
    destl^ := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
{$ENDIF}

    destl := PLongWord(integer(destl) + swidth);
    inc(frac, fracstep);
