    spot := (LongWord(frac) shr FRACBITS) and 127;
    c := curpal[dc_source[spot]];
{$IFDEF INVERSECOLORMAPS}
    {$I R_InverseLight.inc}
{$ELSE}
    destl^ := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
{$ENDIF}

    destl := PLongWord(integer(destl) + swidth);
    inc(frac, fracstep);
