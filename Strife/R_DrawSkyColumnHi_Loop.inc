  // Invert Sky Texture if below horizont level
    spot := (LongWord(frac) shr FRACBITS) and and_mask;

    destl^ := dc_source32[spot];

    destl := PLongWord(integer(destl) + swidth);
    inc(frac, fracstep);
