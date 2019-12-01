  // Invert Sky Texture if below horizont level
    spot := (LongWord(frac) shr FRACBITS) and 127;

    dest^ := dc_source[spot];

    inc(dest, swidth);
    inc(frac, fracstep);
