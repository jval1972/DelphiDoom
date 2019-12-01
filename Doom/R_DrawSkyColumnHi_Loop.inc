  // Invert Sky Texture if below horizont level
    spot := LongWord(frac) shr FRACBITS;
    if spot > and_mask then
      spot := and_mask - (spot and and_mask);

    destl^ := dc_source32[spot];

    destl := PLongWord(integer(destl) + swidth);
    inc(frac, fracstep);
