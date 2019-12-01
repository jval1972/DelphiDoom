  // Invert Sky Texture if below horizont level
    spot := LongWord(frac) shr FRACBITS;
    if spot > sky_max then
      spot := sky_sub - spot;

    destl^ := dc_source32[spot];

    destl := PLongWord(integer(destl) + swidth);
    inc(frac, fracstep);
