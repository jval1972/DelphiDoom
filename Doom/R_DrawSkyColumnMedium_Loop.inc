  // Invert Sky Texture if below horizont level
    spot := LongWord(frac) shr FRACBITS;
    if spot > 127 then
      spot := 127 - (spot and 127);

    dest^ := dc_source[spot];

    inc(dest, swidth);
    inc(frac, fracstep);
