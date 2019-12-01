  // Invert Sky Texture if below horizont level
    spot := LongWord(frac) shr FRACBITS;
    if spot > 199 then
      spot := 400 - spot;

    dest^ := dc_source[spot];

    inc(dest, swidth);
    inc(frac, fracstep);
