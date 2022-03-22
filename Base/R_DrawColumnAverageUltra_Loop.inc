    //  Average with the next pixel
    spot := (LongWord(frac) shr FRACBITS) and 127;
    if lspot <> spot then
    begin
      lspot := spot;
      c1 := dc_colormap32[dc_source[spot]];
      spot2 := (LongWord(spot) + 1) and 127;
      c2 := dc_colormap32[dc_source[spot2]];
    end;

    cfrac2 := LongWord(frac) and (FRACUNIT - 1);
    {$I R_ColorAverage.inc}
    c3 := r + g shl 8 + b;
    c4 := destl^;
    {$I R_ColorMidAverage.inc}

    destl := PLongWord(integer(destl) + swidth);
    inc(frac, fracstep);
