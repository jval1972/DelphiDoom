  // Invert Sky Texture if below horizont level
    cfrac2 := frac and (FRACUNIT - 1);
    spot := LongWord(frac) shr FRACBITS;
    if spot > sky_max then
    begin
      spot := sky_sub - spot;
      c1 := dc_source32[spot];
      c2 := dc_source32[spot - 1];
      {$I ..\Base\R_ColorAverage.inc}
      destl^ := r + g shl 8 + b;
    end
    else
    begin
      c1 := dc_source32[spot];
      c2 := dc_source32[spot + 1];
      {$I ..\Base\R_ColorAverage.inc}
      destl^ := r + g shl 8 + b;
    end;

    destl := PLongWord(integer(destl) + swidth);
    inc(frac, fracstep);
