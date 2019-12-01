      c3 := dc_colormap32[dc_source[(LongWord(frac) shr FRACBITS) and 127]];
      c4 := destl^;
      {$I R_ColorFuzzAverage.inc}

      destl := PLongWord(integer(destl) + swidth);
      inc(frac, fracstep);
