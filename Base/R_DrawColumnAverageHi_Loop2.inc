      spot := (LongWord(frac) shr FRACBITS) and 127;
      if spot <> lspot then
      begin
        lspot := spot;
        c3 := dc_colormap32[dc_source[spot]];
      end;
      c4 := destl^;
      {$I R_ColorMidAverage.inc}

      destl := PLongWord(integer(destl) + swidth);
      inc(frac, fracstep);
