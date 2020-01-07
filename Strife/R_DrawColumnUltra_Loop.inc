      //  Average with the next column of 32 bit dc_source
      spot := (LongWord(frac) shr FRACBITS) and and_mask;
      cfrac2 := LongWord(frac) and (FRACUNIT - 1);
      c1 := dc_source32[spot];
{$IFDEF MASKEDCOLUMN}
      if c1 <> 0 then
      begin
{$ENDIF}
        r1 := c1;
        g1 := c1 shr 8;
        b1 := c1 shr 16;
        c2 := dc_source32[spot + 1];
        r2 := c2;
        g2 := c2 shr 8;
        b2 := c2 shr 16;

{$IFDEF INVERSECOLORMAPS}
        factor1 := FRACUNIT - 1 - cfrac2;

        r := ((r2 * cfrac2) + (r1 * factor1));
        g := ((g2 * cfrac2) + (g1 * factor1));
        b := ((b2 * cfrac2) + (b1 * factor1));
        r := 255 - (r + g + b) div (FRACUNIT * 3);
        destl^ := r + r shl 8 + r shl 16;

{$ELSE}
        factor1 := ((FRACUNIT - 1 - cfrac2) * lfactor) shr FRACBITS;
        factor2 := (cfrac2 * lfactor) shr FRACBITS;

        r := ((r2 * factor2) + (r1 * factor1)) shr FRACBITS;
        g := ((g2 * factor2) + (g1 * factor1)) shr FRACBITS;
        b := ((b2 * factor2) + (b1 * factor1)) shr FRACBITS;
        destl^ := r + g shl 8 + b shl 16;

{$ENDIF}

{$IFDEF MASKEDCOLUMN}
      end;
{$ENDIF}

      destl := PLongWord(integer(destl) + swidth);
      inc(frac, fracstep);
