    //  Average with the next column and next pixel
    spot := (LongWord(frac) shr FRACBITS) and 127;
    c := dc_source32[spot];
{$IFDEF MASKEDCOLUMN}
      if c <> 0 then
      begin
{$ENDIF}
{$IFDEF INVERSECOLORMAPS}
        r1 := c;
        g1 := c shr 8;
        b1 := c shr 16;
        ldest := precal32_ic[r1 + g1 + b1];
{$ELSE}
        ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
{$ENDIF}
        cnt := num_batch_columns;
        while cnt > 0 do
        begin
          destl^ := ldest;
          Inc(destl);
          Dec(cnt);
        end;
{$IFDEF MASKEDCOLUMN}
      end;
{$ENDIF}

      destl := PLongWord(integer(destl) + swidth);
      inc(frac, fracstep);

