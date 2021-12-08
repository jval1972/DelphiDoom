  if lfactor1 >= 0 then
  begin
    while ypos <= w.dc_yh do
    begin
      spot := (LongWord(frac1) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
      c := dc_source32_1[spot];
      destl^ := bf_r1[c and $FF] + bf_g1[(c shr 8) and $FF] + bf_b1[(c shr 16) and $FF];
      inc(frac1, fracstep1);
      destl := PLongWord(integer(destl) + swidth);
      inc(ypos);
    end;
  end
  else
  begin
    while ypos <= w.dc_yh do
    begin
      spot := (LongWord(frac1) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
      c := dc_source32_1[spot];
      r1 := c;
      g1 := c shr 8;
      b1 := c shr 16;
      destl^ := precal32_ic[r1 + g1 + b1];
      inc(frac1, fracstep1);
      destl := PLongWord(integer(destl) + swidth);
      inc(ypos);
    end;
  end;
