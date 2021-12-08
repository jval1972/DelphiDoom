  if lfactor1 >= 0 then
  begin
    if max_yl < min_yh then
    begin
      while ypos < max_yl do
      begin
        w := @walls.walls[0];
        if (ypos >= w.dc_yl) then
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
        end;
        inc(frac1, fracstep1);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) then
        begin
          spot := (LongWord(frac2) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
          c := dc_source32_2[spot];
          destl^ := bf_r2[c and $FF] + bf_g2[(c shr 8) and $FF] + bf_b2[(c shr 16) and $FF];
        end;
        inc(frac2, fracstep2);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) then
        begin
          spot := (LongWord(frac3) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
          c := dc_source32_3[spot];
          destl^ := bf_r3[c and $FF] + bf_g3[(c shr 8) and $FF] + bf_b3[(c shr 16) and $FF];
        end;
        inc(frac3, fracstep3);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) then
        begin
          spot := (LongWord(frac4) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
          c := dc_source32_4[spot];
          destl^ := bf_r4[c and $FF] + bf_g4[(c shr 8) and $FF] + bf_b4[(c shr 16) and $FF];
        end;
        inc(frac4, fracstep4);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) then
        begin
          spot := (LongWord(frac5) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
          c := dc_source32_5[spot];
          destl^ := bf_r5[c and $FF] + bf_g5[(c shr 8) and $FF] + bf_b5[(c shr 16) and $FF];
        end;
        inc(frac5, fracstep5);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) then
        begin
          spot := (LongWord(frac6) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
          c := dc_source32_6[spot];
          destl^ := bf_r6[c and $FF] + bf_g6[(c shr 8) and $FF] + bf_b6[(c shr 16) and $FF];
        end;
        inc(frac6, fracstep6);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) then
        begin
          spot := (LongWord(frac7) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
          c := dc_source32_7[spot];
          destl^ := bf_r7[c and $FF] + bf_g7[(c shr 8) and $FF] + bf_b7[(c shr 16) and $FF];
        end;
        inc(frac7, fracstep7);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) then
        begin
          spot := (LongWord(frac8) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
          c := dc_source32_8[spot];
          destl^ := bf_r8[c and $FF] + bf_g8[(c shr 8) and $FF] + bf_b8[(c shr 16) and $FF];
        end;
        inc(frac8, fracstep8);

        destl := PLongWord(integer(destl) + swidth);
        inc(ypos);
      end;

      while ypos <= min_yh do
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
        inc(destl);

        spot := (LongWord(frac2) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
        c := dc_source32_2[spot];
        destl^ := bf_r2[c and $FF] + bf_g2[(c shr 8) and $FF] + bf_b2[(c shr 16) and $FF];
        inc(frac2, fracstep2);
        inc(destl);

        spot := (LongWord(frac3) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
        c := dc_source32_3[spot];
        destl^ := bf_r3[c and $FF] + bf_g3[(c shr 8) and $FF] + bf_b3[(c shr 16) and $FF];
        inc(frac3, fracstep3);
        inc(destl);

        spot := (LongWord(frac4) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
        c := dc_source32_4[spot];
        destl^ := bf_r4[c and $FF] + bf_g4[(c shr 8) and $FF] + bf_b4[(c shr 16) and $FF];
        inc(frac4, fracstep4);
        inc(destl);

        spot := (LongWord(frac5) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
        c := dc_source32_5[spot];
        destl^ := bf_r5[c and $FF] + bf_g5[(c shr 8) and $FF] + bf_b5[(c shr 16) and $FF];
        inc(frac5, fracstep5);
        inc(destl);

        spot := (LongWord(frac6) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
        c := dc_source32_6[spot];
        destl^ := bf_r6[c and $FF] + bf_g6[(c shr 8) and $FF] + bf_b6[(c shr 16) and $FF];
        inc(frac6, fracstep6);
        inc(destl);

        spot := (LongWord(frac7) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
        c := dc_source32_7[spot];
        destl^ := bf_r7[c and $FF] + bf_g7[(c shr 8) and $FF] + bf_b7[(c shr 16) and $FF];
        inc(frac7, fracstep7);
        inc(destl);

        spot := (LongWord(frac8) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
        c := dc_source32_8[spot];
        destl^ := bf_r8[c and $FF] + bf_g8[(c shr 8) and $FF] + bf_b8[(c shr 16) and $FF];
        inc(frac8, fracstep8);

        destl := PLongWord(integer(destl) + swidth);
        inc(ypos);
      end;

      while ypos <= max_yh do
      begin
        w := @walls.walls[0];
        if (ypos <= w.dc_yh) then
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
        end;
        inc(frac1, fracstep1);
        inc(destl);
        inc(w);

        if (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac2) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
          c := dc_source32_2[spot];
          destl^ := bf_r2[c and $FF] + bf_g2[(c shr 8) and $FF] + bf_b2[(c shr 16) and $FF];
        end;
        inc(frac2, fracstep2);
        inc(destl);
        inc(w);

        if (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac3) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
          c := dc_source32_3[spot];
          destl^ := bf_r3[c and $FF] + bf_g3[(c shr 8) and $FF] + bf_b3[(c shr 16) and $FF];
        end;
        inc(frac3, fracstep3);
        inc(destl);
        inc(w);

        if (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac4) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
          c := dc_source32_4[spot];
          destl^ := bf_r4[c and $FF] + bf_g4[(c shr 8) and $FF] + bf_b4[(c shr 16) and $FF];
        end;
        inc(frac4, fracstep4);
        inc(destl);
        inc(w);

        if (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac5) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
          c := dc_source32_5[spot];
          destl^ := bf_r5[c and $FF] + bf_g5[(c shr 8) and $FF] + bf_b5[(c shr 16) and $FF];
        end;
        inc(frac5, fracstep5);
        inc(destl);
        inc(w);

        if (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac6) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
          c := dc_source32_6[spot];
          destl^ := bf_r6[c and $FF] + bf_g6[(c shr 8) and $FF] + bf_b6[(c shr 16) and $FF];
        end;
        inc(frac6, fracstep6);
        inc(destl);
        inc(w);

        if (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac7) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
          c := dc_source32_7[spot];
          destl^ := bf_r7[c and $FF] + bf_g7[(c shr 8) and $FF] + bf_b7[(c shr 16) and $FF];
        end;
        inc(frac7, fracstep7);
        inc(destl);
        inc(w);

        if (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac8) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
          c := dc_source32_8[spot];
          destl^ := bf_r8[c and $FF] + bf_g8[(c shr 8) and $FF] + bf_b8[(c shr 16) and $FF];
        end;
        inc(frac8, fracstep8);

        destl := PLongWord(integer(destl) + swidth);
        inc(ypos);
      end;
    end
    else
    begin
      while ypos < max_yl do
      begin
        w := @walls.walls[0];
        if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
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
        end;
        inc(frac1, fracstep1);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac2) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
          c := dc_source32_2[spot];
          destl^ := bf_r2[c and $FF] + bf_g2[(c shr 8) and $FF] + bf_b2[(c shr 16) and $FF];
        end;
        inc(frac2, fracstep2);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac3) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
          c := dc_source32_3[spot];
          destl^ := bf_r3[c and $FF] + bf_g3[(c shr 8) and $FF] + bf_b3[(c shr 16) and $FF];
        end;
        inc(frac3, fracstep3);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac4) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
          c := dc_source32_4[spot];
          destl^ := bf_r4[c and $FF] + bf_g4[(c shr 8) and $FF] + bf_b4[(c shr 16) and $FF];
        end;
        inc(frac4, fracstep4);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac5) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
          c := dc_source32_5[spot];
          destl^ := bf_r5[c and $FF] + bf_g5[(c shr 8) and $FF] + bf_b5[(c shr 16) and $FF];
        end;
        inc(frac5, fracstep5);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac6) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
          c := dc_source32_6[spot];
          destl^ := bf_r6[c and $FF] + bf_g6[(c shr 8) and $FF] + bf_b6[(c shr 16) and $FF];
        end;
        inc(frac6, fracstep6);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac7) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
          c := dc_source32_7[spot];
          destl^ := bf_r7[c and $FF] + bf_g7[(c shr 8) and $FF] + bf_b7[(c shr 16) and $FF];
        end;
        inc(frac7, fracstep7);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac8) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
          c := dc_source32_8[spot];
          destl^ := bf_r8[c and $FF] + bf_g8[(c shr 8) and $FF] + bf_b8[(c shr 16) and $FF];
        end;
        inc(frac8, fracstep8);

        destl := PLongWord(integer(destl) + swidth);
        inc(ypos);
      end;

      while ypos <= max_yh do
      begin
        w := @walls.walls[0];
        if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
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
        end;
        inc(frac1, fracstep1);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac2) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
          c := dc_source32_2[spot];
          destl^ := bf_r2[c and $FF] + bf_g2[(c shr 8) and $FF] + bf_b2[(c shr 16) and $FF];
        end;
        inc(frac2, fracstep2);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac3) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
          c := dc_source32_3[spot];
          destl^ := bf_r3[c and $FF] + bf_g3[(c shr 8) and $FF] + bf_b3[(c shr 16) and $FF];
        end;
        inc(frac3, fracstep3);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac4) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
          c := dc_source32_4[spot];
          destl^ := bf_r4[c and $FF] + bf_g4[(c shr 8) and $FF] + bf_b4[(c shr 16) and $FF];
        end;
        inc(frac4, fracstep4);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac5) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
          c := dc_source32_5[spot];
          destl^ := bf_r5[c and $FF] + bf_g5[(c shr 8) and $FF] + bf_b5[(c shr 16) and $FF];
        end;
        inc(frac5, fracstep5);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac6) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
          c := dc_source32_6[spot];
          destl^ := bf_r6[c and $FF] + bf_g6[(c shr 8) and $FF] + bf_b6[(c shr 16) and $FF];
        end;
        inc(frac6, fracstep6);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac7) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
          c := dc_source32_7[spot];
          destl^ := bf_r7[c and $FF] + bf_g7[(c shr 8) and $FF] + bf_b7[(c shr 16) and $FF];
        end;
        inc(frac7, fracstep7);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac8) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
          c := dc_source32_8[spot];
          destl^ := bf_r8[c and $FF] + bf_g8[(c shr 8) and $FF] + bf_b8[(c shr 16) and $FF];
        end;
        inc(frac8, fracstep8);

        destl := PLongWord(integer(destl) + swidth);
        inc(ypos);
      end;
    end
  end
  else
  begin
    if max_yl < min_yh then
    begin
      while ypos < max_yl do
      begin
        w := @walls.walls[0];
        if (ypos >= w.dc_yl) then
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
        end;
        inc(frac1, fracstep1);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) then
        begin
          spot := (LongWord(frac2) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
          c := dc_source32_2[spot];
          r1 := c;
          g1 := c shr 8;
          b1 := c shr 16;
          destl^ := precal32_ic[r1 + g1 + b1];
        end;
        inc(frac2, fracstep2);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) then
        begin
          spot := (LongWord(frac3) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
          c := dc_source32_3[spot];
          r1 := c;
          g1 := c shr 8;
          b1 := c shr 16;
          destl^ := precal32_ic[r1 + g1 + b1];
        end;
        inc(frac3, fracstep3);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) then
        begin
          spot := (LongWord(frac4) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
          c := dc_source32_4[spot];
          r1 := c;
          g1 := c shr 8;
          b1 := c shr 16;
          destl^ := precal32_ic[r1 + g1 + b1];
        end;
        inc(frac4, fracstep4);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) then
        begin
          spot := (LongWord(frac5) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
          c := dc_source32_5[spot];
          r1 := c;
          g1 := c shr 8;
          b1 := c shr 16;
          destl^ := precal32_ic[r1 + g1 + b1];
        end;
        inc(frac5, fracstep5);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) then
        begin
          spot := (LongWord(frac6) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
          c := dc_source32_6[spot];
          r1 := c;
          g1 := c shr 8;
          b1 := c shr 16;
          destl^ := precal32_ic[r1 + g1 + b1];
        end;
        inc(frac6, fracstep6);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) then
        begin
          spot := (LongWord(frac7) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
          c := dc_source32_7[spot];
          r1 := c;
          g1 := c shr 8;
          b1 := c shr 16;
          destl^ := precal32_ic[r1 + g1 + b1];
        end;
        inc(frac7, fracstep7);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) then
        begin
          spot := (LongWord(frac8) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
          c := dc_source32_8[spot];
          r1 := c;
          g1 := c shr 8;
          b1 := c shr 16;
          destl^ := precal32_ic[r1 + g1 + b1];
        end;
        inc(frac8, fracstep8);

        destl := PLongWord(integer(destl) + swidth);
        inc(ypos);
      end;

      while ypos <= min_yh do
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
        inc(destl);

        spot := (LongWord(frac2) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
        c := dc_source32_2[spot];
        r1 := c;
        g1 := c shr 8;
        b1 := c shr 16;
        destl^ := precal32_ic[r1 + g1 + b1];
        inc(frac2, fracstep2);
        inc(destl);

        spot := (LongWord(frac3) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
        c := dc_source32_3[spot];
        r1 := c;
        g1 := c shr 8;
        b1 := c shr 16;
        destl^ := precal32_ic[r1 + g1 + b1];
        inc(frac3, fracstep3);
        inc(destl);

        spot := (LongWord(frac4) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
        c := dc_source32_4[spot];
        r1 := c;
        g1 := c shr 8;
        b1 := c shr 16;
        destl^ := precal32_ic[r1 + g1 + b1];
        inc(frac4, fracstep4);
        inc(destl);

        spot := (LongWord(frac5) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
        c := dc_source32_5[spot];
        r1 := c;
        g1 := c shr 8;
        b1 := c shr 16;
        destl^ := precal32_ic[r1 + g1 + b1];
        inc(frac5, fracstep5);
        inc(destl);

        spot := (LongWord(frac6) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
        c := dc_source32_6[spot];
        r1 := c;
        g1 := c shr 8;
        b1 := c shr 16;
        destl^ := precal32_ic[r1 + g1 + b1];
        inc(frac6, fracstep6);
        inc(destl);

        spot := (LongWord(frac7) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
        c := dc_source32_7[spot];
        r1 := c;
        g1 := c shr 8;
        b1 := c shr 16;
        destl^ := precal32_ic[r1 + g1 + b1];
        inc(frac7, fracstep7);
        inc(destl);

        spot := (LongWord(frac8) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
        c := dc_source32_8[spot];
        r1 := c;
        g1 := c shr 8;
        b1 := c shr 16;
        destl^ := precal32_ic[r1 + g1 + b1];
        inc(frac8, fracstep8);

        destl := PLongWord(integer(destl) + swidth);
        inc(ypos);
      end;

      while ypos <= max_yh do
      begin
        w := @walls.walls[0];
        if (ypos <= w.dc_yh) then
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
        end;
        inc(frac1, fracstep1);
        inc(destl);
        inc(w);

        if (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac2) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
          c := dc_source32_2[spot];
          r1 := c;
          g1 := c shr 8;
          b1 := c shr 16;
          destl^ := precal32_ic[r1 + g1 + b1];
        end;
        inc(frac2, fracstep2);
        inc(destl);
        inc(w);

        if (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac3) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
          c := dc_source32_3[spot];
          r1 := c;
          g1 := c shr 8;
          b1 := c shr 16;
          destl^ := precal32_ic[r1 + g1 + b1];
        end;
        inc(frac3, fracstep3);
        inc(destl);
        inc(w);

        if (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac4) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
          c := dc_source32_4[spot];
          r1 := c;
          g1 := c shr 8;
          b1 := c shr 16;
          destl^ := precal32_ic[r1 + g1 + b1];
        end;
        inc(frac4, fracstep4);
        inc(destl);
        inc(w);

        if (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac5) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
          c := dc_source32_5[spot];
          r1 := c;
          g1 := c shr 8;
          b1 := c shr 16;
          destl^ := precal32_ic[r1 + g1 + b1];
        end;
        inc(frac5, fracstep5);
        inc(destl);
        inc(w);

        if (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac6) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
          c := dc_source32_6[spot];
          r1 := c;
          g1 := c shr 8;
          b1 := c shr 16;
          destl^ := precal32_ic[r1 + g1 + b1];
        end;
        inc(frac6, fracstep6);
        inc(destl);
        inc(w);

        if (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac7) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
          c := dc_source32_7[spot];
          r1 := c;
          g1 := c shr 8;
          b1 := c shr 16;
          destl^ := precal32_ic[r1 + g1 + b1];
        end;
        inc(frac7, fracstep7);
        inc(destl);
        inc(w);

        if (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac8) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
          c := dc_source32_8[spot];
          r1 := c;
          g1 := c shr 8;
          b1 := c shr 16;
          destl^ := precal32_ic[r1 + g1 + b1];
        end;
        inc(frac8, fracstep8);

        destl := PLongWord(integer(destl) + swidth);
        inc(ypos);
      end
    end
    else
    begin
      while ypos < max_yl do
      begin
        w := @walls.walls[0];
        if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
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
        end;
        inc(frac1, fracstep1);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac2) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
          c := dc_source32_2[spot];
          r1 := c;
          g1 := c shr 8;
          b1 := c shr 16;
          destl^ := precal32_ic[r1 + g1 + b1];
        end;
        inc(frac2, fracstep2);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac3) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
          c := dc_source32_3[spot];
          r1 := c;
          g1 := c shr 8;
          b1 := c shr 16;
          destl^ := precal32_ic[r1 + g1 + b1];
        end;
        inc(frac3, fracstep3);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac4) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
          c := dc_source32_4[spot];
          r1 := c;
          g1 := c shr 8;
          b1 := c shr 16;
          destl^ := precal32_ic[r1 + g1 + b1];
        end;
        inc(frac4, fracstep4);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac5) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
          c := dc_source32_5[spot];
          r1 := c;
          g1 := c shr 8;
          b1 := c shr 16;
          destl^ := precal32_ic[r1 + g1 + b1];
        end;
        inc(frac5, fracstep5);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac6) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
          c := dc_source32_6[spot];
          r1 := c;
          g1 := c shr 8;
          b1 := c shr 16;
          destl^ := precal32_ic[r1 + g1 + b1];
        end;
        inc(frac6, fracstep6);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac7) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
          c := dc_source32_7[spot];
          r1 := c;
          g1 := c shr 8;
          b1 := c shr 16;
          destl^ := precal32_ic[r1 + g1 + b1];
        end;
        inc(frac7, fracstep7);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac8) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
          c := dc_source32_8[spot];
          r1 := c;
          g1 := c shr 8;
          b1 := c shr 16;
          destl^ := precal32_ic[r1 + g1 + b1];
        end;
        inc(frac8, fracstep8);

        destl := PLongWord(integer(destl) + swidth);
        inc(ypos);
      end;

      while ypos <= max_yh do
      begin
        w := @walls.walls[0];
        if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
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
        end;
        inc(frac1, fracstep1);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac2) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
          c := dc_source32_2[spot];
          r1 := c;
          g1 := c shr 8;
          b1 := c shr 16;
          destl^ := precal32_ic[r1 + g1 + b1];
        end;
        inc(frac2, fracstep2);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac3) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
          c := dc_source32_3[spot];
          r1 := c;
          g1 := c shr 8;
          b1 := c shr 16;
          destl^ := precal32_ic[r1 + g1 + b1];
        end;
        inc(frac3, fracstep3);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac4) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
          c := dc_source32_4[spot];
          r1 := c;
          g1 := c shr 8;
          b1 := c shr 16;
          destl^ := precal32_ic[r1 + g1 + b1];
        end;
        inc(frac4, fracstep4);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac5) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
          c := dc_source32_5[spot];
          r1 := c;
          g1 := c shr 8;
          b1 := c shr 16;
          destl^ := precal32_ic[r1 + g1 + b1];
        end;
        inc(frac5, fracstep5);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac6) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
          c := dc_source32_6[spot];
          r1 := c;
          g1 := c shr 8;
          b1 := c shr 16;
          destl^ := precal32_ic[r1 + g1 + b1];
        end;
        inc(frac6, fracstep6);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac7) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
          c := dc_source32_7[spot];
          r1 := c;
          g1 := c shr 8;
          b1 := c shr 16;
          destl^ := precal32_ic[r1 + g1 + b1];
        end;
        inc(frac7, fracstep7);
        inc(destl);
        inc(w);

        if (ypos >= w.dc_yl) and (ypos <= w.dc_yh) then
        begin
          spot := (LongWord(frac8) shr FRACBITS)
            {$IFDEF MASK_TC}mod mod_height
            {$ELSE}
            and
            {$IFDEF MASK_127}127{$ENDIF}
            {$IFDEF MASK_255}255{$ENDIF}
            {$IFDEF MASK_511}511{$ENDIF}
            {$IFDEF MASK_1023}1023{$ENDIF}
            {$ENDIF};
          c := dc_source32_8[spot];
          r1 := c;
          g1 := c shr 8;
          b1 := c shr 16;
          destl^ := precal32_ic[r1 + g1 + b1];
        end;
        inc(frac8, fracstep8);

        destl := PLongWord(integer(destl) + swidth);
        inc(ypos);
      end;
    end;
  end;
