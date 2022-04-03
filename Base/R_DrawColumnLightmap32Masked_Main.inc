  destb := @((ylookupl[yl]^)[columnofs[x]]);
  pitch := SCREENWIDTH * SizeOf(LongWord);
  for y := yl to yh do
  begin
    db := cache[y];
    if db.pass = 0 then
    begin
      inc(destb, pitch);
      inc(frac, fracstep);
      Continue;
    end;

    depth := db.depth;
    if (okself or (db.mo <> parms.lightsourcemo)) and ((depth - dbmin) <= dbrange) then
    begin
      if seg <> db.seg then
      begin
        sameseg := (seg = db.seg) and (seg <> nil);
        seg := db.seg;
        if seg <> nil then
          skip := R_PointOnSegSide(parms.lightsourcex, parms.lightsourcey, seg)
        else
          skip := false;
      end;

      if not skip then
      begin
        if not sameseg then
        begin
          dfactor := depth - scale;
          if dfactor < 0 then
            dfactor := FRACUNIT - FixedDivLW(-dfactor, dbdmin)
          else
            dfactor := FRACUNIT - FixedDivLW(dfactor, dbdmax);
        end;

        if dfactor > 0 then
        begin
          factor := FixedMulDiv256(source32[(LongWord(frac) shr FRACBITS) and (LIGHTTEXTURESIZE - 1)], dfactor);

          if factor > 0 then
          begin
            {$IFDEF TBL_B}
            destb^ := destb^ + (tbl_b[destb^] * factor) shr 16;
            {$ENDIF}
            {$IFDEF TBL_G}
            PByteArray(destb)[1] := PByteArray(destb)[1] + (tbl_g[PByteArray(destb)[1]] * factor) shr 16;
            {$ENDIF}
            {$IFDEF TBL_R}
            PByteArray(destb)[2] := PByteArray(destb)[2] + (tbl_r[PByteArray(destb)[2]] * factor) shr 16;
            {$ENDIF}
          end;

        end;
      end;

    end;
    inc(destb, pitch);
    inc(frac, fracstep);
  end;
