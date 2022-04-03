  destb := @((ylookupl[parms.dl_yl]^)[columnofs[x]]);
  pitch := SCREENWIDTH * SizeOf(LongWord);
  for y := parms.dl_yl to parms.dl_yh do
  begin
    dls := source32[(LongWord(frac) shr FRACBITS) and (LIGHTTEXTURESIZE - 1)];
    if dls <> 0 then
    begin
      db := R_ZBufferAt(x, y);
      depth := db.depth;
      if (depth - dbmin) <= dbrange then
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

        if not skip and (seg = nil) and (db.planeheightz < MAXINT) then
          if (parms.lightplanez < 0) = (db.planeheightz < 0) then
            skip := abslightplanez > abs(db.planeheightz);

        if not skip then
        begin
          if not sameseg then
          begin
            dfactor := depth - scale;
            if dfactor < 0 then
              dfactor := ASM_CalcFactor(-dfactor, dbdmin)
            else
              dfactor := ASM_CalcFactor(dfactor, dbdmax);
          end;

          if dfactor > 0 then
          begin
            factor := FixedMulDiv256(dls, dfactor);

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
    end;
    inc(destb, pitch);
    inc(frac, fracstep);
  end;
