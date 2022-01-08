  destb := @((ylookupl[parms.dl_yl]^)[columnofs[x]]);
  pitch := SCREENWIDTH * SizeOf(LongWord);
  pitch1 := pitch;
  {$IFDEF TBL_R}
  pitch1 := pitch1 - 2;
  {$ELSE}
  {$IFDEF TBL_G}
  pitch1 := pitch1 - 1;
  {$ENDIF}
  {$ENDIF}
  fastzbuf.next := parms.dl_yl - 1;
  for y := parms.dl_yl to parms.dl_yh do
  begin
    dls := source32[(LongWord(frac) shr FRACBITS) and (LIGHTTEXTURESIZE - 1)];
    if dls <> 0 then
    begin
      db := R_FastZBufferAt(x, y, @fastzbuf);
      depth := db.depth;
      if (depth >= dbmin) and (depth <= dbmax) then
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
              dfactor := FRACUNIT - FixedDiv(-dfactor, dbdmin)
            else
              dfactor := FRACUNIT - FixedDiv(dfactor, dbdmax);
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
              inc(destb);
              destb^ := destb^ + (tbl_g[destb^] * factor) shr 16;
              {$ENDIF}

              {$IFDEF TBL_R}
              {$IFDEF TBL_G}
              inc(destb);
              {$ELSE}
              inc(destb, 2);
              {$ENDIF}
              destb^ := destb^ + (tbl_r[destb^] * factor) shr 16;
              {$ENDIF}

              inc(destb, pitch1);
              inc(frac, fracstep);
              Continue;
            end;

          end;
        end;
      end;
    end;
    inc(destb, pitch);
    inc(frac, fracstep);
  end;
