  dest := @((ylookup[parms.dl_yl]^)[columnofs[x]]);
  pitch := SCREENWIDTH;
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
              {$IFDEF DOOM_OR_STRIFE}
              c := cvideopal[dest^];
              {$ELSE}
              c := curpal[dest^];
              {$ENDIF}

              rr := (c shr 16) and $FF;
              {$IFDEF TBL_R}
              rr := rr + (tbl_r[rr] * factor) shr 16;
              {$ENDIF}
              rr := rr shr FASTTABLESHIFT;

              gg := (c shr 8) and $FF;
              {$IFDEF TBL_G}
              gg := gg + (tbl_g[gg] * factor) shr 16;
              {$ENDIF}
              gg := gg shr FASTTABLESHIFT;

              bb := (c) and $FF;
              {$IFDEF TBL_B}
              bb := bb + (tbl_b[bb] * factor) shr 16;
              {$ENDIF}
              bb := bb shr FASTTABLESHIFT;

              dest^ := approxcolorindexarray[rr shl (16 - FASTTABLESHIFT - FASTTABLESHIFT) + gg shl (8 - FASTTABLESHIFT) + bb];
            end;


        {  Slower code:
            factor := FixedMul(dls, dfactor);
            if factor > 0 then
            begin
              r1 := FixedMul(r, factor);
              g1 := FixedMul(g, factor);
              b1 := FixedMul(b, factor);
              dest^ := R_FastApproxColorIndex(R_ColorLightAdd(curpal[dest^], r1, g1, b1));
            end; }

          end;
        end;
      end;
    end;
    inc(dest, pitch);
    inc(frac, fracstep);
  end;
