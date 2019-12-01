        plw^ := tbl[src1^, src2^];
        inc(plw);
{$IFNDEF LASTLOOP}
        inc(frac, fracstep);
        if frac > FRACUNIT then
        begin
          inc(src1);
          inc(src2);
{$IFNDEF PRELASTLOOP}
          frac := frac and $FFFF;
{$ENDIF}
        end;
{$ENDIF}

