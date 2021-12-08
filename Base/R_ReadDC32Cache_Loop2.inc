        plw^ := tbl[src1^, src2^];
        inc(plw);
{$IFNDEF LASTLOOP}
        inc(src1);
        inc(src2);
{$ENDIF}

