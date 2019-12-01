        plw^ := videopal[src1^];
        inc(plw);
{$IFNDEF LASTLOOP}
        inc(frac, fracstep);
        if frac > FRACUNIT then
        begin
          inc(src1);
{$IFNDEF PRELASTLOOP}
          frac := frac and $FFFF;
{$ENDIF}
        end;
{$ENDIF}

