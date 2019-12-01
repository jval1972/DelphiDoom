{$IFDEF DOUBLESKY}
        plw^ := skvideopal[src1^];
{$ELSE}
        plw^ := videopal[src1^];
{$ENDIF}
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

