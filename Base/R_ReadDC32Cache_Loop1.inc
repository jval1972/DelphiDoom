{$IFDEF DOOM_OR_STRIFE}
        plw^ := cvideopal[src1^];
{$ELSE}
        plw^ := videopal[src1^];
{$ENDIF}	
        inc(plw);
{$IFNDEF LASTLOOP}
        inc(src1);
{$ENDIF}        
