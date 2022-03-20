    xfrac := ds_xfrac;
    yfrac := ds_yfrac;
    xstep := ds_xstep;
    ystep := ds_ystep;

{$IFNDEF NO_INLINE_LOOPS}
  while count >= 4 do
  begin
    {$I R_DrawSlopeMediumSize_Loop4.inc}
    count := count - 4;
  end;

{$ENDIF}

  for i := 0 to count do
  begin
    {$I R_DrawSlopeMediumSize_Loop.inc}
  end;
