{$IFNDEF NO_INLINE_LOOPS}
  while count >= 4 do
  begin
    {$I R_DrawSkyColumnHi_Loop.inc}
    {$I R_DrawSkyColumnHi_Loop.inc}
    {$I R_DrawSkyColumnHi_Loop.inc}
    {$I R_DrawSkyColumnHi_Loop.inc}
    count := count - 4;
  end;
{$ENDIF}

  // JVAL Sky is full bright!
  fraclimit := frac + fracstep * count;
  while frac < fraclimit do
  begin
    {$I R_DrawSkyColumnHi_Loop.inc}
    {$I R_DrawSkyColumnHi_Loop.inc}
  end;
  while frac <= fraclimit do
  begin
    {$I R_DrawSkyColumnHi_Loop.inc}
  end;
