{$IFNDEF NO_INLINE_LOOPS}
  while count >= 4 do
  begin
    {$I R_DrawColumnAverageUltra_Loop.inc}
    {$I R_DrawColumnAverageUltra_Loop.inc}
    {$I R_DrawColumnAverageUltra_Loop.inc}
    {$I R_DrawColumnAverageUltra_Loop.inc}
    count := count - 4;
  end;
{$ENDIF}

  fraclimit := frac + fracstep * count;
  while frac < fraclimit do
  begin
    {$I R_DrawColumnAverageUltra_Loop.inc}
    {$I R_DrawColumnAverageUltra_Loop.inc}
  end;
  while frac <= fraclimit do
  begin
    {$I R_DrawColumnAverageUltra_Loop.inc}
  end;
