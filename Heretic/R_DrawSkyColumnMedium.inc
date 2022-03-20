{$IFNDEF NO_INLINE_LOOPS}
  while count >= 4 do
  begin
    {$I R_DrawSkyColumnMedium_Loop.inc}
    {$I R_DrawSkyColumnMedium_Loop.inc}
    {$I R_DrawSkyColumnMedium_Loop.inc}
    {$I R_DrawSkyColumnMedium_Loop.inc}
    count := count - 4;
  end;
{$ENDIF}

  for i := 0 to count do
  begin
    {$I R_DrawSkyColumnMedium_Loop.inc}
  end;
