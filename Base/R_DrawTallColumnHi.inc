{$IFNDEF NO_INLINE_LOOPS}
  while count{$IFNDEF HEXEN} >= 32 do
  begin
    {$I R_DrawTallColumnHi_Loop.inc}
    {$I R_DrawTallColumnHi_Loop.inc}
    {$I R_DrawTallColumnHi_Loop.inc}
    {$I R_DrawTallColumnHi_Loop.inc}
    {$I R_DrawTallColumnHi_Loop.inc}
    {$I R_DrawTallColumnHi_Loop.inc}
    {$I R_DrawTallColumnHi_Loop.inc}
    {$I R_DrawTallColumnHi_Loop.inc}
    {$I R_DrawTallColumnHi_Loop.inc}
    {$I R_DrawTallColumnHi_Loop.inc}
    {$I R_DrawTallColumnHi_Loop.inc}
    {$I R_DrawTallColumnHi_Loop.inc}
    {$I R_DrawTallColumnHi_Loop.inc}
    {$I R_DrawTallColumnHi_Loop.inc}
    {$I R_DrawTallColumnHi_Loop.inc}
    {$I R_DrawTallColumnHi_Loop.inc}
    {$I R_DrawTallColumnHi_Loop.inc}
    {$I R_DrawTallColumnHi_Loop.inc}
    {$I R_DrawTallColumnHi_Loop.inc}
    {$I R_DrawTallColumnHi_Loop.inc}
    {$I R_DrawTallColumnHi_Loop.inc}
    {$I R_DrawTallColumnHi_Loop.inc}
    {$I R_DrawTallColumnHi_Loop.inc}
    {$I R_DrawTallColumnHi_Loop.inc}
    {$I R_DrawTallColumnHi_Loop.inc}
    {$I R_DrawTallColumnHi_Loop.inc}
    {$I R_DrawTallColumnHi_Loop.inc}
    {$I R_DrawTallColumnHi_Loop.inc}
    {$I R_DrawTallColumnHi_Loop.inc}
    {$I R_DrawTallColumnHi_Loop.inc}
    {$I R_DrawTallColumnHi_Loop.inc}
    {$I R_DrawTallColumnHi_Loop.inc}
    count := count - 32;
  end;

  if count{$ENDIF} >= 16 {$IFDEF HEXEN}do{$ELSE}then{$ENDIF}
  begin
    {$I R_DrawTallColumnHi_Loop.inc}
    {$I R_DrawTallColumnHi_Loop.inc}
    {$I R_DrawTallColumnHi_Loop.inc}
    {$I R_DrawTallColumnHi_Loop.inc}
    {$I R_DrawTallColumnHi_Loop.inc}
    {$I R_DrawTallColumnHi_Loop.inc}
    {$I R_DrawTallColumnHi_Loop.inc}
    {$I R_DrawTallColumnHi_Loop.inc}
    {$I R_DrawTallColumnHi_Loop.inc}
    {$I R_DrawTallColumnHi_Loop.inc}
    {$I R_DrawTallColumnHi_Loop.inc}
    {$I R_DrawTallColumnHi_Loop.inc}
    {$I R_DrawTallColumnHi_Loop.inc}
    {$I R_DrawTallColumnHi_Loop.inc}
    {$I R_DrawTallColumnHi_Loop.inc}
    {$I R_DrawTallColumnHi_Loop.inc}
    count := count - 16;
  end;

  while count >= 4 do
  begin
    {$I R_DrawTallColumnHi_Loop.inc}
    {$I R_DrawTallColumnHi_Loop.inc}
    {$I R_DrawTallColumnHi_Loop.inc}
    {$I R_DrawTallColumnHi_Loop.inc}
    count := count - 4;
  end;
{$ENDIF}

  fraclimit := frac + count * fracstep;
  while frac < fraclimit do
  begin
    {$I R_DrawTallColumnHi_Loop.inc}
    {$I R_DrawTallColumnHi_Loop.inc}
  end;
  while frac <= fraclimit do
  begin
    {$I R_DrawTallColumnHi_Loop.inc}
  end;
