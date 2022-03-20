  if fracstep < FRACUNIT then
  begin
{$IFNDEF NO_INLINE_LOOPS}
    while count >= 4 do
    begin
      {$I R_DrawTallColumnUltra_Loop.inc}
      {$I R_DrawTallColumnUltra_Loop.inc}
      {$I R_DrawTallColumnUltra_Loop.inc}
      {$I R_DrawTallColumnUltra_Loop.inc}
      count := count - 4;
    end;
{$ENDIF}

    fraclimit := frac + fracstep * count;
    while frac < fraclimit do
    begin
      {$I R_DrawTallColumnUltra_Loop.inc}
      {$I R_DrawTallColumnUltra_Loop.inc}
    end;
    while frac <= fraclimit do
    begin
      {$I R_DrawTallColumnUltra_Loop.inc}
    end;
  end
  else
  begin
  {$UNDEF SMALLSTEPOPTIMIZER}
{$IFNDEF NO_INLINE_LOOPS}
    while count >= 4 do
    begin
      {$I R_DrawTallColumnHi_Loop.inc}
      {$I R_DrawTallColumnHi_Loop.inc}
      {$I R_DrawTallColumnHi_Loop.inc}
      {$I R_DrawTallColumnHi_Loop.inc}
      count := count - 4;
    end;
{$ENDIF}

    fraclimit := frac + fracstep * count;
    while frac < fraclimit do
    begin
      {$I R_DrawTallColumnHi_Loop.inc}
      {$I R_DrawTallColumnHi_Loop.inc}
    end;
    while frac <= fraclimit do
    begin
      {$I R_DrawTallColumnHi_Loop.inc}
    end;
  end;
