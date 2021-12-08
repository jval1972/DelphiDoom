  if fracstep < FRACUNIT then
  begin
{$IFNDEF NO_INLINE_LOOPS}
    while count >= 32 do
    begin
      {$I R_DrawColumnUltra_Loop.inc}
      {$I R_DrawColumnUltra_Loop.inc}
      {$I R_DrawColumnUltra_Loop.inc}
      {$I R_DrawColumnUltra_Loop.inc}
      {$I R_DrawColumnUltra_Loop.inc}
      {$I R_DrawColumnUltra_Loop.inc}
      {$I R_DrawColumnUltra_Loop.inc}
      {$I R_DrawColumnUltra_Loop.inc}
      {$I R_DrawColumnUltra_Loop.inc}
      {$I R_DrawColumnUltra_Loop.inc}
      {$I R_DrawColumnUltra_Loop.inc}
      {$I R_DrawColumnUltra_Loop.inc}
      {$I R_DrawColumnUltra_Loop.inc}
      {$I R_DrawColumnUltra_Loop.inc}
      {$I R_DrawColumnUltra_Loop.inc}
      {$I R_DrawColumnUltra_Loop.inc}
      {$I R_DrawColumnUltra_Loop.inc}
      {$I R_DrawColumnUltra_Loop.inc}
      {$I R_DrawColumnUltra_Loop.inc}
      {$I R_DrawColumnUltra_Loop.inc}
      {$I R_DrawColumnUltra_Loop.inc}
      {$I R_DrawColumnUltra_Loop.inc}
      {$I R_DrawColumnUltra_Loop.inc}
      {$I R_DrawColumnUltra_Loop.inc}
      {$I R_DrawColumnUltra_Loop.inc}
      {$I R_DrawColumnUltra_Loop.inc}
      {$I R_DrawColumnUltra_Loop.inc}
      {$I R_DrawColumnUltra_Loop.inc}
      {$I R_DrawColumnUltra_Loop.inc}
      {$I R_DrawColumnUltra_Loop.inc}
      {$I R_DrawColumnUltra_Loop.inc}
      {$I R_DrawColumnUltra_Loop.inc}
      count := count - 32;
    end;

    if count >= 16 then
    begin
      {$I R_DrawColumnUltra_Loop.inc}
      {$I R_DrawColumnUltra_Loop.inc}
      {$I R_DrawColumnUltra_Loop.inc}
      {$I R_DrawColumnUltra_Loop.inc}
      {$I R_DrawColumnUltra_Loop.inc}
      {$I R_DrawColumnUltra_Loop.inc}
      {$I R_DrawColumnUltra_Loop.inc}
      {$I R_DrawColumnUltra_Loop.inc}
      {$I R_DrawColumnUltra_Loop.inc}
      {$I R_DrawColumnUltra_Loop.inc}
      {$I R_DrawColumnUltra_Loop.inc}
      {$I R_DrawColumnUltra_Loop.inc}
      {$I R_DrawColumnUltra_Loop.inc}
      {$I R_DrawColumnUltra_Loop.inc}
      {$I R_DrawColumnUltra_Loop.inc}
      {$I R_DrawColumnUltra_Loop.inc}
      count := count - 16;
    end;

    while count >= 4 do
    begin
      {$I R_DrawColumnUltra_Loop.inc}
      {$I R_DrawColumnUltra_Loop.inc}
      {$I R_DrawColumnUltra_Loop.inc}
      {$I R_DrawColumnUltra_Loop.inc}
      count := count - 4;
    end;
{$ENDIF}

    fraclimit := frac + fracstep * count;
    while frac < fraclimit do
    begin
      {$I R_DrawColumnUltra_Loop.inc}
      {$I R_DrawColumnUltra_Loop.inc}
    end;
    while frac <= fraclimit do
    begin
      {$I R_DrawColumnUltra_Loop.inc}
    end;
  end
  else
  begin
  {$UNDEF SMALLSTEPOPTIMIZER}
{$IFNDEF NO_INLINE_LOOPS}
    while count >= 16 do
    begin
      {$I R_DrawColumnHi_Loop.inc}
      {$I R_DrawColumnHi_Loop.inc}
      {$I R_DrawColumnHi_Loop.inc}
      {$I R_DrawColumnHi_Loop.inc}
      {$I R_DrawColumnHi_Loop.inc}
      {$I R_DrawColumnHi_Loop.inc}
      {$I R_DrawColumnHi_Loop.inc}
      {$I R_DrawColumnHi_Loop.inc}
      {$I R_DrawColumnHi_Loop.inc}
      {$I R_DrawColumnHi_Loop.inc}
      {$I R_DrawColumnHi_Loop.inc}
      {$I R_DrawColumnHi_Loop.inc}
      {$I R_DrawColumnHi_Loop.inc}
      {$I R_DrawColumnHi_Loop.inc}
      {$I R_DrawColumnHi_Loop.inc}
      {$I R_DrawColumnHi_Loop.inc}
      count := count - 16;
    end;

    while count >= 4 do
    begin
      {$I R_DrawColumnHi_Loop.inc}
      {$I R_DrawColumnHi_Loop.inc}
      {$I R_DrawColumnHi_Loop.inc}
      {$I R_DrawColumnHi_Loop.inc}
      count := count - 4;
    end;
{$ENDIF}

    fraclimit := frac + fracstep * count;
    while frac < fraclimit do
    begin
      {$I R_DrawColumnHi_Loop.inc}
      {$I R_DrawColumnHi_Loop.inc}
    end;
    while frac <= fraclimit do
    begin
      {$I R_DrawColumnHi_Loop.inc}
    end;
  end;
