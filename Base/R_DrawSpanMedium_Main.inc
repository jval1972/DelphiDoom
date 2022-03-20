{$IFDEF SCALE64x64}
    xfrac := ds_xfrac;
    yfrac := ds_yfrac;
    xstep := ds_xstep;
    ystep := ds_ystep;
{$ENDIF}
{$IFDEF SCALE128x128}
    xfrac := ds_xfrac * 2;
    yfrac := ds_yfrac * 2;
    xstep := ds_xstep * 2;
    ystep := ds_ystep * 2;
{$ENDIF}
{$IFDEF SCALE256x256}
    xfrac := ds_xfrac * 4;
    yfrac := ds_yfrac * 4;
    xstep := ds_xstep * 4;
    ystep := ds_ystep * 4;
{$ENDIF}
{$IFDEF SCALE512x512}
    xfrac := ds_xfrac * 8;
    yfrac := ds_yfrac * 8;
    xstep := ds_xstep * 8;
    ystep := ds_ystep * 8;
{$ENDIF}

{$IFNDEF NO_INLINE_LOOPS}
  while count >= 4 do
  begin
    {$I R_DrawSpanMedium_Loop4.inc}
    count := count - 4;
  end;
{$ENDIF}

  for i := 0 to count do
  begin
    {$I R_DrawSpanMedium_Loop.inc}
  end;
