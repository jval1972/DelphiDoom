    //  Average with the next column and next pixel
    spot := (LongWord(frac) shr FRACBITS) and and_mask;
{$IFDEF SMALLSTEPOPTIMIZER}
    if lspot = spot then
    begin
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      inc(frac, fracstep);
    end
    else
    begin
      lspot := spot;
{$ENDIF}

      c := dc_source32[spot];
{$IFDEF MASKEDCOLUMN}
      if c <> 0 then
      begin
{$ENDIF}
{$IFDEF INVERSECOLORMAPS}
      {$I R_InverseLight.inc}
{$ELSE}
      destl^ := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
{$ENDIF}
{$IFDEF MASKEDCOLUMN}
      end;
{$ENDIF}

{$IFDEF SMALLSTEPOPTIMIZER}
      ldest := destl^;
{$ENDIF}

      destl := PLongWord(integer(destl) + swidth);
      inc(frac, fracstep);

{$IFDEF SMALLSTEPOPTIMIZER}
    end;
{$ENDIF}
