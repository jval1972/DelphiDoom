      // Current texture index in u,v.
{$IFDEF FPC}
{$IFDEF SCALE64x64}
      spot := _SHR(yfrac, 10) and (4032) + _SHR(xfrac, FRACBITS) and 63;
{$ENDIF}
{$IFDEF SCALE128x128}
      spot := _SHR(yfrac, 9) and (16256) + _SHR(xfrac, FRACBITS) and 127;
{$ENDIF}
{$IFDEF SCALE256x256}
      spot := _SHR(yfrac, 8) and (65280) + _SHR(xfrac, FRACBITS) and 255;
{$ENDIF}
{$IFDEF SCALE512x512}
      spot := _SHR(yfrac, 7) and (261632) + _SHR(xfrac, FRACBITS) and 511;
{$ENDIF}
{$ELSE}
{$IFDEF SCALE64x64}
      spot := (LongWord(yfrac) shr 10) and (4032) or (LongWord(xfrac) shr FRACBITS) and 63;
{$ENDIF}
{$IFDEF SCALE128x128}
      spot := (LongWord(yfrac) shr 9) and (16256) or (LongWord(xfrac) shr FRACBITS) and 127;
{$ENDIF}
{$IFDEF SCALE256x256}
      spot := (LongWord(yfrac) shr 8) and (65280) or (LongWord(xfrac) shr FRACBITS) and 255;
{$ENDIF}
{$IFDEF SCALE512x512}
      spot := (LongWord(yfrac) shr 7) and (261632) or (LongWord(xfrac) shr FRACBITS) and 511;
{$ENDIF}
{$ENDIF}

      // Lookup pixel from flat texture tile,
      //  transformed using light of ds_lightlevel
      c := ds_source32[spot];
{$IFDEF INVERSECOLORMAPS}
      {$I R_InverseLight.inc}
{$ELSE}
{$IFDEF TRANSPARENTFLAT}
      destl^ := R_ColorMidAverage(c, destl^);
{$ELSE}
      destl^ := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];

//      {$I R_ColorLight.inc}
{$ENDIF}
{$ENDIF}
      inc(destl);

      // Next step in u,v.
      xfrac := xfrac + xstep;
      yfrac := yfrac + ystep;
