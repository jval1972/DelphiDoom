      // Current texture index in u,v.
{$IFDEF FPC}
{$IFDEF SCALE64x64}
{$IFDEF RIPPLE}
      spot := ((LongWord(yfrac + rpl[(LongWord(xfrac) shr 16) and 127]) shr 10) and (4032)) or
              ((LongWord(xfrac + rpl[(LongWord(yfrac) shr 16) and 127]) shr FRACBITS) and 63);
{$ELSE}
      spot := _SHR(yfrac, 10) and (4032) + _SHR(xfrac, FRACBITS) and 63;
{$ENDIF}
{$ENDIF}
{$IFDEF SCALE128x128}
{$IFDEF RIPPLE}
      spot := (LongWord(yfrac + 2 * rpl[(LongWord(xfrac) shr 17) and 127]) shr 9) and (16256) or
              (LongWord(xfrac + 2 * rpl[(LongWord(yfrac) shr 17) and 127]) shr FRACBITS) and 127;
{$ELSE}
      spot := _SHR(yfrac, 9) and (16256) + _SHR(xfrac, FRACBITS) and 127;
{$ENDIF}
{$ENDIF}
{$IFDEF SCALE256x256}
{$IFDEF RIPPLE}
      spot := (LongWord(yfrac + 4 * rpl[(LongWord(xfrac) shr 18) and 127]) shr 8) and (65280) or
              (LongWord(xfrac + 4 * rpl[(LongWord(yfrac) shr 18) and 127]) shr FRACBITS) and 255;
{$ELSE}
      spot := _SHR(yfrac, 8) and (65280) + _SHR(xfrac, FRACBITS) and 255;
{$ENDIF}
{$ENDIF}
{$IFDEF SCALE512x512}
{$IFDEF RIPPLE}
      spot := (LongWord(yfrac + 8 * rpl[(LongWord(xfrac) shr 19) and 127]) shr 7) and (261632) or
              (LongWord(xfrac + 8 * rpl[(LongWord(yfrac) shr 19) and 127]) shr FRACBITS) and 511;
{$ELSE}
      spot := _SHR(yfrac, 7) and (261632) + _SHR(xfrac, FRACBITS) and 511;
{$ENDIF}
{$ENDIF}

{$ELSE}
{$IFDEF SCALE64x64}
{$IFDEF RIPPLE}
      spot := ((LongWord(yfrac + rpl[(LongWord(xfrac) shr 16) and 127]) shr 10) and (4032)) or
              ((LongWord(xfrac + rpl[(LongWord(yfrac) shr 16) and 127]) shr FRACBITS) and 63);
{$ELSE}
      spot := (LongWord(yfrac) shr 10) and (4032) or (LongWord(xfrac) shr FRACBITS) and 63;
{$ENDIF}
{$ENDIF}
{$IFDEF SCALE128x128}
{$IFDEF RIPPLE}
      spot := (LongWord(yfrac + 2 * rpl[(LongWord(xfrac) shr 17) and 127]) shr 9) and (16256) or
              (LongWord(xfrac + 2 * rpl[(LongWord(yfrac) shr 17) and 127]) shr FRACBITS) and 127;
{$ELSE}
      spot := (LongWord(yfrac) shr 9) and (16256) or (LongWord(xfrac) shr FRACBITS) and 127;
{$ENDIF}
{$ENDIF}
{$IFDEF SCALE256x256}
{$IFDEF RIPPLE}
      spot := (LongWord(yfrac + 4 * rpl[(LongWord(xfrac) shr 18) and 127]) shr 8) and (65280) or
              (LongWord(xfrac + 4 * rpl[(LongWord(yfrac) shr 18) and 127]) shr FRACBITS) and 255;
{$ELSE}
      spot := (LongWord(yfrac) shr 8) and (65280) or (LongWord(xfrac) shr FRACBITS) and 255;
{$ENDIF}
{$ENDIF}
{$IFDEF SCALE512x512}
{$IFDEF RIPPLE}
      spot := (LongWord(yfrac + 8 * rpl[(LongWord(xfrac) shr 19) and 127]) shr 7) and (261632) or
              (LongWord(xfrac + 8 * rpl[(LongWord(yfrac) shr 19) and 127]) shr FRACBITS) and 511;
{$ELSE}
      spot := (LongWord(yfrac) shr 7) and (261632) or (LongWord(xfrac) shr FRACBITS) and 511;
{$ENDIF}
{$ENDIF}
{$ENDIF}

      // Lookup pixel from flat texture tile,
      //  transformed using light of ds_lightlevel
      c := ds_source32[spot];
{$IFDEF INVERSECOLORMAPS}
        r1 := c;
        g1 := c shr 8;
        b1 := c shr 16;
        PLongWordArray(destl)[0] := precal32_ic[r1 + g1 + b1];
{$ELSE}
{$IFDEF TRANSPARENTFLAT}
      PLongWordArray(destl)[0] := R_ColorMidAverage(c, destl^);
{$ELSE}
      PLongWordArray(destl)[0] := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
{$ENDIF}
{$ENDIF}
      // Next step in u,v.
      xfrac := xfrac + xstep;
      yfrac := yfrac + ystep;

      // Current texture index in u,v.
{$IFDEF FPC}
{$IFDEF SCALE64x64}
{$IFDEF RIPPLE}
      spot := ((LongWord(yfrac + rpl[(LongWord(xfrac) shr 16) and 127]) shr 10) and (4032)) or
              ((LongWord(xfrac + rpl[(LongWord(yfrac) shr 16) and 127]) shr FRACBITS) and 63);
{$ELSE}
      spot := _SHR(yfrac, 10) and (4032) + _SHR(xfrac, FRACBITS) and 63;
{$ENDIF}
{$ENDIF}
{$IFDEF SCALE128x128}
{$IFDEF RIPPLE}
      spot := (LongWord(yfrac + 2 * rpl[(LongWord(xfrac) shr 17) and 127]) shr 9) and (16256) or
              (LongWord(xfrac + 2 * rpl[(LongWord(yfrac) shr 17) and 127]) shr FRACBITS) and 127;
{$ELSE}
      spot := _SHR(yfrac, 9) and (16256) + _SHR(xfrac, FRACBITS) and 127;
{$ENDIF}
{$ENDIF}
{$IFDEF SCALE256x256}
{$IFDEF RIPPLE}
      spot := (LongWord(yfrac + 4 * rpl[(LongWord(xfrac) shr 18) and 127]) shr 8) and (65280) or
              (LongWord(xfrac + 4 * rpl[(LongWord(yfrac) shr 18) and 127]) shr FRACBITS) and 255;
{$ELSE}
      spot := _SHR(yfrac, 8) and (65280) + _SHR(xfrac, FRACBITS) and 255;
{$ENDIF}
{$ENDIF}
{$IFDEF SCALE512x512}
{$IFDEF RIPPLE}
      spot := (LongWord(yfrac + 8 * rpl[(LongWord(xfrac) shr 19) and 127]) shr 7) and (261632) or
              (LongWord(xfrac + 8 * rpl[(LongWord(yfrac) shr 19) and 127]) shr FRACBITS) and 511;
{$ELSE}
      spot := _SHR(yfrac, 7) and (261632) + _SHR(xfrac, FRACBITS) and 511;
{$ENDIF}
{$ENDIF}

{$ELSE}
{$IFDEF SCALE64x64}
{$IFDEF RIPPLE}
      spot := ((LongWord(yfrac + rpl[(LongWord(xfrac) shr 16) and 127]) shr 10) and (4032)) or
              ((LongWord(xfrac + rpl[(LongWord(yfrac) shr 16) and 127]) shr FRACBITS) and 63);
{$ELSE}
      spot := (LongWord(yfrac) shr 10) and (4032) or (LongWord(xfrac) shr FRACBITS) and 63;
{$ENDIF}
{$ENDIF}
{$IFDEF SCALE128x128}
{$IFDEF RIPPLE}
      spot := (LongWord(yfrac + 2 * rpl[(LongWord(xfrac) shr 17) and 127]) shr 9) and (16256) or
              (LongWord(xfrac + 2 * rpl[(LongWord(yfrac) shr 17) and 127]) shr FRACBITS) and 127;
{$ELSE}
      spot := (LongWord(yfrac) shr 9) and (16256) or (LongWord(xfrac) shr FRACBITS) and 127;
{$ENDIF}
{$ENDIF}
{$IFDEF SCALE256x256}
{$IFDEF RIPPLE}
      spot := (LongWord(yfrac + 4 * rpl[(LongWord(xfrac) shr 18) and 127]) shr 8) and (65280) or
              (LongWord(xfrac + 4 * rpl[(LongWord(yfrac) shr 18) and 127]) shr FRACBITS) and 255;
{$ELSE}
      spot := (LongWord(yfrac) shr 8) and (65280) or (LongWord(xfrac) shr FRACBITS) and 255;
{$ENDIF}
{$ENDIF}
{$IFDEF SCALE512x512}
{$IFDEF RIPPLE}
      spot := (LongWord(yfrac + 8 * rpl[(LongWord(xfrac) shr 19) and 127]) shr 7) and (261632) or
              (LongWord(xfrac + 8 * rpl[(LongWord(yfrac) shr 19) and 127]) shr FRACBITS) and 511;
{$ELSE}
      spot := (LongWord(yfrac) shr 7) and (261632) or (LongWord(xfrac) shr FRACBITS) and 511;
{$ENDIF}
{$ENDIF}
{$ENDIF}

      // Lookup pixel from flat texture tile,
      //  transformed using light of ds_lightlevel
      c := ds_source32[spot];
{$IFDEF INVERSECOLORMAPS}
        r1 := c;
        g1 := c shr 8;
        b1 := c shr 16;
        PLongWordArray(destl)[1] := precal32_ic[r1 + g1 + b1];
{$ELSE}
{$IFDEF TRANSPARENTFLAT}
      PLongWordArray(destl)[1] := R_ColorMidAverage(c, destl^);
{$ELSE}
      PLongWordArray(destl)[1] := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
{$ENDIF}
{$ENDIF}
      // Next step in u,v.
      xfrac := xfrac + xstep;
      yfrac := yfrac + ystep;

      // Current texture index in u,v.
{$IFDEF FPC}
{$IFDEF SCALE64x64}
{$IFDEF RIPPLE}
      spot := ((LongWord(yfrac + rpl[(LongWord(xfrac) shr 16) and 127]) shr 10) and (4032)) or
              ((LongWord(xfrac + rpl[(LongWord(yfrac) shr 16) and 127]) shr FRACBITS) and 63);
{$ELSE}
      spot := _SHR(yfrac, 10) and (4032) + _SHR(xfrac, FRACBITS) and 63;
{$ENDIF}
{$ENDIF}
{$IFDEF SCALE128x128}
{$IFDEF RIPPLE}
      spot := (LongWord(yfrac + 2 * rpl[(LongWord(xfrac) shr 17) and 127]) shr 9) and (16256) or
              (LongWord(xfrac + 2 * rpl[(LongWord(yfrac) shr 17) and 127]) shr FRACBITS) and 127;
{$ELSE}
      spot := _SHR(yfrac, 9) and (16256) + _SHR(xfrac, FRACBITS) and 127;
{$ENDIF}
{$ENDIF}
{$IFDEF SCALE256x256}
{$IFDEF RIPPLE}
      spot := (LongWord(yfrac + 4 * rpl[(LongWord(xfrac) shr 18) and 127]) shr 8) and (65280) or
              (LongWord(xfrac + 4 * rpl[(LongWord(yfrac) shr 18) and 127]) shr FRACBITS) and 255;
{$ELSE}
      spot := _SHR(yfrac, 8) and (65280) + _SHR(xfrac, FRACBITS) and 255;
{$ENDIF}
{$ENDIF}
{$IFDEF SCALE512x512}
{$IFDEF RIPPLE}
      spot := (LongWord(yfrac + 8 * rpl[(LongWord(xfrac) shr 19) and 127]) shr 7) and (261632) or
              (LongWord(xfrac + 8 * rpl[(LongWord(yfrac) shr 19) and 127]) shr FRACBITS) and 511;
{$ELSE}
      spot := _SHR(yfrac, 7) and (261632) + _SHR(xfrac, FRACBITS) and 511;
{$ENDIF}
{$ENDIF}

{$ELSE}
{$IFDEF SCALE64x64}
{$IFDEF RIPPLE}
      spot := ((LongWord(yfrac + rpl[(LongWord(xfrac) shr 16) and 127]) shr 10) and (4032)) or
              ((LongWord(xfrac + rpl[(LongWord(yfrac) shr 16) and 127]) shr FRACBITS) and 63);
{$ELSE}
      spot := (LongWord(yfrac) shr 10) and (4032) or (LongWord(xfrac) shr FRACBITS) and 63;
{$ENDIF}
{$ENDIF}
{$IFDEF SCALE128x128}
{$IFDEF RIPPLE}
      spot := (LongWord(yfrac + 2 * rpl[(LongWord(xfrac) shr 17) and 127]) shr 9) and (16256) or
              (LongWord(xfrac + 2 * rpl[(LongWord(yfrac) shr 17) and 127]) shr FRACBITS) and 127;
{$ELSE}
      spot := (LongWord(yfrac) shr 9) and (16256) or (LongWord(xfrac) shr FRACBITS) and 127;
{$ENDIF}
{$ENDIF}
{$IFDEF SCALE256x256}
{$IFDEF RIPPLE}
      spot := (LongWord(yfrac + 4 * rpl[(LongWord(xfrac) shr 18) and 127]) shr 8) and (65280) or
              (LongWord(xfrac + 4 * rpl[(LongWord(yfrac) shr 18) and 127]) shr FRACBITS) and 255;
{$ELSE}
      spot := (LongWord(yfrac) shr 8) and (65280) or (LongWord(xfrac) shr FRACBITS) and 255;
{$ENDIF}
{$ENDIF}
{$IFDEF SCALE512x512}
{$IFDEF RIPPLE}
      spot := (LongWord(yfrac + 8 * rpl[(LongWord(xfrac) shr 19) and 127]) shr 7) and (261632) or
              (LongWord(xfrac + 8 * rpl[(LongWord(yfrac) shr 19) and 127]) shr FRACBITS) and 511;
{$ELSE}
      spot := (LongWord(yfrac) shr 7) and (261632) or (LongWord(xfrac) shr FRACBITS) and 511;
{$ENDIF}
{$ENDIF}
{$ENDIF}

      // Lookup pixel from flat texture tile,
      //  transformed using light of ds_lightlevel
      c := ds_source32[spot];
{$IFDEF INVERSECOLORMAPS}
        r1 := c;
        g1 := c shr 8;
        b1 := c shr 16;
        PLongWordArray(destl)[2] := precal32_ic[r1 + g1 + b1];
{$ELSE}
{$IFDEF TRANSPARENTFLAT}
      PLongWordArray(destl)[2] := R_ColorMidAverage(c, destl^);
{$ELSE}
      PLongWordArray(destl)[2] := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
{$ENDIF}
{$ENDIF}
      // Next step in u,v.
      xfrac := xfrac + xstep;
      yfrac := yfrac + ystep;

      // Current texture index in u,v.
{$IFDEF FPC}
{$IFDEF SCALE64x64}
{$IFDEF RIPPLE}
      spot := ((LongWord(yfrac + rpl[(LongWord(xfrac) shr 16) and 127]) shr 10) and (4032)) or
              ((LongWord(xfrac + rpl[(LongWord(yfrac) shr 16) and 127]) shr FRACBITS) and 63);
{$ELSE}
      spot := _SHR(yfrac, 10) and (4032) + _SHR(xfrac, FRACBITS) and 63;
{$ENDIF}
{$ENDIF}
{$IFDEF SCALE128x128}
{$IFDEF RIPPLE}
      spot := (LongWord(yfrac + 2 * rpl[(LongWord(xfrac) shr 17) and 127]) shr 9) and (16256) or
              (LongWord(xfrac + 2 * rpl[(LongWord(yfrac) shr 17) and 127]) shr FRACBITS) and 127;
{$ELSE}
      spot := _SHR(yfrac, 9) and (16256) + _SHR(xfrac, FRACBITS) and 127;
{$ENDIF}
{$ENDIF}
{$IFDEF SCALE256x256}
{$IFDEF RIPPLE}
      spot := (LongWord(yfrac + 4 * rpl[(LongWord(xfrac) shr 18) and 127]) shr 8) and (65280) or
              (LongWord(xfrac + 4 * rpl[(LongWord(yfrac) shr 18) and 127]) shr FRACBITS) and 255;
{$ELSE}
      spot := _SHR(yfrac, 8) and (65280) + _SHR(xfrac, FRACBITS) and 255;
{$ENDIF}
{$ENDIF}
{$IFDEF SCALE512x512}
{$IFDEF RIPPLE}
      spot := (LongWord(yfrac + 8 * rpl[(LongWord(xfrac) shr 19) and 127]) shr 7) and (261632) or
              (LongWord(xfrac + 8 * rpl[(LongWord(yfrac) shr 19) and 127]) shr FRACBITS) and 511;
{$ELSE}
      spot := _SHR(yfrac, 7) and (261632) + _SHR(xfrac, FRACBITS) and 511;
{$ENDIF}
{$ENDIF}

{$ELSE}
{$IFDEF SCALE64x64}
{$IFDEF RIPPLE}
      spot := ((LongWord(yfrac + rpl[(LongWord(xfrac) shr 16) and 127]) shr 10) and (4032)) or
              ((LongWord(xfrac + rpl[(LongWord(yfrac) shr 16) and 127]) shr FRACBITS) and 63);
{$ELSE}
      spot := (LongWord(yfrac) shr 10) and (4032) or (LongWord(xfrac) shr FRACBITS) and 63;
{$ENDIF}
{$ENDIF}
{$IFDEF SCALE128x128}
{$IFDEF RIPPLE}
      spot := (LongWord(yfrac + 2 * rpl[(LongWord(xfrac) shr 17) and 127]) shr 9) and (16256) or
              (LongWord(xfrac + 2 * rpl[(LongWord(yfrac) shr 17) and 127]) shr FRACBITS) and 127;
{$ELSE}
      spot := (LongWord(yfrac) shr 9) and (16256) or (LongWord(xfrac) shr FRACBITS) and 127;
{$ENDIF}
{$ENDIF}
{$IFDEF SCALE256x256}
{$IFDEF RIPPLE}
      spot := (LongWord(yfrac + 4 * rpl[(LongWord(xfrac) shr 18) and 127]) shr 8) and (65280) or
              (LongWord(xfrac + 4 * rpl[(LongWord(yfrac) shr 18) and 127]) shr FRACBITS) and 255;
{$ELSE}
      spot := (LongWord(yfrac) shr 8) and (65280) or (LongWord(xfrac) shr FRACBITS) and 255;
{$ENDIF}
{$ENDIF}
{$IFDEF SCALE512x512}
{$IFDEF RIPPLE}
      spot := (LongWord(yfrac + 8 * rpl[(LongWord(xfrac) shr 19) and 127]) shr 7) and (261632) or
              (LongWord(xfrac + 8 * rpl[(LongWord(yfrac) shr 19) and 127]) shr FRACBITS) and 511;
{$ELSE}
      spot := (LongWord(yfrac) shr 7) and (261632) or (LongWord(xfrac) shr FRACBITS) and 511;
{$ENDIF}
{$ENDIF}
{$ENDIF}

      // Lookup pixel from flat texture tile,
      //  transformed using light of ds_lightlevel
      c := ds_source32[spot];
{$IFDEF INVERSECOLORMAPS}
        r1 := c;
        g1 := c shr 8;
        b1 := c shr 16;
        PLongWordArray(destl)[3] := precal32_ic[r1 + g1 + b1];
{$ELSE}
{$IFDEF TRANSPARENTFLAT}
      PLongWordArray(destl)[3] := R_ColorMidAverage(c, destl^);
{$ELSE}
      PLongWordArray(destl)[3] := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
{$ENDIF}
{$ENDIF}
      // Next step in u,v.
      xfrac := xfrac + xstep;
      yfrac := yfrac + ystep;


      inc(destl, 4);
