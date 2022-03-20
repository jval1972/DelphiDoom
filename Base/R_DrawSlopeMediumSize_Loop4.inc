    // Current texture index in u,v.
{$IFDEF SIZE64x64}
{$IFDEF RIPPLE}
      spot := ((LongWord(yfrac + rpl[(LongWord(xfrac) shr 16) and 127]) shr 10) and (4032)) or
              ((LongWord(xfrac + rpl[(LongWord(yfrac) shr 16) and 127]) shr FRACBITS) and 63);
{$ELSE}
      spot := (LongWord(yfrac) shr 10) and (4032) or (LongWord(xfrac) shr FRACBITS) and 63;
{$ENDIF}
{$ENDIF}

{$IFDEF SIZE128x128}
{$IFDEF RIPPLE}
      spot := (LongWord(yfrac + rpl[(LongWord(xfrac) shr 16) and 127]) shr 9) and (16256) or
              (LongWord(xfrac + rpl[(LongWord(yfrac) shr 16) and 127]) shr FRACBITS) and 127;
{$ELSE}
      spot := (LongWord(yfrac) shr 9) and (16256) or (LongWord(xfrac) shr FRACBITS) and 127;
{$ENDIF}
{$ENDIF}

{$IFDEF SIZE256x256}
{$IFDEF RIPPLE}
      spot := (LongWord(yfrac + rpl[(LongWord(xfrac) shr 16) and 127]) shr 8) and (65280) or
              (LongWord(xfrac + rpl[(LongWord(yfrac) shr 16) and 127]) shr FRACBITS) and 255;
{$ELSE}
      spot := (LongWord(yfrac) shr 8) and (65280) or (LongWord(xfrac) shr FRACBITS) and 255;
{$ENDIF}
{$ENDIF}

{$IFDEF SIZE512x512}
{$IFDEF RIPPLE}
      spot := (LongWord(yfrac + rpl[(LongWord(xfrac) shr 16) and 127]) shr 7) and (261632) or
              (LongWord(xfrac + rpl[(LongWord(yfrac) shr 16) and 127]) shr FRACBITS) and 511;
{$ELSE}
      spot := (LongWord(yfrac) shr 7) and (261632) or (LongWord(xfrac) shr FRACBITS) and 511;
{$ENDIF}
{$ENDIF}

{$IFDEF SIZE1024x1024}
{$IFDEF RIPPLE}
      spot := (LongWord(yfrac + rpl[(LongWord(xfrac) shr 16) and 127]) shr 6) and (1047552) or
              (LongWord(xfrac + rpl[(LongWord(yfrac) shr 16) and 127]) shr FRACBITS) and 1023;
{$ELSE}
      spot := (LongWord(yfrac) shr 6) and (1047552) or (LongWord(xfrac) shr FRACBITS) and 1023;
{$ENDIF}
{$ENDIF}

{$IFDEF SIZE2048x2048}
{$IFDEF RIPPLE}
      spot := (LongWord(yfrac + rpl[(LongWord(xfrac) shr 16) and 127]) shr 5) and (4192256) or
              (LongWord(xfrac + rpl[(LongWord(yfrac) shr 16) and 127]) shr FRACBITS) and 2047;
{$ELSE}
      spot := (LongWord(yfrac) shr 5) and (4192256) or (LongWord(xfrac) shr FRACBITS) and 2047;
{$ENDIF}
{$ENDIF}

{$IFDEF SIZE4096x4096}
{$IFDEF RIPPLE}
      spot := (LongWord(yfrac + rpl[(LongWord(xfrac) shr 16) and 127]) shr 4) and (16773120) or
              (LongWord(xfrac + rpl[(LongWord(yfrac) shr 16) and 127]) shr FRACBITS) and 4095;
{$ELSE}
      spot := (LongWord(yfrac) shr 4) and (16773120) or (LongWord(xfrac) shr FRACBITS) and 4095;
{$ENDIF}
{$ENDIF}

    // Lookup pixel from flat texture tile,
    //  re-index using light/colormap.
    PByteArray(dest)[0] := ds_colormap[ds_source[spot]];

    // Next step in u,v.
    xfrac := xfrac + xstep;
    yfrac := yfrac + ystep;

    // Current texture index in u,v.
{$IFDEF SIZE64x64}
{$IFDEF RIPPLE}
      spot := ((LongWord(yfrac + rpl[(LongWord(xfrac) shr 16) and 127]) shr 10) and (4032)) or
              ((LongWord(xfrac + rpl[(LongWord(yfrac) shr 16) and 127]) shr FRACBITS) and 63);
{$ELSE}
      spot := (LongWord(yfrac) shr 10) and (4032) or (LongWord(xfrac) shr FRACBITS) and 63;
{$ENDIF}
{$ENDIF}

{$IFDEF SIZE128x128}
{$IFDEF RIPPLE}
      spot := (LongWord(yfrac + rpl[(LongWord(xfrac) shr 16) and 127]) shr 9) and (16256) or
              (LongWord(xfrac + rpl[(LongWord(yfrac) shr 16) and 127]) shr FRACBITS) and 127;
{$ELSE}
      spot := (LongWord(yfrac) shr 9) and (16256) or (LongWord(xfrac) shr FRACBITS) and 127;
{$ENDIF}
{$ENDIF}

{$IFDEF SIZE256x256}
{$IFDEF RIPPLE}
      spot := (LongWord(yfrac + rpl[(LongWord(xfrac) shr 16) and 127]) shr 8) and (65280) or
              (LongWord(xfrac + rpl[(LongWord(yfrac) shr 16) and 127]) shr FRACBITS) and 255;
{$ELSE}
      spot := (LongWord(yfrac) shr 8) and (65280) or (LongWord(xfrac) shr FRACBITS) and 255;
{$ENDIF}
{$ENDIF}

{$IFDEF SIZE512x512}
{$IFDEF RIPPLE}
      spot := (LongWord(yfrac + rpl[(LongWord(xfrac) shr 16) and 127]) shr 7) and (261632) or
              (LongWord(xfrac + rpl[(LongWord(yfrac) shr 16) and 127]) shr FRACBITS) and 511;
{$ELSE}
      spot := (LongWord(yfrac) shr 7) and (261632) or (LongWord(xfrac) shr FRACBITS) and 511;
{$ENDIF}
{$ENDIF}

{$IFDEF SIZE1024x1024}
{$IFDEF RIPPLE}
      spot := (LongWord(yfrac + rpl[(LongWord(xfrac) shr 16) and 127]) shr 6) and (1047552) or
              (LongWord(xfrac + rpl[(LongWord(yfrac) shr 16) and 127]) shr FRACBITS) and 1023;
{$ELSE}
      spot := (LongWord(yfrac) shr 6) and (1047552) or (LongWord(xfrac) shr FRACBITS) and 1023;
{$ENDIF}
{$ENDIF}

{$IFDEF SIZE2048x2048}
{$IFDEF RIPPLE}
      spot := (LongWord(yfrac + rpl[(LongWord(xfrac) shr 16) and 127]) shr 5) and (4192256) or
              (LongWord(xfrac + rpl[(LongWord(yfrac) shr 16) and 127]) shr FRACBITS) and 2047;
{$ELSE}
      spot := (LongWord(yfrac) shr 5) and (4192256) or (LongWord(xfrac) shr FRACBITS) and 2047;
{$ENDIF}
{$ENDIF}

{$IFDEF SIZE4096x4096}
{$IFDEF RIPPLE}
      spot := (LongWord(yfrac + rpl[(LongWord(xfrac) shr 16) and 127]) shr 4) and (16773120) or
              (LongWord(xfrac + rpl[(LongWord(yfrac) shr 16) and 127]) shr FRACBITS) and 4095;
{$ELSE}
      spot := (LongWord(yfrac) shr 4) and (16773120) or (LongWord(xfrac) shr FRACBITS) and 4095;
{$ENDIF}
{$ENDIF}

    // Lookup pixel from flat texture tile,
    //  re-index using light/colormap.
    PByteArray(dest)[1] := ds_colormap[ds_source[spot]];

    // Next step in u,v.
    xfrac := xfrac + xstep;
    yfrac := yfrac + ystep;

    // Current texture index in u,v.
{$IFDEF SIZE64x64}
{$IFDEF RIPPLE}
      spot := ((LongWord(yfrac + rpl[(LongWord(xfrac) shr 16) and 127]) shr 10) and (4032)) or
              ((LongWord(xfrac + rpl[(LongWord(yfrac) shr 16) and 127]) shr FRACBITS) and 63);
{$ELSE}
      spot := (LongWord(yfrac) shr 10) and (4032) or (LongWord(xfrac) shr FRACBITS) and 63;
{$ENDIF}
{$ENDIF}

{$IFDEF SIZE128x128}
{$IFDEF RIPPLE}
      spot := (LongWord(yfrac + rpl[(LongWord(xfrac) shr 16) and 127]) shr 9) and (16256) or
              (LongWord(xfrac + rpl[(LongWord(yfrac) shr 16) and 127]) shr FRACBITS) and 127;
{$ELSE}
      spot := (LongWord(yfrac) shr 9) and (16256) or (LongWord(xfrac) shr FRACBITS) and 127;
{$ENDIF}
{$ENDIF}

{$IFDEF SIZE256x256}
{$IFDEF RIPPLE}
      spot := (LongWord(yfrac + rpl[(LongWord(xfrac) shr 16) and 127]) shr 8) and (65280) or
              (LongWord(xfrac + rpl[(LongWord(yfrac) shr 16) and 127]) shr FRACBITS) and 255;
{$ELSE}
      spot := (LongWord(yfrac) shr 8) and (65280) or (LongWord(xfrac) shr FRACBITS) and 255;
{$ENDIF}
{$ENDIF}

{$IFDEF SIZE512x512}
{$IFDEF RIPPLE}
      spot := (LongWord(yfrac + rpl[(LongWord(xfrac) shr 16) and 127]) shr 7) and (261632) or
              (LongWord(xfrac + rpl[(LongWord(yfrac) shr 16) and 127]) shr FRACBITS) and 511;
{$ELSE}
      spot := (LongWord(yfrac) shr 7) and (261632) or (LongWord(xfrac) shr FRACBITS) and 511;
{$ENDIF}
{$ENDIF}

{$IFDEF SIZE1024x1024}
{$IFDEF RIPPLE}
      spot := (LongWord(yfrac + rpl[(LongWord(xfrac) shr 16) and 127]) shr 6) and (1047552) or
              (LongWord(xfrac + rpl[(LongWord(yfrac) shr 16) and 127]) shr FRACBITS) and 1023;
{$ELSE}
      spot := (LongWord(yfrac) shr 6) and (1047552) or (LongWord(xfrac) shr FRACBITS) and 1023;
{$ENDIF}
{$ENDIF}

{$IFDEF SIZE2048x2048}
{$IFDEF RIPPLE}
      spot := (LongWord(yfrac + rpl[(LongWord(xfrac) shr 16) and 127]) shr 5) and (4192256) or
              (LongWord(xfrac + rpl[(LongWord(yfrac) shr 16) and 127]) shr FRACBITS) and 2047;
{$ELSE}
      spot := (LongWord(yfrac) shr 5) and (4192256) or (LongWord(xfrac) shr FRACBITS) and 2047;
{$ENDIF}
{$ENDIF}

{$IFDEF SIZE4096x4096}
{$IFDEF RIPPLE}
      spot := (LongWord(yfrac + rpl[(LongWord(xfrac) shr 16) and 127]) shr 4) and (16773120) or
              (LongWord(xfrac + rpl[(LongWord(yfrac) shr 16) and 127]) shr FRACBITS) and 4095;
{$ELSE}
      spot := (LongWord(yfrac) shr 4) and (16773120) or (LongWord(xfrac) shr FRACBITS) and 4095;
{$ENDIF}
{$ENDIF}

    // Lookup pixel from flat texture tile,
    //  re-index using light/colormap.
    PByteArray(dest)[2] := ds_colormap[ds_source[spot]];

    // Next step in u,v.
    xfrac := xfrac + xstep;
    yfrac := yfrac + ystep;

    // Current texture index in u,v.
{$IFDEF SIZE64x64}
{$IFDEF RIPPLE}
      spot := ((LongWord(yfrac + rpl[(LongWord(xfrac) shr 16) and 127]) shr 10) and (4032)) or
              ((LongWord(xfrac + rpl[(LongWord(yfrac) shr 16) and 127]) shr FRACBITS) and 63);
{$ELSE}
      spot := (LongWord(yfrac) shr 10) and (4032) or (LongWord(xfrac) shr FRACBITS) and 63;
{$ENDIF}
{$ENDIF}

{$IFDEF SIZE128x128}
{$IFDEF RIPPLE}
      spot := (LongWord(yfrac + rpl[(LongWord(xfrac) shr 16) and 127]) shr 9) and (16256) or
              (LongWord(xfrac + rpl[(LongWord(yfrac) shr 16) and 127]) shr FRACBITS) and 127;
{$ELSE}
      spot := (LongWord(yfrac) shr 9) and (16256) or (LongWord(xfrac) shr FRACBITS) and 127;
{$ENDIF}
{$ENDIF}

{$IFDEF SIZE256x256}
{$IFDEF RIPPLE}
      spot := (LongWord(yfrac + rpl[(LongWord(xfrac) shr 16) and 127]) shr 8) and (65280) or
              (LongWord(xfrac + rpl[(LongWord(yfrac) shr 16) and 127]) shr FRACBITS) and 255;
{$ELSE}
      spot := (LongWord(yfrac) shr 8) and (65280) or (LongWord(xfrac) shr FRACBITS) and 255;
{$ENDIF}
{$ENDIF}

{$IFDEF SIZE512x512}
{$IFDEF RIPPLE}
      spot := (LongWord(yfrac + rpl[(LongWord(xfrac) shr 16) and 127]) shr 7) and (261632) or
              (LongWord(xfrac + rpl[(LongWord(yfrac) shr 16) and 127]) shr FRACBITS) and 511;
{$ELSE}
      spot := (LongWord(yfrac) shr 7) and (261632) or (LongWord(xfrac) shr FRACBITS) and 511;
{$ENDIF}
{$ENDIF}

{$IFDEF SIZE1024x1024}
{$IFDEF RIPPLE}
      spot := (LongWord(yfrac + rpl[(LongWord(xfrac) shr 16) and 127]) shr 6) and (1047552) or
              (LongWord(xfrac + rpl[(LongWord(yfrac) shr 16) and 127]) shr FRACBITS) and 1023;
{$ELSE}
      spot := (LongWord(yfrac) shr 6) and (1047552) or (LongWord(xfrac) shr FRACBITS) and 1023;
{$ENDIF}
{$ENDIF}

{$IFDEF SIZE2048x2048}
{$IFDEF RIPPLE}
      spot := (LongWord(yfrac + rpl[(LongWord(xfrac) shr 16) and 127]) shr 5) and (4192256) or
              (LongWord(xfrac + rpl[(LongWord(yfrac) shr 16) and 127]) shr FRACBITS) and 2047;
{$ELSE}
      spot := (LongWord(yfrac) shr 5) and (4192256) or (LongWord(xfrac) shr FRACBITS) and 2047;
{$ENDIF}
{$ENDIF}

{$IFDEF SIZE4096x4096}
{$IFDEF RIPPLE}
      spot := (LongWord(yfrac + rpl[(LongWord(xfrac) shr 16) and 127]) shr 4) and (16773120) or
              (LongWord(xfrac + rpl[(LongWord(yfrac) shr 16) and 127]) shr FRACBITS) and 4095;
{$ELSE}
      spot := (LongWord(yfrac) shr 4) and (16773120) or (LongWord(xfrac) shr FRACBITS) and 4095;
{$ENDIF}
{$ENDIF}

    // Lookup pixel from flat texture tile,
    //  re-index using light/colormap.
    PByteArray(dest)[3] := ds_colormap[ds_source[spot]];

    // Next step in u,v.
    xfrac := xfrac + xstep;
    yfrac := yfrac + ystep;


    inc(dest, 4);
