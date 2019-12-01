  r1 := c;
  g1 := c shr 8;
  b1 := c shr 16;
  {$IFDEF FOG}
  r := (fog_color + (r1 * lfactor)) shr FRACBITS;
  g := (fog_color + (g1 * lfactor)) shr FRACBITS;
  b := (fog_color + (b1 * lfactor)) and $FF0000;
  {$ELSE}
  r := (r1 * lfactor) shr FRACBITS;
  g := (g1 * lfactor) shr FRACBITS;
  b := (b1 * lfactor) and $FF0000;
  {$ENDIF}

  destl^ := r + g shl 8 + b;


