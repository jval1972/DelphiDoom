  {$IFDEF FOG}
{  r1 := c;
  g1 := c shr 8;
  b1 := c shr 16;
  r := (fog_color + (r1 * lfactor)) shr FRACBITS;
  g := (fog_color + (g1 * lfactor)) shr FRACBITS;
  b := (fog_color + (b1 * lfactor)) and $FF0000;
  destl^ := r + g shl 8 + b;}
  destl^ := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
  {$ELSE}
  destl^ := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
{  r := (r1 * lfactor) shr FRACBITS;
  g := (g1 * lfactor) shr FRACBITS;
  b := (b1 * lfactor) and $FF0000;}
  {$ENDIF}



