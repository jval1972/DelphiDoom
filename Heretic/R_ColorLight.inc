  r1 := c;
  g1 := c shr 8;
  b1 := c shr 16;
  r := (r1 * lfactor) shr FRACBITS;
  g := (g1 * lfactor) shr FRACBITS;
  b := (b1 * lfactor) and $FF0000;

  destl^ := r + g shl 8 + b;


