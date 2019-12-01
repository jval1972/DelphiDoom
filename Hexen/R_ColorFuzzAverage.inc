  r1 := c3;
  g1 := c3 shr 8;
  b1 := c3 shr 16;
  r2 := c4;
  g2 := c4 shr 8;
  b2 := c4 shr 16;
  r := LongWord(r1 + r2 * 3) shr 2;
  g := LongWord(g1 + g2 * 3) shr 2;
  b := LongWord(b1 + b2 * 3) shr 2;

  destl^ := r + g shl 8 + b shl 16;

