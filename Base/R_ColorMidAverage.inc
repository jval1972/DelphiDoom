  r1 := c3;
  g1 := c3 shr 8;
  b1 := c3 shr 16;
  r2 := c4;
  g2 := c4 shr 8;
  b2 := c4 shr 16;
  r := (r1 + r2) shr 1;
  g := (g1 + g2) shr 1;
  b := (b1 + b2) shr 1;

  destl^ := r + g shl 8 + b shl 16;

