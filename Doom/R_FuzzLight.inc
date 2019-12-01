  r1 := c;
  g1 := c shr 8;
  b1 := c shr 16;
  r := r1 shr 3 * 7;
  g := g1 shr 3 * 7;
  b := b1 shr 3 * 7;

  destl^ := r + g shl 8 + b shl 16;

