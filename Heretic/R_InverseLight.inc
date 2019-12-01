  r1 := c;
  g1 := c shr 8;
  b1 := c shr 16;
  c1 := 255 - (r1 + g1 + b1) div 3;

  destl^ := c1 + c1 shl 8 + c1 shl 16;

