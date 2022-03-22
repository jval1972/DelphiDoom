  // Color averaging
  r1 := c1;
  g1 := c1 shr 8;
  b1 := c1 shr 16;
  r2 := c2;
  g2 := c2 shr 8;
  b2 := c2 shr 16;

  factor1 := FRACUNIT - 1 - cfrac2;
  r := ((r2 * cfrac2) + (r1 * factor1)) shr FRACBITS;
  g := ((g2 * cfrac2) + (g1 * factor1)) shr FRACBITS;
  b := ((b2 * cfrac2) + (b1 * factor1)) and $FF0000;

