  r1 := c;
  g1 := c shr 8;
  b1 := c shr 16;
{$IFDEF WHITE}
  r := (r1 * lfactor) shr FRACBITS;
  if r > 255 then
    r := 255;
  g := (g1 * lfactor) shr FRACBITS;
  if g > 255 then
    g := 255;
  b := (b1 * lfactor) shr FRACBITS;
  if b > 255 then
    b := 255;
{$ENDIF}
{$IFDEF RED}
  r := r1;
  g := g1;
  b := (b1 * lfactor) shr FRACBITS;
  if b > 255 then
    b := 255;
{$ENDIF}
{$IFDEF GREEN}
  r := r1;
  g := (g1 * lfactor) shr FRACBITS;
  if g > 255 then
    g := 255;
  b := b1;
{$ENDIF}
{$IFDEF BLUE}
  r := (r1 * lfactor) shr FRACBITS;
  if r > 255 then
    r := 255;
  g := g1;
  b := b1;
{$ENDIF}
{$IFDEF YELLOW}
  r := r1;
  g := (g1 * lfactor) shr FRACBITS;
  if g > 255 then
    g := 255;
  b := (b1 * lfactor) shr FRACBITS;
  if b > 255 then
    b := 255;
{$ENDIF}

