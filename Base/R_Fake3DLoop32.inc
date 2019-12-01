      y := f3d_ylookup[i];
      idx := viewwindowx + (y + viewwindowy) * SCREENWIDTH + start;
      src := @screen32[idx];
      memcpy(buffer, src, bsize);
      idx := viewwindowx + (i + viewwindowy) * SCREENWIDTH + start;
      dest := @screen32[idx];
      fracstep := f3d.fracstep[y];
      frac := f3d.left[y] * FRACUNIT + start * (fracstep - FRACUNIT);
      if frac < 0 then
        frac := 0;
      limit := @PLongWordArray(dest)[range];
      limit2 := @PLongWordArray(dest)[range - 16];

      while integer(dest) < integer(limit2) do
      begin
        dest^ := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        Inc(dest);
        dest^ := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        Inc(dest);
        dest^ := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        Inc(dest);
        dest^ := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        Inc(dest);
        dest^ := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        Inc(dest);
        dest^ := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        Inc(dest);
        dest^ := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        Inc(dest);
        dest^ := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        Inc(dest);
        dest^ := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        Inc(dest);
        dest^ := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        Inc(dest);
        dest^ := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        Inc(dest);
        dest^ := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        Inc(dest);
        dest^ := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        Inc(dest);
        dest^ := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        Inc(dest);
        dest^ := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        Inc(dest);
        dest^ := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        Inc(dest);
      end;

      while integer(dest) < integer(limit) do
      begin
        dest^ := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        Inc(dest);
      end;
