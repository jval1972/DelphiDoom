      y := f3d_ylookup[i];
      idx := viewwindowx + (y + viewwindowy) * SCREENWIDTH + start;
      src := @screens[SCN_FG][idx];
      memcpy(buffer, src, range);
      idx := viewwindowx + (i + viewwindowy) * SCREENWIDTH + start;
      dest := @screens[SCN_FG][idx];
      fracstep := f3d.fracstep[y];
      frac := f3d.left[y] * FRACUNIT + start * (fracstep - FRACUNIT);
      if frac < 0 then
        frac := 0;
      limit := @PByteArray(dest)[range];
      limit2 := PByte(Integer(limit) - 16);

      while integer(dest) < integer(limit2) do
      begin
        fb.byte1 := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        fb.byte2 := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        fb.byte3 := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        fb.byte4 := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        PLongWord(dest)^ := PLongWord(@fb)^;
        Inc(dest, 4);

        fb.byte1 := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        fb.byte2 := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        fb.byte3 := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        fb.byte4 := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        PLongWord(dest)^ := PLongWord(@fb)^;
        Inc(dest, 4);

        fb.byte1 := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        fb.byte2 := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        fb.byte3 := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        fb.byte4 := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        PLongWord(dest)^ := PLongWord(@fb)^;
        Inc(dest, 4);

        fb.byte1 := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        fb.byte2 := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        fb.byte3 := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        fb.byte4 := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        PLongWord(dest)^ := PLongWord(@fb)^;
        Inc(dest, 4);
      end;

      while integer(dest) < integer(limit) do
      begin
        dest^ := buffer[frac div FRACUNIT];
        frac := frac + fracstep;
        Inc(dest);
      end;
