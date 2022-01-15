procedure R_DrawColorColumnHi_bc5(const count: integer);
var
  destl: PLongWord;
  ldest: LongWord;
  c: LongWord;
  cnt: integer;
  swidth: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
begin
  {$IFDEF DOOM_OR_STRIFE}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;
 
  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);
  swidth := SCREENWIDTH32PITCH - 4 * SizeOf(LongWord);
 
  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
    c := pal[dc_color];
    ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end
  else
  begin
    c := pal[dc_color];
    r1 := c;
    g1 := c shr 8;
    b1 := c shr 16;
    ldest := precal32_ic[r1 + g1 + b1];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end;
end;
 
procedure R_DrawColorColumnHi_bc6(const count: integer);
var
  destl: PLongWord;
  ldest: LongWord;
  c: LongWord;
  cnt: integer;
  swidth: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
begin
  {$IFDEF DOOM_OR_STRIFE}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;
 
  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);
  swidth := SCREENWIDTH32PITCH - 5 * SizeOf(LongWord);
 
  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
    c := pal[dc_color];
    ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end
  else
  begin
    c := pal[dc_color];
    r1 := c;
    g1 := c shr 8;
    b1 := c shr 16;
    ldest := precal32_ic[r1 + g1 + b1];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end;
end;
 
procedure R_DrawColorColumnHi_bc7(const count: integer);
var
  destl: PLongWord;
  ldest: LongWord;
  c: LongWord;
  cnt: integer;
  swidth: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
begin
  {$IFDEF DOOM_OR_STRIFE}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;
 
  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);
  swidth := SCREENWIDTH32PITCH - 6 * SizeOf(LongWord);
 
  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
    c := pal[dc_color];
    ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end
  else
  begin
    c := pal[dc_color];
    r1 := c;
    g1 := c shr 8;
    b1 := c shr 16;
    ldest := precal32_ic[r1 + g1 + b1];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end;
end;
 
procedure R_DrawColorColumnHi_bc8(const count: integer);
var
  destl: PLongWord;
  ldest: LongWord;
  c: LongWord;
  cnt: integer;
  swidth: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
begin
  {$IFDEF DOOM_OR_STRIFE}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;
 
  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);
  swidth := SCREENWIDTH32PITCH - 7 * SizeOf(LongWord);
 
  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
    c := pal[dc_color];
    ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end
  else
  begin
    c := pal[dc_color];
    r1 := c;
    g1 := c shr 8;
    b1 := c shr 16;
    ldest := precal32_ic[r1 + g1 + b1];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end;
end;
 
procedure R_DrawColorColumnHi_bc9(const count: integer);
var
  destl: PLongWord;
  ldest: LongWord;
  c: LongWord;
  cnt: integer;
  swidth: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
begin
  {$IFDEF DOOM_OR_STRIFE}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;
 
  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);
  swidth := SCREENWIDTH32PITCH - 8 * SizeOf(LongWord);
 
  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
    c := pal[dc_color];
    ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end
  else
  begin
    c := pal[dc_color];
    r1 := c;
    g1 := c shr 8;
    b1 := c shr 16;
    ldest := precal32_ic[r1 + g1 + b1];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end;
end;
 
procedure R_DrawColorColumnHi_bc10(const count: integer);
var
  destl: PLongWord;
  ldest: LongWord;
  c: LongWord;
  cnt: integer;
  swidth: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
begin
  {$IFDEF DOOM_OR_STRIFE}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;
 
  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);
  swidth := SCREENWIDTH32PITCH - 9 * SizeOf(LongWord);
 
  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
    c := pal[dc_color];
    ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end
  else
  begin
    c := pal[dc_color];
    r1 := c;
    g1 := c shr 8;
    b1 := c shr 16;
    ldest := precal32_ic[r1 + g1 + b1];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end;
end;
 
procedure R_DrawColorColumnHi_bc11(const count: integer);
var
  destl: PLongWord;
  ldest: LongWord;
  c: LongWord;
  cnt: integer;
  swidth: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
begin
  {$IFDEF DOOM_OR_STRIFE}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;
 
  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);
  swidth := SCREENWIDTH32PITCH - 10 * SizeOf(LongWord);
 
  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
    c := pal[dc_color];
    ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end
  else
  begin
    c := pal[dc_color];
    r1 := c;
    g1 := c shr 8;
    b1 := c shr 16;
    ldest := precal32_ic[r1 + g1 + b1];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end;
end;
 
procedure R_DrawColorColumnHi_bc12(const count: integer);
var
  destl: PLongWord;
  ldest: LongWord;
  c: LongWord;
  cnt: integer;
  swidth: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
begin
  {$IFDEF DOOM_OR_STRIFE}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;
 
  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);
  swidth := SCREENWIDTH32PITCH - 11 * SizeOf(LongWord);
 
  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
    c := pal[dc_color];
    ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end
  else
  begin
    c := pal[dc_color];
    r1 := c;
    g1 := c shr 8;
    b1 := c shr 16;
    ldest := precal32_ic[r1 + g1 + b1];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end;
end;
 
procedure R_DrawColorColumnHi_bc13(const count: integer);
var
  destl: PLongWord;
  ldest: LongWord;
  c: LongWord;
  cnt: integer;
  swidth: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
begin
  {$IFDEF DOOM_OR_STRIFE}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;
 
  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);
  swidth := SCREENWIDTH32PITCH - 12 * SizeOf(LongWord);
 
  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
    c := pal[dc_color];
    ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end
  else
  begin
    c := pal[dc_color];
    r1 := c;
    g1 := c shr 8;
    b1 := c shr 16;
    ldest := precal32_ic[r1 + g1 + b1];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end;
end;
 
procedure R_DrawColorColumnHi_bc14(const count: integer);
var
  destl: PLongWord;
  ldest: LongWord;
  c: LongWord;
  cnt: integer;
  swidth: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
begin
  {$IFDEF DOOM_OR_STRIFE}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;
 
  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);
  swidth := SCREENWIDTH32PITCH - 13 * SizeOf(LongWord);
 
  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
    c := pal[dc_color];
    ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end
  else
  begin
    c := pal[dc_color];
    r1 := c;
    g1 := c shr 8;
    b1 := c shr 16;
    ldest := precal32_ic[r1 + g1 + b1];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end;
end;
 
procedure R_DrawColorColumnHi_bc15(const count: integer);
var
  destl: PLongWord;
  ldest: LongWord;
  c: LongWord;
  cnt: integer;
  swidth: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
begin
  {$IFDEF DOOM_OR_STRIFE}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;
 
  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);
  swidth := SCREENWIDTH32PITCH - 14 * SizeOf(LongWord);
 
  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
    c := pal[dc_color];
    ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end
  else
  begin
    c := pal[dc_color];
    r1 := c;
    g1 := c shr 8;
    b1 := c shr 16;
    ldest := precal32_ic[r1 + g1 + b1];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end;
end;
 
procedure R_DrawColorColumnHi_bc16(const count: integer);
var
  destl: PLongWord;
  ldest: LongWord;
  c: LongWord;
  cnt: integer;
  swidth: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
begin
  {$IFDEF DOOM_OR_STRIFE}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;
 
  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);
  swidth := SCREENWIDTH32PITCH - 15 * SizeOf(LongWord);
 
  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
    c := pal[dc_color];
    ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end
  else
  begin
    c := pal[dc_color];
    r1 := c;
    g1 := c shr 8;
    b1 := c shr 16;
    ldest := precal32_ic[r1 + g1 + b1];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end;
end;
 
procedure R_DrawColorColumnHi_bc17(const count: integer);
var
  destl: PLongWord;
  ldest: LongWord;
  c: LongWord;
  cnt: integer;
  swidth: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
begin
  {$IFDEF DOOM_OR_STRIFE}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;
 
  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);
  swidth := SCREENWIDTH32PITCH - 16 * SizeOf(LongWord);
 
  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
    c := pal[dc_color];
    ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end
  else
  begin
    c := pal[dc_color];
    r1 := c;
    g1 := c shr 8;
    b1 := c shr 16;
    ldest := precal32_ic[r1 + g1 + b1];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end;
end;
 
procedure R_DrawColorColumnHi_bc18(const count: integer);
var
  destl: PLongWord;
  ldest: LongWord;
  c: LongWord;
  cnt: integer;
  swidth: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
begin
  {$IFDEF DOOM_OR_STRIFE}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;
 
  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);
  swidth := SCREENWIDTH32PITCH - 17 * SizeOf(LongWord);
 
  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
    c := pal[dc_color];
    ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end
  else
  begin
    c := pal[dc_color];
    r1 := c;
    g1 := c shr 8;
    b1 := c shr 16;
    ldest := precal32_ic[r1 + g1 + b1];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end;
end;
 
procedure R_DrawColorColumnHi_bc19(const count: integer);
var
  destl: PLongWord;
  ldest: LongWord;
  c: LongWord;
  cnt: integer;
  swidth: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
begin
  {$IFDEF DOOM_OR_STRIFE}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;
 
  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);
  swidth := SCREENWIDTH32PITCH - 18 * SizeOf(LongWord);
 
  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
    c := pal[dc_color];
    ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end
  else
  begin
    c := pal[dc_color];
    r1 := c;
    g1 := c shr 8;
    b1 := c shr 16;
    ldest := precal32_ic[r1 + g1 + b1];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end;
end;
 
procedure R_DrawColorColumnHi_bc20(const count: integer);
var
  destl: PLongWord;
  ldest: LongWord;
  c: LongWord;
  cnt: integer;
  swidth: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
begin
  {$IFDEF DOOM_OR_STRIFE}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;
 
  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);
  swidth := SCREENWIDTH32PITCH - 19 * SizeOf(LongWord);
 
  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
    c := pal[dc_color];
    ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end
  else
  begin
    c := pal[dc_color];
    r1 := c;
    g1 := c shr 8;
    b1 := c shr 16;
    ldest := precal32_ic[r1 + g1 + b1];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end;
end;
 
procedure R_DrawColorColumnHi_bc21(const count: integer);
var
  destl: PLongWord;
  ldest: LongWord;
  c: LongWord;
  cnt: integer;
  swidth: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
begin
  {$IFDEF DOOM_OR_STRIFE}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;
 
  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);
  swidth := SCREENWIDTH32PITCH - 20 * SizeOf(LongWord);
 
  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
    c := pal[dc_color];
    ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end
  else
  begin
    c := pal[dc_color];
    r1 := c;
    g1 := c shr 8;
    b1 := c shr 16;
    ldest := precal32_ic[r1 + g1 + b1];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end;
end;
 
procedure R_DrawColorColumnHi_bc22(const count: integer);
var
  destl: PLongWord;
  ldest: LongWord;
  c: LongWord;
  cnt: integer;
  swidth: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
begin
  {$IFDEF DOOM_OR_STRIFE}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;
 
  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);
  swidth := SCREENWIDTH32PITCH - 21 * SizeOf(LongWord);
 
  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
    c := pal[dc_color];
    ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end
  else
  begin
    c := pal[dc_color];
    r1 := c;
    g1 := c shr 8;
    b1 := c shr 16;
    ldest := precal32_ic[r1 + g1 + b1];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end;
end;
 
procedure R_DrawColorColumnHi_bc23(const count: integer);
var
  destl: PLongWord;
  ldest: LongWord;
  c: LongWord;
  cnt: integer;
  swidth: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
begin
  {$IFDEF DOOM_OR_STRIFE}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;
 
  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);
  swidth := SCREENWIDTH32PITCH - 22 * SizeOf(LongWord);
 
  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
    c := pal[dc_color];
    ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end
  else
  begin
    c := pal[dc_color];
    r1 := c;
    g1 := c shr 8;
    b1 := c shr 16;
    ldest := precal32_ic[r1 + g1 + b1];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end;
end;
 
procedure R_DrawColorColumnHi_bc24(const count: integer);
var
  destl: PLongWord;
  ldest: LongWord;
  c: LongWord;
  cnt: integer;
  swidth: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
begin
  {$IFDEF DOOM_OR_STRIFE}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;
 
  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);
  swidth := SCREENWIDTH32PITCH - 23 * SizeOf(LongWord);
 
  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
    c := pal[dc_color];
    ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end
  else
  begin
    c := pal[dc_color];
    r1 := c;
    g1 := c shr 8;
    b1 := c shr 16;
    ldest := precal32_ic[r1 + g1 + b1];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end;
end;
 
procedure R_DrawColorColumnHi_bc25(const count: integer);
var
  destl: PLongWord;
  ldest: LongWord;
  c: LongWord;
  cnt: integer;
  swidth: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
begin
  {$IFDEF DOOM_OR_STRIFE}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;
 
  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);
  swidth := SCREENWIDTH32PITCH - 24 * SizeOf(LongWord);
 
  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
    c := pal[dc_color];
    ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end
  else
  begin
    c := pal[dc_color];
    r1 := c;
    g1 := c shr 8;
    b1 := c shr 16;
    ldest := precal32_ic[r1 + g1 + b1];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end;
end;
 
procedure R_DrawColorColumnHi_bc26(const count: integer);
var
  destl: PLongWord;
  ldest: LongWord;
  c: LongWord;
  cnt: integer;
  swidth: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
begin
  {$IFDEF DOOM_OR_STRIFE}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;
 
  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);
  swidth := SCREENWIDTH32PITCH - 25 * SizeOf(LongWord);
 
  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
    c := pal[dc_color];
    ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end
  else
  begin
    c := pal[dc_color];
    r1 := c;
    g1 := c shr 8;
    b1 := c shr 16;
    ldest := precal32_ic[r1 + g1 + b1];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end;
end;
 
procedure R_DrawColorColumnHi_bc27(const count: integer);
var
  destl: PLongWord;
  ldest: LongWord;
  c: LongWord;
  cnt: integer;
  swidth: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
begin
  {$IFDEF DOOM_OR_STRIFE}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;
 
  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);
  swidth := SCREENWIDTH32PITCH - 26 * SizeOf(LongWord);
 
  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
    c := pal[dc_color];
    ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end
  else
  begin
    c := pal[dc_color];
    r1 := c;
    g1 := c shr 8;
    b1 := c shr 16;
    ldest := precal32_ic[r1 + g1 + b1];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end;
end;
 
procedure R_DrawColorColumnHi_bc28(const count: integer);
var
  destl: PLongWord;
  ldest: LongWord;
  c: LongWord;
  cnt: integer;
  swidth: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
begin
  {$IFDEF DOOM_OR_STRIFE}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;
 
  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);
  swidth := SCREENWIDTH32PITCH - 27 * SizeOf(LongWord);
 
  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
    c := pal[dc_color];
    ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end
  else
  begin
    c := pal[dc_color];
    r1 := c;
    g1 := c shr 8;
    b1 := c shr 16;
    ldest := precal32_ic[r1 + g1 + b1];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end;
end;
 
procedure R_DrawColorColumnHi_bc29(const count: integer);
var
  destl: PLongWord;
  ldest: LongWord;
  c: LongWord;
  cnt: integer;
  swidth: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
begin
  {$IFDEF DOOM_OR_STRIFE}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;
 
  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);
  swidth := SCREENWIDTH32PITCH - 28 * SizeOf(LongWord);
 
  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
    c := pal[dc_color];
    ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end
  else
  begin
    c := pal[dc_color];
    r1 := c;
    g1 := c shr 8;
    b1 := c shr 16;
    ldest := precal32_ic[r1 + g1 + b1];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end;
end;
 
procedure R_DrawColorColumnHi_bc30(const count: integer);
var
  destl: PLongWord;
  ldest: LongWord;
  c: LongWord;
  cnt: integer;
  swidth: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
begin
  {$IFDEF DOOM_OR_STRIFE}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;
 
  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);
  swidth := SCREENWIDTH32PITCH - 29 * SizeOf(LongWord);
 
  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
    c := pal[dc_color];
    ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end
  else
  begin
    c := pal[dc_color];
    r1 := c;
    g1 := c shr 8;
    b1 := c shr 16;
    ldest := precal32_ic[r1 + g1 + b1];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end;
end;
 
procedure R_DrawColorColumnHi_bc31(const count: integer);
var
  destl: PLongWord;
  ldest: LongWord;
  c: LongWord;
  cnt: integer;
  swidth: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
begin
  {$IFDEF DOOM_OR_STRIFE}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;
 
  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);
  swidth := SCREENWIDTH32PITCH - 30 * SizeOf(LongWord);
 
  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
    c := pal[dc_color];
    ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end
  else
  begin
    c := pal[dc_color];
    r1 := c;
    g1 := c shr 8;
    b1 := c shr 16;
    ldest := precal32_ic[r1 + g1 + b1];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end;
end;
 
procedure R_DrawColorColumnHi_bc32(const count: integer);
var
  destl: PLongWord;
  ldest: LongWord;
  c: LongWord;
  cnt: integer;
  swidth: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
begin
  {$IFDEF DOOM_OR_STRIFE}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;
 
  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);
  swidth := SCREENWIDTH32PITCH - 31 * SizeOf(LongWord);
 
  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
    c := pal[dc_color];
    ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end
  else
  begin
    c := pal[dc_color];
    r1 := c;
    g1 := c shr 8;
    b1 := c shr 16;
    ldest := precal32_ic[r1 + g1 + b1];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end;
end;
 
procedure R_DrawColorColumnHi_bc33(const count: integer);
var
  destl: PLongWord;
  ldest: LongWord;
  c: LongWord;
  cnt: integer;
  swidth: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
begin
  {$IFDEF DOOM_OR_STRIFE}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;
 
  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);
  swidth := SCREENWIDTH32PITCH - 32 * SizeOf(LongWord);
 
  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
    c := pal[dc_color];
    ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end
  else
  begin
    c := pal[dc_color];
    r1 := c;
    g1 := c shr 8;
    b1 := c shr 16;
    ldest := precal32_ic[r1 + g1 + b1];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end;
end;
 
procedure R_DrawColorColumnHi_bc34(const count: integer);
var
  destl: PLongWord;
  ldest: LongWord;
  c: LongWord;
  cnt: integer;
  swidth: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
begin
  {$IFDEF DOOM_OR_STRIFE}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;
 
  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);
  swidth := SCREENWIDTH32PITCH - 33 * SizeOf(LongWord);
 
  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
    c := pal[dc_color];
    ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end
  else
  begin
    c := pal[dc_color];
    r1 := c;
    g1 := c shr 8;
    b1 := c shr 16;
    ldest := precal32_ic[r1 + g1 + b1];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end;
end;
 
procedure R_DrawColorColumnHi_bc35(const count: integer);
var
  destl: PLongWord;
  ldest: LongWord;
  c: LongWord;
  cnt: integer;
  swidth: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
begin
  {$IFDEF DOOM_OR_STRIFE}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;
 
  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);
  swidth := SCREENWIDTH32PITCH - 34 * SizeOf(LongWord);
 
  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
    c := pal[dc_color];
    ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end
  else
  begin
    c := pal[dc_color];
    r1 := c;
    g1 := c shr 8;
    b1 := c shr 16;
    ldest := precal32_ic[r1 + g1 + b1];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end;
end;
 
procedure R_DrawColorColumnHi_bc36(const count: integer);
var
  destl: PLongWord;
  ldest: LongWord;
  c: LongWord;
  cnt: integer;
  swidth: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
begin
  {$IFDEF DOOM_OR_STRIFE}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;
 
  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);
  swidth := SCREENWIDTH32PITCH - 35 * SizeOf(LongWord);
 
  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
    c := pal[dc_color];
    ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end
  else
  begin
    c := pal[dc_color];
    r1 := c;
    g1 := c shr 8;
    b1 := c shr 16;
    ldest := precal32_ic[r1 + g1 + b1];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end;
end;
 
procedure R_DrawColorColumnHi_bc37(const count: integer);
var
  destl: PLongWord;
  ldest: LongWord;
  c: LongWord;
  cnt: integer;
  swidth: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
begin
  {$IFDEF DOOM_OR_STRIFE}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;
 
  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);
  swidth := SCREENWIDTH32PITCH - 36 * SizeOf(LongWord);
 
  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
    c := pal[dc_color];
    ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end
  else
  begin
    c := pal[dc_color];
    r1 := c;
    g1 := c shr 8;
    b1 := c shr 16;
    ldest := precal32_ic[r1 + g1 + b1];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end;
end;
 
procedure R_DrawColorColumnHi_bc38(const count: integer);
var
  destl: PLongWord;
  ldest: LongWord;
  c: LongWord;
  cnt: integer;
  swidth: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
begin
  {$IFDEF DOOM_OR_STRIFE}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;
 
  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);
  swidth := SCREENWIDTH32PITCH - 37 * SizeOf(LongWord);
 
  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
    c := pal[dc_color];
    ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end
  else
  begin
    c := pal[dc_color];
    r1 := c;
    g1 := c shr 8;
    b1 := c shr 16;
    ldest := precal32_ic[r1 + g1 + b1];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end;
end;
 
procedure R_DrawColorColumnHi_bc39(const count: integer);
var
  destl: PLongWord;
  ldest: LongWord;
  c: LongWord;
  cnt: integer;
  swidth: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
begin
  {$IFDEF DOOM_OR_STRIFE}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;
 
  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);
  swidth := SCREENWIDTH32PITCH - 38 * SizeOf(LongWord);
 
  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
    c := pal[dc_color];
    ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end
  else
  begin
    c := pal[dc_color];
    r1 := c;
    g1 := c shr 8;
    b1 := c shr 16;
    ldest := precal32_ic[r1 + g1 + b1];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end;
end;
 
procedure R_DrawColorColumnHi_bc40(const count: integer);
var
  destl: PLongWord;
  ldest: LongWord;
  c: LongWord;
  cnt: integer;
  swidth: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
begin
  {$IFDEF DOOM_OR_STRIFE}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;
 
  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);
  swidth := SCREENWIDTH32PITCH - 39 * SizeOf(LongWord);
 
  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
    c := pal[dc_color];
    ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end
  else
  begin
    c := pal[dc_color];
    r1 := c;
    g1 := c shr 8;
    b1 := c shr 16;
    ldest := precal32_ic[r1 + g1 + b1];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end;
end;
 
procedure R_DrawColorColumnHi_bc41(const count: integer);
var
  destl: PLongWord;
  ldest: LongWord;
  c: LongWord;
  cnt: integer;
  swidth: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
begin
  {$IFDEF DOOM_OR_STRIFE}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;
 
  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);
  swidth := SCREENWIDTH32PITCH - 40 * SizeOf(LongWord);
 
  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
    c := pal[dc_color];
    ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end
  else
  begin
    c := pal[dc_color];
    r1 := c;
    g1 := c shr 8;
    b1 := c shr 16;
    ldest := precal32_ic[r1 + g1 + b1];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end;
end;
 
procedure R_DrawColorColumnHi_bc42(const count: integer);
var
  destl: PLongWord;
  ldest: LongWord;
  c: LongWord;
  cnt: integer;
  swidth: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
begin
  {$IFDEF DOOM_OR_STRIFE}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;
 
  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);
  swidth := SCREENWIDTH32PITCH - 41 * SizeOf(LongWord);
 
  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
    c := pal[dc_color];
    ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end
  else
  begin
    c := pal[dc_color];
    r1 := c;
    g1 := c shr 8;
    b1 := c shr 16;
    ldest := precal32_ic[r1 + g1 + b1];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end;
end;
 
procedure R_DrawColorColumnHi_bc43(const count: integer);
var
  destl: PLongWord;
  ldest: LongWord;
  c: LongWord;
  cnt: integer;
  swidth: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
begin
  {$IFDEF DOOM_OR_STRIFE}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;
 
  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);
  swidth := SCREENWIDTH32PITCH - 42 * SizeOf(LongWord);
 
  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
    c := pal[dc_color];
    ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end
  else
  begin
    c := pal[dc_color];
    r1 := c;
    g1 := c shr 8;
    b1 := c shr 16;
    ldest := precal32_ic[r1 + g1 + b1];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end;
end;
 
procedure R_DrawColorColumnHi_bc44(const count: integer);
var
  destl: PLongWord;
  ldest: LongWord;
  c: LongWord;
  cnt: integer;
  swidth: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
begin
  {$IFDEF DOOM_OR_STRIFE}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;
 
  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);
  swidth := SCREENWIDTH32PITCH - 43 * SizeOf(LongWord);
 
  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
    c := pal[dc_color];
    ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end
  else
  begin
    c := pal[dc_color];
    r1 := c;
    g1 := c shr 8;
    b1 := c shr 16;
    ldest := precal32_ic[r1 + g1 + b1];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end;
end;
 
procedure R_DrawColorColumnHi_bc45(const count: integer);
var
  destl: PLongWord;
  ldest: LongWord;
  c: LongWord;
  cnt: integer;
  swidth: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
begin
  {$IFDEF DOOM_OR_STRIFE}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;
 
  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);
  swidth := SCREENWIDTH32PITCH - 44 * SizeOf(LongWord);
 
  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
    c := pal[dc_color];
    ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end
  else
  begin
    c := pal[dc_color];
    r1 := c;
    g1 := c shr 8;
    b1 := c shr 16;
    ldest := precal32_ic[r1 + g1 + b1];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end;
end;
 
procedure R_DrawColorColumnHi_bc46(const count: integer);
var
  destl: PLongWord;
  ldest: LongWord;
  c: LongWord;
  cnt: integer;
  swidth: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
begin
  {$IFDEF DOOM_OR_STRIFE}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;
 
  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);
  swidth := SCREENWIDTH32PITCH - 45 * SizeOf(LongWord);
 
  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
    c := pal[dc_color];
    ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end
  else
  begin
    c := pal[dc_color];
    r1 := c;
    g1 := c shr 8;
    b1 := c shr 16;
    ldest := precal32_ic[r1 + g1 + b1];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end;
end;
 
procedure R_DrawColorColumnHi_bc47(const count: integer);
var
  destl: PLongWord;
  ldest: LongWord;
  c: LongWord;
  cnt: integer;
  swidth: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
begin
  {$IFDEF DOOM_OR_STRIFE}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;
 
  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);
  swidth := SCREENWIDTH32PITCH - 46 * SizeOf(LongWord);
 
  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
    c := pal[dc_color];
    ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end
  else
  begin
    c := pal[dc_color];
    r1 := c;
    g1 := c shr 8;
    b1 := c shr 16;
    ldest := precal32_ic[r1 + g1 + b1];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end;
end;
 
procedure R_DrawColorColumnHi_bc48(const count: integer);
var
  destl: PLongWord;
  ldest: LongWord;
  c: LongWord;
  cnt: integer;
  swidth: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
begin
  {$IFDEF DOOM_OR_STRIFE}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;
 
  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);
  swidth := SCREENWIDTH32PITCH - 47 * SizeOf(LongWord);
 
  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
    c := pal[dc_color];
    ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end
  else
  begin
    c := pal[dc_color];
    r1 := c;
    g1 := c shr 8;
    b1 := c shr 16;
    ldest := precal32_ic[r1 + g1 + b1];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end;
end;
 
procedure R_DrawColorColumnHi_bc49(const count: integer);
var
  destl: PLongWord;
  ldest: LongWord;
  c: LongWord;
  cnt: integer;
  swidth: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
begin
  {$IFDEF DOOM_OR_STRIFE}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;
 
  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);
  swidth := SCREENWIDTH32PITCH - 48 * SizeOf(LongWord);
 
  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
    c := pal[dc_color];
    ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end
  else
  begin
    c := pal[dc_color];
    r1 := c;
    g1 := c shr 8;
    b1 := c shr 16;
    ldest := precal32_ic[r1 + g1 + b1];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end;
end;
 
procedure R_DrawColorColumnHi_bc50(const count: integer);
var
  destl: PLongWord;
  ldest: LongWord;
  c: LongWord;
  cnt: integer;
  swidth: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
begin
  {$IFDEF DOOM_OR_STRIFE}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;
 
  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);
  swidth := SCREENWIDTH32PITCH - 49 * SizeOf(LongWord);
 
  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
    c := pal[dc_color];
    ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end
  else
  begin
    c := pal[dc_color];
    r1 := c;
    g1 := c shr 8;
    b1 := c shr 16;
    ldest := precal32_ic[r1 + g1 + b1];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end;
end;
 
procedure R_DrawColorColumnHi_bc51(const count: integer);
var
  destl: PLongWord;
  ldest: LongWord;
  c: LongWord;
  cnt: integer;
  swidth: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
begin
  {$IFDEF DOOM_OR_STRIFE}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;
 
  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);
  swidth := SCREENWIDTH32PITCH - 50 * SizeOf(LongWord);
 
  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
    c := pal[dc_color];
    ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end
  else
  begin
    c := pal[dc_color];
    r1 := c;
    g1 := c shr 8;
    b1 := c shr 16;
    ldest := precal32_ic[r1 + g1 + b1];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end;
end;
 
procedure R_DrawColorColumnHi_bc52(const count: integer);
var
  destl: PLongWord;
  ldest: LongWord;
  c: LongWord;
  cnt: integer;
  swidth: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
begin
  {$IFDEF DOOM_OR_STRIFE}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;
 
  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);
  swidth := SCREENWIDTH32PITCH - 51 * SizeOf(LongWord);
 
  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
    c := pal[dc_color];
    ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end
  else
  begin
    c := pal[dc_color];
    r1 := c;
    g1 := c shr 8;
    b1 := c shr 16;
    ldest := precal32_ic[r1 + g1 + b1];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end;
end;
 
procedure R_DrawColorColumnHi_bc53(const count: integer);
var
  destl: PLongWord;
  ldest: LongWord;
  c: LongWord;
  cnt: integer;
  swidth: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
begin
  {$IFDEF DOOM_OR_STRIFE}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;
 
  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);
  swidth := SCREENWIDTH32PITCH - 52 * SizeOf(LongWord);
 
  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
    c := pal[dc_color];
    ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end
  else
  begin
    c := pal[dc_color];
    r1 := c;
    g1 := c shr 8;
    b1 := c shr 16;
    ldest := precal32_ic[r1 + g1 + b1];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end;
end;
 
procedure R_DrawColorColumnHi_bc54(const count: integer);
var
  destl: PLongWord;
  ldest: LongWord;
  c: LongWord;
  cnt: integer;
  swidth: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
begin
  {$IFDEF DOOM_OR_STRIFE}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;
 
  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);
  swidth := SCREENWIDTH32PITCH - 53 * SizeOf(LongWord);
 
  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
    c := pal[dc_color];
    ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end
  else
  begin
    c := pal[dc_color];
    r1 := c;
    g1 := c shr 8;
    b1 := c shr 16;
    ldest := precal32_ic[r1 + g1 + b1];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end;
end;
 
procedure R_DrawColorColumnHi_bc55(const count: integer);
var
  destl: PLongWord;
  ldest: LongWord;
  c: LongWord;
  cnt: integer;
  swidth: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
begin
  {$IFDEF DOOM_OR_STRIFE}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;
 
  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);
  swidth := SCREENWIDTH32PITCH - 54 * SizeOf(LongWord);
 
  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
    c := pal[dc_color];
    ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end
  else
  begin
    c := pal[dc_color];
    r1 := c;
    g1 := c shr 8;
    b1 := c shr 16;
    ldest := precal32_ic[r1 + g1 + b1];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end;
end;
 
procedure R_DrawColorColumnHi_bc56(const count: integer);
var
  destl: PLongWord;
  ldest: LongWord;
  c: LongWord;
  cnt: integer;
  swidth: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
begin
  {$IFDEF DOOM_OR_STRIFE}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;
 
  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);
  swidth := SCREENWIDTH32PITCH - 55 * SizeOf(LongWord);
 
  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
    c := pal[dc_color];
    ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end
  else
  begin
    c := pal[dc_color];
    r1 := c;
    g1 := c shr 8;
    b1 := c shr 16;
    ldest := precal32_ic[r1 + g1 + b1];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end;
end;
 
procedure R_DrawColorColumnHi_bc57(const count: integer);
var
  destl: PLongWord;
  ldest: LongWord;
  c: LongWord;
  cnt: integer;
  swidth: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
begin
  {$IFDEF DOOM_OR_STRIFE}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;
 
  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);
  swidth := SCREENWIDTH32PITCH - 56 * SizeOf(LongWord);
 
  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
    c := pal[dc_color];
    ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end
  else
  begin
    c := pal[dc_color];
    r1 := c;
    g1 := c shr 8;
    b1 := c shr 16;
    ldest := precal32_ic[r1 + g1 + b1];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end;
end;
 
procedure R_DrawColorColumnHi_bc58(const count: integer);
var
  destl: PLongWord;
  ldest: LongWord;
  c: LongWord;
  cnt: integer;
  swidth: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
begin
  {$IFDEF DOOM_OR_STRIFE}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;
 
  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);
  swidth := SCREENWIDTH32PITCH - 57 * SizeOf(LongWord);
 
  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
    c := pal[dc_color];
    ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end
  else
  begin
    c := pal[dc_color];
    r1 := c;
    g1 := c shr 8;
    b1 := c shr 16;
    ldest := precal32_ic[r1 + g1 + b1];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end;
end;
 
procedure R_DrawColorColumnHi_bc59(const count: integer);
var
  destl: PLongWord;
  ldest: LongWord;
  c: LongWord;
  cnt: integer;
  swidth: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
begin
  {$IFDEF DOOM_OR_STRIFE}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;
 
  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);
  swidth := SCREENWIDTH32PITCH - 58 * SizeOf(LongWord);
 
  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
    c := pal[dc_color];
    ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end
  else
  begin
    c := pal[dc_color];
    r1 := c;
    g1 := c shr 8;
    b1 := c shr 16;
    ldest := precal32_ic[r1 + g1 + b1];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end;
end;
 
procedure R_DrawColorColumnHi_bc60(const count: integer);
var
  destl: PLongWord;
  ldest: LongWord;
  c: LongWord;
  cnt: integer;
  swidth: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
begin
  {$IFDEF DOOM_OR_STRIFE}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;
 
  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);
  swidth := SCREENWIDTH32PITCH - 59 * SizeOf(LongWord);
 
  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
    c := pal[dc_color];
    ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end
  else
  begin
    c := pal[dc_color];
    r1 := c;
    g1 := c shr 8;
    b1 := c shr 16;
    ldest := precal32_ic[r1 + g1 + b1];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end;
end;
 
procedure R_DrawColorColumnHi_bc61(const count: integer);
var
  destl: PLongWord;
  ldest: LongWord;
  c: LongWord;
  cnt: integer;
  swidth: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
begin
  {$IFDEF DOOM_OR_STRIFE}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;
 
  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);
  swidth := SCREENWIDTH32PITCH - 60 * SizeOf(LongWord);
 
  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
    c := pal[dc_color];
    ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end
  else
  begin
    c := pal[dc_color];
    r1 := c;
    g1 := c shr 8;
    b1 := c shr 16;
    ldest := precal32_ic[r1 + g1 + b1];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end;
end;
 
procedure R_DrawColorColumnHi_bc62(const count: integer);
var
  destl: PLongWord;
  ldest: LongWord;
  c: LongWord;
  cnt: integer;
  swidth: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
begin
  {$IFDEF DOOM_OR_STRIFE}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;
 
  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);
  swidth := SCREENWIDTH32PITCH - 61 * SizeOf(LongWord);
 
  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
    c := pal[dc_color];
    ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end
  else
  begin
    c := pal[dc_color];
    r1 := c;
    g1 := c shr 8;
    b1 := c shr 16;
    ldest := precal32_ic[r1 + g1 + b1];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end;
end;
 
procedure R_DrawColorColumnHi_bc63(const count: integer);
var
  destl: PLongWord;
  ldest: LongWord;
  c: LongWord;
  cnt: integer;
  swidth: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
begin
  {$IFDEF DOOM_OR_STRIFE}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;
 
  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);
  swidth := SCREENWIDTH32PITCH - 62 * SizeOf(LongWord);
 
  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
    c := pal[dc_color];
    ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end
  else
  begin
    c := pal[dc_color];
    r1 := c;
    g1 := c shr 8;
    b1 := c shr 16;
    ldest := precal32_ic[r1 + g1 + b1];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end;
end;
 
procedure R_DrawColorColumnHi_bc64(const count: integer);
var
  destl: PLongWord;
  ldest: LongWord;
  c: LongWord;
  cnt: integer;
  swidth: integer;
  lfactor: integer;
  r1, g1, b1: byte;
  bf_r: PIntegerArray;
  bf_g: PIntegerArray;
  bf_b: PIntegerArray;
  pal: PLongWordArray;
begin
  {$IFDEF DOOM_OR_STRIFE}
  if customcolormap <> nil then
    pal := @cvideopal
  else
  {$ENDIF}
    pal := @curpal;
 
  destl := @((ylookupl[dc_yl]^)[columnofs[dc_x]]);
  swidth := SCREENWIDTH32PITCH - 63 * SizeOf(LongWord);
 
  lfactor := dc_lightlevel;
  if lfactor >= 0 then
  begin
    R_GetPrecalc32Tables(lfactor, bf_r, bf_g, bf_b, dc_fog);  // JVAL: Mars fog sectors
    c := pal[dc_color];
    ldest := bf_r[c and $FF] + bf_g[(c shr 8) and $FF] + bf_b[(c shr 16) and $FF];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end
  else
  begin
    c := pal[dc_color];
    r1 := c;
    g1 := c shr 8;
    b1 := c shr 16;
    ldest := precal32_ic[r1 + g1 + b1];
 
    cnt := count;
    while cnt > 0 do
    begin
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      Inc(destl);
      destl^ := ldest;
      destl := PLongWord(integer(destl) + swidth);
      dec(cnt);
    end;
  end;
end;
 
