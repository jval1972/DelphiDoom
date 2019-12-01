var
  angle: angle_t;
  index: integer;
  yl: integer;
  yh: integer;
  mid: integer;
  texturecolumn: fixed_t;
  texturecolumnhi: smallint;
  top: integer;
  bottom: integer;
  pceilingclip: PSmallInt;
  pfloorclip: PSmallInt;
  rwx, rwstopx: integer;
begin
  texturecolumn := 0; // shut up compiler warning
  texturecolumnhi := 0;
  rwx := rw_x;
  rwstopx := rw_stopx;
  pceilingclip := @ceilingclip[rwx];
  pfloorclip := @floorclip[rwx];
  while rwx < rwstopx do
  begin
    // mark floor / ceiling areas
    yl := (topfrac + (HEIGHTUNIT - 1)) div HEIGHTUNIT;

    // no space above wall?
    if yl <= pceilingclip^ then
      yl := pceilingclip^ + 1;

    if markceiling then
    begin
      top := pceilingclip^ + 1;
      bottom := yl - 1;

      if bottom >= pfloorclip^ then
        bottom := pfloorclip^ - 1;

      if top <= bottom then
      begin
        ceilingplane.top[rwx] := top;
        ceilingplane.bottom[rwx] := bottom;
      end;
      // SoM: this should be set here
      // jval: Also prevent problems in multithreading rendering
      pceilingclip^ := bottom;
    end;

    yh := bottomfrac div HEIGHTUNIT;

    if yh >= pfloorclip^ then
      yh := pfloorclip^ - 1;

    if markfloor then
    begin
      top := yh + 1;
      bottom := pfloorclip^ - 1;
      if top <= pceilingclip^ then
        top := pceilingclip^ + 1;
      if top <= bottom then
      begin
        floorplane.top[rwx] := top;
        floorplane.bottom[rwx] := bottom;
      end;
      // SoM: This should be set here to prevent overdraw
      // jval: Also prevent problems in multithreading rendering
      pfloorclip^ := top;
    end;

    // texturecolumn and lighting are independent of wall tiers
    if segtextured then
    begin
      // calculate texture offset
      {$IFDEF FPC}
      angle := _SHRW(rw_centerangle + xtoviewangle[rwx], ANGLETOFINESHIFT);
      {$ELSE}
      angle := (rw_centerangle + xtoviewangle[rwx]) shr ANGLETOFINESHIFT;
      {$ENDIF}
      texturecolumn := rw_offset - FixedMul(finetangent[angle], rw_distance);

      texturecolumnhi := texturecolumn shr (FRACBITS - DC_HIRESBITS);
      texturecolumn := texturecolumn shr FRACBITS;
      // calculate lighting
      index := _SHR(rw_scale * 320 div SCREENWIDTH, LIGHTSCALESHIFT);

      if index >=  MAXLIGHTSCALE then
        index := MAXLIGHTSCALE - 1;

      dc_colormap := walllights[index];
      dc_x := rwx;
      dc_iscale := LongWord($ffffffff) div LongWord(rw_scale);
    end;

    // draw the wall tiers
    if midtexture <> 0 then
    begin
      // single sided line
      dc_yl := yl;
      dc_yh := yh;
      dc_texturemid := rw_midtexturemid;
      dc_source := R_GetColumn(midtexture, texturecolumn);
      {$IFDEF RENDERSEGOPTIMIZED}
      R_StoreWallColumn8(@midwalls8);
      {$ELSE}
      wallcolfunc;
      {$ENDIF}
      pceilingclip^ := viewheight;
      pfloorclip^ := -1;
    end
    else
    begin
      // two sided line
      if toptexture <> 0 then
      begin
        // top wall
        mid := pixhigh div HEIGHTUNIT;
        pixhigh := pixhigh + pixhighstep;

        if mid >= pfloorclip^ then
          mid := pfloorclip^ - 1;

        if mid >= yl then
        begin
          dc_yl := yl;
          dc_yh := mid;
          dc_texturemid := rw_toptexturemid;
          dc_source := R_GetColumn(toptexture, texturecolumn);
          {$IFDEF RENDERSEGOPTIMIZED}
          R_StoreWallColumn8(@upperwalls8);
          {$ELSE}
          wallcolfunc;
          {$ENDIF}
          pceilingclip^ := mid;
        end
        else
          pceilingclip^ := yl - 1;
      end
      else
      begin
        // no top wall
        if markceiling then
          pceilingclip^ := yl - 1;
      end;

      if bottomtexture <> 0 then
      begin
        // bottom wall
        mid := (pixlow + HEIGHTUNIT - 1) div HEIGHTUNIT;
        pixlow := pixlow + pixlowstep;

        // no space above wall?
        if mid <= pceilingclip^ then
          mid := pceilingclip^ + 1;

        if mid <= yh then
        begin
          dc_yl := mid;
          dc_yh := yh;
          dc_texturemid := rw_bottomtexturemid;
          dc_source := R_GetColumn(bottomtexture, texturecolumn);
          {$IFDEF RENDERSEGOPTIMIZED}
          R_StoreWallColumn8(@lowerwalls8);
          {$ELSE}
          wallcolfunc;
          {$ENDIF}
          pfloorclip^ := mid;
        end
        else
          pfloorclip^ := yh + 1;
      end
      else
      begin
        // no bottom wall
        if markfloor then
          pfloorclip^ := yh + 1;
      end;

      if maskedtexture then
      begin
        // save texturecol
        // for backdrawing of masked mid texture
        maskedtexturecol[rwx] := texturecolumnhi;
      end;
    end;

    rw_scale := rw_scale + rw_scalestep;
    topfrac := topfrac + topstep;
    bottomfrac := bottomfrac + bottomstep;
    inc(rwx);
    inc(pceilingclip);
    inc(pfloorclip);
  end;
  {$IFDEF RENDERSEGOPTIMIZED}
  R_FlashWallColumns8(@midwalls8);
  R_FlashWallColumns8(@lowerwalls8);
  R_FlashWallColumns8(@upperwalls8);
  {$ENDIF}

end;
