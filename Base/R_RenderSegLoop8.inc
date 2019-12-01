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
{$IFDEF FLOORS3DVIS}
  r: midsiderange_t;
  totalclip: boolean;
{$ENDIF}
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
    {$IFDEF USEDOUBLE}
    yl := Trunc((topfrac_dbl + (HEIGHTUNIT - 1)) / HEIGHTUNIT);
    {$ELSE}
    yl := (topfrac + (HEIGHTUNIT - 1)) div HEIGHTUNIT;
    {$ENDIF}
    if yl > viewheight then
      yl := viewheight + 1;

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
        {$IFDEF DEBUG}
        R_DebugCheckVisPlane(ceilingplane);
        {$ENDIF}
      end;
      // SoM: this should be set here
      // jval: Also prevent problems in multithreading rendering
      pceilingclip^ := bottom;
      {$IFDEF DEBUG}
      R_CheckClipItem(pceilingclip);
      R_CheckClipItem(pfloorclip);
      {$ENDIF}
    end;

    {$IFDEF USEDOUBLE}
    yh := Trunc(bottomfrac_dbl / HEIGHTUNIT);
    {$ELSE}
    yh := bottomfrac div HEIGHTUNIT;
    {$ENDIF}

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
        {$IFDEF DEBUG}
        R_DebugCheckVisPlane(floorplane);
        {$ENDIF}
      end;
      // SoM: this should be set here to prevent overdraw
      // jval: Also prevents problems in multithreading rendering
      pfloorclip^ := top;
      {$IFDEF DEBUG}
      R_CheckClipItem(pfloorclip);
      R_CheckClipItem(pceilingclip);
      {$ENDIF}
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
      {$IFDEF USEDOUBLE}
      index := _SHR(Round(rw_scale_dbl * 320 / SCREENWIDTH), LIGHTSCALESHIFT);
      {$ELSE}
      index := _SHR(rw_scale * 320 div SCREENWIDTH, LIGHTSCALESHIFT);
      {$ENDIF}

      if index >= MAXLIGHTSCALE then
        index := MAXLIGHTSCALE - 1
      else if index < 0 then
        index := 0;

      dc_colormap := walllights[index];
{$IFDEF FLOORS3DVIS}
      if fixedcolormap <> nil then
      begin
        dc_colormap := fixedcolormap;
        segloopcolormap1 := fixedcolormap;
        dc_colormap2 := fixedcolormap;
        segloopcolormap2 := fixedcolormap;
      end
      else
      begin
        segloopcolormap1 := dc_colormap;
        dc_colormap2 := walllights2[index];
        segloopcolormap2 := dc_colormap2;
      end;
{$ENDIF}

      dc_x := rwx;
      {$IFDEF USEDOUBLE}
      if (rw_scale_dbl < 4) and (rw_scale_dbl > -4) then
      begin
        if rw_scale_dbl > 0 then
          dc_iscale := MAXINT div 2
        else
          dc_iscale := - MAXINT div 2
      end
      else
      begin
        dc_iscale := round($100000000 / rw_scale_dbl);
        if dc_iscale > MAXINT div 2 then
          dc_iscale := MAXINT div 2
        else if dc_iscale < -MAXINT div 2 then
          dc_iscale := -MAXINT div 2
      end;
      if (dc_iscale < 4) and (dc_iscale > -4) then
      begin
        if dc_iscale > 0 then
          dc_iscale := 4
        else
          dc_iscale := -4
      end;
      {$ELSE}
      dc_iscale := LongWord($ffffffff) div LongWord(rw_scale);
      {$ENDIF}
    end;

    // draw the wall tiers
    if midtexture <> 0 then
    begin
      // single sided line
      dc_texturemid := rw_midtexturemid;

      dc_source := R_GetColumn(midtexture, texturecolumn);
{$IFDEF FLOORS3DVIS}
      if pds.midvis <> nil then
      begin
        R_SubtractRange(yh, yl, integer(segloopcolormap1),
                        pds.midvis.realbottom[rwx], pds.midvis.realtop[rwx], integer(segloopcolormap2),
                        @r, totalclip);

        if r.count <> 0 then
        begin
          dc_yl := r.ceilingheight[0];
          dc_yh := r.floorheight[0];
          dc_colormap := PByteArray(r.lightlevel[0]);

          {$IFDEF RENDERSEGOPTIMIZED}
          R_StoreWallColumn8(@midwalls8);
          if totalclip then
            R_FlashWallColumns8(@midwalls8);
          {$ELSE}
          wallcolfunc;
          {$ENDIF}
          dc_yl := r.ceilingheight[1];
          dc_yh := r.floorheight[1];
          dc_colormap := PByteArray(r.lightlevel[1]);

          {$IFDEF RENDERSEGOPTIMIZED}
          if totalclip then
            R_FlashWallColumns8(@midwalls8b)
          else
            R_StoreWallColumn8(@midwalls8b);
          {$ELSE}
          wallcolfunc;
          {$ENDIF}
        end
        else
        begin
          {$IFDEF RENDERSEGOPTIMIZED}
          R_FlashWallColumns8(@midwalls8);
          R_FlashWallColumns8(@midwalls8b);
          {$ENDIF}
        end;
      end
      else
      begin
{$ENDIF}
      dc_yl := yl;
      dc_yh := yh;

      {$IFDEF RENDERSEGOPTIMIZED}
      R_StoreWallColumn8(@midwalls8);
      {$ELSE}
      wallcolfunc;
      {$ENDIF}
{$IFDEF FLOORS3DVIS}
      end;
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
	{$IFDEF USEDOUBLE}
        mid := Trunc(pixhigh_dbl / HEIGHTUNIT);
        pixhigh_dbl := pixhigh_dbl + pixhighstep_dbl;
	{$ELSE}
        mid := pixhigh div HEIGHTUNIT;
        pixhigh := pixhigh + pixhighstep;
        {$ENDIF}

        if mid >= pfloorclip^ then
          mid := pfloorclip^ - 1;

        if mid >= yl then
        begin
          dc_texturemid := rw_toptexturemid;
          dc_source := R_GetColumn(toptexture, texturecolumn);

    {$IFDEF FLOORS3DVIS}
          if pds.midvis <> nil then
          begin
            R_SubtractRange(mid, yl, integer(segloopcolormap1),
                            pds.midvis.realbottom[rwx], pds.midvis.realtop[rwx], integer(segloopcolormap2),
                            @r, totalclip);

            if r.count <> 0 then
            begin
              dc_yl := r.ceilingheight[0];
              dc_yh := r.floorheight[0];
              dc_colormap := PByteArray(r.lightlevel[0]);

              {$IFDEF RENDERSEGOPTIMIZED}
              R_StoreWallColumn8(@upperwalls8);
              if totalclip then
                R_FlashWallColumns8(@upperwalls8);
              {$ELSE}
              wallcolfunc;
              {$ENDIF}
              dc_yl := r.ceilingheight[1];
              dc_yh := r.floorheight[1];
              dc_colormap := PByteArray(r.lightlevel[1]);

              {$IFDEF RENDERSEGOPTIMIZED}
              if totalclip then
                R_FlashWallColumns8(@upperwalls8b)
              else
                R_StoreWallColumn8(@upperwalls8b);
              {$ELSE}
              wallcolfunc;
              {$ENDIF}
            end
            else
            begin
              {$IFDEF RENDERSEGOPTIMIZED}
              R_FlashWallColumns8(@upperwalls8);
              R_FlashWallColumns8(@upperwalls8b);
              {$ENDIF}
            end;
          end
          else
          begin
    {$ENDIF}

          dc_yl := yl;
          dc_yh := mid;
          {$IFDEF RENDERSEGOPTIMIZED}
          R_StoreWallColumn8(@upperwalls8);
          {$ELSE}
          wallcolfunc;
          {$ENDIF}
    {$IFDEF FLOORS3DVIS}
          end;
    {$ENDIF}

          pceilingclip^ := mid;
          {$IFDEF DEBUG}
          R_CheckClipItem(pceilingclip);
          R_CheckClipItem(pfloorclip);
          {$ENDIF}
        end
        else
        begin
          pceilingclip^ := yl - 1;
          {$IFDEF DEBUG}
          R_CheckClipItem(pceilingclip);
          R_CheckClipItem(pfloorclip);
          {$ENDIF}
        end;
      end
      else
      begin
        // no top wall
        if markceiling then
        begin
          pceilingclip^ := yl - 1;
          {$IFDEF DEBUG}
          R_CheckClipItem(pceilingclip);
          R_CheckClipItem(pfloorclip);
          {$ENDIF}
        end;
      end;

      if bottomtexture <> 0 then
      begin
        // bottom wall
	{$IFDEF USEDOUBLE}
        mid := Trunc((pixlow_dbl + HEIGHTUNIT - 1) / HEIGHTUNIT);
        pixlow_dbl := pixlow_dbl + pixlowstep_dbl;
	{$ELSE}
        mid := (pixlow + HEIGHTUNIT - 1) div HEIGHTUNIT;
        pixlow := pixlow + pixlowstep;
        {$ENDIF}

        // no space above wall?
        if mid <= pceilingclip^ then
          mid := pceilingclip^ + 1;

        if mid <= yh then
        begin
          dc_texturemid := rw_bottomtexturemid;
          dc_source := R_GetColumn(bottomtexture, texturecolumn);
	  
    {$IFDEF FLOORS3DVIS}
          if pds.midvis <> nil then
          begin
            R_SubtractRange(yh, mid, integer(segloopcolormap1),
                            pds.midvis.realbottom[rwx], pds.midvis.realtop[rwx], integer(segloopcolormap2),
                            @r, totalclip);

            if r.count <> 0 then
            begin
              dc_yl := r.ceilingheight[0];
              dc_yh := r.floorheight[0];
              dc_colormap := PByteArray(r.lightlevel[0]);

              {$IFDEF RENDERSEGOPTIMIZED}
              R_StoreWallColumn8(@lowerwalls8);
              if totalclip then
                R_FlashWallColumns8(@lowerwalls8);
              {$ELSE}
              wallcolfunc;
              {$ENDIF}
              dc_yl := r.ceilingheight[1];
              dc_yh := r.floorheight[1];
              dc_colormap := PByteArray(r.lightlevel[1]);

              {$IFDEF RENDERSEGOPTIMIZED}
              if totalclip then
                R_FlashWallColumns8(@lowerwalls8b)
              else
                R_StoreWallColumn8(@lowerwalls8b);
              {$ELSE}
              wallcolfunc;
              {$ENDIF}
            end
            else
            begin
              {$IFDEF RENDERSEGOPTIMIZED}
              R_FlashWallColumns8(@lowerwalls8);
              R_FlashWallColumns8(@lowerwalls8b);
              {$ENDIF}
            end;
          end
          else
          begin
    {$ENDIF}
          dc_yl := mid;
          dc_yh := yh;
          {$IFDEF RENDERSEGOPTIMIZED}
          R_StoreWallColumn8(@lowerwalls8);
          {$ELSE}
          wallcolfunc;
          {$ENDIF}
    {$IFDEF FLOORS3DVIS}
          end;
    {$ENDIF}
          pfloorclip^ := mid;
          {$IFDEF DEBUG}
          R_CheckClipItem(pfloorclip);
          R_CheckClipItem(pceilingclip);
          {$ENDIF}
        end
        else
        begin
          pfloorclip^ := yh + 1;
          {$IFDEF DEBUG}
          R_CheckClipItem(pfloorclip);
          R_CheckClipItem(pceilingclip);
          {$ENDIF}
        end;
      end
      else
      begin
        // no bottom wall
        if markfloor then
        begin
          pfloorclip^ := yh + 1;
          {$IFDEF DEBUG}
          R_CheckClipItem(pfloorclip);
          R_CheckClipItem(pceilingclip);
          {$ENDIF}
        end;
      end;

      if maskedtexture then
      begin
        // save texturecol
        // for backdrawing of masked mid texture
        maskedtexturecol[rwx] := texturecolumnhi;
{$IFDEF FLOORS3D}
        if pds.thicksidecol <> nil then            // JVAL: 3d Floors
          pds.thicksidecol[rwx] := texturecolumnhi;// JVAL: 3d Floors
{$ENDIF}
      end;
    end;

    {$IFDEF USEDOUBLE}
    rw_scale_dbl := rw_scale_dbl + rw_scalestep_dbl;
    topfrac_dbl := topfrac_dbl + topstep_dbl;
    bottomfrac_dbl := bottomfrac_dbl + bottomstep_dbl;
    {$ELSE}
    rw_scale := rw_scale + rw_scalestep;
    topfrac := topfrac + topstep;
    bottomfrac := bottomfrac + bottomstep;
    {$ENDIF}
    inc(rwx);
    inc(pceilingclip);
    inc(pfloorclip);
  end;
  {$IFDEF RENDERSEGOPTIMIZED}
  R_FlashWallColumns8(@midwalls8);
  R_FlashWallColumns8(@lowerwalls8);
  R_FlashWallColumns8(@upperwalls8);
  {$IFDEF FLOORS3DVIS}
  // JVAL: 3d Floors
  R_FlashWallColumns8(@midwalls8b);
  R_FlashWallColumns8(@lowerwalls8b);
  R_FlashWallColumns8(@upperwalls8b);
  {$ENDIF}
  {$ENDIF}
end;
