//------------------------------------------------------------------------------
//
//  DelphiDoom: A modified and improved DOOM engine for Windows
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 1993-1996 by id Software, Inc.
//  Copyright (C) 2004-2021 by Jim Valavanis
//
//  This program is free software; you can redistribute it and/or
//  modify it under the terms of the GNU General Public License
//  as published by the Free Software Foundation; either version 2
//  of the License, or (at your option) any later version.
//
//  This program is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//  GNU General Public License for more details.
//
//  You should have received a copy of the GNU General Public License
//  along with this program; if not, write to the Free Software
//  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
//  02111-1307, USA.
//
//  DESCRIPTION:
//   Textured AutoMap.
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit am_textured;

interface

procedure AM_setSubSectorDrawFuncs;

procedure AM_drawSubSectors;

procedure AM_InitTextured;

procedure AM_ShutDownTextured;

implementation

uses
  d_delphi,
  doomdef,
  doomdata,
  d_main,
  am_map,
  i_system,
{$IFNDEF OPENGL}
  i_threads,
  r_hires,
  r_draw,
  r_trans8,
  r_flatinfo,
{$ELSE}
  r_precalc,
  gl_automap,
  v_data,
{$ENDIF}
  r_data,
  r_defs,
  r_main,
  p_setup,
  p_local,
  m_fixed,
  tables,
  v_video,
  w_wad,
  z_zone;

type
  drawpoint_t = record
    x, y: integer;
  end;

  drawtriangle_t = array[0..2] of drawpoint_t;
  Pdrawtriangle_t = ^drawtriangle_t;

{$IFNDEF OPENGL}
  drawsegfunc_t = procedure(const xx, yy, cc: integer; const amcolormap: pointer);

var
  drawsegfunc: drawsegfunc_t;
{$ENDIF}

{$IFNDEF OPENGL}
procedure AM_DrawSeg8(xx, yy, cc: integer; const amcolormap: PByteArray);
begin
// JVAL Clip line if in overlay mode
  if amstate = am_overlay then
  begin
    if yy <= viewwindowy then
      exit;
    if yy >= viewwindowy + viewheight then
      exit;
    if xx <= viewwindowx then
      exit;
    if xx >= viewwindowx + viewwidth then
      exit;
  end;
  fb[yy * f_w + xx] := amcolormap[cc];
end;

procedure AM_DrawSeg8Transparent(xx, yy, cc: integer; const amcolormap: PByteArray);
var
  b: PByte;
begin
// JVAL Clip line if in overlay mode
  if amstate = am_overlay then
  begin
    if yy <= viewwindowy then
      exit;
    if yy >= viewwindowy + viewheight then
      exit;
    if xx <= viewwindowx then
      exit;
    if xx >= viewwindowx + viewwidth then
      exit;
  end;
  b := @fb[yy * f_w + xx];
  b^ := averagetrans8table[b^ shl 8 + amcolormap[cc]];
end;

procedure AM_DrawSeg32(xx, yy, cc: integer; const amcolormap: PLongWordArray);
begin
// JVAL Clip line if in overlay mode
  if amstate = am_overlay then
  begin
    if yy <= viewwindowy then
      exit;
    if yy >= viewwindowy + viewheight then
      exit;
    if xx <= viewwindowx then
      exit;
    if xx >= viewwindowx + viewwidth then
      exit;
  end;
  fb32[yy * f_w + xx] := amcolormap[cc];
end;

procedure AM_DrawSeg32Transparent(xx, yy, cc: integer; const amcolormap: PLongWordArray);
var
  pl: PLongWord;
begin
// JVAL Clip line if in overlay mode
  if amstate = am_overlay then
  begin
    if yy <= viewwindowy then
      exit;
    if yy >= viewwindowy + viewheight then
      exit;
    if xx <= viewwindowx then
      exit;
    if xx >= viewwindowx + viewwidth then
      exit;
  end;
  pl := @fb32[yy * f_w + xx];
  pl^ := R_ColorMean(pl^, amcolormap[cc]);
end;
{$ENDIF}

type
  seg_ap3 = array[0..2] of Pseg_t;

var
  pla: angle_t;
  plx, ply: fixed_t;
  amsin, amcos: fixed_t;
  f_mx: integer;

{$IFNDEF OPENGL}
procedure AM_DecodeUV(var xx, yy: integer; const sz: integer);
var
  tmpx: fixed_t;
begin
  if allowautomaprotate then
  begin
    tmpx := plx +
      FixedMul(xx - plx, amcos) -
      FixedMul(yy - ply, amsin);

    yy := ply +
      FixedMul(xx - plx, amsin) +
      FixedMul(yy - ply, amcos);

    xx := tmpx;
  end;
  yy := sz - (yy div MAPUNIT) and sz;
  xx := (xx div MAPUNIT) and sz;
end;

procedure AM_DecodeAngleUV(var xx, yy: integer; const sz: integer;
  const fangx, fangy, fangsin, fangcos: fixed_t);
var
  tmpx: fixed_t;
begin
  if allowautomaprotate then
  begin
    tmpx := plx +
      FixedMul(xx - plx, amcos) -
      FixedMul(yy - ply, amsin);

    yy := ply +
      FixedMul(xx - plx, amsin) +
      FixedMul(yy - ply, amcos);

    xx := tmpx;
  end;

  tmpx := fangx +
    FixedMul(xx - fangx, fangcos) -
    FixedMul(yy - fangy, fangsin);

  yy := fangy +
    FixedMul(xx - fangx, fangsin) +
    FixedMul(yy - fangy, fangcos);

  xx := tmpx;

  yy := sz - (yy div MAPUNIT) and sz;
  xx := (xx div MAPUNIT) and sz;
end;
{$ENDIF}

procedure AM_DrawTexturedTriangle(const lst: seg_ap3; const lump: integer; const sec: Psector_t;
  const aminx, amaxx: integer; const {$IFDEF OPENGL}lightlevel: integer{$ELSE}amcolormap: pointer{$ENDIF});
{$IFNDEF OPENGL}
var
  data: PByteArray;
  flat_width: integer;
  flat: integer;
  fang: angle_t;
  fangx, fangy: fixed_t;
  fangsin, fangcos: fixed_t;

  procedure fillLeftFlatTriangle(v1, v2, v3: drawpoint_t);
  var
    invslope1, invslope2: fixed_t;
    cury1, cury2: fixed_t;
    i, j: integer;
    v: drawpoint_t;
    du, dv: integer;
    iup: integer;
    idown: integer;
    jup: integer;
    jdown: integer;
    xx, yy: integer;
  begin
    if v2.y > v3.y then
    begin
      v := v3;
      v3 := v2;
      v2 := v;
    end;
    invslope1 := Round((v2.y - v1.y) / (v2.x - v1.x) * FRACUNIT);
    invslope2 := Round((v3.y - v1.y) / (v3.x - v1.x) * FRACUNIT);

    cury1 := v1.y * FRACUNIT;
    cury2 := cury1;

    iup := v2.x;
    if iup >= amaxx then
      iup := amaxx - 1;
    idown := v1.x;
    if idown < aminx then
    begin
      idown := aminx;
      cury1 := cury1 + (idown - v1.x) * invslope1;
      cury2 := cury2 + (idown - v1.x) * invslope2;
    end;

    xx := (idown - f_x) * scale_ftom + m_x;
    for i := idown to iup do
    begin
      jup := cury2 div FRACUNIT + 1;
      if jup >= 0 then
      begin
        if jup >= f_mx then
          jup := f_mx - 1;
        jdown := cury1 div FRACUNIT;
        if jdown < 0 then
          jdown := 0;

        yy := m_y - (jdown - f_y - f_mx) * scale_ftom;
        for j := jdown to jup do
        begin
          du := xx;
          dv := yy;
          if fang <> 0 then
            AM_DecodeAngleUV(du, dv, flat_width - 1, fangx, fangy, fangsin, fangcos)
          else
            AM_DecodeUV(du, dv, flat_width - 1);
          {$IFDEF DOOM_OR_STRIFE}
          if sec.floor_xoffs <> 0 then
            du := (du + (sec.floor_xoffs div FRACUNIT)) and (flat_width - 1);
          if sec.floor_yoffs <> 0 then
            dv := (dv + (sec.floor_yoffs div FRACUNIT)) and (flat_width - 1);
          {$ENDIF}
          drawsegfunc(i, j, data[du + dv * flat_width], amcolormap);
          yy := yy - scale_ftom;
        end;
      end;
      cury1 := cury1 + invslope1;
      cury2 := cury2 + invslope2;
      xx := xx + scale_ftom;
    end;
  end;

  procedure fillRightFlatTriangle(v1, v2, v3: drawpoint_t);
  var
    invslope1, invslope2: fixed_t;
    cury1, cury2: fixed_t;
    i, j: integer;
    v: drawpoint_t;
    du, dv: integer;
    idown: integer;
    iup: integer;
    jup: integer;
    jdown: integer;
    xx, yy: integer;
  begin
    if v2.y < v1.y then
    begin
      v := v2;
      v2 := v1;
      v1 := v;
    end;
    invslope1 := Round((v3.y - v1.y) / (v3.x - v1.x) * FRACUNIT);
    invslope2 := Round((v3.y - v2.y) / (v3.x - v2.x) * FRACUNIT);

    cury1 := v3.y * FRACUNIT;
    cury2 := cury1;

    idown := v1.x + 1;
    if idown < aminx then
      idown := aminx;

    iup := v3.x;
    if iup >= amaxx then
    begin
      iup := amaxx - 1;
      cury1 := cury1 - (v3.x - iup) * invslope1;
      cury2 := cury2 - (v3.x - iup) * invslope2;
    end;

    xx := (iup - f_x) * scale_ftom + m_x;
    for i := iup downto idown do
    begin
      cury1 := cury1 - invslope1;
      cury2 := cury2 - invslope2;

      jup := cury2 div FRACUNIT + 1;
      if jup >= 0 then
      begin
        if jup >= f_mx then
          jup := f_mx - 1;
        jdown := cury1 div FRACUNIT;
        if jdown < 0 then
          jdown := 0;

        yy := m_y - (jdown - f_y - f_mx) * scale_ftom;
        for j := jdown to jup do
        begin
          du := xx;
          dv := yy;
          if fang <> 0 then
            AM_DecodeAngleUV(du, dv, flat_width - 1, fangx, fangy, fangsin, fangcos)
          else
            AM_DecodeUV(du, dv, flat_width - 1);
          {$IFDEF DOOM_OR_STRIFE}
          if sec.floor_xoffs <> 0 then
            du := (du + (sec.floor_xoffs div FRACUNIT)) and (flat_width - 1);
          if sec.floor_yoffs <> 0 then
            dv := (dv + (sec.floor_yoffs div FRACUNIT)) and (flat_width - 1);
          {$ENDIF}
          drawsegfunc(i, j, data[du + dv * flat_width], amcolormap);
          yy := yy - scale_ftom;
        end;
      end;
      xx := xx - scale_ftom;
    end;
  end;
{$ENDIF}
var
  v1, v2, v3{$IFNDEF OPENGL}, v4{$ENDIF}: drawpoint_t;
  {$IFDEF OPENGL}
  tmpuv, du0, dv0, du1, dv1, du2, dv2: integer;
  clight: LongWord;
  {$ENDIF}
  t: drawtriangle_t;
  v: Pvertex_t;
  i: integer;
begin
  v := lst[0].v1;
  t[0].x := v.x div FRACTOMAPUNIT;
  t[0].y := v.y div FRACTOMAPUNIT;
{$IFDEF OPENGL}
  du0 := -v.y;
  dv0 := v.x;
{$ENDIF}
  v := lst[1].v1;
  t[1].x := v.x div FRACTOMAPUNIT;
  t[1].y := v.y div FRACTOMAPUNIT;
{$IFDEF OPENGL}
  du1 := -v.y;
  dv1 := v.x;
{$ENDIF}
  v := lst[2].v1;
  t[2].x := v.x div FRACTOMAPUNIT;
  t[2].y := v.y div FRACTOMAPUNIT;
{$IFDEF OPENGL}
  du2 := -v.y;
  dv2 := v.x;
{$ENDIF}
  for i := 0 to 2 do
  begin
    if allowautomaprotate then
      AM_rotate(@t[i].x, @t[i].y, ANG90 - plr.mo.angle, plr.mo.x div FRACTOMAPUNIT, plr.mo.y div FRACTOMAPUNIT);
    t[i].x := CXMTOF(t[i].x);
    t[i].y := CYMTOF(t[i].y);
  end;

  if t[1].x < t[0].x then
  begin
    v1 := t[1];
    t[1] := t[0];
    t[0] := v1;
    {$IFDEF OPENGL}
    tmpuv := du1;
    du1 := du0;
    du0 := tmpuv;
    tmpuv := dv1;
    dv1 := dv0;
    dv0 := tmpuv;
    {$ENDIF}
  end;

  if t[2].x < t[1].x then
  begin
    v1 := t[2];
    t[2] := t[1];
    t[1] := v1;
    {$IFDEF OPENGL}
    tmpuv := du2;
    du2 := du1;
    du1 := tmpuv;
    tmpuv := dv2;
    dv2 := dv1;
    dv1 := tmpuv;
    {$ENDIF}
  end;

  if t[1].x < t[0].x then
  begin
    v1 := t[1];
    t[1] := t[0];
    t[0] := v1;
    {$IFDEF OPENGL}
    tmpuv := du1;
    du1 := du0;
    du0 := tmpuv;
    tmpuv := dv1;
    dv1 := dv0;
    dv0 := tmpuv;
    {$ENDIF}
  end;

  if t[2].x < aminx then
    Exit;

  if t[0].x >= amaxx then
    Exit;

  if t[0].y < 0 then
    if t[1].y < 0 then
      if t[2].y < 0 then
        Exit;

  if t[0].y >= f_mx then
    if t[1].y >= f_mx then
      if t[2].y >= f_mx then
        Exit;

  v1 := t[0];
  v2 := t[1];
  v3 := t[2];

{$IFNDEF OPENGL}
  flat := sec.floorpic;

  if flats[flats[flat].translation].size <= 0 then
    flat_width := 64
  else
    flat_width := dsscalesize[flats[flats[flat].translation].size].flatsize;

  fang := sec.floorangle;
  if fang <> 0 then
  begin
    fangx := sec.flooranglex div FRACTOMAPUNIT;
    fangy := sec.floorangley div FRACTOMAPUNIT;
    fangsin := fixedsine[(ANGLE_MAX - fang) shr FRACBITS];
    fangcos := fixedcosine[(ANGLE_MAX - fang) shr FRACBITS];
  end;

  data := W_CacheLumpNum(lump, PU_LEVEL);

  if v2.x = v3.x then
  begin
    fillLeftFlatTriangle(v1, v2, v3);
    Exit;
  end;

  if v1.x = v2.x then
  begin
    fillRightFlatTriangle(v1, v2, v3);
    Exit;
  end;

  v4.y := round(v1.y + ((v2.x - v1.x) / (v3.x - v1.x)) * (v3.y - v1.y));
  v4.x := v2.x;

  fillLeftFlatTriangle(v1, v2, v4);
  fillRightFlatTriangle(v2, v4, v3);
{$ELSE}
  clight := precal8_tolong[ibetween(lightlevel, 0, 255)];

  gld_AddAutomapTriangle(
    v1.x, v1.y, du0, dv0,
    v2.x, v2.y, du1, dv1,
    v3.x, v3.y, du2, dv2,
    clight,
    lump,
    sec
  );
{$ENDIF}
end;

procedure AM_setSubSectorDrawFuncs;
begin
{$IFNDEF OPENGL}
  if amstate = am_overlay then
  begin
    if videomode = vm32bit then
      drawsegfunc := @AM_DrawSeg32Transparent
    else
      drawsegfunc := @AM_DrawSeg8Transparent;
  end
  else
  begin
    if videomode = vm32bit then
      drawsegfunc := @AM_DrawSeg32
    else
      drawsegfunc := @AM_DrawSeg8;
  end;
{$ENDIF}
end;

type
  dssparams_t = record
    minx, maxx: integer;
  end;
  dssparams_p = ^dssparams_t;

var
  amvalidcount: integer = 0;

function AM_dodrawSubSectors(parms: dssparams_p): integer; {$IFNDEF OPENGL}stdcall;{$ENDIF}
var
  i, j: integer;
  ssector: Psubsector_t;
  seg: Pseg_t;
  lst: seg_ap3;
  drawit: boolean;
  wasdrawned: boolean;
  lump: integer;
  {$IFDEF OPENGL}
  lightlevel: integer;
  {$ELSE}
  cmap: integer;
  amcolormap: pointer;
  {$ENDIF}
begin
  for i := 0 to numsubsectors - 1 do
  begin
    ssector := @subsectors[i];

    if ssector.numlines < 3 then
      continue;

    if am_cheating <> 0 then
      drawit := true
    else
    begin
      drawit := false;
      seg := @segs[ssector.firstline];
      for j := 0 to ssector.numlines - 1 do
      begin
        if not seg.miniseg then
          if seg.linedef.flags and ML_MAPPED <> 0 then
          begin
            drawit := True;
            Break;
          end;
        inc(seg);
      end;
    end;

    if drawit then
    begin
      {$IFDEF OPENGL}
      lightlevel := ssector.sector.lightlevel;
      {$ELSE}
      cmap := (ssector.sector.lightlevel) div 8;
      if cmap > 31 then
        cmap := 31
      else if cmap < 0 then
        cmap := 0;
      if ssector.sector.renderflags and SRF_FOG <> 0 then // JVAL: Mars fog sectors
      begin
        if videomode = vm32bit then
          amcolormap := @fog_colormaps32[(31 - cmap) * 256]
        else
          amcolormap := @fog_colormaps[(31 - cmap) * 256];
      end
      else
      begin
        if videomode = vm32bit then
          amcolormap := @colormaps32[(31 - cmap) * 256]
        else
          amcolormap := @colormaps[(31 - cmap) * 256];
      end;
      {$ENDIF}

      seg := @segs[ssector.firstline];
      seg.v1.amvalidcount := amvalidcount;
      seg.v2.amvalidcount := amvalidcount;
      lst[0] := seg;
      Inc(seg);
      seg.v1.amvalidcount := amvalidcount;
      seg.v2.amvalidcount := amvalidcount;
      lst[2] := seg;
      lump := R_GetLumpForFlat(ssector.sector.floorpic);
      for j := 2 to ssector.numlines - 1 do
      begin
        Inc(seg);
        seg.v1.amvalidcount := amvalidcount;
        seg.v2.amvalidcount := amvalidcount;
        lst[1] := lst[2];
        lst[2] := seg;

        AM_DrawTexturedTriangle(lst, lump, ssector.sector, parms.minx, parms.maxx, {$IFDEF OPENGL}lightlevel{$ELSE}amcolormap{$ENDIF});
      end;
    end;

  end;

  if am_cheating = 0 then
    for i := 0 to numsubsectors - 1 do
    begin
      ssector := @subsectors[i];

      if ssector.numlines < 3 then
        continue;

      wasdrawned := false;
      seg := @segs[ssector.firstline];
      for j := 0 to ssector.numlines - 1 do
      begin
        if not seg.miniseg then
          if seg.linedef.flags and ML_MAPPED <> 0 then
          begin
            wasdrawned := True;
            Break;
          end;
        inc(seg);
      end;
      if wasdrawned then
        continue;

      drawit := false;
      seg := @segs[ssector.firstline];
      for j := 0 to ssector.numlines - 1 do
      begin
        if seg.miniseg then
          if seg.v1.amvalidcount = amvalidcount then
            if seg.v2.amvalidcount = amvalidcount then
            begin
              drawit := True;
              Break;
            end;
        inc(seg);
      end;

      if drawit then
      begin
        {$IFDEF OPENGL}
        lightlevel := ssector.sector.lightlevel;
        {$ELSE}
        cmap := (ssector.sector.lightlevel) div 8;
        if cmap > 31 then
          cmap := 31
        else if cmap < 0 then
          cmap := 0;
        if videomode = vm32bit then
          amcolormap := @colormaps32[(31 - cmap) * 256]
        else
          amcolormap := @colormaps[(31 - cmap) * 256];
        {$ENDIF}

        seg := @segs[ssector.firstline];
        lst[0] := seg;
        Inc(seg);
        lst[2] := seg;
        lump := R_GetLumpForFlat(ssector.sector.floorpic);
        for j := 2 to ssector.numlines - 1 do
        begin
          Inc(seg);
          lst[1] := lst[2];
          lst[2] := seg;
          AM_DrawTexturedTriangle(lst, lump, ssector.sector, parms.minx, parms.maxx, {$IFDEF OPENGL}lightlevel{$ELSE}amcolormap{$ENDIF});
        end;
      end;

    end;

  result := 0;
end;

{$IFNDEF OPENGL}
const
  MAXDSSTHREADS = 14;

var
  dssthreads: array[0..MAXDSSTHREADS - 2] of TDThread;
  numdssthreads: integer = 1;
{$ENDIF}

procedure AM_drawSubSectors;
var
  parms: {$IFNDEF OPENGL}array[0..MAXDSSTHREADS - 1] of{$ENDIF} dssparams_t;
{$IFNDEF OPENGL}
  i: integer;
  l, h: integer;
{$ENDIF}
begin
  Inc(amvalidcount);
  pla := plr.mo.angle - ANG90;
  plx := plr.mo.x div FRACTOMAPUNIT;
  ply := plr.mo.y div FRACTOMAPUNIT;
  amsin := fixedsine[pla shr FRACBITS];
  amcos := fixedcosine[pla shr FRACBITS];

  f_mx := f_h;
  if amstate = am_overlay then
    if screenblocks >= 10 then
      f_mx := SCREENWIDTH;

  {$IFNDEF OPENGL}
  if usemultithread then
  begin
    // Precache floor textures
    for i := 0 to numsectors - 1 do
      W_CacheLumpNum(R_GetLumpForFlat(sectors[i].floorpic), PU_LEVEL);

    numdssthreads := I_GetNumCPUs;
    if numdssthreads > MAXDSSTHREADS then
      numdssthreads := MAXDSSTHREADS
    else if numdssthreads < 2 then
      numdssthreads := 2;

    l := 1;
    h := 0;
    for i := 0 to numdssthreads - 1 do
    begin
      parms[i].minx := l;
      if i = numdssthreads - 1 then
        parms[i].maxx := f_w - 1
      else
      begin
        h := h + f_w div numdssthreads;
        parms[i].maxx := h;
        l := h;
        dssthreads[i].Activate(@parms[i]);
      end;
    end;
    AM_dodrawSubSectors(@parms[numdssthreads - 1]);
    for i := 0 to numdssthreads - 2 do
      dssthreads[i].Wait;
  end
  else
  {$ENDIF}
  begin
    parms{$IFNDEF OPENGL}[0]{$ENDIF}.minx := 1;
    parms{$IFNDEF OPENGL}[0]{$ENDIF}.maxx := f_w - 1;
    AM_dodrawSubSectors(@parms{$IFNDEF OPENGL}[0]{$ENDIF});
  end;
end;

procedure AM_InitTextured;
{$IFNDEF OPENGL}
var
  i: integer;
{$ENDIF}
begin
{$IFNDEF OPENGL}
  for i := 0 to MAXDSSTHREADS - 2 do
    dssthreads[i] := TDThread.Create(@AM_dodrawSubSectors);
{$ENDIF}
end;

procedure AM_ShutDownTextured;
{$IFNDEF OPENGL}
var
  i: integer;
{$ENDIF}
begin
{$IFNDEF OPENGL}
  for i := 0 to MAXDSSTHREADS - 2 do
    dssthreads[i].Free;
{$ENDIF}
end;

end.

