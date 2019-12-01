//------------------------------------------------------------------------------
//
//  DelphiDoom: A modified and improved DOOM engine for Windows
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 2004-2016 by Jim Valavanis
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
//  E-Mail: jimmyvalavanis@yahoo.gr
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
  i_threads,
  i_system,
{$IFNDEF OPENGL}
  r_hires,
  r_draw,
  r_trans8,
{$ELSE}
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

  drawsegfunc_t = procedure(const xx, yy, cc: integer; const amcolormap: pointer);

var
  drawsegfunc: drawsegfunc_t;

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
{$ENDIF}

procedure AM_DrawSeg32(xx, yy, cc: integer; const amcolormap: {$IFNDEF OPENGL}PLongWordArray{$ELSE}PByteArray{$ENDIF});
begin
// JVAL Clip line if in overlay mode
{$IFNDEF OPENGL}
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
{$ELSE}
  fb32[yy * f_w + xx] := videopal[amcolormap[cc]];
{$ENDIF}
end;

procedure AM_DrawSeg32Transparent(xx, yy, cc: integer; const amcolormap: {$IFNDEF OPENGL}PLongWordArray{$ELSE}PByteArray{$ENDIF});
{$IFNDEF OPENGL}
var
  pl: PLongWord;
{$ENDIF}
begin
// JVAL Clip line if in overlay mode
{$IFNDEF OPENGL}
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
{$ELSE}
  fb32[yy * f_w + xx] := videopal[amcolormap[cc]] and $80000000;
{$ENDIF}
{$IFNDEF OPENGL}
  pl := @fb32[yy * f_w + xx];
  pl^ := R_ColorMean(pl^, amcolormap[cc]);
{$ENDIF}
end;

type
  seg_ap3 = array[0..2] of Pseg_t;

var
  pla: angle_t;
  plx, ply: fixed_t;
  amsin, amcos: fixed_t;
  f_mx: integer;

procedure AM_DecodeUV(var xx, yy: integer);
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
  yy := 63 - (yy div FRACUNIT) and 63;
  xx := (xx div FRACUNIT) and 63;
end;

procedure AM_DrawTexturedTriangle(const lst: seg_ap3; const lump: integer; const aminx, amaxx: integer; const amcolormap: pointer);
var
  data: PByteArray;

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
          AM_DecodeUV(du, dv);
          drawsegfunc(i, j, data[du + dv * 64], amcolormap);
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
          AM_DecodeUV(du, dv);
          drawsegfunc(i, j, data[du + dv * 64], amcolormap);
          yy := yy - scale_ftom;
        end;
      end;
      xx := xx - scale_ftom;
    end;
  end;

var
  v1, v2, v3, v4: drawpoint_t;
  t: drawtriangle_t;
  v: Pvertex_t;
  i: integer;
begin
  v := lst[0].v1;
  t[0].x:= v.x;
  t[0].y := v.y;
  v := lst[1].v1;
  t[1].x := v.x;
  t[1].y := v.y;
  v := lst[2].v1;
  t[2].x := v.x;
  t[2].y := v.y;

  for i := 0 to 2 do
  begin
    if allowautomaprotate then
      AM_rotate(@t[i].x, @t[i].y, ANG90 - plr.mo.angle, plr.mo.x, plr.mo.y);
    t[i].x := CXMTOF(t[i].x);
    t[i].y := CYMTOF(t[i].y);
  end;

  if t[1].x < t[0].x then
  begin
    v1 := t[1];
    t[1] := t[0];
    t[0] := v1;
  end;

  if t[2].x < t[1].x then
  begin
    v1 := t[2];
    t[2] := t[1];
    t[1] := v1;
  end;

  if t[1].x < t[0].x then
  begin
    v1 := t[1];
    t[1] := t[0];
    t[0] := v1;
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
end;

procedure AM_setSubSectorDrawFuncs;
begin
{$IFDEF OPENGL}
  if amstate = am_overlay then
    drawsegfunc := @AM_DrawSeg32Transparent
  else
    drawsegfunc := @AM_DrawSeg32;
{$ELSE}
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

function AM_dodrawSubSectors(const parms: dssparams_p): integer; stdcall;
var
  i, j: integer;
  ssector: Psubsector_t;
  seg: Pseg_t;
  lst: seg_ap3;
  drawit: boolean;
  wasdrawned: boolean;
  lump: integer;
  cmap: integer;
  amcolormap: pointer;
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
      cmap := (ssector.sector.lightlevel) div 8;
      if cmap > 31 then
        cmap := 31
      else if cmap < 0 then
        cmap := 0;
      {$IFDEF OPENGL}
      amcolormap := @colormaps[(31 - cmap) * 256];
      {$ELSE}
      if videomode = vm32bit then
        amcolormap := @colormaps32[(31 - cmap) * 256]
      else
        amcolormap := @colormaps[(31 - cmap) * 256];
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

        AM_DrawTexturedTriangle(lst, lump, parms.minx, parms.maxx, amcolormap);
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
        cmap := (ssector.sector.lightlevel) div 8;
        if cmap > 31 then
          cmap := 31
        else if cmap < 0 then
          cmap := 0;
        {$IFDEF OPENGL}
        amcolormap := @colormaps[(31 - cmap) * 256];
        {$ELSE}
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
          AM_DrawTexturedTriangle(lst, lump, parms.minx, parms.maxx, amcolormap);
        end;
      end;

    end;

  result := 0;
end;

const
  MAXDSSTHREADS = 14;

var
  dssthreads: array[0..MAXDSSTHREADS - 2] of TDThread;
  numdssthreads: integer = 1;

procedure AM_drawSubSectors;
var
  parms: array[0..MAXDSSTHREADS - 1] of dssparams_t;
  i: integer;
  l, h: integer;
begin
  Inc(amvalidcount);
  pla := plr.mo.angle - ANG90;
  plx := plr.mo.x;
  ply := plr.mo.y;
  amsin := finesine[pla shr ANGLETOFINESHIFT];
  amcos := finecosine[pla shr ANGLETOFINESHIFT];

  f_mx := f_h;
  if amstate = am_overlay then
    if screenblocks >= 10 then
      f_mx := {$IFDEF OPENGL}V_GetScreenWidth(SCN_FG){$ELSE}SCREENWIDTH{$ENDIF};

  if usemultithread then
  begin
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
  begin
    parms[0].minx := 1;
    parms[0].maxx := f_w - 1;
    AM_dodrawSubSectors(@parms[0]);
  end;
end;

procedure AM_InitTextured;
var
  i: integer;
begin
  for i := 0 to MAXDSSTHREADS - 2 do
    dssthreads[i] := TDThread.Create(@AM_dodrawSubSectors);
end;

procedure AM_ShutDownTextured;
var
  i: integer;
begin
  for i := 0 to MAXDSSTHREADS - 2 do
    dssthreads[i].Free;
end;

end.

