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
//    Dynamic lights for OpenGL rendering (why not in software mode??)
//    LIGHTDEF lump parsing, light animation
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit r_softlights;

interface

uses
  p_mobj_h;

procedure R_MarkDLights(const mo: Pmobj_t);

procedure R_AddAdditionalLights;

procedure R_DrawLightsSingleThread;

procedure R_DrawLightsMultiThread;

var
  lightmapcolorintensity: integer = 128;
  lightwidthfactor: integer = 5;

const
  MINLIGHTWIDTHFACTOR = 0;
  DEFLIGHTWIDTHFACTOR = 5;
  MAXLIGHTWIDTHFACTOR = 10;
  MINLMCOLORSENSITIVITY = 32;
  DEFLMCOLORSENSITIVITY = 128;
  MAXLMCOLORSENSITIVITY = 256;

var
  r_uselightmaps: boolean = true;
  r_lightmaponmasked: boolean = true;
  r_lightmapfadeoutfunc: integer = 0;

const
  LIGHTMAPFADEOUT_LINEAR = 0;
  LIGHTMAPFADEOUT_CURVED = 1;
  LIGHTMAPFADEOUT_PERSIST = 2;
  LIGHTMAPFADEOUT_COSINE = 3;
  LIGHTMAPFADEOUT_SIGMOID = 4;
  NUMLIGHTMAPFADEOUTFUNCS = 5;

procedure R_InitLightTexture;

procedure R_ShutDownLightTexture;

implementation

uses
  d_delphi,
  Math,
  doomdef,
  m_fixed,
  mt_utils,
  p_local,
  p_setup,
  p_maputl,
  r_draw,
  r_dynlights,
  r_vislight,
  r_lights,
  r_main,
  r_defs,
  r_trans8,
  r_zbuffer,
  r_hires,
  tables,
  v_video,
  z_zone;

const
  LIGHTTEXTURESIZE = 128;

var
  lightexturelookup: array[0..LIGHTTEXTURESIZE - 1] of lpost_t;
  lighttexture: PLongWordArray = nil;

//
// R_InitLightTexture
//
procedure R_InitLightTexture;
var
  i, j: integer;
  dist: double;
  c: integer;

  function _sigmoid(const x: integer): integer;
  var
    f: single;
  begin
    if x <= 75 then
      result := round(100 * x / 75)
    else
    begin
      f := (x - 128) / 128 * 4.9065;
      result := ibetween(127 + trunc(164 * arctan(tanh(f / 2)) + 0.778), -255, 255);
      result := ibetween(round(sqrt(255 * 255 - sqr(255 - result))), 0, 255);
    end;
  end;

begin
  if lighttexture = nil then
    lighttexture := PLongWordArray(malloc(LIGHTTEXTURESIZE * LIGHTTEXTURESIZE * SizeOf(LongWord)));
  for i := 0 to LIGHTTEXTURESIZE - 1 do
  begin
    lightexturelookup[i].topdelta := MAXINT;
    lightexturelookup[i].length := 0;
    for j := 0 to LIGHTTEXTURESIZE - 1 do
    begin
      dist := sqrt(sqr(i - (LIGHTTEXTURESIZE shr 1)) + sqr(j - (LIGHTTEXTURESIZE shr 1)));
      if dist <= (LIGHTTEXTURESIZE shr 1) then
      begin
        inc(lightexturelookup[i].length);
        c := round(dist * 512 / LIGHTTEXTURESIZE);
        if c > 255 then
          c := 0
        else if r_lightmapfadeoutfunc = LIGHTMAPFADEOUT_LINEAR then
          c := 255 - c
        else if r_lightmapfadeoutfunc = LIGHTMAPFADEOUT_CURVED then
          c := ibetween(round(255 - c * sqrt(c) / 16), 0, 255)
        else if r_lightmapfadeoutfunc = LIGHTMAPFADEOUT_PERSIST then
          c := ibetween(round(255 - c * c / 256), 0, 255)
        else if r_lightmapfadeoutfunc = LIGHTMAPFADEOUT_SIGMOID then
          c := _sigmoid(255 - c)
        else
          c := ibetween(round(255 * cos(c / 256 * pi / 2)), 0, 255);
        lighttexture[i * LIGHTTEXTURESIZE + j] := c * lightmapcolorintensity;
        if j < lightexturelookup[i].topdelta then
          lightexturelookup[i].topdelta := j;
      end
      else
        lighttexture[i * LIGHTTEXTURESIZE + j] := 0;
    end;
  end;
end;

//
// R_ShutDownLightTexture
//
procedure R_ShutDownLightTexture;
begin
  if lighttexture <> nil then
    memfree(pointer(lighttexture), LIGHTTEXTURESIZE * LIGHTTEXTURESIZE * SizeOf(LongWord));
end;

const
  MAXLIGHTDISTANCE = 2048;
  MAXSQRLIGHTDISTANCE = MAXLIGHTDISTANCE * MAXLIGHTDISTANCE;

procedure R_MarkDLights(const mo: Pmobj_t);
var
  l: PGLDRenderLight;
  i: integer;
  dx, dy, dz: single;
  xdist, ydist, zdist: single;
  psl: Pdlsortitem_t;
  dlights: T2DNumberList;
begin
  if mo.lightvalidcount = rendervalidcount then
    exit;
  dlights := mo.state.dlights;
  if dlights = nil then
    exit;
  mo.lightvalidcount := rendervalidcount;

  xdist := (viewx - mo.x) / FRACUNIT;
  ydist := (viewy - mo.y) / FRACUNIT;
  zdist := (viewz - mo.z) / FRACUNIT;

  for i := 0 to dlights.Count - 1 do
    if (dlights.Numbers[i].num1 = mo._type) or (dlights.Numbers[i].num1 = -1) then
    begin
      l := R_GetDynamicLight(dlights.Numbers[i].num2);
      if numdlitems >= realdlitems then
      begin
        realloc(pointer(dlbuffer), realdlitems * SizeOf(dlsortitem_t), (realdlitems + 32) * SizeOf(dlsortitem_t));
        realdlitems := realdlitems + 32;
      end;

      psl := @dlbuffer[numdlitems];
      psl.l := l;
      // Convert offset coordinates from LIGHTDEF lump
      dx := xdist - l.x;
      dy := ydist - l.z;
      dz := zdist - l.y;
      psl.squaredist := dx * dx + dy * dy + dz * dz;
      if psl.squaredist < MAXSQRLIGHTDISTANCE then
      begin
        psl.x := mo.x + trunc(FRACUNIT * l.x);
        psl.y := mo.y + trunc(FRACUNIT * l.z);
        psl.z := mo.z + trunc(FRACUNIT * l.y);
        psl.radius := trunc(l.radius * FRACUNIT);
        inc(numdlitems);
      end;
    end;
end;

function RIT_AddAdditionalLights(mo: Pmobj_t): boolean;
begin
  R_MarkDLights(mo);
  // keep checking
  result := true;
end;

const
  MAXLIGHTRADIUS = 256 * FRACUNIT;

procedure R_AddAdditionalLights;
var
  x: integer;
  y: integer;
  xl: integer;
  xh: integer;
  yl: integer;
  yh: integer;
begin
  if internalblockmapformat then
  begin
    yh := MapBlockIntY(int64(viewy) + MAXLIGHTRADIUS - int64(bmaporgy));
    yl := MapBlockIntY(int64(viewy) - MAXLIGHTRADIUS - int64(bmaporgy));
    xh := MapBlockIntX(int64(viewx) + MAXLIGHTRADIUS - int64(bmaporgx));
    xl := MapBlockIntX(int64(viewx) - MAXLIGHTRADIUS - int64(bmaporgx));
  end
  else
  begin
    yh := MapBlockInt(viewy + MAXLIGHTRADIUS - bmaporgy);
    yl := MapBlockInt(viewy - MAXLIGHTRADIUS - bmaporgy);
    xh := MapBlockInt(viewx + MAXLIGHTRADIUS - bmaporgx);
    xl := MapBlockInt(viewx - MAXLIGHTRADIUS - bmaporgx);
  end;

  for y := yl to yh do
    for x := xl to xh do
      P_BlockThingsIterator(x, y, RIT_AddAdditionalLights);
end;

const
  DEPTHBUFFER_NEAR = $3FFF * FRACUNIT;
  DEPTHBUFFER_FAR = 256;

function R_GetVisLightProjection(const x, y, z: fixed_t; const radius: fixed_t;
  const color: LongWord): Pvislight_t;
var
  tr_x: fixed_t;
  tr_y: fixed_t;
  gxt: fixed_t;
  gyt: fixed_t;
  tx: fixed_t;
  tz: fixed_t;
  xscale: fixed_t;
  x1: integer;
  x2: integer;
  an: angle_t;
  dx, dy: fixed_t;
begin
  result := nil;

  // transform the origin point
  tr_x := x - viewx;
  tr_y := y - viewy;

  gxt := FixedMul(tr_x, viewcos);
  gyt := -FixedMul(tr_y, viewsin);

  tz := gxt - gyt;

  // thing is behind view plane?
  if tz <= 4 * FRACUNIT then
    exit;

  xscale := FixedDiv(projection, tz);
  if xscale > DEPTHBUFFER_NEAR then
    xscale := DEPTHBUFFER_NEAR;

  gxt := -FixedMul(tr_x, viewsin);
  gyt := FixedMul(tr_y, viewcos);
  tx := -(gyt + gxt);

  // too far off the side?
  if abs(tx) > 4 * tz then
    exit;

  // calculate edges of the shape
  tx := tx - radius;
  x1 := FixedInt(centerxfrac + FixedMul(tx, xscale));

  // off the right side?
  if x1 > viewwidth then
    exit;

  tx := tx + 2 * radius;
  x2 := FixedInt(centerxfrac + FixedMul(tx, xscale)) - 1;

  // off the left side
  if x2 < 0 then
    exit;

  // OK, we have a valid vislight
  result := R_NewVisLight;

  // store information in a vissprite
  result.scale := FixedDiv(projectiony, tz); // JVAL For correct aspect
  result.gx := x;
  result.gy := y;
  result.gz := z;
  result.texturemid := z + radius - viewz;
  result.xiscale := FixedDiv(FRACUNIT, xscale);

  if x1 <= 0 then
  begin
    result.x1 := 0;
    result.startfrac := result.xiscale * (result.x1 - x1);
  end
  else
  begin
    result.x1 := x1;
    result.startfrac := 0;
  end;
  if x2 >= viewwidth then
    result.x2 := viewwidth - 1
  else
    result.x2 := x2;


  // get depthbuffer range
  an := R_PointToAngle(x, y);
  an := an shr ANGLETOFINESHIFT;
  dx := FixedMul(radius, finecosine[an]);
  dy := FixedMul(radius, finesine[an]);

  tr_x := x - viewx + dx;
  tr_y := y - viewy + dy;

  gxt := FixedMul(tr_x, viewcos);
  gyt := -FixedMul(tr_y, viewsin);

  tz := gxt - gyt;

  if tz <= 4 * FRACUNIT then
    result.dbmin := DEPTHBUFFER_NEAR
  else
  begin
    result.dbmin := FixedDiv(projectiony, tz);
    if result.dbmin > DEPTHBUFFER_NEAR then
      result.dbmin := DEPTHBUFFER_NEAR
    else if result.dbmin < 256 then
      result.dbmin := 256;
  end;

  tr_x := x - viewx - dx;
  tr_y := y - viewy - dy;

  gxt := FixedMul(tr_x, viewcos);
  gyt := -FixedMul(tr_y, viewsin);

  tz := gxt - gyt;

  if tz <= 4 * FRACUNIT then
    result.dbmax := DEPTHBUFFER_NEAR
  else
  begin
    result.dbmax := FixedDiv(projectiony, tz);
    if result.dbmax > DEPTHBUFFER_NEAR then
      result.dbmax := DEPTHBUFFER_NEAR
    else if result.dbmax < DEPTHBUFFER_FAR then
      result.dbmax := DEPTHBUFFER_FAR;
  end;
  if result.dbmax = result.dbmin then
  begin
    result.dbmax := result.scale + DEPTHBUFFER_FAR;
    if result.scale < 2 * DEPTHBUFFER_FAR then
      result.dbmin := DEPTHBUFFER_FAR
    else
      result.dbmin := result.scale - DEPTHBUFFER_FAR;
    result.dbdmin := DEPTHBUFFER_FAR;
    result.dbdmax := DEPTHBUFFER_FAR;
  end
  else
  begin
    result.dbdmin := result.scale - result.dbmin;
    result.dbdmax := result.dbmax - result.scale;
  end;

  result.color32 := color;
end;

//
//  R_SortDlights()
//  JVAL: Sort the dynamic lights according to square distance of view
//        (note: closer light is first!)
//
procedure R_SortDlights;

  procedure qsort(l, r: Integer);
  var
    i, j: Integer;
    tmp: dlsortitem_t;
    squaredist: float;
  begin
    repeat
      i := l;
      j := r;
      squaredist := dlbuffer[(l + r) shr 1].squaredist;
      repeat
        while dlbuffer[i].squaredist < squaredist do
          inc(i);
        while dlbuffer[j].squaredist > squaredist do
          dec(j);
        if i <= j then
        begin
          tmp := dlbuffer[i];
          dlbuffer[i] := dlbuffer[j];
          dlbuffer[j] := tmp;
          inc(i);
          dec(j);
        end;
      until i > j;
      if l < j then
        qsort(l, j);
      l := i;
    until i >= r;
  end;

begin
  if numdlitems > 0 then
    qsort(0, numdlitems - 1);
end;

type
  lightparams_t = record
    lightsourcex: fixed_t;
    lightsourcey: fixed_t;
    r, g, b: byte;
    dl_iscale: fixed_t;
    dl_scale: fixed_t;
    dl_texturemid: fixed_t;
    dl_x: integer;
    dl_yl: integer;
    dl_yh: integer;
    db_min: LongWord;
    db_max: LongWord;
    db_dmin: LongWord;
    db_dmax: LongWord;
    dl_fracstep: fixed_t;
    dl_source32: PLongWordArray;
  end;
  Plightparams_t = ^lightparams_t;

const
  MAXLIGHTTHREADS = NUMEXECTHREADS;

var
  lcolumns: array[0..MAXLIGHTTHREADS - 1] of lightparams_t;

type
  drawcolumnlightmap_t = procedure(const parms: Plightparams_t);

var
  drawcolumnlightmap: drawcolumnlightmap_t;

type
  precalc32op1_t = array[0..255] of integer;
  precalc32op1_p = ^precalc32op1_t;

var
  precalc32op1A: array[0..255] of precalc32op1_p;
  precalc32op1_calced: boolean = false;

procedure calc_precalc32op1;
var
  i, j: integer;
begin
  if precalc32op1_calced then
    exit;

  precalc32op1A[0] := nil;
  for j := 1 to 255 do
  begin
    precalc32op1A[j] := Z_Malloc(SizeOf(precalc32op1_t), PU_STATIC, nil);
    for i := 0 to 255 do
      precalc32op1A[j][i] := (255 - i) * j;
  end;

  precalc32op1_calced := true;
end;

type
  fastzbuf_t = record
    db: Pzbufferitem_t;
    next: integer;
  end;
  Pfastzbuf_t = ^fastzbuf_t;

function R_FastZBufferAt(const x, y: integer; const pfdb: Pfastzbuf_t): Pzbufferitem_t;
begin
  if pfdb.next >= y then
  begin
    result := pfdb.db;
    exit;
  end;
  result := R_ZBufferAt(x, y);
  pfdb.db := result;
  if result.seg <> nil then
    pfdb.next := result.stop
  else
    pfdb.next := y;
end;

procedure R_DrawColumnLightmap8(const parms: Plightparams_t);
var
  count, x, y: integer;
  frac: fixed_t;
  fracstep: fixed_t;
  fastzbuf: fastzbuf_t;
  db: Pzbufferitem_t;
  depth: LongWord;
  dbmin, dbmax: LongWord;
  dbdmin, dbdmax: LongWord;
  factor: fixed_t;
  dfactor: fixed_t;
  scale: fixed_t;
  dls: fixed_t;
  seg: Pseg_t;
  skip: boolean;
  sameseg: boolean;
  dest: PByte;
  source32: PLongWordArray;
  pitch: integer;
  r1, g1, b1: LongWord;
  r, g, b: LongWord;
  c: LongWord;
  rr, gg, bb: integer;
  tbl_r, tbl_g, tbl_b: precalc32op1_p;
begin
  count := parms.dl_yh - parms.dl_yl;

  if count < 0 then
    exit;

  frac := parms.dl_texturemid + (parms.dl_yl - centery) * parms.dl_iscale;
  fracstep := parms.dl_fracstep;

  dbmin := parms.db_min;
  dbmax := parms.db_max;
  dbdmin := parms.db_dmin;
  dbdmax := parms.db_dmax;
  r := parms.r;
  g := parms.g;
  b := parms.b;
  x := parms.dl_x;
  scale := parms.dl_scale;
  seg := nil;
  dfactor := 0;
  skip := false;
  sameseg := false;
  source32 := parms.dl_source32;

  tbl_r := precalc32op1A[parms.r];
  tbl_g := precalc32op1A[parms.g];
  tbl_b := precalc32op1A[parms.b];

  dest := @((ylookup[parms.dl_yl]^)[columnofs[x]]);
  pitch := SCREENWIDTH;
  fastzbuf.next := parms.dl_yl - 1;
  for y := parms.dl_yl to parms.dl_yh do
  begin
    dls := source32[(LongWord(frac) shr FRACBITS) and (LIGHTTEXTURESIZE - 1)];
    if dls <> 0 then
    begin
      db := R_FastZBufferAt(x, y, @fastzbuf);
      depth := db.depth;
      if (depth >= dbmin) and (depth <= dbmax) then
      begin
        if seg <> db.seg then
        begin
          sameseg := (seg = db.seg) and (seg <> nil);
          seg := db.seg;
          if seg <> nil then
            skip := R_PointOnSegSide(parms.lightsourcex, parms.lightsourcey, seg)
          else
            skip := false;
        end;

        if not skip then
        begin
          if not sameseg then
          begin
            dfactor := depth - scale;
            if dfactor < 0 then
              dfactor := FRACUNIT - FixedDiv(-dfactor, dbdmin)
            else
              dfactor := FRACUNIT - FixedDiv(dfactor, dbdmax);
          end;

          if dfactor > 0 then
          begin
            factor := FixedMulDiv256(dls, dfactor);

            if factor > 0 then
            begin
              {$IFDEF DOOM_OR_STRIFE}
              c := cvideopal[dest^];
              {$ELSE}
              c := curpal[dest^];
              {$ENDIF}

              rr := (c shr 16) and $FF;
              if tbl_r <> nil then
                rr := rr + (tbl_r[rr] * factor) shr 16;
              rr := rr shr FASTTABLESHIFT;

              gg := (c shr 8) and $FF;
              if tbl_g <> nil then
                gg := gg + (tbl_g[gg] * factor) shr 16;
              gg := gg shr FASTTABLESHIFT;

              bb := (c) and $FF;
              if tbl_b <> nil then
                bb := bb + (tbl_b[bb] * factor) shr 16;
              bb := bb shr FASTTABLESHIFT;

              dest^ := approxcolorindexarray[rr shl (16 - FASTTABLESHIFT - FASTTABLESHIFT) + gg shl (8 - FASTTABLESHIFT) + bb];
            end;


        {  Slower code:
            factor := FixedMul(dls, dfactor);
            if factor > 0 then
            begin
              r1 := FixedMul(r, factor);
              g1 := FixedMul(g, factor);
              b1 := FixedMul(b, factor);
              dest^ := R_FastApproxColorIndex(R_ColorLightAdd(curpal[dest^], r1, g1, b1));
            end; }

          end;
        end;
      end;
    end;
    inc(dest, pitch);
    inc(frac, fracstep);
  end;
end;

procedure R_DrawColumnLightmap32(const parms: Plightparams_t);
var
  count, x, y: integer;
  frac: fixed_t;
  fracstep: fixed_t;
  fastzbuf: fastzbuf_t;
  db: Pzbufferitem_t;
  depth: LongWord;
  dbmin, dbmax: LongWord;
  dbdmin, dbdmax: LongWord;
  factor: fixed_t;
  dfactor: fixed_t;
  scale: fixed_t;
  dls: fixed_t;
  seg: Pseg_t;
  skip: boolean;
  sameseg: boolean;
  destb: PByte;
  source32: PLongWordArray;
  pitch: integer;
  tbl_r, tbl_g, tbl_b: precalc32op1_p;
begin
  count := parms.dl_yh - parms.dl_yl;

  if count < 0 then
    exit;

  frac := parms.dl_texturemid + (parms.dl_yl - centery) * parms.dl_iscale;
  fracstep := parms.dl_fracstep;

  dbmin := parms.db_min;
  dbmax := parms.db_max;
  dbdmin := parms.db_dmin;
  dbdmax := parms.db_dmax;
  x := parms.dl_x;
  scale := parms.dl_scale;
  seg := nil;
  dfactor := 0;
  skip := false;
  sameseg := false;
  source32 := parms.dl_source32;

  tbl_r := precalc32op1A[parms.r];
  tbl_g := precalc32op1A[parms.g];
  tbl_b := precalc32op1A[parms.b];

  destb := @((ylookupl[parms.dl_yl]^)[columnofs[x]]);
  pitch := SCREENWIDTH * SizeOf(LongWord);
  fastzbuf.next := parms.dl_yl - 1;
  for y := parms.dl_yl to parms.dl_yh do
  begin
    dls := source32[(LongWord(frac) shr FRACBITS) and (LIGHTTEXTURESIZE - 1)];
    if dls <> 0 then
    begin
      db := R_FastZBufferAt(x, y, @fastzbuf);
      depth := db.depth;
      if (depth >= dbmin) and (depth <= dbmax) then
      begin
        if seg <> db.seg then
        begin
          sameseg := (seg = db.seg) and (seg <> nil);
          seg := db.seg;
          if seg <> nil then
            skip := R_PointOnSegSide(parms.lightsourcex, parms.lightsourcey, seg)
          else
            skip := false;
        end;

        if not skip then
        begin
          if not sameseg then
          begin
            dfactor := depth - scale;
            if dfactor < 0 then
              dfactor := FRACUNIT - FixedDiv(-dfactor, dbdmin)
            else
              dfactor := FRACUNIT - FixedDiv(dfactor, dbdmax);
          end;

          if dfactor > 0 then
          begin
            factor := FixedMulDiv256(dls, dfactor);

            if factor > 0 then
            begin
              if tbl_b <> nil then
                destb^ := destb^ + (tbl_b[destb^] * factor) shr 16;
              inc(destb);

              if tbl_g <> nil then
                destb^ := destb^ + (tbl_g[destb^] * factor) shr 16;

              if tbl_r <> nil then
              begin
                inc(destb);
                destb^ := destb^ + (tbl_r[destb^] * factor) shr 16;
                dec(destb, 2);
              end
              else
                dec(destb);
            end;

          end;
        end;
      end;
    end;
    inc(destb, pitch);
    inc(frac, fracstep);
  end;
end;

procedure R_DrawVisLight(const psl: Pdlsortitem_t; const threadid, numlthreads: integer);
var
  frac: fixed_t;
  fracstep: fixed_t;
  vis: Pvislight_t;
  w: float;
  spryscale: fixed_t;
  ltopscreen: fixed_t;
  texturecolumn: integer;
  ltopdelta: integer;
  llength: integer;
  topscreen: int64;
  bottomscreen: int64;
  lcolumn: Plightparams_t;
begin
  vis := psl.vis;
  w := 2 * psl.l.radius * lightwidthfactor / DEFLIGHTWIDTHFACTOR;
  frac := trunc(vis.startfrac * LIGHTTEXTURESIZE / w);
  fracstep := trunc(vis.xiscale * LIGHTTEXTURESIZE / w);
  spryscale := trunc(vis.scale * w / LIGHTTEXTURESIZE);
  lcolumn := @lcolumns[threadid];
  lcolumn.lightsourcex := psl.x;
  lcolumn.lightsourcey := psl.y;
  lcolumn.dl_iscale := FixedDivEx(FRACUNIT, spryscale);
  lcolumn.dl_fracstep := FixedDivEx(FRACUNIT, spryscale); //trunc(vis.scale * w / LIGHTTEXTURESIZE));
  lcolumn.dl_scale := vis.scale;
  ltopscreen := centeryfrac - FixedMul(vis.texturemid, vis.scale);

  lcolumn.db_min := vis.dbmin;
  lcolumn.db_max := vis.dbmax;
  lcolumn.db_dmin := vis.dbdmin;
  lcolumn.db_dmax := vis.dbdmax;

  lcolumn.r := (vis.color32 shr 16) and $FF;
  lcolumn.g := (vis.color32 shr 8) and $FF;
  lcolumn.b := vis.color32 and $FF;

  lcolumn.dl_x := vis.x1;

  while lcolumn.dl_x <= vis.x2 do
  begin
    if lcolumn.dl_x mod numlthreads = threadid then
    begin
      texturecolumn := (LongWord(frac) shr FRACBITS) and (LIGHTTEXTURESIZE - 1);
      ltopdelta := lightexturelookup[texturecolumn].topdelta;
      llength := lightexturelookup[texturecolumn].length;
      lcolumn.dl_source32 := @lighttexture[texturecolumn * LIGHTTEXTURESIZE + ltopdelta];
      topscreen := ltopscreen + int64(spryscale) * int64(ltopdelta);
      bottomscreen := topscreen + int64(spryscale) * int64(llength);

      lcolumn.dl_yl := FixedInt64(topscreen + (FRACUNIT - 1));
      lcolumn.dl_yh := FixedInt64(bottomscreen - 1);
      lcolumn.dl_texturemid := (centery - lcolumn.dl_yl) * lcolumn.dl_iscale;

      if lcolumn.dl_yh >= viewheight then
        lcolumn.dl_yh := viewheight - 1;
      if lcolumn.dl_yl < 0 then
        lcolumn.dl_yl := 0;

      drawcolumnlightmap(lcolumn);
    end;
    frac := frac + fracstep;
    inc(lcolumn.dl_x);
  end;
end;

var
  old_lightmapcolorintensity: integer = -1;
  old_lightwidthfactor: integer = -1;
  old_lightmapfadeoutfunc: integer = -1;

procedure R_SetUpLightEffects;
begin
  lightmapcolorintensity := ibetween(lightmapcolorintensity, MINLMCOLORSENSITIVITY, MAXLMCOLORSENSITIVITY);
  lightwidthfactor := ibetween(lightwidthfactor, MINLIGHTWIDTHFACTOR, MAXLIGHTWIDTHFACTOR);
  r_lightmapfadeoutfunc := ibetween(r_lightmapfadeoutfunc, 0, NUMLIGHTMAPFADEOUTFUNCS - 1);
  if (old_lightmapcolorintensity <> lightmapcolorintensity) or (old_lightmapfadeoutfunc <> r_lightmapfadeoutfunc) then
  begin
    old_lightmapcolorintensity := lightmapcolorintensity;
    old_lightmapfadeoutfunc := r_lightmapfadeoutfunc;
    R_InitLightTexture;
  end;
  calc_precalc32op1;
  if videomode = vm32bit then
    drawcolumnlightmap := R_DrawColumnLightmap32
  else
    drawcolumnlightmap := R_DrawColumnLightmap8;
end;

function f2b(const ff: float): byte;
var
  ii: integer;
begin
  ii := trunc(ff * 256);
  if ii <= 0 then
    result := 0
  else if ii >= 255 then
    result := 255
  else
    result := ii;
end;

procedure R_GetVisLightProjections;
var
  i: integer;
  psl: Pdlsortitem_t;
  c: LongWord;
begin
  if fixedcolormapnum = INVERSECOLORMAP then
  begin
    for i := numdlitems - 1 downto 0 do
    begin
      psl := @dlbuffer[i];
      psl.vis := R_GetVisLightProjection(psl.x, psl.y, psl.z, psl.radius * lightwidthfactor div DEFLIGHTWIDTHFACTOR, $FFFFFF);
    end;
  end
  else
  begin
    for i := numdlitems - 1 downto 0 do
    begin
      psl := @dlbuffer[i];
      c := f2b(psl.l.b) + f2b(psl.l.g) shl 8 + f2b(psl.l.r) shl 16;
      psl.vis := R_GetVisLightProjection(psl.x, psl.y, psl.z, psl.radius * lightwidthfactor div DEFLIGHTWIDTHFACTOR, c);
    end;
  end;
end;

procedure R_DrawLightSingleThread(const psl: Pdlsortitem_t);
begin
  if psl.vis <> nil then
    R_DrawVisLight(psl, 0, 1);
end;

procedure R_DrawLightsSingleThread;
var
  i: integer;
begin
  R_SetUpLightEffects;
  R_SortDlights;
  R_GetVisLightProjections;
  for i := 0 to numdlitems - 1 do
    R_DrawLightSingleThread(@dlbuffer[i]);
end;

procedure R_DrawLightMultiThread(const psl: Pdlsortitem_t; const threadid, numlthreads: integer);
begin
  if psl.vis <> nil then
    R_DrawVisLight(psl, threadid, numlthreads);
end;

function _DrawLightsMultiThread_thr(p: iterator_p): integer; stdcall;
var
  i: integer;
begin
  for i := 0 to numdlitems - 1 do
    R_DrawLightMultiThread(@dlbuffer[i], p.idx, p.numidxs);
  result := 0;
end;

procedure R_DrawLightsMultiThread;
begin
  R_SetUpLightEffects;
  R_SortDlights;
  R_GetVisLightProjections;
  MT_Iterate(@_DrawLightsMultiThread_thr, nil);
end;

end.

