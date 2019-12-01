//------------------------------------------------------------------------------
//
//  DelphiHexen: A modified and improved Hexen port for Windows
//  based on original Linux Doom as published by "id Software", on
//  Hexen source as published by "Raven" software and DelphiDoom
//  as published by Jim Valavanis.
//  Copyright (C) 2004-2019 by Jim Valavanis
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
//  Foundation, inc., 59 Temple Place - Suite 330, Boston, MA
//  02111-1307, USA.
//
// DESCRIPTION:
//  Refresh, visplane stuff (floor, ceilings).
//  Here is a core component: drawing the floors and ceilings,
//   while maintaining a per column clipping list only.
//  Moreover, the sky areas have to be determined.
//
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit r_plane;

interface

uses
  d_delphi,
  doomdef,
  m_fixed,
  r_data,
  r_defs,
  r_visplanes;  // JVAL: 3d Floors

procedure R_InitPlanes;

procedure R_ClearPlanes;

{$IFNDEF OPENGL}
type
  mapplanefunc_t = procedure(const y: integer; const x1, x2: integer);
                                        
procedure R_MapPlane(const y: integer; const x1, x2: integer);

procedure R_MakeSpans(x, t1, b1, t2, b2: integer; const func: mapplanefunc_t);

procedure R_DrawPlanes;

procedure R_DoDrawPlane(const pl: Pvisplane_t); // JVAL: 3d Floors
{$ENDIF}
procedure R_CalcPlaneOffsets(const pl: Pvisplane_t);

function R_FindPlane(height: fixed_t; picnum: integer; lightlevel: integer;
  special: integer; flags: LongWord; const floor_or_ceiling: boolean;
  {$IFNDEF OPENGL}slope: Pvisslope_t; {$ENDIF} slopeSID: integer = -1): Pvisplane_t;

{$IFNDEF OPENGL}
function R_CheckPlane(pl: Pvisplane_t; start: integer; stop: integer): Pvisplane_t;
{$ENDIF}

{$IFNDEF OPENGL}
var
//
// Clip values are the solid pixel bounding the range.
//  floorclip starts out SCREENHEIGHT
//  ceilingclip starts out -1
//
  floorclip: packed array[0..MAXWIDTH - 1] of smallint;
  ceilingclip: packed array[0..MAXWIDTH - 1] of smallint;
{$ENDIF}

var
  floorplane: Pvisplane_t;
  ceilingplane: Pvisplane_t;

//
// opening
//

//
//https://www.doomworld.com/vb/source-ports/85967-reasonable-static-limit-for-maxopenings/
//
const
  MAXOPENINGS = MAXWIDTH * MAXHEIGHT;

var
  openings: packed array[0..MAXOPENINGS - 1] of smallint;
  lastopening: integer;

{$IFNDEF OPENGL}
  yslope: array[0..MAXHEIGHT - 1] of fixed_t;
  distscale: array[0..MAXWIDTH - 1] of fixed_t;
{$ENDIF}

{$IFNDEF OPENGL}
function R_NewVisPlane: Pvisplane_t;  // JVAL: 3d Floors
{$ENDIF}

// JVAL: 3d Floors -> moved to interface
// Here comes the obnoxious "visplane".
const
// JVAL - Note about visplanes:
//   Top and Bottom arrays (of visplane_t struct) are now
//   allocated dynamically (using zone memory)
//   Use -zone cmdline param to specify more zone memory allocation
//   if out of memory.
//   See also R_NewVisPlane()
// Now maximum visplanes are 64K (originally 128)
  MAXVISPLANES = $10000;

var
  visplanes: array[0..MAXVISPLANES - 1] of visplane_t;
  lastvisplane: integer;

// JVAL: Slopes - Move to interface
var
  // JVAL: for scrolling textures
  ds_xoffset: fixed_t;    // JVAL SOS
  ds_yoffset: fixed_t;

{$IFNDEF OPENGL}
//
// texture mapping
//
  planezlight: PBytePArray;
  planeheight: fixed_t;
{$ENDIF}

implementation

uses
  doomstat,
  d_player,
  tables,
  i_system,
  p_tick,
  r_sky,
  r_main,
  r_things,
  r_draw,
{$IFNDEF OPENGL}
  r_patch,
  r_ripple,
  r_span,
  r_span32,
  r_column,
  r_hires,
  r_cache_walls,
  r_fake3d,
  r_depthbuffer,
  r_3dfloors, // JVAL: 3d Floors
  r_slopes, // JVAL: Slopes
{$ENDIF}
  z_zone,
  w_wad;


//
// spanstart holds the start of a plane span
// initialized to 0 at start
//
{$IFNDEF OPENGL}
var
  spanstart: array[0..MAXHEIGHT - 1] of integer;
  cachedheight: array[0..MAXHEIGHT - 1] of fixed_t;
  cacheddistance: array[0..MAXHEIGHT -1] of fixed_t;
  cachedxstep: array[0..MAXHEIGHT - 1] of fixed_t;
  cachedystep: array[0..MAXHEIGHT - 1] of fixed_t;
{$ENDIF}


//
// R_InitPlanes
// Only at game startup.
//
procedure R_InitPlanes;
begin
  // Doh!
end;

//
// R_MapPlane
//
// Uses global vars:
//  planeheight
//  ds_source
//  viewx
//  viewy
//
// BASIC PRIMITIVE
//
{$IFNDEF OPENGL}
procedure R_MapPlane(const y: integer; const x1, x2: integer);
var
  distance: fixed_t;
  index: LongWord;
  ncolornum: integer;
  slope: double;
begin
  if x2 - x1 < 0 then
    exit;

  if y >= viewheight then
    exit;

  if y = centery then
    exit;

  if usefake3d and zaxisshift then
    if fake3dspanpresent <> nil then
      if not fake3dspanpresent[y] then
        Exit;

  if planeheight <> cachedheight[y] then
  begin
    cachedheight[y] := planeheight;
    cacheddistance[y] := FixedMul(planeheight, yslope[y]);
    distance := cacheddistance[y];
    slope := (planeheight / abs(centery - y)) * planerelativeaspect;
    ds_xstep := round(dviewsin * slope);
    ds_ystep := round(dviewcos * slope);
    cachedxstep[y] := ds_xstep;
    cachedystep[y] := ds_ystep;
  end
  else
  begin
    distance := cacheddistance[y];
    ds_xstep := cachedxstep[y];
    ds_ystep := cachedystep[y];
  end;

  ds_xfrac :=  viewx + FixedMul(viewcos, distance) + ds_xoffset + (x1 - centerx) * ds_xstep;
  ds_yfrac := -viewy - FixedMul(viewsin, distance) + ds_yoffset + (x1 - centerx) * ds_ystep;

  if fixedcolormap <> nil then
  begin
    ds_colormap := fixedcolormap;
    if videomode = vm32bit then
    begin
      ds_colormap32 := R_GetColormap32(ds_colormap);
      if fixedcolormapnum = INVERSECOLORMAP then
        ds_lightlevel := -1  // Negative value -> Use colormaps
      else
        ds_lightlevel := R_GetColormapLightLevel(ds_colormap);
    end;
  end
  else
  begin
    index := _SHR(distance, LIGHTZSHIFT);

    if index >= MAXLIGHTZ then
      index := MAXLIGHTZ - 1;

    ds_colormap := planezlight[index];
    if videomode = vm32bit then
    begin
      ds_colormap32 := R_GetColormap32(ds_colormap);
      if not forcecolormaps then
      begin
         ncolornum := _SHR(distance, HLL_ZDISTANCESHIFT);
         if ncolornum >= HLL_MAXLIGHTZ then
          ncolornum := HLL_MAXLIGHTZ - 1;
        ds_lightlevel := zlightlevels[ds_llzindex, ncolornum];
      end
      else
      begin
        ds_lightlevel := R_GetColormapLightLevel(ds_colormap);
      end;
    end;
  end;

  ds_y := y;
  ds_x1 := x1;
  ds_x2 := x2;

  // high or low detail
  spanfunc;

  // JVAL: 3d Floors
  if depthbufferactive then
  begin
    if y = centery then
      db_distance := 0
    else
      db_distance := Round(FRACUNIT / (planeheight / abs(centery - y)) * FRACUNIT);
    spandepthbufferproc;
  end;
end;
{$ENDIF}

// JVAL: Visplane hash
const
  VISPLANEHASHSIZE = MAXVISPLANES;
  VISPLANEHASHOVER = 10;

var
  visplanehash: array[0..VISPLANEHASHSIZE + VISPLANEHASHOVER - 1] of LongWord;

//
// R_ClearPlanes
// At begining of frame.
//
procedure R_ClearPlanes;
{$IFNDEF OPENGL}
type
  two_smallints_t = record
    sm1, sm2: SmallInt;
  end;
var
  vv: integer;
  ff, cc: two_smallints_t;
{$ENDIF}
begin
{$IFNDEF OPENGL}
  // opening / clipping determination
  ff.sm1 := viewheight;
  ff.sm2 := viewheight;
  cc.sm1 := -1;
  cc.sm2 := -1;
  vv := PInteger(@ff)^;
  memseti(@floorclip, vv, viewwidth div 2);
  vv := PInteger(@cc)^;
  memseti(@ceilingclip, vv, viewwidth div 2);

  if Odd(viewwidth) then // JVAL: This shouldn't happen
  begin
    floorclip[viewwidth - 1] := viewheight;
    ceilingclip[viewwidth - 1] := -1;
  end;

  ZeroMemory(@visplanehash, SizeOf(visplanehash));

  lastvisplane := 0;
  lastvisplane3d := 0;  // JVAL: 3d Floors
  lastvisslope := 0;    // JVAL: Slopes
  lastopening := 0;

  // texture calculation
  ZeroMemory(@cachedheight, SizeOf(cachedheight));
{$ENDIF}  // JVAL: 3d Floors
end;

//
// R_NewVisPlane
//
// JVAL
//   Create a new visplane
//   Uses zone memory to allocate top and bottom arrays
//
function R_NewVisPlane: Pvisplane_t;
begin
  if lastvisplane > maxvisplane then
  begin
    {$IFNDEF OPENGL}
    visplanes[lastvisplane].top := Pvisindex_tArray(
      Z_Malloc((SCREENWIDTH + 2) * SizeOf(visindex_t), PU_LEVEL, nil));
    visplanes[lastvisplane].bottom := Pvisindex_tArray(
      Z_Malloc((SCREENWIDTH + 2) * SizeOf(visindex_t), PU_LEVEL, nil));
    {$ENDIF}
    maxvisplane := lastvisplane;
  end;

  result := @visplanes[lastvisplane];
  inc(lastvisplane);
end;

//
// R_VisplaneHash
//
function R_VisplaneHash(height: fixed_t; picnum: integer; lightlevel: integer;
  special: integer; flags: LongWord; slopeSID: integer): LongWord;
begin
  result := ((((LongWord(flags) * 3 +
                LongWord(special)) * 1296727 +
                LongWord(lightlevel)) * 1 +
                LongWord(height)) * 233 +
                LongWord(picnum)) * 3 +
                LongWord(height div FRACUNIT) +
                LongWord(height and (FRACUNIT - 1));
  result := result + LongWord(slopeSID + 1) * 7;  // JVAL: Slopes
  result := result and (VISPLANEHASHSIZE - 1);
end;

//
// R_FindPlane
//
function R_FindPlane(height: fixed_t; picnum: integer; lightlevel: integer;
  special: integer; flags: LongWord; const floor_or_ceiling: boolean;
  {$IFNDEF OPENGL}slope: Pvisslope_t; {$ENDIF} slopeSID: integer = -1): Pvisplane_t;
var
  check: integer;
  hash: LongWord;
  p: LongWord;
begin
  if special < 150 then
  begin // Don't let low specials affect search
    special := 0;
  end;

  if picnum = skyflatnum then
  begin
    if floor_or_ceiling then
      height := 1  // all skys map together
    else
      height := 0; // all skys map together
    lightlevel := 0;
    flags := flags and not SRF_SLOPED; // JVAL: Sloped surface do not have sky (why not ?) -> Just copy the sky code to R_DoDrawSlope from R_DoDrawPlane
    slopeSID := -1; // JVAL: Slopes
  end;

  hash := R_VisplaneHash(height, picnum, lightlevel, special, flags, slopeSID);
  check := hash;
  while check < hash + VISPLANEHASHOVER do
  begin
    p := visplanehash[check];
    if p = 0 then
    begin
      result := R_NewVisPlane;  // JVAL: 3d Floors

      result.height := height;
      result.picnum := picnum;
      result.lightlevel := lightlevel;
      result.minx := viewwidth;
      result.maxx := -1;
      result.special := special;
      result.renderflags := flags;
      result.slopeSID := slopeSID;  // JVAL: Slopes
      {$IFNDEF OPENGL}
      result.slope := slope;  // JVAL: Slopes
      memset(@result.top[-1], iVISEND, (2 + SCREENWIDTH) * SizeOf(visindex_t));
      {$ENDIF}

      visplanehash[check] := lastvisplane;
      exit;
    end;
    Dec(p);
    // JVAL: should not happen
    if p >= lastvisplane then
      break;
    result := @visplanes[p];
    if (height = result.height) and
       (picnum = result.picnum) and
       (special = result.special) and
       (lightlevel = result.lightlevel) and
       (slopeSID = result.slopeSID) and // JVAL: Slopes
       (flags = result.renderflags) then
      exit;
    Inc(check);
  end;

  check := 0;
  result := @visplanes[0];
  while check < lastvisplane do
  begin
    if (height = result.height) and
       (picnum = result.picnum) and
       (special = result.special) and
       (lightlevel = result.lightlevel) and
       (slopeSID = result.slopeSID) and // JVAL: Slopes
       (flags = result.renderflags) then
      break;
    inc(check);
    inc(result);
  end;

  if check < lastvisplane then
  begin
    exit;
  end;

  if lastvisplane = MAXVISPLANES then
    I_Error('R_FindPlane(): no more visplanes');

  R_NewVisPlane;

  result.height := height;
  result.picnum := picnum;
  result.lightlevel := lightlevel;
  result.special := special;
  result.minx := viewwidth;
  result.maxx := -1;
  result.renderflags := flags;
  result.slopeSID := slopeSID;  // JVAL: Slopes
  {$IFNDEF OPENGL}
  result.slope := slope;  // JVAL: Slopes
  memset(@result.top[-1], iVISEND, (2 + SCREENWIDTH) * SizeOf(visindex_t));
  {$ENDIF}

  check := hash;
  while check < hash + VISPLANEHASHOVER do
  begin
    if visplanehash[check] = 0 then
    begin
      visplanehash[check] := lastvisplane;
      Break;
    end;
    Inc(check);
  end;

end;

//
// R_CheckPlane
//
{$IFNDEF OPENGL}
function R_CheckPlane(pl: Pvisplane_t; start: integer; stop: integer): Pvisplane_t;
var
  intrl: integer;
  intrh: integer;
  unionl: integer;
  unionh: integer;
  x: integer;
  pll: Pvisplane_t;
begin
  if start < pl.minx then
  begin
    intrl := pl.minx;
    unionl := start;
  end
  else
  begin
    unionl := pl.minx;
    intrl := start;
  end;

  if stop > pl.maxx then
  begin
    intrh := pl.maxx;
    unionh := stop;
  end
  else
  begin
    unionh := pl.maxx;
    intrh := stop;
  end;

  x := intrl;
  while x <= intrh do
  begin
    if pl.top[x] <> VISEND then
      break
    else
      inc(x);
  end;

  if x > intrh then
  begin
    pl.minx := unionl;
    pl.maxx := unionh;

    // use the same one
    result := pl;
    exit;
  end;

  // make a new visplane

  if lastvisplane = MAXVISPLANES then
    I_Error('R_CheckPlane(): no more visplanes');

  pll := @visplanes[lastvisplane];
  pll.height := pl.height;
  pll.picnum := pl.picnum;
  pll.lightlevel := pl.lightlevel;
  pll.special := pl.special;
  pll.renderflags := pl.renderflags;
  pll.slopeSID := pl.slopeSID;  // JVAL: Slopes
  {$IFNDEF OPENGL}
  pll.slope := pl.slope;        // JVAL: Slopes
  {$ENDIF}

  pl := pll;

  R_NewVisPlane;
  visplanehash[R_VisplaneHash(pl.height, pl.picnum, pl.lightlevel,
    pl.special, pl.renderflags, pl.slopeSID)] := lastvisplane;

  pl.minx := start;
  pl.maxx := stop;

  result := pl;

  memset(@result.top[-1], iVISEND, (2 + SCREENWIDTH) * SizeOf(visindex_t));
end;
{$ENDIF}

//
// R_MakeSpans
//
{$IFNDEF OPENGL}
procedure R_MakeSpans(x, t1, b1, t2, b2: integer; const func: mapplanefunc_t);
var
  x1: integer;
begin
  x1 := x - 1;
  if t1 < 0 then
    t1 := 0;
  while (t1 < t2) and (t1 <= b1) do
  begin
  // JVAL 9/7/05
    if t1 < viewwidth then
      func(t1, spanstart[t1], x1);
    inc(t1);
  end;
  while (b1 > b2) and (b1 >= t1) do
  begin
  // JVAL 9/7/05
    if (b1 >= 0) and (b1 < viewwidth) then
      func(b1, spanstart[b1], x1);
    dec(b1);
  end;

  if t2 < 0 then
    t2 := 0;
  while (t2 < t1) and (t2 <= b2) do
  begin
  // JVAL 9/7/05
    if t2 < viewwidth then
      spanstart[t2] := x;
    inc(t2);
  end;
  while (b2 > b1) and (b2 >= t2) do
  begin
  // JVAL 9/7/05
    if (b2 >= 0) and (b2 < viewwidth) then //Length(spanstart)) then
      spanstart[b2] := x;
    dec(b2);
  end;
end;
{$ENDIF}

procedure R_CalcPlaneOffsets(const pl: Pvisplane_t);
var
  scrollOffset: integer;
begin
  if enableflatscrolling then
  begin
    // Handle scrolling flats
    case pl.special of
      201, 202, 203: // Scroll_North_xxx
        begin
          scrollOffset := (leveltime div 2) and 63;
          ds_xoffset := 0;
          ds_yoffset := ((scrollOffset * _SHL(1, pl.special - 201)) and 63) * FRACUNIT;
        end;
      204, 205, 206: // Scroll_East_xxx
        begin
          scrollOffset := (leveltime div 2) and 63;
          ds_xoffset := (((63 - scrollOffset) * _SHL(1, pl.special - 204)) and 63) * FRACUNIT;
          ds_yoffset := 0;
        end;
      207, 208, 209: // Scroll_South_xxx
        begin
          scrollOffset := (leveltime div 2) and 63;
          ds_xoffset := 0;
          ds_yoffset := (((63 - scrollOffset) * _SHL(1, pl.special - 207)) and 63) * FRACUNIT;
        end;
      210, 211, 212: // Scroll_West_xxx
        begin
          scrollOffset := (leveltime div 2) and 63;
          ds_xoffset := ((scrollOffset * _SHL(1, pl.special - 210)) and 63) * FRACUNIT;
          ds_yoffset := 0;
        end;
      213, 214, 215: // Scroll_NorthWest_xxx
        begin
          scrollOffset := (leveltime div 2) and 63;
          ds_xoffset := ((scrollOffset * _SHL(1, pl.special - 213)) and 63) * FRACUNIT;
          ds_yoffset := ds_xoffset;
        end;
      216, 217, 218: // Scroll_NorthEast_xxx
        begin
          scrollOffset := (leveltime div 2) and 63;
          ds_xoffset := (((63 - scrollOffset) * _SHL(1, pl.special - 216)) and 63) * FRACUNIT;
          ds_yoffset := ((scrollOffset * _SHL(1, pl.special - 216)) and 63) * FRACUNIT;
        end;
      219, 220, 221: // Scroll_SouthEast_xxx
        begin
          scrollOffset := (leveltime div 2) and 63;
          ds_xoffset := (((63 - scrollOffset) * _SHL(1, pl.special - 219)) and 63) * FRACUNIT;
          ds_yoffset := ds_xoffset;
        end;
      222, 223, 224: // Scroll_SouthWest_xxx
        begin
          scrollOffset := (leveltime div 2) and 63;
          ds_xoffset := ((scrollOffset * _SHL(1, pl.special - 222)) and 63) * FRACUNIT;
          ds_yoffset := (((63 - scrollOffset) * _SHL(1, pl.special - 222)) and 63) * FRACUNIT;
        end;
      else
        begin
          ds_xoffset := 0;
          ds_yoffset := 0;
        end;
    end;
  end
  else
  begin
    ds_xoffset := 0;
    ds_yoffset := 0;
  end;
{$IFDEF OPENGL}
  pl.xoffs := ds_xoffset;
  pl.yoffs := ds_yoffset;
{$ENDIF}
end;

{$IFNDEF OPENGL}
// Static buffer for double sky
var
  tempsource: array[0..MAXTEXTUREHEIGHT] of LongWord;

//
// R_DrawPlanes
// At the end of each frame.
//
procedure R_DrawPlanes; // JVAL: 3d Floors
var
  i: integer;
  pl: Pvisplane_t;
begin
  for i := 0 to lastvisplane - 1 do
  begin
    pl := @visplanes[i];
    if pl.renderflags and SRF_SLOPED = 0 then
      R_DoDrawPlane(pl)
    else
      R_DoDrawSlope(pl);  //JVAL: Slopes
  end;
end;

procedure R_DoDrawPlane(const pl: Pvisplane_t); // JVAL: 3d Floors
var
  k, sksize: integer;
  light: integer;
  x: integer;
  stop: integer;
  angle: integer;
  offset, offset2: LongWord;
  skytex: integer;
  destb: PByte;
  destl: PLongWord;
begin
  if pl.minx > pl.maxx then
    exit;

  // sky flat
  if pl.picnum = skyflatnum then
  begin
    if videomode = vm8bit then
      dc_iscale := (FRACUNIT * 200 * 61 div 50) div viewheight
    else
      dc_iscale := (FRACUNIT * 256 * 61 div 50) div viewheight;

    dc_texturemid := skytexturemid;

    if DoubleSky then
    begin // Render 2 layers, sky 1 in front
      R_DisableFixedColumn;
      offset := Sky1ColumnOffset * 64;
      offset2 := Sky2ColumnOffset * 64;
      for x := pl.minx to pl.maxx do
      begin
        dc_yl := pl.top[x];
        dc_yh := pl.bottom[x];
        if dc_yl < dc_yh then
        begin
          angle := (viewangle + xtoviewangle[x] + offset) div ANGLETOSKYUNIT;
          dc_texturemod := 0;
          dc_mod := 0;
          dc_x := x;
          R_GetDCs(SkyTexture, angle);
          if videomode = vm32bit then
          begin
            sksize := dc_columnsize;
            memcpy(@tempsource, dc_source32, (dc_columnsize + 1) * 4)
          end
          else
          begin
            sksize := 200;
            memcpy(@tempsource, dc_source, 200);
          end;

          angle := (viewangle + xtoviewangle[x] + offset2) div ANGLETOSKYUNIT;
          if detaillevel > DL_NORMAL then
          begin
            dc_texturemod := (((viewangle + xtoviewangle[x]) mod ANGLETOSKYUNIT) * DC_HIRESFACTOR) div ANGLETOSKYUNIT;
            dc_mod := dc_texturemod;
          end;
          dc_x := x;
          R_GetDCs(Sky2Texture, angle);

          if videomode = vm32bit then
          begin
            destl := @tempsource[0];
            for k := 0 to sksize do
            begin
              if destl^ = 0 then
                destl^ := dc_source32[k];
              inc(destl);
            end;
            dc_source32 := @tempsource;
          end
          else
            begin
            destb := @tempsource[0];
            for k := 0 to 199 do
            begin
              if destb^ = 0 then
                destb^ := dc_source[k];
              inc(destb);
            end;
            dc_source := @tempsource;
          end;

          skycolfunc;
        end;
      end;
      R_EnableFixedColumn;
      exit; // Next visplane
    end;


    R_DisableFixedColumn;

    if pl.special = 200 then
    begin
      offset := Sky2ColumnOffset * 64;
      skytex := Sky2Texture;
    end
    else
    begin
      offset := Sky1ColumnOffset * 64;
      skytex := SkyTexture;
    end;

    for x := pl.minx to pl.maxx do
    begin
      dc_yl := pl.top[x];
      dc_yh := pl.bottom[x];

      if dc_yl < dc_yh then
      begin
        angle := (viewangle + xtoviewangle[x] + offset) div ANGLETOSKYUNIT;
        if detaillevel <= DL_NORMAL then
        begin
          dc_texturemod := 0;
          dc_mod := 0;
        end
        else
        begin
          dc_texturemod := (((viewangle + xtoviewangle[x]) mod ANGLETOSKYUNIT) * DC_HIRESFACTOR) div ANGLETOSKYUNIT;
          dc_mod := dc_texturemod;
        end;
        dc_x := x;
        R_GetDCs(skytex, angle);
        // Sky is always drawn full bright,
        //  i.e. colormaps[0] is used.
        //  Because of this hack, sky is not affected
        //  by INVUL inverse mapping.
        // JVAL
        //  call skycolfunc(), not colfunc(), does not use colormaps!
        skycolfunc;
      end;
    end;
    R_EnableFixedColumn;
    exit;
  end;

  // regular flat
  R_GetDSs(pl.picnum);
  // JVAL SOS
  R_CalcPlaneOffsets(pl);

  planeheight := abs(pl.height - viewz);
  light := _SHR(pl.lightlevel, LIGHTSEGSHIFT) + extralight;

  if light >= LIGHTLEVELS then
    light := LIGHTLEVELS - 1;

  if light < 0 then
    light := 0;

  planezlight := @zlight[light];
  ds_llzindex := light;

  pl.top[pl.maxx + 1] := VISEND;
  pl.top[pl.minx - 1] := VISEND;

  if pl.renderflags and SRF_RIPPLE <> 0 then
  begin
    spanfunc := ripplespanfunc;
    ds_ripple := curripple;
    spanfuncMT := ripplespanfuncMT;
  end
  else
  begin
    spanfunc := basespanfunc;
    ds_ripple := nil;
    spanfuncMT := basespanfuncMT;
  end;

  stop := pl.maxx + 1;

  for x := pl.minx to stop do
  begin
    R_MakeSpans(x, pl.top[x - 1], pl.bottom[x - 1], pl.top[x], pl.bottom[x], @R_MapPlane);
  end;

  if ds_source <> nil then
    Z_ChangeTag(ds_source, PU_CACHE);
end;
{$ENDIF}

end.

