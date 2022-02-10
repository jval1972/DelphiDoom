//------------------------------------------------------------------------------
//
//  DelphiDoom is a source port of the game Doom and it is
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 1993-1996 by id Software, Inc.
//  Copyright (C) 2004-2022 by Jim Valavanis
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
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

//
// JVAL
//
// Volumetric dynamic lightmap using a single 3D texture
//

unit gl_lightmaps;

interface

uses
  d_delphi,
  dglOpenGL;

const
  LIGHTMAPSIZEX = 256;
  LIGHTMAPSIZEY = 64;
  LIGHTMAPSIZEZ = 256;
  LIGHTMAPUNIT = 16;
  LIGHTMAPBUFFERSIZE = LIGHTMAPSIZEX * LIGHTMAPSIZEY * LIGHTMAPSIZEZ * 4;

var
  gl_uselightmaps: boolean = true;
  canuselightmaps: boolean = true;

//==============================================================================
//
// gld_InitLightmap
//
//==============================================================================
procedure gld_InitLightmap;

//==============================================================================
//
// gld_LightmapDone
//
//==============================================================================
procedure gld_LightmapDone;

//==============================================================================
//
// gld_ActivateLightmap
//
//==============================================================================
procedure gld_ActivateLightmap;

//==============================================================================
//
// gld_DeactivateLightmap
//
//==============================================================================
procedure gld_DeactivateLightmap;

//==============================================================================
//
// gld_PauseLightmap
//
//==============================================================================
procedure gld_PauseLightmap;

//==============================================================================
//
// gld_ResumeLightmap
//
//==============================================================================
procedure gld_ResumeLightmap;

implementation

uses
  c_cmds,
  m_fixed,
  r_defs,
  r_main,
  p_tick,
  p_3dfloors,
  gl_defs,
  r_dynlights;

type
  lightmapbuffer_t = array[0..LIGHTMAPBUFFERSIZE - 1] of byte;
  lightmapbuffer_p = ^lightmapbuffer_t;

var
  lightmapbuffer: lightmapbuffer_p;
  lightmap_tex_num: GLUint;

// Lightmap regions to update
type
  lightmapmark_t = record
    x1, x2: integer;
    y1, y2: integer;
    z1, z2: integer;
    dx, dy, dz: integer;
  end;
  lightmapmark_p = ^lightmapmark_t;
  lightmapmark_tArray = array[0..$FFFF] of lightmapmark_t;
  lightmapmark_pArray = ^lightmapmark_tArray;

type
  lightmaprtlitem_t = record
    r, g, b: float;
    inter: integer;
    shadow: integer;
  end;
  lightmaprtlitem_p = ^lightmaprtlitem_t;
  // JVAL:  Actual size is intetionally LIGHTMAPSIZEX * LIGHTMAPSIZEY * LIGHTMAPSIZEZ + 1
  lightmaprtlitem_tArray = array[0..LIGHTMAPSIZEX * LIGHTMAPSIZEY * LIGHTMAPSIZEZ] of lightmaprtlitem_t;
  lightmaprtlitem_pArray = ^lightmaprtlitem_tArray;

const
  MAXSHADOWSHIFT = 8; // JVAL: 8 Shadows max at the same point

var
  SHADOWFACTOR: array[0..MAXSHADOWSHIFT - 1] of float;

var
  lightmapmarks: lightmapmark_pArray;
  maxmarks: integer;
  nummarks: integer;
  maxmarkbytes: integer;
  lightmapdefined: boolean;
  rtllightmap: lightmaprtlitem_pArray;

type
  exportlightmap_t = array[0..LIGHTMAPSIZEX - 1, 0..LIGHTMAPSIZEX - 1, 0..LIGHTMAPSIZEX - 1] of LongWord;
  exportlightmap_p = ^exportlightmap_t;

//==============================================================================
//
// Vox_ExportLightmap
//
//==============================================================================
procedure Vox_ExportLightmap(const fname: string);
var
  x, y, z: integer;
  lst: TDStringList;
  tmp: string;
  c: LongWord;
  skips: integer;
  blancline: boolean;
  lp: exportlightmap_p;

  function _get_color(const rl: lightmaprtlitem_t): LongWord;
  var
    r, g, b: integer;
  begin
    r := Round(rl.r * 256);
    if r < 0 then
      r := 0
    else if r > 255 then
      r := 255;
    g := Round(rl.g * 256);
    if r < 0 then
      g := 0
    else if g > 255 then
      g := 255;
    b := Round(rl.b * 256);
    if b < 0 then
      b := 0
    else if b > 255 then
      b := 255;
    Result := b shl 16 + g shl 8 + r;
  end;

begin
  if strtrim(fname) = '' then
  begin
    printf('Please specify the output filename'#13#10);
    exit;
  end;

  lp := mallocz(SizeOf(exportlightmap_t));

  for x := 0 to LIGHTMAPSIZEX - 1 do
    for y := 0 to LIGHTMAPSIZEY - 1 do
      for z := 0 to LIGHTMAPSIZEZ - 1 do
        lp[x, y, z] := _get_color(rtllightmap[x * (LIGHTMAPSIZEX * LIGHTMAPSIZEY) + y * LIGHTMAPSIZEX + z]);

  lst := TDStringList.Create;
  try
    lst.Add(itoa(LIGHTMAPSIZEX));
    for x := 0 to LIGHTMAPSIZEX - 1 do
      for y := 0 to LIGHTMAPSIZEX - 1 do
      begin
        tmp := '';
        blancline := True;
        skips := 0;
        for z := 0 to LIGHTMAPSIZEX - 1 do
        begin
          c := lp[LIGHTMAPSIZEX - 1 - x, LIGHTMAPSIZEX - 1 - y, z];
          if c <> 0 then
            blancline := False
          else if blancline then
            inc(skips);
          if not blancline then
            tmp := tmp + ' ' + itoa(c);
        end;
        if blancline then
          lst.Add('skip ' + itoa(LIGHTMAPSIZEX))
        else
        begin
          if skips > 0 then
            lst.Add('skip ' + itoa(skips) + ' ' + tmp)
          else
            lst.Add(tmp);
        end;
      end;
    lst.SaveToFile(fname);
  finally
    lst.Free;
  end;
  freemem(pointer(lp), SizeOf(exportlightmap_t));
end;

//==============================================================================
//
// gld_InitLightmap
//
//==============================================================================
procedure gld_InitLightmap;
var
  i: integer;
  rtlp: lightmaprtlitem_p;
begin
  SHADOWFACTOR[0] := 128.0;
  for i := 1 to MAXSHADOWSHIFT - 1 do
    SHADOWFACTOR[i] := 128.0 * (MAXSHADOWSHIFT - i) / MAXSHADOWSHIFT;

  glGenTextures(1, @lightmap_tex_num);

  // JVAL: Setup lightmap mark regions
  maxmarks := 32;  // JVAL: initial value, we will grow it if needed
  lightmapmarks := mallocz(maxmarks * SizeOf(lightmapmark_t));
  // JVAL: Initially we mark the entire LIGHTMAP
  lightmapmarks[0].x1 := 0;
  lightmapmarks[0].x2 := LIGHTMAPSIZEX - 1;
  lightmapmarks[0].y1 := 0;
  lightmapmarks[0].y2 := LIGHTMAPSIZEY - 1;
  lightmapmarks[0].z1 := 0;
  lightmapmarks[0].z2 := LIGHTMAPSIZEZ - 1;
  lightmapmarks[0].dx := LIGHTMAPSIZEX;
  lightmapmarks[0].dy := LIGHTMAPSIZEY;
  lightmapmarks[0].dz := LIGHTMAPSIZEZ;
  nummarks := 1;

  rtllightmap := malloc(SizeOf(lightmaprtlitem_tArray));
  rtlp := @rtllightmap[0];
  for i := 0 to LIGHTMAPSIZEX * LIGHTMAPSIZEY * LIGHTMAPSIZEZ do
  begin
    rtlp.r := 0.0;
    rtlp.g := 0.0;
    rtlp.b := 0.0;
    rtlp.shadow := 0;
    rtlp.inter := 0;
    inc(rtlp);
  end;

  lightmapbuffer := malloc(LIGHTMAPBUFFERSIZE);
  maxmarkbytes := LIGHTMAPBUFFERSIZE;
  lightmapdefined := false;

  C_AddCmd('vox_exportlightmap', @Vox_ExportLightmap);
end;

//==============================================================================
//
// gld_LightmapDone
//
//==============================================================================
procedure gld_LightmapDone;
begin
  memfree(pointer(rtllightmap), SizeOf(lightmaprtlitem_tArray));
  memfree(pointer(lightmapmarks), maxmarks * SizeOf(lightmapmark_t));
  memfree(pointer(lightmapbuffer), LIGHTMAPBUFFERSIZE);
  glDeleteTextures(1, @lightmap_tex_num);
end;

// Starting point of lightmap in OpenGL coordinates
var
  l3dx: float = -LIGHTMAPSIZEX * LIGHTMAPUNIT / MAP_COEFF / 2;
  l3dy: float = -LIGHTMAPSIZEY * LIGHTMAPUNIT / MAP_COEFF / 2;
  l3dz: float = -LIGHTMAPSIZEZ * LIGHTMAPUNIT / MAP_COEFF / 2;

//==============================================================================
// opengl2lightmapx
//
// Utility functions to switch coordinates (LIGHMAP TEXTURE/OpenGL).
//
//==============================================================================
function opengl2lightmapx(const f: float): integer;
begin
  result := trunc((f - l3dx) * (MAP_COEFF / LIGHTMAPUNIT));
end;

//==============================================================================
//
// opengl2lightmapy
//
//==============================================================================
function opengl2lightmapy(const f: float): integer;
begin
  result := trunc((f - l3dy) * (MAP_COEFF / LIGHTMAPUNIT));
end;

//==============================================================================
//
// opengl2lightmapz
//
//==============================================================================
function opengl2lightmapz(const f: float): integer;
begin
  result := trunc((f - l3dz) * (MAP_COEFF / LIGHTMAPUNIT));
end;

//==============================================================================
//
// lightmap2openglx
//
//==============================================================================
function lightmap2openglx(const i: integer): float;
begin
  result := (i * LIGHTMAPUNIT / MAP_COEFF) + l3dx;
end;

//==============================================================================
//
// lightmap2opengly
//
//==============================================================================
function lightmap2opengly(const i: integer): float;
begin
  result := (i * LIGHTMAPUNIT / MAP_COEFF) + l3dy;
end;

//==============================================================================
//
// lightmap2openglz
//
//==============================================================================
function lightmap2openglz(const i: integer): float;
begin
  result := (i * LIGHTMAPUNIT / MAP_COEFF) + l3dz;
end;

var
  lmvalidcount: integer = -1;
//
// gld_CalculateLightmap
//
// JVAL: Calculate the entire lightmap
//
const
  MINLIGHTMAPRADIOUS = 1.7321 * LIGHTMAPUNIT / MAP_COEFF;

//==============================================================================
//
// _float_to_byte
//
//==============================================================================
function _float_to_byte(const f: float): byte;
begin
  if f <= 0.5 then
    Result := 0
  else if f >= 254.5 then
    Result := 255
  else
    Result := Round(f);
end;

//==============================================================================
//
// gld_ClearLightmapMarkSTD
//
//==============================================================================
procedure gld_ClearLightmapMarkSTD(const markp: lightmapmark_p);
var
  ix, iy, iz: integer;
  x_offs, xy_offs: integer;
  rtlp: lightmaprtlitem_p;
begin
  for ix := markp.x1 to markp.x2 do
  begin
    x_offs := ix * (LIGHTMAPSIZEX * LIGHTMAPSIZEY);
    for iy := markp.y1 to markp.y2 do
    begin
      xy_offs := x_offs + iy * LIGHTMAPSIZEX;
      rtlp := @rtllightmap[xy_offs + markp.z1];
      for iz := markp.z1 to markp.z2 do
      begin
        rtlp.r := 0.0;
        rtlp.g := 0.0;
        rtlp.b := 0.0;
        rtlp.shadow := 0;
        rtlp.inter := 0;
        inc(rtlp);
      end;
    end;
  end;
end;

//==============================================================================
//
// gld_CalculateLightmapSTD
//
//==============================================================================
procedure gld_CalculateLightmapSTD;
var
  i: integer;
  ix, iy, iz: integer;
  lx, ly, lz: integer;
  ix1, ix2, iy1, iy2, iz1, iz2: integer;
  fx1, fx2, fy1, fy2, fz1, fz2: float;
  pdls: Pdlsortitem_t;
  pdlsx, pdlsy, pdlsz: float;
  pdlsy1, pdlsy2: float;
  l: PGLDRenderLight;
  squaredist, squarecheck, squarecheck2: float;
  point: PByte;
  lastitem: dlsortitem_t;
  markp: lightmapmark_p;
  numbytes: integer;
  checkradious: float;
  rtlp: lightmaprtlitem_p;
  lo_iz, lo_iyz: float;
  z_offs, yz_offs: integer;
  rgb_max: float;
  ffloorz, fceilingz: float;
  xx, yy, zz: fixed_t;  // Map Coordinates
  sfactor: float;
  sec: Psector_t;
begin
  if lightmapdefined and (lmvalidcount = leveltime) then
    exit;

  lmvalidcount := leveltime;

// -----------------------------------------------------------------------------
// Calculate the starting point of lightmap
  l3dx := lightmap2openglx(opengl2lightmapx(camera.position[0] - LIGHTMAPSIZEX * LIGHTMAPUNIT / MAP_COEFF / 2));
  l3dy := lightmap2opengly(opengl2lightmapy(camera.position[1] - LIGHTMAPSIZEY * LIGHTMAPUNIT / MAP_COEFF / 2));
  l3dz := lightmap2openglz(opengl2lightmapz(camera.position[2] - LIGHTMAPSIZEZ * LIGHTMAPUNIT / MAP_COEFF / 2));

  if camera.rotation[0] >= 25.0 then
    l3dy := l3dy - LIGHTMAPSIZEY * LIGHTMAPUNIT / MAP_COEFF / 2 + 1.0
  else if camera.rotation[0] <= -25.0 then
    l3dy := l3dy + LIGHTMAPSIZEY * LIGHTMAPUNIT / MAP_COEFF / 2 - 1.0;

  if (camera.rotation[1] <= 45.0) and (camera.rotation[1] >= -45.0) then
    l3dz := l3dz - LIGHTMAPSIZEZ * LIGHTMAPUNIT / MAP_COEFF / 2 + 1.0
  else if (camera.rotation[1] <= 135.0) and (camera.rotation[1] > 45.0) then
    l3dx := l3dx + LIGHTMAPSIZEX * LIGHTMAPUNIT / MAP_COEFF / 2 - 1.0
  else if (camera.rotation[1] <= 225.0) and (camera.rotation[1] > 135.0) then
    l3dz := l3dz + LIGHTMAPSIZEZ * LIGHTMAPUNIT / MAP_COEFF / 2 - 1.0
  else
    l3dx := l3dx - LIGHTMAPSIZEX * LIGHTMAPUNIT / MAP_COEFF / 2 + 1.0;

// -----------------------------------------------------------------------------
// Clear the marked regions of lightmap and the rtl lightmap
  memset(lightmapbuffer, 128, maxmarkbytes);
  if not lightmapdefined then
  begin
    glTexImage3D(GL_TEXTURE_3D, 0, 3, LIGHTMAPSIZEX, LIGHTMAPSIZEY, LIGHTMAPSIZEZ, 0, GL_RGBA, GL_UNSIGNED_BYTE, lightmapbuffer);
    lightmapdefined := true;  // JVAL: Don't bother reseting this on a new map
  end
  else
  begin
    markp := @lightmapmarks[nummarks];
    while integer(markp) > integer(lightmapmarks) do
    begin
      dec(markp);
      // JVAL 20171205 We must clear the mark BEFORE calling glTexSubImage3D :)
      gld_ClearLightmapMarkSTD(markp);
      glTexSubImage3D(GL_TEXTURE_3D, 0, markp.x1, markp.y1, markp.z1, markp.dx, markp.dy, markp.dz, GL_RGBA, GL_UNSIGNED_BYTE, lightmapbuffer);
    end;
  end;

// -----------------------------------------------------------------------------
// Itterates all active lights, mark the affected regions, precalc the rtl lightmap
  maxmarkbytes := 0;
  nummarks := 0;

  // Lightmap boundaries
  fx1 := lightmap2openglx(0);
  fx2 := lightmap2openglx(LIGHTMAPSIZEX - 1);
  fy1 := lightmap2opengly(0);
  fy2 := lightmap2opengly(LIGHTMAPSIZEY - 1);
  fz1 := lightmap2openglz(0);
  fz2 := lightmap2openglz(LIGHTMAPSIZEZ - 1);

  pdls := @dlbuffer[numdlitems];
  ZeroMemory(@lastitem, SizeOf(dlsortitem_t));
  while integer(pdls) <> integer(dlbuffer) do
  begin
    dec(pdls);
    if (pdls.l <> lastitem.l) or
       (pdls.x <> lastitem.x) or
       (pdls.y <> lastitem.y) or
       (pdls.z <> lastitem.z) then
    begin
      lastitem := pdls^;
      pdlsx := lastitem.x;
      pdlsy := lastitem.y;
      pdlsz := lastitem.z;

      // Check if the light source is inside the lightmap boundaries
      if (pdlsx >= fx1) and (pdlsx <= fx2) and
         (pdlsy >= fy1) and (pdlsy <= fy2) and
         (pdlsz >= fz1) and (pdlsz <= fz2) then
      begin
        l := pdls.l;
        checkradious := l.radius;
        if checkradious <  MINLIGHTMAPRADIOUS then
          checkradious := MINLIGHTMAPRADIOUS;
        squarecheck := checkradious * checkradious;
        squarecheck2 := squarecheck / 1.141421356;

        ix1 := opengl2lightmapx(pdlsx - checkradious);
        if ix1 < 0 then
          ix1 := 0
        else if ix1 >= LIGHTMAPSIZEX then
          ix1 := LIGHTMAPSIZEX - 1;

        ix2 := opengl2lightmapx(pdlsx + checkradious);
        if ix2 < 0 then
          ix2 := 0
        else if ix2 >= LIGHTMAPSIZEX then
          ix2 := LIGHTMAPSIZEX - 1;

        pdlsy1 := pdlsy - checkradious;
        pdlsy2 := pdlsy + checkradious;

        xx := -Round(pdlsx * MAP_SCALE);
        yy := Round(pdlsz * MAP_SCALE);
        zz := Round(pdlsy * MAP_SCALE);
        sec := R_PointInSubsector(xx, yy).sector;
        if l.shadow then
        begin
          pdlsy1 := (P_3dFloorHeight(sec, xx, yy, zz) - 4 * FRACUNIT) / MAP_SCALE;
          pdlsy2 := pdlsy2 - checkradious / 2;
          ffloorz := (P_3dFloorHeight(sec, xx, yy, zz) - 4 * FRACUNIT) / MAP_SCALE;
          fceilingz := (P_3dCeilingHeight(sec, xx, yy, zz) + 4 * FRACUNIT) / MAP_SCALE;
          if pdlsy2 < ffloorz then
            pdlsy2 := ffloorz
          else if pdlsy2 > fceilingz then
            pdlsy2 := fceilingz;
        end
        else
        begin
          ffloorz := (P_3dFloorHeight(sec, xx, yy, zz) - 4 * FRACUNIT) / MAP_SCALE;
          fceilingz := (P_3dCeilingHeight(sec, xx, yy, zz) + 4 * FRACUNIT) / MAP_SCALE;
          if pdlsy1 < ffloorz then
            pdlsy1 := ffloorz
          else if pdlsy1 > fceilingz then
            pdlsy1 := fceilingz;
          if pdlsy2 < ffloorz then
            pdlsy2 := ffloorz
          else if pdlsy2 > fceilingz then
            pdlsy2 := fceilingz;
        end;

        iy1 := opengl2lightmapy(pdlsy1);
        if iy1 < 0 then
          iy1 := 0
        else if iy1 >= LIGHTMAPSIZEY then
          iy1 := LIGHTMAPSIZEY - 1;

        iy2 := opengl2lightmapy(pdlsy2);
        if iy2 < 0 then
          iy2 := 0
        else if iy2 >= LIGHTMAPSIZEY then
          iy2 := LIGHTMAPSIZEY - 1;

        iz1 := opengl2lightmapz(pdlsz - checkradious);
        if iz1 < 0 then
          iz1 := 0
        else if iz1 >= LIGHTMAPSIZEZ then
          iz1 := LIGHTMAPSIZEZ - 1;

        iz2 := opengl2lightmapz(pdlsz + checkradious);
        if iz2 < 0 then
          iz2 := 0
        else if iz2 >= LIGHTMAPSIZEZ then
          iz2 := LIGHTMAPSIZEZ - 1;

        // Determine the maximum size that will be erased in the next pass
        lx := ix2 - ix1 + 1;
        ly := iy2 - iy1 + 1;
        lz := iz2 - iz1 + 1;
        numbytes := lx * ly * lz * 4;
        if numbytes > maxmarkbytes then
          maxmarkbytes := numbytes;

        for iz := iz1 to iz2 do
        begin
          lo_iz := sqr(pdlsz - lightmap2openglz(iz));
          if lo_iz <= squarecheck then
          begin
            for iy := iy1 to iy2 do
            begin
              lo_iyz := lo_iz + sqr(pdlsy - lightmap2opengly(iy));
              if lo_iyz <= squarecheck then
              begin
                rtlp := @rtllightmap[ix1 * (LIGHTMAPSIZEX * LIGHTMAPSIZEY) + iy * LIGHTMAPSIZEX + iz];
                for ix := ix1 to ix2 do
                begin
                  squaredist := lo_iyz + sqr(pdlsx - lightmap2openglx(ix));
                  if squaredist <= squarecheck2 then
                  begin
                    if l.shadow then
                      inc(rtlp.shadow, 2)
                    else
                    begin
                      rtlp.r := rtlp.r + l.r;
                      rtlp.g := rtlp.g + l.g;
                      rtlp.b := rtlp.b + l.b;
                      inc(rtlp.inter);
                    end;
                  end
                  else if squaredist <= squarecheck then
                  begin
                    if l.shadow then
                      inc(rtlp.shadow)
                    else
                    begin
                      rtlp.r := rtlp.r + l.r / 2;
                      rtlp.g := rtlp.g + l.g / 2;
                      rtlp.b := rtlp.b + l.b / 2;
                      inc(rtlp.inter);
                    end;
                  end;
                  inc(rtlp, LIGHTMAPSIZEX * LIGHTMAPSIZEY);
                end;
              end;
            end;
          end;
        end;

        // Grow the marks array if needed
        if nummarks = maxmarks then
        begin
          realloc(pointer(lightmapmarks), maxmarks * SizeOf(lightmapmark_t), (maxmarks + 16) * SizeOf(lightmapmark_t));
          maxmarks := maxmarks + 16;
        end;
        markp := @lightmapmarks[nummarks];
        markp.x1 := ix1;
        markp.x2 := ix2;
        markp.y1 := iy1;
        markp.y2 := iy2;
        markp.z1 := iz1;
        markp.z2 := iz2;
        markp.dx := lx;
        markp.dy := ly;
        markp.dz := lz;
        inc(nummarks);

      end;
    end;
  end;

// -----------------------------------------------------------------------------
// Create the lightmap texture, uses the affected regions and the rtl lightmap
  markp := @lightmapmarks[nummarks];
  while integer(markp) > integer(lightmapmarks) do
  begin
    dec(markp);
    point := @lightmapbuffer[0];
    for iz := markp.z1 to markp.z2 do
    begin
      for iy := markp.y1 to markp.y2 do
      begin
        rtlp := @rtllightmap[markp.x1 * (LIGHTMAPSIZEX * LIGHTMAPSIZEY) + iy * LIGHTMAPSIZEX + iz];
        for ix := markp.x1 to markp.x2 do
        begin
          if (rtlp.inter = 0) and (rtlp.shadow = 0) then
          begin
            PLongWord(point)^ := $80808080;
            inc(point, 4);
          end
          else
          begin
            if rtlp.shadow = 0 then
            begin
              point^ := _float_to_byte(128.0 + rtlp.r * 127);
              inc(point);
              point^ := _float_to_byte(128.0 + rtlp.g * 127);
              inc(point);
              point^ := _float_to_byte(128.0 + rtlp.b * 127);
              inc(point);
              point^ := $80;
              inc(point);
            end
            else
            begin
              if rtlp.shadow >= MAXSHADOWSHIFT then
                sfactor := SHADOWFACTOR[MAXSHADOWSHIFT - 1]
              else
                sfactor := SHADOWFACTOR[rtlp.shadow];
              point^ := _float_to_byte(sfactor + rtlp.r * 127);
              inc(point);
              point^ := _float_to_byte(sfactor + rtlp.g * 127);
              inc(point);
              point^ := _float_to_byte(sfactor + rtlp.b * 127);
              inc(point);
              point^ := $80;
              inc(point);
            end;
          end;
          inc(rtlp, LIGHTMAPSIZEX * LIGHTMAPSIZEY);
        end;
      end;
    end;
    glTexSubImage3D(GL_TEXTURE_3D, 0, markp.x1, markp.y1, markp.z1, markp.dx, markp.dy, markp.dz, GL_RGBA, GL_UNSIGNED_BYTE, lightmapbuffer);
  end;
end;

//==============================================================================
//
// gld_PlaceLightmapTexture
//
// Texture positioning in world coordinates
//
//==============================================================================
procedure gld_PlaceLightmapTexture;
begin
  glMatrixMode(GL_TEXTURE);
  glPushMatrix;
  glTranslatef(-l3dx * MAP_COEFF / (LIGHTMAPSIZEX * LIGHTMAPUNIT),
               -l3dy * MAP_COEFF / (LIGHTMAPSIZEY * LIGHTMAPUNIT),
               -l3dz * MAP_COEFF / (LIGHTMAPSIZEZ * LIGHTMAPUNIT));

  glMatrixMode(GL_MODELVIEW);
end;

//==============================================================================
// gld_ActivateLightmap
//
// JVAL
//
// Activate Lightmap 3D texture, calls gld_CalculateLightmap
//
//==============================================================================
procedure gld_ActivateLightmap;
var
  TexGenSPlane, TexGenTPlane, TexGenRPlane: TVector4f;
begin
  glActiveTextureARB(GL_TEXTURE1_ARB);
  glEnable(GL_TEXTURE_3D);

  glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE_ARB);
  glTexEnvi(GL_TEXTURE_ENV, GL_RGB_SCALE_ARB, 2);
  glPixelStorei(GL_UNPACK_ALIGNMENT, 4);

  glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_S, GL_CLAMP);
  glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_T, GL_CLAMP);
  glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_R, GL_CLAMP);

  glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MAG_FILTER, GL_LINEAR); //GL_NEAREST); //gl_tex_filter);
  glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MIN_FILTER, GL_LINEAR); //GL_NEAREST); //gl_tex_filter);

  glEnable(GL_TEXTURE_GEN_S);
  glEnable(GL_TEXTURE_GEN_T);
  glEnable(GL_TEXTURE_GEN_R);

  glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
  glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
  glTexGeni(GL_R, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);

  TexGenSPlane[0] := MAP_COEFF / (LIGHTMAPSIZEX * LIGHTMAPUNIT);
  TexGenSPlane[1] := 0.0;
  TexGenSPlane[2] := 0.0;
  TexGenSPlane[3] := 0.0;

  TexGenTPlane[0] := 0.0;
  TexGenTPlane[1] := MAP_COEFF / (LIGHTMAPSIZEY * LIGHTMAPUNIT);
  TexGenTPlane[2] := 0.0;
  TexGenTPlane[3] := 0.0;

  TexGenRPlane[0] := 0.0;
  TexGenRPlane[1] := 0.0;
  TexGenRPlane[2] := MAP_COEFF / (LIGHTMAPSIZEZ * LIGHTMAPUNIT);
  TexGenRPlane[3] := 0.0;

  glTexGenfv(GL_S, GL_OBJECT_PLANE, @TexGenSPlane);
  glTexGenfv(GL_T, GL_OBJECT_PLANE, @TexGenTPlane);
  glTexGenfv(GL_R, GL_OBJECT_PLANE, @TexGenRPlane);

  if not lightmapdefined then
  begin
    glBindTexture(GL_TEXTURE_3D, lightmap_tex_num);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_BASE_LEVEL, 0);
    glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MAX_LEVEL, 0);
  end;

  gld_CalculateLightmapSTD;
  gld_PlaceLightmapTexture;

  glActiveTextureARB(GL_TEXTURE0_ARB);
  glEnable(GL_TEXTURE_2D);
end;

//==============================================================================
// gld_DeactivateLightmap
//
// JVAL
//
// Deactivate Lightmap
//
//==============================================================================
procedure gld_DeactivateLightmap;
begin
  glActiveTextureARB(GL_TEXTURE1_ARB);
  glMatrixMode(GL_TEXTURE);
  glPopMatrix;
  glMatrixMode(GL_MODELVIEW);
  glDisable(GL_TEXTURE_3D);
  glActiveTextureARB(GL_TEXTURE0_ARB);
  glEnable(GL_TEXTURE_2D);
end;

//==============================================================================
//
// gld_PauseLightmap
//
//==============================================================================
procedure gld_PauseLightmap;
begin
  glActiveTextureARB(GL_TEXTURE1_ARB);
  glDisable(GL_TEXTURE_3D);
  glActiveTextureARB(GL_TEXTURE0_ARB);
end;

//==============================================================================
//
// gld_ResumeLightmap
//
//==============================================================================
procedure gld_ResumeLightmap;
begin
  glActiveTextureARB(GL_TEXTURE1_ARB);
  glEnable(GL_TEXTURE_3D);
  glActiveTextureARB(GL_TEXTURE0_ARB);
end;

end.

