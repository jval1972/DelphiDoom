//------------------------------------------------------------------------------
//
//  DelphiDoom: A modified and improved DOOM engine for Windows
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 2004-2013 by Jim Valavanis
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
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : http://sourceforge.net/projects/delphidoom/
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
  LIGHTMAPBUFFERSIZE = LIGHTMAPSIZEX * LIGHTMAPSIZEY * LIGHTMAPSIZEZ * 3;

type
// Please note that the actual size is LIGHTMAPBUFFERSIZE + 1
  lightmapbuffer_t = array[0..LIGHTMAPBUFFERSIZE] of byte;
  lightmapbuffer_p = ^lightmapbuffer_t;

var
  lightmapbuffer: lightmapbuffer_p;
  lightmap_tex_num: GLUint;
  gl_uselightmaps: boolean = true;
  canuselightmaps: boolean = true;

procedure gld_InitLightmap;

procedure gld_LightmapDone;

procedure gld_ActivateLightmap;

procedure gld_DeactivateLightmap;

procedure gld_PauseLightmap;

procedure gld_ResumeLightmap;

implementation

uses
  p_tick,
  gl_defs,
  gl_dlights;

// Lightmap regions to update 
type
  lightmapmark_t = record
    x1, y1, z1: integer;
    x2, y2, z2: integer;
    dx, dy, dz: integer;
  end;
  lightmapmark_p = ^lightmapmark_t;
  lightmapmark_tArray = array[0..$FFFF] of lightmapmark_t;
  lightmapmark_pArray = ^lightmapmark_tArray;

type
  lightmaprtlitem_t = record
    r, g, b: float;
    inter: integer;
    shadow: boolean;
  end;
  lightmaprtlitem_p = ^lightmaprtlitem_t;
  // JVAL:  Actual size is intetionally LIGHTMAPSIZEX * LIGHTMAPSIZEY * LIGHTMAPSIZEZ + 1
  lightmaprtlitem_tArray = array[0..LIGHTMAPSIZEX * LIGHTMAPSIZEY * LIGHTMAPSIZEZ] of lightmaprtlitem_t;
  lightmaprtlitem_pArray = ^lightmaprtlitem_tArray;

var
  lightmapmarks: lightmapmark_pArray;
  maxmarks: integer;
  nummarks: integer;
  maxmarkbytes: integer;
  lightmapdefined: boolean;
  rtllightmap: lightmaprtlitem_pArray;

procedure gld_InitLightmap;
begin
  glGenTextures(1, @lightmap_tex_num);

  // JVAL: Setup lightmap mark regions
  maxmarks := 32;  // JVAL: initial value, we will grow it if needed
  lightmapmarks := malloc(maxmarks * SizeOf(lightmapmark_t));
  // JVAL: Initially we mark the entire LIGHTMAP
  lightmapmarks[0].x1 := 0;
  lightmapmarks[0].y1 := 0;
  lightmapmarks[0].z1 := 0;
  lightmapmarks[0].x2 := LIGHTMAPSIZEX - 1;
  lightmapmarks[0].y2 := LIGHTMAPSIZEY - 1;
  lightmapmarks[0].z2 := LIGHTMAPSIZEZ - 1;
  lightmapmarks[0].dx := LIGHTMAPSIZEX;
  lightmapmarks[0].dy := LIGHTMAPSIZEY;
  lightmapmarks[0].dz := LIGHTMAPSIZEZ;
  nummarks := 1;
  rtllightmap := mallocz(SizeOf(lightmaprtlitem_tArray));
  lightmapbuffer := malloc(LIGHTMAPBUFFERSIZE);
  maxmarkbytes := LIGHTMAPBUFFERSIZE;
  lightmapdefined := false;
end;

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

//
// Utility functions to switch coordinates (LIGHMAP TEXTURE/OpenGL).
//
function opengl2lightmapx(const f: float): integer;
begin
  result := trunc((f - l3dx) * (MAP_COEFF / LIGHTMAPUNIT));
end;

function opengl2lightmapy(const f: float): integer;
begin
  result := trunc((f - l3dy) * (MAP_COEFF / LIGHTMAPUNIT));
end;

function opengl2lightmapz(const f: float): integer;
begin
  result := trunc((f - l3dz) * (MAP_COEFF / LIGHTMAPUNIT));
end;

function lightmap2openglx(const i: integer): float;
begin
  result := (i * LIGHTMAPUNIT / MAP_COEFF) + l3dx;
end;

function lightmap2opengly(const i: integer): float;
begin
  result := (i * LIGHTMAPUNIT / MAP_COEFF) + l3dy;
end;

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

procedure gld_CalculateLightmap;
var
  i: integer;
  ix, iy, iz: integer;
  lx, ly, lz: integer;
  ix1, ix2, iy1, iy2, iz1, iz2: integer;
  fx1, fx2, fy1, fy2, fz1, fz2: float;
  i_xy: integer;
  pdls: Pdlsortitem_t;
  l: PGLDRenderLight;
  squaredist, squarecheck: float;
  point: PByte;
  lastitem: dlsortitem_t;
  markp: lightmapmark_p;
  numbytes: integer;
  checkradious: float;
  rtlp: lightmaprtlitem_p;
  lo_ix, lo_ixy: float;
  xy_offs: integer;
  rgb_max: float;

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
    glTexImage3D(GL_TEXTURE_3D, 0, 3, LIGHTMAPSIZEX, LIGHTMAPSIZEY, LIGHTMAPSIZEZ, 0, GL_RGB, GL_UNSIGNED_BYTE, lightmapbuffer);
    lightmapdefined := true;  // JVAL: Don't bother reseting this on a new map
  end
  else
  begin
    markp := @lightmapmarks[nummarks];
    while integer(markp) > integer(lightmapmarks) do
    begin
      dec(markp);
      glTexSubImage3D(GL_TEXTURE_3D, 0, markp.x1, markp.y1, markp.z1, markp.dx, markp.dy, markp.dz, GL_RGB, GL_UNSIGNED_BYTE, lightmapbuffer);
      for iy := markp.y1 to markp.y2 do
      begin
        i_xy := iy * LIGHTMAPSIZEX + markp.x1;
        for iz := markp.z1 to markp.z2 do
        begin
          rtlp := @rtllightmap[iz * (LIGHTMAPSIZEX * LIGHTMAPSIZEY) + i_xy];
          for ix := markp.x1 to markp.x2 do
          begin
            rtlp.r := 0.0;
            rtlp.g := 0.0;
            rtlp.b := 0.0;
            rtlp.shadow := false;
            rtlp.inter := 0;
            inc(rtlp);
          end;
        end;
      end;
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
      // Check if the light source is inside the lightmap boundaries
      if (pdls.x >= fx1) and (pdls.x <= fx2) and
         (pdls.y >= fy1) and (pdls.y <= fy2) and
         (pdls.z >= fz1) and (pdls.z <= fz2) then
      begin
        l := pdls.l;
        checkradious := l.radious;
        if checkradious <  MINLIGHTMAPRADIOUS then
          checkradious := MINLIGHTMAPRADIOUS;
        squarecheck := checkradious * checkradious;

        ix1 := opengl2lightmapx(pdls.x - checkradious);
        if ix1 < 0 then
          ix1 := 0
        else if ix1 >= LIGHTMAPSIZEX then
          ix1 := LIGHTMAPSIZEX - 1;

        ix2 := opengl2lightmapx(pdls.x + checkradious);
        if ix2 < 0 then
          ix2 := 0
        else if ix2 >= LIGHTMAPSIZEX then
          ix2 := LIGHTMAPSIZEX - 1;

        iy1 := opengl2lightmapy(pdls.y - checkradious);
        if iy1 < 0 then
          iy1 := 0
        else if iy1 >= LIGHTMAPSIZEY then
          iy1 := LIGHTMAPSIZEY - 1;

        iy2 := opengl2lightmapy(pdls.y + checkradious);
        if iy2 < 0 then
          iy2 := 0
        else if iy2 >= LIGHTMAPSIZEY then
          iy2 := LIGHTMAPSIZEY - 1;

        iz1 := opengl2lightmapz(pdls.z - checkradious);
        if iz1 < 0 then
          iz1 := 0
        else if iz1 >= LIGHTMAPSIZEZ then
          iz1 := LIGHTMAPSIZEZ - 1;

        iz2 := opengl2lightmapz(pdls.z + checkradious);
        if iz2 < 0 then
          iz2 := 0
        else if iz2 >= LIGHTMAPSIZEZ then
          iz2 := LIGHTMAPSIZEZ - 1;

        // Determine the maximum size that will be erased in the next pass
        lx := ix2 - ix1 + 1;
        ly := iy2 - iy1 + 1;
        lz := iz2 - iz1 + 1;
        numbytes := lx * ly * lz * 3;
        if numbytes > maxmarkbytes then
          maxmarkbytes := numbytes;

        for ix := ix1 to ix2 do
        begin
          lo_ix := sqr(pdls.x - lightmap2openglx(ix));
          for iy := iy1 to iy2 do
          begin
            lo_ixy := lo_ix + sqr(pdls.y - lightmap2opengly(iy));
            if lo_ixy <= squarecheck then
            begin
              xy_offs := iy * LIGHTMAPSIZEX + ix;
              for iz := iz1 to iz2 do
              begin
                rtlp := @rtllightmap[iz * (LIGHTMAPSIZEX * LIGHTMAPSIZEY) + xy_offs];
                squaredist := lo_ixy + sqr(pdls.z - lightmap2openglz(iz));
                if squaredist <= squarecheck then
                begin
                  if l.shadow then
                    rtlp.shadow := true
                  else
                  begin
                    rtlp.r := rtlp.r + l.r;
                    rtlp.g := rtlp.g + l.g;
                    rtlp.b := rtlp.b + l.b;
                  end;
                  inc(rtlp.inter);
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
        markp.y1 := iy1;
        markp.z1 := iz1;
        markp.x2 := ix2;
        markp.y2 := iy2;
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
    for iy := markp.y1 to markp.y2 do
    begin
      i_xy := iy * LIGHTMAPSIZEX + markp.x1;
      for iz := markp.z1 to markp.z2 do
      begin
        rtlp := @rtllightmap[iz * (LIGHTMAPSIZEX * LIGHTMAPSIZEY) + i_xy];
        for ix := markp.x1 to markp.x2 do
        begin
          if rtlp.inter = 1 then
          begin
            if rtlp.shadow then
            begin
              point^ := 80 + trunc(rtlp.r * 64);
              inc(point);
              point^ := 80 + trunc(rtlp.g * 64);
              inc(point);
              point^ := 80 + trunc(rtlp.b * 64);
              inc(point);
            end
            else
            begin
              point^ := 128 + trunc(rtlp.r * 127);
              inc(point);
              point^ := 128 + trunc(rtlp.g * 127);
              inc(point);
              point^ := 128 + trunc(rtlp.b * 127);
              inc(point);
            end;
          end
          else if rtlp.inter > 1 then
          begin
            rgb_max := rtlp.r;
            if rtlp.g > rgb_max then
              rgb_max := rtlp.g;
            if rtlp.b > rgb_max then
              rgb_max := rtlp.b;
            if rgb_max > 1.0 then
            begin
              rtlp.r := rtlp.r / rgb_max;
              rtlp.g := rtlp.g / rgb_max;
              rtlp.b := rtlp.b / rgb_max;
            end;
            if rtlp.shadow then
            begin
              point^ := 80 + trunc(rtlp.r * 64);
              inc(point);
              point^ := 80 + trunc(rtlp.g * 64);
              inc(point);
              point^ := 80 + trunc(rtlp.b * 64);
              inc(point);
            end
            else
            begin
              point^ := 128 + trunc(rtlp.r * 127);
              inc(point);
              point^ := 128 + trunc(rtlp.g * 127);
              inc(point);
              point^ := 128 + trunc(rtlp.b * 127);
              inc(point);
            end;
          end
          else
          begin
            PWord(point)^ := $8080;
            inc(point, 2);
            point^ := 128;
            inc(point);
          end;
          inc(rtlp);
        end;
      end;
    end;
    glTexSubImage3D(GL_TEXTURE_3D, 0, markp.x1, markp.y1, markp.z1, markp.dx, markp.dy, markp.dz, GL_RGB, GL_UNSIGNED_BYTE, lightmapbuffer);
  end;

end;

//
// gld_PlaceLightmapTexture
//
// Texture positioning in world coordinates
//
procedure gld_PlaceLightmapTexture;
begin
  glMatrixMode(GL_TEXTURE);
  glPushMatrix;
  glTranslatef(-l3dx * MAP_COEFF / (LIGHTMAPSIZEX * LIGHTMAPUNIT),
               -l3dy * MAP_COEFF / (LIGHTMAPSIZEY * LIGHTMAPUNIT),
               -l3dz * MAP_COEFF / (LIGHTMAPSIZEZ * LIGHTMAPUNIT));

  glMatrixMode(GL_MODELVIEW);
end;

//
// JVAL
//
// Activate Lightmap 3D texture, calls gld_CalculateLightmap
//
procedure gld_ActivateLightmap;
var
  TexGenSPlane, TexGenTPlane, TexGenRPlane: TVector4f;
begin
  glActiveTextureARB(GL_TEXTURE1_ARB);
  glEnable(GL_TEXTURE_3D);

  glTexEnvi(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE_ARB);
  glTexEnvi(GL_TEXTURE_ENV, GL_RGB_SCALE_ARB, 2);
  glPixelStorei(GL_UNPACK_ALIGNMENT, 1);

  glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_S, GL_REPEAT);
  glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_T, GL_REPEAT);
  glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_R, GL_REPEAT);

  glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MAG_FILTER, gl_tex_filter);
  glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MIN_FILTER, gl_tex_filter);

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
  	glBindTexture(GL_TEXTURE_3D, lightmap_tex_num);
  gld_CalculateLightmap;
  gld_PlaceLightmapTexture;

  glActiveTextureARB(GL_TEXTURE0_ARB);
  glEnable(GL_TEXTURE_2D);
end;

//
// JVAL
//
// Deactivate Lightmap
//
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

procedure gld_PauseLightmap;
begin
  glActiveTextureARB(GL_TEXTURE1_ARB);
  glDisable(GL_TEXTURE_3D);
  glActiveTextureARB(GL_TEXTURE0_ARB);
end;

procedure gld_ResumeLightmap;
begin
  glActiveTextureARB(GL_TEXTURE1_ARB);
  glEnable(GL_TEXTURE_3D);
  glActiveTextureARB(GL_TEXTURE0_ARB);
end;

end.


