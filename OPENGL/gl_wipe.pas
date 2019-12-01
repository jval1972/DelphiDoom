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
//  Foundation, inc., 59 Temple Place - Suite 330, Boston, MA
//  02111-1307, USA.
//
// DESCRIPTION:
//  Mission start screen wipe/melt, special effects.
//  Mission begin melt/wipe screen special effect.
//
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit gl_wipe;

interface         {

procedure gld_wipe_StartScreen;

procedure gld_wipe_EndScreen;

function gld_wipe_Ticker(ticks: integer): boolean;
                                                      }

implementation

uses
  d_delphi,
  doomdef,
  dglOpenGL,
  m_fixed,
  m_rnd,
  gl_tex,
  z_zone;

var
  wipe_scr_start_tex: GLuint = 0;
  wipe_scr_end_tex: GLuint = 0;

var
  yy: Pfixed_tArray;
  vy: fixed_t;

procedure gld_wipe_initMelt;
var
  i, r: integer;
  py, py1: Pfixed_t;
begin
  // setup initial column positions
  // (y<0 => not ready to scroll yet)
  yy := Z_Malloc(SCREENWIDTH * SizeOf(fixed_t), PU_STATIC, nil);
  py := @yy[0];
  py1 := py;
  py^ := -(M_Random mod 16);
  for i := 1 to SCREENWIDTH - 1 do
  begin
    inc(py);
    r := (M_Random mod 3) - 1;
    py^ := py1^ + r;
    if py^ > 0 then
      py^ := 0
    else if py^ = -16 then
      py^ := -15;
    inc(py1);
  end;

// JVAL change wipe timing
  vy := FRACUNIT * SCREENHEIGHT div 200;
  py := @yy[0];
  for i := 0 to SCREENWIDTH - 1 do
  begin
    py^ := py^ * vy;
    inc(py);
  end;

end;


function gld_CaptureScreenAsTexID: GLuint;
begin
  glActiveTextureARB(GL_TEXTURE0_ARB);

  glGenTextures(1, @result);
  glBindTexture(GL_TEXTURE_2D, result);

  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);

  glTexImage2D(GL_TEXTURE_2D, 0, 3,
    gld_GetTexDimension(SCREENWIDTH), gld_GetTexDimension(SCREENHEIGHT),
    0, GL_RGB, GL_UNSIGNED_BYTE, nil);

  glCopyTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, 0, 0, SCREENWIDTH, SCREENHEIGHT);
end;

function gld_wipe_doMelt(ticks: Integer): integer;
var
  i: integer;
  total_w, total_h: integer;
  fU1, fU2, fV1, fV2: float;
  tx, sx, sy: float;
  yoffs: integer;
  dy: fixed_t;
  py: Pfixed_t;
begin
  result := 1;

  while ticks > 0 do
  begin
    py := @yy[0];
    for i := 0 to SCREENWIDTH - 1 do
    begin
      if py^ < 0 then
      begin
        py^ := py^ + vy;
        result := 0;
      end
      else if py^ < SCREENHEIGHT * FRACUNIT then
      begin
        if py^ <= 15 * vy then
          dy := py^ + vy
        else
          dy := 8 * vy;
        if (py^ + dy) >= SCREENHEIGHT * FRACUNIT then
          dy := SCREENHEIGHT * FRACUNIT - py^;
        py^ := py^ + dy;

        result := 0;
      end;
      inc(py);
    end;
    dec(ticks);
  end;


  total_w := gld_GetTexDimension(SCREENWIDTH);
  total_h := gld_GetTexDimension(SCREENHEIGHT);

  fU1 := 0.0;
  fV1 := SCREENHEIGHT / total_h;
  fU2 := SCREENWIDTH / total_w;
  fV2 := 0.0;

  glActiveTextureARB(GL_TEXTURE0_ARB);

  glBindTexture(GL_TEXTURE_2D, wipe_scr_end_tex);
  glColor3f(1.0, 1.0, 1.0);

  glBegin(GL_TRIANGLE_STRIP);
    glTexCoord2f(fU1, fV1); glVertex2f(0.0, 0.0);
    glTexCoord2f(fU1, fV2); glVertex2f(0.0, SCREENHEIGHT);
    glTexCoord2f(fU2, fV1); glVertex2f(SCREENWIDTH, 0.0);
    glTexCoord2f(fU2, fV2); glVertex2f(SCREENWIDTH, SCREENHEIGHT);
  glEnd();

  glBindTexture(GL_TEXTURE_2D, wipe_scr_start_tex);
  glColor3f(1.0, 1.0, 1.0);

  glBegin(GL_QUAD_STRIP);

  for i := 0 to SCREENWIDTH - 1 do
  begin
    yoffs := yy[i];
    if yoffs < 0 then
      yoffs := 0;

    tx := i / total_w;
    sx := i;
    sy := yoffs;

    glTexCoord2f(tx, fV1); glVertex2f(sx, sy);
    glTexCoord2f(tx, fV2); glVertex2f(sx, sy + SCREENHEIGHT);
  end;

  glEnd();
end;

procedure gld_wipe_exitMelt;
begin
  Z_Free(yy);

  if wipe_scr_start_tex <> 0 then
  begin
    glDeleteTextures(1, @wipe_scr_start_tex);
    wipe_scr_start_tex := 0;
  end;

  if wipe_scr_end_tex <> 0 then
  begin
    glDeleteTextures(1, @wipe_scr_end_tex);
    wipe_scr_end_tex := 0;
  end;

  gld_ResetLastTexture();
end;

procedure gld_wipe_StartScreen;
begin
  wipe_scr_start_tex := gld_CaptureScreenAsTexID();
end;

procedure gld_wipe_EndScreen;
begin
  glFlush();
  wipe_scr_end_tex := gld_CaptureScreenAsTexID();
end;

// when zero, stop the wipe
var
  wiping: boolean = false;

function gld_wipe_Ticker(ticks: integer): boolean;
begin
  // initial stuff
  if not wiping then
  begin
    wiping := true;
    gld_wipe_initMelt;
  end;

  // do a piece of wipe-in
  if gld_wipe_doMelt(ticks) <> 0 then
  begin
    // final stuff
    wiping := false;
    gld_wipe_exitMelt;
  end;

  result := not wiping;
end;

end.
 