//------------------------------------------------------------------------------
//
//  DelphiHeretic: A modified and improved Heretic port for Windows
//  based on original Linux Doom as published by "id Software", on
//  Heretic source as published by "Raven" software and DelphiDoom
//  as published by Jim Valavanis.
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
// DESCRIPTION:
// Game completion, final screen animation.
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit f_finale;

interface

uses
  doomtype,
  d_event,
  info_h;

function F_Responder(ev: Pevent_t): boolean;

{ Called by main loop. }
procedure F_Ticker;

{ Called by main loop. }
procedure F_Drawer;

procedure F_StartFinale;


var
  bgflatE1: string = 'FLOOR25';   // end of Heretic Episode 1
  bgflatE2: string = 'FLATHUH1';  // end of Heretic Episode 2
  bgflatE3: string = 'FLTWAWA2';  // end of Heretic Episode 3
  bgflatE4: string = 'FLOOR28';   // end of Heretic Episode 4
  bgflatE5: string = 'FLOOR08';   // end of Heretic Episode 5

implementation

uses
  d_delphi,
{$IFDEF OPENGL}
  gl_main,
{$ELSE}
  i_video,
{$ENDIF}
  am_map,
  d_player,
  d_main,
  g_game,
  m_menu,
  info,
  p_pspr,
  r_data,
  r_defs,
  r_things,
// Functions.
  i_system,
  z_zone,
  v_data,
  v_video,
  w_wad,
  s_sound,
// Data.
  h_strings,
  sounds,
  doomdef,
  doomstat,
  hu_stuff;

var
// Stage of animation:
//  0 = text, 1 = art screen, 2 = character cast
  finalestage: integer;

  finalecount: integer;

  yval: integer = 0;
  nextscroll: integer = 0;

const
  TEXTSPEED = 3;
  TEXTWAIT = 250;

var
  finaletext: string;
  finaleflat: string;

procedure F_StartFinale;
begin
  gameaction := ga_nothing;
  gamestate := GS_FINALE;
  viewactive := false;
  amstate := am_inactive;

  S_ChangeMusic(Ord(mus_cptd), true);

  case gameepisode of
    1:
      begin
        finaleflat := bgflatE1;
        finaletext := E1TEXT;
      end;
    2:
      begin
        finaleflat := bgflatE2;
        finaletext := E2TEXT;
      end;
    3:
      begin
        finaleflat := bgflatE3;
        finaletext := E3TEXT;
      end;
    4:
      begin
        finaleflat := bgflatE4;
        finaletext := E4TEXT;
      end;
    5:
      begin
        finaleflat := bgflatE5;
        finaletext := E5TEXT;
      end;
  else
    // Ouch.
  end;

  finalestage := 0;
  finalecount := 0;
  yval := 0;
  nextscroll := 0;
end;

function F_Responder(ev: Pevent_t): boolean;
begin
  if ev._type <> ev_keydown then
  begin
    result := false;
    exit;
  end;

  if (finalestage = 1) and (gameepisode = 2) then
  begin // we're showing the water pic, make any key kick to demo mode
    inc(finalestage);
    result := true;
  end
  else
    result := false;
end;

//
// F_Ticker
//
procedure F_Ticker;
begin
  // advance animation
  inc(finalecount);

  if (finalestage = 0) and (finalecount > Length(finaletext) * TEXTSPEED + TEXTWAIT) then
  begin
    finalecount := 0;
    finalestage := 1;
    wipegamestate := -1;    // force a wipe
  end;
end;

procedure F_TextWrite;
var
  src: PByteArray;
  dest: integer;
  x, y, w: integer;
  count: integer;
  ch: string;
  c: char;
  c1: integer;
  i: integer;
  len: integer;
  cx: integer;
  cy: integer;
begin
  // erase the entire screen to a tiled background

  src := W_CacheLumpNum(R_GetLumpForFlat(R_FlatNumForName(finaleflat)), PU_STATIC);
  dest := 0;

  for y := 0 to 200 - 1 do
  begin
    for x := 0 to (320 div 64) - 1 do
    begin
      memcpy(@screens[SCN_TMP, dest], @src[_SHL(y and 63, 6)], 64);
      dest := dest + 64;
    end;

    if 320 and 63 <> 0 then
    begin
      memcpy(@screens[SCN_TMP, dest], @src[_SHL(y and 63, 6)], 320 and 63);
      dest := dest + (320 and 63);
    end;
  end;
  Z_ChangeTag(src, PU_CACHE);

  // draw some of the text onto the screen
  cx := 20;
  cy := 5;
  ch := finaletext;
  len := Length(ch);

  count := (finalecount - 10) div TEXTSPEED;
  if count < 0 then
    count := 0;

  i := 1;
  while count > 0 do
  begin

    if i > len then
      break;

    c := ch[i];
    inc(i);
    if c = #13 then
    begin
      cy := cy + 9;
      continue;
    end;
    if c = #10 then
    begin
      cx := 20;
      continue;
    end;

    c1 := Ord(toupper(c)) - Ord(HU_FONTSTART);
    if (c1 < 0) or (c1 > HU_FONTSIZE) then
    begin
      cx := cx + 5;
      continue;
    end;

    w := hu_font[c1].width;
    if cx + w > 320 then
      break;
    V_DrawPatch(cx, cy, SCN_TMP, hu_font[c1], false);
    cx := cx + w;
    dec(count);
  end;
  V_CopyRect(0, 0, SCN_TMP, 320, 200, 0, 0, SCN_FG, true);

  V_FullScreenStretch;
end;

procedure F_DemonScroll;
var
  p1, p2: PByteArray;
begin
  if finalecount < nextscroll then
    exit;

  if finalecount < 70 then
  begin
    p1 := W_CacheLumpName('FINAL1', PU_STATIC);
    memcpy(screens[SCN_TMP], p1, 320 * 200);
    Z_ChangeTag(p1, PU_CACHE);
    nextscroll := finalecount;
  end
  else if yval < 64000 then
  begin
    p1 := W_CacheLumpName('FINAL1', PU_STATIC);
    p2 := W_CacheLumpName('FINAL2', PU_STATIC);
    memcpy(screens[SCN_TMP], @p2[320 * 200 - yval], yval);
    memcpy(@screens[SCN_TMP][yval], p1, 320 * 200 - yval);
    Z_ChangeTag(p1, PU_CACHE);
    Z_ChangeTag(p2, PU_CACHE);
    yval := yval + 320;
    nextscroll := finalecount + 3;
  end
  else  //else, we'll just sit here and wait, for now
  begin
    p2 := W_CacheLumpName('FINAL2', PU_STATIC);
    memcpy(screens[SCN_TMP], p2, 320 * 200);
    Z_ChangeTag(p2, PU_CACHE);
  end;

  V_CopyRect(0, 0, SCN_TMP, 320, 200, 0, 0, SCN_FG, true);

  V_FullScreenStretch;
end;

var
  underwaterpic: integer = -2;
  underwaterpal: integer = -2;

procedure F_DrawUnderwaterPic;
var
  pal: PByteArray;
begin
  if underwaterpic = -2 then
  begin
    underwaterpic := W_CheckNumForName('E2END');
    underwaterpal := W_CheckNumForName('E2PAL');
    if (underwaterpic = -1) or (underwaterpal = -1) then
    begin
      underwaterpic := -1;
      underwaterpal := -1;
    end;
  end;

  if underwaterpic = -1 then
  begin
    V_PageDrawer(pg_CREDIT);
  end
  else
  begin
    memcpy(@screens[SCN_TMP][0], W_CacheLumpNum(underwaterpic, PU_CACHE), 320 * 200);
    pal := W_CacheLumpNum(underwaterpal, PU_STATIC);
    I_SetPalette(pal);
    V_SetPalette(pal);
    Z_ChangeTag(pal, PU_CACHE);
    V_CopyRect(0, 0, SCN_TMP, 320, 200, 0, 0, SCN_FG, true);
    V_FullScreenStretch;
  end;
end;

procedure F_DrawUnderwater;
var
  pal: PByteArray;
begin
  case finalestage of
    1:
      begin
        menuactive := false;
        paused := false;
        messageToPrint := 0;
        F_DrawUnderwaterPic;
      end;
    2:
      begin
        pal := V_ReadPalette(PU_STATIC);
        V_SetPalette(pal);
        I_SetPalette(pal);
        Z_ChangeTag(pal, PU_CACHE);
        V_PageDrawer(pg_CREDIT);
      end;
  end;
end;

//
// F_Drawer
//
procedure F_Drawer;
begin
  if finalestage = 0 then
  begin
    F_TextWrite;
    exit;
  end;

  case gameepisode of
    1:
      begin
        if gamemode = shareware then
          V_PageDrawer(pg_ORDER)
        else
          V_PageDrawer(pg_CREDIT);
      end;
    2: F_DrawUnderwater;
    3: F_DemonScroll;
    else
      begin
        V_PageDrawer(pg_CREDIT);
      end;
  end;

end;

end.

