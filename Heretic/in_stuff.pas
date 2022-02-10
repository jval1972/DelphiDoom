//------------------------------------------------------------------------------
//
//  DelphiHeretic is a source port of the game Heretic and it is
//  based on original Linux Doom as published by "id Software", on
//  Heretic source as published by "Raven" software and DelphiDoom
//  as published by Jim Valavanis.
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
// DESCRIPTION:
//    Intermission stuff
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit in_stuff;

interface

uses
  d_player;

//==============================================================================
//
// IN_Start
//
//==============================================================================
procedure IN_Start(const wbstartstruct: Pwbstartstruct_t);

//==============================================================================
//
// IN_Ticker
//
//==============================================================================
procedure IN_Ticker;

//==============================================================================
//
// IN_Drawer
//
//==============================================================================
procedure IN_Drawer;

implementation

uses
  d_delphi,
  doomdef,
  doomstat,
  am_map,
  d_event,
  g_game,
  mt_utils,
  hu_stuff,
  i_system,
{$IFDEF OPENGL}
  gl_main,
{$ELSE}
  i_video,
{$ENDIF}
  m_fixed,
  m_menu,
  p_tick,
  p_setup,
  r_defs,
  sb_bar,
  s_sound,
  sounddata,
  v_data,
  v_video,
  w_wad,
  z_zone;

type
  gametype_t = (
    gt_single,
    gt_cooperative,
    gt_deathmatch
  );

// Public functions

var
  intermission: boolean;

var
  skipintermission: boolean;
  interstate: integer = 0;
  intertime: integer = -1;
  oldintertime: integer = 0;
  gametype: gametype_t;

  cnt: integer;

  time: integer;
  hours: integer;
  minutes: integer;
  seconds: integer;

  slaughterboy: integer; // in DM, the player with the most kills

var
  killPercent: array[0..MAXPLAYERS - 1] of integer;
  bonusPercent: array[0..MAXPLAYERS - 1] of integer;
  secretPercent: array[0..MAXPLAYERS - 1] of integer;

var
  patchINTERPIC: Ppatch_t;
  patchBEENTHERE: Ppatch_t;
  patchGOINGTHERE: Ppatch_t;
  FontBNumbers: array[0..9] of Ppatch_t;
  FontBNegative: Ppatch_t;
  FontBSlash: Ppatch_t;
  FontBPercent: Ppatch_t;

var
  FontBLump: integer;
  FontBLumpBase: integer;
  patchFaceOkayBase: integer;
  patchFaceDeadBase: integer;

var
  totalFrags: array[0..MAXPLAYERS - 1] of integer;
  dSlideX: array[0..MAXPLAYERS - 1] of fixed_t;
  dSlideY: array[0..MAXPLAYERS - 1] of fixed_t;

const
  KillersText: array[0..6] of char = ('K', 'I', 'L', 'L', 'E', 'R', 'S');

type
  yahpt_t = record
    x, y: integer;
  end;

const
  YAHspot: array[0..2, 0..8] of yahpt_t = (
  (
    (x: 172; y:  78),
    (x:  86; y:  90),
    (x:  73; y:  66),
    (x: 159; y:  95),
    (x: 148; y: 126),
    (x: 132; y:  54),
    (x: 131; y:  74),
    (x: 208; y: 138),
    (x:  52; y: 101 )
 ),
  (
    (x: 218; y:  57),
    (x: 137; y:  81),
    (x: 155; y: 124),
    (x: 171; y:  68),
    (x: 250; y:  86),
    (x: 136; y:  98),
    (x: 203; y:  90),
    (x: 220; y: 140),
    (x: 279; y: 106)
 ),
  (
    (x:  86; y:  99),
    (x: 124; y: 103),
    (x: 154; y:  79),
    (x: 202; y:  83),
    (x: 178; y:  59),
    (x: 142; y:  58),
    (x: 219; y:  66),
    (x: 247; y:  57),
    (x: 107; y:  80)
  ));

var
  wbs: Pwbstartstruct_t = nil;
  exitpic, enterpic: string; // UMAPINFO

//========================================================================
//
// IN_LoadPics
//
//========================================================================
//
//==============================================================================
procedure IN_LoadPics;
var
  i: integer;
begin
  case gameepisode of
    1: patchINTERPIC := W_CacheLumpName('MAPE1', PU_STATIC);
    2: patchINTERPIC := W_CacheLumpName('MAPE2', PU_STATIC);
    3: patchINTERPIC := W_CacheLumpName('MAPE3', PU_STATIC);
  else
    patchINTERPIC := nil;
  end;

  patchBEENTHERE := W_CacheLumpName('IN_X', PU_STATIC);
  patchGOINGTHERE := W_CacheLumpName('IN_YAH', PU_STATIC);
  FontBLumpBase := W_GetNumForName('FONTB16');
  for i := 0 to 9 do
    FontBNumbers[i] := W_CacheLumpNum(FontBLumpBase + i, PU_STATIC);

  FontBLump := W_GetNumForName('FONTB_S') + 1;
  FontBNegative := W_CacheLumpName('FONTB13', PU_STATIC);

  FontBSlash := W_CacheLumpName('FONTB15', PU_STATIC);
  FontBPercent := W_CacheLumpName('FONTB05', PU_STATIC);
  patchFaceOkayBase := W_GetNumForName('FACEA0');
  patchFaceDeadBase := W_GetNumForName('FACEB0');
end;

//==============================================================================
//
// IN_CheckCustomPics
//
//==============================================================================
procedure IN_CheckCustomPics;
begin
  if (wbs.lastmapinfo <> nil) and (wbs.lastmapinfo.exitpic <> '') then
  begin
    exitpic := wbs.lastmapinfo.exitpic;
    if exitpic = '-' then
      exitpic := ''
    else if W_CheckNumForName(exitpic) < 0 then
      exitpic := '';
  end
  else
    exitpic := '';

  if (wbs.nextmapinfo <> nil) and (wbs.nextmapinfo.enterpic <> '') then
  begin
    enterpic := wbs.nextmapinfo.enterpic;
    if enterpic = '-' then
      enterpic := ''
    else if W_CheckNumForName(enterpic) < 0 then
      enterpic := '';
  end
  else
    enterpic := '';
end;

//========================================================================
//
// IN_InitStats
//
//      Initializes the stats for single player mode
//========================================================================
//
//==============================================================================
procedure IN_InitStats;
var
  i: integer;
  j: integer;
  slaughterfrags: integer;
  posnum: integer;
  slaughtercount: integer;
  playercount: integer;
begin
  if not netgame then
  begin
    gametype := gt_single;
    time := leveltime div TICRATE;
    hours := time div 3600;
    time := time - hours * 3600;
    minutes := time div 60;
    time := time - minutes * 60;
    seconds := time;
  end
  else if netgame and (deathmatch = 0) then
  begin
    gametype := gt_cooperative;
    memset(@killPercent, 0, MAXPLAYERS * SizeOf(integer));
    memset(@bonusPercent, 0, MAXPLAYERS * SizeOf(integer));
    memset(@secretPercent, 0, MAXPLAYERS * SizeOf(integer));
    for i := 0 to MAXPLAYERS - 1 do
    begin
      if playeringame[i] then
      begin
        if totalkills <> 0 then
          killPercent[i] := players[i].killcount * 100 div totalkills;
        if totalitems <> 0 then
          bonusPercent[i] := players[i].itemcount* 100 div totalitems;
        if totalsecret <> 0 then
          secretPercent[i] := players[i].secretcount * 100 div totalsecret;
      end;
    end;
  end
  else
  begin
    gametype := gt_deathmatch;
    slaughterboy := 0;
    slaughterfrags := -9999;
    posnum := 0;
    playercount := 0;
    slaughtercount := 0;
    for i := 0 to MAXPLAYERS - 1 do
    begin
      totalFrags[i] := 0;
      if playeringame[i] then
      begin
        inc(playercount);
        for j := 0 to MAXPLAYERS - 1 do
          if playeringame[j] then
            totalFrags[i] := totalFrags[i] + players[i].frags[j];
        dSlideX[i] := (43 * posnum * FRACUNIT) div 20;
        dSlideY[i] := (36 * posnum * FRACUNIT) div 20;
        inc(posnum);
      end;
      if totalFrags[i] > slaughterfrags then
      begin
        slaughterboy := 1 shl i;
        slaughterfrags := totalFrags[i];
        slaughtercount := 1;
      end
      else if totalFrags[i] = slaughterfrags then
      begin
        slaughterboy := slaughterboy or (1 shl i);
        inc(slaughtercount);
      end;
    end;
    if playercount = slaughtercount then
    begin // don't do the slaughter stuff if everyone is equal
      slaughterboy := 0;
    end;
  end;
end;

//========================================================================
//
// IN_Start
//
//========================================================================
//
//==============================================================================
procedure IN_Start(const wbstartstruct: Pwbstartstruct_t);
var
  palette: PByteArray;
begin
  palette := V_ReadPalette(PU_STATIC);
  I_SetPalette(palette);
  V_SetPalette(palette);
  Z_ChangeTag(palette, PU_CACHE);

  wbs := wbstartstruct;

  IN_CheckCustomPics;
  IN_LoadPics;
  IN_InitStats;
  intermission := true;
  interstate := -1;
  skipintermission := false;
  intertime := 0;
  oldintertime := 0;
  AM_Stop;
  S_ChangeMusic(Ord(mus_intr), true)
end;

//========================================================================
//
// IN_UnloadPics
//
//========================================================================
//
//==============================================================================
procedure IN_UnloadPics;
var
  i: integer;
begin
  if patchINTERPIC <> nil then
    Z_ChangeTag(patchINTERPIC, PU_CACHE);
  Z_ChangeTag(patchBEENTHERE, PU_CACHE);
  Z_ChangeTag(patchGOINGTHERE, PU_CACHE);
  for i := 0 to 9 do
    Z_ChangeTag(FontBNumbers[i], PU_CACHE);
  Z_ChangeTag(FontBNegative, PU_CACHE);
  Z_ChangeTag(FontBSlash, PU_CACHE);
  Z_ChangeTag(FontBPercent, PU_CACHE);
end;

//========================================================================
//
// IN_Stop
//
//========================================================================
//
//==============================================================================
procedure IN_Stop;
begin
  intermission := false;
  IN_UnloadPics;
  SB_state := -1;
end;

//========================================================================
//
// IN_WaitStop
//
//========================================================================
//
//==============================================================================
procedure IN_WaitStop;
begin
  dec(cnt);
  if cnt <= 0 then
  begin
    IN_Stop;
    G_WorldDone;
  end;
end;

//========================================================================
//
// IN_CheckForSkip
//
//      Check to see if any player hit a key
//========================================================================
//
//==============================================================================
procedure IN_CheckForSkip;
var
  i: integer;
  player: Pplayer_t;
begin
  for i := 0 to MAXPLAYERS - 1 do
  begin
    if playeringame[i] then
    begin
      player := @players[i];
      if player.cmd.buttons and BT_ATTACK <> 0 then
      begin
        if not player.attackdown then
          skipintermission := true;
        player.attackdown := true;
      end
      else
        player.attackdown := false;
      if player.cmd.buttons and BT_USE <> 0 then
      begin
        if not player.usedown then
          skipintermission := true;
        player.usedown := true;
      end
      else
        player.usedown := false;
    end;
  end;
end;

//========================================================================
//
// IN_Ticker
//
//========================================================================
//
//==============================================================================
procedure IN_Ticker;
begin
  if not intermission then
    exit;

  if interstate = 3 then
  begin
    IN_WaitStop;
    exit;
  end;

  IN_CheckForSkip;

  inc(intertime);
  if oldintertime < intertime then
  begin
    inc(interstate);
    if (gameepisode > 3) and (interstate >= 1) then
    begin // Extended Wad levels:  skip directly to the next level
      interstate := 3;
    end;
    case interstate of
      0:
        begin
          oldintertime := intertime + 300;
          if gameepisode > 3 then
            oldintertime := intertime + 1200;
        end;
      1:
        begin
          oldintertime := intertime + 200;
        end;
      2:
        begin
          oldintertime := MAXINT;
        end;
      3:
        begin
          cnt := 10;
        end;
    end;
  end;

  if skipintermission then
  begin
    if (interstate = 0) and (intertime < 150) then
    begin
      intertime := 150;
      skipintermission := false;
      exit;
    end
    else if (interstate < 2) and (gameepisode < 4) then
    begin
      interstate := 2;
      skipintermission := false;
      S_StartSound(nil, Ord(sfx_dorcls));
      exit;
    end;
    interstate := 3;
    cnt := 10;
    skipintermission := false;
    S_StartSound(nil, Ord(sfx_dorcls));
  end;
end;

//========================================================================
//
// IN_DrawStatBack
//
//========================================================================
//
//==============================================================================
procedure IN_DrawStatBack;
var
  x: integer;
  y: integer;
  src: PByteArray;
  dest: integer;
begin
  if exitpic <> '' then
    V_TileScreen8(exitpic, SCN_TMP)
  else
  begin
    src := W_CacheLumpName ('FLOOR16', PU_STATIC);
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
  end;
end;

//========================================================================
//
// IN_DrawSingleStats
//
//========================================================================

//========================================================================
//
// IN_DrTextB
//
//========================================================================
//
//==============================================================================
procedure IN_DrTextB(const txt: string; x, y: integer);
var
  i: integer;
  c: char;
  p: Ppatch_t;
begin
  i := 1;
  while i <= Length(txt) do
  begin
    c := toupper(txt[i]);
    inc(i);
    if Ord(c) < 33 then
      x := x + 8
    else
    begin
      p := W_CacheLumpNum(FontBLump + Ord(c) - 33, PU_CACHE);
      V_DrawPatch(x, y, SCN_TMP, p, false); // V_DrawShadowedPatch
      x := x + p.width - 1;
    end;
  end;
end;

//========================================================================
//
// IN_DrawNumber
//
//========================================================================
//
//==============================================================================
procedure IN_DrawNumber(val: integer; x, y: integer; digits: integer);
var
  patch: Ppatch_t;
  xpos: integer;
  oldval: integer;
  realdigits: integer;
  neg: boolean;
begin
  oldval := val;
  xpos := x;
  neg := false;
  realdigits := 1;

  if val < 0 then
  begin //...this should reflect negative frags
    val := -val;
    neg := true;
    if val > 99 then
      val := 99;
  end;
  if val > 9 then
  begin
    inc(realdigits);
    if digits < realdigits then
    begin
      realdigits := digits;
      val := 9;
    end;
  end;
  if val > 99 then
  begin
    inc(realdigits);
    if digits < realdigits then
    begin
      realdigits := digits;
      val := 99;
    end;
  end;
  if val > 999 then
  begin
    inc(realdigits);
    if digits < realdigits then
    begin
      realdigits := digits;
      val := 999;
    end;
  end;
  if digits = 4 then
  begin
    patch := FontBNumbers[val div 1000];
    V_DrawPatch(xpos + 6 - patch.width div 2 - 12, y, SCN_TMP, patch, false); // V_DrawShadowedPatch
  end;
  if digits > 2 then
  begin
    if realdigits > 2 then
    begin
      patch := FontBNumbers[val div 100];
      V_DrawPatch(xpos + 6 - patch.width div 2, y, SCN_TMP, patch, false); // V_DrawShadowedPatch
    end;
    xpos := xpos + 12;
  end;
  val := val mod 100;
  if digits > 1 then
  begin
    if val > 9 then
    begin
      patch := FontBNumbers[val div 10];
      V_DrawPatch(xpos + 6 - patch.width div 2, y, SCN_TMP, patch, false); // V_DrawShadowedPatch
    end
    else if (digits = 2) or (oldval > 99) then
      V_DrawPatch(xpos, y, SCN_TMP, FontBNumbers[0], false); // V_DrawShadowedPatch
    xpos := xpos + 12;
  end;
  val := val mod 10;
  patch := FontBNumbers[val];
  V_DrawPatch(xpos + 6 - patch.width div 2, y, SCN_TMP, patch, false); // V_DrawShadowedPatch
  if neg then
  begin
    patch := FontBNegative;
    V_DrawPatch(xpos + 6 - patch.width div 2 - 12 * realdigits, y, SCN_TMP, patch, false); // V_DrawShadowedPatch
  end;
end;

//========================================================================
//
// IN_DrawTime
//
//========================================================================
//
//==============================================================================
procedure IN_DrawTime(x, y: integer; h, m, s: integer);
begin
  if h <> 0 then
  begin
    IN_DrawNumber(h, x, y, 2);
    IN_DrTextB(':', x + 26, y);
  end;
  x := x + 34;
  if (m <> 0) or (h <> 0) then
    IN_DrawNumber(m, x, y, 2);
  x := x + 34;
  if s <> 0 then
  begin
    IN_DrTextB(':', x - 8, y);
    IN_DrawNumber(s, x, y, 2);
  end;
end;

//==============================================================================
//
// IN_DrawFinished
//
//==============================================================================
procedure IN_DrawFinished;
var
  x, y: integer;
  lpic: Ppatch_t;
  leveltitle: string;
begin
  y := 3;

  // The level defines a new name but no texture for the name.
  if (wbs.lastmapinfo <> nil) and (wbs.lastmapinfo.levelname <> '') and (wbs.lastmapinfo.levelpic = '') then
  begin
    x := 160 - M_StringWidth2(wbs.lastmapinfo.levelname) div 2;
    IN_DrTextB(wbs.lastmapinfo.levelname, x, 3);

    y := 25;
  end
  else if (wbs.lastmapinfo <> nil) and (wbs.lastmapinfo.levelpic <> '') then
  begin
    lpic := W_CacheLumpName(wbs.lastmapinfo.levelpic, PU_CACHE);

    V_DrawPatch((320 - lpic.width) div 2, y, SCN_TMP, lpic, false);

    y := y + (5 * lpic.height) div 4;
  end
  else
  begin
    leveltitle := P_GetMapTitle(wbs.epsd + 1, wbs.last + 1);
    x := 160 - M_StringWidth2(leveltitle) div 2;
    IN_DrTextB(leveltitle, x, 3);
    y := 25;
  end;

  x := 160 - M_StringWidth('FINISHED') div 2;
  M_WriteText(x, y, 'FINISHED');
end;

//==============================================================================
//
// IN_DrawEntering
//
//==============================================================================
procedure IN_DrawEntering(const y: Integer);
var
  x: integer;
  lpic: Ppatch_t;
  leveltitle: string;
begin
  x := 160 - M_StringWidth('NOW ENTERING:') div 2;
  M_WriteText(x, y, 'NOW ENTERING:');

  // The level defines a new name but no texture for the name.
  if (wbs.nextmapinfo <> nil) and (wbs.nextmapinfo.levelname <> '') and (wbs.nextmapinfo.levelpic = '') then
  begin
    x := 160 - M_StringWidth2(wbs.nextmapinfo.levelname) div 2;
    IN_DrTextB(wbs.nextmapinfo.levelname, x, y + 10);
  end
  else if (wbs.nextmapinfo <> nil) and (wbs.nextmapinfo.levelpic <> '') then
  begin
    lpic := W_CacheLumpName(wbs.nextmapinfo.levelpic, PU_CACHE);

    V_DrawPatch((320 - lpic.width) div 2, y + 10, SCN_TMP, lpic, false);
  end
  else
  begin
    leveltitle := P_GetMapTitle(wbs.nextep + 1, wbs.next + 1);
    x := 160 - M_StringWidth2(leveltitle) div 2;
    IN_DrTextB(leveltitle, x, y + 10);
  end;
end;

var
  in_sounds: integer;

//==============================================================================
//
// IN_DrawSingleStats
//
//==============================================================================
procedure IN_DrawSingleStats;
begin
  IN_DrTextB('KILLS', 50, 65);
  IN_DrTextB('ITEMS', 50, 90);
  IN_DrTextB('SECRETS', 50, 115);

  IN_DrawFinished;

  if intertime < 30 then
  begin
    in_sounds := 0;
    exit;
  end;

  if (in_sounds < 1) and (intertime >= 30) then
  begin
    S_StartSound(nil, Ord(sfx_dorcls));
    inc(in_sounds);
  end;
  IN_DrawNumber(players[consoleplayer].killcount, 200, 65, 3);
  V_DrawPatch(237, 65, SCN_TMP, FontBSlash, false); // V_DrawShadowedPatch
  IN_DrawNumber(totalkills, 248, 65, 3);

  if intertime < 60 then
    exit;

  if (in_sounds < 2) and (intertime >= 60) then
  begin
    S_StartSound(nil, Ord(sfx_dorcls));
    inc(in_sounds);
  end;

  IN_DrawNumber(players[consoleplayer].itemcount, 200, 90, 3);
  V_DrawPatch(237, 90, SCN_TMP, FontBSlash, false); // V_DrawShadowedPatch
  IN_DrawNumber(totalitems, 248, 90, 3);

  if intertime < 90 then
    exit;

  if (in_sounds < 3) and (intertime >= 90) then
  begin
    S_StartSound(nil, Ord(sfx_dorcls));
    inc(in_sounds);
  end;
  IN_DrawNumber(players[consoleplayer].secretcount, 200, 115, 3);
  V_DrawPatch(237, 115, SCN_TMP, FontBSlash, false); // V_DrawShadowedPatch
  IN_DrawNumber(totalsecret, 248, 115, 3);

  if intertime < 150 then
    exit;

  if (in_sounds < 4) and (intertime >= 150) then
  begin
    S_StartSound(nil, Ord(sfx_dorcls));
    inc(in_sounds);
  end;

  if (gamemode <> extendedwad) or (gameepisode < 4) then
  begin
    IN_DrTextB('TIME', 85, 160);
    IN_DrawTime(155, 160, hours, minutes, seconds);
  end
  else
  begin
    IN_DrawEntering(160);
    skipintermission := false;
  end;
end;

//========================================================================
//
// IN_DrawCoopStats
//
//========================================================================

var
  in_sounds2: integer;

//==============================================================================
//
// IN_DrawCoopStats
//
//==============================================================================
procedure IN_DrawCoopStats;
var
  i: integer;
  ypos: integer;
begin
  IN_DrTextB('KILLS', 95, 35);
  IN_DrTextB('BONUS', 155, 35);
  IN_DrTextB('SECRET', 232, 35);

  IN_DrawFinished;

  ypos := 50;
  for i := 0 to MAXPLAYERS - 1 do
  begin
    if playeringame[i] then
    begin
      V_DrawPatch(25, ypos, SCN_TMP, W_CacheLumpNum(patchFaceOkayBase + i, PU_CACHE), false); // V_DrawShadowedPatch
      if intertime < 40 then
      begin
        in_sounds2 := 0;
        ypos := ypos + 37;
        continue;
      end
      else if (intertime >= 40) and (in_sounds2 < 1) then
      begin
        S_StartSound(nil, Ord(sfx_dorcls));
        inc(in_sounds2);
      end;
      IN_DrawNumber(killPercent[i], 85, ypos + 10, 3);
      V_DrawPatch(121, ypos + 10, SCN_TMP, FontBPercent, false); // V_DrawShadowedPatch
      IN_DrawNumber(bonusPercent[i], 160, ypos+10, 3);
      V_DrawPatch(196, ypos + 10, SCN_TMP, FontBPercent, false);  // V_DrawShadowedPatch
      IN_DrawNumber(secretPercent[i], 237, ypos+10, 3);
      V_DrawPatch(273, ypos + 10, SCN_TMP, FontBPercent, false);  // V_DrawShadowedPatch
      ypos := ypos + 37;
    end;
  end;
end;

//========================================================================
//
// IN_DrawDMStats
//
//========================================================================

var
  in_sounds3: integer;

//==============================================================================
//
// IN_DrawDMStats
//
//==============================================================================
procedure IN_DrawDMStats;
var
  i: integer;
  j: integer;
  ypos: integer;
  xpos: integer;
  kpos: integer;
begin
  xpos := 90;
  ypos := 55;

  IN_DrTextB('TOTAL', 265, 30);
  M_WriteText(140, 8, 'VICTIMS');
  for i := 0 to 6 do
    M_WriteText(10, 80 + 9 * i, KillersText[i]);
  if intertime < 20 then
  begin
    for i := 0 to MAXPLAYERS - 1 do
    begin
      if playeringame[i] then
      begin
        V_DrawPatch(40, ((ypos * FRACUNIT) + dSlideY[i] * intertime) div FRACUNIT,
          SCN_TMP,
            W_CacheLumpNum(patchFaceOkayBase + i, PU_CACHE), false); // V_DrawShadowedPatch
        V_DrawPatch(((xpos * FRACUNIT) + dSlideX[i] * intertime) div FRACUNIT, 18,
          SCN_TMP,
            W_CacheLumpNum(patchFaceDeadBase + i, PU_CACHE), false);  // V_DrawShadowedPatch
      end;
    end;
    in_sounds3 := 0;
    exit;
  end;

  if (intertime >= 20) and (in_sounds3 < 1) then
  begin
    S_StartSound(nil, Ord(sfx_dorcls));
    inc(in_sounds3);
  end;

  if (intertime >= 100) and (slaughterboy <> 0) and (in_sounds3 < 2) then
  begin
    S_StartSound(nil, Ord(sfx_wpnup));
    inc(in_sounds3);
  end;

  for i := 0 to MAXPLAYERS - 1 do
  begin
    if playeringame[i] then
    begin
      if (intertime < 100) or (i = consoleplayer) then
      begin
        V_DrawPatch(40, ypos, SCN_TMP, W_CacheLumpNum(patchFaceOkayBase + i, PU_CACHE), false); // V_DrawShadowedPatch
        V_DrawPatch(xpos, 18, SCN_TMP, W_CacheLumpNum(patchFaceDeadBase + i, PU_CACHE), false); // V_DrawShadowedPatch
      end
      else
      begin
        V_DrawPatch(40, ypos, SCN_TMP, W_CacheLumpNum(patchFaceOkayBase + i, PU_CACHE), false); // V_DrawFuzzPatch
        V_DrawPatch(xpos, 18, SCN_TMP, W_CacheLumpNum(patchFaceDeadBase + i, PU_CACHE), false); // V_DrawFuzzPatch
      end;
      kpos := 86;
      for j := 0 to MAXPLAYERS - 1 do
      begin
        if playeringame[j] then
        begin
          IN_DrawNumber(players[i].frags[j], kpos, ypos + 10, 3);
          kpos := kpos + 43;
        end;
      end;
      if (slaughterboy and (1 shl i)) <> 0 then
      begin
        if intertime and 16 = 0 then
        begin
          IN_DrawNumber(totalFrags[i], 263, ypos + 10, 3);
        end;
      end
      else
      begin
        IN_DrawNumber(totalFrags[i], 263, ypos + 10, 3);
      end;
      ypos := ypos + 36;
      xpos := xpos + 43;
    end;
  end;
end;

//========================================================================
//
// IN_DrawOldLevel
//
//========================================================================
//
//==============================================================================
procedure IN_DrawOldLevel;
var
  i: integer;
begin
  IN_DrawFinished;

  if enterpic = '' then
  begin
    if wbs.last + 1 = 9 then
    begin
      for i := 0 to gamemap - 1 do
      begin
        V_DrawPatch(YAHspot[gameepisode - 1][i].x, YAHspot[gameepisode - 1][i].y,
          SCN_TMP,
            patchBEENTHERE, false);
      end;
      if intertime and 16 = 0 then
      begin
        V_DrawPatch(YAHspot[gameepisode - 1][8].x, YAHspot[gameepisode - 1][8].y,
          SCN_TMP,
            patchBEENTHERE, false);
      end;
    end
    else
    begin
      for i := 0 to wbs.last do
      begin
        V_DrawPatch(YAHspot[gameepisode - 1][i].x, YAHspot[gameepisode - 1][i].y,
          SCN_TMP,
            patchBEENTHERE, false);
      end;
      if players[consoleplayer].didsecret then
      begin
        V_DrawPatch(YAHspot[gameepisode - 1][8].x, YAHspot[gameepisode - 1][8].y,
          SCN_TMP,
            patchBEENTHERE, false);
      end;
      if intertime and 16 = 0 then
      begin
        V_DrawPatch(YAHspot[gameepisode - 1][wbs.last].x, YAHspot[gameepisode - 1][wbs.last].y,
          SCN_TMP,
            patchBEENTHERE, false);
      end;
    end;
  end;
end;

//========================================================================
//
// IN_DrawYAH
//
//========================================================================
//
//==============================================================================
procedure IN_DrawYAH;
var
  i: integer;
  prevmap: integer;
begin
  IN_DrawEntering(10);

  if (enterpic = '') or (wbs.lastmapinfo <> nil) then
  begin
    if wbs.last = 8 then
      prevmap := gamemap - 1
    else
      prevmap := wbs.last;

    for i := 0 to prevmap - 1 do
      V_DrawPatch(YAHspot[gameepisode - 1][i].x, YAHspot[gameepisode - 1][i].y,
        SCN_TMP,
          patchBEENTHERE, false);

    if players[consoleplayer].didsecret then
      V_DrawPatch(YAHspot[gameepisode - 1][8].x, YAHspot[gameepisode - 1][8].y,
        SCN_TMP,
          patchBEENTHERE, false);

    if (intertime and 16 = 0) or (interstate = 3) then
    begin // draw the destination 'X'
      V_DrawPatch(YAHspot[gameepisode - 1][gamemap - 1].x, YAHspot[gameepisode - 1][gamemap - 1].y,
        SCN_TMP,
          patchGOINGTHERE, false);
    end;
  end;
end;

//========================================================================
//
// IN_Drawer
//
//========================================================================

var
  oldinterstate: integer;

//==============================================================================
//
// IN_Drawer
//
//==============================================================================
procedure IN_Drawer;
begin
  if not intermission then
    exit;

  if interstate = 3 then
    exit;

  MT_ZeroMemory(screens[SCN_TMP], 320 * 200);

  if (oldinterstate <> 2) and (interstate = 2) then
    S_StartSound(nil, Ord(sfx_pstop));

  oldinterstate := interstate;
  case interstate of
    0: // draw stats
      begin
        IN_DrawStatBack;
        case gametype of
          gt_single:
            IN_DrawSingleStats;
          gt_cooperative:
            IN_DrawCoopStats;
          gt_deathmatch:
            IN_DrawDMStats;
        end;
      end;
    1: // leaving old level
      begin
        if enterpic <> '' then
        begin
          V_TileScreen8(enterpic, SCN_TMP);
          IN_DrawFinished;
        end
        else if gameepisode < 4 then
        begin
          V_DrawPatchFullScreenTMP320x200(patchINTERPIC);
          IN_DrawOldLevel;
        end;
      end;
    2: // going to the next level
      begin
        if enterpic <> '' then
        begin
          V_TileScreen8(enterpic, SCN_TMP);
          IN_DrawEntering(10);
        end
        else if gameepisode < 4 then
        begin
          V_DrawPatchFullScreenTMP320x200(patchINTERPIC);
          IN_DrawYAH;
        end;
      end;
    3: // waiting before going to the next level
      begin
        if enterpic <> '' then
          V_TileScreen8(enterpic, SCN_TMP)
        else if gameepisode < 4 then
          V_DrawPatchFullScreenTMP320x200(patchINTERPIC);
      end;
   else
     I_Error('IN_Drawer(): Intermission state (%d) out of range.', [interstate]);
  end;

  V_CopyRect(0, 0, SCN_TMP, 320, 200, 0, 0, SCN_FG, true);

  V_FullScreenStretch;
end;

end.
