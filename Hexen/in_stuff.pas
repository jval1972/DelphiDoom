//------------------------------------------------------------------------------
//
//  DelphiHexen is a source port of the game Hexen and it is
//  based on original Linux Doom as published by "id Software", on
//  Hexen source as published by "Raven" software and DelphiDoom
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
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit in_stuff;

// Intermission stuff

interface

//==============================================================================
//
// IN_Start
//
//==============================================================================
procedure IN_Start;

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
function IN_Drawer: boolean;

implementation

uses
  d_delphi,
{$IFDEF OPENGL}
  gl_main,
{$ELSE}
  i_video,
{$ENDIF}
  i_system,
  am_map,
  d_player,
  d_event,
  g_game,
  m_menu,
  m_fixed,
  p_mapinfo,
  r_defs,
  v_data,
  v_video,
  s_sound,
  sounddata,
  s_sndseq,
  doomdef,
  w_wad,
  z_zone;

type
  gametype_t = (
    gt_single,
    gt_cooperative,
    gt_deathmatch
  );

const
  TEXTSPEED = 3;
  TEXTWAIT = 140;

var
  intermission: boolean;

var
  skipintermission: boolean;
  interstate: integer = 0;
  intertime: integer = -1;
  gametype: gametype_t;
  cnt: integer;
  slaughterboy: integer; // in DM, the player with the most kills
  patchINTERPIC: Ppatch_t;
  FontBNumbers: array[0..9] of Ppatch_t;
  FontBNegative: Ppatch_t;
  FontBSlash: Ppatch_t;
  FontBPercent: Ppatch_t;
  FontABaseLump: integer;
  FontBLump: integer;
  FontBLumpBase: integer;

var
  totalFrags: array[0..MAXPLAYERS - 1] of integer;

var
  HubCount: integer;
  HubText: string;

//==============================================================================
//
// IN_InitStats
//
//   Initializes the stats for single player mode
//
//==============================================================================
procedure IN_InitStats;
var
  i: integer;
  j: integer;
  oldCluster: integer;
  slaughterfrags: integer;
  slaughtercount: integer;
  playercount: integer;
  msgLumpName: string;
  msgSize: integer;
begin
  if deathmatch = 0 then
  begin
    gametype := gt_single;
    HubCount := 0;
    oldCluster := P_GetMapCluster(gamemap);
    if oldCluster <> P_GetMapCluster(LeaveMap) then
    begin
      if (oldCluster >= 1) and (oldCluster <= 5) then
      begin
        sprintf(msgLumpName, 'clus%dmsg', [oldCluster]);
        HubText := W_TextLumpName(msgLumpName);
        msgSize := Length(HubText);
        if msgSize >= MAX_INTRMSN_MESSAGE_SIZE then
          I_Error('IN_InitStats(): Cluster message too long (%s)', [msgLumpName]);
        HubCount := msgSize * TEXTSPEED + TEXTWAIT;
        S_ChangeMusic(Ord(mus_hub), true);
      end;
    end;
  end
  else
  begin
    gametype := gt_deathmatch;
    slaughterboy := 0;
    slaughterfrags := -9999;
    playercount := 0;
    slaughtercount := 0;
    for i := 0 to MAXPLAYERS - 1 do
    begin
      totalFrags[i] := 0;
      if playeringame[i] then
      begin
        inc(playercount);
        for j := 0 to MAXPLAYERS - 1 do
        begin
          if playeringame[j] then
          begin
            totalFrags[i] := totalFrags[i] + players[i].frags[j];
          end;
        end;
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
    S_ChangeMusic(Ord(mus_hub), true);
  end;
end;

//==============================================================================
//
// IN_LoadPics
//
//==============================================================================
procedure IN_LoadPics;
var
  i: integer;
begin
  if (HubCount <> 0) or (gametype = gt_deathmatch) then
  begin
    patchINTERPIC := W_CacheLumpName('INTERPIC', PU_STATIC);
    FontBLumpBase := W_GetNumForName('FONTB16');
    for i := 0 to 9 do
      FontBNumbers[i] := W_CacheLumpNum(FontBLumpBase + i, PU_STATIC);
    FontBLump := W_GetNumForName('FONTB_S') + 1;
    FontBNegative := W_CacheLumpName('FONTB13', PU_STATIC);
    FontABaseLump := W_GetNumForName('FONTA_S') + 1;

    FontBSlash := W_CacheLumpName('FONTB15', PU_STATIC);
    FontBPercent := W_CacheLumpName('FONTB05', PU_STATIC);
  end;
end;

//==============================================================================
//
// IN_UnloadPics
//
//==============================================================================
procedure IN_UnloadPics;
var
  i: integer;
begin
  if (HubCount <> 0) or (gametype = gt_deathmatch) then
  begin
    Z_ChangeTag(patchINTERPIC, PU_CACHE);
    for i := 0 to 9 do
      Z_ChangeTag(FontBNumbers[i], PU_CACHE);
    Z_ChangeTag(FontBNegative, PU_CACHE);
    Z_ChangeTag(FontBSlash, PU_CACHE);
    Z_ChangeTag(FontBPercent, PU_CACHE);
  end;
end;

//==============================================================================
//
// IN_Start
//
//==============================================================================
procedure IN_Start;
var
  i: integer;
  palette: PByteArray;
begin
  palette := V_ReadPalette(PU_STATIC);
  I_SetPalette(palette);
  V_SetPalette(palette);
  Z_ChangeTag(palette, PU_CACHE);

  IN_InitStats;
  IN_LoadPics;
  intermission := true;
  interstate := 0;
  skipintermission := false;
  intertime := 0;
  AM_Stop;
  for i := 0 to MAXPLAYERS - 1 do
  begin
    players[i].messageTics := 0;
    players[i]._message := '';
  end;
  S_StopAllSequences;
end;

//
// WaitStop
//

//==============================================================================
//
// IN_Stop
//
//==============================================================================
procedure IN_Stop;
begin
  intermission := false;
  IN_UnloadPics;
end;

//==============================================================================
//
// IN_WaitStop
//
//==============================================================================
procedure IN_WaitStop;
begin
  dec(cnt);
  if cnt <= 0 then
  begin
    IN_Stop;
    gameaction := ga_leavemap;
  end;
end;

//
// IN_CheckForSkip
//
//   Check to see if any player hit a key

var
  triedToSkip: boolean = false;

//==============================================================================
//
// IN_CheckForSkip
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
        begin
          skipintermission := true;
        end;
        player.attackdown := true;
      end
      else
      begin
        player.attackdown := false;
      end;
      if player.cmd.buttons and BT_USE <> 0 then
      begin
        if not player.usedown then
        begin
          skipintermission := true;
        end;
        player.usedown := true;
      end
      else
      begin
        player.usedown := false;
      end;
    end;
  end;
  if (deathmatch <> 0) and (intertime < 140) then
  begin // wait for 4 seconds before allowing a skip
    if skipintermission then
    begin
      triedToSkip := true;
      skipintermission := false;
    end;
  end
  else
  begin
    if triedToSkip then
    begin
      skipintermission := true;
      triedToSkip := false;
    end;
  end;
end;

//==============================================================================
//
// IN_Ticker
//
//==============================================================================
procedure IN_Ticker;
begin
  if not intermission then
    exit;

  if interstate <> 0 then
  begin
    IN_WaitStop;
    exit;
  end;

  skipintermission := false;
  IN_CheckForSkip;
  inc(intertime);
  if skipintermission or ((gametype = gt_single) and (HubCount = 0)) then
  begin
    interstate := 1;
    cnt := 10;
    skipintermission := false;
  end;
end;

//==============================================================================
//
// IN_DrawHubText
//
//==============================================================================
procedure IN_DrawHubText;
var
  i, count: integer;
  cx, cy: integer;
  w: Ppatch_t;
  c: char;
begin
  cy := 5;
  cx := 10;
  count := (intertime - 10) div TEXTSPEED;
  if count < 0 then
    exit;

  if count > Length(HubText) then
    count := Length(HubText);

  for i := 1 to count do
  begin
    c := HubText[i];

    if c = #10 then
    begin
      cx := 10;
      cy := cy + 9;
      continue;
    end;

    if Ord(c) < 32 then
    begin
      continue;
    end;

    c := toupper(c);
    if Ord(c) = 32 then
    begin
      cx := cx + 5;
      continue;
    end;

    w := W_CacheLumpNum(FontABaseLump + Ord(c) - 33, PU_CACHE);
    if cx + w.width > SCREENWIDTH then
      break;
    V_DrawPatch(cx, cy, SCN_TMP, w, false);
    cx := cx + w.width;
  end;
end;

//==============================================================================
//
// IN_DrNumber
//
//==============================================================================
procedure IN_DrNumber(val: integer; x, y: integer; wrapThresh: integer);
var
  buff: string;
begin
  if not ((val < -9) and (wrapThresh < 1000)) then
  begin
    if val >= wrapThresh then
      sprintf(buff, '%d', [val mod wrapThresh])
    else
      sprintf(buff, '%d', [val]);
  end
  else
    buff := 'XX';
  M_WriteText(x - M_StringWidth(buff) div 2, y, buff);
end;

//==============================================================================
//
// IN_DrNumberBold
//
//==============================================================================
procedure IN_DrNumberBold(val: integer; x, y: integer; wrapThresh: integer);
var
  buff: string;
begin
  if not ((val < -9) and (wrapThresh < 1000)) then
  begin
    if val >= wrapThresh then
      sprintf(buff, '%d', [val mod wrapThresh])
    else
      sprintf(buff, '%d', [val]);
  end
  else
    buff := 'XX';
  M_WriteTextYellow(x - M_StringWidth(buff) div 2, y, buff);
end;

//
// IN_DrDeathTally
//

const
  TALLY_EFFECT_TICKS = 20;
  TALLY_FINAL_X_DELTA = (23 * FRACUNIT);
  TALLY_FINAL_Y_DELTA = (13 * FRACUNIT);
  TALLY_START_XPOS = (178 * FRACUNIT);
  TALLY_STOP_XPOS = (90 * FRACUNIT);
  TALLY_START_YPOS = (132 * FRACUNIT);
  TALLY_STOP_YPOS = (83 * FRACUNIT);
  TALLY_TOP_X = 85;
  TALLY_TOP_Y = 9;
  TALLY_LEFT_X = 7;
  TALLY_LEFT_Y = 71;
  TALLY_TOTALS_X = 291;

var
  showTotals: boolean;

//==============================================================================
//
// IN_DrDeathTally
//
//==============================================================================
procedure IN_DrDeathTally;
var
  i, j: integer;
  xPos, yPos: fixed_t;
  xDelta, yDelta: fixed_t;
  xStart, scale: fixed_t;
  x, y: integer;
  bold: boolean;
  temp: integer;
begin
  V_DrawPatch(TALLY_TOP_X, TALLY_TOP_Y, SCN_TMP, 'tallytop', false);
  V_DrawPatch(TALLY_LEFT_X, TALLY_LEFT_Y, SCN_TMP, 'tallylft', false);
  if intertime < TALLY_EFFECT_TICKS then
  begin
    showTotals := false;
    scale := (intertime * FRACUNIT) div TALLY_EFFECT_TICKS;
    xDelta := FixedMul(scale, TALLY_FINAL_X_DELTA);
    yDelta := FixedMul(scale, TALLY_FINAL_Y_DELTA);
    xStart := TALLY_START_XPOS - FixedMul(scale, TALLY_START_XPOS - TALLY_STOP_XPOS);
    yPos := TALLY_START_YPOS - FixedMul(scale, TALLY_START_YPOS - TALLY_STOP_YPOS);
  end
  else
  begin
    xDelta := TALLY_FINAL_X_DELTA;
    yDelta := TALLY_FINAL_Y_DELTA;
    xStart := TALLY_STOP_XPOS;
    yPos := TALLY_STOP_YPOS;
  end;
  if (intertime >= TALLY_EFFECT_TICKS) and not showTotals then
  begin
    showTotals := true;
    S_StartSound(nil, Ord(SFX_PLATFORM_STOP));
  end;
  y := FixedInt(yPos);
  for i := 0 to MAXPLAYERS - 1 do
  begin
    xPos := xStart;
    for j := 0 to MAXPLAYERS - 1 do
    begin
      x := FixedInt(xPos);
      bold := (i = consoleplayer) or (j = consoleplayer);
      if playeringame[i] and playeringame[j] then
      begin
        if bold then
        begin
          IN_DrNumberBold(players[i].frags[j], x, y, 100);
        end
        else
        begin
          IN_DrNumber(players[i].frags[j], x, y, 100);
        end;
      end
      else
      begin
        temp := M_StringWidth('--') div 2;
        if bold then
        begin
          M_WriteTextYellow(x - temp, y, '--');
        end
        else
        begin
          M_WriteText(x - temp, y, '--');
        end;
      end;
      xPos := xPos + xDelta
    end;
    if showTotals and
       playeringame[i] and
       not ( (slaughterboy and (1 shl i) <> 0) and (intertime and 16 <> 0)) then
    begin
      IN_DrNumber(totalFrags[i], TALLY_TOTALS_X, y, 1000);
    end;
    yPos := yPos + yDelta;
    y := FixedInt(yPos);
  end;
end;

var
  travelpic: integer = -2;
  travelpal: integer = -2;

//==============================================================================
//
// IN_DrawTravel
//
//==============================================================================
procedure IN_DrawTravel;
var
  r: integer;
  pal: PByteArray;
begin
  if travelpic = -2 then
  begin
    travelpic := W_CheckNumForName('TRAVLPIC');
    travelpal := W_CheckNumForName('TRAVLPAL');
    if (travelpic = -1) or (travelpal = -1) then
    begin
      travelpic := -1;
      travelpal := -1;
    end;
  end
  else if travelpic = -1 then
  begin
    r := V_FindAproxColorIndex(@curpal, $FF0000);
    memset(@screens[SCN_TMP][0], r, 320 * 200);
  end
  else
  begin
    memcpy(@screens[SCN_TMP][0], W_CacheLumpNum(travelpic, PU_CACHE), 320 * 200);
    pal := W_CacheLumpNum(travelpal, PU_STATIC);
    I_SetPalette(pal);
    V_SetPalette(pal);
    Z_ChangeTag(pal, PU_CACHE);
  end;
end;

//==============================================================================
//
// IN_Drawer
//
//==============================================================================
function IN_Drawer: boolean;
begin
  if not intermission then
    result := false
  else if interstate <> 0 then
    result := false
  else
    result := true;

  if result then
  begin
    memcpy(@screens[SCN_TMP][0], patchINTERPIC, 320 * 200);
    if gametype = gt_single then
    begin
      if HubCount <> 0 then
        IN_DrawHubText;
    end
    else
      IN_DrDeathTally;
  end
  else
    IN_DrawTravel;

  V_CopyRect(0, 0, SCN_TMP, 320, 200, 0, 0, SCN_FG, true);

  V_FullScreenStretch;
end;

end.
