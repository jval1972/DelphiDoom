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
//  Foundation, inc., 59 Temple Place - Suite 330, Boston, MA
//  02111-1307, USA.
//
// DESCRIPTION:
//  Game completion, final screen animation.
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit f_finale;

interface

uses
  d_event,
  info_h;

//==============================================================================
//
// F_Responder
//
//==============================================================================
function F_Responder(ev: Pevent_t): boolean;

{ Called by main loop. }

//==============================================================================
//
// F_Ticker
//
//==============================================================================
procedure F_Ticker;

{ Called by main loop. }

//==============================================================================
//
// F_Drawer
//
//==============================================================================
procedure F_Drawer;

//==============================================================================
//
// F_StartFinale
//
//==============================================================================
procedure F_StartFinale;

//
// Final DOOM 2 animation
// Casting by id Software.
//   in order of appearance
//
type
  castinfo_t = record
    name: string;
    _type: mobjtype_t;
  end;
  Pcastinfo_t = ^castinfo_t;

const
  NUM_CASTS = 18;

var
  castorder: array[0..NUM_CASTS - 1] of castinfo_t;

  bgflatE1: string = 'FLOOR4_8';  // end of DOOM Episode 1
  bgflatE2: string = 'SFLR6_1';   // end of DOOM Episode 2
  bgflatE3: string = 'MFLR8_4';   // end of DOOM Episode 3
  bgflatE4: string = 'MFLR8_3';   // end of DOOM Episode 4
  bgflat06: string = 'SLIME16';   // DOOM2 after MAP06
  bgflat11: string = 'RROCK14';   // DOOM2 after MAP11
  bgflat20: string = 'RROCK07';   // DOOM2 after MAP20
  bgflat30: string = 'RROCK17';   // DOOM2 after MAP30
  bgflat15: string = 'RROCK13';   // DOOM2 going MAP15 to MAP31
  bgflat31: string = 'RROCK19';   // DOOM2 going MAP31 to MAP32
  bgcastcall: string = 'BOSSBACK';// Panel behind cast call

implementation

uses
  d_delphi,
  am_map,
  d_player,
  d_main,
  g_game,
  info,
  p_pspr,
  p_umapinfo,
  r_data,
  r_defs,
  r_things,
// Functions.
  z_zone,
  v_data,
  v_video,
  w_wad,
  s_sound,
  d_englsh,
  sounddata,
  doomdef,
  doomstat,
  hu_stuff;

var
// Stage of animation:
//  0 = text, 1 = art screen, 2 = character cast
  finalestage: integer;

  finalecount: integer;

const
  TEXTSPEED = 3;
  TEXTWAIT = 250;

var
  finaletext: string;
  finaleflat: string;
  using_FMI: boolean;

//==============================================================================
//
// F_StartCast
//
//==============================================================================
procedure F_StartCast; forward;

//==============================================================================
//
// F_CastTicker
//
//==============================================================================
procedure F_CastTicker; forward;

//==============================================================================
//
// F_CastResponder
//
//==============================================================================
function F_CastResponder(ev: Pevent_t): boolean; forward;

//==============================================================================
//
// F_CastDrawer
//
//==============================================================================
procedure F_CastDrawer; forward;

//==============================================================================
//
// F_StartFinale
//
//==============================================================================
procedure F_StartFinale;
var
  mus_changed: boolean;
begin
  gameaction := ga_nothing;
  gamestate := GS_FINALE;
  viewactive := false;
  amstate := am_inactive;
  mus_changed := false;

  finaletext := '';
  finaleflat := '';

  if (gamemapinfo <> nil) and (gamemapinfo.intermusicnum > 0) then
  begin
    S_ChangeMusic(gamemapinfo.intermusicnum, true);
    mus_changed := true;
  end;

  // Okay - IWAD dependend stuff.
  // This has been changed severly, and
  //  some stuff might have changed in the process.
  case gamemode of
    // DOOM 1 - E1, E3 or E4, but each nine missions
    shareware,
    registered,
    retail:
      begin
        if not mus_changed then
          S_ChangeMusic(Ord(mus_victor), true);
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
        else
          // Ouch.
        end;
      end;
    // DOOM II and missions packs with E1, M34
    commercial:
      begin
        if not mus_changed then
          S_ChangeMusic(Ord(mus_read_m), true);
        case gamemap of
          6:
            begin
              finaleflat := bgflat06;
              case gamemission of
                pack_tnt: finaletext := T1TEXT;
                pack_plutonia: finaletext := P1TEXT;
              else
                finaletext := C1TEXT;
              end;
            end;
         11:
            begin
              finaleflat := bgflat11;
              case gamemission of
                pack_tnt: finaletext := T2TEXT;
                pack_plutonia: finaletext := P2TEXT;
              else
                finaletext := C2TEXT;
              end;
            end;
         20:
            begin
              finaleflat := bgflat20;
              case gamemission of
                pack_tnt: finaletext := T3TEXT;
                pack_plutonia: finaletext := P3TEXT;
              else
                finaletext := C3TEXT;
              end;
            end;
         30:
            begin
              finaleflat := bgflat30;
              case gamemission of
                pack_tnt: finaletext := T4TEXT;
                pack_plutonia: finaletext := P4TEXT;
              else
                finaletext := C4TEXT;
              end;
            end;
         15:
            begin
              finaleflat := bgflat15;
              case gamemission of
                pack_tnt: finaletext := T5TEXT;
                pack_plutonia: finaletext := P5TEXT;
              else
                finaletext := C5TEXT;
              end;
            end;
         31:
            begin
              finaleflat := bgflat31;
              case gamemission of
                pack_tnt: finaletext := T6TEXT;
                pack_plutonia: finaletext := P6TEXT;
              else
                finaletext := C6TEXT;
              end;
            end;
        else
        // Ouch.
        end;
      end;
  else
    begin
      if not mus_changed then
        S_ChangeMusic(Ord(mus_read_m), true);
      finaleflat := 'F_SKY1'; // Not used anywhere else.
      finaletext := C1TEXT;   // FIXME - other text, music?
    end;
  end;

  using_FMI := false;

  if gamemapinfo <> nil then
  begin
    if (gamemapinfo.intertextsecret[0] <> #0) and secretexit and (gamemapinfo.intertextsecret[0] <> '-') then // '-' means that any default intermission was cleared.
      finaletext := ubigstringtostring(gamemapinfo.intertextsecret)
    else if (gamemapinfo.intertext[0] <> #0) and  not secretexit and (gamemapinfo.intertext[0] <> '-') then // '-' means that any default intermission was cleared.
      finaletext := ubigstringtostring(gamemapinfo.intertext);

    if finaletext = '' then
      finaletext := 'The End';  // this is to avoid a crash on a missing text in the last map.

    if gamemapinfo.interbackdrop <> '' then
      finaleflat := gamemapinfo.interbackdrop;

    if finaleflat = '' then
      finaleflat := 'FLOOR4_8'; // use a single fallback for all maps.

    using_FMI := true;
  end;

  finalestage := 0;
  finalecount := 0;
end;

//==============================================================================
//
// F_Responder
//
//==============================================================================
function F_Responder(ev: Pevent_t): boolean;
begin
  if finalestage = 2 then
    result := F_CastResponder(ev)
  else
    result := false;
end;

//==============================================================================
//
// FMI_Ticker
//
//==============================================================================
procedure FMI_Ticker;
begin
  if (gamemapinfo.endpic <> '') and (gamemapinfo.endpic <> '-') then
  begin
    if gamemapinfo.endpic = '$CAST' then
    begin
      F_StartCast;
      using_FMI := false;
    end
    else
    begin
      finalecount := 0;
      finalestage := 1;
      wipegamestate := -1;  // force a wipe
      if gamemapinfo.endpic = '$BUNNY' then
        S_StartMusic(Ord(mus_bunny))
      else if gamemapinfo.endpic = '!' then
        using_FMI := false;
    end;
  end
  else
    gameaction := ga_worlddone; // next level, e.g. MAP07
end;

//==============================================================================
//
// F_Ticker
//
//==============================================================================
procedure F_Ticker;
var
  i: integer;
begin
  // check for skipping
  if (gamemode = commercial) and (finalecount > 50) then
  begin
    // go on to the next level
    i := 0;
    while i < MAXPLAYERS do
    begin
      if players[i].cmd.buttons <> 0 then
        break;
      inc(i);
    end;

    if i < MAXPLAYERS then
    begin
      if using_FMI then
        FMI_Ticker
      else if gamemap = 30 then
        F_StartCast
      else
        gameaction := ga_worlddone;
    end;
  end;

  // advance animation
  inc(finalecount);

  if finalestage = 2 then
  begin
    F_CastTicker;
    exit;
  end;

  if (finalestage = 0) and (finalecount > Length(finaletext) * TEXTSPEED + TEXTWAIT) then
  begin
    if using_FMI then
      FMI_Ticker
    else if gamemode <> commercial then
    begin
      finalecount := 0;
      finalestage := 1;
      wipegamestate := -1;    // force a wipe
      if gameepisode = 3 then
        S_StartMusic(Ord(mus_bunny));
    end;
  end;
end;

//==============================================================================
//
// F_TextWrite
//
//==============================================================================
procedure F_TextWrite;
var
  bkok: boolean;
  w: integer;
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
  bkok := V_TileScreen8(finaleflat, SCN_TMP);

  if not bkok then
    ZeroMemory(@screens[SCN_TMP, 0], V_ScreensSize(SCN_TMP));

  // draw some of the text onto the screen
  cx := 10;
  cy := 10;
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
      cy := cy + 11;
      continue;
    end;
    if c = #10 then
    begin
      cx := 10;
      continue;
    end;

    c1 := Ord(toupper(c)) - Ord(HU_FONTSTART);
    if (c1 < 0) or (c1 > HU_FONTSIZE) then
    begin
      cx := cx + 4;
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

var
  castnum: integer;
  casttics: integer;
  caststate: Pstate_t;
  castdeath: boolean;
  castframes: integer;
  castonmelee: integer;
  castattacking: boolean;

//==============================================================================
//
// F_StartCast
//
//==============================================================================
procedure F_StartCast;
begin
  if finalestage = 2 then
    exit;
  wipegamestate := -1;    // force a screen wipe
  castnum := 0;
  caststate := @states[mobjinfo[Ord(castorder[castnum]._type)].seestate];
  casttics := caststate.tics;
  castdeath := false;
  finalestage := 2;
  castframes := 0;
  castonmelee := 0;
  castattacking := false;
  S_ChangeMusic(Ord(mus_evil), true);
end;

//==============================================================================
//
// F_CastTicker
//
//==============================================================================
procedure F_CastTicker;
var
  st: integer;
  sfx: integer;
begin
  dec(casttics);
  if casttics > 0 then
    exit; // not time to change state yet

  if (caststate.tics = -1) or (caststate.nextstate = S_NULL) then
  begin
    // switch from deathstate to next monster
    inc(castnum);
    castdeath := false;
    if castorder[castnum].name = '' then
      castnum := 0;
    if mobjinfo[Ord(castorder[castnum]._type)].seesound <> 0 then
      S_StartSound(nil, mobjinfo[Ord(castorder[castnum]._type)].seesound);
    caststate := @states[mobjinfo[Ord(castorder[castnum]._type)].seestate];
    castframes := 0;
  end
  else
  begin
  // just advance to next state in animation
    if caststate = @states[Ord(S_PLAY_ATK1)] then
    begin
      castattacking := false;
      castframes := 0;
      caststate := @states[mobjinfo[Ord(castorder[castnum]._type)].seestate];
      casttics := caststate.tics;
      if casttics = -1 then
        casttics := 15;
      exit;
    end;
    st := Ord(caststate.nextstate);
    caststate := @states[st];
    inc(castframes);

    // sound hacks....
    case statenum_t(st) of
      S_PLAY_ATK1:  sfx := Ord(sfx_dshtgn);
      S_POSS_ATK2:  sfx := Ord(sfx_pistol);
      S_SPOS_ATK2:  sfx := Ord(sfx_shotgn);
      S_VILE_ATK2:  sfx := Ord(sfx_vilatk);
      S_SKEL_FIST2: sfx := Ord(sfx_skeswg);
      S_SKEL_FIST4: sfx := Ord(sfx_skepch);
      S_SKEL_MISS2: sfx := Ord(sfx_skeatk);
      S_FATT_ATK8,
      S_FATT_ATK5,
      S_FATT_ATK2:  sfx := Ord(sfx_firsht);
      S_CPOS_ATK2,
      S_CPOS_ATK3,
      S_CPOS_ATK4:  sfx := Ord(sfx_shotgn);
      S_TROO_ATK3:  sfx := Ord(sfx_claw);
      S_SARG_ATK2:  sfx := Ord(sfx_sgtatk);
      S_BOSS_ATK2,
      S_BOS2_ATK2,
      S_HEAD_ATK2:  sfx := Ord(sfx_firsht);
      S_SKULL_ATK2: sfx := Ord(sfx_sklatk);
      S_SPID_ATK2,
      S_SPID_ATK3:  sfx := Ord(sfx_shotgn);
      S_BSPI_ATK2:  sfx := Ord(sfx_plasma);
      S_CYBER_ATK2,
      S_CYBER_ATK4,
      S_CYBER_ATK6: sfx := Ord(sfx_rlaunc);
      S_PAIN_ATK3:  sfx := Ord(sfx_sklatk);
    else
      sfx := 0;
    end;
    if sfx <> 0 then
      S_StartSound(nil, sfx);
  end;

  if castframes = 12 then
  begin
    // go into attack frame
    castattacking := true;
    if castonmelee <> 0 then
      caststate := @states[mobjinfo[Ord(castorder[castnum]._type)].meleestate]
    else
      caststate := @states[mobjinfo[Ord(castorder[castnum]._type)].missilestate];
    castonmelee := castonmelee xor 1;
    if caststate = @states[Ord(S_NULL)] then
    begin
      if castonmelee <> 0 then
        caststate := @states[mobjinfo[Ord(castorder[castnum]._type)].meleestate]
      else
        caststate := @states[mobjinfo[Ord(castorder[castnum]._type)].missilestate];
    end;
  end;

  if castattacking then
  begin
    if (castframes = 24) or
       (caststate = @states[mobjinfo[Ord(castorder[castnum]._type)].seestate]) then
    begin
      castattacking := false;
      castframes := 0;
      caststate := @states[mobjinfo[Ord(castorder[castnum]._type)].seestate];
    end;
  end;

  casttics := caststate.tics;
  if casttics = -1 then
    casttics := 15;
end;

//==============================================================================
//
// F_CastResponder
//
//==============================================================================
function F_CastResponder(ev: Pevent_t): boolean;
begin
  if ev._type <> ev_keydown then
  begin
    result := false;
    exit;
  end;

  if castdeath then
  begin
    result := true; // already in dying frames
    exit;
  end;

  // go into death frame
  castdeath := true;
  caststate := @states[mobjinfo[Ord(castorder[castnum]._type)].deathstate];
  casttics := caststate.tics;
  castframes := 0;
  castattacking := false;
  if mobjinfo[Ord(castorder[castnum]._type)].deathsound <> 0 then
    S_StartSound(nil, mobjinfo[Ord(castorder[castnum]._type)].deathsound);

  result := true;
end;

//==============================================================================
//
// F_CastPrint
//
//==============================================================================
procedure F_CastPrint(const text: string);
var
  ch: string;
  i: integer;
  c: char;
  c1: integer;
  len: integer;
  cx: integer;
  w: integer;
  width: integer;
begin
  // find width
  ch := text;
  width := 0;

  len := Length(ch);
  for i := 1 to len do
  begin
    c := ch[i];
    if c = #0 then
      break;
    c1 := Ord(toupper(c)) - Ord(HU_FONTSTART);
    if (c1 < 0) or (c1 > HU_FONTSIZE) then
      width := width + 4
    else
    begin
      w := hu_font[c1].width;
      width := width + w;
    end;
  end;

  // draw it
  cx := (320 - width) div 2;
  for i := 1 to len do
  begin
    c := ch[i];
    if c = #0 then
      break;
    c1 := Ord(toupper(c)) - Ord(HU_FONTSTART);
    if (c1 < 0) or (c1 > HU_FONTSIZE) then
      cx := cx + 4
    else
    begin
      w := hu_font[c1].width;
      V_DrawPatch(cx, 180, SCN_TMP, hu_font[c1], false);
      cx := cx + w;
    end;
  end;
end;

//==============================================================================
//
// F_CastDrawer
//
//==============================================================================
procedure F_CastDrawer;
var
  sprdef: Pspritedef_t;
  sprframe: Pspriteframe_t;
  lump: integer;
  flip: boolean;
  patch: Ppatch_t;
begin
  // erase the entire screen to a background
  V_DrawPatchFullScreenTMP320x200(bgcastcall);

  F_CastPrint(castorder[castnum].name);

  // draw the current frame in the middle of the screen
  sprdef := @sprites[caststate.sprite];
  sprframe := @sprdef.spriteframes[caststate.frame and FF_FRAMEMASK];
  lump := sprframe.lump[0];
  flip := sprframe.flip[0];

  patch := W_CacheLumpNum(lump + firstspritelump, PU_STATIC);
  if flip then
    V_DrawPatchFlipped(160, 170, SCN_TMP, patch)
  else
    V_DrawPatch(160, 170, SCN_TMP, patch, false);
  Z_ChangeTag(patch, PU_CACHE);

  V_CopyRect(0, 0, SCN_TMP, 320, 200, 0, 0, SCN_FG, true);

  V_FullScreenStretch;
end;

//==============================================================================
//
// F_DrawPatchCol
//
//==============================================================================
procedure F_DrawPatchCol(x: integer; patch: Ppatch_t; col: integer);
var
  column: Pcolumn_t;
  source: PByte;
  dest: PByte;
  desttop: PByte;
  count: integer;
  delta, prevdelta: integer;
  tallpatch: boolean;
begin
  column := Pcolumn_t(integer(patch) + patch.columnofs[col]);
  delta := 0;
  tallpatch := false;
  desttop := PByte(integer(screens[SCN_TMP]) + x);
  // step through the posts in a column
  while column.topdelta <> $ff do
  begin
    source := PByte(integer(column) + 3);
    delta := delta + column.topdelta;
    dest := PByte(integer(desttop) + delta * 320);
    count := column.length;

    while count > 0 do
    begin
      dest^ := source^;
      inc(source);
      inc(dest, 320);
      dec(count);
    end;
    if not tallpatch then
    begin
      prevdelta := column.topdelta;
      column := Pcolumn_t(integer(column) + column.length + 4);
      if column.topdelta > prevdelta then
        delta := 0
      else
        tallpatch := true;
    end
    else
      column := Pcolumn_t(integer(column) + column.length + 4);
  end;
end;

//
// F_BunnyScroll
//
var
  laststage: integer;

//==============================================================================
//
// F_BunnyScroll
//
//==============================================================================
procedure F_BunnyScroll;
var
  scrolled: integer;
  x: integer;
  p1: Ppatch_t;
  p2: Ppatch_t;
  name: string;
  stage: integer;
begin
  p1 := W_CacheLumpName('PFUB2', PU_LEVEL);
  p2 := W_CacheLumpName('PFUB1', PU_LEVEL);

  scrolled := 320 - (finalecount - 230) div 2;
  if scrolled > p1.width then
    scrolled := p1.width
  else if scrolled < 0 then
    scrolled := 0;

  for x := 0 to 320 - 1 do
  begin
    if x + scrolled < p1.width then
      F_DrawPatchCol(x, p1, x + scrolled)
    else
      F_DrawPatchCol(x, p2, x + scrolled - p1.width);
  end;

  if finalecount >= 1130 + p1.width - 320 then
  begin
    if finalecount < 1180 + p1.width - 320 then
    begin
      V_DrawPatch((320 - 13 * 8) div 2,
                  (200 - 8 * 8) div 2,
                   SCN_TMP, 'END0', false);
      laststage := 0;
    end
    else
    begin
      stage := (finalecount - (1180 + p1.width - 320)) div 5;
      if stage > 6 then
        stage := 6;
      if stage > laststage then
      begin
        S_StartSound(nil, Ord(sfx_pistol));
        laststage := stage;
      end;

      sprintf(name,'END%d', [stage]);
      V_DrawPatch((320 - 13 * 8) div 2,
                  (200 - 8 * 8) div 2,
                   SCN_TMP, name, false);
    end;
  end;

  V_CopyRect(0, 0, SCN_TMP, 320, 200, 0, 0, SCN_FG, true);

  V_FullScreenStretch;
end;

//==============================================================================
//
// F_Drawer
//
//==============================================================================
procedure F_Drawer;
begin
  if using_FMI then
  begin
    if finalestage = 0 then
    begin
      F_TextWrite;
      if gamemapinfo.endpic = '' then
        using_FMI := false;
    end
    else if gamemapinfo.endpic = '$BUNNY' then
      F_BunnyScroll
    else
      V_PageDrawer(gamemapinfo.endpic);
    exit;
  end;

  if finalestage = 2 then
  begin
    F_CastDrawer;
    exit;
  end;

  if finalestage = 0 then
  begin
    F_TextWrite;
    exit;
  end;

  case gameepisode of
    1:
      begin
        if gamemode = retail then
          V_PageDrawer(pg_CREDIT)
        else
          V_PageDrawer(pg_HELP2);
      end;
    2:
      begin
        V_PageDrawer(pg_VICTORY2);
      end;
    3:
      begin
        F_BunnyScroll;
      end;
    4:
      begin
        V_PageDrawer(pg_ENDPIC);
      end;
  end;

end;

initialization
  castorder[0].name := CC_ZOMBIE;
  castorder[0]._type := MT_POSSESSED;

  castorder[1].name := CC_SHOTGUN;
  castorder[1]._type := MT_SHOTGUY;

  castorder[2].name := CC_HEAVY;
  castorder[2]._type := MT_CHAINGUY;

  castorder[3].name := CC_IMP;
  castorder[3]._type := MT_TROOP;

  castorder[4].name := CC_DEMON;
  castorder[4]._type := MT_SERGEANT;

  castorder[5].name := CC_LOST;
  castorder[5]._type := MT_SKULL;

  castorder[6].name := CC_CACO;
  castorder[6]._type := MT_HEAD;

  castorder[7].name := CC_HELL;
  castorder[7]._type := MT_KNIGHT;

  castorder[8].name := CC_BARON;
  castorder[8]._type := MT_BRUISER;

  castorder[9].name := CC_ARACH;
  castorder[9]._type := MT_BABY;

  castorder[10].name := CC_PAIN;
  castorder[10]._type := MT_PAIN;

  castorder[11].name := CC_REVEN;
  castorder[11]._type := MT_UNDEAD;

  castorder[12].name := CC_MANCU;
  castorder[12]._type := MT_FATSO;

  castorder[13].name := CC_ARCH;
  castorder[13]._type := MT_VILE;

  castorder[14].name := CC_SPIDER;
  castorder[14]._type := MT_SPIDER;

  castorder[15].name := CC_CYBER;
  castorder[15]._type := MT_CYBORG;

  castorder[16].name := CC_HERO;
  castorder[16]._type := MT_PLAYER;

  castorder[17].name := '';
  castorder[17]._type := mobjtype_t(0);

  laststage := 0;

end.

