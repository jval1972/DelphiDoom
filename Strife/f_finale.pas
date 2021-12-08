//------------------------------------------------------------------------------
//
//  DelphiStrife: A modified and improved Strife source port for Windows.
//
//  Based on:
//    - Linux Doom by "id Software"
//    - Chocolate Strife by "Simon Howard"
//    - DelphiDoom by "Jim Valavanis"
//
//  Copyright (C) 1993-1996 by id Software, Inc.
//  Copyright (C) 2005 Simon Howard
//  Copyright (C) 2010 James Haley, Samuel Villarreal
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
//  Foundation, inc., 59 Temple Place - Suite 330, Boston, MA
//  02111-1307, USA.
//
// DESCRIPTION:
//  Game completion, final screen animation.
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit f_finale;

interface

uses
  d_event,
  info_h;

function F_Responder(ev: Pevent_t): boolean;

{ Called by main loop. }
procedure F_Ticker;

{ Called by main loop. }
procedure F_Drawer;

procedure F_StartFinale;


//
// Final DOOM 2 animation
// Casting by id Software.
//   in order of appearance
//
type
  castinfo_t = record
    isindemo: boolean;
    _type: mobjtype_t;
  end;
  Pcastinfo_t = ^castinfo_t;

const
  NUM_CASTS = 18;

var
  castorder: array[0..NUM_CASTS - 1] of castinfo_t = (
    ( isindemo: true;  _type: MT_PLAYER),
    ( isindemo: true;  _type: MT_BEGGAR1),
    ( isindemo: true;  _type: MT_PEASANT2_A),
    ( isindemo: true;  _type: MT_REBEL1),
    ( isindemo: true;  _type: MT_GUARD1),
    ( isindemo: true;  _type: MT_CRUSADER),
    ( isindemo: true;  _type: MT_RLEADER2),
    ( isindemo: false; _type: MT_SENTINEL),
    ( isindemo: false; _type: MT_STALKER),
    ( isindemo: false; _type: MT_PROGRAMMER),
    ( isindemo: false; _type: MT_REAVER),
    ( isindemo: false; _type: MT_PGUARD),
    ( isindemo: false; _type: MT_INQUISITOR),
    ( isindemo: false; _type: MT_PRIEST),
    ( isindemo: false; _type: MT_SPECTRE_A),
    ( isindemo: false; _type: MT_BISHOP),
    ( isindemo: false; _type: MT_ENTITY),
    ( isindemo: true;  _type: DO_NUMMOBJTYPES)
  );

  bgcastcall: string = 'BOSSBACK';// Panel behind cast call

const
  F_STAGE_TEXT = 0;
  F_STAGE_ARTSCREEN = 1;
  F_STAGE_CAST = 2;

procedure F_Init;

implementation

uses
  d_delphi,
  c_cmds,
  d_check,
  deh_main,
  am_map,
  d_player,
  d_main,
  g_game,
  info,
  p_pspr,
  p_dialog,
  m_menu,
  r_data,
  r_defs,
  r_things,
// Functions.
  z_zone,
  v_data,
  v_video,
  w_wad,
  s_sound,
// Data.
  sounds,
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

const
// [STRIFE] - Slideshow states enumeration
    // Exit states
    SLIDE_EXITHACK    = -99; // Hacky exit - start a new dialog
    SLIDE_HACKHACK    =  -9; // Bizarre unused state
    SLIDE_EXIT        =  -1; // Exit to next finale state
    SLIDE_CHOCO       =  -2; // haleyjd: This state is Choco-specific... see below.

    // Unknown
    SLIDE_UNKNOWN     =   0; // Dunno what it's for, possibly unused

    // MAP03 - Macil's Programmer exposition
    SLIDE_PROGRAMMER1 =   1;
    SLIDE_PROGRAMMER2 =   2;
    SLIDE_PROGRAMMER3 =   3;
    SLIDE_PROGRAMMER4 =   4; // Next state = -99

    // MAP10 - Macil's Sigil exposition
    SLIDE_SIGIL1      =   5;
    SLIDE_SIGIL2      =   6;
    SLIDE_SIGIL3      =   7;
    SLIDE_SIGIL4      =   8; // Next state = -99

    // MAP29 - Endings
    // Good Ending
    SLIDE_GOODEND1    =  10;
    SLIDE_GOODEND2    =  11;
    SLIDE_GOODEND3    =  12;
    SLIDE_GOODEND4    =  13; // Next state = -1

    // Bad Ending
    SLIDE_BADEND1     =  14;
    SLIDE_BADEND2     =  15;
    SLIDE_BADEND3     =  16; // Next state = -1

    // Blah Ending
    SLIDE_BLAHEND1    =  17;
    SLIDE_BLAHEND2    =  18;
    SLIDE_BLAHEND3    =  19; // Next state = -1

    // Demo Ending - haleyjd 20130301: v1.31 only
    SLIDE_DEMOEND1    =  25;
    SLIDE_DEMOEND2    =  26; // Next state = -1

    // JVAL: Teaser demo textdrawer
    SLIDE_TEXT        =  50;

procedure F_StartCast; forward;

procedure F_CastTicker; forward;

function F_CastResponder(ev: Pevent_t): boolean; forward;

procedure F_TextWrite;
var
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

  V_DrawPatchFullScreenTMP320x200(D_Help0Lump);

  // draw some of the text onto the screen
  cx := 20;
  cy := 20;

  ch := M_DialogDimMsg(cx, cy, W_TextLumpName('T1TEXT'), false);
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

  if finalecount > 700 then
    D_StartTitle;
end;


var
  slideshow_panel: string;
  slideshow_state: integer;
  slideshow_tics: integer;

procedure F_StartFinale;
begin
  gameaction := ga_nothing;
  gamestate := GS_FINALE;
  viewactive := false;
  amstate := am_inactive;
  wipegamestate := -1;

  slideshow_panel := char8tostring(W_GetNameForNum(D_Panel0Lump));

  case gamemap of
    3:  // Macil's exposition on the Programmer
      begin
        slideshow_state := SLIDE_PROGRAMMER1;
      end;
    9:  // Super hack for death of Programmer
      begin
        slideshow_state := SLIDE_EXITHACK;
      end;
    10: // Macil's exposition on the Sigil
      begin
        slideshow_state := SLIDE_SIGIL1;
      end;
    29: // Endings
      begin
        if not netgame then
        begin
          if players[0].health <= 0 then      // Bad ending
            slideshow_state := SLIDE_BADEND1  // - Humanity goes extinct
          else
          begin
            if (players[0].questflags and QF_QUEST25 <> 0) and   // Converter destroyed
               (players[0].questflags and QF_QUEST27 <> 0) then  // Computer destroyed (wtf?!)
            // Good ending - You get the hot babe.
              slideshow_state := SLIDE_GOODEND1
            else
              // Blah ending - You win the battle, but fail at life.
              slideshow_state := SLIDE_BLAHEND1;
           end;
         end;
      end;
    34: // For the demo version ending
      begin
        if teaser > 0 then
        begin
          slideshow_state := SLIDE_TEXT
        end
        else
        begin
          slideshow_state := SLIDE_EXIT;

          // haleyjd 20130301: Somebody noticed the demo levels were missing the
          // ending they used to have in the demo version EXE, I guess. But the
          // weird thing is, this will only trigger if you run with strife0.wad,
          // and no released version thereof actually works with the 1.31 EXE
          // due to differing dialog formats... was there to be an updated demo
          // that never got released?!
          if (gameversion = exe_strife_1_31) and isdemoversion then
            slideshow_state := SLIDE_DEMOEND1;
        end;
      end;
  end;

  if teaser > 0 then
    S_ChangeMusic(Ord(mus_drone), true)
  else
    S_ChangeMusic(Ord(mus_dark), true);
  slideshow_tics := 7;
  finalestage := F_STAGE_TEXT;
  finalecount := 0;

end;

function F_Responder(ev: Pevent_t): boolean;
begin
  if finalestage = F_STAGE_CAST then
    result := F_CastResponder(ev)
  else
    result := false;
end;


//
// F_WaitTicker
//
// [STRIFE] New function
// haleyjd 09/13/10: This is called from G_Ticker if gamestate is 1, but we
// have no idea for what it's supposed to be. It is unused.
//
procedure F_WaitTicker;
begin
  inc(finalecount);
  if finalecount >= 250 then
  begin
    gamestate   := GS_FINALE;
    finalestage := 0;
    finalecount := 0;
  end;
end;

//
// F_DoSlideShow
//
// [STRIFE] New function
// haleyjd 09/13/10: Handles slideshow states. Begging to be tabulated!
//
procedure F_DoSlideShow;
var
  patch: Ppatch_t;
begin
  case slideshow_state of
    SLIDE_UNKNOWN: // state #0, seems to be unused
      begin
        slideshow_tics := 700;
        slideshow_state := SLIDE_EXIT;
        slideshow_panel := DEH_GetString('SS2F1');
        S_StartVoice(DEH_GetString('MAC10'));
        slideshow_state := SLIDE_PROGRAMMER2;
        slideshow_tics := 315;
      end;
    SLIDE_PROGRAMMER1: // state #1
      begin
        slideshow_panel := DEH_GetString('SS2F1');
        S_StartVoice(DEH_GetString('MAC10'));
        slideshow_state := SLIDE_PROGRAMMER2;
        slideshow_tics := 315;
      end;
    SLIDE_PROGRAMMER2: // state #2
      begin
        slideshow_panel := DEH_GetString('SS2F2');
        S_StartVoice(DEH_GetString('MAC11'));
        slideshow_state := SLIDE_PROGRAMMER3;
        slideshow_tics := 350;
      end;
    SLIDE_PROGRAMMER3: // state #3
      begin
        slideshow_panel := DEH_GetString('SS2F3');
        S_StartVoice(DEH_GetString('MAC12'));
        slideshow_state := SLIDE_PROGRAMMER4;
        slideshow_tics := 420;
      end;
    SLIDE_PROGRAMMER4: // state #4
      begin
        slideshow_panel := DEH_GetString('SS2F4');
        S_StartVoice(DEH_GetString('MAC13'));
        slideshow_state := SLIDE_EXITHACK; // End of slides
        slideshow_tics := 595;
      end;

    SLIDE_SIGIL1: // state #5
      begin
        slideshow_panel := DEH_GetString('SS3F1');
        S_StartVoice(DEH_GetString('MAC16'));
        slideshow_state := SLIDE_SIGIL2;
        slideshow_tics := 350;
      end;
    SLIDE_SIGIL2: // state #6
      begin
        slideshow_panel := DEH_GetString('SS3F2');
        S_StartVoice(DEH_GetString('MAC17'));
        slideshow_state := SLIDE_SIGIL3;
        slideshow_tics := 420;
      end;
    SLIDE_SIGIL3: // state #7
      begin
        slideshow_panel := DEH_GetString('SS3F3');
        S_StartVoice(DEH_GetString('MAC18'));
        slideshow_tics := 420;
        slideshow_state := SLIDE_SIGIL4;
      end;
    SLIDE_SIGIL4: // state #8
      begin
        slideshow_panel := DEH_GetString('SS3F4');
        S_StartVoice(DEH_GetString('MAC19'));
        slideshow_tics := 385;
        slideshow_state := SLIDE_EXITHACK; // End of slides
      end;

    SLIDE_GOODEND1: // state #10
      begin
        slideshow_panel := DEH_GetString('SS4F1');
        S_StartMusic(Ord(mus_happy));
        S_StartVoice(DEH_GetString('RIE01'));
        slideshow_state := SLIDE_GOODEND2;
        slideshow_tics := 455;
      end;
    SLIDE_GOODEND2: // state #11
      begin
        slideshow_panel := DEH_GetString('SS4F2');
        S_StartVoice(DEH_GetString('BBX01'));
        slideshow_state := SLIDE_GOODEND3;
        slideshow_tics := 385;
      end;
    SLIDE_GOODEND3: // state #12
      begin
        slideshow_panel := DEH_GetString('SS4F3');
        S_StartVoice(DEH_GetString('BBX02'));
        slideshow_state := SLIDE_GOODEND4;
        slideshow_tics := 490;
      end;
    SLIDE_GOODEND4: // state #13
      begin
        slideshow_panel := DEH_GetString('SS4F4');
        slideshow_state := SLIDE_EXIT; // Go to end credits
        slideshow_tics := 980;
      end;

    SLIDE_BADEND1: // state #14
      begin
        S_StartMusic(Ord(mus_sad));
        slideshow_panel := DEH_GetString('SS5F1');
        S_StartVoice(DEH_GetString('SS501b'));
        slideshow_state := SLIDE_BADEND2;
        slideshow_tics := 385;
      end;
    SLIDE_BADEND2: // state #15
      begin
        slideshow_panel := DEH_GetString('SS5F2');
        S_StartVoice(DEH_GetString('SS502b'));
        slideshow_state := SLIDE_BADEND3;
        slideshow_tics := 350;
      end;
    SLIDE_BADEND3: // state #16
      begin
        slideshow_panel := DEH_GetString('SS5F3');
        S_StartVoice(DEH_GetString('SS503b'));
        slideshow_state := SLIDE_EXIT; // Go to end credits
        slideshow_tics := 385;
      end;
    SLIDE_BLAHEND1: // state #17
      begin
        S_StartMusic(Ord(mus_end));
        slideshow_panel := DEH_GetString('SS6F1');
        S_StartVoice(DEH_GetString('SS601A'));
        slideshow_state := SLIDE_BLAHEND2;
        slideshow_tics := 280;
      end;
    SLIDE_BLAHEND2: // state #18
      begin
        S_StartMusic(Ord(mus_end));
        slideshow_panel := DEH_GetString('SS6F2');
        S_StartVoice(DEH_GetString('SS602A'));
        slideshow_state := SLIDE_BLAHEND3;
        slideshow_tics := 280;
      end;
    SLIDE_BLAHEND3: // state #19
      begin
        S_StartMusic(Ord(mus_end));
        slideshow_panel := DEH_GetString('SS6F3');
        S_StartVoice(DEH_GetString('SS603A'));
        slideshow_state := SLIDE_EXIT; // Go to credits
        slideshow_tics := 315;
      end;
    SLIDE_DEMOEND1: // state #25 - only exists in 1.31
      begin
        slideshow_panel := DEH_GetString('PANEL7');
        slideshow_tics := 175;
        slideshow_state := SLIDE_DEMOEND2;
      end;
    SLIDE_DEMOEND2: // state #26 - ditto
      begin
        slideshow_panel := DEH_GetString('VELLOGO');
        slideshow_tics := 175;
        slideshow_state := SLIDE_EXIT; // Go to end credits
      end;
    SLIDE_EXITHACK: // state -99: super hack state
      begin
        gamestate := GS_LEVEL;
        P_DialogStartP1();
      end;
    SLIDE_HACKHACK: // state -9: unknown bizarre unused state
      begin
        S_StartSound(nil, Ord(sfx_rifle));
        slideshow_tics := 3150;
      end;
    SLIDE_EXIT: // state -1: proceed to next finale stage
      begin
        finalecount := 0;
        finalestage := F_STAGE_ARTSCREEN;
        wipegamestate := -1;
        S_StartMusic(Ord(mus_fast));
        // haleyjd 20130301: The ONLY glitch fixed in 1.31 of Strife
        // *would* be something this insignificant, of course!
        if (gameversion <> exe_strife_1_31) then
            slideshow_state := SLIDE_CHOCO; // haleyjd: see below...
      end;
    SLIDE_CHOCO:
      begin
        // haleyjd 09/14/10: This wouldn't be necessary except that Choco
        // doesn't support the V_MarkRect dirty rectangles system. This
        // just so happens to have hidden the fact that the ending
        // does a screenfade every ~19 seconds due to remaining stuck in
        // SLIDE_EXIT state above, UNLESS the menus were active - the
        // V_MarkRect calls in the menu system cause it to be visible.
        // This means that in order to get the same behavior as the vanilla
        // EXE, I need different code. So, come to this state and only set
        // wipegamestate if menuactive is true.
        finalecount := 0;
        finalestage := F_STAGE_ARTSCREEN;
        if menuactive then
          wipegamestate := -1;
        S_StartMusic(Ord(mus_fast));
        slideshow_state := SLIDE_CHOCO; // remain here.
      end;
  end;

  finalecount := 0;
  if gameversion <> exe_strife_1_31 then // See above. This was removed in 1.31.
  begin
    patch := W_CacheLumpNum(D_Panel0Lump, PU_STATIC);
    V_DrawPatchFullScreenTMP320x200(patch);
    Z_ChangeTag(patch, PU_CACHE);
  end;
end;

//
// F_Ticker
//
procedure F_Ticker;
var
  i: integer;
begin
  // check for skipping
  if finalecount > 50 then
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
      finalecount := slideshow_tics;
  end;

  // advance animation
  inc(finalecount);

  if slideshow_state = SLIDE_TEXT then
    F_TextWrite
  else if finalestage = F_STAGE_CAST then
    F_CastTicker
  else if finalecount > slideshow_tics then // [STRIFE] Advance slideshow
    F_DoSlideShow;
end;


var
  castnum: integer;
  casttics: integer;
  caststate: Pstate_t;
  castdeath: boolean;
  castframes: integer;
  castonmelee: integer;
  castattacking: boolean;

//
// F_StartCast
//
// haleyjd 09/13/10: [STRIFE] Heavily modified, yet unused.
// Evidence suggests this was meant to be started from a menu item.
// See m_menu.c for more info.
//
procedure F_StartCast;
begin
  if finalestage = F_STAGE_CAST then
    exit;
  wipegamestate := -1;    // force a screen wipe
  gameaction := ga_nothing;
  viewactive := false;
  amstate := am_inactive;
  castnum := 0;
  gamestate := GS_FINALE;
  caststate := @states[mobjinfo[Ord(castorder[castnum]._type)].seestate];
  casttics := caststate.tics;
  if casttics > 50 then
    casttics := 50;
  castdeath := false;
  finalestage := F_STAGE_CAST;
  castframes := 0;
  castonmelee := 0;
  castattacking := false;
end;

//
// F_CastTicker
//
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
    if isdemoversion then
    // [STRIFE] Demo version had a shorter cast
      if not castorder[castnum].isindemo then
        castnum := 0;
    if castorder[castnum]._type = DO_NUMMOBJTYPES then
      castnum := 0;
    if mobjinfo[Ord(castorder[castnum]._type)].seesound <> 0 then
      S_StartSound(nil, mobjinfo[Ord(castorder[castnum]._type)].seesound);
    caststate := @states[mobjinfo[Ord(castorder[castnum]._type)].seestate];
    castframes := 0;
  end
  else
  begin
  // just advance to next state in animation
    if caststate = @states[Ord(S_PLAY_05)] then   // villsa [STRIFE] - updated
    begin
      castattacking := false;
      castframes := 0;
      caststate := @states[mobjinfo[Ord(castorder[castnum]._type)].seestate];
      casttics := caststate.tics;
      if casttics = -1 then
        casttics := 15
      else if casttics > 50 then
        casttics := 50;
      exit;
    end;
    st := Ord(caststate.nextstate);
    caststate := @states[st];
    inc(castframes);

    if (st <> mobjinfo[Ord(castorder[castnum]._type)].meleestate) and
       (st <> mobjinfo[Ord(castorder[castnum]._type)].missilestate) then
    begin
      if st = Ord(S_PLAY_05) then
        sfx := Ord(sfx_rifle)
      else
        sfx := 0;
    end
    else
      sfx := mobjinfo[Ord(castorder[castnum]._type)].attacksound;
    if sfx <> 0 then
      S_StartSound(nil, sfx);
  end;

  if (castframes = 12) and not castdeath then
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
  if casttics > 50 then // [STRIFE] Cap tics
    casttics := 50
  else if casttics = -1 then
    casttics := 15;
end;

//
// F_CastResponder
//
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
  if casttics > 50 then
    casttics := 50;
  castframes := 0;
  castattacking := false;
  if mobjinfo[Ord(castorder[castnum]._type)].deathsound <> 0 then
    S_StartSound(nil, mobjinfo[Ord(castorder[castnum]._type)].deathsound);

  result := true;
end;

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

//
// F_CastDrawer
//
procedure F_CastDrawer;
var
  sprdef: Pspritedef_t;
  sprframe: Pspriteframe_t;
  lump: integer;
  flip: boolean;
  patch: Ppatch_t;
  castname: string;
begin
  // erase the entire screen to a background
  V_DrawPatchFullScreenTMP320x200(D_Help0Lump);

  castname := mobjinfo[Ord(castorder[castnum]._type)].name2;
  if castname = '' then
  begin
    castname := mobjinfo[Ord(castorder[castnum]._type)].name;
    if Pos('MT_', castname) = 1 then
      Delete(castname, 1, 3);
  end;

  F_CastPrint(castname);

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

//
// F_DrawPatchCol
//
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
  desttop := PByte(integer(screens[SCN_TMP]) + x);

  delta := 0;
  tallpatch := false;
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
// F_DrawMap34End
//
var
  laststage: integer = 0;

procedure F_DrawMap34End;
var
  scrolled: integer;
  x: integer;
  p1: Ppatch_t;
  p2: Ppatch_t;
  name: string;
  stage: integer;
begin
  p1 := W_CacheLumpName(DEH_GetString('credit'), PU_LEVEL);
  p2 := W_CacheLumpName(DEH_GetString('vellogo'), PU_LEVEL);

  scrolled := 320 - (finalecount - 230) div 2;
  if scrolled > 320 then
    scrolled := 320
  else if scrolled < 0 then
    scrolled := 0;

  for x := 0 to 320 - 1 do
  begin
    if x + scrolled < 320 then
      F_DrawPatchCol(x, p1, x + scrolled)
    else
      F_DrawPatchCol(x, p2, x + scrolled - 320);
  end;

  if finalecount >= 1130 then
  begin
    if finalecount < 1180 then
    begin
      V_DrawPatch((320 - 13 * 8) div 2,
                  (200 - 8 * 8) div 2,
                   SCN_TMP, 'END0', false);
      laststage := 0;
    end
    else
    begin
      stage := (finalecount - 1180) div 5;
      if stage > 6 then
        stage := 6;
      if stage > laststage then
      begin
        S_StartSound(nil, Ord(sfx_mtalht));
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

//
// F_Drawer
//
// [STRIFE]
// haleyjd 09/13/10: Modified for slideshow, demo version, etc.
//
procedure F_Drawer;
var
  patch: Ppatch_t;
begin
  if slideshow_state = SLIDE_TEXT then
  begin
    V_CopyRect(0, 0, SCN_TMP, 320, 200, 0, 0, SCN_FG, true);
    V_FullScreenStretch;
    exit;
  end;

  case finalestage of
    F_STAGE_CAST:
      begin
        // Cast didn't have a drawer in any released version
        F_CastDrawer();
      end;
    F_STAGE_TEXT:
      begin
        // Draw slideshow panel
        patch := W_CacheLumpName(slideshow_panel, PU_STATIC);
        V_DrawPatchFullScreenTMP320x200(patch);
        Z_ChangeTag(patch, PU_CACHE);
        V_CopyRect(0, 0, SCN_TMP, 320, 200, 0, 0, SCN_FG, true);
        V_FullScreenStretch;
      end;
    F_STAGE_ARTSCREEN:
      begin
        if gamemap <= 29 then
        begin
          // draw credits
          patch := W_CacheLumpName(DEH_GetString('CREDIT'), PU_STATIC);
          V_DrawPatchFullScreenTMP320x200(patch);
          Z_ChangeTag(patch, PU_CACHE);
          V_CopyRect(0, 0, SCN_TMP, 320, 200, 0, 0, SCN_FG, true);
          V_FullScreenStretch;
        end
        else if gamemap = 34 then
        begin
          // demo version - does nothing meaningful in the final version
          F_DrawMap34End;
        end;
      end;
  end;
end;

procedure F_CmdStartCast(const parm: string);
begin
  F_StartCast;
end;

procedure F_Init;
begin
  C_AddCmd('startcast', @F_CmdStartCast);
end;

end.

