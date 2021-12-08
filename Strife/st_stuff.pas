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
//  Status bar code.
//  Does the face/direction indicator animatin.
//  Does palette indicators as well (red pain/berserk, bright pickup)
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit st_stuff;

interface

uses
  doomdef,
  d_event;

// Size of statusbar.
// Now sensitive for scaling.
const
  ST_HEIGHT = 32;
  ST_WIDTH = 320;
  ST_Y = 200 - ST_HEIGHT;

type
  stdrawoptions_t = (stdo_no, stdo_small, stdo_full);

var
  st_palette: integer;
// lump number for PLAYPAL
  lu_palette: integer;

//
// STATUS BAR
//

// Called by main loop.
function ST_Responder(ev: Pevent_t): boolean;

// Called by main loop.
procedure ST_Ticker;

// Called by main loop.
procedure ST_Drawer(dopt: stdrawoptions_t; refresh: boolean);

// Called when the console player is spawned on each level.
procedure ST_Start;

// Called by startup code.
procedure ST_Init;



// States for status bar code.
type
  st_stateenum_t = (
    st_automapstate,
    st_firstpersonstate
  );

// States for the chat code.
  st_chatstateenum_t = (
    StartChatState,
    WaitDestState,
    GetChatState
  );

function ST_DrawExternal: boolean;

implementation

uses
  d_delphi,
  deh_main,
  tables,
  d_net,
  c_cmds,
  d_items,
  d_main,
  z_zone,
  w_wad,
  info,
  info_common,
  info_h,
{$IFDEF OPENGL}
  gl_main,
  gl_render,
{$ELSE}
  r_hires,
  i_video,
  i_system,
{$ENDIF}
  g_game,
  st_lib,
  hu_lib,
  p_inter,
  p_setup,
  p_enemy,
  p_mobj_h,
  p_dialog,
  p_tick,
  d_player,
  r_defs,
  r_main,
  r_draw,
  am_map,
  m_cheat,
  m_rnd,
  m_fixed,
  m_menu,
  s_sound,
// Needs access to LFB.
  v_data,
  v_video,
// State.
  doomstat,
// Data.
  dstrings,
  d_englsh,
  sounds,
// for mapnames
  hu_stuff;

//
// STATUS BAR DATA
//

const
// Palette indices.
// For damage/bonus red-/gold-shifts
  STARTREDPALS = 1;
  STARTBONUSPALS = 9;
  NUMREDPALS = 8;
  NUMBONUSPALS = 4;
// Radiation suit, green shift.
  RADIATIONPAL = 13;

// Location of status bar
  ST_X = 0;

// Location and size of statistics,
//  justified according to widget type.
// Problem is, within which space? STbar? Screen?
// Note: this could be read in by a lump.
//       Problem is, is the stuff rendered
//       into a buffer,
//       or into the frame buffer?

// AMMO number pos.
  ST_AMMOWIDTH = 3;
  ST_AMMOX = 311;
  ST_AMMOY = 162;

// HEALTH number pos.
// haleyjd 20100901: [STRIFE] Adjusted.
  ST_HEALTHWIDTH = 3;
  ST_HEALTHX = 79;
  ST_HEALTHY = 162;

// Ammunition counter.
// haleyjd 20110213 [STRIFE]: ammo counters for the popup widget
  ST_POPUPAMMOX = 206;

  st_yforammo: array[0..Ord(NUMAMMO) - 1] of integer = ( 75, 99, 91, 139, 131, 115, 123);
  st_wforammo: array[0..Ord(NUMAMMO) - 1] of integer = ( 3,  3,  2,  3,   3,   2,   3);

// Indicate maximum ammunition.
// Only needed because backpack exists.
// haleyjd 20110213 [STRIFE]: maxammo counters for the popup widget
  ST_POPUPMAXAMMOX = 239;

// Dimensions given in characters.
  ST_MSGWIDTH = 52;

var

// main player in game
  plyr: Pplayer_t;

// whether in automap or first-person
  st_gamestate: st_stateenum_t;

// whether left-side main status bar is active
  st_statusbarmode: stdrawoptions_t;

// villsa [STRIFE]
  st_dosizedisplay: boolean = false;

// haleyjd 09/01/10: [STRIFE]
// Whether or not a popup is currently displayed
  st_displaypopup: boolean = false;

// villsa [STRIFE]
  st_popupdisplaytics: integer = 0;

// villsa [STRIFE]
// Whether or not show popup objective screen
  st_showobjective: boolean = false;

// villsa [STRIFE]
  st_showinvpop: boolean = false;

// villsa [STRIFE]
  st_showkeys: boolean = false;

// villsa [STRIFE] TODO - identify variables
  st_keypage: integer = -1;

// haleyjd 20100901: [STRIFE] sbar -> invback
// main inventory background and other bits
  invback: Ppatch_t;     // main bar
  stback: Ppatch_t;      // multiplayer background
  invtop: Ppatch_t;      // top bit
  invpop: Ppatch_t;      // popup frame with text
  invpop2: Ppatch_t;     // plain popup frame
  invpbak: Ppatch_t;     // popup background w/details
  invpbak2: Ppatch_t;    // plain popup background
  invcursor: Ppatch_t;   // cursor

// ammo/weapon/armor patches
  invammo: array[0..Ord(NUMAMMO) - 1] of Ppatch_t; // ammo/weapons
  invsigil: array[0..4] of Ppatch_t;      // sigil pieces
  invarmor: array[0..1] of Ppatch_t;      // armor icons

// names for ammo patches
  invammonames: array[00..Ord(NUMAMMO) - 1] of string = (
    'I_BLIT',
    'I_XQRL',
    'I_PQRL',
    'I_BRY1',
    'I_ROKT',
    'I_GRN1',
    'I_GRN2'
  );

// haleyjd 20100901: [STRIFE] Replaced tallnum, shortnum w/inv fonts
// 0-9, green numbers
  invfontg: array[0..9] of Ppatch_t;

// 0-9, yellow numbers
  invfonty: array[0..9] of Ppatch_t;

// ready-weapon widget
  w_ready: st_number_t; // haleyjd [STRIFE]: This is still used.

// haleyjd: [STRIFE] This is still used but was changed to a st_number_t.
// health widget
  w_health: st_number_t;

// ammo widgets
  w_ammo: array[0..Ord(NUMAMMO) - 1] of st_number_t;     // haleyjd [STRIFE]: Still used.

// max ammo widgets
  w_maxammo: array[0..Ord(NUMAMMO) - 1] of st_number_t;  // haleyjd [STRIFE]: Still used.

const
// Massive bunches of cheat shit
//  to keep it from being easy to figure them out.
// Yeah, right...

  cheat_mus_seq: array[0..4] of char = (
    Chr(234), Chr(42), Chr(178), Chr(118), Chr($ff)
  );  // spin

  cheat_god_seq: array[0..10] of char = (
    Chr(246), Chr(182), Chr(118), Chr(178), Chr(42), Chr(246), Chr(46),
    Chr(166), Chr(118), Chr(46), Chr($ff) // omnipotent
  );

  cheat_ammo_seq: array[0..8] of char = (
    Chr(98), Chr(246), Chr(246), Chr(182), Chr(234), Chr(46), Chr(178),
    Chr(58), Chr($ff) // boomstix
  );


  cheat_noclip_seq: array[0..5] of char = (
    Chr(166), Chr(54), Chr(110), Chr(178), Chr(234), Chr($ff) // elvis
  );

  cheat_clev_seq: array[0..4] of char = (
    Chr(106), Chr(178), Chr(102), Chr(46), Chr($ff) // rift
  );

  cheat_mypos_seq: array[0..3] of char = (
    Chr(230), Chr(42), Chr(234), Chr($ff) // gps
  );

  cheat_scoot_seq: array[0..5] of char = (
    Chr(234), Chr(226), Chr(246), Chr(246), Chr(46), Chr($ff) // scoot
  );

  cheat_nuke_seq: array[0..9] of char = (
    Chr(234), Chr(46), Chr(246), Chr(118), Chr(166), Chr(226), Chr(246),
    Chr(54), Chr(38), Chr($ff) // stonecold
  );


  cheat_keys_seq: array[0..5] of char = (
    Chr(114), Chr(178), Chr(182), Chr(182), Chr(186), Chr($ff)  // jimmy
  );

  cheat_stealth_seq: array[0..7] of char = (
    Chr(230), Chr(106), Chr(178), Chr(42), Chr(42), Chr(166),
    Chr(106), Chr($ff)  // gripper
  );

  cheat_midas_seq: array[0..10] of char = (
    Chr(38), Chr(246), Chr(118), Chr(118), Chr(186), Chr(46), Chr(106),
    Chr(174), Chr(182), Chr(42), Chr($ff) // donnytrump
  );

  cheat_lego_seq: array[0..4] of char = (
    Chr(54), Chr(166), Chr(230), Chr(246), Chr($ff) // lego
  );

  cheat_dots_seq: array[0..4] of char = (
    Chr(38), Chr(246), Chr(46), Chr(234), Chr($ff)  // dots
  );


var
// Now what?
  cheat_mus: cheatseq_t;
  cheat_god: cheatseq_t;
  cheat_ammo: cheatseq_t;
  cheat_noclip: cheatseq_t;
  cheat_clev: cheatseq_t;
  cheat_mypos: cheatseq_t;
  cheat_scoot: cheatseq_t;
  cheat_nuke: cheatseq_t;
  cheat_keys: cheatseq_t;
  cheat_stealth: cheatseq_t;
  cheat_midas: cheatseq_t;
  cheat_lego: cheatseq_t;
  cheat_dots: cheatseq_t;

const
// haleyjd 20110224: enumeration for access to powerup cheats
  ST_PUMPUP_B = 0;
  ST_PUMPUP_I = 1;
  ST_PUMPUP_M = 2;
  ST_PUMPUP_H = 3;
  ST_PUMPUP_P = 4;
  ST_PUMPUP_S = 5;
  ST_PUMPUP_T = 6;
  ST_PUMPUP = 7;
  NUM_ST_PUMPUP = 8;

const
  cheat_pumpupb_seq: array[0..7] of char = (Chr(42), Chr(174), Chr(182), Chr(42), Chr(174), Chr(42), Chr(98), Chr($ff) );// pumpupb
  cheat_pumpupi_seq: array[0..7] of char = (Chr(42), Chr(174), Chr(182), Chr(42), Chr(174), Chr(42), Chr(178), Chr($ff) );// pumpupi
  cheat_pumpupm_seq: array[0..7] of char = (Chr(42), Chr(174), Chr(182), Chr(42), Chr(174), Chr(42), Chr(182), Chr($ff) );// pumpupm
  cheat_pumpuph_seq: array[0..7] of char = (Chr(42), Chr(174), Chr(182), Chr(42), Chr(174), Chr(42), Chr(50), Chr($ff) );// pumpuph
  cheat_pumpupp_seq: array[0..7] of char = (Chr(42), Chr(174), Chr(182), Chr(42), Chr(174), Chr(42), Chr(42), Chr($ff) );// pumpupp
  cheat_pumpups_seq: array[0..7] of char = (Chr(42), Chr(174), Chr(182), Chr(42), Chr(174), Chr(42), Chr(234), Chr($ff) );// pumpups
  cheat_pumpupt_seq: array[0..7] of char = (Chr(42), Chr(174), Chr(182), Chr(42), Chr(174), Chr(42), Chr(46), Chr($ff) );// pumpupt
  cheat_pumpup_seq:  array[0..6] of char = (Chr(42), Chr(174), Chr(182), Chr(42), Chr(174), Chr(42), Chr($ff) );// pumpup

var
  cheat_powerup: array[0..NUM_ST_PUMPUP - 1] of cheatseq_t;

//
// Commands
//
function ST_CmdCheckPlayerStatus: boolean;
begin
  if (plyr = nil) or (plyr.mo = nil) or (gamestate <> GS_LEVEL) or demoplayback or netgame then
  begin
    printf('You can''t specify the command at this time.'#13#10);
    result := false;
  end
  else
    result := true;
end;

procedure ST_CmdGod;
begin
  if not ST_CmdCheckPlayerStatus then
    exit;

  if plyr.playerstate <> PST_DEAD then
  begin
    plyr.cheats := plyr.cheats xor CF_GODMODE;
    if plyr.cheats and CF_GODMODE <> 0 then
    begin
      if plyr.mo <> nil then
        plyr.mo.health := mobjinfo[Ord(MT_PLAYER)].spawnhealth;

      plyr.health := mobjinfo[Ord(MT_PLAYER)].spawnhealth;
      plyr._message := STSTR_DQDON;
    end
    else
      plyr._message := STSTR_DQDOFF;
  end
  else
  begin
    C_ExecuteCmd('closeconsole');
    plyr.playerstate := PST_REBORN;
  end;
  plyr.st_update := true;
end;

procedure ST_CmdMassacre;
begin
  if not ST_CmdCheckPlayerStatus then
    exit;

  if (gamestate = GS_LEVEL) and (plyr.mo <> nil) then
  begin
    P_Massacre;
    plyr._message := STSTR_MASSACRE;
  end;
end;

procedure ST_CmdLowGravity;
begin
  if not ST_CmdCheckPlayerStatus then
    exit;

  plyr.cheats := plyr.cheats xor CF_LOWGRAVITY;
  if plyr.cheats and CF_LOWGRAVITY <> 0 then
    plyr._message := STSTR_LGON
  else
    plyr._message := STSTR_LGOFF;
end;

procedure ST_CmdIDFA;
var
  i: integer;
begin
  if not ST_CmdCheckPlayerStatus then
    exit;

  plyr.armorpoints := 200;
  plyr.armortype := 2;

  for i := 0 to Ord(NUMWEAPONS) - 1 do
    if not isdemoversion or weaponinfo[i].availabledemo then
      plyr.weaponowned[i] := true;

  // Takes away the Sigil, even if you already had it...
  plyr.weaponowned[Ord(wp_sigil)] := false;

  for i := 0 to Ord(NUMAMMO) - 1 do
    plyr.ammo[i] := plyr.maxammo[i];

  plyr._message := STSTR_FAADDED;
end;

procedure ST_CmdIDKFA;
var
  i: integer;
begin
  if not ST_CmdCheckPlayerStatus then
    exit;

  ST_CmdIDFA;

  for i := 0 to Ord(NUMCARDS) - 1 do
    plyr.cards[i] := true;

  plyr._message := STSTR_KFAADDED;
end;

procedure ST_CmdIDKEYS;
var
  i: integer;
begin
  if not ST_CmdCheckPlayerStatus then
    exit;

  for i := 0 to Ord(NUMCARDS) - 1 do
    plyr.cards[i] := true;

  plyr._message := STSTR_KEYSADDED;
end;

procedure ST_CmdIDDT;
begin
  if not ST_CmdCheckPlayerStatus then
    exit;

  am_cheating := (am_cheating + 1) mod 3;
end;

procedure ST_CmdIDNoClip;
begin
  if not ST_CmdCheckPlayerStatus then
    exit;

  plyr.cheats := plyr.cheats xor CF_NOCLIP;

  if plyr.cheats and CF_NOCLIP <> 0 then
  begin
    plyr._message := STSTR_NCON;
    plyr.mo.flags := plyr.mo.flags or MF_NOCLIP
  end
  else
  begin
    plyr._message := STSTR_NCOFF;
    plyr.mo.flags := plyr.mo.flags and not MF_NOCLIP
  end
end;

procedure ST_CmdIDMyPos;
var
  buf: string;
begin
  if not ST_CmdCheckPlayerStatus then
    exit;

  sprintf(buf, 'ang = %d, (x, y, z) = (%d, %d, %d)', [
          plyr.mo.angle div $B60B60,
          plyr.mo.x div FRACUNIT,
          plyr.mo.y div FRACUNIT,
          plyr.mo.z div FRACUNIT]);
  plyr._message := buf;
end;

procedure ST_CmdIDMySubSector;
var
  buf: string;
  ss: Psubsector_t;
begin
  if not ST_CmdCheckPlayerStatus then
    exit;

  ss := R_PointInSubsector(plyr.mo.x, plyr.mo.y);
  sprintf(buf, 'ssector = %d, sector = %d'#13#10, [
          (integer(ss) - integer(@subsectors[0])) div SizeOf(subsector_t),
          (integer(ss.sector) - integer(@sectors[0])) div SizeOf(sector_t)]);
  printf(buf);
end;

//
// STATUS BAR CODE
//
procedure ST_FinishRefresh;
begin
  V_CopyRect(ST_X, ST_Y, SCN_ST, ST_WIDTH, ST_HEIGHT, ST_X, ST_Y, SCN_FG, true);
end;

// Respond to keyboard input events,
//  intercept cheats.
function ST_Responder(ev: Pevent_t): boolean;
var
  i: integer;
  buf: string;
  musnum: integer;
  st_keystate: boolean;
  spot: integer;
  map: integer;
  ateit: boolean; // JVAL Cheats ate the event
  ignoreateit: boolean;
  inv: Pinventory_t;

  function check_cheat(cht: Pcheatseq_t; key: char): boolean;
  var
    cht_ret: cheatstatus_t;
  begin
    cht_ret := cht_CheckCheat(cht, key);
    result := cht_ret = cht_acquired;
    if not ateit then
      ateit := (cht_ret in [{cht_pending,} cht_acquired]) // JVAL: 20211101 - Crouch
  end;

begin
  result := false;
  ateit := false;
  ignoreateit := false;
  st_keystate := false;
  // Filter automap on/off.
  if (ev._type = ev_keyup) and
     ((ev.data1 and $ffff0000) = AM_MSGHEADER) then
  begin
    case ev.data1 of
      AM_MSGENTERED:
        begin
          st_gamestate := st_automapstate;
        end;

      AM_MSGEXITED:
        begin
          //  fprintf(stderr, "AM exited\n");
          st_gamestate := st_firstpersonstate;
        end;
    end;
  end
  else if (ev._type = ev_keyup) then
  begin
    if (ev.data1 <> key_invpop) and
       (ev.data1 <> key_mission) and
       (ev.data1 <> key_invkey) then
    begin
      result := false;
      exit;
    end;

    // villsa [STRIFE]
    if ev.data1 = key_invpop then
      st_showinvpop := false
    else
    begin
      if ev.data1 = key_mission then
        st_showobjective := false
      else
      begin
        if ev.data1 = key_invkey then
        begin
          st_showkeys := false;
          st_keystate := false;
        end;
      end;
    end;

    if not st_showkeys and not st_showobjective and not st_showinvpop then
    begin
      if st_popupdisplaytics = 0 then
      begin
        st_displaypopup := false;
        if st_dosizedisplay then
          M_SizeDisplay(1);
        st_dosizedisplay := false;
      end;
    end;

    result := true;
    exit;

  end
  // if a user keypress...
  else if ev._type = ev_keydown then
  begin
    if plyr = nil then
    begin
      result := false;
      exit;
    end;

    if plyr.mo = nil then
    begin
      result := false;
      exit;
    end;

    // haleyjd 20100927: No input allowed when the player is dead
    if plyr.mo.health <= 0 then
    begin
      result := false;
      exit;
    end;

    // JVAL: Prevent cheat code ate movement
    ignoreateit :=
      (ev.data1 = key_up) or
      (ev.data1 = key_down) or
      (ev.data1 = key_strafeleft) or
      (ev.data1 = key_straferight) or
      (ev.data1 = key_left) or
      (ev.data1 = key_right);

    // keydown events
    if ev.data1 = key_invquery then // inventory query
    begin
      inv := @plyr.inventory[plyr.inventorycursor];
      if inv.amount > 0 then
        plyr._message := itoa(inv.amount) + ' ' + mobjinfo[inv._type].name2
    end;


    // villsa [STRIFE]
    if (ev.data1 = key_invpop) or (ev.data1 = key_invkey) or (ev.data1 = key_mission) then
    begin
      if ev.data1 = key_invkey then
      begin
        st_showobjective := false;
        st_showinvpop := false;

        if not st_keystate then
        begin
          st_keystate := true;
          inc(st_keypage);
          if st_keypage > 2 then
          begin
            st_popupdisplaytics := 0;
            st_showkeys := false;
            st_displaypopup := false;
            st_keypage := -1;
            result := true;
            exit;
          end;
        end;

        if netgame then
          st_popupdisplaytics := 20
        else
          st_popupdisplaytics := 50;

        st_showkeys := true
      end
      else
      begin
        if (ev.data1 <> key_mission) or netgame then
        begin
          if ev.data1 = key_invpop then
          begin
            st_keypage := -1;
            st_popupdisplaytics := 0;
            st_showkeys := false;
            st_showobjective := false;
            st_showinvpop := true;
          end;
        end
        else
        begin
          st_showkeys := netgame;
          st_showinvpop := netgame;
          st_keypage := -1;

          st_popupdisplaytics := ev.data2 xor key_mission;

          st_showobjective := true;
        end
      end;

      if st_showkeys or st_showobjective or st_showinvpop then
      begin
        st_displaypopup := true;
        if viewheight = SCREENHEIGHT then
        begin
          M_SizeDisplay(0);
          st_dosizedisplay := true;
        end;
      end;
    end;

    if ev.data1 = key_invleft then // inventory move left
    begin
      if plyr.inventorycursor > 0 then
        dec(plyr.inventorycursor);
      result := true;
      exit;
    end
    else if ev.data1 = key_invright then
    begin
      if plyr.inventorycursor < plyr.numinventory - 1 then
        inc(plyr.inventorycursor);
      result := true;
      exit;
    end
    else if ev.data1 = key_invhome then
    begin
      plyr.inventorycursor := 0;
      result := true;
      exit;
    end
    else if ev.data1 = key_invend then
    begin
      if plyr.numinventory > 0 then
        plyr.inventorycursor := plyr.numinventory - 1
      else
        plyr.inventorycursor := 0;
      result := true;
      exit;
    end;

    if not netgame then
    begin
      // b. - enabled for more debug fun.
      // if (gameskill != sk_nightmare) {

      if check_cheat(@cheat_dots, Chr(ev.data1)) then
      begin
        devparm := not devparm;
        if devparm then
          plyr._message := DEH_GetString('devparm ON')
        else
          plyr._message := DEH_GetString('devparm OFF');
      end
      // 'omnipotent' cheat for toggleable god mode
      else if check_cheat(@cheat_god, Chr(ev.data1)) then
      begin
        ST_CmdGod;
      end
      // [STRIFE]: "BOOMSTIX" cheat for all normal weapons
      else if check_cheat(@cheat_ammo, Chr(ev.data1)) then
      begin
        ST_CmdIDFA;
      end
      // villsa [STRIFE]: "JIMMY" cheat for all keys
      else if check_cheat(@cheat_keys, Chr(ev.data1)) then
      begin
        ST_CmdIDKEYS;
      end
      // Noclip cheat - "ELVIS" (hah-hah :P )
      else if check_cheat(@cheat_noclip, Chr(ev.data1)) then
      begin
        ST_CmdIDNoClip;
      end
      else if check_cheat(@cheat_stealth, Chr(ev.data1)) then
      begin
        // villsa [STRIFE]: "GRIPPER" cheat; nothing to do with stealth...
        plyr.cheats := plyr.cheats xor CF_NOMOMENTUM;
        if plyr.cheats and CF_NOMOMENTUM <> 0 then
          plyr._message := DEH_GetString('STEALTH BOOTS ON')
        else
          plyr._message := DEH_GetString('STEALTH BOOTS OFF');
      end
      // topo
      else if check_cheat(@cheat_amap, Chr(ev.data1)) then
      begin
        ST_CmdIDDT;
      end
      // 'mus' cheat for changing music
      else if check_cheat(@cheat_mus, Chr(ev.data1)) then
      begin
        plyr._message := STSTR_MUS;
        cht_GetParam(@cheat_mus, buf);

        musnum := (Ord(buf[1]) - Ord('0')) * 10 + Ord(buf[2]) - Ord('0');

        if (Ord(buf[1]) - Ord('0')) * 10 + Ord(buf[2]) - Ord('0') > 35 then
            plyr._message := STSTR_NOMUS
        else
          S_ChangeMusic(musnum, true);
      end
      else if check_cheat(@cheat_stealth, Chr(ev.data1)) then
      begin
        // villsa [STRIFE]: "GRIPPER" cheat; nothing to do with stealth...
        plyr.cheats := plyr.cheats xor CF_NOMOMENTUM;
        if plyr.cheats and CF_NOMOMENTUM <> 0 then
          plyr._message := DEH_GetString('STEALTH BOOTS ON')
        else
          plyr._message := DEH_GetString('STEALTH BOOTS OFF');
      end;

      // [STRIFE]: Handle berserk, invisibility, and envirosuit
      for i := 0 to ST_PUMPUP_B + 2 do
      begin
        if check_cheat(@cheat_powerup[i], Chr(ev.data1)) then
        begin
          if plyr.powers[i] <> 0 then
            plyr.powers[i] := intval(i <> 1)
          else
            P_GivePower(plyr, i);
          plyr._message := STSTR_BEHOLDX;
        end;
      end;

      if check_cheat(@cheat_powerup[ST_PUMPUP_H], Chr(ev.data1)) then
      begin
        // [STRIFE]: PUMPUPH gives medical inventory items
        P_GiveItemToPlayer(plyr, Ord(SPR_STMP), MT_INV_MED1);
        P_GiveItemToPlayer(plyr, Ord(SPR_MDKT), MT_INV_MED2);
        P_GiveItemToPlayer(plyr, Ord(SPR_FULL), MT_INV_MED3);
        plyr._message := DEH_GetString('you got the stuff!');
      end
      else if check_cheat(@cheat_powerup[ST_PUMPUP_P], Chr(ev.data1)) then
      begin
        // [STRIFE]: PUMPUPP gives backpack
        if not plyr.backpack then
        begin
          for i := 0 to Ord(NUMAMMO) - 1 do
            plyr.maxammo[i] := 2 * plyr.maxammo[i];
        end;
        plyr.backpack := true;

        for i := 0 to Ord(NUMAMMO) - 1 do
          P_GiveAmmo(plyr, ammotype_t(i), 1);
        plyr._message := DEH_GetString('you got the stuff!');
      end
      else if check_cheat(@cheat_powerup[ST_PUMPUP_S], Chr(ev.data1)) then
      begin
        // [STRIFE]: PUMPUPS gives stamina and accuracy upgrades
        P_GiveItemToPlayer(plyr, Ord(SPR_TOKN), MT_TOKEN_STAMINA);
        P_GiveItemToPlayer(plyr, Ord(SPR_TOKN), MT_TOKEN_NEW_ACCURACY);
        plyr._message := DEH_GetString('you got the stuff!');
      end
      else if check_cheat(@cheat_powerup[ST_PUMPUP_T], Chr(ev.data1)) then
      begin
        // [STRIFE] PUMPUPT gives targeter
        P_GivePower(plyr, Ord(pw_targeter));
        plyr._message := DEH_GetString('you got the stuff!');
      end
      // [STRIFE]: PUMPUP
      else if check_cheat(@cheat_powerup[ST_PUMPUP], Chr(ev.data1)) then
      begin
        // 'behold' power-up menu
        plyr._message := STSTR_BEHOLD;
        result := false;
        exit;
      end
      // 'mypos' for player position
      else if check_cheat(@cheat_mypos, Chr(ev.data1)) then
      begin
        ST_CmdIDMyPos;
      end
      // 'rift' change-level cheat
      else if check_cheat(@cheat_clev, Chr(ev.data1)) then
      begin
        cht_GetParam(@cheat_clev, buf);
        if length(buf) >= 2 then
        begin
          plyr._message := STSTR_WLEV;

          map := (Ord(buf[1]) - Ord('0')) * 10 + Ord(buf[2]) - Ord('0');

          if W_CheckNumForName(P_GetMapName(map)) > -1 then
          begin
            plyr._message := STSTR_CLEV;
            G_RiftExitLevel(map, 0, plyr.mo.angle);
          end;
        end;
      end
      else if check_cheat(@cheat_scoot, Chr(ev.data1)) then
      begin
        cht_GetParam(@cheat_scoot, buf);
        if length(buf) >= 1 then
        begin
          spot := Ord(buf[1]) - Ord('0');

          if spot < 10 then
          begin
            plyr._message := DEH_GetString('Spawning to spot');
            G_RiftCheat(spot);
            result := false;
            exit;
          end;
        end;
      end
      // villsa [STRIFE]
      else if check_cheat(@cheat_nuke, Chr(ev.data1)) then
      begin
        stonecold := stonecold xor true;
        plyr._message := DEH_GetString('Kill ''em.  Kill ''em All');
        result := false;
        exit;
      end
      // villsa [STRIFE]
      else if check_cheat(@cheat_midas, Chr(ev.data1)) then
      begin
        plyr._message := DEH_GetString('YOU GOT THE MIDAS TOUCH, BABY');
        P_GiveItemToPlayer(plyr, Ord(SPR_HELT), MT_TOKEN_TOUGHNESS);
      end
      // villsa [STRIFE]
      // haleyjd 20110224: No sigil in demo version
      else if not isdemoversion and check_cheat(@cheat_lego, Chr(ev.data1)) then
      begin
        plyr.st_update := true;
        if plyr.weaponowned[Ord(wp_sigil)] then
        begin
          inc(plyr.sigiltype);
          if plyr.sigiltype > 4 then
          begin
            plyr.sigiltype := -1;
            plyr.pendingweapon := wp_fist;
            plyr.weaponowned[Ord(wp_sigil)] := false;
          end
        end
        else
        begin
            plyr.weaponowned[Ord(wp_sigil)] := true;
            plyr.sigiltype := 0;
            plyr.pendingweapon := wp_sigil;
        end;
      end;
    end;
  end;
  if not ignoreateit then
    result := result or ateit;
end;

procedure ST_Ticker;
begin
  // must redirect the pointer if the ready weapon has changed.
  if weaponinfo[Ord(plyr.readyweapon)].ammo = am_noammo then
    w_ready.num := @largeammo
  else
    w_ready.num := @plyr.ammo[Ord(weaponinfo[Ord(plyr.readyweapon)].ammo)];

  w_ready.data := Ord(plyr.readyweapon);

  // STRIFE-TODO: Gobbledeegunk.
  (*
    v2 = dword_88490-- == 1; // no clue yet...
    if(v2)
        dword_DC7F4 = dword_DC7F0;*)

  if st_popupdisplaytics > 0 then
  begin
    dec(st_popupdisplaytics);
    if st_popupdisplaytics = 0 then
    begin
      st_displaypopup := false;
      st_showkeys := false;
      st_keypage := -1;

      if st_dosizedisplay then
        M_SizeDisplay(1);  // mondo hack?

      st_dosizedisplay := false;
    end;
  end;
end;

procedure ST_doPaletteStuff;
var
  palette: integer;
  pal: PByteArray;
  cnt: integer;
  bzc: integer;
  p: pointer;
begin
  if plyr = nil then
    exit;

  cnt := plyr.damagecount;

  if plyr.powers[Ord(pw_strength)] <> 0 then
  begin
    // slowly fade the berzerk out
    bzc := 12 - _SHR(plyr.powers[Ord(pw_strength)], 6);

    if bzc > cnt then
      cnt := bzc;
  end;

  if cnt <> 0 then
  begin
    palette := _SHR(cnt + 7, 3);

    if palette >= NUMREDPALS then
      palette := NUMREDPALS - 1;

    palette := palette + STARTREDPALS;
  end
  else if plyr.bonuscount <> 0 then
  begin
    palette := _SHR(plyr.bonuscount + 7, 3);

    if palette >= NUMBONUSPALS then
      palette := NUMBONUSPALS - 1;

    palette := palette + STARTBONUSPALS;
  end
  else if (plyr.powers[Ord(pw_ironfeet)] > 4 * 32) or
          (plyr.powers[Ord(pw_ironfeet)] and 8 <> 0) then
    palette := RADIATIONPAL
  else
    palette := 0;

  if palette <> st_palette then
  begin
    st_palette := palette;
    {$IFDEF OPENGL}
    gld_SetPalette(palette);
    {$ELSE}
    R_SetPalette(palette);
    {$ENDIF}
    p := W_CacheLumpNum(lu_palette, PU_STATIC);
    pal := PByteArray(integer(p) + palette * 768);
    {$IFDEF OPENGL}
    I_SetPalette(pal);
    V_SetPalette(pal);
    {$ELSE}
    IV_SetPalette(pal);
    {$ENDIF}
    Z_ChangeTag(p, PU_CACHE);
  end;
end;


//
// ST_drawNumFontY
//
// haleyjd 20100919: [STRIFE] New function
// Draws a small yellow number for inventory etc.
//
procedure ST_drawNumFontY(x, y: integer; scn: integer; num: integer);
begin
  if num = 0 then
  begin
    V_DrawPatch(x, y, scn, invfonty[0], scn = SCN_FG);
    exit;
  end;

  while num > 0 do
  begin
    V_DrawPatch(x, y, scn, invfonty[num mod 10], scn = SCN_FG);
    x := x - invfonty[0].width - 1;
    num := num div 10;
  end;
end;

//
// ST_drawNumFontY2
//
// haleyjd 20100919: [STRIFE] New function
// As above, but turns negative numbers into zero.
//
procedure ST_drawNumFontY2(x, y: integer; scn: integer; num: integer);
begin
  if num < 0 then
    exit;

  if num = 0 then
  begin
    V_DrawPatch(x, y, scn, invfonty[0], scn = SCN_FG);
    exit;
  end;

  while num > 0 do
  begin
    V_DrawPatch(x, y, scn, invfonty[num mod 10], scn = SCN_FG);
    x := x - invfonty[0].width + 1;
    num := num div 10;
  end;
end;

//
// ST_drawLine
//
// haleyjd 20100920: [STRIFE] New function
// Basic horizontal line drawing routine used for the health bars.
//
procedure ST_drawLine(x, y: integer; len: integer; color: byte);
var
  i: integer;
begin
  for i := y * 320 + x to y * 320 + x + len - 1 do
    screens[SCN_ST][i] := color;
end;

//
// ST_doRefreshSmall
//
procedure ST_doRefreshSmall;
var
  ammo: ammotype_t;
begin
  if firstinterpolation then
  begin
    R_VideoBlanc(SCN_ST, 320 * 194, 320 * 6);

    ST_drawNumFontY2(15, 194, SCN_ST, plyr.health);
    ammo := weaponinfo[Ord(plyr.readyweapon)].ammo;
    if ammo <> am_noammo then
      ST_drawNumFontY2(310, 194, SCN_ST, plyr.ammo[Ord(ammo)]);
  end;
  V_CopyRectTransparent(0, 194, SCN_ST, 320, 6, 0, 194, SCN_FG, true);
end;

//
// ST_doRefresh
//
// haleyjd 20100920: Evidence more than suggests that Rogue moved all status bar
// drawing down to this function.
//
procedure ST_doRefresh;
var
  firstinventory, icon_x, num_x, i, numdrawn: integer;
  lumpnum: integer;
  patch: Ppatch_t;
  iconname: string;
  barlength: integer;
  oldbarlength: integer;
  lifecolor1: integer;
  lifecolor2: integer;
begin
  // draw status bar background to off-screen buff
  if firstinterpolation then
  begin
    // TODO: only sometimes drawing?
    {$IFDEF OPENGL}
    R_VideoBlanc(SCN_ST, 320 * 55, 320 * 114);
    {$ENDIF}

    plyr.st_update := false;

    // draw main status bar
    V_DrawPatch(ST_X, ST_Y, SCN_ST, invback, false);

    // draw multiplayer armor backdrop if netgame
    // haleyjd 20131031: BUG - vanilla is accessing a NULL pointer here when
    // playing back netdemos! It doesn't appear to draw anything, and there
    // is no apparent ill effect on gameplay, so the best we can do is check.
    if netgame and (stback <> nil) then
      V_DrawPatch(ST_X, 173, SCN_ST, stback, false);

    if plyr.inventorycursor >= 6 then
      firstinventory := plyr.inventorycursor - 5
    else
      firstinventory := 0;

    // Draw cursor.
    if plyr.numinventory > 0 then
      V_DrawPatch(35 * (plyr.inventorycursor - firstinventory) + 42, 180, SCN_ST, invcursor, false);

    // Draw inventory bar
    num_x := 68;
    icon_x := 48;
    i := firstinventory;
    numdrawn := 0;
    while num_x < 278 do
    begin
      if plyr.numinventory <= numdrawn then
        break;

      iconname := 'I_' + Info_GetSpriteNameForNum(Ord(plyr.inventory[i].sprite));

      lumpnum := W_CheckNumForName(iconname);
      if lumpnum = -1 then
        patch := W_CacheLumpName('STCFN063', PU_STATIC)
      else
        patch := W_CacheLumpNum(lumpnum, PU_STATIC);
      V_DrawPatch(icon_x, 182, SCN_ST, patch, false);
      Z_ChangeTag(patch, PU_CACHE);
      ST_drawNumFontY(num_x, 191, SCN_ST, plyr.inventory[i].amount);
      num_x := num_x + 35;
      icon_x := icon_x + 35;
      inc(i);
      inc(numdrawn);
    end;

    // haleyjd 20100919: Draw sigil icon
    if plyr.weaponowned[Ord(wp_sigil)] then
      V_DrawPatch(253, 175, SCN_ST, invsigil[plyr.sigiltype], false);

    // haleyjd 20100919: Draw ammo
    if Ord(weaponinfo[Ord(plyr.readyweapon)].ammo) < Ord(NUMAMMO) then
      V_DrawPatch(290, 180, SCN_ST, invammo[Ord(weaponinfo[Ord(plyr.readyweapon)].ammo)], false);

    // haleyjd 20100919: Draw armor
    if plyr.armortype > 0 then
    begin
      V_DrawPatch(2, 177, SCN_ST, invarmor[plyr.armortype - 1], false);
      ST_drawNumFontY(20, 191, SCN_ST, plyr.armorpoints);
    end;

    // haleyjd 20100920: Draw life bars.

    barlength := plyr.health;
    if barlength > 100 then
      barlength := 200 - plyr.health;
    barlength := barlength * 2;

    if plyr.health < 11 then      // Danger, Will Robinson!
      lifecolor1 := 64
    else if plyr.health < 21 then // Caution
      lifecolor1 := 80
    else                       // All is well.
      lifecolor1 := 96;

    if plyr.cheats and CF_GODMODE <> 0 then // Gold, probably a throwback to DOOM.
      lifecolor1 := 226;

    lifecolor2 := lifecolor1 + 3;

    // Draw the normal health bars
    ST_drawLine(49, 172, barlength, lifecolor1);
    ST_drawLine(49, 173, barlength, lifecolor2);
    ST_drawLine(49, 175, barlength, lifecolor1);
    ST_drawLine(49, 176, barlength, lifecolor2);

    // Draw the > 100 health lines
    if plyr.health > 100 then
    begin
      oldbarlength := barlength;
      lifecolor1 := 112;             // Shades of blue
      lifecolor2 := lifecolor1 + 3;

      // take up the difference not drawn by the first (<= 100) bar
      barlength := 200 - barlength;

      ST_drawLine(49 + oldbarlength, 172, barlength, lifecolor1);
      ST_drawLine(49 + oldbarlength, 173, barlength, lifecolor2);
      ST_drawLine(49 + oldbarlength, 175, barlength, lifecolor1);
      ST_drawLine(49 + oldbarlength, 176, barlength, lifecolor2);
    end;

  end;
  ST_FinishRefresh;
end;


procedure ST_Drawer(dopt: stdrawoptions_t; refresh: boolean);
begin
  if amstate = am_only then
    st_statusbarmode := stdo_full
  else
    st_statusbarmode := dopt;

  // Do red-/gold-shifts from damage/items
  ST_doPaletteStuff;
  if st_statusbarmode = stdo_no then
    exit;

  if st_statusbarmode = stdo_small then
    ST_doRefreshSmall
  else
    ST_doRefresh;
end;

//
// ST_calcFrags
//
// haleyjd [STRIFE] New function.
// Calculate frags for display on the frags popup.
//
function ST_calcFrags(pnum: integer): integer;
var
  i: integer;
begin
  result := 0;

  for i := 0 to MAXPLAYERS - 1 do
    if i = pnum then // self-frags
      result := result - players[pnum].frags[i]
    else
      result := result + players[pnum].frags[i];
end;

//
// ST_drawTime
//
// villsa [STRIFE] New function.
// Draws game time on pop up screen
//
procedure ST_drawTime(x, y: integer; time: integer);
var
  hours: integer;
  minutes: integer;
  seconds: integer;
begin
  // haleyjd 20110213: fixed minutes
  hours := time div 3600;
  minutes := (time div 60) mod 60;
  seconds := time mod 60;

  HUlib_drawYellowText(x, y, IntToStrZFill(2, hours) + ':' + IntToStrZFill(2, minutes) + ':' + IntToStrZFill(2, seconds), {$IFDEF OPENGL}SCN_ST{$ELSE}SCN_FG{$ENDIF});
end;

const
  ST_KEYSPERPAGE  = 10;
  ST_KEYS_X       = 20;
  ST_KEYS_Y       = 63;
  ST_KEYNAME_X    = 17;
  ST_KEYNAME_Y    = 4;
  ST_KEYS_YSTEP   = 17;
  ST_KEYS_NUMROWS = 4;
  ST_KEYS_COL2X   = 160;

//
// ST_drawKeysPopup
//
// haleyjd 20110213: [STRIFE] New function
// This has taken the longest out of almost everything to get working properly.
//
function ST_drawKeysPopup: boolean;
var
  x, y, yt, key, keycount: integer;
  info: Pmobjinfo_t;
  pnum: integer;
  frags: integer;
  haskeyinrange: boolean;
begin
{$IFNDEF OPENGL}
  if usemultithread then
    V_DrawPatchTransparentMT(0, 56, SCN_FG, invpbak2, true)
  else
{$ENDIF}
    V_DrawPatchTransparent(0, 56, SCN_FG, invpbak2, true);
  V_DrawPatch(0, 56, {$IFDEF OPENGL}SCN_ST{$ELSE}SCN_FG{$ENDIF}, invpop2, {$IFDEF OPENGL}false{$ELSE}true{$ENDIF});

  if deathmatch <> 0 then
  begin
    // In deathmatch, the keys popup is replaced by a chart of frag counts

    // first column
    y  := 64;
    yt := 66;

    for pnum := 0 to MAXPLAYERS div 2 - 1 do
    begin
      V_DrawPatch(28, y, {$IFDEF OPENGL}SCN_ST{$ELSE}SCN_FG{$ENDIF}, 'stcolor' + itoa(pnum + 1), {$IFDEF OPENGL}false{$ELSE}true{$ENDIF});
      frags := ST_calcFrags(pnum);
      HUlib_drawYellowText(38, yt, player_names[pnum] + itoa(frags), {$IFDEF OPENGL}SCN_ST{$ELSE}SCN_FG{$ENDIF});
      if not playeringame[pnum] then
        HUlib_drawYellowText(28, pnum * 17 + 65, 'X', {$IFDEF OPENGL}SCN_ST{$ELSE}SCN_FG{$ENDIF});
      y := y + 17;
      yt := yt + 17;
    end;

    // second column
    y  := 64;
    yt := 66;
    for pnum := MAXPLAYERS div 2 to MAXPLAYERS - 1 do
    begin
      V_DrawPatch(158, y, {$IFDEF OPENGL}SCN_ST{$ELSE}SCN_FG{$ENDIF}, 'stcolor' + itoa(pnum + 1), {$IFDEF OPENGL}false{$ELSE}true{$ENDIF});
      frags := ST_calcFrags(pnum);
      HUlib_drawYellowText(168, yt, player_names[pnum] + itoa(frags), {$IFDEF OPENGL}SCN_ST{$ELSE}SCN_FG{$ENDIF});
      if not playeringame[pnum] then
        HUlib_drawYellowText(158, pnum * 17 - 3, 'X', {$IFDEF OPENGL}SCN_ST{$ELSE}SCN_FG{$ENDIF});
      y := y + 17;
      yt := yt + 17;
    end;
  end
  else
  begin
    // Bounds-check page number
    if (st_keypage < 0) or (st_keypage > 2) then
    begin
      st_keypage := -1;
      st_popupdisplaytics := 0;
      st_displaypopup := false;

      {$IFDEF OPENGL}
      V_CopyRectTransparent(0, 55, SCN_ST, 320, 105, 0, 55, SCN_FG, true);
      V_CopyRectTransparent(0, 160, SCN_ST, 320, 9, 0, 160, SCN_FG, true);
      {$ENDIF}
      result := false;
      exit;
    end;

    // Are there any keys to display on this page?
    if st_keypage > 0 then
    begin
      haskeyinrange := false;

      key := ST_KEYSPERPAGE * st_keypage;
      keycount := 0;
      while (keycount < ST_KEYSPERPAGE) and (key < Ord(NUMCARDS)) do
      begin
        if plyr.cards[key] then
          haskeyinrange := true;
        inc(key);
        inc(keycount);
      end;

      if not haskeyinrange then
      begin
        st_displaypopup := false;
        st_showkeys := false;
        st_keypage := -1;

        {$IFDEF OPENGL}
        V_CopyRectTransparent(0, 55, SCN_ST, 320, 105, 0, 55, SCN_FG, true);
        V_CopyRectTransparent(0, 160, SCN_ST, 320, 9, 0, 160, SCN_FG, true);
        {$ENDIF}
        result := false;
        exit;
      end;
    end;

    // Draw the keys for the current page
    key := ST_KEYSPERPAGE * st_keypage;
    keycount := 0;
    x := ST_KEYS_X;
    y := ST_KEYS_Y;
    info := @mobjinfo[Ord(MT_KEY_BASE) + key];

    while (keycount < ST_KEYSPERPAGE) and (key < Ord(NUMCARDS)) do
    begin
      if plyr.cards[key] then
      begin
        // Get spawnstate sprite name and load corresponding icon
        V_DrawPatch(x, y, {$IFDEF OPENGL}SCN_ST{$ELSE}SCN_FG{$ENDIF}, 'I_' + Info_GetSpriteNameForNum(states[info.spawnstate].sprite), {$IFDEF OPENGL}false{$ELSE}true{$ENDIF});
        HUlib_drawYellowText(x + ST_KEYNAME_X, y + ST_KEYNAME_Y, info.name2, {$IFDEF OPENGL}SCN_ST{$ELSE}SCN_FG{$ENDIF});
      end;

      if keycount <> ST_KEYS_NUMROWS then
        y := y + ST_KEYS_YSTEP
      else
      begin
        x := ST_KEYS_COL2X;
        y := ST_KEYS_Y;
      end;
      inc(key);
      inc(keycount);
      inc(info);
    end;
  end;

  {$IFDEF OPENGL}
  V_CopyRectTransparent(0, 55, SCN_ST, 320, 105, 0, 55, SCN_FG, true);
  V_CopyRectTransparent(0, 160, SCN_ST, 320, 9, 0, 160, SCN_FG, true);
 {$ENDIF}
  result := true;
end;

//
// ST_DrawExternal
//
// haleyjd 20100901: [STRIFE] New function.
// * Draws external portions of the status bar such the top bar and popups.
//
function ST_DrawExternal: boolean;
var
  i: integer;
  keys: integer;
begin
  if st_statusbarmode = stdo_full then
  begin
    V_DrawPatch(0, 160, SCN_ST, invtop, false);
    STlib_drawNumPositive(@w_health, SCN_ST, false);
    STlib_drawNumPositive(@w_ready, SCN_ST, false);
    {$IFNDEF OPENGL}
    V_CopyRectTransparent(0, 160, SCN_ST, 320, 9, 0, 160, SCN_FG, true);
    {$ENDIF}
  end;

  if not st_displaypopup then
  begin
    {$IFDEF OPENGL}
    if st_statusbarmode = stdo_full then
      V_CopyRectTransparent(0, 160, SCN_ST, 320, 9, 0, 160, SCN_FG, true);
    {$ENDIF}
    result := false;
    exit;
  end;

  // villsa [STRIFE] added 20100926
  if st_showobjective then
  begin
    {$IFNDEF OPENGL}
    if usemultithread then
      V_DrawPatchTransparentMT(0, 56, SCN_FG, invpbak2, true)
    else
    {$ENDIF}
      V_DrawPatchTransparent(0, 56, SCN_FG, invpbak2, true);
    V_DrawPatch(0, 56, {$IFDEF OPENGL}SCN_ST{$ELSE}SCN_FG{$ENDIF}, invpop2, {$IFDEF OPENGL}false{$ELSE}true{$ENDIF});
    mission_objective := M_DialogDimMsg(24, 74, mission_objective, true);
    HUlib_drawYellowText(24, 74, mission_objective, {$IFDEF OPENGL}SCN_ST{$ELSE}SCN_FG{$ENDIF});
    ST_drawTime(210, 64, leveltime div TICRATE);
  end
  else
  begin
    // villsa [STRIFE] keys popup
    if st_showkeys or (st_popupdisplaytics <> 0) then
    begin
      result := ST_drawKeysPopup;
      exit;
    end;

    V_DrawPatchTransparent(0, 56, SCN_FG, invpbak, true);
    {$IFNDEF OPENGL}
     V_DrawPatch(0, 56, SCN_FG, invpop, true);
    {$ENDIF}

    if firstinterpolation then
    begin
      R_VideoBlanc(SCN_ST, 75 * 320, 85 * 320);
      {$IFDEF OPENGL}
       V_DrawPatch(0, 56, SCN_ST, invpop, false);
      {$ENDIF}
      keys := 0;
      for i := 0 to Ord(NUMCARDS) - 1 do
        if plyr.cards[i] then
          inc(keys);

      ST_drawNumFontY2(261, 132, SCN_ST, keys);

      if plyr.weaponowned[Ord(wp_elecbow)] then
        V_DrawPatch(38, 86, SCN_ST, 'CBOWA0', false);

      if plyr.weaponowned[Ord(wp_rifle)] then
        V_DrawPatch(40, 107, SCN_ST,'RIFLA0', false);

      if plyr.weaponowned[Ord(wp_missile)] then
        V_DrawPatch(39, 131, SCN_ST, 'MMSLA0', false);

      if plyr.weaponowned[Ord(wp_hegrenade)] then
        V_DrawPatch(78, 87, SCN_ST, 'GRNDA0', false);

      if plyr.weaponowned[Ord(wp_flame)] then
        V_DrawPatch(80, 117, SCN_ST, 'FLAMA0', false);

      if plyr.weaponowned[Ord(wp_mauler)] then
        V_DrawPatch(75, 142, SCN_ST, 'TRPDA0', false);

      // haleyjd 20110213: draw ammo
      for i := 0 to Ord(NUMAMMO) - 1 do
      begin
        STlib_drawNumPositive(@w_ammo[i], SCN_ST, false);
        STlib_drawNumPositive(@w_maxammo[i], SCN_ST, false);
      end;

      ST_drawNumFontY2(261, 84,  SCN_ST, plyr.accuracy);
      ST_drawNumFontY2(261, 108, SCN_ST, plyr.stamina);

      if plyr.powers[Ord(pw_communicator)] > 0 then
        V_DrawPatch(280, 130, SCN_ST, 'I_COMM', false);
    end;
    {$IFNDEF OPENGL}
    V_CopyRectTransparent(0, 75, SCN_ST, 320, 85, 0, 75, SCN_FG, true);
    {$ENDIF}
  end;
  {$IFDEF OPENGL}
  V_CopyRectTransparent(0, 55, SCN_ST, 320, 105, 0, 55, SCN_FG, true);
  V_CopyRectTransparent(0, 160, SCN_ST, 320, 9, 0, 160, SCN_FG, true);
  {$ENDIF}

  result := true;
end;


procedure ST_LoadGraphics;
var
  i: integer;
  namebuf: string;
begin
  // Load the numbers, tall and short
  for i := 0 to 9 do
  begin
    sprintf(namebuf, 'INVFONG%d', [i]);
    invfontg[i] := W_CacheLumpName(namebuf, PU_STATIC);

    sprintf(namebuf, 'INVFONY%d', [i]);
    invfonty[i] := W_CacheLumpName(namebuf, PU_STATIC);
  end;

  // haleyjd 20100919: load Sigil patches
  if not isdemoversion then
    for i := 0 to 4 do
    begin
      sprintf(namebuf, 'I_SGL%d', [i + 1]);
      invsigil[i] := W_CacheLumpName(namebuf, PU_STATIC);
    end;

  // load ammo patches
  for i := 0 to Ord(NUMAMMO) - 1 do
    invammo[i] := W_CacheLumpName(invammonames[i], PU_STATIC);

  invarmor[0] := W_CacheLumpName('I_ARM2', PU_STATIC);
  invarmor[1] := W_CacheLumpName('I_ARM1', PU_STATIC);

  if netgame then
  begin
    sprintf(namebuf, 'STBACK0%d', [consoleplayer + 1]);
    stback := W_CacheLumpName(namebuf, PU_STATIC);
  end;

  invback := W_CacheLumpName('INVBACK', PU_STATIC);
  invtop := W_CacheLumpName('INVTOP', PU_STATIC);
  invpop := W_CacheLumpName('INVPOP', PU_STATIC);
  invpop2 := W_CacheLumpName('INVPOP2', PU_STATIC);
  invpbak := W_CacheLumpName('INVPBAK', PU_STATIC);
  invpbak2 := W_CacheLumpName('INVPBAK2', PU_STATIC);
  invcursor := W_CacheLumpName('INVCURS', PU_STATIC)
end;

procedure ST_loadData;
begin
  lu_palette := W_GetNumForName(PLAYPAL);
  ST_LoadGraphics;
end;

procedure ST_UnloadGraphics;
var
  i: integer;
begin
  // Load the numbers, tall and short
  for i := 0 to 9 do
  begin
    Z_ChangeTag(invfontg[i], PU_CACHE);
    Z_ChangeTag(invfonty[i], PU_CACHE);
  end;

  // haleyjd 20100919: load Sigil patches
  if not isdemoversion then
    for i := 0 to 4 do
      Z_ChangeTag(invsigil[i], PU_CACHE);

  // load ammo patches
  for i := 0 to Ord(NUMAMMO) - 1 do
    Z_ChangeTag(invammo[i], PU_CACHE);

  Z_ChangeTag(invarmor[0], PU_CACHE);
  Z_ChangeTag(invarmor[1], PU_CACHE);

  if netgame then
  begin
    Z_ChangeTag(stback, PU_CACHE);
  end;

  Z_ChangeTag(invback, PU_CACHE);
  Z_ChangeTag(invtop, PU_CACHE);
  Z_ChangeTag(invpop, PU_CACHE);
  Z_ChangeTag(invpop2, PU_CACHE);
  Z_ChangeTag(invpbak, PU_CACHE);
  Z_ChangeTag(invpbak2, PU_CACHE);
  Z_ChangeTag(invcursor, PU_CACHE);
end;

procedure ST_InitData;
begin
  plyr := @players[consoleplayer];

  st_gamestate := st_firstpersonstate;
  st_statusbarmode := stdo_full;
  st_palette := -1;

  STlib_init;
end;

procedure ST_CreateWidgets;
var
  i: integer;
begin
  // ready weapon ammo
  STlib_initNum(@w_ready,
                ST_AMMOX,
                ST_AMMOY,
                @invfontg,
                @plyr.ammo[Ord(weaponinfo[Ord(plyr.readyweapon)].ammo)],
                ST_AMMOWIDTH);

  // the last weapon type
  w_ready.data := Ord(plyr.readyweapon);

  // health percentage
  STlib_initNum(@w_health,
                ST_HEALTHX,
                ST_HEALTHY,
                @invfontg,
                @plyr.health,
                ST_HEALTHWIDTH);

  // haleyjd 20110213: Ammo Widgets!!!
  for i := 0 to Ord(NUMAMMO) - 1 do
  begin
    STlib_initNum(@w_ammo[i], ST_POPUPAMMOX, st_yforammo[i],
                  @invfonty, @plyr.ammo[i], st_wforammo[i]);

    STlib_initNum(@w_maxammo[i], ST_POPUPMAXAMMOX, st_yforammo[i],
                  @invfonty, @plyr.maxammo[i], st_wforammo[i]);
  end;
end;

var
  st_stopped: boolean;

procedure ST_Stop;
var
  pal: PByteArray;
begin
  if st_stopped then
    exit;

  pal := W_CacheLumpNum(lu_palette, PU_STATIC);
  {$IFDEF OPENGL}
  I_SetPalette(pal);
  V_SetPalette(pal);
  {$ELSE}
  IV_SetPalette(pal);
  {$ENDIF}
  Z_ChangeTag(pal, PU_CACHE);

  st_stopped := true;
end;

procedure ST_Start;
begin
  if not st_stopped then
    ST_Stop;

  ST_InitData;
  ST_CreateWidgets;
  st_stopped := false;
end;

procedure ST_Init;
begin
////////////////////////////////////////////////////////////////////////////////
// Now what?
  cheat_mus.sequence := get_cheatseq_string(cheat_mus_seq);
  cheat_mus.p := get_cheatseq_string(0);
  cheat_god.sequence := get_cheatseq_string(cheat_god_seq);
  cheat_god.p := get_cheatseq_string(0);
  cheat_ammo.sequence := get_cheatseq_string(cheat_ammo_seq);
  cheat_ammo.p := get_cheatseq_string(0);
  cheat_noclip.sequence := get_cheatseq_string(cheat_noclip_seq);
  cheat_noclip.p := get_cheatseq_string(0);
  cheat_clev.sequence := get_cheatseq_string(cheat_clev_seq);
  cheat_clev.p := get_cheatseq_string(0);
  cheat_mypos.sequence := get_cheatseq_string(cheat_mypos_seq);
  cheat_mypos.p := get_cheatseq_string(0);
  cheat_scoot.sequence := get_cheatseq_string(cheat_scoot_seq);
  cheat_scoot.p := get_cheatseq_string(0);
  cheat_nuke.sequence := get_cheatseq_string(cheat_nuke_seq);
  cheat_nuke.p := get_cheatseq_string(0);
  cheat_keys.sequence := get_cheatseq_string(cheat_keys_seq);
  cheat_keys.p := get_cheatseq_string(0);
  cheat_stealth.sequence := get_cheatseq_string(cheat_stealth_seq);
  cheat_stealth.p := get_cheatseq_string(0);
  cheat_midas.sequence := get_cheatseq_string(cheat_midas_seq);
  cheat_midas.p := get_cheatseq_string(0);
  cheat_lego.sequence := get_cheatseq_string(cheat_lego_seq);
  cheat_lego.p := get_cheatseq_string(0);
  cheat_dots.sequence := get_cheatseq_string(cheat_dots_seq);
  cheat_dots.p := get_cheatseq_string(0);

  cheat_powerup[0].sequence := get_cheatseq_string(cheat_pumpupb_seq);
  cheat_powerup[0].p := get_cheatseq_string(0);
  cheat_powerup[1].sequence := get_cheatseq_string(cheat_pumpupi_seq);
  cheat_powerup[1].p := get_cheatseq_string(0);
  cheat_powerup[2].sequence := get_cheatseq_string(cheat_pumpupm_seq);
  cheat_powerup[2].p := get_cheatseq_string(0);
  cheat_powerup[3].sequence := get_cheatseq_string(cheat_pumpuph_seq);
  cheat_powerup[3].p := get_cheatseq_string(0);
  cheat_powerup[4].sequence := get_cheatseq_string(cheat_pumpupp_seq);
  cheat_powerup[4].p := get_cheatseq_string(0);
  cheat_powerup[5].sequence := get_cheatseq_string(cheat_pumpups_seq);
  cheat_powerup[5].p := get_cheatseq_string(0);
  cheat_powerup[6].sequence := get_cheatseq_string(cheat_pumpupt_seq);
  cheat_powerup[6].p := get_cheatseq_string(0);
  cheat_powerup[7].sequence := get_cheatseq_string(cheat_pumpup_seq);
  cheat_powerup[7].p := get_cheatseq_string(0);

  st_palette := 0;

  st_stopped := true;
////////////////////////////////////////////////////////////////////////////////

  ST_loadData;
  C_AddCmd('god, iddqd, omnipotent', @ST_CmdGod);
  C_AddCmd('massacre', @ST_CmdMassacre);
  C_AddCmd('givefullammo, rambo, idfa, boomstix', @ST_CmdIDFA);
  C_AddCmd('giveallkeys, idkeys', @ST_CmdIDKEYS);
  C_AddCmd('lowgravity', @ST_CmdLowGravity);
  C_AddCmd('topo, iddt', @ST_CmdIDDT);
  C_AddCmd('idspispopd, idclip, elvis', @ST_CmdIDNoClip);
  C_AddCmd('gps, idmypos', @ST_CmdIDMyPos);
  C_AddCmd('idmysubsector, mysubsector', @ST_CmdIDMySubSector);
end;

end.

