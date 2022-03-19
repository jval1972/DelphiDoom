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
//  Status bar code.
//  Does the face/direction indicator animatin.
//  Does palette indicators as well (red pain/berserk, bright pickup)
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
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
  ST_WIDTH426 = 426;
  ST_Y = 200 - ST_HEIGHT;

type
  stdrawoptions_t = (stdo_no, stdo_small, stdo_full);

var
  st_palette: integer;
// lump number for PLAYPAL
  lu_palette: integer;

  st_drawoptions: stdrawoptions_t;

//==============================================================================
// ST_Responder
//
// STATUS BAR
//
// Called by main loop.
//
//==============================================================================
function ST_Responder(ev: Pevent_t): boolean;

//==============================================================================
// ST_Ticker
//
// Called by main loop.
//
//==============================================================================
procedure ST_Ticker;

//==============================================================================
// ST_Drawer
//
// Called by main loop.
//
//==============================================================================
procedure ST_Drawer(dopt: stdrawoptions_t; refresh: boolean);

//==============================================================================
// ST_Start
//
// Called when the console player is spawned on each level.
//
//==============================================================================
procedure ST_Start;

//==============================================================================
// ST_Init
//
// Called by startup code.
//
//==============================================================================
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

var
  p_idfaarmor: integer = 200;
  p_idfaarmorclass: integer = 2;
  p_idkfaarmor: integer = 200;
  p_idkfaarmorclass: integer = 2;

implementation

uses
  d_delphi,
  d_net,
  m_menu,
  tables,
  c_cmds,
  d_items,
  z_zone,
  w_wad,
  info,
  info_h,
{$IFDEF OPENGL}
  gl_main,
  gl_render,
{$ELSE}
  r_hires,
  i_video,
{$ENDIF}
  g_game,
  st_lib,
  p_inter,
  p_setup,
  p_enemy,
  p_umapinfo,
  d_player,
  r_defs,
  r_main,
  r_draw,
  am_map,
  m_cheat,
  m_rnd,
  m_fixed,
  s_sound,
// Needs access to LFB.
  v_data,
  v_video,
// State.
  doomstat,
// Data.
  d_englsh,
  sounddata,
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

// N/256*100% probability
//  that the normal face state will change
  ST_FACEPROBABILITY = 96;

// For Responder
  ST_TOGGLECHAT = KEY_ENTER;

// Location of status bar
  ST_X = 0;
  ST_X2 = 104;

  ST_FX = 143;
  ST_FY = ST_Y + 1; // JVAL was 169;

// Number of status faces.
  ST_NUMPAINFACES = 5;
  ST_NUMSTRAIGHTFACES = 3;
  ST_NUMTURNFACES = 2;
  ST_NUMSPECIALFACES = 3;

  ST_FACESTRIDE = ST_NUMSTRAIGHTFACES + ST_NUMTURNFACES + ST_NUMSPECIALFACES;

  ST_NUMEXTRAFACES = 2;

  ST_NUMFACES = ST_FACESTRIDE * ST_NUMPAINFACES + ST_NUMEXTRAFACES;

  ST_TURNOFFSET = ST_NUMSTRAIGHTFACES;
  ST_OUCHOFFSET = ST_TURNOFFSET + ST_NUMTURNFACES;
  ST_EVILGRINOFFSET = ST_OUCHOFFSET + 1;
  ST_RAMPAGEOFFSET = ST_EVILGRINOFFSET + 1;
  ST_GODFACE = ST_NUMPAINFACES * ST_FACESTRIDE;
  ST_DEADFACE = ST_GODFACE + 1;

  ST_FACESX = 143;
  ST_FACESY = ST_Y; // JVAL was 168;

  ST_EVILGRINCOUNT = 2 * TICRATE;
  ST_STRAIGHTFACECOUNT = TICRATE div 2;
  ST_TURNCOUNT = 1 * TICRATE;
  ST_OUCHCOUNT = 1 * TICRATE;
  ST_RAMPAGEDELAY = 2 * TICRATE;

  ST_MUCHPAIN = 20;

// Location and size of statistics,
//  justified according to widget type.
// Problem is, within which space? STbar? Screen?
// Note: this could be read in by a lump.
//       Problem is, is the stuff rendered
//       into a buffer,
//       or into the frame buffer?

// AMMO number pos.
  ST_AMMOWIDTH = 3;
  ST_AMMOX = 44;
  ST_AMMOY = ST_Y + 3; // JVAL was 171;

// HEALTH number pos.
  ST_HEALTHWIDTH = 3;
  ST_HEALTHX = 90;
  ST_HEALTHY = ST_Y + 3; // JVAL was 171;

// Weapon pos.
  ST_ARMSX = 111;
  ST_ARMSY = ST_Y + 4; // JVAL was 172;
  ST_ARMSBGX = 104;
  ST_ARMSBGY = ST_Y; // JVAL was 168;
  ST_ARMSXSPACE = 12;
  ST_ARMSYSPACE = 10;

// Frags pos.
  ST_FRAGSX = 138;
  ST_FRAGSY = ST_Y + 3; // JVAL was 171;
  ST_FRAGSWIDTH = 2;

// ARMOR number pos.
  ST_ARMORWIDTH = 3;
  ST_ARMORX = 221;
  ST_ARMORY = ST_Y + 3; // JVAL was 171;

// Key icon positions.
  ST_KEY0WIDTH = 8;
  ST_KEY0HEIGHT = 5;
  ST_KEY0X = 239;
  ST_KEY0Y = ST_Y + 3; // JVAL was 171;
  ST_KEY1WIDTH = ST_KEY0WIDTH;
  ST_KEY1X = 239;
  ST_KEY1Y = ST_Y + 13; // JVAL was 181;
  ST_KEY2WIDTH = ST_KEY0WIDTH;
  ST_KEY2X = 239;
  ST_KEY2Y = ST_Y + 23; // JVAL was 191;

// Ammunition counter.
  ST_AMMO0WIDTH = 3;
  ST_AMMO0HEIGHT = 6;
  ST_AMMO0X = 288;
  ST_AMMO0Y = ST_Y + 5; // JVAL was 173;
  ST_AMMO1WIDTH = ST_AMMO0WIDTH;
  ST_AMMO1X = 288;
  ST_AMMO1Y = ST_Y + 11; // JVAL was 179;
  ST_AMMO2WIDTH = ST_AMMO0WIDTH;
  ST_AMMO2X = 288;
  ST_AMMO2Y = ST_Y + 23; // JVAL was 191;
  ST_AMMO3WIDTH = ST_AMMO0WIDTH;
  ST_AMMO3X = 288;
  ST_AMMO3Y = ST_Y + 17; // JVAL was 185;

// Indicate maximum ammunition.
// Only needed because backpack exists.
  ST_MAXAMMO0WIDTH = 3;
  ST_MAXAMMO0HEIGHT = 5;
  ST_MAXAMMO0X = 314;
  ST_MAXAMMO0Y = ST_Y + 5; // JVAL was 173;
  ST_MAXAMMO1WIDTH = ST_MAXAMMO0WIDTH;
  ST_MAXAMMO1X = 314;
  ST_MAXAMMO1Y = ST_Y + 11; // JVAL was 179;
  ST_MAXAMMO2WIDTH = ST_MAXAMMO0WIDTH;
  ST_MAXAMMO2X = 314;
  ST_MAXAMMO2Y = ST_Y + 23; // JVAL was 191;
  ST_MAXAMMO3WIDTH = ST_MAXAMMO0WIDTH;
  ST_MAXAMMO3X = 314;
  ST_MAXAMMO3Y = ST_Y + 17; // JVAL was 185;

// pistol
  ST_WEAPON0X = 110;
  ST_WEAPON0Y = ST_Y + 4; // JVAL was 172;

// shotgun
  ST_WEAPON1X = 122;
  ST_WEAPON1Y = ST_Y + 4; // JVAL was 172;

// chain gun
  ST_WEAPON2X = 134;
  ST_WEAPON2Y = ST_Y + 4; // JVAL was 172;

// missile launcher
  ST_WEAPON3X = 110;
  ST_WEAPON3Y = ST_Y + 13; // JVAL was 181;

// plasma gun
  ST_WEAPON4X = 122;
  ST_WEAPON4Y = ST_Y + 13; // JVAL was 181;

 // bfg
  ST_WEAPON5X = 134;
  ST_WEAPON5Y = ST_Y + 13; // JVAL was 181;

// WPNS title
  ST_WPNSX = 109;
  ST_WPNSY = ST_Y + 23; // JVAL was 191;

 // DETH title
  ST_DETHX = 109;
  ST_DETHY = ST_Y + 23; // JVAL was 191;

//Incoming messages window location
//UNUSED
//   ST_MSGTEXTX     (viewwindowx)
//   ST_MSGTEXTY     (viewwindowy+viewheight-18)
  ST_MSGTEXTX = 0;
  ST_MSGTEXTY = 0;
// Dimensions given in characters.
  ST_MSGWIDTH = 52;
// Or shall I say, in lines?
  ST_MSGHEIGHT = 1;

  ST_OUTTEXTX = 0;
  ST_OUTTEXTY = 6;

// Width, in characters again.
  ST_OUTWIDTH = 52;
 // Height, in lines.
  ST_OUTHEIGHT = 1;

  ST_MAPTITLEY = 0;
  ST_MAPHEIGHT = 1;

// Minimum (small display constants)
// Location of medikit
  ST_MX = 8;
  ST_MY = 29;
// Location of health percentage
  ST_MHEALTHX = 60;
  ST_MHEALTHY = ST_Y + 14;
// Location of ammo number
  ST_MAMMOX = 298;
  ST_MAMMOY = ST_Y + 14;
  ST_MAMMOWIDTH = 3;
// Location of ammo patch
  ST_MWX = 308;
  ST_MWY = 29;

var

// main player in game
  plyr: Pplayer_t;

// ST_Start() has just been called
  st_firsttime: boolean;

// used for timing
  st_clock: LongWord;

// used for making messages go away
  st_msgcounter: integer;

// used when in chat
  st_chatstate: st_chatstateenum_t;

// whether in automap or first-person
  st_gamestate: st_stateenum_t;

// whether left-side main status bar is active
  st_statusbaron: boolean;

// whether status bar chat is active
  st_chat: boolean;

// value of st_chat before message popped up
  st_oldchat: boolean;

// whether chat window has the cursor on
  st_cursoron: boolean;

// !deathmatch
  st_notdeathmatch: boolean;

// !deathmatch && st_statusbaron
  st_armson: boolean;

// !deathmatch
  st_fragson: boolean;

// main bar left
  sbar: Ppatch_t;

// main bar right (for old shareware 0.99 version)
  sbar2: Ppatch_t;

// medikit
  smedikit: integer;
// Weapons ammo for small display
  sammo: array[0..3] of integer;

// 0-9, tall numbers
  tallnum: array[0..9] of Ppatch_t;

// tall % sign
  tallpercent: Ppatch_t;

// 0-9, short, yellow (,different!) numbers
  shortnum: array[0..9] of Ppatch_t;

// 3 key-cards, 3 skulls
  keys: array[0..Ord(NUMCARDS) - 1] of Ppatch_t;

// face status patches
  faces: array[0..ST_NUMFACES - 1] of Ppatch_t;

// face background
  faceback: Ppatch_t;

 // main bar right
  armsbg: Ppatch_t;

// weapon ownership patches
  arms: array[0..5, 0..1] of Ppatch_t;

// ready-weapon widget
  w_ready: st_number_t;

 // in deathmatch only, summary of frags stats
  w_frags: st_number_t;

// health widgets
  w_health: st_percent_t;
  w_health2: st_percent_t;

// arms background
  w_armsbg: st_binicon_t;

// weapon ownership widgets
  w_arms: array[0..5] of st_multicon_t;

// face status widget
  w_faces: st_multicon_t;

// keycard widgets
  w_keyboxes: array[0..2] of st_multicon_t;

// armor widget
  w_armor: st_percent_t;

// ammo widgets
  w_ammo: array[0..3] of st_number_t;
  w_ammo2: array[0..3] of st_number_t;

// max ammo widgets
  w_maxammo: array[0..3] of st_number_t;

// number of frags so far in deathmatch
  st_fragscount: integer;

// used to use appopriately pained face
  st_oldhealth: integer;

// used for evil grin
  oldweaponsowned: array[0..Ord(NUMWEAPONS) - 1] of integer;

 // count until face changes
  st_facecount: integer;

// current face index, used by w_faces
  st_faceindex: integer;

// holds key-type for each key box on bar
  keyboxes: array[0..2] of integer;

// a random number per tick
  st_randomnumber: integer;

const
// Massive bunches of cheat shit
//  to keep it from being easy to figure them out.
// Yeah, right...
  cheat_mus_seq: array[0..8] of char = (
    Chr($b2), Chr($26), Chr($b6), Chr($ae), Chr($ea),
    Chr($1),  Chr($0),  Chr($0),  Chr($ff)
  ); // idmus

  cheat_choppers_seq: array[0..10] of char = (
    Chr($b2), Chr($26), Chr($e2), Chr($32), Chr($f6),
    Chr($2a), Chr($2a), Chr($a6), Chr($6a), Chr($ea),
    Chr($ff) // id...
  );

  cheat_god_seq: array[0..5] of char = (
    Chr($b2), Chr($26), Chr($26), Chr($aa), Chr($26),
    Chr($ff)  // iddqd
  );

  cheat_ammo_seq: array[0..5] of char = (
    Chr($b2), Chr($26), Chr($f2), Chr($66), Chr($a2),
    Chr($ff)  // idkfa
  );

  cheat_ammonokey_seq: array[0..4] of char = (
    Chr($b2), Chr($26), Chr($66), Chr($a2), Chr($ff) // idfa
  );

// Smashing Pumpkins Into Samml Piles Of Putried Debris.
  cheat_noclip_seq: array[0..10] of char = (
    Chr($b2), Chr($26), Chr($ea), Chr($2a), Chr($b2), // idspispopd
    Chr($ea), Chr($2a), Chr($f6), Chr($2a), Chr($26),
    Chr($ff)
  );

//
  cheat_commercial_noclip_seq: array[0..6] of char = (
    Chr($b2), Chr($26), Chr($e2), Chr($36), Chr($b2),
    Chr($2a), Chr($ff)  // idclip
  );

  cheat_powerup_seq0: array[0..9] of char = (
    Chr($b2), Chr($26), Chr($62), Chr($a6), Chr($32),
    Chr($f6), Chr($36), Chr($26), Chr($6e), Chr($ff)  // beholdv
  );

  cheat_powerup_seq1: array[0..9] of char = (
    Chr($b2), Chr($26), Chr($62), Chr($a6), Chr($32),
    Chr($f6), Chr($36), Chr($26), Chr($ea), Chr($ff)  // beholds
  );

  cheat_powerup_seq2: array[0..9] of char = (
    Chr($b2), Chr($26), Chr($62), Chr($a6), Chr($32),
    Chr($f6), Chr($36), Chr($26), Chr($b2), Chr($ff)  // beholdi
  );

  cheat_powerup_seq3: array[0..9] of char = (
    Chr($b2), Chr($26), Chr($62), Chr($a6), Chr($32),
    Chr($f6), Chr($36), Chr($26), Chr($6a), Chr($ff)  // beholdr
  );

  cheat_powerup_seq4: array[0..9] of char = (
    Chr($b2), Chr($26), Chr($62), Chr($a6), Chr($32),
    Chr($f6), Chr($36), Chr($26), Chr($a2), Chr($ff)  // beholda
  );

  cheat_powerup_seq5: array[0..9] of char = (
    Chr($b2), Chr($26), Chr($62), Chr($a6), Chr($32),
    Chr($f6), Chr($36), Chr($26), Chr($36), Chr($ff)  // beholdl
  );

  cheat_powerup_seq6: array[0..8] of char = (
    Chr($b2), Chr($26), Chr($62), Chr($a6), Chr($32),
    Chr($f6), Chr($36), Chr($26), Chr($ff)  // behold
  );

  cheat_clev_seq: array[0..9] of char = (
    Chr($b2), Chr($26), Chr($e2), Chr($36), Chr($a6),
    Chr($6e), Chr($1),  Chr($0),  Chr($0),  Chr($ff)  // idclev
  );

// my position cheat
  cheat_mypos_seq: array[0..7] of char = (
    Chr($b2), Chr($26), Chr($b6), Chr($ba), Chr($2a),
    Chr($f6), Chr($ea), Chr($ff) // idmypos
  );

// JVAL: Give All Keys cheat
  cheat_idkeys_seq: array[0..6] of char = (
    Chr($b2), Chr($26), Chr($f2), Chr($a6), Chr($ba),
    Chr($ea), Chr($ff) // idkeys
  );

var
// Now what?
  cheat_mus: cheatseq_t;
  cheat_god: cheatseq_t;
  cheat_ammo: cheatseq_t;
  cheat_ammonokey: cheatseq_t;
  cheat_keys: cheatseq_t;
  cheat_noclip: cheatseq_t;
  cheat_commercial_noclip: cheatseq_t;

  cheat_powerup: array[0..6] of cheatseq_t;

  cheat_choppers: cheatseq_t;
  cheat_clev: cheatseq_t;
  cheat_mypos: cheatseq_t;

//==============================================================================
// ST_CmdCheckPlayerStatus
//
// Commands
//
//==============================================================================
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

//==============================================================================
//
// ST_CmdGod
//
//==============================================================================
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
end;

//==============================================================================
//
// ST_CmdMassacre
//
//==============================================================================
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

//==============================================================================
//
// ST_CmdLowGravity
//
//==============================================================================
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

//==============================================================================
//
// ST_CmdIDFA
//
//==============================================================================
procedure ST_CmdIDFA;
var
  i: integer;
begin
  if not ST_CmdCheckPlayerStatus then
    exit;

  plyr.armorpoints := p_idfaarmor;
  plyr.armortype := p_idfaarmorclass;

  if gamemode = shareware then
  begin
    for i := 0 to Ord(wp_missile) do
      plyr.weaponowned[i] := 1;

    for i := 0 to Ord(NUMAMMO) - 1 do
      if i <> Ord(am_cell) then
        plyr.ammo[i] := plyr.maxammo[i];
  end
  else
  begin
    for i := 0 to Ord(NUMWEAPONS) - 1 do
      plyr.weaponowned[i] := 1;

    for i := 0 to Ord(NUMAMMO) - 1 do
      plyr.ammo[i] := plyr.maxammo[i];
  end;

  plyr._message := STSTR_FAADDED;
end;

//==============================================================================
//
// ST_CmdIDKFA
//
//==============================================================================
procedure ST_CmdIDKFA;
var
  i: integer;
begin
  if not ST_CmdCheckPlayerStatus then
    exit;

  plyr.armorpoints := p_idkfaarmor;
  plyr.armortype := p_idkfaarmorclass;

  if gamemode = shareware then
  begin
    for i := 0 to Ord(wp_missile) - 1 do
      plyr.weaponowned[i] := 1;

    for i := 0 to Ord(NUMAMMO) - 1 do
      if i <> Ord(am_cell) then
        plyr.ammo[i] := plyr.maxammo[i];
  end
  else
  begin
    for i := 0 to Ord(NUMWEAPONS) - 1 do
      plyr.weaponowned[i] := 1;

    for i := 0 to Ord(NUMAMMO) - 1 do
      plyr.ammo[i] := plyr.maxammo[i];
  end;

  for i := 0 to Ord(NUMCARDS) - 1 do
    plyr.cards[i] := true;

  plyr._message := STSTR_KFAADDED;
end;

//==============================================================================
//
// ST_CmdIDKEYS
//
//==============================================================================
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

//==============================================================================
//
// ST_CmdIDDT
//
//==============================================================================
procedure ST_CmdIDDT;
begin
  if not ST_CmdCheckPlayerStatus then
    exit;

  am_cheating := (am_cheating + 1) mod 3;
end;

//==============================================================================
//
// ST_CmdIDNoClip
//
//==============================================================================
procedure ST_CmdIDNoClip;
begin
  if not ST_CmdCheckPlayerStatus then
    exit;

  plyr.cheats := plyr.cheats xor CF_NOCLIP;

  if plyr.cheats and CF_NOCLIP <> 0 then
    plyr._message := STSTR_NCON
  else
    plyr._message := STSTR_NCOFF;
end;

//==============================================================================
//
// ST_CmdIDMyPos
//
//==============================================================================
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

//==============================================================================
// ST_TALLNUMWIDTH
//
// Should be set to patch width
//  for tall numbers later on
//
//==============================================================================
function ST_TALLNUMWIDTH: integer;
begin
  result := tallnum[0].width;
end;

//==============================================================================
//
// ST_MAPWIDTH
//
//==============================================================================
function ST_MAPWIDTH: integer;
begin
  result := Length(mapnames[(gameepisode - 1) * 9 + (gamemap - 1)]);
end;

//==============================================================================
// ST_RefreshBackground
//
// STATUS BAR CODE
//
//==============================================================================
procedure ST_RefreshBackground;
begin
  if st_statusbaron then
  begin
    if wide_stbar = 426 then
      V_DrawPatch(ST_X, 0, SCN_ST426, sbar, false)
    else
    begin
      V_DrawPatch(ST_X, 0, SCN_ST, sbar, false);
      if oldsharewareversion then
        V_DrawPatch(ST_X2, 0, SCN_ST, sbar2, false);
    end;
    if netgame then
      ST_DrawPatch(ST_FX, 0, faceback);
  end;
end;

//==============================================================================
//
// ST_FinishRefresh
//
//==============================================================================
procedure ST_FinishRefresh;
begin
  if wide_stbar = 426 then
    V_CopyRect(ST_X, 0, SCN_ST426, ST_WIDTH426, ST_HEIGHT, ST_X, ST_Y, SCN_FG, true)
  else
    V_CopyRect(ST_X, 0, SCN_ST, ST_WIDTH, ST_HEIGHT, ST_X, ST_Y, SCN_FG, true);
end;

//==============================================================================
// ST_Responder
//
// Respond to keyboard input events,
//  intercept cheats.
//
//==============================================================================
function ST_Responder(ev: Pevent_t): boolean;
var
  i: integer;
  buf: string;
  musnum: integer;
  epsd: integer;
  map: integer;
  ateit: boolean; // JVAL Cheats ate the event
  entry: Pmapentry_t;

  function check_cheat(cht: Pcheatseq_t; key: char): boolean;
  var
    cht_ret: cheatstatus_t;
  begin
    cht_ret := cht_CheckCheat(cht, key);
    result := cht_ret = cht_acquired;
    if not ateit then
      ateit := (cht_ret in [cht_pending, cht_acquired])
  end;

begin
  result := false;
  ateit := false;
  // Filter automap on/off.
  if (ev._type = ev_keyup) and
     ((ev.data1 and $ffff0000) = AM_MSGHEADER) then
  begin
    case ev.data1 of
      AM_MSGENTERED:
        begin
          st_gamestate := st_automapstate;
          st_firsttime := true;
        end;

      AM_MSGEXITED:
        begin
          //  fprintf(stderr, "AM exited\n");
          st_gamestate := st_firstpersonstate;
        end;
    end;
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

    if not netgame then
    begin
      // b. - enabled for more debug fun.
      // if (gameskill != sk_nightmare) {

      // 'dqd' cheat for toggleable god mode
      if check_cheat(@cheat_god, Chr(ev.data1)) then
      begin
        ST_CmdGod;
      end
      // 'fa' cheat for killer fucking arsenal
      else if check_cheat(@cheat_ammonokey, Chr(ev.data1)) then
      begin
        ST_CmdIDFA;
      end
      // JVAL: 'keys' cheat
      else if check_cheat(@cheat_keys, Chr(ev.data1)) then
      begin
        ST_CmdIDKEYS;
      end
      // 'kfa' cheat for key full ammo
      else if check_cheat(@cheat_ammo, Chr(ev.data1)) then
      begin
        ST_CmdIDKFA;
      end
      else if check_cheat(@cheat_amap, Chr(ev.data1)) then
      begin
        ST_CmdIDDT;
      end
      // 'mus' cheat for changing music
      else if check_cheat(@cheat_mus, Chr(ev.data1)) then
      begin
        plyr._message := STSTR_MUS;
        cht_GetParam(@cheat_mus, buf);

        if gamemode = commercial then
        begin
          musnum := Ord(mus_runnin) + (Ord(buf[1]) - Ord('0')) * 10 + Ord(buf[2]) - Ord('0') - 1;

          if (Ord(buf[1]) - Ord('0')) * 10 + Ord(buf[2]) - Ord('0') > 35 then
            plyr._message := STSTR_NOMUS
          else
            S_ChangeMusic(musnum, true);
        end
        else
        begin
          musnum := Ord(mus_e1m1) + (Ord(buf[1]) - Ord('1')) * 9 + Ord(buf[2]) - Ord('1');

          if (musnum > 0) and (buf[2] <> '0') and
             ( ((musnum < 28) and (gamemode <> shareware)) or
               ((musnum < 10) and (gamemode = shareware))) then
            S_ChangeMusic(musnum, true)
          else
            plyr._message := STSTR_NOMUS;
        end;
      end
      // Simplified, accepting both "noclip" and "idspispopd".
      // no clipping mode cheat
      else if check_cheat(@cheat_noclip, Chr(ev.data1)) or
              check_cheat(@cheat_commercial_noclip, Chr(ev.data1)) then
      begin
        ST_CmdIDNoClip;
      end;
      // 'behold?' power-up cheats
      for i := 0 to 5 do
      begin
        if check_cheat(@cheat_powerup[i], Chr(ev.data1)) then
        begin
          if plyr.powers[i] = 0 then
            P_GivePower(plyr, i)
          else if i <> Ord(pw_strength) then
            plyr.powers[i] := 1
          else
            plyr.powers[i] := 0;

          plyr._message := STSTR_BEHOLDX;
        end;
      end;

      // 'behold' power-up menu
      if check_cheat(@cheat_powerup[6], Chr(ev.data1)) then
      begin
        plyr._message := STSTR_BEHOLD;
      end
      // 'choppers' invulnerability & chainsaw
      else if check_cheat(@cheat_choppers, Chr(ev.data1)) then
      begin
        plyr.weaponowned[Ord(wp_chainsaw)] := 1;
        plyr.powers[Ord(pw_invulnerability)] := INVULNTICS;
        plyr._message := STSTR_CHOPPERS;
      end
      // 'mypos' for player position
      else if check_cheat(@cheat_mypos, Chr(ev.data1)) then
      begin
        ST_CmdIDMyPos;
      end;
    end;

    // 'clev' change-level cheat
    if check_cheat(@cheat_clev, Chr(ev.data1)) then
    begin
      cht_GetParam(@cheat_clev, buf);
      plyr._message := STSTR_WLEV;

      if gamemode = commercial then
      begin
        epsd := 0;
        map := (Ord(buf[1]) - Ord('0')) * 10 + Ord(buf[2]) - Ord('0');
      end
      else
      begin
        epsd := Ord(buf[1]) - Ord('0');
        map := Ord(buf[2]) - Ord('0');
        // Catch invalid maps.
        if epsd < 1 then
          exit;
      end;

      if map < 1 then
        exit;

      // First check if we have a mapinfo entry for the requested level.
      // If this is present the remaining checks should be skipped.
      entry := G_LookupMapinfo(epsd, map);
      if entry <> nil then
        if W_CheckNumForName(P_GetMapName(epsd, map)) > -1 then
        begin
          plyr._message := STSTR_CLEV;
          G_DeferedInitNew(gameskill, epsd, map);
          result := result or ateit;
          exit;
        end;

      // Ohmygod - this is not going to work.
      if (gamemode = retail) and
         ((epsd > 4) or (map > 9)) then
        exit;

      if (gamemode = registered) and
         ((epsd > 3) or (map > 9)) then
        exit;

      if (gamemode = shareware) and
         ((epsd > 1) or (map > 9)) then
        exit;

      if (gamemode = commercial) and
         ((epsd > 1) or (map > 34)) then
        exit;

      // JVAL: Chex Support
      if customgame in [cg_chex, cg_chex2] then
      begin
        epsd := 1;
        if map > 5 then
          map := 5;
      end;

      // So be it.
      if W_CheckNumForName(P_GetMapName(epsd, map)) > -1 then
      begin
        plyr._message := STSTR_CLEV;
        G_DeferedInitNew(gameskill, epsd, map);
      end;
    end;
  end;
  result := result or ateit;
end;

var
  lastcalc: integer;
  oldhealth: integer;

//==============================================================================
//
// ST_calcPainOffset
//
//==============================================================================
function ST_calcPainOffset: integer;
var
  health: integer;
begin
  if plyr.health > 100 then
    health := 100
  else
    health := plyr.health;

  if health <> oldhealth then
  begin
    lastcalc := ST_FACESTRIDE * (((100 - health) * ST_NUMPAINFACES) div 101);
    oldhealth := health;
  end;
  result := lastcalc;
end;

//
// This is a not-very-pretty routine which handles
//  the face states and their timing.
// the precedence of expressions is:
//  dead > evil grin > turned head > straight ahead
//
var
  lastattackdown: integer;
  priority: integer;

//==============================================================================
//
// ST_UpdateFaceWidget
//
//==============================================================================
procedure ST_UpdateFaceWidget;
var
  i: integer;
  to_right: boolean;
  badguyangle: angle_t;
  diffang: angle_t;
  doevilgrin: boolean;
begin
  if priority < 10 then
  begin
    // dead
    if plyr.health = 0 then
    begin
      priority := 9;
      st_faceindex := ST_DEADFACE;
      st_facecount := 1;
    end;
  end;

  if priority < 9 then
  begin
    if plyr.bonuscount <> 0 then
    begin
      // picking up bonus
      doevilgrin := false;

      for i := 0 to Ord(NUMWEAPONS) - 1 do
      begin
        if oldweaponsowned[i] <> plyr.weaponowned[i] then
        begin
          doevilgrin := true;
          oldweaponsowned[i] := plyr.weaponowned[i];
        end;
      end;
      if doevilgrin then
      begin
        // evil grin if just picked up weapon
        priority := 8;
        st_facecount := ST_EVILGRINCOUNT;
        st_faceindex := ST_calcPainOffset + ST_EVILGRINOFFSET;
      end;
    end;
  end;

  if priority < 8 then
  begin
    if (plyr.damagecount <> 0) and
       (plyr.attacker <> nil) and
       (plyr.attacker <> plyr.mo) then
    begin
      // being attacked
      priority := 7;

      if ((G_PlayingEngineVersion <= VERSION203) and (plyr.health - st_oldhealth > ST_MUCHPAIN)) or
         ((G_PlayingEngineVersion > VERSION203) and (st_oldhealth - plyr.health > ST_MUCHPAIN)) then
      begin
        if G_PlayingEngineVersion > VERSION203 then // JVAL: actually, I must check > VERSION204 :)
          priority := 8;
        st_facecount := ST_TURNCOUNT;
        st_faceindex := ST_calcPainOffset + ST_OUCHOFFSET;
      end
      else
      begin
        badguyangle :=
          R_PointToAngle2(plyr.mo.x, plyr.mo.y, plyr.attacker.x, plyr.attacker.y);

        if badguyangle > plyr.mo.angle then
        begin
          // whether right or left
          diffang := badguyangle - plyr.mo.angle;
          to_right := diffang > ANG180;
        end
        else
        begin
          // whether left or right
          diffang := plyr.mo.angle - badguyangle;
          to_right := diffang <= ANG180;
        end; // confusing, aint it?

        st_facecount := ST_TURNCOUNT;
        st_faceindex := ST_calcPainOffset;

        if diffang < ANG45 then
        begin
          // head-on
          st_faceindex := st_faceindex + ST_RAMPAGEOFFSET;
        end
        else if to_right then
        begin
          // turn face right
          st_faceindex := st_faceindex + ST_TURNOFFSET;
        end
        else
        begin
          // turn face left
          st_faceindex := st_faceindex + ST_TURNOFFSET + 1;
        end;
      end;
    end;
  end;

  if priority < 7 then
  begin
    // getting hurt because of your own damn stupidity
    if plyr.damagecount <> 0 then
    begin
      if plyr.health - st_oldhealth > ST_MUCHPAIN then
      begin
        priority := 7;
        st_facecount := ST_TURNCOUNT;
        st_faceindex := ST_calcPainOffset + ST_OUCHOFFSET;
      end
      else
      begin
        priority := 6;
        st_facecount := ST_TURNCOUNT;
        st_faceindex := ST_calcPainOffset + ST_RAMPAGEOFFSET;
      end;
    end;
  end;

  if priority < 6 then
  begin
    // rapid firing
    if plyr.attackdown then
    begin
      if lastattackdown = -1 then
        lastattackdown := ST_RAMPAGEDELAY
      else
      begin
        dec(lastattackdown);
        if lastattackdown = 0 then
        begin
          priority := 5;
          st_faceindex := ST_calcPainOffset + ST_RAMPAGEOFFSET;
          st_facecount := 1;
          lastattackdown := 1;
        end;
      end;
    end
    else
      lastattackdown := -1;
  end;

  if priority < 5 then
  begin
    // invulnerability
    if (plyr.cheats and CF_GODMODE <> 0) or
       (plyr.powers[Ord(pw_invulnerability)] <> 0) then
    begin
      priority := 4;

      st_faceindex := ST_GODFACE;
      st_facecount := 1;
    end;
  end;

  // look left or look right if the facecount has timed out
  if st_facecount = 0 then
  begin
    st_faceindex := ST_calcPainOffset + (st_randomnumber mod 3);
    st_facecount := ST_STRAIGHTFACECOUNT;
    priority := 0;
  end;

  dec(st_facecount);
end;

//==============================================================================
//
// ST_UpdateWidgets
//
//==============================================================================
procedure ST_UpdateWidgets;
var
  i: integer;
begin
  if plyr = nil then
    exit;

  // must redirect the pointer if the ready weapon has changed.
  if weaponinfo[Ord(plyr.readyweapon)].ammo = am_noammo then
    w_ready.num := @largeammo
  else
    w_ready.num := @plyr.ammo[Ord(weaponinfo[Ord(plyr.readyweapon)].ammo)];

  w_ready.data := Ord(plyr.readyweapon);

  // update keycard multiple widgets
  for i := 0 to 2 do
  begin
    if plyr.cards[i] then
      keyboxes[i] := i
    else
      keyboxes[i] := -1;

    if plyr.cards[i + 3] then
      keyboxes[i] := i + 3;
  end;

  // refresh everything if this is him coming back to life
  ST_UpdateFaceWidget;

  // used by the w_armsbg widget
  st_notdeathmatch := deathmatch = 0;

  // used by w_arms[] widgets
  st_armson := st_statusbaron and st_notdeathmatch;

  // used by w_frags widget
  st_fragson := (deathmatch <> 0) and st_statusbaron;
  st_fragscount := 0;

  for i := 0 to MAXPLAYERS - 1 do
  begin
    if i <> consoleplayer then
      st_fragscount := st_fragscount + plyr.frags[i]
    else
      st_fragscount := st_fragscount - plyr.frags[i];
  end;

  // get rid of chat window if up because of message
  dec(st_msgcounter);
  if st_msgcounter = 0 then
    st_chat := st_oldchat;
end;

//==============================================================================
//
// ST_Ticker
//
//==============================================================================
procedure ST_Ticker;
begin
  inc(st_clock);
  st_randomnumber := M_Random;
  ST_UpdateWidgets;
  if plyr <> nil then
    st_oldhealth := plyr.health;
end;

//==============================================================================
//
// ST_DoPaletteStuff
//
//==============================================================================
procedure ST_DoPaletteStuff;
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

  if customgame in [cg_chex, cg_chex2] then
    if (palette >= STARTREDPALS) and (palette < STARTREDPALS + NUMREDPALS) then
      palette := RADIATIONPAL;

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

//==============================================================================
//
// ST_DrawWidgets
//
//==============================================================================
procedure ST_DrawWidgets(refresh: boolean);
var
  i: integer;
begin
  // used by w_arms[] widgets
  st_armson := st_statusbaron and (deathmatch = 0);

  // used by w_frags widget
  st_fragson := (deathmatch <> 0) and st_statusbaron;

  STlib_updateNum(@w_ready, refresh);

  for i := 0 to 3 do
  begin
    STlib_updateNum(@w_ammo[i], refresh);
    STlib_updateNum(@w_maxammo[i], refresh);
  end;

  STlib_updatePercent(@w_health, refresh, false);
  STlib_updatePercent(@w_armor, refresh, false);

  STlib_updateBinIcon(@w_armsbg, refresh);

  for i := 0 to 5 do
    STlib_updateMultIcon(@w_arms[i], refresh);

  STlib_updateMultIcon(@w_faces, refresh);

  for i := 0 to 2 do
    STlib_updateMultIcon(@w_keyboxes[i], refresh);

  STlib_updateNum(@w_frags, refresh);
end;

//==============================================================================
//
// ST_Refresh
//
//==============================================================================
procedure ST_Refresh(refresh: boolean);
begin
  // draw status bar background to off-screen buff
  ST_RefreshBackground;
  // and refresh all widgets
  ST_DrawWidgets(refresh);
  if refresh then
    ST_FinishRefresh;
end;

//==============================================================================
//
// ST_RefreshBackgroundSmall
//
//==============================================================================
procedure ST_RefreshBackgroundSmall;
begin
  if wide_stbar = 426 then
    R_VideoBlanc(SCN_ST426, 0, ST_WIDTH426 * ST_HEIGHT)
  else
    R_VideoBlanc(SCN_ST, 0, ST_WIDTH * ST_HEIGHT);
end;

//==============================================================================
//
// ST_DrawWidgetsSmall
//
//==============================================================================
procedure ST_DrawWidgetsSmall;
var
  ammo: integer;
begin
  STlib_updatePercent(@w_health2, true, true);
  ammo := Ord(weaponinfo[Ord(plyr.readyweapon)].ammo);
  if ammo <> Ord(am_noammo) then
  begin
    STlib_updateNum(@w_ammo2[ammo], true);
    if sammo[ammo] >= 0 then
      ST_DrawPatch(ST_MWX, ST_MWY, sammo[ammo]);
  end;
  ST_DrawPatch(ST_MX, ST_MY, smedikit);
end;

//==============================================================================
//
// ST_FinishRefreshSmall
//
//==============================================================================
procedure ST_FinishRefreshSmall;
begin
  if wide_stbar = 426 then
    V_CopyRectTransparent(0, 0, SCN_ST426, ST_WIDTH426, ST_HEIGHT, 0, ST_Y, SCN_FG, true)
  else
    V_CopyRectTransparent(0, 0, SCN_ST, ST_WIDTH, ST_HEIGHT, 0, ST_Y, SCN_FG, true);
end;

//==============================================================================
//
// ST_RefreshSmall
//
//==============================================================================
procedure ST_RefreshSmall;
begin
  // draw status bar background to off-screen buff
  ST_RefreshBackgroundSmall;
  // and refresh all widgets
  ST_DrawWidgetsSmall;
  ST_FinishRefreshSmall;
end;

//==============================================================================
//
// ST_DoRefresh
//
//==============================================================================
procedure ST_DoRefresh;
begin
  st_firsttime := false;
  ST_Refresh(true);
end;

//==============================================================================
//
// ST_DiffDraw
//
//==============================================================================
procedure ST_DiffDraw;
begin
  // update all widgets
  ST_DrawWidgets(true);

  ST_FinishRefresh;
end;

//==============================================================================
//
// ST_Drawer
//
//==============================================================================
procedure ST_Drawer(dopt: stdrawoptions_t; refresh: boolean);
begin
  st_drawoptions := dopt;
  st_statusbaron := (dopt <> stdo_no) or (amstate = am_only);
  {$IFDEF OPENGL}
  if not st_statusbaron then
  begin
    ST_DoPaletteStuff;
    exit;
  end;
  {$ENDIF}
  st_firsttime := st_firsttime or refresh;

  // Do red-/gold-shifts from damage/items
  ST_DoPaletteStuff;
  if dopt <> stdo_small then
  begin
    if firstinterpolation or (menuactive and (shademenubackground >= 1)) then
    begin
      ST_Refresh(st_firsttime);
      // If just after ST_Start(), refresh all
      if st_firsttime then
        ST_DoRefresh
      // Otherwise, update as little as possible
      else
        ST_DiffDraw;
    end;
  end
  else
    ST_RefreshSmall;
end;

//==============================================================================
//
// ST_LoadGraphics
//
//==============================================================================
procedure ST_LoadGraphics;
var
  i: integer;
  j: integer;
  facenum: integer;
  namebuf: string;
  lump: integer;
begin
  // Load the numbers, tall and short
  for i := 0 to 9 do
  begin
    sprintf(namebuf, 'STTNUM%d', [i]);
    tallnum[i] := W_CacheLumpName(namebuf, PU_STATIC);

    sprintf(namebuf, 'STYSNUM%d', [i]);
    shortnum[i] := W_CacheLumpName(namebuf, PU_STATIC);
  end;

  // Load percent key.
  //Note: why not load STMINUS here, too?
  tallpercent := W_CacheLumpName('STTPRCNT', PU_STATIC);

  // key cards
  for i := 0 to Ord(NUMCARDS) - 1 do
  begin
    sprintf(namebuf, 'STKEYS%d', [i]);
    keys[i] := W_CacheLumpName(namebuf, PU_STATIC);
  end;

  // arms background
  armsbg := W_CacheLumpName('STARMS', PU_STATIC);

  // arms ownership widgets
  for i := 0 to 5 do
  begin
    sprintf(namebuf, 'STGNUM%d', [i + 2]);

    // gray #
    arms[i][0] := W_CacheLumpName(namebuf, PU_STATIC);

    // yellow #
    arms[i][1] := shortnum[i + 2];
  end;

  // face backgrounds for different color players
  sprintf(namebuf, 'STFB%d', [consoleplayer]);
  faceback := W_CacheLumpName(namebuf, PU_STATIC);

  // status bar background bits
  lump :=  W_CheckNumForName('STBAR');
  if lump = -1 then
  begin
    oldsharewareversion := true;
    lump :=  W_CheckNumForName('STMBARL');
    sbar := W_CacheLumpNum(lump, PU_STATIC);
    lump :=  W_CheckNumForName('STMBARR');
    sbar2 := W_CacheLumpNum(lump, PU_STATIC);
    wide_stbar := 320;
  end
  else
  begin
    sbar := W_CacheLumpNum(lump, PU_STATIC);
    wide_stbar := sbar.width;
  end;

// JVAL
// Statusbar medikit, use stimpack patch (STIMA0)
  smedikit := W_GetNumForName('STIMA0');

// JVAL
// Weapons ammo for small display
  sammo[0] := W_GetNumForName('CLIPA0');
  sammo[1] := W_GetNumForName('SHELA0');
// JVAL
// Cells do not exist in shareware version, use W_CheckNumForName instead of W_GetNumForName
  sammo[2] := W_CheckNumForName('CELLA0');
  sammo[3] := W_GetNumForName('ROCKA0');

  // face states
  facenum := 0;
  for i := 0 to ST_NUMPAINFACES - 1 do
  begin
    for j := 0 to ST_NUMSTRAIGHTFACES - 1 do
    begin
      sprintf(namebuf, 'STFST%d%d', [i, j]);
      faces[facenum] := W_CacheLumpName(namebuf, PU_STATIC);
      inc(facenum);
    end;
    sprintf(namebuf, 'STFTR%d0', [i]); // turn right
    faces[facenum] := W_CacheLumpName(namebuf, PU_STATIC);
    inc(facenum);
    sprintf(namebuf, 'STFTL%d0', [i]); // turn left
    faces[facenum] := W_CacheLumpName(namebuf, PU_STATIC);
    inc(facenum);
    sprintf(namebuf, 'STFOUCH%d', [i]); // ouch!
    faces[facenum] := W_CacheLumpName(namebuf, PU_STATIC);
    inc(facenum);
    sprintf(namebuf, 'STFEVL%d', [i]); // evil grin ;)
    faces[facenum] := W_CacheLumpName(namebuf, PU_STATIC);
    inc(facenum);
    sprintf(namebuf, 'STFKILL%d', [i]); // pissed off
    faces[facenum] := W_CacheLumpName(namebuf, PU_STATIC);
    inc(facenum);
  end;
  faces[facenum] := W_CacheLumpName('STFGOD0', PU_STATIC);
  inc(facenum);
  faces[facenum] := W_CacheLumpName('STFDEAD0', PU_STATIC);
end;

//==============================================================================
//
// ST_LoadData
//
//==============================================================================
procedure ST_LoadData;
begin
  lu_palette := W_GetNumForName(PLAYPAL);
  ST_LoadGraphics;
end;

//==============================================================================
//
// ST_UnloadGraphics
//
//==============================================================================
procedure ST_UnloadGraphics;
var
  i: integer;
begin
  // unload the numbers, tall and short
  for i := 0 to 9 do
  begin
    Z_ChangeTag(tallnum[i], PU_CACHE);
    Z_ChangeTag(shortnum[i], PU_CACHE);
  end;
  // unload tall percent
  Z_ChangeTag(tallpercent, PU_CACHE);

  // unload arms background
  Z_ChangeTag(armsbg, PU_CACHE);

  // unload gray #'s
  for i := 0 to 5 do
    Z_ChangeTag(arms[i][0], PU_CACHE);

  // unload the key cards
  for i := 0 to Ord(NUMCARDS) - 1 do
    Z_ChangeTag(keys[i], PU_CACHE);

  Z_ChangeTag(sbar, PU_CACHE);
  if oldsharewareversion then
    Z_ChangeTag(sbar2, PU_CACHE);
  Z_ChangeTag(faceback, PU_CACHE);

  for i := 0 to ST_NUMFACES - 1 do
    Z_ChangeTag(faces[i], PU_CACHE);

  // Note: nobody ain't seen no unloading    // JVAL ???
  //   of stminus yet. Dude.
end;

//==============================================================================
//
// ST_InitData
//
//==============================================================================
procedure ST_InitData;
var
  i: integer;
begin
  st_firsttime := true;
  plyr := @players[consoleplayer];

  st_clock := 0;
  st_chatstate := StartChatState;
  st_gamestate := st_firstpersonstate;

  st_statusbaron := true;
  st_oldchat := false;
  st_chat := false;
  st_cursoron := false;

  st_faceindex := 0;
  st_palette := -1;

  st_oldhealth := -1;

  for i := 0 to Ord(NUMWEAPONS) - 1 do
    oldweaponsowned[i] := plyr.weaponowned[i];

  for i := 0 to 2 do
    keyboxes[i] := -1;

  STlib_init;
end;

//==============================================================================
//
// ST_CreateWidgets
//
//==============================================================================
procedure ST_CreateWidgets;
var
  i: integer;
begin
  // ready weapon ammo
  STlib_initNum(
    @w_ready,
    ST_AMMOX,
    ST_AMMOY,
    @tallnum,
    @plyr.ammo[Ord(weaponinfo[Ord(plyr.readyweapon)].ammo)],
    @st_statusbaron,
    ST_AMMOWIDTH);

  // the last weapon type
  w_ready.data := Ord(plyr.readyweapon);

  // health percentages
  STlib_initPercent(
    @w_health,
    ST_HEALTHX,
    ST_HEALTHY,
    @tallnum,
    @plyr.health,
    @st_statusbaron,
    tallpercent);
  STlib_initPercent(
    @w_health2,
    ST_MHEALTHX,
    ST_MHEALTHY,
    @tallnum,
    @plyr.health,
    @st_statusbaron,
    tallpercent);

  // arms background
  STlib_initBinIcon(
    @w_armsbg,
    ST_ARMSBGX,
    ST_ARMSBGY,
    armsbg,
    @st_notdeathmatch,
    @st_statusbaron);

  // weapons owned
  for i := 0 to 5 do
  begin
    STlib_initMultIcon(
      @w_arms[i],
      ST_ARMSX + (i mod 3) * ST_ARMSXSPACE,
      ST_ARMSY + (i div 3) * ST_ARMSYSPACE,
      @arms[i],
      @plyr.weaponowned[i + 1],
      @st_armson);
  end;

  // frags sum
  STlib_initNum(
    @w_frags,
    ST_FRAGSX,
    ST_FRAGSY,
    @tallnum,
    @st_fragscount,
    @st_fragson,
    ST_FRAGSWIDTH);

  // faces
  STlib_initMultIcon(
    @w_faces,
    ST_FACESX,
    ST_FACESY,
    @faces,
    @st_faceindex,
    @st_statusbaron);

  // armor percentage - should be colored later
  STlib_initPercent(
    @w_armor,
    ST_ARMORX,
    ST_ARMORY,
    @tallnum,
    @plyr.armorpoints,
    @st_statusbaron,
    tallpercent);

  // keyboxes 0-2
  STlib_initMultIcon(
    @w_keyboxes[0],
    ST_KEY0X,
    ST_KEY0Y,
    @keys,
    @keyboxes[0],
    @st_statusbaron);

  STlib_initMultIcon(
    @w_keyboxes[1],
    ST_KEY1X,
    ST_KEY1Y,
    @keys,
    @keyboxes[1],
    @st_statusbaron);

  STlib_initMultIcon(
    @w_keyboxes[2],
    ST_KEY2X,
    ST_KEY2Y,
    @keys,
    @keyboxes[2],
    @st_statusbaron);

  // ammo count (all four kinds)
  STlib_initNum(
    @w_ammo[0],
    ST_AMMO0X,
    ST_AMMO0Y,
    @shortnum,
    @plyr.ammo[0],
    @st_statusbaron,
    ST_AMMO0WIDTH);

  STlib_initNum(
    @w_ammo[1],
    ST_AMMO1X,
    ST_AMMO1Y,
    @shortnum,
    @plyr.ammo[1],
    @st_statusbaron,
    ST_AMMO1WIDTH);

  STlib_initNum(
    @w_ammo[2],
    ST_AMMO2X,
    ST_AMMO2Y,
    @shortnum,
    @plyr.ammo[2],
    @st_statusbaron,
    ST_AMMO2WIDTH);

  STlib_initNum(
    @w_ammo[3],
    ST_AMMO3X,
    ST_AMMO3Y,
    @shortnum,
    @plyr.ammo[3],
    @st_statusbaron,
    ST_AMMO3WIDTH);

  // ammo count for small display
  for i := 0 to 3 do
    STlib_initNum(
      @w_ammo2[i],
      ST_MAMMOX,
      ST_MAMMOY,
      @tallnum,
      @plyr.ammo[i],
      @st_statusbaron,
      ST_MAMMOWIDTH);

  // max ammo count (all four kinds)
  STlib_initNum(
    @w_maxammo[0],
    ST_MAXAMMO0X,
    ST_MAXAMMO0Y,
    @shortnum,
    @plyr.maxammo[0],
    @st_statusbaron,
    ST_MAXAMMO0WIDTH);

  STlib_initNum(
    @w_maxammo[1],
    ST_MAXAMMO1X,
    ST_MAXAMMO1Y,
    @shortnum,
    @plyr.maxammo[1],
    @st_statusbaron,
    ST_MAXAMMO1WIDTH);

  STlib_initNum(
    @w_maxammo[2],
    ST_MAXAMMO2X,
    ST_MAXAMMO2Y,
    @shortnum,
    @plyr.maxammo[2],
    @st_statusbaron,
    ST_MAXAMMO2WIDTH);

  STlib_initNum(
    @w_maxammo[3],
    ST_MAXAMMO3X,
    ST_MAXAMMO3Y,
    @shortnum,
    @plyr.maxammo[3],
    @st_statusbaron,
    ST_MAXAMMO3WIDTH);
end;

var
  st_stopped: boolean;

//==============================================================================
//
// ST_Stop
//
//==============================================================================
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

//==============================================================================
//
// ST_Start
//
//==============================================================================
procedure ST_Start;
begin
  if not st_stopped then
    ST_Stop;

  ST_InitData;
  ST_CreateWidgets;
  st_stopped := false;
end;

//==============================================================================
//
// ST_Init
//
//==============================================================================
procedure ST_Init;
begin
////////////////////////////////////////////////////////////////////////////////
  st_msgcounter := 0;
  st_oldhealth := -1;
  st_facecount := 0;
  st_faceindex := 0;

////////////////////////////////////////////////////////////////////////////////
// Now what?
  cheat_mus.sequence := get_cheatseq_string(cheat_mus_seq);
  cheat_mus.p := get_cheatseq_string(0);
  cheat_god.sequence := get_cheatseq_string(cheat_god_seq);
  cheat_god.p := get_cheatseq_string(0);
  cheat_ammo.sequence := get_cheatseq_string(cheat_ammo_seq);
  cheat_ammo.p := get_cheatseq_string(0);
  cheat_ammonokey.sequence := get_cheatseq_string(cheat_ammonokey_seq);
  cheat_ammonokey.p := get_cheatseq_string(0);
  cheat_keys.sequence := get_cheatseq_string(cheat_idkeys_seq);
  cheat_keys.p := get_cheatseq_string(0);
  cheat_noclip.sequence := get_cheatseq_string(cheat_noclip_seq);
  cheat_noclip.p := get_cheatseq_string(0);
  cheat_commercial_noclip.sequence := get_cheatseq_string(cheat_commercial_noclip_seq);
  cheat_commercial_noclip.p := get_cheatseq_string(0);

  cheat_powerup[0].sequence := get_cheatseq_string(cheat_powerup_seq0);
  cheat_powerup[0].p := get_cheatseq_string(0);
  cheat_powerup[1].sequence := get_cheatseq_string(cheat_powerup_seq1);
  cheat_powerup[1].p := get_cheatseq_string(0);
  cheat_powerup[2].sequence := get_cheatseq_string(cheat_powerup_seq2);
  cheat_powerup[2].p := get_cheatseq_string(0);
  cheat_powerup[3].sequence := get_cheatseq_string(cheat_powerup_seq3);
  cheat_powerup[3].p := get_cheatseq_string(0);
  cheat_powerup[4].sequence := get_cheatseq_string(cheat_powerup_seq4);
  cheat_powerup[4].p := get_cheatseq_string(0);
  cheat_powerup[5].sequence := get_cheatseq_string(cheat_powerup_seq5);
  cheat_powerup[5].p := get_cheatseq_string(0);
  cheat_powerup[6].sequence := get_cheatseq_string(cheat_powerup_seq6);
  cheat_powerup[6].p := get_cheatseq_string(0);

  cheat_choppers.sequence := get_cheatseq_string(cheat_choppers_seq);
  cheat_choppers.p := get_cheatseq_string(0);
  cheat_clev.sequence := get_cheatseq_string(cheat_clev_seq);
  cheat_clev.p := get_cheatseq_string(0);
  cheat_mypos.sequence := get_cheatseq_string(cheat_mypos_seq);
  cheat_mypos.p := get_cheatseq_string(0);

  lastcalc := 0;
  oldhealth := -1;

  lastattackdown := -1;
  priority := 0;

  st_palette := 0;

  st_stopped := true;
////////////////////////////////////////////////////////////////////////////////

  ST_LoadData;
  C_AddCmd('god, iddqd', @ST_CmdGod);
  C_AddCmd('massacre', @ST_CmdMassacre);
  C_AddCmd('givefullammo, rambo, idfa', @ST_CmdIDFA);
  C_AddCmd('giveallkeys, idkeys', @ST_CmdIDKEYS);
  C_AddCmd('lowgravity', @ST_CmdLowGravity);
  C_AddCmd('givefullammoandkeys, idkfa', @ST_CmdIDKFA);
  C_AddCmd('iddt', @ST_CmdIDDT);
  C_AddCmd('idspispopd, idclip', @ST_CmdIDNoClip);
  C_AddCmd('idmypos', @ST_CmdIDMyPos);
end;

end.
