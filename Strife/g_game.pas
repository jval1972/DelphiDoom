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
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit g_game;

interface

uses
  d_delphi,
  doomdef,
  m_fixed,
  tables,
  d_event,
  d_player,
  d_ticcmd;

//
// GAME
//

procedure G_DeathMatchSpawnPlayer(playernum: integer);

procedure G_InitNew(skill: skill_t; map: integer);

// Can be called by the startup code or M_Responder.
// A normal game starts at map 1,
// but a warp test can start elsewhere
procedure G_DeferedInitNew(skill:skill_t; map: integer);

procedure G_CmdNewGame(const parm1, parm2: string);

function G_DeferedPlayDemo(const name: string): boolean;

procedure G_CmdPlayDemo(const name: string);

{ Can be called by the startup code or M_Responder, }
{ calls P_SetupLevel or W_EnterWorld. }
procedure G_LoadGame(const name: string);

procedure G_DoLoadGame(const userload: boolean);

procedure G_CmdSaveGame(const sname: string; const description: string);

{ Only called by startup code. }
procedure G_RecordDemo(const name: string);

procedure G_BeginRecording;

procedure G_TimeDemo(const name: string);

function G_CheckDemoStatus: boolean;

procedure G_ExitLevel(dest: integer);

procedure G_Ticker;

function G_Responder(ev: Pevent_t): boolean;

procedure G_ScreenShot;

procedure G_Quit;

// 19/9/2009 JVAL: For drawing demo progress
function G_DemoProgress: fixed_t;

var
  sendpause: boolean;        // send a pause event next tic

  paused: boolean;

//
// controls (have defaults)
//
  key_right: integer;
  key_left: integer;

  key_up: integer;
  key_down: integer;

// JVAL Look Up and Down
  key_lookup: integer;
  key_lookdown: integer;
  key_lookcenter: integer;
// JVAL Look Left and Right
  key_lookright: integer;
  key_lookleft: integer;

  key_strafeleft: integer;
  key_straferight: integer;
  key_fire: integer;
  key_use: integer;
  key_strafe: integer;
  key_speed: integer;

  key_invleft: integer; //  KEY_INS;
  key_invright: integer; // KEY_DEL;

// JVAL Jump
  key_jump: integer;
// JVAL: 20211101 - Crouch
  key_crouch: integer;

// JVAL 20191207 Key bindings for weapon change
  key_weapon0: integer = Ord('1');
  key_weapon1: integer = Ord('2');
  key_weapon2: integer = Ord('3');
  key_weapon3: integer = Ord('4');
  key_weapon4: integer = Ord('5');
  key_weapon5: integer = Ord('6');
  key_weapon6: integer = Ord('7');
  key_weapon7: integer = Ord('8');
  key_weapon8: integer = Ord('9');
  key_weapon9: integer = Ord('0');

  KEY_WEAPONS: array[0..Ord(NUMWEAPONS) - 1] of PInteger;

// Inventory
  key_usehealth: integer; // 'h'
  key_invquery: integer;  // 'q';
  key_mission: integer;   // 'w';
  key_invpop: integer;    // 'z';
  key_invkey: integer;    // 'k';
  key_invhome: integer;   // KEY_HOME;
  key_invend: integer;    // KEY_END;
  key_invuse: integer;    // KEY_ENTER;
  key_invdrop: integer;   // KEY_BACKSPACE;


  usemouse: boolean;
  invertmouseturn: boolean;
  invertmouselook: boolean;
  mousebfire: integer;
  mousebstrafe: integer;
  mousebforward: integer;

  usejoystick: boolean;
  joybfire: integer;
  joybstrafe: integer;
  joybuse: integer;
  joybspeed: integer;
  joybjump: integer;
  joybcrouch: integer;  // JVAL: 20211101 - Crouch
  joyblleft: integer;
  joyblright: integer;

  demoplayback: boolean;
  showdemoplaybackprogress: boolean;
  preparingdemoplayback: boolean = false;

  gamemap: integer;

  deathmatch: integer; // only if started as net death
  netgame: boolean; // only true if packets are broadcast
  playeringame: array[0..MAXPLAYERS - 1] of boolean;

  consoleplayer: integer; // player taking events and displaying
  displayplayer: integer; // view being displayed
  gametic: integer;

  totalkills, totalsecret: integer; // for intermission

  wminfo: wbstartstruct_t; // parms for world map / intermission

  gameskill: skill_t = sk_medium;

  bodyqueslot: integer;

  precache: boolean; // if true, load all graphics at start

  respawnmonsters: boolean;

  viewactive: boolean;

  singledemo: boolean; // quit after playing a demo from cmdline

  demorecording: boolean = false;

  gameaction: gameaction_t;

  usergame: boolean; // ok to save / end game

procedure G_PlayerReborn(player: integer);

procedure G_BuildTiccmd(cmd: Pticcmd_t);

var
  forwardmove: array[0..1] of shortint;
  sidemove: array[0..1] of shortint;
  angleturn: array[0..2] of smallint;

function G_NeedsCompatibilityMode: boolean;

function G_PlayingEngineVersion: byte;

var
  compatibilitymode: boolean = false;
  oldcompatibilitymode: boolean = false;

type
//
// LOAD GAME MENU
//
  load_e = (
    load1,
    load2,
    load3,
    load4,
    load5,
    load6,
    load7,
    load8,
    load_end
  );

var
  autorunmode: boolean = false;
  keepcheatsinplayerreborn: boolean = false;
  allowplayerjumps: boolean = true;
  allowplayercrouch: boolean = true;

procedure G_StartFinale;

procedure G_RiftExitLevel(map, spot: integer; angle: angle_t);

function G_RiftCheat(riftSpotNum: integer): boolean;

function G_WriteSaveName(slot: integer; charname: string): boolean;

procedure G_ReadCurrent(path: string);

var
  sendsave: boolean;         // send a save event next tic

function G_GetSaveName(name: string): string;

const
  SAVEGAMESIZE = $1000000; // Originally $2C000
  SAVESTRINGSIZE = 24;
  SAVEVERSIONSIZE = 16;

const
  NUMKEYS = 256;

var
  gamekeydown: array[0..NUMKEYS - 1] of boolean;
  mousebuttons: PBooleanArray;
  joybuttons: PBooleanArray;

implementation

uses
  deh_main,
  c_cmds,
  z_zone,
  doomstat,
  doomdata,
  d_items,
  am_map,
  d_net,
  d_net_h,
  d_main,
  f_finale,
  info_h,
  info,
  info_rnd,
  m_rnd,
  i_system,
  i_io,
{$IFNDEF OPENGL}
  r_draw,
{$ENDIF}
  e_endoom,
  m_argv,
  m_misc,
  m_menu,
  m_saves,
  p_setup,
  p_saveg,
  p_tick,
  p_local,
  p_mobj_h,
  p_mobj,
  p_inter,
  p_map,
  p_dialog,
  p_levelinfo,
  ps_main,
  hu_stuff,
  st_stuff,
  w_wad,
  s_sound,
// Data.
  dstrings,
  d_englsh,
  sounds,
// SKY handling - still the wrong place.
  r_data,
  r_sky,
  r_defs,
  r_main,
  r_intrpl;

var
  next_weapon: integer;

// Used for prev/next weapon keys.
// STRIFE-TODO: Check this table makes sense.

type
  weapon_order_item_t = record
    weapon: weapontype_t;
    weapon_num: weapontype_t;
  end;

const
  WEAPONORDERSIZE = 11;

var
  weapon_order_table: array[0..WEAPONORDERSIZE - 1] of weapon_order_item_t = (
    (weapon: wp_fist       ; weapon_num: wp_fist),
    (weapon: wp_poisonbow  ; weapon_num: wp_elecbow),
    (weapon: wp_elecbow    ; weapon_num: wp_elecbow),
    (weapon: wp_rifle      ; weapon_num: wp_rifle),
    (weapon: wp_missile    ; weapon_num: wp_missile),
    (weapon: wp_wpgrenade  ; weapon_num: wp_hegrenade),
    (weapon: wp_hegrenade  ; weapon_num: wp_hegrenade),
    (weapon: wp_flame      ; weapon_num: wp_flame),
    (weapon: wp_torpedo    ; weapon_num: wp_mauler),
    (weapon: wp_mauler     ; weapon_num: wp_mauler),
    (weapon: wp_sigil      ; weapon_num: wp_sigil)
  );


procedure G_ReadDemoTiccmd(cmd: Pticcmd_t); forward;
procedure G_WriteDemoTiccmd(cmd: Pticcmd_t); forward;

procedure G_DoReborn(playernum: integer); forward;

procedure G_DoLoadLevel; forward;
procedure G_DoNewGame; forward;
procedure G_DoPlayDemo; forward;
procedure G_DoCompleted; forward;
procedure G_DoWorldDone; forward;
procedure G_DoSaveGame(path: string); forward;

procedure G_FinishedDemoPlayback;
begin
  demoplayback := false;
  // Restore old compatibility mode
  compatibilitymode := oldcompatibilitymode;
end;

var
  sendcmdsave: boolean;      // send a save event next tic (console)

  timingdemo: boolean;       // if true, exit with report on completion
  starttime: integer;        // for comparative timing purposes

  demoname: string;
  netdemo: boolean;
  demobuffer: PByteArray;
  demo_p: PByteArray;
  demoend: PByte;

  consistancy: array[0..MAXPLAYERS - 1] of array[0..BACKUPTICS - 1] of smallint;

  savebuffer: PByteArray;

const
  TURBOTHRESHOLD = $32;

function MAXPLMOVE: fixed_t;
begin
  result := forwardmove[1];
end;

const
  SLOWTURNTICS = 6;

var
  turnheld: integer;

  lookheld: integer;  // JVAL Look UP and DOWN
  lookheld2: integer; // JVAL Look RIGHT and LEFT

  mousearray: array[0..2] of boolean;

// mouse values are used once
  mousex: integer = 0;
  mousey: integer = 0;

  dclicktime: integer;
  dclickstate: boolean;
  dclicks: integer;
  dclicktime2: integer;
  dclickstate2: boolean;
  dclicks2: integer;

// joystick values are repeated
  joyxmove: integer;
  joyymove: integer;
  joyarray: array[0..NUMJOYBUTTONS - 1] of boolean;

  savegameslot: integer;
  savedescription: string;

const
  BODYQUESIZE  = 32;

var
  bodyque: array[0..BODYQUESIZE - 1] of Pmobj_t;

function G_CmdChecksum(cmd: Pticcmd_t): integer;
var
  i: integer;
begin
  result := 0;
  for i := 0 to SizeOf(cmd^) div 4 - 2 do
    result := result + PIntegerArray(cmd)[i];
end;

function G_IsWeaponSelectable(const weapon: weapontype_t): boolean;
var
  player: Pplayer_t;
begin
  player := @players[consoleplayer];

  // Can't select a weapon if we don't own it.

  if not player.weaponowned[Ord(weapon)] then
  begin
    result := false;
    exit;
  end;

  // Can't use registered-only weapons in demo mode:

  if isdemoversion and not weaponinfo[Ord(weapon)].availabledemo then
  begin
    result := false;
    exit;
  end;

  // Special rules for switching to alternate versions of weapons.
  // These must match the weapon-switching rules in P_PlayerThink()

  // haleyjd 20141024: same fix here as in P_PlayerThink for torpedo.

  if (weapon = wp_torpedo) and (player.ammo[Ord(weaponinfo[Ord(wp_torpedo)].ammo)] < 30) then
  begin
    result := false;
    exit;
  end;

  if player.ammo[Ord(weaponinfo[Ord(weapon)].ammo)] = 0 then
  begin
    result := false;
    exit;
  end;

  result := true;
end;

function G_NextWeapon(const direction: integer): integer;
var
  weapon: weapontype_t;
  start_i, i: integer;
begin
  // Find index in the table.

  if players[consoleplayer].pendingweapon = wp_nochange then
    weapon := players[consoleplayer].readyweapon
  else
    weapon := players[consoleplayer].pendingweapon;

  start_i := 0;
  for i := 0 to WEAPONORDERSIZE - 1 do
    if weapon_order_table[i].weapon = weapon then
    begin
      start_i := i;
      break;
    end;

  // Switch weapon.
  i := start_i;
  repeat;
    i := (i + direction + WEAPONORDERSIZE) mod WEAPONORDERSIZE;
  until not (i <> start_i) and (not G_IsWeaponSelectable(weapon_order_table[i].weapon));

  result := Ord(weapon_order_table[i].weapon_num);
end;


//
// G_BuildTiccmd
// Builds a ticcmd from all of the available inputs
// or reads it from the demo buffer.
// If recording a demo, write it out
//
procedure G_BuildTiccmd(cmd: Pticcmd_t);
var
  i: integer;
  strafe: boolean;
  bstrafe: boolean;
  speed: integer;
  tspeed: integer;
  lspeed: integer;  // JVAL Look up and down
  lspeed2: integer; // JVAL look left and right
  _forward: integer;
  side: integer;
  lookupdown: integer;    // JVAL Look up and down
  look16: integer;        // JVAL Smooth Look Up/Down
  lookleftright: integer; // JVAL look left and right
  base: Pticcmd_t;
  imousex: integer;
  imousey: integer;
  player: Pplayer_t;
  cmd_jump, cmd_crouch: byte;
begin
  base := I_BaseTiccmd;    // empty, or external driver

  memcpy(cmd, base, SizeOf(cmd^));

  cmd.consistancy := consistancy[consoleplayer][maketic mod BACKUPTICS];

  player := @players[consoleplayer];

  // villsa [STRIFE] inventory use key
  if gamekeydown[key_invuse] then
  begin
    if player.numinventory > 0 then
    begin
      cmd.buttons2 := cmd.buttons2 or BT2_INVUSE;
      cmd.inventory := player.inventory[player.inventorycursor].sprite;
    end;
  end;

  // villsa [STRIFE] inventory drop key
  if gamekeydown[key_invdrop] then
  begin
    player := @players[consoleplayer];
    if player.numinventory > 0 then
    begin
      cmd.buttons2 := cmd.buttons2 or BT2_INVDROP;
      cmd.inventory := player.inventory[player.inventorycursor].sprite;
    end;
  end;

  // villsa [STRIFE] use medkit
  if gamekeydown[key_usehealth] then
    cmd.buttons2 := cmd.buttons2 or BT2_HEALTH;

  strafe := gamekeydown[key_strafe] or
            (usemouse and mousebuttons[mousebstrafe]) or
            (usejoystick and joybuttons[joybstrafe]);

  if player.health <= 15 then
    speed := 0
  else
  begin
    speed := intval(gamekeydown[key_speed] or joybuttons[joybspeed]);
    if autorunmode then
      speed := 1 - speed;
  end;

  _forward := 0;
  side := 0;
  lookupdown := 0;
  look16 := 0; // JVAL Smooth Look Up/Down
  lookleftright := 0;

  // use two stage accelerative turning
  // on the keyboard and joystick
  if (joyxmove <> 0) or
     (gamekeydown[key_right]) or
     (gamekeydown[key_left]) then
    turnheld := turnheld + ticdup
  else
    turnheld := 0;

  if turnheld < SLOWTURNTICS then
    tspeed := 2             // slow turn
  else
    tspeed := speed;

  if gamekeydown[key_lookdown] or gamekeydown[key_lookup] then
    lookheld := lookheld + ticdup
  else
    lookheld := 0;

  if lookheld < SLOWTURNTICS then
    lspeed := 1
  else
    lspeed := 2;

  if gamekeydown[key_lookleft] or gamekeydown[key_lookright] or
    (usejoystick and (joybuttons[joyblleft] or joybuttons[joyblright])) then
    lookheld2 := lookheld2 + ticdup
  else
    lookheld2 := 0;

  if lookheld2 < SLOWTURNTICS then
    lspeed2 := 1
  else
    lspeed2 := 2;

  // let movement keys cancel each other out
  if strafe then
  begin
    if gamekeydown[key_right] then
      side := side + sidemove[speed];
    if gamekeydown[key_left] then
      side := side - sidemove[speed];
    if joyxmove > 0 then
      side := side + sidemove[speed];
    if joyxmove < 0 then
      side := side - sidemove[speed];
  end
  else
  begin
    if gamekeydown[key_right] then
      cmd.angleturn := cmd.angleturn - angleturn[tspeed];
    if gamekeydown[key_left] then
      cmd.angleturn := cmd.angleturn + angleturn[tspeed];
    if joyxmove > 0 then
      cmd.angleturn := cmd.angleturn - angleturn[tspeed];
    if joyxmove < 0 then
      cmd.angleturn := cmd.angleturn + angleturn[tspeed];
  end;

  if gamekeydown[key_up] then
    _forward := _forward + forwardmove[speed];

  if gamekeydown[key_down] then
    _forward := _forward - forwardmove[speed];

  // JVAL Look up/down/center keys
  if zaxisshift then
  begin
    if gamekeydown[key_lookup] then
      lookupdown := lspeed;

    if gamekeydown[key_lookdown] then
      lookupdown := -lspeed;

    if gamekeydown[key_lookcenter] then
      lookupdown := TOCENTER;

    look16 := 256 * lookupdown; // JVAL Smooth Look Up/Down
  end;

  // JVAL Look right/left/forward keys
  if gamekeydown[key_lookleft] or (usejoystick and joybuttons[joyblleft]) then
    lookleftright := lspeed2;

  if gamekeydown[key_lookright] or (usejoystick and joybuttons[joyblright]) then
    lookleftright := -lspeed2;

  if joyymove < 0 then
    _forward := _forward + forwardmove[speed];

  if joyymove > 0 then
    _forward := _forward - forwardmove[speed];

  if gamekeydown[key_straferight] then
    side := side + sidemove[speed];

  if gamekeydown[key_strafeleft] then
    side := side - sidemove[speed];

  // buttons
  cmd.chatchar := Ord(HU_dequeueChatChar);

  if gamekeydown[key_fire] or
     (usemouse and mousebuttons[mousebfire]) or
     (usejoystick and joybuttons[joybfire]) then
    cmd.buttons := cmd.buttons or BT_ATTACK;

  if gamekeydown[key_use] or (usejoystick and joybuttons[joybuse]) then
  begin
    cmd.buttons := cmd.buttons or BT_USE;
  // clear double clicks if hit use button
    dclicks := 0;
  end;

  // chainsaw overrides
  for i := 0 to Ord(NUMWEAPONS) - 2 do
    if gamekeydown[KEY_WEAPONS[i]^] then
    begin
      cmd.buttons := cmd.buttons or BT_CHANGE;
      cmd.buttons := cmd.buttons or _SHL(i, BT_WEAPONSHIFT);
      break;
    end;

  // mouse
  if (usemouse and mousebuttons[mousebforward]) then
    _forward := _forward + forwardmove[speed];

  // forward double click
  if usemouse and (mousebuttons[mousebforward] <> dclickstate) and (dclicktime > 1) then
  begin
    dclickstate := mousebuttons[mousebforward];
    if dclickstate then
      inc(dclicks);
    if dclicks = 2 then
    begin
      cmd.buttons := cmd.buttons or BT_USE;
      dclicks := 0;
    end
    else
      dclicktime := 0;
  end
  else
  begin
    dclicktime := dclicktime + ticdup;
    if dclicktime > 20 then
    begin
      dclicks := 0;
      dclickstate := false;
    end
  end;

  // strafe double click
  bstrafe := (usemouse and mousebuttons[mousebstrafe]) or
             (usejoystick and joybuttons[joybstrafe]);
  if (bstrafe <> dclickstate2) and (dclicktime2 > 1) then
  begin
    dclickstate2 := bstrafe;
    if bstrafe then
      inc(dclicks2);
    if dclicks2 = 2 then
    begin
      cmd.buttons := cmd.buttons or BT_USE;
      dclicks2 := 0;
    end
    else
      dclicktime2 := 0;
  end
  else
  begin
    dclicktime2 := dclicktime2 + ticdup;
    if dclicktime2 > 20 then
    begin
      dclicks2 := 0;
      dclickstate2 := false;
    end;
  end;

  // JVAL: invert mouse
  if invertmouseturn then
    imousex := -mousex
  else
    imousex := mousex;

  if strafe then
    side := side - imousex * 2
  else
    cmd.angleturn := cmd.angleturn + imousex * $8;

  if invertmouselook then
    imousey := -mousey
  else
    imousey := mousey;

  if usemouse then
  begin
    lookupdown := lookupdown + imousey div 16;
    if imousey < 0 then
    begin
      if lookupdown < -4 then
        lookupdown := -4;
    end
    else if imousey > 0 then
    begin
      if lookupdown > 4 then
        lookupdown := 4;
    end;

    // JVAL Smooth Look Up/Down
    if G_PlayingEngineVersion < VERSION203 then
      look16 := 256 * lookupdown
    else
    begin
      look16 := look16 + imousey * 16;
      if imousey < 0 then
      begin
        if look16 < -4 * 256 then
          look16 := -4 * 256;
      end
      else if imousey > 0 then
      begin
        if look16 > 4 * 256 then
          look16 := 4 * 256;
      end;
    end;
  end;

  // For smooth mouse movement
  mousex := mousex div 4;
  mousey := mousey div 4;

  if _forward > MAXPLMOVE then
    _forward := MAXPLMOVE
  else if _forward < -MAXPLMOVE then
    _forward := -MAXPLMOVE;

  if side > MAXPLMOVE then
    side := MAXPLMOVE
  else if side < -MAXPLMOVE then
    side := -MAXPLMOVE;

  cmd.forwardmove := cmd.forwardmove + _forward;
  cmd.sidemove := cmd.sidemove + side;

  if players[consoleplayer].playerstate = PST_LIVE then
  begin
    if zaxisshift then
    begin
      if lookupdown < 0 then
        lookupdown := lookupdown + 16;
      cmd.lookupdown := lookupdown;

      // JVAL Smooth Look Up/Down
      if look16 < 0 then
        look16 := look16 + 16 * 256;
      cmd.lookupdown16 := look16;
    end;
    if lookleftright < 0 then
      lookleftright := lookleftright + 16;
    cmd.lookleftright:= lookleftright;
    // JVAL
    // allowplayerjumps variable controls if we accept input for jumping
    if allowplayerjumps and (gamekeydown[key_jump] or (usejoystick and joybuttons[joybjump])) then
    begin
      if players[consoleplayer].oldjump <> 0 then
        cmd_jump := 1
      else
        cmd_jump := 2
      end
    else
      cmd_jump := 0;
    players[consoleplayer].oldjump := cmd_jump;
    // JVAL: 20211101 - Crouch
    // allowplayercrouch variable controls if we accept input for crouching
    if cmd_jump = 0 then
    begin
      if allowplayercrouch and (gamekeydown[key_crouch] or (usejoystick and joybuttons[joybcrouch])) then
      begin
        if players[consoleplayer].oldcrouch <> 0 then
          cmd_crouch := 2
        else
          cmd_crouch := 1
        end
      else
        cmd_crouch := 0;
    end
    else
      cmd_crouch := 0;
    players[consoleplayer].oldcrouch := cmd_crouch;

    cmd.jump_crouch :=
      ((cmd_jump shl CMD_JUMP_SHIFT) and CMD_JUMP_MASK) +
      ((cmd_crouch shl CMD_CROUCH_SHIFT) and CMD_CROUCH_MASK);
  end;

  // special buttons
  if sendpause then
  begin
    sendpause := false;
    cmd.buttons := BT_SPECIAL or BTS_PAUSE;
  end;

  if sendsave then
  begin
    sendsave := false;
    cmd.buttons := BT_SPECIAL or BTS_SAVEGAME or _SHL(savegameslot, BTS_SAVESHIFT);
  end;
  if sendcmdsave then
  begin
    sendcmdsave := false;
    cmd.commands := CM_SAVEGAME;
  end;

end;

//
// G_DoLoadLevel
//
procedure G_DoLoadLevel;
var
  i: integer;
begin
  // Set the sky map.
  // First thing, we have a dummy sky texture name,
  //  a flat. The data is in the WAD only because
  //  we look for an actual index, instead of simply
  //  setting one.
  skyflatnum := R_FlatNumForName(SKYFLATNAME);


  if (gamemap >= 9) and (gamemap < 32) then
    skytexture := R_TextureNumForName('skymnt01')
  else
    skytexture := R_TextureNumForName('skymnt02');

  if wipegamestate = Ord(GS_LEVEL) then
    wipegamestate := -1;             // force a wipe

  gamestate := GS_LEVEL;

  for i := 0 to MAXPLAYERS - 1 do
  begin
    if playeringame[i] and ((players[i].playerstate = PST_DEAD) or (players[i].health <= 0)) then
      players[i].playerstate := PST_REBORN;
    ZeroMemory(@players[i].frags, SizeOf(players[i].frags));
  end;

  PS_NewMap;
  P_SetupLevel(gamemap, 0, gameskill);

  // JVAL: Prevent erroneous demos
  for i := 0 to MAXPLAYERS - 1 do
    if playeringame[i] then
      if players[i].mo = nil then
      begin
        I_Warning('G_DoLoadLevel(): Null player actor, is player start missing?'#13#10);
        gamestate := GS_DEMOSCREEN;
        D_StartTitle;
        exit;
      end;

  displayplayer := consoleplayer;    // view the guy you are playing
  starttime := I_GetTime;
  gameaction := ga_nothing;
  Z_CheckHeap;
  // JVAL: Extra debug check
  if debugmode then
    Z_CheckMemory;

  // clear cmd building stuff
  ZeroMemory(@gamekeydown, SizeOf(gamekeydown));
  joyxmove := 0;
  joyymove := 0;
  mousex := 0;
  mousey := 0;
  sendpause := false;
  sendsave := false;
  sendcmdsave := false;
  paused := false;
  ZeroMemory(mousebuttons, SizeOf(mousebuttons));
  ZeroMemory(joybuttons, SizeOf(joybuttons));

  P_DialogLoad; // villsa [STRIFE]
end;

//
// G_Responder
// Get info needed to make ticcmd_ts for the players.
//
function G_Responder(ev: Pevent_t): boolean;
var
  bmask: integer;
  i: integer;
begin
  if gamestate = GS_ENDOOM then
  begin
    result := E_Responder(ev);
    exit;
  end;
  // allow spy mode changes even during the demo
  if (gamestate = GS_LEVEL) and (ev._type = ev_keydown) and
     (ev.data1 = KEY_F12) and (singledemo or (gameskill = sk_baby)) then
  begin
  // spy mode
    repeat
      inc(displayplayer);
      if displayplayer = MAXPLAYERS then
        displayplayer := 0;
    until not ((not playeringame[displayplayer]) and (displayplayer <> consoleplayer));
    result := true;
    exit;
  end;

  // any other key pops up menu if in demos
  if (gameaction = ga_nothing) and not singledemo and
     (demoplayback or (gamestate = GS_DEMOSCREEN)) then
  begin
    if (ev._type = ev_keydown) or
       ((ev._type = ev_mouse) and (ev.data1 <> 0)) or
       ((ev._type = ev_joystick) and (ev.data1 <> 0)) then
    begin
      if devparm and (ev.data1 = Ord('g')) then
        D_PageTicker  // [STRIFE]: wat? o_O
      else
        M_StartControlPanel;
      result := true;
      exit;
    end;
    result := false;
    exit;
  end;

  if gamestate = GS_LEVEL then
  begin
    if HU_Responder(ev) then
    begin
      result := true; // chat ate the event
      exit;
    end;
    if ST_Responder(ev) then
    begin
      result := true; // status window ate it
      exit;
    end;
    if AM_Responder(ev) then
    begin
      result := true; // automap ate it
      exit;
    end;
  end;

  if gamestate = GS_FINALE then
  begin
    if F_Responder(ev) then
    begin
      result := true; // finale ate the event
      exit;
    end;
  end;

  // For smooth mouse movement
  mousex := mousex div 2;
  mousey := mousey div 2;

  case ev._type of
    ev_keydown:
      begin
        if ev.data1 = KEY_PAUSE then
        begin
          sendpause := true;
          result := true;
          exit;
        end;

        if ev.data1 < NUMKEYS then
          gamekeydown[ev.data1] := true;

        result := true; // eat key down events
        exit;
      end;

    ev_keyup:
      begin
        if ev.data1 < NUMKEYS then
          gamekeydown[ev.data1] := false;
        result := false; // always let key up events filter down
        exit;
      end;

    ev_mouse:
      begin
        if usemouse then
        begin
          mousebuttons[0] := ev.data1 and 1 <> 0;
          mousebuttons[1] := ev.data1 and 2 <> 0;
          mousebuttons[2] := ev.data1 and 4 <> 0;
          mousex := mousex + ((ev.data2 * (mouseSensitivity + 5)) div 10) * mouseSensitivityX div 5;
          mousey := mousey + ((ev.data3 * (mouseSensitivity + 5)) div 10) * mouseSensitivityY div 5;
        end
        else
        begin
          mousebuttons[0] := false;
          mousebuttons[1] := false;
          mousebuttons[2] := false;
          mousex := 0;
          mousey := 0;
        end;
        result := true;    // eat events
        exit;
      end;

    ev_joystick:
      begin
        if usejoystick then
        begin
          bmask := 1;
          for i := 0 to NUMJOYBUTTONS - 1 do
          begin
            joybuttons[i] := (ev.data1 and bmask) <> 0;
            bmask := bmask * 2;
          end;
          joyxmove := ev.data2;
          joyymove := ev.data3;
        end
        else
        begin
          for i := 0 to NUMJOYBUTTONS - 1 do
            joybuttons[i] := false;
          joyxmove := 0;
          joyymove := 0;
        end;
        result := true;    // eat events
        exit;
      end;
  end;

  result := false;
end;

//
// G_Ticker
// Make ticcmd_ts for the players.
//
procedure G_Ticker;
var
  i: integer;
  buf: integer;
  cmd: Pticcmd_t;
  msg: string;
  pl: Pplayer_t;
begin
  // do player reborns if needed
  for i := 0 to MAXPLAYERS - 1 do
    if playeringame[i] and (players[i].playerstate = PST_REBORN) then
      G_DoReborn(i);

  // do things to change the game state
  while (gameaction <> ga_nothing) do
  begin
    case gameaction of
      ga_loadlevel:
        G_DoLoadLevel;
      ga_newgame:
        G_DoNewGame;
      ga_loadgame:
        begin
          G_DoLoadGame(true);
          M_SaveMoveHereToMap;
          M_ReadMisObj;
          M_ReadWorldVars;
        end;
      ga_savegame:
        begin
          M_SaveMoveMapToHere;
          M_SaveMisObj(savepath);
          M_SaveWorldVars(savepath);
          M_SaveSaveScreenShot(savepath);
          G_DoSaveGame(savepath);
        end;
      ga_playdemo:
        G_DoPlayDemo;
      ga_completed:
        G_DoCompleted;
      ga_victory:
        F_StartFinale;
      ga_worlddone:
        G_DoWorldDone;
      ga_screenshot:
        begin
          M_ScreenShot;
          gameaction := ga_nothing;
        end;
    end;
  end;

  // get commands, check consistancy,
  // and build new consistancy check
  buf := (gametic div ticdup) mod BACKUPTICS;

  for i := 0 to MAXPLAYERS - 1 do
  begin
    if playeringame[i] then
    begin
      pl := @players[i];
      cmd := @pl.cmd;

      memcpy(cmd, @netcmds[i][buf], SizeOf(ticcmd_t));

      if demoplayback then
        G_ReadDemoTiccmd(cmd);

      if demorecording then
        G_WriteDemoTiccmd(cmd);

      // check for turbo cheats
      if (cmd.forwardmove > TURBOTHRESHOLD) and
         ((gametic and 31) = 0) and
         (((_SHR(gametic, 5)) and 3) = i) then
      begin
        sprintf(msg, '%s is turbo!', [player_names[i]]);
        players[consoleplayer]._message := msg;
      end;

      if netgame and not netdemo and (gametic mod ticdup = 0) then
      begin
        if (gametic > BACKUPTICS) and
           (consistancy[i][buf] <> cmd.consistancy) then
          I_Error('G_Ticker(): consistency failure (%d should be %d)',
            [cmd.consistancy, consistancy[i][buf]]);

        if pl.mo <> nil then
          consistancy[i][buf] := pl.mo.x
        else
          consistancy[i][buf] := rndindex;
      end;
    end;
  end;

  // check for special buttons
  for i := 0 to MAXPLAYERS - 1 do
  begin
    if playeringame[i] then
    begin
      pl := @players[i];
      if pl.cmd.commands and CM_SAVEGAME <> 0 then
      begin
        savegameslot := Ord(load_end);
        gameaction := ga_savegame;
      end;
      if pl.cmd.buttons and BT_SPECIAL <> 0 then
      begin
        case pl.cmd.buttons and BT_SPECIALMASK of
          BTS_PAUSE:
            begin
              paused := not paused;
              if paused then
                S_PauseSound
              else
                S_ResumeSound;
            end;
          BTS_SAVEGAME:
            begin
              if savedescription = '' then
                savedescription := 'NET GAME';
              savegameslot :=
                _SHR((pl.cmd.buttons and BTS_SAVEMASK), BTS_SAVESHIFT);
              gameaction := ga_savegame;
            end;
        end;
      end;
    end;
  end;

  // do main actions
  case gamestate of
    GS_LEVEL:
      begin
        P_Ticker;
        ST_Ticker;
        AM_Ticker;
        HU_Ticker;
      end;
    GS_FINALE:
      begin
        F_Ticker;
      end;
    GS_DEMOSCREEN:
      begin
        D_PageTicker;
      end;
  end;
end;

//
// G_PlayerReborn
// Called after a player dies
// almost everything is cleared and initialized
//
procedure G_PlayerReborn(player: integer);
var
  p: Pplayer_t;
  i: integer;
  frags: array[0..MAXPLAYERS - 1] of integer;
  killcount: integer;
  allegiance: integer;
  cheats: integer;
begin
  p := @players[player];
  memcpy(@frags, @p.frags, SizeOf(frags));
  killcount := p.killcount;
  allegiance := p.allegiance;

  // JVAL: added option to keep cheats
  if keepcheatsinplayerreborn and not preparingdemoplayback then
  begin
    cheats := p.cheats;
    ZeroMemory(p, SizeOf(player_t));
    p.cheats := cheats;
  end
  else
    ZeroMemory(p, SizeOf(player_t));

  memcpy(@p.frags, @frags, SizeOf(players[player].frags));
  p.killcount := killcount;
  p.allegiance := allegiance;

  p.usedown := true;
  p.attackdown := true;  // don't do anything immediately
  p.inventorydown := true;
  p.playerstate := PST_LIVE;
  p.health := mobjinfo[Ord(MT_PLAYER)].spawnhealth;
  p.readyweapon := wp_fist;
  p.pendingweapon := wp_fist;
  p.weaponowned[Ord(wp_fist)] := true;
  p.cheats := CF_AUTOHEALTH;
  p.centerview := true;

  for i := 0 to Ord(NUMAMMO) - 1 do
    p.maxammo[i] := maxammo[i];

  // [STRIFE] clear inventory
  for i := 0 to NUMINVENTORY - 1 do
    p.inventory[i]._type := nummobjtypes;

  // villsa [STRIFE]: Default objective
  mission_objective := DEH_GetString('Find help');
end;

//
// G_CheckSpot
// Returns false if the player cannot be respawned
// at the given mapthing_t spot
// because something is occupying it
//
function G_CheckSpot(playernum: integer; mthing: Pmapthing_t): boolean;
var
  x: fixed_t;
  y: fixed_t;
  z: fixed_t; // JVAL: 3d floor
  ss: Psubsector_t;
  an: angle_t; // JVAL was u long
  mo: Pmobj_t;
  i: integer;
begin
  x := mthing.x * FRACUNIT;
  y := mthing.y * FRACUNIT;

  if players[playernum].mo = nil then
  begin
    // first spawn of level, before corpses
    for i := 0 to playernum - 1 do
      if (players[i].mo.x = x) and
         (players[i].mo.y = y) then
      begin
        result := false;
        exit;
      end;
    result := true;
    exit;
  end;

  // JVAL: 3d floors
  ss := R_PointInSubsector(x, y);
  z := ss.sector.floorheight;
  if ss.sector.midsec >= 0 then
    if players[playernum].mo.spawnpoint.options and MTF_ONMIDSECTOR <> 0 then
      z := sectors[ss.sector.midsec].ceilingheight;

  if not P_CheckPosition(players[playernum].mo, x, y) then
  begin
    result := false;
    exit;
  end;

  // flush an old corpse if needed
  if not G_NeedsCompatibilityMode then
  begin
    if bodyqueslot >= BODYQUESIZE then
      P_RemoveMobj(bodyque[bodyqueslot mod BODYQUESIZE]);
    bodyque[bodyqueslot mod BODYQUESIZE] := players[playernum].mo;
    inc(bodyqueslot);
  end;

  // spawn a teleport fog
  {$IFDEF FPC}
  an := _SHRW(ANG45 * (mthing.angle div 45), ANGLETOFINESHIFT);
  {$ELSE}
  an := (ANG45 * (mthing.angle div 45)) shr ANGLETOFINESHIFT;
  {$ENDIF}

  mo := P_SpawnMobj(x + 20 * finecosine[an], y + 20 * finesine[an],
          z, Ord(MT_TFOG));

  if players[consoleplayer].viewz <> 1 then
    S_StartSound(mo, Ord(sfx_telept));  // don't start sound on first frame

  result := true;
end;

//
// G_LoadPath
//
// haleyjd 20101003: [STRIFE] New function
// Sets loadpath based on the map and "savepathtemp"
//
procedure G_LoadPath(const map: integer);
var
  mapbuf: string;
begin
  mapbuf := itoa(map);

  // haleyjd: free if already set, and use M_SafeFilePath
  loadpath := M_SafeFilePath(savepathtemp, mapbuf);
end;

//
// G_DeathMatchSpawnPlayer
// Spawns a player at one of the random death match spots
// called at level load and each death
//
procedure G_DeathMatchSpawnPlayer(playernum: integer);
var
  i, j: integer;
  selections: integer;
begin
  selections := deathmatch_p; // JVAL - deathmatchstarts;
  if selections < 4 then
    I_Error('G_DeathMatchSpawnPlayer(): Only %d deathmatch spots, at least 4 required', [selections]);

  for j := 0 to 19 do
  begin
    i := P_Random mod selections;
    if G_CheckSpot(playernum, @deathmatchstarts[i]) then
    begin
      deathmatchstarts[i]._type := playernum + 1;
      P_SpawnPlayer(@deathmatchstarts[i]);
      exit;
    end;
  end;

  // no good spot, so the player will probably get stuck
  P_SpawnPlayer(@playerstarts[playernum]);
end;

//
// G_DoReborn
//
procedure G_DoReborn(playernum: integer);
var
  i: integer;
begin
  if not netgame then
  begin
    // reload the level from scratch
    G_LoadPath(gamemap);
    gameaction := ga_loadlevel;
  end
  else
  begin
    // respawn at the start

    // first dissasociate the corpse
    if players[playernum].mo <> nil then
      players[playernum].mo.player := nil;

    // spawn at random spot if in death match
    if deathmatch <> 0 then
    begin
      G_DeathMatchSpawnPlayer(playernum);
      exit;
    end;

    if G_CheckSpot(playernum, @playerstarts[playernum]) then
    begin
      P_SpawnPlayer(@playerstarts[playernum]);
      exit;
    end;

    // try to spawn at one of the other players spots
    for i := 0 to MAXPLAYERS - 1 do
    begin
      if G_CheckSpot (playernum, @playerstarts[i]) then
      begin
        playerstarts[i]._type := playernum + 1; // fake as other player
        P_SpawnPlayer(@playerstarts[i]);
        playerstarts[i]._type := i + 1; // restore
        exit;
      end;
      // he's going to be inside something.  Too bad.
    end;
    P_SpawnPlayer(@playerstarts[playernum]);
  end;
end;

procedure G_ScreenShot;
begin
  gameaction := ga_screenshot;
end;

//
// G_DoCompleted
//

// haleyjd 08/24/10: [STRIFE] New variables
var
  destmap: integer;   // current destination map when exiting
  riftdest: integer;  // destination spot for player
  riftangle: angle_t; // player angle saved during exit

//
// G_RiftExitLevel
//
// haleyjd 20100824: [STRIFE] New function
// * Called from some exit linedefs to exit to a specific riftspot in the
//   given destination map.
//
procedure G_RiftExitLevel(map, spot: integer; angle: angle_t);
begin
  gameaction := ga_completed;

  // special handling for post-Sigil map changes
  if not isdemoversion then
    if players[0].weaponowned[Ord(wp_sigil)] then
    begin
      if map = 3 then // Front Base -> Abandoned Front Base
        map := 30
      else if map = 7 then // Castle -> New Front Base
        map := 10;
    end;

  // no rifting in deathmatch games
  if deathmatch <> 0 then
    spot := 0;

  riftangle := angle;
  riftdest  := spot;
  destmap   := map;
end;

//
// G_Exit2
//
// haleyjd 20101003: [STRIFE] New function.
// No xrefs to this, doesn't seem to be used. Could have gotten inlined
// somewhere but I haven't seen it.
//
procedure G_Exit2(dest: integer; angle: angle_t);
begin
  riftdest := dest;
  gameaction := ga_completed;
  riftangle := angle;
  destmap := gamemap;
end;


procedure G_ExitLevel(dest: integer);
begin
  if dest = 0 then
    dest := gamemap + 1;
  destmap := dest;
  riftdest := 0;
  gameaction := ga_completed;
end;

procedure G_StartFinale;
begin
  gameaction := ga_victory;
end;

procedure G_DoCompleted;
var
  i: integer;
begin
  gameaction := ga_nothing;

  // deal with powerup states
  for i := 0 to MAXPLAYERS - 1 do
  begin
    if playeringame[i] then
    begin
      // [STRIFE] restore pw_allmap power from mapstate cache
      if destmap < 40 then
      begin
        if players[i].mapstate[destmap] then
          players[i].powers[Ord(pw_allmap)] := 1
        else
          players[i].powers[Ord(pw_allmap)] := 0;
      end;

      // Shadowarmor doesn't persist between maps in netgames
      if netgame then
        players[i].powers[Ord(pw_invisibility)] := 0;
    end;
  end;

  stonecold := false;  // villsa [STRIFE]

  if amstate <> am_inactive then
  begin
    amstate := am_inactive;
    AM_Stop;
  end;

  // [STRIFE] HUB SAVE
  if deathmatch = 0 then
    G_DoSaveGame(savepathtemp);

  gameaction := ga_worlddone;
  viewactive := false;

end;

//
// G_RiftPlayer
//
// haleyjd 20100824: [STRIFE] New function
// Teleports the player to the appropriate rift spot.
//
procedure G_RiftPlayer;
begin
  if riftdest <> 0 then
  begin
    P_TeleportMove(players[0].mo,
                   riftSpots[riftdest - 1].x * FRACUNIT,
                   riftSpots[riftdest - 1].y * FRACUNIT);
    players[0].mo.angle  := riftangle;
    players[0].mo.health := players[0].health;
  end;
end;

//
// G_RiftCheat
//
// haleyjd 20100824: [STRIFE] New function
// Called from the cheat code to jump to a rift spot.
//
function G_RiftCheat(riftSpotNum: integer): boolean;
begin
  result := P_TeleportMove(players[0].mo,
                           riftSpots[riftSpotNum - 1].x * FRACUNIT,
                           riftSpots[riftSpotNum - 1].y * FRACUNIT);
end;


procedure G_DoWorldDone;
var
  temp_leveltime: integer;
  temp_shadow, temp_mvis: boolean;
begin
  temp_leveltime := leveltime;
  temp_shadow := false;
  temp_mvis   := false;

  gamestate := GS_LEVEL;
  gamemap := destmap;

  // [STRIFE] HUB LOAD
  G_LoadPath(destmap);
  if deathmatch = 0 then
  begin
    // Remember Shadowarmor across hub loads
    if players[0].mo.flags and MF_SHADOW <> 0 then
      temp_shadow := true;
    if players[0].mo.flags and MF_MVIS <> 0 then
      temp_mvis := true;
  end;
  G_DoLoadGame(false);

  // [STRIFE] leveltime carries over between maps
  leveltime := temp_leveltime;

  if deathmatch = 0 then
  begin
    // [STRIFE]: transfer saved powerups
    players[0].mo.flags := players[0].mo.flags and not (MF_SHADOW or MF_MVIS);
    if temp_shadow then
      players[0].mo.flags := players[0].mo.flags or MF_SHADOW;
    if temp_mvis then
      players[0].mo.flags := players[0].mo.flags or MF_MVIS;

    // [STRIFE] HUB SAVE
    G_RiftPlayer();
    G_DoSaveGame(savepathtemp);
    M_SaveMisObj(savepathtemp);
    M_SaveWorldVars(savepathtemp);
  end;

  gameaction := ga_nothing;
  viewactive := true;
end;

//
// G_ReadCurrent
//
// haleyjd 20101003: [STRIFE] New function.
// Reads the "CURRENT" file from the given path and then sets it to
// gamemap.
//
procedure G_ReadCurrent(path: string);
var
  temppath: string;
  buffer: PByteArray;
begin
  temppath := M_SafeFilePath(path, 'current');

  if M_ReadFile(temppath, pointer(buffer)) <= 0 then
    gameaction := ga_newgame
  else
  begin
    // haleyjd 20110211: do endian-correct read
    gamemap := buffer[0];
    gameaction := ga_loadgame;
    Z_Free(buffer);
  end;

  G_LoadPath(gamemap);
end;

function G_GetSaveName(name: string): string;
var
  i: integer;
begin
  result := '';
  for i := 0 to Ord(load_end) - 1 do
  begin
    if name = itoa(i) then
    begin
      sprintf(result, SAVEGAMENAME + '%s.SSG', [name]);
      exit;
    end;
  end;

  if strupper(Copy(name, length(name) - 3, 4)) <> '.SSG' then
    result := name + '.SSG'
  else
    result := name;
end;

//
// G_InitFromSavegame
// Can be called by the startup code or the menu task.
//
var
  savename: string;

procedure G_LoadGame(const name: string);
begin
  savename := name;
  gameaction := ga_loadgame;
end;

const
  VERSIONSIZE = 11;

procedure G_DoLoadGame(const userload: boolean);
var
  len: integer;
  i, j: integer;
  a, b, c: integer;
  vcheck: string;
  vsaved: string;
  savedleveltime: integer;
begin
  gameaction := ga_nothing;

  if not fexists(loadpath) then
  begin
    G_DoLoadLevel;
    exit;
  end;

  savegameversion := VERSION; // Assume current version
  savegameversionhack := 0;

  len := M_ReadFile(loadpath, pointer(savebuffer));
  save_p := PByteArray(integer(savebuffer) + SAVESTRINGSIZE);

  // skip the description field
  vcheck := '';
  sprintf(vcheck, 'version %d', [VERSION]);

  if len < Length(vcheck) then
  begin
    I_Warning('G_DoLoadGame(): Saved game is from an unsupported version.'#13#10);
    Z_Free(savebuffer);
    exit; // bad version // by JVAL extra checking
  end;

  for i := 0 to Length(vcheck) - 1 do
    if save_p[i] <> Ord(vcheck[i + 1]) then
    begin
      vsaved := '';
      for j := 0 to Length(vcheck) - 1 do
        vsaved := vsaved + Chr(save_p[j]);
      // JVAL
      // Check for compatible game versions here
      if vsaved = 'version 120' then
        savegameversion := VERSION120
      else if vsaved = 'version 121' then
        savegameversion := VERSION121
      else if vsaved = 'version 122' then
        savegameversion := VERSION122
      else if vsaved = 'version 203' then
        savegameversion := VERSION203
      else if vsaved = 'version 204' then
        savegameversion := VERSION204
      else if vsaved = 'version 205' then
        savegameversion := VERSION205
      else if vsaved = 'version 206' then
        savegameversion := VERSION206
      else if vsaved = 'version 001' then
      begin
        savegameversion := VERSION206;
        savegameversionhack := 1;
      end
      else
      begin
        I_Warning('G_DoLoadGame(): Saved game is from an unsupported version: %s!'#13#10, [vsaved]);
        Z_Free(savebuffer);
        exit; // bad version
      end;
      break;
    end;

  save_p := PByteArray(integer(save_p) + VERSIONSIZE);

  gameskill := skill_t(save_p[0]);
  save_p := PByteArray(integer(save_p) + 1);

  gamemap := save_p[0];
  save_p := PByteArray(integer(save_p) + 1);

  for i := 0 to MAXPLAYERS - 1 do
  begin
    playeringame[i] := save_p[0] <> 0;
    save_p := PByteArray(integer(save_p) + 1);
  end;

  // get the times
  a := save_p[0];
  save_p := PByteArray(integer(save_p) + 1);

  b := save_p[0];
  save_p := PByteArray(integer(save_p) + 1);

  c := save_p[0];
  save_p := PByteArray(integer(save_p) + 1);

  savedleveltime := _SHL(a, 16) + _SHL(b, 8) + c;

  // load a base level

  // STRIFE-TODO: ????
  if userload then
    G_InitNew(gameskill, gamemap)
  else
    G_DoLoadLevel();

  leveltime := savedleveltime;

  // dearchive all the modifications
  P_UnArchivePlayers(userload);
  P_UnArchiveWorld;
  P_UnArchiveThinkers;
  P_UnArchiveSpecials;
  P_UnArchiveMapVariables;
  P_UnArchivePSMapScript;
  P_UnArchiveOverlay;

  if save_p[0] <> $1d then
    I_Error('G_DoLoadGame(): Bad savegame');

  // done
  Z_Free(savebuffer);

  if setsizeneeded then
    R_ExecuteSetViewSize;

  P_LevelInfoChangeMusic;

  // draw the pattern into the back screen
{$IFNDEF OPENGL}
  R_FillBackScreen;
{$ENDIF}
end;

//
// G_WriteSaveName
//
// haleyjd 20101003: [STRIFE] New function
//
// Writes the character name to the NAME file.
//
function G_WriteSaveName(slot: integer; charname: string): boolean;
var
  tmpname: string;
  buf: array[0..31] of char;
  i: integer;
begin
  savegameslot := slot;

  // haleyjd: as above.
  savepath := M_SafeFilePath(M_SaveFileName(''), M_MakeStrifeSaveDir(savegameslot, ''));

  // haleyjd: memset full character_name for safety
  character_name := charname;

  // haleyjd: use M_SafeFilePath
  tmpname := M_SafeFilePath(savepathtemp, 'name');

  for i := 1 to length(character_name) do
  begin
    if i > 32 then
      break;
    buf[i - 1] := character_name[i];
  end;

  for i := length(character_name) to 31 do
    buf[i] := #0;

  // Write the "name" file under the directory
  result := M_WriteFile(tmpname, @buf, 32);

end;

procedure G_DoSaveGame(path: string);
var
  name2: string;
  description: string;
  len: integer;
  i: integer;
  maxsize: integer;
  current_path: string;
  savegame_file: string;
  gamemapbytes: array[0..3] of byte;
  gamemapstr: string;
begin
  gamemapstr := itoa(gamemap);
  savegame_file := M_SafeFilePath(path, gamemapstr);

  // [STRIFE] write the "current" file, which tells which hub map
  //   the save slot is currently on.
  current_path := M_SafeFilePath(path, 'current');
  // haleyjd: endian-agnostic IO
  gamemapbytes[0] := gamemap;
  gamemapbytes[1] := 0;
  gamemapbytes[2] := 0;
  gamemapbytes[3] := 0;
  M_WriteFile(current_path, @gamemapbytes, 4);

  description := savedescription;

  maxsize := SAVEGAMESIZE + PS_MapScriptSaveSize;
  repeat
    savebuffer := Z_Malloc2(maxsize, PU_STATIC, nil);
    if savebuffer = nil then
      maxsize := maxsize * 3 div 4;
  until savebuffer <> nil;

  save_p := savebuffer;

  while length(description) < SAVESTRINGSIZE do
    description := description + ' ';

  memcpy(save_p, @description[1], SAVESTRINGSIZE);

  save_p := PByteArray(integer(save_p) + SAVESTRINGSIZE);
  name2 := '';

  savegameversion := VERSION;
  sprintf(name2, 'version %d', [VERSION]);
  memcpy(save_p, @name2[1], VERSIONSIZE);
  save_p := PByteArray(integer(save_p) + VERSIONSIZE);

  save_p[0] := Ord(gameskill);
  save_p := PByteArray(integer(save_p) + 1);

  save_p[0] := gamemap;
  save_p := PByteArray(integer(save_p) + 1);

  for i := 0 to MAXPLAYERS - 1 do
  begin
    save_p[0] := intval(playeringame[i]);
    save_p := PByteArray(integer(save_p) + 1);
  end;

  save_p[0] := _SHR(leveltime, 16);
  save_p := PByteArray(integer(save_p) + 1);

  save_p[0] := _SHR(leveltime, 8);
  save_p := PByteArray(integer(save_p) + 1);

  save_p[0] := leveltime;
  save_p := PByteArray(integer(save_p) + 1);

  len := integer(save_p) - integer(savebuffer);
  M_WriteFile(savegame_file, savebuffer, len);
  save_p := savebuffer;

  P_ArchivePlayers;

  len := integer(save_p) - integer(savebuffer);
  M_AppendFile(savegame_file, savebuffer, len);
  save_p := savebuffer;

  P_ArchiveWorld;

  len := integer(save_p) - integer(savebuffer);
  M_AppendFile(savegame_file, savebuffer, len);
  save_p := savebuffer;

  P_ArchiveThinkers;

  len := integer(save_p) - integer(savebuffer);
  M_AppendFile(savegame_file, savebuffer, len);
  save_p := savebuffer;

  P_ArchiveSpecials;

  len := integer(save_p) - integer(savebuffer);
  M_AppendFile(savegame_file, savebuffer, len);
  save_p := savebuffer;

  P_ArchiveMapVariables;

  len := integer(save_p) - integer(savebuffer);
  M_AppendFile(savegame_file, savebuffer, len);
  save_p := savebuffer;

  P_ArchivePSMapScript;

  len := integer(save_p) - integer(savebuffer);
  M_AppendFile(savegame_file, savebuffer, len);
  save_p := savebuffer;

  P_ArchiveOverlay;

  save_p[0] := $1d; // consistancy marker

  len := integer(save_p) - integer(savebuffer) + 1;
  if len > maxsize then
    I_Error('G_DoSaveGame(): Savegame buffer overrun');
  M_AppendFile(savegame_file, savebuffer, len);
  Z_Free(savebuffer);
  gameaction := ga_nothing;
  savedescription := '';

  // [STRIFE]: custom message logic
  if path <> savepath then
    players[consoleplayer]._message := GGSAVED;

  // draw the pattern into the back screen
{$IFNDEF OPENGL}
  R_FillBackScreen;
{$ENDIF}
end;

procedure G_CmdSaveGame(const sname: string; const description: string);
begin
  if not usergame or (gamestate <> GS_LEVEL) then
  begin
    printf('You can''t save if you aren''t playing!'#13#10);
    exit;
  end;

  if sname <> '' then
  begin
    savename := sname;
    if description <> '' then
      savedescription := description
    else
      savedescription := savename;
    sendcmdsave := true;
  end
  else
    printf('Usage: savegame [slot(integer)] [description(string)]');
end;

//
// G_InitNew
// Can be called by the startup code or the menu task,
// consoleplayer, displayplayer, playeringame[] should be set.
//
var
  d_skill: skill_t;
  d_map: integer;

procedure G_DeferedInitNew(skill: skill_t; map: integer);
begin
  d_skill := skill;
  d_map := map;
  gameaction := ga_newgame;
end;

procedure G_CmdNewGame(const parm1, parm2: string);
var
  map: integer;
  mapname: string;
begin
  if parm1 = '' then
  begin
    printf('Please specify the level to play'#13#10);
    exit;
  end;

  mapname := strupper(parm1);

  map := atoi(parm1);
  if (mapname[1] = 'M') then
    if length(mapname) = 5 then
      if (mapname[2] = 'A') and
         (mapname[3] = 'P') then
        map := atoi(mapname[4] + mapname[5]);

  if W_CheckNumForName(P_GetMapName(map)) > -1 then
  begin
    players[consoleplayer]._message := STSTR_CLEV;
    G_DeferedInitNew(gameskill, map);
    C_ExecuteCmd('closeconsole', '1');
  end
  else
    I_Warning('G_CmdNewGame(): Can not load map.'#13#10);
end;

procedure G_DoNewGame;
var
  i: integer;
begin
  G_FinishedDemoPlayback; // JVAL: remove???
  netdemo := false;
  netgame := false;
  deathmatch := 0;
  for i := 1 to MAXPLAYERS - 1 do
    playeringame[i] := false;
  respawnparm := false;
  fastparm := false;
  stonecold := false;      // villsa [STRIFE]
  nomonsters := false;
  consoleplayer := 0;
  G_InitNew(d_skill, d_map);
  gameaction := ga_nothing;
end;

//
// G_InitNew
//
// haleyjd 20100824: [STRIFE]:
// * Added riftdest initialization
// * Removed episode parameter
//
procedure G_InitNew(skill: skill_t; map: integer);
var
  i: integer;
  levelinf: Plevelinfo_t;
begin
  if paused then
  begin
    paused := false;
    S_ResumeSound;
  end;

  if skill > sk_nightmare then
    skill := sk_nightmare;

  if gamemode = shareware then
  begin
    if not (map in [32, 33, 34]) then
      map := 32;
  end
  else
  begin
    if map > 34 then
      map := 1;
  end;

  if map < 1 then
    map := 1;

  levelinf := P_GetLevelInfo(P_GetMapName(map));
  levelinf.musname := stringtochar8('');
  levelinf.skyflat := stringtochar8('');

  R_ResetInterpolationBuffer;

  M_ClearRandom;
  PS_NewWorld;

  if (skill = sk_nightmare) or respawnparm then
    respawnmonsters := true
  else
    respawnmonsters := false;

  // JVAL: 20210207 -> Support for fast speed mobj info field
  if fastparm or ((skill = sk_nightmare) and (gameskill <> sk_nightmare)) then
  begin
    for i := 0 to nummobjtypes - 1 do
      if mobjinfo[i].fastspeed <> 0 then
        mobjinfo[i].speed := mobjinfo[i].fastspeed;
  end
  else if (skill <> sk_nightmare) and (gameskill = sk_nightmare) then
  begin
    for i := 0 to nummobjtypes - 1 do
      if mobjinfo[i].normalspeed <> 0 then
        mobjinfo[i].speed := mobjinfo[i].normalspeed;
  end;

  // [STRIFE] Strife skill level mobjinfo/states tweaking
  if skill = sk_baby then
  begin
    // Setting to Baby skill... make things easier.

    // Acolytes walk, attack, and feel pain slower
    for i := Ord(S_AGRD_13) to Ord(S_AGRD_23) do
      states[i].tics := originalstates[i].tics * 2;

    // Reavers attack slower
    for i := Ord(S_ROB1_10) to Ord(S_ROB1_15) do
      states[i].tics := originalstates[i].tics * 2;

    // Turrets attack slower
    for i := Ord(S_TURT_02) to Ord(S_TURT_03) do
      states[i].tics := originalstates[i].tics* 2;

    // Crusaders attack and feel pain slower
    for i := Ord(S_ROB2_09) to Ord(S_ROB2_19) do
      states[i].tics := originalstates[i].tics * 2;

    // Stalkers think, walk, and attack slower
    for i := Ord(S_SPID_03) to Ord(S_SPID_10) do
      states[i].tics := originalstates[i].tics * 2;

    // The Bishop's homing missiles are slower
    mobjinfo[Ord(MT_SEEKMISSILE)].speed := originalmobjinfo[Ord(MT_SEEKMISSILE)].speed div 2;
  end
  else
  begin
    // Setting a higher skill when previously on baby... make things normal

    // Acolytes
    for i := Ord(S_AGRD_13) to Ord(S_AGRD_23) do
      states[i].tics := originalstates[i].tics;

    // Reavers
    for i := Ord(S_ROB1_10) to Ord(S_ROB1_15) do
      states[i].tics := originalstates[i].tics;

    // Turrets
    for i := Ord(S_TURT_02) to Ord(S_TURT_03) do
      states[i].tics := originalstates[i].tics;

    // Crusaders
    for i := Ord(S_ROB2_09) to Ord(S_ROB2_19) do
      states[i].tics := originalstates[i].tics;

    // Stalkers
    for i := Ord(S_SPID_03) to Ord(S_SPID_10) do
      states[i].tics := originalstates[i].tics;

    // The Bishop's homing missiles
     mobjinfo[Ord(MT_SEEKMISSILE)].speed := originalmobjinfo[Ord(MT_SEEKMISSILE)].speed;
  end;

  if fastparm or (skill = sk_nightmare) then
  begin
    // BLOODBATH! Make some things super-aggressive.

    // Acolytes walk, attack, and feel pain twice as fast
    // (This makes just getting out of the first room almost impossible)
    for i := Ord(S_AGRD_13) to Ord(S_AGRD_23) do
      states[i].tics := originalstates[i].tics div 2;

    // Bishop's homing missiles get FASTER
    mobjinfo[Ord(MT_SEEKMISSILE)].speed := originalmobjinfo[Ord(MT_SEEKMISSILE)].speed * 2;
  end;

  // force players to be initialized upon first level load
  for i := 0 to MAXPLAYERS - 1 do
    players[i].playerstate := PST_REBORN;

  usergame := true;  // will be set false if a demo
  paused := false;
  G_FinishedDemoPlayback;
  amstate := am_inactive;
  viewactive := true;
  gamemap := map;
  gameskill := skill;
  riftdest := 0; // haleyjd 08/24/10: [STRIFE] init riftdest to zero on new game

  viewactive := true;

  // [STRIFE] HUBS
  G_LoadPath(gamemap);

  G_DoLoadLevel;
end;

//
// DEMO PLAYBACK
//
const
  DEMOMARKER = $80;

var
  compatibility_done: boolean;

//
// Old game versions compatibility
//
procedure G_SafeCompatibilityDemo;
var
  i: integer;
begin
  if compatibility_done then
    exit;

  if players[consoleplayer].playerstate = PST_DEAD then
  begin
    compatibility_done := true;
    i := 4;
    while (integer(@demo_p[i]) < integer(demoend) - 4) and (demo_p[i] <> DEMOMARKER) do
      inc(i, 4);
    // JVAL: Leave maximum 4 seconds after player death
    if i > 4 * TICRATE * 4 then
    begin
      i := 4 * TICRATE * 4;
      demo_p[i] := DEMOMARKER;
    end;
    ZeroMemory(demo_p, i);
  end;
end;

var
  demoversion: byte;

procedure G_ReadDemoTiccmd(cmd: Pticcmd_t);
begin
  if demo_p[0] = DEMOMARKER then
  begin
    // end of demo data stream
    G_CheckDemoStatus;
    exit;
  end;

  G_SafeCompatibilityDemo;

  cmd.forwardmove := shortint(demo_p[0]);
  demo_p := @demo_p[1];

  cmd.sidemove := shortint(demo_p[0]);
  demo_p := @demo_p[1];

  cmd.angleturn := PSmallInt(demo_p)^;
  demo_p := @demo_p[2];

  cmd.buttons := demo_p[0] and not BT_SPECIAL;
  demo_p := @demo_p[1];

  cmd.buttons2 := demo_p[0] and not BT_SPECIAL;
  demo_p := @demo_p[1];

  cmd.inventory := PInteger(demo_p)^;
  demo_p := @demo_p[4];

  cmd.lookupdown := demo_p[0];
  demo_p := @demo_p[1];
  cmd.lookleftright := demo_p[0];
  demo_p := @demo_p[1];
  cmd.jump_crouch := demo_p[0];
  demo_p := @demo_p[1];

  // JVAL Smooth Look Up/Down
  if demoversion >= VERSION203 then
  begin
    cmd.lookupdown16 := PWord(demo_p)^;
    demo_p := @demo_p[2];
  end
  else
    cmd.lookupdown16 := 256 * cmd.lookupdown;

end;

//
// DEMO RECORDING
//

// Increase the size of the demo buffer to allow unlimited demos
procedure G_IncreaseDemoBuffer;
var
  current_length: integer;
  new_demobuffer: PByteArray;
  new_demop: PByteArray;
  new_length: integer;
begin
  // Find the current size

  current_length := integer(demoend) - integer(demobuffer);

  // Generate a new buffer twice the size
  new_length := current_length + $80000;

  new_demobuffer := Z_Malloc2(new_length, PU_STATIC, nil);
  if new_demobuffer = nil then
    G_CheckDemoStatus;

  new_demop := @new_demobuffer[integer(demo_p) - integer(demobuffer)];

  // Copy over the old data

  memcpy(new_demobuffer, demobuffer, current_length);

  // Free the old buffer and point the demo pointers at the new buffer.

  Z_Free(demobuffer);

  demobuffer := new_demobuffer;
  demo_p := new_demop;
  demoend := @demobuffer[new_length];
end;

procedure G_WriteDemoTiccmd(cmd: Pticcmd_t);
var
  demo_start: PByteArray;
begin
  if gamekeydown[Ord('q')] then // press q to end demo recording
    G_CheckDemoStatus;

  demo_start := demo_p;

  demo_p[0] := Ord(cmd.forwardmove);
  demo_p := @demo_p[1];

  demo_p[0] := Ord(cmd.sidemove);
  demo_p := @demo_p[1];

  PSmallInt(demo_p)^ := cmd.angleturn;
  demo_p := @demo_p[2];

  demo_p[0] := cmd.buttons and not BT_SPECIAL;
  demo_p := @demo_p[1];

  demo_p[0] := cmd.buttons2;
  demo_p := @demo_p[1];

  PInteger(demo_p)^ := cmd.inventory;
  demo_p := @demo_p[4];

  demo_p[0] := cmd.lookupdown;
  demo_p := @demo_p[1];

  demo_p[0] := cmd.lookleftright;
  demo_p := @demo_p[1];

  demo_p[0] := cmd.jump_crouch;
  demo_p := @demo_p[1];

  // JVAL Smooth Look Up/Down
  PWord(demo_p)^ := cmd.lookupdown16;
//  demo_p := @demo_p[2];

  demo_p := demo_start;

  if integer(demo_p) >= integer(demoend) - 2 * SizeOf(ticcmd_t) then
    G_IncreaseDemoBuffer;

  G_ReadDemoTiccmd(cmd);  // make SURE it is exactly the same
end;

//
// G_RecordDemo
//
procedure G_RecordDemo(const name: string);
var
  i: integer;
  maxsize: integer;
begin
  usergame := false;
  demoname := name;
  if Pos('.', demoname) = 0 then
    demoname := demoname + '.lmp';


  i := M_CheckParm ('-maxdemo');
  if (i <> 0) and (i < myargc - 1) then
    maxsize := atoi(myargv[i + 1]) * 1024
  else
    maxsize := SAVEGAMESIZE;

  repeat
    demobuffer := Z_Malloc2(maxsize, PU_STATIC, nil);
    if demobuffer = nil then
      maxsize := maxsize * 3 div 4;
  until demobuffer <> nil;

  demoend := @demobuffer[maxsize];

  demorecording := true;
  Info_Init(true); // JVAL: Start thinkers

end;

const
  DEMOHDR: integer = $4F4D4544; // JVAL: DEMO in hex

procedure G_BeginRecording;
var
  i: integer;
begin
  demo_p := demobuffer;
  demoversion := VERSION;

  PInteger(demo_p)^ := DEMOHDR;
  demo_p := @demo_p[4];

  demo_p[0] := VERSION;
  demo_p := @demo_p[1];

  demo_p[0] := Ord(gameskill);
  demo_p := @demo_p[1];

  demo_p[0] := gamemap;
  demo_p := @demo_p[1];

  demo_p[0] := deathmatch;
  demo_p := @demo_p[1];

  demo_p[0] := intval(respawnparm);
  demo_p := @demo_p[1];

  demo_p[0] := intval(fastparm);
  demo_p := @demo_p[1];

  demo_p[0] := intval(nomonsters);
  demo_p := @demo_p[1];

  demo_p[0] := intval(compatibilitymode);
  demo_p := @demo_p[1];

  demo_p[0] := intval(spawnrandommonsters);
  demo_p := @demo_p[1];

  demo_p[0] := sysrndseed;
  demo_p := @demo_p[1];

  demo_p[0] := consoleplayer;
  demo_p := @demo_p[1];

  for i := 0 to MAXPLAYERS - 1 do
  begin
    demo_p[0] := intval(playeringame[i]);
    demo_p := @demo_p[1];
  end;

end;

//
// G_PlayDemo
//
var
  defdemoname: string;
  externaldemo: boolean = false;

function G_DoPlayExternalDemo(const name: string): boolean;
var
  dmname: string;
begin
  dmname := name;
  if not fexists(dmname) then
    if M_CheckParmCDROM then
      dmname := CD_WORKDIR + dmname;
  if fexists(dmname) then
  begin
    externaldemo := true;
    defdemoname := dmname;
    gameaction := ga_playdemo;
    printf('Playing demo %s'#13#10, [defdemoname]);
    result := true;
    exit;
  end;
  result := false;
end;

function G_DeferedPlayDemo(const name: string): boolean;
var
  dmname: string;
  dmname2: string;
  defdemoname8: char8_t;
  i: integer;
  pdot: integer;
begin
  dmname := name;
  for i := 0 to 9 do
    if itoa(i) = name then
    begin
      sprintf(dmname, 'demo%d', [i]);
      break;
    end;

  pdot := Pos('.', dmname);
  if ((Length(dmname) <= 8) and (pdot = 0)) or ((pdot < 9) and (pdot <> 0)) then
  begin
    ExtractFileBase8(dmname, defdemoname8);
    defdemoname := char8tostring(defdemoname8);
    if W_CheckNumForName(defdemoname) >= 0 then
    begin
      externaldemo := false;
      gameaction := ga_playdemo;
      printf('Playing demo %s'#13#10, [defdemoname]);
      result := true;
      exit;
    end;
  end;

  // JVAL
  // Playdemo command also looks for a filename in current or CD_WORK directory
  dmname2 := dmname;
  if pdot = 0 then
    dmname := dmname + '.lmp';
  if G_DoPlayExternalDemo(dmname) then
  begin
    result := true;
    exit;
  end;

  if pdot = 0 then
  begin
    dmname := dmname2 + '.dem';
    if G_DoPlayExternalDemo(dmname) then
    begin
      result := true;
      exit;
    end;
  end;

  I_Warning('G_DeferedPlayDemo(): Can not find demo %s.'#13#10, [defdemoname]);
  result := false;
end;

procedure G_CmdPlayDemo(const name: string);
begin
  if G_DeferedPlayDemo(name) then
    C_ExecuteCmd('closeconsole', '1');
end;

var
  demotickstart: pointer = nil;

procedure G_DoPlayDemo;
var
  skill: skill_t;
  i, map: integer;
  lump: integer;
  len: integer;
  oldspawnrandommonsters: boolean;
begin
  gameaction := ga_nothing;
  if externaldemo then
    len := M_ReadFile(defdemoname, pointer(demobuffer))
  else
  begin
    lump := W_GetNumForName(defdemoname);
    demobuffer := W_CacheLumpNum(lump, PU_STATIC);
    len := W_LumpLength(lump);
  end;

  demo_p := demobuffer;
  demoend := @demo_p[len];

  if PInteger(demo_p)^ <> DEMOHDR then
  begin
    I_Warning('G_DoPlayDemo(): Demo is from an unsupported game version' + #13#10);
    gameaction := ga_nothing;
    exit;
  end;

  demo_p := @demo_p[4];
  demoversion := demo_p[0];

  if demoversion < VERSION122 then
  begin
    I_Warning('G_DoPlayDemo(): Demo is from an unsupported game version = %d.%.*d' + #13#10,
      [demo_p[0] div 100, 2, demo_p[0] mod 100]);
    gameaction := ga_nothing;
    exit;
  end
  else if demoversion <> VERSION then
    I_Warning('G_DoPlayDemo(): Demo is from a partial supported game version = %d.%.*d' + #13#10,
      [demo_p[0] div 100, 2, demo_p[0] mod 100]);

  demo_p := @demo_p[1];

  skill := skill_t(demo_p[0]);
  demo_p := @demo_p[1];

  map := demo_p[0];
  demo_p := @demo_p[1];

  deathmatch := demo_p[0];
  demo_p := @demo_p[1];

  respawnparm := demo_p[0] <> 0;
  demo_p := @demo_p[1];

  fastparm := demo_p[0] <> 0;
  demo_p := @demo_p[1];

  nomonsters := demo_p[0] <> 0;
  demo_p := @demo_p[1];

  oldcompatibilitymode := compatibilitymode;
  compatibilitymode := demo_p[0] <> 0;
  demo_p := @demo_p[1];

  oldspawnrandommonsters := spawnrandommonsters;
  spawnrandommonsters := demo_p[0] <> 0;
  demo_p := @demo_p[1];
  sysrndseed := demo_p[0];
  demo_p := @demo_p[1];

  consoleplayer := demo_p[0];
  demo_p := @demo_p[1];

  for i := 0 to MAXPLAYERS - 1 do
  begin
    playeringame[i] := demo_p[0] <> 0;
    demo_p := @demo_p[1];
  end;

  if playeringame[1] then
  begin
    netgame := true;
    netdemo := true;
  end;

  preparingdemoplayback := true;
  // don't spend a lot of time in loadlevel if not singledemo
  precache := singledemo; // JVAL original code: precache := false
  G_InitNew(skill, map);
  preparingdemoplayback := false;
  spawnrandommonsters := oldspawnrandommonsters;  // Back to default
  precache := true;
  usergame := false;
  demoplayback := true;
  demotickstart := demo_p;
  Info_Init(true); // JVAL: Start thinkers

  compatibility_done := false;

end;

// 19/9/2009 JVAL: For drawing demo progress
function G_DemoProgress: fixed_t;
begin
  result := round((integer(demo_p) - integer(demotickstart)) / (integer(demoend) - integer(demotickstart)) * FRACUNIT);
  if result > FRACUNIT then
    result := FRACUNIT
  else if result < 0 then
    result := 0;
end;

//
// G_TimeDemo
//
procedure G_TimeDemo(const name: string);
begin
  timingdemo := true;
  singletics := true;

  defdemoname := name;
  gameaction := ga_playdemo;
end;

(*
===================
=
= G_CheckDemoStatus
=
= Called after a death or level completion to allow demos to be cleaned up
= Returns true if a new demo loop action will take place
===================
*)

function G_CheckDemoStatus: boolean;
var
  realtics: integer;
  i: integer;
begin
  if timingdemo then
  begin
    realtics := I_GetTime - starttime;
    if realtics > 0 then
      I_Error('G_CheckDemoStatus(): timed %d gametics in %d realtics'#13#10'(%3.2ffps)',
        [gametic, realtics, gametic / realtics * TICRATE])
    else
      I_Error('G_CheckDemoStatus(): timed %d gametics in %d realtics',
        [gametic, realtics]);
  end;

  if demoplayback then
  begin
    if singledemo then
      I_Quit;

    Z_Free(demoBuffer);
    G_FinishedDemoPlayback;
    netdemo := false;
    netgame := false;
    deathmatch := 0;
    for i := 1 to MAXPLAYERS - 1 do
      playeringame[i] := false;
    respawnparm := false;
    fastparm := false;
    nomonsters := false;
    consoleplayer := 0;
    D_AdvanceDemo;
    result := true;
    exit;
  end;

  if demorecording then
  begin
    demo_p[0] := DEMOMARKER;
    demo_p := @demo_p[1];

    M_WriteFile(demoname, demobuffer, pDiff(demo_p, demobuffer, SizeOf(byte)));
    Z_Free(demobuffer);
    demorecording := false;
    I_Error('G_CheckDemoStatus(): Demo %s recorded', [demoname]);
  end;

  result := false;
end;

function G_NeedsCompatibilityMode: boolean;
begin
  result := compatibilitymode;
end;

function G_PlayingEngineVersion: byte;
begin
  if demoplayback then
    result := demoversion
  else
    result := VERSION;
end;

procedure G_Quit;
begin
  if displayendscreen then
  begin
    gamestate := GS_ENDOOM;
    S_PauseSound; // Stop music in ENDOOM screen
    printf('E_Init: Initializing ENDSTRF screen.'#13#10);
    E_Init;
  end
  else
    I_Quit;
end;

initialization

  forwardmove[0] := $19;
  forwardmove[1] := $32;
  sidemove[0] := $18;
  sidemove[1] := $28;
  angleturn[0] := 640;
  angleturn[1] := 1280;
  angleturn[2] := 320;

  mousebuttons := PBooleanArray(@mousearray[0]);
  joybuttons := PBooleanArray(@joyarray[0]);

  precache := true;

  KEY_WEAPONS[0] := @key_weapon0;
  KEY_WEAPONS[1] := @key_weapon1;
  KEY_WEAPONS[2] := @key_weapon2;
  KEY_WEAPONS[3] := @key_weapon3;
  KEY_WEAPONS[4] := @key_weapon4;
  KEY_WEAPONS[5] := @key_weapon5;
  KEY_WEAPONS[6] := @key_weapon6;
  KEY_WEAPONS[7] := @key_weapon7;
  KEY_WEAPONS[8] := @key_weapon8;
  KEY_WEAPONS[9] := @key_weapon9;

end.

