//------------------------------------------------------------------------------
//
//  DelphiHexen: A modified and improved Hexen port for Windows
//  based on original Linux Doom as published by "id Software", on
//  Hexen source as published by "Raven" software and DelphiDoom
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
  d_event,
  d_player,
  d_ticcmd;

//-----------------------------------------------------------------------------
//
// DESCRIPTION:
//   Duh.
//
//-----------------------------------------------------------------------------

//
// GAME
//

procedure G_DeathMatchSpawnPlayer(playernum: integer);

procedure G_InitNew(skill: skill_t; episode: integer; map: integer);

// Can be called by the startup code or M_Responder.
// A normal game starts at map 1,
// but a warp test can start elsewhere
procedure G_DeferedInitNew(skill:skill_t; map: integer);

procedure G_DeferedInitNewUntranslated(skill:skill_t; map: integer);

procedure G_CmdNewGame(const parm1, parm2: string);

{ Can be called by the startup code or M_Responder, }
{ calls P_SetupLevel or W_EnterWorld. }
procedure G_LoadGame(slot: integer);

procedure G_DoLoadGame;

{ Called by M_Responder. }
procedure G_SaveGame(slot: integer; description: string);

procedure G_CmdSaveGame(const sname: string; const description: string);

procedure G_WorldDone;

procedure G_Ticker;

function G_Responder(ev: Pevent_t): boolean;

procedure G_ScreenShot;

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
// Fly move
  key_flyup: integer;
  key_flydown: integer;
  key_flycenter: integer;
// JVAL Look Left and Right
  key_lookright: integer;
  key_lookleft: integer;
  key_lookforward: integer;

  key_strafeleft: integer;
  key_straferight: integer;
  key_fire: integer;
  key_use: integer;
  key_strafe: integer;
  key_speed: integer;
// JVAL Jump
  key_jump: integer;
// JVAL: 20211101 - Crouch
  key_crouch: integer;

  key_invleft,
  key_invright,
  key_useartifact: integer;

// JVAL 20191207 Key bindings for weapon change
  key_weapon0: integer = Ord('1');
  key_weapon1: integer = Ord('2');
  key_weapon2: integer = Ord('3');
  key_weapon3: integer = Ord('4');

  KEY_WEAPONS: array[0..Ord(NUMWEAPONS) - 1] of PInteger;

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

  preparingdemoplayback: boolean = false;

  gameepisode: integer;
  gamemap: integer;
  prevmap: integer;

  deathmatch: integer; // only if started as net death
  netgame: boolean; // only true if packets are broadcast
  playeringame: array[0..MAXPLAYERS - 1] of boolean;
  PlayerClass: array[0..MAXPLAYERS - 1] of pclass_t;

  consoleplayer: integer; // player taking events and displaying
  displayplayer: integer; // view being displayed
  gametic: integer;

  totalkills, totalitems, totalsecret: integer; // for intermission

  wminfo: wbstartstruct_t; // parms for world map / intermission

  gameskill: skill_t;

  bodyqueslot: integer;

  precache: boolean = true; // if true, load all graphics at start

  respawnmonsters: boolean;

  viewactive: boolean;

  singledemo: boolean; // quit after playing a demo from cmdline

  demorecording: boolean = false;

  gameaction: gameaction_t;

  usergame: boolean; // ok to save / end game

procedure G_SetKeyboardMode(const mode: integer);

procedure G_PlayerReborn(player: integer);

procedure G_BuildTiccmd(cmd: Pticcmd_t);

procedure G_Completed(map, position: integer);

var
  statcopy: pointer = nil;  // for statistics driver

var
  forwardmove: array[0..Ord(NUMCLASSES) - 1, 0..1] of integer = (
    ($1D, $3C),
    ($19, $32),
    ($16, $2E),
    ($18, $31)
  );

  sidemove: array[0..Ord(NUMCLASSES) - 1, 0..1] of integer = (
    ($1B, $3B),
    ($18, $28),
    ($15, $25),
    ($17, $27)
  );

  angleturn: array[0..2] of integer = (640, 1280, 320);

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

var
  LeaveMap: integer;
  LeavePosition: integer;

var
  starttime: integer;        // for comparative timing purposes

const
  SAVEGAMESIZE = $80000; // Originally $2C000

const
  NUMKEYS = 256;

var
  gamekeydown: array[0..NUMKEYS - 1] of boolean;
  mousebuttons: PBooleanArray;
  joybuttons: PBooleanArray;

implementation

uses
  c_cmds,
  z_zone,
  doomstat,
  doomdata,
  am_map,
  d_net,
  d_net_h,
  d_main,
  f_finale,
  info_h,
  info,
  i_system,
  i_io,
  m_argv,
  m_misc,
  m_menu,
  m_rnd,
  g_demo,
  a_action,
  p_setup,
  p_tick,
  p_local,
  p_mobj_h,
  p_mobj,
  p_inter,
  p_map,
  p_user,
  p_acs,
  p_levelinfo,
  ps_main,
  in_stuff,
  hu_stuff,
  sb_bar,
  w_wad,
  s_sound,
// Data.
  xn_strings,
  sounds,
// SKY handling - still the wrong place.
  r_data,
  r_sky,
  r_defs,
  r_main,
  r_draw,
  r_intrpl,
  s_sndseq,
  sv_save,
  tables;


//==========================================================================
//
// G_StartNewInit
//
//==========================================================================

var
  RebornPosition: integer;

procedure G_StartNewInit;
begin
  SV_InitBaseSlot;
  SV_ClearRebornSlot;
  P_ACSInitNewGame;
  // Default the player start spot group to 0
  RebornPosition := 0;
end;


//==========================================================================
//
// G_StartNewGame
//
//==========================================================================
var
  TempSkill: skill_t;

procedure G_StartNewGame(skill: skill_t);
var
  realMap: integer;
begin
  G_StartNewInit;
  realMap := P_TranslateMap(1);
  if realMap = -1 then
    realMap := 1;
  G_InitNew(TempSkill, 1, realMap);
end;


//==========================================================================
//
// G_DoNewGame
//
//==========================================================================

procedure G_DoNewGame;
begin
  G_FinishedDemoPlayback; // JVAL: remove???
  G_StartNewGame(TempSkill);
  gameaction := ga_nothing;
end;

var
  TempEpisode: integer;
  TempMap: integer;

procedure G_DoInitNew;
begin
  SV_InitBaseSlot;
  SV_ClearRebornSlot;
  G_InitNew(TempSkill, TempEpisode, TempMap);
  gameaction := ga_nothing;
end;


var
  sendsave: boolean;         // send a save event next tic
  sendcmdsave: boolean;      // send a save event next tic (console)

  levelstarttic: integer;    // gametic at level start

  consistancy: array[0..MAXPLAYERS - 1] of array[0..BACKUPTICS - 1] of smallint;

function MAXPLMOVE(pclass: integer): fixed_t;
begin
  result := forwardmove[pclass, 1];
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

var
  usearti: boolean = true;


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
  look: integer;    // JVAL Look up and down
  look16: integer;  // JVAL Smooth Look Up/Down
  look2: integer;   // JVAL look left and right
  flyheight: integer;
  base: Pticcmd_t;
  imousex: integer;
  imousey: integer;
  pclass: integer;
  cmd_jump, cmd_crouch: byte;
begin
  pclass := Ord(players[consoleplayer]._class);

  base := I_BaseTiccmd;    // empty, or external driver

  memcpy(cmd, base, SizeOf(cmd^));

  cmd.consistancy := consistancy[consoleplayer][maketic mod BACKUPTICS];

  strafe := gamekeydown[key_strafe] or
            (usemouse and mousebuttons[mousebstrafe]) or
            (usejoystick and joybuttons[joybstrafe]);
  speed := intval(gamekeydown[key_speed] or joybuttons[joybspeed]);
  if autorunmode then
    speed := 1 - speed;

  _forward := 0;
  side := 0;
  look := 0;
  look16 := 0; // JVAL Smooth Look Up/Down
  look2 := 0;
  flyheight := 0;

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
      side := side + sidemove[pclass, speed];
    if gamekeydown[key_left] then
      side := side - sidemove[pclass, speed];
    if joyxmove > 0 then
      side := side + sidemove[pclass, speed];
    if joyxmove < 0 then
      side := side - sidemove[pclass, speed];
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
    _forward := _forward + forwardmove[pclass, speed];

  if gamekeydown[key_down] then
    _forward := _forward - forwardmove[pclass, speed];

  // JVAL Look up/down/center keys
  if zaxisshift then
  begin
    if gamekeydown[key_lookup] then
      look := lspeed;

    if gamekeydown[key_lookdown] then
      look := -lspeed;

    if gamekeydown[key_lookcenter] then
      look := TOCENTER;

    look16 := 256 * look; // JVAL Smooth Look Up/Down
  end;

  // JVAL Look right/left/forward keys
  if gamekeydown[key_lookleft] or (usejoystick and joybuttons[joyblleft]) then
    look2 := lspeed2;

  if gamekeydown[key_lookright] or (usejoystick and joybuttons[joyblright]) then
    look2 := -lspeed2;

  if gamekeydown[key_lookforward] then
    look2 := TOFORWARD;

  if joyymove < 0 then
    _forward := _forward + forwardmove[pclass, speed];

  if joyymove > 0 then
    _forward := _forward - forwardmove[pclass, speed];

  if gamekeydown[key_straferight] then
    side := side + sidemove[pclass, speed];

  if gamekeydown[key_strafeleft] then
    side := side - sidemove[pclass, speed];

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
  for i := 0 to Ord(NUMWEAPONS) - 1 do
    if gamekeydown[KEY_WEAPONS[i]^] then
    begin
      cmd.buttons := cmd.buttons or BT_CHANGE;
      cmd.buttons := cmd.buttons or _SHL(i, BT_WEAPONSHIFT);
      break;
    end;

  // mouse
  if (usemouse and mousebuttons[mousebforward]) then
    _forward := _forward + forwardmove[pclass, speed];

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
    look := look + imousey div 16;
    if imousey < 0 then
    begin
      if look < -4 then
        look := -4;
    end
    else if imousey > 0 then
    begin
      if look > 4 then
        look := 4;
    end;

    // JVAL Smooth Look Up/Down
    if G_PlayingEngineVersion < VERSION203 then
      look16 := 256 * look
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

  // JVAL: For smooth mouse movement
  mousex := mousex div 4;
  mousey := mousey div 4;

  if _forward > MAXPLMOVE(pclass) then
    _forward := MAXPLMOVE(pclass)
  else if _forward < -MAXPLMOVE(pclass) then
    _forward := -MAXPLMOVE(pclass);

  if side > MAXPLMOVE(pclass) then
    side := MAXPLMOVE(pclass)
  else if side < -MAXPLMOVE(pclass) then
    side := -MAXPLMOVE(pclass);

  // Adjust for a player with a speed artifact
  if (players[consoleplayer].powers[Ord(pw_speed)] > 0) and
     (players[consoleplayer].morphTics = 0) then
  begin
    _forward := _SHR1(3 * _forward);
    side := _SHR1(3 * side);
  end;

  cmd.forwardmove := cmd.forwardmove + _forward;
  cmd.sidemove := cmd.sidemove + side;

  if players[consoleplayer].playerstate = PST_LIVE then
  begin
    if zaxisshift then
    begin
      if look < 0 then
        look := look + 16;
      cmd.lookfly := look;

      // JVAL Smooth Look Up/Down
      if look16 < 0 then
        look16 := look16 + 16 * 256;
      cmd.lookupdown16 := look16;
    end;
    if look2 < 0 then
      look2 := look2 + 16;
    cmd.lookleftright:= look2;
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

  // Fly up/down/drop keys
  if gamekeydown[key_flyup] then
    flyheight := 5; // note that the actual flyheight will be twice this

  if gamekeydown[key_flydown] then
    flyheight := -5;

  if gamekeydown[key_flycenter] then
    flyheight := TOCENTER;

  if flyheight < 0 then
    flyheight := flyheight + 16;

  cmd.lookfly := cmd.lookfly or _SHL(flyheight, 4);

  // Use artifact key
  cmd.arti := 0;
  if gamekeydown[key_useartifact] then
  begin
    if gamekeydown[key_speed] and not noartiskip then
    begin
      if players[consoleplayer].inventory[inv_ptr]._type <> Ord(arti_none) then
      begin
        gamekeydown[key_useartifact] := false;
        cmd.arti := $ff; // skip artifact code
      end
    end
    else
    begin
      if inventory then
      begin
        players[consoleplayer].readyArtifact :=
          artitype_t(players[consoleplayer].inventory[inv_ptr]._type);
        inventory := false;
        cmd.arti := 0;
        usearti := false;
      end
      else if usearti then
      begin
        cmd.arti := players[consoleplayer].inventory[inv_ptr]._type;
        usearti := false;
      end;
    end;
  end
  else if gamekeydown[KEY_BACKSPACE] then
  begin
    gamekeydown[KEY_BACKSPACE] := false;   // Use one of each artifact
    cmd.arti := Ord(NUMARTIFACTS);
  end
  else if gamekeydown[Ord('0')] then
  begin
    gamekeydown[Ord('0')] := false;
    cmd.arti := Ord(arti_poisonbag);
  end
  else if gamekeydown[Ord('9')] then
  begin
    gamekeydown[Ord('9')] := false;
    cmd.arti := Ord(arti_blastradius);
  end
  else if gamekeydown[Ord('8')] then
  begin
    gamekeydown[Ord('8')] := false;
    cmd.arti := Ord(arti_teleport);
  end
  else if gamekeydown[Ord('7')] then
  begin
    gamekeydown[Ord('7')] := false;
    cmd.arti := Ord(arti_teleportother);
  end
  else if gamekeydown[Ord('6')] then
  begin
    gamekeydown[Ord('6')] := false;
    cmd.arti := Ord(arti_egg);
  end
  else if gamekeydown[Ord('5')] and (players[consoleplayer].powers[Ord(pw_invulnerability)] = 0) then
  begin
    gamekeydown[Ord('5')] := false;
    cmd.arti := Ord(arti_invulnerability);
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

procedure G_BuildTiccmd202(cmd: Pticcmd_t202);
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
  look: integer;    // JVAL Look up and down
  look2: integer;   // JVAL look left and right
  flyheight: integer;
  base: Pticcmd_t202;
  imousex: integer;
  imousey: integer;
  pclass: integer;
begin
  pclass := Ord(players[consoleplayer]._class);

  base := I_BaseTiccmd202;    // empty, or external driver

  memcpy(cmd, base, SizeOf(cmd^));

  cmd.consistancy := consistancy[consoleplayer][maketic mod BACKUPTICS];

  strafe := gamekeydown[key_strafe] or
            (usemouse and mousebuttons[mousebstrafe]) or
            (usejoystick and joybuttons[joybstrafe]);
  speed := intval(gamekeydown[key_speed] or joybuttons[joybspeed]);
  if autorunmode then
    speed := 1 - speed;

  _forward := 0;
  side := 0;
  look := 0;
  look2 := 0;
  flyheight := 0;

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
      side := side + sidemove[pclass, speed];
    if gamekeydown[key_left] then
      side := side - sidemove[pclass, speed];
    if joyxmove > 0 then
      side := side + sidemove[pclass, speed];
    if joyxmove < 0 then
      side := side - sidemove[pclass, speed];
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
    _forward := _forward + forwardmove[pclass, speed];

  if gamekeydown[key_down] then
    _forward := _forward - forwardmove[pclass, speed];

  // JVAL Look up/down/center keys
  if zaxisshift then
  begin
    if gamekeydown[key_lookup] then
      look := lspeed;

    if gamekeydown[key_lookdown] then
      look := -lspeed;

    if gamekeydown[key_lookcenter] then
      look := TOCENTER;
  end;

  // JVAL Look right/left/forward keys
  if gamekeydown[key_lookleft] or (usejoystick and joybuttons[joyblleft]) then
    look2 := lspeed2;

  if gamekeydown[key_lookright] or (usejoystick and joybuttons[joyblright]) then
    look2 := -lspeed2;

  if gamekeydown[key_lookforward] then
    look2 := TOFORWARD;

  if joyymove < 0 then
    _forward := _forward + forwardmove[pclass, speed];

  if joyymove > 0 then
    _forward := _forward - forwardmove[pclass, speed];

  if gamekeydown[key_straferight] then
    side := side + sidemove[pclass, speed];

  if gamekeydown[key_strafeleft] then
    side := side - sidemove[pclass, speed];

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
  for i := 0 to Ord(NUMWEAPONS) - 1 do
    if gamekeydown[KEY_WEAPONS[i]^] then
    begin
      cmd.buttons := cmd.buttons or BT_CHANGE;
      cmd.buttons := cmd.buttons or _SHL(i, BT_WEAPONSHIFT);
      break;
    end;

  // mouse
  if (usemouse and mousebuttons[mousebforward]) then
    _forward := _forward + forwardmove[pclass, speed];

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
    look := look + imousey div 16;
    if imousey < 0 then
    begin
      if look < -4 then
        look := -4;
    end
    else if imousey > 0 then
    begin
      if look > 4 then
        look := 4;
    end;
  end;
  // JVAL: For smooth mouse movement
  mousex := mousex div 4;
  mousey := mousey div 4;

  if _forward > MAXPLMOVE(pclass) then
    _forward := MAXPLMOVE(pclass)
  else if _forward < -MAXPLMOVE(pclass) then
    _forward := -MAXPLMOVE(pclass);

  if side > MAXPLMOVE(pclass) then
    side := MAXPLMOVE(pclass)
  else if side < -MAXPLMOVE(pclass) then
    side := -MAXPLMOVE(pclass);

  // Adjust for a player with a speed artifact
  if (players[consoleplayer].powers[Ord(pw_speed)] > 0) and
     (players[consoleplayer].morphTics = 0) then
  begin
    _forward := _SHR1(3 * _forward);
    side := _SHR1(3 * side);
  end;

  cmd.forwardmove := cmd.forwardmove + _forward;
  cmd.sidemove := cmd.sidemove + side;

  if players[consoleplayer].playerstate = PST_LIVE then
  begin
    if zaxisshift then
    begin
      if look < 0 then
        look := look + 16;
      cmd.lookfly := look;
    end;
    if look2 < 0 then
      look2 := look2 + 16;
    cmd.lookleftright:= look2;
    // JVAL
    // allowplayerjumps variable controls if we accept input for jumping
    if allowplayerjumps and (gamekeydown[key_jump] or (usejoystick and joybuttons[joybjump])) then
    begin
      if players[consoleplayer].oldjump <> 0 then
        cmd.jump := 1
      else
        cmd.jump := 2
      end
    else
      cmd.jump := 0;
    players[consoleplayer].oldjump := cmd.jump;
  end;

  // special buttons
  if sendpause then
  begin
    sendpause := false;
    cmd.buttons := BT_SPECIAL or BTS_PAUSE;
  end;

  // Fly up/down/drop keys
  if gamekeydown[key_flyup] then
    flyheight := 5; // note that the actual flyheight will be twice this

  if gamekeydown[key_flydown] then
    flyheight := -5;

  if gamekeydown[key_flycenter] then
    flyheight := TOCENTER;

  if flyheight < 0 then
    flyheight := flyheight + 16;

  cmd.lookfly := cmd.lookfly or _SHL(flyheight, 4);

  // Use artifact key
  cmd.arti := 0;
  if gamekeydown[key_useartifact] then
  begin
    if gamekeydown[key_speed] and not noartiskip then
    begin
      if players[consoleplayer].inventory[inv_ptr]._type <> Ord(arti_none) then
      begin
        gamekeydown[key_useartifact] := false;
        cmd.arti := $ff; // skip artifact code
      end
    end
    else
    begin
      if inventory then
      begin
        players[consoleplayer].readyArtifact :=
          artitype_t(players[consoleplayer].inventory[inv_ptr]._type);
        inventory := false;
        cmd.arti := 0;
        usearti := false;
      end
      else if usearti then
      begin
        cmd.arti := players[consoleplayer].inventory[inv_ptr]._type;
        usearti := false;
      end;
    end;
  end
  else if gamekeydown[KEY_BACKSPACE] then
  begin
    gamekeydown[KEY_BACKSPACE] := false;   // Use one of each artifact
    cmd.arti := Ord(NUMARTIFACTS);
  end
  else if gamekeydown[Ord('0')] then
  begin
    gamekeydown[Ord('0')] := false;
    cmd.arti := Ord(arti_poisonbag);
  end
  else if gamekeydown[Ord('9')] then
  begin
    gamekeydown[Ord('9')] := false;
    cmd.arti := Ord(arti_blastradius);
  end
  else if gamekeydown[Ord('8')] then
  begin
    gamekeydown[Ord('8')] := false;
    cmd.arti := Ord(arti_teleport);
  end
  else if gamekeydown[Ord('7')] then
  begin
    gamekeydown[Ord('7')] := false;
    cmd.arti := Ord(arti_teleportother);
  end
  else if gamekeydown[Ord('6')] then
  begin
    gamekeydown[Ord('6')] := false;
    cmd.arti := Ord(arti_egg);
  end
  else if gamekeydown[Ord('5')] and (players[consoleplayer].powers[Ord(pw_invulnerability)] = 0) then
  begin
    gamekeydown[Ord('5')] := false;
    cmd.arti := Ord(arti_invulnerability);
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

  levelstarttic := gametic;        // for time calculation

  if wipegamestate = Ord(GS_LEVEL) then
    wipegamestate := -1;             // force a wipe

  gamestate := GS_LEVEL;

  for i := 0 to MAXPLAYERS - 1 do
  begin
    if playeringame[i] and (players[i].playerstate = PST_DEAD) then
      players[i].playerstate := PST_REBORN;
    ZeroMemory(@players[i].frags, SizeOf(players[i].frags));
  end;

  S_StopAllSequences;
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
end;

var
  inventoryTics: integer = 0;

//
// G_Responder
// Get info needed to make ticcmd_ts for the players.
//
function G_Responder(ev: Pevent_t): boolean;
var
  bmask: integer;
  i: integer;
  plr: Pplayer_t;
begin
  plr := @players[consoleplayer];
//  if (ev._type = ev_keyup) and (ev.data1 = key_useartifact) then
  if (ev._type = ev_keydown) and (ev.data1 = key_useartifact) then
  begin // flag to denote that it's okay to use an artifact
    if not inventory then
      plr.readyArtifact := artitype_t(plr.inventory[inv_ptr]._type);
    usearti := true;
    gamekeydown[key_useartifact] := true;
    result := true;
    exit;
  end;

  // allow spy mode changes even during the demo
  if (gamestate = GS_LEVEL) and (ev._type = ev_keydown) and
     (ev.data1 = KEY_F12) and (singledemo or (deathmatch = 0)) then
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
    if SB_Responder(ev) then
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
        if ev.data1 = key_invleft then
        begin
          inventoryTics := 5 * TICRATE;
          if inventory then
          begin
            dec(inv_ptr);
            if inv_ptr < 0 then
              inv_ptr := 0
            else
            begin
              dec(curpos);
              if curpos < 0 then
                curpos := 0;
            end;
            result := true;
            exit;
          end
          else
            inventory := true;
        end;

        if ev.data1 = key_invright then
        begin
          inventoryTics := 5 * TICRATE;
          if inventory then
          begin
            inc(inv_ptr);
            if inv_ptr >= plr.inventorySlotNum then
            begin
              dec(inv_ptr);
              if inv_ptr < 0 then
                inv_ptr := 0;
            end
            else
            begin
              inc(curpos);
              if curpos > 6 then
                curpos := 6;
            end;
            result := true;
            exit;
          end
          else
            inventory := true;
        end;

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
// PLAYER STRUCTURE FUNCTIONS
// also see P_SpawnPlayer in P_Things
//

//==========================================================================
//
// G_PlayerExitMap
//
// Called when the player leaves a map.
//
//==========================================================================

procedure G_PlayerExitMap(playerNumber: integer);
var
  i: integer;
  player: Pplayer_t;
  flightPower: integer;
begin
  player := @players[playerNumber];

  // Strip all current powers (retain flight)
  flightPower := player.powers[Ord(pw_flight)];
  memset(@player.powers, 0, SizeOf(player.powers));
  player.powers[Ord(pw_flight)] := flightPower;

  if deathmatch <> 0 then
  begin
    player.powers[Ord(pw_flight)] := 0;
  end
  else
  begin
    if P_GetMapCluster(gamemap) <> P_GetMapCluster(LeaveMap) then
    begin // Entering new cluster
      // Strip all keys
      player.keys := 0;

      // Strip flight artifact
      for i := 0 to p_maxartifacts - 1 do
      begin
        player.powers[Ord(pw_flight)] := 0;
        P_PlayerUseArtifact(player, arti_fly);
      end;
      player.powers[Ord(pw_flight)] := 0;
    end;
  end;

  if player.morphTics <> 0 then
  begin
    player.readyweapon := weapontype_t(player.mo.special1); // Restore weapon
    player.morphTics := 0;
  end;
  player.messageTics := 0;
  player.lookdir := 0;
  player.mo.flags := player.mo.flags and not MF_SHADOW; // Remove invisibility
  player.extralight := 0; // Remove weapon flashes
  player.fixedcolormap := 0; // Remove torch
  player.damagecount := 0; // No palette changes
  player.bonuscount := 0;
  player.poisoncount := 0;
  if player = @players[consoleplayer] then
  begin
    viewangleoffset := 0;
  end;
end;


//
// G_PlayerFinishLevel
// Can when a player completes a level.
//
procedure G_PlayerFinishLevel(p: Pplayer_t);
begin
  ZeroMemory(@p.powers, SizeOf(p.powers));
  ZeroMemory(@p.keys, SizeOf(p.keys));
  p.mo.flags := p.mo.flags and not MF_SHADOW; // cancel invisibility
  p.lookdir := 0;       // JVAL cancel lookdir Up/Down
  p.lookdir16 := 0;     // JVAL Smooth Look Up/Down
  p.centering := false;
  p.lookdir2 := 0;      // JVAL cancel lookdir Left/Right
  p.forwarding := false;
  p.extralight := 0;    // cancel gun flashes
  p.fixedcolormap := 0; // cancel ir gogles
  p.damagecount := 0;   // no palette changes
  p.bonuscount := 0;
end;

procedure G_SetKeyboardMode(const mode: integer);
begin
  if mode = 0 then
  begin
    key_right := 174;
    key_left := 172;
    key_up := 173;
    key_down := 175;
    key_strafeleft := 44;
    key_straferight := 46;
    key_jump := 97;
    // JVAL: 20211101 - Crouch
    key_crouch := 122;
    key_fire := 157;
    key_use := 32;
    key_strafe := 184;
    key_speed := 182;
    key_lookup := 197;
    key_lookdown := 202;
    key_lookcenter := 199;
    key_lookright := 42;
    key_lookleft := 47;
    key_lookforward := 13;
    key_flyup := 198;
    key_flydown := 200;
    key_flycenter := 201;
    key_invleft := Ord('[');
    key_invright := Ord(']');
    key_weapon0 := Ord('1');
    key_weapon1 := Ord('2');
    key_weapon2 := Ord('3');
    key_weapon3 := Ord('4');
  end
  else if mode = 1 then
  begin
    key_right := 174;
    key_left := 172;
    key_up := 119;
    key_down := 115;
    key_strafeleft := 97;
    key_straferight := 100;
    key_jump := 101;
    // JVAL: 20211101 - Crouch
    key_crouch := 113;
    key_fire := 157;
    key_use := 32;
    key_strafe := 184;
    key_speed := 182;
    key_lookup := 197;
    key_lookdown := 202;
    key_lookcenter := 199;
    key_lookright := 42;
    key_lookleft := 47;
    key_lookforward := 13;
    key_flyup := 198;
    key_flydown := 200;
    key_flycenter := 201;
    key_invleft := Ord('[');
    key_invright := Ord(']');
    key_weapon0 := Ord('1');
    key_weapon1 := Ord('2');
    key_weapon2 := Ord('3');
    key_weapon3 := Ord('4');
  end
  else if mode = 2 then
  begin
    key_right := 174;
    key_left := 172;
    key_up := 101;
    key_down := 100;
    key_strafeleft := 115;
    key_straferight := 102;
    key_jump := 97;
    // JVAL: 20211101 - Crouch
    key_crouch := 122;
    key_fire := 157;
    key_use := 32;
    key_strafe := 184;
    key_speed := 182;
    key_lookup := 197;
    key_lookdown := 202;
    key_lookcenter := 199;
    key_lookright := 42;
    key_lookleft := 47;
    key_lookforward := 13;
    key_flyup := 198;
    key_flydown := 200;
    key_flycenter := 201;
    key_invleft := Ord('[');
    key_invright := Ord(']');
    key_weapon0 := Ord('1');
    key_weapon1 := Ord('2');
    key_weapon2 := Ord('3');
    key_weapon3 := Ord('4');
  end
end;

//
// G_PlayerReborn
// Called after a player dies
// almost everything is cleared and initialized
//
procedure G_PlayerReborn(player: integer);
var
  p: Pplayer_t;
  frags: array[0..MAXPLAYERS - 1] of integer;
  killcount: integer;
  itemcount: integer;
  secretcount: integer;
  worldTimer: integer;
  cheats: integer;
begin
  p := @players[player];
  memcpy(@frags, @p.frags, SizeOf(frags));
  killcount := p.killcount;
  itemcount := p.itemcount;
  secretcount := p.secretcount;
  worldTimer := p.worldTimer;

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
  p.itemcount := itemcount;
  p.secretcount := secretcount;
  p.worldTimer := worldTimer;
  p._class := PlayerClass[player];

  p.usedown := true;
  p.attackdown := true;  // don't do anything immediately
  p.playerstate := PST_LIVE;

  case p._class of
    PCLASS_FIGHTER: p.health := mobjinfo[Ord(MT_PLAYER_FIGHTER)].spawnhealth;
    PCLASS_CLERIC: p.health := mobjinfo[Ord(MT_PLAYER_CLERIC)].spawnhealth;
    PCLASS_MAGE: p.health := mobjinfo[Ord(MT_PLAYER_MAGE)].spawnhealth;
  else
    p.health := p_maxmorphhealth;
  end;

  // Initial weapons
  p.readyweapon := WP_FIRST;
  p.pendingweapon := WP_FIRST;
  p.weaponowned[Ord(WP_FIRST)] := true;

  localQuakeHappening[player] := 0;

  if player = consoleplayer then
  begin
    inv_ptr := 0; // reset the inventory pointer
    curpos := 0;
    viewangleoffset := 0;
  end;

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

  players[playernum].mo.flags2 := players[playernum].mo.flags2 and not MF2_PASSMOBJ;

  // JVAL: 3d floors
  ss := R_PointInSubsector(x, y);
  z := ss.sector.floorheight;
  if ss.sector.midsec >= 0 then
    if players[playernum].mo.spawnpoint.options and MTF_ONMIDSECTOR <> 0 then
      z := sectors[ss.sector.midsec].ceilingheight;

  if not P_CheckPosition(players[playernum].mo, x, y) then
  begin
    players[playernum].mo.flags2 := players[playernum].mo.flags2 or MF2_PASSMOBJ;
    result := false;
    exit;
  end;
  players[playernum].mo.flags2 := players[playernum].mo.flags2 or MF2_PASSMOBJ;

  // flush an old corpse if needed
  if bodyqueslot >= BODYQUESIZE then
    P_RemoveMobj(bodyque[bodyqueslot mod BODYQUESIZE]);
  bodyque[bodyqueslot mod BODYQUESIZE] := players[playernum].mo;
  inc(bodyqueslot);

  // spawn a teleport fog
  {$IFDEF FPC}
  an := _SHRW(ANG45 * (mthing.angle div 45), ANGLETOFINESHIFT);
  {$ELSE}
  an := (ANG45 * (mthing.angle div 45)) shr ANGLETOFINESHIFT;
  {$ENDIF}

  mo := P_SpawnMobj(x + 20 * finecosine[an], y + 20 * finesine[an],
          z + TELEFOGHEIGHT, Ord(MT_TFOG));

  if players[consoleplayer].viewz <> 1 then
    S_StartSound(mo, Ord(SFX_TELEPORT));  // don't start sound on first frame

  result := true;
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
  selections := deathmatch_p;
  if selections < 4 then
    I_Error('G_DeathMatchSpawnPlayer(): Only %d deathmatch spots, 4 required', [selections]);

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

//==========================================================================
//
// G_DoReborn
//
//==========================================================================

procedure G_DoReborn(playernum: integer);
var
  i: integer;
  oldWeaponowned: array[0..Ord(NUMWEAPONS) - 1] of boolean;
  oldKeys: integer;
  oldPieces: integer;
  foundSpot: boolean;
  bestWeapon: integer;
begin
  if G_CheckDemoStatus then
    exit;

  if not netgame then
  begin
    if SV_RebornSlotAvailable then
    begin // Use the reborn code if the slot is available
      gameaction := ga_singlereborn;
    end
    else
    begin // Start a new game if there's no reborn info
      gameaction := ga_newgame;
    end;
  end
  else
  begin // Net-game
    players[playernum].mo.player := nil; // Dissassociate the corpse

    if deathmatch <> 0 then
    begin // Spawn at random spot if in death match
      G_DeathMatchSpawnPlayer(playernum);
      exit;
    end;

    // Cooperative net-play, retain keys and weapons
    oldKeys := players[playernum].keys;
    oldPieces := players[playernum].pieces;
    for i := 0 to Ord(NUMWEAPONS) - 1 do
      oldWeaponowned[i] := players[playernum].weaponowned[i];

    foundSpot := false;
    if G_CheckSpot(playernum, @playerstarts[RebornPosition][playernum]) then
    begin // Appropriate player start spot is open
      P_SpawnPlayer(@playerstarts[RebornPosition, playernum]);
      foundSpot := true;
    end
    else
    begin
      // Try to spawn at one of the other player start spots
      for i := 0 to MAXPLAYERS - 1 do
      begin
        if G_CheckSpot(playernum, @playerstarts[RebornPosition, i]) then
        begin // Found an open start spot

          // Fake as other player
          playerstarts[RebornPosition, i]._type := playernum + 1;
          P_SpawnPlayer(@playerstarts[RebornPosition, i]);

          // Restore proper player type
          playerstarts[RebornPosition, i]._type := i + 1;

          foundSpot := true;
          break;
        end;
      end;
    end;

    if not foundSpot then
    begin // Player's going to be inside something
      P_SpawnPlayer(@playerstarts[RebornPosition][playernum]);
    end;

    // Restore keys and weapons
    players[playernum].keys := oldKeys;
    players[playernum].pieces := oldPieces;
    bestWeapon := 0;
    for i := 0 to Ord(NUMWEAPONS) - 1 do
    begin
      if oldWeaponowned[i] then
      begin
        bestWeapon := i;
        players[playernum].weaponowned[i] := true;
      end;
    end;
    players[playernum].mana[Ord(MANA_1)] := 25;
    players[playernum].mana[Ord(MANA_2)] := 25;
    if bestWeapon <> 0 then
    begin // Bring up the best weapon
      players[playernum].pendingweapon := weapontype_t(bestWeapon);
    end;
  end;
end;

procedure G_ScreenShot;
begin
  gameaction := ga_screenshot;
end;

//==========================================================================
//
// G_TeleportNewMap
//
// Only called by the warp cheat code.  Works just like normal map to map
// teleporting, but doesn't do any interlude stuff.
//
//==========================================================================

procedure G_TeleportNewMap(map: integer;  position: integer);
begin
  gameaction := ga_leavemap;
  LeaveMap := map;
  LeavePosition := position;
end;

//==========================================================================
//
// G_DoTeleportNewMap
//
//==========================================================================

procedure G_DoTeleportNewMap;
begin
  SV_MapTeleport(LeaveMap, LeavePosition);
  gamestate := GS_LEVEL;
  gameaction := ga_nothing;
  RebornPosition := LeavePosition;
end;

//==========================================================================
//
// G_Completed
//
// Starts intermission routine, which is used only during hub exits,
// and DeathMatch games.
//==========================================================================

procedure G_Completed(map, position: integer);
begin
  gameaction := ga_completed;
  LeaveMap := map;
  LeavePosition := position;
end;

procedure G_DoCompleted;
var
  i: integer;
begin
  gameaction := ga_nothing;
  if G_CheckDemoStatus then
    exit;

  for i := 0 to MAXPLAYERS - 1 do
    if playeringame[i] then
      G_PlayerExitMap(i);

  if amstate <> am_inactive then
  begin
    amstate := am_inactive;
    AM_Stop;
  end;

  if (LeaveMap = -1) and (LeavePosition = -1) then
    gameaction := ga_victory
  else
  begin
    gamestate := GS_INTERMISSION;
    IN_Start;
  end;
end;

//
// G_WorldDone
//
procedure G_WorldDone;
begin
  gameaction := ga_worlddone;
end;

procedure G_DoWorldDone;
begin
  gamestate := GS_LEVEL;
  G_DoLoadLevel;
  gameaction := ga_nothing;
  viewactive := true;
end;

//==========================================================================
//
// G_DoSingleReborn
//
// Called by G_Ticker based on gameaction.  Loads a game from the reborn
// save slot.
//
//==========================================================================

procedure G_DoSingleReborn;
begin
  gameaction := ga_nothing;
  SV_LoadGame(SV_GetRebornSlot);
  SB_SetClassData;
end;


//==========================================================================
//
// G_LoadGame
//
// Can be called by the startup code or the menu task.
//
//==========================================================================

var
  GameLoadSlot: integer;

procedure G_LoadGame(slot: integer);
begin
  GameLoadSlot := slot;
  gameaction := ga_loadgame;
end;

//==========================================================================
//
// G_DoLoadGame
//
// Called by G_Ticker based on gameaction.
//
//==========================================================================

procedure G_DoLoadGame;
begin
  gameaction := ga_nothing;
  SV_LoadGame(GameLoadSlot);
  if not netgame then
  begin // Copy the base slot to the reborn slot
    SV_UpdateRebornSlot;
  end;
  SB_SetClassData;
  P_LevelInfoChangeMusic;
  borderneedsrefresh := true;
end;

//==========================================================================
//
// G_SaveGame
//
// Called by the menu task.  <description> is a 24 byte text string.
//
//==========================================================================

procedure G_SaveGame(slot: integer; description: string);
begin
  savegameslot := slot;
  savedescription := description;
  sendsave := true;
end;

//==========================================================================
//
// G_DoSaveGame
//
// Called by G_Ticker based on gameaction.
//
//==========================================================================

procedure G_DoSaveGame;
begin
  SV_SaveGame(savegameslot, savedescription);
  gameaction := ga_nothing;
  savedescription := '';
  P_SetMessage(@players[consoleplayer], GGSAVED, true);
  borderneedsrefresh := true;
end;


procedure G_CmdSaveGame(const sname: string; const description: string);
var
  slot: integer;
begin
  if not usergame or (gamestate <> GS_LEVEL) then
  begin
    printf('You can''t save if you aren''t playing!'#13#10);
    exit;
  end;

  slot := atoi(sname, -1);

  if (slot < 0) or (slot >= Ord(load_end)) then
  begin
    printf('Invalid savegame slot (%s)'#13#10, [sname]);
    exit;
  end;

  G_SaveGame(slot, description);
end;

//
// G_InitNew
// Can be called by the startup code or the menu task,
// consoleplayer, displayplayer, playeringame[] should be set.
//

procedure G_DeferedInitNew(skill: skill_t; map: integer);
begin
  TempSkill := skill;
  TempEpisode := 1;
  TempMap := P_TranslateMap(map);
  gameaction := ga_initnew;
end;

procedure G_DeferedInitNewUntranslated(skill:skill_t; map: integer);
begin
  TempSkill := skill;
  TempEpisode := 1;
  TempMap := map;
  gameaction := ga_initnew;
end;

procedure G_CmdNewGame(const parm1, parm2: string);
var
  map: integer;
  mapname: string;
begin
  if (parm1 = '') or (parm2 <> '') then
  begin
    printf('Please specify the level to play'#13#10);
    exit;
  end;

  map := atoi(parm1, -1);
  if W_CheckNumForName(P_GetMapName(map)) < 0 then
  begin
    mapname := strupper(parm1);

    if length(mapname) = 5 then
      if (mapname[1] = 'M') then
        if (mapname[2] = 'A') then
          if (mapname[3] = 'P') then
            map := atoi(mapname[4] + mapname[5], -1);
  end;

  if W_CheckNumForName(P_GetMapName(map)) > -1 then
  begin
    players[consoleplayer]._message := STSTR_CLEV;
    G_DeferedInitNewUntranslated(gameskill, map);
    C_ExecuteCmd('closeconsole', '1');
  end
  else
    I_Warning('G_CmdNewGame(): Can not load map: %s.'#13#10, [parm1]);
end;

procedure G_InitNew(skill: skill_t; episode, map: integer);
var
  i: integer;
  levelinf: Plevelinfo_t;
begin
  if paused then
  begin
    paused := false;
    S_ResumeSound;
  end;

  if skill < sk_baby then
    skill := sk_baby
  else if skill > sk_nightmare then
    skill := sk_nightmare;


  if map < 1 then
    map := 1
  else if map > 99 then
    map := 99;

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

  // force players to be initialized upon first level load
  for i := 0 to MAXPLAYERS - 1 do
  begin
    players[i].playerstate := PST_REBORN;
    players[i].worldTimer := 0;
  end;

  usergame := true;  // will be set false if a demo
  paused := false;
  G_FinishedDemoPlayback;
  amstate := am_inactive;
  viewactive := true;
  gameepisode := episode;
  gamemap := map;
  gameskill := skill;

  viewactive := true;

  // Initialize the sky
  R_InitSky(map);

  G_DoLoadLevel;
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
      ga_initnew:
        G_DoInitNew;
      ga_newgame:
        G_DoNewGame;
      ga_loadgame:
        begin
          M_DrawLoadIcon;
          G_DoLoadGame;
        end;
      ga_savegame:
        begin
          M_DrawSaveIcon;
          G_DoSaveGame;
        end;
      ga_singlereborn:
        G_DoSingleReborn;
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
      ga_leavemap:
        begin
          G_DoTeleportNewMap;
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

      if netgame and not netdemo and ((gametic mod ticdup) = 0) then
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

      // turn inventory off after a certain amount of time
      if inventory then
      begin
        if inventoryTics = 0 then
        begin
          players[consoleplayer].readyArtifact := artitype_t(players[consoleplayer].inventory[inv_ptr]._type);
          inventory := false;
          pl.cmd.arti := 0;
        end;
        dec(inventoryTics);
      end;
    end;

  end;

  // do main actions
  case gamestate of
    GS_LEVEL:
      begin
        P_Ticker;
        SB_Ticker;
        AM_Ticker;
        HU_Ticker;
      end;
    GS_INTERMISSION:
      begin
        IN_Ticker;
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

initialization

  mousebuttons := PBooleanArray(@mousearray[0]);
  joybuttons := PBooleanArray(@joyarray[0]);

  KEY_WEAPONS[0] := @key_weapon0;
  KEY_WEAPONS[1] := @key_weapon1;
  KEY_WEAPONS[2] := @key_weapon2;
  KEY_WEAPONS[3] := @key_weapon3;

end.

