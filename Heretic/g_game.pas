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
//  Foundation, inc., 59 Temple Place - Suite 330, Boston, MA
//  02111-1307, USA.
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
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
  d_ticcmd,
  p_umapinfo;

//==============================================================================
// G_DeathMatchSpawnPlayer
//
// GAME
//
//==============================================================================
procedure G_DeathMatchSpawnPlayer(playernum: integer);

//==============================================================================
//
// G_InitNew
//
//==============================================================================
procedure G_InitNew(skill: skill_t; episode: integer; map: integer);

//==============================================================================
// G_DeferedInitNew
//
// Can be called by the startup code or M_Responder.
// A normal game starts at map 1,
// but a warp test can start elsewhere
//
//==============================================================================
procedure G_DeferedInitNew(skill: skill_t; episode: integer; map: integer);

//==============================================================================
//
// G_CmdNewGame
//
//==============================================================================
procedure G_CmdNewGame(const parm1, parm2: string);

//==============================================================================
//
// G_DeferedPlayDemo
//
//==============================================================================
function G_DeferedPlayDemo(const name: string): boolean;

//==============================================================================
//
// G_CmdPlayDemo
//
//==============================================================================
procedure G_CmdPlayDemo(const name: string);

{ Can be called by the startup code or M_Responder, }
{ calls P_SetupLevel or W_EnterWorld. }

//==============================================================================
//
// G_LoadGame
//
//==============================================================================
procedure G_LoadGame(const name: string);

//==============================================================================
//
// G_DoLoadGame
//
//==============================================================================
procedure G_DoLoadGame;

{ Called by M_Responder. }

//==============================================================================
//
// G_SaveGame
//
//==============================================================================
procedure G_SaveGame(slot: integer; const description: string);

//==============================================================================
//
// G_CmdSaveGame
//
//==============================================================================
procedure G_CmdSaveGame(const sname: string; const description: string);

{ Only called by startup code. }

//==============================================================================
//
// G_RecordDemo
//
//==============================================================================
procedure G_RecordDemo(const name: string);

//==============================================================================
//
// G_BeginRecording
//
//==============================================================================
procedure G_BeginRecording;

//==============================================================================
//
// G_TimeDemo
//
//==============================================================================
procedure G_TimeDemo(const name: string);

//==============================================================================
//
// G_CheckDemoStatus
//
//==============================================================================
function G_CheckDemoStatus: boolean;

//==============================================================================
//
// G_ExitLevel
//
//==============================================================================
procedure G_ExitLevel;

//==============================================================================
//
// G_SecretExitLevel
//
//==============================================================================
procedure G_SecretExitLevel;

//==============================================================================
//
// G_WorldDone
//
//==============================================================================
procedure G_WorldDone;

//==============================================================================
//
// G_Ticker
//
//==============================================================================
procedure G_Ticker;

//==============================================================================
//
// G_Responder
//
//==============================================================================
function G_Responder(ev: Pevent_t): boolean;

//==============================================================================
//
// G_ScreenShot
//
//==============================================================================
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

// JVAL 20191207 Key bindings for weapon change
  key_weapon0: integer = Ord('1');
  key_weapon1: integer = Ord('2');
  key_weapon2: integer = Ord('3');
  key_weapon3: integer = Ord('4');
  key_weapon4: integer = Ord('5');
  key_weapon5: integer = Ord('6');
  key_weapon6: integer = Ord('7');
  key_weapon7: integer = Ord('8');

  KEY_WEAPONS: array[0..Ord(NUMWEAPONS) - 1] of PInteger;

  key_invleft,
  key_invright,
  key_useartifact: integer;

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
  preparingdemoplayback: boolean = false;

  gameepisode: integer;
  gamemap: integer;
  gamemapinfo: Pmapentry_t;

  deathmatch: integer; // only if started as net death
  netgame: boolean; // only true if packets are broadcast
  playeringame: array[0..MAXPLAYERS - 1] of boolean;

  consoleplayer: integer; // player taking events and displaying
  displayplayer: integer; // view being displayed
  gametic: integer;

  totalkills, totalitems, totalsecret: integer; // for intermission

  wminfo: wbstartstruct_t; // parms for world map / intermission

  gameskill: skill_t;

  bodyqueslot: integer;

  precache: boolean; // if true, load all graphics at start

  respawnmonsters: boolean;

  viewactive: boolean;

  singledemo: boolean; // quit after playing a demo from cmdline

  demorecording: boolean = false;

  gameaction: gameaction_t;

  usergame: boolean; // ok to save / end game

//==============================================================================
//
// G_SetKeyboardMode
//
//==============================================================================
procedure G_SetKeyboardMode(const mode: integer);

//==============================================================================
//
// G_PlayerReborn
//
//==============================================================================
procedure G_PlayerReborn(player: integer);

//==============================================================================
//
// G_BuildTiccmd
//
//==============================================================================
procedure G_BuildTiccmd(cmd: Pticcmd_t);

var
  statcopy: pointer = nil;        // for statistics driver

var
  forwardmove: array[0..1] of shortint;
  sidemove: array[0..1] of shortint;
  angleturn: array[0..2] of smallint;

//==============================================================================
//
// G_NeedsCompatibilityMode
//
//==============================================================================
function G_NeedsCompatibilityMode: boolean;

//==============================================================================
//
// G_PlayingEngineVersion
//
//==============================================================================
function G_PlayingEngineVersion: byte;

//==============================================================================
//
// G_Quit
//
//==============================================================================
procedure G_Quit;

//==============================================================================
//
// G_LookupMapinfo
//
//==============================================================================
function G_LookupMapinfo(const episode, map: integer): Pmapentry_t;

//==============================================================================
//
// G_ValidateMapName
//
//==============================================================================
function G_ValidateMapName(const amap: string; const pepi, pmapnum: PInteger): boolean;

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
// HERETIC Par Times
  pars: array[1..5, 1..9] of integer;

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

var
  secretexit: boolean;
  wandstart: boolean;

implementation

uses
  c_cmds,
  d_check,
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
  e_endoom,
  m_argv,
  m_misc,
  m_menu,
  m_rnd,
  p_setup,
  p_saveg,
  p_tick,
  p_local,
  p_mobj_h,
  p_mobj,
  p_inter,
  p_map,
  p_acs,
  p_levelinfo,
  ps_main,
  in_stuff,
  hu_stuff,
  sb_bar,
  w_wad,
  s_sound,
// Data.
  h_strings,
  sounddata,
// SKY handling - still the wrong place.
  r_data,
  r_sky,
  r_defs,
  r_main,
  r_draw,
  r_intrpl,
  tables;

//==============================================================================
//
// G_ReadDemoTiccmd
//
//==============================================================================
procedure G_ReadDemoTiccmd(cmd: Pticcmd_t); forward;

//==============================================================================
//
// G_WriteDemoTiccmd
//
//==============================================================================
procedure G_WriteDemoTiccmd(cmd: Pticcmd_t); forward;

//==============================================================================
//
// G_DoReborn
//
//==============================================================================
procedure G_DoReborn(playernum: integer); forward;

//==============================================================================
//
// G_DoLoadLevel
//
//==============================================================================
procedure G_DoLoadLevel; forward;

//==============================================================================
//
// G_DoNewGame
//
//==============================================================================
procedure G_DoNewGame; forward;

//==============================================================================
//
// G_DoPlayDemo
//
//==============================================================================
procedure G_DoPlayDemo; forward;

//==============================================================================
//
// G_DoCompleted
//
//==============================================================================
procedure G_DoCompleted; forward;

//==============================================================================
//
// G_DoWorldDone
//
//==============================================================================
procedure G_DoWorldDone; forward;

//==============================================================================
//
// G_DoSaveGame
//
//==============================================================================
procedure G_DoSaveGame; forward;

//==============================================================================
//
// G_FinishedDemoPlayback
//
//==============================================================================
procedure G_FinishedDemoPlayback;
begin
  demoplayback := false;
  // Restore old compatibility mode
  compatibilitymode := oldcompatibilitymode;
end;

var
  sendsave: boolean;         // send a save event next tic
  sendcmdsave: boolean;      // send a save event next tic (console)

  timingdemo: boolean;       // if true, exit with report on completion
  starttime: integer;        // for comparative timing purposes

  levelstarttic: integer;    // gametic at level start

  demoname: string;
  netdemo: boolean;
  demobuffer: PByteArray;
  demo_p: PByteArray;
  demoend: PByte;

  consistancy: array[0..MAXPLAYERS - 1] of array[0..BACKUPTICS - 1] of smallint;

  savebuffer: PByteArray;

const
  TURBOTHRESHOLD = $32;

//==============================================================================
//
// MAXPLMOVE
//
//==============================================================================
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

var
  usearti: boolean = true;

const
  BODYQUESIZE  = 32;

var
  bodyque: array[0..BODYQUESIZE - 1] of Pmobj_t;

//==============================================================================
//
// G_CmdChecksum
//
//==============================================================================
function G_CmdChecksum(cmd: Pticcmd_t): integer;
var
  i: integer;
begin
  result := 0;
  for i := 0 to SizeOf(cmd^) div 4 - 2 do
    result := result + PIntegerArray(cmd)[i];
end;

//==============================================================================
//
// G_BuildTiccmd
// Builds a ticcmd from all of the available inputs
// or reads it from the demo buffer.
// If recording a demo, write it out
//
//==============================================================================
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
  cmd_jump, cmd_crouch: byte;
begin
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

  if G_PlayingEngineVersion >= VERSION207 then
    if players[consoleplayer].nextfire > leveltime then
      cmd.buttons := cmd.buttons and not BT_ATTACK;

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
  end;

  if gamekeydown[127] and (cmd.arti = 0) and (players[consoleplayer].powers[Ord(pw_weaponlevel2)] = 0) then
  begin
    gamekeydown[127] := false;
    cmd.arti := Ord(arti_tomeofpower);
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

//==============================================================================
//
// G_BuildTiccmd202
//
//==============================================================================
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
begin
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
  end;

  if gamekeydown[127] and (cmd.arti = 0) and (players[consoleplayer].powers[Ord(pw_weaponlevel2)] = 0) then
  begin
    gamekeydown[127] := false;
    cmd.arti := Ord(arti_tomeofpower);
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
const
  skyLumpNames: array[0..4] of string = (
    'SKY1', 'SKY2', 'SKY3', 'SKY1', 'SKY3'
  );

//==============================================================================
//
// G_DoLoadLevel
//
//==============================================================================
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

  if (gamemapinfo <> nil) and (gamemapinfo.skytexture <> '') then
  begin
    skytexture := R_TextureNumForName(gamemapinfo.skytexture);
    if skytexture < 0 then
      skytexture := R_TextureNumForName(skyLumpNames[0]);
  end
  else
  begin
    // Heretic determines the sky texture to be used
    // depending on the current episode.
    // Set the sky map
    if gameepisode > 5 then
      skytexture := R_TextureNumForName(skyLumpNames[0]) // ???
    else
    begin
      skytexture := R_CheckTextureNumForName(skyLumpNames[gameepisode - 1]);
      if skytexture < 0 then
        skytexture := R_TextureNumForName(skyLumpNames[0]);
    end;
  end;
  skytexture1 := skytexture;
  skytexture2 := -1;
  if (gamemapinfo <> nil) and (gamemapinfo.skytexture2 <> '') then
    skytexture2 := R_TextureNumForName(gamemapinfo.skytexture2);
  if skytexture2 < 0 then
    skytexture2 := skytexture;

  levelstarttic := gametic;        // for time calculation

  if wipegamestate = Ord(GS_LEVEL) then
    wipegamestate := -1;  // force a wipe

  gamestate := GS_LEVEL;

  for i := 0 to MAXPLAYERS - 1 do
  begin
    if playeringame[i] and (players[i].playerstate = PST_DEAD) then
      players[i].playerstate := PST_REBORN;
    ZeroMemory(@players[i].frags, SizeOf(players[i].frags));
  end;

  // automatic wand start when advancing from one level to the next
  if wandstart then
  begin
    if not demorecording and not demoplayback and not netgame then
      G_PlayerReborn(0)
    else if (demoplayback or netdemo) and not singledemo then
    begin
      // no-op - silently ignore pistolstart when playing demo from the
      // demo reel
    end
    else
    begin
      if demo_p = nil then
        demorecording := false;
      I_Error('The -wandstart option is not supported for demos and'#13#10' network play.');
    end;
  end;

  PS_NewMap;
  P_SetupLevel(gameepisode, gamemap, 0, gameskill);

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

//==============================================================================
//
// G_Responder
// Get info needed to make ticcmd_ts for the players.
//
//==============================================================================
function G_Responder(ev: Pevent_t): boolean;
var
  bmask: integer;
  i: integer;
  plr: Pplayer_t;
begin
  if gamestate = GS_ENDOOM then
  begin
    result := E_Responder(ev);
    exit;
  end;

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

//==============================================================================
//
// G_Ticker
// Make ticcmd_ts for the players.
//
//==============================================================================
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
        G_DoLoadGame;
      ga_savegame:
        G_DoSaveGame;
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
          players[consoleplayer].readyArtifact :=  artitype_t(players[consoleplayer].inventory[inv_ptr]._type);
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

//
// PLAYER STRUCTURE FUNCTIONS
// also see P_SpawnPlayer in P_Things
//

{

//==============================================================================
//
// G_InitPlayer
// Called at the start.
// Called by the game initialization functions.
//
//==============================================================================
procedure G_InitPlayer(player: integer);
var
  p: Pplayer_t;
begin
  // set up the saved info
  p := @players[player];

  // clear everything else to defaults
  G_PlayerReborn(player);
end;
}

//==============================================================================
//
// G_PlayerFinishLevel
// Can when a player completes a level.
//
//==============================================================================
procedure G_PlayerFinishLevel(p: Pplayer_t);
begin
  ZeroMemory(@p.powers, SizeOf(p.powers));
  ZeroMemory(@p.keys, SizeOf(p.keys));
  if p.mo <> nil then
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

//==============================================================================
//
// G_SetKeyboardMode
//
//==============================================================================
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
    key_weapon4 := Ord('5');
    key_weapon5 := Ord('6');
    key_weapon6 := Ord('7');
    key_weapon7 := Ord('8');
    AM_GOBIGKEY := Ord('o');
    AM_FOLLOWKEY := Ord('f');
    AM_GRIDKEY := Ord('g');
    AM_ROTATEKEY := Ord('r');
    AM_TEXTUREDAUTOMAP := Ord('t');
    AM_MARKKEY := Ord('m');
    AM_CLEARMARKKEY := Ord('c');
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
    key_weapon4 := Ord('5');
    key_weapon5 := Ord('6');
    key_weapon6 := Ord('7');
    key_weapon7 := Ord('8');
    AM_GOBIGKEY := Ord('o');
    AM_FOLLOWKEY := Ord('f');
    AM_GRIDKEY := Ord('g');
    AM_ROTATEKEY := Ord('r');
    AM_TEXTUREDAUTOMAP := Ord('t');
    AM_MARKKEY := Ord('m');
    AM_CLEARMARKKEY := Ord('c');
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
    key_weapon4 := Ord('5');
    key_weapon5 := Ord('6');
    key_weapon6 := Ord('7');
    key_weapon7 := Ord('8');
    AM_GOBIGKEY := Ord('o');
    AM_FOLLOWKEY := Ord('l');
    AM_GRIDKEY := Ord('g');
    AM_ROTATEKEY := Ord('r');
    AM_TEXTUREDAUTOMAP := Ord('t');
    AM_MARKKEY := Ord('m');
    AM_CLEARMARKKEY := Ord('c');
  end;
end;

//==============================================================================
//
// G_PlayerReborn
// Called after a player dies
// almost everything is cleared and initialized
//
//==============================================================================
procedure G_PlayerReborn(player: integer);
var
  p: Pplayer_t;
  i: integer;
  frags: array[0..MAXPLAYERS - 1] of integer;
  killcount: integer;
  itemcount: integer;
  secretcount: integer;
  cheats: integer;
begin
  p := @players[player];
  memcpy(@frags, @p.frags, SizeOf(frags));
  killcount := p.killcount;
  itemcount := p.itemcount;
  secretcount := p.secretcount;

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

  p.usedown := true;
  p.attackdown := true;  // don't do anything immediately
  p.playerstate := PST_LIVE;
  p.health := mobjinfo[Ord(MT_PLAYER)].spawnhealth;

  // Initial weapons
  p.readyweapon := wp_goldwand;
  p.pendingweapon := wp_goldwand;
  p.weaponowned[Ord(wp_staff)] := 1;
  p.weaponowned[Ord(wp_goldwand)] := 1;
  p.ammo[Ord(am_goldwand)] := 50;

  for i := 0 to Ord(NUMAMMO) - 1 do
    p.maxammo[i] := maxammo[i];

  if player = consoleplayer then
  begin
    SB_state := -1; // refresh the status bar
    inv_ptr := 0; // reset the inventory pointer
    curpos := 0;
  end;

end;

//==============================================================================
//
// G_CheckSpot
// Returns false if the player cannot be respawned
// at the given mapthing_t spot
// because something is occupying it
//
//==============================================================================
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
    S_StartSound(mo, Ord(sfx_telept));  // don't start sound on first frame

  result := true;
end;

//==============================================================================
//
// G_DeathMatchSpawnPlayer
// Spawns a player at one of the random death match spots
// called at level load and each death
//
//==============================================================================
procedure G_DeathMatchSpawnPlayer(playernum: integer);
var
  i, j: integer;
  selections: integer;
begin
  selections := deathmatch_p; // JVAL - deathmatchstarts;
  if selections < 4 then
    I_Error('G_DeathMatchSpawnPlayer(): Only %d deathmatch spots, 4 required', [selections]);

  for j := 0 to 19 do
  begin
    i := P_Random mod selections;
    if G_CheckSpot(playernum, @deathmatchstarts[i]) then
    begin
      deathmatchstarts[i]._type := playernum + 1;
      P_SpawnPlayer(@deathmatchstarts[i], @udeathmatchstarts[i]);
      exit;
    end;
  end;

  // no good spot, so the player will probably get stuck
  P_SpawnPlayer(@playerstarts[playernum], @udeathmatchstarts[playernum]);
end;

//==============================================================================
//
// G_DoReborn
//
//==============================================================================
procedure G_DoReborn(playernum: integer);
var
  i: integer;
begin
  if not netgame then
    // reload the level from scratch
    gameaction := ga_loadlevel
  else
  begin
    // respawn at the start

    // first dissasociate the corpse
    players[playernum].mo.player := nil;

    // spawn at random spot if in death match
    if deathmatch <> 0 then
    begin
      G_DeathMatchSpawnPlayer(playernum);
      exit;
    end;

    if G_CheckSpot(playernum, @playerstarts[playernum]) then
    begin
      P_SpawnPlayer(@playerstarts[playernum], @udeathmatchstarts[playernum]);
      exit;
    end;

    // try to spawn at one of the other players spots
    for i := 0 to MAXPLAYERS - 1 do
    begin
      if G_CheckSpot (playernum, @playerstarts[i]) then
      begin
        playerstarts[i]._type := playernum + 1; // fake as other player
        P_SpawnPlayer(@playerstarts[i], @udeathmatchstarts[i]);
        playerstarts[i]._type := i + 1; // restore
        exit;
      end;
      // he's going to be inside something.  Too bad.
    end;
    P_SpawnPlayer(@playerstarts[playernum], @udeathmatchstarts[playernum]);
  end;
end;

//==============================================================================
//
// G_ScreenShot
//
//==============================================================================
procedure G_ScreenShot;
begin
  gameaction := ga_screenshot;
end;

//==============================================================================
// G_ExitLevel
//
// G_DoCompleted
//
//==============================================================================
procedure G_ExitLevel;
begin
  secretexit := false;
  gameaction := ga_completed;
end;

//==============================================================================
// G_SecretExitLevel
//
// Here's for the german edition.
//
//==============================================================================
procedure G_SecretExitLevel;
begin
  secretexit := true;
  gameaction := ga_completed;
end;

//==============================================================================
//
// G_DoCompleted
//
//==============================================================================
procedure G_DoCompleted;
var
  i: integer;
  next: string;
label
  frommapinfo;
begin
  gameaction := ga_nothing;

  for i := 0 to MAXPLAYERS - 1 do
    if playeringame[i] then
      G_PlayerFinishLevel(@players[i]); // take away cards and stuff

  if amstate <> am_inactive then
  begin
    amstate := am_inactive;
    AM_Stop;
  end;

  case gamemap of
    3:
      begin
        if customgame = cg_beta then
        begin
          gameaction := ga_victory;
          exit;
        end;
      end;
    8:
      begin
        gameaction := ga_victory;
        exit;
      end;
    9:
      begin
        for i := 0 to MAXPLAYERS - 1 do
          players[i].didsecret := true;
      end;
  end;

  wminfo.didsecret := players[consoleplayer].didsecret;
  wminfo.epsd := gameepisode - 1;
  wminfo.nextep := gameepisode - 1;
  wminfo.last := gamemap - 1;

  wminfo.lastmapinfo := gamemapinfo;
  wminfo.nextmapinfo := nil;
  if gamemapinfo <> nil then
  begin
    if (gamemapinfo.endpic <> '') and (gamemapinfo.endpic <> '-') and gamemapinfo.nointermission then
    begin
      gameaction := ga_victory;
      exit;
    end;
    next := '';
    if secretexit then
      next := gamemapinfo.nextsecret;
    if next = '' then
      next := gamemapinfo.nextmap;
    if next <> '' then
      if G_ValidateMapName(next, @wminfo.nextep, @wminfo.next) then
      begin
        Dec(wminfo.nextep);
        Dec(wminfo.next);
        // episode change
        if wminfo.nextep <> wminfo.epsd then
          for i := 0 to MAXPLAYERS - 1 do
            players[i].didsecret := false;
        wminfo.didsecret := players[consoleplayer].didsecret;
        wminfo.partime := gamemapinfo.partime;
        goto frommapinfo; // skip past the default setup.
      end;
  end;

  if secretexit then
    wminfo.next := 8  // go to secret level
  else if gamemap = 9 then
  begin
    // returning from secret level
    case gameepisode of
      1: wminfo.next := 7;
      2: wminfo.next := 5;
      3: wminfo.next := 5;
      4: wminfo.next := 5;
      5: wminfo.next := 4;
    end
  end
  else
    wminfo.next := gamemap; // go to next level

  wminfo.partime := TICRATE * pars[gameepisode][gamemap];

frommapinfo:
  wminfo.nextmapinfo := G_LookupMapinfo(wminfo.nextep + 1, wminfo.next + 1);

  wminfo.maxkills := totalkills;
  wminfo.maxitems := totalitems;
  wminfo.maxsecret := totalsecret;
  wminfo.maxfrags := 0;

  wminfo.pnum := consoleplayer;

  for i := 0 to MAXPLAYERS - 1 do
  begin
    wminfo.plyr[i]._in := playeringame[i];
    wminfo.plyr[i].skills := players[i].killcount;
    wminfo.plyr[i].sitems := players[i].itemcount;
    wminfo.plyr[i].ssecret := players[i].secretcount;
    wminfo.plyr[i].stime := leveltime;
    memcpy(@wminfo.plyr[i].frags, @players[i].frags, SizeOf(wminfo.plyr[i].frags));
  end;

  gamestate := GS_INTERMISSION;
  viewactive := false;
  amstate := am_inactive;

  if statcopy <> nil then
    memcpy(statcopy, @wminfo, SizeOf(wminfo));

  IN_Start(@wminfo);
end;

//==============================================================================
//
// G_WorldDone
//
//==============================================================================
procedure G_WorldDone;
begin
  gameaction := ga_worlddone;

  if secretexit then
    players[consoleplayer].didsecret := true;

  if gamemapinfo <> nil then
  begin
    if (gamemapinfo.intertextsecret[0] <> #0) and secretexit then
    begin
      if gamemapinfo.intertextsecret[0] <> '-' then // '-' means that any default intermission was cleared.
        F_StartFinale;
      exit;
    end
    else if (gamemapinfo.intertext[0] <> #0) and not secretexit then
    begin
      if gamemapinfo.intertext[0] <> '-' then // '-' means that any default intermission was cleared.
        F_StartFinale;
      exit;
    end
    else if (gamemapinfo.endpic <> '') and (gamemapinfo.endpic <> '-') then
    begin
      // game ends without a status screen.
      gameaction := ga_victory;
      exit;
    end;
    // if nothing applied, use the defaults.
  end;

end;

//==============================================================================
//
// G_DoWorldDone
//
//==============================================================================
procedure G_DoWorldDone;
begin
  gamestate := GS_LEVEL;
  gameepisode := wminfo.nextep + 1;
  gamemap := wminfo.next + 1;
  gamemapinfo := G_LookupMapinfo(gameepisode, gamemap);
  G_DoLoadLevel;
  gameaction := ga_nothing;
  viewactive := true;
end;

//==============================================================================
//
// G_GetSaveName
//
//==============================================================================
function G_GetSaveName(name: string): string;
var
  i: integer;
begin
  result := '';
  for i := 0 to Ord(load_end) - 1 do
  begin
    if name = itoa(i) then
    begin
      sprintf(result, D_GetSavePath + SAVEGAMENAME + '%s.dsg', [name]);
      exit;
    end;
  end;

  if strupper(Copy(name, length(name) - 3, 4)) <> '.DSG' then
    result := name + '.DSG'
  else
    result := name;
end;

//
// G_InitFromSavegame
// Can be called by the startup code or the menu task.
//
var
  savename: string;

//==============================================================================
//
// G_LoadGame
//
//==============================================================================
procedure G_LoadGame(const name: string);
begin
  savename := name;
  gameaction := ga_loadgame;
end;

const
  VERSIONSIZE = 16;

//==============================================================================
//
// G_DoLoadGame
//
//==============================================================================
procedure G_DoLoadGame;
var
  len: integer;
  i, j: integer;
  a, b, c: integer;
  vcheck: string;
  vsaved: string;
begin
  gameaction := ga_nothing;

  savename := G_GetSaveName(savename);
  if not fexists(savename) then
  begin
    I_Warning('Can not open %s'#13#10, [savename]);
    exit;
  end;

  len := M_ReadFile(savename, pointer(savebuffer));
  save_p := PByteArray(integer(savebuffer) + SAVESTRINGSIZE);

  savegameversion := VERSION; // Assume current version

  // skip the description field
  vcheck := '';
  sprintf(vcheck, 'heretic %d', [VERSION]);

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
      if vsaved = 'version 110' then
        savegameversion := 110
      else if vsaved = 'heretic 111' then
        savegameversion := 111
      else if vsaved = 'heretic 112' then
        savegameversion := 112
      else if vsaved = 'heretic 113' then
        savegameversion := 113
      else if vsaved = 'heretic 114' then
        savegameversion := 114
      else if vsaved = 'heretic 115' then
        savegameversion := 115
      else if vsaved = 'heretic 203' then
        savegameversion := 203
      else if vsaved = 'heretic 204' then
        savegameversion := 204
      else if vsaved = 'heretic 205' then
        savegameversion := 205
      else if vsaved = 'heretic 206' then
        savegameversion := 206
      else
      begin
        I_Warning('G_DoLoadGame(): Saved game is from an unsupported version: %s!'#13#10, [vsaved]);
        Z_Free(savebuffer);
        exit; // bad version
      end;
    end;

  save_p := PByteArray(integer(save_p) + VERSIONSIZE);

  P_UnArchiveScreenShot;

  gameskill := skill_t(save_p[0]);
  defaultskill := save_p[0];
  save_p := PByteArray(integer(save_p) + 1);

  gameepisode := save_p[0];
  save_p := PByteArray(integer(save_p) + 1);

  gamemap := save_p[0];
  save_p := PByteArray(integer(save_p) + 1);

  gamemapinfo := G_LookupMapinfo(gameepisode, gamemap);

  for i := 0 to MAXPLAYERS - 1 do
  begin
    playeringame[i] := save_p[0] <> 0;
    save_p := PByteArray(integer(save_p) + 1);
  end;

  // load a base level
  G_InitNew(gameskill, gameepisode, gamemap);

  // get the times
  a := save_p[0];
  save_p := PByteArray(integer(save_p) + 1);

  b := save_p[0];
  save_p := PByteArray(integer(save_p) + 1);

  c := save_p[0];
  save_p := PByteArray(integer(save_p) + 1);

  leveltime := _SHL(a, 16) + _SHL(b, 8) + c;

  // dearchive all the modifications
  P_UnArchivePlayers;
  P_UnArchiveWorld;
  P_UnArchiveThinkers;
  P_UnArchiveSpecials;
  P_UnArchiveVariables;
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

//==============================================================================
//
// G_SaveGame
// Called by the menu task.
// Description is a 24 byte text string
//
//==============================================================================
procedure G_SaveGame(slot: integer; const description: string);
begin
  savegameslot := slot;
  savedescription := description;
  sendsave := true;
end;

//==============================================================================
//
// G_DoSaveGameInFile
//
//==============================================================================
procedure G_DoSaveGameInFile(name: string);
var
  name2: string;
  description: string;
  len: integer;
  i: integer;
  maxsize: integer;
begin
  name := G_GetSaveName(name);

  description := savedescription;

  maxsize := SAVEGAMESIZE + PS_MapScriptSaveSize;
  repeat
    savebuffer := PByteArray(Z_Malloc2(maxsize, PU_STATIC, nil));
    if savebuffer = nil then
      maxsize := maxsize * 3 div 4;
  until savebuffer <> nil;

  save_p := savebuffer;

  memcpy(save_p, @description[1], SAVESTRINGSIZE);

  save_p := PByteArray(integer(save_p) + SAVESTRINGSIZE);
  name2 := '';

  // JVAL
  // Hack:
  //    Strings 'heretic' and 'version' have the same length!!
  savegameversion := VERSION;
  sprintf(name2, 'heretic %d', [VERSION]);
  while length(name2) < VERSIONSIZE do
    name2 := name2 + ' ';

  memcpy(save_p, @name2[1], VERSIONSIZE);
  save_p := PByteArray(integer(save_p) + VERSIONSIZE);
  P_ArchiveScreenShot;

  save_p[0] := Ord(gameskill);
  save_p := PByteArray(integer(save_p) + 1);

  save_p[0] := gameepisode;
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
  M_WriteFile(name, savebuffer, len);
  save_p := savebuffer;

  P_ArchivePlayers;

  len := integer(save_p) - integer(savebuffer);
  M_AppendFile(name, savebuffer, len);
  save_p := savebuffer;

  P_ArchiveWorld;

  len := integer(save_p) - integer(savebuffer);
  M_AppendFile(name, savebuffer, len);
  save_p := savebuffer;

  P_ArchiveThinkers;

  len := integer(save_p) - integer(savebuffer);
  M_AppendFile(name, savebuffer, len);
  save_p := savebuffer;

  P_ArchiveSpecials;

  len := integer(save_p) - integer(savebuffer);
  M_AppendFile(name, savebuffer, len);
  save_p := savebuffer;

  P_ArchiveVariables;

  len := integer(save_p) - integer(savebuffer);
  M_AppendFile(name, savebuffer, len);
  save_p := savebuffer;

  P_ArchivePSMapScript;

  len := integer(save_p) - integer(savebuffer);
  M_AppendFile(name, savebuffer, len);
  save_p := savebuffer;

  P_ArchiveOverlay;

  save_p[0] := $1d; // consistancy marker

  len := integer(save_p) - integer(savebuffer) + 1;
  if len > maxsize then
    I_Error('G_DoSaveGame(): Savegame buffer overrun');
  M_AppendFile(name, savebuffer, len);

  Z_Free(savebuffer);
  gameaction := ga_nothing;
  savedescription := '';

  players[consoleplayer]._message := GGSAVED;

  // draw the pattern into the back screen
{$IFNDEF OPENGL}
  R_FillBackScreen;
{$ENDIF}
end;

//==============================================================================
//
// G_DoSaveGame
//
//==============================================================================
procedure G_DoSaveGame;
var
  name: string;
begin
  if savegameslot = Ord(load_end) then
    name := savename
  else
  begin
    sprintf(name, M_SaveFileName(D_GetSavePath + SAVEGAMENAME) + '%d.dsg', [savegameslot]);
  end;

  G_DoSaveGameInFile(name);

end;

//==============================================================================
//
// G_CmdSaveGame
//
//==============================================================================
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
  d_episode: integer;
  d_map: integer;

//==============================================================================
//
// G_DeferedInitNew
//
//==============================================================================
procedure G_DeferedInitNew(skill: skill_t; episode, map: integer);
begin
  d_skill := skill;
  d_episode := episode;
  d_map := map;
  gameaction := ga_newgame;
end;

//==============================================================================
//
// G_CmdNewGame
//
//==============================================================================
procedure G_CmdNewGame(const parm1, parm2: string);
var
  epsd, map: integer;
  mapname: string;
begin
  if parm1 = '' then
  begin
    printf('Please specify the level to play'#13#10);
    exit;
  end;

  mapname := strupper(parm1);

  epsd := atoi(parm1);
  map := atoi(parm2);
  if parm2 = '' then
    if (mapname[1] = 'E') then
      if length(mapname) = 4 then
        if (mapname[3] = 'M') then
        begin
          epsd := atoi(mapname[2]);
          map := atoi(mapname[4]);
        end;

  if W_CheckNumForName(P_GetMapName(epsd, map)) > -1 then
  begin
    players[consoleplayer]._message := STSTR_CLEV;
    G_DeferedInitNew(gameskill, epsd, map);
    C_ExecuteCmd('closeconsole', '1');
  end
  else
    I_Warning('G_CmdNewGame(): Can not load map.'#13#10);
end;

//==============================================================================
//
// G_DoNewGame
//
//==============================================================================
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
  nomonsters := false;
  consoleplayer := 0;
  G_InitNew(d_skill, d_episode, d_map);
  gameaction := ga_nothing;
end;

//==============================================================================
//
// G_InitNew
//
//==============================================================================
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

  if skill > sk_nightmare then
    skill := sk_nightmare;

  if episode < 1 then
    episode := 1;

  if map < 1 then
    map := 1
  else if map > 9 then
    map := 9;

  if not EpiCustom and (W_CheckNumForName(P_GetMapName(episode, map)) < 0) then
  begin
    if gamemode = shareware then
    begin
      if episode > 1 then
       episode := 1;  // only start episode 1 on shareware
    end
    else
    begin
      if gamemode = extendedwad then
      begin
        if episode > 5 then
          episode := 5;
      end
      else
      begin
        if episode > 3 then
          episode := 3;
      end;
    end;
  end;

  levelinf := P_GetLevelInfo(P_GetMapName(episode, map));
  levelinf.musname := stringtochar8('');
  levelinf.skyflat := stringtochar8('');

  R_ResetInterpolationBuffer;

  M_ClearRandom;
  PS_NewWorld;
  P_ACSInitNewGame;

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
    players[i].playerstate := PST_REBORN;

  usergame := true;  // will be set false if a demo
  paused := false;
  G_FinishedDemoPlayback;
  amstate := am_inactive;
  viewactive := true;
  gameepisode := episode;
  gamemap := map;
  gameskill := skill;
  gamemapinfo := G_LookupMapinfo(gameepisode, gamemap);

  viewactive := true;

  G_DoLoadLevel;
end;

var
  demoversion: byte;

//
// DEMO PLAYBACK
//
const
  DEMOMARKER = $80;

var
  compatibility_done: boolean;

//==============================================================================
//
// G_ReadDemoTiccmd
//
//==============================================================================
procedure G_ReadDemoTiccmd(cmd: Pticcmd_t);
begin
  if demo_p[0] = DEMOMARKER then
  begin
    // end of demo data stream
    G_CheckDemoStatus;
    exit;
  end;

  cmd.forwardmove := shortint(demo_p[0]);
  demo_p := @demo_p[1];

  cmd.sidemove := shortint(demo_p[0]);
  demo_p := @demo_p[1];

  cmd.angleturn := PSmallInt(demo_p)^;
  demo_p := @demo_p[2];

  cmd.buttons := demo_p[0] and not BT_SPECIAL;
  demo_p := @demo_p[1];

  cmd.lookfly := demo_p[0];
  demo_p := @demo_p[1];

  cmd.arti := demo_p[0];
  demo_p := @demo_p[1];

  cmd.lookleftright := demo_p[0];
  demo_p := @demo_p[1];

  cmd.jump_crouch := demo_p[0];
  demo_p := @demo_p[1];

  if demoversion < VERSION203 then
  begin
    cmd.lookupdown16 := (cmd.lookfly and 15) * 256; // JVAL Smooth Look Up/Down
  end
  else
  begin
    cmd.lookupdown16 := PWord(demo_p)^;
    demo_p := @demo_p[2];
  end;
  cmd.lookupdown := cmd.lookupdown16 div 256; // JVAL unused :)
end;

//==============================================================================
// G_IncreaseDemoBuffer
//
// DEMO RECORDING
//
// Increase the size of the demo buffer to allow unlimited demos
//
//==============================================================================
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

//==============================================================================
//
// G_WriteDemoTiccmd
//
//==============================================================================
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

  demo_p[0] := cmd.lookfly;
  demo_p := @demo_p[1];

  demo_p[0] := cmd.arti;
  demo_p := @demo_p[1];

  demo_p[0] := cmd.lookleftright;
  demo_p := @demo_p[1];

  demo_p[0] := cmd.jump_crouch;
  demo_p := @demo_p[1];

  // JVAL Smooth Look Up/Down
  PWord(demo_p)^ := cmd.lookupdown16;

  demo_p := demo_start;

  if integer(demo_p) >= integer(demoend) - 2 * SizeOf(ticcmd_t) then
    G_IncreaseDemoBuffer;

  G_ReadDemoTiccmd(cmd);  // make SURE it is exactly the same
end;

//==============================================================================
//
// G_RecordDemo
//
//==============================================================================
procedure G_RecordDemo(const name: string);
var
  i: integer;
  maxsize: integer;
begin
  usergame := false;
  demoname := name;
  if CharPos('.', demoname) = 0 then
    demoname := demoname + '.lmp';

  i := M_CheckParm ('-maxdemo');
  if (i <> 0) and (i < myargc - 1) then
    maxsize := atoi(myargv[i + 1]) * 1024
  else
    maxsize := $80000; // JVAL Originally was $20000

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
  DEMOHDR: integer = $4D454448; // JVAL: HDEM in hex

//==============================================================================
//
// G_BeginRecording
//
//==============================================================================
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

  demo_p[0] := gameepisode;
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

//==============================================================================
//
// G_DoPlayExternalDemo
//
//==============================================================================
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

//==============================================================================
//
// G_DeferedPlayDemo
//
//==============================================================================
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

  pdot := CharPos('.', dmname);
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

//==============================================================================
//
// G_CmdPlayDemo
//
//==============================================================================
procedure G_CmdPlayDemo(const name: string);
begin
  if G_DeferedPlayDemo(name) then
    C_ExecuteCmd('closeconsole', '1');
end;

//==============================================================================
//
// G_DoPlayDemo
//
//==============================================================================
procedure G_DoPlayDemo;
var
  skill: skill_t;
  i, episode, map: integer;
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

  if PInteger(demo_p)^ = DEMOHDR then
  begin
    demo_p := @demo_p[4];
    demoversion := demo_p[0];
  end
  else
  begin
    demoversion := demobuffer[0];
  end;

  if demoversion < VERSION110 then
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

  episode := demo_p[0];
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
  if demoversion >= VERSION113 then
  begin
    sysrndseed := demo_p[0];
    demo_p := @demo_p[1];
  end;

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
  G_InitNew(skill, episode, map);
  preparingdemoplayback := false;
  spawnrandommonsters := oldspawnrandommonsters;  // Back to default
  precache := true;
  usergame := false;
  demoplayback := true;
  Info_Init(true); // JVAL: Start thinkers

  compatibility_done := false;

end;

//==============================================================================
//
// G_TimeDemo
//
//==============================================================================
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

//==============================================================================
//
// G_CheckDemoStatus
//
//==============================================================================
function G_CheckDemoStatus: boolean;
var
  endtime: integer;
  i: integer;
begin
  if timingdemo then
  begin
    endtime := I_GetTime;
    I_Error('G_CheckDemoStatus(): timed %d gametics in %d realtics', [gametic, endtime - starttime]);
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

//==============================================================================
//
// G_NeedsCompatibilityMode
//
//==============================================================================
function G_NeedsCompatibilityMode: boolean;
begin
  result := compatibilitymode;
end;

//==============================================================================
//
// G_PlayingEngineVersion
//
//==============================================================================
function G_PlayingEngineVersion: byte;
begin
  if demoplayback then
    result := demoversion
  else
    result := VERSION;
end;

//==============================================================================
//
// G_Quit
//
//==============================================================================
procedure G_Quit;
begin
  if displayendscreen then
  begin
    gamestate := GS_ENDOOM;
    S_PauseSound; // Stop music in ENDOOM screen
    printf('E_Init: Initializing ENDTEXT screen.'#13#10, [mb_used]);
    E_Init;
  end
  else
    I_Quit;
end;

//==============================================================================
//
// G_LookupMapinfo
//
//==============================================================================
function G_LookupMapinfo(const episode, map: integer): Pmapentry_t;
var
  lumpname: string;
  i: integer;
begin
  sprintf(lumpname, 'E%dM%d', [episode, map]);
  for i := 0 to u_mapinfo.mapcount - 1 do
    if lumpname = u_mapinfo.maps[i].mapname then
    begin
      result := @u_mapinfo.maps[i];
      exit;
    end;

  for i := 0 to default_mapinfo.mapcount - 1 do
    if lumpname = default_mapinfo.maps[i].mapname then
    begin
      result := @default_mapinfo.maps[i];
      exit;
    end;

  result := nil;
end;

//==============================================================================
//
// G_ValidateMapName
//
//==============================================================================
function G_ValidateMapName(const amap: string; const pepi, pmapnum: PInteger): boolean;
var
  epi, mapnum: integer;
  len: integer;
  map: string;
begin
  result := false;
  len := Length(amap);
  if len <> 4 then
    exit;
  epi := 0;
  mapnum := 0;
  map := strupper(amap);
  if (map[1] = 'E') and (map[3] = 'M') then
  begin
    if not isdigit(map[2]) then
      exit;
    epi := atoi(map[2]);
    mapnum := atoi(map[4]);
    result := IsIntegerInRange(epi, 1, 8) and IsIntegerInRange(mapnum, 1, 9);
  end;

  if result then
  begin
    if pepi <> nil then
      pepi^ := epi;
    if pmapnum <> nil then
      pmapnum^ := mapnum;
  end;
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

  ZeroMemory(@pars, SizeOf(pars));

  pars[1, 1] := 30;
  pars[1, 2] := 75;
  pars[1, 3] := 120;
  pars[1, 4] := 90;
  pars[1, 5] := 165;
  pars[1, 6] := 180;
  pars[1, 7] := 180;
  pars[1, 8] := 30;
  pars[1, 9] := 165;

  pars[2, 1] := 90;
  pars[2, 2] := 90;
  pars[2, 3] := 90;
  pars[2, 4] := 120;
  pars[2, 5] := 90;
  pars[2, 6] := 360;
  pars[2, 7] := 240;
  pars[2, 8] := 30;
  pars[2, 9] := 170;

  pars[3, 1] := 90;
  pars[3, 2] := 45;
  pars[3, 3] := 90;
  pars[3, 4] := 150;
  pars[3, 5] := 90;
  pars[3, 6] := 90;
  pars[3, 7] := 165;
  pars[3, 8] := 30;
  pars[3, 9] := 135;

  precache := true;

  KEY_WEAPONS[0] := @key_weapon0;
  KEY_WEAPONS[1] := @key_weapon1;
  KEY_WEAPONS[2] := @key_weapon2;
  KEY_WEAPONS[3] := @key_weapon3;
  KEY_WEAPONS[4] := @key_weapon4;
  KEY_WEAPONS[5] := @key_weapon5;
  KEY_WEAPONS[6] := @key_weapon6;
  KEY_WEAPONS[7] := @key_weapon7;

end.

