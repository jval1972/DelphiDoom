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

unit sb_bar;

interface

uses
  d_event;

// Size of statusbar.
// Now sensitive for scaling.
const
  SB_HEIGHT = 38;
  SB_WIDTH = 320;
  SB_Y = 200 - SB_HEIGHT;

// Public Data

var
  DebugSound: boolean; // debug flag for displaying sound info

  inventory: boolean;
  curpos: integer;
  inv_ptr: integer;
  ArtifactFlash: integer;

//==============================================================================
//
// SB_Responder
//
//==============================================================================
function SB_Responder(ev: Pevent_t): boolean;

//==============================================================================
//
// SB_Ticker
//
//==============================================================================
procedure SB_Ticker;

//==============================================================================
//
// SB_Init
//
//==============================================================================
procedure SB_Init;

//==============================================================================
//
// SB_Drawer
//
//==============================================================================
procedure SB_Drawer;

//==============================================================================
//
// SB_SetClassData
//
//==============================================================================
procedure SB_SetClassData;

//==============================================================================
//
// SB_PaletteFlash
//
//==============================================================================
procedure SB_PaletteFlash(forceChange: boolean);

var
  sbiconsactive: boolean = false;

implementation

uses
  d_delphi,
  doomdef,
  doomstat,
  d_net,
  am_map,
  c_cmds,
  d_player,
  g_game,
  g_demo,
  hu_stuff,
  xn_strings,
{$IFDEF OPENGL}
  gl_main,
  gl_render,
{$ELSE}
  i_video,
{$ENDIF}
  info,
  info_h,
  m_rnd,
  m_fixed,
  m_cheat,
  mt_utils,
  p_tick,
  p_pspr_h,
  p_local,
  p_mobj_h,
  p_setup,
  p_enemy,
  p_inter,
  p_user,
  p_pspr,
  r_defs,
  r_data,
  r_draw,
{$IFNDEF OPENGL}
  r_hires,
{$ENDIF}
  r_main,
  s_sound,
  sounddata,
  v_data,
  v_video,
  w_wad,
  z_zone;

// Private Data

var
  HealthMarker: integer;
  CPlayer: Pplayer_t;
  playpalette: integer;

  PatchH2BAR: Ppatch_t;
  PatchH2TOP: Ppatch_t;
  PatchLFEDGE: Ppatch_t;
  PatchRTEDGE: Ppatch_t;
  PatchARMCLEAR: Ppatch_t;
  PatchARTICLEAR: Ppatch_t;
  PatchMANACLEAR: Ppatch_t;
  PatchKILLS: Ppatch_t;
  PatchMANAVIAL1: Ppatch_t;
  PatchMANAVIAL2: Ppatch_t;
  PatchMANAVIALDIM1: Ppatch_t;
  PatchMANAVIALDIM2: Ppatch_t;
  PatchMANADIM1: Ppatch_t;
  PatchMANADIM2: Ppatch_t;
  PatchMANABRIGHT1: Ppatch_t;
  PatchMANABRIGHT2: Ppatch_t;
  PatchCHAIN: Ppatch_t;
  PatchSTATBAR: Ppatch_t;
  PatchKEYBAR: Ppatch_t;
  PatchLIFEGEM: Ppatch_t;
  PatchSELECTBOX: Ppatch_t;
  PatchINumbers: array[0..9] of Ppatch_t;
  PatchNEGATIVE: Ppatch_t;
  PatchSmNumbers: array[0..9] of Ppatch_t;
  PatchINVBAR: Ppatch_t;
  PatchWEAPONSLOT: Ppatch_t;
  PatchWEAPONFULL: Ppatch_t;
  PatchPIECE1: Ppatch_t;
  PatchPIECE2: Ppatch_t;
  PatchPIECE3: Ppatch_t;
  PatchINVLFGEM1: Ppatch_t;
  PatchINVLFGEM2: Ppatch_t;
  PatchINVRTGEM1: Ppatch_t;
  PatchINVRTGEM2: Ppatch_t;

  FontBNumBase: integer;
  SpinFlylump: integer;
  SpinMinotaurLump: integer;
  SpinSpeedLump: integer;
  SpinDefenseLump: integer;

var
  lmp_useartia: integer;
  lmp_keyslot1: integer;
  lmp_armslot1: integer;

//==============================================================================
// SB_CmdCheckPlayerStatus
//
// Commands
//
//==============================================================================
function SB_CmdCheckPlayerStatus: boolean;
begin
  if (CPlayer = nil) or (CPlayer.mo = nil) or (CPlayer.mo = nil) or
     (gamestate <> GS_LEVEL) or demoplayback or netgame then
  begin
    printf('You can''t specify the command at this time.'#13#10);
    result := false;
  end
  else
    result := true;
end;

//==============================================================================
//
// SB_CmdGod
//
//==============================================================================
procedure SB_CmdGod;
begin
  if not SB_CmdCheckPlayerStatus then
    exit;

  if CPlayer.playerstate <> PST_DEAD then
  begin
    CPlayer.cheats := CPlayer.cheats xor CF_GODMODE;
    if CPlayer.cheats and CF_GODMODE <> 0 then
    begin
      CPlayer.health := P_MaxPlayerHealth(CPlayer);

      if CPlayer.mo <> nil then
        CPlayer.mo.health := CPlayer.health;

      CPlayer._message := TXT_CHEATGODON;
    end
    else
      CPlayer._message := TXT_CHEATGODOFF;
  end
  else
  begin
    C_ExecuteCmd('closeconsole');
    CPlayer.playerstate := PST_REBORN;
  end;
end;

//==============================================================================
//
// SB_CmdIddqd
//
//==============================================================================
procedure SB_CmdIddqd;
begin
  if not SB_CmdCheckPlayerStatus then
    exit;

  if CPlayer.mo.health >= 0 then
  begin
    P_DamageMobj(CPlayer.mo, nil, CPlayer.mo, 10000);
    P_SetMessage(CPlayer, TXT_CHEATIDDQD, true);
  end;
end;

//==============================================================================
//
// SB_CmdLowGravity
//
//==============================================================================
procedure SB_CmdLowGravity;
begin
  if not SB_CmdCheckPlayerStatus then
    exit;

  CPlayer.cheats := CPlayer.cheats xor CF_LOWGRAVITY;
  if CPlayer.cheats and CF_LOWGRAVITY <> 0 then
    CPlayer._message := STSTR_LGON
  else
    CPlayer._message := STSTR_LGOFF;
end;

//==============================================================================
//
// SB_CmdIDFA
//
//==============================================================================
procedure SB_CmdIDFA;
var
  i: integer;
begin
  if not SB_CmdCheckPlayerStatus then
    exit;

  for i := 0 to Ord(NUMARMOR) - 1 do
    CPlayer.armorpoints[i] := ArmorIncrement[Ord(CPlayer._class), i];

  for i := 0 to Ord(NUMWEAPONS) - 1 do
    CPlayer.weaponowned[i] := true;

  for i := 0 to Ord(NUMMANA) - 1 do
    CPlayer.mana[i] := MAX_MANA;

  CPlayer._message := TXT_CHEATWEAPONS;
end;

//==============================================================================
//
// SB_CmdIDKFA
//
//==============================================================================
procedure SB_CmdIDKFA;
var
  i: integer;
begin
  if not SB_CmdCheckPlayerStatus then
    exit;

  for i := 0 to Ord(NUMARMOR) - 1 do
    CPlayer.armorpoints[i] := ArmorIncrement[Ord(CPlayer._class), i];

  for i := 0 to Ord(NUMWEAPONS) - 1 do
    CPlayer.weaponowned[i] := true;

  for i := 0 to Ord(NUMMANA) - 1 do
    CPlayer.mana[i] := MAX_MANA;

  CPlayer.keys := 2047;

  CPlayer._message := TXT_CHEATWEAPONS;
end;

//==============================================================================
//
// SB_CmdSkel
//
//==============================================================================
procedure SB_CmdSkel;
begin
  if not SB_CmdCheckPlayerStatus then
    exit;

  CPlayer.keys := 2047;
  CPlayer._message := TXT_CHEATKEYS;
end;

//==============================================================================
//
// SB_CmdIDDT
//
//==============================================================================
procedure SB_CmdIDDT;
begin
  if not SB_CmdCheckPlayerStatus then
    exit;

  am_cheating := (am_cheating + 1) mod 3;
end;

//==============================================================================
//
// SB_CmdIDNoClip
//
//==============================================================================
procedure SB_CmdIDNoClip;
begin
  if not SB_CmdCheckPlayerStatus then
    exit;

  CPlayer.cheats := CPlayer.cheats xor CF_NOCLIP;

  if CPlayer.cheats and CF_NOCLIP <> 0 then
    CPlayer._message := STSTR_NCON
  else
    CPlayer._message := STSTR_NCOFF;
end;

//==============================================================================
//
// SB_CmdMassacre
//
//==============================================================================
procedure SB_CmdMassacre;
var
  count: integer;
begin
  if not SB_CmdCheckPlayerStatus then
    exit;

  count := P_Massacre;
  CPlayer._message := itoa(count) + ' ' + TXT_CHEATMASSACRE;
end;

//==============================================================================
//
// SB_CmdPonce
//
//==============================================================================
procedure SB_CmdPonce;
begin
  if not SB_CmdCheckPlayerStatus then
    exit;

  CPlayer.health := P_MaxPlayerHealth(CPlayer);
  if CPlayer.mo <> nil then
    CPlayer.mo.health := CPlayer.health;

  P_SetMessage(CPlayer, TXT_CHEATHEALTH, false);
end;

//==============================================================================
//
// SB_CmdPig
//
//==============================================================================
procedure SB_CmdPig;
begin
  if not SB_CmdCheckPlayerStatus then
    exit;

  if CPlayer.morphTics > 0 then
    P_UndoPlayerMorph(CPlayer)
  else
     P_MorphPlayer(CPlayer);
  P_SetMessage(CPlayer, TXT_CHEATPIG, true);
end;

//==============================================================================
//
// SB_CmdChangeClass
//
//==============================================================================
procedure SB_CmdChangeClass(const parm: string);
var
  i: integer;
  _class: integer;
  cl: string;
begin
  if not SB_CmdCheckPlayerStatus then
    exit;

  if CPlayer.morphTics > 0 then
  begin // don't change class if the player is morphed
    exit;
  end;

  cl := strupper(parm);
  if cl = 'FIGHTER' then
    _class := Ord(PCLASS_FIGHTER)
  else if cl = 'CLERIC' then
    _class := Ord(PCLASS_CLERIC)
  else if cl = 'MAGE' then
    _class := Ord(PCLASS_MAGE)
  else
  begin
    _class := atoi(cl, -1);
    if (_class < 0) or (_class > 2) then
    begin
      P_SetMessage(CPlayer, 'INVALID PLAYER CLASS: ' + parm, true);
      exit;
    end;
  end;

  CPlayer._class := pclass_t(_class);
  for i := 0 to Ord(NUMARMOR) - 1 do
    CPlayer.armorpoints[i] := 0;

  PlayerClass[consoleplayer] := pclass_t(_class);
  P_PostMorphWeapon(CPlayer, WP_FIRST);
  SB_SetClassData;
end;

//==============================================================================
//
// SB_CmdCheatSoundFunc
//
//==============================================================================
procedure SB_CmdCheatSoundFunc;
begin
  DebugSound := not DebugSound;
  if DebugSound then
    P_SetMessage(CPlayer, TXT_CHEATSOUNDON, true)
  else
    P_SetMessage(CPlayer, TXT_CHEATSOUNDOFF, true);
end;

//==============================================================================
//
// SB_GiveArtifacts
//
//==============================================================================
procedure SB_GiveArtifacts(arti: artitype_t; num: integer);
var
  i: integer;
begin
  if not SB_CmdCheckPlayerStatus then
    exit;

  for i := 0 to num - 1 do
    P_GiveArtifact(CPlayer, arti, nil);
end;

//==============================================================================
//
// SB_CmdGimme
//
//==============================================================================
procedure SB_CmdGimme(const art: string; const num: string);
var
  narti: integer;
  sarti: string;
  arti: artitype_t;
  howmany: integer;
  i: integer;
begin
  if not SB_CmdCheckPlayerStatus then
    exit;

  if num = '' then
  begin
    if strlower(art) = 'all' then
    begin
      for i := Ord(arti_none) + 1 to arti_firstpuzzitem - 1 do
        SB_GiveArtifacts(artitype_t(i), p_maxartifacts);
      P_SetMessage(CPlayer, TXT_ARTIALL);
    end
    else if strlower(art) = 'puzzle' then
    begin
      for i := arti_firstpuzzitem to Ord(NUMARTIFACTS) - 1 do
        SB_GiveArtifacts(artitype_t(i), p_maxartifacts);
      P_SetMessage(CPlayer, TXT_ARTIALLPUZZLE);
    end
    else
    begin
      printf('Usage is:'#13#10);
      printf('  gimme (artifact) (number)'#13#10);
      printf('    artifacts:'#13#10);
      printf('      invulnerability (0)'#13#10);
      printf('      health (1)'#13#10);
      printf('      superhealth (2)'#13#10);
      printf('      healingradius (3)'#13#10);
      printf('      summon (4)'#13#10);
      printf('      torch (5)'#13#10);
      printf('      egg (6)'#13#10);
      printf('      fly (7)'#13#10);
      printf('      blastradius (8)'#13#10);
      printf('      poisonbag (9)'#13#10);
      printf('      teleportother (10)'#13#10);
      printf('      speed (11)'#13#10);
      printf('      boostmana (12)'#13#10);
      printf('      boostarmor (13)'#13#10);
      printf('      teleport (14)'#13#10);
      printf('    number: (1..%d)'#13#10, [p_maxartifacts]);
      printf('      all (no number required)'#13#10);
      printf('      puzzle (no number required)'#13#10);
    end;
    exit;
  end;
  narti := atoi(art, -1);
  if narti in [0..arti_firstpuzzitem - 2] then
    arti := artitype_t(narti + 1)
  else
  begin
    sarti := strlower(art);
    if sarti = 'invulnerability' then
      arti := arti_invulnerability
    else if sarti = 'health' then
      arti := arti_health
    else if sarti = 'superhealth' then
      arti := arti_superhealth
    else if sarti = 'healingradius' then
      arti := arti_healingradius
    else if sarti = 'summon' then
      arti := arti_summon
    else if sarti = 'torch' then
      arti := arti_torch
    else if sarti = 'egg' then
      arti := arti_egg
    else if sarti = 'fly' then
      arti := arti_fly
    else if sarti = 'blastradius' then
      arti := arti_blastradius
    else if sarti = 'poisonbag' then
      arti := arti_poisonbag
    else if sarti = 'teleportother' then
      arti := arti_teleportother
    else if sarti = 'speed' then
      arti := arti_speed
    else if sarti = 'boostmana' then
      arti := arti_boostmana
    else if sarti = 'boostarmor' then
      arti := arti_boostarmor
    else if sarti = 'teleport' then
      arti := arti_teleport
    else
    begin
      printf('Bad artifact: %s'#13#10, [art]);
      exit;
    end;
  end;

  if num <> '' then
  begin
    howmany := atoi(num, -1);
    if (howmany < 1) or (howmany > p_maxartifacts) then
    begin
      printf('Bad number of artifacts: %s'#13#10, [num]);
      printf('please specify a number in range (1..%d)'#13#10, [p_maxartifacts]);
      exit;
    end;
  end
  else
    howmany := 1;

  SB_GiveArtifacts(arti, howmany);
  P_SetMessage(CPlayer, artifactMessages[Ord(arti)]);
end;

const
// Massive bunches of cheat shit
//  to keep it from being easy to figure them out.
// Yeah, right...
  cheat_mus_seq: array[0..8] of char = (
    Chr($b2), Chr($26), Chr($b6), Chr($ae), Chr($ea),
    Chr($1),  Chr($0),  Chr($0),  Chr($ff)
  ); // idmus

  cheat_iddqd_seq: array[0..5] of char = (
    Chr($b2), Chr($26), Chr($26), Chr($aa), Chr($26),
    Chr($ff)  // iddqd
  );

  cheat_quicken_seq: array[0..7] of char = (
    Chr($aa), Chr($ae), Chr($b2), Chr($e2), Chr($f2),
    Chr($a6), Chr($76), Chr($ff)
  );

  cheat_satan_seq: array[0..7] of char = (
    Chr($b2), Chr($26), Chr($ea), Chr($a2), Chr($2e), Chr($a2), Chr($76),
    Chr($ff)  // idsatan
  );

  cheat_ammo_seq: array[0..5] of char = (
    Chr($b2), Chr($26), Chr($f2), Chr($66), Chr($a2),
    Chr($ff)  // idkfa
  );

  cheat_ammonokey_seq: array[0..4] of char = (
    Chr($b2), Chr($26), Chr($66), Chr($a2), Chr($ff) // idfa
  );

  cheat_nra_seq: array[0..3] of char = (
    Chr($76), Chr($6a), Chr($a2), Chr($ff) // nra
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

// massacre cheat
  cheat_massacre_seq: array[0..8] of char = (
    Chr($b6), Chr($a2), Chr($ea), Chr($ea), Chr($a2),
    Chr($e2), Chr($6a), Chr($a6), Chr($ff) // massacre
  );

// casper cheat
  cheat_casper_seq: array[0..6] of char = (
    Chr($e2), Chr($a2), Chr($ea), Chr($2a), Chr($a6),
    Chr($6a), Chr($ff) // casper
  );

// kitty cheat
  cheat_kitty_seq: array[0..5] of char = (
    Chr($f2), Chr($b2), Chr($2e), Chr($2e), Chr($ba),
    Chr($ff) // kitty
  );

// rambo cheat
  cheat_rambo_seq: array[0..5] of char = (
    Chr($6a), Chr($a2), Chr($b6), Chr($62), Chr($f6),
    Chr($ff) // rambo
  );

// skel cheat
  cheat_skel_seq: array[0..6] of char = (
    Chr($b2), Chr($26), Chr($ea), Chr($f2), Chr($a6), Chr($36), Chr($ff) // idskel
  );

// ponce cheat
  cheat_ponce_seq: array[0..5] of char = (
    Chr($2a), Chr($f6), Chr($76), Chr($e2), Chr($a6),
    Chr($ff) // ponce
  );

// noise cheat
  cheat_noise_seq: array[0..5] of char = (
    Chr($76), Chr($f6), Chr($b2), Chr($ea), Chr($a6),
    Chr($ff) // noise
  );

var
// Now what?
  cheat_mus: cheatseq_t;
  cheat_iddqd: cheatseq_t;
  cheat_quicken: cheatseq_t;
  cheat_satan: cheatseq_t;
  cheat_ammo: cheatseq_t;
  cheat_ammonokey: cheatseq_t;
  cheat_noclip: cheatseq_t;
  cheat_commercial_noclip: cheatseq_t;
  cheat_casper: cheatseq_t;
  cheat_skel: cheatseq_t;
  cheat_ponce: cheatseq_t;
  cheat_noise: cheatseq_t;

  cheat_powerup: array[0..6] of cheatseq_t;

  cheat_clev: cheatseq_t;
  cheat_mypos: cheatseq_t;
  cheat_massacre: cheatseq_t;
  cheat_kitty: cheatseq_t;
  cheat_rambo: cheatseq_t;
  cheat_nra: cheatseq_t;

//==============================================================================
// SB_SetClassData
//
//---------------------------------------------------------------------------
//
// PROC SB_Init
//
//---------------------------------------------------------------------------
//
//==============================================================================
procedure SB_SetClassData;
var
  _class: integer;
begin
  _class := Ord(PlayerClass[consoleplayer]); // original player _class (not pig)
  PatchWEAPONSLOT := W_CacheLumpNum(W_GetNumForName('wpslot0') + _class, PU_STATIC);
  PatchWEAPONFULL := W_CacheLumpNum(W_GetNumForName('wpfull0') + _class, PU_STATIC);
  PatchPIECE1 := W_CacheLumpNum(W_GetNumForName('wpiecef1') + _class, PU_STATIC);
  PatchPIECE2 := W_CacheLumpNum(W_GetNumForName('wpiecef2') + _class, PU_STATIC);
  PatchPIECE3 := W_CacheLumpNum(W_GetNumForName('wpiecef3') + _class, PU_STATIC);
  PatchCHAIN := W_CacheLumpNum(W_GetNumForName('chain') + _class, PU_STATIC);
  if not netgame then
  begin // single player game uses red life gem (the second gem)
    PatchLIFEGEM := W_CacheLumpNum(W_GetNumForName('lifegem') + MAXPLAYERS * _class + 1, PU_STATIC);
  end
  else
  begin
    PatchLIFEGEM := W_CacheLumpNum(W_GetNumForName('lifegem') + MAXPLAYERS * _class + consoleplayer, PU_STATIC);
  end;
end;

//==============================================================================
//
// SB_Init
//
//==============================================================================
procedure SB_Init;
var
  i: integer;
  startLump: integer;
begin
// Now what?
  cheat_mus.sequence := get_cheatseq_string(cheat_mus_seq);
  cheat_mus.p := get_cheatseq_string(0);
  cheat_iddqd.sequence := get_cheatseq_string(cheat_iddqd_seq);
  cheat_iddqd.p := get_cheatseq_string(0);
  cheat_quicken.sequence := get_cheatseq_string(cheat_quicken_seq);
  cheat_quicken.p := get_cheatseq_string(0);
  cheat_satan.sequence := get_cheatseq_string(cheat_satan_seq);
  cheat_satan.p := get_cheatseq_string(0);
  cheat_ammo.sequence := get_cheatseq_string(cheat_ammo_seq);
  cheat_ammo.p := get_cheatseq_string(0);
  cheat_ammonokey.sequence := get_cheatseq_string(cheat_ammonokey_seq);
  cheat_ammonokey.p := get_cheatseq_string(0);
  cheat_noclip.sequence := get_cheatseq_string(cheat_noclip_seq);
  cheat_noclip.p := get_cheatseq_string(0);
  cheat_casper.sequence := get_cheatseq_string(cheat_casper_seq);
  cheat_casper.p := get_cheatseq_string(0);
  cheat_commercial_noclip.sequence := get_cheatseq_string(cheat_commercial_noclip_seq);
  cheat_commercial_noclip.p := get_cheatseq_string(0);
  cheat_skel.sequence := get_cheatseq_string(cheat_skel_seq);
  cheat_skel.p := get_cheatseq_string(0);
  cheat_ponce.sequence := get_cheatseq_string(cheat_ponce_seq);
  cheat_ponce.p := get_cheatseq_string(0);
  cheat_noise.sequence := get_cheatseq_string(cheat_noise_seq);
  cheat_noise.p := get_cheatseq_string(0);

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

  cheat_clev.sequence := get_cheatseq_string(cheat_clev_seq);
  cheat_clev.p := get_cheatseq_string(0);
  cheat_mypos.sequence := get_cheatseq_string(cheat_mypos_seq);
  cheat_mypos.p := get_cheatseq_string(0);
  cheat_massacre.sequence := get_cheatseq_string(cheat_massacre_seq);
  cheat_massacre.p := get_cheatseq_string(0);
  cheat_kitty.sequence := get_cheatseq_string(cheat_kitty_seq);
  cheat_kitty.p := get_cheatseq_string(0);
  cheat_rambo.sequence := get_cheatseq_string(cheat_rambo_seq);
  cheat_rambo.p := get_cheatseq_string(0);
  cheat_nra.sequence := get_cheatseq_string(cheat_nra_seq);
  cheat_nra.p := get_cheatseq_string(0);

  PatchH2BAR := W_CacheLumpName('H2BAR', PU_STATIC);
  PatchH2TOP := W_CacheLumpName('H2TOP', PU_STATIC);
  PatchINVBAR := W_CacheLumpName('INVBAR', PU_STATIC);
  PatchLFEDGE := W_CacheLumpName('LFEDGE', PU_STATIC);
  PatchRTEDGE := W_CacheLumpName('RTEDGE', PU_STATIC);
  PatchSTATBAR := W_CacheLumpName('STATBAR', PU_STATIC);
  PatchKEYBAR := W_CacheLumpName('KEYBAR', PU_STATIC);
  PatchSELECTBOX := W_CacheLumpName('SELECTBO', PU_STATIC);
  PatchARTICLEAR := W_CacheLumpName('ARTICLS', PU_STATIC);
  PatchARMCLEAR := W_CacheLumpName('ARMCLS', PU_STATIC);
  PatchMANACLEAR := W_CacheLumpName('MANACLS', PU_STATIC);
  PatchMANAVIAL1 := W_CacheLumpName('MANAVL1', PU_STATIC);
  PatchMANAVIAL2 := W_CacheLumpName('MANAVL2', PU_STATIC);
  PatchMANAVIALDIM1 := W_CacheLumpName('MANAVL1D', PU_STATIC);
  PatchMANAVIALDIM2 := W_CacheLumpName('MANAVL2D', PU_STATIC);
  PatchMANADIM1 := W_CacheLumpName('MANADIM1', PU_STATIC);
  PatchMANADIM2 := W_CacheLumpName('MANADIM2', PU_STATIC);
  PatchMANABRIGHT1 := W_CacheLumpName('MANABRT1', PU_STATIC);
  PatchMANABRIGHT2 := W_CacheLumpName('MANABRT2', PU_STATIC);
  PatchINVLFGEM1 := W_CacheLumpName('invgeml1', PU_STATIC);
  PatchINVLFGEM2 := W_CacheLumpName('invgeml2', PU_STATIC);
  PatchINVRTGEM1 := W_CacheLumpName('invgemr1', PU_STATIC);
  PatchINVRTGEM2 := W_CacheLumpName('invgemr2', PU_STATIC);

  startLump := W_GetNumForName('IN0');
  for i := 0 TO 9 do
    PatchINumbers[i] := W_CacheLumpNum(startLump + i, PU_STATIC);

  PatchNEGATIVE := W_CacheLumpName('NEGNUM', PU_STATIC);
  FontBNumBase := W_GetNumForName('FONTB16');

  startLump := W_GetNumForName('SMALLIN0');
  for i := 0 to 9 do
    PatchSmNumbers[i] := W_CacheLumpNum(startLump + i, PU_STATIC);

  PlayPalette := W_GetNumForName('PLAYPAL');
  SpinFlylump := W_GetNumForName('SPFLY0');
  SpinMinotaurLump := W_GetNumForName('SPMINO0');
  SpinSpeedLump := W_GetNumForName('SPBOOT0');
  SpinDefenseLump := W_GetNumForName('SPSHLD0');

  lmp_useartia := W_GetNumForName('useartia');
  lmp_keyslot1 := W_GetNumForName('keyslot1');
  lmp_armslot1 := W_GetNumForName('armslot1');

  if deathmatch <> 0 then
    PatchKILLS := W_CacheLumpName('KILLS', PU_STATIC);

  SB_SetClassData;

  C_AddCmd('god, quicken, satan, idsatan', @SB_CmdGod);
  C_AddCmd('iddqd', @SB_CmdIddqd);
  C_AddCmd('nra, givefullammo, rambo, idfa', @SB_CmdIDFA);
  C_AddCmd('lowgravity', @SB_CmdLowGravity);
  C_AddCmd('givefullammoandkeys, idkfa, conan', @SB_CmdIDKFA);
  C_AddCmd('iddt', @SB_CmdIDDT);
  C_AddCmd('kitty, casper, idspispopd, idclip', @SB_CmdIDNoClip);
  C_AddCmd('massacre', @SB_CmdMassacre);
  C_AddCmd('gimme', @SB_CmdGimme);
  C_AddCmd('skel, locksmith, idskel', @SB_CmdSkel);
  C_AddCmd('ponce, clubmed', @SB_CmdPonce);
  C_AddCmd('noise, debugsound', @SB_CmdCheatSoundFunc);
  C_AddCmd('deliverance', @SB_CmdPig);
  C_AddCmd('shadowcaster, changeplayerclass', @SB_CmdChangeClass);

end;

//---------------------------------------------------------------------------
//
// PROC SB_Ticker
//
//---------------------------------------------------------------------------
//
//==============================================================================
procedure SB_Ticker;
var
  delta: integer;
  curHealth: integer;
begin
  curHealth := players[consoleplayer].mo.health;
  if curHealth < 0 then
    curHealth := 0;

  if curHealth < HealthMarker then
  begin
    delta := (HealthMarker - curHealth) div 4;
    if delta < 1 then
      delta := 1
    else if delta > 8 then
      delta := 8;
    HealthMarker := HealthMarker - delta;
  end
  else if curHealth > HealthMarker then
  begin
    delta := (curHealth - HealthMarker) div 4;
    if delta < 1 then
      delta := 1
    else if delta > 8 then
      delta := 8;
    HealthMarker := HealthMarker + delta;
  end;
end;

var
  sb_topoffset: integer = 0;

//==============================================================================
//
// SB_DrawPatch
//
//==============================================================================
procedure SB_DrawPatch(const x, y: integer; patch: Ppatch_t); overload;
var
  toffs: integer;
begin
  toffs := y - patch.topoffset;
  if toffs < sb_topoffset then
    sb_topoffset := toffs;
  if sb_topoffset < 0 then
    sb_topoffset := 0;

  V_DrawPatch(x, y, SCN_SB, patch, false);
end;

//==============================================================================
//
// SB_DrawPatch
//
//==============================================================================
procedure SB_DrawPatch(const x, y: integer; const patchname: string); overload;
var
  toffs: integer;
  patch: Ppatch_t;
begin
  patch := W_CacheLumpName(patchname, PU_STATIC);

  toffs := y + patch.topoffset;
  if toffs < sb_topoffset then
    sb_topoffset := toffs;

  if y < sb_topoffset then
    sb_topoffset := y;

  V_DrawPatch(x, y, SCN_SB, patch, false);

  Z_ChangeTag(patch, PU_CACHE);
end;

//==============================================================================
//
// SB_DrINumber
//
// Draws a three digit number.
//
//==============================================================================
procedure SB_DrINumber(val: integer; x, y: integer);
var
  patch: Ppatch_t;
  oldval: integer;
begin
  oldval := val;
  if val < 0 then
  begin
    val := -val;
    if val > 99 then
      val := 99;
    if val > 9 then
    begin
      patch := PatchINumbers[val div 10];
      SB_DrawPatch(x + 8, y, patch);
      SB_DrawPatch(x, y, PatchNEGATIVE);
    end
    else
    begin
      SB_DrawPatch(x + 8, y, PatchNEGATIVE);
    end;
    val := val mod 10;
    patch := PatchINumbers[val];
    SB_DrawPatch(x + 16, y, patch);
    exit;
  end;
  if val > 99 then
  begin
    patch := PatchINumbers[val div 100];
    SB_DrawPatch(x, y, patch);
  end;
  val := val mod 100;
  if (val > 9) or (oldval > 99) then
  begin
    patch := PatchINumbers[val div 10];
    SB_DrawPatch(x + 8, y, patch);
  end;
  val := val mod 10;
  patch := PatchINumbers[val];
  SB_DrawPatch(x + 16, y, patch);
end;

//==============================================================================
//
// SB_DrRedINumber
//
// Draws a three digit number using the red font
//
//==============================================================================
procedure SB_DrRedINumber(val: integer; x, y: integer);
var
  patch: Ppatch_t;
  oldval: integer;
begin
  oldval := val;
  if val < 0 then
    val := 0;
  if val > 99 then
  begin
    patch := W_CacheLumpNum(W_GetNumForName('inred0') + val div 100, PU_CACHE);
    SB_DrawPatch(x, y, patch);
  end;
  val := val mod 100;
  if (val > 9) or (oldval > 99) then
  begin
    patch := W_CacheLumpNum(W_GetNumForName('inred0') + val div 10, PU_CACHE);
    SB_DrawPatch(x + 8, y, patch);
  end;
  val := val mod 10;
  patch := W_CacheLumpNum(W_GetNumForName('inred0') + val, PU_CACHE);
  SB_DrawPatch(x + 16, y, patch);
end;

//==============================================================================
//
// SB_DrBNumber
//
// Draws a three digit number using FontB
//
//==============================================================================
procedure SB_DrBNumber(val: integer; x, y: integer);
var
  patch: Ppatch_t;
  xpos: integer;
  oldval: integer;
begin
  oldval := val;
  xpos := x;
  if val < 0 then
    val := 0;
  if val > 99 then
  begin
    patch := W_CacheLumpNum(FontBNumBase + val div 100, PU_CACHE);
    SB_DrawPatch(xpos + 6 - patch.width div 2, y, patch); // V_DrawShadowedPatch
  end;
  val := val mod 100;
  xpos := xpos + 12;
  if (val > 9) or (oldval > 99) then
  begin
    patch := W_CacheLumpNum(FontBNumBase + val div 10, PU_CACHE);
    SB_DrawPatch(xpos + 6 - patch.width div 2, y, patch); // V_DrawShadowedPatch
  end;
  val := val mod 10;
  xpos := xpos + 12;
  patch := W_CacheLumpNum(FontBNumBase + val, PU_CACHE);
  SB_DrawPatch(xpos + 6 - patch.width div 2, y, patch); // V_DrawShadowedPatch
end;

//==============================================================================
//
// SB_DrSmallNumber
//
// Draws a small two digit number.
//
//==============================================================================
procedure SB_DrSmallNumber(val: integer; x, y: integer);
var
  patch: Ppatch_t;
begin
  if val <= 0 then
    exit;

  if val > 999 then
    val := val mod 1000;

  if val > 99 then
  begin
    patch := PatchSmNumbers[val div 100];
    SB_DrawPatch(x, y, patch);
    patch := PatchSmNumbers[(val mod 100) div 10];
    SB_DrawPatch(x + 4, y, patch);
  end
  else if val > 9 then
  begin
    patch := PatchSmNumbers[val div 10];
    SB_DrawPatch(x + 4, y, patch);
  end;
  val := val mod 10;
  patch := PatchSmNumbers[val];
  SB_DrawPatch(x + 8, y, patch);
end;

//==============================================================================
// SB_WriteText
//
//      Write a string using the hu_font
//
//==============================================================================
procedure SB_WriteText(x, y: integer; const _string: string);
var
  w: integer;
  ch: integer;
  c: integer;
  cx: integer;
  cy: integer;
  len: integer;
begin
  len := Length(_string);
  if len = 0 then
    exit;

  ch := 1;
  cx := x;
  cy := y;

  while true do
  begin
    if ch > len then
      break;

    c := Ord(_string[ch]);
    inc(ch);

    if c = 0 then
      break;

    if c = 10 then
    begin
      cx := x;
      continue;
    end;

    if c = 13 then
    begin
      cy := cy + 12;
      continue;
    end;

    c := Ord(toupper(Chr(c))) - Ord(HU_FONTSTART);
    if (c < 0) or (c >= HU_FONTSIZE) then
    begin
      cx := cx + 4;
      continue;
    end;

    w := hu_font[c].width;
    if (cx + w) > 320 then
      break;
    SB_DrawPatch(cx, cy, hu_font[c]);
    cx := cx + w;
  end;
end;

//---------------------------------------------------------------------------
//
// PROC SB_DrawSoundInfo
//
// Displays sound debugging information.
//
//---------------------------------------------------------------------------

const
  si_xpos: array[0..6] of integer = (1, 72, 110, 156, 200, 250, 280);

//==============================================================================
//
// SB_DrawSoundInfo
//
//==============================================================================
procedure SB_DrawSoundInfo;
var
  i: integer;
  s: soundinfo_t;
  c: Pchaninfo_t;
  txt: string;
  x, y: integer;
begin
  if leveltime and 16 <> 0 then
    SB_WriteText(si_xPos[0], 20, '*** SOUND DEBUG INFO ***');

  S_GetChannelInfo(@s);
  if s.numchannels = 0 then
    exit;

  SB_WriteText(si_xPos[0], 30, 'NAME');
  SB_WriteText(si_xPos[1], 30, 'MO.T');
  SB_WriteText(si_xPos[2], 30, 'MO.X');
  SB_WriteText(si_xPos[3], 30, 'MO.Y');
  SB_WriteText(si_xPos[4], 30, 'ID');
  SB_WriteText(si_xPos[5], 30, 'PRI');
  SB_WriteText(si_xPos[6], 30, 'DIST');
  for i := 0 to s.numchannels - 1 do
  begin
    c := @s.chan[i];
    x := 0;
    y := 40 + i * 10;
    if c.mo = nil then
    begin // Channel is unused
      SB_WriteText(si_xPos[0], y, '------');
      continue;
    end;
    SB_WriteText(si_xPos[x], y, strupper(c.name));
    inc(x);
    sprintf(txt, '%d', [c.mo._type]);
    SB_WriteText(si_xPos[x], y, txt);
    inc(x);
    sprintf(txt, '%d', [c.mo.x div FRACUNIT]);
    SB_WriteText(si_xPos[x], y, txt);
    inc(x);
    sprintf(txt, '%d', [c.mo.y div FRACUNIT]);
    SB_WriteText(si_xPos[x], y, txt);
    inc(x);
    sprintf(txt, '%d', [c.id]);
    SB_WriteText(si_xPos[x], y, txt);
    inc(x);
    sprintf(txt, '%d', [c.priority]);
    SB_WriteText(si_xPos[x], y, txt);
    inc(x);
    sprintf(txt, '%d', [c.distance]);
    SB_WriteText(si_xPos[x], y, txt);
  end;
end;

//---------------------------------------------------------------------------
//
// PROC SB_Drawer
//
//---------------------------------------------------------------------------

const
  patcharti: array[0..32] of string = (
    'ARTIBOX',  // none
    'ARTIINVU', // invulnerability
    'ARTIPTN2', // health
    'ARTISPHL', // superhealth
    'ARTIHRAD', // healing radius
    'ARTISUMN', // summon maulator
    'ARTITRCH', // torch
    'ARTIPORK', // egg
    'ARTISOAR', // fly
    'ARTIBLST', // blast radius
    'ARTIPSBG', // poison bag
    'ARTITELO', // teleport other
    'ARTISPED', // speed
    'ARTIBMAN', // boost mana
    'ARTIBRAC', // boost armor
    'ARTIATLP', // teleport
    'ARTISKLL', // arti_puzzskull
    'ARTIBGEM', // arti_puzzgembig
    'ARTIGEMR', // arti_puzzgemred
    'ARTIGEMG', // arti_puzzgemgreen1
    'ARTIGMG2', // arti_puzzgemgreen2
    'ARTIGEMB', // arti_puzzgemblue1
    'ARTIGMB2', // arti_puzzgemblue2
    'ARTIBOK1', // arti_puzzbook1
    'ARTIBOK2', // arti_puzzbook2
    'ARTISKL2', // arti_puzzskull2
    'ARTIFWEP', // arti_puzzfweapon
    'ARTICWEP', // arti_puzzcweapon
    'ARTIMWEP', // arti_puzzmweapon
    'ARTIGEAR', // arti_puzzgear1
    'ARTIGER2', // arti_puzzgear2
    'ARTIGER3', // arti_puzzgear3
    'ARTIGER4'  // arti_puzzgear4
  );

//
// SB_DrawAnimatedIcons
//

var
  hitCenterFrame: boolean = false;

//==============================================================================
//
// SB_DrawAnimatedIcons
//
//==============================================================================
procedure SB_DrawAnimatedIcons;
var
  frame: integer;
begin
  sbiconsactive := false;
  // Wings of wrath
  if CPlayer.powers[Ord(pw_flight)] > 0 then
  begin
    if (CPlayer.powers[Ord(pw_flight)] > BLINKTHRESHOLD) or
       (CPlayer.powers[Ord(pw_flight)] and 16 = 0) then
    begin
      frame := (leveltime div 3) and 15;
      if CPlayer.mo.flags2 and MF2_FLY <> 0 then
      begin
        if hitCenterFrame and (frame <> 15) and (frame <> 0) then
        begin
          SB_DrawPatch(20, 19, W_CacheLumpNum(SpinFlylump + 15, PU_CACHE));
          sbiconsactive := true;
        end
        else
        begin
          SB_DrawPatch(20, 19, W_CacheLumpNum(SpinFlylump + frame, PU_CACHE));
          sbiconsactive := true;
          hitCenterFrame := false;
        end;
      end
      else
      begin
        if not hitCenterFrame and (frame <> 15) and (frame <> 0) then
        begin
          SB_DrawPatch(20, 19, W_CacheLumpNum(SpinFlylump + frame, PU_CACHE));
          sbiconsactive := true;
          hitCenterFrame := false;
        end
        else
        begin
          SB_DrawPatch(20, 19, W_CacheLumpNum(SpinFlylump + 15, PU_CACHE));
          sbiconsactive := true;
          hitCenterFrame := true;
        end;
      end;
    end;
  end;

  // Speed Boots
  if CPlayer.powers[Ord(pw_speed)] > 0 then
  begin
    if (CPlayer.powers[Ord(pw_speed)] > BLINKTHRESHOLD) or (CPlayer.powers[Ord(pw_speed)] and 16 = 0) then
    begin
      frame := (leveltime div 3) and 15;
      SB_DrawPatch(60, 19, W_CacheLumpNum(SpinSpeedLump + frame, PU_CACHE));
      sbiconsactive := true;
    end;
  end;

  // Defensive power
  if CPlayer.powers[Ord(pw_invulnerability)] > 0 then
  begin
    if (CPlayer.powers[Ord(pw_invulnerability)] > BLINKTHRESHOLD) or (CPlayer.powers[Ord(pw_invulnerability)] and 16 = 0) then
    begin
      frame := (leveltime div 3) and 15;
      SB_DrawPatch(260, 19, W_CacheLumpNum(SpinDefenseLump + frame, PU_CACHE));
      sbiconsactive := true;
    end;
  end;

  // Minotaur Active
  if CPlayer.powers[Ord(pw_minotaur)] > 0 then
  begin
    if (CPlayer.powers[Ord(pw_minotaur)] > BLINKTHRESHOLD) or (CPlayer.powers[Ord(pw_minotaur)] and 16 = 0) then
    begin
      frame := (leveltime div 3) and 15;
      SB_DrawPatch(300, 19, W_CacheLumpNum(SpinMinotaurLump + frame, PU_CACHE));
      sbiconsactive := true;
    end;
  end;
end;

//
// SB_PaletteFlash
//
// Sets the new palette based upon the current values of
// consoleplayer.damagecount and consoleplayer.bonuscount.
//

var
  sb_palette: integer = 0;

//==============================================================================
//
// SB_PaletteFlash
//
//==============================================================================
procedure SB_PaletteFlash(forceChange: boolean);
var
  palette: integer;
  pal: PByteArray;
begin
  if forceChange then
  begin
    sb_palette := -1;
  end;

  if gamestate = GS_LEVEL then
  begin
    CPlayer := @players[consoleplayer];
    if CPlayer.poisoncount > 0 then
    begin
      palette := _SHR3(CPlayer.poisoncount + 7);
      if palette >= NUMPOISONPALS then
      begin
        palette := NUMPOISONPALS - 1;
      end;
      palette := palette + STARTPOISONPALS;
    end
    else if CPlayer.damagecount > 0 then
    begin
      palette := _SHR3(CPlayer.damagecount + 7);
      if palette >= NUMREDPALS then
      begin
        palette := NUMREDPALS - 1;
      end;
      palette := palette + STARTREDPALS;
    end
    else if CPlayer.bonuscount > 0 then
    begin
      palette := _SHR3(CPlayer.bonuscount + 7);
      if palette >= NUMBONUSPALS then
      begin
        palette := NUMBONUSPALS - 1;
      end;
      palette := palette + STARTBONUSPALS;
    end
    else if CPlayer.mo.flags2 and MF2_ICEDAMAGE <> 0 then
    begin // Frozen player
      palette := STARTICEPAL;
    end
    else
    begin
      palette := 0;
    end;
  end
  else
  begin
    palette := 0;
  end;
  if palette <> sb_palette then
  begin
    sb_palette := palette;
    {$IFDEF OPENGL}
    gld_SetPalette(palette);
    {$ELSE}
    R_SetPalette(palette);
    {$ENDIF}
    pal := V_ReadPalette(PU_STATIC);
    I_SetPalette(@pal[palette * 768]);
    V_SetPalette(@pal[palette * 768]);
    Z_ChangeTag(pal, PU_CACHE);
  end;
end;

//==============================================================================
//
// SB_DrawCommonBar
//
//==============================================================================
procedure SB_DrawCommonBar;
var
  healthPos: integer;
begin
  SB_DrawPatch(0, 134, PatchH2TOP);

  healthPos := HealthMarker;
  if healthPos < 0 then
    healthPos := 0
  else if healthPos > 100 then
    healthPos := 100;
  SB_DrawPatch(28 + (((healthPos * 196) div 100) mod 9), 193, PatchCHAIN);
  SB_DrawPatch(7 + ((healthPos * 11) div 5), 193, PatchLIFEGEM);
  SB_DrawPatch(0, 193, PatchLFEDGE);
  SB_DrawPatch(277, 193, PatchRTEDGE);
end;

//
// SB_DrawWeaponPieces
//

const
  PieceX: array[0..Ord(NUMCLASSES) - 1, 0..2] of integer = (
    (190, 225, 234),
    (190, 212, 225),
    (190, 205, 224),
    (  0,   0,   0) // Pig is never used
  );

//==============================================================================
//
// SB_DrawWeaponPieces
//
//==============================================================================
procedure SB_DrawWeaponPieces;
begin
  if CPlayer.pieces = 7 then
  begin
    SB_DrawPatch(190, 162, PatchWEAPONFULL);
    exit;
  end;

  SB_DrawPatch(190, 162, PatchWEAPONSLOT);

  if CPlayer.pieces and WPIECE1 <> 0 then
  begin
    SB_DrawPatch(PieceX[Ord(PlayerClass[consoleplayer]), 0], 162, PatchPIECE1);
  end;
  if CPlayer.pieces and WPIECE2 <> 0 then
  begin
    SB_DrawPatch(PieceX[Ord(PlayerClass[consoleplayer]), 1], 162, PatchPIECE2);
  end;
  if CPlayer.pieces and WPIECE3 <> 0 then
  begin
    SB_DrawPatch(PieceX[Ord(PlayerClass[consoleplayer]), 2], 162, PatchPIECE3);
  end;
end;

//==============================================================================
//
// SB_DrawMainBar
//
//==============================================================================
procedure SB_DrawMainBar;
var
  i: integer;
  temp: integer;
  manaPatch1, manaPatch2: Ppatch_t;
  manaVialPatch1, manaVialPatch2: Ppatch_t;
begin
  // Ready artifact
  if ArtifactFlash > 0 then
  begin
    SB_DrawPatch(144, 160, PatchARTICLEAR);
    SB_DrawPatch(148, 164, W_CacheLumpNum(lmp_useartia + ArtifactFlash - 1, PU_CACHE));
    dec(ArtifactFlash);
  end
  else
  begin
    SB_DrawPatch(144, 160, PatchARTICLEAR);
    if Ord(CPlayer.readyArtifact) > 0 then
    begin
      SB_DrawPatch(143, 163, W_CacheLumpName(patcharti[Ord(CPlayer.readyArtifact)], PU_CACHE));
      if CPlayer.inventory[inv_ptr].count > 1 then
      begin
        SB_DrSmallNumber(CPlayer.inventory[inv_ptr].count, 162, 184);
      end;
    end;
  end;

  // Frags
  if deathmatch <> 0 then
  begin
    temp := 0;
    for i := 0 to MAXPLAYERS - 1 do
    begin
      temp := temp + CPlayer.frags[i];
    end;
    SB_DrawPatch(38, 162, PatchKILLS);
    SB_DrINumber(temp, 40, 176);
  end
  else
  begin
    temp := HealthMarker;
    if temp < 0 then
      temp := 0
    else if temp > 100 then
      temp := 100;
    SB_DrawPatch(41, 178, PatchARMCLEAR);
    if temp >= 25 then
      SB_DrINumber(temp, 40, 176)
    else
      SB_DrRedINumber(temp, 40, 176);
  end;

  // Mana
  temp := CPlayer.mana[0];
  SB_DrawPatch(77, 178, PatchMANACLEAR);
  SB_DrSmallNumber(temp, 79, 181);
  if temp = 0 then // Draw Dim Mana icon
    manaPatch1 := PatchMANADIM1
  else
    manaPatch1 := PatchMANABRIGHT1;

  temp := CPlayer.mana[1];
  SB_DrawPatch(109, 178, PatchMANACLEAR);
  SB_DrSmallNumber(temp, 111, 181);
  manaVialPatch1 := Ppatch_t(1); // force a vial update
  if temp = 0 then
  begin // Draw Dim Mana icon
    manaPatch2 := PatchMANADIM2;
  end
  else
  begin
    manaPatch2 := PatchMANABRIGHT2;
  end;
  if (manaPatch1 <> nil) or (manaPatch2 <> nil) or (manaVialPatch1 <> nil) then
  begin // Update mana graphics based upon mana count/weapon type
    if CPlayer.readyweapon = WP_FIRST then
    begin
      manaPatch1 := PatchMANADIM1;
      manaPatch2 := PatchMANADIM2;
      manaVialPatch1 := PatchMANAVIALDIM1;
      manaVialPatch2 := PatchMANAVIALDIM2;
    end
    else if CPlayer.readyweapon = WP_SECOND then
    begin
      if manaPatch1 = nil then
      begin
        manaPatch1 := PatchMANABRIGHT1;
      end;
      manaVialPatch1 := PatchMANAVIAL1;
      manaPatch2 := PatchMANADIM2;
      manaVialPatch2 := PatchMANAVIALDIM2;
    end
    else if CPlayer.readyweapon = WP_THIRD then
    begin
      manaPatch1 := PatchMANADIM1;
      manaVialPatch1 := PatchMANAVIALDIM1;
      if manaPatch2 = nil then
      begin
        manaPatch2 := PatchMANABRIGHT2;
      end;
      manaVialPatch2 := PatchMANAVIAL2;
    end
    else
    begin
      manaVialPatch1 := PatchMANAVIAL1;
      manaVialPatch2 := PatchMANAVIAL2;
      if manaPatch1 = nil then
      begin
        manaPatch1 := PatchMANABRIGHT1;
      end;
      if manaPatch2 = nil then
      begin
        manaPatch2 := PatchMANABRIGHT2;
      end;
    end;
    SB_DrawPatch(77, 164, manaPatch1);
    SB_DrawPatch(110, 164, manaPatch2);
    SB_DrawPatch(94, 164, manaVialPatch1);
    for i := 165 to 186 - (22 * CPlayer.mana[0]) div MAX_MANA do
    begin
      screens[SCN_SB, i * 320 + 95] := aprox_black;
      screens[SCN_SB, i * 320 + 96] := aprox_black;
      screens[SCN_SB, i * 320 + 97] := aprox_black;
    end;
    SB_DrawPatch(102, 164, manaVialPatch2);
    for i := 165 to 186 - (22 * CPlayer.mana[1]) div MAX_MANA do
    begin
      screens[SCN_SB, i * 320 + 103] := aprox_black;
      screens[SCN_SB, i * 320 + 104] := aprox_black;
      screens[SCN_SB, i * 320 + 105] := aprox_black;
    end;
  end;
  // Armor
  temp := AutoArmorSave[Ord(CPlayer._class)] +
          CPlayer.armorpoints[Ord(ARMOR_ARMOR)] +
          CPlayer.armorpoints[Ord(ARMOR_SHIELD)] +
          CPlayer.armorpoints[Ord(ARMOR_HELMET)] +
          CPlayer.armorpoints[Ord(ARMOR_AMULET)];
  SB_DrawPatch(255, 178, PatchARMCLEAR);
  SB_DrINumber(FixedInt(FixedDiv(temp, 5 * FRACUNIT)), 250, 176);
  // Weapon Pieces
  SB_DrawWeaponPieces;
end;

//==============================================================================
//
// SB_DrawInventoryBar
//
//==============================================================================
procedure SB_DrawInventoryBar;
var
  i: integer;
  x: integer;
begin
  x := inv_ptr - curpos;
  SB_DrawPatch(38, 162, PatchINVBAR);
  for i := 0 to 6 do
  begin
    if (CPlayer.inventorySlotNum > x + i) and (CPlayer.inventory[x + i]._type <> Ord(arti_none)) then
    begin
      SB_DrawPatch(50 + i * 31, 163, patcharti[CPlayer.inventory[x + i]._type]);
      if CPlayer.inventory[x + i].count > 1 then
      begin
        SB_DrSmallNumber(CPlayer.inventory[x + i].count, 68 + i * 31, 185);
      end;
    end;
  end;
  SB_DrawPatch(50 + curpos * 31, 163, PatchSELECTBOX);
  if x <> 0 then
  begin
    if leveltime and 4 = 0 then
      SB_DrawPatch(42, 163, PatchINVLFGEM1)
    else
      SB_DrawPatch(42, 163, PatchINVLFGEM2);
  end;
  if CPlayer.inventorySlotNum - x > 7 then
  begin
    if leveltime and 4 = 0 then
      SB_DrawPatch(269, 163, PatchINVRTGEM1)
    else
      SB_DrawPatch(269, 163, PatchINVRTGEM2);
  end;
end;

//==============================================================================
//
// SB_DrawKeyBar
//
//==============================================================================
procedure SB_DrawKeyBar;
var
  i: integer;
  xPosition: integer;
begin
  xPosition := 46;
  i := 0;
  while (i < Ord(NUMKEYCARDS)) and (xPosition <= 126) do
  begin
    if CPlayer.keys and _SHL(1, i) <> 0 then
    begin
      SB_DrawPatch(xPosition, 164, W_CacheLumpNum(lmp_keyslot1 + i, PU_CACHE));
      xPosition := xPosition + 20;
    end;
    inc(i);
  end;

  for i := 0 to Ord(NUMARMOR) - 1 do
  begin
    if CPlayer.armorpoints[i] = 0 then
      continue;

    if CPlayer.armorpoints[i] <= _SHR2(ArmorIncrement[Ord(CPlayer._class), i]) then
    begin
//        V_DrawFuzzPatch(150 + 31 * i, 164, W_CacheLumpNum(lmp_armslot1 + i));
      SB_DrawPatch(150 + 31 * i, 164, W_CacheLumpNum(lmp_armslot1 + i, PU_CACHE));
    end
    else if CPlayer.armorpoints[i] <= _SHR1(ArmorIncrement[Ord(CPlayer._class), i]) then
    begin
//        V_DrawAltFuzzPatch(150 + 31 * i, 164, W_CacheLumpNum(lmp_armslot1 + i, PU_CACHE);
      SB_DrawPatch(150 + 31 * i, 164, W_CacheLumpNum(lmp_armslot1 + i, PU_CACHE));
    end
    else
    begin
      SB_DrawPatch(150 + 31 * i, 164, W_CacheLumpNum(lmp_armslot1 + i, PU_CACHE));
    end;
  end;
end;

//==============================================================================
//
// SB_DrawFullScreenStuff
//
//==============================================================================
procedure SB_DrawFullScreenStuff;
var
  i: integer;
  x: integer;
  temp: integer;
begin
  if CPlayer.mo.health > 0 then
  begin
    SB_DrBNumber(CPlayer.mo.health, 5, 180);
  end
  else
  begin
    SB_DrBNumber(0, 5, 180);
  end;

  if deathmatch <> 0 then
  begin
    temp := 0;
    for i := 0 to MAXPLAYERS - 1 do
    begin
      if playeringame[i] then
      begin
        temp := temp + CPlayer.frags[i];
      end;
    end;
    SB_DrINumber(temp, 45, 185);
  end;
  if not inventory then
  begin
    if Ord(CPlayer.readyArtifact) > 0 then
    begin
//      V_DrawFuzzPatch(286, 170, 'ARTIBOX');
      SB_DrawPatch(286, 170, 'ARTIBOX');
      SB_DrawPatch(284, 169, patcharti[Ord(CPlayer.readyArtifact)]);
      if CPlayer.inventory[inv_ptr].count > 1 then
      begin
        SB_DrSmallNumber(CPlayer.inventory[inv_ptr].count, 302, 192);
      end;
    end;
  end
  else
  begin
    x := inv_ptr - curpos;
    for i := 0 to 6 do
    begin
//      V_DrawFuzzPatch(50 + i * 31, 168, 'ARTIBOX');
      SB_DrawPatch(50 + i * 31, 168, 'ARTIBOX');
      if (CPlayer.inventorySlotNum > x + i) and (CPlayer.inventory[x + i]._type <> Ord(arti_none)) then
      begin
        SB_DrawPatch(49 + i * 31, 167, patcharti[Ord(CPlayer.inventory[x + i]._type)]);
        if CPlayer.inventory[x + i].count > 1 then
        begin
          SB_DrSmallNumber(CPlayer.inventory[x + i].count, 66 + i * 31, 188);
        end;
      end;
    end;
    SB_DrawPatch(50 + curpos * 31, 167, PatchSELECTBOX);
    if x <> 0 then
    begin
      if leveltime and 4 = 0 then
        SB_DrawPatch(40, 167, PatchINVLFGEM1)
      else
        SB_DrawPatch(40, 167, PatchINVLFGEM2);
    end;
    if CPlayer.inventorySlotNum - x > 7 then
    begin
      if leveltime and 4 = 0 then
        SB_DrawPatch(268, 167, PatchINVRTGEM1)
      else
        SB_DrawPatch(268, 167, PatchINVRTGEM2);
    end;
  end;
end;

//==============================================================================
//
// SB_Drawer
//
//==============================================================================
procedure SB_Drawer;
begin
  if firstinterpolation then
  begin
    MT_ZeroMemory(@screens[SCN_SB][sb_topoffset * 320], 320 * (200 - sb_topoffset));
    sb_topoffset := 158;

    // Sound info debug stuff
    if DebugSound then
      SB_DrawSoundInfo;

    CPlayer := @players[consoleplayer];

    if amstate = am_only then
    begin
      SB_DrawPatch(0, 134, PatchH2BAR);
      SB_DrawPatch(38, 162, PatchKEYBAR);
      SB_DrawKeyBar;
    end
    else if R_FullStOn then
    begin
      SB_DrawFullScreenStuff;
    end
    else if not R_StOff then
    begin
      SB_DrawPatch(0, 134, PatchH2BAR);
      SB_DrawCommonBar;
      if inventory then
        SB_DrawInventoryBar
      else
      begin
        // Main interface
        SB_DrawPatch(38, 162, PatchSTATBAR);
        SB_DrawMainBar;
      end;
    end;
    SB_PaletteFlash(false);
    SB_DrawAnimatedIcons;
  end;

  V_CopyRectTransparent(0, sb_topoffset, SCN_SB, 320, 200 - sb_topoffset, 0, sb_topoffset, SCN_FG, true);
end;

//==============================================================================
//
// SB_Responder
//  Respond to keyboard input events,
//  intercept cheats.
//
//==============================================================================
function SB_Responder(ev: Pevent_t): boolean;
var
  buf: string;
  musnum: integer;
  map: integer;
  ateit: boolean; // JVAL Cheats ate the event

  function check_cheat(cht: Pcheatseq_t; key: char): boolean;
  var
    cht_ret: cheatstatus_t;
  begin
    cht_ret := cht_CheckCheat(cht, key);
    result := cht_ret = cht_acquired;
    if not ateit then
      ateit := (cht_ret in [{cht_pending, }cht_acquired]) // JVAL: 20211101 - Crouch
  end;

begin
  result := false;
  ateit := false;
  // Filter automap on/off.
  if (ev._type = ev_keyup) and
     ((ev.data1 and $ffff0000) = AM_MSGHEADER) then
  begin
  end
  // if a user keypress...
  else if ev._type = ev_keydown then
  begin
    if CPlayer = nil then
    begin
      result := false;
      exit;
    end;

    if CPlayer.mo = nil then
    begin
      result := false;
      exit;
    end;

    if not netgame then
    begin
      // b. - enabled for more debug fun.
      // if (gameskill != sk_nightmare) {

      // 'dqd' cheat for toggleable god mode
      if check_cheat(@cheat_quicken, Chr(ev.data1)) or
         check_cheat(@cheat_satan, Chr(ev.data1)) then
      begin
        SB_CmdGod;
      end
      else if check_cheat(@cheat_iddqd, Chr(ev.data1)) then
      begin
        SB_CmdIddqd;
      end
      // 'fa' cheat for killer fucking arsenal
      // rambo for Heretic
      else if check_cheat(@cheat_ammonokey, Chr(ev.data1)) or
              check_cheat(@cheat_rambo, Chr(ev.data1)) or
              check_cheat(@cheat_nra, Chr(ev.data1)) then
      begin
        SB_CmdIDFA;
      end
      // 'kfa' cheat for key full ammo
      else if check_cheat(@cheat_ammo, Chr(ev.data1)) then
      begin
        SB_CmdIDKFA;
      end
      else if check_cheat(@cheat_skel, Chr(ev.data1)) then
      begin
        SB_CmdSkel;
      end
      else if check_cheat(@cheat_ponce, Chr(ev.data1)) then
      begin
        SB_CmdPonce;
      end
      else if check_cheat(@cheat_noise, Chr(ev.data1)) then
      begin
        SB_CmdCheatSoundFunc;
      end
      else if check_cheat(@cheat_amap, Chr(ev.data1)) then
      begin
        SB_CmdIDDT;
      end
      // 'mus' cheat for changing music
      else if check_cheat(@cheat_mus, Chr(ev.data1)) then
      begin
        CPlayer._message := STSTR_MUS;
        cht_GetParam(@cheat_mus, buf);

        musnum := Ord(mus_map01) + (Ord(buf[1]) - Ord('0')) * 10 + Ord(buf[2]) - Ord('0') - 1;

        if (musnum > 0) and (musnum < nummusic) then
          S_ChangeMusic(musnum, true)
        else
          CPlayer._message := STSTR_NOMUS;
      end
      // Simplified, accepting "kitty", "noclip" and "idspispopd".
      // no clipping mode cheat
      else if check_cheat(@cheat_kitty, Chr(ev.data1)) or
              check_cheat(@cheat_noclip, Chr(ev.data1)) or
              check_cheat(@cheat_commercial_noclip, Chr(ev.data1)) or
              check_cheat(@cheat_casper, Chr(ev.data1)) then
      begin
        SB_CmdIDNoClip;
      end
      else if check_cheat(@cheat_massacre, Chr(ev.data1)) then
      begin
        SB_CmdMassacre;
      end
      else if check_cheat(@cheat_mypos, Chr(ev.data1)) then
      begin
        sprintf(buf, 'ang = %d, (x, y, z) = (%d, %d, %d)', [
          CPlayer.mo.angle div $B60B60,
          CPlayer.mo.x div FRACUNIT,
          CPlayer.mo.y div FRACUNIT,
          CPlayer.mo.z div FRACUNIT]);
        CPlayer._message := buf;
      end;
    end;

    // 'clev' change-level cheat
    if check_cheat(@cheat_clev, Chr(ev.data1)) then
    begin
      cht_GetParam(@cheat_clev, buf);

      CPlayer._message := STSTR_WLEV;

      map := (Ord(buf[1]) - Ord('0')) * 10 + Ord(buf[2]) - Ord('0');

      // So be it.
      if W_CheckNumForName(P_GetMapName(map)) > -1 then
      begin
        CPlayer._message := STSTR_CLEV;
        G_DeferedInitNewUntranslated(gameskill, map);
      end;
    end;
  end;
  if amstate <> am_only then
    result := result or ateit;
end;

end.
