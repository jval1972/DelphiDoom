//------------------------------------------------------------------------------
//
//  DelphiHeretic: A modified and improved Heretic port for Windows
//  based on original Linux Doom as published by "id Software", on
//  Heretic source as published by "Raven" software and DelphiDoom
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
//  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
//  02111-1307, USA.
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit sb_bar;

interface

uses
  d_event;

// Size of statusbar.
// Now sensitive for scaling.
const
  SB_HEIGHT = 40;
  SB_WIDTH = 320;
  SB_Y = 200 - SB_HEIGHT;

var
  SB_state: integer = -1;

// Public Data

var
  DebugSound: boolean; // debug flag for displaying sound info

  inventory: boolean;
  curpos: integer;
  inv_ptr: integer;
  ArtifactFlash: integer;

var
  playerkeys: integer = 0;

function SB_Responder(ev: Pevent_t): boolean;

procedure SB_Ticker;

procedure SB_Init;

procedure SB_Drawer;

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
  hu_stuff,
  h_strings,
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
  r_defs,
  r_data,
  r_draw,
  r_hires,
  r_main,
  s_sound,
  sounds,
  v_data,
  v_video,
  w_wad,
  z_zone;

// Private Data

var
  HealthMarker: integer;
  ChainWiggle: integer;
  CPlayer: Pplayer_t;
  playpalette: integer;

  PatchLTFACE: Ppatch_t;
  PatchRTFACE: Ppatch_t;
  PatchBARBACK: Ppatch_t;
  PatchCHAIN: Ppatch_t;
  PatchSTATBAR: Ppatch_t;
  PatchLIFEGEM: Ppatch_t;
  PatchLTFCTOP: Ppatch_t;
  PatchRTFCTOP: Ppatch_t;
  PatchSELECTBOX: Ppatch_t;
  PatchINVLFGEM1: Ppatch_t;
  PatchINVLFGEM2: Ppatch_t;
  PatchINVRTGEM1: Ppatch_t;
  PatchINVRTGEM2: Ppatch_t;
  PatchINumbers: array[0..9] of Ppatch_t;
  PatchNEGATIVE: Ppatch_t;
  PatchSmNumbers: array[0..9] of Ppatch_t;
  PatchBLACKSQ: Ppatch_t;
  PatchINVBAR: Ppatch_t;
  PatchARMCLEAR: Ppatch_t;
  PatchCHAINBACK: Ppatch_t;

  FontBNumBase: integer;
  spinbooklump: integer;
  spinflylump: integer;
  useartibase: integer;

//
// Commands
//
function SB_CmdCheckPlayerStatus: boolean;
begin
  if (CPlayer = nil) or (gamestate <> GS_LEVEL) or demoplayback or netgame then
  begin
    printf('You can''t specify the command at this time.'#13#10);
    result := false;
  end
  else
    result := true;
end;

procedure SB_CmdGod;
begin
  if not SB_CmdCheckPlayerStatus then
    exit;

  if CPlayer.playerstate <> PST_DEAD then
  begin
    CPlayer.cheats := CPlayer.cheats xor CF_GODMODE;
    if CPlayer.cheats and CF_GODMODE <> 0 then
    begin
      if CPlayer.mo <> nil then
        CPlayer.mo.health := mobjinfo[Ord(MT_PLAYER)].spawnhealth;

      CPlayer.health := mobjinfo[Ord(MT_PLAYER)].spawnhealth;
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

procedure SB_CmdIDFA;
var
  i: integer;
begin
  if not SB_CmdCheckPlayerStatus then
    exit;

  CPlayer.armorpoints := 200;
  CPlayer.armortype := 2;

  for i := 0 to Ord(NUMWEAPONS) - 1 do
    CPlayer.weaponowned[i] := 1;

  if gamemode = shareware then
  begin
    CPlayer.weaponowned[Ord(wp_skullrod)] := 0;
    CPlayer.weaponowned[Ord(wp_phoenixrod)] := 0;
    CPlayer.weaponowned[Ord(wp_mace)] := 0;
  end;

  for i := 0 to Ord(NUMAMMO) - 1 do
    CPlayer.ammo[i] := CPlayer.maxammo[i];

  CPlayer._message := STSTR_FAADDED;
end;

procedure SB_CmdIDKFA;
var
  i: integer;
begin
  if not SB_CmdCheckPlayerStatus then
    exit;

  CPlayer.armorpoints := 200;
  CPlayer.armortype := 2;

  for i := 0 to Ord(NUMWEAPONS) - 1 do
    CPlayer.weaponowned[i] := 1;

  if gamemode = shareware then
  begin
    CPlayer.weaponowned[Ord(wp_skullrod)] := 0;
    CPlayer.weaponowned[Ord(wp_phoenixrod)] := 0;
    CPlayer.weaponowned[Ord(wp_mace)] := 0;
  end;

  for i := 0 to Ord(NUMAMMO) - 1 do
    CPlayer.ammo[i] := CPlayer.maxammo[i];

   for i := 0 to Ord(NUMKEYCARDS) - 1 do
     CPlayer.keys[i] := true;
   playerkeys := 7; // Key refresh flags

   CPlayer._message := TXT_CHEATWEAPONS;
end;

procedure SB_CmdSkel;
var
  i: integer;
begin
  if not SB_CmdCheckPlayerStatus then
    exit;

  for i := 0 to Ord(NUMKEYCARDS) - 1 do
    CPlayer.keys[i] := true;

  playerkeys := 7; // Key refresh flags
  CPlayer._message := TXT_CHEATKEYS;
end;

procedure SB_CmdIDDT;
begin
  if not SB_CmdCheckPlayerStatus then
    exit;

  am_cheating := (am_cheating + 1) mod 3;
end;

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

procedure SB_CmdMassacre;
begin
  if not SB_CmdCheckPlayerStatus then
    exit;

  P_Massacre;
  CPlayer._message := TXT_CHEATMASSACRE;
end;

procedure SB_CmdPonce;
var
  health: integer;
begin
  if not SB_CmdCheckPlayerStatus then
    exit;

  if CPlayer.chickenTics <> 0 then
    health := p_maxchickenhealth
  else
    health := mobjinfo[Ord(MT_PLAYER)].spawnhealth;

  CPlayer.health := health;
  CPlayer.mo.health := health;

  P_SetMessage(CPlayer, TXT_CHEATHEALTH, false);
end;

procedure SB_CmdCheatSoundFunc;
begin
  DebugSound := not DebugSound;
  if DebugSound then
    P_SetMessage(CPlayer, TXT_CHEATSOUNDON, true)
  else
    P_SetMessage(CPlayer, TXT_CHEATSOUNDOFF, true);
end;

procedure SB_GiveArtifacts(arti: artitype_t; num: integer);
var
  i: integer;
begin
  if not SB_CmdCheckPlayerStatus then
    exit;

  i := 0;
  while (CPlayer.inventory[i]._type <> Ord(arti)) and (i < CPlayer.inventorySlotNum) do
    inc(i);

  if i = CPlayer.inventorySlotNum then
  begin
    CPlayer.inventory[i].count := num;
    CPlayer.inventory[i]._type := Ord(arti);
    inc(CPlayer.inventorySlotNum);
  end
  else
  begin
    if CPlayer.inventory[i].count >= p_maxartifacts then // Player already has p_maxartifacts of this item
      exit;
    inc(CPlayer.inventory[i].count, num);
    if CPlayer.inventory[i].count > p_maxartifacts then
      CPlayer.inventory[i].count := p_maxartifacts;
  end;

  if CPlayer.artifactCount = 0 then
    CPlayer.readyArtifact := arti;

  inc(CPlayer.artifactCount);
end;


procedure SB_CmdGimme(const art: string; const num: string);
var
  narti: integer;
  sarti: string;
  arti: artitype_t;
  howmany: integer;
begin
  if not SB_CmdCheckPlayerStatus then
    exit;

  if num = '' then
  begin
    if strlower(art) = 'all' then
    begin
      SB_GiveArtifacts(arti_health, p_maxartifacts);
      SB_GiveArtifacts(arti_fly, p_maxartifacts);
      SB_GiveArtifacts(arti_invulnerability, p_maxartifacts);
      SB_GiveArtifacts(arti_tomeofpower, p_maxartifacts);
      SB_GiveArtifacts(arti_invisibility, p_maxartifacts);
      SB_GiveArtifacts(arti_egg, p_maxartifacts);
      SB_GiveArtifacts(arti_torch, p_maxartifacts);
      SB_GiveArtifacts(arti_firebomb, p_maxartifacts);
      if gamemode <> shareware then
      begin
        SB_GiveArtifacts(arti_superhealth, p_maxartifacts);
        SB_GiveArtifacts(arti_teleport, p_maxartifacts);
      end;
      P_SetMessage(CPlayer, TXT_ARTIALL);
    end
    else
    begin
      printf('Usage is:'#13#10);
      printf('  gimme (artifact) (number)'#13#10);
      printf('    artifacts:'#13#10);
      printf('      invulnerability (0)'#13#10);
      printf('      invisibility (1)'#13#10);
      printf('      health (2)'#13#10);
      printf('      superhealth (3)'#13#10);
      printf('      tomeofpower (4)'#13#10);
      printf('      torch (5)'#13#10);
      printf('      firebomb (6)'#13#10);
      printf('      egg (7)'#13#10);
      printf('      fly (8)'#13#10);
      printf('      teleport (9)'#13#10);
      printf('    number: (1..%d)'#13#10, [p_maxartifacts]);
      printf('      all (no number required)'#13#10);
    end;
    exit;
  end;
  narti := atoi(art, -1);
  if narti in [0..9] then
    arti := artitype_t(narti + 1)
  else
  begin
    sarti := strlower(art);
    if sarti = 'invulnerability' then
      arti := arti_invulnerability
    else if sarti = 'invisibility' then
      arti := arti_invisibility
    else if sarti = 'health' then
      arti := arti_health
    else if sarti = 'superhealth' then
      arti := arti_superhealth
    else if sarti = 'tomeofpower' then
      arti := arti_tomeofpower
    else if sarti = 'torch' then
      arti := arti_torch
    else if sarti = 'firebomb' then
      arti := arti_firebomb
    else if sarti = 'egg' then
      arti := arti_egg
    else if sarti = 'fly' then
      arti := arti_fly
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

  case arti of
    arti_health:
      begin
        SB_GiveArtifacts(arti_health, howmany);
        P_SetMessage(CPlayer, TXT_ARTIHEALTH);
      end;

    arti_fly:
      begin
        SB_GiveArtifacts(arti_fly, howmany);
        P_SetMessage(CPlayer, TXT_ARTIFLY);
      end;

    arti_invulnerability:
      begin
        SB_GiveArtifacts(arti_invulnerability, howmany);
        P_SetMessage(CPlayer, TXT_ARTIINVULNERABILITY);
      end;

    arti_tomeofpower: // Arti_TomeOfPower
      begin
        SB_GiveArtifacts(arti_tomeofpower, howmany);
        P_SetMessage(CPlayer, TXT_ARTITOMEOFPOWER);
      end;

    arti_invisibility:
      begin
        SB_GiveArtifacts(arti_invisibility, howmany);
        P_SetMessage(CPlayer, TXT_ARTIINVISIBILITY, false);
      end;

    arti_egg:
      begin
        SB_GiveArtifacts(arti_egg, howmany);
        P_SetMessage(CPlayer, TXT_ARTIEGG, false);
      end;

    arti_superhealth:
      begin
        if gamemode <> shareware then
        begin
          SB_GiveArtifacts(arti_superhealth, howmany);
          P_SetMessage(CPlayer, TXT_ARTISUPERHEALTH, false);
        end
        else
          P_SetMessage(CPlayer, TXT_CHEATARTIFACTSFAIL, false);
      end;

    arti_torch:
      begin
        SB_GiveArtifacts(arti_torch, howmany);
        P_SetMessage(CPlayer, TXT_ARTITORCH);
      end;

    arti_firebomb:
      begin
        SB_GiveArtifacts(arti_firebomb, howmany);
        P_SetMessage(CPlayer, TXT_ARTIFIREBOMB);
      end;

    arti_teleport:
      begin
        if gamemode <> shareware then
        begin
          SB_GiveArtifacts(arti_teleport, howmany);
          P_SetMessage(CPlayer, TXT_ARTITELEPORT);
        end
        else
          P_SetMessage(CPlayer, TXT_CHEATARTIFACTSFAIL, false);
      end;
  end;
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

// massacre cheat
  cheat_massacre_seq: array[0..8] of char = (
    Chr($b6), Chr($a2), Chr($ea), Chr($ea), Chr($a2),
    Chr($e2), Chr($6a), Chr($a6), Chr($ff) // massacre
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
    Chr($b2), Chr($26), Chr($ea), Chr($f2), Chr($a6), Chr($36), Chr($ff) // skel
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
  cheat_ammo: cheatseq_t;
  cheat_ammonokey: cheatseq_t;
  cheat_noclip: cheatseq_t;
  cheat_commercial_noclip: cheatseq_t;
  cheat_skel: cheatseq_t;
  cheat_ponce: cheatseq_t;
  cheat_noise: cheatseq_t;

  cheat_powerup: array[0..6] of cheatseq_t;

  cheat_clev: cheatseq_t;
  cheat_mypos: cheatseq_t;
  cheat_massacre: cheatseq_t;
  cheat_kitty: cheatseq_t;
  cheat_rambo: cheatseq_t;


//---------------------------------------------------------------------------
//
// PROC SB_Init
//
//---------------------------------------------------------------------------

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
  cheat_ammo.sequence := get_cheatseq_string(cheat_ammo_seq);
  cheat_ammo.p := get_cheatseq_string(0);
  cheat_ammonokey.sequence := get_cheatseq_string(cheat_ammonokey_seq);
  cheat_ammonokey.p := get_cheatseq_string(0);
  cheat_noclip.sequence := get_cheatseq_string(cheat_noclip_seq);
  cheat_noclip.p := get_cheatseq_string(0);
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


  PatchLTFACE := W_CacheLumpName('LTFACE', PU_STATIC);
  PatchRTFACE := W_CacheLumpName('RTFACE', PU_STATIC);
  PatchBARBACK := W_CacheLumpName('BARBACK', PU_STATIC);
  PatchINVBAR := W_CacheLumpName('INVBAR', PU_STATIC);
  PatchCHAIN := W_CacheLumpName('CHAIN', PU_STATIC);
  if deathmatch <> 0 then
    PatchSTATBAR := W_CacheLumpName('STATBAR', PU_STATIC)
  else
    PatchSTATBAR := W_CacheLumpName('LIFEBAR', PU_STATIC);
  if not netgame then // single player game uses red life gem
    PatchLIFEGEM := W_CacheLumpName('LIFEGEM2', PU_STATIC)
  else
    PatchLIFEGEM := W_CacheLumpNum(W_GetNumForName('LIFEGEM0') + consoleplayer, PU_STATIC);

  PatchLTFCTOP := W_CacheLumpName('LTFCTOP', PU_STATIC);
  PatchRTFCTOP := W_CacheLumpName('RTFCTOP', PU_STATIC);
  PatchSELECTBOX := W_CacheLumpName('SELECTBO', PU_STATIC);
  PatchINVLFGEM1 := W_CacheLumpName('INVGEML1', PU_STATIC);
  PatchINVLFGEM2 := W_CacheLumpName('INVGEML2', PU_STATIC);
  PatchINVRTGEM1 := W_CacheLumpName('INVGEMR1', PU_STATIC);
  PatchINVRTGEM2 := W_CacheLumpName('INVGEMR2', PU_STATIC);
  PatchBLACKSQ := W_CacheLumpName('BLACKSQ', PU_STATIC);
  PatchARMCLEAR := W_CacheLumpName('ARMCLEAR', PU_STATIC);
  PatchCHAINBACK := W_CacheLumpName('CHAINBAC', PU_STATIC);

  startLump := W_GetNumForName('IN0');
  for i := 0 to 9 do
    PatchINumbers[i] := W_CacheLumpNum(startLump + i, PU_STATIC);

  PatchNEGATIVE := W_CacheLumpName('NEGNUM', PU_STATIC);
  FontBNumBase := W_GetNumForName('FONTB16');

  startLump := W_GetNumForName('SMALLIN0');
  for i := 0 to 9 do
    PatchSmNumbers[i] := W_CacheLumpNum(startLump + i, PU_STATIC);

  playpalette := W_GetNumForName('PLAYPAL');
  spinbooklump := W_GetNumForName('SPINBK0');
  spinflylump := W_GetNumForName('SPFLY0');
  useartibase := W_GetNumForName('USEARTIA');

  C_AddCmd('god, quicken', @SB_CmdGod);
  C_AddCmd('iddqd', @SB_CmdIddqd);
  C_AddCmd('givefullammo, rambo, idfa', @SB_CmdIDFA);
  C_AddCmd('lowgravity', @SB_CmdLowGravity);
  C_AddCmd('givefullammoandkeys, idkfa', @SB_CmdIDKFA);
  C_AddCmd('iddt', @SB_CmdIDDT);
  C_AddCmd('kitty, idspispopd, idclip', @SB_CmdIDNoClip);
  C_AddCmd('massacre', @SB_CmdMassacre);
  C_AddCmd('gimme', @SB_CmdGimme);
  C_AddCmd('skel, idskel, giveallkeys, idkeys', @SB_CmdSkel);
  C_AddCmd('ponce', @SB_CmdPonce);
  C_AddCmd('noise, debugsound', @SB_CmdCheatSoundFunc);

end;

//---------------------------------------------------------------------------
//
// PROC SB_Ticker
//
//---------------------------------------------------------------------------

procedure SB_Ticker;
var
  delta: integer;
  curHealth: integer;
begin
  if leveltime and 1 <> 0 then
    ChainWiggle := P_Random and 1;

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

//---------------------------------------------------------------------------
//
// PROC SB_DrINumber
//
// Draws a three digit number.
//
//---------------------------------------------------------------------------

procedure SB_DrINumber(val: integer; x, y: integer);
var
  patch: Ppatch_t;
  oldval: integer;
begin
  oldval := val;
  if val < 0 then
  begin
    if val < -9 then
      SB_DrawPatch(x + 1, y + 1, 'LAME')
    else
    begin
      val := -val;
      SB_DrawPatch(x + 18, y, PatchINumbers[val]);
      SB_DrawPatch(x + 9, y, PatchNEGATIVE);
    end;
    exit;
  end;
  if val > 99 then
  begin
    patch := PatchINumbers[val div 100];
    SB_DrawPatch(x, y, patch);
    val := val mod 100;
  end;
  if (val > 9) or (oldval > 99) then
  begin
    patch := PatchINumbers[val div 10];
    SB_DrawPatch(x + 9, y, patch);
    val := val mod 10;
  end;
  patch := PatchINumbers[val];
  SB_DrawPatch(x + 18, y, patch);
end;

//---------------------------------------------------------------------------
//
// PROC SB_DrBNumber
//
// Draws a three digit number using FontB
//
//---------------------------------------------------------------------------

procedure SB_DrBNumber(val: integer; x, y: integer);
var
  patch: Ppatch_t;
  xpos: integer;
  oldval: integer;
begin
  oldval := val;
  xpos := x;
  if val < 0 then
    val  := 0;
  if val > 99 then
  begin
    patch := W_CacheLumpNum(FontBNumBase + val div 100, PU_CACHE);
    SB_DrawPatch(xpos + 6 - patch.width div 2, y, patch); // V_DrawShadowedPatch
    val := val mod 100;
  end;
  xpos := xpos + 12;
  if (val > 9) or (oldval > 99) then
  begin
    patch := W_CacheLumpNum(FontBNumBase + val div 10, PU_CACHE);
    SB_DrawPatch(xpos + 6 - patch.width div 2, y, patch); // V_DrawShadowedPatch
    val := val mod 10;
  end;
  xpos := xpos + 12;
  patch := W_CacheLumpNum(FontBNumBase + val, PU_CACHE);
  SB_DrawPatch(xpos + 6 - patch.width div 2, y, patch); // V_DrawShadowedPatch
end;

//---------------------------------------------------------------------------
//
// PROC SB_DrSmallNumber
//
// Draws a small two digit number.
//
//---------------------------------------------------------------------------

procedure SB_DrSmallNumber(val: integer; x, y: integer);
var
  patch: Ppatch_t;
begin
  if val = 1 then
    exit;

  if val > 9 then
  begin
    patch := PatchSmNumbers[val div 10];
    SB_DrawPatch(x, y, patch);
    val := val mod 10;
  end;
  patch := PatchSmNumbers[val];
  SB_DrawPatch(x + 4, y, patch);
end;

//---------------------------------------------------------------------------
//
// PROC SB_ShadeLine
//
//---------------------------------------------------------------------------

procedure SB_ShadeLine(x, y: integer; height: integer; shade: integer);
var
  dest: PByte;
  shades: PByteArray;
begin
  shades := @colormaps[9 * 256 + shade * 2 * 256];
  dest := @screens[SCN_SB][y * 320 + x];
  while height > 0 do
  begin
    dest^ := shades[dest^];
    inc(dest, 320);
    dec(height);
  end;
end;

//---------------------------------------------------------------------------
//
// PROC SB_ShadeChain
//
//---------------------------------------------------------------------------

procedure SB_ShadeChain;
var
  i: integer;
begin
  for i := 0 to 15 do
  begin
    SB_ShadeLine(277 + i, 190, 10, i div 2);
    SB_ShadeLine(19 + i, 190, 10, 7 - (i div 2));
  end;
end;

//
//      Write a string using the hu_font
//
procedure SB_WriteText(x, y: integer; const str: string);
var
  w: integer;
  ch: integer;
  c: integer;
  cx: integer;
  cy: integer;
  len: integer;
begin
  len := Length(str);
  if len = 0 then
    exit;

  ch := 1;
  cx := x;
  cy := y;

  while true do
  begin
    if ch > len then
      break;

    c := Ord(str[ch]);
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
  patcharti: array[0..10] of string = (
    'ARTIBOX',    // none
    'ARTIINVU',   // invulnerability
    'ARTIINVS',   // invisibility
    'ARTIPTN2',   // health
    'ARTISPHL',   // superhealth
    'ARTIPWBK',   // tomeofpower
    'ARTITRCH',   // torch
    'ARTIFBMB',   // firebomb
    'ARTIEGGC',   // egg
    'ARTISOAR',   // fly
    'ARTIATLP'    // teleport
  );

  ammopic: array[0..5] of string = (
    'INAMGLD',
    'INAMBOW',
    'INAMBST',
    'INAMRAM',
    'INAMPNX',
    'INAMLOB'
  );

var
  oldhealth: integer = -1;
  hitCenterFrame: boolean = false;

procedure SB_DrawFullScreenStuff;
var
  i: integer;
  x: integer;
  temp: integer;
begin
  if CPlayer.mo.health > 0 then
    SB_DrBNumber(CPlayer.mo.health, 5, 180)
  else
    SB_DrBNumber(0, 5, 180);

  if deathmatch <> 0 then
  begin
    temp := 0;
    for i := 0 to MAXPLAYERS - 1 do
      if playeringame[i] then
        temp := temp + CPlayer.frags[i];
    SB_DrINumber(temp, 45, 185);
  end;
  if not inventory then
  begin
    if Ord(CPlayer.readyArtifact) > 0 then
    begin
      SB_DrawPatch(286, 170, 'ARTIBOX'); // V_DrawFuzzPatch
      SB_DrawPatch(286, 170, patcharti[Ord(CPlayer.readyArtifact)]);
      SB_DrSmallNumber(CPlayer.inventory[inv_ptr].count, 307, 192);
    end;
  end
  else
  begin
    x := inv_ptr - curpos;
    for i := 0 to 6 do
    begin
      SB_DrawPatch(50 + i * 31, 168, 'ARTIBOX');  // V_DrawFuzzPatch
      if (CPlayer.inventorySlotNum > x + i) and (CPlayer.inventory[x + i]._type <> Ord(arti_none)) then
      begin
        SB_DrawPatch(50 + i * 31, 168, patcharti[CPlayer.inventory[x + i]._type]);
        SB_DrSmallNumber(CPlayer.inventory[x + i].count, 69 + i * 31, 190);
      end;
    end;
    SB_DrawPatch(50 + curpos * 31, 197, PatchSELECTBOX);
    if x <> 0 then
    begin
      if leveltime and 4 = 0 then
        SB_DrawPatch(38, 167, PatchINVLFGEM1)
      else
        SB_DrawPatch(38, 167, PatchINVLFGEM2);
    end;
    if CPlayer.inventorySlotNum - x > 7 then
    begin
      if leveltime and 4 = 0 then
        SB_DrawPatch(269, 167, PatchINVRTGEM1)
      else
        SB_DrawPatch(269, 167, PatchINVRTGEM2);
    end;
  end;
end;


//---------------------------------------------------------------------------
//
// PROC SB_DrawCommonBar
//
//---------------------------------------------------------------------------

procedure SB_DrawCommonBar;
var
  chainY: integer;
  healthPos: integer;
begin
  SB_DrawPatch(0, 148, PatchLTFCTOP);
  SB_DrawPatch(290, 148, PatchRTFCTOP);

  if oldhealth <> HealthMarker then
  begin
    oldhealth := HealthMarker;
    healthPos := HealthMarker;
    if healthPos < 0 then
      healthPos := 0
    else if healthPos > 100 then
      healthPos := 100;
    healthPos := (healthPos * 256) div 100;
    if HealthMarker = CPlayer.mo.health then
      chainY := 191
    else
      chainY := 191 + ChainWiggle;
    SB_DrawPatch(0, 190, PatchCHAINBACK);
    SB_DrawPatch(2 + (healthPos mod 17), chainY, PatchCHAIN);
    SB_DrawPatch(17 + healthPos, chainY, PatchLIFEGEM);
    SB_DrawPatch(0, 190, PatchLTFACE);
    SB_DrawPatch(276, 190, PatchRTFACE);
    SB_ShadeChain;
  end;
end;

//---------------------------------------------------------------------------
//
// PROC SB_DrawMainBar
//
//---------------------------------------------------------------------------

procedure SB_DrawMainBar;
var
  i: integer;
  temp: integer;
begin
  // Ready artifact
  if ArtifactFlash <> 0 then
  begin
    SB_DrawPatch(180, 161, PatchBLACKSQ);
    SB_DrawPatch(182, 161, W_CacheLumpNum(useartibase + ArtifactFlash - 1, PU_CACHE));
    dec(ArtifactFlash);
  end
  else
  begin
    SB_DrawPatch(180, 161, PatchBLACKSQ);
    if Ord(CPlayer.readyArtifact) > 0 then
    begin
      SB_DrawPatch(179,160, patcharti[Ord(CPlayer.readyArtifact)]);
      SB_DrSmallNumber(CPlayer.inventory[inv_ptr].count, 201, 182);
    end;
  end;

  // Frags
  if deathmatch <> 0 then
  begin
    temp := 0;
    for i := 0 to MAXPLAYERS - 1 do
      temp := temp + CPlayer.frags[i];
    SB_DrawPatch(57, 171, PatchARMCLEAR);
    SB_DrINumber(temp, 61, 170);
  end
  else
  begin
    temp := HealthMarker;
    if temp < 0 then
      temp := 0;
    SB_DrawPatch(57, 171, PatchARMCLEAR);
    SB_DrINumber(temp, 61, 170);
  end;

  // Keys
  if CPlayer.keys[Ord(key_yellow)] then
    SB_DrawPatch(153, 164, 'ykeyicon');
  if CPlayer.keys[Ord(key_green)] then
    SB_DrawPatch(153, 172, 'gkeyicon');
  if CPlayer.keys[Ord(key_blue)] then
    SB_DrawPatch(153, 180, 'bkeyicon');

  // Ammo
  temp := CPlayer.ammo[Ord(wpnlev1info[Ord(CPlayer.readyweapon)].ammo)];
  SB_DrawPatch(108, 161, PatchBLACKSQ);
  if (temp <> 0) and (Ord(CPlayer.readyweapon) > 0) and (Ord(CPlayer.readyweapon) < 7) then
  begin
    SB_DrINumber(temp, 109, 162);
    SB_DrawPatch(111, 172, ammopic[Ord(CPlayer.readyweapon) - 1]);
  end;

  // Armor
  SB_DrawPatch(224, 171, PatchARMCLEAR);
  SB_DrINumber(CPlayer.armorpoints, 228, 170);
end;

//---------------------------------------------------------------------------
//
// PROC SB_DrawInventoryBar
//
//---------------------------------------------------------------------------

procedure SB_DrawInventoryBar;
var
  i: integer;
  x: integer;
begin
  x := inv_ptr - curpos;
  SB_DrawPatch(34, 160, PatchINVBAR);
  for i := 0 to 6 do
  begin
    //SB_DrawPatch(50+i*31, 160, W_CacheLumpName('ARTIBOX', PU_CACHE));
    if (CPlayer.inventorySlotNum > x + i) and (CPlayer.inventory[x+i]._type <> Ord(arti_none)) then
    begin
      SB_DrawPatch(50 + i * 31, 160, patcharti[CPlayer.inventory[x + i]._type]);
      SB_DrSmallNumber(CPlayer.inventory[x + i].count, 69 + i * 31, 182);
    end;
  end;
  SB_DrawPatch(50 + curpos * 31, 189, PatchSELECTBOX);
  if x <> 0 then
  begin
    if leveltime and 4 = 0 then
      SB_DrawPatch(38, 159, PatchINVLFGEM1)
    else
      SB_DrawPatch(38, 159, PatchINVLFGEM2);
  end;
  if CPlayer.inventorySlotNum - x > 7 then
  begin
    if leveltime and 4 = 0 then
      SB_DrawPatch(269, 159, PatchINVRTGEM1)
    else
      SB_DrawPatch(269, 159, PatchINVRTGEM2);
  end;
end;

// sets the new palette based upon current values of player.damagecount
// and player.bonuscount
var
  sb_palette: integer = -1;

procedure SB_PaletteFlash;
var
  palette: integer;
  pal: PByteArray;
begin
  CPlayer := @players[consoleplayer];

  if CPlayer.damagecount <> 0 then
  begin
    palette := _SHR((CPlayer.damagecount + 7), 3);
    if palette >= NUMREDPALS then
      palette := NUMREDPALS - 1;
    palette := palette + STARTREDPALS;
  end
  else if CPlayer.bonuscount <> 0 then
  begin
    palette := _SHR((CPlayer.bonuscount + 7), 3);
    if palette >= NUMBONUSPALS then
      palette := NUMBONUSPALS - 1;
    palette := palette + STARTBONUSPALS;
  end
  else
    palette := 0;

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

procedure SB_Drawer;
var
  frame: integer;
  icons_x: integer;
  icons_y: integer;
begin
  if firstinterpolation then
  begin
    MT_ZeroMemory(@screens[SCN_SB][sb_topoffset * 320], 320 * (200 - sb_topoffset));
    sb_topoffset := 158;

    // Sound info debug stuff
    if DebugSound then
      SB_DrawSoundInfo;

    CPlayer := @players[consoleplayer];
    if not R_StOff or (amstate = am_only) then
    begin
      if (viewheight = SCREENHEIGHT) and (amstate <> am_only) then
      begin
        SB_DrawFullScreenStuff;
        SB_state := -1;
      end
      else
      begin
        SB_DrawPatch(0, 158, PatchBARBACK);
        if players[consoleplayer].cheats and CF_GODMODE <> 0 then
        begin
          SB_DrawPatch(16, 167, 'GOD1');
          SB_DrawPatch(287, 167, 'GOD2');
        end;
        oldhealth := -1;
        SB_DrawCommonBar;
        if not inventory then
        begin
          // Main interface
          SB_DrawPatch(34, 160, PatchSTATBAR);
          SB_DrawMainBar;
          SB_state := 0;
        end
        else
        begin
          SB_DrawPatch(34, 160, PatchINVBAR);
          SB_DrawInventoryBar;
          SB_state := 1;
        end;
      end;
    end;

    SB_PaletteFlash;

    icons_x := (viewwindowx + viewwidth) * 320 div SCREENWIDTH - 20;
    icons_y := viewwindowy * 200 div SCREENHEIGHT + 25;

    // Flight icons
    if CPlayer.powers[Ord(pw_flight)] <> 0 then
    begin
      if (CPlayer.powers[Ord(pw_flight)] > BLINKTHRESHOLD) or (CPlayer.powers[Ord(pw_flight)] and 16 = 0) then
      begin
        frame := (leveltime div 3) and 15;
        if CPlayer.mo.flags2 and MF2_FLY <> 0 then
        begin
          if hitCenterFrame and (frame <> 15) and (frame <> 0) then
            SB_DrawPatch(icons_x, icons_y, W_CacheLumpNum(spinflylump + 15, PU_CACHE))
          else
          begin
            SB_DrawPatch(icons_x, icons_y, W_CacheLumpNum(spinflylump + frame, PU_CACHE));
            hitCenterFrame := false;
          end;
        end
        else
        begin
          if not hitCenterFrame and (frame <> 15) and (frame <> 0) then
          begin
            SB_DrawPatch(icons_x, icons_y, W_CacheLumpNum(spinflylump + frame, PU_CACHE));
            hitCenterFrame := false;
          end
          else
          begin
            SB_DrawPatch(icons_x, icons_y, W_CacheLumpNum(spinflylump + 15, PU_CACHE));
            hitCenterFrame := true;
          end;
        end;
      end;
      icons_y := icons_y + 25;
    end;

    // weaponlevel2 icons
    if (CPlayer.powers[Ord(pw_weaponlevel2)] <> 0) and (CPlayer.chickenTics = 0) then
    begin
      if (CPlayer.powers[Ord(pw_weaponlevel2)] > BLINKTHRESHOLD) or (CPlayer.powers[Ord(pw_weaponlevel2)] and 16 = 0) then
      begin
        frame := (leveltime div 3) and 15;
        SB_DrawPatch(icons_x, icons_y, W_CacheLumpNum(spinbooklump + frame, PU_CACHE));
      end;
    end;
  end;

  V_CopyRectTransparent(0, sb_topoffset, SCN_SB, 320, 200 - sb_topoffset, 0, sb_topoffset, SCN_FG, true);
end;


// Respond to keyboard input events,
//  intercept cheats.
function SB_Responder(ev: Pevent_t): boolean;
var
  buf: string;
  musnum: integer;
  epsd: integer;
  map: integer;
  ateit: boolean; // JVAL Cheats ate the event

  function check_cheat(cht: Pcheatseq_t; key: char): boolean;
  var
    cht_ret: cheatstatus_t;
  begin
    cht_ret := cht_CheckCheat(cht, key);
    result := cht_ret = cht_acquired;
    if not ateit then
      ateit := (cht_ret in [{cht_pending, }cht_acquired]); // JVAL: 20211101 - Crouch
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
      if check_cheat(@cheat_quicken, Chr(ev.data1)) then
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
              check_cheat(@cheat_rambo, Chr(ev.data1)) then
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

        musnum := Ord(mus_e1m1) + (Ord(buf[1]) - Ord('1')) * 9 + Ord(buf[2]) - Ord('1');

        if (musnum > 0) and (buf[2] <> '0') and
           ( ((musnum < 28) and (gamemode <> shareware)) or
             ((musnum < 10) and (gamemode = shareware))) then
          S_ChangeMusic(musnum, true)
        else
          CPlayer._message := STSTR_NOMUS;
      end
      // Simplified, accepting "kitty", "noclip" and "idspispopd".
      // no clipping mode cheat
      else if check_cheat(@cheat_kitty, Chr(ev.data1)) or
              check_cheat(@cheat_noclip, Chr(ev.data1)) or
              check_cheat(@cheat_commercial_noclip, Chr(ev.data1)) then
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

      epsd := Ord(buf[1]) - Ord('0');
      map := Ord(buf[2]) - Ord('0');
      // Catch invalid maps.
      if epsd < 1 then
        exit;

      if map < 1 then
        exit;

      if map > 9 then
        exit;

      // Ohmygod - this is not going to work.
      if (gamemode = extendedwad) and
         (epsd > 5)  then
        exit;

      if (gamemode = registered) and
         (epsd > 3) then
        exit;

      if (gamemode = shareware) and
         (epsd > 1) then
        exit;

      // So be it.
      if W_CheckNumForName(P_GetMapName(epsd, map)) > -1 then
      begin
        CPlayer._message := STSTR_CLEV;
        G_DeferedInitNew(gameskill, epsd, map);
      end;
    end;
  end;
  if amstate <> am_only then
    result := result or ateit;
end;

end.


