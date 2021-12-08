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
// DESCRIPTION:
//  Handling interactions (i.e., collisions).
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit p_inter;

interface

uses
  doomdef,
  xn_strings,
  sounds,
  doomstat,
  m_fixed,
  i_system,
  am_map,
  p_local,
  p_mobj_h,
  s_sound,
  d_player;

function P_GiveMana(player: Pplayer_t; mana: manatype_t; num: integer): boolean;

procedure P_HideSpecialThing(thing: Pmobj_t);

function P_MaxPlayerHealth(player: Pplayer_t): integer;

function P_GiveBody(player: Pplayer_t; num: integer): boolean;

function P_GiveKey(player: Pplayer_t; key: integer): boolean;

function P_GivePower(player: Pplayer_t; power: powertype_t): boolean;

procedure A_RestoreArtifact(arti: Pmobj_t);

procedure A_RestoreSpecialThing1(thing: Pmobj_t);

procedure A_RestoreSpecialThing2(thing: Pmobj_t);

procedure P_MinotaurSlam(source: Pmobj_t; target: Pmobj_t);

function P_MorphPlayer(player: Pplayer_t): boolean;

function P_ActiveMinotaur(master: Pplayer_t): Pmobj_t;

function P_MorphMonster(actor: Pmobj_t): boolean;

procedure P_AutoUseHealth(player: Pplayer_t; saveHealth: integer);

procedure P_TryPickupWeapon(player: Pplayer_t; weaponClass: pclass_t;
  weaponType: weapontype_t; weapon: Pmobj_t; const _message: string);

procedure P_TryPickupWeaponPiece(player: Pplayer_t; matchClass: pclass_t;
  pieceValue: integer; pieceMobj: Pmobj_t);

function P_GiveArmor(player: Pplayer_t; armortype: armortype_t; amount: integer): boolean;

function P_GiveArtifact(player: Pplayer_t; arti: artitype_t; mo: Pmobj_t): boolean;

procedure P_SetDormantArtifact(arti: Pmobj_t);

procedure P_TryPickupArtifact(player: Pplayer_t; artifactType: artitype_t;
  artifact: Pmobj_t);

procedure P_TouchSpecialThing(special: Pmobj_t; toucher: Pmobj_t);

function P_SpawnDroppedMobj(x, y, z: fixed_t; _type: integer): Pmobj_t;

procedure P_DamageMobj(target: Pmobj_t; inflictor: Pmobj_t; source: Pmobj_t; damage: integer);

procedure P_FallingDamage(player: Pplayer_t);

procedure P_PoisonPlayer(player: Pplayer_t; poisoner: Pmobj_t; poison: integer);

procedure P_PoisonDamage(player: Pplayer_t; source: Pmobj_t; damage: integer;
  playPainSound: boolean);

procedure P_CmdSuicide;

procedure P_SetMessage(player: Pplayer_t; const msg: string; ultmsg: boolean = false);

procedure P_SetYellowMessage(player: Pplayer_t; const msg: string; ultmsg: boolean = false);

procedure P_ClearMessage(player: Pplayer_t);


var
  p_maxmorphhealth: integer = 30;
  p_maxartifacts: integer = 25;
  p_quartzflaskhealth: integer = 25;
  p_mysticurnhealth: integer = 100;

  ArmorIncrement: array[0..Ord(NUMCLASSES) - 1, 0..Ord(NUMARMOR) - 1] of integer = (
    ( 25 * FRACUNIT, 20 * FRACUNIT, 15 * FRACUNIT,  5 * FRACUNIT ),
    ( 10 * FRACUNIT, 25 * FRACUNIT,  5 * FRACUNIT, 20 * FRACUNIT ),
    (  5 * FRACUNIT, 15 * FRACUNIT, 10 * FRACUNIT, 25 * FRACUNIT ),
    ( 0, 0, 0, 0 )
  );

  AutoArmorSave: array[0..Ord(NUMCLASSES) - 1] of integer =
  (15 * FRACUNIT, 10 * FRACUNIT, 5 * FRACUNIT, 0 );


implementation

uses
  c_cmds,
  d_delphi,
  d_think,
  info_h,
  info,
  m_menu,
  m_rnd,
  g_demo,
  g_game,
  p_mobj,
  p_obituaries,
  p_3dfloors,
  p_pspr,
  p_pspr_h,
  p_tick,
  p_user,
  p_enemy,
  p_spec,
  p_acs,
  ps_main, // JVAL: Script Events
  r_defs,
  r_main,
  sb_bar,
  tables;

const
  BONUSADD = 6;

//
// GET STUFF
//

//--------------------------------------------------------------------------
//
// FUNC P_GiveMana
//
// Returns true if the player accepted the mana, false if it was
// refused (player has MAX_MANA).
//
//--------------------------------------------------------------------------
function P_GiveMana(player: Pplayer_t; mana: manatype_t; num: integer): boolean;
var
  prevmana: integer;
begin
  if (mana = MANA_NONE) or (mana = MANA_BOTH) then
  begin
    result := false;
    exit;
  end;

  if (Ord(mana) < 0) or (Ord(mana) > Ord(NUMMANA)) then
    I_Error('P_GiveMana(): bad type %d', [Ord(mana)]);

  if player.mana[Ord(mana)] = MAX_MANA then
  begin
    result := false;
    exit;
  end;

  if (gameskill = sk_baby) or (gameskill = sk_nightmare) then
  begin
    // extra ammo in baby mode and nightmare mode
    num := num  + num div 2
  end;

  prevmana := player.mana[Ord(mana)];
  player.mana[Ord(mana)] := player.mana[Ord(mana)] + num;

  if player.mana[Ord(mana)] > MAX_MANA then
    player.mana[Ord(mana)] := MAX_MANA;

  if(player._class = PCLASS_FIGHTER) and (player.readyweapon = WP_SECOND) and (mana = MANA_1) and (prevmana <= 0) then
    P_SetPsprite(player, Ord(ps_weapon), S_FAXEREADY_G);

  result := true;
end;

//----------------------------------------------------------------------------
//
// PROC P_HideSpecialThing
//
//----------------------------------------------------------------------------

procedure P_HideSpecialThing(thing: Pmobj_t);
begin
  thing.flags := thing.flags and not MF_SPECIAL;
  thing.flags2 := thing.flags2 or MF2_DONTDRAW;
  P_SetMobjState(thing, S_HIDESPECIAL1);
end;


//----------------------------------------------------------------------------
//
// PROC P_MaxPlayerHealth
//
// JVAL: Retrieve maximum player health
//
//----------------------------------------------------------------------------

function P_MaxPlayerHealth(player: Pplayer_t): integer;
begin
  if player.morphTics <> 0 then
    result := p_maxmorphhealth
  else
  begin
    case PlayerClass[P_GetPlayerNum(player)] of
      PCLASS_FIGHTER: result := mobjinfo[Ord(MT_PLAYER_FIGHTER)].spawnhealth;
      PCLASS_CLERIC:  result := mobjinfo[Ord(MT_PLAYER_CLERIC)].spawnhealth;
      PCLASS_MAGE:    result := mobjinfo[Ord(MT_PLAYER_MAGE)].spawnhealth;
    else
      result := p_maxmorphhealth;
    end;
  end;
end;

//
// P_GiveBody
// Returns false if the body isn't needed at all
//
function P_GiveBody(player: Pplayer_t; num: integer): boolean;
var
  maxhealth: integer;
begin

  maxhealth := P_MaxPlayerHealth(player);

  if player.health >= maxhealth then
  begin
    result := false;
    exit;
  end;

  player.health := player.health + num;
  if player.health > maxhealth then
    player.health := maxhealth;
  player.mo.health := player.health;

  result := true;
end;

//
// P_GiveKey
//
function P_GiveKey(player: Pplayer_t; key: integer): boolean;
begin
  if player.keys and (1 shl key) <> 0 then
  begin
    result := false;
    exit;
  end;

  player.bonuscount := player.bonuscount + BONUSADD;
  player.keys := player.keys or (1 shl key);
  result := true;
end;

//
// P_GivePower
//
function P_GivePower(player: Pplayer_t; power: powertype_t): boolean;
begin
  if power = pw_invulnerability then
  begin
    if player.powers[Ord(power)] > BLINKTHRESHOLD then
    begin // Already have it
      result := false;
      exit;
    end;
    player.powers[Ord(power)] := INVULNTICS;
    player.mo.flags2 := player.mo.flags2 or MF2_INVULNERABLE;
    player.mo.flags_ex := player.mo.flags_ex or MF_EX_INVULNERABLE;
    if player._class = PCLASS_MAGE then
      player.mo.flags2_ex := player.mo.flags2_ex or MF2_EX_REFLECTIVE;
    result := true;
    exit;
  end;

  if power = pw_flight then
  begin
    if player.powers[Ord(power)] > BLINKTHRESHOLD then
    begin // Already have it
      result := false;
      exit;
    end;
    player.powers[Ord(power)] := FLIGHTTICS;
    player.mo.flags2 := player.mo.flags2 or MF2_FLY;
    player.mo.flags := player.mo.flags or MF_NOGRAVITY;
    if player.mo.z <= player.mo.floorz then
      player.flyheight := 10; // thrust the player in the air a bit
    result := true;
    exit;
  end;

  if power = pw_infrared then
  begin
    if player.powers[Ord(power)] > BLINKTHRESHOLD then
    begin // Already have it
      result := false;
      exit;
    end;
    player.powers[Ord(power)] := INFRATICS;
    result := true;
    exit;
  end;

  if power = pw_speed then
  begin
    if player.powers[Ord(power)] > BLINKTHRESHOLD then
    begin // Already have it
      result := false;
      exit;
    end;
    player.powers[Ord(power)] := SPEEDTICS;
    result := true;
    exit;
  end;

  if power = pw_minotaur then
  begin
    // Doesn't matter if already have power, renew ticker
    player.powers[Ord(power)] := MAULATORTICS;
    result := true;
    exit;
  end;

  if player.powers[Ord(power)] <> 0 then
    result := false // already got it
  else
  begin
    player.powers[Ord(power)] := 1;
    result := true;
  end;
end;


//---------------------------------------------------------------------------
//
// PROC A_RestoreArtifact
//
//---------------------------------------------------------------------------

procedure A_RestoreArtifact(arti: Pmobj_t);
begin
  arti.flags := arti.flags or MF_SPECIAL;
  P_SetMobjState(arti, statenum_t(arti.info.spawnstate));
  S_StartSound(arti, Ord(SFX_RESPAWN));
end;

//---------------------------------------------------------------------------
//
// PROC A_RestoreSpecialThing1
//
// Make a special thing visible again.
//
//---------------------------------------------------------------------------

procedure A_RestoreSpecialThing1(thing: Pmobj_t);
begin
  thing.flags2 := thing.flags2 and not MF2_DONTDRAW;
  S_StartSound(thing, Ord(SFX_RESPAWN));
end;

//---------------------------------------------------------------------------
//
// PROC A_RestoreSpecialThing2
//
//---------------------------------------------------------------------------

procedure A_RestoreSpecialThing2(thing: Pmobj_t);
begin
  thing.flags := thing.flags or MF_SPECIAL;
  P_SetMobjState(thing, statenum_t(thing.info.spawnstate));
end;

//---------------------------------------------------------------------------
//
// FUNC P_MinotaurSlam
//
//---------------------------------------------------------------------------

procedure P_MinotaurSlam(source: Pmobj_t; target: Pmobj_t);
var
  angle: angle_t;
  thrust: fixed_t;
begin
  angle := R_PointToAngle2(source.x, source.y, target.x, target.y);
  angle := angle shr ANGLETOFINESHIFT;
  thrust := 16 * FRACUNIT + (P_Random * 1024);
  target.momx := target.momx + FixedMul(thrust, finecosine[angle]);
  target.momy := target.momy + FixedMul(thrust, finesine[angle]);
  P_DamageMobj(target, nil, nil, HITDICE(4));
  if target.player <> nil then
    target.reactiontime := 14 + (P_Random and 7);
end;

//---------------------------------------------------------------------------
//
// FUNC P_MorphPlayer
//
// Returns true if the player gets turned into a pig.
//
//---------------------------------------------------------------------------

function P_MorphPlayer(player: Pplayer_t): boolean;
var
  pmo: Pmobj_t;
  fog: Pmobj_t;
  beast: Pmobj_t;
  x, y, z: fixed_t;
  angle: angle_t;
  oldFlags2: integer;
begin
  if player.morphTics <> 0 then
  begin
    result := false;
    exit;
  end;

  if player.powers[Ord(pw_invulnerability)] <> 0 then
  begin // Immune when invulnerable
    result := false;
    exit;
  end;

  pmo := player.mo;
  x := pmo.x;
  y := pmo.y;
  z := pmo.z;
  angle := pmo.angle;
  oldFlags2 := pmo.flags2;
  P_SetMobjState(pmo, S_FREETARGMOBJ);
  fog := P_SpawnMobj(x, y, z + TELEFOGHEIGHT, Ord(MT_TFOG));
  S_StartSound(fog, Ord(SFX_TELEPORT));
  beast := P_SpawnMobj(x, y, z, Ord(MT_PIGPLAYER));
  beast.special1 := Ord(player.readyweapon);
  beast.angle := angle;
  beast.player := player;
  player.health := p_maxmorphhealth;
  beast.health := p_maxmorphhealth;
  player.mo := beast;
  memset(@player.armorpoints[0], 0, Ord(NUMARMOR) * SizeOf(integer));
  player._class := PCLASS_PIG;
  if oldFlags2 and MF2_FLY <> 0 then
    beast.flags2 := beast.flags2 or MF2_FLY;
  player.morphTics := MORPHTICS;
  P_ActivateMorphWeapon(player);
  result := true;
end;

// Search thinker list for minotaur
function P_ActiveMinotaur(master: Pplayer_t): Pmobj_t;
var
  mo: Pmobj_t;
  plr: Pplayer_t;
  think: Pthinker_t;
  starttime: PInteger;
begin
  think := thinkercap.next;
  while think <> @thinkercap do
  begin
    if @think._function.acp1 <> @P_MobjThinker then
    begin
      think := think.next;
      continue;
    end;

    mo := Pmobj_t(think);
    if mo._type <> Ord(MT_MINOTAUR) then
    begin
      think := think.next;
      continue;
    end;

    if mo.health <= 0 then
    begin
      think := think.next;
      continue;
    end;

    if mo.flags and MF_COUNTKILL = 0 then // for morphed minotaurs
    begin
      think := think.next;
      continue;
    end;

    if mo.flags and MF_CORPSE <> 0 then
    begin
      think := think.next;
      continue;
    end;

    starttime := PInteger(@mo.args[0]);
    if leveltime - starttime^ >= MAULATORTICS then
    begin
      think := think.next;
      continue;
    end;

    plr := Pmobj_t(mo.special1).player;
    if plr = master then
    begin
      result := mo;
      exit;
    end;
  end;
  result := nil;
end;

//---------------------------------------------------------------------------
//
// FUNC P_MorphMonster
//
//---------------------------------------------------------------------------

function P_MorphMonster(actor: Pmobj_t): boolean;
var
  fog: Pmobj_t;
  beast: Pmobj_t;
  moType: integer;
  x, y, z: fixed_t;
  oldmo: mobj_t;
  i: integer;
  master: Pmobj_t;
begin
  if actor.player <> nil then
  begin
    result := false;
    exit;
  end;

  if actor.flags and MF_COUNTKILL = 0 then
  begin
    result := false;
    exit;
  end;

  if (actor.flags2 and MF2_BOSS <> 0) or (actor.flags_ex and MF_EX_BOSS <> 0) then
  begin
    result := false;
    exit;
  end;

  moType := actor._type;
  case moType of
    Ord(MT_PIG),
    Ord(MT_FIGHTER_BOSS),
    Ord(MT_CLERIC_BOSS),
    Ord(MT_MAGE_BOSS):
      begin
        result := false;
        exit;
      end;
  end;

  oldmo := actor^;
  x := oldmo.x;
  y := oldmo.y;
  z := oldmo.z;
  P_RemoveMobjFromTIDList(actor);
  P_SetMobjState(actor, S_FREETARGMOBJ);
  fog := P_SpawnMobj(x, y, z + TELEFOGHEIGHT, Ord(MT_TFOG));
  S_StartSound(fog, Ord(SFX_TELEPORT));
  beast := P_SpawnMobj(x, y, z, Ord(MT_PIG));
  beast.special2 := moType;
  beast.special1 := MORPHTICS + P_Random;
  beast.flags := beast.flags or (oldmo.flags and MF_SHADOW);
  beast.target := oldmo.target;
  beast.angle := oldmo.angle;
  beast.tid := oldmo.tid;
  beast.special := oldmo.special;
  P_InsertMobjIntoTIDList(beast, oldmo.tid);
  for i := 0 to 4 do
    beast.args[i] := oldmo.args[i];

  // check for turning off minotaur power for active icon
  if moType = Ord(MT_MINOTAUR) then
  begin
    master := Pmobj_t(oldmo.special1);
    if master.health > 0 then
      if P_ActiveMinotaur(master.player) = nil then
        Pplayer_t(master.player).powers[Ord(pw_minotaur)] := 0;
  end;

  result := true;
end;

//---------------------------------------------------------------------------
//
// PROC P_AutoUseHealth
//
//---------------------------------------------------------------------------

procedure P_AutoUseHealth(player: Pplayer_t; saveHealth: integer);
var
  i: integer;
  count: integer;
  normalCount: integer;
  normalSlot: integer;
  superCount: integer;
  superSlot: integer;
begin
  normalCount := 0;
  superCount := 0;
  normalSlot := 0;
  superSlot := 0;

  for i := 0 to player.inventorySlotNum - 1 do
  begin
    if player.inventory[i]._type = Ord(arti_health) then
    begin
      normalSlot := i;
      normalCount := player.inventory[i].count;
    end
    else if player.inventory[i]._type = Ord(arti_superhealth) then
    begin
      superSlot := i;
      superCount := player.inventory[i].count;
    end;
  end;

  if (gameskill = sk_baby) and (normalCount * 25 >= saveHealth) then
  begin // Use quartz flasks
    count := (saveHealth + 24) div 25;
    for i := 0 to count - 1 do
    begin
      player.health := player.health + 25;
      P_PlayerRemoveArtifact(player, normalSlot);
    end;
  end
  else if superCount * 100 >= saveHealth then
  begin // Use mystic urns
    count := (saveHealth + 99) div 100;
    for i := 0 to count - 1 do
    begin
      player.health := player.health + 100;
      P_PlayerRemoveArtifact(player, superSlot);
    end;
  end
  else if (gameskill = sk_baby) and (superCount * 100 + normalCount * 25 >= saveHealth) then
  begin // Use mystic urns and quartz flasks
    count := (saveHealth + 24) div 25;
    saveHealth := saveHealth - count * 25;
    for i := 0 to count - 1 do
    begin
      player.health := player.health + 25;
      P_PlayerRemoveArtifact(player, normalSlot);
    end;
    count := (saveHealth + 99) div 100;
    for i := 0 to count - 1 do
    begin
      player.health := player.health + 100;
      P_PlayerRemoveArtifact(player, normalSlot);
    end;
  end;

  player.mo.health := player.health;
end;

//==========================================================================
//
// P_TryPickupWeapon
//
//==========================================================================

procedure P_TryPickupWeapon(player: Pplayer_t; weaponClass: pclass_t;
  weaponType: weapontype_t; weapon: Pmobj_t; const _message: string);
var
  remove, gaveMana, gaveWeapon: boolean;
begin
  remove := true;
  if player._class <> weaponClass then
  begin // Wrong class, but try to pick up for mana
    if netgame and (deathmatch = 0) then
    begin // Can't pick up weapons for other classes in coop netplay
      exit;
    end;
    if weaponType = WP_SECOND then
    begin
      if not P_GiveMana(player, MANA_1, 25) then
      begin
        exit;
      end;
    end
    else
    begin
      if not P_GiveMana(player, MANA_2, 25) then
      begin
        exit;
      end;
    end;
  end
  else if netgame and (deathmatch = 0) then
  begin // Cooperative net-game
    if player.weaponowned[Ord(weaponType)] then
    begin
      exit;
    end;
    player.weaponowned[Ord(weaponType)] := true;
    if weaponType = WP_SECOND then
    begin
      P_GiveMana(player, MANA_1, 25);
    end
    else
    begin
      P_GiveMana(player, MANA_2, 25);
    end;
    player.pendingweapon := weaponType;
    remove := false;
  end
  else
  begin // Deathmatch or single player game
    if weaponType = WP_SECOND then
    begin
      gaveMana := P_GiveMana(player, MANA_1, 25);
    end
    else
    begin
      gaveMana := P_GiveMana(player, MANA_2, 25);
    end;
    if player.weaponowned[Ord(weaponType)] then
    begin
      gaveWeapon := false;
    end
    else
    begin
      gaveWeapon := true;
      player.weaponowned[Ord(weaponType)] := true;
      if Ord(weaponType) > Ord(player.readyweapon) then
      begin // Only switch to more powerful weapons
        player.pendingweapon := weaponType;
      end;
    end;
    if not (gaveWeapon or gaveMana) then
    begin // Player didn't need the weapon or any mana
      exit;
    end;
  end;

  P_SetMessage(player, _message, false);
  if weapon.special <> 0 then
  begin
    P_ExecuteLineSpecial(weapon.special, @weapon.args, nil, 0, player.mo);
    weapon.special := 0;
  end;

  if remove then
  begin
    if (deathmatch <> 0) and (weapon.flags2 and MF2_DROPPED = 0) then
    begin
      P_HideSpecialThing(weapon);
    end
    else
    begin
      P_RemoveMobj(weapon);
    end;
  end;

  player.bonuscount := player.bonuscount + BONUSADD;
  if player = @players[consoleplayer] then
  begin
    S_StartSound(nil, Ord(SFX_PICKUP_WEAPON));
    SB_PaletteFlash(false);
  end;
end;

//==========================================================================
//
// P_TryPickupWeaponPiece
//
//==========================================================================

var
  pieceValueTrans: array[0..4] of integer = (
    0,                              // 0: never
    WPIECE1 or WPIECE2 or WPIECE3,  // WPIECE1 (1)
    WPIECE2 or WPIECE3,             // WPIECE2 (2)
    0,                              // 3: never
    WPIECE3                         // WPIECE3 (4)
  );

procedure P_TryPickupWeaponPiece(player: Pplayer_t; matchClass: pclass_t;
  pieceValue: integer; pieceMobj: Pmobj_t);
var
  remove, checkAssembled, gaveWeapon: boolean;
  gaveMana: boolean;
  gaveMana1: boolean;
  gaveMana2: boolean;
begin
  remove := true;
  checkAssembled := true;
  gaveWeapon := false;
  if player._class <> matchClass then
  begin // Wrong class, but try to pick up for mana
    if netgame and (deathmatch = 0) then
    begin // Can't pick up wrong-class weapons in coop netplay
      exit;
    end;
    checkAssembled := false;
    gaveMana1 := P_GiveMana(player, MANA_1, 20);
    gaveMana2 := P_GiveMana(player, MANA_2, 20);
    gavemana := gaveMana1 or gaveMana2;
    if not gaveMana then
    begin // Didn't need the mana, so don't pick it up
      exit;
    end;
  end
  else if netgame and (deathmatch = 0) then
  begin // Cooperative net-game
    if player.pieces and pieceValue <> 0 then
    begin // Already has the piece
      exit;
    end;
    pieceValue := pieceValueTrans[pieceValue];
    P_GiveMana(player, MANA_1, 20);
    P_GiveMana(player, MANA_2, 20);
    remove := false;
  end
  else
  begin // Deathmatch or single player game
    gaveMana1 := P_GiveMana(player, MANA_1, 20);
    gaveMana2 := P_GiveMana(player, MANA_2, 20);
    gavemana := gavemana1 or gavemana2;
    if player.pieces and pieceValue <> 0 then
    begin // Already has the piece, check if mana needed
      if not gaveMana then
      begin // Didn't need the mana, so don't pick it up
        exit;
      end;
      checkAssembled := false;
    end;
  end;

  // Pick up the weapon piece
  if pieceMobj.special <> 0 then
  begin
    P_ExecuteLineSpecial(pieceMobj.special, @pieceMobj.args, nil, 0, player.mo);
    pieceMobj.special := 0;
  end;
  if remove then
  begin
    if (deathmatch <> 0) and (pieceMobj.flags2 and MF2_DROPPED = 0) then
    begin
      P_HideSpecialThing(pieceMobj);
    end
    else
    begin
      P_RemoveMobj(pieceMobj);
    end;
  end;
  player.bonuscount := player.bonuscount + BONUSADD;
  if player = @players[consoleplayer] then
  begin
    SB_PaletteFlash(false);
  end;

  // Check if fourth weapon assembled
  if checkAssembled then
  begin
    player.pieces := player.pieces or pieceValue;
    if player.pieces = (WPIECE1 or WPIECE2 or WPIECE3) then
    begin
      gaveWeapon := true;
      player.weaponowned[Ord(WP_FOURTH)] := true;
      player.pendingweapon := WP_FOURTH;
    end;
  end;

  if gaveWeapon then
  begin
    P_SetMessage(player, fourthWeaponText[Ord(matchClass)], false);
    // Play the build-sound full volume for all players
    S_StartSound(nil, Ord(SFX_WEAPON_BUILD));
  end
  else
  begin
    P_SetMessage(player, weaponPieceText[Ord(matchClass)], false);
    if player = @players[consoleplayer] then
    begin
      S_StartSound(nil, Ord(SFX_PICKUP_WEAPON));
    end;
  end;
end;


//---------------------------------------------------------------------------
//
// FUNC P_GiveArmor
//
// Returns false if the armor is worse than the current armor.
//
//---------------------------------------------------------------------------

function P_GiveArmor(player: Pplayer_t; armortype: armortype_t; amount: integer): boolean;
var
  hits: integer;
  totalArmor: integer;
begin
  if amount = -1 then
  begin
    hits := ArmorIncrement[Ord(player._class), Ord(armortype)];
    if player.armorpoints[Ord(armortype)] >= hits then
    begin
      result := false;
      exit;
    end
    else
    begin
      player.armorpoints[Ord(armortype)] := hits;
    end;
  end
  else
  begin
    hits := amount * 5 * FRACUNIT;
    totalArmor := player.armorpoints[Ord(ARMOR_ARMOR)] +
                  player.armorpoints[Ord(ARMOR_SHIELD)] +
                  player.armorpoints[Ord(ARMOR_HELMET)] +
                  player.armorpoints[Ord(ARMOR_AMULET)] +
                  AutoArmorSave[Ord(player._class)];
    if totalArmor < ArmorMax[Ord(player._class)] * 5 * FRACUNIT then
    begin
      player.armorpoints[Ord(armortype)] := player.armorpoints[Ord(armortype)] + hits;
    end
    else
    begin
      result := false;
      exit;
    end;
  end;
  result := true;
end;


//---------------------------------------------------------------------------
//
// FUNC P_GiveArtifact
//
// Returns true if artifact accepted.
//
//---------------------------------------------------------------------------

function P_GiveArtifact(player: Pplayer_t; arti: artitype_t; mo: Pmobj_t): boolean;
var
  i, j: integer;
  slidePointer: boolean;
begin
  slidePointer := false;
  i := 0;
  while (player.inventory[i]._type <> Ord(arti)) and (i < player.inventorySlotNum) do
    inc(i);

  if i = player.inventorySlotNum then
  begin
    if arti < artitype_t(arti_firstpuzzitem) then
    begin
      i := 0;
      while (player.inventory[i]._type < arti_firstpuzzitem) and
            (i < player.inventorySlotNum) do
        inc(i);
      if i <> player.inventorySlotNum then
      begin
        j := player.inventorySlotNum;
        while j > i do
        begin
          player.inventory[j].count := player.inventory[j - 1].count;
          player.inventory[j]._type := player.inventory[j - 1]._type;
          slidePointer := true;
          dec(j);
        end;
      end;
    end;
    player.inventory[i].count := 1;
    player.inventory[i]._type := Ord(arti);
    inc(player.inventorySlotNum);
  end
  else
  begin
    if (Ord(arti) >= arti_firstpuzzitem) and netgame and (deathmatch = 0) then
    begin // Can't carry more than 1 puzzle item in coop netplay
      result := false;
      exit;
    end;

    if player.inventory[i].count >= p_maxartifacts then
    begin // Player already has p_maxartifacts of this item
      result := false;
      exit;
    end;
    inc(player.inventory[i].count);
  end;
  if player.artifactCount = 0 then
  begin
    player.readyArtifact := arti;
  end
  else if (player = @players[consoleplayer]) and slidePointer and (i <= inv_ptr) then
  begin
    inc(inv_ptr);
    inc(curpos);
    if curpos > 6 then
      curpos := 6;
  end;
  inc(player.artifactCount);
  result := true;
end;


//==========================================================================
//
// SetDormantArtifact
//
// Removes the MF_SPECIAL flag and initiates the artifact pickup
// animation.
//
//==========================================================================

procedure P_SetDormantArtifact(arti: Pmobj_t);
begin
  arti.flags := arti.flags and not MF_SPECIAL;
  if (deathmatch <> 0) and (arti.flags2 and MF2_DROPPED = 0) then
  begin
    if arti._type = Ord(MT_ARTIINVULNERABILITY) then
    begin
      P_SetMobjState(arti, S_DORMANTARTI3_1);
    end
    else if (arti._type = Ord(MT_SUMMONMAULATOR)) or (arti._type = Ord(MT_ARTIFLY)) then
    begin
      P_SetMobjState(arti, S_DORMANTARTI2_1);
    end
    else
    begin
      P_SetMobjState(arti, S_DORMANTARTI1_1);
    end;
  end
  else
  begin // Don't respawn
    P_SetMobjState(arti, S_DEADARTI1);
  end;
end;


//==========================================================================
//
// P_TryPickupArtifact
//
//==========================================================================

procedure P_TryPickupArtifact(player: Pplayer_t; artifactType: artitype_t;
  artifact: Pmobj_t);
begin
  if P_GiveArtifact(player, artifactType, artifact) then
  begin
    if artifact.special <> 0 then
    begin
      P_ExecuteLineSpecial(artifact.special, @artifact.args, nil, 0, nil);
      artifact.special := 0;
    end;
    player.bonuscount := player.bonuscount + BONUSADD;
    if artifactType < artitype_t(arti_firstpuzzitem) then
    begin
      P_SetDormantArtifact(artifact);
      S_StartSound(artifact, Ord(SFX_PICKUP_ARTIFACT));
      P_SetMessage(player, artifactMessages[Ord(artifactType)], false);
    end
    else
    begin // Puzzle item
      S_StartSound(nil, Ord(SFX_PICKUP_ITEM));
      P_SetMessage(player, artifactMessages[Ord(artifactType)], true);
      if not netgame or (deathmatch <> 0) then
      begin // Remove puzzle items if not cooperative netplay
        P_RemoveMobj(artifact);
      end;
    end;
  end;
end;

//---------------------------------------------------------------------------
//
// PROC P_TouchSpecialThing
//
//---------------------------------------------------------------------------

procedure P_TouchSpecialThing(special: Pmobj_t; toucher: Pmobj_t);
var
  player: Pplayer_t;
  delta: fixed_t;
  sound: integer;
  respawn: boolean;
begin
  delta := special.z - toucher.z;
  if (delta > toucher.height) or (delta < -32 * FRACUNIT) then
  begin // Out of reach
    exit;
  end;

  if toucher.health <= 0 then
  begin // Toucher is dead
    exit;
  end;

  sound := Ord(SFX_PICKUP_ITEM);
  player := toucher.player;
  respawn := true;
  case special.sprite of
    // Items
    Ord(SPR_PTN1): // Item_HealingPotion
      begin
        if not P_GiveBody(player, 10) then
        begin
          exit;
        end;
        P_SetMessage(player, TXT_ITEMHEALTH, false);
      end;

    Ord(SPR_ARM1):
      begin
        if not P_GiveArmor(player, ARMOR_ARMOR, -1) then
        begin
          exit;
        end;
        P_SetMessage(player, TXT_ARMOR1, false);
      end;

    Ord(SPR_ARM2):
      begin
        if not P_GiveArmor(player, ARMOR_SHIELD, -1) then
        begin
          exit;
        end;
        P_SetMessage(player, TXT_ARMOR2, false);
      end;

    Ord(SPR_ARM3):
      begin
        if not P_GiveArmor(player, ARMOR_HELMET, -1) then
        begin
          exit;
        end;
        P_SetMessage(player, TXT_ARMOR3, false);
      end;

    Ord(SPR_ARM4):
      begin
        if not P_GiveArmor(player, ARMOR_AMULET, -1) then
        begin
          exit;
        end;
        P_SetMessage(player, TXT_ARMOR4, false);
      end;

    // Keys
    Ord(SPR_KEY1),
    Ord(SPR_KEY2),
    Ord(SPR_KEY3),
    Ord(SPR_KEY4),
    Ord(SPR_KEY5),
    Ord(SPR_KEY6),
    Ord(SPR_KEY7),
    Ord(SPR_KEY8),
    Ord(SPR_KEY9),
    Ord(SPR_KEYA),
    Ord(SPR_KEYB):
      begin
        if not P_GiveKey(player, special.sprite - Ord(SPR_KEY1)) then
        begin
          exit;
        end;
        P_SetMessage(player, TextKeyMessages[special.sprite - Ord(SPR_KEY1)]);
        sound := Ord(SFX_PICKUP_KEY);

        // Check and process the special now in the key doesn't
        // get removed for coop netplay
        if special.special <> 0 then
        begin
          P_ExecuteLineSpecial(special.special, @special.args, nil, 0, toucher);
          special.special := 0;
        end;

        if netgame then // Only remove keys in single player game
        begin
          player.bonuscount := player.bonuscount + BONUSADD;
          if player = @players[consoleplayer] then
          begin
            S_StartSound(nil, sound);
            SB_PaletteFlash(false);
          end;
        end;
      end;

    // Artifacts
    Ord(SPR_PTN2):
      begin
        P_TryPickupArtifact(player, arti_health, special);
        exit;
      end;

    Ord(SPR_SOAR):
      begin
        P_TryPickupArtifact(player, arti_fly, special);
        exit;
      end;

    Ord(SPR_INVU):
      begin
        P_TryPickupArtifact(player, arti_invulnerability, special);
        exit;
      end;

    Ord(SPR_SUMN):
      begin
        P_TryPickupArtifact(player, arti_summon, special);
        exit;
      end;

    Ord(SPR_PORK):
      begin
        P_TryPickupArtifact(player, arti_egg, special);
        exit;
      end;

    Ord(SPR_SPHL):
      begin
        P_TryPickupArtifact(player, arti_superhealth, special);
        exit;
      end;

    Ord(SPR_HRAD):
      begin
        P_TryPickupArtifact(player, arti_healingradius, special);
        exit;
      end;

    Ord(SPR_TRCH):
      begin
        P_TryPickupArtifact(player, arti_torch, special);
        exit;
      end;

    Ord(SPR_ATLP):
      begin
        P_TryPickupArtifact(player, arti_teleport, special);
        exit;
      end;

    Ord(SPR_TELO):
      begin
        P_TryPickupArtifact(player, arti_teleportother, special);
        exit;
      end;

    Ord(SPR_PSBG):
      begin
        P_TryPickupArtifact(player, arti_poisonbag, special);
        exit;
      end;

    Ord(SPR_SPED):
      begin
        P_TryPickupArtifact(player, arti_speed, special);
        exit;
      end;

    Ord(SPR_BMAN):
      begin
        P_TryPickupArtifact(player, arti_boostmana, special);
        exit;
      end;

    Ord(SPR_BRAC):
      begin
        P_TryPickupArtifact(player, arti_boostarmor, special);
        exit;
      end;

    Ord(SPR_BLST):
      begin
        P_TryPickupArtifact(player, arti_blastradius, special);
        exit;
      end;

    // Puzzle artifacts
    Ord(SPR_ASKU):
      begin
        P_TryPickupArtifact(player, arti_puzzskull, special);
        exit;
      end;

    Ord(SPR_ABGM):
      begin
        P_TryPickupArtifact(player, arti_puzzgembig, special);
        exit;
      end;

    Ord(SPR_AGMR):
      begin
        P_TryPickupArtifact(player, arti_puzzgemred, special);
        exit;
      end;

    Ord(SPR_AGMG):
      begin
        P_TryPickupArtifact(player, arti_puzzgemgreen1, special);
        exit;
      end;

    Ord(SPR_AGG2):
      begin
        P_TryPickupArtifact(player, arti_puzzgemgreen2, special);
        exit;
      end;

    Ord(SPR_AGMB):
      begin
        P_TryPickupArtifact(player, arti_puzzgemblue1, special);
        exit;
      end;

    Ord(SPR_AGB2):
      begin
        P_TryPickupArtifact(player, arti_puzzgemblue2, special);
        exit;
      end;

    Ord(SPR_ABK1):
      begin
        P_TryPickupArtifact(player, arti_puzzbook1, special);
        exit;
      end;

    Ord(SPR_ABK2):
      begin
        P_TryPickupArtifact(player, arti_puzzbook2, special);
        exit;
      end;

    Ord(SPR_ASK2):
      begin
        P_TryPickupArtifact(player, arti_puzzskull2, special);
        exit;
      end;

    Ord(SPR_AFWP):
      begin
        P_TryPickupArtifact(player, arti_puzzfweapon, special);
        exit;
      end;

    Ord(SPR_ACWP):
      begin
        P_TryPickupArtifact(player, arti_puzzcweapon, special);
        exit;
      end;

    Ord(SPR_AMWP):
      begin
        P_TryPickupArtifact(player, arti_puzzmweapon, special);
        exit;
      end;

    Ord(SPR_AGER):
      begin
        P_TryPickupArtifact(player, arti_puzzgear1, special);
        exit;
      end;

    Ord(SPR_AGR2):
      begin
        P_TryPickupArtifact(player, arti_puzzgear2, special);
        exit;
      end;

    Ord(SPR_AGR3):
      begin
        P_TryPickupArtifact(player, arti_puzzgear3, special);
        exit;
      end;

    Ord(SPR_AGR4):
      begin
        P_TryPickupArtifact(player, arti_puzzgear4, special);
        exit;
      end;

    // Mana
    Ord(SPR_MAN1):
      begin
        if not P_GiveMana(player, MANA_1, 15) then
        begin
          exit;
        end;
        P_SetMessage(player, TXT_MANA_1, false);
      end;

    Ord(SPR_MAN2):
      begin
        if not P_GiveMana(player, MANA_2, 15) then
        begin
          exit;
        end;
        P_SetMessage(player, TXT_MANA_2, false);
      end;

    Ord(SPR_MAN3): // Double Mana Dodecahedron
      begin
        if not P_GiveMana(player, MANA_1, 20) then
        begin
          if not P_GiveMana(player, MANA_2, 20) then
          begin
            exit;
          end;
        end
        else
        begin
          P_GiveMana(player, MANA_2, 20);
        end;
        P_SetMessage(player, TXT_MANA_BOTH, false);
      end;

    // 2nd and 3rd Mage Weapons
    Ord(SPR_WMCS): // Frost Shards
      begin
        P_TryPickupWeapon(player, PCLASS_MAGE, WP_SECOND, special, TXT_WEAPON_M2);
        exit;
      end;

    Ord(SPR_WMLG): // Arc of Death
      begin
        P_TryPickupWeapon(player, PCLASS_MAGE, WP_THIRD, special, TXT_WEAPON_M3);
        exit;
      end;

    // 2nd and 3rd Fighter Weapons
    Ord(SPR_WFAX): // Timon's Axe
      begin
        P_TryPickupWeapon(player, PCLASS_FIGHTER, WP_SECOND, special, TXT_WEAPON_F2);
        exit;
      end;

    Ord(SPR_WFHM): // Hammer of Retribution
      begin
        P_TryPickupWeapon(player, PCLASS_FIGHTER, WP_THIRD, special, TXT_WEAPON_F3);
        exit;
      end;

    // 2nd and 3rd Cleric Weapons
    Ord(SPR_WCSS): // Serpent Staff
      begin
        P_TryPickupWeapon(player, PCLASS_CLERIC, WP_SECOND, special, TXT_WEAPON_C2);
        exit;
      end;

    Ord(SPR_WCFM): // Firestorm
      begin
        P_TryPickupWeapon(player, PCLASS_CLERIC, WP_THIRD, special, TXT_WEAPON_C3);
        exit;
      end;

    // Fourth Weapon Pieces
    Ord(SPR_WFR1):
      begin
        P_TryPickupWeaponPiece(player, PCLASS_FIGHTER, WPIECE1, special);
        exit;
      end;

    Ord(SPR_WFR2):
      begin
        P_TryPickupWeaponPiece(player, PCLASS_FIGHTER, WPIECE2, special);
        exit;
      end;

    Ord(SPR_WFR3):
      begin
        P_TryPickupWeaponPiece(player, PCLASS_FIGHTER, WPIECE3, special);
        exit;
      end;

    Ord(SPR_WCH1):
      begin
        P_TryPickupWeaponPiece(player, PCLASS_CLERIC, WPIECE1, special);
        exit;
      end;

    Ord(SPR_WCH2):
      begin
        P_TryPickupWeaponPiece(player, PCLASS_CLERIC, WPIECE2, special);
        exit;
      end;

    Ord(SPR_WCH3):
      begin
        P_TryPickupWeaponPiece(player, PCLASS_CLERIC, WPIECE3, special);
        exit;
      end;

    Ord(SPR_WMS1):
      begin
        P_TryPickupWeaponPiece(player, PCLASS_MAGE, WPIECE1, special);
        exit;
      end;

    Ord(SPR_WMS2):
      begin
        P_TryPickupWeaponPiece(player, PCLASS_MAGE, WPIECE2, special);
        exit;
      end;

    Ord(SPR_WMS3):
      begin
        P_TryPickupWeaponPiece(player, PCLASS_MAGE, WPIECE3, special);
        exit;
      end;

  else
    I_Error('P_SpecialThing(): Unknown gettable thing (sprite number = %d', [special.sprite]);
  end;

  if special.special <> 0 then
  begin
    P_ExecuteLineSpecial(special.special, @special.args, nil, 0, toucher);
    special.special := 0;
  end;

  if (deathmatch <> 0) and respawn and (special.flags2 and MF2_DROPPED = 0) then
  begin
    P_HideSpecialThing(special);
  end
  else
  begin
    P_RemoveMobj(special);
  end;

  player.bonuscount := player.bonuscount + BONUSADD;
  if player = @players[consoleplayer] then
  begin
    S_StartSound(nil, sound);
    SB_PaletteFlash(false);
  end;
end;

//---------------------------------------------------------------------------
//
// PROC P_KillMobj
//
//---------------------------------------------------------------------------
function P_SpawnDroppedMobj(x, y, z: fixed_t; _type: integer): Pmobj_t;
begin
  result := P_SpawnMobj(x, y, z, _type);
  // JVAL Dropped items fall down to floor.
  if not compatibilitymode then
  begin
    result.z := result.z + 32 * FRACUNIT;
    result.momz := 4 * FRACUNIT;
  end;
end;

procedure P_KillMobj(source: Pmobj_t; target: Pmobj_t);
var
  dummy: integer;
  master: Pmobj_t;
  pl: Pplayer_t;
  item: integer;
  gibhealth: integer;
  zpos: integer;
begin
  target.flags := target.flags and not (MF_SHOOTABLE or MF_FLOAT or MF_SKULLFLY);
  if target.flags3_ex and MF3_EX_NOGRAVITYDEATH = 0 then
    target.flags := target.flags and not MF_NOGRAVITY;

  target.flags3_ex := target.flags3_ex and not MF3_EX_BOUNCE;
  target.flags := target.flags or MF_CORPSE or MF_DROPOFF;
  target.flags2 := target.flags2 and not MF2_PASSMOBJ;
  target.height := _SHR2(target.height);
  if ((target.flags and MF_COUNTKILL <> 0) or (target._type = Ord(MT_ZBELL))) and
     (target.special <> 0) then
  begin // Initiate monster death actions
    if target._type = Ord(MT_SORCBOSS) then
    begin
      dummy := 0;
      P_StartACS(target.special, 0, PByteArray(@dummy), target, nil, 0);
    end
    else
    begin
      P_ExecuteLineSpecial(target.special, @target.args, nil, 0, target);
    end;
  end;
  if (source <> nil) and (source.player <> nil) then
  begin // Check for frag changes
  // JVAL SOS Removed!
{    if target.player <> nil then
    begin
      if target = source then
      begin // Self-frag
        dec(Pplayer_t(target.player).frags[(integer(target.player) - integer(@players[0])) div SizeOf(player_t)]);
        if cmdfrag and netgame and source.player = @players[consoleplayer] then
        begin // Send out a frag count packet
          NET_SendFrags(source.player);
        end;
      end
      else
      begin
        inc(source.player.frags[(integer(target.player) - integer(players)) div SizeOf(player_t)]);
        if cmdfrag and netgame and (source.player = @players[consoleplayer]) then
        begin // Send out a frag count packet
          NET_SendFrags(source.player);
        end;
      end;
    end;}
  end;

  if target.player <> nil then
  begin // Player death
  // JVAL SOS Removed!
{
    if source = nil then
    begin // Self-frag
      dec(target.player.frags[(integer(target.player) - integer(players)) div SizeOf(player_t)]);
      if cmdfrag and netgame and (target.player = @players[consoleplayer]) then
      begin // Send out a frag count packet
        NET_SendFrags(target.player);
      end;
    end;
}
    pl := target.player;
    target.flags := target.flags and not MF_SOLID;
    target.flags2 := target.flags2 and not MF2_FLY;
    pl.powers[Ord(pw_flight)] := 0;
    pl.playerstate := PST_DEAD;

    // JVAL
    // Save the attacker coordinates
    if pl.attacker <> nil then
    begin
      pl.attackerx := pl.attacker.x;
      pl.attackery := pl.attacker.y;
    end;

    P_DropWeapon(target.player);

    if (pl = @players[consoleplayer]) and (amstate = am_only) then
    begin
      // don't die in auto map,
      // switch view prior to dying
      amstate := am_inactive;
      AM_Stop;
    end;

    if target.flags2 and MF2_FIREDAMAGE <> 0 then
    begin // Player flame death
      case pl._class of
        PCLASS_FIGHTER:
          begin
            S_StartSound(target, Ord(SFX_PLAYER_FIGHTER_BURN_DEATH));
            P_SetMobjState(target, S_PLAY_F_FDTH1);
            exit;
          end;
        PCLASS_CLERIC:
          begin
            S_StartSound(target, Ord(SFX_PLAYER_CLERIC_BURN_DEATH));
            P_SetMobjState(target, S_PLAY_C_FDTH1);
            exit;
          end;
        PCLASS_MAGE:
          begin
            S_StartSound(target, Ord(SFX_PLAYER_MAGE_BURN_DEATH));
            P_SetMobjState(target, S_PLAY_M_FDTH1);
            exit;
          end;
      end;
    end;
    if target.flags2 and MF2_ICEDAMAGE <> 0 then
    begin // Player ice death
      target.flags := target.flags and not _SHL(7, MF_TRANSSHIFT); //no translation
      target.flags := target.flags or MF_ICECORPSE;
      case pl._class of
        PCLASS_FIGHTER:
          begin
            P_SetMobjState(target, S_FPLAY_ICE);
            exit;
          end;
        PCLASS_CLERIC:
          begin
            P_SetMobjState(target, S_CPLAY_ICE);
            exit;
          end;
        PCLASS_MAGE:
          begin
            P_SetMobjState(target, S_MPLAY_ICE);
            exit;
          end;
        PCLASS_PIG:
          begin
            P_SetMobjState(target, S_PIG_ICE);
            exit;
          end;
      end;
    end;
  end;
  if target.flags2 and MF2_FIREDAMAGE <> 0 then
  begin
    if (target._type = Ord(MT_FIGHTER_BOSS)) or
       (target._type = Ord(MT_CLERIC_BOSS)) or
       (target._type = Ord(MT_MAGE_BOSS)) then
    begin
      case target._type of

        Ord(MT_FIGHTER_BOSS):
          begin
            S_StartSound(target, Ord(SFX_PLAYER_FIGHTER_BURN_DEATH));
            P_SetMobjState(target, S_PLAY_F_FDTH1);
            exit;
          end;

        Ord(MT_CLERIC_BOSS):
          begin
            S_StartSound(target, Ord(SFX_PLAYER_CLERIC_BURN_DEATH));
            P_SetMobjState(target, S_PLAY_C_FDTH1);
            exit;
          end;

        Ord(MT_MAGE_BOSS):
          begin
            S_StartSound(target, Ord(SFX_PLAYER_MAGE_BURN_DEATH));
            P_SetMobjState(target, S_PLAY_M_FDTH1);
            exit;
          end;

      end;
    end
    else if target._type = Ord(MT_TREEDESTRUCTIBLE) then
    begin
      P_SetMobjState(target, S_ZTREEDES_X1);
      target.height := 24 * FRACUNIT;
      S_StartSound(target, Ord(SFX_TREE_EXPLODE));
      exit;
    end;
  end;
  if target.flags2 and MF2_ICEDAMAGE <> 0 then
  begin
    target.flags := target.flags or MF_ICECORPSE;
    case target._type of
      Ord(MT_BISHOP):
        begin
          P_SetMobjState(target, S_BISHOP_ICE);
          exit;
        end;
      Ord(MT_CENTAUR),
      Ord(MT_CENTAURLEADER):
        begin
          P_SetMobjState(target, S_CENTAUR_ICE);
          exit;
        end;
      Ord(MT_DEMON),
      Ord(MT_DEMON2):
        begin
          P_SetMobjState(target, S_DEMON_ICE);
          exit;
        end;
      Ord(MT_SERPENT),
      Ord(MT_SERPENTLEADER):
        begin
          P_SetMobjState(target, S_SERPENT_ICE);
          exit;
        end;
      Ord(MT_WRAITH),
      Ord(MT_WRAITHB):
        begin
          P_SetMobjState(target, S_WRAITH_ICE);
          exit;
        end;
      Ord(MT_ETTIN):
        begin
          P_SetMobjState(target, S_ETTIN_ICE1);
          exit;
        end;
      Ord(MT_FIREDEMON):
        begin
          P_SetMobjState(target, S_FIRED_ICE1);
          exit;
        end;
      Ord(MT_FIGHTER_BOSS):
        begin
          P_SetMobjState(target, S_FIGHTER_ICE);
          exit;
        end;
      Ord(MT_CLERIC_BOSS):
        begin
          P_SetMobjState(target, S_CLERIC_ICE);
          exit;
        end;
      Ord(MT_MAGE_BOSS):
        begin
          P_SetMobjState(target, S_MAGE_ICE);
          exit;
        end;
      Ord(MT_PIG):
        begin
          P_SetMobjState(target, S_PIG_ICE);
          exit;
        end;
    else
      target.flags := target.flags and not MF_ICECORPSE;
    end;
  end;

  if target._type = Ord(MT_MINOTAUR) then
  begin
    master := Pmobj_t(target.special1);
    if master <> nil then
    begin
      if master.health > 0 then
      begin
        if P_ActiveMinotaur(master.player) = nil then
        begin
          Pplayer_t(master.player).powers[Ord(pw_minotaur)] := 0;
        end;
      end;
    end;
  end
  else if target._type = Ord(MT_TREEDESTRUCTIBLE) then
  begin
    target.height := 24 * FRACUNIT;
  end;

  gibhealth := target.info.gibhealth;
  if gibhealth >= 0 then
    gibhealth := -_SHR1(target.info.spawnhealth);

  if (target.health < gibhealth) and
     (target.info.xdeathstate <> 0) then
  begin // Extreme death
    P_SetMobjState(target, statenum_t(target.info.xdeathstate));
  end
  else
  begin // Normal death
    if (target._type = Ord(MT_FIREDEMON)) and
       (target.z <= target.floorz + 2 * FRACUNIT) and
       (target.info.xdeathstate <> 0) then
    begin
      // This is to fix the imps' staying in fall state
      P_SetMobjState(target, statenum_t(target.info.xdeathstate));
    end
    else
    begin
      P_SetMobjState(target, statenum_t(target.info.deathstate));
    end;
  end;
  target.tics := target.tics - P_Random and 3;

  if target.player <> nil then    // JVAL: Script Events
    PS_EventPlayerDied(pDiff(@players[0], target.player, SizeOf(player_t)), source);
  PS_EventActorDied(target, source); // JVAL: Script Events

  // Drop stuff.
  // This determines the kind of object spawned
  // during the death frame of a thing.

  if target.info.dropitem > 0 then
    item := target.info.dropitem
  else
    item := 0;

// JVAL: Check if dropitem is set to drop a custom item.
  if target.flags2_ex and MF2_EX_CUSTOMDROPITEM <> 0 then
    item := target.dropitem;

// JVAL: 20200301 - Fix P_SpawnDroppedMobj() bug
  if item <= 0 then
    Exit;

  if target.flags4_ex and MF4_EX_ABSOLUTEDROPITEMPOS <> 0 then
    P_SpawnDroppedMobj(target.x, target.y, target.z, item)
  else if Psubsector_t(target.subsector).sector.midsec >= 0 then // JVAL: 3d Floors
  begin
    zpos := P_3dFloorHeight(target);
    P_SpawnDroppedMobj(target.x, target.y, zpos, item)
  end
  else
    P_SpawnDroppedMobj(target.x, target.y, ONFLOORZ, item);
end;


//
//=================
//
// P_DamageMobj
//
// Damages both enemies and players
// inflictor is the thing that caused the damage
//     creature or missile, can be NULL (slime, etc)
// source is the thing to target after taking damage
//    creature or NULL
// Source and inflictor are the same for melee attacks
// source can be null for barrel explosions and other environmental stuff
//==================
//

procedure P_DamageMobj(target: Pmobj_t; inflictor: Pmobj_t; source: Pmobj_t; damage: integer);
var
  ang: angle_t;
  saved: integer;
  savedPercent: fixed_t;
  player: Pplayer_t;
  master: Pmobj_t;
  thrust: fixed_t;
  i: integer;
  mass: integer;
begin
  if target.flags and MF_SHOOTABLE = 0 then
  begin
    // Shouldn't happen
    exit;
  end;

  if target.flags2_ex and MF2_EX_NODAMAGE <> 0 then
  begin
    exit;
  end;

  if inflictor <> nil then
  begin
    if inflictor.flags3_ex and MF3_EX_FREEZEDAMAGE <> 0 then
    begin
      if target.flags3_ex and MF3_EX_NOFREEZEDAMAGE <> 0 then
        exit;
      if target.flags3_ex and MF3_EX_FREEZEDAMAGERESIST <> 0 then
        if damage > 1 then
          damage := _SHR1(damage);
    end;
    if inflictor.flags3_ex and MF3_EX_FLAMEDAMAGE <> 0 then
    begin
      if target.flags3_ex and MF3_EX_NOFLAMEDAMAGE <> 0 then
        exit;
      if target.flags4_ex and MF4_EX_FLAMEDAMAGERESIST <> 0 then
        if damage > 1 then
          damage := _SHR1(damage);
    end;
  end;

  if target.health <= 0 then
  begin
    if (inflictor <> nil) and (inflictor.flags2 and MF2_ICEDAMAGE <> 0) then
    begin
      exit;
    end
    else if target.flags and MF_ICECORPSE <> 0 then // frozen
    begin
      target.tics := 1;
      target.momx := 0;
      target.momy := 0;
    end;
    exit;
  end;

  if ((target.flags2 and MF2_INVULNERABLE <> 0) or (target.flags_ex and MF_EX_INVULNERABLE <> 0)) and
     (damage < 10000) then
  begin // mobj is invulnerable
    if target.player <> nil then
      exit;  // for player, no exceptions

    if inflictor <> nil then
    begin
      case inflictor._type of
        // These inflictors aren't foiled by invulnerability
        Ord(MT_HOLY_FX),
        Ord(MT_POISONCLOUD),
        Ord(MT_FIREBOMB):
          begin
          end;
      else
        exit;
      end;
    end
    else
      exit;
  end;

  player := target.player;
  if player <> nil then
    if (damage < 1000) and
      ((player.cheats and CF_GODMODE <> 0) or (player.powers[Ord(pw_invulnerability)] <> 0)) then
      exit;

  if target.flags and MF_SKULLFLY <> 0 then
  begin
    target.momx := 0;
    target.momy := 0;
    target.momz := 0;
  end;

  if target.flags2 and MF2_DORMANT <> 0 then
  begin
    // Invulnerable, and won't wake up
    exit;
  end;

  if (player <> nil) and (gameskill = sk_baby) then
  begin
    // Take half damage in trainer mode
    damage := _SHR1(damage);
  end;
  // Special damage types
  if inflictor <> nil then
  begin

    case inflictor._type of

      Ord(MT_EGGFX):
        begin
          if player <> nil then
            P_MorphPlayer(player)
          else
            P_MorphMonster(target);
          exit; // Always return
        end;

      Ord(MT_TELOTHER_FX1),
      Ord(MT_TELOTHER_FX2),
      Ord(MT_TELOTHER_FX3),
      Ord(MT_TELOTHER_FX4),
      Ord(MT_TELOTHER_FX5):
        begin
          if (target.flags and MF_COUNTKILL <> 0) and
             (target._type <> Ord(MT_SERPENT)) and
             (target._type <> Ord(MT_SERPENTLEADER)) and
             (target.flags2 and MF2_BOSS = 0) and
             (target.flags_ex and MF_EX_BOSS = 0) then
            P_TeleportOther(target);
          exit;
        end;

      Ord(MT_MINOTAUR):
        begin
          if inflictor.flags and MF_SKULLFLY <> 0 then
          begin // Slam only when in charge mode
            P_MinotaurSlam(inflictor, target);
            exit;
          end;
        end;

      Ord(MT_BISH_FX):
        begin
          // Bishops are just too nasty
          damage := _SHR1(damage);
        end;

      Ord(MT_SHARDFX1):
        begin
          case inflictor.special2 of
            3: damage := damage * 8;
            2: damage := damage * 4;
            1: damage := damage * 2;
          end;
        end;

      Ord(MT_CSTAFF_MISSILE):
        begin
          // Cleric Serpent Staff does poison damage
          if target.player <> nil then
          begin
            P_PoisonPlayer(target.player, source, 20);
            damage := _SHR1(damage);
          end;
        end;

      Ord(MT_ICEGUY_FX2):
        begin
          damage := _SHR1(damage);
        end;

      Ord(MT_POISONDART):
        begin
          if player <> nil then
          begin
            P_PoisonPlayer(player, source, 20);
            damage := _SHR1(damage);
          end;
        end;

      Ord(MT_POISONCLOUD):
        begin
          if player <> nil then
          begin
            if player.poisoncount < 4 then
            begin
              P_PoisonDamage(player, source, 15 + (P_Random and 15), false); // Don't play painsound
              P_PoisonPlayer(player, source, 50);
              S_StartSound(target, Ord(SFX_PLAYER_POISONCOUGH));
            end;
            exit;
          end
          else if target.flags and MF_COUNTKILL = 0 then
          begin // only damage monsters/players with the poison cloud
            exit;
          end;
        end;

      Ord(MT_FSWORD_MISSILE):
        begin
          if target.player <> nil then
          begin
            damage := damage - _SHR2(damage);
          end;
        end;

    end;
  end;

  // Push the target unless source is using the gauntlets
  if (inflictor <> nil) and
     ((source = nil) or (source.player = nil)) and
     (inflictor.flags2 and MF2_NODMGTHRUST = 0) then
  begin
    ang := R_PointToAngle2(inflictor.x, inflictor.y, target.x, target.y);
    mass := target.mass;
    if (mass = 0) or (G_PlayingEngineVersion < VERSION204) then  // JVAL: 20180218 - VERSION 2.0.4.715 Change from (<> 0) to (= 0)
      thrust := 0
    else
      thrust := damage * (FRACUNIT div 8) * 150 div mass;
    // make fall forwards sometimes
    if (damage < 40) and
       (damage > target.health) and
       (target.z - inflictor.z > 64 * FRACUNIT) and
       (P_Random and 1 <> 0) then
    begin
      ang := ang + ANG180;
      thrust := thrust * 4;
    end;
    ang := ang shr ANGLETOFINESHIFT;
    target.momx := target.momx + FixedMul(thrust, finecosine[ang]);
    target.momy := target.momy + FixedMul(thrust, finesine[ang]);
  end;

  //
  // player specific
  //
  if player <> nil then
  begin
    savedPercent := AutoArmorSave[Ord(player._class)] +
                    player.armorpoints[Ord(ARMOR_ARMOR)] +
                    player.armorpoints[Ord(ARMOR_SHIELD)] +
                    player.armorpoints[Ord(ARMOR_HELMET)] +
                    player.armorpoints[Ord(ARMOR_AMULET)];
    if savedPercent <> 0 then
    begin // armor absorbed some damage
      if savedPercent > 100 * FRACUNIT then
        savedPercent := 100 * FRACUNIT;
      for i := 0 to Ord(NUMARMOR) - 1 do
      begin
        if player.armorpoints[i] <> 0 then
        begin
          player.armorpoints[i] :=
            player.armorpoints[i] - FixedDiv(FixedMul(damage * FRACUNIT, ArmorIncrement[Ord(player._class)][i]), 300 * FRACUNIT);
          if player.armorpoints[i] < 2 * FRACUNIT then
            player.armorpoints[i] := 0;
        end;
      end;
      saved := FixedDiv(FixedMul(damage * FRACUNIT, savedPercent), 100 * FRACUNIT);
      if saved > savedPercent * 2 then
        saved := savedPercent * 2;
      damage := damage - FixedInt(saved);
    end;
    if (damage >= player.health) and ((gameskill = sk_baby) or (deathmatch <> 0)) and (player.morphTics = 0) then
    begin // Try to use some inventory health
      P_AutoUseHealth(player, damage - player.health + 1);
    end;
    player.health := player.health - damage; // mirror mobj health here for Dave
    if player.health < 0 then
      player.health := 0;
    player.attacker := source;
    player.damagecount := player.damagecount + damage; // add damage after armor / invuln
    if player.damagecount > 100 then
      player.damagecount := 100; // teleport stomp does 10k points...
    if player = @players[consoleplayer] then
    begin
{    if damage < 100 then
      I_Tactile(40, 10, 40 + damage * 2) // JVAL Force Feedback
    else
      I_Tactile(40, 10, 40 + 100 * 2); // JVAL Force Feedback
}
      SB_PaletteFlash(false);
    end;
  end;

  //
  // do the damage
  //
  target.health := target.health - damage;
  if target.health <= 0 then
  begin // Death
    if inflictor <> nil then
    begin // check for special fire damage or ice damage deaths
      if inflictor.flags2 and MF2_FIREDAMAGE <> 0 then
      begin
        if (player <> nil) and (player.morphTics = 0) then
        begin // Check for flame death
          if (target.health > -50) and (damage > 25) then
          begin
            target.flags2 := target.flags2 or MF2_FIREDAMAGE;
          end;
        end
        else
        begin
          target.flags2 := target.flags2 or MF2_FIREDAMAGE;
        end;
      end
      else if inflictor.flags2 and MF2_ICEDAMAGE <> 0 then
      begin
        target.flags2 := target.flags2 or MF2_ICEDAMAGE;
      end;
    end;
    if (source <> nil) and (source._type = Ord(MT_MINOTAUR)) then
    begin // Minotaur's kills go to his master
      master := Pmobj_t(source.special1);
      if master <> nil then
      begin
        // Make sure still alive and not a pointer to fighter head
        if (master.player <> nil) and (Pplayer_t(master.player).mo = master) then
          source := master;
      end;
    end;
    if (source <> nil) and
       (source.player <> nil) and
       (Pplayer_t(source.player).readyweapon = WP_FOURTH) then
    begin
      // Always extreme death from fourth weapon
      target.health := -5000;
    end;
    P_KillMobj(source, target);
    P_Obituary(target, inflictor, source);
    exit;
  end;
  if (P_Random < target.painchance) and
     (target.flags and MF_SKULLFLY = 0) then
  begin
    if (inflictor <> nil) and
       (inflictor._type >= Ord(MT_LIGHTNING_FLOOR)) and
       (inflictor._type <= Ord(MT_LIGHTNING_ZAP)) then
    begin
      if P_Random < 96 then
      begin
        target.flags := target.flags or MF_JUSTHIT; // fight back!
        P_SetMobjState(target, statenum_t(target.info.painstate));
      end
      else
      begin // 'electrocute' the target
        target.frame := target.frame or FF_FULLBRIGHT;
        if (target.flags and MF_COUNTKILL <> 0) and
           (P_Random < 128) and
           (not S_GetSoundPlayingInfo(target, Ord(SFX_PUPPYBEAT))) then
        begin
          if (target._type = Ord(MT_CENTAUR)) or
             (target._type = Ord(MT_CENTAURLEADER)) or
             (target._type = Ord(MT_ETTIN)) then
            S_StartSound(target, Ord(SFX_PUPPYBEAT));
        end;
      end;
    end
    else
    begin
      target.flags := target.flags or MF_JUSTHIT; // fight back!
      P_SetMobjState(target, statenum_t(target.info.painstate));
      if (inflictor <> nil) and (inflictor._type = Ord(MT_POISONCLOUD)) then
      begin
        if (target.flags and MF_COUNTKILL <> 0) and
           (P_Random < 128) and
           (not S_GetSoundPlayingInfo(target, Ord(SFX_PUPPYBEAT))) then
        begin
          if (target._type = Ord(MT_CENTAUR)) or
             (target._type = Ord(MT_CENTAURLEADER)) or
             (target._type = Ord(MT_ETTIN)) then
          begin
            S_StartSound(target, Ord(SFX_PUPPYBEAT));
          end;
        end;
      end;
    end;
  end;
  target.reactiontime := 0; // we're awake now...
  if (target.threshold = 0) and
     (source <> nil) and
     (source.flags2 and MF2_BOSS = 0) and
     (source.flags_ex and MF_EX_BOSS = 0) and
     (target._type <> Ord(MT_BISHOP)) and
     (target._type <> Ord(MT_MINOTAUR)) then
  begin
    // Target actor is not intent on another actor,
    // so make him chase after source
    if ((target._type = Ord(MT_CENTAUR)) and (source._type = Ord(MT_CENTAURLEADER))) or
       ((target._type = Ord(MT_CENTAURLEADER)) and (source._type = Ord(MT_CENTAUR))) then
      exit;

    target.target := source;
    target.threshold := BASETHRESHOLD;
    if (target.state = @states[target.info.spawnstate]) and (target.info.seestate <> Ord(S_NULL)) then
      P_SetMobjState(target, statenum_t(target.info.seestate));
  end;
end;

//==========================================================================
//
// P_FallingDamage
//
//==========================================================================

procedure P_FallingDamage(player: Pplayer_t);
var
  damage: integer;
  mom: integer;
  dist: integer;
begin
  mom := abs(player.mo.momz);
  dist := FixedMul(mom, 16 * FRACUNIT div 23);

  if mom >= 63 * FRACUNIT then
  begin // automatic death
    P_DamageMobj(player.mo, nil, nil, 10000);
    exit;
  end;

  damage := FixedInt(FixedMul(dist, dist) div 10) - 24;
  if (player.mo.momz > -39 * FRACUNIT) and (damage > player.mo.health)  and (player.mo.health <> 1) then
  begin // No-death threshold
    damage := player.mo.health - 1;
  end;
  S_StartSound(player.mo, Ord(SFX_PLAYER_LAND));
  P_DamageMobj(player.mo, nil, nil, damage);
end;

//==========================================================================
//
// P_PoisonPlayer - Sets up all data concerning poisoning
//
//==========================================================================

procedure P_PoisonPlayer(player: Pplayer_t; poisoner: Pmobj_t; poison: integer);
begin
  if (player.cheats and CF_GODMODE <> 0) or (player.powers[Ord(pw_invulnerability)] <> 0) then
    exit;

  player.poisoncount := player.poisoncount + poison;
  player.poisoner := poisoner;
  if player.poisoncount > 100 then
    player.poisoncount := 100;
end;

//==========================================================================
//
// P_PoisonDamage - Similar to P_DamageMobj
//
//==========================================================================

procedure P_PoisonDamage(player: Pplayer_t; source: Pmobj_t; damage: integer;
  playPainSound: boolean);
var
  target: Pmobj_t;
  inflictor: Pmobj_t;
begin
  target := player.mo;
  if target.health <= 0 then
    exit;

  inflictor := source;

  if ((target.flags2 and MF2_INVULNERABLE <> 0) or (target.flags_ex and MF_EX_INVULNERABLE <> 0)) and
     (damage < 10000) then // mobj is invulnerable
    exit;

  if (player <> nil) and (gameskill = sk_baby) then
    damage := _SHR1(damage); // Take half damage in trainer mode

  if (damage < 1000) and
     ((player.cheats and CF_GODMODE <> 0) or (player.powers[Ord(pw_invulnerability)] <> 0)) then
    exit;

  if (damage >= player.health) and
     ((gameskill = sk_baby) or (deathmatch <> 0)) and
     (player.morphTics = 0) then // Try to use some inventory health
    P_AutoUseHealth(player, damage - player.health + 1);

  player.health := player.health - damage; // mirror mobj health here for Dave
  if player.health < 0 then
    player.health := 0;

  player.attacker := source;

  //
  // do the damage
  //
  target.health := target.health - damage;
  if target.health <= 0 then
  begin // Death
    target.special1 := damage;
    if (player <> nil) and (inflictor <> nil) and (player.morphTics = 0) then
    begin // Check for flame death
      if (inflictor.flags2 and MF2_FIREDAMAGE <> 0) and
         (target.health > -50) and
         (damage > 25) then
        target.flags2 := target.flags2 or MF2_FIREDAMAGE;

      if inflictor.flags2 and MF2_ICEDAMAGE <> 0 then
        target.flags2 := target.flags2 or MF2_ICEDAMAGE;
    end;
    P_KillMobj(source, target);
    P_Obituary(target, inflictor, source);
    exit;
  end;
  if (leveltime and 63 = 0) and playPainSound then
    P_SetMobjState(target, statenum_t(target.info.painstate));
end;

procedure P_CmdSuicide;
begin
  if demoplayback then
  begin
    I_Warning('P_CmdSuicide(): You can''t suicide during demo playback.'#13#10);
    exit;
  end;
  if demorecording then
  begin
    I_Warning('P_CmdSuicide(): You can''t suicide during demo recording.'#13#10);
    exit;
  end;

  if (gamestate = GS_LEVEL) and (players[consoleplayer].mo <> nil) then
  begin
    if players[consoleplayer].health > 0 then
    begin
      C_ExecuteCmd('closeconsole');
      P_DamageMobj(players[consoleplayer].mo, nil, nil, 1000);
      players[consoleplayer]._message := 'You give up too easy';
    end
    else
      printf('You''re already dead.'#13#10);
  end
  else
    I_Warning('P_CmdSuicide(): You can''t suicide if you aren''t playing.'#13#10);
end;

procedure P_SetMessage(player: Pplayer_t; const msg: string; ultmsg: boolean = false);
begin
  if (player.ultimateMessage or (showMessages = 0)) and not ultmsg then
    exit;

  player.messageTics := MESSAGETICS;
  player._message := msg;
  if ultmsg then
    player.ultimateMessage := true;
  player.yellowMessage := false;

end;

procedure P_SetYellowMessage(player: Pplayer_t; const msg: string; ultmsg: boolean = false);
begin
  if (player.ultimateMessage or (showMessages = 0)) and not ultmsg then
    exit;

  player.messageTics := MESSAGETICS;
  player._message := msg;
  if ultmsg then
    player.ultimateMessage := true;

  player.yellowMessage := true;
end;


//==========================================================================
//
// P_ClearMessage
//
//==========================================================================

procedure P_ClearMessage(player: Pplayer_t);
begin
  player.messageTics := 0;
  player._message := '';
  player.ultimateMessage := false;
  player.yellowMessage := false;
end;

end.

