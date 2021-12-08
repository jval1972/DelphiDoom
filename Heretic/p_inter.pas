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
  h_strings,
  sounds,
  doomstat,
  m_rnd,
  i_system,
  am_map,
  p_local,
  p_mobj_h,
  s_sound,
  d_player;

function P_GivePower(player: Pplayer_t; power: integer): boolean;

function P_GiveBody(player: Pplayer_t; num: integer): boolean;

procedure P_TouchSpecialThing(special: Pmobj_t; toucher: Pmobj_t);

procedure P_DamageMobj(target, inflictor, source: Pmobj_t; damage: integer);

// Actions
procedure A_RestoreArtifact(arti: Pmobj_t);

procedure A_RestoreSpecialThing1(thing: Pmobj_t);

procedure A_RestoreSpecialThing2(thing: Pmobj_t);


const
// a weapon is found with two clip loads,
// a big item has five clip loads
  maxammo: array[0..Ord(NUMAMMO) - 1] of integer = (
    100, // gold wand
     50, // crossbow
    200, // blaster
    200, // skull rod
     20, // phoenix rod
    150   // mace
  );

  GetWeaponAmmo: array[0..Ord(NUMWEAPONS) - 1] of integer = (
      0,  // staff
     25,  // gold wand
     10,  // crossbow
     30,  // blaster
     50,  // skull rod
      2,  // phoenix rod
     50,  // mace
      0,  // gauntlets
      0   // beak
  );

  GetAmmoChange: array[0..5] of weapontype_t = (
    wp_goldwand,
    wp_crossbow,
    wp_blaster,
    wp_skullrod,
    wp_phoenixrod,
    wp_mace
  );

  WeaponValue: array[0..8] of integer = (
    1,    // staff
    3,    // goldwand
    4,    // crossbow
    5,    // blaster
    6,    // skullrod
    7,    // phoenixrod
    8,    // mace
    2,    // gauntlets
    0    // beak
  );


procedure P_CmdSuicide;

procedure P_SetMessage(player: Pplayer_t; const msg: string; ultmsg: boolean = false);

var
  p_maxhealth: integer = 200;
  p_maxchickenhealth: integer = 30;
  p_soulspherehealth: integer = 100;
  p_megaspherehealth: integer = 200;
  p_medikithealth: integer = 25;
  p_stimpackhealth: integer = 10;
  p_bonushealth: integer = 1;
  p_maxarmor: integer = 200;
  p_greenarmorclass: integer = 1;
  p_bluearmorclass: integer = 2;
  p_maxartifacts: integer = 16;

function P_GiveArtifact(player: Pplayer_t; arti: artitype_t; mo: Pmobj_t): boolean;

implementation

uses
  c_cmds,
  d_delphi,
  info_h,
  info,
  m_fixed,
  m_menu,
  g_game,
  p_mobj,
  p_obituaries,
  p_3dfloors,
  p_pspr,
  p_pspr_h,
  p_tick,
  p_user,
  p_enemy,
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

//
// P_GiveAmmo
// Num is the number of clip loads,
// not the individual count (0= 1/2 clip).
// Returns false if the ammo can't be picked up at all
//
function P_GiveAmmo(player: Pplayer_t; ammo: ammotype_t; num: integer): boolean;
var
  oldammo: integer;
begin
  if ammo = am_noammo then
  begin
    result := false;
    exit;
  end;

  if (Ord(ammo) < 0) or (Ord(ammo) > Ord(NUMAMMO)) then
    I_Error('P_GiveAmmo(): bad type %d', [Ord(ammo)]);

  if player.ammo[Ord(ammo)] = player.maxammo[Ord(ammo)] then
  begin
    result := false;
    exit;
  end;

  if (gameskill = sk_baby) or (gameskill = sk_nightmare) then
  begin
    // extra ammo in baby mode and nightmare mode
    num := num  + num div 2
  end;


  oldammo := player.ammo[Ord(ammo)];
  player.ammo[Ord(ammo)] := player.ammo[Ord(ammo)] + num;

  if player.ammo[Ord(ammo)] > player.maxammo[Ord(ammo)] then
    player.ammo[Ord(ammo)] := player.maxammo[Ord(ammo)];

  // If non zero ammo,
  // don't change up weapons,
  // player was lower on purpose.
  if oldammo <> 0 then
  begin
    result := true;
    exit;
  end;

  // We were down to zero,
  // so select a new weapon.
  // Preferences are not user selectable.
  if (player.readyweapon = wp_staff) or
     (player.readyweapon = wp_gauntlets) then
    if player.weaponowned[Ord(GetAmmoChange[Ord(ammo)])] <> 0 then
      player.pendingweapon := GetAmmoChange[Ord(ammo)];

  result := true;
end;

//
// P_GiveWeapon
// The weapon name may have a MF_DROPPED flag ored in.
//
function P_GiveWeapon(player: Pplayer_t; weapon: weapontype_t): boolean;
var
  gaveammo: boolean;
  gaveweapon: boolean;
  ammo: ammotype_t;
begin
  ammo := wpnlev1info[Ord(weapon)].ammo;
  if netgame and (deathmatch <> 2) then
  begin
  // leave placed weapons forever on net games
    if player.weaponowned[Ord(weapon)] <> 0 then
    begin
      result := false;
      exit;
    end;

    player.bonuscount := player.bonuscount + BONUSADD;
    player.weaponowned[Ord(weapon)] := 1;

    P_GiveAmmo(player, ammo, Ord(GetWeaponAmmo[Ord(weapon)]));
    player.pendingweapon := weapon;

    if (player = @players[consoleplayer]) then
      S_StartSound(nil, Ord(sfx_wpnup));
    result := false;
    exit;
  end;

  if ammo <> am_noammo then
  begin
  // give one clip with a dropped weapon,
  // two clips with a found weapon
    gaveammo := P_GiveAmmo(player, ammo, GetWeaponAmmo[Ord(weapon)])
  end
  else
    gaveammo := false;

  if player.weaponowned[Ord(weapon)] <> 0 then
    gaveweapon := false
  else
  begin
    gaveweapon := true;
    player.weaponowned[Ord(weapon)] := 1;
    // Only switch to more powerful weapons
    if WeaponValue[Ord(weapon)] > WeaponValue[Ord(player.readyweapon)] then
      player.pendingweapon := weapon;
  end;

  result := gaveweapon or gaveammo;
end;

//
// P_GiveBody
// Returns false if the body isn't needed at all
//
function P_GiveBody(player: Pplayer_t; num: integer): boolean;
var
  maxhealth: integer;
begin
  if player.chickenTics <> 0 then
    maxhealth := p_maxchickenhealth
  else
    maxhealth := mobjinfo[Ord(MT_PLAYER)].spawnhealth;

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
// P_GiveArmor
// Returns false if the armor is worse
// than the current armor.
//
function P_GiveArmor(player: Pplayer_t; armortype: integer): boolean;
var
  hits: integer;
begin
  hits := armortype * 100;
  if player.armorpoints >= hits then
  begin
    result := false;  // don't pick up
    exit;
  end;

  player.armortype := armortype;
  player.armorpoints := hits;

  result := true;
end;

//
// P_GiveCard
//
procedure P_GiveKey(player: Pplayer_t; key: keytype_t);
begin
  if player.keys[Ord(key)] then
    exit;

  if player = @players[consoleplayer] then
    playerkeys := playerkeys or _SHL(1, Ord(key));

  player.bonuscount := BONUSADD;
  player.keys[Ord(key)] := true;
end;

//
// P_GivePower
//
function P_GivePower(player: Pplayer_t; power: integer): boolean;
begin
  if power = Ord(pw_invulnerability) then
  begin
    if player.powers[power] > BLINKTHRESHOLD then
    begin // Already have it
      result := false;
      exit;
    end;
    player.powers[power] := INVULNTICS;
    result := true;
    exit;
  end;

  if power = Ord(pw_weaponlevel2) then
  begin
    if player.powers[power] > BLINKTHRESHOLD then
    begin // Already have it
      result := false;
      exit;
    end;
    player.powers[power] := WPNLEV2TICS;
    result := true;
    exit;
  end;

  if power = Ord(pw_invisibility) then
  begin
    if player.powers[power] > BLINKTHRESHOLD then
    begin // Already have it
      result := false;
      exit;
    end;
    player.powers[power] := INVISTICS;
    player.mo.flags := player.mo.flags or MF_SHADOW;
    result := true;
    exit;
  end;

  if power = Ord(pw_flight) then
  begin
    if player.powers[power] > BLINKTHRESHOLD then
    begin // Already have it
      result := false;
      exit;
    end;
    player.powers[power] := FLIGHTTICS;
    player.mo.flags2 := player.mo.flags2 or MF2_FLY;
    player.mo.flags := player.mo.flags or MF_NOGRAVITY;
    if player.mo.z <= player.mo.floorz then
      player.flyheight := 10; // thrust the player in the air a bit
    result := true;
    exit;
  end;

  if power = Ord(pw_infrared) then
  begin
    if player.powers[power] > BLINKTHRESHOLD then
    begin // Already have it
      result := false;
      exit;
    end;
    player.powers[power] := INFRATICS;
    result := true;
    exit;
  end;

  if player.powers[power] <> 0 then
    result := false // already got it
  else
  begin
    player.powers[power] := 1;
    result := true;
  end;
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
  i: integer;
begin

  i := 0;
  while (player.inventory[i]._type <> Ord(arti)) and (i < player.inventorySlotNum) do
    inc(i);

  if i = player.inventorySlotNum then
  begin
    player.inventory[i].count := 1;
    player.inventory[i]._type := Ord(arti);
    inc(player.inventorySlotNum);
  end
  else
  begin
    if player.inventory[i].count >= p_maxartifacts then
    begin // Player already has 16 of this item
      result := false;
      exit;
    end;
    inc(player.inventory[i].count);
  end;

  if player.artifactCount = 0 then
    player.readyArtifact := arti;

  inc(player.artifactCount);
  if (mo <> nil) and (mo.flags and MF_COUNTITEM <> 0) then
    inc(player.itemcount);

  result := true;
end;

//---------------------------------------------------------------------------
//
// PROC P_SetDormantArtifact
//
// Removes the MF_SPECIAL flag, and initiates the artifact pickup
// animation.
//
//---------------------------------------------------------------------------

procedure P_SetDormantArtifact(arti: Pmobj_t);
begin
  arti.flags := arti.flags and not MF_SPECIAL;

  if (deathmatch <> 0) and (arti._type <> Ord(MT_ARTIINVULNERABILITY)) and (arti._type <> Ord(MT_ARTIINVISIBILITY)) then
    P_SetMobjState(arti, S_DORMANTARTI1)
  else // Don't respawn
    P_SetMobjState(arti, S_DEADARTI1);

  S_StartSound(arti, Ord(sfx_artiup));
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
  S_StartSound(arti, Ord(sfx_respawn));
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

//---------------------------------------------------------------------------
//
// PROC A_RestoreSpecialThing1
//
// Make a special thing visible again.
//
//---------------------------------------------------------------------------

procedure A_RestoreSpecialThing1(thing: Pmobj_t);
begin
  if thing._type = Ord(MT_WMACE) then
  begin // Do random mace placement
    P_RepositionMace(thing);
  end;
  thing.flags2 := thing.flags2 and not MF2_DONTDRAW;
  S_StartSound(thing, Ord(sfx_respawn));
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
  P_DamageMobj(target, nil, nil, HITDICE(6));
  if target.player <> nil then
    target.reactiontime := 14 + (P_Random and 7);
end;

//---------------------------------------------------------------------------
//
// FUNC P_TouchWhirlwind
//
//---------------------------------------------------------------------------

procedure P_TouchWhirlwind(target: Pmobj_t);
var
  randVal: integer;
begin
  target.angle := target.angle + LongWord((P_Random - P_Random) * $100000);
  target.momx := target.momx + (P_Random - P_Random) * 1024;
  target.momy := target.momy + (P_Random - P_Random) * 1024;
  if (leveltime and 16 <> 0) and (target.flags2 and MF2_BOSS = 0) then
  begin
    randVal := P_Random;
    if randVal > 160 then
      randVal := 160;
    target.momz := target.momz + randVal * 1024;
    if target.momz > 12 * FRACUNIT then
      target.momz := 12 * FRACUNIT;
  end;
  if leveltime and 7 = 0 then
    P_DamageMobj(target, nil, nil, 3);
end;

//---------------------------------------------------------------------------
//
// FUNC P_ChickenMorphPlayer
//
// Returns true if the player gets turned into a chicken.
//
//---------------------------------------------------------------------------

function P_ChickenMorphPlayer(player: Pplayer_t): boolean;
var
  pmo: Pmobj_t;
  fog: Pmobj_t;
  chicken: Pmobj_t;
  x, y, z: fixed_t;
  angle: angle_t;
  oldFlags2: integer;
begin
  if player.chickenTics <> 0 then
  begin
    if (player.chickenTics < CHICKENTICS - TICRATE) and
       (player.powers[Ord(pw_weaponlevel2)] = 0) then
    begin // Make a super chicken
      P_GivePower(player, Ord(pw_weaponlevel2));
    end;
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
  S_StartSound(fog, Ord(sfx_telept));
  chicken := P_SpawnMobj(x, y, z, Ord(MT_CHICPLAYER));
  chicken.special1 := Ord(player.readyweapon);
  chicken.angle := angle;
  chicken.player := player;
  player.health := p_maxchickenhealth;
  chicken.health := p_maxchickenhealth;
  player.mo := chicken;
  player.armorpoints := 0;
  player.armortype := 0;
  player.powers[Ord(pw_invisibility)] := 0;
  player.powers[Ord(pw_weaponlevel2)] := 0;
  if oldFlags2 and MF2_FLY <> 0 then
    chicken.flags2 := chicken.flags2 or MF2_FLY;
  player.chickenTics := CHICKENTICS;
  P_ActivateBeak(player);
  result := true;
end;

//---------------------------------------------------------------------------
//
// FUNC P_ChickenMorph
//
//---------------------------------------------------------------------------

function P_ChickenMorph(actor: Pmobj_t): boolean;
var
  fog: Pmobj_t;
  chicken: Pmobj_t;
  target: Pmobj_t;
  moType: integer;
  x, y, z: fixed_t;
  angle: angle_t;
  ghost: integer;
begin
  if actor.player <> nil then
  begin
    result := false;
    exit;
  end;

  moType := actor._type;
  case moType of
    Ord(MT_POD),
    Ord(MT_CHICKEN),
    Ord(MT_HEAD),
    Ord(MT_MINOTAUR),
    Ord(MT_SORCERER1),
    Ord(MT_SORCERER2):
      begin
        result := false;
        exit;
      end;
  end;

  x := actor.x;
  y := actor.y;
  z := actor.z;
  angle := actor.angle;
  ghost := actor.flags and MF_SHADOW;
  target := actor.target;
  P_SetMobjState(actor, S_FREETARGMOBJ);
  fog := P_SpawnMobj(x, y, z + TELEFOGHEIGHT, Ord(MT_TFOG));
  S_StartSound(fog, Ord(sfx_telept));
  chicken := P_SpawnMobj(x, y, z, Ord(MT_CHICKEN));
  chicken.special2 := moType;
  chicken.special1 := CHICKENTICS + P_Random;
  chicken.flags := chicken.flags or ghost;
  chicken.target := target;
  chicken.angle := angle;
  result := true;
end;

//---------------------------------------------------------------------------
//
// FUNC P_AutoUseChaosDevice
//
//---------------------------------------------------------------------------

function P_AutoUseChaosDevice(player: Pplayer_t): boolean;
var
  i: integer;
begin
  for i := 0 to player.inventorySlotNum - 1 do
  begin
    if player.inventory[i]._type = Ord(arti_teleport) then
    begin
      P_PlayerUseArtifact(player, arti_teleport);
      player.health := (player.health + 1) div 2;
      player.mo.health := player.health;
      result := true;
      exit;
    end;
  end;
  result := false;
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

//
// P_TouchSpecialThing
//
procedure P_TouchSpecialThing(special: Pmobj_t; toucher: Pmobj_t);
var
  player: Pplayer_t;
  i: integer;
  delta: fixed_t;
  sound: integer;
  respawn: boolean;
begin
  delta := special.z - toucher.z;

  if (delta > toucher.height) or (delta < - 32 * FRACUNIT) then
  // out of reach
    exit;

  // Dead thing touching.
  // Can happen with a sliding player corpse.
  if toucher.health <= 0 then
    exit;

  sound := Ord(sfx_itemup);
  player := toucher.player;

  respawn := true;
  // Identify by sprite.
  case special.sprite of
  // armor
    Ord(SPR_PTN1): // Item_HealingPotion
      begin
        if not P_GiveBody(player, 10) then
          exit;

        P_SetMessage(player, TXT_ITEMHEALTH);
      end;

    Ord(SPR_SHLD): // Item_Shield1
      begin
        if not P_GiveArmor(player, 1) then
          exit;

        P_SetMessage(player, TXT_ITEMSHIELD1);
      end;

    Ord(SPR_SHD2): // Item_Shield2
      begin
        if not P_GiveArmor(player, 2) then
          exit;

        P_SetMessage(player, TXT_ITEMSHIELD2);
      end;

    Ord(SPR_BAGH): // Item_BagOfHolding
      begin
        if not player.backpack then
        begin
          for i := 0 to Ord(NUMAMMO) - 1 do
            player.maxammo[i] := player.maxammo[i] * 2;
          player.backpack := true;
        end;
        P_GiveAmmo(player, am_goldwand, AMMO_GWND_WIMPY);
        P_GiveAmmo(player, am_blaster, AMMO_BLSR_WIMPY);
        P_GiveAmmo(player, am_crossbow, AMMO_CBOW_WIMPY);
        P_GiveAmmo(player, am_skullrod, AMMO_SKRD_WIMPY);
        P_GiveAmmo(player, am_phoenixrod, AMMO_PHRD_WIMPY);
        P_SetMessage(player, TXT_ITEMBAGOFHOLDING);
      end;

    Ord(SPR_SPMP): // Item_SuperMap
      begin
        if not P_GivePower(player, Ord(pw_allmap)) then
          exit;

        P_SetMessage(player, TXT_ITEMSUPERMAP);
      end;

    // Keys
    Ord(SPR_BKYY): // Key_Blue
      begin
        if not player.keys[Ord(key_blue)] then
          P_SetMessage(player, TXT_GOTBLUEKEY);
        P_GiveKey(player, key_blue);
        if netgame then
          exit;
        sound := Ord(sfx_keyup);
      end;

    Ord(SPR_CKYY): // Key_Yellow
      begin
        if not player.keys[Ord(key_yellow)] then
          P_SetMessage(player, TXT_GOTYELLOWKEY);
        sound := Ord(sfx_keyup);
        P_GiveKey(player, key_yellow);
        if netgame then
          exit;
      end;

    Ord(SPR_AKYY): // Key_Green
      begin
      if not player.keys[Ord(key_green)] then
        P_SetMessage(player, TXT_GOTGREENKEY);
        sound := Ord(sfx_keyup);
        P_GiveKey(player, key_green);
        if netgame then
          exit;
      end;

    // Artifacts
    Ord(SPR_PTN2): // Arti_HealingPotion
      begin
        if P_GiveArtifact(player, arti_health, special) then
        begin
          P_SetMessage(player, TXT_ARTIHEALTH);
          P_SetDormantArtifact(special);
        end;
        exit;
      end;

    Ord(SPR_SOAR): // Arti_Fly
      begin
        if P_GiveArtifact(player, arti_fly, special) then
        begin
          P_SetMessage(player, TXT_ARTIFLY);
          P_SetDormantArtifact(special);
        end;
        exit;
      end;

    Ord(SPR_INVU): // Arti_Invulnerability
      begin
        if P_GiveArtifact(player, arti_invulnerability, special) then
        begin
          P_SetMessage(player, TXT_ARTIINVULNERABILITY);
          P_SetDormantArtifact(special);
        end;
        exit;
      end;

    Ord(SPR_PWBK): // Arti_TomeOfPower
      begin
        if P_GiveArtifact(player, arti_tomeofpower, special) then
        begin
          P_SetMessage(player, TXT_ARTITOMEOFPOWER);
          P_SetDormantArtifact(special);
        end;
        exit;
      end;

    Ord(SPR_INVS): // Arti_Invisibility
      begin
        if P_GiveArtifact(player, arti_invisibility, special) then
        begin
          P_SetMessage(player, TXT_ARTIINVISIBILITY, false);
          P_SetDormantArtifact(special);
        end;
        exit;
      end;

    Ord(SPR_EGGC): // Arti_Egg
      begin
        if P_GiveArtifact(player, arti_egg, special) then
        begin
          P_SetMessage(player, TXT_ARTIEGG, false);
          P_SetDormantArtifact(special);
        end;
        exit;
      end;

    Ord(SPR_SPHL): // Arti_SuperHealth
      begin
        if P_GiveArtifact(player, arti_superhealth, special) then
        begin
          P_SetMessage(player, TXT_ARTISUPERHEALTH, false);
          P_SetDormantArtifact(special);
        end;
        exit;
      end;

    Ord(SPR_TRCH): // Arti_Torch
      begin
        if P_GiveArtifact(player, arti_torch, special) then
        begin
          P_SetMessage(player, TXT_ARTITORCH);
          P_SetDormantArtifact(special);
        end;
        exit;
      end;

    Ord(SPR_FBMB): // Arti_FireBomb
      begin
        if P_GiveArtifact(player, arti_firebomb, special) then
        begin
          P_SetMessage(player, TXT_ARTIFIREBOMB);
          P_SetDormantArtifact(special);
        end;
        exit;
      end;

    Ord(SPR_ATLP): // Arti_Teleport
      begin
        if P_GiveArtifact(player, arti_teleport, special) then
        begin
          P_SetMessage(player, TXT_ARTITELEPORT);
          P_SetDormantArtifact(special);
        end;
        exit;
      end;

    // Ammo
    Ord(SPR_AMG1): // Ammo_GoldWandWimpy
      begin
        if not P_GiveAmmo(player, am_goldwand, special.health) then
          exit;

        P_SetMessage(player, TXT_AMMOGOLDWAND1);
      end;

    Ord(SPR_AMG2): // Ammo_GoldWandHefty
      begin
        if not P_GiveAmmo(player, am_goldwand, special.health) then
          exit;

        P_SetMessage(player, TXT_AMMOGOLDWAND2);
      end;

    Ord(SPR_AMM1): // Ammo_MaceWimpy
      begin
        if not P_GiveAmmo(player, am_mace, special.health) then
          exit;

        P_SetMessage(player, TXT_AMMOMACE1);
      end;

    Ord(SPR_AMM2): // Ammo_MaceHefty
      begin
        if not P_GiveAmmo(player, am_mace, special.health) then
          exit;

        P_SetMessage(player, TXT_AMMOMACE2);
      end;

    Ord(SPR_AMC1): // Ammo_CrossbowWimpy
      begin
        if not P_GiveAmmo(player, am_crossbow, special.health) then
          exit;

        P_SetMessage(player, TXT_AMMOCROSSBOW1);
      end;

    Ord(SPR_AMC2): // Ammo_CrossbowHefty
      begin
        if not P_GiveAmmo(player, am_crossbow, special.health) then
          exit;

        P_SetMessage(player, TXT_AMMOCROSSBOW2);
      end;

    Ord(SPR_AMB1): // Ammo_BlasterWimpy
      begin
        if not P_GiveAmmo(player, am_blaster, special.health) then
          exit;

        P_SetMessage(player, TXT_AMMOBLASTER1);
      end;

    Ord(SPR_AMB2): // Ammo_BlasterHefty
      begin
        if not P_GiveAmmo(player, am_blaster, special.health) then
          exit;

        P_SetMessage(player, TXT_AMMOBLASTER2);
      end;

    Ord(SPR_AMS1): // Ammo_SkullRodWimpy
      begin
        if not P_GiveAmmo(player, am_skullrod, special.health) then
          exit;

        P_SetMessage(player, TXT_AMMOSKULLROD1);
      end;

    Ord(SPR_AMS2): // Ammo_SkullRodHefty
      begin
        if not P_GiveAmmo(player, am_skullrod, special.health) then
          exit;

        P_SetMessage(player, TXT_AMMOSKULLROD2);
      end;

    Ord(SPR_AMP1): // Ammo_PhoenixRodWimpy
      begin
        if not P_GiveAmmo(player, am_phoenixrod, special.health) then
          exit;

        P_SetMessage(player, TXT_AMMOPHOENIXROD1);
      end;

    Ord(SPR_AMP2): // Ammo_PhoenixRodHefty
      begin
        if not P_GiveAmmo(player, am_phoenixrod, special.health) then
          exit;

        P_SetMessage(player, TXT_AMMOPHOENIXROD2);
      end;

    // Weapons
    Ord(SPR_WMCE): // Weapon_Mace
      begin
        if not P_GiveWeapon(player, wp_mace) then
          exit;

        P_SetMessage(player, TXT_WPNMACE);
        sound := Ord(sfx_wpnup);
      end;

    Ord(SPR_WBOW): // Weapon_Crossbow
      begin
        if not P_GiveWeapon(player, wp_crossbow) then
          exit;

        P_SetMessage(player, TXT_WPNCROSSBOW);
        sound := Ord(sfx_wpnup);
      end;

    Ord(SPR_WBLS): // Weapon_Blaster
      begin
        if not P_GiveWeapon(player, wp_blaster) then
          exit;

        P_SetMessage(player, TXT_WPNBLASTER);
        sound := Ord(sfx_wpnup);
      end;

    Ord(SPR_WSKL): // Weapon_SkullRod
      begin
        if not P_GiveWeapon(player, wp_skullrod) then
          exit;

        P_SetMessage(player, TXT_WPNSKULLROD);
        sound := Ord(sfx_wpnup);
      end;

    Ord(SPR_WPHX): // Weapon_PhoenixRod
      begin
        if not P_GiveWeapon(player, wp_phoenixrod) then
          exit;

        P_SetMessage(player, TXT_WPNPHOENIXROD);
        sound := Ord(sfx_wpnup);
      end;

    Ord(SPR_WGNT): // Weapon_Gauntlets
      begin
        if not P_GiveWeapon(player, wp_gauntlets) then
          exit;

        P_SetMessage(player, TXT_WPNGAUNTLETS);
        sound := Ord(sfx_wpnup);
      end;
    else
      I_Error('P_SpecialThing(): Unknown gettable thing');
  end;

  if special.flags and MF_COUNTITEM <> 0 then
    inc(player.itemcount);

  if (deathmatch <> 0) and respawn and (special.flags and MF_DROPPED = 0) then
    P_HideSpecialThing(special)
  else
    P_RemoveMobj(special);

  player.bonuscount := player.bonuscount + BONUSADD;

  if player = @players[consoleplayer] then
  begin
    S_StartSound(nil, sound);
//    SB_PaletteFlash; // JVAL Check
  end;
end;

//
// KillMobj
//
function P_SpawnDroppedMobj(x, y, z: fixed_t; _type: integer): Pmobj_t;
begin
  result := P_SpawnMobj(x, y, z, _type);
  result.flags := result.flags or MF_DROPPED; // special versions of items
  // JVAL Dropped items fall down to floor.
  if not compatibilitymode then
  begin
    result.z := result.z + 32 * FRACUNIT;
    result.momz := 4 * FRACUNIT;
    if G_PlayingEngineVersion >= VERSION207 then
    begin
      result.momx := 64 * N_Random;
      result.momy := 64 * N_Random;
    end;
  end;
end;

procedure P_KillMobj(source: Pmobj_t; target: Pmobj_t);
var
  item: integer;
  gibhealth: integer;
  zpos: integer;
begin
  target.flags := target.flags and not (MF_SHOOTABLE or MF_FLOAT or MF_SKULLFLY);
  if target.flags3_ex and MF3_EX_NOGRAVITYDEATH = 0 then
    target.flags := target.flags and not MF_NOGRAVITY;

  target.flags3_ex := target.flags3_ex and not MF3_EX_BOUNCE;

  target.flags := target.flags or (MF_CORPSE or MF_DROPOFF);
  target.flags2 := target.flags2 and not MF2_PASSMOBJ;
  target.height := _SHR(target.height, 2);

  if (source <> nil) and (source.player <> nil) then
  begin
    // count for intermission
    if target.flags and MF_COUNTKILL <> 0 then
      Pplayer_t(source.player).killcount := Pplayer_t(source.player).killcount + 1;

    if target.player <> nil then
    begin // Frag stuff
      if target = source then // Self-frag
        Pplayer_t(source.player).frags[pDiff(target.player, @players[0], SizeOf(players[0]))] :=
          Pplayer_t(source.player).frags[pDiff(target.player, @players[0], SizeOf(players[0]))] - 1
      else
      begin
        Pplayer_t(source.player).frags[pDiff(target.player, @players[0], SizeOf(players[0]))] :=
          Pplayer_t(source.player).frags[pDiff(target.player, @players[0], SizeOf(players[0]))] + 1;
        if source.player = @players[consoleplayer] then
          S_StartSound(nil, Ord(sfx_gfrag));
        if Pplayer_t(source.player).chickenTics <> 0 then // Make a super chicken
          P_GivePower(source.player, Ord(pw_weaponlevel2));
      end;
    end;
  end
  else if not netgame and (target.flags and MF_COUNTKILL <> 0) then
  begin
    // count all monster deaths,
    // even those caused by other monsters
    players[0].killcount := players[0].killcount + 1;
  end;

  if target.player <> nil then
  begin
    // count environment kills against you
    if source = nil then
      Pplayer_t(target.player).frags[pDiff(target.player, @players[0], SizeOf(players[0]))] :=
        Pplayer_t(target.player).frags[pDiff(target.player, @players[0], SizeOf(players[0]))] - 1;

    target.flags := target.flags and not MF_SOLID;
    target.flags2 := target.flags2 and not MF2_FLY;
    Pplayer_t(target.player).powers[Ord(pw_flight)] := 0;
    Pplayer_t(target.player).powers[Ord(pw_weaponlevel2)] := 0;
    Pplayer_t(target.player).playerstate := PST_DEAD;
    if Pplayer_t(target.player).attacker <> nil then
    begin
      Pplayer_t(target.player).attackerx := Pplayer_t(target.player).attacker.x;
      Pplayer_t(target.player).attackery := Pplayer_t(target.player).attacker.y;
    end;
    P_DropWeapon(target.player);
    if target.flags2 and MF2_FIREDAMAGE <> 0 then
    begin // Player flame death
      P_SetMobjState(target, S_PLAY_FDTH1);
      //S_StartSound(target, sfx_hedat1); // Burn sound
      exit;
    end;

    if (target.player = @players[consoleplayer]) and (amstate = am_only) then
    begin
      // don't die in auto map,
      // switch view prior to dying
      amstate := am_inactive;
      AM_Stop;
    end;

  end;

  gibhealth := target.info.gibhealth;
  if gibhealth >= 0 then
    gibhealth := -(target.info.spawnhealth div 2);

  if (target.health < gibhealth) and (target.info.xdeathstate <> 0) then
    P_SetMobjState(target, statenum_t(target.info.xdeathstate))
  else
    P_SetMobjState(target, statenum_t(target.info.deathstate));
  target.tics := target.tics - (P_Random and 3);

//  if target.tics < 1 then
//    target.tics := 1;
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
// P_DamageMobj
// Damages both enemies and players
// "inflictor" is the thing that caused the damage
//  creature or missile, can be NULL (slime, etc)
// "source" is the thing to target after taking damage
//  creature or NULL
// Source and inflictor are the same for melee attacks.
// Source can be NULL for slime, barrel explosions
// and other environmental stuff.
//
procedure P_DamageMobj(target, inflictor, source: Pmobj_t; damage: integer);
var
  ang: angle_t;
  saved: integer;
  player: Pplayer_t;
  thrust: fixed_t;
  mass: integer;
begin
  if target.flags and MF_SHOOTABLE = 0 then
  begin
  // 29/9/2009 JVAL: Display a warning message for debugging
    I_DevWarning('P_DamageMobj(): Trying to damage unshootable mobj "%s"'#13#10, [target.info.name]);
    exit; // shouldn't happen...
  end;

  // JVAL: Invulnerable monsters
  if target.flags_ex and MF_EX_INVULNERABLE <> 0 then
    exit;

  if target.flags2_ex and MF2_EX_NODAMAGE <> 0 then
    exit;

  if target.health <= 0 then
    exit;

  if target.flags and MF_SKULLFLY <> 0 then
  begin
   // Minotaur is invulnerable during charge attack
    if target._type = Ord(MT_MINOTAUR) then
      exit;

    target.momx := 0;
    target.momy := 0;
    target.momz := 0;
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

  player := target.player;
  if (player <> nil) and (gameskill = sk_baby) then
    damage := _SHR1(damage); // take half damage in trainer mode

  // Special damage types
  if inflictor <> nil then
  begin
    case inflictor._type of
      Ord(MT_EGGFX):
        begin
          if player <> nil then
            P_ChickenMorphPlayer(player)
          else
            P_ChickenMorph(target);
          exit; // Always return
        end;
      Ord(MT_WHIRLWIND):
        begin
          P_TouchWhirlwind(target);
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
      Ord(MT_MACEFX4): // Death ball
        begin
          if (target.flags2 and MF2_BOSS = 0) and (target._type <> Ord(MT_HEAD)) then
          begin // Don't allow cheap boss kills
            if target.player <> nil then
            begin // Player specific checks
              // Can't hurt invulnerable players
              if Pplayer_t(target.player).powers[Ord(pw_invulnerability)] = 0 then
              begin
                if P_AutoUseChaosDevice(target.player) then
                begin // Player was saved using chaos device
                  exit;
                end;
                damage := 10000; // Something's gonna die
              end;
            end;
          end;
        end;
      Ord(MT_PHOENIXFX2): // Flame thrower
        begin
          if (target.player <> nil) and (P_Random < 128) then
          // Freeze player for a bit
            target.reactiontime := target.reactiontime + 4;
        end;
      Ord(MT_RAINPLR1), // Rain missiles
      Ord(MT_RAINPLR2),
      Ord(MT_RAINPLR3),
      Ord(MT_RAINPLR4):
        begin
          if target.flags2 and MF2_BOSS <> 0 then
          // Decrease damage for bosses
            damage := (P_Random and 7) + 1;
        end;
      Ord(MT_HORNRODFX2),
      Ord(MT_PHOENIXFX1):
        begin
          if (target._type = Ord(MT_SORCERER2)) and (P_Random < 96) then
          begin // D'Sparil teleports away
            P_DSparilTeleport(target);
            exit;
          end;
        end;
      Ord(MT_BLASTERFX1),
      Ord(MT_RIPPER):
        begin
          if target._type = Ord(MT_HEAD) then
          begin // Less damage to Ironlich bosses
            damage := P_Random and 1;
            if damage = 0 then
              exit;
          end;
        end;
    end;

    if target.flags_ex and MF_EX_FIRERESIST <> 0 then
    begin
      if damage > 1 then
        damage := _SHR1(damage);
    end;
  end;


  // Some close combat weapons should not
  // inflict thrust and push the victim out of reach,
  // thus kick away unless using the chainsaw.
  if (inflictor <> nil) and (target.flags and MF_NOCLIP = 0) and
    ((source = nil) or (source.player = nil) or (Pplayer_t(source.player).readyweapon <> wp_gauntlets)) and
    (inflictor.flags2 and MF2_NODMGTHRUST = 0) then
  begin
    ang := R_PointToAngle2(inflictor.x, inflictor.y, target.x, target.y);

    mass := target.mass;
    if mass = 0 then
    begin
      I_DevWarning('P_DamageMobj(): Target (%s) mass is zero'#13#10, [target.info.name]);
      thrust := 0;
    end
    else
      thrust := (damage * (FRACUNIT shr 3) * 150) div mass;

    // make fall forwards sometimes
    if (damage < 40) and (damage > target.health) and
       (target.z - inflictor.z > 64 * FRACUNIT) and ((P_Random and 1) <> 0) then
    begin
      ang := ang + ANG180;
      thrust := thrust * 4;
    end;

    {$IFDEF FPC}
    ang := _SHRW(ang, ANGLETOFINESHIFT);
    {$ELSE}
    ang := ang shr ANGLETOFINESHIFT;
    {$ENDIF}
    if (source <> nil) and (source.player <> nil) and (source = inflictor) and
       (Pplayer_t(source.player).powers[Ord(pw_weaponlevel2)] <> 0) and
       (Pplayer_t(source.player).readyweapon = wp_staff) then
    begin
      // Staff power level 2
      target.momx := target.momx + FixedMul(10 * FRACUNIT, finecosine[ang]);
      target.momy := target.momy + FixedMul(10 * FRACUNIT, finesine[ang]);
      if target.flags and MF_NOGRAVITY = 0 then
        target.momz := target.momz + 5 * FRACUNIT;
    end
    else
    begin
      target.momx := target.momx + FixedMul(thrust, finecosine[ang]);
      target.momy := target.momy + FixedMul(thrust, finesine[ang]);
    end;
  end;

  // player specific
  if player <> nil then
  begin
    // Below certain threshold,
    // ignore damage in GOD mode, or with INVUL power.
    if (damage < 1000) and
       ((player.cheats and CF_GODMODE <> 0) or (player.powers[Ord(pw_invulnerability)] <> 0)) then
      exit;

    if player.armortype <> 0 then
    begin
      if player.armortype = 1 then
        saved := damage div 2
      else
        saved := damage div 2 + damage div 4;

      if player.armorpoints <= saved then
      begin
        // armor is used up
        saved := player.armorpoints;
        player.armortype := 0;
      end;
      player.armorpoints := player.armorpoints - saved;
      damage := damage - saved;
    end;

    if (damage >= player.health) and
       ((gameskill = sk_baby) or (deathmatch <> 0)) and
       (player.chickenTics = 0) then
     // Try to use some inventory health
      P_AutoUseHealth(player, damage - player.health + 1);

    player.health := player.health - damage;  // mirror mobj health here for Dave
    if player.health < 0 then
      player.health := 0;

    player.attacker := source;
    player.damagecount := player.damagecount + damage;  // add damage after armor / invuln

    if player.damagecount > 100 then
      player.damagecount := 100;  // teleport stomp does 10k points...

    if player = @players[consoleplayer] then
    begin
      // SB_PaletteFlash; // JVAL: check
    end;
  end;

  // do the damage
  target.health := target.health - damage;
  if target.health <= 0 then
  begin
    target.special1 := damage;

    // Make sure players get frags for chain-reaction kills
    if (target._type = Ord(MT_POD)) and (source <> nil) and (source._type <> Ord(MT_POD)) then
      target.target := source;

    if (player <> nil) and (inflictor <> nil) and (player.chickenTics = 0) then
    begin // Check for flame death
      if (inflictor.flags2 and MF2_FIREDAMAGE <> 0) or
         ((inflictor._type = Ord(MT_PHOENIXFX1)) and (target.health > -50) and (damage > 25)) then
        target.flags2 := target.flags2 or MF2_FIREDAMAGE;
    end;

    P_KillMobj(source, target);
    P_Obituary(target, inflictor, source);
    exit;
  end;

  if (P_Random < target.painchance) and
     ((target.flags and MF_SKULLFLY) = 0) then
  begin
    target.flags := target.flags or MF_JUSTHIT; // fight back!
    P_SetMobjState(target, statenum_t(target.info.painstate));
  end;

  target.reactiontime := 0; // we're awake now...

  if (target.threshold = 0) and
     (source <> nil) and
     (source.flags2 and MF2_BOSS = 0) and
     not ((target._type = Ord(MT_SORCERER2)) and (source._type = Ord(MT_WIZARD))) then
  begin
    // if not intent on another player,
    // chase after this one
    target.target := source;
    target.threshold := BASETHRESHOLD;
    if (target.state = @states[target.info.spawnstate]) and
       (target.info.seestate <> Ord(S_NULL)) then
      P_SetMobjState(target, statenum_t(target.info.seestate));
  end;
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

var
  ultimatemsg: boolean;

procedure P_SetMessage(player: Pplayer_t; const msg: string; ultmsg: boolean = false);
begin
  if (ultimatemsg or (showMessages = 0)) and not ultmsg then
    exit;

  player._message := msg;
//  player.messageTics := MESSAGETICS;
//  BorderTopRefresh = true;
  if ultmsg then
    ultimatemsg := true;
end;

end.



