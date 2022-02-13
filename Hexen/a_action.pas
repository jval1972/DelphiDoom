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

unit a_action;

interface

uses
  d_delphi,
  m_fixed,
  p_mobj_h,
  doomdef;

//==============================================================================
//
// A_PotteryExplode
//
//==============================================================================
procedure A_PotteryExplode(actor: Pmobj_t);

//==============================================================================
//
// A_PotteryChooseBit
//
//==============================================================================
procedure A_PotteryChooseBit(actor: Pmobj_t);

//==============================================================================
//
// A_PotteryCheck
//
//==============================================================================
procedure A_PotteryCheck(actor: Pmobj_t);

//==============================================================================
//
// A_CorpseBloodDrip
//
//==============================================================================
procedure A_CorpseBloodDrip(actor: Pmobj_t);

//==============================================================================
//
// A_CorpseExplode
//
//==============================================================================
procedure A_CorpseExplode(actor: Pmobj_t);

//==============================================================================
//
// A_LeafSpawn
//
//==============================================================================
procedure A_LeafSpawn(actor: Pmobj_t);

//==============================================================================
//
// A_LeafThrust
//
//==============================================================================
procedure A_LeafThrust(actor: Pmobj_t);

//==============================================================================
//
// A_LeafCheck
//
//==============================================================================
procedure A_LeafCheck(actor: Pmobj_t);

//==============================================================================
//
// A_BridgeOrbit
//
//==============================================================================
procedure A_BridgeOrbit(actor: Pmobj_t);

//==============================================================================
//
// A_BridgeInit
//
//==============================================================================
procedure A_BridgeInit(actor: Pmobj_t);

//==============================================================================
//
// A_BridgeRemove
//
//==============================================================================
procedure A_BridgeRemove(actor: Pmobj_t);

//==============================================================================
//
// A_HideThing
//
//==============================================================================
procedure A_HideThing(actor: Pmobj_t);

//==============================================================================
//
// A_UnHideThing
//
//==============================================================================
procedure A_UnHideThing(actor: Pmobj_t);

//==============================================================================
//
// A_SetShootable
//
//==============================================================================
procedure A_SetShootable(actor: Pmobj_t);

//==============================================================================
//
// A_UnSetShootable
//
//==============================================================================
procedure A_UnSetShootable(actor: Pmobj_t);

//==============================================================================
//
// A_SetAltShadow
//
//==============================================================================
procedure A_SetAltShadow(actor: Pmobj_t);

//==============================================================================
//
// A_UnSetAltShadow
//
//==============================================================================
procedure A_UnSetAltShadow(actor: Pmobj_t);

//==============================================================================
//
// A_ContMobjSound
//
//==============================================================================
procedure A_ContMobjSound(actor: Pmobj_t);

//==============================================================================
//
// A_ESound
//
//==============================================================================
procedure A_ESound(mo: Pmobj_t);

//==============================================================================
//
// A_Summon
//
//==============================================================================
procedure A_Summon(actor: Pmobj_t);

//==============================================================================
//
// A_FogSpawn
//
//==============================================================================
procedure A_FogSpawn(actor: Pmobj_t);

//==============================================================================
//
// A_FogMove
//
//==============================================================================
procedure A_FogMove(actor: Pmobj_t);

//==============================================================================
//
// A_PoisonBagInit
//
//==============================================================================
procedure A_PoisonBagInit(actor: Pmobj_t);

//==============================================================================
//
// A_PoisonBagCheck
//
//==============================================================================
procedure A_PoisonBagCheck(actor: Pmobj_t);

//==============================================================================
//
// A_PoisonBagDamage
//
//==============================================================================
procedure A_PoisonBagDamage(actor: Pmobj_t);

//==============================================================================
//
// A_PoisonShroom
//
//==============================================================================
procedure A_PoisonShroom(actor: Pmobj_t);

//==============================================================================
//
// A_CheckThrowBomb
//
//==============================================================================
procedure A_CheckThrowBomb(actor: Pmobj_t);

//==============================================================================
//
// A_LocalQuake
//
//==============================================================================
function A_LocalQuake(args: PByteArray; actor: Pmobj_t): boolean;

//==============================================================================
//
// A_Quake
//
//==============================================================================
procedure A_Quake(actor: Pmobj_t);

//==============================================================================
//
// A_TeloSpawn
//
//==============================================================================
procedure A_TeloSpawn(actor: Pmobj_t; typ: integer);

//==============================================================================
//
// A_TeloSpawnA
//
//==============================================================================
procedure A_TeloSpawnA(actor: Pmobj_t);

//==============================================================================
//
// A_TeloSpawnB
//
//==============================================================================
procedure A_TeloSpawnB(actor: Pmobj_t);

//==============================================================================
//
// A_TeloSpawnC
//
//==============================================================================
procedure A_TeloSpawnC(actor: Pmobj_t);

//==============================================================================
//
// A_TeloSpawnD
//
//==============================================================================
procedure A_TeloSpawnD(actor: Pmobj_t);

//==============================================================================
//
// A_CheckTeleRing
//
//==============================================================================
procedure A_CheckTeleRing(actor: Pmobj_t);

//==============================================================================
//
// P_SpawnDirt
//
//==============================================================================
procedure P_SpawnDirt(actor: Pmobj_t; radius: fixed_t);

//==============================================================================
//
// A_ThrustInitUp
//
//==============================================================================
procedure A_ThrustInitUp(actor: Pmobj_t);

//==============================================================================
//
// A_ThrustInitDn
//
//==============================================================================
procedure A_ThrustInitDn(actor: Pmobj_t);

//==============================================================================
//
// A_ThrustRaise
//
//==============================================================================
procedure A_ThrustRaise(actor: Pmobj_t);

//==============================================================================
//
// A_ThrustLower
//
//==============================================================================
procedure A_ThrustLower(actor: Pmobj_t);

//==============================================================================
//
// A_ThrustBlock
//
//==============================================================================
procedure A_ThrustBlock(actor: Pmobj_t);

//==============================================================================
//
// A_ThrustImpale
//
//==============================================================================
procedure A_ThrustImpale(actor: Pmobj_t);

//==============================================================================
//
// A_SoAExplode
//
//==============================================================================
procedure A_SoAExplode(actor: Pmobj_t);

//==============================================================================
//
// A_BellReset1
//
//==============================================================================
procedure A_BellReset1(actor: Pmobj_t);

//==============================================================================
//
// A_BellReset2
//
//==============================================================================
procedure A_BellReset2(actor: Pmobj_t);

//==============================================================================
//
// A_FlameCheck
//
//==============================================================================
procedure A_FlameCheck(actor: Pmobj_t);

//==============================================================================
//
// A_BatSpawnInit
//
//==============================================================================
procedure A_BatSpawnInit(actor: Pmobj_t);

//==============================================================================
//
// A_BatSpawn
//
//==============================================================================
procedure A_BatSpawn(actor: Pmobj_t);

//==============================================================================
//
// A_BatMove
//
//==============================================================================
procedure A_BatMove(actor: Pmobj_t);

//==============================================================================
//
// A_TreeDeath
//
//==============================================================================
procedure A_TreeDeath(actor: Pmobj_t);

var
  localQuakeHappening: array[0..MAXPLAYERS - 1] of integer;

implementation

uses
  d_main,
  d_player,
  g_game,
  info_h,
  info,
  m_rnd,
  p_mobj,
  p_things,
  p_common,
  p_sight,
  p_map,
  p_tick,
  p_inter,
  p_enemy,
  p_maputl,
  p_local,
  r_main,
  s_sound,
  sounddata,
  tables;

//--------------------------------------------------------------------------
//
// Environmental Action routines
//
//--------------------------------------------------------------------------

//==============================================================================
//
// A_PotteryExplode
//
//==============================================================================
procedure A_PotteryExplode(actor: Pmobj_t);
var
  mo: Pmobj_t;
  i: integer;
begin
  mo := nil;

  for i := (P_Random and 3) + 3 downto 0 do
  begin
    mo := P_SpawnMobj(actor.x, actor.y, actor.z, Ord(MT_POTTERYBIT1));
    P_SetMobjState(mo, statenum_t(mo.info.spawnstate + (P_Random mod 5)));
    if mo <> nil then
    begin
      mo.momz := ((P_Random and 7) + 5) * (3 * FRACUNIT div 4);
      mo.momx := _SHL(P_Random - P_Random, FRACBITS - 6);
      mo.momy := _SHL(P_Random - P_Random, FRACBITS - 6);
    end;
  end;

  S_StartSound(mo, Ord(SFX_POTTERY_EXPLODE));

  if actor.args[0] <> 0 then
  begin // Spawn an item
    if not nomonsters or
       (mobjinfo[Ord(TranslateThingType[actor.args[0]])].flags and MF_COUNTKILL = 0) then
    begin // Only spawn monsters if not -nomonsters
      P_SpawnMobj(actor.x, actor.y, actor.z, Ord(TranslateThingType[actor.args[0]]));
    end;
  end;
  P_RemoveMobj(actor);
end;

//==============================================================================
//
// A_PotteryChooseBit
//
//==============================================================================
procedure A_PotteryChooseBit(actor: Pmobj_t);
begin
  P_SetMobjState(actor, statenum_t(actor.info.deathstate + (P_Random mod 5) + 1));
  actor.tics := 256 + (P_Random * 2);
end;

//==============================================================================
//
// A_PotteryCheck
//
//==============================================================================
procedure A_PotteryCheck(actor: Pmobj_t);
var
  i: integer;
  pmo: Pmobj_t;
  check: boolean;
  rp: angle_t;
  st: integer;
begin
  if not netgame then
  begin
    pmo := players[consoleplayer].mo;
    rp := R_PointToAngle2(pmo.x, pmo.y, actor.x, actor.y);
    if rp > pmo.angle then
      check := rp - pmo.angle <= ANG45
    else
      check := pmo.angle - rp <= ANG45;
    if check and P_CheckSight(actor, pmo) then
    begin // Previous state (pottery bit waiting state)
      st := (integer(actor.state) - integer(@states[0])) div SizeOf(state_t);
      dec(st);
      P_SetMobjState(actor, statenum_t(st));
    end
    else
    begin
      exit;
    end;
  end
  else
  begin
    for i := 0 to MAXPLAYERS - 1 do
    begin
      if not playeringame[i] then
      begin
        continue;
      end;
      pmo := players[i].mo;
      rp := R_PointToAngle2(pmo.x, pmo.y, actor.x, actor.y);
      if rp > pmo.angle then
        check := rp - pmo.angle <= ANG45
      else
        check := pmo.angle - rp <= ANG45;
      if check and P_CheckSight(actor, pmo) then
      begin // Previous state (pottery bit waiting state)
        st := (integer(actor.state) - integer(@states[0])) div SizeOf(state_t);
        dec(st);
        P_SetMobjState(actor, statenum_t(st));
        exit;
      end
    end;
  end;
end;

//==============================================================================
//
// A_CorpseBloodDrip
//
//==============================================================================
procedure A_CorpseBloodDrip(actor: Pmobj_t);
begin
  if P_Random <= 128 then
    P_SpawnMobj(actor.x, actor.y, actor.z + actor.height div 2, Ord(MT_CORPSEBLOODDRIP));
end;

//==============================================================================
//
// A_CorpseExplode
//
//==============================================================================
procedure A_CorpseExplode(actor: Pmobj_t);
var
  mo: Pmobj_t;
  i: integer;
begin
  for i := (P_Random and 3) + 3 downto 0 do
  begin
    mo := P_SpawnMobj(actor.x, actor.y, actor.z, Ord(MT_CORPSEBIT));
    P_SetMobjState(mo, statenum_t(mo.info.spawnstate + (P_Random mod 3)));
    if mo <> nil then
    begin
      mo.momz := ((P_Random and 7) + 5) * (3 * FRACUNIT div 4);
      mo.momx := _SHL(P_Random - P_Random, FRACBITS - 6);
      mo.momy := _SHL(P_Random - P_Random, FRACBITS - 6);
    end;
  end;
  // Spawn a skull
  mo := P_SpawnMobj(actor.x, actor.y, actor.z, Ord(MT_CORPSEBIT));
  P_SetMobjState(mo, S_CORPSEBIT_4);
  if mo <> nil then
  begin
    mo.momz := ((P_Random and 7) + 5) * (3 * FRACUNIT div 4);
    mo.momx := _SHL(P_Random - P_Random, FRACBITS - 6);
    mo.momy := _SHL(P_Random - P_Random, FRACBITS - 6);
    S_StartSound(mo, Ord(SFX_FIRED_DEATH));
  end;
  P_RemoveMobj(actor);
end;

//==============================================================================
//
// A_LeafSpawn
//
//==============================================================================
procedure A_LeafSpawn(actor: Pmobj_t);
var
  mo: Pmobj_t;
  i: integer;
begin

  for i := (P_Random and 3) + 1 downto 0 do
  begin
    mo := P_SpawnMobj(actor.x + _SHL(P_Random - P_Random, 14),
                      actor.y + _SHL(P_Random - P_Random, 14),
                      actor.z + _SHL(P_Random, 14),
                      Ord(MT_LEAF1) + (P_Random and 1));
    if mo <> nil then
    begin
      P_ThrustMobj(mo, actor.angle, _SHL(P_Random, 9) + 3 * FRACUNIT);
      mo.target := actor;
      mo.special1 := 0;
    end;
  end;
end;

//==============================================================================
//
// A_LeafThrust
//
//==============================================================================
procedure A_LeafThrust(actor: Pmobj_t);
begin
  if P_Random <= 96 then
    actor.momz := actor.momz + _SHL(P_Random, 9) + FRACUNIT;
end;

//==============================================================================
//
// A_LeafCheck
//
//==============================================================================
procedure A_LeafCheck(actor: Pmobj_t);
begin
  inc(actor.special1);
  if actor.special1 >= 20 then
  begin
    P_SetMobjState(actor, S_NULL);
    exit;
  end;
  if P_Random > 64 then
  begin
    if (actor.momx = 0) and (actor.momy = 0) then
      P_ThrustMobj(actor, actor.target.angle, _SHL(P_Random, 9) + FRACUNIT);
    exit;
  end;
  P_SetMobjState(actor, S_LEAF1_8);
  actor.momz := _SHL(P_Random, 9) + FRACUNIT;
  P_ThrustMobj(actor, actor.target.angle, _SHL(P_Random, 9) + 2 * FRACUNIT);
  actor.flags := actor.flags or MF_MISSILE;
end;

const
  orbitTableX: array[0..255] of integer = (
    983025,  982725,  981825,  980340,  978255,  975600,  972330,  968490,
    964065,  959070,  953475,  947325,  940590,  933300,  925440,  917025,
    908055,  898545,  888495,  877905,  866775,  855135,  842985,  830310,
    817155,  803490,  789360,  774735,  759660,  744120,  728130,  711690,
    694845,  677565,  659880,  641805,  623340,  604500,  585285,  565725,
    545820,  525600,  505050,  484200,  463065,  441645,  419955,  398010,
    375840,  353430,  330810,  307995,  285000,  261825,  238485,  215010,
    191400,  167685,  143865,  119955,   95970,   71940,   47850,   23745,
      -375,  -24495,  -48600,  -72690,  -96720, -120705, -144600, -168420,
   -192150, -215745, -239220, -262545, -285720, -308715, -331530, -354135,
   -376530, -398700, -420630, -442320, -463725, -484860, -505695, -526230,
   -546450, -566340, -585885, -605085, -623925, -642375, -660435, -678105,
   -695370, -712215, -728625, -744600, -760125, -775200, -789795, -803925,
   -817575, -830715, -843375, -855510, -867135, -878235, -888810, -898845,
   -908340, -917295, -925695, -933540, -940815, -947520, -953670, -959235,
   -964215, -968625, -972450, -975690, -978330, -980400, -981870, -982740,
   -983025, -982725, -981825, -980340, -978255, -975600, -972330, -968490,
   -964065, -959070, -953475, -947325, -940590, -933300, -925440, -917025,
   -908055, -898545, -888495, -877905, -866775, -855135, -842985, -830310,
   -817155, -803490, -789360, -774735, -759660, -744120, -728130, -711690,
   -694845, -677565, -659880, -641805, -623340, -604485, -585285, -565725,
   -545820, -525600, -505050, -484200, -463065, -441645, -419955, -398010,
   -375840, -353430, -330810, -307995, -285000, -261825, -238485, -215010,
   -191400, -167685, -143865, -119955,  -95970,  -71940,  -47850,  -23745,
       375,   24495,   48600,   72690,   96720,  120705,  144600,  168420,
    192150,  215745,  239220,  262545,  285720,  308715,  331530,  354135,
    376530,  398700,  420630,  442320,  463725,  484860,  505695,  526230,
    546450,  566340,  585885,  605085,  623925,  642375,  660435,  678105,
    695370,  712215,  728625,  744600,  760125,  775200,  789795,  803925,
    817575,  830715,  843375,  855510,  867135,  878235,  888810,  898845,
    908340,  917295,  925695,  933540,  940815,  947520,  953670,  959235,
    964215,  968625,  972450,  975690,  978330,  980400,  981870,  982740
  );

  orbitTableY: array[0..255] of integer = (
         375,   24495,   48600,   72690,   96720,  120705,  144600,  168420,
      192150,  215745,  239220,  262545,  285720,  308715,  331530,  354135,
      376530,  398700,  420630,  442320,  463725,  484860,  505695,  526230,
      546450,  566340,  585885,  605085,  623925,  642375,  660435,  678105,
      695370,  712215,  728625,  744600,  760125,  775200,  789795,  803925,
      817575,  830715,  843375,  855510,  867135,  878235,  888810,  898845,
      908340,  917295,  925695,  933540,  940815,  947520,  953670,  959235,
      964215,  968625,  972450,  975690,  978330,  980400,  981870,  982740,
      983025,  982725,  981825,  980340,  978255,  975600,  972330,  968490,
      964065,  959070,  953475,  947325,  940590,  933300,  925440,  917025,
      908055,  898545,  888495,  877905,  866775,  855135,  842985,  830310,
      817155,  803490,  789360,  774735,  759660,  744120,  728130,  711690,
      694845,  677565,  659880,  641805,  623340,  604500,  585285,  565725,
      545820,  525600,  505050,  484200,  463065,  441645,  419955,  398010,
      375840,  353430,  330810,  307995,  285000,  261825,  238485,  215010,
      191400,  167685,  143865,  119955,   95970,   71940,   47850,   23745,
        -375,  -24495,  -48600,  -72690,  -96720, -120705, -144600, -168420,
     -192150, -215745, -239220, -262545, -285720, -308715, -331530, -354135,
     -376530, -398700, -420630, -442320, -463725, -484860, -505695, -526230,
     -546450, -566340, -585885, -605085, -623925, -642375, -660435, -678105,
     -695370, -712215, -728625, -744600, -760125, -775200, -789795, -803925,
     -817575, -830715, -843375, -855510, -867135, -878235, -888810, -898845,
     -908340, -917295, -925695, -933540, -940815, -947520, -953670, -959235,
     -964215, -968625, -972450, -975690, -978330, -980400, -981870, -982740,
     -983025, -982725, -981825, -980340, -978255, -975600, -972330, -968490,
     -964065, -959070, -953475, -947325, -940590, -933300, -925440, -917025,
     -908055, -898545, -888495, -877905, -866775, -855135, -842985, -830310,
     -817155, -803490, -789360, -774735, -759660, -744120, -728130, -711690,
     -694845, -677565, -659880, -641805, -623340, -604485, -585285, -565725,
     -545820, -525600, -505050, -484200, -463065, -441645, -419955, -398010,
     -375840, -353430, -330810, -307995, -285000, -261825, -238485, -215010,
     -191400, -167685, -143865, -119955, -95970, -71940, -47850, -23745
  );

//==============================================================================
// A_BridgeOrbit
//
// New bridge stuff
//  Parent
//    special1  true == removing from world
//
//  Child
//    target    pointer to center mobj
//    args[0]   angle of ball
//
//==============================================================================
procedure A_BridgeOrbit(actor: Pmobj_t);
begin
  if actor.target.special1 <> 0 then
    P_SetMobjState(actor, S_NULL);

  actor.args[0] := actor.args[0] + 3;
  actor.x := actor.target.x + orbitTableX[actor.args[0]];
  actor.y := actor.target.y + orbitTableY[actor.args[0]];
  actor.z := actor.target.z;
end;

//==============================================================================
//
// A_BridgeInit
//
//==============================================================================
procedure A_BridgeInit(actor: Pmobj_t);
var
  startangle: byte;
  ball1, ball2, ball3: Pmobj_t;
  cx, cy, cz: fixed_t;
begin
//  GenerateOrbitTable;
  cx := actor.x;
  cy := actor.y;
  cz := actor.z;
  startangle := P_Random;
  actor.special1 := 0;

  // Spawn triad into world
  ball1 := P_SpawnMobj(cx, cy, cz, Ord(MT_BRIDGEBALL));
  ball1.args[0] := startangle;
  ball1.target := actor;

  ball2 := P_SpawnMobj(cx, cy, cz, Ord(MT_BRIDGEBALL));
  ball2.args[0] := (startangle + 85) and 255;
  ball2.target := actor;

  ball3 := P_SpawnMobj(cx, cy, cz, Ord(MT_BRIDGEBALL));
  ball3.args[0] := (startangle + 170) and 255;
  ball3.target := actor;

  A_BridgeOrbit(ball1);
  A_BridgeOrbit(ball2);
  A_BridgeOrbit(ball3);
end;

//==============================================================================
//
// A_BridgeRemove
//
//==============================================================================
procedure A_BridgeRemove(actor: Pmobj_t);
begin
  actor.special1 := 1; // Removing the bridge
  actor.flags := actor.flags and not MF_SOLID;
  P_SetMobjState(actor, S_FREE_BRIDGE1);
end;

//==============================================================================
//
// A_HideThing
//
//==============================================================================
procedure A_HideThing(actor: Pmobj_t);
begin
  actor.flags2 := actor.flags2 or MF2_DONTDRAW;
end;

//==============================================================================
//
// A_UnHideThing
//
//==============================================================================
procedure A_UnHideThing(actor: Pmobj_t);
begin
  actor.flags2 := actor.flags2 and not MF2_DONTDRAW;
end;

//==============================================================================
//
// A_SetShootable
//
//==============================================================================
procedure A_SetShootable(actor: Pmobj_t);
begin
  actor.flags2 := actor.flags2 and not MF2_NONSHOOTABLE;
  actor.flags := actor.flags or MF_SHOOTABLE;
end;

//==============================================================================
//
// A_UnSetShootable
//
//==============================================================================
procedure A_UnSetShootable(actor: Pmobj_t);
begin
  actor.flags2 := actor.flags2 or MF2_NONSHOOTABLE;
  actor.flags := actor.flags and not MF_SHOOTABLE;
end;

//==============================================================================
//
// A_SetAltShadow
//
//==============================================================================
procedure A_SetAltShadow(actor: Pmobj_t);
begin
  actor.flags := actor.flags and not MF_SHADOW;
  actor.flags := actor.flags or MF_ALTSHADOW;
end;

//==============================================================================
//
// A_UnSetAltShadow
//
//==============================================================================
procedure A_UnSetAltShadow(actor: Pmobj_t);
begin
  actor.flags := actor.flags and not MF_ALTSHADOW;
end;

//--------------------------------------------------------------------------
//
// Sound Action Routines
//
//--------------------------------------------------------------------------

//==============================================================================
//
// A_ContMobjSound
//
//==============================================================================
procedure A_ContMobjSound(actor: Pmobj_t);
begin
  case actor._type of
    Ord(MT_SERPENTFX):
      S_StartSound(actor, Ord(SFX_SERPENTFX_CONTINUOUS));
    Ord(MT_HAMMER_MISSILE):
      S_StartSound(actor, Ord(SFX_FIGHTER_HAMMER_CONTINUOUS));
    Ord(MT_QUAKE_FOCUS):
      S_StartSound(actor, Ord(SFX_EARTHQUAKE));
  end;
end;

//==============================================================================
//
// PROC A_ESound
//
//==============================================================================
procedure A_ESound(mo: Pmobj_t);
var
  sound: integer;
begin
  if mo._type = Ord(MT_SOUNDWIND) then
  begin
    sound := Ord(SFX_WIND);
    S_StartSound(mo, sound);
  end;
end;

//==============================================================================
// A_Summon
//
// Summon Minotaur -- see p_enemy for variable descriptions
//
//==============================================================================
procedure A_Summon(actor: Pmobj_t);
var
  mo: Pmobj_t;
  master: Pmobj_t;
begin
  mo := P_SpawnMobj(actor.x, actor.y, actor.z, Ord(MT_MINOTAUR));
  if mo <> nil then
  begin
    if not P_TestMobjLocation(mo) or (actor.special1 = 0) then
    begin // Didn't fit - change back to artifact
      P_SetMobjState(mo, S_NULL);
      mo := P_SpawnMobj(actor.x, actor.y, actor.z, Ord(MT_SUMMONMAULATOR));
      if mo <> nil then
        mo.flags2 := mo.flags2 or MF2_DROPPED;
    end;

    memcpy(@mo.args, @leveltime, SizeOf(leveltime));
    if actor.special1 <> 0 then // JVAL
    begin
      master := Pmobj_t(actor.special1);
      if master.flags and MF_CORPSE <> 0 then
      begin  // Master dead
        mo.special1 := 0;    // No master
      end
      else
      begin
        mo.special1 := actor.special1;    // Pointer to master (mobj_t *)
        P_GivePower(master.player, pw_minotaur);
      end;

      // Make smoke puff
      P_SpawnMobj(actor.x, actor.y, actor.z, Ord(MT_MNTRSMOKE));
      S_StartSound(actor, Ord(SFX_MAULATOR_ACTIVE));
    end;
  end;
end;

//==============================================================================
// A_FogSpawn
//
// Fog Variables:
//
//    args[0]    Speed (0..10) of fog
//    args[1]    Angle of spread (0..128)
//    args[2]    Frequency of spawn (1..10)
//    args[3]    Lifetime countdown
//    args[4]    Boolean: fog moving?
//    special1   Internal:  Counter for spawn frequency
//    special2   Internal:  Index into floatbob table
//
//==============================================================================
procedure A_FogSpawn(actor: Pmobj_t);
var
  mo: Pmobj_t;
  delta: byte;
begin
  if actor.special1 > 1 then
  begin
    dec(actor.special1);
    exit;
  end;

  mo := nil;
  actor.special1 := actor.args[2];    // Reset frequency count

  case P_Random and 3 of
    0: mo := P_SpawnMobj(actor.x, actor.y, actor.z, Ord(MT_FOGPATCHS));
    1: mo := P_SpawnMobj(actor.x, actor.y, actor.z, Ord(MT_FOGPATCHM));
    2: mo := P_SpawnMobj(actor.x, actor.y, actor.z, Ord(MT_FOGPATCHL));
  end;

  if mo <> nil then
  begin
    delta := actor.args[1];
    if delta = 0 then
      delta := 1;
    mo.angle := actor.angle + _SHLW((P_Random mod delta) - (delta div 2), 24);
    mo.target := actor;
    if actor.args[0] < 1 then
      actor.args[0] := 1;
    mo.args[0] := (P_Random mod actor.args[0]) + 1; // Random speed
    mo.args[3] := actor.args[3];                    // Set lifetime
    mo.args[4] := 1;                                // Set to moving
    mo.special2 := P_Random and 63;
  end;
end;

//==============================================================================
//
// A_FogMove
//
//==============================================================================
procedure A_FogMove(actor: Pmobj_t);
var
  speed: integer;
  angle: angle_t;
  weaveindex: integer;
begin
  if actor.args[4] = 0 then
    exit;

  if actor.args[3] <= 0 then
  begin
    P_SetMobjStateNF(actor, statenum_t(actor.info.deathstate));
    exit;
  end;

  dec(actor.args[3]);

  if actor.args[3] mod 4 = 0 then
  begin
    weaveindex := actor.special2;
    actor.z := actor.z + FloatBobOffsets[weaveindex] div 2;
    actor.special2 := (weaveindex + 1) and 63;
  end;

  speed := _SHL(actor.args[0], FRACBITS);

  angle := actor.angle shr ANGLETOFINESHIFT;
  actor.momx := FixedMul(speed, finecosine[angle]);
  actor.momy := FixedMul(speed, finesine[angle]);
end;

//==============================================================================
//
// A_PoisonBagInit
//
//==============================================================================
procedure A_PoisonBagInit(actor: Pmobj_t);
var
  mo: Pmobj_t;
begin
  mo := P_SpawnMobj(actor.x, actor.y, actor.z + 28 * FRACUNIT, Ord(MT_POISONCLOUD));
  if mo = nil then
    exit;

  mo.momx := 1; // missile objects must move to impact other objects
  mo.special1 := 24 + (P_Random and 7);
  mo.special2 := 0;
  mo.target := actor.target;
  mo.radius := 20 * FRACUNIT;
  mo.height := 30 * FRACUNIT;
  mo.flags := mo.flags and not MF_NOCLIP;
end;

//==============================================================================
//
// A_PoisonBagCheck
//
//==============================================================================
procedure A_PoisonBagCheck(actor: Pmobj_t);
begin
  dec(actor.special1); // JVAL SOS
  if actor.special1 = 0 then
    P_SetMobjState(actor, S_POISONCLOUD_X1);
end;

//==============================================================================
//
// A_PoisonBagDamage
//
//==============================================================================
procedure A_PoisonBagDamage(actor: Pmobj_t);
var
  bobIndex: integer;
begin
  A_Explode(actor);

  bobIndex := actor.special2;
  actor.z := actor.z + _SHR(FloatBobOffsets[bobIndex], 4);
  actor.special2 := (bobIndex + 1) and 63;
end;

//==============================================================================
//
// A_PoisonShroom
//
//==============================================================================
procedure A_PoisonShroom(actor: Pmobj_t);
begin
  actor.tics := 128 + 2 * P_Random;
end;

//==============================================================================
//
// A_CheckThrowBomb
//
//==============================================================================
procedure A_CheckThrowBomb(actor: Pmobj_t);
begin
  if (abs(actor.momx) < 3 * FRACUNIT div 2) and (abs(actor.momy) < 3 * FRACUNIT div 2) and
     (actor.momz < 2 * FRACUNIT) and
     (actor.state = @states[Ord(S_THROWINGBOMB6)]) then
  begin
    P_SetMobjState(actor, S_THROWINGBOMB7);
    actor.z := actor.floorz;
    actor.momz := 0;
    actor.flags2 := actor.flags2 and not MF2_FLOORBOUNCE;
    actor.flags := actor.flags and not MF_MISSILE;
  end;
  dec(actor.health);
  if actor.health <= 0 then
    P_SetMobjState(actor, statenum_t(actor.info.deathstate));
end;

//==============================================================================
//
// A_LocalQuake
//
// Quake variables
//
//    args[0]    Intensity on richter scale (2..9)
//    args[1]    Duration in tics
//    args[2]    Radius for damage
//    args[3]    Radius for tremor
//    args[4]    TID of map thing for focus of quake
//
//==============================================================================
function A_LocalQuake(args: PByteArray; actor: Pmobj_t): boolean;
var
  focus, target: Pmobj_t;
  lastfound: integer;
begin
  result := false;
  lastfound := 0;
  // Find all quake foci
  repeat
    target := P_FindMobjFromTID(args[4], @lastfound);
    if target <> nil then
    begin
      focus := P_SpawnMobj(target.x, target.y, target.z, Ord(MT_QUAKE_FOCUS));
      if focus <> nil then
      begin
        focus.args[0] := args[0];
        focus.args[1] := args[1] div 2;  // decremented every 2 tics
        focus.args[2] := args[2];
        focus.args[3] := args[3];
        focus.args[4] := args[4];
        result := true;
      end;
    end;
  until target = nil;
end;

//==============================================================================
//
// A_Quake
//
//==============================================================================
procedure A_Quake(actor: Pmobj_t);
var
  an: angle_t;
  player: Pplayer_t;
  victim: Pmobj_t;
  richters: integer;
  playnum: integer;
  dist: fixed_t;
  rnd: LongWord;
begin
  richters := actor.args[0];

  dec(actor.args[1]);
  if actor.args[1] > 0 then
  begin
    for playnum := 0 to MAXPLAYERS - 1 do
    begin
      player := @players[playnum];
      if not playeringame[playnum] then
        continue;

      victim := player.mo;
      dist := _SHR(P_AproxDistance(actor.x - victim.x, actor.y - victim.y), FRACBITS + 6);
      // Tested in tile units (64 pixels)
      if dist < actor.args[3] then    // In tremor radius
        localQuakeHappening[playnum] := richters;

      // Check if in damage radius
      if (dist < actor.args[2]) and (victim.z <= victim.floorz) then
      begin
        if P_Random < 50 then
          P_DamageMobj(victim, nil, nil, HITDICE(1));
        // Thrust player around
        rnd := P_Random;
        an := victim.angle + ANG1 * rnd;
        P_ThrustMobj(victim, an, _SHL(richters, FRACBITS - 1));
      end;
    end;
  end
  else
  begin
    for playnum := 0 to MAXPLAYERS - 1 do
      localQuakeHappening[playnum] := 0;
    P_SetMobjState(actor, S_NULL);
  end;
end;

//
// Teleport other stuff
//

const
  TELEPORT_LIFE = 1;

//==============================================================================
//
// A_TeloSpawn
//
//==============================================================================
procedure A_TeloSpawn(actor: Pmobj_t; typ: integer);
var
  mo: Pmobj_t;
begin
  mo := P_SpawnMobj(actor.x, actor.y, actor.z, typ);
  if mo <> nil then
  begin
    mo.special1 := TELEPORT_LIFE;      // Lifetime countdown
    mo.angle := actor.angle;
    mo.target := actor.target;
    mo.momx := actor.momx div 2;
    mo.momy := actor.momy div 2;
    mo.momz := actor.momz div 2;
  end;
end;

//==============================================================================
//
// A_TeloSpawnA
//
//==============================================================================
procedure A_TeloSpawnA(actor: Pmobj_t);
begin
  A_TeloSpawn(actor, Ord(MT_TELOTHER_FX2));
end;

//==============================================================================
//
// A_TeloSpawnB
//
//==============================================================================
procedure A_TeloSpawnB(actor: Pmobj_t);
begin
  A_TeloSpawn(actor, Ord(MT_TELOTHER_FX3));
end;

//==============================================================================
//
// A_TeloSpawnC
//
//==============================================================================
procedure A_TeloSpawnC(actor: Pmobj_t);
begin
  A_TeloSpawn(actor, Ord(MT_TELOTHER_FX4));
end;

//==============================================================================
//
// A_TeloSpawnD
//
//==============================================================================
procedure A_TeloSpawnD(actor: Pmobj_t);
begin
  A_TeloSpawn(actor, Ord(MT_TELOTHER_FX5));
end;

//==============================================================================
//
// A_CheckTeleRing
//
//==============================================================================
procedure A_CheckTeleRing(actor: Pmobj_t);
begin
  dec(actor.special1);
  if actor.special1 <= 0 then
    P_SetMobjState(actor, statenum_t(actor.info.deathstate));
end;

//==============================================================================
// P_SpawnDirt
//
// Dirt stuff
//
//==============================================================================
procedure P_SpawnDirt(actor: Pmobj_t; radius: fixed_t);
var
  x, y, z: fixed_t;
  dtype: mobjtype_t;
  mo: Pmobj_t;
  angle: angle_t;
begin
  angle := _SHLW(P_Random, 5);    //  shl 24  shr 19
  x := actor.x + FixedMul(radius, finecosine[angle]);
  y := actor.y + FixedMul(radius, finesine[angle]);
  z := actor.z + _SHL(P_Random, 9) + FRACUNIT;
  case P_Random and 6 of
    0: dtype := MT_DIRT1;
    1: dtype := MT_DIRT2;
    2: dtype := MT_DIRT3;
    3: dtype := MT_DIRT4;
    4: dtype := MT_DIRT5;
  else
    dtype := MT_DIRT6;
  end;
  mo := P_SpawnMobj(x, y, z, Ord(dtype));
  if mo <> nil then
    mo.momz := _SHL(P_Random, 10);
end;

//==============================================================================
// A_ThrustInitUp
//
// Thrust floor stuff
//
// Thrust Spike Variables
//    special1    pointer to dirt clump mobj
//    special2    speed of raise
//    args[0]    0 := lowered,  1 := raised
//    args[1]    0 := normal,   1 := bloody
//
//==============================================================================
procedure A_ThrustInitUp(actor: Pmobj_t);
begin
  actor.special2 := 5;    // Raise speed
  actor.args[0] := 1;    // Mark as up
  actor.floorclip := 0;
  actor.flags := MF_SOLID;
  actor.flags2 := MF2_NOTELEPORT or MF2_FLOORCLIP;
  actor.special1 := 0;
end;

//==============================================================================
//
// A_ThrustInitDn
//
//==============================================================================
procedure A_ThrustInitDn(actor: Pmobj_t);
var
  mo: Pmobj_t;
begin
  actor.special2 := 5;    // Raise speed
  actor.args[0] := 0;    // Mark as down
  actor.floorclip := actor.info.height;
  actor.flags := 0;
  actor.flags2 := MF2_NOTELEPORT or MF2_FLOORCLIP or MF2_DONTDRAW;
  mo := P_SpawnMobj(actor.x, actor.y, actor.z, Ord(MT_DIRTCLUMP));
  actor.special1 := integer(mo);
end;

//==============================================================================
//
// A_ThrustRaise
//
//==============================================================================
procedure A_ThrustRaise(actor: Pmobj_t);
begin
  if A_RaiseMobj(actor) then
  begin  // Reached it's target height
    actor.args[0] := 1;
    if actor.args[1] <> 0 then
      P_SetMobjStateNF(actor, S_BTHRUSTINIT2_1)
    else
      P_SetMobjStateNF(actor, S_THRUSTINIT2_1);
  end;

  // Lose the dirt clump
  if (actor.floorclip < actor.height) and (actor.special1 <> 0) then
  begin
    P_RemoveMobj(Pmobj_t(actor.special1));
    actor.special1 := 0;
  end;

  // Spawn some dirt
  if P_Random < 40 then
    P_SpawnDirt(actor, actor.radius);
  inc(actor.special2);              // Increase raise speed
end;

//==============================================================================
//
// A_ThrustLower
//
//==============================================================================
procedure A_ThrustLower(actor: Pmobj_t);
begin
  if A_SinkMobj(actor) then
  begin
    actor.args[0] := 0;
    if actor.args[1] <> 0 then
      P_SetMobjStateNF(actor, S_BTHRUSTINIT1_1)
    else
      P_SetMobjStateNF(actor, S_THRUSTINIT1_1);
  end;
end;

//==============================================================================
//
// A_ThrustBlock
//
//==============================================================================
procedure A_ThrustBlock(actor: Pmobj_t);
begin
  actor.flags := actor.flags or MF_SOLID;
end;

//==============================================================================
//
// A_ThrustImpale
//
//==============================================================================
procedure A_ThrustImpale(actor: Pmobj_t);
begin
  // Impale all shootables in radius
  PIT_ThrustSpike(actor);
end;

//==============================================================================
//
// A_SoAExplode - Suit of Armor Explode
//
//==============================================================================
procedure A_SoAExplode(actor: Pmobj_t);
var
  mo: Pmobj_t;
  i: integer;
begin
  for i := 0 to 9 do
  begin
    mo := P_SpawnMobj(actor.x + _SHL(P_Random - 128, 12),
                      actor.y + _SHL(P_Random - 128, 12),
                      actor.z + (P_Random * actor.height div 256),
                      Ord(MT_ZARMORCHUNK));
    P_SetMobjState(mo, statenum_t(mo.info.spawnstate + i));
    if mo <> nil then
    begin
      mo.momz := ((P_Random and 7) + 5) * FRACUNIT;
      mo.momx := _SHL(P_Random - P_Random, FRACBITS - 6);
      mo.momy := _SHL(P_Random - P_Random, FRACBITS - 6);
    end;
  end;

  if actor.args[0] <> 0 then
  begin // Spawn an item
    if not nomonsters or (mobjinfo[Ord(TranslateThingType[actor.args[0]])].flags and MF_COUNTKILL = 0) then
    begin // Only spawn monsters if not -nomonsters
      P_SpawnMobj(actor.x, actor.y, actor.z, Ord(TranslateThingType[actor.args[0]]));
    end;
  end;
  S_StartSound(mo, Ord(SFX_SUITOFARMOR_BREAK));
  P_RemoveMobj(actor);
end;

//==============================================================================
//
// A_BellReset1
//
//==============================================================================
procedure A_BellReset1(actor: Pmobj_t);
begin
  actor.flags := actor.flags or MF_NOGRAVITY;
  actor.height := actor.height * 4;
end;

//==============================================================================
//
// A_BellReset2
//
//==============================================================================
procedure A_BellReset2(actor: Pmobj_t);
begin
  actor.flags := actor.flags or MF_SHOOTABLE;
  actor.flags := actor.flags and not MF_CORPSE;
  actor.health := 5;
end;

//==============================================================================
//
// A_FlameCheck
//
//==============================================================================
procedure A_FlameCheck(actor: Pmobj_t);
begin
  dec(actor.args[0]);
  if actor.args[0] <= 0 then // Called every 8 tics // JVAL SOS
    P_SetMobjState(actor, S_NULL);
end;

//==============================================================================
// A_BatSpawnInit
//
// Bat Spawner Variables
//  special1  frequency counter
//  special2
//  args[0]    frequency of spawn (1=fastest, 10=slowest)
//  args[1]    spread angle (0..255)
//  args[2]
//  args[3]    duration of bats (in octics)
//  args[4]    turn amount per move (in degrees)
//
// Bat Variables
//  special2  lifetime counter
//  args[4]   turn amount per move (in degrees)
//
//==============================================================================
procedure A_BatSpawnInit(actor: Pmobj_t);
begin
  actor.special1 := 0;  // Frequency count
end;

//==============================================================================
//
// A_BatSpawn
//
//==============================================================================
procedure A_BatSpawn(actor: Pmobj_t);
var
  mo: Pmobj_t;
  delta: integer;
  angle: byte;
begin
  // Countdown until next spawn
  dec(actor.special1);
  if actor.special1 > 0 then
    exit;
  actor.special1 := actor.args[0];    // Reset frequency count

  delta := actor.args[1];
  if delta = 0 then
    delta := 1;
  angle := actor.angle + _SHLW((P_Random mod delta) - (delta div 2), 24);
  mo := P_SpawnMissileAngle(actor, Ord(MT_BAT), angle, 0);
  if mo <> nil then
  begin
    mo.args[0] := P_Random and 63;      // floatbob index
    mo.args[4] := actor.args[4];        // turn degrees
    mo.special2 := actor.args[3] * 8;   // Set lifetime
    mo.target := actor;
  end;
end;

//==============================================================================
//
// A_BatMove
//
//==============================================================================
procedure A_BatMove(actor: Pmobj_t);
var
  newangle: angle_t;
  speed: fixed_t;
begin
  if actor.special2 < 0 then
    P_SetMobjState(actor, statenum_t(actor.info.deathstate));

  dec(actor.special2, 2);    // Called every 2 tics

  if P_Random < 128 then
    newangle := actor.angle + ANG1 * actor.args[4]
  else
    newangle := actor.angle - ANG1 * actor.args[4];

  // Adjust momentum vector to new direction
  newangle := newangle shr ANGLETOFINESHIFT;
  speed := FixedMul(actor.info.speed, _SHL(P_Random, 10));
  actor.momx := FixedMul(speed, finecosine[newangle]);
  actor.momy := FixedMul(speed, finesine[newangle]);

  if P_Random < 15 then
    S_StartSound(actor, Ord(SFX_BAT_SCREAM));

  // Handle Z movement
  actor.z := actor.target.z + 2 * FloatBobOffsets[actor.args[0]];
  actor.args[0] := (actor.args[0] + 3) and 63;
end;

//==============================================================================
//
// A_TreeDeath
//
//==============================================================================
procedure A_TreeDeath(actor: Pmobj_t);
begin
  if actor.flags2 and MF2_FIREDAMAGE = 0 then
  begin
    actor.height := actor.height * 4;
    actor.flags := actor.flags or MF_SHOOTABLE;
    actor.flags := actor.flags and not (MF_CORPSE + MF_DROPOFF);
    actor.health := 35;
  end
  else
    P_SetMobjState(actor, statenum_t(actor.info.meleestate));
end;

end.

