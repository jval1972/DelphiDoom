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
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit p_sounds;

interface

uses
  p_mobj_h;

//==============================================================================
//
// A_SeeSound
//
//==============================================================================
procedure A_SeeSound(actor: Pmobj_t; origin: Pmobj_t);

//==============================================================================
//
// A_SeeSound1
//
//==============================================================================
procedure A_SeeSound1(actor: Pmobj_t);

//==============================================================================
//
// A_PainSound
//
//==============================================================================
procedure A_PainSound(actor: Pmobj_t; origin: Pmobj_t);

//==============================================================================
//
// A_PainSound1
//
//==============================================================================
procedure A_PainSound1(actor: Pmobj_t);

//==============================================================================
//
// A_AttackSound
//
//==============================================================================
procedure A_AttackSound(actor: Pmobj_t; origin: Pmobj_t);

//==============================================================================
//
// A_AttackSound1
//
//==============================================================================
procedure A_AttackSound1(actor: Pmobj_t);

//==============================================================================
//
// A_MeleeSound
//
//==============================================================================
procedure A_MeleeSound(actor: Pmobj_t; origin: Pmobj_t);

//==============================================================================
//
// A_MeleeSound1
//
//==============================================================================
procedure A_MeleeSound1(actor: Pmobj_t);

//==============================================================================
//
// A_DeathSound
//
//==============================================================================
procedure A_DeathSound(actor: Pmobj_t; origin: Pmobj_t);

//==============================================================================
//
// A_DeathSound1
//
//==============================================================================
procedure A_DeathSound1(actor: Pmobj_t);

//==============================================================================
//
// A_ActiveSound
//
//==============================================================================
procedure A_ActiveSound(actor: Pmobj_t; origin: Pmobj_t);

//==============================================================================
//
// A_ActiveSound1
//
//==============================================================================
procedure A_ActiveSound1(actor: Pmobj_t);

//==============================================================================
//
// A_RipSound
//
//==============================================================================
procedure A_RipSound(actor: Pmobj_t; origin: Pmobj_t);

//==============================================================================
//
// A_RipSound1
//
//==============================================================================
procedure A_RipSound1(actor: Pmobj_t);

implementation

uses
  {$IFDEF DOOM_OR_STRIFE}
  info_h,
  {$ENDIF}
  p_common,
  s_sound;

//==============================================================================
//
// P_IsBossFV
//
//==============================================================================
function P_IsBossFV(const actor: Pmobj_t): boolean;
begin
  if {$IFDEF DOOM}
     (actor._type = Ord(MT_SPIDER)) or
     (actor._type = Ord(MT_CYBORG)) or
     {$ENDIF}
     {$IFDEF HERETIC_OR_HEXEN}
     (actor.flags2 and MF2_BOSS <> 0) or
     {$ENDIF}
     {$IFDEF STRIFE}
     (actor._type = Ord(MT_ENTITY)) or
     (actor._type = Ord(MT_INQUISITOR)) or
     {$ENDIF}
     (actor.flags_ex and MF_EX_BOSS <> 0) then
  begin
    result := true;
    exit;
  end;
  result := false;
end;

//==============================================================================
//
// A_SeeSound
//
//==============================================================================
procedure A_SeeSound(actor: Pmobj_t; origin: Pmobj_t);
begin
  if actor.info.seesound = 0 then
    exit;

  if actor.flags_ex and MF_EX_RANDOMSEESOUND <> 0 then
    P_RandomSound(origin, actor.info.seesound)
  else
    S_StartSound(origin, actor.info.seesound);

  if actor.flags4_ex and MF4_EX_ALWAYSFINISHSOUND <> 0 then
    S_UnlinkSound(origin)
  else if actor.flags4_ex and MF4_EX_NEVERFINISHSOUND <> 0 then
  // From Woof: [FG] make seesounds uninterruptible
  else if full_sounds then
    S_UnlinkSound(origin);
end;

//==============================================================================
//
// A_SeeSound1
//
//==============================================================================
procedure A_SeeSound1(actor: Pmobj_t);
begin
  if P_IsBossFV(actor) or
     (actor.flags2_ex and MF2_EX_FULLVOLSEE <> 0) then
    A_SeeSound(actor, nil)
  else
    A_SeeSound(actor, actor);
end;

//==============================================================================
//
// A_PainSound
//
//==============================================================================
procedure A_PainSound(actor: Pmobj_t; origin: Pmobj_t);
begin
  if actor.info.painsound = 0 then
    exit;

  if actor.flags_ex and MF_EX_RANDOMPAINSOUND <> 0 then
    P_RandomSound(origin, actor.info.painsound)
  else
    S_StartSound(origin, actor.info.painsound);
end;

//==============================================================================
//
// A_PainSound1
//
//==============================================================================
procedure A_PainSound1(actor: Pmobj_t);
begin
  if actor.flags2_ex and MF2_EX_FULLVOLPAIN <> 0 then
    A_PainSound(actor, nil)
  else
    A_PainSound(actor, actor);
end;

//==============================================================================
//
// A_AttackSound
//
//==============================================================================
procedure A_AttackSound(actor: Pmobj_t; origin: Pmobj_t);
begin
  if actor.info.attacksound = 0 then
    exit;

  if actor.flags_ex and MF_EX_RANDOMATTACKSOUND <> 0 then
    P_RandomSound(origin, actor.info.attacksound)
  else
    S_StartSound(origin, actor.info.attacksound);
end;

//==============================================================================
//
// A_AttackSound1
//
//==============================================================================
procedure A_AttackSound1(actor: Pmobj_t);
begin
  if actor.flags2_ex and MF2_EX_FULLVOLATTACK <> 0 then
    A_AttackSound(actor, nil)
  else
    A_AttackSound(actor, actor);
end;

//==============================================================================
//
// A_MeleeSound
//
//==============================================================================
procedure A_MeleeSound(actor: Pmobj_t; origin: Pmobj_t);
begin
  if actor.info.meleesound = 0 then
    exit;

  if actor.flags_ex and MF_EX_RANDOMMELEESOUND <> 0 then
    P_RandomSound(origin, actor.info.meleesound)
  else
    S_StartSound(origin, actor.info.meleesound);
end;

//==============================================================================
//
// A_MeleeSound1
//
//==============================================================================
procedure A_MeleeSound1(actor: Pmobj_t);
begin
  if actor.flags2_ex and MF2_EX_FULLVOLATTACK <> 0 then
    A_MeleeSound(actor, nil)
  else
    A_MeleeSound(actor, actor);
end;

//==============================================================================
//
// A_DeathSound
//
//==============================================================================
procedure A_DeathSound(actor: Pmobj_t; origin: Pmobj_t);
begin
  if actor.info.deathsound = 0 then
    exit;

  if actor.flags_ex and MF_EX_RANDOMDEATHSOUND <> 0 then
    P_RandomSound(origin, actor.info.deathsound)
  else
    S_StartSound(origin, actor.info.deathsound);
end;

//==============================================================================
//
// A_DeathSound1
//
//==============================================================================
procedure A_DeathSound1(actor: Pmobj_t);
begin
  if P_IsBossFV(actor) or
     (actor.flags2_ex and MF2_EX_FULLVOLDEATH <> 0) then
    A_DeathSound(actor, nil)
  else
    A_DeathSound(actor, actor);
end;

//==============================================================================
//
// A_ActiveSound
//
//==============================================================================
procedure A_ActiveSound(actor: Pmobj_t; origin: Pmobj_t);
begin
  if actor.info.activesound = 0 then
    exit;

  if actor.flags_ex and MF_EX_RANDOMACTIVESOUND <> 0 then
    P_RandomSound(origin, actor.info.activesound)
  else
    S_StartSound(origin, actor.info.activesound);
end;

//==============================================================================
//
// A_ActiveSound1
//
//==============================================================================
procedure A_ActiveSound1(actor: Pmobj_t);
begin
  if actor.flags2_ex and MF2_EX_FULLVOLACTIVE <> 0 then
    A_ActiveSound(actor, nil)
  else
    A_ActiveSound(actor, actor);
end;

//==============================================================================
//
// A_RipSound
//
//==============================================================================
procedure A_RipSound(actor: Pmobj_t; origin: Pmobj_t);
begin
  if actor.info.ripsound = 0 then
    exit;

  if actor.flags4_ex and MF4_EX_RANDOMRIPSOUND <> 0 then
    P_RandomSound(origin, actor.info.ripsound)
  else
    S_StartSound(origin, actor.info.ripsound);
end;

//==============================================================================
//
// A_RipSound1
//
//==============================================================================
procedure A_RipSound1(actor: Pmobj_t);
begin
  if actor.flags4_ex and MF4_EX_FULLVOLRIP <> 0 then
    A_RipSound(actor, nil)
  else
    A_RipSound(actor, actor);
end;

end.

