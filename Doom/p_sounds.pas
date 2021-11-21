//------------------------------------------------------------------------------
//
//  DelphiDoom: A modified and improved DOOM engine for Windows
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 1993-1996 by id Software, Inc.
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

unit p_sounds;

interface

uses
  p_mobj_h;

procedure A_SeeSound(actor: Pmobj_t; origin: Pmobj_t);
procedure A_SeeSound1(actor: Pmobj_t);

procedure A_PainSound(actor: Pmobj_t; origin: Pmobj_t);
procedure A_PainSound1(actor: Pmobj_t);

procedure A_AttackSound(actor: Pmobj_t; origin: Pmobj_t);
procedure A_AttackSound1(actor: Pmobj_t);

procedure A_MeleeSound(actor: Pmobj_t; origin: Pmobj_t);
procedure A_MeleeSound1(actor: Pmobj_t);

procedure A_DeathSound(actor: Pmobj_t; origin: Pmobj_t);
procedure A_DeathSound1(actor: Pmobj_t);

procedure A_ActiveSound(actor: Pmobj_t; origin: Pmobj_t);
procedure A_ActiveSound1(actor: Pmobj_t);

implementation

uses
  info_h,
  p_common,
  s_sound;

procedure A_SeeSound(actor: Pmobj_t; origin: Pmobj_t);
begin
  if actor.info.seesound = 0 then
    exit;

  if actor.info.flags_ex and MF_EX_RANDOMSEESOUND <> 0 then
    P_RandomSound(origin, actor.info.seesound)
  else
    S_StartSound(origin, actor.info.seesound);
end;

procedure A_SeeSound1(actor: Pmobj_t);
begin
  if (actor._type = Ord(MT_SPIDER)) or (actor._type = Ord(MT_CYBORG)) or (actor.info.flags_ex and MF_EX_BOSS <> 0) then
    A_SeeSound(actor, nil)
  else
    A_SeeSound(actor, actor);
end;

procedure A_PainSound(actor: Pmobj_t; origin: Pmobj_t);
begin
  if actor.info.painsound = 0 then
    exit;

  if actor.info.flags_ex and MF_EX_RANDOMPAINSOUND <> 0 then
    P_RandomSound(origin, actor.info.painsound)
  else
    S_StartSound(origin, actor.info.painsound);
end;

procedure A_PainSound1(actor: Pmobj_t);
begin
  if (actor._type = Ord(MT_SPIDER)) or
     (actor._type = Ord(MT_CYBORG)) or
     (actor.info.flags_ex and MF_EX_BOSS <> 0) or
     (actor.info.flags2_ex and MF2_EX_FULLVOLPAIN <> 0) then
    A_PainSound(actor, nil)
  else
    A_PainSound(actor, actor);
end;

procedure A_AttackSound(actor: Pmobj_t; origin: Pmobj_t);
begin
  if actor.info.attacksound = 0 then
    exit;

  if actor.info.flags_ex and MF_EX_RANDOMATTACKSOUND <> 0 then
    P_RandomSound(origin, actor.info.attacksound)
  else
    S_StartSound(origin, actor.info.attacksound);
end;

procedure A_AttackSound1(actor: Pmobj_t);
begin
  if (actor._type = Ord(MT_SPIDER)) or
     (actor._type = Ord(MT_CYBORG)) or
     (actor.info.flags_ex and MF_EX_BOSS <> 0) or
     (actor.info.flags2_ex and MF2_EX_FULLVOLATTACK <> 0) then
    A_AttackSound(actor, nil)
  else
    A_AttackSound(actor, actor);
end;

procedure A_MeleeSound(actor: Pmobj_t; origin: Pmobj_t);
begin
  if actor.info.meleesound = 0 then
    exit;

  if actor.info.flags_ex and MF_EX_RANDOMMELEESOUND <> 0 then
    P_RandomSound(origin, actor.info.meleesound)
  else
    S_StartSound(origin, actor.info.meleesound);
end;

procedure A_MeleeSound1(actor: Pmobj_t);
begin
  if (actor._type = Ord(MT_SPIDER)) or
     (actor._type = Ord(MT_CYBORG)) or
     (actor.info.flags_ex and MF_EX_BOSS <> 0) or
     (actor.info.flags2_ex and MF2_EX_FULLVOLATTACK <> 0) then
    A_MeleeSound(actor, nil)
  else
    A_MeleeSound(actor, actor);
end;

procedure A_DeathSound(actor: Pmobj_t; origin: Pmobj_t);
begin
  if actor.info.deathsound = 0 then
    exit;

  if actor.info.flags_ex and MF_EX_RANDOMDEATHSOUND <> 0 then
    P_RandomSound(origin, actor.info.deathsound)
  else
    S_StartSound(origin, actor.info.deathsound);
end;

procedure A_DeathSound1(actor: Pmobj_t);
begin
  if (actor._type = Ord(MT_SPIDER)) or
     (actor._type = Ord(MT_CYBORG)) or
     (actor.info.flags_ex and MF_EX_BOSS <> 0) or
     (actor.info.flags2_ex and MF2_EX_FULLVOLDEATH <> 0) then
    A_DeathSound(actor, nil)
  else
    A_DeathSound(actor, actor);
end;

procedure A_ActiveSound(actor: Pmobj_t; origin: Pmobj_t);
begin
  if actor.info.activesound = 0 then
    exit;

  if actor.info.flags_ex and MF_EX_RANDOMACTIVESOUND <> 0 then
    P_RandomSound(origin, actor.info.activesound)
  else
    S_StartSound(origin, actor.info.activesound);
end;

procedure A_ActiveSound1(actor: Pmobj_t);
begin
  if (actor._type = Ord(MT_SPIDER)) or
     (actor._type = Ord(MT_CYBORG)) or
     (actor.info.flags_ex and MF_EX_BOSS <> 0) or
     (actor.info.flags2_ex and MF2_EX_FULLVOLACTIVE <> 0) then
    A_ActiveSound(actor, nil)
  else
    A_ActiveSound(actor, actor);
end;

end.

