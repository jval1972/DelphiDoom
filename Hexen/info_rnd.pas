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

unit info_rnd;

// JVAL: Random items

interface

uses
  info_h;

//==============================================================================
//
// Info_SelectRandomMonster
//
//==============================================================================
function Info_SelectRandomMonster(_type: integer): integer;

//==============================================================================
//
// Info_InitRandom
//
//==============================================================================
procedure Info_InitRandom;

//==============================================================================
//
// Info_ShutDownRandom
//
//==============================================================================
procedure Info_ShutDownRandom;

//==============================================================================
//
// Info_IsMonster
//
//==============================================================================
function Info_IsMonster(_type: integer): boolean;

implementation

uses
  d_delphi,
  doomdef,
  info,
  m_rnd,
  p_setup,
  p_mobj,
  p_mobj_h;

type
  randompool_t = record
    check: integer;
    list: TDNumberList;
  end;

const
  NUMMONSTERSCATEGORIES = 5;

var
  rnd_monsters: array[0..NUMMONSTERSCATEGORIES - 1] of randompool_t;
  rnd_monstersinitialized: boolean = false;

//==============================================================================
//
// Info_InitRandomMonsters
//
//==============================================================================
procedure Info_InitRandomMonsters;
var
  i: integer;
  idx: integer;
  check: integer;
begin
  if rnd_monstersinitialized then
    exit;

{  rnd_monsters[0].check := 290;
  rnd_monsters[1].check := 360;
  rnd_monsters[2].check := 400;
  rnd_monsters[3].check := 2000;}
  rnd_monsters[0].check := 1000000;
  rnd_monsters[1].check := 1000000;
  rnd_monsters[2].check := 1000000;
  rnd_monsters[3].check := 1000000;
  rnd_monsters[4].check := MAXINT;

  for i := 0 to NUMMONSTERSCATEGORIES - 1 do
    rnd_monsters[i].list := TDNumberList.Create;

  rnd_monstersinitialized := true;

  for i := 0 to nummobjtypes - 1 do
    if Info_IsMonster(i) and P_GameValidThing(mobjinfo[i].doomednum) then
    begin
      check := mobjinfo[i].spawnhealth;
      idx := 0;
      while (idx < NUMMONSTERSCATEGORIES) and (check >= rnd_monsters[idx].check) do
        inc(idx);
      rnd_monsters[idx].list.Add(i);
    end;
end;

//==============================================================================
//
// Info_ShutDownRandomMonsters
//
//==============================================================================
procedure Info_ShutDownRandomMonsters;
var
  i: integer;
begin
  if not rnd_monstersinitialized then
    exit;

  for i := 0 to NUMMONSTERSCATEGORIES - 1 do
    FreeAndNil(rnd_monsters[i].list);

  rnd_monstersinitialized := false;
end;

//==============================================================================
//
// Info_SelectRandomMonster
//
//==============================================================================
function Info_SelectRandomMonster(_type: integer): integer;
var
  idx: integer;
  check: integer;
begin
  check := mobjinfo[_type].spawnhealth;
  idx := 0;
  while (idx < NUMMONSTERSCATEGORIES) and (check >= rnd_monsters[idx].check) do
    inc(idx);

  result := rnd_monsters[idx].list[N_Random mod rnd_monsters[idx].list.Count];
end;

//==============================================================================
//
// Info_InitRandom
//
//==============================================================================
procedure Info_InitRandom;
begin
  Info_InitRandomMonsters
end;

//==============================================================================
//
// Info_ShutDownRandom
//
//==============================================================================
procedure Info_ShutDownRandom;
begin
  Info_ShutDownRandomMonsters
end;

//==============================================================================
//
// Info_IsMonster
//
//==============================================================================
function Info_IsMonster(_type: integer): boolean;
begin
  result := (mobjinfo[_type].doomednum > MAXPLAYERS) and      // Not player
            (mobjinfo[_type].doomednum <> 9100) and           // Not player
            (mobjinfo[_type].doomednum <> 9101) and           // Not player
            (mobjinfo[_type].doomednum <> 9102) and           // Not player
            (mobjinfo[_type].doomednum <> 9103) and           // Not player
            (mobjinfo[_type].flags and MF_SHOOTABLE <> 0) and // Shootable
            (mobjinfo[_type].flags and MF_COUNTKILL <> 0);    // Count kill or can attack
end;

end.
