//------------------------------------------------------------------------------
//
//  DelphiDoom: A modified and improved DOOM engine for Windows
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 1993-1996 by id Software, Inc.
//  Copyright (C) 2004-2020 by Jim Valavanis
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

unit rtl_types;

interface

uses
  d_delphi;

const
  RTL_ST_SPAWN = 1;
  RTL_ST_SEE = 2;
  RTL_ST_MELEE = 4;
  RTL_ST_MISSILE = 8;
  RTL_ST_PAIN = 16;
  RTL_ST_DEATH = 32;
  RTL_ST_XDEATH = 64;
  RTL_ST_RAISE = 128;
  RTL_ST_HEAL = 256;
  RTL_ST_CRASH = 512;
  {$IFDEF DOOM_OR_STRIFE}
  RTL_ST_INTERACT = 1024;
  {$ENDIF}


type
  rtl_state_t = record
    sprite: string;
    frame: integer;
    tics: integer;
    tics2: integer;
    action: string;
    nextstate: integer;
    misc1: integer;
    misc2: integer;
    flags_ex: integer;
    bright: boolean;
    has_goto: boolean;
    gotostr_needs_calc: boolean;
    gotostr_calc: string;
    alias: string;
    savealias: string;
  end;
  Prtl_state_t = ^rtl_state_t;

type
  rtl_mobjinfo_t = record
    name: string;
    {$IFDEF STRIFE}
    name2: string;
    {$ENDIF}
    inheritsfrom: string;
    doomednum: integer;
    spawnstate: integer;
    spawnhealth: integer;
    seestate: integer;
    seesound: string;
    reactiontime: integer;
    attacksound: string;
    painstate: integer;
    painchance: integer;
    painsound: string;
    meleestate: integer;
    missilestate: integer;
    deathstate: integer;
    xdeathstate: integer;
    deathsound: string;
    speed: integer;
    radius: integer;
    height: integer;
    mass: integer;
    damage: integer;
    activesound: string;
    flags: string;
    {$IFDEF HERETIC_OR_HEXEN}
    flags2: string;
    {$ENDIF}
    flags_ex: string;
    flags2_ex: string;
    flags3_ex: string;
    flags4_ex: string;
    raisestate: integer;
    customsound1: string;
    customsound2: string;
    customsound3: string;
    dropitem: string;
    missiletype: string;
    explosiondamage: integer;
    explosionradius: integer;
    meleedamage: integer;
    meleesound: string;
    renderstyle: string;
    alpha: integer;
    healstate: integer;
    crashstate: integer;
    {$IFDEF DOOM_OR_STRIFE}
    interactstate: integer;
    missileheight: integer;
    {$ENDIF}
    vspeed: float;
    pushfactor: float;
    statesdefined: LongWord;
    replacesid: integer;
    scale: float;
    gravity: float;
  end;
  Prtl_mobjinfo_t = ^rtl_mobjinfo_t;


implementation

end.
