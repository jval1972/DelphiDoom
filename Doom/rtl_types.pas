//------------------------------------------------------------------------------
//
//  DelphiDoom: A modified and improved DOOM engine for Windows
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 2004-2008 by Jim Valavanis
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
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : http://delphidoom.sitesled.com/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit rtl_types;

interface

type
  rtl_state_t = record
    sprite: string;
    frame: integer;
    tics: integer;
    action: string;
    nextstate: integer;
    misc1: integer;
    misc2: integer;
    flags_ex: integer;
    bright: boolean;
  end;
  Prtl_state_t = ^rtl_state_t;

type
  rtl_mobjinfo_t = record
    name: string;
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
    flags_ex: string;
    flags2_ex: string;
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
  end;
  Prtl_mobjinfo_t = ^rtl_mobjinfo_t;


implementation

end.
