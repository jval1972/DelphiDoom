//------------------------------------------------------------------------------
//
//  DelphiStrife is a source port of the game Strife.
//
//  Based on:
//    - Linux Doom by "id Software"
//    - Chocolate Strife by "Simon Howard"
//    - DelphiDoom by "Jim Valavanis"
//
//  Copyright (C) 1993-1996 by id Software, Inc.
//  Copyright (C) 2005 Simon Howard
//  Copyright (C) 2010 James Haley, Samuel Villarreal
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
// DESCRIPTION:
//  Items: key cards, artifacts, weapon, ammunition.
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit d_items;

interface

uses
  doomdef,
  info_h;

//
// mbf21: Internal weapon flags
//
const
  WIF_ENABLEAPS = 1;  // [XA] enable "ammo per shot" field for native Doom weapon codepointers

const
  // no flag
  WPF_NOFLAG = 0;
  // doesn't thrust Mobj's
  WPF_NOTHRUST = 1;
  // weapon is silent
  WPF_SILENT = 2;
  // weapon won't autofire in A_WeaponReady
  WPF_NOAUTOFIRE = 4;
  // monsters consider it a melee weapon
  WPF_FLEEMELEE = 8;
  // can be switched away from when ammo is picked up
  WPF_AUTOSWITCHFROM = $10;
  // cannot be switched to when ammo is picked up
  WPF_NOAUTOSWITCHTO = $20;

type
  { Weapon info: sprite frames, ammunition use. }
  weaponinfo_t = record
    ammo: ammotype_t;
    upstate: integer;
    downstate: integer;
    readystate: integer;
    atkstate: integer;
    holdatkstate: integer;
    flashstate: integer;
    availabledemo: boolean;    // villsa [STRIFE]
    ammopershot: integer; // MBF21
    intflags: integer; // MBF21
    mbf21bits: integer; // MBF21
  end;
  Pweaponinfo_t = ^weaponinfo_t;

//
// PSPRITE ACTIONS for waepons.
// This struct controls the weapon animations.
//
// Each entry is:
//   ammo/amunition type
//   upstate
//   downstate
//   readystate
//   atkstate, i.e. attack/fire/hit frame
//   flashstate, muzzle flash
//

var
  weaponinfo: array[0..Ord(NUMWEAPONS) - 1] of weaponinfo_t = (
  (
    // fist
    ammo: am_noammo;
    upstate: Ord(S_PNCH_03);
    downstate: Ord(S_PNCH_02);
    readystate: Ord(S_PNCH_01);
    atkstate: Ord(S_PNCH_04);
    holdatkstate: Ord(S_NULL);
    flashstate: Ord(S_NULL);
    availabledemo: true;
    ammopershot: 0;
    intflags: 0;
    mbf21bits: WPF_NOFLAG;
  ),
  (
    // electric bow
    ammo: am_elecbolts;
    upstate: Ord(S_XBOW_02);
    downstate: Ord(S_XBOW_01);
    readystate: Ord(S_XBOW_00);
    atkstate: Ord(S_XBOW_03);
    holdatkstate: Ord(S_NULL);
    flashstate: Ord(S_NULL);
    availabledemo: true;
    ammopershot: 1;
    intflags: 0;
    mbf21bits: WPF_NOFLAG;
  ),
  (
    // rifle
    ammo: am_bullets;
    upstate: Ord(S_RIFG_02);
    downstate: Ord(S_RIFG_01);
    readystate: Ord(S_RIFG_00);
    atkstate: Ord(S_RIFF_00);
    holdatkstate: Ord(S_NULL);
    flashstate: Ord(S_NULL);
    availabledemo: true;
    ammopershot: 1;
    intflags: 0;
    mbf21bits: WPF_NOFLAG;
  ),
  (
    // missile launcher
    ammo: am_missiles;
    upstate: Ord(S_MMIS_02);
    downstate: Ord(S_MMIS_01);
    readystate: Ord(S_MMIS_00);
    atkstate: Ord(S_MMIS_03);
    holdatkstate: Ord(S_NULL);
    flashstate: Ord(S_NULL);
    availabledemo: false;
    ammopershot: 1;
    intflags: 0;
    mbf21bits: WPF_NOFLAG;
  ),
  (
    // grenade launcher
    ammo: am_hegrenades;
    upstate: Ord(S_GREN_02);
    downstate: Ord(S_GREN_01);
    readystate: Ord(S_GREN_00);
    atkstate: Ord(S_GREN_03);
    holdatkstate: Ord(S_NULL);
    flashstate: Ord(S_GREF_00);
    availabledemo: false;
    ammopershot: 1;
    intflags: 0;
    mbf21bits: WPF_NOFLAG;
  ),
  (
    // flame thrower
    ammo: am_cell;
    upstate: Ord(S_FLMT_03);
    downstate: Ord(S_FLMT_02);
    readystate: Ord(S_FLMT_00);
    atkstate: Ord(S_FLMF_00);
    holdatkstate: Ord(S_NULL);
    flashstate: Ord(S_NULL);
    availabledemo: true;
    ammopershot: 1;
    intflags: 0;
    mbf21bits: WPF_NOFLAG;
  ),
  (
    // mauler
    ammo: am_cell;
    upstate: Ord(S_BLST_05);
    downstate: Ord(S_BLST_04);
    readystate: Ord(S_BLST_00);
    atkstate: Ord(S_BLSF_00);
    holdatkstate: Ord(S_NULL);
    flashstate: Ord(S_NULL);
    availabledemo: false;
    ammopershot: 20;
    intflags: 0;
    mbf21bits: WPF_NOFLAG;
  ),
  (
    // sigil
    ammo: am_noammo;
    upstate: Ord(S_SIGH_06);
    downstate: Ord(S_SIGH_05);
    readystate: Ord(S_SIGH_00);
    atkstate: Ord(S_SIGH_07);
    holdatkstate: Ord(S_NULL);
    flashstate: Ord(S_SIGF_00);
    availabledemo: false;
    ammopershot: 0;
    intflags: 0;
    mbf21bits: WPF_NOFLAG;
  ),
  (
    // poison bow
    ammo: am_poisonbolts;
    upstate: Ord(S_XBOW_15);
    downstate: Ord(S_XBOW_14);
    readystate: Ord(S_XBOW_13);
    atkstate: Ord(S_XBOW_16);
    holdatkstate: Ord(S_NULL);
    flashstate: Ord(S_NULL);
    availabledemo: true;
    ammopershot: 1;
    intflags: 0;
    mbf21bits: WPF_NOFLAG;
  ),
  (
    // wp grenade launcher
    ammo: am_wpgrenades;
    upstate: Ord(S_GREN_10);
    downstate: Ord(S_GREN_09);
    readystate: Ord(S_GREN_08);
    atkstate: Ord(S_GREN_11);
    holdatkstate: Ord(S_NULL);
    flashstate: Ord(S_GREF_03);
    availabledemo: false;
    ammopershot: 1;
    intflags: 0;
    mbf21bits: WPF_NOFLAG;
  ),
  (
    // torpedo
    ammo: am_cell;
    upstate: Ord(S_BLST_18);
    downstate: Ord(S_BLST_17);
    readystate: Ord(S_BLST_13);
    atkstate: Ord(S_BLST_19);
    holdatkstate: Ord(S_NULL);
    flashstate: Ord(S_NULL);
    availabledemo: false;
    ammopershot: 30;
    intflags: 0;
    mbf21bits: WPF_NOFLAG;
  )
  );

implementation

end.

