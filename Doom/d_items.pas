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
    ammopershot: integer; // MBF21
    intflags: integer; // MBF21
    mbf21bits: integer; // MBF21
  end;
  Pweaponinfo_t = ^weaponinfo_t;

//
// PSPRITE ACTIONS for weapons.
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
  // fist
    (ammo: am_noammo;            upstate: Ord(S_PUNCHUP);   downstate: Ord(S_PUNCHDOWN);
     readystate: Ord(S_PUNCH);   atkstate: Ord(S_PUNCH1);   holdatkstate: Ord(S_NULL);
     flashstate: Ord(S_NULL);
     ammopershot: 1; // MBF21
     intflags: 0; // MBF21
     mbf21bits: WPF_FLEEMELEE or WPF_AUTOSWITCHFROM or WPF_NOAUTOSWITCHTO; // MBF21
    ),
  // pistol
    (ammo: am_clip;              upstate: Ord(S_PISTOLUP);  downstate: Ord(S_PISTOLDOWN);
     readystate: Ord(S_PISTOL);  atkstate: Ord(S_PISTOL1);  holdatkstate: Ord(S_NULL);
     flashstate: Ord(S_PISTOLFLASH);
     ammopershot: 1; // MBF21
     intflags: 0; // MBF21
     mbf21bits: WPF_AUTOSWITCHFROM; // MBF21
    ),
  // shotgun
    (ammo: am_shell;             upstate: Ord(S_SGUNUP);    downstate: Ord(S_SGUNDOWN);
     readystate: Ord(S_SGUN);    atkstate: Ord(S_SGUN1);    holdatkstate: Ord(S_NULL);
     flashstate: Ord(S_SGUNFLASH1);
     ammopershot: 1; // MBF21
     intflags: 0; // MBF21
     mbf21bits: WPF_NOFLAG; // MBF21
    ),
  // chaingun
    (ammo: am_clip;              upstate: Ord(S_CHAINUP);   downstate: Ord(S_CHAINDOWN);
     readystate: Ord(S_CHAIN);   atkstate: Ord(S_CHAIN1);   holdatkstate: Ord(S_NULL);
     flashstate: Ord(S_CHAINFLASH1);
     ammopershot: 1; // MBF21
     intflags: 0; // MBF21
     mbf21bits: WPF_NOFLAG; // MBF21
    ),
  // missile launcher
    (ammo: am_misl;              upstate: Ord(S_MISSILEUP); downstate: Ord(S_MISSILEDOWN);
     readystate: Ord(S_MISSILE); atkstate: Ord(S_MISSILE1); holdatkstate: Ord(S_NULL);
     flashstate: Ord(S_MISSILEFLASH1);
     ammopershot: 1; // MBF21
     intflags: 0; // MBF21
     mbf21bits: WPF_NOAUTOFIRE; // MBF21
    ),
  // plasma rifle
     (ammo: am_cell;             upstate: Ord(S_PLASMAUP);  downstate: Ord(S_PLASMADOWN);
      readystate: Ord(S_PLASMA); atkstate: Ord(S_PLASMA1);  holdatkstate: Ord(S_NULL);
      flashstate: Ord(S_PLASMAFLASH1);
      ammopershot: 1; // MBF21
      intflags: 0; // MBF21
      mbf21bits: WPF_NOFLAG; // MBF21
     ),
  // bfg 9000
     (ammo: am_cell;             upstate: Ord(S_BFGUP);     downstate: Ord(S_BFGDOWN);
      readystate: Ord(S_BFG);    atkstate: Ord(S_BFG1);     holdatkstate: Ord(S_NULL);
      flashstate: Ord(S_BFGFLASH1);
      ammopershot: 40; // MBF21
      intflags: 0; // MBF21
      mbf21bits: WPF_NOAUTOFIRE; // MBF21
     ),
  // chainsaw
     (ammo: am_noammo;           upstate: Ord(S_SAWUP);     downstate: Ord(S_SAWDOWN);
      readystate: Ord(S_SAW);    atkstate: Ord(S_SAW1);     holdatkstate: Ord(S_NULL);
      flashstate: Ord(S_NULL);
      ammopershot: 1; // MBF21
      intflags: 0; // MBF21
      mbf21bits: WPF_NOTHRUST or WPF_FLEEMELEE or WPF_NOAUTOSWITCHTO; // MBF21
     ),
  // super shotgun
     (ammo: am_shell;            upstate: Ord(S_DSGUNUP);   downstate: Ord(S_DSGUNDOWN);
      readystate: Ord(S_DSGUN);  atkstate: Ord(S_DSGUN1);   holdatkstate: Ord(S_NULL);
      flashstate: Ord(S_DSGUNFLASH1);
      ammopershot: 2; // MBF21
      intflags: 0; // MBF21
      mbf21bits: WPF_NOFLAG; // MBF21
     )
  );

implementation

end.

