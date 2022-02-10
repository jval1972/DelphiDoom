//------------------------------------------------------------------------------
//
//  DelphiHeretic is a source port of the game Heretic and it is
//  based on original Linux Doom as published by "id Software", on
//  Heretic source as published by "Raven" software and DelphiDoom
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

unit p_pspr_h;

interface

uses
  doomdef,
  m_fixed,
  info_h;

type
//
// Overlay psprites are scaled shapes
// drawn directly on the view screen,
// coordinates are given for a 320*200 view screen.
//
  psprnum_t = (
    ps_weapon,
    ps_flash,
    NUMPSPRITES
  );

  pspdef_t = record
    state: Pstate_t; // a NULL state means not active
    tics: integer;
    sx: fixed_t;
    sy: fixed_t;
  end;
  Ppspdef_t = ^pspdef_t;

var
  wpnlev1info: array[0..Ord(NUMWEAPONS) - 1] of weaponinfo_t = (
  // Staff
    (ammo: am_noammo;
     upstate: Ord(S_STAFFUP);
     downstate: Ord(S_STAFFDOWN);
     readystate: Ord(S_STAFFREADY);
     atkstate: Ord(S_STAFFATK1_1);
     holdatkstate: Ord(S_STAFFATK1_1);
     flashstate: Ord(S_NULL)),
  // Gold wand
    (ammo: am_goldwand;
     upstate: Ord(S_GOLDWANDUP);
     downstate: Ord(S_GOLDWANDDOWN);
     readystate: Ord(S_GOLDWANDREADY);
     atkstate: Ord(S_GOLDWANDATK1_1);
     holdatkstate: Ord(S_GOLDWANDATK1_1);
     flashstate: Ord(S_NULL);
    ),
  // Crossbow
    (ammo: am_crossbow;
     upstate: Ord(S_CRBOWUP);
     downstate: Ord(S_CRBOWDOWN);
     readystate: Ord(S_CRBOW1);
     atkstate: Ord(S_CRBOWATK1_1);
     holdatkstate: Ord(S_CRBOWATK1_1);
     flashstate: Ord(S_NULL);
    ),
  // Blaster
    (
     ammo: am_blaster;
     upstate: Ord(S_BLASTERUP);
     downstate: Ord(S_BLASTERDOWN);
     readystate: Ord(S_BLASTERREADY);
     atkstate: Ord(S_BLASTERATK1_1);
     holdatkstate: Ord(S_BLASTERATK1_3);
     flashstate: Ord(S_NULL);
    ),
  // Skull rod
    (ammo: am_skullrod;
     upstate: Ord(S_HORNRODUP);
     downstate: Ord(S_HORNRODDOWN);
     readystate: Ord(S_HORNRODREADY);
     atkstate: Ord(S_HORNRODATK1_1);
     holdatkstate: Ord(S_HORNRODATK1_1);
     flashstate: Ord(S_NULL);
    ),
  // Phoenix rod
     (
      ammo: am_phoenixrod;
      upstate: Ord(S_PHOENIXUP);
      downstate: Ord(S_PHOENIXDOWN);
      readystate: Ord(S_PHOENIXREADY);
      atkstate: Ord(S_PHOENIXATK1_1);
      holdatkstate: Ord(S_PHOENIXATK1_1);
      flashstate: Ord(S_NULL);
     ),
  // Mace
     (
      ammo: am_mace;
      upstate: Ord(S_MACEUP);
      downstate: Ord(S_MACEDOWN);
      readystate: Ord(S_MACEREADY);
      atkstate: Ord(S_MACEATK1_1);
      holdatkstate: Ord(S_MACEATK1_2);
      flashstate: Ord(S_NULL);
     ),
  // Gauntlets
     (
      ammo: am_noammo;
      upstate: Ord(S_GAUNTLETUP);
      downstate: Ord(S_GAUNTLETDOWN);
      readystate: Ord(S_GAUNTLETREADY);
      atkstate: Ord(S_GAUNTLETATK1_1);
      holdatkstate: Ord(S_GAUNTLETATK1_3);
      flashstate: Ord(S_NULL);
     ),
  // Beak
     (
      ammo: am_noammo;
      upstate: Ord(S_BEAKUP);
      downstate: Ord(S_BEAKDOWN);
      readystate: Ord(S_BEAKREADY);
      atkstate: Ord(S_BEAKATK1_1);
      holdatkstate: Ord(S_BEAKATK1_1);
      flashstate: Ord(S_NULL);
     )
  );

  wpnlev2info: array[0..Ord(NUMWEAPONS) - 1] of weaponinfo_t = (
  // Staff
    (ammo: am_noammo;
     upstate: Ord(S_STAFFUP2);
     downstate: Ord(S_STAFFDOWN2);
     readystate: Ord(S_STAFFREADY2_1);
     atkstate: Ord(S_STAFFATK2_1);
     holdatkstate: Ord(S_STAFFATK2_1);
     flashstate: Ord(S_NULL)),
  // Gold wand
    (ammo: am_goldwand;
     upstate: Ord(S_GOLDWANDUP);
     downstate: Ord(S_GOLDWANDDOWN);
     readystate: Ord(S_GOLDWANDREADY);
     atkstate: Ord(S_GOLDWANDATK2_1);
     holdatkstate: Ord(S_GOLDWANDATK2_1);
     flashstate: Ord(S_NULL);
    ),
  // Crossbow
    (ammo: am_crossbow;
     upstate: Ord(S_CRBOWUP);
     downstate: Ord(S_CRBOWDOWN);
     readystate: Ord(S_CRBOW1);
     atkstate: Ord(S_CRBOWATK2_1);
     holdatkstate: Ord(S_CRBOWATK2_1);
     flashstate: Ord(S_NULL);
    ),
  // Blaster
    (
     ammo: am_blaster;
     upstate: Ord(S_BLASTERUP);
     downstate: Ord(S_BLASTERDOWN);
     readystate: Ord(S_BLASTERREADY);
     atkstate: Ord(S_BLASTERATK2_1);
     holdatkstate: Ord(S_BLASTERATK2_3);
     flashstate: Ord(S_NULL);
    ),
  // Skull rod
    (ammo: am_skullrod;
     upstate: Ord(S_HORNRODUP);
     downstate: Ord(S_HORNRODDOWN);
     readystate: Ord(S_HORNRODREADY);
     atkstate: Ord(S_HORNRODATK2_1);
     holdatkstate: Ord(S_HORNRODATK2_1);
     flashstate: Ord(S_NULL);
    ),
  // Phoenix rod
     (
      ammo: am_phoenixrod;
      upstate: Ord(S_PHOENIXUP);
      downstate: Ord(S_PHOENIXDOWN);
      readystate: Ord(S_PHOENIXREADY);
      atkstate: Ord(S_PHOENIXATK2_1);
      holdatkstate: Ord(S_PHOENIXATK2_1);
      flashstate: Ord(S_NULL);
     ),
  // Mace
     (
      ammo: am_mace;
      upstate: Ord(S_MACEUP);
      downstate: Ord(S_MACEDOWN);
      readystate: Ord(S_MACEREADY);
      atkstate: Ord(S_MACEATK2_1);
      holdatkstate: Ord(S_MACEATK2_1);
      flashstate: Ord(S_NULL);
     ),
  // Gauntlets
     (
      ammo: am_noammo;
      upstate: Ord(S_GAUNTLETUP2);
      downstate: Ord(S_GAUNTLETDOWN2);
      readystate: Ord(S_GAUNTLETREADY2_1);
      atkstate: Ord(S_GAUNTLETATK2_1);
      holdatkstate: Ord(S_GAUNTLETATK2_3);
      flashstate: Ord(S_NULL);
     ),
  // Beak
     (
      ammo: am_noammo;
      upstate: Ord(S_BEAKUP);
      downstate: Ord(S_BEAKDOWN);
      readystate: Ord(S_BEAKREADY);
      atkstate: Ord(S_BEAKATK2_1);
      holdatkstate: Ord(S_BEAKATK2_1);
      flashstate: Ord(S_NULL);
     )
  );

implementation

end.

