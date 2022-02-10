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
  weaponinfo: array[0..Ord(NUMWEAPONS) - 1, 0..Ord(NUMCLASSES) - 1] of weaponinfo_t = (
    ( // First Weapons
      ( // Fighter First Weapon - Punch
        mana: MANA_NONE;                      // mana
        upstate: Ord(S_PUNCHUP);              // upstate
        downstate: Ord(S_PUNCHDOWN);          // downstate
        readystate: Ord(S_PUNCHREADY);        // readystate
        atkstate: Ord(S_PUNCHATK1_1);         // atkstate
        holdatkstate: Ord(S_PUNCHATK1_1);     // holdatkstate
        flashstate: Ord(S_NULL);              // flashstate
      ),
      ( // Cleric First Weapon - Mace
        mana: MANA_NONE;                      // mana
        upstate: Ord(S_CMACEUP);              // upstate
        downstate: Ord(S_CMACEDOWN);          // downstate
        readystate: Ord(S_CMACEREADY);        // readystate
        atkstate: Ord(S_CMACEATK_1);          // atkstate
        holdatkstate: Ord(S_CMACEATK_1);      // holdatkstate
        flashstate: Ord(S_NULL);              // flashstate
      ),
      ( // Mage First Weapon - Wand
        mana: MANA_NONE;                      // mana
        upstate: Ord(S_MWANDUP);              // upstate
        downstate: Ord(S_MWANDDOWN);          // downstate
        readystate: Ord(S_MWANDREADY);        // readystate
        atkstate: Ord(S_MWANDATK_1);          // atkstate
        holdatkstate: Ord(S_MWANDATK_1);      // holdatkstate
        flashstate: Ord(S_NULL);              // flashstate
      ),
      ( // Pig - Snout
        mana: MANA_NONE;                      // mana
        upstate: Ord(S_SNOUTUP);              // upstate
        downstate: Ord(S_SNOUTDOWN);          // downstate
        readystate: Ord(S_SNOUTREADY);        // readystate
        atkstate: Ord(S_SNOUTATK1);           // atkstate
        holdatkstate: Ord(S_SNOUTATK1);       // holdatkstate
        flashstate: Ord(S_NULL);              // flashstate
      )
     ),

    ( // Second Weapons
      ( // Fighter - Axe
        mana: MANA_NONE;                      // mana
        upstate: Ord(S_FAXEUP);               // upstate
        downstate: Ord(S_FAXEDOWN);           // downstate
        readystate: Ord(S_FAXEREADY);         // readystate
        atkstate: Ord(S_FAXEATK_1);           // atkstate
        holdatkstate: Ord(S_FAXEATK_1);       // holdatkstate
        flashstate: Ord(S_NULL);              // flashstate
      ),
      ( // Cleric - Serpent Staff
        mana: MANA_1;      // mana
        upstate: Ord(S_CSTAFFUP);             // upstate
        downstate: Ord(S_CSTAFFDOWN);         // downstate
        readystate: Ord(S_CSTAFFREADY);       // readystate
        atkstate: Ord(S_CSTAFFATK_1);         // atkstate
        holdatkstate: Ord(S_CSTAFFATK_1);     // holdatkstate
        flashstate: Ord(S_NULL);              // flashstate
      ),
      ( // Mage - Cone of shards
        mana: MANA_1;                         // mana
        upstate: Ord(S_CONEUP);               // upstate
        downstate: Ord(S_CONEDOWN);           // downstate
        readystate: Ord(S_CONEREADY);         // readystate
        atkstate: Ord(S_CONEATK1_1);          // atkstate
        holdatkstate: Ord(S_CONEATK1_3);      // holdatkstate
        flashstate: Ord(S_NULL);              // flashstate
      ),
      ( // Pig - Snout
        mana: MANA_NONE;                      // mana
        upstate: Ord(S_SNOUTUP);              // upstate
        downstate: Ord(S_SNOUTDOWN);          // downstate
        readystate: Ord(S_SNOUTREADY);        // readystate
        atkstate: Ord(S_SNOUTATK1);           // atkstate
        holdatkstate: Ord(S_SNOUTATK1);       // holdatkstate
        flashstate: Ord(S_NULL);              // flashstate
      )
    ),

    ( // Third Weapons
      ( // Fighter - Hammer
        mana: MANA_NONE;                      // mana
        upstate: Ord(S_FHAMMERUP);            // upstate
        downstate: Ord(S_FHAMMERDOWN);        // downstate
        readystate: Ord(S_FHAMMERREADY);      // readystate
        atkstate: Ord(S_FHAMMERATK_1);        // atkstate
        holdatkstate: Ord(S_FHAMMERATK_1);    // holdatkstate
        flashstate: Ord(S_NULL);              // flashstate
      ),
      ( // Cleric - Flame Strike
        mana: MANA_2;                         // mana
        upstate: Ord(S_CFLAMEUP);             // upstate
        downstate: Ord(S_CFLAMEDOWN);         // downstate
        readystate: Ord(S_CFLAMEREADY1);      // readystate
        atkstate: Ord(S_CFLAMEATK_1);         // atkstate
        holdatkstate: Ord(S_CFLAMEATK_1);     // holdatkstate
        flashstate: Ord(S_NULL);              // flashstate
      ),
      ( // Mage - Lightning
        mana: MANA_2;                         // mana
        upstate: Ord(S_MLIGHTNINGUP);         // upstate
        downstate: Ord(S_MLIGHTNINGDOWN);     // downstate
        readystate: Ord(S_MLIGHTNINGREADY);   // readystate
        atkstate: Ord(S_MLIGHTNINGATK_1);     // atkstate
        holdatkstate: Ord(S_MLIGHTNINGATK_1); // holdatkstate
        flashstate: Ord(S_NULL);              // flashstate
      ),
      ( // Pig - Snout
        mana: MANA_NONE;                      // mana
        upstate: Ord(S_SNOUTUP);              // upstate
        downstate: Ord(S_SNOUTDOWN);          // downstate
        readystate: Ord(S_SNOUTREADY);        // readystate
        atkstate: Ord(S_SNOUTATK1);           // atkstate
        holdatkstate: Ord(S_SNOUTATK1);       // holdatkstate
        flashstate: Ord(S_NULL);              // flashstate
      )
    ),

    ( // Fourth Weapons
      ( // Fighter - Rune Sword
        mana: MANA_BOTH;                      // mana
        upstate: Ord(S_FSWORDUP);             // upstate
        downstate: Ord(S_FSWORDDOWN);         // downstate
        readystate: Ord(S_FSWORDREADY);       // readystate
        atkstate: Ord(S_FSWORDATK_1);         // atkstate
        holdatkstate: Ord(S_FSWORDATK_1);     // holdatkstate
        flashstate: Ord(S_NULL);              // flashstate
      ),
      ( // Cleric - Holy Symbol
        mana: MANA_BOTH;                      // mana
        upstate: Ord(S_CHOLYUP);              // upstate
        downstate: Ord(S_CHOLYDOWN);          // downstate
        readystate: Ord(S_CHOLYREADY);        // readystate
        atkstate: Ord(S_CHOLYATK_1);          // atkstate
        holdatkstate: Ord(S_CHOLYATK_1);      // holdatkstate
        flashstate: Ord(S_NULL);              // flashstate
      ),
      ( // Mage - Staff
        mana: MANA_BOTH;                      // mana
        upstate: Ord(S_MSTAFFUP);             // upstate
        downstate: Ord(S_MSTAFFDOWN);         // downstate
        readystate: Ord(S_MSTAFFREADY);       // readystate
        atkstate: Ord(S_MSTAFFATK_1);         // atkstate
        holdatkstate: Ord(S_MSTAFFATK_1);     // holdatkstate
        flashstate: Ord(S_NULL);              // flashstate
      ),
      ( // Pig - Snout
        mana: MANA_NONE;                      // mana
        upstate: Ord(S_SNOUTUP);              // upstate
        downstate: Ord(S_SNOUTDOWN);          // downstate
        readystate: Ord(S_SNOUTREADY);        // readystate
        atkstate: Ord(S_SNOUTATK1);           // atkstate
        holdatkstate: Ord(S_SNOUTATK1);       // holdatkstate
        flashstate: Ord(S_NULL);              // flashstate
      )
    )
  );

implementation

end.
