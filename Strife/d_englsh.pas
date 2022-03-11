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
//  Printed strings for translation.
//  English language support (default).
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit d_englsh;

interface

//
//  Printed strings for translation
//

//
// D_Main.C
//

var
  D_DEVSTR: string =
    'Development mode ON.' + #13#10;
  D_CDROM: string =
    'CD-ROM Version: Accessing strife.cd' + #13#10;

//
//  M_Menu.C
//
  PRESSKEY: string =
    'press a key.';
  PRESSYN: string =
    'press y or n.';

  QUITMSG: string =
    'are you sure you want to' + #13#10 +
    'quit this great game?';

  LOADNET: string =
    'you can''t load while in a net game!' + #13#10;

  QLOADNET: string =
    'you can''t quickload during a netgame!' + #13#10;

  QSAVESPOT: string =
    'you haven''t picked a' + #13#10 + 'quicksave slot yet!' + #13#10;

  SAVEDEAD: string =
    'you ''re not playing a game' + #13#10;

  QSPROMPT: string =
    'quicksave over your game named' + #13#10 + #13#10 +
    '''%s''?' + #13#10;

  QLPROMPT: string =
    'do you want to quickload the game named' + #13#10 + #13#10 +
    '''%s''?' + #13#10;

  SNEWGAME: string =
    'you can''t start a new game' + #13#10 +
    'while in a network game.' + #13#10;

  SNIGHTMARE: string =
    'are you sure? this skill level' + #13#10 +
    'isn''t even remotely fair.';

  MSGOFF: string =
    'Messages OFF';
  MSGON: string =
    'Messages ON';

  NETEND: string =
    'you can''t end a netgame!' + #13#10;
  SENDGAME: string =
    'are you sure you want' + #13#10 + 'to end the game?' + #13#10;

  DOSY: string =
    '(press y to quit)';

var
  DETAILULTRA: string = 'Ultra detail';
  DETAILHI: string = 'High detail';
  DETAILNORM: string = 'Normal detail';
  DETAILMED: string = 'Medium detail';
  DETAILLOW: string = 'Low detail';
  DETAILLOWEST: string = 'Lowest detail';
  GAMMALVL0: string = 'Gamma correction OFF';
  GAMMALVL1: string = 'Gamma correction level 1';
  GAMMALVL2: string = 'Gamma correction level 2';
  GAMMALVL3: string = 'Gamma correction level 3';
  GAMMALVL4: string = 'Gamma correction level 4';
  EMPTYSTRING: string = 'empty slot';

//
//  P_inter.C
//
var
  GOTARMOR: string = 'Picked up the armor.';
  GOTMEGA: string = 'Picked up the MegaArmor!';
  GOTHTHBONUS: string = 'Picked up a health bonus.';
  GOTARMBONUS: string = 'Picked up an armor bonus.';
  GOTSTIM: string = 'Picked up a stimpack.';
  GOTMEDINEED: string = 'Picked up a medikit that you REALLY need!';
  GOTMEDIKIT: string = 'Picked up a medikit.';
  GOTSUPER: string = 'Supercharge!';

  GOTBLUECARD: string = 'Picked up a blue keycard.';
  GOTYELWCARD: string = 'Picked up a yellow keycard.';
  GOTREDCARD: string = 'Picked up a red keycard.';
  GOTBLUESKUL: string = 'Picked up a blue skull key.';
  GOTYELWSKUL: string = 'Picked up a yellow skull key.';
  GOTREDSKULL: string = 'Picked up a red skull key.';

  GOTINVUL: string = 'Invulnerability!';
  GOTBERSERK: string = 'Berserk!';
  GOTINVIS: string = 'Partial Invisibility';
  GOTSUIT: string = 'Radiation Shielding Suit';
  GOTMAP: string = 'Computer Area Map';
  GOTVISOR: string = 'Light Amplification Visor';
  GOTMSPHERE: string = 'MegaSphere!';

  GOTCLIP: string = 'Picked up a clip.';
  GOTCLIPBOX: string = 'Picked up a box of bullets.';
  GOTROCKET: string = 'Picked up a rocket.';
  GOTROCKBOX: string = 'Picked up a box of rockets.';
  GOTCELL: string = 'Picked up an energy cell.';
  GOTCELLBOX: string = 'Picked up an energy cell pack.';
  GOTSHELLS: string = 'Picked up 4 shotgun shells.';

  GOTSHELLBOX: string = 'Picked up a box of shotgun shells.';
  GOTBACKPACK: string = 'Picked up a backpack full of ammo!';

  GOTBFG9000: string = 'You got the BFG9000!  Oh, yes.';
  GOTCHAINGUN: string = 'You got the chaingun!';
  GOTCHAINSAW: string = 'A chainsaw!  Find some meat!';
  GOTLAUNCHER: string = 'You got the rocket launcher!';
  GOTPLASMA: string = 'You got the plasma gun!';
  GOTSHOTGUN: string = 'You got the shotgun!';
  GOTSHOTGUN2: string = 'You got the super shotgun!';

  MSGSECRETSECTOR: string = 'You found a secret area.';

//
// P_Doors.C
//
var
//jff 02/05/98 Create messages specific to card and skull keys
  PD_ANY: string = 'Any key will open this door';
  PD_ALL6: string = 'You need all keys to open this door';

//
// G_game.C
//
var
  GGSAVED: string = 'game saved.';

const
//
//  HU_stuff.C
//
  HUSTR_1 = 'AREA  1: sanctuary';
  HUSTR_2 = 'AREA  2: town';
  HUSTR_3 = 'AREA  3: front base';
  HUSTR_4 = 'AREA  4: power station';
  HUSTR_5 = 'AREA  5: prison';
  HUSTR_6 = 'AREA  6: sewers';
  HUSTR_7 = 'AREA  7: castle';
  HUSTR_8 = 'AREA  8: Audience Chamber';
  HUSTR_9 = 'AREA  9: Castle: Programmer''s Keep';

  HUSTR_10 = 'AREA 10: New Front Base';
  HUSTR_11 = 'AREA 11: Borderlands';
  HUSTR_12 = 'AREA 12: the temple of the oracle';
  HUSTR_13 = 'AREA 13: Catacombs';
  HUSTR_14 = 'AREA 14: mines';
  HUSTR_15 = 'AREA 15: Fortress: Administration';
  HUSTR_16 = 'AREA 16: Fortress: Bishop''s Tower';
  HUSTR_17 = 'AREA 17: Fortress: The Bailey';
  HUSTR_18 = 'AREA 18: Fortress: Stores';
  HUSTR_19 = 'AREA 19: Fortress: Security Complex';

  HUSTR_20 = 'AREA 20: Factory: Receiving';
  HUSTR_21 = 'AREA 21: Factory: Manufacturing';
  HUSTR_22 = 'AREA 22: Factory: Forge';
  HUSTR_23 = 'AREA 23: Order Commons';
  HUSTR_24 = 'AREA 24: Factory: Conversion Chapel';
  HUSTR_25 = 'AREA 25: Catacombs: Ruined Temple';
  HUSTR_26 = 'AREA 26: proving grounds';
  HUSTR_27 = 'AREA 27: The Lab';
  HUSTR_28 = 'AREA 28: Alien Ship';
  HUSTR_29 = 'AREA 29: Entity''s Lair';

  HUSTR_30 = 'AREA 30: Abandoned Front Base';
  HUSTR_31 = 'AREA 31: Training Facility';

  HUSTR_32 = 'AREA  1: Sanctuary';
  HUSTR_33 = 'AREA  2: Town';
  HUSTR_34 = 'AREA  3: Movement Base';

  HUSTR_CHATMACRO1 = 'Fucker!';
  HUSTR_CHATMACRO2 = '--SPLAT-- Instant wall art.';
  HUSTR_CHATMACRO3 = 'That had to hurt!';
  HUSTR_CHATMACRO4 = 'Smackings!';
  HUSTR_CHATMACRO5 = 'Gib-O-Matic baby.';
  HUSTR_CHATMACRO6 = 'Burn!  Yah! Yah!';
  HUSTR_CHATMACRO7 = 'Buh-Bye!';
  HUSTR_CHATMACRO8 = 'Sizzle chest!';
  HUSTR_CHATMACRO9 = 'That sucked!';
  HUSTR_CHATMACRO0 = 'Mommy?';

var
  HUSTR_PLAYER1: string = '1: ';
  HUSTR_PLAYER2: string = '2: ';
  HUSTR_PLAYER3: string = '3: ';
  HUSTR_PLAYER4: string = '4: ';
  HUSTR_PLAYER5: string = '5: ';
  HUSTR_PLAYER6: string = '6: ';
  HUSTR_PLAYER7: string = '7: ';
  HUSTR_PLAYER8: string = '8: ';

var
  HUSTR_TALKTOSELF1: string = 'You mumble to yourself';
  HUSTR_TALKTOSELF2: string = 'Who''s there?';
  HUSTR_TALKTOSELF3: string = 'You scare yourself';
  HUSTR_TALKTOSELF4: string = 'You start to rave';
  HUSTR_TALKTOSELF5: string = 'You''ve lost it...';

  HUSTR_MESSAGESENT: string = '[Message Sent]';
  HUSTR_MSGU: string = '[Message unsent]';

  HUSTR_KEYGREEN: string = 'g';
  HUSTR_KEYINDIGO: string = 'i';
  HUSTR_KEYBROWN: string = 'b';
  HUSTR_KEYRED: string = 'r';

//
//  AM_map.C
//
  AMSTR_FOLLOWON: string = 'Follow Mode ON';
  AMSTR_FOLLOWOFF: string = 'Follow Mode OFF';
  AMSTR_GRIDON: string = 'Grid ON';
  AMSTR_GRIDOFF: string = 'Grid OFF';
  AMSTR_ROTATEON: string = 'Rotate ON';
  AMSTR_ROTATEOFF: string = 'Rotate OFF';
  AMSTR_MARKEDSPOT: string = 'Marked Spot';
  AMSTR_MARKSCLEARED: string = 'All Marks Cleared';

//
//  ST_stuff.C
//
  STSTR_MUS: string = 'Music Change';
  STSTR_NOMUS: string = 'IMPOSSIBLE SELECTION';
  STSTR_DQDON: string = 'You''re Invincible';
  STSTR_DQDOFF: string = 'You''re a looney';
  STSTR_LGON: string = 'Low Gravity Mode On';
  STSTR_LGOFF: string = 'Low Gravity Mode Off';

  STSTR_KEYSADDED: string = 'Keys Added';
  STSTR_KFAADDED: string = 'Very Happy Ammo Added';
  STSTR_FAADDED: string = 'Ammo (no keys) Added';

  STSTR_NCON: string = 'No Clipping Mode ON';
  STSTR_NCOFF: string = 'No Clipping Mode OFF';

  STSTR_BEHOLD: string = 'inVuln, Str, Inviso, Rad, Allmap, or Lite-amp';
  STSTR_BEHOLDX: string = 'Power-up Toggled';

  STSTR_CHOPPERS: string = '... doesn''t suck - GM';
  STSTR_CLEV: string = 'Changing Level...';

  STSTR_WLEV: string = 'Level specified not found';

  STSTR_MASSACRE: string = 'Massacre';

var
  MSG_MODIFIEDGAME: string =
    '===========================================================================' + #13#10 +
    'ATTENTION:  This version of STRIFE has extra files added to it.' + #13#10 +
    '        You will not receive technical support for modified games.' + #13#10 +
    '===========================================================================' + #13#10;

  MSG_SHAREWARE: string =
    '===========================================================================' + #13#10 +
    '                                Shareware!' + #13#10 +
    '===========================================================================' + #13#10;

  MSG_COMMERCIAL: string =
    '===========================================================================' + #13#10 +
    '             This version is NOT SHAREWARE, do not distribute!' + #13#10 +
    '         Please report software piracy to the SPA: 1-800-388-PIR8' + #13#10 +
    '===========================================================================' + #13#10;

  MSG_UNDETERMINED: string =
        '===========================================================================' + #13#10 +
        '                       Undetermined version! (Ouch)' + #13#10 +
        '===========================================================================' + #13#10;

implementation

end.

