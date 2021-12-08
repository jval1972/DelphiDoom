//------------------------------------------------------------------------------
//
//  DelphiStrife: A modified and improved Strife source port for Windows.
//
//  Based on:
//    - Linux Doom by "id Software"
//    - Chocolate Strife by "Simon Howard"
//    - DelphiDoom by "Jim Valavanis"
//
//  Copyright (C) 1993-1996 by id Software, Inc.
//  Copyright (C) 2005 Simon Howard
//  Copyright (C) 2010 James Haley, Samuel Villarreal
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
// DESCRIPTION:
//  Internally used data structures for virtually everything,
//  key definitions, lots of other stuff.
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit doomdef;

interface

uses
  d_delphi;

//
// Global parameters/defines.
//
// STRIFE version
const
  VERSION120 = 120;
  VERSION121 = 121;
  VERSION122 = 122;
  VERSION203 = 203;
  VERSION204 = 204;
  VERSION205 = 205;
  VERSION206 = 206;
  VERSION207 = 207;
  VERSION = VERSION207;
{$IFDEF OPENGL}
  VERSIONSLOPES = VERSION122;
{$ENDIF}

type
// Game mode handling - identify IWAD version
//  to handle IWAD dependend animations etc.
  GameMode_t = (
    shareware,      // shareware, MAP32 - MAP34
    registered,     // registered
    indetermined    // Well, no IWAD found.
  );

  GameVersion_t = (
    exe_strife_1_20,
    exe_strife_1_31
  );

// Identify language to use, software localization.
  Language_t = (
    english,
    french,
    german,
    unknown
  );

const
  // MAX RENDERING DIMENTIONS
  MAXWIDTH = 4096;
  MAXHEIGHT = 3072;

const
  // MIN RENDERING DIMENTIONS
  MINWIDTH = 160;
  MINHEIGHT = 100;

var
// Rendering Engine Screen Dimentions
  SCREENWIDTH: integer;
  SCREENHEIGHT: integer;
{$IFNDEF OPENGL}
// Window Screen Dimentions
  WINDOWWIDTH: integer;
  WINDOWHEIGHT: integer;
{$ENDIF}

  SCREENWIDTH32PITCH: integer;

  fullscreen: {$IFDEF OPENGL}boolean{$ELSE}integer = 0{$ENDIF};
  zaxisshift: boolean = true;

const
// The maximum number of players, multiplayer/networking.
  MAXPLAYERS = 8;

// State updates, number of tics / second.
  TICRATE = 35;

// The current state of the game: whether we are
// playing, gazing at the intermission screen,
// the game final animation, or a demo.
type
  gamestate_t = (
    GS_LEVEL,
    GS_UNKNOWN,
    GS_FINALE,
    GS_DEMOSCREEN,
    GS_ENDOOM
  );

var
  gamestate: gamestate_t;
  oldgamestate: integer = -1;
  gamedirectories: TDStringList;

const
//
// Difficulty/skill settings/filters.
//

// Skill flags.
  MTF_EASY = 1;
  MTF_NORMAL = 2;
  MTF_HARD = 4;
// villsa [STRIFE] standing monsters
  MTF_STAND = 8;
// villsa [STRIFE] don't spawn in single player
  MTF_NOTSINGLE = 16;
// Deaf monsters/do not react to sound.
  MTF_AMBUSH = 32;
// villsa [STRIFE] friendly to players
  MTF_FRIEND = 64;
// villsa [STRIFE] TODO - identify
  MTF_UNKNOWN1 = 128;
// villsa [STRIFE] thing is translucent - STRIFE-TODO: But how much?
  MTF_TRANSLUCENT = 256;
// villsa [STRIFE] thing is more - or less? - translucent - STRIFE-TODO
  MTF_MVIS = 512;
// villsa [STRIFE] TODO - identify
  MTF_UNKNOWN2 = 1024;
// JVAL: 3d floors
  MTF_ONMIDSECTOR = 2048;
// JVAL: Script Events
  MTF_DONOTTRIGGERSCRIPTS = 4096;

type
  skill_t = (
    sk_baby,
    sk_easy,
    sk_medium,
    sk_hard,
    sk_nightmare
  );

//
// Key cards.
//
  card_t = (
    key_BaseKey,        // 0
    key_GovsKey,        // 1
    key_Passcard,       // 2
    key_IDCard,         // 3
    key_PrisonKey,      // 4
    key_SeveredHand,    // 5
    key_Power1Key,      // 6
    key_Power2Key,      // 7
    key_Power3Key,      // 8
    key_GoldKey,        // 9
    key_IDBadge,        // 10
    key_SilverKey,      // 11
    key_OracleKey,      // 12
    key_MilitaryID,     // 13
    key_OrderKey,       // 14
    key_WarehouseKey,   // 15
    key_BrassKey,       // 16
    key_RedCrystalKey,  // 17
    key_BlueCrystalKey, // 18
    key_ChapelKey,      // 19
    key_CatacombKey,    // 20
    key_SecurityKey,    // 21
    key_CoreKey,        // 22
    key_MaulerKey,      // 23
    key_FactoryKey,     // 24
    key_MineKey,        // 25
    key_NewKey5,        // 26
    NUMCARDS
  );

// The defined weapons,
//  including a marker indicating
//  user has not changed weapon.
  weapontype_t = (
    wp_fist,
    wp_elecbow,
    wp_rifle,
    wp_missile,
    wp_hegrenade,
    wp_flame,
    wp_mauler,
    wp_sigil,
    wp_poisonbow,
    wp_wpgrenade,
    wp_torpedo,
    NUMWEAPONS,
    // No pending weapon change.
    wp_nochange
  );

// Ammunition types defined.
  ammotype_t = (
    am_bullets,
    am_elecbolts,
    am_poisonbolts,
    am_cell,
    am_missiles,
    am_hegrenades,
    am_wpgrenades,
    NUMAMMO,
    am_noammo // Unlimited for chainsaw / fist.
  );

// Power up artifacts.
  powertype_t = (
    pw_strength,
    pw_invisibility,
    pw_ironfeet,
    pw_allmap,
    pw_communicator,
    pw_targeter,
    NUMPOWERS
  );
  Ppowertype_t = ^powertype_t;

// villsa [STRIFE]
// quest numbers
type
  questtype_t = (
                // Hex          Watcom Name               player_t offset
    tk_quest1,  // 0x00000001   questflags & 1            0x4D
    tk_quest2,  // 0x00000002   questflags & 2
    tk_quest3,  // 0x00000004   questflags & 4
    tk_quest4,  // 0x00000008   questflags & 8
    tk_quest5,  // 0x00000010   questflags & 10h
    tk_quest6,  // 0x00000020   questflags & 20h
    tk_quest7,  // 0x00000040   questflags & 40h
    tk_quest8,  // 0x00000080   questflags & 80h
    tk_quest9,  // 0x00000100   BYTE1(questflags) & 1     0x4E
    tk_quest10, // 0x00000200   BYTE1(questflags) & 2
    tk_quest11, // 0x00000400   BYTE1(questflags) & 4
    tk_quest12, // 0x00000800   BYTE1(questflags) & 8
    tk_quest13, // 0x00001000   BYTE1(questflags) & 10h
    tk_quest14, // 0x00002000   BYTE1(questflags) & 20h
    tk_quest15, // 0x00004000   BYTE1(questflags) & 40h
    tk_quest16, // 0x00008000   BYTE1(questflags) & 80h
    tk_quest17, // 0x00010000   BYTE2(questflags) & 1     0x4F
    tk_quest18, // 0x00020000   BYTE2(questflags) & 2
    tk_quest19, // 0x00040000   BYTE2(questflags) & 4
    tk_quest20, // 0x00080000   BYTE2(questflags) & 8
    tk_quest21, // 0x00100000   BYTE2(questflags) & 10h
    tk_quest22, // 0x00200000   BYTE2(questflags) & 20h
    tk_quest23, // 0x00400000   BYTE2(questflags) & 40h
    tk_quest24, // 0x00800000   BYTE2(questflags) & 80h
    tk_quest25, // 0x01000000   BYTE3(questflags) & 1     0x50
    tk_quest26, // 0x02000000   BYTE3(questflags) & 2
    tk_quest27, // 0x04000000   BYTE3(questflags) & 4
    tk_quest28, // 0x08000000   BYTE3(questflags) & 8
    tk_quest29, // 0x10000000   BYTE3(questflags) & 10h
    tk_quest30, // 0x20000000   BYTE3(questflags) & 20h
    tk_quest31, // 0x40000000   BYTE3(questflags) & 40h
    tk_quest32, // most likely unused
    tk_numquests
  );

const
    // haleyjd 09/12/10: [STRIFE]
    // flag values for each quest.
    //  Name       Flag from bitnum      Purpose, if known
    QF_QUEST1  = (1  shl 0);  // Obtained Beldin's ring
    QF_QUEST2  = (1  shl 1);  // Stole the Chalice
    QF_QUEST3  = (1  shl 2);  // Permission to visit Irale (visited Macil)
    QF_QUEST4  = (1  shl 3);  // Accepted Gov. Mourel's "messy" chore
    QF_QUEST5  = (1  shl 4);  // Accepted Gov. Mourel's "bloody" chore
    QF_QUEST6  = (1  shl 5);  // Destroyed the Power Coupling
    QF_QUEST7  = (1  shl 6);  // Killed Blue Acolytes ("Scanning Team")
    QF_QUEST8  = (1  shl 7);  // Unused; formerly, picked up Broken Coupling
    QF_QUEST9  = (1  shl 8);  // Obtained Derwin's ear
    QF_QUEST10 = (1  shl 9); // Obtained Prison Pass
    QF_QUEST11 = (1  shl 10); // Obtained Prison Key
    QF_QUEST12 = (1  shl 11); // Obtained Judge Wolenick's hand
    QF_QUEST13 = (1  shl 12); // Freed the Prisoners
    QF_QUEST14 = (1  shl 13); // Destroyed the Power Crystal
    QF_QUEST15 = (1  shl 14); // Obtained Guard Uniform
    QF_QUEST16 = (1  shl 15); // Destroyed the Gate Mechanism
    QF_QUEST17 = (1  shl 16); // Heard Macil's story about the Sigil (MAP10)
    QF_QUEST18 = (1  shl 17); // Obtained Oracle Pass
    QF_QUEST19 = (1  shl 18);
    QF_QUEST20 = (1  shl 19);
    QF_QUEST21 = (1  shl 20); // Killed Bishop
    QF_QUEST22 = (1  shl 21); // Killed Oracle with QUEST21 set
    QF_QUEST23 = (1  shl 22); // Killed Oracle (always given)
    QF_QUEST24 = (1  shl 23); // Killed Macil
    QF_QUEST25 = (1  shl 24); // Destroyed the Converter
    QF_QUEST26 = (1  shl 25); // Killed Loremaster
    QF_QUEST27 = (1  shl 26); // Destroyed the Computer (checked for good ending)
    QF_QUEST28 = (1  shl 27); // Obtained Catacomb Key (checked by line type 228)
    QF_QUEST29 = (1  shl 28); // Destroyed the Mines Transmitter
    QF_QUEST30 = (1  shl 29);
    QF_QUEST31 = (1  shl 30);
    QF_QUEST32 = (1  shl 31); // Unused; BUG: Broken Coupling accidentally sets it.

    QF_ALLQUESTS: LongWord = $FFFFFFFF; // does not include bit 32!

//
// Power up durations,
//  how many seconds till expiration,
//  assuming TICRATE is 35 ticks/second.
//
const
  INVISTICS = (55 * TICRATE); // villsa [STRIFE] changed from 60 to 55
  IRONTICS  = (80 * TICRATE); // villsa [STRIFE] changed from 60 to 80
  PMUPTICS  = (80 * TICRATE); // villsa [STRIFE]
  TARGTICS  = (160 * TICRATE);// villsa [STRIFE]

//
// DOOM keyboard definition.
// This is the stuff configured by Setup.Exe.
// Most key data are simple ascii (uppercased).
//
const
  KEY_RIGHTARROW = $ae;
  KEY_LEFTARROW = $ac;
  KEY_UPARROW = $ad;
  KEY_DOWNARROW = $af;
  KEY_ESCAPE = 27;
  KEY_ENTER = 13;
  KEY_TAB = 9;

  KEY_F1 = $80 + $3b;
  KEY_F2 = $80 + $3c;
  KEY_F3 = $80 + $3d;
  KEY_F4 = $80 + $3e;
  KEY_F5 = $80 + $3f;
  KEY_F6 = $80 + $40;
  KEY_F7 = $80 + $41;
  KEY_F8 = $80 + $42;
  KEY_F9 = $80 + $43;
  KEY_F10 = $80 + $44;
  KEY_F11 = $80 + $57;
  KEY_F12 = $80 + $58;

  KEY_PRNT = $80 + $59;

  KEY_CON = 126;
  KEY_BACKSPACE = 127;
  KEY_PAUSE = $ff;

  KEY_EQUALS = $3d;
  KEY_MINUS = $2d;

  KEY_RSHIFT = $80 + $36;
  KEY_RCTRL = $80 + $1d;
  KEY_RALT = $80 + $38;

  KEY_PAGEDOWN = $80 + $45;
  KEY_PAGEUP = $80 + $46;
  KEY_INS = $80 + $47;


  KEY_HOME = $80 + $48;
  KEY_END = $80 + $49;
  KEY_DELETE = $80 + $4a;


  KEY_LALT = KEY_RALT;

implementation

end.

