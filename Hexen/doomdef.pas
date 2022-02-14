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
// DESCRIPTION:
//  Internally used data structures for virtually everything,
//  key definitions, lots of other stuff.
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit doomdef;

interface

uses
  d_delphi,
  m_fixed;

//
// Global parameters/defines.
//
// DelphiHexen version
const
  VERSION140 = 140; // unsupported
  VERSION141 = 141;
  VERSION142 = 142;
  VERSION203 = 203;
  VERSION204 = 204;
  VERSION205 = 205;
  VERSION206 = 206;
  VERSION207 = 207;
  VERSION = VERSION207;
  VERSIONSLOPES = VERSION142;
  HXS_VERSION_TEXT_140 = 'DelphiHexen Saved Game (HXS)';
  HXS_VERSION_TEXT_141 = 'DelphiHexen Saved Game (HXS) 141';
  HXS_VERSION_TEXT_142 = 'DelphiHexen Saved Game (HXS) 142';
  HXS_VERSION_TEXT_203 = 'DelphiHexen Saved Game (HXS) 203';
  HXS_VERSION_TEXT_204 = 'DelphiHexen Saved Game (HXS) 204';
  HXS_VERSION_TEXT_205 = 'DelphiHexen Saved Game (HXS) 205';
  HXS_VERSION_TEXT_206 = 'DelphiHexen Saved Game (HXS) 206';
  HXS_VERSION_TEXT_207 = 'DelphiHexen Saved Game (HXS) 207';
  HXS_VERSION_TEXT = HXS_VERSION_TEXT_207;

type
// Game mode handling - identify IWAD version
//  to handle IWAD dependend animations etc.
  GameMode_t = (
    shareware,      // Hexen shareware, 4 maps
    registered,     // Hexen registered
    indetermined    // Well, no IWAD found.
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
    GS_INDETERMINED,
    GS_LEVEL,
    GS_INTERMISSION,
    GS_FINALE,
    GS_DEMOSCREEN
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

// Deaf monsters/do not react to sound.
  MTF_AMBUSH = 8;

  MTF_DORMANT = 16;
  MTF_FIGHTER = 32;
  MTF_CLERIC = 64;
  MTF_MAGE = 128;
  MTF_GSINGLE = 256;
  MTF_GCOOP = 512;
  MTF_GDEATHMATCH = 1024;
  MTF_ONMIDSECTOR = 2048;
  MTF_DONOTTRIGGERSCRIPTS = 4096;
  MTF_FRIEND = 8192; // JVAL: version 207

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
  keytype_t = (
    KEY_1,
    KEY_2,
    KEY_3,
    KEY_4,
    KEY_5,
    KEY_6,
    KEY_7,
    KEY_8,
    KEY_9,
    KEY_A,
    KEY_B,
    NUMKEYCARDS
  );

// The defined weapons,
//  including a marker indicating
//  user has not changed weapon.
  weapontype_t = (
    WP_FIRST,
    WP_SECOND,
    WP_THIRD,
    WP_FOURTH,
    NUMWEAPONS,
    // No pending weapon change.
    WP_NOCHANGE
  );

// Player class types
  pclass_t = (
    PCLASS_FIGHTER,
    PCLASS_CLERIC,
    PCLASS_MAGE,
    PCLASS_PIG,
    NUMCLASSES
  );

// Ammunition types defined.
  manatype_t = (
    MANA_1,
    MANA_2,
    NUMMANA,
    MANA_BOTH,
    MANA_NONE
  );

var
  MAX_MANA: integer = 200;

const
  WPIECE1 = 1;
  WPIECE2 = 2;
  WPIECE3 = 4;

type
// Armor types
  armortype_t = (
    ARMOR_ARMOR,
    ARMOR_SHIELD,
    ARMOR_HELMET,
    ARMOR_AMULET,
    NUMARMOR
  );

  { Weapon info: sprite frames, ammunition use. }
  weaponinfo_t = record
    mana: manatype_t;
    upstate: integer;
    downstate: integer;
    readystate: integer;
    atkstate: integer;
    holdatkstate: integer;
    flashstate: integer;
    intflags: integer; // MBF21
    mbf21bits: integer; // MBF21
  end;
  Pweaponinfo_t = ^weaponinfo_t;
  weaponinfo_tArray = array[0..$FFF] of weaponinfo_t;
  Pweaponinfo_tArray = ^weaponinfo_tArray;

// Power up artifacts.
  artitype_t = (
    arti_none,
    arti_invulnerability,
    arti_health,
    arti_superhealth,
    arti_healingradius,
    arti_summon,
    arti_torch,
    arti_egg,
    arti_fly,
    arti_blastradius,
    arti_poisonbag,
    arti_teleportother,
    arti_speed,
    arti_boostmana,
    arti_boostarmor,
    arti_teleport,
    // Puzzle artifacts
    arti_puzzskull,
    arti_puzzgembig,
    arti_puzzgemred,
    arti_puzzgemgreen1,
    arti_puzzgemgreen2,
    arti_puzzgemblue1,
    arti_puzzgemblue2,
    arti_puzzbook1,
    arti_puzzbook2,
    arti_puzzskull2,
    arti_puzzfweapon,
    arti_puzzcweapon,
    arti_puzzmweapon,
    arti_puzzgear1,
    arti_puzzgear2,
    arti_puzzgear3,
    arti_puzzgear4,
    NUMARTIFACTS
  );

const
  arti_firstpuzzitem = Ord(arti_puzzskull);

type
  powertype_t = (
    pw_None,
    pw_invulnerability,
    pw_allmap,
    pw_infrared,
    pw_flight,
    pw_shield,
    pw_health2,
    pw_speed,
    pw_minotaur,
    NUMPOWERS
  );

const
  AMMO_GWND_WIMPY = 10;
  AMMO_GWND_HEFTY = 50;
  AMMO_CBOW_WIMPY = 5;
  AMMO_CBOW_HEFTY = 20;
  AMMO_BLSR_WIMPY = 10;
  AMMO_BLSR_HEFTY = 25;
  AMMO_SKRD_WIMPY = 20;
  AMMO_SKRD_HEFTY = 100;
  AMMO_PHRD_WIMPY = 1;
  AMMO_PHRD_HEFTY = 10;
  AMMO_MACE_WIMPY = 20;
  AMMO_MACE_HEFTY = 100;

//
// Power up durations,
//  how many seconds till expiration,
//  assuming TICRATE is 35 ticks/second.
//
const
  INVULNTICS = 30 * TICRATE;
  INFRATICS = 120 * TICRATE;
  FLIGHTTICS = 60 * TICRATE;
  SPEEDTICS = 45 * TICRATE;
  MORPHTICS = 40 * TICRATE;
  MAULATORTICS = 25 * TICRATE;

  MESSAGETICS = 4 * TICRATE;
  BLINKTHRESHOLD = 4 * 32;

  NUMINVENTORYSLOTS = Ord(NUMARTIFACTS);

type
  inventory_t = record
    _type: integer;
    count: integer;
  end;

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

const
  ps_weapon = 0;
  ps_flash = 1;

const
  MAX_INTRMSN_MESSAGE_SIZE = 1024;

implementation

end.

