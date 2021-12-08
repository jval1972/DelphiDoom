//------------------------------------------------------------------------------
//
//  DelphiHeretic: A modified and improved Heretic port for Windows
//  based on original Linux Doom as published by "id Software", on
//  Heretic source as published by "Raven" software and DelphiDoom
//  as published by Jim Valavanis.
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
//  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
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
  d_delphi,
  m_fixed;

//
// Global parameters/defines.
//
// Heretic version
const
  VERSION110 = 110;
  VERSION111 = 111;
  VERSION112 = 112;
  VERSION113 = 113;
  VERSION114 = 114;
  VERSION115 = 115;
  VERSION203 = 203;
  VERSION204 = 204;
  VERSION205 = 205;
  VERSION206 = 206;
  VERSION207 = 207;
  VERSION = VERSION207;
{$IFDEF OPENGL}
  VERSIONSLOPES = VERSION115;
{$ENDIF}

type
// Game mode handling - identify IWAD version
//  to handle IWAD dependend animations etc.
  GameMode_t = (
    shareware,      // Heretic shareware, E1, M9
    registered,     // Heretic registered, E3, M27
    extendedwad,    // Heretic E5
    indetermined    // Well, no IWAD found.
  );

// Identify language to use, software localization.
  Language_t = (
    english,
    french,
    german,
    unknown
  );

  // JVAL: Support for the beta version
  CustomGame_t = (
    cg_none,
    cg_beta
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
  MAXPLAYERS = 4;

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

// Deaf monsters/do not react to sound.
  MTF_AMBUSH = 8;
  MTF_ONMIDSECTOR = 32;
  MTF_DONOTTRIGGERSCRIPTS = 64;

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
    key_yellow,
    key_green,
    key_blue,
    NUMKEYCARDS
  );


// The defined weapons,
//  including a marker indicating
//  user has not changed weapon.
  weapontype_t = (
    wp_staff,
    wp_goldwand,
    wp_crossbow,
    wp_blaster,
    wp_skullrod,
    wp_phoenixrod,
    wp_mace,
    wp_gauntlets,
    wp_beak,
    NUMWEAPONS,
    // No pending weapon change.
    wp_nochange
  );


// Ammunition types defined.
  ammotype_t = (
    am_goldwand,
    am_crossbow,
    am_blaster,
    am_skullrod,
    am_phoenixrod,
    am_mace,
    NUMAMMO,
    am_noammo // Unlimited for chainsaw / fist.
  );

  { Weapon info: sprite frames, ammunition use. }
  weaponinfo_t = record
    ammo: ammotype_t;
    upstate: integer;
    downstate: integer;
    readystate: integer;
    atkstate: integer;
    holdatkstate: integer;
    flashstate: integer;
  end;
  Pweaponinfo_t = ^weaponinfo_t;
  weaponinfo_tArray = array[0..$FFF] of weaponinfo_t;
  Pweaponinfo_tArray = ^weaponinfo_tArray;

// Power up artifacts.
  artitype_t = (
    arti_none,
    arti_invulnerability,
    arti_invisibility,
    arti_health,
    arti_superhealth,
    arti_tomeofpower,
    arti_torch,
    arti_firebomb,
    arti_egg,
    arti_fly,
    arti_teleport,
    NUMARTIFACTS
  );

  powertype_t = (
    pw_None,
    pw_invulnerability,
    pw_invisibility,
    pw_allmap,
    pw_infrared,
    pw_weaponlevel2,
    pw_flight,
    pw_shield,
    pw_health2,
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
  INVISTICS = 60 * TICRATE;
  INFRATICS = 120 * TICRATE;

  WPNLEV2TICS = 40 * TICRATE;
  FLIGHTTICS = 60 * TICRATE;

  CHICKENTICS = 40 * TICRATE;

  MESSAGETICS = 4 * TICRATE;
  BLINKTHRESHOLD = 4 * 32;

  NUMINVENTORYSLOTS = 14;

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
  TELEFOGHEIGHT = 32 * FRACUNIT;

implementation

end.

