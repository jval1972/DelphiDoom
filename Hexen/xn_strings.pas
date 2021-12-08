//------------------------------------------------------------------------------
//
//  DelphiHexen: A modified and improved Hexen port for Windows
//  based on original Linux Doom as published by "id Software", on
//  Hexen source as published by "Raven" software and DelphiDoom
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
//  HEXEN strings, by language.
//
// DESCRIPTION:
// Globally defined strings.
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit xn_strings;

interface

uses
  doomdef;

// Misc. other strings.
const
//
// File locations,
// relative to current position.
// Path names are OS-sensitive.
//
  DEVMAPS = 'devmaps';

// Not done in french?

// Start-up messages
  NUM_STARTUPMESSAGES = 5;

var
  D_DEVSTR: string =
    'Development mode ON.' + #13#10;
  D_CDROM: string =
    'CD-ROM Version: hexen.cfg from c:\hexen.cd' + #13#10;

//
//  M_Menu.C
//
  PRESSKEY: string =
    'press a key.';
  PRESSYN: string =
    'press y or n.';

  QUITMSG: string =
    'are you sure you want to' + #13#10 + 'quit this great game?';

  LOADNET: string =
    'you can''t do load while in a net game!' + #13#10;

  QLOADNET: string =
    'you can''t quickload during a netgame!' + #13#10;

  QSAVESPOT: string =
    'you haven''t picked a quicksave slot yet!' + #13#10;

  SAVEDEAD: string =
    'you can''t save if you aren''t playing!' + #13#10;

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
  SENDGAME : string =
    'are you sure you want to end the game?' + #13#10;

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
  TXT_MANA_1: string = 'BLUE MANA';
  TXT_MANA_2: string = 'GREEN MANA';
  TXT_MANA_BOTH: string = 'COMBINED MANA';

// Keys

  TXT_KEY_STEEL: string = 'STEEL KEY';
  TXT_KEY_CAVE: string = 'CAVE KEY';
  TXT_KEY_AXE: string = 'AXE KEY';
  TXT_KEY_FIRE: string = 'FIRE KEY';
  TXT_KEY_EMERALD: string = 'EMERALD KEY';
  TXT_KEY_DUNGEON: string = 'DUNGEON KEY';
  TXT_KEY_SILVER: string = 'SILVER KEY';
  TXT_KEY_RUSTED: string = 'RUSTED KEY';
  TXT_KEY_HORN: string = 'HORN KEY';
  TXT_KEY_SWAMP: string = 'SWAMP KEY';
  TXT_KEY_CASTLE: string = 'CASTLE KEY';

var
  TextKeyMessages: array[0..Ord(NUMKEYCARDS) - 1] of string;

const
// Artifacts

  TXT_ARTIINVULNERABILITY: string = 'ICON OF THE DEFENDER';
  TXT_ARTIHEALTH: string = 'QUARTZ FLASK';
  TXT_ARTISUPERHEALTH: string = 'MYSTIC URN';
  TXT_ARTISUMMON: string = 'DARK SERVANT';
  TXT_ARTITORCH: string = 'TORCH';
  TXT_ARTIEGG: string = 'PORKALATOR';
  TXT_ARTIFLY: string = 'WINGS OF WRATH';
  TXT_ARTITELEPORT: string = 'CHAOS DEVICE';
  TXT_ARTIPOISONBAG: string = 'FLECHETTE';
  TXT_ARTITELEPORTOTHER: string = 'BANISHMENT DEVICE';
  TXT_ARTISPEED: string = 'BOOTS OF SPEED';
  TXT_ARTIBOOSTMANA: string = 'KRATER OF MIGHT';
  TXT_ARTIBOOSTARMOR: string = 'DRAGONSKIN BRACERS';
  TXT_ARTIBLASTRADIUS: string = 'DISC OF REPULSION';
  TXT_ARTIHEALINGRADIUS: string = 'MYSTIC AMBIT INCANT';
  TXT_ARTIALL: string = 'ALL ARIFACTS';
  TXT_ARTIALLPUZZLE: string = 'ALL PUZZLE ARIFACTS';

// Puzzle artifacts

  TXT_ARTIPUZZSKULL: string = 'YORICK''S SKULL';
  TXT_ARTIPUZZGEMBIG: string = 'HEART OF D''SPARIL';
  TXT_ARTIPUZZGEMRED: string = 'RUBY PLANET';
  TXT_ARTIPUZZGEMGREEN1: string = 'EMERALD PLANET';
  TXT_ARTIPUZZGEMGREEN2: string = 'EMERALD PLANET';
  TXT_ARTIPUZZGEMBLUE1: string = 'SAPPHIRE PLANET';
  TXT_ARTIPUZZGEMBLUE2: string = 'SAPPHIRE PLANET';
  TXT_ARTIPUZZBOOK1: string = 'DAEMON CODEX';
  TXT_ARTIPUZZBOOK2: string = 'LIBER OSCURA';
  TXT_ARTIPUZZSKULL2: string = 'FLAME MASK';
  TXT_ARTIPUZZFWEAPON: string = 'GLAIVE SEAL';
  TXT_ARTIPUZZCWEAPON: string = 'HOLY RELIC';
  TXT_ARTIPUZZMWEAPON: string = 'SIGIL OF THE MAGUS';
  TXT_ARTIPUZZGEAR: string = 'CLOCK GEAR';
  TXT_USEPUZZLEFAILED: string = 'YOU CANNOT USE THIS HERE';


// Items

  TXT_ITEMHEALTH: string = 'CRYSTAL VIAL';
  TXT_ITEMBAGOFHOLDING: string = 'BAG OF HOLDING';
  TXT_ITEMSHIELD1: string = 'SILVER SHIELD';
  TXT_ITEMSHIELD2: string = 'ENCHANTED SHIELD';
  TXT_ITEMSUPERMAP: string = 'MAP SCROLL';
  TXT_ARMOR1: string = 'MESH ARMOR';
  TXT_ARMOR2: string = 'FALCON SHIELD';
  TXT_ARMOR3: string = 'PLATINUM HELMET';
  TXT_ARMOR4: string = 'AMULET OF WARDING';

// Weapons

  TXT_WEAPON_F2: string = 'TIMON''S AXE';
  TXT_WEAPON_F3: string = 'HAMMER OF RETRIBUTION';
  TXT_WEAPON_F4: string = 'QUIETUS ASSEMBLED';
  TXT_WEAPON_C2: string = 'SERPENT STAFF';
  TXT_WEAPON_C3: string = 'FIRESTORM';
  TXT_WEAPON_C4: string = 'WRAITHVERGE ASSEMBLED';
  TXT_WEAPON_M2: string = 'FROST SHARDS';
  TXT_WEAPON_M3: string = 'ARC OF DEATH';
  TXT_WEAPON_M4: string = 'BLOODSCOURGE ASSEMBLED';
  TXT_QUIETUS_PIECE: string = 'SEGMENT OF QUIETUS';
  TXT_WRAITHVERGE_PIECE: string = 'SEGMENT OF WRAITHVERGE';
  TXT_BLOODSCOURGE_PIECE: string = 'SEGMENT OF BLOODSCOURGE';

//---------------------------------------------------------------------------
//
// SB_bar.c
//
//---------------------------------------------------------------------------

  TXT_CHEATGODON: string = 'GOD MODE ON';
  TXT_CHEATGODOFF: string = 'GOD MODE OFF';
  TXT_CHEATNOCLIPON: string = 'NO CLIPPING ON';
  TXT_CHEATNOCLIPOFF: string = 'NO CLIPPING OFF';
  TXT_CHEATWEAPONS: string = 'ALL WEAPONS';
  TXT_CHEATFLIGHTON: string = 'FLIGHT ON';
  TXT_CHEATFLIGHTOFF: string = 'FLIGHT OFF';
  TXT_CHEATPOWERON: string = 'POWER ON';
  TXT_CHEATPOWEROFF: string = 'POWER OFF';
  TXT_CHEATHEALTH: string = 'FULL HEALTH';
  TXT_CHEATKEYS: string = 'ALL KEYS';
  TXT_CHEATSOUNDON: string = 'SOUND DEBUG ON';
  TXT_CHEATSOUNDOFF: string = 'SOUND DEBUG OFF';
  TXT_CHEATTICKERON: string = 'TICKER ON';
  TXT_CHEATTICKEROFF: string = 'TICKER OFF';
  TXT_CHEATARTIFACTS1: string = 'CHOOSE AN ARTIFACT ( A - J )';
  TXT_CHEATARTIFACTS2: string = 'HOW MANY ( 1 - 9 )';
  TXT_CHEATARTIFACTS3: string = 'YOU GOT IT';
  TXT_CHEATARTIFACTSFAIL: string = 'BAD INPUT';
  TXT_CHEATWARP: string = 'LEVEL WARP';
  TXT_CHEATSCREENSHOT: string = 'SCREENSHOT';
  TXT_CHEATCHICKENON: string = 'CHICKEN ON';
  TXT_CHEATCHICKENOFF: string = 'CHICKEN OFF';
  TXT_CHEATMASSACRE: string = 'MONSTERS KILLED';
  TXT_CHEATIDDQD: string = 'TRYING TO CHEAT, EH?  NOW YOU DIE!';
  TXT_CHEATIDKFA: string = 'CHEATER - YOU DON''T DESERVE WEAPONS';
  TXT_CHEATPIG: string = 'SQUEAL!!';

  MSGSECRETSECTOR: string = 'A Secret is revealed!';

//
// G_game.C
//
var
  GGSAVED: string = 'game saved.';
  GACCESSDENIEDDEMO: string = 'access denied -- demo';

const
  HUSTR_CHATMACRO1 = 'I''m ready to kick butt!';
  HUSTR_CHATMACRO2 = 'I''m OK.';
  HUSTR_CHATMACRO3 = 'I''m not looking too good!';
  HUSTR_CHATMACRO4 = 'Help!';
  HUSTR_CHATMACRO5 = 'You suck!';
  HUSTR_CHATMACRO6 = 'Next time, scumbag...';
  HUSTR_CHATMACRO7 = 'Come here!';
  HUSTR_CHATMACRO8 = 'I''ll take care of it.';
  HUSTR_CHATMACRO9 = 'Yes';
  HUSTR_CHATMACRO0 = 'No';

var
  HUSTR_TALKTOSELF1: string = 'You mumble to yourself';
  HUSTR_TALKTOSELF2: string = 'Who''s there?';
  HUSTR_TALKTOSELF3: string = 'You scare yourself';
  HUSTR_TALKTOSELF4: string = 'You start to rave';
  HUSTR_TALKTOSELF5: string = 'You''ve lost it...';

  HUSTR_MESSAGESENT: string = '[Message Sent]';
  HUSTR_MSGU: string = '[Message unsent]';

  { The following should NOT be changed unless it seems }
  { just AWFULLY necessary }
  HUSTR_PLRGREEN: string = 'Green:';
  HUSTR_PLRINDIGO: string = 'Indigo:';
  HUSTR_PLRBROWN: string = 'Brown:';
  HUSTR_PLRRED: string = 'Red:';

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
//  st_bar
//
  STSTR_MUS: string = 'Music Change';
  STSTR_NOMUS: string = 'IMPOSSIBLE SELECTION';
  STSTR_LGON: string = 'Low Gravity Mode On';
  STSTR_LGOFF: string = 'Low Gravity Mode Off';

  STSTR_KFAADDED: string = 'Very Happy Ammo Added';

  STSTR_NCON: string = 'No Clipping Mode ON';
  STSTR_NCOFF: string = 'No Clipping Mode OFF';

  STSTR_CHOPPERS: string = '... doesn''t suck - GM';
  STSTR_CLEV: string = 'Changing Level...';

  STSTR_WLEV: string = 'Level specified not found';

var
  MSG_MODIFIEDGAME: string =
      '===========================================================================' + #13#10 +
      'ATTENTION:  This version of HEXEN has been modified.' + #13#10 +
      '===========================================================================' + #13#10;

  MSG_SHAREWARE: string =
      '===============================================================================' + #13#10 +
      '                             Hexen:  Beyond Heretic' + #13#10 +
      '' + #13#10 +
      '                              %d Level Demo Version' + #13#10 +
      '                           Press any key to continue.' + #13#10 +
      '===============================================================================' + #13#10;

  MSG_COMMERCIAL: string =
        '===========================================================================' + #13#10 +
        '                 Commercial product - do not distribute!' + #13#10 +
        '         Please report software piracy to the SPA: 1-800-388-PIR8' + #13#10 +
        '===========================================================================' + #13#10;

  MSG_HEXDD: string =
        '===========================================================================' + #13#10 +
        '               Death Kings of the Dark Citadel expansion' + #13#10 +
        '===========================================================================' + #13#10;

  MSG_UNDETERMINED: string =
        '===========================================================================' + #13#10 +
        '                       Undetermined version! (Ouch)' + #13#10 +
        '===========================================================================' + #13#10;


var
  startmsg: array[0..NUM_STARTUPMESSAGES - 1] of string;

var
  fourthWeaponText: array[0..2] of string;
  weaponPieceText: array[0..2] of string;
  artifactMessages: array[0..Ord(NUMARTIFACTS) - 1] of string;

implementation

initialization

  startmsg[0] := '';
  startmsg[1] := '';
  startmsg[2] := '';
  startmsg[3] := '';
  startmsg[4] := '';

  TextKeyMessages[0] := TXT_KEY_STEEL;
  TextKeyMessages[1] := TXT_KEY_CAVE;
  TextKeyMessages[2] := TXT_KEY_AXE;
  TextKeyMessages[3] := TXT_KEY_FIRE;
  TextKeyMessages[4] := TXT_KEY_EMERALD;
  TextKeyMessages[5] := TXT_KEY_DUNGEON;
  TextKeyMessages[6] := TXT_KEY_SILVER;
  TextKeyMessages[7] := TXT_KEY_RUSTED;
  TextKeyMessages[8] := TXT_KEY_HORN;
  TextKeyMessages[9] := TXT_KEY_SWAMP;
  TextKeyMessages[10] := TXT_KEY_CASTLE;

  fourthWeaponText[0] := TXT_WEAPON_F4;
  fourthWeaponText[1] := TXT_WEAPON_C4;
  fourthWeaponText[2] := TXT_WEAPON_M4;

  weaponPieceText[0] := TXT_QUIETUS_PIECE;
  weaponPieceText[1] := TXT_WRAITHVERGE_PIECE;
  weaponPieceText[2] := TXT_BLOODSCOURGE_PIECE;

  artifactMessages[0] := '';
  artifactMessages[1] := TXT_ARTIINVULNERABILITY;
  artifactMessages[2] := TXT_ARTIHEALTH;
  artifactMessages[3] := TXT_ARTISUPERHEALTH;
  artifactMessages[4] := TXT_ARTIHEALINGRADIUS;
  artifactMessages[5] := TXT_ARTISUMMON;
  artifactMessages[6] := TXT_ARTITORCH;
  artifactMessages[7] := TXT_ARTIEGG;
  artifactMessages[8] := TXT_ARTIFLY;
  artifactMessages[9] := TXT_ARTIBLASTRADIUS;
  artifactMessages[10] := TXT_ARTIPOISONBAG;
  artifactMessages[11] := TXT_ARTITELEPORTOTHER;
  artifactMessages[12] := TXT_ARTISPEED;
  artifactMessages[13] := TXT_ARTIBOOSTMANA;
  artifactMessages[14] := TXT_ARTIBOOSTARMOR;
  artifactMessages[15] := TXT_ARTITELEPORT;
  artifactMessages[16] := TXT_ARTIPUZZSKULL;
  artifactMessages[17] := TXT_ARTIPUZZGEMBIG;
  artifactMessages[18] := TXT_ARTIPUZZGEMRED;
  artifactMessages[19] := TXT_ARTIPUZZGEMGREEN1;
  artifactMessages[20] := TXT_ARTIPUZZGEMGREEN2;
  artifactMessages[21] := TXT_ARTIPUZZGEMBLUE1;
  artifactMessages[22] := TXT_ARTIPUZZGEMBLUE2;
  artifactMessages[23] := TXT_ARTIPUZZBOOK1;
  artifactMessages[24] := TXT_ARTIPUZZBOOK2;
  artifactMessages[25] := TXT_ARTIPUZZSKULL2;
  artifactMessages[26] := TXT_ARTIPUZZFWEAPON;
  artifactMessages[27] := TXT_ARTIPUZZCWEAPON;
  artifactMessages[28] := TXT_ARTIPUZZMWEAPON;
  artifactMessages[29] := TXT_ARTIPUZZGEAR;  // All gear pickups use the same text
  artifactMessages[30] := TXT_ARTIPUZZGEAR;
  artifactMessages[31] := TXT_ARTIPUZZGEAR;
  artifactMessages[32] := TXT_ARTIPUZZGEAR;

end.

