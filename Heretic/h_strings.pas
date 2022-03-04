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
// DESCRIPTION:
//  DOOM strings, by language.
//
// DESCRIPTION:
// Globally defined strings.
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit h_strings;

interface

// Misc. other strings.
var
  SAVEGAMENAME: string = 'HTICSAV';

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
    'CD-ROM Version: heretic.cfg from c:\heretic.cd' + #13#10;

//
//  M_Menu.C
//
  PRESSKEY: string =
    'press a key.';
  PRESSYN: string =
    'press y or n.';

  QUITMSG: string =
    'are you sure you want to quit?';

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

  SWSTRING: string =
    'this is the shareware version of Heretic.' + #13#10 +
    'you need to order the entire trilogy.';

  MSGOFF: string =
    'Messages OFF';
  MSGON: string =
    'Messages ON';

  NETEND: string =
    'you can''t end a netgame!' + #13#10;
  SENDGAME: string =
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
var
  TXT_GOTBLUEKEY: string = 'BLUE KEY';
  TXT_GOTYELLOWKEY: string = 'YELLOW KEY';
  TXT_GOTGREENKEY: string = 'GREEN KEY';

// Artifacts

  TXT_ARTIHEALTH: string = 'QUARTZ FLASK';
  TXT_ARTIFLY: string = 'WINGS OF WRATH';
  TXT_ARTIINVULNERABILITY: string = 'RING OF INVINCIBILITY';
  TXT_ARTITOMEOFPOWER: string = 'TOME OF POWER';
  TXT_ARTIINVISIBILITY: string = 'SHADOWSPHERE';
  TXT_ARTIEGG: string = 'MORPH OVUM';
  TXT_ARTISUPERHEALTH: string = 'MYSTIC URN';
  TXT_ARTITORCH: string = 'TORCH';
  TXT_ARTIFIREBOMB: string = 'TIME BOMB OF THE ANCIENTS';
  TXT_ARTITELEPORT: string = 'CHAOS DEVICE';
  TXT_ARTIALL: string = 'ALL ARIFACTS';

// Items

  TXT_ITEMHEALTH: string = 'CRYSTAL VIAL';
  TXT_ITEMBAGOFHOLDING: string = 'BAG OF HOLDING';
  TXT_ITEMSHIELD1: string = 'SILVER SHIELD';
  TXT_ITEMSHIELD2: string = 'ENCHANTED SHIELD';
  TXT_ITEMSUPERMAP: string = 'MAP SCROLL';

// Ammo

  TXT_AMMOGOLDWAND1: string = 'WAND CRYSTAL';
  TXT_AMMOGOLDWAND2: string = 'CRYSTAL GEODE';
  TXT_AMMOMACE1: string = 'MACE SPHERES';
  TXT_AMMOMACE2: string = 'PILE OF MACE SPHERES';
  TXT_AMMOCROSSBOW1: string = 'ETHEREAL ARROWS';
  TXT_AMMOCROSSBOW2: string = 'QUIVER OF ETHEREAL ARROWS';
  TXT_AMMOBLASTER1: string = 'CLAW ORB';
  TXT_AMMOBLASTER2: string = 'ENERGY ORB';
  TXT_AMMOSKULLROD1: string = 'LESSER RUNES';
  TXT_AMMOSKULLROD2: string = 'GREATER RUNES';
  TXT_AMMOPHOENIXROD1: string = 'FLAME ORB';
  TXT_AMMOPHOENIXROD2: string = 'INFERNO ORB';

// Weapons

  TXT_WPNMACE: string = 'FIREMACE';
  TXT_WPNCROSSBOW: string = 'ETHEREAL CROSSBOW';
  TXT_WPNBLASTER: string = 'DRAGON CLAW';
  TXT_WPNSKULLROD: string = 'HELLSTAFF';
  TXT_WPNPHOENIXROD: string = 'PHOENIX ROD';
  TXT_WPNGAUNTLETS: string = 'GAUNTLETS OF THE NECROMANCER';

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
  TXT_CHEATMASSACRE: string = 'MASSACRE';
  TXT_CHEATIDDQD: string = 'TRYING TO CHEAT, EH?  NOW YOU DIE!';
  TXT_CHEATIDKFA: string = 'CHEATER - YOU DON''T DESERVE WEAPONS';

  MSGSECRETSECTOR: string = 'You found a secret area.';

//---------------------------------------------------------------------------
//
// P_doors.c
//
//---------------------------------------------------------------------------

  TXT_NEEDBLUEKEY: string = 'YOU NEED A BLUE KEY TO OPEN THIS DOOR';
  TXT_NEEDGREENKEY: string = 'YOU NEED A GREEN KEY TO OPEN THIS DOOR';
  TXT_NEEDYELLOWKEY: string = 'YOU NEED A YELLOW KEY TO OPEN THIS DOOR';

//
// G_game.C
//
var
  GGSAVED: string = 'game saved.';

const
//
//  HU_stuff.C
//
  HUSTR_E1M1 = 'E1M1:  THE DOCKS';
  HUSTR_E1M2 = 'E1M2:  THE DUNGEONS';
  HUSTR_E1M3 = 'E1M3:  THE GATEHOUSE';
  HUSTR_E1M4 = 'E1M4:  THE GUARD TOWER';
  HUSTR_E1M5 = 'E1M5:  THE CITADEL';
  HUSTR_E1M6 = 'E1M6:  THE CATHEDRAL';
  HUSTR_E1M7 = 'E1M7:  THE CRYPTS';
  HUSTR_E1M8 = 'E1M8:  HELL''S MAW';
  HUSTR_E1M9 = 'E1M9:  THE GRAVEYARD';
  // EPISODE 2 - HELL''S MAW
  HUSTR_E2M1 = 'E2M1:  THE CRATER';
  HUSTR_E2M2 = 'E2M2:  THE LAVA PITS';
  HUSTR_E2M3 = 'E2M3:  THE RIVER OF FIRE';
  HUSTR_E2M4 = 'E2M4:  THE ICE GROTTO';
  HUSTR_E2M5 = 'E2M5:  THE CATACOMBS';
  HUSTR_E2M6 = 'E2M6:  THE LABYRINTH';
  HUSTR_E2M7 = 'E2M7:  THE GREAT HALL';
  HUSTR_E2M8 = 'E2M8:  THE PORTALS OF CHAOS';
  HUSTR_E2M9 = 'E2M9:  THE GLACIER';
  // EPISODE 3 - THE DOME OF D''SPARIL
  HUSTR_E3M1 = 'E3M1:  THE STOREHOUSE';
  HUSTR_E3M2 = 'E3M2:  THE CESSPOOL';
  HUSTR_E3M3 = 'E3M3:  THE CONFLUENCE';
  HUSTR_E3M4 = 'E3M4:  THE AZURE FORTRESS';
  HUSTR_E3M5 = 'E3M5:  THE OPHIDIAN LAIR';
  HUSTR_E3M6 = 'E3M6:  THE HALLS OF FEAR';
  HUSTR_E3M7 = 'E3M7:  THE CHASM';
  HUSTR_E3M8 = 'E3M8:  D''SPARIL''S KEEP';
  HUSTR_E3M9 = 'E3M9:  THE AQUIFER';
  // EPISODE 4: THE OSSUARY
  HUSTR_E4M1 = 'E4M1:  CATAFALQUE';
  HUSTR_E4M2 = 'E4M2:  BLOCKHOUSE';
  HUSTR_E4M3 = 'E4M3:  AMBULATORY';
  HUSTR_E4M4 = 'E4M4:  SEPULCHER';
  HUSTR_E4M5 = 'E4M5:  GREAT STAIR';
  HUSTR_E4M6 = 'E4M6:  HALLS OF THE APOSTATE';
  HUSTR_E4M7 = 'E4M7:  RAMPARTS OF PERDITION';
  HUSTR_E4M8 = 'E4M8:  SHATTERED BRIDGE';
  HUSTR_E4M9 = 'E4M9:  MAUSOLEUM';
  // EPISODE 5: THE STAGNANT DEMESNE
  HUSTR_E5M1 = 'E5M1:  OCHRE CLIFFS';
  HUSTR_E5M2 = 'E5M2:  RAPIDS';
  HUSTR_E5M3 = 'E5M3:  QUAY';
  HUSTR_E5M4 = 'E5M4:  COURTYARD';
  HUSTR_E5M5 = 'E5M5:  HYDRATYR';
  HUSTR_E5M6 = 'E5M6:  COLONNADE';
  HUSTR_E5M7 = 'E5M7:  FOETID MANSE';
  HUSTR_E5M8 = 'E5M8:  FIELD OF JUDGEMENT';
  HUSTR_E5M9 = 'E5M9:  SKEIN OF D''SPARIL';

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
  STSTR_FAADDED: string = 'Ammo (no keys) Added';

  STSTR_NCON: string = 'No Clipping Mode ON';
  STSTR_NCOFF: string = 'No Clipping Mode OFF';

  STSTR_CHOPPERS: string = '... doesn''t suck - GM';
  STSTR_CLEV: string = 'Changing Level...';

  STSTR_WLEV: string = 'Level specified not found';

//
// f_finale
//
  E1TEXT: string =
    'with the destruction of the iron' + #13#10 +
    'liches and their minions, the last' + #13#10 +
    'of the undead are cleared from this' + #13#10 +
    'plane of existence.' + #13#10 + #13#10 +
    'those creatures had to come from' + #13#10 +
    'somewhere, though, and you have the' + #13#10 +
    'sneaky suspicion that the fiery' + #13#10 +
    'portal of hell''s maw opens onto' + #13#10 +
    'their home dimension.' + #13#10 + #13#10 +
    'to make sure that more undead' + #13#10 +
    '(or even worse things) don''t come' + #13#10 +
    'through, you''ll have to seal hell''s' + #13#10 +
    'maw from the other side. of course' + #13#10 +
    'this means you may get stuck in a' + #13#10 +
    'very unfriendly world, but no one' + #13#10 +
    'ever said being a Heretic was easy!';

  E2TEXT: string =
    'the mighty maulotaurs have proved' + #13#10 +
    'to be no match for you, and as' + #13#10 +
    'their steaming corpses slide to the' + #13#10 +
    'ground you feel a sense of grim' + #13#10 +
    'satisfaction that they have been' + #13#10 +
    'destroyed.' + #13#10 + #13#10 +
    'the gateways which they guarded' + #13#10 +
    'have opened, revealing what you' + #13#10 +
    'hope is the way home. but as you' + #13#10 +
    'step through, mocking laughter' + #13#10 +
    'rings in your ears.' + #13#10 + #13#10 +
    'was some other force controlling' + #13#10 +
    'the maulotaurs? could there be even' + #13#10 +
    'more horrific beings through this' + #13#10 +
    'gate? the sweep of a crystal dome' + #13#10 +
    'overhead where the sky should be is' + #13#10 +
    'certainly not a good sign....';

  E3TEXT: string =
    'the death of d''sparil has loosed' + #13#10 +
    'the magical bonds holding his' + #13#10 +
    'creatures on this plane, their' + #13#10 +
    'dying screams overwhelming his own' + #13#10 +
    'cries of agony.' + #13#10 + #13#10 +
    'your oath of vengeance fulfilled,' + #13#10 +
    'you enter the portal to your own' + #13#10 +
    'world, mere moments before the dome' + #13#10 +
    'shatters into a million pieces.' + #13#10 + #13#10 +
    'but if d''sparil''s power is broken' + #13#10 +
    'forever, why don''t you feel safe?' + #13#10 +
    'was it that last shout just before' + #13#10 +
    'his death, the one that sounded' + #13#10 +
    'like a curse? or a summoning? you' + #13#10 +
    'can''t really be sure, but it might' + #13#10 +
    'just have been a scream.' + #13#10 + #13#10 +
    'then again, what about the other' + #13#10 +
    'serpent riders?';

  E4TEXT: string =
    'you thought you would return to your' + #13#10 +
    'own world after d''sparil died, but' + #13#10 +
    'his final act banished you to his' + #13#10 +
    'own plane. here you entered the' + #13#10 +
    'shattered remnants of lands' + #13#10 +
    'conquered by d''sparil. you defeated' + #13#10 +
    'the last guardians of these lands,' + #13#10 +
    'but now you stand before the gates' + #13#10 +
    'to d''sparil''s stronghold. until this' + #13#10 +
    'moment you had no doubts about your' + #13#10 +
    'ability to face anything you might' + #13#10 +
    'encounter, but beyond this portal' + #13#10 +
    'lies the very heart of the evil' + #13#10 +
    'which invaded your world. d''sparil' + #13#10 +
    'might be dead, but the pit where he' + #13#10 +
    'was spawned remains. now you must' + #13#10 +
    'enter that pit in the hopes of' + #13#10 +
    'finding a way out. and somewhere,' + #13#10 +
    'in the darkest corner of d''sparil''s' + #13#10 +
    'demesne, his personal bodyguards' + #13#10 +
    'await your arrival ...';

  E5TEXT: string =
    'as the final maulotaur bellows his' + #13#10 +
    'death-agony, you realize that you' + #13#10 +
    'have never come so close to your own' + #13#10 +
    'destruction. not even the fight with' + #13#10 +
    'd''sparil and his disciples had been' + #13#10 +
    'this desperate. grimly you stare at' + #13#10 +
    'the gates which open before you,' + #13#10 +
    'wondering if they lead home, or if' + #13#10 +
    'they open onto some undreamed-of' + #13#10 +
    'horror. you find yourself wondering' + #13#10 +
    'if you have the strength to go on,' + #13#10 +
    'if nothing but death and pain await' + #13#10 +
    'you. but what else can you do, if' + #13#10 +
    'the will to fight is gone? can you' + #13#10 +
    'force yourself to continue in the' + #13#10 +
    'face of such despair? do you have' + #13#10 +
    'the courage? you find, in the end,' + #13#10 +
    'that it is not within you to' + #13#10 +
    'surrender without a fight. eyes' + #13#10 +
    'wide, you go to meet your fate.';

var
  MSG_MODIFIEDGAME: string =
      '===========================================================================' + #13#10 +
      'ATTENTION:  This version of HERETIC has been modified.' + #13#10 +
      '===========================================================================' + #13#10;

  MSG_SHAREWARE: string =
        '===========================================================================' + #13#10 +
        '                                Shareware!' + #13#10 +
        '===========================================================================' + #13#10;

  MSG_COMMERCIAL: string =
        '===========================================================================' + #13#10 +
        '                 Commercial product - do not distribute!' + #13#10 +
        '         Please report software piracy to the SPA: 1-800-388-PIR8' + #13#10 +
        '===========================================================================' + #13#10;

  MSG_UNDETERMINED: string =
        '===========================================================================' + #13#10 +
        '                       Undetermined version! (Ouch)' + #13#10 +
        '===========================================================================' + #13#10;

var
  startmsg: array[0..NUM_STARTUPMESSAGES - 1] of string;

implementation

initialization

  startmsg[0] := '';
  startmsg[1] := '';
  startmsg[2] := '';
  startmsg[3] := '';
  startmsg[4] := '';

end.

