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
    'CD-ROM Version: Doom32.ini from c:\doomdata' + #13#10;

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
    'this is the shareware version of doom.' + #13#10 +
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
// JVAL: 7/12/2007 Correctly display the amound of picked-up shells
  GOTONESHELL: string = 'Picked up a shotgun shell.';
  GOTMANYSHELLS: string = 'Picked up %d shotgun shells.';

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
  PD_BLUEO: string = 'You need a blue key to activate this object';
  PD_REDO: string = 'You need a red key to activate this object';
  PD_YELLOWO: string = 'You need a yellow key to activate this object';
  PD_BLUEK: string = 'You need a blue key to open this door';
  PD_REDK: string = 'You need a red key to open this door';
  PD_YELLOWK: string = 'You need a yellow key to open this door';
//jff 02/05/98 Create messages specific to card and skull keys
  PD_BLUEC: string = 'You need a blue card to open this door';
  PD_REDC: string = 'You need a red card to open this door';
  PD_YELLOWC: string = 'You need a yellow card to open this door';
  PD_BLUES: string = 'You need a blue skull to open this door';
  PD_REDS: string = 'You need a red skull to open this door';
  PD_YELLOWS: string = 'You need a yellow skull to open this door';
  PD_ANY: string = 'Any key will open this door';
  PD_ALL3: string = 'You need all three keys to open this door';
  PD_ALL6: string = 'You need all six keys to open this door';

//
// G_game.C
//
var
  GGSAVED: string = 'game saved.';

const
//
//  HU_stuff.C
//

  HUSTR_E1M1 = 'E1M1: Hangar';
  HUSTR_E1M2 = 'E1M2: Nuclear Plant';
  HUSTR_E1M3 = 'E1M3: Toxin Refinery';
  HUSTR_E1M4 = 'E1M4: Command Control';
  HUSTR_E1M5 = 'E1M5: Phobos Lab';
  HUSTR_E1M6 = 'E1M6: Central Processing';
  HUSTR_E1M7 = 'E1M7: Computer Station';
  HUSTR_E1M8 = 'E1M8: Phobos Anomaly';
  HUSTR_E1M9 = 'E1M9: Military Base';

  HUSTR_E2M1 = 'E2M1: Deimos Anomaly';
  HUSTR_E2M2 = 'E2M2: Containment Area';
  HUSTR_E2M3 = 'E2M3: Refinery';
  HUSTR_E2M4 = 'E2M4: Deimos Lab';
  HUSTR_E2M5 = 'E2M5: Command Center';
  HUSTR_E2M6 = 'E2M6: Halls of the Damned';
  HUSTR_E2M7 = 'E2M7: Spawning Vats';
  HUSTR_E2M8 = 'E2M8: Tower of Babel';
  HUSTR_E2M9 = 'E2M9: Fortress of Mystery';

  HUSTR_E3M1 = 'E3M1: Hell Keep';
  HUSTR_E3M2 = 'E3M2: Slough of Despair';
  HUSTR_E3M3 = 'E3M3: Pandemonium';
  HUSTR_E3M4 = 'E3M4: House of Pain';
  HUSTR_E3M5 = 'E3M5: Unholy Cathedral';
  HUSTR_E3M6 = 'E3M6: Mt. Erebus';
  HUSTR_E3M7 = 'E3M7: Limbo';
  HUSTR_E3M8 = 'E3M8: Dis';
  HUSTR_E3M9 = 'E3M9: Warrens';

  HUSTR_E4M1 = 'E4M1: Hell Beneath';
  HUSTR_E4M2 = 'E4M2: Perfect Hatred';
  HUSTR_E4M3 = 'E4M3: Sever The Wicked';
  HUSTR_E4M4 = 'E4M4: Unruly Evil';
  HUSTR_E4M5 = 'E4M5: They Will Repent';
  HUSTR_E4M6 = 'E4M6: Against Thee Wickedly';
  HUSTR_E4M7 = 'E4M7: And Hell Followed';
  HUSTR_E4M8 = 'E4M8: Unto The Cruel';
  HUSTR_E4M9 = 'E4M9: Fear';

  HUSTR_1 = 'level 1: entryway';
  HUSTR_2 = 'level 2: underhalls';
  HUSTR_3 = 'level 3: the gantlet';
  HUSTR_4 = 'level 4: the focus';
  HUSTR_5 = 'level 5: the waste tunnels';
  HUSTR_6 = 'level 6: the crusher';
  HUSTR_7 = 'level 7: dead simple';
  HUSTR_8 = 'level 8: tricks and traps';
  HUSTR_9 = 'level 9: the pit';
  HUSTR_10 = 'level 10: refueling base';
  HUSTR_11 = 'level 11: ''o'' of destruction!';

  HUSTR_12 = 'level 12: the factory';
  HUSTR_13 = 'level 13: downtown';
  HUSTR_14 = 'level 14: the inmost dens';
  HUSTR_15 = 'level 15: industrial zone';
  HUSTR_16 = 'level 16: suburbs';
  HUSTR_17 = 'level 17: tenements';
  HUSTR_18 = 'level 18: the courtyard';
  HUSTR_19 = 'level 19: the citadel';
  HUSTR_20 = 'level 20: gotcha!';

  HUSTR_21 = 'level 21: nirvana';
  HUSTR_22 = 'level 22: the catacombs';
  HUSTR_23 = 'level 23: barrels o'' fun';
  HUSTR_24 = 'level 24: the chasm';
  HUSTR_25 = 'level 25: bloodfalls';
  HUSTR_26 = 'level 26: the abandoned mines';
  HUSTR_27 = 'level 27: monster condo';
  HUSTR_28 = 'level 28: the spirit world';
  HUSTR_29 = 'level 29: the living end';
  HUSTR_30 = 'level 30: icon of sin';

  HUSTR_31 = 'level 31: wolfenstein';
  HUSTR_32 = 'level 32: grosse';
  HUSTR_33 = 'level 33: betray';

  PHUSTR_1 = 'level 1: congo';
  PHUSTR_2 = 'level 2: well of souls';
  PHUSTR_3 = 'level 3: aztec';
  PHUSTR_4 = 'level 4: caged';
  PHUSTR_5 = 'level 5: ghost town';
  PHUSTR_6 = 'level 6: baron''s lair';
  PHUSTR_7 = 'level 7: caughtyard';
  PHUSTR_8 = 'level 8: realm';
  PHUSTR_9 = 'level 9: abattoire';
  PHUSTR_10 = 'level 10: onslaught';
  PHUSTR_11 = 'level 11: hunted';

  PHUSTR_12 = 'level 12: speed';
  PHUSTR_13 = 'level 13: the crypt';
  PHUSTR_14 = 'level 14: genesis';
  PHUSTR_15 = 'level 15: the twilight';
  PHUSTR_16 = 'level 16: the omen';
  PHUSTR_17 = 'level 17: compound';
  PHUSTR_18 = 'level 18: neurosphere';
  PHUSTR_19 = 'level 19: nme';
  PHUSTR_20 = 'level 20: the death domain';

  PHUSTR_21 = 'level 21: slayer';
  PHUSTR_22 = 'level 22: impossible mission';
  PHUSTR_23 = 'level 23: tombstone';
  PHUSTR_24 = 'level 24: the final frontier';
  PHUSTR_25 = 'level 25: the temple of darkness';
  PHUSTR_26 = 'level 26: bunker';
  PHUSTR_27 = 'level 27: anti-christ';
  PHUSTR_28 = 'level 28: the sewers';
  PHUSTR_29 = 'level 29: odyssey of noises';
  PHUSTR_30 = 'level 30: the gateway of hell';

  PHUSTR_31 = 'level 31: cyberden';
  PHUSTR_32 = 'level 32: go 2 it';

  THUSTR_1 = 'level 1: system control';
  THUSTR_2 = 'level 2: human bbq';
  THUSTR_3 = 'level 3: power control';
  THUSTR_4 = 'level 4: wormhole';
  THUSTR_5 = 'level 5: hanger';
  THUSTR_6 = 'level 6: open season';
  THUSTR_7 = 'level 7: prison';
  THUSTR_8 = 'level 8: metal';
  THUSTR_9 = 'level 9: stronghold';
  THUSTR_10 = 'level 10: redemption';
  THUSTR_11 = 'level 11: storage facility';

  THUSTR_12 = 'level 12: crater';
  THUSTR_13 = 'level 13: nukage processing';
  THUSTR_14 = 'level 14: steel works';
  THUSTR_15 = 'level 15: dead zone';
  THUSTR_16 = 'level 16: deepest reaches';
  THUSTR_17 = 'level 17: processing area';
  THUSTR_18 = 'level 18: mill';
  THUSTR_19 = 'level 19: shipping/respawning';
  THUSTR_20 = 'level 20: central processing';

  THUSTR_21 = 'level 21: administration center';
  THUSTR_22 = 'level 22: habitat';
  THUSTR_23 = 'level 23: lunar mining project';
  THUSTR_24 = 'level 24: quarry';
  THUSTR_25 = 'level 25: baron''s den';
  THUSTR_26 = 'level 26: ballistyx';
  THUSTR_27 = 'level 27: mount pain';
  THUSTR_28 = 'level 28: heck';
  THUSTR_29 = 'level 29: river styx';
  THUSTR_30 = 'level 30: last call';

  THUSTR_31 = 'level 31: pharaoh';
  THUSTR_32 = 'level 32: caribbean';

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
//  ST_stuff.C
//
  STSTR_MUS: string = 'Music Change';
  STSTR_NOMUS: string = 'IMPOSSIBLE SELECTION';
  STSTR_DQDON: string = 'Degreelessness Mode On';
  STSTR_DQDOFF: string = 'Degreelessness Mode Off';
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

//
// F_Finale.C
//
  E1TEXT: string =
    'Once you beat the big badasses and' + #13#10 +
    'clean out the moon base you''re supposed' + #13#10 +
    'to win, aren''t you? Aren''t you? Where''s' + #13#10 +
    'your fat reward and ticket home? What' + #13#10 +
    'the hell is this? It''s not supposed to' + #13#10 +
    'end this way!' + #13#10 +
    ' ' + #13#10 +
    'It stinks like rotten meat, but looks' + #13#10 +
    'like the lost Deimos base.  Looks like' + #13#10 +
    'you''re stuck on The Shores of Hell.' + #13#10 +
    'The only way out is through.' + #13#10 +
    ' ' + #13#10 +
    'To continue the DOOM experience, play' + #13#10 +
    'The Shores of Hell and its amazing' + #13#10 +
    'sequel, Inferno!' + #13#10;

  E2TEXT: string =
    'You''ve done it! The hideous cyber-' + #13#10 +
    'demon lord that ruled the lost Deimos' + #13#10 +
    'moon base has been slain and you' + #13#10 +
    'are triumphant! But ... where are' + #13#10 +
    'you? You clamber to the edge of the' + #13#10 +
    'moon and look down to see the awful' + #13#10 +
    'truth.' + #13#10 +
    ' ' + #13#10 +
    'Deimos floats above Hell itself!' + #13#10 +
    'You''ve never heard of anyone escaping' + #13#10 +
    'from Hell, but you''ll make the bastards' + #13#10 +
    'sorry they ever heard of you! Quickly,' + #13#10 +
    'you rappel down to  the surface of' + #13#10 +
    'Hell.' + #13#10 +
    ' ' + #13#10 +
    'Now, it''s on to the final chapter of' + #13#10 +
    'DOOM! -- Inferno.';

  E3TEXT: string =
    'The loathsome spiderdemon that' + #13#10 +
    'masterminded the invasion of the moon' + #13#10 +
    'bases and caused so much death has had' + #13#10 +
    'its ass kicked for all time.' + #13#10 +
    '' + #13#10 +
    'A hidden doorway opens and you enter.' + #13#10 +
    'You''ve proven too tough for Hell to' + #13#10 +
    'contain, and now Hell at last plays' + #13#10 +
    'fair -- for you emerge from the door' + #13#10 +
    'to see the green fields of Earth!' + #13#10 +
    'Home at last.' + #13#10 +
    ' ' + #13#10 +
    'You wonder what''s been happening on' + #13#10 +
    'Earth while you were battling evil' + #13#10 +
    'unleashed. It''s good that no Hell-' + #13#10 +
    'spawn could have come through that' + #13#10 +
    'door with you ...';

  E4TEXT: string =
    'the spider mastermind must have sent forth' + #13#10 +
    'its legions of hellspawn before your' + #13#10 +
    'final confrontation with that terrible' + #13#10 +
    'beast from hell.  but you stepped forward' + #13#10 +
    'and brought forth eternal damnation and' + #13#10 +
    'suffering upon the horde as a true hero' + #13#10 +
    'would in the face of something so evil.' + #13#10 +
    ' ' + #13#10 +
    'besides, someone was gonna pay for what' + #13#10 +
    'happened to daisy, your pet rabbit.' + #13#10 +
    ' ' + #13#10 +
    'but now, you see spread before you more' + #13#10 +
    'potential pain and gibbitude as a nation' + #13#10 +
    'of demons run amok among our cities.' + #13#10 +
    ' ' + #13#10 +
    'next stop, hell on earth!';

// after level 6, put this:

  C1TEXT: string =
    'YOU HAVE ENTERED DEEPLY INTO THE INFESTED' + #13#10 +
    'STARPORT. BUT SOMETHING IS WRONG. THE' + #13#10 +
    'MONSTERS HAVE BROUGHT THEIR OWN REALITY' + #13#10 +
    'WITH THEM, AND THE STARPORT''S TECHNOLOGY' + #13#10 +
    'IS BEING SUBVERTED BY THEIR PRESENCE.' + #13#10 +
    ' ' + #13#10 +
    'AHEAD, YOU SEE AN OUTPOST OF HELL, A' + #13#10 +
    'FORTIFIED ZONE. IF YOU CAN GET PAST IT,' + #13#10 +
    'YOU CAN PENETRATE INTO THE HAUNTED HEART' + #13#10 +
    'OF THE STARBASE AND FIND THE CONTROLLING' + #13#10 +
    'SWITCH WHICH HOLDS EARTH''S POPULATION' + #13#10 +
    'HOSTAGE.';

// After level 11, put this:

  C2TEXT: string =
    'YOU HAVE WON! YOUR VICTORY HAS ENABLED' + #13#10 +
    'HUMANKIND TO EVACUATE EARTH AND ESCAPE' + #13#10 +
    'THE NIGHTMARE.  NOW YOU ARE THE ONLY' + #13#10 +
    'HUMAN LEFT ON THE FACE OF THE PLANET.' + #13#10 +
    'CANNIBAL MUTATIONS, CARNIVOROUS ALIENS,' + #13#10 +
    'AND EVIL SPIRITS ARE YOUR ONLY NEIGHBORS.' + #13#10 +
    'YOU SIT BACK AND WAIT FOR DEATH, CONTENT' + #13#10 +
    'THAT YOU HAVE SAVED YOUR SPECIES.' + #13#10 +
    ' ' + #13#10 +
    'BUT THEN, EARTH CONTROL BEAMS DOWN A' + #13#10 +
    'MESSAGE FROM SPACE: ''SENSORS HAVE LOCATED' + #13#10 +
    'THE SOURCE OF THE ALIEN INVASION. IF YOU' + #13#10 +
    'GO THERE, YOU MAY BE ABLE TO BLOCK THEIR' + #13#10 +
    'ENTRY.  THE ALIEN BASE IS IN THE HEART OF' + #13#10 +
    'YOUR OWN HOME CITY, NOT FAR FROM THE' + #13#10 +
    'STARPORT.'' SLOWLY AND PAINFULLY YOU GET' + #13#10 +
    'UP AND RETURN TO THE FRAY.';

// After level 20, put this:

  C3TEXT: string =
    'YOU ARE AT THE CORRUPT HEART OF THE CITY,' + #13#10 +
    'SURROUNDED BY THE CORPSES OF YOUR ENEMIES.' + #13#10 +
    'YOU SEE NO WAY TO DESTROY THE CREATURES' + #13#10 +
    'ENTRYWAY ON THIS SIDE, SO YOU CLENCH YOUR' + #13#10 +
    'TEETH AND PLUNGE THROUGH IT.' + #13#10 +
    ' ' + #13#10 +
    'THERE MUST BE A WAY TO CLOSE IT ON THE' + #13#10 +
    'OTHER SIDE. WHAT DO YOU CARE IF YOU''VE' + #13#10 +
    'GOT TO GO THROUGH HELL TO GET TO IT?';

// After level 29, put this:

  C4TEXT: string =
    'THE HORRENDOUS VISAGE OF THE BIGGEST' + #13#10 +
    'DEMON YOU''VE EVER SEEN CRUMBLES BEFORE' + #13#10 +
    'YOU, AFTER YOU PUMP YOUR ROCKETS INTO' + #13#10 +
    'HIS EXPOSED BRAIN. THE MONSTER SHRIVELS' + #13#10 +
    'UP AND DIES, ITS THRASHING LIMBS' + #13#10 +
    'DEVASTATING UNTOLD MILES OF HELL''S' + #13#10 +
    'SURFACE.' + #13#10 +
    ' ' + #13#10 +
    'YOU''VE DONE IT. THE INVASION IS OVER.' + #13#10 +
    'EARTH IS SAVED. HELL IS A WRECK. YOU' + #13#10 +
    'WONDER WHERE BAD FOLKS WILL GO WHEN THEY' + #13#10 +
    'DIE, NOW. WIPING THE SWEAT FROM YOUR' + #13#10 +
    'FOREHEAD YOU BEGIN THE LONG TREK BACK' + #13#10 +
    'HOME. REBUILDING EARTH OUGHT TO BE A' + #13#10 +
    'LOT MORE FUN THAN RUINING IT WAS.' + #13#10;

// Before level 31, put this:

  C5TEXT: string =
    'CONGRATULATIONS, YOU''VE FOUND THE SECRET' + #13#10 +
    'LEVEL! LOOKS LIKE IT''S BEEN BUILT BY' + #13#10 +
    'HUMANS, RATHER THAN DEMONS. YOU WONDER' + #13#10 +
    'WHO THE INMATES OF THIS CORNER OF HELL' + #13#10 +
    'WILL BE.';

// Before level 32, put this:

  C6TEXT: string =
    'CONGRATULATIONS, YOU''VE FOUND THE' + #13#10 +
    'SUPER SECRET LEVEL!  YOU''D BETTER' + #13#10 +
    'BLAZE THROUGH THIS ONE!' + #13#10;

// after map 06

  P1TEXT: string =
    'You gloat over the steaming carcass of the' + #13#10 +
    'Guardian.  With its death, you''ve wrested' + #13#10 +
    'the Accelerator from the stinking claws' + #13#10 +
    'of Hell.  You relax and glance around the' + #13#10 +
    'room.  Damn!  There was supposed to be at' + #13#10 +
    'least one working prototype, but you can''t' + #13#10 +
    'see it. The demons must have taken it.' + #13#10 +
    ' ' + #13#10 +
    'You must find the prototype, or all your' + #13#10 +
    'struggles will have been wasted. Keep' + #13#10 +
    'moving, keep fighting, keep killing.' + #13#10 +
    'Oh yes, keep living, too.';

// after map 11

  P2TEXT: string =
    'Even the deadly Arch-Vile labyrinth could' + #13#10 +
    'not stop you, and you''ve gotten to the' + #13#10 +
    'prototype Accelerator which is soon' + #13#10 +
    'efficiently and permanently deactivated.' + #13#10 +
    ' ' + #13#10 +
    'You''re good at that kind of thing.';

// after map 20

  P3TEXT: string =
    'You''ve bashed and battered your way into' + #13#10 +
    'the heart of the devil-hive.  Time for a' + #13#10 +
    'Search-and-Destroy mission, aimed at the' + #13#10 +
    'Gatekeeper, whose foul offspring is' + #13#10 +
    'cascading to Earth.  Yeah, he''s bad. But' + #13#10 +
    'you know who''s worse!' + #13#10 +
    ' ' + #13#10 +
    'Grinning evilly, you check your gear, and' + #13#10 +
    'get ready to give the bastard a little Hell' + #13#10 +
    'of your own making!';

// after map 30

  P4TEXT: string =
    'The Gatekeeper''s evil face is splattered' + #13#10 +
    'all over the place.  As its tattered corpse' + #13#10 +
    'collapses, an inverted Gate forms and' + #13#10 +
    'sucks down the shards of the last' + #13#10 +
    'prototype Accelerator, not to mention the' + #13#10 +
    'few remaining demons.  You''re done. Hell' + #13#10 +
    'has gone back to pounding bad dead folks ' + #13#10 +
    'instead of good live ones.  Remember to' + #13#10 +
    'tell your grandkids to put a rocket' + #13#10 +
    'launcher in your coffin. If you go to Hell' + #13#10 +
    'when you die, you''ll need it for some' + #13#10 +
    'final cleaning-up ...';

// before map 31

  P5TEXT: string =
    'You''ve found the second-hardest level we' + #13#10 +
    'got. Hope you have a saved game a level or' + #13#10 +
    'two previous.  If not, be prepared to die' + #13#10 +
    'aplenty. For master marines only.';

// before map 32

  P6TEXT: string =
    'Betcha wondered just what WAS the hardest' + #13#10 +
    'level we had ready for ya?  Now you know.' + #13#10 +
    'No one gets out alive.';

  T1TEXT: string =
    'You''ve fought your way out of the infested' + #13#10 +
    'experimental labs.   It seems that UAC has' + #13#10 +
    'once again gulped it down.  With their' + #13#10 +
    'high turnover, it must be hard for poor' + #13#10 +
    'old UAC to buy corporate health insurance' + #13#10 +
    'nowadays..' + #13#10 +
    ' ' + #13#10 +
    'Ahead lies the military complex, now' + #13#10 +
    'swarming with diseased horrors hot to get' + #13#10 +
    'their teeth into you. With luck, the' + #13#10 +
    'complex still has some warlike ordnance' + #13#10 +
    'laying around.';

  T2TEXT: string =
    'You hear the grinding of heavy machinery' + #13#10 +
    'ahead.  You sure hope they''re not stamping' + #13#10 +
    'out new hellspawn, but you''re ready to' + #13#10 +
    'ream out a whole herd if you have to.' + #13#10 +
    'They might be planning a blood feast, but' + #13#10 +
    'you feel about as mean as two thousand' + #13#10 +
    'maniacs packed into one mad killer.' + #13#10 +
    ' ' + #13#10 +
    'You don''t plan to go down easy.';

  T3TEXT: string =
    'The vista opening ahead looks real damn' + #13#10 +
    'familiar. Smells familiar, too -- like' + #13#10 +
    'fried excrement. You didn''t like this' + #13#10 +
    'place before, and you sure as hell ain''t' + #13#10 +
    'planning to like it now. The more you' + #13#10 +
    'brood on it, the madder you get.' + #13#10 +
    'Hefting your gun, an evil grin trickles' + #13#10 +
    'onto your face. Time to take some names.';

  T4TEXT: string =
    'Suddenly, all is silent, from one horizon' + #13#10 +
    'to the other. The agonizing echo of Hell' + #13#10 +
    'fades away, the nightmare sky turns to' + #13#10 +
    'blue, the heaps of monster corpses start ' + #13#10 +
    'to evaporate along with the evil stench ' + #13#10 +
    'that filled the air. Jeeze, maybe you''ve' + #13#10 +
    'done it. Have you really won?' + #13#10 +
    ' ' + #13#10 +
    'Something rumbles in the distance.' + #13#10 +
    'A blue light begins to glow inside the' + #13#10 +
    'ruined skull of the demon-spitter.';

  T5TEXT: string =
    'What now? Looks totally different. Kind' + #13#10 +
    'of like King Tut''s condo. Well,' + #13#10 +
    'whatever''s here can''t be any worse' + #13#10 +
    'than usual. Can it?  Or maybe it''s best' + #13#10 +
    'to let sleeping gods lie..';

  T6TEXT: string =
    'Time for a vacation. You''ve burst the' + #13#10 +
    'bowels of hell and by golly you''re ready' + #13#10 +
    'for a break. You mutter to yourself,' + #13#10 +
    'Maybe someone else can kick Hell''s ass' + #13#10 +
    'next time around. Ahead lies a quiet town,' + #13#10 +
    'with peaceful flowing water, quaint' + #13#10 +
    'buildings, and presumably no Hellspawn.' + #13#10 +
    ' ' + #13#10 +
    'As you step off the transport, you hear' + #13#10 +
    'the stomp of a cyberdemon''s iron shoe.';

const
//
// Character cast strings F_FINALE.C
//
  CC_ZOMBIE  = 'ZOMBIEMAN';
  CC_SHOTGUN = 'SHOTGUN GUY';
  CC_HEAVY = 'HEAVY WEAPON DUDE';
  CC_IMP = 'IMP';
  CC_DEMON = 'DEMON';
  CC_LOST = 'LOST SOUL';
  CC_CACO = 'CACODEMON';
  CC_HELL = 'HELL KNIGHT';
  CC_BARON = 'BARON OF HELL';
  CC_ARACH = 'ARACHNOTRON';
  CC_PAIN = 'PAIN ELEMENTAL';
  CC_REVEN = 'REVENANT';
  CC_MANCU = 'MANCUBUS';
  CC_ARCH = 'ARCH-VILE';
  CC_SPIDER = 'THE SPIDER MASTERMIND';
  CC_CYBER = 'THE CYBERDEMON';
  CC_HERO = 'OUR HERO';

var
  MSG_MODIFIEDGAME: string =
      '===========================================================================' + #13#10 +
      'ATTENTION:  This version of DOOM has been modified.  If you would like to' + #13#10 +
      'get a copy of the original game, call 1-800-IDGAMES or see the readme file.' + #13#10 +
      '        You will not receive technical support for modified games.' + #13#10 +
      '                      press enter to continue' + #13#10 +
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

implementation

end.

