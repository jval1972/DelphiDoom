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
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit udmf_things;

interface

uses
  d_delphi,
  info_h;

const
{$IFDEF DOOM}
  TranslateThingType: array[0..143] of mobjtype_t = (
    MT_NONE, // T_NONE
    MT_POSSESSED, // T_POSSESSED
    MT_SHOTGUY, // T_SHOTGUY
    MT_VILE, // T_VILE
    MT_FIRE, // T_FIRE
    MT_UNDEAD, // T_UNDEAD
    MT_TRACER, // T_TRACER
    MT_SMOKE, // T_SMOKE
    MT_FATSO, // T_FATSO
    MT_FATSHOT, // T_FATSHOT
    MT_CHAINGUY, // T_CHAINGUY
    MT_TROOP, // T_TROOP
    MT_SERGEANT, // T_SERGEANT
    MT_SHADOWS, // T_SHADOWS
    MT_HEAD, // T_HEAD
    MT_BRUISER, // T_BRUISER
    MT_BRUISERSHOT, // T_BRUISERSHOT
    MT_KNIGHT, // T_KNIGHT
    MT_SKULL, // T_SKULL
    MT_SPIDER, // T_SPIDER
    MT_BABY, // T_BABY
    MT_CYBORG, // T_CYBORG
    MT_PAIN, // T_PAIN
    MT_WOLFSS, // T_WOLFSS
    MT_KEEN, // T_KEEN
    MT_BOSSBRAIN, // T_BOSSBRAIN
    MT_BOSSSPIT, // T_BOSSSPIT
    MT_BOSSTARGET, // T_BOSSTARGET
    MT_SPAWNSHOT, // T_SPAWNSHOT
    MT_SPAWNFIRE, // T_SPAWNFIRE
    MT_BARREL, // T_BARREL
    MT_TROOPSHOT, // T_TROOPSHOT
    MT_HEADSHOT, // T_HEADSHOT
    MT_ROCKET, // T_ROCKET
    MT_PLASMA, // T_PLASMA
    MT_BFG, // T_BFG
    MT_ARACHPLAZ, // T_ARACHPLAZ
    MT_PUFF, // T_PUFF
    MT_BLOOD, // T_BLOOD
    MT_TFOG, // T_TFOG
    MT_IFOG, // T_IFOG
    MT_TELEPORTMAN, // T_TELEPORTMAN
    MT_EXTRABFG, // T_EXTRABFG
    MT_MISC0, // T_MISC0
    MT_MISC1, // T_MISC1
    MT_MISC2, // T_MISC2
    MT_MISC3, // T_MISC3
    MT_MISC4, // T_MISC4
    MT_MISC5, // T_MISC5
    MT_MISC6, // T_MISC6
    MT_MISC7, // T_MISC7
    MT_MISC8, // T_MISC8
    MT_MISC9, // T_MISC9
    MT_MISC10, // T_MISC10
    MT_MISC11, // T_MISC11
    MT_MISC12, // T_MISC12
    MT_INV, // T_INV
    MT_MISC13, // T_MISC13
    MT_INS, // T_INS
    MT_MISC14, // T_MISC14
    MT_MISC15, // T_MISC15
    MT_MISC16, // T_MISC16
    MT_MEGA, // T_MEGA
    MT_CLIP, // T_CLIP
    MT_MISC17, // T_MISC17
    MT_MISC18, // T_MISC18
    MT_MISC19, // T_MISC19
    MT_MISC20, // T_MISC20
    MT_MISC21, // T_MISC21
    MT_MISC22, // T_MISC22
    MT_MISC23, // T_MISC23
    MT_MISC24, // T_MISC24
    MT_MISC25, // T_MISC25
    MT_CHAINGUN, // T_CHAINGUN
    MT_MISC26, // T_MISC26
    MT_MISC27, // T_MISC27
    MT_MISC28, // T_MISC28
    MT_SHOTGUN, // T_SHOTGUN
    MT_SUPERSHOTGUN, // T_SUPERSHOTGUN
    MT_MISC29, // T_MISC29
    MT_MISC30, // T_MISC30
    MT_MISC31, // T_MISC31
    MT_MISC32, // T_MISC32
    MT_MISC33, // T_MISC33
    MT_MISC34, // T_MISC34
    MT_MISC35, // T_MISC35
    MT_MISC36, // T_MISC36
    MT_MISC37, // T_MISC37
    MT_MISC38, // T_MISC38
    MT_MISC39, // T_MISC39
    MT_MISC40, // T_MISC40
    MT_MISC41, // T_MISC41
    MT_MISC42, // T_MISC42
    MT_MISC43, // T_MISC43
    MT_MISC44, // T_MISC44
    MT_MISC45, // T_MISC45
    MT_MISC46, // T_MISC46
    MT_MISC47, // T_MISC47
    MT_MISC48, // T_MISC48
    MT_MISC49, // T_MISC49
    MT_MISC50, // T_MISC50
    MT_MISC51, // T_MISC51
    MT_MISC52, // T_MISC52
    MT_MISC53, // T_MISC53
    MT_MISC54, // T_MISC54
    MT_MISC55, // T_MISC55
    MT_MISC56, // T_MISC56
    MT_MISC57, // T_MISC57
    MT_MISC58, // T_MISC58
    MT_MISC59, // T_MISC59
    MT_MISC60, // T_MISC60
    MT_MISC61, // T_MISC61
    MT_MISC62, // T_MISC62
    MT_MISC63, // T_MISC63
    MT_MISC64, // T_MISC64
    MT_MISC65, // T_MISC65
    MT_MISC66, // T_MISC66
    MT_MISC67, // T_MISC67
    MT_MISC68, // T_MISC68
    MT_MISC69, // T_MISC69
    MT_MISC70, // T_MISC70
    MT_MISC71, // T_MISC71
    MT_MISC72, // T_MISC72
    MT_MISC73, // T_MISC73
    MT_MISC74, // T_MISC74
    MT_MISC75, // T_MISC75
    MT_MISC76, // T_MISC76
    MT_MISC77, // T_MISC77
    MT_MISC78, // T_MISC78
    MT_MISC79, // T_MISC79
    MT_MISC80, // T_MISC80
    MT_MISC81, // T_MISC81
    MT_MISC82, // T_MISC82
    MT_MISC83, // T_MISC83
    MT_MISC84, // T_MISC84
    MT_MISC85, // T_MISC85
    MT_MISC86, // T_MISC86
    MT_PUSH, // T_PUSH
    MT_PULL, // T_PULL
    MT_DOGS, // T_DOGS
    MT_PLASMA1, // T_PLASMA1
    MT_PLASMA2, // T_PLASMA2
    MT_SCEPTRE, // T_SCEPTRE
    MT_BIBLE // T_BIBLE
  );
{$ENDIF}

{$IFDEF HERETIC}
  TranslateThingType: array[0..159] of mobjtype_t = (
    MT_PLAYER, // T_PLAYER
    MT_MISC0, // T_MISC0
    MT_ITEMSHIELD1, // T_ITEMSHIELD1
    MT_ITEMSHIELD2, // T_ITEMSHIELD2
    MT_MISC1, // T_MISC1
    MT_MISC2, // T_MISC2
    MT_ARTIINVISIBILITY, // T_ARTIINVISIBILITY
    MT_MISC3, // T_MISC3
    MT_ARTIFLY, // T_ARTIFLY
    MT_ARTIINVULNERABILITY, // T_ARTIINVULNERABILITY
    MT_ARTITOMEOFPOWER, // T_ARTITOMEOFPOWER
    MT_ARTIEGG, // T_ARTIEGG
    MT_EGGFX, // T_EGGFX
    MT_ARTISUPERHEAL, // T_ARTISUPERHEAL
    MT_MISC4, // T_MISC4
    MT_MISC5, // T_MISC5
    MT_FIREBOMB, // T_FIREBOMB
    MT_ARTITELEPORT, // T_ARTITELEPORT
    MT_POD, // T_POD
    MT_PODGOO, // T_PODGOO
    MT_PODGENERATOR, // T_PODGENERATOR
    MT_SPLASH, // T_SPLASH
    MT_SPLASHBASE, // T_SPLASHBASE
    MT_LAVASPLASH, // T_LAVASPLASH
    MT_LAVASMOKE, // T_LAVASMOKE
    MT_SLUDGECHUNK, // T_SLUDGECHUNK
    MT_SLUDGESPLASH, // T_SLUDGESPLASH
    MT_SKULLHANG70, // T_SKULLHANG70
    MT_SKULLHANG60, // T_SKULLHANG60
    MT_SKULLHANG45, // T_SKULLHANG45
    MT_SKULLHANG35, // T_SKULLHANG35
    MT_CHANDELIER, // T_CHANDELIER
    MT_SERPTORCH, // T_SERPTORCH
    MT_SMALLPILLAR, // T_SMALLPILLAR
    MT_STALAGMITESMALL, // T_STALAGMITESMALL
    MT_STALAGMITELARGE, // T_STALAGMITELARGE
    MT_STALACTITESMALL, // T_STALACTITESMALL
    MT_STALACTITELARGE, // T_STALACTITELARGE
    MT_MISC6, // T_MISC6
    MT_BARREL, // T_BARREL
    MT_MISC7, // T_MISC7
    MT_MISC8, // T_MISC8
    MT_MISC9, // T_MISC9
    MT_MISC10, // T_MISC10
    MT_MISC11, // T_MISC11
    MT_KEYGIZMOBLUE, // T_KEYGIZMOBLUE
    MT_KEYGIZMOGREEN, // T_KEYGIZMOGREEN
    MT_KEYGIZMOYELLOW, // T_KEYGIZMOYELLOW
    MT_KEYGIZMOFLOAT, // T_KEYGIZMOFLOAT
    MT_MISC12, // T_MISC12
    MT_VOLCANOBLAST, // T_VOLCANOBLAST
    MT_VOLCANOTBLAST, // T_VOLCANOTBLAST
    MT_TELEGLITGEN, // T_TELEGLITGEN
    MT_TELEGLITGEN2, // T_TELEGLITGEN2
    MT_TELEGLITTER, // T_TELEGLITTER
    MT_TELEGLITTER2, // T_TELEGLITTER2
    MT_TFOG, // T_TFOG
    MT_TELEPORTMAN, // T_TELEPORTMAN
    MT_STAFFPUFF, // T_STAFFPUFF
    MT_STAFFPUFF2, // T_STAFFPUFF2
    MT_BEAKPUFF, // T_BEAKPUFF
    MT_MISC13, // T_MISC13
    MT_GAUNTLETPUFF1, // T_GAUNTLETPUFF1
    MT_GAUNTLETPUFF2, // T_GAUNTLETPUFF2
    MT_MISC14, // T_MISC14
    MT_BLASTERFX1, // T_BLASTERFX1
    MT_BLASTERSMOKE, // T_BLASTERSMOKE
    MT_RIPPER, // T_RIPPER
    MT_BLASTERPUFF1, // T_BLASTERPUFF1
    MT_BLASTERPUFF2, // T_BLASTERPUFF2
    MT_WMACE, // T_WMACE
    MT_MACEFX1, // T_MACEFX1
    MT_MACEFX2, // T_MACEFX2
    MT_MACEFX3, // T_MACEFX3
    MT_MACEFX4, // T_MACEFX4
    MT_WSKULLROD, // T_WSKULLROD
    MT_HORNRODFX1, // T_HORNRODFX1
    MT_HORNRODFX2, // T_HORNRODFX2
    MT_RAINPLR1, // T_RAINPLR1
    MT_RAINPLR2, // T_RAINPLR2
    MT_RAINPLR3, // T_RAINPLR3
    MT_RAINPLR4, // T_RAINPLR4
    MT_GOLDWANDFX1, // T_GOLDWANDFX1
    MT_GOLDWANDFX2, // T_GOLDWANDFX2
    MT_GOLDWANDPUFF1, // T_GOLDWANDPUFF1
    MT_GOLDWANDPUFF2, // T_GOLDWANDPUFF2
    MT_WPHOENIXROD, // T_WPHOENIXROD
    MT_PHOENIXFX1, // T_PHOENIXFX1
    MT_PHOENIXPUFF, // T_PHOENIXPUFF
    MT_PHOENIXFX2, // T_PHOENIXFX2
    MT_MISC15, // T_MISC15
    MT_CRBOWFX1, // T_CRBOWFX1
    MT_CRBOWFX2, // T_CRBOWFX2
    MT_CRBOWFX3, // T_CRBOWFX3
    MT_CRBOWFX4, // T_CRBOWFX4
    MT_BLOOD, // T_BLOOD
    MT_BLOODSPLATTER, // T_BLOODSPLATTER
    MT_BLOODYSKULL, // T_BLOODYSKULL
    MT_CHICPLAYER, // T_CHICPLAYER
    MT_CHICKEN, // T_CHICKEN
    MT_FEATHER, // T_FEATHER
    MT_MUMMY, // T_MUMMY
    MT_MUMMYLEADER, // T_MUMMYLEADER
    MT_MUMMYGHOST, // T_MUMMYGHOST
    MT_MUMMYLEADERGHOST, // T_MUMMYLEADERGHOST
    MT_MUMMYSOUL, // T_MUMMYSOUL
    MT_MUMMYFX1, // T_MUMMYFX1
    MT_BEAST, // T_BEAST
    MT_BEASTBALL, // T_BEASTBALL
    MT_BURNBALL, // T_BURNBALL
    MT_BURNBALLFB, // T_BURNBALLFB
    MT_PUFFY, // T_PUFFY
    MT_SNAKE, // T_SNAKE
    MT_SNAKEPRO_A, // T_SNAKEPRO_A
    MT_SNAKEPRO_B, // T_SNAKEPRO_B
    MT_HEAD, // T_HEAD
    MT_HEADFX1, // T_HEADFX1
    MT_HEADFX2, // T_HEADFX2
    MT_HEADFX3, // T_HEADFX3
    MT_WHIRLWIND, // T_WHIRLWIND
    MT_CLINK, // T_CLINK
    MT_WIZARD, // T_WIZARD
    MT_WIZFX1, // T_WIZFX1
    MT_IMP, // T_IMP
    MT_IMPLEADER, // T_IMPLEADER
    MT_IMPCHUNK1, // T_IMPCHUNK1
    MT_IMPCHUNK2, // T_IMPCHUNK2
    MT_IMPBALL, // T_IMPBALL
    MT_KNIGHT, // T_KNIGHT
    MT_KNIGHTGHOST, // T_KNIGHTGHOST
    MT_KNIGHTAXE, // T_KNIGHTAXE
    MT_REDAXE, // T_REDAXE
    MT_SORCERER1, // T_SORCERER1
    MT_SRCRFX1, // T_SRCRFX1
    MT_SORCERER2, // T_SORCERER2
    MT_SOR2FX1, // T_SOR2FX1
    MT_SOR2FXSPARK, // T_SOR2FXSPARK
    MT_SOR2FX2, // T_SOR2FX2
    MT_SOR2TELEFADE, // T_SOR2TELEFADE
    MT_MINOTAUR, // T_MINOTAUR
    MT_MNTRFX1, // T_MNTRFX1
    MT_MNTRFX2, // T_MNTRFX2
    MT_MNTRFX3, // T_MNTRFX3
    MT_AKYY, // T_AKYY
    MT_BKYY, // T_BKYY
    MT_CKEY, // T_CKEY
    MT_AMGWNDWIMPY, // T_AMGWNDWIMPY
    MT_AMGWNDHEFTY, // T_AMGWNDHEFTY
    MT_AMMACEWIMPY, // T_AMMACEWIMPY
    MT_AMMACEHEFTY, // T_AMMACEHEFTY
    MT_AMCBOWWIMPY, // T_AMCBOWWIMPY
    MT_AMCBOWHEFTY, // T_AMCBOWHEFTY
    MT_AMSKRDWIMPY, // T_AMSKRDWIMPY
    MT_AMSKRDHEFTY, // T_AMSKRDHEFTY
    MT_AMPHRDWIMPY, // T_AMPHRDWIMPY
    MT_AMPHRDHEFTY, // T_AMPHRDHEFTY
    MT_AMBLSRWIMPY, // T_AMBLSRWIMPY
    MT_AMBLSRHEFTY, // T_AMBLSRHEFTY
    MT_SOUNDWIND, // T_SOUNDWIND
    MT_SOUNDWATERFALL // T_SOUNDWATERFALL
  );
{$ENDIF}

{$IFDEF STRIFE}
  TranslateThingType: array[0..251] of mobjtype_t = (
    MT_PLAYER, // T_PLAYER
    MT_FIELDGUARD, // T_FIELDGUARD
    MT_ZOMBIE, // T_ZOMBIE
    MT_BECOMING, // T_BECOMING
    MT_ZOMBIESPAWNER, // T_ZOMBIESPAWNER
    MT_HUGE_TANK_1, // T_HUGE_TANK_1
    MT_HUGE_TANK_2, // T_HUGE_TANK_2
    MT_HUGE_TANK_3, // T_HUGE_TANK_3
    MT_TANK_4, // T_TANK_4
    MT_TANK_5, // T_TANK_5
    MT_TANK_6, // T_TANK_6
    MT_KNEELING_GUY, // T_KNEELING_GUY
    MT_RLEADER, // T_RLEADER
    MT_RLEADER2, // T_RLEADER2
    MT_MISSILESMOKE, // T_MISSILESMOKE
    MT_REAVER, // T_REAVER
    MT_GUARD1, // T_GUARD1
    MT_GUARD2, // T_GUARD2
    MT_GUARD3, // T_GUARD3
    MT_GUARD4, // T_GUARD4
    MT_GUARD5, // T_GUARD5
    MT_GUARD6, // T_GUARD6
    MT_GUARD7, // T_GUARD7
    MT_GUARD8, // T_GUARD8
    MT_SHADOWGUARD, // T_SHADOWGUARD
    MT_PGUARD, // T_PGUARD
    MT_CRUSADER, // T_CRUSADER
    MT_BISHOP, // T_BISHOP
    MT_ORACLE, // T_ORACLE
    MT_PRIEST, // T_PRIEST
    MT_NODE, // T_NODE
    MT_SPECTREHEAD, // T_SPECTREHEAD
    MT_ENTITY, // T_ENTITY
    MT_SUBENTITY, // T_SUBENTITY
    MT_NEST, // T_NEST
    MT_POD, // T_POD
    MT_SENTINEL, // T_SENTINEL
    MT_STALKER, // T_STALKER
    MT_INQUISITOR, // T_INQUISITOR
    MT_INQARM, // T_INQARM
    MT_PROGRAMMER, // T_PROGRAMMER
    MT_PROGRAMMERBASE, // T_PROGRAMMERBASE
    MT_HOOKSHOT, // T_HOOKSHOT
    MT_CHAINSHOT, // T_CHAINSHOT
    MT_MINIMISSLE, // T_MINIMISSLE
    MT_C_MISSILE, // T_C_MISSILE
    MT_SEEKMISSILE, // T_SEEKMISSILE
    MT_ELECARROW, // T_ELECARROW
    MT_POISARROW, // T_POISARROW
    MT_R_LASER, // T_R_LASER
    MT_L_LASER, // T_L_LASER
    MT_HEGRENADE, // T_HEGRENADE
    MT_PGRENADE, // T_PGRENADE
    MT_INQGRENADE, // T_INQGRENADE
    MT_PFLAME, // T_PFLAME
    MT_TORPEDO, // T_TORPEDO
    MT_TORPEDOSPREAD, // T_TORPEDOSPREAD
    MT_SFIREBALL, // T_SFIREBALL
    MT_C_FLAME, // T_C_FLAME
    MT_STRIFEPUFF3, // T_STRIFEPUFF3
    MT_STRIFEPUFF, // T_STRIFEPUFF
    MT_SPARKPUFF, // T_SPARKPUFF
    MT_BLOOD_DEATH, // T_BLOOD_DEATH
    MT_TFOG, // T_TFOG
    MT_IFOG, // T_IFOG
    MT_TELEPORTMAN, // T_TELEPORTMAN
    MT_MISC_01, // T_MISC_01
    MT_TURRET, // T_TURRET
    MT_GATE, // T_GATE
    MT_COMPUTER, // T_COMPUTER
    MT_INV_MED1, // T_INV_MED1
    MT_INV_MED2, // T_INV_MED2
    MT_INV_MED3, // T_INV_MED3
    MT_DEGNINORE, // T_DEGNINORE
    MT_INV_ARMOR2, // T_INV_ARMOR2
    MT_INV_ARMOR1, // T_INV_ARMOR1
    MT_MISC_22, // T_MISC_22
    MT_MISC_11, // T_MISC_11
    MT_KEY_BASE, // T_KEY_BASE
    MT_GOVSKEY, // T_GOVSKEY
    MT_KEY_TRAVEL, // T_KEY_TRAVEL
    MT_KEY_ID_BLUE, // T_KEY_ID_BLUE
    MT_PRISONKEY, // T_PRISONKEY
    MT_KEY_HAND, // T_KEY_HAND
    MT_POWER1KEY, // T_POWER1KEY
    MT_POWER2KEY, // T_POWER2KEY
    MT_POWER3KEY, // T_POWER3KEY
    MT_KEY_GOLD, // T_KEY_GOLD
    MT_KEY_ID_GOLD, // T_KEY_ID_GOLD
    MT_KEY_SILVER, // T_KEY_SILVER
    MT_KEY_ORACLE, // T_KEY_ORACLE
    MT_MILITARYID, // T_MILITARYID
    MT_KEY_ORDER, // T_KEY_ORDER
    MT_KEY_WAREHOUSE, // T_KEY_WAREHOUSE
    MT_KEY_BRASS, // T_KEY_BRASS
    MT_KEY_RED_CRYSTAL, // T_KEY_RED_CRYSTAL
    MT_KEY_BLUE_CRYSTAL, // T_KEY_BLUE_CRYSTAL
    MT_KEY_CHAPEL, // T_KEY_CHAPEL
    MT_CATACOMBKEY, // T_CATACOMBKEY
    MT_SECURITYKEY, // T_SECURITYKEY
    MT_KEY_CORE, // T_KEY_CORE
    MT_KEY_MAULER, // T_KEY_MAULER
    MT_KEY_FACTORY, // T_KEY_FACTORY
    MT_KEY_MINE, // T_KEY_MINE
    MT_NEWKEY5, // T_NEWKEY5
    MT_INV_SHADOWARMOR, // T_INV_SHADOWARMOR
    MT_INV_SUIT, // T_INV_SUIT
    MT_QUEST_UNIFORM, // T_QUEST_UNIFORM
    MT_QUEST_GUARD_UNIFORM, // T_QUEST_GUARD_UNIFORM
    MT_INV_SUPERMAP, // T_INV_SUPERMAP
    MT_INV_RADAR, // T_INV_RADAR
    MT_BEACON, // T_BEACON
    MT_INV_TARGETER, // T_INV_TARGETER
    MT_MONY_1, // T_MONY_1
    MT_MONY_10, // T_MONY_10
    MT_MONY_25, // T_MONY_25
    MT_MONY_50, // T_MONY_50
    MT_MONY_300, // T_MONY_300
    MT_TOKEN_RING, // T_TOKEN_RING
    MT_INV_CHALICE, // T_INV_CHALICE
    MT_TOKEN_EAR, // T_TOKEN_EAR
    MT_INV_COMMUNICATOR, // T_INV_COMMUNICATOR
    MT_AGREN, // T_AGREN
    MT_APGREN, // T_APGREN
    MT_ACLIP, // T_ACLIP
    MT_AAMMOBOX, // T_AAMMOBOX
    MT_AMINI, // T_AMINI
    MT_AMINIBOX, // T_AMINIBOX
    MT_ACELL, // T_ACELL
    MT_APCELL, // T_APCELL
    MT_APAROW, // T_APAROW
    MT_AAROW, // T_AAROW
    MT_INV_SATCHEL, // T_INV_SATCHEL
    MT_PULSE, // T_PULSE
    MT_RIFLESTAND, // T_RIFLESTAND
    MT_FLAMETHROWER, // T_FLAMETHROWER
    MT_TOKEN_FLAME_THROWER_PARTS, // T_TOKEN_FLAME_THROWER_PARTS
    MT_MISSILELAUNCHER, // T_MISSILELAUNCHER
    MT_BLASTER, // T_BLASTER
    MT_CROSSBOW, // T_CROSSBOW
    MT_GRENADELAUNCHER, // T_GRENADELAUNCHER
    MT_SIGIL_A, // T_SIGIL_A
    MT_SIGIL_B, // T_SIGIL_B
    MT_SIGIL_C, // T_SIGIL_C
    MT_SIGIL_D, // T_SIGIL_D
    MT_SIGIL_E, // T_SIGIL_E
    MT_POWER_CRYSTAL, // T_POWER_CRYSTAL
    MT_RAT, // T_RAT
    MT_MISC_05, // T_MISC_05
    MT_MISC_06, // T_MISC_06
    MT_MISC_15, // T_MISC_15
    MT_LIGHT14, // T_LIGHT14
    MT_LIGHT13, // T_LIGHT13
    MT_LIGHT12, // T_LIGHT12
    MT_LIGHT18, // T_LIGHT18
    MT_PILLAR2, // T_PILLAR2
    MT_PILLAR3, // T_PILLAR3
    MT_PILLAR4, // T_PILLAR4
    MT_PILLAR5, // T_PILLAR5
    MT_PILLAR6, // T_PILLAR6
    MT_PILLAR7, // T_PILLAR7
    MT_CAVE2, // T_CAVE2
    MT_CAVE3, // T_CAVE3
    MT_CAVE4, // T_CAVE4
    MT_CAVE6, // T_CAVE6
    MT_CAVE7, // T_CAVE7
    MT_CAVE5, // T_CAVE5
    MT_LIGHT2, // T_LIGHT2
    MT_LIGHT3, // T_LIGHT3
    MT_MISC_03, // T_MISC_03
    MT_MISC_13, // T_MISC_13
    MT_MISC_02, // T_MISC_02
    MT_MISC_07, // T_MISC_07
    MT_BIO2, // T_BIO2
    MT_TELEPORTSTAND, // T_TELEPORTSTAND
    MT_BIO1, // T_BIO1
    MT_GIBS, // T_GIBS
    MT_MISC_04, // T_MISC_04
    MT_LIGHT11, // T_LIGHT11
    MT_LIGHT10, // T_LIGHT10
    MT_LIGHT9, // T_LIGHT9
    MT_LIGHT8, // T_LIGHT8
    MT_MISC_14, // T_MISC_14
    MT_LIGHT1, // T_LIGHT1
    MT_PILLAR8, // T_PILLAR8
    MT_PILLAR9, // T_PILLAR9
    MT_LIGHT15, // T_LIGHT15
    MT_LIGHT4, // T_LIGHT4
    MT_LIGHT5, // T_LIGHT5
    MT_ROCK1, // T_ROCK1
    MT_ROCK2, // T_ROCK2
    MT_ROCK3, // T_ROCK3
    MT_ROCK4, // T_ROCK4
    MT_TREE7, // T_TREE7
    MT_RUBBLE1, // T_RUBBLE1
    MT_RUBBLE2, // T_RUBBLE2
    MT_RUBBLE3, // T_RUBBLE3
    MT_RUBBLE4, // T_RUBBLE4
    MT_RUBBLE5, // T_RUBBLE5
    MT_RUBBLE6, // T_RUBBLE6
    MT_RUBBLE7, // T_RUBBLE7
    MT_RUBBLE8, // T_RUBBLE8
    MT_MISC_08, // T_MISC_08
    MT_LIGHT6, // T_LIGHT6
    MT_LIGHT7, // T_LIGHT7
    MT_TREE2, // T_TREE2
    MT_TREE3, // T_TREE3
    MT_TREE4, // T_TREE4
    MT_TREE1, // T_TREE1
    MT_TREE6, // T_TREE6
    MT_TREE5, // T_TREE5
    MT_CAVE1, // T_CAVE1
    MT_PILLAR1, // T_PILLAR1
    MT_MISC_10, // T_MISC_10
    MT_MISC_09, // T_MISC_09
    MT_MISC_17, // T_MISC_17
    MT_MISC_18, // T_MISC_18
    MT_MISC_19, // T_MISC_19
    MT_MISC_20, // T_MISC_20
    MT_LIGHT16, // T_LIGHT16
    MT_LIGHT17, // T_LIGHT17
    MT_MISC_21, // T_MISC_21
    MT_MISC_12, // T_MISC_12
    MT_MISC_26, // T_MISC_26
    MT_MISC_23, // T_MISC_23
    MT_MISC_24, // T_MISC_24
    MT_MISC_25, // T_MISC_25
    MT_COUPLING, // T_COUPLING
    MT_COUPLING_BROKEN, // T_COUPLING_BROKEN
    MT_PILLAR10, // T_PILLAR10
    MT_PILLAR11, // T_PILLAR11
    MT_PILLAR12, // T_PILLAR12
    MT_PILLAR13, // T_PILLAR13
    MT_LIGHT19, // T_LIGHT19
    MT_MEAT, // T_MEAT
    MT_JUNK, // T_JUNK
    MT_BURNDROP, // T_BURNDROP
    MT_TOKEN_AMMO, // T_TOKEN_AMMO
    MT_TOKEN_HEALTH, // T_TOKEN_HEALTH
    MT_TOKEN, // T_TOKEN
    MT_TOKEN_ALARM, // T_TOKEN_ALARM
    MT_TOKEN_DOOR1, // T_TOKEN_DOOR1
    MT_TOKEN_SHOPCLOSE, // T_TOKEN_SHOPCLOSE
    MT_TOKEN_PRISON_PASS, // T_TOKEN_PRISON_PASS
    MT_TOKEN_DOOR3, // T_TOKEN_DOOR3
    MT_TOKEN_STAMINA, // T_TOKEN_STAMINA
    MT_TOKEN_NEW_ACCURACY, // T_TOKEN_NEW_ACCURACY
    MT_TOKEN_REPORT, // T_TOKEN_REPORT
    MT_TOKEN_TOUGHNESS, // T_TOKEN_TOUGHNESS
    MT_TOKEN_ACCURACY, // T_TOKEN_ACCURACY
    MT_TOKEN_ORACLE_PASS, // T_TOKEN_ORACLE_PASS
    MT_SLIDESHOW // T_SLIDESHOW
  );
{$ENDIF}

//==============================================================================
//
// EVH_ThingActivate
//
//==============================================================================
function EVH_ThingActivate(tid: integer): boolean;

//==============================================================================
//
// EVH_ThingDeactivate
//
//==============================================================================
function EVH_ThingDeactivate(tid: integer): boolean;

//==============================================================================
//
// EVH_ThingRemove
//
//==============================================================================
function EVH_ThingRemove(tid: integer): boolean;

//==============================================================================
//
// EVH_ThingDestroy
//
//==============================================================================
function EVH_ThingDestroy(tid: integer): boolean;

//==============================================================================
//
// EVH_ThingProjectile
//
//==============================================================================
function EVH_ThingProjectile(args: PByteArray; gravity: boolean): boolean;

//==============================================================================
//
// EVH_ThingSpawn
//
//==============================================================================
function EVH_ThingSpawn(args: PByteArray; fog: boolean): boolean;

implementation

uses
  d_main,
  d_think,
  info,
  m_fixed,
  p_mobj,
  p_mobj_h,
  p_sounds,
  p_tick,
  p_extra,
  p_map,
  p_inter,
  tables,
  sounddata,
  s_sound,
  udmf_mobj,
  udmf_telept,
  doomdef;

//==============================================================================
//
// EVH_ThingProjectile
//
//==============================================================================
function EVH_ThingProjectile(args: PByteArray; gravity: boolean): boolean;
var
  tid: integer;
  angle: angle_t;
  fineAngle: integer;
  speed: fixed_t;
  vspeed: fixed_t;
  moType: mobjtype_t;
  mobj: Pmobj_t;
  newMobj: Pmobj_t;
  searcher: Pthinker_t;
begin
  result := false;
  searcher := @thinkercap;
  tid := args[0];
  moType := TranslateThingType[args[1]];
  if nomonsters and (mobjinfo[Ord(moType)].flags and MF_COUNTKILL <> 0) then
  begin // Don't spawn monsters if -nomonsters
    exit;
  end;

  angle := args[2] * $1000000;
  fineAngle := angle shr ANGLETOFINESHIFT;
  speed := args[3] * 8192;
  vspeed := args[4] * 8192;
  while P_FindMobjFromTID(tid, Pointer(searcher), mobj) <> nil do
  begin
    newMobj := P_SpawnMobj(mobj.x, mobj.y, mobj.z, Ord(moType));
    A_SeeSound(newMobj, newMobj);
    newMobj.target := mobj; // Originator
    newMobj.angle := angle;
    newMobj.momx := FixedMul(speed, finecosine[fineAngle]);
    newMobj.momy := FixedMul(speed, finesine[fineAngle]);
    newMobj.momz := vspeed;
    newMobj.flags := newMobj.flags or MF_DROPPED; // Don't respawn
    if gravity then
      A_LowGravity(newMobj);
    if P_CheckMissileSpawn(newMobj) then
      result := true;
  end;
end;

//==============================================================================
//
// EVH_ThingSpawn
//
//==============================================================================
function EVH_ThingSpawn(args: PByteArray; fog: boolean): boolean;
var
  tid: integer;
  angle: angle_t;
  mobj: Pmobj_t;
  newMobj: Pmobj_t;
  fogMobj: Pmobj_t;
  moType: mobjtype_t;
  searcher: Pthinker_t;
  z: fixed_t;
begin
  result := false;
  searcher := @thinkercap;
  tid := args[0];
  moType := TranslateThingType[args[1]];
  if nomonsters and (mobjinfo[Ord(moType)].flags and MF_COUNTKILL <> 0) then
  begin // Don't spawn monsters if -nomonsters
    exit;
  end;

  angle := args[2] * $1000000;
  while P_FindMobjFromTID(tid, Pointer(searcher), mobj) <> nil do
  begin
    if mobjinfo[Ord(moType)].flags_ex and MF_EX_FLOATBOB <> 0 then
      z := mobj.z - mobj.floorz
    else
      z := mobj.z;
    newMobj := P_SpawnMobj(mobj.x, mobj.y, z, Ord(moType));
    if not P_TestMobjLocation(newMobj) then
    begin // Didn't fit
      P_RemoveMobj(newMobj);
    end
    else
    begin
      newMobj.angle := angle;
      if fog then
      begin
        fogMobj := P_SpawnMobj(mobj.x, mobj.y, mobj.z + TELEFOGHEIGHT, Ord(MT_TFOG));
        S_StartSound(fogMobj, Ord(sfx_telept));
      end;
      newMobj.flags := newMobj.flags or MF_DROPPED; // Don't respawn
      result := true;
    end;
  end;
end;

//==============================================================================
//
// P_ActivateThing
//
//==============================================================================
function P_ActivateThing(mobj: Pmobj_t): boolean;
begin
  if mobj.flags and MF_COUNTKILL <> 0 then
  begin // Monster
    if mobj.flags and MF_AMBUSH <> 0 then
    begin
      mobj.flags := mobj.flags and not MF_AMBUSH;
      mobj.tics := 1;
      result := true;
    end
    else
      result := false;
    exit;
  end;

  result := true;
end;

//==============================================================================
//
// EVH_ThingActivate
//
//==============================================================================
function EVH_ThingActivate(tid: integer): boolean;
var
  mobj: Pmobj_t;
  searcher: Pthinker_t;
begin
  result := false;
  searcher := @thinkercap;
  while P_FindMobjFromTID(tid, Pointer(searcher), mobj) <> nil do
    if P_ActivateThing(mobj) then
      result := true;
end;

//==============================================================================
//
// P_DeactivateThing
//
//==============================================================================
function P_DeactivateThing(mobj: Pmobj_t): boolean;
begin
  if mobj.flags and MF_COUNTKILL <> 0 then
  begin // Monster
    if mobj.flags and MF_AMBUSH = 0 then
    begin
      mobj.flags := mobj.flags or MF_AMBUSH;
      mobj.tics := -1;
      result := true;
    end
    else
      result := false;
    exit;
  end;

  result := true;
end;

//==============================================================================
//
// EVH_ThingDeactivate
//
//==============================================================================
function EVH_ThingDeactivate(tid: integer): boolean;
var
  mobj: Pmobj_t;
  searcher: Pthinker_t;
begin
  result := false;
  searcher := @thinkercap;
  while P_FindMobjFromTID(tid, Pointer(searcher), mobj) <> nil do
    if P_DeactivateThing(mobj) then
      result := true;
end;

//==============================================================================
//
// EVH_ThingRemove
//
//==============================================================================
function EVH_ThingRemove(tid: integer): boolean;
var
  mobj: Pmobj_t;
  searcher: Pthinker_t;
begin
  result := false;
  searcher := @thinkercap;
  while P_FindMobjFromTID(tid, Pointer(searcher), mobj) <> nil do
  begin
    result := true;
    P_RemoveMobj(mobj);
  end;
end;

//==============================================================================
//
// EVH_ThingDestroy
//
//==============================================================================
function EVH_ThingDestroy(tid: integer): boolean;
var
  mobj: Pmobj_t;
  searcher: Pthinker_t;
begin
  result := false;
  searcher := @thinkercap;
  while P_FindMobjFromTID(tid, Pointer(searcher), mobj) <> nil do
  begin
    if mobj.flags and MF_SHOOTABLE <> 0 then
    begin
      P_DamageMobj(mobj, nil, nil, 10000);
      result := true;
    end;
  end;
end;

end.
