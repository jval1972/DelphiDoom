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
//  DESCRIPTION:
//  UMAPINFO support - Heretic actor names
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit p_uactornames;

interface

uses
  info_h;

const
  NUMACTORNAMES = 161;
  ActorNames: array[0..NUMACTORNAMES - 1] of string[MOBJINFONAMESIZE] = (
    'CRYSTAL VIAL',
    'SILVER SHIELD',
    'ENCHANTED SHIELD',
    'BAG OF HOLDING',
    'MAP SCROLL',
    'SHADOWSPHERE',
    'QUARTZ FLASK',
    'WINGS OF WRATH',
    'RING OF INVULNERABILITY',
    'TOME OF POWER',
    'MORPH OVUM',
    'MT_EGGFX',
    'MYSTIC URN',
    'TORCH',
    'TIME BOMB OF THE ANCIENTS',
    'MT_FIREBOMB',
    'CHAOS DEVICE',
    'GASBAG',
    'MT_PODGOO',
    'GASBAG GENERATOR',
    'MT_SPLASH',
    'MT_SPLASHBASE',
    'MT_LAVASPLASH',
    'MT_LAVASMOKE',
    'MT_SLUDGECHUNK',
    'MT_SLUDGESPLASH',
    'HANGING SKULL',
    'HANGING SKULL 2',
    'HANGING SKULL 3',
    'HANGING SKULL 4',
    'CHANDELIER',
    'SERPENT TORCH',
    'SMALL PILLAR',
    'SMALL STALAGMITE',
    'LARGE STALAGMITE',
    'SMALL STALACTITE',
    'LARGE STALACTITE',
    'FIRE BRAZIER',
    'BARREL',
    'BROWN PILLAR',
    'MOSS',
    'MOSS 2',
    'WALL TORCH',
    'HANGING CORPSE',
    'BLUE KEY MARKER',
    'GREEN KEY MARKER',
    'MT_GOLDWANDPUFF1',
    'MT_GOLDWANDPUFF2',
    'PHOENIX ROD',
    'YELLOW KEY MARKER',
    'MT_KEYGIZMOFLOAT',
    'VOLCANO',
    'MT_VOLCANOBLAST',
    'MT_VOLCANOTBLAST',
    'TELEPORT GLITTER',
    'TELEPORT GLITTER EXIT',
    'MT_TELEGLITTER',
    'MT_TELEGLITTER2',
    'MT_TFOG',
    'TELEPORT LANDING',
    'MT_STAFFPUFF',
    'MT_STAFFPUFF2',
    'MT_BEAKPUFF',
    'GAUNTLETS OF THE NECROMANCER',
    'MT_GAUNTLETPUFF1',
    'MT_GAUNTLETPUFF2',
    'DRAGON CLAW',
    'MT_BLASTERFX1',
    'MT_BLASTERSMOKE',
    'MT_RIPPER',
    'MT_BLASTERPUFF1',
    'MT_BLASTERPUFF2',
    'FIREMACE',
    'MT_MACEFX1',
    'MT_MACEFX2',
    'MT_MACEFX3',
    'MT_MACEFX4',
    'HELLSTAFF',
    'MT_HORNRODFX1',
    'MT_HORNRODFX2',
    'MT_RAINPLR1',
    'MT_RAINPLR2',
    'MT_RAINPLR3',
    'MT_RAINPLR4',
    'MT_GOLDWANDFX1',
    'MT_GOLDWANDFX2',
    'NITROGOLEM GHOST',
    'MT_MUMMYSOUL',
    'MT_MUMMYFX1',
    'WEREDRAGON',
    'MT_BEASTBALL',
    'MT_BURNBALL',
    'MT_BURNBALLFB',
    'MT_PUFFY',
    'OPHIDIAN',
    'MT_SNAKEPRO_A',
    'MT_SNAKEPRO_B',
    'IRON LICH',
    'MT_HEADFX1',
    'MT_HEADFX2',
    'MT_PHOENIXFX1',
    'MT_PHOENIXPUFF',
    'MT_PHOENIXFX2',
    'ETHEREAL CROSSBOW',
    'MT_CRBOWFX1',
    'MT_CRBOWFX2',
    'MT_CRBOWFX3',
    'MT_CRBOWFX4',
    'MT_BLOOD',
    'MT_BLOODSPLATTER',
    'MT_PLAYER',
    'MT_BLOODYSKULL',
    'MT_CHICPLAYER',
    'MT_CHICKEN',
    'MT_FEATHER',
    'GOLEM',
    'NITROGOLEM',
    'GOLEM GHOST',
    'MT_HEADFX3',
    'MT_WHIRLWIND',
    'SABRECLAW',
    'DISCIPLE OF D''SPARIL',
    'MT_KNIGHTAXE',
    'MT_REDAXE',
    'D''SPARIL',
    'MT_SRCRFX1',
    'MT_SORCERER2',
    'MT_SOR2FX1',
    'MT_SOR2FXSPARK',
    'MT_WIZFX1',
    'GARGOYLE',
    'FIRE GARGOYLE',
    'MT_IMPCHUNK1',
    'MT_IMPCHUNK2',
    'MT_IMPBALL',
    'UNDEAD WARRIOR',
    'UNDEAD WARRIOR GHOST',
    'INFERNO ORB',
    'CLAW ORB',
    'ENERGY ORB',
    'MT_SOR2FX2',
    'MT_SOR2TELEFADE',
    'MAULOTAUR',
    'FLAMING PELLETS RANGED ATTACK',
    'GROUND FLAME RANGED ATTACK',
    'MT_MNTRFX3',
    'GREEN KEY',
    'BLUE KEY',
    'YELLOW KEY',
    'WAND CRYSTAL',
    'CRYSTAL GEODE',
    'MACE SPHERES',
    'PILE OF MACE SPHERES',
    'ETHEREAL ARROWS',
    'QUIVER OF ETHEREAL ARROWS',
    'LESSER RUNES',
    'GREATER RUNES',
    'FLAME ORB',
    'WIND',
    'WATERFALL',
    'NONE'
  );

implementation

end.

