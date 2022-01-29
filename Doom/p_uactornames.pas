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
//  UMAPINFO support - Doom actor names
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
  NUMACTORNAMES = 250;
  ActorNames: array[0..NUMACTORNAMES - 1] of string[MOBJINFONAMESIZE] = (
    'DoomPlayer',
    'ZombieMan',
    'ShotgunGuy',
    'Archvile',
    'ArchvileFire',
    'Revenant',
    'RevenantTracer',
    'RevenantTracerSmoke',
    'Fatso',
    'FatShot',
    'ChaingunGuy',
    'DoomImp',
    'Demon',
    'Spectre',
    'Cacodemon',
    'BaronOfHell',
    'BaronBall',
    'HellKnight',
    'LostSoul',
    'SpiderMastermind',
    'Arachnotron',
    'Cyberdemon',
    'PainElemental',
    'WolfensteinSS',
    'CommanderKeen',
    'BossBrain',
    'BossEye',
    'BossTarget',
    'SpawnShot',
    'SpawnFire',
    'ExplosiveBarrel',
    'DoomImpBall',
    'CacodemonBall',
    'Rocket',
    'PlasmaBall',
    'BFGBall',
    'ArachnotronPlasma',
    'BulletPuff',
    'Blood',
    'TeleportFog',
    'ItemFog',
    'TeleportDest',
    'BFGExtra',
    'GreenArmor',
    'BlueArmor',
    'HealthBonus',
    'ArmorBonus',
    'BlueCard',
    'RedCard',
    'YellowCard',
    'YellowSkull',
    'RedSkull',
    'BlueSkull',
    'Stimpack',
    'Medikit',
    'Soulsphere',
    'InvulnerabilitySphere',
    'Berserk',
    'BlurSphere',
    'RadSuit',
    'Allmap',
    'Infrared',
    'Megasphere',
    'Clip',
    'ClipBox',
    'RocketAmmo',
    'RocketBox',
    'Cell',
    'CellPack',
    'Shell',
    'ShellBox',
    'Backpack',
    'BFG9000',
    'Chaingun',
    'Chainsaw',
    'RocketLauncher',
    'PlasmaRifle',
    'Shotgun',
    'SuperShotgun',
    'TechLamp',
    'TechLamp2',
    'Column',
    'TallGreenColumn',
    'ShortGreenColumn',
    'TallRedColumn',
    'ShortRedColumn',
    'SkullColumn',
    'HeartColumn',
    'EvilEye',
    'FloatingSkull',
    'TorchTree',
    'BlueTorch',
    'GreenTorch',
    'RedTorch',
    'ShortBlueTorch',
    'ShortGreenTorch',
    'ShortRedTorch',
    'Stalagtite',
    'TechPillar',
    'CandleStick',
    'Candelabra',
    'BloodyTwitch',
    'Meat2',
    'Meat3',
    'Meat4',
    'Meat5',
    'NonsolidMeat2',
    'NonsolidMeat4',
    'NonsolidMeat3',
    'NonsolidMeat5',
    'NonsolidTwitch',
    'DeadCacodemon',
    'DeadMarine',
    'DeadZombieMan',
    'DeadDemon',
    'DeadLostSoul',
    'DeadDoomImp',
    'DeadShotgunGuy',
    'GibbedMarine',
    'GibbedMarineExtra',
    'HeadsOnAStick',
    'Gibs',
    'HeadOnAStick',
    'HeadCandles',
    'DeadStick',
    'LiveStick',
    'BigTree',
    'BurningBarrel',
    'HangNoGuts',
    'HangBNoBrain',
    'HangTLookingDown',
    'HangTSkull',
    'HangTLookingUp',
    'HangTNoBrain',
    'ColonGibs',
    'SmallBloodPool',
    'BrainStem',
  //Boom/MBF additions
    'PointPusher',
    'PointPuller',
    'MBFHelperDog',
    'PlasmaBall1',
    'PlasmaBall2',
    'EvilSceptre',
    'UnholyBible',
    'MusicChanger',
    'Deh_Actor_145',
    'Deh_Actor_146',
    'Deh_Actor_147',
    'Deh_Actor_148',
    'Deh_Actor_149',
  // DEHEXTRA Actors start here
    'Deh_Actor_150', // Extra thing 0
    'Deh_Actor_151', // Extra thing 1
    'Deh_Actor_152', // Extra thing 2
    'Deh_Actor_153', // Extra thing 3
    'Deh_Actor_154', // Extra thing 4
    'Deh_Actor_155', // Extra thing 5
    'Deh_Actor_156', // Extra thing 6
    'Deh_Actor_157', // Extra thing 7
    'Deh_Actor_158', // Extra thing 8
    'Deh_Actor_159', // Extra thing 9
    'Deh_Actor_160', // Extra thing 10
    'Deh_Actor_161', // Extra thing 11
    'Deh_Actor_162', // Extra thing 12
    'Deh_Actor_163', // Extra thing 13
    'Deh_Actor_164', // Extra thing 14
    'Deh_Actor_165', // Extra thing 15
    'Deh_Actor_166', // Extra thing 16
    'Deh_Actor_167', // Extra thing 17
    'Deh_Actor_168', // Extra thing 18
    'Deh_Actor_169', // Extra thing 19
    'Deh_Actor_170', // Extra thing 20
    'Deh_Actor_171', // Extra thing 21
    'Deh_Actor_172', // Extra thing 22
    'Deh_Actor_173', // Extra thing 23
    'Deh_Actor_174', // Extra thing 24
    'Deh_Actor_175', // Extra thing 25
    'Deh_Actor_176', // Extra thing 26
    'Deh_Actor_177', // Extra thing 27
    'Deh_Actor_178', // Extra thing 28
    'Deh_Actor_179', // Extra thing 29
    'Deh_Actor_180', // Extra thing 30
    'Deh_Actor_181', // Extra thing 31
    'Deh_Actor_182', // Extra thing 32
    'Deh_Actor_183', // Extra thing 33
    'Deh_Actor_184', // Extra thing 34
    'Deh_Actor_185', // Extra thing 35
    'Deh_Actor_186', // Extra thing 36
    'Deh_Actor_187', // Extra thing 37
    'Deh_Actor_188', // Extra thing 38
    'Deh_Actor_189', // Extra thing 39
    'Deh_Actor_190', // Extra thing 40
    'Deh_Actor_191', // Extra thing 41
    'Deh_Actor_192', // Extra thing 42
    'Deh_Actor_193', // Extra thing 43
    'Deh_Actor_194', // Extra thing 44
    'Deh_Actor_195', // Extra thing 45
    'Deh_Actor_196', // Extra thing 46
    'Deh_Actor_197', // Extra thing 47
    'Deh_Actor_198', // Extra thing 48
    'Deh_Actor_199', // Extra thing 49
    'Deh_Actor_200', // Extra thing 50
    'Deh_Actor_201', // Extra thing 51
    'Deh_Actor_202', // Extra thing 52
    'Deh_Actor_203', // Extra thing 53
    'Deh_Actor_204', // Extra thing 54
    'Deh_Actor_205', // Extra thing 55
    'Deh_Actor_206', // Extra thing 56
    'Deh_Actor_207', // Extra thing 57
    'Deh_Actor_208', // Extra thing 58
    'Deh_Actor_209', // Extra thing 59
    'Deh_Actor_210', // Extra thing 60
    'Deh_Actor_211', // Extra thing 61
    'Deh_Actor_212', // Extra thing 62
    'Deh_Actor_213', // Extra thing 63
    'Deh_Actor_214', // Extra thing 64
    'Deh_Actor_215', // Extra thing 65
    'Deh_Actor_216', // Extra thing 66
    'Deh_Actor_217', // Extra thing 67
    'Deh_Actor_218', // Extra thing 68
    'Deh_Actor_219', // Extra thing 69
    'Deh_Actor_220', // Extra thing 70
    'Deh_Actor_221', // Extra thing 71
    'Deh_Actor_222', // Extra thing 72
    'Deh_Actor_223', // Extra thing 73
    'Deh_Actor_224', // Extra thing 74
    'Deh_Actor_225', // Extra thing 75
    'Deh_Actor_226', // Extra thing 76
    'Deh_Actor_227', // Extra thing 77
    'Deh_Actor_228', // Extra thing 78
    'Deh_Actor_229', // Extra thing 79
    'Deh_Actor_230', // Extra thing 80
    'Deh_Actor_231', // Extra thing 81
    'Deh_Actor_232', // Extra thing 82
    'Deh_Actor_233', // Extra thing 83
    'Deh_Actor_234', // Extra thing 84
    'Deh_Actor_235', // Extra thing 85
    'Deh_Actor_236', // Extra thing 86
    'Deh_Actor_237', // Extra thing 87
    'Deh_Actor_238', // Extra thing 88
    'Deh_Actor_239', // Extra thing 89
    'Deh_Actor_240', // Extra thing 90
    'Deh_Actor_241', // Extra thing 91
    'Deh_Actor_242', // Extra thing 92
    'Deh_Actor_243', // Extra thing 93
    'Deh_Actor_244', // Extra thing 94
    'Deh_Actor_245', // Extra thing 95
    'Deh_Actor_246', // Extra thing 96
    'Deh_Actor_247', // Extra thing 97
    'Deh_Actor_248', // Extra thing 98
    'Deh_Actor_249'  // Extra thing 99
  );

implementation

end.

