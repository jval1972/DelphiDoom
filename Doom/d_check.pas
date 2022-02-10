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
//  DESCRIPTION (d_main.h):
//   Identify known wads
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit d_check;

interface

//==============================================================================
//
// D_CheckCustomWad
//
//==============================================================================
procedure D_CheckCustomWad(const filename: string);

//==============================================================================
//
// D_GetSavePath
//
//==============================================================================
function D_GetSavePath: string;

implementation

uses
  d_delphi,
  doomdef,
  doomstat,
  m_argv,
  m_crc32,
  w_wad,
  w_wadreader;

type
  iwaddetect_t = record
    crc32: string[8];
    numlumps: integer;
    size: integer;
    version: GameVersion_t;
    customgame: CustomGame_t;
    savepath: string[16];
  end;

const
  NUMIWADDETECTITEMS = 15;
  iwadtbl: array[0..NUMIWADDETECTITEMS - 1] of iwaddetect_t = (
    (crc32: '723e60f9'; numlumps: 2194; size: 11159840; version: exe_doom_1_9; customgame: cg_none; savepath: 'doom'),      // registered
    (crc32: 'bf0eaac0'; numlumps: 2306; size: 12408292; version: exe_ultimate; customgame: cg_none; savepath: 'doomu'),     // Ultimate
    (crc32: 'ff1ba733'; numlumps: 2318; size: 12538385; version: exe_ultimate; customgame: cg_none; savepath: 'doomx'),     // Ultimate X-box
    (crc32: '5efa677e'; numlumps: 2312; size: 12487824; version: exe_ultimate; customgame: cg_none; savepath: 'doombfg'),   // Ultimate BFG
    (crc32: '162b696a'; numlumps: 1264; size:  4196020; version: exe_ultimate; customgame: cg_none; savepath: 'doom1'),     // shareware 1.9
    (crc32: '27eaae69'; numlumps: 2914; size: 14607420; version: exe_doom_1_8; customgame: cg_none; savepath: 'doom2f'),    // Doom2 1.8 french
    (crc32: 'ec8725db'; numlumps: 2919; size: 14604584; version: exe_doom_1_9; customgame: cg_none; savepath: 'doom2'),     // Doom2 1.9
    (crc32: '927a778a'; numlumps: 2935; size: 14691821; version: exe_doom_1_9; customgame: cg_bfg2; savepath: 'doom2bfg'),  // Doom2 BFG
    (crc32: '903dcc27'; numlumps: 3101; size: 18195736; version: exe_final2;   customgame: cg_none; savepath: 'tnt'),       // TNT
    (crc32: 'd4bb05c0'; numlumps: 3106; size: 18654796; version: exe_final2;   customgame: cg_none; savepath: 'tnt'),       // TNT
    (crc32: '7f572c1f'; numlumps: 3101; size: 18222568; version: exe_final2;   customgame: cg_none; savepath: 'tnt'),       // TNT
    (crc32: '48d1453c'; numlumps: 2984; size: 17420824; version: exe_final2;   customgame: cg_none; savepath: 'plutonia'),  // PLUTONIA
    (crc32: '15cd1448'; numlumps: 2988; size: 18240172; version: exe_final2;   customgame: cg_none; savepath: 'plutonia'),  // PLUTONIA
    (crc32: 'b95a03d2'; numlumps: 2649; size: 22102300; version: exe_hacx;     customgame: cg_hacx; savepath: 'hacx11'),    // HACX 1.1
    (crc32: '72e3b8ac'; numlumps: 2784; size: 19321722; version: exe_hacx;     customgame: cg_hacx; savepath: 'hacx12')     // HACX 1.2
  );

type
  pwaddetect_t = record
    crc32: string[8];
    numlumps: integer;
    size: integer;
    nummaps1: integer;
    nummaps2: integer;
    savepath: string[18];
  end;

const
  NUMPWADDETECTITEMS = 244;
  pwadtbl: array[0..NUMPWADDETECTITEMS - 1] of pwaddetect_t = (
    (crc32: 'af4b83e1'; numlumps:  393; size:   6577825; nummaps1:  0; nummaps2: 22; savepath: '1killtng'),
    (crc32: '8fd0901b'; numlumps:  657; size:  23838545; nummaps1:  0; nummaps2: 25; savepath: '1monster'),
    (crc32: '27722ab6'; numlumps:  896; size:  11125020; nummaps1:  0; nummaps2: 32; savepath: '3ha3'),
    (crc32: '07673630'; numlumps:  773; size:   8595583; nummaps1:  0; nummaps2: 32; savepath: '3hafinal'),
    (crc32: '06c90542'; numlumps: 2207; size:  37316148; nummaps1:  0; nummaps2: 32; savepath: '5room'),
    (crc32: '013a1292'; numlumps:  356; size:   4334552; nummaps1:  0; nummaps2: 32; savepath: '10secto2'),
    (crc32: '9b3f177c'; numlumps:  386; size:   9586636; nummaps1:  0; nummaps2: 32; savepath: '10sector'),
    (crc32: '7472bf05'; numlumps: 2784; size:  51086393; nummaps1:  0; nummaps2: 42; savepath: '32in24-14'),
    (crc32: '7943dab9'; numlumps:  267; size:   2818458; nummaps1:  0; nummaps2: 20; savepath: '32in24sp'),
    (crc32: '97424ce6'; numlumps: 2385; size:  16596381; nummaps1:  0; nummaps2: 34; savepath: '50monstr'),
    (crc32: '1b392fd8'; numlumps:  295; size:  17458286; nummaps1:  0; nummaps2: 18; savepath: '50shades'),
    (crc32: 'a2726091'; numlumps:   34; size:   1072233; nummaps1:  0; nummaps2:  3; savepath: '50shades_fixes'),
    (crc32: 'b5b03b6b'; numlumps:  352; size:     41048; nummaps1:  0; nummaps2: 32; savepath: '64'),
    (crc32: 'eebdaeba'; numlumps:  435; size:   6495073; nummaps1:  0; nummaps2: 33; savepath: '300minvr'),
    (crc32: 'ee639a58'; numlumps:  623; size:  11469954; nummaps1:  0; nummaps2: 32; savepath: '1024'),
    (crc32: '503a0e9b'; numlumps: 1874; size:  29784170; nummaps1:  0; nummaps2: 34; savepath: '1024cla2'),
    (crc32: '0cbf572d'; numlumps: 1884; size:  26879317; nummaps1:  0; nummaps2: 34; savepath: '1024CLAU'),
    (crc32: 'c4bc0445'; numlumps:  522; size:  30714163; nummaps1:  0; nummaps2: 32; savepath: '1994TU'),
    (crc32: 'c6f938ec'; numlumps:  926; size:  13487701; nummaps1: 38; nummaps2:  0; savepath: '2002ad10'),
    (crc32: '78644a62'; numlumps:  894; size:  13098973; nummaps1: 37; nummaps2:  0; savepath: '2002ado'),
    (crc32: '94d67c56'; numlumps: 9021; size: 138499716; nummaps1:  0; nummaps2: 32; savepath: 'aaliens'),
    (crc32: 'd484cef2'; numlumps:  631; size:   6339393; nummaps1:  0; nummaps2: 21; savepath: 'abyspe38'),
    (crc32: '918235a0'; numlumps:  601; size:   5048735; nummaps1:  0; nummaps2: 32; savepath: 'ABYSS'),
    (crc32: '9fc89325'; numlumps:  870; size:  36400197; nummaps1:  0; nummaps2: 32; savepath: 'alt'),
    (crc32: '53cc52b3'; numlumps: 1209; size:   7565732; nummaps1: 10; nummaps2:  0; savepath: 'aoddoom1'),
    (crc32: 'cd6fdacf'; numlumps: 1114; size:   6834740; nummaps1:  0; nummaps2: 11; savepath: 'aoddoom2'),
    (crc32: '657c8b8a'; numlumps:  811; size:  27054977; nummaps1:  0; nummaps2: 32; savepath: 'armadosia'),
    (crc32: '4c9b2923'; numlumps:  143; size:   5421209; nummaps1:  0; nummaps2: 13; savepath: 'arma-old'),
    (crc32: '1c045a58'; numlumps:  432; size:   7496303; nummaps1:  0; nummaps2: 32; savepath: 'atbb_f2'),
    (crc32: 'b0001c47'; numlumps: 1175; size:  23819412; nummaps1:  0; nummaps2: 32; savepath: 'AV'),
    (crc32: 'ba08a114'; numlumps: 3553; size: 134409789; nummaps1:  0; nummaps2: 12; savepath: 'Avactor'),
    (crc32: '1600263f'; numlumps: 2563; size:  16973860; nummaps1:  0; nummaps2: 32; savepath: 'batman'),
    (crc32: '0a88749f'; numlumps:  352; size:   1123814; nummaps1:  0; nummaps2: 32; savepath: 'BCSMW'),
    (crc32: '088b9781'; numlumps:  342; size:   6281097; nummaps1: 27; nummaps2:  0; savepath: 'BGComp'),
    (crc32: '319c3e44'; numlumps:  401; size:    411765; nummaps1: 36; nummaps2:  0; savepath: 'Bijou'),
    (crc32: 'cb25ce30'; numlumps:  473; size:   5646416; nummaps1:  0; nummaps2: 19; savepath: 'biowar'),
    (crc32: '7ed0d7fe'; numlumps: 1243; size:  32573881; nummaps1:  0; nummaps2: 32; savepath: 'BStain'),
    (crc32: 'bc5709e2'; numlumps: 2123; size:  22307013; nummaps1:  0; nummaps2: 27; savepath: 'btsx_e1a'),
    (crc32: 'bc2e739d'; numlumps:  680; size:  58710018; nummaps1:  0; nummaps2: 42; savepath: 'calegacy'),
    (crc32: '307a6dc1'; numlumps:  454; size:  14341857; nummaps1:  0; nummaps2: 33; savepath: 'CChest'),
    (crc32: 'a710ace7'; numlumps:  616; size:  27884804; nummaps1:  0; nummaps2: 32; savepath: 'Cchest2'),
    (crc32: 'd1955e83'; numlumps: 1148; size:  42056792; nummaps1:  0; nummaps2: 32; savepath: 'cchest3'),
    (crc32: 'ec733757'; numlumps: 2680; size:  72442794; nummaps1:  0; nummaps2: 33; savepath: 'cchest4'),
    (crc32: '45eb0944'; numlumps:  627; size:   7374482; nummaps1:  0; nummaps2: 19; savepath: 'Chain'),
    (crc32: '393d7692'; numlumps:   36; size:   1113295; nummaps1:  0; nummaps2:  3; savepath: 'Chunks3v2'),
    (crc32: '9aa57e50'; numlumps:   48; size:   1671074; nummaps1:  0; nummaps2:  4; savepath: 'Chunks4'),
    (crc32: '84915505'; numlumps:  537; size:   5970208; nummaps1:  0; nummaps2: 32; savepath: 'CLEIM20'),
    (crc32: '52d97b5e'; numlumps:  459; size:  11435323; nummaps1: 27; nummaps2:  0; savepath: 'concern'),
    (crc32: '090328ba'; numlumps: 1305; size:  17821348; nummaps1:  0; nummaps2: 23; savepath: 'Confinement'),
    (crc32: '6399c049'; numlumps:  770; size:  51206253; nummaps1:  0; nummaps2: 32; savepath: 'CSTLDOOM'),
    (crc32: 'd7921af0'; numlumps:  436; size:   2697956; nummaps1:  0; nummaps2: 32; savepath: 'Cyber110'),
    (crc32: '43dcd0f5'; numlumps:  190; size:   3578875; nummaps1:  0; nummaps2: 11; savepath: 'd2dw-v1'),
    (crc32: 'fcec6a3f'; numlumps:  457; size:  19737215; nummaps1:  0; nummaps2: 33; savepath: 'D2INO'),
    (crc32: '77b8f993'; numlumps:  807; size:  19930175; nummaps1:  0; nummaps2: 32; savepath: 'D2REDUX'),
    (crc32: 'ceda72b5'; numlumps:  697; size:  15095574; nummaps1:  0; nummaps2: 32; savepath: 'D2RELOAD'),
    (crc32: '05b76aaf'; numlumps:  732; size:   6753456; nummaps1:  0; nummaps2: 33; savepath: 'D2TWID'),
    (crc32: '9a0f7dc0'; numlumps: 2463; size:  53697472; nummaps1:  0; nummaps2: 33; savepath: 'DAEDALUS'),
    (crc32: '7affdd8f'; numlumps:  132; size:   1252536; nummaps1:  0; nummaps2: 11; savepath: 'DARKDM'),
    (crc32: 'dc01cead'; numlumps:  121; size:   1754740; nummaps1:  0; nummaps2: 11; savepath: 'DARKEN'),
    (crc32: '5a71eb57'; numlumps: 1052; size:  13495688; nummaps1:  0; nummaps2: 24; savepath: 'Darken2'),
    (crc32: 'e0a92478'; numlumps: 2580; size:  24366899; nummaps1:  0; nummaps2: 10; savepath: 'dbp13'),
    (crc32: '584ab10c'; numlumps: 1112; size:  17414956; nummaps1:  0; nummaps2: 11; savepath: 'DBP14ATF'),
    (crc32: '9847886b'; numlumps: 1665; size:  13168526; nummaps1:  0; nummaps2:  9; savepath: 'dbp20_dnd'),
    (crc32: 'af99badd'; numlumps: 1482; size:  18469518; nummaps1:  0; nummaps2:  9; savepath: 'DBP22'),
    (crc32: '28fa06ba'; numlumps:  571; size:  10324165; nummaps1:  0; nummaps2: 32; savepath: 'DCV'),
    (crc32: '9c58d95e'; numlumps:  332; size:   5270113; nummaps1: 27; nummaps2:  0; savepath: 'DDAMN0'),
    (crc32: '885c515b'; numlumps: 3090; size:  15719816; nummaps1:  0; nummaps2: 32; savepath: 'DHERETIC'),
    (crc32: 'ce3761bd'; numlumps:  353; size:   1938809; nummaps1:  0; nummaps2: 32; savepath: 'Dmonfear'),
    (crc32: 'd50b1f3c'; numlumps:  758; size:  15911841; nummaps1:  0; nummaps2: 24; savepath: 'dmp15a'),
    (crc32: '13757a5d'; numlumps: 1143; size:  23371567; nummaps1:  0; nummaps2: 18; savepath: 'dmp15b'),
    (crc32: '37f7a9d7'; numlumps:  473; size:  15105034; nummaps1:  0; nummaps2: 40; savepath: 'DMP2012'),
    (crc32: '497a001b'; numlumps:  532; size:   8731695; nummaps1:  0; nummaps2: 13; savepath: 'DMP2013B'),
    (crc32: '811bf257'; numlumps: 1000; size:  13601035; nummaps1:  0; nummaps2: 21; savepath: 'DMP2013L'),
    (crc32: '39e772b4'; numlumps:  712; size:  20063019; nummaps1:  0; nummaps2: 29; savepath: 'DMP2014v5'),
    (crc32: '7034babe'; numlumps: 2298; size:  87549237; nummaps1:  0; nummaps2: 52; savepath: 'dmp2017'),
    (crc32: '9302915e'; numlumps:   11; size:   3661764; nummaps1:  1; nummaps2:  0; savepath: 'Doom1OWBeta'),
    (crc32: '5d2ecc70'; numlumps:  391; size:   3368482; nummaps1:  0; nummaps2: 32; savepath: 'doomiiii'),
    (crc32: '5c229a85'; numlumps:  330; size:    438856; nummaps1:  0; nummaps2: 30; savepath: 'DOOMJR'),
    (crc32: '2ee0abc5'; numlumps:  597; size:   7968717; nummaps1:  0; nummaps2: 32; savepath: 'DoomZero'),
    (crc32: 'f72b4744'; numlumps: 1705; size:  18047187; nummaps1:  0; nummaps2: 32; savepath: 'dsv4_rvltinE'),
    (crc32: '5ac037bd'; numlumps: 1698; size:  17914653; nummaps1:  0; nummaps2: 32; savepath: 'dsv4_rvltinL'),
    (crc32: '5d99d356'; numlumps:  398; size:   5073423; nummaps1: 27; nummaps2:  0; savepath: 'DTWID'),
    (crc32: '63e97c31'; numlumps:  152; size:   1276188; nummaps1:  9; nummaps2:  0; savepath: 'DTWID-E5'),
    (crc32: 'df6e7d10'; numlumps:  743; size:   8251802; nummaps1: 55; nummaps2:  0; savepath: 'DTWID-LE'),
    (crc32: 'dd2b05af'; numlumps:  156; size:   1490935; nummaps1: 14; nummaps2:  0; savepath: 'DTWID-LX'),
    (crc32: '48b79e9a'; numlumps:  468; size:  15579841; nummaps1:  0; nummaps2:  5; savepath: 'DV'),
    (crc32: '4c4c207f'; numlumps: 6072; size: 177678822; nummaps1:  0; nummaps2: 32; savepath: 'DVII-1i'),
    (crc32: '108b2c82'; numlumps:  245; size:   2797304; nummaps1:  0; nummaps2: 17; savepath: 'ELMLESIN'),
    (crc32: 'd59c5237'; numlumps:  430; size:   6599128; nummaps1:  0; nummaps2: 32; savepath: 'enirvana'),
    (crc32: 'a12f8f63'; numlumps:  775; size:   9165980; nummaps1:  0; nummaps2:  5; savepath: 'EPIC'),
    (crc32: '9cd8a999'; numlumps:  770; size:   8404269; nummaps1:  0; nummaps2:  5; savepath: 'EPIC1'),
    (crc32: '7f295af6'; numlumps: 2690; size:  37243712; nummaps1:  0; nummaps2: 32; savepath: 'epic2'),
    (crc32: '8e0e1844'; numlumps:  480; size:   6824644; nummaps1:  0; nummaps2: 13; savepath: 'Equinox'),
    (crc32: 'a895a926'; numlumps:  721; size:  34380310; nummaps1:  0; nummaps2: 33; savepath: 'ESP'),
    (crc32: 'd287e614'; numlumps:  496; size:  14649445; nummaps1:  0; nummaps2: 32; savepath: 'estrangd'),
    (crc32: '707b5704'; numlumps:  447; size:   4826814; nummaps1: 36; nummaps2:  0; savepath: 'ET'),
    (crc32: '8c5ff546'; numlumps:  188; size:   1116652; nummaps1:  8; nummaps2:  0; savepath: 'ETERNITY'),
    (crc32: 'f8b03d09'; numlumps:  499; size:  21937804; nummaps1:  0; nummaps2: 34; savepath: 'eviltech'),
    (crc32: '76165ba2'; numlumps: 5406; size: 130864796; nummaps1:  0; nummaps2: 32; savepath: 'Eviternity'),
    (crc32: '22d81f44'; numlumps: 1114; size:  25066976; nummaps1:  0; nummaps2: 24; savepath: 'exomoon'),
    (crc32: '38ea94b6'; numlumps:  624; size:  12120376; nummaps1:  0; nummaps2: 32; savepath: 'fdevil'),
    (crc32: '35102bfe'; numlumps: 1496; size:   7155004; nummaps1: 34; nummaps2:  0; savepath: 'FFdoom2'),
    (crc32: '6b86a458'; numlumps:  488; size:   9956862; nummaps1:  0; nummaps2: 32; savepath: 'Fragport'),
    (crc32: '05034cd5'; numlumps: 2157; size:  32254515; nummaps1:  0; nummaps2: 22; savepath: 'freudslp'),
    (crc32: '3e112436'; numlumps:  482; size:   6105916; nummaps1:  0; nummaps2: 32; savepath: 'garrulo'),
    (crc32: '0b954f84'; numlumps:  544; size:  25359070; nummaps1:  0; nummaps2: 32; savepath: 'gd'),
    (crc32: 'bd4a4da2'; numlumps:   98; size:    797190; nummaps1:  0; nummaps2:  8; savepath: 'ge64dm'),
    (crc32: '95e76bd4'; numlumps:  352; size:   5618208; nummaps1:  0; nummaps2: 32; savepath: 'ge64levs'),
    (crc32: 'fd560ecd'; numlumps: 2146; size:  34346506; nummaps1:  0; nummaps2: 33; savepath: 'GMP'),
    (crc32: 'be6301ee'; numlumps:  526; size:  22222384; nummaps1:  0; nummaps2: 35; savepath: 'h_phobia'),
    (crc32: 'a54bc6fe'; numlumps:   11; size:   7302387; nummaps1:  0; nummaps2:  1; savepath: 'HBFM29'),
    (crc32: '54841136'; numlumps: 1035; size:  48317976; nummaps1:  0; nummaps2: 32; savepath: 'hc092604'),
    (crc32: '5613128f'; numlumps:  352; size:   2882561; nummaps1:  0; nummaps2: 32; savepath: 'HELINHEL'),
    (crc32: 'f31be099'; numlumps:  559; size:  59606366; nummaps1:  0; nummaps2: 32; savepath: 'Hellbnd'),
    (crc32: '863e789f'; numlumps: 1123; size:   9307836; nummaps1:  0; nummaps2: 32; savepath: 'Herian'),
    (crc32: 'ca8afeba'; numlumps: 1744; size:  19840184; nummaps1:  0; nummaps2: 32; savepath: 'HERIAN2'),
    (crc32: 'c52d80ef'; numlumps:  459; size:   6729226; nummaps1: 36; nummaps2:  0; savepath: 'HEROES'),
    (crc32: '19c57f79'; numlumps:  410; size:   6975569; nummaps1:  0; nummaps2: 32; savepath: 'HEROES2'),
    (crc32: '7f8e355b'; numlumps:  352; size:    173487; nummaps1:  0; nummaps2: 32; savepath: 'HoE64'),
    (crc32: 'c4d29ff0'; numlumps:  456; size:   7027154; nummaps1:  0; nummaps2: 32; savepath: 'HR'),
    (crc32: 'c5b234f4'; numlumps:  603; size:   6588096; nummaps1:  0; nummaps2: 32; savepath: 'hr2final'),
    (crc32: '3872f4b9'; numlumps:  471; size:  14070458; nummaps1:  0; nummaps2: 33; savepath: 'ht'),
    (crc32: '618e37d1'; numlumps: 1851; size:  11777864; nummaps1: 27; nummaps2:  0; savepath: 'Hyena'),
    (crc32: '790ba1b5'; numlumps:  714; size:   8397168; nummaps1:  0; nummaps2: 32; savepath: 'ICARUS'),
    (crc32: 'f388bead'; numlumps:  525; size:  10168066; nummaps1:  0; nummaps2: 27; savepath: 'ihmn'),
    (crc32: 'b203ed72'; numlumps:  525; size:  10132698; nummaps1:  0; nummaps2: 27; savepath: 'IHMNbeta'),
    (crc32: '671b1e64'; numlumps:  527; size:  13067031; nummaps1:  0; nummaps2: 32; savepath: 'illumi'),
    (crc32: '95a958ba'; numlumps:  445; size:  13248295; nummaps1:  0; nummaps2: 32; savepath: 'intercep'),
    (crc32: '7a879bab'; numlumps:  226; size:   3088705; nummaps1:  0; nummaps2: 16; savepath: 'INTIME'),
    (crc32: 'd5ee83b4'; numlumps: 1034; size:   9464487; nummaps1: 36; nummaps2:  0; savepath: 'INVASION'),
    (crc32: 'b329b61a'; numlumps:  492; size:   5732961; nummaps1:  0; nummaps2: 20; savepath: 'italo'),
    (crc32: 'd4f84b6a'; numlumps: 1057; size:  21026425; nummaps1:  0; nummaps2: 32; savepath: 'jenesis'),
    (crc32: '338bdc59'; numlumps: 1181; size:  18205945; nummaps1:  0; nummaps2: 17; savepath: 'joetune'),
    (crc32: '23349bd1'; numlumps: 3356; size: 162207136; nummaps1:  0; nummaps2: 22; savepath: 'joi_lstcv15'),
    (crc32: 'c41bf4a9'; numlumps: 2135; size:  45520073; nummaps1:  0; nummaps2: 32; savepath: 'JPCP'),
    (crc32: 'd6dd2141'; numlumps:  587; size:  16127865; nummaps1:  0; nummaps2:  9; savepath: 'khorus'),
    (crc32: 'be21b782'; numlumps:  587; size:  15055263; nummaps1:  0; nummaps2:  9; savepath: 'khorus1'),
    (crc32: 'dd041ca9'; numlumps:  408; size:   4096640; nummaps1:  0; nummaps2: 32; savepath: 'killadve'),
    (crc32: '0d85b04f'; numlumps:  388; size:   3377407; nummaps1:  0; nummaps2: 32; savepath: 'kssht'),
    (crc32: '0195d9c1'; numlumps:  459; size:  15954937; nummaps1:  0; nummaps2: 32; savepath: 'ksutra'),
    (crc32: '3b5d6d37'; numlumps:  397; size:   3438864; nummaps1:  0; nummaps2: 11; savepath: 'LASTEP1'),
    (crc32: 'cf879b85'; numlumps: 2051; size:  12471414; nummaps1: 34; nummaps2:  0; savepath: 'Loki'),
    (crc32: '340317ba'; numlumps:  346; size:   3143853; nummaps1:  0; nummaps2: 23; savepath: 'lostmaps'),
    (crc32: 'f6e28ea0'; numlumps:   57; size:   1088374; nummaps1:  0; nummaps2:  1; savepath: 'ma_sincity'),
    (crc32: '4758f270'; numlumps:  514; size:   5997531; nummaps1:  0; nummaps2: 32; savepath: 'manolaik'),
    (crc32: 'bfdfbcfc'; numlumps:  631; size:  10660908; nummaps1:  0; nummaps2: 32; savepath: 'MARSW301'),
    (crc32: '399f3284'; numlumps: 3920; size:  45087404; nummaps1:  0; nummaps2: 21; savepath: 'mayhem16'),
    (crc32: '1ae1275f'; numlumps: 2504; size:  29040931; nummaps1:  0; nummaps2: 29; savepath: 'mayhem17'),
    (crc32: 'd1706939'; numlumps:   23; size:    942878; nummaps1:  0; nummaps2:  1; savepath: 'mayhem17_updatev1'),
    (crc32: '03dfde1a'; numlumps: 1295; size:  14582859; nummaps1:  0; nummaps2: 34; savepath: 'MAYhem1500v1-50'),
    (crc32: '09f035ea'; numlumps: 1468; size:  21160569; nummaps1:  0; nummaps2: 35; savepath: 'MAYhem2048'),
    (crc32: '401cbfee'; numlumps:  179; size:   1942601; nummaps1: 16; nummaps2:  0; savepath: 'medley'),
    (crc32: '18631b12'; numlumps:  352; size:   4188076; nummaps1:  0; nummaps2: 32; savepath: 'mlm12'),
    (crc32: '38357752'; numlumps:  450; size:   7825282; nummaps1:  0; nummaps2: 32; savepath: 'MM'),
    (crc32: 'c2c001a5'; numlumps:  572; size:  11453000; nummaps1:  0; nummaps2: 32; savepath: 'MM2'),
    (crc32: '6568ba51'; numlumps:  588; size:  10946945; nummaps1:  0; nummaps2: 32; savepath: 'moonbld'),
    (crc32: '186c804b'; numlumps:  775; size:  16184974; nummaps1:  0; nummaps2: 17; savepath: 'MUTINY'),
    (crc32: '9b0d5926'; numlumps:  807; size:  22186414; nummaps1:  0; nummaps2: 32; savepath: 'ndcp'),
    (crc32: 'e881fbb4'; numlumps: 1398; size:  69450467; nummaps1:  0; nummaps2: 32; savepath: 'NDCP2'),
    (crc32: '44f1d987'; numlumps: 1110; size:  19830307; nummaps1: 38; nummaps2:  0; savepath: 'NEIS'),
    (crc32: 'ad7f9292'; numlumps:  108; size:   3819855; nummaps1:  0; nummaps2:  9; savepath: 'NERVE'),
    (crc32: 'd1610b49'; numlumps: 2518; size:  19619216; nummaps1: 36; nummaps2:  0; savepath: 'newmaps'),
    (crc32: '47f7b839'; numlumps: 1144; size:  42300707; nummaps1:  0; nummaps2: 17; savepath: 'NGmvmt2'),
    (crc32: '14e8830f'; numlumps:  300; size:   2753435; nummaps1: 27; nummaps2:  0; savepath: 'NIVELES'),
    (crc32: '5b8465e1'; numlumps:  340; size:   3804458; nummaps1: 27; nummaps2:  0; savepath: 'NJDOOM1'),
    (crc32: 'dd22e748'; numlumps:  340; size:   2733434; nummaps1: 27; nummaps2:  0; savepath: 'nmdu'),
    (crc32: '8d2fd51e'; numlumps:  443; size:  22873782; nummaps1:  0; nummaps2: 32; savepath: 'NOVA'),
    (crc32: '373e992d'; numlumps: 1080; size:  41146670; nummaps1:  0; nummaps2: 32; savepath: 'NOVA2'),
    (crc32: 'fe6d3aa1'; numlumps:  547; size:  30079583; nummaps1:  0; nummaps2: 40; savepath: 'PC_CP'),
    (crc32: '84a75656'; numlumps:  999; size:  21430729; nummaps1:  0; nummaps2: 49; savepath: 'PC_CP2'),
    (crc32: '457a584b'; numlumps: 3158; size:  21663600; nummaps1:  0; nummaps2: 32; savepath: 'perdgate'),
    (crc32: 'd6b5f43a'; numlumps:  356; size:   1870954; nummaps1:  0; nummaps2: 32; savepath: 'Phtga'),
    (crc32: '8472b8df'; numlumps: 1865; size:  20548765; nummaps1:  0; nummaps2: 17; savepath: 'pigeon01'),
    (crc32: 'a6f6cb3d'; numlumps:  352; size:   9396244; nummaps1:  0; nummaps2: 32; savepath: 'pizzaS'),
    (crc32: '9030f6aa'; numlumps:  761; size:  18131932; nummaps1:  0; nummaps2: 33; savepath: 'PL2'),
    (crc32: 'a4ea5010'; numlumps:  426; size:   6270770; nummaps1:  0; nummaps2: 33; savepath: 'pl1024'),
    (crc32: '9f2d899a'; numlumps:    7; size:   1564389; nummaps1:  0; nummaps2:  1; savepath: 'planisf2'),
    (crc32: 'd2693ff9'; numlumps: 2338; size:  17793739; nummaps1:  0; nummaps2: 32; savepath: 'PLUT3'),
    (crc32: '8c6bb568'; numlumps:  496; size:   6091678; nummaps1:  0; nummaps2: 32; savepath: 'PLUT4'),
    (crc32: '374718d6'; numlumps:  159; size:   2571268; nummaps1:  0; nummaps2:  9; savepath: 'PMB'),
    (crc32: '28761865'; numlumps:  725; size:  15409097; nummaps1:  0; nummaps2: 32; savepath: 'PRCP'),
    (crc32: '8b064d63'; numlumps:  260; size:   3966621; nummaps1:  0; nummaps2: 20; savepath: 'PULSE'),
    (crc32: '52078498'; numlumps:  240; size:   1912372; nummaps1: 19; nummaps2:  0; savepath: 'RAGE20'),
    (crc32: '951b14c8'; numlumps:  376; size:   9387758; nummaps1:  0; nummaps2: 12; savepath: 'RD1'),
    (crc32: 'a8e52c22'; numlumps:  798; size:   9085430; nummaps1:  0; nummaps2: 32; savepath: 'Rebirth1'),
    (crc32: '71dc13b3'; numlumps: 1380; size:  20882832; nummaps1:  0; nummaps2:  7; savepath: 'Recycled'),
    (crc32: '0d294ca5'; numlumps: 3164; size:  39460509; nummaps1: 36; nummaps2:  0; savepath: 'rekkrsa'),
    (crc32: '6b2f4a1d'; numlumps:  672; size:  11464486; nummaps1:  0; nummaps2: 32; savepath: 'REQUIEM'),
    (crc32: '3de57091'; numlumps: 2672; size:  41402553; nummaps1:  0; nummaps2: 32; savepath: 'Resurge'),
    (crc32: 'a336443b'; numlumps:  612; size:  12871032; nummaps1:  0; nummaps2: 32; savepath: 'REVERIE'),
    (crc32: '732ac7de'; numlumps:  513; size:   5177933; nummaps1:  0; nummaps2: 32; savepath: 'REVPRBLM'),
    (crc32: 'd3e7d706'; numlumps:  437; size:   4858320; nummaps1:  0; nummaps2: 32; savepath: 'RlmChaos'),
    (crc32: 'f45a028c'; numlumps:  943; size:  12976690; nummaps1:  0; nummaps2: 19; savepath: 'S2PV'),
    (crc32: '2f590a4b'; numlumps:  740; size:  11924260; nummaps1:  0; nummaps2: 30; savepath: 'SCI2'),
    (crc32: 'b2716118'; numlumps:  713; size:   4870909; nummaps1:  0; nummaps2: 23; savepath: 'scimitar'),
    (crc32: 'bff265f7'; numlumps:  511; size:   6215556; nummaps1:  0; nummaps2: 32; savepath: 'SCYTHE'),
    (crc32: '2f42f495'; numlumps: 1243; size:  31285404; nummaps1:  0; nummaps2: 32; savepath: 'scythe2'),
    (crc32: '77c5ae52'; numlumps: 1833; size:  41880800; nummaps1:  0; nummaps2: 35; savepath: 'sf3'),
    (crc32: '3eb107b1'; numlumps:  818; size:  21041587; nummaps1:  0; nummaps2: 32; savepath: 'SF2011'),
    (crc32: '9f2922bd'; numlumps: 1162; size:  53134881; nummaps1:  0; nummaps2: 35; savepath: 'SF2012_final'),
    (crc32: '5a80c2cd'; numlumps:  145; size:   4525740; nummaps1:  9; nummaps2:  0; savepath: 'SIGIL'),
    (crc32: '4fa56fdb'; numlumps:  154; size:   4483345; nummaps1:  9; nummaps2:  0; savepath: 'SIGIL_COMPAT'),
    (crc32: '45f791df'; numlumps: 1223; size:  12789294; nummaps1:  0; nummaps2: 33; savepath: 'SihR2fix'),
    (crc32: '487bc534'; numlumps:  390; size:   1599822; nummaps1:  0; nummaps2: 32; savepath: 'smfsp8'),
    (crc32: '3377e330'; numlumps: 1028; size:  28742634; nummaps1:  0; nummaps2: 33; savepath: 'SODfinal'),
    (crc32: '84205f65'; numlumps: 1432; size:  38752745; nummaps1:  0; nummaps2: 32; savepath: 'spmaster'),
    (crc32: 'be0e468d'; numlumps: 1364; size:  13502597; nummaps1:  0; nummaps2: 32; savepath: 'STRAIN'),
    (crc32: '2c1f6d54'; numlumps: 1739; size:  31939496; nummaps1:  0; nummaps2: 22; savepath: 'SUITCGOR'),
    (crc32: 'e8f7af85'; numlumps:  349; size:  29868268; nummaps1:  0; nummaps2: 14; savepath: 'sunder'),
    (crc32: 'e6304af9'; numlumps: 1816; size: 108211154; nummaps1:  0; nummaps2: 20; savepath: 'Sunder_2206'),
    (crc32: 'f5943e82'; numlumps:   55; size:   7730017; nummaps1:  0; nummaps2:  5; savepath: 'sunder_old'),
    (crc32: 'd8f93a6e'; numlumps: 2788; size:  65966252; nummaps1:  0; nummaps2: 32; savepath: 'sunlust'),
    (crc32: '1f986a3a'; numlumps:  553; size:   5297832; nummaps1:  0; nummaps2: 32; savepath: 'swideath'),
    (crc32: '848e6a7b'; numlumps:  439; size:   5374973; nummaps1: 36; nummaps2:  0; savepath: 'swtchrm'),
    (crc32: '8e2e566c'; numlumps:  319; size:  10136024; nummaps1:  0; nummaps2: 18; savepath: 'Telekom'),
    (crc32: '1df63c90'; numlumps: 1198; size:  48898538; nummaps1:  0; nummaps2: 20; savepath: 'thtthren'),
    (crc32: '7a5af83e'; numlumps:  503; size:   3817609; nummaps1:  0; nummaps2: 32; savepath: 'TickTock'),
    (crc32: '9dac2897'; numlumps:  491; size:   9983277; nummaps1:  0; nummaps2: 32; savepath: 'tj'),
    (crc32: 'e15dafac'; numlumps: 1585; size:  28776797; nummaps1:  0; nummaps2: 32; savepath: 'TNTR'),
    (crc32: '6b43b6ef'; numlumps:  437; size:  13534938; nummaps1:  0; nummaps2: 20; savepath: 'tom19'),
    (crc32: '3df46d43'; numlumps: 1000; size:   8744509; nummaps1:  0; nummaps2: 32; savepath: 'toon2b'),
    (crc32: '193c8f03'; numlumps:  477; size:   9458999; nummaps1:  0; nummaps2: 17; savepath: 'TPBM'),
    (crc32: 'c30b46cd'; numlumps: 1187; size:  21813381; nummaps1:  0; nummaps2: 35; savepath: 'trocket'),
    (crc32: '27fdd908'; numlumps:  127; size:   1970384; nummaps1:  0; nummaps2:  1; savepath: 'TTT'),
    (crc32: '47d8d23b'; numlumps: 2425; size:  18719245; nummaps1: 36; nummaps2:  0; savepath: 'TUCQR6'),
    (crc32: 'cd78c517'; numlumps:  514; size:   5363532; nummaps1:  0; nummaps2: 32; savepath: 'TVR!'),
    (crc32: 'ac6e012e'; numlumps:  474; size:   5243344; nummaps1:  3; nummaps2: 32; savepath: 'Twzone'),
    (crc32: '6c7c2f36'; numlumps:  882; size:  10067828; nummaps1:  0; nummaps2: 32; savepath: 'twzone2'),
    (crc32: '19f6bfb3'; numlumps:  456; size:   7908969; nummaps1:  0; nummaps2: 32; savepath: 'Ultimate'),
    (crc32: '803b1ba4'; numlumps:  334; size:   1750716; nummaps1: 27; nummaps2:  0; savepath: 'unalign'),
    (crc32: 'dac03d98'; numlumps:  391; size:   2526651; nummaps1:  0; nummaps2: 32; savepath: 'unnecess'),
    (crc32: 'ac7500dc'; numlumps: 2402; size:  29236097; nummaps1:  0; nummaps2: 32; savepath: 'ur_final'),
    (crc32: '7d5e1287'; numlumps:  705; size:  13169499; nummaps1:  0; nummaps2: 32; savepath: 'urania'),
    (crc32: '93256823'; numlumps: 5185; size:  96578759; nummaps1:  0; nummaps2: 32; savepath: 'Valiant'),
    (crc32: '9e5ebe56'; numlumps: 5570; size:  96978763; nummaps1:  0; nummaps2: 32; savepath: 'valve'),
    (crc32: '98e3fb99'; numlumps:  438; size:  18342748; nummaps1:  0; nummaps2: 32; savepath: 'VILE'),
    (crc32: 'c7c4e6d3'; numlumps:  352; size:   5568032; nummaps1:  0; nummaps2: 32; savepath: 'vilecore1'),
    (crc32: 'bdf633ce'; numlumps:  246; size:   3633418; nummaps1:  0; nummaps2: 17; savepath: 'VISPIRE'),
    (crc32: '965578c7'; numlumps:  793; size:  10709500; nummaps1:  0; nummaps2: 32; savepath: 'war_3'),
    (crc32: '229507d8'; numlumps:  400; size:   4757287; nummaps1: 36; nummaps2:  0; savepath: 'WD13'),
    (crc32: 'd3a8e90a'; numlumps: 3862; size:  81645310; nummaps1:  0; nummaps2: 32; savepath: 'white2'),
    (crc32: 'd79e0d8e'; numlumps:  845; size:  37101320; nummaps1:  0; nummaps2: 36; savepath: 'WOS'),
    (crc32: 'bd4c8385'; numlumps:  640; size:  43126602; nummaps1:  0; nummaps2: 32; savepath: 'ZOF'),
    (crc32: 'e4be5db7'; numlumps:  460; size:   3052784; nummaps1:  0; nummaps2: 33; savepath: 'zone300')
  );

var
  savepath: string = 'doom';

//==============================================================================
//
// D_CheckCustomIWad
//
//==============================================================================
function D_CheckCustomIWad(const filename: string): boolean;
var
  sname: string;
  crc32: string[8];
  wi: wadinfo_t;
  numlmps: integer;
  f: TFile;
  size: integer;
  i: integer;
begin
  if not fexists(filename) then
  begin
    result := false;
    exit;
  end;

  result := true; // Assuming we will find a valid IWAD
  f := TFile.Create(filename, fOpenReadOnly);
  size := f.Size;
  if f.Read(wi, SizeOf(wadinfo_t)) <> SizeOf(wadinfo_t) then
    numlmps := 0
  else
    numlmps := wi.numlumps;
  f.Free;
  crc32 := '';
  for i := 0 to NUMIWADDETECTITEMS - 1 do
    if (iwadtbl[i].size = size) and (iwadtbl[i].numlumps = numlmps) then
    begin
      if crc32 = '' then
        crc32 := GetCRC32(filename);
      if iwadtbl[i].crc32 = crc32 then
      begin
        gameversion := iwadtbl[i].version;
        customgame := iwadtbl[i].customgame;
        savepath := iwadtbl[i].savepath;
        exit;
      end;
    end;

  sname := strupper(fname(filename));
  // JVAL: Chex Support
  if sname = 'CHEX.WAD' then
    if customgame <> cg_chex2 then
    begin
      gameversion := exe_chex;
      customgame := cg_chex;
      savepath := 'chex';
      Exit;
    end;
  if sname = 'CHEX2.WAD' then
  begin
    gameversion := exe_chex;
    customgame := cg_chex2;
    savepath := 'chex2';
    exit;
  end;
  // JVAL: Hacx Support
  if sname = 'HACX.WAD' then
  begin
    gameversion := exe_hacx;
    customgame := cg_hacx;
    savepath := 'hacx';
    exit;
  end;
  // JVAL: FreeDoom Support
  // Doomworld post -> https://www.doomworld.com/vb/freedoom/66965-source-port-support-for-new-iwad-names/
  if sname = 'FREEDOOM.WAD' then
  begin
    customgame := cg_freedoom;
    savepath := 'freedoom';
    exit;
  end;
  if sname = 'FREEDOOM1.WAD' then
  begin
    customgame := cg_freedoom;
    savepath := 'freedoom1';
    exit;
  end;
  if sname = 'FREEDOOM2.WAD' then
  begin
    customgame := cg_freedoom;
    savepath := 'freedoom2';
    exit;
  end;
  if sname = 'FREEDM.WAD' then
  begin
    customgame := cg_freedoom;
    savepath := 'freedm';
    exit;
  end;
  // JVAL: FINAL DOOM
  if sname = 'TNT.WAD' then
  begin
    gameversion := exe_final2;
    savepath := 'tnt';
    exit;
  end;
  if sname = 'PLUTONIA.WAD' then
  begin
    gameversion := exe_final2;
    savepath := 'plutonia';
    exit;
  end;
  // JVAL: DOOM2F
  if sname = 'DOOM2F.WAD' then
  begin
    gameversion := exe_doom_1_8;
    savepath := 'doom2f';
    exit;
  end;
  // JVAL: DOOM2
  if sname = 'DOOM2.WAD' then
  begin
    gameversion := exe_doom_1_9;
    savepath := 'doom2';
    exit;
  end;
  // JVAL: ULTIMATE DOOM
  if sname = 'DOOM.WAD' then
  begin
    gameversion := exe_ultimate;
    savepath := 'doom';
    exit;
  end;
  if sname = 'DOOM1.WAD' then
  begin
    gameversion := exe_ultimate;
    savepath := 'doom1';
    exit;
  end;
  if sname = 'DOOMU.WAD' then
  begin
    gameversion := exe_ultimate;
    savepath := 'doomu';
    exit;
  end;

  result := false; // Well, not valid IWAD found
end;

//==============================================================================
//
// D_CheckCustomPWad
//
//==============================================================================
function D_CheckCustomPWad(const filename: string): boolean;
var
  crc32: string[8];
  wi: wadinfo_t;
  numlmps: integer;
  f: TFile;
  size: integer;
  i: integer;
begin
  if not fexists(filename) then
  begin
    result := false;
    exit;
  end;

  result := true; // Assuming we will find a valid IWAD
  f := TFile.Create(filename, fOpenReadOnly);
  size := f.Size;
  if f.Read(wi, SizeOf(wadinfo_t)) <> SizeOf(wadinfo_t) then
    numlmps := 0
  else
    numlmps := wi.numlumps;
  f.Free;
  crc32 := '';
  for i := 0 to NUMPWADDETECTITEMS - 1 do
    if (pwadtbl[i].size = size) and (pwadtbl[i].numlumps = numlmps) then
    begin
      if crc32 = '' then
        crc32 := GetCRC32(filename);
      if pwadtbl[i].crc32 = crc32 then
      begin
        savepath := pwadtbl[i].savepath;
        exit;
      end;
    end;
  result := false;
end;

//==============================================================================
//
// D_CheckUnknownWad
//
//==============================================================================
function D_CheckUnknownWad(const filename: string): boolean;
const
  sNUMS = '0123456789';
var
  numlumps: integer;
  nummaps1, nummaps2: integer;
  name, s: string;
  crc: string;
  wad: TWadReader;
  i: integer;
begin
  crc := GetCRC32(filename);

  wad := TWadReader.Create;
  wad.OpenWadFile(filename);
  numlumps := wad.NumEntries;

  splitstring_ch(fname(filename), name, s, '.');
  nummaps1 := 0;
  nummaps2 := 0;

  for i := 0 to numlumps - 1 do
  begin
    s := strupper(wad.EntryName(i));
    if Length(s) = 4 then
    begin
      if s[1] = 'E' then
        if s[3] = 'M' then
          if Pos(s[2], sNUMS) > 0 then
            if Pos(s[4], sNUMS) > 0 then
              inc(nummaps1);
    end
    else if Length(s) = 5 then
    begin
      if s[1] = 'M' then
        if s[2] = 'A' then
          if s[3] = 'P' then
            if Pos(s[4], sNUMS) > 0 then
              if Pos(s[5], sNUMS) > 0 then
                inc(nummaps2);
    end
  end;
  wad.Free;

  result := nummaps1 + nummaps2 > 0;
  if result then
    savepath := name + '_' + crc;
end;

//==============================================================================
//
// D_CheckCustomWad
//
//==============================================================================
procedure D_CheckCustomWad(const filename: string);
begin
  if not D_CheckCustomIWad(filename) then
    if not D_CheckCustomPWad(filename) then
      D_CheckUnknownWad(filename);
end;

//==============================================================================
//
// D_GetSavePath
//
//==============================================================================
function D_GetSavePath: string;
var
  s: string;
begin
  s := 'DATA';
  MkDir(M_SaveFileName(s));
  s := s + '\SAVES';
  MkDir(M_SaveFileName(s));
  s := s + '\' + savepath;
  MkDir(M_SaveFileName(s));
  result := 'DATA\SAVES\' + savepath + '\';
end;

end.
