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
//  Thing frame/state LUT,
//  generated by multigen utilitiy.
//  This one is the original DOOM version, preserved.
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit info_h;

interface

uses
  d_delphi,
  d_think,
  r_renderstyle,
  p_gender,
  sc_params;

type
  spritenum_t = (
    SPR_IMPX, SPR_ACLO, SPR_PTN1, SPR_SHLD, SPR_SHD2,
    SPR_BAGH, SPR_SPMP, SPR_INVS, SPR_PTN2, SPR_SOAR,
    SPR_INVU, SPR_PWBK, SPR_EGGC, SPR_EGGM, SPR_FX01,
    SPR_SPHL, SPR_TRCH, SPR_FBMB, SPR_XPL1, SPR_ATLP,
    SPR_PPOD, SPR_AMG1, SPR_SPSH, SPR_LVAS, SPR_SLDG,
    SPR_SKH1, SPR_SKH2, SPR_SKH3, SPR_SKH4, SPR_CHDL,
    SPR_SRTC, SPR_SMPL, SPR_STGS, SPR_STGL, SPR_STCS,
    SPR_STCL, SPR_KFR1, SPR_BARL, SPR_BRPL, SPR_MOS1,
    SPR_MOS2, SPR_WTRH, SPR_HCOR, SPR_KGZ1, SPR_KGZB,
    SPR_KGZG, SPR_KGZY, SPR_VLCO, SPR_VFBL, SPR_VTFB,
    SPR_SFFI, SPR_TGLT, SPR_TELE, SPR_STFF, SPR_PUF3,
    SPR_PUF4, SPR_BEAK, SPR_WGNT, SPR_GAUN, SPR_PUF1,
    SPR_WBLS, SPR_BLSR, SPR_FX18, SPR_FX17, SPR_WMCE,
    SPR_MACE, SPR_FX02, SPR_WSKL, SPR_HROD, SPR_FX00,
    SPR_FX20, SPR_FX21, SPR_FX22, SPR_FX23, SPR_GWND,
    SPR_PUF2, SPR_WPHX, SPR_PHNX, SPR_FX04, SPR_FX08,
    SPR_FX09, SPR_WBOW, SPR_CRBW, SPR_FX03, SPR_BLOD,
    SPR_PLAY, SPR_FDTH, SPR_BSKL, SPR_CHKN, SPR_MUMM,
    SPR_FX15, SPR_BEAS, SPR_FRB1, SPR_SNKE, SPR_SNFX,
    SPR_HEAD, SPR_FX05, SPR_FX06, SPR_FX07, SPR_CLNK,
    SPR_WZRD, SPR_FX11, SPR_FX10, SPR_KNIG, SPR_SPAX,
    SPR_RAXE, SPR_SRCR, SPR_FX14, SPR_SOR2, SPR_SDTH,
    SPR_FX16, SPR_MNTR, SPR_FX12, SPR_FX13, SPR_AKYY,
    SPR_BKYY, SPR_CKYY, SPR_AMG2, SPR_AMM1, SPR_AMM2,
    SPR_AMC1, SPR_AMC2, SPR_AMS1, SPR_AMS2, SPR_AMP1,
    SPR_AMP2, SPR_AMB1, SPR_AMB2, SPR_TNT1, SPR_NULL,
    DO_NUMSPRITES
  );

  statenum_t = (
    S_NULL,
    S_FREETARGMOBJ,
    S_ITEM_PTN1_1,
    S_ITEM_PTN1_2,
    S_ITEM_PTN1_3,
    S_ITEM_SHLD1,
    S_ITEM_SHD2_1,
    S_ITEM_BAGH1,
    S_ITEM_SPMP1,
    S_HIDESPECIAL1,
    S_HIDESPECIAL2,
    S_HIDESPECIAL3,
    S_HIDESPECIAL4,
    S_HIDESPECIAL5,
    S_HIDESPECIAL6,
    S_HIDESPECIAL7,
    S_HIDESPECIAL8,
    S_HIDESPECIAL9,
    S_HIDESPECIAL10,
    S_HIDESPECIAL11,
    S_DORMANTARTI1,
    S_DORMANTARTI2,
    S_DORMANTARTI3,
    S_DORMANTARTI4,
    S_DORMANTARTI5,
    S_DORMANTARTI6,
    S_DORMANTARTI7,
    S_DORMANTARTI8,
    S_DORMANTARTI9,
    S_DORMANTARTI10,
    S_DORMANTARTI11,
    S_DORMANTARTI12,
    S_DORMANTARTI13,
    S_DORMANTARTI14,
    S_DORMANTARTI15,
    S_DORMANTARTI16,
    S_DORMANTARTI17,
    S_DORMANTARTI18,
    S_DORMANTARTI19,
    S_DORMANTARTI20,
    S_DORMANTARTI21,
    S_DEADARTI1,
    S_DEADARTI2,
    S_DEADARTI3,
    S_DEADARTI4,
    S_DEADARTI5,
    S_DEADARTI6,
    S_DEADARTI7,
    S_DEADARTI8,
    S_DEADARTI9,
    S_DEADARTI10,
    S_ARTI_INVS1,
    S_ARTI_PTN2_1,
    S_ARTI_PTN2_2,
    S_ARTI_PTN2_3,
    S_ARTI_SOAR1,
    S_ARTI_SOAR2,
    S_ARTI_SOAR3,
    S_ARTI_SOAR4,
    S_ARTI_INVU1,
    S_ARTI_INVU2,
    S_ARTI_INVU3,
    S_ARTI_INVU4,
    S_ARTI_PWBK1,
    S_ARTI_EGGC1,
    S_ARTI_EGGC2,
    S_ARTI_EGGC3,
    S_ARTI_EGGC4,
    S_EGGFX1,
    S_EGGFX2,
    S_EGGFX3,
    S_EGGFX4,
    S_EGGFX5,
    S_EGGFXI1_1,
    S_EGGFXI1_2,
    S_EGGFXI1_3,
    S_EGGFXI1_4,
    S_ARTI_SPHL1,
    S_ARTI_TRCH1,
    S_ARTI_TRCH2,
    S_ARTI_TRCH3,
    S_ARTI_FBMB1,
    S_FIREBOMB1,
    S_FIREBOMB2,
    S_FIREBOMB3,
    S_FIREBOMB4,
    S_FIREBOMB5,
    S_FIREBOMB6,
    S_FIREBOMB7,
    S_FIREBOMB8,
    S_FIREBOMB9,
    S_FIREBOMB10,
    S_FIREBOMB11,
    S_ARTI_ATLP1,
    S_ARTI_ATLP2,
    S_ARTI_ATLP3,
    S_ARTI_ATLP4,
    S_POD_WAIT1,
    S_POD_PAIN1,
    S_POD_DIE1,
    S_POD_DIE2,
    S_POD_DIE3,
    S_POD_DIE4,
    S_POD_GROW1,
    S_POD_GROW2,
    S_POD_GROW3,
    S_POD_GROW4,
    S_POD_GROW5,
    S_POD_GROW6,
    S_POD_GROW7,
    S_POD_GROW8,
    S_PODGOO1,
    S_PODGOO2,
    S_PODGOOX,
    S_PODGENERATOR,
    S_SPLASH1,
    S_SPLASH2,
    S_SPLASH3,
    S_SPLASH4,
    S_SPLASHX,
    S_SPLASHBASE1,
    S_SPLASHBASE2,
    S_SPLASHBASE3,
    S_SPLASHBASE4,
    S_SPLASHBASE5,
    S_SPLASHBASE6,
    S_SPLASHBASE7,
    S_LAVASPLASH1,
    S_LAVASPLASH2,
    S_LAVASPLASH3,
    S_LAVASPLASH4,
    S_LAVASPLASH5,
    S_LAVASPLASH6,
    S_LAVASMOKE1,
    S_LAVASMOKE2,
    S_LAVASMOKE3,
    S_LAVASMOKE4,
    S_LAVASMOKE5,
    S_SLUDGECHUNK1,
    S_SLUDGECHUNK2,
    S_SLUDGECHUNK3,
    S_SLUDGECHUNK4,
    S_SLUDGECHUNKX,
    S_SLUDGESPLASH1,
    S_SLUDGESPLASH2,
    S_SLUDGESPLASH3,
    S_SLUDGESPLASH4,
    S_SKULLHANG70_1,
    S_SKULLHANG60_1,
    S_SKULLHANG45_1,
    S_SKULLHANG35_1,
    S_CHANDELIER1,
    S_CHANDELIER2,
    S_CHANDELIER3,
    S_SERPTORCH1,
    S_SERPTORCH2,
    S_SERPTORCH3,
    S_SMALLPILLAR,
    S_STALAGMITESMALL,
    S_STALAGMITELARGE,
    S_STALACTITESMALL,
    S_STALACTITELARGE,
    S_FIREBRAZIER1,
    S_FIREBRAZIER2,
    S_FIREBRAZIER3,
    S_FIREBRAZIER4,
    S_FIREBRAZIER5,
    S_FIREBRAZIER6,
    S_FIREBRAZIER7,
    S_FIREBRAZIER8,
    S_BARREL,
    S_BRPILLAR,
    S_MOSS1,
    S_MOSS2,
    S_WALLTORCH1,
    S_WALLTORCH2,
    S_WALLTORCH3,
    S_HANGINGCORPSE,
    S_KEYGIZMO1,
    S_KEYGIZMO2,
    S_KEYGIZMO3,
    S_KGZ_START,
    S_KGZ_BLUEFLOAT1,
    S_KGZ_GREENFLOAT1,
    S_KGZ_YELLOWFLOAT1,
    S_VOLCANO1,
    S_VOLCANO2,
    S_VOLCANO3,
    S_VOLCANO4,
    S_VOLCANO5,
    S_VOLCANO6,
    S_VOLCANO7,
    S_VOLCANO8,
    S_VOLCANO9,
    S_VOLCANOBALL1,
    S_VOLCANOBALL2,
    S_VOLCANOBALLX1,
    S_VOLCANOBALLX2,
    S_VOLCANOBALLX3,
    S_VOLCANOBALLX4,
    S_VOLCANOBALLX5,
    S_VOLCANOBALLX6,
    S_VOLCANOTBALL1,
    S_VOLCANOTBALL2,
    S_VOLCANOTBALLX1,
    S_VOLCANOTBALLX2,
    S_VOLCANOTBALLX3,
    S_VOLCANOTBALLX4,
    S_VOLCANOTBALLX5,
    S_VOLCANOTBALLX6,
    S_VOLCANOTBALLX7,
    S_TELEGLITGEN1,
    S_TELEGLITGEN2,
    S_TELEGLITTER1_1,
    S_TELEGLITTER1_2,
    S_TELEGLITTER1_3,
    S_TELEGLITTER1_4,
    S_TELEGLITTER1_5,
    S_TELEGLITTER2_1,
    S_TELEGLITTER2_2,
    S_TELEGLITTER2_3,
    S_TELEGLITTER2_4,
    S_TELEGLITTER2_5,
    S_TFOG1,
    S_TFOG2,
    S_TFOG3,
    S_TFOG4,
    S_TFOG5,
    S_TFOG6,
    S_TFOG7,
    S_TFOG8,
    S_TFOG9,
    S_TFOG10,
    S_TFOG11,
    S_TFOG12,
    S_TFOG13,
    S_LIGHTDONE,
    S_STAFFREADY,
    S_STAFFDOWN,
    S_STAFFUP,
    S_STAFFREADY2_1,
    S_STAFFREADY2_2,
    S_STAFFREADY2_3,
    S_STAFFDOWN2,
    S_STAFFUP2,
    S_STAFFATK1_1,
    S_STAFFATK1_2,
    S_STAFFATK1_3,
    S_STAFFATK2_1,
    S_STAFFATK2_2,
    S_STAFFATK2_3,
    S_STAFFPUFF1,
    S_STAFFPUFF2,
    S_STAFFPUFF3,
    S_STAFFPUFF4,
    S_STAFFPUFF2_1,
    S_STAFFPUFF2_2,
    S_STAFFPUFF2_3,
    S_STAFFPUFF2_4,
    S_STAFFPUFF2_5,
    S_STAFFPUFF2_6,
    S_BEAKREADY,
    S_BEAKDOWN,
    S_BEAKUP,
    S_BEAKATK1_1,
    S_BEAKATK2_1,
    S_WGNT,
    S_GAUNTLETREADY,
    S_GAUNTLETDOWN,
    S_GAUNTLETUP,
    S_GAUNTLETREADY2_1,
    S_GAUNTLETREADY2_2,
    S_GAUNTLETREADY2_3,
    S_GAUNTLETDOWN2,
    S_GAUNTLETUP2,
    S_GAUNTLETATK1_1,
    S_GAUNTLETATK1_2,
    S_GAUNTLETATK1_3,
    S_GAUNTLETATK1_4,
    S_GAUNTLETATK1_5,
    S_GAUNTLETATK1_6,
    S_GAUNTLETATK1_7,
    S_GAUNTLETATK2_1,
    S_GAUNTLETATK2_2,
    S_GAUNTLETATK2_3,
    S_GAUNTLETATK2_4,
    S_GAUNTLETATK2_5,
    S_GAUNTLETATK2_6,
    S_GAUNTLETATK2_7,
    S_GAUNTLETPUFF1_1,
    S_GAUNTLETPUFF1_2,
    S_GAUNTLETPUFF1_3,
    S_GAUNTLETPUFF1_4,
    S_GAUNTLETPUFF2_1,
    S_GAUNTLETPUFF2_2,
    S_GAUNTLETPUFF2_3,
    S_GAUNTLETPUFF2_4,
    S_BLSR,
    S_BLASTERREADY,
    S_BLASTERDOWN,
    S_BLASTERUP,
    S_BLASTERATK1_1,
    S_BLASTERATK1_2,
    S_BLASTERATK1_3,
    S_BLASTERATK1_4,
    S_BLASTERATK1_5,
    S_BLASTERATK1_6,
    S_BLASTERATK2_1,
    S_BLASTERATK2_2,
    S_BLASTERATK2_3,
    S_BLASTERATK2_4,
    S_BLASTERATK2_5,
    S_BLASTERATK2_6,
    S_BLASTERFX1_1,
    S_BLASTERFXI1_1,
    S_BLASTERFXI1_2,
    S_BLASTERFXI1_3,
    S_BLASTERFXI1_4,
    S_BLASTERFXI1_5,
    S_BLASTERFXI1_6,
    S_BLASTERFXI1_7,
    S_BLASTERSMOKE1,
    S_BLASTERSMOKE2,
    S_BLASTERSMOKE3,
    S_BLASTERSMOKE4,
    S_BLASTERSMOKE5,
    S_RIPPER1,
    S_RIPPER2,
    S_RIPPERX1,
    S_RIPPERX2,
    S_RIPPERX3,
    S_RIPPERX4,
    S_RIPPERX5,
    S_BLASTERPUFF1_1,
    S_BLASTERPUFF1_2,
    S_BLASTERPUFF1_3,
    S_BLASTERPUFF1_4,
    S_BLASTERPUFF1_5,
    S_BLASTERPUFF2_1,
    S_BLASTERPUFF2_2,
    S_BLASTERPUFF2_3,
    S_BLASTERPUFF2_4,
    S_BLASTERPUFF2_5,
    S_BLASTERPUFF2_6,
    S_BLASTERPUFF2_7,
    S_WMCE,
    S_MACEREADY,
    S_MACEDOWN,
    S_MACEUP,
    S_MACEATK1_1,
    S_MACEATK1_2,
    S_MACEATK1_3,
    S_MACEATK1_4,
    S_MACEATK1_5,
    S_MACEATK1_6,
    S_MACEATK1_7,
    S_MACEATK1_8,
    S_MACEATK1_9,
    S_MACEATK1_10,
    S_MACEATK2_1,
    S_MACEATK2_2,
    S_MACEATK2_3,
    S_MACEATK2_4,
    S_MACEFX1_1,
    S_MACEFX1_2,
    S_MACEFXI1_1,
    S_MACEFXI1_2,
    S_MACEFXI1_3,
    S_MACEFXI1_4,
    S_MACEFXI1_5,
    S_MACEFX2_1,
    S_MACEFX2_2,
    S_MACEFXI2_1,
    S_MACEFX3_1,
    S_MACEFX3_2,
    S_MACEFX4_1,
    S_MACEFXI4_1,
    S_WSKL,
    S_HORNRODREADY,
    S_HORNRODDOWN,
    S_HORNRODUP,
    S_HORNRODATK1_1,
    S_HORNRODATK1_2,
    S_HORNRODATK1_3,
    S_HORNRODATK2_1,
    S_HORNRODATK2_2,
    S_HORNRODATK2_3,
    S_HORNRODATK2_4,
    S_HORNRODATK2_5,
    S_HORNRODATK2_6,
    S_HORNRODATK2_7,
    S_HORNRODATK2_8,
    S_HORNRODATK2_9,
    S_HRODFX1_1,
    S_HRODFX1_2,
    S_HRODFXI1_1,
    S_HRODFXI1_2,
    S_HRODFXI1_3,
    S_HRODFXI1_4,
    S_HRODFXI1_5,
    S_HRODFXI1_6,
    S_HRODFX2_1,
    S_HRODFX2_2,
    S_HRODFX2_3,
    S_HRODFX2_4,
    S_HRODFXI2_1,
    S_HRODFXI2_2,
    S_HRODFXI2_3,
    S_HRODFXI2_4,
    S_HRODFXI2_5,
    S_HRODFXI2_6,
    S_HRODFXI2_7,
    S_HRODFXI2_8,
    S_RAINPLR1_1,
    S_RAINPLR2_1,
    S_RAINPLR3_1,
    S_RAINPLR4_1,
    S_RAINPLR1X_1,
    S_RAINPLR1X_2,
    S_RAINPLR1X_3,
    S_RAINPLR1X_4,
    S_RAINPLR1X_5,
    S_RAINPLR2X_1,
    S_RAINPLR2X_2,
    S_RAINPLR2X_3,
    S_RAINPLR2X_4,
    S_RAINPLR2X_5,
    S_RAINPLR3X_1,
    S_RAINPLR3X_2,
    S_RAINPLR3X_3,
    S_RAINPLR3X_4,
    S_RAINPLR3X_5,
    S_RAINPLR4X_1,
    S_RAINPLR4X_2,
    S_RAINPLR4X_3,
    S_RAINPLR4X_4,
    S_RAINPLR4X_5,
    S_RAINAIRXPLR1_1,
    S_RAINAIRXPLR2_1,
    S_RAINAIRXPLR3_1,
    S_RAINAIRXPLR4_1,
    S_RAINAIRXPLR1_2,
    S_RAINAIRXPLR2_2,
    S_RAINAIRXPLR3_2,
    S_RAINAIRXPLR4_2,
    S_RAINAIRXPLR1_3,
    S_RAINAIRXPLR2_3,
    S_RAINAIRXPLR3_3,
    S_RAINAIRXPLR4_3,
    S_GOLDWANDREADY,
    S_GOLDWANDDOWN,
    S_GOLDWANDUP,
    S_GOLDWANDATK1_1,
    S_GOLDWANDATK1_2,
    S_GOLDWANDATK1_3,
    S_GOLDWANDATK1_4,
    S_GOLDWANDATK2_1,
    S_GOLDWANDATK2_2,
    S_GOLDWANDATK2_3,
    S_GOLDWANDATK2_4,
    S_GWANDFX1_1,
    S_GWANDFX1_2,
    S_GWANDFXI1_1,
    S_GWANDFXI1_2,
    S_GWANDFXI1_3,
    S_GWANDFXI1_4,
    S_GWANDFX2_1,
    S_GWANDFX2_2,
    S_GWANDPUFF1_1,
    S_GWANDPUFF1_2,
    S_GWANDPUFF1_3,
    S_GWANDPUFF1_4,
    S_GWANDPUFF1_5,
    S_WPHX,
    S_PHOENIXREADY,
    S_PHOENIXDOWN,
    S_PHOENIXUP,
    S_PHOENIXATK1_1,
    S_PHOENIXATK1_2,
    S_PHOENIXATK1_3,
    S_PHOENIXATK1_4,
    S_PHOENIXATK1_5,
    S_PHOENIXATK2_1,
    S_PHOENIXATK2_2,
    S_PHOENIXATK2_3,
    S_PHOENIXATK2_4,
    S_PHOENIXFX1_1,
    S_PHOENIXFXI1_1,
    S_PHOENIXFXI1_2,
    S_PHOENIXFXI1_3,
    S_PHOENIXFXI1_4,
    S_PHOENIXFXI1_5,
    S_PHOENIXFXI1_6,
    S_PHOENIXFXI1_7,
    S_PHOENIXFXI1_8,
    S_PHOENIXPUFF1,
    S_PHOENIXPUFF2,
    S_PHOENIXPUFF3,
    S_PHOENIXPUFF4,
    S_PHOENIXPUFF5,
    S_PHOENIXFX2_1,
    S_PHOENIXFX2_2,
    S_PHOENIXFX2_3,
    S_PHOENIXFX2_4,
    S_PHOENIXFX2_5,
    S_PHOENIXFX2_6,
    S_PHOENIXFX2_7,
    S_PHOENIXFX2_8,
    S_PHOENIXFX2_9,
    S_PHOENIXFX2_10,
    S_PHOENIXFXI2_1,
    S_PHOENIXFXI2_2,
    S_PHOENIXFXI2_3,
    S_PHOENIXFXI2_4,
    S_PHOENIXFXI2_5,
    S_WBOW,
    S_CRBOW1,
    S_CRBOW2,
    S_CRBOW3,
    S_CRBOW4,
    S_CRBOW5,
    S_CRBOW6,
    S_CRBOW7,
    S_CRBOW8,
    S_CRBOW9,
    S_CRBOW10,
    S_CRBOW11,
    S_CRBOW12,
    S_CRBOW13,
    S_CRBOW14,
    S_CRBOW15,
    S_CRBOW16,
    S_CRBOW17,
    S_CRBOW18,
    S_CRBOWDOWN,
    S_CRBOWUP,
    S_CRBOWATK1_1,
    S_CRBOWATK1_2,
    S_CRBOWATK1_3,
    S_CRBOWATK1_4,
    S_CRBOWATK1_5,
    S_CRBOWATK1_6,
    S_CRBOWATK1_7,
    S_CRBOWATK1_8,
    S_CRBOWATK2_1,
    S_CRBOWATK2_2,
    S_CRBOWATK2_3,
    S_CRBOWATK2_4,
    S_CRBOWATK2_5,
    S_CRBOWATK2_6,
    S_CRBOWATK2_7,
    S_CRBOWATK2_8,
    S_CRBOWFX1,
    S_CRBOWFXI1_1,
    S_CRBOWFXI1_2,
    S_CRBOWFXI1_3,
    S_CRBOWFX2,
    S_CRBOWFX3,
    S_CRBOWFXI3_1,
    S_CRBOWFXI3_2,
    S_CRBOWFXI3_3,
    S_CRBOWFX4_1,
    S_CRBOWFX4_2,
    S_BLOOD1,
    S_BLOOD2,
    S_BLOOD3,
    S_BLOODSPLATTER1,
    S_BLOODSPLATTER2,
    S_BLOODSPLATTER3,
    S_BLOODSPLATTERX,
    S_PLAY,
    S_PLAY_RUN1,
    S_PLAY_RUN2,
    S_PLAY_RUN3,
    S_PLAY_RUN4,
    S_PLAY_ATK1,
    S_PLAY_ATK2,
    S_PLAY_PAIN,
    S_PLAY_PAIN2,
    S_PLAY_DIE1,
    S_PLAY_DIE2,
    S_PLAY_DIE3,
    S_PLAY_DIE4,
    S_PLAY_DIE5,
    S_PLAY_DIE6,
    S_PLAY_DIE7,
    S_PLAY_DIE8,
    S_PLAY_DIE9,
    S_PLAY_XDIE1,
    S_PLAY_XDIE2,
    S_PLAY_XDIE3,
    S_PLAY_XDIE4,
    S_PLAY_XDIE5,
    S_PLAY_XDIE6,
    S_PLAY_XDIE7,
    S_PLAY_XDIE8,
    S_PLAY_XDIE9,
    S_PLAY_FDTH1,
    S_PLAY_FDTH2,
    S_PLAY_FDTH3,
    S_PLAY_FDTH4,
    S_PLAY_FDTH5,
    S_PLAY_FDTH6,
    S_PLAY_FDTH7,
    S_PLAY_FDTH8,
    S_PLAY_FDTH9,
    S_PLAY_FDTH10,
    S_PLAY_FDTH11,
    S_PLAY_FDTH12,
    S_PLAY_FDTH13,
    S_PLAY_FDTH14,
    S_PLAY_FDTH15,
    S_PLAY_FDTH16,
    S_PLAY_FDTH17,
    S_PLAY_FDTH18,
    S_PLAY_FDTH19,
    S_PLAY_FDTH20,
    S_BLOODYSKULL1,
    S_BLOODYSKULL2,
    S_BLOODYSKULL3,
    S_BLOODYSKULL4,
    S_BLOODYSKULL5,
    S_BLOODYSKULLX1,
    S_BLOODYSKULLX2,
    S_CHICPLAY,
    S_CHICPLAY_RUN1,
    S_CHICPLAY_RUN2,
    S_CHICPLAY_RUN3,
    S_CHICPLAY_RUN4,
    S_CHICPLAY_ATK1,
    S_CHICPLAY_PAIN,
    S_CHICPLAY_PAIN2,
    S_CHICKEN_LOOK1,
    S_CHICKEN_LOOK2,
    S_CHICKEN_WALK1,
    S_CHICKEN_WALK2,
    S_CHICKEN_PAIN1,
    S_CHICKEN_PAIN2,
    S_CHICKEN_ATK1,
    S_CHICKEN_ATK2,
    S_CHICKEN_DIE1,
    S_CHICKEN_DIE2,
    S_CHICKEN_DIE3,
    S_CHICKEN_DIE4,
    S_CHICKEN_DIE5,
    S_CHICKEN_DIE6,
    S_CHICKEN_DIE7,
    S_CHICKEN_DIE8,
    S_FEATHER1,
    S_FEATHER2,
    S_FEATHER3,
    S_FEATHER4,
    S_FEATHER5,
    S_FEATHER6,
    S_FEATHER7,
    S_FEATHER8,
    S_FEATHERX,
    S_MUMMY_LOOK1,
    S_MUMMY_LOOK2,
    S_MUMMY_WALK1,
    S_MUMMY_WALK2,
    S_MUMMY_WALK3,
    S_MUMMY_WALK4,
    S_MUMMY_ATK1,
    S_MUMMY_ATK2,
    S_MUMMY_ATK3,
    S_MUMMYL_ATK1,
    S_MUMMYL_ATK2,
    S_MUMMYL_ATK3,
    S_MUMMYL_ATK4,
    S_MUMMYL_ATK5,
    S_MUMMYL_ATK6,
    S_MUMMY_PAIN1,
    S_MUMMY_PAIN2,
    S_MUMMY_DIE1,
    S_MUMMY_DIE2,
    S_MUMMY_DIE3,
    S_MUMMY_DIE4,
    S_MUMMY_DIE5,
    S_MUMMY_DIE6,
    S_MUMMY_DIE7,
    S_MUMMY_DIE8,
    S_MUMMY_SOUL1,
    S_MUMMY_SOUL2,
    S_MUMMY_SOUL3,
    S_MUMMY_SOUL4,
    S_MUMMY_SOUL5,
    S_MUMMY_SOUL6,
    S_MUMMY_SOUL7,
    S_MUMMYFX1_1,
    S_MUMMYFX1_2,
    S_MUMMYFX1_3,
    S_MUMMYFX1_4,
    S_MUMMYFXI1_1,
    S_MUMMYFXI1_2,
    S_MUMMYFXI1_3,
    S_MUMMYFXI1_4,
    S_BEAST_LOOK1,
    S_BEAST_LOOK2,
    S_BEAST_WALK1,
    S_BEAST_WALK2,
    S_BEAST_WALK3,
    S_BEAST_WALK4,
    S_BEAST_WALK5,
    S_BEAST_WALK6,
    S_BEAST_ATK1,
    S_BEAST_ATK2,
    S_BEAST_PAIN1,
    S_BEAST_PAIN2,
    S_BEAST_DIE1,
    S_BEAST_DIE2,
    S_BEAST_DIE3,
    S_BEAST_DIE4,
    S_BEAST_DIE5,
    S_BEAST_DIE6,
    S_BEAST_DIE7,
    S_BEAST_DIE8,
    S_BEAST_DIE9,
    S_BEAST_XDIE1,
    S_BEAST_XDIE2,
    S_BEAST_XDIE3,
    S_BEAST_XDIE4,
    S_BEAST_XDIE5,
    S_BEAST_XDIE6,
    S_BEAST_XDIE7,
    S_BEAST_XDIE8,
    S_BEASTBALL1,
    S_BEASTBALL2,
    S_BEASTBALL3,
    S_BEASTBALL4,
    S_BEASTBALL5,
    S_BEASTBALL6,
    S_BEASTBALLX1,
    S_BEASTBALLX2,
    S_BEASTBALLX3,
    S_BEASTBALLX4,
    S_BEASTBALLX5,
    S_BURNBALL1,
    S_BURNBALL2,
    S_BURNBALL3,
    S_BURNBALL4,
    S_BURNBALL5,
    S_BURNBALL6,
    S_BURNBALL7,
    S_BURNBALL8,
    S_BURNBALLFB1,
    S_BURNBALLFB2,
    S_BURNBALLFB3,
    S_BURNBALLFB4,
    S_BURNBALLFB5,
    S_BURNBALLFB6,
    S_BURNBALLFB7,
    S_BURNBALLFB8,
    S_PUFFY1,
    S_PUFFY2,
    S_PUFFY3,
    S_PUFFY4,
    S_PUFFY5,
    S_SNAKE_LOOK1,
    S_SNAKE_LOOK2,
    S_SNAKE_WALK1,
    S_SNAKE_WALK2,
    S_SNAKE_WALK3,
    S_SNAKE_WALK4,
    S_SNAKE_ATK1,
    S_SNAKE_ATK2,
    S_SNAKE_ATK3,
    S_SNAKE_ATK4,
    S_SNAKE_ATK5,
    S_SNAKE_ATK6,
    S_SNAKE_ATK7,
    S_SNAKE_ATK8,
    S_SNAKE_ATK9,
    S_SNAKE_PAIN1,
    S_SNAKE_PAIN2,
    S_SNAKE_DIE1,
    S_SNAKE_DIE2,
    S_SNAKE_DIE3,
    S_SNAKE_DIE4,
    S_SNAKE_DIE5,
    S_SNAKE_DIE6,
    S_SNAKE_DIE7,
    S_SNAKE_DIE8,
    S_SNAKE_DIE9,
    S_SNAKE_DIE10,
    S_SNAKEPRO_A1,
    S_SNAKEPRO_A2,
    S_SNAKEPRO_A3,
    S_SNAKEPRO_A4,
    S_SNAKEPRO_AX1,
    S_SNAKEPRO_AX2,
    S_SNAKEPRO_AX3,
    S_SNAKEPRO_AX4,
    S_SNAKEPRO_AX5,
    S_SNAKEPRO_B1,
    S_SNAKEPRO_B2,
    S_SNAKEPRO_BX1,
    S_SNAKEPRO_BX2,
    S_SNAKEPRO_BX3,
    S_SNAKEPRO_BX4,
    S_HEAD_LOOK,
    S_HEAD_FLOAT,
    S_HEAD_ATK1,
    S_HEAD_ATK2,
    S_HEAD_PAIN1,
    S_HEAD_PAIN2,
    S_HEAD_DIE1,
    S_HEAD_DIE2,
    S_HEAD_DIE3,
    S_HEAD_DIE4,
    S_HEAD_DIE5,
    S_HEAD_DIE6,
    S_HEAD_DIE7,
    S_HEADFX1_1,
    S_HEADFX1_2,
    S_HEADFX1_3,
    S_HEADFXI1_1,
    S_HEADFXI1_2,
    S_HEADFXI1_3,
    S_HEADFXI1_4,
    S_HEADFX2_1,
    S_HEADFX2_2,
    S_HEADFX2_3,
    S_HEADFXI2_1,
    S_HEADFXI2_2,
    S_HEADFXI2_3,
    S_HEADFXI2_4,
    S_HEADFX3_1,
    S_HEADFX3_2,
    S_HEADFX3_3,
    S_HEADFX3_4,
    S_HEADFX3_5,
    S_HEADFX3_6,
    S_HEADFXI3_1,
    S_HEADFXI3_2,
    S_HEADFXI3_3,
    S_HEADFXI3_4,
    S_HEADFX4_1,
    S_HEADFX4_2,
    S_HEADFX4_3,
    S_HEADFX4_4,
    S_HEADFX4_5,
    S_HEADFX4_6,
    S_HEADFX4_7,
    S_HEADFXI4_1,
    S_HEADFXI4_2,
    S_HEADFXI4_3,
    S_HEADFXI4_4,
    S_CLINK_LOOK1,
    S_CLINK_LOOK2,
    S_CLINK_WALK1,
    S_CLINK_WALK2,
    S_CLINK_WALK3,
    S_CLINK_WALK4,
    S_CLINK_ATK1,
    S_CLINK_ATK2,
    S_CLINK_ATK3,
    S_CLINK_PAIN1,
    S_CLINK_PAIN2,
    S_CLINK_DIE1,
    S_CLINK_DIE2,
    S_CLINK_DIE3,
    S_CLINK_DIE4,
    S_CLINK_DIE5,
    S_CLINK_DIE6,
    S_CLINK_DIE7,
    S_WIZARD_LOOK1,
    S_WIZARD_LOOK2,
    S_WIZARD_WALK1,
    S_WIZARD_WALK2,
    S_WIZARD_WALK3,
    S_WIZARD_WALK4,
    S_WIZARD_WALK5,
    S_WIZARD_WALK6,
    S_WIZARD_WALK7,
    S_WIZARD_WALK8,
    S_WIZARD_ATK1,
    S_WIZARD_ATK2,
    S_WIZARD_ATK3,
    S_WIZARD_ATK4,
    S_WIZARD_ATK5,
    S_WIZARD_ATK6,
    S_WIZARD_ATK7,
    S_WIZARD_ATK8,
    S_WIZARD_ATK9,
    S_WIZARD_PAIN1,
    S_WIZARD_PAIN2,
    S_WIZARD_DIE1,
    S_WIZARD_DIE2,
    S_WIZARD_DIE3,
    S_WIZARD_DIE4,
    S_WIZARD_DIE5,
    S_WIZARD_DIE6,
    S_WIZARD_DIE7,
    S_WIZARD_DIE8,
    S_WIZFX1_1,
    S_WIZFX1_2,
    S_WIZFXI1_1,
    S_WIZFXI1_2,
    S_WIZFXI1_3,
    S_WIZFXI1_4,
    S_WIZFXI1_5,
    S_IMP_LOOK1,
    S_IMP_LOOK2,
    S_IMP_LOOK3,
    S_IMP_LOOK4,
    S_IMP_FLY1,
    S_IMP_FLY2,
    S_IMP_FLY3,
    S_IMP_FLY4,
    S_IMP_FLY5,
    S_IMP_FLY6,
    S_IMP_FLY7,
    S_IMP_FLY8,
    S_IMP_MEATK1,
    S_IMP_MEATK2,
    S_IMP_MEATK3,
    S_IMP_MSATK1_1,
    S_IMP_MSATK1_2,
    S_IMP_MSATK1_3,
    S_IMP_MSATK1_4,
    S_IMP_MSATK1_5,
    S_IMP_MSATK1_6,
    S_IMP_MSATK2_1,
    S_IMP_MSATK2_2,
    S_IMP_MSATK2_3,
    S_IMP_PAIN1,
    S_IMP_PAIN2,
    S_IMP_DIE1,
    S_IMP_DIE2,
    S_IMP_XDIE1,
    S_IMP_XDIE2,
    S_IMP_XDIE3,
    S_IMP_XDIE4,
    S_IMP_XDIE5,
    S_IMP_CRASH1,
    S_IMP_CRASH2,
    S_IMP_CRASH3,
    S_IMP_CRASH4,
    S_IMP_XCRASH1,
    S_IMP_XCRASH2,
    S_IMP_XCRASH3,
    S_IMP_CHUNKA1,
    S_IMP_CHUNKA2,
    S_IMP_CHUNKA3,
    S_IMP_CHUNKB1,
    S_IMP_CHUNKB2,
    S_IMP_CHUNKB3,
    S_IMPFX1,
    S_IMPFX2,
    S_IMPFX3,
    S_IMPFXI1,
    S_IMPFXI2,
    S_IMPFXI3,
    S_IMPFXI4,
    S_KNIGHT_STND1,
    S_KNIGHT_STND2,
    S_KNIGHT_WALK1,
    S_KNIGHT_WALK2,
    S_KNIGHT_WALK3,
    S_KNIGHT_WALK4,
    S_KNIGHT_ATK1,
    S_KNIGHT_ATK2,
    S_KNIGHT_ATK3,
    S_KNIGHT_ATK4,
    S_KNIGHT_ATK5,
    S_KNIGHT_ATK6,
    S_KNIGHT_PAIN1,
    S_KNIGHT_PAIN2,
    S_KNIGHT_DIE1,
    S_KNIGHT_DIE2,
    S_KNIGHT_DIE3,
    S_KNIGHT_DIE4,
    S_KNIGHT_DIE5,
    S_KNIGHT_DIE6,
    S_KNIGHT_DIE7,
    S_SPINAXE1,
    S_SPINAXE2,
    S_SPINAXE3,
    S_SPINAXEX1,
    S_SPINAXEX2,
    S_SPINAXEX3,
    S_REDAXE1,
    S_REDAXE2,
    S_REDAXEX1,
    S_REDAXEX2,
    S_REDAXEX3,
    S_SRCR1_LOOK1,
    S_SRCR1_LOOK2,
    S_SRCR1_WALK1,
    S_SRCR1_WALK2,
    S_SRCR1_WALK3,
    S_SRCR1_WALK4,
    S_SRCR1_PAIN1,
    S_SRCR1_ATK1,
    S_SRCR1_ATK2,
    S_SRCR1_ATK3,
    S_SRCR1_ATK4,
    S_SRCR1_ATK5,
    S_SRCR1_ATK6,
    S_SRCR1_ATK7,
    S_SRCR1_DIE1,
    S_SRCR1_DIE2,
    S_SRCR1_DIE3,
    S_SRCR1_DIE4,
    S_SRCR1_DIE5,
    S_SRCR1_DIE6,
    S_SRCR1_DIE7,
    S_SRCR1_DIE8,
    S_SRCR1_DIE9,
    S_SRCR1_DIE10,
    S_SRCR1_DIE11,
    S_SRCR1_DIE12,
    S_SRCR1_DIE13,
    S_SRCR1_DIE14,
    S_SRCR1_DIE15,
    S_SRCR1_DIE16,
    S_SRCR1_DIE17,
    S_SRCRFX1_1,
    S_SRCRFX1_2,
    S_SRCRFX1_3,
    S_SRCRFXI1_1,
    S_SRCRFXI1_2,
    S_SRCRFXI1_3,
    S_SRCRFXI1_4,
    S_SRCRFXI1_5,
    S_SOR2_RISE1,
    S_SOR2_RISE2,
    S_SOR2_RISE3,
    S_SOR2_RISE4,
    S_SOR2_RISE5,
    S_SOR2_RISE6,
    S_SOR2_RISE7,
    S_SOR2_LOOK1,
    S_SOR2_LOOK2,
    S_SOR2_WALK1,
    S_SOR2_WALK2,
    S_SOR2_WALK3,
    S_SOR2_WALK4,
    S_SOR2_PAIN1,
    S_SOR2_PAIN2,
    S_SOR2_ATK1,
    S_SOR2_ATK2,
    S_SOR2_ATK3,
    S_SOR2_TELE1,
    S_SOR2_TELE2,
    S_SOR2_TELE3,
    S_SOR2_TELE4,
    S_SOR2_TELE5,
    S_SOR2_TELE6,
    S_SOR2_DIE1,
    S_SOR2_DIE2,
    S_SOR2_DIE3,
    S_SOR2_DIE4,
    S_SOR2_DIE5,
    S_SOR2_DIE6,
    S_SOR2_DIE7,
    S_SOR2_DIE8,
    S_SOR2_DIE9,
    S_SOR2_DIE10,
    S_SOR2_DIE11,
    S_SOR2_DIE12,
    S_SOR2_DIE13,
    S_SOR2_DIE14,
    S_SOR2_DIE15,
    S_SOR2FX1_1,
    S_SOR2FX1_2,
    S_SOR2FX1_3,
    S_SOR2FXI1_1,
    S_SOR2FXI1_2,
    S_SOR2FXI1_3,
    S_SOR2FXI1_4,
    S_SOR2FXI1_5,
    S_SOR2FXI1_6,
    S_SOR2FXSPARK1,
    S_SOR2FXSPARK2,
    S_SOR2FXSPARK3,
    S_SOR2FX2_1,
    S_SOR2FX2_2,
    S_SOR2FX2_3,
    S_SOR2FXI2_1,
    S_SOR2FXI2_2,
    S_SOR2FXI2_3,
    S_SOR2FXI2_4,
    S_SOR2FXI2_5,
    S_SOR2TELEFADE1,
    S_SOR2TELEFADE2,
    S_SOR2TELEFADE3,
    S_SOR2TELEFADE4,
    S_SOR2TELEFADE5,
    S_SOR2TELEFADE6,
    S_MNTR_LOOK1,
    S_MNTR_LOOK2,
    S_MNTR_WALK1,
    S_MNTR_WALK2,
    S_MNTR_WALK3,
    S_MNTR_WALK4,
    S_MNTR_ATK1_1,
    S_MNTR_ATK1_2,
    S_MNTR_ATK1_3,
    S_MNTR_ATK2_1,
    S_MNTR_ATK2_2,
    S_MNTR_ATK2_3,
    S_MNTR_ATK3_1,
    S_MNTR_ATK3_2,
    S_MNTR_ATK3_3,
    S_MNTR_ATK3_4,
    S_MNTR_ATK4_1,
    S_MNTR_PAIN1,
    S_MNTR_PAIN2,
    S_MNTR_DIE1,
    S_MNTR_DIE2,
    S_MNTR_DIE3,
    S_MNTR_DIE4,
    S_MNTR_DIE5,
    S_MNTR_DIE6,
    S_MNTR_DIE7,
    S_MNTR_DIE8,
    S_MNTR_DIE9,
    S_MNTR_DIE10,
    S_MNTR_DIE11,
    S_MNTR_DIE12,
    S_MNTR_DIE13,
    S_MNTR_DIE14,
    S_MNTR_DIE15,
    S_MNTRFX1_1,
    S_MNTRFX1_2,
    S_MNTRFXI1_1,
    S_MNTRFXI1_2,
    S_MNTRFXI1_3,
    S_MNTRFXI1_4,
    S_MNTRFXI1_5,
    S_MNTRFXI1_6,
    S_MNTRFX2_1,
    S_MNTRFXI2_1,
    S_MNTRFXI2_2,
    S_MNTRFXI2_3,
    S_MNTRFXI2_4,
    S_MNTRFXI2_5,
    S_MNTRFX3_1,
    S_MNTRFX3_2,
    S_MNTRFX3_3,
    S_MNTRFX3_4,
    S_MNTRFX3_5,
    S_MNTRFX3_6,
    S_MNTRFX3_7,
    S_MNTRFX3_8,
    S_MNTRFX3_9,
    S_AKYY1,
    S_AKYY2,
    S_AKYY3,
    S_AKYY4,
    S_AKYY5,
    S_AKYY6,
    S_AKYY7,
    S_AKYY8,
    S_AKYY9,
    S_AKYY10,
    S_BKYY1,
    S_BKYY2,
    S_BKYY3,
    S_BKYY4,
    S_BKYY5,
    S_BKYY6,
    S_BKYY7,
    S_BKYY8,
    S_BKYY9,
    S_BKYY10,
    S_CKYY1,
    S_CKYY2,
    S_CKYY3,
    S_CKYY4,
    S_CKYY5,
    S_CKYY6,
    S_CKYY7,
    S_CKYY8,
    S_CKYY9,
    S_AMG1,
    S_AMG2_1,
    S_AMG2_2,
    S_AMG2_3,
    S_AMM1,
    S_AMM2,
    S_AMC1,
    S_AMC2_1,
    S_AMC2_2,
    S_AMC2_3,
    S_AMS1_1,
    S_AMS1_2,
    S_AMS2_1,
    S_AMS2_2,
    S_AMP1_1,
    S_AMP1_2,
    S_AMP1_3,
    S_AMP2_1,
    S_AMP2_2,
    S_AMP2_3,
    S_AMB1_1,
    S_AMB1_2,
    S_AMB1_3,
    S_AMB2_1,
    S_AMB2_2,
    S_AMB2_3,
    S_SND_WIND,
    S_SND_WATERFALL,
    S_NONE,
    DO_NUMSTATES
  );

const
  STATEF_SKILL5FAST = 1;

const
  MAX_STATE_ARGS = 8;

const
  ARG1_DEFINED = 1;
  ARG2_DEFINED = 2;
  ARG3_DEFINED = 4;
  ARG4_DEFINED = 8;
  ARG5_DEFINED = $10;
  ARG6_DEFINED = $20;
  ARG7_DEFINED = $40;
  ARG8_DEFINED = $80;

type
  state_t = record
{$IFDEF OPTIMIZE_FOR_SIZE}
    sprite: smallint;
    frame: integer;
    tics: smallint;
    tics2: smallint;
{$ELSE}
    sprite: integer;
    frame: integer;
    tics: integer;
    tics2: integer;
{$ENDIF}
    action: actionf_t;
    nextstate: statenum_t;
    misc1: integer;
    misc2: integer;
    params: TCustomParamList;
    owners: TDNumberList;
    dlights: T2DNumberList;
{$IFDEF OPENGL}
    models: TDNumberList;
{$ENDIF}
    voxels: TDNumberList;
{$IFNDEF OPENGL}
    voxelradius: integer;
{$ENDIF}
    flags_ex: integer;
    mbf21bits: integer;
    args: array[0..MAX_STATE_ARGS - 1] of integer;
    argsdefined: integer;
  end;
  Pstate_t = ^state_t;

type
  mobjtype_t = (
    MT_MISC0,
    MT_ITEMSHIELD1,
    MT_ITEMSHIELD2,
    MT_MISC1,
    MT_MISC2,
    MT_ARTIINVISIBILITY,
    MT_MISC3,
    MT_ARTIFLY,
    MT_ARTIINVULNERABILITY,
    MT_ARTITOMEOFPOWER,
    MT_ARTIEGG,
    MT_EGGFX,
    MT_ARTISUPERHEAL,
    MT_MISC4,
    MT_MISC5,
    MT_FIREBOMB,
    MT_ARTITELEPORT,
    MT_POD,
    MT_PODGOO,
    MT_PODGENERATOR,
    MT_SPLASH,
    MT_SPLASHBASE,
    MT_LAVASPLASH,
    MT_LAVASMOKE,
    MT_SLUDGECHUNK,
    MT_SLUDGESPLASH,
    MT_SKULLHANG70,
    MT_SKULLHANG60,
    MT_SKULLHANG45,
    MT_SKULLHANG35,
    MT_CHANDELIER,
    MT_SERPTORCH,
    MT_SMALLPILLAR,
    MT_STALAGMITESMALL,
    MT_STALAGMITELARGE,
    MT_STALACTITESMALL,
    MT_STALACTITELARGE,
    MT_MISC6,
    MT_BARREL,
    MT_MISC7,
    MT_MISC8,
    MT_MISC9,
    MT_MISC10,
    MT_MISC11,
    MT_KEYGIZMOBLUE,
    MT_KEYGIZMOGREEN,
    MT_KEYGIZMOYELLOW,
    MT_KEYGIZMOFLOAT,
    MT_MISC12,
    MT_VOLCANOBLAST,
    MT_VOLCANOTBLAST,
    MT_TELEGLITGEN,
    MT_TELEGLITGEN2,
    MT_TELEGLITTER,
    MT_TELEGLITTER2,
    MT_TFOG,
    MT_TELEPORTMAN,
    MT_STAFFPUFF,
    MT_STAFFPUFF2,
    MT_BEAKPUFF,
    MT_MISC13,
    MT_GAUNTLETPUFF1,
    MT_GAUNTLETPUFF2,
    MT_MISC14,
    MT_BLASTERFX1,
    MT_BLASTERSMOKE,
    MT_RIPPER,
    MT_BLASTERPUFF1,
    MT_BLASTERPUFF2,
    MT_WMACE,
    MT_MACEFX1,
    MT_MACEFX2,
    MT_MACEFX3,
    MT_MACEFX4,
    MT_WSKULLROD,
    MT_HORNRODFX1,
    MT_HORNRODFX2,
    MT_RAINPLR1,
    MT_RAINPLR2,
    MT_RAINPLR3,
    MT_RAINPLR4,
    MT_GOLDWANDFX1,
    MT_GOLDWANDFX2,
    MT_GOLDWANDPUFF1,
    MT_GOLDWANDPUFF2,
    MT_WPHOENIXROD,
    MT_PHOENIXFX1,
    MT_PHOENIXPUFF,
    MT_PHOENIXFX2,
    MT_MISC15,
    MT_CRBOWFX1,
    MT_CRBOWFX2,
    MT_CRBOWFX3,
    MT_CRBOWFX4,
    MT_BLOOD,
    MT_BLOODSPLATTER,
    MT_PLAYER,
    MT_BLOODYSKULL,
    MT_CHICPLAYER,
    MT_CHICKEN,
    MT_FEATHER,
    MT_MUMMY,
    MT_MUMMYLEADER,
    MT_MUMMYGHOST,
    MT_MUMMYLEADERGHOST,
    MT_MUMMYSOUL,
    MT_MUMMYFX1,
    MT_BEAST,
    MT_BEASTBALL,
    MT_BURNBALL,
    MT_BURNBALLFB,
    MT_PUFFY,
    MT_SNAKE,
    MT_SNAKEPRO_A,
    MT_SNAKEPRO_B,
    MT_HEAD,
    MT_HEADFX1,
    MT_HEADFX2,
    MT_HEADFX3,
    MT_WHIRLWIND,
    MT_CLINK,
    MT_WIZARD,
    MT_WIZFX1,
    MT_IMP,
    MT_IMPLEADER,
    MT_IMPCHUNK1,
    MT_IMPCHUNK2,
    MT_IMPBALL,
    MT_KNIGHT,
    MT_KNIGHTGHOST,
    MT_KNIGHTAXE,
    MT_REDAXE,
    MT_SORCERER1,
    MT_SRCRFX1,
    MT_SORCERER2,
    MT_SOR2FX1,
    MT_SOR2FXSPARK,
    MT_SOR2FX2,
    MT_SOR2TELEFADE,
    MT_MINOTAUR,
    MT_MNTRFX1,
    MT_MNTRFX2,
    MT_MNTRFX3,
    MT_AKYY,
    MT_BKYY,
    MT_CKEY,
    MT_AMGWNDWIMPY,
    MT_AMGWNDHEFTY,
    MT_AMMACEWIMPY,
    MT_AMMACEHEFTY,
    MT_AMCBOWWIMPY,
    MT_AMCBOWHEFTY,
    MT_AMSKRDWIMPY,
    MT_AMSKRDHEFTY,
    MT_AMPHRDWIMPY,
    MT_AMPHRDHEFTY,
    MT_AMBLSRWIMPY,
    MT_AMBLSRHEFTY,
    MT_SOUNDWIND,
    MT_SOUNDWATERFALL,
    MT_NONE,
    DO_NUMMOBJTYPES
  );

const
  MOBJINFONAMESIZE = 30;

type
  mobjinfo_t = record
    name: array[0..MOBJINFONAMESIZE - 1] of char;
    inheritsfrom: integer;
    doomednum: integer;
    spawnstate: integer;
    spawnhealth: integer;
    seestate: integer;
    seesound: integer;
    reactiontime: integer;
    attacksound: integer;
    painstate: integer;
    painchance: integer;
    painsound: integer;
    meleestate: integer;
    missilestate: integer;
    deathstate: integer;
    xdeathstate: integer;
    deathsound: integer;
    speed: integer;
    radius: integer;
    height: integer;
    mass: integer;
    damage: integer;
    activesound: integer;
    flags: integer;
    flags2: integer;
    flags_ex: integer;
    flags2_ex: integer;
    raisestate: integer;
    customsound1: integer;
    customsound2: integer;
    customsound3: integer;
    meleesound: integer;
    dropitem: integer;
    missiletype: integer;
    explosiondamage: integer;
    explosionradius: integer;
    meleedamage: integer;
    renderstyle: mobjrenderstyle_t;
    alpha: integer;
    healstate: integer;
    crashstate: integer;
    crushstate: integer;
    interactstate: integer;
    vspeed: integer;  // Initial vertical speed
    pushfactor: integer; // How much can be pushed? 1..FRACUNIT
    friction: Integer; // Default is ORIG_FRICTION
    scale: integer;
    gravity: integer;
    flags3_ex: integer;
    flags4_ex: integer;
    minmissilechance: integer;
    floatspeed: integer;
    normalspeed: integer;
    fastspeed: integer;
    obituary: string[64];
    hitobituary: string[64];
    gender: gender_t;
    meleerange: integer;
    maxstepheight: integer;
    maxdropoffheight: integer;
    gibhealth: integer;
    maxtargetrange: integer;
    WeaveIndexXY: integer;
    WeaveIndexZ: integer;
    spriteDX: integer;
    spriteDY: integer;
    flags5_ex: integer;
    flags6_ex: integer;
    // MBF21
    infighting_group: integer;
    projectile_group: integer;
    splash_group: integer;
    mbf21bits: integer; // not actually flags, the bits will be converted to DelphiDoom flags
    ripsound: integer;
    bloodcolor: integer;
    translationname: string[8];
  end;

  Pmobjinfo_t = ^mobjinfo_t;

var
  MT_MAPSPOT: integer = -2;
  MT_MAPSPOTGRAVITY: integer = -2;

implementation

end.

