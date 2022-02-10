//------------------------------------------------------------------------------
//
//  DelphiStrife is a source port of the game Strife.
//
//  Based on:
//    - Linux Doom by "id Software"
//    - Chocolate Strife by "Simon Howard"
//    - DelphiDoom by "Jim Valavanis"
//
//  Copyright (C) 1993-1996 by id Software, Inc.
//  Copyright (C) 2005 Simon Howard
//  Copyright (C) 2010 James Haley, Samuel Villarreal
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
//    Dialog Engine for Strife
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit p_dialog;

interface

uses
  d_player,
  info_h;

//
// Globals
//

// This can be toggled at runtime to determine if the full dialog messages
// are subtitled on screen or not. Defaults to off.
var
  dialogshowtext: boolean = true;

// The global mission objective buffer. This gets written to and read from file,
// and is set by dialogs and line actions.
var
  mission_objective: string;

//==============================================================================
//
// M_InitDialogs
//
//==============================================================================
procedure M_InitDialogs;

//==============================================================================
//
// P_GiveItemToPlayer
//
//==============================================================================
function P_GiveItemToPlayer(player: Pplayer_t; sprnum: integer; _type: mobjtype_t): boolean;

//==============================================================================
//
// P_GiveVoiceObjective
//
//==============================================================================
procedure P_GiveVoiceObjective(const voice, log: string; const minlumpnum: integer);

//==============================================================================
//
// P_DialogStartP1
//
//==============================================================================
procedure P_DialogStartP1;

//==============================================================================
//
// P_DialogStart
//
//==============================================================================
procedure P_DialogStart(player: Pplayer_t);

//==============================================================================
//
// P_PlayerHasItem
//
//==============================================================================
function P_PlayerHasItem(player: Pplayer_t; _type: mobjtype_t): integer;

//==============================================================================
//
// P_GiveObjective
//
//==============================================================================
procedure P_GiveObjective(const x: string; const minlumpnum: integer);

//==============================================================================
//
// P_GiveInventoryItem
//
//==============================================================================
function P_GiveInventoryItem(player: Pplayer_t; sprnum: integer; _type: mobjtype_t): boolean;

const
  MAXINVENTORYSLOTS = 30;

  MDLG_CHOICELEN = 32;
  MDLG_MSGLEN = 80;
  MDLG_NAMELEN = 16;
  MDLG_LUMPLEN = 8;
  MDLG_TEXTLEN = 320;
  MDLG_MAXCHOICES = 5;
  MDLG_MAXITEMS = 3;

type
  mapdlgchoice_t = packed record
    giveitem: integer;                                    // item given when successful
    needitems: array[0..MDLG_MAXITEMS - 1] of integer;    // item needed for success
    needamounts: array[0..MDLG_MAXITEMS - 1] of integer;  // amount of items needed
    txt: array[0..MDLG_CHOICELEN - 1] of char;            // normal text
    txtok: array[0..MDLG_MSGLEN - 1] of char;             // message given on success
    next: integer;                                        // next dialog?
    objective: integer;                                   // ???
    txtno: array[0..MDLG_MSGLEN - 1] of char;             // message given on failure
  end;
  mapdlgchoice_p = ^mapdlgchoice_t;

type
  mapdialog_t = packed record
    speakerid: integer;                                   // script ID# for mobjtype that will use this dialog
    dropitem: integer;                                    // item to drop if that thingtype is killed
    checkitem: array[0..MDLG_MAXITEMS - 1] of integer;    // item(s) needed to see this dialog
    jumptoconv: integer;                                  // conversation to jump to when... ?
    name: array[0..MDLG_NAMELEN - 1] of char;             // name of speaker
    voice: array[0..MDLG_LUMPLEN - 1] of char;            // voice file to play
    backpic: array[0..MDLG_LUMPLEN - 1] of char;          // backdrop pic for character, if any
    txt: array[0..MDLG_TEXTLEN - 1] of char;              // main message text
    // options that this dialog gives the player
    choices: array[0..MDLG_MAXCHOICES - 1] of mapdlgchoice_t;
  end;
  mapdialog_p = ^mapdialog_t;
  mapdialog_a = array[0..255] of mapdialog_t;
  mapdialog_pa = ^mapdialog_a;

  oldmapdialog_t = packed record
    speakerid: integer;                                   // script ID# for mobjtype that will use this dialog
    dropitem: integer;                                    // item to drop if that thingtype is killed
    voice: LongWord;                                      // voice file to play
    name: array[0..MDLG_NAMELEN - 1] of char;             // name of speaker
    txt: array[0..MDLG_TEXTLEN - 1] of char;              // main message text
    // options that this dialog gives the player
    choices: array[0..MDLG_MAXCHOICES - 1] of mapdlgchoice_t;
  end;
  oldmapdialog_p = ^oldmapdialog_t;
  oldmapdialog_a = array[0..255] of oldmapdialog_t;
  oldmapdialog_pa = ^oldmapdialog_a;

//==============================================================================
//
// P_DialogFind
//
//==============================================================================
function P_DialogFind(_type: mobjtype_t; jumptoconv: integer): mapdialog_p;

//==============================================================================
//
// P_DialogLoad
//
//==============================================================================
procedure P_DialogLoad;

implementation

uses
  d_delphi,
  d_check,
  d_event,
  doomstat,
  w_wad,
  deh_main,
  doomdef,
  g_game,
  m_menu,
  m_rnd,
  m_fixed,
  mt_utils,
  z_zone,
  i_system,
  p_mobj_h,
  p_mobj,
  p_inter,
  p_enemy,
  p_doors,
  p_spec,
  p_map,
  p_user,
  p_tick,
  info,
  r_defs,
  r_main,
  tables,
  sounddata,
  s_sound,
  v_data,
  v_video;

type
  teaserxlat_t = record
    id: integer;
    teaser1: integer;
    teaser2: integer;
  end;
  teaserxlat_p = ^teaserxlat_t;

const
  TEASERXLATSIZE = 323;

const
  teaserxlat: array[0..TEASERXLATSIZE - 1] of teaserxlat_t = (
    (id: 2; teaser1: 0; teaser2: 0),
    (id: 3; teaser1: 0; teaser2: 0),
    (id: 4; teaser1: 0; teaser2: 0),
    (id: 5; teaser1: 0; teaser2: 0),
    (id: 6; teaser1: 0; teaser2: 0),
    (id: 7; teaser1: 0; teaser2: 0),
    (id: 8; teaser1: 0; teaser2: 0),
    (id: 9; teaser1: 0; teaser2: 0),
    (id: 10; teaser1: 0; teaser2: 0),
    (id: 11; teaser1: 0; teaser2: 0),
    (id: 12; teaser1: 0; teaser2: 0),
    (id: 13; teaser1: 0; teaser2: 0),
    (id: 14; teaser1: 0; teaser2: 0),
    (id: 15; teaser1: 0; teaser2: 0),
    (id: 16; teaser1: 0; teaser2: 0),
    (id: 17; teaser1: 0; teaser2: 0),
    (id: 18; teaser1: 0; teaser2: 0),
    (id: 19; teaser1: 0; teaser2: 0),
    (id: 20; teaser1: 0; teaser2: 0),
    (id: 21; teaser1: 0; teaser2: 0),
    (id: 22; teaser1: 0; teaser2: 0),
    (id: 23; teaser1: 0; teaser2: 0),
    (id: 24; teaser1: 0; teaser2: 0),
    (id: 25; teaser1: 0; teaser2: 0),
    (id: 26; teaser1: 0; teaser2: 0),
    (id: 27; teaser1: 0; teaser2: 0),
    (id: 28; teaser1: 0; teaser2: 0),
    (id: 29; teaser1: 0; teaser2: 0),
    (id: 30; teaser1: 0; teaser2: 0),
    (id: 31; teaser1: 0; teaser2: 0),
    (id: 32; teaser1: 0; teaser2: 0),
    (id: 33; teaser1: 0; teaser2: 0),
    (id: 34; teaser1: 0; teaser2: 0),
    (id: 35; teaser1: 0; teaser2: 0),
    (id: 36; teaser1: 0; teaser2: 0),
    (id: 37; teaser1: 0; teaser2: 0),
    (id: 38; teaser1: 37; teaser2: 38),
    (id: 39; teaser1: 38; teaser2: 39),
    (id: 40; teaser1: 39; teaser2: 40),
    (id: 41; teaser1: 40; teaser2: 41),
    (id: 42; teaser1: 41; teaser2: 42),
    (id: 43; teaser1: 42; teaser2: 43),
    (id: 44; teaser1: 43; teaser2: 44),
    (id: 45; teaser1: 44; teaser2: 45),
    (id: 46; teaser1: 45; teaser2: 56),
    (id: 47; teaser1: 46; teaser2: 47),
    (id: 48; teaser1: 47; teaser2: 48),
    (id: 49; teaser1: 48; teaser2: 49),
    (id: 50; teaser1: 49; teaser2: 50),
    (id: 51; teaser1: 0; teaser2: 0),
    (id: 52; teaser1: 0; teaser2: 0),
    (id: 53; teaser1: 52; teaser2: 53),
    (id: 54; teaser1: 53; teaser2: 54),
    (id: 55; teaser1: 54; teaser2: 55),
    (id: 56; teaser1: 55; teaser2: 56),
    (id: 57; teaser1: 56; teaser2: 57),
    (id: 58; teaser1: 57; teaser2: 58),
    (id: 59; teaser1: 0; teaser2: 0),
    (id: 60; teaser1: 0; teaser2: 0),
    (id: 61; teaser1: 58; teaser2: 59),
    (id: 62; teaser1: 61; teaser2: 62),
    (id: 63; teaser1: 0; teaser2: 0),
    (id: 64; teaser1: 0; teaser2: 0),
    (id: 65; teaser1: 62; teaser2: 63),
    (id: 66; teaser1: 63; teaser2: 64),
    (id: 67; teaser1: 0; teaser2: 0),
    (id: 68; teaser1: 0; teaser2: 0),
    (id: 69; teaser1: 0; teaser2: 0),
    (id: 70; teaser1: 0; teaser2: 0),
    (id: 71; teaser1: 0; teaser2: 0),
    (id: 72; teaser1: 0; teaser2: 0),
    (id: 73; teaser1: 0; teaser2: 0),
    (id: 74; teaser1: 0; teaser2: 0),
    (id: 75; teaser1: 0; teaser2: 0),
    (id: 76; teaser1: 0; teaser2: 0),
    (id: 77; teaser1: 0; teaser2: 0),
    (id: 78; teaser1: 0; teaser2: 0),
    (id: 79; teaser1: 0; teaser2: 0),
    (id: 80; teaser1: 0; teaser2: 0),
    (id: 81; teaser1: 0; teaser2: 0),
    (id: 82; teaser1: 0; teaser2: 0),
    (id: 84; teaser1: 0; teaser2: 0),
    (id: 85; teaser1: 0; teaser2: 0),
    (id: 86; teaser1: 0; teaser2: 0),
    (id: 87; teaser1: 0; teaser2: 0),
    (id: 88; teaser1: 0; teaser2: 0),
    (id: 89; teaser1: 0; teaser2: 0),
    (id: 90; teaser1: 0; teaser2: 0),
    (id: 91; teaser1: 0; teaser2: 0),
    (id: 92; teaser1: 0; teaser2: 0),
    (id: 93; teaser1: 0; teaser2: 0),
    (id: 94; teaser1: 0; teaser2: 0),
    (id: 95; teaser1: 0; teaser2: 0),
    (id: 96; teaser1: 0; teaser2: 0),
    (id: 97; teaser1: 0; teaser2: 0),
    (id: 98; teaser1: 0; teaser2: 0),
    (id: 99; teaser1: 0; teaser2: 0),
    (id: 102; teaser1: 0; teaser2: 0),
    (id: 102; teaser1: 0; teaser2: 0),
    (id: 106; teaser1: 0; teaser2: 0),
    (id: 107; teaser1: 0; teaser2: 0),
    (id: 108; teaser1: 0; teaser2: 0),
    (id: 121; teaser1: 0; teaser2: 0),
    (id: 122; teaser1: 0; teaser2: 0),
    (id: 123; teaser1: 0; teaser2: 0),
    (id: 124; teaser1: 0; teaser2: 0),
    (id: 125; teaser1: 121; teaser2: 124),
    (id: 126; teaser1: 122; teaser2: 125),
    (id: 127; teaser1: 123; teaser2: 126),
    (id: 128; teaser1: 124; teaser2: 127),
    (id: 129; teaser1: 125; teaser2: 128),
    (id: 130; teaser1: 126; teaser2: 129),
    (id: 131; teaser1: 0; teaser2: 0),
    (id: 132; teaser1: 0; teaser2: 0),
    (id: 133; teaser1: 129; teaser2: 132),
    (id: 134; teaser1: 130; teaser2: 133),
    (id: 135; teaser1: 131; teaser2: 134),
    (id: 136; teaser1: 132; teaser2: 135),
    (id: 137; teaser1: 133; teaser2: 136),
    (id: 138; teaser1: 134; teaser2: 137),
    (id: 139; teaser1: 135; teaser2: 138),
    (id: 140; teaser1: 136; teaser2: 139),
    (id: 141; teaser1: 137; teaser2: 140),
    (id: 142; teaser1: 138; teaser2: 141),
    (id: 143; teaser1: 139; teaser2: 142),
    (id: 144; teaser1: 140; teaser2: 143),
    (id: 145; teaser1: 141; teaser2: 144),
    (id: 146; teaser1: 142; teaser2: 145),
    (id: 147; teaser1: 143; teaser2: 146),
    (id: 148; teaser1: 144; teaser2: 147),
    (id: 149; teaser1: 145; teaser2: 148),
    (id: 150; teaser1: 146; teaser2: 149),
    (id: 151; teaser1: 147; teaser2: 150),
    (id: 152; teaser1: 148; teaser2: 151),
    (id: 153; teaser1: 149; teaser2: 152),
    (id: 154; teaser1: 150; teaser2: 153),
    (id: 155; teaser1: 151; teaser2: 154),
    (id: 156; teaser1: 152; teaser2: 155),
    (id: 157; teaser1: 153; teaser2: 156),
    (id: 158; teaser1: 154; teaser2: 157),
    (id: 159; teaser1: 155; teaser2: 158),
    (id: 160; teaser1: 156; teaser2: 159),
    (id: 161; teaser1: 157; teaser2: 160),
    (id: 162; teaser1: 158; teaser2: 161),
    (id: 163; teaser1: 159; teaser2: 162),
    (id: 164; teaser1: 160; teaser2: 163),
    (id: 165; teaser1: 0; teaser2: 0),
    (id: 166; teaser1: 0; teaser2: 0),
    (id: 167; teaser1: 169; teaser2: 173),
    (id: 168; teaser1: 161; teaser2: 165),
    (id: 169; teaser1: 162; teaser2: 166),
    (id: 170; teaser1: 163; teaser2: 167),
    (id: 171; teaser1: 164; teaser2: 168),
    (id: 172; teaser1: 0; teaser2: 0),
    (id: 173; teaser1: 165; teaser2: 169),
    (id: 174; teaser1: 166; teaser2: 170),
    (id: 175; teaser1: 167; teaser2: 171),
    (id: 176; teaser1: 168; teaser2: 172),
    (id: 177; teaser1: 170; teaser2: 174),
    (id: 178; teaser1: 171; teaser2: 175),
    (id: 179; teaser1: 173; teaser2: 177),
    (id: 180; teaser1: 174; teaser2: 178),
    (id: 181; teaser1: 175; teaser2: 179),
    (id: 182; teaser1: 176; teaser2: 180),
    (id: 183; teaser1: 177; teaser2: 181),
    (id: 184; teaser1: 178; teaser2: 182),
    (id: 185; teaser1: 179; teaser2: 183),
    (id: 186; teaser1: 180; teaser2: 184),
    (id: 187; teaser1: 181; teaser2: 184),
    (id: 188; teaser1: 182; teaser2: 186),
    (id: 189; teaser1: 183; teaser2: 187),
    (id: 190; teaser1: 184; teaser2: 188),
    (id: 191; teaser1: 185; teaser2: 189),
    (id: 192; teaser1: 186; teaser2: 190),
    (id: 193; teaser1: 187; teaser2: 191),
    (id: 194; teaser1: 188; teaser2: 192),
    (id: 195; teaser1: 189; teaser2: 193),
    (id: 196; teaser1: 190; teaser2: 194),
    (id: 197; teaser1: 191; teaser2: 195),
    (id: 198; teaser1: 192; teaser2: 196),
    (id: 199; teaser1: 193; teaser2: 197),
    (id: 200; teaser1: 194; teaser2: 198),
    (id: 201; teaser1: 0; teaser2: 0),
    (id: 202; teaser1: 196; teaser2: 200),
    (id: 203; teaser1: 0; teaser2: 0),
    (id: 204; teaser1: 0; teaser2: 0),
    (id: 205; teaser1: 0; teaser2: 0),
    (id: 206; teaser1: 0; teaser2: 0),
    (id: 207; teaser1: 0; teaser2: 0),
    (id: 208; teaser1: 0; teaser2: 0),
    (id: 209; teaser1: 0; teaser2: 0),
    (id: 210; teaser1: 0; teaser2: 0),
    (id: 211; teaser1: 0; teaser2: 0),
    (id: 212; teaser1: 0; teaser2: 0),
    (id: 213; teaser1: 0; teaser2: 0),
    (id: 214; teaser1: 0; teaser2: 0),
    (id: 215; teaser1: 0; teaser2: 0),
    (id: 216; teaser1: 0; teaser2: 0),
    (id: 217; teaser1: 0; teaser2: 0),
    (id: 218; teaser1: 0; teaser2: 0),
    (id: 219; teaser1: 0; teaser2: 0),
    (id: 220; teaser1: 0; teaser2: 0),
    (id: 221; teaser1: 0; teaser2: 0),
    (id: 222; teaser1: 0; teaser2: 0),
    (id: 223; teaser1: 0; teaser2: 0),
    (id: 224; teaser1: 0; teaser2: 0),
    (id: 225; teaser1: 0; teaser2: 0),
    (id: 226; teaser1: 0; teaser2: 0),
    (id: 227; teaser1: 0; teaser2: 0),
    (id: 228; teaser1: 0; teaser2: 0),
    (id: 229; teaser1: 0; teaser2: 0),
    (id: 230; teaser1: 0; teaser2: 0),
    (id: 231; teaser1: 0; teaser2: 0),
    (id: 232; teaser1: 0; teaser2: 0),
    (id: 233; teaser1: 0; teaser2: 0),
    (id: 234; teaser1: 0; teaser2: 0),
    (id: 235; teaser1: 0; teaser2: 0),
    (id: 236; teaser1: 0; teaser2: 0),
    (id: 237; teaser1: 0; teaser2: 0),
    (id: 238; teaser1: 0; teaser2: 0),
    (id: 239; teaser1: 0; teaser2: 0),
    (id: 240; teaser1: 0; teaser2: 0),
    (id: 241; teaser1: 0; teaser2: 0),
    (id: 242; teaser1: 0; teaser2: 0),
    (id: 243; teaser1: 0; teaser2: 0),
    (id: 244; teaser1: 0; teaser2: 0),
    (id: 245; teaser1: 0; teaser2: 0),
    (id: 246; teaser1: 0; teaser2: 0),
    (id: 247; teaser1: 0; teaser2: 0),
    (id: 248; teaser1: 0; teaser2: 0),
    (id: 249; teaser1: 0; teaser2: 0),
    (id: 250; teaser1: 0; teaser2: 0),
    (id: 251; teaser1: 0; teaser2: 0),
    (id: 252; teaser1: 0; teaser2: 0),
    (id: 253; teaser1: 0; teaser2: 0),
    (id: 254; teaser1: 0; teaser2: 0),
    (id: 255; teaser1: 0; teaser2: 0),
    (id: 256; teaser1: 0; teaser2: 0),
    (id: 257; teaser1: 0; teaser2: 0),
    (id: 258; teaser1: 0; teaser2: 0),
    (id: 259; teaser1: 0; teaser2: 0),
    (id: 260; teaser1: 0; teaser2: 0),
    (id: 261; teaser1: 0; teaser2: 0),
    (id: 262; teaser1: 0; teaser2: 0),
    (id: 263; teaser1: 0; teaser2: 0),
    (id: 264; teaser1: 0; teaser2: 0),
    (id: 265; teaser1: 0; teaser2: 0),
    (id: 266; teaser1: 0; teaser2: 0),
    (id: 267; teaser1: 0; teaser2: 0),
    (id: 268; teaser1: 0; teaser2: 0),
    (id: 269; teaser1: 0; teaser2: 0),
    (id: 270; teaser1: 0; teaser2: 0),
    (id: 271; teaser1: 0; teaser2: 0),
    (id: 272; teaser1: 0; teaser2: 0),
    (id: 273; teaser1: 0; teaser2: 0),
    (id: 274; teaser1: 0; teaser2: 0),
    (id: 275; teaser1: 0; teaser2: 0),
    (id: 276; teaser1: 0; teaser2: 0),
    (id: 277; teaser1: 0; teaser2: 0),
    (id: 278; teaser1: 0; teaser2: 0),
    (id: 279; teaser1: 0; teaser2: 0),
    (id: 280; teaser1: 0; teaser2: 0),
    (id: 281; teaser1: 0; teaser2: 0),
    (id: 282; teaser1: 0; teaser2: 0),
    (id: 283; teaser1: 0; teaser2: 0),
    (id: 284; teaser1: 0; teaser2: 0),
    (id: 285; teaser1: 0; teaser2: 0),
    (id: 286; teaser1: 0; teaser2: 0),
    (id: 287; teaser1: 0; teaser2: 0),
    (id: 288; teaser1: 0; teaser2: 0),
    (id: 289; teaser1: 0; teaser2: 0),
    (id: 290; teaser1: 0; teaser2: 0),
    (id: 291; teaser1: 0; teaser2: 0),
    (id: 292; teaser1: 0; teaser2: 0),
    (id: 293; teaser1: 0; teaser2: 0),
    (id: 294; teaser1: 0; teaser2: 0),
    (id: 297; teaser1: 0; teaser2: 0),
    (id: 298; teaser1: 280; teaser2: 297),
    (id: 299; teaser1: 281; teaser2: 298),
    (id: 300; teaser1: 282; teaser2: 299),
    (id: 301; teaser1: 283; teaser2: 300),
    (id: 302; teaser1: 284; teaser2: 301),
    (id: 303; teaser1: 285; teaser2: 302),
    (id: 304; teaser1: 286; teaser2: 303),
    (id: 305; teaser1: 0; teaser2: 0),
    (id: 306; teaser1: 287; teaser2: 307),
    (id: 307; teaser1: 288; teaser2: 308),
    (id: 308; teaser1: 289; teaser2: 306),
    (id: 309; teaser1: 0; teaser2: 0),
    (id: 310; teaser1: 0; teaser2: 0),
    (id: 311; teaser1: 292; teaser2: 309),
    (id: 312; teaser1: 293; teaser2: 310),
    (id: 313; teaser1: 294; teaser2: 311),
    (id: 314; teaser1: 295; teaser2: 312),
    (id: 315; teaser1: 296; teaser2: 313),
    (id: 316; teaser1: 297; teaser2: 314),
    (id: 317; teaser1: 298; teaser2: 315),
    (id: 318; teaser1: 0; teaser2: 0),
    (id: 319; teaser1: 0; teaser2: 0),
    (id: 320; teaser1: 0; teaser2: 0),
    (id: 321; teaser1: 0; teaser2: 0),
    (id: 322; teaser1: 0; teaser2: 0),
    (id: 323; teaser1: 0; teaser2: 0),
    (id: 324; teaser1: 0; teaser2: 0),
    (id: 325; teaser1: 0; teaser2: 0),
    (id: 326; teaser1: 0; teaser2: 0),
    (id: 327; teaser1: 0; teaser2: 0),
    (id: 328; teaser1: 0; teaser2: 0),
    (id: 329; teaser1: 0; teaser2: 0),
    (id: 330; teaser1: 0; teaser2: 0),
    (id: 331; teaser1: 0; teaser2: 0),
    (id: 332; teaser1: 0; teaser2: 0),
    (id: 333; teaser1: 0; teaser2: 0),
    (id: 334; teaser1: 0; teaser2: 0),
    (id: 335; teaser1: 0; teaser2: 0),
    (id: 336; teaser1: 0; teaser2: 0),
    (id: 337; teaser1: 0; teaser2: 0),
    (id: 338; teaser1: 0; teaser2: 0),
    (id: 339; teaser1: 0; teaser2: 0),
    (id: 340; teaser1: 0; teaser2: 0),
    (id: 341; teaser1: 0; teaser2: 0),
    (id: 342; teaser1: 0; teaser2: 0),
    (id: 343; teaser1: 0; teaser2: 0)
  );

//==============================================================================
//
// P_IdFromXlat
//
// JVAL: Support for old teasers
//
//==============================================================================
function P_IdFromXlat(const id: integer): integer;
var
  i: integer;
begin
  if teaser = 0 then
  begin
    result := id;
    exit;
  end;

  if id <= 0 then
  begin
    result := id;
    exit;
  end;

  result := id;
  if teaser = 1 then
    for i := 0 to TEASERXLATSIZE - 1 do
      if teaserxlat[i].teaser1 = id then
      begin
        result := teaserxlat[i].id;
        exit;
      end;

  if teaser = 2 then
    for i := 0 to TEASERXLATSIZE - 1 do
      if teaserxlat[i].teaser2 = id then
      begin
        result := teaserxlat[i].id;
        exit;
      end;

end;

//==============================================================================
//
// P_GiveObjective
//
// villsa - convenient macro for giving objective logs to player
//
//==============================================================================
procedure P_GiveObjective(const x: string; const minlumpnum: integer);
var
  obj_ln: integer;
begin
  obj_ln := W_CheckNumForName(DEH_GetString(x));
  if obj_ln > minlumpnum then
    mission_objective := W_TextLumpNum(obj_ln);
end;

//==============================================================================
// P_GiveVoiceObjective
//
// haleyjd - voice and objective in one
//
//==============================================================================
procedure P_GiveVoiceObjective(const voice, log: string; const minlumpnum: integer);
var
  obj_ln: integer;
begin
  obj_ln := W_CheckNumForName(DEH_GetString(log));
  S_StartVoice(DEH_GetString(voice));
  if obj_ln > minlumpnum then
    mission_objective := W_TextLumpNum(obj_ln);
end;

//
// Defines and Macros
//

const
  ORIG_MAPDIALOG_SIZE = 1516; // haleyjd: size of the original Strife mapdialog_t structure.
  OLD_MAPDIALOG_SIZE = 1488;  // JVAL: size of the old demo dialog structure
  DLG_DIVIDER = 563952;

//
// Static Globals
//

var
// True if SCRIPT00 is loaded.
  script0loaded: boolean;

// Number of dialogs defined in the current level's script.
  numleveldialogs: integer;

// The actual level dialogs. This didn't exist in Strife, but is new to account
// for structure alignment/packing concerns, given that Chocolate Doom is
// multiplatform.
  leveldialogs: mapdialog_pa;

// The actual script00 dialogs. As above.
  script0dialogs: mapdialog_pa;

// Number of dialogs defined in the SCRIPT00 lump.
  numscript0dialogs: integer;

// The player engaged in dialog. This is always player 1, though, since Rogue
// never completed the ability to use dialog outside of single-player mode.
  dialogplayer: Pplayer_t;

// The object to which the player is speaking.
  dialogtalker: Pmobj_t;

// The talker's current angle
  dialogtalkerangle: angle_t;

// The currently active mapdialog object.
  currentdialog: mapdialog_p;

// Text at the end of the choices
  dialoglastmsgbuffer: string;

// Item to display to player when picked up or recieved
  pickupstring: string;

// Health based on gameskill given by the front's medic
  healthamounts: array[Ord(sk_baby)..Ord(sk_nightmare)] of integer = ( -100 , -75, -50, -50, -100 );

//=============================================================================
//
// Dialog State Sets
//
// These are used to animate certain actors in response to what happens in
// their dialog sequences.
//

type
  dialogstateset_t = record
    _type: mobjtype_t;  // the type of object
    greet: statenum_t;  // greeting state, for start of dialog
    yes: statenum_t;    // "yes" state, for an affirmative response
    no: statenum_t;     // "no" state, when you don't have the right items
  end;
  dialogstateset_p = ^dialogstateset_t;

const
  NUMDIALOGSTATESETS = 5;

var
  dialogstatesets: array[0..NUMDIALOGSTATESETS - 1] of dialogstateset_t = (
    ( _type: MT_PLAYER;       greet: S_NULL;    yes: S_NULL;    no: S_NULL    ),
    ( _type: MT_SHOPKEEPER_W; greet: S_MRGT_00; yes: S_MRYS_00; no: S_MRNO_00 ),
    ( _type: MT_SHOPKEEPER_B; greet: S_MRGT_00; yes: S_MRYS_00; no: S_MRNO_00 ),
    ( _type: MT_SHOPKEEPER_A; greet: S_MRGT_00; yes: S_MRYS_00; no: S_MRNO_00 ),
    ( _type: MT_SHOPKEEPER_M; greet: S_MRGT_00; yes: S_MRYS_00; no: S_MRNO_00 )
  );

// Current dialog talker state
  dialogtalkerstates: dialogstateset_p;

//=============================================================================
//
// Random Messages
//
// Rogue hard-coded these so they wouldn't have to repeat them several times
// in the SCRIPT00 lump, apparently.
//

const
  MAXRNDMESSAGES = 10;
  NUMRNDMESSAGES = 5;

type
  rndmessage_t = record
    type_name: string;
    messages: array[0..MAXRNDMESSAGES - 1] of string;
  end;
  rndmessage_p = ^rndmessage_t;

var
  rndMessages: array[0..NUMRNDMESSAGES - 1] of rndmessage_t = (
    // Peasants
    (
        type_name: 'PEASANT';
        messages: (
            'PLEASE DON''T HURT ME.',

            'IF YOU''RE LOOKING TO HURT ME, I''M '#13#10 +
            'NOT REALLY WORTH THE EFFORT.',

            'I DON''T KNOW ANYTHING.',

            'GO AWAY OR I''LL CALL THE GUARDS!',

            'I WISH SOMETIMES THAT ALL THESE '#13#10 +
            'REBELS WOULD JUST LEARN THEIR '#13#10 +
            'PLACE AND STOP THIS NONSENSE.',

            'JUST LEAVE ME ALONE, OK?',

            'I''M NOT SURE, BUT SOMETIMES I THINK '#13#10 +
            'THAT I KNOW SOME OF THE ACOLYTES.',

            'THE ORDER''S GOT EVERYTHING AROUND HERE PRETTY WELL LOCKED UP TIGHT.',

            'THERE''S NO WAY THAT THIS IS JUST A '#13#10 +
            'SECURITY FORCE.',

            'I''VE HEARD THAT THE ORDER IS REALLY '#13#10 +
            'NERVOUS ABOUT THE FRONT''S '#13#10 +
            'ACTIONS AROUND HERE.'
        )
    ),
    // Rebel
    (
        type_name: 'REBEL';
        messages: (
            'THERE''S NO WAY THE ORDER WILL '#13#10 +
            'STAND AGAINST US.',

            'WE''RE ALMOST READY TO STRIKE. '#13#10 +
            'MACIL''S PLANS ARE FALLING IN PLACE.',

            'WE''RE ALL BEHIND YOU, DON''T WORRY.',

            'DON''T GET TOO CLOSE TO ANY OF THOSE BIG ROBOTS. THEY''LL MELT YOU DOWN '#13#10 +
            'FOR SCRAP!',

            'THE DAY OF OUR GLORY WILL SOON '#13#10 +
            'COME, AND THOSE WHO OPPOSE US WILL '#13#10 +
            'BE CRUSHED!',

            'DON''T GET TOO COMFORTABLE. WE''VE '#13#10 +
            'STILL GOT OUR WORK CUT OUT FOR US.',

            'MACIL SAYS THAT YOU''RE THE NEW '#13#10 +
            'HOPE. BEAR THAT IN MIND.',

            'ONCE WE''VE TAKEN THESE CHARLATANS DOWN, WE''LL BE ABLE TO REBUILD THIS ' +
            'WORLD AS IT SHOULD BE.',

            'REMEMBER THAT YOU AREN''T FIGHTING '#13#10 +
            'JUST FOR YOURSELF, BUT FOR '#13#10 +
            'EVERYONE HERE AND OUTSIDE.',

            'AS LONG AS ONE OF US STILL STANDS, '#13#10 +
            'WE WILL WIN.'
        )
    ),
    // Acolyte
    (
        type_name: 'AGUARD';
        messages: (
            'MOVE ALONG,  PEASANT.',

            'FOLLOW THE TRUE FAITH, ONLY THEN '#13#10 +
            'WILL YOU BEGIN TO UNDERSTAND.',

            'ONLY THROUGH DEATH CAN ONE BE '#13#10 +
            'TRULY REBORN.',

            'I''M NOT INTERESTED IN YOUR USELESS '#13#10 +
            'DRIVEL.',

            'IF I HAD WANTED TO TALK TO YOU I '#13#10 +
            'WOULD HAVE TOLD YOU SO.',

            'GO AND ANNOY SOMEONE ELSE!',

            'KEEP MOVING!',

            'IF THE ALARM GOES OFF, JUST STAY OUT OF OUR WAY!',

            'THE ORDER WILL CLEANSE THE WORLD '#13#10 +
            'AND USHER IT INTO THE NEW ERA.',

            'PROBLEM?  NO, I THOUGHT NOT.'
        )
    ),
    // Beggar
    (
        type_name: 'BEGGAR';
        messages: (
            'ALMS FOR THE POOR?',

            'WHAT ARE YOU LOOKING AT, SURFACER?',

            'YOU WOULDN''T HAVE ANY EXTRA FOOD, WOULD YOU?',

            'YOU  SURFACE PEOPLE WILL NEVER '#13#10 +
            '                                                                 ' +
            '                                      UNDERSTAND US.',

            'HA, THE GUARDS CAN''T FIND US.  THOSE '#13#10 +
            'IDIOTS DON''T EVEN KNOW WE EXIST.',

            'ONE DAY EVERYONE BUT THOSE WHO SERVE THE ORDER WILL BE FORCED TO ' +
            '  JOIN US.',

            'STARE NOW,  BUT YOU KNOW THAT THIS WILL BE YOUR OWN FACE ONE DAY.',

            // Note: 'NOTHING THING' is an authentic typo
            'THERE''S NOTHING THING MORE '#13#10 +
            'ANNOYING THAN A SURFACER WITH AN ATTITUDE!',

            'THE ORDER WILL MAKE SHORT WORK OF YOUR PATHETIC FRONT.',

            'WATCH YOURSELF SURFACER. WE KNOW OUR ENEMIES!'
        )
    ),
    // Templar
    (
        type_name: 'PGUARD';
        messages: (
            'WE ARE THE HANDS OF FATE. TO EARN '#13#10 +
            'OUR WRATH IS TO FIND OBLIVION!',

            'THE ORDER WILL CLEANSE THE WORLD '#13#10 +
            'OF THE WEAK AND CORRUPT!',

            'OBEY THE WILL OF THE MASTERS!',

            'LONG LIFE TO THE BROTHERS OF THE '#13#10 +
            'ORDER!',

            'FREE WILL IS AN ILLUSION THAT BINDS '#13#10 +
            'THE WEAK MINDED.',

            'POWER IS THE PATH TO GLORY. TO '#13#10 +
            'FOLLOW THE ORDER IS TO WALK THAT '#13#10 +
            'PATH!',

            'TAKE YOUR PLACE AMONG THE '#13#10 +
            'RIGHTEOUS, JOIN US!',

            'THE ORDER PROTECTS ITS OWN.',

            'ACOLYTES?  THEY HAVE YET TO SEE THE FULL GLORY OF THE ORDER.',

            'IF THERE IS ANY HONOR INSIDE THAT '#13#10 +
            'PATHETIC SHELL OF A BODY, '#13#10 +
            'YOU''LL ENTER INTO THE ARMS OF THE '#13#10 +
            'ORDER.'
        )
    )
  );

//=============================================================================
//
// Dialog Menu Structure
//
// The Strife dialog system is actually just a serious abuse of the DOOM menu
// engine. Hence why it doesn't work in multiplayer games or during demo
// recording.
//

const
  NUMDIALOGMENUITEMS = 6;

var
  dialogmenuitems: array[0..NUMDIALOGMENUITEMS - 1] of menuitem_t;
  dialogmenu: menu_t;

// Lump number of the dialog background picture, if any.
var
  dialogbgpiclumpnum: integer;

// Name of current speaking character.
  dialogname: string;

// Current dialog text.
  dialogtext: string;

//=============================================================================
//
// Routines
//
// P_ParseDialogLump
//
// haleyjd 09/02/10: This is an original function added to parse out the
// dialogs from the dialog lump rather than reading them raw from the lump
// pointer. This avoids problems with structure packing.
//
// JVAL: changed to support the old demo format.
//
//==============================================================================
procedure P_ParseDialogLump(const lumpname: string; const tag: integer; var dialogs: mapdialog_pa; var numdialogs: integer);
var
  i, j, k: integer;
  lumpnum: integer;
  lumplen: integer;
  olddialogs: oldmapdialog_pa;
  oldformat: boolean;
  voc: string;
begin
  lumpnum := W_CheckNumForName(lumpname);
  if lumpnum < 0 then
  begin
    dialogs := nil;
    numdialogs := 0;
    exit;
  end;

  lumplen := W_LumpLength(lumpnum);
  if lumplen = 0 then
  begin
    dialogs := nil;
    numdialogs := 0;
    exit;
  end;

  if lumplen mod SizeOf(mapdialog_t) = 0 then
  begin
    oldformat := false;
    numdialogs := lumplen div SizeOf(mapdialog_t);
  end
  else if lumplen mod SizeOf(oldmapdialog_t) = 0 then
  begin
    oldformat := true;
    numdialogs := lumplen div SizeOf(oldmapdialog_t);
  end
  else
  begin
    I_Warning('P_ParseDialogLump(): Can not determine dialog version, lump=%s', [lumpname]);
    dialogs := nil;
    numdialogs := 0;
    exit;
  end;

  dialogs := Z_Malloc(numdialogs * SizeOf(mapdialog_t), tag, nil);

  if oldformat then
  begin
    olddialogs := Z_Malloc(lumplen, PU_STATIC, nil);
    W_ReadLump(lumpnum, olddialogs);
    ZeroMemory(dialogs, numdialogs * SizeOf(mapdialog_t));
    for i := 0 to numdialogs - 1 do
    begin
      dialogs[i].speakerid := P_IdFromXlat(olddialogs[i].speakerid);
      dialogs[i].dropitem := P_IdFromXlat(olddialogs[i].dropitem);
      if olddialogs[i].voice <> 0 then
      begin
        sprintf(voc, 'VOC%d', [olddialogs[i].voice]);
        for j := 1 to length(voc) do
          dialogs[i].voice[j - 1] := voc[j];
        for j := length(voc) to MDLG_LUMPLEN - 1 do
          dialogs[i].voice[j] := #0;
      end;
      for j := 0 to MDLG_NAMELEN - 1 do
        dialogs[i].name[j] := olddialogs[i].name[j];
      for j := 0 to MDLG_TEXTLEN - 1 do
        dialogs[i].txt[j] := olddialogs[i].txt[j];
      for j := 0 to MDLG_MAXCHOICES - 1 do
      begin
        dialogs[i].choices[j] := olddialogs[i].choices[j];
        dialogs[i].choices[j].giveitem := P_IdFromXlat(dialogs[i].choices[j].giveitem);
        for k := 0 to MDLG_MAXITEMS - 1 do
          dialogs[i].choices[j].needitems[k] := P_IdFromXlat(dialogs[i].choices[j].needitems[k]);
      end;
    end;
    Z_Free(olddialogs);
  end
  else
    W_ReadLump(lumpnum, dialogs);
end;

//==============================================================================
//
// P_DialogLoad
//
// [STRIFE] New function
// haleyjd 09/02/10: Loads the dialog script for the current map. Also loads
// SCRIPT00 if it has not yet been loaded.
//
// JVAL: simplified :)
//
//==============================================================================
procedure P_DialogLoad;
begin
  // load the SCRIPTxy lump corresponding to MAPxy, if it exists.
  P_ParseDialogLump('SCRIPT' + IntToStrZFill(2, gamemap), PU_LEVEL, leveldialogs, numleveldialogs);

  if not script0loaded then
  begin
    P_ParseDialogLump('SCRIPT00', PU_STATIC, script0dialogs, numscript0dialogs);
    script0loaded := true;
  end;
end;

//==============================================================================
//
// P_PlayerHasItem
//
// [STRIFE] New function
// haleyjd 09/02/10: Checks for inventory items, quest flags, etc. for dialogs.
// Returns the amount possessed, or 0 if none.
//
//==============================================================================
function P_PlayerHasItem(player: Pplayer_t; _type: mobjtype_t): integer;
var
  i: integer;
begin
  if Ord(_type) > 0 then
  begin
    // check keys
    if (_type >= MT_KEY_BASE) and (_type < MT_INV_SHADOWARMOR) then
    begin
      if player.cards[Ord(_type) - Ord(MT_KEY_BASE)] then
        result := 1
      else
        result := 0;
      exit;
    end;

    // check sigil pieces
    if (_type >= MT_SIGIL_A) and (_type <= MT_SIGIL_E) then
    begin
      if Ord(_type) - Ord(MT_SIGIL_A) <= player.sigiltype then
        result := 1
      else
        result := 0;
      exit;
    end;

    // check quest tokens
    if (_type >= MT_TOKEN_QUEST1) and (_type <= MT_TOKEN_QUEST31) then
    begin
      result := player.questflags and _SHLW(1, Ord(_type) - Ord(MT_TOKEN_QUEST1));
      exit;
    end;

    // check inventory
    for i := 0 to NUMINVENTORY - 1 do
      if Ord(_type) = player.inventory[i]._type then
      begin
        result := player.inventory[i].amount;
        exit;
      end;
  end;

  result := 0;
end;

//==============================================================================
//
// P_DialogFind
//
// [STRIFE] New function
// haleyjd 09/03/10: Looks for a dialog definition matching the given
// Script ID # for an mobj.
//
//==============================================================================
function P_DialogFind(_type: mobjtype_t; jumptoconv: integer): mapdialog_p;
var
  i: integer;
begin
  // check the map-specific dialogs first
  for i := 0 to numleveldialogs - 1 do
    if Ord(_type) = leveldialogs[i].speakerid then
    begin
      if jumptoconv <= 1 then
      begin
        result := @leveldialogs[i];
        exit;
      end
      else
        dec(jumptoconv);
    end;

  // check SCRIPT00 dialogs next
  for i := 0 to numscript0dialogs - 1 do
    if Ord(_type) = script0dialogs[i].speakerid then
    begin
      result := @script0dialogs[i];
      exit;
    end;

  // the default dialog is script 0 in the SCRIPT00 lump.
  result := @script0dialogs[0];
end;

//==============================================================================
//
// P_DialogGetStates
//
// [STRIFE] New function
// haleyjd 09/03/10: Find the set of special dialog states (greetings, yes, no)
// for a particular thing type.
//
//==============================================================================
function P_DialogGetStates(const _type: mobjtype_t): dialogstateset_p;
var
  i: integer;
begin
  // look for a match by type
  for i := 0 to NUMDIALOGSTATESETS - 1 do
    if _type = dialogstatesets[i]._type then
    begin
      result := @dialogstatesets[i];
      exit;
    end;

  // return the default 0 record if no match.
  result := @dialogstatesets[0];
end;

//==============================================================================
//
// P_DialogGetMsg
//
// [STRIFE] New function
// haleyjd 09/03/10: Redirects dialog messages when the script indicates that
// the actor should use a random message stored in the executable instead.
//
//==============================================================================
function P_DialogGetMsg(const _message: string): string;
var
  i: integer;
  nameloc: string;
begin
  // if the message starts with "RANDOM"...
  if Pos('RANDOM', strupper(_message)) = 1 then
    if length(_message) >= 11 then
    begin
      nameloc := strupper(_message[8] + _message[9] + _message[10] + _message[11]);

      // look for a match in rndMessages for the string starting
      // 7 chars after "RANDOM_"
      for i := 0 to numrndmessages - 1 do
        if Pos(nameloc, rndMessages[i].type_name) = 1 then
        begin
          // found a match, so return a random message
          result := DEH_GetString(rndMessages[i].messages[M_Random mod MAXRNDMESSAGES]);
          exit;
        end;
    end;

  // otherwise, just return the message passed in.
  result := _message;
end;

//==============================================================================
//
// P_GiveInventoryItem
//
// [STRIFE] New function
// haleyjd 09/03/10: Give an inventory item to the player, if possible.
// villsa 09/09/10: Fleshed out routine
//
//==============================================================================
function P_GiveInventoryItem(player: Pplayer_t; sprnum: integer; _type: mobjtype_t): boolean;
var
  curinv, i: integer;
  item: integer;
  invtail: Pinventory_a;
begin
  curinv := 0;
  result := false;
  item := 0;

  // repaint the status bar due to inventory changing
  player.st_update := true;

  while true do
  begin
    // inventory is full
    if curinv > player.numinventory then
    begin
      result := true;
      exit;
    end;

    item := player.inventory[curinv]._type;
    if Ord(_type) < item then
    begin
      if curinv <> MAXINVENTORYSLOTS then
      begin
        // villsa - sort inventory item if needed
        invtail := @player.inventory[player.numinventory - 1];
        if player.numinventory > curinv then
        begin
          for i := player.numinventory downto curinv + 1 do
          begin
            invtail[1].sprite := invtail[0].sprite;
            invtail[1]._type := invtail[0]._type;
            invtail[1].amount := invtail[0].amount;
            invtail := Pinventory_a(integer(invtail) - SizeOf(inventory_t));
          end;
        end;

        // villsa - add inventory item
        player.inventory[curinv].amount := 1;
        player.inventory[curinv].sprite := sprnum;
        player.inventory[curinv]._type := Ord(_type);

        // sort cursor if needed
        if player.numinventory > 0 then
          if curinv <= player.inventorycursor then
            inc(player.inventorycursor);

        inc(player.numinventory);

        result := true;
        exit;
      end;

      exit;
    end;

    if Ord(_type) = item then
      break;

    inc(curinv);
  end;

  // check amount of inventory item by using the mass from mobjinfo
  result := player.inventory[curinv].amount < mobjinfo[item].mass;
  if result then
    inc(player.inventory[curinv].amount);
end;

//==============================================================================
//
// P_GiveItemToPlayer
//
// [STRIFE] New function
// haleyjd 09/03/10: Sorts out how to give something to the player.
// Not strictly just for inventory items.
// villsa 09/09/10: Fleshed out function
//
//==============================================================================
function P_GiveItemToPlayer(player: Pplayer_t; sprnum: integer; _type: mobjtype_t): boolean;
var
  i: integer;
  junk: line_t;
  sound: integer;
begin
  sound := Ord(sfx_itemup); // haleyjd 09/21/10: different sounds for items

  // set quest if mf_givequest flag is set
  if mobjinfo[Ord(_type)].flags and MF_GIVEQUEST <> 0 then
    player.questflags := player.questflags or _SHLW(1, mobjinfo[Ord(_type)].speed - 1);

  // check for keys
  if (_type >= MT_KEY_BASE) and (_type <= MT_NEWKEY5) then
  begin
    P_GiveCard(player, card_t(Ord(_type) - Ord(MT_KEY_BASE)));
    result := true;
    exit;
  end;

  // check for quest tokens
  if (_type >= MT_TOKEN_QUEST1) and (_type <= MT_TOKEN_QUEST31) then
  begin
    if mobjinfo[Ord(_type)].name2 <> '' then
    begin
      pickupstring := DEH_GetString(mobjinfo[Ord(_type)].name2);
      player._message := pickupstring;
    end;
    player.questflags := player.questflags or _SHLW(1, Ord(_type) - Ord(MT_TOKEN_QUEST1));

    if player = @players[consoleplayer] then
      S_StartSound(nil, sound);
    result := true;
    exit;
  end;

  // haleyjd 09/22/10: Refactored to give sprites higher priority than
  // mobjtypes and to implement missing logic.
  case sprnum of
    Ord(SPR_HELT): // This is given only by the "DONNYTRUMP" cheat (aka Midas)
      begin
        P_GiveInventoryItem(player, Ord(SPR_HELT), MT_TOKEN_TOUGHNESS);
        P_GiveInventoryItem(player, Ord(SPR_GUNT), MT_TOKEN_ACCURACY);

        // [STRIFE] Bizarre...
        for i := 0 to 5 * player.accuracy + 299 do
          P_GiveInventoryItem(player, Ord(SPR_COIN), MT_MONY_1);
      end;

    Ord(SPR_ARM1): // Armor 1
      begin
        if not P_GiveArmor(player, -2) then
          P_GiveInventoryItem(player, sprnum, _type);
      end;

    Ord(SPR_ARM2): // Armor 2
      begin
        if not P_GiveArmor(player, -1) then
          P_GiveInventoryItem(player, sprnum, _type);
      end;

    Ord(SPR_COIN): // 1 Gold
      begin
        P_GiveInventoryItem(player, Ord(SPR_COIN), MT_MONY_1);
      end;

    Ord(SPR_CRED): // 10 Gold
      begin
        for i := 0 to 9 do
          P_GiveInventoryItem(player, Ord(SPR_COIN), MT_MONY_1);
      end;

    Ord(SPR_SACK): // 25 gold
      begin
        for i := 0 to 24 do
          P_GiveInventoryItem(player, Ord(SPR_COIN), MT_MONY_1);
      end;

    Ord(SPR_CHST): // 50 gold
      begin
        for i := 0 to 49 do
          P_GiveInventoryItem(player, Ord(SPR_COIN), MT_MONY_1);
      end; // haleyjd 20141215: missing break, caused Rowan to not take ring from you.

    Ord(SPR_BBOX): // Box of Bullets
      begin
        if not P_GiveAmmo(player, am_bullets, 5) then
        begin
          result := false;
          exit;
        end;
      end;

    Ord(SPR_BLIT): // Bullet Clip
      begin
        if not P_GiveAmmo(player, am_bullets, 1) then
        begin
          result := false;
          exit;
        end;
      end;

    Ord(SPR_PMAP): // Map powerup
      begin
        if not P_GivePower(player, Ord(pw_allmap)) then
        begin
          result := false;
          exit;
        end;
        sound := Ord(sfx_yeah); // bluh-doop!
      end;

    Ord(SPR_COMM): // Communicator
      begin
        if not P_GivePower(player, Ord(pw_communicator)) then
        begin
          result := false;
          exit;
        end;
        sound := Ord(sfx_yeah); // bluh-doop!
      end;

    Ord(SPR_MSSL): // Mini-missile
      begin
        if not P_GiveAmmo(player, am_missiles, 1) then
        begin
          result := false;
          exit;
        end;
      end;

    Ord(SPR_ROKT): // Crate of missiles
      begin
        if not P_GiveAmmo(player, am_missiles, 5) then
        begin
          result := false;
          exit;
        end;
      end;

    Ord(SPR_BRY1): // Battery cell
      begin
        if not P_GiveAmmo(player, am_cell, 1) then
        begin
          result := false;
          exit;
        end;
      end;

    Ord(SPR_CPAC): // Cell pack
      begin
        if not P_GiveAmmo(player, am_cell, 5) then
        begin
          result := false;
          exit;
        end;
      end;

    Ord(SPR_PQRL): // Poison bolts
      begin
        if not P_GiveAmmo(player, am_poisonbolts, 5) then
        begin
          result := false;
          exit;
        end;
      end;

    Ord(SPR_XQRL): // Electric bolts
      begin
        if not P_GiveAmmo(player, am_elecbolts, 5) then
        begin
          result := false;
          exit;
        end;
      end;

    Ord(SPR_GRN1): // HE Grenades
      begin
        if not P_GiveAmmo(player, am_hegrenades, 1) then
        begin
          result := false;
          exit;
        end;
      end;

    Ord(SPR_GRN2): // WP Grenades
      begin
        if not P_GiveAmmo(player, am_wpgrenades, 1) then
        begin
          result := false;
          exit;
        end;
      end;

    Ord(SPR_BKPK): // Backpack (aka Ammo Satchel)
      begin
        if not player.backpack then
        begin
            for i := 0 to Ord(NUMAMMO) - 1 do
              player.maxammo[i] := player.maxammo[i] * 2;

            player.backpack := true;
        end;
        for i := 0 to Ord(NUMAMMO) - 1 do
          P_GiveAmmo(player, ammotype_t(i), 1);
      end;

    Ord(SPR_RIFL): // Assault Rifle
      begin
        if player.weaponowned[Ord(wp_rifle)] then
        begin
          result := false;
          exit;
        end;

        if not P_GiveWeapon(player, wp_rifle, false) then
        begin
          result := false;
          exit;
        end;

        sound := Ord(sfx_wpnup); // SHK-CHK!
      end;

    Ord(SPR_FLAM): // Flamethrower
      begin
        if player.weaponowned[Ord(wp_flame)] then
        begin
          result := false;
          exit;
        end;

        if not P_GiveWeapon(player, wp_flame, false) then
        begin
          result := false;
          exit;
        end;

        sound := Ord(sfx_wpnup); // SHK-CHK!
      end;

    Ord(SPR_MMSL): // Mini-missile Launcher
      begin
        if player.weaponowned[Ord(wp_missile)] then
        begin
          result := false;
          exit;
        end;

        if not P_GiveWeapon(player, wp_missile, false) then
        begin
          result := false;
          exit;
        end;

        sound := Ord(sfx_wpnup); // SHK-CHK!
      end;

    Ord(SPR_TRPD): // Mauler
      begin
        if player.weaponowned[Ord(wp_mauler)] then
        begin
          result := false;
          exit;
        end;

        if not P_GiveWeapon(player, wp_mauler, false) then
        begin
          result := false;
          exit;
        end;

        sound := Ord(sfx_wpnup); // SHK-CHK!
      end;

    Ord(SPR_CBOW): // Here's a crossbow. Just aim straight, and *SPLAT!*
      begin
        if player.weaponowned[Ord(wp_elecbow)] then
        begin
          result := false;
          exit;
        end;

        if not P_GiveWeapon(player, wp_elecbow, false) then
        begin
          result := false;
          exit;
        end;

        sound := Ord(sfx_wpnup); // SHK-CHK!
      end;

    Ord(SPR_TOKN): // Miscellaneous items - These are determined by thingtype.
      begin
        case _type of
          MT_KEY_HAND: // Severed hand
            begin
              P_GiveCard(player, key_SeveredHand);
            end;

          MT_MONY_300: // 300 Gold (this is the only way to get it, in fact)
            begin
              for i := 0 to 299 do
                P_GiveInventoryItem(player, Ord(SPR_COIN), MT_MONY_1);
            end;

          MT_TOKEN_AMMO: // Ammo token - you get this from the Weapons Trainer
            begin
              if player.ammo[Ord(am_bullets)] >= 50 then
              begin
                result := false;
                exit;
              end;

              player.ammo[Ord(am_bullets)] := 50;
            end;

          MT_TOKEN_HEALTH: // Health token - from the Front's doctor
            begin
              if not P_GiveBody(player, healthamounts[Ord(gameskill)]) then
              begin
                result := false;
                exit;
              end;
            end;

          MT_TOKEN_ALARM: // Alarm token - particularly from the Oracle.
            begin
              P_NoiseAlert(player.mo, player.mo);
              A_AlertSpectreC(dialogtalker); // BUG: assumes in a dialog o_O
            end;

          MT_TOKEN_DOOR1: // Door special 1
            begin
              junk.tag := 222;
              EV_DoDoor(@junk, vld_open);
            end;

          MT_TOKEN_PRISON_PASS: // Door special 1 - Prison pass
            begin
              junk.tag := 223;
              EV_DoDoor(@junk, vld_open);
              if gamemap = 2 then // If on Tarnhill, give Prison pass object
                  P_GiveInventoryItem(player, sprnum, _type);
            end;

          MT_TOKEN_SHOPCLOSE: // Door special 3 - "Shop close" - unused?
            begin
              junk.tag := 222;
              EV_DoDoor(@junk, vld_close);
            end;

          MT_TOKEN_DOOR3: // Door special 4 (or 3? :P )
            begin
              junk.tag := 224;
              EV_DoDoor(@junk, vld_close);
            end;

          MT_TOKEN_STAMINA: // Stamina upgrade
            begin
              if player.stamina >= 100 then
              begin
                result := false;
                exit;
              end;

              player.stamina := player.stamina + 10;
              P_GiveBody(player, 200); // full healing
            end;

          MT_TOKEN_NEW_ACCURACY: // Accuracy upgrade
            begin
              if player.accuracy >= 100 then
              begin
                result := false;
                exit;
              end;

              player.accuracy := player.accuracy + 10;
            end;

          MT_SLIDESHOW: // Slideshow (start a finale)
            begin
              gameaction := ga_victory;
              if gamemap = 10 then
                P_GiveItemToPlayer(player, Ord(SPR_TOKN), MT_TOKEN_QUEST17);
            end;

        else // The default is to just give it as an inventory item.
          begin
            P_GiveInventoryItem(player, sprnum, _type);
          end;
        end;
      end;

    else // The ultimate default: Give it as an inventory item.
      if not P_GiveInventoryItem(player, sprnum, _type) then
      begin
        result := false;
        exit;
      end;
  end;

  // Play sound.
  if player = @players[consoleplayer] then
    S_StartSound(nil, sound);

  result := true;
end;

//==============================================================================
//
// P_TakeDialogItem
//
// [STRIFE] New function
// haleyjd 09/03/10: Removes needed items from the player's inventory.
//
//==============================================================================
procedure P_TakeDialogItem(player: Pplayer_t; _type: integer; amount: integer);
var
  i, j: integer;
begin
  if amount <= 0 then
    exit;

  for i := 0 to player.numinventory - 1 do
  begin
    // find a matching item
    if _type <> player.inventory[i]._type then
      continue;

    // if there is none left...
    player.inventory[i].amount := player.inventory[i].amount - amount;
    if player.inventory[i].amount < 1 then
    begin
      // ...shift everything above it down

      // BUG: They should have stopped at j < numinventory. This
      // seems to implicitly assume that numinventory is always at
      // least one less than the max # of slots, otherwise it
      // pulls in data from the following player_t fields:
      // st_update, numinventory, inventorycursor, accuracy, stamina
      for j := i + 1 to player.numinventory do
        player.inventory[j - 1] := player.inventory[j];

      // blank the topmost slot
      // BUG: This will overwrite the aforementioned fields if
      // numinventory is equal to the number of slots!
      // STRIFE-TODO: Overflow emulation?
      // JVAL: fix this
      if player.numinventory < NUMINVENTORY then
      begin
        player.inventory[player.numinventory]._type := nummobjtypes;
        player.inventory[player.numinventory].sprite := -1;
        player.inventory[player.numinventory].amount := 0;
      end;
      dec(player.numinventory);

      // update cursor position
      if player.inventorycursor >= player.numinventory then
        if player.inventorycursor > 0 then
          dec(player.inventorycursor);
    end;  // end if

    exit; // done!

  end;    // end for
end;

//==============================================================================
//
// P_DialogDoChoice
//
// [STRIFE] New function
// haleyjd 09/05/10: Handles making a choice in a dialog. Installed as the
// callback for all items in the dialogmenu structure.
//
//==============================================================================
procedure P_DialogDoChoice(choice: integer);
var
  i, nextdialog: integer;
  candochoice: boolean;
  _message: string;
  currentchoice: mapdlgchoice_p;
  item: integer;
  count: integer;
  objective: integer;
begin
  nextdialog := 0;
  candochoice := true;
  _message := '';

  if choice = -1 then
    choice := dialogmenu.numitems - 1;

  currentchoice := @currentdialog.choices[choice];

  S_StopSound(nil);

  // villsa 09/08/10: converted into for loop
  for i := 0 to MDLG_MAXITEMS - 1 do
    if P_PlayerHasItem(dialogplayer, mobjtype_t(currentchoice.needitems[i])) < currentchoice.needamounts[i] then
    begin
      candochoice := false; // nope, missing something
      break;
    end;

  if (choice <> dialogmenu.numitems - 1) and candochoice then
  begin
    _message := PascalText(@currentchoice.txtok, SizeOf(currentchoice.txtok));
    if Ord(dialogtalkerstates.yes) <> 0 then
      P_SetMobjState(dialogtalker, dialogtalkerstates.yes);

    item := currentchoice.giveitem;
    if (item < 0) or P_GiveItemToPlayer(dialogplayer, states[mobjinfo[item].spawnstate].sprite, mobjtype_t(item)) then
    begin
      // if successful, take needed items
      // villsa 09/08/10: converted into for loop
      for count := 0 to MDLG_MAXITEMS - 1 do
        P_TakeDialogItem(dialogplayer, currentchoice.needitems[count], currentchoice.needamounts[count]);
    end
    else
      _message := DEH_GetString('You seem to have enough!');

    // store next dialog into the talking actor
    nextdialog := currentchoice.next;
    if nextdialog <> 0 then
      dialogtalker.miscdata := abs(nextdialog);
  end
  else
  begin
    // not successful
    _message := PascalText(@currentchoice.txtno, SizeOf(currentchoice.txtno));
    if Ord(dialogtalkerstates.no) <> 0 then
      P_SetMobjState(dialogtalker, dialogtalkerstates.no);
  end;

  if choice <> dialogmenu.numitems - 1 then
  begin
    objective := currentchoice.objective;
    if objective <> 0 then
      mission_objective := W_TextLumpName('LOG' + itoa(objective));
    // haleyjd 20130301: v1.31 hack: if first char of message is a period,
    // clear the player's message. Is this actually used anywhere?
    if gameversion = exe_strife_1_31 then
      if _message <> '' then
        if _message[1] = '.' then
          _message := '';
    dialogplayer._message := _message;
  end;

  dialogtalker.angle := dialogtalkerangle;
  dialogplayer.st_update := true;
  M_ClearMenus;

  if (nextdialog >= 0) or (gameaction = ga_victory) then // Macil hack
    menuindialog := false
  else
    P_DialogStart(dialogplayer);
end;

//==============================================================================
//
// P_DialogDrawer
//
// This function is set as the drawer callback for the dialog menu.
//
//==============================================================================
procedure P_DialogDrawer;
var
  angle: angle_t;
  i: integer;
  y: integer;
  height: integer;
  finaly: integer;
  choicetext: string;
begin
  MT_ZeroMemory(screens[SCN_TMP], 320 * 200);
  // Run down bonuscount faster than usual so that flashes from being given
  // items are less obvious.
  if dialogplayer.bonuscount <> 0 then
  begin
    dialogplayer.bonuscount := dialogplayer.bonuscount - 3;
    if dialogplayer.bonuscount < 0 then
      dialogplayer.bonuscount := 0;
  end;

  angle := R_PointToAngle2(dialogplayer.mo.x,
                           dialogplayer.mo.y,
                           dialogtalker.x,
                           dialogtalker.y);
  angle := angle - dialogplayer.mo.angle;

  // Dismiss the dialog if the player is out of alignment, or the thing he was
  // talking to is now engaged in battle.
  if ((angle > ANG45) and (angle < (ANG270 + ANG45))) or
     ((dialogtalker.flags and MF_NODIALOG) <> 0) then
    P_DialogDoChoice(dialogmenu.numitems - 1);

  dialogtalker.reactiontime := 2;

  // draw background
  if dialogbgpiclumpnum <> -1 then
    V_DrawPatch(0, 0, SCN_TMP, dialogbgpiclumpnum, false);

  // if there's a valid background pic, delay drawing the rest of the menu
  // for a while; otherwise, it will appear immediately
  if (dialogbgpiclumpnum = -1) or (menupausetime <= gametic) then
  begin
    if menuindialog then
    begin
      // time to pause the game?
      if menupausetime + 3 < gametic then
        menupause := true;
    end;

    // draw character name
    M_WriteText(12, 18, dialogname);
    y := 28;

    // show text (optional for dialogs with voices)
    if dialogshowtext or (currentdialog.voice[0] = #0) then
      y := M_WriteText(20, 28, dialogtext).y;

    height := 20 * dialogmenu.numitems;

    finaly := 175 - height;     // preferred height
    if y > finaly then
      finaly := 199 - height; // height it will bump down to if necessary.

    // draw divider
    M_WriteText(42, finaly - 6, DEH_GetString('______________________________'));

    dialogmenu.y := finaly + 6;
    y := 0;

    // draw the menu items
    for i := 0 to dialogmenu.numitems - 2 do
    begin
      choicetext := itoa(i + 1) + ') ' + PascalText(currentdialog.choices[i].txt, SizeOf(currentdialog.choices[i].txt));

      // alternate text for items that need money
      if currentdialog.choices[i].needamounts[0] > 0 then
        choicetext := choicetext + ' for ' + itoa(currentdialog.choices[i].needamounts[0]);

      M_WriteText(dialogmenu.x, dialogmenu.y + 3 + y, choicetext);
      y := y + 19;
    end;

    // draw the final item for dismissing the dialog
    M_WriteText(dialogmenu.x, dialogmenu.y + 3 + y, dialoglastmsgbuffer);
  end;
  V_CopyRectTransparent(0, 0, SCN_TMP, 320, 200, 0, 0, SCN_FG, true);
end;

//==============================================================================
//
// P_DialogStartP1
//
// [STRIFE] New function
// haleyjd 09/13/10: This is a hack used by the finale system.
//
//==============================================================================
procedure P_DialogStartP1;
begin
  P_DialogStart(@players[0]);
end;

//==============================================================================
//
// P_DialogStart
//
// villsa [STRIFE] New function
//
//==============================================================================
procedure P_DialogStart(player: Pplayer_t);
var
  i: integer;
  pic: integer;
  rnd: integer;
  byetext: string;
  jumptoconv: integer;
  stmp: string;
begin
  if menuactive or netgame then
    exit;

  // are we facing towards our NPC?
  P_AimLineAttack(player.mo, player.mo.angle, (128 * FRACUNIT));
  if linetarget = nil then
  begin
    P_AimLineAttack(player.mo, player.mo.angle + (ANG90 div 16), (128 * FRACUNIT));
    if linetarget = nil then
      P_AimLineAttack(player.mo, player.mo.angle - (ANG90 div 16), (128 * FRACUNIT));
  end;

  if linetarget = nil then
    exit;

  // already in combat, can't talk to it
  if linetarget.flags and MF_NODIALOG <> 0 then
    exit;

  // set pointer to the character talking
  dialogtalker := linetarget;
  player.lastdialogtalker := dialogtalker;

  // play a sound
  if player = @players[consoleplayer] then
    S_StartSound(nil, Ord(sfx_radio));

  linetarget.target := player.mo;           // target the player
  dialogtalker.reactiontime := 2;           // set reactiontime
  dialogtalkerangle := dialogtalker.angle;  // remember original angle

  // face talker towards player
  A_FaceTarget(dialogtalker);

  // face towards NPC's direction
  // JVAL: use DelphiDoom's P_PlayerFaceMobj function, turn towards dialogtalker in half second
  P_PlayerFaceMobj(player, dialogtalker, TICRATE div 2);

  player.nextfire := leveltime + TICRATE div 2;

  // set pointer to player talking
  dialogplayer := player;

  // haleyjd 09/08/10: get any stored dialog state from this object
  jumptoconv := linetarget.miscdata;

  // check item requirements
  while true do
  begin
    currentdialog := P_DialogFind(mobjtype_t(linetarget._type), jumptoconv);

    // dialog's jumptoconv equal to 0? There's nothing to jump to.
    if currentdialog.jumptoconv = 0 then
      break;

    // villsa 09/08/10: converted into for loop
    i := 0;
    while i < MDLG_MAXITEMS do
    begin
      // if the item is non-zero, the player must have at least one in his
      // or her inventory
      if (currentdialog.checkitem[i] <> 0) and
         (P_PlayerHasItem(dialogplayer, mobjtype_t(currentdialog.checkitem[i])) < 1) then
        break;
      inc(i);
    end;

    if i < MDLG_MAXITEMS then // didn't find them all? this is our dialog!
      break;

    jumptoconv := currentdialog.jumptoconv;
  end;

  stmp := PascalText(@currentdialog.txt, SizeOf(currentdialog.txt));
  stmp := P_DialogGetMsg(stmp);
  dialogtext := M_DialogDimMsg(20, 28, stmp, false);

  // get states
  dialogtalkerstates := P_DialogGetStates(mobjtype_t(linetarget._type));

  // have talker greet the player
  if dialogtalkerstates.greet <> S_NULL then
    P_SetMobjState(dialogtalker, dialogtalkerstates.greet);

  // get talker's name
  dialogname := PascalText(@currentdialog.name, SizeOf(currentdialog.name));

  if dialogname = '' then
  begin
    // use a fallback:
    if mobjinfo[linetarget._type].name2 <> '' then
      dialogname := mobjinfo[linetarget._type].name2 // mobjtype name
    else
      dialogname := DEH_GetString('Person'); // default name - like Joe in Doom 3 :P
  end;

  // setup number of choices to choose from
  i := 0;
  while i < MDLG_MAXCHOICES do
  begin
    if currentdialog.choices[i].giveitem = 0 then
      break;
    inc(i);
  end;

  // set number of choices to menu
  dialogmenu.numitems := i + 1;

  rnd := M_Random mod 3;

  // setup dialog menu
  M_StartControlPanel;
  menupause := false;
  menuindialog := true;
  menupausetime := gametic + 17;
  currentMenu := @dialogmenu;

  if i >= dialogmenu.lastOn then
    itemOn := dialogmenu.lastOn
  else
    itemOn := 0;

  // get backdrop
  pic := W_CheckNumForName(currentdialog.backpic);
  dialogbgpiclumpnum := pic;
  if pic <> -1 then
    V_DrawPatch(0, 0, SCN_TMP, pic, false);

  // get voice
  S_StartVoice(PascalText(@currentdialog.voice, SizeOf(currentdialog.voice)));

  // get bye text
  if rnd = 2 then
    byetext := DEH_GetString('BYE!')
  else if rnd = 1 then
    byetext := DEH_GetString('Thanks, Bye!')
  else
    byetext := DEH_GetString('See you later!');

  sprintf(dialoglastmsgbuffer, '%d) %s', [i + 1, byetext]);
end;

//==============================================================================
//
// M_InitDialogs
//
//==============================================================================
procedure M_InitDialogs;
var
  i: integer;
  pmi: Pmenuitem_t;
begin
  pmi := @dialogmenuitems[0];
  for i := 0 to NUMDIALOGMENUITEMS - 1 do
  begin
    pmi.status := 1;
    pmi.name := '/';
    pmi.cmd := '';
    pmi.routine := @P_DialogDoChoice;
    pmi.pBoolVal := nil;
    pmi.alphaKey := itoa(i)[1];
    inc(pmi);
  end;

  dialogmenu.numitems := NUMDIALOGMENUITEMS;
  dialogmenu.prevMenu := nil;
  dialogmenu.menuitems := Pmenuitem_tArray(@dialogmenuitems);
  dialogmenu.drawproc := @P_DialogDrawer;  // draw routine
  dialogmenu.x := 42;
  dialogmenu.y := 75;
  dialogmenu.lastOn := 0;
  dialogmenu.itemheight := 19; // LINEHEIGHT;
end;

end.
