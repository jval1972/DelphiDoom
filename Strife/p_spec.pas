//------------------------------------------------------------------------------
//
//  DelphiStrife: A modified and improved Strife source port for Windows.
//
//  Based on:
//    - Linux Doom by "id Software"
//    - Chocolate Strife by "Simon Howard"
//    - DelphiDoom by "Jim Valavanis"
//
//  Copyright (C) 1993-1996 by id Software, Inc.
//  Copyright (C) 2005 Simon Howard
//  Copyright (C) 2010 James Haley, Samuel Villarreal
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
//  Foundation, inc., 59 Temple Place - Suite 330, Boston, MA
//  02111-1307, USA.
//
// DESCRIPTION:
//  Implements special effects:
//  Texture animation, height or lighting changes
//   according to adjacent sectors, respective
//   utility functions, etc.
//  Line Tag handling. Line and Sector triggers.
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit p_spec;

interface

uses
  doomdef,
  m_fixed,
  d_player,
  d_think,
  p_local,
  p_mobj_h,
  r_defs;

//
// End-level timer (-TIMER option)
//

const
//      Define values for map objects
  MO_TELEPORTMAN = 14;

// [STRIFE] Teleportation flags - teleflags
// Not to be conflated with telefrags, though they be tangentially related ;)
const
  TF_NOSRCSND = $01;
  TF_NODSTSND = $02;
  TF_NODSTFOG = $10;
  TF_NOSRCFOG = $20;

  TF_NORMAL      = 0;
  TF_DSTSILENCE  = (TF_NODSTSND or TF_NODSTFOG);      // 0x12 (18) (Not used)
  TF_SRCSILENCE  = (TF_NOSRCSND or TF_NOSRCFOG);      // 0x21 (33)
  TF_FULLSILENCE = (TF_SRCSILENCE or TF_DSTSILENCE);  // 0x33 (51)

// at game start
procedure P_InitPicAnims;

// at map load
procedure P_SpawnSpecials;

// every tic
procedure P_UpdateSpecials;

// when needed
procedure P_ShootSpecialLine(thing: Pmobj_t; line: Pline_t);

procedure P_CrossSpecialLine(linenum: integer; side: integer; thing: Pmobj_t);

procedure P_CrossSpecialLinePtr(line: Pline_t; side: integer; thing: Pmobj_t);

procedure P_PlayerInSpecialSector(player: Pplayer_t; const sector: Psector_t; const height: fixed_t);  // JVAL: 3d Floors

function twoSided(sector: integer; line: integer): boolean;

function twoSidedS(sector: Psector_t; line: integer): boolean;

function getSide(currentSector: integer; line: integer; side: integer): Pside_t;

function P_FindLowestFloorSurrounding(sec: Psector_t): fixed_t;

function P_FindHighestFloorSurrounding(sec: Psector_t): fixed_t;

function P_FindNextHighestFloor(sec: Psector_t; currentheight: integer): fixed_t;

function P_FindLowestCeilingSurrounding(sec: Psector_t): fixed_t;

function P_FindHighestCeilingSurrounding(sec: Psector_t): fixed_t;

function P_FindSectorFromLineTag(line: Pline_t; start: integer): integer;

function P_FindSectorFromLineTag2(line: Pline_t; var start: integer): integer;

function P_FindLineFromLineTag2(line: Pline_t; var start: integer): integer;

function P_FindMinSurroundingLight(sector: Psector_t; max: integer): integer;

function getNextSector(line: Pline_t; sec: Psector_t): Psector_t;

//
// P_LIGHTS
//
type
  fireflicker_t = record
    thinker: thinker_t;
    sector: Psector_t;
    count: integer;
    maxlight: integer;
    minlight: integer;
  end;
  Pfireflicker_t = ^fireflicker_t;

  lightflash_t = record
    thinker: thinker_t;
    sector: Psector_t;
    count: integer;
    maxlight: integer;
    minlight: integer;
    maxtime: integer;
    mintime: integer;
  end;
  Plightflash_t = ^lightflash_t;

  strobe_t = record
    thinker: thinker_t;
    sector: Psector_t;
    count: integer;
    minlight: integer;
    maxlight: integer;
    darktime: integer;
    brighttime: integer;
  end;
  Pstrobe_t = ^strobe_t;

  glow_t = record
    thinker: thinker_t;
    sector: Psector_t;
    minlight: integer;
    maxlight: integer;
    direction: integer;
  end;
  Pglow_t = ^glow_t;

const
  GLOWSPEED = 8;
  STROBEBRIGHT = 5;
  FASTDARK = 15;
  SLOWDARK = 35;

type
  bwhere_e = (
    top,
    middle,
    bottom
  );

  button_t = record
    line: Pline_t;
    where: bwhere_e;
    btexture: integer;
    btimer: integer;
    soundorg: Pmobj_t;
  end;
  Pbutton_t = ^button_t;

const
  // 8 players, 4 buttons each at once, max.
  MAXBUTTONS = 32;

  // 1 second, in ticks.
  BUTTONTIME = 35;


type
  change_e = (
    trigChangeOnly,
    numChangeOnly
  );

type
//
// P_PLATS
//
  plat_e = (
    up,
    down,
    waiting,
    in_stasis
  );

  plattype_e = (
    perpetualRaise,
    downWaitUpStay,
    slowDWUS,           // villsa [STRIFE]
    raiseAndChange,
    raiseToNearestAndChange,
    blazeDWUS,
    upWaitDownStay,     // villsa [STRIFE]
    genLift,      //jff added to support generalized Plat types
    genPerpetual,
    toggleUpDn    //jff 3/14/98 added to support instant toggle type
  );

  plat_t = record
    thinker: thinker_t;
    sector: Psector_t;
    speed: fixed_t;
    low: fixed_t;
    high: fixed_t;
    wait: integer;
    count: integer;
    status: plat_e;
    oldstatus: plat_e;
    crush: boolean;
    tag: integer;
    _type: plattype_e;
  end;
  Pplat_t = ^plat_t;

const
  PLATWAIT = 3;
  PLATSPEED = FRACUNIT;
  MAXPLATS = 512;  // JVAL Originally was 30

type
//
// P_DOORS
//
  vldoor_e = (
    vld_normal,
    vld_close30ThenOpen,
    vld_close,
    vld_open,
    vld_raiseIn5Mins,
    vld_blazeRaise,
    vld_blazeOpen,
    vld_blazeClose,
    vld_shopClose,          // villsa [STRIFE]
    vld_splitRaiseNearest,  // villsa [STRIFE]
    vld_splitOpen,          // villsa [STRIFE]

    //jff 02/05/98 add generalize door types
    vld_genRaise,
    vld_genBlazeRaise,
    vld_genOpen,
    vld_genBlazeOpen,
    vld_genClose,
    vld_genBlazeClose,
    vld_genCdO,
    vld_genBlazeCdO
  );

  vldoor_t = record
    thinker: thinker_t;
    _type: vldoor_e;
    sector: Psector_t;
    line: Pline_t;
    topheight: fixed_t;
    speed: fixed_t;

    // 1 = up, 0 = waiting at top, -1 = down
    direction: integer;

    // tics to wait at the top
    topwait: integer;
    // (keep in case a door going down is reset)
    // when it reaches 0, start going down
    topcountdown: integer;

    // villsa [STRIFE] new field - sound to play when opening
    opensound: integer;

    // villsa [STRIFE] new field - sound to play when closing
    closesound: integer;
  end;
  Pvldoor_t = ^vldoor_t;

const
  VDOORSPEED = FRACUNIT * 2;
  VDOORWAIT = 150;

type
//
// P_CEILNG
//
  ceiling_e = (
    lowerToFloor,
    raiseToHighest,
    lowerToLowest,
    lowerToMaxFloor,
    lowerAndCrush,
    crushAndRaise,
    fastCrushAndRaise,
    silentCrushAndRaise,

    //jff 02/04/98 add types for generalized ceiling mover
    genCeiling,
    genCeilingChg,
    genCeilingChg0,
    genCeilingChgT,

    //jff 02/05/98 add types for generalized ceiling mover
    genCrusher,
    genSilentCrusher
  );

  ceiling_t = record
    thinker: thinker_t;
    _type: ceiling_e;
    sector: Psector_t;
    bottomheight: fixed_t;
    topheight: fixed_t;
    speed: fixed_t;
    oldspeed: fixed_t; // JVAL BOOM compatibility
    crush: boolean;
    // 1 = up, 0 = waiting, -1 = down
    direction: integer;
  //jff 02/04/98 add these to support ceiling changers
    newspecial: integer;
    oldspecial: smallint; //jff 3/14/98 add to fix bug in change transfers // JVAL SOS mayby integer
    texture: smallint; // JVAL BOOM compatibility
    // ID
    tag: integer;
    olddirection: integer;
  end;
  Pceiling_t = ^ceiling_t;

const
  CEILSPEED = FRACUNIT;
  CEILWAIT = 150;
  MAXCEILINGS = 128; // JVAL 20210130 - Old value was 30

type
// villsa [STRIFE] resurrected sliding doors
//
//      Sliding doors...
//
  sd_e = (
    sd_opening,
    sd_waiting,
    sd_closing
  );

  sdt_e = (
    sdt_openOnly,
    sdt_closeOnly,
    sdt_openAndClose
  );

// villsa [STRIFE] Rogue added a second line_t in the struct
// backsector is removed
type
  slidedoor_t = record
    thinker: thinker_t;
    _type: sdt_e;
    line1: Pline_t;
    line2: Pline_t;
    frame: integer;
    whichDoorIndex: integer;
    timer: integer;
    frontsector: Psector_t;
    status: sd_e;
  end;
  slidedoor_p = ^slidedoor_t;
  Pslidedoor_t = ^slidedoor_t;

// villsa [STRIFE] no front/back frames
type
  slidename_t = record
    frame1: string;
    frame2: string;
    frame3: string;
    frame4: string;
    frame5: string;
    frame6: string;
    frame7: string;
    frame8: string;
  end;

// villsa [STRIFE] no front/back frames
type
  slideframe_t = record
    frames: array [0..7] of integer;
  end;

const
// how many frames of animation
  SNUMFRAMES = 8;       // villsa [STRIFE] changed from 4 to 8

  SDOORWAIT = TICRATE * 3;
  SWAITTICS = 4;

// how many diff. types of anims
  MAXSLIDEDOORS = 8;       // villsa [STRIFE] changed from 5 to 8

type
//
// P_FLOOR
//
  floor_e = (
    // lower floor to highest surrounding floor
    lowerFloor,

    // lower floor to lowest surrounding floor
    lowerFloorToLowest,

    // lower floor to highest surrounding floor VERY FAST
    turboLower,

    // raise floor to lowest surrounding CEILING
    raiseFloor,

    // raise floor to next highest surrounding floor
    raiseFloorToNearest,

    //jff 02/03/98 lower floor to next lowest neighbor
    lowerFloorToNearest,

    //jff 02/03/98 lower floor 24 absolute
    lowerFloor24,

    //jff 02/03/98 lower floor 32 absolute
    lowerFloor32Turbo,

    // raise floor to shortest height texture around it
    raiseToTexture,

    // lower floor to lowest surrounding floor
    //  and change floorpic
    lowerAndChange,

    raiseFloor64,
    raiseFloor24,

    //jff 02/03/98 raise floor 32 absolute
    raiseFloor32Turbo,

    raiseFloor24AndChange,
    raiseFloorCrush,

    // raise to next highest floor, turbo-speed
    raiseFloorTurbo,
    donutRaise,
    raiseFloor512,
    raiseFloor512AndChange,

    //jff 02/04/98  add types for generalized floor mover
    genFloor,
    genFloorChg,
    genFloorChg0,
    genFloorChgT,

    //new types for stair builders
    buildStair,
    genBuildStair
  );

  stair_e = (
    build8, // slowly build by 8
    turbo16, // quickly build by 16
    buildDown16 // haleyjd 09/24/10: [STRIFE] new stair type
  );

  floormove_t = record
    thinker: thinker_t;
    _type: floor_e;
    crush: boolean;
    sector: Psector_t;
    direction: integer;
    newspecial: integer;
    oldspecial: smallint;
    texture: smallint;
    floordestheight: fixed_t;
    speed: fixed_t;
  end;
  Pfloormove_t = ^floormove_t;

  elevator_e = (
    elevateUp,
    elevateDown,
    elevateCurrent
  );

  elevator_t = record
    thinker: thinker_t;
    _type: elevator_e;
    sector: Psector_t;
    direction: integer;
    floordestheight: fixed_t;
    ceilingdestheight: fixed_t;
    speed: fixed_t;
  end;
  Pelevator_t = ^elevator_t;

// phares 3/12/98: added new model of friction for ice/sludge effects

  friction_t = record
    thinker: thinker_t;   // Thinker structure for friction
    friction: integer;    // friction value (E800 = normal)
    movefactor: integer;  // inertia factor when adding to momentum
    affectee: integer;    // Number of affected sector
  end;
  Pfriction_t = ^friction_t;

// phares 3/20/98: added new model of Pushers for push/pull effects

  pushertype_e = (
    p_push,
    p_pull,
    p_wind,
    p_current
  );

  pusher_t = record
    thinker: thinker_t;   // Thinker structure for Pusher
    _type: pushertype_e;
    source: Pmobj_t;      // Point source if point pusher
    x_mag: integer;       // X Strength
    y_mag: integer;       // Y Strength
    magnitude: integer;   // Vector strength for point pusher
    radius: integer;      // Effective radius for point pusher
    x, y: integer;        // X of point source if point pusher
    affectee: integer;    // Number of affected sector
  end;
  Ppusher_t = ^pusher_t;

const
  ELEVATORSPEED = 4 * FRACUNIT;
  FLOORSPEED = FRACUNIT;

type
  result_e = (
    ok,
    crushed,
    pastdest
  );

// JVAL: BOOM compatibility
  special_e = (
    floor_special,
    ceiling_special,
    lighting_special
  );

// JVAL: BOOM compatibility
function P_SectorActive(const s: special_e; const sec: Psector_t): boolean;

function P_FindNextLowestFloor(sec: Psector_t; currentheight: fixed_t): fixed_t;

function P_FindShortestTextureAround(secnum: integer): fixed_t;

function P_FindModelCeilingSector(ceildestheight: fixed_t; secnum: integer): Psector_t;

function P_FindModelFloorSector(floordestheight: fixed_t; secnum: integer): Psector_t;

function P_FindNextHighestCeiling(sec: Psector_t; currentheight: fixed_t): fixed_t;

function P_FindNextLowestCeiling(sec: Psector_t; currentheight: fixed_t): fixed_t;

function P_FindShortestUpperAround(secnum: integer): fixed_t;

function P_CanUnlockGenDoor(line: Pline_t; player: Pplayer_t): boolean;

procedure T_Friction(f: Pfriction_t);

procedure T_Pusher(p: Ppusher_t);

type
  linefunc_t = function (line: Pline_t): integer;

const
  MORE_FRICTION_MOMENTUM = 15000; // mud factor based on momentum
  ORIG_FRICTION = $E800;          // original value
  ORIG_FRICTION_FACTOR = 2048;    // original value
  CROUCH_FRICTION_FACTOR = 1536;  // JVAL: 20211101 - Crouch

procedure P_SpawnFriction;

procedure P_SpawnPushers;

var
  levelTimer: boolean;
  levelTimeCount: integer;

function P_GetPushThing(const snum: integer): Pmobj_t;

implementation

uses
  d_delphi,
  deh_main,
  doomdata,
  d_englsh,
  i_system,
  hu_stuff,
  z_zone,
  m_argv,
  m_rnd,
  m_bbox,
  w_wad,
  r_data,
  r_main,
  {$IFNDEF OPENGL}
  r_ripple,
  {$ENDIF}
  info_h,
  tables,
  g_game,
  p_setup,
  p_inter,
  p_switch,
  p_ceilng,
  p_plats,
  p_lights,
  p_sight,
  p_doors,
  p_floor,
  p_telept,
  p_genlin,
  p_map,
  p_maputl,
  p_dialog,
  p_enemy,
  p_mobj,
  p_user,
  p_scroll,
  p_common,
  p_tick,
  s_sound,
// Data.
  sounds;

//
// Animating textures and planes
//
type
  anim_t = record
    istexture: boolean;
    picnum: integer;
    basepic: integer;
    numpics: integer;
    speed: integer;
  end;
  Panim_t = ^anim_t;
  anim_tArray = array[0..$FFFF] of anim_t;
  Panim_tArray = ^anim_tArray;

//
//      source animation definition
//
  animdef_t = record
    istexture: boolean; // if false, it is a flat
    endname: string[8];
    startname: string[8];
    speed: integer;
  end;
  Panimdef_t = ^animdef_t;
  animdef_tArray = array[0..$FFFF] of animdef_t;
  Panimdef_tArray = ^animdef_tArray;


  // for ANIMATED lump (BOOM compatibility)
  wad_animdef_t = packed record
    istexture: byte; // if 0, it is a flat
    endname: array[0..8] of char;
    startname: array[0..8] of char;
    speed: integer;
  end;
  Pwad_animdef_t = ^wad_animdef_t;
  wad_animdef_tArray = array[0..$FFFF] of wad_animdef_t;
  Pwad_animdef_tArray = ^wad_animdef_tArray;

const
  MAXANIMS = 50;
  NUMFIXEDANIMDEFS = 33;

//
// P_InitPicAnims
//

// Floor/ceiling animation sequences,
//  defined by first and last frame,
//  i.e. the flat (64x64 tile) name to
//  be used.
// The full animation sequence is given
//  using all the flats between the start
//  and end entry, in the order found in
//  the WAD file.
//
  fixedanimdefs: array[0..NUMFIXEDANIMDEFS] of animdef_t = (
    (istexture: false; endname: 'F_SCANR8';  startname: 'F_SCANR5';  speed: 4),
    (istexture: false; endname: 'F_WATR03';  startname: 'F_WATR01';  speed: 8),
    (istexture: false; endname: 'F_PWATR3';  startname: 'F_PWATR1';  speed: 11),
    (istexture: false; endname: 'F_SCANR4';  startname: 'F_SCANR1';  speed: 4),
    (istexture: true;  endname: 'SCAN08';    startname: 'SCAN05';    speed: 4),
    (istexture: true;  endname: 'SWTRMG03';  startname: 'SWTRMG01';  speed: 4),
    (istexture: true;  endname: 'SCAN04';    startname: 'SCAN01';    speed: 4),
    (istexture: true;  endname: 'COMP04';    startname: 'COMP01';    speed: 4),
    (istexture: true;  endname: 'COMP08';    startname: 'COMP05';    speed: 6),
    (istexture: true;  endname: 'COMP12';    startname: 'COMP09';    speed: 11),
    (istexture: true;  endname: 'COMP16';    startname: 'COMP13';    speed: 12),
    (istexture: true;  endname: 'COMP20';    startname: 'COMP17';    speed: 12),
    (istexture: true;  endname: 'COMP24';    startname: 'COMP21';    speed: 12),
    (istexture: true;  endname: 'COMP28';    startname: 'COMP25';    speed: 12),
    (istexture: true;  endname: 'COMP32';    startname: 'COMP29';    speed: 12),
    (istexture: true;  endname: 'COMP37';    startname: 'COMP33';    speed: 12),
    (istexture: true;  endname: 'COMP41';    startname: 'COMP38';    speed: 12),
    (istexture: true;  endname: 'COMP49';    startname: 'COMP42';    speed: 10),
    (istexture: true;  endname: 'BRKGRY16';  startname: 'BRKGRY13';  speed: 10),
    (istexture: true;  endname: 'BRNSCN04';  startname: 'BRNSCN01';  speed: 10),
    (istexture: true;  endname: 'CONCRT12';  startname: 'CONCRT09';  speed: 11),
    (istexture: true;  endname: 'CONCRT25';  startname: 'CONCRT22';  speed: 11),
    (istexture: true;  endname: 'WALPMP02';  startname: 'WALPMP01';  speed: 16),
    (istexture: true;  endname: 'WALTEK17';  startname: 'WALTEK16';  speed: 8),
    (istexture: true;  endname: 'FORCE04';   startname: 'FORCE01';   speed: 4),
    (istexture: true;  endname: 'FORCE08';   startname: 'FORCE05';   speed: 4),
    (istexture: true;  endname: 'FAN02';     startname: 'FAN01';     speed: 4),
    (istexture: false; endname: 'F_VWATR3';  startname: 'P_VWATR1';  speed: 4),
    (istexture: false; endname: 'F_HWATR3';  startname: 'F_HWATR1';  speed: 4),
    (istexture: false; endname: 'F_TELE2';   startname: 'F_TELE1';   speed: 4),
    (istexture: false; endname: 'F_FAN2';    startname: 'F_FAN1';    speed: 4),
    (istexture: false; endname: 'F_CONVY2';  startname: 'F_CONVY1';  speed: 4),
    (istexture: false; endname: 'F_RDALN4';  startname: 'F_RDALN1';  speed: 4),
    (istexture: false; endname: '';          startname: '';          speed: 0)
  );

var
  animdefs: Panimdef_tArray;
  anims: Panim_tArray;
  lastanim: integer;

const
//
//      Animating line specials
//
  MAXLINEANIMS = 1024; // JVAL Originally was 64

procedure P_InitPicAnims;
var
  i: integer;
  lump: integer;
  len: integer;
  wadanims: Pwad_animdef_tArray;
  lanim: Panim_t;
  ianimdef: Panimdef_t;
begin
// JVAL
// Check for 'ANIMATED' lump
  animdefs := nil;
  lump := W_CheckNumForName('ANIMATED');
  if lump >= 0 then
  begin
    len := W_LumpLength(lump);
    len := len div SizeOf(wad_animdef_t);
    wadanims := W_CacheLumpNum(lump, PU_STATIC);
    animdefs := Z_Malloc((len + 1) * SizeOf(animdef_t), PU_STATIC, nil);
    anims := Z_Malloc((len + 1) * SizeOf(anim_t), PU_STATIC, nil);
    ianimdef := @animdefs[0];
    for i := 0 to len - 1 do
    begin
      ianimdef.istexture := wadanims[i].istexture <> 0;
      ianimdef.endname := wadanims[i].endname;
      ianimdef.startname := wadanims[i].startname;
      ianimdef.speed := wadanims[i].speed;
      inc(ianimdef);
    end;
    Z_Free(wadanims);
    ianimdef.istexture := false;
    ianimdef.endname := '';
    ianimdef.startname := '';
    ianimdef.speed := 0;
  end;
  if animdefs = nil then
  begin
    animdefs := Z_Malloc(SizeOf(fixedanimdefs), PU_STATIC, nil);
    anims := Z_Malloc((NUMFIXEDANIMDEFS + 1) * SizeOf(anim_t), PU_STATIC, nil);
    memcpy(animdefs, @fixedanimdefs, SizeOf(fixedanimdefs));
  end;

  //  Init animation
  lastanim := 0;

  ianimdef := @animdefs[0];
  while ianimdef.speed <> 0 do
  begin
    lanim := @anims[lastanim];

    if ianimdef.istexture then
    begin
      // different episode ?
      if R_CheckTextureNumForName(ianimdef.startname) = -1 then
      begin
        inc(ianimdef);
        continue;
      end;

      lanim.picnum := R_TextureNumForName(ianimdef.endname);
      lanim.basepic := R_TextureNumForName(ianimdef.startname);
      lanim.istexture := true;
      lanim.numpics := lanim.picnum - lanim.basepic + 1;
    end
    else
    begin
      if W_CheckNumForName(ianimdef.startname) = -1 then
      begin
        inc(ianimdef);
        continue;
      end;

      lanim.picnum := R_FlatNumForName(ianimdef.endname);
      lanim.basepic := R_FlatNumForName(ianimdef.startname);
      lanim.istexture := false;
      lanim.numpics := flats[lanim.picnum].lump - flats[lanim.basepic].lump + 1;
      // JVAL
      // Create new flats as nessesary
      for i := flats[lanim.basepic].lump to flats[lanim.picnum].lump do
        R_NewFlatNumForLump(i);
    end;

    if lanim.numpics < 2 then
      I_Error('P_InitPicAnims(): bad cycle from %s to %s',
        [ianimdef.startname, ianimdef.endname]);

    lanim.speed := ianimdef.speed;
    inc(lastanim);
    inc(ianimdef);
  end;

  Z_Free(animdefs);
end;

//
// UTILITIES
//



//
// getSide()
// Will return a side_t*
//  given the number of the current sector,
//  the line number, and the side (0/1) that you want.
//
function getSide(currentSector: integer; line: integer; side: integer): Pside_t;
begin
  result := @sides[(sectors[currentSector].lines[line]).sidenum[side]];
end;

//
// getSector()
// Will return a sector_t*
//  given the number of the current sector,
//  the line number and the side (0/1) that you want.
//
function getSector(currentSector: integer; line: integer; side: integer): Psector_t;
var
  sidenum: integer;
begin
  sidenum := (sectors[currentSector].lines[line]).sidenum[side];
  if sidenum >= 0 then
    result := sides[sidenum].sector
  else
    result := nil;
end;

//
// twoSided()
// Given the sector number and the line number,
//  it will tell you whether the line is two-sided or not.
//
function twoSided(sector: integer; line: integer): boolean;
begin
  result := sectors[sector].lines[line].sidenum[1] <> 0;
end;

function twoSidedS(sector: Psector_t; line: integer): boolean;
begin
  result := sector.lines[line].sidenum[1] <> 0;
end;

//
// getNextSector()
// Return sector_t * of sector next to current.
// NULL if not two-sided line
//
function getNextSector(line: Pline_t; sec: Psector_t): Psector_t;
begin
  if G_NeedsCompatibilityMode then
  begin
    if (line.flags and ML_TWOSIDED) = 0 then
    begin
      result := nil;
      exit;
    end;
  end;

  if line.frontsector = sec then
  begin
    if G_NeedsCompatibilityMode or (line.backsector <> sec) then
      result := line.backsector
    else
      result := nil;
    exit;
  end;

  result := line.frontsector;
end;

//
// P_FindLowestFloorSurrounding()
// FIND LOWEST FLOOR HEIGHT IN SURROUNDING SECTORS
//
function P_FindLowestFloorSurrounding(sec: Psector_t): fixed_t;
var
  i: integer;
  check: Pline_t;
  other: Psector_t;
begin
  result := sec.floorheight;

  for i := 0 to sec.linecount - 1 do
  begin
    check := sec.lines[i];
    other := getNextSector(check, sec);

    if other <> nil then
      if other.floorheight < result then
        result := other.floorheight;
  end;
end;

//
// P_FindHighestFloorSurrounding()
// FIND HIGHEST FLOOR HEIGHT IN SURROUNDING SECTORS
//
function P_FindHighestFloorSurrounding(sec: Psector_t): fixed_t;
var
  i: integer;
  check: Pline_t;
  other: Psector_t;
begin
  result := -32000 * FRACUNIT;

  for i := 0 to sec.linecount - 1 do
  begin
    check := sec.lines[i];
    other := getNextSector(check, sec);

    if other <> nil then
      if other.floorheight > result then
        result := other.floorheight;
  end;
end;

//
// P_FindNextHighestFloor
// FIND NEXT HIGHEST FLOOR IN SURROUNDING SECTORS
// Note: this should be doable w/o a fixed array.

const
  MAX_ADJOINING_SECTORS = 64;

function P_FindNextHighestFloor(sec: Psector_t; currentheight: integer): fixed_t;
var
  i: integer;
  h: integer;
  check: Pline_t;
  other: Psector_t;
  height: fixed_t;
  heightlist: array[0..MAX_ADJOINING_SECTORS] of fixed_t;
  maxsecs: integer;
begin
  maxsecs := MAX_ADJOINING_SECTORS;
  height := currentheight;

  h := 0;
  for i := 0 to sec.linecount - 1 do
  begin
    check := sec.lines[i];
    other := getNextSector(check, sec);

    if other <> nil then
    begin
      if other.floorheight > height then
      begin
        heightlist[h] := other.floorheight;
        inc(h);
      end;

      // Check for overflow. Exit.
      if h >= maxsecs then
      begin
        I_Warning('P_FindNextHighestFloor(): Sector with more than %d adjoining sectors.'#13#10, [maxsecs]);
        break;
      end;
    end;
  end;

  // Find lowest height in list
  if h = 0 then
  begin
    result := currentheight;
    exit;
  end;

  result := heightlist[0];

  // Range checking?
  for i := 1 to h - 1 do
    if heightlist[i] < result then
      result := heightlist[i];
end;

//
// P_FindNextLowestFloor()
//
// Passed a sector and a floor height, returns the fixed point value
// of the largest floor height in a surrounding sector smaller than
// the floor height passed. If no such height exists the floorheight
// passed is returned.
//
// jff 02/03/98 Twiddled Lee's P_FindNextHighestFloor to make this
//
// JVAL BOOM compatibility
//
function P_FindNextLowestFloor(sec: Psector_t; currentheight: fixed_t): fixed_t;
var
  other: Psector_t;
  i: integer;
begin
  i := 0;
  while i < sec.linecount - 1 do
  begin
    other := getNextSector(sec.lines[i], sec);
    if (other <> nil) and (other.floorheight < currentheight) then
    begin
      result := other.floorheight;
      inc(i);
      while i < sec.linecount do
      begin
        other := getNextSector(sec.lines[i], sec);
        if other <> nil then
          if (other.floorheight > result) and (other.floorheight < currentheight) then
            result := other.floorheight;
        inc(i);
      end;
      exit;
    end;
    inc(i);
  end;
  result := currentheight;
end;

//
// P_FindNextLowestCeiling()
//
// Passed a sector and a ceiling height, returns the fixed point value
// of the largest ceiling height in a surrounding sector smaller than
// the ceiling height passed. If no such height exists the ceiling height
// passed is returned.
//
// jff 02/03/98 Twiddled Lee's P_FindNextHighestFloor to make this
//
// JVAL BOOM compatibility
//
function P_FindNextLowestCeiling(sec: Psector_t; currentheight: fixed_t): fixed_t;
var
  other: Psector_t;
  i: integer;
begin
  i := 0;
  while i < sec.linecount - 1 do
  begin
    other := getNextSector(sec.lines[i], sec);
    if (other <> nil) and (other.ceilingheight < currentheight) then
    begin
      result := other.ceilingheight;
      inc(i);
      while i < sec.linecount do
      begin
        other := getNextSector(sec.lines[i], sec);
        if other <> nil then
          if (other.ceilingheight > result) and (other.ceilingheight < currentheight) then
            result := other.ceilingheight;
        inc(i);
      end;
      exit;
    end;
    inc(i);
  end;
  result := currentheight;
end;

//
// P_FindNextHighestCeiling()
//
// Passed a sector and a ceiling height, returns the fixed point value
// of the smallest ceiling height in a surrounding sector larger than
// the ceiling height passed. If no such height exists the ceiling height
// passed is returned.
//
// jff 02/03/98 Twiddled Lee's P_FindNextHighestFloor to make this
//
// JVAL BOOM compatibility
//
function P_FindNextHighestCeiling(sec: Psector_t; currentheight: fixed_t): fixed_t;
var
  other: Psector_t;
  i: integer;
begin
  i := 0;
  while i < sec.linecount - 1 do
  begin
    other := getNextSector(sec.lines[i], sec);
    if (other <> nil) and (other.ceilingheight > currentheight) then
    begin
      result := other.ceilingheight;
      inc(i);
      while i < sec.linecount do
      begin
        other := getNextSector(sec.lines[i], sec);
        if other <> nil then
          if (other.ceilingheight < result) and (other.ceilingheight > currentheight) then
            result := other.ceilingheight;
        inc(i);
      end;
      exit;
    end;
    inc(i);
  end;
  result := currentheight;
end;

//
// FIND LOWEST CEILING IN THE SURROUNDING SECTORS
//
function P_FindLowestCeilingSurrounding(sec: Psector_t): fixed_t;
var
  i: integer;
  check: Pline_t;
  other: Psector_t;
begin
  result := 32000 * FRACUNIT;

  for i := 0 to sec.linecount - 1 do
  begin
    check := sec.lines[i];
    other := getNextSector(check, sec);

    if other <> nil then
      if other.ceilingheight < result then
        result := other.ceilingheight;
  end;
end;

//
// FIND HIGHEST CEILING IN THE SURROUNDING SECTORS
//
function P_FindHighestCeilingSurrounding(sec: Psector_t): fixed_t;
var
  i: integer;
  check: Pline_t;
  other: Psector_t;
begin
  result := -32000 * FRACUNIT;

  for i := 0 to sec.linecount - 1 do
  begin
    check := sec.lines[i];
    other := getNextSector(check, sec);

    if other <> nil then
      if other.ceilingheight > result then
        result := other.ceilingheight;
  end;
end;

//
// P_FindShortestTextureAround()
//
// Passed a sector number, returns the shortest lower texture on a
// linedef bounding the sector.
//
// Note: If no lower texture exists 32000*FRACUNIT is returned.
//       but if compatibility then MAXINT is returned
//
// jff 02/03/98 Add routine to find shortest lower texture
//
// JVAL BOOM compatibility
//
function P_FindShortestTextureAround(secnum: integer): fixed_t;
var
  side: Pside_t;
  i: integer;
  sec: Psector_t;
begin
  sec := @sectors[secnum];

  result := 32000 * FRACUNIT; //jff 3/13/98 prevent overflow in height calcs

  for i := 0 to sec.linecount - 1 do
    if twoSidedS(sec, i) then
    begin
      side := getSide(secnum, i, 0);
      if side.bottomtexture > 0 then  //jff 8/14/98 texture 0 is a placeholder
        if textureheight[side.bottomtexture] < result then
          result := textureheight[side.bottomtexture];
      side := getSide(secnum, i, 1);
      if side.bottomtexture > 0 then  //jff 8/14/98 texture 0 is a placeholder
        if textureheight[side.bottomtexture] < result then
          result := textureheight[side.bottomtexture];
    end;

end;


//
// P_FindShortestUpperAround()
//
// Passed a sector number, returns the shortest upper texture on a
// linedef bounding the sector.
//
// Note: If no upper texture exists 32000*FRACUNIT is returned.
//       but if compatibility then MAXINT is returned
//
// jff 03/20/98 Add routine to find shortest upper texture
//
// JVAL BOOM compatibility
//
function P_FindShortestUpperAround(secnum: integer): fixed_t;
var
  side: Pside_t;
  i: integer;
  sec: Psector_t;
begin
  sec := @sectors[secnum];

  result := 32000 * FRACUNIT; //jff 3/13/98 prevent overflow in height calcs

  for i := 0 to sec.linecount - 1 do
    if twoSidedS(sec, i) then
    begin
      side := getSide(secnum, i, 0);
      if side.toptexture > 0 then  //jff 8/14/98 texture 0 is a placeholder
        if textureheight[side.toptexture] < result then
          result := textureheight[side.toptexture];
      side := getSide(secnum, i, 1);
      if side.toptexture > 0 then  //jff 8/14/98 texture 0 is a placeholder
        if textureheight[side.toptexture] < result then
          result := textureheight[side.toptexture];
    end;

end;

//
// P_FindModelFloorSector()
//
// Passed a floor height and a sector number, return a pointer to a
// a sector with that floor height across the lowest numbered two sided
// line surrounding the sector.
//
// Note: If no sector at that height bounds the sector passed, return NULL
//
// jff 02/03/98 Add routine to find numeric model floor
//  around a sector specified by sector number
// jff 3/14/98 change first parameter to plain height to allow call
//  from routine not using floormove_t
//
// JVAL BOOM compatibility
//
function P_FindModelFloorSector(floordestheight: fixed_t; secnum: integer): Psector_t;
var
  i: integer;
  linecount: integer;

  function _getLcount(const sec: Psector_t): integer;
  begin
    if G_NeedsCompatibilityMode and (sec.linecount < linecount) then
      result := sec.linecount
    else
      result := linecount;
  end;

begin
  result := @sectors[secnum]; //jff 3/2/98 woops! better do this
  //jff 5/23/98 don't disturb sec->linecount while searching
  // but allow early exit in old demos
  linecount := result.linecount;
  i := 0;
  while i < _getLcount(result) do
  begin
    if twoSided(secnum, i) then
    begin
      if pDiff(getSide(secnum, i, 0).sector, sectors, SizeOf(sector_t)) = secnum then
        result := getSector(secnum, i, 1)
      else
        result := getSector(secnum, i, 0);

      if result <> nil then
        if result.floorheight = floordestheight then
          exit;
    end;
    inc(i);
  end;
  result := nil;
end;

//
// P_FindModelCeilingSector()
//
// Passed a ceiling height and a sector number, return a pointer to a
// a sector with that ceiling height across the lowest numbered two sided
// line surrounding the sector.
//
// Note: If no sector at that height bounds the sector passed, return NULL
//
// jff 02/03/98 Add routine to find numeric model ceiling
//  around a sector specified by sector number
//  used only from generalized ceiling types
// jff 3/14/98 change first parameter to plain height to allow call
//  from routine not using ceiling_t
//
// JVAL BOOM compatibility
//
function P_FindModelCeilingSector(ceildestheight: fixed_t; secnum: integer): Psector_t;
var
  i: integer;
  linecount: integer;

  function _getLcount(const sec: Psector_t): integer;
  begin
    if G_NeedsCompatibilityMode and (sec.linecount < linecount) then
      result := sec.linecount
    else
      result := linecount;
  end;

begin
  result := @sectors[secnum]; //jff 3/2/98 woops! better do this
  //jff 5/23/98 don't disturb sec->linecount while searching
  // but allow early exit in old demos
  linecount := result.linecount;
  i := 0;
  while i < _getLcount(result) do
  begin
    if twoSided(secnum, i) then
    begin
      if pDiff(getSide(secnum, i, 0).sector, sectors, SizeOf(sector_t)) = secnum then
        result := getSector(secnum, i, 1)
      else
        result := getSector(secnum, i, 0);

      if result <> nil then
        if result.ceilingheight = ceildestheight then
          exit;
    end;
    inc(i);
  end;
  result := nil;
end;

//
// RETURN NEXT SECTOR # THAT LINE TAG REFERS TO
//
function P_FindSectorFromLineTag(line: Pline_t; start: integer): integer;
var
  i: integer;
begin
  for i := start + 1 to numsectors - 1 do
    if sectors[i].tag = line.tag then
    begin
      result := i;
      exit;
    end;

  result := -1;
end;

function P_FindSectorFromLineTag2(line: Pline_t; var start: integer): integer;
var
  i: integer;
begin
  for i := start + 1 to numsectors - 1 do
    if sectors[i].tag = line.tag then
    begin
      result := i;
      start := result;
      exit;
    end;

  result := -1;
  start := -1;
end;

//
// killough 4/16/98: Same thing, only for linedefs
//
// JVAL BOOM compatibility
//
function P_FindLineFromLineTag(line: Pline_t; start: integer): integer;
var
  i: integer;
begin
  for i := start + 1 to numlines - 1 do
    if lines[i].tag = line.tag then
    begin
      result := i;
      exit;
    end;

  result := -1;
end;

function P_FindLineFromLineTag2(line: Pline_t; var start: integer): integer;
var
  i: integer;
begin
  for i := start + 1 to numlines - 1 do
    if lines[i].tag = line.tag then
    begin
      result := i;
      start := result;
      exit;
    end;

  result := -1;
  start := -1;
end;

//
// Find minimum light from an adjacent sector
//
function P_FindMinSurroundingLight(sector: Psector_t; max: integer): integer;
var
  i: integer;
  line: Pline_t;
  check: Psector_t;
begin
  result := max;
  for i := 0 to sector.linecount - 1 do
  begin
    line := sector.lines[i];
    check := getNextSector(line, sector);

    if check <> nil then
      if check.lightlevel < result then
        result := check.lightlevel;
  end;
end;

//
// P_CanUnlockGenDoor
//
// Passed a generalized locked door linedef and a player, returns whether
// the player has the keys necessary to unlock that door.
//
// Note: The linedef passed MUST be a generalized locked door type
//       or results are undefined.
//
// jff 02/05/98 routine added to test for unlockability of
//  generalized locked doors
//
// JVAL: Only AnyKey or AllKeys in Strife :(
function P_CanUnlockGenDoor(line: Pline_t; player: Pplayer_t): boolean;
begin

  // determine for each of lock type if player's keys are adequate
  case (line.special and LockedKey) shr LockedKeyShift of

    Ord(AnyKey):
      begin
        if not player.cards[Ord(key_BaseKey)] and
           not player.cards[Ord(key_BlueCrystalKey)] and
           not player.cards[Ord(key_BrassKey)] and
           not player.cards[Ord(key_CatacombKey)] and
           not player.cards[Ord(key_ChapelKey)] and
           not player.cards[Ord(key_CoreKey)] and
           not player.cards[Ord(key_FactoryKey)] and
           not player.cards[Ord(key_GoldKey)] and
           not player.cards[Ord(key_GovsKey)] and
           not player.cards[Ord(key_IDBadge)] and
           not player.cards[Ord(key_IDCard)] and
           not player.cards[Ord(key_MaulerKey)] and
           not player.cards[Ord(key_MilitaryID)] and
           not player.cards[Ord(key_MineKey)] and
           not player.cards[Ord(key_NewKey5)] and
           not player.cards[Ord(key_OracleKey)] and
           not player.cards[Ord(key_OrderKey)] and
           not player.cards[Ord(key_Passcard)] and
           not player.cards[Ord(key_Power1Key)] and
           not player.cards[Ord(key_Power2Key)] and
           not player.cards[Ord(key_Power3Key)] and
           not player.cards[Ord(key_PrisonKey)] and
           not player.cards[Ord(key_RedCrystalKey)] and
           not player.cards[Ord(key_SecurityKey)] and
           not player.cards[Ord(key_SeveredHand)] and
           not player.cards[Ord(key_SilverKey)] and
           not player.cards[Ord(key_WarehouseKey)] then
        begin
          player._message := PD_ANY; // Ty 03/27/98 - externalized
          S_StartSound(player.mo, Ord(sfx_oof));             // killough 3/20/98
          result := false;
          exit;
        end;
      end;

    Ord(AllKeys):
      begin
        if not player.cards[Ord(key_BaseKey)] or
           not player.cards[Ord(key_BlueCrystalKey)] or
           not player.cards[Ord(key_BrassKey)] or
           not player.cards[Ord(key_CatacombKey)] or
           not player.cards[Ord(key_ChapelKey)] or
           not player.cards[Ord(key_CoreKey)] or
           not player.cards[Ord(key_FactoryKey)] or
           not player.cards[Ord(key_GoldKey)] or
           not player.cards[Ord(key_GovsKey)] or
           not player.cards[Ord(key_IDBadge)] or
           not player.cards[Ord(key_IDCard)] or
           not player.cards[Ord(key_MaulerKey)] or
           not player.cards[Ord(key_MilitaryID)] or
           not player.cards[Ord(key_MineKey)] or
           not player.cards[Ord(key_NewKey5)] or
           not player.cards[Ord(key_OracleKey)] or
           not player.cards[Ord(key_OrderKey)] or
           not player.cards[Ord(key_Passcard)] or
           not player.cards[Ord(key_Power1Key)] or
           not player.cards[Ord(key_Power2Key)] or
           not player.cards[Ord(key_Power3Key)] or
           not player.cards[Ord(key_PrisonKey)] or
           not player.cards[Ord(key_RedCrystalKey)] or
           not player.cards[Ord(key_SecurityKey)] or
           not player.cards[Ord(key_SeveredHand)] or
           not player.cards[Ord(key_SilverKey)] or
           not player.cards[Ord(key_WarehouseKey)] then
        begin
          player._message := PD_ALL6; // Ty 03/27/98 - externalized
          S_StartSound(player.mo, Ord(sfx_oof));             // killough 3/20/98
          result := false;
          exit;
        end;
      end;
  end;

  result := true;
end;


//
// P_IsSecret()
//
// Passed a sector, returns if the sector secret type is still active, i.e.
// secret type is set and the secret has not yet been obtained.
//
// jff 3/14/98 added to simplify checks for whether sector is secret
//  in automap and other places
//
function P_IsSecret(sec: Psector_t): boolean;
begin
  result := (sec.special = 9) or (sec.special and SECRET_MASK <> 0);
end;


//
// P_WasSecret()
//
// Passed a sector, returns if the sector secret type is was active, i.e.
// secret type was set and the secret has been obtained already.
//
// jff 3/14/98 added to simplify checks for whether sector is secret
//  in automap and other places
//
function P_WasSecret(sec: Psector_t): boolean;
begin
  result := (sec.oldspecial = 9) or (sec.oldspecial and SECRET_MASK <> 0);
end;


//
// EVENTS
// Events are operations triggered by using, crossing,
// or shooting special lines, or by timed thinkers.
//

var
  crosslinestr: string;
//
// P_CrossSpecialLine - TRIGGER
// Called every time a thing origin is about
//  to cross a line with a non 0 special.
//
procedure P_CrossSpecialLine(linenum: integer; side: integer; thing: Pmobj_t);
begin
  P_CrossSpecialLinePtr(@lines[linenum], side, thing);
end;

procedure P_CrossSpecialLinePtr(line: Pline_t; side: integer; thing: Pmobj_t);
var
  linefunc: linefunc_t;
  ok: boolean;
  map, spot: integer;
  sidedef: Pside_t;
  flag: LongWord;
begin
  // haleyjd 09/21/10: corpses and missiles cannot activate any cross-over
  // line types, *except* 182 (which is for the sake of missiles).
  if (thing.flags and (MF_MISSILE or MF_CORPSE) <> 0) and (line.special <> 182) then
    exit;

  //  Triggers that other things can activate
  if thing.player = nil then
  begin
    ok := false;

    // [STRIFE] Added several line types. Removed none.
    case line.special of
      97,        // TELEPORT RETRIGGER
      185,       // haleyjd: [STRIFE] Silent Teleport (used for Converter)
      195,       // haleyjd: [STRIFE] Silent Teleport and Change Zombie
      231,       // haleyjd: [STRIFE] WR Teleport (Silent at Source)
      125,       // TELEPORT MONSTERONLY TRIGGER
      126,       // TELEPORT MONSTERONLY RETRIGGER
      182,       // haleyjd: [STRIFE] Break glass - it's a W1 type too!
      10,        // PLAT DOWN-WAIT-UP-STAY TRIGGER
      39,        // TELEPORT TRIGGER
      88,        // PLAT DOWN-WAIT-UP-STAY RETRIGGER
      4:         // RAISE DOOR
         ok := true;
    end;
    if not ok then
      exit;
  end;

  // pointer to line function is nil by default, set non-null if
  // line special is walkover generalized linedef type
  linefunc := nil;

  // check each range of generalized linedefs
  if word(line.special) >= CGENFLOORBASE then
  begin
    if thing.player = nil then
      if (line.special and gen_FloorChange <> 0) or (line.special and gen_FloorModel = 0) then
        exit;     // FloorModel is 'Allow Monsters' if FloorChange is 0
    if line.tag = 0 then //jff 2/27/98 all walk generalized types require tag
      exit;
    linefunc := @EV_DoGenFloor;
  end
  else if word(line.special) >= CGENCEILINGBASE then
  begin
    if thing.player = nil then
      if (line.special and CeilingChange <> 0) or (line.special and CeilingModel = 0) then
        exit;     // CeilingModel is 'Allow Monsters' if CeilingChange is 0
    if line.tag = 0 then //jff 2/27/98 all walk generalized types require tag
      exit;
    linefunc := @EV_DoGenCeiling;
  end
  else if word(line.special) >= CGENDOORBASE then
  begin
    if thing.player = nil then
    begin
      if line.special and DoorMonster = 0 then
        exit;                    // monsters disallowed from this door
      if line.flags and ML_SECRET <> 0 then // they can't open secret doors either
        exit;
    end;
    if line.tag = 0 then //3/2/98 move outside the monster check
      exit;
    linefunc := @EV_DoGenDoor;
  end
  else if word(line.special) >= CGENLOCKEDBASE then
  begin
    if thing.player = nil then
      exit;                     // monsters disallowed from unlocking doors
    if (line.special and TriggerType = Ord(WalkOnce)) or (line.special and TriggerType = Ord(WalkMany)) then
    begin //jff 4/1/98 check for being a walk type before reporting door type
      if not P_CanUnlockGenDoor(line, thing.player) then
        exit;
    end
    else
      exit;
    linefunc := @EV_DoGenLockedDoor;
  end
  else if word(line.special) >= CGENLIFTBASE then
  begin
    if thing.player = nil then
      if line.special and LiftMonster = 0 then
        exit; // monsters disallowed
    if line.tag = 0 then //jff 2/27/98 all walk generalized types require tag
      exit;
    linefunc := @EV_DoGenLift;
  end
  else if word(line.special) >= CGENSTAIRSBASE then
  begin
    if thing.player = nil then
      if line.special and StairMonster = 0 then
        exit; // monsters disallowed
    if line.tag = 0 then //jff 2/27/98 all walk generalized types require tag
      exit;
    linefunc := @EV_DoGenStairs;
  end;

  if Assigned(linefunc) then // if it was a valid generalized type
  begin
    case (line.special and TriggerType) shr TriggerTypeShift of
      Ord(WalkOnce):
        begin
          if linefunc(line) <> 0 then
            line.special := 0;    // clear special if a walk once type
        end;
      Ord(WalkMany):
        begin
          linefunc(line);
        end;
    end;
    exit;
  end;

  // Note: could use some const's here.
  case line.special of
    //
    // TRIGGERS.
    // All from here to RETRIGGERS.
    //
    230:
      begin
        // haleyjd 09/21/10: [STRIFE] W1 Open Door if Quest
        sidedef := @sides[line.sidenum[0]];
        flag := (sidedef.rowoffset div FRACUNIT) - 1;

        if Pplayer_t(thing.player).questflags and _SHLW(1, flag) <> 0 then
        begin
          EV_DoDoor(line, vld_open);
          line.special := 0;
        end;
      end;

    // fall-through:
    2:
      begin
        // Open Door - [STRIFE] Verified unmodified.
        EV_DoDoor(line, vld_open);
        line.special := 0;
      end;

    227:
      begin
        // haleyjd 09/21/10: [STRIFE] W1 Close Door if Quest
        sidedef := @sides[line.sidenum[0]];
        flag := (sidedef.rowoffset div FRACUNIT) - 1;

        if Pplayer_t(thing.player).questflags and _SHLW(1, flag) <> 0 then
        begin
          EV_DoDoor(line, vld_close);
          line.special := 0;
        end;
      end;

    // fall-through:
    3:
      begin
        // Close Door - [STRIFE] Verified unmodified.
        EV_DoDoor(line, vld_close);
        line.special := 0;
      end;

    4:
      begin
        // Raise Door - [STRIFE] Verified unmodified.
        EV_DoDoor(line, vld_normal);
        line.special := 0;
      end;

    5:
      begin
        // Raise Floor - [STRIFE] Verified unmodified.
        EV_DoFloor(line, raiseFloor);
        line.special := 0;
      end;

    6:
      begin
        // Fast Ceiling Crush & Raise - [STRIFE] Verified unmodified.
        EV_DoCeiling(line, fastCrushAndRaise);
        line.special := 0;
      end;

    8:
      begin
        // Build Stairs - [STRIFE] Verified unmodified.
        EV_BuildStairs(line, build8);
        line.special := 0;
      end;

    10:
      begin
        // PlatDownWaitUp - [STRIFE] Verified unmodified.
        EV_DoPlat(line, downWaitUpStay, 0);
        line.special := 0;
      end;

    12:
      begin
        // Light Turn On - brightest near - [STRIFE] Verified unmodified.
        EV_LightTurnOn(line, 0);
        line.special := 0;
      end;

    13:
      begin
        // Light Turn On 255 - [STRIFE] Verified unmodified.
        EV_LightTurnOn(line, 255);
        line.special := 0;
      end;

    16:
      begin
        // Close Door 30 - [STRIFE] Verified unmodified.
        EV_DoDoor(line, vld_close30ThenOpen);
        line.special := 0;
      end;

    17:
      begin
        // Start Light Strobing - [STRIFE] Verified unmodified.
        EV_StartLightStrobing(line);
        line.special := 0;
      end;

    19:
      begin
        // Lower Floor - [STRIFE] Verified unmodified.
        EV_DoFloor(line, lowerFloor);
        line.special := 0;
      end;

    22:
      begin
        // villsa [STRIFE] Verified unmodified.
        // Raise floor to nearest height and change texture
        EV_DoPlat(line, raiseToNearestAndChange, 0);
        line.special := 0;
      end;

    25:
      begin
        // Ceiling Crush and Raise - [STRIFE] Verified unmodified.
        EV_DoCeiling(line, crushAndRaise);
        line.special := 0;
      end;

    30:
      begin
        // Raise floor to shortest texture height - [STRIFE] Verified unmodified.
        //  on either side of lines.
        EV_DoFloor(line, raiseToTexture);
        line.special := 0;
      end;

    35:
      begin
        // Lights Very Dark - [STRIFE] Verified unmodified.
        EV_LightTurnOn(line, 35);
        line.special := 0;
      end;

    36:
      begin
        // Lower Floor (TURBO) - [STRIFE] Verified unmodified.
        EV_DoFloor(line, turboLower);
        line.special := 0;
      end;

    37:
      begin
        // LowerAndChange - [STRIFE] Verified unmodified.
        EV_DoFloor(line, lowerAndChange);
        line.special := 0;
      end;

    193:
      begin
        // haleyjd 09/21/10: [STRIFE] W1 Floor Lower to Lowest if Quest
        sidedef := @sides[line.sidenum[0]];
        flag := (sidedef.rowoffset div FRACUNIT) - 1; // note is fixed_t

        // must have the questflag indicated in the line's y offset
        if Pplayer_t(thing.player).questflags and _SHLW(1, flag) <> 0 then
        begin
          EV_DoFloor(line, lowerFloorToLowest);
          line.special := 0;
        end;
      end;

    // fall-through:
    38:
      begin
        // Lower Floor To Lowest - [STRIFE] Verified unmodified.
        EV_DoFloor(line, lowerFloorToLowest);
        line.special := 0;
      end;

    39:
      begin
        // TELEPORT! - [STRIFE] Verified unmodified (except for 0 flags param)
        EV_Teleport(line, side, thing, TF_NORMAL);
        line.special := 0;
      end;

    44:
      begin
        // Ceiling Crush - [STRIFE] Verified unmodified.
        EV_DoCeiling(line, lowerAndCrush);
        line.special := 0;
      end;

    52:
      begin
        // EXIT! - haleyjd 09/21/10: [STRIFE] Exit to level tag/100
        G_ExitLevel(line.tag div 100);
      end;

    53:
      begin
        // Perpetual Platform Raise - [STRIFE] Verified unmodified.
        EV_DoPlat(line, perpetualRaise, 0);
        line.special := 0;
      end;

    54:
      begin
        // Platform Stop - [STRIFE] Verified unmodified.
        EV_StopPlat(line);
        line.special := 0;
      end;

    56:
      begin
        // Raise Floor Crush - [STRIFE] Verified unmodified.
        EV_DoFloor(line, raiseFloorCrush);
        line.special := 0;
      end;

    57:
      begin
        // Ceiling Crush Stop - [STRIFE] Verified unmodified.
        EV_CeilingCrushStop(line);
        line.special := 0;
      end;

    58:
      begin
        // [STRIFE] raiseFloor24 was modified into raiseFloor64
        // Raise Floor 64
        EV_DoFloor(line, raiseFloor64);
        line.special := 0;
      end;

    59:
      begin
        // Raise Floor 24 And Change - [STRIFE] Verified unmodified.
        EV_DoFloor(line, raiseFloor24AndChange);
        line.special := 0;
      end;

    104:
      begin
        // Turn lights off in sector(tag) - [STRIFE] Verified unmodified.
        EV_TurnTagLightsOff(line);
        line.special := 0;
      end;

    108:
      begin
        // Blazing Door Raise (faster than TURBO!) - [STRIFE] Verified unmodified.
        EV_DoDoor(line, vld_blazeRaise);
        line.special := 0;
      end;

    109:
      begin
        // Blazing Door Open (faster than TURBO!) - [STRIFE] Verified unmodified.
        EV_DoDoor(line, vld_blazeOpen);
        line.special := 0;
      end;

    100:
      begin
        // Build Stairs Turbo 16 - [STRIFE] Verified unmodified.
        EV_BuildStairs(line, turbo16);
        line.special := 0;
      end;

    197:
      begin
        // haleyjd 09/21/10: [STRIFE] Blazing Door Close if Has Sigil B
        if Pplayer_t(thing.player).sigiltype > 0 then
        begin
          EV_DoDoor(line, vld_blazeClose);
          line.special := 0;
        end;
      end;

    // fall-through:
    110:
      begin
        // Blazing Door Close (faster than TURBO!) - [STRIFE] Verified unmodified.
        EV_DoDoor(line, vld_blazeClose);
        line.special := 0;
      end;

    119:
      begin
        // Raise floor to nearest surr. floor - [STRIFE] Verified unmodified.
        EV_DoFloor(line, raiseFloorToNearest);
        line.special := 0;
      end;

    121:
      begin
        // villsa [STRIFE] Verified unmodified.
        // Blazing PlatDownWaitUpStay
        EV_DoPlat(line, blazeDWUS, 0);
        line.special := 0;
      end;

    124:
      begin
        // haleyjd 09/21/10: [STRIFE] W1 Start Finale
        // Altered from G_SecretExitLevel.
        G_StartFinale();
      end;

    125:
      begin
        // TELEPORT MonsterONLY - [STRIFE] Verified unmodified
        //    (except for 0 flags parameter)
        if thing.player = nil then
        begin
          EV_Teleport(line, side, thing, TF_NORMAL);
          line.special := 0;
        end;
      end;

    130:
      begin
        // Raise Floor Turbo - [STRIFE] Verified unmodified.
        EV_DoFloor(line, raiseFloorTurbo);
        line.special := 0;
      end;

    141:
      begin
        // Silent Ceiling Crush & Raise - [STRIFE] Verified unmodified.
        EV_DoCeiling(line, silentCrushAndRaise);
        line.special := 0;
      end;

    174:
      begin
        // villsa [STRIFE] Split Open
        EV_DoDoor(line, vld_splitOpen);
        line.special := 0;
      end;

    183:
      begin
        // villsa [STRIFE] Split Raise Nearest
        EV_DoDoor(line, vld_splitRaiseNearest);
        line.special := 0;
      end;

    178:
      begin
        // haleyjd 09/24/10: [STRIFE] W1 Build Stairs Down 16
        EV_BuildStairs(line, buildDown16);
        line.special := 0;
      end;

    179:
      begin
        // haleyjd 09/25/10: [STRIFE] W1 Ceiling Lower to Floor
        EV_DoCeiling(line, lowerToFloor);
        line.special := 0;
      end;

    182:
      begin
        // haleyjd 09/21/10: [STRIFE] Break Glass
        // 182 is a unique linetype in that it is both a G1 and a W1 linetype,
        // but only missiles may activate it as a W1 type.
        if thing.flags and MF_MISSILE <> 0 then
          P_ChangeSwitchTexture(line, true); // why 1? it will be cleared anyway.
      end;

    187:
      begin
        // haleyjd 09/21/10: [STRIFE] W1 Clear Force Fields if Quest
        sidedef := @sides[line.sidenum[0]];
        flag := (sidedef.rowoffset div FRACUNIT) - 1; // note is fixed_t

        // must have the questflag indicated in the line's y offset
        if Pplayer_t(thing.player).questflags and _SHLW(1, flag) <> 0 then
        begin
          // Do it!
          EV_ClearForceFields(line);
          line.special := 0;
        end;
      end;

    188:
      begin
        // haleyjd 09/21/10: [STRIFE] W1 Open Door if Quest 16 (Gate Mechanism
        // Destroyed)
        if Pplayer_t(thing.player).questflags and QF_QUEST16 <> 0 then
        begin
          EV_DoDoor(line, vld_open);
          line.special := 0;
        end;
      end;

    196:
      begin
        // haleyjd 09/26/10: [STRIFE] W1 Floor Lower to Lowest if Sigil Type > 0
        if Pplayer_t(thing.player).sigiltype > 0 then
        begin
            EV_DoFloor(line, lowerFloorToLowest);
            line.special := 0;
        end;
      end;

    200:
      begin
        // haleyjd 09/21/10: [STRIFE] W1 Open Door if Sigil Owned
        if Pplayer_t(thing.player).weaponowned[Ord(wp_sigil)] then
        begin
          EV_DoDoor(line, vld_open);
          line.special := 0;
        end;
      end;

    201,
    202:
      begin
        // haleyjd 09/21/10: [STRIFE] W1 Voiced Objective (First Side Only)
        if (side <> 1) or (line.special = 202) then
          // haleyjd 09/21/10: [STRIFE] W1 Voiced Objective (Tag = VOC/LOG #)
          // must be consoleplayer
          if thing.player = @players[consoleplayer] then
            // must have comm unit
            if Pplayer_t(thing.player).powers[Ord(pw_communicator)] <> 0 then
            begin
              // load voice
              S_StartVoice('VOC' + itoa(line.tag));

              // load objective
              P_GiveObjective('LOG' + itoa(line.tag), 0);

              // Put up a message
              Pplayer_t(thing.player)._message := DEH_GetString('Incoming Message...');

              line.special := 0;
            end;
      end;

    210:
      begin
        // haleyjd 09/21/10: [STRIFE] W1 Voiced Objective if Flamethrower????
        // I don't think this is actually used anywhere o_O
        // must be player 1...
        if thing.player = @players[0] then
          // must have comm unit
          if Pplayer_t(thing.player).powers[Ord(pw_communicator)] <> 0 then
            // must have... the flamethrower?!
            if Pplayer_t(thing.player).weaponowned[Ord(wp_flame)] then
            begin
              // load voice
              S_StartVoice('VOC' + itoa(line.tag));

              // load objective
              P_GiveObjective('LOG' + itoa(line.tag), 0);

              // Put up a message
              Pplayer_t(thing.player)._message := DEH_GetString('Incoming Message from BlackBird...');
              line.special := 0;
            end;
      end;

    212:
      begin
        // haleyjd 09/25/10: [STRIFE] W1 Floor Lower to Lowest if Have Flamethrower
        if Pplayer_t(thing.player).weaponowned[Ord(wp_flame)] then
        begin
            EV_DoFloor(line, lowerFloorToLowest);
            line.special := 0;
        end;
      end;

    215:
      begin
        // haleyjd 09/21/10: [STRIFE] W1 Voiced Objective if Quest (Tag/100, Tag%100)
        // must be player 1...
        if thing.player = @players[0] then
          // must have comm unit
          if Pplayer_t(thing.player).powers[Ord(pw_communicator)] <> 0 then
            if (line.tag = 0) or
               (Pplayer_t(thing.player).questflags and _SHLW(1, line.tag mod 100 - 1) <> 0) then // test for questflag
            begin
              // start voice
              S_StartVoice('VOC' + itoa(line.tag div 100));

              // give objective
              P_GiveObjective('LOG' + itoa(line.tag div 100), 0);

              // Put up a message
              Pplayer_t(thing.player)._message := DEH_GetString('Incoming Message from BlackBird...');
              line.special := 0;
            end;
      end;

    204:
      begin
        // haleyjd 09/21/10: [STRIFE] W1 Change Music (unused!)
        if thing.player = @players[0] then
        begin
          S_ChangeMusic(line.tag, true);
          line.special := 0;
        end;
      end;

    228:
      begin
        // haleyjd 09/21/10: [STRIFE] W1 Entity Voice?
        if Pplayer_t(thing.player).questflags and QF_QUEST24 <> 0 then // Not killed Macil??? // STRIFE-TODO: verify...
        begin
          if Pplayer_t(thing.player).questflags and QF_QUEST28 = 0 then // ????? STRIFE-TODO
            S_StartVoice('VOC128')
          else
            S_StartVoice('VOC130');

          line.special := 0;
        end;
      end;

      //
      // RETRIGGERS.  All from here till end.
      //
    72:
      begin
        // Ceiling Crush - [STRIFE] Verified unmodified.
        EV_DoCeiling(line, lowerAndCrush );
      end;

    73:
      begin
        // Ceiling Crush and Raise - [STRIFE] Verified unmodified.
        EV_DoCeiling(line, crushAndRaise);
      end;

    74:
      begin
        // Ceiling Crush Stop - [STRIFE] Verified unmodified.
        EV_CeilingCrushStop(line);
      end;

    75:
      begin
        // Close Door - [STRIFE] Verified unmodified.
        EV_DoDoor(line, vld_close);
      end;

    76:
      begin
        // Close Door 30 - [STRIFE] Verified unmodified.
        EV_DoDoor(line, vld_close30ThenOpen);
      end;

    77:
      begin
        // Fast Ceiling Crush & Raise - [STRIFE] Verified unmodified.
        EV_DoCeiling(line, fastCrushAndRaise);
      end;

    79:
      begin
        // Lights Very Dark - [STRIFE] Verified unmodified.
        EV_LightTurnOn(line, 35);
      end;

    80:
      begin
        // Light Turn On - brightest near - [STRIFE] Verified unmodified.
        EV_LightTurnOn(line, 0);
      end;

    81:
      begin
        // Light Turn On 255 - [STRIFE] Verified unmodified.
        EV_LightTurnOn(line, 255);
      end;

    82:
      begin
        // Lower Floor To Lowest - [STRIFE] Verified unmodified.
        EV_DoFloor(line, lowerFloorToLowest);
      end;

    83:
      begin
        // Lower Floor - [STRIFE] Verified unmodified.
        EV_DoFloor(line, lowerFloor);
      end;

    84:
      begin
        // LowerAndChange - [STRIFE] Verified unmodified.
        EV_DoFloor(line, lowerAndChange);
      end;

    86:
      begin
        // Open Door - [STRIFE] Verified unmodified.
        EV_DoDoor(line, vld_open);
      end;

    87:
      begin
        // Perpetual Platform Raise - [STRIFE] Verified unmodified.
        EV_DoPlat(line, perpetualRaise, 0);
      end;

    88:
      begin
        // PlatDownWaitUp - [STRIFE] Verified unmodified.
        EV_DoPlat(line, downWaitUpStay, 0);
      end;

    89:
      begin
        // Platform Stop - [STRIFE] Verified unmodified.
        EV_StopPlat(line);
      end;

    216:
      begin
        // haleyjd 09/21/10: [STRIFE] WR Raise Door if Quest
        sidedef := @sides[line.sidenum[0]];
        flag := (sidedef.rowoffset div FRACUNIT) - 1; // note is fixed_t.

        if Pplayer_t(thing.player).questflags and _SHLW(1, flag) <> 0 then
          EV_DoDoor(line, vld_normal);
      end;

    90:
      begin
        // Raise Door - [STRIFE] Verified unmodified.
        EV_DoDoor(line, vld_normal);
      end;

    91:
      begin
        // Raise Floor - [STRIFE] Verified unmodified.
        EV_DoFloor(line, raiseFloor);
      end;

    92:
      begin
        // [STRIFE] raiseFloor24 changed to raiseFloor64
        // Raise Floor 64
        EV_DoFloor(line, raiseFloor64);
      end;

    93:
      begin
        // Raise Floor 24 And Change - [STRIFE] Verified unmodified.
        EV_DoFloor(line, raiseFloor24AndChange);
      end;

    94:
      begin
        // Raise Floor Crush - [STRIFE] Verified unmodified.
        EV_DoFloor(line, raiseFloorCrush);
      end;

    95:
      begin
        // villsa [STRIFE] Verified unmodified.
        // Raise floor to nearest height
        // and change texture.
        EV_DoPlat(line, raiseToNearestAndChange, 0);
      end;

    96:
      begin
        // Raise floor to shortest texture height - [STRIFE] Verified unmodified.
        // on either side of lines.
        EV_DoFloor(line, raiseToTexture);
      end;

    97:
      begin
        // TELEPORT! - [STRIFE] Verified unmodified (except for 0 flags param)
        EV_Teleport(line, side, thing, TF_NORMAL);
      end;

    98:
      begin
        // Lower Floor (TURBO) - [STRIFE] Verified unmodified.
        EV_DoFloor(line, turboLower);
      end;

    105:
      begin
        // Blazing Door Raise (faster than TURBO!) - [STRIFE] Verified unmodified.
        EV_DoDoor(line, vld_blazeRaise);
      end;

    106:
      begin
        // Blazing Door Open (faster than TURBO!) - [STRIFE] Verified unmodified.
        EV_DoDoor(line, vld_blazeOpen);
      end;

    107:
      begin
        // Blazing Door Close (faster than TURBO!) - [STRIFE] Verified unmodified.
        EV_DoDoor(line, vld_blazeClose);
      end;

    120:
      begin
        // villsa [STRIFE] Verified unmodified.
        // Blazing PlatDownWaitUpStay.
        EV_DoPlat(line, blazeDWUS, 0);
      end;

    126:
      begin
        // TELEPORT MonsterONLY. - [STRIFE] Verified unmodified (except for 0 flags param)
        if thing.player = nil then
            EV_Teleport(line, side, thing, TF_NORMAL);
      end;

    128:
      begin
        // Raise To Nearest Floor - [STRIFE] Verified unmodified.
        EV_DoFloor(line, raiseFloorToNearest);
      end;

    129:
      begin
        // Raise Floor Turbo - [STRIFE] Verified unmodified.
        EV_DoFloor(line, raiseFloorTurbo);
      end;

    186,
    145:
      begin
        // haleyjd [STRIFE] Exit Level to Spot, First Side Only
        if (side = 0) or (line.special = 145) then
        begin
          // haleyjd [STRIFE] Exit Level to Spot
          thing.momx := 0;
          thing.momy := 0;
          thing.momz := 0;

          map := line.tag div 100;
          spot := line.tag mod 100;

          if Pplayer_t(thing.player).weaponowned[Ord(wp_sigil)] then
          begin
            if map = 3 then
              map := 30
            else if map = 7 then
              map := 10;
          end;


          Pplayer_t(thing.player)._message := 'Entering' + Copy(mapnames[map - 1], 9, length(mapnames[map - 1]) - 8);

          if netgame and (deathmatch <> 0) then
          begin
            if levelTimer and (levelTimeCount <> 0) then
            begin
              crosslinestr := itoa((levelTimeCount div TICRATE) div 60) + ' min left';
              exit;
            end;

            // raise switch from floor
            EV_DoFloor(line, raiseFloor64);
          end
          else
          begin
            // normal single-player exit

            // BUG: Here is the opening for a flaming player to cross past
            // the exit line and hit a deathmatch switch ;) It's not so much
            // that this is incorrect, as that they forgot to add such a
            // check to the other kind of exit lines too ;)
            if Pplayer_t(thing.player).health <= 0 then
              exit;

            G_RiftExitLevel(map, spot, thing.angle);
          end;
        end;
      end;

    175:
      begin
        // haleyjd 09/21/10: [STRIFE] WR Raise Alarm if < 16 Above Floor
        if thing.z < thing.floorz + 16 * FRACUNIT then
          P_NoiseAlert(Pplayer_t(thing.player).mo, Pplayer_t(thing.player).mo);
      end;

    198,
    150:
      begin
        // haleyjd 09/21/10: [STRIFE] WR Raise Alarm if No Guard Uniform
        if (P_PlayerHasItem(thing.player, MT_QUEST_GUARD_UNIFORM) = 0) or (line.special = 150) then
        begin
          // haleyjd 09/21/10: [STRIFE] WR Raise Alarm
          P_NoiseAlert(Pplayer_t(thing.player).mo, Pplayer_t(thing.player).mo);
        end;
      end;

    208:
      begin
        // haleyjd 09/21/10: [STRIFE] WR Raise Alarm if Have Flamethrower
        // O_o - this is definitely unused. Was an entire flamethrower quest
        // cut out of the game before release?
        if Pplayer_t(thing.player).weaponowned[Ord(wp_flame)] then
          P_NoiseAlert(Pplayer_t(thing.player).mo, Pplayer_t(thing.player).mo);
      end;

    206:
      begin
        // haleyjd 09/21/10: [STRIFE] WR Raise Alarm if Have Chalice
        // This *is* used, inside the Tavern in Tarnhill. Oddly there is also
        // one just randomly placed outside the entrance to the Power Station.
        if P_PlayerHasItem(thing.player, MT_INV_CHALICE) <> 0 then
          P_NoiseAlert(Pplayer_t(thing.player).mo, Pplayer_t(thing.player).mo);
      end;

    184:
      begin
        // villsa [STRIFE] plat up wait down stay
        if EV_DoPlat(line, upWaitDownStay, 0) <> 0 then
          P_ChangeSwitchTexture(line, true); // In P_CrossSpecialLine? Copypasta error? // JVAL: SOS
      end;

    185:
      begin
        // haleyjd 09/21/10: [STRIFE] Silent Teleport (used for Converter)
        EV_Teleport(line, side, thing, TF_FULLSILENCE);
      end;

    195:
      begin
        // haleyjd 09/21/10: [STRIFE] Silent Teleport and Change Zombie
        EV_Teleport(line, side, thing, TF_FULLSILENCE);
        P_SetMobjState(thing, S_AGRD_00); // 419
      end;

    203:
      begin
        // haleyjd 09/21/10: [STRIFE] WR Change Music
        if thing.player = @players[0] then
          S_ChangeMusic(line.tag, true);
      end;

    231:
      begin
        // haleyjd 09/21/10: [STRIFE] WR Teleport (Silent at Source)
        EV_Teleport(line, side, thing, TF_SRCSILENCE);
      end;

        // haleyjd 09/21/10: Moved one-time-use lines up above with the others.
  else
    begin
    // JVAL: Obviously some of the following do not work :(
    //  (OR EVEN WORSE CAUSE BIG PROBLEMS)
      if not G_NeedsCompatibilityMode then
        IF FALSE THEN
        case line.special of
          // Extended walk once triggers

          142:
            begin
              // Raise Floor 512
              // 142 W1  EV_DoFloor(raiseFloor512)
              if EV_DoFloor(line,raiseFloor512) <> 0 then
                line.special := 0;
            end;

{          143:
            begin
              // Raise Floor 24 and change
              // 143 W1  EV_DoPlat(raiseAndChange,24)
              if EV_DoPlat(line,raiseAndChange, 24) <> 0 then
                line.special := 0;
            end;                  }

          144:
            begin
              // Raise Floor 32 and change
              // 144 W1  EV_DoPlat(raiseAndChange,32)
              if EV_DoPlat(line,raiseAndChange, 32) <> 0 then
                line.special := 0;
            end;

          145:
            begin
              // Lower Ceiling to Floor
              // 145 W1  EV_DoCeiling(lowerToFloor)
              if EV_DoCeiling( line, lowerToFloor ) <> 0 then
                line.special := 0;
            end;

          146:
            begin
              // Lower Pillar, Raise Donut
              // 146 W1  EV_DoDonut
              if EV_DoDonut(line) <> 0 then
                line.special := 0;
            end;

          199:
            begin
              // Lower ceiling to lowest surrounding ceiling
              // 199 W1 EV_DoCeiling(lowerToLowest)
              if EV_DoCeiling(line, lowerToLowest) <> 0 then
                line.special := 0;
            end;

          200:
            begin
              // Lower ceiling to highest surrounding floor
              // 200 W1 EV_DoCeiling(lowerToMaxFloor)
              if EV_DoCeiling(line, lowerToMaxFloor) <> 0 then
                line.special := 0;
            end;

          207:
            begin
              // killough 2/16/98: W1 silent teleporter (normal kind)
              if EV_SilentTeleport(line, side, thing) <> 0 then
                line.special := 0;
            end;

            //jff 3/16/98 renumber 215.153
          153:
            begin
              //jff 3/15/98 create texture change no motion type
              // Texture/Type Change Only (Trig)
              // 153 W1 Change Texture/Type Only
              if EV_DoChange(line, trigChangeOnly) <> 0 then
                line.special := 0;
            end;

          239:
            begin
              //jff 3/15/98 create texture change no motion type
              // Texture/Type Change Only (Numeric)
              // 239 W1 Change Texture/Type Only
              if EV_DoChange(line, numChangeOnly) <> 0 then
                line.special := 0;
            end;

          219:
            begin
              // Lower floor to next lower neighbor
              // 219 W1 Lower Floor Next Lower Neighbor
              if EV_DoFloor(line, lowerFloorToNearest) <> 0 then
                line.special := 0;
            end;

          227:
            begin
              // Raise elevator next floor
              // 227 W1 Raise Elevator next floor
              if EV_DoElevator(line, elevateUp) <> 0 then
                line.special := 0;
            end;

          231:
            begin
              // Lower elevator next floor
              // 231 W1 Lower Elevator next floor
              if EV_DoElevator(line, elevateDown) <> 0 then
                line.special := 0;
            end;

          235:
            begin
              // Elevator to current floor
              // 235 W1 Elevator to current floor
              if EV_DoElevator(line, elevateCurrent) <> 0 then
                line.special := 0;
            end;

          243:
            begin
              //jff 3/6/98 make fit within DCK's 256 linedef types
              // killough 2/16/98: W1 silent teleporter (linedef-linedef kind)
              if EV_SilentLineTeleport(line, side, thing, false) <> 0 then
                line.special := 0;
            end;

          262:
            begin
              //jff 4/14/98 add silent line-line reversed
              if EV_SilentLineTeleport(line, side, thing, true) <> 0 then
                line.special := 0;
            end;

          264:
            begin
            //jff 4/14/98 add monster-only silent line-line reversed
              if thing.player = nil then
                if EV_SilentLineTeleport(line, side, thing, true) <> 0 then
                  line.special := 0;
            end;

          266:
            begin
              //jff 4/14/98 add monster-only silent line-line
              if thing.player = nil then
                if EV_SilentLineTeleport(line, side, thing, false) <> 0 then
                  line.special := 0;
            end;

          268: //jff 4/14/98 add monster-only silent
            begin
              if thing.player = nil then
                if EV_SilentTeleport(line, side, thing) <> 0 then
                  line.special := 0;
            end;

          //jff 1/29/98 end of added W1 linedef types

          // Extended walk many retriggerable

          //jff 1/29/98 added new linedef types to fill all functions
          //out so that all have varieties SR, S1, WR, W1

          147:
            begin
              // Raise Floor 512
              // 147 WR  EV_DoFloor(raiseFloor512)
              EV_DoFloor(line, raiseFloor512);
            end;

          148:
            begin
              // Raise Floor 24 and Change
              // 148 WR  EV_DoPlat(raiseAndChange,24)
              EV_DoPlat(line, raiseAndChange, 24);
            end;

          149:
            begin
              // Raise Floor 32 and Change
              // 149 WR  EV_DoPlat(raiseAndChange,32)
              EV_DoPlat(line, raiseAndChange, 32);
            end;

          150:
            begin
              // Start slow silent crusher
              // 150 WR  EV_DoCeiling(silentCrushAndRaise)
              EV_DoCeiling(line, silentCrushAndRaise);
            end;

          151:
            begin
              // RaiseCeilingLowerFloor
              // 151 WR  EV_DoCeiling(raiseToHighest),
              //         EV_DoFloor(lowerFloortoLowest)
              EV_DoCeiling(line, raiseToHighest);
              EV_DoFloor(line, lowerFloorToLowest);
            end;

          152:
            begin
              // Lower Ceiling to Floor
              // 152 WR  EV_DoCeiling(lowerToFloor)
              EV_DoCeiling(line, lowerToFloor);
            end;

            //jff 3/16/98 renumber 153.256
          256:
            begin
              // Build stairs, step 8
              // 256 WR EV_BuildStairs(build8)
              EV_BuildStairs(line, build8);
            end;

            //jff 3/16/98 renumber 154.257
          257:
            begin
              // Build stairs, step 16
              // 257 WR EV_BuildStairs(turbo16)
              EV_BuildStairs(line, turbo16);
            end;

          155:
            begin
              // Lower Pillar, Raise Donut
              // 155 WR  EV_DoDonut
              EV_DoDonut(line);
            end;

          156:
            begin
              // Start lights strobing
              // 156 WR Lights EV_StartLightStrobing
              EV_StartLightStrobing(line);
            end;

          157:
            begin
              // Lights to dimmest near
              // 157 WR Lights EV_TurnTagLightsOff
              EV_TurnTagLightsOff(line);
            end;

          201:
            begin
              // Lower ceiling to lowest surrounding ceiling
              // 201 WR EV_DoCeiling(lowerToLowest)
              EV_DoCeiling(line,lowerToLowest);
            end;

          202:
            begin
              // Lower ceiling to highest surrounding floor
              // 202 WR EV_DoCeiling(lowerToMaxFloor)
              EV_DoCeiling(line,lowerToMaxFloor);
            end;

          208:
            begin
              // killough 2/16/98: WR silent teleporter (normal kind)
              EV_SilentTeleport(line, side, thing);
            end;

          212:
            begin
              //jff 3/14/98 create instant toggle floor type
              // Toggle floor between C and F instantly
              // 212 WR Instant Toggle Floor
              EV_DoPlat(line, toggleUpDn, 0);
            end;

          //jff 3/16/98 renumber 216.154
          154:
            begin
              //jff 3/15/98 create texture change no motion type
              // Texture/Type Change Only (Trigger)
              // 154 WR Change Texture/Type Only
              EV_DoChange(line, trigChangeOnly);
            end;

          240:
            begin
              //jff 3/15/98 create texture change no motion type
              // Texture/Type Change Only (Numeric)
              // 240 WR Change Texture/Type Only
              EV_DoChange(line, numChangeOnly);
            end;

          220:
            begin
              // Lower floor to next lower neighbor
              // 220 WR Lower Floor Next Lower Neighbor
              EV_DoFloor(line, lowerFloorToNearest);
            end;

          228:
            begin
              // Raise elevator next floor
              // 228 WR Raise Elevator next floor
              EV_DoElevator(line, elevateUp);
            end;

          232:
            begin
              // Lower elevator next floor
              // 232 WR Lower Elevator next floor
              EV_DoElevator(line, elevateDown);
            end;

          236:
            begin
              // Elevator to current floor
              // 236 WR Elevator to current floor
              EV_DoElevator(line,elevateCurrent);
            end;

          244:
            begin
              //jff 3/6/98 make fit within DCK's 256 linedef types
              // killough 2/16/98: WR silent teleporter (linedef-linedef kind)
              EV_SilentLineTeleport(line, side, thing, false);
            end;

          263:
            begin
              //jff 4/14/98 add silent line-line reversed
              EV_SilentLineTeleport(line, side, thing, true);
            end;

          265:
            begin
              //jff 4/14/98 add monster-only silent line-line reversed
              if thing.player = nil then
                EV_SilentLineTeleport(line, side, thing, true);
            end;

          267:
            begin
              //jff 4/14/98 add monster-only silent line-line
              if thing.player = nil then
                EV_SilentLineTeleport(line, side, thing, false);
            end;

          269:
            begin
              //jff 4/14/98 add monster-only silent
              if thing.player = nil then
                EV_SilentTeleport(line, side, thing);
            end;

            //jff 1/29/98 end of added WR linedef types

      end;
    end;
  end;
end;

//
// P_ShootSpecialLine - IMPACT SPECIALS
// Called when a thing shoots a special line.
//
// jff 02/12/98 all G1 lines were fixed to check the result from the EV_
// function before clearing the special. This avoids losing the function
// of the line, should the sector already be in motion when the line is
// impacted. Change is qualified by demo_compatibility.
//
// JVAL: Changed for BOOM compatibility
///
procedure P_ShootSpecialLine(thing: Pmobj_t; line: Pline_t);
var
  linefunc: linefunc_t;
begin
  // generalized types
  if not G_NeedsCompatibilityMode then
  begin
    // pointer to line function is nil by default, set non-null if
    // line special is walkover generalized linedef type
    linefunc := nil;

    // check each range of generalized linedefs
    if word(line.special) >= CGENFLOORBASE then
    begin
      if thing.player = nil then
        if (line.special and gen_FloorChange <> 0) or (line.special and gen_FloorModel = 0) then
          exit;   // FloorModel is 'Allow Monsters' if FloorChange is 0
      if line.tag = 0 then //jff 2/27/98 all gun generalized types require tag
        exit;

      linefunc := @EV_DoGenFloor;
    end
    else if word(line.special) >= CGENCEILINGBASE  then
    begin
      if thing.player = nil then
        if (line.special and CeilingChange <> 0) or (line.special and CeilingModel = 0) then
          exit;   // CeilingModel is 'Allow Monsters' if CeilingChange is 0
      if line.tag = 0 then //jff 2/27/98 all gun generalized types require tag
        exit;
      linefunc := @EV_DoGenCeiling;
    end
    else if word(line.special) >= CGENDOORBASE then
    begin
      if thing.player = nil then
      begin
        if line.special and DoorMonster = 0 then
          exit;   // monsters disallowed from this door
        if line.flags and ML_SECRET = 0 then // they can't open secret doors either
          exit;
      end;
      if line.tag = 0 then //jff 3/2/98 all gun generalized types require tag
        exit;
      linefunc := @EV_DoGenDoor;
    end
    else if word(line.special) >= CGENLOCKEDBASE then
    begin
      if thing.player = nil then
        exit;   // monsters disallowed from unlocking doors
      if (line.special and TriggerType = Ord(GunOnce)) or (line.special and TriggerType = Ord(GunMany)) then
      begin //jff 4/1/98 check for being a gun type before reporting door type
        if not P_CanUnlockGenDoor(line, thing.player) then
          exit;
      end
      else
        exit;
      if line.tag = 0 then //jff 2/27/98 all gun generalized types require tag
        exit;

      linefunc := @EV_DoGenLockedDoor;
    end
    else if word(line.special) >= CGENLIFTBASE then
    begin
      if thing.player = nil then
        if line.special and LiftMonster = 0 then
          exit; // monsters disallowed
      linefunc := @EV_DoGenLift;
    end
    else if word(line.special) >= CGENSTAIRSBASE then
    begin
      if thing.player = nil then
        if line.special and StairMonster = 0 then
          exit; // monsters disallowed
      if line.tag = 0 then //jff 2/27/98 all gun generalized types require tag
        exit;
      linefunc := @EV_DoGenStairs;
    end
    else if word(line.special) >= CGENCRUSHERBASE then
    begin
      if thing.player = nil then
        if line.special and StairMonster = 0 then
          exit; // monsters disallowed
      if line.tag = 0 then //jff 2/27/98 all gun generalized types require tag
        exit;
      linefunc := @EV_DoGenCrusher;
    end;

    if Assigned(linefunc) then
    begin
      case (line.special and TriggerType) shr TriggerTypeShift of
        Ord(GunOnce):
          begin
            if linefunc(line) <> 0 then
              P_ChangeSwitchTexture(line, false);
          end;
        Ord(GunMany):
          begin
            if linefunc(line) <> 0 then
              P_ChangeSwitchTexture(line, true);
          end;
      end;
      exit; // if not a gun type, do nothing here
    end;

  end;

  //  Impacts that other things can activate.
  if thing.player = nil then
    case line.special of
      46: ; // OPEN DOOR IMPACT
     182: ; // villsa [STRIFE] for windows
    else
      exit;
    end;

  case line.special of
    24:
      begin
        // RAISE FLOOR
        if (EV_DoFloor(line, raiseFloor) <> 0) or G_NeedsCompatibilityMode then
          P_ChangeSwitchTexture(line, false);
      end;

    46:
      begin
        // OPEN DOOR
        EV_DoDoor(line, vld_open);
        P_ChangeSwitchTexture(line, true);
      end;

    47:
      begin
        // RAISE FLOOR NEAR AND CHANGE
        if (EV_DoPlat(line, raiseToNearestAndChange, 0) <> 0) or G_NeedsCompatibilityMode then
          P_ChangeSwitchTexture(line, false);
      end;

    180:
      begin
        // haleyjd 09/22/10: [STRIFE] G1 Raise Floor 512 & Change
        EV_DoFloor(line, raiseFloor512AndChange);
        P_ChangeSwitchTexture(line, false);
      end;

    182:
      begin
        // villsa [STRIFE] G1 Break Glass
        //   haleyjd: note that 182 is also a W1 type in P_CrossSpecialLine, but
        //   can only be activated in that manner by an MF_MISSILE object.
        P_ChangeSwitchTexture(line, false);
      end;

  else
    begin
      if not G_NeedsCompatibilityMode then
        IF FALSE THEN
        case line.special of
         197:
          begin
            // Exit to next level
            P_ChangeSwitchTexture(line, false);
            G_ExitLevel(0);
          end;

        end;
    end;
  end;
end;

//
// P_PlayerInSpecialSector
// Called every tic frame
//  that the player origin is in a special sector
//
// [STRIFE] Modified for new sector types and changes to old ones.
//
procedure P_PlayerInSpecialSector(player: Pplayer_t; const sector: Psector_t; const height: fixed_t);  // JVAL: 3d Floors
var
  tagval: integer;
  force: fixed_t;
  angle: angle_t;
begin
  // Falling, not all the way down yet?
  if player.mo.z <> height then
    exit;


  // Has hitten ground.
  case sector.special of
     5:
      begin
        // HELLSLIME DAMAGE
        if player.powers[Ord(pw_ironfeet)] = 0 then
          // [STRIFE] +2 to nukagecount
          inc(player.nukagecount, 2);
      end;

    16:
      begin
        // HELLSLIME DAMAGE
        if player.powers[Ord(pw_ironfeet)] = 0 then
          // [STRIFE] +2 to nukagecount
          inc(player.nukagecount, 4);
      end;

     4,
     7:
      begin
        // NUKAGE DAMAGE
        // [STRIFE] Immediate 5 damage every 31 tics
        if player.powers[Ord(pw_ironfeet)] = 0 then
          if leveltime and $1f = 0 then
            P_DamageMobj(player.mo, nil, nil, 5);
      end;

     9:
      begin
        // SECRET SECTOR
        player._message := MSGSECRETSECTOR;
        if player = @players[consoleplayer] then
          S_StartSound(nil, Ord(sfx_yeah));
        sector.special := 0;
      end;

    11:
      begin
        // EXIT SUPER DAMAGE! (for E1M8 finale)
        player.cheats := player.cheats and not CF_GODMODE;

        if leveltime and $1f = 0 then
          P_DamageMobj(player.mo, nil, nil, 20);

        if player.health <= 10 then
          G_ExitLevel(0);
      end;

    15:
      begin
        // haleyjd 08/30/10: [STRIFE] "Instant" Death sector
        P_DamageMobj(player.mo, nil, nil, 999);
      end;

    18:
      begin
        // haleyjd 08/30/10: [STRIFE] Water current
        tagval := sector.tag - 100;

        if player.cheats and CF_NOCLIP <> 0 then
          exit;

        force := (tagval mod 10) * 4096;
        angle := (tagval div 10) * $20000000;

        P_Thrust(player, angle, force);
      end;
  end;

  if sector.special >= 32 then  // BOOM sector specials
  begin
    case (sector.special and DAMAGE_MASK) shr DAMAGE_SHIFT of
      0: // no damage
        begin
        end;

      1: // 2/5 damage per 31 ticks
        begin
          if player.powers[Ord(pw_ironfeet)] = 0 then
            if leveltime and $1f = 0 then
              P_DamageMobj(player.mo, nil, nil, 5);
        end;

      2: // 5/10 damage per 31 ticks
        begin
          if player.powers[Ord(pw_ironfeet)] = 0 then
            if leveltime and $1f = 0 then
              P_DamageMobj(player.mo, nil, nil, 10);
        end;

      3: // 10/20 damage per 31 ticks
        begin
          if (player.powers[Ord(pw_ironfeet)] = 0) or
             (N_Random < 5) then  // take damage even with suit
          begin
            if leveltime and $1f = 0 then
              P_DamageMobj(player.mo, nil, nil, 20);
          end;
        end;
    end;

    if sector.special and SECRET_MASK <> 0 then
    begin
      player._message := MSGSECRETSECTOR;
      sector.special := sector.special and not SECRET_MASK;
      if sector.special < 32 then // if all extended bits clear,
        sector.special := 0;    // sector is not special anymore
    end;

    // phares 3/19/98:
    //
    // If FRICTION_MASK or PUSH_MASK is set, we don't care at this
    // point, since the code to deal with those situations is
    // handled by Thinkers.
  end;
end;

var
  numlinespecials: smallint;
  linespeciallist: array[0..MAXLINEANIMS - 1] of Pline_t;


//
// P_UpdateSpecials
// Animate planes, scroll walls, etc.
//
// [STRIFE] Modifications to support multiple scrolling line types.
//
procedure P_UpdateSpecials;
var
  anim: Panim_t;
  pic: integer;
  i: integer;
  j: integer;
  line: Pline_t;
  button: Pbutton_t;
begin
  // LEVEL TIMER
  if levelTimer then
  begin
    if levelTimeCount > 0 then  // [STRIFE] Does not allow to go negative
      dec(levelTimeCount);

  end;

  // ANIMATE FLATS AND TEXTURES GLOBALLY
  for j := 0 to lastanim - 1 do
  begin
    anim := @anims[j];
    for i := anim.basepic to anim.basepic + anim.numpics - 1 do
    begin
      pic := anim.basepic + ((leveltime div anim.speed + i) mod anim.numpics);
      if anim.istexture then
        texturetranslation[i] := pic
      else
        flats[i].translation := pic;
    end;
  end;

  {$IFNDEF OPENGL}
  curripple := @r_defripple[leveltime and 31];
  {$ENDIF}

  // ANIMATE LINE SPECIALS
  for i := 0 to numlinespecials - 1 do
  begin
    line := linespeciallist[i];
    case line.special of
      48: inc(sides[line.sidenum[0]].textureoffset, FRACUNIT);
     142: // haleyjd 09/25/10 [STRIFE] Scroll Up Slow
          inc(sides[line.sidenum[0]].rowoffset, FRACUNIT);
     143: // haleyjd 09/25/10 [STRIFE] Scroll Down Fast (3 Units/Tic)
          dec(sides[line.sidenum[0]].rowoffset, 3 * FRACUNIT);
     149: // haleyjd 09/25/10 [STRIFE] Scroll Down Slow
          dec(sides[line.sidenum[0]].rowoffset, FRACUNIT);
    end;
  end;


  // DO BUTTONS
  button := @buttonlist[0];
  for i := 0 to MAXBUTTONS - 1 do
  begin
    if button.btimer <> 0 then
    begin
      button.btimer := buttonlist[i].btimer - 1;

      if button.btimer = 0 then
      begin
        case button.where of
          top:
            sides[button.line.sidenum[0]].toptexture := button.btexture;

          middle:
            sides[button.line.sidenum[0]].midtexture := button.btexture;

          bottom:
            sides[button.line.sidenum[0]].bottomtexture := button.btexture;
        end;
        S_StartSound(Pmobj_t(@button.soundorg), Ord(sfx_swtchn));
        ZeroMemory(button, SizeOf(button_t));
      end;

    end;
    inc(button);
  end;
end;

//
// SPECIAL SPAWNING
//

//
// P_SpawnSpecials
// After the map has been loaded, scan for specials
//  that spawn thinkers
//
// Parses command line parameters.
procedure P_SpawnSpecials;
var
  sector: Psector_t;
  i: integer;
  time: integer;
  s, sec: integer;
  ang: angle_t;
begin
  // See if -TIMER needs to be used.
  levelTimer := false;

  // JVAL: Reformated
  if deathmatch <> 0 then
  begin
    // Austin Virtual Gaming 20 min timer on DM play
    i := M_CheckParm('-avg');
    if i <> 0 then
    begin
      levelTimer := true;
      levelTimeCount := 20 * 60 * TICRATE;
    end;

    i := M_CheckParm('-timer');
    if i <> 0 then
    begin
      time := atoi(myargv[i + 1]) * 60 * TICRATE;
      levelTimer := true;
      levelTimeCount := time;
    end;
  end;

  //  Init special SECTORs.
  sector := @sectors[0];
  dec(sector);
  for i := 0 to numsectors - 1 do
  begin
    inc(sector);
    if sector.special = 0 then
      continue;

    if sector.special and SECRET_MASK <> 0 then //jff 3/15/98 count extended
      inc(totalsecret);                         // secret sectors too

    case sector.special and 31 of
     1:
      begin
        // FLICKERING LIGHTS
        P_SpawnLightFlash(sector);
      end;

     2:
      begin
        // STROBE FAST
        P_SpawnStrobeFlash(sector, FASTDARK, 0);
      end;

     3:
      begin
        // STROBE SLOW
        P_SpawnStrobeFlash(sector, SLOWDARK, 0);
      end;

     4:
      begin
        // STROBE FAST/DEATH SLIME
        P_SpawnStrobeFlash(sector, FASTDARK, 0);
        sector.special := sector.special or (3 shl DAMAGE_SHIFT); //jff 3/14/98 put damage bits in
      end;

     8:
      begin
        // GLOWING LIGHT
        P_SpawnGlowingLight(sector);
      end;

     9:
      begin
        // SECRET SECTOR
        if sector.special < 32 then //jff 3/14/98 bits don't count unless not
          inc(totalsecret);         // a generalized sector type
      end;

    10:
      begin
        // DOOR CLOSE IN 30 SECONDS
        P_SpawnDoorCloseIn30(sector);
      end;

    12:
      begin
        // SYNC STROBE SLOW
        P_SpawnStrobeFlash(sector, SLOWDARK, 1);
      end;

    13:
      begin
        // SYNC STROBE FAST
        P_SpawnStrobeFlash(sector, FASTDARK, 1);
      end;

    14:
      begin
        // DOOR RAISE IN 5 MINUTES
        P_SpawnDoorRaiseIn5Mins(sector, i);
      end;

    17:
      begin
        // fire flickering
        P_SpawnFireFlicker(sector);
      end;
    end;
  end;


    //  Init line EFFECTs
  numlinespecials := 0;
  for i := 0 to numlines - 1 do
  begin
    case lines[i].special of
      48, 142, 143, 149: // JVAL: Strife Scrolling specials
        begin
          // EFFECT FIRSTCOL SCROLL+
          if numlinespecials < MAXLINEANIMS then
          begin
            linespeciallist[numlinespecials] := @lines[i];
            inc(numlinespecials);
          end;
        end;
    end;
  end;


  //  Init other misc stuff
  for i := 0 to MAXCEILINGS - 1 do
    activeceilings[i] := nil;

  for i := 0 to MAXPLATS - 1 do
    activeplats[i] := nil;

  for i := 0 to MAXBUTTONS - 1 do
    ZeroMemory(@buttonlist[i], SizeOf(button_t));

  P_SpawnScrollers;

  P_SpawnFriction;  // phares 3/12/98: New friction model using linedefs

  P_SpawnPushers;   // phares 3/20/98: New pusher model using linedefs

  for i := 0 to numlines - 1 do
    case lines[i].special of
      // killough 3/7/98:
      // support for drawn heights coming from different sector
      242:
        begin
          sec := pDiff(sides[lines[i].sidenum[0]].sector, sectors, SizeOf(sector_t));
          s := -1;
          while P_FindSectorFromLineTag2(@lines[i], s) >= 0 do
            sectors[s].heightsec := sec;
        end;

      // killough 3/16/98: Add support for setting
      // floor lighting independently (e.g. lava)
      213:
        begin
          sec := pDiff(sides[lines[i].sidenum[0]].sector, sectors, SizeOf(sector_t));
          s := -1;
          while P_FindSectorFromLineTag2(@lines[i], s) >= 0 do
            sectors[s].floorlightsec := sec;
        end;

      // killough 4/11/98: Add support for setting
      // ceiling lighting independently
      261:
        begin
          sec := pDiff(sides[lines[i].sidenum[0]].sector, sectors, SizeOf(sector_t));
          s := -1;
          while P_FindSectorFromLineTag2(@lines[i], s) >= 0 do
            sectors[s].ceilinglightsec := sec;
        end;
      // JVAL: ripple effect to tagged sectors floor
      279:
        begin
          s := -1;
          while P_FindSectorFromLineTag2(@lines[i], s) >= 0 do
            sectors[s].renderflags := sectors[s].renderflags or SRF_RIPPLE_FLOOR;
        end;
      // JVAL: ripple effect to tagged sectors floor
      280:
        begin
          s := -1;
          while P_FindSectorFromLineTag2(@lines[i], s) >= 0 do
            sectors[s].renderflags := sectors[s].renderflags or SRF_RIPPLE_CEILING;
        end;
      // JVAL: ladder to tagged sectors (when sliding)
      282:
        begin
          s := -1;
          while P_FindSectorFromLineTag2(@lines[i], s) >= 0 do
            sectors[s].flags := sectors[s].flags or SF_LADDER;
        end;
      // JVAL: Slip while descenting a sloped sector
      283:
        begin
          s := -1;
          while P_FindSectorFromLineTag2(@lines[i], s) >= 0 do
            sectors[s].flags := sectors[s].flags or SF_SLIPSLOPEDESCENT;
        end;
      284:
        begin
          ang := R_PointToAngle2(lines[i].v1.x, lines[i].v1.y, lines[i].v2.x, lines[i].v2.y);
          s := -1;
          while P_FindSectorFromLineTag2(@lines[i], s) >= 0 do
          begin
            sectors[s].floorangle := ang;
            sectors[s].flooranglex := lines[i].v1.x;
            sectors[s].floorangley := lines[i].v1.y;
          end;
        end;
      // JVAL: 20200517 - Rotate sector ceiling
      285:
        begin
          ang := R_PointToAngle2(lines[i].v1.x, lines[i].v1.y, lines[i].v2.x, lines[i].v2.y);
          s := -1;
          while P_FindSectorFromLineTag2(@lines[i], s) >= 0 do
          begin
            sectors[s].ceilingangle := ang;
            sectors[s].ceilinganglex := lines[i].v1.x;
            sectors[s].ceilingangley := lines[i].v1.y;
          end;
        end;
      291:  // JVAL: 20200521 - Offset floor texture to vector
        begin
          s := -1;
          while P_FindSectorFromLineTag2(@lines[i], s) >= 0 do
          begin
            sectors[s].floor_xoffs := lines[i].dx;
            sectors[s].floor_yoffs := lines[i].dy;
          end;
        end;
      292:  // JVAL: 20200521 - Offset ceiling texture to vector
        begin
          s := -1;
          while P_FindSectorFromLineTag2(@lines[i], s) >= 0 do
          begin
            sectors[s].ceiling_xoffs := lines[i].dx;
            sectors[s].ceiling_yoffs := lines[i].dy;
          end;
        end;
    end;

  // villsa [STRIFE]
  P_InitSlidingDoorFrames;
end;


// JVAL: BOOM compatibility
function P_SectorActive(const s: special_e; const sec: Psector_t): boolean;
begin
  case s of
    floor_special: result := sec.floordata <> nil;
    ceiling_special: result := sec.ceilingdata <> nil;
    lighting_special:  result := sec.lightingdata <> nil;
  else
    result := false;
  end;
end;


// killough 3/7/98 -- end generalized scroll effects

////////////////////////////////////////////////////////////////////////////
//
// FRICTION EFFECTS
//
// phares 3/12/98: Start of friction effects

// As the player moves, friction is applied by decreasing the x and y
// momentum values on each tic. By varying the percentage of decrease,
// we can simulate muddy or icy conditions. In mud, the player slows
// down faster. In ice, the player slows down more slowly.
//
// The amount of friction change is controlled by the length of a linedef
// with type 223. A length < 100 gives you mud. A length > 100 gives you ice.
//
// Also, each sector where these effects are to take place is given a
// new special type _______. Changing the type value at runtime allows
// these effects to be turned on or off.
//
// Sector boundaries present problems. The player should experience these
// friction changes only when his feet are touching the sector floor. At
// sector boundaries where floor height changes, the player can find
// himself still 'in' one sector, but with his feet at the floor level
// of the next sector (steps up or down). To handle this, Thinkers are used
// in icy/muddy sectors. These thinkers examine each object that is touching
// their sectors, looking for players whose feet are at the same level as
// their floors. Players satisfying this condition are given new friction
// values that are applied by the player movement code later.

/////////////////////////////
//
// This is where abnormal friction is applied to objects in the sectors.
// A friction thinker has been spawned for each sector where less or
// more friction should be applied. The amount applied is proportional to
// the length of the controlling linedef.

procedure T_Friction(f: Pfriction_t);
var
  sec: Psector_t;
  thing: Pmobj_t;
  node: Pmsecnode_t;
begin
  sec := @sectors[f.affectee];

  // Be sure the special sector type is still turned on. If so, proceed.
  // Else, bail out; the sector type has been changed on us.

  if sec.special and FRICTION_MASK = 0 then
    exit;

  // Assign the friction value to players on the floor, non-floating,
  // and clipped. Normally the object's friction value is kept at
  // ORIG_FRICTION and this thinker changes it for icy or muddy floors.

  // In Phase II, you can apply friction to Things other than players.

  // When the object is straddling sectors with the same
  // floorheight that have different frictions, use the lowest
  // friction value (muddy has precedence over icy).

  node := sec.touching_thinglist;
  while node <> nil do
  begin
    thing := node.m_thing;
    if (thing.player <> nil) and
       (thing.flags and (MF_NOGRAVITY or MF_NOCLIP) = 0) and
       (thing.z <= sec.floorheight) then
    begin
      if (thing.friction = ORIG_FRICTION) or     // normal friction?
         (f.friction < thing.friction) then
      begin
        thing.friction := f.friction;
        thing.movefactor := f.movefactor;
      end;
    end;
    node := node.m_snext;
  end;
end;

/////////////////////////////
//
// Add a friction thinker to the thinker list
//
// Add_Friction adds a new friction thinker to the list of active thinkers.
//

procedure Add_Friction(friction: integer; movefactor: integer; affectee: integer);
var
  f: Pfriction_t;
begin
  f := Z_Malloc(SizeOf(friction_t), PU_LEVSPEC, nil);

  f.thinker._function.acp1 := @T_Friction;
  f.friction := friction;
  f.movefactor := movefactor;
  f.affectee := affectee;
  P_AddThinker(@f.thinker);
end;


/////////////////////////////
//
// Initialize the sectors where friction is increased or decreased

procedure P_SpawnFriction;
var
  l, le: Pline_t;
  s: integer;
  len: integer;        // line length controls magnitude
  friction: integer;   // friction value to be applied during movement
  movefactor: integer; // applied to each player move to simulate inertia
begin
  l := @lines[0];
  le := @lines[numlines];

  while l <> le do
  begin
    if l.special = 223 then
    begin
      len := P_AproxDistance(l.dx, l.dy) div FRACUNIT;
      friction := ($1EB8 * len) div $80 + $D000;

      // The following check might seem odd. At the time of movement,
      // the move distance is multiplied by 'friction/0x10000', so a
      // higher friction value actually means 'less friction'.

      if friction > ORIG_FRICTION then       // ice
        movefactor := (($10092 - friction) * $70) div $158
      else
        movefactor := ((friction - $DB34) * $A) div $80;
      s := -1;
      while P_FindSectorFromLineTag2(l, s) >= 0 do
        Add_Friction(friction, movefactor, s);
    end;
    inc(l);
  end;
end;

//
// phares 3/12/98: End of friction effects
//
////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////
//
// PUSH/PULL EFFECT
//
// phares 3/20/98: Start of push/pull effects
//
// This is where push/pull effects are applied to objects in the sectors.
//
// There are four kinds of push effects
//
// 1) Pushing Away
//
//    Pushes you away from a point source defined by the location of an
//    MT_PUSH Thing. The force decreases linearly with distance from the
//    source. This force crosses sector boundaries and is felt w/in a circle
//    whose center is at the MT_PUSH. The force is felt only if the point
//    MT_PUSH can see the target object.
//
// 2) Pulling toward
//
//    Same as Pushing Away except you're pulled toward an MT_PULL point
//    source. This force crosses sector boundaries and is felt w/in a circle
//    whose center is at the MT_PULL. The force is felt only if the point
//    MT_PULL can see the target object.
//
// 3) Wind
//
//    Pushes you in a constant direction. Full force above ground, half
//    force on the ground, nothing if you're below it (water).
//
// 4) Current
//
//    Pushes you in a constant direction. No force above ground, full
//    force if on the ground or below it (water).
//
// The magnitude of the force is controlled by the length of a controlling
// linedef. The force vector for types 3 & 4 is determined by the angle
// of the linedef, and is constant.
//
// For each sector where these effects occur, the sector special type has
// to have the PUSH_MASK bit set. If this bit is turned off by a switch
// at run-time, the effect will not occur. The controlling sector for
// types 1 & 2 is the sector containing the MT_PUSH/MT_PULL Thing.


const
  PUSH_FACTOR = 7;

// tmpusher belongs to the point source (MT_PUSH/MT_PULL).
//
var
  tmpusher: Ppusher_t; // pusher structure for blockmap searches

/////////////////////////////
//
// PIT_PushThing determines the angle and magnitude of the effect.
// The object's x and y momentum values are changed.
//

function PIT_PushThing(thing: Pmobj_t): boolean;
var
  pushangle: angle_t;
  dist: integer;
  speed: integer;
  sx, sy: integer;
begin
  if thing.player = nil then
  begin
    result := false;
    exit;
  end;

  if (thing.flags and (MF_NOGRAVITY or MF_NOCLIP)) <> 0 then
  begin
    result := false;
    exit;
  end;

  sx := tmpusher.x;
  sy := tmpusher.y;
  dist := P_AproxDistance(thing.x - sx,thing.y - sy);
  speed := (tmpusher.magnitude - (dist div (2 * FRACUNIT))) * (1 shl (FRACBITS - PUSH_FACTOR - 1));

  // If speed <= 0, you're outside the effective radius. You also have
  // to be able to see the push/pull source point.

  if (speed > 0) and P_CheckSight(thing, tmpusher.source) then
  begin
    pushangle := R_PointToAngle2(thing.x, thing.y, sx, sy);
    if tmpusher.source._type = Ord(MT_PUSH) then
      pushangle := pushangle + ANG180;    // away
    pushangle := pushangle shr ANGLETOFINESHIFT;
    thing.momx := thing.momx + FixedMul(speed, finecosine[pushangle]);
    thing.momy := thing.momy + FixedMul(speed, finesine[pushangle]);
  end;

  result := true;
end;

/////////////////////////////
//
// T_Pusher looks for all objects that are inside the radius of
// the effect.
//

procedure T_Pusher(p: Ppusher_t);
var
  sec: Psector_t;
  thing: Pmobj_t;
  node: Pmsecnode_t;
  xspeed, yspeed: integer;
  xl, xh, yl, yh, bx, by: integer;
  radius: integer;
  ht: integer;
begin
  sec := @sectors[p.affectee];

  // Be sure the special sector type is still turned on. If so, proceed.
  // Else, bail out; the sector type has been changed on us.
  if sec.special and PUSH_MASK = 0 then
    exit;

  // For constant pushers (wind/current) there are 3 situations:
  //
  // 1) Affected Thing is above the floor.
  //
  //    Apply the full force if wind, no force if current.
  //
  // 2) Affected Thing is on the ground.
  //
  //    Apply half force if wind, full force if current.
  //
  // 3) Affected Thing is below the ground (underwater effect).
  //
  //    Apply no force if wind, full force if current.
  //
  // Apply the effect to clipped players only for now.
  //
  // In Phase II, you can apply these effects to Things other than players.

   if p._type = p_push then
   begin

    // Seek out all pushable things within the force radius of this
    // point pusher. Crosses sectors, so use blockmap.

    tmpusher := p; // MT_PUSH/MT_PULL point source
    radius := p.radius; // where force goes to zero
    tmbbox[BOXTOP]    := p.y + radius;
    tmbbox[BOXBOTTOM] := p.y - radius;
    tmbbox[BOXRIGHT]  := p.x + radius;
    tmbbox[BOXLEFT]   := p.x - radius;

    if internalblockmapformat then
    begin
      xl := MapBlockIntX(int64(tmbbox[BOXLEFT]) - int64(bmaporgx) - MAXRADIUS);
      xh := MapBlockIntX(int64(tmbbox[BOXRIGHT]) - int64(bmaporgx) + MAXRADIUS);
      yl := MapBlockIntY(int64(tmbbox[BOXBOTTOM]) - int64(bmaporgy) - MAXRADIUS);
      yh := MapBlockIntY(int64(tmbbox[BOXTOP]) - int64(bmaporgy) + MAXRADIUS);
    end
    else
    begin
      xl := MapBlockInt(tmbbox[BOXLEFT] - bmaporgx - MAXRADIUS);
      xh := MapBlockInt(tmbbox[BOXRIGHT] - bmaporgx + MAXRADIUS);
      yl := MapBlockInt(tmbbox[BOXBOTTOM] - bmaporgy - MAXRADIUS);
      yh := MapBlockInt(tmbbox[BOXTOP] - bmaporgy + MAXRADIUS);
    end;

    bx := xl;
    while bx <= xh do
    begin
      by := yl;
      while by <= yh do
      begin
        P_BlockThingsIterator(bx, by, PIT_PushThing);
        inc(by);
      end;
      inc(bx);
    end;

    exit;
  end;

  // constant pushers p_wind and p_current

  if sec.heightsec <> -1 then // special water sector?
    ht := sectors[sec.heightsec].floorheight
  else
    ht := 0;
  node := sec.touching_thinglist; // things touching this sector
  while node <> nil do
  begin
    thing := node.m_thing;
    if (thing.player = nil) or (thing.flags and (MF_NOGRAVITY or MF_NOCLIP) <> 0) then
    begin
      node := node.m_snext;
      continue;
    end;

    if p._type = p_wind then
    begin
      if sec.heightsec = -1 then // NOT special water sector
      begin
        if thing.z > thing.floorz then // above ground
        begin
          xspeed := p.x_mag; // full force
          yspeed := p.y_mag;
        end
        else // on ground
        begin
          xspeed := p.x_mag div 2; // half force
          yspeed := p.y_mag div 2;
        end;
      end
      else // special water sector
      begin
        if thing.z > ht then // above ground
        begin
          xspeed := p.x_mag; // full force
          yspeed := p.y_mag;
        end
        else if Pplayer_t(thing.player).viewz < ht then  // underwater
        begin
          xspeed := 0;
          yspeed := 0 // no force
        end
        else // wading in water
        begin
          xspeed := p.x_mag div 2; // half force
          yspeed := p.y_mag div 2;
        end;
      end;
    end
    else // p_current
    begin
      if sec.heightsec = -1 then // NOT special water sector
      begin
        if thing.z > sec.floorheight then // above ground
        begin
          xspeed := 0;
          yspeed := 0; // no force
        end
        else // on ground
        begin
          xspeed := p.x_mag; // full force
          yspeed := p.y_mag;
        end
      end
      else // special water sector
      begin
        if thing.z > ht then // above ground
        begin
          xspeed := 0;
          yspeed := 0; // no force
        end
        else // underwater
        begin
          xspeed := p.x_mag; // full force
          yspeed := p.y_mag;
        end;
      end;
    end;

    thing.momx := thing.momx + (xspeed * (1 shl (FRACBITS - PUSH_FACTOR)));
    thing.momy := thing.momy + (yspeed * (1 shl (FRACBITS - PUSH_FACTOR)));
    node := node.m_snext
  end;
end;



/////////////////////////////
//
// Add a push thinker to the thinker list

procedure Add_Pusher(_type: pushertype_e; x_mag, y_mag: integer; source: Pmobj_t; affectee :integer);
var
  p: Ppusher_t;
begin
  p := Z_Malloc(SizeOf(pusher_t), PU_LEVSPEC, nil);

  p.thinker._function.acp1 := @T_Pusher;
  p.source := source;
  p._type := _type;
  p.x_mag := x_mag div FRACUNIT;
  p.y_mag := y_mag div FRACUNIT;
  p.magnitude := P_AproxDistance(p.x_mag, p.y_mag);
  if source <> nil then // point source exist?
  begin
    p.radius := p.magnitude * (FRACUNIT * 2); // where force goes to zero
    p.x := p.source.x;
    p.y := p.source.y;
  end;
  p.affectee := affectee;
  P_AddThinker(@p.thinker);
end;

/////////////////////////////
//
// P_GetPushThing returns a pointer to an MT_PUSH or MT_PULL thing,
// nil otherwise.

function P_GetPushThing(const snum: integer): Pmobj_t;
var
  thing: Pmobj_t;
  sec: Psector_t;
begin
  sec := @sectors[snum];
  thing := sec.thinglist;
  while thing <> nil do
  begin
    case thing._type of
      Ord(MT_PUSH), Ord(MT_PULL):
        begin
          result := thing;
          exit;
        end;
    else
      thing := thing.snext;
    end;
  end;

  result := nil;
end;

/////////////////////////////
//
// Initialize the sectors where pushers are present
//

procedure P_SpawnPushers;
var
  l, le: Pline_t;
  s: integer;
  thing: Pmobj_t;
begin
  l := @lines[0];
  le := @lines[numlines];
  while l <> le do
  begin
    case l.special of
      224: // wind
        begin
          s := -1;
          while P_FindSectorFromLineTag2(l, s) >= 0 do
            Add_Pusher(p_wind, l.dx, l.dy, nil, s);
        end;
      225: // current
        begin
          s := -1;
          while P_FindSectorFromLineTag2(l, s) >= 0 do
            Add_Pusher(p_current, l.dx, l.dy, nil, s);
        end;
      226: // push/pull
        begin
          s := -1;
          while P_FindSectorFromLineTag2(l, s) >= 0 do
          begin
            thing := P_GetPushThing(s);
            if thing <> nil then // No MT_P* means no effect
              Add_Pusher(p_push, l.dx, l.dy, thing, s);
          end;
        end;
    end;
    inc(l);
  end;
end;

end.

