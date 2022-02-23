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
//  Map Objects, MObj, definition and handling.
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit p_mobj_h;

interface

uses
  m_fixed,
  info_h,
  doomdata,
  p_params,
  r_renderstyle,
  tables, // angle_t
  d_think; // We need the thinker_t stuff.

//
// NOTES: mobj_t
//
// mobj_ts are used to tell the refresh where to draw an image,
// tell the world simulation when objects are contacted,
// and tell the sound driver how to position a sound.
//
// The refresh uses the next and prev links to follow
// lists of things in sectors as they are being drawn.
// The sprite, frame, and angle elements determine which patch_t
// is used to draw the sprite if it is visible.
// The sprite and frame values are allmost allways set
// from state_t structures.
// The statescr.exe utility generates the states.h and states.c
// files that contain the sprite/frame numbers from the
// statescr.txt source file.
// The xyz origin point represents a point at the bottom middle
// of the sprite (between the feet of a biped).
// This is the default origin position for patch_ts grabbed
// with lumpy.exe.
// A walking creature will have its z equal to the floor
// it is standing on.
//
// The sound code uses the x,y, and subsector fields
// to do stereo positioning of any sound effited by the mobj_t.
//
// The play simulation uses the blocklinks, x,y,z, radius, height
// to determine when mobj_ts are touching each other,
// touching lines in the map, or hit by trace lines (gunshots,
// lines of sight, etc).
// The mobj_t->flags element has various bit flags
// used by the simulation.
//
// Every mobj_t is linked into a single sector
// based on its origin coordinates.
// The subsector_t is found with R_PointInSubsector(x,y),
// and the sector_t can be found with subsector->sector.
// The sector links are only used by the rendering code,
// the play simulation does not care about them at all.
//
// Any mobj_t that needs to be acted upon by something else
// in the play world (block movement, be shot, etc) will also
// need to be linked into the blockmap.
// If the thing has the MF_NOBLOCK flag set, it will not use
// the block links. It can still interact with other things,
// but only as the instigator (missiles will run into other
// things, but nothing can run into a missile).
// Each block in the grid is 128*128 units, and knows about
// every line_t that it contains a piece of, and every
// interactable mobj_t that has its origin contained.
//
// A valid mobj_t is a mobj_t that has the proper subsector_t
// filled in for its xy coordinates and is linked into the
// sector from which the subsector was made, or has the
// MF_NOSECTOR flag set (the subsector_t needs to be valid
// even if MF_NOSECTOR is set), and is linked into a blockmap
// block or has the MF_NOBLOCKMAP flag set.
// Links should only be modified by the P_[Un]SetThingPosition()
// functions.
// Do not change the MF_NO? flags while a thing is valid.
//
// Any questions?
//

//
// Misc. mobj flags
//

const
  // Call P_SpecialThing when touched.
  MF_SPECIAL = 1;
  // Blocks.
  MF_SOLID = 2;
  // Can be hit.
  MF_SHOOTABLE = 4;
  // Don't use the sector links (invisible but touchable).
  MF_NOSECTOR = 8;
  // Don't use the blocklinks (inert but displayable)
  MF_NOBLOCKMAP = 16;

  // villsa [STRIFE] Stand around until alerted
  MF_STAND = 32;
  // Will try to attack right back.
  MF_JUSTHIT = 64;
  // Will take at least one step before attacking.
  MF_JUSTATTACKED = 128;
  // On level spawning (initial position),
  //  hang from ceiling instead of stand on floor.
  MF_SPAWNCEILING = 256;
  // Don't apply gravity (every tic),
  //  that is, object will float, keeping current height
  //  or changing it actively.
  MF_NOGRAVITY = 512;

  // Movement flags.
  // This allows jumps from high places.
  MF_DROPOFF = $400;
  // villsa [STRIFE] For players, count as quest item
  MF_GIVEQUEST = $800;
  // Player cheat. ???
  MF_NOCLIP = $1000;
  // villsa [STRIFE] are feet clipped into water/slude floor?
  MF_FEETCLIPPED = $2000;
  // Allow moves to any height, no gravity.
  // For active floaters, e.g. cacodemons, pain elementals.
  MF_FLOAT = $4000;
  // villsa [STRIFE] can NPC talk?
  MF_NODIALOG = $8000;
  // Don't hit same species, explode on block.
  // Player missiles as well as fireballs of various kinds.
  MF_MISSILE = $10000;
  // Dropped by a demon, not level spawned.
  // E.g. ammo clips dropped by dying former humans.
  MF_DROPPED = $20000;
  // Use fuzzy draw (shadow demons or spectres),
  //  temporary player invisibility powerup.
  MF_SHADOW = $40000;
  // Flag: don't bleed when shot (use puff),
  //  barrels and shootable furniture shall not bleed.
  MF_NOBLOOD = $80000;
  // Don't stop moving halfway off a step,
  //  that is, have dead bodies slide down all the way.
  MF_CORPSE = $100000;
  // Floating to a height for a move, ???
  //  don't auto float to target's height.
  MF_INFLOAT = $200000;

  // On kill, count this enemy object
  //  towards intermission kill total.
  // Happy gathering.
  MF_COUNTKILL = $400000;

  // On picking up, count this item object
  //  towards intermission item total.
  MF_AMBUSH = $800000;

  // villsa [STRIFE] flag used for bouncing projectiles
  MF_BOUNCE = $1000000;

  // Don't spawn this object
  //  in death match mode (e.g. key cards).
  MF_NOTDMATCH = $2000000;

  // villsa [STRIFE] friendly towards player with matching flag
  MF_ALLY = $4000000;

  // villsa [STRIFE] 75% or 25% transparency? -- NEEDS VERIFICATION
  MF_MVIS = $8000000;

  // villsa [STRIFE] color translation
  MF_COLORSWAP1 = $10000000;

  // villsa [STRIFE] color translation
  MF_COLORSWAP2 = $20000000;

  // villsa [STRIFE] color translation
  MF_COLORSWAP3 = $40000000;

  // villsa [STRIFE] spectral entity, only damaged by spectral missiles
  MF_SPECTRAL = $80000000;

  // Player sprites in multiplayer modes are modified
  //  using an internal color lookup table for re-indexing.
  // haleyjd 09/06/10: redid for Strife translations
  MF_TRANSLATION = (MF_COLORSWAP1 or MF_COLORSWAP2 or MF_COLORSWAP3);

  // Turns 0x10000000 into 0x01 to get a translation index.
  // villsa [STRIFE] change from 26 to 28
  MF_TRANSSHIFT       = 28;

const
  // Sprite is transparent
  MF_EX_TRANSPARENT = 1;
  // Sprite emits white light
  MF_EX_WHITELIGHT = 2;
  // Sprite emits red light
  MF_EX_REDLIGHT = 4;
  // Sprite emits green light
  MF_EX_GREENLIGHT = 8;
  // Sprite emits blue light
  MF_EX_BLUELIGHT = $10;
  // Sprite emits yellow light
  MF_EX_YELLOWLIGHT = $20;
  // Compination of above flags
  MF_EX_LIGHT = MF_EX_WHITELIGHT or MF_EX_REDLIGHT or MF_EX_GREENLIGHT or MF_EX_BLUELIGHT or MF_EX_YELLOWLIGHT;
  MF_EX_STATE_MASK = MF_EX_LIGHT or MF_EX_TRANSPARENT;

  // State flags
  MF_EX_STATE_RANDOM_SELECT = $40;
  MF_EX_STATE_RANDOM_RANGE = $80;
  MF_EX_STATE_PARAMS_ERROR = $100;
  MF_EX_STATE_ARGS_CHECKED = $200;
  MF_EX_STATE_ARGS_ERROR = $400;

  // Float BOB
  MF_EX_FLOATBOB = $40;
  // No Radius Damage
  MF_EX_NORADIUSDMG = $80;
  // Half the damage
  MF_EX_FIRERESIST = $100;
  // Random see sound
  MF_EX_RANDOMSEESOUND = $200;
  // Random pain sound
  MF_EX_RANDOMPAINSOUND = $400;
  // Random attack sound
  MF_EX_RANDOMATTACKSOUND = $800;
  // Random death sound
  MF_EX_RANDOMDEATHSOUND = $1000;
  // Random active sound
  MF_EX_RANDOMACTIVESOUND = $2000;
  // Random customsound1
  MF_EX_RANDOMCUSTOMSOUND1 = $4000;
  // Random customsound2
  MF_EX_RANDOMCUSTOMSOUND2 = $8000;
  // Random customsound3
  MF_EX_RANDOMCUSTOMSOUND3 = $10000;
  // Custom Explosion (see A_Explode)
  MF_EX_CUSTOMEXPLODE = $20000;
  // The think is boss
  MF_EX_BOSS = $40000;
  // Missile stays on floor
  MF_EX_FLOORHUGGER = $80000;
  // Missile stays on ceiling
  MF_EX_CEILINGHUGGER = $100000;
  // Missile is a seeker
  MF_EX_SEEKERMISSILE = $200000;
  // Mobj spawn on random float z
  MF_EX_SPAWNFLOAT = $400000;
  // Don't hurt one's own kind with explosions
  MF_EX_DONTHURTSPECIES = $800000;
  // Low gravity
  MF_EX_LOWGRAVITY = $1000000;
  // Invulnerable
  MF_EX_INVULNERABLE = $2000000;
  // Random meleesound
  MF_EX_RANDOMMELEESOUND = $4000000;
  // Don't remove missile
  MF_EX_DONOTREMOVE = $8000000;
  // Thing is a ghost
  MF_EX_GHOST = $10000000;
  // Pass thru ghosts
  MF_EX_THRUGHOST = $20000000;
  // Monster can look all around, has eyes in the back
  MF_EX_LOOKALLAROUND = $40000000;

const
  // Medium gravity
  MF2_EX_MEDIUMGRAVITY = 1;
  // No P_HitFloor call
  MF2_EX_NOHITFLOOR = 2;
  // Green Blood
  MF2_EX_GREENBLOOD = 4;
  // Blue Blood
  MF2_EX_BLUEBLOOD = 8;
  // No teleport flag
  MF2_EX_NOTELEPORT = $10;
  // Pushable things
  MF2_EX_PUSHABLE = $20;
  // Mobj can not push
  MF2_EX_CANNOTPUSH = $40;
  // don't generate a vissprite
  MF2_EX_DONTDRAW = $80;
  // Interactive mobj - can be set to interactstate
  MF2_EX_INTERACTIVE = $100;
  // Don't return fire if attacker has same inheritance
  MF2_EX_DONTINFIGHTMONSTERS = $200;
  // Monster walks away
  MF2_EX_FRIGHTENED = $400;
  // Just appeard - new spawn monsters or just teleported monsters
  MF2_EX_JUSTAPPEARED = $800;
  // No damage
  MF2_EX_NODAMAGE = $1000;
  // Do not run scripts
  MF2_EX_DONTRUNSCRIPTS = $2000;
  // Use precise spawnangle
  MF2_EX_PRECISESPAWNANGLE = $4000;
  // Use custom dropitem
  MF2_EX_CUSTOMDROPITEM = $8000;
  // Can not go to sector with different floor picture
  MF2_EX_CANTLEAVEFLOORPIC = $10000;
  // From zDoom wiki:
  // Actors with this flag set will often (about 92% chance) decide to jump down
  // tall ledges to pursue their target, if said target is hostile
  MF2_EX_JUMPDOWN = $20000;
  // Sound at full volume
  MF2_EX_FULLVOLACTIVE = $40000;
  MF2_EX_FULLVOLDEATH = $80000;
  MF2_EX_FULLVOLSEE = $100000;
  MF2_EX_FULLVOLPAIN = $200000;
  MF2_EX_FULLVOLATTACK = $400000;
  // Do not render shadow
  MF2_EX_DONOTRENDERSHADOW = $800000;
  // See invisible player
  MF2_EX_SEEINVISIBLE = $1000000;
  // Missile hurts same species
  MF2_EX_MISSILEHURTSPECIES = $2000000;
  // Actor can jump up
  MF2_EX_JUMPUP = $4000000;
  // Does not block player
  MF2_EX_DONTBLOCKPLAYER = $8000000;
  // Friendly monsters
  MF2_EX_FRIEND = $10000000;

const
  // Bounce on floor
  MF3_EX_FLOORBOUNCE = 1;
  // Bounce on ceiling
  MF3_EX_CEILINGBOUNCE = 2;
  // Bounce on walls
  MF3_EX_WALLBOUNCE = 4;
  // All bounce flags
  MF3_EX_BOUNCE = MF3_EX_FLOORBOUNCE or MF3_EX_CEILINGBOUNCE or MF3_EX_WALLBOUNCE;
  // No max move
  MF3_EX_NOMAXMOVE = 8;
  // No Crush
  MF3_EX_NOCRUSH = 16;
  // Blood do not check damage
  MF3_EX_BLOODIGNOREDAMAGE = 32;
  // No render interpolation
  MF3_EX_NORENDERINTERPOLATION = 64;
  // Line Activate (MBF)
  MF3_EX_LINEDONE = 128;
  // Flip sprite
  MF3_EX_FLIPSPRITE = 256;
  // Half distance to missile
  MF3_EX_MISSILEMORE = $200;
  // 1/8th distance to missile
  MF3_EX_MISSILEEVENMORE = $400;
  // Missiles use alternate damage (Doom)
  MF3_EX_DOOMDAMAGE = $800;
  // Continue on freeze console command
  MF3_EX_NOTIMEFREEZE = $1000;
  // Not scared of frightening
  MF3_EX_NOFEAR = $2000;
  // Cause fear if targeted
  MF3_EX_CAUSEFEAR = $4000;
  // Actor slides against walls
  MF3_EX_SLIDING = $8000;
  // No actor collision
  MF3_EX_THRUACTORS = $10000;
  // No collision with same species (also inheritance)
  MF3_EX_THRUSPECIES = $20000;
  //  Can cross ML_BLOCKMONSTERS lines
  MF3_EX_NOBLOCKMONST = $40000;
  // Do not subject actor to player autoaim
  MF3_EX_NOTAUTOAIMED = $80000;
  // Actor slides against walls alias
  MF3_EX_SLIDE = $100000;
  // Actor slides against walls alias
  MF3_EX_SLIDEONWALLS = $200000;
  // Missiles use absolute damage
  MF3_EX_ABSOLUTEDAMAGE = $400000;
  // Do not fall to ground on death
  MF3_EX_SETGRAVITYONDEATH = $800000;
  // Missile causes freeze damage
  MF3_EX_FREEZEDAMAGE = $1000000;
  // Actor is not hurt by freeze damage missile
  MF3_EX_NOFREEZEDAMAGE = $2000000;
  // Actor is hurt less by freeze damage missile
  MF3_EX_FREEZEDAMAGERESIST = $4000000;
  // Missile causes flame damage
  MF3_EX_FLAMEDAMAGE = $8000000;
  // Actor is not hurt by flame damage missile
  MF3_EX_NOFLAMEDAMAGE = $10000000;
  // Melee attach check z positioning
  MF3_EX_MELEECHECKZ = $20000000;

const
  // Actor is hurt less by flame damage missile
  MF4_EX_FLAMEDAMAGERESIST = 1;
  // No monster collision
  MF4_EX_THRUMONSTERS = 2;
  // Drop item in actual z
  MF4_EX_ABSOLUTEDROPITEMPOS = 4;
  // Can not step up
  MF4_EX_CANNOTSTEP = 8;
  // Can not drop off
  MF4_EX_CANNOTDROPOFF = $10;
  // Force radius damage
  MF4_EX_FORCERADIUSDMG = $20;
  // short missile range (14 * 64)
  MF4_EX_SHORTMRANGE = $40;
  // other things ignore its attacks
  MF4_EX_DMGIGNORED = $80;
  // higher missile attack probability
  MF4_EX_HIGHERMPROB = $100;
  // use half distance for missile attack probability
  MF4_EX_RANGEHALF = $200;
  // no targeting threshold
  MF4_EX_NOTHRESHOLD = $400;
  // long melee range
  MF4_EX_LONGMELEERANGE = $800;
  // projectile rips through targets
  MF4_EX_RIP = $1000;
  // Follow trace
  MF4_EX_TRACEDEFINED = $2000;
  // Tag 666 "boss" on doom 2 map 7
  MF4_EX_MAP07BOSS1 = $4000;
  // Tag 667 "boss" on doom 2 map 7
  MF4_EX_MAP07BOSS2 = $8000;
  // Tag 666 or 667 "boss" on doom 2 map 7
  MF4_EX_MAP07BOSS = MF4_EX_MAP07BOSS1 or MF4_EX_MAP07BOSS2;
  // Self applying lighmap
  MF4_EX_SELFAPPLYINGLIGHT = $10000;
  // Full volume rip sound
  MF4_EX_FULLVOLRIP = $20000;
  // Random rip sound
  MF4_EX_RANDOMRIPSOUND = $40000;
  // Ignore full_sounds console variable and always finishes sounds
  MF4_EX_ALWAYSFINISHSOUND = $80000;
  // Ignore full_sounds console variable and never finishes sounds
  MF4_EX_NEVERFINISHSOUND = $100000;
  // Do not gib
  MF4_EX_DONTGIB = $200000;
  // Backing in melee attack
  MF4_EX_BACKINGMELEE = $400000;
  // Thrusted by the wind
  MF4_EX_WINDTHRUST = $800000;

const
  // Monster can push walls
  MF5_EX_PUSHWALL = $1;
  // Monster can activate cross lines
  MF5_EX_MCROSS = $2;
  // Projectile can activate cross line
  MF5_EX_PCROSS = $4;
  // Projectile activate impact walls
  MF5_EX_IMPACT = $8;

type
// Map Object definition.
//
// [STRIFE]: Amazingly, only one modification was made to mobj_t over DOOM
// 1.666, and that was the addition of the single-byte allegiance field for
// tracking with which player friendly monsters are allied.
//

  Pmobj_t = ^mobj_t;
  mobj_t = record
    // List: thinker links.
    thinker: thinker_t;

    // Info for drawing: position.
    x: fixed_t;
    y: fixed_t;
    z: fixed_t;

    // More list: links in sector (if needed)
    snext: Pmobj_t;
    sprev: Pmobj_t;

    //More drawing info: to determine current sprite.
    angle: angle_t;     // orientation
    viewangle: angle_t; // JVAL Turn head direction
    sprite: integer;// used to find patch_t and flip value
    frame: integer; // might be ORed with FF_FULLBRIGHT

    // Interaction info, by BLOCKMAP.
    // Links in blocks (if needed).
    bpos: integer;
    bidx: integer;

    subsector: pointer; //Psubsector_t;

    // The closest interval over all contacted Sectors.
    floorz: fixed_t;
    ceilingz: fixed_t;

    // For movement checking.
    radius: fixed_t;
    height: fixed_t;

    // Momentums, used to update position.
    momx: fixed_t;
    momy: fixed_t;
    momz: fixed_t;

    // If == validcount, already checked.
    validcount: integer;

    _type: integer;
    info: Pmobjinfo_t;    // &mobjinfo[mobj->type]

    tics: integer;        // state tic counter
    state: Pstate_t;
    prevstate: Pstate_t;
    flags: LongWord;
    flags_ex: LongWord;   // JVAL extended flags (MF_EX_????)
    flags2_ex: LongWord;  // JVAL extended flags (MF_EX_????)
    renderstyle: mobjrenderstyle_t;
    alpha: fixed_t;
    bob: integer;

    health: integer;

    // Movement direction, movement generation (zig-zagging).
    movedir: integer;   // 0-7
    movecount: integer; // when 0, select a new dir

    // Thing being chased/attacked (or NULL),
    // also the originator for missiles.
    target: Pmobj_t;

    // Reaction time: if non 0, don't attack yet.
    // Used by player to freeze a bit after teleporting.
    reactiontime: integer;

    // If >0, the target will be chased
    // no matter what (even if shot)
    threshold: integer;

    // Additional info record for player avatars only.
    // Only valid if type == MT_PLAYER
    player: pointer;  // Pplayer_t;

    // Player number last looked for.
    lastlook: integer;

    // For nightmare respawn.
    spawnpoint: mapthing_t;

    // Thing being chased/attacked for tracers.
    tracer: Pmobj_t;

    fastchasetics: integer;

    // Friction values for the sector the object is in
    friction: integer;  // phares 3/17/98
    movefactor: integer;

    // a linked list of sectors where this object appears
    touching_sectorlist: pointer; {Pmsecnode_t} // phares 3/14/98

    // JVAL: User defined parameters (eg custom Inventory)
    customparams: Pmobjcustomparam_t;

    // JVAL: Unique key
    key: LongWord;

    floorclip: fixed_t;

    // [STRIFE] haleyjd 09/05/10:
    // * In multiplayer this stores allegiance, for friends and teleport beacons
    // * In single-player this tracks dialog state.
    miscdata: byte;

    // JVAL: Interpolation
    prevx: fixed_t;
    prevy: fixed_t;
    prevz: fixed_t;
    prevangle: angle_t;

    nextx: fixed_t;
    nexty: fixed_t;
    nextz: fixed_t;
    nextangle: angle_t;

    intrplcnt: LongWord;

    dropitem: integer;

    // version 205
    lightvalidcount: integer;
    scale: integer;
    pushfactor: integer;
    gravity: integer;
    flags3_ex: integer;  // JVAL extended flags (MF3_EX_????)
    flags4_ex: integer;  // JVAL extended flags (MF4_EX_????)
    rendervalidcount: integer;

    // version 206
    mass: integer;
    args: array[0..4] of integer;
    special: integer;
    master: Pmobj_t;
    WeaveIndexXY: integer;
    WeaveIndexZ: integer;

    // version 207
    painchance: integer;
    spriteDX: integer;
    spriteDY: integer;
    flags5_ex: integer;  // JVAL extended flags (MF5_EX_????)
    flags6_ex: integer;  // JVAL extended flags (MF6_EX_????)
    playerfollowtime: integer;      // JVAL 20211224 - Dogs follow player
    tracefollowtimestamp: integer;  // JVAL 20211224 - Dogs follow player
    tracex: integer;
    tracey: integer;
    tracez: integer;
    // mbf21+
    infighting_group: integer;
    projectile_group: integer;
    splash_group: integer;
    strafecount: integer;
    bloodcolor: integer;
    translationname: string[8];
    translationtable: Pointer;
    tid: integer;
  end;
  Tmobj_tPArray = array[0..$FFFF] of Pmobj_t;
  Pmobj_tPArray = ^Tmobj_tPArray;

  Pmobj_t120 = ^mobj_t120;
  mobj_t120 = record
    // List: thinker links.
    thinker: thinker_t;

    // Info for drawing: position.
    x: fixed_t;
    y: fixed_t;
    z: fixed_t;

    // More list: links in sector (if needed)
    snext: Pmobj_t;
    sprev: Pmobj_t;

    //More drawing info: to determine current sprite.
    angle: angle_t;       // orientation
    viewangle: angle_t;   // JVAL Turn head direction
    sprite: integer;// used to find patch_t and flip value
    frame: integer; // might be ORed with FF_FULLBRIGHT

    // Interaction info, by BLOCKMAP.
    // Links in blocks (if needed).
    bpos: integer;
    bidx: integer;

    subsector: pointer; //Psubsector_t;

    // The closest interval over all contacted Sectors.
    floorz: fixed_t;
    ceilingz: fixed_t;

    // For movement checking.
    radius: fixed_t;
    height: fixed_t;

    // Momentums, used to update position.
    momx: fixed_t;
    momy: fixed_t;
    momz: fixed_t;

    // If == validcount, already checked.
    validcount: integer;

    _type: integer;
    info: Pmobjinfo_t;    // &mobjinfo[mobj->type]

    tics: integer;        // state tic counter
    state: Pstate_t;
    prevstate: Pstate_t;
    flags: LongWord;
    flags_ex: LongWord;   // JVAL extended flags (MF_EX_????)
    flags2_ex: LongWord;  // JVAL extended flags (MF_EX_????)
    renderstyle: mobjrenderstyle_t;
    alpha: fixed_t;
    bob: integer;

    health: integer;

    // Movement direction, movement generation (zig-zagging).
    movedir: integer; // 0-7
    movecount: integer; // when 0, select a new dir

    // Thing being chased/attacked (or NULL),
    // also the originator for missiles.
    target: Pmobj_t;

    // Reaction time: if non 0, don't attack yet.
    // Used by player to freeze a bit after teleporting.
    reactiontime: integer;

    // If >0, the target will be chased
    // no matter what (even if shot)
    threshold: integer;

    // Additional info record for player avatars only.
    // Only valid if type == MT_PLAYER
    player: pointer; //Pplayer_t;

    // Player number last looked for.
    lastlook: integer;

    // For nightmare respawn.
    spawnpoint: mapthing_t;

    // Thing being chased/attacked for tracers.
    tracer: Pmobj_t;

    fastchasetics: integer;

    // Friction values for the sector the object is in
    friction: integer;  // phares 3/17/98
    movefactor: integer;

    // a linked list of sectors where this object appears
    touching_sectorlist: pointer; {Pmsecnode_t} // phares 3/14/98

    // JVAL: User defined parameters (eg custom Inventory)
    customparams: Pmobjcustomparam_t;

    // JVAL: Unique key
    key: LongWord;

    floorclip: fixed_t;

    // [STRIFE] haleyjd 09/05/10:
    // * In multiplayer this stores allegiance, for friends and teleport beacons
    // * In single-player this tracks dialog state.
    miscdata: byte;
  end;

  Pmobj_t121 = ^mobj_t121;
  mobj_t121 = record
    // List: thinker links.
    thinker: thinker_t;

    // Info for drawing: position.
    x: fixed_t;
    y: fixed_t;
    z: fixed_t;

    // More list: links in sector (if needed)
    snext: Pmobj_t;
    sprev: Pmobj_t;

    //More drawing info: to determine current sprite.
    angle: angle_t;       // orientation
    viewangle: angle_t;   // JVAL Turn head direction
    sprite: integer;// used to find patch_t and flip value
    frame: integer; // might be ORed with FF_FULLBRIGHT

    // Interaction info, by BLOCKMAP.
    // Links in blocks (if needed).
    bpos: integer;
    bidx: integer;

    subsector: pointer; //Psubsector_t;

    // The closest interval over all contacted Sectors.
    floorz: fixed_t;
    ceilingz: fixed_t;

    // For movement checking.
    radius: fixed_t;
    height: fixed_t;

    // Momentums, used to update position.
    momx: fixed_t;
    momy: fixed_t;
    momz: fixed_t;

    // If == validcount, already checked.
    validcount: integer;

    _type: integer;
    info: Pmobjinfo_t;    // &mobjinfo[mobj->type]

    tics: integer;        // state tic counter
    state: Pstate_t;
    prevstate: Pstate_t;
    flags: LongWord;
    flags_ex: LongWord;   // JVAL extended flags (MF_EX_????)
    flags2_ex: LongWord;  // JVAL extended flags (MF_EX_????)
    renderstyle: mobjrenderstyle_t;
    alpha: fixed_t;
    bob: integer;

    health: integer;

    // Movement direction, movement generation (zig-zagging).
    movedir: integer; // 0-7
    movecount: integer; // when 0, select a new dir

    // Thing being chased/attacked (or NULL),
    // also the originator for missiles.
    target: Pmobj_t;

    // Reaction time: if non 0, don't attack yet.
    // Used by player to freeze a bit after teleporting.
    reactiontime: integer;

    // If >0, the target will be chased
    // no matter what (even if shot)
    threshold: integer;

    // Additional info record for player avatars only.
    // Only valid if type == MT_PLAYER
    player: pointer; //Pplayer_t;

    // Player number last looked for.
    lastlook: integer;

    // For nightmare respawn.
    spawnpoint: mapthing_t;

    // Thing being chased/attacked for tracers.
    tracer: Pmobj_t;

    fastchasetics: integer;

    // Friction values for the sector the object is in
    friction: integer;  // phares 3/17/98
    movefactor: integer;

    // a linked list of sectors where this object appears
    touching_sectorlist: pointer; {Pmsecnode_t} // phares 3/14/98

    // JVAL: User defined parameters (eg custom Inventory)
    customparams: Pmobjcustomparam_t;

    // JVAL: Unique key
    key: LongWord;

    floorclip: fixed_t;

    // [STRIFE] haleyjd 09/05/10:
    // * In multiplayer this stores allegiance, for friends and teleport beacons
    // * In single-player this tracks dialog state.
    miscdata: byte;

    // JVAL: Interpolation
    prevx: fixed_t;
    prevy: fixed_t;
    prevz: fixed_t;
    prevangle: angle_t;

    nextx: fixed_t;
    nexty: fixed_t;
    nextz: fixed_t;
    nextangle: angle_t;

    intrplcnt: LongWord;
  end;

  Pmobj_t204 = ^mobj_t204;
  mobj_t204 = record
    // List: thinker links.
    thinker: thinker_t;

    // Info for drawing: position.
    x: fixed_t;
    y: fixed_t;
    z: fixed_t;

    // More list: links in sector (if needed)
    snext: Pmobj_t;
    sprev: Pmobj_t;

    //More drawing info: to determine current sprite.
    angle: angle_t;       // orientation
    viewangle: angle_t;   // JVAL Turn head direction
    sprite: integer;// used to find patch_t and flip value
    frame: integer; // might be ORed with FF_FULLBRIGHT

    // Interaction info, by BLOCKMAP.
    // Links in blocks (if needed).
    bpos: integer;
    bidx: integer;

    subsector: pointer; //Psubsector_t;

    // The closest interval over all contacted Sectors.
    floorz: fixed_t;
    ceilingz: fixed_t;

    // For movement checking.
    radius: fixed_t;
    height: fixed_t;

    // Momentums, used to update position.
    momx: fixed_t;
    momy: fixed_t;
    momz: fixed_t;

    // If == validcount, already checked.
    validcount: integer;

    _type: integer;
    info: Pmobjinfo_t;    // &mobjinfo[mobj->type]

    tics: integer;        // state tic counter
    state: Pstate_t;
    prevstate: Pstate_t;
    flags: LongWord;
    flags_ex: LongWord;   // JVAL extended flags (MF_EX_????)
    flags2_ex: LongWord;  // JVAL extended flags (MF_EX_????)
    renderstyle: mobjrenderstyle_t;
    alpha: fixed_t;
    bob: integer;

    health: integer;

    // Movement direction, movement generation (zig-zagging).
    movedir: integer; // 0-7
    movecount: integer; // when 0, select a new dir

    // Thing being chased/attacked (or NULL),
    // also the originator for missiles.
    target: Pmobj_t;

    // Reaction time: if non 0, don't attack yet.
    // Used by player to freeze a bit after teleporting.
    reactiontime: integer;

    // If >0, the target will be chased
    // no matter what (even if shot)
    threshold: integer;

    // Additional info record for player avatars only.
    // Only valid if type == MT_PLAYER
    player: pointer; //Pplayer_t;

    // Player number last looked for.
    lastlook: integer;

    // For nightmare respawn.
    spawnpoint: mapthing_t;

    // Thing being chased/attacked for tracers.
    tracer: Pmobj_t;

    fastchasetics: integer;

    // Friction values for the sector the object is in
    friction: integer;  // phares 3/17/98
    movefactor: integer;

    // a linked list of sectors where this object appears
    touching_sectorlist: pointer; {Pmsecnode_t} // phares 3/14/98

    // JVAL: User defined parameters (eg custom Inventory)
    customparams: Pmobjcustomparam_t;

    // JVAL: Unique key
    key: LongWord;

    floorclip: fixed_t;

    // [STRIFE] haleyjd 09/05/10:
    // * In multiplayer this stores allegiance, for friends and teleport beacons
    // * In single-player this tracks dialog state.
    miscdata: byte;

    // JVAL: Interpolation
    prevx: fixed_t;
    prevy: fixed_t;
    prevz: fixed_t;
    prevangle: angle_t;

    nextx: fixed_t;
    nexty: fixed_t;
    nextz: fixed_t;
    nextangle: angle_t;

    intrplcnt: LongWord;

    dropitem: integer;
  end;

  Pmobj_t205 = ^mobj_t205;
  mobj_t205 = record
    // List: thinker links.
    thinker: thinker_t;

    // Info for drawing: position.
    x: fixed_t;
    y: fixed_t;
    z: fixed_t;

    // More list: links in sector (if needed)
    snext: Pmobj_t;
    sprev: Pmobj_t;

    //More drawing info: to determine current sprite.
    angle: angle_t;     // orientation
    viewangle: angle_t; // JVAL Turn head direction
    sprite: integer;// used to find patch_t and flip value
    frame: integer; // might be ORed with FF_FULLBRIGHT

    // Interaction info, by BLOCKMAP.
    // Links in blocks (if needed).
    bpos: integer;
    bidx: integer;

    subsector: pointer; //Psubsector_t;

    // The closest interval over all contacted Sectors.
    floorz: fixed_t;
    ceilingz: fixed_t;

    // For movement checking.
    radius: fixed_t;
    height: fixed_t;

    // Momentums, used to update position.
    momx: fixed_t;
    momy: fixed_t;
    momz: fixed_t;

    // If == validcount, already checked.
    validcount: integer;

    _type: integer;
    info: Pmobjinfo_t;    // &mobjinfo[mobj->type]

    tics: integer;        // state tic counter
    state: Pstate_t;
    prevstate: Pstate_t;
    flags: LongWord;
    flags_ex: LongWord;   // JVAL extended flags (MF_EX_????)
    flags2_ex: LongWord;  // JVAL extended flags (MF_EX_????)
    renderstyle: mobjrenderstyle_t;
    alpha: fixed_t;
    bob: integer;

    health: integer;

    // Movement direction, movement generation (zig-zagging).
    movedir: integer;   // 0-7
    movecount: integer; // when 0, select a new dir

    // Thing being chased/attacked (or NULL),
    // also the originator for missiles.
    target: Pmobj_t;

    // Reaction time: if non 0, don't attack yet.
    // Used by player to freeze a bit after teleporting.
    reactiontime: integer;

    // If >0, the target will be chased
    // no matter what (even if shot)
    threshold: integer;

    // Additional info record for player avatars only.
    // Only valid if type == MT_PLAYER
    player: pointer;  // Pplayer_t;

    // Player number last looked for.
    lastlook: integer;

    // For nightmare respawn.
    spawnpoint: mapthing_t;

    // Thing being chased/attacked for tracers.
    tracer: Pmobj_t;

    fastchasetics: integer;

    // Friction values for the sector the object is in
    friction: integer;  // phares 3/17/98
    movefactor: integer;

    // a linked list of sectors where this object appears
    touching_sectorlist: pointer; {Pmsecnode_t} // phares 3/14/98

    // JVAL: User defined parameters (eg custom Inventory)
    customparams: Pmobjcustomparam_t;

    // JVAL: Unique key
    key: LongWord;

    floorclip: fixed_t;

    // [STRIFE] haleyjd 09/05/10:
    // * In multiplayer this stores allegiance, for friends and teleport beacons
    // * In single-player this tracks dialog state.
    miscdata: byte;

    // JVAL: Interpolation
    prevx: fixed_t;
    prevy: fixed_t;
    prevz: fixed_t;
    prevangle: angle_t;

    nextx: fixed_t;
    nexty: fixed_t;
    nextz: fixed_t;
    nextangle: angle_t;

    intrplcnt: LongWord;

    dropitem: integer;

    // version 205
    lightvalidcount: integer;
    scale: integer;
    pushfactor: integer;
    gravity: integer;
    flags3_ex: integer;  // JVAL extended flags (MF3_EX_????)
    flags4_ex: integer;  // JVAL extended flags (MF4_EX_????)
    rendervalidcount: integer;
  end;

  Pmobj_t206 = ^mobj_t206;
  mobj_t206 = record
    // List: thinker links.
    thinker: thinker_t;

    // Info for drawing: position.
    x: fixed_t;
    y: fixed_t;
    z: fixed_t;

    // More list: links in sector (if needed)
    snext: Pmobj_t;
    sprev: Pmobj_t;

    //More drawing info: to determine current sprite.
    angle: angle_t;     // orientation
    viewangle: angle_t; // JVAL Turn head direction
    sprite: integer;// used to find patch_t and flip value
    frame: integer; // might be ORed with FF_FULLBRIGHT

    // Interaction info, by BLOCKMAP.
    // Links in blocks (if needed).
    bpos: integer;
    bidx: integer;

    subsector: pointer; //Psubsector_t;

    // The closest interval over all contacted Sectors.
    floorz: fixed_t;
    ceilingz: fixed_t;

    // For movement checking.
    radius: fixed_t;
    height: fixed_t;

    // Momentums, used to update position.
    momx: fixed_t;
    momy: fixed_t;
    momz: fixed_t;

    // If == validcount, already checked.
    validcount: integer;

    _type: integer;
    info: Pmobjinfo_t;    // &mobjinfo[mobj->type]

    tics: integer;        // state tic counter
    state: Pstate_t;
    prevstate: Pstate_t;
    flags: LongWord;
    flags_ex: LongWord;   // JVAL extended flags (MF_EX_????)
    flags2_ex: LongWord;  // JVAL extended flags (MF_EX_????)
    renderstyle: mobjrenderstyle_t;
    alpha: fixed_t;
    bob: integer;

    health: integer;

    // Movement direction, movement generation (zig-zagging).
    movedir: integer;   // 0-7
    movecount: integer; // when 0, select a new dir

    // Thing being chased/attacked (or NULL),
    // also the originator for missiles.
    target: Pmobj_t;

    // Reaction time: if non 0, don't attack yet.
    // Used by player to freeze a bit after teleporting.
    reactiontime: integer;

    // If >0, the target will be chased
    // no matter what (even if shot)
    threshold: integer;

    // Additional info record for player avatars only.
    // Only valid if type == MT_PLAYER
    player: pointer;  // Pplayer_t;

    // Player number last looked for.
    lastlook: integer;

    // For nightmare respawn.
    spawnpoint: mapthing_t;

    // Thing being chased/attacked for tracers.
    tracer: Pmobj_t;

    fastchasetics: integer;

    // Friction values for the sector the object is in
    friction: integer;  // phares 3/17/98
    movefactor: integer;

    // a linked list of sectors where this object appears
    touching_sectorlist: pointer; {Pmsecnode_t} // phares 3/14/98

    // JVAL: User defined parameters (eg custom Inventory)
    customparams: Pmobjcustomparam_t;

    // JVAL: Unique key
    key: LongWord;

    floorclip: fixed_t;

    // [STRIFE] haleyjd 09/05/10:
    // * In multiplayer this stores allegiance, for friends and teleport beacons
    // * In single-player this tracks dialog state.
    miscdata: byte;

    // JVAL: Interpolation
    prevx: fixed_t;
    prevy: fixed_t;
    prevz: fixed_t;
    prevangle: angle_t;

    nextx: fixed_t;
    nexty: fixed_t;
    nextz: fixed_t;
    nextangle: angle_t;

    intrplcnt: LongWord;

    dropitem: integer;

    // version 205
    lightvalidcount: integer;
    scale: integer;
    pushfactor: integer;
    gravity: integer;
    flags3_ex: integer;  // JVAL extended flags (MF3_EX_????)
    flags4_ex: integer;  // JVAL extended flags (MF4_EX_????)
    rendervalidcount: integer;

    // version 206
    mass: integer;
    args: array[0..4] of integer;
    special: integer;
    master: Pmobj_t;
    WeaveIndexXY: integer;
    WeaveIndexZ: integer;
  end;

var
  spawnrandommonsters: boolean = false;

implementation

end.
