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
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit d_player;

interface

uses
// The player data structure depends on a number
// of other structs: items (internal inventory),
// animation states (closely tied to the sprites
// used to represent them, unfortunately).
  p_pspr_h,
// In addition, the player is just a special
// case of the generic moving object/actor.
  p_mobj_h,
// Finally, for odd reasons, the player input
// is buffered within the player data struct,
// as commands per game tick.
  d_ticcmd,
  m_fixed,
  doomdef;

//
// Player states.
//

type
  playerstate_t = (
  // Playing or camping.
    PST_LIVE,
  // Dead on the ground, view follows killer.
    PST_DEAD,
  // Ready to restart/respawn???
    PST_REBORN);

//
// Player internal flags, for cheats and debug.
//

const
  // No clipping, walk through barriers.
  CF_NOCLIP = 1;
  // No damage, no health loss.
  CF_GODMODE = 2;
  // Not really a cheat, just a debug aid.
  CF_NOMOMENTUM = 4;
  // villsa [STRIFE] new cheat
  // set when on fire and disable inventory
  CF_ONFIRE = 8;
  // villsa [STRIFE] new cheat
  // auto-use medkits
  CF_AUTOHEALTH = 16;
  // Low gravity cheat
  CF_LOWGRAVITY = 32;

// haleyjd 08/30/10: [STRIFE]
// Player Inventory Item Structure
const
  NUMINVENTORY = 32;

type
  inventory_t = record
    sprite: integer;  // a sprite number
    _type: integer;   // a thing type
    amount: integer;  // amount being carried
  end;
  Pinventory_t = ^inventory_t;
  inventory_a = array [0..NUMINVENTORY - 1] of inventory_t;
  Pinventory_a = ^inventory_a;


type
//
// Extended player object info: player_t
//
  player_t = record
    mo: Pmobj_t;
    playerstate: playerstate_t;
    cmd202: ticcmd_t202;

    // Determine POV,
    //  including viewpoint bobbing during movement.
    // Focal origin above r.z
    viewz: fixed_t;
    // Base height above floor for viewz.
    viewheight: fixed_t;
    // Bob/squat speed.
    deltaviewheight: fixed_t;
    // bounded/scaled total momentum.
    bob: fixed_t;

    // Look UP/DOWN support
    lookdir: integer;
    centering: boolean;
    // Look LEFT/RIGHT support
    lookdir2: byte;
    oldlook2: integer;
    forwarding: boolean;

    // jump
    oldjump: integer;

    // This is only used between levels,
    // mo->health is used during levels.
    health: integer;
    armorpoints: integer;
    // Armor type is 0-2.
    armortype: integer;

    // Power ups. invinc and invis are tic counters.
    powers: array[0..Ord(NUMPOWERS) - 1] of integer;

    // [STRIFE] Additions:
    sigiltype: integer;               // Type of Sigil carried
    nukagecount: integer;             // Nukage exposure counter
    questflags: LongWord;             // Quest bit flags
    centerview: boolean;              // True if view should be centered
    inventory: inventory_a;           // Player inventory items
    st_update: boolean;               // If true, update status bar
    numinventory: integer;            // Num. active inventory items
    inventorycursor: integer;         // Selected inventory item
    accuracy: integer;                // Accuracy stat
    stamina: integer;                 // Stamina stat

    cards: array[0..Ord(NUMCARDS) - 1] of boolean;
    backpack: boolean;

    // Frags, kills of other players.
    frags: array[0..MAXPLAYERS - 1] of integer;
    readyweapon: weapontype_t;

    // Is wp_nochange if not changing.
    pendingweapon: weapontype_t;

    weaponowned: array[0..Ord(NUMWEAPONS) - 1] of boolean;
    ammo: array[0..Ord(NUMAMMO) - 1] of integer;
    maxammo: array[0..Ord(NUMAMMO) - 1] of integer;

    // True if button down last tic.
    attackdown: boolean;
    usedown: boolean;
    inventorydown: boolean; // STRIFE

    // Bit flags, for cheats and debug.
    // See cheat_t, above.
    cheats: integer;

    // Refired shots are less accurate.
    refire: integer;

    // For intermission stats.
    killcount: integer;

    // Hint messages.
    _message: string[255];

    // For screen flashing (red or bright).
    damagecount: integer;
    bonuscount: integer;

    // Who did damage (NULL for floors/ceilings).
    attacker: Pmobj_t;

    // So gun flashes light up areas.
    extralight: integer;

    // Current PLAYPAL, ???
    //  can be set to REDCOLORMAP for pain, etc.
    fixedcolormap: integer;

    // Player skin colorshift,
    //  0-3 for which color to draw player.
    colormap: integer; // JVAL: is it used somewhere?

    // Overlay view sprites (gun, etc).
    psprites: array[0..Ord(NUMPSPRITES) - 1] of pspdef_t;

    attackerx: fixed_t;
    attackery: fixed_t;

    lastbreath: integer;
    hardbreathtics: integer;

    angletargetx: fixed_t;
    angletargety: fixed_t;
    angletargetticks: integer;

    // STRIFE
    allegiance: integer;

    // [STRIFE] Inefficient means of tracking automap state on all maps
    mapstate: array[0..39] of boolean;

    laddertics: integer;
    viewbob: fixed_t;   // JVAL: Slopes
    slopetics: integer; // JVAL: Slopes
    oldviewz: fixed_t;  // JVAL: Slopes
    teleporttics: integer;
    quaketics: integer;
    lookdir16: integer; // JVAL Smooth Look Up/Down
    cmd: ticcmd_t;      // JVAL Smooth Look Up/Down
    // Version 206
    nextoof: integer;
    lastdialogtalker: Pmobj_t;
    quakeintensity: fixed_t;

    // JVAL: 20211101 - Crouch
    oldcrouch: integer;
    lastongroundtime: integer;
    lastautocrouchtime: integer;
    crouchheight: fixed_t;
  end;
  Pplayer_t = ^player_t;

type
  player_t206 = record
    mo: Pmobj_t;
    playerstate: playerstate_t;
    cmd202: ticcmd_t202;

    // Determine POV,
    //  including viewpoint bobbing during movement.
    // Focal origin above r.z
    viewz: fixed_t;
    // Base height above floor for viewz.
    viewheight: fixed_t;
    // Bob/squat speed.
    deltaviewheight: fixed_t;
    // bounded/scaled total momentum.
    bob: fixed_t;

    // Look UP/DOWN support
    lookdir: integer;
    centering: boolean;
    // Look LEFT/RIGHT support
    lookdir2: byte;
    oldlook2: integer;
    forwarding: boolean;

    // jump
    oldjump: integer;

    // This is only used between levels,
    // mo->health is used during levels.
    health: integer;
    armorpoints: integer;
    // Armor type is 0-2.
    armortype: integer;

    // Power ups. invinc and invis are tic counters.
    powers: array[0..Ord(NUMPOWERS) - 1] of integer;

    // [STRIFE] Additions:
    sigiltype: integer;               // Type of Sigil carried
    nukagecount: integer;             // Nukage exposure counter
    questflags: LongWord;             // Quest bit flags
    centerview: boolean;              // True if view should be centered
    inventory: inventory_a;           // Player inventory items
    st_update: boolean;               // If true, update status bar
    numinventory: integer;            // Num. active inventory items
    inventorycursor: integer;         // Selected inventory item
    accuracy: integer;                // Accuracy stat
    stamina: integer;                 // Stamina stat

    cards: array[0..Ord(NUMCARDS) - 1] of boolean;
    backpack: boolean;

    // Frags, kills of other players.
    frags: array[0..MAXPLAYERS - 1] of integer;
    readyweapon: weapontype_t;

    // Is wp_nochange if not changing.
    pendingweapon: weapontype_t;

    weaponowned: array[0..Ord(NUMWEAPONS) - 1] of boolean;
    ammo: array[0..Ord(NUMAMMO) - 1] of integer;
    maxammo: array[0..Ord(NUMAMMO) - 1] of integer;

    // True if button down last tic.
    attackdown: boolean;
    usedown: boolean;
    inventorydown: boolean; // STRIFE

    // Bit flags, for cheats and debug.
    // See cheat_t, above.
    cheats: integer;

    // Refired shots are less accurate.
    refire: integer;

    // For intermission stats.
    killcount: integer;

    // Hint messages.
    _message: string[255];

    // For screen flashing (red or bright).
    damagecount: integer;
    bonuscount: integer;

    // Who did damage (NULL for floors/ceilings).
    attacker: Pmobj_t;

    // So gun flashes light up areas.
    extralight: integer;

    // Current PLAYPAL, ???
    //  can be set to REDCOLORMAP for pain, etc.
    fixedcolormap: integer;

    // Player skin colorshift,
    //  0-3 for which color to draw player.
    colormap: integer; // JVAL: is it used somewhere?

    // Overlay view sprites (gun, etc).
    psprites: array[0..Ord(NUMPSPRITES) - 1] of pspdef_t;

    attackerx: fixed_t;
    attackery: fixed_t;

    lastbreath: integer;
    hardbreathtics: integer;

    angletargetx: fixed_t;
    angletargety: fixed_t;
    angletargetticks: integer;

    // STRIFE
    allegiance: integer;

    // [STRIFE] Inefficient means of tracking automap state on all maps
    mapstate: array[0..39] of boolean;

    laddertics: integer;
    viewbob: fixed_t;   // JVAL: Slopes
    slopetics: integer; // JVAL: Slopes
    oldviewz: fixed_t;  // JVAL: Slopes
    teleporttics: integer;
    quaketics: integer;
    lookdir16: integer; // JVAL Smooth Look Up/Down
    cmd: ticcmd_t;      // JVAL Smooth Look Up/Down
    // Version 206
    nextoof: integer;
    lastdialogtalker: Pmobj_t;
    quakeintensity: fixed_t;
  end;
  Pplayer_t206 = ^player_t206;

type
  player_t205 = record
    mo: Pmobj_t;
    playerstate: playerstate_t;
    cmd202: ticcmd_t202;

    // Determine POV,
    //  including viewpoint bobbing during movement.
    // Focal origin above r.z
    viewz: fixed_t;
    // Base height above floor for viewz.
    viewheight: fixed_t;
    // Bob/squat speed.
    deltaviewheight: fixed_t;
    // bounded/scaled total momentum.
    bob: fixed_t;

    // Look UP/DOWN support
    lookdir: integer;
    centering: boolean;
    // Look LEFT/RIGHT support
    lookdir2: byte;
    oldlook2: integer;
    forwarding: boolean;

    // jump
    oldjump: integer;

    // This is only used between levels,
    // mo->health is used during levels.
    health: integer;
    armorpoints: integer;
    // Armor type is 0-2.
    armortype: integer;

    // Power ups. invinc and invis are tic counters.
    powers: array[0..Ord(NUMPOWERS) - 1] of integer;

    // [STRIFE] Additions:
    sigiltype: integer;               // Type of Sigil carried
    nukagecount: integer;             // Nukage exposure counter
    questflags: LongWord;             // Quest bit flags
    centerview: boolean;              // True if view should be centered
    inventory: inventory_a;           // Player inventory items
    st_update: boolean;               // If true, update status bar
    numinventory: integer;            // Num. active inventory items
    inventorycursor: integer;         // Selected inventory item
    accuracy: integer;                // Accuracy stat
    stamina: integer;                 // Stamina stat

    cards: array[0..Ord(NUMCARDS) - 1] of boolean;
    backpack: boolean;

    // Frags, kills of other players.
    frags: array[0..MAXPLAYERS - 1] of integer;
    readyweapon: weapontype_t;

    // Is wp_nochange if not changing.
    pendingweapon: weapontype_t;

    weaponowned: array[0..Ord(NUMWEAPONS) - 1] of boolean;
    ammo: array[0..Ord(NUMAMMO) - 1] of integer;
    maxammo: array[0..Ord(NUMAMMO) - 1] of integer;

    // True if button down last tic.
    attackdown: boolean;
    usedown: boolean;
    inventorydown: boolean; // STRIFE

    // Bit flags, for cheats and debug.
    // See cheat_t, above.
    cheats: integer;

    // Refired shots are less accurate.
    refire: integer;

    // For intermission stats.
    killcount: integer;

    // Hint messages.
    _message: string[255];

    // For screen flashing (red or bright).
    damagecount: integer;
    bonuscount: integer;

    // Who did damage (NULL for floors/ceilings).
    attacker: Pmobj_t;

    // So gun flashes light up areas.
    extralight: integer;

    // Current PLAYPAL, ???
    //  can be set to REDCOLORMAP for pain, etc.
    fixedcolormap: integer;

    // Player skin colorshift,
    //  0-3 for which color to draw player.
    colormap: integer; // JVAL: is it used somewhere?

    // Overlay view sprites (gun, etc).
    psprites: array[0..Ord(NUMPSPRITES) - 1] of pspdef_t;

    attackerx: fixed_t;
    attackery: fixed_t;

    lastbreath: integer;
    hardbreathtics: integer;

    angletargetx: fixed_t;
    angletargety: fixed_t;
    angletargetticks: integer;

    // STRIFE
    allegiance: integer;

    // [STRIFE] Inefficient means of tracking automap state on all maps
    mapstate: array[0..39] of boolean;

    laddertics: integer;
    viewbob: fixed_t;   // JVAL: Slopes
    slopetics: integer; // JVAL: Slopes
    oldviewz: fixed_t;  // JVAL: Slopes
    teleporttics: integer;
    quaketics: integer;
    lookdir16: integer; // JVAL Smooth Look Up/Down
    cmd: ticcmd_t;      // JVAL Smooth Look Up/Down
  end;
  Pplayer_t205 = ^player_t205;

type
  player_t203 = record
    mo: Pmobj_t;
    playerstate: playerstate_t;
    cmd202: ticcmd_t;

    // Determine POV,
    //  including viewpoint bobbing during movement.
    // Focal origin above r.z
    viewz: fixed_t;
    // Base height above floor for viewz.
    viewheight: fixed_t;
    // Bob/squat speed.
    deltaviewheight: fixed_t;
    // bounded/scaled total momentum.
    bob: fixed_t;

    // Look UP/DOWN support
    lookdir: integer;
    centering: boolean;
    // Look LEFT/RIGHT support
    lookdir2: byte;
    oldlook2: integer;
    forwarding: boolean;

    // jump
    oldjump: integer;

    // This is only used between levels,
    // mo->health is used during levels.
    health: integer;
    armorpoints: integer;
    // Armor type is 0-2.
    armortype: integer;

    // Power ups. invinc and invis are tic counters.
    powers: array[0..Ord(NUMPOWERS) - 1] of integer;

    // [STRIFE] Additions:
    sigiltype: integer;               // Type of Sigil carried
    nukagecount: integer;             // Nukage exposure counter
    questflags: LongWord;             // Quest bit flags
    centerview: boolean;              // True if view should be centered
    inventory: inventory_a;           // Player inventory items
    st_update: boolean;               // If true, update status bar
    numinventory: integer;            // Num. active inventory items
    inventorycursor: integer;         // Selected inventory item
    accuracy: integer;                // Accuracy stat
    stamina: integer;                 // Stamina stat

    cards: array[0..Ord(NUMCARDS) - 1] of boolean;
    backpack: boolean;

    // Frags, kills of other players.
    frags: array[0..(MAXPLAYERS)-1] of integer;
    readyweapon: weapontype_t;

    // Is wp_nochange if not changing.
    pendingweapon: weapontype_t;

    weaponowned: array[0..Ord(NUMWEAPONS) - 1] of boolean;
    ammo: array[0..Ord(NUMAMMO) - 1] of integer;
    maxammo: array[0..Ord(NUMAMMO) - 1] of integer;

    // True if button down last tic.
    attackdown: boolean;
    usedown: boolean;
    inventorydown: boolean; // STRIFE

    // Bit flags, for cheats and debug.
    // See cheat_t, above.
    cheats: integer;

    // Refired shots are less accurate.
    refire: integer;

    // For intermission stats.
    killcount: integer;

    // Hint messages.
    _message: string[255];

    // For screen flashing (red or bright).
    damagecount: integer;
    bonuscount: integer;

    // Who did damage (NULL for floors/ceilings).
    attacker: Pmobj_t;

    // So gun flashes light up areas.
    extralight: integer;

    // Current PLAYPAL, ???
    //  can be set to REDCOLORMAP for pain, etc.
    fixedcolormap: integer;

    // Player skin colorshift,
    //  0-3 for which color to draw player.
    colormap: integer; // JVAL: is it used somewhere?

    // Overlay view sprites (gun, etc).
    psprites: array[0..Ord(NUMPSPRITES) - 1] of pspdef_t;

    attackerx: fixed_t;
    attackery: fixed_t;

    lastbreath: integer;
    hardbreathtics: integer;

    angletargetx: fixed_t;
    angletargety: fixed_t;
    angletargetticks: integer;

    // STRIFE
    allegiance: integer;

    // [STRIFE] Inefficient means of tracking automap state on all maps
    mapstate: array[0..39] of boolean;

    laddertics: integer;
    viewbob: fixed_t;   // JVAL: Slopes
    slopetics: integer; // JVAL: Slopes
    oldviewz: fixed_t;  // JVAL: Slopes
    teleporttics: integer;
    quaketics: integer;
    lookdir16: integer; // JVAL Smooth Look Up/Down
    cmd: ticcmd_t;      // JVAL Smooth Look Up/Down
  end;
  Pplayer_t203 = ^player_t203;

type
  player_t122 = record
    mo: Pmobj_t;
    playerstate: playerstate_t;
    cmd: ticcmd_t202;

    // Determine POV,
    //  including viewpoint bobbing during movement.
    // Focal origin above r.z
    viewz: fixed_t;
    // Base height above floor for viewz.
    viewheight: fixed_t;
    // Bob/squat speed.
    deltaviewheight: fixed_t;
    // bounded/scaled total momentum.
    bob: fixed_t;

    // Look UP/DOWN support
    lookdir: integer;
    centering: boolean;
    // Look LEFT/RIGHT support
    lookdir2: byte;
    oldlook2: integer;
    forwarding: boolean;

    // jump
    oldjump: integer;

    // This is only used between levels,
    // mo->health is used during levels.
    health: integer;
    armorpoints: integer;
    // Armor type is 0-2.
    armortype: integer;

    // Power ups. invinc and invis are tic counters.
    powers: array[0..Ord(NUMPOWERS) - 1] of integer;

    // [STRIFE] Additions:
    sigiltype: integer;               // Type of Sigil carried
    nukagecount: integer;             // Nukage exposure counter
    questflags: LongWord;             // Quest bit flags
    centerview: boolean;              // True if view should be centered
    inventory: inventory_a;           // Player inventory items
    st_update: boolean;               // If true, update status bar
    numinventory: integer;            // Num. active inventory items
    inventorycursor: integer;         // Selected inventory item
    accuracy: integer;                // Accuracy stat
    stamina: integer;                 // Stamina stat

    cards: array[0..Ord(NUMCARDS) - 1] of boolean;
    backpack: boolean;

    // Frags, kills of other players.
    frags: array[0..(MAXPLAYERS)-1] of integer;
    readyweapon: weapontype_t;

    // Is wp_nochange if not changing.
    pendingweapon: weapontype_t;

    weaponowned: array[0..Ord(NUMWEAPONS) - 1] of boolean;
    ammo: array[0..Ord(NUMAMMO) - 1] of integer;
    maxammo: array[0..Ord(NUMAMMO) - 1] of integer;

    // True if button down last tic.
    attackdown: boolean;
    usedown: boolean;
    inventorydown: boolean; // STRIFE

    // Bit flags, for cheats and debug.
    // See cheat_t, above.
    cheats: integer;

    // Refired shots are less accurate.
    refire: integer;

    // For intermission stats.
    killcount: integer;

    // Hint messages.
    _message: string[255];

    // For screen flashing (red or bright).
    damagecount: integer;
    bonuscount: integer;

    // Who did damage (NULL for floors/ceilings).
    attacker: Pmobj_t;

    // So gun flashes light up areas.
    extralight: integer;

    // Current PLAYPAL, ???
    //  can be set to REDCOLORMAP for pain, etc.
    fixedcolormap: integer;

    // Player skin colorshift,
    //  0-3 for which color to draw player.
    colormap: integer; // JVAL: is it used somewhere?

    // Overlay view sprites (gun, etc).
    psprites: array[0..Ord(NUMPSPRITES) - 1] of pspdef_t;

    attackerx: fixed_t;
    attackery: fixed_t;

    lastbreath: integer;
    hardbreathtics: integer;

    angletargetx: fixed_t;
    angletargety: fixed_t;
    angletargetticks: integer;

    // STRIFE
    allegiance: integer;

    // [STRIFE] Inefficient means of tracking automap state on all maps
    mapstate: array[0..39] of boolean;

    laddertics: integer;
    viewbob: fixed_t; // JVAL: Slopes
    slopetics: integer; // JVAL: Slopes
    oldviewz: fixed_t; // JVAL: Slopes
    teleporttics: integer;
    quaketics: integer;
  end;
  Pplayer_t122 = ^player_t122;

type
  player_t121 = record
    mo: Pmobj_t;
    playerstate: playerstate_t;
    cmd: ticcmd_t202;

    // Determine POV,
    //  including viewpoint bobbing during movement.
    // Focal origin above r.z
    viewz: fixed_t;
    // Base height above floor for viewz.
    viewheight: fixed_t;
    // Bob/squat speed.
    deltaviewheight: fixed_t;
    // bounded/scaled total momentum.
    bob: fixed_t;

    // Look UP/DOWN support
    lookdir: integer;
    centering: boolean;
    // Look LEFT/RIGHT support
    lookdir2: byte;
    oldlook2: integer;
    forwarding: boolean;

    // jump
    oldjump: integer;

    // This is only used between levels,
    // mo->health is used during levels.
    health: integer;
    armorpoints: integer;
    // Armor type is 0-2.
    armortype: integer;

    // Power ups. invinc and invis are tic counters.
    powers: array[0..Ord(NUMPOWERS) - 1] of integer;

    // [STRIFE] Additions:
    sigiltype: integer;               // Type of Sigil carried
    nukagecount: integer;             // Nukage exposure counter
    questflags: LongWord;             // Quest bit flags
    centerview: boolean;              // True if view should be centered
    inventory: inventory_a;           // Player inventory items
    st_update: boolean;               // If true, update status bar
    numinventory: integer;            // Num. active inventory items
    inventorycursor: integer;         // Selected inventory item
    accuracy: integer;                // Accuracy stat
    stamina: integer;                 // Stamina stat

    cards: array[0..Ord(NUMCARDS) - 1] of boolean;
    backpack: boolean;

    // Frags, kills of other players.
    frags: array[0..MAXPLAYERS - 1] of integer;
    readyweapon: weapontype_t;

    // Is wp_nochange if not changing.
    pendingweapon: weapontype_t;

    weaponowned: array[0..Ord(NUMWEAPONS) - 1] of boolean;
    ammo: array[0..Ord(NUMAMMO) - 1] of integer;
    maxammo: array[0..Ord(NUMAMMO) - 1] of integer;

    // True if button down last tic.
    attackdown: boolean;
    usedown: boolean;
    inventorydown: boolean; // STRIFE

    // Bit flags, for cheats and debug.
    // See cheat_t, above.
    cheats: integer;

    // Refired shots are less accurate.
    refire: integer;

    // For intermission stats.
    killcount: integer;

    // Hint messages.
    _message: string[255];

    // For screen flashing (red or bright).
    damagecount: integer;
    bonuscount: integer;

    // Who did damage (NULL for floors/ceilings).
    attacker: Pmobj_t;

    // So gun flashes light up areas.
    extralight: integer;

    // Current PLAYPAL, ???
    //  can be set to REDCOLORMAP for pain, etc.
    fixedcolormap: integer;

    // Player skin colorshift,
    //  0-3 for which color to draw player.
    colormap: integer; // JVAL: is it used somewhere?

    // Overlay view sprites (gun, etc).
    psprites: array[0..Ord(NUMPSPRITES) - 1] of pspdef_t;

    attackerx: fixed_t;
    attackery: fixed_t;

    lastbreath: integer;
    hardbreathtics: integer;

    angletargetx: fixed_t;
    angletargety: fixed_t;
    angletargetticks: integer;

    // STRIFE
    allegiance: integer;

    // [STRIFE] Inefficient means of tracking automap state on all maps
    mapstate: array[0..39] of boolean;

  end;
  Pplayer_t121 = ^player_t121;

//
// INTERMISSION
// Structure passed e.g. to WI_Start(wb)
//

  wbplayerstruct_t = record
    _in: boolean; // whether the player is in game

    // Player stats, kills, collected items etc.
    skills: integer;
    sitems: integer;
    ssecret: integer;
    stime: integer;
    frags: array[0..3] of integer;
    score: integer; // current score on entry, modified on return
  end;
  Pwbplayerstruct_t = ^wbplayerstruct_t;
  wbplayerstruct_tArray = packed array[0..$FFFF] of wbplayerstruct_t;
  Pwbplayerstruct_tArray = ^wbplayerstruct_tArray;

  wbstartstruct_t = record
    epsd: integer; // episode # (0-2)

    // if true, splash the secret level
    didsecret: boolean;

    // previous and next levels, origin 0
    last: integer;
    next: integer;

    maxkills: integer;
    maxitems: integer;
    maxsecret: integer;
    maxfrags: integer;

    // the par time
    partime: integer;

    // index of this player in game
    pnum: integer;

    plyr: array[0..MAXPLAYERS - 1] of wbplayerstruct_t;
  end;
  Pwbstartstruct_t = ^wbstartstruct_t;

var
// JVAL -> moved from g_game
  players: array[0..MAXPLAYERS - 1] of player_t;

// JVAL Min and Max values for player.lookdir
const
  MINLOOKDIR = -110;
  MAXLOOKDIR = 90;

implementation

end.

