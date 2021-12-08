//------------------------------------------------------------------------------
//
//  DelphiDoom: A modified and improved DOOM engine for Windows
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 1993-1996 by id Software, Inc.
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
//  System specific interface stuff.
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit d_ticcmd;

interface

const
  CMD_JUMP_MASK = 3;
  CMD_JUMP_SHIFT = 0;
  CMD_CROUCH_MASK = 12;
  CMD_CROUCH_SHIFT = 2;

// The data sampled per tick (single player)
// and transmitted to other peers (multiplayer).
// Mainly movements/button commands per game tick,
// plus a checksum for internal state consistency.
type
  ticcmd_t = packed record
    forwardmove: shortint;  // *2048 for move
    sidemove: shortint;     // *2048 for move
    angleturn: smallint;    // <<16 for angle delta
    consistancy: smallint;  // checks for net game
    chatchar: byte;
    buttons: byte;
{$IFDEF STRIFE}
    buttons2: byte;
    inventory: integer;
{$ENDIF}
    commands: byte;         // JVAL for special commands
{$IFDEF HERETIC_OR_HEXEN}
    lookfly: byte;          // look up/down/centering/fly
    arti: byte;
{$ENDIF}
    lookupdown: byte;       // JVAL look up/down/centering
    lookleftright: byte;    // JVAL look left/right/forward
    jump_crouch: byte;      // JVAL Jump!
    lookupdown16: word;     // JVAL Smooth Look Up/Down
  end;
  Pticcmd_t = ^ticcmd_t;

  ticcmd_t202 = packed record
    forwardmove: shortint;  // *2048 for move
    sidemove: shortint;     // *2048 for move
    angleturn: smallint;    // <<16 for angle delta
    consistancy: smallint;  // checks for net game
    chatchar: byte;
    buttons: byte;
{$IFDEF STRIFE}
    buttons2: byte;
    inventory: integer;
{$ENDIF}
    commands: byte;         // JVAL for special commands
{$IFDEF HERETIC_OR_HEXEN}
    lookfly: byte;          // look up/down/centering/fly
    arti: byte;
{$ENDIF}
    lookupdown: byte;       // JVAL look up/down/centering
    lookleftright: byte;    // JVAL look left/right/forward
    jump: byte;             // JVAL Jump!
  end;
  Pticcmd_t202 = ^ticcmd_t202;

implementation


end.

