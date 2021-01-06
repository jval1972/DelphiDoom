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
//  AAPTR
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit p_aaptr;

interface

const
  AAPTR_DEFAULT = 0;
  AAPTR_NULL = $1;
  AAPTR_TARGET = $2;
  AAPTR_MASTER = $4;
  AAPTR_TRACER = $8;

  AAPTR_PLAYER_GETTARGET = $10;
  AAPTR_PLAYER_GETCONVERSATION = $20;

  AAPTR_PLAYER1 = $40;
  AAPTR_PLAYER2 = $80;
  AAPTR_PLAYER3 = $100;
  AAPTR_PLAYER4 = $200;
  AAPTR_PLAYER5 = $400;
  AAPTR_PLAYER6 = $800;
  AAPTR_PLAYER7 = $1000;
  AAPTR_PLAYER8 = $2000;

  AAPTR_FRIENDPLAYER = $4000;
  AAPTR_GET_LINETARGET = $8000;

  AAPTR_PLAYER_SELECTORS =
    AAPTR_PLAYER_GETTARGET or AAPTR_PLAYER_GETCONVERSATION;

  AAPTR_GENERAL_SELECTORS =
    AAPTR_TARGET or AAPTR_MASTER or AAPTR_TRACER or AAPTR_FRIENDPLAYER or AAPTR_GET_LINETARGET;

  AAPTR_STATIC_SELECTORS =
    AAPTR_PLAYER1 or AAPTR_PLAYER2 or AAPTR_PLAYER3 or AAPTR_PLAYER4 or
    AAPTR_PLAYER5 or AAPTR_PLAYER6 or AAPTR_PLAYER7 or AAPTR_PLAYER8 or
    AAPTR_NULL;


implementation

end.
 