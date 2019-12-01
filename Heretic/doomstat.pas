//------------------------------------------------------------------------------
//
//  DelphiHeretic: A modified and improved Heretic port for Windows
//  based on original Linux Doom as published by "id Software", on
//  Heretic source as published by "Raven" software and DelphiDoom
//  as published by Jim Valavanis.
//  Copyright (C) 2004-2009 by Jim Valavanis
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
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : http://delphidoom.sitesled.com/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit doomstat;

interface

uses
  doomdef;

{
    doomstat.c
}

// Emacs style mode select   -*- C++ -*-  
//----------------------------------------------------------------------------- 
// 
// $Id:$ 
// 
// Copyright (C) 1993-1996 by id Software, Inc. 
// 
// This source is available for distribution and/or modification 
// only under the terms of the DOOM Source Code License as 
// published by id Software. All rights reserved. 
// 
// The source is distributed in the hope that it will be useful, 
// but WITHOUT ANY WARRANTY; without even the implied warranty of 
// FITNESS FOR A PARTICULAR PURPOSE. See the DOOM Source Code License 
// for more details. 
// 
// $Log:$ 
// 
// DESCRIPTION: 
//  Put all global tate variables here. 
// 
//-----------------------------------------------------------------------------

var
// Game Mode - identify IWAD as shareware, retail etc.
  gamemode: GameMode_t;

// Language.
  language: Language_t;

  customgame: CustomGame_t = cg_none;

// Set if homebrew PWAD stuff has been added.
  modifiedgame : boolean;
  externalpakspresent: boolean = false;
  externaldehspresent: boolean = false;

implementation

initialization
  gamemode := indetermined;
  language := english;
  modifiedgame := false;

end.

