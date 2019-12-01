//------------------------------------------------------------------------------
//
//  DelphiHexen: A modified and improved Hexen port for Windows
//  based on original Linux Doom as published by "id Software", on
//  Hexen source as published by "Raven" software and DelphiDoom
//  as published by Jim Valavanis.
//  Copyright (C) 2004-2008 by Jim Valavanis
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

unit d_net_h;

interface

uses
  d_ticcmd;

const
// Networking and tick handling related.
  BACKUPTICS = 12;

type
  Tcmds = packed array[0..BACKUPTICS - 1] of ticcmd_t;
//
// Network packet data.
//
  doomdata_t = record
    // High bit is retransmit request.
    checksum: LongWord;
    // Only valid if NCMD_RETRANSMIT.
    retransmitfrom: byte;

    starttic: byte;
    player: byte;
    numtics: byte;
    flags: longword;
    cmds: Tcmds;
  end;
  Pdoomdata_t = ^doomdata_t;


var
  netbuffer: Pdoomdata_t; // points inside doomcom

implementation

end.
