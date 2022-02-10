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
// DESCRIPTION:
//  Support for various midi formats:
//   - XMI (Extended midi)
//   - CMF (Creative music file)
//   - SOP (Sopepos)
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit i_xmimusic;

interface

//==============================================================================
//
// IsXMIMusicFile
//
//==============================================================================
function IsXMIMusicFile(const buf: pointer; const size: integer): boolean;

implementation

uses
  xmi_lib,
  xmi_core;

//==============================================================================
//
// IsXMIMusicFile
//
//==============================================================================
function IsXMIMusicFile(const buf: pointer; const size: integer): boolean;
begin
  Result :=
    XMICore.DetectXMIMemory(buf, size) or
    XMICore.DetectCMFMemory(buf, size) or
    XMICore.DetectSOPMemory(buf, size);
end;

end.

