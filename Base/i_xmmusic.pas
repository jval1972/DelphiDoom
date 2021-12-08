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
//  XM music module detection
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit i_xmmusic;

interface

function IsXMMusicFile(const buf: pointer; const size: integer): boolean;

implementation

uses
  d_delphi;

function IsXMMusicFile(const buf: pointer; const size: integer): boolean;
var
  pb: PByteArray;
begin
  Result := size > 16;
  if Result then
  begin
    pb := buf;
    Result := // "Extended Module:"
      (pb[0] = $45) and
      (pb[1] = $78) and
      (pb[2] = $74) and
      (pb[3] = $65) and
      (pb[4] = $6E) and
      (pb[5] = $64) and
      (pb[6] = $65) and
      (pb[7] = $64) and
      (pb[8] = $20) and
      (pb[9] = $4D) and
      (pb[10] = $6F) and
      (pb[11] = $64) and
      (pb[12] = $75) and
      (pb[13] = $6C) and
      (pb[14] = $65) and
      (pb[15] = $3A);
  end;
end;

end.

