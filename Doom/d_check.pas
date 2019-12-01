//------------------------------------------------------------------------------
//
//  DelphiDoom: A modified and improved DOOM engine for Windows
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 2004-2012 by Jim Valavanis
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
//  DESCRIPTION (d_main.h):
//   System specific interface stuff.
//
//  DESCRIPTION:
//   Custom game detection
//
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : http://delphidoom.sitesled.com/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit d_check;

interface

procedure D_CheckCustomWad(const filename: string);

implementation

uses
  d_delphi,
  doomdef,
  doomstat;

procedure D_CheckCustomWad(const filename: string);
begin
  // JVAL: Chex Support
  if strupper(fname(filename)) = 'CHEX.WAD' then
    if customgame <> cg_chex2 then
    begin
      customgame := cg_chex;
      Exit;
    end;
  if strupper(fname(filename)) = 'CHEX2.WAD' then
  begin
    customgame := cg_chex2;
    exit;
  end;
  // JVAL: Hacx Support
  if strupper(fname(filename)) = 'HACX.WAD' then
  begin
    customgame := cg_hacx;
    exit;
  end;
end;

end.
