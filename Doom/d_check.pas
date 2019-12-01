//------------------------------------------------------------------------------
//
//  DelphiDoom: A modified and improved DOOM engine for Windows
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 2004-2013 by Jim Valavanis
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
//  Site  : http://sourceforge.net/projects/delphidoom/
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
var
  sname: string;
begin
  sname := strupper(fname(filename));
  // JVAL: Chex Support
  if sname = 'CHEX.WAD' then
    if customgame <> cg_chex2 then
    begin
      gameversion := exe_chex;
      customgame := cg_chex;
      Exit;
    end;
  if sname = 'CHEX2.WAD' then
  begin
    gameversion := exe_chex;
    customgame := cg_chex2;
    exit;
  end;
  // JVAL: Hacx Support
  if sname = 'HACX.WAD' then
  begin
    gameversion := exe_hacx;
    customgame := cg_hacx;
    exit;
  end;
  // JVAL: FreeDoom Support
  if (sname = 'FREEDOOM.WAD') or (sname = 'FREEDOOM2.WAD') then
  begin
    customgame := cg_freedoom;
    exit;
  end;
  // JVAL: FINAL DOOM
  if (sname = 'TNT.WAD') or (sname = 'PLUTONIA.WAD') then
  begin
    gameversion := exe_final2;
    exit;
  end;
  // JVAL: DOOM2
  if sname = 'DOOM2.WAD' then
  begin
    gameversion := exe_doom_1_9;
    exit;
  end;
  // JVAL: UNTIMATE DOOM
  if (sname = 'DOOM.WAD') or (sname = 'DOOM1.WAD') or (sname = 'DOOMU.WAD') then
  begin
    gameversion := exe_ultimate;
    exit;
  end;
end;

end.
