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
// DESCRIPTION:
//   Various STRIFE wad versions check
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit d_check;

interface

function D_Panel0Lump: integer;

function D_Help0Lump: integer;

function D_UseOldTexture1: boolean;

var
  teaser: integer = 0;

procedure D_IdentifyTeaser;

implementation

uses
  deh_main,
  m_crc32,
  w_wad;

var
  panel0lump: integer = -2;

function D_Panel0Lump: integer;
begin
  if panel0lump <> -2 then
  begin
    result := panel0lump;
    exit;
  end;

  panel0lump := W_CheckNumForName(DEH_GetString('PANEL0'));

  if panel0lump < 0 then
    panel0lump := W_CheckNumForName(DEH_GetString('HLPBCK'));

  result := panel0lump;
end;

var
  help0lump: integer = -2;

function D_Help0Lump: integer;
begin
  if help0lump <> -2 then
  begin
    result := help0lump;
    exit;
  end;

  help0lump := W_CheckNumForName(DEH_GetString('HELP0'));

  if help0lump < 0 then
    help0lump := W_CheckNumForName(DEH_GetString('HLPBCK'));

  result := help0lump;
end;


function D_UseOldTexture1: boolean;
var
  lumpnum: integer;
begin
  lumpnum := W_CheckNumForName('TEXTURE1');
  if lumpnum < 0 then
  begin
    result := false;
    exit;
  end;

  result := (W_LumpLength(lumpnum) = 11564) and (GetLumpCRC32(lumpnum) = 'a5c7ddf4');
end;

procedure D_IdentifyTeaser;
begin
  if W_CheckNumForName('MAP35') >= 0 then
    if W_CheckNumForName('MAP36') >= 0 then
      if W_CheckNumForName('HELP0') < 0 then
        if W_CheckNumForName('PANEL0') < 0 then
          if W_CheckNumForName('MAP02') < 0 then
            if W_CheckNumForName('SCRIPT00') >= 0 then
              if GetLumpCRC32('SCRIPT00') = '66042225' then
              begin
                teaser := 1;
                exit;
              end;

  if W_CheckNumForName('MAP33') >= 0 then
    if W_CheckNumForName('MAP34') >= 0 then
      if W_CheckNumForName('MAP35') < 0 then
        if W_CheckNumForName('HELP0') >= 0 then
          if W_CheckNumForName('PANEL0') >= 0 then
            if W_CheckNumForName('SCRIPT00') >= 0 then
              if GetLumpCRC32('SCRIPT00') = '4f407f19' then
              begin
                teaser := 2;
                exit;
              end;

  teaser := 0;
end;

end.
