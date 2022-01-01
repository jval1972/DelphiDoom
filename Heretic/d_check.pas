//------------------------------------------------------------------------------
//
//  DelphiHeretic: A modified and improved Heretic port for Windows
//  based on original Linux Doom as published by "id Software", on
//  Heretic source as published by "Raven" software and DelphiDoom
//  as published by Jim Valavanis.
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
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit d_check;

interface

procedure D_CheckCustomWad(const filename: string);

function D_GetSavePath: string;

implementation

uses
  d_delphi,
  doomdef,
  doomstat,
  m_argv,
  m_crc32,
  w_wad,
  w_wadreader;

var
  savepath: string = 'heretic';

function D_CheckUnknownWad(const filename: string): boolean;
const
  sNUMS = '0123456789';
var
  numlumps: integer;
  nummaps: integer;
  name, s: string;
  crc: string;
  wad: TWadReader;
  i: integer;
begin
  crc := GetCRC32(filename);

  wad := TWadReader.Create;
  wad.OpenWadFile(filename);
  numlumps := wad.NumEntries;

  splitstring(fname(filename), name, s, '.');
  nummaps := 0;

  for i := 0 to numlumps - 1 do
  begin
    s := strupper(wad.EntryName(i));
    if Length(s) = 4 then
    begin
      if s[1] = 'E' then
        if s[3] = 'M' then
          if Pos(s[2], sNUMS) > 0 then
            if Pos(s[4], sNUMS) > 0 then
              inc(nummaps);
    end;
  end;
  wad.Free;

  result := nummaps > 0;
  if result then
    savepath := name + '_' + crc;
end;

procedure D_CheckCustomWad(const filename: string);
begin
  D_CheckUnknownWad(filename);
end;

function D_GetSavePath: string;
var
  s: string;
begin
  s := 'DATA';
  MkDir(M_SaveFileName(s));
  s := s + '\SAVES';
  MkDir(M_SaveFileName(s));
  s := s + '\' + savepath;
  MkDir(M_SaveFileName(s));
  result := 'DATA\SAVES\' + savepath + '\';
end;

end.
