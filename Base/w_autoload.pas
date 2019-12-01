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
//  Foundation, inc., 59 Temple Place - Suite 330, Boston, MA
//  02111-1307, USA.
//
// DESCRIPTION:
//  Autoload pak files from AUTOLOAD entries inside wads
//
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit w_autoload;

interface

procedure W_AutoLoadPakFiles;

implementation

uses
  w_pak,
  w_wad,
  sc_engine;

const
  AUTOLOADLUMPNAME = 'AUTOLOAD';

procedure W_ParseAutoLoad(const in_text: string);
var
  sc: TScriptEngine;
begin
  sc := TScriptEngine.Create(in_text);
  try
    while sc.GetString do
      PAK_AddFile(sc._String)
  finally
    sc.Free;
  end;
end;

procedure W_AutoLoadPakFiles;
var
  i: integer;
begin
// Retrive modeldef lumps
  for i := 0 to W_NumLumps - 1 do
    if char8tostring(W_GetNameForNum(i)) = AUTOLOADLUMPNAME then
      W_ParseAutoLoad(W_TextLumpNum(i));
end;

end.
