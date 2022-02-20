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
//  General purpose global hash
//  (sdbm hash implementation)
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit m_hash;

interface

//==============================================================================
//
// M_HashIndex
//
//==============================================================================
function M_HashIndex(const s: string): integer;

//==============================================================================
//
// M_HashUpdate
//
//==============================================================================
procedure M_HashUpdate(const s: string; const idx: integer);

implementation

uses
  d_delphi;

const
  SDBMHASHSIZE = $10000;

type
  TSDBMHash = class
  private
    htable: array[0..SDBMHASHSIZE - 1] of Integer;
  public
    constructor Create; virtual;
    function Hash(const s: string): LongWord;
    function GetHashIndex(const h: integer): integer;
    procedure SetHashIndex(const h: integer; const idx: integer);
  end;

var
  globalhashmanager: TSDBMHash;

//==============================================================================
//
// TSDBMHash.Create
//
//==============================================================================
constructor TSDBMHash.Create;
begin
  inherited;
  ZeroMemory(@htable, SizeOf(htable));
end;

//==============================================================================
//
// TSDBMHash.Hash
//
//==============================================================================
function TSDBMHash.Hash(const s: string): LongWord;
var
  i: integer;
begin
  Result := 0;
  for i := 1 to Length(s) do
    Result := Ord(s[i]) + (Result shl 6) + (Result shl 16) - Result;
  Result := Result and (SDBMHASHSIZE - 1);
end;

//==============================================================================
//
// TSDBMHash.GetHashIndex
//
//==============================================================================
function TSDBMHash.GetHashIndex(const h: integer): integer;
begin
  result := htable[h];
end;

//==============================================================================
//
// TSDBMHash.SetHashIndex
//
//==============================================================================
procedure TSDBMHash.SetHashIndex(const h: integer; const idx: integer);
begin
  htable[h] := idx;
end;

//==============================================================================
//
// M_HashIndex
//
//==============================================================================
function M_HashIndex(const s: string): integer;
var
  h: integer;
begin
  h := globalhashmanager.Hash(s);
  Result := globalhashmanager.htable[h];
end;

//==============================================================================
//
// M_HashUpdate
//
//==============================================================================
procedure M_HashUpdate(const s: string; const idx: integer);
var
  h: integer;
begin
  h := globalhashmanager.Hash(s);
  globalhashmanager.htable[h] := idx;
end;

initialization
  globalhashmanager := TSDBMHash.Create;

finalization
  globalhashmanager.Free;

end.

