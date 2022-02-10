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
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit r_renderstyle;

interface

type
  mobjrenderstyle_t = (
    mrs_normal, mrs_translucent, mrs_add, mrs_subtract, NUMMOBJRENDERSTYLES
  );

//==============================================================================
//
// R_GetRenderstyleForName
//
//==============================================================================
function R_GetRenderstyleForName(const s: string): mobjrenderstyle_t;

implementation

uses
  d_delphi,
  deh_main;

//==============================================================================
//
// R_GetRenderstyleForName
//
//==============================================================================
function R_GetRenderstyleForName(const s: string): mobjrenderstyle_t;
var
  check: string;
  idx: integer;
begin
  result := mrs_normal;

  idx := atoi(s, -1);
  if idx >= 0 then
    if idx < Ord(NUMMOBJRENDERSTYLES) then
    begin
      result := mobjrenderstyle_t(idx);
      exit;
    end;

  check := strupper(s);
  if (check = 'ADDITIVE') or (check = 'ADD') then
  begin
    result := mrs_add;
    exit;
  end;

  if (check = 'SUB') or (check = 'SUBTRACT') or (check = 'SUBTRACTIVE') then
  begin
    result := mrs_subtract;
    exit;
  end;

  idx := renderstyle_tokens.IndexOf(check);
  if idx >= 0 then
  begin
    result := mobjrenderstyle_t(idx);
    exit;
  end;
end;

end.
