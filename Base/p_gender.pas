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
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit p_gender;

interface

type
  gender_t = (
    gender_Default,
    gender_Male,
    gender_Female,
    gender_Plural,
    gender_Team,
    gender_Neutral,
    NUMGENDERS
  );

  genderinfo_t = record
    name: string[15];
    ob_g: string[7];
    ob_h: string[7];
    ob_p: string[7];
    ob_s: string[7];
    ob_r: string[7];
  end;

const
  GENDERINFO: array[0..Ord(NUMGENDERS) - 1] of genderinfo_t = (
    (name: 'Default'; ob_g: 'it';   ob_h: 'it';   ob_p: 'its';    ob_s: 'its';    ob_r: 'it''s'),
    (name: 'Male';    ob_g: 'he';   ob_h: 'him';  ob_p: 'his';    ob_s: 'his';    ob_r: 'he''s'),
    (name: 'Female';  ob_g: 'she';  ob_h: 'her';  ob_p: 'her';    ob_s: 'hers';   ob_r: 'she''s'),
    (name: 'Plural';  ob_g: 'they'; ob_h: 'them'; ob_p: 'their';  ob_s: 'theirs'; ob_r: 'they''re'),
    (name: 'Team';    ob_g: 'they'; ob_h: 'them'; ob_p: 'their';  ob_s: 'theirs'; ob_r: 'they''re'),
    (name: 'Neutral'; ob_g: 'it';   ob_h: 'it';   ob_p: 'its';    ob_s: 'its';    ob_r: 'it''s')
  );

function R_GetGenderForName(const s: string): gender_t;

implementation

uses
  d_delphi;

function R_GetGenderForName(const s: string): gender_t;
var
  check: string;
  i, idx: integer;
begin
  result := gender_Default;

  idx := atoi(s, -1);
  if idx >= 0 then
    if idx < Ord(NUMGENDERS) then
    begin
      result := gender_t(idx);
      exit;
    end;

  check := strupper(s);
  for i := 0 to Ord(NUMGENDERS) - 1 do
    if check = strupper(GENDERINFO[i].name) then
    begin
      result := gender_t(i);
      exit;
    end;

  for i := 0 to Ord(NUMGENDERS) - 1 do
    if check = 'GENDER_' + strupper(GENDERINFO[i].name) then
    begin
      result := gender_t(i);
      exit;
    end;
end;

end.
