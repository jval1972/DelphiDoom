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
//  Cheat code checking.
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit m_cheat;

interface

//
// CHEAT SEQUENCE PACKAGE
//

type
  cheatseq_t = record
    sequence: string;
    p: string;
  end;
  Pcheatseq_t = ^cheatseq_t;

  cheatstatus_t = (cht_unknown, cht_pending, cht_acquired);

//==============================================================================
//
// cht_CheckCheat
//
//==============================================================================
function cht_CheckCheat(cht: Pcheatseq_t; key: char): cheatstatus_t;

//==============================================================================
//
// cht_GetParam
//
//==============================================================================
procedure cht_GetParam(cht: Pcheatseq_t; var buffer: string);

//==============================================================================
//
// get_cheatseq_string
//
//==============================================================================
function get_cheatseq_string(const A: array of char): string; overload; // JVAL

//==============================================================================
//
// get_cheatseq_string
//
//==============================================================================
function get_cheatseq_string(const A: string): string; overload; // JVAL

//==============================================================================
//
// get_cheatseq_string
//
//==============================================================================
function get_cheatseq_string(const x: integer): string; overload; // JVAL

implementation

uses
  d_delphi,
  i_system;

//==============================================================================
//
// get_cheatseq_string
//
//==============================================================================
function get_cheatseq_string(const A: array of char): string; // JVAL
var
  i: integer;
begin
  result := '';
  i := 0;
  repeat
    result := result + A[i];
    inc(i);
  until A[i] = Chr($FF);
end;

//==============================================================================
//
// get_cheatseq_string
//
//==============================================================================
function get_cheatseq_string(const A: string): string;  // JVAL
var
  i: integer;
begin
  result := '';
  i := 1;
  repeat
    result := result + A[i];
    inc(i);
  until A[i] = Chr($FF);
end;

//==============================================================================
//
// get_cheatseq_string
//
//==============================================================================
function get_cheatseq_string(const x: integer): string; // JVAL
begin
  result := '';
  if x <> 0 then
    I_Error('get_cheatseq_string(): invalid parameter: %d', [x]);
end;

//==============================================================================
//
// SCRAMBLE
//
//==============================================================================
function SCRAMBLE(a: integer): integer;
begin
  result := _SHL(a and 1, 7) +
            _SHL(a and 2, 5) +
            (a and 4) +
            _SHL(a and 8, 1) +
            _SHR(a and 16, 1) +
            (a and 32) +
            _SHR(a and 64, 5) +
            _SHR(a and 128, 7);
end;

var
  firsttime: boolean = true;
  cheat_xlate_table: array[0..255] of char;

//==============================================================================
// cht_CheckCheat
//
// Called in st_stuff module, which handles the input.
// Returns true if the cheat was successful, false if failed.
//
//==============================================================================
function cht_CheckCheat(cht: Pcheatseq_t; key: char): cheatstatus_t;
var
  i: integer;
begin
  result := cht_unknown;

  if firsttime then
  begin
    firsttime := false;
    for i := 0 to 255 do
      cheat_xlate_table[i] := Chr(SCRAMBLE(i));
  end;

  if (cht.p = '') then
    cht.p := cht.sequence; // initialize if first time

  if (cht.p[1] = #0) then
    cht.p[1] := key
  else if (length(cht.p) > 1) and (cht.p[2] = #0) then
  begin
    cht.p[2] := key;
    result := cht_acquired;
  end
  else if cheat_xlate_table[Ord(key)] = cht.p[1] then
  begin
    result := cht_pending;
    Delete(cht.p, 1, 1)
  end
  else
    cht.p := cht.sequence;

  if length(cht.p) > 0 then
  begin
    if cht.p[1] = #1 then
      Delete(cht.p, 1, 1)
    else if cht.p[1] = Chr($FF) then // end of sequence character
    begin
      cht.p := cht.sequence;
      result := cht_acquired;
    end;
  end
  else
    result := cht_acquired;
end;

//==============================================================================
//
// cht_GetParam
//
//==============================================================================
procedure cht_GetParam(cht: Pcheatseq_t; var buffer: string);
begin
  buffer := cht.p;
end;

end.
