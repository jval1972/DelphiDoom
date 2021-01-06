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
//
// DESCRIPTION:
//  Scripting consts
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit sc_consts;

interface

procedure SC_InitConsts;

procedure SC_ShutDownConsts;

function SC_AddConst(const name: string; const value: integer): boolean;

function SC_GetConst(const name: string; var value: integer): boolean;

implementation

uses
  d_delphi,
  i_system,
  p_aaptr,
  p_common;

const
  CONSTSHASHSIZE = 16;

var
  constshashtable: array[0..CONSTSHASHSIZE - 1] of TDStringList;

function _hash(const s: string): integer;
begin
  result := Ord(s[1]) and (CONSTSHASHSIZE - 1);
end;

procedure SC_InitConsts;
var
  i: integer;
begin
  for i := 0 to CONSTSHASHSIZE - 1 do
    constshashtable[i] := TDStringList.Create;

  SC_AddConst('C_ARG1', 0);
  SC_AddConst('C_ARG2', 1);
  SC_AddConst('C_ARG3', 2);
  SC_AddConst('C_ARG4', 3);
  SC_AddConst('C_ARG5', 4);

  SC_AddConst('AAPTR_DEFAULT', AAPTR_DEFAULT);
  SC_AddConst('AAPTR_NULL', AAPTR_NULL);
  SC_AddConst('AAPTR_TARGET', AAPTR_TARGET);
  SC_AddConst('AAPTR_MASTER', AAPTR_MASTER);
  SC_AddConst('AAPTR_TRACER', AAPTR_TRACER);
  SC_AddConst('AAPTR_PLAYER_GETTARGET', AAPTR_PLAYER_GETTARGET);
  SC_AddConst('AAPTR_PLAYER_GETCONVERSATION', AAPTR_PLAYER_GETCONVERSATION);
  SC_AddConst('AAPTR_PLAYER1', AAPTR_PLAYER1);
  SC_AddConst('AAPTR_PLAYER2', AAPTR_PLAYER2);
  SC_AddConst('AAPTR_PLAYER3', AAPTR_PLAYER3);
  SC_AddConst('AAPTR_PLAYER4', AAPTR_PLAYER4);
  SC_AddConst('AAPTR_PLAYER5', AAPTR_PLAYER5);
  SC_AddConst('AAPTR_PLAYER6', AAPTR_PLAYER6);
  SC_AddConst('AAPTR_PLAYER7', AAPTR_PLAYER7);
  SC_AddConst('AAPTR_PLAYER8', AAPTR_PLAYER8);
  SC_AddConst('AAPTR_FRIENDPLAYER', AAPTR_FRIENDPLAYER);
  SC_AddConst('AAPTR_GET_LINETARGET', AAPTR_GET_LINETARGET);
  SC_AddConst('AAPTR_PLAYER_SELECTORS', AAPTR_PLAYER_SELECTORS);
  SC_AddConst('AAPTR_GENERAL_SELECTORS', AAPTR_GENERAL_SELECTORS);
  SC_AddConst('AAPTR_STATIC_SELECTORS', AAPTR_STATIC_SELECTORS);
  SC_AddConst('PTROP_UNSAFETARGET', PTROP_UNSAFETARGET);
  SC_AddConst('PTROP_UNSAFEMASTER', PTROP_UNSAFEMASTER);
  SC_AddConst('PTROP_NOSAFEGUARDS', PTROP_NOSAFEGUARDS);

  SC_AddConst('SIXF_TRANSFERTRANSLATION', SIXF_TRANSFERTRANSLATION);
  SC_AddConst('SIXF_ABSOLUTEPOSITION', SIXF_ABSOLUTEPOSITION);
  SC_AddConst('SIXF_ABSOLUTEANGLE', SIXF_ABSOLUTEANGLE);
  SC_AddConst('SIXF_ABSOLUTEMOMENTUM', SIXF_ABSOLUTEMOMENTUM);
  SC_AddConst('SIXF_SETMASTER', SIXF_SETMASTER);
  SC_AddConst('SIXF_NOCHECKPOSITION', SIXF_NOCHECKPOSITION);
  SC_AddConst('SIXF_TELEFRAG', SIXF_TELEFRAG);
  SC_AddConst('SIXF_TRANSFERAMBUSHFLAG', SIXF_TRANSFERAMBUSHFLAG);
end;

procedure SC_ShutDownConsts;
var
  i, j: integer;
begin
  for i := 0 to CONSTSHASHSIZE - 1 do
  begin
    for j := 0 to constshashtable[i].Count - 1 do
      constshashtable[i].Objects[j].Free;
    constshashtable[i].Free;
  end;
end;

function SC_AddConst(const name: string; const value: integer): boolean;
var
  uToken: string;
  hash: integer;
begin
  uToken := strupper(strtrim(name));
  if uToken = '' then
  begin
    I_Warning('SC_AddConst(): Trying to add const in hash without a name, value="%d"'#13#10, [value]);
    result := false;
    exit;
  end;

  hash := _hash(uToken);
  if constshashtable[hash].IndexOf(uToken) >= 0 then
  begin
    I_Warning('SC_AddConst(): Trying to add const "%s" twice in hash, value="%d"'#13#10, [name, value]);
    result := false;
    exit;
  end;

  constshashtable[hash].AddObject(uToken, TInteger.Create(value));
  result := true;
end;

function SC_GetConst(const name: string; var value: integer): boolean;
var
  uToken: string;
  hash: integer;
  idx: integer;
begin
  uToken := strupper(strtrim(name));
  if uToken = '' then
  begin
    result := false;
    exit;
  end;

  hash := _hash(uToken);
  idx := constshashtable[hash].IndexOf(uToken);
  if idx >= 0 then
  begin
    value := (constshashtable[hash].Objects[idx] as TInteger).intnum;
    result := true;
    exit;
  end;

  result := false;
end;

end.
