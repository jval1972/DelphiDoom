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
//  DESCRIPTION:
//   Custom mobj params.
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit p_params;

interface

type
  Pmobjcustomparam_t = ^mobjcustomparam_t;
  mobjcustomparam_t = record
    name: string[64];
    value: integer;
    next: Pmobjcustomparam_t;
  end;

//==============================================================================
//
// P_RemoveMobjCustomParams
//
//==============================================================================
procedure P_RemoveMobjCustomParams(const parm: Pmobjcustomparam_t);

//==============================================================================
//
// P_SetMobjCustomParam
//
//==============================================================================
function P_SetMobjCustomParam(const actor1: pointer; const name1: string; const value: integer): Pmobjcustomparam_t;

//==============================================================================
//
// P_GetMobjCustomParam
//
//==============================================================================
function P_GetMobjCustomParam(const actor1: pointer; const name1: string): Pmobjcustomparam_t;

//==============================================================================
//
// P_GetMobjCustomParamValue
//
//==============================================================================
function P_GetMobjCustomParamValue(const actor1: pointer; const name1: string; const def: integer = 0): integer;

implementation

uses
  d_delphi,
  p_mobj_h,
  z_zone;

//==============================================================================
//
// P_RemoveMobjCustomParams
//
//==============================================================================
procedure P_RemoveMobjCustomParams(const parm: Pmobjcustomparam_t);
begin
  if parm <> nil then
  begin
    if parm.next <> nil then
      P_RemoveMobjCustomParams(parm.next);
    Z_Free(parm);
  end;
end;

//==============================================================================
//
// P_SetMobjCustomParam
//
//==============================================================================
function P_SetMobjCustomParam(const actor1: pointer; const name1: string; const value: integer): Pmobjcustomparam_t;
var
  check: Pmobjcustomparam_t;
  actor: Pmobj_t;
  name: string;
begin
  actor := actor1;
  name := RemoveQuotesFromString(name1);
  check := P_GetMobjCustomParam(actor, name);
  if check = nil then
  begin
    check := actor.customparams;
    actor.customparams := Z_Malloc(SizeOf(mobjcustomparam_t), PU_LEVEL, nil);
    actor.customparams.name := strupper(name);
    actor.customparams.value := value;
    actor.customparams.next := check;
    result := actor.customparams;
  end
  else
  begin
    check.value := value;
    result := check;
  end;
end;

//==============================================================================
//
// P_GetMobjCustomParam
//
//==============================================================================
function P_GetMobjCustomParam(const actor1: pointer; const name1: string): Pmobjcustomparam_t;
var
  check: string;
  actor: Pmobj_t;
  name: string;
begin
  actor := actor1;
  if actor = nil then
  begin
    result := nil;
    exit;
  end;
  name := RemoveQuotesFromString(name1);
  check := strupper(name);
  result := actor.customparams;
  while result <> nil do
  begin
    if result.name = check then
      Exit;
    result := result.next;
  end;
  result := nil; // JVAL: unneeded
end;

//==============================================================================
//
// P_GetMobjCustomParamValue
//
//==============================================================================
function P_GetMobjCustomParamValue(const actor1: pointer; const name1: string; const def: integer = 0): integer;
var
  parm: Pmobjcustomparam_t;
  actor: Pmobj_t;
  name: string;
begin
  actor := actor1;
  if actor = nil then
  begin
    result := def;
    exit;
  end;
  name := RemoveQuotesFromString(name1);
  parm := P_GetMobjCustomParam(actor, name);
  if parm <> nil then
    result := parm.value
  else
    result := def;
end;

end.
