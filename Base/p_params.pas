//------------------------------------------------------------------------------
//
//  DelphiDoom: A modified and improved DOOM engine for Windows
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 2004-2016 by Jim Valavanis
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
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : http://sourceforge.net/projects/delphidoom/
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

procedure P_RemoveMobjCustomParams(const parm: Pmobjcustomparam_t);

function P_SetMobjCustomParam(const actor1: pointer; const name: string; const value: integer): Pmobjcustomparam_t;

function P_GetMobjCustomParam(const actor1: pointer; const name: string): Pmobjcustomparam_t;

function P_GetMobjCustomParamValue(const actor1: pointer; const name: string): integer;

implementation

uses
  d_delphi,
  p_mobj_h,
  z_zone;

procedure P_RemoveMobjCustomParams(const parm: Pmobjcustomparam_t);
begin
  if parm <> nil then
  begin
    if parm.next <> nil then
      P_RemoveMobjCustomParams(parm.next);
    Z_Free(parm);
  end;
end;

function P_SetMobjCustomParam(const actor1: pointer; const name: string; const value: integer): Pmobjcustomparam_t;
var
  check: Pmobjcustomparam_t;
  actor: Pmobj_t;
begin
  actor := actor1;
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

function P_GetMobjCustomParam(const actor1: pointer; const name: string): Pmobjcustomparam_t;
var
  check: string;
  actor: Pmobj_t;
begin
  actor := actor1;
  check := strupper(name);
  result := actor.customparams;
  while result <> nil do
  begin
    if result.name = check then
      Exit;
    result := result.next;
  end;
  result := nil; // jval: unneeded 
end;

function P_GetMobjCustomParamValue(const actor1: pointer; const name: string): integer;
var
  parm: Pmobjcustomparam_t;
  actor: Pmobj_t;
begin
  actor := actor1;
  parm := P_GetMobjCustomParam(actor, name);
  if parm <> nil then
    result := parm.value
  else
    result := 0;
end;

end.
