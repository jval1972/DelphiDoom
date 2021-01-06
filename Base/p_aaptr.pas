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
// DESCRIPTION:
//  AAPTR
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit p_aaptr;

interface

uses
  p_mobj_h;

const
  AAPTR_DEFAULT = 0;
  AAPTR_NULL = $1;
  AAPTR_TARGET = $2;
  AAPTR_MASTER = $4;
  AAPTR_TRACER = $8;

  AAPTR_PLAYER_GETTARGET = $10;
  AAPTR_PLAYER_GETCONVERSATION = $20;

  AAPTR_PLAYER1 = $40;
  AAPTR_PLAYER2 = $80;
  AAPTR_PLAYER3 = $100;
  AAPTR_PLAYER4 = $200;
  AAPTR_PLAYER5 = $400;
  AAPTR_PLAYER6 = $800;
  AAPTR_PLAYER7 = $1000;
  AAPTR_PLAYER8 = $2000;

  AAPTR_FRIENDPLAYER = $4000;
  AAPTR_GET_LINETARGET = $8000;

  AAPTR_PLAYER_SELECTORS =
    AAPTR_PLAYER_GETTARGET or AAPTR_PLAYER_GETCONVERSATION;

  AAPTR_GENERAL_SELECTORS =
    AAPTR_TARGET or AAPTR_MASTER or AAPTR_TRACER or AAPTR_FRIENDPLAYER or AAPTR_GET_LINETARGET;

  AAPTR_STATIC_SELECTORS =
    AAPTR_PLAYER1 or AAPTR_PLAYER2 or AAPTR_PLAYER3 or AAPTR_PLAYER4 or
    AAPTR_PLAYER5 or AAPTR_PLAYER6 or AAPTR_PLAYER7 or AAPTR_PLAYER8 or
    AAPTR_NULL;

const
  PTROP_UNSAFETARGET = 1;
  PTROP_UNSAFEMASTER = 2;
  PTROP_NOSAFEGUARDS = PTROP_UNSAFETARGET or PTROP_UNSAFEMASTER;

function COPY_AAPTR(const origin: Pmobj_t; const selector: integer): Pmobj_t;

procedure ASSIGN_AAPTR(const toActor: Pmobj_t; const toSlot: integer; const ptr: Pmobj_t; const flags: integer);

implementation

uses
  d_delphi,
  doomdef,
  {$IFDEF HEXEN}
  p_common,
  {$ELSE}
  p_pspr,
  {$ENDIF}
  d_player,
  g_game,
  p_map;

function AAPTR_RESOLVE_PLAYERNUM(const playernum: integer): Pmobj_t;
begin
  if not IsIntegerInRange(playernum, 0, MAXPLAYERS - 1) then
  begin
    result := nil;
    exit;
  end;

  if playeringame[playernum] then
    result := players[playernum].mo
  else
    result := nil;
end;

function COPY_AAPTR(const origin: Pmobj_t; const selector: integer): Pmobj_t;
var
  i: integer;
begin
  if selector = AAPTR_DEFAULT then
  begin
    result := origin;
    exit;
  end;

  if origin <> nil then
  begin
    if origin.player <> nil then
    begin
      case selector and AAPTR_PLAYER_SELECTORS of
      AAPTR_PLAYER_GETTARGET:
        begin
          P_BulletSlope(origin);
          result := linetarget;
          exit;
        end;
      AAPTR_PLAYER_GETCONVERSATION:
        begin
          {$IFDEF STRIFE}
          result := Pplayer_t(origin.player).lastdialogtalker;
          {$ELSE}
          result := nil;
          {$ENDIF}
          exit;
        end;
      end;
    end;
  end;

  case selector and AAPTR_GENERAL_SELECTORS of
  AAPTR_TARGET:
    begin
      result := origin.target;
      exit;
    end;(*  unsupported
  AAPTR_MASTER:
    begin
      result := origin.master;
      exit;
    end;  *)
  AAPTR_TRACER:
    begin
      result := origin.tracer;
      exit;
    end;
  AAPTR_FRIENDPLAYER:
    begin
      if origin.player <> nil then
        if netgame and (deathmatch = 0) then
          for i := 0 to MAXPLAYERS - 1 do
            if playeringame[i] then
              if origin.player <> @players[i] then
              begin
                result := players[i].mo;
                exit;
              end;

      {$IFDEF STRIFE}
      if origin.flags and MF_ALLY <> 0 then
      {$ELSE}
      if origin.spawnpoint.flags and MTF_FRIEND then
      //  if origin.flags3_ex and MF3_EX_FRIEND <> 0 then
      {$ENDIF}
        for i := 0 to MAXPLAYERS - 1 do
          if playeringame[i] then
          begin
            result := players[i].mo;
            exit;
          end;

      result := nil;
      exit;
    end;
  AAPTR_GET_LINETARGET:
    begin
      P_BulletSlope(origin);
      result := linetarget;
      exit;
    end;
  end;

  case selector and AAPTR_STATIC_SELECTORS of
    AAPTR_PLAYER1: begin result := AAPTR_RESOLVE_PLAYERNUM(0); exit; end;
    AAPTR_PLAYER2: begin result := AAPTR_RESOLVE_PLAYERNUM(1); exit; end;
    AAPTR_PLAYER3: begin result := AAPTR_RESOLVE_PLAYERNUM(2); exit; end;
    AAPTR_PLAYER4: begin result := AAPTR_RESOLVE_PLAYERNUM(3); exit; end;
    AAPTR_PLAYER5: begin result := AAPTR_RESOLVE_PLAYERNUM(4); exit; end;
    AAPTR_PLAYER6: begin result := AAPTR_RESOLVE_PLAYERNUM(5); exit; end;
    AAPTR_PLAYER7: begin result := AAPTR_RESOLVE_PLAYERNUM(6); exit; end;
    AAPTR_PLAYER8: begin result := AAPTR_RESOLVE_PLAYERNUM(7); exit; end;
    AAPTR_NULL:
      begin
        result := nil;
        exit;
      end;
  end;

  result := origin;
end;

procedure ASSIGN_AAPTR(const toActor: Pmobj_t; const toSlot: integer; const ptr: Pmobj_t; const flags: integer);
begin
  case toSlot of
  AAPTR_TARGET: toActor.target := ptr;
  AAPTR_TRACER: toActor.tracer := ptr;
//  AAPTR_MASTER: toActor.master := ptr;
  end;
end;

end.
