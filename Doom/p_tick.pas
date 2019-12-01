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
//  DESCRIPTION:
//    Archiving: SaveGame I/O.
//    Thinker, Ticker.
//
//------------------------------------------------------------------------------
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit p_tick;

interface

uses
  d_think;

//
// THINKERS
// All thinkers should be allocated by Z_Malloc
// so they can be operated on uniformly.
// The actual structures will vary in size,
// but the first element must be thinker_t.
//

var
// Both the head and tail of the thinker list.
  thinkercap: thinker_t;

procedure P_InitThinkers;

procedure P_AddThinker(thinker: Pthinker_t);

procedure P_RemoveThinker(thinker: Pthinker_t);

procedure P_Ticker;

var
  leveltime: integer;
  isgamesuspended: boolean = true;

const
  TICKRATE = 35;

implementation

uses
  doomdef,
  c_con,
  d_player,
  g_game,
  m_menu,
  p_user,
  p_spec,
  p_mobj,
  z_zone;

procedure P_InitThinkers;
begin
  thinkercap.prev := @thinkercap;
  thinkercap.next := @thinkercap;
end;

//
// P_AddThinker
// Adds a new thinker at the end of the list.
//
procedure P_AddThinker(thinker: Pthinker_t);
begin
  thinkercap.prev.next := thinker;
  thinker.next := @thinkercap;
  thinker.prev := thinkercap.prev;
  thinkercap.prev := thinker;
end;

//
// P_RemoveThinker
// Deallocation is lazy -- it will not actually be freed
// until its thinking turn comes up.
//
procedure P_RemoveThinker(thinker: Pthinker_t);
begin
  // FIXME: NOP.
  thinker._function.acv := nil;
end;

//
// P_RunThinkers
//
procedure P_RunThinkers;
var
  currentthinker: Pthinker_t;
begin
  currentthinker := thinkercap.next;
  while currentthinker <> @thinkercap do
  begin
    if not Assigned(currentthinker._function.acv) then
    begin
      // time to remove it
      currentthinker.next.prev := currentthinker.prev;
      currentthinker.prev.next := currentthinker.next;
      Z_Free(currentthinker);
    end
    else
    begin
      if Assigned(currentthinker._function.acp1) then
        currentthinker._function.acp1(currentthinker);
    end;
    currentthinker := currentthinker.next;
  end;
end;

//
// P_Ticker
//
procedure P_Ticker;
var
  i: integer;
begin
  isgamesuspended := true;
  // run the tic
  if paused then
    exit;

  // pause if in menu and at least one tic has been run
  if (not netgame) and menuactive and
     (not demoplayback) and (players[consoleplayer].viewz <> 1) then
    exit;

  if (not demoplayback) and (not demorecording) and C_IsConsoleActive and (not netgame) and (leveltime <> 0) then
    exit;   

  isgamesuspended := false;

  for i := 0 to MAXPLAYERS - 1 do
    if playeringame[i] then
      P_PlayerThink(@players[i]);

  P_RunThinkers;
  P_UpdateSpecials;
  P_RespawnSpecials;

  // for par times
  inc(leveltime);
end;

end.
