//------------------------------------------------------------------------------
//
//  DelphiStrife: A modified and improved Strife source port for Windows.
//
//  Based on:
//    - Linux Doom by "id Software"
//    - Chocolate Strife by "Simon Howard"
//    - DelphiDoom by "Jim Valavanis"
//
//  Copyright (C) 1993-1996 by id Software, Inc.
//  Copyright (C) 2005 Simon Howard
//  Copyright (C) 2010 James Haley, Samuel Villarreal
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
//  DESCRIPTION:
//    Savegame I/O, archiving, persistence.
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit p_saveg;

interface

uses
  d_delphi;

// Persistent storage/archiving.
// These are the load / save game routines.
procedure P_ArchivePlayers;

procedure P_UnArchivePlayers(userload: boolean);

procedure P_ArchiveWorld;

procedure P_UnArchiveWorld;

procedure P_ArchiveThinkers;

procedure P_UnArchiveThinkers;

procedure P_ArchiveSpecials;

procedure P_UnArchiveSpecials;

procedure P_ArchiveMapVariables;

procedure P_ArchiveWorldVariables(const fname: string);

procedure P_UnArchiveMapVariables;

procedure P_UnArchiveWorldVariables(const fname: string);

procedure P_ArchivePSMapScript;

procedure P_UnArchivePSMapScript;

procedure P_ArchiveOverlay;

procedure P_UnArchiveOverlay;

procedure P_ArchiveScreenShot(const fname: string);

var
  save_p: PByteArray;
  savegameversion: integer;
  savegameversionhack: integer;

implementation

uses
  doomdef,
  d_ticcmd,
  d_player,
  d_think,
  g_game,
  m_fixed,
  m_misc,
  mn_screenshot,
  info_h,
  info,
  i_system,
  i_tmp,
  p_3dfloors,
  p_local,
  p_pspr_h,
  p_setup,
  p_mobj_h,
  p_mobj,
  p_mobjlist,
  p_tick,
  p_maputl,
  p_spec,
  p_ceilng,
  p_doors,
  p_floor,
  p_plats,
  p_lights,
  p_scroll,
  p_params,
  p_levelinfo,
  ps_main,
  psi_globals,
  psi_overlay,
  r_defs,
  r_data,
  r_colormaps,
  w_wad,
  z_zone;

// Pads save_p to a 4-byte boundary
//  so that the load/save works on SGI&Gecko.

procedure PADSAVEP;
begin
  if savegameversion < VERSION122 then
    save_p := PByteArray(integer(save_p) + ((4 - (integer(save_p) and 3) and 3)));
end;

//
// P_ArchivePlayers
//
procedure P_ArchivePlayers;
var
  i: integer;
  j: integer;
  dest: Pplayer_t;
begin
  for i := 0 to MAXPLAYERS - 1 do
  begin
    if not playeringame[i] then
      continue;

    PADSAVEP;

    dest := Pplayer_t(save_p);
    memcpy(dest, @players[i], SizeOf(player_t));
    save_p := PByteArray(integer(save_p) + SizeOf(player_t));
    for j := 0 to Ord(NUMPSPRITES) - 1 do
      if dest.psprites[j].state <> nil then
        dest.psprites[j].state := Pstate_t(pDiff(dest.psprites[j].state, @states[0], SizeOf(dest.psprites[j].state^)));
    if dest.lastdialogtalker <> nil then
      dest.lastdialogtalker := Pmobj_t(dest.lastdialogtalker.key);
  end;
end;

//
// P_UnArchivePlayers
//
procedure P_UnArchivePlayers(userload: boolean);
var
  i: integer;
  j: integer;
  p203: Pplayer_t203;
  p205: Pplayer_t205;
  p206: Pplayer_t206;
begin
  for i := 0 to MAXPLAYERS - 1 do
  begin
    if not playeringame[i] then
      continue;

    PADSAVEP;

    if savegameversion >= VERSION207 then
    begin
      if userload then
        memcpy(@players[i], save_p, SizeOf(player_t));
      incp(pointer(save_p), SizeOf(player_t));
    end
    else if savegameversion >= VERSION206 then
    begin
      if userload then
      begin
        p206 := Pplayer_t206(save_p);

        players[i].mo := p206.mo;
        players[i].playerstate := p206.playerstate;
        players[i].cmd202.forwardmove := p206.cmd202.forwardmove;
        players[i].cmd202.sidemove := p206.cmd202.sidemove;
        players[i].cmd202.angleturn := p206.cmd202.angleturn;
        players[i].cmd202.consistancy := p206.cmd202.consistancy;
        players[i].cmd202.chatchar := p206.cmd202.chatchar;
        players[i].cmd202.buttons := p206.cmd202.buttons;
        players[i].cmd202.buttons2 := p206.cmd202.buttons2;
        players[i].cmd202.inventory := p206.cmd202.inventory;
        players[i].cmd202.commands := p206.cmd202.commands;
        players[i].cmd202.lookupdown := p206.cmd202.lookupdown;
        players[i].cmd202.lookleftright := p206.cmd202.lookleftright;
        players[i].cmd202.jump := p206.cmd202.jump;
        players[i].viewz := p206.viewz;
        players[i].viewheight := p206.viewheight;
        players[i].deltaviewheight := p206.deltaviewheight;
        players[i].bob := p206.bob;
        players[i].lookdir := p206.lookdir;
        players[i].centering := p206.centering;
        players[i].lookdir2 := p206.lookdir2;
        players[i].oldlook2 := p206.oldlook2;
        players[i].forwarding := p206.forwarding;
        players[i].oldjump := p206.oldjump;
        players[i].health := p206.health;
        players[i].armorpoints := p206.armorpoints;
        players[i].armortype := p206.armortype;
        for j := 0 to Ord(NUMPOWERS) - 1 do
          players[i].powers[j] := p206.powers[j];
        players[i].sigiltype := p206.sigiltype;
        players[i].nukagecount := p206.nukagecount;
        players[i].questflags := p206.questflags;
        players[i].centerview := p206.centerview;
        players[i].inventory := p206.inventory;
        players[i].st_update := p206.st_update;
        players[i].numinventory := p206.numinventory;
        players[i].inventorycursor := p206.inventorycursor;
        players[i].accuracy := p206.accuracy;
        players[i].stamina := p206.stamina;
        for j := 0 to Ord(NUMCARDS) - 1 do
          players[i].cards[j] := p206.cards[j];
        players[i].backpack := p206.backpack;
        for j := 0 to MAXPLAYERS - 1 do
          players[i].frags[j] := p206.frags[j];
        players[i].readyweapon := p206.readyweapon;
        players[i].pendingweapon := p206.pendingweapon;
        for j := 0 to Ord(NUMWEAPONS) - 1 do
          players[i].weaponowned[j] := p206.weaponowned[j];
        for j := 0 to Ord(NUMAMMO) - 1 do
        begin
          players[i].ammo[j] := p206.ammo[j];
          players[i].maxammo[j] := p206.maxammo[j];
        end;
        players[i].attackdown := p206.attackdown;
        players[i].usedown := p206.usedown;
        players[i].inventorydown := p206.inventorydown;
        players[i].cheats := p206.cheats;
        players[i].refire := p206.refire;
        players[i].killcount := p206.killcount;
        players[i]._message := p206._message;
        players[i].damagecount := p206.damagecount;
        players[i].bonuscount := p206.bonuscount;
        players[i].attacker := p206.attacker;
        players[i].extralight := p206.extralight;
        players[i].fixedcolormap := p206.fixedcolormap;
        players[i].colormap := p206.colormap;
        for j := 0 to Ord(NUMPSPRITES) - 1 do
          players[i].psprites[j] := p206.psprites[j];
        players[i].attackerx := p206.attackerx;
        players[i].attackery := p206.attackery;
        players[i].lastbreath := p206.lastbreath;
        players[i].hardbreathtics := p206.hardbreathtics;
        players[i].angletargetx := p206.angletargetx;
        players[i].angletargety := p206.angletargety;
        players[i].angletargetticks := p206.angletargetticks;
        players[i].allegiance := p206.allegiance;
        for j := 0 to 39 do
          players[i].mapstate[j] := p206.mapstate[j];
        players[i].laddertics := p206.laddertics;
        players[i].viewbob := p206.viewbob;
        players[i].slopetics := p206.slopetics;
        players[i].oldviewz := p206.oldviewz;
        players[i].teleporttics := p206.teleporttics;
        players[i].quaketics := p206.quaketics;
        players[i].lookdir16 := p206.lookdir16;
        players[i].cmd.forwardmove := p206.cmd.forwardmove;
        players[i].cmd.sidemove := p206.cmd.sidemove;
        players[i].cmd.angleturn := p206.cmd.angleturn;
        players[i].cmd.consistancy := p206.cmd.consistancy;
        players[i].cmd.chatchar := p206.cmd.chatchar;
        players[i].cmd.buttons := p206.cmd.buttons;
        players[i].cmd.buttons2 := p206.cmd.buttons2;
        players[i].cmd.inventory := p206.cmd.inventory;
        players[i].cmd.commands := p206.cmd.commands;
        players[i].cmd.lookupdown := p206.cmd.lookupdown;
        players[i].cmd.lookleftright := p206.cmd.lookleftright;
        players[i].cmd.jump_crouch := p206.cmd.jump_crouch;
        players[i].cmd.lookupdown16 := p206.cmd.lookupdown16;
        if savegameversionhack = 0 then
        begin
          players[i].nextoof := p206.nextoof;
          players[i].lastdialogtalker := p206.lastdialogtalker;
          players[i].quakeintensity := p206.quakeintensity;
        end
        else if savegameversionhack = 1 then
        begin
          players[i].nextoof := 0;
          players[i].lastdialogtalker := nil;
          if p206.quaketics > 0 then
            players[i].quakeintensity := FRACUNIT
          else
            players[i].quakeintensity := 0;
          incp(pointer(save_p), -12);
        end;

        // version 207
        players[i].oldcrouch := 0;
        players[i].lastongroundtime := 0;
        players[i].lastautocrouchtime := 0;
        players[i].crouchheight := 0;
      end;
      incp(pointer(save_p), SizeOf(player_t206));
    end
    else if savegameversion >= VERSION204 then
    begin
      if userload then
      begin
        p205 := Pplayer_t205(save_p);

        players[i].mo := p205.mo;
        players[i].playerstate := p205.playerstate;
        players[i].cmd202.forwardmove := p205.cmd202.forwardmove;
        players[i].cmd202.sidemove := p205.cmd202.sidemove;
        players[i].cmd202.angleturn := p205.cmd202.angleturn;
        players[i].cmd202.consistancy := p205.cmd202.consistancy;
        players[i].cmd202.chatchar := p205.cmd202.chatchar;
        players[i].cmd202.buttons := p205.cmd202.buttons;
        players[i].cmd202.buttons2 := p205.cmd202.buttons2;
        players[i].cmd202.inventory := p205.cmd202.inventory;
        players[i].cmd202.commands := p205.cmd202.commands;
        players[i].cmd202.lookupdown := p205.cmd202.lookupdown;
        players[i].cmd202.lookleftright := p205.cmd202.lookleftright;
        players[i].cmd202.jump := p205.cmd202.jump;
        players[i].viewz := p205.viewz;
        players[i].viewheight := p205.viewheight;
        players[i].deltaviewheight := p205.deltaviewheight;
        players[i].bob := p205.bob;
        players[i].lookdir := p205.lookdir;
        players[i].centering := p205.centering;
        players[i].lookdir2 := p205.lookdir2;
        players[i].oldlook2 := p205.oldlook2;
        players[i].forwarding := p205.forwarding;
        players[i].oldjump := p205.oldjump;
        players[i].health := p205.health;
        players[i].armorpoints := p205.armorpoints;
        players[i].armortype := p205.armortype;
        for j := 0 to Ord(NUMPOWERS) - 1 do
          players[i].powers[j] := p205.powers[j];
        players[i].sigiltype := p205.sigiltype;
        players[i].nukagecount := p205.nukagecount;
        players[i].questflags := p205.questflags;
        players[i].centerview := p205.centerview;
        players[i].inventory := p205.inventory;
        players[i].st_update := p205.st_update;
        players[i].numinventory := p205.numinventory;
        players[i].inventorycursor := p205.inventorycursor;
        players[i].accuracy := p205.accuracy;
        players[i].stamina := p205.stamina;
        for j := 0 to Ord(NUMCARDS) - 1 do
          players[i].cards[j] := p205.cards[j];
        players[i].backpack := p205.backpack;
        for j := 0 to MAXPLAYERS - 1 do
          players[i].frags[j] := p205.frags[j];
        players[i].readyweapon := p205.readyweapon;
        players[i].pendingweapon := p205.pendingweapon;
        for j := 0 to Ord(NUMWEAPONS) - 1 do
          players[i].weaponowned[j] := p205.weaponowned[j];
        for j := 0 to Ord(NUMAMMO) - 1 do
        begin
          players[i].ammo[j] := p205.ammo[j];
          players[i].maxammo[j] := p205.maxammo[j];
        end;
        players[i].attackdown := p205.attackdown;
        players[i].usedown := p205.usedown;
        players[i].inventorydown := p205.inventorydown;
        players[i].cheats := p205.cheats;
        players[i].refire := p205.refire;
        players[i].killcount := p205.killcount;
        players[i]._message := p205._message;
        players[i].damagecount := p205.damagecount;
        players[i].bonuscount := p205.bonuscount;
        players[i].attacker := p205.attacker;
        players[i].extralight := p205.extralight;
        players[i].fixedcolormap := p205.fixedcolormap;
        players[i].colormap := p205.colormap;
        for j := 0 to Ord(NUMPSPRITES) - 1 do
          players[i].psprites[j] := p205.psprites[j];
        players[i].attackerx := p205.attackerx;
        players[i].attackery := p205.attackery;
        players[i].lastbreath := p205.lastbreath;
        players[i].hardbreathtics := p205.hardbreathtics;
        players[i].angletargetx := p205.angletargetx;
        players[i].angletargety := p205.angletargety;
        players[i].angletargetticks := p205.angletargetticks;
        players[i].allegiance := p205.allegiance;
        for j := 0 to 39 do
          players[i].mapstate[j] := p205.mapstate[j];
        players[i].laddertics := p205.laddertics;
        players[i].viewbob := p205.viewbob;
        players[i].slopetics := p205.slopetics;
        players[i].oldviewz := p205.oldviewz;
        players[i].teleporttics := p205.teleporttics;
        players[i].quaketics := p205.quaketics;
        players[i].lookdir16 := p205.lookdir16;
        players[i].cmd.forwardmove := p205.cmd.forwardmove;
        players[i].cmd.sidemove := p205.cmd.sidemove;
        players[i].cmd.angleturn := p205.cmd.angleturn;
        players[i].cmd.consistancy := p205.cmd.consistancy;
        players[i].cmd.chatchar := p205.cmd.chatchar;
        players[i].cmd.buttons := p205.cmd.buttons;
        players[i].cmd.buttons2 := p205.cmd.buttons2;
        players[i].cmd.inventory := p205.cmd.inventory;
        players[i].cmd.commands := p205.cmd.commands;
        players[i].cmd.lookupdown := p205.cmd.lookupdown;
        players[i].cmd.lookleftright := p205.cmd.lookleftright;
        players[i].cmd.jump_crouch := p205.cmd.jump_crouch;
        players[i].cmd.lookupdown16 := p205.cmd.lookupdown16;
        players[i].nextoof := 0;
        players[i].lastdialogtalker := nil;
        if players[i].quaketics > 0 then
          players[i].quakeintensity := FRACUNIT
        else
          players[i].quakeintensity := 0;

        // version 207
        players[i].oldcrouch := 0;
        players[i].lastongroundtime := 0;
        players[i].lastautocrouchtime := 0;
        players[i].crouchheight := 0;
      end;
      incp(pointer(save_p), SizeOf(player_t205));
    end
    else if savegameversion = VERSION203 then
    begin
      if userload then
      begin
        p203 := Pplayer_t203(save_p);

        players[i].mo := p203.mo;
        players[i].playerstate := p203.playerstate;
        players[i].cmd202.forwardmove := p203.cmd202.forwardmove;
        players[i].cmd202.sidemove := p203.cmd202.sidemove;
        players[i].cmd202.angleturn := p203.cmd202.angleturn;
        players[i].cmd202.consistancy := p203.cmd202.consistancy;
        players[i].cmd202.chatchar := p203.cmd202.chatchar;
        players[i].cmd202.buttons := p203.cmd202.buttons;
        players[i].cmd202.buttons2 := p203.cmd202.buttons2;
        players[i].cmd202.inventory := p203.cmd202.inventory;
        players[i].cmd202.commands := p203.cmd202.commands;
        players[i].cmd202.lookupdown := p203.cmd202.lookupdown;
        players[i].cmd202.lookleftright := p203.cmd202.lookleftright;
        players[i].cmd202.jump := p203.cmd202.jump_crouch;
        players[i].viewz := p203.viewz;
        players[i].viewheight := p203.viewheight;
        players[i].deltaviewheight := p203.deltaviewheight;
        players[i].bob := p203.bob;
        players[i].lookdir := p203.lookdir;
        players[i].centering := p203.centering;
        players[i].lookdir2 := p203.lookdir2;
        players[i].oldlook2 := p203.oldlook2;
        players[i].forwarding := p203.forwarding;
        players[i].oldjump := p203.oldjump;
        players[i].health := p203.health;
        players[i].armorpoints := p203.armorpoints;
        players[i].armortype := p203.armortype;
        for j := 0 to Ord(NUMPOWERS) - 1 do
          players[i].powers[j] := p203.powers[j];
        players[i].sigiltype := p203.sigiltype;
        players[i].nukagecount := p203.nukagecount;
        players[i].questflags := p203.questflags;
        players[i].centerview := p203.centerview;
        players[i].inventory := p203.inventory;
        players[i].st_update := p203.st_update;
        players[i].numinventory := p203.numinventory;
        players[i].inventorycursor := p203.inventorycursor;
        players[i].accuracy := p203.accuracy;
        players[i].stamina := p203.stamina;
        for j := 0 to Ord(NUMCARDS) - 1 do
          players[i].cards[j] := p203.cards[j];
        players[i].backpack := p203.backpack;
        for j := 0 to MAXPLAYERS - 1 do
          players[i].frags[j] := p203.frags[j];
        players[i].readyweapon := p203.readyweapon;
        players[i].pendingweapon := p203.pendingweapon;
        for j := 0 to Ord(NUMWEAPONS) - 1 do
          players[i].weaponowned[j] := p203.weaponowned[j];
        for j := 0 to Ord(NUMAMMO) - 1 do
        begin
          players[i].ammo[j] := p203.ammo[j];
          players[i].maxammo[j] := p203.maxammo[j];
        end;
        players[i].attackdown := p203.attackdown;
        players[i].usedown := p203.usedown;
        players[i].inventorydown := p203.inventorydown;
        players[i].cheats := p203.cheats;
        players[i].refire := p203.refire;
        players[i].killcount := p203.killcount;
        players[i]._message := p203._message;
        players[i].damagecount := p203.damagecount;
        players[i].bonuscount := p203.bonuscount;
        players[i].attacker := p203.attacker;
        players[i].extralight := p203.extralight;
        players[i].fixedcolormap := p203.fixedcolormap;
        players[i].colormap := p203.colormap;
        for j := 0 to Ord(NUMPSPRITES) - 1 do
          players[i].psprites[j] := p203.psprites[j];
        players[i].attackerx := p203.attackerx;
        players[i].attackery := p203.attackery;
        players[i].lastbreath := p203.lastbreath;
        players[i].hardbreathtics := p203.hardbreathtics;
        players[i].angletargetx := p203.angletargetx;
        players[i].angletargety := p203.angletargety;
        players[i].angletargetticks := p203.angletargetticks;
        players[i].allegiance := p203.allegiance;
        for j := 0 to 39 do
          players[i].mapstate[j] := p203.mapstate[j];
        players[i].laddertics := p203.laddertics;
        players[i].viewbob := p203.viewbob;
        players[i].slopetics := p203.slopetics;
        players[i].oldviewz := p203.oldviewz;
        players[i].teleporttics := p203.teleporttics;
        players[i].quaketics := p203.quaketics;
        players[i].lookdir16 := p203.lookdir16;
        players[i].cmd.forwardmove := p203.cmd.forwardmove;
        players[i].cmd.sidemove := p203.cmd.sidemove;
        players[i].cmd.angleturn := p203.cmd.angleturn;
        players[i].cmd.consistancy := p203.cmd.consistancy;
        players[i].cmd.chatchar := p203.cmd.chatchar;
        players[i].cmd.buttons := p203.cmd.buttons;
        players[i].cmd.buttons2 := p203.cmd.buttons2;
        players[i].cmd.inventory := p203.cmd.inventory;
        players[i].cmd.commands := p203.cmd.commands;
        players[i].cmd.lookupdown := p203.cmd.lookupdown;
        players[i].cmd.lookleftright := p203.cmd.lookleftright;
        players[i].cmd.jump_crouch := p203.cmd.jump_crouch;
        players[i].cmd.lookupdown16 := p203.cmd.lookupdown16;
        players[i].nextoof := 0;
        players[i].lastdialogtalker := nil;
        if players[i].quaketics > 0 then
          players[i].quakeintensity := FRACUNIT
        else
          players[i].quakeintensity := 0;

        // version 207
        players[i].oldcrouch := 0;
        players[i].lastongroundtime := 0;
        players[i].lastautocrouchtime := 0;
        players[i].crouchheight := 0;
      end;
      incp(pointer(save_p), SizeOf(player_t203));
    end
    else if savegameversion = VERSION122 then
    begin
      if userload then
      begin
        memcpy(@players[i], save_p, SizeOf(player_t122));
        players[i].laddertics := 0;
        players[i].viewbob := players[i].bob;
        players[i].slopetics := 0; // JVAL: Slopes
        players[i].oldviewz := players[i].viewz;
        players[i].teleporttics := 0;
        players[i].quaketics := 0;
        players[i].lookdir16 := players[i].lookdir * 16;
        Pticcmd_t202(@players[i].cmd)^ := players[i].cmd202;
        players[i].cmd.lookupdown16 := players[i].cmd.lookupdown * 256;
        players[i].nextoof := 0;
        players[i].lastdialogtalker := nil;
        players[i].quakeintensity := 0;

        // version 207
        players[i].oldcrouch := 0;
        players[i].lastongroundtime := 0;
        players[i].lastautocrouchtime := 0;
        players[i].crouchheight := 0;
      end;
      incp(pointer(save_p), SizeOf(player_t122));
    end
    else if savegameversion <= VERSION121 then
    begin
      if userload then
      begin
        memcpy(@players[i], save_p, SizeOf(player_t121));
        players[i].laddertics := 0;
        players[i].viewbob := players[i].bob;
        players[i].slopetics := 0; // JVAL: Slopes
        players[i].oldviewz := players[i].viewz;
        players[i].teleporttics := 0;
        players[i].quaketics := 0;
        players[i].lookdir16 := players[i].lookdir * 16;
        Pticcmd_t202(@players[i].cmd)^ := players[i].cmd202;
        players[i].cmd.lookupdown16 := players[i].cmd.lookupdown * 256;
        players[i].nextoof := 0;
        players[i].lastdialogtalker := nil;
        players[i].quakeintensity := 0;

        // version 207
        players[i].oldcrouch := 0;
        players[i].lastongroundtime := 0;
        players[i].lastautocrouchtime := 0;
        players[i].crouchheight := 0;
      end;
      incp(pointer(save_p), SizeOf(player_t121));
    end
    else
      I_Error('P_UnArchivePlayers(): Unsupported saved game version: %d', [savegameversion]);

    // will be set when unarc thinker
    players[i].mo := nil;
    players[i]._message := '';
    players[i].attacker := nil;

    if userload then
      for j := 0 to Ord(NUMPSPRITES) - 1 do
        if players[i].psprites[j].state <> nil then
          players[i].psprites[j].state := @states[integer(players[i].psprites[j].state)];
  end;
end;

//
// P_ArchiveWorld
//
procedure P_ArchiveWorld;
var
  i: integer;
  j: integer;
  sec: Psector_t;
  li: Pline_t;
  si: Pside_t;
  put: PSmallIntArray;
  levelinf: Plevelinfo_t;
begin
  put := PSmallIntArray(save_p);

  levelinf := P_GetLevelInfo(P_GetMapName(gamemap));
  Pchar8_t(put)^ := levelinf.musname;
  put := @put[SizeOf(char8_t) div SizeOf(SmallInt)];
  Pchar8_t(put)^ := levelinf.skyflat;
  put := @put[SizeOf(char8_t) div SizeOf(SmallInt)];

  // do sectors
  i := 0;
  while i < numsectors do
  begin
    sec := Psector_t(@sectors[i]);
    PInteger(put)^ := sec.floorheight;
    put := @put[2];
    PInteger(put)^ := sec.ceilingheight;
    put := @put[2];

    Pchar8_t(put)^ := flats[sec.floorpic].name;
    put := @put[SizeOf(char8_t) div SizeOf(SmallInt)];
    Pchar8_t(put)^ := flats[sec.ceilingpic].name;
    put := @put[SizeOf(char8_t) div SizeOf(SmallInt)];

    put[0] := sec.lightlevel;
    put := @put[1];
    put[0] := sec.special; // needed?
    put := @put[1];
    put[0] := sec.tag;  // needed?
    put := @put[1];
    PInteger(put)^ := sec.floor_xoffs;
    put := @put[2];
    PInteger(put)^ := sec.floor_yoffs;
    put := @put[2];
    PInteger(put)^ := sec.ceiling_xoffs;
    put := @put[2];
    PInteger(put)^ := sec.ceiling_yoffs;
    put := @put[2];
    PLongWord(put)^ := sec.renderflags;
    put := @put[2];
    PLongWord(put)^ := sec.flags;
    put := @put[2];
    // JVAL: 3d Floors
    PInteger(put)^ := sec.midsec;
    put := @put[2];
    PInteger(put)^ := sec.midline;
    put := @put[2];
    // JVAL: sector gravity (VERSION 204)
    PInteger(put)^ := sec.gravity;
    put := @put[2];

    // JVAL: 20200221 - Texture angle
    PLongWord(put)^ := sec.floorangle;
    put := @put[2];
    PInteger(put)^ := sec.flooranglex;
    put := @put[2];
    PInteger(put)^ := sec.floorangley;
    put := @put[2];
    PLongWord(put)^ := sec.ceilingangle;
    put := @put[2];
    PInteger(put)^ := sec.ceilinganglex;
    put := @put[2];
    PInteger(put)^ := sec.ceilingangley;
    put := @put[2];

    // JVAL: 20200522 - Slope values
    Pfloat(put)^ := sec.fa;
    put := @put[SizeOf(float) div 2];
    Pfloat(put)^ := sec.fb;
    put := @put[SizeOf(float) div 2];
    Pfloat(put)^ := sec.fd;
    put := @put[SizeOf(float) div 2];
    Pfloat(put)^ := sec.fic;
    put := @put[SizeOf(float) div 2];
    Pfloat(put)^ := sec.ca;
    put := @put[SizeOf(float) div 2];
    Pfloat(put)^ := sec.cb;
    put := @put[SizeOf(float) div 2];
    Pfloat(put)^ := sec.cd;
    put := @put[SizeOf(float) div 2];
    Pfloat(put)^ := sec.cic;
    put := @put[SizeOf(float) div 2];

    PInteger(put)^ := sec.num_saffectees;
    put := @put[2];
    for j := 0 to sec.num_saffectees - 1 do
    begin
      PInteger(put)^ := sec.saffectees[j];
      put := @put[2];
    end;

    inc(i);
  end;

  // do lines
  i := 0;
  while i < numlines do
  begin
    li := Pline_t(@lines[i]);
    put[0] := li.flags;
    put := @put[1];
    put[0] := li.special;
    put := @put[1];
    put[0] := li.tag;
    put := @put[1];
    PLongWord(put)^ := li.renderflags;
    put := @put[2];
    for j := 0 to 1 do
    begin
      if li.sidenum[j] = -1 then
        continue;

      si := @sides[li.sidenum[j]];

      PInteger(put)^ := si.textureoffset;
      put := @put[2];
      PInteger(put)^ := si.rowoffset;
      put := @put[2];

      Pchar8_t(put)^ := R_NameForSideTexture(si.toptexture);
      put := @put[SizeOf(char8_t) div SizeOf(SmallInt)];
      Pchar8_t(put)^ := R_NameForSideTexture(si.bottomtexture);
      put := @put[SizeOf(char8_t) div SizeOf(SmallInt)];
      Pchar8_t(put)^ := R_NameForSideTexture(si.midtexture);
      put := @put[SizeOf(char8_t) div SizeOf(SmallInt)];
    end;
    inc(i);
  end;

  save_p := PByteArray(put);
end;

//
// P_UnArchiveWorld
//
procedure P_UnArchiveWorld;
var
  i: integer;
  j: integer;
  sec: Psector_t;
  li: Pline_t;
  si: Pside_t;
  get: PSmallIntArray;
  levelinf: Plevelinfo_t;
begin
  get := PSmallIntArray(save_p);

  if savegameversion >= VERSION205 then
  begin
    levelinf := P_GetLevelInfo(P_GetMapName(gamemap));
    levelinf.musname := Pchar8_t(get)^;
    get := @get[SizeOf(char8_t) div SizeOf(SmallInt)];
    levelinf.skyflat := Pchar8_t(get)^;
    get := @get[SizeOf(char8_t) div SizeOf(SmallInt)];
  end;

  // do sectors
  i := 0;
  while i < numsectors do
  begin
    sec := Psector_t(@sectors[i]);

    if savegameversion <= VERSION121 then
    begin
      sec.floorheight := get[0] * FRACUNIT;
      get := @get[1];
      sec.ceilingheight := get[0] * FRACUNIT;
      get := @get[1];
      sec.floorpic := get[0];
      get := @get[1];
      sec.ceilingpic := get[0];
      get := @get[1];
    end
    else
    begin
      sec.floorheight := PInteger(get)^;
      get := @get[2];
      sec.ceilingheight := PInteger(get)^;
      get := @get[2];
      sec.floorpic := R_FlatNumForName(Pchar8_t(get)^);
      get := @get[SizeOf(char8_t) div SizeOf(SmallInt)];
      sec.ceilingpic := R_FlatNumForName(Pchar8_t(get)^);
      get := @get[SizeOf(char8_t) div SizeOf(SmallInt)];
    end;

    sec.lightlevel := get[0];
    get := @get[1];
    sec.special := get[0]; // needed?
    get := @get[1];
    sec.tag := get[0]; // needed?
    get := @get[1];
    sec.floordata := nil;
    sec.ceilingdata := nil;
    sec.lightingdata := nil;
    sec.soundtarget := nil;

    if savegameversion <= VERSION121 then
    begin
      sec.floor_xoffs := get[0] * FRACUNIT;
      get := @get[1];
      sec.floor_yoffs := get[0] * FRACUNIT;
      get := @get[1];
      sec.ceiling_xoffs := get[0] * FRACUNIT;
      get := @get[1];
      sec.ceiling_yoffs := get[0] * FRACUNIT;
      get := @get[1];
      sec.midsec := -1;
      sec.midline := -1;
    end
    else
    begin
      sec.floor_xoffs := PInteger(get)^;
      get := @get[2];
      sec.floor_yoffs := PInteger(get)^;
      get := @get[2];
      sec.ceiling_xoffs := PInteger(get)^;
      get := @get[2];
      sec.ceiling_yoffs := PInteger(get)^;
      get := @get[2];
      sec.renderflags := PLongWord(get)^;
      get := @get[2];
      sec.flags := PLongWord(get)^;
      get := @get[2];
      sec.midsec := PInteger(get)^;
      get := @get[2];
      sec.midline := PInteger(get)^;
      get := @get[2];
    end;

    // JVAL: sector gravity (VERSION 204)
    if savegameversion >= VERSION204 then
    begin
      sec.gravity := PInteger(get)^;
      get := @get[2];
    end
    else
      sec.gravity := GRAVITY;

    // JVAL: 20200221 - Texture angle
    if savegameversion >= VERSION206 then
    begin
      sec.floorangle := PLongWord(get)^;
      get := @get[2];
      sec.flooranglex := PInteger(get)^;
      get := @get[2];
      sec.floorangley := PInteger(get)^;
      get := @get[2];
      sec.ceilingangle := PLongWord(get)^;
      get := @get[2];
      sec.ceilinganglex := PInteger(get)^;
      get := @get[2];
      sec.ceilingangley := PInteger(get)^;
      get := @get[2];

      // JVAL: 20200522 - Slope values
      sec.fa := Pfloat(get)^;
      get := @get[SizeOf(float) div 2];
      sec.fb := Pfloat(get)^;
      get := @get[SizeOf(float) div 2];
      sec.fd := Pfloat(get)^;
      get := @get[SizeOf(float) div 2];
      sec.fic := Pfloat(get)^;
      get := @get[SizeOf(float) div 2];
      sec.ca := Pfloat(get)^;
      get := @get[SizeOf(float) div 2];
      sec.cb := Pfloat(get)^;
      get := @get[SizeOf(float) div 2];
      sec.cd := Pfloat(get)^;
      get := @get[SizeOf(float) div 2];
      sec.cic := Pfloat(get)^;
      get := @get[SizeOf(float) div 2];
    end
    else
    begin
      sec.floorangle := 0;
      sec.flooranglex := 0;
      sec.floorangley := 0;
      sec.ceilingangle := 0;
      sec.ceilinganglex := 0;
      sec.ceilingangley := 0;
    end;

    if savegameversion >= VERSION122 then
    begin
      sec.num_saffectees := PInteger(get)^;
      get := @get[2];
      sec.saffectees := Z_Realloc(sec.saffectees, sec.num_saffectees * SizeOf(integer), PU_LEVEL, nil);
      for j := 0 to sec.num_saffectees - 1 do
      begin
        sec.saffectees[j] := PInteger(get)^;
        get := @get[2];
      end;
    end;

    sec.touching_thinglist := nil;
    sec.iSectorID := i;
    inc(i);
  end;

  // do lines
  i := 0;
  while i < numlines do
  begin
    li := Pline_t(@lines[i]);
    li.flags := get[0];
    get := @get[1];
    li.special := get[0];
    get := @get[1];
    li.tag := get[0];
    get := @get[1];
    if savegameversion >= VERSION122 then
    begin
      li.renderflags := PLongWord(get)^;
      get := @get[2];
    end;
    for j := 0 to 1 do
    begin
      if li.sidenum[j] = -1 then
        continue;
      si := @sides[li.sidenum[j]];

      if savegameversion <= VERSION121 then
      begin
        si.textureoffset := get[0] * FRACUNIT;
        get := @get[1];
        si.rowoffset := get[0] * FRACUNIT;
        get := @get[1];
        si.toptexture := get[0];
        get := @get[1];
        si.bottomtexture := get[0];
        get := @get[1];
        si.midtexture := get[0];
        get := @get[1];
      end
      else
      begin
        si.textureoffset := PInteger(get)^;
        get := @get[2];
        si.rowoffset := PInteger(get)^;
        get := @get[2];

        si.toptexture := R_SafeTextureNumForName(Pchar8_t(get)^);
        if si.toptexture = 0 then
          si.toptexture := -1 - R_CustomColorMapForName(Pchar8_t(get)^);
        get := @get[SizeOf(char8_t) div SizeOf(SmallInt)];

        si.bottomtexture := R_SafeTextureNumForName(Pchar8_t(get)^);
        if si.bottomtexture = 0 then
          si.bottomtexture := -1 - R_CustomColorMapForName(Pchar8_t(get)^);
        get := @get[SizeOf(char8_t) div SizeOf(SmallInt)];

        si.midtexture := R_SafeTextureNumForName(Pchar8_t(get)^);
        if si.midtexture = 0 then
          si.midtexture := -1 - R_CustomColorMapForName(Pchar8_t(get)^);
        get := @get[SizeOf(char8_t) div SizeOf(SmallInt)];
      end;
    end;
    inc(i);
  end;
  save_p := PByteArray(get);
end;

//
// Thinkers
//
type
  thinkerclass_t = (tc_end, tc_mobj);

//
// P_ArchiveThinkers
//
procedure P_ArchiveThinkers;
var
  th: Pthinker_t;
  mobj: Pmobj_t;
  parm, parm1: Pmobjcustomparam_t;
begin
  // save off the current thinkers
  th := thinkercap.next;
  while th <> @thinkercap do
  begin
    if @th._function.acp1 = @P_MobjThinker then
    begin
      save_p[0] := Ord(tc_mobj);
      save_p := @save_p[1];
      PADSAVEP;
      mobj := Pmobj_t(save_p);
      memcpy(mobj, th, SizeOf(mobj_t));
      incp(pointer(save_p), SizeOf(mobj_t));
      mobj.state := Pstate_t(pDiff(mobj.state, @states[0], SizeOf(state_t)));
      mobj.prevstate := Pstate_t(pDiff(mobj.prevstate, @states[0], SizeOf(state_t)));
      if mobj.tracer <> nil then
        mobj.tracer := Pmobj_t(mobj.tracer.key);
      if mobj.target <> nil then
        mobj.target := Pmobj_t(mobj.target.key);
      if mobj.master <> nil then
        mobj.master := Pmobj_t(mobj.master.key);

      if mobj.player <> nil then
        mobj.player := Pplayer_t(pDiff(mobj.player, @players[0], SizeOf(player_t)) + 1);

      parm := mobj.customparams;
      while parm <> nil do
      begin
        parm1 := Pmobjcustomparam_t(save_p);
        memcpy(parm1, parm, SizeOf(mobjcustomparam_t));
        incp(pointer(save_p), SizeOf(mobjcustomparam_t));
        parm := parm.next;
      end;

    end;
  // I_Error ("P_ArchiveThinkers: Unknown thinker function");
    th := th.next;
  end;

  // add a terminating marker
  save_p[0] := Ord(tc_end);
  save_p := @save_p[1];
end;

// P_UnArchiveThinkers
//
procedure P_UnArchiveThinkers;
var
  tclass: byte;
  currentthinker: Pthinker_t;
  next: Pthinker_t;
  mobj: Pmobj_t;
  parm: mobjcustomparam_t;
  i: integer;
begin
  // remove all the current thinkers
  currentthinker := thinkercap.next;
  while currentthinker <> @thinkercap do
  begin
    next := currentthinker.next;

    if @currentthinker._function.acp1 = @P_MobjThinker then
      P_RemoveMobj(Pmobj_t(currentthinker))
    else
      Z_Free(currentthinker);

    currentthinker := next;
  end;
  P_InitThinkers;

  // read in saved thinkers
  while true do
  begin
    tclass := save_p[0];
    save_p := @save_p[1];
    case tclass of
      Ord(tc_end):
        begin
          if savegameversion <> VERSION120 then
          begin
            currentthinker := thinkercap.next;
            while currentthinker <> @thinkercap do
            begin
              next := currentthinker.next;

              if @currentthinker._function.acp1 = @P_MobjThinker then
              begin
                Pmobj_t(currentthinker).target := P_FindMobjFromKey(integer(Pmobj_t(currentthinker).target));
                Pmobj_t(currentthinker).tracer := P_FindMobjFromKey(integer(Pmobj_t(currentthinker).tracer));
                Pmobj_t(currentthinker).master := P_FindMobjFromKey(integer(Pmobj_t(currentthinker).master));
              end;

              currentthinker := next;
            end;

            for i := 0 to MAXPLAYERS - 1 do
              if playeringame[i] then
                if players[i].lastdialogtalker <> nil then
                  players[i].lastdialogtalker := P_FindMobjFromKey(integer(players[i].lastdialogtalker));

          end;
          exit; // end of list
        end;

      Ord(tc_mobj):
        begin
          PADSAVEP;
          mobj := Z_Malloc(SizeOf(mobj_t), PU_LEVEL, nil);

          if savegameversion >= VERSION207 then
          begin
            memcpy(mobj, save_p, SizeOf(mobj_t));
            incp(pointer(save_p), SizeOf(mobj_t));
          end
          else if savegameversion >= VERSION206 then
          begin
            if savegameversionhack = 0 then
            begin
              memcpy(mobj, save_p, SizeOf(mobj_t206));
              incp(pointer(save_p), SizeOf(mobj_t206));
            end
            else
            begin
              memcpy(mobj, save_p, SizeOf(mobj_t205));
              incp(pointer(save_p), SizeOf(mobj_t205));

              // version 206
              mobj.mass := mobjinfo[Ord(mobj._type)].mass;
              mobj.args[0] := 0;
              mobj.args[1] := 0;
              mobj.args[2] := 0;
              mobj.args[3] := 0;
              mobj.args[4] := 0;
              mobj.special := 0;
              mobj.master := nil;
              mobj.WeaveIndexXY := 0;
              mobj.WeaveIndexZ := 0;
            end;

            // version 207
            mobj.painchance := mobjinfo[mobj._type].painchance;
            mobj.spriteDX := mobjinfo[Ord(mobj._type)].spriteDX;
            mobj.spriteDY := mobjinfo[Ord(mobj._type)].spriteDY;
          end
          else if savegameversion >= VERSION205 then
          begin
            memcpy(mobj, save_p, SizeOf(mobj_t205));
            incp(pointer(save_p), SizeOf(mobj_t205));

            // version 206
            mobj.mass := mobjinfo[Ord(mobj._type)].mass;
            mobj.args[0] := 0;
            mobj.args[1] := 0;
            mobj.args[2] := 0;
            mobj.args[3] := 0;
            mobj.args[4] := 0;
            mobj.special := 0;
            mobj.master := nil;
            mobj.WeaveIndexXY := 0;
            mobj.WeaveIndexZ := 0;

            // version 207
            mobj.painchance := mobjinfo[mobj._type].painchance;
            mobj.spriteDX := mobjinfo[Ord(mobj._type)].spriteDX;
            mobj.spriteDY := mobjinfo[Ord(mobj._type)].spriteDY;
          end
          else if savegameversion >= VERSION122 then
          begin
            memcpy(mobj, save_p, SizeOf(mobj_t204));
            incp(pointer(save_p), SizeOf(mobj_t204));

            mobj.lightvalidcount := 0;
            mobj.scale := FRACUNIT;
            mobj.pushfactor := FRACUNIT div 4;
            mobj.gravity := FRACUNIT;
            mobj.flags3_ex := 0;
            mobj.flags4_ex := 0;
            mobj.rendervalidcount := 0;

            // version 206
            mobj.mass := mobjinfo[Ord(mobj._type)].mass;
            mobj.args[0] := 0;
            mobj.args[1] := 0;
            mobj.args[2] := 0;
            mobj.args[3] := 0;
            mobj.args[4] := 0;
            mobj.special := 0;
            mobj.master := nil;
            mobj.WeaveIndexXY := 0;
            mobj.WeaveIndexZ := 0;

            // version 207
            mobj.painchance := mobjinfo[mobj._type].painchance;
            mobj.spriteDX := mobjinfo[Ord(mobj._type)].spriteDX;
            mobj.spriteDY := mobjinfo[Ord(mobj._type)].spriteDY;
          end
          else if savegameversion = VERSION121 then
          begin
            memcpy(mobj, save_p, SizeOf(mobj_t121));
            incp(pointer(save_p), SizeOf(mobj_t121));

            mobj.dropitem := 0;

            mobj.lightvalidcount := 0;
            mobj.scale := FRACUNIT;
            mobj.pushfactor := FRACUNIT div 4;
            mobj.gravity := FRACUNIT;
            mobj.flags3_ex := 0;
            mobj.flags4_ex := 0;
            mobj.rendervalidcount := 0;

            // version 206
            mobj.mass := mobjinfo[Ord(mobj._type)].mass;
            mobj.args[0] := 0;
            mobj.args[1] := 0;
            mobj.args[2] := 0;
            mobj.args[3] := 0;
            mobj.args[4] := 0;
            mobj.special := 0;
            mobj.master := nil;
            mobj.WeaveIndexXY := 0;
            mobj.WeaveIndexZ := 0;

            // version 207
            mobj.painchance := mobjinfo[mobj._type].painchance;
            mobj.spriteDX := mobjinfo[Ord(mobj._type)].spriteDX;
            mobj.spriteDY := mobjinfo[Ord(mobj._type)].spriteDY;
          end
          else if savegameversion = VERSION120 then
          begin
            memcpy(mobj, save_p, SizeOf(mobj_t120));
            incp(pointer(save_p), SizeOf(mobj_t120));

            mobj.prevx := mobj.x;
            mobj.prevy := mobj.y;
            mobj.prevz := mobj.z;
            mobj.nextx := mobj.x;
            mobj.nexty := mobj.y;
            mobj.nextz := mobj.z;
            mobj.prevangle := mobj.angle;
            mobj.nextangle := mobj.angle;
            mobj.intrplcnt := 0;

            mobj.dropitem := 0;

            mobj.lightvalidcount := 0;
            mobj.scale := FRACUNIT;
            mobj.pushfactor := FRACUNIT div 4;
            mobj.gravity := FRACUNIT;
            mobj.flags3_ex := 0;
            mobj.flags4_ex := 0;
            mobj.rendervalidcount := 0;

            // version 206
            mobj.mass := mobjinfo[Ord(mobj._type)].mass;
            mobj.args[0] := 0;
            mobj.args[1] := 0;
            mobj.args[2] := 0;
            mobj.args[3] := 0;
            mobj.args[4] := 0;
            mobj.special := 0;
            mobj.master := nil;
            mobj.WeaveIndexXY := 0;
            mobj.WeaveIndexZ := 0;

            // version 207
            mobj.painchance := mobjinfo[mobj._type].painchance;
            mobj.spriteDX := mobjinfo[Ord(mobj._type)].spriteDX;
            mobj.spriteDY := mobjinfo[Ord(mobj._type)].spriteDY;
          end
          else
            I_Error('P_UnArchiveThinkers(): Unsupported saved game version: %d', [savegameversion]);

          mobj.validcount := 0;
          mobj.lightvalidcount := 0;
          mobj.rendervalidcount := 0;

          if mobj.key < 1 then
            mobj.key := P_GenGlobalMobjKey;
          P_NotifyMobjKey(mobj);

          mobj.state := @states[integer(mobj.state)];
          mobj.prevstate := @states[integer(mobj.prevstate)];
          if savegameversion <= VERSION120 then
          begin
            mobj.target := nil;
            mobj.tracer := nil;
            mobj.master := nil;
          end;
          mobj.touching_sectorlist := nil;

          if mobj.customparams <> nil then
          begin
            mobj.customparams := nil;
            repeat
              memcpy(@parm, save_p, SizeOf(mobjcustomparam_t));
              incp(pointer(save_p), SizeOf(mobjcustomparam_t));
              P_SetMobjCustomParam(mobj, parm.name, parm.value);
            until parm.next = nil;
          end;

          if mobj.player <> nil then
          begin
            mobj.player := @players[integer(mobj.player) - 1];

            Pplayer_t(mobj.player).mo := mobj;
          end;

          P_SetThingPosition(mobj);
          mobj.info := @mobjinfo[Ord(mobj._type)];
          mobj.floorz := P_3dFloorHeight(mobj);
          mobj.ceilingz := P_3dCeilingHeight(mobj);
          @mobj.thinker._function.acp1 := @P_MobjThinker;
          P_AddThinker(@mobj.thinker);
        end;
      else
        I_Error('P_UnArchiveThinkers(): Unknown tclass %d in savegame', [tclass]);
    end;
  end;
end;

//
// P_ArchiveSpecials
//
type
  specials_e = (
    tc_ceiling,
    tc_door,
    tc_slidingdoor,
    tc_floor,
    tc_plat,
    tc_flash,
    tc_strobe,
    tc_glow,
    tc_scroll,
    tc_friction,    // phares 3/18/98:  new friction effect thinker
    tc_pusher,      // phares 3/22/98:  new push/pull effect thinker
    tc_fireflicker, // JVAL 20171211
    tc_elevator,    //jff 2/22/98 new elevator type thinker
    tc_endspecials
  );



//
// Things to handle:
//
// T_MoveCeiling, (ceiling_t: sector_t * swizzle), - active list
// T_VerticalDoor, (vldoor_t: sector_t * swizzle),
// T_MoveFloor, (floormove_t: sector_t * swizzle),
// T_LightFlash, (lightflash_t: sector_t * swizzle),
// T_StrobeFlash, (strobe_t: sector_t *),
// T_Glow, (glow_t: sector_t *),
// T_PlatRaise, (plat_t: sector_t *), - active list
//
procedure P_ArchiveSpecials;
var
  th: Pthinker_t;
  th1: Pthinker_t;
  ceiling: Pceiling_t;
  door: Pvldoor_t;
  sdoor: Pslidedoor_t;
  floor: Pfloormove_t;
  plat: Pplat_t;
  flash: Plightflash_t;
  strobe: Pstrobe_t;
  glow: Pglow_t;
  scroll: Pscroll_t;
  friction: Pfriction_t;
  pusher: Ppusher_t;
  flicker: Pfireflicker_t;
  elevator: Pelevator_t;
  i: integer;
begin
  // save off the current thinkers
  th1 := thinkercap.next;
  while th1 <> @thinkercap do
  begin
    th := th1;
    th1 := th1.next;
    if not Assigned(th._function.acv) then
    begin
      i := 0;
      while i < MAXCEILINGS do
      begin
        if activeceilings[i] = Pceiling_t(th) then
          break;
        inc(i);
      end;

      if i < MAXCEILINGS then
      begin
        save_p[0] := Ord(tc_ceiling);
        save_p := @save_p[1];
        PADSAVEP;
        ceiling := Pceiling_t(save_p);
        memcpy(ceiling, th, SizeOf(ceiling_t));
        incp(pointer(save_p), SizeOf(ceiling_t));
        ceiling.sector := Psector_t(pDiff(ceiling.sector, sectors, SizeOf(sector_t)));
      end;
      continue;
    end;

    if @th._function.acp1 = @T_MoveCeiling then
    begin
      save_p[0] := Ord(tc_ceiling);
      save_p := @save_p[1];
      PADSAVEP;
      ceiling := Pceiling_t(save_p);
      memcpy(ceiling, th, SizeOf(ceiling_t));
      incp(pointer(save_p), SizeOf(ceiling_t));
      ceiling.sector := Psector_t(pDiff(ceiling.sector, sectors, SizeOf(sector_t)));
      continue;
    end;

    if @th._function.acp1 = @T_VerticalDoor then
    begin
      save_p[0] := Ord(tc_door);
      save_p := @save_p[1];
      PADSAVEP;
      door := Pvldoor_t(save_p);
      memcpy(door, th, SizeOf(vldoor_t));
      incp(pointer(save_p), SizeOf(vldoor_t));
      door.sector := Psector_t(pDiff(door.sector, sectors, SizeOf(sector_t)));
      if door.line = nil then
        door.line := Pline_t(-1)
      else
        door.line := Pline_t(pDiff(door.line, lines, SizeOf(line_t)));
      continue;
    end;

    if @th._function.acp1 = @T_SlidingDoor then
    begin
      save_p[0] := Ord(tc_slidingdoor);
      save_p := @save_p[1];
      PADSAVEP;
      sdoor := Pslidedoor_t(save_p);
      memcpy(sdoor, th, SizeOf(Pslidedoor_t));
      incp(pointer(save_p), SizeOf(Pslidedoor_t));
      sdoor.frontsector := Psector_t(pDiff(sdoor.frontsector, sectors, SizeOf(sector_t)));
      if sdoor.line1 = nil then
        sdoor.line1 := Pline_t(-1)
      else
        sdoor.line1 := Pline_t(pDiff(sdoor.line1, lines, SizeOf(line_t)));
      if sdoor.line2 = nil then
        sdoor.line2 := Pline_t(-1)
      else
        sdoor.line2 := Pline_t(pDiff(sdoor.line2, lines, SizeOf(line_t)));
      continue;
    end;

    if @th._function.acp1 = @T_MoveFloor then
    begin
      save_p[0] := Ord(tc_floor);
      save_p := @save_p[1];
      PADSAVEP;
      floor := Pfloormove_t(save_p);
      memcpy(floor, th, SizeOf(floormove_t));
      incp(pointer(save_p), SizeOf(floormove_t));
      floor.sector := Psector_t(pDiff(floor.sector, sectors, SizeOf(sector_t)));
      continue;
    end;

    if @th._function.acp1 = @T_PlatRaise then
    begin
      save_p[0] := Ord(tc_plat);
      save_p := @save_p[1];
      PADSAVEP;
      plat := Pplat_t(save_p);
      memcpy(plat, th, SizeOf(plat_t));
      incp(pointer(save_p), SizeOf(plat_t));
      plat.sector := Psector_t(pDiff(plat.sector, sectors, SizeOf(sector_t)));
      continue;
    end;

    if @th._function.acp1 = @T_LightFlash then
    begin
      save_p[0] := Ord(tc_flash);
      save_p := @save_p[1];
      PADSAVEP;
      flash := Plightflash_t(save_p);
      memcpy(flash, th, SizeOf(lightflash_t));
      incp(pointer(save_p), SizeOf(lightflash_t));
      flash.sector := Psector_t(pDiff(flash.sector, sectors, SizeOf(sector_t)));
      continue;
    end;

    if @th._function.acp1 = @T_StrobeFlash then
    begin
      save_p[0] := Ord(tc_strobe);
      save_p := @save_p[1];
      PADSAVEP;
      strobe := Pstrobe_t(save_p);
      memcpy(strobe, th, SizeOf(strobe_t));
      incp(pointer(save_p), SizeOf(strobe_t));
      strobe.sector := Psector_t(pDiff(strobe.sector, sectors, SizeOf(sector_t)));
      continue;
    end;

    if @th._function.acp1 = @T_Glow then
    begin
      save_p[0] := Ord(tc_glow);
      save_p := @save_p[1];
      PADSAVEP;
      glow := Pglow_t(save_p);
      memcpy(glow, th, SizeOf(glow_t));
      incp(pointer(save_p), SizeOf(glow_t));
      glow.sector := Psector_t(pDiff(glow.sector, sectors, SizeOf(sector_t)));
      continue;
    end;

    if @th._function.acp1 = @T_Scroll then
    begin
      save_p[0] := Ord(tc_scroll);
      save_p := @save_p[1];
      PADSAVEP;
      scroll := Pscroll_t(save_p);
      memcpy(scroll, th, SizeOf(scroll_t));
      incp(pointer(save_p), SizeOf(scroll_t));
      continue;
    end;

    if @th._function.acp1 = @T_Friction then
    begin
      save_p[0] := Ord(tc_friction);
      save_p := @save_p[1];
      PADSAVEP;
      friction := Pfriction_t(save_p);
      memcpy(friction, th, SizeOf(friction_t));
      incp(pointer(save_p), SizeOf(friction_t));
      continue;
    end;

    if @th._function.acp1 = @T_Pusher then
    begin
      save_p[0] := Ord(tc_pusher);
      save_p := @save_p[1];
      PADSAVEP;
      pusher := Ppusher_t(save_p);
      memcpy(pusher, th, SizeOf(pusher_t));
      incp(pointer(save_p), SizeOf(pusher_t));
      continue;
    end;

    if @th._function.acp1 = @T_FireFlicker then
    begin
      save_p[0] := Ord(tc_fireflicker);
      save_p := @save_p[1];
      PADSAVEP;
      flicker := Pfireflicker_t(save_p);
      memcpy(flicker, th, SizeOf(fireflicker_t));
      flicker.sector := Psector_t(flicker.sector.iSectorID);
      incp(pointer(save_p), SizeOf(fireflicker_t));
      continue;
    end;

    if @th._function.acp1 = @T_MoveElevator then
    begin
      save_p[0] := Ord(tc_elevator);
      save_p := @save_p[1];
      PADSAVEP;
      elevator := Pelevator_t(save_p);
      memcpy(elevator, th, SizeOf(elevator_t));
      incp(pointer(save_p), SizeOf(elevator_t));
      elevator.sector := Psector_t(elevator.sector.iSectorID);
      continue;
    end;

  end;
  // add a terminating marker
  save_p[0] := Ord(tc_endspecials);
  save_p := @save_p[1];
end;

//
// P_UnArchiveSpecials
//
procedure P_UnArchiveSpecials;
var
  tclass: byte;
  ceiling: Pceiling_t;
  door: Pvldoor_t;
  sdoor: Pslidedoor_t;
  floor: Pfloormove_t;
  plat: Pplat_t;
  flash: Plightflash_t;
  strobe: Pstrobe_t;
  glow: Pglow_t;
  scroll: Pscroll_t;
  friction: Pfriction_t;
  pusher: Ppusher_t;
  flicker: Pfireflicker_t;
  elevator: Pelevator_t;
begin
  // read in saved thinkers
  while true do
  begin
    tclass := save_p[0];
    save_p := @save_p[1];
    case tclass of
      Ord(tc_endspecials):
        exit; // end of list

      Ord(tc_ceiling):
        begin
          PADSAVEP;
          ceiling := Z_Malloc(SizeOf(ceiling_t), PU_LEVEL, nil);
          memcpy(ceiling, save_p, SizeOf(ceiling_t));
          incp(pointer(save_p), SizeOf(ceiling_t));
          ceiling.sector := @sectors[integer(ceiling.sector)];
          ceiling.sector.ceilingdata := ceiling;

          if Assigned(ceiling.thinker._function.acp1) then // JVAL works ???
            @ceiling.thinker._function.acp1 := @T_MoveCeiling;

          P_AddThinker(@ceiling.thinker);
          P_AddActiveCeiling(ceiling);
        end;

      Ord(tc_door):
        begin
          PADSAVEP;
          door := Z_Malloc(SizeOf(vldoor_t), PU_LEVEL, nil);
          memcpy(door, save_p, SizeOf(vldoor_t));
          incp(pointer(save_p), SizeOf(vldoor_t));
          door.sector := @sectors[integer(door.sector)];
          door.sector.ceilingdata := door;
          if integer(door.line) = -1 then
            door.line := nil
          else
            door.line := @lines[integer(door.line)];

          @door.thinker._function.acp1 := @T_VerticalDoor;
          P_AddThinker(@door.thinker);
        end;

      Ord(tc_slidingdoor):
        begin
          PADSAVEP;
          sdoor := Z_Malloc(SizeOf(slidedoor_t), PU_LEVEL, nil);
          memcpy(sdoor, save_p, SizeOf(slidedoor_t));
          incp(pointer(save_p), SizeOf(slidedoor_t));
          sdoor.frontsector := @sectors[integer(sdoor.frontsector)];
          sdoor.frontsector.ceilingdata := sdoor;
          if integer(sdoor.line1) = -1 then
            sdoor.line1 := nil
          else
            sdoor.line1 := @lines[integer(sdoor.line1)];
          if integer(sdoor.line2) = -1 then
            sdoor.line2 := nil
          else
            sdoor.line2 := @lines[integer(sdoor.line2)];

          @sdoor.thinker._function.acp1 := @T_VerticalDoor;
          P_AddThinker(@sdoor.thinker);
        end;

      Ord(tc_floor):
        begin
          PADSAVEP;
          floor := Z_Malloc(SizeOf(floormove_t), PU_LEVEL, nil);
          memcpy(floor, save_p, SizeOf(floormove_t));
          incp(pointer(save_p), SizeOf(floormove_t));
          floor.sector := @sectors[integer(floor.sector)];
          floor.sector.floordata := floor;
          @floor.thinker._function.acp1 := @T_MoveFloor;
          P_AddThinker(@floor.thinker);
        end;

      Ord(tc_plat):
        begin
          PADSAVEP;
          plat := Z_Malloc(SizeOf(plat_t), PU_LEVEL, nil);
          memcpy(plat, save_p, SizeOf(plat_t));
          incp(pointer(save_p), SizeOf(plat_t));
          plat.sector := @sectors[integer(plat.sector)];
          plat.sector.floordata := plat;

          if Assigned(plat.thinker._function.acp1) then
            @plat.thinker._function.acp1 := @T_PlatRaise;

          P_AddThinker(@plat.thinker);
          P_AddActivePlat(plat);
        end;

      Ord(tc_flash):
        begin
          PADSAVEP;
          flash := Z_Malloc(Sizeof(lightflash_t), PU_LEVEL, nil);
          memcpy(flash, save_p, SizeOf(lightflash_t));
          incp(pointer(save_p), SizeOf(lightflash_t));
          flash.sector := @sectors[integer(flash.sector)];
          @flash.thinker._function.acp1 := @T_LightFlash;
          P_AddThinker(@flash.thinker);
        end;

      Ord(tc_strobe):
        begin
          PADSAVEP;
          strobe := Z_Malloc(SizeOf(strobe_t), PU_LEVEL, nil);
          memcpy(strobe, save_p, SizeOf(strobe_t));
          incp(pointer(save_p), SizeOf(strobe_t));
          strobe.sector := @sectors[integer(strobe.sector)];
          @strobe.thinker._function.acp1 := @T_StrobeFlash;
          P_AddThinker(@strobe.thinker);
        end;

      Ord(tc_glow):
        begin
          PADSAVEP;
          glow := Z_Malloc(SizeOf(glow_t), PU_LEVEL, nil);
          memcpy(glow, save_p, SizeOf(glow_t));
          incp(pointer(save_p), SizeOf(glow_t));
          glow.sector := @sectors[integer(glow.sector)];
          @glow.thinker._function.acp1 := @T_Glow;
          P_AddThinker(@glow.thinker);
        end;

      Ord(tc_scroll):
        begin
          PADSAVEP;
          scroll := Z_Malloc(SizeOf(scroll_t), PU_LEVEL, nil);
          memcpy(scroll, save_p, SizeOf(scroll_t));
          incp(pointer(save_p), SizeOf(scroll_t));
          @scroll.thinker._function.acp1 := @T_Scroll;
          P_AddThinker(@scroll.thinker);
        end;

      Ord(tc_friction):
        begin
          PADSAVEP;
          friction := Z_Malloc(SizeOf(friction_t), PU_LEVEL, nil);
          memcpy(friction, save_p, SizeOf(friction_t));
          incp(pointer(save_p), SizeOf(friction_t));
          @friction.thinker._function.acp1 := @T_Friction;
          P_AddThinker(@friction.thinker);
        end;

      Ord(tc_pusher):
        begin
          PADSAVEP;
          pusher := Z_Malloc(SizeOf(pusher_t), PU_LEVEL, nil);
          memcpy(pusher, save_p, SizeOf(pusher_t));
          incp(pointer(save_p), SizeOf(pusher_t));
          @pusher.thinker._function.acp1 := @T_Pusher;
          pusher.source := P_GetPushThing(pusher.affectee);
          P_AddThinker(@pusher.thinker);
        end;

      Ord(tc_fireflicker):
        begin
          if savegameversion <= VERSION203 then // JVAL: old version's tc_endspecials value
            exit;

          PADSAVEP;
          flicker := Z_Malloc(SizeOf(fireflicker_t), PU_LEVEL, nil);
          memcpy(flicker, save_p, SizeOf(fireflicker_t));
          incp(pointer(save_p), SizeOf(fireflicker_t));

          @flicker.thinker._function.acp1 := @T_FireFlicker;
          flicker.sector := @sectors[integer(flicker.sector)];
          P_AddThinker(@flicker.thinker);
        end;

      Ord(tc_elevator):
        begin
          if savegameversion <= VERSION205 then // JVAL: tc_elevator = old value of tc_endspecials
            exit;

          if (savegameversion = VERSION206) and (savegameversionhack = 1) then // JVAL: tc_fireflicker = old value of tc_endspecials
            exit;

          PADSAVEP;
          elevator := Z_Malloc(SizeOf(elevator_t), PU_LEVEL, nil);
          memcpy(elevator, save_p, SizeOf(elevator_t));
          incp(pointer(save_p), SizeOf(elevator_t));

          @elevator.thinker._function.acp1 := @T_MoveElevator;
          elevator.sector := @sectors[integer(elevator.sector)];
          elevator.sector.floordata := elevator; //jff 2/22/98
          elevator.sector.ceilingdata := elevator; //jff 2/22/98
          P_AddThinker(@elevator.thinker);
        end;

      else
        I_Error('P_UnarchiveSpecials(): Unknown tclass %d in savegame', [tclass]);
    end;
  end;
end;

procedure P_ArchiveGlobalVariables(const vars: TGlobalVariablesList; var sp: PBytearray);
var
  sz: integer;
begin
  sz := vars.StructureSize;
  PInteger(sp)^ := sz;
  incp(pointer(sp), SizeOf(integer));
  vars.SaveToBuffer(sp);
  incp(pointer(sp), sz);
end;

procedure P_ArchiveMapVariables;
begin
  P_ArchiveGlobalVariables(mapvars, save_p);
end;

procedure P_ArchiveWorldVariables(const fname: string);
var
  len: integer;
  pp: pointer;
  ppt: PByteArray;
begin
  len := worldvars.StructureSize + SizeOf(integer);
  pp := malloc(len);
  ppt := pp;
  P_ArchiveGlobalVariables(worldvars, ppt);
  M_WriteFile(fname, pp, len);
  memfree(pp, len);
end;

procedure P_UnArchiveGlobalVariables(const vars: TGlobalVariablesList; var sp: PBytearray);
var
  sz: integer;
begin
  if savegameversion <= VERSION121 then
    Exit;

  sz := PInteger(sp)^;
  incp(pointer(sp), SizeOf(integer));
  vars.LoadFromBuffer(sp);
  incp(pointer(sp), sz);
end;

procedure P_UnArchiveMapVariables;
begin
  P_UnArchiveGlobalVariables(mapvars, save_p);
end;

procedure P_UnArchiveWorldVariables(const fname: string);
var
  pp: pointer;
  ppt: PByteArray;
begin
  if savegameversion <= VERSION121 then
    Exit;

  if fexists(fname) then
  begin
    M_ReadFile(fname, pp);
    ppt := pp;
    P_UnArchiveGlobalVariables(worldvars, ppt);
    Z_Free(pp);
  end;
end;

procedure P_ArchivePSMapScript;
var
  fname: string;
  sz: Integer;
begin
  fname := I_NewTempFile('mapscript' + itoa(Random(1000)));
  PS_MapScriptSaveToFile(fname);
  sz := fsize(fname);
  PInteger(save_p)^ := sz;
  incp(pointer(save_p), SizeOf(integer));
  with TFile.Create(fname, fOpenReadOnly) do
  try
    Read(save_p^, sz);
  finally
    Free;
  end;
  fdelete(fname);
  incp(Pointer(save_p), sz);
end;

procedure P_UnArchivePSMapScript;
var
  fname: string;
  sz: Integer;
begin
  if savegameversion <= VERSION121 then
    Exit;

  sz := PInteger(save_p)^;
  incp(pointer(save_p), SizeOf(integer));

  fname := I_NewTempFile('mapscript' + itoa(Random(1000)));
  with TFile.Create(fname, fCreate) do
  try
    Write(save_p^, sz);
  finally
    Free;
  end;
  PS_MapScriptLoadFromFile(fname);
  fdelete(fname);
  incp(Pointer(save_p), sz);
end;

procedure P_ArchiveOverlay;
begin
  overlay.SaveToBuffer(Pointer(save_p));
end;

procedure P_UnArchiveOverlay;
begin
  if savegameversion <= VERSION121 then
    Exit;

  overlay.LoadFromBuffer(Pointer(save_p));
end;

procedure P_ArchiveScreenShot(const fname: string);
begin
  M_WriteFile(fname, @mn_screenshotbuffer, SizeOf(menuscreenbuffer_t));
end;

end.
