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

procedure P_UnArchivePlayers;

procedure P_ArchiveWorld;

procedure P_UnArchiveWorld;

procedure P_ArchiveThinkers;

procedure P_UnArchiveThinkers;

procedure P_ArchiveSpecials;

procedure P_UnArchiveSpecials;

procedure P_ArchiveVariables;

procedure P_UnArchiveVariables;

procedure P_ArchivePSMapScript;

procedure P_UnArchivePSMapScript;

procedure P_ArchiveOverlay;

procedure P_UnArchiveOverlay;

procedure P_ArchiveScreenShot;

procedure P_UnArchiveScreenShot;

var
  save_p: PByteArray;
  savegameversion: integer;

implementation

uses
  doomdef,
  d_ticcmd,
  d_player,
  d_think,
  g_game,
  m_fixed,
  mn_screenshot,
  info_h,
  info,
  i_system,
  i_tmp,
  p_3dfloors, // JVAL: 3d floors
  p_local,    // JVAL: sector gravity (VERSION 204)
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
  r_renderstyle,
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
    incp(pointer(save_p), SizeOf(player_t));
    for j := 0 to Ord(NUMPSPRITES) - 1 do
      if dest.psprites[j].state <> nil then
        dest.psprites[j].state := Pstate_t(pDiff(dest.psprites[j].state, @states[0], SizeOf(dest.psprites[j].state^)));
  end;
end;

//
// P_UnArchivePlayers
//
function P_UnArchiveOldPlayer(p: Pplayer_t): boolean;
begin
  if savegameversion <= VERSION114 then
  begin
    memcpy(pointer(p), save_p, SizeOf(player_t114));
    incp(pointer(save_p), SizeOf(player_t114));
    p.attackerx := 0;
    p.attackery := 0;
    p.lastsoundstepx := 0;
    p.lastsoundstepy := 0;
    p.lastbreath := 0;
    p.hardbreathtics := 0;
    p.angletargetx := 0;
    p.angletargety := 0;
    p.angletargetticks := 0;
    p.laddertics := 0;
    p.viewbob := p.bob;
    p.slopetics := 0; // JVAL: Slopes
    p.oldviewz := p.viewz;
    p.teleporttics := 0;
    p.quaketics := 0;
    p.lookdir16 := p.lookdir * 16; // JVAL Smooth Look Up/Down
    Pticcmd_t202(@p.cmd)^ := p.cmd202;
    p.cmd.lookupdown16 := p.cmd.lookupdown * 256;

    // ver 206
    p.quakeintensity := 0;
    p.nextoof := 0;

    // ver 207
    p.oldcrouch := 0;
    p.lastongroundtime := 0;
    p.lastautocrouchtime := 0;
    p.crouchheight := 0;

    result := true;
  end
  else if savegameversion <= VERSION118 then
  begin
    memcpy(pointer(p), save_p, SizeOf(player_t118));
    incp(pointer(save_p), SizeOf(player_t118));
    p.lastsoundstepx := 0;
    p.lastsoundstepy := 0;
    p.lastbreath := 0;
    p.hardbreathtics := 0;
    p.angletargetx := 0;
    p.angletargety := 0;
    p.angletargetticks := 0;
    p.laddertics := 0;
    p.viewbob := p.bob;
    p.slopetics := 0; // JVAL: Slopes
    p.oldviewz := p.viewz;
    p.teleporttics := 0;
    p.quaketics := 0;
    p.lookdir16 := p.lookdir * 16; // JVAL Smooth Look Up/Down
    Pticcmd_t202(@p.cmd)^ := p.cmd202;
    p.cmd.lookupdown16 := p.cmd.lookupdown * 256;

    // ver 206
    p.quakeintensity := 0;
    p.nextoof := 0;  // JVAL: version 206

    // ver 207
    p.oldcrouch := 0;
    p.lastongroundtime := 0;
    p.lastautocrouchtime := 0;
    p.crouchheight := 0;

    result := true;
  end
  else if savegameversion <= VERSION121 then
  begin
    memcpy(pointer(p), save_p, SizeOf(player_t121));
    incp(pointer(save_p), SizeOf(player_t121));
    p.laddertics := 0;
    p.viewbob := p.bob;
    p.slopetics := 0; // JVAL: Slopes
    p.oldviewz := p.viewz;
    p.teleporttics := 0;
    p.quaketics := 0;
    p.lookdir16 := p.lookdir * 16; // JVAL Smooth Look Up/Down
    Pticcmd_t202(@p.cmd)^ := p.cmd202;
    p.cmd.lookupdown16 := p.cmd.lookupdown * 256;

    // ver 206
    p.quakeintensity := 0;
    p.nextoof := 0;  // JVAL: version 206

    // ver 207
    p.oldcrouch := 0;
    p.lastongroundtime := 0;
    p.lastautocrouchtime := 0;
    p.crouchheight := 0;

    result := true;
  end
  else if savegameversion <= VERSION122 then
  begin
    memcpy(pointer(p), save_p, SizeOf(player_t122));
    incp(pointer(save_p), SizeOf(player_t122));
    p.lookdir16 := p.lookdir * 16; // JVAL Smooth Look Up/Down
    Pticcmd_t202(@p.cmd)^ := p.cmd202;
    p.cmd.lookupdown16 := p.cmd.lookupdown * 256;

    // ver 206
    if p.quaketics > 0 then
      p.quakeintensity := FRACUNIT
    else
      p.quakeintensity := 0;
    p.nextoof := 0;  // JVAL: version 206

    // ver 207
    p.oldcrouch := 0;
    p.lastongroundtime := 0;
    p.lastautocrouchtime := 0;
    p.crouchheight := 0;

    result := true;
  end
  else if savegameversion <= VERSION205 then
  begin
    memcpy(pointer(p), save_p, SizeOf(player_t205));
    incp(pointer(save_p), SizeOf(player_t205));

    // ver 206
    if p.quaketics > 0 then
      p.quakeintensity := FRACUNIT
    else
      p.quakeintensity := 0;
    p.nextoof := 0;  // JVAL: version 206

    result := true;
  end
  else if savegameversion <= VERSION206 then
  begin
    memcpy(pointer(p), save_p, SizeOf(player_t206));
    incp(pointer(save_p), SizeOf(player_t206));

    // ver 207
    p.oldcrouch := 0;
    p.lastongroundtime := 0;
    p.lastautocrouchtime := 0;
    p.crouchheight := 0;

    result := true;
  end
  else
  begin
    result := false;
    exit;
  end;
end;

procedure P_UnArchivePlayers;
var
  i: integer;
  j: integer;
begin
  for i := 0 to MAXPLAYERS - 1 do
  begin
    if not playeringame[i] then
      continue;

    PADSAVEP;

    if savegameversion >= VERSION207 then
    begin
      memcpy(@players[i], save_p, SizeOf(player_t));
      incp(pointer(save_p), SizeOf(player_t));
    end
    else if not P_UnArchiveOldPlayer(@players[i]) then
      I_Error('P_UnArchivePlayers(): Unsupported saved game version: %d', [savegameversion]);

    // will be set when unarc thinker
    players[i].mo := nil;
    players[i]._message := '';
    players[i].attacker := nil;

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

  levelinf := P_GetLevelInfo(P_GetMapName(gameepisode, gamemap));
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
    levelinf := P_GetLevelInfo(P_GetMapName(gameepisode, gamemap));
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

    if savegameversion > VERSION115 then
    begin
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
      end
    end
    else
    begin
      sec.floor_xoffs := 0;
      sec.floor_yoffs := 0;
      sec.ceiling_xoffs := 0;
      sec.ceiling_yoffs := 0;
    end;
    // JVAL: 3d Floors
    if savegameversion >= VERSION122 then
    begin
      sec.midsec := PInteger(get)^;
      get := @get[2];
      sec.midline := PInteger(get)^;
      get := @get[2];
    end
    else
    begin
      sec.midsec := -1;
      sec.midline := -1;
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
    if savegameversion > VERSION205 then
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
      Z_Realloc(sec.saffectees, sec.num_saffectees * SizeOf(integer), PU_LEVEL, nil);
      for j := 0 to sec.num_saffectees - 1 do
      begin
        sec.saffectees[j] := PInteger(get)^;
        get := @get[2];
      end;
    end;

    sec.touching_thinglist := nil;
    sec.iSectorID := i; // JVAL: 3d Floors
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
      if mobj.master <> nil then
        mobj.master := Pmobj_t(mobj.master.key);
      if mobj.target <> nil then
        mobj.target := Pmobj_t(mobj.target.key);
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

function P_UnArchiveOldPmobj(const mobj: Pmobj_t): boolean;
var
  mobj113: Pmobj_t113;
  mobj114: Pmobj_t114;
  mobj115: Pmobj_t115;
  mobj117: Pmobj_t117;
  mobj118: Pmobj_t118;
begin
  ZeroMemory(mobj, SizeOf(mobj_t));
  if savegameversion = VERSION113 then
  begin
    mobj113 := Z_Malloc(SizeOf(mobj_t113), PU_STATIC, nil);
    memcpy(mobj113, save_p, SizeOf(mobj_t113));
    incp(pointer(save_p), SizeOf(mobj_t113));
    mobj.thinker := mobj113.thinker;
    mobj.x := mobj113.x;
    mobj.y := mobj113.y;
    mobj.z := mobj113.z;
    mobj.snext := mobj113.snext;
    mobj.sprev := mobj113.sprev;
    mobj.angle := mobj113.angle;
    mobj.viewangle := mobj113.viewangle;
    mobj.sprite := Ord(mobj113.sprite);
    mobj.frame := mobj113.frame;
    mobj.bpos := -1;
    mobj.bidx := -1;
    mobj.subsector := mobj113.subsector;
    mobj.radius := mobj113.radius;
    mobj.height := mobj113.height;
    mobj.momx := mobj113.momx;
    mobj.momy := mobj113.momy;
    mobj.momz := mobj113.momz;
    mobj.validcount := mobj113.validcount;
    mobj._type := Ord(mobj113._type);
    mobj.tics := mobj113.tics;
    mobj.state := mobj113.state;
    mobj.prevstate := mobj.state;
    mobj.flags := mobj113.flags;
    mobj.flags_ex := mobj113.flags_ex;
    mobj.flags2_ex := 0;
    mobj.renderstyle := mrs_normal;
    mobj.alpha := FRACUNIT;
    mobj.bob := 0;
    mobj.health := mobj113.health;
    mobj.movedir := mobj113.movedir;
    mobj.movecount := mobj113.movecount;
    mobj.reactiontime := mobj113.reactiontime;
    mobj.threshold := mobj113.threshold;
    mobj.player := mobj113.player;
    mobj.lastlook := mobj113.lastlook;
    mobj.spawnpoint := mobj113.spawnpoint;
    mobj.fastchasetics := 0;
    mobj.friction := 0;
    mobj.movefactor := 0;
    mobj.customparams := nil;
    mobj.floorclip := 0;

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

    // ver 206
    mobj.target := nil;
    mobj.tracer := nil;
    mobj.master := nil;
    mobj.mass := mobjinfo[Ord(mobj._type)].mass;
    mobj.args[0] := 0;
    mobj.args[1] := 0;
    mobj.args[2] := 0;
    mobj.args[3] := 0;
    mobj.args[4] := 0;
    mobj.special := 0;
    mobj.WeaveIndexXY := 0;
    mobj.WeaveIndexZ := 0;

    // ver 207
    mobj.painchance := mobjinfo[mobj._type].painchance;
    mobj.spriteDX := mobjinfo[Ord(mobj._type)].spriteDX;
    mobj.spriteDY := mobjinfo[Ord(mobj._type)].spriteDY;

    Z_Free(mobj113);
    result := true
  end
  else if savegameversion = VERSION114 then
  begin
    mobj114 := Z_Malloc(SizeOf(mobj_t114), PU_STATIC, nil);
    memcpy(mobj114, save_p, SizeOf(mobj_t114));
    incp(pointer(save_p), SizeOf(mobj_t114));
    mobj.thinker := mobj114.thinker;
    mobj.x := mobj114.x;
    mobj.y := mobj114.y;
    mobj.z := mobj114.z;
    mobj.snext := mobj114.snext;
    mobj.sprev := mobj114.sprev;
    mobj.angle := mobj114.angle;
    mobj.viewangle := mobj114.viewangle;
    mobj.sprite := Ord(mobj114.sprite);
    mobj.frame := mobj114.frame;
    mobj.bpos := -1;
    mobj.bidx := -1;
    mobj.subsector := mobj114.subsector;
    mobj.radius := mobj114.radius;
    mobj.height := mobj114.height;
    mobj.momx := mobj114.momx;
    mobj.momy := mobj114.momy;
    mobj.momz := mobj114.momz;
    mobj.validcount := mobj114.validcount;
    mobj._type := Ord(mobj114._type);
    mobj.tics := mobj114.tics;
    mobj.state := mobj114.state;
    mobj.prevstate := mobj.state;
    mobj.flags := mobj114.flags;
    mobj.flags_ex := mobj114.flags_ex;
    mobj.flags2_ex := mobj114.flags2_ex;
    mobj.renderstyle := mrs_normal;
    mobj.alpha := FRACUNIT;
    mobj.bob := mobj114.bob;
    mobj.health := mobj114.health;
    mobj.movedir := mobj114.movedir;
    mobj.movecount := mobj114.movecount;
    mobj.reactiontime := mobj114.reactiontime;
    mobj.threshold := mobj114.threshold;
    mobj.player := mobj114.player;
    mobj.lastlook := mobj114.lastlook;
    mobj.spawnpoint := mobj114.spawnpoint;
    mobj.fastchasetics := mobj114.fastchasetics;
    mobj.friction := 0;
    mobj.movefactor := 0;
    mobj.customparams := nil;
    mobj.floorclip := 0;

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

    // ver 206
    mobj.target := nil;
    mobj.tracer := nil;
    mobj.master := nil;
    mobj.mass := mobjinfo[Ord(mobj._type)].mass;
    mobj.args[0] := 0;
    mobj.args[1] := 0;
    mobj.args[2] := 0;
    mobj.args[3] := 0;
    mobj.args[4] := 0;
    mobj.special := 0;
    mobj.WeaveIndexXY := 0;
    mobj.WeaveIndexZ := 0;

    // ver 207
    mobj.painchance := mobjinfo[mobj._type].painchance;
    mobj.spriteDX := mobjinfo[Ord(mobj._type)].spriteDX;
    mobj.spriteDY := mobjinfo[Ord(mobj._type)].spriteDY;

    Z_Free(mobj114);
    result := true
  end
  else if (savegameversion = VERSION115) or (savegameversion = VERSION116) then
  begin
    mobj115 := Z_Malloc(SizeOf(mobj_t115), PU_STATIC, nil);
    memcpy(mobj115, save_p, SizeOf(mobj_t115));
    incp(pointer(save_p), SizeOf(mobj_t115));
    mobj.thinker := mobj115.thinker;
    mobj.x := mobj115.x;
    mobj.y := mobj115.y;
    mobj.z := mobj115.z;
    mobj.snext := mobj115.snext;
    mobj.sprev := mobj115.sprev;
    mobj.angle := mobj115.angle;
    mobj.viewangle := mobj115.viewangle;
    mobj.sprite := mobj115.sprite;
    mobj.frame := mobj115.frame;
    mobj.bpos := -1;
    mobj.bidx := -1;
    mobj.subsector := mobj115.subsector;
    mobj.radius := mobj115.radius;
    mobj.height := mobj115.height;
    mobj.momx := mobj115.momx;
    mobj.momy := mobj115.momy;
    mobj.momz := mobj115.momz;
    mobj.validcount := mobj115.validcount;
    mobj._type := mobj115._type;
    mobj.tics := mobj115.tics;
    mobj.state := mobj115.state;
    mobj.prevstate := mobj.state;
    mobj.flags := mobj115.flags;
    mobj.flags_ex := mobj115.flags_ex;
    mobj.flags2_ex := mobj115.flags2_ex;
    mobj.renderstyle := mobj115.renderstyle;
    mobj.alpha := mobj115.alpha;
    mobj.bob := mobj115.bob;
    mobj.health := mobj115.health;
    mobj.movedir := mobj115.movedir;
    mobj.movecount := mobj115.movecount;
    mobj.reactiontime := mobj115.reactiontime;
    mobj.threshold := mobj115.threshold;
    mobj.player := mobj115.player;
    mobj.lastlook := mobj115.lastlook;
    mobj.spawnpoint := mobj115.spawnpoint;
    mobj.fastchasetics := mobj115.fastchasetics;
    mobj.friction := 0;
    mobj.movefactor := 0;
    mobj.customparams := nil;
    mobj.floorclip := 0;

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

    // ver 206
    mobj.target := nil;
    mobj.tracer := nil;
    mobj.master := nil;
    mobj.mass := mobjinfo[Ord(mobj._type)].mass;
    mobj.args[0] := 0;
    mobj.args[1] := 0;
    mobj.args[2] := 0;
    mobj.args[3] := 0;
    mobj.args[4] := 0;
    mobj.special := 0;
    mobj.WeaveIndexXY := 0;
    mobj.WeaveIndexZ := 0;

    // ver 207
    mobj.painchance := mobjinfo[mobj._type].painchance;
    mobj.spriteDX := mobjinfo[Ord(mobj._type)].spriteDX;
    mobj.spriteDY := mobjinfo[Ord(mobj._type)].spriteDY;

    Z_Free(mobj115);
    result := true
  end
  else if savegameversion = VERSION117 then
  begin
    mobj117 := Z_Malloc(SizeOf(mobj_t117), PU_STATIC, nil);
    memcpy(mobj117, save_p, SizeOf(mobj_t117));
    incp(pointer(save_p), SizeOf(mobj_t117));
    mobj.thinker := mobj117.thinker;
    mobj.x := mobj117.x;
    mobj.y := mobj117.y;
    mobj.z := mobj117.z;
    mobj.snext := mobj117.snext;
    mobj.sprev := mobj117.sprev;
    mobj.angle := mobj117.angle;
    mobj.viewangle := mobj117.viewangle;
    mobj.sprite := mobj117.sprite;
    mobj.frame := mobj117.frame;
    mobj.bpos := mobj117.bpos;
    mobj.bidx := mobj117.bpos;
    mobj.subsector := mobj117.subsector;
    mobj.radius := mobj117.radius;
    mobj.height := mobj117.height;
    mobj.momx := mobj117.momx;
    mobj.momy := mobj117.momy;
    mobj.momz := mobj117.momz;
    mobj.validcount := mobj117.validcount;
    mobj._type := mobj117._type;
    mobj.tics := mobj117.tics;
    mobj.state := mobj117.state;
    mobj.prevstate := mobj.state;
    mobj.flags := mobj117.flags;
    mobj.flags_ex := mobj117.flags_ex;
    mobj.flags2_ex := mobj117.flags2_ex;
    mobj.renderstyle := mobj117.renderstyle;
    mobj.alpha := mobj117.alpha;
    mobj.bob := mobj117.bob;
    mobj.health := mobj117.health;
    mobj.movedir := mobj117.movedir;
    mobj.movecount := mobj117.movecount;
    mobj.reactiontime := mobj117.reactiontime;
    mobj.threshold := mobj117.threshold;
    mobj.player := mobj117.player;
    mobj.lastlook := mobj117.lastlook;
    mobj.spawnpoint := mobj117.spawnpoint;
    mobj.fastchasetics := mobj117.fastchasetics;
    mobj.friction := mobj117.friction;
    mobj.movefactor := mobj117.movefactor;
    mobj.customparams := nil;
    mobj.floorclip := 0;

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

    // ver 206
    mobj.target := nil;
    mobj.tracer := nil;
    mobj.master := nil;
    mobj.mass := mobjinfo[Ord(mobj._type)].mass;
    mobj.args[0] := 0;
    mobj.args[1] := 0;
    mobj.args[2] := 0;
    mobj.args[3] := 0;
    mobj.args[4] := 0;
    mobj.special := 0;
    mobj.WeaveIndexXY := 0;
    mobj.WeaveIndexZ := 0;

    // ver 207
    mobj.painchance := mobjinfo[mobj._type].painchance;
    mobj.spriteDX := mobjinfo[Ord(mobj._type)].spriteDX;
    mobj.spriteDY := mobjinfo[Ord(mobj._type)].spriteDY;

    Z_Free(mobj117);
    result := true
  end
  else if savegameversion = VERSION118 then
  begin
    mobj118 := Z_Malloc(SizeOf(mobj_t118), PU_STATIC, nil);
    memcpy(mobj118, save_p, SizeOf(mobj_t118));
    incp(pointer(save_p), SizeOf(mobj_t118));
    mobj.thinker := mobj118.thinker;
    mobj.x := mobj118.x;
    mobj.y := mobj118.y;
    mobj.z := mobj118.z;
    mobj.snext := mobj118.snext;
    mobj.sprev := mobj118.sprev;
    mobj.angle := mobj118.angle;
    mobj.viewangle := mobj118.viewangle;
    mobj.sprite := mobj118.sprite;
    mobj.frame := mobj118.frame;
    mobj.bpos := mobj118.bpos;
    mobj.bidx := mobj118.bpos;
    mobj.subsector := mobj118.subsector;
    mobj.radius := mobj118.radius;
    mobj.height := mobj118.height;
    mobj.momx := mobj118.momx;
    mobj.momy := mobj118.momy;
    mobj.momz := mobj118.momz;
    mobj.validcount := mobj118.validcount;
    mobj._type := mobj118._type;
    mobj.tics := mobj118.tics;
    mobj.state := mobj118.state;
    mobj.prevstate := mobj118.prevstate;
    mobj.flags := mobj118.flags;
    mobj.flags_ex := mobj118.flags_ex;
    mobj.flags2_ex := mobj118.flags2_ex;
    mobj.renderstyle := mobj118.renderstyle;
    mobj.alpha := mobj118.alpha;
    mobj.bob := mobj118.bob;
    mobj.health := mobj118.health;
    mobj.movedir := mobj118.movedir;
    mobj.movecount := mobj118.movecount;
    mobj.reactiontime := mobj118.reactiontime;
    mobj.threshold := mobj118.threshold;
    mobj.player := mobj118.player;
    mobj.lastlook := mobj118.lastlook;
    mobj.spawnpoint := mobj118.spawnpoint;
    mobj.fastchasetics := mobj118.fastchasetics;
    mobj.friction := mobj118.friction;
    mobj.movefactor := mobj118.movefactor;
    mobj.customparams := nil;
    mobj.floorclip := 0;

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

    // ver 206
    mobj.target := nil;
    mobj.tracer := nil;
    mobj.master := nil;
    mobj.mass := mobjinfo[Ord(mobj._type)].mass;
    mobj.args[0] := 0;
    mobj.args[1] := 0;
    mobj.args[2] := 0;
    mobj.args[3] := 0;
    mobj.args[4] := 0;
    mobj.special := 0;
    mobj.WeaveIndexXY := 0;
    mobj.WeaveIndexZ := 0;

    // ver 207
    mobj.painchance := mobjinfo[mobj._type].painchance;
    mobj.spriteDX := mobjinfo[Ord(mobj._type)].spriteDX;
    mobj.spriteDY := mobjinfo[Ord(mobj._type)].spriteDY;

    Z_Free(mobj118);
    result := true;
  end
  else if (savegameversion = VERSION119) or (savegameversion = VERSION120) then
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

    // ver 206
    mobj.target := nil;
    mobj.tracer := nil;
    mobj.master := nil;
    mobj.mass := mobjinfo[Ord(mobj._type)].mass;
    mobj.args[0] := 0;
    mobj.args[1] := 0;
    mobj.args[2] := 0;
    mobj.args[3] := 0;
    mobj.args[4] := 0;
    mobj.special := 0;
    mobj.WeaveIndexXY := 0;
    mobj.WeaveIndexZ := 0;

    // ver 207
    mobj.painchance := mobjinfo[mobj._type].painchance;
    mobj.spriteDX := mobjinfo[Ord(mobj._type)].spriteDX;
    mobj.spriteDY := mobjinfo[Ord(mobj._type)].spriteDY;

    result := true;
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

    // ver 206
    mobj.target := nil;
    mobj.tracer := nil;
    mobj.master := nil;
    mobj.mass := mobjinfo[Ord(mobj._type)].mass;
    mobj.args[0] := 0;
    mobj.args[1] := 0;
    mobj.args[2] := 0;
    mobj.args[3] := 0;
    mobj.args[4] := 0;
    mobj.special := 0;
    mobj.WeaveIndexXY := 0;
    mobj.WeaveIndexZ := 0;

    // ver 207
    mobj.painchance := mobjinfo[mobj._type].painchance;
    mobj.spriteDX := mobjinfo[Ord(mobj._type)].spriteDX;
    mobj.spriteDY := mobjinfo[Ord(mobj._type)].spriteDY;

    result := true;
  end
  else if (savegameversion = VERSION122) or (savegameversion = VERSION203) or (savegameversion = VERSION204) then
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

    // ver 206
    mobj.target := nil;
    mobj.tracer := nil;
    mobj.master := nil;
    mobj.mass := mobjinfo[Ord(mobj._type)].mass;
    mobj.args[0] := 0;
    mobj.args[1] := 0;
    mobj.args[2] := 0;
    mobj.args[3] := 0;
    mobj.args[4] := 0;
    mobj.special := 0;
    mobj.WeaveIndexXY := 0;
    mobj.WeaveIndexZ := 0;

    // ver 207
    mobj.painchance := mobjinfo[mobj._type].painchance;
    mobj.spriteDX := mobjinfo[Ord(mobj._type)].spriteDX;
    mobj.spriteDY := mobjinfo[Ord(mobj._type)].spriteDY;

    result := true;
  end
  else if savegameversion = VERSION205 then
  begin
    memcpy(mobj, save_p, SizeOf(mobj_t205));
    incp(pointer(save_p), SizeOf(mobj_t205));

    // ver 206
    mobj.target := nil;
    mobj.tracer := nil;
    mobj.master := nil;
    mobj.mass := mobjinfo[Ord(mobj._type)].mass;
    mobj.args[0] := 0;
    mobj.args[1] := 0;
    mobj.args[2] := 0;
    mobj.args[3] := 0;
    mobj.args[4] := 0;
    mobj.special := 0;
    mobj.WeaveIndexXY := 0;
    mobj.WeaveIndexZ := 0;

    // ver 207
    mobj.painchance := mobjinfo[mobj._type].painchance;
    mobj.spriteDX := mobjinfo[Ord(mobj._type)].spriteDX;
    mobj.spriteDY := mobjinfo[Ord(mobj._type)].spriteDY;

    result := true;
  end
  else if savegameversion = VERSION206 then
  begin
    memcpy(mobj, save_p, SizeOf(mobj_t206));
    incp(pointer(save_p), SizeOf(mobj_t206));

    // ver 207
    mobj.painchance := mobjinfo[mobj._type].painchance;
    mobj.spriteDX := mobjinfo[Ord(mobj._type)].spriteDX;
    mobj.spriteDY := mobjinfo[Ord(mobj._type)].spriteDY;

    result := true;
  end
  else
    result := false;
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
          // Retrieve target, tracer and master
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
          else if not P_UnArchiveOldPmobj(mobj) then
            I_Error('P_UnArchiveThinkers(): Unsupported saved game version: %d', [savegameversion]);

          mobj.validcount := 0;
          mobj.lightvalidcount := 0;
          mobj.rendervalidcount := 0;

          if mobj.key < 1 then
            mobj.key := P_GenGlobalMobjKey;
          P_NotifyMobjKey(mobj);

          mobj.state := @states[integer(mobj.state)];
          mobj.prevstate := @states[integer(mobj.prevstate)];
          mobj.info := @mobjinfo[Ord(mobj._type)];
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
          mobj.floorz := P_3dFloorHeight(mobj); // JVAL: 3d floors
          mobj.ceilingz := P_3dCeilingHeight(mobj); // JVAL: 3d floors
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
    tc_floor,
    tc_plat,
    tc_flash,
    tc_strobe,
    tc_glow,
    tc_scroll,
    tc_friction,    // phares 3/18/98:  new friction effect thinker
    tc_pusher,      // phares 3/22/98:  new push/pull effect thinker
    tc_fireflicker, // JVAL correct T_FireFlicker savegame bug
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
  ceiling115: Pceiling_t115;
  door: Pvldoor_t;
  door115: Pvldoor_t115;
  floor: Pfloormove_t;
  floor115: Pfloormove_t115;
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
          if savegameversion <= VERSION115 then
          begin
            ceiling115 := malloc(SizeOf(ceiling_t115));
            memcpy(ceiling115, save_p, SizeOf(ceiling_t115));
            incp(pointer(save_p), SizeOf(ceiling_t115));
            ceiling.thinker := ceiling115.thinker;
            ceiling._type := ceiling115._type;
            ceiling.sector := ceiling115.sector;
            ceiling.bottomheight := ceiling115.bottomheight;
            ceiling.topheight := ceiling115.topheight;
            ceiling.speed := ceiling115.speed;
            ceiling.oldspeed := ceiling115.speed;
            ceiling.crush := ceiling115.crush;
            ceiling.direction := ceiling115.direction;
            ceiling.newspecial := 0;
            ceiling.oldspecial := 0;
            ceiling.texture := 0;
            ceiling.tag := ceiling115.tag;
            ceiling.olddirection := ceiling115.olddirection;
            memfree(pointer(ceiling115), SizeOf(ceiling_t115));
          end
          else
          begin
            memcpy(ceiling, save_p, SizeOf(ceiling_t));
            incp(pointer(save_p), SizeOf(ceiling_t));
          end;
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
          if savegameversion <= VERSION115 then
          begin
            door115 := malloc(SizeOf(vldoor_t115));
            memcpy(door115, save_p, SizeOf(vldoor_t115));
            incp(pointer(save_p), SizeOf(vldoor_t115));
            door.thinker := door115.thinker;
            door._type := door115._type;
            door.sector := door115.sector;
            door.line := Pline_t(-1);
            door.topheight := door115.topheight;
            door.speed := door115.speed;
            door.direction := door115.direction;
            door.topwait := door115.topwait;
            door.topcountdown := door115.topcountdown;
            memfree(pointer(door115), SizeOf(vldoor_t115));
          end
          else
          begin
            memcpy(door, save_p, SizeOf(vldoor_t));
            incp(pointer(save_p), SizeOf(vldoor_t));
          end;
          door.sector := @sectors[integer(door.sector)];
          door.sector.ceilingdata := door;
          if integer(door.line) = -1 then
            door.line := nil
          else
            door.line := @lines[integer(door.line)];

          @door.thinker._function.acp1 := @T_VerticalDoor;
          P_AddThinker(@door.thinker);
        end;

      Ord(tc_floor):
        begin
          PADSAVEP;
          floor := Z_Malloc(SizeOf(floormove_t), PU_LEVEL, nil);
          if savegameversion <= VERSION115 then
          begin
            floor115 := malloc(SizeOf(floormove_t115));
            memcpy(floor115, save_p, SizeOf(floormove_t115));
            incp(pointer(save_p), SizeOf(floormove_t115));
            floor.thinker := floor115.thinker;
            floor._type := floor115._type;
            floor.crush := floor115.crush;
            floor.sector := floor115.sector;
            floor.direction := floor115.direction;
            floor.newspecial := floor115.newspecial;
            floor.oldspecial := floor115.newspecial;
            floor.texture := floor115.texture;
            floor.floordestheight := floor115.floordestheight;
            floor.speed := floor115.speed;
            memfree(pointer(floor115), SizeOf(floormove_t115));
          end
          else
          begin
            memcpy(floor, save_p, SizeOf(floormove_t));
            incp(pointer(save_p), SizeOf(floormove_t));
          end;
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
          if savegameversion <= VERSION115 then // JVAL: tc_scroll = old value of tc_endspecials
            exit;

          PADSAVEP;
          scroll := Z_Malloc(SizeOf(scroll_t), PU_LEVEL, nil);
          memcpy(scroll, save_p, SizeOf(scroll_t));
          incp(pointer(save_p), SizeOf(scroll_t));
          @scroll.thinker._function.acp1 := @T_Scroll;
          P_AddThinker(@scroll.thinker);
        end;

      Ord(tc_friction):
        begin
          if savegameversion <= VERSION116 then // JVAL: tc_friction = old value of tc_endspecials
            exit;

          PADSAVEP;
          friction := Z_Malloc(SizeOf(friction_t), PU_LEVEL, nil);
          memcpy(friction, save_p, SizeOf(friction_t));
          incp(pointer(save_p), SizeOf(friction_t));
          @friction.thinker._function.acp1 := @T_Friction;
          P_AddThinker(@friction.thinker);
        end;

      Ord(tc_pusher):
        begin
          if savegameversion <= VERSION116 then // JVAL: tc_pusher = old value of tc_endspecials
            I_Error('P_UnarchiveSpecials(): Unknown tclass %d in savegame', [tclass]);

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
          if savegameversion <= VERSION203 then // JVAL: tc_fireflicker = old value of tc_endspecials
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
          if savegameversion <= VERSION205 then // JVAL: tc_fireflicker = old value of tc_endspecials
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

procedure P_ArchiveGlobalVariables(const vars: TGlobalVariablesList);
var
  sz: integer;
begin
  sz := vars.StructureSize;
  PInteger(save_p)^ := sz;
  incp(pointer(save_p), SizeOf(integer));
  vars.SaveToBuffer(save_p);
  incp(pointer(save_p), sz);
end;

procedure P_ArchiveVariables;
begin
  P_ArchiveGlobalVariables(mapvars);
  P_ArchiveGlobalVariables(worldvars);
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

procedure P_UnArchiveGlobalVariables(const vars: TGlobalVariablesList);
var
  sz: integer;
begin
  if savegameversion <= VERSION121 then
    Exit;

  sz := PInteger(save_p)^;
  incp(pointer(save_p), SizeOf(integer));
  vars.LoadFromBuffer(save_p);
  incp(pointer(save_p), sz);
end;

procedure P_UnArchiveVariables;
begin
  P_UnArchiveGlobalVariables(mapvars);
  P_UnArchiveGlobalVariables(worldvars);
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
  if savegameversion <= VERSION121 then
    Exit;

  overlay.SaveToBuffer(Pointer(save_p));
end;

procedure P_UnArchiveOverlay;
begin
  if savegameversion <= VERSION121 then
    Exit;

  overlay.LoadFromBuffer(Pointer(save_p));
end;

procedure P_ArchiveScreenShot;
var
  i: integer;
begin
  for i := 0 to MNSCREENSHOT_MAGIC_SIZE - 1 do
    save_p[i] := mn_screenshotbuffer.header[i];
  for i := 0 to MN_SCREENSHOTSIZE - 1 do
    save_p[MNSCREENSHOT_MAGIC_SIZE + i] := mn_screenshotbuffer.data[i];

  incp(pointer(save_p), SizeOf(menuscreenbuffer_t));
end;

procedure P_UnArchiveScreenShot;
begin
  // Nothing to do, just inc the buffer
  if savegameversion >= VERSION206 then
    incp(pointer(save_p), SizeOf(menuscreenbuffer_t));
end;
end.
