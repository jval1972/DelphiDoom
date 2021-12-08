//------------------------------------------------------------------------------
//
//  DelphiHeretic: A modified and improved Heretic port for Windows
//  based on original Linux Doom as published by "id Software", on
//  Heretic source as published by "Raven" software and DelphiDoom
//  as published by Jim Valavanis.
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
//  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
//  02111-1307, USA.
//
// DESCRIPTION:
//  Savegame I/O, archiving, persistence.
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
  doomstat,
  d_player,
  d_think,
  g_game,
  m_fixed,
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
  p_params,
  p_levelinfo,
  ps_main,
  psi_globals,
  psi_overlay,
  r_defs,
  r_data,
  w_wad,
  z_zone;

// Pads save_p to a 4-byte boundary
//  so that the load/save works on SGI&Gecko.

procedure PADSAVEP;
begin
  if savegameversion < VERSION115 then
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
  end;
end;

//
// P_UnArchivePlayers
//
function P_UnArchiveOldPlayer(p: Pplayer_t): boolean;
begin
  if savegameversion <= VERSION110 then
  begin
    memcpy(pointer(p), save_p, SizeOf(player_t110));
    incp(pointer(save_p), SizeOf(player_t110));

    p.attackerx := 0;
    p.attackery := 0;
    p.laddertics := 0;
    p.viewbob := p.bob;
    p.slopetics := 0; // JVAL: Slopes
    p.oldviewz := p.viewz;
    p.teleporttics := 0;
    p.quaketics := 0;
    p.lookdir16 := p.lookdir * 16; // JVAL Smooth Look Up/Down
    Pticcmd_t202(@p.cmd)^ := p.cmd202;
    p.cmd.lookupdown16 := (p.cmd.lookfly and 15) * 256;

    p.quakeintensity := 0;
    p.nextoof := 0;

    // version 207
    p.oldcrouch := 0;
    p.lastongroundtime := 0;
    p.lastautocrouchtime := 0;
    p.crouchheight := 0;

    result := true;
  end
  else if savegameversion <= VERSION114 then
  begin
    memcpy(pointer(p), save_p, SizeOf(player_t114));
    incp(pointer(save_p), SizeOf(player_t114));

    p.laddertics := 0;
    p.viewbob := p.bob;
    p.slopetics := 0; // JVAL: Slopes
    p.oldviewz := p.viewz;
    p.teleporttics := 0;
    p.quaketics := 0;
    p.lookdir16 := p.lookdir * 16; // JVAL Smooth Look Up/Down
    Pticcmd_t202(@p.cmd)^ := p.cmd202;
    p.cmd.lookupdown16 := (p.cmd.lookfly and 15) * 256;

    p.quakeintensity := 0;
    p.nextoof := 0;

    // version 207
    p.oldcrouch := 0;
    p.lastongroundtime := 0;
    p.lastautocrouchtime := 0;
    p.crouchheight := 0;

    result := true;
  end
  else if savegameversion <= VERSION115 then
  begin
    memcpy(pointer(p), save_p, SizeOf(player_t115));
    incp(pointer(save_p), SizeOf(player_t115));

    p.lookdir16 := p.lookdir * 16; // JVAL Smooth Look Up/Down
    Pticcmd_t202(@p.cmd)^ := p.cmd202;
    p.cmd.lookupdown16 := (p.cmd.lookfly and 15) * 256;

    if p.quaketics > 0 then
      p.quakeintensity := FRACUNIT
    else
      p.quakeintensity := 0;
    p.nextoof := 0;

    // version 207
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

    if p.quaketics > 0 then
      p.quakeintensity := FRACUNIT
    else
      p.quakeintensity := 0;
    p.nextoof := 0;

    // version 207
    p.oldcrouch := 0;
    p.lastongroundtime := 0;
    p.lastautocrouchtime := 0;
    p.crouchheight := 0;

    result := true;
  end
  else if savegameversion <= VERSION206 then
  begin
    memcpy(pointer(p), save_p, SizeOf(player_t206));
    incp(pointer(save_p), SizeOf(player_t206));

    // version 207
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
    if savegameversion <= VERSION114 then
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
    if savegameversion >= VERSION115 then
    begin
      sec.renderflags := PLongWord(get)^;
      get := @get[2];
      sec.flags := PLongWord(get)^;
      get := @get[2];
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

    if savegameversion >= VERSION115 then
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

    sec.specialdata := nil;
    sec.soundtarget := nil;
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
    if savegameversion >= VERSION115 then
    begin
      li.renderflags := PLongWord(get)^;
      get := @get[2];
    end;
    for j := 0 to 1 do
    begin
      if li.sidenum[j] = -1 then
        continue;
      si := @sides[li.sidenum[j]];

      if savegameversion <= VERSION114 then
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
        get := @get[SizeOf(char8_t) div SizeOf(SmallInt)];

        si.bottomtexture := R_SafeTextureNumForName(Pchar8_t(get)^);
        get := @get[SizeOf(char8_t) div SizeOf(SmallInt)];

        si.midtexture := R_SafeTextureNumForName(Pchar8_t(get)^);
        get := @get[SizeOf(char8_t) div SizeOf(SmallInt)];
      end;
    end;
    Inc(i);
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
  mobj111: Pmobj_t111;
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
          else if savegameversion >= VERSION206 then
          begin
            memcpy(mobj, save_p, SizeOf(mobj_t206));
            incp(pointer(save_p), SizeOf(mobj_t206));

            // ver 207
            mobj.painchance := mobjinfo[mobj._type].painchance;
            mobj.spriteDX := mobjinfo[Ord(mobj._type)].spriteDX;
            mobj.spriteDY := mobjinfo[Ord(mobj._type)].spriteDY;
          end
          else if savegameversion >= VERSION205 then
          begin
            memcpy(mobj, save_p, SizeOf(mobj_t205));
            incp(pointer(save_p), SizeOf(mobj_t205));

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
            mobj.friction := ORIG_FRICTION;

            // ver 207
            mobj.painchance := mobjinfo[mobj._type].painchance;
            mobj.spriteDX := mobjinfo[Ord(mobj._type)].spriteDX;
            mobj.spriteDY := mobjinfo[Ord(mobj._type)].spriteDY;
          end
          else if savegameversion >= VERSION115 then
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
            mobj.friction := ORIG_FRICTION;

            // ver 207
            mobj.painchance := mobjinfo[mobj._type].painchance;
            mobj.spriteDX := mobjinfo[Ord(mobj._type)].spriteDX;
            mobj.spriteDY := mobjinfo[Ord(mobj._type)].spriteDY;
          end
          else if savegameversion = VERSION114 then
          begin
            memcpy(mobj, save_p, SizeOf(mobj_t114));
            incp(pointer(save_p), SizeOf(mobj_t114));

            mobj.dropitem := 0;

            mobj.lightvalidcount := 0;
            mobj.scale := FRACUNIT;
            mobj.pushfactor := FRACUNIT div 4;
            mobj.gravity := FRACUNIT;
            mobj.flags3_ex := 0;
            mobj.flags4_ex := 0;
            mobj.rendervalidcount := 0;

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
            mobj.friction := ORIG_FRICTION;

            // ver 207
            mobj.painchance := mobjinfo[mobj._type].painchance;
            mobj.spriteDX := mobjinfo[Ord(mobj._type)].spriteDX;
            mobj.spriteDY := mobjinfo[Ord(mobj._type)].spriteDY;
          end
          else if (savegameversion = VERSION112) or (savegameversion = VERSION113) then
          begin
            memcpy(mobj, save_p, SizeOf(mobj_t113));
            incp(pointer(save_p), SizeOf(mobj_t113));

            mobj.prevx := mobj.x;
            mobj.prevy := mobj.y;
            mobj.prevz := mobj.z;
            mobj.nextx := mobj.x;
            mobj.nexty := mobj.y;
            mobj.nextz := mobj.z;
            mobj.prevangle := mobj.angle;
            mobj.nextangle := mobj.angle;
            mobj.intrplcnt := 0;
            mobj.key := 0; // JVAL: Will be set after
            mobj.customparams := nil;

            mobj.dropitem := 0;

            mobj.lightvalidcount := 0;
            mobj.scale := FRACUNIT;
            mobj.pushfactor := FRACUNIT div 4;
            mobj.gravity := FRACUNIT;
            mobj.flags3_ex := 0;
            mobj.flags4_ex := 0;
            mobj.rendervalidcount := 0;

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
            mobj.friction := ORIG_FRICTION;

            // ver 207
            mobj.painchance := mobjinfo[mobj._type].painchance;
            mobj.spriteDX := mobjinfo[Ord(mobj._type)].spriteDX;
            mobj.spriteDY := mobjinfo[Ord(mobj._type)].spriteDY;
          end
          else if (savegameversion = VERSION110) or (savegameversion = VERSION111) then
          begin
            mobj111 := Z_Malloc(SizeOf(mobj_t), PU_STATIC, nil);
            memcpy(mobj111, save_p, SizeOf(mobj_t111));
            incp(pointer(save_p), SizeOf(mobj_t111));
            mobj.thinker := mobj111.thinker;
            mobj.x := mobj111.x;
            mobj.y := mobj111.y;
            mobj.z := mobj111.z;
            mobj.snext := mobj111.snext;
            mobj.sprev := mobj111.sprev;
            mobj.angle := mobj111.angle;
            mobj.viewangle := mobj111.viewangle;
            mobj.sprite := mobj111.sprite;
            mobj.frame := mobj111.frame;
            mobj.bpos := mobj111.bpos;
            mobj.bidx := mobj111.bidx;
            mobj.subsector := mobj111.subsector;
            mobj.floorz := mobj111.floorz;
            mobj.ceilingz := mobj111.ceilingz;
            mobj.radius := mobj111.radius;
            mobj.height := mobj111.height;
            mobj.momx := mobj111.momx;
            mobj.momy := mobj111.momy;
            mobj.momz := mobj111.momz;
            mobj.validcount := mobj111.validcount;
            mobj._type := mobj111._type;
            mobj.info := mobj111.info;
            mobj.tics := mobj111.tics;
            mobj.state := mobj111.state;
            mobj.prevstate := mobj.state;
            mobj.flags := mobj111.flags;
            mobj.flags2 := mobj111.flags2;
            mobj.flags_ex := mobj111.flags_ex;
            mobj.flags2_ex := mobj111.flags2_ex;
            mobj.damage := mobj111.damage;
            mobj.special1 := mobj111.special1;
            mobj.special2 := mobj111.special2;
            mobj.renderstyle := mobj111.renderstyle;
            mobj.alpha := mobj111.alpha;
            mobj.bob := mobj111.bob;
            mobj.health := mobj111.health;
            mobj.movedir := mobj111.movedir;
            mobj.movecount := mobj111.movecount;
            mobj.target := mobj111.target;
            mobj.reactiontime := mobj111.reactiontime;
            mobj.threshold := mobj111.threshold;
            mobj.player := mobj111.player;
            mobj.lastlook := mobj111.lastlook;
            mobj.spawnpoint := mobj111.spawnpoint;
            mobj.tracer := mobj111.tracer;
            mobj.fastchasetics := mobj111.fastchasetics;

            mobj.prevx := mobj.x;
            mobj.prevy := mobj.y;
            mobj.prevz := mobj.z;
            mobj.nextx := mobj.x;
            mobj.nexty := mobj.y;
            mobj.nextz := mobj.z;
            mobj.prevangle := mobj.angle;
            mobj.nextangle := mobj.angle;
            mobj.intrplcnt := 0;
            mobj.key := 0; // JVAL: Will be set after
            mobj.customparams := nil;

            mobj.dropitem := 0;

            mobj.lightvalidcount := 0;
            mobj.scale := FRACUNIT;
            mobj.pushfactor := FRACUNIT div 4;
            mobj.gravity := FRACUNIT;
            mobj.flags3_ex := 0;
            mobj.flags4_ex := 0;
            mobj.rendervalidcount := 0;

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
            mobj.friction := ORIG_FRICTION;

            // ver 207
            mobj.painchance := mobjinfo[mobj._type].painchance;
            mobj.spriteDX := mobjinfo[Ord(mobj._type)].spriteDX;
            mobj.spriteDY := mobjinfo[Ord(mobj._type)].spriteDY;

            Z_Free(mobj111);
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
          if mobj.player <> nil then
          begin
            mobj.player := @players[integer(mobj.player) - 1];

            Pplayer_t(mobj.player).mo := mobj;
          end;

          if mobj.customparams <> nil then
          begin
            mobj.customparams := nil;
            repeat
              memcpy(@parm, save_p, SizeOf(mobjcustomparam_t));
              incp(pointer(save_p), SizeOf(mobjcustomparam_t));
              P_SetMobjCustomParam(mobj, parm.name, parm.value);
            until parm.next = nil;
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
    tc_floor,
    tc_plat,
    tc_flash,
    tc_strobe,
    tc_glow,
    tc_fireflicker, // JVAL 20171211 correct T_FireFlicker savegame bug
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
  flicker: Pfireflicker_t;
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
        ceiling.sector := Psector_t(pDiff(ceiling.sector, @sectors[0], SizeOf(sector_t)));
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
      ceiling.sector := Psector_t(pDiff(ceiling.sector, @sectors[0], SizeOf(sector_t)));
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
      door.sector := Psector_t(pDiff(door.sector, @sectors[0], SizeOf(sector_t)));
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
      floor.sector := Psector_t(pDiff(floor.sector, @sectors[0], SizeOf(sector_t)));
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
      plat.sector := Psector_t(pDiff(plat.sector, @sectors[0], SizeOf(sector_t)));
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
      flash.sector := Psector_t(pDiff(flash.sector, @sectors[0], SizeOf(sector_t)));
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
      strobe.sector := Psector_t(pDiff(strobe.sector, @sectors[0], SizeOf(sector_t)));
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
      glow.sector := Psector_t(glow.sector.iSectorID);
      continue;
    end;

    if @th._function.acp1 = @T_FireFlicker then
    begin
      save_p[0] := Ord(tc_fireflicker);
      save_p := @save_p[1];
      PADSAVEP;
      flicker := Pfireflicker_t(save_p);
      memcpy(flicker, th, SizeOf(fireflicker_t));
      incp(pointer(save_p), SizeOf(fireflicker_t));
      flicker.sector := Psector_t(flicker.sector.iSectorID);
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
  floor: Pfloormove_t;
  plat: Pplat_t;
  flash: Plightflash_t;
  strobe: Pstrobe_t;
  glow: Pglow_t;
  flicker: Pfireflicker_t;
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
          ceiling.sector.specialdata := ceiling;

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
          door.sector.specialdata := door;
          @door.thinker._function.acp1 := @T_VerticalDoor;
          P_AddThinker(@door.thinker);
        end;

      Ord(tc_floor):
        begin
          PADSAVEP;
          floor := Z_Malloc(SizeOf(floormove_t), PU_LEVEL, nil);
          memcpy(floor, save_p, SizeOf(floormove_t));
          incp(pointer(save_p), SizeOf(floormove_t));
          floor.sector := @sectors[integer(floor.sector)];
          floor.sector.specialdata := floor;
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
          plat.sector.specialdata := plat;

          if Assigned(plat.thinker._function.acp1) then  // JVAL ??? from serialization
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

procedure P_UnArchiveGlobalVariables(const vars: TGlobalVariablesList);
var
  sz: integer;
begin
  if savegameversion <= VERSION114 then
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
  if savegameversion <= VERSION114 then
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
  if savegameversion <= VERSION114 then
    Exit;

  overlay.SaveToBuffer(Pointer(save_p));
end;

procedure P_UnArchiveOverlay;
begin
  if savegameversion <= VERSION114 then
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
