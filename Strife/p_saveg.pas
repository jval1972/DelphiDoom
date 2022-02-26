//------------------------------------------------------------------------------
//
//  DelphiStrife is a source port of the game Strife.
//
//  Based on:
//    - Linux Doom by "id Software"
//    - Chocolate Strife by "Simon Howard"
//    - DelphiDoom by "Jim Valavanis"
//
//  Copyright (C) 1993-1996 by id Software, Inc.
//  Copyright (C) 2005 Simon Howard
//  Copyright (C) 2010 James Haley, Samuel Villarreal
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
//    Savegame I/O, archiving, persistence.
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit p_saveg;

interface

uses
  d_delphi;

//==============================================================================
// P_ArchivePlayers
//
// Persistent storage/archiving.
// These are the load / save game routines.
//
//==============================================================================
procedure P_ArchivePlayers;

//==============================================================================
//
// P_UnArchivePlayers
//
//==============================================================================
procedure P_UnArchivePlayers(userload: boolean);

//==============================================================================
//
// P_ArchiveWorld
//
//==============================================================================
procedure P_ArchiveWorld;

//==============================================================================
//
// P_UnArchiveWorld
//
//==============================================================================
procedure P_UnArchiveWorld;

//==============================================================================
//
// P_ArchiveThinkers
//
//==============================================================================
procedure P_ArchiveThinkers;

//==============================================================================
//
// P_UnArchiveThinkers
//
//==============================================================================
procedure P_UnArchiveThinkers;

//==============================================================================
//
// P_ArchiveSpecials
//
//==============================================================================
procedure P_ArchiveSpecials;

//==============================================================================
//
// P_UnArchiveSpecials
//
//==============================================================================
procedure P_UnArchiveSpecials;

//==============================================================================
//
// P_ArchiveMapVariables
//
//==============================================================================
procedure P_ArchiveMapVariables;

//==============================================================================
//
// P_ArchiveWorldVariables
//
//==============================================================================
procedure P_ArchiveWorldVariables(const fname: string);

//==============================================================================
//
// P_UnArchiveMapVariables
//
//==============================================================================
procedure P_UnArchiveMapVariables;

//==============================================================================
//
// P_UnArchiveWorldVariables
//
//==============================================================================
procedure P_UnArchiveWorldVariables(const fname: string);

//==============================================================================
//
// P_ArchivePSMapScript
//
//==============================================================================
procedure P_ArchivePSMapScript;

//==============================================================================
//
// P_UnArchivePSMapScript
//
//==============================================================================
procedure P_UnArchivePSMapScript;

//==============================================================================
//
// P_ArchiveOverlay
//
//==============================================================================
procedure P_ArchiveOverlay;

//==============================================================================
//
// P_UnArchiveOverlay
//
//==============================================================================
procedure P_UnArchiveOverlay;

//==============================================================================
//
// P_ArchiveScreenShot
//
//==============================================================================
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
  p_acs,
  p_local,    // JVAL: sector gravity (VERSION 204)
  p_playertrace,
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
  p_udmf,
  po_man,
  ps_main,
  psi_globals,
  psi_overlay,
  r_defs,
  r_data,
  r_colormaps,
  r_translations,
  sv_strife,
  udmf_ceilng,
  udmf_doors,
  udmf_floor,
  udmf_lights,
  udmf_plats,
  w_wad,
  z_zone;

//==============================================================================
// PADSAVEP
//
// Pads save_p to a 4-byte boundary
//  so that the load/save works on SGI&Gecko.
//
//==============================================================================
procedure PADSAVEP;
begin
  if savegameversion < VERSION122 then
    save_p := PByteArray(integer(save_p) + ((4 - (integer(save_p) and 3) and 3)));
end;

//==============================================================================
//
// P_ArchivePlayers
//
//==============================================================================
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
    if dest.plinetarget <> nil then
      dest.plinetarget := Pmobj_t(dest.plinetarget.key);

    // JVAL: 20211224 - Save player history
    memcpy(save_p, @playerhistory[i], SizeOf(playertracehistory_t));
    incp(pointer(save_p), SizeOf(playertracehistory_t));
  end;
end;

//==============================================================================
//
// P_UnArchivePlayers
//
//==============================================================================
procedure P_UnArchivePlayers(userload: boolean);
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
      if userload then
        memcpy(@players[i], save_p, SizeOf(player_t));
      incp(pointer(save_p), SizeOf(player_t));
    end
    else if savegameversion >= VERSION206 then
    begin
      if userload then
      begin
        memcpy(@players[i], save_p, SizeOf(player_t206));

        // version 207
        players[i].oldcrouch := 0;
        players[i].lastongroundtime := 0;
        players[i].lastautocrouchtime := 0;
        players[i].crouchheight := 0;
        players[i].plinetarget := nil;
        players[i].pcrosstic := 0;
        players[i].nextfire := 0;
      end;
      incp(pointer(save_p), SizeOf(player_t206));
      P_ClearPlayerHistory(@players[i]);
    end
    else if savegameversion >= VERSION204 then
    begin
      if userload then
      begin
        memcpy(@players[i], save_p, SizeOf(player_t205));

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
        players[i].plinetarget := nil;
        players[i].pcrosstic := 0;
        players[i].nextfire := 0;
      end;
      incp(pointer(save_p), SizeOf(player_t205));
      P_ClearPlayerHistory(@players[i]);
    end
    else if savegameversion = VERSION203 then
    begin
      if userload then
      begin
        memcpy(@players[i], save_p, SizeOf(player_t203));

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
        players[i].plinetarget := nil;
        players[i].pcrosstic := 0;
        players[i].nextfire := 0;
      end;
      incp(pointer(save_p), SizeOf(player_t203));
      P_ClearPlayerHistory(@players[i]);
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
        players[i].plinetarget := nil;
        players[i].pcrosstic := 0;
        players[i].nextfire := 0;
      end;
      incp(pointer(save_p), SizeOf(player_t122));
      P_ClearPlayerHistory(@players[i]);
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
        players[i].plinetarget := nil;
        players[i].pcrosstic := 0;
        players[i].nextfire := 0;
      end;
      incp(pointer(save_p), SizeOf(player_t121));
      P_ClearPlayerHistory(@players[i]);
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

    // JVAL: 202111224 - Load player history
    if savegameversion >= VERSION207 then
    begin
      memcpy(@playerhistory[i], save_p, SizeOf(playertracehistory_t));
      incp(pointer(save_p), SizeOf(playertracehistory_t));
    end;
  end;
end;

//==============================================================================
//
// P_ArchiveWorld
//
//==============================================================================
procedure P_ArchiveWorld;
var
  i: integer;
  j: integer;
  sec: Psector_t;
  li: Pline_t;
  si: Pside_t;
  levelinf: Plevelinfo_t;

  procedure _put_char8(const c8: char8_t);
  var
    ii: integer;
    b: byte;
  begin
    for ii := 0 to 7 do
    begin
      b := Ord(c8[ii]);
      save_p[0] := b;
      save_p := @save_p[1];
      if b = 0 then
        break;
    end;
  end;

begin
  levelinf := P_GetLevelInfo(P_GetMapName(gamemap));
  _put_char8(levelinf.musname);
  _put_char8(levelinf.skyflat);

  // do sectors
  i := 0;
  while i < numsectors do
  begin
    sec := Psector_t(@sectors[i]);
    save_p := @save_p[SectorSerializer.SaveToMem(save_p, sec, $FFFF)];

    _put_char8(flats[sec.floorpic].name);
    _put_char8(flats[sec.ceilingpic].name);

    for j := 0 to sec.num_saffectees - 1 do
    begin
      PInteger(save_p)^ := sec.saffectees[j];
      save_p := @save_p[4];
    end;

    inc(i);
  end;

  // do lines
  i := 0;
  while i < numlines do
  begin
    li := Pline_t(@lines[i]);

    save_p := @save_p[LineSerializer.SaveToMem(save_p, li, $FFFF)];

    inc(i);
  end;

  // do sides
  i := 0;
  while i < numsides do
  begin
    si := @sides[i];

    save_p := @save_p[SideSerializer.SaveToMem(save_p, si, $FFFF)];

    _put_char8(R_NameForSideTexture(si.toptexture));
    _put_char8(R_NameForSideTexture(si.bottomtexture));
    _put_char8(R_NameForSideTexture(si.midtexture));

    inc(i);
  end;

  // JVAL: 20220216 - Polyobjs
  PInteger(save_p)^ := po_NumPolyobjs;
  save_p := @save_p[4];
  for i := 0 to po_NumPolyobjs - 1 do
  begin
    PInteger(save_p)^ := polyobjs[i].tag;
    save_p := @save_p[4];
    PLongWord(save_p)^ := polyobjs[i].angle;
    save_p := @save_p[4];
    PInteger(save_p)^ := polyobjs[i].startSpot.x;
    save_p := @save_p[4];
    PInteger(save_p)^ := polyobjs[i].startSpot.y;
    save_p := @save_p[4];
  end;
end;

//==============================================================================
//
// P_UnArchiveWorld206
//
//==============================================================================
procedure P_UnArchiveWorld206;
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
      sec.saffectees := Z_Realloc(sec.saffectees, sec.num_saffectees * SizeOf(integer), PU_LEVEL, nil);
      for j := 0 to sec.num_saffectees - 1 do
      begin
        sec.saffectees[j] := PInteger(get)^;
        get := @get[2];
      end;
    end;


    sec.moreids := [];
    if IsIntegerInRange(sec.tag, 0, 255) then
      Include(sec.moreids, sec.tag);
    sec.seqType := 0;
    sec.lightninglightlevel := 255;
    sec.windthrust := 0;
    sec.windangle := 0;

    sec.touching_thinglist := nil;
    sec.floordata := nil;
    sec.ceilingdata := nil;
    sec.lightingdata := nil;
    sec.soundtarget := nil;
    sec.specialdata := nil;
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

    li.arg1 := 0;
    li.arg2 := 0;
    li.arg3 := 0;
    li.arg4 := 0;
    li.arg5 := 0;
    li.activators := 0;
    li.moreids := [];
    if IsIntegerInRange(li.tag, 0, 255) then
      Include(li.moreids, li.tag);

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

      si.toptextureoffset := 0;
      si.bottomtextureoffset := 0;
      si.midtextureoffset := 0;
      si.toprowoffset := 0;
      si.bottomrowoffset := 0;
      si.midrowoffset := 0;
      si.flags := 0;

    end;
    inc(i);
  end;
  save_p := PByteArray(get);
end;

//==============================================================================
//
// P_UnArchiveWorld
//
//==============================================================================
procedure P_UnArchiveWorld;
var
  i: integer;
  j: integer;
  sec: Psector_t;
  li: Pline_t;
  si: Pside_t;
  levelinf: Plevelinfo_t;
  deltaX: fixed_t;
  deltaY: fixed_t;
  tex: char8_t;

  function _get_char8: char8_t;
  var
    ii: integer;
    ch: Char;
  begin
    for ii := 0 to 7 do
      result[ii] := #0;
    for ii := 0 to 7 do
    begin
      ch := Chr(save_p[0]);
      result[ii] := ch;
      save_p := @save_p[1];
      if ch = #0 then
        break;
    end;
  end;

begin
  if savegameversion <= VERSION206 then
  begin
    P_UnArchiveWorld206;
    exit;
  end;

  levelinf := P_GetLevelInfo(P_GetMapName(gamemap));
  levelinf.musname := _get_char8;
  levelinf.skyflat := _get_char8;

  // do sectors
  i := 0;
  while i < numsectors do
  begin
    sec := Psector_t(@sectors[i]);

    save_p := @save_p[SectorSerializer.LoadFromMem(save_p, sec, $FFFF)];

    sec.floorpic := R_FlatNumForName(_get_char8);
    sec.ceilingpic := R_FlatNumForName(_get_char8);

    sec.saffectees := Z_Realloc(sec.saffectees, sec.num_saffectees * SizeOf(integer), PU_LEVEL, nil);
    for j := 0 to sec.num_saffectees - 1 do
    begin
      sec.saffectees[j] := PInteger(save_p)^;
      save_p := @save_p[4];
    end;

    sec.touching_thinglist := nil;
    sec.floordata := nil;
    sec.ceilingdata := nil;
    sec.lightingdata := nil;
    sec.specialdata := nil;
    sec.soundtarget := nil;
    sec.iSectorID := i; // JVAL: 3d Floors

    inc(i);
  end;

  // do lines
  i := 0;
  while i < numlines do
  begin
    li := Pline_t(@lines[i]);

    save_p := @save_p[LineSerializer.LoadFromMem(save_p, li, $FFFF)];

    inc(i);
  end;

  // do lines
  i := 0;
  while i < numsides do
  begin
    si := Pside_t(@sides[i]);

    save_p := @save_p[SideSerializer.LoadFromMem(save_p, si, $FFFF)];

    tex := _get_char8;
    si.toptexture := R_SafeTextureNumForName(tex);
    if si.toptexture = 0 then
      si.toptexture := -1 - R_CustomColorMapForName(tex);

    tex := _get_char8;
    si.bottomtexture := R_SafeTextureNumForName(tex);
    if si.bottomtexture = 0 then
      si.bottomtexture := -1 - R_CustomColorMapForName(tex);

    tex := _get_char8;
    si.midtexture := R_SafeTextureNumForName(tex);
    if si.midtexture = 0 then
      si.midtexture := -1 - R_CustomColorMapForName(tex);

    inc(i);
  end;

  // JVAL: 20220216 - Polyobjs
  if PInteger(save_p)^ <> po_NumPolyobjs then
    I_Error('P_UnArchiveWorld(): Invalid number of polyobjs (%d should be %d)', [PInteger(save_p)^, po_NumPolyobjs]);
  save_p := @save_p[4];
  for i := 0 to po_NumPolyobjs - 1 do
  begin
    if PInteger(save_p)^ <> polyobjs[i].tag then
      I_Error('P_UnArchiveWorld(): Invalid polyobj tag');
    save_p := @save_p[4];
    PO_RotatePolyobj(polyobjs[i].tag, PLongWord(save_p)^);
    save_p := @save_p[4];
    deltaX := PInteger(save_p)^ - polyobjs[i].startSpot.x;
    save_p := @save_p[4];
    deltaY := PInteger(save_p)^ - polyobjs[i].startSpot.y;
    save_p := @save_p[4];
    PO_MovePolyobj(polyobjs[i].tag, deltaX, deltaY);
    polyobjs[i].prevangle := polyobjs[i].angle; // Interpolation
    polyobjs[i].prevx := polyobjs[i].startSpot.x;
    polyobjs[i].prevy := polyobjs[i].startSpot.y;
  end;
end;

//
// Thinkers
//
type
  thinkerclass_t = (tc_end, tc_mobj);

//==============================================================================
//
// P_ArchiveThinkers
//
//==============================================================================
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

      mobj := Pmobj_t(th);
      save_p := @save_p[MobjSerializer.SaveToMem(save_p, mobj, $FFFF)];

      parm := mobj.customparams;
      while parm <> nil do
      begin
        parm1 := Pmobjcustomparam_t(save_p);
        memcpy(parm1, parm, SizeOf(mobjcustomparam_t));
        incp(pointer(save_p), SizeOf(mobjcustomparam_t));
        parm := parm.next;
      end;
    end;
    th := th.next;
  end;

  // add a terminating marker
  save_p[0] := Ord(tc_end);
  save_p := @save_p[1];
end;

//==============================================================================
//
// P_UnArchiveOldPmobj
//
//==============================================================================
function P_UnArchiveOldPmobj(const amobj: Pmobj_t): boolean;
var
  mobj206: mobj_t206;
  mobj: Pmobj_t206;
  m: TDMemoryStream;
begin
  ZeroMemory(amobj, SizeOf(mobj_t));
  ZeroMemory(@mobj206, SizeOf(mobj_t206));
  mobj := @mobj206;
  if savegameversion = VERSION206 then
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

    result := true;
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

    result := true;
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

    result := true;
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

    result := true;
  end
  else
    result := false;
  if result then
  begin
    m := TDMemoryStream.Create;
    MobjSerializer206.SaveToStream(m, mobj, $FFFF);
    MobjSerializer.LoadFromMem(m.Memory, amobj, m.Size);
    m.Free;
  end;
end;

//==============================================================================
//
// P_UnArchiveThinkers
//
//==============================================================================
procedure P_UnArchiveThinkers;
var
  i: integer;
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
              begin
                if players[i].lastdialogtalker <> nil then
                  players[i].lastdialogtalker := P_FindMobjFromKey(integer(players[i].lastdialogtalker));
                if players[i].plinetarget <> nil then
                  players[i].plinetarget := P_FindMobjFromKey(integer(players[i].plinetarget));
              end;
          end;
          exit; // end of list
        end;

      Ord(tc_mobj):
        begin
          PADSAVEP;
          mobj := Z_Malloc(SizeOf(mobj_t), PU_LEVEL, nil);

          if savegameversion >= VERSION207 then
            save_p := @save_p[MobjSerializer.LoadFromMem(save_p, mobj, $FFFF)]
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

          R_InitMobjTranslation(mobj);
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
    tc_endspecials206,
    tch_movefloor,
    tch_platraise,
    tch_moveceiling,
    tch_light,
    tch_verticaldoor,
    tch_phase,
    tch_interpretacs,
    tch_rotatepoly,
    tch_buildpillar,
    tch_movepoly,
    tch_polydoor,
    tch_floorwaggle,
    tc_endspecials
  );

//==============================================================================
// P_ArchiveSpecials
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
//==============================================================================
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
  light: Plight_t;
  phase: Pphase_t;
  acs: Pacs_t;
  polyevent: Ppolyevent_t;
  pillar: Ppillar_t;
  polydoor: Ppolydoor_t;
  floorwaggle: PfloorWaggle_t;
  i: integer;

  function _savesector(const sec: Psector_t): Psector_t;
  begin
    if sec <> nil then
      result := Psector_t(pDiff(door.sector, sectors, SizeOf(sector_t)))
    else
      result := Psector_t(-1);
  end;

begin
  // save off the current thinkers
  th1 := thinkercap.next;
  while th1 <> @thinkercap do
  begin
    th := th1;
    th1 := th1.next;
    if @th._function.acp1 = @P_MobjThinker then
      continue;

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
        continue;
      end;

      // JVAL: 20220218 - Handle hactiveceilings
      i := 0;
      while i < MAXCEILINGS do
      begin
        if hactiveceilings[i] = Pceiling_t(th) then
          break;
        inc(i);
      end;

      if i < MAXCEILINGS then
      begin
        save_p[0] := Ord(tch_moveceiling);
        save_p := @save_p[1];
        PADSAVEP;
        ceiling := Pceiling_t(save_p);
        memcpy(ceiling, th, SizeOf(ceiling_t));
        incp(pointer(save_p), SizeOf(ceiling_t));
        ceiling.sector := _savesector(ceiling.sector);
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
      incp(pointer(save_p), SizeOf(fireflicker_t));
      flicker.sector := Psector_t(flicker.sector.iSectorID);
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

    // JVAL: 20220218 - Handle UDMF special effects
    if @th._function.acp1 = @TH_MoveFloor then
    begin
      save_p[0] := Ord(tch_movefloor);
      save_p := @save_p[1];
      PADSAVEP;
      floor := Pfloormove_t(save_p);
      memcpy(floor, th, SizeOf(floormove_t));
      incp(pointer(save_p), SizeOf(floormove_t));
      floor.sector := _savesector(floor.sector);
      continue;
    end;

    if @th._function.acp1 = @TH_PlatRaise then
    begin
      save_p[0] := Ord(tch_platraise);
      save_p := @save_p[1];
      PADSAVEP;
      plat := Pplat_t(save_p);
      memcpy(plat, th, SizeOf(plat_t));
      incp(pointer(save_p), SizeOf(plat_t));
      plat.sector := _savesector(plat.sector);
      continue;
    end;

    if @th._function.acp1 = @TH_MoveCeiling then
    begin
      save_p[0] := Ord(tch_moveceiling);
      save_p := @save_p[1];
      PADSAVEP;
      ceiling := Pceiling_t(save_p);
      memcpy(ceiling, th, SizeOf(ceiling_t));
      incp(pointer(save_p), SizeOf(ceiling_t));
      ceiling.sector := _savesector(ceiling.sector);
      continue;
    end;

    if @th._function.acp1 = @TH_Light then
    begin
      save_p[0] := Ord(tch_light);
      save_p := @save_p[1];
      PADSAVEP;
      light := Plight_t(save_p);
      memcpy(light, th, SizeOf(light_t));
      incp(pointer(save_p), SizeOf(light_t));
      light.sector := _savesector(light.sector);
      continue;
    end;

    if @th._function.acp1 = @TH_VerticalDoor then
    begin
      save_p[0] := Ord(tch_verticaldoor);
      save_p := @save_p[1];
      PADSAVEP;
      door := Pvldoor_t(save_p);
      memcpy(door, th, SizeOf(vldoor_t));
      incp(pointer(save_p), SizeOf(vldoor_t));
      door.sector := _savesector(door.sector);
      continue;
    end;

    if @th._function.acp1 = @TH_Phase then
    begin
      save_p[0] := Ord(tch_phase);
      save_p := @save_p[1];
      PADSAVEP;
      phase := Pphase_t(save_p);
      memcpy(phase, th, SizeOf(phase_t));
      incp(pointer(save_p), SizeOf(phase_t));
      phase.sector := _savesector(phase.sector);
      continue;
    end;

    if @th._function.acp1 = @TH_InterpretACS then
    begin
      save_p[0] := Ord(tch_interpretacs);
      save_p := @save_p[1];
      PADSAVEP;
      acs := Pacs_t(save_p);
      memcpy(acs, th, SizeOf(acs_t));
      incp(pointer(save_p), SizeOf(acs_t));
      acs.ip := PInteger(LongWord(acs.ip) - LongWord(ActionCodeBase));
      if acs.line <> nil then
        acs.line := Pline_t(pDiff(acs.line, lines, SizeOf(line_t)))
      else
        acs.line := Pline_t(-1);
      if acs.activator <> nil then
        acs.activator := Pmobj_t(acs.activator.key);
      continue;
    end;

    if @th._function.acp1 = @TH_RotatePoly then
    begin
      save_p[0] := Ord(tch_rotatepoly);
      save_p := @save_p[1];
      PADSAVEP;
      polyevent := Ppolyevent_t(save_p);
      memcpy(polyevent, th, SizeOf(polyevent_t));
      incp(pointer(save_p), SizeOf(polyevent_t));
      continue;
    end;

    if @th._function.acp1 = @TH_BuildPillar then
    begin
      save_p[0] := Ord(tch_buildpillar);
      save_p := @save_p[1];
      PADSAVEP;
      pillar := Ppillar_t(save_p);
      memcpy(pillar, th, SizeOf(pillar_t));
      incp(pointer(save_p), SizeOf(pillar_t));
      pillar.sector := _savesector(pillar.sector);
      continue;
    end;

    if @th._function.acp1 = @TH_MovePoly then
    begin
      save_p[0] := Ord(tch_movepoly);
      save_p := @save_p[1];
      PADSAVEP;
      polyevent := Ppolyevent_t(save_p);
      memcpy(polyevent, th, SizeOf(polyevent_t));
      incp(pointer(save_p), SizeOf(polyevent_t));
      continue;
    end;

    if @th._function.acp1 = @TH_PolyDoor then
    begin
      save_p[0] := Ord(tch_polydoor);
      save_p := @save_p[1];
      PADSAVEP;
      polydoor := Ppolydoor_t(save_p);
      memcpy(polydoor, th, SizeOf(polydoor_t));
      incp(pointer(save_p), SizeOf(polydoor_t));
      continue;
    end;

    if @th._function.acp1 = @TH_FloorWaggle then
    begin
      save_p[0] := Ord(tch_floorwaggle);
      save_p := @save_p[1];
      PADSAVEP;
      floorwaggle := PfloorWaggle_t(save_p);
      memcpy(floorwaggle, th, SizeOf(floorWaggle_t));
      incp(pointer(save_p), SizeOf(floorWaggle_t));
      floorwaggle.sector := _savesector(floorwaggle.sector);
      continue;
    end;

  end;
  // add a terminating marker
  save_p[0] := Ord(tc_endspecials);
  save_p := @save_p[1];
end;

//==============================================================================
//
// P_UnArchiveSpecials
//
//==============================================================================
procedure P_UnArchiveSpecials;
var
  tclass: byte;
  ceiling: Pceiling_t;
  door: Pvldoor_t;
  sdoor: Pslidedoor_t;
  floor: Pfloormove_t;
  floor206: Pfloormove_t206;
  plat: Pplat_t;
  flash: Plightflash_t;
  strobe: Pstrobe_t;
  glow: Pglow_t;
  scroll: Pscroll_t;
  friction: Pfriction_t;
  pusher: Ppusher_t;
  flicker: Pfireflicker_t;
  elevator: Pelevator_t;
  light: Plight_t;
  phase: Pphase_t;
  acs: Pacs_t;
  polyevent: Ppolyevent_t;
  pillar: Ppillar_t;
  polydoor: Ppolydoor_t;
  floorwaggle: PfloorWaggle_t;

  function _restoresector(const sec: Psector_t): Psector_t;
  begin
    if sec = Psector_t(-1) then
      result := nil
    else
      result := @sectors[integer(sec)];
  end;

begin
  // read in saved thinkers
  while true do
  begin
    tclass := save_p[0];
    save_p := @save_p[1];
    case tclass of
      Ord(tc_endspecials):
        exit; // end of list
      Ord(tc_endspecials206):
        exit; // end of list (VERSION <= 206)

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
          if savegameversion <= VERSION206 then // JVAL: tc_elevator = old value of tc_endspecials
          begin
            floor206 := Pfloormove_t206(save_p);
            incp(pointer(save_p), SizeOf(floormove_t206));
            ZeroMemory(floor, SizeOf(floormove_t));
            floor.thinker := floor206.thinker;
            floor._type := floor206._type;
            floor.crush := floor206.crush;
            floor.sector := floor206.sector;
            floor.direction := floor206.direction;
            floor.newspecial := floor206.newspecial;
            floor.oldspecial := floor206.oldspecial;
            floor.texture := floor206.texture;
            floor.floordestheight := floor206.floordestheight;
            floor.speed := floor206.speed;
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

      // JVAL: 20220218 - Handle UDMF special effects
      Ord(tch_moveceiling):
        begin
          PADSAVEP;
          ceiling := Z_Malloc(SizeOf(ceiling_t), PU_LEVEL, nil);
          memcpy(ceiling, save_p, SizeOf(ceiling_t));
          incp(pointer(save_p), SizeOf(ceiling_t));
          ceiling.sector := _restoresector(ceiling.sector);
          if ceiling.sector <> nil then
            ceiling.sector.specialdata := ceiling;

          if Assigned(ceiling.thinker._function.acp1) then
            @ceiling.thinker._function.acp1 := @TH_MoveCeiling;

          P_AddThinker(@ceiling.thinker);
          PH_AddActiveCeiling(ceiling);
        end;

      Ord(tch_movefloor):
        begin
          PADSAVEP;
          floor := Z_Malloc(SizeOf(floormove_t), PU_LEVEL, nil);
          memcpy(floor, save_p, SizeOf(floormove_t));
          incp(pointer(save_p), SizeOf(floormove_t));
          floor.sector := _restoresector(floor.sector);
          if floor.sector <> nil then
            floor.sector.specialdata := floor;

          if Assigned(floor.thinker._function.acp1) then
            @floor.thinker._function.acp1 := @TH_MoveFloor;

          P_AddThinker(@floor.thinker);
        end;

      Ord(tch_platraise):
        begin
          PADSAVEP;
          plat := Z_Malloc(SizeOf(plat_t), PU_LEVEL, nil);
          memcpy(plat, save_p, SizeOf(plat_t));
          incp(pointer(save_p), SizeOf(plat_t));
          plat.sector := _restoresector(plat.sector);
          if plat.sector <> nil then
            plat.sector.specialdata := plat;

          if Assigned(plat.thinker._function.acp1) then
            @plat.thinker._function.acp1 := @TH_PlatRaise;

          P_AddThinker(@plat.thinker);
          PH_AddActivePlat(plat);
        end;

      Ord(tch_light):
        begin
          PADSAVEP;
          light := Z_Malloc(SizeOf(light_t), PU_LEVEL, nil);
          memcpy(light, save_p, SizeOf(light_t));
          incp(pointer(save_p), SizeOf(light_t));
          light.sector := _restoresector(light.sector);
          {if light.sector <> nil then
            light.sector.specialdata := light;}

          if Assigned(light.thinker._function.acp1) then
            @light.thinker._function.acp1 := @TH_Light;

          P_AddThinker(@light.thinker);
        end;

      Ord(tch_verticaldoor):
        begin
          PADSAVEP;
          door := Z_Malloc(SizeOf(vldoor_t), PU_LEVEL, nil);
          memcpy(door, save_p, SizeOf(vldoor_t));
          incp(pointer(save_p), SizeOf(vldoor_t));
          door.sector := _restoresector(door.sector);
          if door.sector <> nil then
            door.sector.specialdata := door;

          if Assigned(door.thinker._function.acp1) then
            @door.thinker._function.acp1 := @TH_VerticalDoor;

          P_AddThinker(@door.thinker);
        end;

      Ord(tch_phase):
        begin
          PADSAVEP;
          phase := Z_Malloc(SizeOf(phase_t), PU_LEVEL, nil);
          memcpy(phase, save_p, SizeOf(phase_t));
          incp(pointer(save_p), SizeOf(phase_t));
          phase.sector := _restoresector(phase.sector);
          {if phase.sector <> nil then
            phase.sector.specialdata := phase;}

          if Assigned(phase.thinker._function.acp1) then
            @phase.thinker._function.acp1 := @TH_Phase;

          P_AddThinker(@phase.thinker);
        end;

      Ord(tch_interpretacs):
        begin
          PADSAVEP;
          acs := Z_Malloc(SizeOf(acs_t), PU_LEVEL, nil);
          memcpy(acs, save_p, SizeOf(acs_t));
          incp(pointer(save_p), SizeOf(acs_t));
          acs.ip := @ActionCodeBase[integer(acs.ip)];
          if integer(acs.line) = -1 then
            acs.line := nil
          else
            acs.line := @lines[integer(acs.line)];
          if acs.activator <> nil then
            acs.activator := P_FindMobjFromKey(integer(acs.activator));

          if Assigned(acs.thinker._function.acp1) then
            @acs.thinker._function.acp1 := @TH_InterpretACS;

          P_AddThinker(@acs.thinker);
        end;

      Ord(tch_rotatepoly):
        begin
          PADSAVEP;
          polyevent := Z_Malloc(SizeOf(polyevent_t), PU_LEVEL, nil);
          memcpy(polyevent, save_p, SizeOf(polyevent_t));
          incp(pointer(save_p), SizeOf(polyevent_t));

          if Assigned(polyevent.thinker._function.acp1) then
            @polyevent.thinker._function.acp1 := @TH_RotatePoly;

          P_AddThinker(@polyevent.thinker);
        end;

      Ord(tch_buildpillar):
        begin
          PADSAVEP;
          pillar := Z_Malloc(SizeOf(pillar_t), PU_LEVEL, nil);
          memcpy(pillar, save_p, SizeOf(pillar_t));
          incp(pointer(save_p), SizeOf(pillar_t));
          pillar.sector := _restoresector(pillar.sector);
          if pillar.sector <> nil then
            pillar.sector.specialdata := pillar;

          if Assigned(pillar.thinker._function.acp1) then
            @pillar.thinker._function.acp1 := @TH_BuildPillar;

          P_AddThinker(@pillar.thinker);
        end;

      Ord(tch_movepoly):
        begin
          PADSAVEP;
          polyevent := Z_Malloc(SizeOf(polyevent_t), PU_LEVEL, nil);
          memcpy(polyevent, save_p, SizeOf(polyevent_t));
          incp(pointer(save_p), SizeOf(polyevent_t));

          if Assigned(polyevent.thinker._function.acp1) then
            @polyevent.thinker._function.acp1 := @TH_MovePoly;

          P_AddThinker(@polyevent.thinker);
        end;

      Ord(tch_polydoor):
        begin
          PADSAVEP;
          polydoor := Z_Malloc(SizeOf(polydoor_t), PU_LEVEL, nil);
          memcpy(polydoor, save_p, SizeOf(polydoor_t));
          incp(pointer(save_p), SizeOf(polydoor_t));

          if Assigned(polydoor.thinker._function.acp1) then
            @polydoor.thinker._function.acp1 := @TH_PolyDoor;

          P_AddThinker(@polydoor.thinker);
        end;

      Ord(tch_floorwaggle):
        begin
          PADSAVEP;
          floorwaggle := Z_Malloc(SizeOf(floorWaggle_t), PU_LEVEL, nil);
          memcpy(floorwaggle, save_p, SizeOf(floorWaggle_t));
          incp(pointer(save_p), SizeOf(floorWaggle_t));
          floorwaggle.sector := _restoresector(floorwaggle.sector);
          if floorwaggle.sector <> nil then
            floorwaggle.sector.specialdata := floorwaggle;

          if Assigned(floorwaggle.thinker._function.acp1) then
            @floorwaggle.thinker._function.acp1 := @TH_FloorWaggle;

          P_AddThinker(@floorwaggle.thinker);
        end;

      else
        I_Error('P_UnarchiveSpecials(): Unknown tclass %d in savegame', [tclass]);
    end;
  end;
end;

//==============================================================================
//
// P_ArchiveGlobalVariables
//
//==============================================================================
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

//==============================================================================
//
// P_ArchiveMapVariables
//
//==============================================================================
procedure P_ArchiveMapVariables;
begin
  P_ArchiveGlobalVariables(mapvars, save_p);
end;

//==============================================================================
//
// P_ArchiveWorldVariables
//
//==============================================================================
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

//==============================================================================
//
// P_UnArchiveGlobalVariables
//
//==============================================================================
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

//==============================================================================
//
// P_UnArchiveMapVariables
//
//==============================================================================
procedure P_UnArchiveMapVariables;
begin
  P_UnArchiveGlobalVariables(mapvars, save_p);
end;

//==============================================================================
//
// P_UnArchiveWorldVariables
//
//==============================================================================
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

//==============================================================================
//
// P_ArchivePSMapScript
//
//==============================================================================
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

//==============================================================================
//
// P_UnArchivePSMapScript
//
//==============================================================================
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

//==============================================================================
//
// P_ArchiveOverlay
//
//==============================================================================
procedure P_ArchiveOverlay;
begin
  overlay.SaveToBuffer(Pointer(save_p));
end;

//==============================================================================
//
// P_UnArchiveOverlay
//
//==============================================================================
procedure P_UnArchiveOverlay;
begin
  if savegameversion <= VERSION121 then
    Exit;

  overlay.LoadFromBuffer(Pointer(save_p));
end;

//==============================================================================
//
// P_ArchiveScreenShot
//
//==============================================================================
procedure P_ArchiveScreenShot(const fname: string);
begin
  M_WriteFile(fname, @mn_screenshotbuffer, SizeOf(menuscreenbuffer_t));
end;

end.
