//------------------------------------------------------------------------------
//
//  DelphiHeretic is a source port of the game Heretic and it is
//  based on original Linux Doom as published by "id Software", on
//  Heretic source as published by "Raven" software and DelphiDoom
//  as published by Jim Valavanis.
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
//  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
//  02111-1307, USA.
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit deh_main;

interface

// JVAL
// Support for DeHackEd patch files
// Support for both DEH and BEX files

uses
  d_delphi,
  d_think,
  info_h;

//==============================================================================
//
// DEH_Parse
//
//==============================================================================
procedure DEH_Parse(const s: TDStringList);

//==============================================================================
//
// DEH_CurrentSettings
//
//==============================================================================
function DEH_CurrentSettings: TDStringList;

//==============================================================================
//
// DEH_Init
//
//==============================================================================
procedure DEH_Init;

//==============================================================================
//
// DEH_ShutDown
//
//==============================================================================
procedure DEH_ShutDown;

const
  DEHMAXACTIONS = 500;

var
  dehnumactions: integer = 0;

type
  deh_action_t = record
    action: actionf_t;
    originalname: string;
    name: string;
    argcount: integer;
    default_args: array[0..MAX_STATE_ARGS - 1] of integer; // mbf21
    {$IFDEF DLL}
    decl: string;
    {$ENDIF}
  end;
  Pdeh_action_t = ^deh_action_t;

type
  deh_string_t = record
    pstr: PString;
    name: string[24];
  end;
  deh_string_tArray = array[0..$FFFF] of deh_string_t;
  Pdeh_string_tArray = ^deh_string_tArray;

  deh_strings_t = record
    numstrings: integer;
    realnumstrings: integer;
    _array: Pdeh_string_tArray;
  end;
  Pdeh_strings_t = ^deh_strings_t;

var
  mobj_tokens: TDTextList;
  mobj_flags: TDTextList;
  mobj_flags2: TDTextList;
  mobj_flags_ex: TDTextList;
  mobj_flags2_ex: TDTextList;
  mobj_flags3_ex: TDTextList;
  mobj_flags4_ex: TDTextList;
  mobj_flags5_ex: TDTextList;
  mobj_flags6_ex: TDTextList;
  mobj_flags_mbf21: TDTextList; // MBF21
  state_tokens: TDTextList;
  state_flags_ex: TDTextList;
  state_flags_mbf21: TDTextList;  // MBF21
  ammo_tokens: TDTextList;
  weapon_tokens: TDTextList;
  weapon_flags_mbf21: TDTextList; // MBF21
  sound_tokens: TDTextList;
  renderstyle_tokens: TDTextList;
  misc_tokens: TDTextList;
  weapontype_tokens: TDTextList;
  ammotype_tokens: TDTextList;
  // MBF21
  infighting_groups: TDTextList;
  projectile_groups: TDTextList;
  splash_groups: TDTextList;

  deh_actions: array[0..DEHMAXACTIONS - 1] of deh_action_t;
  deh_strings: deh_strings_t;

var
  ismbf21: boolean = false; // MBF21

implementation

uses
  TypInfo,
  c_cmds,
  doomdef,
  deh_base,
  d_main,
  h_strings,
  e_endoom,
  f_finale,
  g_game,
  hu_stuff,
  i_system,
  info,
  info_common,
  m_argv,
  m_fixed,
  ps_main,
  p_mobj,
  p_mobj_h,
  p_enemy,
  p_extra,
  p_common,
  p_gender,
  p_pspr,
  p_pspr_h,
  p_inter,
  p_musinfo,
  p_sounds,
  p_simpledialog,
  psi_overlay,
  r_renderstyle,
  r_translations,
  sounddata,
  sounds,
  sc_params,
  sc_engine,
  sc_states,
  v_data;

var
  mobj_tokens_hash: TDEHStringsHashTable;
  mobj_flags_hash: TDEHStringsHashTable;
  mobj_flags2_hash: TDEHStringsHashTable;
  mobj_flags_ex_hash: TDEHStringsHashTable;
  mobj_flags2_ex_hash: TDEHStringsHashTable;
  mobj_flags3_ex_hash: TDEHStringsHashTable;
  mobj_flags4_ex_hash: TDEHStringsHashTable;
  mobj_flags5_ex_hash: TDEHStringsHashTable;
  mobj_flags6_ex_hash: TDEHStringsHashTable;
  mobj_flags_mbf21_hash: TDEHStringsHashTable;

//==============================================================================
//
// DEH_AddString
//
//==============================================================================
procedure DEH_AddString(deh_strings: Pdeh_strings_t; pstr: PString; const name: string);
begin
  if deh_strings.numstrings = deh_strings.realnumstrings then
  begin
    deh_strings.realnumstrings := deh_strings.realnumstrings + 64;
    realloc(
      pointer(deh_strings._array),
      deh_strings.numstrings * SizeOf(deh_string_t),
      deh_strings.realnumstrings * SizeOf(deh_string_t));
  end;

  deh_strings._array[deh_strings.numstrings].pstr := pstr;
  deh_strings._array[deh_strings.numstrings].name := name;
  inc(deh_strings.numstrings);
end;

var
  deh_initialized: boolean = false;

//==============================================================================
//
// DEH_Parse
//
//==============================================================================
procedure DEH_Parse(const s: TDStringList);
var
  i, j, k: integer;
  str: string;
  stmp: string;
  token1: string;
  token2: string;
  token3: string;
  token4: string;
  token5: string;
  group: integer; // MBF21
  fw: string;
  settext: string;
  mustnextline: boolean;

  // Extra xlat
  istransparent: boolean;
  isfriend: boolean;
  isbouncy: boolean;
  istouchy: boolean;

  mobj_no: integer;
  mobj_idx: integer;
  mobj_val: integer;
  mobj_fval: float;

  mobj_flag: integer;
  mobj_setflag: integer;

  state_no: integer;
  state_idx: integer;
  state_val: integer;
  state_flag: integer;
  state_setflag: integer;
  newstate: boolean;

  ammo_no: integer;
  ammo_idx: integer;
  ammo_val: integer;

  weapon_no: integer;
  weapon_idx: integer;
  weapon_val: integer;
  weapon_level: integer;
  weaponinfo_p: Pweaponinfo_tArray;
  weapon_flag: integer;

  sound_no: integer;
  sound_idx: integer;
  sound_val: integer;

  music_idx: integer;

  sprite_idx: integer;
  sprite_val: integer;

  misc_idx: integer;
  misc_val: integer;

  par_episode: integer;
  par_map: integer;
  par_time: integer;

  len1, len2: integer;
  foundtext: boolean;
  foundaction: boolean;

  deh_initialstates: integer;

  code_ptrs: Pactionf_tArray;
begin
  if not deh_initialized then
    DEH_Init;

  deh_initialstates := numstates;

  // JVAL: 20201203 - Preserve initial actions
  code_ptrs := mallocz(deh_initialstates * SizeOf(actionf_t));
  for i := 0 to numstates - 1 do
    code_ptrs[i] := states[i].action;

  i := 0;
  mustnextline := true;
  while i < s.Count do
  begin
    if mustnextline then
      if not DEH_NextLine(s, str, i) then
        break;
    mustnextline := true;

    splitstring_ch(str, token1, token2);

    ////////////////////////////////////////////////////////////////////////////
    if (token1 = 'THING') or (token1 = 'NEWTHING') then
    begin
    ////////////////////////////////////////////////////////////////////////////
    // Parse a thing ///////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
      stmp := parsefirstword(token2);

      if (token1 = 'NEWTHING') or (stmp = '') or (itoa(atoi(stmp)) <> stmp) then
      begin
        if devparm then
        begin
          printf('DEH_Parse(): Expanding mobjinfo table...'#13#10);
          if stmp <> '' then
            printf('             Adding %s mobj'#13#10, [stmp]);
        end;
        mobj_no := Info_GetNewMobjInfo;
        Info_SetMobjName(mobj_no, stmp);
      end
      else
      begin
        // Retrieve think number
        mobj_no := atoi(stmp, -1);

        if stmp = '' then
        begin
          I_Warning('DEH_Parse(): Thing number unspecified after THING keyword'#13#10);
          continue;
        end;

        if (mobj_no <= 0) or (mobj_no > nummobjtypes) then
        begin
          I_Warning('DEH_Parse(): Wrong think number = %d'#13#10, [mobj_no]);
          continue;
        end;
        dec(mobj_no); // JVAL DEH patches start Think numbers from 1
      end;

      istransparent := false;
      isfriend := false;
      isbouncy := false;
      istouchy := false;

      while true do
      begin
        if not DEH_NextLine(s, str, i) then
          break;

        if CharPos('=', str) = 0 then
        begin
          mustnextline := false; // Already got line
          break;
        end;

      // Retrieve current think field index
        splitstring_ch(str, token1, token2, '=');
        token2 := RemoveQuotesFromString(token2);
        mobj_idx := mobj_tokens_hash.IndexOf(token1);

        if mobj_idx = -1 then
        begin
          I_Warning('DEH_Parse(): Wrong think field = %s (Think number = %d)'#13#10, [token1, mobj_no]);
          mustnextline := false; // Already got line
          break;
        end;

        mobj_val := atoi(token2, -1);

        if mobj_idx in [1, 3, 7, 10, 11, 12, 13, 23, 38, 40, 64, 72] then
        begin
          stmp := firstword_ch(token2);
          if (stmp = 'NEWFRAME') or (stmp = 'NEWSTATE') then  // JVAL: a new defined state
          begin
            mobj_val := atoi(secondword(token2), -1);
            if mobj_val < 0 then
            begin
              I_Warning('DEH_Parse(): After %s keyword found invalid numeric %s (Think number = %d)'#13#10, [stmp, secondword(token2), mobj_no]);
              continue;
            end;
            mobj_val := mobj_val + deh_initialstates;
          end
          else if (stmp = 'ORIGINALFRAME') or (stmp = 'ORIGINALSTATE') then  // JVAL: an original defined state
          begin
            mobj_val := atoi(secondword(token2), -1);
            if mobj_val < 0 then
            begin
              I_Warning('DEH_Parse(): After %s keyword found invalid numeric %s (Think number = %d)'#13#10, [stmp, secondword(token2), mobj_no]);
              continue;
            end;
          end
        end;

        case mobj_idx of
           0: mobjinfo[mobj_no].doomednum := mobj_val;
           1: mobjinfo[mobj_no].spawnstate := mobj_val;
           2: mobjinfo[mobj_no].spawnhealth := mobj_val;
           3: mobjinfo[mobj_no].seestate := mobj_val;
           4: mobjinfo[mobj_no].seesound := S_GetSoundNumForName(token2);
           5: mobjinfo[mobj_no].reactiontime := mobj_val;
           6: mobjinfo[mobj_no].attacksound := S_GetSoundNumForName(token2);
           7: mobjinfo[mobj_no].painstate := mobj_val;
           8: mobjinfo[mobj_no].painchance := mobj_val;
           9: mobjinfo[mobj_no].painsound := S_GetSoundNumForName(token2);
          10: mobjinfo[mobj_no].meleestate := mobj_val;
          11: mobjinfo[mobj_no].missilestate := mobj_val;
          12: mobjinfo[mobj_no].deathstate := mobj_val;
          13: mobjinfo[mobj_no].xdeathstate := mobj_val;
          14: mobjinfo[mobj_no].deathsound := S_GetSoundNumForName(token2);
          15: mobjinfo[mobj_no].speed := mobj_val;
          16,
          74: mobjinfo[mobj_no].radius := mobj_val;
          17: mobjinfo[mobj_no].height := mobj_val;
          18: mobjinfo[mobj_no].mass := mobj_val;
          19: mobjinfo[mobj_no].damage := mobj_val;
          20,
          73: mobjinfo[mobj_no].activesound := S_GetSoundNumForName(token2);
          21: begin
                if itoa(mobj_val) = token2 then
                begin
                  mobjinfo[mobj_no].flags := mobj_val;
                  if not istransparent then
                    istransparent := mobj_val < 0;
                  if not isfriend then
                    isfriend := mobj_val and (1 shl 30) <> 0;
                  if not isbouncy then
                    isbouncy := mobj_val and (1 shl 29) <> 0;
                  if not istouchy then
                    istouchy := mobj_val and (1 shl 28) <> 0;
                end
                else
                begin
                  mobj_setflag := -1;
                  repeat
                    splitstring(token2, token3, token4, [' ', '|', ',', '+']);
                    trimproc(token3);
                    mobj_flag := mobj_flags_hash.IndexOf('MF_' + token3);
                    if mobj_flag = -1 then
                      mobj_flag := mobj_flags_hash.IndexOf(token3);
                    if mobj_flag >= 0 then
                    begin
                      if mobj_flag = 31 then
                        istransparent := true
                      else if mobj_flag = 30 then
                        isfriend := true
                      else if mobj_flag = 29 then
                        isbouncy := true
                      else if mobj_flag = 28 then
                        istouchy := true
                      else
                      begin
                        if mobj_setflag = -1 then
                          mobj_setflag := 0;
                        mobj_flag := _SHL(1, mobj_flag);
                        mobj_setflag := mobj_setflag or mobj_flag;
                      end;
                    end;
                    token2 := strtrim(token4);
                  until token2 = '';
                  if mobj_setflag <> -1 then
                  begin
                    mobjinfo[mobj_no].flags := mobj_setflag;
                    if not istransparent then
                      istransparent := mobj_setflag < 0;
                    if not isfriend then
                      isfriend := mobj_setflag and (1 shl 30) <> 0;
                    if not isbouncy then
                      isbouncy := mobj_setflag and (1 shl 29) <> 0;
                    if not istouchy then
                      istouchy := mobj_setflag and (1 shl 28) <> 0;
                  end;
                end;
              end;
          22: begin
                if itoa(mobj_val) = token2 then
                  mobjinfo[mobj_no].flags2 := mobj_val
                else
                begin
                  mobj_setflag := -1;
                  repeat
                    splitstring(token2, token3, token4, [' ', '|', ',', '+']);
                    trimproc(token3);
                    mobj_flag := mobj_flags2_hash.IndexOf('MF2_' + token3);
                    if mobj_flag = -1 then
                      mobj_flag := mobj_flags2_hash.IndexOf('MF_' + token3);
                    if mobj_flag = -1 then
                      mobj_flag := mobj_flags2_hash.IndexOf(token3);
                    if mobj_flag >= 0 then
                    begin
                      if mobj_setflag = -1 then
                        mobj_setflag := 0;
                      mobj_flag := _SHL(1, mobj_flag);
                      mobj_setflag := mobj_setflag or mobj_flag;
                    end;
                    token2 := strtrim(token4);
                  until token2 = '';
                  if mobj_setflag <> -1 then
                    mobjinfo[mobj_no].flags2 := mobj_setflag;

                end;
              end;
          23: mobjinfo[mobj_no].raisestate := mobj_val;
          24: begin
                if itoa(mobj_val) = token2 then
                  mobjinfo[mobj_no].flags_ex := mobj_val  // DelphiHeretic specific (lighting, transparency, etc)
                else
                begin
                  mobj_setflag := -1;
                  repeat
                    splitstring(token2, token3, token4, [' ', '|', ',', '+']);
                    trimproc(token3);
                    mobj_flag := mobj_flags_ex_hash.IndexOf('MF_EX_' + token3);
                    if mobj_flag = -1 then
                      mobj_flag := mobj_flags_ex_hash.IndexOf('MF_' + token3);
                    if mobj_flag = -1 then
                      mobj_flag := mobj_flags_ex_hash.IndexOf(token3);
                    if mobj_flag >= 0 then
                    begin
                      if mobj_setflag = -1 then
                        mobj_setflag := 0;
                      mobj_flag := _SHL(1, mobj_flag);
                      mobj_setflag := mobj_setflag or mobj_flag;
                    end;
                    token2 := strtrim(token4);
                  until token2 = '';
                  if mobj_setflag <> -1 then
                    mobjinfo[mobj_no].flags_ex := mobj_setflag;

                end;
              end;
          25: mobjinfo[mobj_no].customsound1 := S_GetSoundNumForName(token2); // DelphiDoom specific
          26: mobjinfo[mobj_no].customsound2 := S_GetSoundNumForName(token2); // DelphiDoom specific
          27: mobjinfo[mobj_no].customsound3 := S_GetSoundNumForName(token2); // DelphiDoom specific
          28: mobjinfo[mobj_no].explosiondamage := mobj_val;
          29: mobjinfo[mobj_no].explosionradius := mobj_val;
          30: mobjinfo[mobj_no].meleedamage := mobj_val;
          31: begin
                mobjinfo[mobj_no].inheritsfrom := Info_GetMobjNumForName(token2);
                if mobjinfo[mobj_no].inheritsfrom = mobj_no then
                  mobjinfo[mobj_no].inheritsfrom := -1;
              end;
          32: Info_SetMobjName(mobj_no, token2);
          33: mobjinfo[mobj_no].meleesound := S_GetSoundNumForName(token2);
          34: begin
                if mobj_val >= 0 then
                begin
                  if mobj_val < Ord(NUMMOBJRENDERSTYLES) then
                    mobjinfo[mobj_no].renderstyle := mobjrenderstyle_t(mobj_val)
                  else
                    I_Warning('DEH_Parse(): Wrong renderstyle = %s (Think number = %d).'#13#10, [token2, mobj_no]);
                end
                else
                begin
                  mobj_val := Ord(R_GetRenderstyleForName(token2));
                  if mobj_val >= 0 then
                    mobjinfo[mobj_no].renderstyle := mobjrenderstyle_t(mobj_val)
                  else
                    I_Warning('DEH_Parse(): Wrong renderstyle = %s (Think number = %d).'#13#10, [token2, mobj_no])
                end;
              end;
          35: mobjinfo[mobj_no].alpha := mobj_val;
          36,
          77: mobjinfo[mobj_no].dropitem := Info_GetMobjNumForName(token2);
          37: mobjinfo[mobj_no].missiletype := Info_GetMobjNumForName(token2);
          38: mobjinfo[mobj_no].healstate := mobj_val;
          39: begin
                if itoa(mobj_val) = token2 then
                  mobjinfo[mobj_no].flags2_ex := mobj_val  // DelphiHeretic specific
                else
                begin
                  mobj_setflag := -1;
                  repeat
                    splitstring(token2, token3, token4, [' ', '|', ',', '+']);
                    mobj_flag := mobj_flags2_ex_hash.IndexOf('MF2_EX_' + token3);
                    if mobj_flag = -1 then
                      mobj_flag := mobj_flags2_ex_hash.IndexOf(token3);
                    if mobj_flag >= 0 then
                    begin
                      if mobj_setflag = -1 then
                        mobj_setflag := 0;
                      mobj_flag := _SHL(1, mobj_flag);
                      mobj_setflag := mobj_setflag or mobj_flag;
                    end;
                    token2 := token4;
                  until token2 = '';
                  if mobj_setflag <> -1 then
                    mobjinfo[mobj_no].flags2_ex := mobj_setflag;

                end;
              end;
          40: mobjinfo[mobj_no].crashstate := mobj_val;
          41: mobjinfo[mobj_no].vspeed := mobj_val;
          42: mobjinfo[mobj_no].pushfactor := DEH_FixedOrFloat(token2, 64);
          43: mobjinfo[mobj_no].scale := DEH_FixedOrFloat(token2, 64);
          44: mobjinfo[mobj_no].gravity := DEH_FixedOrFloat(token2, 64);
          45: begin
                if itoa(mobj_val) = token2 then
                  mobjinfo[mobj_no].flags3_ex := mobj_val  // DelphiDoom specific
                else
                begin
                  mobj_setflag := -1;
                  repeat
                    splitstring(token2, token3, token4, [' ', '|', ',', '+']);
                    mobj_flag := mobj_flags3_ex_hash.IndexOf('MF3_EX_' + token3);
                    if mobj_flag = -1 then
                      mobj_flag := mobj_flags3_ex_hash.IndexOf(token3);
                    if mobj_flag >= 0 then
                    begin
                      if mobj_setflag = -1 then
                        mobj_setflag := 0;
                      mobj_flag := _SHL(1, mobj_flag);
                      mobj_setflag := mobj_setflag or mobj_flag;
                    end;
                    token2 := token4;
                  until token2 = '';
                  if mobj_setflag <> -1 then
                    mobjinfo[mobj_no].flags3_ex := mobj_setflag;

                end;
              end;
          46: begin
                if itoa(mobj_val) = token2 then
                  mobjinfo[mobj_no].flags4_ex := mobj_val  // DelphiDoom specific
                else
                begin
                  mobj_setflag := -1;
                  repeat
                    splitstring(token2, token3, token4, [' ', '|', ',', '+']);
                    mobj_flag := mobj_flags4_ex_hash.IndexOf('MF4_EX_' + token3);
                    if mobj_flag = -1 then
                      mobj_flag := mobj_flags4_ex_hash.IndexOf(token3);
                    if mobj_flag >= 0 then
                    begin
                      if mobj_setflag = -1 then
                        mobj_setflag := 0;
                      mobj_flag := _SHL(1, mobj_flag);
                      mobj_setflag := mobj_setflag or mobj_flag;
                    end;
                    token2 := token4;
                  until token2 = '';
                  if mobj_setflag <> -1 then
                    mobjinfo[mobj_no].flags4_ex := mobj_setflag;

                end;
              end;
          47: mobjinfo[mobj_no].minmissilechance := mobj_val;
          48: mobjinfo[mobj_no].floatspeed := mobj_val;
          49: mobjinfo[mobj_no].normalspeed := mobj_val;
          50: mobjinfo[mobj_no].fastspeed := mobj_val;
          51: mobjinfo[mobj_no].obituary := token2;
          52: mobjinfo[mobj_no].hitobituary := token2;
          53: begin
                if mobj_val >= 0 then
                begin
                  if mobj_val < Ord(NUMGENDERS) then
                    mobjinfo[mobj_no].gender := gender_t(mobj_val)
                  else
                    I_Warning('DEH_Parse(): Wrong gender = %s (Think number = %d).'#13#10, [token2, mobj_no]);
                end
                else
                begin
                  mobj_val := Ord(R_GetGenderForName(token2));
                  if mobj_val >= 0 then
                    mobjinfo[mobj_no].gender := gender_t(mobj_val)
                  else
                    I_Warning('DEH_Parse(): Wrong gender = %s (Think number = %d).'#13#10, [token2, mobj_no])
                end;
              end;
          54: mobjinfo[mobj_no].meleerange := mobj_val;
          55: mobjinfo[mobj_no].maxstepheight := DEH_FixedOrFloat(token2, 64);
          56: mobjinfo[mobj_no].maxdropoffheight := DEH_FixedOrFloat(token2, 64);
          57: mobjinfo[mobj_no].gibhealth := mobj_val;
          58: mobjinfo[mobj_no].maxtargetrange := mobj_val;
          59: mobjinfo[mobj_no].WeaveIndexXY := mobj_val;
          60: mobjinfo[mobj_no].WeaveIndexZ := mobj_val;
          61: mobjinfo[mobj_no].friction := DEH_FixedOrFloat(token2, 64);
          62: mobjinfo[mobj_no].spriteDX := DEH_FixedOrFloat(token2, 256);
          63: mobjinfo[mobj_no].spriteDY := DEH_FixedOrFloat(token2, 256);
          64: mobjinfo[mobj_no].interactstate := mobj_val;
          65: begin
                if itoa(mobj_val) = token2 then
                  mobjinfo[mobj_no].flags5_ex := mobj_val  // DelphiDoom specific
                else
                begin
                  mobj_setflag := -1;
                  repeat
                    splitstring(token2, token3, token4, [' ', '|', ',', '+']);
                    mobj_flag := mobj_flags5_ex_hash.IndexOf('MF5_EX_' + token3);
                    if mobj_flag = -1 then
                      mobj_flag := mobj_flags5_ex_hash.IndexOf(token3);
                    if mobj_flag >= 0 then
                    begin
                      if mobj_setflag = -1 then
                        mobj_setflag := 0;
                      mobj_flag := _SHL(1, mobj_flag);
                      mobj_setflag := mobj_setflag or mobj_flag;
                    end;
                    token2 := token4;
                  until token2 = '';
                  if mobj_setflag <> -1 then
                    mobjinfo[mobj_no].flags5_ex := mobj_setflag;

                end;
              end;
          66: begin
                if itoa(mobj_val) = token2 then
                  mobjinfo[mobj_no].flags6_ex := mobj_val  // DelphiDoom specific
                else
                begin
                  mobj_setflag := -1;
                  repeat
                    splitstring(token2, token3, token4, [' ', '|', ',', '+']);
                    mobj_flag := mobj_flags6_ex_hash.IndexOf('MF6_EX_' + token3);
                    if mobj_flag = -1 then
                      mobj_flag := mobj_flags6_ex_hash.IndexOf(token3);
                    if mobj_flag >= 0 then
                    begin
                      if mobj_setflag = -1 then
                        mobj_setflag := 0;
                      mobj_flag := _SHL(1, mobj_flag);
                      mobj_setflag := mobj_setflag or mobj_flag;
                    end;
                    token2 := token4;
                  until token2 = '';
                  if mobj_setflag <> -1 then
                    mobjinfo[mobj_no].flags6_ex := mobj_setflag;

                end;
              end;
          67: begin // .mbf21flags
                ismbf21 := true;
                if itoa(mobj_val) = token2 then
                  mobjinfo[mobj_no].mbf21bits := mobj_val
                else
                begin
                  mobj_setflag := -1;
                  repeat
                    splitstring(token2, token3, token4, [' ', '|', ',', '+']);
                    if Pos1('MF2_', token3) then
                      Delete(token3, 1, 4);
                    if Pos1('MBM21_', token3) then
                      Delete(token3, 1, 6);
                    if token3 <> '' then
                    begin
                      mobj_flag := mobj_flags_mbf21_hash.IndexOf(token3);
                      if mobj_flag >= 0 then
                      begin
                        if mobj_setflag = -1 then
                          mobj_setflag := 0;
                        mobj_flag := _SHL(1, mobj_flag);
                        mobj_setflag := mobj_setflag or mobj_flag;
                      end;
                    end;
                    token2 := token4;
                  until token2 = '';
                  if mobj_setflag <> -1 then
                    mobjinfo[mobj_no].mbf21bits := mobj_setflag;
                end;
              end;
          68: begin // .infighting_group (MBF21)
                group := Info_InfightingGroupToInt(token2);
                if group <> IG_INVALID then
                  mobjinfo[mobj_no].infighting_group := group;
              end;
          69: begin // .projectile_group (MBF21)
                group := Info_ProjectileGroupToInt(token2);
                if group <> PG_INVALID then
                  mobjinfo[mobj_no].projectile_group := group;
              end;
          70: begin // .splash_group (MBF21)
                group := Info_SplashGroupToInt(token2);
                if group <> SG_INVALID then
                  mobjinfo[mobj_no].splash_group := group;
              end;
          71: mobjinfo[mobj_no].ripsound := S_GetSoundNumForName(token2); // .ripsound (MBF21)
          72: mobjinfo[mobj_no].crushstate := mobj_val;
          75: mobjinfo[mobj_no].bloodcolor := R_GetBloodTranslationIdForName(token2);
          76: mobjinfo[mobj_no].translationname := strupper(token2);
          78: mobjinfo[mobj_no].missileheight := mobj_val;
          79: mobjinfo[mobj_no].meleethreshold := mobj_val;
        end;
      end;

      if istransparent then
        mobjinfo[mobj_no].flags_ex := mobjinfo[mobj_no].flags_ex or MF_EX_TRANSPARENT;
      if isfriend then
        mobjinfo[mobj_no].flags2_ex := mobjinfo[mobj_no].flags2_ex or MF2_EX_FRIEND;
      if isbouncy then
        mobjinfo[mobj_no].flags3_ex := mobjinfo[mobj_no].flags3_ex or MF3_EX_BOUNCE;

      P_ResolveMBF21Flags(@mobjinfo[mobj_no]);
    end

    ////////////////////////////////////////////////////////////////////////////
    else if (token1 = 'FRAME') or (token1 = 'STATE') then
    begin
    ////////////////////////////////////////////////////////////////////////////
    // Parse a frame ///////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
      stmp := firstword_ch(token2);
      if stmp = '' then
      begin
        I_Warning('DEH_Parse(): State number unspecified after %s keyword'#13#10, [token1]);
        continue;
      end;

      newstate := false;
      // Retrieve state number
      if (stmp = 'NEW') or (stmp = 'NEWFRAME') or (stmp = 'NEWSTATE') then
      begin
        stmp := secondword(token2);
        state_no := atoi(stmp, -1);
        if stmp = '' then
        begin
          I_Warning('DEH_Parse(): New state number unspecified after %s keyword'#13#10, [firstword_ch(token2)]);
          continue;
        end;
        if state_no < 0 then
        begin
          I_Warning('DEH_Parse(): Wrong new state number = %s'#13#10, [stmp]);
          continue;
        end;
        state_no := state_no + deh_initialstates;
        newstate := true;
        if devparm then
          printf('DEH_Parse(): Expanding states table, new state number found: %d'#13#10, [state_no]);
        while numstates <= state_no do
          Info_GetNewState;
      end
      else
      begin
        state_no := atoi(stmp, -1);
        if state_no < 0 then
          state_no := statenames.IndexOfToken(token2);
        if state_no < 0 then
        begin
          I_Warning('DEH_Parse(): Wrong state number = %s'#13#10, [token2]);
          continue;
        end;
        while numstates <= state_no do
          Info_GetNewState;
      end;
      SC_FillStateNames;

      while true do
      begin
        if not DEH_NextLine(s, str, i) then
          break;
        if CharPos('=', str) = 0 then
        begin
          mustnextline := false; // Already got line
          break;
        end;

        splitstring_ch(str, token1, token2, '=');
        state_idx := state_tokens.IndexOf(token1);

        if state_idx = -1 then
        begin
          I_Warning('DEH_Parse(): Wrong state field = %s (State number = %d)'#13#10, [token1, state_no]);
          mustnextline := false; // Already got line
          break;
        end;

        state_val := atoi(token2, -1);

        case state_idx of
           0: states[state_no].sprite := Info_GetSpriteNumForName(token2);
           1:
            begin
              if firstword_ch(token2) = 'BRIGHT' then
                states[state_no].frame := FF_FULLBRIGHT + atoi(secondword(token2))
              else
                states[state_no].frame := state_val;
            end;
           2: states[state_no].tics := state_val;
           3:
            begin
              fw := firstword_ch(token2);
              if (fw = 'ORIGINALSTATE') or (fw = 'ORIGINALFRAME') or (fw = 'ORIGINAL') then
              begin
                state_val := atoi(secondword(token2));
                states[state_no].nextstate := statenum_t(state_val);
              end
              else if (fw = 'NEWSTATE') or (fw = 'NEWFRAME') or (fw = 'NEW') then
              begin
                state_val := atoi(secondword(token2));
                states[state_no].nextstate := statenum_t(deh_initialstates + state_val)
              end
              else if statenames.IndexOfToken(token2) >= 0 then
                states[state_no].nextstate := statenum_t(statenames.IndexOfToken(token2))
              else if newstate then
                states[state_no].nextstate := statenum_t(deh_initialstates + state_val)
              else
                states[state_no].nextstate := statenum_t(state_val);
            end;
           4:
            begin
              if state_val >= 0 then
              begin
                if state_val < dehnumactions then
                  states[state_no].action.acp1 := deh_actions[state_val].action.acp1
                else
                  I_Warning('DEH_Parse(): Wrong action number = %d in state %d (Must be in [0..%d])'#13#10, [state_val, state_no, dehnumactions]);
              end
              else
              begin
                splitstring(token2, token3, token4, [' ', '(']);
                foundaction := false;

                j := DEH_SearchActionFromHash(token3);
                if j >= 0 then
                begin
                  states[state_no].action.acp1 := deh_actions[j].action.acp1;
                  foundaction := True;
                end
                else
                begin
                  for j := 0 to dehnumactions - 1 do
                    if (token3 = deh_actions[j].name) or (token3 = 'A_' + deh_actions[j].name) then
                    begin
                      states[state_no].action.acp1 := deh_actions[j].action.acp1;
                      foundaction := True;
                      break;
                    end;
                end;

                if foundaction then
                begin
                  if states[state_no].params <> nil then
                    states[state_no].params.Free;
                  if token4 <> '' then
                    states[state_no].params := TCustomParamList.Create(SC_FixParenthesisLevel(token4));
                end
                else
                  I_Warning('DEH_Parse(): Unknown action function = "%s" in state %d'#13#10, [token3, state_no]);
              end;
            end;
           5: states[state_no].misc1 := state_val;
           6: states[state_no].misc2 := state_val;
           7: begin
                if itoa(state_val) = token2 then
                  states[state_no].flags_ex := state_val  // DelphiDoom specific (lighting, transparency, etc)
                else
                begin
                  state_setflag := -1;
                  repeat
                    splitstring(token2, token3, token4, [' ', '|', ',', '+']);
                    state_flag := state_flags_ex.IndexOf('MF_EX_' + token3);
                    if state_flag = -1 then
                      state_flag := state_flags_ex.IndexOf('MF_' + token3);
                    if state_flag = -1 then
                      state_flag := state_flags_ex.IndexOf(token3);
                    if state_flag >= 0 then
                    begin
                      if state_setflag = -1 then
                        state_setflag := 0;
                      state_flag := _SHL(1, state_flag);
                      state_setflag := state_setflag or state_flag;
                    end;
                    token2 := token4;
                  until token2 = '';
                  if state_setflag <> -1 then
                    states[state_no].flags_ex := state_setflag;

                end;
              end;
           8: Info_AddStateOwner(@states[state_no], Info_GetMobjNumForName(token2));
           9: states[state_no].tics2 := state_val;
          10: begin
                states[state_no].args[0] := state_val;  // MBF21
                states[state_no].argsdefined := states[state_no].argsdefined or ARG1_DEFINED;
              end;
          11: begin
                states[state_no].args[1] := state_val;  // MBF21
                states[state_no].argsdefined := states[state_no].argsdefined or ARG2_DEFINED;
              end;
          12: begin
                states[state_no].args[2] := state_val;  // MBF21
                states[state_no].argsdefined := states[state_no].argsdefined or ARG3_DEFINED;
              end;
          13: begin
                states[state_no].args[3] := state_val;  // MBF21
                states[state_no].argsdefined := states[state_no].argsdefined or ARG4_DEFINED;
              end;
          14: begin
                states[state_no].args[4] := state_val;  // MBF21
                states[state_no].argsdefined := states[state_no].argsdefined or ARG5_DEFINED;
              end;
          15: begin
                states[state_no].args[5] := state_val;  // MBF21
                states[state_no].argsdefined := states[state_no].argsdefined or ARG6_DEFINED;
              end;
          16: begin
                states[state_no].args[6] := state_val;  // MBF21
                states[state_no].argsdefined := states[state_no].argsdefined or ARG7_DEFINED;
              end;
          17: begin
                states[state_no].args[7] := state_val;  // MBF21
                states[state_no].argsdefined := states[state_no].argsdefined or ARG8_DEFINED;
              end;
          18: begin // MBF21
                if itoa(state_val) = token2 then
                  states[state_no].mbf21bits := state_val
                else
                begin
                  state_setflag := -1;
                  repeat
                    splitstring(token2, token3, token4, [' ', '|', ',', '+']);
                    state_flag := state_flags_mbf21.IndexOf(token3);
                    if state_flag >= 0 then
                    begin
                      if state_setflag = -1 then
                        state_setflag := 0;
                      state_flag := _SHL(1, state_flag);
                      state_setflag := state_setflag or state_flag;
                    end;
                    token2 := token4;
                  until token2 = '';
                  if state_setflag <> -1 then
                    states[state_no].mbf21bits := state_setflag;

                end;
              end;
        end;
      end;

      // MBF21
      if states[state_no].mbf21bits <> 0 then
        ismbf21 := true;
    end

    ////////////////////////////////////////////////////////////////////////////
    else if token1 = 'TEXT' then
    begin
    ////////////////////////////////////////////////////////////////////////////
    // Parse a text ////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
      stmp := token2;
      splitstring_ch(stmp, token1, token2);

      len1 := atoi(token1, -1);
      if len1 <= 0 then
        continue;

      len2 := atoi(token2, -1);
      if len2 <= 0 then
        continue;

      stmp := '';
      while true do
      begin
        if not DEH_NextLine(s, str, i, false) then
          break;

        stmp := stmp + str;
        if length(stmp) >= len1 + len2 then
          break;

        stmp := stmp + #10;
        if length(stmp) >= len1 + len2 then
          break;
      end;
      len2 := Length(stmp) - len1;
      if len2 <= 0 then
        continue;

      settext := Copy(stmp, 1, len1);
      token1 := DEH_StringValue(settext);
      token2 := Copy(stmp, len1 + 1, len2);

      foundtext := false;
      for j := 0 to deh_strings.numstrings - 1 do
        if DEH_StringValue(deh_strings._array[j].pstr^) = token1 then
        begin
          foundtext := true;
          deh_strings._array[j].pstr^ := DEH_CStringToString(token2);
          break;
        end;

      if not foundtext then
      begin
        for j := 1 to nummusic - 1 do // First music is dummy
        begin
          stmp := strupper(S_music[j].name);
          if stmp = token1 then
          begin
            foundtext := true;
            S_music[j].name := token2;
            break;
          end
          else if 'D_' + stmp = token1 then // JVAL Allow D_ prefix for checking
          begin
            foundtext := true;
            S_music[j].name := token2;
            break;
          end
          else if strupper(S_music[j].mapname) = token1 then
          begin
            foundtext := true;
            S_music[j].name := token2;
            break;
          end
        end;

        if not foundtext and (len1 = 4) and (len2 = 4) then
        begin
          for j := 0 to numsprites - 1 do
          begin
            stmp := Chr(sprnames[j] and $FF) +
                    Chr(sprnames[j] shr 8 and $FF) +
                    Chr(sprnames[j] shr 16 and $FF) +
                    Chr(sprnames[j] shr 24 and $FF);
            if stmp = token1 then
            begin
              sprnames[j] := Ord(toupper(token2[1])) +
                             Ord(toupper(token2[2])) shl 8 +
                             Ord(toupper(token2[3])) shl 16 +
                             Ord(toupper(token2[4])) shl 24;
              foundtext := true;
              break;
            end;
          end;
        end;
      end;

      if not foundtext then
        I_Warning('DEH_Parse(): Can not find setable text "%s" - should be changed to "%s"'#13#10, [settext, token2]);

    end

    ////////////////////////////////////////////////////////////////////////////
    else if token1 = 'POINTER' then
    begin
    ////////////////////////////////////////////////////////////////////////////
    // Parse a code pointer ////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
      stmp := secondword(token2, '(');
      if length(stmp) < 7 then
        continue;
      if stmp[length(stmp)] <> ')' then
        continue;

      SetLength(stmp, length(stmp) - 1);
      splitstring_ch(stmp, token1, token2);
      if token1 <> 'FRAME' then
        continue;

      state_no := atoi(token2, -1);
      if state_no < 0 then
        continue;

      if not DEH_NextLine(s, str, i) then
        break;

      if CharPos('=', str) = 0 then
      begin
        mustnextline := false; // Already got line
        continue;
      end;

      splitstring_ch(str, token1, token2, '=');
      if token1 <> 'CODEP FRAME' then
      begin
        mustnextline := false; // Already got line
        continue;
      end;

      state_val := atoi(token2, -1);
      if (state_val >= 0) and (state_val < numstates) then
      begin
        // JVAL: 20201203 - Preserve initial actions
        if state_val < deh_initialstates then
          states[state_no].action.acp1 := code_ptrs[state_val].acp1
        else
          states[state_no].action.acp1 := states[state_val].action.acp1;
      end
      else
        I_Warning('DEH_Parse(): Invalid state number "%s" while parsing CODEP FRAME'#13#10, [token2]);
    end

    ////////////////////////////////////////////////////////////////////////////
    else if token1 = 'SOUND' then
    begin
    ////////////////////////////////////////////////////////////////////////////
    // Parse a sound ///////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
      stmp := firstword_ch(token2);
      if stmp = '' then
        continue;

      if not (stmp[1] in  ['1'..'9', '0']) then
        sound_no := S_GetSoundNumForName(stmp)
      else
      begin
        // Retrieve think number
        sound_no := atoi(stmp, -1);
        if (sound_no < 0) or (sound_no >= numsfx) then
        begin
          I_Warning('DEH_Parse(): Wrong sound number = %s'#13#10, [stmp]);
          if sound_no >= numsfx then
            I_Warning('DEH_Parse(): New sounds can only specified in things definition!'#13#10);
          continue;
        end;
      end;

      while true do
      begin
        if not DEH_NextLine(s, str, i) then
          break;

        if CharPos('=', str) = 0 then
        begin
          mustnextline := false; // Already got line
          break;
        end;

      // Retrieve current think field index
        splitstring_ch(str, token1, token2, '=');
        sound_idx := sound_tokens.IndexOf(token1);

        if sound_idx = -1 then
        begin
          I_Warning('DEH_Parse(): Wrong sound field = %s (Sound number = %d)'#13#10, [token1, sound_no]);
          mustnextline := false; // Already got line
          break;
        end;

        sound_val := atoi(token2);

        case sound_idx of
           0: S_sfx[sound_no].singularity := sound_val <> 0;
           1: S_sfx[sound_no].priority := sound_val;
           2: S_sfx[sound_no].name := token2;
        end;
      end;
    end

    ////////////////////////////////////////////////////////////////////////////
    else if token1 = 'AMMO' then
    begin
    ////////////////////////////////////////////////////////////////////////////
    // Parse ammo //////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
      stmp := firstword_ch(token2);
      if stmp = '' then
        continue;

      ammo_no := atoi(stmp, -1);

      if (ammo_no < 0) or (ammo_no >= Ord(NUMAMMO)) then
      begin
        I_Warning('DEH_Parse(): Wrong ammo number = %s'#13#10, [stmp]);
        continue;
      end;

      while true do
      begin
        if not DEH_NextLine(s, str, i) then
          break;
        if CharPos('=', str) = 0 then
        begin
          mustnextline := false; // Already got line
          break;
        end;

        splitstring_ch(str, token1, token2, '=');
        ammo_idx := ammo_tokens.IndexOf(token1);

        if ammo_idx = -1 then
        begin
          I_Warning('DEH_Parse(): Wrong ammo field = %s (Ammo number = %d)'#13#10, [token1, ammo_no]);
          mustnextline := false; // Already got line
          break;
        end;

        ammo_val := atoi(token2);

        case ammo_idx of
           0: maxammo[ammo_no] := ammo_val;
        end;

      end;
    end

    ////////////////////////////////////////////////////////////////////////////
    else if token1 = 'WEAPON' then
    begin
    ////////////////////////////////////////////////////////////////////////////
    // Parse weapon ////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
      stmp := firstword_ch(token2);
      if stmp = '' then
        continue;

      // Retrieve think number
      weapon_no := atoi(stmp, -1);
      if weapon_no < 0 then
        weapon_no := DEH_WeaponType(stmp);
      if (weapon_no < 0) or (weapon_no >= Ord(NUMWEAPONS)) then
      begin
        I_Warning('DEH_Parse(): Wrong weapon number = %s'#13#10, [stmp]);
        continue;
      end;

      weapon_level := 1;

      while true do
      begin
        if not DEH_NextLine(s, str, i) then
          break;

        if CharPos('=', str) = 0 then
        begin
          mustnextline := false; // Already got line
          break;
        end;

      // Retrieve current think field index
        splitstring_ch(str, token1, token2, '=');
        weapon_idx := weapon_tokens.IndexOf(token1);

        if weapon_idx = -1 then
        begin
          I_Warning('DEH_Parse(): Wrong weapon field = %s (Weapon number = %d)'#13#10, [token1, weapon_no]);
          mustnextline := false; // Already got line
          break;
        end;

        weapon_val := atoi(token2, -1);

        if weapon_idx = 0 then
        begin
          if weapon_val < 0 then
            weapon_val := DEH_AmmoType(token2);
          if weapon_val < 0 then
            I_Warning('DEH_Parse(): After %s keyword found invalid ammo %s (Weapon number = %d)'#13#10, [weapon_tokens.Strings[weapon_idx], token2, weapon_no]);
        end
        else if weapon_idx = 7 then
        begin
          if weapon_val in [1, 2] then
            weapon_level := weapon_val
          else
            I_Warning('DEH_Parse(): After %s keyword found invalid level %s (Weapon number = %d)'#13#10, [weapon_tokens.Strings[weapon_idx], token2, weapon_no]);
          continue;
        end
        else if weapon_val < 0 then
        begin
          if weapon_idx in [1, 2, 3, 4, 5, 6] then
          begin
            stmp := firstword_ch(token2);
            if (stmp = 'NEWFRAME') or (stmp = 'NEWSTATE') then  // JVAL: a new defined state
            begin
              weapon_val := atoi(secondword(token2), -1);
              if weapon_val < 0 then
              begin
                I_Warning('DEH_Parse(): After %s keyword found invalid numeric %s (Weapon number = %d)'#13#10, [stmp, secondword(token2), weapon_no]);
                continue;
              end;
              weapon_val := weapon_val + deh_initialstates;
            end
            else if (stmp = 'ORIGINALFRAME') or (stmp = 'ORIGINALSTATE') then  // JVAL: an original defined state
            begin
              weapon_val := atoi(secondword(token2), -1);
              if weapon_val < 0 then
              begin
                I_Warning('DEH_Parse(): After %s keyword found invalid numeric %s (Weapon number = %d)'#13#10, [stmp, secondword(token2), weapon_no]);
                continue;
              end;
            end;
          end
          else if weapon_idx = 8 then
          begin
            I_Warning('DEH_Parse(): Positive number expected after keyword (%s) - (%s) (Weapon number = %d)'#13#10, [token1, token2, weapon_no]);
            continue;
          end;
        end;

        if weapon_level = 1 then
          weaponinfo_p := @wpnlev1info
        else
          weaponinfo_p := @wpnlev2info;

        case weapon_idx of
           0: weaponinfo_p[weapon_no].ammo := ammotype_t(weapon_val);
           1: weaponinfo_p[weapon_no].upstate := weapon_val;
           2: weaponinfo_p[weapon_no].downstate := weapon_val;
           3: weaponinfo_p[weapon_no].readystate := weapon_val;
           4: weaponinfo_p[weapon_no].atkstate := weapon_val;
           6: weaponinfo_p[weapon_no].holdatkstate := weapon_val;
           5: weaponinfo_p[weapon_no].flashstate := weapon_val;
           8: begin
                if weapon_level = 1 then
                  WeaponAmmoUsePL1[weapon_no] := weapon_val
                else
                  WeaponAmmoUsePL2[weapon_no] := weapon_val;
                weaponinfo_p[weapon_no].intflags := weaponinfo_p[weapon_no].intflags or WIF_ENABLEAPS;
              end;
           9: begin // MBF21
                if itoa(weapon_val) = token2 then
                  weaponinfo_p[weapon_no].mbf21bits := weapon_val
                else
                begin
                  weapon_val := -1;
                  repeat
                    splitstring(token2, token3, token4, [' ', '|', ',', '+']);
                    weapon_flag := weapon_flags_mbf21.IndexOf(token3);
                    if weapon_flag >= 0 then
                    begin
                      if weapon_val = -1 then
                        weapon_val := 0;
                      weapon_flag := _SHL(1, weapon_flag);
                      weapon_val := weapon_val or weapon_flag;
                    end;
                    token2 := token4;
                  until token2 = '';
                  if weapon_val <> -1 then
                    weaponinfo_p[weapon_no].mbf21bits := weapon_val;

                end;
              end;

        end;
      end;

    end

    ////////////////////////////////////////////////////////////////////////////
    else if (token1 = '[SPRITES]') or (token1 = 'SPRITES') then
    begin
    ////////////////////////////////////////////////////////////////////////////
    // Parse sprite ////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
      while true do
      begin
        if not DEH_NextLine(s, str, i) then
          break;

        if CharPos('=', str) = 0 then
        begin
          mustnextline := false; // Already got line
          break;
        end;

        splitstring_ch(str, token1, token2, '=');

        sprite_idx := -1;
        sprite_val := atoi(token1, -1);
        if IsIntegerInRange(sprite_val, 0, Ord(DO_NUMSPRITES) - 1) then
        begin
          sprite_idx := sprite_val;
          token1 := DO_sprnames[sprite_idx];
        end
        else if (numsprites >= Ord(DO_NUMSPRITES)) and IsIntegerInRange(sprite_val, Ord(DO_NUMSPRITES), numsprites) then
        begin
          sprite_idx := sprite_val;
          token1 := Chr(sprnames[sprite_idx] and $FF) +
                    Chr(sprnames[sprite_idx] shr 8 and $FF) +
                    Chr(sprnames[sprite_idx] shr 16 and $FF) +
                    Chr(sprnames[sprite_idx] shr 24 and $FF);
        end;

        if length(token1) <> 4 then
        begin
          I_Warning('DEH_Parse(): Sprite name with %d characters = %s'#13#10, [length(token1), token1]);
          Continue;
        end;

        if length(token2) <> 4 then
        begin
          I_Warning('DEH_Parse(): Sprite name with %d characters = %s'#13#10, [length(token2), token2]);
          Continue;
        end;

        strupperproc(token1);
        strupperproc(token2);

        // JVAL: Check the original sprite names (https://eternity.youfailit.net/wiki/DeHackEd_/_BEX_Reference/Eternity_Extension:_SPRITES_Block)
        if sprite_idx < 0 then
        begin
          for j := 0 to Ord(DO_NUMSPRITES) - 1 do
            if DO_sprnames[j] = token1 then
            begin
              sprite_idx := j;
              break;
            end;
          if sprite_idx < 0 then
            for j := Ord(DO_NUMSPRITES) to numsprites - 1 do
              if Chr(sprnames[j] and $FF) + Chr(sprnames[j] shr 8 and $FF) +
                Chr(sprnames[j] shr 16 and $FF) + Chr(sprnames[j] shr 24 and $FF) = token1 then
              begin
                sprite_idx := j;
                break;
              end;
        end;

        if sprite_idx < 0 then
        begin
          I_Warning('DEH_Parse(): Can not find sprite = %s'#13#10, [token1]);
          Continue;
        end;

        sprnames[sprite_idx] :=
          Ord(token2[1]) +
          Ord(token2[2]) shl 8 +
          Ord(token2[3]) shl 16 +
          Ord(token2[4]) shl 24;
      end;
    end

    ////////////////////////////////////////////////////////////////////////////
    else if token1 = 'CHEAT' then
    begin
    ////////////////////////////////////////////////////////////////////////////
    // Parse cheat /////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
    end

    ////////////////////////////////////////////////////////////////////////////
    else if token1 = 'MISC' then
    begin
    ////////////////////////////////////////////////////////////////////////////
    // Parse misc //////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
      while true do
      begin
        if not DEH_NextLine(s, str, i) then
          break;

        if CharPos('=', str) = 0 then
        begin
          mustnextline := false; // Already got line
          break;
        end;

      // Retrieve current think field index
        splitstring_ch(str, token1, token2, '=');

        misc_idx := misc_tokens.IndexOf(token1);

        if misc_idx = -1 then
        begin
          I_Warning('DEH_Parse(): Wrong misc field = %s'#13#10, [token1]);
          mustnextline := false; // Already got line
          break;
        end;

        misc_val := atoi(token2, -1);
        if misc_val < 0 then // Allow Zero!!
        begin
          I_Warning('DEH_Parse(): Wrong misc value %s, field = %s'#13#10, [token2, token1]);
          mustnextline := false; // Already got line
          break;
        end;

        case misc_idx of
          0: p_maxhealth := misc_val;
          1: p_soulspherehealth := misc_val;
          2: p_megaspherehealth := misc_val;
          3: p_medikithealth := misc_val;
          4: p_stimpackhealth := misc_val;
          5: p_bonushealth := misc_val;
          6: p_maxarmor := misc_val;
          7: p_greenarmorclass := misc_val;
          8: p_bluearmorclass := misc_val;
          9: p_maxchickenhealth := misc_val;
         10: p_maxartifacts := misc_val;
        end;
      end;

    end

    ////////////////////////////////////////////////////////////////////////////
    else if (token1 = '[PARS]') or (token1 = 'PARS') then // BEX
    begin
    ////////////////////////////////////////////////////////////////////////////
    // Parse pars //////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
      while true do
      begin
        if not DEH_NextLine(s, str, i) then
          break;

        splitstring_ch(str, token1, stmp);
        if token1 <> 'PAR' then
        begin
          mustnextline := false; // Already got line
          break;
        end;

        splitstring_ch(stmp, token2, token3);

        if token3 = '' then
          continue;

        stmp := token3;
        splitstring_ch(stmp, token3, token4);

        if token4 <> '' then
        begin // Doom1, Ultimate Doom
          par_episode := atoi(token2, -1);
          par_map := atoi(token3, -1);
          par_time := atoi(token4, -1);
          if par_episode in [1, 2, 3, 4, 5] then
            if par_map in [1..9] then
              if par_time >= 0 then // JVAL =0 ????
                pars[par_episode, par_map] := par_time;
        end
        else
        begin
          I_Warning('DEH_Parse(): Wrong misc value %s, field = %s'#13#10, [token2, token1]);
        end;

      end;
    end

    ////////////////////////////////////////////////////////////////////////////
    else if (token1 = '[STRINGS]') or (token1 = 'STRINGS') then // BEX
    begin
    ////////////////////////////////////////////////////////////////////////////
    // Parse strings ///////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
      while true do
      begin
        if not DEH_NextLine(s, str, i) then
          break;
        if CharPos('=', str) = 0 then
        begin
          mustnextline := false; // Already got line
          break;
        end;

        while str[length(str)] = '\' do  // Multiple lines devide by '\' char
        begin
          str[Length(str)] := #10; // Replace '\' with new line indicator
          if not DEH_NextLine(s, stmp, i) then
            break;
          str := str + stmp;
        end;

        splitstring_ch(str, token1, token2, '=');

        for j := 0 to deh_strings.numstrings - 1 do
          if deh_strings._array[j].name = token1 then
          begin
            deh_strings._array[j].pstr^ := DEH_CStringToString(token2);
            break;
          end;
      end;
    end

    ////////////////////////////////////////////////////////////////////////////
    else if (token1 = '[CODEPTR]') or (token1 = 'CODEPTR') then // BEX
    begin
    ////////////////////////////////////////////////////////////////////////////
    // Parse code pointer //////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
      while true do
      begin
        if not DEH_NextLine(s, str, i) then
          break;
        if CharPos('=', str) = 0 then
        begin
          mustnextline := false; // Already got line
          break;
        end;

        splitstring_ch(str, stmp, token3, '=');
        splitstring_ch(stmp, token1, token2);

        if token1 <> 'FRAME' then
          continue;

        state_no := atoi(token2, -1);
        if state_no < 0 then
          continue;

        state_val := atoi(token3, -1);

        if state_val >= 0 then
        begin
          if state_val < dehnumactions then
            states[state_no].action.acp1 := deh_actions[state_val].action.acp1;
        end
        else
        begin
          splitstring(token3, token4, token5, [' ', '(']);

          j := DEH_SearchActionFromHash(token3);
          if j >= 0 then
            states[state_no].action.acp1 := deh_actions[j].action.acp1
          else
          begin
            for j := 0 to dehnumactions - 1 do
              if (token4 = deh_actions[j].name) or (token4 = 'A_' + deh_actions[j].name) then
              begin
                states[state_no].action.acp1 := deh_actions[j].action.acp1;
                break;
              end;
          end;

          if states[state_no].params <> nil then
            states[state_no].params.Free;
          if token5 <> '' then
            states[state_no].params := TCustomParamList.Create(SC_FixParenthesisLevel(token5));
        end;
      end;
    end

    ////////////////////////////////////////////////////////////////////////////
    else if (token1 = '[MUSIC]') or (token1 = 'MUSIC') then // BEX
    begin
    ////////////////////////////////////////////////////////////////////////////
    // Parse music /////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
      while true do
      begin
        if not DEH_NextLine(s, str, i) then
          break;
        if CharPos('=', str) = 0 then
        begin
          mustnextline := false; // Already got line
          break;
        end;

        splitstring_ch(str, token1, token2, '=');
        token2 := firstword_ch(token2);

        music_idx := atoi(token1, -1);
        if (music_idx >= 1) and (music_idx < nummusic) then
          S_music[music_idx].name := token2
        else
        begin
          for j := 1 to nummusic - 1 do // First music is dummy
          begin
            stmp := strupper(S_music[j].name);
            if stmp = token1 then
            begin
              S_music[j].name := token2;
              break;
            end
            else if 'MUS_' + stmp = token1 then // JVAL Allow MUS_ prefix for checking
            begin
              S_music[j].name := token2;
              break;
            end
            else if strupper(S_music[j].mapname) = token1 then
            begin
              S_music[j].name := token2;
              break;
            end
          end;
        end
      end;
    end

    ////////////////////////////////////////////////////////////////////////////
    else if (token1 = '[SOUND]') or (token1 = '[SOUNDS]') then // BEX
    begin
    ////////////////////////////////////////////////////////////////////////////
    // Parse music /////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
      while true do
      begin
        if not DEH_NextLine(s, str, i) then
          break;
        if CharPos('=', str) = 0 then
        begin
          mustnextline := false; // Already got line
          break;
        end;

        splitstring_ch(str, token1, token2, '=');

        sound_idx := atoi(token1, -1);
        if (sound_idx >= 1) and (sound_idx < numsfx) then
          S_sfx[sound_idx].name := token2
        else
        begin
          for j := 1 to numsfx - 1 do // First sfx is dummy
          begin
            stmp := strupper(S_sfx[j].name);
            if stmp = token1 then
            begin
              S_sfx[j].name := token2;
              break;
            end
            else if 'DS' + stmp = token1 then // JVAL, check the 'DS' prefix
            begin
              S_sfx[j].name := token2;
              break;
            end;
          end;
        end;

      end;
    end

    ////////////////////////////////////////////////////////////////////////////
    else if (token1 = 'SUBMITNEWSTATES') or (token1 = 'SUBMITNEWFRAMES') then // DelphiDoom specific
    begin
    // JVAL: 20201203 - Preserve initial actions
      realloc(pointer(code_ptrs), deh_initialstates * SizeOf(actionf_t), numstates * SizeOf(actionf_t));
      for j := 0 to numstates - 1 do
        code_ptrs[j] := states[j].action;

    ////////////////////////////////////////////////////////////////////////////
    // Resetting internal states counter ///////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
      deh_initialstates := numstates;

    end;

  end;

  // JVAL: 20201203 - Preserve initial actions
  memfree(pointer(code_ptrs), numstates * SizeOf(actionf_t));
end;

//==============================================================================
//
// DEH_CurrentSettings
//
//==============================================================================
function DEH_CurrentSettings: TDStringList;
var
  i, j, k: integer;
  str: string;
  cmdln: string;
  weaponinfo_p: Pweaponinfo_tArray;
begin
  if not deh_initialized then
    DEH_Init;

  result := TDStringList.Create;
  result.Add('Patch File for DeHackEd');
  result.Add('# Created with %s, %s', [D_Version, D_VersionBuilt]);

  cmdln := fname(myargv[0]);
  for i := 1 to myargc - 1 do
    cmdln := cmdln + ' ' + myargv[i];
  result.Add('# Command line options:');
  result.Add('# %s'#13#10, [cmdln]);

  result.Add('');

  result.Add('# Note: Use the ''#'' or ''//'' (DelphiDoom specific) sign to start comment lines.');
  result.Add('');

  result.Add('# General version information');
  result.Add('Doom version = 21');
  result.Add('Patch format = 6');
  result.Add('');

  //////////////////////////////////////////////////////////////////////////////
  // Add things (mobjinfo)
  //////////////////////////////////////////////////////////////////////////////
  result.Add('');
  result.Add('# Things');
  result.Add('');
  for i := 0 to nummobjtypes - 1 do
  begin
    result.Add('Thing %d', [i + 1]);

    result.Add('%s = %s', [capitalizedstring(mobj_tokens[32]), Info_GetMobjName(i)]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[31]), mobjinfo[i].inheritsfrom]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[0]), mobjinfo[i].doomednum]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[1]), mobjinfo[i].spawnstate]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[2]), mobjinfo[i].spawnhealth]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[3]), mobjinfo[i].seestate]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[4]), mobjinfo[i].seesound]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[5]), mobjinfo[i].reactiontime]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[6]), mobjinfo[i].attacksound]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[7]), mobjinfo[i].painstate]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[8]), mobjinfo[i].painchance]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[9]), mobjinfo[i].painsound]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[10]), mobjinfo[i].meleestate]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[11]), mobjinfo[i].missilestate]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[12]), mobjinfo[i].deathstate]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[13]), mobjinfo[i].xdeathstate]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[14]), mobjinfo[i].deathsound]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[15]), mobjinfo[i].speed]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[16]), mobjinfo[i].radius]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[17]), mobjinfo[i].height]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[18]), mobjinfo[i].mass]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[19]), mobjinfo[i].damage]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[20]), mobjinfo[i].activesound]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[23]), mobjinfo[i].raisestate]);

    str := '';
    for j := 0 to mobj_flags.Count - 1 do
    begin
      if mobjinfo[i].flags and _SHL(1, j) <> 0 then
      begin
        if str <> '' then
          str := str + ', ';
        str := str + mobj_flags[j];
      end;
    end;
    if str = '' then
      result.Add('%s = 0', [capitalizedstring(mobj_tokens[21])])
    else
      result.Add('%s = %s', [capitalizedstring(mobj_tokens[21]), str]);

    str := '';
    for j := 0 to mobj_flags2.Count - 1 do
    begin
      if mobjinfo[i].flags2 and _SHL(1, j) <> 0 then
      begin
        if str <> '' then
          str := str + ', ';
        str := str + mobj_flags2[j];
      end;
    end;
    if str = '' then
      result.Add('%s = 0', [capitalizedstring(mobj_tokens[22])])
    else
      result.Add('%s = %s', [capitalizedstring(mobj_tokens[22]), str]);

    if i = 0 then
    begin
      result.Add('');
      result.Add('# The following fields are DelphiHeretic specific enhancements');
    end;

    str := '';
    for j := 0 to mobj_flags_ex.Count - 1 do
    begin
      if mobjinfo[i].flags_ex and _SHL(1, j) <> 0 then
      begin
        if str <> '' then
          str := str + ', ';
        str := str + mobj_flags_ex[j];
      end;
    end;
    if str = '' then
      result.Add('%s = 0', [capitalizedstring(mobj_tokens[24])])
    else
      result.Add('%s = %s', [capitalizedstring(mobj_tokens[24]), str]);

    str := '';
    for j := 0 to mobj_flags2_ex.Count - 1 do
    begin
      if mobjinfo[i].flags2_ex and _SHL(1, j) <> 0 then
      begin
        if str <> '' then
          str := str + ', ';
        str := str + mobj_flags2_ex[j];
      end;
    end;
    if str = '' then
      result.Add('%s = 0', [capitalizedstring(mobj_tokens[39])])
    else
      result.Add('%s = %s', [capitalizedstring(mobj_tokens[39]), str]);

    result.Add('%s = %d', [capitalizedstring(mobj_tokens[25]), mobjinfo[i].customsound1]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[26]), mobjinfo[i].customsound2]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[27]), mobjinfo[i].customsound3]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[28]), mobjinfo[i].explosiondamage]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[29]), mobjinfo[i].explosionradius]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[30]), mobjinfo[i].meleedamage]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[33]), mobjinfo[i].meleesound]);
    result.Add('%s = %s', [capitalizedstring(mobj_tokens[34]), renderstyle_tokens[Ord(mobjinfo[i].renderstyle)]]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[35]), mobjinfo[i].alpha]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[36]), mobjinfo[i].dropitem]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[37]), mobjinfo[i].missiletype]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[38]), mobjinfo[i].healstate]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[40]), mobjinfo[i].crashstate]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[41]), mobjinfo[i].vspeed]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[42]), mobjinfo[i].pushfactor]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[43]), mobjinfo[i].scale]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[44]), mobjinfo[i].gravity]);

    str := '';
    for j := 0 to mobj_flags3_ex.Count - 1 do
    begin
      if mobjinfo[i].flags3_ex and _SHL(1, j) <> 0 then
      begin
        if str <> '' then
          str := str + ', ';
        str := str + mobj_flags3_ex[j];
      end;
    end;
    if str = '' then
      result.Add('%s = 0', [capitalizedstring(mobj_tokens[45])])
    else
      result.Add('%s = %s', [capitalizedstring(mobj_tokens[45]), str]);

    str := '';
    for j := 0 to mobj_flags4_ex.Count - 1 do
    begin
      if mobjinfo[i].flags4_ex and _SHL(1, j) <> 0 then
      begin
        if str <> '' then
          str := str + ', ';
        str := str + mobj_flags4_ex[j];
      end;
    end;
    if str = '' then
      result.Add('%s = 0', [capitalizedstring(mobj_tokens[46])])
    else
      result.Add('%s = %s', [capitalizedstring(mobj_tokens[46]), str]);

    result.Add('%s = %d', [capitalizedstring(mobj_tokens[47]), mobjinfo[i].minmissilechance]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[48]), mobjinfo[i].floatspeed]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[49]), mobjinfo[i].normalspeed]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[50]), mobjinfo[i].fastspeed]);
    result.Add('%s = "%s"', [capitalizedstring(mobj_tokens[51]), mobjinfo[i].obituary]);
    result.Add('%s = "%s"', [capitalizedstring(mobj_tokens[52]), mobjinfo[i].hitobituary]);
    result.Add('%s = %s', [capitalizedstring(mobj_tokens[53]), GENDERINFO[Ord(mobjinfo[i].gender)].name]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[54]), mobjinfo[i].meleerange]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[55]), mobjinfo[i].maxstepheight]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[56]), mobjinfo[i].maxdropoffheight]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[57]), mobjinfo[i].gibhealth]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[58]), mobjinfo[i].maxtargetrange]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[59]), mobjinfo[i].WeaveIndexXY]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[60]), mobjinfo[i].WeaveIndexZ]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[61]), mobjinfo[i].friction]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[62]), mobjinfo[i].spriteDX]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[63]), mobjinfo[i].spriteDY]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[64]), mobjinfo[i].interactstate]);

    str := '';
    for j := 0 to mobj_flags5_ex.Count - 1 do
    begin
      if mobjinfo[i].flags5_ex and _SHL(1, j) <> 0 then
      begin
        if str <> '' then
          str := str + ', ';
        str := str + mobj_flags5_ex[j];
      end;
    end;
    if str = '' then
      result.Add('%s = 0', [capitalizedstring(mobj_tokens[65])])
    else
      result.Add('%s = %s', [capitalizedstring(mobj_tokens[65]), str]);

    str := '';
    for j := 0 to mobj_flags6_ex.Count - 1 do
    begin
      if mobjinfo[i].flags6_ex and _SHL(1, j) <> 0 then
      begin
        if str <> '' then
          str := str + ', ';
        str := str + mobj_flags6_ex[j];
      end;
    end;
    if str = '' then
      result.Add('%s = 0', [capitalizedstring(mobj_tokens[66])])
    else
      result.Add('%s = %s', [capitalizedstring(mobj_tokens[66]), str]);

    // MBF21
    // JVAL: 20220105 - Ignore mbf21bits since they are merged into flagsXX
    result.Add('%s = %s', [capitalizedstring(mobj_tokens[68]), Info_InfightingGroupToString(mobjinfo[i].infighting_group)]);
    result.Add('%s = %s', [capitalizedstring(mobj_tokens[60]), Info_ProjectileGroupToString(mobjinfo[i].projectile_group)]);
    result.Add('%s = %s', [capitalizedstring(mobj_tokens[70]), Info_SplashGroupToString(mobjinfo[i].splash_group)]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[71]), mobjinfo[i].ripsound]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[72]), mobjinfo[i].crushstate]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[75]), mobjinfo[i].bloodcolor]);
    result.Add('%s = "%s"', [capitalizedstring(mobj_tokens[76]), mobjinfo[i].translationname]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[78]), mobjinfo[i].missileheight]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[79]), mobjinfo[i].meleethreshold]);

    result.Add('');
  end;

  result.Add('');
  result.Add('# States');
  result.Add('');
  //////////////////////////////////////////////////////////////////////////////
  // Add frames (states)
  //////////////////////////////////////////////////////////////////////////////
  for i := 1 to numstates - 1 do // JVAL skip state 0 -> S_NULL
  begin
    if i < statenames.Count then
      result.Add('# ' + statenames.Strings[i]);
    result.Add('Frame %d', [i]);
    result.Add('%s = %d', [capitalizedstring(state_tokens[0]), Ord(states[i].sprite)]);
    result.Add('%s = %d', [capitalizedstring(state_tokens[1]), states[i].frame]);
    result.Add('%s = %d', [capitalizedstring(state_tokens[2]), states[i].tics]);
    result.Add('%s = %d', [capitalizedstring(state_tokens[9]), states[i].tics2]);
    result.Add('%s = %d', [capitalizedstring(state_tokens[3]), Ord(states[i].nextstate)]);

    str := '';
    for j := 0 to dehnumactions - 1 do
      if @states[i].action.acp1 = @deh_actions[j].action.acp1 then
      begin
        str := deh_actions[j].name;
        break;
      end;
    if str <> '' then
    begin
      if states[i].params <> nil then
        str := str + ' ' + states[i].params.Declaration;  // Add the parameter list
      result.Add('%s = %s', [capitalizedstring(state_tokens[4]), str]);
    end;

    result.Add('%s = %d', [capitalizedstring(state_tokens[5]), states[i].misc1]);
    result.Add('%s = %d', [capitalizedstring(state_tokens[6]), states[i].misc2]);

    str := '';
    for j := 0 to state_flags_ex.Count - 1 do
    begin
      if states[i].flags_ex and _SHL(1, j) <> 0 then
      begin
        if str <> '' then
          str := str + ', ';
        str := str + state_flags_ex[j];
      end;
    end;

    if i = 1 then
      result.Add('# Flags_ex is DelphiHeretic specific and declares transparency and light effects');

    if str = '' then
      result.Add('%s = 0', [capitalizedstring(state_tokens[7])])
    else
      result.Add('%s = %s', [capitalizedstring(state_tokens[7]), str]);

    // MBF21
    str := '';
    for j := 0 to state_flags_mbf21.Count - 1 do
    begin
      if states[i].mbf21bits and _SHL(1, j) <> 0 then
      begin
        if str <> '' then
          str := str + ', ';
        str := str + state_flags_mbf21[j];
      end;
    end;
    if str = '' then
      result.Add('%s = 0', [capitalizedstring(state_tokens[18])])
    else
      result.Add('%s = %s', [capitalizedstring(state_tokens[18]), str]);

    result.Add('');
  end;

  //////////////////////////////////////////////////////////////////////////////
  // Add Weapons
  //////////////////////////////////////////////////////////////////////////////
  result.Add('');
  result.Add('# Weapons');
  result.Add('');

  for j := 1 to 2 do
    for i := 0 to Ord(NUMWEAPONS) - 1 do
    begin
      result.Add('Weapon %s', [weapontype_tokens.Strings[i]]);

      result.Add('%s = %d', [capitalizedstring(weapon_tokens[7]), j]);
      if j = 1 then
        weaponinfo_p := @wpnlev1info
      else
        weaponinfo_p := @wpnlev2info;

      result.Add('%s = %s', [capitalizedstring(weapon_tokens[0]), ammotype_tokens.Strings[Ord(weaponinfo_p[i].ammo)]]);
      result.Add('%s = %d', [capitalizedstring(weapon_tokens[1]), weaponinfo_p[i].upstate]);
      result.Add('%s = %d', [capitalizedstring(weapon_tokens[2]), weaponinfo_p[i].downstate]);
      result.Add('%s = %d', [capitalizedstring(weapon_tokens[3]), weaponinfo_p[i].readystate]);
      result.Add('%s = %d', [capitalizedstring(weapon_tokens[4]), weaponinfo_p[i].atkstate]);
      result.Add('%s = %d', [capitalizedstring(weapon_tokens[6]), weaponinfo_p[i].holdatkstate]);
      result.Add('%s = %d', [capitalizedstring(weapon_tokens[5]), weaponinfo_p[i].flashstate]);

      if j = 1 then
        result.Add('%s = %d', [capitalizedstring(weapon_tokens[8]), WeaponAmmoUsePL1[i]])
      else
        result.Add('%s = %d', [capitalizedstring(weapon_tokens[8]), WeaponAmmoUsePL2[i]]);

      // MBF
      str := '';
      for k := 0 to weapon_flags_mbf21.Count - 1 do
      begin
        if weaponinfo_p[i].mbf21bits and _SHL(1, j) <> 0 then
        begin
          if str <> '' then
            str := str + ', ';
          str := str + weapon_flags_mbf21[j];
        end;
      end;

      // MBF
      if str = '' then
        result.Add('%s = 0', [capitalizedstring(weapon_tokens[9])])
      else
        result.Add('%s = %s', [capitalizedstring(weapon_tokens[9]), str]);

      result.Add('');
    end;

  //////////////////////////////////////////////////////////////////////////////
  // Add Ammo
  //////////////////////////////////////////////////////////////////////////////
  result.Add('');
  result.Add('# Ammo');
  result.Add('');
  for i := 0 to Ord(NUMAMMO) - 1 do
  begin
    result.Add('Ammo %d', [i]);

    result.Add('%s = %d', [capitalizedstring(ammo_tokens[0]), maxammo[i]]);

    result.Add('');
  end;

  //////////////////////////////////////////////////////////////////////////////
  // Add Misc
  //////////////////////////////////////////////////////////////////////////////
  result.Add('');
  result.Add('# Misc, health and armor bonus');
  result.Add('');
  result.Add('Misc');
  result.Add('');

  result.Add('%s = %d', [capitalizedstring(misc_tokens[0]), p_maxhealth]);
  result.Add('%s = %d', [capitalizedstring(misc_tokens[1]), p_soulspherehealth]);
  result.Add('%s = %d', [capitalizedstring(misc_tokens[2]), p_megaspherehealth]);
  result.Add('%s = %d', [capitalizedstring(misc_tokens[3]), p_medikithealth]);
  result.Add('%s = %d', [capitalizedstring(misc_tokens[4]), p_stimpackhealth]);
  result.Add('%s = %d', [capitalizedstring(misc_tokens[5]), p_bonushealth]);
  result.Add('%s = %d', [capitalizedstring(misc_tokens[6]), p_maxarmor]);
  result.Add('%s = %d', [capitalizedstring(misc_tokens[7]), p_greenarmorclass]);
  result.Add('%s = %d', [capitalizedstring(misc_tokens[8]), p_bluearmorclass]);
  result.Add('%s = %d', [capitalizedstring(misc_tokens[9]), p_maxchickenhealth]);
  result.Add('%s = %d', [capitalizedstring(misc_tokens[10]), p_maxartifacts]);

  result.Add('');

  //////////////////////////////////////////////////////////////////////////////
  // Add pars
  //////////////////////////////////////////////////////////////////////////////
  result.Add('');
  result.Add('# Par times');
  result.Add('');
  result.Add('[PARS]');
  result.Add('# Heretic');
  for i := 1 to 5 do
    for j := 1 to 9 do
      result.Add('PAR %d %d %d', [i, j, pars[i, j]]);

  result.Add('');

  //////////////////////////////////////////////////////////////////////////////
  // Add strings
  //////////////////////////////////////////////////////////////////////////////
  result.Add('');
  result.Add('# Strings');
  result.Add('');
  result.Add('[Strings]');
  for i := 0 to deh_strings.numstrings - 1 do
    result.Add('%s = %s', [deh_strings._array[i].name, DEH_StringToCString(deh_strings._array[i].pstr^)]);
  result.Add('');

  //////////////////////////////////////////////////////////////////////////////
  // Add music
  //////////////////////////////////////////////////////////////////////////////
  result.Add('');
  result.Add('# Music');
  result.Add('');
  result.Add('[MUSIC]');
  for i := 1 to nummusic - 1 do
  begin
    if S_music[i].mapname <> '' then
      result.Add('%d = %s %s(Map: %s)', [i, S_music[i].name, StringOfChar(' ', 6 - Length(S_music[i].name)), S_music[i].mapname])
    else
      result.Add('%d = %s', [i, S_music[i].name]);
  end;
  result.Add('');

  //////////////////////////////////////////////////////////////////////////////
  // Add sounds
  //////////////////////////////////////////////////////////////////////////////
  result.Add('');
  result.Add('# Sounds');
  result.Add('');
  result.Add('[SOUND]');
  for i := 1 to numsfx - 1 do
    result.Add('%d = %s', [i, S_sfx[i].name]);
  result.Add('');

  result.Add(StringOfChar('#', 80));
  result.Add('# End of file');
  result.Add(StringOfChar('#', 80));
end;

//==============================================================================
//
// DEH_Init
//
// JVAL
// Initializing DEH tokens
//
//==============================================================================
procedure DEH_Init;
var
  i, j, k: integer;
begin
  if deh_initialized then
    exit;

  deh_initialized := true;

  DEH_InitActionsHash;

  mobj_tokens := TDTextList.Create;

  mobj_tokens.Add('ID #');               // .doomednum                // 0
  mobj_tokens.Add('INITIAL FRAME');      // .spawnstate               // 1
  mobj_tokens.Add('HIT POINTS');         // .spawnhealth              // 2
  mobj_tokens.Add('FIRST MOVING FRAME'); // .seestate                 // 3
  mobj_tokens.Add('ALERT SOUND');        // .seesound                 // 4
  mobj_tokens.Add('REACTION TIME');      // .reactiontime             // 5
  mobj_tokens.Add('ATTACK SOUND');       // .attacksound              // 6
  mobj_tokens.Add('INJURY FRAME');       // .painstate                // 7
  mobj_tokens.Add('PAIN CHANCE');        // .painchance               // 8
  mobj_tokens.Add('PAIN SOUND');         // .painsound                // 9
  mobj_tokens.Add('CLOSE ATTACK FRAME'); // .meleestate               // 10
  mobj_tokens.Add('FAR ATTACK FRAME');   // .missilestate             // 11
  mobj_tokens.Add('DEATH FRAME');        // .deathstate               // 12
  mobj_tokens.Add('EXPLODING FRAME');    // .xdeathstate              // 13
  mobj_tokens.Add('DEATH SOUND');        // .deathsound               // 14
  mobj_tokens.Add('SPEED');              // .speed                    // 15
  mobj_tokens.Add('WIDTH');              // .radius                   // 16
  mobj_tokens.Add('HEIGHT');             // .height                   // 17
  mobj_tokens.Add('MASS');               // .mass                     // 18
  mobj_tokens.Add('MISSILE DAMAGE');     // .damage                   // 19
  mobj_tokens.Add('ACTION SOUND');       // .activesound              // 20
  mobj_tokens.Add('BITS');               // .flags                    // 21
  mobj_tokens.Add('BITS2');              // .flags                    // 22
  mobj_tokens.Add('RESPAWN FRAME');      // .raisestate               // 23
  mobj_tokens.Add('FLAGS_EX');           // .flags_ex (DelphiDoom)    // 24
  mobj_tokens.Add('CUSTOM SOUND 1');     // .customsound1             // 25
  mobj_tokens.Add('CUSTOM SOUND 2');     // .customsound2             // 26
  mobj_tokens.Add('CUSTOM SOUND 3');     // .customsound3             // 27
  mobj_tokens.Add('EXPLOSION DAMAGE');   // .explosiondamage          // 28
  mobj_tokens.Add('EXPLOSION RADIUS');   // .explosionradius          // 29
  mobj_tokens.Add('MELEE DAMAGE');       // .meleedamage              // 30
  mobj_tokens.Add('INHERITSFROM');       // .inheritsfrom             // 31
  mobj_tokens.Add('NAME');               // .name                     // 32
  mobj_tokens.Add('MELEE SOUND');        // .meleesound               // 33
  mobj_tokens.Add('RENDERSTYLE');        // .renderstyle              // 34
  mobj_tokens.Add('ALPHA');              // .alpha                    // 35
  mobj_tokens.Add('DROPITEM');           // .dropitem                 // 36
  mobj_tokens.Add('MISSILETYPE');        // .missiletype              // 37
  mobj_tokens.Add('HEAL FRAME');         // .healstate                // 38
  mobj_tokens.Add('FLAGS2_EX');          // .flags2_ex (DelphiDoom)   // 39
  mobj_tokens.Add('CRASH FRAME');        // .crashstate               // 40
  mobj_tokens.Add('VSPEED');             // .vspeed                   // 41
  mobj_tokens.Add('PUSHFACTOR');         // .pushfactor               // 42
  mobj_tokens.Add('SCALE');              // .scale                    // 43
  mobj_tokens.Add('GRAVITY');            // .gravity                  // 44
  mobj_tokens.Add('FLAGS3_EX');          // .flags3_ex (DelphiDoom)   // 45
  mobj_tokens.Add('FLAGS4_EX');          // .flags4_ex (DelphiDoom)   // 46
  mobj_tokens.Add('MINMISSILECHANCE');   // .minmissilechance         // 47
  mobj_tokens.Add('FLOAT SPEED');        // .floatspeed               // 48
  mobj_tokens.Add('NORMAL SPEED');       // .normalspeed              // 49
  mobj_tokens.Add('FAST SPEED');         // .fastspeed                // 50
  mobj_tokens.Add('OBITUARY');           // .obituary                 // 51
  mobj_tokens.Add('HIT OBITUARY');       // .hitobituary              // 52
  mobj_tokens.Add('GENDER');             // .gender                   // 53
  mobj_tokens.Add('MELEE RANGE');        // .meleerange               // 54
  mobj_tokens.Add('MAX STEP HEIGHT');    // .maxstepheight            // 55
  mobj_tokens.Add('MAX DROPOFF HEIGHT'); // .maxdropoffheight         // 56
  mobj_tokens.Add('GIB HEALTH');         // .gibhealth                // 57
  mobj_tokens.Add('MAX TARGET RANGE');   // .maxtargetrange           // 58
  mobj_tokens.Add('WEAVE INDEX XY');     // .WeaveIndexXY             // 59
  mobj_tokens.Add('WEAVE INDEX Z');      // .WeaveIndexZ              // 60
  mobj_tokens.Add('FRICTION');           // .Friction                 // 61
  mobj_tokens.Add('SPRITE DX');          // .spriteDX                 // 62
  mobj_tokens.Add('SPRITE DY');          // .spriteDY                 // 63
  mobj_tokens.Add('INTERACT FRAME');     // .interactstate (DelphiDoom) // 64
  mobj_tokens.Add('FLAGS5_EX');          // .flags5_ex (DelphiDoom)   // 65
  mobj_tokens.Add('FLAGS6_EX');          // .flags6_ex (DelphiDoom)   // 66
  mobj_tokens.Add('MBF21 BITS');         // .mbf21flags               // 67
  mobj_tokens.Add('INFIGHTING GROUP');   // .infighting_group         // 68
  mobj_tokens.Add('PROJECTILE GROUP');   // .projectile_group         // 69
  mobj_tokens.Add('SPLASH GROUP');       // .splash_group             // 70
  mobj_tokens.Add('RIP SOUND');          // .ripsound                 // 71
  mobj_tokens.Add('CRUSH FRAME');        // .crushstate               // 72
  mobj_tokens.Add('ACTIVE SOUND');       // .activesound              // 73 - Alias for 20
  mobj_tokens.Add('RADIUS');             // .radius                   // 74 - Alias for 16
  mobj_tokens.Add('BLOOD COLOR');        // .bloodcolor               // 75
  mobj_tokens.Add('TRANSLATION');        // .translationname          // 76
  mobj_tokens.Add('DROPPED ITEM');       // .dropitem                 // 77 - Alias for 36
  mobj_tokens.Add('MISSILEHEIGHT');      // .missileheight (DelphiDoom) // 78
  mobj_tokens.Add('MELEE THRESHOLD');    // .meleethreshold           // 79

  mobj_tokens_hash := TDEHStringsHashTable.Create;
  mobj_tokens_hash.AssignList(mobj_tokens);

  mobj_flags := TDTextList.Create;
  mobj_flags.Add('MF_SPECIAL');
  mobj_flags.Add('MF_SOLID');
  mobj_flags.Add('MF_SHOOTABLE');
  mobj_flags.Add('MF_NOSECTOR');
  mobj_flags.Add('MF_NOBLOCKMAP');
  mobj_flags.Add('MF_AMBUSH');
  mobj_flags.Add('MF_JUSTHIT');
  mobj_flags.Add('MF_JUSTATTACKED');
  mobj_flags.Add('MF_SPAWNCEILING');
  mobj_flags.Add('MF_NOGRAVITY');
  mobj_flags.Add('MF_DROPOFF');
  mobj_flags.Add('MF_PICKUP');
  mobj_flags.Add('MF_NOCLIP');
  mobj_flags.Add('MF_SLIDEONWALLS');
  mobj_flags.Add('MF_FLOAT');
  mobj_flags.Add('MF_TELEPORT');
  mobj_flags.Add('MF_MISSILE');
  mobj_flags.Add('MF_DROPPED');
  mobj_flags.Add('MF_SHADOW');
  mobj_flags.Add('MF_NOBLOOD');
  mobj_flags.Add('MF_CORPSE');
  mobj_flags.Add('MF_INFLOAT');
  mobj_flags.Add('MF_COUNTKILL');
  mobj_flags.Add('MF_COUNTITEM');
  mobj_flags.Add('MF_SKULLFLY');
  mobj_flags.Add('MF_NOTDMATCH');
  mobj_flags.Add('MF_TRANSLATION1');
  mobj_flags.Add('MF_TRANSLATION2');
  mobj_flags.Add('MF_UNUSED2');
  mobj_flags.Add('MF_UNUSED3');
  mobj_flags.Add('MF_UNUSED4');
  mobj_flags.Add('MF_TRANSLUCENT');

  mobj_flags_hash := TDEHStringsHashTable.Create;
  mobj_flags_hash.AssignList(mobj_flags);

  mobj_flags2 := TDTextList.Create;
  mobj_flags2.Add('MF2_LOGRAV');
  mobj_flags2.Add('MF2_WINDTHRUST');
  mobj_flags2.Add('MF2_FLOORBOUNCE');
  mobj_flags2.Add('MF2_THRUGHOST');
  mobj_flags2.Add('MF2_FLY');
  mobj_flags2.Add('MF2_FOOTCLIP');
  mobj_flags2.Add('MF2_SPAWNFLOAT');
  mobj_flags2.Add('MF2_NOTELEPORT');
  mobj_flags2.Add('MF2_RIP');
  mobj_flags2.Add('MF2_PUSHABLE');
  mobj_flags2.Add('MF2_SLIDE');
  mobj_flags2.Add('MF2_ONMOBJ');
  mobj_flags2.Add('MF2_PASSMOBJ');
  mobj_flags2.Add('MF2_CANNOTPUSH');
  mobj_flags2.Add('MF2_FEETARECLIPPED');
  mobj_flags2.Add('MF2_BOSS');
  mobj_flags2.Add('MF2_FIREDAMAGE');
  mobj_flags2.Add('MF2_NODMGTHRUST');
  mobj_flags2.Add('MF2_TELESTOMP');
  mobj_flags2.Add('MF2_FLOATBOB');
  mobj_flags2.Add('MF2_DONTDRAW');

  mobj_flags2_hash := TDEHStringsHashTable.Create;
  mobj_flags2_hash.AssignList(mobj_flags2);

  mobj_flags_ex := TDTextList.Create;
  mobj_flags_ex.Add('MF_EX_TRANSPARENT');
  mobj_flags_ex.Add('MF_EX_WHITELIGHT');
  mobj_flags_ex.Add('MF_EX_REDLIGHT');
  mobj_flags_ex.Add('MF_EX_GREENLIGHT');
  mobj_flags_ex.Add('MF_EX_BLUELIGHT');
  mobj_flags_ex.Add('MF_EX_YELLOWLIGHT');
  mobj_flags_ex.Add('MF_EX_FLOATBOB');
  mobj_flags_ex.Add('MF_EX_NORADIUSDMG');
  mobj_flags_ex.Add('MF_EX_FIRERESIST');
  mobj_flags_ex.Add('MF_EX_RANDOMSEESOUND');
  mobj_flags_ex.Add('MF_EX_RANDOMPAINSOUND');
  mobj_flags_ex.Add('MF_EX_RANDOMATTACKSOUND');
  mobj_flags_ex.Add('MF_EX_RANDOMDEATHSOUND');
  mobj_flags_ex.Add('MF_EX_RANDOMACTIVESOUND');
  mobj_flags_ex.Add('MF_EX_RANDOMCUSTOMSOUND1');
  mobj_flags_ex.Add('MF_EX_RANDOMCUSTOMSOUND2');
  mobj_flags_ex.Add('MF_EX_RANDOMCUSTOMSOUND3');
  mobj_flags_ex.Add('MF_EX_CUSTOMEXPLODE');
  mobj_flags_ex.Add('MF_EX_BOSS');
  mobj_flags_ex.Add('MF_EX_FLOORHUGGER');
  mobj_flags_ex.Add('MF_EX_CEILINGHUGGER');
  mobj_flags_ex.Add('MF_EX_SEEKERMISSILE');
  mobj_flags_ex.Add('MF_EX_SPAWNFLOAT');
  mobj_flags_ex.Add('MF_EX_DONTHURTSPECIES');
  mobj_flags_ex.Add('MF_EX_LOWGRAVITY');
  mobj_flags_ex.Add('MF_EX_INVULNERABLE');
  mobj_flags_ex.Add('MF_EX_RANDOMMELEESOUND');
  mobj_flags_ex.Add('MF_EX_DONOTREMOVE');
  mobj_flags_ex.Add('MF_EX_GHOST');
  mobj_flags_ex.Add('MF_EX_THRUGHOST');
  mobj_flags_ex.Add('MF_EX_LOOKALLAROUND');

  mobj_flags_ex_hash := TDEHStringsHashTable.Create;
  mobj_flags_ex_hash.AssignList(mobj_flags_ex);

  mobj_flags2_ex := TDTextList.Create;
  mobj_flags2_ex.Add('MF2_EX_MEDIUMGRAVITY');
  mobj_flags2_ex.Add('MF2_EX_FRIGHTENED');
  mobj_flags2_ex.Add('MF2_EX_NODAMAGE');
  mobj_flags2_ex.Add('MF2_EX_DONTRUNSCRIPTS');
  mobj_flags2_ex.Add('MF2_EX_PRECISESPAWNANGLE');
  mobj_flags2_ex.Add('MF2_EX_CUSTOMDROPITEM');
  // JVAL: VERSION 204
  mobj_flags2_ex.Add('MF2_EX_CANTLEAVEFLOORPIC');
  mobj_flags2_ex.Add('MF2_EX_JUMPDOWN');
  mobj_flags2_ex.Add('MF2_EX_FULLVOLACTIVE');
  mobj_flags2_ex.Add('MF2_EX_FULLVOLDEATH');
  mobj_flags2_ex.Add('MF2_EX_FULLVOLSEE');
  mobj_flags2_ex.Add('MF2_EX_FULLVOLPAIN');
  mobj_flags2_ex.Add('MF2_EX_FULLVOLATTACK');
  mobj_flags2_ex.Add('MF2_EX_DONOTRENDERSHADOW');
  // JVAL: VERSION 205
  mobj_flags2_ex.Add('MF2_EX_SEEINVISIBLE');
  mobj_flags2_ex.Add('MF2_EX_MISSILEHURTSPECIES');
  mobj_flags2_ex.Add('MF2_EX_NOHITFLOOR');
  mobj_flags2_ex.Add('MF2_EX_JUMPUP');
  mobj_flags2_ex.Add('MF2_EX_DONTBLOCKPLAYER');
  mobj_flags2_ex.Add('MF2_EX_INTERACTIVE'); // JVAL: VERSION 207
  mobj_flags2_ex.Add('MF2_EX_JUSTAPPEARED');
  mobj_flags2_ex.Add('MF2_EX_DONTINFIGHTMONSTERS'); // JVAL: VERSION 207
  mobj_flags2_ex.Add('MF2_EX_FRIEND'); // JVAL: VERSION 207

  mobj_flags2_ex_hash := TDEHStringsHashTable.Create;
  mobj_flags2_ex_hash.AssignList(mobj_flags2_ex);

  mobj_flags3_ex := TDTextList.Create;
  mobj_flags3_ex.Add('MF3_EX_FLOORBOUNCE');
  mobj_flags3_ex.Add('MF3_EX_CEILINGBOUNCE');
  mobj_flags3_ex.Add('MF3_EX_WALLBOUNCE');
  mobj_flags3_ex.Add('MF3_EX_NOMAXMOVE');
  mobj_flags3_ex.Add('MF3_EX_NOCRUSH');
  mobj_flags3_ex.Add('MF3_EX_BLOODIGNOREDAMAGE');
  // JVAL: VERSION 206
  mobj_flags3_ex.Add('MF3_EX_NORENDERINTERPOLATION');
  mobj_flags3_ex.Add('MF3_EX_LINEDONE');
  mobj_flags3_ex.Add('MF3_EX_FLIPSPRITE');
  mobj_flags3_ex.Add('MF3_EX_MISSILEMORE');
  mobj_flags3_ex.Add('MF3_EX_MISSILEEVENMORE');
  mobj_flags3_ex.Add('MF3_EX_STRIFEDAMAGE');
  mobj_flags3_ex.Add('MF3_EX_NOTIMEFREEZE');
  mobj_flags3_ex.Add('MF3_EX_NOFEAR');
  mobj_flags3_ex.Add('MF3_EX_CAUSEFEAR');
  mobj_flags3_ex.Add('MF3_EX_SLIDING');
  mobj_flags3_ex.Add('MF3_EX_THRUACTORS');
  mobj_flags3_ex.Add('MF3_EX_THRUSPECIES');
  mobj_flags3_ex.Add('MF3_EX_NOBLOCKMONST');
  mobj_flags3_ex.Add('MF3_EX_NOTAUTOAIMED');
  // JVAL: VERSION 207
  mobj_flags3_ex.Add('MF3_EX_ABSOLUTEDAMAGE');
  mobj_flags3_ex.Add('MF3_EX_NOGRAVITYDEATH');
  mobj_flags3_ex.Add('MF3_EX_FREEZEDAMAGE');
  mobj_flags3_ex.Add('MF3_EX_NOFREEZEDAMAGE');
  mobj_flags3_ex.Add('MF3_EX_FREEZEDAMAGERESIST');
  mobj_flags3_ex.Add('MF3_EX_FLAMEDAMAGE');
  mobj_flags3_ex.Add('MF3_EX_NOFLAMEDAMAGE');

  mobj_flags3_ex_hash := TDEHStringsHashTable.Create;
  mobj_flags3_ex_hash.AssignList(mobj_flags3_ex);

  mobj_flags4_ex := TDTextList.Create;
  mobj_flags4_ex.Add('MF4_EX_FLAMEDAMAGERESIST');
  mobj_flags4_ex.Add('MF4_EX_THRUMONSTERS');
  mobj_flags4_ex.Add('MF4_EX_ABSOLUTEDROPITEMPOS');
  mobj_flags4_ex.Add('MF4_EX_CANNOTSTEP');
  mobj_flags4_ex.Add('MF4_EX_CANNOTDROPOFF');
  mobj_flags4_ex.Add('MF4_EX_FORCERADIUSDMG');
  mobj_flags4_ex.Add('MF4_EX_SHORTMRANGE');
  mobj_flags4_ex.Add('MF4_EX_DMGIGNORED');
  mobj_flags4_ex.Add('MF4_EX_HIGHERMPROB');
  mobj_flags4_ex.Add('MF4_EX_RANGEHALF');
  mobj_flags4_ex.Add('MF4_EX_NOTHRESHOLD');
  mobj_flags4_ex.Add('MF4_EX_LONGMELEERANGE');
  mobj_flags4_ex.Add('MF4_EX_TRACEDEFINED');
  mobj_flags4_ex.Add('MF4_EX_MAP07BOSS1');
  mobj_flags4_ex.Add('MF4_EX_MAP07BOSS2');
  mobj_flags4_ex.Add('MF4_EX_SELFAPPLYINGLIGHT');
  mobj_flags4_ex.Add('MF4_EX_FULLVOLRIP');
  mobj_flags4_ex.Add('MF4_EX_RANDOMRIPSOUND');
  mobj_flags4_ex.Add('MF4_EX_ALWAYSFINISHSOUND');
  mobj_flags4_ex.Add('MF4_EX_NEVERFINISHSOUND');
  mobj_flags4_ex.Add('MF4_EX_DONTGIB');
  mobj_flags4_ex.Add('MF4_EX_BACKINGMELEE');

  mobj_flags4_ex_hash := TDEHStringsHashTable.Create;
  mobj_flags4_ex_hash.AssignList(mobj_flags4_ex);

  mobj_flags5_ex := TDTextList.Create;
  mobj_flags5_ex.Add('MF5_EX_PUSHWALL');
  mobj_flags5_ex.Add('MF5_EX_MCROSS');
  mobj_flags5_ex.Add('MF5_EX_PCROSS');
  mobj_flags5_ex.Add('MF5_EX_IMPACT');

  mobj_flags5_ex_hash := TDEHStringsHashTable.Create;
  mobj_flags5_ex_hash.AssignList(mobj_flags5_ex);

  mobj_flags6_ex := TDTextList.Create;

  mobj_flags6_ex_hash := TDEHStringsHashTable.Create;
  mobj_flags6_ex_hash.AssignList(mobj_flags6_ex);

  // MBF21
  mobj_flags_mbf21 := TDTextList.Create;
  mobj_flags_mbf21.Add('LOGRAV');
  mobj_flags_mbf21.Add('SHORTMRANGE');
  mobj_flags_mbf21.Add('DMGIGNORED');
  mobj_flags_mbf21.Add('NORADIUSDMG');
  mobj_flags_mbf21.Add('FORCERADIUSDMG');
  mobj_flags_mbf21.Add('HIGHERMPROB');
  mobj_flags_mbf21.Add('RANGEHALF');
  mobj_flags_mbf21.Add('NOTHRESHOLD');
  mobj_flags_mbf21.Add('LONGMELEE');
  mobj_flags_mbf21.Add('BOSS');
  mobj_flags_mbf21.Add('MAP07BOSS1');
  mobj_flags_mbf21.Add('MAP07BOSS2');
  mobj_flags_mbf21.Add('E1M8BOSS');
  mobj_flags_mbf21.Add('E2M8BOSS');
  mobj_flags_mbf21.Add('E3M8BOSS');
  mobj_flags_mbf21.Add('E4M6BOSS');
  mobj_flags_mbf21.Add('E4M8BOSS');
  mobj_flags_mbf21.Add('RIP');
  mobj_flags_mbf21.Add('FULLVOLSOUNDS');

  mobj_flags_mbf21_hash := TDEHStringsHashTable.Create;
  mobj_flags_mbf21_hash.AssignList(mobj_flags_mbf21);

  infighting_groups := TDTextList.Create;
  infighting_groups.Add('IG_DEFAULT');

  projectile_groups := TDTextList.Create;
  projectile_groups.Add('PG_DEFAULT');
  projectile_groups.Add('PG_BARON');

  splash_groups := TDTextList.Create;
  splash_groups.Add('SG_DEFAULT');

  // JVAL: 20200330 - State flags
  state_flags_ex := TDTextList.Create;
  state_flags_ex.Add('MF_EX_TRANSPARENT');
  state_flags_ex.Add('MF_EX_WHITELIGHT');
  state_flags_ex.Add('MF_EX_REDLIGHT');
  state_flags_ex.Add('MF_EX_GREENLIGHT');
  state_flags_ex.Add('MF_EX_BLUELIGHT');
  state_flags_ex.Add('MF_EX_YELLOWLIGHT');
  state_flags_ex.Add('MF_EX_STATE_RANDOM_SELECT');
  state_flags_ex.Add('MF_EX_STATE_RANDOM_RANGE');

  state_flags_mbf21 := TDTextList.Create; // MBF21
  state_flags_mbf21.Add('STATEF_SKILL5FAST'); // MBF21

  state_tokens := TDTextList.Create;
  state_tokens.Add('SPRITE NUMBER');    // 0 //.sprite
  state_tokens.Add('SPRITE SUBNUMBER'); // 1 //.frame
  state_tokens.Add('DURATION');         // 2 //.tics
  state_tokens.Add('NEXT FRAME');       // 3 //.nextstate
  state_tokens.Add('CODEP FRAME');      // 4 //.action
  state_tokens.Add('UNKNOWN 1');        // 5 //.misc1
  state_tokens.Add('UNKNOWN 2');        // 6 //.misc2
  state_tokens.Add('FLAGS_EX');         // 7 //.flags_ex (DelphiDoom)
  state_tokens.Add('OWNER');            // 8 //.Add an owner (DelphiDoom)
  state_tokens.Add('DURATION 2');       // 9 //.tics2
  state_tokens.Add('ARGS1');            // 10 // .args[0]
  state_tokens.Add('ARGS2');            // 11 // .args[1]
  state_tokens.Add('ARGS3');            // 12 // .args[2]
  state_tokens.Add('ARGS4');            // 13 // .args[3]
  state_tokens.Add('ARGS5');            // 14 // .args[4]
  state_tokens.Add('ARGS6');            // 15 // .args[5]
  state_tokens.Add('ARGS7');            // 16 // .args[6]
  state_tokens.Add('ARGS8');            // 17 // .args[7]
  state_tokens.Add('MBF21 BITS');       // 18 // .mbf21bits

  deh_actions[0].action.acp1 := nil;
  deh_actions[0].name := 'NULL';
  deh_actions[0].originalname := 'NULL';
  {$IFDEF DLL}deh_actions[0].decl := '';{$ENDIF}

  dehnumactions := 1;

  DEH_AddAction(@A_AccTeleGlitter, 'A_AccTeleGlitter()'); // 1
  DEH_AddAction(@A_AddPlayerCorpse, 'A_AddPlayerCorpse()'); // 2
  DEH_AddAction(@A_AddPlayerRain, 'A_AddPlayerRain()'); // 3
  DEH_AddAction(@A_BeakAttackPL1, 'A_BeakAttackPL1()'); // 4
  DEH_AddAction(@A_BeakAttackPL2, 'A_BeakAttackPL2()'); // 5
  DEH_AddAction(@A_BeakRaise, 'A_BeakRaise()'); // 6
  DEH_AddAction(@A_BeakReady, 'A_BeakReady()'); // 7
  DEH_AddAction(@A_BeastAttack, 'A_BeastAttack()'); // 8
  DEH_AddAction(@A_BeastPuff, 'A_BeastPuff()'); // 9
  DEH_AddAction(@A_BlueSpark, 'A_BlueSpark()'); // 10
  DEH_AddAction(@A_BoltSpark, 'A_BoltSpark()'); // 11
  DEH_AddAction(@A_BossDeath, 'A_BossDeath()'); // 12
  DEH_AddAction(@A_Chase, 'A_Chase()'); // 13
  DEH_AddAction(@A_CheckBurnGone, 'A_CheckBurnGone()'); // 14
  DEH_AddAction(@A_CheckSkullDone, 'A_CheckSkullDone()'); // 15
  DEH_AddAction(@A_CheckSkullFloor, 'A_CheckSkullFloor()'); // 16
  DEH_AddAction(@A_ChicAttack, 'A_ChicAttack()'); // 17
  DEH_AddAction(@A_ChicChase, 'A_ChicChase()'); // 18
  DEH_AddAction(@A_ChicLook, 'A_ChicLook()'); // 19
  DEH_AddAction(@A_ChicPain, 'A_ChicPain()'); // 20
  DEH_AddAction(@A_ClinkAttack, 'A_ClinkAttack()'); // 21
  DEH_AddAction(@A_ContMobjSound, 'A_ContMobjSound()'); // 22
  DEH_AddAction(@A_DeathBallImpact, 'A_DeathBallImpact()'); // 23
  DEH_AddAction(@A_DripBlood, 'A_DripBlood()'); // 24
  DEH_AddAction(@A_ESound, 'A_ESound()'); // 25
  DEH_AddAction(@A_Explode, 'A_Explode()'); // 26
  DEH_AddAction(@A_FaceTarget, 'A_FaceTarget()'); // 27
  DEH_AddAction(@A_Feathers, 'A_Feathers()'); // 28
  DEH_AddAction(@A_FireBlasterPL1, 'A_FireBlasterPL1()'); // 29
  DEH_AddAction(@A_FireBlasterPL2, 'A_FireBlasterPL2()'); // 30
  DEH_AddAction(@A_FireCrossbowPL1, 'A_FireCrossbowPL1()'); // 31
  DEH_AddAction(@A_FireCrossbowPL2, 'A_FireCrossbowPL2()'); // 32
  DEH_AddAction(@A_FireGoldWandPL1, 'A_FireGoldWandPL1()'); // 33
  DEH_AddAction(@A_FireGoldWandPL2, 'A_FireGoldWandPL2()'); // 34
  DEH_AddAction(@A_FireMacePL1, 'A_FireMacePL1()'); // 35
  DEH_AddAction(@A_FireMacePL2, 'A_FireMacePL2()'); // 36
  DEH_AddAction(@A_FirePhoenixPL1, 'A_FirePhoenixPL1()'); // 37
  DEH_AddAction(@A_FirePhoenixPL2, 'A_FirePhoenixPL2()'); // 38
  DEH_AddAction(@A_FireSkullRodPL1, 'A_FireSkullRodPL1()'); // 39
  DEH_AddAction(@A_FireSkullRodPL2, 'A_FireSkullRodPL2()'); // 40
  DEH_AddAction(@A_FlameEnd, 'A_FlameEnd()'); // 41
  DEH_AddAction(@A_FlameSnd, 'A_FlameSnd()'); // 42
  DEH_AddAction(@A_FloatPuff, 'A_FloatPuff()'); // 43
  DEH_AddAction(@A_FreeTargMobj, 'A_FreeTargMobj()'); // 44
  DEH_AddAction(@A_GauntletAttack, 'A_GauntletAttack()'); // 45
  DEH_AddAction(@A_GenWizard, 'A_GenWizard()'); // 46
  DEH_AddAction(@A_GhostOff, 'A_GhostOff()'); // 47
  DEH_AddAction(@A_HeadAttack, 'A_HeadAttack()'); // 48
  DEH_AddAction(@A_HeadFireGrow, 'A_HeadFireGrow()'); // 49
  DEH_AddAction(@A_HeadIceImpact, 'A_HeadIceImpact()'); // 50
  DEH_AddAction(@A_HideInCeiling, 'A_HideInCeiling()'); // 51
  DEH_AddAction(@A_HideThing, 'A_HideThing()'); // 52
  DEH_AddAction(@A_ImpDeath, 'A_ImpDeath()'); // 53
  DEH_AddAction(@A_ImpExplode, 'A_ImpExplode()'); // 54
  DEH_AddAction(@A_ImpMeAttack, 'A_ImpMeAttack()'); // 55
  DEH_AddAction(@A_ImpMsAttack, 'A_ImpMsAttack()'); // 56
  DEH_AddAction(@A_ImpMsAttack2, 'A_ImpMsAttack2()'); // 57
  DEH_AddAction(@A_ImpXDeath1, 'A_ImpXDeath1()'); // 58
  DEH_AddAction(@A_ImpXDeath2, 'A_ImpXDeath2()'); // 59
  DEH_AddAction(@A_InitKeyGizmo, 'A_InitKeyGizmo()'); // 60
  DEH_AddAction(@A_InitPhoenixPL2, 'A_InitPhoenixPL2()'); // 61
  DEH_AddAction(@A_KnightAttack, 'A_KnightAttack()'); // 62
  DEH_AddAction(@A_Light0, 'A_Light0()'); // 63
  DEH_AddAction(@A_Look, 'A_Look()'); // 64
  DEH_AddAction(@A_Lower, 'A_Lower()'); // 65
  DEH_AddAction(@A_MaceBallImpact, 'A_MaceBallImpact()'); // 66
  DEH_AddAction(@A_MaceBallImpact2, 'A_MaceBallImpact2()'); // 67
  DEH_AddAction(@A_MacePL1Check, 'A_MacePL1Check()'); // 68
  DEH_AddAction(@A_MakePod, 'A_MakePod()'); // 69
  DEH_AddAction(@A_MinotaurAtk1, 'A_MinotaurAtk1()'); // 70
  DEH_AddAction(@A_MinotaurAtk2, 'A_MinotaurAtk2()'); // 71
  DEH_AddAction(@A_MinotaurAtk3, 'A_MinotaurAtk3()'); // 72
  DEH_AddAction(@A_MinotaurCharge, 'A_MinotaurCharge()'); // 73
  DEH_AddAction(@A_MinotaurDecide, 'A_MinotaurDecide()'); // 74
  DEH_AddAction(@A_MntrFloorFire, 'A_MntrFloorFire()'); // 75
  DEH_AddAction(@A_MummyAttack, 'A_MummyAttack()'); // 76
  DEH_AddAction(@A_MummyAttack2, 'A_MummyAttack2()'); // 77
  DEH_AddAction(@A_MummyFX1Seek, 'A_MummyFX1Seek()'); // 78
  DEH_AddAction(@A_MummySoul, 'A_MummySoul()'); // 79
  DEH_AddAction(@A_NoBlocking, 'A_NoBlocking()'); // 80
  DEH_AddAction(@A_Pain, 'A_Pain()'); // 81
  DEH_AddAction(@A_PhoenixPuff, 'A_PhoenixPuff()'); // 82
  DEH_AddAction(@A_PodPain, 'A_PodPain()'); // 83
  DEH_AddAction(@A_RainImpact, 'A_RainImpact()'); // 84
  DEH_AddAction(@A_Raise, 'A_Raise()'); // 85
  DEH_AddAction(@A_ReFire, 'A_ReFire()'); // 86
  DEH_AddAction(@A_RemovePod, 'A_RemovePod()'); // 87
  DEH_AddAction(@A_RestoreArtifact, 'A_RestoreArtifact()'); // 88
  DEH_AddAction(@A_RestoreSpecialThing1, 'A_RestoreSpecialThing1()'); // 89
  DEH_AddAction(@A_RestoreSpecialThing2, 'A_RestoreSpecialThing2()'); // 90
  DEH_AddAction(@A_Scream, 'A_Scream()'); // 91
  DEH_AddAction(@A_ShutdownPhoenixPL2, 'A_ShutdownPhoenixPL2()'); // 92
  DEH_AddAction(@A_SkullPop, 'A_SkullPop()'); // 93
  DEH_AddAction(@A_SkullRodPL2Seek, 'A_SkullRodPL2Seek()'); // 94
  DEH_AddAction(@A_SkullRodStorm, 'A_SkullRodStorm()'); // 95
  DEH_AddAction(@A_SnakeAttack, 'A_SnakeAttack()'); // 96
  DEH_AddAction(@A_SnakeAttack2, 'A_SnakeAttack2()'); // 97
  DEH_AddAction(@A_Sor1Chase, 'A_Sor1Chase()'); // 98
  DEH_AddAction(@A_Sor1Pain, 'A_Sor1Pain()'); // 99
  DEH_AddAction(@A_Sor2DthInit, 'A_Sor2DthInit()'); // 100
  DEH_AddAction(@A_Sor2DthLoop, 'A_Sor2DthLoop()'); // 101
  DEH_AddAction(@A_SorcererRise, 'A_SorcererRise()'); // 102
  DEH_AddAction(@A_SorDBon, 'A_SorDBon()'); // 103
  DEH_AddAction(@A_SorDExp, 'A_SorDExp()'); // 104
  DEH_AddAction(@A_SorDSph, 'A_SorDSph()'); // 105
  DEH_AddAction(@A_SorRise, 'A_SorRise()'); // 106
  DEH_AddAction(@A_SorSightSnd, 'A_SorSightSnd()'); // 107
  DEH_AddAction(@A_SorZap, 'A_SorZap()'); // 108
  DEH_AddAction(@A_SpawnRippers, 'A_SpawnRippers()'); // 109
  DEH_AddAction(@A_SpawnTeleGlitter, 'A_SpawnTeleGlitter()'); // 110
  DEH_AddAction(@A_SpawnTeleGlitter2, 'A_SpawnTeleGlitter2()'); // 111
  DEH_AddAction(@A_Srcr1Attack, 'A_Srcr1Attack()'); // 112
  DEH_AddAction(@A_Srcr2Attack, 'A_Srcr2Attack()'); // 113
  DEH_AddAction(@A_Srcr2Decide, 'A_Srcr2Decide()'); // 114
  DEH_AddAction(@A_StaffAttackPL1, 'A_StaffAttackPL1()'); // 115
  DEH_AddAction(@A_StaffAttackPL2, 'A_StaffAttackPL2()'); // 116
  DEH_AddAction(@A_UnHideThing, 'A_UnHideThing()'); // 117
  DEH_AddAction(@A_VolcanoBlast, 'A_VolcanoBlast()'); // 118
  DEH_AddAction(@A_VolcanoSet, 'A_VolcanoSet()'); // 119
  DEH_AddAction(@A_VolcBallImpact, 'A_VolcBallImpact()'); // 120
  DEH_AddAction(@A_WeaponReady, 'A_WeaponReady()'); // 121
  DEH_AddAction(@A_WhirlwindSeek, 'A_WhirlwindSeek()'); // 122
  DEH_AddAction(@A_WizAtk1, 'A_WizAtk1()'); // 123
  DEH_AddAction(@A_WizAtk2, 'A_WizAtk2()'); // 124
  DEH_AddAction(@A_WizAtk3, 'A_WizAtk3()'); // 125
  DEH_AddAction(@A_CustomSound1, 'A_CustomSound1()'); // 126
  DEH_AddAction(@A_CustomSound2, 'A_CustomSound2()'); // 127
  DEH_AddAction(@A_CustomSound3, 'A_CustomSound3()'); // 128
  DEH_AddAction(@A_RandomPainSound, 'A_RandomPainSound()'); // 129
  DEH_AddAction(@A_RandomSeeSound, 'A_RandomSeeSound()'); // 130
  DEH_AddAction(@A_RandomAttackSound, 'A_RandomAttackSound()'); // 131
  DEH_AddAction(@A_RandomDeathSound, 'A_RandomDeathSound()'); // 132
  DEH_AddAction(@A_RandomActiveSound, 'A_RandomActiveSound()'); // 133
  DEH_AddAction(@A_RandomCustomSound1, 'A_RandomCustomSound1()'); // 134
  DEH_AddAction(@A_RandomCustomSound2, 'A_RandomCustomSound2()'); // 135
  DEH_AddAction(@A_RandomCustomSound3, 'A_RandomCustomSound3()'); // 136
  DEH_AddAction(@A_RandomCustomSound, 'A_RandomCustomSound()'); // 137
  DEH_AddAction(@A_Wander, 'A_Wander()'); // 138
  DEH_AddAction(@A_Playsound, 'A_Playsound(sound: string)'); // 139
  DEH_AddAction(@A_RandomSound, 'A_RandomSound(sound1: string, [sound2: string], ...)'); // 140
  DEH_AddAction(@A_Stop, 'A_Stop()'); // 141
  DEH_AddAction(@A_Jump, 'A_Jump(propability: random_t, offset1: integer, [offset2: integer], ...)'); // 142
  DEH_AddAction(@A_CustomMissile, 'A_CustomMissile(missiletype: string, [height: integer], [offset: integer], [angle: integer], [aimmode: integer], [pitch: integer])'); // 143
  DEH_AddAction(@A_NoGravity, 'A_NoGravity()'); // 144
  DEH_AddAction(@A_Gravity, 'A_Gravity()'); // 145
  DEH_AddAction(@A_NoBlocking, 'A_NoBlocking()'); // 146
  DEH_AddAction(@A_MeleeAttack, 'A_MeleeAttack([mindamage: integer], [maxdamage: integer])'); // 147
  DEH_AddAction(@A_SpawnItem, 'A_SpawnItem(type: string, [distance: float], [zheight: float], [angle: angle])'); // 148
  DEH_AddAction(@A_SeekerMissile, 'A_SeekerMissile(threshold_angle: angle, [turnMax_angle: angle])'); // 149
  DEH_AddAction(@A_CStaffMissileSlither, 'A_CStaffMissileSlither()'); // 150
  DEH_AddAction(@A_SetTranslucent, 'A_SetTranslucent(alpha: float, [style: integer])'); // 151
  DEH_AddAction(@A_Die, 'A_Die()'); // 152
  DEH_AddAction(@A_CustomBulletAttack, 'A_CustomBulletAttack(spread_xy: angle, numbullets: integer, damageperbullet: integer, [range: integer])'); // 153
  DEH_AddAction(@A_FadeOut, 'A_FadeOut(fade: float)'); // 154
  DEH_AddAction(@A_FadeIn, 'A_FadeIn(fade: float)'); // 155
  DEH_AddAction(@A_MissileAttack, 'A_MissileAttack([missiletype: string])'); // 156
  DEH_AddAction(@A_AdjustSideSpot, 'A_AdjustSideSpot(sideoffset: float)'); // 157
  DEH_AddAction(@A_Countdown, 'A_Countdown()'); // 158
  DEH_AddAction(@A_FastChase, 'A_FastChase()'); // 159
  DEH_AddAction(@A_LowGravity, 'A_LowGravity()'); // 160
  DEH_AddAction(@A_ThrustZ, 'A_ThrustZ(momz: float)'); // 161
  DEH_AddAction(@A_ThrustXY, 'A_ThrustXY(mom: float, ang: angle)'); // 162
  DEH_AddAction(@A_Turn, 'A_Turn(value: angle)'); // 163
  DEH_AddAction(@A_JumpIfCloser, 'A_JumpIfCloser(distancetotarget: float, offset: integer)'); // 164
  DEH_AddAction(@A_JumpIfHealthLower, 'A_JumpIfHealthLower(health: integer, offset: integer)'); // 165
  DEH_AddAction(@A_ScreamAndUnblock, 'A_ScreamAndUnblock()'); // 166
  DEH_AddAction(@A_PlayWeaponsound, 'A_PlayWeaponsound(sound: string)'); // 167
  DEH_AddAction(@A_SetInvulnerable, 'A_SetInvulnerable()'); // 168
  DEH_AddAction(@A_UnSetInvulnerable, 'A_UnSetInvulnerable()'); // 169
  DEH_AddAction(@A_RandomMeleeSound, 'A_RandomMeleeSound()'); // 170
  DEH_AddAction(@A_FloatBob, 'A_FloatBob()'); // 171
  DEH_AddAction(@A_NoFloatBob, 'A_NoFloatBob()'); // 172
  DEH_AddAction(@A_Missile, 'A_Missile()'); // 173
  DEH_AddAction(@A_NoMissile, 'A_NoMissile()'); // 174
  DEH_AddAction(@A_ComboAttack, 'A_ComboAttack()'); // 175
  DEH_AddAction(@A_BulletAttack, 'A_BulletAttack([numbullets: integer])'); // 176
  DEH_AddAction(@A_MediumGravity, 'A_MediumGravity()'); // 177
  DEH_AddAction(@A_FadeOut10, 'A_FadeOut10()'); // 178
  DEH_AddAction(@A_FadeOut20, 'A_FadeOut20()'); // 179
  DEH_AddAction(@A_FadeOut30, 'A_FadeOut30()'); // 180
  DEH_AddAction(@A_FadeIn10, 'A_FadeIn10()'); // 181
  DEH_AddAction(@A_FadeIn20, 'A_FadeIn20()'); // 182
  DEH_AddAction(@A_FadeIn30, 'A_FadeIn30()'); // 183
  DEH_AddAction(@A_SpawnItemEx, 'A_SpawnItemEx(itemtype: string, [xofs: float], [yofs: float], [zofs: float], [momx: float], [momy: float], [momz: float], [ang: angle], [flags: integer], [chance: integer])'); // 184
  DEH_AddAction(@A_RandomMissile, 'A_RandomMissile(missile1: string, [missile2: string], ...)'); // 185
  DEH_AddAction(@P_RemoveMobj, 'A_RemoveSelf()'); // 186
  DEH_AddAction(@A_GoTo, 'A_GoTo(propability: random_t, state: state_t)'); // 187
  DEH_AddAction(@A_GoToIfCloser, 'A_GoToIfCloser(distancetotarget: float, state: state_t)'); // 188
  DEH_AddAction(@A_GoToIfHealthLower, 'A_GoToIfHealthLower(health: integer, state: state_t)'); // 189
  DEH_AddAction(@A_ConsoleCommand, 'A_ConsoleCommand(cmd: string, [parm1: string], [parm2: string], ...)'); // 190
  DEH_AddAction(@A_SetFrightened, 'A_SetFrightened()'); // 191
  DEH_AddAction(@A_UnSetFrightened, 'A_UnSetFrightened()'); // 192
  DEH_AddAction(@A_SetCustomParam, 'A_SetCustomParam(param: string, value: integer)'); // 193
  DEH_AddAction(@A_AddCustomParam, 'A_AddCustomParam(param: string, value: integer)'); // 194
  DEH_AddAction(@A_SubtractCustomParam, 'A_SubtractCustomParam(param: string, value: integer)'); // 195
  DEH_AddAction(@A_SetTargetCustomParam, 'A_SetTargetCustomParam(param: string, value: integer)'); // 196
  DEH_AddAction(@A_AddTargetCustomParam, 'A_AddTargetCustomParam(param: string, value: integer)'); // 197
  DEH_AddAction(@A_SubtractTargetCustomParam, 'A_SubtractTargetCustomParam(param: string, value: integer)'); // 198
  DEH_AddAction(@A_JumpIfCustomParam, 'A_JumpIfCustomParam(param: string, value: integer, offset: integer)'); // 199
  DEH_AddAction(@A_JumpIfCustomParamLess, 'A_JumpIfCustomParamLess(param: string, value: integer, offset: integer)'); // 200
  DEH_AddAction(@A_JumpIfCustomParamGreater, 'A_JumpIfCustomParamGreater(param: string, value: integer, offset: integer)'); // 201
  DEH_AddAction(@A_JumpIfTargetCustomParam, 'A_JumpIfTargetCustomParam(param: string, value: integer, offset: integer)'); // 202
  DEH_AddAction(@A_JumpIfTargetCustomParamLess, 'A_JumpIfTargetCustomParamLess(param: string, value: integer, offset: integer)'); // 203
  DEH_AddAction(@A_JumpIfTargetCustomParamGreater, 'A_JumpIfTargetCustomParamGreater(param: string, value: integer, offset: integer)'); // 204
  DEH_AddAction(@A_GoToIfCustomParam, 'A_GoToIfCustomParam(param: string, value: integer, state: state_t)'); // 205
  DEH_AddAction(@A_GoToIfCustomParamLess, 'A_GoToIfCustomParamLess(param: string, value: integer, state: state_t)'); // 206
  DEH_AddAction(@A_GoToIfCustomParamGreater, 'A_GoToIfCustomParamGreater(param: string, value: integer, state: state_t)'); // 207
  DEH_AddAction(@A_GoToIfTargetCustomParam, 'A_GoToIfTargetCustomParam(param: string, value: integer, state: state_t)'); // 208
  DEH_AddAction(@A_GoToIfTargetCustomParamLess, 'A_GoToIfTargetCustomParamLess(param: string, value: integer, state: state_t)'); // 209
  DEH_AddAction(@A_GoToIfTargetCustomParamGreater, 'A_GoToIfTargetCustomParamGreater(param: string, value: integer, state: state_t)'); // 210
  DEH_AddAction(@A_GhostOn, 'A_GhostOn()'); // 211
  DEH_AddAction(@A_RunScript, 'A_RunScript(script1: string, [script2: string], ...)'); // 212
  DEH_AddAction(@A_Turn5, 'A_Turn5()'); // 213
  DEH_AddAction(@A_Turn10, 'A_Turn10()'); // 214
  DEH_AddAction(@A_Blocking, 'A_Blocking()'); // 215
  DEH_AddAction(@A_DoNotRunScripts, 'A_DoNotRunScripts()'); // 216
  DEH_AddAction(@A_DoRunScripts, 'A_DoRunScripts()'); // 217
  DEH_AddAction(@A_TargetDropItem, 'A_TargetDropItem(dropitemtype: string)'); // 218
  DEH_AddAction(@A_DefaultTargetDropItem, 'A_DefaultTargetDropItem()'); // 219
  DEH_AddAction(@A_SetDropItem, 'A_SetDropItem(dropitemtype: string)'); // 220
  DEH_AddAction(@A_SetDefaultDropItem, 'A_SetDefaultDropItem()'); // 221
  DEH_AddAction(@A_GlobalEarthQuake, 'A_GlobalEarthQuake(tics: integer)'); // 222
  DEH_AddAction(@A_JumpIfMapStringEqual, 'A_JumpIfMapStringEqual(parm: string, value: string, offset; integer)'); // 223
  DEH_AddAction(@A_JumpIfMapStringLess, 'A_JumpIfMapStringLess(parm: string, value: string, offset; integer)'); // 224
  DEH_AddAction(@A_JumpIfMapStringGreater, 'A_JumpIfMapStringGreater(parm: string, value: string, offset; integer)'); // 225
  DEH_AddAction(@A_JumpIfMapIntegerEqual, 'A_JumpIfMapIntegerEqual(parm: string, value: integer, offset: integer)'); // 226
  DEH_AddAction(@A_JumpIfMapIntegerLess, 'A_JumpIfMapIntegerLess(parm: string, value: integer, offset: integer)'); // 227
  DEH_AddAction(@A_JumpIfMapIntegerGreater, 'A_JumpIfMapIntegerGreater(parm: string, value: integer, offset: integer)'); // 228
  DEH_AddAction(@A_JumpIfMapFloatEqual, 'A_JumpIfMapFloatEqual(parm: string, value: float, offset: integer)'); // 229
  DEH_AddAction(@A_JumpIfMapFloatLess, 'A_JumpIfMapFloatLess(parm: string, value: float, offset: integer)'); // 230
  DEH_AddAction(@A_JumpIfMapFloatGreater, 'A_JumpIfMapFloatGreater(parm: string, value: float, offset: integer)'); // 231
  DEH_AddAction(@A_JumpIfWorldStringEqual, 'A_JumpIfWorldStringEqual(parm: string, value: string, offset: integer)'); // 232
  DEH_AddAction(@A_JumpIfWorldStringLess, 'A_JumpIfWorldStringLess(parm: string, value: string, offset: integer)'); // 233
  DEH_AddAction(@A_JumpIfWorldStringGreater, 'A_JumpIfWorldStringGreater(parm: string, value: string, offset: integer)'); // 234
  DEH_AddAction(@A_JumpIfWorldIntegerEqual, 'A_JumpIfWorldIntegerEqual(parm: string, value: integer, offset: integer)'); // 235
  DEH_AddAction(@A_JumpIfWorldIntegerLess, 'A_JumpIfWorldIntegerLess(parm: string, value: integer, offset: integer)'); // 236
  DEH_AddAction(@A_JumpIfWorldIntegerGreater, 'A_JumpIfWorldIntegerGreater(parm: string, value: integer, offset: integer)'); // 237
  DEH_AddAction(@A_JumpIfWorldFloatEqual, 'A_JumpIfWorldFloatEqual(parm: string, value: float, offset: integer)'); // 238
  DEH_AddAction(@A_JumpIfWorldFloatLess, 'A_JumpIfWorldFloatLess(parm: string, value: float, offset: integer)'); // 239
  DEH_AddAction(@A_JumpIfWorldFloatGreater, 'A_JumpIfWorldFloatGreater(parm: string, value: float, offset: integer)'); // 240
  DEH_AddAction(@A_GoToIfMapStringEqual, 'A_GoToIfMapStringEqual(parm: string, value: string, state: state_t)'); // 241
  DEH_AddAction(@A_GoToIfMapStringLess, 'A_GoToIfMapStringLess(parm: string, value: string, state: state_t)'); // 242
  DEH_AddAction(@A_GoToIfMapStringGreater, 'A_GoToIfMapStringGreater(parm: string, value: string, state: state_t)'); // 243
  DEH_AddAction(@A_GoToIfMapIntegerEqual, 'A_GoToIfMapIntegerEqual(parm: string, value: integer, state: state_t)'); // 244
  DEH_AddAction(@A_GoToIfMapIntegerLess, 'A_GoToIfMapIntegerLess(parm: string, value: integer, state: state_t)'); // 245
  DEH_AddAction(@A_GoToIfMapIntegerGreater, 'A_GoToIfMapIntegerGreater(parm: string, value: integer, state: state_t)'); // 246
  DEH_AddAction(@A_GoToIfMapFloatEqual, 'A_GoToIfMapFloatEqual(parm: string, value: float, state: state_t)'); // 247
  DEH_AddAction(@A_GoToIfMapFloatLess, 'A_GoToIfMapFloatLess(parm: string, value: float, state: state_t)'); // 248
  DEH_AddAction(@A_GoToIfMapFloatGreater, 'A_GoToIfMapFloatGreater(parm: string, value: float, state: state_t)'); // 249
  DEH_AddAction(@A_GoToIfWorldStringEqual, 'A_GoToIfWorldStringEqual(parm: string, value: string, state: state_t)'); // 250
  DEH_AddAction(@A_GoToIfWorldStringLess, 'A_GoToIfWorldStringLess(parm: string, value: string, state: state_t)'); // 251
  DEH_AddAction(@A_GoToIfWorldStringGreater, 'A_GoToIfWorldStringGreater(parm: string, value: string, state: state_t)'); // 252
  DEH_AddAction(@A_GoToIfWorldIntegerEqual, 'A_GoToIfWorldIntegerEqual(parm: string, value: integer, state: state_t)'); // 253
  DEH_AddAction(@A_GoToIfWorldIntegerLess, 'A_GoToIfWorldIntegerLess(parm: string, value: integer, state: state_t)'); // 254
  DEH_AddAction(@A_GoToIfWorldIntegerGreater, 'A_GoToIfWorldIntegerGreater(parm: string, value: integer, state: state_t)'); // 255
  DEH_AddAction(@A_GoToIfWorldFloatEqual, 'A_GoToIfWorldFloatEqual(parm: string, value: float, state: state_t)'); // 256
  DEH_AddAction(@A_GoToIfWorldFloatLess, 'A_GoToIfWorldFloatLess(parm: string, value: float, state: state_t)'); // 257
  DEH_AddAction(@A_GoToIfWorldFloatGreater, 'A_GoToIfWorldFloatGreater(parm: string, value: float, state: state_t)'); // 258
  DEH_AddAction(@A_PlayerMessage, 'A_PlayerMessage(msg1: string, [msg2: string], ...)'); // 259
  DEH_AddAction(@A_SetMapStr, 'A_SetMapStr(mvar: string; value1: string; [value2: string],...)'); // 260
  DEH_AddAction(@A_SetWorldStr, 'A_SetWorldStr(wvar: string; value1: string; [value2: string],...)'); // 261
  DEH_AddAction(@A_SetMapInt, 'A_SetMapInt(mvar: string; value: integer)'); // 262
  DEH_AddAction(@A_SetWorldInt, 'A_SetWorldInt(wvar: string; value: integer)'); // 263
  DEH_AddAction(@A_SetMapFloat, 'A_SetMapFloat(mvar: string; value: float)'); // 264
  DEH_AddAction(@A_SetWorldFloat, 'A_SetWorldFloat(wvar: string; value: float)'); // 265
  DEH_AddAction(@A_RandomGoto, 'A_RandomGoto(state1: state_t; [state2: state_t],...)'); // 266
  DEH_AddAction(@A_ResetHealth, 'A_ResetHealth()'); // 267
  DEH_AddAction(@A_Recoil, 'A_Recoil(xymom: float)'); // 268
  DEH_AddAction(@A_SetSolid, 'A_SetSolid()'); // 269
  DEH_AddAction(@A_UnSetSolid, 'A_UnSetSolid()'); // 270
  DEH_AddAction(@A_SetFloat, 'A_SetFloat()'); // 271
  DEH_AddAction(@A_UnSetFloat, 'A_UnSetFloat()'); // 272
  DEH_AddAction(@A_SetHealth, 'A_SetHealth(h: integer)'); // 273
  DEH_AddAction(@A_ResetTargetHealth, 'A_ResetTargetHealth()'); // 274
  DEH_AddAction(@A_SetTargetHealth, 'A_SetTargetHealth(h: integer)'); // 275
  DEH_AddAction(@A_ScaleVelocity, 'A_ScaleVelocity(scale: float)'); // 276
  DEH_AddAction(@A_ChangeVelocity, 'A_ChangeVelocity(velx: float, vely: float, velz: float, flags: float)'); // 277
  DEH_AddAction(@A_JumpIf, 'A_JumpIf(propability: boolean, offset1: integer, [offset2: integer], ...)'); // 278
  DEH_AddAction(@A_MusicChanger, 'A_MusicChanger()'); // 279
  DEH_AddAction(@A_SetPushFactor, 'A_SetPushFactor(f: float)'); // 280
  DEH_AddAction(@A_SetScale, 'A_SetScale(s: float)'); // 281
  DEH_AddAction(@A_SetGravity, 'A_SetGravity(g: float)'); // 282
  DEH_AddAction(@A_SetFloorBounce, 'A_SetFloorBounce()'); // 283
  DEH_AddAction(@A_UnSetFloorBounce, 'A_UnSetFloorBounce()'); // 284
  DEH_AddAction(@A_SetCeilingBounce, 'A_SetCeilingBounce()'); // 285
  DEH_AddAction(@A_UnSetCeilingBounce, 'A_UnSetCeilingBounce()'); // 286
  DEH_AddAction(@A_SetWallBounce, 'A_SetWallBounce()'); // 287
  DEH_AddAction(@A_UnSetWallBounce, 'A_UnSetWallBounce()'); // 288
  DEH_AddAction(@A_GlowLight, 'A_GlowLight(color: string)'); // 289
  DEH_AddAction(@A_TraceNearestPlayer, 'A_TraceNearestPlayer(pct: integer, [maxturn: angle_t])'); // 290
  DEH_AddAction(@A_ChangeFlag, 'A_ChangeFlag(flag: string, onoff: boolean)'); // 291
  DEH_AddAction(@A_CheckFloor, 'A_CheckFloor(offset: integer)'); // 292
  DEH_AddAction(@A_CheckCeiling, 'A_CheckCeiling(offset: integer)'); // 293
  DEH_AddAction(@A_StopSound, 'A_StopSound()'); // 294
  DEH_AddAction(@A_JumpIfTargetOutsideMeleeRange, 'A_JumpIfTargetOutsideMeleeRange(offset: integer)'); // 295
  DEH_AddAction(@A_JumpIfTargetInsideMeleeRange, 'A_JumpIfTargetInsideMeleeRange(offset: integer)'); // 296
  DEH_AddAction(@A_JumpIfTracerCloser, 'A_JumpIfTracerCloser(distancetotarget: float, offset: integer)'); // 297
  DEH_AddAction(@A_SetMass, 'A_SetMass(mass: integer)'); // 298
  DEH_AddAction(@A_SetTargetMass, 'A_SetTargetMass(mass: integer)'); // 299
  DEH_AddAction(@A_SetTracerMass, 'A_SetTracerMass(mass: integer)'); // 300
  DEH_AddAction(@A_CheckSight, 'A_CheckSight(offset: integer)'); // 301
  DEH_AddAction(@A_CheckSightOrRange, 'A_CheckSightOrRange(distance: float, offset: integer, [twodi: boolean=false])'); // 302
  DEH_AddAction(@A_CheckRange, 'A_CheckRange(distance: float, offset: integer, [twodi: boolean=false])'); // 303
  DEH_AddAction(@A_CountdownArg, 'A_CountdownArg(arg: integer, offset: integer)'); // 304
  DEH_AddAction(@A_SetArg, 'A_SetArg(arg: integer, value: integer)'); // 305
  DEH_AddAction(@A_SetSpecial, 'A_SetSpecial(special: integer, [arg1, arg2, arg3, arg4, arg5: integer])'); // 306
  DEH_AddAction(@A_CheckFlag, 'A_CheckFlag(flag: string, offset: integer, [aaprt: AAPTR])'); // 307
  DEH_AddAction(@A_SetAngle, 'A_SetAngle(angle: integer, [flags: integer], [aaprt: AAPTR])'); // 308
  DEH_AddAction(@A_SetUserVar, 'A_SetUserVar(varname: string, value: integer)'); // 309
  DEH_AddAction(@A_SetUserArray, 'A_SetUserArray(varname: string, index: integer, value: integer)'); // 310
  DEH_AddAction(@A_SetTics, 'A_SetTics(tics: integer)'); // 311
  DEH_AddAction(@A_DropItem, 'A_DropItem(spawntype: string, amount: integer, chance: integer)'); // 312
  DEH_AddAction(@A_DamageSelf, 'A_DamageSelf(actor: Pmobj_t)'); // 313
  DEH_AddAction(@A_DamageTarget, 'A_DamageTarget(const damage: integer)'); // 314
  DEH_AddAction(@A_DamageTracer, 'A_DamageTracer(const damage: integer)'); // 315
  DEH_AddAction(@A_KillTarget, 'A_KillTarget()'); // 316
  DEH_AddAction(@A_KillTracer, 'A_KillTracer()'); // 317
  DEH_AddAction(@A_RemoveTarget, 'A_RemoveTarget([flags: integer])'); // 318
  DEH_AddAction(@A_RemoveTracer, 'A_RemoveTracer([flags: integer])'); // 319
  DEH_AddAction(@A_Remove, 'A_Remove(aaprt: AAPTR, [flags: integer])'); // 320
  DEH_AddAction(@A_SetFloatBobPhase, 'A_SetFloatBobPhase(bob: integer)'); // 321
  DEH_AddAction(@A_Detonate, 'A_Detonate()'); // 322
  DEH_AddAction(@A_Spawn, 'A_Spawn()'); // 323
  DEH_AddAction(@A_Face, 'A_Face()'); // 324
  DEH_AddAction(@A_Scratch, 'A_Scratch()'); // 325
  DEH_AddAction(@A_RandomJump, 'A_RandomJump()'); // 326
  DEH_AddAction(@A_LineEffect, 'A_LineEffect()'); // 327
  DEH_AddAction(@A_FlipSprite, 'A_FlipSprite()'); // 328
  DEH_AddAction(@A_NoFlipSprite, 'A_NoFlipSprite()'); // 329
  DEH_AddAction(@A_RandomFlipSprite, 'A_RandomFlipSprite(chance: integer)'); // 330
  DEH_AddAction(@A_RandomNoFlipSprite, 'A_RandomNoFlipSprite(chance: integer)'); // 331
  DEH_AddAction(@A_CustomMeleeAttack, 'A_CustomMeleeAttack(damage: integer, meleesound: string, misssound: string)'); // 332
  DEH_AddAction(@A_CustomComboAttack, 'A_CustomComboAttack(missiletype: string, spawnheight: integer, damage: integer, meleesound: string)'); // 333
  DEH_AddAction(@A_SetRenderStyle, 'A_SetRenderStyle(style: renderstyle_t, alpha: float)'); // 334
  DEH_AddAction(@A_FadeTo, 'A_FadeTo(targ: integer, ammount: integer, flags: integer)'); // 335
  DEH_AddAction(@A_SetSize, 'A_SetSize(newradius: integer, newheight: integer, testpos: boolean)'); // 336
  DEH_AddAction(@A_RaiseMaster, 'A_RaiseMaster(copyfriendliness: boolean)'); // 337
  DEH_AddAction(@A_RaiseChildren, 'A_RaiseChildren(copyfriendliness: boolean)'); // 338
  DEH_AddAction(@A_RaiseSiblings, 'A_RaiseSiblings(copyfriendliness: boolean)'); // 339
  DEH_AddAction(@A_SetMasterMass, 'A_SetMasterMass(mass: integer)'); // 340
  DEH_AddAction(@A_KillMaster, 'A_KillMaster()'); // 341
  DEH_AddAction(@A_DamageMaster, 'A_DamageMaster(const damage: integer)'); // 342
  DEH_AddAction(@A_HealThing, 'A_HealThing(amount: integer, max: integer)'); // 343
  DEH_AddAction(@A_RemoveMaster, 'A_RemoveMaster([flags: integer])'); // 344
  DEH_AddAction(@A_BasicAttack, 'A_BasicAttack(MeleeDamage: integer, MeleeSound: integer, MissileType: integer, MissileHeight: float)'); // 345
  DEH_AddAction(@A_SetMasterArg, 'A_SetMasterArg(arg: integer; value: integer)'); // 346
  DEH_AddAction(@A_SetTargetArg, 'A_SetTargetArg(arg: integer; value: integer)'); // 347
  DEH_AddAction(@A_SetTracerArg, 'A_SetTracerArg(arg: integer; value: integer)'); // 348
  DEH_AddAction(@A_Tracer2, 'A_Tracer2()'); // 349
  DEH_AddAction(@A_MonsterRefire, 'A_MonsterRefire(prob: integer, offset: state_t)'); // 350
  DEH_AddAction(@A_RearrangePointers, 'A_RearrangePointers(ptr_target: integer, ptr_master: integer, ptr_tracer: integer, flags: integer)'); // 351
  DEH_AddAction(@A_TransferPointer, 'A_TransferPointer(ptr_source: integer, ptr_recipient: integer, ptr_sourcefield: integer, [ptr_recipientfield: integer], [flags: integer])'); // 352
  DEH_AddAction(@A_AlertMonsters, 'A_AlertMonsters(maxdist: integer, flags: integer)'); // 353
  DEH_AddAction(@A_LocalEarthQuake, 'A_LocalEarthQuake(tics: integer; [intensity: float = 1.0]; [maxdist: float = MAXINT] ;)'); // 354
  DEH_AddAction(@A_LocalEarthQuake, 'A_Quake(tics: integer; [intensity: float = 1.0]; [maxdist: float = MAXINT] ;)'); // 355
  DEH_AddAction(@A_RemoveChildren, 'A_RemoveChildren([flags: integer])'); // 356
  DEH_AddAction(@A_RemoveSiblings, 'A_RemoveSiblings([flags: integer])'); // 357
  DEH_AddAction(@A_KillChildren, 'A_KillChildren()'); // 358
  DEH_AddAction(@A_KillSiblings, 'A_KillSiblings()'); // 359
  DEH_AddAction(@A_Weave, 'A_Weave(xyspeed: integer = 2, zspeed: integer = 2, xydist: float = 2.0, zdist: float = 1.0)'); // 360
  DEH_AddAction(@A_SetWeaveIndexXY, 'A_SetWeaveIndexXY(weavexy: integer)'); // 361
  DEH_AddAction(@A_SetWeaveIndexZ, 'A_SetWeaveIndexZ(weavez: integer)'); // 362
  DEH_AddAction(@A_SetWeaveIndexes, 'A_SetWeaveIndexes(weavexy: integer, weavez: integer)'); // 363
  DEH_AddAction(@A_SetHeight, 'A_SetHeight(newheight: float)'); // 364
  DEH_AddAction(@A_OverlayClear, 'A_OverlayClear()'); // 365
  DEH_AddAction(@A_OverlayDrawPatch, 'A_OverlayDrawPatch(ticks: Integer; patchname: string; x, y: Integer ;)'); // 366
  DEH_AddAction(@A_OverlayDrawPatchStretched, 'A_OverlayDrawPatchStretched(ticks: Integer; patchname: string; x1, y1, x2, y2: Integer ;)'); // 367
  DEH_AddAction(@A_OverlayDrawPixel, 'A_OverlayDrawPixel(ticks: Integer; red, green, blue: byte; x, y: Integer ;)'); // 368
  DEH_AddAction(@A_OverlayDrawRect, 'A_OverlayDrawRect(ticks: Integer; red, green, blue: byte; x1, y1, x2, y2: Integer ;)'); // 369
  DEH_AddAction(@A_OverlayDrawLine, 'A_OverlayDrawLine(ticks: Integer; red, green, blue: byte; x1, y1, x2, y2: Integer ;)'); // 370
  DEH_AddAction(@A_OverlayDrawText, 'A_OverlayDrawText(ticks: Integer; txt: string; align: Integer; x, y: Integer ;)'); // 371
  DEH_AddAction(@A_OverlayDrawLeftText, 'A_OverlayDrawLeftText(ticks: Integer; txt: string; x, y: Integer ;)'); // 372
  DEH_AddAction(@A_OverlayDrawRightText, 'A_OverlayDrawRightText(ticks: Integer; txt: string; x, y: Integer ;)'); // 373
  DEH_AddAction(@A_OverlayDrawCenterText, 'A_OverlayDrawCenterText(ticks: Integer; txt: string; x, y: Integer ;)'); // 374
  DEH_AddAction(@A_SetFriction, 'A_SetFriction(newfriction: float)'); // 375
  DEH_AddAction(@A_PlayerHurtExplode, 'A_PlayerHurtExplode(damage: integer, radius: integer)'); // 376
  DEH_AddAction(@A_SetPushable, 'A_SetPushable()'); // 377
  DEH_AddAction(@A_UnSetPushable, 'A_UnSetPushable()'); // 378
  DEH_AddAction(@A_SetPainChance, 'A_SetPainChance(newchance: integer)'); // 379
  DEH_AddAction(@A_SetSpriteDX, 'A_SetSpriteDX(dx: float)'); // 380
  DEH_AddAction(@A_SetSpriteDY, 'A_SetSpriteDY(dy: float)'); // 381
  DEH_AddAction(@A_SeeSound1, 'A_SeeSound()'); // 382
  DEH_AddAction(@A_PainSound1, 'A_PainSound()'); // 383
  DEH_AddAction(@A_AttackSound1, 'A_AttackSound()'); // 384
  DEH_AddAction(@A_MeleeSound1, 'A_MeleeSound()'); // 385
  DEH_AddAction(@A_DeathSound1, 'A_DeathSound()'); // 386
  DEH_AddAction(@A_ActiveSound1, 'A_ActiveSound()'); // 387
  DEH_AddAction(@A_MatchTargetZ, 'A_MatchTargetZ(zspeed: integer; threshold: integer; maxmomz: integer)'); // 388
  DEH_AddAction(@A_SetInteractive, 'A_SetInteractive()'); // 389
  DEH_AddAction(@A_UnSetInteractive, 'A_UnSetInteractive()'); // 390
  DEH_AddAction(@A_SimpleDialog, 'A_SimpleDialog(dialog1: string; [dialog2...])'); // 391
  DEH_AddAction(@A_SetMasterCustomParam, 'A_SetMasterCustomParam(param: string, value: integer)');
  DEH_AddAction(@A_AddMasterCustomParam, 'A_AddMasterCustomParam(param: string, value: integer)');
  DEH_AddAction(@A_SubtractMasterCustomParam, 'A_SubtractMasterCustomParam(param: string, value: integer)');
  DEH_AddAction(@A_JumpIfMasterCustomParam, 'A_JumpIfMasterCustomParam(param: string, value: integer, offset: integer)');
  DEH_AddAction(@A_JumpIfMasterCustomParamLess, 'A_JumpIfMasterCustomParamLess(param: string, value: integer, offset: integer)');
  DEH_AddAction(@A_JumpIfMasterCustomParamGreater, 'A_JumpIfMasterCustomParamGreater(param: string, value: integer, offset: integer)');
  DEH_AddAction(@A_GoToIfMasterCustomParam, 'A_GoToIfMasterCustomParam(param: string, value: integer, state: state_t)');
  DEH_AddAction(@A_GoToIfMasterCustomParamLess, 'A_GoToIfMasterCustomParamLess(param: string, value: integer, state: state_t)');
  DEH_AddAction(@A_GoToIfMasterCustomParamGreater, 'A_GoToIfMasterCustomParamGreater(param: string, value: integer, state: state_t)');
  DEH_AddAction(@A_SetTracerCustomParam, 'A_SetTracerCustomParam(param: string, value: integer)');
  DEH_AddAction(@A_AddTracerCustomParam, 'A_AddTracerCustomParam(param: string, value: integer)');
  DEH_AddAction(@A_SubtractTracerCustomParam, 'A_SubtractTracerCustomParam(param: string, value: integer)');
  DEH_AddAction(@A_JumpIfTracerCustomParam, 'A_JumpIfTracerCustomParam(param: string, value: integer, offset: integer)');
  DEH_AddAction(@A_JumpIfTracerCustomParamLess, 'A_JumpIfTracerCustomParamLess(param: string, value: integer, offset: integer)');
  DEH_AddAction(@A_JumpIfTracerCustomParamGreater, 'A_JumpIfTracerCustomParamGreater(param: string, value: integer, offset: integer)');
  DEH_AddAction(@A_GoToIfTracerCustomParam, 'A_GoToIfTracerCustomParam(param: string, value: integer, state: state_t)');
  DEH_AddAction(@A_GoToIfTracerCustomParamLess, 'A_GoToIfTracerCustomParamLess(param: string, value: integer, state: state_t)');
  DEH_AddAction(@A_GoToIfTracerCustomParamGreater, 'A_GoToIfTracerCustomParamGreater(param: string, value: integer, state: state_t)');
  DEH_AddAction(@A_SetMonsterInfight, 'A_SetMonsterInfight()');
  DEH_AddAction(@A_UnSetMonsterInfight, 'A_UnSetMonsterInfight()');
  DEH_AddAction(@A_RipSound1, 'A_RipSound()');
  DEH_AddAction(@A_ChangeSpriteFlip, 'A_ChangeSpriteFlip(propability: integer)');
  DEH_AddAction(@A_SpawnObject, 'A_SpawnObject(dehackedtyp: integer; anglefixeddeg: integer; xoffs, yoffs, zoffs: integer; xvel, yvel, zvel: integer)', [0, 0, 0, 0, 0, 0, 0, 0]);
  DEH_AddAction(@A_MonsterProjectile, 'A_MonsterProjectile(dehackedtyp: integer; anglefixeddeg: integer; pitchfixeddeg: integer; xyoffs, zoffs: integer)', [0, 0, 0, 0, 0]);
  DEH_AddAction(@A_MonsterBulletAttack, 'A_MonsterBulletAttack(anglefixed_hspread, anglefixed_vspread: integer; nbullets: integer; basedamage: integer; damagemod: integer)', [0, 0, 1, 3, 5]);
  DEH_AddAction(@A_MonsterMeleeAttack, 'A_MonsterMeleeAttack(basedamage: integer; damagemod: integer; soundid: integer; fixxedrange: integer);', [3, 8, 0, 0]);
  DEH_AddAction(@A_RadiusDamage, 'A_RadiusDamage(damage: integer; radius: integer);', [0, 0]);
  DEH_AddAction(@A_NoiseAlert, 'A_NoiseAlert()');
  DEH_AddAction(@A_HealChase, 'A_HealChase(state: state_t; sound: sound_t);', [0, 0]);
  DEH_AddAction(@A_SeekTracer, 'A_SeekTracer(anglefixed_threshold: integer; anglefixed_maxturn: integer)', [0, 0]);
  DEH_AddAction(@A_FindTracer, 'A_FindTracer(anglefixed_fov: integer; mapblocks: integer);', [0, 10]);
  DEH_AddAction(@A_ClearTracer, 'A_ClearTracer();');
  DEH_AddAction(@A_JumpIfHealthBelow, 'A_JumpIfHealthBelow(state: state_t; health_threshold: integer)', [0, 0]);
  DEH_AddAction(@A_JumpIfTargetInSight, 'A_JumpIfTargetInSight(state: state_t; anglefixed_fov: integer)', [0, 0]);
  DEH_AddAction(@A_JumpIfTargetCloser, 'A_JumpIfTargetCloser(state: state_t; distance: fixed_t)', [0, 0]);
  DEH_AddAction(@A_JumpIfTracerInSight, 'A_JumpIfTracerInSight(state: state_t; anglefixed_fov: integer)', [0, 0]);
  DEH_AddAction(@A_JumpIfTracerCloserMBF21, 'A_JumpIfTracerCloserMBF21(state: state_t; distance: fixed_t)', [0, 0]);
  DEH_AddAction(@A_RandomRipSound, 'A_RandomRipSound();');
  DEH_AddAction(@A_JumpIfFlagsSet, 'A_JumpIfFlagsSet(state: state_t; bits, mbf21bits: integer);', [0, 0, 0]);
  DEH_AddAction(@A_AddFlags, 'A_AddFlags(bits, mbf21bits: integer);', [0, 0]);
  DEH_AddAction(@A_RemoveFlags, 'A_RemoveFlags(bits, mbf21bits: integer);', [0, 0]);
  DEH_AddAction(@A_WeaponProjectile, 'A_WeaponProjectile(typ: integer; anglefixed_ang: integer; anglefixed_pitch: integer; offs_xy: fixed_t; offs_z: fixed_t);', [0, 0, 0, 0, 0]);
  DEH_AddAction(@A_WeaponBulletAttack, 'A_WeaponBulletAttack(anglefixed_hspread, anglefixed_vspread: integer; numbullets: integer; attackdamage, attackmod: integer);', [0, 0, 1, 5, 3]);
  DEH_AddAction(@A_WeaponMeleeAttack, 'A_WeaponMeleeAttack(attackdamage, attackmod: integer; bersekdamagemul: fixed_t; sound: sound_t; range: fixed_t);', [2, 10, 1 * FRACUNIT, 0, 0]);
  DEH_AddAction(@A_WeaponSound, 'A_WeaponSound(sound: sound_t; fullvolume: integer);', [0, 0]);
  DEH_AddAction(@A_WeaponAlert, 'A_WeaponAlert();');
  DEH_AddAction(@A_WeaponJump, 'A_WeaponJump(statenum: integer; chance: integer);', [0, 0]);
  DEH_AddAction(@A_ConsumeAmmo, 'A_ConsumeAmmo(ammo: integer);', [0]);
  DEH_AddAction(@A_CheckAmmo, 'A_CheckAmmo(state: integer; minammo: integer);', [0, 0]);
  DEH_AddAction(@A_RefireTo, 'A_RefireTo(state: integer; skipammo: integer);', [0, 0]);
  DEH_AddAction(@A_GunFlashTo, 'A_GunFlashTo(state: integer; noplayerstatechange: integer);', [0, 0]);
  DEH_AddAction(@A_SetProjectileGroup, 'A_SetProjectileGroup(group: string);');
  DEH_AddAction(@A_SetInfightingGroup, 'A_SetInfightingGroup(group: string);');
  DEH_AddAction(@A_SetSplashGroup, 'A_SetSplashGroup(group: string);');
  DEH_AddAction(@A_Delayfire, 'A_Delayfire(tics: integer = TICRATE);');
  DEH_AddAction(@A_SetTranslation, 'A_SetTranslation(trans: string);');
  DEH_AddAction(@A_SetBloodColor, 'A_SetBloodColor(color: string);');

  for i := 0 to dehnumactions - 1 do
    DEH_AddActionToHash(deh_actions[i].name, i);

  weapontype_tokens := TDTextList.Create;
  for i := 0 to Ord(NUMWEAPONS) do
    weapontype_tokens.Add(strupper(GetENumName(TypeInfo(weapontype_t), i)));

  ammotype_tokens := TDTextList.Create;
  for i := 0 to Ord(NUMAMMO) + 1 do
    ammotype_tokens.Add(strupper(GetENumName(TypeInfo(ammotype_t), i)));

  deh_strings.numstrings := 0;
  deh_strings.realnumstrings := 0;
  deh_strings._array := nil;

  k := 0;
  for i := 1 to 5 do
    for j := 1 to 9 do
    begin
      DEH_AddString(@deh_strings, @mapnames[k], 'HUSTR_E' + itoa(i) + 'M' + itoa(j));
      inc(k);
    end;

  DEH_AddString(@deh_strings, @pg_CREDIT, 'PAGE_CREDIT');
  DEH_AddString(@deh_strings, @pg_TITLE, 'PAGE_TITLE');
  DEH_AddString(@deh_strings, @pg_ORDER, 'PAGE_ORDER');
  DEH_AddString(@deh_strings, @pg_HELP1, 'PAGE_HELP1');
  DEH_AddString(@deh_strings, @pg_HELP2, 'PAGE_HELP2');

  DEH_AddString(@deh_strings, @bgflatE1, 'BGFLATE1');
  DEH_AddString(@deh_strings, @bgflatE2, 'BGFLATE2');
  DEH_AddString(@deh_strings, @bgflatE3, 'BGFLATE3');
  DEH_AddString(@deh_strings, @bgflatE4, 'BGFLATE4');
  DEH_AddString(@deh_strings, @bgflatE5, 'BGFLATE5');
  DEH_AddString(@deh_strings, @EndLumpName, 'END_TEXT_LUMP');
  for i := 0 to 9 do
  begin
    DEH_AddString(@deh_strings, @chat_macros[i], 'HUSTR_CHATMACR' + itoa(i));
    DEH_AddString(@deh_strings, @chat_macros[i], 'HUSTR_CHATMACR' + IntToStrZFill(2, i));
  end;

  DEH_AddString(@deh_strings, @QUITMSG, 'QUITMSG');

  for i := 0 to NUM_STARTUPMESSAGES - 1 do
  begin
    DEH_AddString(@deh_strings, @startmsg[i], 'STARTUP' + itoa(i));
    DEH_AddString(@deh_strings, @startmsg[i], 'STARTUP' + IntToStrZFill(2, i));
  end;

  DEH_AddString(@deh_strings, @HUSTR_TALKTOSELF1, 'HUSTR_TALKTOSELF1');
  DEH_AddString(@deh_strings, @HUSTR_TALKTOSELF2, 'HUSTR_TALKTOSELF2');
  DEH_AddString(@deh_strings, @HUSTR_TALKTOSELF3, 'HUSTR_TALKTOSELF3');
  DEH_AddString(@deh_strings, @HUSTR_TALKTOSELF4, 'HUSTR_TALKTOSELF4');
  DEH_AddString(@deh_strings, @HUSTR_TALKTOSELF5, 'HUSTR_TALKTOSELF5');
  DEH_AddString(@deh_strings, @HUSTR_MESSAGESENT, 'HUSTR_MESSAGESENT');
  DEH_AddString(@deh_strings, @HUSTR_MSGU, 'HUSTR_MSGU');

  DEH_AddString(@deh_strings, @HUSTR_PLRGREEN, 'HUSTR_PLRGREEN');
  DEH_AddString(@deh_strings, @HUSTR_PLRINDIGO, 'HUSTR_PLRINDIGO');
  DEH_AddString(@deh_strings, @HUSTR_PLRBROWN, 'HUSTR_PLRBROWN');
  DEH_AddString(@deh_strings, @HUSTR_PLRRED, 'HUSTR_PLRRED');
  DEH_AddString(@deh_strings, @destination_keys[0], 'HUSTR_KEYGREEN');
  DEH_AddString(@deh_strings, @destination_keys[1], 'HUSTR_KEYINDIGO');
  DEH_AddString(@deh_strings, @destination_keys[2], 'HUSTR_KEYBROWN');
  DEH_AddString(@deh_strings, @destination_keys[3], 'HUSTR_KEYRED');

  DEH_AddString(@deh_strings, @AMSTR_FOLLOWON, 'AMSTR_FOLLOWON');
  DEH_AddString(@deh_strings, @AMSTR_FOLLOWOFF, 'AMSTR_FOLLOWOFF');
  DEH_AddString(@deh_strings, @AMSTR_GRIDON, 'AMSTR_GRIDON');
  DEH_AddString(@deh_strings, @AMSTR_GRIDOFF, 'AMSTR_GRIDOFF');
  DEH_AddString(@deh_strings, @AMSTR_ROTATEON, 'AMSTR_ROTATEON');
  DEH_AddString(@deh_strings, @AMSTR_ROTATEOFF, 'AMSTR_ROTATEOFF');
  DEH_AddString(@deh_strings, @AMSTR_MARKEDSPOT, 'AMSTR_MARKEDSPOT');
  DEH_AddString(@deh_strings, @AMSTR_MARKSCLEARED, 'AMSTR_MARKSCLEARED');

  DEH_AddString(@deh_strings, @STSTR_MUS, 'STSTR_MUS');
  DEH_AddString(@deh_strings, @STSTR_NOMUS, 'STSTR_NOMUS');
  DEH_AddString(@deh_strings, @STSTR_KFAADDED, 'STSTR_KFAADDED');
  DEH_AddString(@deh_strings, @STSTR_FAADDED, 'STSTR_FAADDED');
  DEH_AddString(@deh_strings, @STSTR_NCON, 'STSTR_NCON');
  DEH_AddString(@deh_strings, @STSTR_NCOFF, 'STSTR_NCOFF');
  DEH_AddString(@deh_strings, @STSTR_CHOPPERS, 'STSTR_CHOPPERS');
  DEH_AddString(@deh_strings, @STSTR_CLEV, 'STSTR_CLEV');
  DEH_AddString(@deh_strings, @STSTR_LGON, 'STSTR_LGON');
  DEH_AddString(@deh_strings, @STSTR_LGOFF, 'STSTR_LGOFF');

  DEH_AddString(@deh_strings, @E1TEXT, 'E1TEXT');
  DEH_AddString(@deh_strings, @E2TEXT, 'E2TEXT');
  DEH_AddString(@deh_strings, @E3TEXT, 'E3TEXT');
  DEH_AddString(@deh_strings, @E4TEXT, 'E4TEXT');
  DEH_AddString(@deh_strings, @E5TEXT, 'E5TEXT');

  DEH_AddString(@deh_strings, @TXT_GOTBLUEKEY, 'TXT_GOTBLUEKEY');
  DEH_AddString(@deh_strings, @TXT_GOTYELLOWKEY, 'TXT_GOTYELLOWKEY');
  DEH_AddString(@deh_strings, @TXT_GOTGREENKEY, 'TXT_GOTGREENKEY');

// Artifacts

  DEH_AddString(@deh_strings, @TXT_ARTIHEALTH, 'TXT_ARTIHEALTH');
  DEH_AddString(@deh_strings, @TXT_ARTIFLY, 'TXT_ARTIFLY');
  DEH_AddString(@deh_strings, @TXT_ARTIINVULNERABILITY, 'TXT_ARTIINVULNERABILITY');
  DEH_AddString(@deh_strings, @TXT_ARTITOMEOFPOWER, 'TXT_ARTITOMEOFPOWER');
  DEH_AddString(@deh_strings, @TXT_ARTIINVISIBILITY, 'TXT_ARTIINVISIBILITY');
  DEH_AddString(@deh_strings, @TXT_ARTIEGG, 'TXT_ARTIEGG');
  DEH_AddString(@deh_strings, @TXT_ARTISUPERHEALTH, 'TXT_ARTISUPERHEALTH');
  DEH_AddString(@deh_strings, @TXT_ARTITORCH, 'TXT_ARTITORCH');
  DEH_AddString(@deh_strings, @TXT_ARTIFIREBOMB, 'TXT_ARTIFIREBOMB');
  DEH_AddString(@deh_strings, @TXT_ARTITELEPORT, 'TXT_ARTITELEPORT');
  DEH_AddString(@deh_strings, @TXT_ARTIALL, 'TXT_ARTIALL');

// Items

  DEH_AddString(@deh_strings, @TXT_ITEMHEALTH, 'TXT_ITEMHEALTH');
  DEH_AddString(@deh_strings, @TXT_ITEMBAGOFHOLDING, 'TXT_ITEMBAGOFHOLDING');
  DEH_AddString(@deh_strings, @TXT_ITEMSHIELD1, 'TXT_ITEMSHIELD1');
  DEH_AddString(@deh_strings, @TXT_ITEMSHIELD2, 'TXT_ITEMSHIELD2');
  DEH_AddString(@deh_strings, @TXT_ITEMSUPERMAP, 'TXT_ITEMSUPERMAP');

// Ammo

  DEH_AddString(@deh_strings, @TXT_AMMOGOLDWAND1, 'TXT_AMMOGOLDWAND1');
  DEH_AddString(@deh_strings, @TXT_AMMOGOLDWAND2, 'TXT_AMMOGOLDWAND2');
  DEH_AddString(@deh_strings, @TXT_AMMOMACE1, 'TXT_AMMOMACE1');
  DEH_AddString(@deh_strings, @TXT_AMMOMACE2, 'TXT_AMMOMACE2');
  DEH_AddString(@deh_strings, @TXT_AMMOCROSSBOW1, 'TXT_AMMOCROSSBOW1');
  DEH_AddString(@deh_strings, @TXT_AMMOCROSSBOW2, 'TXT_AMMOCROSSBOW2');
  DEH_AddString(@deh_strings, @TXT_AMMOBLASTER1, 'TXT_AMMOBLASTER1');
  DEH_AddString(@deh_strings, @TXT_AMMOBLASTER2, 'TXT_AMMOBLASTER2');
  DEH_AddString(@deh_strings, @TXT_AMMOSKULLROD1, 'TXT_AMMOSKULLROD1');
  DEH_AddString(@deh_strings, @TXT_AMMOSKULLROD2, 'TXT_AMMOSKULLROD2');
  DEH_AddString(@deh_strings, @TXT_AMMOPHOENIXROD1, 'TXT_AMMOPHOENIXROD1');
  DEH_AddString(@deh_strings, @TXT_AMMOPHOENIXROD2, 'TXT_AMMOPHOENIXROD2');

// Weapons

  DEH_AddString(@deh_strings, @TXT_WPNMACE, 'TXT_WPNMACE');
  DEH_AddString(@deh_strings, @TXT_WPNCROSSBOW, 'TXT_WPNCROSSBOW');
  DEH_AddString(@deh_strings, @TXT_WPNBLASTER, 'TXT_WPNBLASTER');
  DEH_AddString(@deh_strings, @TXT_WPNSKULLROD, 'TXT_WPNSKULLROD');
  DEH_AddString(@deh_strings, @TXT_WPNPHOENIXROD, 'TXT_WPNPHOENIXROD');
  DEH_AddString(@deh_strings, @TXT_WPNGAUNTLETS, 'TXT_WPNGAUNTLETS');

  DEH_AddString(@deh_strings, @TXT_CHEATGODON, 'TXT_CHEATGODON');
  DEH_AddString(@deh_strings, @TXT_CHEATGODOFF, 'TXT_CHEATGODOFF');
  DEH_AddString(@deh_strings, @TXT_CHEATNOCLIPON, 'TXT_CHEATNOCLIPON');
  DEH_AddString(@deh_strings, @TXT_CHEATNOCLIPOFF, 'TXT_CHEATNOCLIPOFF');
  DEH_AddString(@deh_strings, @TXT_CHEATWEAPONS, 'TXT_CHEATWEAPONS');
  DEH_AddString(@deh_strings, @TXT_CHEATKEYS, 'TXT_CHEATKEYS');
  DEH_AddString(@deh_strings, @TXT_CHEATFLIGHTON, 'TXT_CHEATFLIGHTON');
  DEH_AddString(@deh_strings, @TXT_CHEATFLIGHTOFF, 'TXT_CHEATFLIGHTOFF');
  DEH_AddString(@deh_strings, @TXT_CHEATPOWERON, 'TXT_CHEATPOWERON');
  DEH_AddString(@deh_strings, @TXT_CHEATPOWEROFF, 'TXT_CHEATPOWEROFF');
  DEH_AddString(@deh_strings, @TXT_CHEATHEALTH, 'TXT_CHEATHEALTH');
  DEH_AddString(@deh_strings, @TXT_CHEATSOUNDON, 'TXT_CHEATSOUNDON');
  DEH_AddString(@deh_strings, @TXT_CHEATSOUNDOFF, 'TXT_CHEATSOUNDOFF');
  DEH_AddString(@deh_strings, @TXT_CHEATTICKERON, 'TXT_CHEATTICKERON');
  DEH_AddString(@deh_strings, @TXT_CHEATTICKEROFF, 'TXT_CHEATTICKEROFF');
  DEH_AddString(@deh_strings, @TXT_CHEATARTIFACTS1, 'TXT_CHEATARTIFACTS1');
  DEH_AddString(@deh_strings, @TXT_CHEATARTIFACTS2, 'TXT_CHEATARTIFACTS2');
  DEH_AddString(@deh_strings, @TXT_CHEATARTIFACTS3, 'TXT_CHEATARTIFACTS3');
  DEH_AddString(@deh_strings, @TXT_CHEATARTIFACTSFAIL, 'TXT_CHEATARTIFACTSFAIL');
  DEH_AddString(@deh_strings, @TXT_CHEATWARP, 'TXT_CHEATWARP');
  DEH_AddString(@deh_strings, @TXT_CHEATSCREENSHOT, 'TXT_CHEATSCREENSHOT');
  DEH_AddString(@deh_strings, @TXT_CHEATCHICKENON, 'TXT_CHEATCHICKENON');
  DEH_AddString(@deh_strings, @TXT_CHEATCHICKENOFF, 'TXT_CHEATCHICKENOFF');
  DEH_AddString(@deh_strings, @TXT_CHEATMASSACRE, 'TXT_CHEATMASSACRE');
  DEH_AddString(@deh_strings, @TXT_CHEATIDDQD, 'TXT_CHEATIDDQD');
  DEH_AddString(@deh_strings, @TXT_CHEATIDKFA, 'TXT_CHEATIDKFA');

  DEH_AddString(@deh_strings, @MSG_MODIFIEDGAME, 'MSG_MODIFIEDGAME');
  DEH_AddString(@deh_strings, @MSG_SHAREWARE, 'MSG_SHAREWARE');
  DEH_AddString(@deh_strings, @MSG_COMMERCIAL, 'MSG_COMMERCIAL');
  DEH_AddString(@deh_strings, @MSG_UNDETERMINED, 'MSG_UNDETERMINED');

  DEH_AddString(@deh_strings, @MSGSECRETSECTOR, 'MSGSECRETSECTOR');

  DEH_AddString(@deh_strings, @DETAILULTRA, 'DETAILULTRA');
  DEH_AddString(@deh_strings, @DETAILHI, 'DETAILHI');
  DEH_AddString(@deh_strings, @DETAILHI, 'DETAILHIGH');
  DEH_AddString(@deh_strings, @DETAILNORM, 'DETAILNORM');
  DEH_AddString(@deh_strings, @DETAILNORM, 'DETAILNORMAL');
  DEH_AddString(@deh_strings, @DETAILMED, 'DETAILMED');
  DEH_AddString(@deh_strings, @DETAILMED, 'DETAILMEDIUM');
  DEH_AddString(@deh_strings, @DETAILLOW, 'DETAILLO');
  DEH_AddString(@deh_strings, @DETAILLOW, 'DETAILLOW');
  DEH_AddString(@deh_strings, @DETAILLOWEST, 'DETAILLOWEST');

  DEH_AddString(@deh_strings, @GAMMALVL0, 'GAMMALVL0');
  DEH_AddString(@deh_strings, @GAMMALVL1, 'GAMMALVL1');
  DEH_AddString(@deh_strings, @GAMMALVL2, 'GAMMALVL2');
  DEH_AddString(@deh_strings, @GAMMALVL3, 'GAMMALVL3');
  DEH_AddString(@deh_strings, @GAMMALVL4, 'GAMMALVL4');

  DEH_AddString(@deh_strings, @EMPTYSTRING, 'EMPTYSTRING');

  DEH_AddString(@deh_strings, @D_DEVSTR, 'D_DEVSTR');
  DEH_AddString(@deh_strings, @D_CDROM, 'D_CDROM');

  DEH_AddString(@deh_strings, @PRESSKEY, 'PRESSKEY');
  DEH_AddString(@deh_strings, @PRESSYN, 'PRESSYN');
  DEH_AddString(@deh_strings, @LOADNET, 'LOADNET');
  DEH_AddString(@deh_strings, @QLOADNET, 'QLOADNET');
  DEH_AddString(@deh_strings, @QSAVESPOT, 'QSAVESPOT');
  DEH_AddString(@deh_strings, @SAVEDEAD, 'SAVEDEAD');
  DEH_AddString(@deh_strings, @QSPROMPT, 'QSPROMPT');
  DEH_AddString(@deh_strings, @QLPROMPT, 'QLPROMPT');
  DEH_AddString(@deh_strings, @SNEWGAME, 'SNEWGAME');
  DEH_AddString(@deh_strings, @SNIGHTMARE, 'SNIGHTMARE');
  DEH_AddString(@deh_strings, @SWSTRING, 'SWSTRING');
  DEH_AddString(@deh_strings, @MSGOFF, 'MSGOFF');
  DEH_AddString(@deh_strings, @MSGON, 'MSGON');
  DEH_AddString(@deh_strings, @NETEND, 'NETEND');
  DEH_AddString(@deh_strings, @SENDGAME, 'SENDGAME');

  DEH_AddString(@deh_strings, @DOSY, 'DOSY');

  DEH_AddString(@deh_strings, @GGSAVED, 'GGSAVED');
  DEH_AddString(@deh_strings, @SAVEGAMENAME, 'SAVEGAMENAME');

  ammo_tokens := TDTextList.Create;

  ammo_tokens.Add('MAX AMMO');

  weapon_tokens := TDTextList.Create;

  weapon_tokens.Add('AMMO TYPE');     // .ammo
  weapon_tokens.Add('DESELECT FRAME');// .upstate
  weapon_tokens.Add('SELECT FRAME');  // .downstate
  weapon_tokens.Add('BOBBING FRAME'); // .readystate
  weapon_tokens.Add('SHOOTING FRAME');// .atkstate
  weapon_tokens.Add('FIRING FRAME');  // .flashstate
  weapon_tokens.Add('HOLD SHOOTING FRAME');// .holdatkstate
  weapon_tokens.Add('LEVEL');         // level1 or level2
  weapon_tokens.Add('AMMO PER SHOT'); // .ammopershot (MBF21)
  weapon_tokens.Add('MBF21 BITS');    // .mbf21bits (MBF21)

  // MBF21
  weapon_flags_mbf21 := TDTextList.Create;
  weapon_flags_mbf21.Add('NOTHRUST');
  weapon_flags_mbf21.Add('SILENT');
  weapon_flags_mbf21.Add('NOAUTOFIRE');
  weapon_flags_mbf21.Add('FLEEMELEE');
  weapon_flags_mbf21.Add('AUTOSWITCHFROM');
  weapon_flags_mbf21.Add('NOAUTOSWITCHTO');

  sound_tokens := TDTextList.Create;

  sound_tokens.Add('ZERO/ONE');
  sound_tokens.Add('VALUE');
  sound_tokens.Add('NAME'); // DelphiDoom specific

  renderstyle_tokens := TDTextList.Create;

  renderstyle_tokens.Add('NORMAL');
  renderstyle_tokens.Add('TRANSLUCENT');
  renderstyle_tokens.Add('ADD');
  renderstyle_tokens.Add('SUBTRACT');

  misc_tokens := TDTextList.Create;

  misc_tokens.Add('MAX HEALTH');          // p_maxhealth
  misc_tokens.Add('SOULSPHERE HEALTH');   // p_soulspherehealth
  misc_tokens.Add('MEGASPHERE HEALTH');   // p_megaspherehealth
  misc_tokens.Add('MEDIKIT HEALTH');      // p_medikithealth
  misc_tokens.Add('STIMPACK HEALTH');     // p_stimpackhealth
  misc_tokens.Add('HEALTHBONUS HEALTH');  // p_bonushealth
  misc_tokens.Add('MAX ARMOR');           // p_maxarmor
  misc_tokens.Add('GREEN ARMOR CLASS');   // p_greenarmorclass
  misc_tokens.Add('BLUE ARMOR CLASS');    // p_bluearmorclass
  misc_tokens.Add('MAX CHICKEN HEALTH');  // p_maxchickenhealth
  misc_tokens.Add('MAX ARTIFACTS');       // p_maxartifacts

  C_AddCmd('DEH_ParseFile, BEX_ParseFile', @DEH_ParseFile);
  C_AddCmd('DEH_ParseLump, BEX_ParseLump', @DEH_ParseLumpName);
  C_AddCmd('DEH_PrintCurrentSettings, DEH_PrintSettings, BEX_PrintCurrentSettings, BEX_PrintSettings', @DEH_PrintCurrentSettings);
  C_AddCmd('DEH_SaveCurrentSettings, DEH_SaveToFile, BEX_SaveCurrentSettings, BEX_SaveToFile', @DEH_SaveCurrentSettings);
  C_AddCmd('DEH_SaveMobjInfoCSV, BEX_SaveMobjInfoCSV', @DEH_SaveMobjInfoCSV);
  C_AddCmd('DEH_SaveStatesCSV, BEX_SaveStatesCSV', @DEH_SaveStatesCSV);
  C_AddCmd('DEH_SaveSpritesCSV, BEX_SaveSpritesCSV', @DEH_SaveSpritesCSV);
  C_AddCmd('DEH_PrintActions, DEH_ShowActions, BEX_PrintActions, BEX_ShowActions', @DEH_PrintActions);
end;

//==============================================================================
//
// DEH_ShutDown
//
//==============================================================================
procedure DEH_ShutDown;
begin
  if not deh_initialized then
    exit;

  FreeAndNil(mobj_tokens);
  FreeAndNil(mobj_flags);
  FreeAndNil(mobj_flags2);
  FreeAndNil(mobj_flags_ex);
  FreeAndNil(mobj_flags2_ex);
  FreeAndNil(mobj_flags3_ex);
  FreeAndNil(mobj_flags4_ex);
  FreeAndNil(mobj_flags5_ex);
  FreeAndNil(mobj_flags6_ex);
  FreeAndNil(mobj_flags_mbf21); // MBF21
  FreeAndNil(state_flags_ex);
  FreeAndNil(state_flags_mbf21);  // MBF21
  FreeAndNil(state_tokens);
  FreeAndNil(ammo_tokens);
  FreeAndNil(weapon_tokens);
  FreeAndNil(weapon_flags_mbf21); // MBF21
  FreeAndNil(sound_tokens);
  FreeAndNil(renderstyle_tokens);
  FreeAndNil(misc_tokens);
  FreeAndNil(weapontype_tokens);
  FreeAndNil(ammotype_tokens);
  // MBF21
  FreeAndNil(infighting_groups);
  FreeAndNil(projectile_groups);
  FreeAndNil(splash_groups);

  FreeAndNil(mobj_tokens_hash);
  FreeAndNil(mobj_flags_hash);
  FreeAndNil(mobj_flags2_hash);
  FreeAndNil(mobj_flags_ex_hash);
  FreeAndNil(mobj_flags2_ex_hash);
  FreeAndNil(mobj_flags3_ex_hash);
  FreeAndNil(mobj_flags4_ex_hash);
  FreeAndNil(mobj_flags5_ex_hash);
  FreeAndNil(mobj_flags6_ex_hash);
  FreeAndNil(mobj_flags_mbf21_hash); // MBF21

  DEH_ShutDownActionsHash;

  realloc(pointer(deh_strings._array), deh_strings.realnumstrings * SizeOf(deh_string_t), 0);
end;

end.

