//------------------------------------------------------------------------------
//
//  DelphiHexen: A modified and improved Hexen port for Windows
//  based on original Linux Doom as published by "id Software", on
//  Hexen source as published by "Raven" software and DelphiDoom
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
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit deh_main;

interface

// JVAL
// Support for DeHackEd patch files
// Support for both DEH and BEX files

uses
  d_delphi,
  d_think;

procedure DEH_Parse(const s: TDStringList);

function DEH_CurrentSettings: TDStringList;

procedure DEH_Init;

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
    {$IFDEF DLL}decl: string;{$ENDIF}
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
  state_tokens: TDTextList;
  state_flags_ex: TDTextList;
  weapon_tokens: TDTextList;
  sound_tokens: TDTextList;
  renderstyle_tokens: TDTextList;
  misc_tokens: TDTextList;
  weapontype_tokens: TDTextList;
  ammotype_tokens: TDTextList;
  playerclass_tokens: TDTextList;

  deh_actions: array[0..DEHMAXACTIONS - 1] of deh_action_t;
  deh_strings: deh_strings_t;

implementation

uses
  TypInfo,
  c_cmds,
  doomdef,
  deh_base,
  d_main,
  xn_strings,
  f_finale,
  g_game,
  hu_stuff,
  i_system,
  info_h,
  info,
  info_common,
  m_argv,
  m_fixed,
  a_action,
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
  p_user,
  p_musinfo,
  p_sounds,
  p_simpledialog,
  psi_overlay,
  r_renderstyle,
  sounds,
  sc_params,
  sc_engine,
  sc_states,
  v_data,
  sv_save,
  w_wad,
  w_folders,
  w_pak,
  z_zone;

var
  mobj_tokens_hash: TDEHStringsHashTable;
  mobj_flags_hash: TDEHStringsHashTable;
  mobj_flags2_hash: TDEHStringsHashTable;
  mobj_flags_ex_hash: TDEHStringsHashTable;
  mobj_flags2_ex_hash: TDEHStringsHashTable;
  mobj_flags3_ex_hash: TDEHStringsHashTable;
  mobj_flags4_ex_hash: TDEHStringsHashTable;

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
  fw: string;
  settext: string;
  mustnextline: boolean;

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
  weapon_class: integer;
  weapon_idx: integer;
  weapon_val: integer;

  sound_no: integer;
  sound_idx: integer;
  sound_val: integer;

  music_idx: integer;

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

    splitstring(str, token1, token2);

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

      while true do
      begin
        if not DEH_NextLine(s, str, i) then
          break;

        if Pos('=', str) = 0 then
        begin
          mustnextline := false; // Already got line
          break;
        end;

      // Retrieve current think field index
        splitstring(str, token1, token2, '=');
        token2 := RemoveQuotesFromString(token2);
        mobj_idx := mobj_tokens_hash.IndexOf(token1);

        if mobj_idx = -1 then
        begin
          I_Warning('DEH_Parse(): Wrong think field = %s (Think number = %d)'#13#10, [token1, mobj_no]);
          mustnextline := false; // Already got line
          break;
        end;

        mobj_val := atoi(token2, -1);

        if mobj_idx in [1, 3, 7, 10, 11, 12, 13, 23, 38, 64] then
        begin
          stmp := firstword(token2);
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
          16: mobjinfo[mobj_no].radius := mobj_val;
          17: mobjinfo[mobj_no].height := mobj_val;
          18: mobjinfo[mobj_no].mass := mobj_val;
          19: mobjinfo[mobj_no].damage := mobj_val;
          20: mobjinfo[mobj_no].activesound := S_GetSoundNumForName(token2);
          21: begin
                if mobj_val >= 0 then
                  mobjinfo[mobj_no].flags := mobj_val
                else
                begin
                  mobj_setflag := -1;
                  repeat
                    splitstring(token2, token3, token4, [' ', '|', ',', '+']);
                    token3 := strtrim(token3);
                    mobj_flag := mobj_flags_hash.IndexOf('MF_' + token3);
                    if mobj_flag = -1 then
                      mobj_flag := mobj_flags_hash.IndexOf(token3);
                    if mobj_flag >= 0 then
                    begin
                      if mobj_flag = 31 then
                        mobjinfo[mobj_no].flags_ex := mobjinfo[mobj_no].flags_ex or MF_EX_TRANSPARENT
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
                    mobjinfo[mobj_no].flags := mobj_setflag;

                end;
              end;
          22: begin
                if mobj_val >= 0 then
                  mobjinfo[mobj_no].flags2 := mobj_val
                else
                begin
                  mobj_setflag := -1;
                  repeat
                    splitstring(token2, token3, token4, [' ', '|', ',', '+']);
                    token3 := strtrim(token3);
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
                if mobj_val >= 0 then
                  mobjinfo[mobj_no].flags_ex := mobj_val  // DelphiHexen specific (lighting, transparency, etc)
                else
                begin
                  mobj_setflag := -1;
                  repeat
                    splitstring(token2, token3, token4, [' ', '|', ',', '+']);
                    token3 := strtrim(token3);
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
          36: mobjinfo[mobj_no].dropitem := Info_GetMobjNumForName(token2);
          37: mobjinfo[mobj_no].missiletype := Info_GetMobjNumForName(token2);
          38: mobjinfo[mobj_no].healstate := mobj_val;
          39: begin
                if mobj_val >= 0 then
                  mobjinfo[mobj_no].flags2_ex := mobj_val  // DelphiHexen specific
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
                if mobj_val >= 0 then
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
                if mobj_val >= 0 then
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
        end;
      end;

    end



    ////////////////////////////////////////////////////////////////////////////
    else if (token1 = 'FRAME') or (token1 = 'STATE') then
    begin
    ////////////////////////////////////////////////////////////////////////////
    // Parse a frame ///////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
      stmp := firstword(token2);
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
          I_Warning('DEH_Parse(): New state number unspecified after %s keyword'#13#10, [firstword(token2)]);
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
        if Pos('=', str) = 0 then
        begin
          mustnextline := false; // Already got line
          break;
        end;

        splitstring(str, token1, token2, '=');
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
              if firstword(token2) = 'BRIGHT' then
                states[state_no].frame := FF_FULLBRIGHT + atoi(secondword(token2))
              else
                states[state_no].frame := state_val;
            end;
           2: states[state_no].tics := state_val;
           3:
            begin
              fw := firstword(token2);
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
                if state_val >= 0 then
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
        end;
      end;
    end




    ////////////////////////////////////////////////////////////////////////////
    else if token1 = 'TEXT' then
    begin
    ////////////////////////////////////////////////////////////////////////////
    // Parse a text ////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
      stmp := token2;
      splitstring(stmp, token1, token2);

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
              sprnames[j] := Ord(token2[1]) +
                             Ord(token2[2]) shl 8 +
                             Ord(token2[3]) shl 16 +
                             Ord(token2[4]) shl 24;
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
      splitstring(stmp, token1, token2);
      if token1 <> 'FRAME' then
        continue;

      state_no := atoi(token2, -1);
      if state_no < 0 then
        continue;

      if not DEH_NextLine(s, str, i) then
        break;

      if Pos('=', str) = 0 then
      begin
        mustnextline := false; // Already got line
        continue;
      end;

      splitstring(str, token1, token2, '=');
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
      stmp := firstword(token2);
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

        if Pos('=', str) = 0 then
        begin
          mustnextline := false; // Already got line
          break;
        end;

      // Retrieve current think field index
        splitstring(str, token1, token2, '=');
        sound_idx := sound_tokens.IndexOf(token1);

        if sound_idx = -1 then
        begin
          I_Warning('DEH_Parse(): Wrong sound field = %s (Sound number = %d)'#13#10, [token1, sound_no]);
          mustnextline := false; // Already got line
          break;
        end;

        sound_val := atoi(token2);

        case sound_idx of
           0: S_sfx[sound_no].priority := sound_val;
           1: S_sfx[sound_no].name := token2;
        end;
      end;
    end




    ////////////////////////////////////////////////////////////////////////////
    else if token1 = 'MAX MANA' then
    begin
    ////////////////////////////////////////////////////////////////////////////
    // Parse ammo //////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
      stmp := firstword(token2);
      if stmp = '' then
        continue;

      ammo_val := atoi(stmp, -1);
      if ammo_val > 0 then
        MAX_MANA := ammo_val;
    end




    ////////////////////////////////////////////////////////////////////////////
    else if token1 = 'WEAPON' then
    begin
    ////////////////////////////////////////////////////////////////////////////
    // Parse weapon ////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
      stmp := firstword(token2);
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

      stmp := secondword(token2);
      if stmp = '' then
        continue;

      weapon_class := atoi(stmp, -1);
      if weapon_class < 0 then
        weapon_class := DEH_PlayerClass(stmp);

      while true do
      begin
        if not DEH_NextLine(s, str, i) then
          break;

        if Pos('=', str) = 0 then
        begin
          mustnextline := false; // Already got line
          break;
        end;

      // Retrieve current think field index
        splitstring(str, token1, token2, '=');
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
        else if weapon_val < 0 then
        begin
          if weapon_idx in [1, 2, 3, 4, 5, 6] then
          begin
            stmp := firstword(token2);
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
          else
          begin
            I_Warning('DEH_Parse(): Invalid state number (%s) (Weapon number = %d)'#13#10, [token2, weapon_no]);
            continue;
          end;

        end;


        case weapon_idx of
           0: WeaponInfo[weapon_no, weapon_class].mana := manatype_t(weapon_val);
           1: WeaponInfo[weapon_no, weapon_class].upstate := weapon_val;
           2: WeaponInfo[weapon_no, weapon_class].downstate := weapon_val;
           3: WeaponInfo[weapon_no, weapon_class].readystate := weapon_val;
           4: WeaponInfo[weapon_no, weapon_class].atkstate := weapon_val;
           6: WeaponInfo[weapon_no, weapon_class].holdatkstate := weapon_val;
           5: WeaponInfo[weapon_no, weapon_class].flashstate := weapon_val;
        end;
      end;

    end




    ////////////////////////////////////////////////////////////////////////////
    else if token1 = 'SPRITE' then
    begin
    ////////////////////////////////////////////////////////////////////////////
    // Parse sprite ////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
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

        if Pos('=', str) = 0 then
        begin
          mustnextline := false; // Already got line
          break;
        end;

      // Retrieve current think field index
        splitstring(str, token1, token2, '=');

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
          0: p_maxmorphhealth := misc_val;    // 'MAX MORPH HEALTH'
          1: p_maxartifacts := misc_val;      // 'MAX ARTIFACTS'
          2: p_quartzflaskhealth := misc_val; // 'QUARTZ FLASK HEALTH'
          3: p_mysticurnhealth := misc_val;   // 'MYSTIC URN HEALTH'
          4: MAX_MANA := misc_val;            // 'MAX_MANA'
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
        if Pos('=', str) = 0 then
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

        splitstring(str, token1, token2, '=');

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
        if Pos('=', str) = 0 then
        begin
          mustnextline := false; // Already got line
          break;
        end;

        splitstring(str, stmp, token3, '=');
        splitstring(stmp, token1, token2);

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
        if Pos('=', str) = 0 then
        begin
          mustnextline := false; // Already got line
          break;
        end;

        splitstring(str, token1, token2, '=');
        if (length(token2) > 0) and (token2[1] = '(') and (token2[length(token2)] = ')') then
          S_music[music_idx].name := ''
        else
        begin
          token2 := firstword(token2);

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
              end;
            end;
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
        if Pos('=', str) = 0 then
        begin
          mustnextline := false; // Already got line
          break;
        end;

        splitstring(str, token1, token2, '=');

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

function DEH_CurrentSettings: TDStringList;
var
  i, j: integer;
  str: string;
  cmdln: string;
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
      result.Add('# The following fields are DelphiHexen specific enhancements');
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
    if str = '' then
      result.Add('%s = 0', [capitalizedstring(state_tokens[7])])
    else
      result.Add('%s = %s', [capitalizedstring(state_tokens[7]), str]);

    if i = 1 then
      result.Add('# Flags_ex is DelphiHexen specific and declares transparency and light effects');

    result.Add('');
  end;


  //////////////////////////////////////////////////////////////////////////////
  // Add Weapons
  //////////////////////////////////////////////////////////////////////////////
  result.Add('');
  result.Add('# Weapons');
  result.Add('');
  for j := 0 to Ord(NUMCLASSES) - 1 do
    for i := 0 to Ord(NUMWEAPONS) - 1 do
    begin
      result.Add('Weapon %s %s', [weapontype_tokens.Strings[i], playerclass_tokens.Strings[j]]);

      result.Add('%s = %s', [capitalizedstring(weapon_tokens[0]), ammotype_tokens.Strings[Ord(WeaponInfo[i, j].mana)]]);
      result.Add('%s = %d', [capitalizedstring(weapon_tokens[1]), WeaponInfo[i, j].upstate]);
      result.Add('%s = %d', [capitalizedstring(weapon_tokens[2]), WeaponInfo[i, j].downstate]);
      result.Add('%s = %d', [capitalizedstring(weapon_tokens[3]), WeaponInfo[i, j].readystate]);
      result.Add('%s = %d', [capitalizedstring(weapon_tokens[4]), WeaponInfo[i, j].atkstate]);
      result.Add('%s = %d', [capitalizedstring(weapon_tokens[6]), WeaponInfo[i, j].holdatkstate]);
      result.Add('%s = %d', [capitalizedstring(weapon_tokens[5]), WeaponInfo[i, j].flashstate]);

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

  result.Add('%s = %d', [capitalizedstring(misc_tokens[0]), p_maxmorphhealth]);
  result.Add('%s = %d', [capitalizedstring(misc_tokens[1]), p_maxartifacts]);
  result.Add('%s = %d', [capitalizedstring(misc_tokens[2]), p_quartzflaskhealth]);
  result.Add('%s = %d', [capitalizedstring(misc_tokens[3]), p_mysticurnhealth]);
  result.Add('%s = %d', [capitalizedstring(misc_tokens[4]), MAX_MANA]);

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

//
// DEH_Init
//
// JVAL
// Initializing DEH tokens
//
procedure DEH_Init;
var
  i: integer;
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
  mobj_flags.Add('MF_ALTSHADOW');
  mobj_flags.Add('MF_SHADOW');
  mobj_flags.Add('MF_NOBLOOD');
  mobj_flags.Add('MF_CORPSE');
  mobj_flags.Add('MF_INFLOAT');
  mobj_flags.Add('MF_COUNTKILL');
  mobj_flags.Add('MF_ICECORPSE');
  mobj_flags.Add('MF_SKULLFLY');
  mobj_flags.Add('MF_NOTDMATCH');
  mobj_flags.Add('MF_TRANSLATION');
  mobj_flags.Add('MF_UNUSED1');
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
  mobj_flags2.Add('MF2_BLASTED');
  mobj_flags2.Add('MF2_FLY');
  mobj_flags2.Add('MF2_FLOORCLIP');
  mobj_flags2.Add('MF2_SPAWNFLOAT');
  mobj_flags2.Add('MF2_NOTELEPORT');
  mobj_flags2.Add('MF2_RIP');
  mobj_flags2.Add('MF2_PUSHABLE');
  mobj_flags2.Add('MF2_SLIDE');
  mobj_flags2.Add('MF2_ONMOBJ');
  mobj_flags2.Add('MF2_PASSMOBJ');
  mobj_flags2.Add('MF2_CANNOTPUSH');
  mobj_flags2.Add('MF2_DROPPED');
  mobj_flags2.Add('MF2_BOSS');
  mobj_flags2.Add('MF2_FIREDAMAGE');
  mobj_flags2.Add('MF2_NODMGTHRUST');
  mobj_flags2.Add('MF2_TELESTOMP');
  mobj_flags2.Add('MF2_FLOATBOB');
  mobj_flags2.Add('MF2_DONTDRAW');
  mobj_flags2.Add('MF2_IMPACT');
  mobj_flags2.Add('MF2_PUSHWALL');
  mobj_flags2.Add('MF2_MCROSS');
  mobj_flags2.Add('MF2_PCROSS');
  mobj_flags2.Add('MF2_CANTLEAVEFLOORPIC');
  mobj_flags2.Add('MF2_NONSHOOTABLE');
  mobj_flags2.Add('MF2_INVULNERABLE');
  mobj_flags2.Add('MF2_DORMANT');
  mobj_flags2.Add('MF2_ICEDAMAGE');
  mobj_flags2.Add('MF2_SEEKERMISSILE');

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
  mobj_flags2_ex.Add('MF2_EX_REFLECTIVE');
  mobj_flags2_ex.Add('MF2_EX_FRIGHTENED');
  mobj_flags2_ex.Add('MF2_EX_NODAMAGE');
  mobj_flags2_ex.Add('MF2_EX_DONTRUNSCRIPTS');
  mobj_flags2_ex.Add('MF2_EX_PRECISESPAWNANGLE');
  mobj_flags2_ex.Add('MF2_EX_CUSTOMDROPITEM');
  // JVAL: VERSION 204
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

  mobj_flags2_ex_hash := TDEHStringsHashTable.Create;
  mobj_flags2_ex_hash.AssignList(mobj_flags2_ex);


  mobj_flags3_ex := TDTextList.Create;
  mobj_flags3_ex.Add('MF3_EX_FLOORBOUNCE');
  mobj_flags3_ex.Add('MF3_EX_CEILINGBOUNCE');
  mobj_flags3_ex.Add('MF3_EX_WALLBOUNCE');
  mobj_flags3_ex.Add('MF3_EX_NOMAXMOVE');
  mobj_flags3_ex.Add('MF3_EX_NOCRASH');
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
  mobj_flags3_ex.Add('MF3_EX_THRUACTORS');
  mobj_flags3_ex.Add('MF3_EX_THRUSPECIES');
  mobj_flags3_ex.Add('MF3_EX_NOBLOCKMONST');
  mobj_flags3_ex.Add('MF3_EX_NOTAUTOAIMED');
  mobj_flags3_ex.Add('MF3_EX_SLIDING');
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

  mobj_flags4_ex_hash := TDEHStringsHashTable.Create;
  mobj_flags4_ex_hash.AssignList(mobj_flags4_ex);


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

  deh_actions[0].action.acp1 := nil;
  deh_actions[0].name := 'NULL';
  deh_actions[0].originalname := 'NULL';
  {$IFDEF DLL}deh_actions[0].decl := '';{$ENDIF}

  dehnumactions := 1;

  DEH_AddAction(@A_AddPlayerCorpse, 'A_AddPlayerCorpse()'); // 1
  DEH_AddAction(@A_BatMove, 'A_BatMove()'); // 2
  DEH_AddAction(@A_BatSpawn, 'A_BatSpawn()'); // 3
  DEH_AddAction(@A_BatSpawnInit, 'A_BatSpawnInit()'); // 4
  DEH_AddAction(@A_BellReset1, 'A_BellReset1()'); // 5
  DEH_AddAction(@A_BellReset2, 'A_BellReset2()'); // 6
  DEH_AddAction(@A_BishopAttack, 'A_BishopAttack()'); // 7
  DEH_AddAction(@A_BishopAttack2, 'A_BishopAttack2()'); // 8
  DEH_AddAction(@A_BishopChase, 'A_BishopChase()'); // 9
  DEH_AddAction(@A_BishopDecide, 'A_BishopDecide()'); // 10
  DEH_AddAction(@A_BishopDoBlur, 'A_BishopDoBlur()'); // 11
  DEH_AddAction(@A_BishopMissileSeek, 'A_BishopMissileSeek()'); // 12
  DEH_AddAction(@A_BishopMissileWeave, 'A_BishopMissileWeave()'); // 13
  DEH_AddAction(@A_BishopPainBlur, 'A_BishopPainBlur()'); // 14
  DEH_AddAction(@A_BishopPuff, 'A_BishopPuff()'); // 15
  DEH_AddAction(@A_BishopSpawnBlur, 'A_BishopSpawnBlur()'); // 16
  DEH_AddAction(@A_BounceCheck, 'A_BounceCheck()'); // 17
  DEH_AddAction(@A_BridgeInit, 'A_BridgeInit()'); // 18
  DEH_AddAction(@A_BridgeOrbit, 'A_BridgeOrbit()'); // 19
  DEH_AddAction(@A_CentaurAttack, 'A_CentaurAttack()'); // 20
  DEH_AddAction(@A_CentaurAttack2, 'A_CentaurAttack2()'); // 21
  DEH_AddAction(@A_CentaurDefend, 'A_CentaurDefend()'); // 22
  DEH_AddAction(@A_CentaurDropStuff, 'A_CentaurDropStuff()'); // 23
  DEH_AddAction(@A_CFlameAttack, 'A_CFlameAttack()'); // 24
  DEH_AddAction(@A_CFlameMissile, 'A_CFlameMissile()'); // 25
  DEH_AddAction(@A_CFlamePuff, 'A_CFlamePuff()'); // 26
  DEH_AddAction(@A_CFlameRotate, 'A_CFlameRotate()'); // 27
  DEH_AddAction(@A_Chase, 'A_Chase()'); // 28
  DEH_AddAction(@A_CheckBurnGone, 'A_CheckBurnGone()'); // 29
  DEH_AddAction(@A_CheckFloor, 'A_CheckFloor()'); // 30
  DEH_AddAction(@A_CheckSkullDone, 'A_CheckSkullDone()'); // 31
  DEH_AddAction(@A_CheckSkullFloor, 'A_CheckSkullFloor()'); // 32
  DEH_AddAction(@A_CheckTeleRing, 'A_CheckTeleRing()'); // 33
  DEH_AddAction(@A_CheckThrowBomb, 'A_CheckThrowBomb()'); // 34
  DEH_AddAction(@A_CHolyAttack, 'A_CHolyAttack()'); // 35
  DEH_AddAction(@A_CHolyAttack2, 'A_CHolyAttack2()'); // 36
  DEH_AddAction(@A_CHolyCheckScream, 'A_CHolyCheckScream()'); // 37
  DEH_AddAction(@A_CHolyPalette, 'A_CHolyPalette()'); // 38
  DEH_AddAction(@A_CHolySeek, 'A_CHolySeek()'); // 39
  DEH_AddAction(@A_CHolySpawnPuff, 'A_CHolySpawnPuff()'); // 40
  DEH_AddAction(@A_CHolyTail, 'A_CHolyTail()'); // 41
  DEH_AddAction(@A_ClassBossHealth, 'A_ClassBossHealth()'); // 42
  DEH_AddAction(@A_ClericAttack, 'A_ClericAttack()'); // 43
  DEH_AddAction(@A_CMaceAttack, 'A_CMaceAttack()'); // 44
  DEH_AddAction(@A_ContMobjSound, 'A_ContMobjSound()'); // 45
  DEH_AddAction(@A_CorpseBloodDrip, 'A_CorpseBloodDrip()'); // 46
  DEH_AddAction(@A_CorpseExplode, 'A_CorpseExplode()'); // 47
  DEH_AddAction(@A_CStaffAttack, 'A_CStaffAttack()'); // 48
  DEH_AddAction(@A_CStaffCheck, 'A_CStaffCheck()'); // 49
  DEH_AddAction(@A_CStaffCheckBlink, 'A_CStaffCheckBlink()'); // 50
  DEH_AddAction(@A_CStaffInitBlink, 'A_CStaffInitBlink()'); // 51
  DEH_AddAction(@A_CStaffMissileSlither, 'A_CStaffMissileSlither()'); // 52
  DEH_AddAction(@A_DelayGib, 'A_DelayGib()'); // 53
  DEH_AddAction(@A_Demon2Death, 'A_Demon2Death()'); // 54
  DEH_AddAction(@A_DemonAttack1, 'A_DemonAttack1()'); // 55
  DEH_AddAction(@A_DemonAttack2, 'A_DemonAttack2()'); // 56
  DEH_AddAction(@A_DemonDeath, 'A_DemonDeath()'); // 57
  DEH_AddAction(@A_DragonAttack, 'A_DragonAttack()'); // 58
  DEH_AddAction(@A_DragonCheckCrash, 'A_DragonCheckCrash()'); // 59
  DEH_AddAction(@A_DragonFlap, 'A_DragonFlap()'); // 60
  DEH_AddAction(@A_DragonFlight, 'A_DragonFlight()'); // 61
  DEH_AddAction(@A_DragonFX2, 'A_DragonFX2()'); // 62
  DEH_AddAction(@A_DragonInitFlight, 'A_DragonInitFlight()'); // 63
  DEH_AddAction(@A_DragonPain, 'A_DragonPain()'); // 64
  DEH_AddAction(@A_DropMace, 'A_DropMace()'); // 65
  DEH_AddAction(@A_ESound, 'A_ESound()'); // 66
  DEH_AddAction(@A_EttinAttack, 'A_EttinAttack()'); // 67
  DEH_AddAction(@A_Explode, 'A_Explode()'); // 68
  DEH_AddAction(@A_FaceTarget, 'A_FaceTarget()'); // 69
  DEH_AddAction(@A_FastChase, 'A_FastChase()'); // 70
  DEH_AddAction(@A_FAxeAttack, 'A_FAxeAttack()'); // 71
  DEH_AddAction(@A_FHammerAttack, 'A_FHammerAttack()'); // 72
  DEH_AddAction(@A_FHammerThrow, 'A_FHammerThrow()'); // 73
  DEH_AddAction(@A_FighterAttack, 'A_FighterAttack()'); // 74
  DEH_AddAction(@A_FireConePL1, 'A_FireConePL1()'); // 75
  DEH_AddAction(@A_FiredAttack, 'A_FiredAttack()'); // 76
  DEH_AddAction(@A_FiredChase, 'A_FiredChase()'); // 77
  DEH_AddAction(@A_FiredRocks, 'A_FiredRocks()'); // 78
  DEH_AddAction(@A_FiredSplotch, 'A_FiredSplotch()'); // 79
  DEH_AddAction(@A_FlameCheck, 'A_FlameCheck()'); // 80
  DEH_AddAction(@A_FloatGib, 'A_FloatGib()'); // 81
  DEH_AddAction(@A_FogMove, 'A_FogMove()'); // 82
  DEH_AddAction(@A_FogSpawn, 'A_FogSpawn()'); // 83
  DEH_AddAction(@A_FPunchAttack, 'A_FPunchAttack()'); // 84
  DEH_AddAction(@A_FreeTargMobj, 'A_FreeTargMobj()'); // 85
  DEH_AddAction(@A_FreezeDeath, 'A_FreezeDeath()'); // 86
  DEH_AddAction(@A_FreezeDeathChunks, 'A_FreezeDeathChunks()'); // 87
  DEH_AddAction(@A_FSwordAttack, 'A_FSwordAttack()'); // 88
  DEH_AddAction(@A_FSwordFlames, 'A_FSwordFlames()'); // 89
  DEH_AddAction(@A_HideThing, 'A_HideThing()'); // 90
  DEH_AddAction(@A_IceCheckHeadDone, 'A_IceCheckHeadDone()'); // 91
  DEH_AddAction(@A_IceGuyAttack, 'A_IceGuyAttack()'); // 92
  DEH_AddAction(@A_IceGuyChase, 'A_IceGuyChase()'); // 93
  DEH_AddAction(@A_IceGuyDie, 'A_IceGuyDie()'); // 94
  DEH_AddAction(@A_IceGuyLook, 'A_IceGuyLook()'); // 95
  DEH_AddAction(@A_IceGuyMissileExplode, 'A_IceGuyMissileExplode()'); // 96
  DEH_AddAction(@A_IceGuyMissilePuff, 'A_IceGuyMissilePuff()'); // 97
  DEH_AddAction(@A_IceSetTics, 'A_IceSetTics()'); // 98
  DEH_AddAction(@A_KBolt, 'A_KBolt()'); // 99
  DEH_AddAction(@A_KBoltRaise, 'A_KBoltRaise()'); // 100
  DEH_AddAction(@A_KoraxBonePop, 'A_KoraxBonePop()'); // 101
  DEH_AddAction(@A_KoraxChase, 'A_KoraxChase()'); // 102
  DEH_AddAction(@A_KoraxCommand, 'A_KoraxCommand()'); // 103
  DEH_AddAction(@A_KoraxDecide, 'A_KoraxDecide()'); // 104
  DEH_AddAction(@A_KoraxMissile, 'A_KoraxMissile()'); // 105
  DEH_AddAction(@A_KoraxStep, 'A_KoraxStep()'); // 106
  DEH_AddAction(@A_KoraxStep2, 'A_KoraxStep2()'); // 107
  DEH_AddAction(@A_KSpiritRoam, 'A_KSpiritRoam()'); // 108
  DEH_AddAction(@A_LastZap, 'A_LastZap()'); // 109
  DEH_AddAction(@A_LeafCheck, 'A_LeafCheck()'); // 110
  DEH_AddAction(@A_LeafSpawn, 'A_LeafSpawn()'); // 111
  DEH_AddAction(@A_LeafThrust, 'A_LeafThrust()'); // 112
  DEH_AddAction(@A_Light0, 'A_Light0()'); // 113
  DEH_AddAction(@A_LightningClip, 'A_LightningClip()'); // 114
  DEH_AddAction(@A_LightningReady, 'A_LightningReady()'); // 115
  DEH_AddAction(@A_LightningRemove, 'A_LightningRemove()'); // 116
  DEH_AddAction(@A_LightningZap, 'A_LightningZap()'); // 117
  DEH_AddAction(@A_Look, 'A_Look()'); // 118
  DEH_AddAction(@A_Lower, 'A_Lower()'); // 119
  DEH_AddAction(@A_MageAttack, 'A_MageAttack()'); // 120
  DEH_AddAction(@A_MinotaurAtk1, 'A_MinotaurAtk1()'); // 121
  DEH_AddAction(@A_MinotaurAtk2, 'A_MinotaurAtk2()'); // 122
  DEH_AddAction(@A_MinotaurAtk3, 'A_MinotaurAtk3()'); // 123
  DEH_AddAction(@A_MinotaurCharge, 'A_MinotaurCharge()'); // 124
  DEH_AddAction(@A_MinotaurChase, 'A_MinotaurChase()'); // 125
  DEH_AddAction(@A_MinotaurDecide, 'A_MinotaurDecide()'); // 126
  DEH_AddAction(@A_MinotaurFade0, 'A_MinotaurFade0()'); // 127
  DEH_AddAction(@A_MinotaurFade1, 'A_MinotaurFade1()'); // 128
  DEH_AddAction(@A_MinotaurFade2, 'A_MinotaurFade2()'); // 129
  DEH_AddAction(@A_MinotaurLook, 'A_MinotaurLook()'); // 130
  DEH_AddAction(@A_MinotaurRoam, 'A_MinotaurRoam()'); // 131
  DEH_AddAction(@A_MLightningAttack, 'A_MLightningAttack()'); // 132
  DEH_AddAction(@A_MntrFloorFire, 'A_MntrFloorFire()'); // 133
  DEH_AddAction(@A_MStaffAttack, 'A_MStaffAttack()'); // 134
  DEH_AddAction(@A_MStaffPalette, 'A_MStaffPalette()'); // 135
  DEH_AddAction(@A_MStaffTrack, 'A_MStaffTrack()'); // 136
  DEH_AddAction(@A_MStaffWeave, 'A_MStaffWeave()'); // 137
  DEH_AddAction(@A_MWandAttack, 'A_MWandAttack()'); // 138
  DEH_AddAction(@A_NoBlocking, 'A_NoBlocking()'); // 139
  DEH_AddAction(@A_NoGravity, 'A_NoGravity()'); // 140
  DEH_AddAction(@A_Pain, 'A_Pain()'); // 141
  DEH_AddAction(@A_PigAttack, 'A_PigAttack()'); // 142
  DEH_AddAction(@A_PigChase, 'A_PigChase()'); // 143
  DEH_AddAction(@A_PigLook, 'A_PigLook()'); // 144
  DEH_AddAction(@A_PigPain, 'A_PigPain()'); // 145
  DEH_AddAction(@A_PoisonBagCheck, 'A_PoisonBagCheck()'); // 146
  DEH_AddAction(@A_PoisonBagDamage, 'A_PoisonBagDamage()'); // 147
  DEH_AddAction(@A_PoisonBagInit, 'A_PoisonBagInit()'); // 148
  DEH_AddAction(@A_PoisonShroom, 'A_PoisonShroom()'); // 149
  DEH_AddAction(@A_PotteryCheck, 'A_PotteryCheck()'); // 150
  DEH_AddAction(@A_PotteryChooseBit, 'A_PotteryChooseBit()'); // 151
  DEH_AddAction(@A_PotteryExplode, 'A_PotteryExplode()'); // 152
  DEH_AddAction(@A_Quake, 'A_Quake()'); // 153
  DEH_AddAction(@A_QueueCorpse, 'A_QueueCorpse()'); // 154
  DEH_AddAction(@A_Raise, 'A_Raise()'); // 155
  DEH_AddAction(@A_ReFire, 'A_ReFire()'); // 156
  DEH_AddAction(@A_RestoreArtifact, 'A_RestoreArtifact()'); // 157
  DEH_AddAction(@A_RestoreSpecialThing1, 'A_RestoreSpecialThing1()'); // 158
  DEH_AddAction(@A_RestoreSpecialThing2, 'A_RestoreSpecialThing2()'); // 159
  DEH_AddAction(@A_Scream, 'A_Scream()'); // 160
  DEH_AddAction(@A_SerpentBirthScream, 'A_SerpentBirthScream()'); // 161
  DEH_AddAction(@A_SerpentChase, 'A_SerpentChase()'); // 162
  DEH_AddAction(@A_SerpentCheckForAttack, 'A_SerpentCheckForAttack()'); // 163
  DEH_AddAction(@A_SerpentChooseAttack, 'A_SerpentChooseAttack()'); // 164
  DEH_AddAction(@A_SerpentDiveSound, 'A_SerpentDiveSound()'); // 165
  DEH_AddAction(@A_SerpentHeadCheck, 'A_SerpentHeadCheck()'); // 166
  DEH_AddAction(@A_SerpentHeadPop, 'A_SerpentHeadPop()'); // 167
  DEH_AddAction(@A_SerpentHide, 'A_SerpentHide()'); // 168
  DEH_AddAction(@A_SerpentHumpDecide, 'A_SerpentHumpDecide()'); // 169
  DEH_AddAction(@A_SerpentLowerHump, 'A_SerpentLowerHump()'); // 170
  DEH_AddAction(@A_SerpentMeleeAttack, 'A_SerpentMeleeAttack()'); // 171
  DEH_AddAction(@A_SerpentMissileAttack, 'A_SerpentMissileAttack()'); // 172
  DEH_AddAction(@A_SerpentRaiseHump, 'A_SerpentRaiseHump()'); // 173
  DEH_AddAction(@A_SerpentSpawnGibs, 'A_SerpentSpawnGibs()'); // 174
  DEH_AddAction(@A_SerpentUnHide, 'A_SerpentUnHide()'); // 175
  DEH_AddAction(@A_SerpentWalk, 'A_SerpentWalk()'); // 176
  DEH_AddAction(@A_SetAltShadow, 'A_SetAltShadow()'); // 177
  DEH_AddAction(@A_SetReflective, 'A_SetReflective()'); // 178
  DEH_AddAction(@A_SetShootable, 'A_SetShootable()'); // 179
  DEH_AddAction(@A_ShedShard, 'A_ShedShard()'); // 180
  DEH_AddAction(@A_SinkGib, 'A_SinkGib()'); // 181
  DEH_AddAction(@A_SkullPop, 'A_SkullPop()'); // 182
  DEH_AddAction(@A_SmBounce, 'A_SmBounce()'); // 183
  DEH_AddAction(@A_SmokePuffExit, 'A_SmokePuffExit()'); // 184
  DEH_AddAction(@A_SnoutAttack, 'A_SnoutAttack()'); // 185
  DEH_AddAction(@A_SoAExplode, 'A_SoAExplode()'); // 186
  DEH_AddAction(@A_SorcBallOrbit, 'A_SorcBallOrbit()'); // 187
  DEH_AddAction(@A_SorcBallPop, 'A_SorcBallPop()'); // 188
  DEH_AddAction(@A_SorcBossAttack, 'A_SorcBossAttack()'); // 189
  DEH_AddAction(@A_SorcererBishopEntry, 'A_SorcererBishopEntry()'); // 190
  DEH_AddAction(@A_SorcFX1Seek, 'A_SorcFX1Seek()'); // 191
  DEH_AddAction(@A_SorcFX2Orbit, 'A_SorcFX2Orbit()'); // 192
  DEH_AddAction(@A_SorcFX2Split, 'A_SorcFX2Split()'); // 193
  DEH_AddAction(@A_SorcFX4Check, 'A_SorcFX4Check()'); // 194
  DEH_AddAction(@A_SorcSpinBalls, 'A_SorcSpinBalls()'); // 195
  DEH_AddAction(@A_SpawnBishop, 'A_SpawnBishop()'); // 196
  DEH_AddAction(@A_SpawnFizzle, 'A_SpawnFizzle()'); // 197
  DEH_AddAction(@A_SpeedBalls, 'A_SpeedBalls()'); // 198
  DEH_AddAction(@A_SpeedFade, 'A_SpeedFade()'); // 199
  DEH_AddAction(@A_Summon, 'A_Summon()'); // 200
  DEH_AddAction(@A_TeloSpawnA, 'A_TeloSpawnA()'); // 201
  DEH_AddAction(@A_TeloSpawnB, 'A_TeloSpawnB()'); // 202
  DEH_AddAction(@A_TeloSpawnC, 'A_TeloSpawnC()'); // 203
  DEH_AddAction(@A_TeloSpawnD, 'A_TeloSpawnD()'); // 204
  DEH_AddAction(@A_ThrustBlock, 'A_ThrustBlock()'); // 205
  DEH_AddAction(@A_ThrustImpale, 'A_ThrustImpale()'); // 206
  DEH_AddAction(@A_ThrustInitDn, 'A_ThrustInitDn()'); // 207
  DEH_AddAction(@A_ThrustInitUp, 'A_ThrustInitUp()'); // 208
  DEH_AddAction(@A_ThrustLower, 'A_ThrustLower()'); // 209
  DEH_AddAction(@A_ThrustRaise, 'A_ThrustRaise()'); // 210
  DEH_AddAction(@A_TreeDeath, 'A_TreeDeath()'); // 211
  DEH_AddAction(@A_UnHideThing, 'A_UnHideThing()'); // 212
  DEH_AddAction(@A_UnSetInvulnerable, 'A_UnSetInvulnerable()'); // 213
  DEH_AddAction(@A_UnSetReflective, 'A_UnSetReflective()'); // 214
  DEH_AddAction(@A_UnSetShootable, 'A_UnSetShootable()'); // 215
  DEH_AddAction(@A_WeaponReady, 'A_WeaponReady()'); // 216
  DEH_AddAction(@A_WraithChase, 'A_WraithChase()'); // 217
  DEH_AddAction(@A_WraithFX2, 'A_WraithFX2()'); // 218
  DEH_AddAction(@A_WraithFX3, 'A_WraithFX3()'); // 219
  DEH_AddAction(@A_WraithInit, 'A_WraithInit()'); // 220
  DEH_AddAction(@A_WraithLook, 'A_WraithLook()'); // 221
  DEH_AddAction(@A_WraithMelee, 'A_WraithMelee()'); // 222
  DEH_AddAction(@A_WraithMissile, 'A_WraithMissile()'); // 223
  DEH_AddAction(@A_WraithRaise, 'A_WraithRaise()'); // 224
  DEH_AddAction(@A_WraithRaiseInit, 'A_WraithRaiseInit()'); // 225
  DEH_AddAction(@A_ZapMimic, 'A_ZapMimic()'); // 226
  DEH_AddAction(@A_CustomSound1, 'A_CustomSound1()'); // 227
  DEH_AddAction(@A_CustomSound2, 'A_CustomSound2()'); // 228
  DEH_AddAction(@A_CustomSound3, 'A_CustomSound3()'); // 229
  DEH_AddAction(@A_RandomPainSound, 'A_RandomPainSound()'); // 230
  DEH_AddAction(@A_RandomSeeSound, 'A_RandomSeeSound()'); // 231
  DEH_AddAction(@A_RandomAttackSound, 'A_RandomAttackSound()'); // 232
  DEH_AddAction(@A_RandomDeathSound, 'A_RandomDeathSound()'); // 233
  DEH_AddAction(@A_RandomActiveSound, 'A_RandomActiveSound()'); // 234
  DEH_AddAction(@A_RandomCustomSound1, 'A_RandomCustomSound1()'); // 235
  DEH_AddAction(@A_RandomCustomSound2, 'A_RandomCustomSound2()'); // 236
  DEH_AddAction(@A_RandomCustomSound3, 'A_RandomCustomSound3()'); // 237
  DEH_AddAction(@A_RandomCustomSound, 'A_RandomCustomSound()'); // 238
  DEH_AddAction(@A_Wander, 'A_Wander()'); // 239
  DEH_AddAction(@A_Playsound, 'A_Playsound(sound: string)'); // 240
  DEH_AddAction(@A_RandomSound, 'A_RandomSound(sound1: string, [sound2: string], ...)'); // 241
  DEH_AddAction(@A_Stop, 'A_Stop()'); // 242
  DEH_AddAction(@A_Jump, 'A_Jump(propability: random_t, offset1: integer, [offset2: integer], ...)'); // 243
  DEH_AddAction(@A_CustomMissile, 'A_CustomMissile(missiletype: string, [height: integer], [offset: integer], [angle: integer], [aimmode: integer], [pitch: integer])'); // 244
  DEH_AddAction(@A_NoGravity, 'A_NoGravity()'); // 245
  DEH_AddAction(@A_Gravity, 'A_Gravity()'); // 246
  DEH_AddAction(@A_NoBlocking, 'A_NoBlocking()'); // 247
  DEH_AddAction(@A_MeleeAttack, 'A_MeleeAttack([mindamage: integer], [maxdamage: integer])'); // 248
  DEH_AddAction(@A_SpawnItem, 'A_SpawnItem(type: string, [distance: float], [zheight: float], [angle: angle])'); // 249
  DEH_AddAction(@A_SeekerMissile, 'A_SeekerMissile(threshold_angle: angle, [turnMax_angle: angle])'); // 250
  DEH_AddAction(@A_CStaffMissileSlither, 'A_CStaffMissileSlither()'); // 251
  DEH_AddAction(@A_SetTranslucent, 'A_SetTranslucent(alpha: float, [style: integer])'); // 252
  DEH_AddAction(@A_Die, 'A_Die()'); // 253
  DEH_AddAction(@A_CustomBulletAttack, 'A_CustomBulletAttack(spread_xy: angle, numbullets: integer, damageperbullet: integer, [range: integer])'); // 254
  DEH_AddAction(@A_FadeOut, 'A_FadeOut(fade: float)'); // 255
  DEH_AddAction(@A_FadeIn, 'A_FadeIn(fade: float)'); // 256
  DEH_AddAction(@A_MissileAttack, 'A_MissileAttack([missiletype: string])'); // 257
  DEH_AddAction(@A_AdjustSideSpot, 'A_AdjustSideSpot(sideoffset: float)'); // 258
  DEH_AddAction(@A_Countdown, 'A_Countdown()'); // 259
  DEH_AddAction(@A_FastChase, 'A_FastChase()'); // 260
  DEH_AddAction(@A_LowGravity, 'A_LowGravity()'); // 261
  DEH_AddAction(@A_ThrustZ, 'A_ThrustZ(momz: float, ang: angle)'); // 262
  DEH_AddAction(@A_ThrustXY, 'A_ThrustXY(mom: float, ang: angle)'); // 263
  DEH_AddAction(@A_Turn, 'A_Turn(value: angle)'); // 264
  DEH_AddAction(@A_JumpIfCloser, 'A_JumpIfCloser(distancetotarget: float, offset: integer)'); // 265
  DEH_AddAction(@A_JumpIfHealthLower, 'A_JumpIfHealthLower(health: integer, offset: integer)'); // 266
  DEH_AddAction(@A_ScreamAndUnblock, 'A_ScreamAndUnblock()'); // 267
  DEH_AddAction(@A_PlayWeaponsound, 'A_PlayWeaponsound(sound: string)'); // 268
  DEH_AddAction(@A_SetInvulnerable, 'A_SetInvulnerable()'); // 269
  DEH_AddAction(@A_UnSetInvulnerable, 'A_UnSetInvulnerable()'); // 270
  DEH_AddAction(@A_RandomMeleeSound, 'A_RandomMeleeSound()'); // 271
  DEH_AddAction(@A_FloatBob, 'A_FloatBob()'); // 272
  DEH_AddAction(@A_NoFloatBob, 'A_NoFloatBob()'); // 273
  DEH_AddAction(@A_Missile, 'A_Missile()'); // 274
  DEH_AddAction(@A_NoMissile, 'A_NoMissile()'); // 275
  DEH_AddAction(@A_ComboAttack, 'A_ComboAttack()'); // 276
  DEH_AddAction(@A_BulletAttack, 'A_BulletAttack([numbullets: integer])'); // 277
  DEH_AddAction(@A_MediumGravity, 'A_MediumGravity()'); // 278
  DEH_AddAction(@A_FadeOut10, 'A_FadeOut10()'); // 279
  DEH_AddAction(@A_FadeOut20, 'A_FadeOut20()'); // 280
  DEH_AddAction(@A_FadeOut30, 'A_FadeOut30()'); // 281
  DEH_AddAction(@A_FadeIn10, 'A_FadeIn10()'); // 282
  DEH_AddAction(@A_FadeIn20, 'A_FadeIn20()'); // 283
  DEH_AddAction(@A_FadeIn30, 'A_FadeIn30()'); // 284
  DEH_AddAction(@A_SpawnItemEx, 'A_SpawnItemEx(itemtype: string, [xofs: float], [yofs: float], [zofs: float], [momx: float], [momy: float], [momz: float], [ang: angle], [flags: integer], [chance: integer])'); // 285
  DEH_AddAction(@A_RandomMissile, 'A_RandomMissile(missile1: string, [missile2: string], ...)'); // 286
  DEH_AddAction(@P_RemoveMobj, 'A_RemoveSelf()'); // 287
  DEH_AddAction(@A_GoTo, 'A_GoTo(propability: random_t, state: state_t)'); // 288
  DEH_AddAction(@A_GoToIfCloser, 'A_GoToIfCloser(distancetotarget: float, state: state_t)'); // 289
  DEH_AddAction(@A_GoToIfHealthLower, 'A_GoToIfHealthLower(health: integer, state: state_t)'); // 290
  DEH_AddAction(@A_ConsoleCommand, 'A_ConsoleCommand(cmd: string, [parm1: string], [parm2: string], ...)'); // 291
  DEH_AddAction(@A_SetFrightened, 'A_SetFrightened()'); // 292
  DEH_AddAction(@A_UnSetFrightened, 'A_UnSetFrightened()'); // 293
  DEH_AddAction(@A_SetCustomParam, 'A_SetCustomParam(param: string, value: integer)'); // 294
  DEH_AddAction(@A_AddCustomParam, 'A_AddCustomParam(param: string, value: integer)'); // 295
  DEH_AddAction(@A_SubtractCustomParam, 'A_SubtractCustomParam(param: string, value: integer)'); // 296
  DEH_AddAction(@A_SetTargetCustomParam, 'A_SetTargetCustomParam(param: string, value: integer)'); // 297
  DEH_AddAction(@A_AddTargetCustomParam, 'A_AddTargetCustomParam(param: string, value: integer)'); // 298
  DEH_AddAction(@A_SubtractTargetCustomParam, 'A_SubtractTargetCustomParam(param: string, value: integer)'); // 299
  DEH_AddAction(@A_JumpIfCustomParam, 'A_JumpIfCustomParam(param: string, value: integer, offset: integer)'); // 300
  DEH_AddAction(@A_JumpIfCustomParamLess, 'A_JumpIfCustomParamLess(param: string, value: integer, offset: integer)'); // 301
  DEH_AddAction(@A_JumpIfCustomParamGreater, 'A_JumpIfCustomParamGreater(param: string, value: integer, offset: integer)'); // 302
  DEH_AddAction(@A_JumpIfTargetCustomParam, 'A_JumpIfTargetCustomParam(param: string, value: integer, offset: integer)'); // 303
  DEH_AddAction(@A_JumpIfTargetCustomParamLess, 'A_JumpIfTargetCustomParamLess(param: string, value: integer, offset: integer)'); // 304
  DEH_AddAction(@A_JumpIfTargetCustomParamGreater, 'A_JumpIfTargetCustomParamGreater(param: string, value: integer, offset: integer)'); // 305
  DEH_AddAction(@A_GoToIfCustomParam, 'A_GoToIfCustomParam(param: string, value: integer, state: state_t)'); // 306
  DEH_AddAction(@A_GoToIfCustomParamLess, 'A_GoToIfCustomParamLess(param: string, value: integer, state: state_t)'); // 307
  DEH_AddAction(@A_GoToIfCustomParamGreater, 'A_GoToIfCustomParamGreater(param: string, value: integer, state: state_t)'); // 308
  DEH_AddAction(@A_GoToIfTargetCustomParam, 'A_GoToIfTargetCustomParam(param: string, value: integer, state: state_t)'); // 309
  DEH_AddAction(@A_GoToIfTargetCustomParamLess, 'A_GoToIfTargetCustomParamLess(param: string, value: integer, state: state_t)'); // 310
  DEH_AddAction(@A_GoToIfTargetCustomParamGreater, 'A_GoToIfTargetCustomParamGreater(param: string, value: integer, state: state_t)'); // 311
  DEH_AddAction(@A_SetNoDamage, 'A_SetNoDamage()'); // 312
  DEH_AddAction(@A_UnSetNoDamage, 'A_UnSetNoDamage()'); // 313
  DEH_AddAction(@A_RunScript, 'A_RunScript(script1: string, [script2: string], ...)'); // 314
  DEH_AddAction(@A_GhostOn, 'A_GhostOn()'); // 315
  DEH_AddAction(@A_GhostOff, 'A_GhostOff()'); // 316
  DEH_AddAction(@A_Turn5, 'A_Turn5()'); // 317
  DEH_AddAction(@A_Turn10, 'A_Turn10()'); // 318
  DEH_AddAction(@A_Blocking, 'A_Blocking()'); // 319
  DEH_AddAction(@A_DoNotRunScripts, 'A_DoNotRunScripts()'); // 320
  DEH_AddAction(@A_DoRunScripts, 'A_DoRunScripts()'); // 321
  DEH_AddAction(@A_TargetDropItem, 'A_TargetDropItem(dropitemtype: string)'); // 322
  DEH_AddAction(@A_DefaultTargetDropItem, 'A_DefaultTargetDropItem()'); // 323
  DEH_AddAction(@A_SetDropItem, 'A_SetDropItem(dropitemtype: string)'); // 324
  DEH_AddAction(@A_SetDefaultDropItem, 'A_SetDefaultDropItem()'); // 325
  DEH_AddAction(@A_GlobalEarthQuake, 'A_GlobalEarthQuake(tics: integer)'); // 326
  DEH_AddAction(@A_JumpIfMapStringEqual, 'A_JumpIfMapStringEqual(parm: string, value: string, offset; integer)'); // 327
  DEH_AddAction(@A_JumpIfMapStringLess, 'A_JumpIfMapStringLess(parm: string, value: string, offset; integer)'); // 328
  DEH_AddAction(@A_JumpIfMapStringGreater, 'A_JumpIfMapStringGreater(parm: string, value: string, offset; integer)'); // 329
  DEH_AddAction(@A_JumpIfMapIntegerEqual, 'A_JumpIfMapIntegerEqual(parm: string, value: integer, offset: integer)'); // 330
  DEH_AddAction(@A_JumpIfMapIntegerLess, 'A_JumpIfMapIntegerLess(parm: string, value: integer, offset: integer)'); // 331
  DEH_AddAction(@A_JumpIfMapIntegerGreater, 'A_JumpIfMapIntegerGreater(parm: string, value: integer, offset: integer)'); // 332
  DEH_AddAction(@A_JumpIfMapFloatEqual, 'A_JumpIfMapFloatEqual(parm: string, value: float, offset: integer)'); // 333
  DEH_AddAction(@A_JumpIfMapFloatLess, 'A_JumpIfMapFloatLess(parm: string, value: float, offset: integer)'); // 334
  DEH_AddAction(@A_JumpIfMapFloatGreater, 'A_JumpIfMapFloatGreater(parm: string, value: float, offset: integer)'); // 335
  DEH_AddAction(@A_JumpIfWorldStringEqual, 'A_JumpIfWorldStringEqual(parm: string, value: string, offset: integer)'); // 336
  DEH_AddAction(@A_JumpIfWorldStringLess, 'A_JumpIfWorldStringLess(parm: string, value: string, offset: integer)'); // 337
  DEH_AddAction(@A_JumpIfWorldStringGreater, 'A_JumpIfWorldStringGreater(parm: string, value: string, offset: integer)'); // 338
  DEH_AddAction(@A_JumpIfWorldIntegerEqual, 'A_JumpIfWorldIntegerEqual(parm: string, value: integer, offset: integer)'); // 339
  DEH_AddAction(@A_JumpIfWorldIntegerLess, 'A_JumpIfWorldIntegerLess(parm: string, value: integer, offset: integer)'); // 340
  DEH_AddAction(@A_JumpIfWorldIntegerGreater, 'A_JumpIfWorldIntegerGreater(parm: string, value: integer, offset: integer)'); // 341
  DEH_AddAction(@A_JumpIfWorldFloatEqual, 'A_JumpIfWorldFloatEqual(parm: string, value: float, offset: integer)'); // 342
  DEH_AddAction(@A_JumpIfWorldFloatLess, 'A_JumpIfWorldFloatLess(parm: string, value: float, offset: integer)'); // 343
  DEH_AddAction(@A_JumpIfWorldFloatGreater, 'A_JumpIfWorldFloatGreater(parm: string, value: float, offset: integer)'); // 344
  DEH_AddAction(@A_GoToIfMapStringEqual, 'A_GoToIfMapStringEqual(parm: string, value: string, state: state_t)'); // 345
  DEH_AddAction(@A_GoToIfMapStringLess, 'A_GoToIfMapStringLess(parm: string, value: string, state: state_t)'); // 346
  DEH_AddAction(@A_GoToIfMapStringGreater, 'A_GoToIfMapStringGreater(parm: string, value: string, state: state_t)'); // 347
  DEH_AddAction(@A_GoToIfMapIntegerEqual, 'A_GoToIfMapIntegerEqual(parm: string, value: integer, state: state_t)'); // 348
  DEH_AddAction(@A_GoToIfMapIntegerLess, 'A_GoToIfMapIntegerLess(parm: string, value: integer, state: state_t)'); // 349
  DEH_AddAction(@A_GoToIfMapIntegerGreater, 'A_GoToIfMapIntegerGreater(parm: string, value: integer, state: state_t)'); // 350
  DEH_AddAction(@A_GoToIfMapFloatEqual, 'A_GoToIfMapFloatEqual(parm: string, value: float, state: state_t)'); // 351
  DEH_AddAction(@A_GoToIfMapFloatLess, 'A_GoToIfMapFloatLess(parm: string, value: float, state: state_t)'); // 352
  DEH_AddAction(@A_GoToIfMapFloatGreater, 'A_GoToIfMapFloatGreater(parm: string, value: float, state: state_t)'); // 353
  DEH_AddAction(@A_GoToIfWorldStringEqual, 'A_GoToIfWorldStringEqual(parm: string, value: string, state: state_t)'); // 354
  DEH_AddAction(@A_GoToIfWorldStringLess, 'A_GoToIfWorldStringLess(parm: string, value: string, state: state_t)'); // 355
  DEH_AddAction(@A_GoToIfWorldStringGreater, 'A_GoToIfWorldStringGreater(parm: string, value: string, state: state_t)'); // 356
  DEH_AddAction(@A_GoToIfWorldIntegerEqual, 'A_GoToIfWorldIntegerEqual(parm: string, value: integer, state: state_t)'); // 357
  DEH_AddAction(@A_GoToIfWorldIntegerLess, 'A_GoToIfWorldIntegerLess(parm: string, value: integer, state: state_t)'); // 358
  DEH_AddAction(@A_GoToIfWorldIntegerGreater, 'A_GoToIfWorldIntegerGreater(parm: string, value: integer, state: state_t)'); // 359
  DEH_AddAction(@A_GoToIfWorldFloatEqual, 'A_GoToIfWorldFloatEqual(parm: string, value: float, state: state_t)'); // 360
  DEH_AddAction(@A_GoToIfWorldFloatLess, 'A_GoToIfWorldFloatLess(parm: string, value: float, state: state_t)'); // 361
  DEH_AddAction(@A_GoToIfWorldFloatGreater, 'A_GoToIfWorldFloatGreater(parm: string, value: float, state: state_t)'); // 362
  DEH_AddAction(@A_SetMapStr, 'A_SetMapStr(mvar: string; value1: string; [value2: string],...)'); // 363
  DEH_AddAction(@A_SetWorldStr, 'A_SetWorldStr(wvar: string; value1: string; [value2: string],...)'); // 364
  DEH_AddAction(@A_SetMapInt, 'A_SetMapInt(mvar: string; value: integer)'); // 365
  DEH_AddAction(@A_SetWorldInt, 'A_SetWorldInt(wvar: string; value: integer)'); // 366
  DEH_AddAction(@A_SetMapFloat, 'A_SetMapFloat(mvar: string; value: float)'); // 367
  DEH_AddAction(@A_SetWorldFloat, 'A_SetWorldFloat(wvar: string; value: float)'); // 368
  DEH_AddAction(@A_RandomGoto, 'A_RandomGoto(state1: state_t; [state2: state_t],...)'); // 369
  DEH_AddAction(@A_ResetHealth, 'A_ResetHealth()'); // 370
  DEH_AddAction(@A_Recoil, 'A_Recoil(xymom: float)'); // 371
  DEH_AddAction(@A_SetSolid, 'A_SetSolid()'); // 372
  DEH_AddAction(@A_UnSetSolid, 'A_UnSetSolid()'); // 373
  DEH_AddAction(@A_SetFloat, 'A_SetFloat()'); // 374
  DEH_AddAction(@A_UnSetFloat, 'A_UnSetFloat()'); // 375
  DEH_AddAction(@A_SetHealth, 'A_SetHealth(h: integer)'); // 376
  DEH_AddAction(@A_ResetTargetHealth, 'A_ResetTargetHealth()'); // 377
  DEH_AddAction(@A_SetTargetHealth, 'A_SetTargetHealth(h: integer)'); // 378
  DEH_AddAction(@A_ScaleVelocity, 'A_ScaleVelocity(scale: float)'); // 379
  DEH_AddAction(@A_ChangeVelocity, 'A_ChangeVelocity(velx: float, vely: float, velz: float, flags: float)'); // 380
  DEH_AddAction(@A_JumpIf, 'A_JumpIf(propability: boolean, offset1: integer, [offset2: integer], ...)'); // 381
  DEH_AddAction(@A_MusicChanger, 'A_MusicChanger()'); // 382
  DEH_AddAction(@A_SetPushFactor, 'A_SetPushFactor(f: float)'); // 383
  DEH_AddAction(@A_SetScale, 'A_SetScale(s: float)'); // 384
  DEH_AddAction(@A_SetGravity, 'A_SetGravity(g: float)'); // 385
  DEH_AddAction(@A_SetFloorBounce, 'A_SetFloorBounce()'); // 386
  DEH_AddAction(@A_UnSetFloorBounce, 'A_UnSetFloorBounce()'); // 387
  DEH_AddAction(@A_SetCeilingBounce, 'A_SetCeilingBounce()'); // 388
  DEH_AddAction(@A_UnSetCeilingBounce, 'A_UnSetCeilingBounce()'); // 389
  DEH_AddAction(@A_SetWallBounce, 'A_SetWallBounce()'); // 390
  DEH_AddAction(@A_UnSetWallBounce, 'A_UnSetWallBounce()'); // 391
  DEH_AddAction(@A_GlowLight, 'A_GlowLight(color: string)'); // 392
  DEH_AddAction(@A_TraceNearestPlayer, 'A_TraceNearestPlayer(pct: integer, [maxturn: angle_t])'); // 393
  DEH_AddAction(@A_ChangeFlag, 'A_ChangeFlag(flag: string, onoff: boolean)'); // 394
  DEH_AddAction(@A_CheckFloor, 'A_CheckFloor(offset: integer)'); // 395
  DEH_AddAction(@A_CheckCeiling, 'A_CheckCeiling(offset: integer)'); // 396
  DEH_AddAction(@A_StopSound, 'A_StopSound()'); // 397
  DEH_AddAction(@A_JumpIfTargetOutsideMeleeRange, 'A_JumpIfTargetOutsideMeleeRange(offset: integer)'); // 398
  DEH_AddAction(@A_JumpIfTargetInsideMeleeRange, 'A_JumpIfTargetInsideMeleeRange(offset: integer)'); // 399
  DEH_AddAction(@A_JumpIfTracerCloser, 'A_JumpIfTracerCloser(distancetotarget: float, offset: integer)'); // 400
  DEH_AddAction(@A_SetMass, 'A_SetMass(mass: integer)'); // 401
  DEH_AddAction(@A_SetTargetMass, 'A_SetTargetMass(mass: integer)'); // 402
  DEH_AddAction(@A_SetTracerMass, 'A_SetTracerMass(mass: integer)'); // 403
  DEH_AddAction(@A_CheckSight, 'A_CheckSight(offset: integer)'); // 404
  DEH_AddAction(@A_CheckSightOrRange, 'A_CheckSightOrRange(distance: float, offset: integer, [twodi: boolean=false])'); // 405
  DEH_AddAction(@A_CheckRange, 'A_CheckRange(distance: float, offset: integer, [twodi: boolean=false])'); // 406
  DEH_AddAction(@A_CountdownArg, 'A_CountdownArg(arg: integer, offset: integer)'); // 407
  DEH_AddAction(@A_SetArg, 'A_SetArg(arg: integer, value: integer)'); // 408
  DEH_AddAction(@A_SetSpecial, 'A_SetSpecial(special: integer, [arg1, arg2, arg3, arg4, arg5: integer])'); // 409
  DEH_AddAction(@A_CheckFlag, 'A_CheckFlag(flag: string, offset: integer, [aaprt: AAPTR])'); // 410
  DEH_AddAction(@A_SetAngle, 'A_SetAngle(angle: integer, [flags: integer], [aaprt: AAPTR])'); // 411
  DEH_AddAction(@A_SetUserVar, 'A_SetUserVar(varname: string, value: integer)'); // 412
  DEH_AddAction(@A_SetUserArray, 'A_SetUserArray(varname: string, index: integer, value: integer)'); // 413
  DEH_AddAction(@A_SetTics, 'A_SetTics(tics: integer)'); // 414
  DEH_AddAction(@A_DropItem, 'A_DropItem(spawntype: string, amount: integer, chance: integer)'); // 415
  DEH_AddAction(@A_DamageSelf, 'A_DamageSelf(actor: Pmobj_t)'); // 416
  DEH_AddAction(@A_DamageTarget, 'A_DamageTarget(const damage: integer)'); // 417
  DEH_AddAction(@A_DamageTracer, 'A_DamageTracer(const damage: integer)'); // 418
  DEH_AddAction(@A_KillTarget, 'A_KillTarget()'); // 419
  DEH_AddAction(@A_KillTracer, 'A_KillTracer()'); // 420
  DEH_AddAction(@A_RemoveTarget, 'A_RemoveTarget([flags: integer])'); // 421
  DEH_AddAction(@A_RemoveTracer, 'A_RemoveTracer([flags: integer])'); // 422
  DEH_AddAction(@A_Remove, 'A_Remove(aaprt: AAPTR, [flags: integer])'); // 423
  DEH_AddAction(@A_SetFloatBobPhase, 'A_SetFloatBobPhase(bob: integer)'); // 424
  DEH_AddAction(@A_Detonate, 'A_Detonate()'); // 425
  DEH_AddAction(@A_Spawn, 'A_Spawn()'); // 426
  DEH_AddAction(@A_Face, 'A_Face()'); // 427
  DEH_AddAction(@A_Scratch, 'A_Scratch()'); // 428
  DEH_AddAction(@A_RandomJump, 'A_RandomJump()'); // 429
  DEH_AddAction(@A_FlipSprite, 'A_FlipSprite()'); // 430
  DEH_AddAction(@A_NoFlipSprite, 'A_NoFlipSprite()'); // 431
  DEH_AddAction(@A_RandomFlipSprite, 'A_RandomFlipSprite(chance: integer)'); // 432
  DEH_AddAction(@A_RandomNoFlipSprite, 'A_RandomNoFlipSprite(chance: integer)'); // 433
  DEH_AddAction(@A_CustomMeleeAttack, 'A_CustomMeleeAttack(damage: integer, meleesound: string, misssound: string)'); // 434
  DEH_AddAction(@A_CustomComboAttack, 'A_CustomComboAttack(missiletype: string, spawnheight: integer, damage: integer, meleesound: string)'); // 435
  DEH_AddAction(@A_SetRenderStyle, 'A_SetRenderStyle(style: renderstyle_t, alpha: float)'); // 436
  DEH_AddAction(@A_FadeTo, 'A_FadeTo(targ: integer, ammount: integer, flags: integer)'); // 437
  DEH_AddAction(@A_SetSize, 'A_SetSize(newradius: integer, newheight: integer, testpos: boolean)'); // 438
  DEH_AddAction(@A_RaiseMaster, 'A_RaiseMaster(copyfriendliness: boolean)'); // 439
  DEH_AddAction(@A_RaiseChildren, 'A_RaiseChildren(copyfriendliness: boolean)'); // 440
  DEH_AddAction(@A_RaiseSiblings, 'A_RaiseSiblings(copyfriendliness: boolean)'); // 441
  DEH_AddAction(@A_SetMasterMass, 'A_SetMasterMass(mass: integer)'); // 442
  DEH_AddAction(@A_KillMaster, 'A_KillMaster()'); // 443
  DEH_AddAction(@A_DamageMaster, 'A_DamageMaster(const damage: integer)'); // 444
  DEH_AddAction(@A_HealThing, 'A_HealThing(amount: integer, max: integer)'); // 445
  DEH_AddAction(@A_RemoveMaster, 'A_RemoveMaster([flags: integer])'); // 446
  DEH_AddAction(@A_BasicAttack, 'A_BasicAttack(MeleeDamage: integer, MeleeSound: integer, MissileType: integer, MissileHeight: float)'); // 447
  DEH_AddAction(@A_SetMasterArg, 'A_SetMasterArg(arg: integer; value: integer)'); // 448
  DEH_AddAction(@A_SetTargetArg, 'A_SetTargetArg(arg: integer; value: integer)'); // 449
  DEH_AddAction(@A_SetTracerArg, 'A_SetTracerArg(arg: integer; value: integer)'); // 450
  DEH_AddAction(@A_Tracer2, 'A_Tracer()'); // 451
  DEH_AddAction(@A_Tracer2, 'A_Tracer2()'); // 452
  DEH_AddAction(@A_MonsterRefire, 'A_MonsterRefire(prob: integer, offset: state_t)'); // 453
  DEH_AddAction(@A_RearrangePointers, 'A_RearrangePointers(ptr_target: integer, ptr_master: integer, ptr_tracer: integer, flags: integer)'); // 454
  DEH_AddAction(@A_TransferPointer, 'A_TransferPointer(ptr_source: integer, ptr_recipient: integer, ptr_sourcefield: integer, [ptr_recipientfield: integer], [flags: integer])'); // 455
  DEH_AddAction(@A_AlertMonsters, 'A_AlertMonsters(maxdist: integer, flags: integer)'); // 456
  DEH_AddAction(@A_LocalEarthQuake, 'A_LocalEarthQuake(tics: integer; [intensity: float = 1.0]; [maxdist: float = MAXINT] ;)'); // 457
  DEH_AddAction(@A_RemoveChildren, 'A_RemoveChildren([flags: integer])'); // 458
  DEH_AddAction(@A_RemoveSiblings, 'A_RemoveSiblings([flags: integer])'); // 459
  DEH_AddAction(@A_KillChildren, 'A_KillChildren()'); // 460
  DEH_AddAction(@A_KillSiblings, 'A_KillSiblings()'); // 461
  DEH_AddAction(@A_Weave, 'A_Weave(xyspeed: integer = 2, zspeed: integer = 2, xydist: float = 2.0, zdist: float = 1.0)'); // 462
  DEH_AddAction(@A_SetWeaveIndexXY, 'A_SetWeaveIndexXY(weavexy: integer)'); // 463
  DEH_AddAction(@A_SetWeaveIndexZ, 'A_SetWeaveIndexZ(weavez: integer)'); // 464
  DEH_AddAction(@A_SetWeaveIndexes, 'A_SetWeaveIndexes(weavexy: integer, weavez: integer)'); // 465
  DEH_AddAction(@A_SetHeight, 'A_SetHeight(newheight: float)'); // 466
  DEH_AddAction(@A_OverlayClear, 'A_OverlayClear()'); // 467
  DEH_AddAction(@A_OverlayDrawPatch, 'A_OverlayDrawPatch(ticks: Integer; patchname: string; x, y: Integer ;)'); // 468
  DEH_AddAction(@A_OverlayDrawPatchStretched, 'A_OverlayDrawPatchStretched(ticks: Integer; patchname: string; x1, y1, x2, y2: Integer ;)'); // 469
  DEH_AddAction(@A_OverlayDrawPixel, 'A_OverlayDrawPixel(ticks: Integer; red, green, blue: byte; x, y: Integer ;)'); // 470
  DEH_AddAction(@A_OverlayDrawRect, 'A_OverlayDrawRect(ticks: Integer; red, green, blue: byte; x1, y1, x2, y2: Integer ;)'); // 471
  DEH_AddAction(@A_OverlayDrawLine, 'A_OverlayDrawLine(ticks: Integer; red, green, blue: byte; x1, y1, x2, y2: Integer ;)'); // 472
  DEH_AddAction(@A_OverlayDrawText, 'A_OverlayDrawText(ticks: Integer; txt: string; align: Integer; x, y: Integer ;)'); // 473
  DEH_AddAction(@A_OverlayDrawLeftText, 'A_OverlayDrawLeftText(ticks: Integer; txt: string; x, y: Integer ;)'); // 474
  DEH_AddAction(@A_OverlayDrawRightText, 'A_OverlayDrawRightText(ticks: Integer; txt: string; x, y: Integer ;)'); // 475
  DEH_AddAction(@A_OverlayDrawCenterText, 'A_OverlayDrawCenterText(ticks: Integer; txt: string; x, y: Integer ;)'); // 476
  DEH_AddAction(@A_SetFriction, 'A_SetFriction(newfriction: float)'); // 477
  DEH_AddAction(@A_PlayerHurtExplode, 'A_PlayerHurtExplode(damage: integer, radius: integer)'); // 478
  DEH_AddAction(@A_SetPushable, 'A_SetPushable()'); // 479
  DEH_AddAction(@A_UnSetPushable, 'A_UnSetPushable()'); // 480
  DEH_AddAction(@A_SetPainChance, 'A_SetPainChance(newchance: integer)'); // 481
  DEH_AddAction(@A_SetSpriteDX, 'A_SetSpriteDX(dx: float)'); // 482
  DEH_AddAction(@A_SetSpriteDY, 'A_SetSpriteDY(dy: float)'); // 483
  DEH_AddAction(@A_SeeSound1, 'A_SeeSound()'); // 484
  DEH_AddAction(@A_PainSound1, 'A_PainSound()'); // 485
  DEH_AddAction(@A_AttackSound1, 'A_AttackSound()'); // 486
  DEH_AddAction(@A_MeleeSound1, 'A_MeleeSound()'); // 487
  DEH_AddAction(@A_DeathSound1, 'A_DeathSound()'); // 488
  DEH_AddAction(@A_ActiveSound1, 'A_ActiveSound()'); // 489
  DEH_AddAction(@A_MatchTargetZ, 'A_MatchTargetZ(zspeed: integer; threshold: integer; maxmomz: integer)'); // 490
  DEH_AddAction(@A_SetInteractive, 'A_SetInteractive()'); // 491
  DEH_AddAction(@A_UnSetInteractive, 'A_UnSetInteractive()'); // 492
  DEH_AddAction(@A_SimpleDialog, 'A_SimpleDialog(dialog1: string; [dialog2...])'); // 493

  for i := 0 to dehnumactions - 1 do
    DEH_AddActionToHash(deh_actions[i].name, i);

  weapontype_tokens := TDTextList.Create;
  for i := 0 to Ord(NUMWEAPONS) do
    weapontype_tokens.Add(strupper(GetENumName(TypeInfo(weapontype_t), i)));

  ammotype_tokens := TDTextList.Create;
  for i := 0 to Ord(MANA_NONE) do
    ammotype_tokens.Add(strupper(GetENumName(TypeInfo(manatype_t), i)));

  playerclass_tokens := TDTextList.Create;
  for i := 0 to Ord(NUMCLASSES) - 1 do
    playerclass_tokens.Add(strupper(GetENumName(TypeInfo(pclass_t), i)));

  deh_strings.numstrings := 0;
  deh_strings.realnumstrings := 0;
  deh_strings._array := nil;

  for i := 0 to 2 do
      DEH_AddString(@deh_strings, @fourthWeaponText[i], 'FOURTHWEAPONTEXT_' + itoa(i));
  for i := 0 to 2 do
      DEH_AddString(@deh_strings, @weaponPieceText[i], 'WEAPONPIECETEXT_' + itoa(i));

  DEH_AddString(@deh_strings, @pg_CREDIT, 'PAGE_CREDIT');
  DEH_AddString(@deh_strings, @pg_TITLE, 'PAGE_TITLE');
  DEH_AddString(@deh_strings, @pg_HELP1, 'PAGE_HELP1');
  DEH_AddString(@deh_strings, @pg_HELP2, 'PAGE_HELP2');

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
  DEH_AddString(@deh_strings, @STSTR_NCON, 'STSTR_NCON');
  DEH_AddString(@deh_strings, @STSTR_NCOFF, 'STSTR_NCOFF');
  DEH_AddString(@deh_strings, @STSTR_CHOPPERS, 'STSTR_CHOPPERS');
  DEH_AddString(@deh_strings, @STSTR_CLEV, 'STSTR_CLEV');
  DEH_AddString(@deh_strings, @STSTR_LGON, 'STSTR_LGON');
  DEH_AddString(@deh_strings, @STSTR_LGOFF, 'STSTR_LGOFF');


  for i := 0 to Ord(NUMKEYCARDS) - 1 do
      DEH_AddString(@deh_strings, @TextKeyMessages[i], 'TXT_GOTKEY_' + itoa(i));

// Artifacts

  DEH_AddString(@deh_strings, @artifactMessages[1], 'TXT_ARTIINVULNERABILITY');
  DEH_AddString(@deh_strings, @artifactMessages[2], 'TXT_ARTIHEALTH');
  DEH_AddString(@deh_strings, @artifactMessages[3], 'TXT_ARTISUPERHEALTH');
  DEH_AddString(@deh_strings, @artifactMessages[4], 'TXT_ARTIHEALINGRADIUS');
  DEH_AddString(@deh_strings, @artifactMessages[5], 'TXT_ARTISUMMON');
  DEH_AddString(@deh_strings, @artifactMessages[6], 'TXT_ARTITORCH');
  DEH_AddString(@deh_strings, @artifactMessages[7], 'TXT_ARTIEGG');
  DEH_AddString(@deh_strings, @artifactMessages[8], 'TXT_ARTIFLY');
  DEH_AddString(@deh_strings, @artifactMessages[9], 'TXT_ARTIBLASTRADIUS');
  DEH_AddString(@deh_strings, @artifactMessages[10], 'TXT_ARTIPOISONBAG');
  DEH_AddString(@deh_strings, @artifactMessages[11], 'TXT_ARTITELEPORTOTHER');
  DEH_AddString(@deh_strings, @artifactMessages[12], 'TXT_ARTISPEED');
  DEH_AddString(@deh_strings, @artifactMessages[13], 'TXT_ARTIBOOSTMANA');
  DEH_AddString(@deh_strings, @artifactMessages[14], 'TXT_ARTIBOOSTARMOR');
  DEH_AddString(@deh_strings, @artifactMessages[15], 'TXT_ARTITELEPORT');
  DEH_AddString(@deh_strings, @artifactMessages[16], 'TXT_ARTIPUZZSKULL');
  DEH_AddString(@deh_strings, @artifactMessages[17], 'TXT_ARTIPUZZGEMBIG');
  DEH_AddString(@deh_strings, @artifactMessages[18], 'TXT_ARTIPUZZGEMRED');
  DEH_AddString(@deh_strings, @artifactMessages[19], 'TXT_ARTIPUZZGEMGREEN1');
  DEH_AddString(@deh_strings, @artifactMessages[20], 'TXT_ARTIPUZZGEMGREEN2');
  DEH_AddString(@deh_strings, @artifactMessages[21], 'TXT_ARTIPUZZGEMBLUE1');
  DEH_AddString(@deh_strings, @artifactMessages[22], 'TXT_ARTIPUZZGEMBLUE2');
  DEH_AddString(@deh_strings, @artifactMessages[23], 'TXT_ARTIPUZZBOOK1');
  DEH_AddString(@deh_strings, @artifactMessages[24], 'TXT_ARTIPUZZBOOK2');
  DEH_AddString(@deh_strings, @artifactMessages[25], 'TXT_ARTIPUZZSKULL2');
  DEH_AddString(@deh_strings, @artifactMessages[26], 'TXT_ARTIPUZZFWEAPON');
  DEH_AddString(@deh_strings, @artifactMessages[27], 'TXT_ARTIPUZZCWEAPON');
  DEH_AddString(@deh_strings, @artifactMessages[28], 'TXT_ARTIPUZZMWEAPON');
  DEH_AddString(@deh_strings, @artifactMessages[29], 'TXT_ARTIPUZZGEAR');  // All gear pickups use the same text
  DEH_AddString(@deh_strings, @artifactMessages[30], 'TXT_ARTIPUZZGEAR');
  DEH_AddString(@deh_strings, @artifactMessages[31], 'TXT_ARTIPUZZGEAR');
  DEH_AddString(@deh_strings, @artifactMessages[32], 'TXT_ARTIPUZZGEAR');
  DEH_AddString(@deh_strings, @TXT_ARTIALL, 'TXT_ARTIALL');
  DEH_AddString(@deh_strings, @TXT_ARTIALLPUZZLE, 'TXT_ARTIALLPUZZLE');

// Items

  DEH_AddString(@deh_strings, @TXT_ITEMHEALTH, 'TXT_ITEMHEALTH');
  DEH_AddString(@deh_strings, @TXT_ITEMBAGOFHOLDING, 'TXT_ITEMBAGOFHOLDING');
  DEH_AddString(@deh_strings, @TXT_ITEMSHIELD1, 'TXT_ITEMSHIELD1');
  DEH_AddString(@deh_strings, @TXT_ITEMSHIELD2, 'TXT_ITEMSHIELD2');
  DEH_AddString(@deh_strings, @TXT_ITEMSUPERMAP, 'TXT_ITEMSUPERMAP');

// Weapons

  DEH_AddString(@deh_strings, @TXT_WEAPON_F2, 'TXT_WEAPON_F2');
  DEH_AddString(@deh_strings, @TXT_WEAPON_F3, 'TXT_WEAPON_F3');
  DEH_AddString(@deh_strings, @TXT_WEAPON_F4, 'TXT_WEAPON_F4');
  DEH_AddString(@deh_strings, @TXT_WEAPON_C2, 'TXT_WEAPON_C2');
  DEH_AddString(@deh_strings, @TXT_WEAPON_C3, 'TXT_WEAPON_C3');
  DEH_AddString(@deh_strings, @TXT_WEAPON_C4, 'TXT_WEAPON_C4');
  DEH_AddString(@deh_strings, @TXT_WEAPON_M2, 'TXT_WEAPON_M2');
  DEH_AddString(@deh_strings, @TXT_WEAPON_M3, 'TXT_WEAPON_M3');
  DEH_AddString(@deh_strings, @TXT_WEAPON_M4, 'TXT_WEAPON_M4');
  DEH_AddString(@deh_strings, @TXT_QUIETUS_PIECE, 'TXT_QUIETUS_PIECE');
  DEH_AddString(@deh_strings, @TXT_WRAITHVERGE_PIECE, 'TXT_WRAITHVERGE_PIECE');
  DEH_AddString(@deh_strings, @TXT_BLOODSCOURGE_PIECE, 'TXT_BLOODSCOURGE_PIECE');

// Cheats

  DEH_AddString(@deh_strings, @TXT_CHEATGODON, 'TXT_CHEATGODON');
  DEH_AddString(@deh_strings, @TXT_CHEATGODOFF, 'TXT_CHEATGODOFF');
  DEH_AddString(@deh_strings, @TXT_CHEATNOCLIPON, 'TXT_CHEATNOCLIPON');
  DEH_AddString(@deh_strings, @TXT_CHEATNOCLIPOFF, 'TXT_CHEATNOCLIPOFF');
  DEH_AddString(@deh_strings, @TXT_CHEATWEAPONS, 'TXT_CHEATWEAPONS');
  DEH_AddString(@deh_strings, @TXT_CHEATFLIGHTON, 'TXT_CHEATFLIGHTON');
  DEH_AddString(@deh_strings, @TXT_CHEATFLIGHTOFF, 'TXT_CHEATFLIGHTOFF');
  DEH_AddString(@deh_strings, @TXT_CHEATPOWERON, 'TXT_CHEATPOWERON');
  DEH_AddString(@deh_strings, @TXT_CHEATPOWEROFF, 'TXT_CHEATPOWEROFF');
  DEH_AddString(@deh_strings, @TXT_CHEATHEALTH, 'TXT_CHEATHEALTH');
  DEH_AddString(@deh_strings, @TXT_CHEATKEYS, 'TXT_CHEATKEYS');
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
  DEH_AddString(@deh_strings, @TXT_CHEATPIG, 'TXT_CHEATPIG');

  DEH_AddString(@deh_strings, @MSG_MODIFIEDGAME, 'MSG_MODIFIEDGAME');
  DEH_AddString(@deh_strings, @MSG_HEXDD, 'MSG_HEXDD');
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
  DEH_AddString(@deh_strings, @MSGOFF, 'MSGOFF');
  DEH_AddString(@deh_strings, @MSGON, 'MSGON');
  DEH_AddString(@deh_strings, @NETEND, 'NETEND');
  DEH_AddString(@deh_strings, @SENDGAME, 'SENDGAME');

  DEH_AddString(@deh_strings, @DOSY, 'DOSY');

  DEH_AddString(@deh_strings, @GGSAVED, 'GGSAVED');
  DEH_AddString(@deh_strings, @GACCESSDENIEDDEMO, 'GACCESSDENIEDDEMO');
  DEH_AddString(@deh_strings, @SAVEGAMENAME, 'SAVEGAMENAME');
  DEH_AddString(@deh_strings, @SAVEGAMEMAP, 'SAVEGAMEMAP');
  DEH_AddString(@deh_strings, @SAVEGAMENAMEDD, 'SAVEGAMENAMEDD');
  DEH_AddString(@deh_strings, @SAVEGAMEMAPDD, 'SAVEGAMEMAPDD');
  DEH_AddString(@deh_strings, @SAVEPATH, 'SAVEPATH');


  weapon_tokens := TDTextList.Create;

  weapon_tokens.Add('MANA TYPE');           // .mana
  weapon_tokens.Add('DESELECT FRAME');      // .upstate
  weapon_tokens.Add('SELECT FRAME');        // .downstate
  weapon_tokens.Add('BOBBING FRAME');       // .readystate
  weapon_tokens.Add('SHOOTING FRAME');      // .atkstate
  weapon_tokens.Add('FIRING FRAME');        // .flashstate
  weapon_tokens.Add('HOLD SHOOTING FRAME'); // .holdatkstate


  sound_tokens := TDTextList.Create;

  sound_tokens.Add('VALUE');
  sound_tokens.Add('NAME'); // DelphiDoom specific


  renderstyle_tokens := TDTextList.Create;

  renderstyle_tokens.Add('NORMAL');
  renderstyle_tokens.Add('TRANSLUCENT');
  renderstyle_tokens.Add('ADD');
  renderstyle_tokens.Add('SUBTRACT');


  misc_tokens := TDTextList.Create;

  misc_tokens.Add('MAX MORPH HEALTH');    // p_maxmorphhealth
  misc_tokens.Add('MAX ARTIFACTS');       // p_maxartifacts
  misc_tokens.Add('QUARTZ FLASK HEALTH'); // p_quartzflaskhealth
  misc_tokens.Add('MYSTIC URN HEALTH');   // p_mysticurnhealth
  misc_tokens.Add('MAX MANA');            // MAX_MANA


  C_AddCmd('DEH_ParseFile, BEX_ParseFile', @DEH_ParseFile);
  C_AddCmd('DEH_ParseLump, BEX_ParseLump', @DEH_ParseLumpName);
  C_AddCmd('DEH_PrintCurrentSettings, DEH_PrintSettings, BEX_PrintCurrentSettings, BEX_PrintSettings', @DEH_PrintCurrentSettings);
  C_AddCmd('DEH_SaveCurrentSettings, DEH_SaveToFile, BEX_SaveCurrentSettings, BEX_SaveToFile', @DEH_SaveCurrentSettings);
  C_AddCmd('DEH_SaveMobjInfoCSV, BEX_SaveMobjInfoCSV', @DEH_SaveMobjInfoCSV);
  C_AddCmd('DEH_SaveStatesCSV, BEX_SaveStatesCSV', @DEH_SaveStatesCSV);
  C_AddCmd('DEH_SaveSpritesCSV, BEX_SaveSpritesCSV', @DEH_SaveSpritesCSV);
  C_AddCmd('DEH_PrintActions, DEH_ShowActions, BEX_PrintActions, BEX_ShowActions', @DEH_PrintActions);
end;

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
  FreeAndNil(state_flags_ex);
  FreeAndNil(state_tokens);
  FreeAndNil(weapon_tokens);
  FreeAndNil(sound_tokens);
  FreeAndNil(renderstyle_tokens);
  FreeAndNil(misc_tokens);
  FreeAndNil(weapontype_tokens);
  FreeAndNil(ammotype_tokens);
  FreeAndNil(playerclass_tokens);

  FreeAndNil(mobj_tokens_hash);
  FreeAndNil(mobj_flags_hash);
  FreeAndNil(mobj_flags2_hash);
  FreeAndNil(mobj_flags_ex_hash);
  FreeAndNil(mobj_flags2_ex_hash);
  FreeAndNil(mobj_flags3_ex_hash);
  FreeAndNil(mobj_flags4_ex_hash);

  DEH_ShutDownActionsHash;

  realloc(pointer(deh_strings._array), deh_strings.realnumstrings * SizeOf(deh_string_t), 0);
end;

end.

