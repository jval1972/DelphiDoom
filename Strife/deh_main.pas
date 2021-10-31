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
  DEHNUMACTIONS = 391;

type
  deh_action_t = record
    action: actionf_t;
    name: string;
    {$IFDEF DLL}decl: string;{$ENDIF}
  end;
  Pdeh_action_t = ^deh_action_t;

type
  deh_string_t = record
    pstr: PString;
    original: string[255];
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
  mobj_flags_ex: TDTextList;
  mobj_flags2_ex: TDTextList;
  mobj_flags3_ex: TDTextList;
  mobj_flags4_ex: TDTextList;
  state_tokens: TDTextList;
  state_flags_ex: TDTextList;
  ammo_tokens: TDTextList;
  weapon_tokens: TDTextList;
  sound_tokens: TDTextList;
  renderstyle_tokens: TDTextList;
  misc_tokens: TDTextList;
  weapontype_tokens: TDTextList;
  ammotype_tokens: TDTextList;

  deh_actions: array[0..DEHNUMACTIONS - 1] of deh_action_t;
  deh_strings: deh_strings_t;

function Deh_ActionName(action: actionf_t): string;

function DEH_GetString(const s: string): string;

implementation

uses
  TypInfo,
  c_cmds,
  doomdef,
  deh_base,
  d_main,
  d_items,
  d_englsh,
  dstrings,
  e_endoom,
  f_finale,
  g_game,
  hu_stuff,
  i_system,
  info_h,
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
  p_inter,
  p_musinfo,
  psi_overlay,
  r_renderstyle,
  sounds,
  sc_params,
  sc_engine,
  sc_states,
  v_data,
  w_wad,
  w_folders,
  w_pak;

var
  mobj_tokens_hash: TDEHStringsHashTable;
  mobj_flags_hash: TDEHStringsHashTable;
  mobj_flags_ex_hash: TDEHStringsHashTable;
  mobj_flags2_ex_hash: TDEHStringsHashTable;
  mobj_flags3_ex_hash: TDEHStringsHashTable;
  mobj_flags4_ex_hash: TDEHStringsHashTable;


var
  stringhash: array[0..255] of TDNumberList;

function loselose(const s: string): byte;
var
  i: integer;
begin
  result := 0;
  for i := 1 to Length(s) do
    result := result + Ord(s[i]);
end;

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
  deh_strings._array[deh_strings.numstrings].original := pstr^;
  deh_strings._array[deh_strings.numstrings].name := name;
  stringhash[loselose(pstr^)].Add(deh_strings.numstrings);
  inc(deh_strings.numstrings);
end;

var
  deh_initialized: boolean = false;

procedure DEH_Parse(const s: TDStringList);
var
  i, j: integer;
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

        if mobj_idx in [1, 3, 7, 10, 11, 12, 13, 23, 38, 40, 41] then
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
                      if mobj_setflag = -1 then
                        mobj_setflag := 0;
                      mobj_flag := _SHL(1, mobj_flag);
                      mobj_setflag := mobj_setflag or mobj_flag;
                    end;
                    token2 := strtrim(token4);
                  until token2 = '';
                  if mobj_setflag <> -1 then
                    mobjinfo[mobj_no].flags := mobj_setflag;

                end;
              end;
          22: begin
                I_Warning('DEH_Parse(): Unsupported Doom field: "%s".'#13#10, [mobj_tokens[mobj_idx]]);
              end;
          23: mobjinfo[mobj_no].raisestate := mobj_val;
          24: begin
                if mobj_val >= 0 then
                  mobjinfo[mobj_no].flags_ex := mobj_val  // DelphiDoom specific (lighting, transparency, etc)
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
                  mobjinfo[mobj_no].flags2_ex := mobj_val  // DelphiDoom specific
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
          41: mobjinfo[mobj_no].interactstate := mobj_val;
          42: mobjinfo[mobj_no].missileheight := mobj_val;
          43: begin
                for j := 1 to length(token2) do
                begin
                  mobjinfo[mobj_no].name2[j - 1] := token2[j];
                  if j = MOBJINFONAMESIZE then
                    break;
                end;
                for j := length(token2) to MOBJINFONAMESIZE - 1 do
                  mobjinfo[mobj_no].name2[j] := #0;
              end;
          44: mobjinfo[mobj_no].vspeed := mobj_val;
          45: mobjinfo[mobj_no].pushfactor := DEH_FixedOrFloat(token2, 64);
          46: mobjinfo[mobj_no].scale := DEH_FixedOrFloat(token2, 64);
          47: mobjinfo[mobj_no].gravity := DEH_FixedOrFloat(token2, 64);
          48: begin
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
          49: begin
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
          50: mobjinfo[mobj_no].minmissilechance := mobj_val;
          51: mobjinfo[mobj_no].floatspeed := mobj_val;
          52: mobjinfo[mobj_no].normalspeed := mobj_val;
          53: mobjinfo[mobj_no].fastspeed := mobj_val;
          54: mobjinfo[mobj_no].obituary := token2;
          55: mobjinfo[mobj_no].hitobituary := token2;
          56: begin
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
          57: mobjinfo[mobj_no].meleerange := mobj_val;
          58: mobjinfo[mobj_no].maxstepheight := DEH_FixedOrFloat(token2, 64);
          59: mobjinfo[mobj_no].maxdropoffheight := DEH_FixedOrFloat(token2, 64);
          60: mobjinfo[mobj_no].gibhealth := mobj_val;
          61: mobjinfo[mobj_no].maxtargetrange := mobj_val;
          62: mobjinfo[mobj_no].WeaveIndexXY := mobj_val;
          63: mobjinfo[mobj_no].WeaveIndexZ := mobj_val;
          64: mobjinfo[mobj_no].friction := DEH_FixedOrFloat(token2, 64);
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
                if state_val < DEHNUMACTIONS then
                  states[state_no].action.acp1 := deh_actions[state_val].action.acp1
                else
                  I_Warning('DEH_Parse(): Wrong action number = %d in state %d (Must be in [0..%d])'#13#10, [state_val, state_no, DEHNUMACTIONS]);
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
                  for j := 0 to DEHNUMACTIONS - 1 do
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
      stmp := firstword(token2);
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
        if Pos('=', str) = 0 then
        begin
          mustnextline := false; // Already got line
          break;
        end;

        splitstring(str, token1, token2, '=');
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
           1: clipammo[ammo_no] := ammo_val;
        end;

      end;
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
           0: weaponinfo[weapon_no].ammo := ammotype_t(weapon_val);
           1: weaponinfo[weapon_no].upstate := weapon_val;
           2: weaponinfo[weapon_no].downstate := weapon_val;
           3: weaponinfo[weapon_no].readystate := weapon_val;
           4: weaponinfo[weapon_no].atkstate := weapon_val;
           6: weaponinfo[weapon_no].holdatkstate := weapon_val;
           5: weaponinfo[weapon_no].flashstate := weapon_val;
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
          0: p_maxhealth := misc_val;
          1: p_soulspherehealth := misc_val;
          2: p_megaspherehealth := misc_val;
          3: p_medikithealth := misc_val;
          4: p_stimpackhealth := misc_val;
          5: p_bonushealth := misc_val;
          6: p_maxarmor := misc_val;
          7: p_greenarmorclass := misc_val;
          8: p_bluearmorclass := misc_val;
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
          if state_val < DEHNUMACTIONS then
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
            for j := 0 to DEHNUMACTIONS - 1 do
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
            else if 'D_' + stmp = token1 then // JVAL Allow D_ prefix for checking
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
      for i := 0 to numstates - 1 do
        code_ptrs[i] := states[i].action;

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

    if i = 0 then
    begin
      result.Add('');
      result.Add('# The following fields are DelphiStrife specific enhancements');
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
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[41]), mobjinfo[i].interactstate]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[42]), mobjinfo[i].missileheight]);
    result.Add('%s = %s', [capitalizedstring(mobj_tokens[43]), PascalText(mobjinfo[i].name2, MOBJINFONAMESIZE)]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[44]), mobjinfo[i].vspeed]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[45]), mobjinfo[i].pushfactor]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[46]), mobjinfo[i].scale]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[47]), mobjinfo[i].gravity]);

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
      result.Add('%s = 0', [capitalizedstring(mobj_tokens[48])])
    else
      result.Add('%s = %s', [capitalizedstring(mobj_tokens[48]), str]);

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
      result.Add('%s = 0', [capitalizedstring(mobj_tokens[49])])
    else
      result.Add('%s = %s', [capitalizedstring(mobj_tokens[49]), str]);

    result.Add('%s = %d', [capitalizedstring(mobj_tokens[50]), mobjinfo[i].minmissilechance]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[51]), mobjinfo[i].floatspeed]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[52]), mobjinfo[i].normalspeed]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[53]), mobjinfo[i].fastspeed]);
    result.Add('%s = "%s"', [capitalizedstring(mobj_tokens[54]), mobjinfo[i].obituary]);
    result.Add('%s = "%s"', [capitalizedstring(mobj_tokens[55]), mobjinfo[i].hitobituary]);
    result.Add('%s = %s', [capitalizedstring(mobj_tokens[56]), GENDERINFO[Ord(mobjinfo[i].gender)].name]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[57]), mobjinfo[i].meleerange]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[58]), mobjinfo[i].maxstepheight]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[59]), mobjinfo[i].maxdropoffheight]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[60]), mobjinfo[i].gibhealth]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[61]), mobjinfo[i].maxtargetrange]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[62]), mobjinfo[i].WeaveIndexXY]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[63]), mobjinfo[i].WeaveIndexZ]);
    result.Add('%s = %d', [capitalizedstring(mobj_tokens[64]), mobjinfo[i].friction]);

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
    for j := 0 to DEHNUMACTIONS - 1 do
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
      result.Add('# Flags_ex declares transparency and light effects');

    result.Add('');
  end;


  //////////////////////////////////////////////////////////////////////////////
  // Add Weapons
  //////////////////////////////////////////////////////////////////////////////
  result.Add('');
  result.Add('# Weapons');
  result.Add('');
  for i := 0 to Ord(NUMWEAPONS) - 1 do
  begin
    result.Add('Weapon %s', [weapontype_tokens.Strings[i]]);

    result.Add('%s = %s', [capitalizedstring(weapon_tokens[0]), ammotype_tokens.Strings[Ord(weaponinfo[i].ammo)]]);
    result.Add('%s = %d', [capitalizedstring(weapon_tokens[1]), weaponinfo[i].upstate]);
    result.Add('%s = %d', [capitalizedstring(weapon_tokens[2]), weaponinfo[i].downstate]);
    result.Add('%s = %d', [capitalizedstring(weapon_tokens[3]), weaponinfo[i].readystate]);
    result.Add('%s = %d', [capitalizedstring(weapon_tokens[4]), weaponinfo[i].atkstate]);
    result.Add('%s = %d', [capitalizedstring(weapon_tokens[6]), weaponinfo[i].holdatkstate]);
    result.Add('%s = %d', [capitalizedstring(weapon_tokens[5]), weaponinfo[i].flashstate]);

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

  for i := 0 to 255 do
    stringhash[i] := TDNumberList.Create;

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
  mobj_tokens.Add('CRASH FRAME');        // .crashstate (DelphiDoom)  // 40
  mobj_tokens.Add('INTERACT FRAME');     // .interactstate (DelphiDoom) // 41
  mobj_tokens.Add('MISSILEHEIGHT');      // .missileheight (DelphiDoom) // 42
  mobj_tokens.Add('NAME2');              // .name2 (Strife)           // 43
  mobj_tokens.Add('VSPEED');             // .vspeed                   // 44
  mobj_tokens.Add('PUSHFACTOR');         // .pushfactor               // 45
  mobj_tokens.Add('SCALE');              // .scale                    // 46
  mobj_tokens.Add('GRAVITY');            // .gravity                  // 47
  mobj_tokens.Add('FLAGS3_EX');          // .flags3_ex (DelphiDoom)   // 48
  mobj_tokens.Add('FLAGS4_EX');          // .flags4_ex (DelphiDoom)   // 49
  mobj_tokens.Add('MINMISSILECHANCE');   // .minmissilechance         // 50
  mobj_tokens.Add('FLOAT SPEED');        // .floatspeed               // 51
  mobj_tokens.Add('NORMAL SPEED');       // .normalspeed              // 52
  mobj_tokens.Add('FAST SPEED');         // .fastspeed                // 53
  mobj_tokens.Add('OBITUARY');           // .obituary                 // 54
  mobj_tokens.Add('HIT OBITUARY');       // .hitobituary              // 55
  mobj_tokens.Add('GENDER');             // .gender                   // 56
  mobj_tokens.Add('MELEE RANGE');        // .meleerange               // 57
  mobj_tokens.Add('MAX STEP HEIGHT');    // .maxstepheight            // 58
  mobj_tokens.Add('MAX DROPOFF HEIGHT'); // .maxdropoffheight         // 59
  mobj_tokens.Add('GIB HEALTH');         // .gibhealth                // 60
  mobj_tokens.Add('MAX TARGET RANGE');   // .maxtargetrange           // 61
  mobj_tokens.Add('WEAVE INDEX XY');     // .WeaveIndexXY             // 62
  mobj_tokens.Add('WEAVE INDEX Z');      // .WeaveIndexZ              // 63
  mobj_tokens.Add('FRICTION');           // .Friction                 // 63

  mobj_tokens_hash := TDEHStringsHashTable.Create;
  mobj_tokens_hash.AssignList(mobj_tokens);


  mobj_flags := TDTextList.Create;
  mobj_flags.Add('MF_SPECIAL');
  mobj_flags.Add('MF_SOLID');
  mobj_flags.Add('MF_SHOOTABLE');
  mobj_flags.Add('MF_NOSECTOR');
  mobj_flags.Add('MF_NOBLOCKMAP');
  mobj_flags.Add('MF_STAND');
  mobj_flags.Add('MF_JUSTHIT');
  mobj_flags.Add('MF_JUSTATTACKED');
  mobj_flags.Add('MF_SPAWNCEILING');
  mobj_flags.Add('MF_NOGRAVITY');
  mobj_flags.Add('MF_DROPOFF');
  mobj_flags.Add('MF_GIVEQUEST');
  mobj_flags.Add('MF_NOCLIP');
  mobj_flags.Add('MF_FEETCLIPPED');
  mobj_flags.Add('MF_FLOAT');
  mobj_flags.Add('MF_NODIALOG');
  mobj_flags.Add('MF_MISSILE');
  mobj_flags.Add('MF_DROPPED');
  mobj_flags.Add('MF_SHADOW');
  mobj_flags.Add('MF_NOBLOOD');
  mobj_flags.Add('MF_CORPSE');
  mobj_flags.Add('MF_INFLOAT');
  mobj_flags.Add('MF_COUNTKILL');
  mobj_flags.Add('MF_AMBUSH');
  mobj_flags.Add('MF_BOUNCE');
  mobj_flags.Add('MF_NOTDMATCH');
  mobj_flags.Add('MF_ALLY');
  mobj_flags.Add('MF_MVIS');
  mobj_flags.Add('MF_COLORSWAP1');
  mobj_flags.Add('MF_COLORSWAP2');
  mobj_flags.Add('MF_COLORSWAP3');
  mobj_flags.Add('MF_SPECTRAL');

  mobj_flags_hash := TDEHStringsHashTable.Create;
  mobj_flags_hash.AssignList(mobj_flags);


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
  mobj_flags2_ex.Add('MF2_EX_NOHITFLOOR');
  mobj_flags2_ex.Add('MF2_EX_GREENBLOOD');
  mobj_flags2_ex.Add('MF2_EX_BLUEBLOOD');
  mobj_flags2_ex.Add('MF2_EX_NOTELEPORT');
  mobj_flags2_ex.Add('MF2_EX_PUSHABLE');
  mobj_flags2_ex.Add('MF2_EX_CANNOTPUSH');
  mobj_flags2_ex.Add('MF2_EX_DONTDRAW');
  mobj_flags2_ex.Add('MF2_EX_INTERACTIVE');
  mobj_flags2_ex.Add('MF2_EX_DONTINFIGHTMONSTERS');
  mobj_flags2_ex.Add('MF2_EX_FRIGHTENED');
  mobj_flags2_ex.Add('MF2_EX_JUSTAPPEARED');
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
  mobj_flags2_ex.Add('MF2_EX_JUMPUP');
  mobj_flags2_ex.Add('MF2_EX_DONTBLOCKPLAYER');

  mobj_flags2_ex_hash := TDEHStringsHashTable.Create;
  mobj_flags2_ex_hash.AssignList(mobj_flags2_ex);


  mobj_flags3_ex := TDTextList.Create;
  mobj_flags3_ex.Add('MF3_EX_FLOORBOUNCE');
  mobj_flags3_ex.Add('MF3_EX_CEILINGBOUNCE');
  mobj_flags3_ex.Add('MF3_EX_WALLBOUNCE');
  mobj_flags3_ex.Add('MF3_EX_NOMAXMOVE');
  mobj_flags3_ex.Add('MF3_EX_NOCRASH');
  mobj_flags3_ex.Add('MF3_EX_BLOODIGNOREDAMAGE');
  // JVAL: VERSION 206
  mobj_flags3_ex.Add('MF3_EX_NORENDERINTERPOLATION');
  mobj_flags3_ex.Add('MF3_EX_LINEDONE');
  mobj_flags3_ex.Add('MF3_EX_FLIPSPRITE');
  mobj_flags3_ex.Add('MF3_EX_MISSILEMORE');
  mobj_flags3_ex.Add('MF3_EX_MISSILEEVENMORE');
  mobj_flags3_ex.Add('MF3_EX_DOOMDAMAGE');
  mobj_flags3_ex.Add('MF3_EX_NOTIMEFREEZE');
  mobj_flags3_ex.Add('MF3_EX_NOFEAR');
  mobj_flags3_ex.Add('MF3_EX_CAUSEFEAR');
  mobj_flags3_ex.Add('MF3_EX_SLIDING');
  mobj_flags3_ex.Add('MF3_EX_THRUACTORS');
  mobj_flags3_ex.Add('MF3_EX_THRUSPECIES');
  mobj_flags3_ex.Add('MF3_EX_NOBLOCKMONST');
  mobj_flags3_ex.Add('MF3_EX_NOTAUTOAIMED');
  mobj_flags3_ex.Add('MF3_EX_SLIDE');
  mobj_flags3_ex.Add('MF3_EX_SLIDEONWALLS');
  mobj_flags3_ex.Add('MF3_EX_ABSOLUTEDAMAGE');
  mobj_flags3_ex.Add('MF3_EX_SETGRAVITYONDEATH');
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
  {$IFDEF DLL}deh_actions[0].decl := '';{$ENDIF}
  deh_actions[1].action.acp1 := @A_Light0;
  deh_actions[1].name := strupper('Light0');
  {$IFDEF DLL}deh_actions[1].decl := 'A_Light0()';{$ENDIF}
  deh_actions[2].action.acp1 := @A_WeaponReady;
  deh_actions[2].name := strupper('WeaponReady');
  {$IFDEF DLL}deh_actions[2].decl := 'A_WeaponReady()';{$ENDIF}
  deh_actions[3].action.acp1 := @A_Lower;
  deh_actions[3].name := strupper('Lower');
  {$IFDEF DLL}deh_actions[3].decl := 'A_Lower()';{$ENDIF}
  deh_actions[4].action.acp1 := @A_Raise;
  deh_actions[4].name := strupper('Raise');
  {$IFDEF DLL}deh_actions[4].decl := 'A_Raise()';{$ENDIF}
  deh_actions[5].action.acp1 := @A_Punch;
  deh_actions[5].name := strupper('Punch');
  {$IFDEF DLL}deh_actions[5].decl := 'A_Punch()';{$ENDIF}
  deh_actions[6].action.acp1 := @A_ReFire;
  deh_actions[6].name := strupper('ReFire');
  {$IFDEF DLL}deh_actions[6].decl := 'A_ReFire()';{$ENDIF}
  deh_actions[7].action.acp1 := @A_Light1;
  deh_actions[7].name := strupper('Light1');
  {$IFDEF DLL}deh_actions[7].decl := 'A_Light1()';{$ENDIF}
  deh_actions[8].action.acp1 := @A_Light2;
  deh_actions[8].name := strupper('Light2');
  {$IFDEF DLL}deh_actions[8].decl := 'A_Light2()';{$ENDIF}
  deh_actions[9].action.acp1 := @A_CheckReload;
  deh_actions[9].name := strupper('CheckReload');
  {$IFDEF DLL}deh_actions[9].decl := 'A_CheckReload()';{$ENDIF}
  deh_actions[10].action.acp1 := @A_GunFlash;
  deh_actions[10].name := strupper('GunFlash');
  {$IFDEF DLL}deh_actions[10].decl := 'A_GunFlash()';{$ENDIF}
  deh_actions[11].action.acp1 := @A_FireMissile;
  deh_actions[11].name := strupper('FireMissile');
  {$IFDEF DLL}deh_actions[11].decl := 'A_FireMissile()';{$ENDIF}
  deh_actions[12].action.acp1 := @A_Explode;
  deh_actions[12].name := strupper('Explode');
  {$IFDEF DLL}deh_actions[12].decl := 'A_Explode()';{$ENDIF}
  deh_actions[13].action.acp1 := @A_Pain;
  deh_actions[13].name := strupper('Pain');
  {$IFDEF DLL}deh_actions[13].decl := 'A_Pain()';{$ENDIF}
  deh_actions[14].action.acp1 := @A_PlayerScream;
  deh_actions[14].name := strupper('PlayerScream');
  {$IFDEF DLL}deh_actions[14].decl := 'A_PlayerScream()';{$ENDIF}
  deh_actions[15].action.acp1 := @A_Fall;
  deh_actions[15].name := strupper('Fall');
  {$IFDEF DLL}deh_actions[15].decl := 'A_Fall()';{$ENDIF}
  deh_actions[16].action.acp1 := @A_XScream;
  deh_actions[16].name := strupper('XScream');
  {$IFDEF DLL}deh_actions[16].decl := 'A_XScream()';{$ENDIF}
  deh_actions[17].action.acp1 := @A_Look;
  deh_actions[17].name := strupper('Look');
  {$IFDEF DLL}deh_actions[17].decl := 'A_Look()';{$ENDIF}
  deh_actions[18].action.acp1 := @A_Chase;
  deh_actions[18].name := strupper('Chase');
  {$IFDEF DLL}deh_actions[18].decl := 'A_Chase()';{$ENDIF}
  deh_actions[19].action.acp1 := @A_FaceTarget;
  deh_actions[19].name := strupper('FaceTarget');
  {$IFDEF DLL}deh_actions[19].decl := 'A_FaceTarget()';{$ENDIF}
  deh_actions[20].action.acp1 := @A_Scream;
  deh_actions[20].name := strupper('Scream');
  {$IFDEF DLL}deh_actions[20].decl := 'A_Scream()';{$ENDIF}
  deh_actions[21].action.acp1 := @A_Tracer;
  deh_actions[21].name := strupper('Tracer');
  {$IFDEF DLL}deh_actions[21].decl := 'A_Tracer()';{$ENDIF}
  deh_actions[22].action.acp1 := @A_BossDeath;
  deh_actions[22].name := strupper('BossDeath');
  {$IFDEF DLL}deh_actions[22].decl := 'A_BossDeath()';{$ENDIF}
  deh_actions[23].action.acp1 := @A_Listen;
  deh_actions[23].name := strupper('Listen');
  {$IFDEF DLL}deh_actions[23].decl := 'A_Listen()';{$ENDIF}
  deh_actions[24].action.acp1 := @A_RandomWalk;
  deh_actions[24].name := strupper('RandomWalk');
  {$IFDEF DLL}deh_actions[24].decl := 'A_RandomWalk()';{$ENDIF}
  deh_actions[25].action.acp1 := @A_FriendLook;
  deh_actions[25].name := strupper('FriendLook');
  {$IFDEF DLL}deh_actions[25].decl := 'A_FriendLook()';{$ENDIF}
  deh_actions[26].action.acp1 := @A_Listen;
  deh_actions[26].name := strupper('Listen');
  {$IFDEF DLL}deh_actions[26].decl := 'A_Listen()';{$ENDIF}
  deh_actions[27].action.acp1 := @A_PeasantPunch;
  deh_actions[27].name := strupper('PeasantPunch');
  {$IFDEF DLL}deh_actions[27].decl := 'A_PeasantPunch()';{$ENDIF}
  deh_actions[28].action.acp1 := @A_ReaverAttack;
  deh_actions[28].name := strupper('ReaverAttack');
  {$IFDEF DLL}deh_actions[28].decl := 'A_ReaverAttack()';{$ENDIF}
  deh_actions[29].action.acp1 := @A_CheckTargetVisible;
  deh_actions[29].name := strupper('CheckTargetVisible');
  {$IFDEF DLL}deh_actions[29].decl := 'A_CheckTargetVisible()';{$ENDIF}
  deh_actions[30].action.acp1 := @A_SentinelAttack;
  deh_actions[30].name := strupper('SentinelAttack');
  {$IFDEF DLL}deh_actions[30].decl := 'A_SentinelAttack()';{$ENDIF}
  deh_actions[31].action.acp1 := @A_StalkerThink;
  deh_actions[31].name := strupper('StalkerThink');
  {$IFDEF DLL}deh_actions[31].decl := 'A_StalkerThink()';{$ENDIF}
  deh_actions[32].action.acp1 := @A_StalkerSetLook;
  deh_actions[32].name := strupper('StalkerSetLook');
  {$IFDEF DLL}deh_actions[32].decl := 'A_StalkerSetLook()';{$ENDIF}
  deh_actions[33].action.acp1 := @A_StalkerDrop;
  deh_actions[33].name := strupper('StalkerDrop');
  {$IFDEF DLL}deh_actions[33].decl := 'A_StalkerDrop()';{$ENDIF}
  deh_actions[34].action.acp1 := @A_StalkerScratch;
  deh_actions[34].name := strupper('StalkerScratch');
  {$IFDEF DLL}deh_actions[34].decl := 'A_StalkerScratch()';{$ENDIF}
  deh_actions[35].action.acp1 := @A_FloatWeave;
  deh_actions[35].name := strupper('FloatWeave');
  {$IFDEF DLL}deh_actions[35].decl := 'A_FloatWeave()';{$ENDIF}
  deh_actions[36].action.acp1 := @A_RobotMelee;
  deh_actions[36].name := strupper('RobotMelee');
  {$IFDEF DLL}deh_actions[36].decl := 'A_RobotMelee()';{$ENDIF}
  deh_actions[37].action.acp1 := @A_TemplarMauler;
  deh_actions[37].name := strupper('TemplarMauler');
  {$IFDEF DLL}deh_actions[37].decl := 'A_TemplarMauler()';{$ENDIF}
  deh_actions[38].action.acp1 := @A_CrusaderAttack;
  deh_actions[38].name := strupper('CrusaderAttack');
  {$IFDEF DLL}deh_actions[38].decl := 'A_CrusaderAttack()';{$ENDIF}
  deh_actions[39].action.acp1 := @A_CrusaderLeft;
  deh_actions[39].name := strupper('CrusaderLeft');
  {$IFDEF DLL}deh_actions[39].decl := 'A_CrusaderLeft()';{$ENDIF}
  deh_actions[40].action.acp1 := @A_CrusaderRight;
  deh_actions[40].name := strupper('CrusaderRight');
  {$IFDEF DLL}deh_actions[40].decl := 'A_CrusaderRight()';{$ENDIF}
  deh_actions[41].action.acp1 := @A_CheckTargetVisible2;
  deh_actions[41].name := strupper('CheckTargetVisible2');
  {$IFDEF DLL}deh_actions[41].decl := 'A_CheckTargetVisible2()';{$ENDIF}
  deh_actions[42].action.acp1 := @A_InqFlyCheck;
  deh_actions[42].name := strupper('InqFlyCheck');
  {$IFDEF DLL}deh_actions[42].decl := 'A_InqFlyCheck()';{$ENDIF}
  deh_actions[43].action.acp1 := @A_InqGrenade;
  deh_actions[43].name := strupper('InqGrenade');
  {$IFDEF DLL}deh_actions[43].decl := 'A_InqGrenade()';{$ENDIF}
  deh_actions[44].action.acp1 := @A_InqTakeOff;
  deh_actions[44].name := strupper('InqTakeOff');
  {$IFDEF DLL}deh_actions[44].decl := 'A_InqTakeOff()';{$ENDIF}
  deh_actions[45].action.acp1 := @A_InqFly;
  deh_actions[45].name := strupper('InqFly');
  {$IFDEF DLL}deh_actions[45].decl := 'A_InqFly()';{$ENDIF}
  deh_actions[46].action.acp1 := @A_FireSigilWeapon;
  deh_actions[46].name := strupper('FireSigilWeapon');
  {$IFDEF DLL}deh_actions[46].decl := 'A_FireSigilWeapon()';{$ENDIF}
  deh_actions[47].action.acp1 := @A_ProgrammerAttack;
  deh_actions[47].name := strupper('ProgrammerAttack');
  {$IFDEF DLL}deh_actions[47].decl := 'A_ProgrammerAttack()';{$ENDIF}
  deh_actions[48].action.acp1 := @A_Sigil_A_Action;
  deh_actions[48].name := strupper('Sigil_A_Action');
  {$IFDEF DLL}deh_actions[48].decl := 'A_Sigil_A_Action()';{$ENDIF}
  deh_actions[49].action.acp1 := @A_SpectreEAttack;
  deh_actions[49].name := strupper('SpectreEAttack');
  {$IFDEF DLL}deh_actions[49].decl := 'A_SpectreEAttack()';{$ENDIF}
  deh_actions[50].action.acp1 := @A_SpectreCAttack;
  deh_actions[50].name := strupper('SpectreCAttack');
  {$IFDEF DLL}deh_actions[50].decl := 'A_SpectreCAttack()';{$ENDIF}
  deh_actions[51].action.acp1 := @A_AlertSpectreC;
  deh_actions[51].name := strupper('AlertSpectreC');
  {$IFDEF DLL}deh_actions[51].decl := 'A_AlertSpectreC()';{$ENDIF}
  deh_actions[52].action.acp1 := @A_Sigil_E_Action;
  deh_actions[52].name := strupper('Sigil_E_Action');
  {$IFDEF DLL}deh_actions[52].decl := 'A_Sigil_E_Action()';{$ENDIF}
  deh_actions[53].action.acp1 := @A_SigilTrail;
  deh_actions[53].name := strupper('SigilTrail');
  {$IFDEF DLL}deh_actions[53].decl := 'A_SigilTrail()';{$ENDIF}
  deh_actions[54].action.acp1 := @A_SpectreDAttack;
  deh_actions[54].name := strupper('SpectreDAttack');
  {$IFDEF DLL}deh_actions[54].decl := 'A_SpectreDAttack()';{$ENDIF}
  deh_actions[55].action.acp1 := @A_FireSigilEOffshoot;
  deh_actions[55].name := strupper('FireSigilEOffshoot');
  {$IFDEF DLL}deh_actions[55].decl := 'A_FireSigilEOffshoot()';{$ENDIF}
  deh_actions[56].action.acp1 := @A_ShadowOff;
  deh_actions[56].name := strupper('ShadowOff');
  {$IFDEF DLL}deh_actions[56].decl := 'A_ShadowOff()';{$ENDIF}
  deh_actions[57].action.acp1 := @A_ModifyVisibility;
  deh_actions[57].name := strupper('ModifyVisibility');
  {$IFDEF DLL}deh_actions[57].decl := 'A_ModifyVisibility()';{$ENDIF}
  deh_actions[58].action.acp1 := @A_ShadowOn;
  deh_actions[58].name := strupper('ShadowOn');
  {$IFDEF DLL}deh_actions[58].decl := 'A_ShadowOn()';{$ENDIF}
  deh_actions[59].action.acp1 := @A_SetTLOptions;
  deh_actions[59].name := strupper('SetTLOptions');
  {$IFDEF DLL}deh_actions[59].decl := 'A_SetTLOptions()';{$ENDIF}
  deh_actions[60].action.acp1 := @A_BossMeleeAtk;
  deh_actions[60].name := strupper('BossMeleeAtk');
  {$IFDEF DLL}deh_actions[60].decl := 'A_BossMeleeAtk()';{$ENDIF}
  deh_actions[61].action.acp1 := @A_BishopAttack;
  deh_actions[61].name := strupper('BishopAttack');
  {$IFDEF DLL}deh_actions[61].decl := 'A_BishopAttack()';{$ENDIF}
  deh_actions[62].action.acp1 := @A_FireHookShot;
  deh_actions[62].name := strupper('FireHookShot');
  {$IFDEF DLL}deh_actions[62].decl := 'A_FireHookShot()';{$ENDIF}
  deh_actions[63].action.acp1 := @A_FireChainShot;
  deh_actions[63].name := strupper('FireChainShot');
  {$IFDEF DLL}deh_actions[63].decl := 'A_FireChainShot()';{$ENDIF}
  deh_actions[64].action.acp1 := @A_MissileSmoke;
  deh_actions[64].name := strupper('MissileSmoke');
  {$IFDEF DLL}deh_actions[64].decl := 'A_MissileSmoke()';{$ENDIF}
  deh_actions[65].action.acp1 := @A_SpawnSparkPuff;
  deh_actions[65].name := strupper('SpawnSparkPuff');
  {$IFDEF DLL}deh_actions[65].decl := 'A_SpawnSparkPuff()';{$ENDIF}
  deh_actions[66].action.acp1 := @A_ProgrammerMelee;
  deh_actions[66].name := strupper('ProgrammerMelee');
  {$IFDEF DLL}deh_actions[66].decl := 'A_ProgrammerMelee()';{$ENDIF}
  deh_actions[67].action.acp1 := @A_PeasantCrash;
  deh_actions[67].name := strupper('PeasantCrash');
  {$IFDEF DLL}deh_actions[67].decl := 'A_PeasantCrash()';{$ENDIF}
  deh_actions[68].action.acp1 := @A_HideZombie;
  deh_actions[68].name := strupper('HideZombie');
  {$IFDEF DLL}deh_actions[68].decl := 'A_HideZombie()';{$ENDIF}
  deh_actions[69].action.acp1 := @A_MerchantPain;
  deh_actions[69].name := strupper('MerchantPain');
  {$IFDEF DLL}deh_actions[69].decl := 'A_MerchantPain()';{$ENDIF}
  deh_actions[70].action.acp1 := @A_ProgrammerDie;
  deh_actions[70].name := strupper('ProgrammerDie');
  {$IFDEF DLL}deh_actions[70].decl := 'A_ProgrammerDie()';{$ENDIF}
  deh_actions[71].action.acp1 := @A_InqTossArm;
  deh_actions[71].name := strupper('InqTossArm');
  {$IFDEF DLL}deh_actions[71].decl := 'A_InqTossArm()';{$ENDIF}
  deh_actions[72].action.acp1 := @A_SpawnSpectreA;
  deh_actions[72].name := strupper('SpawnSpectreA');
  {$IFDEF DLL}deh_actions[72].decl := 'A_SpawnSpectreA()';{$ENDIF}
  deh_actions[73].action.acp1 := @A_SpawnSpectreB;
  deh_actions[73].name := strupper('SpawnSpectreB');
  {$IFDEF DLL}deh_actions[73].decl := 'A_SpawnSpectreB()';{$ENDIF}
  deh_actions[74].action.acp1 := @A_SpawnSpectreC;
  deh_actions[74].name := strupper('SpawnSpectreC');
  {$IFDEF DLL}deh_actions[74].decl := 'A_SpawnSpectreC()';{$ENDIF}
  deh_actions[75].action.acp1 := @A_SpawnSpectreD;
  deh_actions[75].name := strupper('SpawnSpectreD');
  {$IFDEF DLL}deh_actions[75].decl := 'A_SpawnSpectreD()';{$ENDIF}
  deh_actions[76].action.acp1 := @A_SpawnSpectreE;
  deh_actions[76].name := strupper('SpawnSpectreE');
  {$IFDEF DLL}deh_actions[76].decl := 'A_SpawnSpectreE()';{$ENDIF}
  deh_actions[77].action.acp1 := @A_SpawnEntity;
  deh_actions[77].name := strupper('SpawnEntity');
  {$IFDEF DLL}deh_actions[77].decl := 'A_SpawnEntity()';{$ENDIF}
  deh_actions[78].action.acp1 := @A_EntityDeath;
  deh_actions[78].name := strupper('EntityDeath');
  {$IFDEF DLL}deh_actions[78].decl := 'A_EntityDeath()';{$ENDIF}
  deh_actions[79].action.acp1 := @A_SpawnZombie;
  deh_actions[79].name := strupper('SpawnZombie');
  {$IFDEF DLL}deh_actions[79].decl := 'A_SpawnZombie()';{$ENDIF}
  deh_actions[80].action.acp1 := @A_ZombieInSpecialSector;
  deh_actions[80].name := strupper('ZombieInSpecialSector');
  {$IFDEF DLL}deh_actions[80].decl := 'A_ZombieInSpecialSector()';{$ENDIF}
  deh_actions[81].action.acp1 := @A_CrystalExplode;
  deh_actions[81].name := strupper('CrystalExplode');
  {$IFDEF DLL}deh_actions[81].decl := 'A_CrystalExplode()';{$ENDIF}
  deh_actions[82].action.acp1 := @A_QuestMsg;
  deh_actions[82].name := strupper('QuestMsg');
  {$IFDEF DLL}deh_actions[82].decl := 'A_QuestMsg()';{$ENDIF}
  deh_actions[83].action.acp1 := @A_ExtraLightOff;
  deh_actions[83].name := strupper('ExtraLightOff');
  {$IFDEF DLL}deh_actions[83].decl := 'A_ExtraLightOff()';{$ENDIF}
  deh_actions[84].action.acp1 := @A_CrystalRadiusAtk;
  deh_actions[84].name := strupper('CrystalRadiusAtk');
  {$IFDEF DLL}deh_actions[84].decl := 'A_CrystalRadiusAtk()';{$ENDIF}
  deh_actions[85].action.acp1 := @A_DeathExplode1;
  deh_actions[85].name := strupper('DeathExplode1');
  {$IFDEF DLL}deh_actions[85].decl := 'A_DeathExplode1()';{$ENDIF}
  deh_actions[86].action.acp1 := @A_DeathExplode2;
  deh_actions[86].name := strupper('DeathExplode2');
  {$IFDEF DLL}deh_actions[86].decl := 'A_DeathExplode2()';{$ENDIF}
  deh_actions[87].action.acp1 := @A_DeathExplode3;
  deh_actions[87].name := strupper('DeathExplode3');
  {$IFDEF DLL}deh_actions[87].decl := 'A_DeathExplode3()';{$ENDIF}
  deh_actions[88].action.acp1 := @A_DeathExplode5;
  deh_actions[88].name := strupper('DeathExplode5');
  {$IFDEF DLL}deh_actions[88].decl := 'A_DeathExplode5()';{$ENDIF}
  deh_actions[89].action.acp1 := @A_RaiseAlarm;
  deh_actions[89].name := strupper('RaiseAlarm');
  {$IFDEF DLL}deh_actions[89].decl := 'A_RaiseAlarm()';{$ENDIF}
  deh_actions[90].action.acp1 := @A_MissileTick;
  deh_actions[90].name := strupper('MissileTick');
  {$IFDEF DLL}deh_actions[90].decl := 'A_MissileTick()';{$ENDIF}
  deh_actions[91].action.acp1 := @A_SpawnGrenadeFire;
  deh_actions[91].name := strupper('SpawnGrenadeFire');
  {$IFDEF DLL}deh_actions[91].decl := 'A_SpawnGrenadeFire()';{$ENDIF}
  deh_actions[92].action.acp1 := @A_NodeChunk;
  deh_actions[92].name := strupper('NodeChunk');
  {$IFDEF DLL}deh_actions[92].decl := 'A_NodeChunk()';{$ENDIF}
  deh_actions[93].action.acp1 := @A_HeadChunk;
  deh_actions[93].name := strupper('HeadChunk');
  {$IFDEF DLL}deh_actions[93].decl := 'A_HeadChunk()';{$ENDIF}
  deh_actions[94].action.acp1 := @A_BurnSpread;
  deh_actions[94].name := strupper('BurnSpread');
  {$IFDEF DLL}deh_actions[94].decl := 'A_BurnSpread()';{$ENDIF}
  deh_actions[95].action.acp1 := @A_AcolyteSpecial;
  deh_actions[95].name := strupper('AcolyteSpecial');
  {$IFDEF DLL}deh_actions[95].decl := 'A_AcolyteSpecial()';{$ENDIF}
  deh_actions[96].action.acp1 := @A_InqChase;
  deh_actions[96].name := strupper('InqChase');
  {$IFDEF DLL}deh_actions[96].decl := 'A_InqChase()';{$ENDIF}
  deh_actions[97].action.acp1 := @A_StalkerChase;
  deh_actions[97].name := strupper('StalkerChase');
  {$IFDEF DLL}deh_actions[97].decl := 'A_StalkerChase()';{$ENDIF}
  deh_actions[98].action.acp1 := @A_TeleportBeacon;
  deh_actions[98].name := strupper('TeleportBeacon');
  {$IFDEF DLL}deh_actions[98].decl := 'A_TeleportBeacon()';{$ENDIF}
  deh_actions[99].action.acp1 := @A_BodyParts;
  deh_actions[99].name := strupper('BodyParts');
  {$IFDEF DLL}deh_actions[99].decl := 'A_BodyParts()';{$ENDIF}
  deh_actions[100].action.acp1 := @A_ClaxonBlare;
  deh_actions[100].name := strupper('ClaxonBlare');
  {$IFDEF DLL}deh_actions[100].decl := 'A_ClaxonBlare()';{$ENDIF}
  deh_actions[101].action.acp1 := @A_ActiveSoundSTRF;
  deh_actions[101].name := strupper('ActiveSoundSTRF');
  {$IFDEF DLL}deh_actions[101].decl := 'A_ActiveSoundSTRF()';{$ENDIF}
  deh_actions[102].action.acp1 := @A_ClearSoundTarget;
  deh_actions[102].name := strupper('ClearSoundTarget');
  {$IFDEF DLL}deh_actions[102].decl := 'A_ClearSoundTarget()';{$ENDIF}
  deh_actions[103].action.acp1 := @A_DropBurnFlesh;
  deh_actions[103].name := strupper('DropBurnFlesh');
  {$IFDEF DLL}deh_actions[103].decl := 'A_DropBurnFlesh()';{$ENDIF}
  deh_actions[104].action.acp1 := @A_FlameDeath;
  deh_actions[104].name := strupper('FlameDeath');
  {$IFDEF DLL}deh_actions[104].decl := 'A_FlameDeath()';{$ENDIF}
  deh_actions[105].action.acp1 := @A_ClearForceField;
  deh_actions[105].name := strupper('ClearForceField');
  {$IFDEF DLL}deh_actions[105].decl := 'A_ClearForceField()';{$ENDIF}
  deh_actions[106].action.acp1 := @A_FireFlameThrower;
  deh_actions[106].name := strupper('FireFlameThrower');
  {$IFDEF DLL}deh_actions[106].decl := 'A_FireFlameThrower()';{$ENDIF}
  deh_actions[107].action.acp1 := @A_FireMauler2;
  deh_actions[107].name := strupper('FireMauler2');
  {$IFDEF DLL}deh_actions[107].decl := 'A_FireMauler2()';{$ENDIF}
  deh_actions[108].action.acp1 := @A_FireGrenade;
  deh_actions[108].name := strupper('FireGrenade');
  {$IFDEF DLL}deh_actions[108].decl := 'A_FireGrenade()';{$ENDIF}
  deh_actions[109].action.acp1 := @A_FireElectricBolt;
  deh_actions[109].name := strupper('FireElectricBolt');
  {$IFDEF DLL}deh_actions[109].decl := 'A_FireElectricBolt()';{$ENDIF}
  deh_actions[110].action.acp1 := @A_FirePoisonBolt;
  deh_actions[110].name := strupper('FirePoisonBolt');
  {$IFDEF DLL}deh_actions[110].decl := 'A_FirePoisonBolt()';{$ENDIF}
  deh_actions[111].action.acp1 := @A_FireRifle;
  deh_actions[111].name := strupper('FireRifle');
  {$IFDEF DLL}deh_actions[111].decl := 'A_FireRifle()';{$ENDIF}
  deh_actions[112].action.acp1 := @A_FireMauler1;
  deh_actions[112].name := strupper('FireMauler1');
  {$IFDEF DLL}deh_actions[112].decl := 'A_FireMauler1()';{$ENDIF}
  deh_actions[113].action.acp1 := @A_SigilSound;
  deh_actions[113].name := strupper('SigilSound');
  {$IFDEF DLL}deh_actions[113].decl := 'A_SigilSound()';{$ENDIF}
  deh_actions[114].action.acp1 := @A_FireSigil;
  deh_actions[114].name := strupper('FireSigil');
  {$IFDEF DLL}deh_actions[114].decl := 'A_FireSigil()';{$ENDIF}
  deh_actions[115].action.acp1 := @A_GunFlashThinker;
  deh_actions[115].name := strupper('GunFlashThinker');
  {$IFDEF DLL}deh_actions[115].decl := 'A_GunFlashThinker()';{$ENDIF}
  deh_actions[116].action.acp1 := @A_SigilShock;
  deh_actions[116].name := strupper('SigilShock');
  {$IFDEF DLL}deh_actions[116].decl := 'A_SigilShock()';{$ENDIF}
  deh_actions[117].action.acp1 := @A_TorpedoExplode;
  deh_actions[117].name := strupper('TorpedoExplode');
  {$IFDEF DLL}deh_actions[117].decl := 'A_TorpedoExplode()';{$ENDIF}
  deh_actions[118].action.acp1 := @A_MaulerSound;
  deh_actions[118].name := strupper('MaulerSound');
  {$IFDEF DLL}deh_actions[118].decl := 'A_MaulerSound()';{$ENDIF}
  deh_actions[119].action.acp1 := @A_CustomSound1;
  deh_actions[119].name := strupper('CustomSound1');
  {$IFDEF DLL}deh_actions[119].decl := 'A_CustomSound1()';{$ENDIF}
  deh_actions[120].action.acp1 := @A_CustomSound2;
  deh_actions[120].name := strupper('CustomSound2');
  {$IFDEF DLL}deh_actions[120].decl := 'A_CustomSound2()';{$ENDIF}
  deh_actions[121].action.acp1 := @A_CustomSound3;
  deh_actions[121].name := strupper('CustomSound3');
  {$IFDEF DLL}deh_actions[121].decl := 'A_CustomSound3()';{$ENDIF}
  deh_actions[122].action.acp1 := @A_RandomPainSound;
  deh_actions[122].name := strupper('RandomPainSound');
  {$IFDEF DLL}deh_actions[122].decl := 'A_RandomPainSound()';{$ENDIF}
  deh_actions[123].action.acp1 := @A_RandomSeeSound;
  deh_actions[123].name := strupper('RandomSeeSound');
  {$IFDEF DLL}deh_actions[123].decl := 'A_RandomSeeSound()';{$ENDIF}
  deh_actions[124].action.acp1 := @A_RandomAttackSound;
  deh_actions[124].name := strupper('RandomAttackSound');
  {$IFDEF DLL}deh_actions[124].decl := 'A_RandomAttackSound()';{$ENDIF}
  deh_actions[125].action.acp1 := @A_RandomDeathSound;
  deh_actions[125].name := strupper('RandomDeathSound');
  {$IFDEF DLL}deh_actions[125].decl := 'A_RandomDeathSound()';{$ENDIF}
  deh_actions[126].action.acp1 := @A_RandomActiveSound;
  deh_actions[126].name := strupper('RandomActiveSound');
  {$IFDEF DLL}deh_actions[126].decl := 'A_RandomActiveSound()';{$ENDIF}
  deh_actions[127].action.acp1 := @A_RandomCustomSound1;
  deh_actions[127].name := strupper('RandomCustomSound1');
  {$IFDEF DLL}deh_actions[127].decl := 'A_RandomCustomSound1()';{$ENDIF}
  deh_actions[128].action.acp1 := @A_RandomCustomSound2;
  deh_actions[128].name := strupper('RandomCustomSound2');
  {$IFDEF DLL}deh_actions[128].decl := 'A_RandomCustomSound2()';{$ENDIF}
  deh_actions[129].action.acp1 := @A_RandomCustomSound3;
  deh_actions[129].name := strupper('RandomCustomSound3');
  {$IFDEF DLL}deh_actions[129].decl := 'A_RandomCustomSound3()';{$ENDIF}
  deh_actions[130].action.acp1 := @A_RandomCustomSound;
  deh_actions[130].name := strupper('RandomCustomSound');
  {$IFDEF DLL}deh_actions[130].decl := 'A_RandomCustomSound()';{$ENDIF}
  deh_actions[131].action.acp1 := @A_Playsound;
  deh_actions[131].name := strupper('Playsound');
  {$IFDEF DLL}deh_actions[131].decl := 'A_Playsound(sound: string)';{$ENDIF}
  deh_actions[132].action.acp1 := @A_RandomSound;
  deh_actions[132].name := strupper('RandomSound');
  {$IFDEF DLL}deh_actions[132].decl := 'A_RandomSound(sound1: string, [sound2: string], ...)';{$ENDIF}
  deh_actions[133].action.acp1 := @A_Stop;
  deh_actions[133].name := strupper('Stop');
  {$IFDEF DLL}deh_actions[133].decl := 'A_Stop()';{$ENDIF}
  deh_actions[134].action.acp1 := @A_Jump;
  deh_actions[134].name := strupper('Jump');
  {$IFDEF DLL}deh_actions[134].decl := 'A_Jump(propability: random_t, offset1: integer, [offset2: integer], ...)';{$ENDIF}
  deh_actions[135].action.acp1 := @A_CustomMissile;
  deh_actions[135].name := strupper('CustomMissile');
  {$IFDEF DLL}deh_actions[135].decl := 'A_CustomMissile(missiletype: string, [height: integer], [offset: integer], [angle: integer], [aimmode: integer], [pitch: integer])';{$ENDIF}
  deh_actions[136].action.acp1 := @A_NoGravity;
  deh_actions[136].name := strupper('NoGravity');
  {$IFDEF DLL}deh_actions[136].decl := 'A_NoGravity()';{$ENDIF}
  deh_actions[137].action.acp1 := @A_Gravity;
  deh_actions[137].name := strupper('Gravity');
  {$IFDEF DLL}deh_actions[137].decl := 'A_Gravity()';{$ENDIF}
  deh_actions[138].action.acp1 := @A_NoBlocking;
  deh_actions[138].name := strupper('NoBlocking');
  {$IFDEF DLL}deh_actions[138].decl := 'A_NoBlocking()';{$ENDIF}
  deh_actions[139].action.acp1 := @A_MeleeAttack;
  deh_actions[139].name := strupper('MeleeAttack');
  {$IFDEF DLL}deh_actions[139].decl := 'A_MeleeAttack([mindamage: integer], [maxdamage: integer])';{$ENDIF}
  deh_actions[140].action.acp1 := @A_SpawnItem;
  deh_actions[140].name := strupper('SpawnItem');
  {$IFDEF DLL}deh_actions[140].decl := 'A_SpawnItem(type: string, [distance: float], [zheight: float], [angle: angle])';{$ENDIF}
  deh_actions[141].action.acp1 := @A_SeekerMissile;
  deh_actions[141].name := strupper('SeekerMissile');
  {$IFDEF DLL}deh_actions[141].decl := 'A_SeekerMissile(threshold_angle: angle, [turnMax_angle: angle])';{$ENDIF}
  deh_actions[142].action.acp1 := @A_CStaffMissileSlither;
  deh_actions[142].name := strupper('CStaffMissileSlither');
  {$IFDEF DLL}deh_actions[142].decl := 'A_CStaffMissileSlither()';{$ENDIF}
  deh_actions[143].action.acp1 := @A_SetTranslucent;
  deh_actions[143].name := strupper('SetTranslucent');
  {$IFDEF DLL}deh_actions[143].decl := 'A_SetTranslucent(alpha: float, [style: integer])';{$ENDIF}
  deh_actions[144].action.acp1 := @A_Die;
  deh_actions[144].name := strupper('Die');
  {$IFDEF DLL}deh_actions[144].decl := 'A_Die()';{$ENDIF}
  deh_actions[145].action.acp1 := @A_CustomBulletAttack;
  deh_actions[145].name := strupper('CustomBulletAttack');
  {$IFDEF DLL}deh_actions[145].decl := 'A_CustomBulletAttack(spread_xy: angle, numbullets: integer, damageperbullet: integer, [range: integer])';{$ENDIF}
  deh_actions[146].action.acp1 := @A_FadeOut;
  deh_actions[146].name := strupper('FadeOut');
  {$IFDEF DLL}deh_actions[146].decl := 'A_FadeOut(fade: float)';{$ENDIF}
  deh_actions[147].action.acp1 := @A_FadeIn;
  deh_actions[147].name := strupper('FadeIn');
  {$IFDEF DLL}deh_actions[147].decl := 'A_FadeIn(fade: float)';{$ENDIF}
  deh_actions[148].action.acp1 := @A_MissileAttack;
  deh_actions[148].name := strupper('MissileAttack');
  {$IFDEF DLL}deh_actions[148].decl := 'A_MissileAttack([missiletype: string])';{$ENDIF}
  deh_actions[149].action.acp1 := @A_AdjustSideSpot;
  deh_actions[149].name := strupper('AdjustSideSpot');
  {$IFDEF DLL}deh_actions[149].decl := 'A_AdjustSideSpot(sideoffset: float)';{$ENDIF}
  deh_actions[150].action.acp1 := @A_Countdown;
  deh_actions[150].name := strupper('Countdown');
  {$IFDEF DLL}deh_actions[150].decl := 'A_Countdown()';{$ENDIF}
  deh_actions[151].action.acp1 := @A_FastChase;
  deh_actions[151].name := strupper('FastChase');
  {$IFDEF DLL}deh_actions[151].decl := 'A_FastChase()';{$ENDIF}
  deh_actions[152].action.acp1 := @A_LowGravity;
  deh_actions[152].name := strupper('LowGravity');
  {$IFDEF DLL}deh_actions[152].decl := 'A_LowGravity()';{$ENDIF}
  deh_actions[153].action.acp1 := @A_ThrustZ;
  deh_actions[153].name := strupper('ThrustZ');
  {$IFDEF DLL}deh_actions[153].decl := 'A_ThrustZ(momz: float)';{$ENDIF}
  deh_actions[154].action.acp1 := @A_ThrustXY;
  deh_actions[154].name := strupper('ThrustXY');
  {$IFDEF DLL}deh_actions[154].decl := 'A_ThrustXY(mom: float, ang: angle)';{$ENDIF}
  deh_actions[155].action.acp1 := @A_Turn;
  deh_actions[155].name := strupper('Turn');
  {$IFDEF DLL}deh_actions[155].decl := 'A_Turn(value: angle)';{$ENDIF}
  deh_actions[156].action.acp1 := @A_JumpIfCloser;
  deh_actions[156].name := strupper('JumpIfCloser');
  {$IFDEF DLL}deh_actions[156].decl := 'A_JumpIfCloser(distancetotarget: float, offset: integer)';{$ENDIF}
  deh_actions[157].action.acp1 := @A_JumpIfHealthLower;
  deh_actions[157].name := strupper('JumpIfHealthLower');
  {$IFDEF DLL}deh_actions[157].decl := 'A_JumpIfHealthLower(health: integer, offset: integer)';{$ENDIF}
  deh_actions[158].action.acp1 := @A_ScreamAndUnblock;
  deh_actions[158].name := strupper('ScreamAndUnblock');
  {$IFDEF DLL}deh_actions[158].decl := 'A_ScreamAndUnblock()';{$ENDIF}
  deh_actions[159].action.acp1 := @A_PlayWeaponsound;
  deh_actions[159].name := strupper('PlayWeaponsound');
  {$IFDEF DLL}deh_actions[159].decl := 'A_PlayWeaponsound(sound: string)';{$ENDIF}
  deh_actions[160].action.acp1 := @A_SetInvulnerable;
  deh_actions[160].name := strupper('SetInvulnerable');
  {$IFDEF DLL}deh_actions[160].decl := 'A_SetInvulnerable()';{$ENDIF}
  deh_actions[161].action.acp1 := @A_UnSetInvulnerable;
  deh_actions[161].name := strupper('UnSetInvulnerable');
  {$IFDEF DLL}deh_actions[161].decl := 'A_UnSetInvulnerable()';{$ENDIF}
  deh_actions[162].action.acp1 := @A_RandomMeleeSound;
  deh_actions[162].name := strupper('RandomMeleeSound');
  {$IFDEF DLL}deh_actions[162].decl := 'A_RandomMeleeSound()';{$ENDIF}
  deh_actions[163].action.acp1 := @A_FloatBob;
  deh_actions[163].name := strupper('FloatBob');
  {$IFDEF DLL}deh_actions[163].decl := 'A_FloatBob()';{$ENDIF}
  deh_actions[164].action.acp1 := @A_NoFloatBob;
  deh_actions[164].name := strupper('NoFloatBob');
  {$IFDEF DLL}deh_actions[164].decl := 'A_NoFloatBob()';{$ENDIF}
  deh_actions[165].action.acp1 := @A_Missile;
  deh_actions[165].name := strupper('Missile');
  {$IFDEF DLL}deh_actions[165].decl := 'A_Missile()';{$ENDIF}
  deh_actions[166].action.acp1 := @A_NoMissile;
  deh_actions[166].name := strupper('NoMissile');
  {$IFDEF DLL}deh_actions[166].decl := 'A_NoMissile()';{$ENDIF}
  deh_actions[167].action.acp1 := @A_ComboAttack;
  deh_actions[167].name := strupper('ComboAttack');
  {$IFDEF DLL}deh_actions[167].decl := 'A_ComboAttack()';{$ENDIF}
  deh_actions[168].action.acp1 := @A_BulletAttack;
  deh_actions[168].name := strupper('BulletAttack');
  {$IFDEF DLL}deh_actions[168].decl := 'A_BulletAttack([numbullets: integer])';{$ENDIF}
  deh_actions[169].action.acp1 := @A_MediumGravity;
  deh_actions[169].name := strupper('MediumGravity');
  {$IFDEF DLL}deh_actions[169].decl := 'A_MediumGravity()';{$ENDIF}
  deh_actions[170].action.acp1 := @A_Wander;
  deh_actions[170].name := strupper('Wander');
  {$IFDEF DLL}deh_actions[170].decl := 'A_Wander()';{$ENDIF}
  deh_actions[171].action.acp1 := @A_FadeOut10;
  deh_actions[171].name := strupper('FadeOut10');
  {$IFDEF DLL}deh_actions[171].decl := 'A_FadeOut10()';{$ENDIF}
  deh_actions[172].action.acp1 := @A_FadeOut20;
  deh_actions[172].name := strupper('FadeOut20');
  {$IFDEF DLL}deh_actions[172].decl := 'A_FadeOut20()';{$ENDIF}
  deh_actions[173].action.acp1 := @A_FadeOut30;
  deh_actions[173].name := strupper('FadeOut30');
  {$IFDEF DLL}deh_actions[173].decl := 'A_FadeOut30()';{$ENDIF}
  deh_actions[174].action.acp1 := @A_FadeIn10;
  deh_actions[174].name := strupper('FadeIn10');
  {$IFDEF DLL}deh_actions[174].decl := 'A_FadeIn10()';{$ENDIF}
  deh_actions[175].action.acp1 := @A_FadeIn20;
  deh_actions[175].name := strupper('FadeIn20');
  {$IFDEF DLL}deh_actions[175].decl := 'A_FadeIn20()';{$ENDIF}
  deh_actions[176].action.acp1 := @A_FadeIn30;
  deh_actions[176].name := strupper('FadeIn30');
  {$IFDEF DLL}deh_actions[176].decl := 'A_FadeIn30()';{$ENDIF}
  deh_actions[177].action.acp1 := @A_SpawnItemEx;
  deh_actions[177].name := strupper('SpawnItemEx');
  {$IFDEF DLL}deh_actions[177].decl := 'A_SpawnItemEx(itemtype: string, [xofs: float], [yofs: float], [zofs: float], [momx: float], [momy: float], [momz: float], [ang: angle], [flags: integer], [chance: integer])';{$ENDIF}
  deh_actions[178].action.acp1 := @A_RandomMissile;
  deh_actions[178].name := strupper('RandomMissile');
  {$IFDEF DLL}deh_actions[178].decl := 'A_RandomMissile(missile1: string, [missile2: string], ...)';{$ENDIF}
  deh_actions[179].action.acp1 := @A_HideThing;
  deh_actions[179].name := strupper('HideThing');
  {$IFDEF DLL}deh_actions[179].decl := 'A_HideThing()';{$ENDIF}
  deh_actions[180].action.acp1 := @A_UnHideThing;
  deh_actions[180].name := strupper('UnHideThing');
  {$IFDEF DLL}deh_actions[180].decl := 'A_UnHideThing()';{$ENDIF}
  deh_actions[181].action.acp1 := @A_SpawnDebris;
  deh_actions[181].name := strupper('SpawnDebris');
  {$IFDEF DLL}deh_actions[181].decl := 'A_SpawnDebris(debristype: string, [count: integer = 1], [horz_mom: integer], [vert_mom: integer])';{$ENDIF}
  deh_actions[182].action.acp1 := @A_Turn5;
  deh_actions[182].name := strupper('Turn5');
  {$IFDEF DLL}deh_actions[182].decl := 'A_Turn5()';{$ENDIF}
  deh_actions[183].action.acp1 := @A_Turn10;
  deh_actions[183].name := strupper('Turn10');
  {$IFDEF DLL}deh_actions[183].decl := 'A_Turn10()';{$ENDIF}
  deh_actions[184].action.acp1 := @A_SpawnSmokeUp;
  deh_actions[184].name := strupper('SpawnSmokeUp');
  {$IFDEF DLL}deh_actions[184].decl := 'A_SpawnSmokeUp()';{$ENDIF}
  deh_actions[185].action.acp1 := @A_SpawnSmokeDown;
  deh_actions[185].name := strupper('SpawnSmokeDown');
  {$IFDEF DLL}deh_actions[185].decl := 'A_SpawnSmokeDown()';{$ENDIF}
  deh_actions[186].action.acp1 := @A_SpawnSmokeHorz;
  deh_actions[186].name := strupper('SpawnSmokeHorz');
  {$IFDEF DLL}deh_actions[186].decl := 'A_SpawnSmokeHorz(height: integer)';{$ENDIF}
  deh_actions[187].action.acp1 := @A_SetInteractive;
  deh_actions[187].name := strupper('SetInteractive');
  {$IFDEF DLL}deh_actions[187].decl := 'A_SetInteractive()';{$ENDIF}
  deh_actions[188].action.acp1 := @A_UnSetInteractive;
  deh_actions[188].name := strupper('UnSetInteractive');
  {$IFDEF DLL}deh_actions[188].decl := 'A_UnSetInteractive()';{$ENDIF}
  deh_actions[189].action.acp1 := @A_SetMonsterInfight;
  deh_actions[189].name := strupper('SetMonsterInfight');
  {$IFDEF DLL}deh_actions[189].decl := 'A_SetMonsterInfight()';{$ENDIF}
  deh_actions[190].action.acp1 := @A_UnSetMonsterInfight;
  deh_actions[190].name := strupper('UnSetMonsterInfight');
  {$IFDEF DLL}deh_actions[190].decl := 'A_UnSetMonsterInfight()';{$ENDIF}
  deh_actions[191].action.acp1 := @P_RemoveMobj;
  deh_actions[191].name := strupper('RemoveSelf');
  {$IFDEF DLL}deh_actions[191].decl := 'A_RemoveSelf()';{$ENDIF}
  deh_actions[192].action.acp1 := @A_NoiseAlert;
  deh_actions[192].name := strupper('NoiseAlert');
  {$IFDEF DLL}deh_actions[192].decl := 'A_NoiseAlert()';{$ENDIF}
  deh_actions[193].action.acp1 := @A_ConsoleCommand;
  deh_actions[193].name := strupper('ConsoleCommand');
  {$IFDEF DLL}deh_actions[193].decl := 'A_ConsoleCommand(cmd: string, [parm1: string], [parm2: string], ...)';{$ENDIF}
  deh_actions[194].action.acp1 := @A_SetCustomParam;
  deh_actions[194].name := strupper('SetCustomParam');
  {$IFDEF DLL}deh_actions[194].decl := 'A_SetCustomParam(param: string, value: integer)';{$ENDIF}
  deh_actions[195].action.acp1 := @A_AddCustomParam;
  deh_actions[195].name := strupper('AddCustomParam');
  {$IFDEF DLL}deh_actions[195].decl := 'A_AddCustomParam(param: string, value: integer)';{$ENDIF}
  deh_actions[196].action.acp1 := @A_SubtractCustomParam;
  deh_actions[196].name := strupper('SubtractCustomParam');
  {$IFDEF DLL}deh_actions[196].decl := 'A_SubtractCustomParam(param: string, value: integer)';{$ENDIF}
  deh_actions[197].action.acp1 := @A_SetTargetCustomParam;
  deh_actions[197].name := strupper('SetTargetCustomParam');
  {$IFDEF DLL}deh_actions[197].decl := 'A_SetTargetCustomParam(param: string, value: integer)';{$ENDIF}
  deh_actions[198].action.acp1 := @A_AddTargetCustomParam;
  deh_actions[198].name := strupper('AddTargetCustomParam');
  {$IFDEF DLL}deh_actions[198].decl := 'A_AddTargetCustomParam(param: string, value: integer)';{$ENDIF}
  deh_actions[199].action.acp1 := @A_SubtractTargetCustomParam;
  deh_actions[199].name := strupper('SubtractTargetCustomParam');
  {$IFDEF DLL}deh_actions[199].decl := 'A_SubtractTargetCustomParam(param: string, value: integer)';{$ENDIF}
  deh_actions[200].action.acp1 := @A_JumpIfCustomParam;
  deh_actions[200].name := strupper('JumpIfCustomParam');
  {$IFDEF DLL}deh_actions[200].decl := 'A_JumpIfCustomParam(param: string, value: integer, offset: integer)';{$ENDIF}
  deh_actions[201].action.acp1 := @A_JumpIfCustomParamLess;
  deh_actions[201].name := strupper('JumpIfCustomParamLess');
  {$IFDEF DLL}deh_actions[201].decl := 'A_JumpIfCustomParamLess(param: string, value: integer, offset: integer)';{$ENDIF}
  deh_actions[202].action.acp1 := @A_JumpIfCustomParamGreater;
  deh_actions[202].name := strupper('JumpIfCustomParamGreater');
  {$IFDEF DLL}deh_actions[202].decl := 'A_JumpIfCustomParamGreater(param: string, value: integer, offset: integer)';{$ENDIF}
  deh_actions[203].action.acp1 := @A_JumpIfTargetCustomParam;
  deh_actions[203].name := strupper('JumpIfTargetCustomParam');
  {$IFDEF DLL}deh_actions[203].decl := 'A_JumpIfTargetCustomParam(param: string, value: integer, offset: integer)';{$ENDIF}
  deh_actions[204].action.acp1 := @A_JumpIfTargetCustomParamLess;
  deh_actions[204].name := strupper('JumpIfTargetCustomParamLess');
  {$IFDEF DLL}deh_actions[204].decl := 'A_JumpIfTargetCustomParamLess(param: string, value: integer, offset: integer)';{$ENDIF}
  deh_actions[205].action.acp1 := @A_JumpIfTargetCustomParamGreater;
  deh_actions[205].name := strupper('JumpIfTargetCustomParamGreater');
  {$IFDEF DLL}deh_actions[205].decl := 'A_JumpIfTargetCustomParamGreater(param: string, value: integer, offset: integer)';{$ENDIF}
  deh_actions[206].action.acp1 := @A_SetShootable;
  deh_actions[206].name := strupper('SetShootable');
  {$IFDEF DLL}deh_actions[206].decl := 'A_SetShootable()';{$ENDIF}
  deh_actions[207].action.acp1 := @A_UnSetShootable;
  deh_actions[207].name := strupper('UnSetShootable');
  {$IFDEF DLL}deh_actions[207].decl := 'A_UnSetShootable()';{$ENDIF}
  deh_actions[208].action.acp1 := @A_PlayerMessage;
  deh_actions[208].name := strupper('PlayerMessage');
  {$IFDEF DLL}deh_actions[208].decl := 'A_PlayerMessage(msg1: string, [msg2: string], ...)';{$ENDIF}
  deh_actions[209].action.acp1 := @A_PlayerFaceMe;
  deh_actions[209].name := strupper('PlayerFaceMe');
  {$IFDEF DLL}deh_actions[209].decl := 'A_PlayerFaceMe(tics: integer)';{$ENDIF}
  deh_actions[210].action.acp1 := @A_GoTo;
  deh_actions[210].name := strupper('GoTo');
  {$IFDEF DLL}deh_actions[210].decl := 'A_GoTo(propability: random_t, state: state_t)';{$ENDIF}
  deh_actions[211].action.acp1 := @A_GoToIfCloser;
  deh_actions[211].name := strupper('GoToIfCloser');
  {$IFDEF DLL}deh_actions[211].decl := 'A_GoToIfCloser(distancetotarget: float, state: state_t)';{$ENDIF}
  deh_actions[212].action.acp1 := @A_GoToIfHealthLower;
  deh_actions[212].name := strupper('GoToIfHealthLower');
  {$IFDEF DLL}deh_actions[212].decl := 'A_GoToIfHealthLower(health: integer, state: state_t)';{$ENDIF}
  deh_actions[213].action.acp1 := @A_GoToIfCustomParam;
  deh_actions[213].name := strupper('GoToIfCustomParam');
  {$IFDEF DLL}deh_actions[213].decl := 'A_GoToIfCustomParam(param: string, value: integer, state: state_t)';{$ENDIF}
  deh_actions[214].action.acp1 := @A_GoToIfCustomParamLess;
  deh_actions[214].name := strupper('GoToIfCustomParamLess');
  {$IFDEF DLL}deh_actions[214].decl := 'A_GoToIfCustomParamLess(param: string, value: integer, state: state_t)';{$ENDIF}
  deh_actions[215].action.acp1 := @A_GoToIfCustomParamGreater;
  deh_actions[215].name := strupper('GoToIfCustomParamGreater');
  {$IFDEF DLL}deh_actions[215].decl := 'A_GoToIfCustomParamGreater(param: string, value: integer, state: state_t)';{$ENDIF}
  deh_actions[216].action.acp1 := @A_GoToIfTargetCustomParam;
  deh_actions[216].name := strupper('GoToIfTargetCustomParam');
  {$IFDEF DLL}deh_actions[216].decl := 'A_GoToIfTargetCustomParam(param: string, value: integer, state: state_t)';{$ENDIF}
  deh_actions[217].action.acp1 := @A_GoToIfTargetCustomParamLess;
  deh_actions[217].name := strupper('GoToIfTargetCustomParamLess');
  {$IFDEF DLL}deh_actions[217].decl := 'A_GoToIfTargetCustomParamLess(param: string, value: integer, state: state_t)';{$ENDIF}
  deh_actions[218].action.acp1 := @A_GoToIfTargetCustomParamGreater;
  deh_actions[218].name := strupper('GoToIfTargetCustomParamGreater');
  {$IFDEF DLL}deh_actions[218].decl := 'A_GoToIfTargetCustomParamGreater(param: string, value: integer, state: state_t)';{$ENDIF}
  deh_actions[219].action.acp1 := @A_SetFloorClip;
  deh_actions[219].name := strupper('SetFloorClip');
  {$IFDEF DLL}deh_actions[219].decl := 'A_SetFloorClip()';{$ENDIF}
  deh_actions[220].action.acp1 := @A_UnSetFloorClip;
  deh_actions[220].name := strupper('UnSetFloorClip');
  {$IFDEF DLL}deh_actions[220].decl := 'A_UnSetFloorClip()';{$ENDIF}
  deh_actions[221].action.acp1 := @A_SetFrightened;
  deh_actions[221].name := strupper('SetFrightened');
  {$IFDEF DLL}deh_actions[221].decl := 'A_SetFrightened()';{$ENDIF}
  deh_actions[222].action.acp1 := @A_UnSetFrightened;
  deh_actions[222].name := strupper('UnSetFrightened');
  {$IFDEF DLL}deh_actions[222].decl := 'A_UnSetFrightened()';{$ENDIF}
  deh_actions[223].action.acp1 := @A_SetNoDamage;
  deh_actions[223].name := strupper('SetNoDamage');
  {$IFDEF DLL}deh_actions[223].decl := 'A_SetNoDamage()';{$ENDIF}
  deh_actions[224].action.acp1 := @A_UnSetNoDamage;
  deh_actions[224].name := strupper('UnSetNoDamage');
  {$IFDEF DLL}deh_actions[224].decl := 'A_UnSetNoDamage()';{$ENDIF}
  deh_actions[225].action.acp1 := @A_RunScript;
  deh_actions[225].name := strupper('RunScript');
  {$IFDEF DLL}deh_actions[225].decl := 'A_RunScript(script1: string, [script2: string], ...)';{$ENDIF}
  deh_actions[226].action.acp1 := @A_GhostOn;
  deh_actions[226].name := strupper('GhostOn');
  {$IFDEF DLL}deh_actions[226].decl := 'A_GhostOn()';{$ENDIF}
  deh_actions[227].action.acp1 := @A_GhostOff;
  deh_actions[227].name := strupper('GhostOff');
  {$IFDEF DLL}deh_actions[227].decl := 'A_GhostOff()';{$ENDIF}
  deh_actions[228].action.acp1 := @A_Blocking;
  deh_actions[228].name := strupper('Blocking');
  {$IFDEF DLL}deh_actions[228].decl := 'A_Blocking()';{$ENDIF}
  deh_actions[229].action.acp1 := @A_DoNotRunScripts;
  deh_actions[229].name := strupper('DoNotRunScripts');
  {$IFDEF DLL}deh_actions[229].decl := 'A_DoNotRunScripts()';{$ENDIF}
  deh_actions[230].action.acp1 := @A_DoRunScripts;
  deh_actions[230].name := strupper('DoRunScripts');
  {$IFDEF DLL}deh_actions[230].decl := 'A_DoRunScripts()';{$ENDIF}
  deh_actions[231].action.acp1 := @A_TargetDropItem;
  deh_actions[231].name := strupper('TargetDropItem');
  {$IFDEF DLL}deh_actions[231].decl := 'A_TargetDropItem(dropitemtype: string)';{$ENDIF}
  deh_actions[232].action.acp1 := @A_DefaultTargetDropItem;
  deh_actions[232].name := strupper('DefaultTargetDropItem');
  {$IFDEF DLL}deh_actions[232].decl := 'A_DefaultTargetDropItem()';{$ENDIF}
  deh_actions[233].action.acp1 := @A_SetDropItem;
  deh_actions[233].name := strupper('SetDropItem');
  {$IFDEF DLL}deh_actions[233].decl := 'A_SetDropItem(dropitemtype: string)';{$ENDIF}
  deh_actions[234].action.acp1 := @A_SetDefaultDropItem;
  deh_actions[234].name := strupper('SetDefaultDropItem');
  {$IFDEF DLL}deh_actions[234].decl := 'A_SetDefaultDropItem()';{$ENDIF}
  deh_actions[235].action.acp1 := @A_GlobalEarthQuake;
  deh_actions[235].name := strupper('GlobalEarthQuake');
  {$IFDEF DLL}deh_actions[235].decl := 'A_GlobalEarthQuake(tics: integer)';{$ENDIF}
  deh_actions[236].action.acp1 := @A_JumpIfMapStringEqual;
  deh_actions[236].name := strupper('JumpIfMapStringEqual');
  {$IFDEF DLL}deh_actions[236].decl := 'A_JumpIfMapStringEqual(parm: string, value: string, offset; integer)';{$ENDIF}
  deh_actions[237].action.acp1 := @A_JumpIfMapStringLess;
  deh_actions[237].name := strupper('JumpIfMapStringLess');
  {$IFDEF DLL}deh_actions[237].decl := 'A_JumpIfMapStringLess(parm: string, value: string, offset; integer)';{$ENDIF}
  deh_actions[238].action.acp1 := @A_JumpIfMapStringGreater;
  deh_actions[238].name := strupper('JumpIfMapStringGreater');
  {$IFDEF DLL}deh_actions[238].decl := 'A_JumpIfMapStringGreater(parm: string, value: string, offset; integer)';{$ENDIF}
  deh_actions[239].action.acp1 := @A_JumpIfMapIntegerEqual;
  deh_actions[239].name := strupper('JumpIfMapIntegerEqual');
  {$IFDEF DLL}deh_actions[239].decl := 'A_JumpIfMapIntegerEqual(parm: string, value: integer, offset: integer)';{$ENDIF}
  deh_actions[240].action.acp1 := @A_JumpIfMapIntegerLess;
  deh_actions[240].name := strupper('JumpIfMapIntegerLess');
  {$IFDEF DLL}deh_actions[240].decl := 'A_JumpIfMapIntegerLess(parm: string, value: integer, offset: integer)';{$ENDIF}
  deh_actions[241].action.acp1 := @A_JumpIfMapIntegerGreater;
  deh_actions[241].name := strupper('JumpIfMapIntegerGreater');
  {$IFDEF DLL}deh_actions[241].decl := 'A_JumpIfMapIntegerGreater(parm: string, value: integer, offset: integer)';{$ENDIF}
  deh_actions[242].action.acp1 := @A_JumpIfMapFloatEqual;
  deh_actions[242].name := strupper('JumpIfMapFloatEqual');
  {$IFDEF DLL}deh_actions[242].decl := 'A_JumpIfMapFloatEqual(parm: string, value: float, offset: integer)';{$ENDIF}
  deh_actions[243].action.acp1 := @A_JumpIfMapFloatLess;
  deh_actions[243].name := strupper('JumpIfMapFloatLess');
  {$IFDEF DLL}deh_actions[243].decl := 'A_JumpIfMapFloatLess(parm: string, value: float, offset: integer)';{$ENDIF}
  deh_actions[244].action.acp1 := @A_JumpIfMapFloatGreater;
  deh_actions[244].name := strupper('JumpIfMapFloatGreater');
  {$IFDEF DLL}deh_actions[244].decl := 'A_JumpIfMapFloatGreater(parm: string, value: float, offset: integer)';{$ENDIF}
  deh_actions[245].action.acp1 := @A_JumpIfWorldStringEqual;
  deh_actions[245].name := strupper('JumpIfWorldStringEqual');
  {$IFDEF DLL}deh_actions[245].decl := 'A_JumpIfWorldStringEqual(parm: string, value: string, offset: integer)';{$ENDIF}
  deh_actions[246].action.acp1 := @A_JumpIfWorldStringLess;
  deh_actions[246].name := strupper('JumpIfWorldStringLess');
  {$IFDEF DLL}deh_actions[246].decl := 'A_JumpIfWorldStringLess(parm: string, value: string, offset: integer)';{$ENDIF}
  deh_actions[247].action.acp1 := @A_JumpIfWorldStringGreater;
  deh_actions[247].name := strupper('JumpIfWorldStringGreater');
  {$IFDEF DLL}deh_actions[247].decl := 'A_JumpIfWorldStringGreater(parm: string, value: string, offset: integer)';{$ENDIF}
  deh_actions[248].action.acp1 := @A_JumpIfWorldIntegerEqual;
  deh_actions[248].name := strupper('JumpIfWorldIntegerEqual');
  {$IFDEF DLL}deh_actions[248].decl := 'A_JumpIfWorldIntegerEqual(parm: string, value: integer, offset: integer)';{$ENDIF}
  deh_actions[249].action.acp1 := @A_JumpIfWorldIntegerLess;
  deh_actions[249].name := strupper('JumpIfWorldIntegerLess');
  {$IFDEF DLL}deh_actions[249].decl := 'A_JumpIfWorldIntegerLess(parm: string, value: integer, offset: integer)';{$ENDIF}
  deh_actions[250].action.acp1 := @A_JumpIfWorldIntegerGreater;
  deh_actions[250].name := strupper('JumpIfWorldIntegerGreater');
  {$IFDEF DLL}deh_actions[250].decl := 'A_JumpIfWorldIntegerGreater(parm: string, value: integer, offset: integer)';{$ENDIF}
  deh_actions[251].action.acp1 := @A_JumpIfWorldFloatEqual;
  deh_actions[251].name := strupper('JumpIfWorldFloatEqual');
  {$IFDEF DLL}deh_actions[251].decl := 'A_JumpIfWorldFloatEqual(parm: string, value: float, offset: integer)';{$ENDIF}
  deh_actions[252].action.acp1 := @A_JumpIfWorldFloatLess;
  deh_actions[252].name := strupper('JumpIfWorldFloatLess');
  {$IFDEF DLL}deh_actions[252].decl := 'A_JumpIfWorldFloatLess(parm: string, value: float, offset: integer)';{$ENDIF}
  deh_actions[253].action.acp1 := @A_JumpIfWorldFloatGreater;
  deh_actions[253].name := strupper('JumpIfWorldFloatGreater');
  {$IFDEF DLL}deh_actions[253].decl := 'A_JumpIfWorldFloatGreater(parm: string, value: float, offset: integer)';{$ENDIF}
  deh_actions[254].action.acp1 := @A_GoToIfMapStringEqual;
  deh_actions[254].name := strupper('GoToIfMapStringEqual');
  {$IFDEF DLL}deh_actions[254].decl := 'A_GoToIfMapStringEqual(parm: string, value: string, state: state_t)';{$ENDIF}
  deh_actions[255].action.acp1 := @A_GoToIfMapStringLess;
  deh_actions[255].name := strupper('GoToIfMapStringLess');
  {$IFDEF DLL}deh_actions[255].decl := 'A_GoToIfMapStringLess(parm: string, value: string, state: state_t)';{$ENDIF}
  deh_actions[256].action.acp1 := @A_GoToIfMapStringGreater;
  deh_actions[256].name := strupper('GoToIfMapStringGreater');
  {$IFDEF DLL}deh_actions[256].decl := 'A_GoToIfMapStringGreater(parm: string, value: string, state: state_t)';{$ENDIF}
  deh_actions[257].action.acp1 := @A_GoToIfMapIntegerEqual;
  deh_actions[257].name := strupper('GoToIfMapIntegerEqual');
  {$IFDEF DLL}deh_actions[257].decl := 'A_GoToIfMapIntegerEqual(parm: string, value: integer, state: state_t)';{$ENDIF}
  deh_actions[258].action.acp1 := @A_GoToIfMapIntegerLess;
  deh_actions[258].name := strupper('GoToIfMapIntegerLess');
  {$IFDEF DLL}deh_actions[258].decl := 'A_GoToIfMapIntegerLess(parm: string, value: integer, state: state_t)';{$ENDIF}
  deh_actions[259].action.acp1 := @A_GoToIfMapIntegerGreater;
  deh_actions[259].name := strupper('GoToIfMapIntegerGreater');
  {$IFDEF DLL}deh_actions[259].decl := 'A_GoToIfMapIntegerGreater(parm: string, value: integer, state: state_t)';{$ENDIF}
  deh_actions[260].action.acp1 := @A_GoToIfMapFloatEqual;
  deh_actions[260].name := strupper('GoToIfMapFloatEqual');
  {$IFDEF DLL}deh_actions[260].decl := 'A_GoToIfMapFloatEqual(parm: string, value: float, state: state_t)';{$ENDIF}
  deh_actions[261].action.acp1 := @A_GoToIfMapFloatLess;
  deh_actions[261].name := strupper('GoToIfMapFloatLess');
  {$IFDEF DLL}deh_actions[261].decl := 'A_GoToIfMapFloatLess(parm: string, value: float, state: state_t)';{$ENDIF}
  deh_actions[262].action.acp1 := @A_GoToIfMapFloatGreater;
  deh_actions[262].name := strupper('GoToIfMapFloatGreater');
  {$IFDEF DLL}deh_actions[262].decl := 'A_GoToIfMapFloatGreater(parm: string, value: float, state: state_t)';{$ENDIF}
  deh_actions[263].action.acp1 := @A_GoToIfWorldStringEqual;
  deh_actions[263].name := strupper('GoToIfWorldStringEqual');
  {$IFDEF DLL}deh_actions[263].decl := 'A_GoToIfWorldStringEqual(parm: string, value: string, state: state_t)';{$ENDIF}
  deh_actions[264].action.acp1 := @A_GoToIfWorldStringLess;
  deh_actions[264].name := strupper('GoToIfWorldStringLess');
  {$IFDEF DLL}deh_actions[264].decl := 'A_GoToIfWorldStringLess(parm: string, value: string, state: state_t)';{$ENDIF}
  deh_actions[265].action.acp1 := @A_GoToIfWorldStringGreater;
  deh_actions[265].name := strupper('GoToIfWorldStringGreater');
  {$IFDEF DLL}deh_actions[265].decl := 'A_GoToIfWorldStringGreater(parm: string, value: string, state: state_t)';{$ENDIF}
  deh_actions[266].action.acp1 := @A_GoToIfWorldIntegerEqual;
  deh_actions[266].name := strupper('GoToIfWorldIntegerEqual');
  {$IFDEF DLL}deh_actions[266].decl := 'A_GoToIfWorldIntegerEqual(parm: string, value: integer, state: state_t)';{$ENDIF}
  deh_actions[267].action.acp1 := @A_GoToIfWorldIntegerLess;
  deh_actions[267].name := strupper('GoToIfWorldIntegerLess');
  {$IFDEF DLL}deh_actions[267].decl := 'A_GoToIfWorldIntegerLess(parm: string, value: integer, state: state_t)';{$ENDIF}
  deh_actions[268].action.acp1 := @A_GoToIfWorldIntegerGreater;
  deh_actions[268].name := strupper('GoToIfWorldIntegerGreater');
  {$IFDEF DLL}deh_actions[268].decl := 'A_GoToIfWorldIntegerGreater(parm: string, value: integer, state: state_t)';{$ENDIF}
  deh_actions[269].action.acp1 := @A_GoToIfWorldFloatEqual;
  deh_actions[269].name := strupper('GoToIfWorldFloatEqual');
  {$IFDEF DLL}deh_actions[269].decl := 'A_GoToIfWorldFloatEqual(parm: string, value: float, state: state_t)';{$ENDIF}
  deh_actions[270].action.acp1 := @A_GoToIfWorldFloatLess;
  deh_actions[270].name := strupper('GoToIfWorldFloatLess');
  {$IFDEF DLL}deh_actions[270].decl := 'A_GoToIfWorldFloatLess(parm: string, value: float, state: state_t)';{$ENDIF}
  deh_actions[271].action.acp1 := @A_GoToIfWorldFloatGreater;
  deh_actions[271].name := strupper('GoToIfWorldFloatGreater');
  {$IFDEF DLL}deh_actions[271].decl := 'A_GoToIfWorldFloatGreater(parm: string, value: float, state: state_t)';{$ENDIF}
  deh_actions[272].action.acp1 := @A_SetMapStr;
  deh_actions[272].name := strupper('SetMapStr');
  {$IFDEF DLL}deh_actions[272].decl := 'A_SetMapStr(mvar: string; value1: string; [value2: string],...)';{$ENDIF}
  deh_actions[273].action.acp1 := @A_SetWorldStr;
  deh_actions[273].name := strupper('SetWorldStr');
  {$IFDEF DLL}deh_actions[273].decl := 'A_SetWorldStr(wvar: string; value1: string; [value2: string],...)';{$ENDIF}
  deh_actions[274].action.acp1 := @A_SetMapInt;
  deh_actions[274].name := strupper('SetMapInt');
  {$IFDEF DLL}deh_actions[274].decl := 'A_SetMapInt(mvar: string; value: integer)';{$ENDIF}
  deh_actions[275].action.acp1 := @A_SetWorldInt;
  deh_actions[275].name := strupper('SetWorldInt');
  {$IFDEF DLL}deh_actions[275].decl := 'A_SetWorldInt(wvar: string; value: integer)';{$ENDIF}
  deh_actions[276].action.acp1 := @A_SetMapFloat;
  deh_actions[276].name := strupper('SetMapFloat');
  {$IFDEF DLL}deh_actions[276].decl := 'A_SetMapFloat(mvar: string; value: float)';{$ENDIF}
  deh_actions[277].action.acp1 := @A_SetWorldFloat;
  deh_actions[277].name := strupper('SetWorldFloat');
  {$IFDEF DLL}deh_actions[277].decl := 'A_SetWorldFloat(wvar: string; value: float)';{$ENDIF}
  deh_actions[278].action.acp1 := @A_RandomGoto;
  deh_actions[278].name := strupper('RandomGoto');
  {$IFDEF DLL}deh_actions[278].decl := 'A_RandomGoto(state1: state_t; [state2: state_t],...)';{$ENDIF}
  deh_actions[279].action.acp1 := @A_ResetHealth;
  deh_actions[279].name := strupper('ResetHealth');
  {$IFDEF DLL}deh_actions[279].decl := 'A_ResetHealth';{$ENDIF}
  deh_actions[280].action.acp1 := @A_Recoil;
  deh_actions[280].name := strupper('Recoil');
  {$IFDEF DLL}deh_actions[280].decl := 'A_Recoil(xymom: float)';{$ENDIF}
  deh_actions[281].action.acp1 := @A_SetSolid;
  deh_actions[281].name := strupper('SetSolid');
  {$IFDEF DLL}deh_actions[281].decl := 'A_SetSolid()';{$ENDIF}
  deh_actions[282].action.acp1 := @A_UnSetSolid;
  deh_actions[282].name := strupper('UnSetSolid');
  {$IFDEF DLL}deh_actions[282].decl := 'A_UnSetSolid()';{$ENDIF}
  deh_actions[283].action.acp1 := @A_SetFloat;
  deh_actions[283].name := strupper('SetFloat');
  {$IFDEF DLL}deh_actions[283].decl := 'A_SetFloat()';{$ENDIF}
  deh_actions[284].action.acp1 := @A_UnSetFloat;
  deh_actions[284].name := strupper('UnSetFloat');
  {$IFDEF DLL}deh_actions[284].decl := 'A_UnSetFloat()';{$ENDIF}
  deh_actions[285].action.acp1 := @A_SetHealth;
  deh_actions[285].name := strupper('SetHealth');
  {$IFDEF DLL}deh_actions[285].decl := 'A_SetHealth(h: integer)';{$ENDIF}
  deh_actions[286].action.acp1 := @A_ResetTargetHealth;
  deh_actions[286].name := strupper('ResetTargetHealth');
  {$IFDEF DLL}deh_actions[286].decl := 'A_ResetTargetHealth()';{$ENDIF}
  deh_actions[287].action.acp1 := @A_SetTargetHealth;
  deh_actions[287].name := strupper('SetTargetHealth');
  {$IFDEF DLL}deh_actions[287].decl := 'A_SetTargetHealth(h: integer)';{$ENDIF}
  deh_actions[288].action.acp1 := @A_ScaleVelocity;
  deh_actions[288].name := strupper('ScaleVelocity');
  {$IFDEF DLL}deh_actions[288].decl := 'ScaleVelocity(scale: float)';{$ENDIF}
  deh_actions[289].action.acp1 := @A_ChangeVelocity;
  deh_actions[289].name := strupper('ChangeVelocity');
  {$IFDEF DLL}deh_actions[289].decl := 'A_ChangeVelocity(velx: float, vely: float, velz: float, flags: float)';{$ENDIF}
  deh_actions[290].action.acp1 := @A_JumpIf;
  deh_actions[290].name := strupper('JumpIf');
  {$IFDEF DLL}deh_actions[290].decl := 'A_JumpIf(propability: boolean, offset1: integer, [offset2: integer], ...)';{$ENDIF}
  deh_actions[291].action.acp1 := @A_MusicChanger;
  deh_actions[291].name := strupper('MusicChanger');
  {$IFDEF DLL}deh_actions[291].decl := 'A_MusicChanger';{$ENDIF}
  deh_actions[292].action.acp1 := @A_SetPushFactor;
  deh_actions[292].name := strupper('SetPushFactor');
  {$IFDEF DLL}deh_actions[292].decl := 'A_SetPushFactor(f: float)';{$ENDIF}
  deh_actions[293].action.acp1 := @A_SetScale;
  deh_actions[293].name := strupper('SetScale');
  {$IFDEF DLL}deh_actions[293].decl := 'A_SetScale(s: float)';{$ENDIF}
  deh_actions[294].action.acp1 := @A_SetGravity;
  deh_actions[294].name := strupper('SetGravity');
  {$IFDEF DLL}deh_actions[294].decl := 'A_SetGravity(g: float)';{$ENDIF}
  deh_actions[295].action.acp1 := @A_SetFloorBounce;
  deh_actions[295].name := strupper('SetFloorBounce');
  {$IFDEF DLL}deh_actions[295].decl := 'A_SetFloorBounce()';{$ENDIF}
  deh_actions[296].action.acp1 := @A_UnSetFloorBounce;
  deh_actions[296].name := strupper('UnSetFloorBounce');
  {$IFDEF DLL}deh_actions[296].decl := 'A_UnSetFloorBounce()';{$ENDIF}
  deh_actions[297].action.acp1 := @A_SetCeilingBounce;
  deh_actions[297].name := strupper('SetCeilingBounce');
  {$IFDEF DLL}deh_actions[297].decl := 'A_SetCeilingBounce()';{$ENDIF}
  deh_actions[298].action.acp1 := @A_UnSetCeilingBounce;
  deh_actions[298].name := strupper('UnSetCeilingBounce');
  {$IFDEF DLL}deh_actions[298].decl := 'A_UnSetCeilingBounce()';{$ENDIF}
  deh_actions[299].action.acp1 := @A_SetWallBounce;
  deh_actions[299].name := strupper('SetWallBounce');
  {$IFDEF DLL}deh_actions[299].decl := 'A_SetWallBounce()';{$ENDIF}
  deh_actions[300].action.acp1 := @A_UnSetWallBounce;
  deh_actions[300].name := strupper('UnSetWallBounce');
  {$IFDEF DLL}deh_actions[300].decl := 'A_UnSetWallBounce()';{$ENDIF}
  deh_actions[301].action.acp1 := @A_GlowLight;
  deh_actions[301].name := strupper('GlowLight');
  {$IFDEF DLL}deh_actions[301].decl := 'A_GlowLight(color: string)';{$ENDIF}
  deh_actions[302].action.acp1 := @A_TraceNearestPlayer;
  deh_actions[302].name := strupper('TraceNearestPlayer');
  {$IFDEF DLL}deh_actions[302].decl := 'A_TraceNearestPlayer(pct: integer, [maxturn: angle_t])';{$ENDIF}
  deh_actions[303].action.acp1 := @A_ChangeFlag;
  deh_actions[303].name := strupper('ChangeFlag');
  {$IFDEF DLL}deh_actions[303].decl := 'A_ChangeFlag(flag: string, onoff: boolean)';{$ENDIF}
  deh_actions[304].action.acp1 := @A_CheckFloor;
  deh_actions[304].name := strupper('CheckFloor');
  {$IFDEF DLL}deh_actions[304].decl := 'A_CheckFloor(offset: integer)';{$ENDIF}
  deh_actions[305].action.acp1 := @A_CheckCeiling;
  deh_actions[305].name := strupper('CheckCeiling');
  {$IFDEF DLL}deh_actions[305].decl := 'A_CheckCeiling(offset: integer)';{$ENDIF}
  deh_actions[306].action.acp1 := @A_StopSound;
  deh_actions[306].name := strupper('StopSound');
  {$IFDEF DLL}deh_actions[306].decl := 'A_StopSound()';{$ENDIF}
  deh_actions[307].action.acp1 := @A_JumpIfTargetOutsideMeleeRange;
  deh_actions[307].name := strupper('JumpIfTargetOutsideMeleeRange');
  {$IFDEF DLL}deh_actions[307].decl := 'A_JumpIfTargetOutsideMeleeRange(offset: integer)';{$ENDIF}
  deh_actions[308].action.acp1 := @A_JumpIfTargetInsideMeleeRange;
  deh_actions[308].name := strupper('JumpIfTargetInsideMeleeRange');
  {$IFDEF DLL}deh_actions[308].decl := 'A_JumpIfTargetInsideMeleeRange(offset: integer)';{$ENDIF}
  deh_actions[309].action.acp1 := @A_JumpIfTracerCloser;
  deh_actions[309].name := strupper('JumpIfTracerCloser');
  {$IFDEF DLL}deh_actions[309].decl := 'A_JumpIfTracerCloser(distancetotarget: float, offset: integer)';{$ENDIF}
  deh_actions[310].action.acp1 := @A_SetMass;
  deh_actions[310].name := strupper('SetMass');
  {$IFDEF DLL}deh_actions[310].decl := 'A_SetMass(mass: integer)';{$ENDIF}
  deh_actions[311].action.acp1 := @A_SetTargetMass;
  deh_actions[311].name := strupper('SetTargetMass');
  {$IFDEF DLL}deh_actions[311].decl := 'A_SetTargetMass(mass: integer)';{$ENDIF}
  deh_actions[312].action.acp1 := @A_SetTracerMass;
  deh_actions[312].name := strupper('SetTracerMass');
  {$IFDEF DLL}deh_actions[312].decl := 'A_SetTracerMass(mass: integer)';{$ENDIF}
  deh_actions[313].action.acp1 := @A_CheckSight;
  deh_actions[313].name := strupper('CheckSight');
  {$IFDEF DLL}deh_actions[313].decl := 'A_CheckSight(offset: integer)';{$ENDIF}
  deh_actions[314].action.acp1 := @A_CheckSightOrRange;
  deh_actions[314].name := strupper('CheckSightOrRange');
  {$IFDEF DLL}deh_actions[314].decl := 'A_CheckSightOrRange(distance: float, offset: integer, [twodi: boolean=false])';{$ENDIF}
  deh_actions[315].action.acp1 := @A_CheckRange;
  deh_actions[315].name := strupper('CheckRange');
  {$IFDEF DLL}deh_actions[315].decl := 'A_CheckRange(distance: float, offset: integer, [twodi: boolean=false])';{$ENDIF}
  deh_actions[316].action.acp1 := @A_CountdownArg;
  deh_actions[316].name := strupper('CountdownArg');
  {$IFDEF DLL}deh_actions[316].decl := 'A_CountdownArg(arg: integer, offset: integer)';{$ENDIF}
  deh_actions[317].action.acp1 := @A_SetArg;
  deh_actions[317].name := strupper('SetArg');
  {$IFDEF DLL}deh_actions[317].decl := 'A_SetArg(arg: integer, value: integer)';{$ENDIF}
  deh_actions[318].action.acp1 := @A_SetSpecial;
  deh_actions[318].name := strupper('SetSpecial');
  {$IFDEF DLL}deh_actions[318].decl := 'A_SetSpecial(special: integer, [arg1, arg2, arg3, arg4, arg5: integer])';{$ENDIF}
  deh_actions[319].action.acp1 := @A_CheckFlag;
  deh_actions[319].name := strupper('CheckFlag');
  {$IFDEF DLL}deh_actions[319].decl := 'A_CheckFlag(flag: string, offset: integer, [aaprt: AAPTR])';{$ENDIF}
  deh_actions[320].action.acp1 := @A_SetAngle;
  deh_actions[320].name := strupper('SetAngle');
  {$IFDEF DLL}deh_actions[320].decl := 'A_SetAngle(angle: integer, [flags: integer], [aaprt: AAPTR])';{$ENDIF}
  deh_actions[321].action.acp1 := @A_SetUserVar;
  deh_actions[321].name := strupper('SetUserVar');
  {$IFDEF DLL}deh_actions[321].decl := 'A_SetUserVar(varname: string, value: integer)';{$ENDIF}
  deh_actions[322].action.acp1 := @A_SetUserArray;
  deh_actions[322].name := strupper('SetUserArray');
  {$IFDEF DLL}deh_actions[322].decl := 'A_SetUserArray(varname: string, index: integer, value: integer)';{$ENDIF}
  deh_actions[323].action.acp1 := @A_SetTics;
  deh_actions[323].name := strupper('SetTics');
  {$IFDEF DLL}deh_actions[323].decl := 'A_SetTics(tics: integer)';{$ENDIF}
  deh_actions[324].action.acp1 := @A_DropItem;
  deh_actions[324].name := strupper('DropItem');
  {$IFDEF DLL}deh_actions[324].decl := 'A_DropItem(spawntype: string, amount: integer, chance: integer)';{$ENDIF}
  deh_actions[325].action.acp1 := @A_DamageSelf;
  deh_actions[325].name := strupper('DamageSelf');
  {$IFDEF DLL}deh_actions[325].decl := 'A_DamageSelf(actor: Pmobj_t)';{$ENDIF}
  deh_actions[326].action.acp1 := @A_DamageTarget;
  deh_actions[326].name := strupper('DamageTarget');
  {$IFDEF DLL}deh_actions[326].decl := 'A_DamageTarget(const damage: integer)';{$ENDIF}
  deh_actions[327].action.acp1 := @A_DamageTracer;
  deh_actions[327].name := strupper('DamageTracer');
  {$IFDEF DLL}deh_actions[327].decl := 'A_DamageTracer(const damage: integer)';{$ENDIF}
  deh_actions[328].action.acp1 := @A_KillTarget;
  deh_actions[328].name := strupper('KillTarget');
  {$IFDEF DLL}deh_actions[328].decl := 'A_KillTarget()';{$ENDIF}
  deh_actions[329].action.acp1 := @A_KillTracer;
  deh_actions[329].name := strupper('KillTracer');
  {$IFDEF DLL}deh_actions[329].decl := 'A_KillTracer()';{$ENDIF}
  deh_actions[330].action.acp1 := @A_RemoveTarget;
  deh_actions[330].name := strupper('RemoveTarget');
  {$IFDEF DLL}deh_actions[330].decl := 'A_RemoveTarget([flags: integer])';{$ENDIF}
  deh_actions[331].action.acp1 := @A_RemoveTracer;
  deh_actions[331].name := strupper('RemoveTracer');
  {$IFDEF DLL}deh_actions[331].decl := 'A_RemoveTracer([flags: integer])';{$ENDIF}
  deh_actions[332].action.acp1 := @A_Remove;
  deh_actions[332].name := strupper('Remove');
  {$IFDEF DLL}deh_actions[332].decl := 'A_Remove(aaprt: AAPTR, [flags: integer])';{$ENDIF}
  deh_actions[333].action.acp1 := @A_SetFloatBobPhase;
  deh_actions[333].name := strupper('SetFloatBobPhase');
  {$IFDEF DLL}deh_actions[333].decl := 'A_SetFloatBobPhase(bob: integer)';{$ENDIF}
  deh_actions[334].action.acp1 := @A_Detonate;
  deh_actions[334].name := strupper('Detonate');
  {$IFDEF DLL}deh_actions[334].decl := 'A_Detonate()';{$ENDIF}
  deh_actions[335].action.acp1 := @A_Spawn;
  deh_actions[335].name := strupper('Spawn');
  {$IFDEF DLL}deh_actions[335].decl := 'A_Spawn()';{$ENDIF}
  deh_actions[336].action.acp1 := @A_Face;
  deh_actions[336].name := strupper('Face');
  {$IFDEF DLL}deh_actions[336].decl := 'A_Face()';{$ENDIF}
  deh_actions[337].action.acp1 := @A_Scratch;
  deh_actions[337].name := strupper('Scratch');
  {$IFDEF DLL}deh_actions[337].decl := 'A_Scratch()';{$ENDIF}
  deh_actions[338].action.acp1 := @A_RandomJump;
  deh_actions[338].name := strupper('RandomJump');
  {$IFDEF DLL}deh_actions[338].decl := 'A_RandomJump()';{$ENDIF}
  deh_actions[339].action.acp1 := @A_LineEffect;
  deh_actions[339].name := strupper('LineEffect');
  {$IFDEF DLL}deh_actions[339].decl := 'A_LineEffect()';{$ENDIF}
  deh_actions[340].action.acp1 := @A_FlipSprite;
  deh_actions[340].name := strupper('FlipSprite');
  {$IFDEF DLL}deh_actions[340].decl := 'A_FlipSprite()';{$ENDIF}
  deh_actions[341].action.acp1 := @A_NoFlipSprite;
  deh_actions[341].name := strupper('NoFlipSprite');
  {$IFDEF DLL}deh_actions[341].decl := 'A_NoFlipSprite()';{$ENDIF}
  deh_actions[342].action.acp1 := @A_RandomFlipSprite;
  deh_actions[342].name := strupper('RandomFlipSprite');
  {$IFDEF DLL}deh_actions[342].decl := 'A_RandomFlipSprite(chance: integer)';{$ENDIF}
  deh_actions[343].action.acp1 := @A_RandomNoFlipSprite;
  deh_actions[343].name := strupper('RandomNoFlipSprite');
  {$IFDEF DLL}deh_actions[343].decl := 'A_RandomNoFlipSprite(chance: integer)';{$ENDIF}
  deh_actions[344].action.acp1 := @A_CustomMeleeAttack;
  deh_actions[344].name := strupper('CustomMeleeAttack');
  {$IFDEF DLL}deh_actions[344].decl := 'A_CustomMeleeAttack(damage: integer, meleesound: string, misssound: string)';{$ENDIF}
  deh_actions[345].action.acp1 := @A_CustomComboAttack;
  deh_actions[345].name := strupper('CustomComboAttack');
  {$IFDEF DLL}deh_actions[345].decl := 'A_CustomComboAttack(missiletype: string, spawnheight: integer, damage: integer, meleesound: string)';{$ENDIF}
  deh_actions[346].action.acp1 := @A_SetRenderStyle;
  deh_actions[346].name := strupper('SetRenderStyle');
  {$IFDEF DLL}deh_actions[346].decl := 'A_SetRenderStyle(style: renderstyle_t, alpha: float)';{$ENDIF}
  deh_actions[347].action.acp1 := @A_FadeTo;
  deh_actions[347].name := strupper('FadeTo');
  {$IFDEF DLL}deh_actions[347].decl := 'A_FadeTo(targ: integer, ammount: integer, flags: integer)';{$ENDIF}
  deh_actions[348].action.acp1 := @A_SetSize;
  deh_actions[348].name := strupper('SetSize');
  {$IFDEF DLL}deh_actions[348].decl := 'A_SetSize(newradius: integer, newheight: integer, testpos: boolean)';{$ENDIF}
  deh_actions[349].action.acp1 := @A_RaiseMaster;
  deh_actions[349].name := strupper('RaiseMaster');
  {$IFDEF DLL}deh_actions[349].decl := 'A_RaiseMaster(copyfriendliness: boolean)';{$ENDIF}
  deh_actions[350].action.acp1 := @A_RaiseChildren;
  deh_actions[350].name := strupper('RaiseChildren');
  {$IFDEF DLL}deh_actions[350].decl := 'A_RaiseChildren(copyfriendliness: boolean)';{$ENDIF}
  deh_actions[351].action.acp1 := @A_RaiseSiblings;
  deh_actions[351].name := strupper('RaiseSiblings');
  {$IFDEF DLL}deh_actions[351].decl := 'A_RaiseSiblings(copyfriendliness: boolean)';{$ENDIF}
  deh_actions[352].action.acp1 := @A_SetMasterMass;
  deh_actions[352].name := strupper('SetMasterMass');
  {$IFDEF DLL}deh_actions[352].decl := 'A_SetMasterMass(mass: integer)';{$ENDIF}
  deh_actions[353].action.acp1 := @A_KillMaster;
  deh_actions[353].name := strupper('KillMaster');
  {$IFDEF DLL}deh_actions[353].decl := 'A_KillMaster()';{$ENDIF}
  deh_actions[354].action.acp1 := @A_DamageMaster;
  deh_actions[354].name := strupper('DamageMaster');
  {$IFDEF DLL}deh_actions[354].decl := 'A_DamageMaster(const damage: integer)';{$ENDIF}
  deh_actions[355].action.acp1 := @A_HealThing;
  deh_actions[355].name := strupper('HealThing');
  {$IFDEF DLL}deh_actions[355].decl := 'A_HealThing(amount: integer, max: integer)';{$ENDIF}
  deh_actions[356].action.acp1 := @A_RemoveMaster;
  deh_actions[356].name := strupper('RemoveMaster');
  {$IFDEF DLL}deh_actions[356].decl := 'A_RemoveMaster([flags: integer])';{$ENDIF}
  deh_actions[357].action.acp1 := @A_BasicAttack;
  deh_actions[357].name := strupper('BasicAttack');
  {$IFDEF DLL}deh_actions[357].decl := 'A_BasicAttack(MeleeDamage: integer, MeleeSound: integer, MissileType: integer, MissileHeight: float)';{$ENDIF}
  deh_actions[358].action.acp1 := @A_SetMasterArg;
  deh_actions[358].name := strupper('SetMasterArg');
  {$IFDEF DLL}deh_actions[358].decl := 'A_SetMasterArg(arg: integer; value: integer)';{$ENDIF}
  deh_actions[359].action.acp1 := @A_SetTargetArg;
  deh_actions[359].name := strupper('SetTargetArg');
  {$IFDEF DLL}deh_actions[359].decl := 'A_SetTargetArg(arg: integer; value: integer)';{$ENDIF}
  deh_actions[360].action.acp1 := @A_SetTracerArg;
  deh_actions[360].name := strupper('SetTracerArg');
  {$IFDEF DLL}deh_actions[360].decl := 'A_SetTracerArg(arg: integer; value: integer)';{$ENDIF}
  deh_actions[361].action.acp1 := @A_Tracer2;
  deh_actions[361].name := strupper('Tracer2');
  {$IFDEF DLL}deh_actions[361].decl := 'A_Tracer2()';{$ENDIF}
  deh_actions[362].action.acp1 := @A_MonsterRefire;
  deh_actions[362].name := strupper('MonsterRefire');
  {$IFDEF DLL}deh_actions[362].decl := 'A_MonsterRefire(prob: integer, offset: state_t)';{$ENDIF}
  deh_actions[363].action.acp1 := @A_RearrangePointers;
  deh_actions[363].name := strupper('RearrangePointers');
  {$IFDEF DLL}deh_actions[363].decl := 'A_RearrangePointers(ptr_target: integer, ptr_master: integer, ptr_tracer: integer, flags: integer)';{$ENDIF}
  deh_actions[364].action.acp1 := @A_TransferPointer;
  deh_actions[364].name := strupper('TransferPointer');
  {$IFDEF DLL}deh_actions[364].decl := 'A_TransferPointer(ptr_source: integer, ptr_recipient: integer, ptr_sourcefield: integer, [ptr_recipientfield: integer], [flags: integer])';{$ENDIF}
  deh_actions[365].action.acp1 := @A_AlertMonsters;
  deh_actions[365].name := strupper('AlertMonsters');
  {$IFDEF DLL}deh_actions[365].decl := 'A_AlertMonsters(maxdist: integer, flags: integer)';{$ENDIF}
  deh_actions[366].action.acp1 := @A_LocalEarthQuake;
  deh_actions[366].name := strupper('LocalEarthQuake');
  {$IFDEF DLL}deh_actions[366].decl := 'A_LocalEarthQuake(tics: integer; [intensity: float = 1.0]; [maxdist: float = MAXINT] ;)';{$ENDIF}
  deh_actions[367].action.acp1 := @A_LocalEarthQuake;
  deh_actions[367].name := strupper('Quake');
  {$IFDEF DLL}deh_actions[367].decl := 'A_Quake(tics: integer; [intensity: float = 1.0]; [maxdist: float = MAXINT] ;)';{$ENDIF}
  deh_actions[368].action.acp1 := @A_RemoveChildren;
  deh_actions[368].name := strupper('RemoveChildren');
  {$IFDEF DLL}deh_actions[368].decl := 'A_RemoveChildren([flags: integer])';{$ENDIF}
  deh_actions[369].action.acp1 := @A_RemoveSiblings;
  deh_actions[369].name := strupper('RemoveSiblings');
  {$IFDEF DLL}deh_actions[369].decl := 'A_RemoveSiblings([flags: integer])';{$ENDIF}
  deh_actions[370].action.acp1 := @A_KillChildren;
  deh_actions[370].name := strupper('KillChildren');
  {$IFDEF DLL}deh_actions[370].decl := 'A_KillChildren()';{$ENDIF}
  deh_actions[371].action.acp1 := @A_KillSiblings;
  deh_actions[371].name := strupper('KillSiblings');
  {$IFDEF DLL}deh_actions[371].decl := 'A_KillSiblings()';{$ENDIF}
  deh_actions[372].action.acp1 := @A_Weave;
  deh_actions[372].name := strupper('Weave');
  {$IFDEF DLL}deh_actions[372].decl := 'A_Weave(xyspeed: integer = 2, zspeed: integer = 2, xydist: float = 2.0, zdist: float = 1.0)';{$ENDIF}
  deh_actions[373].action.acp1 := @A_SetWeaveIndexXY;
  deh_actions[373].name := strupper('SetWeaveIndexXY');
  {$IFDEF DLL}deh_actions[373].decl := 'A_SetWeaveIndexXY(weavexy: integer)';{$ENDIF}
  deh_actions[374].action.acp1 := @A_SetWeaveIndexZ;
  deh_actions[374].name := strupper('SetWeaveIndexZ');
  {$IFDEF DLL}deh_actions[374].decl := 'A_SetWeaveIndexZ(weavez: integer)';{$ENDIF}
  deh_actions[375].action.acp1 := @A_SetWeaveIndexes;
  deh_actions[375].name := strupper('SetWeaveIndexes');
  {$IFDEF DLL}deh_actions[375].decl := 'A_SetWeaveIndexes(weavexy: integer, weavez: integer)';{$ENDIF}
  deh_actions[376].action.acp1 := @A_SetHeight;
  deh_actions[376].name := strupper('SetHeight');
  {$IFDEF DLL}deh_actions[376].decl := 'A_SetHeight(newheight: float)';{$ENDIF}
  deh_actions[377].action.acp1 := @A_OverlayClear;
  deh_actions[377].name := strupper('OverlayClear');
  {$IFDEF DLL}deh_actions[377].decl := 'A_OverlayClear()';{$ENDIF}
  deh_actions[378].action.acp1 := @A_OverlayDrawPatch;
  deh_actions[378].name := strupper('OverlayDrawPatch');
  {$IFDEF DLL}deh_actions[378].decl := 'A_OverlayDrawPatch(ticks: Integer; patchname: string; x, y: Integer ;)';{$ENDIF}
  deh_actions[379].action.acp1 := @A_OverlayDrawPatchStretched;
  deh_actions[379].name := strupper('OverlayDrawPatchStretched');
  {$IFDEF DLL}deh_actions[379].decl := 'A_OverlayDrawPatchStretched(ticks: Integer; patchname: string; x1, y1, x2, y2: Integer ;)';{$ENDIF}
  deh_actions[380].action.acp1 := @A_OverlayDrawPixel;
  deh_actions[380].name := strupper('OverlayDrawPixel');
  {$IFDEF DLL}deh_actions[380].decl := 'A_OverlayDrawPixel(ticks: Integer; red, green, blue: byte; x, y: Integer ;)';{$ENDIF}
  deh_actions[381].action.acp1 := @A_OverlayDrawRect;
  deh_actions[381].name := strupper('OverlayDrawRect');
  {$IFDEF DLL}deh_actions[381].decl := 'A_OverlayDrawRect(ticks: Integer; red, green, blue: byte; x1, y1, x2, y2: Integer ;)';{$ENDIF}
  deh_actions[382].action.acp1 := @A_OverlayDrawLine;
  deh_actions[382].name := strupper('OverlayDrawLine');
  {$IFDEF DLL}deh_actions[382].decl := 'A_OverlayDrawLine(ticks: Integer; red, green, blue: byte; x1, y1, x2, y2: Integer ;)';{$ENDIF}
  deh_actions[383].action.acp1 := @A_OverlayDrawText;
  deh_actions[383].name := strupper('OverlayDrawText');
  {$IFDEF DLL}deh_actions[383].decl := 'A_OverlayDrawText(ticks: Integer; txt: string; align: Integer; x, y: Integer ;)';{$ENDIF}
  deh_actions[384].action.acp1 := @A_OverlayDrawLeftText;
  deh_actions[384].name := strupper('OverlayDrawLeftText');
  {$IFDEF DLL}deh_actions[384].decl := 'A_OverlayDrawLeftText(ticks: Integer; txt: string; x, y: Integer ;)';{$ENDIF}
  deh_actions[385].action.acp1 := @A_OverlayDrawRightText;
  deh_actions[385].name := strupper('OverlayDrawRightText');
  {$IFDEF DLL}deh_actions[385].decl := 'A_OverlayDrawRightText(ticks: Integer; txt: string; x, y: Integer ;)';{$ENDIF}
  deh_actions[386].action.acp1 := @A_OverlayDrawCenterText;
  deh_actions[386].name := strupper('OverlayDrawCenterText');
  {$IFDEF DLL}deh_actions[386].decl := 'A_OverlayDrawCenterText(ticks: Integer; txt: string; x, y: Integer ;)';{$ENDIF}
  deh_actions[387].action.acp1 := @A_SetFriction;
  deh_actions[387].name := strupper('SetFriction');
  {$IFDEF DLL}deh_actions[387].decl := 'A_SetFriction(newfriction: float)';{$ENDIF}
  deh_actions[388].action.acp1 := @A_PlayerHurtExplode;
  deh_actions[388].name := strupper('PlayerHurtExplode');
  {$IFDEF DLL}deh_actions[388].decl := 'A_PlayerHurtExplode(damage: integer, radius: integer)';{$ENDIF}
  deh_actions[389].action.acp1 := @A_SetPushable;
  deh_actions[389].name := strupper('SetPushable');
  {$IFDEF DLL}deh_actions[389].decl := 'A_SetPushable()';{$ENDIF}
  deh_actions[390].action.acp1 := @A_UnSetPushable;
  deh_actions[390].name := strupper('UnSetPushable');
  {$IFDEF DLL}deh_actions[390].decl := 'A_UnSetPushable()';{$ENDIF}


  for i := 0 to DEHNUMACTIONS - 1 do
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

  for i := 0 to 8 do
    DEH_AddString(@deh_strings, @mapnames[i], 'HUSTR_' + IntToStrZFill(2, i + 1));

  for i := 9 to 33 do
    DEH_AddString(@deh_strings, @mapnames[i], 'HUSTR_' + itoa(i + 1));

  DEH_AddString(@deh_strings, @pg_CREDIT, 'PAGE_CREDIT');
  DEH_AddString(@deh_strings, @pg_HELP, 'PAGE_HELP');
  DEH_AddString(@deh_strings, @pg_HELP1, 'PAGE_HELP1');
  DEH_AddString(@deh_strings, @pg_HELP2, 'PAGE_HELP2');
  DEH_AddString(@deh_strings, @pg_HELP3, 'PAGE_HELP3');
  DEH_AddString(@deh_strings, @pg_VICTORY2, 'PAGE_VICTORY2');
  DEH_AddString(@deh_strings, @pg_ENDPIC, 'PAGE_ENDPIC');
  DEH_AddString(@deh_strings, @pg_TITLE, 'PAGE_TITLE');

  DEH_AddString(@deh_strings, @bgcastcall, 'BGCASTCALL');
  DEH_AddString(@deh_strings, @EndLumpName, 'END_TEXT_LUMP');
  for i := 0 to 9 do
  begin
    DEH_AddString(@deh_strings, @chat_macros[i], 'HUSTR_CHATMACR' + itoa(i));
    DEH_AddString(@deh_strings, @chat_macros[i], 'HUSTR_CHATMACR' + IntToStrZFill(2, i));
  end;

  DEH_AddString(@deh_strings, @QUITMSG, 'QUITMSG');
  for i := 0 to 9 do
  begin
    DEH_AddString(@deh_strings, @endmsg[i], 'ENDMSG' + itoa(i));
    DEH_AddString(@deh_strings, @endmsg[i], 'ENDMSG' + IntToStrZFill(2, i));
  end;

  for i := 10 to NUM_QUITMESSAGES do
  begin
    DEH_AddString(@deh_strings, @endmsg[i], 'ENDMSG' + itoa(i));
  end;

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

  DEH_AddString(@deh_strings, @HUSTR_PLAYER1, 'HUSTR_PLAYER1');
  DEH_AddString(@deh_strings, @HUSTR_PLAYER2, 'HUSTR_PLAYER2');
  DEH_AddString(@deh_strings, @HUSTR_PLAYER3, 'HUSTR_PLAYER3');
  DEH_AddString(@deh_strings, @HUSTR_PLAYER4, 'HUSTR_PLAYER4');
  DEH_AddString(@deh_strings, @HUSTR_PLAYER5, 'HUSTR_PLAYER5');
  DEH_AddString(@deh_strings, @HUSTR_PLAYER6, 'HUSTR_PLAYER6');
  DEH_AddString(@deh_strings, @HUSTR_PLAYER7, 'HUSTR_PLAYER7');
  DEH_AddString(@deh_strings, @HUSTR_PLAYER8, 'HUSTR_PLAYER8');
  DEH_AddString(@deh_strings, @key_multi_msgplayer[0], 'key_multi_msgplayer[0]');
  DEH_AddString(@deh_strings, @key_multi_msgplayer[1], 'key_multi_msgplayer[1]');
  DEH_AddString(@deh_strings, @key_multi_msgplayer[2], 'key_multi_msgplayer[2]');
  DEH_AddString(@deh_strings, @key_multi_msgplayer[3], 'key_multi_msgplayer[3]');
  DEH_AddString(@deh_strings, @key_multi_msgplayer[4], 'key_multi_msgplayer[4]');
  DEH_AddString(@deh_strings, @key_multi_msgplayer[5], 'key_multi_msgplayer[5]');
  DEH_AddString(@deh_strings, @key_multi_msgplayer[6], 'key_multi_msgplayer[6]');
  DEH_AddString(@deh_strings, @key_multi_msgplayer[7], 'key_multi_msgplayer[7]');

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
  DEH_AddString(@deh_strings, @STSTR_DQDON, 'STSTR_DQDON');
  DEH_AddString(@deh_strings, @STSTR_DQDOFF, 'STSTR_DQDOFF');
  DEH_AddString(@deh_strings, @STSTR_KEYSADDED, 'STSTR_KEYSADDED');
  DEH_AddString(@deh_strings, @STSTR_KFAADDED, 'STSTR_KFAADDED');
  DEH_AddString(@deh_strings, @STSTR_FAADDED, 'STSTR_FAADDED');
  DEH_AddString(@deh_strings, @STSTR_NCON, 'STSTR_NCON');
  DEH_AddString(@deh_strings, @STSTR_NCOFF, 'STSTR_NCOFF');
  DEH_AddString(@deh_strings, @STSTR_BEHOLD, 'STSTR_BEHOLD');
  DEH_AddString(@deh_strings, @STSTR_BEHOLDX, 'STSTR_BEHOLDX');
  DEH_AddString(@deh_strings, @STSTR_CHOPPERS, 'STSTR_CHOPPERS');
  DEH_AddString(@deh_strings, @STSTR_CLEV, 'STSTR_CLEV');
  DEH_AddString(@deh_strings, @STSTR_LGON, 'STSTR_LGON');
  DEH_AddString(@deh_strings, @STSTR_LGOFF, 'STSTR_LGOFF');

  DEH_AddString(@deh_strings, @STSTR_MASSACRE, 'STSTR_MASSACRE');

  DEH_AddString(@deh_strings, @MSG_MODIFIEDGAME, 'MSG_MODIFIEDGAME');
  DEH_AddString(@deh_strings, @MSG_SHAREWARE, 'MSG_SHAREWARE');
  DEH_AddString(@deh_strings, @MSG_COMMERCIAL, 'MSG_COMMERCIAL');
  DEH_AddString(@deh_strings, @MSG_UNDETERMINED, 'MSG_UNDETERMINED');

  DEH_AddString(@deh_strings, @GOTARMOR, 'GOTARMOR');
  DEH_AddString(@deh_strings, @GOTMEGA, 'GOTMEGA');
  DEH_AddString(@deh_strings, @GOTHTHBONUS, 'GOTHTHBONUS');
  DEH_AddString(@deh_strings, @GOTARMBONUS, 'GOTARMBONUS');
  DEH_AddString(@deh_strings, @GOTSTIM, 'GOTSTIM');
  DEH_AddString(@deh_strings, @GOTMEDINEED, 'GOTMEDINEED');
  DEH_AddString(@deh_strings, @GOTMEDIKIT, 'GOTMEDIKIT');
  DEH_AddString(@deh_strings, @GOTSUPER, 'GOTSUPER');
  DEH_AddString(@deh_strings, @GOTBLUECARD, 'GOTBLUECARD');
  DEH_AddString(@deh_strings, @GOTYELWCARD, 'GOTYELWCARD');
  DEH_AddString(@deh_strings, @GOTREDCARD, 'GOTREDCARD');
  DEH_AddString(@deh_strings, @GOTBLUESKUL, 'GOTBLUESKUL');
  DEH_AddString(@deh_strings, @GOTYELWSKUL, 'GOTYELWSKUL');
  DEH_AddString(@deh_strings, @GOTREDSKULL, 'GOTREDSKULL');
  DEH_AddString(@deh_strings, @GOTINVUL, 'GOTINVUL');
  DEH_AddString(@deh_strings, @GOTBERSERK, 'GOTBERSERK');
  DEH_AddString(@deh_strings, @GOTINVIS, 'GOTINVIS');
  DEH_AddString(@deh_strings, @GOTSUIT, 'GOTSUIT');
  DEH_AddString(@deh_strings, @GOTMAP, 'GOTMAP');
  DEH_AddString(@deh_strings, @GOTVISOR, 'GOTVISOR');
  DEH_AddString(@deh_strings, @GOTMSPHERE, 'GOTMSPHERE');
  DEH_AddString(@deh_strings, @GOTCLIP, 'GOTCLIP');
  DEH_AddString(@deh_strings, @GOTCLIPBOX, 'GOTCLIPBOX');
  DEH_AddString(@deh_strings, @GOTROCKET, 'GOTROCKET');
  DEH_AddString(@deh_strings, @GOTROCKBOX, 'GOTROCKBOX');
  DEH_AddString(@deh_strings, @GOTCELL, 'GOTCELL');
  DEH_AddString(@deh_strings, @GOTCELLBOX, 'GOTCELLBOX');
  DEH_AddString(@deh_strings, @GOTSHELLS, 'GOTSHELLS');
  DEH_AddString(@deh_strings, @GOTSHELLBOX, 'GOTSHELLBOX');
  DEH_AddString(@deh_strings, @GOTBACKPACK, 'GOTBACKPACK');
  DEH_AddString(@deh_strings, @GOTBFG9000, 'GOTBFG9000');
  DEH_AddString(@deh_strings, @GOTCHAINGUN, 'GOTCHAINGUN');
  DEH_AddString(@deh_strings, @GOTCHAINSAW, 'GOTCHAINSAW');
  DEH_AddString(@deh_strings, @GOTLAUNCHER, 'GOTLAUNCHER');
  DEH_AddString(@deh_strings, @GOTPLASMA, 'GOTPLASMA');
  DEH_AddString(@deh_strings, @GOTSHOTGUN, 'GOTSHOTGUN');
  DEH_AddString(@deh_strings, @GOTSHOTGUN2, 'GOTSHOTGUN2');
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

  DEH_AddString(@deh_strings, @PD_ANY, 'PD_ANY');
  DEH_AddString(@deh_strings, @PD_ALL6, 'PD_ALL6');

  DEH_AddString(@deh_strings, @GGSAVED, 'GGSAVED');
  DEH_AddString(@deh_strings, @SAVEGAMENAME, 'SAVEGAMENAME');


  ammo_tokens := TDTextList.Create;

  ammo_tokens.Add('MAX AMMO');
  ammo_tokens.Add('PER AMMO');


  weapon_tokens := TDTextList.Create;

  weapon_tokens.Add('AMMO TYPE');     // .ammo
  weapon_tokens.Add('DESELECT FRAME');// .upstate
  weapon_tokens.Add('SELECT FRAME');  // .downstate
  weapon_tokens.Add('BOBBING FRAME'); // .readystate
  weapon_tokens.Add('SHOOTING FRAME');// .atkstate
  weapon_tokens.Add('FIRING FRAME');  // .flashstate
  weapon_tokens.Add('HOLD SHOOTING FRAME');// .holdatkstate


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
var
  i: integer;
begin
  if not deh_initialized then
    exit;

  FreeAndNil(mobj_tokens);
  FreeAndNil(mobj_flags);
  FreeAndNil(mobj_flags_ex);
  FreeAndNil(mobj_flags2_ex);
  FreeAndNil(mobj_flags3_ex);
  FreeAndNil(mobj_flags4_ex);
  FreeAndNil(state_flags_ex);
  FreeAndNil(state_tokens);
  FreeAndNil(ammo_tokens);
  FreeAndNil(weapon_tokens);
  FreeAndNil(sound_tokens);
  FreeAndNil(renderstyle_tokens);
  FreeAndNil(misc_tokens);
  FreeAndNil(weapontype_tokens);
  FreeAndNil(ammotype_tokens);

  FreeAndNil(mobj_tokens_hash);
  FreeAndNil(mobj_flags_hash);
  FreeAndNil(mobj_flags_ex_hash);
  FreeAndNil(mobj_flags2_ex_hash);
  FreeAndNil(mobj_flags3_ex_hash);
  FreeAndNil(mobj_flags4_ex_hash);

  DEH_ShutDownActionsHash;

  realloc(pointer(deh_strings._array), deh_strings.realnumstrings * SizeOf(deh_string_t), 0);

  for i := 0 to 255 do
    stringhash[i].Free;
end;

function Deh_ActionName(action: actionf_t): string;
var
  i: integer;
begin
  for i := 0 to DEHNUMACTIONS - 1 do
  begin
    if @deh_actions[i].action.acp1 = @action.acp1 then
    begin
      result := deh_actions[i].name;
      Exit;
    end;
  end;
  result := '';
end;

// JVAL: 20190919 working
function DEH_GetString(const s: string): string;
var
  i: integer;
  lhash: byte;
  num: integer;
begin
  lhash := loselose(s);
  for i := 0 to stringhash[lhash].Count - 1 do
  begin
    num := stringhash[lhash].Numbers[i];
    if deh_strings._array[num].original = s then
    begin
      result := deh_strings._array[num].pstr^;
      exit;
    end;
  end;

  result := s;
end;

end.

