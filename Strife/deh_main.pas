//------------------------------------------------------------------------------
//
//  DelphiStrife: A modified and improved Strife source port for Windows.
//
//  Based on:
//    - Linux Doom by "id Software"
//    - Chocolate Strife by "Simon Howard"
//    - DelphiDoom by "Jim Valavanis"
//
//  Copyright (C) 2004-2018 by Jim Valavanis
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
//  E-Mail: jimmyvalavanis@yahoo.gr
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

function DEH_ParseLumpName(const lumpname: string): boolean;

procedure DEH_ParseLumpNum(const lump: integer);

procedure DEH_ParseFile(const filename: string);

procedure DEH_Parse(const deh_tx: string); overload;

procedure DEH_Parse(const s: TDStringList); overload;

function DEH_CurrentSettings: TDStringList;

procedure DEH_Init;

procedure DEH_ShutDown;

const
  DEHNUMACTIONS = 272;

type
  deh_action_t = record
    action: actionf_t;
    name: string;
  end;

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
  mobj_flags_ex: TDTextList;
  mobj_flags2_ex: TDTextList;
  state_tokens: TDTextList;
  ammo_tokens: TDTextList;
  weapon_tokens: TDTextList;
  sound_tokens: TDTextList;
  renderstyle_tokens: TDTextList;
  misc_tokens: TDTextList;

  deh_actions: array[0..DEHNUMACTIONS - 1] of deh_action_t;
  deh_strings: deh_strings_t;

function deh_actionname(action: actionf_t): string;

function DEH_GetString(const s: string): string;

implementation

uses
  c_cmds,
  doomdef,
  d_main,
  d_items,
  d_englsh,
  dstrings,
{$IFNDEF OPENGL}
  e_endoom,
{$ENDIF}
  f_finale,
  g_game,
  hu_stuff,
  i_system,
  info_h,
  info,
  m_argv,
  p_mobj,
  p_mobj_h,
  p_enemy,
  p_extra,
  p_common,
  p_pspr,
  p_inter,
  ps_main,
  sounds,
  sc_params,
  v_data,
  w_wad,
  w_pak;

function DEH_NextLine(const s: TDStringList; var str: string; var counter: integer; const skipblanc: boolean = true): boolean;
var
  trimmed: string;
begin
  result := counter < s.Count;
  if not result then
    exit;

  str := s[counter];
  trimmed := strtrim(str);
  if skipblanc then
    str := trimmed;
  inc(counter);
  if skipblanc and (str = '') then
  begin
    result := DEH_NextLine(s, str, counter);
    exit;
  end;
  if Pos('#', trimmed) = 1 then
  begin
    result := DEH_NextLine(s, str, counter);
    exit;
  end;
  if Pos('//', trimmed) = 1 then // JVAL: Allow "//" as comments also
  begin
    result := DEH_NextLine(s, str, counter);
    exit;
  end;
  str := strupper(str);
end;

procedure DEH_Parse(const deh_tx: string);
var
  s: TDStringList;
begin
  s := TDStringList.Create;
  try
    s.Text := deh_tx;
    DEH_Parse(s);
  finally
    s.Free;
  end;
end;

function DEH_ParseLumpName(const lumpname: string): boolean;
var
  lump: integer;
begin
  lump := W_CheckNumForName(lumpname);
  if lump >= 0 then
  begin
    DEH_ParseLumpNum(lump);
    result := true;
  end
  else
    result := false;
end;

procedure DEH_ParseLumpNum(const lump: integer);
begin
  if lump < 0 then
    exit;

  DEH_Parse(W_TextLumpNum(lump));
end;

procedure DEH_ParseFile(const filename: string);
var
  fname: string;
  fnames: TDStringList;
  s: TDStringList;
  i: integer;
  done: boolean;
  strm: TPakStream;
begin
  if filename = '' then
  begin
    I_Warning('DEH_ParseFile(): Please specify the dehacked file to parse'#13#10);
    exit;
  end;

  fnames := TDStringList.Create;
  s := TDStringList.Create;
  try
    if Pos('.', filename) = 0 then
    begin
      fnames.Add('%s.%s', [filename, 'deh']);
      fnames.Add('%s.%s', [filename, 'bex']);
    end
    else
      fnames.Add(filename);

    for i := fnames.Count - 1 downto 0 do
      if M_SaveFileName(fnames[i]) <> fnames[i] then
        fnames.Add(M_SaveFileName(fnames[i]));

    fname := '';
    done := false;
    for i := 0 to fnames.Count - 1 do
    begin
      strm := TPakStream.Create(fnames[i], pm_short);
      strm.OnBeginBusy := I_BeginDiskBusy;
      if strm.IOResult = 0 then
        done := s.LoadFromStream(strm) and (strm.IOResult = 0);
      strm.Free;
      if done then
      begin
        fname := fnames[i];
        break;
      end;
    end;

    if not done then
    begin
      I_Warning('DEH_ParseFile(): %s not found'#13#10, [filename]);
      exit;
    end;

    printf(' parsing file %s'#13#10, [fname]);
    DEH_Parse(s);

  finally
    s.Free;
    fnames.Free;
  end;
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
  deh_strings._array[deh_strings.numstrings].name := name;
  inc(deh_strings.numstrings);
end;

function DEH_StringToCString(const s: string): string;
var
  i, len: integer;
begin
  result := '';
  if s = '' then
  begin
    exit;
  end;

  len := Length(s);
  for i := 1 to len do
  begin
    if s[i] <> #13 then
    begin
      if s[i] = #10 then
        result := result + '\n'
      else
        result := result + s[i];
    end;
  end;
end;

function DEH_CStringToString(const s: string): string;
var
  i, len: integer;
begin
  result := '';
  if s = '' then
  begin
    exit;
  end;

  result := s[1];

  len := Length(s);
  for i := 2 to len do
  begin
    if s[i] = 'N' then
    begin
      if (Length(result) > 0) and (result[Length(result)] = '\') then
      begin
        result[Length(result)] := #13;
        result := result + #10
      end
      else
        result := result + 'N'
    end
    else if s[i] = 'R' then
    begin
      if (Length(result) > 0) and (result[Length(result)] = '\') then
        SetLength(result, Length(result) - 1)
      else
        result := result + 'R';
    end
    else if s[i] = #10 then
    begin
      if (Length(result) > 0) and (result[Length(result)] <> #13) then
        result := result + #13#10
      else
        result := result + #10;
    end
    else
      result := result + s[i];
  end;
end;

function DEH_StringValue(const s: string): string;
var
  i: integer;
begin
  result := '';
  for i := 1 to Length(s) do
  begin
    if toupper(s[i]) in ['A'..'Z', '1'..'9', '0'] then
      result := result + toupper(s[i]);
  end;
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
  settext: string;
  mustnextline: boolean;

  mobj_no: integer;
  mobj_idx: integer;
  mobj_val: integer;

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
  foundaction: Boolean;

  deh_initialstates: integer;
begin
  if not deh_initialized then
    DEH_Init;

  deh_initialstates := numstates;

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
      stmp := firstword(token2);

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

        if (mobj_no <= 0) or (mobj_no > Ord(DO_NUMMOBJTYPES)) then
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
        mobj_idx := mobj_tokens.IndexOf(token1);

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
                    splitstring(token2, token3, token4, ['|', ',', '+']);
                    mobj_flag := mobj_flags.IndexOf('MF_' + token3);
                    if mobj_flag = -1 then
                      mobj_flag := mobj_flags.IndexOf(token3);
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
                    token2 := token4;
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
                    splitstring(token2, token3, token4, ['|', ',', '+']);
                    mobj_flag := mobj_flags_ex.IndexOf('MF_EX_' + token3);
                    if mobj_flag = -1 then
                      mobj_flag := mobj_flags_ex.IndexOf('MF_' + token3);
                    if mobj_flag = -1 then
                      mobj_flag := mobj_flags_ex.IndexOf(token3);
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
                    mobjinfo[mobj_no].flags_ex := mobj_setflag;

                end;
              end;
          25: mobjinfo[mobj_no].customsound1 := S_GetSoundNumForName(token2); // DelphiDoom specific
          26: mobjinfo[mobj_no].customsound2 := S_GetSoundNumForName(token2); // DelphiDoom specific
          27: mobjinfo[mobj_no].customsound3 := S_GetSoundNumForName(token2); // DelphiDoom specific
          28: mobjinfo[mobj_no].explosiondamage := mobj_val;
          29: mobjinfo[mobj_no].explosionradius := mobj_val;
          30: mobjinfo[mobj_no].meleedamage := mobj_val;
          31: mobjinfo[mobj_no].inheritsfrom := Info_GetMobjNumForName(token2);
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
                  mobj_val := renderstyle_tokens.IndexOf(token2);
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
                    splitstring(token2, token3, token4, ['|', ',', '+']);
                    mobj_flag := mobj_flags2_ex.IndexOf('MF2_EX_' + token3);
                    if mobj_flag = -1 then
                      mobj_flag := mobj_flags2_ex.IndexOf('MF_EX_' + token3);
                    if mobj_flag = -1 then
                      mobj_flag := mobj_flags2_ex.IndexOf('MF_' + token3);
                    if mobj_flag = -1 then
                      mobj_flag := mobj_flags2_ex.IndexOf(token3);
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
        if (state_no < 0) or (state_no >= Ord(DO_NUMSTATES)) then
        begin
          I_Warning('DEH_Parse(): Wrong state number = %s'#13#10, [stmp]);
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
              if (firstword(token2) = 'ORIGINALSTATE') or (firstword(token2) = 'ORIGINALFRAME') or (firstword(token2) = 'ORIGINAL') then
              begin
                state_val := atoi(secondword(token2));
                states[state_no].nextstate := statenum_t(state_val);
              end
              else if (firstword(token2) = 'NEWSTATE') or (firstword(token2) = 'NEWFRAME') or (firstword(token2) = 'NEW') then
              begin
                state_val := atoi(secondword(token2));
                states[state_no].nextstate := statenum_t(deh_initialstates + state_val)
              end
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
                for j := 0 to DEHNUMACTIONS - 1 do
                  if (token3 = deh_actions[j].name) or (token3 = 'A_' + deh_actions[j].name) then
                  begin
                    states[state_no].action.acp1 := deh_actions[j].action.acp1;
                    foundaction := True;
                    break;
                  end;
                if foundaction then
                begin
                  if token4 <> '' then
                    states[state_no].params := TCustomParamList.CreateFromText(token4);
                end
                else
                  I_Warning('DEH_Parse(): Unknown action function = "%s" in state %d'#13#10, [token3, state_no]);
              end;
            end;
           5: states[state_no].misc1 := state_val;
           6: states[state_no].misc2 := state_val;
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
        for j := 1 to Ord(NUMMUSIC) - 1 do // First music is dummy
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
      end;

      if not foundtext then
        I_Warning('DEH_Parse(): Can not find setable text "%s"'#13#10, [settext]);

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
        states[state_no].action.acp1 := states[state_val].action.acp1;
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

        weapon_val := atoi(token2);

        case weapon_idx of
           0: weaponinfo[weapon_no].ammo := ammotype_t(weapon_val);
           1: weaponinfo[weapon_no].upstate := weapon_val;
           2: weaponinfo[weapon_no].downstate := weapon_val;
           3: weaponinfo[weapon_no].readystate := weapon_val;
           4: weaponinfo[weapon_no].atkstate := weapon_val;
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
          for j := 0 to DEHNUMACTIONS - 1 do
            if (token4 = deh_actions[j].name) or (token4 = 'A_' + deh_actions[j].name) then
            begin
              states[state_no].action.acp1 := deh_actions[j].action.acp1;
              break;
            end;
          if token5 <> '' then
            states[state_no].params := TCustomParamList.CreateFromText(token5);
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
        if (music_idx >= 1) and (music_idx < Ord(NUMMUSIC)) then
          S_music[music_idx].name := token2
        else
        begin
          for j := 1 to Ord(NUMMUSIC) - 1 do // First music is dummy
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
    ////////////////////////////////////////////////////////////////////////////
    // Resetting internal states counter ///////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////////
      deh_initialstates := numstates;
    end;

  end;

end;

function DEH_CurrentSettings: TDStringList;
var
  i, j: integer;
  str: string;
begin
  if not deh_initialized then
    DEH_Init;

  result := TDStringList.Create;
  result.Add('Patch File for DeHackEd');
  result.Add('# Created with %s, %s', [D_Version, D_VersionBuilt]);
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
      result.Add('# The following fields are DelphiDoom specific enhancements');
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
    result.Add('Frame %d', [i]);
    result.Add('%s = %d', [capitalizedstring(state_tokens[0]), Ord(states[i].sprite)]);
    result.Add('%s = %d', [capitalizedstring(state_tokens[1]), states[i].frame]);
    result.Add('%s = %d', [capitalizedstring(state_tokens[2]), states[i].tics]);
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
        for j := 0 to states[i].params.Count - 1 do
          str := str + ' "' + states[i].params.StrVal[j] + '"';  // Add the parameter list
      result.Add('%s = %s', [capitalizedstring(state_tokens[4]), str]);
    end;

    result.Add('%s = %d', [capitalizedstring(state_tokens[5]), states[i].misc1]);
    result.Add('%s = %d', [capitalizedstring(state_tokens[6]), states[i].misc2]);

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
    result.Add('Weapon %d', [i]);

    result.Add('%s = %d', [capitalizedstring(weapon_tokens[0]), Ord(weaponinfo[i].ammo)]);
    result.Add('%s = %d', [capitalizedstring(weapon_tokens[1]), weaponinfo[i].upstate]);
    result.Add('%s = %d', [capitalizedstring(weapon_tokens[2]), weaponinfo[i].downstate]);
    result.Add('%s = %d', [capitalizedstring(weapon_tokens[3]), weaponinfo[i].readystate]);
    result.Add('%s = %d', [capitalizedstring(weapon_tokens[4]), weaponinfo[i].atkstate]);
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


  result.Add('');
  result.Add('# Strings');
  result.Add('');
  //////////////////////////////////////////////////////////////////////////////
  // Add strings
  //////////////////////////////////////////////////////////////////////////////
  result.Add('[Strings]');
  for i := 0 to deh_strings.numstrings - 1 do
    result.Add('%s = %s', [deh_strings._array[i].name, DEH_StringToCString(deh_strings._array[i].pstr^)]);
  result.Add('');


  result.Add('');
  result.Add('# Music');
  result.Add('');
  //////////////////////////////////////////////////////////////////////////////
  // Add strings
  //////////////////////////////////////////////////////////////////////////////
  result.Add('[MUSIC]');
  for i := 1 to Ord(NUMMUSIC) - 1 do
  begin
    if S_music[i].mapname <> '' then
      result.Add('%d = %s %s(Map: %s)', [i, S_music[i].name, StringOfChar(' ', 6 - Length(S_music[i].name)), S_music[i].mapname])
    else
      result.Add('%d = %s', [i, S_music[i].name]);
  end;
  result.Add('');


  result.Add('');
  result.Add('# Sounds');
  result.Add('');
  //////////////////////////////////////////////////////////////////////////////
  // Add strings
  //////////////////////////////////////////////////////////////////////////////
  result.Add('[SOUND]');
  for i := 1 to numsfx - 1 do
    result.Add('%d = %s', [i, S_sfx[i].name]);
  result.Add('');


  result.Add(StringOfChar('#', 80));
  result.Add('# End of file');
  result.Add(StringOfChar('#', 80));
end;

procedure DEH_PrintCurrentSettings;
var
  s: TDSTringList;
  i: integer;
begin
  s := DEH_CurrentSettings;
  try
    for i := 0 to s.Count - 1 do
      printf('%s'#13#10, [s[i]]);
  finally
    s.Free;
  end;
end;

procedure DEH_SaveCurrentSettings(const fname: string);
var
  s: TDSTringList;
  fname1: string;
begin
  if fname = '' then
  begin
    printf('Please specify the filename to save current DEHACKED settings'#13#10);
    exit;
  end;

  if Pos('.', fname) = 0 then
    fname1 := fname + '.bex'
  else
    fname1 := fname;

  fname1 := M_SaveFileName(fname1);

  s := DEH_CurrentSettings;
  try
    s.SaveToFile(fname1);
    printf('DEHACKED settings saved to %s'#13#10, [fname1]);
  finally
    s.Free;
  end;
end;

procedure DEH_PrintActions;
var
  i: integer;
begin
  for i := 0 to DEHNUMACTIONS - 1 do
    printf('A_%s'#13#10, [deh_actions[i].name]);
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
  mobj_tokens.Add('MISSILEHEIGHT');      // .interactstate (DelphiDoom) // 42
  mobj_tokens.Add('NAME2');              // .name2 (Strife)           // 43


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
  // JVAL: VERSION204
  mobj_flags2_ex.Add('MF2_EX_CANTLEAVEFLOORPIC');
  mobj_flags2_ex.Add('MF2_EX_JUMPDOWN');
  mobj_flags2_ex.Add('MF2_EX_FULLVOLACTIVE');
  mobj_flags2_ex.Add('MF2_EX_FULLVOLDEATH');
  mobj_flags2_ex.Add('MF2_EX_FULLVOLSEE');
  mobj_flags2_ex.Add('MF2_EX_FULLVOLPAIN');
  mobj_flags2_ex.Add('MF2_EX_FULLVOLATTACK');
  mobj_flags2_ex.Add('MF2_EX_DONOTRENDERSHADOW');

  state_tokens := TDTextList.Create;
  state_tokens.Add('SPRITE NUMBER');    // .sprite
  state_tokens.Add('SPRITE SUBNUMBER'); // .frame
  state_tokens.Add('DURATION');         // .tics
  state_tokens.Add('NEXT FRAME');       // .nextstate
  state_tokens.Add('CODEP FRAME');      // .action
  state_tokens.Add('UNKNOWN 1');        // .misc1
  state_tokens.Add('UNKNOWN 2');        // .misc2
  state_tokens.Add('FLAGS_EX');         // JVAL: unused since in strife we do not have flags_ex field in state_t

  deh_actions[0].action.acp1 := nil;
  deh_actions[0].name := 'NULL';
  deh_actions[1].action.acp1 := @A_Light0;
  deh_actions[1].name := strupper('Light0');
  deh_actions[2].action.acp1 := @A_WeaponReady;
  deh_actions[2].name := strupper('WeaponReady');
  deh_actions[3].action.acp1 := @A_Lower;
  deh_actions[3].name := strupper('Lower');
  deh_actions[4].action.acp1 := @A_Raise;
  deh_actions[4].name := strupper('Raise');
  deh_actions[5].action.acp1 := @A_Punch;
  deh_actions[5].name := strupper('Punch');
  deh_actions[6].action.acp1 := @A_ReFire;
  deh_actions[6].name := strupper('ReFire');
  deh_actions[7].action.acp1 := @A_Light1;
  deh_actions[7].name := strupper('Light1');
  deh_actions[8].action.acp1 := @A_Light2;
  deh_actions[8].name := strupper('Light2');
  deh_actions[9].action.acp1 := @A_CheckReload;
  deh_actions[9].name := strupper('CheckReload');
  deh_actions[10].action.acp1 := @A_GunFlash;
  deh_actions[10].name := strupper('GunFlash');
  deh_actions[11].action.acp1 := @A_FireMissile;
  deh_actions[11].name := strupper('FireMissile');
  deh_actions[12].action.acp1 := @A_Explode;
  deh_actions[12].name := strupper('Explode');
  deh_actions[13].action.acp1 := @A_Pain;
  deh_actions[13].name := strupper('Pain');
  deh_actions[14].action.acp1 := @A_PlayerScream;
  deh_actions[14].name := strupper('PlayerScream');
  deh_actions[15].action.acp1 := @A_Fall;
  deh_actions[15].name := strupper('Fall');
  deh_actions[16].action.acp1 := @A_XScream;
  deh_actions[16].name := strupper('XScream');
  deh_actions[17].action.acp1 := @A_Look;
  deh_actions[17].name := strupper('Look');
  deh_actions[18].action.acp1 := @A_Chase;
  deh_actions[18].name := strupper('Chase');
  deh_actions[19].action.acp1 := @A_FaceTarget;
  deh_actions[19].name := strupper('FaceTarget');
  deh_actions[20].action.acp1 := @A_Scream;
  deh_actions[20].name := strupper('Scream');
  deh_actions[21].action.acp1 := @A_Tracer;
  deh_actions[21].name := strupper('Tracer');
  deh_actions[22].action.acp1 := @A_BossDeath;
  deh_actions[22].name := strupper('BossDeath');
  deh_actions[23].action.acp1 := @A_Listen;
  deh_actions[23].name := strupper('Listen');
  deh_actions[24].action.acp1 := @A_RandomWalk;
  deh_actions[24].name := strupper('RandomWalk');
  deh_actions[25].action.acp1 := @A_FriendLook;
  deh_actions[25].name := strupper('FriendLook');
  deh_actions[26].action.acp1 := @A_Listen;
  deh_actions[26].name := strupper('Listen');
  deh_actions[27].action.acp1 := @A_PeasantPunch;
  deh_actions[27].name := strupper('PeasantPunch');
  deh_actions[28].action.acp1 := @A_ReaverAttack;
  deh_actions[28].name := strupper('ReaverAttack');
  deh_actions[29].action.acp1 := @A_CheckTargetVisible;
  deh_actions[29].name := strupper('CheckTargetVisible');
  deh_actions[30].action.acp1 := @A_SentinelAttack;
  deh_actions[30].name := strupper('SentinelAttack');
  deh_actions[31].action.acp1 := @A_StalkerThink;
  deh_actions[31].name := strupper('StalkerThink');
  deh_actions[32].action.acp1 := @A_StalkerSetLook;
  deh_actions[32].name := strupper('StalkerSetLook');
  deh_actions[33].action.acp1 := @A_StalkerDrop;
  deh_actions[33].name := strupper('StalkerDrop');
  deh_actions[34].action.acp1 := @A_StalkerScratch;
  deh_actions[34].name := strupper('StalkerScratch');
  deh_actions[35].action.acp1 := @A_FloatWeave;
  deh_actions[35].name := strupper('FloatWeave');
  deh_actions[36].action.acp1 := @A_RobotMelee;
  deh_actions[36].name := strupper('RobotMelee');
  deh_actions[37].action.acp1 := @A_TemplarMauler;
  deh_actions[37].name := strupper('TemplarMauler');
  deh_actions[38].action.acp1 := @A_CrusaderAttack;
  deh_actions[38].name := strupper('CrusaderAttack');
  deh_actions[39].action.acp1 := @A_CrusaderLeft;
  deh_actions[39].name := strupper('CrusaderLeft');
  deh_actions[40].action.acp1 := @A_CrusaderRight;
  deh_actions[40].name := strupper('CrusaderRight');
  deh_actions[41].action.acp1 := @A_CheckTargetVisible2;
  deh_actions[41].name := strupper('CheckTargetVisible2');
  deh_actions[42].action.acp1 := @A_InqFlyCheck;
  deh_actions[42].name := strupper('InqFlyCheck');
  deh_actions[43].action.acp1 := @A_InqGrenade;
  deh_actions[43].name := strupper('InqGrenade');
  deh_actions[44].action.acp1 := @A_InqTakeOff;
  deh_actions[44].name := strupper('InqTakeOff');
  deh_actions[45].action.acp1 := @A_InqFly;
  deh_actions[45].name := strupper('InqFly');
  deh_actions[46].action.acp1 := @A_FireSigilWeapon;
  deh_actions[46].name := strupper('FireSigilWeapon');
  deh_actions[47].action.acp1 := @A_ProgrammerAttack;
  deh_actions[47].name := strupper('ProgrammerAttack');
  deh_actions[48].action.acp1 := @A_Sigil_A_Action;
  deh_actions[48].name := strupper('Sigil_A_Action');
  deh_actions[49].action.acp1 := @A_SpectreEAttack;
  deh_actions[49].name := strupper('SpectreEAttack');
  deh_actions[50].action.acp1 := @A_SpectreCAttack;
  deh_actions[50].name := strupper('SpectreCAttack');
  deh_actions[51].action.acp1 := @A_AlertSpectreC;
  deh_actions[51].name := strupper('AlertSpectreC');
  deh_actions[52].action.acp1 := @A_Sigil_E_Action;
  deh_actions[52].name := strupper('Sigil_E_Action');
  deh_actions[53].action.acp1 := @A_SigilTrail;
  deh_actions[53].name := strupper('SigilTrail');
  deh_actions[54].action.acp1 := @A_SpectreDAttack;
  deh_actions[54].name := strupper('SpectreDAttack');
  deh_actions[55].action.acp1 := @A_FireSigilEOffshoot;
  deh_actions[55].name := strupper('FireSigilEOffshoot');
  deh_actions[56].action.acp1 := @A_ShadowOff;
  deh_actions[56].name := strupper('ShadowOff');
  deh_actions[57].action.acp1 := @A_ModifyVisibility;
  deh_actions[57].name := strupper('ModifyVisibility');
  deh_actions[58].action.acp1 := @A_ShadowOn;
  deh_actions[58].name := strupper('ShadowOn');
  deh_actions[59].action.acp1 := @A_SetTLOptions;
  deh_actions[59].name := strupper('SetTLOptions');
  deh_actions[60].action.acp1 := @A_BossMeleeAtk;
  deh_actions[60].name := strupper('BossMeleeAtk');
  deh_actions[61].action.acp1 := @A_BishopAttack;
  deh_actions[61].name := strupper('BishopAttack');
  deh_actions[62].action.acp1 := @A_FireHookShot;
  deh_actions[62].name := strupper('FireHookShot');
  deh_actions[63].action.acp1 := @A_FireChainShot;
  deh_actions[63].name := strupper('FireChainShot');
  deh_actions[64].action.acp1 := @A_MissileSmoke;
  deh_actions[64].name := strupper('MissileSmoke');
  deh_actions[65].action.acp1 := @A_SpawnSparkPuff;
  deh_actions[65].name := strupper('SpawnSparkPuff');
  deh_actions[66].action.acp1 := @A_ProgrammerMelee;
  deh_actions[66].name := strupper('ProgrammerMelee');
  deh_actions[67].action.acp1 := @A_PeasantCrash;
  deh_actions[67].name := strupper('PeasantCrash');
  deh_actions[68].action.acp1 := @A_HideZombie;
  deh_actions[68].name := strupper('HideZombie');
  deh_actions[69].action.acp1 := @A_MerchantPain;
  deh_actions[69].name := strupper('MerchantPain');
  deh_actions[70].action.acp1 := @A_ProgrammerDie;
  deh_actions[70].name := strupper('ProgrammerDie');
  deh_actions[71].action.acp1 := @A_InqTossArm;
  deh_actions[71].name := strupper('InqTossArm');
  deh_actions[72].action.acp1 := @A_SpawnSpectreA;
  deh_actions[72].name := strupper('SpawnSpectreA');
  deh_actions[73].action.acp1 := @A_SpawnSpectreB;
  deh_actions[73].name := strupper('SpawnSpectreB');
  deh_actions[74].action.acp1 := @A_SpawnSpectreC;
  deh_actions[74].name := strupper('SpawnSpectreC');
  deh_actions[75].action.acp1 := @A_SpawnSpectreD;
  deh_actions[75].name := strupper('SpawnSpectreD');
  deh_actions[76].action.acp1 := @A_SpawnSpectreE;
  deh_actions[76].name := strupper('SpawnSpectreE');
  deh_actions[77].action.acp1 := @A_SpawnEntity;
  deh_actions[77].name := strupper('SpawnEntity');
  deh_actions[78].action.acp1 := @A_EntityDeath;
  deh_actions[78].name := strupper('EntityDeath');
  deh_actions[79].action.acp1 := @A_SpawnZombie;
  deh_actions[79].name := strupper('SpawnZombie');
  deh_actions[80].action.acp1 := @A_ZombieInSpecialSector;
  deh_actions[80].name := strupper('ZombieInSpecialSector');
  deh_actions[81].action.acp1 := @A_CrystalExplode;
  deh_actions[81].name := strupper('CrystalExplode');
  deh_actions[82].action.acp1 := @A_QuestMsg;
  deh_actions[82].name := strupper('QuestMsg');
  deh_actions[83].action.acp1 := @A_ExtraLightOff;
  deh_actions[83].name := strupper('ExtraLightOff');
  deh_actions[84].action.acp1 := @A_CrystalRadiusAtk;
  deh_actions[84].name := strupper('CrystalRadiusAtk');
  deh_actions[85].action.acp1 := @A_DeathExplode1;
  deh_actions[85].name := strupper('DeathExplode1');
  deh_actions[86].action.acp1 := @A_DeathExplode2;
  deh_actions[86].name := strupper('DeathExplode2');
  deh_actions[87].action.acp1 := @A_DeathExplode3;
  deh_actions[87].name := strupper('DeathExplode3');
  deh_actions[88].action.acp1 := @A_DeathExplode5;
  deh_actions[88].name := strupper('DeathExplode5');
  deh_actions[89].action.acp1 := @A_RaiseAlarm;
  deh_actions[89].name := strupper('RaiseAlarm');
  deh_actions[90].action.acp1 := @A_MissileTick;
  deh_actions[90].name := strupper('MissileTick');
  deh_actions[91].action.acp1 := @A_SpawnGrenadeFire;
  deh_actions[91].name := strupper('SpawnGrenadeFire');
  deh_actions[92].action.acp1 := @A_NodeChunk;
  deh_actions[92].name := strupper('NodeChunk');
  deh_actions[93].action.acp1 := @A_HeadChunk;
  deh_actions[93].name := strupper('HeadChunk');
  deh_actions[94].action.acp1 := @A_BurnSpread;
  deh_actions[94].name := strupper('BurnSpread');
  deh_actions[95].action.acp1 := @A_AcolyteSpecial;
  deh_actions[95].name := strupper('AcolyteSpecial');
  deh_actions[96].action.acp1 := @A_InqChase;
  deh_actions[96].name := strupper('InqChase');
  deh_actions[97].action.acp1 := @A_StalkerChase;
  deh_actions[97].name := strupper('StalkerChase');
  deh_actions[98].action.acp1 := @A_TeleportBeacon;
  deh_actions[98].name := strupper('TeleportBeacon');
  deh_actions[99].action.acp1 := @A_BodyParts;
  deh_actions[99].name := strupper('BodyParts');
  deh_actions[100].action.acp1 := @A_ClaxonBlare;
  deh_actions[100].name := strupper('ClaxonBlare');
  deh_actions[101].action.acp1 := @A_ActiveSoundSTRF;
  deh_actions[101].name := strupper('ActiveSoundSTRF');
  deh_actions[102].action.acp1 := @A_ClearSoundTarget;
  deh_actions[102].name := strupper('ClearSoundTarget');
  deh_actions[103].action.acp1 := @A_DropBurnFlesh;
  deh_actions[103].name := strupper('DropBurnFlesh');
  deh_actions[104].action.acp1 := @A_FlameDeath;
  deh_actions[104].name := strupper('FlameDeath');
  deh_actions[105].action.acp1 := @A_ClearForceField;
  deh_actions[105].name := strupper('ClearForceField');
  deh_actions[106].action.acp1 := @A_FireFlameThrower;
  deh_actions[106].name := strupper('FireFlameThrower');
  deh_actions[107].action.acp1 := @A_FireMauler2;
  deh_actions[107].name := strupper('FireMauler2');
  deh_actions[108].action.acp1 := @A_FireGrenade;
  deh_actions[108].name := strupper('FireGrenade');
  deh_actions[109].action.acp1 := @A_FireElectricBolt;
  deh_actions[109].name := strupper('FireElectricBolt');
  deh_actions[110].action.acp1 := @A_FirePoisonBolt;
  deh_actions[110].name := strupper('FirePoisonBolt');
  deh_actions[111].action.acp1 := @A_FireRifle;
  deh_actions[111].name := strupper('FireRifle');
  deh_actions[112].action.acp1 := @A_FireMauler1;
  deh_actions[112].name := strupper('FireMauler1');
  deh_actions[113].action.acp1 := @A_SigilSound;
  deh_actions[113].name := strupper('SigilSound');
  deh_actions[114].action.acp1 := @A_FireSigil;
  deh_actions[114].name := strupper('FireSigil');
  deh_actions[115].action.acp1 := @A_GunFlashThinker;
  deh_actions[115].name := strupper('GunFlashThinker');
  deh_actions[116].action.acp1 := @A_SigilShock;
  deh_actions[116].name := strupper('SigilShock');
  deh_actions[117].action.acp1 := @A_TorpedoExplode;
  deh_actions[117].name := strupper('TorpedoExplode');
  deh_actions[118].action.acp1 := @A_MaulerSound;
  deh_actions[118].name := strupper('MaulerSound');
  deh_actions[119].action.acp1 := @A_CustomSound1;
  deh_actions[119].name := strupper('CustomSound1');
  deh_actions[120].action.acp1 := @A_CustomSound2;
  deh_actions[120].name := strupper('CustomSound2');
  deh_actions[121].action.acp1 := @A_CustomSound3;
  deh_actions[121].name := strupper('CustomSound3');
  deh_actions[122].action.acp1 := @A_RandomPainSound;
  deh_actions[122].name := strupper('RandomPainSound');
  deh_actions[123].action.acp1 := @A_RandomSeeSound;
  deh_actions[123].name := strupper('RandomSeeSound');
  deh_actions[124].action.acp1 := @A_RandomAttackSound;
  deh_actions[124].name := strupper('RandomAttackSound');
  deh_actions[125].action.acp1 := @A_RandomDeathSound;
  deh_actions[125].name := strupper('RandomDeathSound');
  deh_actions[126].action.acp1 := @A_RandomActiveSound;
  deh_actions[126].name := strupper('RandomActiveSound');
  deh_actions[127].action.acp1 := @A_RandomCustomSound1;
  deh_actions[127].name := strupper('RandomCustomSound1');
  deh_actions[128].action.acp1 := @A_RandomCustomSound2;
  deh_actions[128].name := strupper('RandomCustomSound2');
  deh_actions[129].action.acp1 := @A_RandomCustomSound3;
  deh_actions[129].name := strupper('RandomCustomSound3');
  deh_actions[130].action.acp1 := @A_RandomCustomSound;
  deh_actions[130].name := strupper('RandomCustomSound');
  deh_actions[131].action.acp1 := @A_Playsound;
  deh_actions[131].name := strupper('Playsound');
  deh_actions[132].action.acp1 := @A_RandomSound;
  deh_actions[132].name := strupper('RandomSound');
  deh_actions[133].action.acp1 := @A_Stop;
  deh_actions[133].name := strupper('Stop');
  deh_actions[134].action.acp1 := @A_Jump;
  deh_actions[134].name := strupper('Jump');
  deh_actions[135].action.acp1 := @A_CustomMissile;
  deh_actions[135].name := strupper('CustomMissile');
  deh_actions[136].action.acp1 := @A_NoGravity;
  deh_actions[136].name := strupper('NoGravity');
  deh_actions[137].action.acp1 := @A_Gravity;
  deh_actions[137].name := strupper('Gravity');
  deh_actions[138].action.acp1 := @A_NoBlocking;
  deh_actions[138].name := strupper('NoBlocking');
  deh_actions[139].action.acp1 := @A_MeleeAttack;
  deh_actions[139].name := strupper('MeleeAttack');
  deh_actions[140].action.acp1 := @A_SpawnItem;
  deh_actions[140].name := strupper('SpawnItem');
  deh_actions[141].action.acp1 := @A_SeekerMissile;
  deh_actions[141].name := strupper('SeekerMissile');
  deh_actions[142].action.acp1 := @A_CStaffMissileSlither;
  deh_actions[142].name := strupper('CStaffMissileSlither');
  deh_actions[143].action.acp1 := @A_SetTranslucent;
  deh_actions[143].name := strupper('SetTranslucent');
  deh_actions[144].action.acp1 := @A_Die;
  deh_actions[144].name := strupper('Die');
  deh_actions[145].action.acp1 := @A_CustomBulletAttack;
  deh_actions[145].name := strupper('CustomBulletAttack');
  deh_actions[146].action.acp1 := @A_FadeOut;
  deh_actions[146].name := strupper('FadeOut');
  deh_actions[147].action.acp1 := @A_FadeIn;
  deh_actions[147].name := strupper('FadeIn');
  deh_actions[148].action.acp1 := @A_MissileAttack;
  deh_actions[148].name := strupper('MissileAttack');
  deh_actions[149].action.acp1 := @A_AdjustSideSpot;
  deh_actions[149].name := strupper('AdjustSideSpot');
  deh_actions[150].action.acp1 := @A_Countdown;
  deh_actions[150].name := strupper('Countdown');
  deh_actions[151].action.acp1 := @A_FastChase;
  deh_actions[151].name := strupper('FastChase');
  deh_actions[152].action.acp1 := @A_LowGravity;
  deh_actions[152].name := strupper('LowGravity');
  deh_actions[153].action.acp1 := @A_ThrustZ;
  deh_actions[153].name := strupper('ThrustZ');
  deh_actions[154].action.acp1 := @A_ThrustXY;
  deh_actions[154].name := strupper('ThrustXY');
  deh_actions[155].action.acp1 := @A_Turn;
  deh_actions[155].name := strupper('Turn');
  deh_actions[156].action.acp1 := @A_JumpIfCloser;
  deh_actions[156].name := strupper('JumpIfCloser');
  deh_actions[157].action.acp1 := @A_JumpIfHealthLower;
  deh_actions[157].name := strupper('JumpIfHealthLower');
  deh_actions[158].action.acp1 := @A_ScreamAndUnblock;
  deh_actions[158].name := strupper('ScreamAndUnblock');
  deh_actions[159].action.acp1 := @A_PlayWeaponsound;
  deh_actions[159].name := strupper('PlayWeaponsound');
  deh_actions[160].action.acp1 := @A_SetInvulnerable;
  deh_actions[160].name := strupper('SetInvulnerable');
  deh_actions[161].action.acp1 := @A_UnSetInvulnerable;
  deh_actions[161].name := strupper('UnSetInvulnerable');
  deh_actions[162].action.acp1 := @A_RandomMeleeSound;
  deh_actions[162].name := strupper('RandomMeleeSound');
  deh_actions[163].action.acp1 := @A_FloatBob;
  deh_actions[163].name := strupper('FloatBob');
  deh_actions[164].action.acp1 := @A_NoFloatBob;
  deh_actions[164].name := strupper('NoFloatBob');
  deh_actions[165].action.acp1 := @A_Missile;
  deh_actions[165].name := strupper('Missile');
  deh_actions[166].action.acp1 := @A_NoMissile;
  deh_actions[166].name := strupper('NoMissile');
  deh_actions[167].action.acp1 := @A_ComboAttack;
  deh_actions[167].name := strupper('ComboAttack');
  deh_actions[168].action.acp1 := @A_BulletAttack;
  deh_actions[168].name := strupper('BulletAttack');
  deh_actions[169].action.acp1 := @A_MediumGravity;
  deh_actions[169].name := strupper('MediumGravity');
  deh_actions[170].action.acp1 := @A_Wander;
  deh_actions[170].name := strupper('Wander');
  deh_actions[171].action.acp1 := @A_FadeOut10;
  deh_actions[171].name := strupper('FadeOut10');
  deh_actions[172].action.acp1 := @A_FadeOut20;
  deh_actions[172].name := strupper('FadeOut20');
  deh_actions[173].action.acp1 := @A_FadeOut30;
  deh_actions[173].name := strupper('FadeOut30');
  deh_actions[174].action.acp1 := @A_FadeIn10;
  deh_actions[174].name := strupper('FadeIn10');
  deh_actions[175].action.acp1 := @A_FadeIn20;
  deh_actions[175].name := strupper('FadeIn20');
  deh_actions[176].action.acp1 := @A_FadeIn30;
  deh_actions[176].name := strupper('FadeIn30');
  deh_actions[177].action.acp1 := @A_SpawnItemEx;
  deh_actions[177].name := strupper('SpawnItemEx');
  deh_actions[178].action.acp1 := @A_RandomMissile;
  deh_actions[178].name := strupper('RandomMissile');
  deh_actions[179].action.acp1 := @A_HideThing;
  deh_actions[179].name := strupper('HideThing');
  deh_actions[180].action.acp1 := @A_UnHideThing;
  deh_actions[180].name := strupper('UnHideThing');
  deh_actions[181].action.acp1 := @A_SpawnDebris;
  deh_actions[181].name := strupper('SpawnDebris');
  deh_actions[182].action.acp1 := @A_Turn5;
  deh_actions[182].name := strupper('Turn5');
  deh_actions[183].action.acp1 := @A_Turn10;
  deh_actions[183].name := strupper('Turn10');
  deh_actions[184].action.acp1 := @A_SpawnSmokeUp;
  deh_actions[184].name := strupper('SpawnSmokeUp');
  deh_actions[185].action.acp1 := @A_SpawnSmokeDown;
  deh_actions[185].name := strupper('SpawnSmokeDown');
  deh_actions[186].action.acp1 := @A_SpawnSmokeHorz;
  deh_actions[186].name := strupper('SpawnSmokeHorz');
  deh_actions[187].action.acp1 := @A_SetInteractive;
  deh_actions[187].name := strupper('SetInteractive');
  deh_actions[188].action.acp1 := @A_UnSetInteractive;
  deh_actions[188].name := strupper('UnSetInteractive');
  deh_actions[189].action.acp1 := @A_SetMonsterInfight;
  deh_actions[189].name := strupper('SetMonsterInfight');
  deh_actions[190].action.acp1 := @A_UnSetMonsterInfight;
  deh_actions[190].name := strupper('UnSetMonsterInfight');
  deh_actions[191].action.acp1 := @P_RemoveMobj;
  deh_actions[191].name := strupper('RemoveSelf');
  deh_actions[192].action.acp1 := @A_NoiseAlert;
  deh_actions[192].name := strupper('NoiseAlert');
  deh_actions[193].action.acp1 := @A_ConsoleCommand;
  deh_actions[193].name := strupper('ConsoleCommand');
  deh_actions[194].action.acp1 := @A_SetCustomParam;
  deh_actions[194].name := strupper('SetCustomParam');
  deh_actions[195].action.acp1 := @A_AddCustomParam;
  deh_actions[195].name := strupper('AddCustomParam');
  deh_actions[196].action.acp1 := @A_SubtractCustomParam;
  deh_actions[196].name := strupper('SubtractCustomParam');
  deh_actions[197].action.acp1 := @A_SetTargetCustomParam;
  deh_actions[197].name := strupper('SetTargetCustomParam');
  deh_actions[198].action.acp1 := @A_AddTargetCustomParam;
  deh_actions[198].name := strupper('AddTargetCustomParam');
  deh_actions[199].action.acp1 := @A_SubtractTargetCustomParam;
  deh_actions[199].name := strupper('SubtractTargetCustomParam');
  deh_actions[200].action.acp1 := @A_JumpIfCustomParam;
  deh_actions[200].name := strupper('JumpIfCustomParam');
  deh_actions[201].action.acp1 := @A_JumpIfCustomParamLess;
  deh_actions[201].name := strupper('JumpIfCustomParamLess');
  deh_actions[202].action.acp1 := @A_JumpIfCustomParamGreater;
  deh_actions[202].name := strupper('JumpIfCustomParamGreater');
  deh_actions[203].action.acp1 := @A_JumpIfTargetCustomParam;
  deh_actions[203].name := strupper('JumpIfTargetCustomParam');
  deh_actions[204].action.acp1 := @A_JumpIfTargetCustomParamLess;
  deh_actions[204].name := strupper('JumpIfTargetCustomParamLess');
  deh_actions[205].action.acp1 := @A_JumpIfTargetCustomParamGreater;
  deh_actions[205].name := strupper('JumpIfTargetCustomParamGreater');
  deh_actions[206].action.acp1 := @A_SetShootable;
  deh_actions[206].name := strupper('SetShootable');
  deh_actions[207].action.acp1 := @A_UnSetShootable;
  deh_actions[207].name := strupper('UnSetShootable');
  deh_actions[208].action.acp1 := @A_PlayerMessage;
  deh_actions[208].name := strupper('PlayerMessage');
  deh_actions[209].action.acp1 := @A_PlayerFaceMe;
  deh_actions[209].name := strupper('PlayerFaceMe');
  deh_actions[210].action.acp1 := @A_GoTo;
  deh_actions[210].name := strupper('GoTo');
  deh_actions[211].action.acp1 := @A_GoToIfCloser;
  deh_actions[211].name := strupper('GoToIfCloser');
  deh_actions[212].action.acp1 := @A_GoToIfHealthLower;
  deh_actions[212].name := strupper('GoToIfHealthLower');
  deh_actions[213].action.acp1 := @A_GoToIfCustomParam;
  deh_actions[213].name := strupper('GoToIfCustomParam');
  deh_actions[214].action.acp1 := @A_GoToIfCustomParamLess;
  deh_actions[214].name := strupper('GoToIfCustomParamLess');
  deh_actions[215].action.acp1 := @A_GoToIfCustomParamGreater;
  deh_actions[215].name := strupper('GoToIfCustomParamGreater');
  deh_actions[216].action.acp1 := @A_GoToIfTargetCustomParam;
  deh_actions[216].name := strupper('GoToIfTargetCustomParam');
  deh_actions[217].action.acp1 := @A_GoToIfTargetCustomParamLess;
  deh_actions[217].name := strupper('GoToIfTargetCustomParamLess');
  deh_actions[218].action.acp1 := @A_GoToIfTargetCustomParamGreater;
  deh_actions[218].name := strupper('GoToIfTargetCustomParamGreater');
  deh_actions[219].action.acp1 := @A_SetFloorClip;
  deh_actions[219].name := strupper('SetFloorClip');
  deh_actions[220].action.acp1 := @A_UnSetFloorClip;
  deh_actions[220].name := strupper('UnSetFloorClip');
  deh_actions[221].action.acp1 := @A_SetFrightened;
  deh_actions[221].name := strupper('SetFrightened');
  deh_actions[222].action.acp1 := @A_UnSetFrightened;
  deh_actions[222].name := strupper('UnSetFrightened');
  deh_actions[223].action.acp1 := @A_SetNoDamage;
  deh_actions[223].name := strupper('SetNoDamage');
  deh_actions[224].action.acp1 := @A_UnSetNoDamage;
  deh_actions[224].name := strupper('UnSetNoDamage');
  deh_actions[225].action.acp1 := @A_RunScript;
  deh_actions[225].name := strupper('RunScript');
  deh_actions[226].action.acp1 := @A_GhostOn;
  deh_actions[226].name := strupper('GhostOn');
  deh_actions[227].action.acp1 := @A_GhostOff;
  deh_actions[227].name := strupper('GhostOff');
  deh_actions[228].action.acp1 := @A_Blocking;
  deh_actions[228].name := strupper('Blocking');
  deh_actions[229].action.acp1 := @A_DoNotRunScripts;
  deh_actions[229].name := strupper('DoNotRunScripts');
  deh_actions[230].action.acp1 := @A_DoRunScripts;
  deh_actions[230].name := strupper('DoRunScripts');
  deh_actions[231].action.acp1 := @A_TargetDropItem;
  deh_actions[231].name := strupper('TargetDropItem');
  deh_actions[232].action.acp1 := @A_DefaultTargetDropItem;
  deh_actions[232].name := strupper('DefaultTargetDropItem');
  deh_actions[233].action.acp1 := @A_SetDropItem;
  deh_actions[233].name := strupper('SetDropItem');
  deh_actions[234].action.acp1 := @A_SetDefaultDropItem;
  deh_actions[234].name := strupper('SetDefaultDropItem');
  deh_actions[235].action.acp1 := @A_GlobalEarthQuake;
  deh_actions[235].name := strupper('GlobalEarthQuake');
  deh_actions[236].action.acp1 := @A_JumpIfMapStringEqual;
  deh_actions[236].name := strupper('JumpIfMapStringEqual');
  deh_actions[237].action.acp1 := @A_JumpIfMapStringLess;
  deh_actions[237].name := strupper('JumpIfMapStringLess');
  deh_actions[238].action.acp1 := @A_JumpIfMapStringGreater;
  deh_actions[238].name := strupper('JumpIfMapStringGreater');
  deh_actions[239].action.acp1 := @A_JumpIfMapIntegerEqual;
  deh_actions[239].name := strupper('JumpIfMapIntegerEqual');
  deh_actions[240].action.acp1 := @A_JumpIfMapIntegerLess;
  deh_actions[240].name := strupper('JumpIfMapIntegerLess');
  deh_actions[241].action.acp1 := @A_JumpIfMapIntegerGreater;
  deh_actions[241].name := strupper('JumpIfMapIntegerGreater');
  deh_actions[242].action.acp1 := @A_JumpIfMapFloatEqual;
  deh_actions[242].name := strupper('JumpIfMapFloatEqual');
  deh_actions[243].action.acp1 := @A_JumpIfMapFloatLess;
  deh_actions[243].name := strupper('JumpIfMapFloatLess');
  deh_actions[244].action.acp1 := @A_JumpIfMapFloatGreater;
  deh_actions[244].name := strupper('JumpIfMapFloatGreater');
  deh_actions[245].action.acp1 := @A_JumpIfWorldStringEqual;
  deh_actions[245].name := strupper('JumpIfWorldStringEqual');
  deh_actions[246].action.acp1 := @A_JumpIfWorldStringLess;
  deh_actions[246].name := strupper('JumpIfWorldStringLess');
  deh_actions[247].action.acp1 := @A_JumpIfWorldStringGreater;
  deh_actions[247].name := strupper('JumpIfWorldStringGreater');
  deh_actions[248].action.acp1 := @A_JumpIfWorldIntegerEqual;
  deh_actions[248].name := strupper('JumpIfWorldIntegerEqual');
  deh_actions[249].action.acp1 := @A_JumpIfWorldIntegerLess;
  deh_actions[249].name := strupper('JumpIfWorldIntegerLess');
  deh_actions[250].action.acp1 := @A_JumpIfWorldIntegerGreater;
  deh_actions[250].name := strupper('JumpIfWorldIntegerGreater');
  deh_actions[251].action.acp1 := @A_JumpIfWorldFloatEqual;
  deh_actions[251].name := strupper('JumpIfWorldFloatEqual');
  deh_actions[252].action.acp1 := @A_JumpIfWorldFloatLess;
  deh_actions[252].name := strupper('JumpIfWorldFloatLess');
  deh_actions[253].action.acp1 := @A_JumpIfWorldFloatGreater;
  deh_actions[253].name := strupper('JumpIfWorldFloatGreater');
  deh_actions[254].action.acp1 := @A_GoToIfMapStringEqual;
  deh_actions[254].name := strupper('GoToIfMapStringEqual');
  deh_actions[255].action.acp1 := @A_GoToIfMapStringLess;
  deh_actions[255].name := strupper('GoToIfMapStringLess');
  deh_actions[256].action.acp1 := @A_GoToIfMapStringGreater;
  deh_actions[256].name := strupper('GoToIfMapStringGreater');
  deh_actions[257].action.acp1 := @A_GoToIfMapIntegerEqual;
  deh_actions[257].name := strupper('GoToIfMapIntegerEqual');
  deh_actions[258].action.acp1 := @A_GoToIfMapIntegerLess;
  deh_actions[258].name := strupper('GoToIfMapIntegerLess');
  deh_actions[259].action.acp1 := @A_GoToIfMapIntegerGreater;
  deh_actions[259].name := strupper('GoToIfMapIntegerGreater');
  deh_actions[260].action.acp1 := @A_GoToIfMapFloatEqual;
  deh_actions[260].name := strupper('GoToIfMapFloatEqual');
  deh_actions[261].action.acp1 := @A_GoToIfMapFloatLess;
  deh_actions[261].name := strupper('GoToIfMapFloatLess');
  deh_actions[262].action.acp1 := @A_GoToIfMapFloatGreater;
  deh_actions[262].name := strupper('GoToIfMapFloatGreater');
  deh_actions[263].action.acp1 := @A_GoToIfWorldStringEqual;
  deh_actions[263].name := strupper('GoToIfWorldStringEqual');
  deh_actions[264].action.acp1 := @A_GoToIfWorldStringLess;
  deh_actions[264].name := strupper('GoToIfWorldStringLess');
  deh_actions[265].action.acp1 := @A_GoToIfWorldStringGreater;
  deh_actions[265].name := strupper('GoToIfWorldStringGreater');
  deh_actions[266].action.acp1 := @A_GoToIfWorldIntegerEqual;
  deh_actions[266].name := strupper('GoToIfWorldIntegerEqual');
  deh_actions[267].action.acp1 := @A_GoToIfWorldIntegerLess;
  deh_actions[267].name := strupper('GoToIfWorldIntegerLess');
  deh_actions[268].action.acp1 := @A_GoToIfWorldIntegerGreater;
  deh_actions[268].name := strupper('GoToIfWorldIntegerGreater');
  deh_actions[269].action.acp1 := @A_GoToIfWorldFloatEqual;
  deh_actions[269].name := strupper('GoToIfWorldFloatEqual');
  deh_actions[270].action.acp1 := @A_GoToIfWorldFloatLess;
  deh_actions[270].name := strupper('GoToIfWorldFloatLess');
  deh_actions[271].action.acp1 := @A_GoToIfWorldFloatGreater;
  deh_actions[271].name := strupper('GoToIfWorldFloatGreater');

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
{$IFNDEF OPENGL}
  DEH_AddString(@deh_strings, @EndLumpName, 'END_TEXT_LUMP');
{$ENDIF}
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


  sound_tokens := TDTextList.Create;

  sound_tokens.Add('ZERO/ONE');
  sound_tokens.Add('VALUE');
  sound_tokens.Add('NAME'); // DelphiDoom specific


  renderstyle_tokens := TDTextList.Create;

  renderstyle_tokens.Add('NORMAL');
  renderstyle_tokens.Add('TRANSLUCENT');
  renderstyle_tokens.Add('ADD');


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
  C_AddCmd('DEH_PrintActions, DEH_ShowActions, BEX_PrintActions, BEX_ShowActions', @DEH_PrintActions);
end;

procedure DEH_ShutDown;
begin
  if not deh_initialized then
    exit;

  FreeAndNil(mobj_tokens);
  FreeAndNil(mobj_flags);
  FreeAndNil(mobj_flags_ex);
  FreeAndNil(mobj_flags2_ex);
  FreeAndNil(state_tokens);
  FreeAndNil(ammo_tokens);
  FreeAndNil(weapon_tokens);
  FreeAndNil(sound_tokens);
  FreeAndNil(renderstyle_tokens);
  FreeAndNil(misc_tokens);

  realloc(pointer(deh_strings._array), deh_strings.realnumstrings * SizeOf(deh_string_t), 0);
end;

function deh_actionname(action: actionf_t): string;
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

// JVAL: stub
function DEH_GetString(const s: string): string;
begin
  result := s;
end;

end.

