//------------------------------------------------------------------------------
//
//  DelphiDoom: A modified and improved DOOM engine for Windows
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 1993-1996 by id Software, Inc.
//  Copyright (C) 2004-2020 by Jim Valavanis
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
//
// DESCRIPTION:
//  ACTORDEF lump parser (Custom defined enemies)
//  SNDINFO lump parser (Sound aliases)
//
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit sc_actordef;

interface

uses
  d_delphi;

procedure SC_ParseSndInfoLumps;

procedure SC_ParseActordefLumps;

procedure SC_ParseActordefLump(const in_text: string);

procedure SC_Init;

procedure SC_ShutDown;

function SC_SoundAlias(const snd: string): string;

implementation

uses
  d_main,
  deh_base,
  deh_main,
  m_fixed,
  i_system,
  {$IFNDEF FPC}
  i_startup,
  {$ENDIF}
  info,
  info_common,
  info_h,
  r_renderstyle,
  rtl_types,
  sc_engine,
  sc_states,
  sc_tokens,
  sc_thinker,
  sc_evaluate_actor,
  sc_utils,
  ps_main,
{$IFDEF HEXEN}
  sounds,
{$ENDIF}
  w_pak,
  w_wad;

var
  soundaliases: TDStringList;

const
  MAXSTATES = 512;

function fixsndaliasstr(const src: string): string;
var
  i: integer;
begin
  result := src;
  for i := 1 to length(result) do
  begin
    if result[i] = '\' then
      result[i] := '/'
    else if result[i] in ['"', '{', '}'] then
      result[i] := ' '
    else
      result[i] := toupper(result[i]);
  end;
end;

function SC_SoundAlias(const snd: string): string;
var
  check: string;
  id: integer;
begin
  if snd = '' then
  begin
    result := '0';
    exit;
  end;

  check := fixsndaliasstr(snd);
  if firstword(check) = '$RANDOM' then
    check := secondword(check);
  id := soundaliases.IndexOfName(check);
  if id >= 0 then
  begin
    result := soundaliases.Values[check];
    if (length(result) > 8) or (Pos('/', result) > 0) then
    begin
      id := soundaliases.IndexOfName(result);
      if id >= 0 then
        result := soundaliases.Values[result];
    end;
    if (length(result) > 8) or (Pos('/', result) > 0) then
    begin
      I_Warning('SC_SoundAlias(): Sound %s has length %d > 8.'#13#10, [result, length(result)]);
      result := '0';
    end;
    exit;
  end;

  if Pos('/', snd) > 0 then
  begin
    I_Warning('SC_SoundAlias(): Sound %s does not have a corresponding alias.'#13#10, [snd]);
    result := '0';
    exit;
  end;

  if length(snd) > 8 then
  begin
    I_Warning('SC_SoundAlias(): Sound %s has length %d > 8.'#13#10, [snd, length(snd)]);
    result := '0';
    exit;
  end;

  result := snd;
end;

type
  TActordefScriptEngine = class(TScriptEngine)
  private
    procedure AddFlagAliases;
  public
    function MatchFlag(const flag: string): boolean;
    {$IFDEF HERETIC_OR_HEXEN}
    function MatchFlag2(const flag: string): boolean;
    {$ENDIF}
    function MatchFlagEx(const flag_ex: string): boolean;
    function MatchFlag2Ex(const flag2_ex: string): boolean;
    function MatchFlag3Ex(const flag3_ex: string): boolean;
    function MatchFlag4Ex(const flag4_ex: string): boolean;
  end;

procedure TActordefScriptEngine.AddFlagAliases;
begin
  AddAlias('CANPASS', 'PASSMOBJ');
  AddAlias('+CANPASS', 'PASSMOBJ');
  AddAlias('-CANPASS', '-PASSMOBJ');
  AddAlias('DONTSPLASH', 'NOHITFLOOR');
  AddAlias('+DONTSPLASH', 'NOHITFLOOR');
  AddAlias('-DONTSPLASH', '-NOHITFLOOR');
end;

function TActordefScriptEngine.MatchFlag(const flag: string): boolean;
begin
  AddFlagAliases;
  result := MatchString(flag) or MatchString('+' + flag) or MatchString('MF_' + flag);
  ClearAliases;
end;

{$IFDEF HERETIC_OR_HEXEN}
function TActordefScriptEngine.MatchFlag2(const flag: string): boolean;
begin
  AddFlagAliases;
  result := MatchString(flag) or MatchString('+' + flag) or MatchString('MF_' + flag) or MatchString('MF2_' + flag);
  ClearAliases;
end;
{$ENDIF}

function TActordefScriptEngine.MatchFlagEx(const flag_ex: string): boolean;
begin
  AddFlagAliases;
  result := MatchString(flag_ex) or MatchString('+' + flag_ex) or MatchString('MF_' + flag_ex) or MatchString('MF_EX_' + flag_ex);
  ClearAliases;
end;

function TActordefScriptEngine.MatchFlag2Ex(const flag2_ex: string): boolean;
begin
  AddFlagAliases;
  result := MatchString(flag2_ex) or MatchString('+' + flag2_ex) or MatchString('MF2_' + flag2_ex) or MatchString('MF2_EX_' + flag2_ex);
  ClearAliases;
end;

function TActordefScriptEngine.MatchFlag3Ex(const flag3_ex: string): boolean;
begin
  AddFlagAliases;
  result := MatchString(flag3_ex) or MatchString('+' + flag3_ex) or MatchString('MF3_' + flag3_ex) or MatchString('MF3_EX_' + flag3_ex);
  ClearAliases;
end;

function TActordefScriptEngine.MatchFlag4Ex(const flag4_ex: string): boolean;
begin
  AddFlagAliases;
  result := MatchString(flag4_ex) or MatchString('+' + flag4_ex) or MatchString('MF4_' + flag4_ex) or MatchString('MF4_EX_' + flag4_ex);
  ClearAliases;
end;

const
  ORIGINALSTATEMARKER = $FFFFF;
  ACTORDEFLUMPNAME = 'ACTORDEF';
  SNDINFOLUMPNAME = 'SNDINFO';

procedure SC_DoParseActordefLump(const in_text: string);
var
  mobj: rtl_mobjinfo_t;
  pinf: Pmobjinfo_t;
  sc: TActordefScriptEngine;
  foundstates: boolean;
  numstates: integer;
  m_states: array[0..MAXSTATES - 1] of rtl_state_t;
  pm_state: Prtl_state_t;
  res: string;
  ismissile: boolean;
  i, idx: integer;
  state_tokens: TDStringList;
  th: rtl_thinker_t;
  passcriptname: string;
  passcriptfile: string;
  passcript: string;
  passcriptline: string;
  actorpending: boolean;

  function MatchFlags: boolean;
  var
    i: integer;
    flag: string;
  begin
    for i := 0 to mobj_flags.Count - 1 do
    begin
      flag := mobj_flags[i];
      if Pos('MF_', flag) = 1 then
        flag := Copy(flag, 4, length(flag) - 3);
      if sc.MatchFlag(flag) then
      begin
        mobj.flags := mobj.flags + flag + ' ';
        result := true;
        exit;
      end;
    end;

    result := false;
  end;

  function RemoveFlag(const inp: string; const aflag: string): string;
  var
    sctmp: TScriptEngine;
    acheck, icheck: string;
  begin
    result := '';
    acheck := strupper(aflag);
    if Pos('MF2_EX_', acheck) = 1 then
      acheck := Copy(acheck, 8, length(acheck) - 7)
    else if Pos('MF_EX_', acheck) = 1 then
      acheck := Copy(acheck, 7, length(acheck) - 6)
    {$IFDEF HERETIC_OR_HEXEN}
    else if Pos('MF2_', acheck) = 1 then
      acheck := Copy(acheck, 5, length(acheck) - 4)
    {$ENDIF}
    else if Pos('MF_', acheck) = 1 then
      acheck := Copy(acheck, 4, length(acheck) - 3);
    sctmp := TScriptEngine.Create(inp);
    while sctmp.GetString do
    begin
      icheck := strupper(sctmp._String);
      if (icheck <> acheck) and
         (icheck <> 'MF_' + acheck) and
         (icheck <> 'MF2_' + acheck) and
         (icheck <> 'MF_EX_' + acheck) and
         (icheck <> 'MF2_EX_' + acheck) then
        result := result + icheck + ' ';
    end;
    sctmp.Free;
  end;

  function MatchFlags_Delete: boolean;
  var
    i: integer;
    flag: string;
    check: string;
  begin
    check := sc._String;
    if Pos('-', check) <> 1 then
    begin
      result := false;
      exit;
    end;
    if Pos('+', check) = 1 then
    begin
      result := false;
      exit;
    end;

    for i := 0 to mobj_flags.Count - 1 do
    begin
      flag := mobj_flags[i];
      if Pos('MF_', flag) = 1 then
        flag := Copy(flag, 4, length(flag) - 3);
      if sc.MatchFlag('-' + flag) then
      begin
        mobj.flags := RemoveFlag(mobj.flags, flag);
        result := true;
        exit;
      end;
    end;

    result := false;
  end;

  {$IFDEF HERETIC_OR_HEXEN}
  function MatchFlags2: boolean;
  var
    i: integer;
    flag: string;
  begin
    for i := 0 to mobj_flags2.Count - 1 do
    begin
      flag := mobj_flags2[i];
      if Pos('MF2_', flag) = 1 then
        flag := Copy(flag, 5, length(flag) - 4)
      else if Pos('MF_', flag) = 1 then
        flag := Copy(flag, 4, length(flag) - 3);
      if sc.MatchFlag2(flag) then
      begin
        mobj.flags2 := mobj.flags2 + flag + ' ';
        result := true;
        exit;
      end;
    end;

    result := false;
  end;

  function MatchFlags2_Delete: boolean;
  var
    i: integer;
    flag: string;
    check: string;
  begin
    check := sc._String;
    if Pos('-', check) <> 1 then
    begin
      result := false;
      exit;
    end;
    if Pos('+', check) = 1 then
    begin
      result := false;
      exit;
    end;

    for i := 0 to mobj_flags2.Count - 1 do
    begin
      flag := mobj_flags2[i];
      if Pos('MF2_', flag) = 1 then
        flag := Copy(flag, 5, length(flag) - 4)
      else if Pos('MF_', flag) = 1 then
        flag := Copy(flag, 4, length(flag) - 3);
      if sc.MatchFlag2('-' + flag) then
      begin
        mobj.flags2 := RemoveFlag(mobj.flags2, flag);
        result := true;
        exit;
      end;
    end;

    result := false;
  end;
  {$ENDIF}

  function MatchFlagsEx: boolean;
  var
    i: integer;
    flag: string;
  begin
    for i := 0 to mobj_flags_ex.Count - 1 do
    begin
      flag := mobj_flags_ex[i];
      if Pos('MF_EX_', flag) = 1 then
        flag := Copy(flag, 7, length(flag) - 6)
      else if Pos('MF_', flag) = 1 then
        flag := Copy(flag, 4, length(flag) - 3);
      if sc.MatchFlagEx(flag) then
      begin
        mobj.flags_ex := mobj.flags_ex + flag + ' ';
        result := true;
        exit;
      end;
    end;

    result := false;
  end;

  function MatchFlagsEx_Delete: boolean;
  var
    i: integer;
    flag: string;
    check: string;
  begin
    check := sc._String;
    if Pos('-', check) <> 1 then
    begin
      result := false;
      exit;
    end;
    if Pos('+', check) = 1 then
    begin
      result := false;
      exit;
    end;

    for i := 0 to mobj_flags_ex.Count - 1 do
    begin
      flag := mobj_flags_ex[i];
      if Pos('MF_EX_', flag) = 1 then
        flag := Copy(flag, 7, length(flag) - 6)
      else if Pos('MF_', flag) = 1 then
        flag := Copy(flag, 4, length(flag) - 3);
      if sc.MatchFlagEx('-' + flag) then
      begin
        mobj.flags_ex := RemoveFlag(mobj.flags_ex, flag);
        result := true;
        exit;
      end;
    end;

    result := false;
  end;

  function MatchFlags2Ex: boolean;
  var
    i: integer;
    flag: string;
  begin
    for i := 0 to mobj_flags2_ex.Count - 1 do
    begin
      flag := mobj_flags2_ex[i];
      if Pos('MF2_EX_', flag) = 1 then
        flag := Copy(flag, 8, length(flag) - 7)
      else if Pos('MF_EX_', flag) = 1 then
        flag := Copy(flag, 7, length(flag) - 6)
      else if Pos('MF2_', flag) = 1 then
        flag := Copy(flag, 5, length(flag) - 4)
      else if Pos('MF_', flag) = 1 then
        flag := Copy(flag, 4, length(flag) - 3);
      if sc.MatchFlag2Ex(flag) then
      begin
        mobj.flags2_ex := mobj.flags2_ex + flag + ' ';
        result := true;
        exit;
      end;
    end;

    result := false;
  end;

  function MatchFlags2Ex_Delete: boolean;
  var
    i: integer;
    flag: string;
    check: string;
  begin
    check := sc._String;
    if Pos('-', check) <> 1 then
    begin
      result := false;
      exit;
    end;
    if Pos('+', check) = 1 then
    begin
      result := false;
      exit;
    end;

    for i := 0 to mobj_flags2_ex.Count - 1 do
    begin
      flag := mobj_flags2_ex[i];
      if Pos('MF2_EX_', flag) = 1 then
        flag := Copy(flag, 8, length(flag) - 7)
      else if Pos('MF_EX_', flag) = 1 then
        flag := Copy(flag, 7, length(flag) - 6)
      else if Pos('MF2_', flag) = 1 then
        flag := Copy(flag, 5, length(flag) - 4)
      else if Pos('MF_', flag) = 1 then
        flag := Copy(flag, 4, length(flag) - 3);
      if sc.MatchFlag2Ex('-' + flag) then
      begin
        mobj.flags2_ex := RemoveFlag(mobj.flags2_ex, flag);
        result := true;
        exit;
      end;
    end;

    result := false;
  end;

  function MatchFlags3Ex: boolean;
  var
    i: integer;
    flag: string;
  begin
    for i := 0 to mobj_flags3_ex.Count - 1 do
    begin
      flag := mobj_flags3_ex[i];
      if Pos('MF3_EX_', flag) = 1 then
        flag := Copy(flag, 8, length(flag) - 7)
      else if Pos('MF2_EX_', flag) = 1 then
        flag := Copy(flag, 8, length(flag) - 7)
      else if Pos('MF_EX_', flag) = 1 then
        flag := Copy(flag, 7, length(flag) - 6)
      else if Pos('MF2_', flag) = 1 then
        flag := Copy(flag, 5, length(flag) - 4)
      else if Pos('MF_', flag) = 1 then
        flag := Copy(flag, 4, length(flag) - 3);
      if sc.MatchFlag3Ex(flag) then
      begin
        mobj.flags3_ex := mobj.flags3_ex + flag + ' ';
        result := true;
        exit;
      end;
    end;

    result := false;
  end;

  function MatchFlags3Ex_Delete: boolean;
  var
    i: integer;
    flag: string;
    check: string;
  begin
    check := sc._String;
    if Pos('-', check) <> 1 then
    begin
      result := false;
      exit;
    end;
    if Pos('+', check) = 1 then
    begin
      result := false;
      exit;
    end;

    for i := 0 to mobj_flags3_ex.Count - 1 do
    begin
      flag := mobj_flags3_ex[i];
      if Pos('MF3_EX_', flag) = 1 then
        flag := Copy(flag, 8, length(flag) - 7)
      else if Pos('MF2_EX_', flag) = 1 then
        flag := Copy(flag, 8, length(flag) - 7)
      else if Pos('MF_EX_', flag) = 1 then
        flag := Copy(flag, 7, length(flag) - 6)
      else if Pos('MF2_', flag) = 1 then
        flag := Copy(flag, 5, length(flag) - 4)
      else if Pos('MF_', flag) = 1 then
        flag := Copy(flag, 4, length(flag) - 3);
      if sc.MatchFlag3Ex('-' + flag) then
      begin
        mobj.flags3_ex := RemoveFlag(mobj.flags3_ex, flag);
        result := true;
        exit;
      end;
    end;

    result := false;
  end;

  function MatchFlags4Ex: boolean;
  var
    i: integer;
    flag: string;
  begin
    for i := 0 to mobj_flags4_ex.Count - 1 do
    begin
      flag := mobj_flags4_ex[i];
      if Pos('MF4_EX_', flag) = 1 then
        flag := Copy(flag, 8, length(flag) - 7)
      else if Pos('MF3_EX_', flag) = 1 then
        flag := Copy(flag, 8, length(flag) - 7)
      else if Pos('MF2_EX_', flag) = 1 then
        flag := Copy(flag, 8, length(flag) - 7)
      else if Pos('MF_EX_', flag) = 1 then
        flag := Copy(flag, 7, length(flag) - 6)
      else if Pos('MF2_', flag) = 1 then
        flag := Copy(flag, 5, length(flag) - 4)
      else if Pos('MF_', flag) = 1 then
        flag := Copy(flag, 4, length(flag) - 3);
      if sc.MatchFlag4Ex(flag) then
      begin
        mobj.flags4_ex := mobj.flags4_ex + flag + ' ';
        result := true;
        exit;
      end;
    end;

    result := false;
  end;

  function MatchFlags4Ex_Delete: boolean;
  var
    i: integer;
    flag: string;
    check: string;
  begin
    check := sc._String;
    if Pos('-', check) <> 1 then
    begin
      result := false;
      exit;
    end;
    if Pos('+', check) = 1 then
    begin
      result := false;
      exit;
    end;

    for i := 0 to mobj_flags4_ex.Count - 1 do
    begin
      flag := mobj_flags3_ex[i];
      if Pos('MF4_EX_', flag) = 1 then
        flag := Copy(flag, 8, length(flag) - 7)
      else if Pos('MF3_EX_', flag) = 1 then
        flag := Copy(flag, 8, length(flag) - 7)
      else if Pos('MF2_EX_', flag) = 1 then
        flag := Copy(flag, 8, length(flag) - 7)
      else if Pos('MF_EX_', flag) = 1 then
        flag := Copy(flag, 7, length(flag) - 6)
      else if Pos('MF2_', flag) = 1 then
        flag := Copy(flag, 5, length(flag) - 4)
      else if Pos('MF_', flag) = 1 then
        flag := Copy(flag, 4, length(flag) - 3);
      if sc.MatchFlag4Ex('-' + flag) then
      begin
        mobj.flags4_ex := RemoveFlag(mobj.flags4_ex, flag);
        result := true;
        exit;
      end;
    end;

    result := false;
  end;


  function statecheckPos(const st: string; var sgoto: string): boolean;
  var
    sgoto1: string;
    len1, len2: integer;
  begin
    sgoto1 := strremovespaces(sgoto);
    Result := Pos(st, sgoto1) = 1;
    if Result then
    begin
      len1 := Length(sgoto1);
      len2 := Length(st);
      if len1 = len2 then
      begin
        sgoto := sgoto1;
        Exit;
      end;
      if sgoto1[len2 + 1] in [' ', '+', '-'] then
      begin
        sgoto := sgoto1;
        Exit;
      end;
      Result := False;
    end;
  end;

  function ParseState(const base: integer): boolean;
  var
    sprite: string;
    frames: string;
    tics: integer;
    action: string;
    i: integer;
    gotostr: string;
    offs: integer;
    bright: boolean;
    stmp: string;
    p: integer;
    alias: string;
    savealias: string;
    blevel: integer;
    restline: string;
  begin
    result := false;
    if sc._Finished then
      exit;
    sc.GetString;
    if sc.MatchString('loop') then
    begin
      m_states[numstates - 1].nextstate := base;
      blevel := sc.BracketLevel;
      if sc.GetString then
      begin
        if blevel = sc.BracketLevel then
          if Pos(':', sc._String) < 1 then
            result := true; // ACTOR definition not finished - same bracket level and not a new state
        sc.UnGet;
      end;
      exit;
    end
    else if sc.MatchString('stop') then
    begin
      m_states[numstates - 1].nextstate := -1; // S_NULL
      exit;
    end
    else if sc.MatchString('goto') then
    begin
      gotostr := strupper(sc.GetStringEOL);
      p := Pos('//', gotostr);
      if p > 0 then
        gotostr := Copy(gotostr, 1, p - 1);
      if (mobj.statesdefined and RTL_ST_SPAWN <> 0) and statecheckPos('SPAWN', gotostr) then
      begin
        if length(gotostr) > 5 then
          offs := atoi(strremovespaces(Copy(gotostr, 6, Length(gotostr) - 5)))
        else
          offs := 0;
        m_states[numstates - 1].nextstate := mobj.spawnstate + offs;
        m_states[numstates - 1].has_goto := true;
      end
      else if (mobj.statesdefined and RTL_ST_SEE <> 0) and statecheckPos('SEE', gotostr) then
      begin
        if length(gotostr) > 3 then
          offs := atoi(strremovespaces(Copy(gotostr, 4, Length(gotostr) - 3)))
        else
          offs := 0;
        m_states[numstates - 1].nextstate := mobj.seestate + offs;
        m_states[numstates - 1].has_goto := true;
      end
      else if (mobj.statesdefined and RTL_ST_MELEE <> 0) and statecheckPos('MELEE', gotostr) then
      begin
        if length(gotostr) > 5 then
          offs := atoi(strremovespaces(Copy(gotostr, 6, Length(gotostr) - 5)))
        else
          offs := 0;
        m_states[numstates - 1].nextstate := mobj.meleestate + offs;
        m_states[numstates - 1].has_goto := true;
      end
      else if (mobj.statesdefined and RTL_ST_MISSILE <> 0) and statecheckPos('MISSILE', gotostr) then
      begin
        if length(gotostr) > 7 then
          offs := atoi(strremovespaces(Copy(gotostr, 8, Length(gotostr) - 7)))
        else
          offs := 0;
        m_states[numstates - 1].nextstate := mobj.missilestate + offs;
        m_states[numstates - 1].has_goto := true;
      end
      else if (mobj.statesdefined and RTL_ST_PAIN <> 0) and statecheckPos('PAIN', gotostr) then
      begin
        if length(gotostr) > 4 then
          offs := atoi(strremovespaces(Copy(gotostr, 5, Length(gotostr) - 4)))
        else
          offs := 0;
        m_states[numstates - 1].nextstate := mobj.painstate + offs;
        m_states[numstates - 1].has_goto := true;
      end
      else if (mobj.statesdefined and RTL_ST_DEATH <> 0) and statecheckPos('DEATH', gotostr) then
      begin
        if length(gotostr) > 5 then
          offs := atoi(strremovespaces(Copy(gotostr, 6, Length(gotostr) - 5)))
        else
          offs := 0;
        m_states[numstates - 1].nextstate := mobj.deathstate + offs;
        m_states[numstates - 1].has_goto := true;
      end
      else if (mobj.statesdefined and RTL_ST_XDEATH <> 0) and statecheckPos('XDEATH', gotostr) then
      begin
        if length(gotostr) > 6 then
          offs := atoi(strremovespaces(Copy(gotostr, 7, Length(gotostr) - 6)))
        else
          offs := 0;
        m_states[numstates - 1].nextstate := mobj.xdeathstate + offs;
        m_states[numstates - 1].has_goto := true;
      end
      else if (mobj.statesdefined and RTL_ST_RAISE <> 0) and statecheckPos('RAISE', gotostr) then
      begin
        if length(gotostr) > 5 then
          offs := atoi(strremovespaces(Copy(gotostr, 6, Length(gotostr) - 5)))
        else
          offs := 0;
        m_states[numstates - 1].nextstate := mobj.raisestate + offs;
        m_states[numstates - 1].has_goto := true;
      end
      else if (mobj.statesdefined and RTL_ST_HEAL <> 0) and statecheckPos('HEAL', gotostr) then
      begin
        if length(gotostr) > 4 then
          offs := atoi(strremovespaces(Copy(gotostr, 5, Length(gotostr) - 4)))
        else
          offs := 0;
        m_states[numstates - 1].nextstate := mobj.healstate + offs;
        m_states[numstates - 1].has_goto := true;
      end
      else if (mobj.statesdefined and RTL_ST_CRASH <> 0) and statecheckPos('CRASH', gotostr) then
      begin
        if length(gotostr) > 5 then
          offs := atoi(strremovespaces(Copy(gotostr, 6, Length(gotostr) - 5)))
        else
          offs := 0;
        m_states[numstates - 1].nextstate := mobj.crashstate + offs;
        m_states[numstates - 1].has_goto := true;
      end
      {$IFDEF DOOM_OR_STRIFE}
      else if (mobj.statesdefined and RTL_ST_INTERACT <> 0) and statecheckPos('INTERACT', gotostr) then
      begin
        if length(gotostr) > 8 then
          offs := atoi(strremovespaces(Copy(gotostr, 9, Length(gotostr) - 8)))
        else
          offs := 0;
        m_states[numstates - 1].nextstate := mobj.interactstate + offs;
        m_states[numstates - 1].has_goto := true;
      end
      {$ENDIF}
      else
      begin
        //I_Warning('SC_ActordefToDEH(): Unknown label "goto %s"'#13#10, [gotostr]);
        //exit;
        m_states[numstates - 1].gotostr_needs_calc := true;
        m_states[numstates - 1].gotostr_calc := gotostr;

      end;
      result := sc.MatchString(state_tokens) = -1;

      exit;
    end;

    result := sc.MatchString(state_tokens) = -1;

    if not result then
      exit;

    if sc.BracketLevel = 0 then
    begin
      if not m_states[numstates - 1].has_goto then
        m_states[numstates - 1].nextstate := numstates - 1; // finished without stop keyword
      result := false;
      sc.UnGet;
      exit;
    end;

    result := true;

    alias := sc._String;
    savealias := '';
    if alias <> '' then
    begin
      if alias[Length(alias)] = ':' then
      begin
        SetLength(alias, Length(alias) - 1);
        savealias := alias;
        alias := 'S_' + strupper(mobj.name + '_' + alias);
        sc.GetString;
      end
      else
        alias := '';
    end;

    sprite := sc._string;
    sc.GetString;
    frames := sc._string;
    sc.GetInteger;
    tics := sc._integer;

    bright := false;
    action := '';
    sc.GetString;
    if sc.NewLine then
      sc.UnGet
    else
    begin
      if strupper(sc._string) = 'BRIGHT' then
        stmp := sc._string + ' ' + SC_RemoveLineComments(sc.GetStringEOLUnChanged)
      else
      begin
        restline := strtrim(SC_RemoveLineComments(sc.GetStringEOLUnChanged));
        if restline <> '' then
        begin
          if restline[length(restline)] = ')' then
            restline := '(' + restline;
          if restline[1] <> '(' then
            restline := ' ' + restline;
        end;
        stmp := sc._string + restline;
      end;

      bright := false;
      stmp := strtrim(stmp);
      if strupper(firstword(stmp)) = 'BRIGHT' then
      begin
        bright := true;
        action := secondword(stmp);
      end
      else if strupper(lastword(stmp)) = 'BRIGHT' then
      begin
        bright := true;
        action := Copy(stmp, 1, length(stmp) - 7);
      end
      else
        action := stmp;
    end;

    if action = '' then
      action := 'A_Null';

    for i := 1 to length(frames) do
    begin
      if numstates = MAXSTATES then
      begin
        I_Warning('SC_ActordefToDEH(): Object has more than %d states'#13#10, [MAXSTATES]);
      end
      else
      begin
        pm_state := @m_states[numstates];
        pm_state.sprite := sprite;
        pm_state.frame := Ord(frames[i]) - Ord('A');
        pm_state.tics := tics;
        pm_state.action := action;
        pm_state.nextstate := numstates + 1;
        pm_state.bright := bright;
        pm_state.alias := alias;
        pm_state.savealias := savealias;
        alias := ''; // Alias only for the first state
        savealias := '';
        inc(numstates);
      end;
    end;

  end;

  procedure AddRes(const s: string);
  begin
    res := res + s + #13#10;
  end;

  procedure AddMobjStateRes(const st: integer; const prefix: string);
  begin
    if st = -1 then
      AddRes(prefix + ' Frame = OriginalFrame 0')
    else if st >= ORIGINALSTATEMARKER then
      AddRes(prefix + ' Frame = OriginalFrame ' + itoa(st - ORIGINALSTATEMARKER))
    else
      AddRes(prefix + ' Frame = NewFrame ' + itoa(st));
  end;

  function ResolveGoto(const s: string): string;
  var
    st: string;
    fw, sw: string;
    pps, ppp, ppb: integer;
    ret: integer;

    function _stindex(const sss: string): integer;
    var
      sss1, sss2: string;
      i, p, idx: integer;
      inf: Pmobjinfo_t;
      doself: boolean;
    begin
      result := statenames.IndexOfToken(sss);
      if result >= 0 then
      begin
        result := ORIGINALSTATEMARKER + result;
        exit;
      end;

      sss1 := strupper(sss);

      doself := false;
      inf := nil;

      p := Pos('::', sss1);
      if p < 2 then // eg allow "goto ::spawn"
        doself := true
      else
      begin
        sss2 := strtrim(Copy(sss1, 1, p - 1));
        sss1 := Copy(sss1, p + 2, Length(sss1) - p - 1);
        idx := -1;
        if sss2 = 'SUPER' then
          idx := Info_GetMobjNumForName(mobj.inheritsfrom)
        else if sss2 = 'SELF' then
          doself := true
        else
          idx := Info_GetMobjNumForName(sss2);

        if (idx >= 0) and (idx < nummobjtypes) then
          inf := @mobjinfo[idx]
        else
          inf := nil;
      end;

      if inf <> nil then
      begin
        if sss1 = 'SPAWN' then
        begin
          result := ORIGINALSTATEMARKER + inf.spawnstate;
          exit;
        end
        else if sss1 = 'SEE' then
        begin
          result := ORIGINALSTATEMARKER + inf.seestate;
          exit;
        end
        else if sss1 = 'MELEE' then
        begin
          result := ORIGINALSTATEMARKER + inf.meleestate;
          exit;
        end
        else if sss1 = 'MISSILE' then
        begin
          result := ORIGINALSTATEMARKER + inf.missilestate;
          exit;
        end
        else if sss1 = 'MISSILE' then
        begin
          result := ORIGINALSTATEMARKER + inf.missilestate;
          exit;
        end
        else if sss1 = 'PAIN' then
        begin
          result := ORIGINALSTATEMARKER + inf.painstate;
          exit;
        end
        else if sss1 = 'DEATH' then
        begin
          result := ORIGINALSTATEMARKER + inf.deathstate;
          exit;
        end
        else if sss1 = 'XDEATH' then
        begin
          result := ORIGINALSTATEMARKER + inf.xdeathstate;
          exit;
        end
        else if sss1 = 'RAISE' then
        begin
          result := ORIGINALSTATEMARKER + inf.raisestate;
          exit;
        end
        else if sss1 = 'CRASH' then
        begin
          result := ORIGINALSTATEMARKER + inf.crashstate;
          exit;
        end
        {$IFDEF DOOM_OR_STRIFE}
        else if sss1 = 'INTERACT' then
        begin
          result := ORIGINALSTATEMARKER + inf.interactstate;
          exit;
        end
        {$ENDIF}
        else
        begin
          result := statenames.IndexOfToken('S_' + strupper(inf.name) + '_' + sss);
          if result >= 0 then
            exit;
          result := statenames.IndexOfToken('S_' + strremovespaces(strupper(inf.name)) + '_' + sss);
          if result >= 0 then
            exit;
        end;
      end;

      if doself then
      begin
        if sss1 = 'SPAWN' then
        begin
          result := mobj.spawnstate;
          exit;
        end
        else if sss1 = 'SEE' then
        begin
          result := mobj.seestate;
          exit;
        end
        else if sss1 = 'MELEE' then
        begin
          result := mobj.meleestate;
          exit;
        end
        else if sss1 = 'MISSILE' then
        begin
          result := mobj.missilestate;
          exit;
        end
        else if sss1 = 'MISSILE' then
        begin
          result := mobj.missilestate;
          exit;
        end
        else if sss1 = 'PAIN' then
        begin
          result := mobj.painstate;
          exit;
        end
        else if sss1 = 'DEATH' then
        begin
          result := mobj.deathstate;
          exit;
        end
        else if sss1 = 'XDEATH' then
        begin
          result := mobj.xdeathstate;
          exit;
        end
        else if sss1 = 'RAISE' then
        begin
          result := mobj.raisestate;
          exit;
        end
        else if sss1 = 'CRASH' then
        begin
          result := mobj.crashstate;
          exit;
        end
        {$IFDEF DOOM_OR_STRIFE}
        else if sss1 = 'INTERACT' then
        begin
          result := mobj.interactstate;
          exit;
        end
        {$ENDIF}
        else
        begin
          for i := 0 to numstates - 1 do
            if sss1 = strtrim(strupper(m_states[i].savealias)) then
            begin
              result := i;
              exit;
            end;
        end;
      end;

      sss1 := 'S_' + strupper(mobj.name) + '_' + sss;
      result := statenames.IndexOfToken(sss1);
    end;

  begin
    st := strtrim(strupper(strtrim(s)));
    pps := Pos('+', st);
    ppp := Pos('-', st);
    ppb := Pos(' ', st);
    ret := -1;
    if (ppb = 0) and (ppp = 0) and (pps = 0) then
    begin
      ret := _stindex(st);
    end
    else
    // JVAL: 20170927 evaluate small expressions
    //       20191003 rewritten, fixed
    begin
      st := strremovespaces(st);
      pps := Pos('+', st);
      ppp := Pos('-', st);
      if pps > 0 then
      begin
        splitstring(st, fw, sw, '+');
        ret := _stindex(fw) + atoi(sw, 0);
      end
      else if ppp > 0 then
      begin
        splitstring(st, fw, sw, '-');
        ret := _stindex(fw) - atoi(sw, 0);
      end;
    end;

    if ret = -1 then
    begin
      I_Warning('SC_ActordefToDEH(): Unknown label "goto %s"'#13#10, [s]);
      result := 'OriginalFrame 0';
    end
    else if ret >= ORIGINALSTATEMARKER then
      result := 'OriginalFrame ' + itoa(ret - ORIGINALSTATEMARKER)
    else
      result := 'NewFrame ' + itoa(ret);
  end;

  procedure SubmitParsedData;
  var
    cnt: integer;
    stateprefix: string;
  begin
    if not actorpending then
      Exit;
    actorpending := false;
    res := '';

    if mobj.replacesid >= 0 then
    begin
      AddRes('Thing ' + itoa(mobj.replacesid + 1)); // JVAL DEH patches start Think numbers from 1
      Info_AddMobjNameAlias(mobj.name, Info_GetMobjName(mobj.replacesid));
    end
    else
      AddRes('NewThing ' + mobj.name);
    if mobj.inheritsfrom <> '' then
      AddRes('Inheritsfrom = ' + mobj.inheritsfrom)
    else
      AddRes('Inheritsfrom = -1');
{$IFDEF STRIFE}
    if mobj.name2 <> '' then
      AddRes('name = ' + mobj.name2);
{$ENDIF}
    AddRes('Id # = ' + itoa(mobj.doomednum));
    AddMobjStateRes(mobj.spawnstate, 'Initial');
    AddRes('Hit Points = ' + itoa(mobj.spawnhealth));
    AddMobjStateRes(mobj.seestate, 'First Moving');
    AddRes('Alert Sound = ' + SC_SoundAlias(mobj.seesound));
    AddRes('Reaction Time = ' + itoa(mobj.reactiontime));
    AddRes('Attack Sound = ' + SC_SoundAlias(mobj.attacksound));
    AddMobjStateRes(mobj.painstate, 'Injury');
    AddRes('Pain Chance = ' + itoa(mobj.painchance));
    AddRes('Pain Sound = ' + SC_SoundAlias(mobj.painsound));
    AddMobjStateRes(mobj.meleestate, 'Close Attack');
    AddMobjStateRes(mobj.missilestate, 'Far Attack');
    AddMobjStateRes(mobj.deathstate, 'Death');
    AddMobjStateRes(mobj.xdeathstate, 'Exploding');
    AddMobjStateRes(mobj.healstate, 'Heal');
    AddMobjStateRes(mobj.crashstate, 'Crash');
    {$IFDEF DOOM_OR_STRIFE}
    AddMobjStateRes(mobj.interactstate, 'Interact');
    {$ENDIF}
    AddRes('Death Sound = ' + SC_SoundAlias(mobj.deathsound));
    ismissile := Pos('MF_MISSILE', mobj.flags) > 0;
    if ismissile then
      if mobj.speed < 2048 then // JVAL fix me
        mobj.speed := mobj.speed * FRACUNIT;
    AddRes('Speed = ' + itoa(mobj.speed));
    AddRes('VSpeed = ' + itoa(round(mobj.vspeed * FRACUNIT)));
    AddRes('Pushfactor = ' + itoa(round(mobj.pushfactor * FRACUNIT)));
    AddRes('Width = ' + itoa(mobj.radius * FRACUNIT));
    AddRes('Height = ' + itoa(mobj.height * FRACUNIT));
    AddRes('Mass = ' + itoa(mobj.mass));
    AddRes('Missile Damage = ' + itoa(mobj.damage));
    AddRes('Action Sound = ' + SC_SoundAlias(mobj.activesound));
    AddMobjStateRes(mobj.raisestate, 'Respawn');
    AddRes('Bits = ' + mobj.flags);
    AddRes('flags_ex = ' + mobj.flags_ex);
    AddRes('Custom Sound 1 = ' + SC_SoundAlias(mobj.customsound1));
    AddRes('Custom Sound 2 = ' + SC_SoundAlias(mobj.customsound2));
    AddRes('Custom Sound 3 = ' + SC_SoundAlias(mobj.customsound3));
    AddRes('Melee Damage = ' + itoa(mobj.meleedamage));
    AddRes('Melee Sound = ' + SC_SoundAlias(mobj.meleesound));
    AddRes('Explosion Damage = ' + itoa(mobj.explosiondamage));
    AddRes('Explosion Radius = ' + itoa(mobj.explosionradius));
    AddRes('Dropitem = ' + mobj.dropitem);
    AddRes('Missiletype = ' + mobj.missiletype);
    if mobj.renderstyle <> '' then
      AddRes('Renderstyle = ' + mobj.renderstyle)
    else
      AddRes('Renderstyle = NORMAL');
    AddRes('Alpha = ' + itoa(mobj.alpha));
    AddRes('flags2_ex = ' + mobj.flags2_ex);
    AddRes('flags3_ex = ' + mobj.flags3_ex);
    AddRes('flags4_ex = ' + mobj.flags4_ex);
    {$IFDEF DOOM_OR_STRIFE}
    AddRes('missileheight = ' + itoa(mobj.missileheight));
    {$ENDIF}
    AddRes('Scale = ' + itoa(round(mobj.scale * FRACUNIT)));
    AddRes('gravity = ' + itoa(round(mobj.gravity * FRACUNIT)));
    AddRes('');

    if numstates > 0 then
    begin
      if m_states[numstates - 1].nextstate = numstates then
        m_states[numstates - 1].nextstate := numstates - 1;

      stateprefix := 'S_' + strupper(mobj.name);
      for cnt := 0 to numstates - 1 do
      begin
        if m_states[cnt].alias <> '' then
          statenames.Add(stateprefix + itoa(cnt) + ', ' + m_states[cnt].alias)
        else
          statenames.Add(stateprefix + itoa(cnt));
        AddRes('Frame NewFrame ' + itoa(cnt));
        AddRes('Sprite Number = ' + m_states[cnt].sprite);
        if m_states[cnt].bright then
          AddRes('Sprite Subnumber = Bright ' + itoa(m_states[cnt].frame))
        else
          AddRes('Sprite Subnumber = ' + itoa(m_states[cnt].frame));
        AddRes('Duration = ' + itoa(m_states[cnt].tics));

        if m_states[cnt].gotostr_needs_calc then
          AddRes('Next Frame = ' + ResolveGoto(m_states[cnt].gotostr_calc))
        else if m_states[cnt].nextstate >= ORIGINALSTATEMARKER then
          AddRes('Next Frame = OriginalFrame ' + itoa(m_states[cnt].nextstate - ORIGINALSTATEMARKER))
        else if m_states[cnt].nextstate >= 0 then
          AddRes('Next Frame = NewFrame ' + itoa(m_states[cnt].nextstate))
        else
          AddRes('Next Frame = OriginalFrame 0');
        AddRes('Codep Frame = ' + m_states[cnt].action);
        AddRes('Unknown 1 = 0');
        AddRes('Unknown 2 = 0');
        {$IFNDEF STRIFE}
        AddRes('Flags_ex = 0');
        {$ENDIF}
        AddRes('Owner = ' + mobj.name);
        AddRes('');
      end;
      AddRes('');
      AddRes('');
      AddRes('SubmitNewFrames');
    end;

    AddRes('');
    AddRes('');
    if devparm then
    begin
      printf('SC_SubmitParsedData(): Submiting actordef lump to DEH subsystem:'#13#10);
      printf(res);
      printf('--------'#13#10);
    end;
    DEH_ParseText(res);
  end;

  procedure DoParseInlineScript(const typ: psscripttype_t);
  begin
    // Inline script(source) inside ACTORDEF lump
    passcript := '';

    if sc.GetString then
    begin
      passcriptname := sc._String;
      if passcriptname <> '' then
      begin
        if passcriptname[Length(passcriptname)] = ';' then
          SetLength(passcriptname, Length(passcriptname) - 1);
        if Length(passcriptname) > 0 then
        begin
          passcriptline := sc.GetStringEOLUnChanged;
          if passcriptline = '' then
            passcriptline := sc.GetStringEOLUnChanged;
          while (strupper(firstword(passcriptline, [Chr(9), ' ', ';', '.'])) <> 'ENDSCRIPT') and (not sc._Finished) do
          begin
            if passcript = '' then
              passcript := passcriptline
            else
              passcript := passcript + #13#10 + passcriptline;
            passcriptline := sc.GetStringEOLUnChanged;
          end;
          if passcript <> '' then
          begin
            PS_AddSourceScript(passcriptname, passcript, typ);
          end;
        end;
      end;
    end;
  end;

  procedure DoParseActorAlias;
  var
    s1: string;
    i, p: integer;
    sctmp: TScriptEngine;
    lst: TDStringList;
    sinitial: string;
  begin
    s1 := sc.GetStringEOLWithQuotes;
    sinitial := s1;
    p := Pos('//', s1);
    if p > 0 then
      s1 := Copy(s1, 1, p - 1);

    for i := 1 to length(s1) do
      if s1[i] = '=' then
          s1[i] := ' ';

    lst := TDStringList.Create;

    sctmp := TScriptEngine.Create(s1);
    while sctmp.GetString do
      lst.Add(sctmp._String);
    sctmp.Free;

    if lst.Count < 2 then
      I_Warning('DoParseActorAlias(): Invalid alias declaration "%s"'#13#10, [sinitial])
    else
      for i := 0 to lst.Count - 2 do
        if not Info_AddMobjNameAlias(lst.Strings[i], lst.Strings[i + 1]) then
          I_Warning('DoParseActorAlias(): Can not match aliases "%s" and "%s"'#13#10, [lst.Strings[i], lst.Strings[i + 1]]);

    lst.Free;
  end;

  procedure DoDEHParse;
  var
    lst: TDStringList;
    i: integer;
  begin
    lst := sc.GetTokensEOL;
    for i := 0 to lst.Count - 1 do
      DEH_ParseText(PAK_ReadFileAsString(lst.Strings[i]));
    lst.Free;
  end;

  procedure DoDEHParseAll;
  var
    lst: TDStringList;
    i: integer;
  begin
    lst := sc.GetTokensEOL;
    for i := 0 to lst.Count - 1 do
      DEH_ParseText(PAK_ReadAllFilesAsString(lst.Strings[i]));
    lst.Free;
  end;

var
  slist: TDStringList;
  stmp: string;
  isreplace, isinherit: boolean;
  rstyle: mobjrenderstyle_t;
begin
  state_tokens := TDStringList.Create;
  state_tokens.Add('spawn:');
  state_tokens.Add('see:');
  state_tokens.Add('melee:');
  state_tokens.Add('missile:');
  state_tokens.Add('pain:');
  state_tokens.Add('death:');
  state_tokens.Add('xdeath:');
  state_tokens.Add('raise:');
  state_tokens.Add('heal:');
  state_tokens.Add('crash:');
  {$IFDEF DOOM_OR_STRIFE}
  state_tokens.Add('interact:');
  {$ENDIF}

  if devparm then
  begin
    printf('--------'#13#10);
    printf('SC_ParseActordefLump(): Parsing %s lump:'#13#10, [ACTORDEFLUMPNAME]);

    slist := TDStringList.Create;
    try
      slist.Text := in_text;
      for i := 0 to slist.Count - 1 do
        printf('%s: %s'#13#10, [IntToStrZFill(6, i + 1), slist[i]]);
    finally
      slist.Free;
    end;

    printf('--------'#13#10);
  end;

  actorpending := false;
  sc := TActordefScriptEngine.Create(in_text);
  while sc.GetString do
  begin
    // JVAL: THINKER KEYWORD
    if sc.MatchString('THINKER') then
    begin
      sc.MustGetString;
      th.name := sc._String;
      th.interval := 1;
      th.dn := -1;
      th.repeatcnt := 0;
      th.script := '';

      sc.MustGetString;
      if not sc.NewLine then
      begin
        th.dn := atoi(sc._string);
        sc.MustGetString;
      end;

      stmp := strupper(sc._String);
      slist := TDStringList.Create;
      slist.Add('SCRIPT');
      slist.Add('REPEAT');
      slist.Add('INTERVAL');
      while slist.IndexOf(stmp) >= 0 do
      begin
        if stmp = 'SCRIPT' then
        begin
          sc.MustGetString;
          th.script := sc._String;
          slist.Delete(slist.IndexOf('SCRIPT'));
        end
        else if stmp = 'REPEAT' then
        begin
          sc.MustGetString;
          if sc.MatchString('FOREVER') then
            th.repeatcnt := 0
          else
            th.repeatcnt := atoi(sc._String);
          if sc.GetString then
            if not sc.MatchString('TIME' + decide(th.repeatcnt = 1, '', 'S')) then
              sc.UnGet;
          slist.Delete(slist.IndexOf('REPEAT'));
        end
        else if stmp = 'INTERVAL' then
        begin
          sc.MustGetInteger;
          th.interval := sc._Integer;
          slist.Delete(slist.IndexOf('INTERVAL'));
        end;
        if sc._Finished then
          Break;
        sc.GetString;
        stmp := strupper(sc._String);
      end;
      slist.Free;

      if th.script <> '' then
        SC_SubmitThinker(@th)
      else
        I_Warning('SC_ActordefToDEH(): "THINKER" %s does not have script'#13#10, [th.name]);

      if sc._Finished then
        Break
    end;

    if sc.MatchString('GLOBAL') then  // JVAL: Global script
    begin
      sc.MustGetString;
      if strupper(sc._String) = 'SCRIPT' then
      begin
        DoParseInlineScript(pst_global);
      end
      else if sc.MatchString('EXTERNAL') then
      begin
        // External script(source) inside external pak file
        sc.MustGetString;
        if strupper(sc._String) = 'SCRIPT' then
        begin
          sc.MustGetString;
          passcriptname := sc._String;
          sc.MustGetString;
          passcriptfile := sc._String;
          PS_AddSourceScript(passcriptname, PAK_ReadFileAsString(passcriptfile), pst_global);
        end
        else
          I_Warning('SC_ActordefToDEH(): Unknown token "%s" found, "SCRIPT" expected'#13#10, [sc._String]);
      end
      else
        I_Warning('SC_ActordefToDEH(): Unknown token "%s" found, "SCRIPT" expected'#13#10, [sc._String]);
    end;

    // JVAL: PascalScript extraction :)
    if sc.MatchString('SCRIPT') then
    begin
      DoParseInlineScript(pst_normal);
    end
    else if sc.MatchString('COMPILED') or sc.MatchString('PRECOMPILED') then
    begin
      // Compiled script inside external pak file
      sc.MustGetString;
      if strupper(sc._String) = 'SCRIPT' then
      begin
        sc.MustGetString;
        passcriptname := sc._String;
        sc.MustGetString;
        passcriptfile := sc._String;
        PS_AddCompiledScript(passcriptname, passcriptfile);
      end
      else
        I_Warning('SC_ActordefToDEH(): Unknown token "%s" found, "SCRIPT" expected'#13#10, [sc._String]);
    end
    else if sc.MatchString('EXTERNAL') then
    begin
      // External script(source) inside external pak file
      sc.MustGetString;
      if strupper(sc._String) = 'SCRIPT' then
      begin
        sc.MustGetString;
        passcriptname := sc._String;
        sc.MustGetString;
        passcriptfile := sc._String;
        PS_AddSourceScript(passcriptname, PAK_ReadFileAsString(passcriptfile), pst_normal);
      end
      else
        I_Warning('SC_ActordefToDEH(): Unknown token "%s" found, "SCRIPT" expected'#13#10, [sc._String]);
    end;

    if sc.MatchString('ACTORALIAS') then
      DoParseActorAlias
    else if sc.MatchString('DEH_PARSE') then
      DoDEHParse
    else if sc.MatchString('DEH_PARSE_ALL') then
      DoDEHParseAll
    else if sc.MatchString('ACTOR') then
    begin
      pinf := nil;
      actorpending := true;
      FillChar(mobj, SizeOf(mobj), 0);
      mobj.spawnstate := -1;
      mobj.seestate := -1;
      mobj.painstate := -1;
      mobj.meleestate := -1;
      mobj.missilestate := -1;
      mobj.deathstate := -1;
      mobj.xdeathstate := -1;
      mobj.raisestate := -1;
      mobj.healstate := -1;
      mobj.crashstate := -1;
      {$IFDEF DOOM_OR_STRIFE}
      mobj.interactstate := -1;
      {$ENDIF}
      mobj.flags := '';
      {$IFDEF HERETIC_OR_HEXEN}
      mobj.flags2 := '';
      {$ENDIF}
      mobj.flags_ex := '';
      mobj.flags2_ex := '';
      mobj.flags3_ex := '';
      mobj.flags4_ex := '';
      {$IFDEF STRIFE}
      mobj.name2 := '';
      {$ENDIF}
      mobj.scale := 1.0;
      mobj.pushfactor := 0.25; {DEFPUSHFACTOR / FRACUNIT;}
      mobj.gravity := 1.0;
      mobj.replacesid := -1;
      ismissile := false;
      FillChar(m_states, SizeOf(m_states), 0);
      sc.GetString;
      mobj.name := sc._String;
      isreplace := false;
      isinherit := false;
      if Pos(':', mobj.name) = Length(mobj.name) then
      begin
        SetLength(mobj.name, Length(mobj.name) - 1);
        sc.GetString;
        mobj.inheritsfrom := sc._string;
        isinherit := true;
      end
      else
      begin
        if not sc.GetString then
          break;

        isinherit := sc.MatchString(':') or sc.MatchString('inherits') or sc.MatchString('inheritsfrom');
        isreplace := sc.MatchString('replaces');

        if isreplace or isinherit then
        begin
          if not sc.GetString then
            break;
          mobj.inheritsfrom := sc._string;

        end;
      end;

      if mobj.inheritsfrom <> '' then
      begin
        idx := Info_GetMobjNumForName(mobj.inheritsfrom);
        if idx >= 0 then
        begin
          pinf := @mobjinfo[idx];
          if isreplace then
          begin
            mobj.doomednum := pinf.doomednum;
            mobj.replacesid := idx;
          end;
          mobj.spawnhealth := pinf.spawnhealth;
          mobj.seesound := itoa(pinf.seesound);
          mobj.reactiontime := pinf.reactiontime;
          mobj.attacksound := itoa(pinf.attacksound);
          mobj.painchance := pinf.painchance;
          mobj.painsound := itoa(pinf.painsound);
          mobj.deathsound := itoa(pinf.deathsound);
          mobj.speed := pinf.speed;
          mobj.radius := pinf.radius div FRACUNIT;
          mobj.height := pinf.height div FRACUNIT;
          mobj.mass := pinf.mass;
          mobj.damage := pinf.damage;
          mobj.activesound := itoa(pinf.activesound);
          for i := 0 to 31 do
            if pinf.flags and _SHL(1, i) <> 0 then
              mobj.flags := mobj.flags + mobj_flags[i] + ' ';
          {$IFDEF HERETIC_OR_HEXEN}
          for i := 0 to 31 do
            if pinf.flags2 and _SHL(1, i) <> 0 then
              mobj.flags2 := mobj.flags2 + mobj_flags2[i] + ' ';
          {$ENDIF}
          for i := 0 to 31 do
            if pinf.flags_ex and _SHL(1, i) <> 0 then
              mobj.flags_ex := mobj.flags_ex + mobj_flags_ex[i] + ' ';
          for i := 0 to 31 do
            if pinf.flags2_ex and _SHL(1, i) <> 0 then
              mobj.flags2_ex := mobj.flags2_ex + mobj_flags2_ex[i] + ' ';
          for i := 0 to 31 do
            if pinf.flags3_ex and _SHL(1, i) <> 0 then
              mobj.flags3_ex := mobj.flags3_ex + mobj_flags3_ex[i] + ' ';
          for i := 0 to 31 do
            if pinf.flags4_ex and _SHL(1, i) <> 0 then
              mobj.flags4_ex := mobj.flags4_ex + mobj_flags4_ex[i] + ' ';
          mobj.customsound1 := itoa(pinf.customsound1);
          mobj.customsound2 := itoa(pinf.customsound2);
          mobj.customsound3 := itoa(pinf.customsound3);
          mobj.dropitem := itoa(pinf.dropitem);
          mobj.missiletype := itoa(pinf.missiletype);
          mobj.explosiondamage := pinf.explosiondamage;
          mobj.explosionradius := pinf.explosionradius;
          mobj.meleedamage := pinf.meleedamage;
          mobj.meleesound := itoa(pinf.meleesound);
          mobj.renderstyle := itoa(Ord(pinf.renderstyle));
          mobj.alpha := pinf.alpha;
          {$IFDEF DOOM_OR_STRIFE}
          mobj.missileheight := pinf.missileheight;
          {$ENDIF}
          mobj.vspeed := pinf.vspeed / FRACUNIT;
          mobj.pushfactor := pinf.pushfactor / FRACUNIT;
          mobj.scale := pinf.scale / FRACUNIT;

          mobj.spawnstate := ORIGINALSTATEMARKER + pinf.spawnstate;
          mobj.seestate := ORIGINALSTATEMARKER + pinf.seestate;
          mobj.painstate := ORIGINALSTATEMARKER + pinf.painstate;
          mobj.meleestate := ORIGINALSTATEMARKER + pinf.meleestate;
          mobj.missilestate := ORIGINALSTATEMARKER + pinf.missilestate;
          mobj.deathstate := ORIGINALSTATEMARKER + pinf.deathstate;
          mobj.xdeathstate := ORIGINALSTATEMARKER + pinf.xdeathstate;
          mobj.raisestate := ORIGINALSTATEMARKER + pinf.raisestate;
          mobj.healstate := ORIGINALSTATEMARKER + pinf.healstate;
          mobj.crashstate := ORIGINALSTATEMARKER + pinf.crashstate;
          {$IFDEF DOOM_OR_STRIFE}
          mobj.interactstate := ORIGINALSTATEMARKER + pinf.interactstate;
          {$ENDIF}
          if mobj.spawnstate > ORIGINALSTATEMARKER then
            mobj.statesdefined := mobj.statesdefined or RTL_ST_SPAWN;
          if mobj.seestate > ORIGINALSTATEMARKER then
            mobj.statesdefined := mobj.statesdefined or RTL_ST_SEE;
          if mobj.meleestate > ORIGINALSTATEMARKER then
            mobj.statesdefined := mobj.statesdefined or RTL_ST_MELEE;
          if mobj.missilestate > ORIGINALSTATEMARKER then
            mobj.statesdefined := mobj.statesdefined or RTL_ST_MISSILE;
          if mobj.painstate > ORIGINALSTATEMARKER then
            mobj.statesdefined := mobj.statesdefined or RTL_ST_PAIN;
          if mobj.deathstate > ORIGINALSTATEMARKER then
            mobj.statesdefined := mobj.statesdefined or RTL_ST_DEATH;
          if mobj.xdeathstate > ORIGINALSTATEMARKER then
            mobj.statesdefined := mobj.statesdefined or RTL_ST_XDEATH;
          if mobj.raisestate > ORIGINALSTATEMARKER then
            mobj.statesdefined := mobj.statesdefined or RTL_ST_RAISE;
          if mobj.healstate > ORIGINALSTATEMARKER then
            mobj.statesdefined := mobj.statesdefined or RTL_ST_HEAL;
          if mobj.crashstate > ORIGINALSTATEMARKER then
            mobj.statesdefined := mobj.statesdefined or RTL_ST_CRASH;
          {$IFDEF DOOM_OR_STRIFE}
          if mobj.interactstate > ORIGINALSTATEMARKER then
            mobj.statesdefined := mobj.statesdefined or RTL_ST_INTERACT;
          {$ENDIF};
        end;

        if not sc.GetString then
          break;
      end;

      if sc.NewLine then
      begin
        if isinherit then
          mobj.doomednum := -1;
      end
      else
      begin
        if StrIsInteger(sc._string) then
        begin
          mobj.doomednum := atoi(sc._string);
          if not sc.GetString then
            break;
        end;
      end;

      foundstates := false;
      repeat
        {$IFDEF STRIFE}
        if sc.MatchString('name') or sc.MatchString('strifename') then
        begin
          sc.GetString;
          mobj.name2 := sc._String;
          sc.GetString;
        end
        else
        {$ENDIF}
        if sc.MatchString('health') then
        begin
          sc.GetInteger;
          mobj.spawnhealth := sc._integer;
          sc.GetString;
        end
        // When "inherits" is after the first line of actor we do not copy properties
        else if sc.MatchString('inherits') or sc.MatchString('inheritsfrom') then
        begin
          if not sc.GetString then
            break;
          mobj.inheritsfrom := sc._string;
        end
        else if sc.MatchString('replaces') then
        begin
          if not sc.GetString then
            break;
          idx := Info_GetMobjNumForName(sc._string);
          if idx >= 0 then
          begin
            if (mobj.doomednum > 0) and (mobjinfo[idx].doomednum <> mobj.doomednum) then
              I_Warning('SC_ActordefToDEH(): Replaces keyword found but points to a new doomednum %d, old doomednum=%d', [mobjinfo[idx].doomednum, mobj.doomednum])
            else
            begin
              mobj.doomednum := mobjinfo[idx].doomednum;
              mobj.replacesid := idx;
            end;
          end
          else
            I_Warning('SC_ActordefToDEH(): Replaces keyword point to an unknown mobj %s'#13#10, [sc._string]);
          sc.GetString;
        end
        else if sc.MatchString('monster') or sc.MatchString('+monster') then
        begin
           mobj.flags := mobj.flags + 'MF_SOLID MF_SHOOTABLE MF_COUNTKILL ';
           sc.GetString;
        end
        else if sc.MatchString('projectile') or sc.MatchString('+projectile') then
        begin
           mobj.flags := mobj.flags + 'MF_NOGRAVITY MF_DROPOFF  MF_MISSILE ';
           sc.GetString;
        end

        else if sc.MatchString('RENDERSTYLE') then
        begin
          sc.GetString;
          rstyle := R_GetRenderstyleForName(sc._String);
          mobj.renderstyle := renderstyle_tokens[Ord(rstyle)];
          sc.GetString;
        end

        else if sc.MatchString('ALPHA') then
        begin
          sc.GetFloat;
          mobj.alpha := round(sc._float * FRACUNIT);
          sc.GetString;
        end

        else if MatchFlags then
          sc.GetString
        {$IFDEF HERETIC_OR_HEXEN}
        else if MatchFlags2 then
          sc.GetString
        {$ENDIF}
        else if MatchFlagsEx then
          sc.GetString
        else if MatchFlags2Ex then
          sc.GetString
        else if MatchFlags3Ex then
          sc.GetString
        else if MatchFlags4Ex then
          sc.GetString

        else if MatchFlags_Delete then
          sc.GetString
        {$IFDEF HERETIC_OR_HEXEN}
        else if MatchFlags2_Delete then
          sc.GetString
        {$ENDIF}
        else if MatchFlagsEx_Delete then
          sc.GetString
        else if MatchFlags2Ex_Delete then
          sc.GetString
        else if MatchFlags3Ex_Delete then
          sc.GetString
        else if MatchFlags4Ex_Delete then
          sc.GetString

        else if sc.MatchString('DEFAULTMISSILE') or sc.MatchString('+DEFAULTMISSILE') then // JVAL: DelphiDoom specific
        begin
          mobj.flags := mobj.flags + 'NOGRAVITY MISSILE NOBLOCKMAP DROPOFF ';
          sc.GetString;
        end
        else if sc.MatchString('DEFAULTTRANSPARENT') or sc.MatchString('+DEFAULTTRANSPARENT') then // JVAL: DelphiDoom specific
        begin
          mobj.renderstyle := 'TRANSLUCENT';
          mobj.alpha := round(0.67 * FRACUNIT);
          sc.GetString;
        end
        else if sc.MatchString('DEFAULTADDITIVE') or sc.MatchString('+DEFAULTADDITIVE') then // JVAL: DelphiDoom specific
        begin
          mobj.renderstyle := 'ADD';
          mobj.alpha := round(0.67 * FRACUNIT);
          sc.GetString;
        end

        else if sc.MatchString('FULLVOLSOUND') or sc.MatchString('FULLVOLSOUNDS') or
                sc.MatchString('+FULLVOLSOUND') or sc.MatchString('+FULLVOLSOUNDS') then
        begin
          mobj.flags2_ex := mobj.flags2_ex + 'FULLVOLACTIVE FULLVOLDEATH FULLVOLSEE FULLVOLPAIN FULLVOLATTACK ';
          sc.GetString;
        end

        else if sc.MatchString('radius') or sc.MatchString('width') then  // JVAL: width -> DelphiDoom specific
        begin
          sc.GetInteger;
          mobj.radius := sc._integer;
          sc.GetString;
        end
        else if sc.MatchString('height') then
        begin
          sc.GetInteger;
          mobj.height := sc._integer;
          sc.GetString;
        end
        else if sc.MatchString('vspeed') then
        begin
          sc.GetFloat;
          mobj.vspeed := sc._float;
          sc.GetString;
        end
        else if sc.MatchString('pushfactor') then
        begin
          sc.GetFloat;
          mobj.pushfactor := sc._float;
          // JVAL In case that we encounter a fixed_t value
          //      Normal values for pushfactor are 0..1 in ACTORDEF lumps
          //      and 0..FRACUNIT in mobjinfo table
          if mobj.pushfactor > 64 then
            mobj.pushfactor := mobj.pushfactor / FRACUNIT;
          sc.GetString;
        end
        else if sc.MatchString('scale') then
        begin
          sc.GetFloat;
          mobj.scale := sc._float;
          if mobj.scale > 64 then
            mobj.scale := mobj.scale / FRACUNIT;
          sc.GetString;
        end
        else if sc.MatchString('gravity') then
        begin
          sc.GetFloat;
          mobj.gravity := sc._float;
          if mobj.gravity > 64 then
            mobj.gravity := mobj.gravity / FRACUNIT;
          sc.GetString;
        end
        else if sc.MatchString('speed') then
        begin
          sc.GetInteger;
          mobj.speed := sc._integer;
          sc.GetString;
        end
        else if sc.MatchString('damage') then
        begin
          sc.GetInteger;
          mobj.damage := sc._integer;
          sc.GetString;
        end
        else if sc.MatchString('painchance') then
        begin
          sc.GetInteger;
          mobj.painchance := sc._integer;
          sc.GetString;
        end
        else if sc.MatchString('mass') then
        begin
          sc.GetInteger;
          mobj.mass := sc._integer;
          sc.GetString;
        end
        else if sc.MatchString('reactiontime') then
        begin
          sc.GetInteger;
          mobj.reactiontime := sc._integer;
          sc.GetString;
        end
        else if sc.MatchString('dropitem') then
        begin
          sc.GetString;
          mobj.dropitem := sc._string;
          sc.GetString;
        end
        else if sc.MatchString('missiletype') then
        begin
          sc.GetString;
          mobj.missiletype := sc._string;
          sc.GetString;
        end
        else if sc.MatchString('explosiondamage') then
        begin
          sc.GetInteger;
          mobj.explosiondamage := sc._integer;
          sc.GetString;
        end
        else if sc.MatchString('explosionradius') then
        begin
          sc.GetInteger;
          mobj.explosionradius := sc._integer;
          sc.GetString;
        end
        else if sc.MatchString('meleedamage') then
        begin
          sc.GetInteger;
          mobj.meleedamage := sc._Integer;
          sc.GetString;
        end
        else if sc.MatchString('seesound') then
        begin
          sc.GetString;
          mobj.seesound := sc._String;
          sc.GetString;
        end
        else if sc.MatchString('attacksound') then
        begin
          sc.GetString;
          mobj.attacksound := sc._String;
          sc.GetString;
        end
        else if sc.MatchString('painsound') then
        begin
          sc.GetString;
          mobj.painsound := sc._String;
          sc.GetString;
        end
        else if sc.MatchString('deathsound') then
        begin
          sc.GetString;
          mobj.deathsound := sc._String;
          sc.GetString;
        end
        else if sc.MatchString('activesound') then
        begin
          sc.GetString;
          mobj.activesound := sc._String;
          sc.GetString;
        end
        else if sc.MatchString('customsound1') then
        begin
          sc.GetString;
          mobj.customsound1 := sc._String;
          sc.GetString;
        end
        else if sc.MatchString('customsound2') then
        begin
          sc.GetString;
          mobj.customsound2 := sc._String;
          sc.GetString;
        end
        else if sc.MatchString('customsound3') then
        begin
          sc.GetString;
          mobj.customsound3 := sc._String;
          sc.GetString;
        end
        else if sc.MatchString('meleesound') then
        begin
          sc.GetString;
          mobj.meleesound := sc._String;
          sc.GetString;
        end
        {$IFDEF DOOM_OR_STRIFE}
        else if sc.MatchString('missileheight') then
        begin
          sc.GetInteger;
          mobj.missileheight := sc._Integer;
          sc.GetString;
        end
        {$ENDIF}
        else if sc.MatchString('states') then
        begin
          foundstates := true;
        end
        else
        begin
          if sc.MatchString('ACTOR') or
             sc.MatchString('ACTORALIAS') or
             sc.MatchString('DEH_PARSE') or
             sc.MatchString('DEH_PARSE_ALL') or
             sc.MatchString('COMPILED') or
             sc.MatchString('PRECOMPILED') or
             sc.MatchString('EXTERNAL') or
             sc.MatchString('SCRIPT') or
             sc.MatchString('THINKER') or
             sc.MatchString('GLOBAL') then
          begin
            sc.UnGet;
            break;
          end
          else
          begin
            if mobj.name <> '' then
              stmp := ' while parsing mobj "' + mobj.name + '"'
            else
              stmp := '';
            if mobj.doomednum > -1 then
              stmp := stmp + ' (doomednum=' + itoa(mobj.doomednum) + ')';
            I_Warning('SC_ActordefToDEH(): Unknown token "%s" found%s'#13#10, [sc._String, stmp]);
            sc.GetString;
          end;
        end;
      until foundstates or sc._Finished;

      if strtrim(mobj.flags) = '' then
        mobj.flags := '0';
      {$IFDEF HERETIC_OR_HEXEN}
      if strtrim(mobj.flags2) = '' then
        mobj.flags := '0';
      {$ENDIF}
      if strtrim(mobj.flags_ex) = '' then
        mobj.flags_ex := '0';
      if strtrim(mobj.flags2_ex) = '' then
        mobj.flags2_ex := '0';
      if strtrim(mobj.flags3_ex) = '' then
        mobj.flags3_ex := '0';
      if strtrim(mobj.flags4_ex) = '' then
        mobj.flags4_ex := '0';

      numstates := 0;

      while sc.GetString do
      begin
        if sc.MatchString('ACTOR') or
           sc.MatchString('ACTORALIAS') or
           sc.MatchString('DEH_PARSE') or
           sc.MatchString('DEH_PARSE_ALL') or
           sc.MatchString('COMPILED') or
           sc.MatchString('PRECOMPILED') or
           sc.MatchString('EXTERNAL') or
           sc.MatchString('SCRIPT') or
           sc.MatchString('THINKER') or
           sc.MatchString('GLOBAL') then
        begin
          SubmitParsedData;
          sc.UnGet;
          break;
        end;
        if sc.MatchString('spawn:') then
        begin
          mobj.statesdefined := mobj.statesdefined or RTL_ST_SPAWN;
          mobj.spawnstate := numstates;
          repeat until not ParseState(mobj.spawnstate);
        end
        else if sc.MatchString('see:') then
        begin
          mobj.statesdefined := mobj.statesdefined or RTL_ST_SEE;
          mobj.seestate := numstates;
          repeat until not ParseState(mobj.seestate);
        end
        else if sc.MatchString('heal:') then
        begin
          mobj.statesdefined := mobj.statesdefined or RTL_ST_HEAL;
          mobj.healstate := numstates;
          repeat until not ParseState(mobj.healstate);
        end
        else if sc.MatchString('crash:') then
        begin
          mobj.statesdefined := mobj.statesdefined or RTL_ST_CRASH;
          mobj.crashstate := numstates;
          repeat until not ParseState(mobj.crashstate);
        end
        {$IFDEF DOOM_OR_STRIFE}
        else if sc.MatchString('interact:') then
        begin
          mobj.statesdefined := mobj.statesdefined or RTL_ST_INTERACT;
          mobj.interactstate := numstates;
          repeat until not ParseState(mobj.interactstate);
        end
        {$ENDIF}
        else if sc.MatchString('melee:') then
        begin
          mobj.statesdefined := mobj.statesdefined or RTL_ST_MELEE;
          mobj.meleestate := numstates;
          repeat until not ParseState(mobj.meleestate);
        end
        else if sc.MatchString('missile:') then
        begin
          mobj.statesdefined := mobj.statesdefined or RTL_ST_MISSILE;
          mobj.missilestate := numstates;
          repeat until not ParseState(mobj.missilestate);
        end
        else if sc.MatchString('pain:') then
        begin
          mobj.statesdefined := mobj.statesdefined or RTL_ST_PAIN;
          mobj.painstate := numstates;
          repeat until not ParseState(mobj.painstate);
        end
        else if sc.MatchString('death:') then
        begin
          mobj.statesdefined := mobj.statesdefined or RTL_ST_DEATH;
          mobj.deathstate := numstates;
          repeat until not ParseState(mobj.deathstate);
        end
        else if sc.MatchString('xdeath:') then
        begin
          mobj.statesdefined := mobj.statesdefined or RTL_ST_XDEATH;
          mobj.xdeathstate := numstates;
          repeat until not ParseState(mobj.xdeathstate);
        end
        else if sc.MatchString('raise:') then
        begin
          mobj.statesdefined := mobj.statesdefined or RTL_ST_RAISE;
          mobj.raisestate := numstates;
          repeat until not ParseState(mobj.raisestate);
        end
      end;

    end;

  end;

  SubmitParsedData;

  sc.Free;
  state_tokens.Free;
end;

procedure SC_ParseActordefLump(const in_text: string);
begin
  SC_DoParseActordefLump(SC_Preprocess(in_text, false));
end;

var
  sound_tx: string;

procedure SC_DoRetrieveSndInfo(const in_text: string);
begin
  if sound_tx = '' then
    sound_tx := in_text
  else
    sound_tx := sound_tx + #13#10 + in_text;
end;

procedure SC_RetrieveSndInfo(const in_text: string);
begin
  SC_DoRetrieveSndInfo(SC_Preprocess(in_text, false));
end;

procedure SC_ParseSndInfoLumps;
var
  i, p: integer;
  s: TDStringList;
  str1, str2, stmp: string;
  {$IFDEF HEXEN}
  j, mapid: integer;
  {$ENDIF}
begin
// JVAL
// Retrive sndinfo lumps for sound alias list
  sound_tx := '';
  for i := 0 to W_NumLumps - 1 do
  begin
    if char8tostring(W_GetNameForNum(i)) = SNDINFOLUMPNAME then
    begin
      if sound_tx = '' then
        sound_tx := W_TextLumpNum(i)
      else
        sound_tx := sound_tx + #13#10 + W_TextLumpNum(i);
    end;
  end;

  PAK_StringIterator(SNDINFOLUMPNAME, SC_RetrieveSndInfo);
  PAK_StringIterator(SNDINFOLUMPNAME + '.txt', SC_RetrieveSndInfo);

  s := TDStringList.Create;
  try
    s.Text := fixsndaliasstr(sound_tx);
    for i := 0 to s.Count - 1 do
    begin
      stmp := strtrim(s[i]);
      p := Pos('//', stmp);
      if p > 0 then
        stmp := Copy(stmp, 1, p - 1);
      p := Pos(';', stmp);
      if p > 0 then
        stmp := Copy(stmp, 1, p - 1);
      if stmp <> '' then
      begin
        splitstring(stmp, str1, str2, [' ', '=', Chr($09)]);
        str2 := strtrim(str2);
        if str2 <> '' then
        begin
          {$IFDEF HEXEN}
          // JVAL
          // Check for MAP song name
          if str1 = '$MAP' then
          begin
            stmp := str2;
            splitstring(stmp, str1, str2, [' ', '=', Chr($09)]);
            if str1 <> '0' then
            begin
              mapid := atoi(str1, -1);
              if (mapid > 0) and (mapid < nummusic) then
                S_music[mapid].name := str2;
            end;
          end
          else
          begin
          {$ENDIF}
          str1 := strtrim(str1);
          soundaliases.Add('%s=%s', [str1, str2]);
          {$IFDEF HEXEN}
            for j := 1 to Ord(DO_NUMSFX) - 1 do
              if strupper(S_sfx[j].tagname) = str1 then
                S_sfx[j].name := str2;
          end;
          {$ENDIF}
        end;
      end;
    end;
  finally
    s.Free;
  end;
end;

procedure SC_ParseActordefLumps;
var
  i: integer;
  lumplist: TDNumberList;
begin
  SC_ParseStatedefLump;

  {$IFNDEF FPC}
  SUC_Disable;
  {$ENDIF}

// Retrive actordef lumps
  lumplist := TDNumberList.Create;
  for i := 0 to W_NumLumps - 1 do
    if char8tostring(W_GetNameForNum(i)) = ACTORDEFLUMPNAME then
      lumplist.Add(i);

  {$IFNDEF FPC}
  SUC_SecondaryProgressInit(lumplist.Count);
  {$ENDIF}

  for i := 0 to lumplist.Count - 1 do
  begin
    {$IFNDEF FPC}
    SUC_SecondaryProgress(i);
    {$ENDIF}
    SC_ParseActordefLump(W_TextLumpNum(lumplist[i]));
  end;

  {$IFNDEF FPC}
  SUC_SecondaryProgressDone;
  {$ENDIF}
  PAK_StringIterator(ACTORDEFLUMPNAME, SC_ParseActordefLump);
  PAK_StringIterator(ACTORDEFLUMPNAME + '.txt', SC_ParseActordefLump);

  lumplist.Free;

  {$IFNDEF FPC}
  SUC_Enable;
  {$ENDIF}
end;

procedure SC_Init;
begin
  soundaliases := TDStringList.Create;
  statenames := TTokenList.Create;
  SC_InitActorEvaluator;
end;

procedure SC_ShutDown;
begin
  soundaliases.Free;
  statenames.Free;
  SC_ShutDownActorEvaluator;
end;

end.

