//------------------------------------------------------------------------------
//
//  DelphiDoom: A modified and improved DOOM engine for Windows
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 2004-2017 by Jim Valavanis
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
//  E-Mail: jimmyvalavanis@yahoo.gr
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit sc_actordef;

interface

uses
  d_delphi;

procedure SC_ParseActordefLumps;

procedure SC_ParseActordefLump(const in_text: string);

procedure SC_Init;

procedure SC_ShutDown;

function SC_SoundAlias(const snd: string): string;

implementation

uses
  d_main,
  deh_main,
  m_fixed,
  i_system,
  {$IFNDEF FPC}
  i_startup,
  {$ENDIF}
  info,
  info_h,
  rtl_types,
  sc_engine,
  sc_states,
  sc_tokens,
  sc_thinker,
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
  public
    function MatchFlag(const flag: string): boolean;
    function MatchFlag2(const flag: string): boolean;
    function MatchFlagEx(const flag_ex: string): boolean;
    function MatchFlag2Ex(const flag2_ex: string): boolean;
  end;

function TActordefScriptEngine.MatchFlag(const flag: string): boolean;
begin
  result := MatchString(flag) or MatchString('+' + flag) or MatchString('MF_' + flag);
end;

function TActordefScriptEngine.MatchFlag2(const flag: string): boolean;
begin
  result := MatchString(flag) or MatchString('+' + flag) or MatchString('MF_' + flag) or MatchString('MF2_' + flag);
end;

function TActordefScriptEngine.MatchFlagEx(const flag_ex: string): boolean;
begin
  result := MatchString(flag_ex) or MatchString('+' + flag_ex) or MatchString('MF_' + flag_ex) or MatchString('MF_EX_' + flag_ex);
end;

function TActordefScriptEngine.MatchFlag2Ex(const flag2_ex: string): boolean;
begin
  result := MatchString(flag2_ex) or MatchString('+' + flag2_ex) or MatchString('MF2_' + flag2_ex) or MatchString('MF2_EX_' + flag2_ex);
end;


const
  ORIGINALSTATEMARKER = $FFFFF;
  ACTORDEFLUMPNAME = 'ACTORDEF';
  SNDINFOLUMPNAME = 'SNDINFO';

procedure SC_ParseActordefLump(const in_text: string);
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
    finished: boolean;
    p: integer;
    alias: string;
  begin
    result := false;
    if sc._Finished then
      exit;
    sc.GetString;
    if sc.MatchString('loop') then
    begin
      m_states[numstates - 1].nextstate := base;
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
      if Pos('SPAWN', gotostr) = 1 then
      begin
        if length(gotostr) > 5 then
          offs := atoi(strremovespaces(Copy(gotostr, 6, Length(gotostr) - 5)))
        else
          offs := 0;
        m_states[numstates - 1].nextstate := mobj.spawnstate + offs;
        m_states[numstates - 1].has_goto := true;
      end
      else if Pos('SEE', gotostr) = 1 then
      begin
        if length(gotostr) > 3 then
          offs := atoi(strremovespaces(Copy(gotostr, 4, Length(gotostr) - 3)))
        else
          offs := 0;
        m_states[numstates - 1].nextstate := mobj.seestate + offs;
        m_states[numstates - 1].has_goto := true;
      end
      else if Pos('MELEE', gotostr) = 1 then
      begin
        if length(gotostr) > 5 then
          offs := atoi(strremovespaces(Copy(gotostr, 6, Length(gotostr) - 5)))
        else
          offs := 0;
        m_states[numstates - 1].nextstate := mobj.meleestate + offs;
        m_states[numstates - 1].has_goto := true;
      end
      else if Pos('MISSILE', gotostr) = 1 then
      begin
        if length(gotostr) > 7 then
          offs := atoi(strremovespaces(Copy(gotostr, 8, Length(gotostr) - 7)))
        else
          offs := 0;
        m_states[numstates - 1].nextstate := mobj.missilestate + offs;
        m_states[numstates - 1].has_goto := true;
      end
      else if Pos('PAIN', gotostr) = 1 then
      begin
        if length(gotostr) > 4 then
          offs := atoi(strremovespaces(Copy(gotostr, 5, Length(gotostr) - 4)))
        else
          offs := 0;
        m_states[numstates - 1].nextstate := mobj.painstate + offs;
        m_states[numstates - 1].has_goto := true;
      end
      else if Pos('DEATH', gotostr) = 1 then
      begin
        if length(gotostr) > 5 then
          offs := atoi(strremovespaces(Copy(gotostr, 6, Length(gotostr) - 5)))
        else
          offs := 0;
        m_states[numstates - 1].nextstate := mobj.deathstate + offs;
        m_states[numstates - 1].has_goto := true;
      end
      else if Pos('XDEATH', gotostr) = 1 then
      begin
        if length(gotostr) > 6 then
          offs := atoi(strremovespaces(Copy(gotostr, 7, Length(gotostr) - 6)))
        else
          offs := 0;
        m_states[numstates - 1].nextstate := mobj.xdeathstate + offs;
        m_states[numstates - 1].has_goto := true;
      end
      else if Pos('RAISE', gotostr) = 1 then
      begin
        if length(gotostr) > 5 then
          offs := atoi(strremovespaces(Copy(gotostr, 6, Length(gotostr) - 5)))
        else
          offs := 0;
        m_states[numstates - 1].nextstate := mobj.raisestate + offs;
        m_states[numstates - 1].has_goto := true;
      end
      else if Pos('HEAL', gotostr) = 1 then
      begin
        if length(gotostr) > 4 then
          offs := atoi(strremovespaces(Copy(gotostr, 5, Length(gotostr) - 4)))
        else
          offs := 0;
        m_states[numstates - 1].nextstate := mobj.healstate + offs;
        m_states[numstates - 1].has_goto := true;
      end
      else if Pos('CRASH', gotostr) = 1 then
      begin
        if length(gotostr) > 5 then
          offs := atoi(strremovespaces(Copy(gotostr, 6, Length(gotostr) - 5)))
        else
          offs := 0;
        m_states[numstates - 1].nextstate := mobj.crashstate + offs;
        m_states[numstates - 1].has_goto := true;
      end
      {$IFDEF DOOM_OR_STRIFE}
      else if Pos('INTERACT', gotostr) = 1 then
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
        I_Warning('SC_ActordefToDEH(): Unknown label "goto %s"'#13#10, [gotostr]);
        exit;
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
    if alias <> '' then
    begin
      if alias[Length(alias)] = ':' then
      begin
        sc.GetString;
        SetLength(alias, Length(alias) - 1);
        alias := 'S_' + strupper(mobj.name + '_' + alias);
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
      stmp := '';
      finished := false;
      while not sc.NewLine and not finished do
      begin
        if stmp <> '' then
          stmp := stmp + ' ';
        if sc.ParenthesisLevel = 1 then
          stmp := stmp + '"' + sc._string + '"'
        else
          stmp := stmp + sc._string;
        finished := not sc.GetString;
      end;
      if not finished then
        sc.UnGet;

      bright := false;
      stmp := strtrim(stmp);  // JVAL for safety, unneeded
      for i := 1 to length(stmp) do
        if stmp[i] in ['(', ')',','] then
          stmp[i] := ' ';
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
        alias := ''; // Alias only for the first state
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

  procedure SubmitParsedData;
  var
    cnt: integer;
    stateprefix: string;
  begin
    if not actorpending then
      Exit;
    actorpending := false;
    res := '';
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
    if ismissile then
      if mobj.speed < 2048 then // JVAL fix me
        mobj.speed := mobj.speed * FRACUNIT;
    AddRes('Speed = ' + itoa(mobj.speed));
    AddRes('Width = ' + itoa(mobj.radius * $10000));
    AddRes('Height = ' + itoa(mobj.height * $10000));
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
    {$IFDEF DOOM_OR_STRIFE}
    AddRes('missileheight = ' + itoa(mobj.missileheight));
    {$ENDIF}
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
        if m_states[cnt].nextstate >= ORIGINALSTATEMARKER then
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
    DEH_Parse(res);
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
          while (strupper(firstword(passcriptline, [' ', ';', '.'])) <> 'ENDSCRIPT') and (not sc._Finished) do
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

var
  slist: TDStringList;
  stmp: string;
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
      sc.MustGetInteger;
      th.dn := sc._Integer;
      sc.MustGetString;
      if strupper(sc._String) <> 'SCRIPT' then
      begin
        I_Warning('SC_ActordefToDEH(): Unknown token "%s" found, "SCRIPT" expected'#13#10, [sc._String]);
        sc.UnGet;
        Continue;
      end;
      sc.MustGetString;
      th.script := sc._String;

      if not sc.GetString then
      begin
        th.repeatcnt := 0;
        SC_SubmitThinker(@th);
        Break;
      end;

      if strupper(sc._String) <> 'REPEAT' then
      begin
        th.repeatcnt := 0;
        SC_SubmitThinker(@th);
        sc.UnGet;
        Continue;
      end;

      sc.MustGetString;
      if strupper(sc._String) = 'FOREVER' then
        th.repeatcnt := 0
      else
        th.repeatcnt := atoi(sc._String);

      SC_SubmitThinker(@th);
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

    if sc.MatchString('ACTOR') then
    begin
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
      {$IFDEF STRIFE}
      mobj.name2 := '';
      {$ENDIF}
      ismissile := false;
      FillChar(m_states, SizeOf(m_states), 0);
      sc.GetString;
      mobj.name := sc._String;
      if Pos(':', mobj.name) = Length(mobj.name) then
      begin
        SetLength(mobj.name, Length(mobj.name) - 1);
        sc.GetString;
        mobj.inheritsfrom := sc._string;
      end
      else
      begin
        if not sc.GetString then
          break;

        if sc.MatchString('replaces') or sc.MatchString(':') or
           sc.MatchString('inherits') or sc.MatchString('inheritsfrom') then
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
          mobj.customsound1 := itoa(pinf.customsound1);
          mobj.customsound2 := itoa(pinf.customsound2);
          mobj.customsound3 := itoa(pinf.customsound3);
          mobj.dropitem := itoa(pinf.dropitem);
          mobj.missiletype := itoa(pinf.missiletype);
          mobj.explosiondamage := pinf.explosiondamage;
          mobj.explosionradius := pinf.explosionradius;
          mobj.meleedamage := pinf.meleedamage;
          mobj.meleesound := itoa(pinf.meleesound);
          {$IFDEF DOOM_OR_STRIFE}
          mobj.missileheight := pinf.missileheight;
          {$ENDIF}

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

          mobj.renderstyle := itoa(Ord(pinf.renderstyle));
          mobj.alpha := pinf.alpha;

        end;

        if not sc.GetString then
          break;
      end;

      if sc.NewLine then
      begin
        mobj.doomednum := -1;
      end
      else
      begin
        mobj.doomednum := atoi(sc._string);
        if not sc.GetString then
          break;
      end;

      foundstates := false;
      repeat
        {$IFDEF STRIFE}
        if sc.MatchString('name') then
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
        else if sc.MatchString('monster') then
        begin
           mobj.flags := mobj.flags + 'MF_SOLID MF_SHOOTABLE MF_COUNTKILL ';
           sc.GetString;
        end

        else if sc.MatchString('RENDERSTYLE') then
        begin
          sc.GetString;
          mobj.renderstyle := sc._String;
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
          if mobj.name <> '' then
            stmp := ' while parsing mobj "' + mobj.name + '"'
          else
            stmp := '';
          if mobj.doomednum > -1 then
            stmp := stmp + ' (doomednum=' + itoa(mobj.doomednum) + ')';
          I_Warning('SC_ActordefToDEH(): Unknown token "%s" found%s'#13#10, [sc._String, stmp]);
          sc.GetString;
        end;
      until foundstates or sc._Finished;

      if mobj.flags = '' then
        mobj.flags := '0';
      if mobj.flags_ex = '' then
        mobj.flags_ex := '0';
      if mobj.flags2_ex = '' then
        mobj.flags2_ex := '0';

      numstates := 0;

      while sc.GetString do
      begin
        if sc.MatchString('ACTOR') or
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
          mobj.spawnstate := numstates;
          repeat until not ParseState(mobj.spawnstate);
        end
        else if sc.MatchString('see:') then
        begin
          mobj.seestate := numstates;
          repeat until not ParseState(mobj.seestate);
        end
        else if sc.MatchString('heal:') then
        begin
          mobj.healstate := numstates;
          repeat until not ParseState(mobj.healstate);
        end
        else if sc.MatchString('crash:') then
        begin
          mobj.crashstate := numstates;
          repeat until not ParseState(mobj.crashstate);
        end
        {$IFDEF DOOM_OR_STRIFE}
        else if sc.MatchString('interact:') then
        begin
          mobj.interactstate := numstates;
          repeat until not ParseState(mobj.interactstate);
        end
        {$ENDIF}
        else if sc.MatchString('melee:') then
        begin
          mobj.meleestate := numstates;
          repeat until not ParseState(mobj.meleestate);
        end
        else if sc.MatchString('missile:') then
        begin
          mobj.missilestate := numstates;
          repeat until not ParseState(mobj.missilestate);
        end
        else if sc.MatchString('pain:') then
        begin
          mobj.painstate := numstates;
          repeat until not ParseState(mobj.painstate);
        end
        else if sc.MatchString('death:') then
        begin
          mobj.deathstate := numstates;
          repeat until not ParseState(mobj.deathstate);
        end
        else if sc.MatchString('xdeath:') then
        begin
          mobj.xdeathstate := numstates;
          repeat until not ParseState(mobj.xdeathstate);
        end
        else if sc.MatchString('raise:') then
        begin
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

procedure SC_ParseActordefLumps;
var
  i: integer;
  sound_tx: string;
  s: TDStringList;
  stmp: string;
  str1, str2: string;
  p: integer;
  {$IFDEF HEXEN}
  j, mapid: integer;
  {$ENDIF}
  lumplist: TDNumberList;
begin
  SC_ParseStatedefLump;
// JVAL
// Retrive sndinfo lumps for sound alias list
  sound_tx := '';
  for i := 0 to W_NumLumps - 1 do
  begin
    if char8tostring(W_GetNameForNum(i)) = SNDINFOLUMPNAME then
      sound_tx := sound_tx + W_TextLumpNum(i);
  end;

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
              if (mapid > 0) and (mapid < Ord(NUMMUSIC)) then
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
end;

procedure SC_ShutDown;
begin
  soundaliases.Free;
  statenames.Free;
end;

end.

