//------------------------------------------------------------------------------
//
//  DelphiDoom is a source port of the game Doom and it is
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 1993-1996 by id Software, Inc.
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
//    Thing frame/state LUT,
//    generated by multigen utilitiy.
//    This one is the original DOOM version, preserved.
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

// JVAL: Needed for model definition

unit sc_states;

interface

uses
  p_mobj_h,
  sc_tokens;

var
  statenames: TTokenList;

//==============================================================================
//
// SC_ParseStatedefLump
//
//==============================================================================
procedure SC_ParseStatedefLump;

//==============================================================================
//
// SC_DefaultStatedefLump
//
//==============================================================================
procedure SC_DefaultStatedefLump;

//==============================================================================
//
// P_GetStateFromName
//
//==============================================================================
function P_GetStateFromName(const actor: Pmobj_t; const s1: string): integer;

//==============================================================================
//
// P_GetStateFromNameWithOffsetCheck
//
//==============================================================================
function P_GetStateFromNameWithOffsetCheck(const actor: Pmobj_t; const s1: string): integer;

//==============================================================================
//
// SC_FillStateNames
//
//==============================================================================
procedure SC_FillStateNames;

implementation

uses
  TypInfo,
  d_delphi,
  info_h,
  info,
  info_common,
  sc_engine,
  sc_params,
  w_wad;

const
  STATEDEFLUMPNAME = 'STATEDEF';

var
  default_states_added: boolean = false;

//==============================================================================
//
// SC_DefaultStatedefLump
//
//==============================================================================
procedure SC_DefaultStatedefLump;
var
  st: statenum_t;
begin
  if statenames = nil then
    statenames := TTokenList.Create;
  if not default_states_added then
  begin
    for st := statenum_t(0) to statenum_t(Ord(DO_NUMSTATES) - 1) do
      statenames.Add(strupper(GetENumName(TypeInfo(statenum_t), Ord(st))));
    default_states_added := true;
  end;
end;

//==============================================================================
//
// SC_ParseStatedefLump
//
//==============================================================================
procedure SC_ParseStatedefLump;
var
  i: integer;
  sc: TScriptEngine;
  found: boolean;
  x: integer;

  procedure _newstatealias(const id: integer; const alias: string);
  var
    tid: integer;
  begin
    if statenames.Count > id then
    begin
      if strupper(statenames.Strings[id]) = strupper(alias) then
        exit;
      tid := statenames.IndexOfToken(alias);
      if tid = id then
        exit;
    end;
    statenames.Add(alias);
  end;

begin
  found := false;
  for i := 0 to W_NumLumps - 1 do
    if char8tostring(W_GetNameForNum(i)) = STATEDEFLUMPNAME then
    begin
      found := true;
      sc := TScriptEngine.Create(W_TextLumpNum(i));
      x := 0;
      while sc.GetString do
      begin
        _newstatealias(x, strupper(sc._String));
        inc(x);
      end;
//        statenames.Add(strupper(sc._String));
      sc.Free;
      break;
    end;

  // JVAL: Patch for stand alone script compiler
  if not found then
    SC_DefaultStatedefLump;
end;

//==============================================================================
//
// P_GetStateFromName
//
//==============================================================================
function P_GetStateFromName(const actor: Pmobj_t; const s1: string): integer;
var
  s, st: string;
  fw, sw: string;
  pps, ppp, ppb: integer;

  function _stindex(const sss: string): integer;
  var
    sss1, sss2: string;
    p, idx: integer;
    inf: Pmobjinfo_t;
  begin
    result := statenames.IndexOfToken(sss);
    if result >= 0 then
      exit;

    sss1 := strupper(sss);

    p := Pos('::', sss1);
    if p < 2 then // eg allow "goto ::spawn"
      inf := actor.info
    else
    begin
      sss2 := strtrim(Copy(sss1, 1, p - 1));
      sss1 := Copy(sss1, p + 2, Length(sss1) - p - 3);
      if sss2 = 'SUPER' then
        idx := actor.info.inheritsfrom
      else
        idx := Info_GetMobjNumForName(sss2);
      if (idx >= 0) and (idx < nummobjtypes) then
        inf := @mobjinfo[idx]
      else if sss2 = 'SELF' then // eg allow "goto self::spawn"
        inf := actor.info
      else
        inf := nil;
    end;

    if inf <> nil then
    begin
      if sss1 = 'SPAWN' then
      begin
        result := inf.spawnstate;
        exit;
      end
      else if sss1 = 'SEE' then
      begin
        result := inf.seestate;
        exit;
      end
      else if sss1 = 'MELEE' then
      begin
        result := inf.meleestate;
        exit;
      end
      else if sss1 = 'MISSILE' then
      begin
        result := inf.missilestate;
        exit;
      end
      else if sss1 = 'MISSILE' then
      begin
        result := inf.missilestate;
        exit;
      end
      else if sss1 = 'PAIN' then
      begin
        result := inf.painstate;
        exit;
      end
      else if sss1 = 'DEATH' then
      begin
        result := inf.deathstate;
        exit;
      end
      else if sss1 = 'XDEATH' then
      begin
        result := inf.xdeathstate;
        exit;
      end
      else if sss1 = 'RAISE' then
      begin
        result := inf.raisestate;
        exit;
      end
      else if sss1 = 'CRASH' then
      begin
        result := inf.crashstate;
        exit;
      end
      else if sss1 = 'CRUSH' then
      begin
        result := inf.crushstate;
        exit;
      end
      else if sss1 = 'INTERACT' then
      begin
        result := inf.interactstate;
        exit;
      end
    end;

    sss1 := 'S_' + strupper(actor.info.name) + '_' + sss;
    result := statenames.IndexOfToken(sss1);
    if result < 0 then
      if StrIsLongWord(s1) then
        result := atoi(s1);
  end;

begin
  s := SC_EvalString(s1);
  st := strupper(strtrim(s));
  pps := CharPos('+', st);
  ppp := CharPos('-', st);
  ppb := CharPos(' ', st);
  if (ppb = 0) and (ppp = 0) and (pps = 0) then
  begin
    Result := _stindex(st);
    Exit;
  end
  else
  // JVAL: 20170927 evaluate small expressions
  //       20191003 rewritten, fixed
  begin
    st := strremovespaces(st);
    pps := CharPos('+', st);
    ppp := CharPos('-', st);
    if pps > 0 then
    begin
      splitstring_ch(st, fw, sw, '+');
      Result := _stindex(fw) + atoi(sw, 0);
      Exit;
    end;
    if ppp > 0 then
    begin
      splitstring_ch(st, fw, sw, '-');
      Result := _stindex(fw) - atoi(sw, 0);
      Exit;
    end;
  end;
  Result := -1; // JVAL: No match
end;

//==============================================================================
//
// P_GetStateFromNameWithOffsetCheck
//
//==============================================================================
function P_GetStateFromNameWithOffsetCheck(const actor: Pmobj_t; const s1: string): integer;
var
  s: string;
  check: string;
begin
  s := SC_EvalString(s1);
  check := s;
  if check = '' then
  begin
    Result := 0;
    Exit;
  end;

  if check[1] in ['-', '+'] then
    Delete(check, 1, 1);

  if StrIsLongWord(check) then
    Result := ((integer(actor.state) - integer(states)) div SizeOf(state_t)) + atoi(s)
  else
    Result := P_GetStateFromName(actor, s);
end;

//==============================================================================
//
// SC_FillStateNames
//
//==============================================================================
procedure SC_FillStateNames;
begin
  while statenames.Count < numstates do
    statenames.Add('S_' + itoa(statenames.Count));
end;

end.

