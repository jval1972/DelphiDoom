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
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit info_common;

interface

uses
  d_delphi,
  info_h;

function Info_GetMobjNumForDoomNum(const dn: integer): integer;

procedure Info_InitDnLookUp;

procedure Info_ShutDownDnLookUp;

procedure Info_CheckStates;

function Info_GetNewState: integer;

function Info_GetNewMobjInfo: integer;

function Info_GetSpriteNumForName(const name: string): integer;

function Info_GetSpriteNameForNum(const id: integer): string;

function Info_CheckSpriteNumForName(const name: string): integer;

function Info_GetMobjNumForName(const name: string): integer;

procedure Info_SetMobjName(const mobj_no: integer; const name: string);

function Info_GetMobjName(const mobj_no: integer): string; overload;

function Info_GetMobjName(const minfo: Pmobjinfo_t): string; overload;

procedure Info_ShutDown;

function Info_GetInheritance(const imo: Pmobjinfo_t): integer;

function Info_AddMobjNameAlias(const alias1: string; const alias2: string): boolean;

function Info_GetStatesForMobjInfo(const mobjno: integer): TDNumberList;

procedure Info_AddStateOwner(const st: Pstate_t; const moidx: integer);

procedure Info_InitStateOwners;

function Info_ResolveMobjType(const name: string; const mt: PInteger): boolean;

procedure Info_SaveActions;

function Info_RestoreActions: boolean;

implementation

uses
  d_think,
  i_system,
  m_fixed,
  p_spec,
  info;

var
  dnLookUp: PIntegerArray = nil; // JVAL: Doom Editor Number LookUp

const
  DNLOOKUPSIZE = $10000;

var
  mobjinfo_aliases: TDStringList = nil;


procedure Info_InitDnLookUp;
begin
  if dnLookUp = nil then
    dnLookUp := mallocz(DNLOOKUPSIZE * SizeOf(integer));

  if mobjinfo_aliases = nil then
    mobjinfo_aliases := TDStringList.Create;
end;

procedure Info_ShutDownDnLookUp;
begin
  memfree(pointer(dnLookUp), DNLOOKUPSIZE * SizeOf(integer));
  mobjinfo_aliases.Free;
  mobjinfo_aliases := nil;
end;

function Info_AddMobjNameAlias(const alias1: string; const alias2: string): boolean;
var
  num1, num2: integer;
  str: string;
begin
  if mobjinfo_aliases = nil then
  begin
    result := false;
    exit;
  end;
  num1 := Info_GetMobjNumForName(alias1);
  if num1 >= 0 then
  begin
    str := strupper(strremovespaces(alias2)) + '=' + itoa(num1);
    if mobjinfo_aliases.IndexOf(str) < 0 then
      mobjinfo_aliases.Add(str);
  end;
  num2 := Info_GetMobjNumForName(alias2);
  if num2 >= 0 then
  begin
    str := strupper(strremovespaces(alias1)) + '=' + itoa(num2);
    if mobjinfo_aliases.IndexOf(str) < 0 then
      mobjinfo_aliases.Add(str);
  end;
  result := (num1 >= 0) or (num2 >= 0);
end;

function Info_GetMobjNumForAlias(const name: string): integer;
var
  i: integer;
  check: string;
  snum: string;
begin
  if mobjinfo_aliases = nil then
  begin
    result := -1;
    exit;
  end;

  check := strupper(strremovespaces(name)) + '=';
  for i := mobjinfo_aliases.Count - 1 downto 0 do
  begin
    if Pos(check, mobjinfo_aliases.Strings[i]) = 1 then
    begin
      splitstring(mobjinfo_aliases.Strings[i], check, snum, '=');
      result := atoi(snum);
      exit;
    end;
  end;

  result := -1;
end;

function Info_GetMobjNumForDoomNum(const dn: integer): integer;
var
  i: integer;
  idx: integer;
begin
  if dnLookUp <> nil then
  begin
    idx := dn mod DNLOOKUPSIZE;
    if (idx >= 0) and (idx < DNLOOKUPSIZE) then
    begin
      idx := dnLookUp[idx];
      if (idx >= 0) and (idx < nummobjtypes) then
        if mobjinfo[idx].doomednum = dn then
        begin
          result := idx;
          Exit;
        end;
    end;
  end;

  // JVAL 20201215
  // Changed traverse order of mobjinfo table.
  // For custom content [Ord(DO_NUMMOBJTYPES)..nummobjtypes - 1] the search is
  // done backwards.
  // For build-in content [0..Ord(DO_NUMMOBJTYPES) - 1] the search is done
  // forward

  // First Backward search for custom content
  for i := nummobjtypes - 1 downto Ord(DO_NUMMOBJTYPES) do
  begin
    if mobjinfo[i].doomednum = dn then
    begin
      result := i;
      if dnLookUp <> nil then
        dnLookUp[dn mod DNLOOKUPSIZE] := i;
      Exit;
    end;
  end;

  // Forward search for build-in content
  for i := 1 to Ord(DO_NUMMOBJTYPES) - 1 do
  begin
    if mobjinfo[i].doomednum = dn then
    begin
      result := i;
      if dnLookUp <> nil then
        dnLookUp[dn mod DNLOOKUPSIZE] := i;
      Exit;
    end;
  end;
  result := -1;
end;

procedure Info_CheckStates;
var
  i: integer;
  loops: integer;
  st: Pstate_t;
begin
  for i := 0 to numstates - 1 do
  begin
    loops := 0;
    st := @states[i];
    repeat
      if st.nextstate = S_NULL then
        break;
      st := @states[Ord(st.nextstate)];
      inc(loops);
      if loops > $FFFF then
      begin
        I_Warning('Info_CheckStates(): State %d has possible infinite loop.'#13#10, [i]);
        break;
      end;
    until st.tics <> 0;
  end;
end;

function Info_GetNewState: integer;
begin
  realloc(pointer(states), numstates * SizeOf(state_t), (numstates + 1) * SizeOf(state_t));
  ZeroMemory(@states[numstates], SizeOf(state_t));
  result := numstates;
  inc(numstates);
end;

function Info_GetNewMobjInfo: integer;
begin
  realloc(pointer(mobjinfo), nummobjtypes * SizeOf(mobjinfo_t), (nummobjtypes + 1) * SizeOf(mobjinfo_t));
  ZeroMemory(@mobjinfo[nummobjtypes], SizeOf(mobjinfo_t));
  {$IFDEF STRIFE}
  mobjinfo[nummobjtypes].name2 := '';
  {$ENDIF}
  mobjinfo[nummobjtypes].inheritsfrom := -1; // Set to -1
  mobjinfo[nummobjtypes].doomednum := -1; // Set to -1
  mobjinfo[nummobjtypes].pushfactor := DEFPUSHFACTOR;
  mobjinfo[nummobjtypes].friction := ORIG_FRICTION;
  mobjinfo[nummobjtypes].scale := FRACUNIT;
  mobjinfo[nummobjtypes].gravity := FRACUNIT;
  result := nummobjtypes;
  inc(nummobjtypes);
end;

function Info_GetSpriteNumForName(const name: string): integer;
var
  spr_name: string;
  i: integer;
  check: integer;
begin
  result := atoi(name, -1);

  if (result >= 0) and (result < numsprites) and (itoa(result) = name) then
    exit;


  if Length(name) <> 4 then
    I_Error('Info_GetSpriteNumForName(): Sprite name "%s" must have 4 characters', [name]);

  spr_name := strupper(name);

  check := Ord(spr_name[1]) +
           Ord(spr_name[2]) shl 8 +
           Ord(spr_name[3]) shl 16 +
           Ord(spr_name[4]) shl 24;

  for i := 0 to numsprites - 1 do
    if sprnames[i] = check then
    begin
      result := i;
      exit;
    end;

  result := numsprites;

  sprnames[numsprites] := check;
  inc(numsprites);
  realloc(pointer(sprnames), numsprites * 4, (numsprites + 1) * 4);
  sprnames[numsprites] := 0;
end;

function Info_GetSpriteNameForNum(const id: integer): string;
var
  check: LongWord;
begin
  result := '';
  if (id < 0) or (id >= numsprites) then
    exit;

  check := PLongWord(@sprnames[id])^;
  result := Chr(check and $FF) + Chr((check shr 8) and $FF) + Chr((check shr 16) and $FF) + Chr((check shr 24) and $FF);
end;

function Info_CheckSpriteNumForName(const name: string): integer;
var
  spr_name: string;
  i: integer;
  check: integer;
begin
  result := atoi(name, -1);

  if (result >= 0) and (result < numsprites) and (itoa(result) = name) then
    exit;


  if Length(name) <> 4 then
    I_Error('Info_CheckSpriteNumForName(): Sprite name "%s" must have 4 characters', [name]);

  spr_name := strupper(name);

  check := Ord(spr_name[1]) +
           Ord(spr_name[2]) shl 8 +
           Ord(spr_name[3]) shl 16 +
           Ord(spr_name[4]) shl 24;

  for i := 0 to numsprites - 1 do
    if sprnames[i] = check then
    begin
      result := i;
      exit;
    end;

  result := -1;
end;

function Info_GetMobjNumForName(const name: string): integer;
var
  mobj_name: string;
  check: string;
  i: integer;
begin
  if (name = '') or (name = '-1') then
  begin
    result := -1;
    exit;
  end;

  result := atoi(name, -1);

  if (result >= 0) and (result < nummobjtypes) and (itoa(result) = name) then
    exit;

  mobj_name := strupper(strtrim(name));
  if Length(mobj_name) > MOBJINFONAMESIZE then
    SetLength(mobj_name, MOBJINFONAMESIZE);
  for i := nummobjtypes - 1 downto 0 do
  begin
    check := Info_GetMobjName(i);
    check := strupper(strtrim(check));
    if check = mobj_name then
    begin
      result := i;
      exit;
    end;
  end;

  mobj_name := strremovespaces(strupper(strtrim(name)));
  if Length(mobj_name) > MOBJINFONAMESIZE then
    SetLength(mobj_name, MOBJINFONAMESIZE);
  for i := nummobjtypes - 1 downto 0 do
  begin
    check := Info_GetMobjName(i);
    check := strremovespaces(strupper(strtrim(check)));
    if check = mobj_name then
    begin
      result := i;
      exit;
    end;
  end;

  result := Info_GetMobjNumForAlias(mobj_name);
end;

procedure Info_SetMobjName(const mobj_no: integer; const name: string);
var
  i: integer;
  len: integer;
begin
  len := Length(name);
  if len > MOBJINFONAMESIZE then
    len := MOBJINFONAMESIZE;
  for i := 0 to len - 1 do
    mobjinfo[mobj_no].name[i] := name[i + 1];
  for i := len to MOBJINFONAMESIZE - 1 do
    mobjinfo[mobj_no].name[i] := #0;
end;

function Info_GetMobjName(const mobj_no: integer): string;
var
  i: integer;
  p: PChar;
begin
  result := '';
  p := @mobjinfo[mobj_no].name[0];
  for i := 0 to MOBJINFONAMESIZE - 1 do
    if p^ = #0 then
      exit
    else
    begin
      result := result + p^;
      inc(p);
    end;
end;

function Info_GetMobjName(const minfo: Pmobjinfo_t): string; overload;
var
  i: integer;
  p: PChar;
begin
  result := '';
  p := @minfo.name[0];
  for i := 0 to MOBJINFONAMESIZE - 1 do
    if p^ = #0 then
      exit
    else
    begin
      result := result + p^;
      inc(p);
    end;
end;

procedure Info_ShutDown;
var
  i: integer;
begin
  Info_ShutDownDnLookUp;
  for i := 0 to numstates - 1 do
  begin
    if states[i].params <> nil then
      FreeAndNil(states[i].params);
    if states[i].owners <> nil then
      FreeAndNil(states[i].owners);
    if states[i].dlights <> nil then
      FreeAndNil(states[i].dlights);
{$IFDEF OPENGL}
    if states[i].models <> nil then
      FreeAndNil(states[i].models);
{$ENDIF}
    if states[i].voxels <> nil then
      FreeAndNil(states[i].voxels);
  end;

  memfree(pointer(states), numstates * SizeOf(state_t));
  memfree(pointer(mobjinfo), nummobjtypes * SizeOf(mobjinfo_t));
  memfree(pointer(sprnames), (numsprites + 1) * 4);
end;

function Info_GetInheritance(const imo: Pmobjinfo_t): integer;
var
  mo: Pmobjinfo_t;
  loops: integer;
begin
  mo := imo;
  result := mo.inheritsfrom;

  if result <> -1 then
  begin
    loops := 0;
    while true do
    begin
      mo := @mobjinfo[mo.inheritsfrom];
      if mo.inheritsfrom = -1 then
        exit
      else
        result := mo.inheritsfrom;
      // JVAL: Prevent wrong inheritances of actordef lumps
      inc(loops);
      if loops > nummobjtypes then
      begin
        result := -1;
        break;
      end;
    end;
  end;

  if result = -1 then
    result :=  (integer(imo) - integer(@mobjinfo[0])) div SizeOf(mobjinfo_t);

end;

function Info_GetStatesForMobjInfo(const mobjno: integer): TDNumberList;
var
  N: TDNumberList;
  st: integer;

  procedure _AddStates(stnum: integer);
  var
    i: integer;
  begin
    for i := 0 to numstates - 1 do
    begin
      if N.IndexOf(stnum) >= 0 then
        exit
      else
      begin
        if stnum <= 0 then
          exit;
        N.Add(stnum);
        stnum := Ord(states[stnum].nextstate);
        if stnum <= 0 then
          exit;
      end;
    end;
  end;

begin
  N := TDNumberList.Create;
  if mobjno < 0 then
  begin
    result := N;
    exit;
  end;

  _AddStates(mobjinfo[mobjno].spawnstate);
  _AddStates(mobjinfo[mobjno].seestate);
  _AddStates(mobjinfo[mobjno].painstate);
  _AddStates(mobjinfo[mobjno].meleestate);
  _AddStates(mobjinfo[mobjno].missilestate);
  _AddStates(mobjinfo[mobjno].deathstate);
  _AddStates(mobjinfo[mobjno].xdeathstate);
  _AddStates(mobjinfo[mobjno].raisestate);
  _AddStates(mobjinfo[mobjno].healstate);
  _AddStates(mobjinfo[mobjno].crashstate);
  _AddStates(mobjinfo[mobjno].interactstate);

  for st := 0 to numstates - 1 do
    if states[st].owners <> nil then
      if N.IndexOf(st) < 0 then
        if states[st].owners.IndexOf(mobjno) >= 0 then
          N.Add(st);

  result := N;
end;

procedure Info_AddStateOwner(const st: Pstate_t; const moidx: integer);
begin
  if (moidx < 0) or (moidx >= nummobjtypes) then
    exit;

  if st.owners = nil then
    st.owners := TDNumberList.Create;
  if st.owners.IndexOf(moidx) < 0 then
    st.owners.Add(moidx);
end;

procedure Info_InitStateOwners;
var
  N: TDNumberList;
  i, j: integer;
begin
  for i := 0 to nummobjtypes - 1 do
  begin
    N := Info_GetStatesForMobjInfo(i);
    for j := 0 to N.Count - 1 do
      Info_AddStateOwner(@states[N.Numbers[j]], i);
    N.Free;
  end;
end;

function Info_ResolveMobjType(const name: string; const mt: PInteger): boolean;
begin
  if mt^ >= 0 then
  begin
    result := true;
    exit;
  end;

  if mt^ = -2 then
    mt^ := Info_GetMobjNumForName(name);

  result := mt^ >= 0;
end;

var
  save_actions: array[0..Ord(DO_NUMSTATES) - 1] of actionf_t;
  actions_saved: boolean = false;

procedure Info_SaveActions;
var
  i: integer;
begin
  for i := 0 to Ord(DO_NUMSTATES) - 1 do
    save_actions[i] := states[i].action;
  actions_saved := true;
end;

function Info_RestoreActions: boolean;
var
  i: integer;
begin
  result := actions_saved;
  if not result then
    exit;

  for i := 0 to Ord(DO_NUMSTATES) - 1 do
    states[i].action := save_actions[i];
end;

end.

