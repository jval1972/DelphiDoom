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
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit info_common;

interface

uses
  d_delphi,
  doomtype,
  info_h;

//==============================================================================
//
// Info_GetMobjNumForDoomNum
//
//==============================================================================
function Info_GetMobjNumForDoomNum(const dn: integer): integer;

//==============================================================================
//
// Info_InitDnLookUp
//
//==============================================================================
procedure Info_InitDnLookUp;

//==============================================================================
//
// Info_ShutDownDnLookUp
//
//==============================================================================
procedure Info_ShutDownDnLookUp;

//==============================================================================
//
// Info_CheckStates
//
//==============================================================================
procedure Info_CheckStates;

//==============================================================================
//
// Info_CheckStatesArgs
//
//==============================================================================
procedure Info_CheckStatesArgs;

//==============================================================================
//
// Info_GetNewState
//
//==============================================================================
function Info_GetNewState: integer;

//==============================================================================
//
// Info_GetNewMobjInfo
//
//==============================================================================
function Info_GetNewMobjInfo: integer;

//==============================================================================
//
// Info_GetSpriteNumForName
//
//==============================================================================
function Info_GetSpriteNumForName(const name: string): integer;

//==============================================================================
//
// Info_GetSpriteNameForNum
//
//==============================================================================
function Info_GetSpriteNameForNum(const id: integer): string;

//==============================================================================
//
// Info_CheckSpriteNumForName
//
//==============================================================================
function Info_CheckSpriteNumForName(const name: string): integer;

//==============================================================================
//
// Info_GetMobjNumForName
//
//==============================================================================
function Info_GetMobjNumForName(const name: string): integer;

//==============================================================================
//
// Info_SetMobjName
//
//==============================================================================
procedure Info_SetMobjName(const mobj_no: integer; const name: string);

//==============================================================================
//
// Info_GetMobjName
//
//==============================================================================
function Info_GetMobjName(const mobj_no: integer): string; overload;

//==============================================================================
//
// Info_GetMobjName
//
//==============================================================================
function Info_GetMobjName(const minfo: Pmobjinfo_t): string; overload;

//==============================================================================
//
// Info_ShutDown
//
//==============================================================================
procedure Info_ShutDown;

//==============================================================================
//
// Info_GetInheritance
//
//==============================================================================
function Info_GetInheritance(const imo: Pmobjinfo_t): integer;

//==============================================================================
//
// Info_AddMobjNameAlias
//
//==============================================================================
function Info_AddMobjNameAlias(const alias1: string; const alias2: string): boolean;

//==============================================================================
//
// Info_GetStatesForMobjInfo
//
//==============================================================================
function Info_GetStatesForMobjInfo(const mobjno: integer): TDNumberList;

//==============================================================================
//
// Info_AddStateOwner
//
//==============================================================================
procedure Info_AddStateOwner(const st: Pstate_t; const moidx: integer);

//==============================================================================
//
// Info_InitStateOwners
//
//==============================================================================
procedure Info_InitStateOwners;

//==============================================================================
//
// Info_ResolveMobjType
//
//==============================================================================
function Info_ResolveMobjType(const name: string; const mt: PInteger): boolean;

//==============================================================================
//
// Info_SaveActions
//
//==============================================================================
procedure Info_SaveActions;

//==============================================================================
//
// Info_RestoreActions
//
//==============================================================================
function Info_RestoreActions: boolean;

// MBF21 groups
const
  // infighting groups
  IG_INVALID = MININT;
  IG_DEFAULT = 0;
  IG_END = 1;
  // projectile groups
  PG_INVALID = MININT;
  PG_GROUPLESS = -1;
  PG_DEFAULT = 0;
  {$IFDEF DOOM}
  PG_BARON = 1;
  PG_END = 2;
  {$ELSE}
  PG_END = 1;
  {$ENDIF}
  // Splash groups
  SG_INVALID = MININT;
  SG_DEFAULT = 0;
  SG_END = 1;

//==============================================================================
//
// Info_InfightingGroupToString
//
//==============================================================================
function Info_InfightingGroupToString(const i: integer): string;

//==============================================================================
//
// Info_InfightingGroupToInt
//
//==============================================================================
function Info_InfightingGroupToInt(const s: string): integer;

//==============================================================================
//
// Info_ProjectileGroupToString
//
//==============================================================================
function Info_ProjectileGroupToString(const i: integer): string;

//==============================================================================
//
// Info_ProjectileGroupToInt
//
//==============================================================================
function Info_ProjectileGroupToInt(const s: string): integer;

//==============================================================================
//
// Info_SplashGroupToString
//
//==============================================================================
function Info_SplashGroupToString(const i: integer): string;

//==============================================================================
//
// Info_SplashGroupToInt
//
//==============================================================================
function Info_SplashGroupToInt(const s: string): integer;

implementation

uses
  d_think,
  deh_main,
  i_system,
  m_fixed,
  p_spec,
  p_common,
  sc_consts,
  info;

var
  dnLookUp: PIntegerArray = nil; // JVAL: Doom Editor Number LookUp

const
  DNLOOKUPSIZE = $10000;

var
  mobjinfo_aliases: TDStringList = nil;

//==============================================================================
//
// Info_InitDnLookUp
//
//==============================================================================
procedure Info_InitDnLookUp;
begin
  if dnLookUp = nil then
    dnLookUp := mallocz(DNLOOKUPSIZE * SizeOf(integer));

  if mobjinfo_aliases = nil then
    mobjinfo_aliases := TDStringList.Create;
end;

//==============================================================================
//
// Info_ShutDownDnLookUp
//
//==============================================================================
procedure Info_ShutDownDnLookUp;
begin
  memfree(pointer(dnLookUp), DNLOOKUPSIZE * SizeOf(integer));
  mobjinfo_aliases.Free;
  mobjinfo_aliases := nil;
end;

//==============================================================================
//
// Info_AddMobjNameAlias
//
//==============================================================================
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

//==============================================================================
//
// Info_GetMobjNumForAlias
//
//==============================================================================
function Info_GetMobjNumForAlias(const name: string): integer;
const
  DEH_ACTOR_PREFIX = 'DEH_ACTOR_';
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

  check := strupper(strremovespaces(name));
  if Pos(DEH_ACTOR_PREFIX, check) = 1 then
  begin
    snum := Copy(check, Length(DEH_ACTOR_PREFIX) + 1, Length(check) - Length(DEH_ACTOR_PREFIX));
    if StrIsLongWord(snum) then
    begin
      result := atoi(snum);
      if IsIntegerInRange(result, 0, nummobjtypes - 1) then
        exit;
    end;
  end;

  check := check + '=';
  for i := mobjinfo_aliases.Count - 1 downto 0 do
  begin
    if Pos(check, mobjinfo_aliases.Strings[i]) = 1 then
    begin
      splitstring_ch(mobjinfo_aliases.Strings[i], check, snum, '=');
      result := atoi(snum);
      exit;
    end;
  end;

  result := -1;
end;

//==============================================================================
//
// Info_GetMobjNumForDoomNum
//
//==============================================================================
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

//==============================================================================
//
// Info_CheckStates
//
//==============================================================================
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

//==============================================================================
//
// Info_CheckStatesArgs
//
//==============================================================================
procedure Info_CheckStatesArgs;
var
  i: integer;
begin
  for i := 0 to numstates - 1 do
    P_CheckStateArgs(@states[i]);
end;

//==============================================================================
//
// Info_GetNewState
//
//==============================================================================
function Info_GetNewState: integer;
const
  ST_GROWSTEP = 256;
begin
  if numstates >= numrealstates then
  begin
    realloc(pointer(states), numrealstates * SizeOf(state_t), (numrealstates + ST_GROWSTEP) * SizeOf(state_t));
    ZeroMemory(@states[numrealstates], ST_GROWSTEP * SizeOf(state_t));
    numrealstates := numrealstates + ST_GROWSTEP;
  end;
  states[numstates].sprite := Ord(SPR_NULL);
  states[numstates].tics := -1;
  states[numstates].tics2 := -1;
  states[numstates].nextstate := statenum_t(numstates);
  result := numstates;
  inc(numstates);
end;

//==============================================================================
//
// Info_GetNewMobjInfo
//
//==============================================================================
function Info_GetNewMobjInfo: integer;
const
  MI_GROWSTEP = 64;
begin
  if nummobjtypes >= numrealmobjtypes then
  begin
    realloc(pointer(mobjinfo), numrealmobjtypes * SizeOf(mobjinfo_t), (numrealmobjtypes + MI_GROWSTEP) * SizeOf(mobjinfo_t));
    ZeroMemory(@mobjinfo[numrealmobjtypes], MI_GROWSTEP * SizeOf(mobjinfo_t));
    numrealmobjtypes := numrealmobjtypes + MI_GROWSTEP;
  end;
  {$IFDEF STRIFE}
  mobjinfo[nummobjtypes].name2 := '';
  {$ENDIF}
  mobjinfo[nummobjtypes].inheritsfrom := -1; // Set to -1
  mobjinfo[nummobjtypes].doomednum := -1; // Set to -1
  mobjinfo[nummobjtypes].pushfactor := DEFPUSHFACTOR;
  mobjinfo[nummobjtypes].friction := ORIG_FRICTION;
  mobjinfo[nummobjtypes].scale := FRACUNIT;
  mobjinfo[nummobjtypes].gravity := FRACUNIT;
  mobjinfo[nummobjtypes].infighting_group := IG_DEFAULT;
  mobjinfo[nummobjtypes].projectile_group := PG_DEFAULT;
  mobjinfo[nummobjtypes].splash_group := SG_DEFAULT;
  result := nummobjtypes;
  inc(nummobjtypes);
end;

//==============================================================================
//
// Info_GetSpriteNumForName
//
//==============================================================================
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

//==============================================================================
//
// Info_GetSpriteNameForNum
//
//==============================================================================
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

//==============================================================================
//
// Info_CheckSpriteNumForName
//
//==============================================================================
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

//==============================================================================
//
// Info_GetMobjNumForName
//
//==============================================================================
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
    trimprocU(check);
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

//==============================================================================
//
// Info_SetMobjName
//
//==============================================================================
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

//==============================================================================
//
// Info_GetMobjName
//
//==============================================================================
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

//==============================================================================
//
// Info_GetMobjName
//
//==============================================================================
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

//==============================================================================
//
// Info_ShutDown
//
//==============================================================================
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

  memfree(pointer(states), numrealstates * SizeOf(state_t));
  memfree(pointer(mobjinfo), numrealmobjtypes * SizeOf(mobjinfo_t));
  memfree(pointer(sprnames), (numsprites + 1) * 4);
end;

//==============================================================================
//
// Info_GetInheritance
//
//==============================================================================
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

//==============================================================================
//
// Info_GetStatesForMobjInfo
//
//==============================================================================
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
  _AddStates(mobjinfo[mobjno].crushstate);
  _AddStates(mobjinfo[mobjno].interactstate);

  for st := 0 to numstates - 1 do
    if states[st].owners <> nil then
      if N.IndexOf(st) < 0 then
        if states[st].owners.IndexOf(mobjno) >= 0 then
          N.Add(st);

  result := N;
end;

//==============================================================================
//
// Info_AddStateOwner
//
//==============================================================================
procedure Info_AddStateOwner(const st: Pstate_t; const moidx: integer);
begin
  if (moidx < 0) or (moidx >= nummobjtypes) then
    exit;

  if st.owners = nil then
    st.owners := TDNumberList.Create;
  if st.owners.IndexOf(moidx) < 0 then
    st.owners.Add(moidx);
end;

//==============================================================================
//
// Info_InitStateOwners
//
//==============================================================================
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

//==============================================================================
//
// Info_ResolveMobjType
//
//==============================================================================
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

//==============================================================================
//
// Info_SaveActions
//
//==============================================================================
procedure Info_SaveActions;
var
  i: integer;
begin
  for i := 0 to Ord(DO_NUMSTATES) - 1 do
    save_actions[i] := states[i].action;
  actions_saved := true;
end;

//==============================================================================
//
// Info_RestoreActions
//
//==============================================================================
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

//==============================================================================
//
// Info_InfightingGroupToString
//
//==============================================================================
function Info_InfightingGroupToString(const i: integer): string;
begin
  if i = IG_DEFAULT then
    result := 'IG_DEFAULT'
  else if i = IG_INVALID then
    result := 'IG_INVALID'
  else if IsIntegerInRange(i, 0, infighting_groups.Count - 1) then
    result := infighting_groups.Strings[i]
  else
    result := itoa(i);
end;

//==============================================================================
//
// Info_InfightingGroupToInt
//
//==============================================================================
function Info_InfightingGroupToInt(const s: string): integer;
var
  check: string;
begin
  check := strupper(s);
  if Pos('IG_', check) <> 1 then
    check := 'IG_' + check;

  if check = 'IG_DEFAULT' then
  begin
    result := IG_DEFAULT;
    exit;
  end;

  if check = 'IG_INVALID' then
  begin
    result := IG_INVALID;
    exit;
  end;

  result := infighting_groups.IndexOf(check);
  if result >= 0 then
    exit;

  result := atoi(s);
  if result < 0 then
    result := IG_INVALID
  else
  begin
    check := 'IG_' + itoa(result);
    result := infighting_groups.Add(check);
    SC_AddConst(check, result); // JVAL: 20220104 - Dynamically add const
  end;
end;

//==============================================================================
//
// Info_ProjectileGroupToString
//
//==============================================================================
function Info_ProjectileGroupToString(const i: integer): string;
begin
  if i = PG_GROUPLESS then
    result := 'PG_GROUPLESS'
  else if i = PG_INVALID then
    result := 'PG_INVALID'
  else if i = PG_DEFAULT then
    result := 'PG_DEFAULT'
  {$IFDEF DOOM}
  else if i = PG_BARON then
    result := 'PG_BARON'
  {$ENDIF}
  else if IsIntegerInRange(i, 0, projectile_groups.Count - 1) then
    result := projectile_groups.Strings[i]
  else
    result := itoa(i);
end;

//==============================================================================
//
// Info_ProjectileGroupToInt
//
//==============================================================================
function Info_ProjectileGroupToInt(const s: string): integer;
var
  check: string;
begin
  check := strupper(s);
  if Pos('PG_', check) <> 1 then
    check := 'PG_' + check;

  if check = 'PG_GROUPLESS' then
  begin
    result := PG_GROUPLESS;
    exit;
  end;

  if check = 'PG_INVALID' then
  begin
    result := PG_INVALID;
    exit;
  end;

  if check = 'PG_DEFAULT' then
  begin
    result := PG_DEFAULT;
    exit;
  end;

  {$IFDEF DOOM}
  if check = 'PG_BARON' then
  begin
    result := PG_BARON;
    exit;
  end;
  {$ENDIF}

  result := projectile_groups.IndexOf(check);
  if result >= 0 then
    exit;

  result := atoi(s);
  if result < 0 then
    result := PG_GROUPLESS
  else
  begin
    check := 'PG_' + itoa(result);
    result := projectile_groups.Add(check);
    SC_AddConst(check, result); // JVAL: 20220104 - Dynamically add const
  end;
end;

//==============================================================================
//
// Info_SplashGroupToString
//
//==============================================================================
function Info_SplashGroupToString(const i: integer): string;
begin
  if i = SG_DEFAULT then
    result := 'SG_DEFAULT'
  else if i = SG_INVALID then
    result := 'SG_INVALID'
  else if IsIntegerInRange(i, 0, splash_groups.Count - 1) then
    result := splash_groups.Strings[i]
  else
    result := itoa(i);
end;

//==============================================================================
//
// Info_SplashGroupToInt
//
//==============================================================================
function Info_SplashGroupToInt(const s: string): integer;
var
  check: string;
begin
  check := strupper(s);
  if Pos('SG_', check) <> 1 then
    check := 'SG_' + check;

  if check = 'SG_DEFAULT' then
  begin
    result := SG_DEFAULT;
    exit;
  end;

  if check = 'SG_INVALID' then
  begin
    result := SG_INVALID;
    exit;
  end;

  result := splash_groups.IndexOf(check);
  if result >= 0 then
    exit;

  result := atoi(s);
  if result < 0 then
    result := SG_INVALID
  else
  begin
    check := 'SG_' + itoa(result);
    result := splash_groups.Add(check);
    SC_AddConst(check, result); // JVAL: 20220104 - Dynamically add const
  end;
end;

end.

