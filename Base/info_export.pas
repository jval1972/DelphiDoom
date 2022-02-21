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

unit info_export;

interface

//==============================================================================
//
// Info_GetMobjinfoPascal
//
//==============================================================================
function Info_GetMobjinfoPascal(const astart: integer = 0; const afinish: integer = -1): string;

//==============================================================================
//
// Info_GetMobjinfoPascalConst
//
//==============================================================================
function Info_GetMobjinfoPascalConst(const astart: integer = 0; const afinish: integer = -1): string;

//==============================================================================
//
// Info_GetStatesPascal
//
//==============================================================================
function Info_GetStatesPascal(const astart: integer = 0; const afinish: integer = -1): string;

//==============================================================================
//
// Info_InitExportCommands
//
//==============================================================================
procedure Info_InitExportCommands;

implementation

uses
  TypInfo,
  d_delphi,
  c_cmds,
  d_think,
  deh_main,
  info,
  info_h,
  info_common,
  m_argv,
  m_fixed,
  p_gender,
  p_spec,
  r_renderstyle,
  sounddata;

//==============================================================================
//
// _state_name_Ord
//
//==============================================================================
function _state_name_Ord(const x: Integer): string;
begin
  if x < 0 then
    Result := 'Ord(S_NULL)'
  else if x < Ord(DO_NUMSTATES) then
    Result := 'Ord(' + strupper(GetENumName(TypeInfo(statenum_t), x)) + ')'
  else
    Result := itoa(x);
end;

//==============================================================================
//
// _state_name
//
//==============================================================================
function _state_name(const x: Integer): string;
begin
  if x < 0 then
    Result := 'S_NULL'
  else if x < Ord(DO_NUMSTATES) then
    Result := strupper(GetENumName(TypeInfo(statenum_t), x))
  else
    Result := 'statenum_t(' + itoa(x) + ')';
end;

//==============================================================================
//
// _sound_name_Ord
//
//==============================================================================
function _sound_name_Ord(const x: Integer): string;
begin
  if x < 0 then
    Result := 'Ord(sfx_None)'
  else if x < Ord(DO_NUMMUSIC) then
    Result := 'Ord(' + GetENumName(TypeInfo(sfxenum_t), x) + ')'
  else
    Result := itoa(x);
end;

//==============================================================================
//
// _mobjinfo_name_Ord
//
//==============================================================================
function _mobjinfo_name_Ord(const x: Integer): string;
begin
  if x < 0 then
    Result := '-1'
  else if x < Ord(DO_NUMMOBJTYPES) then
    Result := 'Ord(' + strupper(GetENumName(TypeInfo(mobjtype_t), x)) + ')'
  else
    Result := itoa(x);
end;

//==============================================================================
//
// _gender_name
//
//==============================================================================
function _gender_name(const x: Integer): string;
begin
  if x < 0 then
    Result := GetENumName(TypeInfo(gender_t), 0)
  else if x < Ord(NUMGENDERS) then
    Result := GetENumName(TypeInfo(gender_t), x)
  else
    Result := 'gender_t(' + itoa(x) + ')';
end;

//==============================================================================
//
// _pascal_name
//
//==============================================================================
function _pascal_name(const s: string): string;
var
  i: integer;
begin
  Result := '';
  for i := 1 to length(s) do
  begin
    if s[i] = '''' then
      Result := Result + s[i] + s[i]
    else
      Result := Result + s[i];
  end;
end;

//==============================================================================
//
// _renderstyle_name
//
//==============================================================================
function _renderstyle_name(const x: Integer): string;
begin
  if x < 0 then
    Result := GetENumName(TypeInfo(mobjrenderstyle_t), 0)
  else if x < Ord(NUMGENDERS) then
    Result := GetENumName(TypeInfo(mobjrenderstyle_t), x)
  else
    Result := 'mobjrenderstyle_t(' + itoa(x) + ')';
end;

//==============================================================================
//
// itoa_FRACUNIT
//
//==============================================================================
function itoa_FRACUNIT(const x: integer): string;
begin
  if Abs(x) < FRACUNIT then
    Result := itoa(x)
  else if x = FRACUNIT then
    Result := 'FRACUNIT'
  else if x = -FRACUNIT then
    Result := '-FRACUNIT'
  else if x mod FRACUNIT = 0 then
    Result := itoa(x div FRACUNIT) + ' * FRACUNIT'
  else
    Result := itoa(x);
end;

//==============================================================================
//
// itoa_FLAGS
//
//==============================================================================
function itoa_FLAGS(const x: Integer; const flags: TDTextList): string;
var
  i: integer;
  l: LongWord;
  lx: LongWord;
  sl: TDStringList;
begin
  if x = 0 then
  begin
    Result := '0';
    Exit;
  end;

  l := 1;
  lx := x;
  sl := TDStringList.Create;
  for i := 0 to flags.Count - 1 do
  begin
    if lx and l = l then
      sl.Add(flags.Strings[i]);
    l := l * 2
  end;

  Result := '';
  for i := 0 to sl.Count - 1 do
  begin
    Result := Result + sl.Strings[i];
    if i < sl.Count - 1 then
      Result := Result + ' or ';
  end;

  if Result = '' then
    Result := '0';

  sl.Free;
end;

//==============================================================================
//
// _spritenum_name_Ord
//
//==============================================================================
function _spritenum_name_Ord(const x: Integer): string;
begin
  if x < 0 then
    Result := 'Ord(SPR_NULL)'
  else if x < Ord(DO_NUMSPRITES) then
    Result := 'Ord(' + GetENumName(TypeInfo(spritenum_t), x) + ')'
  else
    Result := itoa(x);
end;

//==============================================================================
//
// _action_name
//
//==============================================================================
function _action_name(const action: actionf_t): string;
var
  i: integer;
begin
  for i := 0 to dehnumactions - 1 do
    if @deh_actions[i].action.acp1 = @action.acp1 then
    begin
      Result := '@A_' + capitalizedstring(deh_actions[i].name);
      if Result = '@A_Null' then
        Result := 'nil';
      Exit;
    end;

  Result := 'nil';
end;

//==============================================================================
//
// Info_GetMobjinfoPascal
//
//==============================================================================
function Info_GetMobjinfoPascal(const astart: integer = 0; const afinish: integer = -1): string;
var
  ret: string;
  i: integer;
  start, finish: integer;
  mname: string;

  procedure AddLn(const s: string);
  begin
    if s <> '' then
      ret := ret + '  ' + s + #13#10
    else
      ret := ret + #13#10;
  end;

begin
  ret := '';

  start := astart;
  if start < 0 then
    start := 0;

  if afinish < 0 then
    finish := nummobjtypes - 1
  else
    finish := afinish;

  AddLn('////////////////////////////////////////////////////////////////////////////////');
  AddLn('// mobjinfo');
  AddLn('////////////////////////////////////////////////////////////////////////////////');
  AddLn('');
  for i := start to finish do
  begin
    AddLn('////////////////////////////////////////////////////////////////////////////////');
    if i < Ord(DO_NUMMOBJTYPES) then
      AddLn('// ' + strupper(GetENumName(TypeInfo(mobjtype_t), i)))
    else
      AddLn('// #' + itoa(i));

    mname := _mobjinfo_name_Ord(i);
    AddLn('mobjinfo[' + mname + '].name := ''' + _pascal_name(mobjinfo[i].name) + ''';');
    {$IFDEF STRIFE}
    AddLn('mobjinfo[' + mname + '].name2 := ''' + _pascal_name(mobjinfo[i].name2) + ''';');
    {$ENDIF}
    AddLn('mobjinfo[' + mname + '].inheritsfrom := ' + itoa(mobjinfo[i].inheritsfrom) + ';');
    AddLn('mobjinfo[' + mname + '].doomednum := ' + itoa(mobjinfo[i].doomednum) + ';');
    AddLn('mobjinfo[' + mname + '].spawnstate := ' + _state_name_Ord(mobjinfo[i].spawnstate) + ';');
    AddLn('mobjinfo[' + mname + '].spawnhealth := ' + itoa(mobjinfo[i].spawnhealth) + ';');
    AddLn('mobjinfo[' + mname + '].seestate := ' + _state_name_Ord(mobjinfo[i].seestate) + ';');
    AddLn('mobjinfo[' + mname + '].seesound := ' + _sound_name_Ord(mobjinfo[i].seesound) + ';');
    AddLn('mobjinfo[' + mname + '].reactiontime := ' + itoa(mobjinfo[i].reactiontime) + ';');
    AddLn('mobjinfo[' + mname + '].attacksound := ' + _sound_name_Ord(mobjinfo[i].attacksound) + ';');
    AddLn('mobjinfo[' + mname + '].painstate := ' + _state_name_Ord(mobjinfo[i].painstate) + ';');
    AddLn('mobjinfo[' + mname + '].painchance := ' + itoa(mobjinfo[i].painchance) + ';');
    AddLn('mobjinfo[' + mname + '].painsound := ' + _sound_name_Ord(mobjinfo[i].painsound) + ';');
    AddLn('mobjinfo[' + mname + '].meleestate := ' + _state_name_Ord(mobjinfo[i].meleestate) + ';');
    AddLn('mobjinfo[' + mname + '].missilestate := ' + _state_name_Ord(mobjinfo[i].missilestate) + ';');
    AddLn('mobjinfo[' + mname + '].deathstate := ' + _state_name_Ord(mobjinfo[i].deathstate) + ';');
    AddLn('mobjinfo[' + mname + '].xdeathstate := ' + _state_name_Ord(mobjinfo[i].xdeathstate) + ';');
    AddLn('mobjinfo[' + mname + '].deathsound := ' + _sound_name_Ord(mobjinfo[i].deathsound) + ';');
    AddLn('mobjinfo[' + mname + '].speed := ' + itoa_FRACUNIT(mobjinfo[i].speed) + ';');
    AddLn('mobjinfo[' + mname + '].radius := ' + itoa_FRACUNIT(mobjinfo[i].radius) + ';');
    AddLn('mobjinfo[' + mname + '].height := ' + itoa_FRACUNIT(mobjinfo[i].height) + ';');
    AddLn('mobjinfo[' + mname + '].mass := ' + itoa(mobjinfo[i].mass) + ';');
    AddLn('mobjinfo[' + mname + '].damage := ' + itoa(mobjinfo[i].damage) + ';');
    AddLn('mobjinfo[' + mname + '].activesound := ' + _sound_name_Ord(mobjinfo[i].activesound) + ';');
    AddLn('mobjinfo[' + mname + '].flags := ' + itoa_FLAGS(mobjinfo[i].flags, mobj_flags) + ';');
    {$IFDEF HERETIC_OR_HEXEN}
    AddLn('mobjinfo[' + mname + '].flags2 := ' + itoa_FLAGS(mobjinfo[i].flags2, mobj_flags2) + ';');
    {$ENDIF}
    AddLn('mobjinfo[' + mname + '].flags_ex := ' + itoa_FLAGS(mobjinfo[i].flags_ex, mobj_flags_ex) + ';');
    AddLn('mobjinfo[' + mname + '].flags2_ex := ' + itoa_FLAGS(mobjinfo[i].flags2_ex, mobj_flags2_ex) + ';');
    AddLn('mobjinfo[' + mname + '].raisestate := ' + _state_name_Ord(mobjinfo[i].raisestate) + ';');
    AddLn('mobjinfo[' + mname + '].customsound1 := ' + _sound_name_Ord(mobjinfo[i].customsound1) + ';');
    AddLn('mobjinfo[' + mname + '].customsound2 := ' + _sound_name_Ord(mobjinfo[i].customsound2) + ';');
    AddLn('mobjinfo[' + mname + '].customsound3 := ' + _sound_name_Ord(mobjinfo[i].customsound3) + ';');
    AddLn('mobjinfo[' + mname + '].meleesound := ' + _sound_name_Ord(mobjinfo[i].meleesound) + ';');
    AddLn('mobjinfo[' + mname + '].dropitem := ' + itoa(mobjinfo[i].dropitem) + ';');
    AddLn('mobjinfo[' + mname + '].missiletype := ' + itoa(mobjinfo[i].missiletype) + ';');
    AddLn('mobjinfo[' + mname + '].explosiondamage := ' + itoa(mobjinfo[i].explosiondamage) + ';');
    AddLn('mobjinfo[' + mname + '].explosionradius := ' + itoa(mobjinfo[i].explosionradius) + ';');
    AddLn('mobjinfo[' + mname + '].meleedamage := ' + itoa(mobjinfo[i].meleedamage) + ';');
    AddLn('mobjinfo[' + mname + '].renderstyle := ' + _renderstyle_name(Ord(mobjinfo[i].renderstyle)) + ';');
    AddLn('mobjinfo[' + mname + '].alpha := ' + itoa(mobjinfo[i].alpha) + ';');
    AddLn('mobjinfo[' + mname + '].healstate := ' + _state_name_Ord(mobjinfo[i].healstate) + ';');
    AddLn('mobjinfo[' + mname + '].crashstate := ' + _state_name_Ord(mobjinfo[i].crashstate) + ';');
    AddLn('mobjinfo[' + mname + '].crushstate := ' + _state_name_Ord(mobjinfo[i].crushstate) + ';');
    AddLn('mobjinfo[' + mname + '].interactstate := ' + _state_name_Ord(mobjinfo[i].interactstate) + ';');
    AddLn('mobjinfo[' + mname + '].missileheight := ' + itoa(mobjinfo[i].missileheight) + ';');
    AddLn('mobjinfo[' + mname + '].vspeed := ' + itoa(mobjinfo[i].vspeed) + ';');
    if mobjinfo[i].pushfactor = DEFPUSHFACTOR then
      AddLn('mobjinfo[' + mname + '].pushfactor := DEFPUSHFACTOR;')
    else
      AddLn('mobjinfo[' + mname + '].pushfactor := ' + itoa(mobjinfo[i].pushfactor) + ';');
    if mobjinfo[i].friction = ORIG_FRICTION then
      AddLn('mobjinfo[' + mname + '].friction := ORIG_FRICTION;')
    else
      AddLn('mobjinfo[' + mname + '].friction := ' + itoa(mobjinfo[i].friction) + ';');
    AddLn('mobjinfo[' + mname + '].scale := ' + itoa_FRACUNIT(mobjinfo[i].scale) + ';');
    AddLn('mobjinfo[' + mname + '].gravity := ' + itoa_FRACUNIT(mobjinfo[i].gravity) + ';');
    AddLn('mobjinfo[' + mname + '].flags3_ex := ' + itoa_FLAGS(mobjinfo[i].flags3_ex, mobj_flags3_ex) + ';');
    AddLn('mobjinfo[' + mname + '].flags4_ex := ' + itoa_FLAGS(mobjinfo[i].flags4_ex, mobj_flags4_ex) + ';');
    AddLn('mobjinfo[' + mname + '].minmissilechance := ' + itoa(mobjinfo[i].minmissilechance) + ';');
    AddLn('mobjinfo[' + mname + '].floatspeed := ' + itoa_FRACUNIT(mobjinfo[i].floatspeed) + ';');
    AddLn('mobjinfo[' + mname + '].normalspeed := ' + itoa_FRACUNIT(mobjinfo[i].normalspeed) + ';');
    AddLn('mobjinfo[' + mname + '].fastspeed := ' + itoa_FRACUNIT(mobjinfo[i].fastspeed) + ';');
    AddLn('mobjinfo[' + mname + '].obituary := ''' + _pascal_name(mobjinfo[i].obituary) + ''';');
    AddLn('mobjinfo[' + mname + '].hitobituary := ''' + _pascal_name(mobjinfo[i].hitobituary) + ''';');
    AddLn('mobjinfo[' + mname + '].gender := ' + _gender_name(Ord(mobjinfo[i].gender)) + ';');
    AddLn('mobjinfo[' + mname + '].meleerange := ' + itoa(mobjinfo[i].meleerange) + ';');
    AddLn('mobjinfo[' + mname + '].maxstepheight := ' + itoa(mobjinfo[i].maxstepheight) + ';');
    AddLn('mobjinfo[' + mname + '].maxdropoffheight := ' + itoa(mobjinfo[i].maxdropoffheight) + ';');
    AddLn('mobjinfo[' + mname + '].gibhealth := ' + itoa(mobjinfo[i].gibhealth) + ';');
    AddLn('mobjinfo[' + mname + '].maxtargetrange := ' + itoa(mobjinfo[i].maxtargetrange) + ';');
    AddLn('mobjinfo[' + mname + '].WeaveIndexXY := ' + itoa(mobjinfo[i].WeaveIndexXY) + ';');
    AddLn('mobjinfo[' + mname + '].WeaveIndexZ := ' + itoa(mobjinfo[i].WeaveIndexZ) + ';');
    AddLn('mobjinfo[' + mname + '].spriteDX := ' + itoa_FRACUNIT(mobjinfo[i].spriteDX) + ';');
    AddLn('mobjinfo[' + mname + '].spriteDY := ' + itoa_FRACUNIT(mobjinfo[i].spriteDY) + ';');
    AddLn('mobjinfo[' + mname + '].flags5_ex := ' + itoa_FLAGS(mobjinfo[i].flags5_ex, mobj_flags5_ex) + ';');
    AddLn('mobjinfo[' + mname + '].flags6_ex := ' + itoa_FLAGS(mobjinfo[i].flags6_ex, mobj_flags6_ex) + ';');
    AddLn('mobjinfo[' + mname + '].infighting_group := ' + Info_InfightingGroupToString(mobjinfo[i].infighting_group) + ';');
    AddLn('mobjinfo[' + mname + '].projectile_group := ' + Info_ProjectileGroupToString(mobjinfo[i].projectile_group) + ';');
    AddLn('mobjinfo[' + mname + '].splash_group := ' + Info_SplashGroupToString(mobjinfo[i].splash_group) + ';');
    AddLn('mobjinfo[' + mname + '].ripsound := ' + _sound_name_Ord(mobjinfo[i].ripsound) + ';');
    AddLn('mobjinfo[' + mname + '].bloodcolor := ' + itoa(mobjinfo[i].bloodcolor) + ';');
    AddLn('mobjinfo[' + mname + '].translationname := ''' + mobjinfo[i].translationname + ''';');
    AddLn('mobjinfo[' + mname + '].meleethreshold := ' + itoa(mobjinfo[i].meleethreshold) + ';');
    AddLn('');
  end;

  Result := ret;
end;

//==============================================================================
//
// Info_GetMobjinfoPascalConst
//
//==============================================================================
function Info_GetMobjinfoPascalConst(const astart: integer = 0; const afinish: integer = -1): string;
var
  ret: string;
  i: integer;
  start, finish: integer;
  fmobjinfo: PmobjinfoArray_t;

  procedure AddLn(const s: string);
  begin
    if s <> '' then
      ret := ret + '  ' + s + #13#10
    else
      ret := ret + #13#10;
  end;

  procedure AddField(const s1, s2: string);
  var
    s: string;
  begin
    s := '    ' + s1 + ': ' + s2 + ';';
    while length(s) < 79 do
      s := s + ' ';
    s := s + ' // ' + s1 + #13#10;
    ret := ret + s;
  end;

begin
  ret := '';

  start := astart;
  if start < 0 then
    start := 0;

  if afinish < 0 then
    finish := nummobjtypes - 1
  else
    finish := afinish;

  if (start = 0) and (finish = Ord(DO_NUMMOBJTYPES) - 1) then
    fmobjinfo := pDO_mobjinfo
  else
    fmobjinfo := mobjinfo;

  AddLn('////////////////////////////////////////////////////////////////////////////////');
  AddLn('// mobjinfo');
  AddLn('////////////////////////////////////////////////////////////////////////////////');
  AddLn('');
  AddLn('(');
  AddLn('');
  for i := start to finish do
  begin
    AddLn('////////////////////////////////////////////////////////////////////////////////');
    if i < Ord(DO_NUMMOBJTYPES) then
      AddLn(' ( // ' + strupper(GetENumName(TypeInfo(mobjtype_t), i)))
    else
      AddLn(' ( // #' + itoa(i) + ' (' + _mobjinfo_name_Ord(i) + ')');

    AddField('name', '''' + _pascal_name(fmobjinfo[i].name) + '''');
    {$IFDEF STRIFE}
    AddField('name2', '''' + _pascal_name(fmobjinfo[i].name2) + '''');
    {$ENDIF}
    AddField('inheritsfrom', itoa(fmobjinfo[i].inheritsfrom));
    AddField('doomednum', itoa(fmobjinfo[i].doomednum));
    AddField('spawnstate', _state_name_Ord(fmobjinfo[i].spawnstate));
    AddField('spawnhealth', itoa(fmobjinfo[i].spawnhealth));
    AddField('seestate', _state_name_Ord(fmobjinfo[i].seestate));
    AddField('seesound', _sound_name_Ord(fmobjinfo[i].seesound));
    AddField('reactiontime', itoa(fmobjinfo[i].reactiontime));
    AddField('attacksound', _sound_name_Ord(fmobjinfo[i].attacksound));
    AddField('painstate', _state_name_Ord(fmobjinfo[i].painstate));
    AddField('painchance', itoa(fmobjinfo[i].painchance));
    AddField('painsound', _sound_name_Ord(fmobjinfo[i].painsound));
    AddField('meleestate', _state_name_Ord(fmobjinfo[i].meleestate));
    AddField('missilestate', _state_name_Ord(fmobjinfo[i].missilestate));
    AddField('deathstate', _state_name_Ord(fmobjinfo[i].deathstate));
    AddField('xdeathstate', _state_name_Ord(fmobjinfo[i].xdeathstate));
    AddField('deathsound', _sound_name_Ord(fmobjinfo[i].deathsound));
    AddField('speed', itoa_FRACUNIT(fmobjinfo[i].speed));
    AddField('radius', itoa_FRACUNIT(fmobjinfo[i].radius));
    AddField('height', itoa_FRACUNIT(fmobjinfo[i].height));
    AddField('mass', itoa(fmobjinfo[i].mass));
    AddField('damage', itoa(fmobjinfo[i].damage));
    AddField('activesound', _sound_name_Ord(fmobjinfo[i].activesound));
    AddField('flags', itoa_FLAGS(fmobjinfo[i].flags, mobj_flags));
    {$IFDEF HERETIC_OR_HEXEN}
    AddField('flags2', itoa_FLAGS(fmobjinfo[i].flags2, mobj_flags2));
    {$ENDIF}
    AddField('flags_ex', itoa_FLAGS(fmobjinfo[i].flags_ex, mobj_flags_ex));
    AddField('flags2_ex', itoa_FLAGS(fmobjinfo[i].flags2_ex, mobj_flags2_ex));
    AddField('raisestate', _state_name_Ord(fmobjinfo[i].raisestate));
    AddField('customsound1', _sound_name_Ord(fmobjinfo[i].customsound1));
    AddField('customsound2', _sound_name_Ord(fmobjinfo[i].customsound2));
    AddField('customsound3', _sound_name_Ord(fmobjinfo[i].customsound3));
    AddField('meleesound', _sound_name_Ord(fmobjinfo[i].meleesound));
    AddField('dropitem', itoa(fmobjinfo[i].dropitem));
    AddField('missiletype', itoa(fmobjinfo[i].missiletype));
    AddField('explosiondamage', itoa(fmobjinfo[i].explosiondamage));
    AddField('explosionradius', itoa(fmobjinfo[i].explosionradius));
    AddField('meleedamage', itoa(fmobjinfo[i].meleedamage));
    AddField('renderstyle', _renderstyle_name(Ord(fmobjinfo[i].renderstyle)));
    AddField('alpha', itoa(fmobjinfo[i].alpha));
    AddField('healstate', _state_name_Ord(fmobjinfo[i].healstate));
    AddField('crashstate', _state_name_Ord(fmobjinfo[i].crashstate));
    AddField('crushstate', _state_name_Ord(fmobjinfo[i].crushstate));
    AddField('interactstate', _state_name_Ord(fmobjinfo[i].interactstate));
    AddField('missileheight', itoa(fmobjinfo[i].missileheight));
    AddField('vspeed', itoa(fmobjinfo[i].vspeed));
    if fmobjinfo[i].pushfactor = DEFPUSHFACTOR then
      AddField('pushfactor', 'DEFPUSHFACTOR')
    else
      AddField('pushfactor', itoa(fmobjinfo[i].pushfactor));
    if fmobjinfo[i].friction = ORIG_FRICTION then
      AddField('friction', 'ORIG_FRICTION')
    else
      AddField('friction', itoa(fmobjinfo[i].friction));
    AddField('scale', itoa_FRACUNIT(fmobjinfo[i].scale));
    AddField('gravity', itoa_FRACUNIT(fmobjinfo[i].gravity));
    AddField('flags3_ex', itoa_FLAGS(fmobjinfo[i].flags3_ex, mobj_flags3_ex));
    AddField('flags4_ex', itoa_FLAGS(fmobjinfo[i].flags4_ex, mobj_flags4_ex));
    AddField('minmissilechance', itoa(fmobjinfo[i].minmissilechance));
    AddField('floatspeed', itoa_FRACUNIT(fmobjinfo[i].floatspeed));
    AddField('normalspeed', itoa_FRACUNIT(fmobjinfo[i].normalspeed));
    AddField('fastspeed', itoa_FRACUNIT(fmobjinfo[i].fastspeed));
    AddField('obituary', '''' + _pascal_name(fmobjinfo[i].obituary) + '''');
    AddField('hitobituary', '''' + _pascal_name(fmobjinfo[i].hitobituary) + '''');
    AddField('gender', _gender_name(Ord(fmobjinfo[i].gender)));
    AddField('meleerange', itoa(fmobjinfo[i].meleerange));
    AddField('maxstepheight', itoa(fmobjinfo[i].maxstepheight));
    AddField('maxdropoffheight', itoa(fmobjinfo[i].maxdropoffheight));
    AddField('gibhealth', itoa(fmobjinfo[i].gibhealth));
    AddField('maxtargetrange', itoa(fmobjinfo[i].maxtargetrange));
    AddField('WeaveIndexXY', itoa(fmobjinfo[i].WeaveIndexXY));
    AddField('WeaveIndexZ', itoa(fmobjinfo[i].WeaveIndexZ));
    AddField('spriteDX', itoa_FRACUNIT(fmobjinfo[i].spriteDX));
    AddField('spriteDY', itoa_FRACUNIT(fmobjinfo[i].spriteDY));
    AddField('flags5_ex', itoa_FLAGS(fmobjinfo[i].flags5_ex, mobj_flags5_ex));
    AddField('flags6_ex', itoa_FLAGS(fmobjinfo[i].flags6_ex, mobj_flags6_ex));
    AddField('infighting_group', Info_InfightingGroupToString(mobjinfo[i].infighting_group));
    AddField('projectile_group', Info_ProjectileGroupToString(mobjinfo[i].projectile_group));
    AddField('splash_group', Info_SplashGroupToString(mobjinfo[i].splash_group));
    AddField('ripsound', _sound_name_Ord(fmobjinfo[i].ripsound));
    AddField('bloodcolor', itoa(fmobjinfo[i].bloodcolor));
    AddField('translationname', '''' + _pascal_name(fmobjinfo[i].translationname) + '''');
    AddField('meleethreshold', itoa(fmobjinfo[i].meleethreshold));
    if i = finish then
      AddLn(' )')
    else
      AddLn(' ),');
    AddLn('');
  end;
  AddLn(')');

  Result := ret;
end;

//==============================================================================
//
// Info_GetStatesPascal
//
//==============================================================================
function Info_GetStatesPascal(const astart: integer = 0; const afinish: integer = -1): string;
var
  ret: string;
  i: integer;
  start, finish: integer;
  sname: string;

  procedure AddLn(const s: string);
  begin
    if s <> '' then
      ret := ret + '  ' + s + #13#10
    else
      ret := ret + #13#10;
  end;

begin
  ret := '';

  start := astart;
  if start < 0 then
    start := 0;

  if afinish < 0 then
    finish := numstates - 1
  else
    finish := afinish;

  AddLn('////////////////////////////////////////////////////////////////////////////////');
  AddLn('// states');
  AddLn('////////////////////////////////////////////////////////////////////////////////');
  AddLn('');
  for i := start to finish do
  begin
    AddLn('////////////////////////////////////////////////////////////////////////////////');
    if i < Ord(DO_NUMSTATES) then
      AddLn('// ' + strupper(GetENumName(TypeInfo(statenum_t), i)))
    else
      AddLn('// #' + itoa(i));

    sname := _state_name_Ord(i);

    AddLn('states[' + sname + '].sprite := ' + _spritenum_name_Ord(Ord(states[i].sprite)) + ';');
    AddLn('states[' + sname + '].frame := ' + itoa(states[i].frame) + ';');
    AddLn('states[' + sname + '].tics := ' + itoa(states[i].tics) + ';');
    AddLn('states[' + sname + '].tics2 := ' + itoa(states[i].tics2) + ';');
    AddLn('states[' + sname + '].action.acp1 := ' + _action_name(states[i].action) + ';');
    AddLn('states[' + sname + '].nextstate := ' + _state_name(Ord(states[i].nextstate)) + ';');
    AddLn('states[' + sname + '].misc1 := ' + itoa(states[i].misc1) + ';');
    AddLn('states[' + sname + '].misc2 := ' + itoa(states[i].misc2) + ';');
    AddLn('states[' + sname + '].flags_ex := ' + itoa_FLAGS(states[i].flags_ex, state_flags_ex) + ';');

    AddLn('');
  end;

  Result := ret;
end;

//==============================================================================
//
// CmdExportInfoPascal
//
//==============================================================================
procedure CmdExportInfoPascal(const fname: string);
var
  fname1: string;
  s: string;
  sl: TDStringList;
begin
  if fname = '' then
  begin
    printf('Please specify the filename to save info export'#13#10);
    exit;
  end;

  if CharPos('.', fname) = 0 then
    fname1 := fname + '.txt'
  else
    fname1 := fname;

  fname1 := M_SaveFileName(fname1);

  s := Info_GetMobjinfoPascal + Info_GetStatesPascal;

  sl := TDStringList.Create;
  try
    sl.Text := s;
    sl.SaveToFile(fname1);
    printf('Info settings saved to %s'#13#10, [fname1]);
  finally
    sl.Free;
  end;
end;

//==============================================================================
//
// CmdExportInfoPascalBI
//
//==============================================================================
procedure CmdExportInfoPascalBI(const fname: string);
var
  fname1: string;
  s: string;
  sl: TDStringList;
begin
  if fname = '' then
  begin
    printf('Please specify the filename to save info export'#13#10);
    exit;
  end;

  if CharPos('.', fname) = 0 then
    fname1 := fname + '.txt'
  else
    fname1 := fname;

  fname1 := M_SaveFileName(fname1);

  s := Info_GetMobjinfoPascal(0, Ord(DO_NUMMOBJTYPES) - 1) + Info_GetStatesPascal(0, Ord(DO_NUMSTATES) - 1);

  sl := TDStringList.Create;
  try
    sl.Text := s;
    sl.SaveToFile(fname1);
    printf('Info settings saved to %s'#13#10, [fname1]);
  finally
    sl.Free;
  end;
end;

//==============================================================================
//
// CmdExportInfoPascalConst
//
//==============================================================================
procedure CmdExportInfoPascalConst(const fname: string);
var
  fname1: string;
  s: string;
  sl: TDStringList;
begin
  if fname = '' then
  begin
    printf('Please specify the filename to save info export'#13#10);
    exit;
  end;

  if CharPos('.', fname) = 0 then
    fname1 := fname + '.txt'
  else
    fname1 := fname;

  fname1 := M_SaveFileName(fname1);

  s := Info_GetMobjinfoPascalConst;

  sl := TDStringList.Create;
  try
    sl.Text := s;
    sl.SaveToFile(fname1);
    printf('Info settings saved to %s'#13#10, [fname1]);
  finally
    sl.Free;
  end;
end;

//==============================================================================
//
// CmdExportInfoPascalConstBI
//
//==============================================================================
procedure CmdExportInfoPascalConstBI(const fname: string);
var
  fname1: string;
  s: string;
  sl: TDStringList;
begin
  if fname = '' then
  begin
    printf('Please specify the filename to save info export'#13#10);
    exit;
  end;

  if CharPos('.', fname) = 0 then
    fname1 := fname + '.txt'
  else
    fname1 := fname;

  fname1 := M_SaveFileName(fname1);

  s := Info_GetMobjinfoPascalConst(0, Ord(DO_NUMMOBJTYPES) - 1);

  sl := TDStringList.Create;
  try
    sl.Text := s;
    sl.SaveToFile(fname1);
    printf('Info settings saved to %s'#13#10, [fname1]);
  finally
    sl.Free;
  end;
end;

var
  export_commands_registered: boolean = false;

//==============================================================================
//
// Info_InitExportCommands
//
//==============================================================================
procedure Info_InitExportCommands;
begin
  if export_commands_registered then
    exit;
  C_AddCmd('ExportInfoPascal, InfoExportPascal', @CmdExportInfoPascal);
  C_AddCmd('SaveInfoPascal, InfoSavePascal', @CmdExportInfoPascal);
  C_AddCmd('ExportInfoPascalBI, InfoExportPascalBI, ExportInfoPascalBuiltIn, InfoExportPascalBuiltIn', @CmdExportInfoPascalBI);
  C_AddCmd('SaveInfoPascalBI, InfoSavePascalBI, SaveInfoPascalBuiltIn, InfoSavePascalBuiltIn', @CmdExportInfoPascalBI);
  C_AddCmd('ExportInfoPascalConst, InfoExportPascalConst, SaveInfoPascalConst, InfoSavePascalConst', @CmdExportInfoPascalConst);
  C_AddCmd('ExportInfoPascalConstBI, InfoExportPascalConstBI, SaveInfoPascalConstBI, InfoSavePascalConstBI', @CmdExportInfoPascalConstBI);

  export_commands_registered := true;
end;

end.
