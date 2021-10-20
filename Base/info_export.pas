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

unit info_export;

interface

function Info_GetMobjinfoPascal(const astart: integer = 0; const afinish: integer = -1): string;

function Info_GetStatesPascal(const astart: integer = 0; const afinish: integer = -1): string;

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
  m_argv,
  m_fixed,
  p_gender,
  r_renderstyle,
  sounds;

function _state_name_Ord(const x: Integer): string;
begin
  if x < 0 then
    Result := 'Ord(S_NULL)'
  else if x < Ord(DO_NUMSTATES) then
    Result := 'Ord(' + strupper(GetENumName(TypeInfo(statenum_t), x)) + ')'
  else
    Result := itoa(x);
end;

function _state_name(const x: Integer): string;
begin
  if x < 0 then
    Result := 'S_NULL'
  else if x < Ord(DO_NUMSTATES) then
    Result := strupper(GetENumName(TypeInfo(statenum_t), x))
  else
    Result := 'statenum_t(' + itoa(x) + ')';
end;

function _sound_name_Ord(const x: Integer): string;
begin
  if x < 0 then
    Result := 'Ord(sfx_None)'
  else if x < Ord(DO_NUMMUSIC) then
    Result := 'Ord(' + GetENumName(TypeInfo(sfxenum_t), x) + ')'
  else
    Result := itoa(x);
end;

function _mobjinfo_name_Ord(const x: Integer): string;
begin
  if x < 0 then
    Result := '-1'
  else if x < Ord(DO_NUMMOBJTYPES) then
    Result := 'Ord(' + strupper(GetENumName(TypeInfo(mobjtype_t), x)) + ')'
  else
    Result := itoa(x);
end;

function _gender_name(const x: Integer): string;
begin
  if x < 0 then
    Result := GetENumName(TypeInfo(gender_t), 0)
  else if x < Ord(NUMGENDERS) then
    Result := GetENumName(TypeInfo(gender_t), x)
  else
    Result := 'gender_t(' + itoa(x) + ')';
end;

function _renderstyle_name(const x: Integer): string;
begin
  if x < 0 then
    Result := GetENumName(TypeInfo(mobjrenderstyle_t), 0)
  else if x < Ord(NUMGENDERS) then
    Result := GetENumName(TypeInfo(mobjrenderstyle_t), x)
  else
    Result := 'mobjrenderstyle_t(' + itoa(x) + ')';
end;

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

function _spritenum_name_Ord(const x: Integer): string;
begin
  if x < 0 then
    Result := 'Ord(SPR_NULL)'
  else if x < Ord(DO_NUMSPRITES) then
    Result := 'Ord(' + GetENumName(TypeInfo(spritenum_t), x) + ')'
  else
    Result := itoa(x);
end;

function _action_name(const action: actionf_t): string;
var
  i: integer;
begin
  for i := 0 to DEHNUMACTIONS - 1 do
    if @deh_actions[i].action.acp1 = @action.acp1 then
    begin
      Result := '@A_' + capitalizedstring(deh_actions[i].name);
      if Result = '@A_Null' then
        Result := 'nil';
      Exit;
    end;

  Result := 'nil';
end;

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
    AddLn('mobjinfo[' + mname + '].name := ''' + mobjinfo[i].name + ''';');
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
    AddLn('mobjinfo[' + mname + '].speed := ' + itoa(mobjinfo[i].speed) + ';');
    AddLn('mobjinfo[' + mname + '].radius := ' + itoa_FRACUNIT(mobjinfo[i].radius) + ';');
    AddLn('mobjinfo[' + mname + '].height := ' + itoa_FRACUNIT(mobjinfo[i].height) + ';');
    AddLn('mobjinfo[' + mname + '].mass := ' + itoa(mobjinfo[i].mass) + ';');
    AddLn('mobjinfo[' + mname + '].damage := ' + itoa(mobjinfo[i].damage) + ';');
    AddLn('mobjinfo[' + mname + '].activesound := ' + _sound_name_Ord(mobjinfo[i].activesound) + ';');
    AddLn('mobjinfo[' + mname + '].flags := ' + itoa_FLAGS(mobjinfo[i].flags, mobj_flags) + ';');
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
    AddLn('mobjinfo[' + mname + '].interactstate := ' + _state_name_Ord(mobjinfo[i].interactstate) + ';');
    AddLn('mobjinfo[' + mname + '].missileheight := ' + itoa(mobjinfo[i].missileheight) + ';');
    AddLn('mobjinfo[' + mname + '].vspeed := ' + itoa(mobjinfo[i].vspeed) + ';');
    AddLn('mobjinfo[' + mname + '].pushfactor := ' + itoa(mobjinfo[i].pushfactor) + ';');
    AddLn('mobjinfo[' + mname + '].friction := ' + itoa(mobjinfo[i].friction) + ';');
    AddLn('mobjinfo[' + mname + '].scale := ' + itoa_FRACUNIT(mobjinfo[i].scale) + ';');
    AddLn('mobjinfo[' + mname + '].gravity := ' + itoa_FRACUNIT(mobjinfo[i].gravity) + ';');
    AddLn('mobjinfo[' + mname + '].flags3_ex := ' + itoa_FLAGS(mobjinfo[i].flags3_ex, mobj_flags3_ex) + ';');
    AddLn('mobjinfo[' + mname + '].flags4_ex := ' + itoa_FLAGS(mobjinfo[i].flags4_ex, mobj_flags4_ex) + ';');
    AddLn('mobjinfo[' + mname + '].minmissilechance := ' + itoa(mobjinfo[i].minmissilechance) + ';');
    AddLn('mobjinfo[' + mname + '].floatspeed := ' + itoa(mobjinfo[i].floatspeed) + ';');
    AddLn('mobjinfo[' + mname + '].normalspeed := ' + itoa(mobjinfo[i].normalspeed) + ';');
    AddLn('mobjinfo[' + mname + '].fastspeed := ' + itoa(mobjinfo[i].fastspeed) + ';');
    AddLn('mobjinfo[' + mname + '].obituary: ''' + mobjinfo[i].obituary + ''';');
    AddLn('mobjinfo[' + mname + '].hitobituary: ''' + mobjinfo[i].hitobituary + ''';');
    AddLn('mobjinfo[' + mname + '].gender := ' + _gender_name(Ord(mobjinfo[i].gender)) + ';');
    AddLn('mobjinfo[' + mname + '].meleerange := ' + itoa(mobjinfo[i].meleerange) + ';');
    AddLn('mobjinfo[' + mname + '].maxstepheight := ' + itoa(mobjinfo[i].maxstepheight) + ';');
    AddLn('mobjinfo[' + mname + '].maxdropoffheight := ' + itoa(mobjinfo[i].maxdropoffheight) + ';');
    AddLn('mobjinfo[' + mname + '].gibhealth := ' + itoa(mobjinfo[i].gibhealth) + ';');
    AddLn('mobjinfo[' + mname + '].maxtargetrange := ' + itoa(mobjinfo[i].maxtargetrange) + ';');
    AddLn('mobjinfo[' + mname + '].WeaveIndexXY := ' + itoa(mobjinfo[i].WeaveIndexXY) + ';');
    AddLn('mobjinfo[' + mname + '].WeaveIndexZ := ' + itoa(mobjinfo[i].WeaveIndexZ) + ';');
    AddLn('');
  end;

  Result := ret;
end;

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
    AddLn('states[' + sname + '].flags_ex: ' + itoa_FLAGS(states[i].flags_ex, state_flags_ex) + ';');

    AddLn('');
  end;

  Result := ret;
end;

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

  if Pos('.', fname) = 0 then
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

  if Pos('.', fname) = 0 then
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

var
  export_commands_registered: boolean = false;

procedure Info_InitExportCommands;
begin
  if export_commands_registered then
    exit;
  C_AddCmd('ExportInfoPascal, InfoExportPascal', @CmdExportInfoPascal);
  C_AddCmd('ExportInfoPascalBI, InfoExportPascalBI, ExportInfoPascalBuiltIn, InfoExportPascalBuiltIn', @CmdExportInfoPascalBI);

  export_commands_registered := true;
end;

end.
