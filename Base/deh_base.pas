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

unit deh_base;

interface

uses
  d_delphi,
  d_think,
  m_fixed;

//==============================================================================
//
// DEH_NextLine
//
//==============================================================================
function DEH_NextLine(const s: TDStringList; var str: string; var counter: integer; const skipblanc: boolean = true): boolean;

//==============================================================================
//
// DEH_ParseText
//
//==============================================================================
procedure DEH_ParseText(const deh_tx: string);

//==============================================================================
//
// DEH_ParseLumpName
//
//==============================================================================
function DEH_ParseLumpName(const lumpname: string): boolean;

//==============================================================================
//
// DEH_ParseLumpNames
//
//==============================================================================
function DEH_ParseLumpNames(const lumpname: string): boolean;

//==============================================================================
//
// DEH_ParseLumpNum
//
//==============================================================================
procedure DEH_ParseLumpNum(const lump: integer);

//==============================================================================
//
// DEH_ParseFile
//
//==============================================================================
procedure DEH_ParseFile(const filename: string);

//==============================================================================
//
// DEH_StringToCString
//
//==============================================================================
function DEH_StringToCString(const s: string): string;

//==============================================================================
//
// DEH_CStringToString
//
//==============================================================================
function DEH_CStringToString(const s: string): string;

//==============================================================================
//
// DEH_StringValue
//
//==============================================================================
function DEH_StringValue(const s: string): string;

//==============================================================================
//
// DEH_PrintCurrentSettings
//
//==============================================================================
procedure DEH_PrintCurrentSettings;

//==============================================================================
//
// DEH_SaveCurrentSettings
//
//==============================================================================
procedure DEH_SaveCurrentSettings(const fname: string);

//==============================================================================
//
// DEH_CurrentActordef
//
//==============================================================================
function DEH_CurrentActordef: string;

//==============================================================================
//
// DEH_PrintActordef
//
//==============================================================================
procedure DEH_PrintActordef;

//==============================================================================
//
// DEH_SaveActordef
//
//==============================================================================
procedure DEH_SaveActordef(const fname: string);

//==============================================================================
//
// DEH_CurrentWeapondef
//
//==============================================================================
function DEH_CurrentWeapondef: string;

//==============================================================================
//
// DEH_PrintWeapondef
//
//==============================================================================
procedure DEH_PrintWeapondef;

//==============================================================================
//
// DEH_SaveWeapondef
//
//==============================================================================
procedure DEH_SaveWeapondef(const fname: string);

//==============================================================================
//
// DEH_CurrentStateOwners
//
//==============================================================================
function DEH_CurrentStateOwners: string;

//==============================================================================
//
// DEH_PrintStateOwners
//
//==============================================================================
procedure DEH_PrintStateOwners;

//==============================================================================
//
// DEH_SaveStateOwners
//
//==============================================================================
procedure DEH_SaveStateOwners(const fname: string);

//==============================================================================
//
// DEH_PrintActions
//
//==============================================================================
procedure DEH_PrintActions;

//==============================================================================
//
// DEH_FixedOrFloat
//
//==============================================================================
function DEH_FixedOrFloat(const token: string; const tolerance: integer): fixed_t;

//==============================================================================
//
// DEH_MobjInfoCSV
//
//==============================================================================
function DEH_MobjInfoCSV: TDStringList;

//==============================================================================
//
// DEH_SaveMobjInfoCSV
//
//==============================================================================
procedure DEH_SaveMobjInfoCSV(const fname: string);

//==============================================================================
//
// DEH_StatesCSV
//
//==============================================================================
function DEH_StatesCSV: TDStringList;

//==============================================================================
//
// DEH_SaveStatesCSV
//
//==============================================================================
procedure DEH_SaveStatesCSV(const fname: string);

//==============================================================================
//
// DEH_SpritesCSV
//
//==============================================================================
function DEH_SpritesCSV: TDStringList;

//==============================================================================
//
// DEH_SaveSpritesCSV
//
//==============================================================================
procedure DEH_SaveSpritesCSV(const fname: string);

//==============================================================================
//
// DEH_ActionName
//
//==============================================================================
function DEH_ActionName(action: actionf_t): string;

//==============================================================================
//
// DEH_InitActionsHash
//
//==============================================================================
procedure DEH_InitActionsHash;

//==============================================================================
//
// DEH_ShutDownActionsHash
//
//==============================================================================
procedure DEH_ShutDownActionsHash;

//==============================================================================
//
// DEH_AddActionToHash
//
//==============================================================================
procedure DEH_AddActionToHash(const act: string; const idpos: integer);

//==============================================================================
//
// DEH_SearchActionFromHash
//
//==============================================================================
function DEH_SearchActionFromHash(const act: string): integer;

//==============================================================================
//
// DEH_AmmoType
//
//==============================================================================
function DEH_AmmoType(const str: string): integer;

//==============================================================================
//
// DEH_WeaponType
//
//==============================================================================
function DEH_WeaponType(const str: string): integer;

//==============================================================================
//
// DEH_AddAction
//
//==============================================================================
procedure DEH_AddAction(const acp1: actionf_p1; const desc: string); overload;

//==============================================================================
//
// DEH_AddAction
//
//==============================================================================
procedure DEH_AddAction(const acp1: actionf_p1; const desc: string; const def_args: array of integer); overload;

{$IFDEF  HEXEN}

//==============================================================================
//
// DEH_PlayerClass
//
//==============================================================================
function DEH_PlayerClass(const str: string): integer;
{$ENDIF}

const
  DEH_STRINGLIST_HASH_SIZE = 64;

type
  // Assuming the assigned TDStringList is in UpperCase
  TDEHStringsHashTable = class(TObject)
  private
    positions: array[0..DEH_STRINGLIST_HASH_SIZE - 1] of TDNumberList;
    fList: TDTextList;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure AssignList(const s: TDTextList);
    function IndexOf(const value: string): integer;
    property List: TDTextList read fList;
  end;

implementation

uses
  TypInfo,
  deh_main,
  doomdef,
  {$IFDEF DOOM_OR_STRIFE}
  d_items,
  {$ENDIF}
  i_system,
  info,
  info_h,
  m_argv,
  sc_actordef,
  sc_states,
  w_folders,
  w_pak,
  w_wad;

//==============================================================================
//
// DEH_NextLine
//
//==============================================================================
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
  if CharPos('#', trimmed) = 1 then
  begin
    result := DEH_NextLine(s, str, counter);
    exit;
  end;
  if Pos('//', trimmed) = 1 then // JVAL: Allow // as comments also
  begin
    result := DEH_NextLine(s, str, counter);
    exit;
  end;
  strupperproc(str);
end;

//==============================================================================
//
// DEH_ParseText
//
//==============================================================================
procedure DEH_ParseText(const deh_tx: string);
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

//==============================================================================
//
// DEH_ParseLumpName
//
//==============================================================================
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

//==============================================================================
//
// DEH_ParseLumpNames
//
//==============================================================================
function DEH_ParseLumpNames(const lumpname: string): boolean;
var
  i: integer;
  cnt: integer;
  uName: string;
begin
  cnt := 0;
  uName := strupper(lumpname);
  for i := 0 to W_NumLumps - 1 do
    if char8tostring(W_GetNameForNum(i)) = uName then
    begin
      DEH_ParseText(W_TextLumpNum(i));
      inc(cnt);
    end;

  cnt := cnt + PAK_StringIterator(lumpname, DEH_ParseText);
  cnt := cnt + PAK_StringIterator(lumpname + '.DEH', DEH_ParseText);
  cnt := cnt + PAK_StringIterator(lumpname + '.BEX', DEH_ParseText);

  result := cnt > 0;
end;

//==============================================================================
//
// DEH_ParseLumpNum
//
//==============================================================================
procedure DEH_ParseLumpNum(const lump: integer);
begin
  if lump < 0 then
    exit;

  DEH_ParseText(W_TextLumpNum(lump));
end;

//==============================================================================
//
// DEH_ParseFile
//
//==============================================================================
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
    if CharPos('.', filename) = 0 then
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
      strm := TPakStream.Create(fnames[i], pm_short, '', FOLDER_DEHACKED);
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

//==============================================================================
//
// DEH_StringToCString
//
//==============================================================================
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

//==============================================================================
//
// DEH_CStringToString
//
//==============================================================================
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

//==============================================================================
//
// DEH_StringValue
//
//==============================================================================
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

//==============================================================================
//
// DEH_PrintCurrentSettings
//
//==============================================================================
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

//==============================================================================
//
// DEH_SaveCurrentSettings
//
//==============================================================================
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

  if CharPos('.', fname) = 0 then
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

//==============================================================================
//
// DEH_CurrentActordef
//
//==============================================================================
function DEH_CurrentActordef: string;
var
  m: integer;
begin
  result := '';
  for m := 0 to nummobjtypes - 1 do
    result := result + SC_GetActordefDeclaration(@mobjinfo[m]);
end;

//==============================================================================
//
// DEH_PrintActordef
//
//==============================================================================
procedure DEH_PrintActordef;
var
  s: TDSTringList;
  i: integer;
begin
  s := TDSTringList.Create;
  try
    s.Text := DEH_CurrentActordef;
    for i := 0 to s.Count - 1 do
      printf('%s'#13#10, [s[i]]);
  finally
    s.Free;
  end;
end;

//==============================================================================
//
// DEH_SaveActordef
//
//==============================================================================
procedure DEH_SaveActordef(const fname: string);
var
  s: TDSTringList;
  fname1: string;
begin
  if fname = '' then
  begin
    printf('Please specify the filename to save current ACTORDEF settings'#13#10);
    exit;
  end;

  if CharPos('.', fname) = 0 then
    fname1 := fname + '.txt'
  else
    fname1 := fname;

  fname1 := M_SaveFileName(fname1);

  s := TDSTringList.Create;
  try
    s.Text := DEH_CurrentActordef;
    s.SaveToFile(fname1);
    printf('ACTORDEF settings saved to %s'#13#10, [fname1]);
  finally
    s.Free;
  end;
end;

//==============================================================================
//
// DEH_CurrentWeapondef
//
//==============================================================================
function DEH_CurrentWeapondef: string;
var
  w: integer;
{$IFDEF HERETIC_OR_HEXEN}
  i: integer;
{$ENDIF}
begin
  result := '';
{$IFDEF HERETIC}
  for i := 1 to 2 do
{$ENDIF}
{$IFDEF HEXEN}
  for i := 0 to Ord(NUMCLASSES) - 1 do
{$ENDIF}
    for w := 0 to Ord(NUMWEAPONS) - 1 do
      result := result + SC_GetWeapondefDeclaration(w{$IFDEF HERETIC_OR_HEXEN}, i{$ENDIF});
end;

//==============================================================================
//
// DEH_PrintWeapondef
//
//==============================================================================
procedure DEH_PrintWeapondef;
var
  s: TDSTringList;
  i: integer;
begin
  s := TDSTringList.Create;
  try
    s.Text := DEH_CurrentWeapondef;
    for i := 0 to s.Count - 1 do
      printf('%s'#13#10, [s[i]]);
  finally
    s.Free;
  end;
end;

//==============================================================================
//
// DEH_SaveWeapondef
//
//==============================================================================
procedure DEH_SaveWeapondef(const fname: string);
var
  s: TDSTringList;
  fname1: string;
begin
  if fname = '' then
  begin
    printf('Please specify the filename to save current WEAPONDEF settings'#13#10);
    exit;
  end;

  if CharPos('.', fname) = 0 then
    fname1 := fname + '.txt'
  else
    fname1 := fname;

  fname1 := M_SaveFileName(fname1);

  s := TDSTringList.Create;
  try
    s.Text := DEH_CurrentWeapondef;
    s.SaveToFile(fname1);
    printf('WEAPONDEF settings saved to %s'#13#10, [fname1]);
  finally
    s.Free;
  end;
end;

//==============================================================================
//
// DEH_CurrentStateOwners
//
//==============================================================================
function DEH_CurrentStateOwners: string;
var
  i, j: integer;
  s1, s2: string;
begin
  result := '';
  for i := 0 to numstates - 1 do
  begin
    s1 := statenames.Strings[i];
    s2 := '';
    if states[i].owners <> nil then
    begin
      for j := 0 to states[i].owners.Count - 1 do
        s2 := s2 + '"' + strtrim(mobjinfo[states[i].owners.Numbers[j]].name) + '" ';
      trimproc(s2);
    end;
    result := result + s1 + '=' + s2 + #13#10;
  end;
end;

//==============================================================================
//
// DEH_PrintStateOwners
//
//==============================================================================
procedure DEH_PrintStateOwners;
var
  s: TDSTringList;
  i: integer;
begin
  s := TDSTringList.Create;
  try
    s.Text := DEH_CurrentStateOwners;
    for i := 0 to s.Count - 1 do
      printf('%s'#13#10, [s[i]]);
  finally
    s.Free;
  end;
end;

//==============================================================================
//
// DEH_SaveStateOwners
//
//==============================================================================
procedure DEH_SaveStateOwners(const fname: string);
var
  s: TDSTringList;
  fname1: string;
begin
  if fname = '' then
  begin
    printf('Please specify the filename to save current state owners'#13#10);
    exit;
  end;

  if CharPos('.', fname) = 0 then
    fname1 := fname + '.txt'
  else
    fname1 := fname;

  fname1 := M_SaveFileName(fname1);

  s := TDSTringList.Create;
  try
    s.Text := DEH_CurrentStateOwners;
    s.SaveToFile(fname1);
    printf('State owners saved to %s'#13#10, [fname1]);
  finally
    s.Free;
  end;
end;

//==============================================================================
//
// DEH_PrintActions
//
//==============================================================================
procedure DEH_PrintActions;
var
  i: integer;
begin
  for i := 0 to dehnumactions - 1 do
    printf('A_%s'#13#10, [deh_actions[i].name]);
end;

//==============================================================================
//
// DEH_FixedOrFloat
//
//==============================================================================
function DEH_FixedOrFloat(const token: string; const tolerance: integer): fixed_t;
var
  fv: float;
begin
  if (Pos('.', token) > 0) or (Pos(',', token) > 0) then
  begin
    fv := atof(token, 0);
    if fabs(fv) > tolerance then
      result := round(fv)
    else
      result := round(fv * FRACUNIT);
  end
  else
    result := atoi(token);
end;

//==============================================================================
//
// DEH_MobjInfoCSV
//
//==============================================================================
function DEH_MobjInfoCSV: TDStringList;
var
  i, idx1, idx2: integer;
  s1, s2, headstr, datstr: string;
  csstring: string;
  cs: TDStringList;
begin
  DEH_Init;

  cs := DEH_CurrentSettings;
  idx1 := cs.IndexOf('# Things');
  idx2 := cs.IndexOf('# States');

  headstr := '';
  for i := idx1 + 1 to idx2 - 1 do
  begin
    csstring := strtrim(cs.Strings[i]);
    if csstring <> '' then
      if CharPos('#', csstring) <> 1 then
        if Pos('//', csstring) < 1 then
        begin
          if Pos('THING ', strupper(csstring)) = 1 then
          begin
            if headstr = '' then
              headstr := '"id"'
            else
              break;
          end
          else if headstr <> '' then
          begin
            splitstring_ch(csstring, s1, s2, '=');
            trimproc(s1);
            headstr := headstr + ';' + '"' + s1 + '"';
          end;
        end;
  end;

  result := TDStringList.Create;
  result.Add(headstr);

  datstr := '';
  for i := idx1 + 1 to idx2 - 1 do
  begin
    csstring := strtrim(cs.Strings[i]);
    if csstring <> '' then
      if CharPos('#', csstring) <> 1 then
        if Pos('//', csstring) < 1 then
          if Pos('THING ', strupper(csstring)) < 1 then
          begin
            splitstring_ch(csstring, s1, s2, '=');
            trimproc(s2);
            if s2 = '' then
              s2 := '-';
            datstr := datstr + ';' + '"' + s2 + '"';
          end
          else
          begin
            if datstr <> '' then
              result.Add(datstr);
            datstr := '"' + itoa(result.Count - 1) + '"';
          end;
  end;
  if datstr <> '' then
    result.Add(datstr);
  cs.Free;
end;

//==============================================================================
//
// DEH_SaveMobjInfoCSV
//
//==============================================================================
procedure DEH_SaveMobjInfoCSV(const fname: string);
var
  s: TDSTringList;
  fname1: string;
begin
  if fname = '' then
  begin
    printf('Please specify the filename to save current MobjInfo'#13#10);
    exit;
  end;

  if CharPos('.', fname) = 0 then
    fname1 := fname + '.csv'
  else
    fname1 := fname;

  fname1 := M_SaveFileName(fname1);

  s := DEH_MobjInfoCSV;
  try
    s.SaveToFile(fname1);
    printf('MobjInfo saved to %s'#13#10, [fname1]);
  finally
    s.Free;
  end;
end;

//==============================================================================
//
// DEH_StatesCSV
//
//==============================================================================
function DEH_StatesCSV: TDStringList;
var
  i, j, idx1, idx2: integer;
  s1, s2, headstr, datstr: string;
  cs: TDStringList;

  function _statename(const x: integer): string;
  begin
    if x < Ord(DO_NUMSTATES) then
      result := strupper(GetENumName(TypeInfo(statenum_t), x))
    else
      result := 'S_STATE_' + itoa(x);
  end;

begin
  DEH_Init;

  cs := DEH_CurrentSettings;
  idx1 := cs.IndexOf('# States');
  idx2 := cs.IndexOf('# Weapons');

  headstr := '';
  for i := idx1 + 1 to idx2 - 1 do
    if strtrim(cs.Strings[i]) <> '' then
      if CharPos('#', strtrim(cs.Strings[i])) <> 1 then
        if Pos('//', strtrim(cs.Strings[i])) < 1 then
        begin
          if Pos('FRAME ', strtrim(strupper(cs.Strings[i]))) = 1 then
          begin
            if headstr = '' then
              headstr := '"Name";"id"'
            else
              break;
          end
          else if headstr <> '' then
          begin
            splitstring_ch(strtrim(cs.Strings[i]), s1, s2, '=');
            headstr := headstr + ';' + '"' + strtrim(s1) + '"';
          end;
        end;

  result := TDStringList.Create;
  result.Add(headstr);
  result.Add('"S_NULL";"0";"0";"0";"0";"-1";"0";"NULL";"0";"0";"0";"0"');

  datstr := '';
  for i := idx1 + 1 to idx2 - 1 do
  begin
    if strtrim(cs.Strings[i]) <> '' then
      if CharPos('#', strtrim(cs.Strings[i])) <> 1 then
        if Pos('//', strtrim(cs.Strings[i])) < 1 then
          if Pos('FRAME ', strtrim(strupper(cs.Strings[i]))) <> 1 then
          begin
            splitstring_ch(strtrim(cs.Strings[i]), s1, s2, '=');
            for j := 1 to length(s2) do
              if s2[j] = '"' then
                s2[j] := ' ';
            datstr := datstr + ';' + '"' + strtrim(s2) + '"';
          end
          else
          begin
            if datstr <> '' then
              result.Add('"' + _statename(result.Count - 1) + '";' + datstr);
            datstr := '"' + itoa(result.Count - 1) + '"';
          end;
  end;
  if datstr <> '' then
    result.Add('"' + _statename(result.Count - 1) + '";' + datstr);
  cs.Free;
end;

//==============================================================================
//
// DEH_SaveStatesCSV
//
//==============================================================================
procedure DEH_SaveStatesCSV(const fname: string);
var
  s: TDSTringList;
  fname1: string;
begin
  if fname = '' then
  begin
    printf('Please specify the filename to save current States'#13#10);
    exit;
  end;

  if CharPos('.', fname) = 0 then
    fname1 := fname + '.csv'
  else
    fname1 := fname;

  fname1 := M_SaveFileName(fname1);

  s := DEH_StatesCSV;
  try
    s.SaveToFile(fname1);
    printf('States saved to %s'#13#10, [fname1]);
  finally
    s.Free;
  end;
end;

//==============================================================================
//
// DEH_SpritesCSV
//
//==============================================================================
function DEH_SpritesCSV: TDStringList;
var
  i: integer;
  p: PByteArray;
begin
  result := TDStringList.Create;

  result.Add('"id";"Sprite"');

  for i := 0 to numsprites - 1 do
  begin
    p := @sprnames[i];
    result.Add(itoa(i + 1) + ';' + '"' + Chr(p[0]) + Chr(p[1]) + Chr(p[2]) + Chr(p[3]) + '"');
  end;
end;

//==============================================================================
//
// DEH_SaveSpritesCSV
//
//==============================================================================
procedure DEH_SaveSpritesCSV(const fname: string);
var
  s: TDSTringList;
  fname1: string;
begin
  if fname = '' then
  begin
    printf('Please specify the filename to save current Sprites'#13#10);
    exit;
  end;

  if CharPos('.', fname) = 0 then
    fname1 := fname + '.csv'
  else
    fname1 := fname;

  fname1 := M_SaveFileName(fname1);

  s := DEH_SpritesCSV;
  try
    s.SaveToFile(fname1);
    printf('Sprites saved to %s'#13#10, [fname1]);
  finally
    s.Free;
  end;
end;

//==============================================================================
//
// DEH_ActionName
//
//==============================================================================
function DEH_ActionName(action: actionf_t): string;
var
  i: integer;
begin
  for i := 0 to dehnumactions - 1 do
  begin
    if @deh_actions[i].action.acp1 = @action.acp1 then
    begin
      result := deh_actions[i].name;
      Exit;
    end;
  end;
  result := '';
end;

const
  DEH_ACTIONS_HASH_SIZE = 128;

var
  dehactionshasttable: array[0..DEH_ACTIONS_HASH_SIZE - 1] of TDStringList;

//==============================================================================
//
// DEH_InitActionsHash
//
//==============================================================================
procedure DEH_InitActionsHash;
var
  i: integer;
begin
  for i := 0 to DEH_ACTIONS_HASH_SIZE - 1 do
    dehactionshasttable[i] := TDStringList.Create;
end;

//==============================================================================
//
// DEH_ShutDownActionsHash
//
//==============================================================================
procedure DEH_ShutDownActionsHash;
var
  i, j: integer;
begin
  for i := 0 to DEH_ACTIONS_HASH_SIZE - 1 do
  begin
    for j := 0 to dehactionshasttable[i].Count - 1 do
      dehactionshasttable[i].Objects[j].Free;
    dehactionshasttable[i].Free;
  end;
end;

//==============================================================================
//
// DEH_FixActionName
//
//==============================================================================
function DEH_FixActionName(const act: string): string;
begin
  if Pos('A_', act) = 1 then
    result := strupper(Copy(act, 3, Length(act) - 2))
  else if Pos('a_', act) = 1 then
    result := strupper(Copy(act, 3, Length(act) - 2))
  else
    result := strupper(act);
end;

//==============================================================================
//
// dehactionhash
//
//==============================================================================
function dehactionhash(const act: string): LongWord;
var
  i: integer;
begin
  if act = '' then
  begin
    result := 0;
    exit;
  end;

  result := 5381 * 33 + Ord(act[1]);

  for i := 2 to Length(act) do
    result := result * 33 + Ord(act[i]);

  result := result and (DEH_ACTIONS_HASH_SIZE - 1);
end;

//==============================================================================
//
// DEH_AddActionToHash
//
//==============================================================================
procedure DEH_AddActionToHash(const act: string; const idpos: integer);
var
  hash: LongWord;
  str: string;
begin
  str := DEH_FixActionName(act);
  hash := dehactionhash(str);
  dehactionshasttable[hash].AddObject(str, TInteger.Create(idpos));
end;

//==============================================================================
//
// DEH_SearchActionFromHash
//
//==============================================================================
function DEH_SearchActionFromHash(const act: string): integer;
var
  hash: LongWord;
  str: string;
  i: integer;
begin
  str := DEH_FixActionName(act);
  hash := dehactionhash(str);
  for i := 0 to dehactionshasttable[hash].Count - 1 do
    if str = dehactionshasttable[hash].Strings[i] then
    begin
      result := (dehactionshasttable[hash].Objects[i] as TInteger).intnum;
      exit;
    end;

  result := -1;
end;

//==============================================================================
//
// dehstringhash
//
//==============================================================================
function dehstringhash(const s: string): LongWord;
var
  i: integer;
begin
  if s = '' then
  begin
    result := 0;
    exit;
  end;

  result := 5381 * 33 + Ord(toupper(s[Length(s)]));

  for i := Length(s) - 1 downto 1 do
    result := result * 33 + Ord(toupper(s[i]));

  result := result and (DEH_STRINGLIST_HASH_SIZE - 1);
end;

//==============================================================================
//
// TDEHStringsHashTable.Create
//
//==============================================================================
constructor TDEHStringsHashTable.Create;
var
  i: integer;
begin
  Inherited Create;
  for i := 0 to DEH_STRINGLIST_HASH_SIZE - 1 do
    positions[i] := TDNumberList.Create;
  fList := nil;
end;

//==============================================================================
//
// TDEHStringsHashTable.Destroy
//
//==============================================================================
destructor TDEHStringsHashTable.Destroy;
var
  i: integer;
begin
  for i := 0 to DEH_STRINGLIST_HASH_SIZE - 1 do
    positions[i].Free;
  Inherited Destroy;
end;

//==============================================================================
//
// TDEHStringsHashTable.AssignList
//
//==============================================================================
procedure TDEHStringsHashTable.AssignList(const s: TDTextList);
var
  i: integer;
  h: LongWord;
begin
  for i := 0 to DEH_STRINGLIST_HASH_SIZE - 1 do
    positions[i].Clear;
  fList := s;
  for i := 0 to fList.Count - 1 do
  begin
    h := dehstringhash(fList.Strings[i]);
    positions[h].Add(i);
  end;
end;

//==============================================================================
//
// TDEHStringsHashTable.IndexOf
//
//==============================================================================
function TDEHStringsHashTable.IndexOf(const value: string): integer;
var
  h: integer;
  i: integer;
  n: integer;
  check: string;
begin
  if flist = nil then
  begin
    result := -1;
    exit;
  end;

  check := strupper(value);
  h := dehstringhash(check);
  for i := 0 to positions[h].Count - 1 do
  begin
    n := positions[h].Numbers[i];
    if (n >= 0) and (n < fList.Count) then
      if fList.Strings[n] = check then
      begin
        result := n;
        exit;
      end;
  end;

  result := fList.IndexOf(check);
end;

//==============================================================================
//
// DEH_AmmoType
//
//==============================================================================
function DEH_AmmoType(const str: string): integer;
var
  stmp: string;
begin
  if ammotype_tokens = nil then
  begin
    result := atoi(str, -1);
    exit;
  end;

  if StrIsInteger(str) then
  begin
    result := atoi(str, -1);
    exit;
  end;

  stmp := strupper(str);
  {$IFNDEF HEXEN}
  if Pos('AM_', stmp) <> 1 then
    stmp := stmp + 'AM_';
  {$ENDIF}

  result := ammotype_tokens.IndexOf(stmp);
end;

//==============================================================================
//
// DEH_WeaponType
//
//==============================================================================
function DEH_WeaponType(const str: string): integer;
var
  stmp: string;
begin
  if weapontype_tokens = nil then
  begin
    result := atoi(str, -1);
    exit;
  end;

  if StrIsInteger(str) then
  begin
    result := atoi(str, -1);
    exit;
  end;

  stmp := strupper(str);
  if Pos('WP_', stmp) <> 1 then
    stmp := stmp + 'WP_';

  result := weapontype_tokens.IndexOf(stmp);
end;

//==============================================================================
//
// DEH_AddAction
//
//==============================================================================
procedure DEH_AddAction(const acp1: actionf_p1; const desc: string);
var
  aname: string;
  i: integer;
begin
  if dehnumactions >= DEHMAXACTIONS then
    I_Error('DEH_AddAction(): Trying to add more than %d actions', [DEHMAXACTIONS]);

  deh_actions[dehnumactions].action.acp1 := @acp1;
  aname := firstword(desc, [' ', ';', '(', '[', ':', #7, #9, #10, #13]);
  if Pos('A_', strupper(aname)) = 1 then
    Delete(aname, 1, 2);
  deh_actions[dehnumactions].originalname := aname;
  deh_actions[dehnumactions].name := strupper(aname);
  deh_actions[dehnumactions].argcount := 0;
  for i := 0 to MAX_STATE_ARGS - 1 do
    deh_actions[dehnumactions].default_args[i] := 0;
  {$IFDEF DLL}deh_actions[dehnumactions].decl := desc;{$ENDIF}
  Inc(dehnumactions);
end;

//==============================================================================
//
// DEH_AddAction
//
//==============================================================================
procedure DEH_AddAction(const acp1: actionf_p1; const desc: string; const def_args: array of integer); overload;
var
  aname: string;
  n_args, i: integer;
begin
  if dehnumactions >= DEHMAXACTIONS then
    I_Error('DEH_AddAction(): Trying to add more than %d actions', [DEHMAXACTIONS]);

  deh_actions[dehnumactions].action.acp1 := @acp1;
  aname := firstword(desc, [' ', ';', '(', '[', ':', #7, #9, #10, #13]);
  if Pos('A_', strupper(aname)) = 1 then
    Delete(aname, 1, 2);
  deh_actions[dehnumactions].originalname := aname;
  deh_actions[dehnumactions].name := strupper(aname);
  n_args := Length(def_args);
  deh_actions[dehnumactions].argcount := n_args;
  for i := 0 to n_args - 1 do
    deh_actions[dehnumactions].default_args[i] := def_args[i];
  for i := n_args to MAX_STATE_ARGS - 1 do
    deh_actions[dehnumactions].default_args[i] := 0;
  {$IFDEF DLL}deh_actions[dehnumactions].decl := desc;{$ENDIF}
  Inc(dehnumactions);
end;

{$IFDEF HEXEN}

//==============================================================================
//
// DEH_PlayerClass
//
//==============================================================================
function DEH_PlayerClass(const str: string): integer;
var
  stmp: string;
begin
  if playerclass_tokens = nil then
  begin
    result := atoi(str, -1);
    exit;
  end;

  if StrIsInteger(str) then
  begin
    result := atoi(str, -1);
    exit;
  end;

  stmp := strupper(str);
  if Pos('PCLASS_', stmp) <> 1 then
    stmp := stmp + 'PCLASS_';

  result := playerclass_tokens.IndexOf(stmp);
end;
{$ENDIF}

end.

