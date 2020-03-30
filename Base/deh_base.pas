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
//------------------------------------------------------------------------------
//  Site  : http://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit deh_base;

interface

uses
  d_delphi,
  d_think,
  m_fixed;

function DEH_NextLine(const s: TDStringList; var str: string; var counter: integer; const skipblanc: boolean = true): boolean;

procedure DEH_ParseText(const deh_tx: string);

function DEH_ParseLumpName(const lumpname: string): boolean;

procedure DEH_ParseLumpNum(const lump: integer);

procedure DEH_ParseFile(const filename: string);

function DEH_StringToCString(const s: string): string;

function DEH_CStringToString(const s: string): string;

function DEH_StringValue(const s: string): string;

procedure DEH_PrintCurrentSettings;

procedure DEH_SaveCurrentSettings(const fname: string);

function DEH_CurrentActordef: string;

procedure DEH_PrintActordef;

procedure DEH_SaveActordef(const fname: string);

procedure DEH_PrintActions;

function DEH_FixedOrFloat(const token: string; const tolerance: integer): fixed_t;

function DEH_MobjInfoCSV: TDStringList;

procedure DEH_SaveMobjInfoCSV(const fname: string);

function DEH_StatesCSV: TDStringList;

procedure DEH_SaveStatesCSV(const fname: string);

function DEH_SpritesCSV: TDStringList;

procedure DEH_SaveSpritesCSV(const fname: string);

function DEH_ActionName(action: actionf_t): string;

implementation

uses
  TypInfo,
  deh_main,
  i_system,
  info,
  info_h,
  m_argv,
  sc_actordef,
  w_folders,
  w_pak,
  w_wad;

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
  if Pos('//', trimmed) = 1 then // JVAL: Allow // as comments also
  begin
    result := DEH_NextLine(s, str, counter);
    exit;
  end;
  str := strupper(str);
end;

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

  DEH_ParseText(W_TextLumpNum(lump));
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

function DEH_CurrentActordef: string;
var
  m: integer;
begin
  result := '';
  for m := 0 to nummobjtypes - 1 do
    result := result + SC_GetActordefDeclaration(@mobjinfo[m]);
end;

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

  if Pos('.', fname) = 0 then
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

procedure DEH_PrintActions;
var
  i: integer;
begin
  for i := 0 to DEHNUMACTIONS - 1 do
    printf('A_%s'#13#10, [deh_actions[i].name]);
end;

function DEH_FixedOrFloat(const token: string; const tolerance: integer): fixed_t;
var
  fv: float;
begin
  if (Pos('.', token) > 0) or (Pos(',', token) > 0) then
  begin
    fv := atof(token, 0);
    if fv > tolerance then
      result := round(fv)
    else
      result := round(fv * FRACUNIT);
  end
  else
    result := atoi(token);
end;

function DEH_MobjInfoCSV: TDStringList;
var
  i, idx1, idx2: integer;
  s1, s2, headstr, datstr: string;
  cs: TDStringList;
begin
  DEH_Init;

  cs := DEH_CurrentSettings;
  idx1 := cs.IndexOf('# Things');
  idx2 := cs.IndexOf('# States');

  headstr := '';
  for i := idx1 + 1 to idx2 + 1 do
    if strtrim(cs.Strings[i]) <> '' then
      if Pos('#', strtrim(cs.Strings[i])) <> 1 then
        if Pos('//', strtrim(cs.Strings[i])) < 1 then
        begin
          if Pos('THING ', strtrim(strupper(cs.Strings[i]))) = 1 then
          begin
            if headstr = '' then
              headstr := '"id"'
            else
              break;
          end
          else if headstr <> '' then
          begin
            splitstring(strtrim(cs.Strings[i]), s1, s2, '=');
            headstr := headstr + ';' + '"' + strtrim(s1) + '"';
          end;
        end;

  result := TDStringList.Create;
  result.Add(headstr);

  datstr := '';
  for i := idx1 + 1 to idx2 - 1 do
  begin
    if strtrim(cs.Strings[i]) <> '' then
      if Pos('#', strtrim(cs.Strings[i])) <> 1 then
        if Pos('//', strtrim(cs.Strings[i])) < 1 then
          if Pos('THING ', strtrim(strupper(cs.Strings[i]))) < 1 then
          begin
            splitstring(strtrim(cs.Strings[i]), s1, s2, '=');
            datstr := datstr + ';' + '"' + strtrim(s2) + '"';
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

  if Pos('.', fname) = 0 then
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
      if Pos('#', strtrim(cs.Strings[i])) <> 1 then
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
            splitstring(strtrim(cs.Strings[i]), s1, s2, '=');
            headstr := headstr + ';' + '"' + strtrim(s1) + '"';
          end;
        end;

  result := TDStringList.Create;
  result.Add(headstr);
  result.Add('"S_NULL";"0";"0";"0";"0";"-1";"0";"NULL";"0";"0";"0"');

  datstr := '';
  for i := idx1 + 1 to idx2 - 1 do
  begin
    if strtrim(cs.Strings[i]) <> '' then
      if Pos('#', strtrim(cs.Strings[i])) <> 1 then
        if Pos('//', strtrim(cs.Strings[i])) < 1 then
          if Pos('FRAME ', strtrim(strupper(cs.Strings[i]))) <> 1 then
          begin
            splitstring(strtrim(cs.Strings[i]), s1, s2, '=');
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

  if Pos('.', fname) = 0 then
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

  if Pos('.', fname) = 0 then
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

function DEH_ActionName(action: actionf_t): string;
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

end.

