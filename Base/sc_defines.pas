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
//
// DESCRIPTION:
//  Defines preprocessor
//
//------------------------------------------------------------------------------
//  Site  : https://sourceforge.net/projects/delphidoom/
//------------------------------------------------------------------------------

{$I Doom32.inc}

unit sc_defines;

interface

uses
  SysUtils, Classes;

type
  TDefineState = class(TObject)
  private
    FInElse: Boolean;
    FDoWrite: Boolean;
  public
    property InElse: Boolean read FInElse write FInElse;
    property DoWrite: Boolean read FDoWrite write FDoWrite;
  end;

  TDefineStates = class(TObject)
  private
    FItems: TList;
    function GetCount: Longint;
    function GetItem(I: Integer): TDefineState;
    function GetWrite: Boolean;
    function GetPrevWrite: Boolean; //JeromeWelsh - nesting fix
  public
    constructor Create;
    destructor Destroy; override;
    property Count: Longint read GetCount;
    property Item[I: Longint]: TDefineState read GetItem; default;
    function Add: TDefineState;
    procedure Delete(I: Longint);
    procedure Clear;
    property DoWrite: Boolean read GetWrite;
    property DoPrevWrite: Boolean read GetPrevWrite; //JeromeWelsh - nesting fix
  end;

  TDefinesPreprocessor = class(TObject)
  private
    FDefines: TStringList;
    FDefineState: TDefineStates;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure AddDefine(const def: string);
    function Preprocess(const inp: string): string;
  end;

var
  gamedefines: TStringList;

//==============================================================================
//
// SC_InitGameDefines
//
//==============================================================================
procedure SC_InitGameDefines;

//==============================================================================
//
// SC_ShutDownGameDefines
//
//==============================================================================
procedure SC_ShutDownGameDefines;

//==============================================================================
//
// SC_AddDefine
//
//==============================================================================
procedure SC_AddDefine(const adef: string);

implementation

uses
  i_system;

//==============================================================================
//
// TDefineStates.Create
//
//==============================================================================
constructor TDefineStates.Create;
begin
  inherited Create;
  FItems := TList.Create;
end;

//==============================================================================
//
// TDefineStates.Destroy
//
//==============================================================================
destructor TDefineStates.Destroy;
var
  i: integer;
begin
  for i := FItems.Count - 1 downto 0 do
    TDefineState(FItems[i]).Free;
  FItems.Free;
  inherited Destroy;
end;

//==============================================================================
//
// TDefineStates.Add
//
//==============================================================================
function TDefineStates.Add: TDefineState;
begin
  Result := TDefineState.Create;
  FItems.Add(Result);
end;

//==============================================================================
//
// TDefineStates.Clear
//
//==============================================================================
procedure TDefineStates.Clear;
var
  i: integer;
begin
  for i := Longint(FItems.Count) - 1 downto 0 do
    TDefineState(FItems[i]).Free;
  FItems.Clear;
end;

//==============================================================================
//
// TDefineStates.Delete
//
//==============================================================================
procedure TDefineStates.Delete(i: integer);
begin
  TDefineState(FItems[i]).Free;
  FItems.Delete(i);
end;

//==============================================================================
//
// TDefineStates.GetCount
//
//==============================================================================
function TDefineStates.GetCount: integer;
begin
  Result := FItems.Count;
end;

//==============================================================================
//
// TDefineStates.GetItem
//
//==============================================================================
function TDefineStates.GetItem(i: integer): TDefineState;
begin
  Result := FItems[i];
end;

//==============================================================================
//
// TDefineStates.GetWrite
//
//==============================================================================
function TDefineStates.GetWrite: Boolean;
begin
  if FItems.Count = 0 then
    Result := True
  else
    Result := TDefineState(FItems[FItems.Count - 1]).DoWrite;
end;

//==============================================================================
// TDefineStates.GetPrevWrite
//
//JeromeWelsh - nesting fix
//
//==============================================================================
function TDefineStates.GetPrevWrite: Boolean;
begin
  if FItems.Count < 2 then
    Result := True
  else
    Result := TDefineState(FItems[FItems.Count - 2]).DoWrite;
end;

//==============================================================================
// TDefinesPreprocessor.Create
//
//------------------------------------------------------------------------------
//
//==============================================================================
constructor TDefinesPreprocessor.Create;
begin
  FDefines := TStringList.Create;
  FDefineState := TDefineStates.Create;
  inherited;
end;

//==============================================================================
//
// TDefinesPreprocessor.Destroy
//
//==============================================================================
destructor TDefinesPreprocessor.Destroy;
begin
  FDefines.Free;
  FDefineState.Free;
  inherited;
end;

//==============================================================================
//
// TDefinesPreprocessor.AddDefine
//
//==============================================================================
procedure TDefinesPreprocessor.AddDefine(const def: string);
var
  adef: string;
begin
  adef := UpperCase(def);
  if FDefines.IndexOf(adef) < 0 then
    FDefines.Add(adef);
end;

//==============================================================================
//
// splitstring
//
//==============================================================================
procedure splitstring(const inp: string; var out1, out2: string; const splitter: string = ' ');
var
  p: integer;
begin
  p := Pos(splitter, inp);
  if p = 0 then
  begin
    out1 := inp;
    out2 := '';
  end
  else
  begin
    out1 := Trim(Copy(inp, 1, p - 1));
    out2 := Trim(Copy(inp, p + 1, Length(inp) - p));
  end;
end;

//==============================================================================
//
// TDefinesPreprocessor.Preprocess
//
//==============================================================================
function TDefinesPreprocessor.Preprocess(const inp: string): string;
var
  sinp, sout: TStringList;
  i: integer;
  line: string;
  stmp, s1, s2: string;
  dw: boolean;
  ds: TDefineState;
  oldDefines: TStringList;

  procedure _Error(const err: string);
  var
    j: integer;
  begin
    I_Warning('TDefinesPreprocessor.Preprocess(): ' + err + ' (line ' + IntToStr(i) + ')'#13#10);
    for j := i - 3 to i + 3 do
    begin
      if j >= 0 then
        if j < sinp.Count then
        begin
          if j = 0 then
            I_Warning('                                  ' + sinp.Strings[j] + #13#10)
          else
            I_Warning('--------------------------------->' + sinp.Strings[j] + #13#10);
        end;
    end;
  end;

begin
  oldDefines := TStringList.Create;
  oldDefines.AddStrings(FDefines);

  sinp := TStringList.Create;
  sout := TStringList.Create;

  sinp.Text := inp;
  for i := 0 to sinp.Count - 1 do
  begin
    line := sinp.Strings[i];
    stmp := UpperCase(Trim(line));
    splitstring(stmp, s1, s2);
    if s1 = '#DEFINE' then
    begin
      if s2 = '' then
        _Error('Identifier expected after #define keyword')
      else
        AddDefine(s2);
    end
    else if s1 = '#UNDEF' then
    begin
      if s2 = '' then
        _Error('Identifier expected after #undef keyword')
      else if FDefines.IndexOf(s2) < 0 then
        _Error('"' + s2 + '" is not defined!')
      else
        FDefines.Delete(FDefines.IndexOf(s2));
    end
    else if s1 = '#IFDEF' then
    begin
      dw := (FDefines.IndexOf(s2) >= 0) and FDefineState.DoWrite;
      FDefineState.Add.DoWrite := dw;
    end
    else if s1 = '#IFNDEF' then
    begin
      dw := (FDefines.IndexOf(s2) < 0) and FDefineState.DoWrite;
      FDefineState.Add.DoWrite := dw;
    end
    else if s1 = '#ENDIF' then
    begin
      if FDefineState.Count = 0 then
         _Error('#ENDIF without opening')
      else
        FDefineState.Delete(FDefineState.Count - 1);
    end
    else if s1 = '#ELSE' then
    begin
      if s2 <> '' then
        _Error('#ELSE keyword followed by "' + s2 + '" is not allowed')
      else if FDefineState.Count = 0 then
        _Error('#ELSE without opening')
      else
      begin
        ds := FDefineState[FDefineState.Count - 1];
        if ds.InElse then
          _Error('#ELSE keyword used twice')
        else
        begin
          ds.FInElse := True;
          ds.DoWrite := not ds.DoWrite and FDefineState.DoPrevWrite;
        end
      end;
    end
    else if s1 = '#EXITIF' then
    begin
      if FDefineState.DoWrite then
        if FDefines.IndexOf(s2) >= 0 then
          Break;
    end
    else if FDefineState.DoWrite then
      sout.Add(line);
  end;
  result := sout.Text;

  sinp.Free;
  sout.Free;

  FDefines.Clear;
  FDefines.AddStrings(oldDefines);
  oldDefines.Free;
end;

//==============================================================================
//
// SC_InitGameDefines
//
//==============================================================================
procedure SC_InitGameDefines;
begin
  gamedefines := TStringList.Create;
end;

//==============================================================================
//
// SC_ShutDownGameDefines
//
//==============================================================================
procedure SC_ShutDownGameDefines;
begin
  gamedefines.Free;
end;

//==============================================================================
//
// SC_AddDefine
//
//==============================================================================
procedure SC_AddDefine(const adef: string);
var
  udef: string;
begin
  udef := UpperCase(adef);
  if gamedefines.IndexOf(udef) < 0 then
    gamedefines.Add(udef);
end;

end.
