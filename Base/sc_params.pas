//------------------------------------------------------------------------------
//
//  DelphiDoom: A modified and improved DOOM engine for Windows
//  based on original Linux Doom as published by "id Software"
//  Copyright (C) 1993-1996 by id Software, Inc.
//  Copyright (C) 2004-2019 by Jim Valavanis
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

unit sc_params;

//
// JVAL
// Dynamic Custom Parameter List for ACTORDEF LUMPS 

interface

uses
  m_fixed;

const
  GLBF_MAP_STRING = 1;
  GLBF_MAP_INTEGER = 2;
  GLBF_MAP_FLOAT = 3;
  GLBF_WORLD_STRING = 4;
  GLBF_WORLD_INTEGER = 5;
  GLBF_WORLD_FLOAT = 6;
  GLBF_MOBJ_CUSTOMPARM = 7;
  GLBF_MOBJ_TARGETCUSTOMPARM = 8;

type
  customparam_t = record
    s_param: string[64];
    i_param: integer;
    i_parm1, i_parm2: integer;
    f_param: single;
    f_parm1, f_parm2: single;
    fx_param: fixed_t;
    globalidx: integer;
    israndom: byte;
    computed: boolean;
  end;
  Pcustomparam_t = ^customparam_t;
  customparam_tArray = array[0..$FFF] of customparam_t;
  Pcustomparam_tArray = ^customparam_tArray;

  TCustomParamList = class
  private
    fList: Pcustomparam_tArray;
    fNumItems: integer;
    fActor: pointer;
  protected
    function GetIsComputed(index: integer): boolean; virtual;
    function GetInteger(index: integer): integer; virtual;
    procedure PutInteger(index: Integer; const value: integer); virtual;
    function GetFloat(index: integer): single; virtual;
    procedure PutFloat(index: Integer; const value: single); virtual;
    function GetFixed(index: integer): fixed_t; virtual;
    function GetString(index: integer): string; virtual;
    function GetDeclaration(index: integer): string; virtual;
  public
    constructor Create; virtual;
    constructor CreateFromText(const tx: string); virtual;
    destructor Destroy; override;
    procedure Add(const value: string); virtual;
    property Count: integer read fNumItems;
    property IsComputed[index: Integer]: boolean read GetIsComputed;
    property IntVal[index: Integer]: integer read GetInteger write PutInteger;
    property FloatVal[index: Integer]: single read GetFloat write PutFloat;
    property FixedVal[index: Integer]: fixed_t read GetFixed;
    property StrVal[index: Integer]: string read GetString;
    property Declaration[index: Integer]: string read GetDeclaration;
    property Actor: pointer read fActor write fActor;
  end;

implementation

uses
  d_delphi,
  m_rnd,
  psi_globals,
  p_mobj_h,
  p_params,
  sc_engine;

constructor TCustomParamList.Create;
begin
  fList := nil;
  fNumItems := 0;
  fActor := nil;
end;

constructor TCustomParamList.CreateFromText(const tx: string);
var
  i: integer;
  sc: TScriptEngine;
  token1: string;
  token: string;
  utoken: string;
  inquote: boolean;
begin
  Create;

  token1 := tx;
  inquote := False;
  for i := 1 to length(token1) do
  begin
    if token1[i] in ['{', ',', '}', '(', ')'] then
      if not inquote then
        token1[i] := ' ';
    if (token1[i] = '"') or (token1[i] = '''') then
      inquote := not inquote;
  end;

  sc := TScriptEngine.Create(token1);
  try
    while sc.GetString do
    begin
      token := sc._String;
      utoken := strupper(token);
      if utoken = 'RANDOM' then
      begin
        for i := 0 to 1 do
        begin
          if sc.GetString then
            token := token + ' ' + sc._String
          else
            break;
        end;
      end
      else if (utoken = 'MAPSTR') or
              (utoken = 'WORLDSTR') or
              (utoken = 'MAPINT') or
              (utoken = 'WORLDINT') or
              (utoken = 'MAPFLOAT') or
              (utoken = 'WORLDFLOAT') or
              (utoken = 'CUSTOMPARAM') or
              (utoken = 'TARGETCUSTOMPARAM') then
      begin
        if sc.GetString then
          token := token + ' ' + sc._String;
      end;
      Add(token);
    end;
  finally
    sc.Free;
  end;
end;

destructor TCustomParamList.Destroy;
begin
  if fNumItems > 0 then
    realloc(pointer(fList), fNumItems * SizeOf(customparam_t), 0);
end;

procedure TCustomParamList.Add(const value: string);
var
  ival: integer;
  fval: single;
  token, token1, token2: string;
  utoken: string;
begin
  realloc(pointer(fList), fNumItems * SizeOf(customparam_t), (fNumItems + 1) * SizeOf(customparam_t));
  splitstring(value, token1, token);
  utoken := strupper(token1);
  if utoken = 'RANDOM' then
  begin
    fList[fNumItems].israndom := 1;
    fList[fNumItems].globalidx := 0;
    fList[fNumItems].s_param := '';
    fList[fNumItems].computed := false;
    if token = '' then
    begin
      fList[fNumItems].i_parm1 := 0;
      fList[fNumItems].i_parm2 := 255;
    end
    else
    begin
      splitstring(token, token1, token2);
      if token2 = '' then
      begin
        fList[fNumItems].i_parm1 := 0;
        fList[fNumItems].i_parm2 := atoi(token1, 255);
      end
      else
      begin
        fList[fNumItems].i_parm1 := atoi(token1, 0);
        fList[fNumItems].i_parm2 := atoi(token2, 255);
      end;
    end;
  end
  else if utoken = 'FLOATRANDOM' then
  begin
    fList[fNumItems].israndom := 2;
    fList[fNumItems].globalidx := 0;
    fList[fNumItems].s_param := '';
    fList[fNumItems].computed := false;
    if token = '' then
    begin
      fList[fNumItems].f_parm1 := 0;
      fList[fNumItems].f_parm2 := 255;
    end
    else
    begin
      splitstring(token, token1, token2);
      if token2 = '' then
      begin
        fList[fNumItems].f_parm1 := 0;
        fList[fNumItems].f_parm2 := atof(token1, 255);
      end
      else
      begin
        fList[fNumItems].f_parm1 := atof(token1, 0);
        fList[fNumItems].f_parm2 := atof(token2, 255);
      end;
    end;
  end
  else
  begin
    fList[fNumItems].israndom := 0;
    if utoken = 'MAPSTR' then
    begin
      fList[fNumItems].globalidx := GLBF_MAP_STRING;
      fList[fNumItems].s_param := token;
    end
    else if utoken = 'WORLDSTR' then
    begin
      fList[fNumItems].globalidx := GLBF_WORLD_STRING;
      fList[fNumItems].s_param := token;
    end
    else if utoken = 'MAPINT' then
    begin
      fList[fNumItems].globalidx := GLBF_MAP_INTEGER;
      fList[fNumItems].s_param := token;
    end
    else if utoken = 'WORLDINT' then
    begin
      fList[fNumItems].globalidx := GLBF_WORLD_INTEGER;
      fList[fNumItems].s_param := token;
    end
    else if utoken = 'MAPFLOAT' then
    begin
      fList[fNumItems].globalidx := GLBF_MAP_FLOAT;
      fList[fNumItems].s_param := token;
    end
    else if utoken = 'WORLDFLOAT' then
    begin
      fList[fNumItems].globalidx := GLBF_WORLD_FLOAT;
      fList[fNumItems].s_param := token;
    end
    else if utoken = 'CUSTOMPARAM' then
    begin
      fList[fNumItems].globalidx := GLBF_MOBJ_CUSTOMPARM;
      fList[fNumItems].s_param := token;
    end
    else if utoken = 'TARGETCUSTOMPARAM' then
    begin
      fList[fNumItems].globalidx := GLBF_MOBJ_TARGETCUSTOMPARM;
      fList[fNumItems].s_param := token;
    end
    else
    begin
      fList[fNumItems].globalidx := 0;
      fList[fNumItems].s_param := value;
    end;
    ival := atoi(value);
    fList[fNumItems].i_param := ival;
    fList[fNumItems].computed := itoa(ival) = value;
    if fList[fNumItems].computed then
    begin
      fList[fNumItems].f_param := ival;
      fList[fNumItems].fx_param := ival * FRACUNIT;
    end
    else
    begin
      fval := atof(value);
      fList[fNumItems].f_param := fval;
      fList[fNumItems].fx_param := round(fval * FRACUNIT);
      fList[fNumItems].computed := ftoa(ival) = value;
    end;
  end;

  inc(fNumItems);
end;

function TCustomParamList.GetIsComputed(index: integer): boolean;
begin
  result := fList[index].computed;
end;

function TCustomParamList.GetInteger(index: integer): integer;
var
  parm: Pmobjcustomparam_t;
begin
  if (index >= 0) and (index < fNumItems) then
  begin
    if fList[index].israndom = 1 then
      result := fList[index].i_parm1 + (N_Random * (fList[index].i_parm2 - fList[index].i_parm1 + 1)) div 256
    else if fList[index].israndom = 2 then
      result := round(fList[index].f_parm1 + (N_Random * (fList[index].f_parm2 - fList[index].f_parm1 + 1)) / 256)
    else
    begin
      case fList[index].globalidx of
        GLBF_MAP_STRING:
          result := atoi(PS_GetMapStr(fList[index].s_param), 0);
        GLBF_MAP_INTEGER:
          result := PS_GetMapInt(fList[index].s_param);
        GLBF_MAP_FLOAT:
          result := round(PS_GetMapFloat(fList[index].s_param));
        GLBF_WORLD_STRING:
          result := atoi(PS_GetWorldStr(fList[index].s_param), 0);
        GLBF_WORLD_INTEGER:
          result := PS_GetWorldInt(fList[index].s_param);
        GLBF_WORLD_FLOAT:
          result := round(PS_GetWorldFloat(fList[index].s_param));
        GLBF_MOBJ_CUSTOMPARM:
          begin
            result := 0;
            if fActor <> nil then
            begin
              parm := P_GetMobjCustomParam(fActor, fList[index].s_param);
              if parm <> nil then
                result := parm.value;
            end;
          end;
        GLBF_MOBJ_TARGETCUSTOMPARM:
          begin
            result := 0;
            if fActor <> nil then
              if Pmobj_t(fActor).target <> nil then
              begin
                parm := P_GetMobjCustomParam(Pmobj_t(fActor).target, fList[index].s_param);
                if parm <> nil then
                  result := parm.value;
              end;
          end;
      else
        result := fList[index].i_param;
      end;
    end;
  end
  else
    result := 0;
end;

procedure TCustomParamList.PutInteger(index: Integer; const value: integer);
begin
  if (index >= 0) and (index < fNumItems) then
  begin
    fList[index].i_param := value;
    { -----------------------------
     old code:
    fList[index].computed := true;
    fList[index].israndom := false;
    -------------------------------}
    if fList[index].israndom = 0 then
      if fList[index].globalidx = 0 then
        fList[index].computed := true;
  end;
end;

function TCustomParamList.GetFloat(index: integer): single;
var
  parm: Pmobjcustomparam_t;
begin
  if (index >= 0) and (index < fNumItems) then
  begin
    if fList[index].israndom = 1 then
      result := (fList[index].i_parm1 * FRACUNIT + N_Random * (fList[index].i_parm2 - fList[index].i_parm1 + 1) * 256) / FRACUNIT
    else if fList[index].israndom = 2 then
      result := (fList[index].f_parm1 * FRACUNIT + N_Random * (fList[index].f_parm2 - fList[index].f_parm1 + 1) * 256) / FRACUNIT
    else
    begin
      case fList[index].globalidx of
        GLBF_MAP_STRING:
          result := atof(PS_GetMapStr(fList[index].s_param), 0.0);
        GLBF_MAP_INTEGER:
          result := PS_GetMapInt(fList[index].s_param);
        GLBF_MAP_FLOAT:
          result := PS_GetMapFloat(fList[index].s_param);
        GLBF_WORLD_STRING:
          result := atof(PS_GetWorldStr(fList[index].s_param), 0.0);
        GLBF_WORLD_INTEGER:
          result := PS_GetWorldInt(fList[index].s_param);
        GLBF_WORLD_FLOAT:
          result := PS_GetWorldFloat(fList[index].s_param);
        GLBF_MOBJ_CUSTOMPARM:
          begin
            result := 0.0;
            if fActor <> nil then
            begin
              parm := P_GetMobjCustomParam(fActor, fList[index].s_param);
              if parm <> nil then
                result := parm.value;
            end;
          end;
        GLBF_MOBJ_TARGETCUSTOMPARM:
          begin
            result := 0.0;
            if fActor <> nil then
              if Pmobj_t(fActor).target <> nil then
              begin
                parm := P_GetMobjCustomParam(Pmobj_t(fActor).target, fList[index].s_param);
                if parm <> nil then
                  result := parm.value;
              end;
          end;
      else
        result := fList[index].f_param;
      end;
    end;
  end
  else
    result := 0;
end;

procedure TCustomParamList.PutFloat(index: Integer; const value: float);
begin
  if (index >= 0) and (index < fNumItems) then
  begin
    fList[index].f_param := value;
    fList[index].fx_param := round(value * FRACUNIT);
    {----------------------------
    old code:
    fList[index].computed := true;
    fList[index].israndom := false;
    -------------------------------}
    if fList[index].israndom = 0 then
      if fList[index].globalidx = 0 then
        fList[index].computed := true;
  end;
end;

function TCustomParamList.GetFixed(index: integer): fixed_t;
var
  parm: Pmobjcustomparam_t;
begin
  if (index >= 0) and (index < fNumItems) then
  begin
    if fList[index].israndom = 1 then
      result := fList[index].i_parm1 * FRACUNIT + N_Random * (fList[index].i_parm2 - fList[index].i_parm1 + 1) * 256
    else if fList[index].israndom = 2 then
      result := round(fList[index].f_parm1 * FRACUNIT + N_Random * (fList[index].f_parm2 - fList[index].f_parm1 + 1) * 256)
    else
    begin
      case fList[index].globalidx of
        GLBF_MAP_STRING:
          result := round(atof(PS_GetMapStr(fList[index].s_param), 0.0) * FRACUNIT);
        GLBF_MAP_INTEGER:
          result := PS_GetMapInt(fList[index].s_param) * FRACUNIT;
        GLBF_MAP_FLOAT:
          result := round(PS_GetMapFloat(fList[index].s_param) * FRACUNIT);
        GLBF_WORLD_STRING:
          result := round(atof(PS_GetWorldStr(fList[index].s_param), 0.0) * FRACUNIT);
        GLBF_WORLD_INTEGER:
          result := PS_GetWorldInt(fList[index].s_param) * FRACUNIT;
        GLBF_WORLD_FLOAT:
          result := round(PS_GetWorldFloat(fList[index].s_param) * FRACUNIT);
        GLBF_MOBJ_CUSTOMPARM:
          begin
            result := 0;
            if fActor <> nil then
            begin
              parm := P_GetMobjCustomParam(fActor, fList[index].s_param);
              if parm <> nil then
                result := parm.value * FRACUNIT;
            end;
          end;
        GLBF_MOBJ_TARGETCUSTOMPARM:
          begin
            result := 0;
            if fActor <> nil then
              if Pmobj_t(fActor).target <> nil then
              begin
                parm := P_GetMobjCustomParam(Pmobj_t(fActor).target, fList[index].s_param);
                if parm <> nil then
                  result := parm.value * FRACUNIT;
              end;
          end;
      else
        result := fList[index].fx_param;
      end;
    end;
  end
  else
    result := 0;
end;

function TCustomParamList.GetString(index: integer): string;
var
  parm: Pmobjcustomparam_t;
begin
  if (index >= 0) and (index < fNumItems) then
  begin
    case fList[index].globalidx of
      GLBF_MAP_STRING:
        result := PS_GetMapStr(fList[index].s_param);
      GLBF_MAP_INTEGER:
        result := itoa(PS_GetMapInt(fList[index].s_param));
        GLBF_MAP_FLOAT:
          result := ftoa(PS_GetMapFloat(fList[index].s_param));
        GLBF_WORLD_STRING:
          result := PS_GetWorldStr(fList[index].s_param);
        GLBF_WORLD_INTEGER:
          result := itoa(PS_GetWorldInt(fList[index].s_param));
        GLBF_WORLD_FLOAT:
          result := ftoa(PS_GetWorldFloat(fList[index].s_param));
        GLBF_MOBJ_CUSTOMPARM:
          begin
            result := '';
            if fActor <> nil then
            begin
              parm := P_GetMobjCustomParam(fActor, fList[index].s_param);
              if parm <> nil then
                result := itoa(parm.value);
            end;
          end;
        GLBF_MOBJ_TARGETCUSTOMPARM:
          begin
            result := '';
            if fActor <> nil then
              if Pmobj_t(fActor).target <> nil then
              begin
                parm := P_GetMobjCustomParam(Pmobj_t(fActor).target, fList[index].s_param);
                if parm <> nil then
                  result := itoa(parm.value);
              end;
          end;
      else
        result := fList[index].s_param
    end;
  end
  else
    result := '';
end;

function TCustomParamList.GetDeclaration(index: integer): string;
begin
  if (index >= 0) and (index < fNumItems) then
  begin
    if fList[index].israndom = 1 then
      result := 'Random(' + itoa(fList[index].i_parm1) + ', ' + itoa(fList[index].i_parm2) + ')'
    else if fList[index].israndom = 2 then
      result := 'FloatRandom(' + ftoa(fList[index].f_parm1) + ', ' + ftoa(fList[index].f_parm2) + ')'
    else
    begin
      case fList[index].globalidx of
        GLBF_MAP_STRING:
          result := 'MapStr("' + fList[index].s_param + '")';
        GLBF_MAP_INTEGER:
          result := 'MapInt("' + fList[index].s_param + '")';
        GLBF_MAP_FLOAT:
          result := 'MapFloat("' + fList[index].s_param + '")';
        GLBF_WORLD_STRING:
          result := 'WorldStr("' + fList[index].s_param + '")';
        GLBF_WORLD_INTEGER:
          result := 'WorldInt("' + fList[index].s_param + '")';
        GLBF_WORLD_FLOAT:
          result := 'WorldFloat("' + fList[index].s_param + '")';
        GLBF_MOBJ_CUSTOMPARM:
          result := 'CustomParam("' + fList[index].s_param + '")';
        GLBF_MOBJ_TARGETCUSTOMPARM:
          result := 'TargetCustomParam("' + fList[index].s_param + '")';
      else
        result := '"' + fList[index].s_param + '"';
      end;
    end;
  end
  else
    result := '';
end;

end.
