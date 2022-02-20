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
  GLBF_RANDOM = 9;
  GLBF_FRANDOM = 10;
  GLBF_EVALUATE = 11;
  GLBF_MOBJ_MASTERCUSTOMPARM = 12;
  GLBF_MOBJ_TRACERCUSTOMPARM = 13;
  GLBF_FORCE_EVALUATE = 14;

const
  CPFLG_FLOATSTRING = 1;

type
  customparam_t = record
    s_param: string[255];
    i_param: integer;
    i_parm1, i_parm2: integer;
    f_param: single;
    f_parm1, f_parm2: single;
    fx_param: fixed_t;
    globalidx: integer;
    computed: boolean;
    flags: integer;
  end;
  Pcustomparam_t = ^customparam_t;
  customparam_tArray = array[0..$FFF] of customparam_t;
  Pcustomparam_tArray = ^customparam_tArray;

  TCustomParamList = class
  private
    fList: Pcustomparam_tArray;
    fNumItems: integer;
    fActor: pointer;
    fdeclaration: string;
  protected
    function GetIsComputed(index: integer): boolean; virtual;
    function GetInteger(index: integer): integer; virtual;
    procedure PutInteger(index: integer; const value: integer); virtual;
    function GetFloat(index: integer): single; virtual;
    procedure PutFloat(index: integer; const value: single); virtual;
    function GetFixed(index: integer): fixed_t; virtual;
    function GetMBF21Fixed(index: integer): fixed_t; virtual;
    function GetBool(index: integer): boolean; virtual;
    function GetString(index: integer): string; virtual;
    function GetEvaluateString(index: integer): string; virtual;
  public
    constructor Create(const tx: string); virtual;
    destructor Destroy; override;
    procedure AddParam(const parmtype: integer; const value: string); virtual;
    property Count: integer read fNumItems;
    property IsComputed[index: integer]: boolean read GetIsComputed;
    property IntVal[index: integer]: integer read GetInteger write PutInteger;
    property FloatVal[index: integer]: single read GetFloat write PutFloat;
    property FixedVal[index: integer]: fixed_t read GetFixed;
    property MBF21FixedVal[index: integer]: fixed_t read GetMBF21Fixed;
    property BoolVal[index: integer]: boolean read GetBool;
    property StrVal[index: integer]: string read GetString;
    property EvaluateStrVal[index: integer]: string read GetEvaluateString;
    property Declaration: string read fdeclaration;
    property Actor: pointer read fActor write fActor;
  end;

//==============================================================================
//
// SC_EvalString
//
//==============================================================================
function SC_EvalString(const token: string): string;

implementation

uses
  d_delphi,
  m_rnd,
  psi_globals,
  p_mobj_h,
  p_params,
  sc_engine,
  sc_evaluate_actor;

//==============================================================================
//
// TCustomParamList.Create
//
//==============================================================================
constructor TCustomParamList.Create(const tx: string);
var
  i, j: integer;
  ok: boolean;
  token: string;
  utoken: string;
  lst, lstparam: TDStringList;
begin
  fList := nil;
  fNumItems := 0;
  fActor := nil;
  fdeclaration := tx;

  lst := SC_ParamsToList(tx);

  for i := 0 to lst.Count - 1 do
  begin
    if StrIsInteger(lst[i]) then
    begin
      AddParam(0, lst[i])
    end
    else if StrIsFloat(lst[i]) then
    begin
      AddParam(0, lst[i])
    end
    else if SC_IsStringInQuotes(lst[i]) then
    begin
      AddParam(0, Copy(lst[i], 2, length(lst[i]) - 2));
    end
    else
    begin
      lstparam := wordstolist(lst[i], [' ', #13, #10, #9, #7, '(', ',', ')', '[', ']']);

      if lstparam.Count > 0 then
      begin
        token := strupper(lstparam[0]);
        utoken := strupper(token);
        if utoken = 'RANDOM' then
        begin
          if lstparam.Count <= 3 then
          begin
            ok := true;
            for j := 1 to lstparam.Count - 1 do
              ok := ok and StrIsInteger(lstparam[j]);
            if ok then
              AddParam(GLBF_RANDOM, 'RANDOM ' + lstparam[1] + ' ' + lstparam[2])
            else
              AddParam(GLBF_EVALUATE, lst[i]);
          end
          else
            AddParam(GLBF_EVALUATE, lst[i]);
        end
        else if (utoken = 'FRANDOM') or (utoken = 'FLOATRANDOM') then
        begin
          if lstparam.Count <= 3 then
          begin
            ok := true;
            for j := 1 to lstparam.Count - 1 do
              ok := ok and StrIsFloat(lstparam[j]);
            if ok then
              AddParam(GLBF_FRANDOM, 'FRANDOM ' + lstparam[1] + ' ' + lstparam[2])
            else
              AddParam(GLBF_EVALUATE, lst[i]);
          end
          else
            AddParam(GLBF_EVALUATE, lst[i]);
        end
        else if utoken = 'EVAL' then
          AddParam(GLBF_FORCE_EVALUATE, lst[i])
        else if (utoken = 'MAPSTR') or
                (utoken = 'WORLDSTR') or
                (utoken = 'MAPINT') or
                (utoken = 'WORLDINT') or
                (utoken = 'MAPFLOAT') or
                (utoken = 'WORLDFLOAT') or
                (utoken = 'CUSTOMPARAM') or
                (utoken = 'TARGETCUSTOMPARAM') or
                (utoken = 'MASTERCUSTOMPARAM') or
                (utoken = 'TRACERCUSTOMPARAM') then
        begin
          ok := false;
          if lstparam.Count = 2 then
            if SC_IsStringInQuotes(lstparam[1]) then
              ok := true;
          if ok then
          begin
            if utoken = 'MAPSTR' then
              AddParam(GLBF_MAP_STRING, 'MAPSTR ' + RemoveQuotesFromString(lstparam[1]))
            else if utoken = 'WORLDSTR' then
              AddParam(GLBF_WORLD_STRING, 'WORLDSTR ' + RemoveQuotesFromString(lstparam[1]))
            else if utoken = 'MAPINT' then
              AddParam(GLBF_MAP_INTEGER, 'MAPINT ' + RemoveQuotesFromString(lstparam[1]))
            else if utoken = 'WORLDINT' then
              AddParam(GLBF_WORLD_INTEGER, 'WORLDINT ' + RemoveQuotesFromString(lstparam[1]))
            else if utoken = 'MAPFLOAT' then
              AddParam(GLBF_MAP_FLOAT, 'MAPFLOAT ' + RemoveQuotesFromString(lstparam[1]))
            else if utoken = 'WORLDFLOAT' then
              AddParam(GLBF_WORLD_FLOAT, 'WORLDFLOAT ' + RemoveQuotesFromString(lstparam[1]))
            else if utoken = 'CUSTOMPARAM' then
              AddParam(GLBF_MOBJ_CUSTOMPARM, 'CUSTOMPARAM ' + RemoveQuotesFromString(lstparam[1]))
            else if utoken = 'TARGETCUSTOMPARAM' then
              AddParam(GLBF_MOBJ_TARGETCUSTOMPARM, 'TARGETCUSTOMPARAM ' + RemoveQuotesFromString(lstparam[1]))
            else if utoken = 'MASTERCUSTOMPARAM' then
              AddParam(GLBF_MOBJ_MASTERCUSTOMPARM, 'MASTERCUSTOMPARAM ' + RemoveQuotesFromString(lstparam[1]))
            else if utoken = 'TRACERCUSTOMPARAM' then
              AddParam(GLBF_MOBJ_TRACERCUSTOMPARM, 'TRACERCUSTOMPARAM ' + RemoveQuotesFromString(lstparam[1]))
          end
          else
          begin
            AddParam(GLBF_EVALUATE, lst[i]);
          end;
        end
        else
        begin
          if (lstparam.Count = 1) and not SC_IsActorEvaluatorSingleToken(lstparam[0]) then
            AddParam(0, lstparam[0])
          else
            AddParam(GLBF_EVALUATE, lst[i]);
        end;

      end;

      lstparam.Free;
    end;
  end;

  lst.Free;

end;

//==============================================================================
//
// TCustomParamList.Destroy
//
//==============================================================================
destructor TCustomParamList.Destroy;
begin
  if fNumItems > 0 then
    realloc(pointer(fList), fNumItems * SizeOf(customparam_t), 0);
end;

//==============================================================================
//
// TCustomParamList.AddParam
//
//==============================================================================
procedure TCustomParamList.AddParam(const parmtype: integer; const value: string);
var
  ival: integer;
  fval: single;
  token, token1, token2: string;
  utoken: string;
begin
  realloc(pointer(fList), fNumItems * SizeOf(customparam_t), (fNumItems + 1) * SizeOf(customparam_t));
  fList[fNumItems].flags := 0;
  splitstring_ch(value, token1, token);
  utoken := strupper(token1);
  if (utoken = 'RANDOM') and (parmtype = GLBF_RANDOM) then
  begin
    fList[fNumItems].globalidx := GLBF_RANDOM;
    fList[fNumItems].s_param := '';
    fList[fNumItems].computed := false;
    if token = '' then
    begin
      fList[fNumItems].i_parm1 := 0;
      fList[fNumItems].i_parm2 := 255;
    end
    else
    begin
      splitstring_ch(token, token1, token2);
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
  else if ((utoken = 'FRANDOM') or (utoken = 'FLOATRANDOM')) and (parmtype = GLBF_FRANDOM) then
  begin
    fList[fNumItems].globalidx := GLBF_FRANDOM;
    fList[fNumItems].s_param := '';
    fList[fNumItems].computed := false;
    if token = '' then
    begin
      fList[fNumItems].f_parm1 := 0;
      fList[fNumItems].f_parm2 := 255;
    end
    else
    begin
      splitstring_ch(token, token1, token2);
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
  else if (utoken = 'EVAL') and (parmtype = GLBF_FORCE_EVALUATE) then
  begin
    fList[fNumItems].globalidx := GLBF_FORCE_EVALUATE;
    fList[fNumItems].computed := false;
    fList[fNumItems].s_param := value;
  end
  else
  begin
    if (utoken = 'MAPSTR') and (parmtype = GLBF_MAP_STRING) then
    begin
      fList[fNumItems].globalidx := GLBF_MAP_STRING;
      fList[fNumItems].s_param := token;
    end
    else if (utoken = 'WORLDSTR') and (parmtype = GLBF_WORLD_STRING) then
    begin
      fList[fNumItems].globalidx := GLBF_WORLD_STRING;
      fList[fNumItems].s_param := token;
    end
    else if (utoken = 'MAPINT') and (parmtype = GLBF_MAP_INTEGER) then
    begin
      fList[fNumItems].globalidx := GLBF_MAP_INTEGER;
      fList[fNumItems].s_param := token;
    end
    else if (utoken = 'WORLDINT') and (parmtype = GLBF_WORLD_INTEGER) then
    begin
      fList[fNumItems].globalidx := GLBF_WORLD_INTEGER;
      fList[fNumItems].s_param := token;
    end
    else if (utoken = 'MAPFLOAT') and (parmtype = GLBF_MAP_FLOAT) then
    begin
      fList[fNumItems].globalidx := GLBF_MAP_FLOAT;
      fList[fNumItems].s_param := token;
    end
    else if (utoken = 'WORLDFLOAT') and (parmtype = GLBF_WORLD_FLOAT) then
    begin
      fList[fNumItems].globalidx := GLBF_WORLD_FLOAT;
      fList[fNumItems].s_param := token;
    end
    else if (utoken = 'CUSTOMPARAM') and (parmtype = GLBF_MOBJ_CUSTOMPARM) then
    begin
      fList[fNumItems].globalidx := GLBF_MOBJ_CUSTOMPARM;
      fList[fNumItems].s_param := token;
    end
    else if (utoken = 'TARGETCUSTOMPARAM') and (parmtype = GLBF_MOBJ_TARGETCUSTOMPARM) then
    begin
      fList[fNumItems].globalidx := GLBF_MOBJ_TARGETCUSTOMPARM;
      fList[fNumItems].s_param := token;
    end
    else if (utoken = 'MASTERCUSTOMPARAM') and (parmtype = GLBF_MOBJ_MASTERCUSTOMPARM) then
    begin
      fList[fNumItems].globalidx := GLBF_MOBJ_MASTERCUSTOMPARM;
      fList[fNumItems].s_param := token;
    end
    else if (utoken = 'TRACERCUSTOMPARAM') and (parmtype = GLBF_MOBJ_TRACERCUSTOMPARM) then
    begin
      fList[fNumItems].globalidx := GLBF_MOBJ_TRACERCUSTOMPARM;
      fList[fNumItems].s_param := token;
    end
    else
    begin
      fList[fNumItems].globalidx := parmtype;
      fList[fNumItems].s_param := value;
      if StrIsFloat(value) then
        if not StrIsInteger(value) then
          fList[fNumItems].flags := fList[fNumItems].flags or CPFLG_FLOATSTRING;
    end;
    if parmtype = 0 then
    begin
      fList[fNumItems].globalidx := 0;
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
    end
    else
    begin
      fList[fNumItems].globalidx := GLBF_EVALUATE;
      fList[fNumItems].computed := false;
      fList[fNumItems].s_param := value;
    end;
  end;

  inc(fNumItems);
end;

//==============================================================================
//
// TCustomParamList.GetIsComputed
//
//==============================================================================
function TCustomParamList.GetIsComputed(index: integer): boolean;
begin
  result := fList[index].computed;
end;

//==============================================================================
//
// TCustomParamList.GetInteger
//
//==============================================================================
function TCustomParamList.GetInteger(index: integer): integer;
var
  parm: Pmobjcustomparam_t;
begin
  if (index >= 0) and (index < fNumItems) then
  begin
    case fList[index].globalidx of
      GLBF_RANDOM:
        result := fList[index].i_parm1 + (N_Random * (fList[index].i_parm2 - fList[index].i_parm1 + 1)) div 256;
      GLBF_FRANDOM:
        result := round(fList[index].f_parm1 + (N_Random * (fList[index].f_parm2 - fList[index].f_parm1 + 1)) / 256);
      GLBF_EVALUATE,
      GLBF_FORCE_EVALUATE:
        result := round(atof(SC_EvaluateActorExpression(fActor, fList[index].s_param)));
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
      GLBF_MOBJ_MASTERCUSTOMPARM:
        begin
          result := 0;
          if fActor <> nil then
            if Pmobj_t(fActor).master <> nil then
            begin
              parm := P_GetMobjCustomParam(Pmobj_t(fActor).master, fList[index].s_param);
              if parm <> nil then
                result := parm.value;
            end;
        end;
      GLBF_MOBJ_TRACERCUSTOMPARM:
        begin
          result := 0;
          if fActor <> nil then
            if Pmobj_t(fActor).tracer <> nil then
            begin
              parm := P_GetMobjCustomParam(Pmobj_t(fActor).tracer, fList[index].s_param);
              if parm <> nil then
                result := parm.value;
            end;
        end;
    else
      result := fList[index].i_param;
    end;
  end
  else
    result := 0;
end;

//==============================================================================
//
// TCustomParamList.PutInteger
//
//==============================================================================
procedure TCustomParamList.PutInteger(index: integer; const value: integer);
begin
  if (index >= 0) and (index < fNumItems) then
  begin
    fList[index].i_param := value;
    { -----------------------------
     old code:
    fList[index].computed := true;
    fList[index].israndom := false;
    -------------------------------}
    if fList[index].globalidx = 0 then
      fList[index].computed := true;
  end;
end;

//==============================================================================
//
// TCustomParamList.GetFloat
//
//==============================================================================
function TCustomParamList.GetFloat(index: integer): single;
var
  parm: Pmobjcustomparam_t;
begin
  if (index >= 0) and (index < fNumItems) then
  begin
    case fList[index].globalidx of
      GLBF_RANDOM:
        result := (fList[index].i_parm1 * FRACUNIT + N_Random * (fList[index].i_parm2 - fList[index].i_parm1 + 1) * 256) / FRACUNIT;
      GLBF_FRANDOM:
        result := (fList[index].f_parm1 * FRACUNIT + N_Random * (fList[index].f_parm2 - fList[index].f_parm1 + 1) * 256) / FRACUNIT;
      GLBF_EVALUATE,
      GLBF_FORCE_EVALUATE:
        result := atof(SC_EvaluateActorExpression(fActor, fList[index].s_param));
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
      GLBF_MOBJ_MASTERCUSTOMPARM:
        begin
          result := 0.0;
          if fActor <> nil then
            if Pmobj_t(fActor).master <> nil then
            begin
              parm := P_GetMobjCustomParam(Pmobj_t(fActor).master, fList[index].s_param);
              if parm <> nil then
                result := parm.value;
            end;
        end;
      GLBF_MOBJ_TRACERCUSTOMPARM:
        begin
          result := 0.0;
          if fActor <> nil then
            if Pmobj_t(fActor).tracer <> nil then
            begin
              parm := P_GetMobjCustomParam(Pmobj_t(fActor).tracer, fList[index].s_param);
              if parm <> nil then
                result := parm.value;
            end;
        end;
    else
      result := fList[index].f_param;
    end;
  end
  else
    result := 0.0;
end;

//==============================================================================
//
// TCustomParamList.PutFloat
//
//==============================================================================
procedure TCustomParamList.PutFloat(index: integer; const value: float);
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
    if fList[index].globalidx = 0 then
      fList[index].computed := true;
  end;
end;

//==============================================================================
//
// TCustomParamList.GetFixed
//
//==============================================================================
function TCustomParamList.GetFixed(index: integer): fixed_t;
var
  parm: Pmobjcustomparam_t;
begin
  if (index >= 0) and (index < fNumItems) then
  begin
    case fList[index].globalidx of
      GLBF_RANDOM:
        result := fList[index].i_parm1 * FRACUNIT + N_Random * (fList[index].i_parm2 - fList[index].i_parm1 + 1) * 256;
      GLBF_FRANDOM:
        result := round(fList[index].f_parm1 * FRACUNIT + N_Random * (fList[index].f_parm2 - fList[index].f_parm1 + 1) * 256);
      GLBF_EVALUATE,
      GLBF_FORCE_EVALUATE:
        result := round(atof(SC_EvaluateActorExpression(fActor, fList[index].s_param)) * FRACUNIT);
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
      GLBF_MOBJ_MASTERCUSTOMPARM:
        begin
          result := 0;
          if fActor <> nil then
            if Pmobj_t(fActor).master <> nil then
            begin
              parm := P_GetMobjCustomParam(Pmobj_t(fActor).master, fList[index].s_param);
              if parm <> nil then
                result := parm.value * FRACUNIT;
            end;
        end;
      GLBF_MOBJ_TRACERCUSTOMPARM:
        begin
          result := 0;
          if fActor <> nil then
            if Pmobj_t(fActor).tracer <> nil then
            begin
              parm := P_GetMobjCustomParam(Pmobj_t(fActor).tracer, fList[index].s_param);
              if parm <> nil then
                result := parm.value * FRACUNIT;
            end;
        end;
    else
      result := fList[index].fx_param;
    end;
  end
  else
    result := 0;
end;

//==============================================================================
//
// TCustomParamList.GetMBF21Fixed
//
//==============================================================================
function TCustomParamList.GetMBF21Fixed(index: integer): fixed_t;
begin
  if (index >= 0) and (index < fNumItems) then
  begin
    if fList[index].flags and CPFLG_FLOATSTRING <> 0 then
      result := FloatToFixed(atof(fList[index].s_param))
    else
      result := GetInteger(index);
  end
  else
    result := 0;
end;

//==============================================================================
//
// TCustomParamList.GetBool
//
//==============================================================================
function TCustomParamList.GetBool(index: integer): boolean;
var
  ret: string;
begin
  if (index >= 0) and (index < fNumItems) then
  begin
    ret := SC_EvaluateActorExpression(fActor, fList[index].s_param);
    result := strupper(RemoveQuotesFromString(ret)) = 'TRUE';
  end
  else
    result := false;
end;

//==============================================================================
//
// TCustomParamList.GetString
//
//==============================================================================
function TCustomParamList.GetString(index: integer): string;
var
  parm: Pmobjcustomparam_t;
begin
  if (index >= 0) and (index < fNumItems) then
  begin
    case fList[index].globalidx of
      GLBF_FORCE_EVALUATE:
        result := RemoveQuotesFromString(SC_EvaluateActorExpression(fActor, fList[index].s_param));
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
      GLBF_MOBJ_MASTERCUSTOMPARM:
        begin
          result := '';
          if fActor <> nil then
            if Pmobj_t(fActor).master <> nil then
            begin
              parm := P_GetMobjCustomParam(Pmobj_t(fActor).master, fList[index].s_param);
              if parm <> nil then
                result := itoa(parm.value);
            end;
        end;
      GLBF_MOBJ_TRACERCUSTOMPARM:
        begin
          result := '';
          if fActor <> nil then
            if Pmobj_t(fActor).tracer <> nil then
            begin
              parm := P_GetMobjCustomParam(Pmobj_t(fActor).tracer, fList[index].s_param);
              if parm <> nil then
                result := itoa(parm.value);
            end;
        end;
      else
        result := RemoveQuotesFromString(fList[index].s_param);
    end;
  end
  else
    result := '';
end;

//==============================================================================
//
// TCustomParamList.GetEvaluateString
//
//==============================================================================
function TCustomParamList.GetEvaluateString(index: integer): string;
var
  parm: Pmobjcustomparam_t;
begin
  if (index >= 0) and (index < fNumItems) then
  begin
    case fList[index].globalidx of
      GLBF_EVALUATE,
      GLBF_FORCE_EVALUATE:
        result := RemoveQuotesFromString(SC_EvaluateActorExpression(fActor, fList[index].s_param));
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
      GLBF_MOBJ_MASTERCUSTOMPARM:
        begin
          result := '';
          if fActor <> nil then
            if Pmobj_t(fActor).master <> nil then
            begin
              parm := P_GetMobjCustomParam(Pmobj_t(fActor).master, fList[index].s_param);
              if parm <> nil then
                result := itoa(parm.value);
            end;
        end;
      GLBF_MOBJ_TRACERCUSTOMPARM:
        begin
          result := '';
          if fActor <> nil then
            if Pmobj_t(fActor).tracer <> nil then
            begin
              parm := P_GetMobjCustomParam(Pmobj_t(fActor).tracer, fList[index].s_param);
              if parm <> nil then
                result := itoa(parm.value);
            end;
        end;
      else
        result := RemoveQuotesFromString(fList[index].s_param);
    end;
  end
  else
    result := '';
end;

//==============================================================================
//
// SC_EvalString
//
//==============================================================================
function SC_EvalString(const token: string): string;
var
  sl: TDStringList;
  r1, r2: integer;
begin
  if token = '' then
  begin
    Result := '';
    Exit;
  end;

  if toupper(token[1]) = 'R' then // Speedup Hack :)
  begin
    sl := wordstolist(token, ['(', ')', ' ', ',', '[', ']']);
    if sl.Count > 1 then
    begin
      if strupper(sl.strings[0]) = 'RANDOMPICK' then
      begin
        sl.Delete(0);
        Result := RemoveQuotesFromString(sl.Strings[N_Random mod sl.Count]);
        sl.Free;
        Exit;
      end
      else if strupper(sl.strings[0]) = 'RANDOM' then
      begin
        if sl.Count > 2 then
        begin
          r1 := atoi(sl.strings[1]);
          r2 := atoi(sl.strings[2]);
        end
        else if sl.count = 2 then
        begin
          r1 := 0;
          r2 := atoi(sl.strings[1]);
        end
        else
        begin
          r1 := 0;
          r2 := 255;
        end;
        Result := itoa(r1 + (N_Random * (r2 - r1 + 1)) div 256);
        sl.Free;
        Exit;
      end;
    end;
    sl.Free;
  end;

  Result := RemoveQuotesFromString(token);
end;

end.
