{ Compiletime DLL importing support }
unit uPSC_dll;

{$I PascalScript.inc}
interface
{

  Function FindWindow(c1, c2: PChar): Cardinal; external 'FindWindow@user32.dll stdcall';

}
uses
  ps_compiler, ps_utils, ps_defs;

{$IFDEF DELPHI3UP }
resourceString
{$ELSE }
const
{$ENDIF }
  RPS_Invalid_External = 'Invalid External';
  RPS_InvalidCallingConvention = 'Invalid Calling Convention';

//==============================================================================
//
// DllExternalProc
//
//==============================================================================
function DllExternalProc(Sender: TPSPascalCompiler; Decl: TPSParametersDecl; const OriginalName, FExternal: TbtString): TPSRegProc;

type
  TDllCallingConvention = (
    clRegister,
    clPascal,
    ClCdecl,
    ClStdCall
  );

var
  DefaultCC: TDllCallingConvention;

//==============================================================================
//
// RegisterDll_Compiletime
//
//==============================================================================
procedure RegisterDll_Compiletime(cs: TPSPascalCompiler);

implementation

//==============================================================================
//
// rpos
//
//==============================================================================
function rpos(ch: TbtChar; const s: TbtString): Longint;
var
  i: Longint;
begin
  for i := Length(s) downto 1 do
    if s[i] = ch then
    begin
      Result := i;
      Exit;
    end;
  Result := 0;
end;

//==============================================================================
//
// RemoveQuotes
//
//==============================================================================
function RemoveQuotes(s: TbtString): TbtString;
begin
  Result := s;
  if Result = '' then
    Exit;
  if Result[1] = '"' then
    Delete(Result, 1, 1);
  if (Result <> '') and (Result[Length(Result)] = '"') then
    Delete(Result, Length(Result), 1);
end;

//==============================================================================
//
// DllExternalProc
//
//==============================================================================
function DllExternalProc(Sender: TPSPascalCompiler; Decl: TPSParametersDecl; const OriginalName, FExternal: TbtString): TPSRegProc;
var
  FuncName,
  Name,
  FuncCC, s, s2: AnsiString;
  CC: TDllCallingConvention;
  DelayLoad, LoadWithAlteredSearchPath: Boolean;
begin
  Name := FastUpperCase(OriginalName);
  DelayLoad := False;
  LoadWithAlteredSearchPath := False;
  FuncCC := FExternal;

  if (Pos(TbtChar('@'), FuncCC) = 0) then
  begin
    Sender.MakeError('', ecCustomError, TbtString(RPS_Invalid_External));
    Result := nil;
    Exit;
  end;
  FuncName := Copy(FuncCC, 1, rpos('@', FuncCC) - 1) + #0;
  Delete(FuncCc, 1, Length(FuncName));
  if Pos(TbtChar(' '), Funccc) <> 0 then
  begin
    if FuncCC[1] = '"' then
    begin
      Delete(FuncCC, 1, 1);
      FuncName := RemoveQuotes(Copy(FuncCC, 1, Pos(TbtChar('"'), FuncCC) - 1)) + #0 + FuncName;
      Delete(FuncCC,1, Pos(TbtChar('"'), FuncCC));
      if (FuncCC <> '') and (FuncCC[1] = ' ') then
        Delete(FuncCC, 1, 1);
    end
    else
    begin
      FuncName := Copy(FuncCc, 1, Pos(TbtChar(' '), FuncCC) - 1) + #0 + FuncName;
      Delete(FuncCC, 1, Pos(TbtChar(' '), FuncCC));
    end;
    if Pos(TbtChar(' '), FuncCC) > 0 then
    begin
      s := Copy(FuncCC, Pos(TbtChar(' '), Funccc) + 1, MaxInt);
      FuncCC := FastUpperCase(Copy(FuncCC, 1, Pos(TbtChar(' '), FuncCC) - 1));
      Delete(FuncCC, Pos(TbtChar(' '), Funccc), MaxInt);
      repeat
        if Pos(TbtChar(' '), s) > 0 then
        begin
          s2 := Copy(s, 1, Pos(TbtChar(' '), s) - 1);
          Delete(s, 1, Pos(TbtChar(' '), s));
        end
        else
        begin
          s2 := s;
          s := '';
        end;
        if FastUpperCase(s2) = 'DELAYLOAD' then
          DelayLoad := True
        {$IFNDEF LINUX}
        else
        if FastUpperCase(s2) = 'LOADWITHALTEREDSEARCHPATH' then
          LoadWithAlteredSearchPath := True
        {$ENDIF}
        else
        begin
          Sender.MakeError('', ecCustomError, TbtString(RPS_Invalid_External));
          Result := nil;
          Exit;
        end;
      until s = '';

    end
    else
      FuncCC := FastUpperCase(FuncCC);
    if FuncCC = 'STDCALL' then
      cc := ClStdCall
    else if FuncCC = 'CDECL' then
      cc := ClCdecl
    else if FuncCC = 'REGISTER' then
      cc := clRegister
    else if FuncCC = 'PASCAL' then
      cc := clPascal
    else
    begin
      Sender.MakeError('', ecCustomError, TbtString(RPS_InvalidCallingConvention));
      Result := nil;
      Exit;
    end;
  end
  else
  begin
    FuncName := RemoveQuotes(FuncCC) + #0 + FuncName;
    FuncCC := '';
    cc := DefaultCC;
  end;
  FuncName := 'dll:' + FuncName + TbtChar(cc) + TbtChar(bytebool(DelayLoad)) + TbtChar(bytebool(LoadWithAlteredSearchPath)) + declToBits(Decl);
  Result := TPSRegProc.Create;
  Result.ImportDecl := FuncName;
  Result.Decl.Assign(Decl);
  Result.Name := Name;
  Result.OrgName := OriginalName;
  Result.ExportName := False;
end;

//==============================================================================
//
// RegisterDll_Compiletime
//
//==============================================================================
procedure RegisterDll_Compiletime(cs: TPSPascalCompiler);
begin
  cs.OnExternalProc := DllExternalProc;
  cs.AddFunction('procedure UnloadDll(S: string)');
  cs.AddFunction('function DllGetLastError: LongInt');
end;

begin
  DefaultCc := clRegister;
end.

