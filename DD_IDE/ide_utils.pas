unit ide_utils;

interface

uses
  Classes, SysUtils, Graphics;

type
  outproc_t = procedure (const c: TColor; const s: string) of object;

  TAnsiString = class
    str: string;
    constructor Create(const astring: AnsiString);
  end;

procedure splitstring(const inp: AnsiString; out out1, out2: AnsiString; const splitter: AnsiString); overload;

procedure splitstring(const inp: AnsiString; out out1, out2, out3: AnsiString; const splitter: AnsiString); overload;

function firstword(const inp: AnsiString; const splitter: AnsiString): string;

function secondword(const inp: AnsiString; const splitter: AnsiString): string;

function LogOutput(const c: TColor; const s: string): string; overload;

function LogOutput(const c: TColor; const Fmt: string; const Args: array of const): string; overload;

const
  LOG_INFO = TColor($208020);
  LOG_NORMAL = TColor($100020);
  LOG_ERROR = TColor($2020FF);
  LOG_WARNING = TColor($4040FF);

var
  outproc: outproc_t;

procedure FreeStringList(var l: TStringList);

type
  TSimpleString = class(TObject)
    str: string;
    constructor Create(const s: string);
  end;

function CheckParm(const parm: string): integer;

procedure BackupFile(const fname: string);

implementation

constructor TAnsiString.Create(const astring: AnsiString);
begin
  str := astring;
end;

procedure splitstring(const inp: AnsiString; out out1, out2: AnsiString; const splitter: AnsiString); overload;
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

procedure splitstring(const inp: AnsiString; out out1, out2, out3: AnsiString; const splitter: AnsiString); overload;
var
  tmp: AnsiString;
begin
  splitstring(inp, out1, tmp, splitter);
  splitstring(tmp, out2, out3, splitter);
end;

function firstword(const inp: AnsiString; const splitter: AnsiString): string;
var
  tmp: string;
begin
  splitstring(inp, Result, tmp, splitter);
end;

function secondword(const inp: AnsiString; const splitter: AnsiString): string;
var
  tmp: string;
begin
  splitstring(inp, tmp, Result, splitter);
end;

function LogOutput(const c: TColor; const s: string): string;
begin
  if Assigned(outproc) then
  begin
    Result := s;
    outproc(c, Result);
  end
  else
    Result := '';
end;

function LogOutput(const c: TColor; const Fmt: string; const Args: array of const): string;
begin
  Result := LogOutput(c, Format(Fmt, Args));
end;

procedure FreeStringList(var l: TStringList);
var
  i: integer;
begin
  if l = nil then
    Exit;

  for i := 0 to l.Count - 1 do
    l.Objects[i].Free;
  l.Free;
  l := nil;
end;

constructor TSimpleString.Create(const s: string);
begin
  inherited Create;
  str := s;
end;

function CheckParm(const parm: string): integer;
var
  i: integer;
  parmU: string;
begin
  parmU := UpperCase(parm);
  for i := 1 to ParamCount do
    if parmU = UpperCase(ParamStr(i)) then
    begin
      Result := i;
      Exit;
    end;
  Result := -1;
end;

procedure CopyFile(const sname, dname: string);
var
  FromF, ToF: file;
  NumRead, NumWritten: Integer;
  Buf: array[1..8192] of Char;
begin
  if FileExists(sname) then
  begin
    AssignFile(FromF, sname);
    Reset(FromF, 1);
    AssignFile(ToF, dname);
    Rewrite(ToF, 1);
    repeat
      BlockRead(FromF, Buf, SizeOf(Buf), NumRead);
      BlockWrite(ToF, Buf, NumRead, NumWritten);
    until (NumRead = 0) or (NumWritten <> NumRead);
    CloseFile(FromF);
    CloseFile(ToF);
  end
  else
  begin
    AssignFile(ToF, dname);
    Rewrite(ToF, 1);
    CloseFile(ToF);
  end;
end;

procedure BackupFile(const fname: string);
var
  fbck: string;
begin
  if not FileExists(fname) then
    Exit;
  fbck := fname + '_' + FormatDateTime('yyyymmdd', Now);
  if FileExists(fbck) then
    fbck := fbck + '_latest';
  CopyFile(fname, fbck);
end;

end.

