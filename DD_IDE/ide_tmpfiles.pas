unit ide_tmpfiles;

interface

function I_NewTempFile(const name: string): string;

implementation

uses
  Windows, SysUtils, Classes;

var
  tempfiles: TStringList;

procedure I_InitTempFiles;
begin
  tempfiles := TStringList.Create;
end;

procedure I_ShutDownTempFiles;
var
  i: integer;
begin
{$I-}
  for i := 0 to tempfiles.Count - 1 do
    if FileExists(tempfiles.Strings[i]) then
      DeleteFile(tempfiles.Strings[i]);
{$I+}
  tempfiles.Free;
end;

function StringVal(const Str: PChar): string;
begin
  FmtStr(result, '%s', [Str]);
end;

function I_NewTempFile(const name: string): string;
var
  buf: array[0..1024] of char;
begin
  ZeroMemory(@buf, SizeOf(buf));
  GetTempPath(SizeOf(buf), buf);
  result :=  StringVal(buf) + '\' + ExtractFileName(name);
  tempfiles.Add(result);
end;

initialization
  I_InitTempFiles;

finalization
  I_ShutDownTempFiles;

end.

