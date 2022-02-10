unit ide_tmpfiles;

interface

//==============================================================================
//
// I_NewTempFile
//
//==============================================================================
function I_NewTempFile(const name: string): string;

implementation

uses
  Windows, SysUtils, Classes;

var
  tempfiles: TStringList;

//==============================================================================
//
// I_InitTempFiles
//
//==============================================================================
procedure I_InitTempFiles;
begin
  tempfiles := TStringList.Create;
end;

//==============================================================================
//
// I_ShutDownTempFiles
//
//==============================================================================
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

//==============================================================================
//
// StringVal
//
//==============================================================================
function StringVal(const Str: PChar): string;
begin
  FmtStr(result, '%s', [Str]);
end;

//==============================================================================
//
// I_NewTempFile
//
//==============================================================================
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

