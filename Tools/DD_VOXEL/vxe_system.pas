unit vxe_system;

interface

function I_GetTime: integer;

const
  TICRATE = 100;

implementation

uses
  Windows;

var
  basetime: int64;
  Freq: int64;

function I_GetSysTime: extended;
var
  _time: int64;
begin
  if Freq = 1000 then
    _time := GetTickCount
  else
  begin
    if not QueryPerformanceCounter(_time) then
    // QueryPerformanceCounter() failed, basetime reset.
    begin
      _time := GetTickCount;
      Freq := 1000;
      basetime := 0;
    end;
  end;
  if basetime = 0 then
    basetime := _time;
  result := (_time - basetime) / Freq;
end;

function I_GetTime: integer;
begin
  result := trunc(I_GetSysTime * TICRATE);
end;

initialization
  basetime := 0;

  if not QueryPerformanceFrequency(Freq) then
    Freq := 1000;

end.
