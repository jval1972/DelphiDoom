unit ide_defs;

interface

//==============================================================================
//
// ide_LoadSettingFromFile
//
//==============================================================================
function ide_LoadSettingFromFile(const fn: string): boolean;

//==============================================================================
//
// ide_SaveSettingsToFile
//
//==============================================================================
procedure ide_SaveSettingsToFile(const fn: string);

var
  opt_useglpixels: Boolean = false;
  opt_renderaxes: Boolean = true;
  opt_renderglgrid: Boolean = true;
  opt_renderwireframe: Boolean = false;

implementation

uses
  SysUtils, Classes,
  ide_utils;

const
  NUMSETTINGS = 4;

type
  TSettingsType = (tstInteger, tstBoolean);

  TSettingItem = record
    desc: string;
    typeof: TSettingsType;
    location: pointer;
  end;

var
  Settings: array[0..NUMSETTINGS - 1] of TSettingItem = (
    (
      desc: 'GL_PIXELS';
      typeof: tstBoolean;
      location: @opt_useglpixels;
    ),
    (
      desc: 'GL_RENDERAXES';
      typeof: tstBoolean;
      location: @opt_renderaxes;
    ),
    (
      desc: 'GL_RENDERGRID';
      typeof: tstBoolean;
      location: @opt_renderglgrid;
    ),
    (
      desc: 'GL_RENDERWIREFRAME';
      typeof: tstBoolean;
      location: @opt_renderwireframe;
    )
  );

//==============================================================================
//
// IntToBool
//
//==============================================================================
function IntToBool(const x: integer): boolean;
begin
  Result := x <> 0;
end;

//==============================================================================
//
// BoolToInt
//
//==============================================================================
function BoolToInt(const b: boolean): integer;
begin
  if b then
    Result := 1
  else
    Result := 0;
end;

//==============================================================================
//
// ide_SaveSettingsToFile
//
//==============================================================================
procedure ide_SaveSettingsToFile(const fn: string);
var
  s: TStringList;
  i: integer;
begin
  s := TStringList.Create;
  try
    for i := 0 to NUMSETTINGS - 1 do
    begin
      if Settings[i].typeof = tstInteger then
        s.Add(Format('%s=%d', [Settings[i].desc, PInteger(Settings[i].location)^]))
      else if Settings[i].typeof = tstBoolean then
        s.Add(Format('%s=%d', [Settings[i].desc, BoolToInt(PBoolean(Settings[i].location)^)]));
    end;
    s.SaveToFile(fn);
  finally
    s.Free;
  end;
end;

//==============================================================================
//
// ide_LoadSettingFromFile
//
//==============================================================================
function ide_LoadSettingFromFile(const fn: string): boolean;
var
  s: TStringList;
  i, j: integer;
  s1, s2: string;
  itmp: integer;
begin
  if not FileExists(fn) then
  begin
    result := false;
    exit;
  end;
  result := true;

  s := TStringList.Create;
  try
    s.LoadFromFile(fn);
    begin
      for i := 0 to s.Count - 1 do
      begin
        splitstring(s.Strings[i], s1, s2, '=');
        if s2 <> '' then
        begin
          s1 := UpperCase(s1);
          for j := 0 to NUMSETTINGS - 1 do
            if UpperCase(Settings[j].desc) = s1 then
            begin
              if Settings[j].typeof = tstInteger then
              begin
                itmp := StrToIntDef(s2, PInteger(Settings[j].location)^);
                PInteger(Settings[j].location)^ := itmp;
              end
              else if Settings[j].typeof = tstBoolean then
              begin
                itmp := StrToIntDef(s2, BoolToInt(PBoolean(Settings[j].location)^));
                PBoolean(Settings[j].location)^ := IntToBool(itmp);
              end;
            end;
        end;
      end;
    end;
  finally
    s.Free;
  end;

end;

end.

