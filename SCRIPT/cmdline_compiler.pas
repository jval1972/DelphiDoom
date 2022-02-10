unit cmdline_compiler;

interface

//==============================================================================
//
// CmdLineCompiler
//
//==============================================================================
procedure CmdLineCompiler;

implementation

uses
  Windows,
  SysUtils,
  Classes,
  ddc_base;

const
  APPNAME = 'ddc';

//==============================================================================
//
// CheckParam
//
//==============================================================================
function CheckParam(const s: string): Integer;
var
  i: integer;
  sU: string;
begin
  Result := 0;
  sU := UpperCase(s);
  for i := 1 to ParamCount do
    if UpperCase(ParamStr(i)) = sU then
    begin
      Result := i;
      Exit;
    end;
end;

var
  forcewait: boolean = False;

//==============================================================================
//
// ShowHelp
//
//==============================================================================
procedure ShowHelp;
begin
  Writeln;
  Writeln('Usage:');
  Writeln('  ' + APPNAME + '.exe -input scriptname.ddscript [-output scriptname.ddout]/[-nooutput] ');
  Writeln('Parameters:');
  Writeln('  scriptname.ddscript  : The input script filename');
  Writeln('  scriptname.ddout     : The output compiled code filename');
  Writeln('Additional parameters:');
  Writeln('  [-game]              : [DOOM/HERETIC/HEXEN/STRIFE/RADIX/MARS]');
  Writeln('  [-doom]              : Uses DOOM compiler (default)');
  Writeln('  [-heretic]           : Uses HERETIC compiler');
  Writeln('  [-hexen]             : Uses HEXEN compiler');
  Writeln('  [-strife]            : Uses STRIFE compiler');
  Writeln('  [-radix]             : Uses RADIX compiler');
  Writeln('  [-mars]              : Uses Mars3D compiler');
  Writeln('  [-nooutput]          : do not generate output file');
  Writeln('  [-wait]              : wait for key when done');
  forcewait := True;
end;

//==============================================================================
//
// PressAnyKeyToContinue
//
//==============================================================================
procedure PressAnyKeyToContinue;
var
  k: byte;
begin
  while True do
    for k := 0 to 255 do
    begin
      GetAsyncKeyState(k);
      if GetAsyncKeyState(k) <> 0 then
        Exit;
    end;
end;

//==============================================================================
//
// CmdLineCompilerMain
//
//==============================================================================
procedure CmdLineCompilerMain;
var
  ver: string;
  inp_fname: string;
  out_fname: string;
  p: integer;
  game: string;
  code: string;
  pcode: string;
  msgs: string;
  sList: TStringList;
  ret: boolean;
  fout: TFileStream;
begin
  Str(DDCVERSION / 100:1:2, ver);
  Writeln('DelphiDoom command line script Compiler (' + APPNAME + ') - Version ' + ver);

  if ParamCount < 1 then
  begin
    ShowHelp;
    Exit;
  end;

  inp_fname := '';
  p := CheckParam('-input');
  if (p > 0) and (p < ParamCount) then
    inp_fname := ParamStr(p + 1)
  else if ParamCount = 1 then
  begin
    if FileExists(ParamStr(1)) then
      inp_fname := ParamStr(1);
  end;

  if inp_fname = '' then
  begin
    ShowHelp;
    Exit;
  end;

  game := 'doom';
  p := CheckParam('-game');
  if (p > 0) and (p < ParamCount) then
  begin
    game := LowerCase(ParamStr(p + 1));
    if (game <> 'doom') then
      if (game <> 'heretic') then
        if (game <> 'hexen') then
          if (game <> 'strife') then
            if (game <> 'radix') then
              if (game <> 'mars') then
                if (game <> 'mars3d') then
                  Writeln('WARNING: Unknown game "' + game + '"');
  end;

  p := CheckParam('-doom');
  if p > 0 then
    game := 'doom';

  p := CheckParam('-heretic');
  if p > 0 then
    game := 'heretic';

  p := CheckParam('-hexen');
  if p > 0 then
    game := 'hexen';

  p := CheckParam('-strife');
  if p > 0 then
    game := 'strife';

  p := CheckParam('-radix');
  if p > 0 then
    game := 'radix';

  p := CheckParam('-mars');
  if p > 0 then
    game := 'mars';

  p := CheckParam('-mars3d');
  if p > 0 then
    game := 'mars';

  code := '';
  try
    sList := TStringList.Create;
    try
      sList.LoadFromFile(inp_fname);
      code := sList.Text;
    finally
      sList.Free;
    end;
  except
    Writeln('ERROR: Can not load "' + inp_fname + '".');
    Exit;
  end;

  pcode := '';
  msgs := '';
  ret := dll_compile(game, code, pcode, msgs);
  Writeln(msgs);

  if ret then
  begin
    out_fname := ChangeFileExt(inp_fname, '.ddout');
    if UpperCase(out_fname) = UpperCase(inp_fname) then
    begin
      Writeln('ERROR: Input and output files can not be the same.');
      Exit;
    end;

    p := CheckParam('-output');
    if (p > 0) and (p < ParamCount) then
      out_fname := ParamStr(p + 1);
    p := CheckParam('-nooutput');
    if p = 0 then
    try
      fout := TFileStream.Create(out_fname, fmCreate or fmShareExclusive);
      try
        for p := 1 to Length(pcode) do // Ouch :P
          fout.Write(pcode[p], 1);
      finally
        fout.Free;
      end;
    except
      Writeln('ERROR: Can not save "' + out_fname + '".');
      Exit;
    end;
  end;
end;

//==============================================================================
//
// CmdLineCompiler
//
//==============================================================================
procedure CmdLineCompiler;
begin
  CmdLineCompilerMain;
  if forcewait or (CheckParam('-wait') > 0) then
    PressAnyKeyToContinue;
end;

end.

